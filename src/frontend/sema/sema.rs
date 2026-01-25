//! Semantic analysis pass.
//!
//! Walks the AST to:
//! - Build a scoped symbol table of declarations
//! - Track function signatures for call validation
//! - Resolve typedef names and typeof(expr) via ExprTypeChecker
//! - Collect information needed by the IR lowering phase
//! - Map __builtin_* identifiers to their libc equivalents
//!
//! Expression CType inference is available via `type_checker::ExprTypeChecker`,
//! which uses SymbolTable + TypeContext + FunctionInfo to infer types without
//! depending on lowering state. This enables typeof(expr) resolution and
//! will eventually support type annotations on AST nodes.
//!
//! This pass does NOT reject programs with type errors (yet); it collects
//! information for the lowerer. Full type checking is TODO.

use crate::common::symbol_table::{Symbol, SymbolTable, StorageClass};
use crate::common::type_builder;
use crate::common::types::{CType, FunctionType, StructLayout};
use crate::common::source::Span;
use crate::frontend::parser::ast::*;
use crate::frontend::sema::builtins;
use super::type_context::{TypeContext, FunctionTypedefInfo};
use super::const_eval::{SemaConstEval, ConstMap};

use std::cell::Cell;
use crate::common::fx_hash::FxHashMap;

/// Map from AST expression node identity to its inferred CType.
///
/// Uses the raw pointer address of `Expr` nodes as a stable key. This works
/// because the AST is allocated once during parsing and is not moved or
/// reallocated before the lowerer consumes it. The lowerer can look up any
/// expression node's CType in O(1) without re-computing it.
///
/// This is the core data structure for Step 3 of the typed-AST plan: sema
/// annotates every expression it can type-check, and the lowerer consults
/// these annotations as a fallback before doing its own (more expensive)
/// type inference.
pub type ExprTypeMap = FxHashMap<usize, CType>;

/// Information about a function collected during semantic analysis.
#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: String,
    pub return_type: CType,
    pub params: Vec<(CType, Option<String>)>,
    pub variadic: bool,
    pub is_defined: bool,
}

/// Results of semantic analysis, used by the lowering phase.
#[derive(Debug)]
pub struct SemaResult {
    /// Function signatures discovered during analysis.
    pub functions: FxHashMap<String, FunctionInfo>,
    /// Type context populated by sema: typedefs, enum constants, struct layouts,
    /// function typedefs, function pointer typedefs.
    pub type_context: TypeContext,
    /// Expression type annotations: maps AST Expr node addresses to their
    /// inferred CTypes. Populated during sema's analyze_expr walk using
    /// ExprTypeChecker. The lowerer consults this as a fallback in
    /// get_expr_ctype() after its own lowering-state-based inference fails.
    pub expr_types: ExprTypeMap,
    /// Pre-computed constant expression values: maps AST Expr node addresses
    /// to their compile-time IrConst values. Populated during sema's walk
    /// using SemaConstEval (handles float literals, cast chains, sizeof,
    /// binary ops with proper signedness semantics). The lowerer consults
    /// this as an O(1) fast path before its own eval_const_expr.
    pub const_values: ConstMap,
}

impl Default for SemaResult {
    fn default() -> Self {
        Self {
            functions: FxHashMap::default(),
            type_context: TypeContext::new(),
            expr_types: ExprTypeMap::default(),
            const_values: ConstMap::default(),
        }
    }
}

/// Semantic analyzer that builds a scoped symbol table and collects type info.
pub struct SemanticAnalyzer {
    /// The scoped symbol table for name resolution.
    symbol_table: SymbolTable,
    /// Accumulated results for the lowerer.
    result: SemaResult,
    /// Current enum counter for auto-incrementing enum values.
    enum_counter: i64,
    /// Counter for generating unique anonymous struct/union keys.
    /// Uses Cell for interior mutability so type_spec_to_ctype can take &self.
    anon_struct_counter: Cell<usize>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut analyzer = Self {
            symbol_table: SymbolTable::new(),
            result: SemaResult::default(),
            enum_counter: 0,
            anon_struct_counter: Cell::new(0),
        };
        // Pre-populate with common implicit declarations
        analyzer.declare_implicit_functions();
        analyzer
    }

    /// Analyze a translation unit. Builds symbol table and collects semantic info.
    /// Returns Ok(()) on success, or a list of error strings on failure.
    pub fn analyze(&mut self, tu: &TranslationUnit) -> Result<(), Vec<String>> {
        for decl in &tu.decls {
            match decl {
                ExternalDecl::FunctionDef(func) => {
                    self.analyze_function_def(func);
                }
                ExternalDecl::Declaration(decl) => {
                    self.analyze_declaration(decl, /* is_global */ true);
                }
                ExternalDecl::TopLevelAsm(_) => {
                    // Top-level asm is passed through verbatim; no semantic analysis needed
                }
            }
        }
        // For now, we don't produce fatal errors - we collect info for the lowerer.
        // TODO: return errors for truly invalid programs
        Ok(())
    }

    /// Get the analysis results (consumed after analysis).
    pub fn into_result(self) -> SemaResult {
        // Synchronize the TypeContext's anonymous struct counter with sema's counter.
        // Both generate keys in the format "__anon_struct_{id}", and the lowerer uses
        // TypeContext's counter. Without this, the lowerer would start at 0 and
        // overwrite struct layouts that sema stored at those keys.
        let sema_count = self.anon_struct_counter.get() as u32;
        self.result.type_context.set_anon_ctype_counter(sema_count);
        self.result
    }

    /// Get a reference to the analysis results.
    pub fn result(&self) -> &SemaResult {
        &self.result
    }

    // === Function analysis ===

    fn analyze_function_def(&mut self, func: &FunctionDef) {
        let return_type = self.type_spec_to_ctype(&func.return_type);
        let params: Vec<(CType, Option<String>)> = func.params.iter().map(|p| {
            let ty = self.type_spec_to_ctype(&p.type_spec);
            (ty, p.name.clone())
        }).collect();

        let func_info = FunctionInfo {
            name: func.name.clone(),
            return_type: return_type.clone(),
            params: params.clone(),
            variadic: func.variadic,
            is_defined: true,
        };

        // Register in function table
        self.result.functions.insert(func.name.clone(), func_info);

        // Register in symbol table
        let func_ctype = CType::Function(Box::new(FunctionType {
            return_type: return_type.clone(),
            params: params.clone(),
            variadic: func.variadic,
        }));
        let storage_class = if func.is_static {
            StorageClass::Static
        } else {
            StorageClass::Extern
        };
        self.symbol_table.declare(Symbol {
            name: func.name.clone(),
            ty: func_ctype,
            storage_class,
            span: func.span,
            is_defined: true,
        });

        // Push scope for function body (both symbol table and type context,
        // so local struct/union definitions don't overwrite global layouts)
        self.symbol_table.push_scope();
        self.result.type_context.push_scope();

        // Declare parameters in function scope
        for param in &func.params {
            if let Some(name) = &param.name {
                let ty = self.type_spec_to_ctype(&param.type_spec);
                self.symbol_table.declare(Symbol {
                    name: name.clone(),
                    ty,
                    storage_class: StorageClass::Auto,
                    span: param.span,
                    is_defined: true,
                });
            }
        }

        // Analyze function body
        self.analyze_compound_stmt(&func.body);

        // Pop function scope
        self.result.type_context.pop_scope();
        self.symbol_table.pop_scope();
    }

    // === Declaration analysis ===

    fn analyze_declaration(&mut self, decl: &Declaration, is_global: bool) {
        // Register enum constants from this declaration's type specifier.
        // This recursively walks into struct/union fields to find inline
        // enum definitions (e.g., `struct { enum { A, B } mode; }`).
        self.collect_enum_constants_from_type_spec(&decl.type_spec);

        let base_type = self.type_spec_to_ctype(&decl.type_spec);

        // Handle typedef declarations: populate TypeContext with typedef info
        if decl.is_typedef {
            for declarator in &decl.declarators {
                if declarator.name.is_empty() {
                    continue;
                }
                // Check for function typedef (e.g., typedef int func_t(int, int);)
                let has_func_derived = declarator.derived.iter().any(|d|
                    matches!(d, DerivedDeclarator::Function(_, _)));
                let has_fptr_derived = declarator.derived.iter().any(|d|
                    matches!(d, DerivedDeclarator::FunctionPointer(_, _)));

                if has_func_derived && !has_fptr_derived {
                    // Function typedef like: typedef int func_t(int x);
                    if let Some(DerivedDeclarator::Function(params, variadic)) =
                        declarator.derived.iter().find(|d| matches!(d, DerivedDeclarator::Function(_, _)))
                    {
                        let ptr_count = declarator.derived.iter()
                            .take_while(|d| matches!(d, DerivedDeclarator::Pointer))
                            .count();
                        let mut return_type = decl.type_spec.clone();
                        for _ in 0..ptr_count {
                            return_type = TypeSpecifier::Pointer(Box::new(return_type));
                        }
                        self.result.type_context.function_typedefs.insert(
                            declarator.name.clone(),
                            FunctionTypedefInfo {
                                return_type,
                                params: params.clone(),
                                variadic: *variadic,
                            },
                        );
                    }
                }

                // Function pointer typedef (e.g., typedef void *(*lua_Alloc)(void *, ...))
                if has_fptr_derived {
                    if let Some(fptr_derived) = declarator.derived.iter().find(|d|
                        matches!(d, DerivedDeclarator::FunctionPointer(_, _)))
                    {
                        if let DerivedDeclarator::FunctionPointer(params, variadic) = fptr_derived {
                            let ptr_count = declarator.derived.iter()
                                .take_while(|d| matches!(d, DerivedDeclarator::Pointer))
                                .count();
                            let ret_ptr_count = if ptr_count > 0 { ptr_count - 1 } else { 0 };
                            let mut return_type = decl.type_spec.clone();
                            for _ in 0..ret_ptr_count {
                                return_type = TypeSpecifier::Pointer(Box::new(return_type));
                            }
                            self.result.type_context.func_ptr_typedefs.insert(declarator.name.clone());
                            self.result.type_context.func_ptr_typedef_info.insert(
                                declarator.name.clone(),
                                FunctionTypedefInfo {
                                    return_type,
                                    params: params.clone(),
                                    variadic: *variadic,
                                },
                            );
                        }
                    }
                }

                // Store resolved CType for the typedef
                let resolved_ctype = self.build_full_ctype(&decl.type_spec, &declarator.derived);
                self.result.type_context.typedefs.insert(declarator.name.clone(), resolved_ctype);
            }
            return; // typedefs don't declare variables
        }

        for init_decl in &decl.declarators {
            let mut full_type = if init_decl.derived.is_empty() {
                base_type.clone()
            } else {
                type_builder::build_full_ctype(self, &decl.type_spec, &init_decl.derived)
            };

            // Resolve incomplete array sizes from initializers (e.g., int arr[] = {1,2,3})
            // This must happen before storing the symbol so sizeof(arr) works in
            // subsequent global initializer const-evaluation.
            if let CType::Array(ref elem, None) = full_type {
                if let Some(ref init) = init_decl.init {
                    if let Some(count) = Self::count_initializer_elements(init, elem) {
                        full_type = CType::Array(elem.clone(), Some(count));
                    }
                }
            }

            let storage = if decl.is_extern {
                StorageClass::Extern
            } else if decl.is_static {
                StorageClass::Static
            } else if is_global {
                StorageClass::Extern
            } else {
                StorageClass::Auto
            };

            // Check if this is a function declaration (prototype)
            if let CType::Function(ref ft) = full_type {
                let func_info = FunctionInfo {
                    name: init_decl.name.clone(),
                    return_type: ft.return_type.clone(),
                    params: ft.params.clone(),
                    variadic: ft.variadic,
                    is_defined: false,
                };
                self.result.functions.entry(init_decl.name.clone())
                    .or_insert(func_info);
            }

            self.symbol_table.declare(Symbol {
                name: init_decl.name.clone(),
                ty: full_type,
                storage_class: storage,
                span: init_decl.span,
                is_defined: init_decl.init.is_some(),
            });

            // Analyze initializer expressions
            if let Some(init) = &init_decl.init {
                self.analyze_initializer(init);
            }
        }
    }

    /// Count the number of array elements from an initializer.
    /// Used to resolve incomplete array types (e.g., `int arr[] = {1,2,3}`)
    /// so that sizeof(arr) works correctly in subsequent const evaluation.
    ///
    /// Uses the same index-tracking algorithm as lowering's
    /// `compute_init_list_array_size`. Handles initializer lists with
    /// designators, string literals for char arrays, and brace-wrapped
    /// string literals.
    fn count_initializer_elements(init: &Initializer, elem_ty: &CType) -> Option<usize> {
        match init {
            Initializer::List(items) => {
                // Check for brace-wrapped string literal: char c[] = {"hello"}
                let is_char_elem = matches!(elem_ty, CType::Char | CType::UChar);
                let is_int_elem = matches!(elem_ty, CType::Int);
                if items.len() == 1 && items[0].designators.is_empty() {
                    if is_char_elem {
                        if let Initializer::Expr(Expr::StringLiteral(s, _)) = &items[0].init {
                            return Some(s.chars().count() + 1);
                        }
                    }
                    if is_int_elem {
                        if let Initializer::Expr(Expr::WideStringLiteral(s, _)) = &items[0].init {
                            // For wchar_t arrays, each Unicode codepoint is one element
                            return Some(s.chars().count() + 1);
                        }
                    }
                }

                // General case: track current index through the initializer list.
                let mut max_idx = 0usize;
                let mut current_idx = 0usize;
                for item in items {
                    // If this item has an array index designator, jump to that index
                    if let Some(designator) = item.designators.first() {
                        match designator {
                            Designator::Index(idx_expr) => {
                                if let Some(idx) = Self::eval_designator_index(idx_expr) {
                                    current_idx = idx;
                                }
                            }
                            Designator::Range(_lo, hi) => {
                                // GNU range designator [lo ... hi]: size determined by hi
                                if let Some(idx) = Self::eval_designator_index(hi) {
                                    current_idx = idx;
                                }
                            }
                            Designator::Field(_) => {
                                // Field designator on struct element - don't advance
                            }
                        }
                    }
                    if current_idx >= max_idx {
                        max_idx = current_idx + 1;
                    }
                    // Only advance if this item doesn't have a field designator
                    // (multiple items may target different fields of the same element)
                    let has_field_designator = item.designators.iter()
                        .any(|d| matches!(d, Designator::Field(_)));
                    if !has_field_designator {
                        current_idx += 1;
                    }
                }
                let has_any_designator = items.iter()
                    .any(|item| !item.designators.is_empty());
                if has_any_designator {
                    Some(max_idx)
                } else {
                    Some(max_idx.max(items.len()))
                }
            }
            Initializer::Expr(expr) => {
                // String literal initializer for char arrays: char s[] = "hello"
                match (elem_ty, expr) {
                    (CType::Char | CType::UChar, Expr::StringLiteral(s, _)) => {
                        Some(s.chars().count() + 1) // +1 for null terminator
                    }
                    (CType::Int, Expr::WideStringLiteral(s, _)) => {
                        // For wchar_t arrays, each Unicode codepoint is one element
                        Some(s.chars().count() + 1)
                    }
                    _ => None,
                }
            }
        }
    }

    /// Try to evaluate a designator index expression as a usize.
    /// Handles integer and char literals. Returns None for complex
    /// constant expressions; the lowering phase handles those cases.
    // TODO: extend to handle const expressions (e.g., enum values, sizeof)
    fn eval_designator_index(expr: &Expr) -> Option<usize> {
        match expr {
            Expr::IntLiteral(n, _) | Expr::LongLiteral(n, _) => Some(*n as usize),
            Expr::UIntLiteral(n, _) | Expr::ULongLiteral(n, _) => Some(*n as usize),
            Expr::CharLiteral(n, _) => Some(*n as usize),
            _ => None,
        }
    }

    fn process_enum_variants(&mut self, variants: &[EnumVariant]) {
        self.enum_counter = 0;
        for variant in variants {
            if let Some(val_expr) = &variant.value {
                // Walk the enum value expression through analyze_expr first to
                // populate the expr_types cache. Without this, infer_expr_ctype
                // called from eval_const_expr would have no cached sub-expression
                // types and could exhibit O(2^N) blowup on deep expression chains
                // (e.g., `enum { NUM = +1+1+1+...+1 }`).
                self.analyze_expr(val_expr);
                // Try to evaluate constant expression
                if let Some(val) = self.eval_const_expr(val_expr) {
                    self.enum_counter = val;
                }
            }
            self.result.type_context.enum_constants.insert(variant.name.clone(), self.enum_counter);
            self.symbol_table.declare(Symbol {
                name: variant.name.clone(),
                ty: CType::Int,
                storage_class: StorageClass::Auto, // enum constants are like const int
                span: Span::dummy(),
                is_defined: true,
            });
            self.enum_counter += 1;
        }
    }

    /// Recursively collect enum constants from a type specifier, walking into
    /// struct/union field types. This ensures that inline enum definitions
    /// within structs (e.g., `struct { enum { A, B } mode; }`) have their
    /// constants registered in the enclosing scope, matching GCC/Clang behavior.
    fn collect_enum_constants_from_type_spec(&mut self, ts: &TypeSpecifier) {
        match ts {
            TypeSpecifier::Enum(_, Some(variants)) => {
                self.process_enum_variants(variants);
            }
            TypeSpecifier::Struct(_, Some(fields), _, _, _)
            | TypeSpecifier::Union(_, Some(fields), _, _, _) => {
                for field in fields {
                    self.collect_enum_constants_from_type_spec(&field.type_spec);
                }
            }
            TypeSpecifier::Array(inner, _) | TypeSpecifier::Pointer(inner) => {
                self.collect_enum_constants_from_type_spec(inner);
            }
            _ => {}
        }
    }

    // === Statement analysis ===

    fn analyze_compound_stmt(&mut self, compound: &CompoundStmt) {
        self.symbol_table.push_scope();
        self.result.type_context.push_scope();
        for item in &compound.items {
            match item {
                BlockItem::Declaration(decl) => {
                    self.analyze_declaration(decl, /* is_global */ false);
                }
                BlockItem::Statement(stmt) => {
                    self.analyze_stmt(stmt);
                }
            }
        }
        self.result.type_context.pop_scope();
        self.symbol_table.pop_scope();
    }

    fn analyze_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(Some(expr)) => {
                self.analyze_expr(expr);
            }
            Stmt::Expr(None) => {}
            Stmt::Return(Some(expr), _) => {
                self.analyze_expr(expr);
            }
            Stmt::Return(None, _) => {}
            Stmt::If(cond, then_br, else_br, _) => {
                self.analyze_expr(cond);
                self.analyze_stmt(then_br);
                if let Some(else_stmt) = else_br {
                    self.analyze_stmt(else_stmt);
                }
            }
            Stmt::While(cond, body, _) => {
                self.analyze_expr(cond);
                self.analyze_stmt(body);
            }
            Stmt::DoWhile(body, cond, _) => {
                self.analyze_stmt(body);
                self.analyze_expr(cond);
            }
            Stmt::For(init, cond, inc, body, _) => {
                self.symbol_table.push_scope();
                self.result.type_context.push_scope();
                if let Some(init) = init {
                    match init.as_ref() {
                        ForInit::Declaration(decl) => {
                            self.analyze_declaration(decl, false);
                        }
                        ForInit::Expr(expr) => {
                            self.analyze_expr(expr);
                        }
                    }
                }
                if let Some(cond) = cond {
                    self.analyze_expr(cond);
                }
                if let Some(inc) = inc {
                    self.analyze_expr(inc);
                }
                self.analyze_stmt(body);
                self.result.type_context.pop_scope();
                self.symbol_table.pop_scope();
            }
            Stmt::Compound(compound) => {
                self.analyze_compound_stmt(compound);
            }
            Stmt::Switch(expr, body, _) => {
                self.analyze_expr(expr);
                self.analyze_stmt(body);
            }
            Stmt::Case(expr, body, _) => {
                self.analyze_expr(expr);
                self.analyze_stmt(body);
            }
            Stmt::CaseRange(low, high, body, _) => {
                self.analyze_expr(low);
                self.analyze_expr(high);
                self.analyze_stmt(body);
            }
            Stmt::Default(body, _) => {
                self.analyze_stmt(body);
            }
            Stmt::Label(_, body, _) => {
                self.analyze_stmt(body);
            }
            Stmt::Break(_) | Stmt::Continue(_) | Stmt::Goto(_, _) => {}
            Stmt::GotoIndirect(expr, _) => {
                self.analyze_expr(expr);
            }
            Stmt::InlineAsm { outputs, inputs, .. } => {
                for out in outputs {
                    self.analyze_expr(&out.expr);
                }
                for inp in inputs {
                    self.analyze_expr(&inp.expr);
                }
            }
        }
    }

    // === Expression analysis ===

    fn analyze_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Identifier(name, _span) => {
                // Check if it's a known symbol
                if self.symbol_table.lookup(name).is_none()
                    && !self.result.type_context.enum_constants.contains_key(name)
                    && !builtins::is_builtin(name)
                    && !self.result.functions.contains_key(name)
                    && name != "__func__" && name != "__FUNCTION__"
                    && name != "__PRETTY_FUNCTION__"
                {
                    eprintln!("warning: '{}' undeclared", name);
                }
            }
            Expr::FunctionCall(callee, args, _) => {
                // Check for builtin calls
                if let Expr::Identifier(name, _) = callee.as_ref() {
                    if builtins::is_builtin(name) {
                        // Valid builtin call - will be resolved during lowering
                    } else if self.result.functions.get(name).is_none()
                        && self.symbol_table.lookup(name).is_none()
                    {
                        // Implicit function declaration (C89 style) - register it
                        eprintln!("warning: implicit declaration of function '{}'", name);
                        let func_info = FunctionInfo {
                            name: name.clone(),
                            return_type: CType::Int, // implicit return int
                            params: Vec::new(),
                            variadic: true, // unknown params
                            is_defined: false,
                        };
                        self.result.functions.insert(name.clone(), func_info);
                    }
                }
                self.analyze_expr(callee);
                for arg in args {
                    self.analyze_expr(arg);
                }
            }
            Expr::BinaryOp(_, lhs, rhs, _) => {
                self.analyze_expr(lhs);
                self.analyze_expr(rhs);
            }
            Expr::UnaryOp(_, operand, _) => {
                self.analyze_expr(operand);
            }
            Expr::PostfixOp(_, operand, _) => {
                self.analyze_expr(operand);
            }
            Expr::Assign(lhs, rhs, _) => {
                self.analyze_expr(lhs);
                self.analyze_expr(rhs);
            }
            Expr::CompoundAssign(_, lhs, rhs, _) => {
                self.analyze_expr(lhs);
                self.analyze_expr(rhs);
            }
            Expr::Conditional(cond, then_expr, else_expr, _) => {
                self.analyze_expr(cond);
                self.analyze_expr(then_expr);
                self.analyze_expr(else_expr);
            }
            Expr::GnuConditional(cond, else_expr, _) => {
                self.analyze_expr(cond);
                self.analyze_expr(else_expr);
            }
            Expr::ArraySubscript(arr, idx, _) => {
                self.analyze_expr(arr);
                self.analyze_expr(idx);
            }
            Expr::MemberAccess(obj, _, _) => {
                self.analyze_expr(obj);
            }
            Expr::PointerMemberAccess(obj, _, _) => {
                self.analyze_expr(obj);
            }
            Expr::Cast(_, inner, _) => {
                self.analyze_expr(inner);
            }
            Expr::Sizeof(..) | Expr::Alignof(..) => {} // sizeof/_Alignof are always compile-time
            Expr::AddressOf(inner, _) => {
                self.analyze_expr(inner);
            }
            Expr::Deref(inner, _) => {
                self.analyze_expr(inner);
            }
            Expr::Comma(lhs, rhs, _) => {
                self.analyze_expr(lhs);
                self.analyze_expr(rhs);
            }
            Expr::CompoundLiteral(_, init, _) => {
                self.analyze_initializer(init);
            }
            Expr::StmtExpr(compound, _) => {
                self.analyze_compound_stmt(compound);
            }
            Expr::VaArg(ap_expr, _, _) => {
                self.analyze_expr(ap_expr);
            }
            Expr::GenericSelection(controlling, associations, _) => {
                self.analyze_expr(controlling);
                for assoc in associations {
                    self.analyze_expr(&assoc.expr);
                }
            }
            // Literals don't need analysis
            Expr::IntLiteral(_, _)
            | Expr::UIntLiteral(_, _)
            | Expr::LongLiteral(_, _)
            | Expr::ULongLiteral(_, _)
            | Expr::FloatLiteral(_, _)
            | Expr::FloatLiteralF32(_, _)
            | Expr::FloatLiteralLongDouble(_, _)
            | Expr::ImaginaryLiteral(_, _)
            | Expr::ImaginaryLiteralF32(_, _)
            | Expr::ImaginaryLiteralLongDouble(_, _)
            | Expr::StringLiteral(_, _)
            | Expr::WideStringLiteral(_, _)
            | Expr::CharLiteral(_, _) => {}
            // Label address (&&label) - just a compile-time address
            Expr::LabelAddr(_, _) => {}
            // __builtin_types_compatible_p(type1, type2) - compile-time constant
            Expr::BuiltinTypesCompatibleP(_, _, _) => {}
        }

        // After analyzing sub-expressions, infer this expression's CType using
        // the ExprTypeChecker and store it in the annotation map. The lowerer
        // will consult this map as a fast O(1) fallback before doing its own
        // (more expensive) type inference that requires lowering state.
        self.annotate_expr_type(expr);

        // Also try to evaluate this expression as a compile-time constant.
        // The lowerer will consult the const_values map as an O(1) fast path
        // before doing its own eval_const_expr.
        self.annotate_const_value(expr);
    }

    /// Infer the CType of an expression via ExprTypeChecker and record it
    /// in the expr_types annotation map, keyed by the expression's pointer address.
    fn annotate_expr_type(&mut self, expr: &Expr) {
        let checker = super::type_checker::ExprTypeChecker {
            symbols: &self.symbol_table,
            types: &self.result.type_context,
            functions: &self.result.functions,
            expr_types: Some(&self.result.expr_types),
        };
        if let Some(ctype) = checker.infer_expr_ctype(expr) {
            let key = expr as *const Expr as usize;
            self.result.expr_types.insert(key, ctype);
        }
    }

    /// Try to evaluate an expression as a compile-time constant using
    /// SemaConstEval and record the result in the const_values map.
    /// The lowerer will consult this as an O(1) fast path.
    fn annotate_const_value(&mut self, expr: &Expr) {
        let evaluator = SemaConstEval {
            types: &self.result.type_context,
            symbols: &self.symbol_table,
            functions: &self.result.functions,
            const_values: Some(&self.result.const_values),
            expr_types: Some(&self.result.expr_types),
        };
        if let Some(val) = evaluator.eval_const_expr(expr) {
            let key = expr as *const Expr as usize;
            self.result.const_values.insert(key, val);
        }
    }

    fn analyze_initializer(&mut self, init: &Initializer) {
        match init {
            Initializer::Expr(expr) => {
                self.analyze_expr(expr);
            }
            Initializer::List(items) => {
                for item in items {
                    self.analyze_initializer(&item.init);
                }
            }
        }
    }

    // === Type conversion utilities ===

    /// Convert an AST TypeSpecifier to a CType.
    /// Delegates to the shared `TypeConvertContext::resolve_type_spec_to_ctype` default
    /// method, which handles all 22 primitive types and delegates struct/union/enum/typedef
    /// to sema-specific trait methods.
    fn type_spec_to_ctype(&self, spec: &TypeSpecifier) -> CType {
        use crate::common::type_builder::TypeConvertContext;
        self.resolve_type_spec_to_ctype(spec)
    }

    fn convert_struct_fields(&self, fields: &[StructFieldDecl]) -> Vec<crate::common::types::StructField> {
        fields.iter().filter_map(|f| {
            let ty = if f.derived.is_empty() {
                self.type_spec_to_ctype(&f.type_spec)
            } else {
                // Delegate to shared build_full_ctype for correct inside-out type construction
                type_builder::build_full_ctype(self, &f.type_spec, &f.derived)
            };
            let name = f.name.clone().unwrap_or_default();
            let bit_width = f.bit_width.as_ref().and_then(|bw| {
                self.eval_const_expr(bw).map(|v| v as u32)
            });
            Some(crate::common::types::StructField {
                name,
                ty,
                bit_width,
                alignment: f.alignment,
            })
        }).collect()
    }

    /// Build a full CType from a TypeSpecifier + derived declarators.
    /// Delegates to the shared type_builder module for canonical inside-out
    /// declarator application logic.
    fn build_full_ctype(&self, type_spec: &TypeSpecifier, derived: &[DerivedDeclarator]) -> CType {
        type_builder::build_full_ctype(self, type_spec, derived)
    }

    // === Constant expression evaluation ===

    /// Try to evaluate a constant expression at compile time.
    /// Returns None if the expression cannot be evaluated.
    ///
    /// This delegates to the richer SemaConstEval which returns IrConst,
    /// then extracts the i64 value. This ensures enum values, bitfield
    /// widths, and array sizes all use the same evaluation logic.
    fn eval_const_expr(&self, expr: &Expr) -> Option<i64> {
        let evaluator = SemaConstEval {
            types: &self.result.type_context,
            symbols: &self.symbol_table,
            functions: &self.result.functions,
            const_values: Some(&self.result.const_values),
            expr_types: Some(&self.result.expr_types),
        };
        evaluator.eval_const_expr(expr)?.to_i64()
    }

    // === Implicit declarations ===

    /// Pre-declare common implicit functions that C programs expect.
    fn declare_implicit_functions(&mut self) {
        // Common libc functions that tests may use without headers
        let implicit_funcs = [
            ("printf", CType::Int, true),
            ("fprintf", CType::Int, true),
            ("sprintf", CType::Int, true),
            ("snprintf", CType::Int, true),
            ("puts", CType::Int, false),
            ("putchar", CType::Int, false),
            ("getchar", CType::Int, false),
            ("malloc", CType::Pointer(Box::new(CType::Void)), false),
            ("calloc", CType::Pointer(Box::new(CType::Void)), false),
            ("realloc", CType::Pointer(Box::new(CType::Void)), false),
            ("free", CType::Void, false),
            ("exit", CType::Void, false),
            ("abort", CType::Void, false),
            ("memcpy", CType::Pointer(Box::new(CType::Void)), false),
            ("memmove", CType::Pointer(Box::new(CType::Void)), false),
            ("memset", CType::Pointer(Box::new(CType::Void)), false),
            ("memcmp", CType::Int, false),
            ("strlen", CType::ULong, false),
            ("strcmp", CType::Int, false),
            ("strcpy", CType::Pointer(Box::new(CType::Char)), false),
            ("atoi", CType::Int, false),
            ("atol", CType::Long, false),
            ("abs", CType::Int, false),
        ];

        for (name, ret_type, variadic) in &implicit_funcs {
            let func_info = FunctionInfo {
                name: name.to_string(),
                return_type: ret_type.clone(),
                params: Vec::new(),
                variadic: *variadic,
                is_defined: false,
            };
            self.result.functions.insert(name.to_string(), func_info);
        }
    }
}

/// Implement TypeConvertContext so shared type_builder functions can call back
/// into sema for type resolution and constant expression evaluation.
///
/// The 4 divergent methods handle sema-specific behavior:
/// - typedef: looks up in type_context.typedefs
/// - struct/union: converts fields and computes layout
/// - enum: returns CType::Enum with name info (preserves enum identity)
/// - typeof: returns CType::Int (sema doesn't have full expr type resolution yet)
impl type_builder::TypeConvertContext for SemanticAnalyzer {
    fn resolve_typedef(&self, name: &str) -> CType {
        if let Some(resolved) = self.result.type_context.typedefs.get(name) {
            resolved.clone()
        } else {
            CType::Int
        }
    }

    fn resolve_struct_or_union(
        &self,
        name: &Option<String>,
        fields: &Option<Vec<StructFieldDecl>>,
        is_union: bool,
        is_packed: bool,
        pragma_pack: Option<usize>,
        struct_aligned: Option<usize>,
    ) -> CType {
        let prefix = if is_union { "union" } else { "struct" };
        let struct_fields = fields.as_ref().map(|f| self.convert_struct_fields(f)).unwrap_or_default();
        let max_field_align = if is_packed { Some(1) } else { pragma_pack };
        let key = if let Some(tag) = name {
            format!("{}.{}", prefix, tag)
        } else {
            let id = self.anon_struct_counter.get();
            self.anon_struct_counter.set(id + 1);
            format!("__anon_struct_{}", id)
        };
        if !struct_fields.is_empty() {
            let mut layout = if is_union {
                StructLayout::for_union(&struct_fields, &self.result.type_context.struct_layouts)
            } else {
                StructLayout::for_struct_with_packing(
                    &struct_fields, max_field_align, &self.result.type_context.struct_layouts
                )
            };
            if is_packed && is_union {
                layout.align = 1;
                layout.size = layout.fields.iter()
                    .map(|f| f.ty.size_ctx(&self.result.type_context.struct_layouts))
                    .max().unwrap_or(0);
            } else if !is_packed && !is_union {
                // pragma pack for non-packed structs is already handled by for_struct_with_packing
            } else if let Some(pack) = pragma_pack {
                if is_union && pack < layout.align {
                    layout.align = pack;
                    let mask = layout.align - 1;
                    layout.size = (layout.size + mask) & !mask;
                }
            }
            if let Some(a) = struct_aligned {
                if a > layout.align {
                    layout.align = a;
                    let mask = layout.align - 1;
                    layout.size = (layout.size + mask) & !mask;
                }
            }
            self.result.type_context.insert_struct_layout_scoped_from_ref(&key, layout);
        } else if self.result.type_context.struct_layouts.get(&key).is_none() {
            let layout = StructLayout {
                fields: Vec::new(),
                size: 0,
                align: 1,
                is_union,
                is_transparent_union: false,
            };
            self.result.type_context.insert_struct_layout_scoped_from_ref(&key, layout);
        }
        if is_union { CType::Union(key.into()) } else { CType::Struct(key.into()) }
    }

    fn resolve_enum(&self, name: &Option<String>, _variants: &Option<Vec<EnumVariant>>) -> CType {
        // Sema preserves enum identity for diagnostics. Variant processing is
        // done separately via process_enum_variants (requires &mut self).
        CType::Enum(crate::common::types::EnumType {
            name: name.clone(),
            variants: Vec::new(), // TODO: carry variant info
        })
    }

    fn resolve_typeof_expr(&self, expr: &Expr) -> CType {
        // Use the ExprTypeChecker to infer typeof(expr) from sema state.
        let checker = super::type_checker::ExprTypeChecker {
            symbols: &self.symbol_table,
            types: &self.result.type_context,
            functions: &self.result.functions,
            expr_types: Some(&self.result.expr_types),
        };
        checker.infer_expr_ctype(expr).unwrap_or(CType::Int)
    }

    fn eval_const_expr_as_usize(&self, expr: &Expr) -> Option<usize> {
        self.eval_const_expr(expr).map(|v| v as usize)
    }
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}
