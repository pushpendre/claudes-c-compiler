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
    /// Warnings collected during analysis (non-fatal).
    pub warnings: Vec<String>,
    /// Expression type annotations: maps AST Expr node addresses to their
    /// inferred CTypes. Populated during sema's analyze_expr walk using
    /// ExprTypeChecker. The lowerer consults this as a fallback in
    /// get_expr_ctype() after its own lowering-state-based inference fails.
    pub expr_types: ExprTypeMap,
}

impl Default for SemaResult {
    fn default() -> Self {
        Self {
            functions: FxHashMap::default(),
            type_context: TypeContext::new(),
            warnings: Vec::new(),
            expr_types: ExprTypeMap::default(),
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
            }
        }
        // For now, we don't produce fatal errors - we collect info for the lowerer.
        // TODO: return errors for truly invalid programs
        Ok(())
    }

    /// Get the analysis results (consumed after analysis).
    pub fn into_result(self) -> SemaResult {
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

        // Push scope for function body
        self.symbol_table.push_scope();

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
        self.symbol_table.pop_scope();
    }

    // === Declaration analysis ===

    fn analyze_declaration(&mut self, decl: &Declaration, is_global: bool) {
        // Check for enum declarations (which define constants)
        if let TypeSpecifier::Enum(_, Some(variants)) = &decl.type_spec {
            self.process_enum_variants(variants);
        }

        // Check for struct/union definitions
        // (These are registered implicitly through type_spec_to_ctype)

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
            let full_type = if init_decl.derived.is_empty() {
                base_type.clone()
            } else {
                type_builder::build_full_ctype(self, &decl.type_spec, &init_decl.derived)
            };
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

    fn process_enum_variants(&mut self, variants: &[EnumVariant]) {
        self.enum_counter = 0;
        for variant in variants {
            if let Some(val_expr) = &variant.value {
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

    // === Statement analysis ===

    fn analyze_compound_stmt(&mut self, compound: &CompoundStmt) {
        self.symbol_table.push_scope();
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
                    // Emit warning for undeclared identifier
                    let warning = format!("'{}' undeclared", name);
                    eprintln!("warning: {}", warning);
                    self.result.warnings.push(warning);
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
                        // Emit warning matching GCC's -Wimplicit-function-declaration format
                        let warning = format!(
                            "implicit declaration of function '{}'",
                            name
                        );
                        eprintln!("warning: {}", warning);
                        self.result.warnings.push(warning);
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
    }

    /// Infer the CType of an expression via ExprTypeChecker and record it
    /// in the expr_types annotation map, keyed by the expression's pointer address.
    fn annotate_expr_type(&mut self, expr: &Expr) {
        let checker = super::type_checker::ExprTypeChecker {
            symbols: &self.symbol_table,
            types: &self.result.type_context,
            functions: &self.result.functions,
        };
        if let Some(ctype) = checker.infer_expr_ctype(expr) {
            let key = expr as *const Expr as usize;
            self.result.expr_types.insert(key, ctype);
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
    fn eval_const_expr(&self, expr: &Expr) -> Option<i64> {
        match expr {
            Expr::IntLiteral(val, _) | Expr::LongLiteral(val, _) => Some(*val),
            Expr::UIntLiteral(val, _) | Expr::ULongLiteral(val, _) => Some(*val as i64),
            Expr::CharLiteral(ch, _) => Some(*ch as i64),
            Expr::Identifier(name, _) => {
                self.result.type_context.enum_constants.get(name).copied()
            }
            Expr::BinaryOp(op, lhs, rhs, _) => {
                let l = self.eval_const_expr(lhs)?;
                let r = self.eval_const_expr(rhs)?;
                Some(match op {
                    BinOp::Add => l.wrapping_add(r),
                    BinOp::Sub => l.wrapping_sub(r),
                    BinOp::Mul => l.wrapping_mul(r),
                    BinOp::Div => if r != 0 { l.wrapping_div(r) } else { return None },
                    BinOp::Mod => if r != 0 { l.wrapping_rem(r) } else { return None },
                    BinOp::BitAnd => l & r,
                    BinOp::BitOr => l | r,
                    BinOp::BitXor => l ^ r,
                    BinOp::Shl => l.wrapping_shl(r as u32),
                    BinOp::Shr => l.wrapping_shr(r as u32),
                    BinOp::Eq => if l == r { 1 } else { 0 },
                    BinOp::Ne => if l != r { 1 } else { 0 },
                    BinOp::Lt => if l < r { 1 } else { 0 },
                    BinOp::Le => if l <= r { 1 } else { 0 },
                    BinOp::Gt => if l > r { 1 } else { 0 },
                    BinOp::Ge => if l >= r { 1 } else { 0 },
                    BinOp::LogicalAnd => if l != 0 && r != 0 { 1 } else { 0 },
                    BinOp::LogicalOr => if l != 0 || r != 0 { 1 } else { 0 },
                })
            }
            Expr::UnaryOp(op, inner, _) => {
                let v = self.eval_const_expr(inner)?;
                Some(match op {
                    UnaryOp::Neg => v.wrapping_neg(),
                    UnaryOp::BitNot => !v,
                    UnaryOp::LogicalNot => if v == 0 { 1 } else { 0 },
                    UnaryOp::Plus => v,
                    _ => return None,
                })
            }
            Expr::Cast(_, inner, _) => {
                // Simple cast: try evaluating the inner expression
                self.eval_const_expr(inner)
            }
            Expr::GnuConditional(cond, else_expr, _) => {
                let c = self.eval_const_expr(cond)?;
                if c != 0 {
                    Some(c) // condition value is used as result
                } else {
                    self.eval_const_expr(else_expr)
                }
            }
            Expr::Conditional(cond, then_expr, else_expr, _) => {
                let c = self.eval_const_expr(cond)?;
                if c != 0 {
                    self.eval_const_expr(then_expr)
                } else {
                    self.eval_const_expr(else_expr)
                }
            }
            Expr::Sizeof(arg, _) => {
                // TODO: compute sizeof for types
                match arg.as_ref() {
                    SizeofArg::Type(type_spec) => {
                        // Rough size computation
                        Some(self.sizeof_type_spec(type_spec) as i64)
                    }
                    SizeofArg::Expr(_) => {
                        // Can't easily determine type of expression here
                        None
                    }
                }
            }
            Expr::Alignof(ref ts, _) => {
                Some(self.alignof_type_spec(ts) as i64)
            }
            _ => None,
        }
    }

    /// Compute sizeof for a type specifier, used in constant expression evaluation.
    /// This must accurately handle struct/union types by looking up their cached layouts,
    /// since array dimensions in struct fields may reference sizeof(struct_type).
    fn sizeof_type_spec(&self, spec: &TypeSpecifier) -> usize {
        match spec {
            TypeSpecifier::Void => 0,
            TypeSpecifier::Char | TypeSpecifier::UnsignedChar => 1,
            TypeSpecifier::Short | TypeSpecifier::UnsignedShort => 2,
            TypeSpecifier::Int | TypeSpecifier::UnsignedInt | TypeSpecifier::Signed | TypeSpecifier::Unsigned => 4,
            TypeSpecifier::Long | TypeSpecifier::UnsignedLong | TypeSpecifier::LongLong | TypeSpecifier::UnsignedLongLong => 8,
            TypeSpecifier::Int128 | TypeSpecifier::UnsignedInt128 => 16,
            TypeSpecifier::Float => 4,
            TypeSpecifier::Double => 8,
            TypeSpecifier::LongDouble => 16,
            TypeSpecifier::Bool => 1,
            TypeSpecifier::ComplexFloat => 8,
            TypeSpecifier::ComplexDouble => 16,
            TypeSpecifier::ComplexLongDouble => 32,
            TypeSpecifier::Pointer(_) => 8,
            TypeSpecifier::FunctionPointer(_, _, _) => 8,
            TypeSpecifier::Array(elem, Some(size)) => {
                let elem_size = self.sizeof_type_spec(elem);
                if let Some(n) = self.eval_const_expr(size) {
                    elem_size * n as usize
                } else {
                    8 // fallback
                }
            }
            TypeSpecifier::Array(_, None) => 8, // incomplete array
            TypeSpecifier::Struct(tag, fields, is_packed, pragma_pack, struct_aligned) => {
                // Look up cached layout for tagged structs
                if let Some(tag) = tag {
                    let key = format!("struct.{}", tag);
                    if let Some(layout) = self.result.type_context.struct_layouts.get(&key) {
                        return layout.size;
                    }
                }
                // For inline struct definitions, compute from fields
                if let Some(fields) = fields {
                    let struct_fields = self.convert_struct_fields(fields);
                    if !struct_fields.is_empty() {
                        let max_field_align = if *is_packed { Some(1) } else { *pragma_pack };
                        let mut layout = StructLayout::for_struct_with_packing(
                            &struct_fields, max_field_align, &self.result.type_context.struct_layouts
                        );
                        if let Some(a) = struct_aligned {
                            if *a > layout.align {
                                layout.align = *a;
                                let mask = layout.align - 1;
                                layout.size = (layout.size + mask) & !mask;
                            }
                        }
                        return layout.size;
                    }
                }
                0 // incomplete struct
            }
            TypeSpecifier::Union(tag, fields, is_packed, pragma_pack, struct_aligned) => {
                // Look up cached layout for tagged unions
                if let Some(tag) = tag {
                    let key = format!("union.{}", tag);
                    if let Some(layout) = self.result.type_context.struct_layouts.get(&key) {
                        return layout.size;
                    }
                }
                // For inline union definitions, compute from fields
                if let Some(fields) = fields {
                    let union_fields = self.convert_struct_fields(fields);
                    if !union_fields.is_empty() {
                        let mut layout = StructLayout::for_union(&union_fields, &self.result.type_context.struct_layouts);
                        if *is_packed {
                            layout.align = 1;
                            layout.size = layout.fields.iter().map(|f| f.ty.size_ctx(&self.result.type_context.struct_layouts)).max().unwrap_or(0);
                        } else if let Some(pack) = pragma_pack {
                            if *pack < layout.align {
                                layout.align = *pack;
                                let mask = layout.align - 1;
                                layout.size = (layout.size + mask) & !mask;
                            }
                        }
                        if let Some(a) = struct_aligned {
                            if *a > layout.align {
                                layout.align = *a;
                                let mask = layout.align - 1;
                                layout.size = (layout.size + mask) & !mask;
                            }
                        }
                        return layout.size;
                    }
                }
                0 // incomplete union
            }
            TypeSpecifier::Enum(_, _) => 4, // enums are int-sized
            TypeSpecifier::TypedefName(name) => {
                // Resolve typedef and compute size from the resolved CType
                if let Some(ctype) = self.result.type_context.typedefs.get(name) {
                    ctype.size_ctx(&self.result.type_context.struct_layouts)
                } else {
                    8 // fallback for unknown typedef
                }
            }
            TypeSpecifier::TypeofType(inner) => self.sizeof_type_spec(inner),
            _ => 8, // fallback for remaining cases (Typeof expr, etc.)
        }
    }

    /// Compute alignof for a type specifier, used in constant expression evaluation.
    /// Must accurately handle struct/union types by looking up their cached layouts.
    fn alignof_type_spec(&self, spec: &TypeSpecifier) -> usize {
        match spec {
            TypeSpecifier::Void | TypeSpecifier::Bool => 1,
            TypeSpecifier::Char | TypeSpecifier::UnsignedChar => 1,
            TypeSpecifier::Short | TypeSpecifier::UnsignedShort => 2,
            TypeSpecifier::Int | TypeSpecifier::UnsignedInt | TypeSpecifier::Signed | TypeSpecifier::Unsigned => 4,
            TypeSpecifier::Long | TypeSpecifier::UnsignedLong | TypeSpecifier::LongLong | TypeSpecifier::UnsignedLongLong => 8,
            TypeSpecifier::Int128 | TypeSpecifier::UnsignedInt128 => 16,
            TypeSpecifier::Float => 4,
            TypeSpecifier::Double => 8,
            TypeSpecifier::LongDouble => 16,
            TypeSpecifier::ComplexFloat => 4,
            TypeSpecifier::ComplexDouble => 8,
            TypeSpecifier::ComplexLongDouble => 16,
            TypeSpecifier::Pointer(_) | TypeSpecifier::FunctionPointer(_, _, _) => 8,
            TypeSpecifier::Array(elem, _) => self.alignof_type_spec(elem),
            TypeSpecifier::Struct(tag, fields, is_packed, pragma_pack, struct_aligned) => {
                // Look up cached layout for tagged structs
                if let Some(tag) = tag {
                    let key = format!("struct.{}", tag);
                    if let Some(layout) = self.result.type_context.struct_layouts.get(&key) {
                        return layout.align;
                    }
                }
                // For inline struct definitions, compute from fields
                if let Some(fields) = fields {
                    let struct_fields = self.convert_struct_fields(fields);
                    if !struct_fields.is_empty() {
                        let max_field_align = if *is_packed { Some(1) } else { *pragma_pack };
                        let mut layout = StructLayout::for_struct_with_packing(
                            &struct_fields, max_field_align, &self.result.type_context.struct_layouts
                        );
                        if let Some(a) = struct_aligned {
                            if *a > layout.align {
                                layout.align = *a;
                            }
                        }
                        return layout.align;
                    }
                }
                1 // incomplete struct
            }
            TypeSpecifier::Union(tag, fields, is_packed, _pragma_pack, struct_aligned) => {
                // Look up cached layout for tagged unions
                if let Some(tag) = tag {
                    let key = format!("union.{}", tag);
                    if let Some(layout) = self.result.type_context.struct_layouts.get(&key) {
                        return layout.align;
                    }
                }
                // For inline union definitions, compute from fields
                if let Some(fields) = fields {
                    let union_fields = self.convert_struct_fields(fields);
                    if !union_fields.is_empty() {
                        let mut layout = StructLayout::for_union(&union_fields, &self.result.type_context.struct_layouts);
                        if *is_packed {
                            layout.align = 1;
                        }
                        if let Some(a) = struct_aligned {
                            if *a > layout.align {
                                layout.align = *a;
                            }
                        }
                        return layout.align;
                    }
                }
                1 // incomplete union
            }
            TypeSpecifier::Enum(_, _) => 4, // enums are int-aligned
            TypeSpecifier::TypedefName(name) => {
                // Resolve typedef and compute alignment from the resolved CType
                if let Some(ctype) = self.result.type_context.typedefs.get(name) {
                    ctype.align_ctx(&self.result.type_context.struct_layouts)
                } else {
                    8 // fallback for unknown typedef
                }
            }
            TypeSpecifier::TypeofType(inner) => self.alignof_type_spec(inner),
            _ => 8, // fallback for remaining cases
        }
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
            self.result.type_context.insert_struct_layout_from_ref(&key, layout);
        } else if self.result.type_context.struct_layouts.get(&key).is_none() {
            let layout = StructLayout {
                fields: Vec::new(),
                size: 0,
                align: 1,
                is_union,
                is_transparent_union: false,
            };
            self.result.type_context.insert_struct_layout_from_ref(&key, layout);
        }
        if is_union { CType::Union(key) } else { CType::Struct(key) }
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
