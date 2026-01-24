//! Semantic analysis pass.
//!
//! Walks the AST to:
//! - Build a scoped symbol table of declarations
//! - Track function signatures for call validation
//! - Resolve typedef names
//! - Collect information needed by the IR lowering phase
//! - Map __builtin_* identifiers to their libc equivalents
//!
//! This pass does NOT reject programs with type errors (yet); it collects
//! information for the lowerer. Full type checking is TODO.

use crate::common::symbol_table::{Symbol, SymbolTable, StorageClass};
use crate::common::types::{CType, FunctionType};
use crate::common::source::Span;
use crate::frontend::parser::ast::*;
use crate::frontend::sema::builtins;

use std::collections::HashMap;

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
#[derive(Debug, Default)]
pub struct SemaResult {
    /// Function signatures discovered during analysis.
    pub functions: HashMap<String, FunctionInfo>,
    /// Typedef mappings (name -> resolved CType).
    pub typedefs: HashMap<String, CType>,
    /// Enum constant values (name -> integer value).
    pub enum_constants: HashMap<String, i64>,
    /// Warnings collected during analysis (non-fatal).
    pub warnings: Vec<String>,
}

/// Semantic analyzer that builds a scoped symbol table and collects type info.
pub struct SemanticAnalyzer {
    /// The scoped symbol table for name resolution.
    symbol_table: SymbolTable,
    /// Accumulated results for the lowerer.
    result: SemaResult,
    /// Current enum counter for auto-incrementing enum values.
    enum_counter: i64,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut analyzer = Self {
            symbol_table: SymbolTable::new(),
            result: SemaResult::default(),
            enum_counter: 0,
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

        for init_decl in &decl.declarators {
            let full_type = self.apply_derived_declarators(&base_type, &init_decl.derived);
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
            self.result.enum_constants.insert(variant.name.clone(), self.enum_counter);
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
                    && !self.result.enum_constants.contains_key(name)
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
    fn type_spec_to_ctype(&mut self, spec: &TypeSpecifier) -> CType {
        match spec {
            TypeSpecifier::Void => CType::Void,
            TypeSpecifier::Char => CType::Char,
            TypeSpecifier::Short => CType::Short,
            TypeSpecifier::Int => CType::Int,
            TypeSpecifier::Long => CType::Long,
            TypeSpecifier::LongLong => CType::LongLong,
            TypeSpecifier::Float => CType::Float,
            TypeSpecifier::Double => CType::Double,
            TypeSpecifier::LongDouble => CType::LongDouble,
            TypeSpecifier::Signed => CType::Int,
            TypeSpecifier::Unsigned => CType::UInt,
            TypeSpecifier::UnsignedChar => CType::UChar,
            TypeSpecifier::UnsignedShort => CType::UShort,
            TypeSpecifier::UnsignedInt => CType::UInt,
            TypeSpecifier::UnsignedLong => CType::ULong,
            TypeSpecifier::UnsignedLongLong => CType::ULongLong,
            TypeSpecifier::Int128 => CType::Int128,
            TypeSpecifier::UnsignedInt128 => CType::UInt128,
            TypeSpecifier::Bool => CType::Bool,
            TypeSpecifier::ComplexFloat => CType::ComplexFloat,
            TypeSpecifier::ComplexDouble => CType::ComplexDouble,
            TypeSpecifier::ComplexLongDouble => CType::ComplexLongDouble,
            TypeSpecifier::Pointer(inner) => {
                let inner_type = self.type_spec_to_ctype(inner);
                CType::Pointer(Box::new(inner_type))
            }
            TypeSpecifier::Array(elem, size_expr) => {
                let elem_type = self.type_spec_to_ctype(elem);
                let size = size_expr.as_ref().and_then(|e| self.eval_const_expr(e).map(|v| v as usize));
                CType::Array(Box::new(elem_type), size)
            }
            TypeSpecifier::Struct(name, fields, is_packed, pragma_pack_align, struct_aligned) => {
                let struct_fields = fields.as_ref().map(|f| self.convert_struct_fields(f)).unwrap_or_default();
                let effective_align = if *is_packed {
                    Some(1)
                } else {
                    *pragma_pack_align
                };
                if struct_fields.is_empty() {
                    let mut st = crate::common::types::StructType::new_empty(name.clone(), *is_packed, effective_align);
                    if let Some(a) = struct_aligned {
                        st.apply_min_alignment(*a);
                    }
                    CType::Struct(std::sync::Arc::new(st))
                } else {
                    let mut st = crate::common::types::StructType::new_struct(name.clone(), struct_fields, *is_packed, effective_align);
                    if let Some(a) = struct_aligned {
                        st.apply_min_alignment(*a);
                    }
                    CType::Struct(std::sync::Arc::new(st))
                }
            }
            TypeSpecifier::Union(name, fields, is_packed, pragma_pack_align, struct_aligned) => {
                let union_fields = fields.as_ref().map(|f| self.convert_struct_fields(f)).unwrap_or_default();
                let effective_align = if *is_packed {
                    Some(1)
                } else {
                    *pragma_pack_align
                };
                if union_fields.is_empty() {
                    let mut st = crate::common::types::StructType::new_empty(name.clone(), *is_packed, effective_align);
                    if let Some(a) = struct_aligned {
                        st.apply_min_alignment(*a);
                    }
                    CType::Union(std::sync::Arc::new(st))
                } else {
                    let mut st = crate::common::types::StructType::new_union(name.clone(), union_fields, *is_packed, effective_align);
                    if let Some(a) = struct_aligned {
                        st.apply_min_alignment(*a);
                    }
                    CType::Union(std::sync::Arc::new(st))
                }
            }
            TypeSpecifier::Enum(name, variants) => {
                if let Some(variants) = variants {
                    self.process_enum_variants(variants);
                }
                CType::Enum(crate::common::types::EnumType {
                    name: name.clone(),
                    variants: Vec::new(), // TODO: carry variant info
                })
            }
            TypeSpecifier::TypedefName(name) => {
                // Look up the typedef
                if let Some(resolved) = self.result.typedefs.get(name) {
                    resolved.clone()
                } else {
                    // Unknown typedef - treat as int (common fallback)
                    CType::Int
                }
            }
            TypeSpecifier::Typeof(_expr) => {
                // typeof(expr): would need expression type inference
                // For now, treat as int (sema doesn't have full expr type resolution)
                CType::Int
            }
            TypeSpecifier::TypeofType(inner) => {
                // typeof(type-name): just resolve the inner type
                self.type_spec_to_ctype(inner)
            }
        }
    }

    fn convert_struct_fields(&mut self, fields: &[StructFieldDecl]) -> Vec<crate::common::types::StructField> {
        fields.iter().filter_map(|f| {
            let ty = if f.derived.is_empty() {
                self.type_spec_to_ctype(&f.type_spec)
            } else {
                // Complex declarator (function pointer, etc.): apply derived
                let base = self.type_spec_to_ctype(&f.type_spec);
                self.apply_derived_declarators(&base, &f.derived)
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

    /// Apply derived declarators (pointers, arrays, function params) to a base type.
    fn apply_derived_declarators(&mut self, base: &CType, derived: &[DerivedDeclarator]) -> CType {
        // For multi-dimensional arrays like int arr[N][M], the derived list
        // contains [Array(N), Array(M)]. To build the correct type
        // Array(Array(int, M), N), we must apply consecutive array dimensions
        // in reverse order (innermost/rightmost first).
        let mut ty = base.clone();
        let mut i = 0;
        while i < derived.len() {
            match &derived[i] {
                DerivedDeclarator::Pointer => {
                    ty = CType::Pointer(Box::new(ty));
                    i += 1;
                }
                DerivedDeclarator::Array(_) => {
                    // Collect consecutive array dimensions
                    let start = i;
                    let mut array_sizes: Vec<Option<usize>> = Vec::new();
                    while i < derived.len() {
                        if let DerivedDeclarator::Array(size_expr) = &derived[i] {
                            let size = size_expr.as_ref()
                                .and_then(|e| self.eval_const_expr(e).map(|v| v as usize));
                            array_sizes.push(size);
                            i += 1;
                        } else {
                            break;
                        }
                    }
                    // Apply in reverse: innermost (rightmost) dimension wraps first
                    for size in array_sizes.into_iter().rev() {
                        ty = CType::Array(Box::new(ty), size);
                    }
                }
                DerivedDeclarator::Function(params, variadic)
                | DerivedDeclarator::FunctionPointer(params, variadic) => {
                    let param_types: Vec<(CType, Option<String>)> = params.iter().map(|p| {
                        let pt = self.type_spec_to_ctype(&p.type_spec);
                        (pt, p.name.clone())
                    }).collect();
                    let variadic = *variadic;
                    ty = CType::Function(Box::new(FunctionType {
                        return_type: ty,
                        params: param_types,
                        variadic,
                    }));
                    i += 1;
                }
            }
        }
        ty
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
                self.result.enum_constants.get(name).copied()
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

    /// Rough sizeof for a type specifier, used in constant expression evaluation.
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
            TypeSpecifier::Pointer(_) => 8,
            TypeSpecifier::Array(elem, Some(size)) => {
                let elem_size = self.sizeof_type_spec(elem);
                if let Some(n) = self.eval_const_expr(size) {
                    elem_size * n as usize
                } else {
                    8 // fallback
                }
            }
            TypeSpecifier::Array(_, None) => 8, // incomplete array
            _ => 8, // default for structs, enums, etc.
        }
    }

    /// Rough alignof for a type specifier.
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
            TypeSpecifier::Pointer(_) => 8,
            TypeSpecifier::Array(elem, _) => self.alignof_type_spec(elem),
            _ => 8, // default for structs, enums, etc.
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

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}
