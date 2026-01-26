//! Expression-level CType inference for semantic analysis.
//!
//! This module provides CType inference for expressions using only sema-available
//! state: SymbolTable (variable/function types), TypeContext (typedefs, struct
//! layouts, enum constants), and FunctionInfo (function signatures).
//!
//! This is a key step toward making sema produce a typed AST: by inferring
//! expression types in sema rather than the lowerer, we can:
//! - Report type errors as diagnostics instead of lowering panics
//! - Enable future type annotations on AST nodes
//! - Reduce the lowerer's responsibility from type-checking + IR emission to
//!   just IR emission
//!
//! The `ExprTypeChecker` operates on immutable references and does not modify
//! any state. It is designed to be called from `SemanticAnalyzer::analyze_expr`.

use crate::common::types::{AddressSpace, CType, FunctionType};
use crate::common::symbol_table::SymbolTable;
use crate::common::fx_hash::FxHashMap;
use crate::frontend::parser::ast::*;
use super::type_context::TypeContext;
use super::sema::FunctionInfo;

/// Expression type checker that infers CTypes using only sema-available state.
///
/// This struct borrows the sema state needed for type inference and provides
/// methods to compute the CType of any expression. Unlike the lowerer's
/// `get_expr_ctype`, this does not depend on IR-level state (allocas, IR types,
/// global variable metadata).
///
/// When `expr_types` is provided, previously-computed types are looked up in O(1)
/// instead of re-traversing the AST. This prevents exponential blowup on deeply
/// nested expressions like `1+1+1+...+1` where `infer_binop_ctype` would
/// otherwise recursively re-evaluate lhs/rhs types multiple times per node.
pub struct ExprTypeChecker<'a> {
    /// Symbol table for variable/function type lookup.
    pub symbols: &'a SymbolTable,
    /// Type context for typedef, enum, and struct layout resolution.
    pub types: &'a TypeContext,
    /// Function signatures for return type resolution.
    pub functions: &'a FxHashMap<String, FunctionInfo>,
    /// Pre-computed expression types from bottom-up sema walk (memoization cache).
    /// When set, `infer_expr_ctype` checks this map before recursing.
    pub expr_types: Option<&'a FxHashMap<usize, CType>>,
}

impl<'a> ExprTypeChecker<'a> {
    /// Infer the CType of an expression.
    ///
    /// Returns `Some(CType)` when the type can be determined from sema state,
    /// `None` when the type depends on lowering-specific information (e.g.,
    /// complex expression chains through typeof).
    pub fn infer_expr_ctype(&self, expr: &Expr) -> Option<CType> {
        // Memoization: if this expression's type was already computed during
        // the bottom-up sema walk, return it in O(1) instead of re-traversing.
        // This prevents O(2^N) blowup on deep expression chains.
        if let Some(cache) = self.expr_types {
            let key = expr as *const Expr as usize;
            if let Some(cached) = cache.get(&key) {
                return Some(cached.clone());
            }
        }

        match expr {
            // Literals have well-defined types
            Expr::IntLiteral(_, _) | Expr::CharLiteral(_, _) => Some(CType::Int),
            Expr::UIntLiteral(_, _) => Some(CType::UInt),
            Expr::LongLiteral(_, _) => Some(CType::Long),
            Expr::ULongLiteral(_, _) => Some(CType::ULong),
            Expr::FloatLiteral(_, _) => Some(CType::Double),
            Expr::FloatLiteralF32(_, _) => Some(CType::Float),
            Expr::FloatLiteralLongDouble(_, _) => Some(CType::LongDouble),
            Expr::ImaginaryLiteral(_, _) => Some(CType::ComplexDouble),
            Expr::ImaginaryLiteralF32(_, _) => Some(CType::ComplexFloat),
            Expr::ImaginaryLiteralLongDouble(_, _) => Some(CType::ComplexLongDouble),
            Expr::StringLiteral(_, _) => Some(CType::Pointer(Box::new(CType::Char), AddressSpace::Default)),
            Expr::WideStringLiteral(_, _) => Some(CType::Pointer(Box::new(CType::Int), AddressSpace::Default)),

            // Identifiers: look up in symbol table or enum constants
            Expr::Identifier(name, _) => {
                if name == "__func__" || name == "__FUNCTION__" || name == "__PRETTY_FUNCTION__" {
                    return Some(CType::Pointer(Box::new(CType::Char), AddressSpace::Default));
                }
                if self.types.enum_constants.contains_key(name) {
                    return Some(CType::Int);
                }
                if let Some(sym) = self.symbols.lookup(name) {
                    return Some(sym.ty.clone());
                }
                // Check function signatures for implicitly declared functions
                if let Some(func_info) = self.functions.get(name) {
                    return Some(CType::Function(Box::new(FunctionType {
                        return_type: func_info.return_type.clone(),
                        params: func_info.params.clone(),
                        variadic: func_info.variadic,
                    })));
                }
                None
            }

            // Cast: type is the target type
            Expr::Cast(type_spec, _, _) => {
                Some(self.resolve_type_spec(type_spec))
            }

            // Sizeof and Alignof always produce size_t (unsigned long on 64-bit)
            Expr::Sizeof(_, _) | Expr::Alignof(_, _) | Expr::AlignofExpr(_, _) => Some(CType::ULong),

            // Address-of wraps in Pointer
            Expr::AddressOf(inner, _) => {
                if let Some(inner_ct) = self.infer_expr_ctype(inner) {
                    Some(CType::Pointer(Box::new(inner_ct), AddressSpace::Default))
                } else {
                    // Even if inner type unknown, result is some pointer
                    Some(CType::Pointer(Box::new(CType::Void), AddressSpace::Default))
                }
            }

            // Dereference peels off one Pointer/Array layer
            Expr::Deref(inner, _) => {
                if let Some(inner_ct) = self.infer_expr_ctype(inner) {
                    match inner_ct {
                        CType::Pointer(pointee, _) => Some(*pointee),
                        CType::Array(elem, _) => Some(*elem),
                        // Dereferencing a function is a no-op in C
                        CType::Function(_) => Some(inner_ct),
                        _ => None,
                    }
                } else {
                    None
                }
            }

            // Array subscript peels off one Array/Pointer layer
            Expr::ArraySubscript(base, index, _) => {
                // Try base first (arr[i])
                if let Some(base_ct) = self.infer_expr_ctype(base) {
                    match base_ct {
                        CType::Array(elem, _) => return Some(*elem),
                        CType::Pointer(pointee, _) => return Some(*pointee),
                        _ => {}
                    }
                }
                // Reverse subscript (i[arr])
                if let Some(idx_ct) = self.infer_expr_ctype(index) {
                    match idx_ct {
                        CType::Array(elem, _) => return Some(*elem),
                        CType::Pointer(pointee, _) => return Some(*pointee),
                        _ => {}
                    }
                }
                None
            }

            // Member access: look up field type in struct layout
            Expr::MemberAccess(base_expr, field_name, _) => {
                self.infer_field_ctype(base_expr, field_name, false)
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                self.infer_field_ctype(base_expr, field_name, true)
            }

            // Unary operators
            Expr::UnaryOp(op, inner, _) => {
                match op {
                    UnaryOp::LogicalNot => Some(CType::Int),
                    UnaryOp::Neg | UnaryOp::Plus | UnaryOp::BitNot => {
                        if let Some(inner_ct) = self.infer_expr_ctype(inner) {
                            if inner_ct.is_integer() {
                                Some(inner_ct.integer_promoted())
                            } else {
                                Some(inner_ct)
                            }
                        } else {
                            None
                        }
                    }
                    UnaryOp::PreInc | UnaryOp::PreDec => self.infer_expr_ctype(inner),
                    UnaryOp::RealPart | UnaryOp::ImagPart => {
                        if let Some(inner_ct) = self.infer_expr_ctype(inner) {
                            Some(inner_ct.complex_component_type())
                        } else {
                            None
                        }
                    }
                }
            }

            // Postfix operators preserve the operand type
            Expr::PostfixOp(_, inner, _) => self.infer_expr_ctype(inner),

            // Binary operators
            Expr::BinaryOp(op, lhs, rhs, _) => {
                self.infer_binop_ctype(op, lhs, rhs)
            }

            // Assignment: type of the left-hand side
            Expr::Assign(lhs, _, _) | Expr::CompoundAssign(_, lhs, _, _) => {
                self.infer_expr_ctype(lhs)
            }

            // Conditional: type of the then-branch (matches lowerer behavior).
            // For pointer operands, this avoids incorrect arithmetic conversion.
            // TODO: implement full C11 6.5.15 composite type rules for pointers
            Expr::Conditional(_, then_expr, _, _) => self.infer_expr_ctype(then_expr),
            Expr::GnuConditional(cond, _, _) => self.infer_expr_ctype(cond),

            // Comma: type of the right expression
            Expr::Comma(_, rhs, _) => self.infer_expr_ctype(rhs),

            // Function call: determine return type
            Expr::FunctionCall(func, args, _) => {
                // __builtin_choose_expr(const_expr, expr1, expr2) has the type
                // of the selected branch, not a fixed return type.
                if let Expr::Identifier(name, _) = func.as_ref() {
                    if name == "__builtin_choose_expr" && args.len() >= 3 {
                        let cond = self.eval_const_expr(&args[0]).unwrap_or(1);
                        return if cond != 0 {
                            self.infer_expr_ctype(&args[1])
                        } else {
                            self.infer_expr_ctype(&args[2])
                        };
                    }
                }
                self.infer_call_return_ctype(func)
            }

            // VaArg and CompoundLiteral: type from type specifier
            Expr::VaArg(_, type_spec, _) | Expr::CompoundLiteral(type_spec, _, _) => {
                Some(self.resolve_type_spec(type_spec))
            }

            // Statement expression: type of the last expression statement
            Expr::StmtExpr(compound, _) => {
                if let Some(last) = compound.items.last() {
                    if let BlockItem::Statement(Stmt::Expr(Some(expr))) = last {
                        return self.infer_expr_ctype(expr);
                    }
                }
                None
            }

            // _Generic: resolve based on controlling expression type
            Expr::GenericSelection(controlling, associations, _) => {
                self.infer_generic_selection_ctype(controlling, associations)
            }

            // Label address: void*
            Expr::LabelAddr(_, _) => Some(CType::Pointer(Box::new(CType::Void), AddressSpace::Default)),

            // __builtin_types_compatible_p: int
            Expr::BuiltinTypesCompatibleP(_, _, _) => Some(CType::Int),
        }
    }

    /// Infer the CType of a binary operation.
    ///
    /// Evaluates each operand's type at most once to avoid exponential blowup
    /// on deeply nested expression chains like `+1+1+1+...+1` (which appear in
    /// preprocessor-generated enum initializers).
    fn infer_binop_ctype(&self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Option<CType> {
        // Comparison and logical operators always produce int
        match op {
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge
            | BinOp::LogicalAnd | BinOp::LogicalOr => {
                return Some(CType::Int);
            }
            _ => {}
        }

        // Evaluate each operand type once and reuse the results.
        let lct = self.infer_expr_ctype(lhs);
        let rct = self.infer_expr_ctype(rhs);

        // Shift operators: result type is the promoted type of the left operand
        if matches!(op, BinOp::Shl | BinOp::Shr) {
            if let Some(l) = lct {
                return Some(l.integer_promoted());
            }
            return Some(CType::Int);
        }

        // Pointer arithmetic for Add and Sub
        if matches!(op, BinOp::Add | BinOp::Sub) {
            if let Some(ref l) = lct {
                match l {
                    CType::Pointer(_, _) => {
                        if *op == BinOp::Sub {
                            if let Some(ref r) = rct {
                                if r.is_pointer_like() {
                                    return Some(CType::Long);
                                }
                            }
                        }
                        return lct;
                    }
                    CType::Array(elem, _) => {
                        if *op == BinOp::Sub {
                            if let Some(ref r) = rct {
                                if r.is_pointer_like() {
                                    return Some(CType::Long);
                                }
                            }
                        }
                        return Some(CType::Pointer(elem.clone(), AddressSpace::Default));
                    }
                    _ => {}
                }
            }
            if *op == BinOp::Add {
                // int + ptr case
                if let Some(ref r) = rct {
                    match r {
                        CType::Pointer(_, _) => return rct,
                        CType::Array(elem, _) => return Some(CType::Pointer(elem.clone(), AddressSpace::Default)),
                        _ => {}
                    }
                }
            }
        }

        // Arithmetic/bitwise: usual arithmetic conversions
        match (lct, rct) {
            (Some(l), Some(r)) => Some(CType::usual_arithmetic_conversion(&l, &r)),
            (Some(l), None) => Some(l.integer_promoted()),
            (None, Some(r)) => Some(r.integer_promoted()),
            (None, None) => None,
        }
    }

    /// Infer the return CType of a function call.
    fn infer_call_return_ctype(&self, func: &Expr) -> Option<CType> {
        // Strip Deref layers (dereferencing function pointers is a no-op in C)
        let mut stripped = func;
        while let Expr::Deref(inner, _) = stripped {
            stripped = inner;
        }

        if let Expr::Identifier(name, _) = stripped {
            // Check function signatures first
            if let Some(func_info) = self.functions.get(name.as_str()) {
                return Some(func_info.return_type.clone());
            }
            // Check builtin return types
            if let Some(ct) = Self::builtin_return_ctype(name) {
                return Some(ct);
            }
            // Check symbol table for function pointer variables
            if let Some(sym) = self.symbols.lookup(name) {
                return Self::extract_return_ctype_from_type(&sym.ty);
            }
        }

        // For complex expressions (indirect calls through computed function pointers),
        // try to infer the function pointer's type
        if let Some(func_ct) = self.infer_expr_ctype(stripped) {
            return Self::extract_return_ctype_from_type(&func_ct);
        }

        None
    }

    /// Extract the return CType from a function or function pointer type.
    fn extract_return_ctype_from_type(ct: &CType) -> Option<CType> {
        match ct {
            CType::Function(ft) => Some(ft.return_type.clone()),
            CType::Pointer(inner, _) => match inner.as_ref() {
                CType::Function(ft) => Some(ft.return_type.clone()),
                // Pointer-to-function-pointer
                CType::Pointer(inner2, _) => match inner2.as_ref() {
                    CType::Function(ft) => Some(ft.return_type.clone()),
                    _ => Some(inner.as_ref().clone()),
                },
                other => Some(other.clone()),
            },
            _ => None,
        }
    }

    /// Infer the CType of a struct/union field access.
    fn infer_field_ctype(&self, base_expr: &Expr, field_name: &str, is_pointer: bool) -> Option<CType> {
        let base_ctype = if is_pointer {
            match self.infer_expr_ctype(base_expr)? {
                CType::Pointer(inner, _) => *inner,
                CType::Array(inner, _) => *inner,
                _ => return None,
            }
        } else {
            self.infer_expr_ctype(base_expr)?
        };

        match &base_ctype {
            CType::Struct(key) | CType::Union(key) => {
                if let Some(layout) = self.types.struct_layouts.get(key.as_ref()) {
                    if let Some((_offset, field_ct)) = layout.field_offset(field_name, &self.types.struct_layouts) {
                        return Some(field_ct);
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Resolve a _Generic selection based on the controlling expression's type.
    fn infer_generic_selection_ctype(&self, controlling: &Expr, associations: &[GenericAssociation]) -> Option<CType> {
        let controlling_ct = self.infer_expr_ctype(controlling);
        let mut default_expr: Option<&Expr> = None;

        for assoc in associations {
            match &assoc.type_spec {
                None => { default_expr = Some(&assoc.expr); }
                Some(type_spec) => {
                    let assoc_ct = self.resolve_type_spec(type_spec);
                    if let Some(ref ctrl_ct) = controlling_ct {
                        if self.ctype_matches_generic(ctrl_ct, &assoc_ct) {
                            return self.infer_expr_ctype(&assoc.expr);
                        }
                    }
                }
            }
        }

        if let Some(def) = default_expr {
            return self.infer_expr_ctype(def);
        }
        None
    }

    /// Check if two CTypes match for _Generic selection purposes.
    /// Uses compatible-type rules: ignores qualifiers, matches arrays with pointers.
    fn ctype_matches_generic(&self, controlling: &CType, assoc: &CType) -> bool {
        // Exact match
        if std::mem::discriminant(controlling) == std::mem::discriminant(assoc) {
            match (controlling, assoc) {
                (CType::Pointer(a, _), CType::Pointer(b, _)) => {
                    return self.ctype_matches_generic(a, b);
                }
                (CType::Array(a, _), CType::Array(b, _)) => {
                    return self.ctype_matches_generic(a, b);
                }
                _ => return true,
            }
        }
        // Array decays to pointer for _Generic matching
        if let CType::Array(elem, _) = controlling {
            if let CType::Pointer(pointee, _) = assoc {
                return self.ctype_matches_generic(elem, pointee);
            }
        }
        // Enum matches int
        if matches!(controlling, CType::Enum(_)) && matches!(assoc, CType::Int) {
            return true;
        }
        if matches!(controlling, CType::Int) && matches!(assoc, CType::Enum(_)) {
            return true;
        }
        false
    }

    /// Resolve a TypeSpecifier to a CType using sema's type context.
    /// This delegates to the type_builder's shared conversion via TypeConvertContext.
    fn resolve_type_spec(&self, spec: &TypeSpecifier) -> CType {
        // Use a simple inline resolution for common cases to avoid the trait dispatch
        match spec {
            TypeSpecifier::Void => CType::Void,
            TypeSpecifier::Bool => CType::Bool,
            TypeSpecifier::Char => CType::Char,
            TypeSpecifier::UnsignedChar => CType::UChar,
            TypeSpecifier::Short => CType::Short,
            TypeSpecifier::UnsignedShort => CType::UShort,
            TypeSpecifier::Int | TypeSpecifier::Signed => CType::Int,
            TypeSpecifier::UnsignedInt | TypeSpecifier::Unsigned => CType::UInt,
            TypeSpecifier::Long => CType::Long,
            TypeSpecifier::UnsignedLong => CType::ULong,
            TypeSpecifier::LongLong => CType::LongLong,
            TypeSpecifier::UnsignedLongLong => CType::ULongLong,
            TypeSpecifier::Int128 => CType::Int128,
            TypeSpecifier::UnsignedInt128 => CType::UInt128,
            TypeSpecifier::Float => CType::Float,
            TypeSpecifier::Double => CType::Double,
            TypeSpecifier::LongDouble => CType::LongDouble,
            TypeSpecifier::ComplexFloat => CType::ComplexFloat,
            TypeSpecifier::ComplexDouble => CType::ComplexDouble,
            TypeSpecifier::ComplexLongDouble => CType::ComplexLongDouble,
            TypeSpecifier::Pointer(inner, addr_space) => {
                CType::Pointer(Box::new(self.resolve_type_spec(inner)), *addr_space)
            }
            TypeSpecifier::Array(elem, size) => {
                let elem_ct = self.resolve_type_spec(elem);
                let arr_size = size.as_ref().and_then(|s| self.eval_const_expr(s));
                CType::Array(Box::new(elem_ct), arr_size.map(|s| s as usize))
            }
            TypeSpecifier::TypedefName(name) => {
                if let Some(resolved) = self.types.typedefs.get(name) {
                    resolved.clone()
                } else {
                    CType::Int // fallback for unknown typedef
                }
            }
            TypeSpecifier::Enum(_, _) => CType::Int, // enums are int-sized
            TypeSpecifier::Struct(tag, _, _, _, _) => {
                if let Some(tag) = tag {
                    CType::Struct(format!("struct.{}", tag).into())
                } else {
                    // TODO: anonymous structs get unique keys via anon_struct_counter
                    // in sema.rs, but we don't have access to the counter here.
                    // This means typeof(anon_struct_expr) may not resolve correctly.
                    // Fix by threading the anonymous key through the AST.
                    CType::Int
                }
            }
            TypeSpecifier::Union(tag, _, _, _, _) => {
                if let Some(tag) = tag {
                    CType::Union(format!("union.{}", tag).into())
                } else {
                    // TODO: anonymous unions get unique keys via anon_struct_counter
                    // in sema.rs, but we don't have access to the counter here.
                    CType::Int
                }
            }
            TypeSpecifier::TypeofType(inner) => self.resolve_type_spec(inner),
            TypeSpecifier::Typeof(expr) => {
                // typeof(expr): try to infer the expression's type
                self.infer_expr_ctype(expr).unwrap_or(CType::Int)
            }
            TypeSpecifier::FunctionPointer(ret, params, variadic) => {
                let ret_ct = self.resolve_type_spec(ret);
                let param_cts: Vec<(CType, Option<String>)> = params.iter().map(|p| {
                    (self.resolve_type_spec(&p.type_spec), p.name.clone())
                }).collect();
                CType::Pointer(Box::new(CType::Function(Box::new(FunctionType {
                    return_type: ret_ct,
                    params: param_cts,
                    variadic: *variadic,
                }))), AddressSpace::Default)
            }
            // TODO: handle remaining TypeSpecifier variants
            _ => CType::Int,
        }
    }

    /// Evaluate a constant expression for array sizes and similar contexts.
    /// Delegates to SemaConstEval for the full implementation, unifying
    /// const_eval across all sema modules.
    fn eval_const_expr(&self, expr: &Expr) -> Option<i64> {
        let evaluator = super::const_eval::SemaConstEval {
            types: self.types,
            symbols: self.symbols,
            functions: self.functions,
            const_values: None,
            expr_types: self.expr_types,
        };
        evaluator.eval_const_expr(expr)?.to_i64()
    }

    /// Return CType for known builtins.
    /// This is the CType-level equivalent of the lowerer's builtin_return_type (IrType).
    /// TODO: consolidate with sema/builtins.rs BuiltinInfo to avoid duplication.
    fn builtin_return_ctype(name: &str) -> Option<CType> {
        match name {
            // Float-returning builtins
            "__builtin_inf" | "__builtin_huge_val"
            | "__builtin_nan" | "__builtin_fabs" | "__builtin_sqrt"
            | "__builtin_sin" | "__builtin_cos" | "__builtin_log"
            | "__builtin_log2" | "__builtin_exp" | "__builtin_pow"
            | "__builtin_floor" | "__builtin_ceil" | "__builtin_round"
            | "__builtin_fmin" | "__builtin_fmax" | "__builtin_copysign" => Some(CType::Double),

            "__builtin_inff" | "__builtin_huge_valf"
            | "__builtin_nanf" | "__builtin_fabsf" | "__builtin_sqrtf"
            | "__builtin_sinf" | "__builtin_cosf" | "__builtin_logf"
            | "__builtin_expf" | "__builtin_powf" | "__builtin_floorf"
            | "__builtin_ceilf" | "__builtin_roundf"
            | "__builtin_copysignf" => Some(CType::Float),

            "__builtin_infl" | "__builtin_huge_vall"
            | "__builtin_nanl" | "__builtin_fabsl" => Some(CType::LongDouble),

            // Integer-returning builtins
            "__builtin_fpclassify" | "__builtin_isnan" | "__builtin_isinf"
            | "__builtin_isfinite" | "__builtin_isnormal" | "__builtin_signbit"
            | "__builtin_signbitf" | "__builtin_signbitl" | "__builtin_isinf_sign"
            | "__builtin_isgreater" | "__builtin_isgreaterequal"
            | "__builtin_isless" | "__builtin_islessequal"
            | "__builtin_islessgreater" | "__builtin_isunordered"
            | "__builtin_clz" | "__builtin_clzl" | "__builtin_clzll"
            | "__builtin_ctz" | "__builtin_ctzl" | "__builtin_ctzll"
            | "__builtin_clrsb" | "__builtin_clrsbl" | "__builtin_clrsbll"
            | "__builtin_popcount" | "__builtin_popcountl" | "__builtin_popcountll"
            | "__builtin_parity" | "__builtin_parityl" | "__builtin_parityll"
            | "__builtin_ffs" | "__builtin_ffsl" | "__builtin_ffsll"
            | "__builtin_types_compatible_p" | "__builtin_classify_type"
            | "__builtin_constant_p" | "__builtin_object_size"
            | "__builtin_bswap16" | "__builtin_bswap32" | "__builtin_bswap64" => Some(CType::Int),

            // __builtin_expect returns long (evaluates both args, returns first)
            "__builtin_expect" | "__builtin_expect_with_probability" => Some(CType::Long),

            // Complex component extraction
            "creal" | "__builtin_creal" | "cimag" | "__builtin_cimag" => Some(CType::Double),
            "crealf" | "__builtin_crealf" | "cimagf" | "__builtin_cimagf" => Some(CType::Float),
            "creall" | "__builtin_creall" | "cimagl" | "__builtin_cimagl" => Some(CType::LongDouble),
            "cabs" | "__builtin_cabs" => Some(CType::Double),
            "cabsf" | "__builtin_cabsf" => Some(CType::Float),
            "cabsl" | "__builtin_cabsl" => Some(CType::LongDouble),
            "carg" | "__builtin_carg" => Some(CType::Double),
            "cargf" | "__builtin_cargf" => Some(CType::Float),

            // Memory/string builtins returning pointers
            "__builtin_memcpy" | "__builtin_memmove" | "__builtin_memset"
            | "__builtin_alloca" | "__builtin_alloca_with_align"
            | "__builtin_frame_address" | "__builtin_return_address" => {
                Some(CType::Pointer(Box::new(CType::Void), AddressSpace::Default))
            }

            // Void-returning builtins
            "__builtin_va_start" | "__builtin_va_end" | "__builtin_va_copy"
            | "__builtin_abort" | "__builtin_exit" | "__builtin_trap"
            | "__builtin_unreachable" | "__builtin_prefetch"
            | "__builtin___clear_cache" => Some(CType::Void),

            _ => None,
        }
    }
}
