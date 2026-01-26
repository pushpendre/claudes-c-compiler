/// Expression type resolution and sizing.
///
/// This module contains functions for determining the IR type and size of C expressions.
/// It includes helpers for binary operations, subscript, function call return types,
/// `_Generic` selections, `sizeof` computation, and CType-level expression type resolution.

use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{AddressSpace, CType, IrType};
use super::lowering::Lowerer;

/// Promote small integer types to I32, matching C integer promotion rules.
/// I8, U8, I16, U16 all promote to I32. All other types are returned unchanged.
fn promote_integer(ty: IrType) -> IrType {
    match ty {
        IrType::I8 | IrType::U8 | IrType::I16 | IrType::U16 | IrType::I32 => IrType::I32,
        IrType::U32 => IrType::U32,
        other => other,
    }
}

/// Apply C11 6.3.1.1p2 integer promotion for bitfield types.
/// "If an int can represent all values of the original type (as restricted by the
/// width, for a bit-field), the value is converted to an int; otherwise, it is
/// converted to an unsigned int."
///
/// This applies regardless of the declared storage type of the bitfield. A
/// `uint64_t arg_count : 16` bitfield has values 0..65535, which fit in int,
/// so it promotes to int (I32).
fn bitfield_promoted_type(field_ty: IrType, bf_info: Option<(u32, u32)>) -> IrType {
    if let Some((_bit_offset, bit_width)) = bf_info {
        let is_signed = field_ty.is_signed();
        if is_signed {
            // Signed bitfield: values range is -(2^(w-1)) to 2^(w-1)-1.
            // int (32-bit signed) can represent all values if width <= 32.
            if bit_width <= 32 {
                IrType::I32
            } else {
                field_ty
            }
        } else {
            // Unsigned bitfield: values range is 0 to 2^w - 1.
            // int (32-bit signed, max 2^31-1) can represent all values if width <= 31.
            if bit_width <= 31 {
                IrType::I32
            } else if bit_width == 32 {
                // int cannot represent 0..2^32-1, so promote to unsigned int.
                IrType::U32
            } else {
                field_ty
            }
        }
    } else {
        field_ty
    }
}

impl Lowerer {

    /// Check if a TypeSpecifier resolves to long double.
    pub(super) fn is_type_spec_long_double(&self, ts: &TypeSpecifier) -> bool {
        match ts {
            TypeSpecifier::LongDouble => true,
            TypeSpecifier::TypedefName(name) => {
                if let Some(ctype) = self.types.typedefs.get(name) {
                    matches!(ctype, CType::LongDouble)
                } else {
                    false
                }
            }
            TypeSpecifier::TypeofType(inner) => self.is_type_spec_long_double(inner),
            _ => false,
        }
    }

    /// Get the zero constant for a given IR type.
    pub(super) fn zero_const(&self, ty: IrType) -> IrConst {
        if ty == IrType::Void { IrConst::Zero } else { IrConst::zero(ty) }
    }

    /// Check if an expression refers to a struct/union value (not pointer-to-struct).
    /// Returns the struct/union size if the expression produces a struct/union value,
    /// or None if it's not a struct/union. Unifies the old expr_is_struct_value (check)
    /// and get_struct_size_for_expr (size) into a single dispatch.
    pub(super) fn struct_value_size(&self, expr: &Expr) -> Option<usize> {
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(info) = self.func_state.as_ref().and_then(|fs| fs.locals.get(name)) {
                    if info.is_struct { return Some(info.alloc_size); }
                    return None; // local found but not struct; don't fall through to globals
                }
                if let Some(ginfo) = self.globals.get(name) {
                    if ginfo.is_struct {
                        return Some(ginfo.struct_layout.as_ref().map_or(8, |l| l.size));
                    }
                }
                None
            }
            Expr::MemberAccess(base_expr, field_name, _) | Expr::PointerMemberAccess(base_expr, field_name, _) => {
                let is_ptr = matches!(expr, Expr::PointerMemberAccess(..));
                let ctype = self.resolve_field_ctype(base_expr, field_name, is_ptr)?;
                if ctype.is_struct_or_union() {
                    Some(self.ctype_size(&ctype))
                } else { None }
            }
            Expr::ArraySubscript(_, _, _) | Expr::Deref(_, _)
            | Expr::FunctionCall(_, _, _) | Expr::Conditional(_, _, _, _)
            | Expr::GnuConditional(_, _, _)
            | Expr::Assign(_, _, _) | Expr::CompoundAssign(_, _, _, _)
            | Expr::Comma(_, _, _) => {
                let ctype = self.get_expr_ctype(expr)?;
                if ctype.is_struct_or_union() {
                    Some(self.ctype_size(&ctype))
                } else { None }
            }
            Expr::CompoundLiteral(type_spec, _, _) => {
                let ctype = self.type_spec_to_ctype(type_spec);
                if ctype.is_struct_or_union() {
                    Some(self.sizeof_type(type_spec))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Extract the return IrType from a function pointer's CType.
    /// The function pointer CType can be:
    ///   - Pointer(Function(ft)): ft.return_type is the actual C return type
    ///   - Pointer(X): typedef lost the Function node; X is the return type
    ///   - Function(ft): direct function type
    fn extract_func_ptr_return_type(ctype: &CType) -> IrType {
        match ctype {
            CType::Pointer(inner, _) => match inner.as_ref() {
                CType::Function(ft) => IrType::from_ctype(&ft.return_type),
                // For parameter function pointers, CType is just Pointer(ReturnType)
                // without the Function wrapper
                other => IrType::from_ctype(other),
            },
            CType::Function(ft) => IrType::from_ctype(&ft.return_type),
            _ => IrType::I64,
        }
    }

    /// For indirect calls (function pointer calls), determine if the return type is a struct
    /// and return its size. Returns None if not a struct return or if the type cannot be determined.
    pub(super) fn get_call_return_struct_size(&self, func: &Expr) -> Option<usize> {
        // For indirect calls, extract the return type from the function pointer's CType.
        // Strip Deref layers since dereferencing function pointers is a no-op in C.
        let func_ctype = match func {
            Expr::Identifier(name, _) => {
                // Could be a function pointer variable
                if let Some(vi) = self.lookup_var_info(name) {
                    vi.c_type.clone()
                } else {
                    None
                }
            }
            Expr::Deref(..) => {
                let mut expr = func;
                while let Expr::Deref(inner, _) = expr {
                    expr = inner;
                }
                self.get_expr_ctype(expr)
            }
            _ => self.get_expr_ctype(func),
        };

        if let Some(ref ctype) = func_ctype {
            // Navigate through CType to find the return type
            let ret_ctype = ctype.func_ptr_return_type(false);
            if let Some(ret_ct) = ret_ctype {
                if ret_ct.is_struct_or_union() {
                    // Use resolve_ctype_size to look up struct layouts properly;
                    // CType::size() returns 0 for Struct/Union since they only
                    // store a key, not the actual layout.
                    return Some(self.resolve_ctype_size(&ret_ct));
                }
            }
        }
        None
    }

    /// Return the IR type for known builtins that return float or specific types.
    /// Returns None for builtins without special return type handling.
    pub(super) fn builtin_return_type(name: &str) -> Option<IrType> {
        match name {
            // Float-returning builtins
            "__builtin_inf" | "__builtin_huge_val" => Some(IrType::F64),
            "__builtin_inff" | "__builtin_huge_valf" => Some(IrType::F32),
            "__builtin_infl" | "__builtin_huge_vall" => Some(IrType::F128),
            "__builtin_nan" => Some(IrType::F64),
            "__builtin_nanf" => Some(IrType::F32),
            "__builtin_nanl" => Some(IrType::F128),
            "__builtin_fabs" | "__builtin_sqrt" | "__builtin_sin" | "__builtin_cos"
            | "__builtin_log" | "__builtin_log2" | "__builtin_exp" | "__builtin_pow"
            | "__builtin_floor" | "__builtin_ceil" | "__builtin_round"
            | "__builtin_fmin" | "__builtin_fmax" | "__builtin_copysign" => Some(IrType::F64),
            "__builtin_fabsf" | "__builtin_sqrtf" | "__builtin_sinf" | "__builtin_cosf"
            | "__builtin_logf" | "__builtin_expf" | "__builtin_powf"
            | "__builtin_floorf" | "__builtin_ceilf" | "__builtin_roundf"
            | "__builtin_copysignf" => Some(IrType::F32),
            "__builtin_fabsl" => Some(IrType::F128),
            // Integer-returning classification builtins
            "__builtin_fpclassify" | "__builtin_isnan" | "__builtin_isinf"
            | "__builtin_isfinite" | "__builtin_isnormal" | "__builtin_signbit"
            | "__builtin_signbitf" | "__builtin_signbitl" | "__builtin_isinf_sign"
            | "__builtin_isgreater" | "__builtin_isgreaterequal"
            | "__builtin_isless" | "__builtin_islessequal"
            | "__builtin_islessgreater" | "__builtin_isunordered" => Some(IrType::I32),
            // Bit manipulation builtins return int
            "__builtin_clz" | "__builtin_clzl" | "__builtin_clzll"
            | "__builtin_ctz" | "__builtin_ctzl" | "__builtin_ctzll"
            | "__builtin_clrsb" | "__builtin_clrsbl" | "__builtin_clrsbll"
            | "__builtin_popcount" | "__builtin_popcountl" | "__builtin_popcountll"
            | "__builtin_parity" | "__builtin_parityl" | "__builtin_parityll"
            | "__builtin_ffs" | "__builtin_ffsl" | "__builtin_ffsll" => Some(IrType::I32),
            // Complex number component extraction builtins
            "creal" | "__builtin_creal" | "cimag" | "__builtin_cimag" => Some(IrType::F64),
            "crealf" | "__builtin_crealf" | "cimagf" | "__builtin_cimagf" => Some(IrType::F32),
            "creall" | "__builtin_creall" | "cimagl" | "__builtin_cimagl" => Some(IrType::F128),
            // Complex absolute value
            "cabs" | "__builtin_cabs" => Some(IrType::F64),
            "cabsf" | "__builtin_cabsf" => Some(IrType::F32),
            "cabsl" | "__builtin_cabsl" => Some(IrType::F128),
            // Complex argument
            "carg" | "__builtin_carg" => Some(IrType::F64),
            "cargf" | "__builtin_cargf" => Some(IrType::F32),
            _ => None,
        }
    }

    /// Get the IR type for a binary operation expression.
    fn get_binop_type(&self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> IrType {
        match op {
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge
            | BinOp::LogicalAnd | BinOp::LogicalOr => IrType::I64,
            BinOp::Shl | BinOp::Shr => {
                let lty = self.get_expr_type(lhs);
                promote_integer(lty)
            }
            _ => {
                // Iterate left-skewed chains to avoid O(2^n) recursion
                let rty = self.get_expr_type(rhs);
                let mut result = rty;
                let mut cur: &Expr = lhs;
                // Check complex for the rhs first
                if matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div) && rty == IrType::Ptr {
                    let rct = self.expr_ctype(rhs);
                    if rct.is_complex() {
                        return IrType::Ptr;
                    }
                }
                loop {
                    match cur {
                        Expr::BinaryOp(op2, inner_lhs, inner_rhs, _)
                            if !op2.is_comparison()
                                && !matches!(op2, BinOp::LogicalAnd | BinOp::LogicalOr | BinOp::Shl | BinOp::Shr) =>
                        {
                            let r_ty = self.get_expr_type(inner_rhs.as_ref());
                            // Check complex for inner rhs
                            if matches!(op2, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div) && r_ty == IrType::Ptr {
                                let rct = self.expr_ctype(inner_rhs.as_ref());
                                if rct.is_complex() {
                                    return IrType::Ptr;
                                }
                            }
                            result = Self::wider_type(result, r_ty);
                            cur = inner_lhs.as_ref();
                        }
                        _ => {
                            let l_ty = self.get_expr_type(cur);
                            // Check complex for leftmost leaf
                            if l_ty == IrType::Ptr {
                                let lct = self.expr_ctype(cur);
                                if lct.is_complex() {
                                    return IrType::Ptr;
                                }
                            }
                            result = Self::wider_type(result, l_ty);
                            break;
                        }
                    }
                }
                result
            }
        }
    }

    /// Return the wider/common type between two types, preferring float > int.
    fn wider_type(a: IrType, b: IrType) -> IrType {
        if a == IrType::F128 || b == IrType::F128 {
            IrType::F128
        } else if a == IrType::F64 || b == IrType::F64 {
            IrType::F64
        } else if a == IrType::F32 || b == IrType::F32 {
            IrType::F32
        } else {
            Self::common_type(a, b)
        }
    }

    /// Get the IR type for an array subscript expression.
    fn get_subscript_type(&self, base: &Expr, index: &Expr) -> IrType {
        if let Some(base_ctype) = self.get_expr_ctype(base) {
            match base_ctype {
                CType::Array(elem, _) => return IrType::from_ctype(&elem),
                CType::Pointer(pointee, _) => return IrType::from_ctype(&pointee),
                _ => {}
            }
        }
        if let Some(idx_ctype) = self.get_expr_ctype(index) {
            match idx_ctype {
                CType::Array(elem, _) => return IrType::from_ctype(&elem),
                CType::Pointer(pointee, _) => return IrType::from_ctype(&pointee),
                _ => {}
            }
        }
        // Reconstruct the full subscript expr for get_array_root_name
        // We use base/index directly to look up root names
        let root_name = self.get_array_root_name_from_subscript(base, index);
        if let Some(name) = root_name {
            if let Some(vi) = self.lookup_var_info(&name) {
                if vi.is_array {
                    // For globals with multi-dim strides, use stride-based type
                    if !vi.array_dim_strides.is_empty() {
                        return self.ir_type_for_elem_size(*vi.array_dim_strides.last().unwrap_or(&8));
                    }
                    return vi.ty;
                }
            }
        }
        for operand in [base, index] {
            if let Expr::Identifier(name, _) = operand {
                if let Some(vi) = self.lookup_var_info(name) {
                    if let Some(pt) = vi.pointee_type {
                        return pt;
                    }
                    if vi.is_array {
                        return vi.ty;
                    }
                }
            }
        }
        match base {
            Expr::MemberAccess(base_expr, field_name, _) | Expr::PointerMemberAccess(base_expr, field_name, _) => {
                let is_ptr = matches!(base, Expr::PointerMemberAccess(..));
                if let Some(ctype) = self.resolve_field_ctype(base_expr, field_name, is_ptr) {
                    if let CType::Array(elem_ty, _) = &ctype {
                        return IrType::from_ctype(elem_ty);
                    }
                    if let CType::Pointer(pointee, _) = &ctype {
                        return IrType::from_ctype(pointee);
                    }
                }
            }
            _ => {}
        }
        if let Some(pt) = self.get_pointee_type_of_expr(base) {
            return pt;
        }
        if let Some(pt) = self.get_pointee_type_of_expr(index) {
            return pt;
        }
        IrType::I64
    }

    /// Helper to get array root name from subscript base/index without needing the full
    /// ArraySubscript expression node.
    fn get_array_root_name_from_subscript(&self, base: &Expr, index: &Expr) -> Option<String> {
        // Try base first (normal case: arr[i])
        if let Some(name) = self.get_array_root_name(base) {
            return Some(name);
        }
        // Try index (reverse subscript: i[arr])
        self.get_array_root_name(index)
    }

    /// Get the IR type for a function call's return value.
    /// Strips Deref layers since dereferencing function pointers is a no-op in C.
    pub(super) fn get_call_return_type(&self, func: &Expr) -> IrType {
        // Strip all Deref layers to get the underlying expression
        let mut stripped = func;
        while let Expr::Deref(inner, _) = stripped {
            stripped = inner;
        }
        if let Expr::Identifier(name, _) = stripped {
            if let Some(ret_ty) = self.func_meta.sigs.get(name.as_str()).map(|s| s.return_type) {
                return ret_ty;
            }
            if let Some(ret_ty) = self.func_meta.ptr_sigs.get(name.as_str()).map(|s| s.return_type) {
                return ret_ty;
            }
            if let Some(ret_ty) = Self::builtin_return_type(name) {
                return ret_ty;
            }
            // Fall back to sema's function signatures for IrType derivation
            if let Some(func_info) = self.sema_functions.get(name.as_str()) {
                return IrType::from_ctype(&func_info.return_type);
            }
        }
        if let Some(ctype) = self.get_expr_ctype(stripped) {
            return Self::extract_func_ptr_return_type(&ctype);
        }
        if let Some(ctype) = self.get_expr_ctype(func) {
            return Self::extract_func_ptr_return_type(&ctype);
        }
        IrType::I64
    }

    /// Resolve the CType of a _Generic selection expression.
    /// Used by get_expr_ctype_lowerer so that typeof(_Generic(...)) resolves correctly.
    fn resolve_generic_selection_ctype(&self, controlling: &Expr, associations: &[GenericAssociation]) -> Option<CType> {
        let controlling_ctype = self.get_expr_ctype(controlling);
        let controlling_ir_type = self.get_expr_type(controlling);
        let mut default_expr: Option<&Expr> = None;
        for assoc in associations.iter() {
            match &assoc.type_spec {
                None => { default_expr = Some(&assoc.expr); }
                Some(type_spec) => {
                    let assoc_ctype = self.type_spec_to_ctype(type_spec);
                    if let Some(ref ctrl_ct) = controlling_ctype {
                        if self.ctype_matches_generic(ctrl_ct, &assoc_ctype) {
                            return self.get_expr_ctype(&assoc.expr);
                        }
                    } else {
                        let assoc_ir_type = self.type_spec_to_ir(type_spec);
                        if assoc_ir_type == controlling_ir_type {
                            return self.get_expr_ctype(&assoc.expr);
                        }
                    }
                }
            }
        }
        if let Some(def) = default_expr {
            return self.get_expr_ctype(def);
        }
        None
    }

    /// Resolve the type of a _Generic selection expression.
    fn resolve_generic_selection_type(&self, controlling: &Expr, associations: &[GenericAssociation]) -> IrType {
        let controlling_ctype = self.get_expr_ctype(controlling);
        let controlling_ir_type = self.get_expr_type(controlling);
        let mut default_expr: Option<&Expr> = None;
        for assoc in associations.iter() {
            match &assoc.type_spec {
                None => { default_expr = Some(&assoc.expr); }
                Some(type_spec) => {
                    let assoc_ctype = self.type_spec_to_ctype(type_spec);
                    if let Some(ref ctrl_ct) = controlling_ctype {
                        if self.ctype_matches_generic(ctrl_ct, &assoc_ctype) {
                            return self.get_expr_type(&assoc.expr);
                        }
                    } else {
                        let assoc_ir_type = self.type_spec_to_ir(type_spec);
                        if assoc_ir_type == controlling_ir_type {
                            return self.get_expr_type(&assoc.expr);
                        }
                    }
                }
            }
        }
        if let Some(def) = default_expr {
            return self.get_expr_type(def);
        }
        IrType::I64
    }

    /// Get the IR type for an expression (best-effort, based on locals/globals info).
    pub(super) fn get_expr_type(&self, expr: &Expr) -> IrType {
        match expr {
            Expr::IntLiteral(_, _) | Expr::LongLiteral(_, _) | Expr::CharLiteral(_, _) => IrType::I64,
            Expr::UIntLiteral(_, _) | Expr::ULongLiteral(_, _) => IrType::U64,
            Expr::FloatLiteral(_, _) => IrType::F64,
            Expr::FloatLiteralF32(_, _) => IrType::F32,
            Expr::FloatLiteralLongDouble(_, _) => IrType::F128,
            Expr::ImaginaryLiteral(_, _) | Expr::ImaginaryLiteralF32(_, _)
            | Expr::ImaginaryLiteralLongDouble(_, _) => IrType::Ptr,
            Expr::StringLiteral(_, _) | Expr::WideStringLiteral(_, _) => IrType::Ptr,
            Expr::Cast(ref target_type, _, _) => self.type_spec_to_ir(target_type),
            Expr::UnaryOp(UnaryOp::RealPart, inner, _) | Expr::UnaryOp(UnaryOp::ImagPart, inner, _) => {
                let inner_ct = self.expr_ctype(inner);
                if inner_ct.is_complex() {
                    return Self::complex_component_ir_type(&inner_ct);
                }
                self.get_expr_type(inner)
            }
            Expr::UnaryOp(UnaryOp::Neg, inner, _) | Expr::UnaryOp(UnaryOp::Plus, inner, _)
            | Expr::UnaryOp(UnaryOp::BitNot, inner, _) => {
                let inner_ty = self.get_expr_type(inner);
                // Only check for complex types if the inner type is Ptr (which complex uses)
                if inner_ty == IrType::Ptr {
                    let inner_ct = self.expr_ctype(inner);
                    if inner_ct.is_complex() {
                        return IrType::Ptr;
                    }
                }
                if inner_ty.is_float() {
                    return inner_ty;
                }
                promote_integer(inner_ty)
            }
            Expr::UnaryOp(UnaryOp::PreInc, inner, _) | Expr::UnaryOp(UnaryOp::PreDec, inner, _) => {
                self.get_expr_type(inner)
            }
            Expr::BinaryOp(op, lhs, rhs, _) => {
                self.get_binop_type(op, lhs, rhs)
            }
            Expr::Assign(lhs, _, _) | Expr::CompoundAssign(_, lhs, _, _) => {
                self.get_expr_type(lhs)
            }
            Expr::Conditional(_, then_expr, else_expr, _) => {
                let then_ty = self.get_expr_type(then_expr);
                let else_ty = self.get_expr_type(else_expr);
                Self::common_type(then_ty, else_ty)
            }
            Expr::GnuConditional(cond, else_expr, _) => {
                let cond_ty = self.get_expr_type(cond);
                let else_ty = self.get_expr_type(else_expr);
                Self::common_type(cond_ty, else_ty)
            }
            Expr::Comma(_, rhs, _) => self.get_expr_type(rhs),
            Expr::PostfixOp(_, inner, _) => self.get_expr_type(inner),
            Expr::AddressOf(_, _) => IrType::Ptr,
            Expr::Sizeof(_, _) => IrType::U64,
            Expr::GenericSelection(controlling, associations, _) => {
                self.resolve_generic_selection_type(controlling, associations)
            }
            Expr::FunctionCall(func, args, _) => {
                // __builtin_choose_expr(const_expr, expr1, expr2): return type of
                // the selected branch expression, not a fixed return type.
                if let Expr::Identifier(name, _) = func.as_ref() {
                    if name == "__builtin_choose_expr" && args.len() >= 3 {
                        let cond = self.eval_const_expr(&args[0]);
                        let is_nonzero = match cond {
                            Some(IrConst::I64(v)) => v != 0,
                            Some(IrConst::I32(v)) => v != 0,
                            _ => true,
                        };
                        return if is_nonzero {
                            self.get_expr_type(&args[1])
                        } else {
                            self.get_expr_type(&args[2])
                        };
                    }
                }
                self.get_call_return_type(func)
            }
            Expr::VaArg(_, type_spec, _) => self.resolve_va_arg_type(type_spec),
            Expr::Identifier(name, _) => {
                if name == "__func__" || name == "__FUNCTION__" || name == "__PRETTY_FUNCTION__" {
                    return IrType::Ptr;
                }
                if self.types.enum_constants.contains_key(name) {
                    return IrType::I32;
                }
                if let Some(vi) = self.lookup_var_info(name) {
                    if vi.is_array {
                        return IrType::Ptr;
                    }
                    return vi.ty;
                }
                IrType::I64
            }
            Expr::ArraySubscript(base, index, _) => {
                self.get_subscript_type(base, index)
            }
            Expr::Deref(inner, _) => {
                if let Some(inner_ctype) = self.get_expr_ctype(inner) {
                    match inner_ctype {
                        CType::Pointer(pointee, _) => return IrType::from_ctype(&pointee),
                        CType::Array(elem, _) => return IrType::from_ctype(&elem),
                        _ => {}
                    }
                }
                if let Some(pt) = self.get_pointee_type_of_expr(inner) {
                    return pt;
                }
                IrType::I64
            }
            Expr::MemberAccess(base_expr, field_name, _) => {
                let (_, field_ty, bf_info) = self.resolve_member_access_full(base_expr, field_name);
                bitfield_promoted_type(field_ty, bf_info)
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                let (_, field_ty, bf_info) = self.resolve_pointer_member_access_full(base_expr, field_name);
                bitfield_promoted_type(field_ty, bf_info)
            }
            Expr::StmtExpr(compound, _) => {
                // Statement expression: type is the type of the last expression statement
                if let Some(last) = compound.items.last() {
                    if let BlockItem::Statement(Stmt::Expr(Some(expr))) = last {
                        return self.get_expr_type(expr);
                    }
                }
                IrType::I64
            }
            Expr::CompoundLiteral(type_name, _, _) => {
                self.type_spec_to_ir(type_name)
            }
            _ => IrType::I64,
        }
    }

    /// Get the sizeof for an identifier expression.
    fn sizeof_identifier(&self, name: &str) -> usize {
        if let Some(info) = self.func_state.as_ref().and_then(|fs| fs.locals.get(name)) {
            if info.is_array || info.is_struct {
                return info.alloc_size;
            }
            // Use CType size if available (handles long double, function types, etc.)
            if let Some(ref ct) = info.c_type {
                if matches!(ct, CType::Function(_)) {
                    // GCC extension: sizeof(function_type) == 1
                    return 1;
                }
                let ct_size = self.ctype_size(ct);
                if ct_size > 0 {
                    return ct_size;
                }
            }
            return info.ty.size();
        }
        if let Some(ginfo) = self.globals.get(name) {
            if ginfo.is_array || ginfo.is_struct {
                for g in &self.module.globals {
                    if g.name == *name {
                        return g.size;
                    }
                }
            }
            // Use CType size if available
            if let Some(ref ct) = ginfo.c_type {
                let ct_size = self.ctype_size(ct);
                if ct_size > 0 {
                    return ct_size;
                }
            }
            return ginfo.ty.size();
        }
        // GCC extension: sizeof(function_name) == 1
        if self.known_functions.contains(name) {
            return 1;
        }
        4 // default: int
    }

    /// Get the sizeof for a dereference expression.
    fn sizeof_deref(&self, inner: &Expr) -> usize {
        // Use CType-based resolution first
        if let Some(inner_ctype) = self.get_expr_ctype(inner) {
            match &inner_ctype {
                CType::Pointer(pointee, _) => {
                    // GCC extension: sizeof(*void_ptr) == 1, sizeof(*func_ptr) == 1
                    if matches!(pointee.as_ref(), CType::Void | CType::Function(_)) {
                        return 1;
                    }
                    let sz = self.resolve_ctype_size(pointee);
                    if sz == 0 {
                        return 1;
                    }
                    return sz;
                }
                CType::Array(elem, _) => return self.resolve_ctype_size(elem).max(1),
                // GCC extension: sizeof(*func) == 1 where func is a function
                CType::Function(_) => return 1,
                _ => {}
            }
        }
        if let Expr::Identifier(name, _) = inner {
            if let Some(vi) = self.lookup_var_info(name) {
                // GCC extension: sizeof(*func_ptr) == 1
                if let Some(ref ct) = vi.c_type {
                    if matches!(ct, CType::Function(_)) {
                        return 1;
                    }
                    if let CType::Pointer(pointee, _) = ct {
                        if matches!(pointee.as_ref(), CType::Function(_)) {
                            return 1;
                        }
                    }
                }
                if vi.elem_size > 0 {
                    return vi.elem_size;
                }
            }
            // GCC extension: sizeof(*func_name) == 1 for known functions
            if self.known_functions.contains(name) {
                return 1;
            }
        }
        8 // TODO: better type tracking for nested derefs
    }

    /// Get the sizeof for an array subscript expression.
    fn sizeof_subscript(&self, base: &Expr, index: &Expr) -> usize {
        // Use CType-based resolution first (handles string literals, typed pointers)
        if let Some(base_ctype) = self.get_expr_ctype(base) {
            match &base_ctype {
                CType::Array(elem, _) => return self.resolve_ctype_size(elem).max(1),
                CType::Pointer(pointee, _) => return self.resolve_ctype_size(pointee).max(1),
                _ => {}
            }
        }
        // Also check reverse subscript (index[base])
        if let Some(idx_ctype) = self.get_expr_ctype(index) {
            match &idx_ctype {
                CType::Array(elem, _) => return self.resolve_ctype_size(elem).max(1),
                CType::Pointer(pointee, _) => return self.resolve_ctype_size(pointee).max(1),
                _ => {}
            }
        }
        if let Expr::Identifier(name, _) = base {
            if let Some(vi) = self.lookup_var_info(name) {
                if vi.elem_size > 0 {
                    return vi.elem_size;
                }
            }
        }
        // Fallback for member access bases (e.g., p->c[0] or x.arr[0])
        match base {
            Expr::MemberAccess(base_expr, field_name, _) | Expr::PointerMemberAccess(base_expr, field_name, _) => {
                let is_ptr = matches!(base, Expr::PointerMemberAccess(..));
                if let Some(ctype) = self.resolve_field_ctype(base_expr, field_name, is_ptr) {
                    match &ctype {
                        CType::Array(elem_ty, _) => return self.resolve_ctype_size(elem_ty).max(1),
                        CType::Pointer(pointee, _) => return self.resolve_ctype_size(pointee).max(1),
                        _ => {}
                    }
                }
            }
            _ => {}
        }
        4 // default: int element
    }

    /// Get the sizeof for a member access expression.
    fn sizeof_member_access(&self, base_expr: &Expr, field_name: &str, is_pointer: bool) -> usize {
        if let Some(ctype) = self.resolve_field_ctype(base_expr, field_name, is_pointer) {
            let sz = self.ctype_size(&ctype);
            if sz > 0 { return sz; }
        }
        if is_pointer {
            let (_, field_ty) = self.resolve_pointer_member_access(base_expr, field_name);
            field_ty.size()
        } else {
            let (_, field_ty) = self.resolve_member_access(base_expr, field_name);
            field_ty.size()
        }
    }

    /// Get the sizeof for a binary operation expression.
    fn sizeof_binop(&self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> usize {
        match op {
            // Comparison/logical: result is int
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le
            | BinOp::Gt | BinOp::Ge | BinOp::LogicalAnd | BinOp::LogicalOr => 4,
            // Pointer subtraction: result is ptrdiff_t (8 bytes on 64-bit)
            BinOp::Sub if self.sizeof_operand_is_pointer_like(lhs)
                && self.sizeof_operand_is_pointer_like(rhs) => 8,
            // Pointer arithmetic (ptr + int, int + ptr, ptr - int): result is pointer
            BinOp::Add | BinOp::Sub
                if self.sizeof_operand_is_pointer_like(lhs)
                || self.sizeof_operand_is_pointer_like(rhs) => 8,
            // Shift operators: result type is promoted left operand
            BinOp::Shl | BinOp::Shr => {
                self.sizeof_expr(lhs).max(4) // integer promotion of left operand only
            }
            // Arithmetic/bitwise: usual arithmetic conversions
            _ => {
                let ls = self.sizeof_expr(lhs);
                let rs = self.sizeof_expr(rhs);
                ls.max(rs).max(4) // integer promotion
            }
        }
    }

    /// Check if an expression is pointer-like for sizeof computation.
    /// Arrays decay to pointers in expression context, so both pointers and arrays
    /// produce pointer-typed results in arithmetic.
    fn sizeof_operand_is_pointer_like(&self, expr: &Expr) -> bool {
        if self.expr_is_array_name(expr) {
            return true;
        }
        if let Some(ctype) = self.get_expr_ctype(expr) {
            return matches!(ctype, CType::Pointer(_, _) | CType::Array(_, _));
        }
        false
    }

    /// Compute sizeof for an expression operand (sizeof expr).
    /// Returns the size in bytes of the expression's type.
    pub(super) fn sizeof_expr(&self, expr: &Expr) -> usize {
        match expr {
            // Integer literal: type int (4 bytes), unless value overflows to long
            Expr::IntLiteral(val, _) => {
                if *val >= i32::MIN as i64 && *val <= i32::MAX as i64 {
                    4
                } else {
                    8
                }
            }
            // Unsigned int literal: type unsigned int (4 bytes) if fits, else unsigned long
            Expr::UIntLiteral(val, _) => {
                if *val <= u32::MAX as u64 {
                    4
                } else {
                    8
                }
            }
            // Long/unsigned long literal: always 8 bytes
            Expr::LongLiteral(_, _) | Expr::ULongLiteral(_, _) => 8,
            // Float literal: type double (8 bytes) by default in C
            Expr::FloatLiteral(_, _) => 8,
            // Float literal with f suffix: type float (4 bytes)
            Expr::FloatLiteralF32(_, _) => 4,
            // Float literal with L suffix: type long double (16 bytes)
            Expr::FloatLiteralLongDouble(_, _) => 16,
            // Char literal: type int in C (4 bytes)
            Expr::CharLiteral(_, _) => 4,
            // String literal: array of char, size = length + 1 (null terminator)
            Expr::StringLiteral(s, _) => s.chars().count() + 1,
            // Wide string literal: array of wchar_t (4 bytes each), size = (chars + 1) * 4
            Expr::WideStringLiteral(s, _) => (s.chars().count() + 1) * 4,

            // Variable: look up its alloc_size or type
            Expr::Identifier(name, _) => {
                self.sizeof_identifier(name)
            }

            // Dereference: element type size
            Expr::Deref(inner, _) => {
                self.sizeof_deref(inner)
            }

            // Array subscript: element type size
            Expr::ArraySubscript(base, index, _) => {
                self.sizeof_subscript(base, index)
            }

            // sizeof(sizeof(...)) or sizeof(_Alignof(...)) -> size_t = 8 on 64-bit
            Expr::Sizeof(_, _) | Expr::Alignof(_, _) | Expr::AlignofExpr(_, _) => 8,

            // Cast: size of the target type
            Expr::Cast(target_type, _, _) => {
                self.sizeof_type(target_type)
            }

            // Member access: member field size (use CType for accurate array/struct sizes)
            Expr::MemberAccess(base_expr, field_name, _) => {
                self.sizeof_member_access(base_expr, field_name, false)
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                self.sizeof_member_access(base_expr, field_name, true)
            }

            // Address-of: pointer (8 bytes)
            Expr::AddressOf(_, _) => 8,

            // Unary operations
            Expr::UnaryOp(op, inner, _) => {
                match op {
                    UnaryOp::LogicalNot => 4, // result is int
                    UnaryOp::Neg | UnaryOp::Plus | UnaryOp::BitNot => {
                        self.sizeof_expr(inner).max(4) // integer promotion
                    }
                    UnaryOp::PreInc | UnaryOp::PreDec => self.sizeof_expr(inner),
                    UnaryOp::RealPart | UnaryOp::ImagPart => {
                        // Result is the component type size
                        let inner_ctype = self.expr_ctype(inner);
                        inner_ctype.complex_component_type().size()
                    }
                }
            }

            // Postfix operations preserve the operand type
            Expr::PostfixOp(_, inner, _) => self.sizeof_expr(inner),

            // Binary operations
            Expr::BinaryOp(op, lhs, rhs, _) => {
                self.sizeof_binop(op, lhs, rhs)
            }

            // Conditional: common type of both branches
            Expr::Conditional(_, then_e, else_e, _) => {
                let ts = self.sizeof_expr(then_e);
                let es = self.sizeof_expr(else_e);
                ts.max(es)
            }
            Expr::GnuConditional(cond, else_e, _) => {
                let cs = self.sizeof_expr(cond);
                let es = self.sizeof_expr(else_e);
                cs.max(es)
            }

            // Assignment: type of the left-hand side
            Expr::Assign(lhs, _, _) | Expr::CompoundAssign(_, lhs, _, _) => {
                self.sizeof_expr(lhs)
            }

            // Comma: type of the right expression
            Expr::Comma(_, rhs, _) => {
                self.sizeof_expr(rhs)
            }

            // Function call: use the actual return type
            Expr::FunctionCall(_, _, _) => {
                // Prefer CType which has correct struct/union sizes
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    self.ctype_size(&ctype)
                } else {
                    let ret_ty = self.get_expr_type(expr);
                    ret_ty.size()
                }
            }

            // Compound literal: size of the type (handle incomplete array types)
            Expr::CompoundLiteral(ts, ref init, _) => {
                let ctype = self.type_spec_to_ctype(ts);
                match (&ctype, init.as_ref()) {
                    (CType::Array(ref elem_ct, None), Initializer::List(items)) => {
                        self.ctype_size(elem_ct).max(1) * items.len()
                    }
                    _ => self.sizeof_type(ts),
                }
            }

            // Default
            _ => 4,
        }
    }

    /// Compute alignof for an expression by inferring its type and returning
    /// the type's alignment. This implements GCC's __alignof__(expr) semantics.
    pub(super) fn alignof_expr(&self, expr: &Expr) -> usize {
        // Try to get the CType of the expression for accurate alignment
        if let Some(ctype) = self.get_expr_ctype(expr) {
            return self.ctype_align(&ctype);
        }
        // Fallback: derive alignment from the expression's size
        // (this handles literals and simple cases where CType isn't available)
        let size = self.sizeof_expr(expr);
        match size {
            1 => 1,
            2 => 2,
            4 => 4,
            16 => 16,  // long double, __int128
            _ => 8,
        }
    }

    /// Get the element size for a compound literal type.
    /// For arrays, returns the element size; for scalars/structs, returns the full size.
    pub(super) fn compound_literal_elem_size(&self, ts: &TypeSpecifier) -> usize {
        let ctype = self.type_spec_to_ctype(ts);
        match &ctype {
            CType::Array(elem_ct, _) => self.ctype_size(elem_ct).max(1),
            _ => self.sizeof_type(ts),
        }
    }

    /// Get the CType of a binary operation expression.
    fn get_binop_ctype(&self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Option<CType> {
        // Comparison and logical operators always produce int
        match op {
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge
            | BinOp::LogicalAnd | BinOp::LogicalOr => {
                return Some(CType::Int);
            }
            _ => {}
        }

        // Shift operators: result type is the promoted type of the left operand
        if matches!(op, BinOp::Shl | BinOp::Shr) {
            if let Some(lct) = self.get_expr_ctype(lhs) {
                return Some(Self::integer_promote_ctype(&lct));
            }
            return Some(CType::Int);
        }

        // Pointer arithmetic for Add and Sub
        if matches!(op, BinOp::Add | BinOp::Sub) {
            if let Some(lct) = self.get_expr_ctype(lhs) {
                match &lct {
                    CType::Pointer(_, _) => {
                        if *op == BinOp::Sub {
                            // ptr - ptr = ptrdiff_t (long)
                            if let Some(rct) = self.get_expr_ctype(rhs) {
                                if rct.is_pointer_like() {
                                    return Some(CType::Long);
                                }
                            }
                        }
                        return Some(lct);
                    }
                    CType::Array(elem, _) => {
                        if *op == BinOp::Sub {
                            // array - ptr/array = ptrdiff_t (long)
                            if let Some(rct) = self.get_expr_ctype(rhs) {
                                if rct.is_pointer_like() {
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
                if let Some(rct) = self.get_expr_ctype(rhs) {
                    match rct {
                        CType::Pointer(_, _) => return Some(rct),
                        CType::Array(elem, _) => return Some(CType::Pointer(elem, AddressSpace::Default)),
                        _ => {}
                    }
                }
            }
        }

        // For arithmetic (Add, Sub, Mul, Div, Mod) and bitwise (BitAnd, BitOr, BitXor)
        // operators on non-pointer types, apply C usual arithmetic conversions.
        let lct = self.get_expr_ctype(lhs);
        let rct = self.get_expr_ctype(rhs);
        match (lct, rct) {
            (Some(l), Some(r)) => Some(CType::usual_arithmetic_conversion(&l, &r)),
            (Some(l), None) => Some(Self::integer_promote_ctype(&l)),
            (None, Some(r)) => Some(Self::integer_promote_ctype(&r)),
            (None, None) => None,
        }
    }

    /// Apply C integer promotion rules to a CType.
    /// Types smaller than int are promoted to int.
    /// Delegates to CType::integer_promoted().
    fn integer_promote_ctype(ct: &CType) -> CType {
        ct.integer_promoted()
    }

    /// Resolve a potentially stale (forward-declared) struct/union CType by looking
    /// up the latest complete definition from the ctype_cache.
    /// If the struct has no fields but has a name, the cache may have the full definition.
    fn resolve_forward_declared_ctype(&self, ctype: CType) -> CType {
        match &ctype {
            CType::Struct(key) | CType::Union(key) => {
                // Check if the layout for this key is a forward-declaration stub (size 0, no fields)
                let is_incomplete = self.types.struct_layouts.get(&**key)
                    .map(|l| l.fields.is_empty())
                    .unwrap_or(true);
                if is_incomplete {
                    // Try the ctype_cache for a complete version
                    if let Some(cached) = self.types.ctype_cache.borrow().get(&**key) {
                        match cached {
                            CType::Struct(cached_key) | CType::Union(cached_key) => {
                                let cached_complete = self.types.struct_layouts.get(&**cached_key)
                                    .map(|l| !l.fields.is_empty())
                                    .unwrap_or(false);
                                if cached_complete {
                                    return cached.clone();
                                }
                            }
                            _ => {}
                        }
                    }
                }
                ctype
            }
            _ => ctype,
        }
    }

    /// Get the CType of a struct/union field.
    /// Recursively searches anonymous struct/union members to find the field,
    /// matching the behavior of StructLayout::field_offset().
    fn get_field_ctype(&self, base_expr: &Expr, field_name: &str, is_pointer_access: bool) -> Option<CType> {
        let base_ctype = if is_pointer_access {
            // For p->field, get CType of p, then dereference
            // Arrays decay to pointers, so arr->field is valid when arr is an array
            match self.get_expr_ctype(base_expr)? {
                CType::Pointer(inner, _) => *inner,
                CType::Array(inner, _) => *inner,
                _ => return None,
            }
        } else {
            self.get_expr_ctype(base_expr)?
        };
        // Resolve forward-declared (incomplete) struct/union types that may have
        // been cached before the full definition was available.
        let base_ctype = self.resolve_forward_declared_ctype(base_ctype);
        // Look up field in the struct/union type, recursing into anonymous members
        match &base_ctype {
            CType::Struct(key) | CType::Union(key) => {
                if let Some(layout) = self.types.struct_layouts.get(&**key) {
                    // Use field_offset which recursively searches anonymous
                    // struct/union members, returning the correct field type
                    if let Some((_offset, ctype)) = layout.field_offset(field_name, &self.types.struct_layouts) {
                        return Some(ctype);
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Look up the CType of an expression from sema's pre-computed annotation map.
    /// Returns None if sema did not annotate this expression (e.g., because the
    /// type depends on lowering-specific state like alloca info).
    fn lookup_sema_expr_type(&self, expr: &Expr) -> Option<CType> {
        let key = expr as *const Expr as usize;
        self.sema_expr_types.get(&key).cloned()
    }

    /// Get the full CType of an expression by recursion.
    /// Returns None if the type cannot be determined from CType tracking.
    ///
    /// Resolution order:
    /// 1. Lowerer-specific inference (uses locals, globals, func_meta, etc.)
    /// 2. Sema annotation fallback (pre-computed ExprTypeMap from sema pass)
    ///
    /// The lowerer-specific path is checked first because it has access to
    /// lowering state (variable allocas, global metadata) that may produce
    /// more precise types than sema's symbol-table-only inference.
    pub(super) fn get_expr_ctype(&self, expr: &Expr) -> Option<CType> {
        // Try lowerer-specific inference first
        let result = self.get_expr_ctype_lowerer(expr);
        if result.is_some() {
            return result;
        }
        // Fall back to sema's pre-computed type annotation
        self.lookup_sema_expr_type(expr)
    }

    /// Lowerer-specific CType inference using lowering state (locals, globals, func_meta).
    /// This is the original get_expr_ctype logic, now separated so the public
    /// get_expr_ctype can add a sema fallback after it.
    fn get_expr_ctype_lowerer(&self, expr: &Expr) -> Option<CType> {
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(vi) = self.lookup_var_info(name) {
                    return vi.c_type.clone();
                }
                // Fall back to sema's function signatures for function-typed identifiers
                // (e.g., taking address of a function: &func_name)
                if let Some(func_info) = self.sema_functions.get(name.as_str()) {
                    return Some(CType::Function(Box::new(crate::common::types::FunctionType {
                        return_type: func_info.return_type.clone(),
                        params: func_info.params.clone(),
                        variadic: func_info.variadic,
                    })));
                }
                None
            }
            Expr::Deref(inner, _) => {
                // Dereferencing peels off one Pointer/Array layer
                if let Some(inner_ct) = self.get_expr_ctype(inner) {
                    match inner_ct {
                        CType::Pointer(pointee, _) => return Some(*pointee),
                        CType::Array(elem, _) => return Some(*elem),
                        _ => {}
                    }
                }
                None
            }
            Expr::AddressOf(inner, _) => {
                // Address-of wraps in Pointer
                if let Some(inner_ct) = self.get_expr_ctype(inner) {
                    return Some(CType::Pointer(Box::new(inner_ct), AddressSpace::Default));
                }
                None
            }
            Expr::ArraySubscript(base, index, _) => {
                // Subscript peels off one Array/Pointer layer
                if let Some(base_ct) = self.get_expr_ctype(base) {
                    match base_ct {
                        CType::Array(elem, _) => return Some(*elem),
                        CType::Pointer(pointee, _) => return Some(*pointee),
                        _ => {}
                    }
                }
                // Reverse subscript: index[base]
                if let Some(idx_ct) = self.get_expr_ctype(index) {
                    match idx_ct {
                        CType::Array(elem, _) => return Some(*elem),
                        CType::Pointer(pointee, _) => return Some(*pointee),
                        _ => {}
                    }
                }
                None
            }
            Expr::Cast(ref type_spec, _, _) => {
                Some(self.type_spec_to_ctype(type_spec))
            }
            Expr::MemberAccess(base_expr, field_name, _) => {
                self.get_field_ctype(base_expr, field_name, false)
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                self.get_field_ctype(base_expr, field_name, true)
            }
            Expr::UnaryOp(UnaryOp::Plus, inner, _)
            | Expr::UnaryOp(UnaryOp::Neg, inner, _)
            | Expr::UnaryOp(UnaryOp::PreInc, inner, _)
            | Expr::UnaryOp(UnaryOp::PreDec, inner, _) => {
                self.get_expr_ctype(inner)
            }
            Expr::PostfixOp(_, inner, _) => self.get_expr_ctype(inner),
            Expr::Assign(lhs, _, _) | Expr::CompoundAssign(_, lhs, _, _) => {
                self.get_expr_ctype(lhs)
            }
            Expr::Conditional(_, then_expr, _, _) => self.get_expr_ctype(then_expr),
            Expr::GnuConditional(cond, _, _) => self.get_expr_ctype(cond),
            Expr::Comma(_, last, _) => self.get_expr_ctype(last),
            Expr::StringLiteral(_, _) => {
                // String literals have type char[] which decays to char*
                Some(CType::Pointer(Box::new(CType::Char), AddressSpace::Default))
            }
            Expr::BinaryOp(op, lhs, rhs, _) => {
                self.get_binop_ctype(op, lhs, rhs)
            }
            // Literal types for _Generic support
            Expr::IntLiteral(_, _) => Some(CType::Int),
            Expr::UIntLiteral(_, _) => Some(CType::UInt),
            Expr::LongLiteral(_, _) => Some(CType::Long),
            Expr::ULongLiteral(_, _) => Some(CType::ULong),
            Expr::CharLiteral(_, _) => Some(CType::Int), // char literals have type int in C
            Expr::FloatLiteral(_, _) => Some(CType::Double),
            Expr::FloatLiteralF32(_, _) => Some(CType::Float),
            Expr::FloatLiteralLongDouble(_, _) => Some(CType::LongDouble),
            // Wide string literal L"..." has type wchar_t* (which is int* on all targets)
            Expr::WideStringLiteral(_, _) => Some(CType::Pointer(Box::new(CType::Int), AddressSpace::Default)),
            Expr::FunctionCall(func, args, _) => {
                if let Expr::Identifier(name, _) = func.as_ref() {
                    // __builtin_choose_expr(const_expr, expr1, expr2): return CType
                    // of the selected branch expression.
                    if name == "__builtin_choose_expr" && args.len() >= 3 {
                        let cond = self.eval_const_expr(&args[0]);
                        let is_nonzero = match cond {
                            Some(IrConst::I64(v)) => v != 0,
                            Some(IrConst::I32(v)) => v != 0,
                            _ => true,
                        };
                        return if is_nonzero {
                            self.get_expr_ctype(&args[1])
                        } else {
                            self.get_expr_ctype(&args[2])
                        };
                    }
                    // First check lowerer's own func_meta (has ABI-adjusted return_ctype)
                    if let Some(ctype) = self.func_meta.sigs.get(name.as_str()).and_then(|s| s.return_ctype.as_ref()) {
                        return Some(ctype.clone());
                    }
                    // Fall back to sema's authoritative function signatures
                    if let Some(func_info) = self.sema_functions.get(name.as_str()) {
                        return Some(func_info.return_type.clone());
                    }
                }
                // For indirect calls through function pointer variables,
                // extract the return type from the pointer's CType.
                // Strip Deref layers since dereferencing function pointers is a no-op.
                let mut stripped_func: &Expr = func.as_ref();
                while let Expr::Deref(inner, _) = stripped_func {
                    stripped_func = inner;
                }
                let func_ctype = match stripped_func {
                    Expr::Identifier(name, _) => {
                        if let Some(vi) = self.lookup_var_info(name) {
                            vi.c_type.clone()
                        } else {
                            None
                        }
                    }
                    _ => self.get_expr_ctype(stripped_func),
                };
                if let Some(ctype) = func_ctype {
                    if let Some(ret_ct) = ctype.func_ptr_return_type(false) {
                        return Some(ret_ct);
                    }
                }
                None
            }
            Expr::VaArg(_, type_spec, _) | Expr::CompoundLiteral(type_spec, _, _) => {
                Some(self.type_spec_to_ctype(type_spec))
            }
            Expr::GenericSelection(controlling, associations, _) => {
                self.resolve_generic_selection_ctype(controlling, associations)
            }
            _ => None,
        }
    }
}
