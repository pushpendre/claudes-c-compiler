/// Expression type resolution and sizing.
///
/// This module contains functions for determining the IR type and size of C expressions.
/// It includes helpers for binary operations, subscript, function call return types,
/// `_Generic` selections, `sizeof` computation, and CType-level expression type resolution.

use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{CType, IrType};
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
/// A bitfield of type unsigned int with width < 32 promotes to int (I32),
/// because int can represent all values of the bitfield.
/// Non-bitfield types or full-width bitfields are returned unchanged.
fn bitfield_promoted_type(field_ty: IrType, bf_info: Option<(u32, u32)>) -> IrType {
    if let Some((_bit_offset, bit_width)) = bf_info {
        // Per C11 6.3.1.1p2: if int can represent all values of the bitfield,
        // it promotes to int; otherwise unsigned int.
        // For signed bitfields with width <= 32, int can always represent the values.
        // For unsigned bitfields with width < 32 (i.e., fewer bits than int),
        // int can represent all values (0..2^width-1 fits in signed 32-bit).
        // For unsigned bitfields with width == 32, int cannot represent all values,
        // so it stays unsigned int.
        match field_ty {
            IrType::U32 if bit_width < 32 => IrType::I32,
            IrType::I32 | IrType::U32 if bit_width <= 16 => IrType::I32,
            IrType::U16 if bit_width < 16 => IrType::I32,
            IrType::I16 | IrType::U16 => IrType::I32,
            IrType::U8 | IrType::I8 => IrType::I32,
            _ => field_ty,
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
                if let Some(resolved) = self.typedefs.get(name) {
                    self.is_type_spec_long_double(resolved)
                } else {
                    false
                }
            }
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
                if let Some(info) = self.locals.get(name) {
                    if info.is_struct { return Some(info.alloc_size); }
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
                if matches!(ctype, CType::Struct(_) | CType::Union(_)) { Some(ctype.size()) } else { None }
            }
            Expr::ArraySubscript(_, _, _) | Expr::Deref(_, _)
            | Expr::FunctionCall(_, _, _) | Expr::Conditional(_, _, _, _)
            | Expr::GnuConditional(_, _, _) => {
                let ctype = self.get_expr_ctype(expr)?;
                if matches!(ctype, CType::Struct(_) | CType::Union(_)) { Some(ctype.size()) } else { None }
            }
            Expr::CompoundLiteral(type_spec, _, _) => {
                let resolved = self.resolve_type_spec(type_spec);
                if matches!(resolved, TypeSpecifier::Struct(..) | TypeSpecifier::Union(..)) {
                    Some(self.sizeof_type(type_spec))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Extract the return IrType from a function pointer's CType.
    /// Handles the spurious Pointer wrapper that build_full_ctype adds for (*fp)() syntax.
    /// For `double (*fp)(void)`, the CType is Function { return_type: Pointer(Double) }
    /// and we need to unwrap the Pointer to get F64.
    fn extract_func_ptr_return_type(ctype: &CType) -> IrType {
        match ctype {
            CType::Pointer(inner) => match inner.as_ref() {
                CType::Function(ft) => Self::peel_ptr_from_return_type(&ft.return_type),
                // For parameter function pointers, CType is just Pointer(ReturnType)
                // without the Function wrapper (type_spec_to_ctype doesn't generate Function nodes)
                CType::Float => IrType::F32,
                CType::Double | CType::LongDouble => IrType::F64,
                // Pointer(Pointer(X)) means returning a pointer type
                CType::Pointer(_) => IrType::Ptr,
                _ => IrType::I64,
            },
            CType::Function(ft) => Self::peel_ptr_from_return_type(&ft.return_type),
            _ => IrType::I64,
        }
    }

    /// Peel one Pointer layer from a function's return_type CType.
    /// This compensates for build_full_ctype wrapping the return type in Pointer
    /// due to the (*fp) declarator syntax.
    pub(super) fn peel_ptr_from_return_type(return_type: &CType) -> IrType {
        match return_type {
            CType::Pointer(inner) => match inner.as_ref() {
                CType::Float => IrType::F32,
                CType::Double | CType::LongDouble => IrType::F64,
                CType::Void => IrType::I64,
                CType::Char | CType::UChar | CType::Short | CType::UShort
                | CType::Int | CType::UInt | CType::Bool => IrType::I32,
                CType::Long | CType::ULong | CType::LongLong | CType::ULongLong => IrType::I64,
                // Pointer(Pointer(...)) means actual pointer return type
                CType::Pointer(_) => IrType::Ptr,
                CType::Struct(_) | CType::Union(_) => IrType::Ptr,
                CType::Function(_) => IrType::Ptr,
                CType::Array(_, _) => IrType::Ptr,
                _ => IrType::I64,
            },
            // No Pointer wrapper - direct conversion
            _ => IrType::from_ctype(return_type),
        }
    }

    /// For indirect calls (function pointer calls), determine if the return type is a struct
    /// and return its size. Returns None if not a struct return or if the type cannot be determined.
    pub(super) fn get_call_return_struct_size(&self, func: &Expr) -> Option<usize> {
        // For indirect calls, extract the return type from the function pointer's CType
        let func_ctype = match func {
            Expr::Identifier(name, _) => {
                // Could be a function pointer variable
                if let Some(vi) = self.lookup_var_info(name) {
                    vi.c_type.clone()
                } else {
                    None
                }
            }
            Expr::Deref(inner, _) => self.get_expr_ctype(inner),
            _ => self.get_expr_ctype(func),
        };

        if let Some(ctype) = func_ctype {
            // Navigate through CType to find the return type
            let ret_ctype = Self::extract_func_ptr_return_ctype(&ctype);
            if let Some(ret_ct) = ret_ctype {
                if matches!(ret_ct, CType::Struct(_) | CType::Union(_)) {
                    return Some(ret_ct.size());
                }
            }
        }
        None
    }

    /// Extract the return CType from a function pointer CType.
    /// Handles Pointer(Function(ft)) patterns and peels the extra Pointer wrapper
    /// that build_full_ctype adds.
    fn extract_func_ptr_return_ctype(ctype: &CType) -> Option<CType> {
        match ctype {
            CType::Pointer(inner) => match inner.as_ref() {
                CType::Function(ft) => {
                    // Peel one Pointer wrapper from return type (added by declarator syntax)
                    match &ft.return_type {
                        CType::Pointer(ret_inner) => Some(ret_inner.as_ref().clone()),
                        other => Some(other.clone()),
                    }
                }
                // For typedef function pointers, the CType may be Pointer(ReturnType)
                // without the Function wrapper (build_full_ctype stores only the return
                // type for typedef'd function pointer variables). Since this function is
                // only called when we know the expression is a function call, the inner
                // type IS the return type.
                other => Some(other.clone()),
            },
            CType::Function(ft) => {
                match &ft.return_type {
                    CType::Pointer(ret_inner) => Some(ret_inner.as_ref().clone()),
                    other => Some(other.clone()),
                }
            }
            _ => None,
        }
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
                let lty = self.get_expr_type(lhs);
                let rty = self.get_expr_type(rhs);
                // Only check complex types if either operand is Ptr
                // (complex types are represented as Ptr in IR)
                if matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div) {
                    if lty == IrType::Ptr || rty == IrType::Ptr {
                        let lct = self.expr_ctype(lhs);
                        let rct = self.expr_ctype(rhs);
                        if lct.is_complex() || rct.is_complex() {
                            return IrType::Ptr;
                        }
                    }
                }
                if lty == IrType::F128 || rty == IrType::F128 {
                    IrType::F128
                } else if lty == IrType::F64 || rty == IrType::F64 {
                    IrType::F64
                } else if lty == IrType::F32 || rty == IrType::F32 {
                    IrType::F32
                } else {
                    Self::common_type(lty, rty)
                }
            }
        }
    }

    /// Get the IR type for an array subscript expression.
    fn get_subscript_type(&self, base: &Expr, index: &Expr) -> IrType {
        if let Some(base_ctype) = self.get_expr_ctype(base) {
            match base_ctype {
                CType::Array(elem, _) => return IrType::from_ctype(&elem),
                CType::Pointer(pointee) => return IrType::from_ctype(&pointee),
                _ => {}
            }
        }
        if let Some(idx_ctype) = self.get_expr_ctype(index) {
            match idx_ctype {
                CType::Array(elem, _) => return IrType::from_ctype(&elem),
                CType::Pointer(pointee) => return IrType::from_ctype(&pointee),
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
    fn get_call_return_type(&self, func: &Expr) -> IrType {
        if let Expr::Identifier(name, _) = func {
            if let Some(&ret_ty) = self.func_meta.return_types.get(name) {
                return ret_ty;
            }
            if let Some(&ret_ty) = self.func_meta.ptr_return_types.get(name) {
                return ret_ty;
            }
            if let Some(ret_ty) = Self::builtin_return_type(name) {
                return ret_ty;
            }
        }
        if let Expr::Deref(inner, _) = func {
            if let Expr::Identifier(name, _) = inner.as_ref() {
                if let Some(&ret_ty) = self.func_meta.return_types.get(name) {
                    return ret_ty;
                }
                if let Some(&ret_ty) = self.func_meta.ptr_return_types.get(name) {
                    return ret_ty;
                }
            }
            if let Some(inner_ctype) = self.get_expr_ctype(inner) {
                return Self::extract_func_ptr_return_type(&inner_ctype);
            }
        }
        if let Some(ctype) = self.get_expr_ctype(func) {
            return Self::extract_func_ptr_return_type(&ctype);
        }
        IrType::I64
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
            Expr::FunctionCall(func, _, _) => {
                self.get_call_return_type(func)
            }
            Expr::VaArg(_, type_spec, _) => self.resolve_va_arg_type(type_spec),
            Expr::Identifier(name, _) => {
                if name == "__func__" || name == "__FUNCTION__" || name == "__PRETTY_FUNCTION__" {
                    return IrType::Ptr;
                }
                if self.enum_constants.contains_key(name) {
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
                        CType::Pointer(pointee) => return IrType::from_ctype(&pointee),
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
            _ => IrType::I64,
        }
    }

    /// Get the sizeof for an identifier expression.
    fn sizeof_identifier(&self, name: &str) -> usize {
        if let Some(info) = self.locals.get(name) {
            if info.is_array || info.is_struct {
                return info.alloc_size;
            }
            // Use CType size if available (handles long double, function types, etc.)
            if let Some(ref ct) = info.c_type {
                if matches!(ct, CType::Function(_)) {
                    // GCC extension: sizeof(function_type) == 1
                    return 1;
                }
                let ct_size = ct.size();
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
                let ct_size = ct.size();
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
                CType::Pointer(pointee) => {
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
                    if let CType::Pointer(pointee) = ct {
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
                CType::Pointer(pointee) => return self.resolve_ctype_size(pointee).max(1),
                _ => {}
            }
        }
        // Also check reverse subscript (index[base])
        if let Some(idx_ctype) = self.get_expr_ctype(index) {
            match &idx_ctype {
                CType::Array(elem, _) => return self.resolve_ctype_size(elem).max(1),
                CType::Pointer(pointee) => return self.resolve_ctype_size(pointee).max(1),
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
        4 // default: int element
    }

    /// Get the sizeof for a member access expression.
    fn sizeof_member_access(&self, base_expr: &Expr, field_name: &str, is_pointer: bool) -> usize {
        if let Some(ctype) = self.resolve_field_ctype(base_expr, field_name, is_pointer) {
            let sz = ctype.size();
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
            Expr::Sizeof(_, _) | Expr::Alignof(_, _) => 8,

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
                    ctype.size()
                } else {
                    let ret_ty = self.get_expr_type(expr);
                    ret_ty.size()
                }
            }

            // Compound literal: size of the type (handle incomplete array types)
            Expr::CompoundLiteral(ts, ref init, _) => {
                let resolved = self.resolve_type_spec(ts);
                match (resolved, init.as_ref()) {
                    (TypeSpecifier::Array(ref elem, None), Initializer::List(items)) => {
                        self.sizeof_type(elem) * items.len()
                    }
                    _ => self.sizeof_type(ts),
                }
            }

            // Default
            _ => 4,
        }
    }

    /// Get the element size for a compound literal type.
    /// For arrays, returns the element size; for scalars/structs, returns the full size.
    pub(super) fn compound_literal_elem_size(&self, ts: &TypeSpecifier) -> usize {
        let resolved = self.resolve_type_spec(ts);
        match resolved {
            TypeSpecifier::Array(elem, _) => self.sizeof_type(elem),
            _ => self.sizeof_type(ts),
        }
    }

    /// Get the CType of a binary operation expression.
    fn get_binop_ctype(&self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Option<CType> {
        // Pointer arithmetic: ptr + int or int + ptr returns the pointer type
        // Pointer subtraction: ptr - ptr returns ptrdiff_t (Long)
        match op {
            BinOp::Add => {
                if let Some(lct) = self.get_expr_ctype(lhs) {
                    match lct {
                        CType::Pointer(_) => return Some(lct),
                        // Array decays to pointer-to-element in arithmetic context
                        CType::Array(elem, _) => return Some(CType::Pointer(elem)),
                        _ => {}
                    }
                }
                if let Some(rct) = self.get_expr_ctype(rhs) {
                    match rct {
                        CType::Pointer(_) => return Some(rct),
                        CType::Array(elem, _) => return Some(CType::Pointer(elem)),
                        _ => {}
                    }
                }
                None
            }
            BinOp::Sub => {
                if let Some(lct) = self.get_expr_ctype(lhs) {
                    let is_ptr = matches!(&lct, CType::Pointer(_) | CType::Array(_, _));
                    if is_ptr {
                        // Check if rhs is also pointer (ptr - ptr = ptrdiff_t)
                        if let Some(rct) = self.get_expr_ctype(rhs) {
                            if matches!(&rct, CType::Pointer(_) | CType::Array(_, _)) {
                                return Some(CType::Long);
                            }
                        }
                        // ptr - int = decayed pointer type
                        return Some(match lct {
                            CType::Array(elem, _) => CType::Pointer(elem),
                            other => other,
                        });
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Get the CType of a struct/union field.
    fn get_field_ctype(&self, base_expr: &Expr, field_name: &str, is_pointer_access: bool) -> Option<CType> {
        let base_ctype = if is_pointer_access {
            // For p->field, get CType of p, then dereference
            match self.get_expr_ctype(base_expr)? {
                CType::Pointer(inner) => *inner,
                _ => return None,
            }
        } else {
            self.get_expr_ctype(base_expr)?
        };
        // Look up field in the struct/union type
        match base_ctype {
            CType::Struct(st) | CType::Union(st) => {
                for field in &st.fields {
                    if field.name == field_name {
                        return Some(field.ty.clone());
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Get the full CType of an expression by recursion.
    /// Returns None if the type cannot be determined from CType tracking.
    pub(super) fn get_expr_ctype(&self, expr: &Expr) -> Option<CType> {
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(vi) = self.lookup_var_info(name) {
                    return vi.c_type.clone();
                }
                None
            }
            Expr::Deref(inner, _) => {
                // Dereferencing peels off one Pointer/Array layer
                if let Some(inner_ct) = self.get_expr_ctype(inner) {
                    match inner_ct {
                        CType::Pointer(pointee) => return Some(*pointee),
                        CType::Array(elem, _) => return Some(*elem),
                        _ => {}
                    }
                }
                None
            }
            Expr::AddressOf(inner, _) => {
                // Address-of wraps in Pointer
                if let Some(inner_ct) = self.get_expr_ctype(inner) {
                    return Some(CType::Pointer(Box::new(inner_ct)));
                }
                None
            }
            Expr::ArraySubscript(base, index, _) => {
                // Subscript peels off one Array/Pointer layer
                if let Some(base_ct) = self.get_expr_ctype(base) {
                    match base_ct {
                        CType::Array(elem, _) => return Some(*elem),
                        CType::Pointer(pointee) => return Some(*pointee),
                        _ => {}
                    }
                }
                // Reverse subscript: index[base]
                if let Some(idx_ct) = self.get_expr_ctype(index) {
                    match idx_ct {
                        CType::Array(elem, _) => return Some(*elem),
                        CType::Pointer(pointee) => return Some(*pointee),
                        _ => {}
                    }
                }
                None
            }
            Expr::Cast(ref type_spec, _, _) => {
                let resolved = self.resolve_type_spec(type_spec);
                Some(self.type_spec_to_ctype(resolved))
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
                Some(CType::Pointer(Box::new(CType::Char)))
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
            Expr::WideStringLiteral(_, _) => Some(CType::Pointer(Box::new(CType::Int))),
            Expr::FunctionCall(func, _, _) => {
                if let Expr::Identifier(name, _) = func.as_ref() {
                    if let Some(ctype) = self.func_meta.return_ctypes.get(name) {
                        return Some(ctype.clone());
                    }
                }
                // For indirect calls through function pointer variables,
                // extract the return type from the pointer's CType
                let func_ctype = match func.as_ref() {
                    Expr::Identifier(name, _) => {
                        if let Some(vi) = self.lookup_var_info(name) {
                            vi.c_type.clone()
                        } else {
                            None
                        }
                    }
                    Expr::Deref(inner, _) => self.get_expr_ctype(inner),
                    _ => self.get_expr_ctype(func),
                };
                if let Some(ctype) = func_ctype {
                    if let Some(ret_ct) = Self::extract_func_ptr_return_ctype(&ctype) {
                        return Some(ret_ct);
                    }
                }
                None
            }
            Expr::VaArg(_, type_spec, _) | Expr::CompoundLiteral(type_spec, _, _) => {
                let resolved = self.resolve_type_spec(type_spec);
                Some(self.type_spec_to_ctype(resolved))
            }
            _ => None,
        }
    }
}
