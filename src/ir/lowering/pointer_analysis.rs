//! Pointer type analysis for expressions.
//!
//! This module handles determining whether expressions are pointers, computing
//! their pointee sizes (for pointer arithmetic scaling), and resolving pointee
//! IR types (for correct loads/stores through pointer dereference and subscript).

use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, StructLayout, CType};
use super::lowering::Lowerer;

impl Lowerer {
    /// For a pointer-to-struct parameter type (e.g., `struct TAG *p`), get the
    /// pointed-to struct's layout. This enables `p->field` access.
    pub(super) fn get_struct_layout_for_pointer_param(&self, type_spec: &TypeSpecifier) -> Option<StructLayout> {
        // Resolve typedefs first (e.g., typedef union Outer *OuterPtr)
        let resolved = self.resolve_type_spec(type_spec);
        match resolved {
            TypeSpecifier::Pointer(inner) => self.get_struct_layout_for_type(inner),
            _ => None,
        }
    }

    /// Compute the IR type of the pointee for a pointer/array type specifier.
    /// For `Pointer(Char)`, returns Some(I8).
    /// For `Pointer(Int)`, returns Some(I32).
    /// For `Array(Int, _)`, returns Some(I32).
    /// Resolves typedef names before pattern matching.
    pub(super) fn pointee_ir_type(&self, type_spec: &TypeSpecifier) -> Option<IrType> {
        let resolved = self.resolve_type_spec(type_spec);
        match &resolved {
            TypeSpecifier::Pointer(inner) => Some(self.type_spec_to_ir(inner)),
            TypeSpecifier::Array(inner, _) => Some(self.type_spec_to_ir(inner)),
            _ => None,
        }
    }

    /// Compute the pointee type for a declaration, considering both the base type
    /// specifier and derived declarators (pointer/array).
    /// For `char *s` (type_spec=Char, derived=[Pointer]): returns Some(I8)
    /// For `int *p` (type_spec=Int, derived=[Pointer]): returns Some(I32)
    /// For `int **pp` (type_spec=Int, derived=[Pointer, Pointer]): returns Some(Ptr)
    /// For `int a[10]` (type_spec=Int, derived=[Array(10)]): returns Some(I32)
    pub(super) fn compute_pointee_type(&self, type_spec: &TypeSpecifier, derived: &[DerivedDeclarator]) -> Option<IrType> {
        // Count pointer and array levels
        let ptr_count = derived.iter().filter(|d| matches!(d, DerivedDeclarator::Pointer)).count();
        let has_array = derived.iter().any(|d| matches!(d, DerivedDeclarator::Array(_)));

        if ptr_count > 1 {
            // Multi-level pointer (e.g., int **pp) - pointee is a pointer
            Some(IrType::Ptr)
        } else if ptr_count == 1 {
            // Single pointer - pointee is the base type
            if has_array {
                Some(IrType::Ptr)
            } else {
                match type_spec {
                    TypeSpecifier::Pointer(inner) => Some(self.type_spec_to_ir(inner)),
                    _ => Some(self.type_spec_to_ir(type_spec)),
                }
            }
        } else if has_array {
            // Array (e.g., int a[10]) - element type is the base type
            Some(self.type_spec_to_ir(type_spec))
        } else {
            // Check if the type_spec itself is a pointer
            self.pointee_ir_type(type_spec)
        }
    }

    /// Check if an lvalue expression targets a _Bool variable (requires normalization).
    /// Handles direct identifiers, pointer dereferences (*pval), array subscripts (arr[i]),
    /// and member accesses (s.field, p->field) where the target type is _Bool.
    pub(super) fn is_bool_lvalue(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(info) = self.func_state.as_ref().and_then(|fs| fs.locals.get(name)) {
                    return info.is_bool;
                }
                if let Some(vi) = self.lookup_var_info(name) {
                    if let Some(ref ct) = vi.c_type {
                        return matches!(ct, CType::Bool);
                    }
                }
                false
            }
            Expr::Deref(_, _)
            | Expr::ArraySubscript(_, _, _)
            | Expr::MemberAccess(_, _, _)
            | Expr::PointerMemberAccess(_, _, _) => {
                // Use CType resolution to check if the lvalue target is _Bool
                if let Some(ct) = self.get_expr_ctype(expr) {
                    return matches!(ct, CType::Bool);
                }
                false
            }
            _ => false,
        }
    }

    /// Check if an expression has pointer type (for pointer arithmetic).
    pub(super) fn expr_is_pointer(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(vi) = self.lookup_var_info(name) {
                    // Arrays decay to pointers in expression context
                    return (vi.ty == IrType::Ptr || vi.is_array) && !vi.is_struct;
                }
                false
            }
            Expr::AddressOf(_, _) => true,
            Expr::PostfixOp(_, inner, _) => self.expr_is_pointer(inner),
            Expr::UnaryOp(op, inner, _) => {
                match op {
                    UnaryOp::PreInc | UnaryOp::PreDec => self.expr_is_pointer(inner),
                    _ => false,
                }
            }
            Expr::ArraySubscript(base, _, _) => {
                // Result of subscript on pointer-to-pointer
                if let Some(pt) = self.get_pointee_type_of_expr(base) {
                    return pt == IrType::Ptr;
                }
                false
            }
            Expr::Cast(ref type_spec, _, _) => {
                matches!(type_spec, TypeSpecifier::Pointer(_))
            }
            Expr::StringLiteral(_, _) | Expr::WideStringLiteral(_, _) => true,
            Expr::BinaryOp(op, lhs, rhs, _) => {
                match op {
                    BinOp::Add => {
                        // ptr + int or int + ptr yields a pointer
                        self.expr_is_pointer(lhs) || self.expr_is_pointer(rhs)
                    }
                    BinOp::Sub => {
                        // ptr - int yields a pointer; ptr - ptr yields an integer
                        let lhs_ptr = self.expr_is_pointer(lhs);
                        let rhs_ptr = self.expr_is_pointer(rhs);
                        lhs_ptr && !rhs_ptr
                    }
                    _ => false,
                }
            }
            Expr::Conditional(_, then_expr, else_expr, _) => {
                self.expr_is_pointer(then_expr) || self.expr_is_pointer(else_expr)
            }
            Expr::GnuConditional(cond, else_expr, _) => {
                self.expr_is_pointer(cond) || self.expr_is_pointer(else_expr)
            }
            Expr::Comma(_, rhs, _) => self.expr_is_pointer(rhs),
            Expr::FunctionCall(func, _, _) => {
                // Check CType for function call return
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    return matches!(ctype, CType::Pointer(_));
                }
                // Fallback: check IrType return type
                if let Expr::Identifier(name, _) = func.as_ref() {
                    if let Some(ret_ty) = self.func_meta.sigs.get(name.as_str()).map(|s| s.return_type) {
                        return ret_ty == IrType::Ptr;
                    }
                }
                false
            }
            Expr::MemberAccess(base_expr, field_name, _) => {
                if let Some(ctype) = self.resolve_field_ctype(base_expr, field_name, false) {
                    return matches!(ctype, CType::Array(_, _) | CType::Pointer(_));
                }
                false
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                if let Some(ctype) = self.resolve_field_ctype(base_expr, field_name, true) {
                    return matches!(ctype, CType::Array(_, _) | CType::Pointer(_));
                }
                false
            }
            Expr::Deref(_, _) => {
                // Dereferencing a pointer-to-array yields an array which decays to pointer.
                // Dereferencing a pointer-to-pointer yields a pointer.
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    return matches!(ctype, CType::Array(_, _) | CType::Pointer(_));
                }
                false
            }
            Expr::Assign(_, _, _) | Expr::CompoundAssign(_, _, _, _) => {
                // Assignment result has the type of the LHS
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    return matches!(ctype, CType::Array(_, _) | CType::Pointer(_));
                }
                false
            }
            _ => false,
        }
    }

    /// Resolve the actual size of a CType, handling forward-declared/self-referential
    /// struct/union types whose cached_size may be stale (0 or wrong).
    /// For tagged structs/unions, always prefer the authoritative struct_layouts
    /// HashMap which is updated when the full definition is encountered.
    pub(super) fn resolve_ctype_size(&self, ctype: &CType) -> usize {
        // For tagged struct/union types, always look up the authoritative layout first.
        // The CType's cached_size may be stale if the type was captured before the
        // full definition was processed (e.g., forward-declared union embedded in
        // another type's CType that was built before the union was fully defined).
        match ctype {
            CType::Struct(st) => {
                if let Some(tag) = &st.name {
                    let key = format!("struct.{}", tag);
                    if let Some(layout) = self.struct_layouts.get(&key) {
                        return layout.size;
                    }
                }
            }
            CType::Union(st) => {
                if let Some(tag) = &st.name {
                    let key = format!("union.{}", tag);
                    if let Some(layout) = self.struct_layouts.get(&key) {
                        return layout.size;
                    }
                }
            }
            _ => {}
        }
        // Fall back to the cached size in the CType itself
        ctype.size()
    }

    /// Get the element size for a pointer expression (for scaling in pointer arithmetic).
    /// For `int *p`, returns 4. For `char *s`, returns 1.
    pub(super) fn get_pointer_elem_size_from_expr(&self, expr: &Expr) -> usize {
        // Try CType-based resolution first for accurate type information
        if let Some(ctype) = self.get_expr_ctype(expr) {
            match &ctype {
                CType::Pointer(pointee) => return self.resolve_ctype_size(pointee).max(1),
                CType::Array(elem, _) => return self.resolve_ctype_size(elem).max(1),
                _ => {}
            }
        }
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(vi) = self.lookup_var_info(name) {
                    if let Some(pt) = vi.pointee_type {
                        return pt.size();
                    }
                    if vi.elem_size > 0 {
                        return vi.elem_size;
                    }
                }
                8
            }
            Expr::PostfixOp(_, inner, _) => self.get_pointer_elem_size_from_expr(inner),
            Expr::UnaryOp(op, inner, _) => {
                match op {
                    UnaryOp::PreInc | UnaryOp::PreDec => self.get_pointer_elem_size_from_expr(inner),
                    _ => 8,
                }
            }
            Expr::BinaryOp(op, lhs, rhs, _) => {
                // ptr + int or ptr - int: get elem size from the pointer operand
                match op {
                    BinOp::Add => {
                        if self.expr_is_pointer(lhs) {
                            self.get_pointer_elem_size_from_expr(lhs)
                        } else if self.expr_is_pointer(rhs) {
                            self.get_pointer_elem_size_from_expr(rhs)
                        } else {
                            8
                        }
                    }
                    BinOp::Sub => {
                        if self.expr_is_pointer(lhs) {
                            self.get_pointer_elem_size_from_expr(lhs)
                        } else {
                            8
                        }
                    }
                    _ => 8,
                }
            }
            Expr::Conditional(_, then_expr, _, _) => self.get_pointer_elem_size_from_expr(then_expr),
            Expr::GnuConditional(cond, _, _) => self.get_pointer_elem_size_from_expr(cond),
            Expr::Comma(_, rhs, _) => self.get_pointer_elem_size_from_expr(rhs),
            Expr::FunctionCall(_, _, _) => {
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    if let CType::Pointer(pointee) = &ctype {
                        return self.resolve_ctype_size(pointee).max(1);
                    }
                }
                8
            }
            Expr::AddressOf(inner, _) => {
                // &x: pointer to typeof(x) -- use sizeof_expr which correctly
                // resolves struct/union sizes via CType and struct_layouts,
                // unlike get_expr_type().size() which returns 8 for all
                // struct/union types (since they map to IrType::Ptr in the IR).
                self.sizeof_expr(inner).max(1)
            }
            Expr::Cast(ref type_spec, _, _) => {
                if let TypeSpecifier::Pointer(ref inner) = type_spec {
                    self.sizeof_type(inner)
                } else {
                    8
                }
            }
            Expr::MemberAccess(base_expr, field_name, _) | Expr::PointerMemberAccess(base_expr, field_name, _) => {
                let is_ptr = matches!(expr, Expr::PointerMemberAccess(..));
                if let Some(ctype) = self.resolve_field_ctype(base_expr, field_name, is_ptr) {
                    match &ctype {
                        CType::Array(elem_ty, _) => return self.resolve_ctype_size(elem_ty).max(1),
                        CType::Pointer(pointee_ty) => return self.resolve_ctype_size(pointee_ty).max(1),
                        _ => {}
                    }
                }
                8
            }
            _ => {
                // Try using get_pointee_type_of_expr as a fallback
                if let Some(pt) = self.get_pointee_type_of_expr(expr) {
                    return pt.size();
                }
                8
            }
        }
    }

    /// Get the pointee type for a pointer expression - i.e., what type you get when dereferencing it.
    pub(super) fn get_pointee_type_of_expr(&self, expr: &Expr) -> Option<IrType> {
        // First try CType-based resolution (handles multi-level pointers correctly)
        if let Some(ctype) = self.get_expr_ctype(expr) {
            match ctype {
                CType::Pointer(inner) => return Some(IrType::from_ctype(&inner)),
                CType::Array(elem, _) => return Some(IrType::from_ctype(&elem)),
                _ => {}
            }
        }
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(vi) = self.lookup_var_info(name) {
                    return vi.pointee_type;
                }
                None
            }
            Expr::PostfixOp(_, inner, _) => {
                self.get_pointee_type_of_expr(inner)
            }
            Expr::UnaryOp(op, inner, _) => {
                match op {
                    UnaryOp::PreInc | UnaryOp::PreDec => {
                        self.get_pointee_type_of_expr(inner)
                    }
                    _ => None,
                }
            }
            Expr::BinaryOp(_, lhs, rhs, _) => {
                if let Some(pt) = self.get_pointee_type_of_expr(lhs) {
                    return Some(pt);
                }
                self.get_pointee_type_of_expr(rhs)
            }
            Expr::Cast(ref type_spec, inner, _) => {
                if let TypeSpecifier::Pointer(ref pointee_ts) = type_spec {
                    let pt = self.type_spec_to_ir(pointee_ts);
                    return Some(pt);
                }
                self.get_pointee_type_of_expr(inner)
            }
            Expr::Conditional(_, then_expr, else_expr, _) => {
                if let Some(pt) = self.get_pointee_type_of_expr(then_expr) {
                    return Some(pt);
                }
                self.get_pointee_type_of_expr(else_expr)
            }
            Expr::GnuConditional(cond, else_expr, _) => {
                if let Some(pt) = self.get_pointee_type_of_expr(cond) {
                    return Some(pt);
                }
                self.get_pointee_type_of_expr(else_expr)
            }
            Expr::Comma(_, last, _) => {
                self.get_pointee_type_of_expr(last)
            }
            Expr::AddressOf(inner, _) => {
                let ty = self.get_expr_type(inner);
                Some(ty)
            }
            Expr::Assign(_, rhs, _) => {
                self.get_pointee_type_of_expr(rhs)
            }
            Expr::MemberAccess(base_expr, field_name, _) | Expr::PointerMemberAccess(base_expr, field_name, _) => {
                let is_ptr = matches!(expr, Expr::PointerMemberAccess(..));
                if let Some(ctype) = self.resolve_field_ctype(base_expr, field_name, is_ptr) {
                    match ctype {
                        CType::Pointer(inner) => return Some(IrType::from_ctype(&inner)),
                        CType::Array(elem, _) => return Some(IrType::from_ctype(&elem)),
                        _ => {}
                    }
                }
                None
            }
            _ => None,
        }
    }
}
