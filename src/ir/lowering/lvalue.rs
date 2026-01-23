use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, CType};
use super::lowering::{Lowerer, LValue};

impl Lowerer {
    /// Try to get the lvalue (address) of an expression.
    /// Returns Some(LValue) if the expression is an lvalue, None otherwise.
    pub(super) fn lower_lvalue(&mut self, expr: &Expr) -> Option<LValue> {
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(info) = self.locals.get(name).cloned() {
                    Some(LValue::Variable(info.alloca))
                } else if self.globals.contains_key(name) {
                    // Global variable: emit GlobalAddr to get its address
                    let addr = self.fresh_value();
                    self.emit(Instruction::GlobalAddr { dest: addr, name: name.clone() });
                    Some(LValue::Address(addr))
                } else {
                    None
                }
            }
            Expr::Deref(inner, _) => {
                // *ptr -> the address is the value of ptr
                let ptr_val = self.lower_expr(inner);
                match ptr_val {
                    Operand::Value(v) => Some(LValue::Address(v)),
                    Operand::Const(_) => {
                        // Constant address - copy to a value
                        let dest = self.fresh_value();
                        self.emit(Instruction::Copy { dest, src: ptr_val });
                        Some(LValue::Address(dest))
                    }
                }
            }
            Expr::ArraySubscript(base, index, _) => {
                // base[index] -> compute address of element
                let addr = self.compute_array_element_addr(base, index);
                Some(LValue::Address(addr))
            }
            Expr::MemberAccess(base_expr, field_name, _) => {
                // s.field as lvalue -> compute address of the field
                let (field_offset, _field_ty) = self.resolve_member_access(base_expr, field_name);
                let base_addr = self.get_struct_base_addr(base_expr);
                let field_addr = self.fresh_value();
                self.emit(Instruction::GetElementPtr {
                    dest: field_addr,
                    base: base_addr,
                    offset: Operand::Const(IrConst::I64(field_offset as i64)),
                    ty: IrType::Ptr,
                });
                Some(LValue::Address(field_addr))
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                // p->field as lvalue -> load pointer, compute field address
                let ptr_val = self.lower_expr(base_expr);
                let base_addr = match ptr_val {
                    Operand::Value(v) => v,
                    Operand::Const(_) => {
                        let tmp = self.fresh_value();
                        self.emit(Instruction::Copy { dest: tmp, src: ptr_val });
                        tmp
                    }
                };
                let (field_offset, _field_ty) = self.resolve_pointer_member_access(base_expr, field_name);
                let field_addr = self.fresh_value();
                self.emit(Instruction::GetElementPtr {
                    dest: field_addr,
                    base: base_addr,
                    offset: Operand::Const(IrConst::I64(field_offset as i64)),
                    ty: IrType::Ptr,
                });
                Some(LValue::Address(field_addr))
            }
            _ => None,
        }
    }

    /// Get the address (as a Value) from an LValue.
    pub(super) fn lvalue_addr(&self, lv: &LValue) -> Value {
        match lv {
            LValue::Variable(v) => *v,
            LValue::Address(v) => *v,
        }
    }

    /// Load the value from an lvalue with a specific type.
    pub(super) fn load_lvalue_typed(&mut self, lv: &LValue, ty: IrType) -> Operand {
        let addr = self.lvalue_addr(lv);
        let dest = self.fresh_value();
        self.emit(Instruction::Load { dest, ptr: addr, ty });
        Operand::Value(dest)
    }

    /// Store a value to an lvalue with a specific type.
    pub(super) fn store_lvalue_typed(&mut self, lv: &LValue, val: Operand, ty: IrType) {
        let addr = self.lvalue_addr(lv);
        self.emit(Instruction::Store { val, ptr: addr, ty });
    }

    /// Compute the address of an array element: base_addr + index * elem_size.
    /// For multi-dimensional arrays, uses the correct stride for the subscript depth.
    /// Handles reverse subscript (e.g., 3[arr]) by normalizing operands so that
    /// the pointer/array is always the base and the integer is the index.
    pub(super) fn compute_array_element_addr(&mut self, base: &Expr, index: &Expr) -> Value {
        // In C, a[i] == i[a] == *(a + i). The parser always puts the expression
        // before '[' as base and the one inside as index. We normalize so the
        // pointer/array is always the base.
        let (actual_base, actual_index) = if self.expr_is_pointer(base) || self.expr_is_array_name(base) {
            (base, index)
        } else if self.expr_is_pointer(index) || self.expr_is_array_name(index) {
            // Reverse subscript: integer[array] -> swap to array[integer]
            (index, base)
        } else {
            // Default: assume first is base
            (base, index)
        };

        let index_val = self.lower_expr(actual_index);

        // Determine the element size based on subscript depth for multi-dim arrays
        let elem_size = self.get_array_elem_size_for_subscript(actual_base);

        // Get the base address - for nested subscripts (a[i][j]), don't load
        let base_addr = self.get_array_base_addr(actual_base);

        // Compute offset = index * elem_size
        let offset = if elem_size == 1 {
            index_val
        } else {
            let size_const = Operand::Const(IrConst::I64(elem_size as i64));
            let mul_dest = self.fresh_value();
            self.emit(Instruction::BinOp {
                dest: mul_dest,
                op: IrBinOp::Mul,
                lhs: index_val,
                rhs: size_const,
                ty: IrType::I64,
            });
            Operand::Value(mul_dest)
        };

        // GEP: base + offset
        let addr = self.fresh_value();
        let base_val = match base_addr {
            Operand::Value(v) => v,
            Operand::Const(_) => {
                let tmp = self.fresh_value();
                self.emit(Instruction::Copy { dest: tmp, src: base_addr });
                tmp
            }
        };
        self.emit(Instruction::GetElementPtr {
            dest: addr,
            base: base_val,
            offset,
            ty: IrType::I64,
        });
        addr
    }

    /// Get the base address for an array expression.
    /// For declared arrays, this is the alloca itself.
    /// For pointers, this is the loaded pointer value.
    /// For nested subscripts (a[i] where a is multi-dim), compute address without loading.
    pub(super) fn get_array_base_addr(&mut self, base: &Expr) -> Operand {
        match base {
            Expr::Identifier(name, _) => {
                if let Some(info) = self.locals.get(name).cloned() {
                    if info.is_array {
                        return Operand::Value(info.alloca);
                    } else {
                        let loaded = self.fresh_value();
                        self.emit(Instruction::Load { dest: loaded, ptr: info.alloca, ty: IrType::Ptr });
                        return Operand::Value(loaded);
                    }
                }
                if let Some(ginfo) = self.globals.get(name).cloned() {
                    let addr = self.fresh_value();
                    self.emit(Instruction::GlobalAddr { dest: addr, name: name.clone() });
                    if ginfo.is_array {
                        return Operand::Value(addr);
                    } else {
                        let loaded = self.fresh_value();
                        self.emit(Instruction::Load { dest: loaded, ptr: addr, ty: IrType::Ptr });
                        return Operand::Value(loaded);
                    }
                }
                self.lower_expr(base)
            }
            Expr::ArraySubscript(inner_base, inner_index, _) => {
                // For multi-dim arrays: a[i] when a is int[2][3] should return
                // the address (a + i*stride), NOT load from it.
                if self.subscript_result_is_array(base) {
                    // This is a sub-array access - compute address, don't load
                    let addr = self.compute_array_element_addr(inner_base, inner_index);
                    return Operand::Value(addr);
                }
                // Otherwise it's a pointer - evaluate and use as address
                self.lower_expr(base)
            }
            Expr::MemberAccess(base_expr, field_name, _) => {
                // Check if the field is an array type - arrays decay to pointers,
                // so we return the field address (not load the value).
                let field_is_array = self.resolve_member_field_ctype(base_expr, field_name)
                    .map(|ct| matches!(ct, CType::Array(_, _)))
                    .unwrap_or(false);
                if field_is_array {
                    // Return address of the array field (array decays to pointer)
                    let (field_offset, _) = self.resolve_member_access(base_expr, field_name);
                    let base_addr = self.get_struct_base_addr(base_expr);
                    let field_addr = self.fresh_value();
                    self.emit(Instruction::GetElementPtr {
                        dest: field_addr,
                        base: base_addr,
                        offset: Operand::Const(IrConst::I64(field_offset as i64)),
                        ty: IrType::Ptr,
                    });
                    return Operand::Value(field_addr);
                }
                // Not an array field - load the pointer value
                self.lower_expr(base)
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                // Check if the field is an array type - arrays decay to pointers
                let field_is_array = self.resolve_pointer_member_field_ctype(base_expr, field_name)
                    .map(|ct| matches!(ct, CType::Array(_, _)))
                    .unwrap_or(false);
                if field_is_array {
                    // Return address of the array field (array decays to pointer)
                    let ptr_val = self.lower_expr(base_expr);
                    let base_addr = match ptr_val {
                        Operand::Value(v) => v,
                        Operand::Const(_) => {
                            let tmp = self.fresh_value();
                            self.emit(Instruction::Copy { dest: tmp, src: ptr_val });
                            tmp
                        }
                    };
                    let (field_offset, _) = self.resolve_pointer_member_access(base_expr, field_name);
                    let field_addr = self.fresh_value();
                    self.emit(Instruction::GetElementPtr {
                        dest: field_addr,
                        base: base_addr,
                        offset: Operand::Const(IrConst::I64(field_offset as i64)),
                        ty: IrType::Ptr,
                    });
                    return Operand::Value(field_addr);
                }
                // Not an array field - load the pointer value
                self.lower_expr(base)
            }
            _ => {
                self.lower_expr(base)
            }
        }
    }

    /// Get the element size for an array subscript, accounting for multi-dimensional arrays.
    /// For a[i] where a is int[2][3], returns 12 (3*4 = stride of first dimension).
    /// For a[i][j] where a is int[2][3], returns 4 (element size).
    pub(super) fn get_array_elem_size_for_subscript(&self, base: &Expr) -> usize {
        // String literals have char elements (size 1)
        if matches!(base, Expr::StringLiteral(_, _)) {
            return 1;
        }

        // Handle struct/union member access: s.field[i] or p->field[i]
        if let Some(elem_size) = self.get_member_array_elem_size(base) {
            return elem_size;
        }

        // Find the root array name and count subscript depth
        let root_name = self.get_array_root_name_from_base(base);
        let depth = self.count_subscript_depth(base);

        if let Some(name) = root_name {
            if let Some(info) = self.locals.get(&name) {
                if !info.array_dim_strides.is_empty() {
                    // depth 0 means base is the array name, so use strides[0]
                    // depth 1 means base is a[i], so use strides[1]
                    if depth < info.array_dim_strides.len() {
                        return info.array_dim_strides[depth];
                    }
                    return *info.array_dim_strides.last().unwrap_or(&info.elem_size.max(1));
                }
                if info.elem_size > 0 {
                    return info.elem_size;
                }
            }
            if let Some(ginfo) = self.globals.get(&name) {
                if !ginfo.array_dim_strides.is_empty() {
                    if depth < ginfo.array_dim_strides.len() {
                        return ginfo.array_dim_strides[depth];
                    }
                    return *ginfo.array_dim_strides.last().unwrap_or(&ginfo.elem_size.max(1));
                }
                if ginfo.elem_size > 0 {
                    return ginfo.elem_size;
                }
            }
        }

        // Fallback to old behavior
        self.get_array_elem_size(base)
    }

    /// Get the element size for a struct/union member that is an array or pointer.
    /// For s.field where field is `char[8]`, returns 1 (sizeof(char)).
    /// For s.arr where arr is `int[10]`, returns 4 (sizeof(int)).
    /// For s.ptr where ptr is `struct S *`, returns sizeof(struct S).
    fn get_member_array_elem_size(&self, base: &Expr) -> Option<usize> {
        match base {
            Expr::MemberAccess(base_expr, field_name, _) => {
                if let Some(ctype) = self.resolve_member_field_ctype(base_expr, field_name) {
                    match &ctype {
                        CType::Array(elem_ty, _) => return Some(elem_ty.size()),
                        CType::Pointer(pointee_ty) => return Some(pointee_ty.size()),
                        _ => {}
                    }
                }
                None
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                if let Some(ctype) = self.resolve_pointer_member_field_ctype(base_expr, field_name) {
                    match &ctype {
                        CType::Array(elem_ty, _) => return Some(elem_ty.size()),
                        CType::Pointer(pointee_ty) => return Some(pointee_ty.size()),
                        _ => {}
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Get the element size for array subscript (legacy, for 1D arrays).
    pub(super) fn get_array_elem_size(&self, base: &Expr) -> usize {
        // String literals have char elements (size 1)
        if matches!(base, Expr::StringLiteral(_, _)) {
            return 1;
        }
        if let Expr::Identifier(name, _) = base {
            if let Some(info) = self.locals.get(name) {
                if info.elem_size > 0 {
                    return info.elem_size;
                }
                // Use pointee_type as fallback
                if let Some(pt) = info.pointee_type {
                    return pt.size();
                }
            }
            if let Some(ginfo) = self.globals.get(name) {
                if ginfo.elem_size > 0 {
                    return ginfo.elem_size;
                }
                if let Some(pt) = ginfo.pointee_type {
                    return pt.size();
                }
            }
        }
        // Default element size: for pointers we don't know the element type,
        // use 8 bytes as a safe default for pointer dereferencing.
        8
    }

    /// Check if the result of an array subscript is still a sub-array (not a scalar).
    /// For int a[2][3]: a[i] results in a sub-array (int[3]), a[i][j] is a scalar.
    pub(super) fn subscript_result_is_array(&self, expr: &Expr) -> bool {
        if let Expr::ArraySubscript(base, _, _) = expr {
            let root_name = self.get_array_root_name_from_base(base);
            let depth = self.count_subscript_depth(base) + 1; // +1 for this subscript

            if let Some(name) = root_name {
                if let Some(info) = self.locals.get(&name) {
                    if info.array_dim_strides.len() > 1 {
                        return depth < info.array_dim_strides.len();
                    }
                }
                if let Some(ginfo) = self.globals.get(&name) {
                    if ginfo.array_dim_strides.len() > 1 {
                        return depth < ginfo.array_dim_strides.len();
                    }
                }
            }
        }
        false
    }

    /// Count the subscript nesting depth from the base expression.
    /// For Identifier: depth = 0
    /// For ArraySubscript(Identifier, _): depth = 1
    /// For ArraySubscript(ArraySubscript(Identifier, _), _): depth = 2
    pub(super) fn count_subscript_depth(&self, base: &Expr) -> usize {
        match base {
            Expr::ArraySubscript(inner, _, _) => 1 + self.count_subscript_depth(inner),
            _ => 0,
        }
    }

    /// Get the root array name from the base of a subscript expression.
    /// For Identifier("a"): returns Some("a")
    /// For ArraySubscript(Identifier("a"), _): returns Some("a")
    pub(super) fn get_array_root_name_from_base(&self, base: &Expr) -> Option<String> {
        match base {
            Expr::Identifier(name, _) => Some(name.clone()),
            Expr::ArraySubscript(inner, _, _) => self.get_array_root_name_from_base(inner),
            _ => None,
        }
    }

    /// Get the root array name from a full expression (including outer subscript).
    /// Handles reverse subscript by checking both base and index.
    pub(super) fn get_array_root_name(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Identifier(name, _) => Some(name.clone()),
            Expr::ArraySubscript(base, index, _) => {
                // Try base first (normal case: arr[i])
                if let Some(name) = self.get_array_root_name(base) {
                    return Some(name);
                }
                // Try index (reverse subscript: i[arr])
                self.get_array_root_name(index)
            }
            _ => None,
        }
    }

    /// Compute the element size for a pointer type specifier.
    pub(super) fn pointee_elem_size(&self, type_spec: &TypeSpecifier) -> usize {
        match type_spec {
            TypeSpecifier::Pointer(inner) => self.sizeof_type(inner),
            TypeSpecifier::Array(inner, _) => self.sizeof_type(inner),
            _ => 0,
        }
    }

    /// Check if an expression is an array name (identifier declared as an array).
    /// Used for normalizing reverse subscript: 3[arr] -> arr[3].
    pub(super) fn expr_is_array_name(&self, expr: &Expr) -> bool {
        if let Expr::Identifier(name, _) = expr {
            if let Some(info) = self.locals.get(name) {
                return info.is_array;
            }
            if let Some(ginfo) = self.globals.get(name) {
                return ginfo.is_array;
            }
        }
        false
    }
}
