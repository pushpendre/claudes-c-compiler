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
                // Check locals first so inner-scope locals shadow outer static locals
                if let Some(info) = self.locals.get(name).cloned() {
                    // Static locals: emit fresh GlobalAddr at point of use
                    if let Some(ref global_name) = info.static_global_name {
                        let addr = self.fresh_value();
                        self.emit(Instruction::GlobalAddr { dest: addr, name: global_name.clone() });
                        return Some(LValue::Address(addr));
                    }
                    return Some(LValue::Variable(info.alloca));
                }
                // Static local variables: resolve through mangled name
                if let Some(mangled) = self.static_local_names.get(name).cloned() {
                    let addr = self.fresh_value();
                    self.emit(Instruction::GlobalAddr { dest: addr, name: mangled });
                    return Some(LValue::Address(addr));
                }
                if self.globals.contains_key(name) {
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
            Expr::UnaryOp(UnaryOp::RealPart, inner, _) => {
                // __real__ z as lvalue -> address of real part (offset 0 from complex base)
                if let Some(lv) = self.lower_lvalue(inner) {
                    let addr = self.lvalue_addr(&lv);
                    Some(LValue::Address(addr))
                } else {
                    None
                }
            }
            Expr::UnaryOp(UnaryOp::ImagPart, inner, _) => {
                // __imag__ z as lvalue -> address of imag part (offset = sizeof(component))
                if let Some(lv) = self.lower_lvalue(inner) {
                    let addr = self.lvalue_addr(&lv);
                    let inner_ct = self.expr_ctype(inner);
                    let comp_size = match inner_ct {
                        CType::ComplexFloat => 4i64,
                        CType::ComplexLongDouble => 16i64,
                        _ => 8i64, // ComplexDouble
                    };
                    let imag_addr = self.fresh_value();
                    self.emit(Instruction::GetElementPtr {
                        dest: imag_addr,
                        base: addr,
                        offset: Operand::Const(IrConst::I64(comp_size)),
                        ty: IrType::I8,
                    });
                    Some(LValue::Address(imag_addr))
                } else {
                    None
                }
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

        // Check for VLA runtime stride first
        let vla_stride = self.get_vla_stride_for_subscript(actual_base);

        // Determine the element size based on subscript depth for multi-dim arrays
        let elem_size = self.get_array_elem_size_for_subscript(actual_base);

        // Get the base address - for nested subscripts (a[i][j]), don't load
        let base_addr = self.get_array_base_addr(actual_base);

        // Compute offset = index * stride (runtime VLA stride or compile-time elem_size)
        let offset = if let Some(stride_val) = vla_stride {
            // Use runtime VLA stride
            let mul = self.emit_binop_val(IrBinOp::Mul, index_val, Operand::Value(stride_val), IrType::I64);
            Operand::Value(mul)
        } else if elem_size == 1 {
            index_val
        } else {
            let mul = self.emit_binop_val(IrBinOp::Mul, index_val, Operand::Const(IrConst::I64(elem_size as i64)), IrType::I64);
            Operand::Value(mul)
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

    /// Get the VLA runtime stride value for a subscript expression, if available.
    /// For `m[i]` where m has VLA strides, returns the runtime stride at depth 0.
    /// For `m[i][j]` where the outer subscript `m[i]` already used VLA, returns stride at depth 1.
    fn get_vla_stride_for_subscript(&self, base: &Expr) -> Option<Value> {
        let root_name = self.get_array_root_name_from_base(base)?;
        let depth = self.count_subscript_depth(base);

        if let Some(info) = self.locals.get(&root_name) {
            if !info.vla_strides.is_empty() && depth < info.vla_strides.len() {
                return info.vla_strides[depth];
            }
        }
        None
    }

    /// Get the base address for an array expression.
    /// For declared arrays, this is the alloca itself.
    /// For pointers, this is the loaded pointer value.
    /// For nested subscripts (a[i] where a is multi-dim), compute address without loading.
    pub(super) fn get_array_base_addr(&mut self, base: &Expr) -> Operand {
        match base {
            Expr::Identifier(name, _) => {
                // Check locals first so inner-scope locals shadow statics
                if let Some(info) = self.locals.get(name).cloned() {
                    // Static locals: emit fresh GlobalAddr at point of use
                    if let Some(ref global_name) = info.static_global_name {
                        let addr = self.fresh_value();
                        self.emit(Instruction::GlobalAddr { dest: addr, name: global_name.clone() });
                        if info.is_array {
                            return Operand::Value(addr);
                        } else {
                            let loaded = self.fresh_value();
                            self.emit(Instruction::Load { dest: loaded, ptr: addr, ty: IrType::Ptr });
                            return Operand::Value(loaded);
                        }
                    }
                    if info.is_array {
                        return Operand::Value(info.alloca);
                    } else {
                        let loaded = self.fresh_value();
                        self.emit(Instruction::Load { dest: loaded, ptr: info.alloca, ty: IrType::Ptr });
                        return Operand::Value(loaded);
                    }
                }
                // Static locals: resolve via mangled name from globals
                if let Some(mangled) = self.static_local_names.get(name).cloned() {
                    if let Some(ginfo) = self.globals.get(&mangled).cloned() {
                        let addr = self.fresh_value();
                        self.emit(Instruction::GlobalAddr { dest: addr, name: mangled });
                        if ginfo.is_array {
                            return Operand::Value(addr);
                        } else {
                            let loaded = self.fresh_value();
                            self.emit(Instruction::Load { dest: loaded, ptr: addr, ty: IrType::Ptr });
                            return Operand::Value(loaded);
                        }
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
                let field_is_array = self.resolve_field_ctype(base_expr, field_name, false)
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
                let field_is_array = self.resolve_field_ctype(base_expr, field_name, true)
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
            Expr::Deref(inner, _) => {
                // Handle *arr where arr is a multi-dimensional array.
                // e.g., (*x)[i] where x is int[2][3]: *x yields int[3] (sub-array),
                // which decays to a pointer = the base address of x.
                let deref_yields_array = self.get_expr_ctype(base)
                    .map(|ct| matches!(ct, CType::Array(_, _)))
                    .unwrap_or(false);
                if deref_yields_array {
                    // The dereference yields a sub-array, so the address is just
                    // the value of the inner expression (the array base address).
                    return self.lower_expr(inner);
                }
                // Otherwise, it's a normal pointer dereference - evaluate to get address
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
            if let Some(vi) = self.lookup_var_info(&name) {
                if !vi.array_dim_strides.is_empty() {
                    // depth 0 means base is the array name, so use strides[0]
                    // depth 1 means base is a[i], so use strides[1]
                    if depth < vi.array_dim_strides.len() {
                        return vi.array_dim_strides[depth];
                    }
                    // depth exceeds array dimensions: the element type might be a
                    // pointer (e.g., char *arr[] at depth 1). Use c_type to resolve
                    // the correct pointee element size rather than the array stride.
                    if let Some(ref ctype) = vi.c_type {
                        if let Some(sz) = self.peel_pointer_elem_size(ctype, depth) {
                            return sz;
                        }
                    }
                    return *vi.array_dim_strides.last().unwrap_or(&vi.elem_size.max(1));
                }
                // For pointer-to-pointer types (e.g., char **argv), when depth > 0,
                // we need to peel off pointer levels to get the correct element size.
                // argv[i] strides by sizeof(char*) = 8, but argv[i][j] should stride
                // by sizeof(char) = 1. Use c_type to resolve the correct pointee size.
                if depth > 0 {
                    if let Some(ref ctype) = vi.c_type {
                        if let Some(sz) = self.peel_pointer_elem_size(ctype, depth) {
                            return sz;
                        }
                    }
                }
                if vi.elem_size > 0 {
                    return vi.elem_size;
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
        let (base_expr, field_name, is_ptr) = match base {
            Expr::MemberAccess(base_expr, field_name, _) => (base_expr, field_name, false),
            Expr::PointerMemberAccess(base_expr, field_name, _) => (base_expr, field_name, true),
            _ => return None,
        };
        if let Some(ctype) = self.resolve_field_ctype(base_expr, field_name, is_ptr) {
            match &ctype {
                CType::Array(elem_ty, _) => return Some(elem_ty.size()),
                CType::Pointer(pointee_ty) => return Some(pointee_ty.size()),
                _ => {}
            }
        }
        None
    }

    /// Get the element size for array subscript (legacy, for 1D arrays).
    pub(super) fn get_array_elem_size(&self, base: &Expr) -> usize {
        // String literals have char elements (size 1)
        if matches!(base, Expr::StringLiteral(_, _)) {
            return 1;
        }
        if let Expr::Identifier(name, _) = base {
            if let Some(vi) = self.lookup_var_info(name) {
                if vi.elem_size > 0 {
                    return vi.elem_size;
                }
                if let Some(pt) = vi.pointee_type {
                    return pt.size();
                }
            }
        }
        // For non-identifier expressions (e.g., pointer arithmetic like arr+1, casts, etc.),
        // try to resolve the element size via CType.
        if let Some(ctype) = self.get_expr_ctype(base) {
            match &ctype {
                CType::Pointer(pointee) => {
                    let sz = self.resolve_ctype_size(pointee);
                    if sz > 0 { return sz; }
                }
                CType::Array(elem, _) => {
                    let sz = self.resolve_ctype_size(elem);
                    if sz > 0 { return sz; }
                }
                _ => {}
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
                if let Some(vi) = self.lookup_var_info(&name) {
                    if vi.array_dim_strides.len() > 1 {
                        return depth < vi.array_dim_strides.len();
                    }
                }
            }

            // Fallback: check CType of the subscript expression itself.
            // This handles struct member multi-dim arrays (e.g., m.data[i] where data is int[4][4])
            // where get_array_root_name_from_base returns None.
            if let Some(ctype) = self.get_expr_ctype(expr) {
                return matches!(ctype, CType::Array(_, _));
            }
        }
        false
    }

    /// Count the subscript nesting depth from the base expression.
    /// For Identifier: depth = 0
    /// For ArraySubscript(Identifier, _): depth = 1
    /// For ArraySubscript(ArraySubscript(Identifier, _), _): depth = 2
    /// For Deref(Identifier): depth = 1 (deref peels one array dimension)
    pub(super) fn count_subscript_depth(&self, base: &Expr) -> usize {
        match base {
            Expr::ArraySubscript(inner, _, _) => 1 + self.count_subscript_depth(inner),
            Expr::Deref(inner, _) => {
                // Dereferencing a multi-dimensional array peels one dimension,
                // equivalent to one level of subscripting.
                // Recurse into inner to handle chained derefs like **x.
                1 + self.count_subscript_depth(inner)
            }
            _ => 0,
        }
    }

    /// Peel `depth` levels of pointer/array indirection from a CType,
    /// then return the element size of the resulting pointer or array type.
    /// For `char **` at depth 1: Pointer(Pointer(Char)) -> Pointer(Char) -> pointee is Char -> size 1.
    /// For `int **` at depth 1: Pointer(Pointer(Int)) -> Pointer(Int) -> pointee is Int -> size 4.
    /// For `char *arr[]` at depth 1: Array(Pointer(Char), _) -> Pointer(Char) -> pointee is Char -> size 1.
    fn peel_pointer_elem_size(&self, ctype: &CType, depth: usize) -> Option<usize> {
        let mut ty = ctype;
        for _ in 0..depth {
            match ty {
                CType::Pointer(inner) => ty = inner,
                CType::Array(inner, _) => ty = inner,
                _ => return None,
            }
        }
        // ty is now the type after peeling `depth` levels.
        // It should be a pointer or array; return the size of what it points to.
        match ty {
            CType::Pointer(pointee) => {
                let sz = self.resolve_ctype_size(pointee);
                if sz > 0 { Some(sz) } else { None }
            }
            CType::Array(elem, _) => {
                let sz = self.resolve_ctype_size(elem);
                if sz > 0 { Some(sz) } else { None }
            }
            _ => None,
        }
    }

    /// Get the root array name from the base of a subscript expression.
    /// For Identifier("a"): returns Some("a")
    /// For ArraySubscript(Identifier("a"), _): returns Some("a")
    /// For Deref(Identifier("a")): returns Some("a") (for *arr patterns on multi-dim arrays)
    pub(super) fn get_array_root_name_from_base(&self, base: &Expr) -> Option<String> {
        match base {
            Expr::Identifier(name, _) => Some(name.clone()),
            Expr::ArraySubscript(inner, _, _) => self.get_array_root_name_from_base(inner),
            Expr::Deref(inner, _) => self.get_array_root_name_from_base(inner),
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
    /// Resolves typedef names before pattern matching.
    pub(super) fn pointee_elem_size(&self, type_spec: &TypeSpecifier) -> usize {
        let resolved = self.resolve_type_spec(type_spec);
        match &resolved {
            TypeSpecifier::Pointer(inner) => self.sizeof_type(inner),
            TypeSpecifier::Array(inner, _) => self.sizeof_type(inner),
            _ => 0,
        }
    }

    /// Check if an expression is an array name (identifier declared as an array).
    /// Used for normalizing reverse subscript: 3[arr] -> arr[3].
    pub(super) fn expr_is_array_name(&self, expr: &Expr) -> bool {
        if let Expr::Identifier(name, _) = expr {
            if let Some(vi) = self.lookup_var_info(name) {
                return vi.is_array;
            }
        }
        false
    }
}
