use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, StructLayout, CType};
use super::lowering::Lowerer;

impl Lowerer {
    /// Register a struct/union type definition from a TypeSpecifier, computing and
    /// caching its layout in self.struct_layouts. Also recursively registers any
    /// nested struct/union types defined in the fields.
    pub(super) fn register_struct_type(&mut self, ts: &TypeSpecifier) {
        match ts {
            TypeSpecifier::Struct(tag, Some(fields), is_packed, pragma_pack, _) => {
                // Recursively register nested struct/union types in fields
                self.register_nested_struct_types(fields);
                let max_field_align = if *is_packed { Some(1) } else { *pragma_pack };
                let layout = self.compute_struct_union_layout_packed(fields, false, max_field_align);
                let key = self.struct_layout_key(tag, false);
                self.struct_layouts.insert(key, layout);
            }
            TypeSpecifier::Union(tag, Some(fields), is_packed, pragma_pack, _) => {
                // Recursively register nested struct/union types in fields
                self.register_nested_struct_types(fields);
                let max_field_align = if *is_packed { Some(1) } else { *pragma_pack };
                let layout = self.compute_struct_union_layout_packed(fields, true, max_field_align);
                let key = self.struct_layout_key(tag, true);
                self.struct_layouts.insert(key, layout);
            }
            _ => {}
        }
    }

    /// Recursively register any struct/union types defined inline in field declarations.
    /// This handles cases like:
    ///   struct Outer { struct Inner { int x; } field; };
    /// where `struct Inner` needs to be registered so it can be referenced later.
    fn register_nested_struct_types(&mut self, fields: &[StructFieldDecl]) {
        for field in fields {
            self.register_nested_in_type_spec(&field.type_spec);
        }
    }

    /// Walk a TypeSpecifier and register any struct/union definitions found within it.
    fn register_nested_in_type_spec(&mut self, ts: &TypeSpecifier) {
        match ts {
            TypeSpecifier::Struct(tag, Some(_fields), _, _, _) if tag.is_some() => {
                // This is a named struct definition inside a field - register it
                self.register_struct_type(ts);
            }
            TypeSpecifier::Union(tag, Some(_fields), _, _, _) if tag.is_some() => {
                // This is a named union definition inside a field - register it
                self.register_struct_type(ts);
            }
            TypeSpecifier::Pointer(inner) => {
                // Walk through pointer types to find nested struct defs
                self.register_nested_in_type_spec(inner);
            }
            TypeSpecifier::Array(inner, _) => {
                self.register_nested_in_type_spec(inner);
            }
            _ => {}
        }
    }

    /// Compute a layout key for a struct/union.
    fn struct_layout_key(&mut self, tag: &Option<String>, is_union: bool) -> String {
        let prefix = if is_union { "union." } else { "struct." };
        if let Some(name) = tag {
            format!("{}{}", prefix, name)
        } else {
            let id = self.next_anon_struct;
            self.next_anon_struct += 1;
            format!("{}__anon_{}", prefix, id)
        }
    }

    /// Get the cached struct layout for a TypeSpecifier, if it's a struct/union type.
    /// Prefers cached layout from struct_layouts when a tag name is available.
    pub(super) fn get_struct_layout_for_type(&self, ts: &TypeSpecifier) -> Option<StructLayout> {
        let ts = self.resolve_type_spec(ts);
        match ts {
            TypeSpecifier::Struct(tag, Some(fields), is_packed, pragma_pack, _) => {
                if let Some(tag) = tag {
                    if let Some(layout) = self.struct_layouts.get(&format!("struct.{}", tag)) {
                        return Some(layout.clone());
                    }
                }
                let max_field_align = if *is_packed { Some(1) } else { *pragma_pack };
                Some(self.compute_struct_union_layout_packed(&fields, false, max_field_align))
            }
            TypeSpecifier::Struct(Some(tag), None, _, _, _) => {
                self.struct_layouts.get(&format!("struct.{}", tag)).cloned()
            }
            TypeSpecifier::Union(tag, Some(fields), is_packed, pragma_pack, _) => {
                if let Some(tag) = tag {
                    if let Some(layout) = self.struct_layouts.get(&format!("union.{}", tag)) {
                        return Some(layout.clone());
                    }
                }
                let max_field_align = if *is_packed { Some(1) } else { *pragma_pack };
                Some(self.compute_struct_union_layout_packed(&fields, true, max_field_align))
            }
            TypeSpecifier::Union(Some(tag), None, _, _, _) => {
                self.struct_layouts.get(&format!("union.{}", tag)).cloned()
            }
            // For typedef'd array types like `typedef S arr_t[4]`, peel the
            // Array wrapper(s) to find the inner struct/union element type.
            TypeSpecifier::Array(inner, _) => {
                self.get_struct_layout_for_type(inner)
            }
            _ => None,
        }
    }

    /// Get the base address of a struct variable (for member access).
    /// For struct locals, the alloca IS the struct base.
    /// For struct globals, we emit GlobalAddr.
    pub(super) fn get_struct_base_addr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Identifier(name, _) => {
                // Static local variables: resolve via mangled global name
                if let Some(mangled) = self.static_local_names.get(name).cloned() {
                    let addr = self.fresh_value();
                    self.emit(Instruction::GlobalAddr { dest: addr, name: mangled });
                    return addr;
                }
                if let Some(info) = self.locals.get(name).cloned() {
                    // Static locals: emit fresh GlobalAddr
                    if let Some(ref global_name) = info.static_global_name {
                        let addr = self.fresh_value();
                        self.emit(Instruction::GlobalAddr { dest: addr, name: global_name.clone() });
                        return addr;
                    }
                    if info.is_struct {
                        // The alloca is the struct base address
                        return info.alloca;
                    }
                    // It's a pointer to struct: load the pointer
                    let loaded = self.fresh_value();
                    self.emit(Instruction::Load { dest: loaded, ptr: info.alloca, ty: IrType::Ptr });
                    return loaded;
                }
                if self.globals.contains_key(name) {
                    let addr = self.fresh_value();
                    self.emit(Instruction::GlobalAddr { dest: addr, name: name.clone() });
                    return addr;
                }
                // Unknown - try to evaluate as expression
                let val = self.lower_expr(expr);
                match val {
                    Operand::Value(v) => v,
                    Operand::Const(_) => {
                        let tmp = self.fresh_value();
                        self.emit(Instruction::Copy { dest: tmp, src: val });
                        tmp
                    }
                }
            }
            Expr::Deref(inner, _) => {
                // (*ptr).field - evaluate ptr to get base address
                let val = self.lower_expr(inner);
                match val {
                    Operand::Value(v) => v,
                    Operand::Const(_) => {
                        let tmp = self.fresh_value();
                        self.emit(Instruction::Copy { dest: tmp, src: val });
                        tmp
                    }
                }
            }
            Expr::ArraySubscript(base, index, _) => {
                // array[i].field
                self.compute_array_element_addr(base, index)
            }
            Expr::MemberAccess(inner_base, inner_field, _) => {
                // Nested member access: s.inner.field
                let (inner_offset, _) = self.resolve_member_access(inner_base, inner_field);
                let inner_base_addr = self.get_struct_base_addr(inner_base);
                let inner_addr = self.fresh_value();
                self.emit(Instruction::GetElementPtr {
                    dest: inner_addr,
                    base: inner_base_addr,
                    offset: Operand::Const(IrConst::I64(inner_offset as i64)),
                    ty: IrType::Ptr,
                });
                inner_addr
            }
            Expr::PointerMemberAccess(inner_base, inner_field, _) => {
                // Nested access via pointer: p->inner.field
                // Load the pointer, compute offset of the inner field, return address
                let ptr_val = self.lower_expr(inner_base);
                let base_addr = match ptr_val {
                    Operand::Value(v) => v,
                    Operand::Const(_) => {
                        let tmp = self.fresh_value();
                        self.emit(Instruction::Copy { dest: tmp, src: ptr_val });
                        tmp
                    }
                };
                let (inner_offset, _) = self.resolve_pointer_member_access(inner_base, inner_field);
                let inner_addr = self.fresh_value();
                self.emit(Instruction::GetElementPtr {
                    dest: inner_addr,
                    base: base_addr,
                    offset: Operand::Const(IrConst::I64(inner_offset as i64)),
                    ty: IrType::Ptr,
                });
                inner_addr
            }
            Expr::FunctionCall(func_expr, _, _) => {
                // Function returning a struct: call the function, then store
                // the return value to a temporary alloca so we have an address.
                let struct_size = self.struct_value_size(expr).unwrap_or(8);

                // Check if this is an sret call (struct > 16 bytes with hidden pointer)
                // or a two-register return (9-16 bytes) - both return an alloca address
                let returns_address = if let Expr::Identifier(name, _) = func_expr.as_ref() {
                    // Detect function pointer variables: identifiers that are
                    // local/global variables rather than known function names
                    let is_fptr_var = (self.locals.contains_key(name) && !self.known_functions.contains(name))
                        || (!self.locals.contains_key(name) && self.globals.contains_key(name) && !self.known_functions.contains(name));
                    if is_fptr_var {
                        // Indirect call through variable: use struct size to determine ABI
                        struct_size > 8
                    } else {
                        self.func_meta.sret_functions.contains_key(name)
                            || self.func_meta.two_reg_return_functions.contains_key(name)
                    }
                } else {
                    // Indirect call through expression: determine from return type
                    struct_size > 8
                };

                if returns_address {
                    // For sret and two-register calls, lower_expr returns the alloca
                    // address directly (the struct data is already there)
                    let val = self.lower_expr(expr);
                    match val {
                        Operand::Value(v) => v,
                        Operand::Const(_) => {
                            let tmp = self.fresh_value();
                            self.emit(Instruction::Copy { dest: tmp, src: val });
                            tmp
                        }
                    }
                } else {
                    // Small struct (<= 8 bytes): return value in rax IS
                    // the packed struct data, not an address.
                    let val = self.lower_expr(expr);
                    let alloca = self.fresh_value();
                    let alloc_size = if struct_size > 0 { struct_size } else { 8 };
                    self.emit(Instruction::Alloca { dest: alloca, size: alloc_size, ty: IrType::I64 });
                    self.emit(Instruction::Store { val, ptr: alloca, ty: IrType::I64 });
                    alloca
                }
            }
            _ => {
                // For expressions that might produce packed struct data (e.g. ternary
                // with struct-returning function calls), detect and spill to an alloca.
                if self.expr_produces_packed_struct_data(expr) {
                    let struct_size = self.struct_value_size(expr).unwrap_or(8);
                    let val = self.lower_expr(expr);
                    let alloca = self.fresh_value();
                    let alloc_size = if struct_size > 0 { struct_size } else { 8 };
                    self.emit(Instruction::Alloca { dest: alloca, size: alloc_size, ty: IrType::I64 });
                    self.emit(Instruction::Store { val, ptr: alloca, ty: IrType::I64 });
                    alloca
                } else {
                    let val = self.lower_expr(expr);
                    match val {
                        Operand::Value(v) => v,
                        Operand::Const(_) => {
                            let tmp = self.fresh_value();
                            self.emit(Instruction::Copy { dest: tmp, src: val });
                            tmp
                        }
                    }
                }
            }
        }
    }

    /// Check if an expression produces packed struct data (non-address value)
    /// rather than a pointer to struct data. This happens for small structs
    /// (<= 8 bytes) returned from non-sret, non-two-reg function calls, either
    /// directly or through ternary/comma expressions.
    pub(super) fn expr_produces_packed_struct_data(&self, expr: &Expr) -> bool {
        match expr {
            Expr::FunctionCall(func_expr, _, _) => {
                let struct_size = self.struct_value_size(expr).unwrap_or(8);
                if struct_size > 8 {
                    // 9+ byte structs: sret or two-reg return, both produce addresses
                    return false;
                }
                // Small struct (<= 8 bytes): produces packed data unless somehow sret
                if let Expr::Identifier(name, _) = func_expr.as_ref() {
                    !self.func_meta.sret_functions.contains_key(name)
                        && !self.func_meta.two_reg_return_functions.contains_key(name)
                } else {
                    true
                }
            }
            Expr::Conditional(_, then_expr, else_expr, _) => {
                // If either branch produces packed struct data, the ternary does too
                self.expr_produces_packed_struct_data(then_expr)
                    || self.expr_produces_packed_struct_data(else_expr)
            }
            Expr::GnuConditional(cond, else_expr, _) => {
                self.expr_produces_packed_struct_data(cond)
                    || self.expr_produces_packed_struct_data(else_expr)
            }
            Expr::Comma(_, last, _) => {
                self.expr_produces_packed_struct_data(last)
            }
            _ => false,
        }
    }

    /// Resolve member access on a struct expression: returns (byte_offset, ir_type_of_field).
    pub(super) fn resolve_member_access(&self, base_expr: &Expr, field_name: &str) -> (usize, IrType) {
        // Try to find the struct layout from the base expression
        if let Some(layout) = self.get_layout_for_expr(base_expr) {
            if let Some((offset, ctype)) = layout.field_offset(field_name) {
                return (offset, IrType::from_ctype(ctype));
            }
        }
        // Fallback: assume 4-byte aligned int fields
        (0, IrType::I32)
    }

    /// Resolve member access with full bitfield info.
    /// Returns (byte_offset, ir_type, Option<(bit_offset, bit_width)>).
    pub(super) fn resolve_member_access_full(&self, base_expr: &Expr, field_name: &str) -> (usize, IrType, Option<(u32, u32)>) {
        if let Some(layout) = self.get_layout_for_expr(base_expr) {
            if let Some(fl) = layout.field_layout(field_name) {
                let ir_ty = IrType::from_ctype(&fl.ty);
                let bf = match (fl.bit_offset, fl.bit_width) {
                    (Some(bo), Some(bw)) => Some((bo, bw)),
                    _ => None,
                };
                return (fl.offset, ir_ty, bf);
            }
            // Fallback: search anonymous struct/union members recursively
            // (field_layout only does flat lookup; field_offset searches anonymous members)
            if let Some((offset, ctype)) = layout.field_offset(field_name) {
                return (offset, IrType::from_ctype(ctype), None);
            }
        }
        (0, IrType::I32, None)
    }

    /// Resolve pointer member access (p->field): returns (byte_offset, ir_type_of_field).
    /// For p->field, we need the struct layout that p points to.
    pub(super) fn resolve_pointer_member_access(&self, base_expr: &Expr, field_name: &str) -> (usize, IrType) {
        // Try to determine what struct layout p points to
        if let Some(layout) = self.get_pointed_struct_layout(base_expr) {
            if let Some((offset, ctype)) = layout.field_offset(field_name) {
                return (offset, IrType::from_ctype(ctype));
            }
        }
        // Fallback: assume first field at offset 0
        (0, IrType::I32)
    }

    /// Resolve pointer member access with full bitfield info.
    pub(super) fn resolve_pointer_member_access_full(&self, base_expr: &Expr, field_name: &str) -> (usize, IrType, Option<(u32, u32)>) {
        if let Some(layout) = self.get_pointed_struct_layout(base_expr) {
            if let Some(fl) = layout.field_layout(field_name) {
                let ir_ty = IrType::from_ctype(&fl.ty);
                let bf = match (fl.bit_offset, fl.bit_width) {
                    (Some(bo), Some(bw)) => Some((bo, bw)),
                    _ => None,
                };
                return (fl.offset, ir_ty, bf);
            }
            // Fallback: search anonymous struct/union members recursively
            if let Some((offset, ctype)) = layout.field_offset(field_name) {
                return (offset, IrType::from_ctype(ctype), None);
            }
        }
        (0, IrType::I32, None)
    }

    /// Try to determine the struct layout that an expression (a pointer) points to.
    fn get_pointed_struct_layout(&self, expr: &Expr) -> Option<StructLayout> {
        match expr {
            Expr::Identifier(name, _) => {
                // Check static locals first
                if let Some(mangled) = self.static_local_names.get(name) {
                    if let Some(ginfo) = self.globals.get(mangled) {
                        if ginfo.struct_layout.is_some() {
                            return ginfo.struct_layout.clone();
                        }
                    }
                }
                // Check if this variable has struct layout info (pointer to struct)
                if let Some(vi) = self.lookup_var_info(name) {
                    if let Some(ref layout) = vi.struct_layout {
                        return Some(layout.clone());
                    }
                    // Fallback: resolve from c_type if layout was not available at declaration time
                    // (forward-declared struct pointer case)
                    if let Some(ref ct) = vi.c_type {
                        if let Some(layout) = self.resolve_struct_from_pointer_ctype(ct) {
                            return Some(layout);
                        }
                    }
                }
                // Fallback: resolve struct layout from the variable's CType
                // (handles forward-declared struct pointers where layout was None at decl time)
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    if let CType::Pointer(pointee) = &ctype {
                        return self.struct_layout_from_ctype(pointee);
                    }
                }
                None
            }
            Expr::MemberAccess(inner_base, inner_field, _) => {
                // expr is something like s.p where p is a pointer to struct.
                // Get the type of the field `p` from the struct layout of `s`.
                if let Some(outer_layout) = self.get_layout_for_expr(inner_base) {
                    if let Some((_offset, ctype)) = outer_layout.field_offset(inner_field) {
                        // If the field is a pointer to struct, resolve directly
                        if let Some(layout) = self.resolve_struct_from_pointer_ctype(ctype) {
                            return Some(layout);
                        }
                        // If the field is an array of structs/unions, handle array decay
                        if let CType::Array(elem, _) = ctype {
                            return self.struct_layout_from_ctype(elem);
                        }
                    }
                }
                None
            }
            Expr::PointerMemberAccess(inner_base, inner_field, _) => {
                // expr is something like p->q where q is also a pointer to struct
                if let Some(inner_layout) = self.get_pointed_struct_layout(inner_base) {
                    if let Some((_offset, ctype)) = inner_layout.field_offset(inner_field) {
                        // If the field is a pointer to struct, resolve directly
                        if let Some(layout) = self.resolve_struct_from_pointer_ctype(ctype) {
                            return Some(layout);
                        }
                        // If the field is an array of structs/unions, the array decays to a
                        // pointer to the first element, so resolve the element struct layout
                        if let CType::Array(elem, _) = ctype {
                            return self.struct_layout_from_ctype(elem);
                        }
                    }
                }
                None
            }
            Expr::UnaryOp(UnaryOp::PreInc | UnaryOp::PreDec, inner, _) => {
                self.get_pointed_struct_layout(inner)
            }
            Expr::PostfixOp(_, inner, _) => {
                self.get_pointed_struct_layout(inner)
            }
            Expr::BinaryOp(op, lhs, rhs, _) if matches!(op, BinOp::Add | BinOp::Sub) => {
                // Pointer arithmetic: (p + i) or (p - i) preserves the pointed-to struct type
                if let Some(layout) = self.get_pointed_struct_layout(lhs) {
                    return Some(layout);
                }
                // Try rhs for commutative case: (i + p)
                if matches!(op, BinOp::Add) {
                    if let Some(layout) = self.get_pointed_struct_layout(rhs) {
                        return Some(layout);
                    }
                }
                None
            }
            Expr::Cast(type_spec, inner, _) => {
                // Cast to struct pointer type
                if let Some(layout) = self.get_struct_layout_for_pointer_type(type_spec) {
                    return Some(layout);
                }
                // Try inner expression
                self.get_pointed_struct_layout(inner)
            }
            Expr::AddressOf(inner, _) => {
                // &expr - result is a pointer to expr's type
                self.get_layout_for_expr(inner)
            }
            Expr::Deref(inner, _) => {
                // *pp where pp is a pointer to pointer to struct
                if let Some(ctype) = self.get_expr_ctype(inner) {
                    if let CType::Pointer(inner_ct) = &ctype {
                        if let CType::Pointer(pointee) = inner_ct.as_ref() {
                            return self.struct_layout_from_ctype(pointee);
                        }
                    }
                }
                // Fallback: propagate through inner
                self.get_pointed_struct_layout(inner)
            }
            Expr::ArraySubscript(base, _, _) => {
                // pp[i] where pp is an array of struct pointers
                if let Some(ctype) = self.get_expr_ctype(base) {
                    match &ctype {
                        CType::Array(elem, _) | CType::Pointer(elem) => {
                            if let CType::Pointer(pointee) = elem.as_ref() {
                                return self.struct_layout_from_ctype(pointee);
                            }
                        }
                        _ => {}
                    }
                }
                // Fallback: try base directly
                self.get_pointed_struct_layout(base)
            }
            Expr::FunctionCall(func, _, _) => {
                // Function returning a struct pointer
                if let Expr::Identifier(name, _) = func.as_ref() {
                    if let Some(ctype) = self.func_meta.return_ctypes.get(name) {
                        if let CType::Pointer(pointee) = ctype {
                            return self.struct_layout_from_ctype(pointee);
                        }
                    }
                }
                None
            }
            Expr::Conditional(_, then_expr, _, _) => {
                self.get_pointed_struct_layout(then_expr)
            }
            Expr::GnuConditional(cond, _, _) => {
                self.get_pointed_struct_layout(cond)
            }
            Expr::Comma(_, last, _) => {
                self.get_pointed_struct_layout(last)
            }
            Expr::Assign(_, rhs, _) | Expr::CompoundAssign(_, _, rhs, _) => {
                self.get_pointed_struct_layout(rhs)
            }
            _ => {
                // Generic fallback: try CType-based resolution
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    if let CType::Pointer(pointee) = &ctype {
                        return self.struct_layout_from_ctype(pointee);
                    }
                }
                None
            }
        }
    }

    /// Get struct layout from a CType (struct or union).
    /// Prefers cached layout from struct_layouts when available.
    fn struct_layout_from_ctype(&self, ctype: &CType) -> Option<StructLayout> {
        match ctype {
            CType::Struct(st) => {
                // Try cache first (by tag name)
                if let Some(ref tag) = st.name {
                    let key = format!("struct.{}", tag);
                    if let Some(layout) = self.struct_layouts.get(&key) {
                        return Some(layout.clone());
                    }
                }
                if st.fields.is_empty() {
                    return None;
                }
                Some(StructLayout::for_struct(&st.fields))
            }
            CType::Union(st) => {
                // Try cache first (by tag name)
                if let Some(ref tag) = st.name {
                    let key = format!("union.{}", tag);
                    if let Some(layout) = self.struct_layouts.get(&key) {
                        return Some(layout.clone());
                    }
                }
                if st.fields.is_empty() {
                    return None;
                }
                Some(StructLayout::for_union(&st.fields))
            }
            _ => None,
        }
    }

    /// Get struct layout from a type specifier that should be a pointer to struct
    fn get_struct_layout_for_pointer_type(&self, type_spec: &TypeSpecifier) -> Option<StructLayout> {
        let resolved = self.resolve_type_spec(type_spec);
        let ctype = self.type_spec_to_ctype(resolved);
        if let CType::Pointer(pointee) = &ctype {
            return self.struct_layout_from_ctype(pointee);
        }
        None
    }

    /// Try to get the struct layout for an expression.
    fn get_layout_for_expr(&self, expr: &Expr) -> Option<StructLayout> {
        match expr {
            Expr::Identifier(name, _) => {
                // Check static locals first (resolved via mangled name)
                if let Some(mangled) = self.static_local_names.get(name) {
                    if let Some(ginfo) = self.globals.get(mangled) {
                        return ginfo.struct_layout.clone();
                    }
                }
                if let Some(vi) = self.lookup_var_info(name) {
                    return vi.struct_layout.clone();
                }
                None
            }
            Expr::MemberAccess(inner_base, inner_field, _) => {
                // For nested member access, find the layout of the inner field
                if let Some(outer_layout) = self.get_layout_for_expr(inner_base) {
                    if let Some((_offset, ctype)) = outer_layout.field_offset(inner_field) {
                        if let Some(layout) = self.struct_layout_from_ctype(ctype) {
                            return Some(layout);
                        }
                        // If the field is an array of structs, return the element struct layout
                        if let CType::Array(elem, _) = ctype {
                            if let Some(layout) = self.struct_layout_from_ctype(elem) {
                                return Some(layout);
                            }
                        }
                    }
                }
                None
            }
            Expr::PointerMemberAccess(inner_base, inner_field, _) => {
                // For p->field where field is an embedded struct, get its layout
                if let Some(pointed_layout) = self.get_pointed_struct_layout(inner_base) {
                    if let Some((_offset, ctype)) = pointed_layout.field_offset(inner_field) {
                        if let Some(layout) = self.struct_layout_from_ctype(ctype) {
                            return Some(layout);
                        }
                        // If the field is an array of structs, return the element struct layout
                        if let CType::Array(elem, _) = ctype {
                            if let Some(layout) = self.struct_layout_from_ctype(elem) {
                                return Some(layout);
                            }
                        }
                    }
                }
                None
            }
            Expr::ArraySubscript(base, _, _) => {
                // array[i] where array is of struct type
                // First try getting the layout directly from base
                if let Some(layout) = self.get_layout_for_expr(base) {
                    return Some(layout);
                }
                // If base is a member access yielding an array of structs,
                // use CType to resolve the element type
                if let Some(base_ctype) = self.get_expr_ctype(base) {
                    match &base_ctype {
                        CType::Array(elem, _) => {
                            if let Some(layout) = self.struct_layout_from_ctype(elem) {
                                return Some(layout);
                            }
                        }
                        CType::Pointer(pointee) => {
                            if let Some(layout) = self.struct_layout_from_ctype(pointee) {
                                return Some(layout);
                            }
                        }
                        _ => {}
                    }
                }
                // Also try: base is a pointer to struct/union (for ptr[i] patterns)
                if let Some(layout) = self.get_pointed_struct_layout(base) {
                    return Some(layout);
                }
                None
            }
            Expr::CompoundLiteral(type_spec, _, _) => {
                // Compound literal: (struct tag){...} - get layout from the type specifier
                self.get_struct_layout_for_type(type_spec)
            }
            Expr::Deref(inner, _) => {
                // (*ptr) where ptr points to a struct
                self.get_pointed_struct_layout(inner)
            }
            Expr::Cast(type_spec, inner, _) => {
                // Cast to struct type, or cast wrapping a compound literal
                self.get_struct_layout_for_type(type_spec)
                    .or_else(|| self.get_layout_for_expr(inner))
            }
            Expr::FunctionCall(func, _, _) => {
                // Function returning a struct: look up the return CType
                if let Expr::Identifier(name, _) = func.as_ref() {
                    if let Some(ctype) = self.func_meta.return_ctypes.get(name) {
                        if let Some(layout) = self.struct_layout_from_ctype(ctype) {
                            return Some(layout);
                        }
                    }
                }
                None
            }
            Expr::Assign(lhs, _, _) | Expr::CompoundAssign(_, lhs, _, _) => {
                // Assignment returns the type of the LHS
                self.get_layout_for_expr(lhs)
            }
            Expr::Conditional(_, then_expr, _, _) => {
                // Ternary returns the type of either branch
                self.get_layout_for_expr(then_expr)
            }
            Expr::GnuConditional(cond, _, _) => {
                self.get_layout_for_expr(cond)
            }
            Expr::Comma(_, last, _) => {
                self.get_layout_for_expr(last)
            }
            _ => None,
        }
    }

    /// Resolve member access to get the CType of the field (not just the IrType).
    /// This is needed to check if a field is an array type (for array decay behavior).
    pub(super) fn resolve_member_field_ctype(&self, base_expr: &Expr, field_name: &str) -> Option<CType> {
        if let Some(layout) = self.get_layout_for_expr(base_expr) {
            if let Some((_offset, ctype)) = layout.field_offset(field_name) {
                return Some(ctype.clone());
            }
        }
        None
    }

    /// Resolve pointer member access to get the CType of the field.
    pub(super) fn resolve_pointer_member_field_ctype(&self, base_expr: &Expr, field_name: &str) -> Option<CType> {
        if let Some(layout) = self.get_pointed_struct_layout(base_expr) {
            if let Some((_offset, ctype)) = layout.field_offset(field_name) {
                return Some(ctype.clone());
            }
        }
        None
    }

    /// Resolve member access and return full info including CType.
    /// Returns (byte_offset, ir_type, bitfield_info, field_ctype).
    pub(super) fn resolve_member_access_with_ctype(&self, base_expr: &Expr, field_name: &str) -> (usize, IrType, Option<(u32, u32)>, Option<CType>) {
        if let Some(layout) = self.get_layout_for_expr(base_expr) {
            if let Some(fl) = layout.field_layout(field_name) {
                let ir_ty = IrType::from_ctype(&fl.ty);
                let bf = match (fl.bit_offset, fl.bit_width) {
                    (Some(bo), Some(bw)) => Some((bo, bw)),
                    _ => None,
                };
                return (fl.offset, ir_ty, bf, Some(fl.ty.clone()));
            }
            if let Some((offset, ctype)) = layout.field_offset(field_name) {
                return (offset, IrType::from_ctype(ctype), None, Some(ctype.clone()));
            }
        }
        (0, IrType::I32, None, None)
    }

    /// Resolve pointer member access and return full info including CType.
    /// Returns (byte_offset, ir_type, bitfield_info, field_ctype).
    pub(super) fn resolve_pointer_member_access_with_ctype(&self, base_expr: &Expr, field_name: &str) -> (usize, IrType, Option<(u32, u32)>, Option<CType>) {
        if let Some(layout) = self.get_pointed_struct_layout(base_expr) {
            if let Some(fl) = layout.field_layout(field_name) {
                let ir_ty = IrType::from_ctype(&fl.ty);
                let bf = match (fl.bit_offset, fl.bit_width) {
                    (Some(bo), Some(bw)) => Some((bo, bw)),
                    _ => None,
                };
                return (fl.offset, ir_ty, bf, Some(fl.ty.clone()));
            }
            if let Some((offset, ctype)) = layout.field_offset(field_name) {
                return (offset, IrType::from_ctype(ctype), None, Some(ctype.clone()));
            }
        }
        (0, IrType::I32, None, None)
    }

    /// Given a CType that should be a Pointer to a struct, resolve the struct layout.
    /// Handles self-referential structs by looking up the cache when fields are empty.
    fn resolve_struct_from_pointer_ctype(&self, ctype: &CType) -> Option<StructLayout> {
        if let CType::Pointer(inner) = ctype {
            return self.struct_layout_from_ctype(inner);
        }
        None
    }
}
