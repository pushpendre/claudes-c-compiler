use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, StructField, StructLayout, CType};
use super::lowering::Lowerer;

impl Lowerer {
    /// Register a struct/union type definition from a TypeSpecifier, computing and
    /// caching its layout in self.struct_layouts.
    pub(super) fn register_struct_type(&mut self, ts: &TypeSpecifier) {
        match ts {
            TypeSpecifier::Struct(tag, Some(fields)) => {
                let layout = self.compute_struct_layout(fields, false);
                let key = self.struct_layout_key(tag, false);
                self.struct_layouts.insert(key, layout);
            }
            TypeSpecifier::Union(tag, Some(fields)) => {
                let layout = self.compute_struct_layout(fields, true);
                let key = self.struct_layout_key(tag, true);
                self.struct_layouts.insert(key, layout);
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

    /// Compute struct/union layout from AST field declarations.
    fn compute_struct_layout(&self, fields: &[StructFieldDecl], is_union: bool) -> StructLayout {
        let struct_fields: Vec<StructField> = fields.iter().map(|f| {
            StructField {
                name: f.name.clone().unwrap_or_default(),
                ty: self.type_spec_to_ctype(&f.type_spec),
                bit_width: None, // TODO: bitfields
            }
        }).collect();

        if is_union {
            StructLayout::for_union(&struct_fields)
        } else {
            StructLayout::for_struct(&struct_fields)
        }
    }

    /// Get the cached struct layout for a TypeSpecifier, if it's a struct/union type.
    pub(super) fn get_struct_layout_for_type(&self, ts: &TypeSpecifier) -> Option<StructLayout> {
        let ts = self.resolve_type_spec(ts);
        match ts {
            TypeSpecifier::Struct(tag, Some(fields)) => {
                // Inline struct definition: compute layout directly
                let struct_fields: Vec<StructField> = fields.iter().map(|f| {
                    StructField {
                        name: f.name.clone().unwrap_or_default(),
                        ty: self.type_spec_to_ctype(&f.type_spec),
                        bit_width: None,
                    }
                }).collect();
                Some(StructLayout::for_struct(&struct_fields))
            }
            TypeSpecifier::Struct(Some(tag), None) => {
                // Reference to previously defined struct
                let key = format!("struct.{}", tag);
                self.struct_layouts.get(&key).cloned()
            }
            TypeSpecifier::Union(tag, Some(fields)) => {
                let struct_fields: Vec<StructField> = fields.iter().map(|f| {
                    StructField {
                        name: f.name.clone().unwrap_or_default(),
                        ty: self.type_spec_to_ctype(&f.type_spec),
                        bit_width: None,
                    }
                }).collect();
                Some(StructLayout::for_union(&struct_fields))
            }
            TypeSpecifier::Union(Some(tag), None) => {
                let key = format!("union.{}", tag);
                self.struct_layouts.get(&key).cloned()
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
                if let Some(info) = self.locals.get(name).cloned() {
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
            _ => {
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

    /// Resolve member access on a struct expression: returns (byte_offset, ir_type_of_field).
    pub(super) fn resolve_member_access(&self, base_expr: &Expr, field_name: &str) -> (usize, IrType) {
        // Try to find the struct layout from the base expression
        if let Some(layout) = self.get_layout_for_expr(base_expr) {
            if let Some((offset, ctype)) = layout.field_offset(field_name) {
                return (offset, IrType::from_ctype(ctype));
            }
        }
        // Fallback: assume 4-byte aligned int fields
        // TODO: improve fallback by tracking more type info
        (0, IrType::I32)
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

    /// Try to determine the struct layout that an expression (a pointer) points to.
    fn get_pointed_struct_layout(&self, expr: &Expr) -> Option<StructLayout> {
        match expr {
            Expr::Identifier(name, _) => {
                // Check if this variable has struct layout info (pointer to struct)
                if let Some(info) = self.locals.get(name) {
                    return info.struct_layout.clone();
                }
                if let Some(ginfo) = self.globals.get(name) {
                    return ginfo.struct_layout.clone();
                }
                None
            }
            Expr::MemberAccess(inner_base, inner_field, _) => {
                // expr is something like s.p where p is a pointer to struct.
                // Get the type of the field `p` from the struct layout of `s`.
                if let Some(outer_layout) = self.get_layout_for_expr(inner_base) {
                    if let Some((_offset, ctype)) = outer_layout.field_offset(inner_field) {
                        return self.resolve_struct_from_pointer_ctype(ctype);
                    }
                }
                None
            }
            Expr::PointerMemberAccess(inner_base, inner_field, _) => {
                // expr is something like p->q where q is also a pointer to struct
                if let Some(inner_layout) = self.get_pointed_struct_layout(inner_base) {
                    if let Some((_offset, ctype)) = inner_layout.field_offset(inner_field) {
                        return self.resolve_struct_from_pointer_ctype(ctype);
                    }
                }
                None
            }
            Expr::UnaryOp(UnaryOp::PreInc | UnaryOp::PreDec, inner, _) => {
                self.get_pointed_struct_layout(inner)
            }
            _ => None,
        }
    }

    /// Try to get the struct layout for an expression.
    fn get_layout_for_expr(&self, expr: &Expr) -> Option<StructLayout> {
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(info) = self.locals.get(name) {
                    return info.struct_layout.clone();
                }
                if let Some(ginfo) = self.globals.get(name) {
                    return ginfo.struct_layout.clone();
                }
                None
            }
            Expr::MemberAccess(inner_base, inner_field, _) => {
                // For nested member access, find the layout of the inner field
                if let Some(outer_layout) = self.get_layout_for_expr(inner_base) {
                    if let Some((_offset, ctype)) = outer_layout.field_offset(inner_field) {
                        // If the inner field is itself a struct, get its layout
                        if let CType::Struct(st) = ctype {
                            return Some(StructLayout::for_struct(&st.fields));
                        }
                        if let CType::Union(st) = ctype {
                            return Some(StructLayout::for_union(&st.fields));
                        }
                    }
                }
                None
            }
            Expr::PointerMemberAccess(inner_base, inner_field, _) => {
                // For p->field where field is an embedded struct, get its layout
                if let Some(pointed_layout) = self.get_pointed_struct_layout(inner_base) {
                    if let Some((_offset, ctype)) = pointed_layout.field_offset(inner_field) {
                        if let CType::Struct(st) = ctype {
                            return Some(StructLayout::for_struct(&st.fields));
                        }
                        if let CType::Union(st) = ctype {
                            return Some(StructLayout::for_union(&st.fields));
                        }
                    }
                }
                None
            }
            Expr::ArraySubscript(base, _, _) => {
                // array[i] where array is of struct type
                self.get_layout_for_expr(base)
            }
            Expr::CompoundLiteral(type_spec, _, _) => {
                self.get_struct_layout_for_type(type_spec)
            }
            Expr::Cast(type_spec, inner, _) => {
                // Cast to struct type, or cast wrapping a compound literal
                self.get_struct_layout_for_type(type_spec)
                    .or_else(|| self.get_layout_for_expr(inner))
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

    /// Given a CType that should be a Pointer to a struct, resolve the struct layout.
    /// Handles self-referential structs by looking up the cache when fields are empty.
    fn resolve_struct_from_pointer_ctype(&self, ctype: &CType) -> Option<StructLayout> {
        if let CType::Pointer(inner) = ctype {
            match inner.as_ref() {
                CType::Struct(st) => {
                    if st.fields.is_empty() {
                        // Empty fields: likely a forward/self-referential struct.
                        // Look up by tag name from the cache.
                        if let Some(ref tag) = st.name {
                            let key = format!("struct.{}", tag);
                            return self.struct_layouts.get(&key).cloned();
                        }
                    }
                    return Some(StructLayout::for_struct(&st.fields));
                }
                CType::Union(st) => {
                    if st.fields.is_empty() {
                        if let Some(ref tag) = st.name {
                            let key = format!("union.{}", tag);
                            return self.struct_layouts.get(&key).cloned();
                        }
                    }
                    return Some(StructLayout::for_union(&st.fields));
                }
                _ => {}
            }
        }
        None
    }
}
