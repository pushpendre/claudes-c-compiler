//! Global initialization subsystem for the IR lowerer.
//!
//! This module handles lowering of global variable initializers from AST
//! `Initializer` nodes to IR `GlobalInit` values. It covers struct/union
//! initializers (including nested, designated, bitfield, and flexible array
//! members), array initializers (multi-dimensional, flat, and pointer arrays),
//! compound literal globals, and scalar initializers.

use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, StructLayout, CType};
use super::lowering::Lowerer;

/// Append `count` zero bytes (as `GlobalInit::Scalar(IrConst::I8(0))`) to `elements`.
/// Used throughout global initialization to emit padding and zero-fill.
fn push_zero_bytes(elements: &mut Vec<GlobalInit>, count: usize) {
    for _ in 0..count {
        elements.push(GlobalInit::Scalar(IrConst::I8(0)));
    }
}

/// Result of filling an array/FAM field in fill_struct_global_bytes,
/// indicating how to advance the item index.
struct ArrayFillResult {
    new_item_idx: usize,
    /// If true, caller should `continue` (skip the default field_idx update).
    skip_update: bool,
}

impl Lowerer {
    /// Lower a global initializer to a GlobalInit value.
    pub(super) fn lower_global_init(
        &mut self,
        init: &Initializer,
        _type_spec: &TypeSpecifier,
        base_ty: IrType,
        is_array: bool,
        elem_size: usize,
        total_size: usize,
        struct_layout: &Option<StructLayout>,
        array_dim_strides: &[usize],
    ) -> GlobalInit {
        // Check if the target type is long double (need to emit as x87 80-bit)
        let is_long_double_target = self.is_type_spec_long_double(_type_spec);

        match init {
            Initializer::Expr(expr) => {
                // Try to evaluate as a constant
                if let Some(val) = self.eval_const_expr(expr) {
                    // Convert integer constants to float if target type is float/double
                    // Use the expression's type for signedness (e.g., unsigned long long -> float)
                    let src_ty = self.get_expr_type(expr);
                    let val = self.coerce_const_to_type_with_src(val, base_ty, src_ty);
                    // If target is long double, promote F64 to LongDouble for proper encoding
                    let val = if is_long_double_target {
                        match val {
                            IrConst::F64(v) => IrConst::LongDouble(v),
                            IrConst::F32(v) => IrConst::LongDouble(v as f64),
                            IrConst::I64(v) => {
                                if src_ty.is_unsigned() {
                                    IrConst::LongDouble((v as u64) as f64)
                                } else {
                                    IrConst::LongDouble(v as f64)
                                }
                            }
                            IrConst::I32(v) => IrConst::LongDouble(v as f64),
                            other => other, // LongDouble already, or other type
                        }
                    } else {
                        val
                    };
                    return GlobalInit::Scalar(val);
                }
                // String literal initializer
                if let Expr::StringLiteral(s, _) = expr {
                    if is_array && (base_ty == IrType::I8 || base_ty == IrType::U8) {
                        // Char array: char s[] = "hello" -> inline the string bytes
                        return GlobalInit::String(s.clone());
                    } else {
                        // Pointer: const char *s = "hello" -> reference .rodata label
                        let label = self.intern_string_literal(s);
                        return GlobalInit::GlobalAddr(label);
                    }
                }
                // Handle &(compound_literal) at file scope: create anonymous global
                if let Expr::AddressOf(inner, _) = expr {
                    if let Expr::CompoundLiteral(ref cl_type_spec, ref cl_init, _) = inner.as_ref() {
                        return self.create_compound_literal_global(cl_type_spec, cl_init);
                    }
                }
                // Handle (compound_literal) used as struct initializer value
                if let Expr::CompoundLiteral(ref cl_type_spec, ref cl_init, _) = expr {
                    if base_ty == IrType::Ptr {
                        // Pointer: create anonymous global and return address
                        return self.create_compound_literal_global(cl_type_spec, cl_init);
                    }
                }
                // Try to evaluate as a global address expression (e.g., &x, func, arr, &arr[3], &s.field)
                if let Some(addr_init) = self.eval_global_addr_expr(expr) {
                    return addr_init;
                }
                // Complex global initializer: try to evaluate as {real, imag} pair
                {
                    let resolved = self.resolve_type_spec(_type_spec).clone();
                    if matches!(resolved, TypeSpecifier::ComplexFloat | TypeSpecifier::ComplexDouble | TypeSpecifier::ComplexLongDouble) {
                        if let Some(init) = self.eval_complex_global_init(expr, &resolved) {
                            return init;
                        }
                    }
                }
                // Can't evaluate - zero init as fallback
                GlobalInit::Zero
            }
            Initializer::List(items) => {
                // Handle brace-wrapped string literal for char arrays:
                // char c[] = {"hello"} or static char c[] = {"hello"}
                // But NOT for pointer arrays like char *arr[] = {"hello"} where
                // elem_size > base_ty.size() (elem is pointer, base is char).
                let is_char_not_ptr_array = elem_size <= base_ty.size().max(1);
                if is_array && is_char_not_ptr_array && (base_ty == IrType::I8 || base_ty == IrType::U8) {
                    if items.len() == 1 && items[0].designators.is_empty() {
                        if let Initializer::Expr(Expr::StringLiteral(s, _)) = &items[0].init {
                            return GlobalInit::String(s.clone());
                        }
                    }
                }

                // Array of complex types: emit {real, imag} pairs for each element.
                // Complex types (e.g., double _Complex) have base_ty=Ptr in IR but are
                // actually stored as {real, imag} pairs. We detect this case early and
                // use eval_complex_global_init to properly evaluate each element.
                let is_complex_element = is_array && matches!(
                    self.resolve_type_spec(_type_spec),
                    TypeSpecifier::ComplexFloat | TypeSpecifier::ComplexDouble | TypeSpecifier::ComplexLongDouble
                );
                if is_complex_element {
                    let resolved = self.resolve_type_spec(_type_spec).clone();
                    let num_elems = total_size / elem_size.max(1);
                    // Each complex element is stored as [real, imag] pair.
                    // Total scalar values = num_elems * 2.
                    let total_scalars = num_elems * 2;
                    let zero_pair: Vec<IrConst> = match &resolved {
                        TypeSpecifier::ComplexFloat => vec![IrConst::F32(0.0), IrConst::F32(0.0)],
                        TypeSpecifier::ComplexLongDouble => vec![IrConst::LongDouble(0.0), IrConst::LongDouble(0.0)],
                        _ => vec![IrConst::F64(0.0), IrConst::F64(0.0)],
                    };
                    let mut values: Vec<IrConst> = Vec::with_capacity(total_scalars);
                    for _ in 0..num_elems {
                        values.extend_from_slice(&zero_pair);
                    }
                    let mut current_idx = 0usize;
                    for item in items {
                        if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                            if let Some(idx) = self.eval_const_expr(idx_expr).and_then(|c| c.to_usize()) {
                                current_idx = idx;
                            }
                        }
                        if current_idx < num_elems {
                            let expr = match &item.init {
                                Initializer::Expr(e) => Some(e),
                                Initializer::List(sub_items) => {
                                    Self::unwrap_nested_init_expr(sub_items)
                                }
                            };
                            if let Some(expr) = expr {
                                if let Some((real, imag)) = self.eval_complex_const_public(expr) {
                                    let base_offset = current_idx * 2;
                                    match &resolved {
                                        TypeSpecifier::ComplexFloat => {
                                            values[base_offset] = IrConst::F32(real as f32);
                                            values[base_offset + 1] = IrConst::F32(imag as f32);
                                        }
                                        TypeSpecifier::ComplexLongDouble => {
                                            values[base_offset] = IrConst::LongDouble(real);
                                            values[base_offset + 1] = IrConst::LongDouble(imag);
                                        }
                                        _ => {
                                            values[base_offset] = IrConst::F64(real);
                                            values[base_offset + 1] = IrConst::F64(imag);
                                        }
                                    }
                                }
                            }
                        }
                        current_idx += 1;
                    }
                    return GlobalInit::Array(values);
                }

                if is_array && elem_size > 0 {
                    // For struct arrays, elem_size is the actual struct size (from sizeof_type),
                    // whereas base_ty.size() may return Ptr size (8). Use elem_size for structs.
                    // For long double arrays, elem_size=16 but base_ty=F64 (size=8), so use elem_size.
                    // For pointer arrays (e.g., char *arr[N]), elem_size=8 (pointer) but
                    // base_ty=I8 (char), so use elem_size when it's larger than base_ty.size().
                    let num_elems = if struct_layout.is_some() || is_long_double_target {
                        total_size / elem_size.max(1)
                    } else {
                        let base_type_size = base_ty.size().max(1);
                        if elem_size > base_type_size {
                            total_size / elem_size
                        } else {
                            total_size / base_type_size
                        }
                    };

                    // Array of structs: emit as byte array using struct layout.
                    // But skip byte-serialization if any struct field is a pointer type
                    // (pointers need .quad directives for address relocations).
                    let has_ptr_fields = struct_layout.as_ref().map_or(false, |layout| {
                        layout.fields.iter().any(|f| matches!(f.ty, CType::Pointer(_) | CType::Function(_)))
                    });
                    if let Some(ref layout) = struct_layout {
                        if has_ptr_fields {
                            // Use Compound approach for struct arrays with pointer fields
                            return self.lower_struct_array_with_ptrs(items, layout, num_elems);
                        }
                        let struct_size = layout.size;
                        // For multi-dimensional arrays, elem_size > struct_size (row stride).
                        // Use elem_size as the stride for the outer dimension.
                        let outer_stride = if elem_size > struct_size { elem_size } else { struct_size };
                        let mut bytes = vec![0u8; total_size];
                        let mut current_idx = 0usize;
                        let mut item_idx = 0usize;
                        while item_idx < items.len() {
                            let item = &items[item_idx];
                            // Check for index designator
                            if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                                if let Some(idx) = self.eval_const_expr(idx_expr).and_then(|c| c.to_usize()) {
                                    current_idx = idx;
                                }
                            }
                            if current_idx >= num_elems {
                                break;
                            }
                            let base_offset = current_idx * outer_stride;
                            // Check for field designator after index designator: [idx].field = val
                            let field_designator_name = item.designators.iter().find_map(|d| {
                                if let Designator::Field(ref name) = d {
                                    Some(name.clone())
                                } else {
                                    None
                                }
                            });
                            match &item.init {
                                Initializer::List(sub_items) => {
                                    // Check if this is a sub-array (multi-dim array of structs).
                                    // A sub-array means outer_stride > struct_size (multi-dim array)
                                    // AND all sub_items are Lists (each representing a full struct init).
                                    // For 1D arrays where struct fields include sub-struct braces,
                                    // outer_stride == struct_size, so we don't misidentify them.
                                    let is_subarray = outer_stride > struct_size
                                        && !sub_items.is_empty()
                                        && sub_items.iter().all(|si| matches!(&si.init, Initializer::List(_)));
                                    if is_subarray {
                                        // Multi-dimensional: sub_items are struct elements in a row.
                                        // base_offset is the start of this row in the byte buffer.
                                        for (si, sub_item) in sub_items.iter().enumerate() {
                                            let sub_offset = base_offset + si * struct_size;
                                            if sub_offset + struct_size > bytes.len() { break; }
                                            match &sub_item.init {
                                                Initializer::List(inner) => {
                                                    self.write_struct_init_to_bytes(&mut bytes, sub_offset, inner, layout);
                                                }
                                                Initializer::Expr(_) => {
                                                    self.fill_struct_global_bytes(&[sub_item.clone()], layout, &mut bytes, sub_offset);
                                                }
                                            }
                                        }
                                        item_idx += 1;
                                    } else {
                                        // Single struct initializer
                                        self.write_struct_init_to_bytes(&mut bytes, base_offset, sub_items, layout);
                                        item_idx += 1;
                                    }
                                }
                                Initializer::Expr(expr) => {
                                    if let Some(ref fname) = field_designator_name {
                                        // [idx].field = val: write to specific field
                                        if let Some(val) = self.eval_const_expr(expr) {
                                            if let Some(field) = layout.fields.iter().find(|f| &f.name == fname) {
                                                let field_ir_ty = IrType::from_ctype(&field.ty);
                                                self.write_const_to_bytes(&mut bytes, base_offset + field.offset, &val, field_ir_ty);
                                            }
                                        }
                                        item_idx += 1;
                                    } else {
                                        // Flat init: consume items for all fields of this struct
                                        let consumed = self.fill_struct_global_bytes(&items[item_idx..], layout, &mut bytes, base_offset);
                                        item_idx += consumed;
                                    }
                                }
                            }
                            // Only advance current_idx if no field designator (sequential init)
                            if field_designator_name.is_none() {
                                current_idx += 1;
                            }
                        }
                        let values: Vec<IrConst> = bytes.iter().map(|&b| IrConst::I8(b as i8)).collect();
                        return GlobalInit::Array(values);
                    }

                    // Check if any element is an address expression or string literal
                    // (for pointer arrays like char *arr[] or func_ptr arr[])
                    // For multi-dim char arrays (char a[2][4] = {"abc", "xyz"}), string literals
                    // should be inlined as bytes, not treated as address expressions.
                    // Distinguish from char *arr[] by checking array_dim_strides.len() > 1.
                    let is_multidim_char_array = matches!(base_ty, IrType::I8 | IrType::U8)
                        && array_dim_strides.len() > 1;
                    // Also check for pointer arrays (elem_size > base_ty.size())
                    // which indicates char *arr[] or similar pointer-to-char arrays.
                    let is_ptr_array = elem_size > base_ty.size().max(1);
                    let has_addr_exprs = items.iter().any(|item| {
                        // Check direct Expr items for address expressions (original check)
                        if let Initializer::Expr(expr) = &item.init {
                            if matches!(expr, Expr::StringLiteral(_, _)) {
                                return !is_multidim_char_array;
                            }
                            if matches!(expr, Expr::LabelAddr(_, _)) {
                                return true;
                            }
                            if self.eval_const_expr(expr).is_none() && self.eval_global_addr_expr(expr).is_some() {
                                return true;
                            }
                        }
                        // Also recurse into nested lists for string literals (double-brace init)
                        Self::init_contains_addr_expr(item, is_multidim_char_array)
                    }) || (is_ptr_array && !is_multidim_char_array && items.iter().any(|item| {
                        Self::init_contains_string_literal(item)
                    }));

                    if has_addr_exprs {
                        // Use Compound initializer for arrays containing address expressions.
                        // Support designated initializers: [idx] = val
                        let mut elements: Vec<GlobalInit> = (0..num_elems).map(|_| GlobalInit::Zero).collect();
                        let mut current_idx = 0usize;
                        for item in items {
                            if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                                if let Some(idx) = self.eval_const_expr(idx_expr).and_then(|c| c.to_usize()) {
                                    current_idx = idx;
                                }
                            }
                            if current_idx < num_elems {
                                let mut elem_parts = Vec::new();
                                self.collect_compound_init_element(&item.init, &mut elem_parts);
                                if let Some(elem) = elem_parts.into_iter().next() {
                                    elements[current_idx] = elem;
                                }
                            }
                            current_idx += 1;
                        }
                        return GlobalInit::Compound(elements);
                    }

                    let zero_val = self.typed_zero_const(base_ty, is_long_double_target);
                    let mut values = vec![zero_val; num_elems];
                    // For multi-dim arrays, flatten nested init lists
                    if array_dim_strides.len() > 1 {
                        let innermost_stride = array_dim_strides.last().copied().unwrap_or(1).max(1);
                        let total_scalar_elems = if is_long_double_target {
                            total_size / 16
                        } else {
                            total_size / innermost_stride
                        };
                        let mut values_flat = vec![self.typed_zero_const(base_ty, is_long_double_target); total_scalar_elems];
                        let mut flat = Vec::with_capacity(total_scalar_elems);
                        self.flatten_global_array_init(items, array_dim_strides, base_ty, &mut flat);
                        for (i, v) in flat.into_iter().enumerate() {
                            if i < total_scalar_elems {
                                values_flat[i] = Self::maybe_promote_long_double(v, is_long_double_target);
                            }
                        }
                        return GlobalInit::Array(values_flat);
                    } else {
                        // Support designated initializers: [idx] = val
                        let mut current_idx = 0usize;
                        for item in items {
                            if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                                if let Some(idx) = self.eval_const_expr(idx_expr).and_then(|c| c.to_usize()) {
                                    current_idx = idx;
                                }
                            }
                            if current_idx < num_elems {
                                let val = match &item.init {
                                    Initializer::Expr(expr) => {
                                        let raw = self.eval_const_expr(expr).unwrap_or(self.zero_const(base_ty));
                                        let expr_ty = self.get_expr_type(expr);
                                        self.coerce_const_to_type_with_src(raw, base_ty, expr_ty)
                                    }
                                    Initializer::List(sub_items) => {
                                        let mut sub_vals = Vec::new();
                                        for sub in sub_items {
                                            self.flatten_global_init_item(&sub.init, base_ty, &mut sub_vals);
                                        }
                                        let raw = sub_vals.into_iter().next().unwrap_or(self.zero_const(base_ty));
                                        raw.coerce_to(base_ty)
                                    }
                                };
                                values[current_idx] = Self::maybe_promote_long_double(val, is_long_double_target);
                            }
                            current_idx += 1;
                        }
                    }
                    return GlobalInit::Array(values);
                }

                // Struct/union initializer list: emit field-by-field constants
                if let Some(ref layout) = struct_layout {
                    return self.lower_struct_global_init(items, layout);
                }

                // Scalar with braces: int x = { 1 };
                // C11 6.7.9: A scalar can be initialized with a single braced expression.
                if !is_array && items.len() >= 1 {
                    if let Initializer::Expr(expr) = &items[0].init {
                        if let Some(val) = self.eval_const_expr(expr) {
                            let expr_ty = self.get_expr_type(expr);
                            return GlobalInit::Scalar(self.coerce_const_to_type_with_src(val, base_ty, expr_ty));
                        }
                        // Try address expression
                        if let Some(addr_init) = self.eval_global_addr_expr(expr) {
                            return addr_init;
                        }
                    }
                }

                // Fallback: try to emit as an array of I32 constants
                // (handles cases like plain `{1, 2, 3}` without type info)
                let mut values = Vec::new();
                for item in items {
                    if let Initializer::Expr(expr) = &item.init {
                        if let Some(val) = self.eval_const_expr(expr) {
                            values.push(val);
                        } else {
                            values.push(IrConst::I32(0));
                        }
                    } else {
                        values.push(IrConst::I32(0));
                    }
                }
                if !values.is_empty() {
                    return GlobalInit::Array(values);
                }
                GlobalInit::Zero
            }
        }
    }

    /// Create an anonymous global for a compound literal at file scope.
    /// Used for: struct S *s = &(struct S){1, 2};
    fn create_compound_literal_global(
        &mut self,
        type_spec: &TypeSpecifier,
        init: &Initializer,
    ) -> GlobalInit {
        let label = format!(".Lcompound_lit_{}", self.next_anon_struct);
        self.next_anon_struct += 1;

        let base_ty = self.type_spec_to_ir(type_spec);
        let struct_layout = self.get_struct_layout_for_type(type_spec);
        let alloc_size = if let Some(ref layout) = struct_layout {
            layout.size
        } else {
            self.sizeof_type(type_spec)
        };

        let align = if let Some(ref layout) = struct_layout {
            layout.align.max(base_ty.align())
        } else {
            base_ty.align()
        };

        let global_init = self.lower_global_init(init, type_spec, base_ty, false, 0, alloc_size, &struct_layout, &[]);

        let global_ty = if matches!(&global_init, GlobalInit::Array(vals) if !vals.is_empty() && matches!(vals[0], IrConst::I8(_))) {
            IrType::I8
        } else if struct_layout.is_some() && matches!(global_init, GlobalInit::Array(_)) {
            IrType::I8
        } else {
            base_ty
        };

        self.module.globals.push(IrGlobal {
            name: label.clone(),
            ty: global_ty,
            size: alloc_size,
            align,
            init: global_init,
            is_static: true, // compound literal globals are local to translation unit
            is_extern: false,
        });

        GlobalInit::GlobalAddr(label)
    }

    /// Promote an IrConst value to LongDouble (16 bytes) for long double array elements.
    fn promote_to_long_double(val: IrConst) -> IrConst {
        match val {
            IrConst::F64(v) => IrConst::LongDouble(v),
            IrConst::F32(v) => IrConst::LongDouble(v as f64),
            IrConst::I64(v) => IrConst::LongDouble(v as f64),
            IrConst::I32(v) => IrConst::LongDouble(v as f64),
            IrConst::I16(v) => IrConst::LongDouble(v as f64),
            IrConst::I8(v) => IrConst::LongDouble(v as f64),
            other => other, // LongDouble already or Zero
        }
    }

    /// Conditionally promote a value to long double based on target type.
    fn maybe_promote_long_double(val: IrConst, is_long_double: bool) -> IrConst {
        if is_long_double { Self::promote_to_long_double(val) } else { val }
    }

    /// Get the appropriate zero constant for a type, considering long double.
    fn typed_zero_const(&self, base_ty: IrType, is_long_double: bool) -> IrConst {
        if is_long_double { IrConst::LongDouble(0.0) } else { self.zero_const(base_ty) }
    }

    /// Lower a struct initializer list to a GlobalInit::Array of field values.
    /// Emits each field's value at its appropriate position, with padding bytes as zeros.
    /// Supports designated initializers like {.b = 2, .a = 1}.
    /// Check if a type contains pointer elements (either directly or as array elements)
    fn type_has_pointer_elements(ty: &CType) -> bool {
        match ty {
            CType::Pointer(_) => true,
            CType::Array(inner, _) => Self::type_has_pointer_elements(inner),
            _ => false,
        }
    }

    /// Check if an initializer (possibly nested) contains expressions that require
    /// address relocations (string literals, address-of expressions, etc.)
    fn init_has_addr_exprs(&self, init: &Initializer) -> bool {
        match init {
            Initializer::Expr(expr) => {
                matches!(expr, Expr::StringLiteral(_, _))
                    || (self.eval_const_expr(expr).is_none() && self.eval_global_addr_expr(expr).is_some())
            }
            Initializer::List(items) => {
                items.iter().any(|item| self.init_has_addr_exprs(&item.init))
            }
        }
    }

    fn lower_struct_global_init(
        &mut self,
        items: &[InitializerItem],
        layout: &StructLayout,
    ) -> GlobalInit {
        // Check if any field has an address expression (needs relocation).
        // This includes:
        // - String literals initializing pointer fields directly
        // - String literals initializing pointer array fields (flat or braced)
        // - Address-of expressions (&var) initializing pointer fields
        // - Nested initializer lists containing any of the above
        let has_addr_fields = self.struct_init_has_addr_fields(items, layout);

        if has_addr_fields {
            return self.lower_struct_global_init_compound(items, layout);
        }

        // Compute total size including flexible array member data if present
        let total_size = layout.size + self.compute_fam_extra_size(items, layout);
        let mut bytes = vec![0u8; total_size];
        self.fill_struct_global_bytes(items, layout, &mut bytes, 0);

        // Emit as array of I8 (byte) constants
        let values: Vec<IrConst> = bytes.iter().map(|&b| IrConst::I8(b as i8)).collect();
        GlobalInit::Array(values)
    }

    /// Compute extra bytes needed for a flexible array member (FAM) at the end of a struct.
    /// Returns 0 if there is no FAM or no initializer data for it.
    fn compute_fam_extra_size(&self, items: &[InitializerItem], layout: &StructLayout) -> usize {
        if let Some(last_field) = layout.fields.last() {
            if let CType::Array(ref elem_ty, None) = last_field.ty {
                let elem_size = elem_ty.size();
                let last_field_idx = layout.fields.len() - 1;
                let mut current_field_idx = 0usize;
                for (item_idx, item) in items.iter().enumerate() {
                    let field_idx = self.resolve_struct_init_field_idx(item, layout, current_field_idx);
                    if field_idx == last_field_idx {
                        let num_elems = match &item.init {
                            Initializer::List(sub_items) => sub_items.len(),
                            Initializer::Expr(_) => items.len() - item_idx,
                        };
                        return num_elems * elem_size;
                    }
                    if field_idx < layout.fields.len() {
                        current_field_idx = field_idx + 1;
                    }
                }
            }
        }
        0
    }

    /// Check if a struct initializer list contains any fields that require address relocations.
    /// This handles flat init (where multiple items fill an array field), braced init,
    /// and designated init patterns.
    fn struct_init_has_addr_fields(&self, items: &[InitializerItem], layout: &StructLayout) -> bool {
        let mut current_field_idx = 0usize;
        let mut item_idx = 0usize;

        while item_idx < items.len() {
            let item = &items[item_idx];
            let field_idx = self.resolve_struct_init_field_idx(item, layout, current_field_idx);

            if field_idx >= layout.fields.len() {
                // Could be flat init filling into an array field; check if previous field
                // was an array of pointers
                item_idx += 1;
                continue;
            }

            let field_ty = &layout.fields[field_idx].ty;

            match &item.init {
                Initializer::Expr(expr) => {
                    // Direct string literal or address expression
                    if matches!(expr, Expr::StringLiteral(_, _)) {
                        // Check if field is a pointer or array of pointers
                        if Self::type_has_pointer_elements(field_ty) {
                            return true;
                        }
                    }
                    if self.eval_const_expr(expr).is_none() && self.eval_global_addr_expr(expr).is_some() {
                        return true;
                    }
                    // For flat array-of-pointer init, consume remaining items for this field
                    if let CType::Array(elem_ty, Some(arr_size)) = field_ty {
                        if Self::type_has_pointer_elements(elem_ty) {
                            // Check if any of the remaining items for this array have addr exprs
                            for i in 1..*arr_size {
                                let next = item_idx + i;
                                if next >= items.len() { break; }
                                if !items[next].designators.is_empty() { break; }
                                if self.init_has_addr_exprs(&items[next].init) {
                                    return true;
                                }
                            }
                        }
                    }
                }
                Initializer::List(nested_items) => {
                    // Check if the nested list contains addr expressions and the field
                    // is or contains pointer types
                    if Self::type_has_pointer_elements(field_ty) {
                        if self.init_has_addr_exprs(&item.init) {
                            return true;
                        }
                    }
                    // Also check for nested structs containing pointer fields
                    if let Some(nested_layout) = self.get_struct_layout_for_ctype(field_ty) {
                        if self.struct_init_has_addr_fields(nested_items, &nested_layout) {
                            return true;
                        }
                    }
                }
            }

            current_field_idx = field_idx + 1;
            item_idx += 1;
        }
        false
    }

    /// Recursively fill byte buffer for struct global initialization.
    /// Returns the number of initializer items consumed.
    ///
    /// Handles nested structs/unions, arrays (including multi-dimensional), string
    /// literals, designators, flexible array members, and bitfields.
    fn fill_struct_global_bytes(
        &self,
        items: &[InitializerItem],
        layout: &StructLayout,
        bytes: &mut [u8],
        base_offset: usize,
    ) -> usize {
        let mut item_idx = 0usize;
        let mut current_field_idx = 0usize;

        while item_idx < items.len() {
            let item = &items[item_idx];

            let designator_name = match item.designators.first() {
                Some(Designator::Field(ref name)) => Some(name.as_str()),
                _ => None,
            };
            let array_start_idx = self.extract_index_designator(item, designator_name.is_some());
            let field_idx = match layout.resolve_init_field_idx(designator_name, current_field_idx) {
                Some(idx) => idx,
                None => {
                    if designator_name.is_some() { item_idx += 1; continue; }
                    break;
                }
            };
            let field_layout = &layout.fields[field_idx];
            let field_offset = base_offset + field_layout.offset;

            let has_nested_designator = item.designators.len() > 1
                && matches!(item.designators.first(), Some(Designator::Field(_)));

            match &field_layout.ty {
                // Nested designator into struct/union: drill into sub-composite
                CType::Struct(st) if has_nested_designator => {
                    self.fill_nested_designator_composite(
                        item, &StructLayout::for_struct(&st.fields), bytes, field_offset,
                    );
                    item_idx += 1;
                }
                CType::Union(st) if has_nested_designator => {
                    self.fill_nested_designator_composite(
                        item, &StructLayout::for_union(&st.fields), bytes, field_offset,
                    );
                    item_idx += 1;
                }
                // Nested designator into array: .field[idx] = val
                // After the designated element, continue consuming non-designated
                // items for subsequent array positions (C11 6.7.9p17).
                CType::Array(elem_ty, Some(arr_size)) if has_nested_designator => {
                    let arr_size = *arr_size;
                    let desig_idx = self.fill_nested_designator_array(item, elem_ty, arr_size, bytes, field_offset);
                    item_idx += 1;
                    // Continue consuming subsequent non-designated items for positions
                    // desig_idx+1, desig_idx+2, ... within the same array field
                    let elem_size = elem_ty.size();
                    let elem_ir_ty = IrType::from_ctype(elem_ty);
                    let mut ai = desig_idx + 1;
                    while ai < arr_size && item_idx < items.len() {
                        let next_item = &items[item_idx];
                        if !next_item.designators.is_empty() {
                            break;
                        }
                        if let Initializer::Expr(ref expr) = next_item.init {
                            let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                            let elem_offset = field_offset + ai * elem_size;
                            self.write_const_to_bytes(bytes, elem_offset, &val, elem_ir_ty);
                        } else {
                            break;
                        }
                        item_idx += 1;
                        ai += 1;
                    }
                }
                // Struct or union field (non-nested designator)
                CType::Struct(st) => {
                    let sub_layout = StructLayout::for_struct(&st.fields);
                    item_idx += self.fill_composite_field(
                        &items[item_idx..], &sub_layout, bytes, field_offset,
                    );
                }
                CType::Union(st) => {
                    let sub_layout = StructLayout::for_union(&st.fields);
                    item_idx += self.fill_composite_field(
                        &items[item_idx..], &sub_layout, bytes, field_offset,
                    );
                }
                // Fixed-size array field
                CType::Array(elem_ty, Some(arr_size)) => {
                    let advanced = self.fill_array_field(
                        items, item_idx, elem_ty, *arr_size,
                        bytes, field_offset, array_start_idx,
                    );
                    if advanced.skip_update {
                        item_idx = advanced.new_item_idx;
                        current_field_idx = field_idx + 1;
                        continue;
                    }
                    item_idx = advanced.new_item_idx;
                }
                // Flexible array member (FAM)
                CType::Array(elem_ty, None) => {
                    let advanced = self.fill_fam_field(
                        items, item_idx, elem_ty, bytes, field_offset,
                    );
                    if advanced.skip_update {
                        item_idx = advanced.new_item_idx;
                        current_field_idx = field_idx + 1;
                        continue;
                    }
                    item_idx = advanced.new_item_idx;
                }
                // Complex field: write {real, imag} pair to bytes
                CType::ComplexFloat | CType::ComplexDouble | CType::ComplexLongDouble => {
                    self.write_complex_field_to_bytes(bytes, field_offset, &field_layout.ty, &item.init);
                    item_idx += 1;
                }
                // Scalar field (possibly bitfield)
                _ => {
                    let field_ir_ty = IrType::from_ctype(&field_layout.ty);
                    let val = self.eval_init_scalar(&item.init);
                    if let (Some(bit_offset), Some(bit_width)) = (field_layout.bit_offset, field_layout.bit_width) {
                        self.write_bitfield_to_bytes(bytes, field_offset, &val, field_ir_ty, bit_offset, bit_width);
                    } else {
                        self.write_const_to_bytes(bytes, field_offset, &val, field_ir_ty);
                    }
                    item_idx += 1;
                }
            }
            current_field_idx = field_idx + 1;

            if layout.is_union && designator_name.is_none() {
                break;
            }
        }
        item_idx
    }

    // --- fill_struct_global_bytes helpers ---

    /// Extract an array index designator from an initializer item.
    fn extract_index_designator(&self, item: &InitializerItem, has_field_desig: bool) -> Option<usize> {
        if has_field_desig {
            item.designators.iter().find_map(|d| {
                if let Designator::Index(ref idx_expr) = d {
                    self.eval_const_expr(idx_expr).and_then(|c| c.to_usize())
                } else {
                    None
                }
            })
        } else {
            match item.designators.first() {
                Some(Designator::Index(ref idx_expr)) => {
                    self.eval_const_expr(idx_expr).and_then(|c| c.to_usize())
                }
                _ => None,
            }
        }
    }

    /// Evaluate an initializer to a scalar constant (handles both Expr and brace-wrapped List).
    fn eval_init_scalar(&self, init: &Initializer) -> IrConst {
        match init {
            Initializer::Expr(expr) => self.eval_const_expr(expr).unwrap_or(IrConst::I64(0)),
            Initializer::List(sub_items) => {
                sub_items.first()
                    .and_then(|first| {
                        if let Initializer::Expr(expr) = &first.init {
                            self.eval_const_expr(expr)
                        } else {
                            None
                        }
                    })
                    .unwrap_or(IrConst::I64(0))
            }
        }
    }

    /// Write a string literal into a byte buffer at the given offset, with null terminator.
    fn write_string_to_bytes(bytes: &mut [u8], offset: usize, s: &str, max_len: usize) {
        let str_bytes = s.as_bytes();
        for (i, &b) in str_bytes.iter().enumerate() {
            if i >= max_len { break; }
            if offset + i < bytes.len() {
                bytes[offset + i] = b;
            }
        }
        if str_bytes.len() < max_len && offset + str_bytes.len() < bytes.len() {
            bytes[offset + str_bytes.len()] = 0;
        }
    }

    /// Handle nested designator drilling into a struct/union sub-field (e.g., .a.b = val).
    fn fill_nested_designator_composite(
        &self, item: &InitializerItem, sub_layout: &StructLayout,
        bytes: &mut [u8], field_offset: usize,
    ) {
        let sub_item = InitializerItem {
            designators: item.designators[1..].to_vec(),
            init: item.init.clone(),
        };
        self.fill_struct_global_bytes(&[sub_item], sub_layout, bytes, field_offset);
    }

    /// Process a nested designator into an array field (e.g., .field[idx] = val).
    /// Returns the array index that was designated.
    fn fill_nested_designator_array(
        &self, item: &InitializerItem, elem_ty: &CType, arr_size: usize,
        bytes: &mut [u8], field_offset: usize,
    ) -> usize {
        let elem_size = elem_ty.size();
        let elem_ir_ty = IrType::from_ctype(elem_ty);

        // Collect all Index and Field designators after the first (which is Field("name"))
        // For .a[1][2].b we have designators: [Field("a"), Index(1), Index(2), Field("b")]
        // After stripping Field("a"), remaining: [Index(1), Index(2), Field("b")]
        let remaining = &item.designators[1..];

        // Find first Index designator (outer array index)
        let (first_idx_pos, idx) = remaining.iter().enumerate().find_map(|(i, d)| {
            if let Designator::Index(ref idx_expr) = d {
                self.eval_const_expr(idx_expr).and_then(|c| c.to_usize()).map(|v| (i, v))
            } else {
                None
            }
        }).unwrap_or((0, 0));

        if idx >= arr_size {
            return idx;
        }

        let elem_offset = field_offset + idx * elem_size;

        // Check for further designators after the first Index
        let after_first_idx = &remaining[first_idx_pos + 1..];

        // Collect remaining Field designators for struct drilling
        let remaining_field_desigs: Vec<_> = after_first_idx.iter()
            .filter(|d| matches!(d, Designator::Field(_)))
            .cloned()
            .collect();

        // Collect remaining Index designators for multi-dimensional array drilling
        let remaining_index_desigs: Vec<_> = after_first_idx.iter()
            .filter(|d| matches!(d, Designator::Index(_)))
            .cloned()
            .collect();

        if !remaining_field_desigs.is_empty() {
            // Drill into struct element: .a[1].b = val
            if !remaining_index_desigs.is_empty() {
                // .a[1].b[2] - remaining has both field and index
                // Build designator list with fields and indices from after_first_idx
                let sub_desigs: Vec<_> = after_first_idx.iter().cloned().collect();
                if let Some(sub_layout) = self.get_struct_layout_for_ctype(elem_ty) {
                    let sub_item = InitializerItem {
                        designators: sub_desigs,
                        init: item.init.clone(),
                    };
                    self.fill_struct_global_bytes(&[sub_item], &sub_layout, bytes, elem_offset);
                }
            } else {
                if let Some(sub_layout) = self.get_struct_layout_for_ctype(elem_ty) {
                    let sub_item = InitializerItem {
                        designators: remaining_field_desigs,
                        init: item.init.clone(),
                    };
                    self.fill_struct_global_bytes(&[sub_item], &sub_layout, bytes, elem_offset);
                }
            }
        } else if !remaining_index_desigs.is_empty() {
            // Multi-dimensional array: .a[1][2] = val
            // elem_ty is the inner array type (e.g., float[10] for float a[3][10])
            if let CType::Array(inner_elem_ty, Some(inner_size)) = elem_ty {
                // Recursively handle the inner array with the remaining index designators
                let inner_idx = remaining_index_desigs.iter().find_map(|d| {
                    if let Designator::Index(ref idx_expr) = d {
                        self.eval_const_expr(idx_expr).and_then(|c| c.to_usize())
                    } else {
                        None
                    }
                }).unwrap_or(0);
                if inner_idx < *inner_size {
                    let inner_elem_size = inner_elem_ty.size();
                    let inner_ir_ty = IrType::from_ctype(inner_elem_ty);
                    let inner_offset = elem_offset + inner_idx * inner_elem_size;

                    // Check for even more Index designators (3D+ arrays)
                    let further_indices: Vec<_> = remaining_index_desigs[1..].to_vec();
                    if !further_indices.is_empty() {
                        if let CType::Array(deeper_elem, Some(deeper_size)) = inner_elem_ty.as_ref() {
                            // Build sub-item with remaining indices as [Field("dummy"), Index(...)]
                            // Actually we can recursively call ourselves
                            let sub_item = InitializerItem {
                                designators: {
                                    let mut d = vec![Designator::Field(String::new())]; // dummy field
                                    d.extend(further_indices);
                                    d
                                },
                                init: item.init.clone(),
                            };
                            self.fill_nested_designator_array(
                                &sub_item, inner_elem_ty, *inner_size,
                                bytes, elem_offset,
                            );
                        }
                    } else if let Initializer::Expr(ref expr) = item.init {
                        if let Expr::StringLiteral(s, _) = expr {
                            if let CType::Array(deep_inner, Some(deep_size)) = inner_elem_ty.as_ref() {
                                if matches!(deep_inner.as_ref(), CType::Char | CType::UChar) {
                                    Self::write_string_to_bytes(bytes, inner_offset, s, *deep_size);
                                } else {
                                    let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                                    self.write_const_to_bytes(bytes, inner_offset, &val, inner_ir_ty);
                                }
                            } else {
                                let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                                self.write_const_to_bytes(bytes, inner_offset, &val, inner_ir_ty);
                            }
                        } else {
                            let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                            self.write_const_to_bytes(bytes, inner_offset, &val, inner_ir_ty);
                        }
                    }
                }
            }
        } else if let Initializer::Expr(ref expr) = item.init {
            // No further designators - store value at elem_offset
            // Handle string literal initializing a char array element
            // (e.g., .a[1] = "abc" where a is char a[3][10])
            if let Expr::StringLiteral(s, _) = expr {
                if let CType::Array(inner, Some(inner_size)) = elem_ty {
                    if matches!(inner.as_ref(), CType::Char | CType::UChar) {
                        Self::write_string_to_bytes(bytes, elem_offset, s, *inner_size);
                    } else {
                        let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                        self.write_const_to_bytes(bytes, elem_offset, &val, elem_ir_ty);
                    }
                } else if matches!(elem_ty, CType::Char | CType::UChar) {
                    let val = s.as_bytes().first().map(|&b| IrConst::I8(b as i8)).unwrap_or(IrConst::I8(0));
                    self.write_const_to_bytes(bytes, elem_offset, &val, elem_ir_ty);
                } else {
                    let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                    self.write_const_to_bytes(bytes, elem_offset, &val, elem_ir_ty);
                }
            } else {
                let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                self.write_const_to_bytes(bytes, elem_offset, &val, elem_ir_ty);
            }
        } else if let Initializer::List(ref sub_items) = item.init {
            // Handle list init for array element (e.g., .a[1] = {1,2,3})
            match elem_ty {
                CType::Array(inner_elem_ty, Some(inner_size)) => {
                    let inner_elem_size = inner_elem_ty.size();
                    let inner_ir_ty = IrType::from_ctype(inner_elem_ty);
                    for (si, sub_item) in sub_items.iter().enumerate() {
                        if si >= *inner_size { break; }
                        if let Initializer::Expr(ref expr) = sub_item.init {
                            let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                            let inner_offset = elem_offset + si * inner_elem_size;
                            self.write_const_to_bytes(bytes, inner_offset, &val, inner_ir_ty);
                        }
                    }
                }
                CType::Struct(ref st) => {
                    let sub_layout = StructLayout::for_struct(&st.fields);
                    self.fill_struct_global_bytes(sub_items, &sub_layout, bytes, elem_offset);
                }
                _ => {}
            }
        }
        idx
    }

    /// Fill a composite (struct/union) field from an initializer.
    /// Returns the number of items consumed.
    fn fill_composite_field(
        &self, items: &[InitializerItem], sub_layout: &StructLayout,
        bytes: &mut [u8], field_offset: usize,
    ) -> usize {
        match &items[0].init {
            Initializer::List(sub_items) => {
                self.fill_struct_global_bytes(sub_items, sub_layout, bytes, field_offset);
                1
            }
            Initializer::Expr(_) => {
                let consumed = self.fill_struct_global_bytes(items, sub_layout, bytes, field_offset);
                if consumed == 0 { 1 } else { consumed }
            }
        }
    }

    /// Fill a fixed-size array field. Handles string literals, arrays of composites,
    /// arrays of scalars with designators, and flat initialization.
    fn fill_array_field(
        &self,
        items: &[InitializerItem],
        item_idx: usize,
        elem_ty: &CType,
        arr_size: usize,
        bytes: &mut [u8],
        field_offset: usize,
        array_start_idx: Option<usize>,
    ) -> ArrayFillResult {
        let item = &items[item_idx];
        let elem_size = elem_ty.size();
        let elem_ir_ty = IrType::from_ctype(elem_ty);

        match &item.init {
            Initializer::List(sub_items) => {
                // Check for brace-wrapped string literal: { "hello" }
                if self.try_fill_string_literal_init(sub_items, elem_ty, arr_size, bytes, field_offset) {
                    return ArrayFillResult { new_item_idx: item_idx + 1, skip_update: true };
                }
                if matches!(elem_ty, CType::Struct(_) | CType::Union(_)) {
                    self.fill_array_of_composites(sub_items, elem_ty, arr_size, elem_size, bytes, field_offset);
                } else {
                    self.fill_array_of_scalars(sub_items, arr_size, elem_size, elem_ir_ty, bytes, field_offset);
                }
                ArrayFillResult { new_item_idx: item_idx + 1, skip_update: false }
            }
            Initializer::Expr(expr) => {
                // String literal for char array
                if let Expr::StringLiteral(s, _) = expr {
                    if matches!(elem_ty, CType::Char | CType::UChar) {
                        Self::write_string_to_bytes(bytes, field_offset, s, arr_size);
                        return ArrayFillResult { new_item_idx: item_idx + 1, skip_update: true };
                    }
                }
                // Flat init from consecutive items
                let start_ai = array_start_idx.unwrap_or(0);
                let new_idx = if matches!(elem_ty, CType::Struct(_) | CType::Union(_)) {
                    self.fill_flat_array_of_composites(
                        items, item_idx, elem_ty, arr_size, elem_size, elem_ir_ty,
                        bytes, field_offset, start_ai,
                    )
                } else {
                    self.fill_flat_array_of_scalars(
                        items, item_idx, arr_size, elem_size, elem_ir_ty,
                        bytes, field_offset, start_ai,
                    )
                };
                ArrayFillResult { new_item_idx: new_idx, skip_update: true }
            }
        }
    }

    /// Fill a flexible array member (FAM) field.
    fn fill_fam_field(
        &self,
        items: &[InitializerItem],
        item_idx: usize,
        elem_ty: &CType,
        bytes: &mut [u8],
        field_offset: usize,
    ) -> ArrayFillResult {
        let elem_size = elem_ty.size();
        let elem_ir_ty = IrType::from_ctype(elem_ty);

        match &items[item_idx].init {
            Initializer::List(sub_items) => {
                for (ai, sub_item) in sub_items.iter().enumerate() {
                    let elem_offset = field_offset + ai * elem_size;
                    if elem_offset + elem_size > bytes.len() { break; }
                    let val = self.eval_init_scalar(&sub_item.init);
                    self.write_const_to_bytes(bytes, elem_offset, &val, elem_ir_ty);
                }
                ArrayFillResult { new_item_idx: item_idx + 1, skip_update: false }
            }
            Initializer::Expr(expr) => {
                let mut ai = 0usize;
                let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                let elem_offset = field_offset + ai * elem_size;
                if elem_offset + elem_size <= bytes.len() {
                    self.write_const_to_bytes(bytes, elem_offset, &val, elem_ir_ty);
                }
                ai += 1;
                let mut new_idx = item_idx + 1;
                while new_idx < items.len() {
                    let next_item = &items[new_idx];
                    if !next_item.designators.is_empty() { break; }
                    if let Initializer::Expr(e) = &next_item.init {
                        let val = self.eval_const_expr(e).unwrap_or(IrConst::I64(0));
                        let elem_offset = field_offset + ai * elem_size;
                        if elem_offset + elem_size <= bytes.len() {
                            self.write_const_to_bytes(bytes, elem_offset, &val, elem_ir_ty);
                        }
                        ai += 1;
                        new_idx += 1;
                    } else {
                        break;
                    }
                }
                ArrayFillResult { new_item_idx: new_idx, skip_update: true }
            }
        }
    }

    /// Try to interpret a brace-wrapped init list as a string literal for a char array.
    /// Returns true if handled.
    fn try_fill_string_literal_init(
        &self, sub_items: &[InitializerItem], elem_ty: &CType, arr_size: usize,
        bytes: &mut [u8], field_offset: usize,
    ) -> bool {
        if sub_items.len() == 1 && sub_items[0].designators.is_empty() {
            if let Initializer::Expr(Expr::StringLiteral(s, _)) = &sub_items[0].init {
                if matches!(elem_ty, CType::Char | CType::UChar) {
                    Self::write_string_to_bytes(bytes, field_offset, s, arr_size);
                    return true;
                }
            }
        }
        false
    }

    /// Fill an array of struct/union elements from a braced sub-item list.
    fn fill_array_of_composites(
        &self, sub_items: &[InitializerItem], elem_ty: &CType,
        arr_size: usize, elem_size: usize, bytes: &mut [u8], field_offset: usize,
    ) {
        let sub_layout = self.get_composite_layout(elem_ty);
        let mut sub_idx = 0usize;
        let mut ai = 0usize;
        while ai < arr_size && sub_idx < sub_items.len() {
            let elem_offset = field_offset + ai * elem_size;
            match &sub_items[sub_idx].init {
                Initializer::List(inner_items) => {
                    self.fill_struct_global_bytes(inner_items, &sub_layout, bytes, elem_offset);
                    sub_idx += 1;
                }
                Initializer::Expr(_) => {
                    let consumed = self.fill_struct_global_bytes(&sub_items[sub_idx..], &sub_layout, bytes, elem_offset);
                    sub_idx += consumed;
                }
            }
            ai += 1;
        }
    }

    /// Fill an array of scalar elements from a braced sub-item list (with designator support).
    fn fill_array_of_scalars(
        &self, sub_items: &[InitializerItem], arr_size: usize,
        elem_size: usize, elem_ir_ty: IrType, bytes: &mut [u8], field_offset: usize,
    ) {
        let mut ai = 0usize;
        for sub_item in sub_items.iter() {
            if let Some(Designator::Index(ref idx_expr)) = sub_item.designators.first() {
                if let Some(idx) = self.eval_const_expr(idx_expr).and_then(|c| c.to_usize()) {
                    ai = idx;
                }
            }
            if ai >= arr_size { break; }
            let elem_offset = field_offset + ai * elem_size;
            let val = self.eval_init_scalar(&sub_item.init);
            self.write_const_to_bytes(bytes, elem_offset, &val, elem_ir_ty);
            ai += 1;
        }
    }

    /// Fill flat init of an array of composites from consecutive items.
    /// Returns the new item_idx.
    fn fill_flat_array_of_composites(
        &self, items: &[InitializerItem], mut item_idx: usize,
        elem_ty: &CType, arr_size: usize, elem_size: usize, elem_ir_ty: IrType,
        bytes: &mut [u8], field_offset: usize, start_ai: usize,
    ) -> usize {
        let sub_layout = self.get_composite_layout(elem_ty);
        if matches!(elem_ty, CType::Struct(_)) {
            for ai in start_ai..arr_size {
                if item_idx >= items.len() { break; }
                let elem_offset = field_offset + ai * elem_size;
                let consumed = self.fill_struct_global_bytes(&items[item_idx..], &sub_layout, bytes, elem_offset);
                item_idx += consumed;
            }
        } else {
            // Union: take one item per element
            for ai in start_ai..arr_size {
                if item_idx >= items.len() { break; }
                let elem_offset = field_offset + ai * elem_size;
                if let Initializer::Expr(e) = &items[item_idx].init {
                    let val = self.eval_const_expr(e).unwrap_or(IrConst::I64(0));
                    self.write_const_to_bytes(bytes, elem_offset, &val, elem_ir_ty);
                }
                item_idx += 1;
            }
        }
        item_idx
    }

    /// Fill flat init of an array of scalars from consecutive items.
    /// Returns the new item_idx.
    fn fill_flat_array_of_scalars(
        &self, items: &[InitializerItem], item_idx: usize,
        arr_size: usize, elem_size: usize, elem_ir_ty: IrType,
        bytes: &mut [u8], field_offset: usize, start_ai: usize,
    ) -> usize {
        let mut consumed = 0usize;
        let mut ai = start_ai;
        while ai < arr_size && (item_idx + consumed) < items.len() {
            let cur_item = &items[item_idx + consumed];
            if !cur_item.designators.is_empty() && consumed > 0 { break; }
            if let Initializer::Expr(e) = &cur_item.init {
                let val = self.eval_const_expr(e).unwrap_or(IrConst::I64(0));
                let elem_offset = field_offset + ai * elem_size;
                self.write_const_to_bytes(bytes, elem_offset, &val, elem_ir_ty);
                consumed += 1;
                ai += 1;
            } else {
                break;
            }
        }
        item_idx + consumed.max(1)
    }

    /// Get the StructLayout for a composite (struct or union) CType.
    fn get_composite_layout(&self, ty: &CType) -> StructLayout {
        match ty {
            CType::Struct(st) => StructLayout::for_struct(&st.fields),
            CType::Union(st) => StructLayout::for_union(&st.fields),
            _ => unreachable!("get_composite_layout called on non-composite type"),
        }
    }

    /// Lower a struct global init that contains address expressions.
    /// Emits field-by-field using Compound, with padding bytes between fields.
    /// Handles flat init (where multiple items fill an array-of-pointer field),
    /// braced init, and designated init patterns.
    fn lower_struct_global_init_compound(
        &mut self,
        items: &[InitializerItem],
        layout: &StructLayout,
    ) -> GlobalInit {
        let total_size = layout.size;
        let mut elements: Vec<GlobalInit> = Vec::new();
        let mut current_offset = 0usize;

        // Build a map of field_idx -> list of initializer items.
        // For array fields with flat init, multiple items may map to the same field.
        let mut field_inits: Vec<Vec<&InitializerItem>> = vec![Vec::new(); layout.fields.len()];
        let mut current_field_idx = 0usize;

        let mut item_idx = 0;
        while item_idx < items.len() {
            let item = &items[item_idx];
            let field_idx = self.resolve_struct_init_field_idx(item, layout, current_field_idx);
            if field_idx >= layout.fields.len() {
                item_idx += 1;
                continue;
            }

            let field_ty = &layout.fields[field_idx].ty;

            // Check if this is a flat init filling an array field
            if let CType::Array(_, Some(arr_size)) = field_ty {
                if matches!(&item.init, Initializer::Expr(_)) {
                    // Flat init: consume up to arr_size items for this array field
                    let mut consumed = 0;
                    while consumed < *arr_size && (item_idx + consumed) < items.len() {
                        let cur_item = &items[item_idx + consumed];
                        // Stop if we hit a designator targeting a different field
                        if !cur_item.designators.is_empty() && consumed > 0 {
                            break;
                        }
                        if matches!(&cur_item.init, Initializer::List(_)) && consumed > 0 {
                            break;
                        }
                        field_inits[field_idx].push(cur_item);
                        consumed += 1;
                    }
                    item_idx += consumed;
                    current_field_idx = field_idx + 1;
                    let has_designator = !item.designators.is_empty();
                    if layout.is_union && !has_designator { break; }
                    continue;
                }
            }

            field_inits[field_idx].push(item);
            current_field_idx = field_idx + 1;
            item_idx += 1;
            let has_designator = !item.designators.is_empty();
            if layout.is_union && !has_designator { break; }
        }

        let mut fi = 0;
        while fi < layout.fields.len() {
            let field_offset = layout.fields[fi].offset;
            let field_size = layout.fields[fi].ty.size();
            let field_is_pointer = matches!(layout.fields[fi].ty, CType::Pointer(_) | CType::Function(_));

            // Check if this is a bitfield: if so, we need to pack all bitfields
            // sharing the same storage unit into a single byte buffer.
            if layout.fields[fi].bit_offset.is_some() {
                let storage_unit_offset = field_offset;
                let storage_unit_size = field_size; // All bitfields in this unit have the same type size

                // Emit padding before this storage unit
                if storage_unit_offset > current_offset {
                    let pad = storage_unit_offset - current_offset;
                    push_zero_bytes(&mut elements, pad);
                    current_offset = storage_unit_offset;
                }

                // Allocate a byte buffer for the storage unit and pack all
                // bitfield values into it using read-modify-write.
                let mut unit_bytes = vec![0u8; storage_unit_size];

                // Process all consecutive bitfield fields sharing this storage unit offset
                while fi < layout.fields.len()
                    && layout.fields[fi].offset == storage_unit_offset
                    && layout.fields[fi].bit_offset.is_some()
                {
                    let bit_offset = layout.fields[fi].bit_offset.unwrap();
                    let bit_width = layout.fields[fi].bit_width.unwrap();
                    let field_ir_ty = IrType::from_ctype(&layout.fields[fi].ty);

                    let inits = &field_inits[fi];
                    let val = if !inits.is_empty() {
                        self.eval_init_scalar(&inits[0].init)
                    } else {
                        IrConst::I32(0) // Zero-init for missing initializers
                    };

                    // Pack this bitfield value into the storage unit buffer
                    self.write_bitfield_to_bytes(&mut unit_bytes, 0, &val, field_ir_ty, bit_offset, bit_width);
                    fi += 1;
                }

                // Emit the packed storage unit as individual bytes
                for &b in &unit_bytes {
                    elements.push(GlobalInit::Scalar(IrConst::I8(b as i8)));
                }
                current_offset += storage_unit_size;
                continue;
            }

            // Emit padding before this field
            if field_offset > current_offset {
                let pad = field_offset - current_offset;
                push_zero_bytes(&mut elements, pad);
                current_offset = field_offset;
            }

            // Check if this is a bitfield - if so, collect all bitfields sharing the
            // same storage unit and pack them into a single value.
            if layout.fields[fi].bit_offset.is_some() {
                let storage_offset = field_offset;
                let storage_size = field_size;
                let mut packed_val: u64 = 0;

                // Pack all bitfields at this same storage unit offset
                while fi < layout.fields.len()
                    && layout.fields[fi].bit_offset.is_some()
                    && layout.fields[fi].offset == storage_offset
                {
                    let bit_offset = layout.fields[fi].bit_offset.unwrap();
                    let bit_width = layout.fields[fi].bit_width.unwrap_or(0);
                    if bit_width > 0 {
                        let inits = &field_inits[fi];
                        let val = if !inits.is_empty() {
                            self.eval_init_scalar(&inits[0].init).to_u64().unwrap_or(0)
                        } else {
                            0
                        };
                        let mask = if bit_width >= 64 { u64::MAX } else { (1u64 << bit_width) - 1 };
                        packed_val |= (val & mask) << bit_offset;
                    }
                    fi += 1;
                }

                // Emit the packed storage unit as bytes (little-endian)
                let le = packed_val.to_le_bytes();
                for i in 0..storage_size {
                    elements.push(GlobalInit::Scalar(IrConst::I8(le[i] as i8)));
                }
                current_offset += storage_size;
                continue;
            }

            let inits = &field_inits[fi];
            if inits.is_empty() {
                // No initializer for this field - zero fill
                push_zero_bytes(&mut elements, field_size);
            } else if inits.len() == 1 {
                let item = inits[0];
                // Check if this is a designated init for an array-of-pointer field
                // e.g., .a[1] = "abc" where a is char *a[3]
                let has_array_idx_designator = item.designators.iter().any(|d| matches!(d, Designator::Index(_)));
                if has_array_idx_designator {
                    if let CType::Array(elem_ty, Some(arr_size)) = &layout.fields[fi].ty {
                        if Self::type_has_pointer_elements(elem_ty) {
                            // Designated init for pointer array element
                            self.emit_compound_ptr_array_designated_init(
                                &mut elements, &[item], elem_ty, *arr_size);
                        } else {
                            // Non-pointer array with designator - use byte-level approach
                            self.emit_compound_field_init(&mut elements, &item.init, &layout.fields[fi].ty, field_size, field_is_pointer);
                        }
                    } else {
                        self.emit_compound_field_init(&mut elements, &item.init, &layout.fields[fi].ty, field_size, field_is_pointer);
                    }
                } else {
                    self.emit_compound_field_init(&mut elements, &item.init, &layout.fields[fi].ty, field_size, field_is_pointer);
                }
            } else {
                // Multiple items for this field (flat array init)
                self.emit_compound_flat_array_init(&mut elements, inits, &layout.fields[fi].ty, field_size);
            }
            current_offset += field_size;
            fi += 1;
        }

        // Trailing padding
        while current_offset < total_size {
            elements.push(GlobalInit::Scalar(IrConst::I8(0)));
            current_offset += 1;
        }

        GlobalInit::Compound(elements)
    }

    /// Emit a single field initializer in compound (relocation-aware) mode.
    fn emit_compound_field_init(
        &mut self,
        elements: &mut Vec<GlobalInit>,
        init: &Initializer,
        field_ty: &CType,
        field_size: usize,
        field_is_pointer: bool,
    ) {
        // Complex fields: emit {real, imag} as byte pairs
        if field_ty.is_complex() {
            let mut bytes = vec![0u8; field_size];
            self.write_complex_field_to_bytes(&mut bytes, 0, field_ty, init);
            for &b in &bytes {
                elements.push(GlobalInit::Scalar(IrConst::I8(b as i8)));
            }
            return;
        }

        match init {
            Initializer::Expr(expr) => {
                if let Expr::StringLiteral(s, _) = expr {
                    if field_is_pointer {
                        // String literal initializing a pointer field:
                        // create a .rodata string entry and emit GlobalAddr
                        let label = self.intern_string_literal(s);
                        elements.push(GlobalInit::GlobalAddr(label));
                    } else {
                        // String literal initializing a char array field
                        let s_bytes = s.as_bytes();
                        for (i, &b) in s_bytes.iter().enumerate() {
                            if i >= field_size { break; }
                            elements.push(GlobalInit::Scalar(IrConst::I8(b as i8)));
                        }
                        // null terminator + remaining zero fill
                        for _ in s_bytes.len()..field_size {
                            elements.push(GlobalInit::Scalar(IrConst::I8(0)));
                        }
                    }
                } else if let Some(val) = self.eval_const_expr(expr) {
                    // Scalar constant - emit as bytes
                    self.push_const_as_bytes(elements, &val, field_size);
                } else if let Some(addr_init) = self.eval_global_addr_expr(expr) {
                    // Address expression - emit as relocation
                    elements.push(addr_init);
                } else {
                    // Unknown - zero fill
                    push_zero_bytes(elements, field_size);
                }
            }
            Initializer::List(nested_items) => {
                // Check if this is an array of pointers
                if let CType::Array(elem_ty, Some(arr_size)) = field_ty {
                    if Self::type_has_pointer_elements(elem_ty) {
                        // Array of pointers: emit each element as a GlobalAddr or zero
                        self.emit_compound_ptr_array_init(elements, nested_items, elem_ty, *arr_size);
                        return;
                    }
                }

                // Check if nested struct has pointer fields
                let field_ty_clone = field_ty.clone();
                if let Some(nested_layout) = self.get_struct_layout_for_ctype(&field_ty_clone) {
                    if self.struct_init_has_addr_fields(nested_items, &nested_layout) {
                        // Recursively handle nested struct with address fields
                        let nested = self.lower_struct_global_init_compound(nested_items, &nested_layout);
                        // Flatten nested Compound into our elements
                        if let GlobalInit::Compound(nested_elems) = nested {
                            elements.extend(nested_elems);
                        } else {
                            // Shouldn't happen, but zero-fill as fallback
                            push_zero_bytes(elements, field_size);
                        }
                        return;
                    }
                }

                // No address fields - serialize to bytes
                let mut bytes = vec![0u8; field_size];
                if let Some(nested_layout) = self.get_struct_layout_for_ctype(&field_ty_clone) {
                    self.fill_struct_global_bytes(nested_items, &nested_layout, &mut bytes, 0);
                } else {
                    // Simple array or scalar list
                    let mut byte_offset = 0;
                    let elem_ir_ty = match &field_ty_clone {
                        CType::Array(inner, _) => IrType::from_ctype(inner),
                        other => IrType::from_ctype(other),
                    };
                    let elem_size = elem_ir_ty.size().max(1);
                    for ni in nested_items {
                        if byte_offset >= field_size { break; }
                        if let Initializer::Expr(ref e) = ni.init {
                            if let Some(val) = self.eval_const_expr(e) {
                                let e_ty = self.get_expr_type(e);
                                let val = self.coerce_const_to_type_with_src(val, elem_ir_ty, e_ty);
                                self.write_const_to_bytes(&mut bytes, byte_offset, &val, elem_ir_ty);
                            }
                        }
                        byte_offset += elem_size;
                    }
                }
                for b in &bytes {
                    elements.push(GlobalInit::Scalar(IrConst::I8(*b as i8)));
                }
            }
        }
    }

    /// Emit an array-of-pointers field from a braced initializer list.
    /// Each element is either a GlobalAddr (for string literals / address expressions)
    /// or zero-filled (for missing elements).
    fn emit_compound_ptr_array_init(
        &mut self,
        elements: &mut Vec<GlobalInit>,
        items: &[InitializerItem],
        elem_ty: &CType,
        arr_size: usize,
    ) {
        let ptr_size = 8; // 64-bit pointers
        let mut ai = 0usize;

        for item in items {
            if ai >= arr_size { break; }

            // Handle index designator: [idx] = val
            if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                if let Some(idx) = self.eval_const_expr(idx_expr).and_then(|c| c.to_usize()) {
                    // Zero-fill skipped elements
                    while ai < idx && ai < arr_size {
                        push_zero_bytes(elements, ptr_size);
                        ai += 1;
                    }
                }
            }
            if ai >= arr_size { break; }

            if let Initializer::Expr(ref expr) = item.init {
                if let Expr::StringLiteral(s, _) = expr {
                    let label = self.intern_string_literal(s);
                    elements.push(GlobalInit::GlobalAddr(label));
                } else if let Some(addr_init) = self.eval_global_addr_expr(expr) {
                    elements.push(addr_init);
                } else if let Some(val) = self.eval_const_expr(expr) {
                    self.push_const_as_bytes(elements, &val, ptr_size);
                } else {
                    // zero
                    push_zero_bytes(elements, ptr_size);
                }
            } else {
                // Nested list - zero fill this element
                push_zero_bytes(elements, ptr_size);
            }
            ai += 1;
        }

        // Zero-fill remaining elements
        while ai < arr_size {
            push_zero_bytes(elements, ptr_size);
            ai += 1;
        }
    }

    /// Emit a designated initializer for a pointer array field.
    /// Handles cases like `.a[1] = "abc"` where a is `char *a[3]`.
    /// The items may have field+index designators; we extract the index from the designators.
    fn emit_compound_ptr_array_designated_init(
        &mut self,
        elements: &mut Vec<GlobalInit>,
        items: &[&InitializerItem],
        elem_ty: &CType,
        arr_size: usize,
    ) {
        let ptr_size = 8; // 64-bit pointers
        // Build a sparse map of which indices are initialized
        let mut index_inits: Vec<Option<&Initializer>> = vec![None; arr_size];

        for item in items {
            // Find the Index designator (may be after a Field designator)
            let idx = item.designators.iter().find_map(|d| {
                if let Designator::Index(ref idx_expr) = d {
                    self.eval_const_expr(idx_expr).and_then(|c| c.to_usize())
                } else {
                    None
                }
            }).unwrap_or(0);

            if idx < arr_size {
                index_inits[idx] = Some(&item.init);
            }
        }

        // Emit each element
        for ai in 0..arr_size {
            if let Some(init) = index_inits[ai] {
                if let Initializer::Expr(ref expr) = init {
                    if let Expr::StringLiteral(s, _) = expr {
                        let label = self.intern_string_literal(s);
                        elements.push(GlobalInit::GlobalAddr(label));
                    } else if let Some(addr_init) = self.eval_global_addr_expr(expr) {
                        elements.push(addr_init);
                    } else if let Some(val) = self.eval_const_expr(expr) {
                        self.push_const_as_bytes(elements, &val, ptr_size);
                    } else {
                        push_zero_bytes(elements, ptr_size);
                    }
                } else {
                    push_zero_bytes(elements, ptr_size);
                }
            } else {
                // Uninitialized element - zero fill
                push_zero_bytes(elements, ptr_size);
            }
        }
    }

    /// Emit a flat array init for a field that has multiple items (flat init style).
    /// E.g., struct { char *s[2]; } x = { "abc", "def" };
    fn emit_compound_flat_array_init(
        &mut self,
        elements: &mut Vec<GlobalInit>,
        inits: &[&InitializerItem],
        field_ty: &CType,
        field_size: usize,
    ) {
        let (elem_ty, arr_size) = match field_ty {
            CType::Array(inner, Some(size)) => (inner.as_ref(), *size),
            _ => {
                // Not an array - just use first item
                if let Some(first) = inits.first() {
                    let field_is_pointer = matches!(field_ty, CType::Pointer(_));
                    self.emit_compound_field_init(elements, &first.init, field_ty, field_size, field_is_pointer);
                } else {
                    push_zero_bytes(elements, field_size);
                }
                return;
            }
        };

        let elem_is_pointer = Self::type_has_pointer_elements(elem_ty);
        let elem_size = elem_ty.size();
        let ptr_size = 8; // 64-bit

        let mut ai = 0usize;
        for item in inits {
            if ai >= arr_size { break; }

            if let Initializer::Expr(ref expr) = item.init {
                if elem_is_pointer {
                    if let Expr::StringLiteral(s, _) = expr {
                        let label = self.intern_string_literal(s);
                        elements.push(GlobalInit::GlobalAddr(label));
                    } else if let Some(addr_init) = self.eval_global_addr_expr(expr) {
                        elements.push(addr_init);
                    } else if let Some(val) = self.eval_const_expr(expr) {
                        self.push_const_as_bytes(elements, &val, ptr_size);
                    } else {
                        push_zero_bytes(elements, ptr_size);
                    }
                } else {
                    if let Some(val) = self.eval_const_expr(expr) {
                        self.push_const_as_bytes(elements, &val, elem_size);
                    } else {
                        push_zero_bytes(elements, elem_size);
                    }
                }
            } else {
                // Nested list - zero fill element
                push_zero_bytes(elements, elem_size);
            }
            ai += 1;
        }

        // Zero-fill remaining elements
        while ai < arr_size {
            push_zero_bytes(elements, elem_size);
            ai += 1;
        }
    }

    /// Push a constant value as individual bytes into a compound init element list.
    fn push_const_as_bytes(&self, elements: &mut Vec<GlobalInit>, val: &IrConst, size: usize) {
        let mut bytes = Vec::with_capacity(size);
        val.push_le_bytes(&mut bytes, size);
        for b in bytes {
            elements.push(GlobalInit::Scalar(IrConst::I8(b as i8)));
        }
    }

    /// Resolve which struct field a positional or designated initializer targets.
    fn resolve_struct_init_field_idx(
        &self,
        item: &InitializerItem,
        layout: &StructLayout,
        current_field_idx: usize,
    ) -> usize {
        let designator_name = match item.designators.first() {
            Some(Designator::Field(ref name)) => Some(name.as_str()),
            _ => None,
        };
        layout.resolve_init_field_idx(designator_name, current_field_idx)
            .unwrap_or(current_field_idx)
    }

    /// Write an initializer item to a byte buffer at the given offset.
    fn write_init_item_to_bytes(&self, bytes: &mut [u8], offset: usize, init: &Initializer, field_ty: &CType) {
        match init {
            Initializer::Expr(expr) => {
                let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                let field_ir_ty = IrType::from_ctype(field_ty);
                self.write_const_to_bytes(bytes, offset, &val, field_ir_ty);
            }
            Initializer::List(sub_items) => {
                // Try struct/union layout first
                if let Some(sub_layout) = self.get_struct_layout_for_ctype(field_ty) {
                    self.write_struct_init_to_bytes(bytes, offset, sub_items, &sub_layout);
                } else if let CType::Array(elem_ty, count) = field_ty {
                    let elem_size = elem_ty.size();
                    for (idx, sub_item) in sub_items.iter().enumerate() {
                        if count.map_or(false, |c| idx >= c) { break; }
                        if let Initializer::Expr(expr) = &sub_item.init {
                            let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                            let elem_ir_ty = IrType::from_ctype(elem_ty);
                            self.write_const_to_bytes(bytes, offset + idx * elem_size, &val, elem_ir_ty);
                        }
                    }
                } else {
                    // Scalar in braces
                    if let Some(first) = sub_items.first() {
                        if let Initializer::Expr(expr) = &first.init {
                            let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                            let field_ir_ty = IrType::from_ctype(field_ty);
                            self.write_const_to_bytes(bytes, offset, &val, field_ir_ty);
                        }
                    }
                }
            }
        }
    }

    /// Write an IrConst value to a byte buffer at the given offset using the field's IR type.
    fn write_const_to_bytes(&self, bytes: &mut [u8], offset: usize, val: &IrConst, ty: IrType) {
        let coerced = val.coerce_to(ty);
        let size = ty.size();
        let mut le_buf = Vec::with_capacity(size);
        coerced.push_le_bytes(&mut le_buf, size);
        for (i, &b) in le_buf.iter().enumerate() {
            if offset + i < bytes.len() {
                bytes[offset + i] = b;
            }
        }
    }

    /// Write a bitfield value into a byte buffer at the given offset.
    /// Uses read-modify-write to pack the value at the correct bit position.
    fn write_bitfield_to_bytes(&self, bytes: &mut [u8], offset: usize, val: &IrConst, ty: IrType, bit_offset: u32, bit_width: u32) {
        let int_val = val.to_u64().unwrap_or(0);

        let size = ty.size();
        let mask = if bit_width >= 64 { u64::MAX } else { (1u64 << bit_width) - 1 };
        let field_val = (int_val & mask) << bit_offset;
        let clear_mask = !(mask << bit_offset);

        // Read current storage unit value (little-endian)
        let mut current = 0u64;
        for i in 0..size {
            if offset + i < bytes.len() {
                current |= (bytes[offset + i] as u64) << (i * 8);
            }
        }

        // Modify: clear field bits and OR in new value
        let new_val = (current & clear_mask) | field_val;

        // Write back (little-endian)
        let le = new_val.to_le_bytes();
        for i in 0..size {
            if offset + i < bytes.len() {
                bytes[offset + i] = le[i];
            }
        }
    }

    /// Write a complex field ({real, imag} pair) to a byte buffer.
    /// Evaluates the initializer as a complex constant expression and writes
    /// both components at the correct offsets within the field.
    fn write_complex_field_to_bytes(
        &self,
        bytes: &mut [u8],
        field_offset: usize,
        complex_ctype: &CType,
        init: &Initializer,
    ) {
        let comp_size = match complex_ctype {
            CType::ComplexFloat => 4,
            CType::ComplexDouble => 8,
            CType::ComplexLongDouble => 16,
            _ => 8,
        };

        // Try to extract (real, imag) from the initializer
        let (real, imag) = match init {
            Initializer::Expr(expr) => {
                self.eval_complex_const_public(expr).unwrap_or((0.0, 0.0))
            }
            Initializer::List(items) => {
                // {real, imag} or {real} (imag defaults to 0)
                let real_val = items.first().and_then(|item| {
                    if let Initializer::Expr(e) = &item.init {
                        self.eval_const_expr(e).and_then(|c| match c {
                            IrConst::F64(v) => Some(v),
                            IrConst::F32(v) => Some(v as f64),
                            IrConst::I64(v) => Some(v as f64),
                            IrConst::I32(v) => Some(v as f64),
                            IrConst::LongDouble(v) => Some(v),
                            _ => None,
                        })
                    } else {
                        None
                    }
                }).unwrap_or(0.0);
                let imag_val = items.get(1).and_then(|item| {
                    if let Initializer::Expr(e) = &item.init {
                        self.eval_const_expr(e).and_then(|c| match c {
                            IrConst::F64(v) => Some(v),
                            IrConst::F32(v) => Some(v as f64),
                            IrConst::I64(v) => Some(v as f64),
                            IrConst::I32(v) => Some(v as f64),
                            IrConst::LongDouble(v) => Some(v),
                            _ => None,
                        })
                    } else {
                        None
                    }
                }).unwrap_or(0.0);
                (real_val, imag_val)
            }
        };

        // Write real part at field_offset
        match complex_ctype {
            CType::ComplexFloat => {
                let real_const = IrConst::F32(real as f32);
                let imag_const = IrConst::F32(imag as f32);
                self.write_const_to_bytes(bytes, field_offset, &real_const, IrType::F32);
                self.write_const_to_bytes(bytes, field_offset + comp_size, &imag_const, IrType::F32);
            }
            CType::ComplexLongDouble => {
                let real_const = IrConst::LongDouble(real);
                let imag_const = IrConst::LongDouble(imag);
                self.write_const_to_bytes(bytes, field_offset, &real_const, IrType::F128);
                self.write_const_to_bytes(bytes, field_offset + comp_size, &imag_const, IrType::F128);
            }
            _ => {
                // ComplexDouble
                let real_const = IrConst::F64(real);
                let imag_const = IrConst::F64(imag);
                self.write_const_to_bytes(bytes, field_offset, &real_const, IrType::F64);
                self.write_const_to_bytes(bytes, field_offset + comp_size, &imag_const, IrType::F64);
            }
        }
    }

    /// Write a struct initializer list to a byte buffer at the given base offset.
    /// Handles nested struct fields recursively.
    fn write_struct_init_to_bytes(
        &self,
        bytes: &mut [u8],
        base_offset: usize,
        items: &[InitializerItem],
        layout: &StructLayout,
    ) {
        let mut current_field_idx = 0usize;
        for item in items {
            let designator_name = match item.designators.first() {
                Some(Designator::Field(ref name)) => Some(name.as_str()),
                _ => None,
            };
            let field_idx = match layout.resolve_init_field_idx(designator_name, current_field_idx) {
                Some(idx) => idx,
                None => break,
            };

            let field_layout = &layout.fields[field_idx];
            let field_offset = base_offset + field_layout.offset;

            // Handle nested designators (e.g., .a.j = 2): drill into sub-struct
            let has_nested_designator = item.designators.len() > 1
                && matches!(item.designators.first(), Some(Designator::Field(_)));
            if has_nested_designator {
                if let Some(sub_layout) = self.get_struct_layout_for_ctype(&field_layout.ty) {
                    let sub_item = InitializerItem {
                        designators: item.designators[1..].to_vec(),
                        init: item.init.clone(),
                    };
                    self.write_struct_init_to_bytes(bytes, field_offset, &[sub_item], &sub_layout);
                }
                current_field_idx = field_idx + 1;
                continue;
            }

            // Complex fields: use specialized handler for {real, imag} pair
            if field_layout.ty.is_complex() {
                self.write_complex_field_to_bytes(bytes, field_offset, &field_layout.ty, &item.init);
                current_field_idx = field_idx + 1;
                continue;
            }

            match &item.init {
                Initializer::Expr(expr) => {
                    if let Expr::StringLiteral(s, _) = expr {
                        // String literal initializing a char array field
                        if let CType::Array(_, Some(arr_size)) = &field_layout.ty {
                            let s_bytes = s.as_bytes();
                            for (i, &b) in s_bytes.iter().enumerate() {
                                if i >= *arr_size { break; }
                                if field_offset + i < bytes.len() {
                                    bytes[field_offset + i] = b;
                                }
                            }
                            if s_bytes.len() < *arr_size && field_offset + s_bytes.len() < bytes.len() {
                                bytes[field_offset + s_bytes.len()] = 0;
                            }
                        }
                    } else {
                        let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                        let field_ir_ty = IrType::from_ctype(&field_layout.ty);
                        if let (Some(bit_offset), Some(bit_width)) = (field_layout.bit_offset, field_layout.bit_width) {
                            self.write_bitfield_to_bytes(bytes, field_offset, &val, field_ir_ty, bit_offset, bit_width);
                        } else {
                            self.write_const_to_bytes(bytes, field_offset, &val, field_ir_ty);
                        }
                    }
                }
                Initializer::List(sub_items) => {
                    // Nested struct/union: recurse if the field is a struct type
                    if let Some(sub_layout) = self.get_struct_layout_for_ctype(&field_layout.ty) {
                        self.write_struct_init_to_bytes(bytes, field_offset, sub_items, &sub_layout);
                    } else {
                        // Array field or other nested init: try to write elements sequentially
                        if let CType::Array(ref elem_ty, _) = field_layout.ty {
                            let elem_ir_ty = IrType::from_ctype(elem_ty);
                            let elem_size = elem_ty.size();
                            for (i, sub_item) in sub_items.iter().enumerate() {
                                if let Initializer::Expr(expr) = &sub_item.init {
                                    let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                                    self.write_const_to_bytes(bytes, field_offset + i * elem_size, &val, elem_ir_ty);
                                }
                            }
                        } else if let Some(first) = sub_items.first() {
                            if let Initializer::Expr(expr) = &first.init {
                                let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                                let field_ir_ty = IrType::from_ctype(&field_layout.ty);
                                self.write_const_to_bytes(bytes, field_offset, &val, field_ir_ty);
                            }
                        }
                    }
                }
            }

            current_field_idx = field_idx + 1;

            // For unions with flat init, only the first member can be initialized.
            // Designated inits can overwrite (last designator wins).
            if layout.is_union && designator_name.is_none() {
                break;
            }
        }
    }

    /// Resolve a pointer field's initializer expression to a GlobalInit.
    /// Handles string literals, global addresses, function pointers, etc.
    fn resolve_ptr_field_init(&mut self, expr: &Expr) -> Option<GlobalInit> {
        // String literal: create a .rodata entry and reference it
        if let Expr::StringLiteral(s, _) = expr {
            let label = self.intern_string_literal(s);
            return Some(GlobalInit::GlobalAddr(label));
        }
        // Try as a global address expression (&x, func name, array name, etc.)
        if let Some(addr) = self.eval_global_addr_expr(expr) {
            return Some(addr);
        }
        // Integer constant 0 -> null pointer
        if let Some(val) = self.eval_const_expr(expr) {
            if let Some(v) = self.const_to_i64(&val) {
                if v == 0 {
                    return None; // Will be zero in the byte buffer
                }
            }
            // Non-zero constant pointer (unusual but possible)
            return Some(GlobalInit::Scalar(val));
        }
        None
    }

    /// Get a StructLayout for a CType if it's a struct or union.
    fn get_struct_layout_for_ctype(&self, ty: &CType) -> Option<StructLayout> {
        match ty {
            CType::Struct(st) => {
                // First try to look up by name in registered struct_layouts
                if let Some(ref name) = st.name {
                    let key = format!("struct.{}", name);
                    if let Some(layout) = self.struct_layouts.get(&key) {
                        return Some(layout.clone());
                    }
                }
                // Compute from fields
                Some(StructLayout::for_struct(&st.fields))
            }
            CType::Union(st) => {
                if let Some(ref name) = st.name {
                    let key = format!("union.{}", name);
                    if let Some(layout) = self.struct_layouts.get(&key) {
                        return Some(layout.clone());
                    }
                }
                Some(StructLayout::for_union(&st.fields))
            }
            _ => None,
        }
    }

    /// Lower an array of structs where some fields are pointers.
    /// Uses byte-level serialization but with Compound for address elements.
    /// Each struct element is emitted as a mix of byte constants and pointer-sized addresses.
    fn lower_struct_array_with_ptrs(
        &mut self,
        items: &[InitializerItem],
        layout: &StructLayout,
        num_elems: usize,
    ) -> GlobalInit {
        // We emit the entire array as a sequence of Compound elements.
        // Each element is either:
        //   - Scalar(I8) for byte-level constant data
        //   - GlobalAddr for pointer fields
        let struct_size = layout.size;
        let total_size = num_elems * struct_size;
        let mut compound_elements: Vec<GlobalInit> = Vec::new();

        // Initialize with zero bytes
        let mut bytes = vec![0u8; total_size];
        // Track which byte ranges are pointer fields that need address relocations
        let mut ptr_ranges: Vec<(usize, GlobalInit)> = Vec::new(); // (byte_offset, addr_init)

        for elem_idx in 0..num_elems {
            let item = items.get(elem_idx);
            let base_offset = elem_idx * struct_size;

            if let Some(item) = item {
                match &item.init {
                    Initializer::List(sub_items) => {
                        let mut current_field_idx = 0usize;
                        for sub_item in sub_items {
                            let desig_name = match sub_item.designators.first() {
                                Some(Designator::Field(ref name)) => Some(name.as_str()),
                                _ => None,
                            };
                            let field_idx = match layout.resolve_init_field_idx(desig_name, current_field_idx) {
                                Some(idx) => idx,
                                None => break,
                            };
                            let field = &layout.fields[field_idx];
                            let field_offset = base_offset + field.offset;
                            let field_ir_ty = IrType::from_ctype(&field.ty);
                            let is_ptr_field = matches!(field.ty, CType::Pointer(_) | CType::Function(_));

                            if let Initializer::Expr(expr) = &sub_item.init {
                                if is_ptr_field {
                                    if let Some(addr_init) = self.resolve_ptr_field_init(expr) {
                                        ptr_ranges.push((field_offset, addr_init));
                                    } else if let Some(val) = self.eval_const_expr(expr) {
                                        self.write_const_to_bytes(&mut bytes, field_offset, &val, field_ir_ty);
                                    }
                                } else if let (Some(bit_offset), Some(bit_width)) = (field.bit_offset, field.bit_width) {
                                    // Bitfield: use read-modify-write to pack into storage unit
                                    let val = self.eval_init_scalar(&sub_item.init);
                                    self.write_bitfield_to_bytes(&mut bytes, field_offset, &val, field_ir_ty, bit_offset, bit_width);
                                } else {
                                    if let Some(val) = self.eval_const_expr(expr) {
                                        self.write_const_to_bytes(&mut bytes, field_offset, &val, field_ir_ty);
                                    }
                                }
                            }
                            current_field_idx = field_idx + 1;
                        }
                    }
                    Initializer::Expr(expr) => {
                        // Single expression for first field
                        if !layout.fields.is_empty() {
                            let field = &layout.fields[0];
                            let field_offset = base_offset + field.offset;
                            let field_ir_ty = IrType::from_ctype(&field.ty);
                            let is_ptr_field = matches!(field.ty, CType::Pointer(_) | CType::Function(_));
                            if is_ptr_field {
                                if let Some(addr_init) = self.resolve_ptr_field_init(expr) {
                                    ptr_ranges.push((field_offset, addr_init));
                                }
                            } else if field.bit_offset.is_some() {
                                if let Some(val) = self.eval_const_expr(expr) {
                                    let bit_offset = field.bit_offset.unwrap();
                                    let bit_width = field.bit_width.unwrap();
                                    self.write_bitfield_to_bytes(&mut bytes, field_offset, &val, field_ir_ty, bit_offset, bit_width);
                                }
                            } else if let Some(val) = self.eval_const_expr(expr) {
                                self.write_const_to_bytes(&mut bytes, field_offset, &val, field_ir_ty);
                            }
                        }
                    }
                }
            }
        }

        // Sort ptr_ranges by offset
        ptr_ranges.sort_by_key(|&(off, _)| off);

        // Emit the byte stream, replacing pointer-sized regions with GlobalAddr elements
        let mut pos = 0;
        for (ptr_off, ref addr_init) in &ptr_ranges {
            // Emit bytes before this pointer
            while pos < *ptr_off {
                compound_elements.push(GlobalInit::Scalar(IrConst::I8(bytes[pos] as i8)));
                pos += 1;
            }
            // Emit the pointer reference
            compound_elements.push(addr_init.clone());
            pos += 8; // pointer is 8 bytes
        }
        // Emit remaining bytes
        while pos < total_size {
            compound_elements.push(GlobalInit::Scalar(IrConst::I8(bytes[pos] as i8)));
            pos += 1;
        }

        GlobalInit::Compound(compound_elements)
    }

    // --- Array flattening helpers ---

    /// Flatten a multi-dimensional array initializer list for global arrays.
    /// Handles C initialization rules:
    /// - Braced sub-lists map to the next sub-array dimension, padded with zeros
    /// - Bare scalar expressions fill base elements left-to-right without sub-array padding
    fn flatten_global_array_init(
        &self,
        items: &[InitializerItem],
        array_dim_strides: &[usize],
        base_ty: IrType,
        values: &mut Vec<IrConst>,
    ) {
        let base_type_size = base_ty.size().max(1);
        if array_dim_strides.len() <= 1 {
            for item in items {
                self.flatten_global_init_item(&item.init, base_ty, values);
            }
            return;
        }
        // Number of base elements per sub-array at this dimension level
        let sub_elem_count = if array_dim_strides[0] > 0 && base_type_size > 0 {
            array_dim_strides[0] / base_type_size
        } else {
            1
        };
        for item in items {
            match &item.init {
                Initializer::List(sub_items) => {
                    // Check for braced string literal initializing a char sub-array: { "abc" }
                    // This is common in patterns like: unsigned char arr[3][4] = { {"abc"}, {"def"} };
                    if sub_items.len() == 1 {
                        if let Initializer::Expr(Expr::StringLiteral(s, _)) = &sub_items[0].init {
                            let start_len = values.len();
                            for &byte in s.as_bytes() {
                                values.push(IrConst::I64(byte as i64));
                            }
                            // Add null terminator if room
                            if values.len() < start_len + sub_elem_count {
                                values.push(IrConst::I64(0));
                            }
                            // Pad to sub_elem_count
                            while values.len() < start_len + sub_elem_count {
                                values.push(self.zero_const(base_ty));
                            }
                            // Truncate if string was too long for the sub-array
                            if values.len() > start_len + sub_elem_count {
                                values.truncate(start_len + sub_elem_count);
                            }
                            continue;
                        }
                    }
                    // Braced sub-list: recurse into next dimension, then pad to sub_elem_count
                    let start_len = values.len();
                    self.flatten_global_array_init(sub_items, &array_dim_strides[1..], base_ty, values);
                    while values.len() < start_len + sub_elem_count {
                        values.push(self.zero_const(base_ty));
                    }
                }
                Initializer::Expr(expr) => {
                    if let Expr::StringLiteral(s, _) = expr {
                        // String literal initializing a char sub-array: inline the bytes
                        let start_len = values.len();
                        for &byte in s.as_bytes() {
                            values.push(IrConst::I64(byte as i64));
                        }
                        // Add null terminator if room
                        if values.len() < start_len + sub_elem_count {
                            values.push(IrConst::I64(0));
                        }
                        // Pad to sub_elem_count
                        while values.len() < start_len + sub_elem_count {
                            values.push(self.zero_const(base_ty));
                        }
                        // Truncate if string was too long for the sub-array
                        if values.len() > start_len + sub_elem_count {
                            values.truncate(start_len + sub_elem_count);
                        }
                    } else if let Some(val) = self.eval_const_expr(expr) {
                        // Bare scalar: fills one base element, no sub-array padding
                        let expr_ty = self.get_expr_type(expr);
                        values.push(self.coerce_const_to_type_with_src(val, base_ty, expr_ty));
                    } else {
                        values.push(self.zero_const(base_ty));
                    }
                }
            }
        }
    }

    /// Check if an initializer item contains an address expression or string literal,
    /// recursing into nested Initializer::List items (for double-brace init like {{"str"}}).
    fn init_contains_addr_expr(item: &InitializerItem, is_multidim_char_array: bool) -> bool {
        match &item.init {
            Initializer::Expr(expr) => {
                if matches!(expr, Expr::StringLiteral(_, _)) {
                    !is_multidim_char_array
                } else {
                    // Cannot call self methods in a static context, so conservatively
                    // return false for non-string expressions; the caller handles
                    // address expressions via eval_global_addr_expr separately.
                    false
                }
            }
            Initializer::List(sub_items) => {
                sub_items.iter().any(|sub| Self::init_contains_addr_expr(sub, is_multidim_char_array))
            }
        }
    }

    /// Check if an initializer item contains a string literal anywhere (including nested lists).
    fn init_contains_string_literal(item: &InitializerItem) -> bool {
        match &item.init {
            Initializer::Expr(expr) => matches!(expr, Expr::StringLiteral(_, _)),
            Initializer::List(sub_items) => {
                sub_items.iter().any(|sub| Self::init_contains_string_literal(sub))
            }
        }
    }

    /// Collect a single compound initializer element, handling nested lists (double-brace init).
    /// For `{{"str"}}`, unwraps the nested list to find the string literal.
    /// For `{"str"}`, directly handles the string literal expression.
    fn collect_compound_init_element(&mut self, init: &Initializer, elements: &mut Vec<GlobalInit>) {
        match init {
            Initializer::Expr(expr) => {
                if let Expr::StringLiteral(s, _) = expr {
                    // String literal: create .rodata entry and use its label
                    let label = self.intern_string_literal(s);
                    elements.push(GlobalInit::GlobalAddr(label));
                } else if let Expr::LabelAddr(label_name, _) = expr {
                    // GCC &&label extension: emit label address as GlobalAddr
                    let scoped_label = self.get_or_create_user_label(label_name);
                    elements.push(GlobalInit::GlobalAddr(scoped_label));
                } else if let Some(val) = self.eval_const_expr(expr) {
                    elements.push(GlobalInit::Scalar(val));
                } else if let Some(addr) = self.eval_global_addr_expr(expr) {
                    elements.push(addr);
                } else {
                    elements.push(GlobalInit::Zero);
                }
            }
            Initializer::List(sub_items) => {
                // Double-brace init like {{"str"}} or {{expr}}:
                // Unwrap the nested list and process the first element as the value
                // for this array slot.
                if sub_items.len() >= 1 {
                    self.collect_compound_init_element(&sub_items[0].init, elements);
                } else {
                    elements.push(GlobalInit::Zero);
                }
            }
        }
    }

    /// Flatten a single initializer item, recursing into nested lists.
    fn flatten_global_init_item(&self, init: &Initializer, base_ty: IrType, values: &mut Vec<IrConst>) {
        match init {
            Initializer::Expr(expr) => {
                if let Expr::StringLiteral(s, _) = expr {
                    // String literal initializing a char array element: inline bytes
                    for &byte in s.as_bytes() {
                        values.push(IrConst::I64(byte as i64));
                    }
                    // Add null terminator
                    values.push(IrConst::I64(0));
                } else if let Some(val) = self.eval_const_expr(expr) {
                    let expr_ty = self.get_expr_type(expr);
                    values.push(self.coerce_const_to_type_with_src(val, base_ty, expr_ty));
                } else {
                    values.push(self.zero_const(base_ty));
                }
            }
            Initializer::List(items) => {
                for item in items {
                    self.flatten_global_init_item(&item.init, base_ty, values);
                }
            }
        }
    }
}
