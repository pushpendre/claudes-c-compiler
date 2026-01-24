//! Compound (relocation-aware) global initialization.
//!
//! This module handles global variable initialization where fields may contain
//! address expressions requiring relocations. Unlike simple byte-level
//! initialization, compound initialization emits a mix of scalar byte constants
//! and `GlobalAddr` entries that represent addresses resolved at link time.
//!
//! Struct, union, and array initializers with pointer fields (e.g., string
//! literals, function pointers, address-of expressions) are lowered here using
//! `GlobalInit::Compound`, which preserves relocation information for the linker.

use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, StructLayout, CType};
use super::lowering::Lowerer;
use super::global_init_bytes::push_zero_bytes;

impl Lowerer {
    /// Lower a struct global init that contains address expressions.
    /// Emits field-by-field using Compound, with padding bytes between fields.
    /// Handles flat init (where multiple items fill an array-of-pointer field),
    /// braced init, and designated init patterns.
    pub(super) fn lower_struct_global_init_compound(
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

            // Check if this is a flat init filling an array field.
            // A string literal initializing a char array is NOT flat init - it's a single
            // complete initializer for the entire array (e.g., char c[10] = "hello").
            let is_string_literal = matches!(&item.init, Initializer::Expr(Expr::StringLiteral(..)));
            if let CType::Array(_, Some(arr_size)) = field_ty {
                if matches!(&item.init, Initializer::Expr(_)) && !is_string_literal {
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

        // For unions, find the one initialized field and emit only that,
        // padding to the full union size. Non-union structs emit all fields.
        if layout.is_union {
            // Find which field (if any) has an initializer
            let mut init_fi = None;
            for (i, inits) in field_inits.iter().enumerate() {
                if !inits.is_empty() {
                    init_fi = Some(i);
                    break;
                }
            }
            let union_size = layout.size;
            if let Some(fi) = init_fi {
                let field_size = layout.fields[fi].ty.size();
                let field_is_pointer = matches!(layout.fields[fi].ty, CType::Pointer(_) | CType::Function(_));
                let inits = &field_inits[fi];
                if inits.len() == 1 {
                    let item = inits[0];
                    let has_nested_designator = item.designators.len() > 1
                        && matches!(item.designators.first(), Some(Designator::Field(_)));
                    if has_nested_designator {
                        self.emit_compound_nested_designator_init(
                            &mut elements, item, &layout.fields[fi].ty, field_size);
                    } else {
                        self.emit_compound_field_init(&mut elements, &item.init, &layout.fields[fi].ty, field_size, field_is_pointer);
                    }
                } else if inits.len() > 1 {
                    self.emit_compound_flat_array_init(&mut elements, inits, &layout.fields[fi].ty, field_size);
                } else {
                    push_zero_bytes(&mut elements, field_size);
                }
                // Pad to full union size
                if field_size < union_size {
                    push_zero_bytes(&mut elements, union_size - field_size);
                }
                current_offset = union_size;
            } else {
                // No initialized field - zero fill entire union
                push_zero_bytes(&mut elements, union_size);
                current_offset = union_size;
            }
        } else {

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

                // Emit padding before this storage unit (only if unit starts after current pos)
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

                // When the storage unit overlaps with already-written data
                // (due to align_down placement), skip the overlapping bytes.
                let skip = if current_offset > storage_unit_offset {
                    current_offset - storage_unit_offset
                } else {
                    0
                };
                // Emit only the non-overlapping portion of the storage unit
                for &b in &unit_bytes[skip..] {
                    elements.push(GlobalInit::Scalar(IrConst::I8(b as i8)));
                }
                current_offset = storage_unit_offset + storage_unit_size;
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

                // When the storage unit overlaps with already-written data
                // (due to align_down placement), skip the overlapping bytes.
                let skip = if current_offset > storage_offset {
                    current_offset - storage_offset
                } else {
                    0
                };
                // Emit only the non-overlapping portion of the storage unit
                let le = packed_val.to_le_bytes();
                for i in skip..storage_size {
                    elements.push(GlobalInit::Scalar(IrConst::I8(le[i] as i8)));
                }
                current_offset = storage_offset + storage_size;
                continue;
            }

            let inits = &field_inits[fi];
            if inits.is_empty() {
                // No initializer for this field - zero fill
                push_zero_bytes(&mut elements, field_size);
            } else if inits.len() == 1 {
                let item = inits[0];

                // Check for nested designators: e.g., .u.keyword={"hello", -5}
                // where item.designators = [Field("u"), Field("keyword")]
                // The first designator was used to resolve the field index (fi),
                // but remaining designators must drill into the sub-composite.
                let has_nested_designator = item.designators.len() > 1
                    && matches!(item.designators.first(), Some(Designator::Field(_)));

                if has_nested_designator {
                    self.emit_compound_nested_designator_init(
                        &mut elements, item, &layout.fields[fi].ty, field_size);
                } else {
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
                }
            } else {
                // Multiple items for this field (flat array init)
                self.emit_compound_flat_array_init(&mut elements, inits, &layout.fields[fi].ty, field_size);
            }
            current_offset += field_size;
            fi += 1;
        }

        } // end of else (non-union struct path)

        // Trailing padding
        while current_offset < total_size {
            elements.push(GlobalInit::Scalar(IrConst::I8(0)));
            current_offset += 1;
        }

        GlobalInit::Compound(elements)
    }

    /// Emit a single field initializer in compound (relocation-aware) mode.
    pub(super) fn emit_compound_field_init(
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
                        // Use chars() to get raw byte values (each char is U+0000..U+00FF)
                        let s_chars: Vec<u8> = s.chars().map(|c| c as u8).collect();
                        for (i, &b) in s_chars.iter().enumerate() {
                            if i >= field_size { break; }
                            elements.push(GlobalInit::Scalar(IrConst::I8(b as i8)));
                        }
                        // null terminator + remaining zero fill
                        for _ in s_chars.len()..field_size {
                            elements.push(GlobalInit::Scalar(IrConst::I8(0)));
                        }
                    }
                } else if let Some(val) = self.eval_const_expr(expr) {
                    // Scalar constant - coerce to field type and emit as bytes
                    let field_ir_ty = IrType::from_ctype(field_ty);
                    let coerced = val.coerce_to(field_ir_ty);
                    self.push_const_as_bytes(elements, &coerced, field_size);
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

    /// Handle a nested designator in compound mode by drilling into the sub-composite.
    ///
    /// For example, given `struct S { union U u; } x = {.u.keyword={"hello", -5}};`,
    /// the item has designators [Field("u"), Field("keyword")] and init = List(["hello", -5]).
    /// The first designator was already consumed to resolve `u` as the target field.
    /// This function strips the first designator and recursively initializes the
    /// sub-composite (struct/union) field with the remaining designators.
    fn emit_compound_nested_designator_init(
        &mut self,
        elements: &mut Vec<GlobalInit>,
        item: &InitializerItem,
        field_ty: &CType,
        field_size: usize,
    ) {
        // Build a sub-item with the remaining designators (strip the first one)
        let sub_item = InitializerItem {
            designators: item.designators[1..].to_vec(),
            init: item.init.clone(),
        };

        // Try to get the sub-composite layout for this field
        let field_ty_clone = field_ty.clone();
        if let Some(sub_layout) = self.get_struct_layout_for_ctype(&field_ty_clone) {
            // Check if the sub-init has address fields that need compound handling
            let sub_items = vec![sub_item];
            if self.struct_init_has_addr_fields(&sub_items, &sub_layout) {
                // Recursively handle as compound
                let nested = self.lower_struct_global_init_compound(&sub_items, &sub_layout);
                if let GlobalInit::Compound(nested_elems) = nested {
                    // Pad/truncate to field_size
                    let mut emitted = 0;
                    for elem in nested_elems {
                        if emitted >= field_size { break; }
                        let elem_size = match &elem {
                            GlobalInit::Scalar(_) => 1,
                            GlobalInit::GlobalAddr(_) | GlobalInit::GlobalAddrOffset(_, _) => 8,
                            GlobalInit::Compound(inner) => inner.len(),
                            GlobalInit::Array(vals) => vals.len(),
                            GlobalInit::Zero => 0,
                            GlobalInit::String(s) => s.len(),
                            GlobalInit::WideString(ws) => ws.len() * 4,
                        };
                        elements.push(elem);
                        emitted += elem_size;
                    }
                    // Zero-fill any remaining bytes
                    while emitted < field_size {
                        elements.push(GlobalInit::Scalar(IrConst::I8(0)));
                        emitted += 1;
                    }
                } else {
                    push_zero_bytes(elements, field_size);
                }
            } else {
                // No address fields - use byte-level approach
                let mut bytes = vec![0u8; field_size];
                self.fill_struct_global_bytes(&sub_items, &sub_layout, &mut bytes, 0);
                for b in &bytes {
                    elements.push(GlobalInit::Scalar(IrConst::I8(*b as i8)));
                }
            }
        } else {
            // Not a struct/union - shouldn't have nested designators, but handle gracefully
            let field_is_pointer = matches!(field_ty, CType::Pointer(_) | CType::Function(_));
            self.emit_compound_field_init(elements, &item.init, field_ty, field_size, field_is_pointer);
        }
    }

    /// Emit an array-of-pointers field from a braced initializer list.
    /// Each element is either a GlobalAddr (for string literals / address expressions)
    /// or zero-filled (for missing elements).
    pub(super) fn emit_compound_ptr_array_init(
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
    pub(super) fn emit_compound_ptr_array_designated_init(
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
    pub(super) fn emit_compound_flat_array_init(
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
                        let elem_ir_ty = IrType::from_ctype(elem_ty);
                        let coerced = val.coerce_to(elem_ir_ty);
                        self.push_const_as_bytes(elements, &coerced, elem_size);
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

    /// Collect pointer relocations from a nested designator init item.
    /// Drills into the sub-composite using the item's designators to find pointer fields
    /// and resolves their address expressions for proper relocation tracking.
    fn collect_ptr_ranges_from_nested_init(
        &mut self,
        item: &InitializerItem,
        layout: &StructLayout,
        base_offset: usize,
        ptr_ranges: &mut Vec<(usize, GlobalInit)>,
    ) {
        // Resolve which field this designator targets
        let field_idx = self.resolve_struct_init_field_idx(item, layout, 0);
        if field_idx >= layout.fields.len() { return; }

        let field = &layout.fields[field_idx];
        let field_offset = base_offset + field.offset;

        // If there are more nested designators, recurse
        if item.designators.len() > 1 && matches!(item.designators.first(), Some(Designator::Field(_))) {
            let sub_item = InitializerItem {
                designators: item.designators[1..].to_vec(),
                init: item.init.clone(),
            };
            if let Some(sub_layout) = self.get_struct_layout_for_ctype(&field.ty) {
                self.collect_ptr_ranges_from_nested_init(&sub_item, &sub_layout, field_offset, ptr_ranges);
            }
            return;
        }

        // Terminal designator: check if this field or its sub-fields contain pointers
        let is_ptr_field = matches!(field.ty, CType::Pointer(_) | CType::Function(_));
        if is_ptr_field {
            if let Initializer::Expr(ref expr) = item.init {
                if let Some(addr_init) = self.resolve_ptr_field_init(expr) {
                    ptr_ranges.push((field_offset, addr_init));
                }
            }
        } else if let Initializer::List(ref sub_items) = item.init {
            // The init is a list for a struct/union - scan its fields for pointers
            if let Some(sub_layout) = self.get_struct_layout_for_ctype(&field.ty) {
                self.collect_ptr_ranges_from_struct_init_list(
                    sub_items, &sub_layout, field_offset, ptr_ranges);
            }
        }
    }

    /// Scan a braced init list for a struct and collect pointer/function-pointer
    /// relocations. This handles the case where a nested struct field is initialized
    /// with `{ .fn1 = hello, .fn2 = world }` and needs address relocations collected
    /// for its pointer-typed sub-fields. Recurses into further nested structs.
    fn collect_ptr_ranges_from_struct_init_list(
        &mut self,
        items: &[InitializerItem],
        layout: &StructLayout,
        base_offset: usize,
        ptr_ranges: &mut Vec<(usize, GlobalInit)>,
    ) {
        let mut current_fi = 0usize;
        for si in items {
            // Handle nested designators (e.g., .config.i = &server.field)
            // by following the designator chain to the actual target field
            let has_nested_desig = si.designators.len() > 1
                && matches!(si.designators.first(), Some(Designator::Field(_)));
            if has_nested_desig {
                self.collect_ptr_ranges_from_nested_init(si, layout, base_offset, ptr_ranges);
                let si_fi = self.resolve_struct_init_field_idx(si, layout, current_fi);
                current_fi = si_fi + 1;
                continue;
            }

            let si_fi = self.resolve_struct_init_field_idx(si, layout, current_fi);
            if si_fi >= layout.fields.len() { continue; }
            let si_field = &layout.fields[si_fi];
            let si_offset = base_offset + si_field.offset;
            let si_is_ptr = matches!(si_field.ty, CType::Pointer(_) | CType::Function(_));

            if si_is_ptr {
                if let Initializer::Expr(ref expr) = si.init {
                    if let Some(addr_init) = self.resolve_ptr_field_init(expr) {
                        ptr_ranges.push((si_offset, addr_init));
                    }
                }
            } else if let Initializer::List(ref nested_items) = si.init {
                // Recurse into nested structs/unions that may also contain pointers
                if let Some(sub_layout) = self.get_struct_layout_for_ctype(&si_field.ty) {
                    self.collect_ptr_ranges_from_struct_init_list(
                        nested_items, &sub_layout, si_offset, ptr_ranges);
                }
                // Handle arrays of pointers within nested struct
                if Self::type_has_pointer_elements(&si_field.ty) && matches!(si_field.ty, CType::Array(..)) {
                    let arr_size = match &si_field.ty {
                        CType::Array(_, Some(s)) => *s,
                        _ => nested_items.len(),
                    };
                    for (ai, inner_item) in nested_items.iter().enumerate() {
                        if ai >= arr_size { break; }
                        let elem_offset = si_offset + ai * 8;
                        if let Initializer::Expr(ref expr) = inner_item.init {
                            if let Some(addr_init) = self.resolve_ptr_field_init(expr) {
                                ptr_ranges.push((elem_offset, addr_init));
                            }
                        }
                    }
                }
            }
            current_fi = si_fi + 1;
        }
    }

    /// Resolve which struct field a positional or designated initializer targets.
    pub(super) fn resolve_struct_init_field_idx(
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

    /// Resolve a pointer field's initializer expression to a GlobalInit.
    /// Handles string literals, global addresses, function pointers, etc.
    pub(super) fn resolve_ptr_field_init(&mut self, expr: &Expr) -> Option<GlobalInit> {
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
    pub(super) fn get_struct_layout_for_ctype(&self, ty: &CType) -> Option<StructLayout> {
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
    pub(super) fn lower_struct_array_with_ptrs(
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
                            let is_ptr_array = Self::type_has_pointer_elements(&field.ty)
                                && matches!(field.ty, CType::Array(..));

                            // Handle nested designators: e.g., .bs.keyword={"GET", -1}
                            // Strip first designator and drill into sub-composite
                            let has_nested_designator = sub_item.designators.len() > 1
                                && matches!(sub_item.designators.first(), Some(Designator::Field(_)));

                            if has_nested_designator {
                                let nested_sub_item = InitializerItem {
                                    designators: sub_item.designators[1..].to_vec(),
                                    init: sub_item.init.clone(),
                                };
                                if let Some(sub_layout) = self.get_struct_layout_for_ctype(&field.ty) {
                                    // Use fill_struct_global_bytes for byte-level data
                                    self.fill_struct_global_bytes(
                                        &[nested_sub_item.clone()], &sub_layout, &mut bytes, field_offset);
                                    // Also check for pointer fields that need relocations
                                    self.collect_ptr_ranges_from_nested_init(
                                        &nested_sub_item, &sub_layout, field_offset, &mut ptr_ranges);
                                }
                                current_field_idx = field_idx + 1;
                                continue;
                            }

                            if let Initializer::Expr(expr) = &sub_item.init {
                                if is_ptr_field {
                                    if let Some(addr_init) = self.resolve_ptr_field_init(expr) {
                                        ptr_ranges.push((field_offset, addr_init));
                                    } else if let Some(val) = self.eval_const_expr(expr) {
                                        self.write_const_to_bytes(&mut bytes, field_offset, &val, field_ir_ty);
                                    }
                                } else if is_ptr_array {
                                    // Single expr for a pointer array field (flat init start)
                                    // Resolve as address for the first element
                                    if let Some(addr_init) = self.resolve_ptr_field_init(expr) {
                                        ptr_ranges.push((field_offset, addr_init));
                                    } else if let Some(val) = self.eval_const_expr(expr) {
                                        self.write_const_to_bytes(&mut bytes, field_offset, &val, IrType::I64);
                                    }
                                } else if let (Some(bit_offset), Some(bit_width)) = (field.bit_offset, field.bit_width) {
                                    let val = self.eval_init_scalar(&sub_item.init);
                                    self.write_bitfield_to_bytes(&mut bytes, field_offset, &val, field_ir_ty, bit_offset, bit_width);
                                } else {
                                    if let Some(val) = self.eval_const_expr(expr) {
                                        self.write_const_to_bytes(&mut bytes, field_offset, &val, field_ir_ty);
                                    }
                                }
                            } else if let Initializer::List(ref inner_items) = sub_item.init {
                                if is_ptr_array {
                                    // Array-of-pointers field with braced init: {f1, f2, ...}
                                    let arr_size = match &field.ty {
                                        CType::Array(_, Some(s)) => *s,
                                        _ => inner_items.len(),
                                    };
                                    for (ai, inner_item) in inner_items.iter().enumerate() {
                                        if ai >= arr_size { break; }
                                        let elem_offset = field_offset + ai * 8;
                                        if let Initializer::Expr(ref expr) = inner_item.init {
                                            if let Some(addr_init) = self.resolve_ptr_field_init(expr) {
                                                ptr_ranges.push((elem_offset, addr_init));
                                            } else if let Some(val) = self.eval_const_expr(expr) {
                                                self.write_const_to_bytes(&mut bytes, elem_offset, &val, IrType::I64);
                                            }
                                        }
                                    }
                                } else if let Some(sub_layout) = self.get_struct_layout_for_ctype(&field.ty) {
                                    // Nested struct with braced init
                                    self.fill_struct_global_bytes(inner_items, &sub_layout, &mut bytes, field_offset);
                                    // Also collect pointer/function-pointer relocations from nested struct fields
                                    self.collect_ptr_ranges_from_struct_init_list(
                                        inner_items, &sub_layout, field_offset, &mut ptr_ranges);
                                } else {
                                    // Array of scalars with braced init
                                    let elem_size = match &field.ty {
                                        CType::Array(inner, _) => inner.size(),
                                        _ => field_ir_ty.size(),
                                    };
                                    let elem_ir_ty = match &field.ty {
                                        CType::Array(inner, _) => IrType::from_ctype(inner),
                                        _ => field_ir_ty,
                                    };
                                    for (ai, inner_item) in inner_items.iter().enumerate() {
                                        let elem_offset = field_offset + ai * elem_size;
                                        if let Initializer::Expr(ref expr) = inner_item.init {
                                            if let Some(val) = self.eval_const_expr(expr) {
                                                self.write_const_to_bytes(&mut bytes, elem_offset, &val, elem_ir_ty);
                                            }
                                        }
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
}
