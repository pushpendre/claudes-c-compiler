//! Global initialization subsystem for the IR lowerer.
//!
//! This module handles lowering of global variable initializers from AST
//! `Initializer` nodes to IR `GlobalInit` values. It covers struct/union
//! initializers (including nested, designated, bitfield, and flexible array
//! members), array initializers (multi-dimensional, flat, and pointer arrays),
//! compound literal globals, and scalar initializers.

use crate::frontend::parser::ast::*;
use crate::ir::ir::*;

/// Kind of string literal for extract_string_literal helper.
enum StringLitKind {
    Narrow,
    Wide,
    Char16,
}
use crate::common::types::{IrType, StructLayout, RcLayout, CType};
use super::lowering::Lowerer;
use super::global_init_helpers as h;

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
        struct_layout: &Option<RcLayout>,
        array_dim_strides: &[usize],
    ) -> GlobalInit {
        // Check if the target type is long double (need to emit as x87 80-bit)
        let is_long_double_target = self.is_type_spec_long_double(_type_spec);
        // Check if the target element type is _Bool (C11 6.3.1.2 normalization needed)
        let is_bool_target = self.is_type_bool(_type_spec);

        match init {
            Initializer::Expr(expr) => {
                // Try to evaluate as a constant
                if let Some(val) = self.eval_const_expr(expr) {
                    // Convert integer constants to float if target type is float/double
                    // Use the expression's type for signedness (e.g., unsigned long long -> float)
                    let src_ty = self.get_expr_type(expr);
                    let val = if is_bool_target {
                        val.bool_normalize()
                    } else {
                        self.coerce_const_to_type_with_src(val, base_ty, src_ty)
                    };
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
                    } else if is_array && (base_ty == IrType::I32 || base_ty == IrType::U32) {
                        // Narrow string to wchar_t/char32_t array: promote each byte to I32
                        let chars: Vec<u32> = s.chars().map(|c| c as u32).collect();
                        return GlobalInit::WideString(chars);
                    } else if is_array && (base_ty == IrType::I16 || base_ty == IrType::U16) {
                        // Narrow string to char16_t array: promote each byte to U16
                        let chars: Vec<u16> = s.chars().map(|c| c as u16).collect();
                        return GlobalInit::Char16String(chars);
                    } else {
                        // Pointer: const char *s = "hello" -> reference .rodata label
                        let label = self.intern_string_literal(s);
                        return GlobalInit::GlobalAddr(label);
                    }
                }
                // Wide string literal initializer
                if let Expr::WideStringLiteral(s, _) = expr {
                    if is_array && (base_ty == IrType::I32 || base_ty == IrType::U32) {
                        // wchar_t/char32_t array: wchar_t s[] = L"hello" -> inline as I32 array
                        let chars: Vec<u32> = s.chars().map(|c| c as u32).collect();
                        return GlobalInit::WideString(chars);
                    } else if is_array && (base_ty == IrType::I8 || base_ty == IrType::U8) {
                        // Wide string to char array (just take low bytes)
                        return GlobalInit::String(s.clone());
                    } else if is_array && (base_ty == IrType::I16 || base_ty == IrType::U16) {
                        // Wide string to char16_t array: truncate each char to u16
                        let chars: Vec<u16> = s.chars().map(|c| c as u16).collect();
                        return GlobalInit::Char16String(chars);
                    } else {
                        // Pointer: const wchar_t *s = L"hello" -> reference .rodata label
                        let label = self.intern_wide_string_literal(s);
                        return GlobalInit::GlobalAddr(label);
                    }
                }
                // char16_t string literal initializer (u"...")
                if let Expr::Char16StringLiteral(s, _) = expr {
                    if is_array && (base_ty == IrType::I16 || base_ty == IrType::U16) {
                        // char16_t array: char16_t s[] = u"hello" -> inline as U16 array
                        let chars: Vec<u16> = s.chars().map(|c| c as u16).collect();
                        return GlobalInit::Char16String(chars);
                    } else if is_array && (base_ty == IrType::I8 || base_ty == IrType::U8) {
                        // char16_t string to char array (just take low bytes)
                        return GlobalInit::String(s.clone());
                    } else if is_array && (base_ty == IrType::I32 || base_ty == IrType::U32) {
                        // char16_t string to wchar_t/char32_t array: promote each to u32
                        let chars: Vec<u32> = s.chars().map(|c| c as u32).collect();
                        return GlobalInit::WideString(chars);
                    } else {
                        // Pointer: const char16_t *s = u"hello" -> reference .rodata label
                        let label = self.intern_char16_string_literal(s);
                        return GlobalInit::GlobalAddr(label);
                    }
                }
                // Handle &(compound_literal) at file scope: create anonymous global
                if let Expr::AddressOf(inner, _) = expr {
                    if let Expr::CompoundLiteral(ref cl_type_spec, ref cl_init, _) = inner.as_ref() {
                        return self.create_compound_literal_global(cl_type_spec, cl_init);
                    }
                }
                // Handle (compound_literal) used as initializer value
                if let Expr::CompoundLiteral(ref cl_type_spec, ref cl_init, _) = expr {
                    let cl_ctype = self.type_spec_to_ctype(cl_type_spec);
                    let is_aggregate = matches!(
                        cl_ctype,
                        CType::Struct(..) | CType::Union(..) | CType::Array(..)
                    );
                    if !is_aggregate {
                        // Scalar or pointer compound literal: create anonymous global
                        return self.create_compound_literal_global(cl_type_spec, cl_init);
                    }
                    // Aggregate compound literal (struct/union/array): recursively
                    // lower using the compound literal's own type info.
                    // e.g. .mask = (cpumask_t){ { [0] = ~0UL } }
                    let cl_base_ty = self.type_spec_to_ir(cl_type_spec);
                    let cl_layout = self.get_struct_layout_for_type(cl_type_spec);
                    let cl_size = if let Some(ref layout) = cl_layout {
                        layout.size
                    } else {
                        self.sizeof_type(cl_type_spec)
                    };
                    let cl_is_array = matches!(cl_ctype, CType::Array(..));
                    return self.lower_global_init(
                        cl_init, cl_type_spec, cl_base_ty, cl_is_array,
                        0, cl_size, &cl_layout, &[],
                    );
                }
                // Handle string literal with constant offset: "str" + N or "str" - N
                if let Some(addr_init) = self.eval_string_literal_addr_expr(expr) {
                    return addr_init;
                }
                // Try to evaluate as a global address expression (e.g., &x, func, arr, &arr[3], &s.field)
                if let Some(addr_init) = self.eval_global_addr_expr(expr) {
                    return addr_init;
                }
                // Try label difference: &&lab1 - &&lab2 (computed goto dispatch tables)
                if let Some(label_diff) = self.eval_label_diff_expr(expr, base_ty.size().max(4)) {
                    return label_diff;
                }
                // Complex global initializer: try to evaluate as {real, imag} pair
                {
                    let ctype = self.type_spec_to_ctype(_type_spec);
                    if ctype.is_complex() {
                        if let Some(init) = self.eval_complex_global_init(expr, &ctype) {
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
                let complex_ctype = self.type_spec_to_ctype(_type_spec);
                let is_complex_element = is_array && complex_ctype.is_complex();
                if is_complex_element {
                    let num_elems = total_size / elem_size.max(1);
                    // Each complex element is stored as [real, imag] pair.
                    // Total scalar values = num_elems * 2.
                    let total_scalars = num_elems * 2;
                    let zero_pair: Vec<IrConst> = match &complex_ctype {
                        CType::ComplexFloat => vec![IrConst::F32(0.0), IrConst::F32(0.0)],
                        CType::ComplexLongDouble => vec![IrConst::LongDouble(0.0), IrConst::LongDouble(0.0)],
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
                                    match &complex_ctype {
                                        CType::ComplexFloat => {
                                            values[base_offset] = IrConst::F32(real as f32);
                                            values[base_offset + 1] = IrConst::F32(imag as f32);
                                        }
                                        CType::ComplexLongDouble => {
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
                    // But skip byte-serialization if any struct field is or contains
                    // a pointer type (pointers need .quad directives for address relocations).
                    let has_ptr_fields = struct_layout.as_ref()
                        .map_or(false, |layout| layout.has_pointer_fields(&self.types));
                    if let Some(ref layout) = struct_layout {
                        if has_ptr_fields {
                            // Use Compound approach for struct arrays with pointer fields
                            if array_dim_strides.len() > 1 {
                                // Multi-dimensional struct array (e.g., struct S grid[2][3])
                                return self.lower_struct_array_with_ptrs_multidim(
                                    items, layout, total_size, array_dim_strides,
                                );
                            }
                            // 1D struct array
                            return self.lower_struct_array_with_ptrs(items, layout, num_elems);
                        }
                        let struct_size = layout.size;
                        let mut bytes = vec![0u8; total_size];
                        self.fill_multidim_struct_array_bytes(
                            items, layout, struct_size, array_dim_strides,
                            &mut bytes, 0, total_size,
                        );
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
                            // Also detect label addresses nested in binary ops
                            // (e.g., &&lab1 - &&lab0 for computed goto dispatch tables)
                            if Self::expr_contains_label_addr(expr) {
                                return true;
                            }
                            if self.eval_const_expr(expr).is_none() && self.eval_global_addr_expr(expr).is_some() {
                                return true;
                            }
                        }
                        // Also recurse into nested lists for string literals (double-brace init)
                        h::init_contains_addr_expr(item, is_multidim_char_array)
                    }) || (is_ptr_array && !is_multidim_char_array && items.iter().any(|item| {
                        h::init_contains_string_literal(item)
                    }));

                    if has_addr_exprs {
                        // Use Compound initializer for arrays containing address expressions.
                        // Each Compound element becomes one .quad directive in the backend.
                        // For multi-dimensional arrays, we need the TOTAL number of base-type
                        // slots, not the number of outer-dimension elements.
                        let base_type_size = base_ty.size().max(1);
                        let flat_num_elems = total_size / base_type_size;
                        // For multi-dimensional arrays, compute how many flat elements
                        // each top-level sub-array item spans. E.g., int *arr[2][3]
                        // has stride 24 (3*8) for the outer dimension, so each outer
                        // sub-array spans 3 flat pointer slots.
                        let outer_stride = if array_dim_strides.len() >= 2 {
                            array_dim_strides[0] / base_type_size
                        } else {
                            1
                        };
                        let mut elements: Vec<GlobalInit> = (0..flat_num_elems).map(|_| GlobalInit::Zero).collect();
                        // Detect if items are structured (nested lists for sub-arrays)
                        // vs flat (individual expressions for each element).
                        let is_structured = outer_stride > 1 && items.iter().any(|item| {
                            matches!(&item.init, Initializer::List(_))
                        });
                        let mut current_idx = 0usize;
                        for item in items {
                            if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                                if let Some(idx) = self.eval_const_expr(idx_expr).and_then(|c| c.to_usize()) {
                                    current_idx = idx;
                                }
                            }
                            // For structured init (nested lists), each item spans outer_stride
                            // flat positions. For flat init, each item is one flat position.
                            let flat_idx = if is_structured {
                                current_idx * outer_stride
                            } else {
                                current_idx
                            };
                            if flat_idx < flat_num_elems {
                                let mut elem_parts = Vec::new();
                                self.collect_compound_init_element(&item.init, &mut elem_parts, elem_size);
                                // Place collected elements at the correct flat positions
                                for (i, elem) in elem_parts.into_iter().enumerate() {
                                    if flat_idx + i < flat_num_elems {
                                        let elem = match elem {
                                            GlobalInit::Scalar(val) => {
                                                GlobalInit::Scalar(val.coerce_to(base_ty))
                                            }
                                            other => other,
                                        };
                                        elements[flat_idx + i] = elem;
                                    }
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
                        self.flatten_global_array_init_bool(items, array_dim_strides, base_ty, &mut flat, is_bool_target);
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
                                        if is_bool_target {
                                            raw.bool_normalize()
                                        } else {
                                            let expr_ty = self.get_expr_type(expr);
                                            self.coerce_const_to_type_with_src(raw, base_ty, expr_ty)
                                        }
                                    }
                                    Initializer::List(sub_items) => {
                                        let mut sub_vals = Vec::new();
                                        for sub in sub_items {
                                            self.flatten_global_init_item(&sub.init, base_ty, &mut sub_vals);
                                        }
                                        let raw = sub_vals.into_iter().next().unwrap_or(self.zero_const(base_ty));
                                        if is_bool_target {
                                            raw.bool_normalize()
                                        } else {
                                            raw.coerce_to(base_ty)
                                        }
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

                // Scalar with braces: int x = { 1 }; or int x = {{{1}}};
                // C11 6.7.9: A scalar can be initialized with a single braced expression.
                if !is_array && items.len() >= 1 {
                    if let Some(expr) = Self::unwrap_nested_init_expr(items) {
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

                // Fallback: try to emit as an array of constants coerced to base_ty
                let mut values = Vec::new();
                for item in items {
                    if let Initializer::Expr(expr) = &item.init {
                        if let Some(val) = self.eval_const_expr(expr) {
                            values.push(val.coerce_to(base_ty));
                        } else {
                            values.push(self.zero_const(base_ty));
                        }
                    } else {
                        values.push(self.zero_const(base_ty));
                    }
                }
                if !values.is_empty() {
                    return GlobalInit::Array(values);
                }
                GlobalInit::Zero
            }
        }
    }

    /// Evaluate a string literal address expression for static initializers.
    /// Handles patterns like:
    ///   "hello"        -> GlobalAddr(.Lstr0)
    ///   "hello" + 2    -> GlobalAddrOffset(.Lstr0, 2)
    ///   "hello" - 1    -> GlobalAddrOffset(.Lstr0, -1)
    ///   (type*)"hello"       -> GlobalAddr(.Lstr0)
    ///   (type*)("hello" + 2) -> GlobalAddrOffset(.Lstr0, 2)
    /// Returns None if the expression is not a string literal address expression.
    pub(super) fn eval_string_literal_addr_expr(&mut self, expr: &Expr) -> Option<GlobalInit> {
        match expr {
            // "str" + N  or  N + "str"
            Expr::BinaryOp(BinOp::Add, lhs, rhs, _) => {
                // Try lhs = string, rhs = offset
                if let Some(result) = self.eval_string_literal_with_offset(lhs, rhs, false) {
                    return Some(result);
                }
                // Try rhs = string, lhs = offset (commutative)
                if let Some(result) = self.eval_string_literal_with_offset(rhs, lhs, false) {
                    return Some(result);
                }
                None
            }
            // "str" - N
            Expr::BinaryOp(BinOp::Sub, lhs, rhs, _) => {
                self.eval_string_literal_with_offset(lhs, rhs, true)
            }
            // (type*)"str" or (type*)("str" + N)
            Expr::Cast(_, inner, _) => {
                self.eval_string_literal_addr_expr(inner)
            }
            _ => None,
        }
    }

    /// Helper: given a string literal expression and an offset expression,
    /// intern the string and return GlobalAddr or GlobalAddrOffset.
    fn eval_string_literal_with_offset(
        &mut self,
        str_expr: &Expr,
        offset_expr: &Expr,
        negate: bool,
    ) -> Option<GlobalInit> {
        // Check if str_expr is a string literal (possibly through a cast)
        let (s, kind) = self.extract_string_literal(str_expr)?;
        let offset_val = self.eval_const_expr(offset_expr)?;
        let offset = self.const_to_i64(&offset_val)?;
        let byte_offset = if negate { -offset } else { offset };

        let label = match kind {
            StringLitKind::Narrow => self.intern_string_literal(&s),
            StringLitKind::Wide => self.intern_wide_string_literal(&s),
            StringLitKind::Char16 => self.intern_char16_string_literal(&s),
        };

        if byte_offset == 0 {
            Some(GlobalInit::GlobalAddr(label))
        } else {
            Some(GlobalInit::GlobalAddrOffset(label, byte_offset))
        }
    }

    /// Extract a string literal from an expression, possibly through casts.
    /// Returns (string_content, string_kind).
    fn extract_string_literal(&self, expr: &Expr) -> Option<(String, StringLitKind)> {
        match expr {
            Expr::StringLiteral(s, _) => Some((s.clone(), StringLitKind::Narrow)),
            Expr::WideStringLiteral(s, _) => Some((s.clone(), StringLitKind::Wide)),
            Expr::Char16StringLiteral(s, _) => Some((s.clone(), StringLitKind::Char16)),
            Expr::Cast(_, inner, _) => self.extract_string_literal(inner),
            _ => None,
        }
    }

    /// Create an anonymous global for a compound literal at file scope.
    /// Used for: struct S *s = &(struct S){1, 2};
    pub(super) fn create_compound_literal_global(
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

        self.emitted_global_names.insert(label.clone());
        self.module.globals.push(IrGlobal {
            name: label.clone(),
            ty: global_ty,
            size: alloc_size,
            align,
            init: global_init,
            is_static: true, // compound literal globals are local to translation unit
            is_extern: false,
            is_common: false,
            section: None,
            is_weak: false,
            visibility: None,
            has_explicit_align: false, // compound literals don't have explicit alignment
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

    /// Check if an initializer (possibly nested) contains expressions that require
    /// address relocations (string literals, address-of expressions, etc.)
    fn init_has_addr_exprs(&self, init: &Initializer) -> bool {
        match init {
            Initializer::Expr(expr) => {
                // String literal or expression containing one (e.g., "str" + N)
                if h::expr_contains_string_literal(expr) {
                    return true;
                }
                // &(compound_literal) at file scope
                if let Expr::AddressOf(inner, _) = expr {
                    if matches!(inner.as_ref(), Expr::CompoundLiteral(..)) {
                        return true;
                    }
                }
                // Non-pointer compound literal: check its inner initializer
                if let Expr::CompoundLiteral(_, ref cl_init, _) = expr {
                    return self.init_has_addr_exprs(cl_init);
                }
                self.eval_const_expr(expr).is_none() && self.eval_global_addr_expr(expr).is_some()
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
                let elem_size = self.resolve_ctype_size(elem_ty);
                let last_field_idx = layout.fields.len() - 1;
                let mut current_field_idx = 0usize;
                for (item_idx, item) in items.iter().enumerate() {
                    let field_idx = self.resolve_struct_init_field_idx(item, layout, current_field_idx);
                    if field_idx == last_field_idx {
                        let num_elems = match &item.init {
                            Initializer::List(sub_items) => sub_items.len(),
                            Initializer::Expr(Expr::StringLiteral(s, _)) => {
                                // String literal: length + null terminator
                                if matches!(elem_ty.as_ref(), CType::Char | CType::UChar) {
                                    s.len() + 1
                                } else {
                                    items.len() - item_idx
                                }
                            }
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
    /// and designated init patterns, including multi-level designators like .u.field = {...}.
    pub(super) fn struct_init_has_addr_fields(&self, items: &[InitializerItem], layout: &StructLayout) -> bool {
        let mut current_field_idx = 0usize;
        let mut item_idx = 0usize;

        while item_idx < items.len() {
            let item = &items[item_idx];
            let field_idx = self.resolve_struct_init_field_idx(item, layout, current_field_idx);

            if field_idx >= layout.fields.len() {
                item_idx += 1;
                continue;
            }

            let field_ty = &layout.fields[field_idx].ty;

            if h::has_nested_field_designator(item) {
                if self.nested_designator_has_addr_fields(item, field_ty) {
                    return true;
                }
                current_field_idx = field_idx + 1;
                item_idx += 1;
                continue;
            }

            match &item.init {
                Initializer::Expr(expr) => {
                    if h::expr_contains_string_literal(expr) {
                        if h::type_has_pointer_elements(field_ty, &self.types) {
                            return true;
                        }
                    }
                    if self.eval_const_expr(expr).is_none() && self.eval_global_addr_expr(expr).is_some() {
                        return true;
                    }
                    if let Expr::AddressOf(inner, _) = expr {
                        if matches!(inner.as_ref(), Expr::CompoundLiteral(..)) {
                            return true;
                        }
                    }
                    if let CType::Array(elem_ty, Some(arr_size)) = field_ty {
                        if h::type_has_pointer_elements(elem_ty, &self.types) {
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
                    if h::type_has_pointer_elements(field_ty, &self.types) {
                        if self.init_has_addr_exprs(&item.init) {
                            return true;
                        }
                    }
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

    /// Check if a multi-level designated initializer (e.g., .bs.keyword = {"STORE", -1})
    /// contains address fields by drilling through the designator chain to find the
    /// actual target type.
    fn nested_designator_has_addr_fields(&self, item: &InitializerItem, outer_ty: &CType) -> bool {
        let drill = match self.drill_designators(&item.designators[1..], outer_ty) {
            Some(d) => d,
            None => return false,
        };
        let current_ty = drill.target_ty;

        if matches!(&current_ty, CType::Pointer(_, _) | CType::Function(_)) {
            return self.init_has_addr_exprs(&item.init);
        }

        if let Some(target_layout) = self.get_struct_layout_for_ctype(&current_ty) {
            if target_layout.has_pointer_fields(&self.types) && self.init_has_addr_exprs(&item.init) {
                return true;
            }
            if let Initializer::List(nested_items) = &item.init {
                if self.struct_init_has_addr_fields(nested_items, &target_layout) {
                    return true;
                }
            }
        }

        if let CType::Array(elem_ty, _) = &current_ty {
            if h::type_has_pointer_elements(elem_ty, &self.types) {
                return self.init_has_addr_exprs(&item.init);
            }
        }

        false
    }

    // --- Array flattening helpers ---

    /// Inline a string literal into a values array for a sub-array element.
    /// Pushes string bytes, null terminator, pads to `sub_elem_count`, and truncates if needed.
    fn inline_string_to_values(
        &self, s: &str, sub_elem_count: usize, base_ty: IrType, values: &mut Vec<IrConst>,
    ) {
        let start_len = values.len();
        // Use chars() to get raw byte values (each char U+0000..U+00FF)
        for c in s.chars() {
            values.push(IrConst::I64(c as u8 as i64));
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
    }

    fn flatten_global_array_init_bool(
        &self,
        items: &[InitializerItem],
        array_dim_strides: &[usize],
        base_ty: IrType,
        values: &mut Vec<IrConst>,
        is_bool_target: bool,
    ) {
        let base_type_size = base_ty.size().max(1);
        if array_dim_strides.len() <= 1 {
            // 1D array: support designated initializers [idx] = val
            // Designators may jump forward or backward, so we need to handle
            // both cases: padding for forward jumps, overwriting for backward.
            let mut current_idx = 0usize;
            for item in items {
                if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                    if let Some(idx) = self.eval_const_expr(idx_expr).and_then(|c| c.to_usize()) {
                        current_idx = idx;
                    }
                }
                // Pad values up to current_idx if needed (forward jump)
                while values.len() < current_idx {
                    values.push(self.zero_const(base_ty));
                }
                if current_idx < values.len() {
                    // Backward jump or overwrite: evaluate and write at the designated position
                    let mut tmp = Vec::new();
                    self.flatten_global_init_item_bool(&item.init, base_ty, &mut tmp, is_bool_target);
                    let n = tmp.len();
                    for (i, v) in tmp.into_iter().enumerate() {
                        let pos = current_idx + i;
                        if pos < values.len() {
                            values[pos] = v;
                        } else {
                            values.push(v);
                        }
                    }
                    current_idx += n.max(1);
                } else {
                    // Forward jump (already padded above): append at end
                    self.flatten_global_init_item_bool(&item.init, base_ty, values, is_bool_target);
                    current_idx = values.len();
                }
            }
            return;
        }
        // Number of base elements per sub-array at this dimension level
        let sub_elem_count = if array_dim_strides[0] > 0 && base_type_size > 0 {
            array_dim_strides[0] / base_type_size
        } else {
            1
        };
        let start_len = values.len();
        let mut current_outer_idx = 0usize;
        for item in items {
            // Check for multi-dimensional designators: [i][j]...
            let index_designators: Vec<usize> = item.designators.iter().filter_map(|d| {
                if let Designator::Index(ref idx_expr) = d {
                    self.eval_const_expr(idx_expr).and_then(|c| c.to_usize())
                } else {
                    None
                }
            }).collect();

            if !index_designators.is_empty() {
                // Compute flat scalar index from multi-dimensional indices and strides
                let flat_idx = self.compute_flat_index_from_designators(
                    &index_designators, array_dim_strides, base_type_size
                );
                // Pad values up to flat_idx if needed
                while values.len() <= flat_idx {
                    values.push(self.zero_const(base_ty));
                }
                match &item.init {
                    Initializer::List(sub_items) => {
                        // Braced sub-list at a designated position: recurse into temp
                        // and overwrite values at flat_idx to support backward jumps.
                        let remaining_dims = array_dim_strides.len().saturating_sub(index_designators.len());
                        let sub_strides = &array_dim_strides[array_dim_strides.len() - remaining_dims..];
                        let mut tmp = Vec::new();
                        if sub_strides.is_empty() || remaining_dims == 0 {
                            self.flatten_global_init_item_bool(&item.init, base_ty, &mut tmp, is_bool_target);
                        } else {
                            self.flatten_global_array_init_bool(sub_items, sub_strides, base_ty, &mut tmp, is_bool_target);
                        }
                        // Write tmp values at flat_idx, overwriting or extending as needed
                        for (i, v) in tmp.into_iter().enumerate() {
                            let pos = flat_idx + i;
                            if pos < values.len() {
                                values[pos] = v;
                            } else {
                                while values.len() < pos {
                                    values.push(self.zero_const(base_ty));
                                }
                                values.push(v);
                            }
                        }
                    }
                    Initializer::Expr(expr) => {
                        if let Expr::StringLiteral(s, _) = expr {
                            // String at designated position: evaluate into temp and overwrite
                            let mut tmp = Vec::new();
                            let string_sub_count = if index_designators.len() < array_dim_strides.len() {
                                let remaining = &array_dim_strides[index_designators.len()..];
                                if !remaining.is_empty() && base_type_size > 0 {
                                    remaining[0] / base_type_size
                                } else {
                                    sub_elem_count
                                }
                            } else {
                                1
                            };
                            self.inline_string_to_values(s, string_sub_count, base_ty, &mut tmp);
                            // Write tmp values at flat_idx, overwriting or extending
                            for (i, v) in tmp.into_iter().enumerate() {
                                let pos = flat_idx + i;
                                if pos < values.len() {
                                    values[pos] = v;
                                } else {
                                    while values.len() < pos {
                                        values.push(self.zero_const(base_ty));
                                    }
                                    values.push(v);
                                }
                            }
                        } else {
                            // Scalar at designated flat position
                            if let Some(val) = self.eval_const_expr(expr) {
                                values[flat_idx] = if is_bool_target {
                                    val.bool_normalize()
                                } else {
                                    let expr_ty = self.get_expr_type(expr);
                                    self.coerce_const_to_type_with_src(val, base_ty, expr_ty)
                                };
                            }
                        }
                    }
                }
                // Update current_outer_idx based on the first designator
                current_outer_idx = index_designators[0] + 1;
                continue;
            }

            // No designator: sequential processing
            match &item.init {
                Initializer::List(sub_items) => {
                    // Braced sub-list: aligns to the next sub-array boundary
                    let target_start = start_len + current_outer_idx * sub_elem_count;
                    while values.len() < target_start {
                        values.push(self.zero_const(base_ty));
                    }
                    // Check for braced string literal initializing a char sub-array: { "abc" }
                    if sub_items.len() == 1 {
                        if let Initializer::Expr(Expr::StringLiteral(s, _)) = &sub_items[0].init {
                            self.inline_string_to_values(s, sub_elem_count, base_ty, values);
                            current_outer_idx += 1;
                            continue;
                        }
                    }
                    // Braced sub-list: recurse into a fresh temporary vector so that
                    // designator indices are relative to the sub-array start, not the
                    // accumulated values vector. This is essential for backward jumps.
                    let mut sub_values = Vec::with_capacity(sub_elem_count);
                    self.flatten_global_array_init_bool(sub_items, &array_dim_strides[1..], base_ty, &mut sub_values, is_bool_target);
                    // Pad sub_values to sub_elem_count
                    while sub_values.len() < sub_elem_count {
                        sub_values.push(self.zero_const(base_ty));
                    }
                    values.extend(sub_values.into_iter().take(sub_elem_count));
                    current_outer_idx += 1;
                }
                Initializer::Expr(expr) => {
                    // Bare scalar in a multi-dim array: fills the next sequential
                    // scalar position (flat initialization without inner braces).
                    // Per C standard, scalars fill in row-major order.
                    if let Expr::StringLiteral(s, _) = expr {
                        let target_start = start_len + current_outer_idx * sub_elem_count;
                        while values.len() < target_start {
                            values.push(self.zero_const(base_ty));
                        }
                        self.inline_string_to_values(s, sub_elem_count, base_ty, values);
                        current_outer_idx += 1;
                    } else if let Some(val) = self.eval_const_expr(expr) {
                        values.push(if is_bool_target {
                            val.bool_normalize()
                        } else {
                            let expr_ty = self.get_expr_type(expr);
                            self.coerce_const_to_type_with_src(val, base_ty, expr_ty)
                        });
                        // Update current_outer_idx based on relative position from start
                        let relative_pos = values.len() - start_len;
                        if sub_elem_count > 0 {
                            current_outer_idx = (relative_pos + sub_elem_count - 1) / sub_elem_count;
                        }
                    } else {
                        values.push(self.zero_const(base_ty));
                        let relative_pos = values.len() - start_len;
                        if sub_elem_count > 0 {
                            current_outer_idx = (relative_pos + sub_elem_count - 1) / sub_elem_count;
                        }
                    }
                }
            }
        }
    }

    /// Compute a flat scalar index from multi-dimensional designator indices and array strides.
    /// For example, for `int grid[3][3]` with strides `[12, 4]` and designator `[1][2]`:
    /// flat_idx = 1 * (12/4) + 2 = 5
    fn compute_flat_index_from_designators(
        &self,
        indices: &[usize],
        array_dim_strides: &[usize],
        base_type_size: usize,
    ) -> usize {
        let mut flat_idx = 0usize;
        for (i, &idx) in indices.iter().enumerate() {
            let elems_per_entry = if i < array_dim_strides.len() && base_type_size > 0 {
                array_dim_strides[i] / base_type_size
            } else {
                1
            };
            flat_idx += idx * elems_per_entry;
        }
        flat_idx
    }

    /// Collect a single compound initializer element, handling nested lists (double-brace init).
    /// For `{{"str"}}`, unwraps the nested list to find the string literal.
    /// For `{"str"}`, directly handles the string literal expression.
    /// `elem_size` is the byte size of the target element (used for label diffs).
    fn collect_compound_init_element(&mut self, init: &Initializer, elements: &mut Vec<GlobalInit>, elem_size: usize) {
        match init {
            Initializer::Expr(expr) => {
                if let Expr::StringLiteral(s, _) = expr {
                    // String literal: create .rodata entry and use its label
                    let label = self.intern_string_literal(s);
                    elements.push(GlobalInit::GlobalAddr(label));
                } else if let Expr::LabelAddr(label_name, _) = expr {
                    // GCC &&label extension: emit label address as GlobalAddr
                    let scoped_label = self.get_or_create_user_label(label_name);
                    elements.push(GlobalInit::GlobalAddr(scoped_label.as_label()));
                } else if let Some(val) = self.eval_const_expr(expr) {
                    // Promote narrow integer constants to match element size.
                    // E.g., integer literal 0 evaluates to I32(0) but in a pointer
                    // array (elem_size == 8) it must emit as .quad 0, not .long 0.
                    let val = match val {
                        IrConst::I32(v) if elem_size == 8 => IrConst::I64(v as i64),
                        IrConst::I16(v) if elem_size >= 4 => {
                            if elem_size == 8 { IrConst::I64(v as i64) } else { IrConst::I32(v as i32) }
                        }
                        other => other,
                    };
                    elements.push(GlobalInit::Scalar(val));
                } else if let Some(label_diff) = self.eval_label_diff_expr(expr, elem_size) {
                    // Label difference: &&lab1 - &&lab2 (computed goto dispatch tables)
                    elements.push(label_diff);
                } else if let Some(addr) = self.eval_string_literal_addr_expr(expr) {
                    // String literal +/- offset: "str" + N -> GlobalAddrOffset
                    elements.push(addr);
                } else if let Some(addr) = self.eval_global_addr_expr(expr) {
                    elements.push(addr);
                } else {
                    elements.push(GlobalInit::Zero);
                }
            }
            Initializer::List(sub_items) => {
                // Nested brace init like {{&x, &y, &z}} for a sub-array row:
                // Recursively process each sub-item to collect all elements.
                if sub_items.is_empty() {
                    elements.push(GlobalInit::Zero);
                } else {
                    for sub in sub_items {
                        self.collect_compound_init_element(&sub.init, elements, elem_size);
                    }
                }
            }
        }
    }

    /// Try to evaluate a label difference expression: `&&lab1 - &&lab2`.
    /// Returns `GlobalLabelDiff(lab1_label, lab2_label, byte_size)` if the
    /// expression is a subtraction of two label addresses.
    /// The `byte_size` parameter specifies the target element width (4 for int, 8 for long).
    /// Only valid inside a function (labels must be in the enclosing function scope).
    fn eval_label_diff_expr(&mut self, expr: &Expr, byte_size: usize) -> Option<GlobalInit> {
        // Label differences are only valid inside functions (static locals).
        // Guard against file-scope globals where func_state is None.
        if self.func_state.is_none() {
            return None;
        }
        if let Expr::BinaryOp(BinOp::Sub, lhs, rhs, _) = expr {
            let lhs_inner = Self::strip_casts(lhs);
            let rhs_inner = Self::strip_casts(rhs);
            if let (Expr::LabelAddr(lab1, _), Expr::LabelAddr(lab2, _)) = (lhs_inner, rhs_inner) {
                let scoped1 = self.get_or_create_user_label(lab1);
                let scoped2 = self.get_or_create_user_label(lab2);
                return Some(GlobalInit::GlobalLabelDiff(
                    scoped1.as_label(),
                    scoped2.as_label(),
                    byte_size,
                ));
            }
        }
        None
    }

    /// Strip cast expressions to find the underlying expression.
    fn strip_casts(expr: &Expr) -> &Expr {
        match expr {
            Expr::Cast(_, inner, _) => Self::strip_casts(inner),
            _ => expr,
        }
    }

    /// Check if an expression contains label address references in binary ops
    /// (e.g., &&lab1 - &&lab0 for computed goto dispatch tables).
    // TODO: More complex nested arithmetic with label addresses
    // (e.g., (&&lab1 - &&lab2) + offset) is not detected here.
    fn expr_contains_label_addr(expr: &Expr) -> bool {
        match expr {
            Expr::LabelAddr(_, _) => true,
            Expr::BinaryOp(_, lhs, rhs, _) => {
                Self::expr_contains_label_addr(lhs) || Self::expr_contains_label_addr(rhs)
            }
            Expr::Cast(_, inner, _) => Self::expr_contains_label_addr(inner),
            _ => false,
        }
    }

    /// Flatten a single initializer item, recursing into nested lists.
    fn flatten_global_init_item(&self, init: &Initializer, base_ty: IrType, values: &mut Vec<IrConst>) {
        self.flatten_global_init_item_bool(init, base_ty, values, false)
    }

    /// Flatten a single initializer item with _Bool awareness.
    fn flatten_global_init_item_bool(&self, init: &Initializer, base_ty: IrType, values: &mut Vec<IrConst>, is_bool_target: bool) {
        match init {
            Initializer::Expr(expr) => {
                if let Expr::StringLiteral(s, _) = expr {
                    // String literal initializing a char array element: inline bytes
                    // Use chars() to get raw byte values (each char U+0000..U+00FF)
                    for c in s.chars() {
                        values.push(IrConst::I64(c as u8 as i64));
                    }
                    // Add null terminator
                    values.push(IrConst::I64(0));
                } else if let Some(val) = self.eval_const_expr(expr) {
                    values.push(if is_bool_target {
                        val.bool_normalize()
                    } else {
                        let expr_ty = self.get_expr_type(expr);
                        self.coerce_const_to_type_with_src(val, base_ty, expr_ty)
                    });
                } else {
                    values.push(self.zero_const(base_ty));
                }
            }
            Initializer::List(items) => {
                for item in items {
                    self.flatten_global_init_item_bool(&item.init, base_ty, values, is_bool_target);
                }
            }
        }
    }
}
