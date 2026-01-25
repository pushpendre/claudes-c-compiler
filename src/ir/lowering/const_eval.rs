/// Constant expression evaluation for compile-time computation.
///
/// This module contains all functions related to evaluating constant expressions
/// at compile time, including integer and floating-point arithmetic, cast chains,
/// global address resolution, offsetof patterns, and initializer list size computation.

use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{CType, IrType, StructLayout};
use crate::common::const_arith;
use super::lowering::Lowerer;

impl Lowerer {
    /// Look up a pre-computed constant value from sema's ConstMap.
    /// Returns Some(IrConst) if sema successfully evaluated this expression
    /// at compile time during its pass.
    fn lookup_sema_const(&self, expr: &Expr) -> Option<IrConst> {
        let key = expr as *const Expr as usize;
        self.sema_const_values.get(&key).cloned()
    }

    /// Try to evaluate a constant expression at compile time.
    ///
    /// First checks sema's pre-computed ConstMap (O(1) lookup for expressions
    /// that sema could evaluate). Falls back to the lowerer's own evaluation
    /// for expressions that require lowering-specific state (global addresses,
    /// const local values, etc.).
    pub(super) fn eval_const_expr(&self, expr: &Expr) -> Option<IrConst> {
        // Fast path: consult sema's pre-computed constant values.
        // This avoids re-evaluating expressions that sema already handled.
        // We skip the sema lookup for identifiers since the lowerer may have
        // more information (const local values, static locals) that sema lacks.
        if !matches!(expr, Expr::Identifier(_, _)) {
            if let Some(val) = self.lookup_sema_const(expr) {
                return Some(val);
            }
        }
        match expr {
            Expr::IntLiteral(val, _) | Expr::LongLiteral(val, _) => {
                Some(IrConst::I64(*val))
            }
            Expr::UIntLiteral(val, _) | Expr::ULongLiteral(val, _) => {
                Some(IrConst::I64(*val as i64))
            }
            Expr::CharLiteral(ch, _) => {
                Some(IrConst::I32(*ch as i32))
            }
            Expr::FloatLiteral(val, _) => {
                Some(IrConst::F64(*val))
            }
            Expr::FloatLiteralF32(val, _) => {
                Some(IrConst::F32(*val as f32))
            }
            Expr::FloatLiteralLongDouble(val, _) => {
                Some(IrConst::LongDouble(*val))
            }
            Expr::UnaryOp(UnaryOp::Plus, inner, _) => {
                self.eval_const_expr(inner)
            }
            Expr::UnaryOp(UnaryOp::Neg, inner, _) => {
                const_arith::negate_const(self.eval_const_expr(inner)?)
            }
            Expr::BinaryOp(op, lhs, rhs, _) => {
                let l = self.eval_const_expr(lhs)?;
                let r = self.eval_const_expr(rhs)?;
                // Use both operand types for proper usual arithmetic conversions
                let lhs_ty = self.get_expr_type(lhs);
                let rhs_ty = self.get_expr_type(rhs);
                self.eval_const_binop(op, &l, &r, lhs_ty, rhs_ty)
            }
            Expr::UnaryOp(UnaryOp::BitNot, inner, _) => {
                const_arith::bitnot_const(self.eval_const_expr(inner)?)
            }
            Expr::Cast(ref target_type, inner, _) => {
                let target_ir_ty = self.type_spec_to_ir(target_type);
                let src_val = self.eval_const_expr(inner)?;

                // Handle float source types: use value-based conversion, not bit manipulation
                if let Some(fv) = src_val.to_f64() {
                    if matches!(&src_val, IrConst::F32(_) | IrConst::F64(_) | IrConst::LongDouble(_)) {
                        return IrConst::cast_float_to_target(fv, target_ir_ty);
                    }
                }

                // Integer source: use bit-based cast chain evaluation
                let (bits, _src_signed) = self.eval_const_expr_as_bits(inner)?;
                let target_width = target_ir_ty.size() * 8;
                let target_signed = matches!(target_ir_ty, IrType::I8 | IrType::I16 | IrType::I32 | IrType::I64);

                // Truncate to target width
                let truncated = if target_width >= 64 {
                    bits
                } else {
                    bits & ((1u64 << target_width) - 1)
                };

                // Convert to IrConst based on target type
                let result = match target_ir_ty {
                    IrType::I8 => IrConst::I8(truncated as i8),
                    IrType::U8 => IrConst::I64(truncated as u8 as i64),
                    IrType::I16 => IrConst::I16(truncated as i16),
                    IrType::U16 => IrConst::I64(truncated as u16 as i64),
                    IrType::I32 => IrConst::I32(truncated as i32),
                    IrType::U32 => IrConst::I64(truncated as u32 as i64),
                    IrType::I64 | IrType::U64 | IrType::Ptr => IrConst::I64(truncated as i64),
                    IrType::F32 => {
                        let int_val = if target_signed { truncated as i64 as f32 } else { truncated as u64 as f32 };
                        IrConst::F32(int_val)
                    }
                    IrType::F64 => {
                        let int_val = if target_signed { truncated as i64 as f64 } else { truncated as u64 as f64 };
                        IrConst::F64(int_val)
                    }
                    _ => return None,
                };
                Some(result)
            }
            Expr::Identifier(name, _) => {
                // Look up enum constants first
                if let Some(&val) = self.types.enum_constants.get(name) {
                    return Some(IrConst::I64(val));
                }
                // Look up const-qualified local variable values
                // (e.g., const int len = 5000; int arr[len];)
                if let Some(ref fs) = self.func_state {
                    if let Some(&val) = fs.const_local_values.get(name) {
                        return Some(IrConst::I64(val));
                    }
                }
                None
            }
            Expr::Sizeof(arg, _) => {
                let size = match arg.as_ref() {
                    SizeofArg::Type(ts) => self.sizeof_type(ts),
                    SizeofArg::Expr(e) => self.sizeof_expr(e),
                };
                Some(IrConst::I64(size as i64))
            }
            Expr::Alignof(ref ts, _) => {
                let align = self.alignof_type(ts);
                Some(IrConst::I64(align as i64))
            }
            Expr::Conditional(cond, then_e, else_e, _) => {
                // Ternary in constant expr: evaluate condition and pick branch
                let cond_val = self.eval_const_expr(cond)?;
                if cond_val.is_nonzero() {
                    self.eval_const_expr(then_e)
                } else {
                    self.eval_const_expr(else_e)
                }
            }
            Expr::GnuConditional(cond, else_e, _) => {
                let cond_val = self.eval_const_expr(cond)?;
                if cond_val.is_nonzero() {
                    Some(cond_val) // condition value is used as result
                } else {
                    self.eval_const_expr(else_e)
                }
            }
            Expr::UnaryOp(UnaryOp::LogicalNot, inner, _) => {
                let val = self.eval_const_expr(inner)?;
                Some(IrConst::I64(if val.is_nonzero() { 0 } else { 1 }))
            }
            // Handle &((type*)0)->member pattern (offsetof)
            Expr::AddressOf(inner, _) => {
                self.eval_offsetof_pattern(inner)
            }
            Expr::BuiltinTypesCompatibleP(ref type1, ref type2, _) => {
                let result = self.eval_types_compatible(type1, type2);
                Some(IrConst::I64(result as i64))
            }
            _ => None,
        }
    }

    /// Evaluate the offsetof pattern: &((type*)0)->member
    /// Returns Some(IrConst::I64(offset)) if the expression matches the pattern.
    fn eval_offsetof_pattern(&self, expr: &Expr) -> Option<IrConst> {
        // Pattern: ((type*)0)->member or ((type*)0)->member.submember
        // Also handle: (*((type*)0)).member
        match expr {
            Expr::PointerMemberAccess(base, field_name, _) => {
                // base should be (type*)0 - a cast of 0 to a pointer type
                let (type_spec, base_offset) = self.extract_null_pointer_cast_with_offset(base)?;
                let layout = self.get_struct_layout_for_type(&type_spec)?;
                let (field_offset, _field_ty) = layout.field_offset(field_name, &self.types)?;
                Some(IrConst::I64((base_offset + field_offset) as i64))
            }
            Expr::MemberAccess(base, field_name, _) => {
                // base might be *((type*)0)
                if let Expr::Deref(inner, _) = base.as_ref() {
                    let (type_spec, base_offset) = self.extract_null_pointer_cast_with_offset(inner)?;
                    let layout = self.get_struct_layout_for_type(&type_spec)?;
                    let (field_offset, _field_ty) = layout.field_offset(field_name, &self.types)?;
                    Some(IrConst::I64((base_offset + field_offset) as i64))
                } else {
                    None
                }
            }
            Expr::ArraySubscript(base, index, _) => {
                // Handle &((type*)0)->member[index] pattern
                // base is PointerMemberAccess or MemberAccess that results in an array
                let base_offset = self.eval_offsetof_pattern(base)?;
                if let IrConst::I64(boff) = base_offset {
                    if let Some(idx_val) = self.eval_const_expr(index) {
                        if let IrConst::I64(idx) = idx_val {
                            // Get the element size of the array member
                            if let Some(ctype) = self.get_expr_ctype(base) {
                                let elem_size = match &ctype {
                                    CType::Array(elem, _) => self.resolve_ctype_size(elem),
                                    _ => return None,
                                };
                                return Some(IrConst::I64(boff + idx * elem_size as i64));
                            }
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Extract the struct type from a (type*)0 pattern, returning the base TypeSpecifier
    /// for the struct type and any accumulated offset from nested member access.
    fn extract_null_pointer_cast_with_offset(&self, expr: &Expr) -> Option<(TypeSpecifier, usize)> {
        match expr {
            Expr::Cast(ref type_spec, inner, _) => {
                // The type should be a Pointer to a struct
                if let TypeSpecifier::Pointer(inner_ts) = type_spec {
                    // Check that the inner expression is 0
                    if self.is_zero_expr(inner) {
                        return Some((*inner_ts.clone(), 0));
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Check if an expression evaluates to 0 (integer literal 0 or cast of 0).
    fn is_zero_expr(&self, expr: &Expr) -> bool {
        const_arith::is_zero_expr(expr)
    }

    /// Evaluate a constant expression, returning raw u64 bits and signedness.
    /// This preserves signedness information through cast chains.
    /// Signedness determines how the value is widened in the next cast.
    fn eval_const_expr_as_bits(&self, expr: &Expr) -> Option<(u64, bool)> {
        match expr {
            Expr::Cast(ref target_type, inner, _) => {
                let (bits, _src_signed) = self.eval_const_expr_as_bits(inner)?;
                let target_ir_ty = self.type_spec_to_ir(target_type);
                let target_width = target_ir_ty.size() * 8;
                let target_signed = matches!(target_ir_ty, IrType::I8 | IrType::I16 | IrType::I32 | IrType::I64);
                Some(const_arith::truncate_and_extend_bits(bits, target_width, target_signed))
            }
            _ => {
                let val = self.eval_const_expr(expr)?;
                let bits = match &val {
                    IrConst::F32(v) => *v as i64 as u64,
                    IrConst::F64(v) => *v as i64 as u64,
                    _ => val.to_i64().unwrap_or(0) as u64,
                };
                Some((bits, true))
            }
        }
    }

    /// Resolve a variable name to its global name, checking static local names first.
    fn resolve_to_global_name(&self, name: &str) -> Option<String> {
        if let Some(ref fs) = self.func_state {
            if let Some(mangled) = fs.static_local_names.get(name) {
                return Some(mangled.clone());
            }
        }
        if self.globals.contains_key(name) {
            Some(name.to_string())
        } else {
            None
        }
    }

    /// Try to evaluate an expression as a global address constant.
    /// This handles patterns like:
    /// - `&x` (address of a global variable)
    /// - `func` (function name used as pointer)
    /// - `arr` (array name decays to pointer)
    /// - `&arr[3]` (address of array element with constant index)
    /// - `&s.field` (address of struct field)
    /// - `(type *)&x` (cast of address expression)
    /// - `&x + n` or `&x - n` (pointer arithmetic on global address)
    pub(super) fn eval_global_addr_expr(&self, expr: &Expr) -> Option<GlobalInit> {
        match expr {
            // &x -> GlobalAddr("x")
            Expr::AddressOf(inner, _) => {
                match inner.as_ref() {
                    Expr::Identifier(name, _) => {
                        // Check static local names first (local statics shadow globals)
                        if let Some(ref fs) = self.func_state {
                            if let Some(mangled) = fs.static_local_names.get(name) {
                                return Some(GlobalInit::GlobalAddr(mangled.clone()));
                            }
                        }
                        // Address of a global variable or function
                        if self.globals.contains_key(name) || self.known_functions.contains(name) {
                            return Some(GlobalInit::GlobalAddr(name.clone()));
                        }
                        None
                    }
                    // &arr[index] -> GlobalAddrOffset("arr", index * elem_size)
                    Expr::ArraySubscript(base, index, _) => {
                        if let Expr::Identifier(name, _) = base.as_ref() {
                            if let Some(global_name) = self.resolve_to_global_name(name) {
                                if let Some(ginfo) = self.globals.get(&global_name) {
                                    if ginfo.is_array {
                                        if let Some(idx_val) = self.eval_const_expr(index) {
                                            if let Some(idx) = self.const_to_i64(&idx_val) {
                                                let offset = idx * ginfo.elem_size as i64;
                                                if offset == 0 {
                                                    return Some(GlobalInit::GlobalAddr(global_name));
                                                }
                                                return Some(GlobalInit::GlobalAddrOffset(global_name, offset));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        None
                    }
                    // &s.field or &s.a.b.c -> GlobalAddrOffset("s", total_field_offset)
                    Expr::MemberAccess(_, _, _) => {
                        self.resolve_chained_member_access(inner)
                    }
                    _ => None,
                }
            }
            // Function name as pointer: void (*fp)(void) = func;
            Expr::Identifier(name, _) => {
                if self.known_functions.contains(name) {
                    return Some(GlobalInit::GlobalAddr(name.clone()));
                }
                // Check static local array names first (they shadow globals)
                if let Some(ref fs) = self.func_state {
                    if let Some(mangled) = fs.static_local_names.get(name) {
                        if let Some(ginfo) = self.globals.get(mangled) {
                            if ginfo.is_array {
                                return Some(GlobalInit::GlobalAddr(mangled.clone()));
                            }
                        }
                    }
                }
                // Array name decays to pointer: int *p = arr;
                if let Some(ginfo) = self.globals.get(name) {
                    if ginfo.is_array {
                        return Some(GlobalInit::GlobalAddr(name.clone()));
                    }
                }
                None
            }
            // (type *)expr -> try evaluating the inner expression
            Expr::Cast(_, inner, _) => {
                self.eval_global_addr_expr(inner)
            }
            // &x + n or arr + n -> GlobalAddrOffset with byte offset
            Expr::BinaryOp(BinOp::Add, lhs, rhs, _) => {
                // Try lhs as address, rhs as constant offset
                if let Some(addr) = self.eval_global_addr_base_and_offset(lhs, rhs) {
                    return Some(addr);
                }
                // Try rhs as address, lhs as constant offset (commutative)
                if let Some(addr) = self.eval_global_addr_base_and_offset(rhs, lhs) {
                    return Some(addr);
                }
                None
            }
            // &x - n -> GlobalAddrOffset with negative byte offset
            Expr::BinaryOp(BinOp::Sub, lhs, rhs, _) => {
                if let Some(base_init) = self.eval_global_addr_expr(lhs) {
                    if let Some(offset_val) = self.eval_const_expr(rhs) {
                        if let Some(offset) = self.const_to_i64(&offset_val) {
                            // If the expression was cast to an integer type
                            // (e.g., (uintptr_t)ptr - 1), use byte-level arithmetic.
                            let elem_size = if self.expr_is_pointer(lhs) {
                                self.get_pointer_elem_size_from_expr(lhs)
                            } else {
                                1
                            };
                            let byte_offset = -(offset * elem_size as i64);
                            match base_init {
                                GlobalInit::GlobalAddr(name) => {
                                    if byte_offset == 0 {
                                        return Some(GlobalInit::GlobalAddr(name));
                                    }
                                    return Some(GlobalInit::GlobalAddrOffset(name, byte_offset));
                                }
                                GlobalInit::GlobalAddrOffset(name, base_off) => {
                                    let total = base_off + byte_offset;
                                    if total == 0 {
                                        return Some(GlobalInit::GlobalAddr(name));
                                    }
                                    return Some(GlobalInit::GlobalAddrOffset(name, total));
                                }
                                _ => {}
                            }
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Helper for pointer arithmetic: base_expr + offset_expr where base is an address
    fn eval_global_addr_base_and_offset(&self, base_expr: &Expr, offset_expr: &Expr) -> Option<GlobalInit> {
        let base_init = self.eval_global_addr_expr(base_expr)?;
        let offset_val = self.eval_const_expr(offset_expr)?;
        let offset = self.const_to_i64(&offset_val)?;
        // If the expression was cast to an integer type (e.g., (uintptr_t)ptr + 1),
        // arithmetic is byte-level (scale factor 1), not pointer-scaled.
        let elem_size = if self.expr_is_pointer(base_expr) {
            self.get_pointer_elem_size_from_expr(base_expr)
        } else {
            1
        };
        let byte_offset = offset * elem_size as i64;
        match base_init {
            GlobalInit::GlobalAddr(name) => {
                if byte_offset == 0 {
                    Some(GlobalInit::GlobalAddr(name))
                } else {
                    Some(GlobalInit::GlobalAddrOffset(name, byte_offset))
                }
            }
            GlobalInit::GlobalAddrOffset(name, base_off) => {
                let total = base_off + byte_offset;
                if total == 0 {
                    Some(GlobalInit::GlobalAddr(name))
                } else {
                    Some(GlobalInit::GlobalAddrOffset(name, total))
                }
            }
            _ => None,
        }
    }

    /// Resolve a chained member access expression like `s.a.b.c` to a global address.
    /// Walks the chain from the root identifier, accumulating field offsets.
    fn resolve_chained_member_access(&self, expr: &Expr) -> Option<GlobalInit> {
        // Collect the chain of field names from innermost to outermost
        let mut fields = Vec::new();
        let mut current = expr;
        loop {
            match current {
                Expr::MemberAccess(base, field, _) => {
                    fields.push(field.clone());
                    current = base.as_ref();
                }
                Expr::Identifier(name, _) => {
                    let global_name = self.resolve_to_global_name(name)?;
                    let ginfo = self.globals.get(&global_name)?;
                    // Walk from the root struct through the field chain
                    let mut total_offset: i64 = 0;
                    let mut current_layout = ginfo.struct_layout.clone()?;
                    // fields are in reverse order (outermost field last)
                    for field_name in fields.iter().rev() {
                        let mut found = false;
                        for f in &current_layout.fields {
                            if f.name == *field_name {
                                total_offset += f.offset as i64;
                                // Try to get the layout of this field for further chaining
                                current_layout = match &f.ty {
                                    CType::Struct(key) | CType::Union(key) => {
                                        self.types.struct_layouts.get(key).cloned()
                                            .unwrap_or_else(StructLayout::empty)
                                    }
                                    _ => StructLayout::empty(),
                                };
                                found = true;
                                break;
                            }
                        }
                        if !found {
                            return None;
                        }
                    }
                    if total_offset == 0 {
                        return Some(GlobalInit::GlobalAddr(global_name));
                    }
                    return Some(GlobalInit::GlobalAddrOffset(global_name, total_offset));
                }
                _ => return None,
            }
        }
    }

    /// Evaluate a constant binary operation.
    /// Uses both operand types for C's usual arithmetic conversions (C11 6.3.1.8).
    /// Delegates arithmetic to the shared implementation in `common::const_arith`.
    fn eval_const_binop(&self, op: &BinOp, lhs: &IrConst, rhs: &IrConst, lhs_ty: IrType, rhs_ty: IrType) -> Option<IrConst> {
        // Apply usual arithmetic conversions: use the wider of both operand types
        let lhs_size = lhs_ty.size().max(4);
        let rhs_size = rhs_ty.size().max(4);
        let result_size = lhs_size.max(rhs_size);
        let is_32bit = result_size <= 4;
        let is_unsigned = if lhs_size == rhs_size {
            lhs_ty.is_unsigned() || rhs_ty.is_unsigned()
        } else if lhs_size > rhs_size {
            lhs_ty.is_unsigned()
        } else {
            rhs_ty.is_unsigned()
        };
        const_arith::eval_const_binop(op, lhs, rhs, is_32bit, is_unsigned)
    }

    /// Compute the effective array size from an initializer list with potential designators.
    /// Returns the minimum array size needed to hold all designated (and positional) elements.
    /// For char arrays, handles brace-wrapped string literals: char c[] = {"hello"} -> size = 6
    pub(super) fn compute_init_list_array_size_for_char_array(
        &self,
        items: &[InitializerItem],
        base_ty: IrType,
    ) -> usize {
        // Special case: char c[] = {"hello"} - single brace-wrapped string literal
        if (base_ty == IrType::I8 || base_ty == IrType::U8)
            && items.len() == 1
            && items[0].designators.is_empty()
        {
            if let Initializer::Expr(Expr::StringLiteral(s, _)) = &items[0].init {
                return s.chars().count() + 1; // +1 for null terminator
            }
            if let Initializer::Expr(Expr::WideStringLiteral(s, _)) = &items[0].init {
                return s.chars().count() + 1; // wide string to char array
            }
        }
        // Special case: wchar_t w[] = {L"hello"} - single brace-wrapped wide string literal
        if base_ty == IrType::I32
            && items.len() == 1
            && items[0].designators.is_empty()
        {
            if let Initializer::Expr(Expr::WideStringLiteral(s, _)) = &items[0].init {
                return s.chars().count() + 1; // +1 for null terminator (count in wchar_t elements)
            }
            if let Initializer::Expr(Expr::StringLiteral(s, _)) = &items[0].init {
                return s.len() + 1; // narrow string to wchar_t array (each byte is an element)
            }
        }
        self.compute_init_list_array_size(items)
    }

    /// Compute the effective array size from an initializer list with potential designators.
    /// Returns the minimum array size needed to hold all designated (and positional) elements.
    pub(super) fn compute_init_list_array_size(&self, items: &[InitializerItem]) -> usize {
        let mut max_idx = 0usize;
        let mut current_idx = 0usize;
        for item in items {
            if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                if let Some(idx) = self.eval_const_expr_for_designator(idx_expr) {
                    current_idx = idx;
                }
            }
            if current_idx >= max_idx {
                max_idx = current_idx + 1;
            }
            // Only advance to next element if this item does NOT have a field
            // designator (e.g., [0].field = val should NOT advance the index,
            // since multiple items may target different fields of the same element)
            let has_field_designator = item.designators.iter().any(|d| matches!(d, Designator::Field(_)));
            if !has_field_designator {
                current_idx += 1;
            }
        }
        // For non-designated cases, each item is one element so use items.len().
        // For designated cases, max_idx already accounts for the correct count.
        let has_any_designator = items.iter().any(|item| !item.designators.is_empty());
        if has_any_designator {
            max_idx
        } else {
            max_idx.max(items.len())
        }
    }

    /// Compute the number of scalar initializer items needed to flat-initialize a CType.
    /// For scalar types: 1. For arrays: element_count * scalars_per_element.
    /// For structs/unions: sum of scalar counts of all fields (union uses max field count).
    fn flat_scalar_count(&self, ty: &CType) -> usize {
        match ty {
            CType::Array(elem_ty, Some(size)) => {
                *size * self.flat_scalar_count(elem_ty)
            }
            CType::Array(_, None) => {
                // Unsized array treated as 0 for counting purposes
                0
            }
            CType::Struct(key) | CType::Union(key) => {
                if let Some(layout) = self.types.struct_layouts.get(key) {
                    self.flat_scalar_count_for_layout(layout)
                } else {
                    1
                }
            }
            // All scalar types (int, float, pointer, etc.) consume 1 initializer item
            _ => 1,
        }
    }

    /// Compute the flat scalar count for a struct/union layout.
    /// For structs: sum of flat scalar counts of all fields.
    /// For unions: the maximum flat scalar count among fields (since only one is initialized).
    fn flat_scalar_count_for_layout(&self, layout: &StructLayout) -> usize {
        if layout.fields.is_empty() {
            return 0;
        }
        if layout.is_union {
            // Union: only one field is initialized, use max for sizing purposes
            layout.fields.iter()
                .map(|f| self.flat_scalar_count(&f.ty))
                .max()
                .unwrap_or(1)
        } else {
            // Struct: all fields are initialized sequentially
            layout.fields.iter()
                .map(|f| self.flat_scalar_count(&f.ty))
                .sum()
        }
    }

    /// Compute the number of struct elements in a flat initializer list for an unsized
    /// array of structs. Handles both braced (each item is one struct) and flat (items
    /// fill struct fields sequentially) initialization styles, as well as [idx] designators.
    /// E.g., struct {int a; char b;} x[] = {1, 'c', 2, 'd'} -> 2 elements
    ///       struct {int a; char b;} x[] = {{1,'c'}, {2,'d'}} -> 2 elements
    ///       struct {int a; char b;} x[] = {[2] = {1,'c'}} -> 3 elements (indices 0,1,2)
    pub(super) fn compute_struct_array_init_count(
        &self,
        items: &[InitializerItem],
        layout: &StructLayout,
    ) -> usize {
        // Use flat scalar count: accounts for array fields consuming multiple items
        let flat_count = self.flat_scalar_count_for_layout(layout);
        if flat_count == 0 {
            return items.len();
        }

        // Build per-field flat scalar counts for mapping fields_consumed to field index
        let field_scalar_counts: Vec<usize> = layout
            .fields
            .iter()
            .map(|f| self.flat_scalar_count(&f.ty))
            .collect();

        let mut max_idx = 0usize;
        let mut current_idx = 0usize;
        let mut fields_consumed = 0usize;

        for item in items {
            // Check for [idx] designator (array index)
            if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                if let Some(idx) = self.eval_const_expr_for_designator(idx_expr) {
                    current_idx = idx;
                    fields_consumed = 0; // Reset field counter for new struct element
                }
            }

            // Check if this item starts a new struct element (braced or designator)
            let is_braced = matches!(item.init, Initializer::List(_));
            let has_field_designator = item.designators.iter().any(|d| matches!(d, Designator::Field(_)));

            if is_braced || has_field_designator {
                // Each braced item or field-designated item is one struct element
                // (or part of the current struct element if field-designated)
                if has_field_designator {
                    // .field = val stays in current struct element
                } else {
                    // Braced list is one complete struct element
                    if fields_consumed > 0 {
                        // We were in the middle of a flat init - advance to next
                        current_idx += 1;
                        fields_consumed = 0;
                    }
                }
                if current_idx >= max_idx {
                    max_idx = current_idx + 1;
                }
                if !has_field_designator {
                    current_idx += 1;
                }
            } else {
                // Flat init: determine how many scalar slots this item consumes.
                // A string literal initializing a char/wchar_t array field fills the
                // entire array, not just one scalar slot.
                let slots = if self.flat_init_item_is_string_for_char_array(
                    &item.init,
                    fields_consumed,
                    &field_scalar_counts,
                    layout,
                ) {
                    // String literal fills the entire current char array field
                    self.remaining_scalars_in_current_field(fields_consumed, &field_scalar_counts)
                } else {
                    1
                };
                fields_consumed += slots;
                if fields_consumed >= flat_count {
                    // Completed one struct element
                    if current_idx >= max_idx {
                        max_idx = current_idx + 1;
                    }
                    current_idx += 1;
                    fields_consumed = 0;
                }
            }
        }

        // If there are remaining fields consumed, count the partial struct element
        if fields_consumed > 0 {
            if current_idx >= max_idx {
                max_idx = current_idx + 1;
            }
        }

        max_idx
    }

    /// Check if a flat initializer item is a string literal that initializes a char array field.
    fn flat_init_item_is_string_for_char_array(
        &self,
        init: &Initializer,
        fields_consumed: usize,
        field_scalar_counts: &[usize],
        layout: &StructLayout,
    ) -> bool {
        let is_string = matches!(
            init,
            Initializer::Expr(Expr::StringLiteral(_, _) | Expr::WideStringLiteral(_, _))
        );
        if !is_string {
            return false;
        }
        // Find which field index corresponds to fields_consumed
        let mut remaining = fields_consumed;
        for (fi, &count) in field_scalar_counts.iter().enumerate() {
            if remaining < count {
                // This is the field at index fi, check if it's a char/wchar_t array
                return Self::is_char_array_type(&layout.fields[fi].ty);
            }
            remaining -= count;
        }
        false
    }

    /// Compute how many scalar slots remain in the current field given a flat position.
    fn remaining_scalars_in_current_field(
        &self,
        fields_consumed: usize,
        field_scalar_counts: &[usize],
    ) -> usize {
        let mut remaining = fields_consumed;
        for &count in field_scalar_counts {
            if remaining < count {
                return count - remaining;
            }
            remaining -= count;
        }
        1
    }

    /// Check if a CType is a char or wchar_t array (eligible for string literal initialization).
    fn is_char_array_type(ty: &CType) -> bool {
        match ty {
            CType::Array(elem, _) => {
                matches!(
                    elem.as_ref(),
                    CType::Char | CType::UChar
                    | CType::Int | CType::UInt
                )
            }
            _ => false,
        }
    }

    /// Evaluate a constant expression and return as usize (for array index designators).
    pub(super) fn eval_const_expr_for_designator(&self, expr: &Expr) -> Option<usize> {
        self.eval_const_expr(expr).and_then(|v| v.to_usize())
    }

    /// Convert an IrConst to i64. Delegates to IrConst::to_i64().
    pub(super) fn const_to_i64(&self, c: &IrConst) -> Option<i64> {
        c.to_i64()
    }

    /// Coerce a constant to the target type, using the source expression's type for signedness.
    pub(super) fn coerce_const_to_type_with_src(&self, val: IrConst, target_ty: IrType, src_ty: IrType) -> IrConst {
        val.coerce_to_with_src(target_ty, Some(src_ty))
    }

    /// Collect array dimensions from nested Array type specifiers.
    /// Extract an integer value from any integer literal expression (Int, UInt, Long, ULong).
    /// Used for array sizes and other compile-time integer expressions.
    pub(super) fn expr_as_array_size(&self, expr: &Expr) -> Option<i64> {
        // Try simple literals first (fast path)
        match expr {
            Expr::IntLiteral(n, _) | Expr::LongLiteral(n, _) => return Some(*n),
            Expr::UIntLiteral(n, _) | Expr::ULongLiteral(n, _) => return Some(*n as i64),
            _ => {}
        }
        // Fall back to full constant expression evaluation (handles sizeof, arithmetic, etc.)
        if let Some(val) = self.eval_const_expr(expr) {
            return self.const_to_i64(&val);
        }
        None
    }
}

