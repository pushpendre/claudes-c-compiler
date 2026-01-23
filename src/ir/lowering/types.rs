use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, StructField, StructLayout, CType};
use crate::common::source::Span;
use super::lowering::Lowerer;

impl Lowerer {
    /// Try to evaluate a constant expression at compile time.
    pub(super) fn eval_const_expr(&self, expr: &Expr) -> Option<IrConst> {
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
            Expr::UnaryOp(UnaryOp::Plus, inner, _) => {
                // Unary plus: identity, just evaluate the inner expression
                self.eval_const_expr(inner)
            }
            Expr::UnaryOp(UnaryOp::Neg, inner, _) => {
                if let Some(val) = self.eval_const_expr(inner) {
                    match val {
                        IrConst::I64(v) => Some(IrConst::I64(-v)),
                        IrConst::I32(v) => Some(IrConst::I32(-v)),
                        IrConst::F64(v) => Some(IrConst::F64(-v)),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            Expr::BinaryOp(op, lhs, rhs, _) => {
                let l = self.eval_const_expr(lhs)?;
                let r = self.eval_const_expr(rhs)?;
                // For shift and arithmetic operations, we need the LHS type for signedness/width
                let lhs_ty = self.get_expr_type(lhs);
                self.eval_const_binop(op, &l, &r, lhs_ty)
            }
            Expr::UnaryOp(UnaryOp::BitNot, inner, _) => {
                if let Some(val) = self.eval_const_expr(inner) {
                    match val {
                        IrConst::I64(v) => Some(IrConst::I64(!v)),
                        IrConst::I32(v) => Some(IrConst::I32(!v)),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            Expr::Cast(ref target_type, inner, _) => {
                // Evaluate cast chains properly using (bits, is_signed, width) representation
                let (bits, _src_signed) = self.eval_const_expr_as_bits(inner)?;
                let target_ir_ty = self.type_spec_to_ir(target_type);
                let target_width = target_ir_ty.size() * 8;
                let target_signed = matches!(target_ir_ty, IrType::I8 | IrType::I16 | IrType::I32 | IrType::I64);

                // Truncate to target width
                let truncated = if target_width >= 64 {
                    bits
                } else {
                    bits & ((1u64 << target_width) - 1)
                };

                // Convert to IrConst based on target type
                // For unsigned types, use I64 to preserve zero-extension semantics
                // (IrConst::I32 would sign-extend when later converted to i64)
                let result = match target_ir_ty {
                    IrType::I8 => IrConst::I8(truncated as i8),
                    IrType::U8 => IrConst::I64(truncated as u8 as i64),
                    IrType::I16 => IrConst::I16(truncated as i16),
                    IrType::U16 => IrConst::I64(truncated as u16 as i64),
                    IrType::I32 => IrConst::I32(truncated as i32),
                    IrType::U32 => IrConst::I64(truncated as u32 as i64),
                    IrType::I64 | IrType::U64 | IrType::Ptr => IrConst::I64(truncated as i64),
                    IrType::F32 => {
                        let int_val = if target_signed { truncated as i64 as f32 } else { truncated as f32 };
                        IrConst::F32(int_val)
                    }
                    IrType::F64 => {
                        let int_val = if target_signed { truncated as i64 as f64 } else { truncated as f64 };
                        IrConst::F64(int_val)
                    }
                    _ => return None,
                };
                Some(result)
            }
            Expr::Identifier(name, _) => {
                // Look up enum constants
                if let Some(&val) = self.enum_constants.get(name) {
                    Some(IrConst::I64(val))
                } else {
                    None
                }
            }
            Expr::Sizeof(arg, _) => {
                let size = match arg.as_ref() {
                    SizeofArg::Type(ts) => self.sizeof_type(ts),
                    SizeofArg::Expr(e) => self.sizeof_expr(e),
                };
                Some(IrConst::I64(size as i64))
            }
            Expr::Conditional(cond, then_e, else_e, _) => {
                // Ternary in constant expr: evaluate condition and pick branch
                let cond_val = self.eval_const_expr(cond)?;
                let is_true = match cond_val {
                    IrConst::I64(v) => v != 0,
                    IrConst::I32(v) => v != 0,
                    IrConst::I8(v) => v != 0,
                    IrConst::I16(v) => v != 0,
                    _ => return None,
                };
                if is_true {
                    self.eval_const_expr(then_e)
                } else {
                    self.eval_const_expr(else_e)
                }
            }
            Expr::UnaryOp(UnaryOp::LogicalNot, inner, _) => {
                let val = self.eval_const_expr(inner)?;
                let result = match val {
                    IrConst::I64(v) => if v == 0 { 1i64 } else { 0 },
                    IrConst::I32(v) => if v == 0 { 1i64 } else { 0 },
                    _ => return None,
                };
                Some(IrConst::I64(result))
            }
            _ => None,
        }
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

                // Truncate to target width
                let truncated = if target_width >= 64 {
                    bits
                } else {
                    bits & ((1u64 << target_width) - 1)
                };

                // If target is signed, sign-extend to 64 bits for the next operation
                let result = if target_signed && target_width < 64 {
                    // Sign-extend
                    let sign_bit = 1u64 << (target_width - 1);
                    if truncated & sign_bit != 0 {
                        truncated | !((1u64 << target_width) - 1)
                    } else {
                        truncated
                    }
                } else {
                    // Zero-extend (unsigned or already 64 bits)
                    truncated
                };

                Some((result, target_signed))
            }
            _ => {
                // For non-cast expressions, evaluate normally and convert to bits
                let val = self.eval_const_expr(expr)?;
                // Sign-extend to i64, then interpret as u64 bit pattern
                let bits = match &val {
                    IrConst::F32(v) => *v as i64 as u64,
                    IrConst::F64(v) => *v as i64 as u64,
                    _ => val.to_i64().unwrap_or(0) as u64,
                };
                Some((bits, true))
            }
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
                        // Address of a global variable or function
                        if self.globals.contains_key(name) || self.known_functions.contains(name) {
                            return Some(GlobalInit::GlobalAddr(name.clone()));
                        }
                        // Check if this is a static local variable (stored under mangled name)
                        if let Some(mangled) = self.static_local_names.get(name) {
                            return Some(GlobalInit::GlobalAddr(mangled.clone()));
                        }
                        None
                    }
                    // &arr[index] -> GlobalAddrOffset("arr", index * elem_size)
                    Expr::ArraySubscript(base, index, _) => {
                        if let Expr::Identifier(name, _) = base.as_ref() {
                            // Resolve name: check globals directly, then static local aliases
                            let resolved = if self.globals.contains_key(name.as_str()) {
                                Some(name.clone())
                            } else {
                                self.static_local_names.get(name.as_str()).cloned()
                            };
                            if let Some(global_name) = resolved {
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
                    // &s.field -> GlobalAddrOffset("s", field_offset)
                    Expr::MemberAccess(base, field, _) => {
                        if let Expr::Identifier(name, _) = base.as_ref() {
                            // Resolve name: check globals directly, then static local aliases
                            let resolved = if self.globals.contains_key(name.as_str()) {
                                Some(name.clone())
                            } else {
                                self.static_local_names.get(name.as_str()).cloned()
                            };
                            if let Some(global_name) = resolved {
                                if let Some(ginfo) = self.globals.get(&global_name) {
                                    if let Some(ref layout) = ginfo.struct_layout {
                                        for f in &layout.fields {
                                            if f.name == *field {
                                                if f.offset == 0 {
                                                    return Some(GlobalInit::GlobalAddr(global_name));
                                                }
                                                return Some(GlobalInit::GlobalAddrOffset(global_name, f.offset as i64));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        None
                    }
                    _ => None,
                }
            }
            // Function name as pointer: void (*fp)(void) = func;
            Expr::Identifier(name, _) => {
                if self.known_functions.contains(name) {
                    return Some(GlobalInit::GlobalAddr(name.clone()));
                }
                // Array name decays to pointer: int *p = arr;
                if let Some(ginfo) = self.globals.get(name) {
                    if ginfo.is_array {
                        return Some(GlobalInit::GlobalAddr(name.clone()));
                    }
                }
                // Check static local array names
                if let Some(mangled) = self.static_local_names.get(name) {
                    if let Some(ginfo) = self.globals.get(mangled) {
                        if ginfo.is_array {
                            return Some(GlobalInit::GlobalAddr(mangled.clone()));
                        }
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
                            let elem_size = self.get_pointer_elem_size_from_expr(lhs);
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
        let elem_size = self.get_pointer_elem_size_from_expr(base_expr);
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

    /// Evaluate a constant binary operation.
    fn eval_const_binop(&self, op: &BinOp, lhs: &IrConst, rhs: &IrConst, lhs_ty: IrType) -> Option<IrConst> {
        let l = self.const_to_i64(lhs)?;
        let r = self.const_to_i64(rhs)?;
        let is_32bit = lhs_ty.size() <= 4;
        let is_unsigned = lhs_ty.is_unsigned();
        let result = match op {
            BinOp::Add => {
                let v = l.wrapping_add(r);
                if is_32bit { v as i32 as i64 } else { v }
            }
            BinOp::Sub => {
                let v = l.wrapping_sub(r);
                if is_32bit { v as i32 as i64 } else { v }
            }
            BinOp::Mul => {
                let v = l.wrapping_mul(r);
                if is_32bit { v as i32 as i64 } else { v }
            }
            BinOp::Div => {
                if r == 0 { return None; }
                if is_unsigned {
                    if is_32bit {
                        ((l as u32).wrapping_div(r as u32)) as i64
                    } else {
                        ((l as u64).wrapping_div(r as u64)) as i64
                    }
                } else {
                    let v = l.wrapping_div(r);
                    if is_32bit { v as i32 as i64 } else { v }
                }
            }
            BinOp::Mod => {
                if r == 0 { return None; }
                if is_unsigned {
                    if is_32bit {
                        ((l as u32).wrapping_rem(r as u32)) as i64
                    } else {
                        ((l as u64).wrapping_rem(r as u64)) as i64
                    }
                } else {
                    let v = l.wrapping_rem(r);
                    if is_32bit { v as i32 as i64 } else { v }
                }
            }
            BinOp::BitAnd => l & r,
            BinOp::BitOr => l | r,
            BinOp::BitXor => l ^ r,
            BinOp::Shl => {
                let v = l.wrapping_shl(r as u32);
                if is_32bit { v as i32 as i64 } else { v }
            }
            BinOp::Shr => {
                if is_unsigned {
                    if is_32bit {
                        ((l as u32).wrapping_shr(r as u32)) as i64
                    } else {
                        (l as u64).wrapping_shr(r as u32) as i64
                    }
                } else {
                    if is_32bit {
                        (l as i32).wrapping_shr(r as u32) as i64
                    } else {
                        l.wrapping_shr(r as u32)
                    }
                }
            }
            BinOp::Eq => if l == r { 1 } else { 0 },
            BinOp::Ne => if l != r { 1 } else { 0 },
            BinOp::Lt => {
                if is_unsigned { if (l as u64) < (r as u64) { 1 } else { 0 } }
                else { if l < r { 1 } else { 0 } }
            },
            BinOp::Gt => {
                if is_unsigned { if (l as u64) > (r as u64) { 1 } else { 0 } }
                else { if l > r { 1 } else { 0 } }
            },
            BinOp::Le => {
                if is_unsigned { if (l as u64) <= (r as u64) { 1 } else { 0 } }
                else { if l <= r { 1 } else { 0 } }
            },
            BinOp::Ge => {
                if is_unsigned { if (l as u64) >= (r as u64) { 1 } else { 0 } }
                else { if l >= r { 1 } else { 0 } }
            },
            BinOp::LogicalAnd => if l != 0 && r != 0 { 1 } else { 0 },
            BinOp::LogicalOr => if l != 0 || r != 0 { 1 } else { 0 },
            _ => return None,
        };
        Some(IrConst::I64(result))
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
                return s.as_bytes().len() + 1; // +1 for null terminator
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
            current_idx += 1;
        }
        // At minimum, the array size equals items.len() for non-designated cases
        max_idx.max(items.len())
    }

    /// Evaluate a constant expression and return as usize (for array index designators).
    pub(super) fn eval_const_expr_for_designator(&self, expr: &Expr) -> Option<usize> {
        self.eval_const_expr(expr).and_then(|v| v.to_usize())
    }

    /// Convert an IrConst to i64. Delegates to IrConst::to_i64().
    pub(super) fn const_to_i64(&self, c: &IrConst) -> Option<i64> {
        c.to_i64()
    }

    /// Coerce an IrConst to match a target IrType. Delegates to IrConst::coerce_to().
    pub(super) fn coerce_const_to_type(&self, val: IrConst, target_ty: IrType) -> IrConst {
        val.coerce_to(target_ty)
    }

    /// Get the zero constant for a given IR type.
    pub(super) fn zero_const(&self, ty: IrType) -> IrConst {
        if ty == IrType::Void { IrConst::Zero } else { IrConst::zero(ty) }
    }

    /// Check if an expression refers to a struct/union value (not pointer-to-struct).
    pub(super) fn expr_is_struct_value(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(info) = self.locals.get(name) {
                    return info.is_struct;
                }
                if let Some(ginfo) = self.globals.get(name) {
                    return ginfo.is_struct;
                }
                false
            }
            Expr::MemberAccess(base_expr, field_name, _) => {
                if let Some(ctype) = self.resolve_member_field_ctype(base_expr, field_name) {
                    return matches!(ctype, CType::Struct(_) | CType::Union(_));
                }
                false
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                if let Some(ctype) = self.resolve_pointer_member_field_ctype(base_expr, field_name) {
                    return matches!(ctype, CType::Struct(_) | CType::Union(_));
                }
                false
            }
            Expr::ArraySubscript(_, _, _) => {
                // e.g. p->t[3] where t is an array of structs
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    return matches!(ctype, CType::Struct(_) | CType::Union(_));
                }
                false
            }
            Expr::Deref(_, _) => {
                // *ptr where ptr points to struct - could be struct value
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    return matches!(ctype, CType::Struct(_) | CType::Union(_));
                }
                false
            }
            _ => false,
        }
    }

    /// Get the struct size for a struct-valued expression.
    pub(super) fn get_struct_size_for_expr(&self, expr: &Expr) -> usize {
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(info) = self.locals.get(name) {
                    if info.is_struct {
                        return info.alloc_size;
                    }
                }
                if let Some(ginfo) = self.globals.get(name) {
                    if ginfo.is_struct {
                        if let Some(ref layout) = ginfo.struct_layout {
                            return layout.size;
                        }
                    }
                }
                8 // fallback
            }
            Expr::MemberAccess(base_expr, field_name, _) => {
                if let Some(ctype) = self.resolve_member_field_ctype(base_expr, field_name) {
                    return ctype.size();
                }
                8
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                if let Some(ctype) = self.resolve_pointer_member_field_ctype(base_expr, field_name) {
                    return ctype.size();
                }
                8
            }
            Expr::ArraySubscript(_, _, _) => {
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    return ctype.size();
                }
                8
            }
            Expr::Deref(_, _) => {
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    return ctype.size();
                }
                8
            }
            _ => 8,
        }
    }

    /// Get the IR type for an expression (best-effort, based on locals/globals info).
    pub(super) fn get_expr_type(&self, expr: &Expr) -> IrType {
        match expr {
            Expr::IntLiteral(_, _) | Expr::LongLiteral(_, _) | Expr::CharLiteral(_, _) => return IrType::I64,
            Expr::UIntLiteral(_, _) | Expr::ULongLiteral(_, _) => return IrType::U64,
            Expr::FloatLiteral(_, _) => return IrType::F64,
            Expr::FloatLiteralF32(_, _) => return IrType::F32,
            Expr::StringLiteral(_, _) => return IrType::Ptr,
            Expr::Cast(ref target_type, _, _) => return self.type_spec_to_ir(target_type),
            Expr::UnaryOp(UnaryOp::Neg, inner, _) | Expr::UnaryOp(UnaryOp::Plus, inner, _)
            | Expr::UnaryOp(UnaryOp::PreInc, inner, _) | Expr::UnaryOp(UnaryOp::PreDec, inner, _)
            | Expr::UnaryOp(UnaryOp::BitNot, inner, _) => {
                return self.get_expr_type(inner);
            }
            Expr::BinaryOp(op, lhs, rhs, _) => {
                match op {
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge
                    | BinOp::LogicalAnd | BinOp::LogicalOr => return IrType::I64,
                    _ => {
                        let lty = self.get_expr_type(lhs);
                        let rty = self.get_expr_type(rhs);
                        if lty == IrType::F64 || rty == IrType::F64 {
                            return IrType::F64;
                        } else if lty == IrType::F32 || rty == IrType::F32 {
                            return IrType::F32;
                        }
                    }
                }
            }
            Expr::Assign(lhs, _, _) | Expr::CompoundAssign(_, lhs, _, _) => {
                return self.get_expr_type(lhs);
            }
            Expr::Conditional(_, then_expr, else_expr, _) => {
                let then_ty = self.get_expr_type(then_expr);
                let else_ty = self.get_expr_type(else_expr);
                return Self::common_type(then_ty, else_ty);
            }
            Expr::Comma(_, rhs, _) => return self.get_expr_type(rhs),
            Expr::PostfixOp(_, inner, _) => return self.get_expr_type(inner),
            Expr::AddressOf(_, _) => return IrType::Ptr,
            Expr::Sizeof(_, _) => return IrType::U64,  // sizeof returns size_t (unsigned long)
            Expr::FunctionCall(func, _, _) => {
                // Look up the return type of the called function
                if let Expr::Identifier(name, _) = func.as_ref() {
                    if let Some(&ret_ty) = self.function_return_types.get(name) {
                        return ret_ty;
                    }
                    if let Some(&ret_ty) = self.function_ptr_return_types.get(name) {
                        return ret_ty;
                    }
                }
                // For indirect calls (function pointers), determine return type from CType
                if let Some(ctype) = self.get_expr_ctype(func) {
                    match &ctype {
                        CType::Pointer(inner) => match inner.as_ref() {
                            CType::Function(ft) => return IrType::from_ctype(&ft.return_type),
                            CType::Pointer(ret) => match ret.as_ref() {
                                CType::Float => return IrType::F32,
                                CType::Double => return IrType::F64,
                                _ => {}
                            },
                            CType::Float => return IrType::F32,
                            CType::Double => return IrType::F64,
                            _ => {}
                        },
                        _ => {}
                    }
                }
                return IrType::I64;
            }
            Expr::VaArg(_, type_spec, _) => return self.resolve_va_arg_type(type_spec),
            _ => {}
        }
        match expr {
            Expr::Identifier(name, _) => {
                if name == "__func__" || name == "__FUNCTION__" || name == "__PRETTY_FUNCTION__" {
                    return IrType::Ptr;
                }
                if self.enum_constants.contains_key(name) {
                    return IrType::I32;
                }
                if let Some(info) = self.locals.get(name) {
                    return info.ty;
                }
                if let Some(ginfo) = self.globals.get(name) {
                    return ginfo.ty;
                }
                IrType::I64
            }
            Expr::ArraySubscript(base, index, _) => {
                // First try CType-based resolution (handles float arrays correctly)
                if let Some(base_ctype) = self.get_expr_ctype(base) {
                    match base_ctype {
                        CType::Array(elem, _) => return IrType::from_ctype(&elem),
                        CType::Pointer(pointee) => return IrType::from_ctype(&pointee),
                        _ => {}
                    }
                }
                // Also check reverse subscript (index[base])
                if let Some(idx_ctype) = self.get_expr_ctype(index) {
                    match idx_ctype {
                        CType::Array(elem, _) => return IrType::from_ctype(&elem),
                        CType::Pointer(pointee) => return IrType::from_ctype(&pointee),
                        _ => {}
                    }
                }
                // Fallback: For multi-dim arrays, find the root identifier
                let root_name = self.get_array_root_name(expr);
                if let Some(name) = root_name {
                    if let Some(info) = self.locals.get(&name) {
                        if info.is_array {
                            return info.ty;
                        }
                    }
                    if let Some(ginfo) = self.globals.get(&name) {
                        if ginfo.is_array {
                            return self.ir_type_for_elem_size(*ginfo.array_dim_strides.last().unwrap_or(&8));
                        }
                    }
                }
                // Check both base and index for identifier with type info
                // (handles reverse subscript like 3[arr] where index is the array)
                for operand in [base.as_ref(), index.as_ref()] {
                    if let Expr::Identifier(name, _) = operand {
                        if let Some(info) = self.locals.get(name) {
                            if let Some(pt) = info.pointee_type {
                                return pt;
                            }
                            if info.is_array {
                                return info.ty;
                            }
                        }
                        if let Some(ginfo) = self.globals.get(name) {
                            if let Some(pt) = ginfo.pointee_type {
                                return pt;
                            }
                            if ginfo.is_array {
                                return ginfo.ty;
                            }
                        }
                    }
                }
                // For struct/union member array access: s.arr[i] or p->arr[i]
                match base.as_ref() {
                    Expr::MemberAccess(base_expr, field_name, _) => {
                        if let Some(ctype) = self.resolve_member_field_ctype(base_expr, field_name) {
                            if let CType::Array(elem_ty, _) = &ctype {
                                return IrType::from_ctype(elem_ty);
                            }
                        }
                    }
                    Expr::PointerMemberAccess(base_expr, field_name, _) => {
                        if let Some(ctype) = self.resolve_pointer_member_field_ctype(base_expr, field_name) {
                            if let CType::Array(elem_ty, _) = &ctype {
                                return IrType::from_ctype(elem_ty);
                            }
                        }
                    }
                    _ => {}
                }
                // For complex base expressions, try to resolve pointee type
                if let Some(pt) = self.get_pointee_type_of_expr(base) {
                    return pt;
                }
                // Also try the index (reverse subscript with complex pointer expr)
                if let Some(pt) = self.get_pointee_type_of_expr(index) {
                    return pt;
                }
                IrType::I64
            }
            Expr::Deref(inner, _) => {
                // Dereference: use CType-based resolution for multi-level pointers
                if let Some(inner_ctype) = self.get_expr_ctype(inner) {
                    match inner_ctype {
                        CType::Pointer(pointee) => return IrType::from_ctype(&pointee),
                        CType::Array(elem, _) => return IrType::from_ctype(&elem),
                        _ => {}
                    }
                }
                // Fallback: use heuristic-based approach
                if let Some(pt) = self.get_pointee_type_of_expr(inner) {
                    return pt;
                }
                IrType::I64
            }
            Expr::MemberAccess(base_expr, field_name, _) => {
                let (_, field_ty) = self.resolve_member_access(base_expr, field_name);
                field_ty
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                let (_, field_ty) = self.resolve_pointer_member_access(base_expr, field_name);
                field_ty
            }
            _ => IrType::I64,
        }
    }

    /// Resolve a TypeSpecifier, following typedef chains.
    /// Returns the underlying TypeSpecifier (non-TypedefName).
    pub(super) fn resolve_type_spec<'a>(&'a self, ts: &'a TypeSpecifier) -> &'a TypeSpecifier {
        let mut current = ts;
        // Follow typedef chains (limit depth to prevent infinite loops)
        for _ in 0..32 {
            if let TypeSpecifier::TypedefName(name) = current {
                if let Some(resolved) = self.typedefs.get(name) {
                    current = resolved;
                    continue;
                }
            }
            break;
        }
        current
    }

    /// Pre-populate typedef mappings for builtin/standard types.
    pub(super) fn seed_builtin_typedefs(&mut self) {
        use TypeSpecifier::*;
        let builtins: &[(&str, TypeSpecifier)] = &[
            // <stddef.h>
            ("size_t", UnsignedLong),
            ("ssize_t", Long),
            ("ptrdiff_t", Long),
            ("wchar_t", Int),
            ("wint_t", UnsignedInt),
            // <stdint.h> - exact width types
            ("int8_t", Char),
            ("int16_t", Short),
            ("int32_t", Int),
            ("int64_t", Long),
            ("uint8_t", UnsignedChar),
            ("uint16_t", UnsignedShort),
            ("uint32_t", UnsignedInt),
            ("uint64_t", UnsignedLong),
            ("intptr_t", Long),
            ("uintptr_t", UnsignedLong),
            ("intmax_t", Long),
            ("uintmax_t", UnsignedLong),
            // least types
            ("int_least8_t", Char),
            ("int_least16_t", Short),
            ("int_least32_t", Int),
            ("int_least64_t", Long),
            ("uint_least8_t", UnsignedChar),
            ("uint_least16_t", UnsignedShort),
            ("uint_least32_t", UnsignedInt),
            ("uint_least64_t", UnsignedLong),
            // fast types
            ("int_fast8_t", Char),
            ("int_fast16_t", Long),
            ("int_fast32_t", Long),
            ("int_fast64_t", Long),
            ("uint_fast8_t", UnsignedChar),
            ("uint_fast16_t", UnsignedLong),
            ("uint_fast32_t", UnsignedLong),
            ("uint_fast64_t", UnsignedLong),
            // <signal.h>
            ("sig_atomic_t", Int),
            // <time.h>
            ("time_t", Long),
            ("clock_t", Long),
            ("timer_t", Pointer(Box::new(Void))),
            ("clockid_t", Int),
            // <sys/types.h>
            ("off_t", Long),
            ("pid_t", Int),
            ("uid_t", UnsignedInt),
            ("gid_t", UnsignedInt),
            ("mode_t", UnsignedInt),
            ("dev_t", UnsignedLong),
            ("ino_t", UnsignedLong),
            ("nlink_t", UnsignedLong),
            ("blksize_t", Long),
            ("blkcnt_t", Long),
            // GNU/glibc common
            ("ulong", UnsignedLong),
            ("ushort", UnsignedShort),
            ("uint", UnsignedInt),
            ("__u8", UnsignedChar),
            ("__u16", UnsignedShort),
            ("__u32", UnsignedInt),
            ("__u64", UnsignedLong),
            ("__s8", Char),
            ("__s16", Short),
            ("__s32", Int),
            ("__s64", Long),
            // <stdarg.h> - va_list is an array type __va_list_tag[1] (24 bytes on x86-64).
            // We represent it as Array(Char, 24) so it allocates 24 bytes on the stack
            // and decays to a pointer when passed to functions.
            ("va_list", Array(Box::new(Char), Some(Box::new(Expr::IntLiteral(24, Span::dummy()))))),
            ("__builtin_va_list", Array(Box::new(Char), Some(Box::new(Expr::IntLiteral(24, Span::dummy()))))),
            ("__gnuc_va_list", Array(Box::new(Char), Some(Box::new(Expr::IntLiteral(24, Span::dummy()))))),
            // <locale.h>
            ("locale_t", Pointer(Box::new(Void))),
            // <pthread.h> - opaque types, treat as unsigned long or pointer
            ("pthread_t", UnsignedLong),
            ("pthread_mutex_t", Pointer(Box::new(Void))),
            ("pthread_cond_t", Pointer(Box::new(Void))),
            ("pthread_key_t", UnsignedInt),
            ("pthread_attr_t", Pointer(Box::new(Void))),
            ("pthread_once_t", Int),
            ("pthread_mutexattr_t", Pointer(Box::new(Void))),
            ("pthread_condattr_t", Pointer(Box::new(Void))),
            // <setjmp.h>
            ("jmp_buf", Pointer(Box::new(Void))),
            ("sigjmp_buf", Pointer(Box::new(Void))),
            // <stdio.h>
            ("FILE", Pointer(Box::new(Void))),
            ("fpos_t", Long),
            // <dirent.h>
            ("DIR", Pointer(Box::new(Void))),
        ];
        for (name, ts) in builtins {
            self.typedefs.insert(name.to_string(), ts.clone());
        }
        // Also add the __u_char etc. POSIX internal names
        let posix_extras: &[(&str, TypeSpecifier)] = &[
            ("__u_char", UnsignedChar),
            ("__u_short", UnsignedShort),
            ("__u_int", UnsignedInt),
            ("__u_long", UnsignedLong),
            ("__int8_t", Char),
            ("__int16_t", Short),
            ("__int32_t", Int),
            ("__int64_t", Long),
            ("__uint8_t", UnsignedChar),
            ("__uint16_t", UnsignedShort),
            ("__uint32_t", UnsignedInt),
            ("__uint64_t", UnsignedLong),
        ];
        for (name, ts) in posix_extras {
            self.typedefs.insert(name.to_string(), ts.clone());
        }
    }

    /// Seed known libc math function signatures for correct calling convention.
    /// Without these, calls like atanf(1) would pass integer args in %rdi instead of %xmm0.
    pub(super) fn seed_libc_math_functions(&mut self) {
        use IrType::*;
        // float func(float) - single-precision math
        let f_f: &[&str] = &[
            "sinf", "cosf", "tanf", "asinf", "acosf", "atanf",
            "sinhf", "coshf", "tanhf", "asinhf", "acoshf", "atanhf",
            "expf", "exp2f", "expm1f", "logf", "log2f", "log10f", "log1pf",
            "sqrtf", "cbrtf", "fabsf", "ceilf", "floorf", "roundf", "truncf",
            "rintf", "nearbyintf", "erff", "erfcf", "tgammaf", "lgammaf",
        ];
        for name in f_f {
            self.function_return_types.insert(name.to_string(), F32);
            self.function_param_types.insert(name.to_string(), vec![F32]);
        }
        // double func(double) - double-precision math
        let d_d: &[&str] = &[
            "sin", "cos", "tan", "asin", "acos", "atan",
            "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
            "exp", "exp2", "expm1", "log", "log2", "log10", "log1p",
            "sqrt", "cbrt", "fabs", "ceil", "floor", "round", "trunc",
            "rint", "nearbyint", "erf", "erfc", "tgamma", "lgamma",
        ];
        for name in d_d {
            self.function_return_types.insert(name.to_string(), F64);
            self.function_param_types.insert(name.to_string(), vec![F64]);
        }
        // float func(float, float) - two-arg single-precision
        let f_ff: &[&str] = &["atan2f", "powf", "fmodf", "remainderf", "copysignf", "fminf", "fmaxf", "fdimf", "hypotf"];
        for name in f_ff {
            self.function_return_types.insert(name.to_string(), F32);
            self.function_param_types.insert(name.to_string(), vec![F32, F32]);
        }
        // double func(double, double) - two-arg double-precision
        let d_dd: &[&str] = &["atan2", "pow", "fmod", "remainder", "copysign", "fmin", "fmax", "fdim", "hypot"];
        for name in d_dd {
            self.function_return_types.insert(name.to_string(), F64);
            self.function_param_types.insert(name.to_string(), vec![F64, F64]);
        }
        // int/long returning functions
        self.function_return_types.insert("abs".to_string(), I32);
        self.function_param_types.insert("abs".to_string(), vec![I32]);
        self.function_return_types.insert("labs".to_string(), I64);
        self.function_param_types.insert("labs".to_string(), vec![I64]);
        // float func(float, int)
        self.function_return_types.insert("ldexpf".to_string(), F32);
        self.function_param_types.insert("ldexpf".to_string(), vec![F32, I32]);
        self.function_return_types.insert("ldexp".to_string(), F64);
        self.function_param_types.insert("ldexp".to_string(), vec![F64, I32]);
        self.function_return_types.insert("scalbnf".to_string(), F32);
        self.function_param_types.insert("scalbnf".to_string(), vec![F32, I32]);
        self.function_return_types.insert("scalbn".to_string(), F64);
        self.function_param_types.insert("scalbn".to_string(), vec![F64, I32]);
    }

    pub(super) fn type_spec_to_ir(&self, ts: &TypeSpecifier) -> IrType {
        let ts = self.resolve_type_spec(ts);
        match ts {
            TypeSpecifier::Void => IrType::Void,
            TypeSpecifier::Bool => IrType::U8, // _Bool is 1 byte, unsigned
            TypeSpecifier::Char => IrType::I8,
            TypeSpecifier::UnsignedChar => IrType::U8,
            TypeSpecifier::Short => IrType::I16,
            TypeSpecifier::UnsignedShort => IrType::U16,
            TypeSpecifier::Int => IrType::I32,
            TypeSpecifier::UnsignedInt => IrType::U32,
            TypeSpecifier::Long | TypeSpecifier::LongLong => IrType::I64,
            TypeSpecifier::UnsignedLong | TypeSpecifier::UnsignedLongLong => IrType::U64,
            TypeSpecifier::Float => IrType::F32,
            TypeSpecifier::Double | TypeSpecifier::LongDouble => IrType::F64,
            TypeSpecifier::Pointer(_) => IrType::Ptr,
            TypeSpecifier::Array(_, _) => IrType::Ptr,
            TypeSpecifier::Struct(_, _) | TypeSpecifier::Union(_, _) => IrType::Ptr,
            TypeSpecifier::Enum(_, _) => IrType::I32,
            TypeSpecifier::TypedefName(_) => IrType::I64, // fallback for unresolved typedef
            TypeSpecifier::Signed => IrType::I32,
            TypeSpecifier::Unsigned => IrType::U32,
        }
    }

    pub(super) fn sizeof_type(&self, ts: &TypeSpecifier) -> usize {
        let ts = self.resolve_type_spec(ts);
        match ts {
            TypeSpecifier::Void => 1, // GCC extension: sizeof(void) == 1
            TypeSpecifier::Bool => 1, // _Bool is 1 byte
            TypeSpecifier::Char | TypeSpecifier::UnsignedChar => 1,
            TypeSpecifier::Short | TypeSpecifier::UnsignedShort => 2,
            TypeSpecifier::Int | TypeSpecifier::UnsignedInt => 4,
            TypeSpecifier::Long | TypeSpecifier::UnsignedLong
            | TypeSpecifier::LongLong | TypeSpecifier::UnsignedLongLong => 8,
            TypeSpecifier::Float => 4,
            TypeSpecifier::Double => 8,
            TypeSpecifier::LongDouble => 16,
            TypeSpecifier::Pointer(_) => 8,
            TypeSpecifier::Array(elem, Some(size_expr)) => {
                let elem_size = self.sizeof_type(elem);
                let n = self.expr_as_array_size(size_expr);
                if let Some(n) = n {
                    return elem_size * (n as usize);
                }
                elem_size
            }
            TypeSpecifier::Struct(_, Some(fields)) | TypeSpecifier::Union(_, Some(fields)) => {
                let is_union = matches!(ts, TypeSpecifier::Union(_, _));
                let struct_fields: Vec<StructField> = fields.iter().map(|f| {
                    let bit_width = f.bit_width.as_ref().and_then(|bw| {
                        self.eval_const_expr(bw).and_then(|c| c.to_u32())
                    });
                    StructField {
                        name: f.name.clone().unwrap_or_default(),
                        ty: self.type_spec_to_ctype(&f.type_spec),
                        bit_width,
                    }
                }).collect();
                let layout = if is_union {
                    StructLayout::for_union(&struct_fields)
                } else {
                    StructLayout::for_struct(&struct_fields)
                };
                layout.size
            }
            TypeSpecifier::Struct(Some(tag), None) => {
                let key = format!("struct.{}", tag);
                self.struct_layouts.get(&key).map(|l| l.size).unwrap_or(8)
            }
            TypeSpecifier::Union(Some(tag), None) => {
                let key = format!("union.{}", tag);
                self.struct_layouts.get(&key).map(|l| l.size).unwrap_or(8)
            }
            _ => 8,
        }
    }

    /// For a pointer-to-array parameter type (e.g., Pointer(Array(Array(Int, 4), 3))),
    /// compute the array dimension strides for multi-dimensional subscript access.
    /// Returns strides for depth 0, 1, 2, ... where depth 0 is the outermost subscript.
    /// E.g., for int (*arr)[3][4]: strides = [3*4*4=48, 4*4=16, 4]
    /// For int (*arr)[3]: strides = [3*4=12, 4]
    pub(super) fn compute_ptr_array_strides(&self, type_spec: &TypeSpecifier) -> Vec<usize> {
        let ts = self.resolve_type_spec(type_spec);
        if let TypeSpecifier::Pointer(inner) = ts {
            // Collect dimensions from nested Array types
            let mut dims: Vec<usize> = Vec::new();
            let mut current = &*inner;
            loop {
                let resolved = self.resolve_type_spec(current);
                if let TypeSpecifier::Array(elem, size_expr) = resolved {
                    let n = size_expr.as_ref().and_then(|e| self.expr_as_array_size(e)).unwrap_or(1);
                    dims.push(n as usize);
                    current = &*elem;
                } else {
                    break;
                }
            }
            if dims.is_empty() {
                return vec![];
            }
            // Compute base element size (the innermost non-array type)
            let base_elem_size = self.sizeof_type(current);
            // Compute strides: stride[i] = product of dims[i..] * base_elem_size
            let mut strides = Vec::with_capacity(dims.len() + 1);
            // stride 0 = full row size = product(all dims) * base_elem_size
            // This is sizeof(pointee), already used as elem_size
            let full_size: usize = dims.iter().product::<usize>() * base_elem_size;
            strides.push(full_size);
            // stride 1 = product(dims[1..]) * base_elem_size
            for i in 1..dims.len() {
                let stride: usize = dims[i..].iter().product::<usize>() * base_elem_size;
                strides.push(stride);
            }
            // Final stride = base_elem_size (for the innermost subscript)
            strides.push(base_elem_size);
            strides
        } else {
            vec![]
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
            // Char literal: type int in C (4 bytes)
            Expr::CharLiteral(_, _) => 4,
            // String literal: array of char, size = length + 1 (null terminator)
            Expr::StringLiteral(s, _) => s.len() + 1,

            // Variable: look up its alloc_size or type
            Expr::Identifier(name, _) => {
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

            // Dereference: element type size
            Expr::Deref(inner, _) => {
                // Use CType-based resolution first
                if let Some(inner_ctype) = self.get_expr_ctype(inner) {
                    match &inner_ctype {
                        CType::Pointer(pointee) => {
                            // GCC extension: sizeof(*void_ptr) == 1, sizeof(*func_ptr) == 1
                            if matches!(pointee.as_ref(), CType::Void | CType::Function(_)) {
                                return 1;
                            }
                            let sz = pointee.size();
                            if sz == 0 {
                                return 1;
                            }
                            return sz;
                        }
                        CType::Array(elem, _) => return elem.size(),
                        // GCC extension: sizeof(*func) == 1 where func is a function
                        CType::Function(_) => return 1,
                        _ => {}
                    }
                }
                if let Expr::Identifier(name, _) = inner.as_ref() {
                    if let Some(info) = self.locals.get(name) {
                        // GCC extension: sizeof(*func_ptr) == 1
                        if let Some(ref ct) = info.c_type {
                            if matches!(ct, CType::Function(_)) {
                                return 1;
                            }
                            // Function pointer dereference: sizeof(*fptr) == 1
                            if let CType::Pointer(pointee) = ct {
                                if matches!(pointee.as_ref(), CType::Function(_)) {
                                    return 1;
                                }
                            }
                        }
                        if info.elem_size > 0 {
                            return info.elem_size;
                        }
                    }
                    if let Some(ginfo) = self.globals.get(name) {
                        if ginfo.elem_size > 0 {
                            return ginfo.elem_size;
                        }
                    }
                    // GCC extension: sizeof(*func_name) == 1 for known functions
                    if self.known_functions.contains(name) {
                        return 1;
                    }
                }
                8 // TODO: better type tracking for nested derefs
            }

            // Array subscript: element type size
            Expr::ArraySubscript(base, index, _) => {
                // Use CType-based resolution first (handles string literals, typed pointers)
                if let Some(base_ctype) = self.get_expr_ctype(base) {
                    match &base_ctype {
                        CType::Array(elem, _) => return elem.size(),
                        CType::Pointer(pointee) => return pointee.size(),
                        _ => {}
                    }
                }
                // Also check reverse subscript (index[base])
                if let Some(idx_ctype) = self.get_expr_ctype(index) {
                    match &idx_ctype {
                        CType::Array(elem, _) => return elem.size(),
                        CType::Pointer(pointee) => return pointee.size(),
                        _ => {}
                    }
                }
                if let Expr::Identifier(name, _) = base.as_ref() {
                    if let Some(info) = self.locals.get(name) {
                        if info.elem_size > 0 {
                            return info.elem_size;
                        }
                    }
                    if let Some(ginfo) = self.globals.get(name) {
                        if ginfo.elem_size > 0 {
                            return ginfo.elem_size;
                        }
                    }
                }
                4 // default: int element
            }

            // sizeof(sizeof(...)) -> size_t = 8 on 64-bit
            Expr::Sizeof(_, _) => 8,

            // Cast: size of the target type
            Expr::Cast(target_type, _, _) => {
                self.sizeof_type(target_type)
            }

            // Member access: member field size
            Expr::MemberAccess(base_expr, field_name, _) => {
                let (_, field_ty) = self.resolve_member_access(base_expr, field_name);
                field_ty.size()
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                let (_, field_ty) = self.resolve_pointer_member_access(base_expr, field_name);
                field_ty.size()
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
                }
            }

            // Postfix operations preserve the operand type
            Expr::PostfixOp(_, inner, _) => self.sizeof_expr(inner),

            // Binary operations
            Expr::BinaryOp(op, lhs, rhs, _) => {
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

            // Conditional: common type of both branches
            Expr::Conditional(_, then_e, else_e, _) => {
                let ts = self.sizeof_expr(then_e);
                let es = self.sizeof_expr(else_e);
                ts.max(es)
            }

            // Assignment: type of the left-hand side
            Expr::Assign(lhs, _, _) | Expr::CompoundAssign(_, lhs, _, _) => {
                self.sizeof_expr(lhs)
            }

            // Comma: type of the right expression
            Expr::Comma(_, rhs, _) => {
                self.sizeof_expr(rhs)
            }

            // Function call: default to int (4 bytes)
            Expr::FunctionCall(_, _, _) => 4,

            // Compound literal: size of the type
            Expr::CompoundLiteral(ts, _, _) => self.sizeof_type(ts),

            // Default
            _ => 4,
        }
    }

    /// Get the element size for a compound literal type.
    /// For arrays, returns the element size; for scalars/structs, returns the full size.
    pub(super) fn compound_literal_elem_size(&self, ts: &TypeSpecifier) -> usize {
        match ts {
            TypeSpecifier::Array(elem, _) => self.sizeof_type(elem),
            _ => self.sizeof_type(ts),
        }
    }

    /// Compute allocation info for a declaration.
    /// Returns (alloc_size, elem_size, is_array, is_pointer, array_dim_strides).
    /// For multi-dimensional arrays like int a[2][3], array_dim_strides = [12, 4]
    /// (stride for dim 0 = 3*4=12, stride for dim 1 = 4).
    pub(super) fn compute_decl_info(&self, ts: &TypeSpecifier, derived: &[DerivedDeclarator]) -> (usize, usize, bool, bool, Vec<usize>) {
        let ts = self.resolve_type_spec(ts);
        // Check for pointer declarators (from derived or from the resolved type itself)
        let has_pointer = derived.iter().any(|d| matches!(d, DerivedDeclarator::Pointer))
            || matches!(ts, TypeSpecifier::Pointer(_));

        let has_array = derived.iter().any(|d| matches!(d, DerivedDeclarator::Array(_)));

        // Handle pointer and array combinations
        if has_pointer && !has_array {
            // Simple pointer: int *p, or typedef'd pointer (e.g., typedef struct Foo *FooPtr)
            let elem_size = if let TypeSpecifier::Pointer(inner) = ts {
                // Pointer is in the type spec itself (typedef'd pointer)
                self.sizeof_type(self.resolve_type_spec(inner))
            } else {
                self.sizeof_type(ts)
            };
            return (8, elem_size, false, true, vec![]);
        }
        if has_pointer && has_array {
            // Check order: if Pointer comes before Array in derived list,
            // this is an array of pointers (e.g., int *arr[3]).
            // If Array comes before Pointer (e.g., int (*p)[5]), it's pointer to array.
            let ptr_pos = derived.iter().position(|d| matches!(d, DerivedDeclarator::Pointer));
            let arr_pos = derived.iter().position(|d| matches!(d, DerivedDeclarator::Array(_)));

            // If pointer is from resolved type spec (not in derived), and array is in derived,
            // this is an array of typedef'd pointers (e.g., typedef int *intptr; intptr arr[3];
            // or typedef int (*fn_t)(int); fn_t ops[2];)
            let pointer_from_type_spec = ptr_pos.is_none() && matches!(ts, TypeSpecifier::Pointer(_));

            if pointer_from_type_spec || matches!((ptr_pos, arr_pos), (Some(pp), Some(ap)) if pp < ap) {
                // Array of pointers: int *arr[3] or typedef'd_ptr arr[3]
                // Each element is a pointer (8 bytes)
                let array_dims: Vec<Option<usize>> = derived.iter().filter_map(|d| {
                    if let DerivedDeclarator::Array(size_expr) = d {
                        let dim = size_expr.as_ref().and_then(|e| {
                            self.expr_as_array_size(e).map(|n| n as usize)
                        });
                        Some(dim)
                    } else {
                        None
                    }
                }).collect();
                let total_elems: usize = array_dims.iter().map(|d| d.unwrap_or(256)).product();
                let total_size = total_elems * 8; // each element is a pointer
                return (total_size, 8, true, false, vec![]);
            }
            // Pointer to array (e.g., int (*p)[5]) - treat as pointer
            // Compute strides from the Array dims in the derived list
            let array_dims: Vec<usize> = derived.iter().filter_map(|d| {
                if let DerivedDeclarator::Array(size_expr) = d {
                    let dim = size_expr.as_ref().and_then(|e| {
                        self.expr_as_array_size(e).map(|n| n as usize)
                    });
                    Some(dim.unwrap_or(1))
                } else {
                    None
                }
            }).collect();
            let base_elem_size = self.sizeof_type(ts);
            let full_array_size: usize = array_dims.iter().product::<usize>() * base_elem_size;
            // strides[0] = full pointed-to array size, strides[i] = product of dims[i..] * base
            let mut strides = vec![full_array_size];
            for i in 0..array_dims.len() {
                let stride: usize = array_dims[i+1..].iter().product::<usize>().max(1) * base_elem_size;
                strides.push(stride);
            }
            let elem_size = full_array_size;
            return (8, elem_size, false, true, strides);
        }

        // If the resolved type itself is an Array (e.g., va_list = Array(Char, 24))
        // and there are no derived array declarators, handle it as an array type.
        if !has_array && !has_pointer {
            if let TypeSpecifier::Array(elem, size_expr) = ts {
                let elem_size = self.sizeof_type(elem).max(1);
                let dim = size_expr.as_ref().and_then(|e| {
                    self.expr_as_array_size(e).map(|n| n as usize)
                }).unwrap_or(1);
                let total = dim * elem_size;
                return (total, elem_size, true, false, vec![elem_size]);
            }
        }

        // Check for array declarators - collect all dimensions
        let array_dims: Vec<Option<usize>> = derived.iter().filter_map(|d| {
            if let DerivedDeclarator::Array(size_expr) = d {
                let dim = size_expr.as_ref().and_then(|e| {
                    self.expr_as_array_size(e).map(|n| n as usize)
                });
                Some(dim)
            } else {
                None
            }
        }).collect();

        if !array_dims.is_empty() {
            let base_elem_size = self.sizeof_type(ts).max(1);

            // Also account for array dimensions in the type specifier itself
            // e.g., if type is Array(Array(Int, 3), 2) from the parser
            let type_dims = self.collect_type_array_dims(ts);

            // Combine: derived dims come first (outermost), then type dims
            let all_dims: Vec<usize> = array_dims.iter().map(|d| d.unwrap_or(256))
                .chain(type_dims.iter().copied())
                .collect();

            // Compute total size = product of all dims * base_elem_size
            let total: usize = all_dims.iter().product::<usize>() * base_elem_size;

            // Compute strides: stride[i] = product of dims[i+1..] * base_elem_size
            let mut strides = Vec::with_capacity(all_dims.len());
            for i in 0..all_dims.len() {
                let stride: usize = all_dims[i+1..].iter().product::<usize>() * base_elem_size;
                strides.push(stride);
            }

            // elem_size is the stride of the outermost dimension (for 1D compat, it's base_elem_size)
            let elem_size = if strides.len() > 1 { strides[0] } else { base_elem_size };

            return (total, elem_size, true, false, strides);
        }

        // For struct/union types, use their layout size
        if let Some(layout) = self.get_struct_layout_for_type(ts) {
            return (layout.size, 0, false, false, vec![]);
        }

        // Regular scalar - use sizeof_type for the allocation size
        // (8 bytes for most scalars, 16 for long double)
        let scalar_size = self.sizeof_type(ts).max(8);
        (scalar_size, 0, false, false, vec![])
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

    /// For Array(Array(Int, 3), 2), returns [2, 3] (but we skip the outermost
    /// since that comes from the derived declarator).
    fn collect_type_array_dims(&self, ts: &TypeSpecifier) -> Vec<usize> {
        let mut dims = Vec::new();
        let mut current = ts;
        loop {
            if let TypeSpecifier::Array(inner, Some(size_expr)) = current {
                if let Some(n) = self.expr_as_array_size(size_expr) {
                    dims.push(n as usize);
                }
                current = inner.as_ref();
            } else {
                break;
            }
        }
        dims
    }

    /// Map an element size in bytes to an appropriate IrType.
    pub(super) fn ir_type_for_elem_size(&self, size: usize) -> IrType {
        match size {
            1 => IrType::I8,
            2 => IrType::I16,
            4 => IrType::I32,
            8 => IrType::I64,
            _ => IrType::I64,
        }
    }

    /// Convert a TypeSpecifier to CType (for struct layout computation).
    pub(super) fn type_spec_to_ctype(&self, ts: &TypeSpecifier) -> CType {
        let ts = self.resolve_type_spec(ts);
        match ts {
            TypeSpecifier::Void => CType::Void,
            TypeSpecifier::Char => CType::Char,
            TypeSpecifier::UnsignedChar => CType::UChar,
            TypeSpecifier::Short => CType::Short,
            TypeSpecifier::UnsignedShort => CType::UShort,
            TypeSpecifier::Bool => CType::Bool, // _Bool: 1-byte unsigned, normalizes to 0 or 1 on store
            TypeSpecifier::Int | TypeSpecifier::Signed => CType::Int,
            TypeSpecifier::UnsignedInt | TypeSpecifier::Unsigned => CType::UInt,
            TypeSpecifier::Long => CType::Long,
            TypeSpecifier::UnsignedLong => CType::ULong,
            TypeSpecifier::LongLong => CType::LongLong,
            TypeSpecifier::UnsignedLongLong => CType::ULongLong,
            TypeSpecifier::Float => CType::Float,
            TypeSpecifier::Double => CType::Double,
            TypeSpecifier::LongDouble => CType::LongDouble,
            TypeSpecifier::Pointer(inner) => CType::Pointer(Box::new(self.type_spec_to_ctype(inner))),
            TypeSpecifier::Array(elem, size_expr) => {
                let elem_ctype = self.type_spec_to_ctype(elem);
                let size = size_expr.as_ref().and_then(|e| {
                    self.expr_as_array_size(e).map(|n| n as usize)
                });
                CType::Array(Box::new(elem_ctype), size)
            }
            TypeSpecifier::Struct(name, fields) => {
                self.struct_or_union_to_ctype(name, fields, false)
            }
            TypeSpecifier::Union(name, fields) => {
                self.struct_or_union_to_ctype(name, fields, true)
            }
            TypeSpecifier::Enum(_, _) => CType::Int, // enums are int-sized
            TypeSpecifier::TypedefName(_) => CType::Int, // TODO: resolve typedef
        }
    }

    /// Convert a struct or union TypeSpecifier to CType.
    /// `is_union` selects between struct and union semantics.
    fn struct_or_union_to_ctype(
        &self,
        name: &Option<String>,
        fields: &Option<Vec<StructFieldDecl>>,
        is_union: bool,
    ) -> CType {
        let make = |st: crate::common::types::StructType| -> CType {
            if is_union { CType::Union(st) } else { CType::Struct(st) }
        };
        let prefix = if is_union { "union" } else { "struct" };

        if let Some(fs) = fields {
            let struct_fields: Vec<StructField> = fs.iter().map(|f| {
                let bit_width = f.bit_width.as_ref().and_then(|bw| {
                    self.eval_const_expr(bw).and_then(|c| c.to_u32())
                });
                StructField {
                    name: f.name.clone().unwrap_or_default(),
                    ty: self.type_spec_to_ctype(&f.type_spec),
                    bit_width,
                }
            }).collect();
            make(crate::common::types::StructType {
                name: name.clone(),
                fields: struct_fields,
            })
        } else if let Some(tag) = name {
            let key = format!("{}.{}", prefix, tag);
            if let Some(layout) = self.struct_layouts.get(&key) {
                let struct_fields: Vec<StructField> = layout.fields.iter().map(|f| {
                    StructField {
                        name: f.name.clone(),
                        ty: f.ty.clone(),
                        bit_width: f.bit_width,
                    }
                }).collect();
                make(crate::common::types::StructType {
                    name: Some(tag.clone()),
                    fields: struct_fields,
                })
            } else {
                make(crate::common::types::StructType {
                    name: Some(tag.clone()),
                    fields: Vec::new(),
                })
            }
        } else {
            make(crate::common::types::StructType {
                name: None,
                fields: Vec::new(),
            })
        }
    }

    /// Build a full CType from a TypeSpecifier and DerivedDeclarator chain.
    /// For `int **p`, type_spec=Int, derived=[Pointer, Pointer] -> Pointer(Pointer(Int)).
    /// For `int *arr[3]`, type_spec=Int, derived=[Pointer, Array(3)] -> Array(Pointer(Int), 3).
    pub(super) fn build_full_ctype(&self, type_spec: &TypeSpecifier, derived: &[DerivedDeclarator]) -> CType {
        let resolved = self.resolve_type_spec(type_spec);
        let base = self.type_spec_to_ctype(resolved);
        let mut result = base;
        // Process derived declarators in groups:
        // - Pointers are applied immediately (forward order)
        // - Consecutive Array dimensions are collected and applied in reverse
        //   (so `int m[3][4]` with derived [Array(3), Array(4)] becomes Array(Array(Int,4),3))
        let mut i = 0;
        while i < derived.len() {
            match &derived[i] {
                DerivedDeclarator::Pointer => {
                    result = CType::Pointer(Box::new(result));
                    i += 1;
                }
                DerivedDeclarator::Array(_) => {
                    // Collect all consecutive Array declarators
                    let start = i;
                    while i < derived.len() && matches!(&derived[i], DerivedDeclarator::Array(_)) {
                        i += 1;
                    }
                    // Apply array dimensions in reverse order (innermost first)
                    for j in (start..i).rev() {
                        if let DerivedDeclarator::Array(size_expr) = &derived[j] {
                            let size = size_expr.as_ref().and_then(|e| {
                                self.expr_as_array_size(e).map(|n| n as usize)
                            });
                            result = CType::Array(Box::new(result), size);
                        }
                    }
                }
                DerivedDeclarator::Function(_, _) => {
                    let func_type = CType::Function(Box::new(crate::common::types::FunctionType {
                        return_type: result,
                        params: Vec::new(),
                        variadic: false,
                    }));
                    result = func_type;
                    i += 1;
                }
                DerivedDeclarator::FunctionPointer(_, _) => {
                    let func_type = CType::Function(Box::new(crate::common::types::FunctionType {
                        return_type: result,
                        params: Vec::new(),
                        variadic: false,
                    }));
                    result = func_type;
                    i += 1;
                }
            }
        }
        result
    }

    /// Get the full CType of an expression by recursion.
    /// Returns None if the type cannot be determined from CType tracking.
    pub(super) fn get_expr_ctype(&self, expr: &Expr) -> Option<CType> {
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(info) = self.locals.get(name) {
                    return info.c_type.clone();
                }
                if let Some(ginfo) = self.globals.get(name) {
                    return ginfo.c_type.clone();
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
            Expr::Comma(_, last, _) => self.get_expr_ctype(last),
            Expr::StringLiteral(_, _) => {
                // String literals have type char[] which decays to char*
                Some(CType::Pointer(Box::new(CType::Char)))
            }
            Expr::BinaryOp(op, lhs, rhs, _) => {
                // Pointer arithmetic: ptr + int or int + ptr returns the pointer type
                // Pointer subtraction: ptr - ptr returns ptrdiff_t (Long)
                match op {
                    BinOp::Add => {
                        if let Some(lct) = self.get_expr_ctype(lhs) {
                            if matches!(&lct, CType::Pointer(_) | CType::Array(_, _)) {
                                return Some(lct);
                            }
                        }
                        if let Some(rct) = self.get_expr_ctype(rhs) {
                            if matches!(&rct, CType::Pointer(_) | CType::Array(_, _)) {
                                return Some(rct);
                            }
                        }
                        None
                    }
                    BinOp::Sub => {
                        if let Some(lct) = self.get_expr_ctype(lhs) {
                            if matches!(&lct, CType::Pointer(_) | CType::Array(_, _)) {
                                // Check if rhs is also pointer (ptr - ptr = ptrdiff_t)
                                if let Some(rct) = self.get_expr_ctype(rhs) {
                                    if matches!(&rct, CType::Pointer(_) | CType::Array(_, _)) {
                                        return Some(CType::Long);
                                    }
                                }
                                // ptr - int = same pointer type
                                return Some(lct);
                            }
                        }
                        None
                    }
                    _ => None,
                }
            }
            Expr::FunctionCall(func, _, _) => {
                if let Expr::Identifier(name, _) = func.as_ref() {
                    if let Some(ctype) = self.function_return_ctypes.get(name) {
                        return Some(ctype.clone());
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

}
