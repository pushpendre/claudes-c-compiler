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
            Expr::FloatLiteralLongDouble(val, _) => {
                Some(IrConst::LongDouble(*val))
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
                        // Integer promotion: sub-int types promote to int before negation
                        IrConst::I8(v) => Some(IrConst::I32(-(v as i32))),
                        IrConst::I16(v) => Some(IrConst::I32(-(v as i32))),
                        IrConst::F64(v) => Some(IrConst::F64(-v)),
                        IrConst::F32(v) => Some(IrConst::F32(-v)),
                        IrConst::LongDouble(v) => Some(IrConst::LongDouble(-v)),
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
                        // Integer promotion: sub-int types promote to int before complement
                        IrConst::I8(v) => Some(IrConst::I32(!(v as i32))),
                        IrConst::I16(v) => Some(IrConst::I32(!(v as i32))),
                        _ => None,
                    }
                } else {
                    None
                }
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
                if let Some(&val) = self.enum_constants.get(name) {
                    return Some(IrConst::I64(val));
                }
                // Look up const-qualified local variable values
                // (e.g., const int len = 5000; int arr[len];)
                if let Some(&val) = self.const_local_values.get(name) {
                    return Some(IrConst::I64(val));
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
            Expr::UnaryOp(UnaryOp::LogicalNot, inner, _) => {
                let val = self.eval_const_expr(inner)?;
                Some(IrConst::I64(if val.is_nonzero() { 0 } else { 1 }))
            }
            // Handle &((type*)0)->member pattern (offsetof)
            Expr::AddressOf(inner, _) => {
                self.eval_offsetof_pattern(inner)
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
                let (field_offset, _field_ty) = layout.field_offset(field_name)?;
                Some(IrConst::I64((base_offset + field_offset) as i64))
            }
            Expr::MemberAccess(base, field_name, _) => {
                // base might be *((type*)0)
                if let Expr::Deref(inner, _) = base.as_ref() {
                    let (type_spec, base_offset) = self.extract_null_pointer_cast_with_offset(inner)?;
                    let layout = self.get_struct_layout_for_type(&type_spec)?;
                    let (field_offset, _field_ty) = layout.field_offset(field_name)?;
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
                                    CType::Array(elem, _) => elem.size(),
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
        match expr {
            Expr::IntLiteral(0, _) | Expr::UIntLiteral(0, _)
            | Expr::LongLiteral(0, _) | Expr::ULongLiteral(0, _) => true,
            Expr::Cast(_, inner, _) => self.is_zero_expr(inner),
            _ => false,
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
                        // Check static local names first (local statics shadow globals)
                        if let Some(mangled) = self.static_local_names.get(name) {
                            return Some(GlobalInit::GlobalAddr(mangled.clone()));
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
                            // Resolve name: check static locals first (they shadow globals)
                            let resolved = if let Some(mangled) = self.static_local_names.get(name.as_str()) {
                                Some(mangled.clone())
                            } else if self.globals.contains_key(name.as_str()) {
                                Some(name.clone())
                            } else {
                                None
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
                            // Resolve name: check static locals first (they shadow globals)
                            let resolved = if let Some(mangled) = self.static_local_names.get(name.as_str()) {
                                Some(mangled.clone())
                            } else if self.globals.contains_key(name.as_str()) {
                                Some(name.clone())
                            } else {
                                None
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
                // Check static local array names first (they shadow globals)
                if let Some(mangled) = self.static_local_names.get(name) {
                    if let Some(ginfo) = self.globals.get(mangled) {
                        if ginfo.is_array {
                            return Some(GlobalInit::GlobalAddr(mangled.clone()));
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
        // Check if either operand is floating-point
        let lhs_is_float = matches!(lhs, IrConst::F32(_) | IrConst::F64(_));
        let rhs_is_float = matches!(rhs, IrConst::F32(_) | IrConst::F64(_));

        if lhs_is_float || rhs_is_float {
            return self.eval_const_binop_float(op, lhs, rhs);
        }

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

    /// Evaluate a binary operation on floating-point constant operands.
    /// Promotes both operands to the wider float type (f64 if either is f64).
    fn eval_const_binop_float(&self, op: &BinOp, lhs: &IrConst, rhs: &IrConst) -> Option<IrConst> {
        // Determine if result should be f32 (both are f32) or f64 (either is f64 or int)
        let use_f64 = matches!(lhs, IrConst::F64(_)) || matches!(rhs, IrConst::F64(_))
            || (!matches!(lhs, IrConst::F32(_)) && !matches!(rhs, IrConst::F32(_)));

        // Convert both operands to f64 for computation
        let l = lhs.to_f64()?;
        let r = rhs.to_f64()?;

        match op {
            // Arithmetic operations return float
            BinOp::Add => {
                let v = l + r;
                Some(if use_f64 { IrConst::F64(v) } else { IrConst::F32(v as f32) })
            }
            BinOp::Sub => {
                let v = l - r;
                Some(if use_f64 { IrConst::F64(v) } else { IrConst::F32(v as f32) })
            }
            BinOp::Mul => {
                let v = l * r;
                Some(if use_f64 { IrConst::F64(v) } else { IrConst::F32(v as f32) })
            }
            BinOp::Div => {
                // IEEE 754: division by zero produces infinity or NaN, which is valid
                let v = l / r;
                Some(if use_f64 { IrConst::F64(v) } else { IrConst::F32(v as f32) })
            }
            // Comparison operations return int
            BinOp::Eq => Some(IrConst::I64(if l == r { 1 } else { 0 })),
            BinOp::Ne => Some(IrConst::I64(if l != r { 1 } else { 0 })),
            BinOp::Lt => Some(IrConst::I64(if l < r { 1 } else { 0 })),
            BinOp::Gt => Some(IrConst::I64(if l > r { 1 } else { 0 })),
            BinOp::Le => Some(IrConst::I64(if l <= r { 1 } else { 0 })),
            BinOp::Ge => Some(IrConst::I64(if l >= r { 1 } else { 0 })),
            // Logical operations
            BinOp::LogicalAnd => Some(IrConst::I64(if l != 0.0 && r != 0.0 { 1 } else { 0 })),
            BinOp::LogicalOr => Some(IrConst::I64(if l != 0.0 || r != 0.0 { 1 } else { 0 })),
            // Bitwise/shift operations are not valid on floats
            _ => None,
        }
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
        let num_fields = layout.fields.len();
        if num_fields == 0 {
            return items.len();
        }

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
                // Flat init: this scalar fills one field of the current struct element
                fields_consumed += 1;
                if fields_consumed >= num_fields {
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

    /// Check if a TypeSpecifier resolves to long double.
    pub(super) fn is_type_spec_long_double(&self, ts: &TypeSpecifier) -> bool {
        match ts {
            TypeSpecifier::LongDouble => true,
            TypeSpecifier::TypedefName(name) => {
                if let Some(resolved) = self.typedefs.get(name) {
                    self.is_type_spec_long_double(resolved)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Get the zero constant for a given IR type.
    pub(super) fn zero_const(&self, ty: IrType) -> IrConst {
        if ty == IrType::Void { IrConst::Zero } else { IrConst::zero(ty) }
    }

    /// Check if an expression refers to a struct/union value (not pointer-to-struct).
    /// Returns the struct/union size if the expression produces a struct/union value,
    /// or None if it's not a struct/union. Unifies the old expr_is_struct_value (check)
    /// and get_struct_size_for_expr (size) into a single dispatch.
    pub(super) fn struct_value_size(&self, expr: &Expr) -> Option<usize> {
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(info) = self.locals.get(name) {
                    if info.is_struct { return Some(info.alloc_size); }
                }
                if let Some(ginfo) = self.globals.get(name) {
                    if ginfo.is_struct {
                        return Some(ginfo.struct_layout.as_ref().map_or(8, |l| l.size));
                    }
                }
                None
            }
            Expr::MemberAccess(base_expr, field_name, _) | Expr::PointerMemberAccess(base_expr, field_name, _) => {
                let is_ptr = matches!(expr, Expr::PointerMemberAccess(..));
                let ctype = self.resolve_field_ctype(base_expr, field_name, is_ptr)?;
                if matches!(ctype, CType::Struct(_) | CType::Union(_)) { Some(ctype.size()) } else { None }
            }
            Expr::ArraySubscript(_, _, _) | Expr::Deref(_, _)
            | Expr::FunctionCall(_, _, _) | Expr::Conditional(_, _, _, _) => {
                let ctype = self.get_expr_ctype(expr)?;
                if matches!(ctype, CType::Struct(_) | CType::Union(_)) { Some(ctype.size()) } else { None }
            }
            Expr::CompoundLiteral(type_spec, _, _) => {
                let resolved = self.resolve_type_spec(type_spec);
                if matches!(resolved, TypeSpecifier::Struct(..) | TypeSpecifier::Union(..)) {
                    Some(self.sizeof_type(type_spec))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Extract the return IrType from a function pointer's CType.
    /// Handles the spurious Pointer wrapper that build_full_ctype adds for (*fp)() syntax.
    /// For `double (*fp)(void)`, the CType is Function { return_type: Pointer(Double) }
    /// and we need to unwrap the Pointer to get F64.
    fn extract_func_ptr_return_type(ctype: &CType) -> IrType {
        match ctype {
            CType::Pointer(inner) => match inner.as_ref() {
                CType::Function(ft) => Self::peel_ptr_from_return_type(&ft.return_type),
                // For parameter function pointers, CType is just Pointer(ReturnType)
                // without the Function wrapper (type_spec_to_ctype doesn't generate Function nodes)
                CType::Float => IrType::F32,
                CType::Double | CType::LongDouble => IrType::F64,
                // Pointer(Pointer(X)) means returning a pointer type
                CType::Pointer(_) => IrType::Ptr,
                _ => IrType::I64,
            },
            CType::Function(ft) => Self::peel_ptr_from_return_type(&ft.return_type),
            _ => IrType::I64,
        }
    }

    /// Peel one Pointer layer from a function's return_type CType.
    /// This compensates for build_full_ctype wrapping the return type in Pointer
    /// due to the (*fp) declarator syntax.
    pub(super) fn peel_ptr_from_return_type(return_type: &CType) -> IrType {
        match return_type {
            CType::Pointer(inner) => match inner.as_ref() {
                CType::Float => IrType::F32,
                CType::Double | CType::LongDouble => IrType::F64,
                CType::Void => IrType::I64,
                CType::Char | CType::UChar | CType::Short | CType::UShort
                | CType::Int | CType::UInt | CType::Bool => IrType::I32,
                CType::Long | CType::ULong | CType::LongLong | CType::ULongLong => IrType::I64,
                // Pointer(Pointer(...)) means actual pointer return type
                CType::Pointer(_) => IrType::Ptr,
                CType::Struct(_) | CType::Union(_) => IrType::Ptr,
                CType::Function(_) => IrType::Ptr,
                CType::Array(_, _) => IrType::Ptr,
                _ => IrType::I64,
            },
            // No Pointer wrapper - direct conversion
            _ => IrType::from_ctype(return_type),
        }
    }

    /// Return the IR type for known builtins that return float or specific types.
    /// Returns None for builtins without special return type handling.
    pub(super) fn builtin_return_type(name: &str) -> Option<IrType> {
        match name {
            // Float-returning builtins
            "__builtin_inf" | "__builtin_huge_val" => Some(IrType::F64),
            "__builtin_inff" | "__builtin_huge_valf" => Some(IrType::F32),
            "__builtin_infl" | "__builtin_huge_vall" => Some(IrType::F128),
            "__builtin_nan" => Some(IrType::F64),
            "__builtin_nanf" => Some(IrType::F32),
            "__builtin_nanl" => Some(IrType::F128),
            "__builtin_fabs" | "__builtin_sqrt" | "__builtin_sin" | "__builtin_cos"
            | "__builtin_log" | "__builtin_log2" | "__builtin_exp" | "__builtin_pow"
            | "__builtin_floor" | "__builtin_ceil" | "__builtin_round"
            | "__builtin_fmin" | "__builtin_fmax" | "__builtin_copysign" => Some(IrType::F64),
            "__builtin_fabsf" | "__builtin_sqrtf" | "__builtin_sinf" | "__builtin_cosf"
            | "__builtin_logf" | "__builtin_expf" | "__builtin_powf"
            | "__builtin_floorf" | "__builtin_ceilf" | "__builtin_roundf"
            | "__builtin_copysignf" => Some(IrType::F32),
            "__builtin_fabsl" => Some(IrType::F128),
            // Integer-returning classification builtins
            "__builtin_fpclassify" | "__builtin_isnan" | "__builtin_isinf"
            | "__builtin_isfinite" | "__builtin_isnormal" | "__builtin_signbit"
            | "__builtin_signbitf" | "__builtin_signbitl" | "__builtin_isinf_sign"
            | "__builtin_isgreater" | "__builtin_isgreaterequal"
            | "__builtin_isless" | "__builtin_islessequal"
            | "__builtin_islessgreater" | "__builtin_isunordered" => Some(IrType::I32),
            // Bit manipulation builtins return int
            "__builtin_clz" | "__builtin_clzl" | "__builtin_clzll"
            | "__builtin_ctz" | "__builtin_ctzl" | "__builtin_ctzll"
            | "__builtin_popcount" | "__builtin_popcountl" | "__builtin_popcountll"
            | "__builtin_parity" | "__builtin_parityl" | "__builtin_parityll"
            | "__builtin_ffs" | "__builtin_ffsl" | "__builtin_ffsll" => Some(IrType::I32),
            // Complex number component extraction builtins
            "creal" | "__builtin_creal" | "cimag" | "__builtin_cimag" => Some(IrType::F64),
            "crealf" | "__builtin_crealf" | "cimagf" | "__builtin_cimagf" => Some(IrType::F32),
            "creall" | "__builtin_creall" | "cimagl" | "__builtin_cimagl" => Some(IrType::F128),
            // Complex absolute value
            "cabs" | "__builtin_cabs" => Some(IrType::F64),
            "cabsf" | "__builtin_cabsf" => Some(IrType::F32),
            "cabsl" | "__builtin_cabsl" => Some(IrType::F128),
            // Complex argument
            "carg" | "__builtin_carg" => Some(IrType::F64),
            "cargf" | "__builtin_cargf" => Some(IrType::F32),
            _ => None,
        }
    }

    /// Get the IR type for an expression (best-effort, based on locals/globals info).
    pub(super) fn get_expr_type(&self, expr: &Expr) -> IrType {
        match expr {
            Expr::IntLiteral(_, _) | Expr::LongLiteral(_, _) | Expr::CharLiteral(_, _) => IrType::I64,
            Expr::UIntLiteral(_, _) | Expr::ULongLiteral(_, _) => IrType::U64,
            Expr::FloatLiteral(_, _) => IrType::F64,
            Expr::FloatLiteralF32(_, _) => IrType::F32,
            Expr::FloatLiteralLongDouble(_, _) => IrType::F128,
            Expr::ImaginaryLiteral(_, _) | Expr::ImaginaryLiteralF32(_, _)
            | Expr::ImaginaryLiteralLongDouble(_, _) => IrType::Ptr,
            Expr::StringLiteral(_, _) => IrType::Ptr,
            Expr::Cast(ref target_type, _, _) => self.type_spec_to_ir(target_type),
            Expr::UnaryOp(UnaryOp::RealPart, inner, _) | Expr::UnaryOp(UnaryOp::ImagPart, inner, _) => {
                let inner_ct = self.expr_ctype(inner);
                if inner_ct.is_complex() {
                    return Self::complex_component_ir_type(&inner_ct);
                }
                self.get_expr_type(inner)
            }
            Expr::UnaryOp(UnaryOp::Neg, inner, _) | Expr::UnaryOp(UnaryOp::Plus, inner, _)
            | Expr::UnaryOp(UnaryOp::BitNot, inner, _) => {
                let inner_ty = self.get_expr_type(inner);
                // Only check for complex types if the inner type is Ptr (which complex uses)
                if inner_ty == IrType::Ptr {
                    let inner_ct = self.expr_ctype(inner);
                    if inner_ct.is_complex() {
                        return IrType::Ptr;
                    }
                }
                if inner_ty.is_float() {
                    return inner_ty;
                }
                match inner_ty {
                    IrType::I8 | IrType::U8 | IrType::I16 | IrType::U16 | IrType::I32 => IrType::I32,
                    IrType::U32 => IrType::U32,
                    _ => inner_ty,
                }
            }
            Expr::UnaryOp(UnaryOp::PreInc, inner, _) | Expr::UnaryOp(UnaryOp::PreDec, inner, _) => {
                self.get_expr_type(inner)
            }
            Expr::BinaryOp(op, lhs, rhs, _) => {
                match op {
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge
                    | BinOp::LogicalAnd | BinOp::LogicalOr => IrType::I64,
                    BinOp::Shl | BinOp::Shr => {
                        let lty = self.get_expr_type(lhs);
                        match lty {
                            IrType::I8 | IrType::U8 | IrType::I16 | IrType::U16 | IrType::I32 => IrType::I32,
                            IrType::U32 => IrType::U32,
                            _ => lty,
                        }
                    }
                    _ => {
                        let lty = self.get_expr_type(lhs);
                        let rty = self.get_expr_type(rhs);
                        // Only check complex types if either operand is Ptr
                        // (complex types are represented as Ptr in IR)
                        if matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div) {
                            if lty == IrType::Ptr || rty == IrType::Ptr {
                                let lct = self.expr_ctype(lhs);
                                let rct = self.expr_ctype(rhs);
                                if lct.is_complex() || rct.is_complex() {
                                    return IrType::Ptr;
                                }
                            }
                        }
                        if lty == IrType::F128 || rty == IrType::F128 {
                            IrType::F128
                        } else if lty == IrType::F64 || rty == IrType::F64 {
                            IrType::F64
                        } else if lty == IrType::F32 || rty == IrType::F32 {
                            IrType::F32
                        } else {
                            Self::common_type(lty, rty)
                        }
                    }
                }
            }
            Expr::Assign(lhs, _, _) | Expr::CompoundAssign(_, lhs, _, _) => {
                self.get_expr_type(lhs)
            }
            Expr::Conditional(_, then_expr, else_expr, _) => {
                let then_ty = self.get_expr_type(then_expr);
                let else_ty = self.get_expr_type(else_expr);
                Self::common_type(then_ty, else_ty)
            }
            Expr::Comma(_, rhs, _) => self.get_expr_type(rhs),
            Expr::PostfixOp(_, inner, _) => self.get_expr_type(inner),
            Expr::AddressOf(_, _) => IrType::Ptr,
            Expr::Sizeof(_, _) => IrType::U64,
            Expr::GenericSelection(controlling, associations, _) => {
                let controlling_ctype = self.get_expr_ctype(controlling);
                let controlling_ir_type = self.get_expr_type(controlling);
                let mut default_expr: Option<&Expr> = None;
                for assoc in associations.iter() {
                    match &assoc.type_spec {
                        None => { default_expr = Some(&assoc.expr); }
                        Some(type_spec) => {
                            let assoc_ctype = self.type_spec_to_ctype(type_spec);
                            if let Some(ref ctrl_ct) = controlling_ctype {
                                if self.ctype_matches_generic(ctrl_ct, &assoc_ctype) {
                                    return self.get_expr_type(&assoc.expr);
                                }
                            } else {
                                let assoc_ir_type = self.type_spec_to_ir(type_spec);
                                if assoc_ir_type == controlling_ir_type {
                                    return self.get_expr_type(&assoc.expr);
                                }
                            }
                        }
                    }
                }
                if let Some(def) = default_expr {
                    return self.get_expr_type(def);
                }
                IrType::I64
            }
            Expr::FunctionCall(func, _, _) => {
                if let Expr::Identifier(name, _) = func.as_ref() {
                    if let Some(&ret_ty) = self.func_meta.return_types.get(name) {
                        return ret_ty;
                    }
                    if let Some(&ret_ty) = self.func_meta.ptr_return_types.get(name) {
                        return ret_ty;
                    }
                    if let Some(ret_ty) = Self::builtin_return_type(name) {
                        return ret_ty;
                    }
                }
                if let Expr::Deref(inner, _) = func.as_ref() {
                    if let Expr::Identifier(name, _) = inner.as_ref() {
                        if let Some(&ret_ty) = self.func_meta.return_types.get(name) {
                            return ret_ty;
                        }
                        if let Some(&ret_ty) = self.func_meta.ptr_return_types.get(name) {
                            return ret_ty;
                        }
                    }
                    if let Some(inner_ctype) = self.get_expr_ctype(inner) {
                        return Self::extract_func_ptr_return_type(&inner_ctype);
                    }
                }
                if let Some(ctype) = self.get_expr_ctype(func) {
                    return Self::extract_func_ptr_return_type(&ctype);
                }
                IrType::I64
            }
            Expr::VaArg(_, type_spec, _) => self.resolve_va_arg_type(type_spec),
            Expr::Identifier(name, _) => {
                if name == "__func__" || name == "__FUNCTION__" || name == "__PRETTY_FUNCTION__" {
                    return IrType::Ptr;
                }
                if self.enum_constants.contains_key(name) {
                    return IrType::I32;
                }
                if let Some(vi) = self.lookup_var_info(name) {
                    if vi.is_array {
                        return IrType::Ptr;
                    }
                    return vi.ty;
                }
                IrType::I64
            }
            Expr::ArraySubscript(base, index, _) => {
                if let Some(base_ctype) = self.get_expr_ctype(base) {
                    match base_ctype {
                        CType::Array(elem, _) => return IrType::from_ctype(&elem),
                        CType::Pointer(pointee) => return IrType::from_ctype(&pointee),
                        _ => {}
                    }
                }
                if let Some(idx_ctype) = self.get_expr_ctype(index) {
                    match idx_ctype {
                        CType::Array(elem, _) => return IrType::from_ctype(&elem),
                        CType::Pointer(pointee) => return IrType::from_ctype(&pointee),
                        _ => {}
                    }
                }
                let root_name = self.get_array_root_name(expr);
                if let Some(name) = root_name {
                    if let Some(vi) = self.lookup_var_info(&name) {
                        if vi.is_array {
                            // For globals with multi-dim strides, use stride-based type
                            if !vi.array_dim_strides.is_empty() {
                                return self.ir_type_for_elem_size(*vi.array_dim_strides.last().unwrap_or(&8));
                            }
                            return vi.ty;
                        }
                    }
                }
                for operand in [base.as_ref(), index.as_ref()] {
                    if let Expr::Identifier(name, _) = operand {
                        if let Some(vi) = self.lookup_var_info(name) {
                            if let Some(pt) = vi.pointee_type {
                                return pt;
                            }
                            if vi.is_array {
                                return vi.ty;
                            }
                        }
                    }
                }
                match base.as_ref() {
                    Expr::MemberAccess(base_expr, field_name, _) | Expr::PointerMemberAccess(base_expr, field_name, _) => {
                        let is_ptr = matches!(base.as_ref(), Expr::PointerMemberAccess(..));
                        if let Some(ctype) = self.resolve_field_ctype(base_expr, field_name, is_ptr) {
                            if let CType::Array(elem_ty, _) = &ctype {
                                return IrType::from_ctype(elem_ty);
                            }
                        }
                    }
                    _ => {}
                }
                if let Some(pt) = self.get_pointee_type_of_expr(base) {
                    return pt;
                }
                if let Some(pt) = self.get_pointee_type_of_expr(index) {
                    return pt;
                }
                IrType::I64
            }
            Expr::Deref(inner, _) => {
                if let Some(inner_ctype) = self.get_expr_ctype(inner) {
                    match inner_ctype {
                        CType::Pointer(pointee) => return IrType::from_ctype(&pointee),
                        CType::Array(elem, _) => return IrType::from_ctype(&elem),
                        _ => {}
                    }
                }
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
            match current {
                TypeSpecifier::TypedefName(name) => {
                    if let Some(resolved) = self.typedefs.get(name) {
                        current = resolved;
                        continue;
                    }
                }
                TypeSpecifier::TypeofType(inner) => {
                    // typeof(type-name): resolve the inner type directly
                    current = inner;
                    continue;
                }
                _ => {}
            }
            break;
        }
        current
    }

    /// Resolve typeof(expr) to a concrete TypeSpecifier by analyzing the expression type.
    /// Returns a new TypeSpecifier if the input is Typeof, otherwise returns a clone of the input.
    pub(super) fn resolve_typeof(&self, ts: &TypeSpecifier) -> TypeSpecifier {
        match ts {
            TypeSpecifier::Typeof(expr) => {
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    Self::ctype_to_type_spec(&ctype)
                } else {
                    TypeSpecifier::Int // fallback
                }
            }
            TypeSpecifier::TypeofType(inner) => {
                // Recursively resolve inner
                self.resolve_typeof(inner)
            }
            TypeSpecifier::TypedefName(name) => {
                if let Some(resolved) = self.typedefs.get(name) {
                    // Check if the typedef itself is a typeof
                    self.resolve_typeof(resolved)
                } else {
                    ts.clone()
                }
            }
            other => other.clone(),
        }
    }

    /// Convert a CType back to a TypeSpecifier (for typeof resolution).
    fn ctype_to_type_spec(ctype: &CType) -> TypeSpecifier {
        match ctype {
            CType::Void => TypeSpecifier::Void,
            CType::Bool => TypeSpecifier::Bool,
            CType::Char => TypeSpecifier::Char,
            CType::UChar => TypeSpecifier::UnsignedChar,
            CType::Short => TypeSpecifier::Short,
            CType::UShort => TypeSpecifier::UnsignedShort,
            CType::Int => TypeSpecifier::Int,
            CType::UInt => TypeSpecifier::UnsignedInt,
            CType::Long => TypeSpecifier::Long,
            CType::ULong => TypeSpecifier::UnsignedLong,
            CType::LongLong => TypeSpecifier::LongLong,
            CType::ULongLong => TypeSpecifier::UnsignedLongLong,
            CType::Float => TypeSpecifier::Float,
            CType::Double => TypeSpecifier::Double,
            CType::LongDouble => TypeSpecifier::LongDouble,
            CType::ComplexFloat => TypeSpecifier::ComplexFloat,
            CType::ComplexDouble => TypeSpecifier::ComplexDouble,
            CType::ComplexLongDouble => TypeSpecifier::ComplexLongDouble,
            CType::Pointer(inner) => TypeSpecifier::Pointer(Box::new(Self::ctype_to_type_spec(inner))),
            CType::Array(elem, size) => TypeSpecifier::Array(
                Box::new(Self::ctype_to_type_spec(elem)),
                size.map(|s| Box::new(Expr::IntLiteral(s as i64, crate::common::source::Span::dummy()))),
            ),
            CType::Struct(st) => {
                // Return as struct tag reference if possible
                if let Some(name) = &st.name {
                    TypeSpecifier::Struct(Some(name.clone()), None, false, None)
                } else {
                    TypeSpecifier::Int // anonymous struct fallback
                }
            }
            CType::Union(st) => {
                if let Some(name) = &st.name {
                    TypeSpecifier::Union(Some(name.clone()), None, false, None)
                } else {
                    TypeSpecifier::Int // anonymous union fallback
                }
            }
            CType::Enum(et) => {
                TypeSpecifier::Enum(et.name.clone(), None)
            }
            CType::Function { .. } => TypeSpecifier::Int, // function type fallback
        }
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
            // <stdarg.h> - va_list is an array type. Size varies by arch:
            //   x86-64: 24 bytes (gp_offset, fp_offset, overflow_arg_area, reg_save_area)
            //   AArch64: 32 bytes (__stack, __gr_top, __vr_top, __gr_offs, __vr_offs)
            //   RISC-V: 8 bytes (just a pointer)
            // We use 32 bytes (the max) to be safe across all architectures.
            // va_list decays to a pointer when passed to functions (it's an array type).
            ("va_list", Array(Box::new(Char), Some(Box::new(Expr::IntLiteral(32, Span::dummy()))))),
            ("__builtin_va_list", Array(Box::new(Char), Some(Box::new(Expr::IntLiteral(32, Span::dummy()))))),
            ("__gnuc_va_list", Array(Box::new(Char), Some(Box::new(Expr::IntLiteral(32, Span::dummy()))))),
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
            self.func_meta.return_types.insert(name.to_string(), F32);
            self.func_meta.param_types.insert(name.to_string(), vec![F32]);
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
            self.func_meta.return_types.insert(name.to_string(), F64);
            self.func_meta.param_types.insert(name.to_string(), vec![F64]);
        }
        // float func(float, float) - two-arg single-precision
        let f_ff: &[&str] = &["atan2f", "powf", "fmodf", "remainderf", "copysignf", "fminf", "fmaxf", "fdimf", "hypotf"];
        for name in f_ff {
            self.func_meta.return_types.insert(name.to_string(), F32);
            self.func_meta.param_types.insert(name.to_string(), vec![F32, F32]);
        }
        // double func(double, double) - two-arg double-precision
        let d_dd: &[&str] = &["atan2", "pow", "fmod", "remainder", "copysign", "fmin", "fmax", "fdim", "hypot"];
        for name in d_dd {
            self.func_meta.return_types.insert(name.to_string(), F64);
            self.func_meta.param_types.insert(name.to_string(), vec![F64, F64]);
        }
        // int/long returning functions
        self.func_meta.return_types.insert("abs".to_string(), I32);
        self.func_meta.param_types.insert("abs".to_string(), vec![I32]);
        self.func_meta.return_types.insert("labs".to_string(), I64);
        self.func_meta.param_types.insert("labs".to_string(), vec![I64]);
        // float func(float, int)
        self.func_meta.return_types.insert("ldexpf".to_string(), F32);
        self.func_meta.param_types.insert("ldexpf".to_string(), vec![F32, I32]);
        self.func_meta.return_types.insert("ldexp".to_string(), F64);
        self.func_meta.param_types.insert("ldexp".to_string(), vec![F64, I32]);
        self.func_meta.return_types.insert("scalbnf".to_string(), F32);
        self.func_meta.param_types.insert("scalbnf".to_string(), vec![F32, I32]);
        self.func_meta.return_types.insert("scalbn".to_string(), F64);
        self.func_meta.param_types.insert("scalbn".to_string(), vec![F64, I32]);
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
            TypeSpecifier::Double => IrType::F64,
            TypeSpecifier::LongDouble => IrType::F128,
            // Complex types are handled as aggregate (pointer to stack slot)
            TypeSpecifier::ComplexFloat | TypeSpecifier::ComplexDouble | TypeSpecifier::ComplexLongDouble => IrType::Ptr,
            TypeSpecifier::Pointer(_) => IrType::Ptr,
            TypeSpecifier::Array(_, _) => IrType::Ptr,
            TypeSpecifier::Struct(_, _, _, _) | TypeSpecifier::Union(_, _, _, _) => IrType::Ptr,
            TypeSpecifier::Enum(_, _) => IrType::I32,
            TypeSpecifier::TypedefName(_) => IrType::I64, // fallback for unresolved typedef
            TypeSpecifier::Signed => IrType::I32,
            TypeSpecifier::Unsigned => IrType::U32,
            TypeSpecifier::Typeof(_) => IrType::I64, // fallback: typeof(expr) not resolved at IR level
            TypeSpecifier::TypeofType(inner) => self.type_spec_to_ir(inner),
        }
    }

    /// Get the (size, alignment) for a scalar type specifier. Returns None for
    /// compound types (arrays, structs, unions) that need recursive computation.
    fn scalar_type_size_align(ts: &TypeSpecifier) -> Option<(usize, usize)> {
        // For scalar types, size == alignment on x86-64 (except LongDouble).
        match ts {
            TypeSpecifier::Void | TypeSpecifier::Bool => Some((1, 1)),
            TypeSpecifier::Char | TypeSpecifier::UnsignedChar => Some((1, 1)),
            TypeSpecifier::Short | TypeSpecifier::UnsignedShort => Some((2, 2)),
            TypeSpecifier::Int | TypeSpecifier::UnsignedInt
            | TypeSpecifier::Signed | TypeSpecifier::Unsigned => Some((4, 4)),
            TypeSpecifier::Long | TypeSpecifier::UnsignedLong
            | TypeSpecifier::LongLong | TypeSpecifier::UnsignedLongLong => Some((8, 8)),
            TypeSpecifier::Float => Some((4, 4)),
            TypeSpecifier::Double => Some((8, 8)),
            TypeSpecifier::LongDouble => Some((16, 16)),
            TypeSpecifier::ComplexFloat => Some((8, 4)),
            TypeSpecifier::ComplexDouble => Some((16, 8)),
            TypeSpecifier::ComplexLongDouble => Some((32, 16)),
            TypeSpecifier::Pointer(_) => Some((8, 8)),
            TypeSpecifier::Enum(_, _) => Some((4, 4)),
            TypeSpecifier::TypedefName(_) => Some((8, 8)), // fallback for unresolved typedefs
            _ => None,
        }
    }

    /// Look up a struct/union layout by tag name, returning the full layout.
    fn get_struct_union_layout_by_tag(&self, kind: &str, tag: &str) -> Option<&StructLayout> {
        let key = format!("{}.{}", kind, tag);
        self.struct_layouts.get(&key)
    }

    /// Get the struct/union layout for a resolved TypeSpecifier.
    /// Handles both inline field definitions and tag-only forward references.
    fn struct_union_layout(&self, ts: &TypeSpecifier) -> Option<StructLayout> {
        match ts {
            TypeSpecifier::Struct(tag, Some(fields), is_packed, pragma_pack) => {
                // Use cached layout for tagged structs
                if let Some(tag) = tag {
                    if let Some(layout) = self.struct_layouts.get(&format!("struct.{}", tag)) {
                        return Some(layout.clone());
                    }
                }
                let max_field_align = if *is_packed { Some(1) } else { *pragma_pack };
                Some(self.compute_struct_union_layout_packed(fields, false, max_field_align))
            }
            TypeSpecifier::Union(tag, Some(fields), is_packed, pragma_pack) => {
                // Use cached layout for tagged unions
                if let Some(tag) = tag {
                    if let Some(layout) = self.struct_layouts.get(&format!("union.{}", tag)) {
                        return Some(layout.clone());
                    }
                }
                let max_field_align = if *is_packed { Some(1) } else { *pragma_pack };
                Some(self.compute_struct_union_layout_packed(fields, true, max_field_align))
            }
            TypeSpecifier::Struct(Some(tag), None, _, _) =>
                self.get_struct_union_layout_by_tag("struct", tag).cloned(),
            TypeSpecifier::Union(Some(tag), None, _, _) =>
                self.get_struct_union_layout_by_tag("union", tag).cloned(),
            _ => None,
        }
    }

    pub(super) fn compute_struct_union_layout_packed(&self, fields: &[StructFieldDecl], is_union: bool, max_field_align: Option<usize>) -> StructLayout {
        let struct_fields: Vec<StructField> = fields.iter().map(|f| {
            let bit_width = f.bit_width.as_ref().and_then(|bw| {
                self.eval_const_expr(bw).and_then(|c| c.to_u32())
            });
            let ty = self.struct_field_ctype(f);
            StructField {
                name: f.name.clone().unwrap_or_default(),
                ty,
                bit_width,
            }
        }).collect();
        if is_union {
            StructLayout::for_union(&struct_fields)
        } else {
            StructLayout::for_struct_with_packing(&struct_fields, max_field_align)
        }
    }

    pub(super) fn sizeof_type(&self, ts: &TypeSpecifier) -> usize {
        let ts = self.resolve_type_spec(ts);
        if let Some((size, _)) = Self::scalar_type_size_align(ts) {
            return size;
        }
        if let TypeSpecifier::Array(elem, Some(size_expr)) = ts {
            let elem_size = self.sizeof_type(elem);
            return self.expr_as_array_size(size_expr)
                .map(|n| elem_size * n as usize)
                .unwrap_or(elem_size);
        }
        self.struct_union_layout(ts).map(|l| l.size).unwrap_or(8)
    }

    /// Compute the alignment of a type in bytes (_Alignof).
    pub(super) fn alignof_type(&self, ts: &TypeSpecifier) -> usize {
        let ts = self.resolve_type_spec(ts);
        if let Some((_, align)) = Self::scalar_type_size_align(ts) {
            return align;
        }
        if let TypeSpecifier::Array(elem, _) = ts {
            return self.alignof_type(elem);
        }
        self.struct_union_layout(ts).map(|l| l.align).unwrap_or(8)
    }

    /// Collect array dimensions from derived declarators.
    /// Returns None for unsized dimensions (e.g., `int arr[]`).
    fn collect_derived_array_dims(&self, derived: &[DerivedDeclarator]) -> Vec<Option<usize>> {
        derived.iter().filter_map(|d| {
            if let DerivedDeclarator::Array(size_expr) = d {
                Some(size_expr.as_ref().and_then(|e| self.expr_as_array_size(e).map(|n| n as usize)))
            } else {
                None
            }
        }).collect()
    }

    /// Compute strides from an array of dimension sizes and a base element size.
    /// stride[i] = product(dims[i+1..]) * base_elem_size.
    /// E.g., dims=[3,4], base=4 -> strides=[16, 4].
    fn compute_strides_from_dims(dims: &[usize], base_elem_size: usize) -> Vec<usize> {
        let mut strides = Vec::with_capacity(dims.len());
        for i in 0..dims.len() {
            let stride: usize = dims[i+1..].iter().product::<usize>().max(1) * base_elem_size;
            strides.push(stride);
        }
        strides
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
            // Strides: [full_size, stride_for_dim_1, ..., base_elem_size]
            // full_size = product(all_dims) * base, then per-dim strides
            let full_size: usize = dims.iter().product::<usize>() * base_elem_size;
            let mut strides = vec![full_size];
            strides.extend(Self::compute_strides_from_dims(&dims, base_elem_size));
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
            // Float literal with L suffix: type long double (16 bytes)
            Expr::FloatLiteralLongDouble(_, _) => 16,
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
                    if let Some(vi) = self.lookup_var_info(name) {
                        // GCC extension: sizeof(*func_ptr) == 1
                        if let Some(ref ct) = vi.c_type {
                            if matches!(ct, CType::Function(_)) {
                                return 1;
                            }
                            if let CType::Pointer(pointee) = ct {
                                if matches!(pointee.as_ref(), CType::Function(_)) {
                                    return 1;
                                }
                            }
                        }
                        if vi.elem_size > 0 {
                            return vi.elem_size;
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
                    if let Some(vi) = self.lookup_var_info(name) {
                        if vi.elem_size > 0 {
                            return vi.elem_size;
                        }
                    }
                }
                4 // default: int element
            }

            // sizeof(sizeof(...)) or sizeof(_Alignof(...)) -> size_t = 8 on 64-bit
            Expr::Sizeof(_, _) | Expr::Alignof(_, _) => 8,

            // Cast: size of the target type
            Expr::Cast(target_type, _, _) => {
                self.sizeof_type(target_type)
            }

            // Member access: member field size (use CType for accurate array/struct sizes)
            Expr::MemberAccess(base_expr, field_name, _) => {
                if let Some(ctype) = self.resolve_field_ctype(base_expr, field_name, false) {
                    let sz = ctype.size();
                    if sz > 0 { return sz; }
                }
                let (_, field_ty) = self.resolve_member_access(base_expr, field_name);
                field_ty.size()
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                if let Some(ctype) = self.resolve_field_ctype(base_expr, field_name, true) {
                    let sz = ctype.size();
                    if sz > 0 { return sz; }
                }
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
                    UnaryOp::RealPart | UnaryOp::ImagPart => {
                        // Result is the component type size
                        let inner_ctype = self.expr_ctype(inner);
                        inner_ctype.complex_component_type().size()
                    }
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

            // Function call: use the actual return type
            Expr::FunctionCall(_, _, _) => {
                // Prefer CType which has correct struct/union sizes
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    ctype.size()
                } else {
                    let ret_ty = self.get_expr_type(expr);
                    ret_ty.size()
                }
            }

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
            let ptr_count = derived.iter().filter(|d| matches!(d, DerivedDeclarator::Pointer)).count();
            let elem_size = if let TypeSpecifier::Pointer(inner) = ts {
                // Pointer is in the type spec itself (typedef'd pointer)
                // If there are also pointer levels in derived, each derived pointer adds
                // a level of indirection (e.g., typedef int *intptr; intptr *pp -> int **pp)
                if ptr_count >= 1 {
                    8 // element is a pointer itself
                } else {
                    self.sizeof_type(self.resolve_type_spec(inner))
                }
            } else if ptr_count >= 2 {
                // Multiple pointer levels (e.g., char **p): element type is a pointer (size 8)
                8
            } else {
                self.sizeof_type(ts)
            };
            return (8, elem_size, false, true, vec![]);
        }
        if has_pointer && has_array {
            // Check order: if Pointer comes before Array in derived list,
            // this is an array of pointers (e.g., int *arr[3]).
            // If Array comes before Pointer (e.g., int (*p)[5]), it's pointer to array.
            // Exception: if FunctionPointer is present (e.g., int (*ops[2])(int,int)),
            // it's an array of function pointers regardless of Pointer/Array order.
            let ptr_pos = derived.iter().position(|d| matches!(d, DerivedDeclarator::Pointer));
            let arr_pos = derived.iter().position(|d| matches!(d, DerivedDeclarator::Array(_)));
            let has_func_ptr = derived.iter().any(|d| matches!(d,
                DerivedDeclarator::FunctionPointer(_, _) | DerivedDeclarator::Function(_, _)));

            // If pointer is from resolved type spec (not in derived), and array is in derived,
            // this is an array of typedef'd pointers (e.g., typedef int *intptr; intptr arr[3];
            // or typedef int (*fn_t)(int); fn_t ops[2];)
            let pointer_from_type_spec = ptr_pos.is_none() && matches!(ts, TypeSpecifier::Pointer(_));

            if has_func_ptr || pointer_from_type_spec || matches!((ptr_pos, arr_pos), (Some(pp), Some(ap)) if pp < ap) {
                // Array of pointers: int *arr[3] or typedef'd_ptr arr[3]
                // Each element is a pointer (8 bytes)
                let array_dims = self.collect_derived_array_dims(derived);
                let resolved_dims: Vec<usize> = array_dims.iter().map(|d| d.unwrap_or(256)).collect();
                let total_size: usize = resolved_dims.iter().product::<usize>() * 8;
                let strides = if resolved_dims.len() > 1 {
                    Self::compute_strides_from_dims(&resolved_dims, 8)
                } else {
                    vec![8]  // 1D pointer array: stride is just pointer size
                };
                return (total_size, 8, true, false, strides);
            }
            // Pointer to array (e.g., int (*p)[5]) - treat as pointer
            // Compute strides from the Array dims in the derived list
            let array_dims: Vec<usize> = self.collect_derived_array_dims(derived)
                .iter().map(|d| d.unwrap_or(1)).collect();
            let base_elem_size = self.sizeof_type(ts);
            let full_array_size: usize = array_dims.iter().product::<usize>() * base_elem_size;
            // strides[0] = full pointed-to array size, then per-dim strides
            let mut strides = vec![full_array_size];
            strides.extend(Self::compute_strides_from_dims(&array_dims, base_elem_size));
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
        let array_dims = self.collect_derived_array_dims(derived);

        if !array_dims.is_empty() {
            // If derived declarators include Function/FunctionPointer,
            // the element type is a function pointer (8 bytes), not the return type.
            let has_func_ptr = derived.iter().any(|d| matches!(d,
                DerivedDeclarator::Function(_, _) | DerivedDeclarator::FunctionPointer(_, _)));
            // Also account for array dimensions in the type specifier itself
            // e.g., if type is Array(Array(Int, 3), 2) from the parser,
            // or from a typedef like: typedef unsigned char byte4_t[4];
            let type_dims = self.collect_type_array_dims(ts);

            let base_elem_size = if has_func_ptr {
                8 // function pointer size
            } else if !type_dims.is_empty() {
                // When ts itself is an array type (from typedef), use the innermost
                // element size, since collect_type_array_dims already extracts the
                // array dimensions which will be included in all_dims.
                let mut inner = ts;
                while let TypeSpecifier::Array(elem, _) = inner {
                    inner = elem.as_ref();
                }
                self.sizeof_type(inner).max(1)
            } else {
                self.sizeof_type(ts).max(1)
            };

            // Combine: derived dims come first (outermost), then type dims
            let all_dims: Vec<usize> = array_dims.iter().map(|d| d.unwrap_or(256))
                .chain(type_dims.iter().copied())
                .collect();

            // Compute total size = product of all dims * base_elem_size
            let total: usize = all_dims.iter().product::<usize>() * base_elem_size;

            // Compute strides: stride[i] = product of dims[i+1..] * base_elem_size
            let strides = Self::compute_strides_from_dims(&all_dims, base_elem_size);

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
            TypeSpecifier::ComplexFloat => CType::ComplexFloat,
            TypeSpecifier::ComplexDouble => CType::ComplexDouble,
            TypeSpecifier::ComplexLongDouble => CType::ComplexLongDouble,
            TypeSpecifier::Pointer(inner) => CType::Pointer(Box::new(self.type_spec_to_ctype(inner))),
            TypeSpecifier::Array(elem, size_expr) => {
                let elem_ctype = self.type_spec_to_ctype(elem);
                let size = size_expr.as_ref().and_then(|e| {
                    self.expr_as_array_size(e).map(|n| n as usize)
                });
                CType::Array(Box::new(elem_ctype), size)
            }
            TypeSpecifier::Struct(name, fields, is_packed, pragma_pack) => {
                self.struct_or_union_to_ctype(name, fields, false, *is_packed, *pragma_pack)
            }
            TypeSpecifier::Union(name, fields, is_packed, pragma_pack) => {
                self.struct_or_union_to_ctype(name, fields, true, *is_packed, *pragma_pack)
            }
            TypeSpecifier::Enum(_, _) => CType::Int, // enums are int-sized
            TypeSpecifier::TypedefName(_) => CType::Int, // TODO: resolve typedef
            TypeSpecifier::Typeof(expr) => {
                // typeof(expr): get type from expression
                self.get_expr_ctype(expr).unwrap_or(CType::Int)
            }
            TypeSpecifier::TypeofType(inner_ts) => {
                // typeof(type): just use the inner type
                self.type_spec_to_ctype(inner_ts)
            }
        }
    }

    /// Convert a struct or union TypeSpecifier to CType.
    /// `is_union` selects between struct and union semantics.
    /// `is_packed` indicates __attribute__((packed)).
    /// `pragma_pack` is the #pragma pack(N) alignment, if any.
    fn struct_or_union_to_ctype(
        &self,
        name: &Option<String>,
        fields: &Option<Vec<StructFieldDecl>>,
        is_union: bool,
        is_packed: bool,
        pragma_pack: Option<usize>,
    ) -> CType {
        let make = |st: crate::common::types::StructType| -> CType {
            let arc_st = std::sync::Arc::new(st);
            if is_union { CType::Union(arc_st) } else { CType::Struct(arc_st) }
        };
        let prefix = if is_union { "union" } else { "struct" };
        // __attribute__((packed)) forces alignment 1; #pragma pack(N) caps to N.
        let max_field_align = if is_packed { Some(1) } else { pragma_pack };

        if let Some(fs) = fields {
            // Inline definition with fields: check cache first for named types
            if let Some(tag) = name {
                let cache_key = format!("{}.{}", prefix, tag);
                if let Some(cached) = self.ctype_cache.borrow().get(&cache_key) {
                    return cached.clone();
                }
            }
            let struct_fields: Vec<StructField> = fs.iter().map(|f| {
                let bit_width = f.bit_width.as_ref().and_then(|bw| {
                    self.eval_const_expr(bw).and_then(|c| c.to_u32())
                });
                let ty = self.struct_field_ctype(f);
                StructField {
                    name: f.name.clone().unwrap_or_default(),
                    ty,
                    bit_width,
                }
            }).collect();
            let st = if is_union {
                crate::common::types::StructType::new_union(name.clone(), struct_fields, is_packed, max_field_align)
            } else {
                crate::common::types::StructType::new_struct(name.clone(), struct_fields, is_packed, max_field_align)
            };
            let result = make(st);
            if let Some(tag) = name {
                let cache_key = format!("{}.{}", prefix, tag);
                self.ctype_cache.borrow_mut().insert(cache_key, result.clone());
            }
            result
        } else if let Some(tag) = name {
            let cache_key = format!("{}.{}", prefix, tag);
            // Check cache first
            if let Some(cached) = self.ctype_cache.borrow().get(&cache_key) {
                return cached.clone();
            }
            let key = format!("{}.{}", prefix, tag);
            if let Some(layout) = self.struct_layouts.get(&key) {
                let struct_fields: Vec<StructField> = layout.fields.iter().map(|f| {
                    StructField {
                        name: f.name.clone(),
                        ty: f.ty.clone(),
                        bit_width: f.bit_width,
                    }
                }).collect();
                let st = if is_union {
                    crate::common::types::StructType::new_union(Some(tag.clone()), struct_fields, is_packed, max_field_align)
                } else {
                    crate::common::types::StructType::new_struct(Some(tag.clone()), struct_fields, is_packed, max_field_align)
                };
                let result = make(st);
                self.ctype_cache.borrow_mut().insert(cache_key, result.clone());
                result
            } else {
                make(crate::common::types::StructType::new_empty(Some(tag.clone()), is_packed, max_field_align))
            }
        } else {
            make(crate::common::types::StructType::new_empty(None, is_packed, max_field_align))
        }
    }

    /// Get the CType for a struct field declaration, accounting for derived declarators.
    /// For simple fields (derived is empty), just converts type_spec.
    /// For complex fields (function pointers, etc.), uses build_full_ctype.
    pub(super) fn struct_field_ctype(&self, f: &StructFieldDecl) -> CType {
        if f.derived.is_empty() {
            self.type_spec_to_ctype(&f.type_spec)
        } else {
            self.build_full_ctype(&f.type_spec, &f.derived)
        }
    }

    /// Build a full CType from a TypeSpecifier and DerivedDeclarator chain.
    ///
    /// The derived list is produced by parse_declarator's combine_declarator_parts,
    /// which stores declarators outer-to-inner. For building the CType, we need to
    /// process inner-to-outer (inside-out rule).
    ///
    /// Examples (derived list -> CType):
    /// - `int **p`: [Pointer, Pointer] -> Pointer(Pointer(Int))
    /// - `int *arr[3]`: [Pointer, Array(3)] -> Array(Pointer(Int), 3)
    /// - `int (*fp)(int)`: [Pointer, FunctionPointer([int])] -> Pointer(Function(Int->Int))
    /// - `int (*fp[3])(int)`: [Array(3), Pointer, FunctionPointer([int])] -> Array(Pointer(Function(Int->Int)), 3)
    pub(super) fn build_full_ctype(&self, type_spec: &TypeSpecifier, derived: &[DerivedDeclarator]) -> CType {
        let resolved = self.resolve_type_spec(type_spec);
        let base = self.type_spec_to_ctype(resolved);

        // Process derived declarators. The list is ordered outer-to-inner:
        // outermost wrapping (e.g. Array) first, innermost (closest to base type, e.g.
        // FunctionPointer) last. We need to build inside-out: first build the
        // inner type from the base, then wrap with outer layers.
        //
        // Strategy: find the innermost function pointer group first (Pointer+FunctionPointer),
        // build the function pointer type from the base, then apply remaining outer
        // wrappers (Array, Pointer).

        // Separate into prefix (outer wrappers) and suffix (Pointer+FunctionPointer core)
        // Look for the pattern: [outer...] [Pointer] [FunctionPointer]
        // where the Pointer+FunctionPointer pair is the function pointer core.
        let fptr_idx = self.find_function_pointer_core(derived);

        if let Some(fp_start) = fptr_idx {
            // Build the function pointer type from base
            let mut result = base;

            // Process from fp_start to end (the function pointer core and any
            // additional inner wrappers after it)
            let mut i = fp_start;
            while i < derived.len() {
                match &derived[i] {
                    DerivedDeclarator::Pointer => {
                        if i + 1 < derived.len() && matches!(&derived[i + 1], DerivedDeclarator::FunctionPointer(params, _) | DerivedDeclarator::Function(params, _)) {
                            let (params, variadic) = match &derived[i + 1] {
                                DerivedDeclarator::FunctionPointer(p, v) | DerivedDeclarator::Function(p, v) => (p, *v),
                                _ => unreachable!(),
                            };
                            let param_types = self.convert_param_decls_to_ctypes(params);
                            let func_type = CType::Function(Box::new(crate::common::types::FunctionType {
                                return_type: result,
                                params: param_types,
                                variadic,
                            }));
                            result = CType::Pointer(Box::new(func_type));
                            i += 2;
                        } else {
                            result = CType::Pointer(Box::new(result));
                            i += 1;
                        }
                    }
                    DerivedDeclarator::FunctionPointer(params, variadic) => {
                        let param_types = self.convert_param_decls_to_ctypes(params);
                        let func_type = CType::Function(Box::new(crate::common::types::FunctionType {
                            return_type: result,
                            params: param_types,
                            variadic: *variadic,
                        }));
                        result = CType::Pointer(Box::new(func_type));
                        i += 1;
                    }
                    DerivedDeclarator::Function(params, variadic) => {
                        let param_types = self.convert_param_decls_to_ctypes(params);
                        let func_type = CType::Function(Box::new(crate::common::types::FunctionType {
                            return_type: result,
                            params: param_types,
                            variadic: *variadic,
                        }));
                        result = func_type;
                        i += 1;
                    }
                    _ => { i += 1; }
                }
            }

            // Now apply outer wrappers (prefix before fp_start): Array, Pointer
            // These are outermost, so apply them in reverse order
            let prefix = &derived[..fp_start];
            for d in prefix.iter().rev() {
                match d {
                    DerivedDeclarator::Array(size_expr) => {
                        let size = size_expr.as_ref().and_then(|e| {
                            self.expr_as_array_size(e).map(|n| n as usize)
                        });
                        result = CType::Array(Box::new(result), size);
                    }
                    DerivedDeclarator::Pointer => {
                        result = CType::Pointer(Box::new(result));
                    }
                    _ => {}
                }
            }

            result
        } else {
            // No function pointer - simple case
            let mut result = base;
            let mut i = 0;
            while i < derived.len() {
                match &derived[i] {
                    DerivedDeclarator::Pointer => {
                        result = CType::Pointer(Box::new(result));
                        i += 1;
                    }
                    DerivedDeclarator::Array(_) => {
                        let start = i;
                        while i < derived.len() && matches!(&derived[i], DerivedDeclarator::Array(_)) {
                            i += 1;
                        }
                        for j in (start..i).rev() {
                            if let DerivedDeclarator::Array(size_expr) = &derived[j] {
                                let size = size_expr.as_ref().and_then(|e| {
                                    self.expr_as_array_size(e).map(|n| n as usize)
                                });
                                result = CType::Array(Box::new(result), size);
                            }
                        }
                    }
                    _ => { i += 1; }
                }
            }
            result
        }
    }

    /// Find the start index of the function pointer core in a derived declarator list.
    /// The function pointer core is [Pointer, FunctionPointer] or standalone FunctionPointer.
    fn find_function_pointer_core(&self, derived: &[DerivedDeclarator]) -> Option<usize> {
        // Look for Pointer followed by FunctionPointer
        for i in 0..derived.len() {
            if matches!(&derived[i], DerivedDeclarator::Pointer) {
                if i + 1 < derived.len() && matches!(&derived[i + 1], DerivedDeclarator::FunctionPointer(_, _)) {
                    return Some(i);
                }
            }
            // Standalone FunctionPointer
            if matches!(&derived[i], DerivedDeclarator::FunctionPointer(_, _)) {
                return Some(i);
            }
            // Standalone Function (for function declarations)
            if matches!(&derived[i], DerivedDeclarator::Function(_, _)) {
                return Some(i);
            }
        }
        None
    }

    /// Convert ParamDecl list to CType list for function types.
    fn convert_param_decls_to_ctypes(&self, params: &[ParamDecl]) -> Vec<(CType, Option<String>)> {
        params.iter().map(|p| {
            let ty = self.type_spec_to_ctype(&self.resolve_type_spec(&p.type_spec));
            (ty, p.name.clone())
        }).collect()
    }

    /// Get the full CType of an expression by recursion.
    /// Returns None if the type cannot be determined from CType tracking.
    pub(super) fn get_expr_ctype(&self, expr: &Expr) -> Option<CType> {
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(vi) = self.lookup_var_info(name) {
                    return vi.c_type.clone();
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
                            match lct {
                                CType::Pointer(_) => return Some(lct),
                                // Array decays to pointer-to-element in arithmetic context
                                CType::Array(elem, _) => return Some(CType::Pointer(elem)),
                                _ => {}
                            }
                        }
                        if let Some(rct) = self.get_expr_ctype(rhs) {
                            match rct {
                                CType::Pointer(_) => return Some(rct),
                                CType::Array(elem, _) => return Some(CType::Pointer(elem)),
                                _ => {}
                            }
                        }
                        None
                    }
                    BinOp::Sub => {
                        if let Some(lct) = self.get_expr_ctype(lhs) {
                            let is_ptr = matches!(&lct, CType::Pointer(_) | CType::Array(_, _));
                            if is_ptr {
                                // Check if rhs is also pointer (ptr - ptr = ptrdiff_t)
                                if let Some(rct) = self.get_expr_ctype(rhs) {
                                    if matches!(&rct, CType::Pointer(_) | CType::Array(_, _)) {
                                        return Some(CType::Long);
                                    }
                                }
                                // ptr - int = decayed pointer type
                                return Some(match lct {
                                    CType::Array(elem, _) => CType::Pointer(elem),
                                    other => other,
                                });
                            }
                        }
                        None
                    }
                    _ => None,
                }
            }
            // Literal types for _Generic support
            Expr::IntLiteral(_, _) => Some(CType::Int),
            Expr::UIntLiteral(_, _) => Some(CType::UInt),
            Expr::LongLiteral(_, _) => Some(CType::Long),
            Expr::ULongLiteral(_, _) => Some(CType::ULong),
            Expr::CharLiteral(_, _) => Some(CType::Int), // char literals have type int in C
            Expr::FloatLiteral(_, _) => Some(CType::Double),
            Expr::FloatLiteralF32(_, _) => Some(CType::Float),
            Expr::FloatLiteralLongDouble(_, _) => Some(CType::LongDouble),
            Expr::StringLiteral(_, _) => Some(CType::Pointer(Box::new(CType::Char))),
            Expr::FunctionCall(func, _, _) => {
                if let Expr::Identifier(name, _) = func.as_ref() {
                    if let Some(ctype) = self.func_meta.return_ctypes.get(name) {
                        return Some(ctype.clone());
                    }
                }
                None
            }
            Expr::VaArg(_, type_spec, _) => {
                let resolved = self.resolve_type_spec(type_spec);
                Some(self.type_spec_to_ctype(resolved))
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
