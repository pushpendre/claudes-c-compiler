use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, StructField, StructLayout, CType};
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
                self.eval_const_binop(op, &l, &r)
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
            Expr::Cast(_, inner, _) => {
                // For now, just pass through casts in constant expressions
                self.eval_const_expr(inner)
            }
            Expr::Identifier(name, _) => {
                // Look up enum constants
                if let Some(&val) = self.enum_constants.get(name) {
                    Some(IrConst::I64(val))
                } else {
                    None
                }
            }
            _ => None,
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
                        // Address of a global variable
                        if self.globals.contains_key(name) {
                            return Some(GlobalInit::GlobalAddr(name.clone()));
                        }
                        None
                    }
                    // &arr[index] -> GlobalAddrOffset("arr", index * elem_size)
                    Expr::ArraySubscript(base, index, _) => {
                        if let Expr::Identifier(name, _) = base.as_ref() {
                            if let Some(ginfo) = self.globals.get(name.as_str()) {
                                if ginfo.is_array {
                                    if let Some(idx_val) = self.eval_const_expr(index) {
                                        if let Some(idx) = self.const_to_i64(&idx_val) {
                                            let offset = idx * ginfo.elem_size as i64;
                                            if offset == 0 {
                                                return Some(GlobalInit::GlobalAddr(name.clone()));
                                            }
                                            return Some(GlobalInit::GlobalAddrOffset(name.clone(), offset));
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
                            if let Some(ginfo) = self.globals.get(name) {
                                if let Some(ref layout) = ginfo.struct_layout {
                                    for f in &layout.fields {
                                        if f.name == *field {
                                            if f.offset == 0 {
                                                return Some(GlobalInit::GlobalAddr(name.clone()));
                                            }
                                            return Some(GlobalInit::GlobalAddrOffset(name.clone(), f.offset as i64));
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
    fn eval_const_binop(&self, op: &BinOp, lhs: &IrConst, rhs: &IrConst) -> Option<IrConst> {
        let l = self.const_to_i64(lhs)?;
        let r = self.const_to_i64(rhs)?;
        let result = match op {
            BinOp::Add => l.wrapping_add(r),
            BinOp::Sub => l.wrapping_sub(r),
            BinOp::Mul => l.wrapping_mul(r),
            BinOp::Div => if r != 0 { l.wrapping_div(r) } else { return None; },
            BinOp::Mod => if r != 0 { l.wrapping_rem(r) } else { return None; },
            BinOp::BitAnd => l & r,
            BinOp::BitOr => l | r,
            BinOp::BitXor => l ^ r,
            BinOp::Shl => l.wrapping_shl(r as u32),
            BinOp::Shr => l.wrapping_shr(r as u32),
            _ => return None,
        };
        Some(IrConst::I64(result))
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
        if let Some(val) = self.eval_const_expr(expr) {
            match val {
                IrConst::I8(v) => Some(v as usize),
                IrConst::I16(v) => Some(v as usize),
                IrConst::I32(v) => Some(v as usize),
                IrConst::I64(v) => Some(v as usize),
                IrConst::Zero => Some(0),
                _ => None,
            }
        } else {
            None
        }
    }

    /// Convert an IrConst to i64.
    pub(super) fn const_to_i64(&self, c: &IrConst) -> Option<i64> {
        match c {
            IrConst::I8(v) => Some(*v as i64),
            IrConst::I16(v) => Some(*v as i64),
            IrConst::I32(v) => Some(*v as i64),
            IrConst::I64(v) => Some(*v),
            IrConst::Zero => Some(0),
            _ => None,
        }
    }

    /// Get the zero constant for a given IR type.
    pub(super) fn zero_const(&self, ty: IrType) -> IrConst {
        match ty {
            IrType::I8 | IrType::U8 => IrConst::I8(0),
            IrType::I16 | IrType::U16 => IrConst::I16(0),
            IrType::I32 | IrType::U32 => IrConst::I32(0),
            IrType::I64 | IrType::U64 | IrType::Ptr => IrConst::I64(0),
            IrType::F32 => IrConst::F32(0.0),
            IrType::F64 => IrConst::F64(0.0),
            IrType::Void => IrConst::Zero,
        }
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
            Expr::Deref(_, _) => {
                // *ptr where ptr points to struct - could be struct value
                // TODO: track pointed-to type more precisely
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
            Expr::Conditional(_, then_expr, _, _) => return self.get_expr_type(then_expr),
            Expr::Comma(_, rhs, _) => return self.get_expr_type(rhs),
            Expr::PostfixOp(_, inner, _) => return self.get_expr_type(inner),
            Expr::AddressOf(_, _) => return IrType::Ptr,
            Expr::FunctionCall(func, _, _) => {
                // Look up the return type of the called function
                if let Expr::Identifier(name, _) = func.as_ref() {
                    if let Some(&ret_ty) = self.function_return_types.get(name) {
                        return ret_ty;
                    }
                }
                return IrType::I64;
            }
            _ => {}
        }
        match expr {
            Expr::Identifier(name, _) => {
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
            // <stdarg.h> - va_list is a pointer-like type
            ("va_list", Pointer(Box::new(Void))),
            ("__builtin_va_list", Pointer(Box::new(Void))),
            ("__gnuc_va_list", Pointer(Box::new(Void))),
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

    pub(super) fn type_spec_to_ir(&self, ts: &TypeSpecifier) -> IrType {
        let ts = self.resolve_type_spec(ts);
        match ts {
            TypeSpecifier::Void => IrType::Void,
            TypeSpecifier::Char => IrType::I8,
            TypeSpecifier::UnsignedChar => IrType::U8,
            TypeSpecifier::Short => IrType::I16,
            TypeSpecifier::UnsignedShort => IrType::U16,
            TypeSpecifier::Bool => IrType::U8,
            TypeSpecifier::Int => IrType::I32,
            TypeSpecifier::UnsignedInt => IrType::U32,
            TypeSpecifier::Long | TypeSpecifier::LongLong => IrType::I64,
            TypeSpecifier::UnsignedLong | TypeSpecifier::UnsignedLongLong => IrType::U64,
            TypeSpecifier::Float => IrType::F32,
            TypeSpecifier::Double => IrType::F64,
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
            TypeSpecifier::Void => 0,
            TypeSpecifier::Char | TypeSpecifier::UnsignedChar => 1,
            TypeSpecifier::Short | TypeSpecifier::UnsignedShort => 2,
            TypeSpecifier::Bool => 1,
            TypeSpecifier::Int | TypeSpecifier::UnsignedInt => 4,
            TypeSpecifier::Long | TypeSpecifier::UnsignedLong
            | TypeSpecifier::LongLong | TypeSpecifier::UnsignedLongLong => 8,
            TypeSpecifier::Float => 4,
            TypeSpecifier::Double => 8,
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
                    StructField {
                        name: f.name.clone().unwrap_or_default(),
                        ty: self.type_spec_to_ctype(&f.type_spec),
                        bit_width: None,
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
                    return ginfo.ty.size();
                }
                4 // default: int
            }

            // Dereference: element type size
            Expr::Deref(inner, _) => {
                if let Expr::Identifier(name, _) = inner.as_ref() {
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
                8 // TODO: better type tracking for nested derefs
            }

            // Array subscript: element type size
            Expr::ArraySubscript(base, _, _) => {
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
        // Check for pointer declarators
        let has_pointer = derived.iter().any(|d| matches!(d, DerivedDeclarator::Pointer));

        let has_array = derived.iter().any(|d| matches!(d, DerivedDeclarator::Array(_)));

        // Handle pointer and array combinations
        if has_pointer && !has_array {
            // Simple pointer: int *p
            let elem_size = self.sizeof_type(ts);
            return (8, elem_size, false, true, vec![]);
        }
        if has_pointer && has_array {
            // Check order: if Pointer comes before Array in derived list,
            // this is an array of pointers (e.g., int *arr[3]).
            // If Array comes before Pointer (e.g., int (*p)[5]), it's pointer to array.
            let ptr_pos = derived.iter().position(|d| matches!(d, DerivedDeclarator::Pointer));
            let arr_pos = derived.iter().position(|d| matches!(d, DerivedDeclarator::Array(_)));
            if let (Some(pp), Some(ap)) = (ptr_pos, arr_pos) {
                if pp < ap {
                    // Array of pointers: int *arr[3]
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
            }
            // Pointer to array (e.g., int (*p)[5]) - treat as pointer
            let elem_size = self.sizeof_type(ts);
            return (8, elem_size, false, true, vec![]);
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

        // Regular scalar - we use 8-byte slots for each stack value
        (8, 0, false, false, vec![])
    }

    /// Collect array dimensions from nested Array type specifiers.
    /// Extract an integer value from any integer literal expression (Int, UInt, Long, ULong).
    /// Used for array sizes and other compile-time integer expressions.
    pub(super) fn expr_as_array_size(&self, expr: &Expr) -> Option<i64> {
        match expr {
            Expr::IntLiteral(n, _) | Expr::LongLiteral(n, _) => Some(*n),
            Expr::UIntLiteral(n, _) | Expr::ULongLiteral(n, _) => Some(*n as i64),
            _ => None,
        }
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
            TypeSpecifier::Int | TypeSpecifier::Signed => CType::Int,
            TypeSpecifier::Bool => CType::UChar,
            TypeSpecifier::UnsignedInt | TypeSpecifier::Unsigned => CType::UInt,
            TypeSpecifier::Long => CType::Long,
            TypeSpecifier::UnsignedLong => CType::ULong,
            TypeSpecifier::LongLong => CType::LongLong,
            TypeSpecifier::UnsignedLongLong => CType::ULongLong,
            TypeSpecifier::Float => CType::Float,
            TypeSpecifier::Double => CType::Double,
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
                StructField {
                    name: f.name.clone().unwrap_or_default(),
                    ty: self.type_spec_to_ctype(&f.type_spec),
                    bit_width: None,
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
                        bit_width: None,
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
    pub(super) fn build_full_ctype(&self, type_spec: &TypeSpecifier, derived: &[DerivedDeclarator]) -> CType {
        let resolved = self.resolve_type_spec(type_spec);
        let base = self.type_spec_to_ctype(resolved);
        let mut result = base;
        // Derived declarators are in reverse order: `int *p[]` has [Pointer, Array]
        // but semantically it's Array of Pointer to Int, so process in reverse.
        for d in derived.iter().rev() {
            match d {
                DerivedDeclarator::Pointer => {
                    result = CType::Pointer(Box::new(result));
                }
                DerivedDeclarator::Array(size_expr) => {
                    let size = size_expr.as_ref().and_then(|e| {
                        self.expr_as_array_size(e).map(|n| n as usize)
                    });
                    result = CType::Array(Box::new(result), size);
                }
                DerivedDeclarator::Function(_, _) | DerivedDeclarator::FunctionPointer(_, _) => {
                    // Function declarator - treat as pointer to function
                    result = CType::Pointer(Box::new(result));
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
