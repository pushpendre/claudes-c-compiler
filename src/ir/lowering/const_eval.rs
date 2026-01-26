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

    /// Check if an expression tree contains a Sizeof node at any depth.
    /// Used to avoid trusting sema's pre-computed values for expressions involving
    /// sizeof, since sema evaluates sizeof before the lowerer resolves unsized array
    /// dimensions from initializers. For example, `sizeof(cases) / sizeof(cases[0]) + 1`
    /// contains Sizeof nodes in the division subtree, so the entire expression must
    /// be recomputed by the lowerer.
    fn expr_contains_sizeof(expr: &Expr) -> bool {
        match expr {
            Expr::Sizeof(_, _) => true,
            Expr::BinaryOp(_, lhs, rhs, _) => {
                Self::expr_contains_sizeof(lhs) || Self::expr_contains_sizeof(rhs)
            }
            Expr::UnaryOp(_, inner, _) | Expr::PostfixOp(_, inner, _) => {
                Self::expr_contains_sizeof(inner)
            }
            Expr::Cast(_, inner, _) => Self::expr_contains_sizeof(inner),
            Expr::Conditional(cond, then_e, else_e, _) => {
                Self::expr_contains_sizeof(cond)
                    || Self::expr_contains_sizeof(then_e)
                    || Self::expr_contains_sizeof(else_e)
            }
            Expr::GnuConditional(cond, else_e, _) => {
                Self::expr_contains_sizeof(cond) || Self::expr_contains_sizeof(else_e)
            }
            Expr::Comma(lhs, rhs, _) => {
                Self::expr_contains_sizeof(lhs) || Self::expr_contains_sizeof(rhs)
            }
            _ => false,
        }
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
        // We skip the sema lookup for:
        //   - Identifiers: the lowerer may have more information (const local values,
        //     static locals) that sema lacks.
        //   - Expressions containing sizeof: sema may have computed sizeof for unsized
        //     arrays (e.g., `PT cases[] = {1,2,3,...}`) before the lowerer resolved the
        //     actual element count from the initializer. The lowerer's sizeof_expr/
        //     sizeof_type uses the correctly-sized global, so we must recompute.
        //     This check covers sizeof itself and any parent expression containing it,
        //     such as `sizeof(x) / sizeof(x[0]) + 1`.
        if !matches!(expr, Expr::Identifier(_, _)) && !Self::expr_contains_sizeof(expr) {
            if let Some(val) = self.lookup_sema_const(expr) {
                return Some(val);
            }
        }
        match expr {
            // Preserve C type width: IntLiteral is `int` (32-bit) when value fits, otherwise `long`.
            Expr::IntLiteral(val, _) => {
                if *val >= i32::MIN as i64 && *val <= i32::MAX as i64 {
                    Some(IrConst::I32(*val as i32))
                } else {
                    Some(IrConst::I64(*val))
                }
            }
            Expr::LongLiteral(val, _) => {
                Some(IrConst::I64(*val))
            }
            // UIntLiteral stays as I64 to preserve the unsigned value.
            Expr::UIntLiteral(val, _) => {
                Some(IrConst::I64(*val as i64))
            }
            Expr::ULongLiteral(val, _) => {
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
                let val = self.eval_const_expr(inner)?;
                // C integer promotion: promote sub-int types to int before negation.
                // For unsigned sub-int types (unsigned char/short), zero-extend to
                // preserve the unsigned value. Without this, I8(-1) representing
                // unsigned char 255 would be negated as -(-1) = 1 instead of -(255) = -255.
                let promoted = self.promote_const_for_unary(inner, val);
                const_arith::negate_const(promoted)
            }
            Expr::BinaryOp(op, lhs, rhs, _) => {
                let l = self.eval_const_expr(lhs)?;
                let r = self.eval_const_expr(rhs)?;
                // Use infer_expr_type (C semantic types) for proper usual arithmetic
                // conversions. get_expr_type returns IR storage types (IntLiteral → I64)
                // which loses 32-bit width info needed for correct folding of
                // expressions like (1 << 31) / N.
                let lhs_ty = self.infer_expr_type(lhs);
                let rhs_ty = self.infer_expr_type(rhs);
                self.eval_const_binop(op, &l, &r, lhs_ty, rhs_ty)
            }
            Expr::UnaryOp(UnaryOp::BitNot, inner, _) => {
                let val = self.eval_const_expr(inner)?;
                let promoted = self.promote_const_for_unary(inner, val);
                const_arith::bitnot_const(promoted)
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

                // Handle I128 source: use full 128-bit value to avoid truncation
                // through the u64-based eval_const_expr_as_bits path
                if let IrConst::I128(v128) = src_val {
                    // Determine source signedness for int-to-float conversions
                    let src_ty = self.get_expr_type(inner);
                    let src_unsigned = src_ty.is_unsigned();
                    return Some(Self::cast_i128_to_ir_type(v128, target_ir_ty, src_unsigned));
                }

                // Integer source: use bit-based cast chain evaluation
                let (bits, _src_signed) = self.eval_const_expr_as_bits(inner)?;
                let target_width = target_ir_ty.size() * 8;
                let target_signed = matches!(target_ir_ty, IrType::I8 | IrType::I16 | IrType::I32 | IrType::I64 | IrType::I128);

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
                    IrType::I128 | IrType::U128 => IrConst::I128(truncated as i128),
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
            Expr::AlignofExpr(ref inner_expr, _) => {
                let align = self.alignof_expr(inner_expr);
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
            // Handle compile-time builtin function calls in constant expressions.
            // __builtin_choose_expr(const_expr, expr1, expr2) selects expr1 or expr2
            // at compile time. __builtin_constant_p(expr) returns 1 if expr is a
            // compile-time constant, 0 otherwise. These are needed for global
            // initializer contexts where the result must be a constant.
            Expr::FunctionCall(func, args, _) => {
                if let Expr::Identifier(name, _) = func.as_ref() {
                    match name.as_str() {
                        "__builtin_choose_expr" if args.len() >= 3 => {
                            let cond = self.eval_const_expr(&args[0])?;
                            if cond.is_nonzero() {
                                self.eval_const_expr(&args[1])
                            } else {
                                self.eval_const_expr(&args[2])
                            }
                        }
                        "__builtin_constant_p" => {
                            let is_const = if let Some(arg) = args.first() {
                                self.eval_const_expr(arg).is_some()
                            } else {
                                false
                            };
                            Some(IrConst::I32(if is_const { 1 } else { 0 }))
                        }
                        "__builtin_expect" | "__builtin_expect_with_probability" => {
                            // __builtin_expect(val, expected) -> val
                            if let Some(arg) = args.first() {
                                self.eval_const_expr(arg)
                            } else {
                                None
                            }
                        }
                        "__builtin_bswap16" => {
                            let val = self.eval_const_expr(args.first()?)?;
                            let v = val.to_i64()? as u16;
                            Some(IrConst::I32(v.swap_bytes() as i32))
                        }
                        "__builtin_bswap32" => {
                            let val = self.eval_const_expr(args.first()?)?;
                            let v = val.to_i64()? as u32;
                            Some(IrConst::I32(v.swap_bytes() as i32))
                        }
                        "__builtin_bswap64" => {
                            let val = self.eval_const_expr(args.first()?)?;
                            let v = val.to_i64()? as u64;
                            Some(IrConst::I64(v.swap_bytes() as i64))
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            // Handle compound literals in constant expressions:
            // ((type) { value }) -> evaluate the inner initializer's scalar value.
            // This is critical for global/static array initializers where compound
            // literals like ((pgprot_t) { 0x120 }) must be evaluated at compile time.
            // Only treat as scalar if the type is NOT a multi-field aggregate;
            // multi-field structs must go through the proper struct init path.
            Expr::CompoundLiteral(ref type_spec, ref init, _) => {
                let cl_ctype = self.type_spec_to_ctype(type_spec);
                let is_multi_field_aggregate = match &cl_ctype {
                    CType::Struct(key) | CType::Union(key) => {
                        if let Some(layout) = self.types.struct_layouts.get(&**key) {
                            layout.fields.len() > 1
                        } else {
                            false
                        }
                    }
                    CType::Array(..) => true,
                    _ => false,
                };
                if is_multi_field_aggregate {
                    None
                } else {
                    self.eval_const_initializer_scalar(init)
                }
            }
            _ => None,
        }
    }

    /// Evaluate an initializer to a scalar constant for use in constant expressions.
    /// Handles both direct expressions and brace-wrapped lists (including nested ones
    /// like `{ { 42 } }` which occur when a struct compound literal has a single field).
    fn eval_const_initializer_scalar(&self, init: &Initializer) -> Option<IrConst> {
        match init {
            Initializer::Expr(expr) => self.eval_const_expr(expr),
            Initializer::List(items) => {
                // For a struct/union compound literal with one field,
                // the initializer list has one item whose value is the scalar.
                // Recurse into the first item.
                if let Some(first) = items.first() {
                    self.eval_const_initializer_scalar(&first.init)
                } else {
                    None
                }
            }
        }
    }

    /// Try to extract a global address from an initializer.
    /// Recurses into brace-wrapped lists to find the first pointer/address value.
    /// This handles compound literals like `((struct Wrap) {inc_global})` where
    /// the inner initializer contains a function pointer or global address.
    fn eval_global_addr_from_initializer(&self, init: &Initializer) -> Option<GlobalInit> {
        match init {
            Initializer::Expr(expr) => self.eval_global_addr_expr(expr),
            Initializer::List(items) => {
                if let Some(first) = items.first() {
                    self.eval_global_addr_from_initializer(&first.init)
                } else {
                    None
                }
            }
        }
    }

    /// Evaluate the offsetof pattern: &((type*)0)->member
    /// Also handles nested member access like &((type*)0)->data.x
    /// Returns Some(IrConst::I64(offset)) if the expression matches the pattern.
    fn eval_offsetof_pattern(&self, expr: &Expr) -> Option<IrConst> {
        let (offset, _ty) = self.eval_offsetof_pattern_with_type(expr)?;
        Some(IrConst::I64(offset as i64))
    }

    /// Evaluate an offsetof sub-expression, returning both the accumulated byte offset
    /// and the CType of the resulting expression (needed for chained member access).
    fn eval_offsetof_pattern_with_type(&self, expr: &Expr) -> Option<(usize, CType)> {
        match expr {
            Expr::PointerMemberAccess(base, field_name, _) => {
                // base should be (type*)0 - a cast of 0 to a pointer type
                let (type_spec, base_offset) = self.extract_null_pointer_cast_with_offset(base)?;
                let layout = self.get_struct_layout_for_type(&type_spec)?;
                let (field_offset, field_ty) = layout.field_offset(field_name, &self.types)?;
                Some((base_offset + field_offset, field_ty))
            }
            Expr::MemberAccess(base, field_name, _) => {
                // First try: base is *((type*)0) (deref pattern)
                if let Expr::Deref(inner, _) = base.as_ref() {
                    let (type_spec, base_offset) = self.extract_null_pointer_cast_with_offset(inner)?;
                    let layout = self.get_struct_layout_for_type(&type_spec)?;
                    let (field_offset, field_ty) = layout.field_offset(field_name, &self.types)?;
                    return Some((base_offset + field_offset, field_ty));
                }
                // Second try: base is itself an offsetof sub-expression (chained access)
                // e.g., ((type*)0)->data.x where base = ((type*)0)->data
                let (base_offset, base_type) = self.eval_offsetof_pattern_with_type(base)?;
                let struct_key = match &base_type {
                    CType::Struct(key) | CType::Union(key) => key.clone(),
                    _ => return None,
                };
                let layout = self.types.struct_layouts.get(&*struct_key)?;
                let (field_offset, field_ty) = layout.field_offset(field_name, &self.types)?;
                Some((base_offset + field_offset, field_ty))
            }
            Expr::ArraySubscript(base, index, _) => {
                // Handle &((type*)0)->member[index] pattern
                let (base_offset, base_type) = self.eval_offsetof_pattern_with_type(base)?;
                let idx_val = self.eval_const_expr(index)?;
                let idx = idx_val.to_i64()?;
                let elem_size = match &base_type {
                    CType::Array(elem, _) => self.resolve_ctype_size(elem),
                    _ => return None,
                };
                let elem_ty = match &base_type {
                    CType::Array(elem, _) => (**elem).clone(),
                    _ => return None,
                };
                Some(((base_offset as i64 + idx * elem_size as i64) as usize, elem_ty))
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
                if let TypeSpecifier::Pointer(inner_ts, _) = type_spec {
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

    /// Cast a full 128-bit integer value to an IrType without going through u64 truncation.
    ///
    /// For targets <= 64 bits, extracts the lower bits. For 128-bit targets, preserves the
    /// full value. For float targets, uses value-based conversion from the full i128.
    fn cast_i128_to_ir_type(v128: i128, target: IrType, src_unsigned: bool) -> IrConst {
        let bits_lo = v128 as u64; // lower 64 bits
        match target {
            IrType::I8 => IrConst::I8(bits_lo as i8),
            IrType::U8 => IrConst::I64(bits_lo as u8 as i64),
            IrType::I16 => IrConst::I16(bits_lo as i16),
            IrType::U16 => IrConst::I64(bits_lo as u16 as i64),
            IrType::I32 => IrConst::I32(bits_lo as i32),
            IrType::U32 => IrConst::I64(bits_lo as u32 as i64),
            IrType::I64 | IrType::U64 | IrType::Ptr => IrConst::I64(bits_lo as i64),
            IrType::I128 | IrType::U128 => IrConst::I128(v128),
            IrType::F32 => {
                // int-to-float: signedness comes from the source type
                let fv = if src_unsigned { (v128 as u128) as f32 } else { v128 as f32 };
                IrConst::F32(fv)
            }
            IrType::F64 => {
                let fv = if src_unsigned { (v128 as u128) as f64 } else { v128 as f64 };
                IrConst::F64(fv)
            }
            IrType::F128 => {
                let fv = if src_unsigned { (v128 as u128) as f64 } else { v128 as f64 };
                IrConst::LongDouble(fv)
            }
            _ => IrConst::I128(v128), // fallback: preserve value
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
                    // &arr[i] or &arr[i][j][k] -> GlobalAddrOffset("arr", total_offset)
                    Expr::ArraySubscript(_, _, _) => {
                        self.resolve_chained_array_subscript(inner)
                    }
                    // &s.field or &s.a.b.c or &arr[i].field -> GlobalAddrOffset
                    Expr::MemberAccess(_, _, _) => {
                        self.resolve_chained_member_access(inner)
                    }
                    // &(base->field) where base is pointer arithmetic on a global array
                    // e.g., &((Upgrade_items + 1)->uaattrid)
                    Expr::PointerMemberAccess(base, field, _) => {
                        self.resolve_pointer_member_access_addr(base, field)
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
            // ((struct S) { func_ptr }) -> unwrap compound literal and try inner init
            // This handles cases like ((struct Wrap) {inc_global}) in global initializers
            // where the compound literal wraps a struct containing a function pointer.
            Expr::CompoundLiteral(_, ref init, _) => {
                self.eval_global_addr_from_initializer(init)
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

    /// Resolve a chained array subscript expression like `arr[i][j][k]` to a global
    /// address with computed offset. Walks nested ArraySubscript nodes from outermost
    /// to innermost, collecting (index, stride) pairs to compute the total byte offset.
    /// Handles 1D (`&arr[i]`), 2D (`&arr[i][j]`), and higher-dimensional arrays.
    fn resolve_chained_array_subscript(&self, expr: &Expr) -> Option<GlobalInit> {
        // Collect subscripts from outer to inner: arr[i][j] has outer=[j] inner=[i]
        let mut subscripts: Vec<&Expr> = Vec::new();
        let mut current = expr;
        loop {
            match current {
                Expr::ArraySubscript(base, index, _) => {
                    subscripts.push(index);
                    current = base.as_ref();
                }
                _ => break,
            }
        }

        // Try to resolve the base. It can be:
        // 1. A plain Identifier (global array)
        // 2. A Cast of a global address expression, e.g.:
        //    (const char *)boot_cpu_data.x86_capability
        //    which reinterprets the member as a different pointer/array type.
        match current {
            Expr::Identifier(name, _) => {
                let global_name = self.resolve_to_global_name(name)?;
                let ginfo = self.globals.get(&global_name)?;
                if !ginfo.is_array {
                    return None;
                }
                subscripts.reverse();

                let mut total_offset: i64 = 0;
                let strides = &ginfo.array_dim_strides;
                for (dim, idx_expr) in subscripts.iter().enumerate() {
                    let idx_val = self.eval_const_expr(idx_expr)?;
                    let idx = self.const_to_i64(&idx_val)?;
                    let stride = if !strides.is_empty() && dim < strides.len() {
                        strides[dim] as i64
                    } else if dim == 0 {
                        ginfo.elem_size as i64
                    } else {
                        return None;
                    };
                    total_offset += idx * stride;
                }

                if total_offset == 0 {
                    Some(GlobalInit::GlobalAddr(global_name))
                } else {
                    Some(GlobalInit::GlobalAddrOffset(global_name, total_offset))
                }
            }
            Expr::Cast(type_spec, inner, _) => {
                // Handle patterns like ((const char *)boot_cpu_data.x86_capability)[N]
                // The inner expression should resolve to a global address, and the cast's
                // pointee type determines the element size for subscript strides.
                self.resolve_cast_array_subscript(type_spec, inner, &subscripts)
            }
            _ => None,
        }
    }

    /// Resolve an array subscript where the base is a cast of a global address
    /// expression. For example: `((const char *)boot_cpu_data.x86_capability)[1]`
    /// resolves to `boot_cpu_data + offsetof(x86_capability) + 1 * sizeof(char)`.
    fn resolve_cast_array_subscript(
        &self,
        cast_type: &TypeSpecifier,
        inner_expr: &Expr,
        subscripts: &[&Expr],
    ) -> Option<GlobalInit> {
        // The cast type should be a pointer type; the pointee is the element type.
        let cast_ctype = self.type_spec_to_ctype(cast_type);
        let elem_size = match &cast_ctype {
            CType::Pointer(pointee, _) => self.ctype_size(pointee),
            // If it's an array type, use the element size
            CType::Array(elem, _) => self.ctype_size(elem),
            _ => return None,
        };
        if elem_size == 0 {
            return None;
        }

        // Resolve the inner expression as a global address.
        // This handles MemberAccess on globals (e.g., boot_cpu_data.x86_capability),
        // AddressOf patterns, identifiers, etc.
        let base_init = self.resolve_inner_as_global_addr(inner_expr)?;
        let (global_name, base_offset) = match &base_init {
            GlobalInit::GlobalAddr(name) => (name.clone(), 0i64),
            GlobalInit::GlobalAddrOffset(name, off) => (name.clone(), *off),
            _ => return None,
        };

        // Compute the total subscript offset. Only a single subscript dimension
        // makes sense here since the cast reinterprets the base as a flat pointer.
        // subscripts are in outer-to-inner order; reverse for left-to-right.
        let mut total_offset = base_offset;
        for idx_expr in subscripts.iter().rev() {
            let idx_val = self.eval_const_expr(idx_expr)?;
            let idx = self.const_to_i64(&idx_val)?;
            total_offset += idx * elem_size as i64;
        }

        if total_offset == 0 {
            Some(GlobalInit::GlobalAddr(global_name))
        } else {
            Some(GlobalInit::GlobalAddrOffset(global_name, total_offset))
        }
    }

    /// Resolve an expression to a global address, handling member access on global
    /// structs (e.g., `boot_cpu_data.x86_capability` -> GlobalAddrOffset("boot_cpu_data", 8)).
    /// This is used as the base of cast+subscript patterns in inline asm operands.
    fn resolve_inner_as_global_addr(&self, expr: &Expr) -> Option<GlobalInit> {
        match expr {
            // Direct global identifier - treat as address of the global
            Expr::Identifier(name, _) => {
                let global_name = self.resolve_to_global_name(name)?;
                Some(GlobalInit::GlobalAddr(global_name))
            }
            // struct_var.field -> global + field_offset
            Expr::MemberAccess(base, field, _) => {
                // Resolve the base to a global address
                let base_init = self.resolve_inner_as_global_addr(base)?;
                let (global_name, base_off) = match &base_init {
                    GlobalInit::GlobalAddr(name) => (name.clone(), 0i64),
                    GlobalInit::GlobalAddrOffset(name, off) => (name.clone(), *off),
                    _ => return None,
                };
                // Look up the struct layout to get the field offset
                let ginfo = self.globals.get(&global_name)?;
                let layout = ginfo.struct_layout.clone()?;
                let (field_offset, _field_ty) = layout.field_offset(field, &self.types)?;
                let total = base_off + field_offset as i64;
                if total == 0 {
                    Some(GlobalInit::GlobalAddr(global_name))
                } else {
                    Some(GlobalInit::GlobalAddrOffset(global_name, total))
                }
            }
            // AddressOf(&x) -> address of x
            Expr::AddressOf(_inner, _) => {
                self.eval_global_addr_expr(expr)
            }
            // Cast preserves the address
            Expr::Cast(_, inner, _) => {
                self.resolve_inner_as_global_addr(inner)
            }
            _ => None,
        }
    }

    /// Resolve a chained member access expression like `s.a.b.c` to a global address.
    /// Walks the chain from the root identifier, accumulating field offsets.
    /// Also handles `arr[i].field` and `(arr + i)->field` patterns.
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
                    let base_offset: i64 = 0;
                    let start_layout = ginfo.struct_layout.clone()?;
                    return self.apply_field_chain_offsets(&global_name, base_offset, &start_layout, &fields);
                }
                // Handle &arr[i].field - ArraySubscript as base of member chain
                Expr::ArraySubscript(base, index, _) => {
                    if let Expr::Identifier(name, _) = base.as_ref() {
                        let global_name = self.resolve_to_global_name(name)?;
                        let ginfo = self.globals.get(&global_name)?;
                        if ginfo.is_array {
                            let idx_val = self.eval_const_expr(index)?;
                            let idx = self.const_to_i64(&idx_val)?;
                            let base_offset = idx * ginfo.elem_size as i64;
                            // The element type should be a struct for member access
                            let start_layout = ginfo.struct_layout.clone()?;
                            return self.apply_field_chain_offsets(&global_name, base_offset, &start_layout, &fields);
                        }
                    }
                    return None;
                }
                _ => return None,
            }
        }
    }

    /// Apply a chain of field names to a base global+offset, accumulating field offsets.
    /// `fields` are in reverse order (outermost field last, innermost first).
    fn apply_field_chain_offsets(
        &self,
        global_name: &str,
        base_offset: i64,
        start_layout: &std::rc::Rc<StructLayout>,
        fields: &[String],
    ) -> Option<GlobalInit> {
        let mut total_offset = base_offset;
        let mut current_layout = start_layout.clone();
        // fields are in reverse order (outermost field last)
        for field_name in fields.iter().rev() {
            let mut found = false;
            // Try field_offset which handles anonymous structs/unions
            if let Some((foff, fty)) = current_layout.field_offset(field_name, &self.types) {
                total_offset += foff as i64;
                current_layout = match &fty {
                    CType::Struct(key) | CType::Union(key) => {
                        self.types.struct_layouts.get(&**key).cloned()
                            .unwrap_or_else(StructLayout::empty_rc)
                    }
                    _ => StructLayout::empty_rc(),
                };
                found = true;
            }
            if !found {
                return None;
            }
        }
        if total_offset == 0 {
            Some(GlobalInit::GlobalAddr(global_name.to_string()))
        } else {
            Some(GlobalInit::GlobalAddrOffset(global_name.to_string(), total_offset))
        }
    }

    /// Resolve &(base->field) where base is a constant pointer expression
    /// involving a global array (e.g., &((Upgrade_items + 1)->uaattrid)).
    /// The base must resolve to a global address (possibly with offset).
    fn resolve_pointer_member_access_addr(&self, base: &Expr, field: &str) -> Option<GlobalInit> {
        // The base expression should be a pointer to a global (array element).
        // Try to evaluate it as a global address expression.
        let base_init = self.eval_global_addr_expr(base)?;
        let (global_name, base_offset) = match &base_init {
            GlobalInit::GlobalAddr(name) => (name.clone(), 0i64),
            GlobalInit::GlobalAddrOffset(name, off) => (name.clone(), *off),
            _ => return None,
        };
        // Get the struct layout for the element type.
        // The global should be an array of structs.
        let ginfo = self.globals.get(&global_name)?;
        let layout = ginfo.struct_layout.clone()?;
        let (field_offset, _field_ty) = layout.field_offset(field, &self.types)?;
        let total_offset = base_offset + field_offset as i64;
        if total_offset == 0 {
            Some(GlobalInit::GlobalAddr(global_name))
        } else {
            Some(GlobalInit::GlobalAddrOffset(global_name, total_offset))
        }
    }

    /// Evaluate a constant binary operation.
    /// Uses both operand types for C's usual arithmetic conversions (C11 6.3.1.8),
    /// except for shifts where only the LHS type determines the result type (C11 6.5.7).
    /// Delegates arithmetic to the shared implementation in `common::const_arith`.
    fn eval_const_binop(&self, op: &BinOp, lhs: &IrConst, rhs: &IrConst, lhs_ty: IrType, rhs_ty: IrType) -> Option<IrConst> {
        let lhs_size = lhs_ty.size().max(4);
        let is_shift = matches!(op, BinOp::Shl | BinOp::Shr);

        // For shifts (C11 6.5.7): result type is the promoted LHS type only.
        // For other ops: apply usual arithmetic conversions using both operand types.
        let (is_32bit, is_unsigned) = if is_shift {
            (lhs_size <= 4, lhs_ty.is_unsigned())
        } else {
            let rhs_size = rhs_ty.size().max(4);
            let result_size = lhs_size.max(rhs_size);
            let is_unsigned = if lhs_size == rhs_size {
                lhs_ty.is_unsigned() || rhs_ty.is_unsigned()
            } else if lhs_size > rhs_size {
                lhs_ty.is_unsigned()
            } else {
                rhs_ty.is_unsigned()
            };
            (result_size <= 4, is_unsigned)
        };
        const_arith::eval_const_binop(op, lhs, rhs, is_32bit, is_unsigned)
    }

    /// Promote a sub-int constant (I8/I16) to I32 for unary arithmetic,
    /// using unsigned zero-extension when the expression has an unsigned type.
    /// C11 6.3.1.1: unsigned char/short promote to int by zero-extending.
    fn promote_const_for_unary(&self, expr: &Expr, val: IrConst) -> IrConst {
        match &val {
            IrConst::I8(v) => {
                let is_unsigned = self.is_expr_unsigned_for_const(expr);
                if is_unsigned {
                    IrConst::I32(*v as u8 as i32)
                } else {
                    IrConst::I32(*v as i32)
                }
            }
            IrConst::I16(v) => {
                let is_unsigned = self.is_expr_unsigned_for_const(expr);
                if is_unsigned {
                    IrConst::I32(*v as u16 as i32)
                } else {
                    IrConst::I32(*v as i32)
                }
            }
            _ => val,
        }
    }

    /// Check if an expression has an unsigned type for constant evaluation.
    fn is_expr_unsigned_for_const(&self, expr: &Expr) -> bool {
        if let Expr::Cast(ref target_type, _, _) = expr {
            let ty = self.type_spec_to_ir(target_type);
            return ty.is_unsigned();
        }
        let ty = self.infer_expr_type(expr);
        ty.is_unsigned()
    }

    /// Try to constant-fold a binary operation from its parts.
    /// Used by lower_binary_op to avoid generating IR for constant expressions,
    /// ensuring correct C type semantics (especially 32-bit vs 64-bit width).
    pub(super) fn eval_const_expr_from_parts(&self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Option<IrConst> {
        let l = self.eval_const_expr(lhs)?;
        let r = self.eval_const_expr(rhs)?;
        let lhs_ty = self.infer_expr_type(lhs);
        let rhs_ty = self.infer_expr_type(rhs);
        let result = self.eval_const_binop(op, &l, &r, lhs_ty, rhs_ty)?;
        // Convert to I64 for IR operand compatibility (IR uses I64 for all int operations).
        Some(match result {
            IrConst::I32(v) => IrConst::I64(v as i64),
            IrConst::I8(v) => IrConst::I64(v as i64),
            IrConst::I16(v) => IrConst::I64(v as i64),
            other => other,
        })
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
            if let Initializer::Expr(Expr::WideStringLiteral(s, _) | Expr::Char16StringLiteral(s, _)) = &items[0].init {
                return s.chars().count() + 1; // wide/char16 string to char array
            }
        }
        // Special case: wchar_t w[] = {L"hello"} - single brace-wrapped wide string literal
        if (base_ty == IrType::I32 || base_ty == IrType::U32)
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
        // Special case: char16_t c[] = {u"hello"} - single brace-wrapped char16_t string literal
        if (base_ty == IrType::I16 || base_ty == IrType::U16)
            && items.len() == 1
            && items[0].designators.is_empty()
        {
            if let Initializer::Expr(Expr::Char16StringLiteral(s, _)) = &items[0].init {
                return s.chars().count() + 1; // +1 for null terminator (count in char16_t elements)
            }
            if let Initializer::Expr(Expr::WideStringLiteral(s, _) | Expr::StringLiteral(s, _)) = &items[0].init {
                return s.chars().count() + 1; // string to char16_t array
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
                if let Some(layout) = self.types.struct_layouts.get(&**key) {
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
            Initializer::Expr(Expr::StringLiteral(_, _) | Expr::WideStringLiteral(_, _)
                | Expr::Char16StringLiteral(_, _))
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

