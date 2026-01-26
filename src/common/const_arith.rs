/// Shared constant-expression arithmetic helpers.
///
/// Used by both `sema::const_eval` and `ir::lowering::const_eval` for
/// compile-time constant expression evaluation with proper C semantics.
///
/// The functions here handle the pure arithmetic: given IrConst operands and
/// width/signedness parameters, they compute the result. The callers (sema and
/// lowering) are responsible for determining width/signedness from their own
/// type systems (CType vs IrType) before calling these shared functions.

use crate::ir::ir::IrConst;
use crate::frontend::parser::ast::BinOp;

// === Low-level arithmetic primitives ===

/// Wrap an i64 result to 32-bit width if `is_32bit` is true, otherwise return as-is.
/// This handles the C semantics of truncating arithmetic results to `int` width.
#[inline]
pub fn wrap_result(v: i64, is_32bit: bool) -> i64 {
    if is_32bit { v as i32 as i64 } else { v }
}

/// Perform an unsigned binary operation, handling 32-bit vs 64-bit width.
/// Converts operands to the appropriate unsigned type, applies the operation,
/// and sign-extends the result back to i64.
#[inline]
pub fn unsigned_op(l: i64, r: i64, is_32bit: bool, op: fn(u64, u64) -> u64) -> i64 {
    if is_32bit {
        op(l as u32 as u64, r as u32 as u64) as u32 as i64
    } else {
        op(l as u64, r as u64) as i64
    }
}

/// Convert a boolean to i64 (1 for true, 0 for false).
#[inline]
pub fn bool_to_i64(b: bool) -> i64 {
    if b { 1 } else { 0 }
}

// === Shared constant binary operation evaluators ===
//
// These replace the near-identical eval_const_binop / eval_const_binop_float
// implementations that were duplicated between sema::const_eval and
// ir::lowering::const_eval. Both callers now delegate here.

/// Evaluate a constant integer binary operation with proper C width/signedness.
///
/// The caller determines `is_32bit` and `is_unsigned` from their type system:
/// - sema uses CType: `(ctype_size <= 4, ctype.is_unsigned())`
/// - lowering uses IrType: `(ir_type.size() <= 4, ir_type.is_unsigned())`
///
/// Returns `Some(IrConst::I64(result))` or `None` for division by zero.
pub fn eval_const_binop_int(op: &BinOp, l: i64, r: i64, is_32bit: bool, is_unsigned: bool) -> Option<IrConst> {
    let result = match op {
        BinOp::Add => wrap_result(l.wrapping_add(r), is_32bit),
        BinOp::Sub => wrap_result(l.wrapping_sub(r), is_32bit),
        BinOp::Mul => wrap_result(l.wrapping_mul(r), is_32bit),
        BinOp::Div => {
            if r == 0 { return None; }
            if is_unsigned {
                unsigned_op(l, r, is_32bit, u64::wrapping_div)
            } else {
                wrap_result(l.wrapping_div(r), is_32bit)
            }
        }
        BinOp::Mod => {
            if r == 0 { return None; }
            if is_unsigned {
                unsigned_op(l, r, is_32bit, u64::wrapping_rem)
            } else {
                wrap_result(l.wrapping_rem(r), is_32bit)
            }
        }
        BinOp::BitAnd => l & r,
        BinOp::BitOr => l | r,
        BinOp::BitXor => l ^ r,
        BinOp::Shl => wrap_result(l.wrapping_shl(r as u32), is_32bit),
        BinOp::Shr => {
            if is_unsigned {
                unsigned_op(l, r, is_32bit, |a, b| a.wrapping_shr(b as u32))
            } else if is_32bit {
                (l as i32).wrapping_shr(r as u32) as i64
            } else {
                l.wrapping_shr(r as u32)
            }
        }
        BinOp::Eq => {
            if is_32bit { bool_to_i64(l as u32 == r as u32) }
            else if is_unsigned { bool_to_i64(l as u64 == r as u64) }
            else { bool_to_i64(l == r) }
        }
        BinOp::Ne => {
            if is_32bit { bool_to_i64(l as u32 != r as u32) }
            else if is_unsigned { bool_to_i64(l as u64 != r as u64) }
            else { bool_to_i64(l != r) }
        }
        BinOp::Lt => {
            if is_32bit { if is_unsigned { bool_to_i64((l as u32) < (r as u32)) } else { bool_to_i64((l as i32) < (r as i32)) } }
            else if is_unsigned { bool_to_i64((l as u64) < (r as u64)) }
            else { bool_to_i64(l < r) }
        }
        BinOp::Gt => {
            if is_32bit { if is_unsigned { bool_to_i64((l as u32) > (r as u32)) } else { bool_to_i64((l as i32) > (r as i32)) } }
            else if is_unsigned { bool_to_i64((l as u64) > (r as u64)) }
            else { bool_to_i64(l > r) }
        }
        BinOp::Le => {
            if is_32bit { if is_unsigned { bool_to_i64((l as u32) <= (r as u32)) } else { bool_to_i64((l as i32) <= (r as i32)) } }
            else if is_unsigned { bool_to_i64((l as u64) <= (r as u64)) }
            else { bool_to_i64(l <= r) }
        }
        BinOp::Ge => {
            if is_32bit { if is_unsigned { bool_to_i64((l as u32) >= (r as u32)) } else { bool_to_i64((l as i32) >= (r as i32)) } }
            else if is_unsigned { bool_to_i64((l as u64) >= (r as u64)) }
            else { bool_to_i64(l >= r) }
        }
        BinOp::LogicalAnd => bool_to_i64(l != 0 && r != 0),
        BinOp::LogicalOr => bool_to_i64(l != 0 || r != 0),
    };
    // Preserve the result width so that downstream operations (e.g., division
    // using the result of a shift) can correctly infer the C type. This is
    // critical for expressions like (1 << 31) / N where the shift result must
    // be recognized as 32-bit (INT_MIN = -2147483648) not 64-bit (positive).
    //
    // For unsigned 32-bit results, we must use I64 with zero-extension because
    // IrConst::I32 is signed and cannot correctly represent unsigned values >= 2^31.
    // E.g., (2147483647 * 2U + 1U) = 4294967295U would be stored as I32(-1),
    // which sign-extends to -1 (0xFFFFFFFFFFFFFFFF) when widened to 64-bit.
    // Using I64(4294967295) preserves the correct unsigned value.
    if is_32bit {
        if is_unsigned {
            Some(IrConst::I64(result as u32 as i64))
        } else {
            Some(IrConst::I32(result as i32))
        }
    } else {
        Some(IrConst::I64(result))
    }
}

/// Evaluate a constant floating-point binary operation.
///
/// For LongDouble operands, uses full x87 80-bit precision arithmetic via
/// inline assembly (on x86-64 hosts) to avoid precision loss from f64 truncation.
/// For F32/F64, uses native Rust arithmetic.
///
/// Comparison and logical operations always return `IrConst::I64`.
pub fn eval_const_binop_float(op: &BinOp, lhs: &IrConst, rhs: &IrConst) -> Option<IrConst> {
    use crate::common::long_double;

    let use_long_double = matches!(lhs, IrConst::LongDouble(..)) || matches!(rhs, IrConst::LongDouble(..));
    let use_f32 = matches!(lhs, IrConst::F32(_)) && matches!(rhs, IrConst::F32(_));

    // For long double arithmetic, use full x87 precision on the raw bytes
    if use_long_double {
        let la = lhs.x87_bytes();
        let ra = rhs.x87_bytes();

        match op {
            BinOp::Add => {
                let result_bytes = long_double::x87_add(&la, &ra);
                let approx = long_double::x87_to_f64(&result_bytes);
                Some(IrConst::long_double_with_bytes(approx, result_bytes))
            }
            BinOp::Sub => {
                let result_bytes = long_double::x87_sub(&la, &ra);
                let approx = long_double::x87_to_f64(&result_bytes);
                Some(IrConst::long_double_with_bytes(approx, result_bytes))
            }
            BinOp::Mul => {
                let result_bytes = long_double::x87_mul(&la, &ra);
                let approx = long_double::x87_to_f64(&result_bytes);
                Some(IrConst::long_double_with_bytes(approx, result_bytes))
            }
            BinOp::Div => {
                let result_bytes = long_double::x87_div(&la, &ra);
                let approx = long_double::x87_to_f64(&result_bytes);
                Some(IrConst::long_double_with_bytes(approx, result_bytes))
            }
            BinOp::Mod => {
                let result_bytes = long_double::x87_rem(&la, &ra);
                let approx = long_double::x87_to_f64(&result_bytes);
                Some(IrConst::long_double_with_bytes(approx, result_bytes))
            }
            // Comparisons use x87 compare for full precision
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
                let cmp = long_double::x87_cmp(&la, &ra);
                let result = match op {
                    BinOp::Eq => cmp == 0,
                    BinOp::Ne => cmp != 0,
                    BinOp::Lt => cmp == -1,
                    BinOp::Gt => cmp == 1,
                    BinOp::Le => cmp == -1 || cmp == 0,
                    BinOp::Ge => cmp == 1 || cmp == 0,
                    _ => unreachable!(),
                };
                Some(IrConst::I64(if result { 1 } else { 0 }))
            }
            BinOp::LogicalAnd | BinOp::LogicalOr => {
                // For logical ops, check if values are nonzero using x87 compare with zero
                let zero = [0u8; 16];
                let l_nonzero = long_double::x87_cmp(&la, &zero) != 0;
                let r_nonzero = long_double::x87_cmp(&ra, &zero) != 0;
                let result = match op {
                    BinOp::LogicalAnd => l_nonzero && r_nonzero,
                    BinOp::LogicalOr => l_nonzero || r_nonzero,
                    _ => unreachable!(),
                };
                Some(IrConst::I64(if result { 1 } else { 0 }))
            }
            _ => None,
        }
    } else {
        // F32/F64 path: use native Rust arithmetic
        let l = lhs.to_f64()?;
        let r = rhs.to_f64()?;

        let make_float = |v: f64| -> IrConst {
            if use_f32 {
                IrConst::F32(v as f32)
            } else {
                IrConst::F64(v)
            }
        };

        match op {
            BinOp::Add => Some(make_float(l + r)),
            BinOp::Sub => Some(make_float(l - r)),
            BinOp::Mul => Some(make_float(l * r)),
            BinOp::Div => Some(make_float(l / r)),
            BinOp::Eq => Some(IrConst::I64(if l == r { 1 } else { 0 })),
            BinOp::Ne => Some(IrConst::I64(if l != r { 1 } else { 0 })),
            BinOp::Lt => Some(IrConst::I64(if l < r { 1 } else { 0 })),
            BinOp::Gt => Some(IrConst::I64(if l > r { 1 } else { 0 })),
            BinOp::Le => Some(IrConst::I64(if l <= r { 1 } else { 0 })),
            BinOp::Ge => Some(IrConst::I64(if l >= r { 1 } else { 0 })),
            BinOp::LogicalAnd => Some(IrConst::I64(if l != 0.0 && r != 0.0 { 1 } else { 0 })),
            BinOp::LogicalOr => Some(IrConst::I64(if l != 0.0 || r != 0.0 { 1 } else { 0 })),
            _ => None,
        }
    }
}

/// Evaluate a constant 128-bit integer binary operation.
///
/// Called when at least one operand is I128. Uses native Rust i128/u128 arithmetic
/// to avoid the truncation that occurs when using the i64 path.
pub fn eval_const_binop_i128(op: &BinOp, lhs: &IrConst, rhs: &IrConst, is_unsigned: bool) -> Option<IrConst> {
    // When the operation is unsigned, operands that are smaller than 128-bit
    // must be zero-extended (not sign-extended) to 128 bits.
    // E.g., u64 0xCAFEBABE12345678 should become u128 0x00000000_00000000_CAFEBABE12345678,
    // not 0xFFFFFFFF_FFFFFFFF_CAFEBABE12345678 (which is what sign-extension gives).
    let l = if is_unsigned && !matches!(lhs, IrConst::I128(_)) {
        // Zero-extend: treat the i64 bit pattern as u64, then widen to u128
        (lhs.to_i64()? as u64 as u128) as i128
    } else {
        lhs.to_i128()?
    };
    let r = if is_unsigned && !matches!(rhs, IrConst::I128(_)) {
        (rhs.to_i64()? as u64 as u128) as i128
    } else {
        rhs.to_i128()?
    };

    let bool_result = |b: bool| -> Option<IrConst> {
        Some(IrConst::I64(if b { 1 } else { 0 }))
    };

    match op {
        BinOp::Add => Some(IrConst::I128(l.wrapping_add(r))),
        BinOp::Sub => Some(IrConst::I128(l.wrapping_sub(r))),
        BinOp::Mul => Some(IrConst::I128(l.wrapping_mul(r))),
        BinOp::Div => {
            if r == 0 { return None; }
            if is_unsigned {
                Some(IrConst::I128((l as u128).wrapping_div(r as u128) as i128))
            } else {
                Some(IrConst::I128(l.wrapping_div(r)))
            }
        }
        BinOp::Mod => {
            if r == 0 { return None; }
            if is_unsigned {
                Some(IrConst::I128((l as u128).wrapping_rem(r as u128) as i128))
            } else {
                Some(IrConst::I128(l.wrapping_rem(r)))
            }
        }
        BinOp::BitAnd => Some(IrConst::I128(l & r)),
        BinOp::BitOr => Some(IrConst::I128(l | r)),
        BinOp::BitXor => Some(IrConst::I128(l ^ r)),
        BinOp::Shl => Some(IrConst::I128(l.wrapping_shl(r as u32))),
        BinOp::Shr => {
            if is_unsigned {
                Some(IrConst::I128((l as u128).wrapping_shr(r as u32) as i128))
            } else {
                Some(IrConst::I128(l.wrapping_shr(r as u32)))
            }
        }
        BinOp::Eq => bool_result(l == r),
        BinOp::Ne => bool_result(l != r),
        BinOp::Lt => {
            if is_unsigned { bool_result((l as u128) < (r as u128)) }
            else { bool_result(l < r) }
        }
        BinOp::Gt => {
            if is_unsigned { bool_result((l as u128) > (r as u128)) }
            else { bool_result(l > r) }
        }
        BinOp::Le => {
            if is_unsigned { bool_result((l as u128) <= (r as u128)) }
            else { bool_result(l <= r) }
        }
        BinOp::Ge => {
            if is_unsigned { bool_result((l as u128) >= (r as u128)) }
            else { bool_result(l >= r) }
        }
        BinOp::LogicalAnd => bool_result(l != 0 && r != 0),
        BinOp::LogicalOr => bool_result(l != 0 || r != 0),
    }
}

/// Evaluate a constant binary operation, dispatching to int, i128, or float as needed.
///
/// This is the top-level entry point for constant binary evaluation.
/// The caller provides `is_32bit` and `is_unsigned` for the integer path.
pub fn eval_const_binop(op: &BinOp, lhs: &IrConst, rhs: &IrConst, is_32bit: bool, is_unsigned: bool) -> Option<IrConst> {
    let lhs_is_float = matches!(lhs, IrConst::F32(_) | IrConst::F64(_) | IrConst::LongDouble(..));
    let rhs_is_float = matches!(rhs, IrConst::F32(_) | IrConst::F64(_) | IrConst::LongDouble(..));

    if lhs_is_float || rhs_is_float {
        return eval_const_binop_float(op, lhs, rhs);
    }

    // Use native i128 arithmetic when either operand is I128 to avoid truncation.
    if matches!(lhs, IrConst::I128(_)) || matches!(rhs, IrConst::I128(_)) {
        return eval_const_binop_i128(op, lhs, rhs, is_unsigned);
    }

    let l = lhs.to_i64()?;
    let r = rhs.to_i64()?;
    eval_const_binop_int(op, l, r, is_32bit, is_unsigned)
}

/// Negate a constant value (unary `-`).
/// Sub-int types are promoted to i32 per C integer promotion rules.
/// Uses wrapping negation to handle MIN values (e.g. -(-2^63) wraps to -2^63 in C).
pub fn negate_const(val: IrConst) -> Option<IrConst> {
    match val {
        IrConst::I128(v) => Some(IrConst::I128(v.wrapping_neg())),
        IrConst::I64(v) => Some(IrConst::I64(v.wrapping_neg())),
        IrConst::I32(v) => Some(IrConst::I32(v.wrapping_neg())),
        IrConst::I8(v) => Some(IrConst::I32((v as i32).wrapping_neg())),
        IrConst::I16(v) => Some(IrConst::I32((v as i32).wrapping_neg())),
        IrConst::F64(v) => Some(IrConst::F64(-v)),
        IrConst::F32(v) => Some(IrConst::F32(-v)),
        IrConst::LongDouble(v, bytes) => {
            // Negate by flipping the sign bit in the x87 bytes directly,
            // preserving full 80-bit precision
            let neg_bytes = crate::common::long_double::x87_neg(&bytes);
            Some(IrConst::long_double_with_bytes(-v, neg_bytes))
        }
        _ => None,
    }
}

/// Bitwise NOT of a constant value (unary `~`).
/// Sub-int types are promoted to i32 per C integer promotion rules.
pub fn bitnot_const(val: IrConst) -> Option<IrConst> {
    match val {
        IrConst::I128(v) => Some(IrConst::I128(!v)),
        IrConst::I64(v) => Some(IrConst::I64(!v)),
        IrConst::I32(v) => Some(IrConst::I32(!v)),
        IrConst::I8(v) => Some(IrConst::I32(!(v as i32))),
        IrConst::I16(v) => Some(IrConst::I32(!(v as i32))),
        _ => None,
    }
}

/// Check if an AST expression is a zero literal (0 or cast of 0).
/// Used for offsetof pattern detection: `&((type*)0)->member`.
pub fn is_zero_expr(expr: &crate::frontend::parser::ast::Expr) -> bool {
    use crate::frontend::parser::ast::Expr;
    match expr {
        Expr::IntLiteral(0, _) | Expr::UIntLiteral(0, _)
        | Expr::LongLiteral(0, _) | Expr::ULongLiteral(0, _) => true,
        Expr::Cast(_, inner, _) => is_zero_expr(inner),
        _ => false,
    }
}

/// Evaluate raw u64 bit truncation and sign extension for a cast chain.
///
/// Given raw bits from a source value, truncates to `target_width` bits
/// and optionally sign-extends back to 64 bits. Returns `(result_bits, target_signed)`.
///
/// The caller determines `target_width` and `target_signed` from their type system.
pub fn truncate_and_extend_bits(bits: u64, target_width: usize, target_signed: bool) -> (u64, bool) {
    // Truncate to target width
    let truncated = if target_width >= 64 || target_width == 0 {
        bits
    } else {
        bits & ((1u64 << target_width) - 1)
    };

    // If target is signed, sign-extend to 64 bits
    let result = if target_signed && target_width < 64 && target_width > 0 {
        let sign_bit = 1u64 << (target_width - 1);
        if truncated & sign_bit != 0 {
            truncated | !((1u64 << target_width) - 1)
        } else {
            truncated
        }
    } else {
        truncated
    };

    (result, target_signed)
}
