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
        BinOp::Eq => bool_to_i64(l == r),
        BinOp::Ne => bool_to_i64(l != r),
        BinOp::Lt => {
            if is_unsigned { bool_to_i64((l as u64) < (r as u64)) }
            else { bool_to_i64(l < r) }
        }
        BinOp::Gt => {
            if is_unsigned { bool_to_i64((l as u64) > (r as u64)) }
            else { bool_to_i64(l > r) }
        }
        BinOp::Le => {
            if is_unsigned { bool_to_i64((l as u64) <= (r as u64)) }
            else { bool_to_i64(l <= r) }
        }
        BinOp::Ge => {
            if is_unsigned { bool_to_i64((l as u64) >= (r as u64)) }
            else { bool_to_i64(l >= r) }
        }
        BinOp::LogicalAnd => bool_to_i64(l != 0 && r != 0),
        BinOp::LogicalOr => bool_to_i64(l != 0 || r != 0),
    };
    Some(IrConst::I64(result))
}

/// Evaluate a constant floating-point binary operation.
///
/// Promotes both operands to f64 for computation, then wraps the result in
/// the appropriate float variant: LongDouble if either operand is LongDouble,
/// F32 if both are F32, otherwise F64.
///
/// Comparison and logical operations always return `IrConst::I64`.
pub fn eval_const_binop_float(op: &BinOp, lhs: &IrConst, rhs: &IrConst) -> Option<IrConst> {
    let use_long_double = matches!(lhs, IrConst::LongDouble(_)) || matches!(rhs, IrConst::LongDouble(_));
    let use_f32 = matches!(lhs, IrConst::F32(_)) && matches!(rhs, IrConst::F32(_));

    let l = lhs.to_f64()?;
    let r = rhs.to_f64()?;

    let make_float = |v: f64| -> IrConst {
        if use_long_double {
            IrConst::LongDouble(v)
        } else if use_f32 {
            IrConst::F32(v as f32)
        } else {
            IrConst::F64(v)
        }
    };

    match op {
        BinOp::Add => Some(make_float(l + r)),
        BinOp::Sub => Some(make_float(l - r)),
        BinOp::Mul => Some(make_float(l * r)),
        BinOp::Div => Some(make_float(l / r)), // IEEE 754: div by zero -> inf/NaN
        BinOp::Eq => Some(IrConst::I64(if l == r { 1 } else { 0 })),
        BinOp::Ne => Some(IrConst::I64(if l != r { 1 } else { 0 })),
        BinOp::Lt => Some(IrConst::I64(if l < r { 1 } else { 0 })),
        BinOp::Gt => Some(IrConst::I64(if l > r { 1 } else { 0 })),
        BinOp::Le => Some(IrConst::I64(if l <= r { 1 } else { 0 })),
        BinOp::Ge => Some(IrConst::I64(if l >= r { 1 } else { 0 })),
        BinOp::LogicalAnd => Some(IrConst::I64(if l != 0.0 && r != 0.0 { 1 } else { 0 })),
        BinOp::LogicalOr => Some(IrConst::I64(if l != 0.0 || r != 0.0 { 1 } else { 0 })),
        _ => None, // Bitwise/shift not valid on floats
    }
}

/// Evaluate a constant binary operation, dispatching to int or float as needed.
///
/// This is the top-level entry point for constant binary evaluation.
/// The caller provides `is_32bit` and `is_unsigned` for the integer path.
pub fn eval_const_binop(op: &BinOp, lhs: &IrConst, rhs: &IrConst, is_32bit: bool, is_unsigned: bool) -> Option<IrConst> {
    let lhs_is_float = matches!(lhs, IrConst::F32(_) | IrConst::F64(_) | IrConst::LongDouble(_));
    let rhs_is_float = matches!(rhs, IrConst::F32(_) | IrConst::F64(_) | IrConst::LongDouble(_));

    if lhs_is_float || rhs_is_float {
        return eval_const_binop_float(op, lhs, rhs);
    }

    let l = lhs.to_i64()?;
    let r = rhs.to_i64()?;
    eval_const_binop_int(op, l, r, is_32bit, is_unsigned)
}

/// Negate a constant value (unary `-`).
/// Sub-int types are promoted to i32 per C integer promotion rules.
pub fn negate_const(val: IrConst) -> Option<IrConst> {
    match val {
        IrConst::I64(v) => Some(IrConst::I64(-v)),
        IrConst::I32(v) => Some(IrConst::I32(-v)),
        IrConst::I8(v) => Some(IrConst::I32(-(v as i32))),
        IrConst::I16(v) => Some(IrConst::I32(-(v as i32))),
        IrConst::F64(v) => Some(IrConst::F64(-v)),
        IrConst::F32(v) => Some(IrConst::F32(-v)),
        IrConst::LongDouble(v) => Some(IrConst::LongDouble(-v)),
        _ => None,
    }
}

/// Bitwise NOT of a constant value (unary `~`).
/// Sub-int types are promoted to i32 per C integer promotion rules.
pub fn bitnot_const(val: IrConst) -> Option<IrConst> {
    match val {
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
