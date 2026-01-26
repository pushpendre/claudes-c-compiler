//! Constant folding optimization pass.
//!
//! This pass evaluates operations on constant operands at compile time,
//! replacing the instruction with the computed constant. This eliminates
//! redundant computation and enables further optimizations (DCE, etc.).

use crate::ir::ir::*;
use crate::common::types::IrType;

/// Run constant folding on the entire module.
/// Returns the number of instructions folded.
pub fn run(module: &mut IrModule) -> usize {
    module.for_each_function(fold_function)
}

/// Fold constants within a single function.
/// Iterates until no more folding is possible (fixpoint).
fn fold_function(func: &mut IrFunction) -> usize {
    let mut total = 0;
    loop {
        let folded = fold_function_once(func);
        if folded == 0 {
            break;
        }
        total += folded;
    }
    total
}

/// Single pass of constant folding over a function.
/// Edits instructions in-place to avoid allocating a new Vec per block.
fn fold_function_once(func: &mut IrFunction) -> usize {
    let mut folded = 0;

    for block in &mut func.blocks {
        for inst in &mut block.instructions {
            if let Some(folded_inst) = try_fold(inst) {
                *inst = folded_inst;
                folded += 1;
            }
        }
    }

    folded
}

/// Try to fold a single instruction. Returns Some(replacement) if foldable.
fn try_fold(inst: &Instruction) -> Option<Instruction> {
    match inst {
        Instruction::BinOp { dest, op, lhs, rhs, ty } => {
            // Skip 128-bit types: our fold_binop uses i64, which would truncate
            if ty.is_128bit() {
                return None;
            }
            // Try float folding first
            if ty.is_float() {
                let l = as_f64_const(lhs)?;
                let r = as_f64_const(rhs)?;
                let result = fold_float_binop(*op, l, r)?;
                return Some(Instruction::Copy {
                    dest: *dest,
                    src: Operand::Const(make_float_const(result, *ty)),
                });
            }
            let lhs_const = as_i64_const(lhs)?;
            let rhs_const = as_i64_const(rhs)?;
            let result = fold_binop(*op, lhs_const, rhs_const)?;
            Some(Instruction::Copy {
                dest: *dest,
                src: Operand::Const(IrConst::from_i64(result, *ty)),
            })
        }
        Instruction::UnaryOp { dest, op, src, ty } => {
            // Skip 128-bit types
            if ty.is_128bit() {
                return None;
            }
            // Try float unary folding
            if ty.is_float() {
                let s = as_f64_const(src)?;
                let result = fold_float_unaryop(*op, s)?;
                return Some(Instruction::Copy {
                    dest: *dest,
                    src: Operand::Const(make_float_const(result, *ty)),
                });
            }
            let src_const = as_i64_const(src)?;
            let result = fold_unaryop(*op, src_const, *ty)?;
            Some(Instruction::Copy {
                dest: *dest,
                src: Operand::Const(IrConst::from_i64(result, *ty)),
            })
        }
        Instruction::Cmp { dest, op, lhs, rhs, ty } => {
            // Skip 128-bit comparison types
            if ty.is_128bit() {
                return None;
            }
            // Try float comparison folding
            if ty.is_float() {
                let l = as_f64_const(lhs)?;
                let r = as_f64_const(rhs)?;
                let result = fold_float_cmp(*op, l, r);
                return Some(Instruction::Copy {
                    dest: *dest,
                    src: Operand::Const(IrConst::I32(result as i32)),
                });
            }
            let lhs_const = as_i64_const(lhs)?;
            let rhs_const = as_i64_const(rhs)?;
            let result = fold_cmp(*op, lhs_const, rhs_const);
            Some(Instruction::Copy {
                dest: *dest,
                src: Operand::Const(IrConst::I32(result as i32)),
            })
        }
        Instruction::Cast { dest, src, from_ty, to_ty } => {
            // Skip casts involving 128-bit types
            if from_ty.is_128bit() || to_ty.is_128bit() {
                return None;
            }
            // Handle float-to-int and int-to-float casts
            if from_ty.is_float() || to_ty.is_float() {
                return try_fold_float_cast(*dest, src, *from_ty, *to_ty);
            }
            let src_const = as_i64_const(src)?;
            let result = fold_cast(src_const, *from_ty, *to_ty);
            Some(Instruction::Copy {
                dest: *dest,
                src: Operand::Const(IrConst::from_i64(result, *to_ty)),
            })
        }
        Instruction::Select { dest, cond, true_val, false_val, .. } => {
            // If the condition is a known constant, fold to the appropriate value
            let cond_const = as_i64_const(cond)?;
            let result = if cond_const != 0 { *true_val } else { *false_val };
            Some(Instruction::Copy {
                dest: *dest,
                src: result,
            })
        }
        _ => None,
    }
}

/// Extract a constant integer value from an operand.
fn as_i64_const(op: &Operand) -> Option<i64> {
    match op {
        Operand::Const(c) => c.to_i64(),
        Operand::Value(_) => None,
    }
}

/// Extract a constant floating-point value from an operand.
fn as_f64_const(op: &Operand) -> Option<f64> {
    match op {
        Operand::Const(IrConst::F32(v)) => Some(*v as f64),
        Operand::Const(IrConst::F64(v)) => Some(*v),
        Operand::Const(IrConst::LongDouble(v)) => Some(*v),
        _ => None,
    }
}

/// Create a float constant of the appropriate type from an f64 value.
fn make_float_const(val: f64, ty: IrType) -> IrConst {
    match ty {
        IrType::F32 => IrConst::F32(val as f32),
        IrType::F64 => IrConst::F64(val),
        IrType::F128 => IrConst::LongDouble(val),
        _ => unreachable!("make_float_const called with non-float type"),
    }
}

/// Evaluate a binary operation on two constant floats.
/// Uses Rust's native f64 arithmetic which is IEEE 754 compliant.
fn fold_float_binop(op: IrBinOp, lhs: f64, rhs: f64) -> Option<f64> {
    Some(match op {
        IrBinOp::Add => lhs + rhs,
        IrBinOp::Sub => lhs - rhs,
        IrBinOp::Mul => lhs * rhs,
        // For division, allow folding even for division by zero (produces Inf/-Inf/NaN per IEEE 754)
        IrBinOp::SDiv | IrBinOp::UDiv => lhs / rhs,
        IrBinOp::SRem | IrBinOp::URem => lhs % rhs,
        // Bitwise ops don't apply to floats
        _ => return None,
    })
}

/// Evaluate a unary operation on a constant float.
fn fold_float_unaryop(op: IrUnaryOp, src: f64) -> Option<f64> {
    match op {
        IrUnaryOp::Neg => Some(-src),
        _ => None,
    }
}

/// Evaluate a comparison on two constant floats.
/// Uses IEEE 754 comparison semantics (NaN comparisons return false for ordered ops).
fn fold_float_cmp(op: IrCmpOp, lhs: f64, rhs: f64) -> bool {
    match op {
        IrCmpOp::Eq => lhs == rhs,
        IrCmpOp::Ne => lhs != rhs,
        // For floats, we use ordered comparisons (signed variants)
        IrCmpOp::Slt | IrCmpOp::Ult => lhs < rhs,
        IrCmpOp::Sle | IrCmpOp::Ule => lhs <= rhs,
        IrCmpOp::Sgt | IrCmpOp::Ugt => lhs > rhs,
        IrCmpOp::Sge | IrCmpOp::Uge => lhs >= rhs,
    }
}

/// Try to fold a cast involving float types.
// TODO: simplify.rs also has float cast folding via IrConst::cast_float_to_target
// which doesn't check for NaN/Inf. These should be unified eventually.
fn try_fold_float_cast(dest: Value, src: &Operand, from_ty: IrType, to_ty: IrType) -> Option<Instruction> {
    let src_const = match src {
        Operand::Const(c) => c,
        _ => return None,
    };
    let result = match (from_ty.is_float(), to_ty.is_float()) {
        (true, true) => {
            // float-to-float conversion
            let val = as_f64_const(src)?;
            make_float_const(val, to_ty)
        }
        (true, false) => {
            // float-to-int conversion
            let val = as_f64_const(src)?;
            // Don't fold if value can't be represented as i64
            if !val.is_finite() || val < i64::MIN as f64 || val > i64::MAX as f64 {
                return None;
            }
            IrConst::from_i64(val as i64, to_ty)
        }
        (false, true) => {
            // int-to-float conversion
            let val = src_const.to_i64()?;
            if from_ty.is_unsigned() {
                make_float_const(val as u64 as f64, to_ty)
            } else {
                make_float_const(val as f64, to_ty)
            }
        }
        _ => return None,
    };
    Some(Instruction::Copy {
        dest,
        src: Operand::Const(result),
    })
}

/// Evaluate a binary operation on two constant integers.
fn fold_binop(op: IrBinOp, lhs: i64, rhs: i64) -> Option<i64> {
    Some(match op {
        IrBinOp::Add => lhs.wrapping_add(rhs),
        IrBinOp::Sub => lhs.wrapping_sub(rhs),
        IrBinOp::Mul => lhs.wrapping_mul(rhs),
        IrBinOp::SDiv => {
            if rhs == 0 { return None; } // division by zero is UB, don't fold
            lhs.wrapping_div(rhs)
        }
        IrBinOp::UDiv => {
            if rhs == 0 { return None; }
            (lhs as u64).wrapping_div(rhs as u64) as i64
        }
        IrBinOp::SRem => {
            if rhs == 0 { return None; }
            lhs.wrapping_rem(rhs)
        }
        IrBinOp::URem => {
            if rhs == 0 { return None; }
            (lhs as u64).wrapping_rem(rhs as u64) as i64
        }
        IrBinOp::And => lhs & rhs,
        IrBinOp::Or => lhs | rhs,
        IrBinOp::Xor => lhs ^ rhs,
        IrBinOp::Shl => lhs.wrapping_shl(rhs as u32),
        IrBinOp::AShr => lhs.wrapping_shr(rhs as u32),
        IrBinOp::LShr => (lhs as u64).wrapping_shr(rhs as u32) as i64,
    })
}

/// Evaluate a unary operation on a constant integer.
/// Width-sensitive operations (CLZ, CTZ, Popcount, Bswap) use `ty` to determine
/// whether to operate on 32 or 64 bits, matching the runtime semantics of
/// __builtin_clz vs __builtin_clzll, etc.
fn fold_unaryop(op: IrUnaryOp, src: i64, ty: IrType) -> Option<i64> {
    let is_32bit = ty == IrType::I32 || ty == IrType::U32
        || ty == IrType::I16 || ty == IrType::U16
        || ty == IrType::I8 || ty == IrType::U8;
    Some(match op {
        IrUnaryOp::Neg => src.wrapping_neg(),
        IrUnaryOp::Not => !src,
        IrUnaryOp::Clz => {
            if is_32bit {
                (src as u32).leading_zeros() as i64
            } else {
                (src as u64).leading_zeros() as i64
            }
        }
        IrUnaryOp::Ctz => {
            if src == 0 {
                if is_32bit { 32 } else { 64 }
            } else if is_32bit {
                (src as u32).trailing_zeros() as i64
            } else {
                (src as u64).trailing_zeros() as i64
            }
        }
        IrUnaryOp::Bswap => {
            if is_32bit {
                (src as u32).swap_bytes() as i64
            } else {
                (src as u64).swap_bytes() as i64
            }
        }
        IrUnaryOp::Popcount => {
            if is_32bit {
                (src as u32).count_ones() as i64
            } else {
                (src as u64).count_ones() as i64
            }
        }
    })
}

/// Evaluate a comparison on two constant integers.
fn fold_cmp(op: IrCmpOp, lhs: i64, rhs: i64) -> bool {
    match op {
        IrCmpOp::Eq => lhs == rhs,
        IrCmpOp::Ne => lhs != rhs,
        IrCmpOp::Slt => lhs < rhs,
        IrCmpOp::Sle => lhs <= rhs,
        IrCmpOp::Sgt => lhs > rhs,
        IrCmpOp::Sge => lhs >= rhs,
        IrCmpOp::Ult => (lhs as u64) < (rhs as u64),
        IrCmpOp::Ule => (lhs as u64) <= (rhs as u64),
        IrCmpOp::Ugt => (lhs as u64) > (rhs as u64),
        IrCmpOp::Uge => (lhs as u64) >= (rhs as u64),
    }
}

/// Evaluate a type cast on a constant.
///
/// For signed source types, we sign-extend to get the correct i64 representation.
/// For unsigned source types, we zero-extend (mask to type width).
/// Same logic applies to the target type.
fn fold_cast(val: i64, from_ty: crate::common::types::IrType, to_ty: crate::common::types::IrType) -> i64 {
    use crate::common::types::IrType;

    // First, normalize the value to the source type's width and signedness.
    // Signed types sign-extend; unsigned types zero-extend.
    let src_val = match from_ty {
        IrType::I8 => val as i8 as i64,
        IrType::U8 => val as u8 as i64,
        IrType::I16 => val as i16 as i64,
        IrType::U16 => val as u16 as i64,
        IrType::I32 => val as i32 as i64,
        IrType::U32 => val as u32 as i64,
        _ => val,
    };

    // Then convert to target type width and signedness.
    match to_ty {
        IrType::I8 => src_val as i8 as i64,
        IrType::U8 => src_val as u8 as i64,
        IrType::I16 => src_val as i16 as i64,
        IrType::U16 => src_val as u16 as i64,
        IrType::I32 => src_val as i32 as i64,
        IrType::U32 => src_val as u32 as i64,
        _ => src_val,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::types::IrType;

    #[test]
    fn test_fold_binop_add() {
        assert_eq!(fold_binop(IrBinOp::Add, 3, 4), Some(7));
    }

    #[test]
    fn test_fold_binop_sub() {
        assert_eq!(fold_binop(IrBinOp::Sub, 10, 3), Some(7));
    }

    #[test]
    fn test_fold_binop_mul() {
        assert_eq!(fold_binop(IrBinOp::Mul, 6, 7), Some(42));
    }

    #[test]
    fn test_fold_binop_div() {
        assert_eq!(fold_binop(IrBinOp::SDiv, 10, 3), Some(3));
        assert_eq!(fold_binop(IrBinOp::SDiv, -10, 3), Some(-3));
    }

    #[test]
    fn test_fold_binop_div_by_zero() {
        assert_eq!(fold_binop(IrBinOp::SDiv, 10, 0), None);
        assert_eq!(fold_binop(IrBinOp::UDiv, 10, 0), None);
        assert_eq!(fold_binop(IrBinOp::SRem, 10, 0), None);
    }

    #[test]
    fn test_fold_binop_bitwise() {
        assert_eq!(fold_binop(IrBinOp::And, 0xFF, 0x0F), Some(0x0F));
        assert_eq!(fold_binop(IrBinOp::Or, 0xF0, 0x0F), Some(0xFF));
        assert_eq!(fold_binop(IrBinOp::Xor, 0xFF, 0xFF), Some(0));
    }

    #[test]
    fn test_fold_binop_shift() {
        assert_eq!(fold_binop(IrBinOp::Shl, 1, 3), Some(8));
        assert_eq!(fold_binop(IrBinOp::AShr, -8, 2), Some(-2));
        assert_eq!(fold_binop(IrBinOp::LShr, -1i64, 32), Some(0xFFFFFFFF));
    }

    #[test]
    fn test_fold_unaryop() {
        assert_eq!(fold_unaryop(IrUnaryOp::Neg, 5, IrType::I64), Some(-5));
        assert_eq!(fold_unaryop(IrUnaryOp::Not, 0, IrType::I64), Some(-1));
        // 32-bit popcount of -33 (0xFFFFFFDF) = 31 set bits
        assert_eq!(fold_unaryop(IrUnaryOp::Popcount, -33, IrType::I32), Some(31));
        // 64-bit popcount of -33 (0xFFFFFFFFFFFFFFDF) = 63 set bits
        assert_eq!(fold_unaryop(IrUnaryOp::Popcount, -33, IrType::I64), Some(63));
        // 32-bit CLZ of 1 = 31
        assert_eq!(fold_unaryop(IrUnaryOp::Clz, 1, IrType::I32), Some(31));
        // 64-bit CLZ of 1 = 63
        assert_eq!(fold_unaryop(IrUnaryOp::Clz, 1, IrType::I64), Some(63));
    }

    #[test]
    fn test_fold_cmp() {
        assert!(fold_cmp(IrCmpOp::Eq, 5, 5));
        assert!(!fold_cmp(IrCmpOp::Eq, 5, 6));
        assert!(fold_cmp(IrCmpOp::Slt, -1, 0));
        // -1 as u64 is large, so unsigned comparison flips
        assert!(!fold_cmp(IrCmpOp::Ult, -1i64, 0));
        assert!(fold_cmp(IrCmpOp::Ugt, -1i64, 0));
    }

    #[test]
    fn test_fold_cast() {
        // Sign-extend i8 to i32
        assert_eq!(fold_cast(-1, IrType::I8, IrType::I32), -1);
        // Truncate i32 to i8 (signed)
        assert_eq!(fold_cast(256, IrType::I32, IrType::I8), 0);
        assert_eq!(fold_cast(255, IrType::I32, IrType::I8), -1);
    }

    #[test]
    fn test_fold_cast_unsigned_source() {
        // Zero-extend U8 to I32: 0xFF as u8 = 255, zero-extended to 255
        assert_eq!(fold_cast(-1, IrType::U8, IrType::I32), 255);
        // Zero-extend U8 to I64
        assert_eq!(fold_cast(-1, IrType::U8, IrType::I64), 255);
        // Zero-extend U16 to I32: 0xFFFF as u16 = 65535
        assert_eq!(fold_cast(-1, IrType::U16, IrType::I32), 65535);
        // Zero-extend U16 to I64
        assert_eq!(fold_cast(-1, IrType::U16, IrType::I64), 65535);
        // Zero-extend U32 to I64: 0xFFFFFFFF as u32 = 4294967295
        assert_eq!(fold_cast(-1, IrType::U32, IrType::I64), 4294967295);
    }

    #[test]
    fn test_fold_cast_unsigned_target() {
        // Truncate I32 to U8: 0x1FF & 0xFF = 0xFF = 255
        assert_eq!(fold_cast(0x1FF, IrType::I32, IrType::U8), 255);
        // Truncate I32 to U8: -1 & 0xFF = 255
        assert_eq!(fold_cast(-1, IrType::I32, IrType::U8), 255);
        // Truncate I32 to U16: 0x1FFFF & 0xFFFF = 65535
        assert_eq!(fold_cast(0x1FFFF, IrType::I32, IrType::U16), 65535);
        // Truncate I64 to U32: 0x1FFFFFFFF & 0xFFFFFFFF = 4294967295
        assert_eq!(fold_cast(0x1FFFFFFFF_i64, IrType::I64, IrType::U32), 4294967295);
        // Truncate I64 to U32: -1 & 0xFFFFFFFF = 4294967295
        assert_eq!(fold_cast(-1, IrType::I64, IrType::U32), 4294967295);
    }

    #[test]
    fn test_fold_cast_unsigned_to_unsigned() {
        // U8 255 to U16: zero-extend to 255
        assert_eq!(fold_cast(255, IrType::U8, IrType::U16), 255);
        // U16 to U8: truncate 0x1FF to 0xFF = 255
        assert_eq!(fold_cast(0x1FF, IrType::U16, IrType::U8), 255);
        // U32 to U8: truncate 0x1FF to 0xFF = 255
        assert_eq!(fold_cast(0x1FF, IrType::U32, IrType::U8), 255);
    }

    #[test]
    fn test_try_fold_binop() {
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Add,
            lhs: Operand::Const(IrConst::I32(3)),
            rhs: Operand::Const(IrConst::I32(4)),
            ty: IrType::I32,
        };
        let folded = try_fold(&inst).unwrap();
        match folded {
            Instruction::Copy { src: Operand::Const(IrConst::I32(7)), .. } => {}
            _ => panic!("Expected Copy with constant 7"),
        }
    }

    #[test]
    fn test_no_fold_with_value_operand() {
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Add,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I32(4)),
            ty: IrType::I32,
        };
        assert!(try_fold(&inst).is_none());
    }

    // === Float constant folding tests ===

    #[test]
    fn test_fold_float_neg_zero_add() {
        // -0.0 + 0.0 = +0.0 (IEEE 754)
        let result = fold_float_binop(IrBinOp::Add, -0.0, 0.0).unwrap();
        assert!(result == 0.0 && !result.is_sign_negative(), "Expected +0.0");
    }

    #[test]
    fn test_fold_float_neg_zero_add_self() {
        // -0.0 + -0.0 = -0.0 (IEEE 754)
        let result = fold_float_binop(IrBinOp::Add, -0.0, -0.0).unwrap();
        assert!(result == 0.0 && result.is_sign_negative(), "Expected -0.0");
    }

    #[test]
    fn test_fold_float_nan_mul_zero() {
        // NaN * 0.0 = NaN (IEEE 754)
        let result = fold_float_binop(IrBinOp::Mul, f64::NAN, 0.0).unwrap();
        assert!(result.is_nan(), "Expected NaN");
    }

    #[test]
    fn test_fold_float_neg_mul_zero() {
        // -5.0 * 0.0 = -0.0 (IEEE 754)
        let result = fold_float_binop(IrBinOp::Mul, -5.0, 0.0).unwrap();
        assert!(result == 0.0 && result.is_sign_negative(), "Expected -0.0");
    }

    #[test]
    fn test_fold_float_div_by_zero() {
        // 1.0 / 0.0 = +Inf (IEEE 754)
        let result = fold_float_binop(IrBinOp::SDiv, 1.0, 0.0).unwrap();
        assert!(result.is_infinite() && result.is_sign_positive());
    }

    #[test]
    fn test_fold_float_neg_div_zero() {
        // -1.0 / 0.0 = -Inf (IEEE 754)
        let result = fold_float_binop(IrBinOp::SDiv, -1.0, 0.0).unwrap();
        assert!(result.is_infinite() && result.is_sign_negative());
    }

    #[test]
    fn test_fold_float_cmp_neg_zero_eq() {
        // -0.0 == 0.0 is true (IEEE 754)
        assert!(fold_float_cmp(IrCmpOp::Eq, -0.0, 0.0));
    }

    #[test]
    fn test_fold_float_cmp_nan_ne() {
        // NaN != NaN is true (IEEE 754)
        assert!(fold_float_cmp(IrCmpOp::Ne, f64::NAN, f64::NAN));
        // NaN == NaN is false
        assert!(!fold_float_cmp(IrCmpOp::Eq, f64::NAN, f64::NAN));
        // NaN < 1.0 is false
        assert!(!fold_float_cmp(IrCmpOp::Slt, f64::NAN, 1.0));
    }

    #[test]
    fn test_fold_float_unary_neg() {
        let result = fold_float_unaryop(IrUnaryOp::Neg, 5.0).unwrap();
        assert_eq!(result, -5.0);
        // Negating -0.0 gives +0.0
        let result = fold_float_unaryop(IrUnaryOp::Neg, -0.0).unwrap();
        assert!(result == 0.0 && !result.is_sign_negative());
    }

    #[test]
    fn test_fold_float_cast_int_to_float() {
        let inst = Instruction::Cast {
            dest: Value(0),
            src: Operand::Const(IrConst::I32(42)),
            from_ty: IrType::I32,
            to_ty: IrType::F64,
        };
        let result = try_fold(&inst).unwrap();
        match result {
            Instruction::Copy { src: Operand::Const(IrConst::F64(v)), .. } => {
                assert_eq!(v, 42.0);
            }
            _ => panic!("Expected Copy with F64(42.0)"),
        }
    }

    #[test]
    fn test_fold_float_cast_float_to_int() {
        let inst = Instruction::Cast {
            dest: Value(0),
            src: Operand::Const(IrConst::F64(3.14)),
            from_ty: IrType::F64,
            to_ty: IrType::I32,
        };
        let result = try_fold(&inst).unwrap();
        match result {
            Instruction::Copy { src: Operand::Const(IrConst::I32(v)), .. } => {
                assert_eq!(v, 3);
            }
            _ => panic!("Expected Copy with I32(3)"),
        }
    }

    #[test]
    fn test_fold_float_cast_nan_to_int_no_fold() {
        // NaN to int should not fold
        let inst = Instruction::Cast {
            dest: Value(0),
            src: Operand::Const(IrConst::F64(f64::NAN)),
            from_ty: IrType::F64,
            to_ty: IrType::I32,
        };
        assert!(try_fold(&inst).is_none());
    }

    #[test]
    fn test_fold_float_cast_overflow_to_int_no_fold() {
        // 1e20 exceeds i32 range, should not fold
        let inst = Instruction::Cast {
            dest: Value(0),
            src: Operand::Const(IrConst::F64(1e20)),
            from_ty: IrType::F64,
            to_ty: IrType::I32,
        };
        assert!(try_fold(&inst).is_none());
    }

    #[test]
    fn test_fold_float_cast_inf_to_int_no_fold() {
        let inst = Instruction::Cast {
            dest: Value(0),
            src: Operand::Const(IrConst::F64(f64::INFINITY)),
            from_ty: IrType::F64,
            to_ty: IrType::I64,
        };
        assert!(try_fold(&inst).is_none());
    }

    #[test]
    fn test_fold_float_binop_instruction() {
        // Full instruction folding: F64(3.0) + F64(4.0) => F64(7.0)
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Add,
            lhs: Operand::Const(IrConst::F64(3.0)),
            rhs: Operand::Const(IrConst::F64(4.0)),
            ty: IrType::F64,
        };
        let result = try_fold(&inst).unwrap();
        match result {
            Instruction::Copy { src: Operand::Const(IrConst::F64(v)), .. } => {
                assert_eq!(v, 7.0);
            }
            _ => panic!("Expected Copy with F64(7.0)"),
        }
    }
}
