//! Constant folding optimization pass.
//!
//! This pass evaluates operations on constant operands at compile time,
//! replacing the instruction with the computed constant. This eliminates
//! redundant computation and enables further optimizations (DCE, etc.).

use crate::ir::ir::*;

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
            let src_const = as_i64_const(src)?;
            let result = fold_unaryop(*op, src_const)?;
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
            let src_const = as_i64_const(src)?;
            let result = fold_cast(src_const, *from_ty, *to_ty);
            Some(Instruction::Copy {
                dest: *dest,
                src: Operand::Const(IrConst::from_i64(result, *to_ty)),
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
fn fold_unaryop(op: IrUnaryOp, src: i64) -> Option<i64> {
    Some(match op {
        IrUnaryOp::Neg => src.wrapping_neg(),
        IrUnaryOp::Not => !src,
        IrUnaryOp::Clz => (src as u64).leading_zeros() as i64,
        IrUnaryOp::Ctz => if src == 0 { 64 } else { (src as u64).trailing_zeros() as i64 },
        IrUnaryOp::Bswap => (src as u64).swap_bytes() as i64,
        IrUnaryOp::Popcount => (src as u64).count_ones() as i64,
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
fn fold_cast(val: i64, from_ty: crate::common::types::IrType, to_ty: crate::common::types::IrType) -> i64 {
    use crate::common::types::IrType;

    // First truncate/sign-extend to source type width
    let src_val = match from_ty {
        IrType::I8 => val as i8 as i64,
        IrType::I16 => val as i16 as i64,
        IrType::I32 => val as i32 as i64,
        _ => val,
    };

    // Then convert to target type
    match to_ty {
        IrType::I8 => src_val as i8 as i64,
        IrType::I16 => src_val as i16 as i64,
        IrType::I32 => src_val as i32 as i64,
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
        assert_eq!(fold_unaryop(IrUnaryOp::Neg, 5), Some(-5));
        assert_eq!(fold_unaryop(IrUnaryOp::Not, 0), Some(-1));
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
        // Truncate i32 to i8
        assert_eq!(fold_cast(256, IrType::I32, IrType::I8), 0);
        assert_eq!(fold_cast(255, IrType::I32, IrType::I8), -1);
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
}
