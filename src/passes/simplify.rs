//! Algebraic simplification and strength reduction pass.
//!
//! Applies algebraic identities to simplify instructions:
//! - x + 0 => x, 0 + x => x
//! - x - 0 => x, x - x => 0
//! - x * 0 => 0, x * 1 => x
//! - x / 1 => x, x / x => 1
//! - x % 1 => 0, x % x => 0
//! - x & 0 => 0, x & x => x
//! - x | 0 => x, x | x => x
//! - x ^ 0 => x, x ^ x => 0
//! - x << 0 => x, x >> 0 => x
//!
//! Strength reduction (integer types only):
//! - x * 2^k => x << k  (multiply by power-of-2 to shift)
//! - x * 2 => x + x     (slightly cheaper on some uarches)
//! - x / 2^k => x >> k  (unsigned divide by power-of-2 to logical shift)
//!
//! Redundant instruction elimination:
//! - Cast where from_ty == to_ty => Copy
//! - GetElementPtr with constant zero offset => Copy of base

use crate::common::types::IrType;
use crate::ir::ir::*;

/// Run algebraic simplification on the module.
/// Returns the number of instructions simplified.
pub fn run(module: &mut IrModule) -> usize {
    module.for_each_function(simplify_function)
}

fn simplify_function(func: &mut IrFunction) -> usize {
    let mut total = 0;
    for block in &mut func.blocks {
        for inst in &mut block.instructions {
            if let Some(simplified) = try_simplify(inst) {
                *inst = simplified;
                total += 1;
            }
        }
    }
    total
}

/// Try to simplify an instruction using algebraic identities and strength reduction.
fn try_simplify(inst: &Instruction) -> Option<Instruction> {
    match inst {
        Instruction::BinOp { dest, op, lhs, rhs, ty } => {
            simplify_binop(*dest, *op, lhs, rhs, *ty)
        }
        Instruction::Cast { dest, src, from_ty, to_ty } => {
            simplify_cast(*dest, src, *from_ty, *to_ty)
        }
        Instruction::GetElementPtr { dest, base, offset, ty } => {
            simplify_gep(*dest, *base, offset, *ty)
        }
        _ => None,
    }
}

/// Simplify a Cast instruction.
/// Cast from type T to the same type T is a no-op copy.
fn simplify_cast(dest: Value, src: &Operand, from_ty: IrType, to_ty: IrType) -> Option<Instruction> {
    if from_ty == to_ty {
        return Some(Instruction::Copy { dest, src: *src });
    }
    None
}

/// Simplify a GetElementPtr instruction.
/// GEP with a constant zero offset is just a copy of the base pointer.
fn simplify_gep(dest: Value, base: Value, offset: &Operand, _ty: IrType) -> Option<Instruction> {
    if is_zero(offset) {
        return Some(Instruction::Copy { dest, src: Operand::Value(base) });
    }
    None
}

/// Return the log2 of a positive i64 value if it is a power of 2, or None otherwise.
fn const_power_of_two(op: &Operand) -> Option<u32> {
    match op {
        Operand::Const(c) => {
            let val = c.to_i64()?;
            if val > 0 && (val & (val - 1)) == 0 {
                Some(val.trailing_zeros())
            } else {
                None
            }
        }
        _ => None,
    }
}

fn simplify_binop(
    dest: Value,
    op: IrBinOp,
    lhs: &Operand,
    rhs: &Operand,
    ty: IrType,
) -> Option<Instruction> {
    let lhs_zero = is_zero(lhs);
    let rhs_zero = is_zero(rhs);
    let lhs_one = is_one(lhs);
    let rhs_one = is_one(rhs);
    let same_value = same_value_operands(lhs, rhs);

    match op {
        IrBinOp::Add => {
            if rhs_zero {
                // x + 0 => x
                return Some(Instruction::Copy { dest, src: *lhs });
            }
            if lhs_zero {
                // 0 + x => x
                return Some(Instruction::Copy { dest, src: *rhs });
            }
        }
        IrBinOp::Sub => {
            if rhs_zero {
                // x - 0 => x
                return Some(Instruction::Copy { dest, src: *lhs });
            }
            if same_value {
                // x - x => 0
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::zero(ty)),
                });
            }
        }
        IrBinOp::Mul => {
            if rhs_zero || lhs_zero {
                // x * 0 or 0 * x => 0
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::zero(ty)),
                });
            }
            if rhs_one {
                // x * 1 => x
                return Some(Instruction::Copy { dest, src: *lhs });
            }
            if lhs_one {
                // 1 * x => x
                return Some(Instruction::Copy { dest, src: *rhs });
            }
            // Strength reduction: multiply by power-of-2 => shift left
            // Only for integer types (not floats)
            if ty.is_integer() || ty == IrType::Ptr {
                if let Some(shift) = const_power_of_two(rhs) {
                    if shift == 1 {
                        // x * 2 => x + x
                        return Some(Instruction::BinOp {
                            dest,
                            op: IrBinOp::Add,
                            lhs: *lhs,
                            rhs: *lhs,
                            ty,
                        });
                    }
                    // x * 2^k => x << k
                    return Some(Instruction::BinOp {
                        dest,
                        op: IrBinOp::Shl,
                        lhs: *lhs,
                        rhs: Operand::Const(IrConst::from_i64(shift as i64, ty)),
                        ty,
                    });
                }
                if let Some(shift) = const_power_of_two(lhs) {
                    if shift == 1 {
                        // 2 * x => x + x
                        return Some(Instruction::BinOp {
                            dest,
                            op: IrBinOp::Add,
                            lhs: *rhs,
                            rhs: *rhs,
                            ty,
                        });
                    }
                    // 2^k * x => x << k
                    return Some(Instruction::BinOp {
                        dest,
                        op: IrBinOp::Shl,
                        lhs: *rhs,
                        rhs: Operand::Const(IrConst::from_i64(shift as i64, ty)),
                        ty,
                    });
                }
            }
        }
        IrBinOp::SDiv | IrBinOp::UDiv => {
            if rhs_one {
                // x / 1 => x
                return Some(Instruction::Copy { dest, src: *lhs });
            }
            if same_value {
                // x / x => 1 (assuming x != 0, which is UB anyway)
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::one(ty)),
                });
            }
            // Strength reduction: unsigned divide by power-of-2 => logical shift right
            if op == IrBinOp::UDiv {
                if (ty.is_integer() || ty == IrType::Ptr) && ty.is_unsigned() {
                    if let Some(shift) = const_power_of_two(rhs) {
                        return Some(Instruction::BinOp {
                            dest,
                            op: IrBinOp::LShr,
                            lhs: *lhs,
                            rhs: Operand::Const(IrConst::from_i64(shift as i64, ty)),
                            ty,
                        });
                    }
                }
            }
        }
        IrBinOp::SRem | IrBinOp::URem => {
            if rhs_one {
                // x % 1 => 0
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::zero(ty)),
                });
            }
            if same_value {
                // x % x => 0
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::zero(ty)),
                });
            }
            // Strength reduction: unsigned rem by power-of-2 => bitwise AND with mask
            if op == IrBinOp::URem {
                if (ty.is_integer() || ty == IrType::Ptr) && ty.is_unsigned() {
                    if let Some(shift) = const_power_of_two(rhs) {
                        // x % 2^k => x & (2^k - 1)
                        let mask = (1i64 << shift) - 1;
                        return Some(Instruction::BinOp {
                            dest,
                            op: IrBinOp::And,
                            lhs: *lhs,
                            rhs: Operand::Const(IrConst::from_i64(mask, ty)),
                            ty,
                        });
                    }
                }
            }
        }
        IrBinOp::And => {
            if rhs_zero || lhs_zero {
                // x & 0 => 0
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::zero(ty)),
                });
            }
            if same_value {
                // x & x => x
                return Some(Instruction::Copy { dest, src: *lhs });
            }
        }
        IrBinOp::Or => {
            if rhs_zero {
                // x | 0 => x
                return Some(Instruction::Copy { dest, src: *lhs });
            }
            if lhs_zero {
                // 0 | x => x
                return Some(Instruction::Copy { dest, src: *rhs });
            }
            if same_value {
                // x | x => x
                return Some(Instruction::Copy { dest, src: *lhs });
            }
        }
        IrBinOp::Xor => {
            if rhs_zero {
                // x ^ 0 => x
                return Some(Instruction::Copy { dest, src: *lhs });
            }
            if lhs_zero {
                // 0 ^ x => x
                return Some(Instruction::Copy { dest, src: *rhs });
            }
            if same_value {
                // x ^ x => 0
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::zero(ty)),
                });
            }
        }
        IrBinOp::Shl | IrBinOp::AShr | IrBinOp::LShr => {
            if rhs_zero {
                // x << 0 or x >> 0 => x
                return Some(Instruction::Copy { dest, src: *lhs });
            }
            if lhs_zero {
                // 0 << x or 0 >> x => 0
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::zero(ty)),
                });
            }
        }
    }

    None
}

/// Check if an operand is zero.
fn is_zero(op: &Operand) -> bool {
    matches!(op, Operand::Const(c) if c.is_zero())
}

/// Check if an operand is one.
fn is_one(op: &Operand) -> bool {
    matches!(op, Operand::Const(c) if c.is_one())
}

/// Check if two operands refer to the same value.
fn same_value_operands(lhs: &Operand, rhs: &Operand) -> bool {
    match (lhs, rhs) {
        (Operand::Value(a), Operand::Value(b)) => a.0 == b.0,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_zero() {
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Add,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I32(0)),
            ty: IrType::I32,
        };
        let result = try_simplify(&inst).unwrap();
        match result {
            Instruction::Copy { src: Operand::Value(v), .. } => assert_eq!(v.0, 1),
            _ => panic!("Expected Copy"),
        }
    }

    #[test]
    fn test_mul_zero() {
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Mul,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I32(0)),
            ty: IrType::I32,
        };
        let result = try_simplify(&inst).unwrap();
        match result {
            Instruction::Copy { src: Operand::Const(IrConst::I32(0)), .. } => {}
            _ => panic!("Expected Copy with zero"),
        }
    }

    #[test]
    fn test_mul_one() {
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Mul,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I32(1)),
            ty: IrType::I32,
        };
        let result = try_simplify(&inst).unwrap();
        match result {
            Instruction::Copy { src: Operand::Value(v), .. } => assert_eq!(v.0, 1),
            _ => panic!("Expected Copy of lhs"),
        }
    }

    #[test]
    fn test_sub_self() {
        let inst = Instruction::BinOp {
            dest: Value(2),
            op: IrBinOp::Sub,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Value(Value(1)),
            ty: IrType::I32,
        };
        let result = try_simplify(&inst).unwrap();
        match result {
            Instruction::Copy { src: Operand::Const(IrConst::I32(0)), .. } => {}
            _ => panic!("Expected Copy with zero"),
        }
    }

    #[test]
    fn test_xor_self() {
        let inst = Instruction::BinOp {
            dest: Value(2),
            op: IrBinOp::Xor,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Value(Value(1)),
            ty: IrType::I32,
        };
        let result = try_simplify(&inst).unwrap();
        match result {
            Instruction::Copy { src: Operand::Const(IrConst::I32(0)), .. } => {}
            _ => panic!("Expected Copy with zero"),
        }
    }

    #[test]
    fn test_and_self() {
        let inst = Instruction::BinOp {
            dest: Value(2),
            op: IrBinOp::And,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Value(Value(1)),
            ty: IrType::I32,
        };
        let result = try_simplify(&inst).unwrap();
        match result {
            Instruction::Copy { src: Operand::Value(v), .. } => assert_eq!(v.0, 1),
            _ => panic!("Expected Copy of operand"),
        }
    }

    #[test]
    fn test_no_simplify() {
        // x + y (non-trivial) should not simplify
        let inst = Instruction::BinOp {
            dest: Value(2),
            op: IrBinOp::Add,
            lhs: Operand::Value(Value(0)),
            rhs: Operand::Value(Value(1)),
            ty: IrType::I32,
        };
        assert!(try_simplify(&inst).is_none());
    }

    #[test]
    fn test_mul_power_of_two_to_shift() {
        // x * 4 => x << 2
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Mul,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I64(4)),
            ty: IrType::I64,
        };
        let result = try_simplify(&inst).unwrap();
        match result {
            Instruction::BinOp { op: IrBinOp::Shl, rhs: Operand::Const(IrConst::I64(2)), .. } => {}
            _ => panic!("Expected Shl by 2, got {:?}", result),
        }
    }

    #[test]
    fn test_mul_two_to_add() {
        // x * 2 => x + x
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Mul,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I64(2)),
            ty: IrType::I64,
        };
        let result = try_simplify(&inst).unwrap();
        match result {
            Instruction::BinOp { op: IrBinOp::Add, lhs: Operand::Value(a), rhs: Operand::Value(b), .. } => {
                assert_eq!(a.0, 1);
                assert_eq!(b.0, 1);
            }
            _ => panic!("Expected Add x,x, got {:?}", result),
        }
    }

    #[test]
    fn test_mul_power_of_two_i32() {
        // x * 8 => x << 3 (I32)
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Mul,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I32(8)),
            ty: IrType::I32,
        };
        let result = try_simplify(&inst).unwrap();
        match result {
            Instruction::BinOp { op: IrBinOp::Shl, rhs: Operand::Const(IrConst::I32(3)), .. } => {}
            _ => panic!("Expected Shl by 3, got {:?}", result),
        }
    }

    #[test]
    fn test_mul_non_power_of_two_no_change() {
        // x * 3 should NOT be simplified to shift
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Mul,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I64(3)),
            ty: IrType::I64,
        };
        assert!(try_simplify(&inst).is_none());
    }

    #[test]
    fn test_mul_float_no_strength_reduction() {
        // x * 2.0 should NOT be simplified (float type)
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Mul,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I64(2)),
            ty: IrType::F64,
        };
        assert!(try_simplify(&inst).is_none());
    }

    #[test]
    fn test_udiv_power_of_two() {
        // x /u 8 => x >> 3  (unsigned)
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::UDiv,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I64(8)),
            ty: IrType::U64,
        };
        let result = try_simplify(&inst).unwrap();
        match result {
            Instruction::BinOp { op: IrBinOp::LShr, rhs: Operand::Const(IrConst::I64(3)), .. } => {}
            _ => panic!("Expected LShr by 3, got {:?}", result),
        }
    }

    #[test]
    fn test_urem_power_of_two() {
        // x %u 8 => x & 7  (unsigned)
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::URem,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I64(8)),
            ty: IrType::U64,
        };
        let result = try_simplify(&inst).unwrap();
        match result {
            Instruction::BinOp { op: IrBinOp::And, rhs: Operand::Const(IrConst::I64(7)), .. } => {}
            _ => panic!("Expected And with 7, got {:?}", result),
        }
    }

    #[test]
    fn test_cast_same_type() {
        let inst = Instruction::Cast {
            dest: Value(0),
            src: Operand::Value(Value(1)),
            from_ty: IrType::I32,
            to_ty: IrType::I32,
        };
        let result = try_simplify(&inst).unwrap();
        match result {
            Instruction::Copy { src: Operand::Value(v), .. } => assert_eq!(v.0, 1),
            _ => panic!("Expected Copy"),
        }
    }

    #[test]
    fn test_cast_different_type_no_change() {
        let inst = Instruction::Cast {
            dest: Value(0),
            src: Operand::Value(Value(1)),
            from_ty: IrType::I32,
            to_ty: IrType::I64,
        };
        assert!(try_simplify(&inst).is_none());
    }

    #[test]
    fn test_gep_zero_offset() {
        let inst = Instruction::GetElementPtr {
            dest: Value(0),
            base: Value(1),
            offset: Operand::Const(IrConst::I64(0)),
            ty: IrType::Ptr,
        };
        let result = try_simplify(&inst).unwrap();
        match result {
            Instruction::Copy { src: Operand::Value(v), .. } => assert_eq!(v.0, 1),
            _ => panic!("Expected Copy of base"),
        }
    }

    #[test]
    fn test_gep_nonzero_no_change() {
        let inst = Instruction::GetElementPtr {
            dest: Value(0),
            base: Value(1),
            offset: Operand::Const(IrConst::I64(4)),
            ty: IrType::Ptr,
        };
        assert!(try_simplify(&inst).is_none());
    }
}
