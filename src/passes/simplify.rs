//! Algebraic simplification (strength reduction) pass.
//!
//! Applies algebraic identities to simplify instructions:
//! - x + 0 => x
//! - x * 1 => x
//! - x * 0 => 0
//! - x - 0 => x
//! - x / 1 => x
//! - x & 0 => 0
//! - x | 0 => x
//! - x ^ 0 => x
//! - x << 0 => x
//! - x >> 0 => x
//! - x - x => 0
//! - x ^ x => 0
//! - x & x => x
//! - x | x => x

use crate::ir::ir::*;

/// Run algebraic simplification on the module.
/// Returns the number of instructions simplified.
pub fn run(module: &mut IrModule) -> usize {
    module.for_each_function(simplify_function)
}

fn simplify_function(func: &mut IrFunction) -> usize {
    let mut total = 0;
    for block in &mut func.blocks {
        let mut new_instructions = Vec::with_capacity(block.instructions.len());
        for inst in block.instructions.drain(..) {
            match try_simplify(&inst) {
                Some(simplified) => {
                    new_instructions.push(simplified);
                    total += 1;
                }
                None => {
                    new_instructions.push(inst);
                }
            }
        }
        block.instructions = new_instructions;
    }
    total
}

/// Try to simplify an instruction using algebraic identities.
fn try_simplify(inst: &Instruction) -> Option<Instruction> {
    match inst {
        Instruction::BinOp { dest, op, lhs, rhs, ty } => {
            simplify_binop(*dest, *op, lhs, rhs, *ty)
        }
        _ => None,
    }
}

fn simplify_binop(
    dest: Value,
    op: IrBinOp,
    lhs: &Operand,
    rhs: &Operand,
    ty: crate::common::types::IrType,
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
                return Some(Instruction::Copy { dest, src: lhs.clone() });
            }
            if lhs_zero {
                // 0 + x => x
                return Some(Instruction::Copy { dest, src: rhs.clone() });
            }
        }
        IrBinOp::Sub => {
            if rhs_zero {
                // x - 0 => x
                return Some(Instruction::Copy { dest, src: lhs.clone() });
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
                return Some(Instruction::Copy { dest, src: lhs.clone() });
            }
            if lhs_one {
                // 1 * x => x
                return Some(Instruction::Copy { dest, src: rhs.clone() });
            }
        }
        IrBinOp::SDiv | IrBinOp::UDiv => {
            if rhs_one {
                // x / 1 => x
                return Some(Instruction::Copy { dest, src: lhs.clone() });
            }
            if same_value {
                // x / x => 1 (assuming x != 0, which is UB anyway)
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::one(ty)),
                });
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
                return Some(Instruction::Copy { dest, src: lhs.clone() });
            }
        }
        IrBinOp::Or => {
            if rhs_zero {
                // x | 0 => x
                return Some(Instruction::Copy { dest, src: lhs.clone() });
            }
            if lhs_zero {
                // 0 | x => x
                return Some(Instruction::Copy { dest, src: rhs.clone() });
            }
            if same_value {
                // x | x => x
                return Some(Instruction::Copy { dest, src: lhs.clone() });
            }
        }
        IrBinOp::Xor => {
            if rhs_zero {
                // x ^ 0 => x
                return Some(Instruction::Copy { dest, src: lhs.clone() });
            }
            if lhs_zero {
                // 0 ^ x => x
                return Some(Instruction::Copy { dest, src: rhs.clone() });
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
                return Some(Instruction::Copy { dest, src: lhs.clone() });
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
    use crate::common::types::IrType;

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
}
