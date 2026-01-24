//! Global Value Numbering (GVN) pass.
//!
//! This pass assigns "value numbers" to expressions and replaces redundant
//! computations with references to previously computed values. This performs
//! common subexpression elimination (CSE) within basic blocks.
//!
//! Currently implements local (intra-block) value numbering. Global (cross-block)
//! value numbering requires dominator tree analysis.
//! TODO: Extend to full dominator-based GVN.

use std::collections::HashMap;
use crate::ir::ir::*;

/// A value number expression key. Two instructions with the same ExprKey
/// compute the same value (assuming their operands are equivalent).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ExprKey {
    BinOp { op: IrBinOp, lhs: VNOperand, rhs: VNOperand },
    UnaryOp { op: IrUnaryOp, src: VNOperand },
    Cmp { op: IrCmpOp, lhs: VNOperand, rhs: VNOperand },
}

/// A value-numbered operand: either a constant or a value number.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum VNOperand {
    Const(ConstHashKey),
    ValueNum(u32),
}

/// Run GVN (local value numbering) on the entire module.
/// Returns the number of instructions eliminated.
pub fn run(module: &mut IrModule) -> usize {
    module.for_each_function(run_lvn_function)
}

/// Run local value numbering on a function (per-block).
fn run_lvn_function(func: &mut IrFunction) -> usize {
    let mut total = 0;
    for block in &mut func.blocks {
        total += run_lvn_block(block);
    }
    total
}

/// Run local value numbering on a single basic block.
/// Replaces redundant computations with Copy instructions referencing
/// the first computation of that expression.
fn run_lvn_block(block: &mut BasicBlock) -> usize {
    // Maps expression keys to the Value that first computed them
    let mut expr_to_value: HashMap<ExprKey, Value> = HashMap::new();
    // Maps Value IDs to their value numbers (for canonicalization)
    let mut value_numbers: HashMap<u32, u32> = HashMap::new();
    let mut next_vn: u32 = 0;

    let mut eliminated = 0;
    let mut new_instructions = Vec::with_capacity(block.instructions.len());

    for inst in block.instructions.drain(..) {
        match make_expr_key(&inst, &value_numbers) {
            Some((expr_key, dest)) => {
                if let Some(&existing_value) = expr_to_value.get(&expr_key) {
                    // This expression was already computed - replace with copy
                    let existing_vn = value_numbers.get(&existing_value.0).copied().unwrap_or(existing_value.0);
                    value_numbers.insert(dest.0, existing_vn);
                    new_instructions.push(Instruction::Copy {
                        dest,
                        src: Operand::Value(existing_value),
                    });
                    eliminated += 1;
                } else {
                    // New expression - assign value number and record it
                    let vn = next_vn;
                    next_vn += 1;
                    value_numbers.insert(dest.0, vn);
                    expr_to_value.insert(expr_key, dest);
                    new_instructions.push(inst);
                }
            }
            None => {
                // Not a numberable expression (store, call, alloca, etc.)
                // Assign fresh value numbers to any dest
                if let Some(dest) = inst.dest() {
                    let vn = next_vn;
                    next_vn += 1;
                    value_numbers.insert(dest.0, vn);
                }
                new_instructions.push(inst);
            }
        }
    }

    block.instructions = new_instructions;
    eliminated
}

/// Try to create an ExprKey for an instruction (for value numbering).
/// Returns the expression key and the destination value, or None if
/// the instruction is not eligible for value numbering.
fn make_expr_key(inst: &Instruction, value_numbers: &HashMap<u32, u32>) -> Option<(ExprKey, Value)> {
    match inst {
        Instruction::BinOp { dest, op, lhs, rhs, .. } => {
            let lhs_vn = operand_to_vn(lhs, value_numbers);
            let rhs_vn = operand_to_vn(rhs, value_numbers);

            // For commutative operations, canonicalize operand order
            let (lhs_vn, rhs_vn) = if op.is_commutative() {
                canonical_order(lhs_vn, rhs_vn)
            } else {
                (lhs_vn, rhs_vn)
            };

            Some((ExprKey::BinOp { op: *op, lhs: lhs_vn, rhs: rhs_vn }, *dest))
        }
        Instruction::UnaryOp { dest, op, src, .. } => {
            let src_vn = operand_to_vn(src, value_numbers);
            Some((ExprKey::UnaryOp { op: *op, src: src_vn }, *dest))
        }
        Instruction::Cmp { dest, op, lhs, rhs, .. } => {
            let lhs_vn = operand_to_vn(lhs, value_numbers);
            let rhs_vn = operand_to_vn(rhs, value_numbers);
            Some((ExprKey::Cmp { op: *op, lhs: lhs_vn, rhs: rhs_vn }, *dest))
        }
        // Other instructions are not eligible for simple value numbering
        _ => None,
    }
}

/// Convert an Operand to a VNOperand for hashing.
fn operand_to_vn(op: &Operand, value_numbers: &HashMap<u32, u32>) -> VNOperand {
    match op {
        Operand::Const(c) => VNOperand::Const(c.to_hash_key()),
        Operand::Value(v) => {
            let vn = value_numbers.get(&v.0).copied().unwrap_or(v.0);
            VNOperand::ValueNum(vn)
        }
    }
}

/// Canonicalize operand order for commutative operations.
/// Ensures (a + b) and (b + a) hash to the same key.
fn canonical_order(lhs: VNOperand, rhs: VNOperand) -> (VNOperand, VNOperand) {
    if should_swap(&lhs, &rhs) {
        (rhs, lhs)
    } else {
        (lhs, rhs)
    }
}

fn should_swap(lhs: &VNOperand, rhs: &VNOperand) -> bool {
    match (lhs, rhs) {
        (VNOperand::ValueNum(_), VNOperand::Const(_)) => true,
        (VNOperand::ValueNum(a), VNOperand::ValueNum(b)) => a > b,
        (VNOperand::Const(a), VNOperand::Const(b)) => a > b,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::types::IrType;

    #[test]
    fn test_commutative_cse() {
        // Test that a + b and b + a are recognized as the same expression
        let mut block = BasicBlock {
            label: "entry".to_string(),
            instructions: vec![
                // %0 = add %a, %b
                Instruction::BinOp {
                    dest: Value(2),
                    op: IrBinOp::Add,
                    lhs: Operand::Value(Value(0)),
                    rhs: Operand::Value(Value(1)),
                    ty: IrType::I32,
                },
                // %1 = add %b, %a  (same expression, reversed operands)
                Instruction::BinOp {
                    dest: Value(3),
                    op: IrBinOp::Add,
                    lhs: Operand::Value(Value(1)),
                    rhs: Operand::Value(Value(0)),
                    ty: IrType::I32,
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(3)))),
        };

        let eliminated = run_lvn_block(&mut block);
        assert_eq!(eliminated, 1);

        // Second instruction should be a Copy
        match &block.instructions[1] {
            Instruction::Copy { dest, src: Operand::Value(v) } => {
                assert_eq!(dest.0, 3);
                assert_eq!(v.0, 2);
            }
            other => panic!("Expected Copy instruction, got {:?}", other),
        }
    }

    #[test]
    fn test_non_commutative_not_cse() {
        // Test that a - b and b - a are NOT treated as the same
        let mut block = BasicBlock {
            label: "entry".to_string(),
            instructions: vec![
                Instruction::BinOp {
                    dest: Value(2),
                    op: IrBinOp::Sub,
                    lhs: Operand::Value(Value(0)),
                    rhs: Operand::Value(Value(1)),
                    ty: IrType::I32,
                },
                Instruction::BinOp {
                    dest: Value(3),
                    op: IrBinOp::Sub,
                    lhs: Operand::Value(Value(1)),
                    rhs: Operand::Value(Value(0)),
                    ty: IrType::I32,
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(3)))),
        };

        let eliminated = run_lvn_block(&mut block);
        assert_eq!(eliminated, 0); // Should NOT eliminate
    }

    #[test]
    fn test_constant_cse() {
        // Two identical constant expressions should be CSE'd
        let mut block = BasicBlock {
            label: "entry".to_string(),
            instructions: vec![
                Instruction::BinOp {
                    dest: Value(0),
                    op: IrBinOp::Add,
                    lhs: Operand::Const(IrConst::I32(3)),
                    rhs: Operand::Const(IrConst::I32(4)),
                    ty: IrType::I32,
                },
                Instruction::BinOp {
                    dest: Value(1),
                    op: IrBinOp::Add,
                    lhs: Operand::Const(IrConst::I32(3)),
                    rhs: Operand::Const(IrConst::I32(4)),
                    ty: IrType::I32,
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(1)))),
        };

        let eliminated = run_lvn_block(&mut block);
        assert_eq!(eliminated, 1);
    }

    #[test]
    fn test_is_commutative() {
        assert!(IrBinOp::Add.is_commutative());
        assert!(IrBinOp::Mul.is_commutative());
        assert!(!IrBinOp::Sub.is_commutative());
        assert!(!IrBinOp::SDiv.is_commutative());
    }
}
