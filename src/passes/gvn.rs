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
    BinOp { op: BinOpKey, lhs: VNOperand, rhs: VNOperand },
    UnaryOp { op: UnaryOpKey, src: VNOperand },
    Cmp { op: CmpOpKey, lhs: VNOperand, rhs: VNOperand },
}

/// A value-numbered operand: either a constant or a value number.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum VNOperand {
    Const(ConstKey),
    ValueNum(u32),
}

/// Hashable representation of constants.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ConstKey {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(u32),   // use bits for hashing
    F64(u64),   // use bits for hashing
    Zero,
}

/// Hashable binary operation identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BinOpKey {
    Add, Sub, Mul, SDiv, UDiv, SRem, URem,
    And, Or, Xor, Shl, AShr, LShr,
}

/// Hashable unary operation identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum UnaryOpKey {
    Neg, Not,
}

/// Hashable comparison operation identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum CmpOpKey {
    Eq, Ne, Slt, Sle, Sgt, Sge, Ult, Ule, Ugt, Uge,
}

/// Run GVN (local value numbering) on the entire module.
/// Returns the number of instructions eliminated.
pub fn run(module: &mut IrModule) -> usize {
    let mut total = 0;
    for func in &mut module.functions {
        if func.is_declaration {
            continue;
        }
        total += run_lvn_function(func);
    }
    total
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
                if let Some(dest) = get_inst_dest(&inst) {
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
            let op_key = binop_to_key(*op);

            // For commutative operations, canonicalize operand order
            let (lhs_vn, rhs_vn) = if is_commutative(*op) {
                canonical_order(lhs_vn, rhs_vn)
            } else {
                (lhs_vn, rhs_vn)
            };

            Some((ExprKey::BinOp { op: op_key, lhs: lhs_vn, rhs: rhs_vn }, *dest))
        }
        Instruction::UnaryOp { dest, op, src, .. } => {
            let src_vn = operand_to_vn(src, value_numbers);
            let op_key = unaryop_to_key(*op);
            Some((ExprKey::UnaryOp { op: op_key, src: src_vn }, *dest))
        }
        Instruction::Cmp { dest, op, lhs, rhs, .. } => {
            let lhs_vn = operand_to_vn(lhs, value_numbers);
            let rhs_vn = operand_to_vn(rhs, value_numbers);
            let op_key = cmpop_to_key(*op);
            Some((ExprKey::Cmp { op: op_key, lhs: lhs_vn, rhs: rhs_vn }, *dest))
        }
        // Other instructions are not eligible for simple value numbering
        _ => None,
    }
}

/// Convert an Operand to a VNOperand for hashing.
fn operand_to_vn(op: &Operand, value_numbers: &HashMap<u32, u32>) -> VNOperand {
    match op {
        Operand::Const(c) => VNOperand::Const(const_to_key(c)),
        Operand::Value(v) => {
            let vn = value_numbers.get(&v.0).copied().unwrap_or(v.0);
            VNOperand::ValueNum(vn)
        }
    }
}

/// Canonicalize operand order for commutative operations.
/// Ensures (a + b) and (b + a) hash to the same key.
fn canonical_order(lhs: VNOperand, rhs: VNOperand) -> (VNOperand, VNOperand) {
    // Simple ordering: constants before value numbers, lower numbers first
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
        (VNOperand::Const(a), VNOperand::Const(b)) => format!("{:?}", a) > format!("{:?}", b),
        _ => false,
    }
}

/// Check if a binary operation is commutative.
fn is_commutative(op: IrBinOp) -> bool {
    matches!(op, IrBinOp::Add | IrBinOp::Mul | IrBinOp::And | IrBinOp::Or | IrBinOp::Xor)
}

/// Get dest of an instruction.
fn get_inst_dest(inst: &Instruction) -> Option<Value> {
    match inst {
        Instruction::Alloca { dest, .. } => Some(*dest),
        Instruction::Load { dest, .. } => Some(*dest),
        Instruction::BinOp { dest, .. } => Some(*dest),
        Instruction::UnaryOp { dest, .. } => Some(*dest),
        Instruction::Cmp { dest, .. } => Some(*dest),
        Instruction::Call { dest, .. } => *dest,
        Instruction::CallIndirect { dest, .. } => *dest,
        Instruction::GetElementPtr { dest, .. } => Some(*dest),
        Instruction::Cast { dest, .. } => Some(*dest),
        Instruction::Copy { dest, .. } => Some(*dest),
        Instruction::GlobalAddr { dest, .. } => Some(*dest),
        Instruction::Store { .. } => None,
    }
}

fn const_to_key(c: &IrConst) -> ConstKey {
    match c {
        IrConst::I8(v) => ConstKey::I8(*v),
        IrConst::I16(v) => ConstKey::I16(*v),
        IrConst::I32(v) => ConstKey::I32(*v),
        IrConst::I64(v) => ConstKey::I64(*v),
        IrConst::F32(v) => ConstKey::F32(v.to_bits()),
        IrConst::F64(v) => ConstKey::F64(v.to_bits()),
        IrConst::Zero => ConstKey::Zero,
    }
}

fn binop_to_key(op: IrBinOp) -> BinOpKey {
    match op {
        IrBinOp::Add => BinOpKey::Add,
        IrBinOp::Sub => BinOpKey::Sub,
        IrBinOp::Mul => BinOpKey::Mul,
        IrBinOp::SDiv => BinOpKey::SDiv,
        IrBinOp::UDiv => BinOpKey::UDiv,
        IrBinOp::SRem => BinOpKey::SRem,
        IrBinOp::URem => BinOpKey::URem,
        IrBinOp::And => BinOpKey::And,
        IrBinOp::Or => BinOpKey::Or,
        IrBinOp::Xor => BinOpKey::Xor,
        IrBinOp::Shl => BinOpKey::Shl,
        IrBinOp::AShr => BinOpKey::AShr,
        IrBinOp::LShr => BinOpKey::LShr,
    }
}

fn unaryop_to_key(op: IrUnaryOp) -> UnaryOpKey {
    match op {
        IrUnaryOp::Neg => UnaryOpKey::Neg,
        IrUnaryOp::Not => UnaryOpKey::Not,
    }
}

fn cmpop_to_key(op: IrCmpOp) -> CmpOpKey {
    match op {
        IrCmpOp::Eq => CmpOpKey::Eq,
        IrCmpOp::Ne => CmpOpKey::Ne,
        IrCmpOp::Slt => CmpOpKey::Slt,
        IrCmpOp::Sle => CmpOpKey::Sle,
        IrCmpOp::Sgt => CmpOpKey::Sgt,
        IrCmpOp::Sge => CmpOpKey::Sge,
        IrCmpOp::Ult => CmpOpKey::Ult,
        IrCmpOp::Ule => CmpOpKey::Ule,
        IrCmpOp::Ugt => CmpOpKey::Ugt,
        IrCmpOp::Uge => CmpOpKey::Uge,
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
        assert!(is_commutative(IrBinOp::Add));
        assert!(is_commutative(IrBinOp::Mul));
        assert!(!is_commutative(IrBinOp::Sub));
        assert!(!is_commutative(IrBinOp::SDiv));
    }
}
