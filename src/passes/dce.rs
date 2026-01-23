//! Dead Code Elimination (DCE) pass.
//!
//! Removes instructions whose results are never used by any other instruction
//! or terminator. This is a backwards dataflow analysis: we mark all values
//! that are live (used somewhere), then remove instructions that define
//! dead values.
//!
//! Side-effecting instructions (stores, calls) are never removed.

use std::collections::HashSet;
use crate::ir::ir::*;

/// Run dead code elimination on the entire module.
/// Returns the number of instructions removed.
pub fn run(module: &mut IrModule) -> usize {
    let mut total_removed = 0;
    for func in &mut module.functions {
        if func.is_declaration {
            continue;
        }
        total_removed += eliminate_dead_code(func);
    }
    total_removed
}

/// Eliminate dead code in a single function.
/// Iterates until no more dead code is found (fixpoint).
fn eliminate_dead_code(func: &mut IrFunction) -> usize {
    let mut total = 0;
    loop {
        let removed = eliminate_dead_code_once(func);
        if removed == 0 {
            break;
        }
        total += removed;
    }
    total
}

/// Single pass of dead code elimination.
fn eliminate_dead_code_once(func: &mut IrFunction) -> usize {
    // Step 1: Collect all used values
    let used_values = collect_used_values(func);

    // Step 2: Remove instructions that define unused values (and have no side effects)
    let mut removed = 0;
    for block in &mut func.blocks {
        let original_len = block.instructions.len();
        block.instructions.retain(|inst| {
            if has_side_effects(inst) {
                return true; // never remove side-effecting instructions
            }
            match get_dest(inst) {
                Some(dest) => {
                    if used_values.contains(&dest.0) {
                        true // value is used, keep it
                    } else {
                        false // dead instruction, remove it
                    }
                }
                None => true, // no dest means it's side-effecting (already covered above)
            }
        });
        removed += original_len - block.instructions.len();
    }

    removed
}

/// Collect all Value IDs that are used in the function.
fn collect_used_values(func: &IrFunction) -> HashSet<u32> {
    let mut used = HashSet::new();

    for block in &func.blocks {
        // Collect uses from instructions
        for inst in &block.instructions {
            collect_instruction_uses(inst, &mut used);
        }

        // Collect uses from terminators
        collect_terminator_uses(&block.terminator, &mut used);
    }

    used
}

/// Record all values used by an instruction.
fn collect_instruction_uses(inst: &Instruction, used: &mut HashSet<u32>) {
    match inst {
        Instruction::Alloca { .. } => {}
        Instruction::Store { val, ptr, .. } => {
            collect_operand_uses(val, used);
            used.insert(ptr.0);
        }
        Instruction::Load { ptr, .. } => {
            used.insert(ptr.0);
        }
        Instruction::BinOp { lhs, rhs, .. } => {
            collect_operand_uses(lhs, used);
            collect_operand_uses(rhs, used);
        }
        Instruction::UnaryOp { src, .. } => {
            collect_operand_uses(src, used);
        }
        Instruction::Cmp { lhs, rhs, .. } => {
            collect_operand_uses(lhs, used);
            collect_operand_uses(rhs, used);
        }
        Instruction::Call { args, .. } => {
            for arg in args {
                collect_operand_uses(arg, used);
            }
        }
        Instruction::CallIndirect { func_ptr, args, .. } => {
            collect_operand_uses(func_ptr, used);
            for arg in args {
                collect_operand_uses(arg, used);
            }
        }
        Instruction::GetElementPtr { base, offset, .. } => {
            used.insert(base.0);
            collect_operand_uses(offset, used);
        }
        Instruction::Cast { src, .. } => {
            collect_operand_uses(src, used);
        }
        Instruction::Copy { src, .. } => {
            collect_operand_uses(src, used);
        }
        Instruction::GlobalAddr { .. } => {}
        Instruction::Memcpy { dest, src, .. } => {
            used.insert(dest.0);
            used.insert(src.0);
        }
    }
}

/// Record all values used by a terminator.
fn collect_terminator_uses(term: &Terminator, used: &mut HashSet<u32>) {
    match term {
        Terminator::Return(Some(val)) => {
            collect_operand_uses(val, used);
        }
        Terminator::Return(None) => {}
        Terminator::Branch(_) => {}
        Terminator::CondBranch { cond, .. } => {
            collect_operand_uses(cond, used);
        }
        Terminator::Unreachable => {}
    }
}

/// Record value uses from an operand.
fn collect_operand_uses(op: &Operand, used: &mut HashSet<u32>) {
    if let Operand::Value(v) = op {
        used.insert(v.0);
    }
}

/// Check if an instruction has side effects (must not be removed).
fn has_side_effects(inst: &Instruction) -> bool {
    matches!(inst,
        Instruction::Store { .. } |
        Instruction::Call { .. } |
        Instruction::CallIndirect { .. } |
        Instruction::Memcpy { .. }
    )
}

/// Get the destination value of an instruction, if any.
fn get_dest(inst: &Instruction) -> Option<Value> {
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
        Instruction::Memcpy { .. } => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::types::IrType;

    fn make_simple_func() -> IrFunction {
        // Function with: %0 = alloca i32, %1 = add 3, 4 (dead), store 42 to %0, load from %0
        let mut func = IrFunction::new("test".to_string(), IrType::I32, vec![], false);
        func.blocks.push(BasicBlock {
            label: "entry".to_string(),
            instructions: vec![
                Instruction::Alloca { dest: Value(0), ty: IrType::I32, size: 4 },
                // Dead instruction: result %1 is never used
                Instruction::BinOp {
                    dest: Value(1),
                    op: IrBinOp::Add,
                    lhs: Operand::Const(IrConst::I32(3)),
                    rhs: Operand::Const(IrConst::I32(4)),
                    ty: IrType::I32,
                },
                Instruction::Store {
                    val: Operand::Const(IrConst::I32(42)),
                    ptr: Value(0),
                    ty: IrType::I32,
                },
                Instruction::Load {
                    dest: Value(2),
                    ptr: Value(0),
                    ty: IrType::I32,
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(2)))),
        });
        func
    }

    #[test]
    fn test_collect_used_values() {
        let func = make_simple_func();
        let used = collect_used_values(&func);
        // Value(0) is used by Store and Load
        assert!(used.contains(&0));
        // Value(1) is dead (not used anywhere)
        assert!(!used.contains(&1));
        // Value(2) is used by Return
        assert!(used.contains(&2));
    }

    #[test]
    fn test_eliminate_dead_binop() {
        let mut func = make_simple_func();
        let removed = eliminate_dead_code(&mut func);
        assert_eq!(removed, 1); // The dead BinOp should be removed
        // Verify the remaining instructions
        assert_eq!(func.blocks[0].instructions.len(), 3); // alloca, store, load
    }

    #[test]
    fn test_side_effects_preserved() {
        // Calls should never be removed even if result is unused
        let mut func = IrFunction::new("test".to_string(), IrType::Void, vec![], false);
        func.blocks.push(BasicBlock {
            label: "entry".to_string(),
            instructions: vec![
                Instruction::Call {
                    dest: Some(Value(0)),
                    func: "printf".to_string(),
                    args: vec![],
                    arg_types: vec![],
                    return_type: IrType::I32,
                },
            ],
            terminator: Terminator::Return(None),
        });
        let removed = eliminate_dead_code(&mut func);
        assert_eq!(removed, 0); // Call should not be removed
    }
}
