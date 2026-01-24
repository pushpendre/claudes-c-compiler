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
    module.for_each_function(eliminate_dead_code)
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
            match inst.dest() {
                Some(dest) => used_values.contains(&dest.0),
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
        Instruction::VaArg { va_list_ptr, .. } => {
            used.insert(va_list_ptr.0);
        }
        Instruction::VaStart { va_list_ptr } => {
            used.insert(va_list_ptr.0);
        }
        Instruction::VaEnd { va_list_ptr } => {
            used.insert(va_list_ptr.0);
        }
        Instruction::VaCopy { dest_ptr, src_ptr } => {
            used.insert(dest_ptr.0);
            used.insert(src_ptr.0);
        }
        Instruction::AtomicRmw { ptr, val, .. } => {
            collect_operand_uses(ptr, used);
            collect_operand_uses(val, used);
        }
        Instruction::AtomicCmpxchg { ptr, expected, desired, .. } => {
            collect_operand_uses(ptr, used);
            collect_operand_uses(expected, used);
            collect_operand_uses(desired, used);
        }
        Instruction::AtomicLoad { ptr, .. } => {
            collect_operand_uses(ptr, used);
        }
        Instruction::AtomicStore { ptr, val, .. } => {
            collect_operand_uses(ptr, used);
            collect_operand_uses(val, used);
        }
        Instruction::Fence { .. } => {}
        Instruction::Phi { incoming, .. } => {
            for (op, _label) in incoming {
                collect_operand_uses(op, used);
            }
        }
        Instruction::LabelAddr { .. } => {}
        Instruction::InlineAsm { outputs, inputs, .. } => {
            for (_, ptr, _) in outputs {
                used.insert(ptr.0);
            }
            for (_, op, _) in inputs {
                collect_operand_uses(op, used);
            }
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
        Terminator::IndirectBranch { target, .. } => {
            collect_operand_uses(target, used);
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
        // Alloca must never be removed: codegen uses positional indexing
        // (find_param_alloca) to map function parameters to their stack slots.
        // Removing unused parameter allocas shifts indices and causes miscompilation.
        Instruction::Alloca { .. } |
        Instruction::Store { .. } |
        Instruction::Call { .. } |
        Instruction::CallIndirect { .. } |
        Instruction::Memcpy { .. } |
        Instruction::VaStart { .. } |
        Instruction::VaEnd { .. } |
        Instruction::VaCopy { .. } |
        Instruction::VaArg { .. } |
        Instruction::AtomicRmw { .. } |
        Instruction::AtomicCmpxchg { .. } |
        Instruction::AtomicLoad { .. } |
        Instruction::AtomicStore { .. } |
        Instruction::Fence { .. } |
        Instruction::InlineAsm { .. }
    )
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
                    is_variadic: true,
                    num_fixed_args: 0,
                },
            ],
            terminator: Terminator::Return(None),
        });
        let removed = eliminate_dead_code(&mut func);
        assert_eq!(removed, 0); // Call should not be removed
    }
}
