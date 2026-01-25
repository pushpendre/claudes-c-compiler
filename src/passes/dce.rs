//! Dead Code Elimination (DCE) pass.
//!
//! Removes instructions whose results are never used by any other instruction
//! or terminator. This is a backwards dataflow analysis: we mark all values
//! that are live (used somewhere), then remove instructions that define
//! dead values.
//!
//! Side-effecting instructions (stores, calls) are never removed.
//!
//! Performance: Uses a flat bitvector indexed by Value ID instead of a HashSet,
//! since Value IDs are dense sequential u32s. This gives O(1) set/check with
//! no hashing overhead and excellent cache locality.

use crate::ir::ir::*;

/// Run dead code elimination on the entire module.
/// Returns the number of instructions removed.
pub fn run(module: &mut IrModule) -> usize {
    module.for_each_function(eliminate_dead_code)
}

/// Eliminate dead code in a single function.
/// Iterates until no more dead code is found (fixpoint).
fn eliminate_dead_code(func: &mut IrFunction) -> usize {
    let max_id = func.max_value_id() as usize;
    // Allocate the bitvector once and reuse across fixpoint iterations
    let mut used = vec![false; max_id + 1];
    let mut total = 0;
    loop {
        // Clear for this iteration
        for slot in used.iter_mut() {
            *slot = false;
        }
        collect_used_values(func, &mut used);

        let mut removed = 0;
        for block in &mut func.blocks {
            let original_len = block.instructions.len();
            block.instructions.retain(|inst| {
                if has_side_effects(inst) {
                    return true;
                }
                match inst.dest() {
                    Some(dest) => {
                        let id = dest.0 as usize;
                        id < used.len() && used[id]
                    }
                    None => true,
                }
            });
            removed += original_len - block.instructions.len();
        }

        if removed == 0 {
            break;
        }
        total += removed;
    }
    total
}

/// Collect all Value IDs that are used in the function into a bitvector.
fn collect_used_values(func: &IrFunction, used: &mut [bool]) {
    for block in &func.blocks {
        for inst in &block.instructions {
            collect_instruction_uses(inst, used);
        }
        collect_terminator_uses(&block.terminator, used);
    }
}

/// Mark a value as used in the bitvector.
#[inline(always)]
fn mark_used(used: &mut [bool], id: u32) {
    let idx = id as usize;
    if idx < used.len() {
        used[idx] = true;
    }
}

/// Mark all values used by an operand.
#[inline(always)]
fn mark_operand_used(op: &Operand, used: &mut [bool]) {
    if let Operand::Value(v) = op {
        mark_used(used, v.0);
    }
}

/// Record all values used by an instruction.
fn collect_instruction_uses(inst: &Instruction, used: &mut [bool]) {
    match inst {
        Instruction::Alloca { .. } => {}
        Instruction::DynAlloca { size, .. } => {
            mark_operand_used(size, used);
        }
        Instruction::Store { val, ptr, .. } => {
            mark_operand_used(val, used);
            mark_used(used, ptr.0);
        }
        Instruction::Load { ptr, .. } => {
            mark_used(used, ptr.0);
        }
        Instruction::BinOp { lhs, rhs, .. } => {
            mark_operand_used(lhs, used);
            mark_operand_used(rhs, used);
        }
        Instruction::UnaryOp { src, .. } => {
            mark_operand_used(src, used);
        }
        Instruction::Cmp { lhs, rhs, .. } => {
            mark_operand_used(lhs, used);
            mark_operand_used(rhs, used);
        }
        Instruction::Call { args, .. } => {
            for arg in args {
                mark_operand_used(arg, used);
            }
        }
        Instruction::CallIndirect { func_ptr, args, .. } => {
            mark_operand_used(func_ptr, used);
            for arg in args {
                mark_operand_used(arg, used);
            }
        }
        Instruction::GetElementPtr { base, offset, .. } => {
            mark_used(used, base.0);
            mark_operand_used(offset, used);
        }
        Instruction::Cast { src, .. } => {
            mark_operand_used(src, used);
        }
        Instruction::Copy { src, .. } => {
            mark_operand_used(src, used);
        }
        Instruction::GlobalAddr { .. } => {}
        Instruction::Memcpy { dest, src, .. } => {
            mark_used(used, dest.0);
            mark_used(used, src.0);
        }
        Instruction::VaArg { va_list_ptr, .. } => {
            mark_used(used, va_list_ptr.0);
        }
        Instruction::VaStart { va_list_ptr } => {
            mark_used(used, va_list_ptr.0);
        }
        Instruction::VaEnd { va_list_ptr } => {
            mark_used(used, va_list_ptr.0);
        }
        Instruction::VaCopy { dest_ptr, src_ptr } => {
            mark_used(used, dest_ptr.0);
            mark_used(used, src_ptr.0);
        }
        Instruction::AtomicRmw { ptr, val, .. } => {
            mark_operand_used(ptr, used);
            mark_operand_used(val, used);
        }
        Instruction::AtomicCmpxchg { ptr, expected, desired, .. } => {
            mark_operand_used(ptr, used);
            mark_operand_used(expected, used);
            mark_operand_used(desired, used);
        }
        Instruction::AtomicLoad { ptr, .. } => {
            mark_operand_used(ptr, used);
        }
        Instruction::AtomicStore { ptr, val, .. } => {
            mark_operand_used(ptr, used);
            mark_operand_used(val, used);
        }
        Instruction::Fence { .. } => {}
        Instruction::Phi { incoming, .. } => {
            for (op, _label) in incoming {
                mark_operand_used(op, used);
            }
        }
        Instruction::LabelAddr { .. } => {}
        Instruction::GetReturnF64Second { .. } => {}
        Instruction::GetReturnF32Second { .. } => {}
        Instruction::SetReturnF64Second { src } => {
            mark_operand_used(src, used);
        }
        Instruction::SetReturnF32Second { src } => {
            mark_operand_used(src, used);
        }
        Instruction::InlineAsm { outputs, inputs, .. } => {
            for (_, ptr, _) in outputs {
                mark_used(used, ptr.0);
            }
            for (_, op, _) in inputs {
                mark_operand_used(op, used);
            }
        }
        Instruction::Intrinsic { dest_ptr, args, .. } => {
            if let Some(ptr) = dest_ptr {
                mark_used(used, ptr.0);
            }
            for arg in args {
                mark_operand_used(arg, used);
            }
        }
    }
}

/// Record all values used by a terminator.
fn collect_terminator_uses(term: &Terminator, used: &mut [bool]) {
    match term {
        Terminator::Return(Some(val)) => {
            mark_operand_used(val, used);
        }
        Terminator::Return(None) => {}
        Terminator::Branch(_) => {}
        Terminator::CondBranch { cond, .. } => {
            mark_operand_used(cond, used);
        }
        Terminator::IndirectBranch { target, .. } => {
            mark_operand_used(target, used);
        }
        Terminator::Unreachable => {}
    }
}

/// Check if an instruction has side effects (must not be removed).
fn has_side_effects(inst: &Instruction) -> bool {
    matches!(inst,
        // Alloca must never be removed: codegen uses positional indexing
        // (find_param_alloca) to map function parameters to their stack slots.
        // Removing unused parameter allocas shifts indices and causes miscompilation.
        Instruction::Alloca { .. } |
        // DynAlloca modifies the stack pointer at runtime - always has side effects
        Instruction::DynAlloca { .. } |
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
        Instruction::GetReturnF64Second { .. } |
        Instruction::GetReturnF32Second { .. } |
        Instruction::SetReturnF64Second { .. } |
        Instruction::SetReturnF32Second { .. } |
        Instruction::InlineAsm { .. } |
        Instruction::Intrinsic { .. }
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
            label: BlockId(0),
            instructions: vec![
                Instruction::Alloca { dest: Value(0), ty: IrType::I32, size: 4, align: 0 },
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
        let max_id = func.max_value_id() as usize;
        let mut used = vec![false; max_id + 1];
        collect_used_values(&func, &mut used);
        // Value(0) is used by Store and Load
        assert!(used[0]);
        // Value(1) is dead (not used anywhere)
        assert!(!used[1]);
        // Value(2) is used by Return
        assert!(used[2]);
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
            label: BlockId(0),
            instructions: vec![
                Instruction::Call {
                    dest: Some(Value(0)),
                    func: "printf".to_string(),
                    args: vec![],
                    arg_types: vec![],
                    return_type: IrType::I32,
                    is_variadic: true,
                    num_fixed_args: 0,
                    struct_arg_sizes: vec![],
                },
            ],
            terminator: Terminator::Return(None),
        });
        let removed = eliminate_dead_code(&mut func);
        assert_eq!(removed, 0); // Call should not be removed
    }
}
