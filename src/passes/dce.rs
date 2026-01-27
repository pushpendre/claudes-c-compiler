//! Dead Code Elimination (DCE) pass.
//!
//! Removes instructions whose results are never used by any other instruction
//! or terminator. This is a backwards dataflow analysis: we build use-counts
//! for all values, then remove instructions that define unused values and
//! transitively propagate the removal via a worklist.
//!
//! Side-effecting instructions (stores, calls) are never removed.
//!
//! Performance: Uses a use-count-based worklist approach instead of a fixpoint
//! loop. This processes each instruction at most twice (once for counting, once
//! for removal), giving O(n) complexity instead of O(n*k) where k is the
//! maximum dead chain length.

use crate::ir::ir::*;

/// Run dead code elimination on the entire module.
/// Returns the number of instructions removed.
pub fn run(module: &mut IrModule) -> usize {
    module.for_each_function(eliminate_dead_code)
}

/// Eliminate dead code in a single function using use-count-based worklist DCE.
///
/// Algorithm:
/// 1. Build use-counts for all values (single pass)
/// 2. Build a def-map: value_id -> (block_idx, inst_idx) for non-side-effecting instructions
/// 3. Find initially-dead instructions (non-side-effecting, dest use-count == 0)
/// 4. Process worklist: for each dead instruction, decrement operands' use-counts;
///    if any drops to 0, add its defining instruction to the worklist
/// 5. Sweep: remove all dead instructions in a single pass
pub(crate) fn eliminate_dead_code(func: &mut IrFunction) -> usize {
    let max_id = func.max_value_id() as usize;
    if max_id == 0 && func.blocks.len() <= 1 {
        // Tiny function, use the simple path
        return eliminate_dead_code_simple(func, max_id);
    }

    // Step 1: Build use-counts for all values.
    let mut use_count: Vec<u32> = vec![0; max_id + 1];
    for block in &func.blocks {
        for inst in &block.instructions {
            for_each_instruction_use(inst, |id| {
                let idx = id as usize;
                if idx < use_count.len() {
                    use_count[idx] += 1;
                }
            });
        }
        for_each_terminator_use(&block.terminator, |id| {
            let idx = id as usize;
            if idx < use_count.len() {
                use_count[idx] += 1;
            }
        });
    }

    // Step 2: Build def-map and identify initially-dead instructions.
    // dead[block_idx][inst_idx] = true means instruction should be removed.
    let mut dead: Vec<Vec<bool>> = func.blocks.iter()
        .map(|b| vec![false; b.instructions.len()])
        .collect();

    // Map from value_id -> (block_idx, inst_idx) for removable (non-side-effecting) defs.
    let mut def_loc: Vec<(u32, u32)> = vec![(u32::MAX, u32::MAX); max_id + 1];

    // Worklist of dead instructions to process (block_idx, inst_idx).
    let mut worklist: Vec<(u32, u32)> = Vec::new();

    for (bi, block) in func.blocks.iter().enumerate() {
        for (ii, inst) in block.instructions.iter().enumerate() {
            if has_side_effects(inst) {
                continue;
            }
            if let Some(dest) = inst.dest() {
                let id = dest.0 as usize;
                if id <= max_id {
                    def_loc[id] = (bi as u32, ii as u32);
                    if use_count[id] == 0 {
                        dead[bi][ii] = true;
                        worklist.push((bi as u32, ii as u32));
                    }
                }
            }
        }
    }

    // Step 3: Process worklist - transitively remove dead chains.
    while let Some((bi, ii)) = worklist.pop() {
        let inst = &func.blocks[bi as usize].instructions[ii as usize];
        // Decrement use-counts of this instruction's operands.
        for_each_instruction_use(inst, |id| {
            let idx = id as usize;
            if idx < use_count.len() {
                use_count[idx] = use_count[idx].saturating_sub(1);
                if use_count[idx] == 0 {
                    let (dbi, dii) = def_loc[idx];
                    if dbi != u32::MAX && !dead[dbi as usize][dii as usize] {
                        dead[dbi as usize][dii as usize] = true;
                        worklist.push((dbi, dii));
                    }
                }
            }
        });
    }

    // Step 4: Sweep - remove all dead instructions.
    let mut total = 0;
    for (bi, block) in func.blocks.iter_mut().enumerate() {
        let dead_flags = &dead[bi];
        let original_len = block.instructions.len();

        // Count dead instructions
        let dead_count = dead_flags.iter().filter(|&&d| d).count();
        if dead_count == 0 {
            continue;
        }

        let has_spans = !block.source_spans.is_empty();
        if has_spans {
            let mut write_idx = 0;
            for read_idx in 0..original_len {
                if !dead_flags[read_idx] {
                    if write_idx != read_idx {
                        block.instructions.swap(write_idx, read_idx);
                        block.source_spans.swap(write_idx, read_idx);
                    }
                    write_idx += 1;
                }
            }
            block.instructions.truncate(write_idx);
            block.source_spans.truncate(write_idx);
        } else {
            let mut idx = 0;
            block.instructions.retain(|_| {
                let keep = !dead_flags[idx];
                idx += 1;
                keep
            });
        }
        total += dead_count;
    }

    total
}

/// Simple fixpoint DCE for very small functions (avoids overhead of def-map + worklist).
fn eliminate_dead_code_simple(func: &mut IrFunction, max_id: usize) -> usize {
    let mut used = vec![false; max_id + 1];
    let mut total = 0;
    loop {
        for slot in used.iter_mut() {
            *slot = false;
        }
        for block in &func.blocks {
            for inst in &block.instructions {
                for_each_instruction_use(inst, |id| {
                    let idx = id as usize;
                    if idx < used.len() {
                        used[idx] = true;
                    }
                });
            }
            for_each_terminator_use(&block.terminator, |id| {
                let idx = id as usize;
                if idx < used.len() {
                    used[idx] = true;
                }
            });
        }

        let mut removed = 0;
        for block in &mut func.blocks {
            let original_len = block.instructions.len();
            let has_spans = !block.source_spans.is_empty();

            if has_spans {
                let mut write_idx = 0;
                for read_idx in 0..block.instructions.len() {
                    let keep = is_live(&block.instructions[read_idx], &used);
                    if keep {
                        if write_idx != read_idx {
                            block.instructions.swap(write_idx, read_idx);
                            block.source_spans.swap(write_idx, read_idx);
                        }
                        write_idx += 1;
                    }
                }
                block.instructions.truncate(write_idx);
                block.source_spans.truncate(write_idx);
            } else {
                block.instructions.retain(|inst| is_live(inst, &used));
            }
            removed += original_len - block.instructions.len();
        }

        if removed == 0 {
            break;
        }
        total += removed;
    }
    total
}

/// Check if an instruction is live (should be retained).
#[inline]
fn is_live(inst: &Instruction, used: &[bool]) -> bool {
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
}

// ==========================================================================
// Unified instruction/terminator use-visitor
// ==========================================================================
//
// All use-analysis in DCE (use-counting, bitvector marking, decrementing)
// goes through these two visitor functions. This ensures a single match
// block per IR structure (Instruction and Terminator), avoiding the
// maintenance burden of keeping multiple match blocks in sync.

/// Call `f(value_id)` for every Value ID used as an operand in `inst`.
#[inline]
fn for_each_instruction_use(inst: &Instruction, mut f: impl FnMut(u32)) {
    #[inline(always)]
    fn visit_operand(op: &Operand, f: &mut impl FnMut(u32)) {
        if let Operand::Value(v) = op {
            f(v.0);
        }
    }

    match inst {
        Instruction::Alloca { .. } => {}
        Instruction::DynAlloca { size, .. } => {
            visit_operand(size, &mut f);
        }
        Instruction::Store { val, ptr, .. } => {
            visit_operand(val, &mut f);
            f(ptr.0);
        }
        Instruction::Load { ptr, .. } => {
            f(ptr.0);
        }
        Instruction::BinOp { lhs, rhs, .. } => {
            visit_operand(lhs, &mut f);
            visit_operand(rhs, &mut f);
        }
        Instruction::UnaryOp { src, .. } => {
            visit_operand(src, &mut f);
        }
        Instruction::Cmp { lhs, rhs, .. } => {
            visit_operand(lhs, &mut f);
            visit_operand(rhs, &mut f);
        }
        Instruction::Call { args, .. } => {
            for arg in args {
                visit_operand(arg, &mut f);
            }
        }
        Instruction::CallIndirect { func_ptr, args, .. } => {
            visit_operand(func_ptr, &mut f);
            for arg in args {
                visit_operand(arg, &mut f);
            }
        }
        Instruction::GetElementPtr { base, offset, .. } => {
            f(base.0);
            visit_operand(offset, &mut f);
        }
        Instruction::Cast { src, .. } => {
            visit_operand(src, &mut f);
        }
        Instruction::Copy { src, .. } => {
            visit_operand(src, &mut f);
        }
        Instruction::GlobalAddr { .. } => {}
        Instruction::Memcpy { dest, src, .. } => {
            f(dest.0);
            f(src.0);
        }
        Instruction::VaArg { va_list_ptr, .. } => {
            f(va_list_ptr.0);
        }
        Instruction::VaStart { va_list_ptr } => {
            f(va_list_ptr.0);
        }
        Instruction::VaEnd { va_list_ptr } => {
            f(va_list_ptr.0);
        }
        Instruction::VaCopy { dest_ptr, src_ptr } => {
            f(dest_ptr.0);
            f(src_ptr.0);
        }
        Instruction::AtomicRmw { ptr, val, .. } => {
            visit_operand(ptr, &mut f);
            visit_operand(val, &mut f);
        }
        Instruction::AtomicCmpxchg { ptr, expected, desired, .. } => {
            visit_operand(ptr, &mut f);
            visit_operand(expected, &mut f);
            visit_operand(desired, &mut f);
        }
        Instruction::AtomicLoad { ptr, .. } => {
            visit_operand(ptr, &mut f);
        }
        Instruction::AtomicStore { ptr, val, .. } => {
            visit_operand(ptr, &mut f);
            visit_operand(val, &mut f);
        }
        Instruction::Fence { .. } => {}
        Instruction::Phi { incoming, .. } => {
            for (op, _label) in incoming {
                visit_operand(op, &mut f);
            }
        }
        Instruction::Select { cond, true_val, false_val, .. } => {
            visit_operand(cond, &mut f);
            visit_operand(true_val, &mut f);
            visit_operand(false_val, &mut f);
        }
        Instruction::LabelAddr { .. } => {}
        Instruction::GetReturnF64Second { .. } => {}
        Instruction::GetReturnF32Second { .. } => {}
        Instruction::GetReturnF128Second { .. } => {}
        Instruction::SetReturnF64Second { src } => {
            visit_operand(src, &mut f);
        }
        Instruction::SetReturnF32Second { src } => {
            visit_operand(src, &mut f);
        }
        Instruction::SetReturnF128Second { src } => {
            visit_operand(src, &mut f);
        }
        Instruction::InlineAsm { outputs, inputs, .. } => {
            for (_, ptr, _) in outputs {
                f(ptr.0);
            }
            for (_, op, _) in inputs {
                visit_operand(op, &mut f);
            }
        }
        Instruction::Intrinsic { dest_ptr, args, .. } => {
            if let Some(ptr) = dest_ptr {
                f(ptr.0);
            }
            for arg in args {
                visit_operand(arg, &mut f);
            }
        }
        Instruction::StackSave { .. } => {}
        Instruction::StackRestore { ptr } => {
            f(ptr.0);
        }
        Instruction::ParamRef { .. } => {}
    }
}

/// Call `f(value_id)` for every Value ID used as an operand in a terminator.
#[inline]
fn for_each_terminator_use(term: &Terminator, mut f: impl FnMut(u32)) {
    #[inline(always)]
    fn visit_operand(op: &Operand, f: &mut impl FnMut(u32)) {
        if let Operand::Value(v) = op {
            f(v.0);
        }
    }

    match term {
        Terminator::Return(Some(val)) => visit_operand(val, &mut f),
        Terminator::Return(None) => {}
        Terminator::Branch(_) => {}
        Terminator::CondBranch { cond, .. } => visit_operand(cond, &mut f),
        Terminator::IndirectBranch { target, .. } => visit_operand(target, &mut f),
        Terminator::Switch { val, .. } => visit_operand(val, &mut f),
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
        Instruction::GetReturnF128Second { .. } |
        Instruction::SetReturnF64Second { .. } |
        Instruction::SetReturnF32Second { .. } |
        Instruction::SetReturnF128Second { .. } |
        Instruction::InlineAsm { .. } |
        // StackRestore modifies the stack pointer at runtime - must not be removed.
        // StackSave is kept alive by its use in StackRestore (normal DCE liveness).
        Instruction::StackRestore { .. }
    ) || matches!(inst, Instruction::Intrinsic { op, .. } if !op.is_pure())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::types::{AddressSpace, IrType};

    fn make_simple_func() -> IrFunction {
        // Function with: %0 = alloca i32, %1 = add 3, 4 (dead), store 42 to %0, load from %0
        let mut func = IrFunction::new("test".to_string(), IrType::I32, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Alloca { dest: Value(0), ty: IrType::I32, size: 4, align: 0, volatile: false },
                // Dead instruction: result %1 is never used
                Instruction::BinOp {
                    dest: Value(1),
                    op: IrBinOp::Add,
                    lhs: Operand::Const(IrConst::I32(3)),
                    rhs: Operand::Const(IrConst::I32(4)),
                    ty: IrType::I32,
                },
                Instruction::Store { val: Operand::Const(IrConst::I32(42)), ptr: Value(0), ty: IrType::I32, seg_override: AddressSpace::Default },
                Instruction::Load { dest: Value(2), ptr: Value(0), ty: IrType::I32, seg_override: AddressSpace::Default },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(2)))),
            source_spans: Vec::new(),
        });
        func
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
                    struct_arg_classes: Vec::new(),
                },
            ],
            terminator: Terminator::Return(None),
            source_spans: Vec::new(),
        });
        let removed = eliminate_dead_code(&mut func);
        assert_eq!(removed, 0); // Call should not be removed
    }

    #[test]
    fn test_transitive_dead_chain() {
        // %0 = alloca i32  (side-effecting, kept)
        // %1 = add 1, 2  (dead, only used by %2)
        // %2 = add %1, 3  (dead, only used by %3)
        // %3 = add %2, 4  (dead, not used at all)
        // return void
        // All of %1, %2, %3 should be removed in a single pass.
        let mut func = IrFunction::new("test".to_string(), IrType::Void, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Alloca { dest: Value(0), ty: IrType::I32, size: 4, align: 0, volatile: false },
                Instruction::BinOp {
                    dest: Value(1),
                    op: IrBinOp::Add,
                    lhs: Operand::Const(IrConst::I32(1)),
                    rhs: Operand::Const(IrConst::I32(2)),
                    ty: IrType::I32,
                },
                Instruction::BinOp {
                    dest: Value(2),
                    op: IrBinOp::Add,
                    lhs: Operand::Value(Value(1)),
                    rhs: Operand::Const(IrConst::I32(3)),
                    ty: IrType::I32,
                },
                Instruction::BinOp {
                    dest: Value(3),
                    op: IrBinOp::Add,
                    lhs: Operand::Value(Value(2)),
                    rhs: Operand::Const(IrConst::I32(4)),
                    ty: IrType::I32,
                },
            ],
            terminator: Terminator::Return(None),
            source_spans: Vec::new(),
        });

        let removed = eliminate_dead_code(&mut func);
        assert_eq!(removed, 3); // All three dead BinOps should be removed
        assert_eq!(func.blocks[0].instructions.len(), 1); // Only alloca remains
    }
}
