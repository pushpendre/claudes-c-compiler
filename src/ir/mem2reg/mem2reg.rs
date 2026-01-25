//! mem2reg: Promote stack allocas to SSA registers with phi insertion.
//!
//! This implements the standard SSA construction algorithm:
//! 1. Identify promotable allocas (scalar, only loaded/stored, not address-taken)
//! 2. Build CFG and compute dominator tree (Cooper-Harvey-Kennedy algorithm)
//! 3. Compute dominance frontiers
//! 4. Insert phi nodes at iterated dominance frontiers of defining blocks
//! 5. Rename variables via dominator tree DFS
//!
//! Reference: "A Simple, Fast Dominance Algorithm" by Cooper, Harvey, Kennedy (2001)

use crate::common::fx_hash::{FxHashMap, FxHashSet};
use std::collections::VecDeque;
use crate::ir::ir::*;
use crate::ir::analysis;
use crate::common::types::IrType;

/// Promote allocas to SSA form with phi insertion.
/// Only promotes scalar allocas that are exclusively loaded/stored
/// (not address-taken by GEP, memcpy, va_start, etc.).
pub fn promote_allocas(module: &mut IrModule) {
    for func in &mut module.functions {
        if func.is_declaration || func.blocks.is_empty() {
            continue;
        }
        promote_function(func);
    }
}

/// Information about a promotable alloca.
struct AllocaInfo {
    /// The Value that is the alloca's destination (pointer to the stack slot).
    alloca_value: Value,
    /// The IR type of the stored value.
    ty: IrType,
    /// Block indices where this alloca is stored to (defining blocks).
    def_blocks: FxHashSet<usize>,
    /// Block indices where this alloca is loaded from (use blocks).
    use_blocks: FxHashSet<usize>,
}

/// Promote allocas in a single function to SSA form.
fn promote_function(func: &mut IrFunction) {
    if func.blocks.is_empty() {
        return;
    }

    // Step 1: Identify promotable allocas
    let alloca_infos = find_promotable_allocas(func);
    if alloca_infos.is_empty() {
        return;
    }

    // Step 2: Build CFG
    let num_blocks = func.blocks.len();
    let label_to_idx = analysis::build_label_map(func);
    let (preds, succs) = analysis::build_cfg(func, &label_to_idx);

    // Step 3: Compute dominator tree
    let idom = analysis::compute_dominators(num_blocks, &preds, &succs);

    // Step 4: Compute dominance frontiers
    let df = analysis::compute_dominance_frontiers(num_blocks, &preds, &idom);

    // Step 5: Insert phi nodes
    let phi_locations = insert_phis(&alloca_infos, &df, num_blocks);

    // Step 6: Rename variables
    let dom_children = analysis::build_dom_tree_children(num_blocks, &idom);
    rename_variables(func, &alloca_infos, &phi_locations, &dom_children, &preds, &label_to_idx);
}

/// Find all allocas that can be promoted to SSA registers.
/// A promotable alloca must:
/// - Be in the entry block
/// - Have scalar type (not used for arrays/structs via size > 8)
/// - Only be used by Load and Store instructions (not address-taken)
fn find_promotable_allocas(func: &IrFunction) -> Vec<AllocaInfo> {
    let num_params = func.params.len();

    // Collect all allocas from the entry block, skipping the first N (parameter allocas).
    // Parameter allocas receive their initial values from the backend (register stores)
    // which are not visible in the IR, so they must not be promoted.
    let mut alloca_index = 0;
    let entry_allocas: Vec<(Value, IrType, usize)> = func.blocks[0]
        .instructions
        .iter()
        .filter_map(|inst| {
            if let Instruction::Alloca { dest, ty, size, volatile, .. } = inst {
                let idx = alloca_index;
                alloca_index += 1;
                // Skip parameter allocas (first num_params allocas)
                if idx < num_params {
                    return None;
                }
                // Never promote volatile allocas -- volatile locals must remain
                // in memory so their values survive setjmp/longjmp and are not
                // cached in registers that longjmp would restore to stale values.
                if *volatile {
                    return None;
                }
                // Only promote scalar allocas (size <= 8 bytes)
                // Larger allocas are for arrays/structs passed by value
                let type_size = ir_type_size(*ty);
                if *size <= type_size && type_size <= 8 {
                    Some((*dest, *ty, *size))
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect();

    if entry_allocas.is_empty() {
        return Vec::new();
    }

    // Build set of candidate alloca values
    let candidate_set: FxHashSet<u32> = entry_allocas.iter().map(|(v, _, _)| v.0).collect();

    // Check all uses: only Load and Store targeting the alloca pointer are allowed
    let mut disqualified: FxHashSet<u32> = FxHashSet::default();
    let mut def_blocks: FxHashMap<u32, FxHashSet<usize>> = FxHashMap::default();
    let mut use_blocks: FxHashMap<u32, FxHashSet<usize>> = FxHashMap::default();

    for (block_idx, block) in func.blocks.iter().enumerate() {
        for inst in &block.instructions {
            match inst {
                Instruction::Load { ptr, .. } => {
                    if candidate_set.contains(&ptr.0) && !disqualified.contains(&ptr.0) {
                        use_blocks.entry(ptr.0).or_default().insert(block_idx);
                    }
                }
                Instruction::Store { val, ptr, .. } => {
                    if candidate_set.contains(&ptr.0) && !disqualified.contains(&ptr.0) {
                        def_blocks.entry(ptr.0).or_default().insert(block_idx);
                    }
                    // If a candidate alloca value appears as the stored VALUE (not ptr),
                    // it means the alloca's address is being used as data (e.g., array-to-pointer
                    // decay: Y.p = local_array). This is an address-taken use and the alloca
                    // must not be promoted, since promotion would lose the stack address.
                    if let Operand::Value(v) = val {
                        if candidate_set.contains(&v.0) {
                            disqualified.insert(v.0);
                        }
                    }
                }
                // Any other use of the alloca value disqualifies it
                _ => {
                    for used_val in instruction_used_values(inst) {
                        if candidate_set.contains(&used_val) {
                            disqualified.insert(used_val);
                        }
                    }
                }
            }
        }

        // Check terminator uses
        for used_val in terminator_used_values(&block.terminator) {
            if candidate_set.contains(&used_val) {
                disqualified.insert(used_val);
            }
        }
    }

    // Build final list of promotable allocas
    entry_allocas
        .into_iter()
        .filter(|(v, _, _)| !disqualified.contains(&v.0))
        .map(|(alloca_value, ty, _)| {
            AllocaInfo {
                alloca_value,
                ty,
                def_blocks: def_blocks.remove(&alloca_value.0).unwrap_or_default(),
                use_blocks: use_blocks.remove(&alloca_value.0).unwrap_or_default(),
            }
        })
        // Only promote allocas that are actually used (have loads or stores)
        .filter(|info| !info.def_blocks.is_empty() || !info.use_blocks.is_empty())
        .collect()
}

/// Get all Value IDs used by an instruction (excluding Load ptr and Store ptr
/// which are handled specially for alloca tracking).
fn instruction_used_values(inst: &Instruction) -> Vec<u32> {
    let mut used = Vec::new();
    match inst {
        // Load ptr and Store ptr uses are handled by the caller.
        // Store val must still be tracked for disqualification.
        Instruction::Load { .. } | Instruction::Alloca { .. } => {}
        Instruction::Store { val, .. } => {
            add_operand_values(val, &mut used);
        }
        Instruction::DynAlloca { size, .. } => {
            add_operand_values(size, &mut used);
        }
        Instruction::BinOp { lhs, rhs, .. } => {
            add_operand_values(lhs, &mut used);
            add_operand_values(rhs, &mut used);
        }
        Instruction::UnaryOp { src, .. } => {
            add_operand_values(src, &mut used);
        }
        Instruction::Cmp { lhs, rhs, .. } => {
            add_operand_values(lhs, &mut used);
            add_operand_values(rhs, &mut used);
        }
        Instruction::Call { args, .. } => {
            for arg in args { add_operand_values(arg, &mut used); }
        }
        Instruction::CallIndirect { func_ptr, args, .. } => {
            add_operand_values(func_ptr, &mut used);
            for arg in args { add_operand_values(arg, &mut used); }
        }
        Instruction::GetElementPtr { base, offset, .. } => {
            used.push(base.0);
            add_operand_values(offset, &mut used);
        }
        Instruction::Cast { src, .. } => {
            add_operand_values(src, &mut used);
        }
        Instruction::Copy { src, .. } => {
            add_operand_values(src, &mut used);
        }
        Instruction::GlobalAddr { .. } => {}
        Instruction::Memcpy { dest, src, .. } => {
            used.push(dest.0);
            used.push(src.0);
        }
        Instruction::VaArg { va_list_ptr, .. } => {
            used.push(va_list_ptr.0);
        }
        Instruction::VaStart { va_list_ptr } => {
            used.push(va_list_ptr.0);
        }
        Instruction::VaEnd { va_list_ptr } => {
            used.push(va_list_ptr.0);
        }
        Instruction::VaCopy { dest_ptr, src_ptr } => {
            used.push(dest_ptr.0);
            used.push(src_ptr.0);
        }
        Instruction::AtomicRmw { ptr, val, .. } => {
            add_operand_values(ptr, &mut used);
            add_operand_values(val, &mut used);
        }
        Instruction::AtomicCmpxchg { ptr, expected, desired, .. } => {
            add_operand_values(ptr, &mut used);
            add_operand_values(expected, &mut used);
            add_operand_values(desired, &mut used);
        }
        Instruction::AtomicLoad { ptr, .. } => {
            add_operand_values(ptr, &mut used);
        }
        Instruction::AtomicStore { ptr, val, .. } => {
            add_operand_values(ptr, &mut used);
            add_operand_values(val, &mut used);
        }
        Instruction::Fence { .. } => {}
        Instruction::Phi { incoming, .. } => {
            for (op, _) in incoming {
                add_operand_values(op, &mut used);
            }
        }
        Instruction::LabelAddr { .. } => {}
        Instruction::GetReturnF64Second { .. } => {}
        Instruction::GetReturnF32Second { .. } => {}
        Instruction::SetReturnF64Second { src } => {
            add_operand_values(src, &mut used);
        }
        Instruction::SetReturnF32Second { src } => {
            add_operand_values(src, &mut used);
        }
        Instruction::InlineAsm { outputs, inputs, .. } => {
            for (_, ptr, _) in outputs {
                used.push(ptr.0);
            }
            for (_, op, _) in inputs {
                add_operand_values(op, &mut used);
            }
        }
        Instruction::Intrinsic { dest_ptr, args, .. } => {
            if let Some(ptr) = dest_ptr {
                used.push(ptr.0);
            }
            for arg in args {
                add_operand_values(arg, &mut used);
            }
        }
    }
    used
}

fn terminator_used_values(term: &Terminator) -> Vec<u32> {
    let mut used = Vec::new();
    match term {
        Terminator::Return(Some(op)) => add_operand_values(op, &mut used),
        Terminator::CondBranch { cond, .. } => add_operand_values(cond, &mut used),
        Terminator::IndirectBranch { target, .. } => add_operand_values(target, &mut used),
        _ => {}
    }
    used
}

fn add_operand_values(op: &Operand, used: &mut Vec<u32>) {
    if let Operand::Value(v) = op {
        used.push(v.0);
    }
}

/// Determine where phi nodes need to be inserted.
/// Returns a map: block_index -> set of alloca indices that need phis there.
fn insert_phis(
    alloca_infos: &[AllocaInfo],
    df: &[FxHashSet<usize>],
    num_blocks: usize,
) -> Vec<FxHashSet<usize>> {
    // phi_locations[block_idx] = set of alloca indices that need a phi at this block
    let mut phi_locations = vec![FxHashSet::default(); num_blocks];

    for (alloca_idx, info) in alloca_infos.iter().enumerate() {
        // Iterated dominance frontier algorithm
        let mut worklist: VecDeque<usize> = info.def_blocks.iter().copied().collect();
        let mut has_phi: FxHashSet<usize> = FxHashSet::default();
        let mut ever_in_worklist: FxHashSet<usize> = info.def_blocks.clone();

        while let Some(block) = worklist.pop_front() {
            for &df_block in &df[block] {
                if has_phi.insert(df_block) {
                    phi_locations[df_block].insert(alloca_idx);
                    if ever_in_worklist.insert(df_block) {
                        worklist.push_back(df_block);
                    }
                }
            }
        }
    }

    phi_locations
}


/// Rename variables to complete SSA construction.
/// This traverses the dominator tree, maintaining stacks of current definitions
/// for each promoted alloca, and rewrites loads/stores to use SSA values.
fn rename_variables(
    func: &mut IrFunction,
    alloca_infos: &[AllocaInfo],
    phi_locations: &[FxHashSet<usize>],
    dom_children: &[Vec<usize>],
    preds: &[Vec<usize>],
    label_to_idx: &FxHashMap<BlockId, usize>,
) {
    let num_allocas = alloca_infos.len();

    // Map alloca value -> alloca index for quick lookup
    let alloca_to_idx: FxHashMap<u32, usize> = alloca_infos
        .iter()
        .enumerate()
        .map(|(i, info)| (info.alloca_value.0, i))
        .collect();

    // Use cached next_value_id if available, otherwise scan
    let mut next_value = if func.next_value_id > 0 {
        func.next_value_id
    } else {
        func.max_value_id() + 1
    };

    // First, insert phi instructions at the appropriate blocks.
    // We do this before renaming so the phi dests get fresh values during rename.
    // For now, insert placeholder phis with empty incoming lists.
    // phi_dests[block_idx][alloca_idx] = the Value for the phi's dest (if there is one)
    let mut phi_dests: Vec<FxHashMap<usize, Value>> = vec![FxHashMap::default(); func.blocks.len()];

    for (block_idx, alloca_set) in phi_locations.iter().enumerate() {
        let mut phi_instructions = Vec::new();
        for &alloca_idx in alloca_set {
            let info = &alloca_infos[alloca_idx];
            let dest = Value(next_value);
            next_value += 1;
            phi_dests[block_idx].insert(alloca_idx, dest);
            phi_instructions.push(Instruction::Phi {
                dest,
                ty: info.ty,
                incoming: Vec::new(), // will be filled during rename
            });
        }
        // Prepend phis to block instructions
        if !phi_instructions.is_empty() {
            phi_instructions.append(&mut func.blocks[block_idx].instructions);
            func.blocks[block_idx].instructions = phi_instructions;
        }
    }

    // Rename using dominator tree DFS
    // def_stacks[alloca_idx] = stack of current SSA value for that alloca
    let mut def_stacks: Vec<Vec<Operand>> = vec![Vec::new(); num_allocas];

    // Initialize with undef (zero constant of appropriate type) for allocas
    // that might be read before being written
    for (i, info) in alloca_infos.iter().enumerate() {
        def_stacks[i].push(Operand::Const(IrConst::zero(info.ty)));
    }

    rename_block(
        0,
        func,
        &alloca_to_idx,
        alloca_infos,
        &mut def_stacks,
        &mut next_value,
        &phi_dests,
        dom_children,
        preds,
        label_to_idx,
    );

    // Remove promoted allocas from the entry block, and remove dead loads/stores
    remove_promoted_instructions(func, &alloca_to_idx);

    // Update cached next_value_id for downstream passes
    func.next_value_id = next_value;
}

/// Recursive dominator-tree DFS for variable renaming.
fn rename_block(
    block_idx: usize,
    func: &mut IrFunction,
    alloca_to_idx: &FxHashMap<u32, usize>,
    alloca_infos: &[AllocaInfo],
    def_stacks: &mut [Vec<Operand>],
    next_value: &mut u32,
    phi_dests: &[FxHashMap<usize, Value>],
    dom_children: &[Vec<usize>],
    preds: &[Vec<usize>],
    label_to_idx: &FxHashMap<BlockId, usize>,
) {
    // Record stack depths so we can pop on exit
    let stack_depths: Vec<usize> = def_stacks.iter().map(|s| s.len()).collect();

    // Process phi nodes in this block - push their dests onto stacks
    for inst in &func.blocks[block_idx].instructions {
        if let Instruction::Phi { dest, .. } = inst {
            // Find which alloca this phi is for
            if let Some(&alloca_idx) = phi_dests[block_idx]
                .iter()
                .find(|(_, &v)| v == *dest)
                .map(|(idx, _)| idx)
            {
                def_stacks[alloca_idx].push(Operand::Value(*dest));
            }
        }
    }

    // Rewrite instructions in this block
    let mut new_instructions = Vec::with_capacity(func.blocks[block_idx].instructions.len());
    for inst in func.blocks[block_idx].instructions.drain(..) {
        match inst {
            Instruction::Load { dest, ptr, ty } => {
                if let Some(&alloca_idx) = alloca_to_idx.get(&ptr.0) {
                    // Replace load with copy from current SSA value
                    let current_val = def_stacks[alloca_idx].last().cloned()
                        .unwrap_or(Operand::Const(IrConst::zero(ty)));
                    new_instructions.push(Instruction::Copy {
                        dest,
                        src: current_val,
                    });
                } else {
                    new_instructions.push(Instruction::Load { dest, ptr, ty });
                }
            }
            Instruction::Store { val, ptr, ty } => {
                if let Some(&alloca_idx) = alloca_to_idx.get(&ptr.0) {
                    // Push the stored value onto the def stack
                    def_stacks[alloca_idx].push(val.clone());
                    // Remove the store (it's now represented by the SSA def)
                } else {
                    new_instructions.push(Instruction::Store { val, ptr, ty });
                }
            }
            other => {
                new_instructions.push(other);
            }
        }
    }
    func.blocks[block_idx].instructions = new_instructions;

    // Also rewrite terminator operands if they reference promoted allocas
    // (this shouldn't normally happen, but let's be safe)

    // Fill in phi incoming values in successor blocks
    let succ_labels = get_successor_labels(&func.blocks[block_idx].terminator);
    let current_block_label = func.blocks[block_idx].label;

    for succ_label in &succ_labels {
        if let Some(&succ_idx) = label_to_idx.get(succ_label) {
            // For each phi in the successor block, fill in our value
            for inst in &mut func.blocks[succ_idx].instructions {
                if let Instruction::Phi { dest, incoming, .. } = inst {
                    // Find which alloca this phi is for
                    if let Some(&alloca_idx) = phi_dests[succ_idx]
                        .iter()
                        .find(|(_, &v)| v == *dest)
                        .map(|(idx, _)| idx)
                    {
                        let current_val = def_stacks[alloca_idx].last().cloned()
                            .unwrap_or(Operand::Const(IrConst::zero(alloca_infos[alloca_idx].ty)));
                        incoming.push((current_val, current_block_label));
                    }
                } else {
                    break; // Phis are always at the start of a block
                }
            }
        }
    }

    // Recurse into dominator tree children
    let children: Vec<usize> = dom_children[block_idx].clone();
    for child in children {
        rename_block(
            child,
            func,
            alloca_to_idx,
            alloca_infos,
            def_stacks,
            next_value,
            phi_dests,
            dom_children,
            preds,
            label_to_idx,
        );
    }

    // Pop definitions pushed in this block
    for (i, &depth) in stack_depths.iter().enumerate() {
        def_stacks[i].truncate(depth);
    }
}

/// Get successor labels from a terminator.
fn get_successor_labels(term: &Terminator) -> Vec<BlockId> {
    match term {
        Terminator::Branch(label) => vec![*label],
        Terminator::CondBranch { true_label, false_label, .. } => {
            if true_label == false_label {
                vec![*true_label]
            } else {
                vec![*true_label, *false_label]
            }
        }
        Terminator::IndirectBranch { possible_targets, .. } => possible_targets.clone(),
        Terminator::Return(_) | Terminator::Unreachable => Vec::new(),
    }
}

/// Remove promoted alloca, load, and store instructions.
fn remove_promoted_instructions(func: &mut IrFunction, alloca_to_idx: &FxHashMap<u32, usize>) {
    // Count the total number of allocas that are parameters (by position in entry block).
    // The first N allocas (where N = number of params) are parameter allocas.
    // We must NOT remove those because the backend's find_param_alloca uses positional indexing.
    let num_params = func.params.len();

    // Identify which promoted allocas are parameter allocas by their position
    let mut param_alloca_values: FxHashSet<u32> = FxHashSet::default();
    let mut alloca_count = 0;
    for inst in &func.blocks[0].instructions {
        if let Instruction::Alloca { dest, .. } = inst {
            if alloca_count < num_params {
                param_alloca_values.insert(dest.0);
            }
            alloca_count += 1;
        }
    }

    for block in &mut func.blocks {
        block.instructions.retain(|inst| {
            match inst {
                Instruction::Alloca { dest, .. } => {
                    // Keep parameter allocas, remove promoted non-parameter allocas
                    if alloca_to_idx.contains_key(&dest.0) && !param_alloca_values.contains(&dest.0) {
                        false // remove
                    } else {
                        true // keep
                    }
                }
                Instruction::Store { ptr, .. } => {
                    // Remove stores to promoted allocas
                    !alloca_to_idx.contains_key(&ptr.0)
                }
                Instruction::Load { ptr, .. } => {
                    // Loads to promoted allocas have already been replaced with Copy
                    // But there shouldn't be any left; just in case, keep them
                    !alloca_to_idx.contains_key(&ptr.0)
                }
                _ => true,
            }
        });
    }
}

/// Return the byte size for an IrType.
fn ir_type_size(ty: IrType) -> usize {
    match ty {
        IrType::I8 | IrType::U8 => 1,
        IrType::I16 | IrType::U16 => 2,
        IrType::I32 | IrType::U32 | IrType::F32 => 4,
        IrType::I64 | IrType::U64 | IrType::F64 | IrType::Ptr => 8,
        IrType::I128 | IrType::U128 | IrType::F128 => 16,
        IrType::Void => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper to build a simple function with one local variable.
    /// int f() { int x = 42; return x; }
    fn make_simple_function() -> IrFunction {
        let mut func = IrFunction::new("f".to_string(), IrType::I32, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                // %0 = alloca i32
                Instruction::Alloca { dest: Value(0), ty: IrType::I32, size: 4, align: 0, volatile: false },
                // store 42, %0
                Instruction::Store {
                    val: Operand::Const(IrConst::I32(42)),
                    ptr: Value(0),
                    ty: IrType::I32,
                },
                // %1 = load %0
                Instruction::Load { dest: Value(1), ptr: Value(0), ty: IrType::I32 },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(1)))),
        });
        func
    }

    #[test]
    fn test_simple_promotion() {
        let mut module = IrModule::new();
        module.functions.push(make_simple_function());
        promote_allocas(&mut module);

        let func = &module.functions[0];
        // The alloca should be removed, store removed, load replaced with copy
        let entry = &func.blocks[0];
        // Should have just a Copy instruction (load was replaced)
        assert!(entry.instructions.iter().any(|inst| matches!(inst, Instruction::Copy { .. })));
        // Should not have any Store to the promoted alloca
        assert!(!entry.instructions.iter().any(|inst|
            matches!(inst, Instruction::Store { ptr: Value(0), .. })
        ));
    }

    #[test]
    fn test_diamond_phi_insertion() {
        // int f(int cond) {
        //   int x;
        //   if (cond) x = 1; else x = 2;
        //   return x;
        // }
        let mut func = IrFunction::new(
            "f".to_string(),
            IrType::I32,
            vec![IrParam { name: "cond".to_string(), ty: IrType::I32, struct_size: None }],
            false,
        );

        // entry: alloca for param, alloca for x, branch
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                // %0 = alloca i32 (param)
                Instruction::Alloca { dest: Value(0), ty: IrType::I32, size: 4, align: 0, volatile: false },
                // %1 = alloca i32 (x)
                Instruction::Alloca { dest: Value(1), ty: IrType::I32, size: 4, align: 0, volatile: false },
                // %2 = load %0 (read param)
                Instruction::Load { dest: Value(2), ptr: Value(0), ty: IrType::I32 },
                // %3 = cmp ne %2, 0
                Instruction::Cmp {
                    dest: Value(3), op: IrCmpOp::Ne,
                    lhs: Operand::Value(Value(2)),
                    rhs: Operand::Const(IrConst::I32(0)),
                    ty: IrType::I32,
                },
            ],
            terminator: Terminator::CondBranch {
                cond: Operand::Value(Value(3)),
                true_label: BlockId(1),
                false_label: BlockId(2),
            },
        });

        // then: store 1 to x, branch to merge
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![
                Instruction::Store {
                    val: Operand::Const(IrConst::I32(1)),
                    ptr: Value(1),
                    ty: IrType::I32,
                },
            ],
            terminator: Terminator::Branch(BlockId(3)),
        });

        // else: store 2 to x, branch to merge
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![
                Instruction::Store {
                    val: Operand::Const(IrConst::I32(2)),
                    ptr: Value(1),
                    ty: IrType::I32,
                },
            ],
            terminator: Terminator::Branch(BlockId(3)),
        });

        // merge: load x, return
        func.blocks.push(BasicBlock {
            label: BlockId(3),
            instructions: vec![
                Instruction::Load { dest: Value(4), ptr: Value(1), ty: IrType::I32 },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(4)))),
        });

        let mut module = IrModule::new();
        module.functions.push(func);
        promote_allocas(&mut module);

        let func = &module.functions[0];
        // The merge block should have a phi node
        let merge = &func.blocks[3];
        let has_phi = merge.instructions.iter().any(|inst| matches!(inst, Instruction::Phi { .. }));
        assert!(has_phi, "Expected phi node in merge block");

        // Verify phi has two incoming values
        if let Some(Instruction::Phi { incoming, .. }) = merge.instructions.first() {
            assert_eq!(incoming.len(), 2, "Phi should have 2 incoming values");
        }
    }

    #[test]
    fn test_non_promotable_address_taken() {
        // An alloca whose address is passed to a function should not be promoted
        let mut func = IrFunction::new("f".to_string(), IrType::I32, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Alloca { dest: Value(0), ty: IrType::I32, size: 4, align: 0, volatile: false },
                Instruction::Store {
                    val: Operand::Const(IrConst::I32(42)),
                    ptr: Value(0),
                    ty: IrType::I32,
                },
                // Pass address to a function (address-taken)
                Instruction::Call {
                    dest: None,
                    func: "use_ptr".to_string(),
                    args: vec![Operand::Value(Value(0))],
                    arg_types: vec![IrType::Ptr],
                    return_type: IrType::Void,
                    is_variadic: false,
                    num_fixed_args: 1,
                    struct_arg_sizes: vec![None],
                },
                Instruction::Load { dest: Value(1), ptr: Value(0), ty: IrType::I32 },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(1)))),
        });

        let mut module = IrModule::new();
        module.functions.push(func);
        promote_allocas(&mut module);

        // The alloca should NOT be promoted (address is taken by call)
        let func = &module.functions[0];
        let has_alloca = func.blocks[0].instructions.iter().any(|inst|
            matches!(inst, Instruction::Alloca { dest: Value(0), .. })
        );
        assert!(has_alloca, "Address-taken alloca should not be promoted");
    }

    #[test]
    fn test_loop_phi() {
        // int f() { int sum = 0; for (int i = 0; i < 10; i++) sum += i; return sum; }
        let mut func = IrFunction::new("f".to_string(), IrType::I32, vec![], false);

        // entry: allocas, init, branch to loop header
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Alloca { dest: Value(0), ty: IrType::I32, size: 4, align: 0, volatile: false }, // sum
                Instruction::Alloca { dest: Value(1), ty: IrType::I32, size: 4, align: 0, volatile: false }, // i
                Instruction::Store { val: Operand::Const(IrConst::I32(0)), ptr: Value(0), ty: IrType::I32 },
                Instruction::Store { val: Operand::Const(IrConst::I32(0)), ptr: Value(1), ty: IrType::I32 },
            ],
            terminator: Terminator::Branch(BlockId(1)),
        });

        // loop_header: load i, cmp, cond branch
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![
                Instruction::Load { dest: Value(2), ptr: Value(1), ty: IrType::I32 },
                Instruction::Cmp {
                    dest: Value(3), op: IrCmpOp::Slt,
                    lhs: Operand::Value(Value(2)),
                    rhs: Operand::Const(IrConst::I32(10)),
                    ty: IrType::I32,
                },
            ],
            terminator: Terminator::CondBranch {
                cond: Operand::Value(Value(3)),
                true_label: BlockId(2),
                false_label: BlockId(3),
            },
        });

        // loop_body: sum += i, i++, branch back
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![
                Instruction::Load { dest: Value(4), ptr: Value(0), ty: IrType::I32 },
                Instruction::Load { dest: Value(5), ptr: Value(1), ty: IrType::I32 },
                Instruction::BinOp {
                    dest: Value(6), op: IrBinOp::Add,
                    lhs: Operand::Value(Value(4)),
                    rhs: Operand::Value(Value(5)),
                    ty: IrType::I32,
                },
                Instruction::Store { val: Operand::Value(Value(6)), ptr: Value(0), ty: IrType::I32 },
                Instruction::BinOp {
                    dest: Value(7), op: IrBinOp::Add,
                    lhs: Operand::Value(Value(5)),
                    rhs: Operand::Const(IrConst::I32(1)),
                    ty: IrType::I32,
                },
                Instruction::Store { val: Operand::Value(Value(7)), ptr: Value(1), ty: IrType::I32 },
            ],
            terminator: Terminator::Branch(BlockId(1)),
        });

        // exit: load sum, return
        func.blocks.push(BasicBlock {
            label: BlockId(3),
            instructions: vec![
                Instruction::Load { dest: Value(8), ptr: Value(0), ty: IrType::I32 },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(8)))),
        });

        let mut module = IrModule::new();
        module.functions.push(func);
        promote_allocas(&mut module);

        let func = &module.functions[0];
        // loop_header should have phi nodes for sum and i
        let header = &func.blocks[1];
        let phi_count = header.instructions.iter()
            .filter(|inst| matches!(inst, Instruction::Phi { .. }))
            .count();
        assert_eq!(phi_count, 2, "Loop header should have 2 phi nodes");
    }

    #[test]
    fn test_volatile_alloca_not_promoted() {
        // A volatile alloca should never be promoted to SSA, even though
        // it is scalar and only used by loads/stores.
        let mut func = IrFunction::new("f".to_string(), IrType::I32, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                // %0 = alloca i32 (volatile)
                Instruction::Alloca { dest: Value(0), ty: IrType::I32, size: 4, align: 0, volatile: true },
                Instruction::Store {
                    val: Operand::Const(IrConst::I32(42)),
                    ptr: Value(0),
                    ty: IrType::I32,
                },
                Instruction::Load { dest: Value(1), ptr: Value(0), ty: IrType::I32 },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(1)))),
        });

        let mut module = IrModule::new();
        module.functions.push(func);
        promote_allocas(&mut module);

        // The volatile alloca should NOT be promoted
        let func = &module.functions[0];
        let has_alloca = func.blocks[0].instructions.iter().any(|inst|
            matches!(inst, Instruction::Alloca { dest: Value(0), volatile: true, .. })
        );
        assert!(has_alloca, "Volatile alloca should not be promoted");
        // Store should still exist
        let has_store = func.blocks[0].instructions.iter().any(|inst|
            matches!(inst, Instruction::Store { ptr: Value(0), .. })
        );
        assert!(has_store, "Store to volatile alloca should not be removed");
    }

    #[test]
    fn test_dominator_computation() {
        // Simple diamond CFG: 0 -> 1, 0 -> 2, 1 -> 3, 2 -> 3
        let succs = vec![
            vec![1, 2], // 0
            vec![3],    // 1
            vec![3],    // 2
            vec![],     // 3
        ];
        let preds = vec![
            vec![],     // 0
            vec![0],    // 1
            vec![0],    // 2
            vec![1, 2], // 3
        ];
        let idom = analysis::compute_dominators(4, &preds, &succs);
        assert_eq!(idom[0], 0); // entry dominates itself
        assert_eq!(idom[1], 0); // 0 dominates 1
        assert_eq!(idom[2], 0); // 0 dominates 2
        assert_eq!(idom[3], 0); // 0 dominates 3 (join point)
    }

    #[test]
    fn test_dominance_frontier() {
        // Diamond: 0 -> 1, 0 -> 2, 1 -> 3, 2 -> 3
        let preds = vec![
            vec![],     // 0
            vec![0],    // 1
            vec![0],    // 2
            vec![1, 2], // 3
        ];
        let idom = vec![0, 0, 0, 0];
        let df = analysis::compute_dominance_frontiers(4, &preds, &idom);
        // DF(1) = {3}, DF(2) = {3}
        assert!(df[1].contains(&3));
        assert!(df[2].contains(&3));
        assert!(df[0].is_empty());
        assert!(df[3].is_empty());
    }
}
