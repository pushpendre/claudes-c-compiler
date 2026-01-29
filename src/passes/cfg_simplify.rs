//! CFG Simplification pass.
//!
//! Simplifies the control flow graph by:
//! 1. Folding `CondBranch` with a known-constant condition to `Branch`
//! 2. Folding `Switch` with a known-constant value to `Branch` (matching case or default)
//! 3. Converting `CondBranch` where both targets are the same to `Branch`
//! 4. Threading jump chains: if block A branches to empty block B which just
//!    branches to C, redirect A to branch directly to C (only when safe)
//! 5. Removing dead (unreachable) blocks that have no predecessors
//! 6. Simplifying trivial phi nodes (single-entry or all-same-value) to Copy
//!
//! This pass runs to a fixpoint, since one simplification can enable others.
//! Phi nodes in successor blocks are updated when edges are redirected.

use crate::common::fx_hash::{FxHashMap, FxHashSet};
use crate::common::types::IrType;
use crate::ir::ir::*;

/// Maximum depth for resolving transitive jump chains (A→B→C→...),
/// to prevent pathological cases.
const MAX_CHAIN_DEPTH: u32 = 32;

/// Run CFG simplification on the entire module.
/// Returns the number of simplifications made.
pub fn run(module: &mut IrModule) -> usize {
    module.for_each_function(simplify_cfg)
}

/// Per-function entry point for dirty-tracking pipeline.
pub(crate) fn run_function(func: &mut IrFunction) -> usize {
    simplify_cfg(func)
}

/// Build a map from BlockId -> index in func.blocks for O(1) lookup.
#[inline]
fn build_label_to_idx(func: &IrFunction) -> FxHashMap<BlockId, usize> {
    func.blocks.iter().enumerate().map(|(i, b)| (b.label, i)).collect()
}

/// Simplify the CFG of a single function.
/// Iterates until no more simplifications are possible (fixpoint).
///
/// Builds the `label_to_idx` map once per fixpoint iteration and shares it
/// across sub-passes that need block lookups, avoiding redundant HashMap
/// construction (previously rebuilt 3-4 times per iteration).
pub(crate) fn simplify_cfg(func: &mut IrFunction) -> usize {
    if func.blocks.len() <= 1 {
        return 0;
    }

    let mut total = 0;
    loop {
        let mut changed = 0;
        // Build the label-to-index map once per fixpoint iteration.
        // Sub-passes that modify block structure (remove_dead_blocks) will
        // invalidate this map, but they run last in the iteration so the
        // map is valid for all lookups within the iteration.
        let label_to_idx = build_label_to_idx(func);
        changed += fold_constant_cond_branches_with_map(func, &label_to_idx);
        changed += fold_constant_switches_with_map(func, &label_to_idx);
        changed += simplify_redundant_cond_branches(func);
        changed += thread_jump_chains_with_map(func, &label_to_idx);
        changed += remove_dead_blocks(func);
        changed += simplify_trivial_phis(func);
        changed += merge_single_pred_blocks(func);
        if changed == 0 {
            break;
        }
        total += changed;
    }
    total
}

/// Convert `CondBranch { cond, true_label: X, false_label: X }` to `Branch(X)`.
/// The condition is dead and will be cleaned up by DCE.
fn simplify_redundant_cond_branches(func: &mut IrFunction) -> usize {
    let mut count = 0;
    for block in &mut func.blocks {
        if let Terminator::CondBranch { true_label, false_label, .. } = &block.terminator {
            if true_label == false_label {
                let target = *true_label;
                block.terminator = Terminator::Branch(target);
                count += 1;
            }
        }
    }
    count
}

/// Fold `CondBranch` with a known-constant condition into an unconditional `Branch`.
///
/// After constant folding + copy propagation, a CondBranch may have a constant
/// condition (e.g., `CondBranch { cond: Const(1), true_label, false_label }`).
/// This arises in switch(sizeof(T)) patterns where the dispatch comparisons
/// fold to constants. Converting these to unconditional branches enables dead
/// block removal to eliminate the unreachable switch cases.
///
/// When folding, we must clean up phi nodes in the not-taken target block:
/// the phi entries referencing the current block must be removed since the edge
/// no longer exists. Without this cleanup, stale phi entries can cause
/// miscompilation when the not-taken block is still reachable from other paths.
fn fold_constant_cond_branches_with_map(func: &mut IrFunction, label_to_idx: &FxHashMap<BlockId, usize>) -> usize {
    // Build predecessor count: how many predecessors each block has.
    // Used to enable cross-block value resolution for single-predecessor blocks.
    let mut pred_count: FxHashMap<BlockId, u32> = FxHashMap::default();
    let mut single_pred: FxHashMap<BlockId, BlockId> = FxHashMap::default();
    for block in func.blocks.iter() {
        match &block.terminator {
            Terminator::Branch(target) => {
                let count = pred_count.entry(*target).or_insert(0);
                *count += 1;
                if *count == 1 { single_pred.insert(*target, block.label); }
                else { single_pred.remove(target); }
            }
            Terminator::CondBranch { true_label, false_label, .. } => {
                for target in [true_label, false_label] {
                    let count = pred_count.entry(*target).or_insert(0);
                    *count += 1;
                    if *count == 1 { single_pred.insert(*target, block.label); }
                    else { single_pred.remove(target); }
                }
            }
            Terminator::Switch { cases, default, .. } => {
                let mut targets: Vec<BlockId> = vec![*default];
                for (_, t) in cases { targets.push(*t); }
                for target in targets {
                    let count = pred_count.entry(target).or_insert(0);
                    *count += 1;
                    if *count == 1 { single_pred.insert(target, block.label); }
                    else { single_pred.remove(&target); }
                }
            }
            _ => {}
        }
    }

    // Build a global value definition map for cross-block resolution.
    // Maps Value -> (block_index, instruction_index) for quick lookup.
    // This enables resolving values defined in other blocks, which is critical
    // for eliminating dead code after inlining always_inline functions.
    let global_val_map = build_global_value_map(func);

    // First pass: collect the folding decisions.
    // Each entry: (block_index, taken_target, not_taken_target, block_label)
    let mut folds: Vec<(usize, BlockId, BlockId, BlockId)> = Vec::new();

    for (idx, block) in func.blocks.iter().enumerate() {
        if let Terminator::CondBranch { cond, true_label, false_label } = &block.terminator {
            let const_val = match cond {
                Operand::Const(c) => Some(is_const_nonzero(c)),
                Operand::Value(v) => {
                    // Look through Copy/Phi/Cmp/Select instructions in this block to resolve
                    // the Value to a constant. This handles the case where
                    // simplify_trivial_phis created a Copy { dest: V, src: Const(c) }
                    // in a previous fixpoint iteration, but copy_prop hasn't run yet.
                    let resolved = resolve_value_to_const_in_block(block, *v);
                    if resolved.is_some() {
                        resolved.map(|c| is_const_nonzero(&c))
                    } else {
                        // If not found in this block, walk the single-predecessor chain
                        // (blocks with exactly one predecessor that unconditionally branches
                        // here). This handles cases where the value definition is multiple
                        // blocks away after inlining and partial optimization.
                        let mut current_label = block.label;
                        let mut resolved_from_pred = None;
                        for _ in 0..8 {
                            let pred_label = match single_pred.get(&current_label) {
                                Some(l) => *l,
                                None => break,
                            };
                            let pred_idx = match label_to_idx.get(&pred_label) {
                                Some(&i) => i,
                                None => break,
                            };
                            let pred_block = &func.blocks[pred_idx];
                            if !matches!(&pred_block.terminator, Terminator::Branch(t) if *t == current_label) {
                                break;
                            }
                            if let Some(c) = resolve_value_to_const_in_block(pred_block, *v) {
                                resolved_from_pred = Some(c);
                                break;
                            }
                            current_label = pred_label;
                        }
                        // If still not resolved, try global cross-block resolution.
                        // This is needed for the kernel's alternative_has_cap_unlikely()
                        // pattern: after switch folding and phi simplification, the
                        // cpucap_is_possible result (0 for impossible capabilities) may
                        // be defined in a non-predecessor block, flowing through Cmp/Cast
                        // chains in intermediate blocks.
                        if resolved_from_pred.is_none() {
                            resolved_from_pred = resolve_value_globally(func, *v, &global_val_map, 0);
                        }
                        resolved_from_pred.map(|c| is_const_nonzero(&c))
                    }
                }
            };
            if let Some(is_true) = const_val {
                let taken = if is_true { *true_label } else { *false_label };
                let not_taken = if is_true { *false_label } else { *true_label };
                folds.push((idx, taken, not_taken, block.label));
            }
        }
    }

    if folds.is_empty() {
        return 0;
    }

    let count = folds.len();

    // Apply the folds: change terminators to unconditional branches
    for &(idx, taken, _, _) in &folds {
        func.blocks[idx].terminator = Terminator::Branch(taken);
    }

    // Clean up phi nodes in not-taken target blocks.
    // Remove phi entries that reference the folding block, since that edge
    // no longer exists. Only remove when the not-taken target differs from
    // the taken target (if they're the same, the edge still exists).
    for &(_, taken, not_taken, block_label) in &folds {
        if taken == not_taken {
            // Both branches go to the same block - edge is preserved, no cleanup needed
            continue;
        }
        // Find the not-taken block and remove phi entries from block_label
        if let Some(&block_idx) = label_to_idx.get(&not_taken) {
            let block = &mut func.blocks[block_idx];
            for inst in &mut block.instructions {
                if let Instruction::Phi { incoming, .. } = inst {
                    incoming.retain(|(_, label)| *label != block_label);
                }
            }
        }
    }

    count
}

/// Fold `Switch` with a known-constant value into an unconditional `Branch`.
///
/// After inlining + constant folding + copy propagation, a Switch may have a
/// constant value (e.g., `Switch { val: Const(37), cases: [...], default }`).
/// This arises when `always_inline` functions containing switch statements are
/// inlined at call sites with constant arguments. The switch value becomes a
/// known constant, so we can resolve the target at compile time.
///
/// This is critical for the Linux kernel's `cpucap_is_possible()` pattern:
/// a switch on a capability number that should resolve at compile time but
/// otherwise generates a large runtime comparison chain against a constant.
///
/// When folding, we clean up phi nodes in all not-taken target blocks by
/// removing entries referencing the current block, since those edges no
/// longer exist.
fn fold_constant_switches_with_map(func: &mut IrFunction, label_to_idx: &FxHashMap<BlockId, usize>) -> usize {
    // First pass: collect the folding decisions.
    // Each entry: (block_index, taken_target, not_taken_targets, block_label)
    let mut folds: Vec<(usize, BlockId, Vec<BlockId>, BlockId)> = Vec::new();

    for (idx, block) in func.blocks.iter().enumerate() {
        if let Terminator::Switch { val, cases, default } = &block.terminator {
            let resolved_const = match val {
                Operand::Const(c) => Some(*c),
                Operand::Value(v) => resolve_value_to_const_in_block(block, *v),
            };
            if let Some(c) = resolved_const {
                if let Some(switch_int) = c.to_i64() {
                    // Find the matching case, or fall through to default
                    let taken = cases.iter()
                        .find(|(cv, _)| *cv == switch_int)
                        .map(|(_, label)| *label)
                        .unwrap_or(*default);

                    // Collect all not-taken targets (unique, excluding taken)
                    let mut not_taken = Vec::new();
                    if *default != taken && !not_taken.contains(default) {
                        not_taken.push(*default);
                    }
                    for (_, label) in cases {
                        if *label != taken && !not_taken.contains(label) {
                            not_taken.push(*label);
                        }
                    }

                    folds.push((idx, taken, not_taken, block.label));
                }
            }
        }
    }

    if folds.is_empty() {
        return 0;
    }

    let count = folds.len();

    // Apply the folds: change terminators to unconditional branches
    for &(idx, taken, _, _) in &folds {
        func.blocks[idx].terminator = Terminator::Branch(taken);
    }

    // Clean up phi nodes in not-taken target blocks.
    // Remove phi entries that reference the folding block, since those edges
    // no longer exist.
    for (_, _, ref not_taken, block_label) in &folds {
        for not_taken_target in not_taken {
            if let Some(&block_idx) = label_to_idx.get(not_taken_target) {
                let block = &mut func.blocks[block_idx];
                for inst in &mut block.instructions {
                    if let Instruction::Phi { incoming, .. } = inst {
                        incoming.retain(|(_, label)| *label != *block_label);
                    }
                }
            }
        }
    }

    count
}

/// Build a map from Value -> (block_index, instruction_index) for the entire function.
/// This allows resolve_value_globally to find where a value is defined across blocks.
fn build_global_value_map(func: &IrFunction) -> FxHashMap<Value, (usize, usize)> {
    let mut map = FxHashMap::default();
    for (bi, block) in func.blocks.iter().enumerate() {
        for (ii, inst) in block.instructions.iter().enumerate() {
            let dest = match inst {
                Instruction::Copy { dest, .. } |
                Instruction::Phi { dest, .. } |
                Instruction::Cmp { dest, .. } |
                Instruction::Cast { dest, .. } |
                Instruction::Select { dest, .. } |
                Instruction::BinOp { dest, .. } |
                Instruction::UnaryOp { dest, .. } => Some(*dest),
                _ => None,
            };
            if let Some(d) = dest {
                map.insert(d, (bi, ii));
            }
        }
    }
    map
}

/// Resolve a Value to a constant by following its definition chain across blocks.
/// This handles chains like: Phi → Copy → Cmp(Ne, Cast(Phi), 0) where the inner
/// Phi has collapsed to a constant but the outer chain spans multiple blocks.
///
/// This is critical for the kernel's `alternative_has_cap_unlikely()` pattern:
/// after inlining and switch folding, `cpucap_is_possible(58)` returns 0, but
/// the result flows through Cmp/Cast/Copy chains in separate blocks before
/// reaching the CondBranch that guards the asm goto block.
fn resolve_value_globally(func: &IrFunction, v: Value, val_map: &FxHashMap<Value, (usize, usize)>, depth: usize) -> Option<IrConst> {
    if depth > 16 {
        return None;
    }
    let &(bi, ii) = val_map.get(&v)?;
    let inst = &func.blocks[bi].instructions[ii];
    match inst {
        Instruction::Copy { src: Operand::Const(c), .. } => Some(*c),
        Instruction::Copy { src: Operand::Value(sv), .. } => {
            resolve_value_globally(func, *sv, val_map, depth + 1)
        }
        Instruction::Phi { incoming, .. } => {
            // All incoming must be the same constant
            let mut common_val: Option<i64> = None;
            let mut first_const: Option<IrConst> = None;
            for (op, _) in incoming {
                let c = match op {
                    Operand::Const(c) => Some(*c),
                    Operand::Value(pv) => resolve_value_globally(func, *pv, val_map, depth + 1),
                };
                match c {
                    Some(c) => {
                        if let Some(ci) = c.to_i64() {
                            if let Some(prev) = common_val {
                                if prev != ci { return None; }
                            } else {
                                common_val = Some(ci);
                                first_const = Some(c);
                            }
                        } else {
                            return None;
                        }
                    }
                    None => return None,
                }
            }
            first_const
        }
        Instruction::Cmp { op, lhs, rhs, ty, .. } => {
            let l = resolve_operand_globally(func, lhs, val_map, depth + 1)?;
            let r = resolve_operand_globally(func, rhs, val_map, depth + 1)?;
            let l = truncate_to_cmp_type(l, *ty);
            let r = truncate_to_cmp_type(r, *ty);
            let result = match op {
                IrCmpOp::Eq => l == r,
                IrCmpOp::Ne => l != r,
                IrCmpOp::Slt => l < r,
                IrCmpOp::Sle => l <= r,
                IrCmpOp::Sgt => l > r,
                IrCmpOp::Sge => l >= r,
                IrCmpOp::Ult => (l as u64) < (r as u64),
                IrCmpOp::Ule => (l as u64) <= (r as u64),
                IrCmpOp::Ugt => (l as u64) > (r as u64),
                IrCmpOp::Uge => (l as u64) >= (r as u64),
            };
            Some(IrConst::I32(if result { 1 } else { 0 }))
        }
        Instruction::Cast { src: Operand::Const(c), .. } => Some(*c),
        Instruction::Cast { src: Operand::Value(sv), .. } => {
            resolve_value_globally(func, *sv, val_map, depth + 1)
        }
        Instruction::Select { cond, true_val, false_val, .. } => {
            let cond_val = resolve_operand_globally(func, cond, val_map, depth + 1)?;
            let chosen = if cond_val != 0 { true_val } else { false_val };
            match chosen {
                Operand::Const(c) => Some(*c),
                Operand::Value(cv) => resolve_value_globally(func, *cv, val_map, depth + 1),
            }
        }
        _ => None,
    }
}

/// Resolve an operand to an i64 constant using global cross-block resolution.
fn resolve_operand_globally(func: &IrFunction, op: &Operand, val_map: &FxHashMap<Value, (usize, usize)>, depth: usize) -> Option<i64> {
    match op {
        Operand::Const(c) => c.to_i64(),
        Operand::Value(v) => resolve_value_globally(func, *v, val_map, depth)?.to_i64(),
    }
}

/// Look through Copy, Phi, Cmp, and Select instructions in a block to resolve a Value to
/// a constant. This allows fold_constant_cond_branches and fold_constant_switches
/// to see through Copy instructions created by simplify_trivial_phis within
/// the same cfg_simplify fixpoint loop, without waiting for a separate copy_prop pass.
///
/// Also handles Cmp instructions where both operands resolve to constants within
/// the same block, which is critical for the kernel's `cpucap_is_possible()` pattern:
/// after switch folding resolves `cpucap_is_possible(66)` → 0, the comparison
/// `!cpucap_is_possible(66)` (i.e., `Cmp(Ne, 0, 0)`) can be evaluated at compile
/// time, enabling dead code elimination of feature-gated code paths like
/// `preserve_sve_context`.
fn resolve_value_to_const_in_block(block: &BasicBlock, v: Value) -> Option<IrConst> {
    // First try direct resolution (Copy, Phi)
    for inst in &block.instructions {
        match inst {
            Instruction::Copy { dest, src: Operand::Const(c) } if *dest == v => {
                return Some(*c);
            }
            Instruction::Phi { dest, incoming, .. } if *dest == v => {
                // Check if all incoming values are the same constant (by integer value)
                let mut common_val: Option<i64> = None;
                let mut first_const: Option<IrConst> = None;
                for (op, _) in incoming {
                    match op {
                        Operand::Const(c) => {
                            if let Some(ci) = c.to_i64() {
                                if let Some(prev) = common_val {
                                    if prev != ci {
                                        return None; // Different constants
                                    }
                                } else {
                                    common_val = Some(ci);
                                    first_const = Some(*c);
                                }
                            } else {
                                return None; // Can't convert to i64
                            }
                        }
                        _ => return None, // Non-constant incoming value
                    }
                }
                return first_const;
            }
            // Evaluate Cmp with constant operands
            Instruction::Cmp { dest, op, lhs, rhs, ty } if *dest == v => {
                let lhs_const = resolve_operand_to_i64_in_block(block, lhs);
                let rhs_const = resolve_operand_to_i64_in_block(block, rhs);
                if let (Some(l), Some(r)) = (lhs_const, rhs_const) {
                    // Truncate operands to the comparison type's width before comparing.
                    // Constants may be stored in different IrConst variants (e.g., I32 vs I64)
                    // and to_i64() sign-extends I32 values. Without truncation, a U32 value
                    // like 0xFFFFFFFE stored as IrConst::I32(-2) sign-extends to i64 -2,
                    // while the same value stored as IrConst::I64(4294967294) stays positive,
                    // causing incorrect comparison results.
                    let l = truncate_to_cmp_type(l, *ty);
                    let r = truncate_to_cmp_type(r, *ty);
                    let result = match op {
                        IrCmpOp::Eq => l == r,
                        IrCmpOp::Ne => l != r,
                        IrCmpOp::Slt => l < r,
                        IrCmpOp::Sle => l <= r,
                        IrCmpOp::Sgt => l > r,
                        IrCmpOp::Sge => l >= r,
                        IrCmpOp::Ult => (l as u64) < (r as u64),
                        IrCmpOp::Ule => (l as u64) <= (r as u64),
                        IrCmpOp::Ugt => (l as u64) > (r as u64),
                        IrCmpOp::Uge => (l as u64) >= (r as u64),
                    };
                    return Some(IrConst::I32(if result { 1 } else { 0 }));
                }
            }
            // Evaluate Select with constant condition
            Instruction::Select { dest, cond, true_val, false_val, .. } if *dest == v => {
                if let Some(cond_const) = resolve_operand_to_i64_in_block(block, cond) {
                    let chosen = if cond_const != 0 { true_val } else { false_val };
                    match chosen {
                        Operand::Const(c) => return Some(*c),
                        Operand::Value(cv) => return resolve_value_to_const_in_block(block, *cv),
                    }
                }
            }
            _ => {}
        }
    }
    None
}

/// Resolve an operand to an i64 constant, looking through Copy and Phi
/// instructions in the same block.
fn resolve_operand_to_i64_in_block(block: &BasicBlock, op: &Operand) -> Option<i64> {
    match op {
        Operand::Const(c) => c.to_i64(),
        Operand::Value(v) => resolve_value_to_const_in_block(block, *v)?.to_i64(),
    }
}

/// Truncate an i64 value to the width of the given IR type, with proper
/// sign/zero extension back to i64. This ensures constants stored in different
/// IrConst variants (e.g., I32 vs I64) are normalized to the same representation
/// before comparison, matching the semantics in constant_fold::truncate_to_type.
fn truncate_to_cmp_type(val: i64, ty: IrType) -> i64 {
    match ty {
        IrType::I8 => val as i8 as i64,
        IrType::U8 => val as u8 as i64,
        IrType::I16 => val as i16 as i64,
        IrType::U16 => val as u16 as i64,
        IrType::I32 => val as i32 as i64,
        IrType::U32 => val as u32 as i64,
        _ => val,
    }
}

fn is_const_nonzero(c: &IrConst) -> bool {
    match c {
        IrConst::I8(v) => *v != 0,
        IrConst::I16(v) => *v != 0,
        IrConst::I32(v) => *v != 0,
        IrConst::I64(v) => *v != 0,
        IrConst::I128(v) => *v != 0,
        IrConst::F32(v) => *v != 0.0,
        IrConst::F64(v) => *v != 0.0,
        IrConst::LongDouble(v, _) => *v != 0.0,
        IrConst::Zero => false,
    }
}

/// Check if threading a CondBranch's two edges to the same final target would
/// create a phi conflict. This happens when the final target block has a phi
/// node that carries different values from the two paths.
///
/// Example: Block A CondBranch(true: B, false: C), B is empty forwarding to C.
/// C has Phi with (val_from_A, A) and (val_from_B, B) where the values differ.
/// Threading B out would merge both edges to C, and the subsequent redundant-
/// branch simplification converts CondBranch(true:C, false:C) to Branch(C).
/// Dead block removal then deletes B, removing (val_from_B, B) from the phi,
/// losing the true-path value.
///
/// For multi-hop chains (B -> C -> D), the phi in D references C (the last hop),
/// not B (the start). We must use the last hop for phi lookups.
///
/// Parameters:
/// - `block_label`: the predecessor block (A) with the CondBranch
/// - `true_label`: the true target (B, which may forward to the final target)
/// - `false_label`: the false target (C, which may forward or be the final target)
/// - `target`: the final target block both paths would reach
/// - `resolved`: the forwarding resolution map (block -> (final_target, phi_lookup_block))
fn would_create_phi_conflict(
    func: &IrFunction,
    block_label: BlockId,
    true_label: BlockId,
    false_label: BlockId,
    target: BlockId,
    resolved: &FxHashMap<BlockId, (BlockId, BlockId)>,
    label_to_idx: &FxHashMap<BlockId, usize>,
) -> bool {
    // Find the target block using O(1) lookup
    let target_block = match label_to_idx.get(&target) {
        Some(&idx) => &func.blocks[idx],
        None => return false,
    };

    // Determine the phi-lookup label for each path.
    // For a path through an intermediate chain, the phi references the immediate
    // predecessor of target (the phi_lookup_block from resolved), not the first
    // intermediate. For a direct path (not resolved), the phi references block_label.
    let true_phi_label = if let Some(&(_, phi_block)) = resolved.get(&true_label) {
        phi_block
    } else {
        block_label
    };
    let false_phi_label = if let Some(&(_, phi_block)) = resolved.get(&false_label) {
        phi_block
    } else {
        block_label
    };

    // If both paths use the same phi label, there's no way they carry different values
    if true_phi_label == false_phi_label {
        return false;
    }

    // Check each phi in the target block
    for inst in &target_block.instructions {
        if let Instruction::Phi { incoming, .. } = inst {
            let mut true_value = None;
            let mut false_value = None;

            for (val, label) in incoming {
                if *label == true_phi_label {
                    true_value = Some(val);
                }
                if *label == false_phi_label {
                    false_value = Some(val);
                }
            }

            // If both paths have entries and they differ, this is a conflict
            if let (Some(tv), Some(fv)) = (true_value, false_value) {
                if !operands_equal(tv, fv) {
                    return true;
                }
            }
        }
    }

    false
}

/// Compare two operands for structural equality.
fn operands_equal(a: &Operand, b: &Operand) -> bool {
    match (a, b) {
        (Operand::Value(v1), Operand::Value(v2)) => v1.0 == v2.0,
        (Operand::Const(c1), Operand::Const(c2)) => consts_equal_for_phi(c1, c2),
        _ => false,
    }
}

/// Compare two IR constants for phi simplification.
/// Integer constants of different widths but same numeric value are considered equal
/// (e.g., I32(0) == I64(0)). This is important because different IR paths may produce
/// the same semantic value at different IR type widths (e.g., a Cmp returns I32 while
/// the short-circuit && default uses I64).
fn consts_equal_for_phi(a: &IrConst, b: &IrConst) -> bool {
    // Fast path: same variant
    if a.to_hash_key() == b.to_hash_key() {
        return true;
    }
    // Cross-width integer comparison: extract as i64 and compare
    match (a.to_i64(), b.to_i64()) {
        (Some(va), Some(vb)) => va == vb,
        _ => false,
    }
}

/// Thread jump chains: if a block branches to an empty forwarding block
/// (no instructions, terminates with unconditional Branch), redirect to
/// skip the intermediate block.
///
/// We only thread through a block if:
/// - The intermediate block has NO instructions (including no phi nodes)
/// - The intermediate block terminates with an unconditional Branch
///
/// After threading, we update phi nodes in the target block to replace
/// references to the intermediate block with references to the redirected
/// predecessor.
///
/// Special care: when threading would cause a CondBranch's true and false
/// targets to become the same block (both edges merge), AND the target has
/// phi nodes that carry different values from the two paths, we must NOT
/// thread. Otherwise the merge block's phi loses the ability to distinguish
/// the two control flow paths, causing miscompilation.
fn thread_jump_chains_with_map(func: &mut IrFunction, label_to_idx: &FxHashMap<BlockId, usize>) -> usize {
    // Build a map of block_id -> forwarding target for empty blocks.
    // An "empty forwarding block" has no instructions (including no phis)
    // and terminates with Branch(target).
    let forwarding: FxHashMap<BlockId, BlockId> = func.blocks.iter()
        .filter(|block| {
            block.instructions.is_empty()
                && matches!(&block.terminator, Terminator::Branch(_))
        })
        .map(|block| {
            if let Terminator::Branch(target) = &block.terminator {
                (block.label, *target)
            } else {
                unreachable!()
            }
        })
        .collect();

    if forwarding.is_empty() {
        return 0;
    }

    // Resolve transitive chains with cycle detection.
    // If A -> B -> C where both B and C are forwarding blocks, resolve to A -> final.
    // We also track the immediate predecessor of the final target for phi updates:
    // in chain B -> C -> D, the final target is D and the immediate predecessor is C.
    // Phi nodes in D reference C (not B), so we need C to look up phi values.
    let resolved: FxHashMap<BlockId, (BlockId, BlockId)> = {
        let mut resolved = FxHashMap::default();
        for &start in forwarding.keys() {
            let mut prev = start;
            let mut current = start;
            let mut depth = 0;
            while let Some(&next) = forwarding.get(&current) {
                if next == start || depth > MAX_CHAIN_DEPTH {
                    break; // cycle or too deep
                }
                prev = current;
                current = next;
                depth += 1;
            }
            if current != start {
                // resolved maps: start -> (final_target, immediate_predecessor_of_final)
                resolved.insert(start, (current, prev));
            }
        }
        resolved
    };

    if resolved.is_empty() {
        return 0;
    }

    // Collect the redirections we need to make.
    // Each edge change: (old_intermediate, new_target, phi_lookup_block)
    // phi_lookup_block is the immediate predecessor of new_target in the chain,
    // which is the block whose label appears in new_target's phi nodes.
    let mut redirections: Vec<(usize, Vec<(BlockId, BlockId, BlockId)>)> = Vec::new();

    for block_idx in 0..func.blocks.len() {
        let mut edge_changes = Vec::new();
        let block_label = func.blocks[block_idx].label;

        match &func.blocks[block_idx].terminator {
            Terminator::Branch(target) => {
                if let Some(&(resolved_target, phi_block)) = resolved.get(target) {
                    edge_changes.push((*target, resolved_target, phi_block));
                }
            }
            Terminator::CondBranch { true_label, false_label, .. } => {
                let true_resolved = resolved.get(true_label).copied();
                let false_resolved = resolved.get(false_label).copied();

                // Determine the final targets after potential threading
                let true_final = true_resolved.map(|(t, _)| t).unwrap_or(*true_label);
                let false_final = false_resolved.map(|(t, _)| t).unwrap_or(*false_label);

                if true_final == false_final && true_label != false_label {
                    // Threading would make both branches go to the same block.
                    // This is dangerous when the target has phi nodes: the phi
                    // entries from the two intermediates carry path-specific
                    // values, and merging both edges into one would lose this
                    // distinction. Check for phi conflict.
                    if would_create_phi_conflict(func, block_label, *true_label,
                                                  *false_label, true_final, &resolved,
                                                  &label_to_idx) {
                        // Phi conflict: don't thread either edge
                    } else {
                        // No phi conflict - safe to thread both edges
                        if let Some((rt, rt_phi)) = true_resolved {
                            edge_changes.push((*true_label, rt, rt_phi));
                        }
                        if let Some((rf, rf_phi)) = false_resolved {
                            if !edge_changes.iter().any(|(old, new, _)| *old == *false_label && *new == rf) {
                                edge_changes.push((*false_label, rf, rf_phi));
                            }
                        }
                    }
                } else if true_final != false_final {
                    // Different final targets - safe to thread
                    if let Some((rt, rt_phi)) = true_resolved {
                        edge_changes.push((*true_label, rt, rt_phi));
                    }
                    if let Some((rf, rf_phi)) = false_resolved {
                        if !edge_changes.iter().any(|(old, new, _)| *old == *false_label && *new == rf) {
                            edge_changes.push((*false_label, rf, rf_phi));
                        }
                    }
                }
            }
            // TODO: IndirectBranch targets could also be threaded through
            // empty blocks, but computed goto is rare enough to skip for now.
            _ => {}
        }

        if !edge_changes.is_empty() {
            redirections.push((block_idx, edge_changes));
        }
    }

    if redirections.is_empty() {
        return 0;
    }

    // Apply the redirections.
    let mut count = 0;
    for (block_idx, edge_changes) in &redirections {
        let block_label = func.blocks[*block_idx].label;

        // Update the terminator
        match &mut func.blocks[*block_idx].terminator {
            Terminator::Branch(target) => {
                for (old, new, _) in edge_changes {
                    if target == old {
                        *target = *new;
                    }
                }
            }
            Terminator::CondBranch { true_label, false_label, .. } => {
                for (old, new, _) in edge_changes {
                    if true_label == old {
                        *true_label = *new;
                    }
                    if false_label == old {
                        *false_label = *new;
                    }
                }
            }
            _ => {}
        }
        count += 1;

        // Update phi nodes in the new target blocks.
        // For each edge change, we use phi_lookup_block (the immediate predecessor
        // of new_target in the forwarding chain) to find the correct phi value.
        // In a chain A -> B -> C -> D, when redirecting A from B to D:
        //   - old_intermediate = B (the original target)
        //   - new_target = D (the final resolved target)
        //   - phi_lookup_block = C (D's phi entries reference C, not B)
        for (_old_intermediate, new_target, phi_lookup_block) in edge_changes {
            // Find the new_target block using O(1) lookup
            if let Some(&target_idx) = label_to_idx.get(new_target) {
                let block = &mut func.blocks[target_idx];
                for inst in &mut block.instructions {
                    if let Instruction::Phi { incoming, .. } = inst {
                        // Look up the phi value using the immediate predecessor
                        // in the chain (phi_lookup_block), which is the block
                        // that phi entries in new_target actually reference.
                        let value_from_chain = incoming.iter()
                            .find(|(_, label)| *label == *phi_lookup_block)
                            .map(|(val, _)| *val);
                        if let Some(val) = value_from_chain {
                            // Only add if block_label doesn't already have an entry
                            let already_has_entry = incoming.iter()
                                .any(|(_, label)| *label == block_label);
                            if !already_has_entry {
                                incoming.push((val, block_label));
                            }
                        }
                    }
                }
            }
        }
    }

    count
}

/// Remove blocks that have no predecessors (except the entry block, blocks[0]).
/// Returns the number of blocks removed.
fn remove_dead_blocks(func: &mut IrFunction) -> usize {
    if func.blocks.len() <= 1 {
        return 0;
    }

    // Compute the set of blocks reachable from the entry block
    let entry = func.blocks[0].label;
    let mut reachable = FxHashSet::default();
    reachable.insert(entry);

    // Build a map from block ID to index for quick lookup
    let block_map: FxHashMap<BlockId, usize> = func.blocks.iter()
        .enumerate()
        .map(|(i, b)| (b.label, i))
        .collect();

    // BFS from entry block
    let mut worklist = vec![entry];
    while let Some(block_id) = worklist.pop() {
        if let Some(&idx) = block_map.get(&block_id) {
            // Successor blocks from terminator (no Vec allocation)
            for_each_terminator_target(&func.blocks[idx].terminator, |target| {
                if reachable.insert(target) {
                    worklist.push(target);
                }
            });
            // LabelAddr and InlineAsm goto labels (computed goto targets)
            for inst in &func.blocks[idx].instructions {
                if let Instruction::LabelAddr { label, .. } = inst {
                    if reachable.insert(*label) {
                        worklist.push(*label);
                    }
                }
                if let Instruction::InlineAsm { goto_labels, .. } = inst {
                    // Always mark asm goto target blocks as reachable.
                    // The backend always emits the asm body (using $0 placeholders
                    // for unsatisfiable immediate constraints), so goto target labels
                    // must exist in the assembly output. Removing them would cause
                    // linker errors like "undefined reference to '.L30'" when the
                    // asm template references %l[label] in .pushsection directives
                    // like __jump_table.
                    for (_, label) in goto_labels {
                        if reachable.insert(*label) {
                            worklist.push(*label);
                        }
                    }
                }
            }
        }
    }

    // Collect dead blocks
    let dead_blocks: FxHashSet<BlockId> = func.blocks.iter()
        .map(|b| b.label)
        .filter(|label| !reachable.contains(label))
        .collect();

    if dead_blocks.is_empty() {
        return 0;
    }

    // Clean up phi nodes in reachable blocks that reference dead blocks
    for block in &mut func.blocks {
        if !reachable.contains(&block.label) {
            continue;
        }
        for inst in &mut block.instructions {
            if let Instruction::Phi { incoming, .. } = inst {
                incoming.retain(|(_, label)| !dead_blocks.contains(label));
            }
            // Defensive: clean up InlineAsm goto_labels that reference
            // dead blocks. While asm goto targets are always marked reachable
            // above, this guards against edge cases where other passes might
            // remove blocks that happen to be goto targets.
            if let Instruction::InlineAsm { goto_labels, .. } = inst {
                goto_labels.retain(|(_, label)| !dead_blocks.contains(label));
            }
        }
    }

    let original_len = func.blocks.len();
    func.blocks.retain(|b| reachable.contains(&b.label));
    original_len - func.blocks.len()
}

/// Simplify trivial phi nodes: phi nodes with exactly one incoming edge,
/// or where all incoming values are identical, are replaced with Copy
/// instructions. This enables copy propagation to propagate the value
/// to all uses, and subsequent constant branch folding can then eliminate
/// dead branches.
///
/// This is critical for patterns like `if (1 || expr)` where the `||`
/// short-circuit generates a phi that merges two paths, but after constant
/// branch folding removes the dead path, the phi has only one incoming
/// edge remaining. Without this simplification, the phi result stays as
/// a non-constant Value, preventing the outer `if` from being folded.
fn simplify_trivial_phis(func: &mut IrFunction) -> usize {
    let mut count = 0;

    for block in &mut func.blocks {
        for inst in &mut block.instructions {
            if let Instruction::Phi { dest, incoming, .. } = inst {
                let replacement = if incoming.len() == 1 {
                    // Single incoming edge: replace with Copy
                    Some(incoming[0].0)
                } else if incoming.len() > 1 {
                    // Check if all incoming values are identical
                    let first = &incoming[0].0;
                    if incoming.iter().all(|(val, _)| operands_equal(val, first)) {
                        Some(*first)
                    } else {
                        None
                    }
                } else {
                    None
                };

                if let Some(src) = replacement {
                    *inst = Instruction::Copy { dest: *dest, src };
                    count += 1;
                }
            }
        }
    }

    count
}

/// Merge single-predecessor blocks into their predecessor.
///
/// When block A ends with `Branch(B)` and B has exactly one predecessor (A),
/// the two blocks can be fused: B's instructions are appended to A and A
/// inherits B's terminator. This eliminates a block boundary, turning values
/// that were "multi-block" (defined in A, used in B) into single-block values.
///
/// This is critical for stack frame reduction after inlining: each inlined
/// function body creates a chain of blocks connected by unconditional branches.
/// Without fusion, SSA values crossing these artificial block boundaries are
/// classified as multi-block and each gets a permanent stack slot (8 bytes).
/// After fusion, these values become block-local and participate in Tier 3
/// coalescing, sharing stack space across non-overlapping regions.
///
/// Phi nodes in B with a single predecessor A are converted to Copy instructions
/// (they should already have been simplified by `simplify_trivial_phis`, but we
/// handle them defensively).
fn merge_single_pred_blocks(func: &mut IrFunction) -> usize {
    if func.blocks.len() <= 1 {
        return 0;
    }

    // Build predecessor count and single-predecessor map.
    // pred_count[B] = number of distinct predecessors of B.
    // single_pred_from[B] = A if A is B's only predecessor (via Branch(B)).
    let mut pred_count: FxHashMap<BlockId, u32> = FxHashMap::default();
    for block in func.blocks.iter() {
        for_each_terminator_target(&block.terminator, |target| {
            *pred_count.entry(target).or_insert(0) += 1;
        });
    }

    // Identify which blocks can be merged and build fusion map.
    // A block B can be merged into A if:
    // 1. B has exactly one predecessor
    // 2. A's terminator is Branch(B) (unconditional)
    // 3. B is not the entry block (block index 0)
    // 4. B does not contain LabelAddr references to itself (computed goto targets)
    // 5. B does not contain InlineAsm with goto_labels (asm goto targets must
    //    remain as separate blocks for correct label emission)
    //
    // We process fusions iteratively: merge_into[A_label] = B_label means
    // A should absorb B. We process in order so chains (A->B->C) are handled
    // by the fixpoint loop in simplify_cfg.

    let entry_label = func.blocks[0].label;
    let label_to_idx: FxHashMap<BlockId, usize> = func.blocks.iter()
        .enumerate().map(|(i, b)| (b.label, i)).collect();

    // Collect fusion pairs: (pred_idx, succ_idx) where pred absorbs succ.
    let mut fusions: Vec<(usize, usize)> = Vec::new();
    // Track blocks that are already targets of a fusion (to prevent double-merging).
    let mut absorbed: FxHashSet<usize> = FxHashSet::default();
    // Track blocks that are already predecessors in a fusion.
    let mut absorbers: FxHashSet<usize> = FxHashSet::default();

    for (idx, block) in func.blocks.iter().enumerate() {
        if let Terminator::Branch(target) = &block.terminator {
            // Skip if target is entry block.
            if *target == entry_label {
                continue;
            }
            // Skip if target has multiple predecessors.
            if pred_count.get(target).copied().unwrap_or(0) != 1 {
                continue;
            }
            let &succ_idx = match label_to_idx.get(target) {
                Some(si) => si,
                None => continue,
            };
            // Don't merge a block with itself.
            if idx == succ_idx {
                continue;
            }
            // Don't merge if the successor is already being absorbed or is an absorber.
            if absorbed.contains(&succ_idx) || absorbers.contains(&succ_idx) {
                continue;
            }
            // Don't merge if this block is already being absorbed.
            if absorbed.contains(&idx) {
                continue;
            }
            // Check if the successor contains any InlineAsm with goto_labels.
            // These blocks must remain separate for correct label emission.
            let has_asm_goto = func.blocks[succ_idx].instructions.iter().any(|inst| {
                if let Instruction::InlineAsm { goto_labels, .. } = inst {
                    !goto_labels.is_empty()
                } else {
                    false
                }
            });
            if has_asm_goto {
                continue;
            }
            // Check if any block references the successor's label via LabelAddr.
            // Such blocks are computed goto targets and must keep their identity.
            let label_addr_target = func.blocks.iter().any(|b| {
                b.instructions.iter().any(|inst| {
                    if let Instruction::LabelAddr { label, .. } = inst {
                        *label == *target
                    } else {
                        false
                    }
                })
            });
            if label_addr_target {
                continue;
            }

            fusions.push((idx, succ_idx));
            absorbed.insert(succ_idx);
            absorbers.insert(idx);
        }
    }

    if fusions.is_empty() {
        return 0;
    }

    let count = fusions.len();

    // Perform the fusions.
    // We need to be careful about indices since we're modifying func.blocks.
    // Strategy: process fusions by swapping out instructions, then remove absorbed blocks.

    for &(pred_idx, succ_idx) in &fusions {
        // Take the successor's instructions and terminator.
        let succ_instructions = std::mem::take(&mut func.blocks[succ_idx].instructions);
        let succ_terminator = std::mem::replace(
            &mut func.blocks[succ_idx].terminator,
            Terminator::Unreachable,
        );
        let succ_spans = std::mem::take(&mut func.blocks[succ_idx].source_spans);
        let pred_label = func.blocks[pred_idx].label;

        // Convert any phi nodes in the successor to Copy instructions.
        // Since B has exactly one predecessor A, each Phi([(val, A)]) -> Copy(val).
        let mut converted_instructions: Vec<Instruction> = Vec::with_capacity(succ_instructions.len());
        let mut converted_spans = Vec::with_capacity(succ_spans.len());
        for (i, inst) in succ_instructions.into_iter().enumerate() {
            if let Instruction::Phi { dest, incoming, .. } = &inst {
                // Find the value from the predecessor.
                let src = incoming.iter()
                    .find(|(_, label)| *label == pred_label)
                    .map(|(op, _)| *op)
                    .unwrap_or_else(|| {
                        // Fallback: use first incoming value if pred not found
                        // (shouldn't happen for single-pred blocks).
                        if !incoming.is_empty() {
                            incoming[0].0
                        } else {
                            Operand::Const(IrConst::I64(0))
                        }
                    });
                converted_instructions.push(Instruction::Copy { dest: *dest, src });
            } else {
                converted_instructions.push(inst);
            }
            if i < succ_spans.len() {
                converted_spans.push(succ_spans[i]);
            }
        }

        // Append to predecessor, maintaining the source_spans invariant:
        // source_spans must be either empty or exactly parallel to instructions.
        // When merging blocks with mixed span states, we pad the missing side
        // with dummy spans to keep them in sync.
        let pred_has_spans = !func.blocks[pred_idx].source_spans.is_empty();
        let succ_has_spans = !converted_spans.is_empty();

        if pred_has_spans && !succ_has_spans && !converted_instructions.is_empty() {
            // Predecessor has spans but successor doesn't: pad successor spans
            // with dummy entries so the merged block stays in sync.
            converted_spans.resize(converted_instructions.len(), crate::common::source::Span::dummy());
        } else if !pred_has_spans && succ_has_spans {
            // Successor has spans but predecessor doesn't: pad predecessor spans
            // with dummy entries for its existing instructions.
            let pred_inst_len = func.blocks[pred_idx].instructions.len();
            func.blocks[pred_idx].source_spans.resize(
                pred_inst_len,
                crate::common::source::Span::dummy(),
            );
        }

        func.blocks[pred_idx].instructions.extend(converted_instructions);
        func.blocks[pred_idx].terminator = succ_terminator;
        if !converted_spans.is_empty() {
            func.blocks[pred_idx].source_spans.extend(converted_spans);
        }
    }

    // Remove absorbed blocks (mark as unreachable, then let remove_dead_blocks clean up).
    // We already set their terminators to Unreachable and cleared instructions,
    // so remove_dead_blocks will handle them on the next iteration.
    // But we also need to update any phi nodes in other blocks that reference
    // the absorbed block's label to reference the predecessor's label instead.
    let absorbed_to_pred: FxHashMap<BlockId, BlockId> = fusions.iter()
        .map(|&(pred_idx, succ_idx)| (func.blocks[succ_idx].label, func.blocks[pred_idx].label))
        .collect();

    for block in &mut func.blocks {
        // Update phi nodes: replace references to absorbed blocks with their predecessors.
        for inst in &mut block.instructions {
            if let Instruction::Phi { incoming, .. } = inst {
                for (_, label) in incoming.iter_mut() {
                    if let Some(&new_label) = absorbed_to_pred.get(label) {
                        *label = new_label;
                    }
                }
            }
        }
        // Update terminators that reference absorbed blocks.
        match &mut block.terminator {
            Terminator::Branch(target) => {
                if let Some(&new_target) = absorbed_to_pred.get(target) {
                    *target = new_target;
                }
            }
            Terminator::CondBranch { true_label, false_label, .. } => {
                if let Some(&new_target) = absorbed_to_pred.get(true_label) {
                    *true_label = new_target;
                }
                if let Some(&new_target) = absorbed_to_pred.get(false_label) {
                    *false_label = new_target;
                }
            }
            Terminator::Switch { cases, default, .. } => {
                if let Some(&new_target) = absorbed_to_pred.get(default) {
                    *default = new_target;
                }
                for (_, label) in cases.iter_mut() {
                    if let Some(&new_target) = absorbed_to_pred.get(label) {
                        *label = new_target;
                    }
                }
            }
            Terminator::IndirectBranch { possible_targets, .. } => {
                for target in possible_targets.iter_mut() {
                    if let Some(&new_target) = absorbed_to_pred.get(target) {
                        *target = new_target;
                    }
                }
            }
            _ => {}
        }
    }

    count
}

/// Visit each branch target of a terminator, calling `f` for each unique target.
/// This avoids allocating a Vec for each call, which is significant in hot paths.
#[inline]
fn for_each_terminator_target(term: &Terminator, mut f: impl FnMut(BlockId)) {
    match term {
        Terminator::Branch(target) => f(*target),
        Terminator::CondBranch { true_label, false_label, .. } => {
            f(*true_label);
            f(*false_label);
        }
        Terminator::IndirectBranch { possible_targets, .. } => {
            for &target in possible_targets {
                f(target);
            }
        }
        Terminator::Switch { cases, default, .. } => {
            f(*default);
            // For switch, we need to deduplicate targets to avoid visiting
            // the same block multiple times. Use a small inline set.
            for &(_, label) in cases {
                if label != *default {
                    f(label);
                }
            }
        }
        Terminator::Return(_) | Terminator::Unreachable => {}
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::types::IrType;

    #[test]
    fn test_redundant_cond_branch() {
        let mut func = IrFunction::new("test".to_string(), IrType::Void, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Copy { dest: Value(0), src: Operand::Const(IrConst::I32(1)) },
            ],
            terminator: Terminator::CondBranch {
                cond: Operand::Value(Value(0)),
                true_label: BlockId(1),
                false_label: BlockId(1),
            },
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![],
            terminator: Terminator::Return(None),
            source_spans: Vec::new(),
        });

        let count = simplify_cfg(&mut func);
        assert!(count > 0);
        assert!(matches!(func.blocks[0].terminator, Terminator::Branch(BlockId(1))));
    }

    #[test]
    fn test_jump_chain_threading() {
        let mut func = IrFunction::new("test".to_string(), IrType::Void, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![],
            terminator: Terminator::Branch(BlockId(1)),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![],
            terminator: Terminator::Branch(BlockId(2)),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![],
            terminator: Terminator::Return(None),
            source_spans: Vec::new(),
        });

        let count = simplify_cfg(&mut func);
        assert!(count > 0);
        assert!(matches!(func.blocks[0].terminator, Terminator::Branch(BlockId(2))));
    }

    #[test]
    fn test_dead_block_elimination() {
        let mut func = IrFunction::new("test".to_string(), IrType::Void, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![],
            terminator: Terminator::Return(None),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![
                Instruction::Copy { dest: Value(0), src: Operand::Const(IrConst::I32(42)) },
            ],
            terminator: Terminator::Return(None),
            source_spans: Vec::new(),
        });

        let count = simplify_cfg(&mut func);
        assert!(count > 0);
        assert_eq!(func.blocks.len(), 1);
        assert_eq!(func.blocks[0].label, BlockId(0));
    }

    #[test]
    fn test_combined_simplifications() {
        let mut func = IrFunction::new("test".to_string(), IrType::Void, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Copy { dest: Value(0), src: Operand::Const(IrConst::I32(1)) },
            ],
            terminator: Terminator::CondBranch {
                cond: Operand::Value(Value(0)),
                true_label: BlockId(1),
                false_label: BlockId(1),
            },
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![],
            terminator: Terminator::Branch(BlockId(2)),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![],
            terminator: Terminator::Return(None),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(3),
            instructions: vec![],
            terminator: Terminator::Return(None),
            source_spans: Vec::new(),
        });

        let count = simplify_cfg(&mut func);
        assert!(count > 0);
        assert!(func.blocks.len() <= 3);
        match &func.blocks[0].terminator {
            Terminator::Branch(target) => assert_eq!(*target, BlockId(2)),
            _ => panic!("Expected Branch terminator"),
        }
    }

    #[test]
    fn test_phi_update_on_thread() {
        // Block 0 -> Block 1 (empty) -> Block 2 (has phi referencing Block 1)
        let mut func = IrFunction::new("test".to_string(), IrType::I32, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Copy { dest: Value(0), src: Operand::Const(IrConst::I32(42)) },
            ],
            terminator: Terminator::Branch(BlockId(1)),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![],
            terminator: Terminator::Branch(BlockId(2)),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![
                Instruction::Phi {
                    dest: Value(1),
                    ty: IrType::I32,
                    incoming: vec![(Operand::Value(Value(0)), BlockId(1))],
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(1)))),
            source_spans: Vec::new(),
        });

        let count = simplify_cfg(&mut func);
        assert!(count > 0);
        assert!(matches!(func.blocks[0].terminator, Terminator::Branch(BlockId(2))));

        // After threading Block 0 -> Block 2 (skipping empty Block 1),
        // the phi in Block 2 gets a new entry for Block 0. Then Block 1 becomes
        // dead and is removed, leaving a single-entry phi which simplify_trivial_phis
        // converts to a Copy instruction.
        let last_block = func.blocks.last().unwrap();
        match &last_block.instructions[0] {
            Instruction::Phi { incoming, .. } => {
                assert!(incoming.iter().any(|(_, label)| *label == BlockId(0)));
            }
            Instruction::Copy { dest, .. } => {
                // After trivial phi simplification, the phi became a Copy
                assert_eq!(dest.0, 1);
            }
            other => panic!("Expected Phi or Copy instruction, got {:?}", other),
        }
    }

    #[test]
    fn test_no_thread_through_block_with_instructions() {
        // Block 1 has an instruction, so it should NOT be threaded
        let mut func = IrFunction::new("test".to_string(), IrType::I32, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![],
            terminator: Terminator::Branch(BlockId(1)),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![
                Instruction::Copy { dest: Value(0), src: Operand::Const(IrConst::I32(42)) },
            ],
            terminator: Terminator::Branch(BlockId(2)),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![],
            terminator: Terminator::Return(Some(Operand::Value(Value(0)))),
            source_spans: Vec::new(),
        });

        let count = simplify_cfg(&mut func);
        // No jump threading should occur (block 1 has instructions)
        assert!(matches!(func.blocks[0].terminator, Terminator::Branch(BlockId(1))));
        // But the function still has all 3 blocks
        assert_eq!(func.blocks.len(), 3);
    }

    #[test]
    fn test_cond_branch_threading() {
        // Block 0 cond-branches to Block 1 (empty fwd) and Block 2 (empty fwd),
        // both forward to Block 3
        let mut func = IrFunction::new("test".to_string(), IrType::Void, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Copy { dest: Value(0), src: Operand::Const(IrConst::I32(1)) },
            ],
            terminator: Terminator::CondBranch {
                cond: Operand::Value(Value(0)),
                true_label: BlockId(1),
                false_label: BlockId(2),
            },
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![],
            terminator: Terminator::Branch(BlockId(3)),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![],
            terminator: Terminator::Branch(BlockId(3)),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(3),
            instructions: vec![],
            terminator: Terminator::Return(None),
            source_spans: Vec::new(),
        });

        let count = simplify_cfg(&mut func);
        assert!(count > 0);
        // After threading, Block 0 should go directly to Block 3 for both targets
        // Then redundant cond branch converts to Branch(3)
        assert!(matches!(func.blocks[0].terminator, Terminator::Branch(BlockId(3))));
    }

    #[test]
    fn test_no_thread_when_phi_conflict() {
        // This tests the bug where threading both branches of a conditional
        // through empty forwarding blocks to the same target would lose phi
        // node distinctions.
        //
        // Block 0: CondBranch(cond, true:.L1, false:.L3)
        // Block 1 (.L1): empty, Branch(.L3)
        // Block 3 (.L3): Phi(dest=%8, [(Value(2), .L0), (Const(1), .L1)])
        //
        // The phi distinguishes between the true path (constant 1, via .L1)
        // and the false path (value 2, direct from .L0).
        //
        // Bug: threading .L1 to .L3 would make .L0 go directly to .L3 for
        // both branches, then simplify to unconditional Branch(.L3), losing
        // the Const(1) phi incoming.
        let mut func = IrFunction::new("test".to_string(), IrType::I64, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Cmp {
                    dest: Value(5),
                    op: IrCmpOp::Eq,
                    lhs: Operand::Value(Value(2)),
                    rhs: Operand::Const(IrConst::I64(0)),
                    ty: IrType::I64,
                },
            ],
            terminator: Terminator::CondBranch {
                cond: Operand::Value(Value(5)),
                true_label: BlockId(1),
                false_label: BlockId(3),
            },
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![],
            terminator: Terminator::Branch(BlockId(3)),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(3),
            instructions: vec![
                Instruction::Phi {
                    dest: Value(8),
                    ty: IrType::I64,
                    incoming: vec![
                        (Operand::Value(Value(2)), BlockId(0)),
                        (Operand::Const(IrConst::I64(1)), BlockId(1)),
                    ],
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(8)))),
            source_spans: Vec::new(),
        });

        simplify_cfg(&mut func);

        // The CondBranch must NOT be simplified to an unconditional Branch.
        // The phi must still have two distinct incoming values.
        match &func.blocks[0].terminator {
            Terminator::CondBranch { true_label, false_label, .. } => {
                // The true branch should still go through .L1 (not threaded)
                // to preserve the phi distinction
                assert!(
                    *true_label == BlockId(1) || *false_label != *true_label,
                    "Should not thread both branches to same target when phi has different values"
                );
            }
            Terminator::Branch(_) => {
                panic!("CondBranch was incorrectly simplified to unconditional Branch, losing phi distinction");
            }
            _ => panic!("Unexpected terminator"),
        }

        // Verify the phi still has distinct entries
        let merge_block = func.blocks.iter().find(|b| b.label == BlockId(3)).unwrap();
        if let Instruction::Phi { incoming, .. } = &merge_block.instructions[0] {
            assert!(incoming.len() >= 2, "Phi must retain at least 2 incoming edges");
            // Find the constant 1 entry - it must still be present
            let has_const_1 = incoming.iter().any(|(val, _)| {
                matches!(val, Operand::Const(IrConst::I64(1)))
            });
            assert!(has_const_1, "Phi must still have the Const(1) incoming value");
        } else {
            panic!("Expected Phi instruction in merge block");
        }
    }

    #[test]
    fn test_trivial_phi_simplification() {
        // Simulates the pattern from __builtin_constant_p(v) && expr:
        //
        // Block 0: CondBranch { cond: Const(0), true: Block 1, false: Block 2 }
        // Block 1: (RHS of &&)
        //   %1 = Cmp ...
        //   Branch(Block 2)
        // Block 2: (merge)
        //   %2 = Phi [(Const(0), Block 0), (%1, Block 1)]
        //   CondBranch { cond: %2, true: Block 3, false: Block 4 }
        //
        // After fold_constant_cond_branches folds Block 0's CondBranch (cond=0)
        // to Branch(Block 2) and removes Block 1's phi edge, then
        // remove_dead_blocks eliminates Block 1, the phi becomes:
        //   %2 = Phi [(Const(0), Block 0)]
        //
        // simplify_trivial_phis converts this to:
        //   %2 = Copy Const(0)
        //
        // Note: cfg_simplify alone cannot fold the outer CondBranch in Block 2
        // because fold_constant_cond_branches only matches Operand::Const, not
        // Values defined by Copy. The full pipeline (cfg_simplify -> copy_prop ->
        // cfg_simplify) is needed to complete the elimination. This test verifies
        // only what cfg_simplify accomplishes: the phi-to-Copy conversion and
        // dead block removal.
        let mut func = IrFunction::new("test".to_string(), IrType::I32, vec![], false);

        // Block 0: constant false condition
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![],
            terminator: Terminator::CondBranch {
                cond: Operand::Const(IrConst::I64(0)),
                true_label: BlockId(1),
                false_label: BlockId(2),
            },
            source_spans: Vec::new(),
        });

        // Block 1: RHS of && (will become dead)
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![
                Instruction::Cmp {
                    dest: Value(1),
                    op: IrCmpOp::Ne,
                    lhs: Operand::Value(Value(0)),
                    rhs: Operand::Const(IrConst::I64(0)),
                    ty: IrType::I64,
                },
            ],
            terminator: Terminator::Branch(BlockId(2)),
            source_spans: Vec::new(),
        });

        // Block 2: merge with phi
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![
                Instruction::Phi {
                    dest: Value(2),
                    ty: IrType::I64,
                    incoming: vec![
                        (Operand::Const(IrConst::I64(0)), BlockId(0)),
                        (Operand::Value(Value(1)), BlockId(1)),
                    ],
                },
            ],
            terminator: Terminator::CondBranch {
                cond: Operand::Value(Value(2)),
                true_label: BlockId(3),
                false_label: BlockId(4),
            },
            source_spans: Vec::new(),
        });

        // Block 3: dead branch (e.g., __field_overflow call)
        func.blocks.push(BasicBlock {
            label: BlockId(3),
            instructions: vec![],
            terminator: Terminator::Return(Some(Operand::Const(IrConst::I32(1)))),
            source_spans: Vec::new(),
        });

        // Block 4: continuation
        func.blocks.push(BasicBlock {
            label: BlockId(4),
            instructions: vec![],
            terminator: Terminator::Return(Some(Operand::Const(IrConst::I32(0)))),
            source_spans: Vec::new(),
        });

        let _count = simplify_cfg(&mut func);

        // Verify Block 1 (the RHS of &&) is eliminated as unreachable
        let has_block_1 = func.blocks.iter().any(|b| b.label == BlockId(1));
        assert!(!has_block_1, "Dead block 1 (RHS of &&) should be eliminated");

        // Verify the phi in Block 2 was converted to a Copy
        let block_2 = func.blocks.iter().find(|b| b.label == BlockId(2)).unwrap();
        match &block_2.instructions[0] {
            Instruction::Copy { dest, src } => {
                assert_eq!(dest.0, 2);
                assert!(matches!(src, Operand::Const(IrConst::I64(0))),
                    "Copy source should be Const(0)");
            }
            other => panic!("Expected Copy instruction, got {:?}", other),
        }
    }

    #[test]
    fn test_fold_constant_switch() {
        // Switch { val: Const(37), cases: [(10,.L1),(20,.L2),(30,.L3)], default: .L4 }
        // should fold to Branch(.L4) since 37 doesn't match any case.
        let mut func = IrFunction::new("test".to_string(), IrType::I32, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![],
            terminator: Terminator::Switch {
                val: Operand::Const(IrConst::I64(37)),
                cases: vec![(10, BlockId(1)), (20, BlockId(2)), (30, BlockId(3))],
                default: BlockId(4),
            },
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![],
            terminator: Terminator::Return(Some(Operand::Const(IrConst::I32(100)))),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![],
            terminator: Terminator::Return(Some(Operand::Const(IrConst::I32(200)))),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(3),
            instructions: vec![],
            terminator: Terminator::Return(Some(Operand::Const(IrConst::I32(300)))),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(4),
            instructions: vec![],
            terminator: Terminator::Return(Some(Operand::Const(IrConst::I32(-1)))),
            source_spans: Vec::new(),
        });

        let count = simplify_cfg(&mut func);
        assert!(count > 0, "Should have made simplifications");
        // Block 0 should now branch directly to default (Block 4)
        assert!(matches!(func.blocks[0].terminator, Terminator::Branch(BlockId(4))),
            "Switch on constant 37 should fold to Branch(default=Block4)");
        // Dead blocks should be eliminated
        assert!(!func.blocks.iter().any(|b| b.label == BlockId(1)),
            "Block 1 (case 10) should be dead");
        assert!(!func.blocks.iter().any(|b| b.label == BlockId(2)),
            "Block 2 (case 20) should be dead");
        assert!(!func.blocks.iter().any(|b| b.label == BlockId(3)),
            "Block 3 (case 30) should be dead");
    }

    #[test]
    fn test_fold_constant_switch_matching_case() {
        // Switch { val: Const(20), cases: [(10,.L1),(20,.L2),(30,.L3)], default: .L4 }
        // should fold to Branch(.L2) since 20 matches case 20.
        let mut func = IrFunction::new("test".to_string(), IrType::I32, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![],
            terminator: Terminator::Switch {
                val: Operand::Const(IrConst::I64(20)),
                cases: vec![(10, BlockId(1)), (20, BlockId(2)), (30, BlockId(3))],
                default: BlockId(4),
            },
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![],
            terminator: Terminator::Return(Some(Operand::Const(IrConst::I32(100)))),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![],
            terminator: Terminator::Return(Some(Operand::Const(IrConst::I32(200)))),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(3),
            instructions: vec![],
            terminator: Terminator::Return(Some(Operand::Const(IrConst::I32(300)))),
            source_spans: Vec::new(),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(4),
            instructions: vec![],
            terminator: Terminator::Return(Some(Operand::Const(IrConst::I32(-1)))),
            source_spans: Vec::new(),
        });

        let count = simplify_cfg(&mut func);
        assert!(count > 0, "Should have made simplifications");
        // Block 0 should now branch directly to case 20 (Block 2)
        assert!(matches!(func.blocks[0].terminator, Terminator::Branch(BlockId(2))),
            "Switch on constant 20 should fold to Branch(Block2)");
        // Other case blocks should be dead
        assert!(!func.blocks.iter().any(|b| b.label == BlockId(1)),
            "Block 1 (case 10) should be dead");
        assert!(!func.blocks.iter().any(|b| b.label == BlockId(3)),
            "Block 3 (case 30) should be dead");
        assert!(!func.blocks.iter().any(|b| b.label == BlockId(4)),
            "Block 4 (default) should be dead");
    }

    #[test]
    fn test_fold_constant_switch_phi_cleanup() {
        // Switch with a phi in the default block. When folding the switch
        // to take a case branch, the phi entry for the switch block must be
        // removed from the not-taken default block.
        //
        // Block 0: cond branch to Block 1 or Block 3
        // Block 1: constant switch -> case 10: Block 2, default: Block 3
        // Block 2: return 42
        // Block 3: phi from Block 0 and Block 1, return phi result
        let mut func = IrFunction::new("test".to_string(), IrType::I32, vec![], false);

        // Block 0: cond branch to Block 1 (switch) or Block 3 (default directly)
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Copy { dest: Value(1), src: Operand::Const(IrConst::I32(1)) },
            ],
            terminator: Terminator::CondBranch {
                cond: Operand::Value(Value(1)),
                true_label: BlockId(1),
                false_label: BlockId(3),
            },
            source_spans: Vec::new(),
        });

        // Block 1: constant switch - value 10 matches case 10 -> Block 2
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![],
            terminator: Terminator::Switch {
                val: Operand::Const(IrConst::I64(10)),
                cases: vec![(10, BlockId(2))],
                default: BlockId(3),
            },
            source_spans: Vec::new(),
        });

        // Block 2: the taken case
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![],
            terminator: Terminator::Return(Some(Operand::Const(IrConst::I32(42)))),
            source_spans: Vec::new(),
        });

        // Block 3: default block with phi - has entries from Block 0 and Block 1
        func.blocks.push(BasicBlock {
            label: BlockId(3),
            instructions: vec![
                Instruction::Phi {
                    dest: Value(0),
                    ty: IrType::I32,
                    incoming: vec![
                        (Operand::Const(IrConst::I32(1)), BlockId(0)),
                        (Operand::Const(IrConst::I32(2)), BlockId(1)),
                    ],
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(0)))),
            source_spans: Vec::new(),
        });

        let count = simplify_cfg(&mut func);
        assert!(count > 0, "Should have made simplifications");

        // Block 3 must still exist (reachable from Block 0's false branch)
        let block3 = func.blocks.iter().find(|b| b.label == BlockId(3)).unwrap();
        // Block 3's phi should NOT have an entry from Block 1 (the switch was
        // folded to take case 10 -> Block 2, so Block 1 no longer branches to Block 3)
        match &block3.instructions[0] {
            Instruction::Phi { incoming, .. } => {
                assert!(!incoming.iter().any(|(_, label)| *label == BlockId(1)),
                    "Phi should not have entry from Block 1 after switch fold");
            }
            Instruction::Copy { .. } => {
                // After dead block elimination of Block 1, the phi may have been
                // simplified to a Copy (single incoming from Block 0). This is fine.
            }
            other => panic!("Expected Phi or Copy instruction in Block 3, got {:?}", other),
        }
    }
}
