//! Stack layout: slot assignment, alloca coalescing, and regalloc helpers.
//!
//! This module contains the stack space calculation infrastructure:
//! - `calculate_stack_space_common`: three-tier stack slot allocation
//! - `compute_coalescable_allocas`: escape analysis for alloca coalescing
//! - `collect_inline_asm_callee_saved`: shared ASM clobber scan
//! - `run_regalloc_and_merge_clobbers`: register allocator + clobber merge
//! - `filter_available_regs`: callee-saved register filtering
//! - `find_param_alloca`: parameter alloca lookup

use crate::ir::ir::*;
use crate::common::types::IrType;
use crate::common::fx_hash::{FxHashMap, FxHashSet};
use super::state::StackSlot;
use super::liveness::{
    for_each_operand_in_instruction, for_each_value_use_in_instruction,
    for_each_operand_in_terminator, compute_live_intervals,
};

/// Compute the "use-block map" for all values in the function.
/// For each value, records the set of block indices where that value is referenced.
/// This is used to determine:
/// - For entry-block allocas: which blocks reference the alloca pointer
/// - For non-alloca values: whether the value is used outside its defining block
///
/// Phi node operands are attributed to their SOURCE block (not the Phi's block).
/// This is critical for coalescing in dispatch-style code (switch/computed goto):
/// a value defined in block A that flows through a Phi to block B is semantically
/// "used at the end of block A" (where the branch resolves the Phi). Attributing
/// Phi uses to the source block allows such values to be considered block-local
/// to A, enabling stack slot coalescing across case handlers.
fn compute_value_use_blocks(func: &IrFunction) -> FxHashMap<u32, Vec<usize>> {
    let mut uses: FxHashMap<u32, Vec<usize>> = FxHashMap::default();

    let mut record_use = |id: u32, block_idx: usize, uses: &mut FxHashMap<u32, Vec<usize>>| {
        let blocks = uses.entry(id).or_insert_with(Vec::new);
        if blocks.last() != Some(&block_idx) {
            blocks.push(block_idx);
        }
    };

    // Build BlockId -> block_index map for Phi source block resolution.
    let block_id_to_idx: FxHashMap<u32, usize> = func.blocks.iter().enumerate()
        .map(|(idx, b)| (b.label.0, idx))
        .collect();

    for (block_idx, block) in func.blocks.iter().enumerate() {
        for inst in &block.instructions {
            // Handle Phi nodes specially: attribute each operand's use to its
            // source block rather than the block containing the Phi. This reflects
            // SSA semantics where Phi inputs are "evaluated" at the end of the
            // predecessor block, not at the start of the Phi's block.
            if let Instruction::Phi { incoming, .. } = inst {
                for (op, src_block_id) in incoming {
                    if let Operand::Value(v) = op {
                        if let Some(&src_idx) = block_id_to_idx.get(&src_block_id.0) {
                            record_use(v.0, src_idx, &mut uses);
                        } else {
                            // Fallback: if source block not found, attribute to Phi's block
                            record_use(v.0, block_idx, &mut uses);
                        }
                    }
                }
            } else {
                for_each_operand_in_instruction(inst, |op| {
                    if let Operand::Value(v) = op {
                        record_use(v.0, block_idx, &mut uses);
                    }
                });
            }
            for_each_value_use_in_instruction(inst, |v| {
                record_use(v.0, block_idx, &mut uses);
            });
        }
        for_each_operand_in_terminator(&block.terminator, |op| {
            if let Operand::Value(v) = op {
                record_use(v.0, block_idx, &mut uses);
            }
        });
    }
    uses
}

/// Result of alloca coalescability analysis.
struct CoalescableAllocas {
    /// Single-block allocas: alloca ID -> the single block index where it's used.
    /// These can use block-local coalescing (Tier 3).
    single_block: FxHashMap<u32, usize>,
    /// Dead non-param allocas: alloca IDs with no uses at all.
    /// These can be skipped entirely (no stack slot needed).
    dead: FxHashSet<u32>,
}

/// Compute which allocas can be coalesced (either block-locally or via liveness packing).
///
/// An alloca is coalescable if:
/// 1. It is not a parameter alloca and not dead
/// 2. Its address never "escapes" — it's never stored as a value,
///    used in casts/binops/phi/select, or used in terminators
/// 3. No GEP derived from it escapes either (transitive check)
///
/// Single-block allocas (all uses in one block) use block-local coalescing.
/// Multi-block allocas (uses span multiple blocks) use liveness-based packing
/// based on their [min_use_block, max_use_block] range.
fn compute_coalescable_allocas(
    func: &IrFunction,
    dead_param_allocas: &FxHashSet<u32>,
    param_alloca_values: &[Value],
) -> CoalescableAllocas {
    // Collect all alloca value IDs (excluding dead params and param allocas).
    let param_set: FxHashSet<u32> = param_alloca_values.iter().map(|v| v.0).collect();
    let mut alloca_set: FxHashSet<u32> = FxHashSet::default();
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::Alloca { dest, .. } = inst {
                if !dead_param_allocas.contains(&dest.0) && !param_set.contains(&dest.0) {
                    alloca_set.insert(dest.0);
                }
            }
        }
    }

    if alloca_set.is_empty() {
        return CoalescableAllocas { single_block: FxHashMap::default(), dead: FxHashSet::default() };
    }

    // Track which allocas are "escaped" (address leaked to a call, stored, etc.)
    let mut escaped: FxHashSet<u32> = FxHashSet::default();

    // Track GEP chains: map from GEP result -> the root alloca it was derived from.
    // This allows us to transitively mark an alloca as escaped if a GEP derived from
    // it escapes.
    let mut gep_to_alloca: FxHashMap<u32, u32> = FxHashMap::default();

    // Track use blocks for each alloca: alloca_id -> set of block indices where used.
    let mut alloca_use_blocks: FxHashMap<u32, Vec<usize>> = FxHashMap::default();

    // First pass: build GEP chain map and detect escapes.
    for (block_idx, block) in func.blocks.iter().enumerate() {
        for inst in &block.instructions {
            // Build GEP -> root alloca mapping
            if let Instruction::GetElementPtr { dest, base, .. } = inst {
                // Check if the base is an alloca or a GEP derived from an alloca
                let root = if alloca_set.contains(&base.0) {
                    Some(base.0)
                } else {
                    gep_to_alloca.get(&base.0).copied()
                };
                if let Some(root_alloca) = root {
                    gep_to_alloca.insert(dest.0, root_alloca);
                    // Record this use of the alloca
                    let blocks = alloca_use_blocks.entry(root_alloca).or_insert_with(Vec::new);
                    if blocks.last() != Some(&block_idx) {
                        blocks.push(block_idx);
                    }
                }
            }

            // Check for alloca/GEP-derived values used in various instructions.
            // Alloca pointers passed to calls are marked as escaped since the
            // callee may store the pointer in a data structure that outlives the call.
            match inst {
                Instruction::Call { args, .. } | Instruction::CallIndirect { args, .. } => {
                    // Mark allocas passed as call arguments as escaped.
                    // The callee may store the pointer in a data structure that
                    // outlives the call (e.g. Lua GC roots, linked lists, callbacks).
                    // Coalescing such allocas with others can cause memory corruption.
                    for arg in args {
                        if let Operand::Value(v) = arg {
                            if alloca_set.contains(&v.0) {
                                escaped.insert(v.0);
                                let blocks = alloca_use_blocks.entry(v.0).or_insert_with(Vec::new);
                                if blocks.last() != Some(&block_idx) { blocks.push(block_idx); }
                            } else if let Some(&root) = gep_to_alloca.get(&v.0) {
                                escaped.insert(root);
                                let blocks = alloca_use_blocks.entry(root).or_insert_with(Vec::new);
                                if blocks.last() != Some(&block_idx) { blocks.push(block_idx); }
                            }
                        }
                    }
                }
                // Store: if the alloca's VALUE is stored (not as ptr), address escapes
                Instruction::Store { val, ptr, .. } => {
                    if let Operand::Value(v) = val {
                        if alloca_set.contains(&v.0) {
                            escaped.insert(v.0);
                        } else if let Some(&root) = gep_to_alloca.get(&v.0) {
                            escaped.insert(root);
                        }
                    }
                    // Record use of alloca as ptr (non-escaping use)
                    if alloca_set.contains(&ptr.0) {
                        let blocks = alloca_use_blocks.entry(ptr.0).or_insert_with(Vec::new);
                        if blocks.last() != Some(&block_idx) {
                            blocks.push(block_idx);
                        }
                    } else if let Some(&root) = gep_to_alloca.get(&ptr.0) {
                        let blocks = alloca_use_blocks.entry(root).or_insert_with(Vec::new);
                        if blocks.last() != Some(&block_idx) {
                            blocks.push(block_idx);
                        }
                    }
                }
                // Load: record use of alloca as ptr (non-escaping use)
                Instruction::Load { ptr, .. } => {
                    if alloca_set.contains(&ptr.0) {
                        let blocks = alloca_use_blocks.entry(ptr.0).or_insert_with(Vec::new);
                        if blocks.last() != Some(&block_idx) {
                            blocks.push(block_idx);
                        }
                    } else if let Some(&root) = gep_to_alloca.get(&ptr.0) {
                        let blocks = alloca_use_blocks.entry(root).or_insert_with(Vec::new);
                        if blocks.last() != Some(&block_idx) {
                            blocks.push(block_idx);
                        }
                    }
                }
                // Memcpy: record use of alloca as dest/src (non-escaping use)
                Instruction::Memcpy { dest, src, .. } => {
                    for v in [dest, src] {
                        if alloca_set.contains(&v.0) {
                            let blocks = alloca_use_blocks.entry(v.0).or_insert_with(Vec::new);
                            if blocks.last() != Some(&block_idx) {
                                blocks.push(block_idx);
                            }
                        } else if let Some(&root) = gep_to_alloca.get(&v.0) {
                            let blocks = alloca_use_blocks.entry(root).or_insert_with(Vec::new);
                            if blocks.last() != Some(&block_idx) {
                                blocks.push(block_idx);
                            }
                        }
                    }
                }
                // InlineAsm: outputs and memory-constraint inputs are uses of the alloca.
                // The asm block reads/writes through the pointer during execution.
                // However, if an alloca's address is used as an immediate/"i"
                // constraint input, it could theoretically be embedded in code, but
                // this is extremely rare for local allocas and we still coalesce
                // only across blocks that never execute simultaneously.
                Instruction::InlineAsm { outputs, inputs, .. } => {
                    for (_, v, _) in outputs {
                        if alloca_set.contains(&v.0) {
                            let blocks = alloca_use_blocks.entry(v.0).or_insert_with(Vec::new);
                            if blocks.last() != Some(&block_idx) { blocks.push(block_idx); }
                        } else if let Some(&root) = gep_to_alloca.get(&v.0) {
                            let blocks = alloca_use_blocks.entry(root).or_insert_with(Vec::new);
                            if blocks.last() != Some(&block_idx) { blocks.push(block_idx); }
                        }
                    }
                    for (_, op, _) in inputs {
                        if let Operand::Value(v) = op {
                            if alloca_set.contains(&v.0) {
                                let blocks = alloca_use_blocks.entry(v.0).or_insert_with(Vec::new);
                                if blocks.last() != Some(&block_idx) { blocks.push(block_idx); }
                            } else if let Some(&root) = gep_to_alloca.get(&v.0) {
                                let blocks = alloca_use_blocks.entry(root).or_insert_with(Vec::new);
                                if blocks.last() != Some(&block_idx) { blocks.push(block_idx); }
                            }
                        }
                    }
                }
                // Intrinsic: record uses (intrinsics like memset/memcpy just operate
                // on the pointer during execution, they don't store it)
                Instruction::Intrinsic { dest_ptr, args, .. } => {
                    if let Some(dp) = dest_ptr {
                        if alloca_set.contains(&dp.0) {
                            let blocks = alloca_use_blocks.entry(dp.0).or_insert_with(Vec::new);
                            if blocks.last() != Some(&block_idx) { blocks.push(block_idx); }
                        } else if let Some(&root) = gep_to_alloca.get(&dp.0) {
                            let blocks = alloca_use_blocks.entry(root).or_insert_with(Vec::new);
                            if blocks.last() != Some(&block_idx) { blocks.push(block_idx); }
                        }
                    }
                    for arg in args {
                        if let Operand::Value(v) = arg {
                            if alloca_set.contains(&v.0) {
                                let blocks = alloca_use_blocks.entry(v.0).or_insert_with(Vec::new);
                                if blocks.last() != Some(&block_idx) { blocks.push(block_idx); }
                            } else if let Some(&root) = gep_to_alloca.get(&v.0) {
                                let blocks = alloca_use_blocks.entry(root).or_insert_with(Vec::new);
                                if blocks.last() != Some(&block_idx) { blocks.push(block_idx); }
                            }
                        }
                    }
                }
                // Atomic ops: if alloca is used as ptr, record use. If used as val, it escapes.
                Instruction::AtomicRmw { ptr, val, .. } => {
                    if let Operand::Value(v) = val {
                        if alloca_set.contains(&v.0) { escaped.insert(v.0); }
                        else if let Some(&root) = gep_to_alloca.get(&v.0) { escaped.insert(root); }
                    }
                    if let Operand::Value(v) = ptr {
                        if alloca_set.contains(&v.0) {
                            let blocks = alloca_use_blocks.entry(v.0).or_insert_with(Vec::new);
                            if blocks.last() != Some(&block_idx) { blocks.push(block_idx); }
                        }
                    }
                }
                Instruction::AtomicCmpxchg { ptr, expected, desired, .. } => {
                    for op in [expected, desired] {
                        if let Operand::Value(v) = op {
                            if alloca_set.contains(&v.0) { escaped.insert(v.0); }
                            else if let Some(&root) = gep_to_alloca.get(&v.0) { escaped.insert(root); }
                        }
                    }
                    if let Operand::Value(v) = ptr {
                        if alloca_set.contains(&v.0) {
                            let blocks = alloca_use_blocks.entry(v.0).or_insert_with(Vec::new);
                            if blocks.last() != Some(&block_idx) { blocks.push(block_idx); }
                        }
                    }
                }
                Instruction::AtomicLoad { ptr, .. } => {
                    if let Operand::Value(v) = ptr {
                        if alloca_set.contains(&v.0) {
                            let blocks = alloca_use_blocks.entry(v.0).or_insert_with(Vec::new);
                            if blocks.last() != Some(&block_idx) { blocks.push(block_idx); }
                        } else if let Some(&root) = gep_to_alloca.get(&v.0) {
                            let blocks = alloca_use_blocks.entry(root).or_insert_with(Vec::new);
                            if blocks.last() != Some(&block_idx) { blocks.push(block_idx); }
                        }
                    }
                }
                Instruction::AtomicStore { ptr, val, .. } => {
                    if let Operand::Value(v) = val {
                        if alloca_set.contains(&v.0) { escaped.insert(v.0); }
                        else if let Some(&root) = gep_to_alloca.get(&v.0) { escaped.insert(root); }
                    }
                    if let Operand::Value(v) = ptr {
                        if alloca_set.contains(&v.0) {
                            let blocks = alloca_use_blocks.entry(v.0).or_insert_with(Vec::new);
                            if blocks.last() != Some(&block_idx) { blocks.push(block_idx); }
                        }
                    }
                }
                // VaStart/VaEnd/VaCopy/VaArg: conservatively escape
                Instruction::VaStart { va_list_ptr } | Instruction::VaEnd { va_list_ptr } | Instruction::VaArg { va_list_ptr, .. } => {
                    if alloca_set.contains(&va_list_ptr.0) {
                        escaped.insert(va_list_ptr.0);
                    }
                }
                Instruction::VaCopy { dest_ptr, src_ptr } => {
                    if alloca_set.contains(&dest_ptr.0) { escaped.insert(dest_ptr.0); }
                    if alloca_set.contains(&src_ptr.0) { escaped.insert(src_ptr.0); }
                }
                // Cast/Copy/BinOp/Cmp/Select/UnaryOp: if alloca value used as operand, it escapes
                // (these compute on the address value, which means it could be stored later)
                Instruction::Cast { src, .. } | Instruction::Copy { src, .. } | Instruction::UnaryOp { src, .. } => {
                    if let Operand::Value(v) = src {
                        if alloca_set.contains(&v.0) { escaped.insert(v.0); }
                        else if let Some(&root) = gep_to_alloca.get(&v.0) { escaped.insert(root); }
                    }
                }
                Instruction::BinOp { lhs, rhs, .. } | Instruction::Cmp { lhs, rhs, .. } => {
                    for op in [lhs, rhs] {
                        if let Operand::Value(v) = op {
                            if alloca_set.contains(&v.0) { escaped.insert(v.0); }
                            else if let Some(&root) = gep_to_alloca.get(&v.0) { escaped.insert(root); }
                        }
                    }
                }
                Instruction::Select { cond, true_val, false_val, .. } => {
                    for op in [cond, true_val, false_val] {
                        if let Operand::Value(v) = op {
                            if alloca_set.contains(&v.0) { escaped.insert(v.0); }
                            else if let Some(&root) = gep_to_alloca.get(&v.0) { escaped.insert(root); }
                        }
                    }
                }
                Instruction::Phi { incoming, .. } => {
                    for (op, _) in incoming {
                        if let Operand::Value(v) = op {
                            if alloca_set.contains(&v.0) { escaped.insert(v.0); }
                            else if let Some(&root) = gep_to_alloca.get(&v.0) { escaped.insert(root); }
                        }
                    }
                }
                // Other instructions that don't use allocas
                _ => {}
            }
        }

        // Check terminators for alloca/GEP-derived value uses
        for_each_operand_in_terminator(&block.terminator, |op| {
            if let Operand::Value(v) = op {
                if alloca_set.contains(&v.0) {
                    escaped.insert(v.0);
                } else if let Some(&root) = gep_to_alloca.get(&v.0) {
                    escaped.insert(root);
                }
            }
        });
    }

    // Build result: separate non-escaped allocas into single-block and dead.
    // Multi-block non-escaping allocas are not coalesced (they get permanent slots)
    // because block-index-based interval packing is unsound with loops.
    let mut single_block: FxHashMap<u32, usize> = FxHashMap::default();
    let mut dead: FxHashSet<u32> = FxHashSet::default();
    for &alloca_id in &alloca_set {
        if escaped.contains(&alloca_id) {
            continue;
        }
        if let Some(blocks) = alloca_use_blocks.get(&alloca_id) {
            let mut unique: Vec<usize> = blocks.clone();
            unique.sort_unstable();
            unique.dedup();
            if unique.len() == 1 {
                single_block.insert(alloca_id, unique[0]);
            }
            // Multi-block allocas fall through: not coalesced, get permanent slots.
        } else {
            // Alloca with no uses at all - completely dead, skip allocation.
            dead.insert(alloca_id);
        }
    }

    CoalescableAllocas { single_block, dead }
}

/// Scan inline-asm instructions for callee-saved register usage.
///
/// Iterates over all inline-asm instructions in `func`, checking output/input
/// constraints and clobber lists for callee-saved registers.  Uses two callbacks:
/// - `constraint_to_phys`: maps an output/input constraint string to a PhysReg
/// - `clobber_to_phys`: maps a clobber register name to a PhysReg
///
/// Any discovered callee-saved PhysRegs are appended to `used` (deduplicated).
/// This shared helper eliminates duplicated scan loops in x86 and RISC-V.
pub fn collect_inline_asm_callee_saved(
    func: &IrFunction,
    used: &mut Vec<super::regalloc::PhysReg>,
    constraint_to_phys: impl Fn(&str) -> Option<super::regalloc::PhysReg>,
    clobber_to_phys: impl Fn(&str) -> Option<super::regalloc::PhysReg>,
) {
    let mut already: FxHashSet<u8> = used.iter().map(|r| r.0).collect();
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::InlineAsm { outputs, inputs, clobbers, .. } = inst {
                for (constraint, _, _) in outputs {
                    let c = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
                    if let Some(phys) = constraint_to_phys(c) {
                        if already.insert(phys.0) { used.push(phys); }
                    }
                }
                for (constraint, _, _) in inputs {
                    let c = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
                    if let Some(phys) = constraint_to_phys(c) {
                        if already.insert(phys.0) { used.push(phys); }
                    }
                }
                for clobber in clobbers {
                    if let Some(phys) = clobber_to_phys(clobber.as_str()) {
                        if already.insert(phys.0) { used.push(phys); }
                    }
                }
            }
        }
    }
}

/// Run register allocation and merge ASM-clobbered callee-saved registers.
///
/// This shared helper eliminates duplicated regalloc setup boilerplate across
/// all three backends (x86, ARM, RISC-V).  Each backend supplies its callee-saved
/// register list and pre-collected ASM clobber list; this function handles the
/// common steps: filtering available registers, running the allocator, storing
/// results, merging clobbers into `used_callee_saved`, and building the
/// `reg_assigned` set.
///
/// Returns `(reg_assigned, cached_liveness)` for use by `calculate_stack_space_common`.
pub fn run_regalloc_and_merge_clobbers(
    func: &IrFunction,
    available_regs: Vec<super::regalloc::PhysReg>,
    asm_clobbered_regs: &[super::regalloc::PhysReg],
    reg_assignments: &mut FxHashMap<u32, super::regalloc::PhysReg>,
    used_callee_saved: &mut Vec<super::regalloc::PhysReg>,
) -> (FxHashSet<u32>, Option<super::liveness::LivenessResult>) {
    let config = super::regalloc::RegAllocConfig { available_regs };
    let alloc_result = super::regalloc::allocate_registers(func, &config);
    *reg_assignments = alloc_result.assignments;
    *used_callee_saved = alloc_result.used_regs;
    let cached_liveness = alloc_result.liveness;

    // Merge inline-asm clobbered callee-saved registers into the save/restore
    // list (they need to be preserved per the ABI even though we don't allocate
    // values to them).
    for phys in asm_clobbered_regs {
        if !used_callee_saved.iter().any(|r| r.0 == phys.0) {
            used_callee_saved.push(*phys);
        }
    }
    used_callee_saved.sort_by_key(|r| r.0);

    let reg_assigned: FxHashSet<u32> = reg_assignments.keys().copied().collect();
    (reg_assigned, cached_liveness)
}

/// Filter a callee-saved register list by removing ASM-clobbered entries.
/// Returns the filtered list suitable for passing to `run_regalloc_and_merge_clobbers`.
pub fn filter_available_regs(
    callee_saved: &[super::regalloc::PhysReg],
    asm_clobbered: &[super::regalloc::PhysReg],
) -> Vec<super::regalloc::PhysReg> {
    let mut available = callee_saved.to_vec();
    if !asm_clobbered.is_empty() {
        let clobbered_set: FxHashSet<u8> = asm_clobbered.iter().map(|r| r.0).collect();
        available.retain(|r| !clobbered_set.contains(&r.0));
    }
    available
}

/// Shared stack space calculation: iterates over all instructions, assigns stack
/// slots for allocas and value results. Arch-specific offset direction is handled
/// by the `assign_slot` closure.
///
/// Allocas may be coalesced if their address never escapes and all uses are in a
/// single basic block. Non-escaping single-block allocas share stack space with
/// allocas from other blocks (block-local coalescing).
///
/// Non-alloca SSA temporaries may be coalesced: values defined and used only
/// within a single basic block (including the entry block) share stack space
/// with temporaries from other blocks. This dramatically reduces frame size for
/// functions with large switch statements (e.g., bison parsers, Lua's VM
/// interpreter) where thousands of case blocks each produce many block-local
/// temporaries that never coexist.
///
/// `initial_offset`: starting offset (e.g., 0 for x86, 16 for ARM/RISC-V to skip saved regs)
/// `assign_slot`: maps (current_space, raw_alloca_size, alignment) -> (slot_offset, new_space)
pub fn calculate_stack_space_common(
    state: &mut super::state::CodegenState,
    func: &IrFunction,
    initial_offset: i64,
    assign_slot: impl Fn(i64, i64, i64) -> (i64, i64),
    reg_assigned: &FxHashSet<u32>,
    cached_liveness: Option<super::liveness::LivenessResult>,
) -> i64 {
    let num_blocks = func.blocks.len();

    // Enable block-local coalescing (Tier 3) for all multi-block functions.
    // Enable liveness-based packing (Tier 2) for all multi-block functions.
    //
    // Tier 3 is safe for all functions: single-block values share stack space
    // across different blocks since only one block's values are live at a time.
    // Tier 2 uses liveness intervals to pack multi-block values into shared slots;
    // values with non-overlapping live intervals share the same stack slot.
    //
    // Both tiers are enabled for any function with >= 2 blocks. Even small
    // functions benefit: a 3-block function with 20 intermediates can save
    // 100+ bytes of frame space by sharing slots. This is critical for
    // recursive functions where per-frame savings compound (e.g., PostgreSQL
    // plpgsql recursion triggers stack depth limit with large frames) and for
    // kernel functions that expand macros creating many short-lived multi-block
    // intermediates (e.g., list_for_each_entry_safe, spin_lock).
    let coalesce = num_blocks >= 2;
    let enable_tier2 = num_blocks >= 2;

    // Build use-block map: for each value, which blocks reference it.
    let use_blocks_map = if coalesce {
        compute_value_use_blocks(func)
    } else {
        FxHashMap::default()
    };

    // Map from value ID -> the block where it's defined.
    // After phi elimination, a single-phi destination can be defined via Copy
    // in multiple predecessor blocks. Track these multi-definition values so
    // they are never misclassified as block-local (Tier 3).
    let mut def_block: FxHashMap<u32, usize> = FxHashMap::default();
    let mut multi_def_values: FxHashSet<u32> = FxHashSet::default();
    if coalesce {
        for (block_idx, block) in func.blocks.iter().enumerate() {
            for inst in &block.instructions {
                if let Some(dest) = inst.dest() {
                    if let Some(&prev_blk) = def_block.get(&dest.0) {
                        if prev_blk != block_idx {
                            multi_def_values.insert(dest.0);
                        }
                    }
                    def_block.insert(dest.0, block_idx);
                }
            }
        }
    }

    // Determine if a non-alloca value can be assigned to a block-local pool slot.
    // Returns Some(def_block_idx) if the value is defined and used only within a
    // single block (including the entry block), making it safe to share stack
    // space with values from other blocks (since only one block executes at a time).
    let coalescable_group = |val_id: u32| -> Option<usize> {
        if !coalesce {
            return None;
        }
        // Values defined in multiple blocks (from phi elimination) must use
        // liveness-based packing (Tier 2), never block-local coalescing (Tier 3).
        if multi_def_values.contains(&val_id) {
            return None;
        }
        if let Some(&def_blk) = def_block.get(&val_id) {
            if let Some(blocks) = use_blocks_map.get(&val_id) {
                let mut unique: Vec<usize> = blocks.clone();
                unique.sort_unstable();
                unique.dedup();

                if unique.is_empty() {
                    return Some(def_blk); // Dead value, safe to coalesce.
                }

                // Single-block value (defined and used in the same block).
                // Safe to share a pool slot: each block gets its own pool, and
                // pools from different blocks share the same stack offset because
                // only one block executes at a time.
                // This includes entry block (def_blk == 0) values that are purely
                // local to the entry block -- these are common intermediate results
                // (sign extensions, casts, arithmetic) that don't cross block
                // boundaries and can safely share pool slots.
                if unique.len() == 1 && unique[0] == def_blk {
                    return Some(def_blk);
                }

            } else {
                return Some(def_blk); // No uses - dead value.
            }
        }
        None
    };

    // Three-tier allocation:
    // Tier 1: Allocas get permanent slots (addressable memory).
    // Tier 2: Multi-block non-alloca values use liveness-based packing: values
    //         with non-overlapping live intervals share the same stack slot.
    // Tier 3: Single-block non-alloca values use block-local coalescing (as before).

    struct DeferredSlot {
        dest_id: u32,
        size: i64,
        align: i64,
        block_offset: i64,
    }

    // Multi-block value pending liveness-based packing.
    struct MultiBlockValue {
        dest_id: u32,
        slot_size: i64,
    }

    // Collect ALL Value IDs referenced as operands anywhere in the function body.
    // Used for both dead param alloca detection and dead value elimination.
    let used_values: FxHashSet<u32> = {
        let mut used: FxHashSet<u32> = FxHashSet::default();
        for block in &func.blocks {
            for inst in &block.instructions {
                for_each_operand_in_instruction(inst, |op| {
                    if let Operand::Value(v) = op { used.insert(v.0); }
                });
                for_each_value_use_in_instruction(inst, |v| { used.insert(v.0); });
            }
            for_each_operand_in_terminator(&block.terminator, |op| {
                if let Operand::Value(v) = op { used.insert(v.0); }
            });
        }
        used
    };

    // Dead param alloca detection: find param allocas whose Value IDs are never
    // used in any instruction or terminator. These represent function parameters
    // that are completely unused in the function body, so we can skip allocating
    // stack space for them and skip storing incoming register values.
    let dead_param_allocas: FxHashSet<u32> = {
        let mut dead = FxHashSet::default();
        if !func.param_alloca_values.is_empty() {
            let param_set: FxHashSet<u32> = func.param_alloca_values.iter().map(|v| v.0).collect();
            for &pv in &param_set {
                if !used_values.contains(&pv) {
                    dead.insert(pv);
                }
            }
        }
        dead
    };

    // Tell CodegenState which values are register-assigned so that
    // resolve_slot_addr can return a dummy Indirect slot for them.
    state.reg_assigned_values = reg_assigned.clone();

    // Alloca coalescing: identify allocas whose address never escapes and whose
    // uses are confined to a single basic block. These can share stack space with
    // allocas from other blocks (block-local coalescing), dramatically reducing
    // frame size for functions with many inlined helper calls.
    //
    // An alloca is "non-escaping" if:
    // - Its Value is never used as a call/callindirect argument
    // - Its Value is never stored as a value (only used as ptr in load/store/gep/memcpy)
    // - Its Value is never used in inline asm inputs
    // - Its Value is never returned or used in terminators
    // - No GEP derived from it has any of the above escape conditions
    //
    // A non-escaping alloca is "single-block" if all uses of it (and all GEPs
    // derived from it) are in the same basic block.
    let coalescable_allocas: CoalescableAllocas = if coalesce {
        compute_coalescable_allocas(func, &dead_param_allocas, &func.param_alloca_values)
    } else {
        CoalescableAllocas { single_block: FxHashMap::default(), dead: FxHashSet::default() }
    };

    // ── Copy coalescing ──────────────────────────────────────────────
    // When we see `dest = Copy src_value`, dest and src can share the same
    // stack slot if their live ranges don't interfere. This eliminates a
    // separate slot allocation and makes the Copy a harmless self-move.
    //
    // Safety: we only coalesce when the Copy is the SOLE use of the source
    // value. This guarantees the source is dead after the Copy, so sharing
    // the slot cannot corrupt any other reader of the source. This avoids
    // the "lost copy" problem in phi parallel copy groups (e.g., swap patterns).
    let mut copy_alias: FxHashMap<u32, u32> = FxHashMap::default();
    {
        // Count uses of each value across all instructions (as operands).
        let mut use_count: FxHashMap<u32, u32> = FxHashMap::default();
        for block in &func.blocks {
            for inst in &block.instructions {
                super::liveness::for_each_operand_in_instruction(inst, |op| {
                    if let Operand::Value(v) = op {
                        *use_count.entry(v.0).or_insert(0) += 1;
                    }
                });
                super::liveness::for_each_value_use_in_instruction(inst, |v| {
                    *use_count.entry(v.0).or_insert(0) += 1;
                });
            }
            // Also count uses in terminators.
            super::liveness::for_each_operand_in_terminator(&block.terminator, |op| {
                if let Operand::Value(v) = op {
                    *use_count.entry(v.0).or_insert(0) += 1;
                }
            });
        }

        // Collect all Copy instructions where src is a Value (not a constant)
        // and the Copy is the sole use of the source.
        let mut raw_aliases: Vec<(u32, u32)> = Vec::new();
        for block in &func.blocks {
            for inst in &block.instructions {
                if let Instruction::Copy { dest, src: Operand::Value(src_val) } = inst {
                    let d = dest.0;
                    let s = src_val.0;
                    // Exclude values that cannot safely share a slot:
                    // - multi-def values (defined in multiple blocks by phi elimination)
                    // - register-assigned values (no stack slot)
                    if multi_def_values.contains(&d) || multi_def_values.contains(&s) {
                        continue;
                    }
                    if reg_assigned.contains(&d) || reg_assigned.contains(&s) {
                        continue;
                    }
                    // Only coalesce if Copy is the sole use of the source.
                    // This guarantees source is dead after Copy, preventing
                    // the lost-copy problem in phi parallel copy groups.
                    if use_count.get(&s).copied().unwrap_or(0) != 1 {
                        continue;
                    }
                    raw_aliases.push((d, s));
                }
            }
        }
        // Build alias map with transitive resolution: follow chains to find root.
        // E.g., if A copies B and B copies C, both A and B alias to C.
        for (dest_id, src_id) in raw_aliases {
            // Find root of src.
            let mut root = src_id;
            let mut depth = 0;
            while let Some(&parent) = copy_alias.get(&root) {
                root = parent;
                depth += 1;
                if depth > 100 { break; } // safety limit
            }
            // Don't alias to self.
            if root != dest_id {
                copy_alias.insert(dest_id, root);
            }
        }
        // Remove aliases where the root is an alloca (alloca slots are special).
        // We must check this after chain resolution since the root might be an alloca
        // even if the direct src isn't.
        let alloca_ids: FxHashSet<u32> = func.blocks.iter()
            .flat_map(|b| b.instructions.iter())
            .filter_map(|inst| {
                if let Instruction::Alloca { dest, .. } = inst { Some(dest.0) } else { None }
            })
            .collect();
        copy_alias.retain(|dest_id, root_id| {
            !alloca_ids.contains(root_id) && !alloca_ids.contains(dest_id)
        });
    }

    let mut non_local_space = initial_offset;
    let mut deferred_slots: Vec<DeferredSlot> = Vec::new();
    let mut multi_block_values: Vec<MultiBlockValue> = Vec::new();

    // Per-block running space counter for block-local values.
    // Indexed by block index, holds the current accumulated space for that block.
    let mut block_space: FxHashMap<usize, i64> = FxHashMap::default();
    let mut max_block_local_space: i64 = 0;

    for (_block_idx, block) in func.blocks.iter().enumerate() {
        for inst in &block.instructions {
            if let Instruction::Alloca { dest, size, ty, align, .. } = inst {
                let effective_align = *align;
                let extra = if effective_align > 16 { effective_align - 1 } else { 0 };
                let raw_size = if *size == 0 { 8 } else { *size as i64 };

                state.alloca_values.insert(dest.0);
                state.alloca_types.insert(dest.0, *ty);
                if effective_align > 16 {
                    state.alloca_alignments.insert(dest.0, effective_align);
                }

                // Skip stack slot allocation for dead param allocas.
                // The alloca is still registered in alloca_values/alloca_types so
                // the backend recognizes it as an alloca, but no stack slot is
                // assigned. get_slot() will return None, causing emit_store_params
                // to skip storing the incoming register value for this parameter.
                if dead_param_allocas.contains(&dest.0) {
                    continue;
                }

                // Skip stack slot allocation for dead non-param allocas.
                // These are allocas from inlined functions whose variables are
                // never actually used. No code references them, so no slot needed.
                if coalescable_allocas.dead.contains(&dest.0) {
                    continue;
                }

                // Check if this alloca can be coalesced.
                // Over-aligned allocas (> 16) are excluded because their
                // alignment padding complicates coalescing.
                if effective_align <= 16 {
                    // Single-block allocas: use block-local coalescing (Tier 3).
                    if let Some(&use_block) = coalescable_allocas.single_block.get(&dest.0) {
                        let alloca_size = raw_size + extra as i64;
                        let alloca_align = *align as i64;
                        let bs = block_space.entry(use_block).or_insert(0);
                        let before = *bs;
                        let (_, new_space) = assign_slot(*bs, alloca_size, alloca_align);
                        *bs = new_space;
                        if new_space > max_block_local_space {
                            max_block_local_space = new_space;
                        }
                        deferred_slots.push(DeferredSlot {
                            dest_id: dest.0, size: alloca_size, align: alloca_align,
                            block_offset: before,
                        });
                        continue;
                    }
                    // Multi-block non-escaping allocas get permanent slots.
                    // Note: liveness-based interval packing using [min_block, max_block]
                    // ranges is unsound because block index order does not correspond to
                    // execution order in the presence of loops.
                }

                // Non-coalescable allocas get permanent slots.
                let (slot, new_space) = assign_slot(non_local_space, raw_size + extra as i64, *align as i64);
                state.value_locations.insert(dest.0, StackSlot(slot));
                non_local_space = new_space;
            } else if let Some(dest) = inst.dest() {
                let is_i128 = matches!(inst.result_type(), Some(IrType::I128) | Some(IrType::U128));
                let is_f128 = matches!(inst.result_type(), Some(IrType::F128));
                let slot_size: i64 = if is_i128 || is_f128 { 16 } else { 8 };

                if is_i128 {
                    state.i128_values.insert(dest.0);
                }

                // Skip stack slot allocation for values assigned to registers.
                // These values will live in callee-saved registers and never
                // need a stack slot, saving significant frame space.
                if reg_assigned.contains(&dest.0) {
                    continue;
                }

                // Skip stack slot allocation for dead values (defined but never
                // used as an operand). These are leftovers from DCE not catching
                // all dead code, or side-effect-free instructions whose results
                // are unused. Skipping their slots saves 8 bytes each, which
                // compounds significantly in recursive functions.
                //
                // This is safe because store_rax_to / store_t0_to already check
                // get_slot() and gracefully skip the store when no slot exists.
                // The instruction still executes (preserving side effects for
                // calls), but the result simply isn't persisted to the stack.
                if !used_values.contains(&dest.0) {
                    continue;
                }

                // Skip slot allocation for copy-aliased values.
                // These will get the same slot as their root after all slots are assigned.
                // Don't alias i128/f128 values (16-byte slots) to avoid size mismatches.
                if !is_i128 && !is_f128 && copy_alias.contains_key(&dest.0) {
                    continue;
                }

                if let Some(target_blk) = coalescable_group(dest.0) {
                    let bs = block_space.entry(target_blk).or_insert(0);
                    let before = *bs;
                    let (_, new_space) = assign_slot(*bs, slot_size, 0);
                    *bs = new_space;
                    if new_space > max_block_local_space {
                        max_block_local_space = new_space;
                    }
                    deferred_slots.push(DeferredSlot {
                        dest_id: dest.0, size: slot_size, align: 0,
                        block_offset: before,
                    });
                } else {
                    // Collect multi-block values for liveness-based packing.
                    multi_block_values.push(MultiBlockValue {
                        dest_id: dest.0,
                        slot_size,
                    });
                }
            }
        }
    }

    // Tier 2: Liveness-based stack slot packing for multi-block values.
    // Use live intervals to identify non-overlapping values that can share
    // the same stack slot, reducing frame size significantly.
    if !multi_block_values.is_empty() && enable_tier2 {
        // Reuse liveness data from register allocation when available, avoiding
        // a redundant O(blocks * values * iterations) dataflow computation.
        // For large functions (e.g., luaV_execute: 922 blocks, 99K instructions),
        // this saves ~0.5s of compile time per function.
        let liveness = cached_liveness.unwrap_or_else(|| compute_live_intervals(func));

        // Build map from value ID -> live interval.
        let mut interval_map: FxHashMap<u32, (u32, u32)> = FxHashMap::default();
        for iv in &liveness.intervals {
            interval_map.insert(iv.value_id, (iv.start, iv.end));
        }

        // Separate values by slot size for packing (8-byte and 16-byte pools).
        // Values of different sizes should not share slots to avoid alignment issues.
        let mut values_8: Vec<(u32, u32, u32)> = Vec::new(); // (dest_id, start, end)
        let mut values_16: Vec<(u32, u32, u32)> = Vec::new();
        let mut no_interval: Vec<(u32, i64)> = Vec::new(); // (dest_id, size) - fallback

        for mbv in &multi_block_values {
            if let Some(&(start, end)) = interval_map.get(&mbv.dest_id) {
                if mbv.slot_size == 16 {
                    values_16.push((mbv.dest_id, start, end));
                } else {
                    values_8.push((mbv.dest_id, start, end));
                }
            } else {
                // No interval info (e.g., dead value) - assign permanent slot.
                no_interval.push((mbv.dest_id, mbv.slot_size));
            }
        }

        // Pack values using a greedy interval coloring algorithm:
        // Sort by start point, then greedily assign to the first slot whose
        // previous occupant's interval has ended. This is optimal for interval
        // graphs (chromatic number equals clique number).
        fn pack_values_into_slots(
            values: &mut Vec<(u32, u32, u32)>,
            state: &mut super::state::CodegenState,
            non_local_space: &mut i64,
            slot_size: i64,
            assign_slot: &impl Fn(i64, i64, i64) -> (i64, i64),
        ) {
            if values.is_empty() {
                return;
            }

            // Sort by interval start point (ascending).
            values.sort_by_key(|&(_, start, _)| start);

            // Use a min-heap of (end_point, slot_index) to efficiently find the
            // slot whose current occupant ends earliest. If that slot's end < our
            // start, we can reuse it. This gives O(N log S) instead of the previous
            // O(N * S) linear scan, which is critical for large functions where
            // both N (values) and S (slots) can be in the tens of thousands.
            use std::collections::BinaryHeap;
            use std::cmp::Reverse;

            // Min-heap: (Reverse(end_point), slot_index)
            let mut heap: BinaryHeap<Reverse<(u32, usize)>> = BinaryHeap::new();
            // Slot data: (stack_slot_offset,) indexed by slot_index
            let mut slot_offsets: Vec<i64> = Vec::new();

            for &(dest_id, start, end) in values.iter() {
                // Check if the earliest-ending slot is free (end < start).
                if let Some(&Reverse((slot_end, slot_idx))) = heap.peek() {
                    if slot_end < start {
                        // Reuse this slot.
                        heap.pop();
                        let slot_offset = slot_offsets[slot_idx];
                        heap.push(Reverse((end, slot_idx)));
                        state.value_locations.insert(dest_id, StackSlot(slot_offset));
                        continue;
                    }
                }
                // No free slot — allocate a new one.
                let slot_idx = slot_offsets.len();
                let (slot, new_space) = assign_slot(*non_local_space, slot_size, 0);
                state.value_locations.insert(dest_id, StackSlot(slot));
                *non_local_space = new_space;
                slot_offsets.push(slot);
                heap.push(Reverse((end, slot_idx)));
            }
        }

        pack_values_into_slots(&mut values_8, state, &mut non_local_space, 8, &assign_slot);
        pack_values_into_slots(&mut values_16, state, &mut non_local_space, 16, &assign_slot);

        // Assign permanent slots for values without interval info.
        for (dest_id, size) in no_interval {
            let (slot, new_space) = assign_slot(non_local_space, size, 0);
            state.value_locations.insert(dest_id, StackSlot(slot));
            non_local_space = new_space;
        }
    } else {
        // Fallback: assign permanent slots for all multi-block values (no coalescing).
        for mbv in &multi_block_values {
            let (slot, new_space) = assign_slot(non_local_space, mbv.slot_size, 0);
            state.value_locations.insert(mbv.dest_id, StackSlot(slot));
            non_local_space = new_space;
        }
    }

    // Pack multi-block non-escaping allocas using liveness-based interval packing.
    // Assign final offsets for deferred block-local values.
    // All deferred values share a pool starting at non_local_space.
    // Each value's final slot is computed by calling assign_slot with
    // the global base plus the value's block-local offset.

    let total_space = if !deferred_slots.is_empty() && max_block_local_space > 0 {
        for ds in &deferred_slots {
            let (slot, _) = assign_slot(non_local_space + ds.block_offset, ds.size, ds.align);
            state.value_locations.insert(ds.dest_id, StackSlot(slot));
        }
        non_local_space + max_block_local_space
    } else {
        non_local_space
    };

    // ── Copy alias resolution ──────────────────────────────────────────
    // Propagate stack slots from root values to their copy aliases.
    // Each aliased value gets the same StackSlot as its root, eliminating
    // a separate slot allocation and making the Copy a harmless self-move.
    for (&dest_id, &root_id) in &copy_alias {
        if let Some(&slot) = state.value_locations.get(&root_id) {
            state.value_locations.insert(dest_id, slot);
        }
        // If root has no slot (e.g., it was optimized away or reg-assigned),
        // the aliased value also gets no slot. The Copy will still work via
        // the accumulator path since operand_to_rax handles missing slots.
    }

    total_space
}

/// Find the nth alloca instruction in the entry block (used for parameter storage).
pub fn find_param_alloca(func: &IrFunction, param_idx: usize) -> Option<(Value, IrType)> {
    func.blocks.first().and_then(|block| {
        block.instructions.iter()
            .filter(|i| matches!(i, Instruction::Alloca { .. }))
            .nth(param_idx)
            .and_then(|inst| {
                if let Instruction::Alloca { dest, ty, .. } = inst {
                    Some((*dest, *ty))
                } else {
                    None
                }
            })
    })
}
