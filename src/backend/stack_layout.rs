//! Stack layout: slot assignment, alloca coalescing, and regalloc helpers.
//!
//! ## Architecture
//!
//! Stack space calculation uses a three-tier allocation scheme:
//!
//! - **Tier 1**: Allocas get permanent, non-shared slots (addressable memory).
//!   Exception: non-escaping single-block allocas use Tier 3 sharing.
//!
//! - **Tier 2**: Multi-block non-alloca SSA temporaries use liveness-based packing.
//!   Values with non-overlapping live intervals share the same stack slot,
//!   using a greedy interval coloring algorithm via a min-heap.
//!
//! - **Tier 3**: Single-block non-alloca values use block-local coalescing with
//!   intra-block greedy slot reuse. Each block has its own pool; pools from
//!   different blocks overlap since only one block executes at a time.
//!
//! ## Key functions
//!
//! - `calculate_stack_space_common`: orchestrates the three-tier allocation
//! - `compute_coalescable_allocas`: escape analysis for alloca coalescing
//! - `collect_inline_asm_callee_saved`: shared ASM clobber scan
//! - `run_regalloc_and_merge_clobbers`: register allocator + clobber merge
//! - `filter_available_regs`: callee-saved register filtering
//! - `find_param_alloca`: parameter alloca lookup

use crate::ir::ir::{
    Instruction,
    IrConst,
    IrFunction,
    Operand,
    Terminator,
    Value,
};
use crate::common::types::IrType;
use crate::common::fx_hash::{FxHashMap, FxHashSet};
use super::state::StackSlot;
use super::liveness::{
    for_each_operand_in_instruction, for_each_value_use_in_instruction,
    for_each_operand_in_terminator, compute_live_intervals,
};

// ── Helper structs for slot allocation ────────────────────────────────────

/// A block-local slot whose final offset is deferred until after all tiers
/// have computed their space requirements. The final offset is:
/// `non_local_space + block_offset`.
struct DeferredSlot {
    dest_id: u32,
    size: i64,
    align: i64,
    block_offset: i64,
}

/// Multi-block value pending Tier 2 liveness-based packing.
struct MultiBlockValue {
    dest_id: u32,
    slot_size: i64,
}

/// Block-local non-alloca value pending Tier 3 intra-block reuse.
struct BlockLocalValue {
    dest_id: u32,
    slot_size: i64,
    block_idx: usize,
}

/// Intermediate state passed between phases of `calculate_stack_space_common`.
struct StackLayoutContext {
    /// Whether coalescing and multi-tier allocation is enabled (num_blocks >= 2).
    coalesce: bool,
    /// Per-value use-block map: value_id -> list of block indices where used.
    use_blocks_map: FxHashMap<u32, Vec<usize>>,
    /// Value ID -> defining block index.
    def_block: FxHashMap<u32, usize>,
    /// Values defined in multiple blocks (from phi elimination).
    multi_def_values: FxHashSet<u32>,
    /// Copy alias map: dest_id -> root_id (values sharing the same stack slot).
    copy_alias: FxHashMap<u32, u32>,
    /// All value IDs referenced as operands in the function body.
    used_values: FxHashSet<u32>,
    /// Dead parameter allocas (unused params, skip slot allocation).
    dead_param_allocas: FxHashSet<u32>,
    /// Alloca coalescing analysis results.
    coalescable_allocas: CoalescableAllocas,
    /// Values that are produced and immediately consumed by the next instruction,
    /// as the first operand loaded into the accumulator. These values don't need
    /// stack slots — the accumulator register cache keeps them alive.
    immediately_consumed: FxHashSet<u32>,
}

// ── Value use-block map ───────────────────────────────────────────────────

/// Compute the "use-block map" for all values in the function.
/// For each value, records the set of block indices where that value is referenced.
///
/// Phi node operands are attributed to their SOURCE block (not the Phi's block).
/// This is critical for coalescing in dispatch-style code (switch/computed goto):
/// a value defined in block A that flows through a Phi to block B is semantically
/// "used at the end of block A" (where the branch resolves the Phi). Attributing
/// Phi uses to the source block allows such values to be considered block-local
/// to A, enabling stack slot coalescing across case handlers.
fn compute_value_use_blocks(func: &IrFunction) -> FxHashMap<u32, Vec<usize>> {
    let mut uses: FxHashMap<u32, Vec<usize>> = FxHashMap::default();

    let record_use = |id: u32, block_idx: usize, uses: &mut FxHashMap<u32, Vec<usize>>| {
        let blocks = uses.entry(id).or_default();
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

// ── Alloca coalescability ─────────────────────────────────────────────────

/// Result of alloca coalescability analysis.
struct CoalescableAllocas {
    /// Single-block allocas: alloca ID -> the single block index where it's used.
    /// These can use block-local coalescing (Tier 3).
    single_block: FxHashMap<u32, usize>,
    /// Dead non-param allocas: alloca IDs with no uses at all.
    /// These can be skipped entirely (no stack slot needed).
    dead: FxHashSet<u32>,
}

/// Escape analysis state for alloca coalescability computation.
struct AllocaEscapeAnalysis {
    alloca_set: FxHashSet<u32>,
    gep_to_alloca: FxHashMap<u32, u32>,
    escaped: FxHashSet<u32>,
    use_blocks: FxHashMap<u32, Vec<usize>>,
}

impl AllocaEscapeAnalysis {
    fn new(func: &IrFunction, dead_param_allocas: &FxHashSet<u32>, param_alloca_values: &[Value]) -> Self {
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
        Self {
            alloca_set,
            gep_to_alloca: FxHashMap::default(),
            escaped: FxHashSet::default(),
            use_blocks: FxHashMap::default(),
        }
    }

    /// Resolve a value to its root alloca, if any.
    fn resolve_root(&self, val_id: u32) -> Option<u32> {
        if self.alloca_set.contains(&val_id) {
            Some(val_id)
        } else {
            self.gep_to_alloca.get(&val_id).copied()
        }
    }

    /// Record a use of an alloca (or GEP-derived value) in a block.
    fn record_use(&mut self, val_id: u32, block_idx: usize) {
        if let Some(root) = self.resolve_root(val_id) {
            let blocks = self.use_blocks.entry(root).or_default();
            if blocks.last() != Some(&block_idx) {
                blocks.push(block_idx);
            }
        }
    }

    /// Mark an alloca (or GEP-derived value) as escaped and record the use.
    fn mark_escaped(&mut self, val_id: u32, block_idx: usize) {
        if let Some(root) = self.resolve_root(val_id) {
            self.escaped.insert(root);
            let blocks = self.use_blocks.entry(root).or_default();
            if blocks.last() != Some(&block_idx) { blocks.push(block_idx); }
        }
    }

    /// Mark an operand's value as escaped (no use-block recording).
    fn mark_operand_escaped(&mut self, op: &Operand) {
        if let Operand::Value(v) = op {
            if let Some(root) = self.resolve_root(v.0) {
                self.escaped.insert(root);
            }
        }
    }

    /// Mark a direct value as escaped (for va_list ptrs etc).
    fn mark_direct_escaped(&mut self, val_id: u32) {
        if self.alloca_set.contains(&val_id) {
            self.escaped.insert(val_id);
        }
    }

    /// Scan all instructions and terminators for escape conditions and use sites.
    fn scan_instructions(&mut self, func: &IrFunction) {
        for (block_idx, block) in func.blocks.iter().enumerate() {
            for inst in &block.instructions {
                self.scan_gep(inst, block_idx);
                self.scan_instruction(inst, block_idx);
            }
            self.scan_terminator(&block.terminator);
        }
    }

    fn scan_gep(&mut self, inst: &Instruction, block_idx: usize) {
        if let Instruction::GetElementPtr { dest, base, .. } = inst {
            if let Some(root_alloca) = self.resolve_root(base.0) {
                self.gep_to_alloca.insert(dest.0, root_alloca);
                self.record_use(dest.0, block_idx);
            }
        }
    }

    fn scan_instruction(&mut self, inst: &Instruction, block_idx: usize) {
        match inst {
            Instruction::Call { info, .. } | Instruction::CallIndirect { info, .. } => {
                for arg in &info.args {
                    if let Operand::Value(v) = arg {
                        self.mark_escaped(v.0, block_idx);
                    }
                }
            }
            Instruction::Store { val, ptr, .. } => {
                self.mark_operand_escaped(val);
                self.record_use(ptr.0, block_idx);
            }
            Instruction::Load { ptr, .. } => {
                self.record_use(ptr.0, block_idx);
            }
            Instruction::Memcpy { dest, src, .. } => {
                for v in [dest, src] { self.record_use(v.0, block_idx); }
            }
            Instruction::InlineAsm { outputs, inputs, .. } => {
                for (_, v, _) in outputs { self.mark_escaped(v.0, block_idx); }
                for (_, op, _) in inputs {
                    if let Operand::Value(v) = op { self.mark_escaped(v.0, block_idx); }
                }
            }
            Instruction::Intrinsic { dest_ptr, args, .. } => {
                if let Some(dp) = dest_ptr { self.record_use(dp.0, block_idx); }
                for arg in args {
                    if let Operand::Value(v) = arg { self.record_use(v.0, block_idx); }
                }
            }
            Instruction::AtomicRmw { ptr, val, .. } => {
                self.mark_operand_escaped(val);
                if let Operand::Value(v) = ptr { self.record_use(v.0, block_idx); }
            }
            Instruction::AtomicCmpxchg { ptr, expected, desired, .. } => {
                for op in [expected, desired] { self.mark_operand_escaped(op); }
                if let Operand::Value(v) = ptr { self.record_use(v.0, block_idx); }
            }
            Instruction::AtomicLoad { ptr: Operand::Value(v), .. } => {
                self.record_use(v.0, block_idx);
            }
            Instruction::AtomicStore { ptr, val, .. } => {
                self.mark_operand_escaped(val);
                if let Operand::Value(v) = ptr { self.record_use(v.0, block_idx); }
            }
            Instruction::VaStart { va_list_ptr } | Instruction::VaEnd { va_list_ptr } | Instruction::VaArg { va_list_ptr, .. } => {
                self.mark_direct_escaped(va_list_ptr.0);
            }
            Instruction::VaCopy { dest_ptr, src_ptr } => {
                self.mark_direct_escaped(dest_ptr.0);
                self.mark_direct_escaped(src_ptr.0);
            }
            Instruction::VaArgStruct { dest_ptr, va_list_ptr, .. } => {
                self.mark_direct_escaped(dest_ptr.0);
                self.mark_direct_escaped(va_list_ptr.0);
            }
            Instruction::Cast { src, .. } | Instruction::Copy { src, .. } | Instruction::UnaryOp { src, .. } => {
                self.mark_operand_escaped(src);
            }
            Instruction::BinOp { lhs, rhs, .. } | Instruction::Cmp { lhs, rhs, .. } => {
                for op in [lhs, rhs] { self.mark_operand_escaped(op); }
            }
            Instruction::Select { cond, true_val, false_val, .. } => {
                for op in [cond, true_val, false_val] { self.mark_operand_escaped(op); }
            }
            Instruction::Phi { incoming, .. } => {
                for (op, _) in incoming { self.mark_operand_escaped(op); }
            }
            _ => {}
        }
    }

    fn scan_terminator(&mut self, terminator: &Terminator) {
        for_each_operand_in_terminator(terminator, |op| {
            if let Operand::Value(v) = op {
                if let Some(root) = self.resolve_root(v.0) {
                    self.escaped.insert(root);
                }
            }
        });
    }

    /// Classify non-escaped allocas into single-block and dead.
    fn into_result(self) -> CoalescableAllocas {
        let mut single_block: FxHashMap<u32, usize> = FxHashMap::default();
        let mut dead: FxHashSet<u32> = FxHashSet::default();
        for &alloca_id in &self.alloca_set {
            if self.escaped.contains(&alloca_id) {
                continue;
            }
            if let Some(blocks) = self.use_blocks.get(&alloca_id) {
                let mut unique: Vec<usize> = blocks.clone();
                unique.sort_unstable();
                unique.dedup();
                if unique.len() == 1 {
                    single_block.insert(alloca_id, unique[0]);
                }
                // Multi-block allocas: not coalesced, get permanent slots.
            } else {
                dead.insert(alloca_id);
            }
        }
        CoalescableAllocas { single_block, dead }
    }
}

/// Compute which allocas can be coalesced (either block-locally or via liveness packing).
///
/// An alloca is coalescable if its address never "escapes" (stored as a value,
/// used in casts/binops/phi/select, passed to calls, or used in terminators).
/// GEP chains are tracked transitively.
fn compute_coalescable_allocas(
    func: &IrFunction,
    dead_param_allocas: &FxHashSet<u32>,
    param_alloca_values: &[Value],
) -> CoalescableAllocas {
    let mut analysis = AllocaEscapeAnalysis::new(func, dead_param_allocas, param_alloca_values);
    if analysis.alloca_set.is_empty() {
        return CoalescableAllocas { single_block: FxHashMap::default(), dead: FxHashSet::default() };
    }
    analysis.scan_instructions(func);
    analysis.into_result()
}

// ── Inline asm callee-saved scanning ──────────────────────────────────────

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
                    let c = constraint.trim_start_matches(['=', '+', '&']);
                    if let Some(phys) = constraint_to_phys(c) {
                        if already.insert(phys.0) { used.push(phys); }
                    }
                }
                for (constraint, _, _) in inputs {
                    let c = constraint.trim_start_matches(['=', '+', '&']);
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

// ── Register allocation helpers ───────────────────────────────────────────

/// Run register allocation and merge ASM-clobbered callee-saved registers.
///
/// This shared helper eliminates duplicated regalloc setup boilerplate across
/// all four backends (x86-64, i686, AArch64, RISC-V 64).  Each backend supplies its callee-saved
/// register list and pre-collected ASM clobber list; this function handles the
/// common steps: filtering available registers, running the allocator, storing
/// results, merging clobbers into `used_callee_saved`, and building the
/// `reg_assigned` set.
///
/// Returns `(reg_assigned, cached_liveness)` for use by `calculate_stack_space_common`.
pub fn run_regalloc_and_merge_clobbers(
    func: &IrFunction,
    available_regs: Vec<super::regalloc::PhysReg>,
    caller_saved_regs: Vec<super::regalloc::PhysReg>,
    asm_clobbered_regs: &[super::regalloc::PhysReg],
    reg_assignments: &mut FxHashMap<u32, super::regalloc::PhysReg>,
    used_callee_saved: &mut Vec<super::regalloc::PhysReg>,
    allow_inline_asm_regalloc: bool,
) -> (FxHashSet<u32>, Option<super::liveness::LivenessResult>) {
    let config = super::regalloc::RegAllocConfig { available_regs, caller_saved_regs, allow_inline_asm_regalloc };
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

// ── Main stack space calculation ──────────────────────────────────────────

/// Shared stack space calculation: iterates over all instructions, assigns stack
/// slots for allocas and value results. Arch-specific offset direction is handled
/// by the `assign_slot` closure.
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

    // Enable coalescing and multi-tier allocation for any multi-block function.
    // Even small functions benefit: a 3-block function with 20 intermediates can
    // save 100+ bytes. Critical for recursive functions (PostgreSQL plpgsql) and
    // kernel functions with macro-expanded short-lived intermediates.
    let coalesce = num_blocks >= 2;

    // Phase 1: Build analysis context (use-blocks, def-blocks, used values,
    //          dead param allocas, alloca coalescability, copy aliases).
    let ctx = build_layout_context(func, coalesce, reg_assigned);

    // Tell CodegenState which values are register-assigned so that
    // resolve_slot_addr can return a dummy Indirect slot for them.
    state.reg_assigned_values = reg_assigned.clone();

    // Phase 2: Classify all instructions into the three tiers.
    let mut non_local_space = initial_offset;
    let mut deferred_slots: Vec<DeferredSlot> = Vec::new();
    let mut multi_block_values: Vec<MultiBlockValue> = Vec::new();
    let mut block_local_values: Vec<BlockLocalValue> = Vec::new();
    let mut block_space: FxHashMap<usize, i64> = FxHashMap::default();
    let mut max_block_local_space: i64 = 0;

    classify_instructions(
        state, func, &ctx, &assign_slot, reg_assigned,
        &mut non_local_space, &mut deferred_slots, &mut multi_block_values,
        &mut block_local_values, &mut block_space, &mut max_block_local_space,
    );

    // Phase 3: Tier 3 — block-local greedy slot reuse.
    assign_tier3_block_local_slots(
        func, &ctx, coalesce,
        &block_local_values, &mut deferred_slots,
        &mut block_space, &mut max_block_local_space, &assign_slot,
    );

    // Phase 4: Tier 2 — liveness-based packing for multi-block values.
    assign_tier2_liveness_packed_slots(
        state, coalesce, cached_liveness, func,
        &multi_block_values, &mut non_local_space, &assign_slot,
    );

    // Phase 5: Finalize deferred block-local slots by adding the global base offset.
    let total_space = finalize_deferred_slots(
        state, &deferred_slots, non_local_space, max_block_local_space, &assign_slot,
    );

    // Phase 6: Resolve copy aliases (propagate slots from root to aliased values).
    resolve_copy_aliases(state, &ctx.copy_alias);

    // Phase 7: Propagate wide-value status through Copy chains (32-bit targets only).
    propagate_wide_values(state, func, &ctx.copy_alias);

    total_space
}

// ── Phase 1: Build analysis context ───────────────────────────────────────

/// Build all the analysis data needed by the three-tier slot allocator.
/// This includes use-block maps, definition tracking, copy coalescing analysis,
/// dead param detection, and alloca coalescability.
fn build_layout_context(
    func: &IrFunction,
    coalesce: bool,
    reg_assigned: &FxHashSet<u32>,
) -> StackLayoutContext {
    // Build use-block map
    let mut use_blocks_map = if coalesce {
        compute_value_use_blocks(func)
    } else {
        FxHashMap::default()
    };

    // Build def-block map and identify multi-definition values (phi elimination).
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

    // Collect all Value IDs referenced as operands (for dead value/param detection).
    let used_values = collect_used_values(func);

    // Detect dead parameter allocas.
    let dead_param_allocas = find_dead_param_allocas(func, &used_values);

    // Alloca coalescability analysis.
    let coalescable_allocas = if coalesce {
        compute_coalescable_allocas(func, &dead_param_allocas, &func.param_alloca_values)
    } else {
        CoalescableAllocas { single_block: FxHashMap::default(), dead: FxHashSet::default() }
    };

    // Copy coalescing analysis.
    let copy_alias = build_copy_alias_map(
        func, &def_block, &multi_def_values, reg_assigned, &use_blocks_map,
    );

    // Immediately-consumed value analysis: identify values that can skip stack slots.
    let immediately_consumed = compute_immediately_consumed(func);

    // Propagate copy-alias uses into use_blocks_map so that root values account
    // for their aliases' use sites when deciding block-local vs. multi-block.
    if coalesce && !copy_alias.is_empty() {
        for (&dest_id, &root_id) in &copy_alias {
            if let Some(dest_blocks) = use_blocks_map.get(&dest_id).cloned() {
                let root_blocks = use_blocks_map.entry(root_id).or_insert_with(Vec::new);
                for blk in dest_blocks {
                    if root_blocks.last() != Some(&blk) {
                        root_blocks.push(blk);
                    }
                }
            }
        }
    }

    StackLayoutContext {
        coalesce,
        use_blocks_map,
        def_block,
        multi_def_values,
        copy_alias,
        used_values,
        dead_param_allocas,
        coalescable_allocas,
        immediately_consumed,
    }
}

/// Collect all Value IDs referenced as operands anywhere in the function body.
fn collect_used_values(func: &IrFunction) -> FxHashSet<u32> {
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
}

/// Find param allocas whose Value IDs are never used in any instruction or terminator.
/// These represent completely unused function parameters.
///
/// Exception: if a ParamRef instruction exists for a given param index, the
/// corresponding param alloca must NOT be marked dead. emit_store_params needs
/// to save the incoming register to the alloca slot because emit_param_ref
/// may read from ABI registers that get clobbered during emit_store_params.
fn find_dead_param_allocas(func: &IrFunction, used_values: &FxHashSet<u32>) -> FxHashSet<u32> {
    let mut dead = FxHashSet::default();
    if !func.param_alloca_values.is_empty() {
        let mut paramref_indices: FxHashSet<usize> = FxHashSet::default();
        for block in &func.blocks {
            for inst in &block.instructions {
                if let Instruction::ParamRef { param_idx, .. } = inst {
                    paramref_indices.insert(*param_idx);
                }
            }
        }
        for (idx, pv) in func.param_alloca_values.iter().enumerate() {
            if !used_values.contains(&pv.0) && !paramref_indices.contains(&idx) {
                dead.insert(pv.0);
            }
        }
    }
    dead
}

// ── Copy coalescing ───────────────────────────────────────────────────────

/// Build the copy alias map: dest_id -> root_id for Copy instructions where
/// dest and src can share the same stack slot.
///
/// Safety: only coalesces when the Copy is the SOLE use of the source value,
/// guaranteeing the source is dead after the Copy (avoids the "lost copy"
/// problem in phi parallel copy groups).
fn build_copy_alias_map(
    func: &IrFunction,
    def_block: &FxHashMap<u32, usize>,
    multi_def_values: &FxHashSet<u32>,
    reg_assigned: &FxHashSet<u32>,
    use_blocks_map: &FxHashMap<u32, Vec<usize>>,
) -> FxHashMap<u32, u32> {
    // Count uses of each value across all instructions.
    let mut use_count: FxHashMap<u32, u32> = FxHashMap::default();
    for block in &func.blocks {
        for inst in &block.instructions {
            for_each_operand_in_instruction(inst, |op| {
                if let Operand::Value(v) = op {
                    *use_count.entry(v.0).or_insert(0) += 1;
                }
            });
            for_each_value_use_in_instruction(inst, |v| {
                *use_count.entry(v.0).or_insert(0) += 1;
            });
        }
        for_each_operand_in_terminator(&block.terminator, |op| {
            if let Operand::Value(v) = op {
                *use_count.entry(v.0).or_insert(0) += 1;
            }
        });
    }

    // Collect Copy instructions eligible for aliasing.
    let mut raw_aliases: Vec<(u32, u32)> = Vec::new();
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::Copy { dest, src: Operand::Value(src_val) } = inst {
                let d = dest.0;
                let s = src_val.0;
                // Exclude multi-def values and register-assigned values.
                if multi_def_values.contains(&d) || multi_def_values.contains(&s) {
                    continue;
                }
                if reg_assigned.contains(&d) || reg_assigned.contains(&s) {
                    continue;
                }
                // Only coalesce if Copy is the sole use of the source.
                if use_count.get(&s).copied().unwrap_or(0) != 1 {
                    continue;
                }
                // Only coalesce if dest's uses are in the same block as source's definition.
                // Cross-block aliasing is unsafe: the root's liveness interval doesn't
                // account for the alias's uses in other blocks.
                if let Some(&src_def_blk) = def_block.get(&s) {
                    if let Some(dest_use_blocks) = use_blocks_map.get(&d) {
                        if dest_use_blocks.iter().any(|&b| b != src_def_blk) {
                            continue;
                        }
                    }
                }
                raw_aliases.push((d, s));
            }
        }
    }

    // Build alias map with transitive resolution: follow chains to find root.
    // Safety limit on chain depth guards against pathological cycles.
    const MAX_ALIAS_CHAIN_DEPTH: usize = 100;
    let mut copy_alias: FxHashMap<u32, u32> = FxHashMap::default();
    for (dest_id, src_id) in raw_aliases {
        let mut root = src_id;
        let mut depth = 0;
        while let Some(&parent) = copy_alias.get(&root) {
            root = parent;
            depth += 1;
            if depth > MAX_ALIAS_CHAIN_DEPTH { break; }
        }
        if root != dest_id {
            copy_alias.insert(dest_id, root);
        }
    }

    // Remove aliases where root or dest is an alloca (alloca slots are special).
    let alloca_ids: FxHashSet<u32> = func.blocks.iter()
        .flat_map(|b| b.instructions.iter())
        .filter_map(|inst| {
            if let Instruction::Alloca { dest, .. } = inst { Some(dest.0) } else { None }
        })
        .collect();
    copy_alias.retain(|dest_id, root_id| {
        !alloca_ids.contains(root_id) && !alloca_ids.contains(dest_id)
    });

    // Remove aliases for InlineAsm output pointer values. InlineAsm Phase 4 reads
    // output pointers from stack slots AFTER the asm executes; if aliased, the
    // root's slot may be reused between the Copy and the InlineAsm, corrupting
    // the pointer read in Phase 4.
    let mut asm_output_ptrs: FxHashSet<u32> = FxHashSet::default();
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::InlineAsm { outputs, .. } = inst {
                for (_, v, _) in outputs {
                    asm_output_ptrs.insert(v.0);
                }
            }
        }
    }
    if !asm_output_ptrs.is_empty() {
        copy_alias.retain(|dest_id, _| !asm_output_ptrs.contains(dest_id));
    }

    copy_alias
}

// ── Immediately-consumed value analysis ───────────────────────────────────

/// Identify values that can skip stack slot allocation because they are
/// produced and consumed in adjacent instructions within the same block.
///
/// A value V defined at instruction I can skip its slot if:
/// 1. V has exactly one use as an Operand (loaded via operand_to_rax/rcx)
/// 2. That use is at instruction I+1 (or in the block terminator if I is last)
/// 3. V is the FIRST Operand of the consumer (loaded first into the accumulator)
/// 4. V is NOT used as a Value reference (ptr in Store/Load, base in GEP, etc.)
/// 5. V is not i128/f128 (these need 16-byte slots with special handling)
/// 6. V is not from a Copy instruction (copy aliasing needs the root's slot)
/// 7. V is not from an Alloca (allocas always need addressable slots)
///
/// The codegen accumulator cache ensures correctness: store_rax_to sets the
/// cache, and the next instruction's operand_to_rax finds V there.
fn compute_immediately_consumed(func: &IrFunction) -> FxHashSet<u32> {
    let mut result = FxHashSet::default();

    // First pass: count uses per value (both Operand and Value-ref uses).
    let mut operand_use_count: FxHashMap<u32, u32> = FxHashMap::default();
    let mut has_value_ref_use: FxHashSet<u32> = FxHashSet::default();

    for block in &func.blocks {
        for inst in &block.instructions {
            for_each_operand_in_instruction(inst, |op| {
                if let Operand::Value(v) = op {
                    *operand_use_count.entry(v.0).or_insert(0) += 1;
                }
            });
            for_each_value_use_in_instruction(inst, |v| {
                has_value_ref_use.insert(v.0);
            });
        }
        for_each_operand_in_terminator(&block.terminator, |op| {
            if let Operand::Value(v) = op {
                *operand_use_count.entry(v.0).or_insert(0) += 1;
            }
        });
    }

    // Collect all copy-alias roots: values that serve as the slot source for copies.
    // These must keep their slots since aliased copies will use them.
    let mut copy_alias_roots: FxHashSet<u32> = FxHashSet::default();
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::Copy { src: Operand::Value(v), .. } = inst {
                copy_alias_roots.insert(v.0);
            }
        }
    }

    // Second pass: check adjacency and first-operand conditions.
    for block in &func.blocks {
        let insts = &block.instructions;
        for (i, inst) in insts.iter().enumerate() {
            let dest = match inst.dest() {
                Some(d) => d,
                None => continue,
            };

            // Only acc-preserving producers are safe: after these execute,
            // the accumulator cache still holds the result. Cache-invalidating
            // instructions (Call, Atomic*, DynAlloca, etc.) clear the cache
            // after store_rax_to, so the next instruction can't find the value.
            if !is_acc_preserving_producer(inst) { continue; }
            // Skip i128/f128 (special 16-byte handling uses emit_load_acc_pair /
            // emit_store_acc_pair which bypass the normal accumulator cache).
            if involves_i128_or_f128(inst) { continue; }
            // Skip if value has Value-ref uses (ptr/base in Store/Load/GEP).
            if has_value_ref_use.contains(&dest.0) { continue; }
            // Skip if value is a copy-alias root (other values share its slot).
            if copy_alias_roots.contains(&dest.0) { continue; }
            // Must have exactly one Operand use.
            if operand_use_count.get(&dest.0).copied().unwrap_or(0) != 1 { continue; }

            // Check if the single use is in the immediately next instruction
            // or in the block terminator (if this is the last instruction).
            if i + 1 < insts.len() {
                // Use must be in instruction i+1, as the first Operand.
                let next = &insts[i + 1];
                if is_safe_sole_consumer(next, dest.0) {
                    result.insert(dest.0);
                }
            } else {
                // Last instruction: use must be in the terminator, as the sole Operand.
                if is_sole_operand_of_terminator(&block.terminator, dest.0) {
                    result.insert(dest.0);
                }
            }
        }
    }

    result
}

/// Check if an instruction is an "acc-preserving" producer: after execution,
/// the accumulator register cache still holds the result value. Only these
/// instructions can participate in the skip-slot optimization as producers.
///
/// Cache-invalidating instructions (Call, Store, Atomic*, DynAlloca, InlineAsm,
/// etc.) call invalidate_all() after execution, clearing the cache.
fn is_acc_preserving_producer(inst: &Instruction) -> bool {
    matches!(inst,
        Instruction::Load { .. }
        | Instruction::BinOp { .. }
        | Instruction::UnaryOp { .. }
        | Instruction::Cmp { .. }
        | Instruction::Cast { .. }
        | Instruction::GetElementPtr { .. }
        | Instruction::GlobalAddr { .. }
        | Instruction::Select { .. }
        | Instruction::LabelAddr { .. }
    )
}

/// Check if an instruction involves I128/U128/F128 types in any operand position.
/// These use emit_load_acc_pair / emit_store_acc_pair which bypass the normal
/// accumulator cache, so they cannot participate in skip-slot optimization.
fn involves_i128_or_f128(inst: &Instruction) -> bool {
    fn is_wide(ty: IrType) -> bool {
        matches!(ty, IrType::I128 | IrType::U128 | IrType::F128)
    }
    match inst {
        Instruction::Cast { from_ty, to_ty, .. } => is_wide(*from_ty) || is_wide(*to_ty),
        Instruction::UnaryOp { ty, .. } => is_wide(*ty),
        Instruction::BinOp { ty, .. } => is_wide(*ty),
        Instruction::Cmp { ty, .. } => is_wide(*ty),
        Instruction::Load { ty, .. } => is_wide(*ty),
        _ => {
            // For other instructions, just check the result type.
            matches!(inst.result_type(), Some(ty) if is_wide(ty))
        }
    }
}

/// Check if value_id is the sole Operand loaded by the given instruction,
/// with guaranteed loading order (no other operand loaded before it).
///
/// Only single-operand consumers are safe: Store (val loaded first), Cast,
/// UnaryOp, Copy. Two-operand instructions (BinOp, Cmp) are excluded because
/// codegen may load the OTHER operand first (e.g. BinOp's rhs_conflicts path,
/// float Cmp's Lt/Le operand swap). GEP excluded because OverAligned base
/// computation clobbers %rax before offset is loaded.
fn is_safe_sole_consumer(inst: &Instruction, value_id: u32) -> bool {
    match inst {
        // Store: val is always loaded first via emit_load_operand (operand_to_rax)
        Instruction::Store { val: Operand::Value(v), .. } => v.0 == value_id,
        // Single-operand instructions: loaded via operand_to_rax, no other operand
        Instruction::Cast { src: Operand::Value(v), .. } => v.0 == value_id,
        Instruction::UnaryOp { src: Operand::Value(v), .. } => v.0 == value_id,
        Instruction::Copy { src: Operand::Value(v), .. } => v.0 == value_id,
        // All other instructions: not safe (BinOp, Cmp, GEP, Call, Select, etc.)
        _ => false,
    }
}

/// Check if value_id is the sole operand of a block terminator.
fn is_sole_operand_of_terminator(term: &Terminator, value_id: u32) -> bool {
    match term {
        Terminator::Return(Some(Operand::Value(v))) => v.0 == value_id,
        Terminator::CondBranch { cond: Operand::Value(v), .. } => v.0 == value_id,
        Terminator::Switch { val: Operand::Value(v), .. } => v.0 == value_id,
        Terminator::IndirectBranch { target: Operand::Value(v), .. } => v.0 == value_id,
        _ => false,
    }
}

// ── Phase 2: Classify instructions into tiers ─────────────────────────────

/// Determine if a non-alloca value can be assigned to a block-local pool slot (Tier 3).
/// Returns `Some(def_block_idx)` if the value is defined and used only within a
/// single block, making it safe to share stack space with values from other blocks.
fn coalescable_group(
    val_id: u32,
    ctx: &StackLayoutContext,
) -> Option<usize> {
    if !ctx.coalesce {
        return None;
    }
    // Values defined in multiple blocks (from phi elimination) must use Tier 2.
    if ctx.multi_def_values.contains(&val_id) {
        return None;
    }
    if let Some(&def_blk) = ctx.def_block.get(&val_id) {
        if let Some(blocks) = ctx.use_blocks_map.get(&val_id) {
            let mut unique: Vec<usize> = blocks.clone();
            unique.sort_unstable();
            unique.dedup();

            if unique.is_empty() {
                return Some(def_blk); // Dead value, safe to coalesce.
            }

            // Single-block value: defined and used in the same block.
            if unique.len() == 1 && unique[0] == def_blk {
                return Some(def_blk);
            }
        } else {
            return Some(def_blk); // No uses - dead value.
        }
    }
    None
}

/// Walk all instructions and classify each into Tier 1 (permanent alloca slots),
/// Tier 2 (multi-block, liveness-packed), or Tier 3 (block-local, greedy reuse).
#[allow(clippy::too_many_arguments)]
fn classify_instructions(
    state: &mut super::state::CodegenState,
    func: &IrFunction,
    ctx: &StackLayoutContext,
    assign_slot: &impl Fn(i64, i64, i64) -> (i64, i64),
    reg_assigned: &FxHashSet<u32>,
    non_local_space: &mut i64,
    deferred_slots: &mut Vec<DeferredSlot>,
    multi_block_values: &mut Vec<MultiBlockValue>,
    block_local_values: &mut Vec<BlockLocalValue>,
    block_space: &mut FxHashMap<usize, i64>,
    max_block_local_space: &mut i64,
) {
    let mut collected_values: FxHashSet<u32> = FxHashSet::default();

    // Build set of values that are defined (as dest) by non-InlineAsm
    // instructions. This identifies "indirect" asm output pointers:
    //
    // When an InlineAsm output is "=r"(*ptr), the output value is the
    // loaded pointer (from a Load that became a Copy after mem2reg).
    // This value is defined by a Copy/Load/Phi instruction, AND it
    // appears as an InlineAsm output. The asm result must be stored
    // THROUGH the pointer, not directly into a stack slot.
    //
    // When an InlineAsm output is "=r"(x) and x was promoted by mem2reg,
    // the output value is a fresh SSA value created by mem2reg. This
    // value is NOT defined by any other instruction -- it only appears
    // in the InlineAsm outputs. This value DOES need a direct stack slot.
    //
    // The distinction: if an InlineAsm output value is also the dest of
    // a non-InlineAsm instruction, it's an indirect pointer that should
    // NOT be promoted to a direct asm output slot.
    let mut non_asm_defined: FxHashSet<u32> = FxHashSet::default();
    for block in &func.blocks {
        for inst in &block.instructions {
            if !matches!(inst, Instruction::InlineAsm { .. }) {
                if let Some(dest) = inst.dest() {
                    non_asm_defined.insert(dest.0);
                }
            }
        }
    }

    // Pre-scan: find param allocas whose memory is modified after the initial
    // emit_store_params Store. The ParamRef optimization (reusing the alloca
    // slot for the initial parameter value) is only safe when the alloca's
    // content is never overwritten. If any additional store targets the param
    // alloca (directly, through a GEP, or via an escaped pointer to a callee),
    // the alloca may hold a different value than the ParamRef expects.
    let modified_param_allocas: FxHashSet<u32> = {
        let param_alloca_set: FxHashSet<u32> =
            func.param_alloca_values.iter().map(|v| v.0).collect();

        // Map GEP dest -> param alloca root (for chained GEPs)
        let mut gep_to_param: FxHashMap<u32, u32> = FxHashMap::default();
        for block in &func.blocks {
            for inst in &block.instructions {
                if let Instruction::GetElementPtr { dest, base, .. } = inst {
                    if param_alloca_set.contains(&base.0) {
                        gep_to_param.insert(dest.0, base.0);
                    } else if let Some(&root) = gep_to_param.get(&base.0) {
                        gep_to_param.insert(dest.0, root);
                    }
                }
            }
        }

        // Count stores to each param alloca. The initial emit_store_params
        // generates exactly one store per param. Any additional store means
        // the param alloca is modified.
        let mut store_count: FxHashMap<u32, u32> = FxHashMap::default();
        let mut escaped = FxHashSet::default();
        for block in &func.blocks {
            for inst in &block.instructions {
                match inst {
                    Instruction::Store { ptr, .. } => {
                        // Direct store to param alloca
                        if param_alloca_set.contains(&ptr.0) {
                            *store_count.entry(ptr.0).or_insert(0) += 1;
                        }
                        // Store through GEP of param alloca
                        if let Some(&root) = gep_to_param.get(&ptr.0) {
                            *store_count.entry(root).or_insert(0) += 1;
                        }
                    }
                    Instruction::Call { info, .. } | Instruction::CallIndirect { info, .. } => {
                        // If param alloca address (or GEP of it) is passed to
                        // a call, the callee may modify it.
                        for arg in &info.args {
                            if let Operand::Value(v) = arg {
                                if let Some(&root) = gep_to_param.get(&v.0) {
                                    escaped.insert(root);
                                }
                                if param_alloca_set.contains(&v.0) {
                                    escaped.insert(v.0);
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        // A param alloca is "modified" if it has more than 1 store (the
        // initial emit_store_params store) or its address escapes to a call.
        let mut modified = escaped;
        for (&alloca_id, &count) in &store_count {
            if count > 1 {
                modified.insert(alloca_id);
            }
        }
        modified
    };

    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::Alloca { dest, size, ty, align, .. } = inst {
                classify_alloca(
                    state, dest, *size, *ty, *align, ctx,
                    assign_slot, non_local_space, deferred_slots,
                    block_space, max_block_local_space,
                );
            } else if let Instruction::InlineAsm { outputs, operand_types, .. } = inst {
                // Promoted InlineAsm output values need stack slots to hold
                // the output register value. These are "direct" slots (like
                // allocas) -- the slot contains the value itself, not a pointer.
                //
                // However, values defined by non-InlineAsm instructions (Copy,
                // Load, Phi, etc.) are pointer dereference outputs (e.g.,
                // "=r"(*ptr)). After mem2reg, the Load that produced the
                // pointer becomes a Copy, but the value still represents a
                // pre-existing pointer. These must NOT be promoted to direct
                // slots -- their stack slot holds the pointer itself, and
                // store_output_from_reg must store the asm result THROUGH
                // the pointer. Promoting them would cause the result to be
                // written to the slot instead of through the pointer.
                //
                // This check is more robust than checking reg_assigned alone,
                // because it also handles the case where the pointer value
                // is NOT register-allocated (e.g., due to register pressure
                // forcing the pointer to a stack slot).
                for (out_idx, (_, out_val, _)) in outputs.iter().enumerate() {
                    if !state.alloca_values.contains(&out_val.0)
                        && !non_asm_defined.contains(&out_val.0)
                        && collected_values.insert(out_val.0)
                    {
                        let slot_size: i64 = if out_idx < operand_types.len() {
                            match operand_types[out_idx] {
                                IrType::I128 | IrType::U128 | IrType::F128 => 16,
                                _ => 8,
                            }
                        } else {
                            8
                        };
                        state.asm_output_values.insert(out_val.0);
                        multi_block_values.push(MultiBlockValue {
                            dest_id: out_val.0,
                            slot_size,
                        });
                    }
                }
            } else if let Instruction::ParamRef { dest, param_idx, ty } = inst {
                // ParamRef loads a parameter value from its alloca slot.
                // Instead of allocating a separate stack slot for the ParamRef
                // dest, reuse the param alloca's slot. This saves 8 bytes per
                // promoted parameter (significant for kernel functions with
                // many parameters where frame size is critical).
                //
                // Safety: the alloca slot is rounded up to 8 bytes (by the
                // assign_slot callback), so storing a full 8-byte movq is safe.
                // emit_param_ref loads from the alloca with sign/zero extension,
                // then stores back to the same slot, which is a valid self-update
                // that sets the upper bytes to the correct extension.
                //
                // Exception: when the param alloca is modified after the initial
                // store (e.g., by an inlined callee writing to it directly, or
                // its address escaping to a call), the ParamRef must have its
                // own separate slot. Otherwise the ParamRef would read back the
                // modified value instead of the original parameter value.
                if *param_idx < func.param_alloca_values.len() {
                    let alloca_val = func.param_alloca_values[*param_idx];
                    if !modified_param_allocas.contains(&alloca_val.0) {
                    if let Some(&slot) = state.value_locations.get(&alloca_val.0) {
                        state.value_locations.insert(dest.0, slot);
                        // Propagate type tracking even when reusing the alloca
                        // slot, so downstream Copy instructions use the correct
                        // multi-word paths for wide/i128/f128 values.
                        if matches!(ty, IrType::I128 | IrType::U128) {
                            state.i128_values.insert(dest.0);
                        }
                        if crate::common::types::target_is_32bit() {
                            if matches!(ty, IrType::F64 | IrType::I64 | IrType::U64) {
                                state.wide_values.insert(dest.0);
                            }
                        }
                        continue;
                    }
                    }
                }
                // Fallthrough: if no alloca slot found, classify normally.
                classify_value(
                    state, *dest, inst, ctx, reg_assigned,
                    &mut collected_values, multi_block_values,
                    block_local_values,
                );
            } else if let Some(dest) = inst.dest() {
                classify_value(
                    state, dest, inst, ctx, reg_assigned,
                    &mut collected_values, multi_block_values,
                    block_local_values,
                );
            }
        }
    }
}

/// Classify a single Alloca instruction into Tier 1 (permanent) or Tier 3 (block-local).
fn classify_alloca(
    state: &mut super::state::CodegenState,
    dest: &Value,
    size: usize,
    ty: IrType,
    align: usize,
    ctx: &StackLayoutContext,
    assign_slot: &impl Fn(i64, i64, i64) -> (i64, i64),
    non_local_space: &mut i64,
    deferred_slots: &mut Vec<DeferredSlot>,
    block_space: &mut FxHashMap<usize, i64>,
    max_block_local_space: &mut i64,
) {
    let effective_align = align;
    let extra = if effective_align > 16 { effective_align - 1 } else { 0 };
    let ptr_size = crate::common::types::target_ptr_size() as i64;
    // Alloca slots must be at least pointer-sized (8 bytes on 64-bit, 4 on 32-bit)
    // to safely hold ParamRef values that store via movq/sd (full register width).
    let raw_size = if size == 0 {
        ptr_size
    } else {
        (size as i64).max(ptr_size)
    };

    state.alloca_values.insert(dest.0);
    state.alloca_types.insert(dest.0, ty);
    if effective_align > 16 {
        state.alloca_alignments.insert(dest.0, effective_align);
    }

    // Skip dead param allocas (still registered so backend recognizes them).
    if ctx.dead_param_allocas.contains(&dest.0) {
        return;
    }

    // Skip dead non-param allocas (never referenced by any instruction).
    if ctx.coalescable_allocas.dead.contains(&dest.0) {
        return;
    }

    // Single-block allocas: use block-local coalescing (Tier 3).
    // Over-aligned allocas (> 16) are excluded because their alignment
    // padding complicates coalescing.
    if effective_align <= 16 {
        if let Some(&use_block) = ctx.coalescable_allocas.single_block.get(&dest.0) {
            let alloca_size = raw_size + extra as i64;
            let alloca_align = align as i64;
            let bs = block_space.entry(use_block).or_insert(0);
            let before = *bs;
            let (_, new_space) = assign_slot(*bs, alloca_size, alloca_align);
            *bs = new_space;
            if new_space > *max_block_local_space {
                *max_block_local_space = new_space;
            }
            deferred_slots.push(DeferredSlot {
                dest_id: dest.0, size: alloca_size, align: alloca_align,
                block_offset: before,
            });
            return;
        }
    }

    // Non-coalescable allocas get permanent Tier 1 slots.
    let (slot, new_space) = assign_slot(*non_local_space, raw_size + extra as i64, align as i64);
    state.value_locations.insert(dest.0, StackSlot(slot));
    *non_local_space = new_space;
}

/// Classify a non-alloca value into Tier 2 (multi-block) or Tier 3 (block-local).
fn classify_value(
    state: &mut super::state::CodegenState,
    dest: Value,
    inst: &Instruction,
    ctx: &StackLayoutContext,
    reg_assigned: &FxHashSet<u32>,
    collected_values: &mut FxHashSet<u32>,
    multi_block_values: &mut Vec<MultiBlockValue>,
    block_local_values: &mut Vec<BlockLocalValue>,
) {
    let mut is_i128 = matches!(inst.result_type(), Some(IrType::I128) | Some(IrType::U128));
    let is_f128 = matches!(inst.result_type(), Some(IrType::F128))
        || matches!(inst, Instruction::Copy { src: Operand::Const(IrConst::LongDouble(..)), .. });

    // Copy instructions have result_type() = None, so we must check whether
    // the source operand is an I128 value. If it is, the Copy dest also needs
    // a 16-byte slot; otherwise the codegen's emit_copy_i128 will overflow an
    // 8-byte slot into the adjacent stack slot, corrupting other values.
    if !is_i128 {
        if let Instruction::Copy { src: Operand::Value(src_val), .. } = inst {
            if state.i128_values.contains(&src_val.0) {
                is_i128 = true;
            }
        }
    }

    // Detect small values (types that fit in 4 bytes on 64-bit targets).
    // Currently used to populate small_slot_values for future store/load
    // width optimization. Slot allocation remains 8-byte minimum because
    // the backend's store/load paths aren't fully type-safe yet (some paths
    // always use movq/sd/str x0 regardless of IR type).
    let is_small = !crate::common::types::target_is_32bit() && matches!(
        inst.result_type(),
        Some(IrType::I8) | Some(IrType::U8) |
        Some(IrType::I16) | Some(IrType::U16) |
        Some(IrType::I32) | Some(IrType::U32) |
        Some(IrType::F32)
    );
    let slot_size: i64 = if is_i128 || is_f128 {
        16
    } else {
        8
    };

    if is_i128 {
        state.i128_values.insert(dest.0);
    }

    // On 32-bit targets, track values wider than 32 bits for multi-word copy handling.
    if crate::common::types::target_is_32bit() {
        let is_wide = matches!(inst.result_type(),
            Some(IrType::F64) | Some(IrType::I64) | Some(IrType::U64));
        if is_wide {
            state.wide_values.insert(dest.0);
        }
    }

    // Skip register-assigned values (no stack slot needed).
    if reg_assigned.contains(&dest.0) {
        return;
    }

    // Skip dead values (defined but never used).
    if !ctx.used_values.contains(&dest.0) {
        return;
    }

    // Skip copy-aliased values (they'll share root's slot). Not for i128/f128.
    if !is_i128 && !is_f128 && ctx.copy_alias.contains_key(&dest.0) {
        return;
    }

    // Skip immediately-consumed values: produced and consumed in adjacent
    // instructions, kept alive in the accumulator register cache without
    // needing a stack slot. Not for i128/f128 (need 16-byte special handling).
    // On 32-bit targets, also exclude F64/I64/U64 ("wide" values) because
    // they can't fit in the 32-bit accumulator (EAX). F64 values use x87
    // and must be stored to memory between operations; I64/U64 need
    // multi-word handling via edx:eax pairs that require stack slots.
    let is_wide_on_32bit = crate::common::types::target_is_32bit() && matches!(
        inst.result_type(),
        Some(IrType::F64) | Some(IrType::I64) | Some(IrType::U64)
    );
    if !is_i128 && !is_f128 && !is_wide_on_32bit && ctx.immediately_consumed.contains(&dest.0) {
        return;
    }

    // Dedup multi-def values (phi results appear in multiple blocks).
    if !collected_values.insert(dest.0) {
        return;
    }

    // Track values that use 4-byte slots so store/load paths can emit
    // 4-byte instructions (movl, sw/lw, str/ldr w-reg) instead of 8-byte.
    if is_small {
        state.small_slot_values.insert(dest.0);
    }

    if let Some(target_blk) = coalescable_group(dest.0, ctx) {
        block_local_values.push(BlockLocalValue {
            dest_id: dest.0,
            slot_size,
            block_idx: target_blk,
        });
    } else {
        multi_block_values.push(MultiBlockValue {
            dest_id: dest.0,
            slot_size,
        });
    }
}

// ── Phase 3: Tier 3 block-local greedy slot reuse ─────────────────────────

/// Assign stack slots for block-local values using intra-block greedy reuse.
///
/// Within a single block, values have short lifetimes. By tracking when each
/// value is last used, we can reuse its stack slot for later values. This is
/// critical for functions like blake2s_compress_generic where macro expansion
/// creates thousands of short-lived intermediates in a single loop body block.
#[allow(clippy::too_many_arguments)]
fn assign_tier3_block_local_slots(
    func: &IrFunction,
    ctx: &StackLayoutContext,
    coalesce: bool,
    block_local_values: &[BlockLocalValue],
    deferred_slots: &mut Vec<DeferredSlot>,
    block_space: &mut FxHashMap<usize, i64>,
    max_block_local_space: &mut i64,
    assign_slot: &impl Fn(i64, i64, i64) -> (i64, i64),
) {
    if block_local_values.is_empty() {
        return;
    }

    if !coalesce {
        // Fallback: no reuse, just accumulate.
        for blv in block_local_values {
            let bs = block_space.entry(blv.block_idx).or_insert(0);
            let before = *bs;
            let (_, new_space) = assign_slot(*bs, blv.slot_size, 0);
            *bs = new_space;
            if new_space > *max_block_local_space {
                *max_block_local_space = new_space;
            }
            deferred_slots.push(DeferredSlot {
                dest_id: blv.dest_id, size: blv.slot_size, align: 0,
                block_offset: before,
            });
        }
        return;
    }

    // Pre-compute per-block last-use and definition instruction indices.
    let block_local_set: FxHashSet<u32> = block_local_values.iter().map(|v| v.dest_id).collect();
    let mut last_use: FxHashMap<u32, usize> = FxHashMap::default();
    let mut def_inst_idx: FxHashMap<u32, usize> = FxHashMap::default();

    for block in &func.blocks {
        for (inst_idx, inst) in block.instructions.iter().enumerate() {
            if let Some(dest) = inst.dest() {
                if block_local_set.contains(&dest.0) {
                    def_inst_idx.insert(dest.0, inst_idx);
                }
            }
            for_each_operand_in_instruction(inst, |op| {
                if let Operand::Value(v) = op {
                    if block_local_set.contains(&v.0) {
                        last_use.insert(v.0, inst_idx);
                    }
                }
            });
            for_each_value_use_in_instruction(inst, |v| {
                if block_local_set.contains(&v.0) {
                    last_use.insert(v.0, inst_idx);
                }
            });
            // Extend last_use for InlineAsm output pointer values: Phase 4 reads
            // these from stack slots AFTER the asm executes. Also extend
            // copy-alias roots which hold the actual slot.
            if let Instruction::InlineAsm { outputs, .. } = inst {
                for (_, v, _) in outputs {
                    let extended = inst_idx + 1;
                    if block_local_set.contains(&v.0) {
                        last_use.insert(v.0, extended);
                    }
                    if let Some(&root) = ctx.copy_alias.get(&v.0) {
                        if block_local_set.contains(&root) {
                            last_use.insert(root, extended);
                        }
                    }
                }
            }
        }
        for_each_operand_in_terminator(&block.terminator, |op| {
            if let Operand::Value(v) = op {
                if block_local_set.contains(&v.0) {
                    last_use.insert(v.0, block.instructions.len());
                }
            }
        });

        // F128 source pointer liveness extension (Tier 3 block-local mirror).
        //
        // When an F128 Load uses a pointer, the codegen records that pointer so
        // Call emission can reload the full 128-bit value later. The pointer's
        // slot must stay live until the F128 dest's last use, otherwise the
        // greedy slot coloring reuses it and the Call dereferences garbage.
        for inst in &block.instructions {
            if let Instruction::Load { dest, ptr, ty, .. } = inst {
                if *ty == IrType::F128 {
                    if block_local_set.contains(&ptr.0) {
                        if let Some(&dest_last) = last_use.get(&dest.0) {
                            let ptr_last = last_use.get(&ptr.0).copied().unwrap_or(0);
                            if dest_last > ptr_last {
                                last_use.insert(ptr.0, dest_last);
                            }
                            if let Some(&root) = ctx.copy_alias.get(&ptr.0) {
                                if block_local_set.contains(&root) {
                                    let root_last = last_use.get(&root).copied().unwrap_or(0);
                                    if dest_last > root_last {
                                        last_use.insert(root, dest_last);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Group block-local values by block, preserving definition order.
    let mut per_block: FxHashMap<usize, Vec<(u32, i64)>> = FxHashMap::default();
    for blv in block_local_values {
        per_block.entry(blv.block_idx).or_default().push((blv.dest_id, blv.slot_size));
    }

    // For each block, assign slots with greedy coloring.
    for (blk_idx, values) in &per_block {
        let mut active: Vec<(usize, i64, i64)> = Vec::new(); // (last_use, offset, size)
        let mut free_8: Vec<i64> = Vec::new();
        let mut free_16: Vec<i64> = Vec::new();
        let mut block_peak: i64 = block_space.get(blk_idx).copied().unwrap_or(0);

        for &(dest_id, slot_size) in values {
            let my_def = def_inst_idx.get(&dest_id).copied().unwrap_or(0);

            // Release expired slots.
            let mut i = 0;
            while i < active.len() {
                if active[i].0 < my_def {
                    let (_, off, sz) = active.swap_remove(i);
                    if sz == 16 { free_16.push(off); } else { free_8.push(off); }
                } else {
                    i += 1;
                }
            }

            // Try to reuse a freed slot of matching size.
            let free_list = if slot_size == 16 { &mut free_16 } else { &mut free_8 };
            let offset = if let Some(reused) = free_list.pop() {
                reused
            } else {
                let off = block_peak;
                block_peak += slot_size;
                off
            };

            let my_last = last_use.get(&dest_id).copied().unwrap_or(my_def);
            active.push((my_last, offset, slot_size));

            deferred_slots.push(DeferredSlot {
                dest_id,
                size: slot_size,
                align: 0,
                block_offset: offset,
            });
        }

        if block_peak > *max_block_local_space {
            *max_block_local_space = block_peak;
        }
    }
}

// ── Phase 4: Tier 2 liveness-based packing ────────────────────────────────

/// Assign stack slots for multi-block values using liveness-based packing.
///
/// Uses a greedy interval coloring algorithm: sort by start point, greedily assign
/// to the first slot whose previous occupant's interval has ended. This is optimal
/// for interval graphs (chromatic number equals clique number).
fn assign_tier2_liveness_packed_slots(
    state: &mut super::state::CodegenState,
    coalesce: bool,
    cached_liveness: Option<super::liveness::LivenessResult>,
    func: &IrFunction,
    multi_block_values: &[MultiBlockValue],
    non_local_space: &mut i64,
    assign_slot: &impl Fn(i64, i64, i64) -> (i64, i64),
) {
    if multi_block_values.is_empty() {
        return;
    }

    if !coalesce {
        // Fallback: permanent slots for all multi-block values.
        for mbv in multi_block_values {
            let (slot, new_space) = assign_slot(*non_local_space, mbv.slot_size, 0);
            state.value_locations.insert(mbv.dest_id, StackSlot(slot));
            *non_local_space = new_space;
        }
        return;
    }

    // Reuse liveness data from register allocation when available.
    let liveness = cached_liveness.unwrap_or_else(|| compute_live_intervals(func));

    let mut interval_map: FxHashMap<u32, (u32, u32)> = FxHashMap::default();
    for iv in &liveness.intervals {
        interval_map.insert(iv.value_id, (iv.start, iv.end));
    }

    // Separate by slot size for packing (8-byte and 16-byte pools).
    let mut values_8: Vec<(u32, u32, u32)> = Vec::new();
    let mut values_16: Vec<(u32, u32, u32)> = Vec::new();
    let mut no_interval: Vec<(u32, i64)> = Vec::new();

    for mbv in multi_block_values {
        if let Some(&(start, end)) = interval_map.get(&mbv.dest_id) {
            if mbv.slot_size == 16 {
                values_16.push((mbv.dest_id, start, end));
            } else {
                values_8.push((mbv.dest_id, start, end));
            }
        } else {
            no_interval.push((mbv.dest_id, mbv.slot_size));
        }
    }

    pack_values_into_slots(&mut values_8, state, non_local_space, 8, assign_slot);
    pack_values_into_slots(&mut values_16, state, non_local_space, 16, assign_slot);

    // Assign permanent slots for values without interval info.
    for (dest_id, size) in no_interval {
        let (slot, new_space) = assign_slot(*non_local_space, size, 0);
        state.value_locations.insert(dest_id, StackSlot(slot));
        *non_local_space = new_space;
    }
}

/// Pack values with known live intervals into shared stack slots using a min-heap.
/// O(N log S) where N = values and S = slots.
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

    values.sort_by_key(|&(_, start, _)| start);

    use std::collections::BinaryHeap;
    use std::cmp::Reverse;

    let mut heap: BinaryHeap<Reverse<(u32, usize)>> = BinaryHeap::new();
    let mut slot_offsets: Vec<i64> = Vec::new();

    for &(dest_id, start, end) in values.iter() {
        if let Some(&Reverse((slot_end, slot_idx))) = heap.peek() {
            if slot_end < start {
                heap.pop();
                let slot_offset = slot_offsets[slot_idx];
                heap.push(Reverse((end, slot_idx)));
                state.value_locations.insert(dest_id, StackSlot(slot_offset));
                continue;
            }
        }
        let slot_idx = slot_offsets.len();
        let (slot, new_space) = assign_slot(*non_local_space, slot_size, 0);
        state.value_locations.insert(dest_id, StackSlot(slot));
        *non_local_space = new_space;
        slot_offsets.push(slot);
        heap.push(Reverse((end, slot_idx)));
    }
}

// ── Phase 5: Finalize deferred slots ──────────────────────────────────────

/// Assign final offsets for deferred block-local values. All deferred values
/// share a pool starting at `non_local_space`; each value's final slot is
/// computed by adding its block-local offset to the global base.
fn finalize_deferred_slots(
    state: &mut super::state::CodegenState,
    deferred_slots: &[DeferredSlot],
    non_local_space: i64,
    max_block_local_space: i64,
    assign_slot: &impl Fn(i64, i64, i64) -> (i64, i64),
) -> i64 {
    if !deferred_slots.is_empty() && max_block_local_space > 0 {
        for ds in deferred_slots {
            let (slot, _) = assign_slot(non_local_space + ds.block_offset, ds.size, ds.align);
            state.value_locations.insert(ds.dest_id, StackSlot(slot));
        }
        non_local_space + max_block_local_space
    } else {
        non_local_space
    }
}

// ── Phase 6: Copy alias resolution ────────────────────────────────────────

/// Propagate stack slots from root values to their copy aliases.
/// Each aliased value gets the same StackSlot as its root, eliminating
/// a separate slot allocation and making the Copy a harmless self-move.
fn resolve_copy_aliases(
    state: &mut super::state::CodegenState,
    copy_alias: &FxHashMap<u32, u32>,
) {
    for (&dest_id, &root_id) in copy_alias {
        if let Some(&slot) = state.value_locations.get(&root_id) {
            state.value_locations.insert(dest_id, slot);
        }
        // Propagate small-slot property: if the root uses a 4-byte slot,
        // the aliased copy must also use 4-byte store/load to avoid overflow.
        if state.small_slot_values.contains(&root_id) {
            state.small_slot_values.insert(dest_id);
        }
        // If root has no slot (optimized away or reg-assigned), the aliased
        // value also gets no slot. The Copy works via accumulator path.
    }
}

// ── Phase 7: Wide value propagation ───────────────────────────────────────

/// On 32-bit targets, propagate wide-value status through Copy chains.
///
/// Copy instructions for 64-bit values (F64, I64, U64) need 8-byte copies
/// (two movl instructions) instead of the default 4-byte. The initial
/// wide_values set only includes typed instructions; phi elimination creates
/// Copy chains where the destination has no type info. We propagate using
/// fixpoint iteration to handle cycles from phi copies.
fn propagate_wide_values(
    state: &mut super::state::CodegenState,
    func: &IrFunction,
    copy_alias: &FxHashMap<u32, u32>,
) {
    if !crate::common::types::target_is_32bit() || state.wide_values.is_empty() {
        return;
    }

    let mut copy_edges: Vec<(u32, u32)> = Vec::new();
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::Copy { dest, src: Operand::Value(src_val) } = inst {
                copy_edges.push((dest.0, src_val.0));
            }
        }
    }
    // Also propagate through copy aliases (forward only: root -> dest).
    for (&dest_id, &root_id) in copy_alias {
        copy_edges.push((dest_id, root_id));
    }

    if copy_edges.is_empty() {
        return;
    }

    // Fixpoint iteration: propagate wide status until stable.
    let mut changed = true;
    let mut iters = 0;
    while changed && iters < 100 {
        changed = false;
        iters += 1;
        for &(dest_id, src_id) in &copy_edges {
            if state.wide_values.contains(&src_id) && !state.wide_values.contains(&dest_id) {
                state.wide_values.insert(dest_id);
                changed = true;
            }
        }
    }

}

// ── Utility ───────────────────────────────────────────────────────────────

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
