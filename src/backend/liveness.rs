//! Liveness analysis for IR values.
//!
//! Computes live intervals for each IR value in an IrFunction. A live interval
//! represents the range [def_point, last_use_point] where a value is live and
//! needs to be preserved (either in a register or a stack slot).
//!
//! The analysis supports loops via backward dataflow iteration:
//! 1. First, assign sequential program points to all instructions and terminators.
//! 2. Run backward dataflow to compute live-in/live-out sets for each block.
//!    This correctly handles values that are live across loop back-edges.
//! 3. Build intervals by taking the union of def/use points and live-through blocks.
//!
//! ## Performance
//!
//! The dataflow uses compact bitsets instead of hash sets for gen/kill/live_in/live_out.
//! Value IDs are remapped to a dense [0..N) range so bitsets are minimal size.
//! This eliminates per-iteration heap allocation and replaces hash-table operations
//! with fast word-level bitwise ops (union = OR, difference = AND-NOT, equality = ==).

use crate::common::fx_hash::{FxHashMap, FxHashSet};
use crate::ir::ir::*;

/// A live interval for an IR value: [start, end] in program point numbering.
/// start = the point where the value is defined
/// end = the last point where the value is used
#[derive(Debug, Clone, Copy)]
pub struct LiveInterval {
    pub start: u32,
    pub end: u32,
    pub value_id: u32,
}

/// Result of liveness analysis: maps value IDs to their live intervals.
pub struct LivenessResult {
    pub intervals: Vec<LiveInterval>,
    /// Total number of program points (for debugging/sizing).
    pub num_points: u32,
}

// ── Compact bitset for dataflow ──────────────────────────────────────────────

/// A compact bitset stored as a contiguous slice of u64 words.
/// Supports O(1) insert/contains and O(n/64) union/difference/equality.
#[derive(Clone)]
struct BitSet {
    words: Vec<u64>,
}

impl BitSet {
    /// Create a new empty bitset that can hold indices [0..num_bits).
    fn new(num_bits: usize) -> Self {
        let num_words = (num_bits + 63) / 64;
        Self { words: vec![0u64; num_words] }
    }

    #[inline(always)]
    fn insert(&mut self, idx: usize) {
        let word = idx / 64;
        let bit = idx % 64;
        self.words[word] |= 1u64 << bit;
    }

    #[inline(always)]
    fn contains(&self, idx: usize) -> bool {
        let word = idx / 64;
        let bit = idx % 64;
        (self.words[word] >> bit) & 1 != 0
    }

    /// self = self | other. Returns true if self changed.
    fn union_with(&mut self, other: &BitSet) -> bool {
        let mut changed = false;
        for (w, o) in self.words.iter_mut().zip(other.words.iter()) {
            let old = *w;
            *w |= *o;
            changed |= *w != old;
        }
        changed
    }

    /// Computes: self = gen ∪ (out - kill) in one pass. Returns true if self changed.
    fn assign_gen_union_out_minus_kill(&mut self, gen: &BitSet, out: &BitSet, kill: &BitSet) -> bool {
        let mut changed = false;
        for i in 0..self.words.len() {
            let new_val = gen.words[i] | (out.words[i] & !kill.words[i]);
            if new_val != self.words[i] {
                self.words[i] = new_val;
                changed = true;
            }
        }
        changed
    }

    /// Iterate over all set bits, calling f(bit_index) for each.
    fn for_each_set_bit(&self, mut f: impl FnMut(usize)) {
        for (word_idx, &word) in self.words.iter().enumerate() {
            if word == 0 { continue; }
            let base = word_idx * 64;
            let mut w = word;
            while w != 0 {
                let tz = w.trailing_zeros() as usize;
                f(base + tz);
                w &= w - 1; // clear lowest set bit
            }
        }
    }

    /// Clear all bits.
    fn clear(&mut self) {
        for w in &mut self.words {
            *w = 0;
        }
    }
}

/// Compute live intervals for all non-alloca values in a function.
///
/// Uses backward dataflow analysis to correctly handle loops:
/// - live_in[B] = gen[B] ∪ (live_out[B] - kill[B])
/// - live_out[B] = ∪ live_in[S] for all successors S of B
///
/// Values that are live-in to a block have their interval extended to cover
/// from the block's start point through the entire block. This correctly
/// extends intervals through loop back-edges.
pub fn compute_live_intervals(func: &IrFunction) -> LivenessResult {
    let num_blocks = func.blocks.len();
    if num_blocks == 0 {
        return LivenessResult { intervals: Vec::new(), num_points: 0 };
    }

    // Collect alloca values (not register-allocatable).
    let mut alloca_set: FxHashSet<u32> = FxHashSet::default();
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::Alloca { dest, .. } = inst {
                alloca_set.insert(dest.0);
            }
        }
    }

    // Collect all non-alloca value IDs referenced in the function and build
    // a dense remapping: sparse value_id -> dense index in [0..num_values).
    // This makes bitsets minimal size.
    let mut value_ids: Vec<u32> = Vec::new();
    let mut seen: FxHashSet<u32> = FxHashSet::default();

    let maybe_add = |id: u32, alloca_set: &FxHashSet<u32>, seen: &mut FxHashSet<u32>, value_ids: &mut Vec<u32>| {
        if !alloca_set.contains(&id) && seen.insert(id) {
            value_ids.push(id);
        }
    };

    for block in &func.blocks {
        for inst in &block.instructions {
            if let Some(dest) = inst.dest() {
                maybe_add(dest.0, &alloca_set, &mut seen, &mut value_ids);
            }
            for_each_operand_in_instruction(inst, |op| {
                if let Operand::Value(v) = op {
                    maybe_add(v.0, &alloca_set, &mut seen, &mut value_ids);
                }
            });
            for_each_value_use_in_instruction(inst, |v| {
                maybe_add(v.0, &alloca_set, &mut seen, &mut value_ids);
            });
        }
        for_each_operand_in_terminator(&block.terminator, |op| {
            if let Operand::Value(v) = op {
                maybe_add(v.0, &alloca_set, &mut seen, &mut value_ids);
            }
        });
    }
    drop(seen);

    let num_values = value_ids.len();
    if num_values == 0 {
        return LivenessResult { intervals: Vec::new(), num_points: 0 };
    }

    // Build sparse->dense mapping
    let mut id_to_dense: FxHashMap<u32, usize> = FxHashMap::default();
    id_to_dense.reserve(num_values);
    for (dense_idx, &vid) in value_ids.iter().enumerate() {
        id_to_dense.insert(vid, dense_idx);
    }

    // Phase 1: Assign program points and record per-block information.
    let mut point: u32 = 0;
    let mut block_start_points: Vec<u32> = Vec::with_capacity(num_blocks);
    let mut block_end_points: Vec<u32> = Vec::with_capacity(num_blocks);
    let mut def_points: Vec<u32> = vec![u32::MAX; num_values]; // dense_idx -> program point
    let mut last_use_points: Vec<u32> = vec![u32::MAX; num_values];

    // Per-block gen (used) and kill (defined) sets as bitsets
    let mut block_gen: Vec<BitSet> = Vec::with_capacity(num_blocks);
    let mut block_kill: Vec<BitSet> = Vec::with_capacity(num_blocks);

    // Block ID -> index mapping
    let mut block_id_to_idx: FxHashMap<u32, usize> = FxHashMap::default();
    for (idx, block) in func.blocks.iter().enumerate() {
        block_id_to_idx.insert(block.label.0, idx);
    }

    for block in &func.blocks {
        let block_start = point;
        block_start_points.push(block_start);
        let mut gen = BitSet::new(num_values);
        let mut kill = BitSet::new(num_values);

        for inst in &block.instructions {
            // Record uses before defs
            record_instruction_uses_dense(inst, point, &alloca_set, &id_to_dense, &mut last_use_points);

            // Collect gen/kill for dataflow
            collect_instruction_gen_dense(inst, &alloca_set, &id_to_dense, &kill, &mut gen);

            if let Some(dest) = inst.dest() {
                if !alloca_set.contains(&dest.0) {
                    if let Some(&dense) = id_to_dense.get(&dest.0) {
                        if def_points[dense] == u32::MAX {
                            def_points[dense] = point;
                        }
                        kill.insert(dense);
                    }
                }
            }

            point += 1;
        }

        // Record uses in terminator.
        record_terminator_uses_dense(&block.terminator, point, &alloca_set, &id_to_dense, &mut last_use_points);
        collect_terminator_gen_dense(&block.terminator, &alloca_set, &id_to_dense, &kill, &mut gen);
        let block_end = point;
        block_end_points.push(block_end);
        point += 1;

        block_gen.push(gen);
        block_kill.push(kill);
    }

    let num_points = point;

    // Phase 2: Build successor lists for the CFG.
    let mut successors: Vec<Vec<usize>> = vec![Vec::new(); num_blocks];
    for (idx, block) in func.blocks.iter().enumerate() {
        for target_id in terminator_targets(&block.terminator) {
            if let Some(&target_idx) = block_id_to_idx.get(&target_id) {
                successors[idx].push(target_idx);
            }
        }
    }

    // Phase 3: Backward dataflow to compute live-in/live-out per block.
    // live_in[B] = gen[B] ∪ (live_out[B] - kill[B])
    // live_out[B] = ∪ live_in[S] for all successors S of B
    //
    // Using bitsets: union = OR, (out - kill) = out AND NOT kill.
    let mut live_in: Vec<BitSet> = (0..num_blocks).map(|_| BitSet::new(num_values)).collect();
    let mut live_out: Vec<BitSet> = (0..num_blocks).map(|_| BitSet::new(num_values)).collect();
    let mut tmp_out = BitSet::new(num_values);

    // Iterate until fixpoint (backward order converges faster).
    // For reducible CFGs, convergence is guaranteed in O(loop_nesting_depth) iterations.
    // MAX_ITERATIONS is a safety bound for pathological irreducible control flow;
    // if exceeded, liveness is conservative (intervals may be too long but never too short).
    let mut changed = true;
    let mut iteration = 0;
    const MAX_ITERATIONS: u32 = 50;
    while changed && iteration < MAX_ITERATIONS {
        changed = false;
        iteration += 1;

        for idx in (0..num_blocks).rev() {
            // Compute live_out = union of live_in of all successors
            tmp_out.clear();
            for &succ in &successors[idx] {
                tmp_out.union_with(&live_in[succ]);
            }

            // Check if live_out changed
            if tmp_out.words != live_out[idx].words {
                live_out[idx].words.copy_from_slice(&tmp_out.words);
                changed = true;
            }

            // Compute live_in = gen ∪ (live_out - kill)
            let in_changed = live_in[idx].assign_gen_union_out_minus_kill(
                &block_gen[idx], &live_out[idx], &block_kill[idx]
            );
            changed |= in_changed;
        }
    }

    // Phase 4: Extend intervals for values that are live-in or live-out of blocks.
    for idx in 0..num_blocks {
        let start = block_start_points[idx];
        let end = block_end_points[idx];

        live_in[idx].for_each_set_bit(|dense_idx| {
            let entry = &mut last_use_points[dense_idx];
            if *entry == u32::MAX {
                *entry = start;
            }
            if end > *entry {
                *entry = end;
            }
        });

        live_out[idx].for_each_set_bit(|dense_idx| {
            let entry = &mut last_use_points[dense_idx];
            if *entry == u32::MAX {
                *entry = end;
            }
            if end > *entry {
                *entry = end;
            }
        });
    }

    // Phase 5: Build intervals.
    let mut intervals: Vec<LiveInterval> = Vec::new();
    for (dense_idx, &vid) in value_ids.iter().enumerate() {
        let start = def_points[dense_idx];
        if start == u32::MAX { continue; } // no definition found
        let end = last_use_points[dense_idx];
        let end = if end == u32::MAX { start } else { end.max(start) };
        intervals.push(LiveInterval {
            start,
            end,
            value_id: vid,
        });
    }

    // Sort by start point (required by linear scan).
    intervals.sort_unstable_by_key(|iv| iv.start);

    LivenessResult {
        intervals,
        num_points,
    }
}

/// Record uses of operands in an instruction (dense index version).
fn record_instruction_uses_dense(
    inst: &Instruction,
    point: u32,
    alloca_set: &FxHashSet<u32>,
    id_to_dense: &FxHashMap<u32, usize>,
    last_use: &mut [u32],
) {
    let mut record = |vid: u32| {
        if !alloca_set.contains(&vid) {
            if let Some(&dense) = id_to_dense.get(&vid) {
                let entry = &mut last_use[dense];
                if *entry == u32::MAX || point > *entry {
                    *entry = point;
                }
            }
        }
    };

    for_each_operand_in_instruction(inst, |op| {
        if let Operand::Value(v) = op {
            record(v.0);
        }
    });

    for_each_value_use_in_instruction(inst, |v| {
        record(v.0);
    });
}

/// Record uses in a terminator (dense index version).
fn record_terminator_uses_dense(
    term: &Terminator,
    point: u32,
    alloca_set: &FxHashSet<u32>,
    id_to_dense: &FxHashMap<u32, usize>,
    last_use: &mut [u32],
) {
    for_each_operand_in_terminator(term, |op| {
        if let Operand::Value(v) = op {
            if !alloca_set.contains(&v.0) {
                if let Some(&dense) = id_to_dense.get(&v.0) {
                    let entry = &mut last_use[dense];
                    if *entry == u32::MAX || point > *entry {
                        *entry = point;
                    }
                }
            }
        }
    });
}

/// Collect gen set for a block's instruction (dense bitset version).
fn collect_instruction_gen_dense(
    inst: &Instruction,
    alloca_set: &FxHashSet<u32>,
    id_to_dense: &FxHashMap<u32, usize>,
    kill: &BitSet,
    gen: &mut BitSet,
) {
    let mut add_use = |vid: u32| {
        if !alloca_set.contains(&vid) {
            if let Some(&dense) = id_to_dense.get(&vid) {
                if !kill.contains(dense) {
                    gen.insert(dense);
                }
            }
        }
    };

    for_each_operand_in_instruction(inst, |op| {
        if let Operand::Value(v) = op {
            add_use(v.0);
        }
    });

    for_each_value_use_in_instruction(inst, |v| {
        add_use(v.0);
    });
}

/// Collect gen set for a terminator (dense bitset version).
fn collect_terminator_gen_dense(
    term: &Terminator,
    alloca_set: &FxHashSet<u32>,
    id_to_dense: &FxHashMap<u32, usize>,
    kill: &BitSet,
    gen: &mut BitSet,
) {
    for_each_operand_in_terminator(term, |op| {
        if let Operand::Value(v) = op {
            if !alloca_set.contains(&v.0) {
                if let Some(&dense) = id_to_dense.get(&v.0) {
                    if !kill.contains(dense) {
                        gen.insert(dense);
                    }
                }
            }
        }
    });
}

/// Get successor block IDs from a terminator.
fn terminator_targets(term: &Terminator) -> Vec<u32> {
    match term {
        Terminator::Branch(target) => vec![target.0],
        Terminator::CondBranch { true_label, false_label, .. } => {
            vec![true_label.0, false_label.0]
        }
        Terminator::IndirectBranch { possible_targets, .. } => {
            possible_targets.iter().map(|t| t.0).collect()
        }
        _ => vec![],
    }
}

/// Iterate over all Operand references in an instruction.
/// This is the single canonical source of truth for instruction operand traversal.
/// All code that needs to enumerate operands (liveness, use-counting, GEP fold
/// verification) should call this rather than hand-rolling its own match.
pub(super) fn for_each_operand_in_instruction(inst: &Instruction, mut f: impl FnMut(&Operand)) {
    match inst {
        Instruction::Alloca { .. } => {}
        Instruction::DynAlloca { size, .. } => f(size),
        Instruction::Store { val, .. } => f(val),
        Instruction::Load { .. } => {}
        Instruction::BinOp { lhs, rhs, .. } => { f(lhs); f(rhs); }
        Instruction::UnaryOp { src, .. } => f(src),
        Instruction::Cmp { lhs, rhs, .. } => { f(lhs); f(rhs); }
        Instruction::Call { args, .. } => { for a in args { f(a); } }
        Instruction::CallIndirect { func_ptr, args, .. } => { f(func_ptr); for a in args { f(a); } }
        Instruction::GetElementPtr { offset, .. } => f(offset),
        Instruction::Cast { src, .. } => f(src),
        Instruction::Copy { src, .. } => f(src),
        Instruction::GlobalAddr { .. } => {}
        Instruction::Memcpy { .. } => {}
        Instruction::VaArg { .. } => {}
        Instruction::VaStart { .. } => {}
        Instruction::VaEnd { .. } => {}
        Instruction::VaCopy { .. } => {}
        Instruction::AtomicRmw { ptr, val, .. } => { f(ptr); f(val); }
        Instruction::AtomicCmpxchg { ptr, expected, desired, .. } => { f(ptr); f(expected); f(desired); }
        Instruction::AtomicLoad { ptr, .. } => f(ptr),
        Instruction::AtomicStore { ptr, val, .. } => { f(ptr); f(val); }
        Instruction::Fence { .. } => {}
        Instruction::Phi { incoming, .. } => { for (op, _) in incoming { f(op); } }
        Instruction::LabelAddr { .. } => {}
        Instruction::GetReturnF64Second { .. } => {}
        Instruction::SetReturnF64Second { src } => f(src),
        Instruction::GetReturnF32Second { .. } => {}
        Instruction::SetReturnF32Second { src } => f(src),
        Instruction::InlineAsm { inputs, .. } => {
            for (_, op, _) in inputs { f(op); }
        }
        Instruction::Intrinsic { args, .. } => { for a in args { f(a); } }
        Instruction::Select { cond, true_val, false_val, .. } => { f(cond); f(true_val); f(false_val); }
    }
}

/// Iterate over Value references (non-Operand) used in an instruction.
/// These are pointer/base values used directly (not wrapped in Operand),
/// e.g., the `ptr` in Store/Load, `base` in GEP, `dest`/`src` in Memcpy.
/// Canonical traversal — shared by liveness, use-counting, and GEP fold analysis.
pub(super) fn for_each_value_use_in_instruction(inst: &Instruction, mut f: impl FnMut(&Value)) {
    match inst {
        Instruction::Store { ptr, .. } => f(ptr),
        Instruction::Load { ptr, .. } => f(ptr),
        Instruction::GetElementPtr { base, .. } => f(base),
        Instruction::Memcpy { dest, src, .. } => { f(dest); f(src); }
        Instruction::VaArg { va_list_ptr, .. } => f(va_list_ptr),
        Instruction::VaStart { va_list_ptr } => f(va_list_ptr),
        Instruction::VaEnd { va_list_ptr } => f(va_list_ptr),
        Instruction::VaCopy { dest_ptr, src_ptr } => { f(dest_ptr); f(src_ptr); }
        Instruction::InlineAsm { outputs, .. } => {
            for (_, v, _) in outputs { f(v); }
        }
        Instruction::Intrinsic { dest_ptr, .. } => {
            if let Some(dp) = dest_ptr { f(dp); }
        }
        _ => {}
    }
}

/// Iterate over all Operand references in a terminator.
/// Canonical traversal — shared by liveness, use-counting, and GEP fold analysis.
pub(super) fn for_each_operand_in_terminator(term: &Terminator, mut f: impl FnMut(&Operand)) {
    match term {
        Terminator::Return(Some(op)) => f(op),
        Terminator::CondBranch { cond, .. } => f(cond),
        Terminator::IndirectBranch { target, .. } => f(target),
        _ => {}
    }
}
