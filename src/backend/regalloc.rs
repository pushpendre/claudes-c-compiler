//! Linear scan register allocator.
//!
//! Assigns physical registers to IR values based on their live intervals.
//! Values with the longest live ranges and most uses get priority for register
//! assignment. Values that don't fit in available registers remain on the stack.
//!
//! Three-phase allocation:
//! 1. **Callee-saved registers** (x86: rbx, r12-r15; ARM: x20-x28; RISC-V: s1, s7-s11):
//!    Assigned to values whose live ranges span function calls. These registers
//!    are preserved across calls by the ABI, so no save/restore is needed at call
//!    sites (but prologue/epilogue must save them).
//!
//! 2. **Caller-saved registers** (x86: r11, r10, r8, r9; ARM: x13, x14):
//!    Assigned to values whose live ranges do NOT span any function call. These
//!    registers are destroyed by calls, so they can only hold values between calls.
//!    No prologue/epilogue save/restore is needed since we never assign them to
//!    values that cross call boundaries.
//!
//! 3. **Callee-saved spillover**: After phases 1 and 2, any remaining callee-saved
//!    registers are assigned to the highest-priority non-call-spanning values that
//!    didn't fit in the caller-saved pool. This is critical for call-free hot loops
//!    (e.g., hash functions, matrix multiply, sorting) where all values compete for
//!    only a few caller-saved registers. The one-time prologue/epilogue save/restore
//!    cost is amortized over many loop iterations.

use crate::common::fx_hash::{FxHashMap, FxHashSet};
use crate::common::types::IrType;
use crate::ir::ir::*;
use super::liveness::{LiveInterval, compute_live_intervals, for_each_operand_in_instruction, for_each_operand_in_terminator};

/// A physical register assignment.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PhysReg(pub u8);

/// Result of register allocation for a function.
pub struct RegAllocResult {
    /// Map from value ID -> assigned physical register.
    pub assignments: FxHashMap<u32, PhysReg>,
    /// Set of physical registers actually used (for prologue/epilogue save/restore).
    pub used_regs: Vec<PhysReg>,
    /// The liveness analysis computed during register allocation, if any.
    /// Cached here so that calculate_stack_space_common can reuse it for
    /// Tier 2 liveness-based stack slot packing, avoiding a redundant
    /// O(blocks * values * iterations) dataflow computation.
    /// None when no registers were available (empty available_regs).
    pub liveness: Option<super::liveness::LivenessResult>,
}

/// Configuration for the register allocator.
pub struct RegAllocConfig {
    /// Available callee-saved registers for allocation (e.g., s1-s11 for RISC-V).
    pub available_regs: Vec<PhysReg>,
    /// Available caller-saved registers for allocation.
    /// These are assigned to values whose live ranges do NOT span any call.
    /// Since they don't cross calls, no prologue/epilogue save/restore is needed.
    /// Examples: x86 r11, r10, r8, r9.
    pub caller_saved_regs: Vec<PhysReg>,
}

/// Run the linear scan register allocator on a function.
///
/// Strategy: We assign callee-saved registers to values with the longest
/// live intervals. This is a simplified linear scan that doesn't split
/// intervals — values either get a register for their entire lifetime or
/// remain on the stack.
///
/// We avoid allocating registers to:
/// - Alloca values (they represent stack addresses)
/// - i128/float values (they need special register paths)
/// - Values used only once right after definition (no benefit from register)
pub fn allocate_registers(
    func: &IrFunction,
    config: &RegAllocConfig,
) -> RegAllocResult {
    if config.available_regs.is_empty() && config.caller_saved_regs.is_empty() {
        return RegAllocResult {
            assignments: FxHashMap::default(),
            used_regs: Vec::new(),
            liveness: None,
        };
    }

    // Note: Register allocation is now enabled for functions with atomics.
    // Atomic operations in all backends (x86, ARM, RISC-V) access their operands
    // exclusively through regalloc-aware helpers (operand_to_rax/x0/t0 and
    // store_rax_to/x0_to/t0_to), so register-allocated values work correctly.
    // The atomic pointer operands are individually excluded from register
    // allocation eligibility below (lines ~177-196) since they need stable
    // stack addresses for the memory access instructions.
    //
    // Previously, register allocation was completely disabled for functions with
    // any atomic instruction, which caused massive stack frames (e.g., 19KB for
    // the kernel's ___slab_alloc) and kernel stack overflows.

    // On 32-bit targets, I64/U64 values need two registers (eax:edx) and cannot
    // be allocated to a single callee-saved register. Exclude them from eligibility.
    let is_32bit = crate::common::types::target_is_32bit();

    // Liveness analysis now uses backward dataflow iteration to correctly
    // handle loops (values live across back-edges have their intervals extended).
    let liveness = compute_live_intervals(func);

    // Count uses per value for prioritization, weighted by loop depth.
    //
    // Uses inside loops are weighted more heavily because they execute more
    // frequently. A use inside a loop at depth D contributes 10^D to the
    // weighted use count (so a use in a singly-nested loop counts 10x, doubly-
    // nested counts 100x, etc.). This ensures inner-loop temporaries get
    // priority for register allocation over values in straight-line code,
    // which is critical for performance in compute-heavy loops like zlib's
    // deflate_slow, longest_match, and slide_hash.
    let mut use_count: FxHashMap<u32, u64> = FxHashMap::default();

    // Precompute per-block loop weight: 10^depth, capped to avoid overflow.
    let block_loop_weight: Vec<u64> = liveness.block_loop_depth.iter()
        .map(|&d| {
            match d {
                0 => 1,
                1 => 10,
                2 => 100,
                3 => 1000,
                _ => 10_000, // cap at 10K for very deep nesting
            }
        })
        .collect();

    // Use a whitelist approach: only allocate registers for values produced
    // by simple, well-understood instructions that store results via the
    // standard accumulator path (e.g., store_rax_to on x86, store_t0_to on RISC-V).
    let mut eligible: FxHashSet<u32> = FxHashSet::default();

    // Track values whose types don't fit in a single GPR (floats, i128, and
    // on 32-bit targets: i64/u64). Copy instructions that chain from these
    // values must also be excluded from register allocation.
    let mut non_gpr_values: FxHashSet<u32> = FxHashSet::default();

    // Helper closure to check if a type is unsuitable for GPR allocation
    let is_non_gpr_type = |ty: &IrType| -> bool {
        ty.is_float() || ty.is_long_double()
            || matches!(ty, IrType::I128 | IrType::U128)
            || (is_32bit && matches!(ty, IrType::I64 | IrType::U64))
    };

    // First pass: collect non-GPR values from typed instructions
    for block in &func.blocks {
        for inst in &block.instructions {
            match inst {
                Instruction::BinOp { dest, ty, .. }
                | Instruction::UnaryOp { dest, ty, .. } => {
                    if is_non_gpr_type(ty) {
                        non_gpr_values.insert(dest.0);
                    }
                }
                Instruction::Cast { dest, to_ty, from_ty, .. } => {
                    if is_non_gpr_type(to_ty) || is_non_gpr_type(from_ty) {
                        non_gpr_values.insert(dest.0);
                    }
                }
                Instruction::Load { dest, ty, .. } => {
                    if is_non_gpr_type(ty) {
                        non_gpr_values.insert(dest.0);
                    }
                }
                Instruction::Call { info, .. }
                | Instruction::CallIndirect { info, .. } => {
                    if let Some(dest) = info.dest {
                        if is_non_gpr_type(&info.return_type) {
                            non_gpr_values.insert(dest.0);
                        }
                    }
                }
                Instruction::Select { dest, ty, .. } => {
                    if is_non_gpr_type(ty) {
                        non_gpr_values.insert(dest.0);
                    }
                }
                Instruction::AtomicLoad { dest, ty, .. }
                | Instruction::AtomicRmw { dest, ty, .. }
                | Instruction::AtomicCmpxchg { dest, ty, .. } => {
                    if is_non_gpr_type(ty) {
                        non_gpr_values.insert(dest.0);
                    }
                }
                _ => {}
            }
        }
    }

    // Propagate non-GPR status through Copy chains: if a Copy's source is a
    // non-GPR value, the dest is also non-GPR. Iterate until fixpoint since
    // Copies can chain (Copy a->b, Copy b->c).
    loop {
        let mut changed = false;
        for block in &func.blocks {
            for inst in &block.instructions {
                if let Instruction::Copy { dest, src } = inst {
                    if non_gpr_values.contains(&dest.0) {
                        continue;
                    }
                    let src_is_non_gpr = match src {
                        Operand::Value(v) => non_gpr_values.contains(&v.0),
                        Operand::Const(IrConst::F32(_)) | Operand::Const(IrConst::F64(_))
                        | Operand::Const(IrConst::LongDouble(..))
                        | Operand::Const(IrConst::I128(_)) => true,
                        Operand::Const(IrConst::I64(_)) if is_32bit => true,
                        _ => false,
                    };
                    if src_is_non_gpr {
                        non_gpr_values.insert(dest.0);
                        changed = true;
                    }
                }
            }
        }
        if !changed {
            break;
        }
    }

    for (block_idx, block) in func.blocks.iter().enumerate() {
        // Get the loop weight for this block (default 1 if no loop info available).
        let weight: u64 = if block_idx < block_loop_weight.len() {
            block_loop_weight[block_idx]
        } else {
            1
        };

        for inst in &block.instructions {
            // Values eligible for register allocation: those stored via the
            // standard accumulator path (store_rax_to on x86, store_t0_to on RISC-V).
            // Exclude float and i128 types since they use different register paths.
            match inst {
                Instruction::BinOp { dest, ty, .. }
                | Instruction::UnaryOp { dest, ty, .. } => {
                    if !is_non_gpr_type(ty) {
                        eligible.insert(dest.0);
                    }
                }
                Instruction::Cmp { dest, .. } => {
                    eligible.insert(dest.0);
                }
                Instruction::Cast { dest, to_ty, from_ty, .. } => {
                    if !is_non_gpr_type(to_ty) && !is_non_gpr_type(from_ty) {
                        eligible.insert(dest.0);
                    }
                }
                Instruction::Load { dest, ty, .. } => {
                    if !is_non_gpr_type(ty) {
                        eligible.insert(dest.0);
                    }
                }
                Instruction::GetElementPtr { dest, .. } => {
                    eligible.insert(dest.0);
                }
                Instruction::Copy { dest, src: _ } => {
                    // Copy instructions are eligible unless the source produces a
                    // non-GPR value (float, i128, or i64 on 32-bit). We check both
                    // constant types and propagated non-GPR status from Value sources.
                    if !non_gpr_values.contains(&dest.0) {
                        eligible.insert(dest.0);
                    }
                }
                // Call results are eligible for callee-saved register allocation.
                // The result arrives in the accumulator (rax on x86, x0 on ARM, a0 on
                // RISC-V), and emit_call_store_result calls emit_store_result which
                // uses store_rax_to/store_t0_to — both of which are register-aware
                // and will emit a reg-to-reg move (e.g., movq %rax, %rbx) instead of
                // a stack spill. This is critical for reducing stack frame sizes in
                // functions with many calls (e.g., kernel page allocator functions
                // that call spin_lock, list helpers, etc.) where call results used
                // across subsequent calls would otherwise each consume an 8-byte
                // stack slot.
                Instruction::Call { info, .. }
                | Instruction::CallIndirect { info, .. } => {
                    if let Some(dest) = info.dest {
                        if !is_non_gpr_type(&info.return_type) {
                            eligible.insert(dest.0);
                        }
                    }
                }
                // Select produces its result via the standard accumulator path
                // (cmov on x86, csel on ARM, branch on RISC-V) and stores via
                // store_rax_to/store_t0_to. Eligible unless float/i128.
                Instruction::Select { dest, ty, .. } => {
                    if !is_non_gpr_type(ty) {
                        eligible.insert(dest.0);
                    }
                }
                // GlobalAddr produces a pointer via LEA/adrp+add/la into the
                // accumulator, then stores via store_rax_to/store_t0_to.
                // Always a pointer type, so always eligible.
                Instruction::GlobalAddr { dest, .. } => {
                    eligible.insert(dest.0);
                }
                // LabelAddr produces a pointer via LEA/adrp+add/lla into the
                // accumulator, then stores via store_rax_to/store_t0_to.
                Instruction::LabelAddr { dest, .. } => {
                    eligible.insert(dest.0);
                }
                // Atomic operations store their results via store_rax_to/store_t0_to.
                // AtomicLoad loads a value from memory atomically.
                Instruction::AtomicLoad { dest, ty, .. } => {
                    if !is_non_gpr_type(ty) {
                        eligible.insert(dest.0);
                    }
                }
                // AtomicRmw performs read-modify-write and returns old value.
                Instruction::AtomicRmw { dest, ty, .. } => {
                    if !is_non_gpr_type(ty) {
                        eligible.insert(dest.0);
                    }
                }
                // AtomicCmpxchg returns old value or bool via accumulator.
                Instruction::AtomicCmpxchg { dest, ty, .. } => {
                    if !is_non_gpr_type(ty) {
                        eligible.insert(dest.0);
                    }
                }
                _ => {}
            }

            // Count uses of operands, weighted by loop depth of the containing block.
            // Uses inside loops contribute more to the score, ensuring inner-loop
            // values get priority for register allocation.
            for_each_operand_in_instruction(inst, |op| {
                if let Operand::Value(v) = op {
                    *use_count.entry(v.0).or_insert(0) += weight;
                }
            });
        }
        for_each_operand_in_terminator(&block.terminator, |op| {
            if let Operand::Value(v) = op {
                *use_count.entry(v.0).or_insert(0) += weight;
            }
        });
    }

    // Exclude values used as pointers in CallIndirect, Memcpy,
    // VaArg/VaStart/VaEnd/VaCopy, atomics, or StackRestore. These operations
    // use resolve_slot_addr() in backend-specific codepaths that don't check
    // reg_assignments before accessing the slot.
    //
    // Load/Store/GEP pointer values are NOT excluded: their Indirect codepaths
    // (emit_load_ptr_from_slot, emit_slot_addr_to_secondary, emit_gep_indirect_const)
    // all check reg_assignments first and use the register directly when available.
    // resolve_slot_addr returns a dummy Indirect(StackSlot(0)) for register-assigned
    // values, which is never actually dereferenced.
    for block in &func.blocks {
        for inst in &block.instructions {
            match inst {
                Instruction::CallIndirect { func_ptr, .. } => {
                    if let Operand::Value(v) = func_ptr {
                        eligible.remove(&v.0);
                    }
                }
                Instruction::Memcpy { dest, src, .. } => {
                    eligible.remove(&dest.0);
                    eligible.remove(&src.0);
                }
                Instruction::VaArg { va_list_ptr, .. } => {
                    eligible.remove(&va_list_ptr.0);
                }
                Instruction::VaStart { va_list_ptr } => {
                    eligible.remove(&va_list_ptr.0);
                }
                Instruction::VaEnd { va_list_ptr } => {
                    eligible.remove(&va_list_ptr.0);
                }
                Instruction::VaCopy { dest_ptr, src_ptr } => {
                    eligible.remove(&dest_ptr.0);
                    eligible.remove(&src_ptr.0);
                }
                Instruction::VaArgStruct { dest_ptr, va_list_ptr, .. } => {
                    eligible.remove(&dest_ptr.0);
                    eligible.remove(&va_list_ptr.0);
                }
                Instruction::AtomicRmw { ptr, .. } => {
                    if let Operand::Value(v) = ptr {
                        eligible.remove(&v.0);
                    }
                }
                Instruction::AtomicCmpxchg { ptr, .. } => {
                    if let Operand::Value(v) = ptr {
                        eligible.remove(&v.0);
                    }
                }
                Instruction::AtomicLoad { ptr, .. } => {
                    if let Operand::Value(v) = ptr {
                        eligible.remove(&v.0);
                    }
                }
                Instruction::AtomicStore { ptr, .. } => {
                    if let Operand::Value(v) = ptr {
                        eligible.remove(&v.0);
                    }
                }
                Instruction::StackRestore { ptr } => {
                    eligible.remove(&ptr.0);
                }
                Instruction::InlineAsm { outputs, inputs, .. } => {
                    // Inline asm operands are accessed via resolve_slot_addr()
                    // in codegen, which requires a valid stack slot.
                    for (_, val, _) in outputs {
                        eligible.remove(&val.0);
                    }
                    for (_, op, _) in inputs {
                        if let Operand::Value(v) = op {
                            eligible.remove(&v.0);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    // Filter intervals to only eligible values whose live ranges span across
    // at least one function call. Callee-saved registers exist to preserve values
    // across calls — using them for values that don't cross call boundaries wastes
    // stack space on save/restore without benefit (the value could stay on the stack
    // or in a caller-saved register). This optimization significantly reduces stack
    // frame sizes, especially for functions with many short-lived temporaries.
    let call_points = &liveness.call_points;
    let mut candidates: Vec<&LiveInterval> = liveness.intervals.iter()
        .filter(|iv| eligible.contains(&iv.value_id))
        .filter(|iv| iv.end > iv.start) // Must span at least 2 points
        .filter(|iv| {
            // Only allocate callee-saved regs to values that span a call.
            // Use binary search since call_points is sorted by program point.
            // Check if any call point falls within [start, end].
            let start_idx = call_points.partition_point(|&cp| cp < iv.start);
            start_idx < call_points.len() && call_points[start_idx] <= iv.end
        })
        .collect();

    // Prioritize by weighted use count (loop-aware).
    //
    // The weighted use count accounts for loop nesting depth: uses inside loops
    // are weighted by 10^depth, so inner-loop values automatically get much
    // higher scores. This replaces the previous interval_length * use_count
    // heuristic which didn't account for execution frequency and caused
    // inner-loop temporaries to be spilled to the stack.
    //
    // Tie-breaking by interval length (descending) ensures that among values
    // with equal weighted use counts, longer-lived values (which benefit more
    // from being in a register) are preferred.
    candidates.sort_by(|a, b| {
        let score_a = use_count.get(&a.value_id).copied().unwrap_or(1) as u64;
        let score_b = use_count.get(&b.value_id).copied().unwrap_or(1) as u64;
        score_b.cmp(&score_a)
            .then_with(|| {
                let len_a = (a.end - a.start) as u64;
                let len_b = (b.end - b.start) as u64;
                len_b.cmp(&len_a)
            })
    });

    // Linear scan: greedily assign registers.
    // Each register tracks when it becomes free (the end point of the value using it).
    let num_regs = config.available_regs.len();
    let mut reg_free_until: Vec<u32> = vec![0; num_regs];
    let mut assignments: FxHashMap<u32, PhysReg> = FxHashMap::default();
    let mut used_regs_set: FxHashSet<u8> = FxHashSet::default();

    // Register reuse preference: prefer already-saved registers over new ones.
    //
    // Each distinct callee-saved register used costs 8 bytes of stack space
    // in the prologue (save) and epilogue (restore). Reusing a register that's
    // already being saved has zero additional frame cost.
    //
    // By preferring already-used registers, we pack values into fewer callee-saved
    // registers when possible. This reduces the number of save/restore entries in
    // the prologue/epilogue, shrinking the stack frame. This is especially impactful
    // for functions with many short-lived intermediate values (e.g., from I32->I64
    // promotion) where sequential values can reuse the same register.

    for interval in &candidates {
        // Find a register that is free at this interval's start.
        // Prefer reusing a register that's already being saved/restored (zero
        // additional frame cost) over introducing a new callee-saved register.
        let mut best_already_used: Option<usize> = None;
        let mut best_already_used_free_time: u32 = u32::MAX;
        let mut best_new: Option<usize> = None;
        let mut best_new_free_time: u32 = u32::MAX;

        for (i, &free_until) in reg_free_until.iter().enumerate() {
            if free_until <= interval.start {
                let reg_id = config.available_regs[i].0;
                if used_regs_set.contains(&reg_id) {
                    // Already saved/restored — reusing costs nothing extra.
                    if best_already_used.is_none() || free_until < best_already_used_free_time {
                        best_already_used = Some(i);
                        best_already_used_free_time = free_until;
                    }
                } else {
                    // Would introduce a new callee-saved register.
                    if best_new.is_none() || free_until < best_new_free_time {
                        best_new = Some(i);
                        best_new_free_time = free_until;
                    }
                }
            }
        }

        // Choose: always prefer an already-saved register (free to reuse).
        // Fall back to a new callee-saved register if none is available.
        let chosen = best_already_used.or(best_new);

        if let Some(reg_idx) = chosen {
            // Assign this register.
            reg_free_until[reg_idx] = interval.end + 1;
            assignments.insert(interval.value_id, config.available_regs[reg_idx]);
            used_regs_set.insert(config.available_regs[reg_idx].0);
        }
        // If no register available, the value stays on the stack (no spill insertion
        // needed since the stack slot already exists).
    }

    // Build sorted list of used callee-saved registers (for prologue/epilogue).
    let mut used_regs: Vec<PhysReg> = used_regs_set.iter().map(|&r| PhysReg(r)).collect();
    used_regs.sort_by_key(|r| r.0);

    // Phase 2: Caller-saved register allocation.
    //
    // Assign caller-saved registers to eligible values whose live ranges do NOT
    // span any function call. Since these registers are destroyed by calls, we
    // can only use them for values that live entirely between calls (or in
    // call-free regions). No prologue/epilogue save/restore is needed.
    if !config.caller_saved_regs.is_empty() {
        let mut caller_candidates: Vec<&LiveInterval> = liveness.intervals.iter()
            .filter(|iv| eligible.contains(&iv.value_id))
            .filter(|iv| !assignments.contains_key(&iv.value_id)) // Not already in a callee-saved reg
            .filter(|iv| iv.end > iv.start) // Must span at least 2 points
            .filter(|iv| {
                // Only allocate caller-saved regs to values that do NOT span a call.
                let start_idx = call_points.partition_point(|&cp| cp < iv.start);
                !(start_idx < call_points.len() && call_points[start_idx] <= iv.end)
            })
            .collect();

        // Prioritize by weighted use count (loop-aware), same as Phase 1.
        caller_candidates.sort_by(|a, b| {
            let score_a = use_count.get(&a.value_id).copied().unwrap_or(1) as u64;
            let score_b = use_count.get(&b.value_id).copied().unwrap_or(1) as u64;
            score_b.cmp(&score_a)
                .then_with(|| {
                    let len_a = (a.end - a.start) as u64;
                    let len_b = (b.end - b.start) as u64;
                    len_b.cmp(&len_a)
                })
        });

        // Linear scan for caller-saved registers.
        let num_caller_regs = config.caller_saved_regs.len();
        let mut caller_free_until: Vec<u32> = vec![0; num_caller_regs];

        for interval in &caller_candidates {
            let mut best: Option<usize> = None;
            let mut best_free_time: u32 = u32::MAX;

            for (i, &free_until) in caller_free_until.iter().enumerate() {
                if free_until <= interval.start {
                    if best.is_none() || free_until < best_free_time {
                        best = Some(i);
                        best_free_time = free_until;
                    }
                }
            }

            if let Some(reg_idx) = best {
                caller_free_until[reg_idx] = interval.end + 1;
                assignments.insert(interval.value_id, config.caller_saved_regs[reg_idx]);
            }
        }
    }

    // Phase 3: Callee-saved spillover for non-call-spanning values.
    //
    // After Phase 1 (callee-saved for call-spanning) and Phase 2 (caller-saved for
    // non-call-spanning), there may be high-priority values in call-free loops that
    // didn't get a register because the caller-saved pool (4 regs on x86) overflowed.
    // Meanwhile, callee-saved registers may be sitting unused because the function
    // has few or no call-spanning values.
    //
    // Assign remaining callee-saved registers to these overflow values. The cost is
    // a one-time prologue/epilogue save/restore (16 bytes of stack per register),
    // but the benefit is eliminating repeated stack spills in hot loops. For a loop
    // that iterates N times, this saves ~2*N memory accesses per value promoted.
    {
        let mut spillover_candidates: Vec<&LiveInterval> = liveness.intervals.iter()
            .filter(|iv| eligible.contains(&iv.value_id))
            .filter(|iv| !assignments.contains_key(&iv.value_id)) // Not already allocated
            .filter(|iv| iv.end > iv.start) // Must span at least 2 points
            .filter(|iv| {
                // Only non-call-spanning values (caller-saved overflow).
                let start_idx = call_points.partition_point(|&cp| cp < iv.start);
                !(start_idx < call_points.len() && call_points[start_idx] <= iv.end)
            })
            .collect();

        // Prioritize by weighted use count (loop-aware), same as Phases 1 & 2.
        spillover_candidates.sort_by(|a, b| {
            let score_a = use_count.get(&a.value_id).copied().unwrap_or(1) as u64;
            let score_b = use_count.get(&b.value_id).copied().unwrap_or(1) as u64;
            score_b.cmp(&score_a)
                .then_with(|| {
                    let len_a = (a.end - a.start) as u64;
                    let len_b = (b.end - b.start) as u64;
                    len_b.cmp(&len_a)
                })
        });

        // Use the existing reg_free_until from Phase 1 since it tracks callee-saved
        // register availability.
        for interval in &spillover_candidates {
            // Prefer reusing already-saved registers (zero additional frame cost).
            let mut best_already_used: Option<usize> = None;
            let mut best_already_used_free_time: u32 = u32::MAX;
            let mut best_new: Option<usize> = None;
            let mut best_new_free_time: u32 = u32::MAX;

            for (i, &free_until) in reg_free_until.iter().enumerate() {
                if free_until <= interval.start {
                    let reg_id = config.available_regs[i].0;
                    if used_regs_set.contains(&reg_id) {
                        if best_already_used.is_none() || free_until < best_already_used_free_time {
                            best_already_used = Some(i);
                            best_already_used_free_time = free_until;
                        }
                    } else {
                        if best_new.is_none() || free_until < best_new_free_time {
                            best_new = Some(i);
                            best_new_free_time = free_until;
                        }
                    }
                }
            }

            let chosen = best_already_used.or(best_new);

            if let Some(reg_idx) = chosen {
                reg_free_until[reg_idx] = interval.end + 1;
                assignments.insert(interval.value_id, config.available_regs[reg_idx]);
                used_regs_set.insert(config.available_regs[reg_idx].0);
            }
        }

        // Rebuild used_regs since Phase 3 may have added new callee-saved registers.
        used_regs = used_regs_set.iter().map(|&r| PhysReg(r)).collect();
        used_regs.sort_by_key(|r| r.0);
    }

    RegAllocResult {
        assignments,
        used_regs,
        liveness: Some(liveness),
    }
}

