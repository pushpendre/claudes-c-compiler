//! Linear scan register allocator.
//!
//! Assigns physical registers to IR values based on their live intervals.
//! Values with the longest live ranges and most uses get priority for register
//! assignment. Values that don't fit in available registers remain on the stack.
//!
//! Shared by all backends: x86-64 (rbx, r12-r15), AArch64 (x20-x28), and RISC-V (s1, s7-s11).
//! Uses callee-saved registers so no save/restore is needed around calls.

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
    /// Available registers for allocation (e.g., s1-s11 for RISC-V).
    pub available_regs: Vec<PhysReg>,
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
/// - i128 values (they need register pairs, handled specially)
/// - Values defined by Call/CallIndirect (result is in a0, needs immediate spill)
/// - Values used only once right after definition (no benefit from register)
pub fn allocate_registers(
    func: &IrFunction,
    config: &RegAllocConfig,
) -> RegAllocResult {
    if config.available_regs.is_empty() {
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

    // Liveness analysis now uses backward dataflow iteration to correctly
    // handle loops (values live across back-edges have their intervals extended).
    let liveness = compute_live_intervals(func);

    // Count uses per value for prioritization.
    let mut use_count: FxHashMap<u32, u32> = FxHashMap::default();

    // Use a whitelist approach: only allocate registers for values produced
    // by simple, well-understood instructions that store results via the
    // standard accumulator path (e.g., store_rax_to on x86, store_t0_to on RISC-V).
    let mut eligible: FxHashSet<u32> = FxHashSet::default();

    for block in &func.blocks {
        for inst in &block.instructions {
            // Only BinOp, UnaryOp, Cmp, Cast, Load, and GEP results are eligible.
            // These all store their result via emit_store_result -> store_t0_to.
            // Exclude float and i128 types since they use different register paths.
            match inst {
                Instruction::BinOp { dest, ty, .. }
                | Instruction::UnaryOp { dest, ty, .. } => {
                    if !ty.is_float() && !ty.is_long_double()
                        && !matches!(ty, IrType::I128 | IrType::U128) {
                        eligible.insert(dest.0);
                    }
                }
                Instruction::Cmp { dest, .. } => {
                    eligible.insert(dest.0);
                }
                Instruction::Cast { dest, to_ty, from_ty, .. } => {
                    if !to_ty.is_float() && !to_ty.is_long_double()
                        && !from_ty.is_float() && !from_ty.is_long_double()
                        && !matches!(to_ty, IrType::I128 | IrType::U128)
                        && !matches!(from_ty, IrType::I128 | IrType::U128) {
                        eligible.insert(dest.0);
                    }
                }
                Instruction::Load { dest, ty, .. } => {
                    if !ty.is_float() && !ty.is_long_double()
                        && !matches!(ty, IrType::I128 | IrType::U128) {
                        eligible.insert(dest.0);
                    }
                }
                Instruction::GetElementPtr { dest, .. } => {
                    eligible.insert(dest.0);
                }
                Instruction::Copy { dest, src } => {
                    // Copy instructions (often from phi elimination) are eligible
                    // unless the source is a float, long double, or i128 constant
                    // (these use different register paths).
                    let is_ineligible = matches!(src,
                        Operand::Const(IrConst::F32(_)) | Operand::Const(IrConst::F64(_)) |
                        Operand::Const(IrConst::LongDouble(..)) | Operand::Const(IrConst::I128(_))
                    );
                    if !is_ineligible {
                        eligible.insert(dest.0);
                    }
                }
                _ => {}
            }

            // Count uses of operands via shared iterators.
            for_each_operand_in_instruction(inst, |op| {
                if let Operand::Value(v) = op {
                    *use_count.entry(v.0).or_insert(0) += 1;
                }
            });
        }
        for_each_operand_in_terminator(&block.terminator, |op| {
            if let Operand::Value(v) = op {
                *use_count.entry(v.0).or_insert(0) += 1;
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

    // Prioritize: longer intervals with more uses get registers first.
    // Score = interval_length * use_count (heuristic).
    candidates.sort_by(|a, b| {
        let score_a = (a.end - a.start) as u64 * use_count.get(&a.value_id).copied().unwrap_or(1) as u64;
        let score_b = (b.end - b.start) as u64 * use_count.get(&b.value_id).copied().unwrap_or(1) as u64;
        score_b.cmp(&score_a) // Descending: highest score first
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

    // Build sorted list of used registers.
    let mut used_regs: Vec<PhysReg> = used_regs_set.iter().map(|&r| PhysReg(r)).collect();
    used_regs.sort_by_key(|r| r.0);

    RegAllocResult {
        assignments,
        used_regs,
        liveness: Some(liveness),
    }
}

