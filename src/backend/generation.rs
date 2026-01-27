//! Module, function, and instruction generation dispatch.
//!
//! This module contains the top-level entry points that drive code generation:
//! - `generate_module`: emits data sections and iterates over functions
//! - `generate_function`: emits prologue, basic blocks, and epilogue
//! - `generate_instruction`: dispatches each IR instruction to arch trait methods
//! - `generate_terminator`: dispatches terminators to arch trait methods
//!
//! These functions are arch-independent — they use the `ArchCodegen` trait to call
//! into the backend-specific implementations.

use crate::ir::ir::*;
use crate::common::types::{AddressSpace, IrType};
use crate::common::fx_hash::{FxHashMap, FxHashSet};
use super::common;
use super::traits::ArchCodegen;
use super::state::StackSlot;
use super::liveness::{for_each_operand_in_instruction, for_each_value_use_in_instruction, for_each_operand_in_terminator, compute_live_intervals};

/// Information about a GEP with a constant offset that can be folded into
/// Load/Store addressing modes. Instead of computing `base + offset` as a
/// separate instruction and spilling to stack, the constant offset is merged
/// directly into the memory operand of the subsequent load/store.
#[derive(Debug, Clone, Copy)]
pub(super) struct GepFoldInfo {
    /// The base pointer value (an alloca or previously-computed pointer).
    pub(super) base: Value,
    /// The constant byte offset to add to the base address.
    pub(super) offset: i64,
}

/// Build a map of GEP destinations that can be folded into Load/Store instructions.
///
/// A GEP is foldable when:
/// 1. Its offset is a compile-time constant (Operand::Const)
/// 2. The constant fits in a 32-bit signed displacement (x86 addressing limit)
/// 3. The GEP result is only used as the ptr operand of Load/Store instructions
///    (not used by other instructions, terminators, or as a value operand)
///
/// When all conditions are met, the GEP instruction is skipped during codegen,
/// and each Load/Store that uses it receives the (base, offset) directly.
fn build_gep_fold_map(func: &IrFunction, use_counts: &[u32]) -> FxHashMap<u32, GepFoldInfo> {
    let mut gep_map: FxHashMap<u32, GepFoldInfo> = FxHashMap::default();

    // Phase 1: Collect all GEPs with constant offsets.
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::GetElementPtr { dest, base, offset: Operand::Const(c), .. } = inst {
                let offset_val = match c {
                    IrConst::I64(n) => *n,
                    IrConst::I32(n) => *n as i64,
                    IrConst::I16(n) => *n as i64,
                    IrConst::I8(n) => *n as i64,
                    _ => continue,
                };
                // Offset must fit in 32-bit signed displacement for x86.
                // Also reasonable for ARM (signed 9-bit unscaled or 12-bit scaled)
                // and RISC-V (signed 12-bit).
                // Use i32 range as the safe common limit.
                if offset_val >= i32::MIN as i64 && offset_val <= i32::MAX as i64 {
                    gep_map.insert(dest.0, GepFoldInfo { base: *base, offset: offset_val });
                }
            }
        }
    }

    if gep_map.is_empty() {
        return gep_map;
    }

    // Phase 2: Verify that each candidate GEP dest is ONLY used as Load/Store ptr.
    // If it's used anywhere else (as a value operand, in a call, in a terminator,
    // or as a base of another GEP), we cannot fold it.
    //
    // Strategy: Load.ptr and Store.ptr are the ONLY foldable use positions.
    // - Load: ptr is a Value (visited by for_each_value_use), no Operand uses → skip entirely.
    // - Store: ptr (Value) is foldable, but val (Operand) is NOT → check only Operand uses.
    // - All other instructions: ANY reference to a GEP dest invalidates folding.
    let mut non_ptr_uses: FxHashSet<u32> = FxHashSet::default();

    // Helper: mark a GEP dest as non-foldable if used outside Load/Store ptr position.
    let mut mark_non_ptr = |id: u32| {
        if gep_map.contains_key(&id) {
            non_ptr_uses.insert(id);
        }
    };

    for block in &func.blocks {
        for inst in &block.instructions {
            match inst {
                // Load.ptr is foldable — UNLESS the load type is i128/u128,
                // because the i128 load path in generate_instruction doesn't
                // support GEP folding and falls through to emit_load which
                // expects the pointer value to have been computed.
                Instruction::Load { ptr, ty, .. } => {
                    if matches!(ty, IrType::I128 | IrType::U128) {
                        mark_non_ptr(ptr.0);
                    }
                }
                // Store.ptr is foldable, but Store.val is an Operand that is NOT foldable.
                // Also invalidate if the store type is i128/u128, for the same
                // reason as Load above: the i128 store path doesn't fold GEPs.
                Instruction::Store { val, ptr, ty , .. } => {
                    if let Operand::Value(v) = val { mark_non_ptr(v.0); }
                    if matches!(ty, IrType::I128 | IrType::U128) {
                        mark_non_ptr(ptr.0);
                    }
                }
                // All other instructions: any reference invalidates folding.
                _ => {
                    for_each_operand_in_instruction(inst, |op| {
                        if let Operand::Value(v) = op { mark_non_ptr(v.0); }
                    });
                    for_each_value_use_in_instruction(inst, |v| mark_non_ptr(v.0));
                }
            }
        }
        for_each_operand_in_terminator(&block.terminator, |op| {
            if let Operand::Value(v) = op { mark_non_ptr(v.0); }
        });
    }

    // Remove GEPs that have non-ptr uses.
    for val_id in &non_ptr_uses {
        gep_map.remove(val_id);
    }

    // Also remove GEPs that are unused (use_count == 0).
    gep_map.retain(|val_id, _| {
        (*val_id as usize) < use_counts.len() && use_counts[*val_id as usize] > 0
    });

    gep_map
}

/// Build a map from Value IDs to global symbol names (with optional offsets).
/// Maps values produced by `GlobalAddr { name }` to `"name"`, and values
/// produced by `GEP(GlobalAddr { name }, const_offset)` to `"name+offset"`.
/// Used to emit direct symbol(%rip) references for segment-overridden loads/stores.
fn build_global_addr_map(func: &IrFunction) -> FxHashMap<u32, String> {
    let mut map: FxHashMap<u32, String> = FxHashMap::default();
    for block in &func.blocks {
        for inst in &block.instructions {
            match inst {
                Instruction::GlobalAddr { dest, name } => {
                    map.insert(dest.0, name.clone());
                }
                Instruction::GetElementPtr { dest, base, offset: Operand::Const(c), .. } => {
                    if let Some(base_name) = map.get(&base.0) {
                        let offset_val = match c {
                            IrConst::I64(n) => *n,
                            IrConst::I32(n) => *n as i64,
                            IrConst::I16(n) => *n as i64,
                            IrConst::I8(n) => *n as i64,
                            _ => continue,
                        };
                        let sym = if offset_val == 0 {
                            base_name.clone()
                        } else if offset_val > 0 {
                            format!("{}+{}", base_name, offset_val)
                        } else {
                            format!("{}{}", base_name, offset_val)
                        };
                        map.insert(dest.0, sym);
                    }
                }
                _ => {}
            }
        }
    }
    map
}

/// Returns the number of times each IR Value is used as an operand in
/// instructions or terminators. Indexed by Value ID; used to identify
/// single-use values eligible for compare-branch fusion.
fn count_value_uses(func: &IrFunction) -> Vec<u32> {
    // Find the max value ID to size the vector.
    let mut max_id: u32 = 0;
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Some(dest) = inst.dest() {
                max_id = max_id.max(dest.0);
            }
        }
    }
    let mut counts = vec![0u32; max_id as usize + 1];

    // Helper: increment use count for a value ID, bounds-checked.
    let mut count_id = |id: u32| {
        if (id as usize) < counts.len() {
            counts[id as usize] += 1;
        }
    };

    for block in &func.blocks {
        for inst in &block.instructions {
            for_each_operand_in_instruction(inst, |op| {
                if let Operand::Value(v) = op { count_id(v.0); }
            });
            for_each_value_use_in_instruction(inst, |v| count_id(v.0));
        }
        for_each_operand_in_terminator(&block.terminator, |op| {
            if let Operand::Value(v) = op { count_id(v.0); }
        });
    }
    counts
}

/// Detect if a block's last instruction is a Cmp whose result is only used
/// by the block's CondBranch terminator. Returns the index of the Cmp if
/// fusion is possible, None otherwise.
fn detect_cmp_branch_fusion(block: &BasicBlock, use_counts: &[u32]) -> Option<usize> {
    // Terminator must be a CondBranch
    let (cond, _, _) = match &block.terminator {
        Terminator::CondBranch { cond, true_label, false_label } => (cond, true_label, false_label),
        _ => return None,
    };

    // The condition must be a Value (not a constant)
    let cond_val = match cond {
        Operand::Value(v) => v,
        _ => return None,
    };

    // Find the last instruction that is a Cmp producing this value
    let last_idx = block.instructions.len().checked_sub(1)?;
    let last_inst = &block.instructions[last_idx];

    let (dest, _op, _lhs, _rhs, ty) = match last_inst {
        Instruction::Cmp { dest, op, lhs, rhs, ty } => (dest, op, lhs, rhs, ty),
        _ => return None,
    };

    // The Cmp dest must be the same as the CondBranch cond
    if dest.0 != cond_val.0 {
        return None;
    }

    // Don't fuse i128 or float comparisons (they have special codegen paths)
    if is_i128_type(*ty) || ty.is_float() {
        return None;
    }

    // The Cmp result must be used exactly once (by the CondBranch terminator)
    if (cond_val.0 as usize) < use_counts.len() && use_counts[cond_val.0 as usize] == 1 {
        Some(last_idx)
    } else {
        None
    }
}

/// Generate assembly for a module using the given architecture's codegen.
pub fn generate_module(cg: &mut dyn ArchCodegen, module: &IrModule) -> String {
    // Pre-size the output buffer based on total IR instruction count to avoid
    // repeated reallocations. Each IR instruction typically generates ~40 bytes
    // of assembly text.
    {
        let total_insts: usize = module.functions.iter()
            .map(|f| f.blocks.iter().map(|b| b.instructions.len()).sum::<usize>())
            .sum();
        let estimated_bytes = (total_insts * 40).max(256 * 1024).min(64 * 1024 * 1024);
        let state = cg.state();
        if state.out.buf.capacity() < estimated_bytes {
            state.out.buf.reserve(estimated_bytes - state.out.buf.capacity());
        }
    }

    // Build the set of locally-defined symbols for PIC mode.
    // Symbols with hidden/internal/protected visibility are resolved at link time
    // (not load time), so they don't need GOT/PLT indirection.
    {
        let state = cg.state();
        for func in &module.functions {
            if func.is_static || matches!(func.visibility.as_deref(), Some("hidden" | "internal" | "protected")) {
                state.local_symbols.insert(func.name.clone());
            }
        }
        for global in &module.globals {
            if global.is_static || matches!(global.visibility.as_deref(), Some("hidden" | "internal" | "protected")) {
                state.local_symbols.insert(global.name.clone());
            }
        }
        // Also mark symbols from extern declarations with hidden/internal/protected
        // visibility as local — they're guaranteed to be resolved within the link unit.
        for (name, _is_weak, visibility) in &module.symbol_attrs {
            if matches!(visibility.as_deref(), Some("hidden" | "internal" | "protected")) {
                state.local_symbols.insert(name.clone());
            }
        }
        for (label, _) in &module.string_literals {
            state.local_symbols.insert(label.clone());
        }
        for (label, _) in &module.wide_string_literals {
            state.local_symbols.insert(label.clone());
        }
    }

    // Emit data sections
    let ptr_dir = cg.ptr_directive();
    common::emit_data_sections(&mut cg.state().out, module, ptr_dir);

    // Emit top-level asm("...") directives verbatim (e.g., musl's _start definition)
    // Switch to .text first so that labels/code in the asm land in the correct section
    // (after emit_data_sections we may be in .bss or .data).
    if !module.toplevel_asm.is_empty() {
        cg.state().emit(".text");
        for asm_str in &module.toplevel_asm {
            cg.state().emit(asm_str);
        }
    }

    // Emit visibility directives for declaration-only (extern) functions with non-default
    // visibility. This ensures the assembler marks undefined symbols correctly for PIC.
    for func in &module.functions {
        if func.is_declaration {
            if let Some(ref vis) = func.visibility {
                match vis.as_str() {
                    "hidden" => cg.state().emit_fmt(format_args!(".hidden {}", func.name)),
                    "protected" => cg.state().emit_fmt(format_args!(".protected {}", func.name)),
                    "internal" => cg.state().emit_fmt(format_args!(".internal {}", func.name)),
                    _ => {}
                }
            }
        }
    }

    // Text section
    cg.state().emit(".section .text");
    let mut in_custom_section = false;
    for func in &module.functions {
        if !func.is_declaration {
            if let Some(ref sect) = func.section {
                cg.state().emit_fmt(format_args!(".section {},\"ax\",@progbits", sect));
                cg.state().current_text_section = sect.clone();
                in_custom_section = true;
            } else if in_custom_section {
                cg.state().emit(".section .text");
                cg.state().current_text_section = ".text".to_string();
                in_custom_section = false;
            } else {
                cg.state().current_text_section = ".text".to_string();
            }
            generate_function(cg, func);
        }
    }

    // Emit symbol aliases from __attribute__((alias("target")))
    for (alias_name, target_name, is_weak) in &module.aliases {
        cg.state().emit("");
        if *is_weak {
            cg.state().emit_fmt(format_args!(".weak {}", alias_name));
        } else {
            cg.state().emit_fmt(format_args!(".globl {}", alias_name));
        }
        cg.state().emit_fmt(format_args!(".set {},{}", alias_name, target_name));
    }

    // Emit symbol attribute directives (.weak, .hidden) for declarations.
    // Visibility directives like .hidden are emitted for both defined and undefined symbols.
    // For undefined (extern) symbols, .hidden tells the linker that the symbol will be
    // resolved within the same link unit, which is critical for PIC code to avoid
    // unnecessary GOT/PLT indirection (e.g., in the Linux kernel EFI stub).
    for (name, is_weak, visibility) in &module.symbol_attrs {
        if *is_weak {
            cg.state().emit_fmt(format_args!(".weak {}", name));
        }
        if let Some(ref vis) = visibility {
            match vis.as_str() {
                "hidden" => cg.state().emit_fmt(format_args!(".hidden {}", name)),
                "protected" => cg.state().emit_fmt(format_args!(".protected {}", name)),
                "internal" => cg.state().emit_fmt(format_args!(".internal {}", name)),
                _ => {}
            }
        }
    }

    // Emit .init_array for constructor functions
    for ctor in &module.constructors {
        cg.state().emit("");
        cg.state().emit(".section .init_array,\"aw\",@init_array");
        cg.state().emit_fmt(format_args!(".align {}", ptr_dir.align_arg(8)));
        cg.state().emit_fmt(format_args!("{} {}", ptr_dir.as_str(), ctor));
    }

    // Emit .fini_array for destructor functions
    for dtor in &module.destructors {
        cg.state().emit("");
        cg.state().emit(".section .fini_array,\"aw\",@fini_array");
        cg.state().emit_fmt(format_args!(".align {}", ptr_dir.align_arg(8)));
        cg.state().emit_fmt(format_args!("{} {}", ptr_dir.as_str(), dtor));
    }

    // Emit .note.GNU-stack section to indicate non-executable stack
    cg.state().emit("");
    cg.state().emit(".section .note.GNU-stack,\"\",@progbits");

    std::mem::take(&mut cg.state().out.buf)
}

/// Generate code for a single function.
fn generate_function(cg: &mut dyn ArchCodegen, func: &IrFunction) {
    cg.state().reset_for_function();

    let type_dir = cg.function_type_directive();
    if !func.is_static {
        if func.is_weak {
            cg.state().emit_fmt(format_args!(".weak {}", func.name));
        } else {
            cg.state().emit_fmt(format_args!(".globl {}", func.name));
        }
    }
    // Emit visibility directive from __attribute__((visibility("..."))) on function defs
    if let Some(ref vis) = func.visibility {
        match vis.as_str() {
            "hidden" => cg.state().emit_fmt(format_args!(".hidden {}", func.name)),
            "protected" => cg.state().emit_fmt(format_args!(".protected {}", func.name)),
            "internal" => cg.state().emit_fmt(format_args!(".internal {}", func.name)),
            _ => {} // "default" or unknown: no directive needed
        }
    }

    // Emit patchable function entry NOP padding (-fpatchable-function-entry=N,M).
    // This is used by the Linux kernel for ftrace and static call patching.
    // Format: M NOPs before the entry point, (N-M) NOPs after, plus a
    // __patchable_function_entries section pointing to the NOP area.
    //
    // Skip patchable entries for inline functions: our compiler emits all static
    // inline functions from headers as separate definitions (since we don't inline
    // them yet). Emitting __patchable_function_entries for each of these would create
    // thousands of entries per file (~1400 instead of ~5), overwhelming the kernel's
    // ftrace initialization and causing boot hangs. GCC avoids this by inlining
    // static inline functions so they never get their own patchable entries.
    let emit_patchable = !func.is_inline;
    if emit_patchable {
        if let Some((total, before)) = cg.state().patchable_function_entry {
            if total > 0 {
                let pfe_id = cg.state().next_label_id();
                let pfe_label = format!(".LPFE{}", pfe_id);

                // Emit __patchable_function_entries section with a pointer to the NOP area
                cg.state().emit_fmt(format_args!(
                    ".section __patchable_function_entries,\"awo\",@progbits,{}",
                    pfe_label
                ));
                cg.state().emit(".align 8");
                cg.state().emit_fmt(format_args!(".quad {}", pfe_label));

                // Switch back to the function's section (custom or .text)
                if let Some(ref sect) = func.section {
                    cg.state().emit_fmt(format_args!(".section {},\"ax\",@progbits", sect));
                } else {
                    cg.state().emit(".text");
                }

                // Emit the LPFE label and M NOPs before the function entry point
                cg.state().emit_fmt(format_args!("{}:", pfe_label));
                for _ in 0..before {
                    cg.state().emit("nop");
                }
            }
        }
    }

    cg.state().emit_fmt(format_args!(".type {}, {}", func.name, type_dir));
    cg.state().emit_fmt(format_args!("{}:", func.name));

    // Emit (N-M) NOPs after the function entry point for patchable function entry
    if emit_patchable {
        if let Some((total, before)) = cg.state().patchable_function_entry {
            let after = total.saturating_sub(before);
            for _ in 0..after {
                cg.state().emit("nop");
            }
        }
    }

    // Pre-scan for DynAlloca/StackRestore: if present, the epilogue must restore SP from
    // the frame pointer instead of adding back the compile-time frame size.
    let has_dyn_alloca = func.blocks.iter().any(|block| {
        block.instructions.iter().any(|inst| matches!(inst, Instruction::DynAlloca { .. } | Instruction::StackRestore { .. }))
    });
    cg.state().has_dyn_alloca = has_dyn_alloca;

    // Calculate stack space and emit prologue
    let raw_space = cg.calculate_stack_space(func);
    let frame_size = cg.aligned_frame_size(raw_space);
    cg.emit_prologue(func, frame_size);

    // Store parameters
    cg.emit_store_params(func);

    // Generate basic blocks
    let entry_label = func.blocks.first().map(|b| b.label);

    // Pre-scan: count uses of each Value across the entire function to identify
    // single-use Cmp results eligible for compare-branch fusion.
    let value_use_counts = count_value_uses(func);

    // Pre-scan: identify GEPs with constant offsets that can be folded into
    // Load/Store addressing modes, eliminating the GEP instruction entirely.
    let gep_fold_map = build_gep_fold_map(func, &value_use_counts);

    // Pre-scan: map Value IDs to global symbol names (with offsets from GEP).
    // Used to emit direct symbol(%rip) references for segment-overridden loads/stores.
    let global_addr_map = build_global_addr_map(func);

    for block in &func.blocks {
        if Some(block.label) != entry_label {
            // Invalidate register cache at block boundaries: a value in a register
            // from the previous block's fall-through is not guaranteed to be valid
            // if control arrives from a different predecessor.
            cg.state().reg_cache.invalidate_all();
            cg.state().out.emit_block_label(block.label.0);
        }

        // Check for compare-branch fusion opportunity:
        // If the last instruction is a Cmp whose result is only used by the
        // CondBranch terminator, emit a fused compare-and-conditional-jump
        // instead of materializing the boolean result to a register/stack slot.
        let fuse_idx = detect_cmp_branch_fusion(block, &value_use_counts);

        for (idx, inst) in block.instructions.iter().enumerate() {
            if Some(idx) == fuse_idx {
                // Skip this Cmp -- it will be emitted fused with the terminator
                continue;
            }
            // Skip GEP instructions whose offset has been folded into Load/Store.
            // Only skip when the GEP's base is an alloca (Direct or OverAligned),
            // because alloca slots are stable and never reused by liveness packing.
            // For non-alloca (Indirect) bases, the base pointer's slot might be
            // reused by another value between the GEP definition and the Load/Store
            // use point (due to Tier 2/3 slot coalescing), so we must eagerly
            // compute the GEP result rather than deferring to the Load/Store point.
            if let Instruction::GetElementPtr { dest, base, .. } = inst {
                if gep_fold_map.contains_key(&dest.0) && cg.state_ref().is_alloca(base.0) {
                    continue;
                }
            }
            generate_instruction(cg, inst, &gep_fold_map, &global_addr_map);
        }

        if let Some(fi) = fuse_idx {
            // Emit fused compare-and-branch: cmp + jCC directly
            if let Instruction::Cmp { dest: _, op, lhs, rhs, ty } = &block.instructions[fi] {
                if let Terminator::CondBranch { cond: _, true_label, false_label } = &block.terminator {
                    cg.emit_fused_cmp_branch_blocks(*op, lhs, rhs, *ty, *true_label, *false_label);
                }
            }
        } else {
            generate_terminator(cg, &block.terminator, frame_size);
        }
    }

    cg.state().emit_fmt(format_args!(".size {}, .-{}", func.name, func.name));
    cg.state().emit("");
}

/// Dispatch a single IR instruction to the appropriate arch method.
///
/// Register cache management strategy:
/// The cache tracks which IR value is currently in the accumulator register
/// (rax on x86, x0 on ARM, t0 on RISC-V).
///
/// Many instructions follow the pattern: load operand(s) → compute → store_result(dest),
/// which means the accumulator holds dest's value when the instruction completes.
/// For these "acc-preserving" instructions, we keep the cache valid so the next
/// instruction can skip reloading the result.
///
/// Instructions that clobber the accumulator unpredictably (calls, stores, atomics,
/// inline asm, va_arg, memcpy, etc.) invalidate the cache after execution.
fn generate_instruction(cg: &mut dyn ArchCodegen, inst: &Instruction, gep_fold_map: &FxHashMap<u32, GepFoldInfo>, global_addr_map: &FxHashMap<u32, String>) {
    match inst {
        Instruction::Alloca { .. } => {
            // Space already allocated in prologue; does not touch registers
        }
        Instruction::Copy { dest, src } => {
            // Skip Copy when dest and src share the same stack slot (from copy coalescing).
            // The Copy is a no-op since both refer to the same memory location.
            if let Operand::Value(src_val) = src {
                let dest_slot = cg.state_ref().get_slot(dest.0);
                let src_slot = cg.state_ref().get_slot(src_val.0);
                if let (Some(ds), Some(ss)) = (dest_slot, src_slot) {
                    if ds.0 == ss.0 {
                        // Same slot: skip the copy entirely. Update reg cache so that
                        // if src was cached in the accumulator, dest is too.
                        if cg.state_ref().reg_cache.acc_has(src_val.0, false) {
                            cg.state().reg_cache.set_acc(dest.0, false);
                        }
                        return;
                    }
                }
            }

            let is_i128_copy = match src {
                Operand::Value(v) => cg.state_ref().is_i128_value(v.0),
                Operand::Const(IrConst::I128(_)) => true,
                _ => false,
            };
            if is_i128_copy {
                cg.state().i128_values.insert(dest.0);
                cg.emit_copy_i128(dest, src);
                cg.state().reg_cache.invalidate_all();
            } else {
                // Copy: use emit_copy_value which may emit a direct register-to-register
                // copy if the backend supports it, avoiding the accumulator roundtrip.
                cg.emit_copy_value(dest, src);
            }
        }

        // ── Acc-preserving instructions ──────────────────────────────────
        // These all end with emit_store_result(dest) or store_rax_to(dest),
        // which sets the reg cache correctly. The accumulator holds dest's
        // value after execution, so we do NOT invalidate.

        Instruction::Load { dest, ptr, ty, seg_override } => {
            if *seg_override != AddressSpace::Default {
                // Segment-overridden load: if the pointer is a global address,
                // emit a direct symbol(%rip) reference (e.g., %gs:symbol(%rip))
                // instead of loading into a register (which would use the absolute
                // address as a segment offset, corrupting memory).
                if let Some(sym) = global_addr_map.get(&ptr.0) {
                    cg.emit_seg_load_symbol(dest, sym, *ty, *seg_override);
                } else {
                    cg.emit_seg_load(dest, ptr, *ty, *seg_override);
                }
                return;
            }
            // Check if the ptr comes from a foldable GEP with constant offset.
            // Only fold when the GEP's base is an alloca, because alloca slots
            // are stable and never reused by liveness packing. Non-alloca base
            // slots may be overwritten between GEP definition and Load use.
            if let Some(gep_info) = gep_fold_map.get(&ptr.0) {
                if !is_i128_type(*ty) && cg.state_ref().is_alloca(gep_info.base.0) {
                    cg.emit_load_with_const_offset(dest, &gep_info.base, gep_info.offset, *ty);
                    return;
                }
            }
            // emit_load ends with emit_store_result(dest) for non-i128.
            // For i128, it ends with emit_store_acc_pair(dest).
            cg.emit_load(dest, ptr, *ty);
            if is_i128_type(*ty) {
                cg.state().reg_cache.invalidate_all();
            }
            // Non-i128: cache is set by emit_store_result(dest) inside emit_load.
        }
        Instruction::BinOp { dest, op, lhs, rhs, ty } => {
            // emit_binop dispatches to emit_int_binop (ends with store_rax_to),
            // emit_float_binop (ends with emit_store_result), or emit_i128_binop.
            cg.emit_binop(dest, *op, lhs, rhs, *ty);
            if is_i128_type(*ty) {
                cg.state().reg_cache.invalidate_all();
            }
            // Non-i128: cache is set by store_rax_to(dest) / emit_store_result(dest).
        }
        Instruction::UnaryOp { dest, op, src, ty } => {
            // emit_unaryop ends with emit_store_result(dest) for non-i128.
            cg.emit_unaryop(dest, *op, src, *ty);
            if is_i128_type(*ty) {
                cg.state().reg_cache.invalidate_all();
            }
        }
        Instruction::Cmp { dest, op, lhs, rhs, ty } => {
            // emit_cmp ends with store_rax_to(dest) for each backend.
            cg.emit_cmp(dest, *op, lhs, rhs, *ty);
            // Cmp result is always i32/i8, never i128. Cache is valid.
        }
        Instruction::Cast { dest, src, from_ty, to_ty } => {
            // emit_cast ends with emit_store_result(dest) for non-i128 paths.
            cg.emit_cast(dest, src, *from_ty, *to_ty);
            if is_i128_type(*to_ty) || is_i128_type(*from_ty) {
                cg.state().reg_cache.invalidate_all();
            }
        }
        Instruction::GetElementPtr { dest, base, offset, .. } => {
            // emit_gep ends with emit_store_result(dest).
            cg.emit_gep(dest, base, offset);
            // Cache is set by emit_store_result(dest) inside emit_gep.
        }
        Instruction::GlobalAddr { dest, name } => {
            // emit_global_addr loads an address and stores to dest.
            cg.emit_global_addr(dest, name);
            // The implementation calls emit_store_result(dest), so cache is valid.
        }
        Instruction::Select { dest, cond, true_val, false_val, ty } => {
            cg.emit_select(dest, cond, true_val, false_val, *ty);
            // emit_select ends with emit_store_result(dest). Cache is valid.
        }
        Instruction::LabelAddr { dest, label } => {
            let label_str = label.as_label();
            cg.emit_label_addr(dest, &label_str);
            // emit_label_addr calls emit_store_result(dest).
        }

        // ── Cache-invalidating instructions ──────────────────────────────
        // These clobber the accumulator unpredictably or don't produce a
        // simple acc → dest result.

        _ => {
            match inst {
                Instruction::DynAlloca { dest, size, align } => cg.emit_dyn_alloca(dest, size, *align),
                Instruction::Store { val, ptr, ty, seg_override } => {
                    if *seg_override != AddressSpace::Default {
                        if let Some(sym) = global_addr_map.get(&ptr.0) {
                            cg.emit_seg_store_symbol(val, sym, *ty, *seg_override);
                        } else {
                            cg.emit_seg_store(val, ptr, *ty, *seg_override);
                        }
                    } else if let Some(gep_info) = gep_fold_map.get(&ptr.0) {
                        // Fold GEP into store (alloca bases only; see Load comment).
                        if !is_i128_type(*ty) && cg.state_ref().is_alloca(gep_info.base.0) {
                            cg.emit_store_with_const_offset(val, &gep_info.base, gep_info.offset, *ty);
                        } else {
                            cg.emit_store(val, ptr, *ty);
                        }
                    } else {
                        cg.emit_store(val, ptr, *ty);
                    }
                }
                Instruction::Call { dest, func, args, arg_types, return_type, is_variadic, num_fixed_args, struct_arg_sizes, struct_arg_classes } =>
                    cg.emit_call(args, arg_types, Some(func), None, *dest, *return_type, *is_variadic, *num_fixed_args, struct_arg_sizes, struct_arg_classes),
                Instruction::CallIndirect { dest, func_ptr, args, arg_types, return_type, is_variadic, num_fixed_args, struct_arg_sizes, struct_arg_classes } =>
                    cg.emit_call(args, arg_types, None, Some(func_ptr), *dest, *return_type, *is_variadic, *num_fixed_args, struct_arg_sizes, struct_arg_classes),
                Instruction::Memcpy { dest, src, size } => cg.emit_memcpy(dest, src, *size),
                Instruction::VaArg { dest, va_list_ptr, result_ty } => cg.emit_va_arg(dest, va_list_ptr, *result_ty),
                Instruction::VaStart { va_list_ptr } => cg.emit_va_start(va_list_ptr),
                Instruction::VaEnd { va_list_ptr } => cg.emit_va_end(va_list_ptr),
                Instruction::VaCopy { dest_ptr, src_ptr } => cg.emit_va_copy(dest_ptr, src_ptr),
                Instruction::AtomicRmw { dest, op, ptr, val, ty, ordering } => cg.emit_atomic_rmw(dest, *op, ptr, val, *ty, *ordering),
                Instruction::AtomicCmpxchg { dest, ptr, expected, desired, ty, success_ordering, failure_ordering, returns_bool } =>
                    cg.emit_atomic_cmpxchg(dest, ptr, expected, desired, *ty, *success_ordering, *failure_ordering, *returns_bool),
                Instruction::AtomicLoad { dest, ptr, ty, ordering } => cg.emit_atomic_load(dest, ptr, *ty, *ordering),
                Instruction::AtomicStore { ptr, val, ty, ordering } => cg.emit_atomic_store(ptr, val, *ty, *ordering),
                Instruction::Fence { ordering } => cg.emit_fence(*ordering),
                Instruction::Phi { .. } => { /* resolved before codegen */ }
                Instruction::GetReturnF64Second { dest } => cg.emit_get_return_f64_second(dest),
                Instruction::SetReturnF64Second { src } => cg.emit_set_return_f64_second(src),
                Instruction::GetReturnF32Second { dest } => cg.emit_get_return_f32_second(dest),
                Instruction::SetReturnF32Second { src } => cg.emit_set_return_f32_second(src),
                Instruction::InlineAsm { template, outputs, inputs, clobbers, operand_types, goto_labels, input_symbols, seg_overrides } =>
                    cg.emit_inline_asm_with_segs(template, outputs, inputs, clobbers, operand_types, goto_labels, input_symbols, seg_overrides),
                Instruction::Intrinsic { dest, op, dest_ptr, args } => cg.emit_intrinsic(dest, op, dest_ptr, args),
                Instruction::StackSave { dest } => cg.emit_stack_save(dest),
                Instruction::StackRestore { ptr } => cg.emit_stack_restore(ptr),
                Instruction::Alloca { .. } | Instruction::Copy { .. }
                | Instruction::Load { .. } | Instruction::BinOp { .. }
                | Instruction::UnaryOp { .. } | Instruction::Cmp { .. }
                | Instruction::Cast { .. } | Instruction::GetElementPtr { .. }
                | Instruction::GlobalAddr { .. } | Instruction::LabelAddr { .. }
                | Instruction::Select { .. } => unreachable!(),
            }
            cg.state().reg_cache.invalidate_all();
        }
    }
}

/// Dispatch a terminator to the appropriate arch method.
fn generate_terminator(cg: &mut dyn ArchCodegen, term: &Terminator, frame_size: i64) {
    match term {
        Terminator::Return(val) => {
            cg.emit_return(val.as_ref(), frame_size);
        }
        Terminator::Branch(label) => {
            cg.emit_branch_to_block(*label);
        }
        Terminator::CondBranch { cond, true_label, false_label } => {
            cg.emit_cond_branch_blocks(cond, *true_label, *false_label);
        }
        Terminator::IndirectBranch { target, .. } => {
            cg.emit_indirect_branch(target);
        }
        Terminator::Switch { val, cases, default } => {
            cg.emit_switch(val, cases, default);
        }
        Terminator::Unreachable => {
            cg.emit_unreachable();
        }
    }
}

/// Check if an IR type is a 128-bit integer type (I128 or U128).
pub fn is_i128_type(ty: IrType) -> bool {
    matches!(ty, IrType::I128 | IrType::U128)
}

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
