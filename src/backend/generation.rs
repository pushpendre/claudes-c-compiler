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
use crate::common::types::IrType;
use crate::common::fx_hash::{FxHashMap, FxHashSet};
use super::common;
use super::traits::ArchCodegen;
use super::state::StackSlot;

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
    let mut non_ptr_uses: FxHashSet<u32> = FxHashSet::default();

    for block in &func.blocks {
        for inst in &block.instructions {
            match inst {
                // Load/Store: ptr usage is foldable, but val usage is not.
                Instruction::Load { ptr, .. } => {
                    // ptr usage is OK — this is exactly what we want to fold.
                    let _ = ptr;
                }
                Instruction::Store { val, ptr, .. } => {
                    // ptr usage is OK, but val usage is not foldable.
                    let _ = ptr;
                    if let Operand::Value(v) = val {
                        if gep_map.contains_key(&v.0) {
                            non_ptr_uses.insert(v.0);
                        }
                    }
                }
                // Any other instruction that references a GEP dest as a value
                // operand means we can't fold it.
                _ => {
                    // Check all operand positions for references to GEP dests.
                    let check_op = |op: &Operand, set: &mut FxHashSet<u32>| {
                        if let Operand::Value(v) = op {
                            if gep_map.contains_key(&v.0) {
                                set.insert(v.0);
                            }
                        }
                    };
                    let check_val = |v: &Value, set: &mut FxHashSet<u32>| {
                        if gep_map.contains_key(&v.0) {
                            set.insert(v.0);
                        }
                    };
                    match inst {
                        Instruction::BinOp { lhs, rhs, .. } | Instruction::Cmp { lhs, rhs, .. } => {
                            check_op(lhs, &mut non_ptr_uses);
                            check_op(rhs, &mut non_ptr_uses);
                        }
                        Instruction::UnaryOp { src, .. } | Instruction::Cast { src, .. }
                        | Instruction::Copy { src, .. } => {
                            check_op(src, &mut non_ptr_uses);
                        }
                        Instruction::GetElementPtr { base, offset, .. } => {
                            // GEP base: if it points to another foldable GEP, can't fold.
                            check_val(base, &mut non_ptr_uses);
                            check_op(offset, &mut non_ptr_uses);
                        }
                        Instruction::Call { args, .. } => {
                            for a in args { check_op(a, &mut non_ptr_uses); }
                        }
                        Instruction::CallIndirect { func_ptr, args, .. } => {
                            check_op(func_ptr, &mut non_ptr_uses);
                            for a in args { check_op(a, &mut non_ptr_uses); }
                        }
                        Instruction::Memcpy { dest, src, .. } => {
                            check_val(dest, &mut non_ptr_uses);
                            check_val(src, &mut non_ptr_uses);
                        }
                        Instruction::Phi { incoming, .. } => {
                            for (op, _) in incoming { check_op(op, &mut non_ptr_uses); }
                        }
                        Instruction::AtomicRmw { ptr, val, .. } => {
                            check_op(ptr, &mut non_ptr_uses);
                            check_op(val, &mut non_ptr_uses);
                        }
                        Instruction::AtomicCmpxchg { ptr, expected, desired, .. } => {
                            check_op(ptr, &mut non_ptr_uses);
                            check_op(expected, &mut non_ptr_uses);
                            check_op(desired, &mut non_ptr_uses);
                        }
                        Instruction::AtomicLoad { ptr, .. } => check_op(ptr, &mut non_ptr_uses),
                        Instruction::AtomicStore { ptr, val, .. } => {
                            check_op(ptr, &mut non_ptr_uses);
                            check_op(val, &mut non_ptr_uses);
                        }
                        Instruction::DynAlloca { size, .. } => check_op(size, &mut non_ptr_uses),
                        Instruction::VaArg { va_list_ptr, .. } | Instruction::VaStart { va_list_ptr }
                        | Instruction::VaEnd { va_list_ptr } => check_val(va_list_ptr, &mut non_ptr_uses),
                        Instruction::VaCopy { dest_ptr, src_ptr } => {
                            check_val(dest_ptr, &mut non_ptr_uses);
                            check_val(src_ptr, &mut non_ptr_uses);
                        }
                        Instruction::SetReturnF64Second { src } | Instruction::SetReturnF32Second { src } =>
                            check_op(src, &mut non_ptr_uses),
                        Instruction::InlineAsm { inputs, .. } => {
                            for (_, op, _) in inputs { check_op(op, &mut non_ptr_uses); }
                        }
                        Instruction::Intrinsic { args, .. } => {
                            for a in args { check_op(a, &mut non_ptr_uses); }
                        }
                        _ => {}
                    }
                }
            }
        }
        // Also check terminators.
        match &block.terminator {
            Terminator::CondBranch { cond, .. } => {
                if let Operand::Value(v) = cond {
                    if gep_map.contains_key(&v.0) {
                        non_ptr_uses.insert(v.0);
                    }
                }
            }
            Terminator::Return(Some(op)) => {
                if let Operand::Value(v) = op {
                    if gep_map.contains_key(&v.0) {
                        non_ptr_uses.insert(v.0);
                    }
                }
            }
            Terminator::IndirectBranch { target, .. } => {
                if let Operand::Value(v) = target {
                    if gep_map.contains_key(&v.0) {
                        non_ptr_uses.insert(v.0);
                    }
                }
            }
            _ => {}
        }
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

/// Returns the number of times each IR Value is used as an operand in
/// instructions or terminators. Indexed by Value ID; used to identify
/// single-use values eligible for compare-branch fusion.
fn count_value_uses(func: &IrFunction) -> Vec<u32> {
    // Find the max value ID to size the vector
    let mut max_id: u32 = 0;
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Some(dest) = inst.dest() {
                max_id = max_id.max(dest.0);
            }
        }
    }
    let mut counts = vec![0u32; max_id as usize + 1];

    let count_op = |op: &Operand, counts: &mut Vec<u32>| {
        if let Operand::Value(v) = op {
            if (v.0 as usize) < counts.len() {
                counts[v.0 as usize] += 1;
            }
        }
    };

    let count_val = |v: &Value, counts: &mut Vec<u32>| {
        if (v.0 as usize) < counts.len() {
            counts[v.0 as usize] += 1;
        }
    };

    for block in &func.blocks {
        for inst in &block.instructions {
            match inst {
                Instruction::BinOp { lhs, rhs, .. } | Instruction::Cmp { lhs, rhs, .. } => {
                    count_op(lhs, &mut counts); count_op(rhs, &mut counts);
                }
                Instruction::UnaryOp { src, .. } | Instruction::Cast { src, .. }
                | Instruction::Copy { src, .. } => count_op(src, &mut counts),
                Instruction::Load { ptr, .. } => count_val(ptr, &mut counts),
                Instruction::Store { val, ptr, .. } => {
                    count_op(val, &mut counts); count_val(ptr, &mut counts);
                }
                Instruction::GetElementPtr { base, offset, .. } => {
                    count_val(base, &mut counts); count_op(offset, &mut counts);
                }
                Instruction::Call { args, .. } => { for a in args { count_op(a, &mut counts); } }
                Instruction::CallIndirect { func_ptr, args, .. } => {
                    count_op(func_ptr, &mut counts);
                    for a in args { count_op(a, &mut counts); }
                }
                Instruction::Memcpy { dest, src, .. } => {
                    count_val(dest, &mut counts); count_val(src, &mut counts);
                }
                Instruction::Phi { incoming, .. } => {
                    for (op, _) in incoming { count_op(op, &mut counts); }
                }
                Instruction::AtomicRmw { ptr, val, .. } => {
                    count_op(ptr, &mut counts); count_op(val, &mut counts);
                }
                Instruction::AtomicCmpxchg { ptr, expected, desired, .. } => {
                    count_op(ptr, &mut counts); count_op(expected, &mut counts);
                    count_op(desired, &mut counts);
                }
                Instruction::AtomicLoad { ptr, .. } => count_op(ptr, &mut counts),
                Instruction::AtomicStore { ptr, val, .. } => {
                    count_op(ptr, &mut counts); count_op(val, &mut counts);
                }
                Instruction::DynAlloca { size, .. } => count_op(size, &mut counts),
                Instruction::VaArg { va_list_ptr, .. } | Instruction::VaStart { va_list_ptr }
                | Instruction::VaEnd { va_list_ptr } => count_val(va_list_ptr, &mut counts),
                Instruction::VaCopy { dest_ptr, src_ptr } => {
                    count_val(dest_ptr, &mut counts); count_val(src_ptr, &mut counts);
                }
                Instruction::SetReturnF64Second { src } | Instruction::SetReturnF32Second { src } =>
                    count_op(src, &mut counts),
                Instruction::InlineAsm { inputs, .. } => {
                    for (_, op, _) in inputs { count_op(op, &mut counts); }
                }
                Instruction::Intrinsic { args, .. } => { for a in args { count_op(a, &mut counts); } }
                _ => {} // Alloca, GlobalAddr, LabelAddr, GetReturn*, Fence have no Value operands
            }
        }
        // Count uses in terminators
        match &block.terminator {
            Terminator::CondBranch { cond, .. } => count_op(cond, &mut counts),
            Terminator::Return(Some(op)) => count_op(op, &mut counts),
            Terminator::IndirectBranch { target, .. } => count_op(target, &mut counts),
            _ => {}
        }
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
    {
        let state = cg.state();
        for func in &module.functions {
            if func.is_static {
                state.local_symbols.insert(func.name.clone());
            }
        }
        for global in &module.globals {
            if global.is_static {
                state.local_symbols.insert(global.name.clone());
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
    for asm_str in &module.toplevel_asm {
        cg.state().emit(asm_str);
    }

    // Text section
    cg.state().emit(".section .text");
    let mut in_custom_section = false;
    for func in &module.functions {
        if !func.is_declaration {
            if let Some(ref sect) = func.section {
                cg.state().emit_fmt(format_args!(".section {},\"ax\",@progbits", sect));
                in_custom_section = true;
            } else if in_custom_section {
                cg.state().emit(".section .text");
                in_custom_section = false;
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
    // Only emit visibility directives for symbols that are actually defined in this module,
    // since hidden/protected visibility on extern-only declarations would cause linker errors.
    let defined_funcs: FxHashSet<&str> = module.functions.iter()
        .filter(|f| !f.is_declaration)
        .map(|f| f.name.as_str())
        .collect();
    let defined_globals: FxHashSet<&str> = module.globals.iter()
        .filter(|g| !g.is_extern)
        .map(|g| g.name.as_str())
        .collect();
    // Also collect alias names as "defined" since .set creates a definition
    let alias_names: FxHashSet<&str> = module.aliases.iter()
        .map(|(name, _, _)| name.as_str())
        .collect();
    for (name, is_weak, visibility) in &module.symbol_attrs {
        let is_defined = defined_funcs.contains(name.as_str())
            || defined_globals.contains(name.as_str())
            || alias_names.contains(name.as_str());
        if *is_weak {
            cg.state().emit_fmt(format_args!(".weak {}", name));
        }
        // Only emit visibility for defined symbols
        if is_defined {
            if let Some(ref vis) = visibility {
                match vis.as_str() {
                    "hidden" => cg.state().emit_fmt(format_args!(".hidden {}", name)),
                    "protected" => cg.state().emit_fmt(format_args!(".protected {}", name)),
                    "internal" => cg.state().emit_fmt(format_args!(".internal {}", name)),
                    _ => {}
                }
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
        cg.state().emit_fmt(format_args!(".globl {}", func.name));
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
    cg.state().emit_fmt(format_args!(".type {}, {}", func.name, type_dir));
    cg.state().emit_fmt(format_args!("{}:", func.name));

    // Pre-scan for DynAlloca: if present, the epilogue must restore SP from
    // the frame pointer instead of adding back the compile-time frame size.
    let has_dyn_alloca = func.blocks.iter().any(|block| {
        block.instructions.iter().any(|inst| matches!(inst, Instruction::DynAlloca { .. }))
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

    for block in &func.blocks {
        if Some(block.label) != entry_label {
            // Invalidate register cache at block boundaries: a value in a register
            // from the previous block's fall-through is not guaranteed to be valid
            // if control arrives from a different predecessor.
            cg.state().reg_cache.invalidate_all();
            cg.state().emit_fmt(format_args!("{}:", block.label));
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
            // Note: Only skip when the GEP's base is an alloca (Direct or OverAligned),
            // because alloca slots are always valid %rbp-relative. For Indirect bases
            // (pointer in stack slot), the GEP must still execute because the register
            // allocator may reuse the base's register between the GEP and the Load/Store.
            if let Instruction::GetElementPtr { dest, base, .. } = inst {
                if gep_fold_map.contains_key(&dest.0) && cg.state_ref().is_alloca(base.0) {
                    continue;
                }
            }
            generate_instruction(cg, inst, &gep_fold_map);
        }

        if let Some(fi) = fuse_idx {
            // Emit fused compare-and-branch: cmp + jCC directly
            if let Instruction::Cmp { dest: _, op, lhs, rhs, ty } = &block.instructions[fi] {
                if let Terminator::CondBranch { cond: _, true_label, false_label } = &block.terminator {
                    let true_str = true_label.as_label();
                    let false_str = false_label.as_label();
                    cg.emit_fused_cmp_branch(*op, lhs, rhs, *ty, &true_str, &false_str);
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
fn generate_instruction(cg: &mut dyn ArchCodegen, inst: &Instruction, gep_fold_map: &FxHashMap<u32, GepFoldInfo>) {
    match inst {
        Instruction::Alloca { .. } => {
            // Space already allocated in prologue; does not touch registers
        }
        Instruction::Copy { dest, src } => {
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

        Instruction::Load { dest, ptr, ty } => {
            // Check if the ptr comes from a foldable GEP with constant offset.
            // Only fold when the GEP's base is an alloca (Direct/OverAligned), where
            // the slot address is always valid. For Indirect bases, the GEP was NOT
            // skipped and already computed the address, so use normal load.
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
                Instruction::Store { val, ptr, ty } => {
                    // Check if the ptr comes from a foldable GEP with constant offset.
                    // Only fold for alloca bases (see Load comment above).
                    if let Some(gep_info) = gep_fold_map.get(&ptr.0) {
                        if !is_i128_type(*ty) && cg.state_ref().is_alloca(gep_info.base.0) {
                            cg.emit_store_with_const_offset(val, &gep_info.base, gep_info.offset, *ty);
                        } else {
                            cg.emit_store(val, ptr, *ty);
                        }
                    } else {
                        cg.emit_store(val, ptr, *ty);
                    }
                }
                Instruction::Call { dest, func, args, arg_types, return_type, is_variadic, num_fixed_args, struct_arg_sizes } =>
                    cg.emit_call(args, arg_types, Some(func), None, *dest, *return_type, *is_variadic, *num_fixed_args, struct_arg_sizes),
                Instruction::CallIndirect { dest, func_ptr, args, arg_types, return_type, is_variadic, num_fixed_args, struct_arg_sizes } =>
                    cg.emit_call(args, arg_types, None, Some(func_ptr), *dest, *return_type, *is_variadic, *num_fixed_args, struct_arg_sizes),
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
                Instruction::InlineAsm { template, outputs, inputs, clobbers, operand_types, goto_labels, input_symbols } =>
                    cg.emit_inline_asm(template, outputs, inputs, clobbers, operand_types, goto_labels, input_symbols),
                Instruction::Intrinsic { dest, op, dest_ptr, args } => cg.emit_intrinsic(dest, op, dest_ptr, args),
                Instruction::Alloca { .. } | Instruction::Copy { .. }
                | Instruction::Load { .. } | Instruction::BinOp { .. }
                | Instruction::UnaryOp { .. } | Instruction::Cmp { .. }
                | Instruction::Cast { .. } | Instruction::GetElementPtr { .. }
                | Instruction::GlobalAddr { .. } | Instruction::LabelAddr { .. } => unreachable!(),
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
            let label_str = label.as_label();
            cg.emit_branch(&label_str);
        }
        Terminator::CondBranch { cond, true_label, false_label } => {
            let true_str = true_label.as_label();
            let false_str = false_label.as_label();
            cg.emit_cond_branch(cond, &true_str, &false_str);
        }
        Terminator::IndirectBranch { target, .. } => {
            cg.emit_indirect_branch(target);
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
) -> i64 {
    let mut space = initial_offset;
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::Alloca { dest, size, ty, align, .. } = inst {
                let effective_align = *align;
                let extra = if effective_align > 16 { effective_align - 1 } else { 0 };
                let raw_size = if *size == 0 { 8 } else { *size as i64 };
                let (slot, new_space) = assign_slot(space, raw_size + extra as i64, *align as i64);
                state.value_locations.insert(dest.0, StackSlot(slot));
                state.alloca_values.insert(dest.0);
                state.alloca_types.insert(dest.0, *ty);
                if effective_align > 16 {
                    state.alloca_alignments.insert(dest.0, effective_align);
                }
                space = new_space;
            } else if let Some(dest) = inst.dest() {
                let is_i128 = matches!(inst.result_type(), Some(IrType::I128) | Some(IrType::U128));
                let slot_size = if is_i128 { 16 } else { 8 };
                let (slot, new_space) = assign_slot(space, slot_size, 0);
                state.value_locations.insert(dest.0, StackSlot(slot));
                if is_i128 {
                    state.i128_values.insert(dest.0);
                }
                space = new_space;
            }
        }
    }
    space
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
