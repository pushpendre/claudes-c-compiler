//! x86-64 peephole optimizer: optimization passes.
//!
//! Contains the main entry point (`peephole_optimize`) and all optimization
//! passes: local pattern matching, push/pop elimination, compare-and-branch
//! fusion, dead store elimination, global store forwarding, and unused
//! callee-save register elimination.

use super::types::*;

// ── Constants ────────────────────────────────────────────────────────────────

/// Maximum iterations for Phase 1 (local peephole passes).
/// Local patterns rarely chain deeper than 3-4 levels, so 8 provides ample headroom.
const MAX_LOCAL_PASS_ITERATIONS: usize = 8;

/// Maximum iterations for Phase 3 (local cleanup after global passes).
/// Post-global cleanup is shallow (mostly dead store + adjacent pairs), so 4 suffices.
const MAX_POST_GLOBAL_ITERATIONS: usize = 4;

/// Maximum number of store/load offsets tracked during compare-and-branch fusion.
/// In practice, at most 2 stores appear between setCC and testq (the Cmp result
/// store + possibly a sign-extension store). 4 provides a comfortable margin;
/// if exceeded, fusion conservatively bails out.
const MAX_TRACKED_STORE_LOAD_OFFSETS: usize = 4;

/// Size of the instruction lookahead window for compare-and-branch fusion.
/// Collects up to this many non-NOP instructions (including the cmp itself)
/// to search for the setCC → store/load → test → branch pattern.
const CMP_FUSION_LOOKAHEAD: usize = 8;

// ── Helpers ───────────────────────────────────────────────────────────────────

/// Check if a register ID refers to a valid general-purpose register (0..=15).
#[inline]
fn is_valid_gp_reg(reg: RegId) -> bool {
    reg != REG_NONE && reg <= REG_GP_MAX
}

// ── Main entry point ─────────────────────────────────────────────────────────

/// Run peephole optimization on x86-64 assembly text.
/// Returns the optimized assembly string.
///
/// Pass structure for speed:
/// 1. Run cheap local passes iteratively until convergence (max `MAX_LOCAL_PASS_ITERATIONS`).
///    These are O(n) single-scan passes that only look at adjacent/nearby lines.
/// 2. Run expensive global passes once. `global_store_forwarding` is O(n) but with
///    higher constant factor due to tracking slot→register mappings. It subsumes
///    the functionality of local store-load forwarding across wider windows.
/// 3. Run local passes one more time to clean up opportunities exposed by the
///    global passes (max `MAX_POST_GLOBAL_ITERATIONS` iterations).
pub fn peephole_optimize(asm: String) -> String {
    let mut store = LineStore::new(asm);
    let line_count = store.len();
    let mut infos: Vec<LineInfo> = (0..line_count).map(|i| classify_line(store.get(i))).collect();

    // Phase 1: Iterative cheap local passes.
    // Run combined_local_pass first; if it makes changes, also run push/pop passes.
    // The push/pop passes are only useful when the combined pass has removed or
    // modified instructions, exposing new push/pop pair opportunities.
    let mut changed = true;
    let mut pass_count = 0;
    while changed && pass_count < MAX_LOCAL_PASS_ITERATIONS {
        changed = false;
        let local_changed = combined_local_pass(&mut store, &mut infos);
        changed |= local_changed;
        // Fuse movq %REG, %rax + movl %eax, %eax -> movl %REGd, %eax
        changed |= fuse_movq_ext_truncation(&mut store, &mut infos);
        // Only run push/pop passes if local pass made changes or this is the first iteration
        // (first iteration always runs all passes to catch initial opportunities)
        if local_changed || pass_count == 0 {
            changed |= eliminate_push_pop_pairs(&store, &mut infos);
            changed |= eliminate_binop_push_pop_pattern(&mut store, &mut infos);
        }
        pass_count += 1;
    }

    // Phase 2: Expensive global passes (run once)
    // Store forwarding and dead store elimination run BEFORE compare-and-branch
    // fusion so that dead store/load pairs are cleaned up first. This lets
    // fuse_compare_and_branch correctly detect stores that are truly live
    // (needed by another basic block) vs. dead stores left over from local
    // store/load elimination.
    let global_changed = global_store_forwarding(&mut store, &mut infos);
    // Register copy propagation runs after store forwarding has converted
    // stack store/load pairs into register moves (e.g., movq %rax, %rcx).
    // It folds these moves into subsequent memory operands to eliminate
    // intermediate register copies.
    let global_changed = global_changed | propagate_register_copies(&mut store, &mut infos);
    let global_changed = global_changed | eliminate_dead_stores(&store, &mut infos);
    let global_changed = global_changed | fuse_compare_and_branch(&mut store, &mut infos);

    // Phase 3: One more local cleanup if global passes made changes.
    if global_changed {
        let mut changed2 = true;
        let mut pass_count2 = 0;
        while changed2 && pass_count2 < MAX_POST_GLOBAL_ITERATIONS {
            changed2 = false;
            changed2 |= combined_local_pass(&mut store, &mut infos);
            changed2 |= fuse_movq_ext_truncation(&mut store, &mut infos);
            changed2 |= eliminate_dead_stores(&store, &mut infos);
            pass_count2 += 1;
        }
    }

    // Phase 4: Eliminate loop backedge trampoline blocks.
    // SSA codegen creates "trampoline" blocks for loop back-edges to resolve phi
    // nodes. These blocks contain only register shuffles (movq %regA, %regB) and
    // a final jmp back to the loop header. This pass detects such trampolines and
    // coalesces the register copies by rewriting the loop body to update registers
    // in-place, then redirects the conditional branch directly to the loop header.
    let trampoline_changed = eliminate_loop_trampolines(&mut store, &mut infos);

    // Phase 4b: If trampoline elimination made changes, do another round of local
    // cleanup (the coalescing may expose self-moves and dead stores).
    if trampoline_changed {
        let mut changed3 = true;
        let mut pass_count3 = 0;
        while changed3 && pass_count3 < MAX_POST_GLOBAL_ITERATIONS {
            changed3 = false;
            changed3 |= combined_local_pass(&mut store, &mut infos);
            changed3 |= fuse_movq_ext_truncation(&mut store, &mut infos);
            changed3 |= eliminate_dead_stores(&store, &mut infos);
            pass_count3 += 1;
        }
    }

    // Phase 5: Global dead store elimination for never-read stack slots.
    // After global_store_forwarding converts loads to register-to-register moves
    // or NOPs, many stores to stack slots become dead because the slot is never
    // loaded from anywhere in the function. This pass scans each function to find
    // stack slots that are never read and eliminates stores to them.
    eliminate_never_read_stores(&store, &mut infos);

    // Phase 6: Eliminate unused callee-saved register saves/restores.
    // After all optimization passes, some callee-saved registers that were
    // allocated by the register allocator may have had all their body uses
    // eliminated by earlier peephole passes. Detect these and remove the
    // now-unnecessary prologue save and epilogue restore instructions.
    // Note: the stack frame size is preserved even when saves are eliminated;
    // see comment in eliminate_unused_callee_saves for rationale.
    eliminate_unused_callee_saves(&store, &mut infos);

    store.build_result(&infos)
}

// ── Unused callee-saved register elimination ─────────────────────────────────
//
// After peephole optimization, some callee-saved registers may no longer be
// referenced in the function body (all uses were optimized away). This pass
// detects such registers and removes their prologue save / epilogue restore
// instructions. The stack frame is not shrunk (see rationale inside function).

/// Check if a register family ID is callee-saved (rbx=3, r12=12, r13=13, r14=14, r15=15).
fn is_callee_saved_reg(reg: RegId) -> bool {
    matches!(reg, 3 | 12 | 13 | 14 | 15)
}

/// Check if a line of assembly text references a given register family.
/// Uses the pre-computed reg_refs bitmask for O(1) lookup.
#[inline]
fn line_references_reg_fast(info: &LineInfo, reg: RegId) -> bool {
    info.reg_refs & (1u16 << reg) != 0
}

fn eliminate_unused_callee_saves(store: &LineStore, infos: &mut [LineInfo]) {
    let len = store.len();
    if len == 0 {
        return;
    }

    // Find function boundaries by scanning for function labels.
    // A function starts at a .globl or .type directive followed by a label,
    // or more simply, we look for the prologue pattern:
    //   pushq %rbp
    //   movq %rsp, %rbp
    //   subq $N, %rsp
    // We process each such function independently.

    let mut i = 0;
    while i < len {
        // Look for the prologue: pushq %rbp
        if infos[i].is_nop() {
            i += 1;
            continue;
        }
        if !matches!(infos[i].kind, LineKind::Push { reg: 5 }) {
            // Not pushq %rbp
            i += 1;
            continue;
        }

        // Next non-nop should be "movq %rsp, %rbp"
        let mut j = next_non_nop(infos, i + 1, len);
        if j >= len {
            i = j;
            continue;
        }
        // Check it's "movq %rsp, %rbp"
        let mov_rbp_line = infos[j].trimmed(store.get(j));
        if mov_rbp_line != "movq %rsp, %rbp" {
            i = j + 1;
            continue;
        }
        j += 1;

        // Next non-nop should be "subq $N, %rsp"
        j = next_non_nop(infos, j, len);
        if j >= len {
            i = j;
            continue;
        }
        let subq_line = infos[j].trimmed(store.get(j));
        // Validate this is a "subq $N, %rsp" instruction (prologue frame allocation).
        if let Some(rest) = subq_line.strip_prefix("subq $") {
            if let Some(val_str) = rest.strip_suffix(", %rsp") {
                if val_str.parse::<i64>().is_err() {
                    i = j + 1;
                    continue;
                }
            } else {
                i = j + 1;
                continue;
            }
        } else {
            // No subq: no callee saves possible in this pattern
            i = j + 1;
            continue;
        }
        j += 1;

        // Collect callee-saved register saves immediately after subq.
        // Pattern: movq %REG, OFFSET(%rbp) where REG is callee-saved.
        struct CalleeSave {
            reg: RegId,
            offset: i32,
            save_line_idx: usize,
        }
        let mut saves: Vec<CalleeSave> = Vec::new();
        j = next_non_nop(infos, j, len);
        while j < len {
            if infos[j].is_nop() {
                j += 1;
                continue;
            }
            if let LineKind::StoreRbp { reg, offset, size: MoveSize::Q } = infos[j].kind {
                if is_callee_saved_reg(reg) && offset < 0 {
                    saves.push(CalleeSave {
                        reg,
                        offset,
                        save_line_idx: j,
                    });
                    j += 1;
                    continue;
                }
            }
            break;
        }

        if saves.is_empty() {
            i = j;
            continue;
        }

        // Find the end of this function by looking for the .size directive.
        let body_start = j;
        let mut func_end = len;
        for k in body_start..len {
            if infos[k].is_nop() {
                continue;
            }
            // .size directive ends a function
            let line = infos[k].trimmed(store.get(k));
            if line.starts_with(".size ") {
                func_end = k + 1;
                break;
            }
        }

        // For each callee-saved register, check if it's referenced in the body
        // (between saves and function end), excluding the save/restore lines themselves.
        // Collect restore line indices too.
        for save in &saves {
            let reg = save.reg;

            // Find all restore lines for this register (LoadRbp with same reg and offset)
            let mut restore_indices: Vec<usize> = Vec::new();
            let mut body_has_reference = false;

            for k in body_start..func_end {
                if infos[k].is_nop() {
                    continue;
                }

                // Check if this is a restore of the same register at the same offset
                if let LineKind::LoadRbp { reg: load_reg, offset: load_offset, size: MoveSize::Q } = infos[k].kind {
                    if load_reg == reg && load_offset == save.offset {
                        // Check if this is in an epilogue context
                        if is_near_epilogue(infos, k) {
                            restore_indices.push(k);
                            continue;
                        }
                    }
                }

                // Check if the line references this register family using the
                // pre-computed reg_refs bitmask for O(1) lookup.
                if line_references_reg_fast(&infos[k], reg) {
                    body_has_reference = true;
                    break;
                }
            }

            if !body_has_reference && !restore_indices.is_empty() {
                // This callee-saved register is unused in the body.
                // NOP out the save and all restores.
                mark_nop(&mut infos[save.save_line_idx]);
                for &ri in &restore_indices {
                    mark_nop(&mut infos[ri]);
                }
            }
        }

        // Note: we intentionally do NOT shrink the stack frame (subq $N, %rsp)
        // even though some callee-saved saves were eliminated. The remaining saves
        // still reference their original rbp-relative offsets, which are below rsp
        // if we shrink the frame. Data below rsp can be corrupted by interrupts
        // or signal handlers. Keeping the original frame size ensures all saved
        // registers remain safely above rsp. The unused slots become dead space,
        // which wastes a few bytes but is safe.
        // TODO: To also shrink the frame, we would need to rewrite the offsets of
        // all remaining callee-saved saves/restores to pack them tightly.

        i = func_end;
    }
}

// ── Combined local pass ───────────────────────────────────────────────────────
//
// Merges 5 simple local passes into a single linear scan to avoid redundant
// iteration over the lines array. Each of the original passes did a full O(n)
// scan; by combining them we do one scan that checks all patterns at each line.
//
// Merged passes:
//   1. eliminate_adjacent_store_load: store/load at same %rbp offset
//   2. eliminate_redundant_jumps: jmp to the immediately following label
//   3. eliminate_redundant_movq_self: movq %reg, %reg (same src/dst)
//   4. eliminate_redundant_cltq: cltq after movslq/movq$ to %rax
//   5. eliminate_redundant_zero_extend: redundant zero/sign extensions

fn combined_local_pass(store: &mut LineStore, infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = store.len();

    let mut i = 0;
    while i < len {
        if infos[i].is_nop() {
            i += 1;
            continue;
        }

        // --- Pattern: self-move elimination (movq %reg, %reg) ---
        // Pre-classified as SelfMove during classify_line, avoiding string parsing.
        if infos[i].kind == LineKind::SelfMove {
            mark_nop(&mut infos[i]);
            changed = true;
            i += 1;
            continue;
        }

        // --- Pattern: reverse-move elimination ---
        // Detects `movq %regA, %regB` followed by `movq %regB, %regA` and
        // eliminates the second mov (since %regA still holds the original value).
        //
        // This pattern arises from the accumulator-centric codegen: a value in
        // %rax is saved to a callee-saved register, then immediately loaded back
        // to %rax for the next operation. Example:
        //   movq %rax, %rbx    ; save to callee-saved reg
        //   movq %rbx, %rax    ; load back (REDUNDANT)
        //
        // Safety: We only skip NOPs and StoreRbp between the two instructions.
        // StoreRbp reads registers but never modifies any GP register value.
        // Any other instruction type (call, label, jmp, arithmetic, etc.) causes
        // the search to stop via `break`, so we never incorrectly eliminate a
        // reverse move when an intervening instruction could have modified %regA.
        if let LineKind::Other { dest_reg: dest_a } = infos[i].kind {
            if is_valid_gp_reg(dest_a) {
                let line_i = infos[i].trimmed(store.get(i));
                // Parse "movq %srcReg, %dstReg" pattern
                if let Some(rest) = line_i.strip_prefix("movq ") {
                    if let Some((src_str, dst_str)) = rest.split_once(',') {
                        let src = src_str.trim();
                        let dst = dst_str.trim();
                        let src_fam = register_family_fast(src);
                        let dst_fam = register_family_fast(dst);
                        // Both must be GP registers, different families, both register operands
                        // (same-reg case is handled by SelfMove above)
                        if is_valid_gp_reg(src_fam) && is_valid_gp_reg(dst_fam)
                            && src_fam != dst_fam
                            && src.starts_with('%') && dst.starts_with('%')
                        {
                            // Find the next non-NOP, non-StoreRbp instruction.
                            // Limit search to 8 lines to avoid pathological scanning.
                            let mut j = i + 1;
                            let search_limit = (i + 8).min(len);
                            while j < search_limit {
                                if infos[j].is_nop() {
                                    j += 1;
                                    continue;
                                }
                                if matches!(infos[j].kind, LineKind::StoreRbp { .. }) {
                                    j += 1;
                                    continue;
                                }
                                break;
                            }
                            if j < search_limit {
                                // Check if line j is the reverse: movq %dstReg, %srcReg
                                if let LineKind::Other { dest_reg: dest_b } = infos[j].kind {
                                    if dest_b == src_fam {
                                        let line_j = infos[j].trimmed(store.get(j));
                                        if let Some(rest_j) = line_j.strip_prefix("movq ") {
                                            if let Some((src_j, dst_j)) = rest_j.split_once(',') {
                                                let src_j = src_j.trim();
                                                let dst_j = dst_j.trim();
                                                let src_j_fam = register_family_fast(src_j);
                                                let dst_j_fam = register_family_fast(dst_j);
                                                if src_j_fam == dst_fam && dst_j_fam == src_fam {
                                                    mark_nop(&mut infos[j]);
                                                    changed = true;
                                                    i += 1;
                                                    continue;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // --- Pattern: redundant jump to next label ---
        if infos[i].kind == LineKind::Jmp {
            let jmp_line = infos[i].trimmed(store.get(i));
            if let Some(target) = jmp_line.strip_prefix("jmp ") {
                let target = target.trim();
                // Find the next non-NOP, non-empty line
                let mut found_redundant = false;
                for j in (i + 1)..len {
                    if infos[j].is_nop() || infos[j].kind == LineKind::Empty {
                        continue;
                    }
                    if infos[j].kind == LineKind::Label {
                        let next = infos[j].trimmed(store.get(j));
                        if let Some(label) = next.strip_suffix(':') {
                            if label == target {
                                mark_nop(&mut infos[i]);
                                changed = true;
                                found_redundant = true;
                            }
                        }
                    }
                    break;
                }
                if found_redundant {
                    i += 1;
                    continue;
                }
            }
        }

        // --- Pattern: conditional branch inversion for fall-through ---
        // Detects:
        //   jCC .Ltrue        (conditional jump)
        //   jmp .Lfalse       (unconditional jump)
        //   .Ltrue:           (label matching the conditional target)
        //
        // Transforms to:
        //   j!CC .Lfalse      (inverted condition, jump to false target)
        //   .Ltrue:           (fall through naturally)
        //
        // This eliminates one instruction per conditional branch where the true
        // target is the fall-through block. Common in loop headers and if-else.
        if infos[i].kind == LineKind::CondJmp {
            let cond_line = infos[i].trimmed(store.get(i));
            // Parse: "jCC target" -> extract CC and target
            if let Some(space_pos) = cond_line.find(' ') {
                let cc = &cond_line[1..space_pos]; // e.g., "l", "ge", "ne"
                let cond_target = cond_line[space_pos + 1..].trim();
                // Find the next non-NOP line (should be jmp)
                let mut j = i + 1;
                while j < len && infos[j].is_nop() {
                    j += 1;
                }
                if j < len && infos[j].kind == LineKind::Jmp {
                    let jmp_line = infos[j].trimmed(store.get(j));
                    if let Some(jmp_target) = jmp_line.strip_prefix("jmp ") {
                        let jmp_target = jmp_target.trim();
                        // Find the next non-NOP/non-empty line after jmp (should be a label)
                        let mut k = j + 1;
                        while k < len && (infos[k].is_nop() || infos[k].kind == LineKind::Empty) {
                            k += 1;
                        }
                        if k < len && infos[k].kind == LineKind::Label {
                            let label_line = infos[k].trimmed(store.get(k));
                            if let Some(label_name) = label_line.strip_suffix(':') {
                                if label_name == cond_target {
                                    // The conditional target is the fall-through label.
                                    // Invert the condition and jump to the jmp's target instead.
                                    let inv_cc = invert_cc(cc);
                                    // Only apply if inversion actually changed the CC
                                    // (invert_cc returns the same string if unknown)
                                    if inv_cc != cc {
                                        let new_line = format!("    j{} {}", inv_cc, jmp_target);
                                        replace_line(store, &mut infos[i], i, new_line);
                                        mark_nop(&mut infos[j]); // Remove the jmp
                                        changed = true;
                                        i += 1;
                                        continue;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // --- Pattern: adjacent store/load at same %rbp offset ---
        // Note: this pattern does NOT replace lines, it only NOPs or delegates
        // to the store-forwarding global pass for reg-to-reg moves.
        if let LineKind::StoreRbp { reg: sr, offset: so, size: ss } = infos[i].kind {
            if i + 1 < len && !infos[i + 1].is_nop() {
                if let LineKind::LoadRbp { reg: lr, offset: lo, size: ls } = infos[i + 1].kind {
                    if so == lo && ss == ls {
                        // Guard: only optimize when both registers are recognized.
                        // REG_NONE means the register is not a known GP/MMX/XMM family
                        // (e.g., unrecognized register names). Treating two REG_NONE
                        // values as "same register" would incorrectly eliminate loads
                        // between different non-GP registers (e.g., %mm0 store then
                        // %mm2 load at same offset).
                        if sr == lr && sr != REG_NONE {
                            // Same register: load is redundant
                            mark_nop(&mut infos[i + 1]);
                            changed = true;
                            i += 1;
                            continue;
                        }
                        // Different register cases are handled by global_store_forwarding
                    }
                }
            }
        }

        // --- Pattern: redundant zero/sign extension (including cltq) ---
        // Uses pre-classified ExtKind to avoid repeated starts_with/ends_with
        // string comparisons on every iteration.
        //
        // Find the next non-NOP instruction, skipping stores to rbp (which don't
        // modify registers we care about for extension redundancy).
        let mut ext_idx = i + 1;
        while ext_idx < len && ext_idx < i + 10 {
            if infos[ext_idx].is_nop() {
                ext_idx += 1;
                continue;
            }
            if matches!(infos[ext_idx].kind, LineKind::StoreRbp { .. }) {
                ext_idx += 1;
                continue;
            }
            break;
        }

        if ext_idx < len && !infos[ext_idx].is_nop() {
            let next_ext = infos[ext_idx].ext_kind;
            let prev_ext = infos[i].ext_kind;

            // Use pre-classified ExtKind for fast matching.
            // Note: Some ExtKind values serve dual roles (both consumer and producer).
            // For example, MovslqEaxRax is a consumer (after another movslq-to-rax) AND
            // a producer (for subsequent cltq, since both sign-extend eax to rax).
            // Similarly, MovzbqAlRax/MovzwqAxRax/MovsbqAlRax can act as producers for
            // a subsequent identical extension.
            let is_redundant_ext = match next_ext {
                ExtKind::MovzbqAlRax => matches!(prev_ext, ExtKind::ProducerMovzbqToRax | ExtKind::MovzbqAlRax),
                ExtKind::MovzwqAxRax => matches!(prev_ext, ExtKind::ProducerMovzwqToRax | ExtKind::MovzwqAxRax),
                ExtKind::MovsbqAlRax => matches!(prev_ext, ExtKind::ProducerMovsbqToRax | ExtKind::MovsbqAlRax),
                ExtKind::MovslqEaxRax => matches!(prev_ext, ExtKind::ProducerMovslqToRax | ExtKind::MovslqEaxRax),
                ExtKind::Cltq => matches!(prev_ext,
                    ExtKind::ProducerMovslqToRax | ExtKind::ProducerMovqConstRax |
                    ExtKind::MovslqEaxRax | ExtKind::Cltq),
                ExtKind::MovlEaxEax => matches!(prev_ext,
                    ExtKind::ProducerArith32 | ExtKind::ProducerMovlToEax |
                    ExtKind::ProducerMovzbToEax | ExtKind::ProducerMovzbqToRax |
                    ExtKind::ProducerMovzwToEax | ExtKind::ProducerMovzwqToRax |
                    ExtKind::ProducerDiv32 |
                    ExtKind::MovlEaxEax),
                _ => false,
            };

            if is_redundant_ext {
                mark_nop(&mut infos[ext_idx]);
                changed = true;
                i += 1;
                continue;
            }

            // --- Extended scan: cltq past non-rax-clobbering instructions ---
            //
            // When the consumer is cltq but the immediately preceding instruction
            // is not a recognized sign-extension producer, scan backward past
            // instructions that don't write to %rax to find the actual producer.
            //
            // Common pattern from the accumulator-based codegen:
            //   cltq / movslq ..., %rax     ; sign-extend %rax (producer)
            //   movq %rax, %r8              ; save to callee-saved (doesn't modify %rax)
            //   cltq                        ; REDUNDANT - %rax still sign-extended
            //
            // Safety: we only scan past instructions whose dest_reg != 0 (rax family)
            // and that are not barriers (labels, calls, jumps, etc.). Calls clobber
            // caller-saved registers including %rax, so they stop the scan. StoreRbp
            // and NOPs are also safe to skip (they don't modify any register).
            //
            // Important: we first check that instruction[i] itself doesn't write to
            // %rax. If it does (e.g., addl %ecx, %eax), the cltq is NOT redundant
            // even if a sign-extension producer exists further back.
            if next_ext == ExtKind::Cltq && !is_redundant_ext {
                // First check: does instruction[i] itself write to %rax?
                // If so, any earlier sign-extension is invalidated.
                let i_writes_rax = match infos[i].kind {
                    LineKind::Other { dest_reg } => dest_reg == 0,
                    LineKind::LoadRbp { reg, .. } => reg == 0,
                    LineKind::StoreRbp { .. } => false,
                    LineKind::Nop | LineKind::Empty => false,
                    _ => true, // conservative: barriers, calls, etc. may write rax
                };

                if !i_writes_rax && i > 0 {
                    let mut found_producer = false;
                    let scan_limit = if i >= 6 { i - 6 } else { 0 };
                    let mut k = i - 1;
                    while k >= scan_limit {
                        if infos[k].is_nop() {
                            if k == 0 { break; }
                            k -= 1;
                            continue;
                        }
                        if matches!(infos[k].kind, LineKind::StoreRbp { .. }) {
                            if k == 0 { break; }
                            k -= 1;
                            continue;
                        }
                        // Stop at barriers (labels, calls, jumps, ret)
                        if infos[k].is_barrier() {
                            break;
                        }
                        // Check if this instruction is a sign-extension producer for rax
                        let k_ext = infos[k].ext_kind;
                        if matches!(k_ext,
                            ExtKind::ProducerMovslqToRax | ExtKind::ProducerMovqConstRax |
                            ExtKind::MovslqEaxRax | ExtKind::Cltq)
                        {
                            found_producer = true;
                            break;
                        }
                        // Check if this instruction writes to %rax (family 0)
                        // If it does, %rax may no longer be sign-extended, so stop.
                        let writes_rax = match infos[k].kind {
                            LineKind::Other { dest_reg } => dest_reg == 0,
                            LineKind::LoadRbp { reg, .. } => reg == 0,
                            _ => true, // conservative: treat unknown as writing rax
                        };
                        if writes_rax {
                            break;
                        }
                        // This instruction doesn't write rax - safe to skip past
                        if k == 0 { break; }
                        k -= 1;
                    }
                    if found_producer {
                        mark_nop(&mut infos[ext_idx]);
                        changed = true;
                        i += 1;
                        continue;
                    }
                }
            }
        }

        i += 1;
    }
    changed
}

// ── Movq + extension/truncation fusion ───────────────────────────────────────
//
// Fuses `movq %REG, %rax` followed by a cast instruction into a single
// instruction. The two-instruction pattern arises from the accumulator-based
// codegen model: emit_load_operand loads a 64-bit value into %rax, then
// emit_cast_instrs emits an extension/truncation on %rax/%eax/%ax/%al.
//
// Fused patterns (all require REG != rax, no intervening non-NOP instructions):
//   movq %REG, %rax + movl %eax, %eax   -> movl %REGd, %eax    (truncate to u32)
//   movq %REG, %rax + movslq %eax, %rax -> movslq %REGd, %rax  (sign-extend i32->i64)
//   movq %REG, %rax + cltq              -> movslq %REGd, %rax   (sign-extend i32->i64)
//   movq %REG, %rax + movzbq %al, %rax  -> movzbl %REGb, %eax  (zero-extend u8->i64)
//   movq %REG, %rax + movzwq %ax, %rax  -> movzwl %REGw, %eax  (zero-extend u16->i64)
//   movq %REG, %rax + movsbq %al, %rax  -> movsbq %REGb, %rax  (sign-extend i8->i64)
//
// Safety: the two instructions must be truly adjacent (only NOPs between them,
// NOT stores) because intermediate code may read the full 64-bit %rax value
// before the truncation.

fn fuse_movq_ext_truncation(store: &mut LineStore, infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = store.len();

    let mut i = 0;
    while i + 1 < len {
        // Look for ProducerMovqRegToRax
        if infos[i].ext_kind != ExtKind::ProducerMovqRegToRax {
            i += 1;
            continue;
        }

        // Find next non-NOP instruction (skip only NOPs, not stores)
        let mut j = i + 1;
        while j < len && infos[j].is_nop() {
            j += 1;
        }
        if j >= len {
            i += 1;
            continue;
        }

        // Check if next instruction is a fusable extension/truncation on %rax
        let next_ext = infos[j].ext_kind;
        let fusable = matches!(next_ext,
            ExtKind::MovlEaxEax | ExtKind::MovslqEaxRax | ExtKind::Cltq |
            ExtKind::MovzbqAlRax | ExtKind::MovzwqAxRax |
            ExtKind::MovsbqAlRax);
        if !fusable {
            i += 1;
            continue;
        }

        // Extract source register family from the movq instruction
        let movq_line = infos[i].trimmed(store.get(i));
        let src_family = if let Some(rest) = movq_line.strip_prefix("movq ") {
            if let Some((src, _dst)) = rest.split_once(',') {
                let src = src.trim();
                let fam = register_family_fast(src);
                if fam != REG_NONE && fam != 0 { fam } else { REG_NONE }
            } else { REG_NONE }
        } else { REG_NONE };

        if src_family == REG_NONE {
            i += 1;
            continue;
        }

        // Build the fused instruction based on the extension type
        let new_text = match next_ext {
            ExtKind::MovlEaxEax => {
                // movq %REG, %rax + movl %eax, %eax -> movl %REGd, %eax
                let src_32 = REG_NAMES[1][src_family as usize];
                format!("    movl {}, %eax", src_32)
            }
            ExtKind::MovslqEaxRax | ExtKind::Cltq => {
                // movq %REG, %rax + movslq/cltq -> movslq %REGd, %rax
                let src_32 = REG_NAMES[1][src_family as usize];
                format!("    movslq {}, %rax", src_32)
            }
            ExtKind::MovzbqAlRax => {
                // movq %REG, %rax + movzbq %al, %rax -> movzbl %REGb, %eax
                let src_8 = REG_NAMES[3][src_family as usize];
                format!("    movzbl {}, %eax", src_8)
            }
            ExtKind::MovzwqAxRax => {
                // movq %REG, %rax + movzwq %ax, %rax -> movzwl %REGw, %eax
                let src_16 = REG_NAMES[2][src_family as usize];
                format!("    movzwl {}, %eax", src_16)
            }
            ExtKind::MovsbqAlRax => {
                // movq %REG, %rax + movsbq %al, %rax -> movsbq %REGb, %rax
                let src_8 = REG_NAMES[3][src_family as usize];
                format!("    movsbq {}, %rax", src_8)
            }
            _ => unreachable!("mov+ext fusion matched unexpected ExtKind"),
        };

        replace_line(store, &mut infos[i], i, new_text);
        mark_nop(&mut infos[j]);
        changed = true;
        i = j + 1;
        continue;
    }
    changed
}

// ── Push/pop pair elimination ─────────────────────────────────────────────────

fn eliminate_push_pop_pairs(store: &LineStore, infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = store.len();

    for i in 0..len.saturating_sub(2) {
        let push_reg_id = match infos[i].kind {
            LineKind::Push { reg } if reg != REG_NONE => reg,
            _ => continue,
        };

        for j in (i + 1)..std::cmp::min(i + 4, len) {
            if infos[j].is_nop() {
                continue;
            }
            if let LineKind::Pop { reg: pop_reg_id } = infos[j].kind {
                if pop_reg_id == push_reg_id {
                    let mut safe = true;
                    for k in (i + 1)..j {
                        if infos[k].is_nop() {
                            continue;
                        }
                        if instruction_modifies_reg_id(&infos[k], push_reg_id) {
                            safe = false;
                            break;
                        }
                    }
                    if safe {
                        mark_nop(&mut infos[i]);
                        mark_nop(&mut infos[j]);
                        changed = true;
                    }
                }
                break;
            }
            if infos[j].is_push() {
                break;
            }
            if matches!(infos[j].kind, LineKind::Call | LineKind::Jmp | LineKind::JmpIndirect | LineKind::Ret) {
                break;
            }
        }
    }
    changed
}

/// Fast check whether a line's instruction modifies a register identified by RegId.
/// Uses pre-parsed `LineInfo` fields to avoid string parsing in the hot path.
#[inline]
fn instruction_modifies_reg_id(info: &LineInfo, reg_id: RegId) -> bool {
    match info.kind {
        LineKind::StoreRbp { .. } | LineKind::Cmp | LineKind::Nop | LineKind::Empty
        | LineKind::Label | LineKind::Directive | LineKind::Jmp | LineKind::JmpIndirect
        | LineKind::CondJmp | LineKind::SelfMove => false,
        LineKind::LoadRbp { reg, .. } => reg == reg_id,
        LineKind::Pop { reg } => reg == reg_id,
        LineKind::Push { .. } => false, // push reads, doesn't modify the source reg
        LineKind::SetCC { reg } => reg_id == reg, // setCC writes to the byte register's family
        LineKind::Call => matches!(reg_id, 0 | 1 | 2 | 6 | 7 | 8 | 9 | 10 | 11),
        LineKind::Ret => false,
        LineKind::Other { dest_reg } => {
            if dest_reg == reg_id {
                return true;
            }
            // div/idiv also clobber rdx (family 2), and rax (family 0)
            // mul also clobbers rdx
            // The dest_reg for these is already 0 (rax), check rdx separately
            if dest_reg == 0 && reg_id == 2 {
                // The parse_dest_reg_fast sets dest_reg=0 for div/idiv/mul
                // These also clobber rdx. We conservatively say true.
                // This is acceptable because push/pop pairs rarely involve rdx.
                return true; // TODO: could be more precise
            }
            false
        }
    }
}

// ── Binary-op push/pop pattern ────────────────────────────────────────────────

fn eliminate_binop_push_pop_pattern(store: &mut LineStore, infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = store.len();

    let mut i = 0;
    while i + 3 < len {
        let push_reg_id = match infos[i].kind {
            LineKind::Push { reg } if reg != REG_NONE => reg,
            _ => { i += 1; continue; }
        };

        let push_line = infos[i].trimmed(store.get(i));
        let push_reg = match push_line.strip_prefix("pushq ") {
            Some(r) => r.trim(),
            None => { i += 1; continue; }
        };

        // Find next 3 non-NOP lines
        let mut real_indices = [0usize; 3];
        let count = collect_non_nop_indices(infos, i, len, &mut real_indices);

        if count == 3 {
            let load_idx = real_indices[0];
            let move_idx = real_indices[1];
            let pop_idx = real_indices[2];

            if let LineKind::Pop { reg: pop_reg_id } = infos[pop_idx].kind {
                if pop_reg_id == push_reg_id {
                    let load_line = infos[load_idx].trimmed(store.get(load_idx));
                    let move_line = infos[move_idx].trimmed(store.get(move_idx));

                    if let Some(move_target) = parse_reg_to_reg_move(move_line, push_reg) {
                        if instruction_writes_to(load_line, push_reg) && can_redirect_instruction(load_line) {
                            if let Some(new_load) = replace_dest_register(load_line, push_reg, move_target) {
                                mark_nop(&mut infos[i]);
                                let new_text = format!("    {}", new_load);
                                replace_line(store, &mut infos[load_idx], load_idx, new_text);
                                mark_nop(&mut infos[move_idx]);
                                mark_nop(&mut infos[pop_idx]);
                                changed = true;
                                i = pop_idx + 1;
                                continue;
                            }
                        }
                    }
                }
            }
        }

        i += 1;
    }
    changed
}

// ── Compare-and-branch fusion ─────────────────────────────────────────────────

fn fuse_compare_and_branch(store: &mut LineStore, infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = store.len();

    let mut i = 0;
    while i < len {
        if infos[i].kind != LineKind::Cmp {
            i += 1;
            continue;
        }

        // Collect next non-NOP lines: cmp itself + (CMP_FUSION_LOOKAHEAD-1) following
        let mut seq_indices = [0usize; CMP_FUSION_LOOKAHEAD];
        seq_indices[0] = i;
        let mut rest = [0usize; CMP_FUSION_LOOKAHEAD - 1];
        let rest_count = collect_non_nop_indices::<{ CMP_FUSION_LOOKAHEAD - 1 }>(infos, i, len, &mut rest);
        seq_indices[1..(rest_count + 1)].copy_from_slice(&rest[..rest_count]);
        let seq_count = 1 + rest_count;

        if seq_count < 4 {
            i += 1;
            continue;
        }

        // Second must be setCC
        if !matches!(infos[seq_indices[1]].kind, LineKind::SetCC { .. }) {
            i += 1;
            continue;
        }
        let set_line = infos[seq_indices[1]].trimmed(store.get(seq_indices[1]));
        let cc = match parse_setcc(set_line) {
            Some(c) => c,
            None => { i += 1; continue; }
        };

        // Scan for testq %rax, %rax pattern.
        // Track StoreRbp offsets so we can bail out if any store's slot is
        // potentially read by another basic block (no matching load nearby).
        let mut test_idx = None;
        let mut store_offsets: [i32; MAX_TRACKED_STORE_LOAD_OFFSETS] = [0; MAX_TRACKED_STORE_LOAD_OFFSETS];
        let mut store_count = 0usize;
        let mut scan = 2;
        while scan < seq_count {
            let si = seq_indices[scan];
            let line = infos[si].trimmed(store.get(si));

            // Skip zero-extend of setcc result
            if line.starts_with("movzbq %al,") || line.starts_with("movzbl %al,") {
                scan += 1;
                continue;
            }
            // Skip store/load to rbp (pre-parsed fast check).
            // Track store offsets to detect unmatched cross-block stores.
            if let LineKind::StoreRbp { offset, .. } = infos[si].kind {
                if store_count < MAX_TRACKED_STORE_LOAD_OFFSETS {
                    store_offsets[store_count] = offset;
                    store_count += 1;
                } else {
                    // Too many stores to track — conservatively bail out
                    store_count = usize::MAX;
                    break;
                }
                scan += 1;
                continue;
            }
            if matches!(infos[si].kind, LineKind::LoadRbp { .. }) {
                scan += 1;
                continue;
            }
            // Skip cltq and movslq
            if line == "cltq" || line.starts_with("movslq ") {
                scan += 1;
                continue;
            }
            // Check for test
            if line == "testq %rax, %rax" || line == "testl %eax, %eax" {
                test_idx = Some(scan);
                break;
            }
            break;
        }

        let test_scan = match test_idx {
            Some(t) => t,
            None => { i += 1; continue; }
        };

        // If there are stores in the sequence, verify each has a matching
        // load nearby (including NOP'd lines from earlier local passes).
        // A store without a matching load means the Cmp result is live
        // beyond this branch (read by another basic block).  Fusion would
        // delete the setCC+movzbq that materialise the boolean, leaving
        // the slot uninitialised or written with the wrong value.
        if store_count == usize::MAX {
            // Overflow: too many stores to track, conservatively skip fusion
            i += 1;
            continue;
        }
        if store_count > 0 {
            let range_start = seq_indices[1];
            let range_end = seq_indices[test_scan];
            let mut load_offsets: [i32; MAX_TRACKED_STORE_LOAD_OFFSETS] = [0; MAX_TRACKED_STORE_LOAD_OFFSETS];
            let mut load_count = 0usize;
            for ri in range_start..=range_end {
                let off = match infos[ri].kind {
                    LineKind::LoadRbp { offset, .. } => Some(offset),
                    LineKind::Nop => {
                        // Re-classify the original text to recover NOP'd loads
                        let orig = classify_line(store.get(ri));
                        match orig.kind {
                            LineKind::LoadRbp { offset, .. } => Some(offset),
                            _ => None,
                        }
                    }
                    _ => None,
                };
                if let Some(o) = off {
                    if load_count < MAX_TRACKED_STORE_LOAD_OFFSETS { load_offsets[load_count] = o; load_count += 1; }
                }
            }
            let has_unmatched_store = (0..store_count).any(|si| {
                !(0..load_count).any(|li| load_offsets[li] == store_offsets[si])
            });
            if has_unmatched_store {
                i += 1;
                continue;
            }
        }

        if test_scan + 1 >= seq_count {
            i += 1;
            continue;
        }

        let jmp_line = infos[seq_indices[test_scan + 1]].trimmed(store.get(seq_indices[test_scan + 1]));
        let (is_jne, branch_target) = if let Some(target) = jmp_line.strip_prefix("jne ") {
            (true, target.trim())
        } else if let Some(target) = jmp_line.strip_prefix("je ") {
            (false, target.trim())
        } else {
            i += 1;
            continue;
        };

        let fused_cc = if is_jne { cc } else { invert_cc(cc) };
        let fused_jcc = format!("    j{} {}", fused_cc, branch_target);

        // NOP out everything from setCC through testq
        for s in 1..=test_scan {
            mark_nop(&mut infos[seq_indices[s]]);
        }
        // Replace the jne/je with the fused conditional jump
        let idx = seq_indices[test_scan + 1];
        replace_line(store, &mut infos[idx], idx, fused_jcc);

        changed = true;
        i = idx + 1;
    }

    changed
}


/// Replace all register-family occurrences of `old_family` with `new_family` in `line`.
/// Handles all register sizes: 64-bit (%rax), 32-bit (%eax), 16-bit (%ax), 8-bit (%al).
/// For example, replacing family 0 (rax) with family 1 (rcx) will convert:
///   %rax -> %rcx, %eax -> %ecx, %ax -> %cx, %al -> %cl
fn replace_reg_family(line: &str, old_id: RegId, new_id: RegId) -> String {
    let mut result = line.to_string();
    // Replace in order from longest to shortest to avoid partial matches.
    // 64-bit names are longest (e.g., %r10, %rax), then 32-bit, 16-bit, 8-bit.
    for size_idx in 0..4 {
        let old_name = REG_NAMES[size_idx][old_id as usize];
        let new_name = REG_NAMES[size_idx][new_id as usize];
        if old_name == new_name {
            continue;
        }
        result = replace_reg_name_exact(&result, old_name, new_name);
    }
    result
}

/// Replace all complete occurrences of `old_reg` with `new_reg` in `line`.
/// A "complete" occurrence means `old_reg` is not a prefix of a longer register
/// name (e.g., replacing `%r8` must not match `%r8d` or `%r8b`).
/// After `old_reg`, the next character must be a delimiter: `,`, `)`, ` `, or end-of-string.
fn replace_reg_name_exact(line: &str, old_reg: &str, new_reg: &str) -> String {
    let mut result = String::with_capacity(line.len());
    let bytes = line.as_bytes();
    let old_bytes = old_reg.as_bytes();
    let old_len = old_bytes.len();
    let mut pos = 0;

    while pos < bytes.len() {
        if pos + old_len <= bytes.len() && &bytes[pos..pos + old_len] == old_bytes {
            // Check that this is a complete register name
            let after = pos + old_len;
            let is_complete = after >= bytes.len()
                || matches!(bytes[after], b',' | b')' | b' ' | b'\t' | b'\n');
            if is_complete {
                result.push_str(new_reg);
                pos += old_len;
                continue;
            }
        }
        result.push(bytes[pos] as char);
        pos += 1;
    }
    result
}

/// Replace register family occurrences only in the source operand part
/// (before the last comma). The destination operand is left unchanged.
fn replace_reg_family_in_source(line: &str, old_id: RegId, new_id: RegId) -> String {
    if let Some(comma_pos) = line.rfind(',') {
        let src_part = &line[..comma_pos];
        let dst_part = &line[comma_pos..];
        let new_src = replace_reg_family(src_part, old_id, new_id);
        format!("{}{}", new_src, dst_part)
    } else {
        replace_reg_family(line, old_id, new_id)
    }
}

// ── Register copy propagation ─────────────────────────────────────────────────
//
// Propagates register-to-register copies across entire basic blocks to
// eliminate intermediate moves. The accumulator-based codegen routes many
// operations through %rax as an intermediary, producing chains like:
//
//   movq %rax, %rcx             # copy rax -> rcx
//   movq %rcx, %rdx             # copy rcx -> rdx (really rax -> rdx)
//   addq %rcx, %r8              # uses rcx (really rax)
//
// After propagation, this becomes:
//
//   movq %rax, %rcx             # potentially dead
//   movq %rax, %rdx             # uses rax directly
//   addq %rax, %r8              # uses rax directly
//
// The dead movq instructions are cleaned up by subsequent passes.
//
// Algorithm: Scan each basic block maintaining a map of active copies
// (copy_src[dst] = src). For each instruction:
//   1. If it's a movq %src, %dst: add copy_src[dst] = src to the map
//   2. For any instruction, replace read-uses of copied registers
//   3. Invalidate copies when src or dst registers are overwritten
//   4. Clear all copies at basic block boundaries
//
// Safety:
//   - Only handles 64-bit GP reg-to-reg moves (movq %src, %dst)
//   - Skips rsp/rbp registers (families 4/5)
//   - Skips instructions with implicit register usage (div, mul, shifts with %cl)
//   - Follows transitive chains: if copy_src[A]=B and copy_src[B]=C, resolves A->C

/// Check if an instruction has implicit register usage that makes register
/// substitution unsafe (div/idiv/mul use rax/rdx, shifts use cl, etc.).
fn has_implicit_reg_usage(trimmed: &str) -> bool {
    let nb = trimmed.as_bytes();
    if nb.len() < 3 {
        return false;
    }
    (nb[0] == b'd' && nb[1] == b'i' && nb[2] == b'v') || // div, divl, divq
    (nb[0] == b'i' && nb[1] == b'd' && nb[2] == b'i') || // idiv
    (nb[0] == b'm' && nb[1] == b'u' && nb[2] == b'l') || // mul, mulq
    trimmed.starts_with("cqto") || trimmed.starts_with("cdq") ||
    trimmed.starts_with("cqo") || trimmed.starts_with("cwd") ||
    trimmed.starts_with("rep ") || trimmed.starts_with("repne ") ||
    trimmed.starts_with("cpuid") || trimmed.starts_with("syscall") ||
    trimmed.starts_with("rdtsc") || trimmed.starts_with("rdmsr") ||
    trimmed.starts_with("wrmsr") ||
    trimmed.starts_with("xchg") || trimmed.starts_with("cmpxchg") ||
    trimmed.starts_with("lock ")
}

/// Check if an instruction is a shift/rotate that implicitly uses %cl.
fn is_shift_or_rotate(trimmed: &str) -> bool {
    let nb = trimmed.as_bytes();
    nb.len() >= 3 && (
        (nb[0] == b's' && (nb[1] == b'h' || nb[1] == b'a')) || // shl, shr, sal, sar
        (nb[0] == b'r' && (nb[1] == b'o' || nb[1] == b'c'))    // rol, ror, rcl, rcr
    )
}

/// Parse a movq %src, %dst instruction. Returns (src_family, dst_family) if valid.
fn parse_reg_to_reg_movq(info: &LineInfo, trimmed: &str) -> Option<(RegId, RegId)> {
    if let LineKind::Other { dest_reg } = info.kind {
        if dest_reg == REG_NONE || dest_reg > REG_GP_MAX {
            return None;
        }
        if let Some(rest) = trimmed.strip_prefix("movq ") {
            if let Some((src_part, dst_part)) = rest.split_once(',') {
                let src = src_part.trim();
                let dst = dst_part.trim();
                if !src.starts_with("%r") || !dst.starts_with("%r") {
                    return None;
                }
                let sfam = register_family_fast(src);
                let dfam = register_family_fast(dst);
                if sfam == REG_NONE || sfam > REG_GP_MAX || sfam == 4 || sfam == 5
                    || dfam == REG_NONE || dfam > REG_GP_MAX || dfam == 4 || dfam == 5
                    || sfam == dfam
                {
                    return None;
                }
                return Some((sfam, dfam));
            }
        }
    }
    None
}

/// Get the destination register of an instruction (the register it writes to).
fn get_dest_reg(info: &LineInfo) -> RegId {
    match info.kind {
        LineKind::Other { dest_reg } => dest_reg,
        LineKind::StoreRbp { .. } => REG_NONE, // stores don't write to a register
        LineKind::LoadRbp { reg, .. } => reg,
        LineKind::SetCC { reg } => reg,
        LineKind::Pop { reg } => reg,
        LineKind::Cmp => REG_NONE,
        LineKind::Push { .. } => REG_NONE,
        _ => REG_NONE,
    }
}

/// Try to replace uses of `dst_id` with `src_id` in instruction at index `j`.
/// Returns true if a replacement was made.
fn try_propagate_into(
    store: &mut LineStore,
    infos: &mut [LineInfo],
    j: usize,
    src_id: RegId,
    dst_id: RegId,
) -> bool {
    let trimmed = infos[j].trimmed(store.get(j));

    // The instruction must reference the destination register
    if infos[j].reg_refs & (1u16 << dst_id) == 0 {
        return false;
    }

    // Skip instructions with implicit register usage
    if has_implicit_reg_usage(trimmed) {
        return false;
    }

    // Skip shift/rotate when propagating into %rcx (they need %cl)
    if dst_id == 1 && is_shift_or_rotate(trimmed) {
        return false;
    }

    let next_dest = get_dest_reg(&infos[j]);

    let src_name = REG_NAMES[0][src_id as usize];
    let dst_name = REG_NAMES[0][dst_id as usize];

    // Case 1: next instruction writes to src_id
    if next_dest == src_id {
        // Source register is being written by this instruction.
        // Only safe if dst appears ONLY in a memory base position like (%dst),
        // which is read before the destination write.
        let dst_paren = format!("({})", dst_name);
        if !trimmed.contains(dst_paren.as_str()) {
            return false;
        }
        // Also check src doesn't appear directly as a source operand in addition
        // to being the destination.
        let src_paren = format!("({})", src_name);
        let src_direct_count = trimmed.matches(src_name).count();
        let src_paren_count = trimmed.matches(src_paren.as_str()).count();
        let is_dest_only = if let Some((_before, after_comma)) = trimmed.rsplit_once(',') {
            after_comma.trim() == src_name
        } else {
            false
        };
        let src_as_source = src_direct_count - src_paren_count - if is_dest_only { 1 } else { 0 };
        if src_as_source > 0 {
            return false;
        }
        let new_text = format!("    {}", replace_reg_family(trimmed, dst_id, src_id));
        replace_line(store, &mut infos[j], j, new_text);
        return true;
    }

    // Case 2: dst is not the destination - replace all occurrences
    if next_dest != dst_id {
        let new_content = replace_reg_family(trimmed, dst_id, src_id);
        if new_content != trimmed {
            let new_text = format!("    {}", new_content);
            replace_line(store, &mut infos[j], j, new_text);
            return true;
        }
        return false;
    }

    // Case 3: dst is the destination AND a source (e.g., addq %rcx, %rcx)
    // For single-operand read-modify-write instructions (negq, notq, incq, decq),
    // the single operand is both source and destination. Don't replace.
    if !trimmed.contains(',') {
        return false;
    }
    // Only replace the source-position occurrences.
    let new_content = replace_reg_family_in_source(trimmed, dst_id, src_id);
    if new_content != trimmed {
        let new_text = format!("    {}", new_content);
        replace_line(store, &mut infos[j], j, new_text);
        return true;
    }
    false
}

fn propagate_register_copies(store: &mut LineStore, infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = store.len();

    // copy_src[dst] = src means "dst currently holds the same value as src"
    // REG_NONE means no active copy for this register.
    // We use a fixed-size array indexed by register family (0..=15).
    let mut copy_src: [RegId; 16] = [REG_NONE; 16];

    let mut i = 0;
    while i < len {
        // At basic block boundaries, clear all copies
        if infos[i].is_barrier() {
            copy_src = [REG_NONE; 16];
            i += 1;
            continue;
        }

        if infos[i].is_nop() {
            i += 1;
            continue;
        }

        // Check if this is a reg-to-reg movq that defines a new copy
        let trimmed = infos[i].trimmed(store.get(i));
        if let Some((src_id, dst_id)) = parse_reg_to_reg_movq(&infos[i], trimmed) {
            // Resolve transitive copies: if src itself is a copy of something,
            // use the ultimate source. This handles chains like:
            //   movq %rax, %rcx   -> copy_src[rcx] = rax
            //   movq %rcx, %rdx   -> copy_src[rdx] = rax (not rcx)
            let ultimate_src = if copy_src[src_id as usize] != REG_NONE {
                copy_src[src_id as usize]
            } else {
                src_id
            };

            // If the source register has an active copy, rewrite the movq to
            // use the ultimate source directly. For example:
            //   movq %rax, %r14   ; copy_src[rax] = NONE, so r14 = rax
            //   movq %r14, %rax   ; copy_src[r14] = rax, rewrite to: movq %rax, %rax (self-move!)
            //   movq %rax, %rdi   ; copy_src[rax] = r14, rewrite to: movq %r14, %rdi
            if ultimate_src != src_id && ultimate_src != dst_id {
                // Rewrite movq %src, %dst -> movq %ultimate_src, %dst
                let new_src_name = REG_NAMES[0][ultimate_src as usize];
                let dst_name = REG_NAMES[0][dst_id as usize];
                let new_text = format!("    movq {}, {}", new_src_name, dst_name);
                replace_line(store, &mut infos[i], i, new_text);
                changed = true;
            } else if ultimate_src == dst_id {
                // This movq copies a register to itself via an intermediate.
                // E.g., movq %rax, %rcx; movq %rcx, %rax -> the second is movq %rax, %rax (self-move).
                // Mark as NOP - it will be cleaned up.
                mark_nop(&mut infos[i]);
                changed = true;
                i += 1;
                continue;
            }

            // Before recording: invalidate any copies that have dst as their source.
            // Writing to dst invalidates "X is a copy of dst".
            for k in 0..16u8 {
                if copy_src[k as usize] == dst_id {
                    copy_src[k as usize] = REG_NONE;
                }
            }

            // Record the copy
            copy_src[dst_id as usize] = ultimate_src;

            i += 1;
            continue;
        }

        // Not a copy instruction. Try to propagate active copies into this instruction.
        // First, determine which registers this instruction writes to (kills).
        let dest_reg = get_dest_reg(&infos[i]);

        // Try to propagate each active copy into this instruction.
        // We process one copy at a time to avoid conflicts from multiple replacements.
        // Iterate through active copies and try to apply them.
        let mut did_propagate = false;
        for reg in 0..16u8 {
            let src = copy_src[reg as usize];
            if src == REG_NONE {
                continue;
            }
            // Check if this instruction references the copied register
            if infos[i].reg_refs & (1u16 << reg) == 0 {
                continue;
            }
            // Skip if instruction has implicit register usage
            let cur_trimmed = infos[i].trimmed(store.get(i));
            if has_implicit_reg_usage(cur_trimmed) {
                break; // Don't propagate any copy into this instruction
            }

            // Try to propagate: replace reg with src
            if try_propagate_into(store, infos, i, src, reg) {
                changed = true;
                did_propagate = true;
                // After replacement, the line text changed - re-read for subsequent copies.
                // Only do one replacement per instruction to keep things safe.
                break;
            }
        }

        // If we propagated, re-process this instruction (it may have new opportunities
        // or different register references now). But limit to avoid infinite loops.
        if did_propagate {
            // Don't increment i - re-process this line for additional copy propagations
            // But we need a limit to prevent infinite loops
            // Actually, let's just continue; the next iteration of the outer loop
            // (phase 1/3 re-run) will catch cascaded opportunities.
            // Fall through to invalidation logic below.
        }

        // Invalidate copies affected by this instruction's writes.
        // If this instruction writes to register `dest_reg`, then:
        //   1. copy_src[dest_reg] is no longer valid (dest_reg got a new value)
        //   2. Any copy whose source is dest_reg is invalidated
        if dest_reg != REG_NONE && dest_reg <= REG_GP_MAX {
            copy_src[dest_reg as usize] = REG_NONE;
            for k in 0..16u8 {
                if copy_src[k as usize] == dest_reg {
                    copy_src[k as usize] = REG_NONE;
                }
            }
        }

        // Instructions with implicit register usage (xchg, cmpxchg, lock prefix,
        // div/idiv, mul, etc.) may modify registers not captured by dest_reg.
        // For example, xchgl %eax, (%rcx) writes to %eax but parse_dest_reg_fast
        // sees (%rcx) as the last operand and returns REG_NONE. Similarly, div/idiv
        // write both rax and rdx. Conservatively invalidate all register copies
        // when we encounter such instructions.
        {
            let cur_trimmed = infos[i].trimmed(store.get(i));
            if has_implicit_reg_usage(cur_trimmed) {
                copy_src = [REG_NONE; 16];
            }
        }

        i += 1;
    }
    changed
}

// ── Dead store elimination ────────────────────────────────────────────────────

fn eliminate_dead_stores(store: &LineStore, infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = store.len();
    const WINDOW: usize = 16;

    // Reusable stack buffer for the "OFFSET(%rbp)" pattern string, used as fallback
    // when the pre-parsed rbp_offset is RBP_OFFSET_NONE (multiple/complex refs).
    // Uses write_rbp_pattern to avoid core::fmt overhead.
    let mut pattern_bytes = [0u8; 24];

    for i in 0..len {
        let (store_offset, store_size) = match infos[i].kind {
            LineKind::StoreRbp { offset, size, .. } => (offset, size),
            _ => continue,
        };

        // Byte range of the original store: [store_offset, store_end)
        // Note: offsets are negative from rbp, so store_offset is the lowest address
        // and store_offset + byte_size is the highest.
        let store_bytes = store_size.byte_size();

        let end = std::cmp::min(i + WINDOW, len);
        let mut slot_read = false;
        let mut slot_overwritten = false;
        let mut pattern_len: usize = 0;

        for j in (i + 1)..end {
            if infos[j].is_nop() {
                continue;
            }

            // Barrier: conservatively assume slot may be read
            if infos[j].is_barrier() {
                slot_read = true;
                break;
            }

            // Load from overlapping range = slot is read.
            if let LineKind::LoadRbp { offset: load_off, size: load_sz, .. } = infos[j].kind {
                if ranges_overlap(store_offset, store_bytes, load_off, load_sz.byte_size()) {
                    slot_read = true;
                    break;
                }
            }

            // Another store to same offset = slot overwritten, but ONLY if the new
            // store fully covers the original store's byte range.
            if let LineKind::StoreRbp { offset: new_off, size: new_sz, .. } = infos[j].kind {
                let new_bytes = new_sz.byte_size();
                if new_off <= store_offset && new_off + new_bytes >= store_offset + store_bytes {
                    slot_overwritten = true;
                    break;
                }
                // Partial overlap: conservatively treat as a read.
                if ranges_overlap(store_offset, store_bytes, new_off, new_bytes) {
                    slot_read = true;
                    break;
                }
            }

            // Catch-all: check if line references any byte in the store's range
            // (handles leaq, movslq, etc. that aren't classified as store/load)
            if matches!(infos[j].kind, LineKind::Other { .. }) {
                // Check for indirect memory access using cached flag (computed
                // once during classify_line, avoiding repeated byte scans).
                if infos[j].has_indirect_mem {
                    slot_read = true;
                    break;
                }
                // Fast path: use pre-parsed rbp_offset for O(1) integer comparison
                // instead of O(n) string search.
                let rbp_off = infos[j].rbp_offset;
                if rbp_off != RBP_OFFSET_NONE {
                    // Check if the other line's rbp reference falls within the
                    // store's byte range. We don't know the other line's access
                    // size, so conservatively treat any overlap as a read.
                    if rbp_off >= store_offset && rbp_off < store_offset + store_bytes {
                        slot_read = true;
                        break;
                    }
                    // Also check if a wider access at rbp_off could overlap:
                    // the other line could access up to 8 bytes starting at rbp_off.
                    // Conservative: if rbp_off + 8 > store_offset, there could be overlap.
                    if rbp_off < store_offset && rbp_off + 8 > store_offset {
                        slot_read = true;
                        break;
                    }
                    // rbp_offset is known and doesn't overlap - this line is safe
                    continue;
                }
                // Fallback: rbp_offset is RBP_OFFSET_NONE (no rbp ref or complex pattern).
                // Lines with no (%rbp) at all won't match; the string check handles
                // edge cases where parse_rbp_offset couldn't determine a single offset.
                if pattern_len == 0 {
                    pattern_len = write_rbp_pattern(&mut pattern_bytes, store_offset);
                }
                // write_rbp_pattern only writes ASCII digits, '-', and "(%rbp)" bytes.
                let pattern = std::str::from_utf8(&pattern_bytes[..pattern_len])
                    .expect("rbp pattern produced non-UTF8");
                let line = infos[j].trimmed(store.get(j));
                if line.contains(pattern) {
                    slot_read = true;
                    break;
                }
                // For wider stores, also check intermediate offsets in the store's range
                if store_bytes > 1 {
                    for byte_off in 1..store_bytes {
                        let check_off = store_offset + byte_off;
                        let check_len = write_rbp_pattern(&mut pattern_bytes, check_off);
                        // write_rbp_pattern only writes ASCII digits, '-', and "(%rbp)" bytes.
                        let check_pattern = std::str::from_utf8(&pattern_bytes[..check_len])
                            .expect("rbp pattern produced non-UTF8");
                        let line = infos[j].trimmed(store.get(j));
                        if line.contains(check_pattern) {
                            slot_read = true;
                            break;
                        }
                    }
                    if slot_read { break; }
                }
            }
        }

        if slot_overwritten && !slot_read {
            mark_nop(&mut infos[i]);
            changed = true;
        }
    }

    changed
}

// ── Global dead store elimination for never-read stack slots ─────────────────
//
// After global_store_forwarding converts loads to register-to-register moves
// or NOPs, many stores to stack slots become dead because the slot is never
// loaded from anywhere in the function. The local eliminate_dead_stores pass
// only catches stores overwritten within a 16-instruction window. This pass
// does a whole-function analysis: if a stack slot is never read from (no LoadRbp,
// no Other instruction referencing its %rbp offset), the store is dead.
//
// Safety: we conservatively treat any %rbp reference in an Other instruction
// as a "read" of that slot (even if it's a leaq taking the address). If any
// instruction has has_indirect_mem (e.g., inline asm, rep movsb), we bail out
// for the entire function since indirect memory accesses could read any slot.
// Callee-saved register saves (in the prologue) are preserved.

fn eliminate_never_read_stores(store: &LineStore, infos: &mut [LineInfo]) {
    let len = store.len();
    if len == 0 {
        return;
    }

    // Process each function independently. Functions are delimited by the
    // prologue pattern: pushq %rbp; movq %rsp, %rbp; subq $N, %rsp.
    let mut i = 0;
    while i < len {
        if infos[i].is_nop() {
            i += 1;
            continue;
        }
        // Look for function prologue: pushq %rbp
        if !matches!(infos[i].kind, LineKind::Push { reg: 5 }) {
            i += 1;
            continue;
        }

        // Next non-nop should be "movq %rsp, %rbp"
        let mut j = next_non_nop(infos, i + 1, len);
        if j >= len {
            i = j;
            continue;
        }
        let mov_line = infos[j].trimmed(store.get(j));
        if mov_line != "movq %rsp, %rbp" {
            i = j + 1;
            continue;
        }
        j += 1;

        // Next non-nop should be "subq $N, %rsp"
        j = next_non_nop(infos, j, len);
        if j >= len {
            i = j;
            continue;
        }
        let subq_line = infos[j].trimmed(store.get(j));
        let is_subq = if let Some(rest) = subq_line.strip_prefix("subq $") {
            rest.strip_suffix(", %rsp").and_then(|v| v.parse::<i64>().ok()).is_some()
        } else {
            false
        };
        if !is_subq {
            i = j + 1;
            continue;
        }
        j += 1;

        // Skip callee-saved register saves (immediately after subq)
        j = next_non_nop(infos, j, len);
        let mut callee_save_end = j;
        while callee_save_end < len {
            if infos[callee_save_end].is_nop() {
                callee_save_end += 1;
                continue;
            }
            if let LineKind::StoreRbp { reg, size: MoveSize::Q, .. } = infos[callee_save_end].kind {
                if is_callee_saved_reg(reg) {
                    callee_save_end += 1;
                    continue;
                }
            }
            break;
        }
        let body_start = callee_save_end;

        // Find the end of this function (.size directive)
        let mut func_end = len;
        for k in body_start..len {
            if infos[k].is_nop() {
                continue;
            }
            let line = infos[k].trimmed(store.get(k));
            if line.starts_with(".size ") {
                func_end = k + 1;
                break;
            }
        }

        // Phase 1: Collect all "read" byte ranges on the stack in this function.
        // A byte range [offset, offset+size) is "read" if ANY non-store instruction
        // accesses it. We store (offset, byte_size) pairs and check for overlap.
        //
        // We check every line type that could possibly access memory:
        //   - LoadRbp: direct load (pre-parsed offset and size)
        //   - Other: pre-parsed rbp_offset (covers leaq, movq, etc.)
        //   - All other instruction types: parse raw text for %rbp references
        // If any instruction has has_indirect_mem, bail out for safety.
        let mut has_indirect = false;
        // Read ranges: (start_offset, byte_size) pairs
        let mut read_ranges: Vec<(i32, i32)> = Vec::new();

        for k in body_start..func_end {
            if infos[k].is_nop() {
                continue;
            }

            if infos[k].has_indirect_mem {
                has_indirect = true;
                break;
            }

            match infos[k].kind {
                // StoreRbp: this is what we're trying to eliminate; don't count as read
                LineKind::StoreRbp { .. } => {}
                // LoadRbp: direct load - record the full byte range
                LineKind::LoadRbp { offset, size, .. } => {
                    read_ranges.push((offset, size.byte_size()));
                }
                // Other: has pre-parsed rbp_offset.
                // Special case: `leaq N(%rbp), %reg` takes the ADDRESS of a
                // stack slot, meaning the pointer escapes. Bail out.
                // For all other instructions, use a conservative 32-byte access
                // size to cover SSE (16B), x87 (10-16B), and complex (32B) ops.
                LineKind::Other { .. } => {
                    let rbp_off = infos[k].rbp_offset;
                    if rbp_off != RBP_OFFSET_NONE {
                        let line = infos[k].trimmed(store.get(k));
                        if line.starts_with("leaq ") {
                            // Address of stack slot escapes - bail out
                            has_indirect = true;
                            break;
                        }
                        // Other instructions can have varying access sizes:
                        // - Standard GP ops: 1-8 bytes
                        // - SSE ops (movdqa etc.): 16 bytes
                        // - x87/long double ops: 10-16 bytes
                        // - Complex number ops: up to 32 bytes
                        // Use 32 bytes as conservative upper bound to cover
                        // all possible access patterns.
                        read_ranges.push((rbp_off, 32));
                    } else {
                        // rbp_offset is RBP_OFFSET_NONE: either no %rbp ref,
                        // or multiple different offsets. If the line text contains
                        // (%rbp), we can't determine which slot(s) are referenced,
                        // so bail out for safety.
                        let line = infos[k].trimmed(store.get(k));
                        if line.contains("(%rbp)") {
                            has_indirect = true;
                            break;
                        }
                    }
                }
                // Lines that can't reference %rbp memory operands
                LineKind::Nop | LineKind::Empty | LineKind::SelfMove
                | LineKind::Label | LineKind::Jmp | LineKind::CondJmp
                | LineKind::JmpIndirect | LineKind::Ret | LineKind::Directive => {}
                // All other types (Cmp, Push, Pop, SetCC, Call): parse raw text
                // for %rbp references to be safe (assume 8-byte access)
                _ => {
                    let line = infos[k].trimmed(store.get(k));
                    let rbp_off = parse_rbp_offset(line);
                    if rbp_off != RBP_OFFSET_NONE {
                        read_ranges.push((rbp_off, 8));
                    } else if line.contains("(%rbp)") {
                        // Multiple or unparseable %rbp references - bail out
                        has_indirect = true;
                        break;
                    }
                }
            }
        }

        if has_indirect {
            i = func_end;
            continue;
        }

        // Phase 2: Eliminate stores to slots whose byte range doesn't overlap
        // with any read range.
        for k in body_start..func_end {
            if infos[k].is_nop() {
                continue;
            }
            if let LineKind::StoreRbp { offset, size, .. } = infos[k].kind {
                let store_bytes = size.byte_size();
                let is_read = read_ranges.iter().any(|&(r_off, r_sz)| {
                    ranges_overlap(offset, store_bytes, r_off, r_sz)
                });
                if !is_read {
                    mark_nop(&mut infos[k]);
                }
            }
        }

        i = func_end;
    }
}

// ── Global store forwarding across basic block boundaries ─────────────────────
//
// Tracks register→slot mappings across the function, forwarding stored values
// to subsequent loads. Key insight: at a label reached only by fallthrough (not
// a jump target), register state from the previous instruction is fully known,
// so we can safely forward across such labels.
//
// For labels that ARE jump targets, all mappings are invalidated because the
// jump source may have different register values. This is critical for loops
// in the stack-based codegen model.

/// A tracked store mapping: we know that stack slot at `offset` contains the
/// value that was in register `reg_id` with the given `size`.
#[derive(Clone, Copy)]
struct SlotMapping {
    reg_id: RegId,
    size: MoveSize,
}

/// Clear all slot→register mappings. Used at control flow boundaries
/// (jump targets, returns, unconditional jumps) where register state is unknown.
#[inline]
fn invalidate_all_mappings(slot_entries: &mut Vec<SlotEntry>, reg_offsets: &mut [SmallVec; 16]) {
    slot_entries.clear();
    for rs in reg_offsets.iter_mut() {
        rs.clear();
    }
}

/// Check if a LoadRbp at position `pos` is part of the function epilogue.
/// The epilogue pattern is: callee-save restores, then `movq %rbp, %rsp; popq %rbp; ret/jmp`.
/// We look forward from `pos` for a Ret or Jmp (for __x86_return_thunk) within a small window,
/// only allowing other LoadRbp or Pop or Other (for stack teardown) instructions between.
fn is_near_epilogue(infos: &[LineInfo], pos: usize) -> bool {
    let limit = (pos + 20).min(infos.len());
    for j in (pos + 1)..limit {
        if infos[j].is_nop() {
            continue;
        }
        match infos[j].kind {
            // More callee-save restores or stack teardown moves are expected
            LineKind::LoadRbp { .. } | LineKind::Pop { .. } | LineKind::SelfMove => continue,
            // Stack pointer restoration (movq %rbp, %rsp) is classified as Other
            LineKind::Other { .. } => continue,
            // Found the return instruction - this is an epilogue
            LineKind::Ret | LineKind::Jmp | LineKind::JmpIndirect => return true,
            // Any other instruction type means we're not in the epilogue
            _ => return false,
        }
    }
    false
}

fn global_store_forwarding(store: &mut LineStore, infos: &mut [LineInfo]) -> bool {
    let len = store.len();
    if len == 0 {
        return false;
    }

    let jump_targets = collect_jump_targets(store, infos, len);

    // Phase 2: Forward scan with slot→register mappings.
    let mut slot_entries: Vec<SlotEntry> = Vec::new();
    let mut reg_offsets: [SmallVec; 16] = Default::default();
    let mut changed = false;
    let mut prev_was_unconditional_jump = false;

    for i in 0..len {
        if infos[i].is_nop() || infos[i].kind == LineKind::Empty {
            continue;
        }

        // Default: most instructions are not unconditional jumps.
        // Only Jmp/JmpIndirect/Ret set this to true below.
        let was_uncond_jump = prev_was_unconditional_jump;
        prev_was_unconditional_jump = false;

        match infos[i].kind {
            LineKind::Label => {
                gsf_handle_label(store, infos, i, &jump_targets,
                    &mut slot_entries, &mut reg_offsets, was_uncond_jump);
            }

            LineKind::StoreRbp { reg, offset, size } => {
                gsf_handle_store(reg, offset, size,
                    &mut slot_entries, &mut reg_offsets);
            }

            LineKind::LoadRbp { reg: load_reg, offset: load_offset, size: load_size } => {
                changed |= gsf_handle_load(store, infos, i, load_reg, load_offset, load_size,
                    &mut slot_entries, &mut reg_offsets);
            }

            LineKind::Jmp | LineKind::JmpIndirect | LineKind::Ret => {
                invalidate_all_mappings(&mut slot_entries, &mut reg_offsets);
                prev_was_unconditional_jump = true;
            }

            LineKind::Call => {
                for &r in &[0u8, 1, 2, 6, 7, 8, 9, 10, 11] {
                    invalidate_reg_flat(&mut slot_entries, &mut reg_offsets, r);
                }
            }

            LineKind::Pop { reg } | LineKind::SetCC { reg } => {
                if is_valid_gp_reg(reg) {
                    invalidate_reg_flat(&mut slot_entries, &mut reg_offsets, reg);
                }
            }

            LineKind::Other { dest_reg } => {
                gsf_handle_other(store, infos, i, dest_reg,
                    &mut slot_entries, &mut reg_offsets);
            }

            LineKind::CondJmp | LineKind::Cmp | LineKind::Push { .. }
            | LineKind::Directive => {}

            _ => {}
        }
    }

    changed
}

/// Jump target analysis result for global store forwarding.
struct JumpTargets {
    is_jump_target: Vec<bool>,
    has_non_numeric_jump_targets: bool,
}

/// Phase 1: Collect all jump/branch targets using a flat Vec<bool> indexed by
/// label suffix number. For non-numeric labels, fall back to treating them as
/// jump targets.
fn collect_jump_targets(store: &LineStore, infos: &[LineInfo], len: usize) -> JumpTargets {
    let mut max_label_num: u32 = 0;
    for i in 0..len {
        if infos[i].kind == LineKind::Label {
            let trimmed = infos[i].trimmed(store.get(i));
            if let Some(n) = parse_label_number(trimmed) {
                if n > max_label_num {
                    max_label_num = n;
                }
            }
        }
    }
    let mut is_jump_target = vec![false; (max_label_num + 1) as usize];
    let mut has_non_numeric_jump_targets = false;
    let mut has_indirect_jump = false;
    for i in 0..len {
        match infos[i].kind {
            LineKind::Jmp | LineKind::CondJmp => {
                let trimmed = infos[i].trimmed(store.get(i));
                if let Some(target) = extract_jump_target(trimmed) {
                    if let Some(n) = parse_dotl_number(target) {
                        if (n as usize) < is_jump_target.len() {
                            is_jump_target[n as usize] = true;
                        }
                    } else {
                        has_non_numeric_jump_targets = true;
                    }
                }
            }
            LineKind::JmpIndirect => {
                has_indirect_jump = true;
            }
            _ => {}
        }
    }
    if has_indirect_jump {
        for v in is_jump_target.iter_mut() {
            *v = true;
        }
        has_non_numeric_jump_targets = true;
    }
    JumpTargets { is_jump_target, has_non_numeric_jump_targets }
}

/// Handle a Label in global store forwarding: invalidate mappings if this label
/// is a jump target or follows unreachable code.
fn gsf_handle_label(
    store: &LineStore, infos: &[LineInfo], i: usize,
    targets: &JumpTargets,
    slot_entries: &mut Vec<SlotEntry>, reg_offsets: &mut [SmallVec; 16],
    prev_was_unconditional_jump: bool,
) {
    let label_name = infos[i].trimmed(store.get(i));
    let is_target = if let Some(n) = parse_label_number(label_name) {
        (n as usize) < targets.is_jump_target.len() && targets.is_jump_target[n as usize]
    } else {
        targets.has_non_numeric_jump_targets
    };
    if is_target || prev_was_unconditional_jump {
        invalidate_all_mappings(slot_entries, reg_offsets);
    }
}

/// Handle a StoreRbp in global store forwarding: update slot→register mapping.
fn gsf_handle_store(
    reg: RegId, offset: i32, size: MoveSize,
    slot_entries: &mut Vec<SlotEntry>, reg_offsets: &mut [SmallVec; 16],
) {
    // Invalidate overlapping mappings.
    invalidate_slots_at(slot_entries, reg_offsets, offset, size.byte_size());
    // Record new mapping (GP registers only).
    if is_valid_gp_reg(reg) {
        slot_entries.push(SlotEntry {
            offset,
            mapping: SlotMapping { reg_id: reg, size },
            active: true,
        });
        reg_offsets[reg as usize].push(offset);
    }
    // Compact when the vec grows beyond 64.
    if slot_entries.len() > 64 {
        slot_entries.retain(|e| e.active);
    }
}

/// Handle a LoadRbp in global store forwarding: forward from register or eliminate.
fn gsf_handle_load(
    store: &mut LineStore, infos: &mut [LineInfo], i: usize,
    load_reg: RegId, load_offset: i32, load_size: MoveSize,
    slot_entries: &mut Vec<SlotEntry>, reg_offsets: &mut [SmallVec; 16],
) -> bool {
    let mut changed = false;
    let mapping = slot_entries.iter().rev()
        .find(|e| e.active && e.offset == load_offset)
        .map(|e| e.mapping);
    if let Some(mapping) = mapping {
        if mapping.size == load_size && mapping.reg_id != REG_NONE {
            // Don't eliminate callee-save register restores in the epilogue.
            let is_epilogue_restore = matches!(load_reg, 3 | 12 | 13 | 14 | 15)
                && load_offset < 0
                && is_near_epilogue(infos, i);
            if load_reg == mapping.reg_id && !is_epilogue_restore {
                mark_nop(&mut infos[i]);
                changed = true;
            } else if load_reg != REG_NONE && load_reg != mapping.reg_id {
                let store_reg_str = reg_id_to_name(mapping.reg_id, load_size);
                let load_reg_str = reg_id_to_name(load_reg, load_size);
                let new_text = format!("    {} {}, {}",
                    load_size.mnemonic(), store_reg_str, load_reg_str);
                replace_line(store, &mut infos[i], i, new_text);
                changed = true;
            }
        }
    }
    if is_valid_gp_reg(load_reg) {
        invalidate_reg_flat(slot_entries, reg_offsets, load_reg);
    }
    changed
}

/// Handle an Other instruction in global store forwarding: invalidate
/// registers and slots based on dest_reg, RMW, and indirect memory access.
fn gsf_handle_other(
    store: &LineStore, infos: &[LineInfo], i: usize, dest_reg: RegId,
    slot_entries: &mut Vec<SlotEntry>, reg_offsets: &mut [SmallVec; 16],
) {
    // Invalidate destination register.
    if is_valid_gp_reg(dest_reg) {
        invalidate_reg_flat(slot_entries, reg_offsets, dest_reg);
        // div/idiv/mul/cqto/cqo/cdq also clobber rdx (family 2).
        if dest_reg == 0 {
            let trimmed = infos[i].trimmed(store.get(i));
            if trimmed.starts_with("div") || trimmed.starts_with("idiv")
                || trimmed.starts_with("mul")
                || trimmed == "cqto" || trimmed == "cqo" || trimmed == "cdq"
            {
                invalidate_reg_flat(slot_entries, reg_offsets, 2);
            }
        }
    }

    // Read-modify-write instructions with a memory destination at (%rbp) offset.
    if dest_reg == REG_NONE && infos[i].rbp_offset != RBP_OFFSET_NONE {
        invalidate_slots_at(slot_entries, reg_offsets, infos[i].rbp_offset, 0);
    }

    // Indirect memory access: conservatively invalidate everything.
    if infos[i].has_indirect_mem {
        invalidate_all_mappings(slot_entries, reg_offsets);
    } else if infos[i].rbp_offset != RBP_OFFSET_NONE {
        // Unrecognized instruction with %rbp offset: conservatively invalidate
        // overlapping slot mappings (unknown access size, use 1-byte overlap).
        invalidate_slots_at(slot_entries, reg_offsets, infos[i].rbp_offset, 1);
    }
}

/// Deactivate a single slot entry and remove its offset from the per-register
/// tracking. This is the single point of deactivation to avoid duplicating
/// the `active = false` + `remove_val` pair across multiple functions.
#[inline]
fn deactivate_entry(entry: &mut SlotEntry, reg_offsets: &mut [SmallVec; 16]) {
    let old_reg = entry.mapping.reg_id;
    entry.active = false;
    reg_offsets[old_reg as usize].remove_val(entry.offset);
}

/// Invalidate slot mappings at a given offset. When `access_size == 0`, only
/// exact-offset matches are invalidated; otherwise, any mapping whose byte
/// range overlaps `[offset, offset + access_size)` is invalidated.
fn invalidate_slots_at(
    slot_entries: &mut Vec<SlotEntry>, reg_offsets: &mut [SmallVec; 16],
    offset: i32, access_size: i32,
) {
    for entry in slot_entries.iter_mut().filter(|e| e.active) {
        let hit = if access_size == 0 {
            entry.offset == offset
        } else {
            ranges_overlap(offset, access_size, entry.offset, entry.mapping.size.byte_size())
        };
        if hit {
            deactivate_entry(entry, reg_offsets);
        }
    }
}

/// A slot entry for flat-array store forwarding.
#[derive(Clone, Copy)]
struct SlotEntry {
    offset: i32,
    mapping: SlotMapping,
    active: bool,
}

/// Small inline vector for register->offset tracking (avoids heap allocation
/// for the common case of <=4 offsets per register).
#[derive(Clone, Default)]
struct SmallVec {
    inline: [i32; 4],
    len: u8,
    overflow: Option<Vec<i32>>,
}

impl SmallVec {
    #[inline]
    fn push(&mut self, val: i32) {
        if let Some(ref mut ov) = self.overflow {
            ov.push(val);
        } else if (self.len as usize) < 4 {
            self.inline[self.len as usize] = val;
            self.len += 1;
        } else {
            let mut v = Vec::with_capacity(8);
            v.extend_from_slice(&self.inline[..4]);
            v.push(val);
            self.overflow = Some(v);
        }
    }

    #[inline]
    fn clear(&mut self) {
        self.len = 0;
        self.overflow = None;
    }

    #[inline]
    fn remove_val(&mut self, val: i32) {
        if let Some(ref mut ov) = self.overflow {
            ov.retain(|&v| v != val);
        } else {
            let n = self.len as usize;
            for j in 0..n {
                if self.inline[j] == val {
                    // Swap-remove
                    self.inline[j] = self.inline[n - 1];
                    self.len -= 1;
                    return;
                }
            }
        }
    }

    #[inline]
    fn iter(&self) -> SmallVecIter<'_> {
        SmallVecIter { sv: self, idx: 0 }
    }
}

struct SmallVecIter<'a> {
    sv: &'a SmallVec,
    idx: usize,
}

impl<'a> Iterator for SmallVecIter<'a> {
    type Item = i32;
    #[inline]
    fn next(&mut self) -> Option<i32> {
        if let Some(ref ov) = self.sv.overflow {
            if self.idx < ov.len() {
                let v = ov[self.idx];
                self.idx += 1;
                Some(v)
            } else {
                None
            }
        } else if self.idx < self.sv.len as usize {
            let v = self.sv.inline[self.idx];
            self.idx += 1;
            Some(v)
        } else {
            None
        }
    }
}

/// Parse ".LBB<number>:" label into its number, e.g. ".LBB123:" -> Some(123)
#[inline]
fn parse_label_number(label_with_colon: &str) -> Option<u32> {
    let s = label_with_colon.strip_suffix(':')?;
    parse_dotl_number(s)
}

/// Parse ".LBB<number>" into its number, e.g. ".LBB123" -> Some(123)
#[inline]
fn parse_dotl_number(s: &str) -> Option<u32> {
    let rest = s.strip_prefix(".LBB")?;
    let b = rest.as_bytes();
    if b.is_empty() || b[0] < b'0' || b[0] > b'9' {
        return None;
    }
    let mut v: u32 = 0;
    for &c in b {
        if c.is_ascii_digit() {
            v = v.wrapping_mul(10).wrapping_add((c - b'0') as u32);
        } else {
            return None;
        }
    }
    Some(v)
}

/// Remove all slot mappings backed by a given register (flat array version).
fn invalidate_reg_flat(
    slot_entries: &mut [SlotEntry],
    reg_offsets: &mut [SmallVec; 16],
    reg_id: RegId,
) {
    let offsets = &reg_offsets[reg_id as usize];
    for offset in offsets.iter() {
        // Find and deactivate the entry for this offset backed by this register
        for entry in slot_entries.iter_mut().rev() {
            if entry.active && entry.offset == offset && entry.mapping.reg_id == reg_id {
                entry.active = false;
                break;
            }
        }
    }
    reg_offsets[reg_id as usize].clear();
}

/// Extract the jump target label from a jump/branch instruction.
fn extract_jump_target(s: &str) -> Option<&str> {
    // Handle: jmp .LBB1, je .LBB1, jne .LBB1, etc.
    if let Some(rest) = s.strip_prefix("jmp ") {
        return Some(rest.trim());
    }
    // Conditional jumps: j<cc> <target>
    if s.starts_with('j') {
        if let Some(space) = s.find(' ') {
            return Some(s[space + 1..].trim());
        }
    }
    None
}

// ── Loop trampoline elimination ──────────────────────────────────────────────
//
// SSA codegen creates "trampoline" blocks for loop back-edges to resolve phi
// nodes. Instead of updating loop variables in-place, it creates new SSA values
// in fresh registers and uses a separate block to shuffle them back:
//
//   .LOOP:
//       ; ... loop body using %r9, %r10, %r11 ...
//       movq %r9, %r14           ; copy old dest to new reg
//       addq $320, %r14          ; modify new dest
//       movq %r10, %r15          ; copy old frac to new reg
//       addl %r8d, %r15d         ; modify new frac
//       ; ... loop condition ...
//       jne .TRAMPOLINE
//   .TRAMPOLINE:
//       movq %r14, %r9           ; shuffle new dest back
//       movq %r15, %r10          ; shuffle new frac back
//       jmp .LOOP
//
// This pass detects trampoline blocks and coalesces the register copies:
//   1. For each trampoline copy %src -> %dst, find where %src was created in
//      the predecessor (as a copy from %dst followed by modifications).
//   2. Rewrite those modifications to target %dst directly.
//   3. NOP the initial copy and the trampoline copy.
//   4. Redirect the branch directly to the loop header.
//
// Result: the loop body modifies registers in-place, eliminating 5+ instructions
// per iteration in typical rendering loops.

fn eliminate_loop_trampolines(store: &mut LineStore, infos: &mut [LineInfo]) -> bool {
    let len = store.len();
    if len < 4 {
        return false;
    }

    let mut changed = false;

    // Build a map of label_name -> line_index for all labels.
    // Also count how many branches target each label (to detect single-predecessor).
    let mut label_positions: Vec<(u32, usize)> = Vec::new(); // (label_num, line_idx)
    let mut label_branch_count: Vec<u32> = Vec::new(); // indexed by label number
    let mut max_label_num: u32 = 0;

    for i in 0..len {
        if infos[i].is_nop() { continue; }
        if infos[i].kind == LineKind::Label {
            let trimmed = infos[i].trimmed(store.get(i));
            if let Some(n) = parse_label_number(trimmed) {
                label_positions.push((n, i));
                if n > max_label_num { max_label_num = n; }
            }
        }
    }

    if label_positions.is_empty() {
        return false;
    }

    // Build label_num -> line_index lookup
    let mut label_line: Vec<usize> = vec![usize::MAX; (max_label_num + 1) as usize];
    for &(num, idx) in &label_positions {
        label_line[num as usize] = idx;
    }

    // Count branch references to each label
    label_branch_count.resize((max_label_num + 1) as usize, 0);
    for i in 0..len {
        if infos[i].is_nop() { continue; }
        match infos[i].kind {
            LineKind::Jmp | LineKind::CondJmp => {
                let trimmed = infos[i].trimmed(store.get(i));
                if let Some(target) = extract_jump_target(trimmed) {
                    if let Some(n) = parse_dotl_number(target) {
                        if (n as usize) < label_branch_count.len() {
                            label_branch_count[n as usize] += 1;
                        }
                    }
                }
            }
            _ => {}
        }
    }

    // Find trampoline blocks: label followed by only reg-reg movq (possibly via
    // %rax from stack) and ending with jmp .LBB_N.
    for &(tramp_num, tramp_label_idx) in &label_positions {
        // Only consider trampolines targeted by exactly one branch
        if label_branch_count[tramp_num as usize] != 1 {
            continue;
        }

        // Parse the trampoline block contents
        let mut tramp_moves: Vec<(RegId, RegId)> = Vec::new(); // (src_family, dst_family)
        let mut tramp_jmp_target: Option<u32> = None;
        let mut has_stack_load = false;
        let mut tramp_stack_loads: Vec<(i32, RegId, usize, usize)> = Vec::new(); // (offset, dst_fam, load_idx, mov_idx)
        let mut tramp_all_lines: Vec<usize> = Vec::new();

        let mut j = tramp_label_idx + 1;
        while j < len {
            if infos[j].is_nop() || infos[j].kind == LineKind::Empty {
                j += 1;
                continue;
            }
            let trimmed = infos[j].trimmed(store.get(j));

            // Check for movq %regA, %regB (register-to-register copy)
            if let Some(rest) = trimmed.strip_prefix("movq %") {
                if let Some((src_str, dst_str)) = rest.split_once(", %") {
                    let src_name = format!("%{}", src_str);
                    let dst_name = format!("%{}", dst_str.trim());
                    let src_fam = register_family_fast(&src_name);
                    let dst_fam = register_family_fast(&dst_name);
                    if src_fam != REG_NONE && dst_fam != REG_NONE && src_fam != dst_fam {
                        tramp_moves.push((src_fam, dst_fam));
                        tramp_all_lines.push(j);
                        j += 1;
                        continue;
                    }
                }
            }

            // Check for movslq %regA, %regB (sign-extend register copy)
            if let Some(rest) = trimmed.strip_prefix("movslq %") {
                if let Some((src_str, dst_str)) = rest.split_once(", %") {
                    let src_name = format!("%{}", src_str);
                    let dst_name = format!("%{}", dst_str.trim());
                    let src_fam = register_family_fast(&src_name);
                    let dst_fam = register_family_fast(&dst_name);
                    if src_fam != REG_NONE && dst_fam != REG_NONE && src_fam != dst_fam {
                        tramp_moves.push((src_fam, dst_fam));
                        tramp_all_lines.push(j);
                        j += 1;
                        continue;
                    }
                }
            }

            // Check for stack load pattern: movq -N(%rbp), %rax (intermediate for
            // stack-to-register transfer, must be followed by movq %rax, %regB)
            if let LineKind::LoadRbp { reg: 0, offset, .. } = infos[j].kind {
                // This is a load to %rax from stack - peek at next to see if it's
                // movq %rax, %regB (forming a stack-to-register pair)
                let mut k = j + 1;
                while k < len && (infos[k].is_nop() || infos[k].kind == LineKind::Empty) {
                    k += 1;
                }
                if k < len {
                    let next_trimmed = infos[k].trimmed(store.get(k));
                    if let Some(rest) = next_trimmed.strip_prefix("movq %rax, %") {
                        let dst_name = format!("%{}", rest.trim());
                        let dst_fam = register_family_fast(&dst_name);
                        if dst_fam != REG_NONE && dst_fam != 0 {
                            // This is a stack load via %rax to a target register.
                            // Record it with a special marker source (REG_NONE)
                            // and the stack offset for later coalescing.
                            has_stack_load = true;
                            tramp_stack_loads.push((offset, dst_fam, j, k));
                            tramp_all_lines.push(j);
                            tramp_all_lines.push(k);
                            j = k + 1;
                            continue;
                        }
                    }
                }
                // Not a valid trampoline pattern
                break;
            }

            // Check for jmp .LBB_N (final instruction)
            if infos[j].kind == LineKind::Jmp {
                if let Some(target) = extract_jump_target(trimmed) {
                    if let Some(n) = parse_dotl_number(target) {
                        tramp_jmp_target = Some(n);
                        tramp_all_lines.push(j);
                    }
                }
                break;
            }

            // Any other instruction - not a trampoline
            break;
        }

        // Must have a jmp target and at least one register move
        let target_num = match tramp_jmp_target {
            Some(n) => n,
            None => continue,
        };

        // Must have at least one move
        if tramp_moves.is_empty() && !has_stack_load {
            continue;
        }

        // Stack loads in the trampoline require special handling.
        // For each stack load (offset -> dst_fam), we need to find the corresponding
        // store in the predecessor block and coalesce it.

        // Find the conditional branch that targets this trampoline
        let mut branch_idx = None;
        for i in 0..len {
            if infos[i].is_nop() { continue; }
            if infos[i].kind == LineKind::CondJmp {
                let trimmed = infos[i].trimmed(store.get(i));
                if let Some(target) = extract_jump_target(trimmed) {
                    if let Some(n) = parse_dotl_number(target) {
                        if n == tramp_num {
                            branch_idx = Some(i);
                            break;
                        }
                    }
                }
            }
        }

        let branch_idx = match branch_idx {
            Some(i) => i,
            None => continue,
        };

        // Now try to coalesce each trampoline move.
        // For each trampoline move (src -> dst), look backwards from the branch to
        // find where src was created as a copy of dst, and rewrite modifications of
        // src to target dst instead.
        //
        // Pattern we're looking for in the predecessor block:
        //   movq %dst, %src     ; create new SSA value as copy of old
        //   <modify %src>       ; one or more instructions that modify %src
        //   ...                  ; (no reads of %dst between here and trampoline)
        //   jCC .TRAMPOLINE     ; conditional branch
        // Trampoline:
        //   movq %src, %dst     ; shuffle back
        //   jmp .LOOP

        // Per-move coalescing results: track which trampoline moves we can coalesce.
        // Each entry corresponds to a move in tramp_moves at the same index.
        let mut move_coalesced: Vec<bool> = Vec::with_capacity(tramp_moves.len());
        let mut coalesce_actions: Vec<(usize, RegId, RegId)> = Vec::new(); // (line_idx, old_reg, new_reg)
        let mut copy_nop_lines: Vec<usize> = Vec::new();
        for &(src_fam, dst_fam) in &tramp_moves {
            // Search backwards from branch for movq %dst_64, %src_64
            // (the initial copy that creates the new SSA value)
            let src_64 = REG_NAMES[0][src_fam as usize];
            let dst_64 = REG_NAMES[0][dst_fam as usize];

            let mut copy_idx = None;
            let mut modifications: Vec<usize> = Vec::new();
            let mut scan_ok = true;

            // Scan backwards from branch to find the copy and modifications
            let mut k = branch_idx;
            while k > 0 {
                k -= 1;
                if infos[k].is_nop() || infos[k].kind == LineKind::Empty {
                    continue;
                }
                // Stop at labels (block boundary)
                if infos[k].kind == LineKind::Label {
                    break;
                }
                // Stop at calls, jumps, etc.
                if matches!(infos[k].kind, LineKind::Call | LineKind::Jmp |
                    LineKind::JmpIndirect | LineKind::Ret) {
                    break;
                }

                let trimmed = infos[k].trimmed(store.get(k));

                // Check if this instruction modifies src_fam
                let modifies_src = match infos[k].kind {
                    LineKind::Other { dest_reg } => dest_reg == src_fam,
                    LineKind::StoreRbp { .. } => false,
                    LineKind::LoadRbp { reg, .. } => reg == src_fam,
                    LineKind::SetCC { reg } => reg == src_fam,
                    LineKind::Pop { reg } => reg == src_fam,
                    _ => false,
                };

                // Check if this is the initial copy: movq %dst, %src
                if modifies_src {
                    let expected_copy = format!("movq {}, {}", dst_64, src_64);
                    if trimmed == expected_copy {
                        copy_idx = Some(k);
                        break;
                    }
                    // Check for movslq copy variant: movslq %dst_32, %src_64
                    // (sign-extending copy from dst to src - can't simply NOP)
                    let dst_32 = REG_NAMES[1][dst_fam as usize];
                    let expected_movslq = format!("movslq {}, {}", dst_32, src_64);
                    if trimmed == expected_movslq {
                        scan_ok = false;
                        break;
                    }
                    modifications.push(k);
                    continue;
                }

                // Check if this instruction reads src_fam without modifying it.
                // Since we'll NOP the initial copy (movq %dst, %src), any read of
                // src between the copy and the branch must be rewritten to read dst
                // instead. Record such instructions for rewriting.
                if infos[k].reg_refs & (1u16 << src_fam) != 0 {
                    // This instruction reads src but doesn't modify it. We need to
                    // rewrite it to use dst instead. But first check that it doesn't
                    // ALSO reference dst (that would make the rewrite ambiguous).
                    if infos[k].reg_refs & (1u16 << dst_fam) != 0 {
                        // Both src and dst are referenced - can't safely rewrite.
                        scan_ok = false;
                        break;
                    }
                    modifications.push(k);
                    continue;
                }

                // Check if this instruction reads dst_fam (the register we want to
                // rewrite to). If so, we can't coalesce because that would change
                // the value read. We need to ensure dst_fam is NOT read between
                // the copy and the branch.
                if infos[k].reg_refs & (1u16 << dst_fam) != 0 {
                    scan_ok = false;
                    break;
                }
            }

            if !scan_ok || copy_idx.is_none() {
                move_coalesced.push(false);
                continue;
            }

            let copy_idx = copy_idx.unwrap();

            // Verify: on the fall-through path (after the branch), the coalescing
            // is safe. Two conditions must hold:
            //
            // 1. dst_fam is NOT read before being overwritten. After coalescing,
            //    dst holds the POST-modification value, but the fall-through code
            //    might expect the PRE-modification value.
            //
            // 2. src_fam is NOT read before being overwritten. After coalescing,
            //    src_fam is never written (the initial copy is NOPped and the
            //    modifications target dst_fam instead), so any fall-through read
            //    of src_fam would see a stale/uninitialized value.
            //
            // Conservative approach: scan forward from the branch until we hit a
            // ret or unconditional jmp. If either register is read before being
            // overwritten, bail out. When we reach an unconditional jmp, follow
            // its target (up to 2 levels) and continue checking there, since the
            // exit block may read src_fam which was NOPped by coalescing.
            let check_regs = [dst_fam, src_fam];
            let mut fall_through_safe = true;
            let mut m = branch_idx + 1;
            // Track whether each register has been killed (overwritten) on the
            // fall-through path so far.
            let mut killed = [false; 2];
            // Allow following unconditional jmps to their targets (up to 2 hops)
            let mut jumps_followed = 0u32;
            'ft_scan: while m < len {
                if infos[m].is_nop() || infos[m].kind == LineKind::Empty
                    || infos[m].kind == LineKind::Label {
                    m += 1;
                    continue;
                }
                // At unconditional jumps: follow the target if we haven't
                // confirmed all registers are dead, to catch exit blocks
                // that read src_fam after the loop.
                if infos[m].kind == LineKind::Jmp {
                    if jumps_followed < 2 && (!killed[0] || !killed[1]) {
                        let trimmed = infos[m].trimmed(store.get(m));
                        if let Some(target) = extract_jump_target(trimmed) {
                            if let Some(n) = parse_dotl_number(target) {
                                if (n as usize) < label_line.len()
                                    && label_line[n as usize] != usize::MAX
                                {
                                    m = label_line[n as usize] + 1;
                                    jumps_followed += 1;
                                    continue 'ft_scan;
                                }
                            }
                        }
                    }
                    break;
                }
                if matches!(infos[m].kind, LineKind::JmpIndirect | LineKind::Ret) {
                    break;
                }
                // Conditional jumps: fall-through continues past them, but they
                // might also jump to code that uses the registers. Conservatively
                // bail out since the analysis would need to check both paths.
                if infos[m].kind == LineKind::CondJmp {
                    fall_through_safe = false;
                    break;
                }
                for i in 0..2 {
                    if killed[i] {
                        continue;
                    }
                    let reg = check_regs[i];
                    // Check if reg is read (including by stores that reference it)
                    if infos[m].reg_refs & (1u16 << reg) != 0 {
                        fall_through_safe = false;
                        break;
                    }
                    // Check if reg is written (then old value is dead)
                    let writes_reg = match infos[m].kind {
                        LineKind::Other { dest_reg } => dest_reg == reg,
                        LineKind::LoadRbp { reg: r, .. } => r == reg,
                        LineKind::SetCC { reg: r } => r == reg,
                        LineKind::Pop { reg: r } => r == reg,
                        _ => false,
                    };
                    if writes_reg {
                        killed[i] = true;
                    }
                }
                if !fall_through_safe {
                    break;
                }
                // If both registers are killed, no further checking needed
                if killed[0] && killed[1] {
                    break;
                }
                m += 1;
            }
            if !fall_through_safe {
                move_coalesced.push(false);
                continue;
            }

            // Record the coalescing actions:
            // 1. NOP the initial copy (movq %dst, %src)
            copy_nop_lines.push(copy_idx);

            // 2. Rewrite each modification of %src to target %dst instead
            for &mod_idx in &modifications {
                coalesce_actions.push((mod_idx, src_fam, dst_fam));
            }

            move_coalesced.push(true);
        }

        // Stack-load coalescing is not attempted here because the store to a
        // stack slot may serve dual purposes (loop variable AND local variable
        // used on the fall-through path). Eliminating the store would break
        // non-trampoline paths that read from the same slot.
        // Only reg-reg coalescing is performed.
        let stack_coalesced: Vec<bool> = vec![false; tramp_stack_loads.len()];
        let stack_nop_lines: Vec<usize> = Vec::new();
        let stack_store_rewrites: Vec<(usize, String)> = Vec::new();

        // Count how many moves/stack-loads were coalesced
        let num_moves_coalesced = move_coalesced.iter().filter(|&&c| c).count();
        let num_stack_coalesced = stack_coalesced.iter().filter(|&&c| c).count();
        let total_coalesced = num_moves_coalesced + num_stack_coalesced;

        // Need at least one coalesced item to make progress
        if total_coalesced == 0 {
            continue;
        }

        let all_coalesced = num_moves_coalesced == tramp_moves.len()
            && num_stack_coalesced == tramp_stack_loads.len();

        // Apply the register-register coalescing actions
        for &nop_idx in &copy_nop_lines {
            mark_nop(&mut infos[nop_idx]);
        }

        for &(mod_idx, old_fam, new_fam) in &coalesce_actions {
            let old_line = infos[mod_idx].trimmed(store.get(mod_idx)).to_string();
            if let Some(new_line) = rewrite_instruction_register(&old_line, old_fam, new_fam) {
                replace_line(store, &mut infos[mod_idx], mod_idx, format!("    {}", new_line));
            }
        }

        // Apply stack-load coalescing: rewrite stores and NOP trampoline load+mov pairs
        for &(store_idx, ref new_line) in &stack_store_rewrites {
            replace_line(store, &mut infos[store_idx], store_idx, new_line.clone());
        }
        for &nop_idx in &stack_nop_lines {
            mark_nop(&mut infos[nop_idx]);
        }

        if all_coalesced {
            // All moves coalesced - NOP the entire trampoline and redirect branch
            for &line_idx in &tramp_all_lines {
                mark_nop(&mut infos[line_idx]);
            }
            mark_nop(&mut infos[tramp_label_idx]);

            // Redirect the conditional branch to the loop header
            let branch_trimmed = infos[branch_idx].trimmed(store.get(branch_idx)).to_string();
            if let Some(space_pos) = branch_trimmed.find(' ') {
                let cc = &branch_trimmed[..space_pos];
                let target_label = format!(".LBB{}", target_num);
                let new_branch = format!("    {} {}", cc, target_label);
                replace_line(store, &mut infos[branch_idx], branch_idx, new_branch);
            }
        } else {
            // Partial coalescing - NOP only the coalesced move lines from the
            // trampoline. The trampoline block remains for the uncoalesced moves.
            //
            // We need to figure out which trampoline lines correspond to coalesced
            // reg-reg moves vs. coalesced stack-load pairs.
            //
            // tramp_all_lines was built in parse order. We need to map each
            // trampoline line back to whether it was coalesced.
            //
            // Approach: rebuild the mapping by re-scanning the trampoline.
            // Reg-reg moves are at indices that match their position in tramp_moves.
            // Stack-load pairs occupy 2 lines each.
            // The jmp is always the last line in tramp_all_lines.
            //
            // Instead of complex index tracking, just identify lines to NOP:
            // For each coalesced reg-reg move, the corresponding trampoline line
            // is a movq/movslq with src_fam/dst_fam that we can match.
            for (idx, &(src_fam, dst_fam)) in tramp_moves.iter().enumerate() {
                if !move_coalesced[idx] { continue; }
                // Find and NOP the trampoline line for this move
                for &line_idx in &tramp_all_lines {
                    if infos[line_idx].is_nop() { continue; }
                    let trimmed = infos[line_idx].trimmed(store.get(line_idx));
                    // Check if this line is "movq %src, %dst" or "movslq %src, %dst"
                    let src_64 = REG_NAMES[0][src_fam as usize];
                    let dst_64 = REG_NAMES[0][dst_fam as usize];
                    let expected = format!("movq {}, {}", src_64, dst_64);
                    if trimmed == expected {
                        mark_nop(&mut infos[line_idx]);
                        break;
                    }
                    // Also check movslq
                    let src_32 = REG_NAMES[1][src_fam as usize];
                    let expected2 = format!("movslq {}, {}", src_32, dst_64);
                    if trimmed == expected2 {
                        mark_nop(&mut infos[line_idx]);
                        break;
                    }
                }
            }
            // Stack-load pairs that were coalesced already had their trampoline
            // lines (load_line, mov_line) NOPed via stack_nop_lines above.
        }

        changed = true;
    }

    changed
}

/// Rewrite an instruction to use a different register family.
/// For example, rewriting `addq $320, %r14` with old=r14, new=r9 gives `addq $320, %r9`.
fn rewrite_instruction_register(inst: &str, old_fam: RegId, new_fam: RegId) -> Option<String> {
    // Replace all occurrences of registers in the old family with the new family.
    // We need to handle all sizes: 64-bit (%rXX), 32-bit (%eXX/%rXXd), 16-bit, 8-bit.
    // Use replace_reg_name_exact (with word-boundary checking) to avoid corrupting
    // longer register names. For example, naive replacement of %r11 -> %rbx in
    // "%r11d" would produce "%rbxd" instead of the correct "%ebx".
    let result = replace_reg_family(inst, old_fam, new_fam);
    // Verify we actually changed something
    if result == inst {
        None
    } else {
        Some(result)
    }
}

#[cfg(test)]
fn is_self_move(s: &str) -> bool {
    if let Some(rest) = s.strip_prefix("movq ") {
        let rest = rest.trim();
        if let Some((src, dst)) = rest.split_once(',') {
            let src = src.trim();
            let dst = dst.trim();
            if src == dst && src.starts_with('%') {
                return true;
            }
        }
    }
    false
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_redundant_store_load() {
        let asm = "    movq %rax, -8(%rbp)\n    movq -8(%rbp), %rax\n".to_string();
        let result = peephole_optimize(asm);
        assert_eq!(result.trim(), "movq %rax, -8(%rbp)");
    }

    #[test]
    fn test_store_load_different_reg() {
        let asm = "    movq %rax, -8(%rbp)\n    movq -8(%rbp), %rcx\n".to_string();
        let result = peephole_optimize(asm);
        assert!(result.contains("movq %rax, -8(%rbp)"));
        assert!(result.contains("movq %rax, %rcx"));
        assert!(!result.contains("movq -8(%rbp), %rcx"));
    }

    #[test]
    fn test_redundant_jump() {
        let asm = "    jmp .Lfoo\n.Lfoo:\n".to_string();
        let result = peephole_optimize(asm);
        assert!(!result.contains("jmp"));
        assert!(result.contains(".Lfoo:"));
    }

    #[test]
    fn test_push_pop_elimination() {
        let asm = "    pushq %rax\n    movq %rax, %rcx\n    popq %rax\n".to_string();
        let result = peephole_optimize(asm);
        assert!(!result.contains("pushq"));
        assert!(!result.contains("popq"));
        assert!(result.contains("movq %rax, %rcx"));
    }

    #[test]
    fn test_self_move() {
        let asm = "    movq %rax, %rax\n".to_string();
        let result = peephole_optimize(asm);
        assert_eq!(result.trim(), "");
    }

    #[test]
    fn test_parse_store_to_rbp() {
        assert!(parse_store_to_rbp_str("movq %rax, -8(%rbp)").is_some());
        assert!(parse_store_to_rbp_str("movl %eax, -16(%rbp)").is_some());
        assert!(parse_store_to_rbp_str("movq $5, -8(%rbp)").is_none());
    }

    #[test]
    fn test_parse_load_from_rbp() {
        assert!(parse_load_from_rbp_str("movq -8(%rbp), %rax").is_some());
        assert!(parse_load_from_rbp_str("movslq -8(%rbp), %rax").is_some());
    }

    #[test]
    fn test_compare_branch_fusion_with_matched_store_load() {
        // When the sequence includes a store + matching load at the same
        // offset, the store-load pair is just a codegen roundtrip.  Fusion
        // IS safe because no other block reads the slot (the load is right
        // here in the same sequence).
        let asm = [
            "    cmpq %rcx, %rax",
            "    setl %al",
            "    movzbq %al, %rax",
            "    movq %rax, -24(%rbp)",
            "    movq -24(%rbp), %rax",
            "    testq %rax, %rax",
            "    jne .LBB2",
            "    jmp .LBB4",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("cmpq %rcx, %rax"), "should keep the cmp");
        // Matched store+load pair → fusion is safe
        assert!(result.contains("jl .LBB2"), "should fuse to jl: {}", result);
        assert!(!result.contains("setl"), "should eliminate setl");
    }

    #[test]
    fn test_compare_branch_fusion_short() {
        let asm = [
            "    cmpq %rcx, %rax",
            "    setl %al",
            "    movzbq %al, %rax",
            "    testq %rax, %rax",
            "    jne .LBB2",
            "    jmp .LBB4",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("jl .LBB2"), "should fuse to jl: {}", result);
        assert!(!result.contains("setl"), "should eliminate setl");
    }

    #[test]
    fn test_compare_branch_fusion_je() {
        let asm = [
            "    cmpq %rcx, %rax",
            "    setl %al",
            "    movzbq %al, %rax",
            "    testq %rax, %rax",
            "    je .Lfalse",
            "    jmp .Ltrue",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("jge .Lfalse"), "should fuse to jge: {}", result);
    }

    #[test]
    fn test_non_adjacent_store_load_same_reg() {
        let asm = [
            "    movq %rax, -24(%rbp)",
            "    movq %rcx, -32(%rbp)",
            "    movq -24(%rbp), %rax",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(!result.contains("-24(%rbp), %rax"), "should eliminate the load: {}", result);
    }

    #[test]
    fn test_non_adjacent_store_load_diff_reg() {
        let asm = [
            "    movq %rax, -24(%rbp)",
            "    movq %rcx, -32(%rbp)",
            "    movq -24(%rbp), %rdx",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("movq %rax, %rdx"), "should forward to reg-reg: {}", result);
    }

    #[test]
    fn test_non_adjacent_store_load_reg_modified() {
        let asm = [
            "    movq %rax, -24(%rbp)",
            "    movq -32(%rbp), %rax",
            "    movq -24(%rbp), %rcx",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("-24(%rbp), %rcx") || result.contains("%rax, %rcx"),
            "should not forward since rax was modified: {}", result);
    }

    #[test]
    fn test_redundant_cltq() {
        let asm = "    movslq -8(%rbp), %rax\n    cltq\n".to_string();
        let result = peephole_optimize(asm);
        assert!(result.contains("movslq"), "should keep movslq");
        assert!(!result.contains("cltq"), "should eliminate redundant cltq: {}", result);
    }

    #[test]
    fn test_dead_store_elimination() {
        let asm = [
            "    movq %rax, -24(%rbp)",
            "    movq %rcx, -24(%rbp)",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(!result.contains("%rax, -24(%rbp)"), "first store should be dead: {}", result);
        assert!(result.contains("%rcx, -24(%rbp)"), "second store should remain: {}", result);
    }

    #[test]
    fn test_condition_codes() {
        for (cc, expected_jcc) in &[("e", "je"), ("ne", "jne"), ("l", "jl"), ("g", "jg"),
                                     ("le", "jle"), ("ge", "jge"), ("b", "jb"), ("a", "ja")] {
            let asm = format!(
                "    cmpq %rcx, %rax\n    set{} %al\n    movzbq %al, %rax\n    testq %rax, %rax\n    jne .LBB1\n",
                cc
            );
            let result = peephole_optimize(asm);
            assert!(result.contains(&format!("{} .LBB1", expected_jcc)),
                "cc={} should produce {}: {}", cc, expected_jcc, result);
        }
    }

    #[test]
    fn test_global_store_forward_across_fallthrough_label() {
        // Store before a fallthrough-only label should be forwarded to a load after it
        let asm = [
            "    movq %rax, -24(%rbp)",
            "    movq %rcx, -32(%rbp)",
            ".Lfallthrough:",                  // Not a jump target, only fallthrough
            "    movq -24(%rbp), %rax",        // Should be eliminated (same reg)
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(!result.contains("-24(%rbp), %rax"),
            "should forward across fallthrough label: {}", result);
    }

    #[test]
    fn test_global_store_forward_blocked_at_jump_target() {
        // Store before a label that is a jump target should NOT be forwarded
        let asm = [
            "    movq %rax, -24(%rbp)",
            "    jmp .Lskip",
            ".Ltarget:",                       // This label is jumped to
            "    movq -24(%rbp), %rax",        // Should NOT be eliminated
            ".Lskip:",
            "    ret",
            "    jmp .Ltarget",                // Jump to .Ltarget
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        // The load at .Ltarget should remain because .Ltarget has unknown predecessor state
        assert!(result.contains("-24(%rbp), %rax") || result.contains("-24(%rbp),"),
            "should NOT forward across jump target: {}", result);
    }

    #[test]
    fn test_global_store_forward_across_cond_branch() {
        // Store before a conditional branch should be forwarded on the fallthrough path
        let asm = [
            "    movq %rax, -24(%rbp)",
            "    cmpq %rcx, %rax",
            "    jne .Lother",
            "    movq -24(%rbp), %rdx",        // Fallthrough: should get forwarded
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("movq %rax, %rdx"),
            "should forward on fallthrough after cond branch: {}", result);
    }

    #[test]
    fn test_global_store_forward_invalidated_by_call() {
        // Stores should be invalidated across function calls (caller-saved regs)
        let asm = [
            "    movq %rax, -24(%rbp)",
            "    callq some_func",
            "    movq -24(%rbp), %rax",        // Should NOT be eliminated (rax clobbered)
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("-24(%rbp), %rax"),
            "should not forward across call (rax clobbered): {}", result);
    }

    #[test]
    fn test_global_store_forward_callee_saved_across_call() {
        // Callee-saved registers (rbx, r12-r15) should survive across calls
        let asm = [
            "    movq %rbx, -24(%rbp)",
            "    callq some_func",
            "    movq -24(%rbp), %rbx",        // Should be eliminated (rbx is callee-saved)
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(!result.contains("-24(%rbp), %rbx"),
            "should forward callee-saved reg across call: {}", result);
    }

    #[test]
    fn test_global_store_forward_invalidated_by_unrecognized_rbp_write() {
        // Unrecognized instructions (e.g. inline asm) writing to a %rbp-relative
        // slot must invalidate the mapping so the subsequent load is kept.
        let asm = [
            "    movl %eax, -8(%rbp)",
            "    movntil %ecx, -8(%rbp)",    // inline asm: unrecognized write to slot
            "    movl -8(%rbp), %eax",       // must NOT be eliminated
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("-8(%rbp), %eax"),
            "must not eliminate load after unrecognized write to same slot: {}", result);
    }

    #[test]
    fn test_classify_line() {
        let info = classify_line("    movq %rax, -8(%rbp)");
        assert!(matches!(info.kind, LineKind::StoreRbp { reg: 0, offset: -8, size: MoveSize::Q }));

        let info = classify_line("    movq -16(%rbp), %rcx");
        assert!(matches!(info.kind, LineKind::LoadRbp { reg: 1, offset: -16, size: MoveSize::Q }));

        let info = classify_line(".Lfoo:");
        assert_eq!(info.kind, LineKind::Label);

        let info = classify_line("    jmp .LBB1");
        assert_eq!(info.kind, LineKind::Jmp);

        let info = classify_line("    ret");
        assert_eq!(info.kind, LineKind::Ret);
    }

    #[test]
    fn test_parse_rbp_offset() {
        // Standard negative offset
        assert_eq!(parse_rbp_offset("leaq -24(%rbp), %rax"), -24);
        // Bare (%rbp) with no numeric prefix
        assert_eq!(parse_rbp_offset("addq (%rbp), %rax"), 0);
        // Positive offset
        assert_eq!(parse_rbp_offset("movq 16(%rbp), %rdx"), 16);
        // No (%rbp) reference at all
        assert_eq!(parse_rbp_offset("movq %rax, %rcx"), RBP_OFFSET_NONE);
        // Two different rbp offsets -> NONE (can't pre-classify)
        assert_eq!(parse_rbp_offset("movq -8(%rbp), -16(%rbp)"), RBP_OFFSET_NONE);
        // Two identical rbp offsets -> that offset
        assert_eq!(parse_rbp_offset("addq -8(%rbp), -8(%rbp)"), -8);
    }

    #[test]
    fn test_compare_branch_fusion_no_fuse_cross_block_store() {
        // When the Cmp result is stored to a stack slot that is NOT loaded back
        // in the same sequence (i.e. it's read by a different basic block),
        // fusion must be suppressed.  Otherwise the setCC+movzbq that compute
        // the boolean value are deleted and the store either disappears or
        // writes the wrong data, leaving the cross-block read uninitialised.
        let asm = [
            "    cmpq $0, %rbx",
            "    sete %al",
            "    movzbq %al, %rax",
            "    movq %rax, -40(%rbp)",   // store for cross-block use (no matching load)
            "    testq %rax, %rax",
            "    jne .LBB8",
            "    jmp .LBB10",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        // The store must survive: another block reads -40(%rbp).
        assert!(result.contains("-40(%rbp)"),
            "must preserve cross-block store: {}", result);
        // setCC and movzbq must also survive since the store needs the boolean value.
        assert!(result.contains("sete"),
            "must preserve sete for cross-block store: {}", result);
    }

    #[test]
    fn test_jmp_star_reg_classified_as_indirect() {
        // `jmp *%rcx` is an indirect jump (used by switch jump tables).
        // All labels must be treated as jump targets when indirect jumps exist,
        // because the jump table in .rodata references labels that the peephole
        // scanner cannot see.
        let asm = [
            "    movq %rax, -40(%rbp)",
            "    jmp *%rcx",                   // indirect jump via register
            ".LBB21:",                          // jump table target
            "    movq -40(%rbp), %rax",        // MUST NOT be eliminated
            "    movq %rax, -160(%rbp)",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("-40(%rbp), %rax"),
            "must NOT eliminate load after indirect jump target label: {}", result);
    }

    #[test]
    fn test_jmpq_star_reg_classified_as_indirect() {
        // `jmpq *%rax` is also an indirect jump (AT&T syntax with size suffix)
        let asm = [
            "    movq %rax, -40(%rbp)",
            "    jmpq *%rax",
            ".LBB5:",
            "    movq -40(%rbp), %rax",
            "    ret",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("-40(%rbp), %rax"),
            "must NOT eliminate load after jmpq* indirect jump target: {}", result);
    }

    #[test]
    fn test_inline_asm_rdmsr_invalidates_store_forwarding() {
        // Inline asm "1: rdmsr ; xor %esi,%esi" clobbers %eax and %edx
        // (rdmsr writes eax:edx). The peephole optimizer must not forward
        // a stale %rax value across this inline asm line.
        // This is the pattern from kernel arch/x86/kernel/apic/apic.c
        // (native_read_msr_safe) that caused a boot hang.
        let asm = [
            "    leaq -16(%rbp), %rax",           // rax = &err
            "    movq %rax, -40(%rbp)",            // save ptr to stack slot
            "    movabsq $27, %rcx",               // msr number
            "    1: rdmsr ; xor %esi,%esi",        // inline asm: clobbers rax, rdx
            "    pushq %rcx",
            "    movq -40(%rbp), %rcx",            // MUST load from stack, NOT forward rax
            "    movl %esi, (%rcx)",               // store err through pointer
            "    popq %rcx",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        // The load from -40(%rbp) must NOT be replaced with "movq %rax, %rcx"
        // because rax was clobbered by rdmsr.
        assert!(result.contains("-40(%rbp), %rcx"),
            "must NOT forward rax across rdmsr (rax clobbered by inline asm): {}", result);
    }

    #[test]
    fn test_semicolon_multi_instruction_invalidates_mappings() {
        // Lines with ';' (statement separator) contain multiple instructions.
        // The peephole optimizer can't safely analyze all clobbers.
        let asm = [
            "    movq %rax, -24(%rbp)",
            "    xorl %eax, %eax ; movl $1, %ecx",  // multi-instr: clobbers rax, ecx
            "    movq -24(%rbp), %rax",              // must NOT be eliminated
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("-24(%rbp), %rax"),
            "must not forward across multi-instruction line with ';': {}", result);
    }

    #[test]
    fn test_rdmsr_standalone_invalidates_mappings() {
        // Standalone rdmsr instruction clobbers eax and edx implicitly.
        let asm = [
            "    movq %rax, -24(%rbp)",
            "    rdmsr",
            "    movq -24(%rbp), %rax",   // must NOT be eliminated
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("-24(%rbp), %rax"),
            "must not forward across rdmsr (implicit clobber of rax/rdx): {}", result);
    }

    #[test]
    fn test_cpuid_invalidates_mappings() {
        // cpuid clobbers eax, ebx, ecx, edx.
        let asm = [
            "    movq %rax, -24(%rbp)",
            "    cpuid",
            "    movq -24(%rbp), %rax",   // must NOT be eliminated
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("-24(%rbp), %rax"),
            "must not forward across cpuid (implicit clobber of rax/rbx/rcx/rdx): {}", result);
    }

    #[test]
    fn test_setcc_non_al_invalidates_store_forwarding() {
        // setCC can write to any byte register (not just %al).
        // When inline asm emits "sete %cl", it clobbers %ecx/%rcx (family 1).
        // The store forwarding pass must not forward a stale %rcx value
        // across such a setCC instruction.
        let asm = [
            "    movl %ecx, -8(%rbp)",     // store from ecx to stack slot
            "    sete %cl",                // clobbers ecx (low byte)
            "    movl -8(%rbp), %eax",     // must load from stack, NOT forward ecx
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("-8(%rbp), %eax"),
            "must NOT forward ecx across sete %%cl (ecx clobbered): {}", result);
    }

    #[test]
    fn test_setcc_al_still_invalidates_rax() {
        // Verify that setCC writing to %al still correctly invalidates rax.
        let asm = [
            "    movq %rax, -16(%rbp)",    // store from rax to stack slot
            "    sete %al",                // clobbers rax (low byte)
            "    movq -16(%rbp), %rax",    // must load from stack, NOT eliminate
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("-16(%rbp), %rax"),
            "must NOT forward rax across sete %%al (rax clobbered): {}", result);
    }

    #[test]
    fn test_syscall_invalidates_mappings() {
        // syscall clobbers rcx (return RIP) and r11 (saved RFLAGS).
        // Store forwarding must not forward rcx across a syscall instruction.
        let asm = [
            "    movq %rcx, -16(%rbp)",    // store rcx to stack slot
            "    syscall",                 // clobbers rcx and r11
            "    movq -16(%rbp), %rcx",    // must load from stack, NOT forward
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("-16(%rbp), %rcx"),
            "must NOT forward rcx across syscall (rcx clobbered): {}", result);
    }

    #[test]
    fn test_loop_trampoline_simple_coalesce() {
        // Simple loop trampoline: one register copy gets coalesced.
        // Before: copy %r9 to %r14, modify %r14, trampoline copies %r14 back to %r9
        // After: modify %r9 directly, branch to loop header directly
        let asm = [
            ".LBB1:",                       // loop header
            "    movq %r9, %rax",           // use r9 in loop body
            "    movq %r9, %r14",           // copy to new reg
            "    addq $320, %r14",          // modify new reg
            "    testq %rax, %rax",         // loop condition
            "    jne .LBB2",               // branch to trampoline
            "    ret",                     // loop exit
            ".LBB2:",                       // trampoline
            "    movq %r14, %r9",           // shuffle back
            "    jmp .LBB1",               // back to loop header
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        // The trampoline should be eliminated.
        // %r14 should be replaced with %r9 (in-place update).
        assert!(result.contains("addq $320, %r9"),
            "should rewrite addq to target %r9 directly: {}", result);
        assert!(!result.contains("movq %r9, %r14"),
            "should eliminate the initial copy: {}", result);
        assert!(!result.contains("movq %r14, %r9"),
            "should eliminate the trampoline copy: {}", result);
        // Branch should go directly to loop header
        assert!(result.contains("jne .LBB1"),
            "should redirect branch to loop header: {}", result);
    }

    #[test]
    fn test_loop_trampoline_two_copies() {
        // Two register copies in the trampoline (common in DOOM rendering loops).
        let asm = [
            ".LBB10:",                      // loop header
            "    movq %r9, %rax",           // use r9
            "    movq %r10, %rcx",          // use r10
            "    movq %r9, %r14",           // copy dest to new reg
            "    addq $320, %r14",          // modify new dest
            "    movq %r10, %r15",          // copy frac to new reg
            "    addl %r8d, %r15d",         // modify new frac
            "    testq %rax, %rax",         // loop condition
            "    jne .LBB20",              // branch to trampoline
            "    ret",
            ".LBB20:",                      // trampoline
            "    movq %r14, %r9",           // shuffle dest back
            "    movq %r15, %r10",          // shuffle frac back
            "    jmp .LBB10",              // back to loop header
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        // Both copies should be coalesced
        assert!(result.contains("addq $320, %r9"),
            "should rewrite dest addq to %r9: {}", result);
        assert!(result.contains("addl %r8d, %r10d"),
            "should rewrite frac addl to %r10d: {}", result);
        assert!(!result.contains("movq %r9, %r14"),
            "should eliminate dest copy: {}", result);
        assert!(!result.contains("movq %r10, %r15"),
            "should eliminate frac copy: {}", result);
        assert!(result.contains("jne .LBB10"),
            "should redirect branch to loop header: {}", result);
    }

    #[test]
    fn test_condbranch_inversion_fallthrough() {
        // Pattern: jl .LBB2 ; jmp .LBB4 ; .LBB2:
        // Should become: jge .LBB4 ; .LBB2:
        let asm = [
            "    cmpl %r8d, %eax",
            "    jl .LBB2",
            "    jmp .LBB4",
            ".LBB2:",
            "    movq %rax, %rcx",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        // The jl should be inverted to jge, and jmp removed
        assert!(result.contains("jge .LBB4"),
            "should invert jl to jge: {}", result);
        assert!(!result.contains("jl .LBB2"),
            "should remove original jl: {}", result);
        assert!(!result.contains("jmp .LBB4"),
            "should remove the jmp: {}", result);
        assert!(result.contains(".LBB2:"),
            "should keep the label: {}", result);
    }

    #[test]
    fn test_condbranch_inversion_je_to_jne() {
        // Pattern: je .Ltrue ; jmp .Lfalse ; .Ltrue:
        // Should become: jne .Lfalse ; .Ltrue:
        let asm = [
            "    testq %rax, %rax",
            "    je .Ltrue",
            "    jmp .Lfalse",
            ".Ltrue:",
            "    ret",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("jne .Lfalse"),
            "should invert je to jne: {}", result);
        assert!(!result.contains("jmp .Lfalse"),
            "should remove the jmp: {}", result);
    }

    #[test]
    fn test_condbranch_no_inversion_when_not_fallthrough() {
        // Pattern: jl .LBB5 ; jmp .LBB4 ; .LBB2:
        // NOT a fallthrough (label is .LBB2, not .LBB5) -- should NOT invert
        let asm = [
            "    cmpl %r8d, %eax",
            "    jl .LBB5",
            "    jmp .LBB4",
            ".LBB2:",
            "    movq %rax, %rcx",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        // Should keep both jumps (possibly the jmp .LBB4 is eliminated if .LBB4
        // happens to be next, but jl .LBB5 should remain)
        assert!(result.contains("jl .LBB5"),
            "should keep jl when not fallthrough: {}", result);
    }

    #[test]
    fn test_back_to_back_cltq() {
        // cltq is idempotent; back-to-back cltq;cltq should eliminate the second
        let asm = "    cltq\n    cltq\n".to_string();
        let result = peephole_optimize(asm);
        assert_eq!(result.matches("cltq").count(), 1,
            "should keep only one cltq: {}", result);
    }

    #[test]
    fn test_cltq_backward_scan_over_non_rax_write() {
        // Pattern: movslq ..., %rax; movq %rax, %r8; cltq
        // The cltq is redundant because movslq already sign-extended %rax,
        // and movq %rax, %r8 doesn't modify %rax.
        let asm = [
            "    movslq -8(%rbp), %rax",
            "    movq %rax, %r8",
            "    cltq",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(!result.contains("cltq"),
            "cltq should be eliminated after movslq past non-rax-write: {}", result);
    }

    #[test]
    fn test_cltq_backward_scan_blocked_by_rax_write() {
        // Pattern: movslq ..., %rax; addl $1, %eax; cltq
        // The cltq is NOT redundant because addl writes to %eax, invalidating
        // the sign-extension from movslq.
        let asm = [
            "    movslq -8(%rbp), %rax",
            "    addl $1, %eax",
            "    cltq",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("cltq"),
            "cltq should NOT be eliminated when rax is modified: {}", result);
    }

    #[test]
    fn test_cltq_backward_scan_blocked_by_call() {
        // Pattern: cltq; call foo; cltq
        // The second cltq is NOT redundant because call clobbers %rax.
        let asm = [
            "    cltq",
            "    call foo",
            "    cltq",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert_eq!(result.matches("cltq").count(), 2,
            "both cltq should survive when call intervenes: {}", result);
    }

    #[test]
    fn test_cltq_backward_scan_with_store_rbp() {
        // Pattern: cltq; movq %rax, -16(%rbp); movq %rax, %r9; cltq
        // The second cltq is redundant: store-to-rbp and mov-to-r9 don't clobber %rax.
        let asm = [
            "    cltq",
            "    movq %rax, -16(%rbp)",
            "    movq %rax, %r9",
            "    cltq",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert_eq!(result.matches("cltq").count(), 1,
            "second cltq should be eliminated past store and mov: {}", result);
    }
}
