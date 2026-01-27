//! x86-64 peephole optimizer: optimization passes.
//!
//! Contains the main entry point (`peephole_optimize`) and all optimization
//! passes: local pattern matching, push/pop elimination, compare-and-branch
//! fusion, dead store elimination, global store forwarding, and unused
//! callee-save register elimination.

use super::types::*;

// ── Main entry point ─────────────────────────────────────────────────────────

/// Run peephole optimization on x86-64 assembly text.
/// Returns the optimized assembly string.
///
/// Pass structure for speed:
/// 1. Run cheap local passes iteratively until convergence (max 8 iterations).
///    These are O(n) single-scan passes that only look at adjacent/nearby lines.
/// 2. Run expensive global passes once. `global_store_forwarding` is O(n) but with
///    higher constant factor due to tracking slot→register mappings. It subsumes
///    the functionality of local store-load forwarding across wider windows.
/// 3. Run local passes one more time to clean up opportunities exposed by the
///    global passes (e.g., dead stores from forwarded loads).
pub fn peephole_optimize(asm: String) -> String {
    let mut store = LineStore::new(asm);
    let line_count = store.len();
    let mut infos: Vec<LineInfo> = (0..line_count).map(|i| classify_line(store.get(i))).collect();

    // Phase 1: Iterative cheap local passes.
    // Run combined_local_pass first; if it makes changes, also run push/pop passes.
    // The push/pop passes are only useful when the combined pass has removed or
    // modified instructions, exposing new push/pop pair opportunities.
    // 8 iterations max: local patterns rarely chain deeper than 3-4 levels.
    let mut changed = true;
    let mut pass_count = 0;
    while changed && pass_count < 8 {
        changed = false;
        let local_changed = combined_local_pass(&store, &mut infos);
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
    let global_changed = global_changed | eliminate_dead_stores(&store, &mut infos);
    let global_changed = global_changed | fuse_compare_and_branch(&mut store, &mut infos);

    // Phase 3: One more local cleanup if global passes made changes.
    // 4 iterations: cleanup after globals is shallow (mostly dead store + adjacent pairs).
    if global_changed {
        let mut changed2 = true;
        let mut pass_count2 = 0;
        while changed2 && pass_count2 < 4 {
            changed2 = false;
            changed2 |= combined_local_pass(&store, &mut infos);
            changed2 |= fuse_movq_ext_truncation(&mut store, &mut infos);
            changed2 |= eliminate_dead_stores(&store, &mut infos);
            pass_count2 += 1;
        }
    }

    // Phase 4: Eliminate unused callee-saved register saves/restores.
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

fn combined_local_pass(store: &LineStore, infos: &mut [LineInfo]) -> bool {
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

        // --- Pattern: adjacent store/load at same %rbp offset ---
        // Note: this pattern does NOT replace lines, it only NOPs or delegates
        // to the store-forwarding global pass for reg-to-reg moves.
        if let LineKind::StoreRbp { reg: sr, offset: so, size: ss } = infos[i].kind {
            if i + 1 < len && !infos[i + 1].is_nop() {
                if let LineKind::LoadRbp { reg: lr, offset: lo, size: ls } = infos[i + 1].kind {
                    if so == lo && ss == ls {
                        if sr == lr {
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
                    ExtKind::MovslqEaxRax),
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
            _ => unreachable!(),
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
        LineKind::SetCC => reg_id == 0, // setCC writes %al (rax family)
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

        // Collect next non-NOP lines (up to 8): cmp itself + 7 following
        let mut seq_indices = [0usize; 8];
        seq_indices[0] = i;
        let mut rest = [0usize; 7];
        let rest_count = collect_non_nop_indices::<7>(infos, i, len, &mut rest);
        for k in 0..rest_count { seq_indices[k + 1] = rest[k]; }
        let seq_count = 1 + rest_count;

        if seq_count < 4 {
            i += 1;
            continue;
        }

        // Second must be setCC
        if infos[seq_indices[1]].kind != LineKind::SetCC {
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
        // At most 2 stores can appear between setCC and testq in practice
        // (the Cmp result store + possibly a sign-extension store).  Use 4
        // as a comfortable limit; if exceeded, conservatively bail out.
        let mut store_offsets: [i32; 4] = [0; 4];
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
                if store_count < 4 {
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
            let mut load_offsets: [i32; 4] = [0; 4];
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
                    if load_count < 4 { load_offsets[load_count] = o; load_count += 1; }
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
                // SAFETY: write_rbp_pattern only writes ASCII digits, '-', and "(%rbp)" bytes.
                let pattern = unsafe { std::str::from_utf8_unchecked(&pattern_bytes[..pattern_len]) };
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
                        // SAFETY: write_rbp_pattern only writes ASCII digits, '-', and "(%rbp)" bytes.
                        let check_pattern = unsafe { std::str::from_utf8_unchecked(&pattern_bytes[..check_len]) };
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

    // Phase 1: Collect all jump/branch targets using a flat Vec<bool> indexed by
    // label suffix number (e.g., ".L123" -> index 123). This avoids HashSet heap
    // allocation for the common case of numeric labels.
    // For non-numeric labels, we fall back to treating them as jump targets.
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
    // If the function contains any indirect jumps (computed goto), every label
    // is a potential jump target. We can't statically determine which labels
    // the indirect jump may reach, so conservatively mark them all.
    if has_indirect_jump {
        for v in is_jump_target.iter_mut() {
            *v = true;
        }
        has_non_numeric_jump_targets = true;
    }

    // Phase 2: Forward scan with slot→register mappings.
    // Uses a flat Vec of SlotEntry (linear scan) instead of HashMap for the
    // small number of active slots per basic block. Each entry records a stack
    // offset, the register that holds the value, and the line index of the store.
    // Inactive entries are left in the Vec (marked active=false) to avoid
    // expensive removal; reverse iteration finds the most recent active mapping.
    let mut slot_entries: Vec<SlotEntry> = Vec::new();
    // Track which registers currently hold "valid" values (i.e., haven't been
    // clobbered since the store). reg_offsets[reg_id] = list of slot_entries indices
    let mut reg_offsets: [SmallVec; 16] = Default::default();
    let mut changed = false;

    // Track if the previous non-nop instruction was a jump (meaning the current
    // label is only reachable from the jump, not fallthrough)
    let mut prev_was_unconditional_jump = false;

    for i in 0..len {
        if infos[i].is_nop() || infos[i].kind == LineKind::Empty {
            continue;
        }

        match infos[i].kind {
            LineKind::Label => {
                let label_name = infos[i].trimmed(store.get(i));
                // Check if this label is a jump target
                let is_target = if let Some(n) = parse_label_number(label_name) {
                    (n as usize) < is_jump_target.len() && is_jump_target[n as usize]
                } else {
                    // Non-numeric label: conservative - treat as jump target
                    // if any non-numeric jump targets exist
                    has_non_numeric_jump_targets
                };

                if is_target || prev_was_unconditional_jump {
                    // This label has multiple predecessors (jump target) or follows
                    // unreachable code (after unconditional jump). We can't know what
                    // register state the jumping block leaves, so invalidate all mappings.
                    invalidate_all_mappings(&mut slot_entries, &mut reg_offsets);
                }
                // Fallthrough-only labels keep mappings intact.
                prev_was_unconditional_jump = false;
            }

            LineKind::StoreRbp { reg, offset, size } => {
                // A store updates the mapping for this slot.
                // Invalidate any existing mapping whose byte range overlaps with
                // this store. A store of N bytes at `offset` covers the range
                // [offset, offset + N). Any active mapping at `e.offset` with
                // size `e.mapping.size` covers [e.offset, e.offset + e_bytes).
                let store_bytes = size.byte_size();
                for entry in slot_entries.iter_mut().filter(|e| e.active) {
                    if ranges_overlap(offset, store_bytes, entry.offset, entry.mapping.size.byte_size()) {
                        let old_reg = entry.mapping.reg_id;
                        entry.active = false;
                        reg_offsets[old_reg as usize].remove_val(entry.offset);
                    }
                }
                // Record the new mapping.
                if reg != REG_NONE {
                    slot_entries.push(SlotEntry {
                        offset,
                        mapping: SlotMapping { reg_id: reg, size },
                        active: true,
                    });
                    reg_offsets[reg as usize].push(offset);
                }
                // Compact: remove inactive entries when the vec grows beyond 64.
                // A typical basic block has 5-20 active slot mappings, so 64 gives
                // ample headroom before compaction triggers. This keeps reverse-
                // iteration scans O(active_count) rather than O(total_ever_inserted).
                if slot_entries.len() > 64 {
                    slot_entries.retain(|e| e.active);
                }
                prev_was_unconditional_jump = false;
            }

            LineKind::LoadRbp { reg: load_reg, offset: load_offset, size: load_size } => {
                // Check if we have a mapping for this slot. Iterate in reverse
                // so we find the most recently added (and thus current) mapping.
                let mapping = slot_entries.iter().rev()
                    .find(|e| e.active && e.offset == load_offset)
                    .map(|e| e.mapping);
                if let Some(mapping) = mapping {
                    if mapping.size == load_size && mapping.reg_id != REG_NONE {
                        // Don't eliminate callee-save register restores in the epilogue.
                        // objtool (Linux kernel) validates that all callee-save registers
                        // saved in the prologue are properly restored before return.
                        // Removing these "redundant" restores breaks objtool validation.
                        let is_epilogue_restore = matches!(load_reg, 3 | 12 | 13 | 14 | 15)
                            && load_offset < 0
                            && is_near_epilogue(&infos, i);
                        if load_reg == mapping.reg_id && !is_epilogue_restore {
                            // Same register: load is redundant
                            mark_nop(&mut infos[i]);
                            changed = true;
                        } else if load_reg != REG_NONE && load_reg != mapping.reg_id {
                            // Different register: replace with reg-to-reg move
                            // Use reg_id_to_name to avoid re-parsing the store line
                            let store_reg_str = reg_id_to_name(mapping.reg_id, load_size);
                            let load_reg_str = reg_id_to_name(load_reg, load_size);
                            let new_text = format!("    {} {}, {}",
                                load_size.mnemonic(), store_reg_str, load_reg_str);
                            replace_line(store, &mut infos[i], i, new_text);
                            changed = true;
                        }
                    }
                }

                if load_reg != REG_NONE {
                    invalidate_reg_flat(&mut slot_entries, &mut reg_offsets, load_reg);
                }
                prev_was_unconditional_jump = false;
            }

            LineKind::Jmp | LineKind::JmpIndirect => {
                // After an unconditional jump, execution continues at the target.
                // We can't know the state at the target (it may have other
                // predecessors), so clear everything.
                invalidate_all_mappings(&mut slot_entries, &mut reg_offsets);
                prev_was_unconditional_jump = true;
            }

            LineKind::CondJmp => {
                // Conditional jumps don't modify registers; they just branch.
                // The register state on the fallthrough path is unchanged.
                prev_was_unconditional_jump = false;
            }

            LineKind::Call => {
                // Function calls clobber caller-saved registers:
                // rax(0), rcx(1), rdx(2), rsi(6), rdi(7), r8(8), r9(9), r10(10), r11(11)
                for &r in &[0u8, 1, 2, 6, 7, 8, 9, 10, 11] {
                    invalidate_reg_flat(&mut slot_entries, &mut reg_offsets, r);
                }
                prev_was_unconditional_jump = false;
            }

            LineKind::Ret => {
                // End of function; clear everything.
                invalidate_all_mappings(&mut slot_entries, &mut reg_offsets);
                prev_was_unconditional_jump = true;
            }

            LineKind::Push { .. } => {
                // push reads a register, doesn't modify it
                prev_was_unconditional_jump = false;
            }

            LineKind::Pop { reg } => {
                if reg != REG_NONE {
                    invalidate_reg_flat(&mut slot_entries, &mut reg_offsets, reg);
                }
                prev_was_unconditional_jump = false;
            }

            LineKind::Directive => {
                // Don't update prev_was_unconditional_jump.
            }

            LineKind::Other { dest_reg } => {
                // Use pre-parsed destination register for fast invalidation.
                if dest_reg != REG_NONE {
                    invalidate_reg_flat(&mut slot_entries, &mut reg_offsets, dest_reg);
                    // Some instructions with dest_reg=0 (rax) also clobber rdx (family 2):
                    // - div/idiv/mul: produce quotient in rax, remainder in rdx
                    // - cqto/cqo: sign-extend rax into rdx:rax
                    // - cdq: sign-extend eax into edx:eax
                    // parse_dest_reg_fast returns 0 (rax) for all of these.
                    if dest_reg == 0 {
                        let trimmed = infos[i].trimmed(store.get(i));
                        if trimmed.starts_with("div") || trimmed.starts_with("idiv")
                            || trimmed.starts_with("mul")
                            || trimmed == "cqto" || trimmed == "cqo" || trimmed == "cdq"
                        {
                            invalidate_reg_flat(&mut slot_entries, &mut reg_offsets, 2);
                        }
                    }
                }
                // Read-modify-write instructions with a memory destination at a
                // (%rbp) offset (e.g., `orl $0x40, -8(%rbp)`) modify the stack
                // slot without having a register destination. Invalidate the slot
                // mapping so subsequent loads aren't incorrectly eliminated.
                if dest_reg == REG_NONE && infos[i].rbp_offset != RBP_OFFSET_NONE {
                    let rmw_offset = infos[i].rbp_offset;
                    for entry in slot_entries.iter_mut().filter(|e| e.active) {
                        if entry.offset == rmw_offset {
                            let old_reg = entry.mapping.reg_id;
                            entry.active = false;
                            reg_offsets[old_reg as usize].remove_val(entry.offset);
                        }
                    }
                }
                // Use cached has_indirect_mem flag instead of re-scanning the line.
                if infos[i].has_indirect_mem {
                    invalidate_all_mappings(&mut slot_entries, &mut reg_offsets);
                } else {
                    // Instructions like inline asm (e.g. `movntiq %rcx, -8(%rbp)`)
                    // may write to a stack slot via %rbp without being classified as
                    // StoreRbp. If the line has a known rbp_offset, conservatively
                    // invalidate any slot mapping at that offset so we don't forward
                    // a stale register value through a subsequent load.
                    let rbp_off = infos[i].rbp_offset;
                    if rbp_off != RBP_OFFSET_NONE {
                        for entry in slot_entries.iter_mut().filter(|e| e.active) {
                            // We don't know the access size for Other instructions,
                            // so conservatively check if this offset falls anywhere
                            // in the mapped slot's byte range.
                            let e_bytes = entry.mapping.size.byte_size();
                            if ranges_overlap(rbp_off, 1, entry.offset, e_bytes) {
                                let old_reg = entry.mapping.reg_id;
                                entry.active = false;
                                reg_offsets[old_reg as usize].remove_val(entry.offset);
                            }
                        }
                    }
                }
                // Unrecognized instructions (e.g. inline asm) that reference a
                // %rbp-relative slot may write to it.  Invalidate any mapping
                // for that slot so later loads are not incorrectly eliminated.
                let rbp_off = infos[i].rbp_offset;
                if rbp_off != RBP_OFFSET_NONE {
                    for entry in slot_entries.iter_mut().filter(|e| e.active) {
                        if entry.offset == rbp_off {
                            let old_reg = entry.mapping.reg_id;
                            entry.active = false;
                            reg_offsets[old_reg as usize].remove_val(rbp_off);
                        }
                    }
                }
                prev_was_unconditional_jump = false;
            }

            LineKind::SetCC => {
                // setCC writes to %al, which is part of rax (family 0).
                invalidate_reg_flat(&mut slot_entries, &mut reg_offsets, 0);
                prev_was_unconditional_jump = false;
            }

            LineKind::Cmp => {
                // cmp/test don't modify registers, only flags.
                prev_was_unconditional_jump = false;
            }

            _ => {
                prev_was_unconditional_jump = false;
            }
        }
    }

    changed
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
#[derive(Clone)]
struct SmallVec {
    inline: [i32; 4],
    len: u8,
    overflow: Option<Vec<i32>>,
}

impl Default for SmallVec {
    fn default() -> Self {
        SmallVec { inline: [0; 4], len: 0, overflow: None }
    }
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
        } else {
            if self.idx < self.sv.len as usize {
                let v = self.sv.inline[self.idx];
                self.idx += 1;
                Some(v)
            } else {
                None
            }
        }
    }
}

/// Parse ".L<number>:" label into its number, e.g. ".L123:" -> Some(123)
#[inline]
fn parse_label_number(label_with_colon: &str) -> Option<u32> {
    let s = label_with_colon.strip_suffix(':')?;
    parse_dotl_number(s)
}

/// Parse ".L<number>" into its number, e.g. ".L123" -> Some(123)
#[inline]
fn parse_dotl_number(s: &str) -> Option<u32> {
    let rest = s.strip_prefix(".L")?;
    let b = rest.as_bytes();
    if b.is_empty() || b[0] < b'0' || b[0] > b'9' {
        return None;
    }
    let mut v: u32 = 0;
    for &c in b {
        if c >= b'0' && c <= b'9' {
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
    // Handle: jmp .L1, je .L1, jne .L1, etc.
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
            "    jne .L2",
            "    jmp .L4",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("cmpq %rcx, %rax"), "should keep the cmp");
        // Matched store+load pair → fusion is safe
        assert!(result.contains("jl .L2"), "should fuse to jl: {}", result);
        assert!(!result.contains("setl"), "should eliminate setl");
    }

    #[test]
    fn test_compare_branch_fusion_short() {
        let asm = [
            "    cmpq %rcx, %rax",
            "    setl %al",
            "    movzbq %al, %rax",
            "    testq %rax, %rax",
            "    jne .L2",
            "    jmp .L4",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("jl .L2"), "should fuse to jl: {}", result);
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
                "    cmpq %rcx, %rax\n    set{} %al\n    movzbq %al, %rax\n    testq %rax, %rax\n    jne .L1\n",
                cc
            );
            let result = peephole_optimize(asm);
            assert!(result.contains(&format!("{} .L1", expected_jcc)),
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

        let info = classify_line("    jmp .L1");
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
            "    jne .L8",
            "    jmp .L10",
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
            ".L21:",                            // jump table target
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
            ".L5:",
            "    movq -40(%rbp), %rax",
            "    ret",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("-40(%rbp), %rax"),
            "must NOT eliminate load after jmpq* indirect jump target: {}", result);
    }
}
