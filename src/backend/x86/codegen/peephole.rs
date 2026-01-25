//! x86-64 peephole optimizer for assembly text.
//!
//! This pass operates on the generated assembly text, scanning for and eliminating
//! common redundant instruction patterns that arise from the stack-based codegen:
//!
//! 1. Redundant store/load: `movq %rax, N(%rbp)` followed by `movq N(%rbp), %rax`
//!    -> eliminates the load (value is already in %rax)
//!
//! 2. Store then load to different reg: `movq %rax, N(%rbp)` followed by
//!    `movq N(%rbp), %rcx` -> replaces load with `movq %rax, %rcx`
//!
//! 3. Redundant jump: `jmp .Lfoo` where `.Lfoo:` is the very next label
//!    -> eliminates the jump
//!
//! 4. Push/pop elimination: `pushq %rax` / `movq %rax, %rcx` / `popq %rax`
//!    -> replaces with `movq %rax, %rcx` (saves the push/pop pair when the
//!    pushed value is immediately restored)
//!
//! Performance: Lines are pre-parsed into `LineInfo` structs on entry. Pattern
//! matching uses integer/enum comparisons instead of repeated string parsing.
//! When a line is mutated, only that line's `LineInfo` is re-parsed.

// ── Pre-parsed line metadata ─────────────────────────────────────────────────

/// Register identifier (0..=15 for GPRs, 255 = unknown/none).
/// Matches the x86 register family numbering.
type RegId = u8;
const REG_NONE: RegId = 255;

/// Compact classification of a single assembly line.
/// Stored in a parallel array alongside the raw text, so hot loops
/// can check integer fields instead of re-parsing strings.
#[derive(Clone, Copy)]
struct LineInfo {
    kind: LineKind,
}

/// What kind of assembly line this is, with pre-extracted fields for the
/// patterns we care about.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LineKind {
    Nop,                // Deleted line (NUL sentinel)
    Empty,              // Blank line

    /// `movX %reg, offset(%rbp)` – store register to stack slot
    StoreRbp { reg: RegId, offset: i32, size: MoveSize },
    /// `movX offset(%rbp), %reg` or `movslq offset(%rbp), %reg` – load from stack slot
    LoadRbp  { reg: RegId, offset: i32, size: MoveSize },

    Label,              // `name:`
    Jmp,                // `jmp label`
    CondJmp,            // `je`/`jne`/`jl`/... label
    Call,               // `call ...`
    Ret,                // `ret`
    Push,               // `pushq %reg`
    Pop,                // `popq %reg`
    SetCC,              // `setCC %al`
    Cmp,                // `cmpX`/`testX`/`ucomis*`
    Directive,          // Lines starting with `.`

    /// Everything else (regular instructions)
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MoveSize {
    Q,   // movq  (64-bit)
    L,   // movl  (32-bit)
    W,   // movw  (16-bit)
    B,   // movb  (8-bit)
    SLQ, // movslq (sign-extend 32->64)
}

impl MoveSize {
    fn mnemonic(self) -> &'static str {
        match self {
            MoveSize::Q => "movq",
            MoveSize::L => "movl",
            MoveSize::W => "movw",
            MoveSize::B => "movb",
            MoveSize::SLQ => "movslq",
        }
    }
}

impl LineInfo {
    #[inline]
    fn is_nop(self) -> bool { self.kind == LineKind::Nop }
    #[inline]
    fn is_barrier(self) -> bool {
        matches!(self.kind,
            LineKind::Label | LineKind::Call | LineKind::Jmp | LineKind::CondJmp |
            LineKind::Ret | LineKind::Directive)
    }
}

// ── Line parsing ─────────────────────────────────────────────────────────────

/// Strip leading spaces from an assembly line.
#[inline]
fn trim_asm(s: &str) -> &str {
    let b = s.as_bytes();
    if b.first() == Some(&b' ') {
        let mut i = 0;
        while i < b.len() && b[i] == b' ' {
            i += 1;
        }
        &s[i..]
    } else {
        s
    }
}

/// Parse one assembly line into a `LineInfo`.
fn classify_line(raw: &str) -> LineInfo {
    let b = raw.as_bytes();
    // NUL sentinel = dead line
    if b.first() == Some(&0) {
        return LineInfo { kind: LineKind::Nop };
    }
    let s = trim_asm(raw);
    if s.is_empty() {
        return LineInfo { kind: LineKind::Empty };
    }

    // Label: ends with ':'
    if s.as_bytes().last() == Some(&b':') {
        return LineInfo { kind: LineKind::Label };
    }

    // Directive: starts with '.'
    if s.as_bytes().first() == Some(&b'.') {
        return LineInfo { kind: LineKind::Directive };
    }

    // Try store/load to/from rbp first (hottest path)
    if let Some((reg_str, offset_str, size)) = parse_store_to_rbp_str(s) {
        let reg = register_family(reg_str).unwrap_or(REG_NONE);
        let offset = fast_parse_i32(offset_str);
        return LineInfo { kind: LineKind::StoreRbp { reg, offset, size } };
    }
    if let Some((offset_str, reg_str, size)) = parse_load_from_rbp_str(s) {
        let reg = register_family(reg_str).unwrap_or(REG_NONE);
        let offset = fast_parse_i32(offset_str);
        return LineInfo { kind: LineKind::LoadRbp { reg, offset, size } };
    }

    // Control flow
    if s.starts_with("jmp ") {
        return LineInfo { kind: LineKind::Jmp };
    }
    if s.starts_with("call") {
        return LineInfo { kind: LineKind::Call };
    }
    if s == "ret" {
        return LineInfo { kind: LineKind::Ret };
    }

    // Conditional jumps
    if is_conditional_jump(s) {
        return LineInfo { kind: LineKind::CondJmp };
    }

    // Push / Pop
    if s.starts_with("pushq ") {
        return LineInfo { kind: LineKind::Push };
    }
    if s.starts_with("popq ") {
        return LineInfo { kind: LineKind::Pop };
    }

    // SetCC
    if s.starts_with("set") && parse_setcc(s).is_some() {
        return LineInfo { kind: LineKind::SetCC };
    }

    // Compare / test
    if s.starts_with("cmpq ") || s.starts_with("cmpl ") || s.starts_with("cmpw ")
        || s.starts_with("cmpb ") || s.starts_with("testq ") || s.starts_with("testl ")
        || s.starts_with("testw ") || s.starts_with("testb ")
        || s.starts_with("ucomisd ") || s.starts_with("ucomiss ")
    {
        return LineInfo { kind: LineKind::Cmp };
    }

    LineInfo { kind: LineKind::Other }
}

/// Fast i32 parse for stack offsets like "-8", "-24", "0", etc.
/// Falls back to 0 on unparseable inputs (should not happen with valid asm).
#[inline]
fn fast_parse_i32(s: &str) -> i32 {
    let b = s.as_bytes();
    if b.is_empty() {
        return 0;
    }
    let (neg, start) = if b[0] == b'-' { (true, 1) } else { (false, 0) };
    let mut v: i32 = 0;
    for &c in &b[start..] {
        if c >= b'0' && c <= b'9' {
            v = v.wrapping_mul(10).wrapping_add((c - b'0') as i32);
        } else {
            break;
        }
    }
    if neg { -v } else { v }
}

// ── Existing string-level parsers (used once during classify and for mutations) ──

/// Parse `movX %reg, offset(%rbp)` (store to rbp-relative slot).
/// Returns (register_str, offset_str, size).
fn parse_store_to_rbp_str(s: &str) -> Option<(&str, &str, MoveSize)> {
    let (rest, size) = if let Some(r) = s.strip_prefix("movq ") {
        (r, MoveSize::Q)
    } else if let Some(r) = s.strip_prefix("movl ") {
        (r, MoveSize::L)
    } else if let Some(r) = s.strip_prefix("movw ") {
        (r, MoveSize::W)
    } else if let Some(r) = s.strip_prefix("movb ") {
        (r, MoveSize::B)
    } else {
        return None;
    };

    let (src, dst) = rest.split_once(',')?;
    let src = src.trim();
    let dst = dst.trim();

    if !src.starts_with('%') {
        return None;
    }
    if !dst.ends_with("(%rbp)") {
        return None;
    }
    let offset = &dst[..dst.len() - 6];

    Some((src, offset, size))
}

/// Parse `movX offset(%rbp), %reg` or `movslq offset(%rbp), %reg` (load from rbp).
/// Returns (offset_str, register_str, size).
fn parse_load_from_rbp_str(s: &str) -> Option<(&str, &str, MoveSize)> {
    let (rest, size) = if let Some(r) = s.strip_prefix("movq ") {
        (r, MoveSize::Q)
    } else if let Some(r) = s.strip_prefix("movl ") {
        (r, MoveSize::L)
    } else if let Some(r) = s.strip_prefix("movw ") {
        (r, MoveSize::W)
    } else if let Some(r) = s.strip_prefix("movb ") {
        (r, MoveSize::B)
    } else if let Some(r) = s.strip_prefix("movslq ") {
        (r, MoveSize::SLQ)
    } else {
        return None;
    };

    let (src, dst) = rest.split_once(',')?;
    let src = src.trim();
    let dst = dst.trim();

    if !src.ends_with("(%rbp)") {
        return None;
    }
    let offset = &src[..src.len() - 6];
    if !dst.starts_with('%') {
        return None;
    }

    Some((offset, dst, size))
}

// ── NOP helpers ──────────────────────────────────────────────────────────────

#[inline]
fn is_nop(line: &str) -> bool {
    line.as_bytes().first() == Some(&0)
}

#[inline]
fn mark_nop(line: &mut String, info: &mut LineInfo) {
    line.clear();
    line.push('\0');
    *info = LineInfo { kind: LineKind::Nop };
}

/// Replace a line's text and re-classify it.
#[inline]
fn replace_line(line: &mut String, info: &mut LineInfo, new_text: String) {
    *line = new_text;
    *info = classify_line(line);
}

// ── Main entry point ─────────────────────────────────────────────────────────

/// Run peephole optimization on x86-64 assembly text.
/// Returns the optimized assembly string.
pub fn peephole_optimize(asm: String) -> String {
    let mut lines: Vec<String> = asm.lines().map(|s| s.to_string()).collect();
    // Drop the original string early to free memory before we allocate the result.
    // The lines Vec now owns all the line data.
    drop(asm);

    let mut infos: Vec<LineInfo> = lines.iter().map(|l| classify_line(l)).collect();

    let mut changed = true;
    let mut pass_count = 0;
    while changed && pass_count < 10 {
        changed = false;
        changed |= eliminate_redundant_store_load(&mut lines, &mut infos);
        changed |= eliminate_store_load_different_reg(&mut lines, &mut infos);
        changed |= eliminate_redundant_jumps(&mut lines, &mut infos);
        changed |= eliminate_push_pop_pairs(&mut lines, &mut infos);
        changed |= eliminate_binop_push_pop_pattern(&mut lines, &mut infos);
        changed |= eliminate_redundant_movq_self(&mut lines, &mut infos);
        changed |= fuse_compare_and_branch(&mut lines, &mut infos);
        changed |= forward_store_load_non_adjacent(&mut lines, &mut infos);
        changed |= global_store_forwarding(&mut lines, &mut infos);
        changed |= eliminate_redundant_cltq(&mut lines, &mut infos);
        changed |= eliminate_redundant_zero_extend(&mut lines, &mut infos);
        changed |= eliminate_dead_stores(&mut lines, &mut infos);
        pass_count += 1;
    }

    // Remove NOP markers and rebuild. Use total line bytes as capacity upper bound
    // (cheaper than computing exact surviving size).
    let total_bytes: usize = lines.iter().map(|l| l.len() + 1).sum();
    let mut result = String::with_capacity(total_bytes);
    for (line, info) in lines.iter().zip(infos.iter()) {
        if !info.is_nop() {
            result.push_str(line);
            result.push('\n');
        }
    }
    result
}

// ── Pattern 1: Redundant store followed by load of same value ────────────────

fn eliminate_redundant_store_load(lines: &mut [String], infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = lines.len();

    for i in 0..len.saturating_sub(1) {
        if infos[i].is_nop() || infos[i + 1].is_nop() {
            continue;
        }

        if let LineKind::StoreRbp { reg: sr, offset: so, size: ss } = infos[i].kind {
            if let LineKind::LoadRbp { reg: lr, offset: lo, size: ls } = infos[i + 1].kind {
                if so == lo && sr == lr && ss == ls {
                    mark_nop(&mut lines[i + 1], &mut infos[i + 1]);
                    changed = true;
                }
            }
        }
    }
    changed
}

// ── Pattern 2: Store then load to different register ─────────────────────────

fn eliminate_store_load_different_reg(lines: &mut [String], infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = lines.len();

    for i in 0..len.saturating_sub(1) {
        if infos[i].is_nop() || infos[i + 1].is_nop() {
            continue;
        }

        if let LineKind::StoreRbp { reg: sr, offset: so, size: ss } = infos[i].kind {
            if let LineKind::LoadRbp { reg: lr, offset: lo, size: ls } = infos[i + 1].kind {
                if so == lo && sr != lr && ss == ls {
                    // Need register name strings from the original line for the replacement
                    let store_line = trim_asm(&lines[i]);
                    let (store_reg, _, _) = parse_store_to_rbp_str(store_line).unwrap();
                    let load_line = trim_asm(&lines[i + 1]);
                    let (_, load_reg, _) = parse_load_from_rbp_str(load_line).unwrap();
                    let new_text = format!("    {} {}, {}", ss.mnemonic(), store_reg, load_reg);
                    replace_line(&mut lines[i + 1], &mut infos[i + 1], new_text);
                    changed = true;
                }
            }
        }
    }
    changed
}

// ── Pattern 3: Redundant jumps ───────────────────────────────────────────────

fn eliminate_redundant_jumps(lines: &mut [String], infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = lines.len();

    for i in 0..len.saturating_sub(1) {
        if infos[i].kind != LineKind::Jmp {
            continue;
        }

        let jmp_line = trim_asm(&lines[i]);
        if let Some(target) = jmp_line.strip_prefix("jmp ") {
            let target = target.trim();
            // Find the next non-NOP, non-empty line
            for j in (i + 1)..len {
                if infos[j].is_nop() || infos[j].kind == LineKind::Empty {
                    continue;
                }
                // Check if it's the target label
                if infos[j].kind == LineKind::Label {
                    let next = trim_asm(&lines[j]);
                    if let Some(label) = next.strip_suffix(':') {
                        if label == target {
                            mark_nop(&mut lines[i], &mut infos[i]);
                            changed = true;
                        }
                    }
                }
                break;
            }
        }
    }
    changed
}

// ── Pattern 4: Push/pop pair elimination ─────────────────────────────────────

fn eliminate_push_pop_pairs(lines: &mut [String], infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = lines.len();

    for i in 0..len.saturating_sub(2) {
        if infos[i].kind != LineKind::Push {
            continue;
        }

        let push_line = trim_asm(&lines[i]);
        if let Some(push_reg) = push_line.strip_prefix("pushq ") {
            let push_reg = push_reg.trim();
            if !push_reg.starts_with('%') {
                continue;
            }

            for j in (i + 1)..std::cmp::min(i + 4, len) {
                if infos[j].is_nop() {
                    continue;
                }
                // Use pre-parsed info for quick rejection
                if infos[j].kind == LineKind::Pop {
                    let line = trim_asm(&lines[j]);
                    if let Some(pop_reg) = line.strip_prefix("popq ") {
                        let pop_reg = pop_reg.trim();
                        if pop_reg == push_reg {
                            let mut safe = true;
                            for k in (i + 1)..j {
                                if infos[k].is_nop() {
                                    continue;
                                }
                                if instruction_modifies_reg(trim_asm(&lines[k]), push_reg) {
                                    safe = false;
                                    break;
                                }
                            }
                            if safe {
                                mark_nop(&mut lines[i], &mut infos[i]);
                                mark_nop(&mut lines[j], &mut infos[j]);
                                changed = true;
                            }
                        }
                    }
                    break;
                }
                if infos[j].kind == LineKind::Push {
                    break;
                }
                if matches!(infos[j].kind, LineKind::Call | LineKind::Jmp | LineKind::Ret) {
                    break;
                }
            }
        }
    }
    changed
}

// ── Pattern 5: Binary-op push/pop pattern ────────────────────────────────────

fn eliminate_binop_push_pop_pattern(lines: &mut [String], infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = lines.len();

    let mut i = 0;
    while i + 3 < len {
        if infos[i].kind != LineKind::Push {
            i += 1;
            continue;
        }

        let push_line = trim_asm(&lines[i]);
        if let Some(push_reg) = push_line.strip_prefix("pushq ") {
            let push_reg = push_reg.trim();
            if !push_reg.starts_with('%') {
                i += 1;
                continue;
            }

            // Find next 3 non-NOP lines
            let mut real_indices = [0usize; 3];
            let mut count = 0;
            let mut j = i + 1;
            while j < len && count < 3 {
                if !infos[j].is_nop() {
                    real_indices[count] = j;
                    count += 1;
                }
                j += 1;
            }

            if count == 3 {
                let load_idx = real_indices[0];
                let move_idx = real_indices[1];
                let pop_idx = real_indices[2];

                if infos[pop_idx].kind == LineKind::Pop {
                    let pop_line = trim_asm(&lines[pop_idx]);
                    if let Some(pop_reg) = pop_line.strip_prefix("popq ") {
                        let pop_reg = pop_reg.trim();
                        if pop_reg == push_reg {
                            let load_line = trim_asm(&lines[load_idx]);
                            let move_line = trim_asm(&lines[move_idx]);

                            if let Some(move_target) = parse_reg_to_reg_move(move_line, push_reg) {
                                if instruction_writes_to(load_line, push_reg) && can_redirect_instruction(load_line) {
                                    if let Some(new_load) = replace_dest_register(load_line, push_reg, move_target) {
                                        mark_nop(&mut lines[i], &mut infos[i]);
                                        let new_text = format!("    {}", new_load);
                                        replace_line(&mut lines[load_idx], &mut infos[load_idx], new_text);
                                        mark_nop(&mut lines[move_idx], &mut infos[move_idx]);
                                        mark_nop(&mut lines[pop_idx], &mut infos[pop_idx]);
                                        changed = true;
                                        i = pop_idx + 1;
                                        continue;
                                    }
                                }
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

// ── Pattern 6: Redundant self-moves ──────────────────────────────────────────

fn eliminate_redundant_movq_self(lines: &mut [String], infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    for i in 0..lines.len() {
        if infos[i].is_nop() {
            continue;
        }
        let trimmed = trim_asm(&lines[i]);
        if is_self_move(trimmed) {
            mark_nop(&mut lines[i], &mut infos[i]);
            changed = true;
        }
    }
    changed
}

// ── Pattern 7: Compare-and-branch fusion ─────────────────────────────────────

fn fuse_compare_and_branch(lines: &mut [String], infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = lines.len();

    let mut i = 0;
    while i < len {
        if infos[i].kind != LineKind::Cmp {
            i += 1;
            continue;
        }

        // Collect next non-NOP lines (up to 8)
        let mut seq_indices = [0usize; 8];
        let mut seq_count = 0;
        let mut j = i;
        while j < len && seq_count < 8 {
            if !infos[j].is_nop() {
                seq_indices[seq_count] = j;
                seq_count += 1;
            }
            j += 1;
        }

        if seq_count < 4 {
            i += 1;
            continue;
        }

        // Second must be setCC
        if infos[seq_indices[1]].kind != LineKind::SetCC {
            i += 1;
            continue;
        }
        let set_line = trim_asm(&lines[seq_indices[1]]);
        let cc = match parse_setcc(set_line) {
            Some(c) => c,
            None => { i += 1; continue; }
        };

        // Scan for testq %rax, %rax pattern
        let mut test_idx = None;
        let mut scan = 2;
        while scan < seq_count {
            let si = seq_indices[scan];
            let line = trim_asm(&lines[si]);

            // Skip zero-extend of setcc result
            if line.starts_with("movzbq %al,") || line.starts_with("movzbl %al,") {
                scan += 1;
                continue;
            }
            // Skip store/load to rbp (pre-parsed fast check)
            if matches!(infos[si].kind, LineKind::StoreRbp { .. }) {
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

        if test_scan + 1 >= seq_count {
            i += 1;
            continue;
        }

        let jmp_line = trim_asm(&lines[seq_indices[test_scan + 1]]);
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
            mark_nop(&mut lines[seq_indices[s]], &mut infos[seq_indices[s]]);
        }
        // Replace the jne/je with the fused conditional jump
        let idx = seq_indices[test_scan + 1];
        replace_line(&mut lines[idx], &mut infos[idx], fused_jcc);

        changed = true;
        i = idx + 1;
    }

    changed
}

// ── Pattern 8: Forward store to non-adjacent load ────────────────────────────

fn forward_store_load_non_adjacent(lines: &mut [String], infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = lines.len();
    const WINDOW: usize = 20;

    for i in 0..len {
        let (store_reg_id, store_offset, store_size) = match infos[i].kind {
            LineKind::StoreRbp { reg, offset, size } => (reg, offset, size),
            _ => continue,
        };

        // Get register name string for replacement (only needed if we find a match)
        // We defer this to avoid parsing unless needed
        let mut store_reg_str: Option<String> = None;

        let mut reg_still_valid = true;
        let end = std::cmp::min(i + WINDOW, len);

        for j in (i + 1)..end {
            if infos[j].is_nop() {
                continue;
            }

            // Barrier check using pre-parsed info (fast integer comparison)
            if infos[j].is_barrier() {
                break;
            }

            // Another store to same offset kills forwarding
            if let LineKind::StoreRbp { offset, .. } = infos[j].kind {
                if offset == store_offset {
                    break;
                }
            }

            // Load from same offset?
            if let LineKind::LoadRbp { reg: load_reg_id, offset: load_offset, size: load_size } = infos[j].kind {
                if load_offset == store_offset && load_size == store_size && reg_still_valid {
                    // Lazy-init the register name string
                    if store_reg_str.is_none() {
                        let trimmed = trim_asm(&lines[i]);
                        let (sr, _, _) = parse_store_to_rbp_str(trimmed).unwrap();
                        store_reg_str = Some(sr.to_string());
                    }
                    let sr = store_reg_str.as_ref().unwrap();

                    if load_reg_id == store_reg_id {
                        // Same register: load is redundant
                        mark_nop(&mut lines[j], &mut infos[j]);
                        changed = true;
                        continue;
                    } else {
                        // Different register: replace with reg-to-reg move
                        let load_line = trim_asm(&lines[j]);
                        let (_, load_reg, _) = parse_load_from_rbp_str(load_line).unwrap();
                        let new_text = format!("    {} {}, {}", store_size.mnemonic(), sr, load_reg);
                        replace_line(&mut lines[j], &mut infos[j], new_text);
                        changed = true;
                        continue;
                    }
                }
            }

            // Check if instruction modifies the source register
            if reg_still_valid {
                // Quick check: if this line is a store to rbp, it only modifies
                // a stack slot, not a register. Similarly loads only modify the dest reg.
                match infos[j].kind {
                    LineKind::StoreRbp { .. } => {
                        // A store doesn't modify any GPR (the source is read, not written)
                        // Already checked same-offset above, so this is safe to skip
                    }
                    LineKind::LoadRbp { reg, .. } => {
                        // Load modifies its destination register
                        if reg == store_reg_id {
                            reg_still_valid = false;
                        }
                    }
                    _ => {
                        // Fall back to string-based check for other instructions
                        if store_reg_str.is_none() {
                            let trimmed = trim_asm(&lines[i]);
                            let (sr, _, _) = parse_store_to_rbp_str(trimmed).unwrap();
                            store_reg_str = Some(sr.to_string());
                        }
                        if instruction_modifies_reg(trim_asm(&lines[j]), store_reg_str.as_ref().unwrap()) {
                            reg_still_valid = false;
                        }
                    }
                }
            }
        }
    }

    changed
}

// ── Pattern 9: Redundant cltq after movslq ───────────────────────────────────

fn eliminate_redundant_cltq(lines: &mut [String], infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = lines.len();

    for i in 0..len.saturating_sub(1) {
        if infos[i].is_nop() || infos[i + 1].is_nop() {
            continue;
        }

        let curr_is_cltq = trim_asm(&lines[i + 1]) == "cltq";
        if !curr_is_cltq {
            continue;
        }

        let prev = trim_asm(&lines[i]);

        // movslq already sign-extends, cltq is redundant
        if prev.starts_with("movslq ") && prev.contains("%rax") {
            mark_nop(&mut lines[i + 1], &mut infos[i + 1]);
            changed = true;
            continue;
        }

        // movq $IMM, %rax followed by cltq is also redundant
        if prev.starts_with("movq $") && prev.ends_with("%rax") {
            mark_nop(&mut lines[i + 1], &mut infos[i + 1]);
            changed = true;
        }
    }

    changed
}

// ── Pattern 10: Dead store elimination ───────────────────────────────────────

fn eliminate_dead_stores(lines: &mut [String], infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = lines.len();
    const WINDOW: usize = 16;

    // Reusable buffer for the "OFFSET(%rbp)" pattern string, avoiding
    // heap allocation on every inner-loop iteration.
    let mut pattern_buf = String::with_capacity(20);

    for i in 0..len {
        let store_offset = match infos[i].kind {
            LineKind::StoreRbp { offset, .. } => offset,
            _ => continue,
        };

        let end = std::cmp::min(i + WINDOW, len);
        let mut slot_read = false;
        let mut slot_overwritten = false;
        // Build the "OFFSET(%rbp)" pattern lazily (only when an Other line appears)
        let mut pattern_built = false;

        for j in (i + 1)..end {
            if infos[j].is_nop() {
                continue;
            }

            // Barrier: conservatively assume slot may be read
            if infos[j].is_barrier() {
                slot_read = true;
                break;
            }

            // Load from same offset = slot is read
            if let LineKind::LoadRbp { offset, .. } = infos[j].kind {
                if offset == store_offset {
                    slot_read = true;
                    break;
                }
            }

            // Another store to same offset = slot overwritten
            if let LineKind::StoreRbp { offset, .. } = infos[j].kind {
                if offset == store_offset {
                    slot_overwritten = true;
                    break;
                }
            }

            // Catch-all: check if line references the offset via string search
            // (handles leaq, movslq, etc. that aren't classified as store/load)
            if infos[j].kind == LineKind::Other {
                // Build pattern once per outer iteration, reuse for all Other lines
                if !pattern_built {
                    pattern_buf.clear();
                    use std::fmt::Write;
                    write!(pattern_buf, "{}(%rbp)", store_offset).unwrap();
                    pattern_built = true;
                }
                let line = trim_asm(&lines[j]);
                if line.contains(pattern_buf.as_str()) {
                    slot_read = true;
                    break;
                }
            }
        }

        if slot_overwritten && !slot_read {
            mark_nop(&mut lines[i], &mut infos[i]);
            changed = true;
        }
    }

    changed
}

// ── Helper functions ─────────────────────────────────────────────────────────

/// Check if a line is a conditional jump instruction.
fn is_conditional_jump(s: &str) -> bool {
    s.starts_with("je ") || s.starts_with("jne ") || s.starts_with("jl ")
        || s.starts_with("jle ") || s.starts_with("jg ") || s.starts_with("jge ")
        || s.starts_with("jb ") || s.starts_with("jbe ") || s.starts_with("ja ")
        || s.starts_with("jae ") || s.starts_with("js ") || s.starts_with("jns ")
        || s.starts_with("jo ") || s.starts_with("jno ") || s.starts_with("jp ")
        || s.starts_with("jnp ") || s.starts_with("jz ") || s.starts_with("jnz ")
}

/// Check if an instruction is a self-move (e.g., movq %rax, %rax).
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

/// Parse `movq %src, %dst` and return %dst if %src matches expected_src.
fn parse_reg_to_reg_move<'a>(line: &'a str, expected_src: &str) -> Option<&'a str> {
    for prefix in &["movq ", "movl "] {
        if let Some(rest) = line.strip_prefix(prefix) {
            if let Some((src, dst)) = rest.split_once(',') {
                let src = src.trim();
                let dst = dst.trim();
                if src == expected_src && dst.starts_with('%') {
                    return Some(dst);
                }
            }
        }
    }
    None
}

/// Check if an instruction writes to a specific register as its destination.
fn instruction_writes_to(inst: &str, reg: &str) -> bool {
    if let Some((_op, operands)) = inst.split_once(' ') {
        if let Some((_src, dst)) = operands.rsplit_once(',') {
            let dst = dst.trim();
            if dst == reg || register_overlaps(dst, reg) {
                return true;
            }
        }
    }
    false
}

/// Check if an instruction can have its destination register replaced safely.
fn can_redirect_instruction(inst: &str) -> bool {
    if inst.starts_with("movabsq ") {
        return false;
    }
    if inst.starts_with(".") || inst.ends_with(":") {
        return false;
    }
    true
}

/// Replace the destination register in an instruction.
fn replace_dest_register(inst: &str, old_reg: &str, new_reg: &str) -> Option<String> {
    if !old_reg.starts_with("%r") || !new_reg.starts_with("%r") {
        return None;
    }

    // Handle `xorq %reg, %reg` (zero idiom)
    if let Some(rest) = inst.strip_prefix("xorq ") {
        if let Some((src, dst)) = rest.split_once(',') {
            let src = src.trim();
            let dst = dst.trim();
            if src == old_reg && dst == old_reg {
                return Some(format!("xorq {}, {}", new_reg, new_reg));
            }
        }
    }

    for prefix in &["movq ", "movslq ", "leaq ", "movzbq "] {
        if let Some(rest) = inst.strip_prefix(prefix) {
            if let Some((src, dst)) = rest.rsplit_once(',') {
                let src = src.trim();
                let dst = dst.trim();
                if dst == old_reg {
                    if !src.contains(old_reg) {
                        return Some(format!("{}{}, {}", prefix, src, new_reg));
                    }
                }
            }
        }
    }

    None
}

/// Check if an instruction modifies the given register.
fn instruction_modifies_reg(inst: &str, reg: &str) -> bool {
    if inst.is_empty() || is_nop(inst) || inst.ends_with(':') || inst.starts_with('.') {
        return false;
    }

    if let Some((_op, operands)) = inst.split_once(' ') {
        if let Some((_src, dst)) = operands.rsplit_once(',') {
            let dst = dst.trim();
            if dst == reg || register_overlaps(dst, reg) {
                return true;
            }
        } else {
            let operand = operands.trim();
            if inst.starts_with("pop") && (operand == reg || register_overlaps(operand, reg)) {
                return true;
            }
            if (inst.starts_with("inc") || inst.starts_with("dec") ||
                inst.starts_with("not") || inst.starts_with("neg")) &&
                (operand == reg || register_overlaps(operand, reg)) {
                return true;
            }
        }
    }

    false
}

/// Parse a setCC instruction and return the condition code string.
fn parse_setcc(s: &str) -> Option<&str> {
    if !s.starts_with("set") {
        return None;
    }
    let rest = &s[3..];
    let space_idx = rest.find(' ')?;
    let cc = &rest[..space_idx];
    match cc {
        "e" | "ne" | "l" | "le" | "g" | "ge" | "b" | "be" | "a" | "ae"
        | "s" | "ns" | "o" | "no" | "p" | "np" | "z" | "nz" => Some(cc),
        _ => None,
    }
}

/// Invert a condition code (e.g., "e" -> "ne", "l" -> "ge")
fn invert_cc(cc: &str) -> &str {
    match cc {
        "e" | "z" => "ne",
        "ne" | "nz" => "e",
        "l" => "ge",
        "ge" => "l",
        "le" => "g",
        "g" => "le",
        "b" => "ae",
        "ae" => "b",
        "be" => "a",
        "a" => "be",
        "s" => "ns",
        "ns" => "s",
        "o" => "no",
        "no" => "o",
        "p" => "np",
        "np" => "p",
        _ => cc,
    }
}

/// Pattern 10b: Eliminate redundant zero/sign-extension after instructions that
/// already produce a fully extended result.
///
/// Examples:
///   movzbq (%rcx), %rax / movzbq %al, %rax  -> remove second movzbq
///   movzwq (%rcx), %rax / movzwq %ax, %rax  -> remove second movzwq
///   movsbq (%rcx), %rax / movsbq %al, %rax  -> remove second movsbq
///   movslq (%rcx), %rax / movslq %eax, %rax -> remove second movslq
///   addl %ecx, %eax / cltq / cltq           -> remove second cltq
///
/// Also: movzbq %al, %rax after cmpq/testq + setCC + movzbq is redundant
/// (the setCC + first movzbq already produces a zero-extended result).
///
/// We handle the general case: if instruction N writes a zero/sign-extended
/// value to %rax, and instruction N+1 or N+2 does the same extension again,
/// the second one is redundant.
fn eliminate_redundant_zero_extend(lines: &mut [String], infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = lines.len();

    for i in 0..len.saturating_sub(1) {
        if infos[i].is_nop() {
            continue;
        }

        // Find the next non-NOP instruction after i
        let mut ext_idx = i + 1;
        // Look through intervening stores to rbp (which don't modify %rax)
        // Use a wider window (up to 10) to catch cases where many SSA copies
        // create multiple stores between the producing instruction and the
        // redundant extension.
        while ext_idx < len && ext_idx < i + 10 {
            if infos[ext_idx].is_nop() {
                ext_idx += 1;
                continue;
            }
            // Skip stores to rbp - they read %rax but don't modify it
            if matches!(infos[ext_idx].kind, LineKind::StoreRbp { .. }) {
                ext_idx += 1;
                continue;
            }
            break;
        }

        if ext_idx >= len || infos[ext_idx].is_nop() {
            continue;
        }

        let next = trim_asm(&lines[ext_idx]);
        let prev = trim_asm(&lines[i]);

        // Check if the next instruction is a redundant zero/sign extension to rax
        let is_redundant_ext = {
            // movzbq %al, %rax after movzbq <mem>, %rax
            if next == "movzbq %al, %rax" {
                prev.starts_with("movzbq ") && prev.ends_with("%rax")
            }
            // movzwq %ax, %rax after movzwq <mem>, %rax
            else if next == "movzwq %ax, %rax" {
                prev.starts_with("movzwq ") && prev.ends_with("%rax")
            }
            // movsbq %al, %rax after movsbq <mem>, %rax
            else if next == "movsbq %al, %rax" {
                prev.starts_with("movsbq ") && prev.ends_with("%rax")
            }
            // movslq %eax, %rax after movslq <mem>, %rax
            else if next == "movslq %eax, %rax" {
                prev.starts_with("movslq ") && prev.ends_with("%rax")
            }
            // movl %eax, %eax (zero-extend 32->64) after operations that already
            // produce a zero-extended 32-bit result:
            // - addl/subl/imull/andl/orl/xorl write eax (auto-zeroes upper 32 bits)
            // - movl <mem>, %eax (auto-zeroes upper 32 bits)
            // - movzbl/movzbq already zero-extends
            else if next == "movl %eax, %eax" {
                prev.starts_with("addl ") || prev.starts_with("subl ")
                    || prev.starts_with("imull ") || prev.starts_with("andl ")
                    || prev.starts_with("orl ") || prev.starts_with("xorl ")
                    || prev.starts_with("shll ") || prev.starts_with("shrl ")
                    || (prev.starts_with("movl ") && prev.ends_with("%eax"))
                    || (prev.starts_with("movzbl ") && prev.ends_with("%eax"))
                    || (prev.starts_with("movzbq ") && prev.ends_with("%rax"))
                    || (prev.starts_with("movzwl ") && prev.ends_with("%eax"))
                    || (prev.starts_with("movzwq ") && prev.ends_with("%rax"))
                    || prev == "divl %ecx" || prev == "idivl %ecx"
            }
            // cltq after movslq ... %rax (sign-extend already done)
            // Also: cltq after addl/subl etc. + cltq (already handled by eliminate_redundant_cltq)
            else if next == "cltq" {
                // Already covered by eliminate_redundant_cltq for adjacent, but
                // handle through intervening stores here
                (prev.starts_with("movslq ") && prev.ends_with("%rax"))
                    || (prev.starts_with("movq $") && prev.ends_with("%rax"))
            }
            else {
                false
            }
        };

        if is_redundant_ext {
            mark_nop(&mut lines[ext_idx], &mut infos[ext_idx]);
            changed = true;
        }
    }

    changed
}

/// Check if two register names overlap (e.g., %eax overlaps with %rax).
fn register_overlaps(a: &str, b: &str) -> bool {
    if a == b {
        return true;
    }
    let a_family = register_family(a);
    let b_family = register_family(b);
    a_family.is_some() && a_family == b_family
}

/// Get the register family (0-15) for an x86 register name.
fn register_family(reg: &str) -> Option<u8> {
    match reg {
        "%rax" | "%eax" | "%ax" | "%al" | "%ah" => Some(0),
        "%rcx" | "%ecx" | "%cx" | "%cl" | "%ch" => Some(1),
        "%rdx" | "%edx" | "%dx" | "%dl" | "%dh" => Some(2),
        "%rbx" | "%ebx" | "%bx" | "%bl" | "%bh" => Some(3),
        "%rsp" | "%esp" | "%sp" | "%spl" => Some(4),
        "%rbp" | "%ebp" | "%bp" | "%bpl" => Some(5),
        "%rsi" | "%esi" | "%si" | "%sil" => Some(6),
        "%rdi" | "%edi" | "%di" | "%dil" => Some(7),
        "%r8" | "%r8d" | "%r8w" | "%r8b" => Some(8),
        "%r9" | "%r9d" | "%r9w" | "%r9b" => Some(9),
        "%r10" | "%r10d" | "%r10w" | "%r10b" => Some(10),
        "%r11" | "%r11d" | "%r11w" | "%r11b" => Some(11),
        "%r12" | "%r12d" | "%r12w" | "%r12b" => Some(12),
        "%r13" | "%r13d" | "%r13w" | "%r13b" => Some(13),
        "%r14" | "%r14d" | "%r14w" | "%r14b" => Some(14),
        "%r15" | "%r15d" | "%r15w" | "%r15b" => Some(15),
        _ => None,
    }
}

// ── Pattern 12: Global store forwarding across basic block boundaries ─────────
//
// The existing store forwarding (Pattern 8) stops at labels and jumps.
// This pass does a full-function scan that tracks register→slot mappings,
// allowing store-to-load forwarding to cross fallthrough label boundaries.
//
// Key insight: At a label that is only reached by fallthrough (not targeted by
// any jump), the register state from the previous instruction is fully known.
// We can safely forward stored values across such labels.
//
// For labels that ARE jump targets, we must invalidate all mappings because
// the jump source may have different register values.
//
// This optimization is critical for loops in the stack-based codegen model,
// where the loop body spans multiple basic blocks separated by labels.

/// A tracked store mapping: we know that stack slot at `offset` contains the
/// value that was in register `reg_id` with the given `size`.
#[derive(Clone, Copy)]
struct SlotMapping {
    reg_id: RegId,
    size: MoveSize,
    /// Index of the store instruction that created this mapping
    store_idx: usize,
}

fn global_store_forwarding(lines: &mut [String], infos: &mut [LineInfo]) -> bool {
    let len = lines.len();
    if len == 0 {
        return false;
    }

    // Phase 1: Collect all jump/branch targets so we know which labels are
    // reachable only by fallthrough.
    let mut jump_targets = std::collections::HashSet::new();
    for i in 0..len {
        match infos[i].kind {
            LineKind::Jmp | LineKind::CondJmp => {
                let trimmed = trim_asm(&lines[i]);
                if let Some(target) = extract_jump_target(trimmed) {
                    jump_targets.insert(target.to_string());
                }
            }
            _ => {}
        }
    }

    // Phase 2: Forward scan with slot→register mappings.
    // Map: stack offset -> SlotMapping
    let mut slot_map: std::collections::HashMap<i32, SlotMapping> = std::collections::HashMap::new();
    // Track which registers currently hold "valid" values (i.e., haven't been
    // clobbered since the store). Map: reg_id -> set of offsets it's backing.
    let mut reg_slots: [Vec<i32>; 16] = Default::default();
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
                let label_name = trim_asm(&lines[i]);
                let label_name = label_name.strip_suffix(':').unwrap_or(label_name);

                if jump_targets.contains(label_name) {
                    // This label is a jump target. Multiple predecessors possible.
                    // Must invalidate all mappings since we don't know what state
                    // the jumping block leaves registers in.
                    slot_map.clear();
                    for rs in reg_slots.iter_mut() {
                        rs.clear();
                    }
                } else if prev_was_unconditional_jump {
                    // Label after unconditional jump with no other jumps to it:
                    // unreachable code. Clear state.
                    slot_map.clear();
                    for rs in reg_slots.iter_mut() {
                        rs.clear();
                    }
                }
                // Otherwise: fallthrough-only label. Keep mappings intact.
                prev_was_unconditional_jump = false;
            }

            LineKind::StoreRbp { reg, offset, size } => {
                // A store updates the mapping for this slot.
                // First, remove old mapping for this offset if any.
                if let Some(old) = slot_map.remove(&offset) {
                    if let Some(slots) = reg_slots.get_mut(old.reg_id as usize) {
                        slots.retain(|&o| o != offset);
                    }
                }
                // Record the new mapping.
                if reg != REG_NONE {
                    slot_map.insert(offset, SlotMapping { reg_id: reg, size, store_idx: i });
                    if let Some(slots) = reg_slots.get_mut(reg as usize) {
                        slots.push(offset);
                    }
                }
                prev_was_unconditional_jump = false;
            }

            LineKind::LoadRbp { reg: load_reg, offset: load_offset, size: load_size } => {
                // Check if we have a mapping for this slot.
                if let Some(&mapping) = slot_map.get(&load_offset) {
                    if mapping.size == load_size {
                        // We can forward! Get the register name from the store.
                        let store_trimmed = trim_asm(&lines[mapping.store_idx]);
                        if let Some((store_reg_str, _, _)) = parse_store_to_rbp_str(store_trimmed) {
                            if load_reg == mapping.reg_id {
                                // Same register: just remove the load.
                                mark_nop(&mut lines[i], &mut infos[i]);
                                changed = true;
                            } else {
                                // Different register: replace load with reg-to-reg move.
                                let load_trimmed = trim_asm(&lines[i]);
                                if let Some((_, load_reg_str, _)) = parse_load_from_rbp_str(load_trimmed) {
                                    let new_text = format!("    {} {}, {}",
                                        load_size.mnemonic(), store_reg_str, load_reg_str);
                                    replace_line(&mut lines[i], &mut infos[i], new_text);
                                    changed = true;
                                }
                            }
                        }
                    }
                }

                // The load modifies its destination register, which may
                // invalidate existing mappings backed by that register.
                if load_reg != REG_NONE {
                    invalidate_reg(&mut slot_map, &mut reg_slots, load_reg);
                }
                prev_was_unconditional_jump = false;
            }

            LineKind::Jmp => {
                // After an unconditional jump, execution continues at the target.
                // We can't know the state at the target (it may have other
                // predecessors), so clear everything.
                slot_map.clear();
                for rs in reg_slots.iter_mut() {
                    rs.clear();
                }
                prev_was_unconditional_jump = true;
            }

            LineKind::CondJmp => {
                // After a conditional jump, execution may fall through to the
                // next instruction. The fallthrough path preserves register state,
                // but we must invalidate all mappings because the condition flags
                // may have been set by modifying a register.
                // Actually, cond jumps don't modify registers; they just branch.
                // The register state on the fallthrough path is unchanged.
                // Keep mappings for the fallthrough path.
                prev_was_unconditional_jump = false;
            }

            LineKind::Call => {
                // Function calls clobber caller-saved registers.
                // Caller-saved: rax(0), rcx(1), rdx(2), rsi(6), rdi(7), r8(8), r9(9), r10(10), r11(11)
                for &r in &[0u8, 1, 2, 6, 7, 8, 9, 10, 11] {
                    invalidate_reg(&mut slot_map, &mut reg_slots, r);
                }
                prev_was_unconditional_jump = false;
            }

            LineKind::Ret => {
                // End of function; clear everything.
                slot_map.clear();
                for rs in reg_slots.iter_mut() {
                    rs.clear();
                }
                prev_was_unconditional_jump = true;
            }

            LineKind::Push | LineKind::Pop => {
                // Push reads a register, pop writes to one.
                // Be conservative: pop modifies its register.
                if infos[i].kind == LineKind::Pop {
                    let trimmed = trim_asm(&lines[i]);
                    if let Some(reg_str) = trimmed.strip_prefix("popq ") {
                        let reg_str = reg_str.trim();
                        if let Some(fam) = register_family(reg_str) {
                            invalidate_reg(&mut slot_map, &mut reg_slots, fam);
                        }
                    }
                }
                prev_was_unconditional_jump = false;
            }

            LineKind::Directive => {
                // Assembly directives don't affect register state.
                // Don't update prev_was_unconditional_jump.
            }

            LineKind::Other => {
                // For any other instruction, check which registers it modifies.
                let trimmed = trim_asm(&lines[i]);
                // Conservative: invalidate registers that appear as destinations.
                if let Some((_op, operands)) = trimmed.split_once(' ') {
                    if let Some((_src, dst)) = operands.rsplit_once(',') {
                        let dst = dst.trim();
                        if let Some(fam) = register_family(dst) {
                            invalidate_reg(&mut slot_map, &mut reg_slots, fam);
                        }
                    } else {
                        // Single-operand instructions: inc, dec, not, neg, etc.
                        let operand = operands.trim();
                        if trimmed.starts_with("inc") || trimmed.starts_with("dec")
                            || trimmed.starts_with("not") || trimmed.starts_with("neg")
                        {
                            if let Some(fam) = register_family(operand) {
                                invalidate_reg(&mut slot_map, &mut reg_slots, fam);
                            }
                        }
                    }
                }
                // Some instructions implicitly write rax (e.g., cltq, cqto, div, idiv, mul)
                if trimmed == "cltq" || trimmed == "cqto" || trimmed == "cdq" || trimmed == "cqo"
                    || trimmed.starts_with("div") || trimmed.starts_with("idiv")
                    || trimmed.starts_with("mul") || trimmed.starts_with("imul")
                {
                    invalidate_reg(&mut slot_map, &mut reg_slots, 0); // rax
                    // div/idiv/mul also clobber rdx
                    if trimmed.starts_with("div") || trimmed.starts_with("idiv")
                        || trimmed.starts_with("mul")
                    {
                        invalidate_reg(&mut slot_map, &mut reg_slots, 2); // rdx
                    }
                }
                // movzbq, movzwq, movsbq, movswq, movslq with %rax as dest
                if (trimmed.starts_with("movzbq ") || trimmed.starts_with("movzwq ")
                    || trimmed.starts_with("movsbq ") || trimmed.starts_with("movswq ")
                    || trimmed.starts_with("movslq "))
                    && trimmed.ends_with("%rax")
                {
                    invalidate_reg(&mut slot_map, &mut reg_slots, 0);
                }
                prev_was_unconditional_jump = false;
            }

            LineKind::SetCC => {
                // setCC writes to %al, which is part of rax (family 0).
                invalidate_reg(&mut slot_map, &mut reg_slots, 0);
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

/// Remove all slot mappings backed by a given register.
fn invalidate_reg(
    slot_map: &mut std::collections::HashMap<i32, SlotMapping>,
    reg_slots: &mut [Vec<i32>; 16],
    reg_id: RegId,
) {
    if let Some(slots) = reg_slots.get_mut(reg_id as usize) {
        for &offset in slots.iter() {
            // Only remove if the mapping is still for this register
            // (it may have been overwritten by a store from a different register)
            if let Some(mapping) = slot_map.get(&offset) {
                if mapping.reg_id == reg_id {
                    slot_map.remove(&offset);
                }
            }
        }
        slots.clear();
    }
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
    fn test_compare_branch_fusion_full() {
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
        assert!(result.contains("jl .L2"), "should fuse to jl: {}", result);
        assert!(result.contains("jmp .L4"), "should keep the fallthrough jmp");
        assert!(!result.contains("setl"), "should eliminate setl");
        assert!(!result.contains("movzbq"), "should eliminate movzbq");
        assert!(!result.contains("testq"), "should eliminate testq");
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
}
