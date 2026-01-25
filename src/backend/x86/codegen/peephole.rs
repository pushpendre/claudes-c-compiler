//! x86-64 peephole optimizer for assembly text.
//!
//! Operates on generated assembly text to eliminate redundant patterns from the
//! stack-based codegen. Lines are pre-parsed into `LineInfo` structs so hot-path
//! pattern matching uses integer/enum comparisons instead of string parsing.
//!
//! ## Pass structure
//!
//! 1. **Local passes** (iterative, up to 8 rounds): `combined_local_pass` merges
//!    5 single-scan patterns (adjacent store/load, redundant jumps, self-moves,
//!    redundant cltq, redundant zero/sign extensions) plus push/pop elimination
//!    and binary-op push/pop rewriting.
//!
//! 2. **Global passes** (once): compare-and-branch fusion, global store forwarding
//!    (across fallthrough labels), and dead store elimination.
//!
//! 3. **Local cleanup** (up to 4 rounds): re-run local passes to clean up
//!    opportunities exposed by global passes.

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
    Push { reg: RegId },  // `pushq %reg`
    Pop { reg: RegId },   // `popq %reg`
    SetCC,              // `setCC %al`
    Cmp,                // `cmpX`/`testX`/`ucomis*`
    Directive,          // Lines starting with `.`

    /// Everything else (regular instructions).
    /// `dest_reg` is the pre-parsed destination register family (REG_NONE if unknown).
    /// This allows fast register-modification checks without re-parsing.
    Other { dest_reg: RegId },
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
    #[inline]
    fn is_push(self) -> bool { matches!(self.kind, LineKind::Push { .. }) }
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

    // Push / Pop (extract register for fast checks)
    if s.starts_with("pushq ") {
        let reg = register_family(s[6..].trim()).unwrap_or(REG_NONE);
        return LineInfo { kind: LineKind::Push { reg } };
    }
    if s.starts_with("popq ") {
        let reg = register_family(s[5..].trim()).unwrap_or(REG_NONE);
        return LineInfo { kind: LineKind::Pop { reg } };
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

    // Pre-parse destination register for fast modification checks.
    let dest_reg = parse_dest_reg_fast(s);
    LineInfo { kind: LineKind::Other { dest_reg } }
}

/// Fast extraction of the destination register family from a generic instruction.
/// Handles the AT&T syntax convention where the last operand is the destination.
/// Also handles implicit writes (cltq, cqto, div, mul, etc.).
#[inline]
fn parse_dest_reg_fast(s: &str) -> RegId {
    let b = s.as_bytes();
    // Implicit rax writers
    if b.len() >= 4 {
        if b[0] == b'c' && (s == "cltq" || s == "cqto" || s == "cdq" || s == "cqo") {
            return 0; // rax family
        }
    }
    // Single-operand div/idiv/mul implicitly write rax:rdx.
    // Note: imul is not listed because the codegen only emits two/three-operand
    // forms (imulq %rcx, %rax / imulq $imm, %rax, %rax) which write to the
    // explicit destination, handled by the comma-based dest extraction below.
    if b.len() >= 3 && (b[0] == b'd' || b[0] == b'i' || b[0] == b'm') {
        if s.starts_with("div") || s.starts_with("idiv") || s.starts_with("mul") {
            return 0; // rax family (also rdx, but we track rax as primary)
        }
    }
    // Two-operand instructions: last operand is destination
    if let Some(comma_pos) = memrchr(b',', b) {
        let after_comma = &s[comma_pos + 1..];
        let trimmed = after_comma.trim();
        return register_family(trimmed).unwrap_or(REG_NONE);
    }
    // Single-operand instructions (inc, dec, not, neg, pop)
    if b.len() >= 4 && (b[0] == b'i' || b[0] == b'd' || b[0] == b'n') {
        if s.starts_with("inc") || s.starts_with("dec") || s.starts_with("not") || s.starts_with("neg") {
            if let Some(space_pos) = s.find(' ') {
                let operand = s[space_pos + 1..].trim();
                return register_family(operand).unwrap_or(REG_NONE);
            }
        }
    }
    REG_NONE
}

/// Find the last occurrence of byte `needle` in `haystack`.
#[inline]
fn memrchr(needle: u8, haystack: &[u8]) -> Option<usize> {
    let mut i = haystack.len();
    while i > 0 {
        i -= 1;
        if haystack[i] == needle {
            return Some(i);
        }
    }
    None
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

/// Strip a `mov*` prefix from an instruction, returning the remainder and size.
/// Handles movq, movl, movw, movb, and optionally movslq (for load parsing).
fn strip_mov_prefix(s: &str, allow_slq: bool) -> Option<(&str, MoveSize)> {
    if let Some(r) = s.strip_prefix("movq ") {
        Some((r, MoveSize::Q))
    } else if let Some(r) = s.strip_prefix("movl ") {
        Some((r, MoveSize::L))
    } else if let Some(r) = s.strip_prefix("movw ") {
        Some((r, MoveSize::W))
    } else if let Some(r) = s.strip_prefix("movb ") {
        Some((r, MoveSize::B))
    } else if allow_slq {
        s.strip_prefix("movslq ").map(|r| (r, MoveSize::SLQ))
    } else {
        None
    }
}

/// Parse `movX %reg, offset(%rbp)` (store to rbp-relative slot).
/// Returns (register_str, offset_str, size).
fn parse_store_to_rbp_str(s: &str) -> Option<(&str, &str, MoveSize)> {
    let (rest, size) = strip_mov_prefix(s, false)?;

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
    let (rest, size) = strip_mov_prefix(s, true)?;

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
    // Pre-count lines to avoid Vec reallocation during split
    let line_count = count_newlines(asm.as_bytes()) + 1;
    let mut lines: Vec<String> = Vec::with_capacity(line_count);
    lines.extend(asm.lines().map(|s| s.to_string()));
    // Drop the original string early to free memory before we allocate the result.
    drop(asm);

    let mut infos: Vec<LineInfo> = lines.iter().map(|l| classify_line(l)).collect();

    // Phase 1: Iterative cheap local passes.
    // 8 iterations is sufficient: local patterns rarely chain deeper than 3-4 levels.
    // The combined_local_pass merges 5 simple passes into one linear scan,
    // reducing the number of full scans from 7 to 3 per iteration.
    let mut changed = true;
    let mut pass_count = 0;
    while changed && pass_count < 8 {
        changed = false;
        changed |= combined_local_pass(&mut lines, &mut infos);
        changed |= eliminate_push_pop_pairs(&mut lines, &mut infos);
        changed |= eliminate_binop_push_pop_pattern(&mut lines, &mut infos);
        pass_count += 1;
    }

    // Phase 2: Expensive global passes (run once)
    let global_changed = fuse_compare_and_branch(&mut lines, &mut infos);
    let global_changed = global_changed | global_store_forwarding(&mut lines, &mut infos);
    let global_changed = global_changed | eliminate_dead_stores(&mut lines, &mut infos);

    // Phase 3: One more local cleanup if global passes made changes.
    // 4 iterations: cleanup after globals is shallow (mostly dead store + adjacent pairs).
    if global_changed {
        let mut changed2 = true;
        let mut pass_count2 = 0;
        while changed2 && pass_count2 < 4 {
            changed2 = false;
            changed2 |= combined_local_pass(&mut lines, &mut infos);
            changed2 |= eliminate_dead_stores(&mut lines, &mut infos);
            pass_count2 += 1;
        }
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

/// Count newlines in a byte slice for pre-sizing the lines Vec.
#[inline]
fn count_newlines(bytes: &[u8]) -> usize {
    bytes.iter().filter(|&&b| b == b'\n').count()
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

fn combined_local_pass(lines: &mut [String], infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = lines.len();

    let mut i = 0;
    while i < len {
        if infos[i].is_nop() {
            i += 1;
            continue;
        }

        // --- Pattern: self-move elimination (movq %reg, %reg) ---
        if matches!(infos[i].kind, LineKind::Other { .. }) {
            let trimmed = trim_asm(&lines[i]);
            if is_self_move(trimmed) {
                mark_nop(&mut lines[i], &mut infos[i]);
                changed = true;
                i += 1;
                continue;
            }
        }

        // --- Pattern: redundant jump to next label ---
        if infos[i].kind == LineKind::Jmp {
            let jmp_line = trim_asm(&lines[i]);
            if let Some(target) = jmp_line.strip_prefix("jmp ") {
                let target = target.trim();
                // Find the next non-NOP, non-empty line
                let mut found_redundant = false;
                for j in (i + 1)..len {
                    if infos[j].is_nop() || infos[j].kind == LineKind::Empty {
                        continue;
                    }
                    if infos[j].kind == LineKind::Label {
                        let next = trim_asm(&lines[j]);
                        if let Some(label) = next.strip_suffix(':') {
                            if label == target {
                                mark_nop(&mut lines[i], &mut infos[i]);
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
        if let LineKind::StoreRbp { reg: sr, offset: so, size: ss } = infos[i].kind {
            if i + 1 < len && !infos[i + 1].is_nop() {
                if let LineKind::LoadRbp { reg: lr, offset: lo, size: ls } = infos[i + 1].kind {
                    if so == lo && ss == ls {
                        if sr == lr {
                            // Same register: load is redundant
                            mark_nop(&mut lines[i + 1], &mut infos[i + 1]);
                            changed = true;
                            i += 1;
                            continue;
                        } else {
                            // Different register: replace with reg-to-reg move
                            let store_line = trim_asm(&lines[i]);
                            let (store_reg, _, _) = parse_store_to_rbp_str(store_line).unwrap();
                            let load_line = trim_asm(&lines[i + 1]);
                            let (_, load_reg, _) = parse_load_from_rbp_str(load_line).unwrap();
                            let new_text = format!("    {} {}, {}", ss.mnemonic(), store_reg, load_reg);
                            replace_line(&mut lines[i + 1], &mut infos[i + 1], new_text);
                            changed = true;
                            i += 1;
                            continue;
                        }
                    }
                }
            }
        }

        // For the next two patterns, we need the next non-NOP instruction after i.
        // We compute it once and use it for both cltq and zero-extend checks.
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
            let next = trim_asm(&lines[ext_idx]);
            let prev = trim_asm(&lines[i]);

            // --- Pattern: redundant zero/sign extension (including cltq) ---
            let is_redundant_ext = {
                if next == "movzbq %al, %rax" {
                    prev.starts_with("movzbq ") && prev.ends_with("%rax")
                } else if next == "movzwq %ax, %rax" {
                    prev.starts_with("movzwq ") && prev.ends_with("%rax")
                } else if next == "movsbq %al, %rax" {
                    prev.starts_with("movsbq ") && prev.ends_with("%rax")
                } else if next == "movslq %eax, %rax" {
                    prev.starts_with("movslq ") && prev.ends_with("%rax")
                } else if next == "movl %eax, %eax" {
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
                } else if next == "cltq" {
                    // cltq after movslq ... %rax (through intervening stores)
                    (prev.starts_with("movslq ") && prev.ends_with("%rax"))
                        || (prev.starts_with("movq $") && prev.ends_with("%rax"))
                } else {
                    false
                }
            };

            if is_redundant_ext {
                mark_nop(&mut lines[ext_idx], &mut infos[ext_idx]);
                changed = true;
                i += 1;
                continue;
            }
        }

        i += 1;
    }
    changed
}

// ── Push/pop pair elimination ─────────────────────────────────────────────────

fn eliminate_push_pop_pairs(lines: &mut [String], infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = lines.len();

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
                        mark_nop(&mut lines[i], &mut infos[i]);
                        mark_nop(&mut lines[j], &mut infos[j]);
                        changed = true;
                    }
                }
                break;
            }
            if infos[j].is_push() {
                break;
            }
            if matches!(infos[j].kind, LineKind::Call | LineKind::Jmp | LineKind::Ret) {
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
        | LineKind::Label | LineKind::Directive | LineKind::Jmp | LineKind::CondJmp => false,
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

fn eliminate_binop_push_pop_pattern(lines: &mut [String], infos: &mut [LineInfo]) -> bool {
    let mut changed = false;
    let len = lines.len();

    let mut i = 0;
    while i + 3 < len {
        let push_reg_id = match infos[i].kind {
            LineKind::Push { reg } if reg != REG_NONE => reg,
            _ => { i += 1; continue; }
        };

        let push_line = trim_asm(&lines[i]);
        let push_reg = match push_line.strip_prefix("pushq ") {
            Some(r) => r.trim(),
            None => { i += 1; continue; }
        };

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

            if let LineKind::Pop { reg: pop_reg_id } = infos[pop_idx].kind {
                if pop_reg_id == push_reg_id {
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

        i += 1;
    }
    changed
}

// ── Compare-and-branch fusion ─────────────────────────────────────────────────

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


// ── Dead store elimination ────────────────────────────────────────────────────

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
            if matches!(infos[j].kind, LineKind::Other { .. }) {
                let line = trim_asm(&lines[j]);
                // Check for indirect memory access through a register (not %rbp).
                // E.g., (%rcx), (%rdi), 8(%rax) -- these could alias any stack
                // slot when the register holds a pointer to a stack variable
                // (address-taken via &var). Treat as barrier.
                if has_indirect_memory_access(line) {
                    slot_read = true;
                    break;
                }
                // Build pattern once per outer iteration, reuse for all Other lines
                if !pattern_built {
                    pattern_buf.clear();
                    use std::fmt::Write;
                    write!(pattern_buf, "{}(%rbp)", store_offset).unwrap();
                    pattern_built = true;
                }
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

/// Check if an assembly line contains an indirect memory access through a register
/// (not %rbp or %rsp). Examples: `(%rcx)`, `8(%rdi)`, `(%rax)`.
/// These could alias any stack slot when a pointer to a local variable is used.
fn has_indirect_memory_access(s: &str) -> bool {
    // Look for patterns like "(%r" where the register is not rbp or rsp
    let bytes = s.as_bytes();
    let len = bytes.len();
    let mut i = 0;
    while i < len {
        if bytes[i] == b'(' && i + 2 < len && bytes[i + 1] == b'%' {
            // Found "(%" - check if it's %rbp or %rsp (which are not indirect aliasing)
            let rest = &s[i + 2..];
            if !rest.starts_with("rbp") && !rest.starts_with("rsp")
                && !rest.starts_with("rip") {
                return true;
            }
        }
        i += 1;
    }
    false
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
    /// Index of the store instruction that created this mapping
    store_idx: usize,
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

fn global_store_forwarding(lines: &mut [String], infos: &mut [LineInfo]) -> bool {
    let len = lines.len();
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
            let trimmed = trim_asm(&lines[i]);
            if let Some(n) = parse_label_number(trimmed) {
                if n > max_label_num {
                    max_label_num = n;
                }
            }
        }
    }
    let mut is_jump_target = vec![false; (max_label_num + 1) as usize];
    let mut has_non_numeric_jump_targets = false;
    for i in 0..len {
        match infos[i].kind {
            LineKind::Jmp | LineKind::CondJmp => {
                let trimmed = trim_asm(&lines[i]);
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
            _ => {}
        }
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
                let label_name = trim_asm(&lines[i]);
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
                // First, remove old mapping for this offset if any.
                if let Some(entry) = slot_entries.iter_mut().find(|e| e.active && e.offset == offset) {
                    let old_reg = entry.mapping.reg_id;
                    entry.active = false;
                    reg_offsets[old_reg as usize].remove_val(offset);
                }
                // Record the new mapping.
                if reg != REG_NONE {
                    slot_entries.push(SlotEntry {
                        offset,
                        mapping: SlotMapping { reg_id: reg, size, store_idx: i },
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
                    if mapping.size == load_size {
                        let store_trimmed = trim_asm(&lines[mapping.store_idx]);
                        if let Some((store_reg_str, _, _)) = parse_store_to_rbp_str(store_trimmed) {
                            if load_reg == mapping.reg_id {
                                mark_nop(&mut lines[i], &mut infos[i]);
                                changed = true;
                            } else {
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

                if load_reg != REG_NONE {
                    invalidate_reg_flat(&mut slot_entries, &mut reg_offsets, load_reg);
                }
                prev_was_unconditional_jump = false;
            }

            LineKind::Jmp => {
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
                    // div/idiv/mul also clobber rdx (family 2).
                    // parse_dest_reg_fast returns 0 (rax) for these; also invalidate rdx.
                    if dest_reg == 0 {
                        let trimmed = trim_asm(&lines[i]);
                        if trimmed.starts_with("div") || trimmed.starts_with("idiv")
                            || trimmed.starts_with("mul")
                        {
                            invalidate_reg_flat(&mut slot_entries, &mut reg_offsets, 2);
                        }
                    }
                }
                // Indirect memory access through a register (e.g., movl %edx, (%rcx))
                // could write to any stack slot, invalidating all slot→register mappings.
                if has_indirect_memory_access(trim_asm(&lines[i])) {
                    invalidate_all_mappings(&mut slot_entries, &mut reg_offsets);
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
