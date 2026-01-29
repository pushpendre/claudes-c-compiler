//! X86 inline assembly template substitution and register formatting.
//!
//! This module provides helper methods for processing x86 inline assembly operands,
//! including template variable substitution (%0, %[name], etc.) and register name
//! formatting with size modifiers (b/w/k/q/h/l) and special modifiers (P/a/c).

use std::borrow::Cow;
use crate::common::types::IrType;
use crate::ir::ir::BlockId;
use super::codegen::X86Codegen;

impl X86Codegen {
    /// Substitute %0, %1, %[name], %k0, %b1, %w2, %q3, %h4, %c0, %P0, %a0, %n0, %l[name] etc.
    /// in x86 asm template.
    ///
    /// Modifier summary:
    /// - `k` (32-bit), `w` (16-bit), `b` (8-bit low), `h` (8-bit high), `q` (64-bit), `l` (32-bit alt)
    /// - `c`: raw constant (no `$` prefix for immediates, no `%` prefix for registers)
    /// - `P`: raw symbol/constant (like `c` — used for `call %P[func]`)
    /// - `a`: address reference (emits `symbol(%rip)` for symbols, raw value for immediates)
    /// - `n`: negated immediate constant (emits the negative of the constant value)
    ///
    /// Uses operand_types to determine the default register size when no modifier is given.
    pub(super) fn substitute_x86_asm_operands(
        line: &str,
        op_regs: &[String],
        op_names: &[Option<String>],
        op_is_memory: &[bool],
        op_mem_addrs: &[String],
        op_types: &[IrType],
        gcc_to_internal: &[usize],
        goto_labels: &[(String, BlockId)],
        op_imm_values: &[Option<i64>],
        op_imm_symbols: &[Option<String>],
    ) -> String {
        let mut result = String::new();
        let chars: Vec<char> = line.chars().collect();
        let mut i = 0;
        while i < chars.len() {
            if chars[i] == '%' && i + 1 < chars.len() {
                i += 1;
                // %% -> literal %
                if chars[i] == '%' {
                    result.push('%');
                    i += 1;
                    continue;
                }
                // Check for x86 size/format modifiers:
                //   k (32), w (16), b (8-low), h (8-high), q (64), l (32-alt),
                //   c (raw constant), P (raw symbol), a (address)
                // But 'l' followed by '[' may be a goto label reference %l[name]
                let mut modifier = None;
                if chars[i] == 'l' && i + 1 < chars.len() && chars[i + 1] == '[' && !goto_labels.is_empty() {
                    // This could be %l[name] goto label reference
                    // Parse the name and check if it's a goto label first
                    let saved_i = i;
                    i += 1; // skip 'l'
                    i += 1; // skip '['
                    let name_start = i;
                    while i < chars.len() && chars[i] != ']' { i += 1; }
                    let name: String = chars[name_start..i].iter().collect();
                    if i < chars.len() { i += 1; } // skip ']'

                    if let Some((_, block_id)) = goto_labels.iter().find(|(n, _)| n == &name) {
                        // It's a goto label — emit the assembly label
                        result.push_str(&block_id.to_string());
                        continue;
                    }
                    // Not a goto label — backtrack and treat as %l (32-bit modifier) + [name]
                    i = saved_i;
                    modifier = Some('l');
                    i += 1; // skip 'l', will parse [name] below
                } else if chars[i] == 'l' && i + 1 < chars.len() && chars[i + 1].is_ascii_digit() && !goto_labels.is_empty() {
                    // %l<N> could be a goto label positional reference
                    let saved_i = i;
                    i += 1; // skip 'l'
                    let mut num = 0usize;
                    while i < chars.len() && chars[i].is_ascii_digit() {
                        num = num * 10 + (chars[i] as usize - '0' as usize);
                        i += 1;
                    }
                    // GCC numbers goto labels after all operands.
                    // %l<N> where N >= num_operands refers to label (N - num_operands).
                    let label_idx = num.wrapping_sub(op_regs.len());
                    if label_idx < goto_labels.len() {
                        result.push_str(&goto_labels[label_idx].1.to_string());
                        continue;
                    }
                    // Not a valid label index — backtrack
                    i = saved_i;
                    modifier = Some('l');
                    i += 1;
                } else if chars[i] == 'P' {
                    // %P: raw symbol/value modifier (uppercase — always a modifier, never a register)
                    if i + 1 < chars.len() && (chars[i + 1].is_ascii_digit() || chars[i + 1] == '[') {
                        modifier = Some('P');
                        i += 1;
                    }
                } else if matches!(chars[i], 'k' | 'w' | 'b' | 'h' | 'q' | 'l' | 'c' | 'a' | 'n') {
                    if i + 1 < chars.len() && (chars[i + 1].is_ascii_digit() || chars[i + 1] == '[') {
                        modifier = Some(chars[i]);
                        i += 1;
                    }
                }

                if chars[i] == '[' {
                    // Named operand: %[name] or %k[name]
                    i += 1;
                    let name_start = i;
                    while i < chars.len() && chars[i] != ']' {
                        i += 1;
                    }
                    let name: String = chars[name_start..i].iter().collect();
                    if i < chars.len() { i += 1; } // skip ]

                    let mut found = false;
                    for (idx, op_name) in op_names.iter().enumerate() {
                        if let Some(ref n) = op_name {
                            if n == &name {
                                Self::emit_operand_with_modifier(&mut result, idx, modifier,
                                    op_regs, op_is_memory, op_mem_addrs, op_types,
                                    op_imm_values, op_imm_symbols);
                                found = true;
                                break;
                            }
                        }
                    }
                    if !found {
                        result.push('%');
                        if let Some(m) = modifier { result.push(m); }
                        result.push('[');
                        result.push_str(&name);
                        result.push(']');
                    }
                } else if chars[i].is_ascii_digit() {
                    // Positional operand: %0, %1, %k2, etc.
                    // GCC operand numbers skip synthetic "+" inputs, so map through gcc_to_internal
                    let mut num = 0usize;
                    while i < chars.len() && chars[i].is_ascii_digit() {
                        num = num * 10 + (chars[i] as usize - '0' as usize);
                        i += 1;
                    }
                    let internal_idx = if num < gcc_to_internal.len() {
                        gcc_to_internal[num]
                    } else {
                        num // fallback: direct mapping
                    };
                    if internal_idx < op_regs.len() {
                        Self::emit_operand_with_modifier(&mut result, internal_idx, modifier,
                            op_regs, op_is_memory, op_mem_addrs, op_types,
                            op_imm_values, op_imm_symbols);
                    } else {
                        result.push('%');
                        if let Some(m) = modifier { result.push(m); }
                        result.push_str(&num.to_string());
                    }
                } else {
                    // Not a recognized pattern, emit as-is (e.g. %rax, %eax, etc.)
                    result.push('%');
                    if let Some(m) = modifier { result.push(m); }
                    result.push(chars[i]);
                    i += 1;
                }
            } else {
                result.push(chars[i]);
                i += 1;
            }
        }
        result
    }

    /// Emit a single operand with the given modifier into the result string.
    /// Shared helper for both named and positional operand substitution.
    fn emit_operand_with_modifier(
        result: &mut String,
        idx: usize,
        modifier: Option<char>,
        op_regs: &[String],
        op_is_memory: &[bool],
        op_mem_addrs: &[String],
        op_types: &[IrType],
        op_imm_values: &[Option<i64>],
        op_imm_symbols: &[Option<String>],
    ) {
        let is_raw = matches!(modifier, Some('c') | Some('P'));
        let is_addr = modifier == Some('a');
        let is_neg = modifier == Some('n');
        let has_symbol = op_imm_symbols.get(idx).and_then(|s| s.as_ref());
        let has_imm = op_imm_values.get(idx).and_then(|v| v.as_ref());

        if is_neg {
            // %n: emit the negated constant value (no $ prefix)
            if let Some(&imm) = has_imm {
                result.push_str(&imm.wrapping_neg().to_string());
            } else {
                // Fallback: emit as-is if not an immediate (shouldn't happen with correct usage)
                result.push_str(&op_regs[idx]);
            }
        } else if is_raw {
            // %c / %P: emit raw value without $ or % prefix
            if let Some(sym) = has_symbol {
                result.push_str(sym);
            } else if let Some(imm) = has_imm {
                result.push_str(&imm.to_string());
            } else if op_is_memory[idx] {
                // Memory operands with %P/%c emit their address directly.
                // Without this, we'd fall through to the register branch
                // and incorrectly emit just the register name.
                result.push_str(&op_mem_addrs[idx]);
            } else {
                // Register: emit without % prefix
                result.push_str(&op_regs[idx]);
            }
        } else if is_addr {
            // %a: emit as address reference
            if let Some(sym) = has_symbol {
                // Symbol: emit as symbol(%rip) for RIP-relative addressing
                result.push_str(&format!("{}(%rip)", sym));
            } else if let Some(imm) = has_imm {
                // Immediate: emit raw value (absolute address)
                result.push_str(&imm.to_string());
            } else if op_is_memory[idx] {
                // Memory operand: emit its address
                result.push_str(&op_mem_addrs[idx]);
            } else {
                // Register: emit as indirect (%reg)
                result.push_str(&format!("(%{})", op_regs[idx]));
            }
        } else if op_is_memory[idx] {
            // Memory operand — emit the pre-computed mem_addr string.
            // This must be checked BEFORE symbol/immediate checks because
            // memory operands with "+m" constraints may also have imm_symbol
            // set (from extract_mem_operand_symbol), but should be emitted as
            // a memory reference (e.g., "symbol(%rip)"), not as an immediate
            // (e.g., "$symbol").
            result.push_str(&op_mem_addrs[idx]);
        } else if let Some(sym) = has_symbol {
            // Symbol immediate (e.g., "i" constraint with &global_var+offset)
            // Emit as $symbol for AT&T syntax
            result.push_str(&format!("${}", sym));
        } else if let Some(imm) = has_imm {
            // Normal immediate — emit as $value
            result.push_str(&format!("${}", imm));
        } else {
            let effective_mod = modifier.or_else(|| Self::default_modifier_for_type(op_types.get(idx).copied()));
            result.push('%');
            result.push_str(&Self::format_x86_reg(&op_regs[idx], effective_mod));
        }
    }

    /// Determine the default register size modifier based on the operand's IR type.
    /// In GCC inline asm, the default register size matches the C type:
    /// - 8-bit types -> %al (modifier 'b')
    /// - 16-bit types -> %ax (modifier 'w')
    /// - 32-bit types -> %eax (modifier 'k')
    /// - 64-bit types -> %rax (no modifier / 'q')
    pub(super) fn default_modifier_for_type(ty: Option<IrType>) -> Option<char> {
        match ty {
            Some(IrType::I8) | Some(IrType::U8) => Some('b'),
            Some(IrType::I16) | Some(IrType::U16) => Some('w'),
            Some(IrType::I32) | Some(IrType::U32) | Some(IrType::F32) => Some('k'),
            // I64, U64, Ptr, F64 all use 64-bit registers (default)
            _ => None,
        }
    }

    /// Format x86 register with size modifier.
    /// Modifiers: k (32-bit), w (16-bit), b (8-bit low), h (8-bit high), q (64-bit), l (32-bit alt)
    /// XMM registers (xmm0-xmm15) have no size variants and are returned as-is.
    pub(super) fn format_x86_reg<'a>(reg: &'a str, modifier: Option<char>) -> Cow<'a, str> {
        // XMM registers don't have size variants
        if reg.starts_with("xmm") {
            return Cow::Borrowed(reg);
        }
        // x87 FPU stack registers don't have size variants
        if reg.starts_with("st(") || reg == "st" {
            return Cow::Borrowed(reg);
        }
        match modifier {
            Some('k') | Some('l') => Self::reg_to_32(reg),
            Some('w') => Self::reg_to_16(reg),
            Some('b') => Self::reg_to_8l(reg),
            Some('h') => Self::reg_to_8h(reg),
            Some('q') | None => Cow::Borrowed(reg),
            _ => Cow::Borrowed(reg),
        }
    }

    /// Convert 64-bit register name to 32-bit variant.
    pub(super) fn reg_to_32<'a>(reg: &'a str) -> Cow<'a, str> {
        match reg {
            "rax" => Cow::Borrowed("eax"),
            "rbx" => Cow::Borrowed("ebx"),
            "rcx" => Cow::Borrowed("ecx"),
            "rdx" => Cow::Borrowed("edx"),
            "rsi" => Cow::Borrowed("esi"),
            "rdi" => Cow::Borrowed("edi"),
            "rbp" => Cow::Borrowed("ebp"),
            "rsp" => Cow::Borrowed("esp"),
            _ if reg.starts_with('r') => Cow::Owned(format!("{}d", reg)), // r8 -> r8d, r10 -> r10d
            _ => Cow::Borrowed(reg),
        }
    }

    /// Convert any register name (8/16/32-bit) to its 64-bit equivalent.
    /// If the register is already 64-bit, returns it unchanged.
    pub(super) fn reg_to_64<'a>(reg: &'a str) -> Cow<'a, str> {
        match reg {
            "eax" | "ax" | "al" | "ah" => Cow::Borrowed("rax"),
            "ebx" | "bx" | "bl" | "bh" => Cow::Borrowed("rbx"),
            "ecx" | "cx" | "cl" | "ch" => Cow::Borrowed("rcx"),
            "edx" | "dx" | "dl" | "dh" => Cow::Borrowed("rdx"),
            "esi" | "si" | "sil" => Cow::Borrowed("rsi"),
            "edi" | "di" | "dil" => Cow::Borrowed("rdi"),
            "ebp" | "bp" | "bpl" => Cow::Borrowed("rbp"),
            "esp" | "sp" | "spl" => Cow::Borrowed("rsp"),
            "r8d" | "r8w" | "r8b" => Cow::Borrowed("r8"),
            "r9d" | "r9w" | "r9b" => Cow::Borrowed("r9"),
            "r10d" | "r10w" | "r10b" => Cow::Borrowed("r10"),
            "r11d" | "r11w" | "r11b" => Cow::Borrowed("r11"),
            "r12d" | "r12w" | "r12b" => Cow::Borrowed("r12"),
            "r13d" | "r13w" | "r13b" => Cow::Borrowed("r13"),
            "r14d" | "r14w" | "r14b" => Cow::Borrowed("r14"),
            "r15d" | "r15w" | "r15b" => Cow::Borrowed("r15"),
            _ => Cow::Borrowed(reg),
        }
    }

    /// Convert 64-bit register name to 16-bit variant.
    pub(super) fn reg_to_16<'a>(reg: &'a str) -> Cow<'a, str> {
        match reg {
            "rax" => Cow::Borrowed("ax"),
            "rbx" => Cow::Borrowed("bx"),
            "rcx" => Cow::Borrowed("cx"),
            "rdx" => Cow::Borrowed("dx"),
            "rsi" => Cow::Borrowed("si"),
            "rdi" => Cow::Borrowed("di"),
            "rbp" => Cow::Borrowed("bp"),
            "rsp" => Cow::Borrowed("sp"),
            _ if reg.starts_with('r') => Cow::Owned(format!("{}w", reg)), // r8 -> r8w
            _ => Cow::Borrowed(reg),
        }
    }

    /// Convert 64-bit register name to 8-bit low variant.
    pub(super) fn reg_to_8l<'a>(reg: &'a str) -> Cow<'a, str> {
        match reg {
            "rax" => Cow::Borrowed("al"),
            "rbx" => Cow::Borrowed("bl"),
            "rcx" => Cow::Borrowed("cl"),
            "rdx" => Cow::Borrowed("dl"),
            "rsi" => Cow::Borrowed("sil"),
            "rdi" => Cow::Borrowed("dil"),
            _ if reg.starts_with('r') => Cow::Owned(format!("{}b", reg)), // r8 -> r8b
            _ => Cow::Borrowed(reg),
        }
    }

    /// Convert 64-bit register name to 8-bit high variant.
    pub(super) fn reg_to_8h<'a>(reg: &'a str) -> Cow<'a, str> {
        match reg {
            "rax" => Cow::Borrowed("ah"),
            "rbx" => Cow::Borrowed("bh"),
            "rcx" => Cow::Borrowed("ch"),
            "rdx" => Cow::Borrowed("dh"),
            _ => Self::reg_to_8l(reg), // fallback to low byte
        }
    }

    /// Map GCC inline asm condition code suffix to x86 SETcc suffix.
    /// GCC's =@cc<cond> maps directly to x86 condition codes in most cases.
    pub(super) fn gcc_cc_to_x86(cond: &str) -> &'static str {
        match cond {
            "e" | "z" => "e",       // equal / zero
            "ne" | "nz" => "ne",    // not equal / not zero
            "s" => "s",             // sign (negative)
            "ns" => "ns",           // not sign (non-negative)
            "o" => "o",             // overflow
            "no" => "no",           // no overflow
            "c" => "c",             // carry
            "nc" => "nc",           // no carry
            "a" | "nbe" => "a",     // above (unsigned >)
            "ae" | "nb" => "ae",    // above or equal (unsigned >=)
            "b" | "nae" => "b",     // below (unsigned <)
            "be" | "na" => "be",    // below or equal (unsigned <=)
            "g" | "nle" => "g",     // greater (signed >)
            "ge" | "nl" => "ge",    // greater or equal (signed >=)
            "l" | "nge" => "l",     // less (signed <)
            "le" | "ng" => "le",    // less or equal (signed <=)
            "p" | "pe" => "p",      // parity even
            "np" | "po" => "np",    // parity odd / no parity
            // TODO: emit a warning/diagnostic for unrecognized condition code suffixes
            _ => "e",               // fallback to equal
        }
    }
}
