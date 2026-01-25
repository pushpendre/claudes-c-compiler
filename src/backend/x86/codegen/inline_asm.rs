//! X86 inline assembly template substitution and register formatting.
//!
//! This module provides helper methods for processing x86 inline assembly operands,
//! including template variable substitution (%0, %[name], etc.) and register name
//! formatting with size modifiers (b/w/k/q/h/l).

use crate::common::types::IrType;
use crate::ir::ir::BlockId;
use super::codegen::X86Codegen;

impl X86Codegen {
    /// Substitute %0, %1, %[name], %k0, %b1, %w2, %q3, %h4, %c0, %l[name] etc. in x86 asm template.
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
                //   k (32), w (16), b (8-low), h (8-high), q (64), l (32-alt), c (raw constant)
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
                } else if matches!(chars[i], 'k' | 'w' | 'b' | 'h' | 'q' | 'l' | 'c') {
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
                                if modifier == Some('c') {
                                    // %c[name]: raw constant — emit immediate value or register without prefix
                                    if let Some(Some(imm)) = op_imm_values.get(idx) {
                                        result.push_str(&format!("{}", imm));
                                    } else {
                                        result.push_str(&op_regs[idx]);
                                    }
                                } else if let Some(Some(imm)) = op_imm_values.get(idx) {
                                    // Immediate operand — emit as $value
                                    result.push_str(&format!("${}", imm));
                                } else if op_is_memory[idx] {
                                    result.push_str(&op_mem_addrs[idx]);
                                } else {
                                    let effective_mod = modifier.or_else(|| Self::default_modifier_for_type(op_types.get(idx).copied()));
                                    result.push('%');
                                    result.push_str(&Self::format_x86_reg(&op_regs[idx], effective_mod));
                                }
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
                        if modifier == Some('c') {
                            // %c<N>: raw constant — emit immediate value or register without prefix
                            if let Some(Some(imm)) = op_imm_values.get(internal_idx) {
                                result.push_str(&format!("{}", imm));
                            } else {
                                result.push_str(&op_regs[internal_idx]);
                            }
                        } else if let Some(Some(imm)) = op_imm_values.get(internal_idx) {
                            // Immediate operand — emit as $value
                            result.push_str(&format!("${}", imm));
                        } else if op_is_memory[internal_idx] {
                            result.push_str(&op_mem_addrs[internal_idx]);
                        } else {
                            let effective_mod = modifier.or_else(|| Self::default_modifier_for_type(op_types.get(internal_idx).copied()));
                            result.push('%');
                            result.push_str(&Self::format_x86_reg(&op_regs[internal_idx], effective_mod));
                        }
                    } else {
                        result.push('%');
                        if let Some(m) = modifier { result.push(m); }
                        result.push_str(&format!("{}", num));
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
    pub(super) fn format_x86_reg(reg: &str, modifier: Option<char>) -> String {
        // XMM registers don't have size variants
        if reg.starts_with("xmm") {
            return reg.to_string();
        }
        match modifier {
            Some('k') | Some('l') => {
                // 32-bit version
                Self::reg_to_32(reg)
            }
            Some('w') => {
                // 16-bit version
                Self::reg_to_16(reg)
            }
            Some('b') => {
                // 8-bit low version
                Self::reg_to_8l(reg)
            }
            Some('h') => {
                // 8-bit high version
                Self::reg_to_8h(reg)
            }
            Some('q') | None => {
                // 64-bit (default for x86-64)
                reg.to_string()
            }
            _ => reg.to_string(),
        }
    }

    /// Convert 64-bit register name to 32-bit variant.
    pub(super) fn reg_to_32(reg: &str) -> String {
        match reg {
            "rax" => "eax".to_string(),
            "rbx" => "ebx".to_string(),
            "rcx" => "ecx".to_string(),
            "rdx" => "edx".to_string(),
            "rsi" => "esi".to_string(),
            "rdi" => "edi".to_string(),
            "rbp" => "ebp".to_string(),
            "rsp" => "esp".to_string(),
            _ if reg.starts_with('r') => format!("{}d", reg), // r8 -> r8d, r10 -> r10d
            _ => reg.to_string(),
        }
    }

    /// Convert 64-bit register name to 16-bit variant.
    pub(super) fn reg_to_16(reg: &str) -> String {
        match reg {
            "rax" => "ax".to_string(),
            "rbx" => "bx".to_string(),
            "rcx" => "cx".to_string(),
            "rdx" => "dx".to_string(),
            "rsi" => "si".to_string(),
            "rdi" => "di".to_string(),
            "rbp" => "bp".to_string(),
            "rsp" => "sp".to_string(),
            _ if reg.starts_with('r') => format!("{}w", reg), // r8 -> r8w
            _ => reg.to_string(),
        }
    }

    /// Convert 64-bit register name to 8-bit low variant.
    pub(super) fn reg_to_8l(reg: &str) -> String {
        match reg {
            "rax" => "al".to_string(),
            "rbx" => "bl".to_string(),
            "rcx" => "cl".to_string(),
            "rdx" => "dl".to_string(),
            "rsi" => "sil".to_string(),
            "rdi" => "dil".to_string(),
            _ if reg.starts_with('r') => format!("{}b", reg), // r8 -> r8b
            _ => reg.to_string(),
        }
    }

    /// Convert 64-bit register name to 8-bit high variant.
    pub(super) fn reg_to_8h(reg: &str) -> String {
        match reg {
            "rax" => "ah".to_string(),
            "rbx" => "bh".to_string(),
            "rcx" => "ch".to_string(),
            "rdx" => "dh".to_string(),
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
