//! i686 inline assembly template substitution and register formatting.
//!
//! The default register size is 32-bit (eax, etc.). Supports GCC-style modifiers
//! for size variants (w=16-bit, b=8-bit low, h=8-bit high) and special operand
//! forms (c=raw constant, P=raw symbol, a=address, n=negated immediate).

use std::borrow::Cow;
use crate::common::types::IrType;
use crate::ir::ir::BlockId;
use super::codegen::I686Codegen;

impl I686Codegen {
    /// Substitute %0, %1, %[name], %k0, %b1, %w2, %h3, %c0, %P0, %a0, %n0, %l[name] etc.
    /// in i686 asm template.
    ///
    /// On i686, the default register size is 32-bit (e.g., %eax for "=a" constraint).
    /// Modifiers: k (32-bit), w (16-bit), b (8-bit low), h (8-bit high),
    ///            c (raw constant), P (raw symbol), a (address), n (negated immediate)
    pub(super) fn substitute_i686_asm_operands(
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
                // Check for modifiers
                let mut modifier = None;
                if chars[i] == 'l' && i + 1 < chars.len() && chars[i + 1] == '[' && !goto_labels.is_empty() {
                    // %l[name] goto label reference
                    let saved_i = i;
                    i += 1; // skip 'l'
                    i += 1; // skip '['
                    let name_start = i;
                    while i < chars.len() && chars[i] != ']' { i += 1; }
                    let name: String = chars[name_start..i].iter().collect();
                    if i < chars.len() { i += 1; } // skip ']'
                    if let Some((_, block_id)) = goto_labels.iter().find(|(n, _)| n == &name) {
                        result.push_str(&block_id.to_string());
                        continue;
                    }
                    i = saved_i;
                    modifier = Some('l');
                    i += 1;
                } else if chars[i] == 'l' && i + 1 < chars.len() && chars[i + 1].is_ascii_digit() && !goto_labels.is_empty() {
                    // %l<N> goto label positional reference
                    let saved_i = i;
                    i += 1; // skip 'l'
                    let mut num = 0usize;
                    while i < chars.len() && chars[i].is_ascii_digit() {
                        num = num * 10 + (chars[i] as usize - '0' as usize);
                        i += 1;
                    }
                    let label_idx = num.wrapping_sub(op_regs.len());
                    if label_idx < goto_labels.len() {
                        result.push_str(&goto_labels[label_idx].1.to_string());
                        continue;
                    }
                    i = saved_i;
                    modifier = Some('l');
                    i += 1;
                } else if chars[i] == 'P' {
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
                    if i < chars.len() { i += 1; }

                    let mut found = false;
                    for (idx, op_name) in op_names.iter().enumerate() {
                        if let Some(ref n) = op_name {
                            if n == &name {
                                Self::emit_i686_operand(&mut result, idx, modifier,
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
                    let mut num = 0usize;
                    while i < chars.len() && chars[i].is_ascii_digit() {
                        num = num * 10 + (chars[i] as usize - '0' as usize);
                        i += 1;
                    }
                    let internal_idx = if num < gcc_to_internal.len() {
                        gcc_to_internal[num]
                    } else {
                        num
                    };
                    if internal_idx < op_regs.len() {
                        Self::emit_i686_operand(&mut result, internal_idx, modifier,
                            op_regs, op_is_memory, op_mem_addrs, op_types,
                            op_imm_values, op_imm_symbols);
                    } else {
                        result.push('%');
                        if let Some(m) = modifier { result.push(m); }
                        result.push_str(&num.to_string());
                    }
                } else {
                    // Not a recognized pattern (e.g., %eax, %ax, etc.)
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
    fn emit_i686_operand(
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
            if let Some(&imm) = has_imm {
                result.push_str(&imm.wrapping_neg().to_string());
            } else {
                result.push_str(&op_regs[idx]);
            }
        } else if is_raw {
            if let Some(sym) = has_symbol {
                result.push_str(sym);
            } else if let Some(imm) = has_imm {
                result.push_str(&imm.to_string());
            } else if op_is_memory[idx] {
                result.push_str(&op_mem_addrs[idx]);
            } else {
                result.push_str(&op_regs[idx]);
            }
        } else if is_addr {
            if let Some(sym) = has_symbol {
                result.push_str(sym);
            } else if let Some(imm) = has_imm {
                result.push_str(&imm.to_string());
            } else if op_is_memory[idx] {
                result.push_str(&op_mem_addrs[idx]);
            } else {
                result.push_str(&format!("(%{})", op_regs[idx]));
            }
        } else if op_is_memory[idx] {
            result.push_str(&op_mem_addrs[idx]);
        } else if let Some(sym) = has_symbol {
            result.push_str(&format!("${}", sym));
        } else if let Some(imm) = has_imm {
            result.push_str(&format!("${}", imm));
        } else {
            let effective_mod = modifier.or_else(|| Self::i686_default_modifier_for_type(op_types.get(idx).copied()));
            result.push('%');
            result.push_str(&Self::format_i686_reg(&op_regs[idx], effective_mod));
        }
    }

    /// Determine the default register size modifier based on the operand's IR type.
    /// On i686, the default is 32-bit, so only smaller types get a modifier.
    fn i686_default_modifier_for_type(ty: Option<IrType>) -> Option<char> {
        match ty {
            Some(IrType::I8) | Some(IrType::U8) => Some('b'),
            Some(IrType::I16) | Some(IrType::U16) => Some('w'),
            // 32-bit is the default on i686
            _ => None,
        }
    }

    /// Format i686 register with size modifier.
    /// On i686, default (no modifier or 'k') is 32-bit.
    fn format_i686_reg<'a>(reg: &'a str, modifier: Option<char>) -> Cow<'a, str> {
        if reg.starts_with("xmm") || reg.starts_with("st(") || reg == "st" {
            return Cow::Borrowed(reg);
        }
        match modifier {
            Some('w') => Self::reg_to_16(reg),
            Some('b') => Self::reg_to_8l(reg),
            Some('h') => Self::reg_to_8h(reg),
            // 'k', 'l', 'q', or no modifier => 32-bit (no 64-bit on i686)
            _ => Self::reg_to_32(reg),
        }
    }
}
