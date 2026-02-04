//! ELF relocatable object file writer for x86-64.
//!
//! Produces .o files from parsed and encoded assembly. Handles:
//! - Multiple sections (.text, .data, .rodata, .bss, etc.)
//! - Symbol table with local/global/weak symbols
//! - Relocations (R_X86_64_PC32, R_X86_64_PLT32, R_X86_64_32S, etc.)
//! - Section flags and types

use std::collections::HashMap;
use super::parser::*;
use super::encoder::*;
use crate::backend::elf::{self as elf_mod,
    SHT_PROGBITS,
    SHF_ALLOC, SHF_EXECINSTR,
    STB_LOCAL, STB_GLOBAL, STB_WEAK,
    STT_NOTYPE, STT_OBJECT, STT_FUNC, STT_TLS,
    STV_DEFAULT, STV_INTERNAL, STV_HIDDEN, STV_PROTECTED,
    ELFCLASS64, EM_X86_64,
    resolve_numeric_labels, parse_section_flags,
    ElfConfig, ObjSection, ObjSymbol, ObjReloc, SymbolTableInput,
};

/// Tracks a jump instruction for relaxation (long -> short).
#[derive(Clone, Debug)]
struct JumpInfo {
    /// Offset of the start of the jump instruction in section data.
    offset: usize,
    /// Total length of the instruction (5 for JMP rel32, 6 for Jcc rel32).
    len: usize,
    /// Target label name.
    target: String,
    /// Whether this is a conditional jump (Jcc) vs unconditional (JMP).
    is_conditional: bool,
    /// Whether this jump has already been relaxed to short form.
    relaxed: bool,
}

/// Tracks a `.org` directive so padding can be recalculated after jump relaxation.
/// When jumps are shortened by relaxation, code before a `.org` region shrinks but
/// the padding stays the same size, causing misalignment. After relaxation,
/// we need to recalculate padding to maintain the correct absolute position.
#[derive(Clone, Debug)]
struct OrgInfo {
    /// Offset in section data where the org directive was processed.
    /// This is where NOP padding was (or would be) inserted.
    data_offset: usize,
    /// The label whose position anchors this org target.
    base_label: String,
    /// The offset from the base label (org target = label_pos + base_offset).
    base_offset: i64,
}

/// Tracks a section being built.
struct Section {
    name: String,
    section_type: u32,
    flags: u64,
    data: Vec<u8>,
    alignment: u64,
    /// Relocations for this section.
    relocations: Vec<ElfRelocation>,
    /// Jump instructions eligible for short-form relaxation.
    jumps: Vec<JumpInfo>,
    /// `.org` padding regions that may need adjustment after jump relaxation.
    org_regions: Vec<OrgInfo>,
    /// Index in the final section header table.
    #[allow(dead_code)]
    index: usize,
    /// COMDAT group name, if this section is part of a COMDAT group.
    comdat_group: Option<String>,
}

#[derive(Clone)]
struct ElfRelocation {
    offset: u64,
    symbol: String,
    reloc_type: u32,
    addend: i64,
    /// For symbol-difference relocations (.long a - b), stores the `b` symbol.
    /// The ELF writer resolves this: addend += offset_in_section - offset_of(b)
    diff_symbol: Option<String>,
    /// Size of the data to patch (1 for .byte, 2 for .2byte, 4 for .long, 8 for .quad).
    /// Used by resolve_internal_relocations to know how many bytes to write.
    patch_size: u8,
}

/// Symbol info collected during assembly.
struct SymbolInfo {
    name: String,
    binding: u8,
    sym_type: u8,
    visibility: u8,
    section: Option<String>, // section name, or None for undefined/common
    value: u64,
    size: u64,
    /// For .comm symbols
    is_common: bool,
    common_align: u32,
}

/// Builds an ELF relocatable object file from parsed assembly items.
pub struct ElfWriter {
    sections: Vec<Section>,
    symbols: Vec<SymbolInfo>,
    /// Map from section name to index in `sections`.
    section_map: HashMap<String, usize>,
    /// Map from symbol name to index in `symbols`.
    symbol_map: HashMap<String, usize>,
    /// Current section index.
    current_section: Option<usize>,
    /// Map from label name to (section_index, offset).
    label_positions: HashMap<String, (usize, u64)>,
    /// All positions for numeric labels (for 1b/1f resolution).
    /// Maps label name to Vec of (section_index, offset).
    numeric_label_positions: HashMap<String, Vec<(usize, u64)>>,
    /// Pending symbol attributes (.globl, .type, .size, etc.) that apply to the next label.
    pending_globals: Vec<String>,
    pending_weaks: Vec<String>,
    pending_types: HashMap<String, SymbolKind>,
    pending_sizes: HashMap<String, SizeExpr>,
    pending_hidden: Vec<String>,
    pending_protected: Vec<String>,
    pending_internal: Vec<String>,
    /// Set aliases.
    aliases: HashMap<String, String>,
    /// Section stack for .pushsection/.popsection.
    section_stack: Vec<Option<usize>>,
    /// Deferred `.skip` expressions: (section_index, offset_in_section, expression, fill_byte).
    /// Evaluated after all labels are known.
    deferred_skips: Vec<(usize, usize, String, u8)>,
    /// Deferred byte-sized symbol diffs: (section_index, offset_in_section, sym_a, sym_b, size).
    /// Resolved after deferred skips are inserted (since skip insertion shifts offsets).
    deferred_byte_diffs: Vec<(usize, usize, String, String, usize)>,
}

// Numeric label resolution functions (is_numeric_label, parse_numeric_ref,
// resolve_numeric_labels, etc.) are in crate::backend::elf and imported above.

/// Token in an expression for the deferred expression evaluator.
#[derive(Debug, Clone, PartialEq)]
enum ExprToken {
    Number(i64),
    Symbol(String),
    Plus,
    Minus,
    Star,
    LParen,
    RParen,
    Lt,
    Gt,
    And,
    Or,
    Xor,
    Not,
}

/// Tokenize an expression string into ExprToken values.
fn tokenize_expr(expr: &str) -> Result<Vec<ExprToken>, String> {
    let mut tokens = Vec::new();
    let bytes = expr.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        match bytes[i] {
            b' ' | b'\t' => { i += 1; }
            b'+' => { tokens.push(ExprToken::Plus); i += 1; }
            b'-' => { tokens.push(ExprToken::Minus); i += 1; }
            b'*' => { tokens.push(ExprToken::Star); i += 1; }
            b'(' => { tokens.push(ExprToken::LParen); i += 1; }
            b')' => { tokens.push(ExprToken::RParen); i += 1; }
            b'<' => { tokens.push(ExprToken::Lt); i += 1; }
            b'>' => { tokens.push(ExprToken::Gt); i += 1; }
            b'&' => { tokens.push(ExprToken::And); i += 1; }
            b'|' => { tokens.push(ExprToken::Or); i += 1; }
            b'^' => { tokens.push(ExprToken::Xor); i += 1; }
            b'~' => { tokens.push(ExprToken::Not); i += 1; }
            b'0'..=b'9' => {
                let start = i;
                // Check for hex prefix
                if i + 1 < bytes.len() && bytes[i] == b'0' && (bytes[i+1] == b'x' || bytes[i+1] == b'X') {
                    i += 2;
                    while i < bytes.len() && bytes[i].is_ascii_hexdigit() {
                        i += 1;
                    }
                } else {
                    while i < bytes.len() && bytes[i].is_ascii_digit() {
                        i += 1;
                    }
                }
                let num_str = &expr[start..i];
                let val = if num_str.starts_with("0x") || num_str.starts_with("0X") {
                    i64::from_str_radix(&num_str[2..], 16)
                        .map_err(|_| format!("bad hex number: {}", num_str))?
                } else {
                    num_str.parse::<i64>()
                        .map_err(|_| format!("bad number: {}", num_str))?
                };
                tokens.push(ExprToken::Number(val));
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'.' => {
                let start = i;
                while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_' || bytes[i] == b'.') {
                    i += 1;
                }
                tokens.push(ExprToken::Symbol(expr[start..i].to_string()));
            }
            c => return Err(format!("unexpected character in expression: '{}' (0x{:02x})", c as char, c)),
        }
    }

    Ok(tokens)
}

impl ElfWriter {
    pub fn new() -> Self {
        ElfWriter {
            sections: Vec::new(),
            symbols: Vec::new(),
            section_map: HashMap::new(),
            symbol_map: HashMap::new(),
            current_section: None,
            label_positions: HashMap::new(),
            numeric_label_positions: HashMap::new(),
            pending_globals: Vec::new(),
            pending_weaks: Vec::new(),
            pending_types: HashMap::new(),
            pending_sizes: HashMap::new(),
            pending_hidden: Vec::new(),
            pending_protected: Vec::new(),
            pending_internal: Vec::new(),
            aliases: HashMap::new(),
            section_stack: Vec::new(),
            deferred_skips: Vec::new(),
            deferred_byte_diffs: Vec::new(),
        }
    }

    /// Build the ELF object file from parsed assembly items.
    pub fn build(mut self, items: &[AsmItem]) -> Result<Vec<u8>, String> {
        // Resolve numeric local labels (1:, 2:, etc.) to unique names
        let items = resolve_numeric_labels(items);

        // First pass: process all items
        for item in &items {
            self.process_item(item)?;
        }

        // Second pass: resolve internal labels and build the ELF
        self.emit_elf()
    }

    fn get_or_create_section(&mut self, name: &str, section_type: u32, flags: u64, comdat_group: Option<String>) -> usize {
        if let Some(&idx) = self.section_map.get(name) {
            return idx;
        }
        let idx = self.sections.len();
        self.sections.push(Section {
            name: name.to_string(),
            section_type,
            flags,
            data: Vec::new(),
            alignment: if flags & SHF_EXECINSTR != 0 && name != ".init" && name != ".fini" { 16 } else { 1 },
            relocations: Vec::new(),
            jumps: Vec::new(),
            org_regions: Vec::new(),
            index: 0, // will be set later
            comdat_group,
        });
        self.section_map.insert(name.to_string(), idx);
        idx
    }

    fn current_section_mut(&mut self) -> Result<&mut Section, String> {
        let idx = self.current_section.ok_or("no active section")?;
        Ok(&mut self.sections[idx])
    }

    fn switch_section(&mut self, dir: &SectionDirective) {
        let (section_type, flags) = parse_section_flags(&dir.name, dir.flags.as_deref(), dir.section_type.as_deref());
        let idx = self.get_or_create_section(&dir.name, section_type, flags, dir.comdat_group.clone());
        self.current_section = Some(idx);
    }

    fn process_item(&mut self, item: &AsmItem) -> Result<(), String> {
        match item {
            AsmItem::Section(dir) => {
                self.switch_section(dir);
            }
            AsmItem::PushSection(dir) => {
                // Save the current section and switch to the new one
                self.section_stack.push(self.current_section);
                self.switch_section(dir);
            }
            AsmItem::PopSection => {
                // Restore the previous section from the stack
                if let Some(prev) = self.section_stack.pop() {
                    self.current_section = prev;
                }
                // If stack is empty, silently keep current section (matches GNU as behavior)
            }
            AsmItem::Global(name) => {
                self.pending_globals.push(name.clone());
            }
            AsmItem::Weak(name) => {
                self.pending_weaks.push(name.clone());
            }
            AsmItem::Hidden(name) => {
                self.pending_hidden.push(name.clone());
            }
            AsmItem::Protected(name) => {
                self.pending_protected.push(name.clone());
            }
            AsmItem::Internal(name) => {
                self.pending_internal.push(name.clone());
            }
            AsmItem::SymbolType(name, kind) => {
                self.pending_types.insert(name.clone(), *kind);
            }
            AsmItem::Size(name, expr) => {
                // For CurrentMinusSymbol (.-sym), record the current position
                // as an internal label so that after jump relaxation we can
                // compute the correct size from updated label positions.
                let resolved = match expr {
                    SizeExpr::CurrentMinusSymbol(start_sym) => {
                        if let Some(sec_idx) = self.current_section {
                            let current_off = self.sections[sec_idx].data.len() as u64;
                            let end_label = format!(".Lsize_end_{}", name);
                            self.label_positions.insert(end_label.clone(), (sec_idx, current_off));
                            SizeExpr::SymbolDiff(end_label, start_sym.clone())
                        } else {
                            expr.clone()
                        }
                    }
                    other => other.clone(),
                };
                self.pending_sizes.insert(name.clone(), resolved);
            }
            AsmItem::Label(name) => {
                self.ensure_section()?;
                let sec_idx = self.current_section.unwrap();
                let offset = self.sections[sec_idx].data.len() as u64;
                self.label_positions.insert(name.clone(), (sec_idx, offset));

                // Track numeric label positions for 1b/1f resolution
                if name.chars().all(|c| c.is_ascii_digit()) {
                    self.numeric_label_positions
                        .entry(name.clone())
                        .or_default()
                        .push((sec_idx, offset));
                }

                // Create/update symbol
                self.ensure_symbol(name, sec_idx, offset);
            }
            AsmItem::Align(n) => {
                if let Some(sec_idx) = self.current_section {
                    let section = &mut self.sections[sec_idx];
                    let align = *n as u64;
                    if align > section.alignment {
                        section.alignment = align;
                    }
                    // Pad to alignment
                    let current = section.data.len() as u64;
                    let aligned = (current + align - 1) & !(align - 1);
                    let padding = (aligned - current) as usize;
                    if section.flags & SHF_EXECINSTR != 0 {
                        // NOP-fill for code sections
                        section.data.extend(std::iter::repeat_n(0x90, padding));
                    } else {
                        section.data.extend(std::iter::repeat_n(0, padding));
                    }
                }
            }
            AsmItem::Byte(vals) => {
                self.emit_data_values(vals, 1)?;
            }
            AsmItem::Short(vals) => {
                self.emit_data_values(vals, 2)?;
            }
            AsmItem::Long(vals) => {
                self.emit_data_values(vals, 4)?;
            }
            AsmItem::Quad(vals) => {
                self.emit_data_values(vals, 8)?;
            }
            AsmItem::Zero(n) => {
                let section = self.current_section_mut()?;
                section.data.extend(std::iter::repeat_n(0u8, *n as usize));
            }
            AsmItem::Org(sym, offset) => {
                if let Some(sec_idx) = self.current_section {
                    let current = self.sections[sec_idx].data.len() as u64;
                    let target = if sym.is_empty() {
                        *offset as u64
                    } else if let Some(&(label_sec, label_off)) = self.label_positions.get(sym.as_str()) {
                        if label_sec == sec_idx {
                            (label_off as i64 + offset) as u64
                        } else {
                            return Err(format!(".org symbol {} not in current section", sym));
                        }
                    } else if let Some((label_sec, label_off)) = self.resolve_numeric_label(sym, current, sec_idx) {
                        if label_sec == sec_idx {
                            (label_off as i64 + offset) as u64
                        } else {
                            return Err(format!(".org symbol {} not in current section", sym));
                        }
                    } else {
                        return Err(format!(".org: unknown symbol {}", sym));
                    };
                    // Record org region for post-relaxation fixup (always, even if no padding needed now)
                    if !sym.is_empty() {
                        self.sections[sec_idx].org_regions.push(OrgInfo {
                            data_offset: current as usize,
                            base_label: sym.clone(),
                            base_offset: *offset,
                        });
                    }
                    if target > current {
                        let padding = (target - current) as usize;
                        let fill = if self.sections[sec_idx].flags & SHF_EXECINSTR != 0 { 0x90u8 } else { 0u8 };
                        self.sections[sec_idx].data.extend(std::iter::repeat_n(fill, padding));
                    }
                }
            }
            AsmItem::SkipExpr(expr, fill) => {
                // Record a deferred skip - will be evaluated after all labels are known
                let sec_idx = self.current_section.ok_or("no active section for .skip")?;
                let offset = self.sections[sec_idx].data.len();
                self.deferred_skips.push((sec_idx, offset, expr.clone(), *fill));
            }
            AsmItem::Asciz(bytes) | AsmItem::Ascii(bytes) => {
                let section = self.current_section_mut()?;
                section.data.extend_from_slice(bytes);
            }
            AsmItem::Comm(name, size, align) => {
                // Common symbol
                let sym_idx = self.symbols.len();
                self.symbols.push(SymbolInfo {
                    name: name.clone(),
                    binding: STB_GLOBAL,
                    sym_type: STT_OBJECT,
                    visibility: STV_DEFAULT,
                    section: None,
                    value: *align as u64,  // For common, value is alignment
                    size: *size,
                    is_common: true,
                    common_align: *align,
                });
                self.symbol_map.insert(name.clone(), sym_idx);
            }
            AsmItem::Set(alias, target) => {
                self.aliases.insert(alias.clone(), target.clone());
            }
            AsmItem::Instruction(instr) => {
                self.encode_instruction(instr)?;
            }
            // Ignored items
            AsmItem::Cfi(_) | AsmItem::File(_, _) | AsmItem::Loc(_, _, _)
            | AsmItem::OptionDirective(_) | AsmItem::Empty => {}
        }
        Ok(())
    }

    fn ensure_section(&mut self) -> Result<(), String> {
        if self.current_section.is_none() {
            // Default to .text
            let idx = self.get_or_create_section(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, None);
            self.current_section = Some(idx);
        }
        Ok(())
    }

    fn ensure_symbol(&mut self, name: &str, sec_idx: usize, offset: u64) {
        let sec_name = self.sections[sec_idx].name.clone();

        if let Some(&sym_idx) = self.symbol_map.get(name) {
            // Update existing symbol
            let sym = &mut self.symbols[sym_idx];
            sym.section = Some(sec_name);
            sym.value = offset;
        } else {
            // Create new symbol
            let binding = if self.pending_globals.contains(&name.to_string()) {
                STB_GLOBAL
            } else if self.pending_weaks.contains(&name.to_string()) {
                STB_WEAK
            } else {
                STB_LOCAL
            };

            let sym_type = match self.pending_types.get(name) {
                Some(SymbolKind::Function) => STT_FUNC,
                Some(SymbolKind::Object) => STT_OBJECT,
                Some(SymbolKind::TlsObject) => STT_TLS,
                Some(SymbolKind::NoType) | None => STT_NOTYPE,
            };

            let visibility = if self.pending_hidden.contains(&name.to_string()) {
                STV_HIDDEN
            } else if self.pending_protected.contains(&name.to_string()) {
                STV_PROTECTED
            } else if self.pending_internal.contains(&name.to_string()) {
                STV_INTERNAL
            } else {
                STV_DEFAULT
            };

            let sym_idx = self.symbols.len();
            self.symbols.push(SymbolInfo {
                name: name.to_string(),
                binding,
                sym_type,
                visibility,
                section: Some(sec_name),
                value: offset,
                size: 0,
                is_common: false,
                common_align: 0,
            });
            self.symbol_map.insert(name.to_string(), sym_idx);
        }
    }

    fn emit_data_values(&mut self, vals: &[DataValue], size: usize) -> Result<(), String> {
        let sec_idx = self.current_section.ok_or("no active section")?;

        for val in vals {
            match val {
                DataValue::Integer(v) => {
                    let section = &mut self.sections[sec_idx];
                    match size {
                        1 => section.data.push(*v as u8),
                        2 => section.data.extend_from_slice(&(*v as i16).to_le_bytes()),
                        4 => section.data.extend_from_slice(&(*v as i32).to_le_bytes()),
                        _ => section.data.extend_from_slice(&v.to_le_bytes()),
                    }
                }
                DataValue::Symbol(sym) => {
                    // Resolve .set aliases: if sym is a `.set` alias for a label-difference
                    // expression (e.g., `.set .Lset0, .LECIE-.LSCIE`), emit as SymbolDiff.
                    if let Some(target) = self.aliases.get(sym).cloned() {
                        if let Some(pos) = target.find('-') {
                            let a = target[..pos].trim().to_string();
                            let b = target[pos+1..].trim().to_string();
                            let offset = self.sections[sec_idx].data.len() as u64;
                            self.sections[sec_idx].relocations.push(ElfRelocation {
                                offset,
                                symbol: a,
                                reloc_type: if size <= 4 { R_X86_64_PC32 } else { R_X86_64_64 },
                                addend: 0,
                                diff_symbol: Some(b),
                                patch_size: size as u8,
                            });
                            let section = &mut self.sections[sec_idx];
                            section.data.extend(std::iter::repeat_n(0, size));
                            continue;
                        }
                    }
                    let offset = self.sections[sec_idx].data.len() as u64;
                    let reloc_type = if size == 4 { R_X86_64_32 } else { R_X86_64_64 };
                    self.sections[sec_idx].relocations.push(ElfRelocation {
                        offset,
                        symbol: sym.clone(),
                        reloc_type,
                        addend: 0,
                        diff_symbol: None,
                        patch_size: size as u8,
                    });
                    let section = &mut self.sections[sec_idx];
                    section.data.extend(std::iter::repeat_n(0, size));
                }
                DataValue::SymbolOffset(sym, addend) => {
                    let offset = self.sections[sec_idx].data.len() as u64;
                    let reloc_type = if size == 4 { R_X86_64_32 } else { R_X86_64_64 };
                    self.sections[sec_idx].relocations.push(ElfRelocation {
                        offset,
                        symbol: sym.clone(),
                        reloc_type,
                        addend: *addend,
                        diff_symbol: None,
                        patch_size: size as u8,
                    });
                    let section = &mut self.sections[sec_idx];
                    section.data.extend(std::iter::repeat_n(0, size));
                }
                DataValue::SymbolDiff(a, b) => {
                    let offset = self.sections[sec_idx].data.len() as u64;
                    // Resolve .set aliases in both operands
                    let a_resolved = self.aliases.get(a).cloned().unwrap_or_else(|| a.clone());
                    let b_resolved = self.aliases.get(b).cloned().unwrap_or_else(|| b.clone());

                    if b_resolved == "." {
                        // `sym - .` means PC-relative: emit a PC32 relocation.
                        self.sections[sec_idx].relocations.push(ElfRelocation {
                            offset,
                            symbol: a_resolved,
                            reloc_type: R_X86_64_PC32,
                            addend: 0,
                            diff_symbol: None,
                            patch_size: size as u8,
                        });
                        let section = &mut self.sections[sec_idx];
                        section.data.extend(std::iter::repeat_n(0, size));
                    } else if size <= 2 {
                        // For byte/short-sized diffs, defer resolution until after
                        // deferred skips are inserted (skip insertion shifts offsets).
                        let offset_usize = self.sections[sec_idx].data.len();
                        self.deferred_byte_diffs.push((sec_idx, offset_usize, a_resolved, b_resolved, size));
                        let section = &mut self.sections[sec_idx];
                        section.data.extend(std::iter::repeat_n(0, size));
                    } else {
                        // For `.long a - b` or `.quad a - b`:
                        // Store as a relocation with diff_symbol set.
                        self.sections[sec_idx].relocations.push(ElfRelocation {
                            offset,
                            symbol: a_resolved,
                            reloc_type: if size == 4 { R_X86_64_PC32 } else { R_X86_64_64 },
                            addend: 0,
                            diff_symbol: Some(b_resolved),
                            patch_size: size as u8,
                        });
                        let section = &mut self.sections[sec_idx];
                        section.data.extend(std::iter::repeat_n(0, size));
                    }
                }
            }
        }
        Ok(())
    }

    fn encode_instruction(&mut self, instr: &Instruction) -> Result<(), String> {
        self.ensure_section()?;
        let sec_idx = self.current_section.unwrap();

        let mut encoder = InstructionEncoder::new();
        encoder.offset = self.sections[sec_idx].data.len() as u64;
        encoder.encode(instr)?;

        // Copy encoded bytes to section
        let base_offset = self.sections[sec_idx].data.len() as u64;
        let instr_len = encoder.bytes.len();
        self.sections[sec_idx].data.extend_from_slice(&encoder.bytes);

        // Detect jump instructions eligible for short-form relaxation.
        // Long JMP rel32 = E9 xx xx xx xx (5 bytes)
        // Long Jcc rel32 = 0F 8x xx xx xx xx (6 bytes)
        // Only consider jumps to labels (they'll have a PC32 relocation).
        if let Some(ref label) = self.get_jump_target_label(instr) {
            let is_conditional = instr.mnemonic != "jmp";
            let expected_len = if is_conditional { 6 } else { 5 };
            if instr_len == expected_len {
                self.sections[sec_idx].jumps.push(JumpInfo {
                    offset: base_offset as usize,
                    len: expected_len,
                    target: label.clone(),
                    is_conditional,
                    relaxed: false,
                });
            }
        }

        // Copy relocations, adjusting offsets
        for reloc in encoder.relocations {
            self.sections[sec_idx].relocations.push(ElfRelocation {
                offset: base_offset + reloc.offset,
                symbol: reloc.symbol,
                reloc_type: reloc.reloc_type,
                addend: reloc.addend,
                diff_symbol: None,
                patch_size: 4,
            });
        }

        Ok(())
    }

    /// Extract the target label from a jump instruction, if any.
    fn get_jump_target_label(&self, instr: &Instruction) -> Option<String> {
        let mnem = &instr.mnemonic;
        let is_jump = mnem.starts_with('j') && mnem.len() >= 2;
        if !is_jump {
            return None;
        }
        if instr.operands.len() != 1 {
            return None;
        }
        if let Operand::Label(label) = &instr.operands[0] {
            Some(label.clone())
        } else {
            None
        }
    }

    /// Resolve deferred `.skip` expressions after all labels are known.
    /// Evaluates the expression, inserts the fill bytes, and adjusts all subsequent
    /// label positions and relocation offsets in the same section.
    fn resolve_deferred_skips(&mut self) -> Result<(), String> {
        // Process in reverse order within each section so earlier insertions
        // don't invalidate the offsets of later ones.
        let mut skips = std::mem::take(&mut self.deferred_skips);
        // Sort by (section_index, offset) in reverse order
        skips.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)).reverse());

        for (sec_idx, offset, expr, fill) in &skips {
            // Evaluate the expression
            let val = self.evaluate_expr(expr)?;
            let count = if val < 0 { 0usize } else { val as usize };

            if count == 0 {
                continue;
            }

            // Insert `count` fill bytes at `offset` in the section
            let fill_bytes: Vec<u8> = vec![*fill; count];
            self.sections[*sec_idx].data.splice(*offset..*offset, fill_bytes);

            // Adjust label positions: any label in this section at or after `offset` shifts by `count`
            for (_, (lsec, loff)) in self.label_positions.iter_mut() {
                if *lsec == *sec_idx && (*loff as usize) >= *offset {
                    *loff += count as u64;
                }
            }

            // Adjust numeric label positions
            for (_, positions) in self.numeric_label_positions.iter_mut() {
                for (lsec, loff) in positions.iter_mut() {
                    if *lsec == *sec_idx && (*loff as usize) >= *offset {
                        *loff += count as u64;
                    }
                }
            }

            // Adjust relocation offsets
            for reloc in self.sections[*sec_idx].relocations.iter_mut() {
                if (reloc.offset as usize) >= *offset {
                    reloc.offset += count as u64;
                }
            }

            // Adjust jump offsets
            for jump in self.sections[*sec_idx].jumps.iter_mut() {
                if jump.offset >= *offset {
                    jump.offset += count;
                }
            }

            // Adjust other deferred byte diffs in the same section
            for (bsec, boff, _, _, _) in self.deferred_byte_diffs.iter_mut() {
                if *bsec == *sec_idx && *boff >= *offset {
                    *boff += count;
                }
            }
        }

        Ok(())
    }

    /// Resolve deferred byte-sized symbol diffs after skip resolution.
    fn resolve_deferred_byte_diffs(&mut self) -> Result<(), String> {
        let diffs = std::mem::take(&mut self.deferred_byte_diffs);
        for (sec_idx, offset, sym_a, sym_b, size) in &diffs {
            let pos_a = self.label_positions.get(sym_a)
                .ok_or_else(|| format!("undefined label in .byte diff: {}", sym_a))?;
            let pos_b = self.label_positions.get(sym_b)
                .ok_or_else(|| format!("undefined label in .byte diff: {}", sym_b))?;

            if pos_a.0 != pos_b.0 {
                return Err(format!("cross-section .byte diff: {} - {}", sym_a, sym_b));
            }

            let diff = (pos_a.1 as i64) - (pos_b.1 as i64);
            match size {
                1 => {
                    self.sections[*sec_idx].data[*offset] = diff as u8;
                }
                2 => {
                    let bytes = (diff as i16).to_le_bytes();
                    self.sections[*sec_idx].data[*offset] = bytes[0];
                    self.sections[*sec_idx].data[*offset + 1] = bytes[1];
                }
                _ => unreachable!(),
            }
        }
        Ok(())
    }

    /// Evaluate a `.skip` expression string using known label positions.
    /// Supports arithmetic: +, -, *, negation, comparisons (<, >), bitwise (&, |, ^).
    /// Uses GNU assembler convention: comparisons return -1 (all ones) for true, 0 for false.
    fn evaluate_expr(&self, expr: &str) -> Result<i64, String> {
        let expr = expr.trim();
        let tokens = tokenize_expr(expr)?;
        let mut pos = 0;
        let result = self.parse_expr_or(&tokens, &mut pos)?;
        if pos < tokens.len() {
            return Err(format!("unexpected token in expression at position {}: {:?}", pos, tokens.get(pos)));
        }
        Ok(result)
    }

    fn parse_expr_or(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        let mut val = self.parse_expr_xor(tokens, pos)?;
        while *pos < tokens.len() {
            match tokens[*pos] {
                ExprToken::Or => { *pos += 1; val |= self.parse_expr_xor(tokens, pos)?; }
                _ => break,
            }
        }
        Ok(val)
    }

    fn parse_expr_xor(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        let mut val = self.parse_expr_and(tokens, pos)?;
        while *pos < tokens.len() {
            match tokens[*pos] {
                ExprToken::Xor => { *pos += 1; val ^= self.parse_expr_and(tokens, pos)?; }
                _ => break,
            }
        }
        Ok(val)
    }

    fn parse_expr_and(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        let mut val = self.parse_expr_cmp(tokens, pos)?;
        while *pos < tokens.len() {
            match tokens[*pos] {
                ExprToken::And => { *pos += 1; val &= self.parse_expr_cmp(tokens, pos)?; }
                _ => break,
            }
        }
        Ok(val)
    }

    fn parse_expr_cmp(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        let mut val = self.parse_expr_add(tokens, pos)?;
        while *pos < tokens.len() {
            match tokens[*pos] {
                ExprToken::Lt => {
                    *pos += 1;
                    let rhs = self.parse_expr_add(tokens, pos)?;
                    // GNU as: comparisons return -1 for true, 0 for false
                    val = if val < rhs { -1 } else { 0 };
                }
                ExprToken::Gt => {
                    *pos += 1;
                    let rhs = self.parse_expr_add(tokens, pos)?;
                    val = if val > rhs { -1 } else { 0 };
                }
                _ => break,
            }
        }
        Ok(val)
    }

    fn parse_expr_add(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        let mut val = self.parse_expr_mul(tokens, pos)?;
        while *pos < tokens.len() {
            match tokens[*pos] {
                ExprToken::Plus => { *pos += 1; val = val.wrapping_add(self.parse_expr_mul(tokens, pos)?); }
                ExprToken::Minus => { *pos += 1; val = val.wrapping_sub(self.parse_expr_mul(tokens, pos)?); }
                _ => break,
            }
        }
        Ok(val)
    }

    fn parse_expr_mul(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        let mut val = self.parse_expr_unary(tokens, pos)?;
        while *pos < tokens.len() {
            match tokens[*pos] {
                ExprToken::Star => { *pos += 1; val = val.wrapping_mul(self.parse_expr_unary(tokens, pos)?); }
                _ => break,
            }
        }
        Ok(val)
    }

    fn parse_expr_unary(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        if *pos < tokens.len() {
            match tokens[*pos] {
                ExprToken::Minus => {
                    *pos += 1;
                    let val = self.parse_expr_unary(tokens, pos)?;
                    Ok(-val)
                }
                ExprToken::Plus => {
                    *pos += 1;
                    self.parse_expr_unary(tokens, pos)
                }
                ExprToken::Not => {
                    *pos += 1;
                    let val = self.parse_expr_unary(tokens, pos)?;
                    Ok(!val)
                }
                _ => self.parse_expr_primary(tokens, pos),
            }
        } else {
            Err("unexpected end of expression".to_string())
        }
    }

    fn parse_expr_primary(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        if *pos >= tokens.len() {
            return Err("unexpected end of expression".to_string());
        }
        match &tokens[*pos] {
            ExprToken::Number(n) => {
                *pos += 1;
                Ok(*n)
            }
            ExprToken::Symbol(name) => {
                *pos += 1;
                if let Some(&(_, offset)) = self.label_positions.get(name.as_str()) {
                    Ok(offset as i64)
                } else {
                    Err(format!("undefined symbol in expression: {}", name))
                }
            }
            ExprToken::LParen => {
                *pos += 1;
                let val = self.parse_expr_or(tokens, pos)?;
                if *pos < tokens.len() && tokens[*pos] == ExprToken::RParen {
                    *pos += 1;
                } else {
                    return Err("missing closing parenthesis".to_string());
                }
                Ok(val)
            }
            other => Err(format!("unexpected token: {:?}", other)),
        }
    }

    /// Emit the final ELF object file.
    fn emit_elf(mut self) -> Result<Vec<u8>, String> {
        // Relax long jumps to short jumps where possible.
        // This must happen BEFORE resolving sizes and updating symbol values,
        // because relaxation changes label positions and section data lengths.
        self.relax_jumps();

        // Resolve deferred .skip expressions (label arithmetic) - inserts bytes and shifts labels.
        self.resolve_deferred_skips()?;

        // Resolve deferred byte-sized symbol diffs (must happen after skip resolution).
        self.resolve_deferred_byte_diffs()?;

        // Resolve internal relocations (labels within the same section)
        self.resolve_internal_relocations();

        // Convert to shared ObjSection/ObjSymbol format and delegate to shared writer.
        // The x86 assembler uses section symbols for .L* label relocations:
        // when a relocation references a .L* label, we convert it to reference the
        // section name (so the shared writer maps it to the section symbol) with the
        // label's offset baked into the addend.
        let section_names: Vec<String> = self.sections.iter().map(|s| s.name.clone()).collect();

        let mut shared_sections: HashMap<String, ObjSection> = HashMap::new();
        for sec in &self.sections {
            let mut relocs = Vec::new();
            for reloc in &sec.relocations {
                let (sym_name, mut addend) = if reloc.symbol.starts_with('.') {
                    // Internal label (.L*, .Lstr*, etc.): convert to section symbol + offset
                    if let Some(&(target_sec, target_off)) = self.label_positions.get(&reloc.symbol) {
                        (section_names[target_sec].clone(), reloc.addend + target_off as i64)
                    } else {
                        (reloc.symbol.clone(), reloc.addend)
                    }
                } else {
                    (reloc.symbol.clone(), reloc.addend)
                };

                // Handle symbol-difference relocations (.long a - b)
                if let Some(ref diff_sym) = reloc.diff_symbol {
                    if let Some(&(_b_sec, b_off)) = self.label_positions.get(diff_sym) {
                        addend += reloc.offset as i64 - b_off as i64;
                    }
                }

                relocs.push(ObjReloc {
                    offset: reloc.offset,
                    reloc_type: reloc.reloc_type,
                    symbol_name: sym_name,
                    addend,
                });
            }
            shared_sections.insert(sec.name.clone(), ObjSection {
                name: sec.name.clone(),
                sh_type: sec.section_type,
                sh_flags: sec.flags,
                data: sec.data.clone(),
                sh_addralign: sec.alignment,
                relocs,
                comdat_group: sec.comdat_group.clone(),
            });
        }

        // Convert label positions from (section_index, offset) to (section_name, offset)
        // for the shared symbol table builder.
        let labels: HashMap<String, (String, u64)> = self.label_positions.iter()
            .map(|(name, &(sec_idx, offset))| {
                (name.clone(), (section_names[sec_idx].clone(), offset))
            })
            .collect();

        // Convert pending_globals/weaks from Vec<String> to HashMap<String, bool>
        let global_symbols: HashMap<String, bool> = self.pending_globals.iter()
            .map(|s| (s.clone(), true))
            .collect();
        let weak_symbols: HashMap<String, bool> = self.pending_weaks.iter()
            .map(|s| (s.clone(), true))
            .collect();

        // Convert pending_types from HashMap<String, SymbolKind> to HashMap<String, u8>
        let symbol_types: HashMap<String, u8> = self.pending_types.iter()
            .map(|(name, kind)| {
                let stt = match kind {
                    SymbolKind::Function => STT_FUNC,
                    SymbolKind::Object => STT_OBJECT,
                    SymbolKind::TlsObject => STT_TLS,
                    SymbolKind::NoType => STT_NOTYPE,
                };
                (name.clone(), stt)
            })
            .collect();

        // Resolve pending_sizes to concrete u64 values (after relaxation)
        let symbol_sizes: HashMap<String, u64> = self.pending_sizes.iter()
            .map(|(name, expr)| {
                let size = match expr {
                    SizeExpr::Constant(v) => *v,
                    SizeExpr::CurrentMinusSymbol(start_sym) => {
                        if let Some(&(sec_idx, start_off)) = self.label_positions.get(start_sym) {
                            let end = self.sections[sec_idx].data.len() as u64;
                            end - start_off
                        } else {
                            0
                        }
                    }
                    SizeExpr::SymbolDiff(end_label, start_label) => {
                        let end_off = self.label_positions.get(end_label).map(|p| p.1).unwrap_or(0);
                        let start_off = self.label_positions.get(start_label).map(|p| p.1).unwrap_or(0);
                        end_off.wrapping_sub(start_off)
                    }
                };
                (name.clone(), size)
            })
            .collect();

        // Convert pending_hidden/protected/internal from Vec<String> to HashMap<String, u8>
        let mut symbol_visibility: HashMap<String, u8> = HashMap::new();
        for name in &self.pending_hidden {
            symbol_visibility.insert(name.clone(), STV_HIDDEN);
        }
        for name in &self.pending_protected {
            symbol_visibility.insert(name.clone(), STV_PROTECTED);
        }
        for name in &self.pending_internal {
            symbol_visibility.insert(name.clone(), STV_INTERNAL);
        }

        // Use the shared symbol table builder (same as ARM and RISC-V assemblers).
        // This handles defined labels, .set/.equ aliases, undefined symbols from
        // relocations, and proper binding/type/visibility for all of them.
        let symtab_input = SymbolTableInput {
            labels: &labels,
            global_symbols: &global_symbols,
            weak_symbols: &weak_symbols,
            symbol_types: &symbol_types,
            symbol_sizes: &symbol_sizes,
            symbol_visibility: &symbol_visibility,
            aliases: &self.aliases,
            sections: &shared_sections,
            include_referenced_locals: false,
        };

        let mut shared_symbols = elf_mod::build_elf_symbol_table(&symtab_input);

        // Add COMMON symbols separately (they don't have labels/label_positions)
        for sym in &self.symbols {
            if sym.is_common {
                shared_symbols.push(ObjSymbol {
                    name: sym.name.clone(),
                    value: sym.common_align as u64,
                    size: sym.size,
                    binding: sym.binding,
                    sym_type: sym.sym_type,
                    visibility: sym.visibility,
                    section_name: "*COM*".to_string(),
                });
            }
        }

        let config = ElfConfig {
            e_machine: EM_X86_64,
            e_flags: 0,
            elf_class: ELFCLASS64,
        };

        elf_mod::write_relocatable_object(
            &config,
            &section_names,
            &shared_sections,
            &shared_symbols,
        )
    }

    /// Relax long jumps to short jumps when the displacement fits in a signed byte.
    ///
    /// Long JMP (E9 rel32, 5 bytes) -> Short JMP (EB rel8, 2 bytes): saves 3 bytes
    /// Long Jcc (0F 8x rel32, 6 bytes) -> Short Jcc (7x rel8, 2 bytes): saves 4 bytes
    ///
    /// Uses an iterative approach: shrinking one jump may bring other jumps within
    /// short range, so we repeat until no more relaxation is possible.
    fn relax_jumps(&mut self) {
        for sec_idx in 0..self.sections.len() {
            if self.sections[sec_idx].jumps.is_empty() {
                continue;
            }

            // Iterate until convergence
            loop {
                let mut any_relaxed = false;

                // Collect label positions within this section
                let mut local_labels: HashMap<String, usize> = HashMap::new();
                for (name, &(s_idx, offset)) in &self.label_positions {
                    if s_idx == sec_idx {
                        local_labels.insert(name.clone(), offset as usize);
                    }
                }

                // Find jumps to relax
                let mut to_relax: Vec<usize> = Vec::new();
                for (j_idx, jump) in self.sections[sec_idx].jumps.iter().enumerate() {
                    if jump.relaxed {
                        continue;
                    }
                    let target_off_opt = local_labels.get(&jump.target).copied()
                        .or_else(|| {
                            // Try numeric label resolution
                            self.resolve_numeric_label(&jump.target, jump.offset as u64, sec_idx)
                                .map(|(_, off)| off as usize)
                        });
                    if let Some(target_off) = target_off_opt {
                        // Compute displacement from end of the SHORT instruction
                        // Short form is always 2 bytes, so end = jump.offset + 2
                        let short_end = jump.offset as i64 + 2;
                        let disp = target_off as i64 - short_end;
                        if (-128..=127).contains(&disp) {
                            to_relax.push(j_idx);
                        }
                    }
                }

                if to_relax.is_empty() {
                    break;
                }

                // Process relaxations from back to front so offsets stay valid
                to_relax.sort_unstable();
                to_relax.reverse();

                for &j_idx in &to_relax {
                    let jump = &self.sections[sec_idx].jumps[j_idx];
                    let offset = jump.offset;
                    let old_len = jump.len;
                    let is_conditional = jump.is_conditional;
                    let new_len = 2usize; // short form is always 2 bytes
                    let shrink = old_len - new_len;

                    // Rewrite the instruction bytes in place:
                    // For Jcc: replace [0F, 8x, d32...] with [7x, d8]
                    // For JMP: replace [E9, d32...] with [EB, d8]
                    let data = &mut self.sections[sec_idx].data;
                    if is_conditional {
                        // Extract condition code from 0F 8x encoding
                        let cc = data[offset + 1] - 0x80;
                        data[offset] = 0x70 + cc;
                        data[offset + 1] = 0; // placeholder displacement, will be resolved later
                    } else {
                        data[offset] = 0xEB;
                        data[offset + 1] = 0; // placeholder displacement
                    }

                    // Remove the extra bytes
                    let remove_start = offset + new_len;
                    let remove_end = offset + old_len;
                    data.drain(remove_start..remove_end);

                    // Update all label positions after this point
                    for (_, pos) in self.label_positions.iter_mut() {
                        if pos.0 == sec_idx && (pos.1 as usize) > offset {
                            pos.1 -= shrink as u64;
                        }
                    }
                    // Also update numeric label positions
                    for (_, positions) in self.numeric_label_positions.iter_mut() {
                        for pos in positions.iter_mut() {
                            if pos.0 == sec_idx && (pos.1 as usize) > offset {
                                pos.1 -= shrink as u64;
                            }
                        }
                    }

                    // Update all relocation offsets after this point
                    // Also remove the relocation for this jump (it becomes inline displacement)
                    self.sections[sec_idx].relocations.retain_mut(|reloc| {
                        let reloc_off = reloc.offset as usize;
                        // The old relocation for this jump was at offset+1 (JMP) or offset+2 (Jcc)
                        let old_reloc_pos = if is_conditional { offset + 2 } else { offset + 1 };
                        if reloc_off == old_reloc_pos {
                            // Remove this relocation - displacement is resolved inline
                            return false;
                        }
                        if reloc_off > offset {
                            reloc.offset -= shrink as u64;
                        }
                        true
                    });

                    // Update all other jump offsets after this point
                    for other_jump in self.sections[sec_idx].jumps.iter_mut() {
                        if other_jump.offset > offset {
                            other_jump.offset -= shrink;
                        }
                    }

                    // Update org region data offsets
                    for org in self.sections[sec_idx].org_regions.iter_mut() {
                        if org.data_offset > offset {
                            org.data_offset -= shrink;
                        }
                    }

                    // Mark this jump as relaxed
                    self.sections[sec_idx].jumps[j_idx].relaxed = true;
                    self.sections[sec_idx].jumps[j_idx].len = new_len;
                    any_relaxed = true;
                }

                if !any_relaxed {
                    break;
                }
            }

            // Fix up .org padding regions after jump relaxation.
            // When jumps before an .org region are shortened, the code before
            // the padding shrinks, but the padding stays the same size. We need
            // to expand the padding so that code after it remains at the correct
            // absolute position (relative to the base label).
            self.fixup_org_regions(sec_idx);

            // Now resolve the short jump displacements inline
            let mut local_labels: HashMap<String, usize> = HashMap::new();
            for (name, &(s_idx, offset)) in &self.label_positions {
                if s_idx == sec_idx {
                    local_labels.insert(name.clone(), offset as usize);
                }
            }

            // Collect patches to apply
            let patches: Vec<(usize, u8)> = self.sections[sec_idx].jumps.iter()
                .filter(|j| j.relaxed)
                .filter_map(|jump| {
                    let target = local_labels.get(&jump.target).copied()
                        .or_else(|| {
                            self.resolve_numeric_label(&jump.target, jump.offset as u64, sec_idx)
                                .map(|(_, off)| off as usize)
                        });
                    target.map(|target_off| {
                        let end_of_instr = jump.offset + 2;
                        let disp = (target_off as i64 - end_of_instr as i64) as i8;
                        (jump.offset + 1, disp as u8)
                    })
                })
                .collect();

            for (off, byte) in patches {
                self.sections[sec_idx].data[off] = byte;
            }
        }
    }

    fn fixup_org_regions(&mut self, sec_idx: usize) {
        let org_count = self.sections[sec_idx].org_regions.len();
        if org_count == 0 {
            return;
        }

        // Sort org regions by data_offset (ascending)
        self.sections[sec_idx].org_regions.sort_by_key(|o| o.data_offset);

        // Process from first to last. When we insert/remove bytes for an
        // org region, it shifts all subsequent regions' positions. We update
        // their data_offsets accordingly so the next iteration uses correct values.
        for org_idx in 0..org_count {
            let org = &self.sections[sec_idx].org_regions[org_idx];
            let base_label = org.base_label.clone();
            let base_offset = org.base_offset;
            let data_offset = org.data_offset;

            // Look up the (post-relaxation) position of the base label
            let label_pos = if let Some(&(lbl_sec, lbl_off)) = self.label_positions.get(base_label.as_str()) {
                if lbl_sec != sec_idx {
                    continue;
                }
                lbl_off as usize
            } else {
                continue;
            };

            let target = (label_pos as i64 + base_offset) as usize;

            if data_offset == target {
                // Already at the right position, no adjustment needed
                continue;
            }

            let is_exec = self.sections[sec_idx].flags & SHF_EXECINSTR != 0;
            let fill_byte: u8 = if is_exec { 0x90 } else { 0x00 };

            if data_offset > target {
                // Current position is past the target. We need to remove
                // NOP padding bytes before this point to move it back.
                let excess = data_offset - target;
                let data = &self.sections[sec_idx].data;

                // Count how many consecutive fill bytes precede data_offset
                let mut nop_count = 0;
                while nop_count < data_offset && data[data_offset - 1 - nop_count] == fill_byte {
                    nop_count += 1;
                }

                if nop_count >= excess {
                    // Remove `excess` NOP bytes from right before data_offset
                    let remove_start = data_offset - excess;
                    self.sections[sec_idx].data.drain(remove_start..data_offset);

                    // Adjust all offsets after the removed region
                    Self::adjust_offsets_after_remove(
                        &mut self.label_positions,
                        &mut self.numeric_label_positions,
                        &mut self.sections[sec_idx],
                        sec_idx, remove_start, excess,
                    );
                    // Update later org regions
                    for later_idx in (org_idx + 1)..org_count {
                        if self.sections[sec_idx].org_regions[later_idx].data_offset > remove_start {
                            self.sections[sec_idx].org_regions[later_idx].data_offset -= excess;
                        }
                    }
                    self.sections[sec_idx].org_regions[org_idx].data_offset = target;
                }
            } else {
                // Current position is before the target. Insert NOP padding.
                let deficit = target - data_offset;
                let insert_pos = data_offset;
                self.sections[sec_idx].data.splice(
                    insert_pos..insert_pos,
                    std::iter::repeat_n(fill_byte, deficit),
                );

                // Adjust all offsets at and after the insertion point
                Self::adjust_offsets_after_insert(
                    &mut self.label_positions,
                    &mut self.numeric_label_positions,
                    &mut self.sections[sec_idx],
                    sec_idx, insert_pos, deficit,
                );
                // Update later org regions
                for later_idx in (org_idx + 1)..org_count {
                    if self.sections[sec_idx].org_regions[later_idx].data_offset >= insert_pos {
                        self.sections[sec_idx].org_regions[later_idx].data_offset += deficit;
                    }
                }
                self.sections[sec_idx].org_regions[org_idx].data_offset = target;
            }
        }
    }

    /// Adjust all tracked offsets after inserting `count` bytes at `pos`.
    /// Items at `pos` and after are shifted forward.
    fn adjust_offsets_after_insert(
        label_positions: &mut HashMap<String, (usize, u64)>,
        numeric_label_positions: &mut HashMap<String, Vec<(usize, u64)>>,
        section: &mut Section,
        sec_idx: usize,
        pos: usize,
        count: usize,
    ) {
        for (_, lbl_pos) in label_positions.iter_mut() {
            if lbl_pos.0 == sec_idx && (lbl_pos.1 as usize) >= pos {
                lbl_pos.1 += count as u64;
            }
        }
        for (_, positions) in numeric_label_positions.iter_mut() {
            for lbl_pos in positions.iter_mut() {
                if lbl_pos.0 == sec_idx && (lbl_pos.1 as usize) >= pos {
                    lbl_pos.1 += count as u64;
                }
            }
        }
        for reloc in section.relocations.iter_mut() {
            if (reloc.offset as usize) >= pos {
                reloc.offset += count as u64;
            }
        }
        for jump in section.jumps.iter_mut() {
            if jump.offset >= pos {
                jump.offset += count;
            }
        }
    }

    /// Adjust all tracked offsets after removing `count` bytes starting at `pos`.
    /// Items after `pos + count` are shifted backward by `count`.
    fn adjust_offsets_after_remove(
        label_positions: &mut HashMap<String, (usize, u64)>,
        numeric_label_positions: &mut HashMap<String, Vec<(usize, u64)>>,
        section: &mut Section,
        sec_idx: usize,
        pos: usize,
        count: usize,
    ) {
        for (_, lbl_pos) in label_positions.iter_mut() {
            if lbl_pos.0 == sec_idx && (lbl_pos.1 as usize) > pos {
                lbl_pos.1 -= count as u64;
            }
        }
        for (_, positions) in numeric_label_positions.iter_mut() {
            for lbl_pos in positions.iter_mut() {
                if lbl_pos.0 == sec_idx && (lbl_pos.1 as usize) > pos {
                    lbl_pos.1 -= count as u64;
                }
            }
        }
        for reloc in section.relocations.iter_mut() {
            if (reloc.offset as usize) > pos {
                reloc.offset -= count as u64;
            }
        }
        for jump in section.jumps.iter_mut() {
            if jump.offset > pos {
                jump.offset -= count;
            }
        }
    }

    /// Check whether a symbol is local (not global/weak).
    /// Only local symbols can have their relocations resolved inline;
    /// global/weak symbols must keep relocations for the linker so that
    /// symbol interposition and PLT redirection work correctly.
    fn is_local_symbol(&self, name: &str) -> bool {
        // Internal assembler labels (.L*) are always local
        if name.starts_with('.') {
            return true;
        }
        // Numeric labels (e.g., "1f", "1b", "2f", "2b") are always local
        if name.len() >= 2 {
            let last = name.as_bytes()[name.len() - 1];
            if (last == b'f' || last == b'b') && name[..name.len()-1].chars().all(|c| c.is_ascii_digit()) {
                return true;
            }
        }
        // Check the symbol table for binding
        if let Some(&sym_idx) = self.symbol_map.get(name) {
            self.symbols[sym_idx].binding == STB_LOCAL
        } else {
            // Unknown symbol - treat as external (non-local)
            false
        }
    }

    /// Resolve a numeric label reference like "1b" or "1f".
    /// Returns the (section_index, offset) of the target label.
    fn resolve_numeric_label(&self, symbol: &str, reloc_offset: u64, sec_idx: usize) -> Option<(usize, u64)> {
        let len = symbol.len();
        if len < 2 {
            return None;
        }
        let suffix = symbol.as_bytes()[len - 1];
        if suffix != b'b' && suffix != b'f' {
            return None;
        }
        let label_num = &symbol[..len - 1];
        if !label_num.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }

        let positions = self.numeric_label_positions.get(label_num)?;
        if suffix == b'b' {
            // Backward: find the nearest label BEFORE (or at) reloc_offset in the same section
            let mut best: Option<(usize, u64)> = None;
            for &(s_idx, off) in positions {
                if s_idx == sec_idx && off <= reloc_offset
                    && (best.is_none() || off > best.unwrap().1) {
                        best = Some((s_idx, off));
                    }
            }
            best
        } else {
            // Forward: find the nearest label AFTER reloc_offset in the same section
            let mut best: Option<(usize, u64)> = None;
            for &(s_idx, off) in positions {
                if s_idx == sec_idx && off > reloc_offset
                    && (best.is_none() || off < best.unwrap().1) {
                        best = Some((s_idx, off));
                    }
            }
            best
        }
    }

    /// Resolve relocations that reference internal labels (same-section).
    /// Only resolves relocations for local symbols; global/weak symbols
    /// keep their relocations so the linker can handle interposition/PLT.
    fn resolve_internal_relocations(&mut self) {
        for sec_idx in 0..self.sections.len() {
            let mut resolved: Vec<(usize, i32, usize)> = Vec::new(); // (offset, value, patch_size)
            let mut pc8_patches: Vec<(usize, u8)> = Vec::new();
            let mut unresolved = Vec::new();

            for reloc in &self.sections[sec_idx].relocations {
                // SymbolDiff relocations are cross-section; don't resolve inline
                if reloc.diff_symbol.is_some() {
                    // For SymbolDiff where both symbols end up in the same section,
                    // we CAN resolve to a constant.
                    if let Some(ref diff_sym) = reloc.diff_symbol {
                        if let (Some(&(a_sec, a_off)), Some(&(b_sec, b_off))) = (
                            self.label_positions.get(&reloc.symbol),
                            self.label_positions.get(diff_sym),
                        ) {
                            if a_sec == b_sec {
                                // Both in same section: constant = a - b
                                let val = a_off as i64 - b_off as i64;
                                resolved.push((reloc.offset as usize, val as i32, reloc.patch_size as usize));
                                continue;
                            }
                        }
                    }
                    unresolved.push(reloc.clone());
                    continue;
                }

                // Try regular label lookup first, then numeric label resolution (1b, 1f, etc.)
                let label_pos = self.label_positions.get(&reloc.symbol).copied()
                    .or_else(|| self.resolve_numeric_label(&reloc.symbol, reloc.offset, sec_idx));
                if let Some((target_sec, target_off)) = label_pos {
                    let is_local = self.is_local_symbol(&reloc.symbol);

                    // Handle internal PC8 relocations (loop/jrcxz)
                    if reloc.reloc_type == R_X86_64_PC8_INTERNAL && target_sec == sec_idx {
                        let rel = (target_off as i64) + reloc.addend - (reloc.offset as i64);
                        if (-128..=127).contains(&rel) {
                            pc8_patches.push((reloc.offset as usize, rel as u8));
                        }
                        // Either way, don't keep as unresolved
                        continue;
                    }

                    if target_sec == sec_idx && is_local
                        && (reloc.reloc_type == R_X86_64_PC32 || reloc.reloc_type == R_X86_64_PLT32)
                    {
                        // Same-section PC-relative to a local symbol: resolve now.
                        let rel = (target_off as i64) + reloc.addend - (reloc.offset as i64);
                        resolved.push((reloc.offset as usize, rel as i32, reloc.patch_size as usize));
                    } else if is_local && reloc.reloc_type == R_X86_64_32 {
                        // Absolute reference to a local symbol
                        let val = (target_off as i64) + reloc.addend;
                        resolved.push((reloc.offset as usize, val as i32, reloc.patch_size as usize));
                    } else {
                        unresolved.push(reloc.clone());
                    }
                } else {
                    unresolved.push(reloc.clone());
                }
            }

            // Patch resolved relocations into section data
            for (offset, value, psz) in resolved {
                if psz == 1 {
                    self.sections[sec_idx].data[offset] = value as u8;
                } else if psz == 2 {
                    let bytes = (value as i16).to_le_bytes();
                    self.sections[sec_idx].data[offset..offset + 2].copy_from_slice(&bytes);
                } else {
                    let bytes = value.to_le_bytes();
                    self.sections[sec_idx].data[offset..offset + 4].copy_from_slice(&bytes);
                }
            }
            // Patch PC8 relocations (single-byte patches for loop/jrcxz)
            for (offset, value) in pc8_patches {
                self.sections[sec_idx].data[offset] = value;
            }

            self.sections[sec_idx].relocations = unresolved;
        }
    }
}


// parse_section_flags is now in crate::backend::elf and imported above.
