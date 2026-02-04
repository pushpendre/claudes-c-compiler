//! 32-bit ELF relocatable object file writer for i686.
//!
//! Produces .o files from parsed and encoded assembly. Uses ELFCLASS32,
//! EM_386, and Elf32 structures. Uses REL (not RELA) relocation format.

use std::collections::HashMap;
use crate::backend::x86::assembler::parser::*;
use super::encoder::*;
use crate::backend::elf::{
    SHT_PROGBITS, SHT_NOBITS,
    SHT_INIT_ARRAY, SHT_FINI_ARRAY, SHT_NOTE,
    STB_LOCAL, STB_GLOBAL, STB_WEAK,
    STT_NOTYPE, STT_OBJECT, STT_FUNC, STT_TLS,
    STV_DEFAULT, STV_INTERNAL, STV_HIDDEN, STV_PROTECTED,
};

// i686 uses u32 section flags (ELF32)
const SHF_WRITE: u32 = 0x1;
const SHF_ALLOC: u32 = 0x2;
const SHF_EXECINSTR: u32 = 0x4;
const SHF_MERGE: u32 = 0x10;
const SHF_STRINGS: u32 = 0x20;
const SHF_TLS: u32 = 0x400;
const SHF_GROUP: u32 = 0x200;

/// Tracks a jump instruction for relaxation.
#[derive(Clone, Debug)]
struct JumpInfo {
    offset: usize,
    len: usize,
    target: String,
    is_conditional: bool,
    relaxed: bool,
}

/// Tracks a section being built.
struct Section {
    name: String,
    section_type: u32,
    flags: u32,
    data: Vec<u8>,
    alignment: u32,
    relocations: Vec<ElfRelocation>,
    jumps: Vec<JumpInfo>,
}

#[derive(Clone)]
struct ElfRelocation {
    offset: u32,
    symbol: String,
    reloc_type: u32,
    addend: i32,
}

/// Symbol info collected during assembly.
struct SymbolInfo {
    name: String,
    binding: u8,
    sym_type: u8,
    visibility: u8,
    section: Option<String>,
    value: u32,
    size: u32,
    is_common: bool,
    common_align: u32,
}

/// Builds a 32-bit ELF relocatable object file from parsed assembly items.
pub struct ElfWriter {
    sections: Vec<Section>,
    symbols: Vec<SymbolInfo>,
    section_map: HashMap<String, usize>,
    symbol_map: HashMap<String, usize>,
    current_section: Option<usize>,
    label_positions: HashMap<String, (usize, u32)>,
    pending_globals: Vec<String>,
    pending_weaks: Vec<String>,
    pending_types: HashMap<String, SymbolKind>,
    pending_sizes: HashMap<String, SizeExpr>,
    pending_hidden: Vec<String>,
    pending_protected: Vec<String>,
    pending_internal: Vec<String>,
    aliases: HashMap<String, String>,
    /// Whether we're in .code16gcc mode
    #[allow(dead_code)]
    code16gcc: bool,
    /// Section stack for .pushsection/.popsection.
    section_stack: Vec<Option<usize>>,
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
            pending_globals: Vec::new(),
            pending_weaks: Vec::new(),
            pending_types: HashMap::new(),
            pending_sizes: HashMap::new(),
            pending_hidden: Vec::new(),
            pending_protected: Vec::new(),
            pending_internal: Vec::new(),
            aliases: HashMap::new(),
            section_stack: Vec::new(),
            code16gcc: false,
        }
    }

    /// Build the ELF object file from parsed assembly items.
    pub fn build(mut self, items: &[AsmItem]) -> Result<Vec<u8>, String> {
        for item in items {
            self.process_item(item)?;
        }
        self.emit_elf()
    }

    fn get_or_create_section(&mut self, name: &str, section_type: u32, flags: u32) -> usize {
        if let Some(&idx) = self.section_map.get(name) {
            return idx;
        }
        let idx = self.sections.len();
        self.sections.push(Section {
            name: name.to_string(),
            section_type,
            flags,
            data: Vec::new(),
            alignment: if flags & SHF_EXECINSTR != 0 { 16 } else { 1 },
            relocations: Vec::new(),
            jumps: Vec::new(),
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
        let idx = self.get_or_create_section(&dir.name, section_type, flags);
        self.current_section = Some(idx);
    }

    fn process_item(&mut self, item: &AsmItem) -> Result<(), String> {
        match item {
            AsmItem::Section(dir) => {
                self.switch_section(dir);
            }
            AsmItem::PushSection(dir) => {
                self.section_stack.push(self.current_section);
                self.switch_section(dir);
            }
            AsmItem::PopSection => {
                if let Some(prev) = self.section_stack.pop() {
                    self.current_section = prev;
                }
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
                            let current_off = self.sections[sec_idx].data.len() as u32;
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
                let offset = self.sections[sec_idx].data.len() as u32;
                self.label_positions.insert(name.clone(), (sec_idx, offset));
                self.ensure_symbol(name, sec_idx, offset);
            }
            AsmItem::Align(n) => {
                if let Some(sec_idx) = self.current_section {
                    let section = &mut self.sections[sec_idx];
                    let align = *n;
                    if align > section.alignment {
                        section.alignment = align;
                    }
                    let current = section.data.len() as u32;
                    let aligned = (current + align - 1) & !(align - 1);
                    let padding = (aligned - current) as usize;
                    if section.flags & SHF_EXECINSTR != 0 {
                        section.data.extend(std::iter::repeat_n(0x90, padding));
                    } else {
                        section.data.extend(std::iter::repeat_n(0, padding));
                    }
                }
            }
            AsmItem::Byte(vals) => {
                let section = self.current_section_mut()?;
                for val in vals {
                    match val {
                        DataValue::Integer(v) => section.data.push(*v as u8),
                        _ => return Err("unsupported label expression in .byte for i686".to_string()),
                    }
                }
            }
            AsmItem::Short(vals) => {
                let section = self.current_section_mut()?;
                for v in vals {
                    section.data.extend_from_slice(&v.to_le_bytes());
                }
            }
            AsmItem::Long(vals) => {
                self.emit_data_values(vals, 4)?;
            }
            AsmItem::Quad(vals) => {
                // 64-bit data values (used rarely on i686, but needed for debug info etc.)
                self.emit_data_values_64(vals)?;
            }
            AsmItem::Zero(n) => {
                let section = self.current_section_mut()?;
                section.data.extend(std::iter::repeat_n(0u8, *n as usize));
            }
            AsmItem::SkipExpr(expr, fill) => {
                // i686 doesn't need deferred skip expression support (kernel uses x86-64)
                // Try simple integer parse; error otherwise
                if let Ok(val) = expr.trim().parse::<u32>() {
                    let section = self.current_section_mut()?;
                    section.data.extend(std::iter::repeat(*fill).take(val as usize));
                } else {
                    return Err(format!("unsupported .skip expression in i686: {}", expr));
                }
            }
            AsmItem::Asciz(bytes) | AsmItem::Ascii(bytes) => {
                let section = self.current_section_mut()?;
                section.data.extend_from_slice(bytes);
            }
            AsmItem::Comm(name, size, align) => {
                let sym_idx = self.symbols.len();
                self.symbols.push(SymbolInfo {
                    name: name.clone(),
                    binding: STB_GLOBAL,
                    sym_type: STT_OBJECT,
                    visibility: STV_DEFAULT,
                    section: None,
                    value: *align,
                    size: *size as u32,
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
            AsmItem::Cfi(_) | AsmItem::File(_, _) | AsmItem::Loc(_, _, _)
            | AsmItem::OptionDirective(_) | AsmItem::Empty => {}
        }
        Ok(())
    }

    fn ensure_section(&mut self) -> Result<(), String> {
        if self.current_section.is_none() {
            let idx = self.get_or_create_section(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR);
            self.current_section = Some(idx);
        }
        Ok(())
    }

    fn ensure_symbol(&mut self, name: &str, sec_idx: usize, offset: u32) {
        let sec_name = self.sections[sec_idx].name.clone();

        if let Some(&sym_idx) = self.symbol_map.get(name) {
            let sym = &mut self.symbols[sym_idx];
            sym.section = Some(sec_name);
            sym.value = offset;
        } else {
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
                    section.data.extend_from_slice(&(*v as i32).to_le_bytes());
                }
                DataValue::Symbol(sym) => {
                    let offset = self.sections[sec_idx].data.len() as u32;
                    self.sections[sec_idx].relocations.push(ElfRelocation {
                        offset,
                        symbol: sym.clone(),
                        reloc_type: R_386_32,
                        addend: 0,
                    });
                    let section = &mut self.sections[sec_idx];
                    section.data.extend(std::iter::repeat_n(0, size));
                }
                DataValue::SymbolOffset(sym, addend) => {
                    let offset = self.sections[sec_idx].data.len() as u32;
                    self.sections[sec_idx].relocations.push(ElfRelocation {
                        offset,
                        symbol: sym.clone(),
                        reloc_type: R_386_32,
                        addend: *addend as i32,
                    });
                    let section = &mut self.sections[sec_idx];
                    section.data.extend(std::iter::repeat_n(0, size));
                }
                DataValue::SymbolDiff(a, _b) => {
                    let offset = self.sections[sec_idx].data.len() as u32;
                    self.sections[sec_idx].relocations.push(ElfRelocation {
                        offset,
                        symbol: a.clone(),
                        reloc_type: R_386_PC32,
                        addend: 0,
                    });
                    let section = &mut self.sections[sec_idx];
                    section.data.extend(std::iter::repeat_n(0, size));
                }
            }
        }
        Ok(())
    }

    fn emit_data_values_64(&mut self, vals: &[DataValue]) -> Result<(), String> {
        let sec_idx = self.current_section.ok_or("no active section")?;

        for val in vals {
            match val {
                DataValue::Integer(v) => {
                    let section = &mut self.sections[sec_idx];
                    section.data.extend_from_slice(&v.to_le_bytes());
                }
                DataValue::Symbol(sym) => {
                    // For 64-bit data on 32-bit, emit two relocations or just the low 32 bits
                    let offset = self.sections[sec_idx].data.len() as u32;
                    self.sections[sec_idx].relocations.push(ElfRelocation {
                        offset,
                        symbol: sym.clone(),
                        reloc_type: R_386_32,
                        addend: 0,
                    });
                    let section = &mut self.sections[sec_idx];
                    section.data.extend(std::iter::repeat_n(0, 8));
                }
                DataValue::SymbolOffset(sym, addend) => {
                    let offset = self.sections[sec_idx].data.len() as u32;
                    self.sections[sec_idx].relocations.push(ElfRelocation {
                        offset,
                        symbol: sym.clone(),
                        reloc_type: R_386_32,
                        addend: *addend as i32,
                    });
                    let section = &mut self.sections[sec_idx];
                    section.data.extend(std::iter::repeat_n(0, 8));
                }
                DataValue::SymbolDiff(a, _b) => {
                    let offset = self.sections[sec_idx].data.len() as u32;
                    self.sections[sec_idx].relocations.push(ElfRelocation {
                        offset,
                        symbol: a.clone(),
                        reloc_type: R_386_PC32,
                        addend: 0,
                    });
                    let section = &mut self.sections[sec_idx];
                    section.data.extend(std::iter::repeat_n(0, 8));
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

        let base_offset = self.sections[sec_idx].data.len() as u32;
        let instr_len = encoder.bytes.len();
        self.sections[sec_idx].data.extend_from_slice(&encoder.bytes);

        // Detect jump instructions for relaxation
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

        // Copy relocations
        for reloc in encoder.relocations {
            self.sections[sec_idx].relocations.push(ElfRelocation {
                offset: base_offset + reloc.offset as u32,
                symbol: reloc.symbol,
                reloc_type: reloc.reloc_type,
                addend: reloc.addend as i32,
            });
        }

        Ok(())
    }

    fn get_jump_target_label(&self, instr: &Instruction) -> Option<String> {
        let mnem = &instr.mnemonic;
        let is_jump = mnem == "jmp" || (mnem.starts_with('j') && mnem.len() >= 2);
        if !is_jump { return None; }
        if instr.operands.len() != 1 { return None; }
        if let Operand::Label(label) = &instr.operands[0] {
            Some(label.clone())
        } else {
            None
        }
    }

    fn emit_elf(mut self) -> Result<Vec<u8>, String> {
        // Relax jumps first, before resolving sizes and updating symbol values
        self.relax_jumps();

        // After relaxation, update all symbol values from the (now-correct) label_positions.
        for (name, &(_sec_idx, offset)) in &self.label_positions {
            if let Some(&sym_idx) = self.symbol_map.get(name) {
                self.symbols[sym_idx].value = offset;
            }
        }

        // Resolve sizes (after relaxation so sizes are correct)
        for (name, expr) in &self.pending_sizes {
            if let Some(&sym_idx) = self.symbol_map.get(name) {
                let size = match expr {
                    SizeExpr::Constant(v) => *v as u32,
                    SizeExpr::CurrentMinusSymbol(start_sym) => {
                        if let Some(&(sec_idx, start_off)) = self.label_positions.get(start_sym) {
                            let end = self.sections[sec_idx].data.len() as u32;
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
                self.symbols[sym_idx].size = size;
            }
        }

        // Resolve internal relocations
        self.resolve_internal_relocations();

        // Resolve .set aliases into proper symbols with correct binding/visibility.
        for (alias, target) in &self.aliases {
            let (section, value, size, target_sym_type) = if let Some(&sym_idx) = self.symbol_map.get(target.as_str()) {
                let sym = &self.symbols[sym_idx];
                (sym.section.clone(), sym.value, sym.size, sym.sym_type)
            } else if let Some(&(sec_idx, offset)) = self.label_positions.get(target.as_str()) {
                (Some(self.sections[sec_idx].name.clone()), offset, 0u32, STT_NOTYPE)
            } else {
                continue;
            };

            let binding = if self.pending_globals.contains(alias) {
                STB_GLOBAL
            } else if self.pending_weaks.contains(alias) {
                STB_WEAK
            } else {
                STB_LOCAL
            };

            let sym_type = match self.pending_types.get(alias.as_str()) {
                Some(SymbolKind::Function) => STT_FUNC,
                Some(SymbolKind::Object) => STT_OBJECT,
                Some(SymbolKind::TlsObject) => STT_TLS,
                Some(SymbolKind::NoType) => STT_NOTYPE,
                None => target_sym_type,
            };

            let visibility = if self.pending_hidden.contains(alias) {
                STV_HIDDEN
            } else if self.pending_protected.contains(alias) {
                STV_PROTECTED
            } else if self.pending_internal.contains(alias) {
                STV_INTERNAL
            } else {
                STV_DEFAULT
            };

            let sym_idx = self.symbols.len();
            self.symbols.push(SymbolInfo {
                name: alias.clone(),
                binding,
                sym_type,
                visibility,
                section,
                value,
                size,
                is_common: false,
                common_align: 0,
            });
            self.symbol_map.insert(alias.clone(), sym_idx);
        }

        // Convert to shared format and delegate to shared writer.
        // For i686 REL format, addends must be patched into section data
        // since REL entries don't carry an explicit addend field.
        use crate::backend::elf::{self as elf_mod, ElfConfig, ObjSection, ObjSymbol, ObjReloc, ELFCLASS32, EM_386};

        let section_names: Vec<String> = self.sections.iter().map(|s| s.name.clone()).collect();

        let mut shared_sections: std::collections::HashMap<String, ObjSection> = std::collections::HashMap::new();
        for sec in &self.sections {
            // Clone section data so we can patch implicit addends into it
            let mut data = sec.data.clone();

            let mut relocs = Vec::new();
            for reloc in &sec.relocations {
                let (sym_name, addend) = if reloc.symbol.starts_with('.') {
                    // Internal label: convert to section symbol + offset
                    if let Some(&(target_sec, target_off)) = self.label_positions.get(&reloc.symbol) {
                        (section_names[target_sec].clone(), reloc.addend + target_off as i32)
                    } else {
                        (reloc.symbol.clone(), reloc.addend)
                    }
                } else {
                    (reloc.symbol.clone(), reloc.addend)
                };

                // For REL format: patch the addend into the section data
                let off = reloc.offset as usize;
                if off + 4 <= data.len() {
                    let existing = i32::from_le_bytes([data[off], data[off+1], data[off+2], data[off+3]]);
                    let patched = existing.wrapping_add(addend);
                    data[off..off+4].copy_from_slice(&patched.to_le_bytes());
                }

                relocs.push(ObjReloc {
                    offset: reloc.offset as u64,
                    reloc_type: reloc.reloc_type,
                    symbol_name: sym_name,
                    addend: 0, // REL format: addend is in the data, not the reloc entry
                });
            }
            shared_sections.insert(sec.name.clone(), ObjSection {
                name: sec.name.clone(),
                sh_type: sec.section_type,
                sh_flags: sec.flags as u64,
                data,
                sh_addralign: sec.alignment as u64,
                relocs,
            });
        }

        // Convert symbols, filtering out .L* labels
        let shared_symbols: Vec<ObjSymbol> = self.symbols.iter()
            .filter(|sym| !sym.name.is_empty() && !sym.name.starts_with('.'))
            .map(|sym| ObjSymbol {
                name: sym.name.clone(),
                value: if sym.is_common { sym.common_align as u64 } else { sym.value as u64 },
                size: sym.size as u64,
                binding: sym.binding,
                sym_type: sym.sym_type,
                visibility: sym.visibility,
                section_name: if sym.is_common {
                    "*COM*".to_string()
                } else {
                    sym.section.clone().unwrap_or_default()
                },
            }).collect();

        let config = ElfConfig {
            e_machine: EM_386,
            e_flags: 0,
            elf_class: ELFCLASS32,
        };

        elf_mod::write_relocatable_object(
            &config,
            &section_names,
            &shared_sections,
            &shared_symbols,
        )
    }

    fn relax_jumps(&mut self) {
        for sec_idx in 0..self.sections.len() {
            if self.sections[sec_idx].jumps.is_empty() {
                continue;
            }

            loop {
                let mut any_relaxed = false;
                let mut local_labels: HashMap<String, usize> = HashMap::new();
                for (name, &(s_idx, offset)) in &self.label_positions {
                    if s_idx == sec_idx {
                        local_labels.insert(name.clone(), offset as usize);
                    }
                }

                let mut to_relax: Vec<usize> = Vec::new();
                for (j_idx, jump) in self.sections[sec_idx].jumps.iter().enumerate() {
                    if jump.relaxed { continue; }
                    if let Some(&target_off) = local_labels.get(&jump.target) {
                        let short_end = jump.offset as i64 + 2;
                        let disp = target_off as i64 - short_end;
                        if (-128..=127).contains(&disp) {
                            to_relax.push(j_idx);
                        }
                    }
                }

                if to_relax.is_empty() { break; }

                to_relax.sort_unstable();
                to_relax.reverse();

                for &j_idx in &to_relax {
                    let jump = &self.sections[sec_idx].jumps[j_idx];
                    let offset = jump.offset;
                    let old_len = jump.len;
                    let is_conditional = jump.is_conditional;
                    let new_len = 2usize;
                    let shrink = old_len - new_len;

                    let data = &mut self.sections[sec_idx].data;
                    if is_conditional {
                        let cc = data[offset + 1] - 0x80;
                        data[offset] = 0x70 + cc;
                        data[offset + 1] = 0;
                    } else {
                        data[offset] = 0xEB;
                        data[offset + 1] = 0;
                    }

                    let remove_start = offset + new_len;
                    let remove_end = offset + old_len;
                    data.drain(remove_start..remove_end);

                    for (_, pos) in self.label_positions.iter_mut() {
                        if pos.0 == sec_idx && (pos.1 as usize) > offset {
                            pos.1 -= shrink as u32;
                        }
                    }

                    self.sections[sec_idx].relocations.retain_mut(|reloc| {
                        let reloc_off = reloc.offset as usize;
                        let old_reloc_pos = if is_conditional { offset + 2 } else { offset + 1 };
                        if reloc_off == old_reloc_pos {
                            return false;
                        }
                        if reloc_off > offset {
                            reloc.offset -= shrink as u32;
                        }
                        true
                    });

                    for other_jump in self.sections[sec_idx].jumps.iter_mut() {
                        if other_jump.offset > offset {
                            other_jump.offset -= shrink;
                        }
                    }

                    self.sections[sec_idx].jumps[j_idx].relaxed = true;
                    self.sections[sec_idx].jumps[j_idx].len = new_len;
                    any_relaxed = true;
                }

                if !any_relaxed { break; }
            }

            // Resolve short jump displacements
            let mut local_labels: HashMap<String, usize> = HashMap::new();
            for (name, &(s_idx, offset)) in &self.label_positions {
                if s_idx == sec_idx {
                    local_labels.insert(name.clone(), offset as usize);
                }
            }

            let patches: Vec<(usize, u8)> = self.sections[sec_idx].jumps.iter()
                .filter(|j| j.relaxed)
                .filter_map(|jump| {
                    local_labels.get(&jump.target).map(|&target_off| {
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

    /// Check whether a symbol is local (not global/weak).
    /// Only local symbols can have their relocations resolved inline;
    /// global/weak symbols must keep relocations for the linker so that
    /// symbol interposition and PLT redirection work correctly.
    fn is_local_symbol(&self, name: &str) -> bool {
        // Internal assembler labels (.L*) are always local
        if name.starts_with('.') {
            return true;
        }
        // Check the symbol table for binding
        if let Some(&sym_idx) = self.symbol_map.get(name) {
            self.symbols[sym_idx].binding == STB_LOCAL
        } else {
            // Unknown symbol - treat as external (non-local)
            false
        }
    }

    /// Resolve relocations that reference internal labels (same-section).
    /// Only resolves relocations for local symbols; global/weak symbols
    /// keep their relocations so the linker can handle interposition/PLT.
    fn resolve_internal_relocations(&mut self) {
        for sec_idx in 0..self.sections.len() {
            let mut resolved = Vec::new();
            let mut unresolved = Vec::new();

            for reloc in &self.sections[sec_idx].relocations {
                if let Some(&(target_sec, target_off)) = self.label_positions.get(&reloc.symbol) {
                    let is_local = self.is_local_symbol(&reloc.symbol);
                    if target_sec == sec_idx && is_local
                        && (reloc.reloc_type == R_386_PC32 || reloc.reloc_type == R_386_PLT32)
                    {
                        // Same-section PC-relative to a local symbol: resolve now.
                        let rel = (target_off as i64) + reloc.addend as i64 - (reloc.offset as i64);
                        resolved.push((reloc.offset as usize, rel as i32));
                    } else {
                        unresolved.push(reloc.clone());
                    }
                } else {
                    unresolved.push(reloc.clone());
                }
            }

            for (offset, value) in resolved {
                let bytes = value.to_le_bytes();
                self.sections[sec_idx].data[offset..offset + 4].copy_from_slice(&bytes);
            }

            self.sections[sec_idx].relocations = unresolved;
        }
    }
}


/// Parse section name, flags, and type into ELF section type and flags (32-bit).
fn parse_section_flags(name: &str, flags_str: Option<&str>, type_str: Option<&str>) -> (u32, u32) {
    let (default_type, default_flags) = match name {
        ".text" => (SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR),
        ".data" => (SHT_PROGBITS, SHF_ALLOC | SHF_WRITE),
        ".bss" => (SHT_NOBITS, SHF_ALLOC | SHF_WRITE),
        ".rodata" => (SHT_PROGBITS, SHF_ALLOC),
        ".tdata" => (SHT_PROGBITS, SHF_ALLOC | SHF_WRITE | SHF_TLS),
        ".tbss" => (SHT_NOBITS, SHF_ALLOC | SHF_WRITE | SHF_TLS),
        ".init_array" => (SHT_INIT_ARRAY, SHF_ALLOC | SHF_WRITE),
        ".fini_array" => (SHT_FINI_ARRAY, SHF_ALLOC | SHF_WRITE),
        n if n.starts_with(".text.") => (SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR),
        n if n.starts_with(".data.") => (SHT_PROGBITS, SHF_ALLOC | SHF_WRITE),
        n if n.starts_with(".bss.") => (SHT_NOBITS, SHF_ALLOC | SHF_WRITE),
        n if n.starts_with(".rodata.") => (SHT_PROGBITS, SHF_ALLOC),
        n if n.starts_with(".note.") => (SHT_NOTE, 0),
        _ => (SHT_PROGBITS, 0),
    };

    if flags_str.is_none() && type_str.is_none() {
        return (default_type, default_flags);
    }

    let mut flags = 0u32;
    if let Some(f) = flags_str {
        for c in f.chars() {
            match c {
                'a' => flags |= SHF_ALLOC,
                'w' => flags |= SHF_WRITE,
                'x' => flags |= SHF_EXECINSTR,
                'M' => flags |= SHF_MERGE,
                'S' => flags |= SHF_STRINGS,
                'T' => flags |= SHF_TLS,
                'G' => flags |= SHF_GROUP,
                'o' => {}
                _ => {}
            }
        }
    } else {
        flags = default_flags;
    }

    let section_type = if let Some(t) = type_str {
        match t {
            "@progbits" => SHT_PROGBITS,
            "@nobits" => SHT_NOBITS,
            "@note" => SHT_NOTE,
            "@init_array" => SHT_INIT_ARRAY,
            "@fini_array" => SHT_FINI_ARRAY,
            _ => default_type,
        }
    } else {
        default_type
    };

    (section_type, flags)
}
