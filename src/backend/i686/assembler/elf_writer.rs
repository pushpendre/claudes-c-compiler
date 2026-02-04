//! 32-bit ELF relocatable object file writer for i686.
//!
//! Produces .o files from parsed and encoded assembly. Uses ELFCLASS32,
//! EM_386, and Elf32 structures. Uses REL (not RELA) relocation format.

use std::collections::HashMap;
use crate::backend::x86::assembler::parser::*;
use super::encoder::*;

// ELF constants for 32-bit
const ELFCLASS32: u8 = 1;
const ELFDATA2LSB: u8 = 1;
const EV_CURRENT: u8 = 1;
const ELFOSABI_NONE: u8 = 0;
const ET_REL: u16 = 1;
const EM_386: u16 = 3;

// Section header types
const SHT_NULL: u32 = 0;
const SHT_PROGBITS: u32 = 1;
const SHT_SYMTAB: u32 = 2;
const SHT_STRTAB: u32 = 3;
const SHT_REL: u32 = 9;
const SHT_NOBITS: u32 = 8;
const SHT_INIT_ARRAY: u32 = 14;
const SHT_FINI_ARRAY: u32 = 15;
const SHT_NOTE: u32 = 7;

// Section header flags
const SHF_WRITE: u32 = 0x1;
const SHF_ALLOC: u32 = 0x2;
const SHF_EXECINSTR: u32 = 0x4;
const SHF_MERGE: u32 = 0x10;
const SHF_STRINGS: u32 = 0x20;
const SHF_TLS: u32 = 0x400;
const SHF_GROUP: u32 = 0x200;

// Symbol bindings
const STB_LOCAL: u8 = 0;
const STB_GLOBAL: u8 = 1;
const STB_WEAK: u8 = 2;

// Symbol types
const STT_NOTYPE: u8 = 0;
const STT_OBJECT: u8 = 1;
const STT_FUNC: u8 = 2;
const STT_SECTION: u8 = 3;
const STT_TLS: u8 = 6;

// Symbol visibility
const STV_DEFAULT: u8 = 0;
const STV_INTERNAL: u8 = 1;
const STV_HIDDEN: u8 = 2;
const STV_PROTECTED: u8 = 3;

const SHN_UNDEF: u16 = 0;
const SHN_COMMON: u16 = 0xFFF2;

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
                // Resolve .-symbol immediately to capture the current position
                match expr {
                    SizeExpr::CurrentMinusSymbol(start_sym) => {
                        if let Some(&(sec_idx, start_off)) = self.label_positions.get(start_sym) {
                            let current_off = self.sections[sec_idx].data.len() as u32;
                            let size = (current_off - start_off) as u64;
                            self.pending_sizes.insert(name.clone(), SizeExpr::Constant(size));
                        } else {
                            self.pending_sizes.insert(name.clone(), expr.clone());
                        }
                    }
                    _ => {
                        self.pending_sizes.insert(name.clone(), expr.clone());
                    }
                }
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
                        section.data.extend(std::iter::repeat(0x90).take(padding));
                    } else {
                        section.data.extend(std::iter::repeat(0).take(padding));
                    }
                }
            }
            AsmItem::Byte(vals) => {
                let section = self.current_section_mut()?;
                section.data.extend_from_slice(vals);
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
                section.data.extend(std::iter::repeat(0).take(*n as usize));
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
                    value: *align as u32,
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
                    section.data.extend(std::iter::repeat(0).take(size));
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
                    section.data.extend(std::iter::repeat(0).take(size));
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
                    section.data.extend(std::iter::repeat(0).take(size));
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
                    section.data.extend(std::iter::repeat(0).take(8));
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
                    section.data.extend(std::iter::repeat(0).take(8));
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
                    section.data.extend(std::iter::repeat(0).take(8));
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
        // Relax jumps first - this changes label positions and section data
        self.relax_jumps();

        // Update symbol values from label_positions after relaxation
        for (name, &(_, offset)) in &self.label_positions {
            if let Some(&sym_idx) = self.symbol_map.get(name) {
                self.symbols[sym_idx].value = offset;
            }
        }

        // Resolve sizes (after relaxation so positions are final)
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
                };
                self.symbols[sym_idx].size = size;
            }
        }

        // Resolve internal relocations
        self.resolve_internal_relocations();

        // Build the ELF file
        let mut elf = Elf32ByteWriter::new();
        elf.write_object(&self.sections, &self.symbols, &self.aliases, &self.label_positions)
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
                        if disp >= -128 && disp <= 127 {
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

/// Low-level 32-bit ELF byte serializer.
struct Elf32ByteWriter {
    output: Vec<u8>,
}

impl Elf32ByteWriter {
    fn new() -> Self {
        Elf32ByteWriter { output: Vec::new() }
    }

    fn write_object(
        &mut self,
        sections: &[Section],
        symbols: &[SymbolInfo],
        aliases: &HashMap<String, String>,
        label_positions: &HashMap<String, (usize, u32)>,
    ) -> Result<Vec<u8>, String> {
        let mut shstrtab = StringTable::new();
        let mut strtab = StringTable::new();

        shstrtab.add("");
        for sec in sections {
            shstrtab.add(&sec.name);
        }
        shstrtab.add(".symtab");
        shstrtab.add(".strtab");
        shstrtab.add(".shstrtab");
        for sec in sections {
            if !sec.relocations.is_empty() {
                shstrtab.add(&format!(".rel{}", sec.name));
            }
        }

        strtab.add("");
        for sym in symbols {
            if !sym.name.is_empty() {
                strtab.add(&sym.name);
            }
        }
        for (alias, _) in aliases {
            strtab.add(alias);
        }

        // Build symbol table
        let mut sym_entries: Vec<Sym32Entry> = Vec::new();
        sym_entries.push(Sym32Entry::null());

        // Section symbols
        let mut section_sym_indices: HashMap<usize, usize> = HashMap::new();
        for (i, _sec) in sections.iter().enumerate() {
            section_sym_indices.insert(i, sym_entries.len());
            sym_entries.push(Sym32Entry {
                name_offset: 0,
                value: 0,
                size: 0,
                info: (STB_LOCAL << 4) | STT_SECTION,
                other: STV_DEFAULT,
                shndx: (i + 1) as u16,
            });
        }

        // Local defined symbols
        let mut symbol_to_elf_idx: HashMap<String, usize> = HashMap::new();
        for sym in symbols {
            if sym.binding != STB_LOCAL { continue; }
            if sym.name.is_empty() || sym.name.starts_with('.') { continue; }

            let shndx = if sym.is_common {
                SHN_COMMON
            } else if let Some(ref sec_name) = sym.section {
                if let Some(sec_idx) = sections.iter().position(|s| s.name == *sec_name) {
                    (sec_idx + 1) as u16
                } else {
                    SHN_UNDEF
                }
            } else {
                SHN_UNDEF
            };

            symbol_to_elf_idx.insert(sym.name.clone(), sym_entries.len());
            sym_entries.push(Sym32Entry {
                name_offset: strtab.get_offset(&sym.name),
                value: sym.value,
                size: sym.size,
                info: (sym.binding << 4) | sym.sym_type,
                other: sym.visibility,
                shndx,
            });
        }

        let first_global = sym_entries.len() as u32;

        // Global and weak symbols
        for sym in symbols {
            if sym.binding == STB_LOCAL { continue; }

            let shndx = if sym.is_common {
                SHN_COMMON
            } else if let Some(ref sec_name) = sym.section {
                if let Some(sec_idx) = sections.iter().position(|s| s.name == *sec_name) {
                    (sec_idx + 1) as u16
                } else {
                    SHN_UNDEF
                }
            } else {
                SHN_UNDEF
            };

            symbol_to_elf_idx.insert(sym.name.clone(), sym_entries.len());
            sym_entries.push(Sym32Entry {
                name_offset: strtab.get_offset(&sym.name),
                value: if sym.is_common { sym.common_align } else { sym.value },
                size: sym.size,
                info: (sym.binding << 4) | sym.sym_type,
                other: sym.visibility,
                shndx,
            });
        }

        // Alias symbols
        for (alias, target) in aliases {
            if let Some(&target_idx) = symbol_to_elf_idx.get(target) {
                let target_entry = &sym_entries[target_idx];
                symbol_to_elf_idx.insert(alias.clone(), sym_entries.len());
                sym_entries.push(Sym32Entry {
                    name_offset: strtab.get_offset(alias),
                    value: target_entry.value,
                    size: target_entry.size,
                    info: target_entry.info,
                    other: target_entry.other,
                    shndx: target_entry.shndx,
                });
            }
        }

        // Create undefined symbols for external references
        for sec in sections {
            for reloc in &sec.relocations {
                let sym_name = &reloc.symbol;
                if sym_name.is_empty() || sym_name.starts_with('.') { continue; }
                if symbol_to_elf_idx.contains_key(sym_name) { continue; }
                strtab.add(sym_name);
                symbol_to_elf_idx.insert(sym_name.clone(), sym_entries.len());
                sym_entries.push(Sym32Entry {
                    name_offset: strtab.get_offset(sym_name),
                    value: 0,
                    size: 0,
                    info: (STB_GLOBAL << 4) | STT_NOTYPE,
                    other: STV_DEFAULT,
                    shndx: SHN_UNDEF,
                });
            }
        }

        // Compute layout
        // Elf32 header = 52 bytes
        let ehdr_size = 52u32;
        let mut offset = ehdr_size;

        // Section data offsets
        let mut section_offsets: Vec<u32> = Vec::new();
        for sec in sections {
            let align = std::cmp::max(sec.alignment, 1);
            offset = (offset + align - 1) & !(align - 1);
            section_offsets.push(offset);
            if sec.section_type != SHT_NOBITS {
                offset += sec.data.len() as u32;
            }
        }

        // Symtab (Elf32_Sym = 16 bytes each)
        let symtab_offset = (offset + 3) & !3; // 4-byte aligned
        let sym_entry_size = 16u32;
        let symtab_size = sym_entries.len() as u32 * sym_entry_size;
        offset = symtab_offset + symtab_size;

        // Strtab
        let strtab_offset = offset;
        let strtab_data = strtab.finish();
        offset += strtab_data.len() as u32;

        // Shstrtab
        let shstrtab_offset = offset;
        let shstrtab_data = shstrtab.finish();
        offset += shstrtab_data.len() as u32;

        // Rel sections (Elf32_Rel = 8 bytes each, NOT Elf32_Rela)
        let rel_entry_size = 8u32;
        let mut rel_offsets: Vec<(usize, u32, u32)> = Vec::new();
        for (i, sec) in sections.iter().enumerate() {
            if !sec.relocations.is_empty() {
                offset = (offset + 3) & !3;
                let rel_size = sec.relocations.len() as u32 * rel_entry_size;
                rel_offsets.push((i, offset, rel_size));
                offset += rel_size;
            }
        }

        // Section header table (Elf32_Shdr = 40 bytes each)
        let shdr_offset = (offset + 3) & !3;

        let num_data_sections = sections.len();
        let symtab_shndx = num_data_sections + 1;
        let strtab_shndx = num_data_sections + 2;
        let shstrtab_shndx = num_data_sections + 3;
        let total_sections = shstrtab_shndx + 1 + rel_offsets.len();

        // Write ELF header (52 bytes for 32-bit)
        self.output.clear();
        self.output.reserve(shdr_offset as usize + total_sections * 40);

        // e_ident
        self.output.extend_from_slice(&[0x7f, b'E', b'L', b'F']);
        self.output.push(ELFCLASS32);
        self.output.push(ELFDATA2LSB);
        self.output.push(EV_CURRENT);
        self.output.push(ELFOSABI_NONE);
        self.output.extend_from_slice(&[0u8; 8]); // padding

        self.write_u16(ET_REL);
        self.write_u16(EM_386);
        self.write_u32(1);                         // e_version
        self.write_u32(0);                         // e_entry
        self.write_u32(0);                         // e_phoff
        self.write_u32(shdr_offset);               // e_shoff
        self.write_u32(0);                         // e_flags
        self.write_u16(52);                        // e_ehsize
        self.write_u16(0);                         // e_phentsize
        self.write_u16(0);                         // e_phnum
        self.write_u16(40);                        // e_shentsize
        self.write_u16(total_sections as u16);     // e_shnum
        self.write_u16(shstrtab_shndx as u16);     // e_shstrndx

        // Write section data
        for (i, sec) in sections.iter().enumerate() {
            while self.output.len() < section_offsets[i] as usize {
                self.output.push(0);
            }
            if sec.section_type != SHT_NOBITS {
                self.output.extend_from_slice(&sec.data);
            }
        }

        // Write symtab (Elf32_Sym: 16 bytes each)
        while self.output.len() < symtab_offset as usize {
            self.output.push(0);
        }
        for entry in &sym_entries {
            self.write_u32(entry.name_offset as u32); // st_name
            self.write_u32(entry.value);               // st_value
            self.write_u32(entry.size);                // st_size
            self.output.push(entry.info);              // st_info
            self.output.push(entry.other);             // st_other
            self.write_u16(entry.shndx);               // st_shndx
        }

        // Write strtab
        self.output.extend_from_slice(&strtab_data);

        // Write shstrtab
        self.output.extend_from_slice(&shstrtab_data);

        // Write rel sections (Elf32_Rel: 8 bytes each)
        for &(sec_idx, rel_offset, _) in &rel_offsets {
            while self.output.len() < rel_offset as usize {
                self.output.push(0);
            }
            for reloc in &sections[sec_idx].relocations {
                let sym_idx = self.resolve_reloc_symbol(
                    &reloc.symbol, &symbol_to_elf_idx, &section_sym_indices,
                    label_positions, sections,
                );

                // For REL format, the addend is embedded in the instruction bytes.
                // We need to patch the section data with the addend.
                // For internal symbols referenced via section symbol, add the offset.
                let addend = if let Some(&(target_sec, target_off)) = label_positions.get(&reloc.symbol) {
                    if let Some(&sec_sym) = section_sym_indices.get(&target_sec) {
                        if sym_idx == sec_sym {
                            reloc.addend + target_off as i32
                        } else {
                            reloc.addend
                        }
                    } else {
                        reloc.addend
                    }
                } else {
                    reloc.addend
                };

                // Patch the implicit addend into section data
                // (REL format stores addend in the instruction bytes)
                if (reloc.offset as usize) + 4 <= sections[sec_idx].data.len() {
                    // Read existing value and add our addend
                    let off = reloc.offset as usize;
                    let existing = i32::from_le_bytes([
                        sections[sec_idx].data[off],
                        sections[sec_idx].data[off + 1],
                        sections[sec_idx].data[off + 2],
                        sections[sec_idx].data[off + 3],
                    ]);
                    let patched = existing.wrapping_add(addend);
                    // We can't mutate sections here since they're borrowed.
                    // We'll write the patched value via the output if already written,
                    // or we need a different approach. For now, we'll handle this
                    // by embedding the addend before writing sections.
                    // Since we already wrote section data above, we need to patch in output.
                    let sec_data_offset = section_offsets[sec_idx] as usize + off;
                    if sec_data_offset + 4 <= self.output.len() {
                        let bytes = patched.to_le_bytes();
                        self.output[sec_data_offset..sec_data_offset + 4].copy_from_slice(&bytes);
                    }
                }

                self.write_u32(reloc.offset);
                // r_info = (sym << 8) | type  (32-bit format)
                let r_info = ((sym_idx as u32) << 8) | (reloc.reloc_type & 0xFF);
                self.write_u32(r_info);
            }
        }

        // Write section header table (Elf32_Shdr: 40 bytes each)
        while self.output.len() < shdr_offset as usize {
            self.output.push(0);
        }

        // Section 0: NULL
        self.write_shdr32(0, SHT_NULL, 0, 0, 0, 0, 0, 0, 0, 0);

        // Data sections
        for (i, sec) in sections.iter().enumerate() {
            self.write_shdr32(
                shstrtab.get_offset(&sec.name) as u32,
                sec.section_type,
                sec.flags,
                0,
                section_offsets[i],
                sec.data.len() as u32,
                0,
                0,
                sec.alignment,
                0,
            );
        }

        // .symtab
        self.write_shdr32(
            shstrtab.get_offset(".symtab") as u32,
            SHT_SYMTAB,
            0,
            0,
            symtab_offset,
            symtab_size,
            strtab_shndx as u32,
            first_global,
            4,
            sym_entry_size,
        );

        // .strtab
        self.write_shdr32(
            shstrtab.get_offset(".strtab") as u32,
            SHT_STRTAB,
            0,
            0,
            strtab_offset,
            strtab_data.len() as u32,
            0, 0, 1, 0,
        );

        // .shstrtab
        self.write_shdr32(
            shstrtab.get_offset(".shstrtab") as u32,
            SHT_STRTAB,
            0,
            0,
            shstrtab_offset,
            shstrtab_data.len() as u32,
            0, 0, 1, 0,
        );

        // .rel.* sections
        for &(sec_idx, rel_offset, rel_size) in &rel_offsets {
            let rel_name = format!(".rel{}", sections[sec_idx].name);
            self.write_shdr32(
                shstrtab.get_offset(&rel_name) as u32,
                SHT_REL,
                0,
                0,
                rel_offset,
                rel_size,
                symtab_shndx as u32,
                (sec_idx + 1) as u32,
                4,
                rel_entry_size,
            );
        }

        Ok(self.output.clone())
    }

    fn resolve_reloc_symbol(
        &self,
        symbol: &str,
        symbol_to_elf_idx: &HashMap<String, usize>,
        section_sym_indices: &HashMap<usize, usize>,
        label_positions: &HashMap<String, (usize, u32)>,
        _sections: &[Section],
    ) -> usize {
        if let Some(&idx) = symbol_to_elf_idx.get(symbol) {
            return idx;
        }
        if let Some(&(sec_idx, _)) = label_positions.get(symbol) {
            if let Some(&sec_sym_idx) = section_sym_indices.get(&sec_idx) {
                return sec_sym_idx;
            }
        }
        0
    }

    fn write_shdr32(&mut self, name: u32, shtype: u32, flags: u32, addr: u32,
                    offset: u32, size: u32, link: u32, info: u32, addralign: u32, entsize: u32) {
        self.write_u32(name);
        self.write_u32(shtype);
        self.write_u32(flags);
        self.write_u32(addr);
        self.write_u32(offset);
        self.write_u32(size);
        self.write_u32(link);
        self.write_u32(info);
        self.write_u32(addralign);
        self.write_u32(entsize);
    }

    fn write_u16(&mut self, v: u16) { self.output.extend_from_slice(&v.to_le_bytes()); }
    fn write_u32(&mut self, v: u32) { self.output.extend_from_slice(&v.to_le_bytes()); }
}

/// Elf32_Sym entry.
struct Sym32Entry {
    name_offset: usize,
    value: u32,
    size: u32,
    info: u8,
    other: u8,
    shndx: u16,
}

impl Sym32Entry {
    fn null() -> Self {
        Sym32Entry { name_offset: 0, value: 0, size: 0, info: 0, other: 0, shndx: 0 }
    }
}

/// Simple string table builder.
struct StringTable {
    data: Vec<u8>,
    offsets: HashMap<String, usize>,
}

impl StringTable {
    fn new() -> Self {
        StringTable { data: Vec::new(), offsets: HashMap::new() }
    }

    fn add(&mut self, s: &str) {
        if self.offsets.contains_key(s) { return; }
        let offset = self.data.len();
        self.data.extend_from_slice(s.as_bytes());
        self.data.push(0);
        self.offsets.insert(s.to_string(), offset);
    }

    fn get_offset(&self, s: &str) -> usize {
        self.offsets.get(s).copied().unwrap_or(0)
    }

    fn finish(&self) -> Vec<u8> {
        self.data.clone()
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
