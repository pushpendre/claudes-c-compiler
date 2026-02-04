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

// ELF constants
const ELFCLASS64: u8 = 2;
const ELFDATA2LSB: u8 = 1;
const EV_CURRENT: u8 = 1;
const ELFOSABI_NONE: u8 = 0;
const ET_REL: u16 = 1;
const EM_X86_64: u16 = 62;

// Section header types
const SHT_NULL: u32 = 0;
const SHT_PROGBITS: u32 = 1;
const SHT_SYMTAB: u32 = 2;
const SHT_STRTAB: u32 = 3;
const SHT_RELA: u32 = 4;
const SHT_NOBITS: u32 = 8;
const SHT_INIT_ARRAY: u32 = 14;
const SHT_FINI_ARRAY: u32 = 15;
const SHT_NOTE: u32 = 7;

// Section header flags
const SHF_WRITE: u64 = 0x1;
const SHF_ALLOC: u64 = 0x2;
const SHF_EXECINSTR: u64 = 0x4;
const SHF_MERGE: u64 = 0x10;
const SHF_STRINGS: u64 = 0x20;
const SHF_TLS: u64 = 0x400;
const SHF_GROUP: u64 = 0x200;

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
#[allow(dead_code)]
const SHN_ABS: u16 = 0xFFF1;
const SHN_COMMON: u16 = 0xFFF2;

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
    /// Index in the final section header table.
    #[allow(dead_code)]
    index: usize,
}

#[derive(Clone)]
struct ElfRelocation {
    offset: u64,
    symbol: String,
    reloc_type: u32,
    addend: i64,
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
        }
    }

    /// Build the ELF object file from parsed assembly items.
    pub fn build(mut self, items: &[AsmItem]) -> Result<Vec<u8>, String> {
        // First pass: process all items
        for item in items {
            self.process_item(item)?;
        }

        // Second pass: resolve internal labels and build the ELF
        self.emit_elf()
    }

    fn get_or_create_section(&mut self, name: &str, section_type: u32, flags: u64) -> usize {
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
            index: 0, // will be set later
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
                            let current_off = self.sections[sec_idx].data.len() as u64;
                            let size = current_off - start_off;
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
                let offset = self.sections[sec_idx].data.len() as u64;
                self.label_positions.insert(name.clone(), (sec_idx, offset));

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
                self.emit_data_values(vals, 8)?;
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
            let idx = self.get_or_create_section(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR);
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
                    if size == 4 {
                        section.data.extend_from_slice(&(*v as i32).to_le_bytes());
                    } else {
                        section.data.extend_from_slice(&v.to_le_bytes());
                    }
                }
                DataValue::Symbol(sym) => {
                    let offset = self.sections[sec_idx].data.len() as u64;
                    let reloc_type = if size == 4 { R_X86_64_32 } else { R_X86_64_64 };
                    self.sections[sec_idx].relocations.push(ElfRelocation {
                        offset,
                        symbol: sym.clone(),
                        reloc_type,
                        addend: 0,
                    });
                    let section = &mut self.sections[sec_idx];
                    section.data.extend(std::iter::repeat(0).take(size));
                }
                DataValue::SymbolOffset(sym, addend) => {
                    let offset = self.sections[sec_idx].data.len() as u64;
                    let reloc_type = if size == 4 { R_X86_64_32 } else { R_X86_64_64 };
                    self.sections[sec_idx].relocations.push(ElfRelocation {
                        offset,
                        symbol: sym.clone(),
                        reloc_type,
                        addend: *addend,
                    });
                    let section = &mut self.sections[sec_idx];
                    section.data.extend(std::iter::repeat(0).take(size));
                }
                DataValue::SymbolDiff(a, _b) => {
                    // For .long .LBB3 - .Ljt_0, we need to resolve this.
                    // If both are in the same section, this becomes a constant.
                    // Otherwise, we need a pair of relocations.
                    // For now, emit placeholder and add relocation.
                    let offset = self.sections[sec_idx].data.len() as u64;
                    // Emit symbol_a - symbol_b as a relocation against symbol_a
                    // with addend that subtracts symbol_b's position
                    self.sections[sec_idx].relocations.push(ElfRelocation {
                        offset,
                        symbol: a.clone(),
                        reloc_type: if size == 4 { R_X86_64_PC32 } else { R_X86_64_64 },
                        addend: 0,
                    });
                    let section = &mut self.sections[sec_idx];
                    section.data.extend(std::iter::repeat(0).take(size));
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
            });
        }

        Ok(())
    }

    /// Extract the target label from a jump instruction, if any.
    fn get_jump_target_label(&self, instr: &Instruction) -> Option<String> {
        let mnem = &instr.mnemonic;
        let is_jump = mnem == "jmp" || (mnem.starts_with('j') && mnem.len() >= 2 && mnem != "jmp");
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

    /// Emit the final ELF object file.
    fn emit_elf(mut self) -> Result<Vec<u8>, String> {
        // Relax long jumps to short jumps where possible.
        // This must happen BEFORE resolving sizes and symbol values,
        // because relaxation changes label positions and section data lengths.
        self.relax_jumps();

        // Update symbol values from label_positions after relaxation
        for (name, &(sec_idx, offset)) in &self.label_positions {
            if let Some(&sym_idx) = self.symbol_map.get(name) {
                self.symbols[sym_idx].value = offset;
            }
        }

        // Resolve sizes from .size directives (after relaxation so positions are final)
        for (name, expr) in &self.pending_sizes {
            if let Some(&sym_idx) = self.symbol_map.get(name) {
                let size = match expr {
                    SizeExpr::Constant(v) => *v,
                    SizeExpr::CurrentMinusSymbol(start_sym) => {
                        // .-symbol: difference from start of symbol to current position
                        if let Some(&(sec_idx, start_off)) = self.label_positions.get(start_sym) {
                            let end = self.sections[sec_idx].data.len() as u64;
                            end - start_off
                        } else {
                            0
                        }
                    }
                };
                self.symbols[sym_idx].size = size;
            }
        }

        // Resolve internal relocations (labels within the same section)
        self.resolve_internal_relocations();

        // Build the ELF file
        let mut elf = ElfByteWriter::new();
        elf.write_object(&self.sections, &self.symbols, &self.aliases, &self.label_positions)
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
                    if let Some(&target_off) = local_labels.get(&jump.target) {
                        // Compute displacement from end of the SHORT instruction
                        // Short form is always 2 bytes, so end = jump.offset + 2
                        let short_end = jump.offset as i64 + 2;
                        let disp = target_off as i64 - short_end;
                        if disp >= -128 && disp <= 127 {
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

                    // Mark this jump as relaxed
                    self.sections[sec_idx].jumps[j_idx].relaxed = true;
                    self.sections[sec_idx].jumps[j_idx].len = new_len;
                    any_relaxed = true;
                }

                if !any_relaxed {
                    break;
                }
            }

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
                        && (reloc.reloc_type == R_X86_64_PC32 || reloc.reloc_type == R_X86_64_PLT32)
                    {
                        // Same-section PC-relative to a local symbol: resolve now.
                        // ELF formula: S + A - P
                        // S = target_off (symbol value)
                        // A = reloc.addend
                        // P = reloc.offset (relocation position)
                        let rel = (target_off as i64) + reloc.addend - (reloc.offset as i64);
                        resolved.push((reloc.offset as usize, rel as i32));
                    } else {
                        unresolved.push(reloc.clone());
                    }
                } else {
                    unresolved.push(reloc.clone());
                }
            }

            // Patch resolved relocations into section data
            for (offset, value) in resolved {
                let bytes = value.to_le_bytes();
                self.sections[sec_idx].data[offset..offset + 4].copy_from_slice(&bytes);
            }

            self.sections[sec_idx].relocations = unresolved;
        }
    }
}

/// Low-level ELF byte serializer.
struct ElfByteWriter {
    output: Vec<u8>,
}

impl ElfByteWriter {
    fn new() -> Self {
        ElfByteWriter { output: Vec::new() }
    }

    fn write_object(
        &mut self,
        sections: &[Section],
        symbols: &[SymbolInfo],
        aliases: &HashMap<String, String>,
        label_positions: &HashMap<String, (usize, u64)>,
    ) -> Result<Vec<u8>, String> {
        // Plan the layout:
        // 1. ELF header (64 bytes)
        // 2. Section data (each section's contents)
        // 3. .symtab (symbol table)
        // 4. .strtab (string table for symbols)
        // 5. .shstrtab (string table for section names)
        // 6. .rela.* sections (one per section with relocations)
        // 7. Section header table

        // Build string tables
        let mut shstrtab = StringTable::new();
        let mut strtab = StringTable::new();

        // Add section names
        shstrtab.add(""); // Null entry
        for sec in sections {
            shstrtab.add(&sec.name);
        }
        shstrtab.add(".symtab");
        shstrtab.add(".strtab");
        shstrtab.add(".shstrtab");
        for sec in sections {
            if !sec.relocations.is_empty() {
                shstrtab.add(&format!(".rela{}", sec.name));
            }
        }

        // Add symbol names
        strtab.add(""); // Null entry
        for sym in symbols {
            if !sym.name.is_empty() {
                strtab.add(&sym.name);
            }
        }
        // Add names for alias symbols
        for (alias, _) in aliases {
            strtab.add(alias);
        }

        // Build symbol table
        // Order: null symbol, local symbols (section + defined), then global/weak
        let mut sym_entries: Vec<SymEntry> = Vec::new();
        // Null symbol (index 0)
        sym_entries.push(SymEntry::null());

        // Section symbols (one per section, local)
        // Section symbols use st_name=0 (empty) since the section header identifies them.
        let mut section_sym_indices: HashMap<usize, usize> = HashMap::new();
        for (i, _sec) in sections.iter().enumerate() {
            section_sym_indices.insert(i, sym_entries.len());
            sym_entries.push(SymEntry {
                name_offset: 0, // Section symbols use empty name
                info: (STB_LOCAL << 4) | STT_SECTION,
                other: STV_DEFAULT,
                shndx: (i + 1) as u16, // section indices start at 1 (0 is null)
                value: 0,
                size: 0,
            });
        }

        // Local defined symbols
        let mut symbol_to_elf_idx: HashMap<String, usize> = HashMap::new();
        for sym in symbols {
            if sym.binding != STB_LOCAL {
                continue;
            }
            if sym.name.is_empty() || sym.name.starts_with('.') {
                // Skip internal labels (we handle them via section symbols)
                continue;
            }
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
            sym_entries.push(SymEntry {
                name_offset: strtab.get_offset(&sym.name),
                info: (sym.binding << 4) | sym.sym_type,
                other: sym.visibility,
                shndx,
                value: sym.value,
                size: sym.size,
            });
        }

        let first_global = sym_entries.len() as u32;

        // Global and weak symbols
        for sym in symbols {
            if sym.binding == STB_LOCAL {
                continue;
            }
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
            sym_entries.push(SymEntry {
                name_offset: strtab.get_offset(&sym.name),
                info: (sym.binding << 4) | sym.sym_type,
                other: sym.visibility,
                shndx,
                value: if sym.is_common { sym.common_align as u64 } else { sym.value },
                size: sym.size,
            });
        }

        // Alias symbols (.set)
        for (alias, target) in aliases {
            // Find target symbol
            if let Some(&target_idx) = symbol_to_elf_idx.get(target) {
                let target_entry = &sym_entries[target_idx];
                symbol_to_elf_idx.insert(alias.clone(), sym_entries.len());
                sym_entries.push(SymEntry {
                    name_offset: strtab.get_offset(alias),
                    info: target_entry.info,
                    other: target_entry.other,
                    shndx: target_entry.shndx,
                    value: target_entry.value,
                    size: target_entry.size,
                });
            }
        }

        // Create undefined symbols for any relocations that reference
        // symbols not yet in the symbol table (external references like printf).
        for sec in sections {
            for reloc in &sec.relocations {
                let sym_name = &reloc.symbol;
                if sym_name.is_empty() || sym_name.starts_with('.') {
                    continue; // Skip internal labels
                }
                if symbol_to_elf_idx.contains_key(sym_name) {
                    continue; // Already defined
                }
                // Create undefined global symbol
                strtab.add(sym_name);
                symbol_to_elf_idx.insert(sym_name.clone(), sym_entries.len());
                sym_entries.push(SymEntry {
                    name_offset: strtab.get_offset(sym_name),
                    info: (STB_GLOBAL << 4) | STT_NOTYPE,
                    other: STV_DEFAULT,
                    shndx: SHN_UNDEF,
                    value: 0,
                    size: 0,
                });
            }
        }

        // Now compute the layout
        let ehdr_size = 64u64;
        let mut offset = ehdr_size;

        // Section data offsets
        let mut section_offsets: Vec<u64> = Vec::new();
        for sec in sections {
            // Align section data
            let align = std::cmp::max(sec.alignment, 1);
            offset = (offset + align - 1) & !(align - 1);
            section_offsets.push(offset);
            if sec.section_type != SHT_NOBITS {
                offset += sec.data.len() as u64;
            }
        }

        // Symtab
        let symtab_offset = (offset + 7) & !7; // 8-byte aligned
        let sym_entry_size = 24u64; // Elf64_Sym size
        let symtab_size = sym_entries.len() as u64 * sym_entry_size;
        offset = symtab_offset + symtab_size;

        // Strtab
        let strtab_offset = offset;
        let strtab_data = strtab.finish();
        offset += strtab_data.len() as u64;

        // Shstrtab
        let shstrtab_offset = offset;
        let shstrtab_data = shstrtab.finish();
        offset += shstrtab_data.len() as u64;

        // Rela sections
        let rela_entry_size = 24u64; // Elf64_Rela size
        let mut rela_offsets: Vec<(usize, u64, u64)> = Vec::new(); // (sec_idx, offset, size)
        for (i, sec) in sections.iter().enumerate() {
            if !sec.relocations.is_empty() {
                offset = (offset + 7) & !7;
                let rela_size = sec.relocations.len() as u64 * rela_entry_size;
                rela_offsets.push((i, offset, rela_size));
                offset += rela_size;
            }
        }

        // Section header table
        let shdr_offset = (offset + 7) & !7;

        // Count total sections:
        // 0: NULL
        // 1..N: data sections
        // N+1: .symtab
        // N+2: .strtab
        // N+3: .shstrtab
        // N+4..: .rela.* sections
        let num_data_sections = sections.len();
        let symtab_shndx = num_data_sections + 1;
        let strtab_shndx = num_data_sections + 2;
        let shstrtab_shndx = num_data_sections + 3;
        let total_sections = shstrtab_shndx + 1 + rela_offsets.len();

        // Write ELF header
        self.output.clear();
        self.output.reserve(shdr_offset as usize + total_sections * 64);

        // e_ident
        self.output.extend_from_slice(&[0x7f, b'E', b'L', b'F']); // magic
        self.output.push(ELFCLASS64);
        self.output.push(ELFDATA2LSB);
        self.output.push(EV_CURRENT);
        self.output.push(ELFOSABI_NONE);
        self.output.extend_from_slice(&[0u8; 8]); // padding

        self.write_u16(ET_REL);           // e_type
        self.write_u16(EM_X86_64);        // e_machine
        self.write_u32(1);                // e_version
        self.write_u64(0);                // e_entry
        self.write_u64(0);                // e_phoff
        self.write_u64(shdr_offset);      // e_shoff
        self.write_u32(0);                // e_flags
        self.write_u16(64);               // e_ehsize
        self.write_u16(0);                // e_phentsize
        self.write_u16(0);                // e_phnum
        self.write_u16(64);               // e_shentsize
        self.write_u16(total_sections as u16); // e_shnum
        self.write_u16(shstrtab_shndx as u16); // e_shstrndx

        // Write section data
        for (i, sec) in sections.iter().enumerate() {
            // Pad to alignment
            while self.output.len() < section_offsets[i] as usize {
                self.output.push(0);
            }
            if sec.section_type != SHT_NOBITS {
                self.output.extend_from_slice(&sec.data);
            }
        }

        // Write symtab
        while self.output.len() < symtab_offset as usize {
            self.output.push(0);
        }
        for entry in &sym_entries {
            self.write_u32(entry.name_offset as u32); // st_name
            self.output.push(entry.info);              // st_info
            self.output.push(entry.other);             // st_other
            self.write_u16(entry.shndx);              // st_shndx
            self.write_u64(entry.value);              // st_value
            self.write_u64(entry.size);               // st_size
        }

        // Write strtab
        self.output.extend_from_slice(&strtab_data);

        // Write shstrtab
        self.output.extend_from_slice(&shstrtab_data);

        // Write rela sections
        for &(sec_idx, rela_offset, _) in &rela_offsets {
            while self.output.len() < rela_offset as usize {
                self.output.push(0);
            }
            for reloc in &sections[sec_idx].relocations {
                // Find symbol index in ELF symtab
                let sym_idx = self.resolve_reloc_symbol(
                    &reloc.symbol, &symbol_to_elf_idx, &section_sym_indices,
                    label_positions, sections,
                );

                self.write_u64(reloc.offset);
                // r_info = (sym << 32) | type
                let r_info = ((sym_idx as u64) << 32) | (reloc.reloc_type as u64);
                self.write_u64(r_info);

                // Compute addend: for local symbols referenced by section symbol,
                // the addend includes the symbol's offset within the section
                let addend = if let Some(&(target_sec, target_off)) = label_positions.get(&reloc.symbol) {
                    if let Some(&sec_sym) = section_sym_indices.get(&target_sec) {
                        if sym_idx == sec_sym {
                            // Using section symbol: addend must include offset
                            reloc.addend + target_off as i64
                        } else {
                            reloc.addend
                        }
                    } else {
                        reloc.addend
                    }
                } else {
                    reloc.addend
                };
                self.write_i64(addend);
            }
        }

        // Write section header table
        while self.output.len() < shdr_offset as usize {
            self.output.push(0);
        }

        // Section 0: NULL
        self.write_shdr(0, SHT_NULL, 0, 0, 0, 0, 0, 0, 0, 0);

        // Data sections
        for (i, sec) in sections.iter().enumerate() {
            self.write_shdr(
                shstrtab.get_offset(&sec.name) as u32,
                sec.section_type,
                sec.flags,
                0, // addr
                section_offsets[i],
                if sec.section_type == SHT_NOBITS { sec.data.len() as u64 } else { sec.data.len() as u64 },
                0, // link
                0, // info
                sec.alignment,
                0, // entsize
            );
        }

        // .symtab
        self.write_shdr(
            shstrtab.get_offset(".symtab") as u32,
            SHT_SYMTAB,
            0,
            0,
            symtab_offset,
            symtab_size,
            strtab_shndx as u32, // link to strtab
            first_global,        // info: first global symbol index
            8,
            sym_entry_size,
        );

        // .strtab
        self.write_shdr(
            shstrtab.get_offset(".strtab") as u32,
            SHT_STRTAB,
            0,
            0,
            strtab_offset,
            strtab_data.len() as u64,
            0, 0, 1, 0,
        );

        // .shstrtab
        self.write_shdr(
            shstrtab.get_offset(".shstrtab") as u32,
            SHT_STRTAB,
            0,
            0,
            shstrtab_offset,
            shstrtab_data.len() as u64,
            0, 0, 1, 0,
        );

        // .rela.* sections
        for &(sec_idx, rela_offset, rela_size) in &rela_offsets {
            let rela_name = format!(".rela{}", sections[sec_idx].name);
            self.write_shdr(
                shstrtab.get_offset(&rela_name) as u32,
                SHT_RELA,
                0,
                0,
                rela_offset,
                rela_size,
                symtab_shndx as u32,     // link to symtab
                (sec_idx + 1) as u32,     // info: section this rela applies to
                8,
                rela_entry_size,
            );
        }

        Ok(self.output.clone())
    }

    fn resolve_reloc_symbol(
        &self,
        symbol: &str,
        symbol_to_elf_idx: &HashMap<String, usize>,
        section_sym_indices: &HashMap<usize, usize>,
        label_positions: &HashMap<String, (usize, u64)>,
        _sections: &[Section],
    ) -> usize {
        // First try direct symbol lookup
        if let Some(&idx) = symbol_to_elf_idx.get(symbol) {
            return idx;
        }

        // For internal labels (.LBB*, .Lstr*, etc.), use the section symbol
        if let Some(&(sec_idx, _)) = label_positions.get(symbol) {
            if let Some(&sec_sym_idx) = section_sym_indices.get(&sec_idx) {
                return sec_sym_idx;
            }
        }

        // External/undefined symbol - it should be in the symbol table
        // If not found, return 0 (undefined)
        // TODO: Create undefined symbol entries for forward references
        0
    }

    fn write_shdr(&mut self, name: u32, shtype: u32, flags: u64, addr: u64,
                  offset: u64, size: u64, link: u32, info: u32, addralign: u64, entsize: u64) {
        self.write_u32(name);
        self.write_u32(shtype);
        self.write_u64(flags);
        self.write_u64(addr);
        self.write_u64(offset);
        self.write_u64(size);
        self.write_u32(link);
        self.write_u32(info);
        self.write_u64(addralign);
        self.write_u64(entsize);
    }

    fn write_u16(&mut self, v: u16) { self.output.extend_from_slice(&v.to_le_bytes()); }
    fn write_u32(&mut self, v: u32) { self.output.extend_from_slice(&v.to_le_bytes()); }
    fn write_u64(&mut self, v: u64) { self.output.extend_from_slice(&v.to_le_bytes()); }
    fn write_i64(&mut self, v: i64) { self.output.extend_from_slice(&v.to_le_bytes()); }
}

/// Elf64_Sym entry.
struct SymEntry {
    name_offset: usize,
    info: u8,
    other: u8,
    shndx: u16,
    value: u64,
    size: u64,
}

impl SymEntry {
    fn null() -> Self {
        SymEntry {
            name_offset: 0,
            info: 0,
            other: 0,
            shndx: 0,
            value: 0,
            size: 0,
        }
    }
}

/// Simple string table builder.
struct StringTable {
    data: Vec<u8>,
    offsets: HashMap<String, usize>,
}

impl StringTable {
    fn new() -> Self {
        StringTable {
            data: Vec::new(),
            offsets: HashMap::new(),
        }
    }

    fn add(&mut self, s: &str) {
        if self.offsets.contains_key(s) {
            return;
        }
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

/// Parse section name, flags string, and type into ELF section type and flags.
fn parse_section_flags(name: &str, flags_str: Option<&str>, type_str: Option<&str>) -> (u32, u64) {
    // Default flags based on well-known section names
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

    let mut flags = 0u64;
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
                'o' => {} // SHF_LINK_ORDER - handle later
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
