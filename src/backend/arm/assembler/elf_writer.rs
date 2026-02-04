//! ELF object file writer for AArch64.
//!
//! Takes parsed assembly statements and produces an ELF .o (relocatable) file
//! with proper sections, symbols, and relocations for AArch64/ELF64.

#![allow(dead_code)]

use std::collections::HashMap;
use super::parser::{AsmStatement, AsmDirective, SectionDirective, SymbolKind, SizeExpr, DataValue, Operand};
use super::encoder::{encode_instruction, EncodeResult, RelocType};
use crate::backend::elf::{self,
    SHT_PROGBITS, SHT_NOBITS, SHT_NOTE,
    SHF_WRITE, SHF_ALLOC, SHF_EXECINSTR, SHF_MERGE, SHF_STRINGS,
    SHF_TLS, SHF_GROUP,
    STB_GLOBAL,
    STT_NOTYPE, STT_OBJECT, STT_FUNC, STT_TLS,
    STV_DEFAULT, STV_HIDDEN, STV_PROTECTED, STV_INTERNAL,
    ELFCLASS64, EM_AARCH64,
    SymbolTableInput,
};

/// An ELF section being built.
struct Section {
    name: String,
    sh_type: u32,
    sh_flags: u64,
    data: Vec<u8>,
    sh_addralign: u64,
    sh_entsize: u64,
    sh_link: u32,
    sh_info: u32,
    /// Relocations for this section
    relocs: Vec<ElfReloc>,
}

/// A relocation entry.
struct ElfReloc {
    offset: u64,
    reloc_type: u32,
    symbol_name: String,
    addend: i64,
}

/// A symbol entry.
struct ElfSymbol {
    name: String,
    value: u64,
    size: u64,
    binding: u8,
    sym_type: u8,
    visibility: u8,
    section_name: String,
}

/// The ELF writer state machine.
pub struct ElfWriter {
    /// Current section we're emitting into
    current_section: String,
    /// All sections being built
    sections: HashMap<String, Section>,
    /// Section order (for deterministic output)
    section_order: Vec<String>,
    /// Symbol table
    symbols: Vec<ElfSymbol>,
    /// Local labels -> (section, offset) for branch resolution
    labels: HashMap<String, (String, u64)>,
    /// Pending relocations that reference labels (need fixup)
    pending_branch_relocs: Vec<PendingReloc>,
    /// Pending symbol differences to resolve after all labels are known
    pending_sym_diffs: Vec<PendingSymDiff>,
    /// Current alignment state
    current_align: u64,
    /// Symbols that have been declared .globl
    global_symbols: HashMap<String, bool>,
    /// Symbols declared .weak
    weak_symbols: HashMap<String, bool>,
    /// Symbol types from .type directives
    symbol_types: HashMap<String, u8>,
    /// Symbol sizes from .size directives
    symbol_sizes: HashMap<String, u64>,
    /// Symbol visibility from .hidden/.protected/.internal
    symbol_visibility: HashMap<String, u8>,
    /// Symbol aliases from .set/.equ directives
    aliases: HashMap<String, String>,
    /// Section stack for .pushsection/.popsection
    section_stack: Vec<String>,
}

struct PendingReloc {
    section: String,
    offset: u64,
    reloc_type: u32,
    symbol: String,
    addend: i64,
}

/// A pending symbol difference to be resolved after all labels are known.
struct PendingSymDiff {
    /// Section containing the data directive
    section: String,
    /// Offset within that section where the value should be written
    offset: u64,
    /// The positive symbol (A in A - B)
    sym_a: String,
    /// The negative symbol (B in A - B)
    sym_b: String,
    /// Extra addend
    extra_addend: i64,
    /// Size in bytes (4 or 8)
    size: usize,
}

impl ElfWriter {
    pub fn new() -> Self {
        Self {
            current_section: String::new(),
            sections: HashMap::new(),
            section_order: Vec::new(),
            symbols: Vec::new(),
            labels: HashMap::new(),
            pending_branch_relocs: Vec::new(),
            pending_sym_diffs: Vec::new(),
            current_align: 4,
            global_symbols: HashMap::new(),
            weak_symbols: HashMap::new(),
            symbol_types: HashMap::new(),
            symbol_sizes: HashMap::new(),
            symbol_visibility: HashMap::new(),
            aliases: HashMap::new(),
            section_stack: Vec::new(),
        }
    }

    fn ensure_section(&mut self, name: &str, sh_type: u32, sh_flags: u64, align: u64) {
        if !self.sections.contains_key(name) {
            self.sections.insert(name.to_string(), Section {
                name: name.to_string(),
                sh_type,
                sh_flags,
                data: Vec::new(),
                sh_addralign: align,
                sh_entsize: 0,
                sh_link: 0,
                sh_info: 0,
                relocs: Vec::new(),
            });
            self.section_order.push(name.to_string());
        }
    }

    fn current_offset(&self) -> u64 {
        self.sections.get(&self.current_section)
            .map(|s| s.data.len() as u64)
            .unwrap_or(0)
    }

    fn emit_bytes(&mut self, bytes: &[u8]) {
        if let Some(section) = self.sections.get_mut(&self.current_section) {
            section.data.extend_from_slice(bytes);
        }
    }

    fn emit_u32_le(&mut self, val: u32) {
        self.emit_bytes(&val.to_le_bytes());
    }

    fn add_reloc(&mut self, reloc_type: u32, symbol: String, addend: i64) {
        let offset = self.current_offset();
        let section = self.current_section.clone();
        if let Some(s) = self.sections.get_mut(&section) {
            s.relocs.push(ElfReloc {
                offset,
                reloc_type,
                symbol_name: symbol,
                addend,
            });
        }
    }

    /// Resolve pending symbol differences after all labels are known.
    fn resolve_sym_diffs(&mut self) -> Result<(), String> {
        let pending = std::mem::take(&mut self.pending_sym_diffs);
        for diff in &pending {
            let sym_a_info = self.labels.get(&diff.sym_a).cloned();
            let sym_b_info = self.labels.get(&diff.sym_b).cloned();

            match (sym_a_info, sym_b_info) {
                (Some((sec_a, off_a)), Some((sec_b, off_b))) => {
                    if sec_a == sec_b {
                        // Same section: resolve at assembly time by patching the data
                        let value = (off_a as i64) - (off_b as i64) + diff.extra_addend;
                        if let Some(section) = self.sections.get_mut(&diff.section) {
                            let off = diff.offset as usize;
                            if diff.size == 4 && off + 4 <= section.data.len() {
                                section.data[off..off + 4].copy_from_slice(&(value as i32).to_le_bytes());
                            } else if diff.size == 8 && off + 8 <= section.data.len() {
                                section.data[off..off + 8].copy_from_slice(&value.to_le_bytes());
                            }
                        }
                    } else {
                        // Cross-section: emit R_AARCH64_PREL32 referencing sym_a's section symbol
                        // PREL32 computes: S + A - P
                        // We want: (sec_a_base + off_a) - (sec_b_base + off_b) + extra_addend
                        // P = diff.section base + diff.offset
                        // If diff.section == sec_b:
                        //   want = (sec_a_base + off_a) - (sec_b_base + off_b) + extra_addend
                        //   PREL32 = sec_a_base + A - (sec_b_base + diff.offset)
                        //   so A = off_a + diff.offset - off_b + extra_addend
                        let addend = off_a as i64 + diff.offset as i64 - off_b as i64 + diff.extra_addend;
                        if let Some(section) = self.sections.get_mut(&diff.section) {
                            section.relocs.push(ElfReloc {
                                offset: diff.offset,
                                reloc_type: RelocType::Prel32.elf_type(),
                                symbol_name: sec_a.clone(),
                                addend,
                            });
                        }
                    }
                }
                _ => {
                    // Forward-referenced or external symbols: emit PREL32 with symbol name
                    // This shouldn't happen for jump tables, but handle gracefully
                    if let Some(section) = self.sections.get_mut(&diff.section) {
                        section.relocs.push(ElfReloc {
                            offset: diff.offset,
                            reloc_type: RelocType::Prel32.elf_type(),
                            symbol_name: diff.sym_a.clone(),
                            addend: diff.extra_addend,
                        });
                    }
                }
            }
        }
        Ok(())
    }

    fn align_to(&mut self, align: u64) {
        if align <= 1 {
            return;
        }
        if let Some(section) = self.sections.get_mut(&self.current_section) {
            let current = section.data.len() as u64;
            let aligned = (current + align - 1) & !(align - 1);
            let padding = (aligned - current) as usize;
            if section.sh_flags & SHF_EXECINSTR != 0 {
                // NOP-fill for code sections: AArch64 NOP = 0xD503201F (little-endian)
                let nop: [u8; 4] = [0x1f, 0x20, 0x03, 0xd5];
                let full_nops = padding / 4;
                let remainder = padding % 4;
                for _ in 0..full_nops {
                    section.data.extend_from_slice(&nop);
                }
                // Fill any remaining bytes (shouldn't happen if alignment is >= 4)
                section.data.extend(std::iter::repeat_n(0u8, remainder));
            } else {
                section.data.extend(std::iter::repeat_n(0u8, padding));
            }
            if align > section.sh_addralign {
                section.sh_addralign = align;
            }
        }
    }

    /// Process all parsed assembly statements.
    pub fn process_statements(&mut self, statements: &[AsmStatement]) -> Result<(), String> {
        for stmt in statements {
            self.process_statement(stmt)?;
        }
        // Resolve symbol differences first (needs all labels to be known)
        self.resolve_sym_diffs()?;
        self.resolve_local_branches()?;
        Ok(())
    }

    fn process_statement(&mut self, stmt: &AsmStatement) -> Result<(), String> {
        match stmt {
            AsmStatement::Empty => Ok(()),

            AsmStatement::Label(name) => {
                // Default to .text if no section directive has been seen yet
                if self.current_section.is_empty() {
                    self.ensure_section(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, 4);
                    self.current_section = ".text".to_string();
                }
                // Record label position
                let section = self.current_section.clone();
                let offset = self.current_offset();
                self.labels.insert(name.clone(), (section, offset));
                Ok(())
            }

            AsmStatement::Directive(dir) => {
                self.process_directive(dir)
            }

            AsmStatement::Instruction { mnemonic, operands, raw_operands } => {
                self.process_instruction(mnemonic, operands, raw_operands)
            }
        }
    }

    fn process_directive(&mut self, dir: &AsmDirective) -> Result<(), String> {
        match dir {
            AsmDirective::Section(sec) => {
                self.process_section_directive(sec);
                Ok(())
            }

            AsmDirective::PushSection(sec) => {
                self.section_stack.push(self.current_section.clone());
                self.process_section_directive(sec);
                Ok(())
            }

            AsmDirective::PopSection => {
                // Restore the previous section from the stack
                if let Some(prev) = self.section_stack.pop() {
                    self.current_section = prev;
                }
                // If stack is empty, silently keep current section (matches GNU as behavior)
                Ok(())
            }

            AsmDirective::Global(sym) => {
                self.global_symbols.insert(sym.clone(), true);
                Ok(())
            }

            AsmDirective::Weak(sym) => {
                self.weak_symbols.insert(sym.clone(), true);
                Ok(())
            }

            AsmDirective::Hidden(sym) => {
                self.symbol_visibility.insert(sym.clone(), STV_HIDDEN);
                Ok(())
            }

            AsmDirective::Protected(sym) => {
                self.symbol_visibility.insert(sym.clone(), STV_PROTECTED);
                Ok(())
            }

            AsmDirective::Internal(sym) => {
                self.symbol_visibility.insert(sym.clone(), STV_INTERNAL);
                Ok(())
            }

            AsmDirective::SymbolType(sym, kind) => {
                let st = match kind {
                    SymbolKind::Function => STT_FUNC,
                    SymbolKind::Object => STT_OBJECT,
                    SymbolKind::TlsObject => STT_TLS,
                    SymbolKind::NoType => STT_NOTYPE,
                };
                self.symbol_types.insert(sym.clone(), st);
                Ok(())
            }

            AsmDirective::Size(sym, expr) => {
                match expr {
                    SizeExpr::CurrentMinusSymbol(label) => {
                        if let Some((section, label_offset)) = self.labels.get(label) {
                            if *section == self.current_section {
                                let current = self.current_offset();
                                let size = current - label_offset;
                                self.symbol_sizes.insert(sym.clone(), size);
                            }
                        }
                    }
                    SizeExpr::Constant(size) => {
                        self.symbol_sizes.insert(sym.clone(), *size);
                    }
                }
                Ok(())
            }

            AsmDirective::Align(bytes) => {
                self.align_to(*bytes);
                Ok(())
            }

            AsmDirective::Balign(bytes) => {
                self.align_to(*bytes);
                Ok(())
            }

            AsmDirective::Byte(vals) => {
                self.emit_bytes(vals);
                Ok(())
            }

            AsmDirective::Short(vals) => {
                for val in vals {
                    self.emit_bytes(&(*val as u16).to_le_bytes());
                }
                Ok(())
            }

            AsmDirective::Long(vals) => {
                self.emit_data_values(vals, 4)
            }

            AsmDirective::Quad(vals) => {
                self.emit_data_values(vals, 8)
            }

            AsmDirective::Zero(size, fill) => {
                self.emit_bytes(&vec![*fill; *size]);
                Ok(())
            }

            AsmDirective::Asciz(bytes) => {
                // Already includes null terminator from parser
                self.emit_bytes(bytes);
                Ok(())
            }

            AsmDirective::Ascii(bytes) => {
                self.emit_bytes(bytes);
                Ok(())
            }

            AsmDirective::Comm(sym, size, align) => {
                self.symbols.push(ElfSymbol {
                    name: sym.clone(),
                    value: *align, // For COMMON, value = alignment
                    size: *size,
                    binding: STB_GLOBAL,
                    sym_type: STT_OBJECT,
                    visibility: STV_DEFAULT,
                    section_name: "*COM*".to_string(),
                });
                Ok(())
            }

            AsmDirective::Local(_) => {
                // Symbols are local by default
                Ok(())
            }

            AsmDirective::Set(alias, target) => {
                self.aliases.insert(alias.clone(), target.clone());
                Ok(())
            }

            AsmDirective::Cfi | AsmDirective::Ignored => Ok(()),
        }
    }

    /// Process a section directive with already-parsed fields.
    fn process_section_directive(&mut self, sec: &SectionDirective) {
        let sec_name = &sec.name;

        // Determine section type from parsed type string
        let sh_type = match sec.section_type.as_deref() {
            Some("@nobits") => SHT_NOBITS,
            Some("@note") => SHT_NOTE,
            _ => {
                // Default type based on section name
                if sec_name == ".bss" || sec_name.starts_with(".bss.") || sec_name.starts_with(".tbss") {
                    SHT_NOBITS
                } else {
                    SHT_PROGBITS
                }
            }
        };

        // Determine section flags from parsed flags string
        let mut sh_flags = 0u64;
        if let Some(flags) = &sec.flags {
            if flags.contains('a') { sh_flags |= SHF_ALLOC; }
            if flags.contains('w') { sh_flags |= SHF_WRITE; }
            if flags.contains('x') { sh_flags |= SHF_EXECINSTR; }
            if flags.contains('M') { sh_flags |= SHF_MERGE; }
            if flags.contains('S') { sh_flags |= SHF_STRINGS; }
            if flags.contains('T') { sh_flags |= SHF_TLS; }
            if flags.contains('G') { sh_flags |= SHF_GROUP; }
        }

        // Set default flags based on section name if none specified
        if sh_flags == 0 {
            sh_flags = default_section_flags(sec_name);
        }

        let align = if sec_name == ".text" { 4 } else { 1 };
        self.ensure_section(sec_name, sh_type, sh_flags, align);
        self.current_section = sec_name.clone();
    }

    /// Emit typed data values (Long or Quad) with proper relocations.
    fn emit_data_values(&mut self, vals: &[DataValue], size: usize) -> Result<(), String> {
        for val in vals {
            match val {
                DataValue::Integer(v) => {
                    if size == 4 {
                        self.emit_bytes(&(*v as u32).to_le_bytes());
                    } else {
                        self.emit_bytes(&(*v as u64).to_le_bytes());
                    }
                }
                DataValue::Symbol(sym) => {
                    let reloc_type = if size == 4 {
                        RelocType::Abs32.elf_type()
                    } else {
                        RelocType::Abs64.elf_type()
                    };
                    self.add_reloc(reloc_type, sym.clone(), 0);
                    if size == 4 {
                        self.emit_bytes(&0u32.to_le_bytes());
                    } else {
                        self.emit_bytes(&0u64.to_le_bytes());
                    }
                }
                DataValue::SymbolOffset(sym, addend) => {
                    let reloc_type = if size == 4 {
                        RelocType::Abs32.elf_type()
                    } else {
                        RelocType::Abs64.elf_type()
                    };
                    self.add_reloc(reloc_type, sym.clone(), *addend);
                    if size == 4 {
                        self.emit_bytes(&0u32.to_le_bytes());
                    } else {
                        self.emit_bytes(&0u64.to_le_bytes());
                    }
                }
                DataValue::SymbolDiff(sym_a, sym_b) => {
                    self.record_sym_diff(sym_a, sym_b, 0, size);
                }
                DataValue::SymbolDiffAddend(sym_a, sym_b, addend) => {
                    self.record_sym_diff(sym_a, sym_b, *addend, size);
                }
            }
        }
        Ok(())
    }

    /// Record a pending symbol difference for deferred resolution.
    fn record_sym_diff(&mut self, sym_a: &str, sym_b: &str, extra_addend: i64, size: usize) {
        let section = self.current_section.clone();
        let offset = self.current_offset();
        self.pending_sym_diffs.push(PendingSymDiff {
            section,
            offset,
            sym_a: sym_a.to_string(),
            sym_b: sym_b.to_string(),
            extra_addend,
            size,
        });
        // Emit placeholder bytes
        if size == 4 {
            self.emit_bytes(&0u32.to_le_bytes());
        } else {
            self.emit_bytes(&0u64.to_le_bytes());
        }
    }

    fn process_instruction(&mut self, mnemonic: &str, operands: &[Operand], raw_operands: &str) -> Result<(), String> {
        // Make sure we're in a text section
        if self.current_section.is_empty() {
            self.ensure_section(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, 4);
            self.current_section = ".text".to_string();
        }

        match encode_instruction(mnemonic, operands, raw_operands) {
            Ok(EncodeResult::Word(word)) => {
                self.emit_u32_le(word);
                Ok(())
            }
            Ok(EncodeResult::WordWithReloc { word, reloc }) => {
                // Check if it's a local label reference we can resolve
                let is_local = reloc.symbol.starts_with(".L") || reloc.symbol.starts_with(".l");

                if is_local {
                    // Store pending reloc - will be resolved after all labels are known
                    let offset = self.current_offset();
                    self.pending_branch_relocs.push(PendingReloc {
                        section: self.current_section.clone(),
                        offset,
                        reloc_type: reloc.reloc_type.elf_type(),
                        symbol: reloc.symbol.clone(),
                        addend: reloc.addend,
                    });
                    self.emit_u32_le(word);
                } else {
                    self.add_reloc(reloc.reloc_type.elf_type(), reloc.symbol, reloc.addend);
                    self.emit_u32_le(word);
                }
                Ok(())
            }
            Ok(EncodeResult::Words(words)) => {
                for word in words {
                    self.emit_u32_le(word);
                }
                Ok(())
            }
            Ok(EncodeResult::Skip) => Ok(()),
            Err(e) => {
                Err(e)
            }
        }
    }

    /// Resolve local branch labels to PC-relative offsets.
    fn resolve_local_branches(&mut self) -> Result<(), String> {
        for reloc in &self.pending_branch_relocs {
            let (target_section, target_offset) = self.labels.get(&reloc.symbol)
                .ok_or_else(|| format!("undefined local label: {}", reloc.symbol))?
                .clone();

            if target_section != reloc.section {
                // Cross-section reference - convert local label to section symbol + offset
                // The linker doesn't know about local labels, so reference the section symbol
                // with addend = label offset within its section
                if let Some(section) = self.sections.get_mut(&reloc.section) {
                    section.relocs.push(ElfReloc {
                        offset: reloc.offset,
                        reloc_type: reloc.reloc_type,
                        symbol_name: target_section.clone(),
                        addend: target_offset as i64 + reloc.addend,
                    });
                }
                continue;
            }

            let pc_offset = (target_offset as i64) - (reloc.offset as i64) + reloc.addend;

            if let Some(section) = self.sections.get_mut(&reloc.section) {
                let instr_offset = reloc.offset as usize;
                if instr_offset + 4 > section.data.len() {
                    continue;
                }

                let mut word = u32::from_le_bytes([
                    section.data[instr_offset],
                    section.data[instr_offset + 1],
                    section.data[instr_offset + 2],
                    section.data[instr_offset + 3],
                ]);

                match reloc.reloc_type {
                    282 | 283 => {
                        // R_AARCH64_JUMP26 / R_AARCH64_CALL26
                        let imm26 = ((pc_offset >> 2) as u32) & 0x3FFFFFF;
                        word |= imm26;
                    }
                    280 => {
                        // R_AARCH64_CONDBR19
                        let imm19 = ((pc_offset >> 2) as u32) & 0x7FFFF;
                        word |= imm19 << 5;
                    }
                    279 => {
                        // R_AARCH64_TSTBR14
                        let imm14 = ((pc_offset >> 2) as u32) & 0x3FFF;
                        word |= imm14 << 5;
                    }
                    274 => {
                        // R_AARCH64_ADR_PREL_LO21 - ADR instruction
                        // ADR: op immlo[1:0] 10000 immhi[18:0] Rd
                        // immlo is bits [30:29], immhi is bits [23:5]
                        let imm = pc_offset as i32;
                        let immlo = (imm as u32) & 0x3;
                        let immhi = ((imm as u32) >> 2) & 0x7FFFF;
                        word |= (immlo << 29) | (immhi << 5);
                    }
                    275 => {
                        // R_AARCH64_ADR_PREL_PG_HI21 - ADRP instruction (local resolution)
                        // For same-section local labels, compute page-relative offset
                        let pc_page = (reloc.offset as i64) & !0xFFF;
                        let target_page = (target_offset as i64) & !0xFFF;
                        let page_off = target_page - pc_page;
                        let imm = (page_off >> 12) as i32;
                        let immlo = (imm as u32) & 0x3;
                        let immhi = ((imm as u32) >> 2) & 0x7FFFF;
                        word |= (immlo << 29) | (immhi << 5);
                    }
                    _ => {
                        // Unknown reloc type for local branch - leave as external
                        section.relocs.push(ElfReloc {
                            offset: reloc.offset,
                            reloc_type: reloc.reloc_type,
                            symbol_name: reloc.symbol.clone(),
                            addend: reloc.addend,
                        });
                        continue;
                    }
                }

                section.data[instr_offset..instr_offset + 4].copy_from_slice(&word.to_le_bytes());
            }
        }
        Ok(())
    }

    /// Resolve local label references in data relocations.
    ///
    /// When a data directive like `.xword .Lstr0` references a local label
    /// in a different section, the local label won't be in the symbol table.
    /// We need to convert these to section_symbol + offset_of_label_in_section,
    /// just like GCC's assembler does.
    fn resolve_local_data_relocs(&mut self) {
        let labels = &self.labels;
        for sec_name in &self.section_order.clone() {
            if let Some(section) = self.sections.get_mut(sec_name) {
                for reloc in &mut section.relocs {
                    // Check if this references a local label (starts with .L or .l)
                    if (reloc.symbol_name.starts_with(".L") || reloc.symbol_name.starts_with(".l"))
                        && !reloc.symbol_name.is_empty()
                    {
                        if let Some((label_section, label_offset)) = labels.get(&reloc.symbol_name) {
                            // Convert to section symbol + addend
                            reloc.addend += *label_offset as i64;
                            reloc.symbol_name = label_section.clone();
                        }
                    }
                }
            }
        }
    }

    /// Write the final ELF object file.
    ///
    /// Uses shared `build_elf_symbol_table` for symbol table construction
    /// and `write_relocatable_object` for ELF serialization.
    pub fn write_elf(&mut self, output_path: &str) -> Result<(), String> {
        // Resolve local label references in data relocations before building symbol table
        self.resolve_local_data_relocs();

        // Convert internal sections to shared ObjSection format
        let shared_sections = elf::convert_sections_to_shared(
            &self.section_order,
            &self.sections,
            |section| elf::ObjSection {
                name: section.name.clone(),
                sh_type: section.sh_type,
                sh_flags: section.sh_flags,
                data: section.data.clone(),
                sh_addralign: section.sh_addralign,
                relocs: section.relocs.iter().map(|r| elf::ObjReloc {
                    offset: r.offset,
                    reloc_type: r.reloc_type,
                    symbol_name: r.symbol_name.clone(),
                    addend: r.addend,
                }).collect(),
            },
        );

        // Add COMMON symbols directly
        let mut extra_symbols: Vec<elf::ObjSymbol> = self.symbols.iter().map(|sym| elf::ObjSymbol {
            name: sym.name.clone(),
            value: sym.value,
            size: sym.size,
            binding: sym.binding,
            sym_type: sym.sym_type,
            visibility: sym.visibility,
            section_name: sym.section_name.clone(),
        }).collect();

        let symtab_input = SymbolTableInput {
            labels: &self.labels,
            global_symbols: &self.global_symbols,
            weak_symbols: &self.weak_symbols,
            symbol_types: &self.symbol_types,
            symbol_sizes: &self.symbol_sizes,
            symbol_visibility: &self.symbol_visibility,
            aliases: &self.aliases,
            sections: &shared_sections,
            include_referenced_locals: false,
        };

        let config = elf::ElfConfig {
            e_machine: EM_AARCH64,
            e_flags: 0,
            elf_class: ELFCLASS64,
        };

        let mut symbols = elf::build_elf_symbol_table(&symtab_input);
        symbols.append(&mut extra_symbols);

        let elf_bytes = elf::write_relocatable_object(
            &config,
            &self.section_order,
            &shared_sections,
            &symbols,
        )?;

        std::fs::write(output_path, &elf_bytes)
            .map_err(|e| format!("failed to write ELF file: {}", e))?;

        Ok(())
    }

}

use elf::default_section_flags;

