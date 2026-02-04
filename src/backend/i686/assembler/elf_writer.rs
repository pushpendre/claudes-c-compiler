//! 32-bit ELF relocatable object file writer for i686.
//!
//! Produces .o files from parsed and encoded assembly. Uses ELFCLASS32,
//! EM_386, and Elf32 structures. Uses REL (not RELA) relocation format.

use std::collections::HashMap;
use crate::backend::x86::assembler::parser::*;
use super::encoder::*;
use crate::backend::elf::{self as elf_mod,
    SHT_PROGBITS,
    STB_LOCAL, STB_GLOBAL, STB_WEAK,
    STT_NOTYPE, STT_OBJECT, STT_FUNC, STT_TLS,
    STV_DEFAULT, STV_INTERNAL, STV_HIDDEN, STV_PROTECTED,
    ELFCLASS32, EM_386,
    resolve_numeric_labels, parse_section_flags,
    ElfConfig, ObjSection, ObjSymbol, ObjReloc, SymbolTableInput,
};

// i686 uses u32 section flags (ELF32) for local section structs.
// Only the flags used directly in codegen are defined here;
// parse_section_flags in backend::elf handles the full set.
const SHF_ALLOC: u32 = 0x2;
const SHF_EXECINSTR: u32 = 0x4;

/// Tracks a jump instruction for relaxation.
#[derive(Clone, Debug)]
struct JumpInfo {
    offset: usize,
    len: usize,
    target: String,
    is_conditional: bool,
    relaxed: bool,
}

/// Tracks an alignment or .org marker within a section.
/// Used to re-pad after jump relaxation shrinks instructions.
#[derive(Clone, Debug)]
struct AlignMarker {
    /// Offset in the section data where padding starts.
    offset: usize,
    /// Number of padding bytes that were originally inserted.
    padding: usize,
    /// The kind of alignment: Align(n) for .balign n, or Org for a label-relative position.
    kind: AlignMarkerKind,
}

#[derive(Clone, Debug)]
enum AlignMarkerKind {
    /// .balign N — pad to N-byte boundary
    Align(u32),
    /// .org label + offset — advance to a fixed position.
    /// Stores the label name and the constant offset so the target can be
    /// recomputed after relaxation shifts labels.
    Org { label: String, addend: i64 },
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
    /// Alignment and .org markers for post-relaxation fixup
    align_markers: Vec<AlignMarker>,
    /// COMDAT group name, if this section is part of a COMDAT group.
    comdat_group: Option<String>,
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

// Numeric label resolution and section flags parsing functions are shared
// via crate::backend::elf (resolve_numeric_labels, parse_section_flags, etc.)

/// Builds a 32-bit ELF relocatable object file from parsed assembly items.
pub struct ElfWriter {
    sections: Vec<Section>,
    symbols: Vec<SymbolInfo>,
    section_map: HashMap<String, usize>,
    symbol_map: HashMap<String, usize>,
    current_section: Option<usize>,
    label_positions: HashMap<String, (usize, u32)>,
    /// Tracks positions of numeric labels (1:, 2:, etc.) for runtime resolution
    /// during jump relaxation and relocation processing.
    numeric_label_positions: HashMap<String, Vec<(usize, u32)>>,
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
            code16gcc: false,
        }
    }

    /// Build the ELF object file from parsed assembly items.
    pub fn build(mut self, items: &[AsmItem]) -> Result<Vec<u8>, String> {
        // Resolve numeric local labels (1:, 2:, etc.) to unique names
        let items = resolve_numeric_labels(items);

        for item in &items {
            self.process_item(item)?;
        }
        self.emit_elf()
    }

    fn get_or_create_section(&mut self, name: &str, section_type: u32, flags: u32, comdat_group: Option<String>) -> usize {
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
            align_markers: Vec::new(),
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
        let (section_type, flags64) = parse_section_flags(&dir.name, dir.flags.as_deref(), dir.section_type.as_deref());
        let idx = self.get_or_create_section(&dir.name, section_type, flags64 as u32, dir.comdat_group.clone());
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

                // Track numeric label positions for 1b/1f resolution.
                // The pre-pass (resolve_numeric_labels) rewrites most references, but
                // this runtime tracking provides defense-in-depth for jump relaxation
                // and relocation resolution where offsets may shift.
                if name.chars().all(|c| c.is_ascii_digit()) {
                    self.numeric_label_positions
                        .entry(name.clone())
                        .or_default()
                        .push((sec_idx, offset));
                }

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
                    // Record alignment marker for post-relaxation fixup
                    if padding > 0 && align > 1 {
                        section.align_markers.push(AlignMarker {
                            offset: current as usize,
                            padding,
                            kind: AlignMarkerKind::Align(align),
                        });
                    }
                    if section.flags & SHF_EXECINSTR != 0 {
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
                // 64-bit data values (used rarely on i686, but needed for debug info etc.)
                self.emit_data_values_64(vals)?;
            }
            AsmItem::Zero(n) => {
                let section = self.current_section_mut()?;
                section.data.extend(std::iter::repeat_n(0u8, *n as usize));
            }
            AsmItem::Org(sym, offset) => {
                if let Some(sec_idx) = self.current_section {
                    let target = if sym.is_empty() {
                        *offset as u32
                    } else if let Some(&(label_sec, label_off)) = self.label_positions.get(sym.as_str()) {
                        if label_sec == sec_idx {
                            (label_off as i64 + *offset) as u32
                        } else {
                            return Err(format!(".org symbol {} not in current section", sym));
                        }
                    } else {
                        return Err(format!(".org: unknown symbol {}", sym));
                    };
                    let current = self.sections[sec_idx].data.len() as u32;
                    // Record .org marker for post-relaxation fixup.
                    // Store the label + addend so the target can be recomputed
                    // after jump relaxation shifts label positions.
                    if target > current {
                        self.sections[sec_idx].align_markers.push(AlignMarker {
                            offset: current as usize,
                            padding: (target - current) as usize,
                            kind: AlignMarkerKind::Org {
                                label: sym.clone(),
                                addend: *offset,
                            },
                        });
                        let padding = (target - current) as usize;
                        let fill = if self.sections[sec_idx].flags & SHF_EXECINSTR != 0 { 0x90u8 } else { 0u8 };
                        self.sections[sec_idx].data.extend(std::iter::repeat_n(fill, padding));
                    }
                }
            }
            AsmItem::SkipExpr(expr, fill) => {
                // i686 doesn't need deferred skip expression support (kernel uses x86-64)
                // Try simple integer parse; error otherwise
                if let Ok(val) = expr.trim().parse::<u32>() {
                    let section = self.current_section_mut()?;
                    section.data.extend(std::iter::repeat_n(*fill, val as usize));
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
            let idx = self.get_or_create_section(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, None);
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
                    if size == 1 {
                        section.data.push(*v as u8);
                    } else if size == 2 {
                        section.data.extend_from_slice(&(*v as i16).to_le_bytes());
                    } else {
                        section.data.extend_from_slice(&(*v as i32).to_le_bytes());
                    }
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

        // Resolve internal relocations
        self.resolve_internal_relocations();

        // Convert to shared format and delegate to shared writer.
        // For i686 REL format, addends must be patched into section data
        // since REL entries don't carry an explicit addend field.
        let section_names: Vec<String> = self.sections.iter().map(|s| s.name.clone()).collect();

        let mut shared_sections: HashMap<String, ObjSection> = HashMap::new();
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
                comdat_group: sec.comdat_group.clone(),
            });
        }

        // Convert label positions from (section_index, offset) to (section_name, offset)
        // for the shared symbol table builder.
        let labels: HashMap<String, (String, u64)> = self.label_positions.iter()
            .map(|(name, &(sec_idx, offset))| {
                (name.clone(), (section_names[sec_idx].clone(), offset as u64))
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
                            end - start_off as u64
                        } else {
                            0
                        }
                    }
                    SizeExpr::SymbolDiff(end_label, start_label) => {
                        let end_off = self.label_positions.get(end_label).map(|p| p.1 as u64).unwrap_or(0);
                        let start_off = self.label_positions.get(start_label).map(|p| p.1 as u64).unwrap_or(0);
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
                    size: sym.size as u64,
                    binding: sym.binding,
                    sym_type: sym.sym_type,
                    visibility: sym.visibility,
                    section_name: "*COM*".to_string(),
                });
            }
        }

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

    /// Resolve a numeric label reference like "1b" or "1f".
    /// Returns the (section_index, offset) of the target label.
    fn resolve_numeric_label(&self, symbol: &str, reloc_offset: u32, sec_idx: usize) -> Option<(usize, u32)> {
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
            let mut best: Option<(usize, u32)> = None;
            for &(s_idx, off) in positions {
                if s_idx == sec_idx && off <= reloc_offset
                    && (best.is_none() || off > best.unwrap().1)
                {
                    best = Some((s_idx, off));
                }
            }
            best
        } else {
            // Forward: find the nearest label AFTER reloc_offset in the same section
            let mut best: Option<(usize, u32)> = None;
            for &(s_idx, off) in positions {
                if s_idx == sec_idx && off > reloc_offset
                    && (best.is_none() || off < best.unwrap().1)
                {
                    best = Some((s_idx, off));
                }
            }
            best
        }
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
                    let target_off_opt = local_labels.get(&jump.target).copied()
                        .or_else(|| {
                            // Try numeric label resolution
                            self.resolve_numeric_label(&jump.target, jump.offset as u32, sec_idx)
                                .map(|(_, off)| off as usize)
                        });
                    if let Some(target_off) = target_off_opt {
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

                    // Also update numeric label positions
                    for (_, positions) in self.numeric_label_positions.iter_mut() {
                        for pos in positions.iter_mut() {
                            if pos.0 == sec_idx && (pos.1 as usize) > offset {
                                pos.1 -= shrink as u32;
                            }
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

                    // Update alignment marker offsets
                    for marker in self.sections[sec_idx].align_markers.iter_mut() {
                        if marker.offset > offset {
                            marker.offset -= shrink;
                        }
                    }

                    self.sections[sec_idx].jumps[j_idx].relaxed = true;
                    self.sections[sec_idx].jumps[j_idx].len = new_len;
                    any_relaxed = true;
                }

                if !any_relaxed { break; }
            }

            // Post-relaxation fixup: re-pad alignment markers that were broken
            // by jump relaxation. When a jump shrinks, the nop padding before
            // a subsequent .balign/.org is now too small. We scan alignment
            // markers and insert additional nops where needed.
            self.fixup_alignment_markers(sec_idx);

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
                    let target = local_labels.get(&jump.target).copied()
                        .or_else(|| {
                            self.resolve_numeric_label(&jump.target, jump.offset as u32, sec_idx)
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

    /// Re-pad alignment markers that were broken by jump relaxation.
    ///
    /// When a jump instruction is relaxed (shrunk from 5 to 2 bytes), the
    /// subsequent nop padding bytes are not adjusted. This function scans
    /// all alignment markers in a section and either inserts additional nops
    /// (when padding is too small) or removes excess nops (when padding is
    /// too large, e.g. a .balign overshot a subsequent .org target).
    fn fixup_alignment_markers(&mut self, sec_idx: usize) {
        if self.sections[sec_idx].align_markers.is_empty() {
            return;
        }

        let is_exec = self.sections[sec_idx].flags & SHF_EXECINSTR != 0;
        let fill_byte = if is_exec { 0x90u8 } else { 0u8 };

        let mut marker_idx = 0;
        loop {
            if marker_idx >= self.sections[sec_idx].align_markers.len() {
                break;
            }
            let current_offset = self.sections[sec_idx].align_markers[marker_idx].offset;
            let kind = self.sections[sec_idx].align_markers[marker_idx].kind.clone();

            let needed_end = match &kind {
                AlignMarkerKind::Align(align) => {
                    let a = *align as usize;
                    if a <= 1 { marker_idx += 1; continue; }
                    (current_offset + a - 1) & !(a - 1)
                }
                AlignMarkerKind::Org { label, addend } => {
                    if label.is_empty() {
                        *addend as usize
                    } else if let Some(&(l_sec, l_off)) = self.label_positions.get(label.as_str()) {
                        if l_sec == sec_idx {
                            (l_off as i64 + *addend) as usize
                        } else {
                            marker_idx += 1; continue;
                        }
                    } else {
                        marker_idx += 1; continue;
                    }
                }
            };

            let needed_padding = if needed_end > current_offset {
                needed_end - current_offset
            } else {
                0
            };

            // Use the stored padding size rather than counting consecutive fill bytes.
            // Counting 0x90 bytes greedily is wrong because subsequent code may also
            // contain 0x90 (nop) bytes (e.g., trampoline code tables with nop padding
            // between entries). The stored `padding` field is the exact count of fill
            // bytes originally emitted at this marker location.
            let existing_padding = self.sections[sec_idx].align_markers[marker_idx].padding;

            if needed_padding > existing_padding {
                // Need MORE padding: insert additional fill bytes
                let insert_at = current_offset + existing_padding;
                let extra = needed_padding - existing_padding;
                let insert_bytes = vec![fill_byte; extra];
                self.sections[sec_idx].data.splice(insert_at..insert_at, insert_bytes);
                self.shift_after(sec_idx, insert_at, extra as i64, marker_idx);
            } else if needed_padding < existing_padding {
                // Need LESS padding: remove excess fill bytes
                let remove_count = existing_padding - needed_padding;
                let remove_start = current_offset + needed_padding;
                let remove_end = remove_start + remove_count;
                self.sections[sec_idx].data.drain(remove_start..remove_end);
                self.shift_after(sec_idx, remove_start, -(remove_count as i64), marker_idx);
            }

            marker_idx += 1;
        }
    }

    /// Shift all labels, relocations, jumps, and subsequent alignment markers
    /// in a section after an insertion or removal at `at_offset`.
    /// `delta` is positive for insertions and negative for removals.
    fn shift_after(&mut self, sec_idx: usize, at_offset: usize, delta: i64, current_marker_idx: usize) {
        if delta == 0 { return; }
        for (_, pos) in self.label_positions.iter_mut() {
            if pos.0 == sec_idx && (pos.1 as usize) >= at_offset {
                pos.1 = (pos.1 as i64 + delta) as u32;
            }
        }
        for (_, positions) in self.numeric_label_positions.iter_mut() {
            for pos in positions.iter_mut() {
                if pos.0 == sec_idx && (pos.1 as usize) >= at_offset {
                    pos.1 = (pos.1 as i64 + delta) as u32;
                }
            }
        }
        for reloc in self.sections[sec_idx].relocations.iter_mut() {
            if (reloc.offset as usize) >= at_offset {
                reloc.offset = (reloc.offset as i64 + delta) as u32;
            }
        }
        for jump in self.sections[sec_idx].jumps.iter_mut() {
            if jump.offset >= at_offset {
                jump.offset = (jump.offset as i64 + delta) as usize;
            }
        }
        for i in (current_marker_idx + 1)..self.sections[sec_idx].align_markers.len() {
            if self.sections[sec_idx].align_markers[i].offset >= at_offset {
                self.sections[sec_idx].align_markers[i].offset =
                    (self.sections[sec_idx].align_markers[i].offset as i64 + delta) as usize;
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

    /// Resolve relocations that reference internal labels (same-section).
    /// Only resolves relocations for local symbols; global/weak symbols
    /// keep their relocations so the linker can handle interposition/PLT.
    fn resolve_internal_relocations(&mut self) {
        for sec_idx in 0..self.sections.len() {
            let mut resolved = Vec::new();
            let mut unresolved = Vec::new();

            for reloc in &self.sections[sec_idx].relocations {
                // Try regular label lookup first, then numeric label resolution (1b, 1f, etc.)
                let label_pos = self.label_positions.get(&reloc.symbol).copied()
                    .or_else(|| self.resolve_numeric_label(&reloc.symbol, reloc.offset, sec_idx));
                if let Some((target_sec, target_off)) = label_pos {
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


// parse_section_flags is now in crate::backend::elf and imported above.
