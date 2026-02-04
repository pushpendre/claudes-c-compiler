//! ELF object file writer for RISC-V.
//!
//! Takes parsed assembly statements and produces an ELF .o (relocatable) file
//! with proper sections, symbols, and relocations for RISC-V 64-bit ELF.

#![allow(dead_code)]

use std::collections::{HashMap, HashSet};
use super::parser::{AsmStatement, Operand, Directive, DataValue, SymbolType, Visibility, SizeExpr};
use super::encoder::{encode_instruction, encode_insn_directive, EncodeResult, RelocType};
use super::compress;
use crate::backend::elf::{self,
    SHT_PROGBITS, SHT_NOBITS, SHT_NOTE,
    SHF_WRITE, SHF_ALLOC, SHF_EXECINSTR, SHF_MERGE, SHF_STRINGS,
    SHF_TLS, SHF_GROUP,
    STB_GLOBAL,
    STT_NOTYPE, STT_OBJECT, STT_FUNC, STT_TLS,
    STV_DEFAULT, STV_HIDDEN, STV_PROTECTED, STV_INTERNAL,
    ELFCLASS64, EM_RISCV,
    SymbolTableInput,
};
use elf::default_section_flags;

// ELF flags for RISC-V
const EF_RISCV_RVC: u32 = 0x1;
const EF_RISCV_FLOAT_ABI_DOUBLE: u32 = 0x4;

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
    /// Counter for generating synthetic pcrel_hi labels
    pcrel_hi_counter: u32,
    /// GNU numeric labels: e.g., "1" -> [(section, offset), ...] in definition order.
    /// Used to resolve `1b` (backward ref) and `1f` (forward ref) references.
    numeric_labels: HashMap<String, Vec<(String, u64)>>,
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
    /// For pcrel_lo12 relocations resolved locally: the offset of the
    /// corresponding auipc (pcrel_hi) instruction. The lo12 value is
    /// computed relative to the auipc's PC, not the pcrel_lo instruction's PC.
    pcrel_hi_offset: Option<u64>,
}

/// Check if a label name is a GNU numeric label (e.g., "1", "42").
fn is_numeric_label(name: &str) -> bool {
    !name.is_empty() && name.chars().all(|c| c.is_ascii_digit())
}

/// Check if a symbol reference is a GNU numeric label reference (e.g., "1b", "1f", "42b").
/// Returns Some((label_name, is_backward)) if it is, None otherwise.
fn parse_numeric_label_ref(symbol: &str) -> Option<(&str, bool)> {
    if symbol.len() < 2 {
        return None;
    }
    let last_char = symbol.as_bytes()[symbol.len() - 1];
    let is_backward = last_char == b'b' || last_char == b'B';
    let is_forward = last_char == b'f' || last_char == b'F';
    if !is_backward && !is_forward {
        return None;
    }
    let label_part = &symbol[..symbol.len() - 1];
    if label_part.is_empty() || !label_part.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    Some((label_part, is_backward))
}

/// Pre-process assembly statements to resolve GNU numeric label references.
/// Numeric labels like `1:` can be defined multiple times. References like `1b`
/// (backward) and `1f` (forward) must resolve to the nearest matching definition.
/// This function renames all numeric labels to unique `.Lnum_N_K` names and
/// rewrites all `Nb`/`Nf` references in instruction operands to the resolved name.
fn resolve_numeric_label_refs(statements: &[AsmStatement]) -> Vec<AsmStatement> {
    use super::parser::Operand;

    // First pass: collect all numeric label definition positions (by statement index).
    // Map: label_name -> [(stmt_index, instance_id), ...]
    let mut label_defs: HashMap<String, Vec<(usize, usize)>> = HashMap::new();
    let mut instance_counter: HashMap<String, usize> = HashMap::new();

    for (i, stmt) in statements.iter().enumerate() {
        if let AsmStatement::Label(name) = stmt {
            if is_numeric_label(name) {
                let instance = instance_counter.entry(name.clone()).or_insert(0);
                label_defs.entry(name.clone()).or_default().push((i, *instance));
                *instance += 1;
            }
        }
    }

    // If no numeric labels exist, return a clone of the original statements
    if label_defs.is_empty() {
        return statements.to_vec();
    }

    // Second pass: rewrite labels and references
    let mut result = Vec::with_capacity(statements.len());

    for (i, stmt) in statements.iter().enumerate() {
        match stmt {
            AsmStatement::Label(name) if is_numeric_label(name) => {
                // Find this definition's instance id
                if let Some(defs) = label_defs.get(name) {
                    for &(def_idx, inst_id) in defs {
                        if def_idx == i {
                            let new_name = format!(".Lnum_{}_{}", name, inst_id);
                            result.push(AsmStatement::Label(new_name));
                            break;
                        }
                    }
                } else {
                    result.push(stmt.clone());
                }
            }
            AsmStatement::Instruction { mnemonic, operands, raw_operands } => {
                let new_operands: Vec<Operand> = operands.iter().map(|op| {
                    rewrite_numeric_ref_in_operand(op, i, &label_defs)
                }).collect();
                result.push(AsmStatement::Instruction {
                    mnemonic: mnemonic.clone(),
                    operands: new_operands,
                    raw_operands: raw_operands.clone(),
                });
            }
            _ => result.push(stmt.clone()),
        }
    }

    result
}

/// Rewrite a numeric label reference in an operand to a synthetic label name.
fn rewrite_numeric_ref_in_operand(
    op: &super::parser::Operand,
    stmt_idx: usize,
    label_defs: &HashMap<String, Vec<(usize, usize)>>,
) -> super::parser::Operand {
    use super::parser::Operand;

    match op {
        Operand::Symbol(s) => {
            if let Some(new_name) = resolve_numeric_ref_name(s, stmt_idx, label_defs) {
                Operand::Symbol(new_name)
            } else {
                op.clone()
            }
        }
        Operand::Label(s) => {
            if let Some(new_name) = resolve_numeric_ref_name(s, stmt_idx, label_defs) {
                Operand::Label(new_name)
            } else {
                op.clone()
            }
        }
        Operand::SymbolOffset(s, off) => {
            if let Some(new_name) = resolve_numeric_ref_name(s, stmt_idx, label_defs) {
                Operand::SymbolOffset(new_name, *off)
            } else {
                op.clone()
            }
        }
        _ => op.clone(),
    }
}

/// Resolve a numeric label reference like "1b" or "2f" to a synthetic label name.
fn resolve_numeric_ref_name(
    symbol: &str,
    stmt_idx: usize,
    label_defs: &HashMap<String, Vec<(usize, usize)>>,
) -> Option<String> {
    let (label_name, is_backward) = parse_numeric_label_ref(symbol)?;
    let defs = label_defs.get(label_name)?;

    if is_backward {
        // Find the last definition at or before stmt_idx
        let mut best: Option<usize> = None;
        for &(def_idx, inst_id) in defs {
            if def_idx < stmt_idx {
                best = Some(inst_id);
            }
        }
        best.map(|inst_id| format!(".Lnum_{}_{}", label_name, inst_id))
    } else {
        // Find the first definition after stmt_idx
        for &(def_idx, inst_id) in defs {
            if def_idx > stmt_idx {
                return Some(format!(".Lnum_{}_{}", label_name, inst_id));
            }
        }
        None
    }
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
            global_symbols: HashMap::new(),
            weak_symbols: HashMap::new(),
            symbol_types: HashMap::new(),
            symbol_sizes: HashMap::new(),
            symbol_visibility: HashMap::new(),
            pcrel_hi_counter: 0,
            numeric_labels: HashMap::new(),
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

    fn emit_u16_le(&mut self, val: u16) {
        self.emit_bytes(&val.to_le_bytes());
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

    // NOTE: R_RISCV_RELAX relocations are intentionally not emitted.
    // Our assembler resolves local branches by patching immediates directly
    // (in resolve_local_branches). If R_RISCV_RELAX were emitted, the linker
    // could perform relaxation (e.g., shortening auipc+jalr to jal), which
    // changes code layout and invalidates our locally-resolved branch offsets.
    // To properly support RELAX, we would need to keep all references as
    // relocations and let the linker resolve everything.

    fn align_to(&mut self, align: u64) {
        if align <= 1 {
            return;
        }
        if let Some(section) = self.sections.get_mut(&self.current_section) {
            let current = section.data.len() as u64;
            let aligned = (current + align - 1) & !(align - 1);
            let padding = (aligned - current) as usize;
            // For text sections, pad with NOP instructions (0x00000013 = addi x0, x0, 0)
            if section.sh_flags & SHF_EXECINSTR != 0 && align >= 4 {
                let nop = 0x00000013u32;
                let nop_count = padding / 4;
                let remainder = padding % 4;
                for _ in 0..nop_count {
                    section.data.extend_from_slice(&nop.to_le_bytes());
                }
                for _ in 0..remainder {
                    section.data.push(0);
                }
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
        // Pre-process: resolve GNU numeric label references (e.g., "1b", "2f") by
        // renaming numeric labels to unique synthetic names like ".Lnum_1_0".
        let statements = resolve_numeric_label_refs(statements);
        for stmt in &statements {
            self.process_statement(stmt)?;
        }
        self.compress_executable_sections();
        self.resolve_local_branches()?;
        Ok(())
    }

    fn process_statement(&mut self, stmt: &AsmStatement) -> Result<(), String> {
        match stmt {
            AsmStatement::Empty => Ok(()),

            AsmStatement::Label(name) => {
                // Ensure we have a section
                if self.current_section.is_empty() {
                    self.ensure_section(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, 2);
                    self.current_section = ".text".to_string();
                }
                let section = self.current_section.clone();
                let offset = self.current_offset();
                self.labels.insert(name.clone(), (section.clone(), offset));
                // Track GNU numeric labels (e.g., "1:", "2:") for Nb/Nf resolution.
                // Numeric labels can be defined multiple times; each definition is
                // appended so that forward/backward refs can find the correct one.
                if is_numeric_label(name) {
                    self.numeric_labels
                        .entry(name.clone())
                        .or_default()
                        .push((section, offset));
                }
                Ok(())
            }

            AsmStatement::Directive(directive) => {
                self.process_directive(directive)
            }

            AsmStatement::Instruction { mnemonic, operands, raw_operands } => {
                self.process_instruction(mnemonic, operands, raw_operands)
            }
        }
    }

    fn process_directive(&mut self, directive: &Directive) -> Result<(), String> {
        match directive {
            Directive::PushSection(info) => {
                self.section_stack.push(self.current_section.clone());
                // Same logic as Directive::Section
                return self.process_directive(&Directive::Section(info.clone()));
            }
            Directive::PopSection => {
                // Restore the previous section from the stack
                if let Some(prev) = self.section_stack.pop() {
                    self.current_section = prev;
                }
                // If stack is empty, silently keep current section (matches GNU as behavior)
                return Ok(());
            }
            Directive::Section(info) => {
                let sh_type = match info.sec_type.as_str() {
                    "@nobits" => SHT_NOBITS,
                    "@note" => SHT_NOTE,
                    _ => SHT_PROGBITS,
                };
                let mut sh_flags = 0u64;
                if info.flags.contains('a') { sh_flags |= SHF_ALLOC; }
                if info.flags.contains('w') { sh_flags |= SHF_WRITE; }
                if info.flags.contains('x') { sh_flags |= SHF_EXECINSTR; }
                if info.flags.contains('M') { sh_flags |= SHF_MERGE; }
                if info.flags.contains('S') { sh_flags |= SHF_STRINGS; }
                if info.flags.contains('T') { sh_flags |= SHF_TLS; }
                if info.flags.contains('G') { sh_flags |= SHF_GROUP; }

                // Only use default flags if no flags were explicitly provided.
                // An explicit empty flags string (e.g., "") means no flags (0).
                if sh_flags == 0 && !info.flags_explicit {
                    sh_flags = default_section_flags(&info.name);
                }

                // Use alignment 2 for text sections (RV64C compressed instructions
                // are 2-byte aligned), 1 for everything else unless specified.
                let align = if sh_flags & SHF_EXECINSTR != 0 { 2 } else { 1 };
                self.ensure_section(&info.name, sh_type, sh_flags, align);
                self.current_section = info.name.clone();
                Ok(())
            }

            Directive::Text => {
                self.ensure_section(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, 2);
                self.current_section = ".text".to_string();
                Ok(())
            }

            Directive::Data => {
                self.ensure_section(".data", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE, 1);
                self.current_section = ".data".to_string();
                Ok(())
            }

            Directive::Bss => {
                self.ensure_section(".bss", SHT_NOBITS, SHF_ALLOC | SHF_WRITE, 1);
                self.current_section = ".bss".to_string();
                Ok(())
            }

            Directive::Rodata => {
                self.ensure_section(".rodata", SHT_PROGBITS, SHF_ALLOC, 1);
                self.current_section = ".rodata".to_string();
                Ok(())
            }

            Directive::Globl(sym) => {
                self.global_symbols.insert(sym.clone(), true);
                Ok(())
            }

            Directive::Weak(sym) => {
                self.weak_symbols.insert(sym.clone(), true);
                Ok(())
            }

            Directive::SymVisibility(sym, vis) => {
                let v = match vis {
                    Visibility::Hidden => STV_HIDDEN,
                    Visibility::Protected => STV_PROTECTED,
                    Visibility::Internal => STV_INTERNAL,
                };
                self.symbol_visibility.insert(sym.clone(), v);
                Ok(())
            }

            Directive::Type(sym, st) => {
                let elf_type = match st {
                    SymbolType::Function => STT_FUNC,
                    SymbolType::Object => STT_OBJECT,
                    SymbolType::TlsObject => STT_TLS,
                    SymbolType::NoType => STT_NOTYPE,
                };
                self.symbol_types.insert(sym.clone(), elf_type);
                Ok(())
            }

            Directive::Size(sym, size_expr) => {
                match size_expr {
                    SizeExpr::CurrentMinus(label) => {
                        if let Some((section, label_offset)) = self.labels.get(label) {
                            if *section == self.current_section {
                                let current = self.current_offset();
                                let size = current - label_offset;
                                self.symbol_sizes.insert(sym.clone(), size);
                            }
                        }
                    }
                    SizeExpr::Absolute(size) => {
                        self.symbol_sizes.insert(sym.clone(), *size);
                    }
                }
                Ok(())
            }

            Directive::Align(val) => {
                // RISC-V .align N means 2^N bytes (same as .p2align)
                let bytes = 1u64 << val;
                self.align_to(bytes);
                Ok(())
            }

            Directive::Balign(val) => {
                self.align_to(*val);
                Ok(())
            }

            Directive::Byte(values) => {
                for dv in values {
                    match dv {
                        DataValue::Integer(v) => self.emit_bytes(&[*v as u8]),
                        // Symbol refs/diffs in .byte are not supported (no 1-byte relocation type)
                        _ => self.emit_bytes(&[0u8]),
                    }
                }
                Ok(())
            }

            Directive::Short(values) => {
                for dv in values {
                    match dv {
                        DataValue::Integer(v) => self.emit_bytes(&(*v as u16).to_le_bytes()),
                        // Symbol refs/diffs in .short are not supported (no 2-byte relocation type)
                        _ => self.emit_bytes(&0u16.to_le_bytes()),
                    }
                }
                Ok(())
            }

            Directive::Long(values) => {
                for dv in values {
                    self.emit_data_value(dv, 4)?;
                }
                Ok(())
            }

            Directive::Quad(values) => {
                for dv in values {
                    self.emit_data_value(dv, 8)?;
                }
                Ok(())
            }

            Directive::Zero { size, fill } => {
                self.emit_bytes(&vec![*fill; *size]);
                Ok(())
            }

            Directive::Asciz(s) => {
                self.emit_bytes(s);
                self.emit_bytes(&[0]); // null terminator
                Ok(())
            }

            Directive::Ascii(s) => {
                self.emit_bytes(s);
                Ok(())
            }

            Directive::Comm { sym, size, align } => {
                self.symbols.push(ElfSymbol {
                    name: sym.clone(),
                    value: *align,
                    size: *size,
                    binding: STB_GLOBAL,
                    sym_type: STT_OBJECT,
                    visibility: STV_DEFAULT,
                    section_name: "*COM*".to_string(),
                });
                Ok(())
            }

            Directive::Local(_) => {
                // .local symbol - marks symbol as local (default)
                // Nothing to do since symbols are local by default
                Ok(())
            }

            Directive::Set(alias, target) => {
                self.aliases.insert(alias.clone(), target.clone());
                Ok(())
            }

            Directive::ArchOption(_) => {
                // TODO: implement .option rvc/norvc/push/pop for compression control
                Ok(())
            }

            Directive::Attribute(_) => {
                // RISC-V attribute directives
                Ok(())
            }

            Directive::Cfi | Directive::Ignored => Ok(()),

            Directive::Insn(args) => {
                // .insn directive: emit a raw instruction encoding
                if self.current_section.is_empty() {
                    self.ensure_section(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, 2);
                    self.current_section = ".text".to_string();
                }
                match encode_insn_directive(args) {
                    Ok(EncodeResult::Word(word)) => {
                        self.emit_u32_le(word);
                        Ok(())
                    }
                    Ok(EncodeResult::Half(half)) => {
                        self.emit_u16_le(half);
                        Ok(())
                    }
                    Ok(_) => Ok(()),
                    Err(e) => Err(e),
                }
            }

            Directive::Unknown { name, args } => {
                Err(format!("unsupported RISC-V assembler directive: {} {}", name, args))
            }
        }
    }

    /// Emit a typed data value for .long (size=4) or .quad (size=8).
    /// Handles integers, symbol references, and symbol differences.
    fn emit_data_value(&mut self, dv: &DataValue, size: usize) -> Result<(), String> {
        match dv {
            DataValue::SymbolDiff { sym_a, sym_b, addend } => {
                let (add_type, sub_type) = if size == 4 {
                    (RelocType::Add32.elf_type(), RelocType::Sub32.elf_type())
                } else {
                    (RelocType::Add64.elf_type(), RelocType::Sub64.elf_type())
                };
                self.add_reloc(add_type, sym_a.clone(), *addend);
                self.add_reloc(sub_type, sym_b.clone(), 0);
                if size == 4 {
                    self.emit_bytes(&0u32.to_le_bytes());
                } else {
                    self.emit_bytes(&0u64.to_le_bytes());
                }
            }
            DataValue::Symbol { name, addend } => {
                let reloc_type = if size == 4 {
                    RelocType::Abs32.elf_type()
                } else {
                    RelocType::Abs64.elf_type()
                };
                self.add_reloc(reloc_type, name.clone(), *addend);
                if size == 4 {
                    self.emit_bytes(&0u32.to_le_bytes());
                } else {
                    self.emit_bytes(&0u64.to_le_bytes());
                }
            }
            DataValue::Integer(v) => {
                if size == 4 {
                    self.emit_bytes(&(*v as u32).to_le_bytes());
                } else {
                    self.emit_bytes(&(*v as u64).to_le_bytes());
                }
            }
        }
        Ok(())
    }

    fn process_instruction(&mut self, mnemonic: &str, operands: &[Operand], raw_operands: &str) -> Result<(), String> {
        // Make sure we're in a text section
        if self.current_section.is_empty() {
            self.ensure_section(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, 2);
            self.current_section = ".text".to_string();
        }

        match encode_instruction(mnemonic, operands, raw_operands) {
            Ok(EncodeResult::Word(word)) => {
                self.emit_u32_le(word);
                Ok(())
            }
            Ok(EncodeResult::Half(half)) => {
                self.emit_u16_le(half);
                Ok(())
            }
            Ok(EncodeResult::WordWithReloc { word, reloc }) => {
                let elf_type = reloc.reloc_type.elf_type();
                let is_pcrel_hi = elf_type == 23 || elf_type == 20 || elf_type == 22 || elf_type == 21;

                if is_pcrel_hi {
                    // Create a synthetic label at this auipc instruction so that
                    // subsequent %pcrel_lo references can find the matching %pcrel_hi.
                    let label = format!(".Lpcrel_hi{}", self.pcrel_hi_counter);
                    self.pcrel_hi_counter += 1;
                    let section = self.current_section.clone();
                    let offset = self.current_offset();
                    self.labels.insert(label, (section, offset));
                }

                let is_local = reloc.symbol.starts_with(".L") || reloc.symbol.starts_with(".l")
                            || parse_numeric_label_ref(&reloc.symbol).is_some();

                if is_local {
                    let offset = self.current_offset();
                    self.pending_branch_relocs.push(PendingReloc {
                        section: self.current_section.clone(),
                        offset,
                        reloc_type: elf_type,
                        symbol: reloc.symbol.clone(),
                        addend: reloc.addend,
                        pcrel_hi_offset: None,
                    });
                    self.emit_u32_le(word);
                } else {
                    self.add_reloc(elf_type, reloc.symbol, reloc.addend);
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
            Ok(EncodeResult::WordsWithRelocs(items)) => {
                // Handle pcrel_hi20+pcrel_lo12 pairs. When found, create a
                // synthetic local label at the auipc offset and redirect the
                // pcrel_lo12 relocation to reference that label.
                //
                // Both pcrel_hi and pcrel_lo are always emitted as external
                // relocations for the linker. This avoids inconsistency
                // when pcrel_hi is resolved locally but pcrel_lo isn't.
                let mut pcrel_hi_label: Option<String> = None;

                for (word, reloc_opt) in &items {
                    if let Some(reloc) = reloc_opt {
                        let elf_type = reloc.reloc_type.elf_type();
                        let is_pcrel_hi = elf_type == 23; // R_RISCV_PCREL_HI20
                        let is_got_hi = elf_type == 20;   // R_RISCV_GOT_HI20
                        let is_tls_gd_hi = elf_type == 22; // R_RISCV_TLS_GD_HI20
                        let is_tls_got_hi = elf_type == 21; // R_RISCV_TLS_GOT_HI20

                        if is_pcrel_hi || is_got_hi || is_tls_gd_hi || is_tls_got_hi {
                            // Create synthetic label at the auipc offset
                            let label = format!(".Lpcrel_hi{}", self.pcrel_hi_counter);
                            self.pcrel_hi_counter += 1;
                            let section = self.current_section.clone();
                            let offset = self.current_offset();
                            self.labels.insert(label.clone(), (section, offset));
                            pcrel_hi_label = Some(label);

                            // Always emit as a relocation (never resolve locally)
                            // so the paired pcrel_lo can also be resolved by the linker.
                            self.add_reloc(elf_type, reloc.symbol.clone(), reloc.addend);
                            self.emit_u32_le(*word);
                            continue;
                        }

                        let is_pcrel_lo12_i = elf_type == 24; // R_RISCV_PCREL_LO12_I
                        let is_pcrel_lo12_s = elf_type == 25; // R_RISCV_PCREL_LO12_S

                        if let Some(hi_label) = pcrel_hi_label.as_ref().filter(|_| is_pcrel_lo12_i || is_pcrel_lo12_s) {
                            // Emit pcrel_lo referencing the synthetic label
                            let hi_label = hi_label.clone();
                            self.add_reloc(elf_type, hi_label, 0);
                            self.emit_u32_le(*word);
                            continue;
                        }

                        // Non-pcrel relocation, handle normally
                        let is_local = reloc.symbol.starts_with(".L") || reloc.symbol.starts_with(".l")
                            || parse_numeric_label_ref(&reloc.symbol).is_some();
                        if is_local {
                            let offset = self.current_offset();
                            self.pending_branch_relocs.push(PendingReloc {
                                section: self.current_section.clone(),
                                offset,
                                reloc_type: elf_type,
                                symbol: reloc.symbol.clone(),
                                addend: reloc.addend,
                                pcrel_hi_offset: None,
                            });
                        } else {
                            self.add_reloc(elf_type, reloc.symbol.clone(), reloc.addend);
                        }
                    }
                    self.emit_u32_le(*word);
                }
                Ok(())
            }
            Ok(EncodeResult::Skip) => Ok(()),
            Err(e) => {
                Err(e)
            }
        }
    }

    /// Compress eligible 32-bit instructions in executable sections to 16-bit
    /// RV64C equivalents. Updates all label offsets, pending reloc offsets, and
    /// section reloc offsets to account for the reduced instruction sizes.
    fn compress_executable_sections(&mut self) {
        let exec_sections: Vec<String> = self.sections.iter()
            .filter(|(_, s)| (s.sh_flags & SHF_EXECINSTR) != 0)
            .map(|(name, _)| name.clone())
            .collect();

        for sec_name in &exec_sections {
            // Build set of offsets that have relocations (these must not be compressed)
            let mut reloc_offsets = HashSet::new();

            // Pending branch relocs
            for pr in &self.pending_branch_relocs {
                if pr.section == *sec_name {
                    reloc_offsets.insert(pr.offset);
                    // For CALL_PLT (auipc+jalr pair), also mark the jalr at offset+4
                    if pr.reloc_type == 19 {
                        reloc_offsets.insert(pr.offset + 4);
                    }
                }
            }

            // Section relocs (external symbols)
            if let Some(section) = self.sections.get(sec_name) {
                for r in &section.relocs {
                    reloc_offsets.insert(r.offset);
                    // For CALL_PLT (auipc+jalr pair), also mark the jalr at offset+4
                    if r.reloc_type == 19 {
                        reloc_offsets.insert(r.offset + 4);
                    }
                }
            }

            let section_data = match self.sections.get(sec_name) {
                Some(s) => s.data.clone(),
                None => continue,
            };

            let (new_data, offset_map) = compress::compress_section(&section_data, &reloc_offsets);

            if new_data.len() == section_data.len() {
                continue; // Nothing was compressed
            }

            // Update section data
            if let Some(section) = self.sections.get_mut(sec_name) {
                section.data = new_data;
                // Update section alignment to 2 (halfword) for compressed code
                // Actually, keep original alignment but update relocs
                for r in &mut section.relocs {
                    r.offset = compress::remap_offset(r.offset, &offset_map);
                }
            }

            // Update pending branch relocs
            for pr in &mut self.pending_branch_relocs {
                if pr.section == *sec_name {
                    pr.offset = compress::remap_offset(pr.offset, &offset_map);
                }
            }

            // Update labels pointing into this section
            for (_, (label_sec, label_offset)) in self.labels.iter_mut() {
                if label_sec == sec_name {
                    *label_offset = compress::remap_offset(*label_offset, &offset_map);
                }
            }

            // Update numeric labels pointing into this section
            for (_, defs) in self.numeric_labels.iter_mut() {
                for (def_sec, def_offset) in defs.iter_mut() {
                    if def_sec == sec_name {
                        *def_offset = compress::remap_offset(*def_offset, &offset_map);
                    }
                }
            }

            // Update symbol values pointing into this section
            for sym in &mut self.symbols {
                if sym.section_name == *sec_name {
                    sym.value = compress::remap_offset(sym.value, &offset_map);
                }
            }
        }
    }

    /// Resolve a numeric label reference like "1b" or "1f" to a (section, offset).
    /// For `Nb` (backward), find the most recent definition of label N at or before `ref_offset`.
    /// For `Nf` (forward), find the first definition of label N after `ref_offset`.
    fn resolve_numeric_label_ref(
        &self,
        label_name: &str,
        is_backward: bool,
        ref_section: &str,
        ref_offset: u64,
    ) -> Option<(String, u64)> {
        let defs = self.numeric_labels.get(label_name)?;
        if is_backward {
            // Find the last definition at or before ref_offset in the same section
            let mut best: Option<&(String, u64)> = None;
            for def in defs {
                if def.0 == ref_section && def.1 <= ref_offset {
                    best = Some(def);
                }
            }
            best.cloned()
        } else {
            // Find the first definition after ref_offset in the same section
            for def in defs {
                if def.0 == ref_section && def.1 > ref_offset {
                    return Some(def.clone());
                }
            }
            None
        }
    }

    /// Resolve local branch labels to PC-relative offsets.
    fn resolve_local_branches(&mut self) -> Result<(), String> {
        for reloc in &self.pending_branch_relocs {
            // Try resolving as a numeric label reference (e.g., "1b", "1f")
            let resolved = if let Some((label_name, is_backward)) = parse_numeric_label_ref(&reloc.symbol) {
                self.resolve_numeric_label_ref(label_name, is_backward, &reloc.section, reloc.offset)
            } else {
                self.labels.get(&reloc.symbol).cloned()
            };

            let (target_section, target_offset) = match resolved {
                Some(v) => v,
                None => {
                    // Undefined local label - leave as external relocation
                    if let Some(section) = self.sections.get_mut(&reloc.section) {
                        section.relocs.push(ElfReloc {
                            offset: reloc.offset,
                            reloc_type: reloc.reloc_type,
                            symbol_name: reloc.symbol.clone(),
                            addend: reloc.addend,
                        });
                    }
                    continue;
                }
            };

            if target_section != reloc.section {
                // Cross-section reference
                if let Some(section) = self.sections.get_mut(&reloc.section) {
                    section.relocs.push(ElfReloc {
                        offset: reloc.offset,
                        reloc_type: reloc.reloc_type,
                        symbol_name: reloc.symbol.clone(),
                        addend: reloc.addend,
                    });
                }
                continue;
            }

            // For pcrel_lo12 with a stored pcrel_hi_offset, compute the offset
            // from the auipc instruction's PC (not the pcrel_lo instruction's PC).
            let ref_offset = reloc.pcrel_hi_offset.unwrap_or(reloc.offset);
            let pc_offset = (target_offset as i64) - (ref_offset as i64) + reloc.addend;

            if let Some(section) = self.sections.get_mut(&reloc.section) {
                let instr_offset = reloc.offset as usize;

                match reloc.reloc_type {
                    16 => {
                        // R_RISCV_BRANCH (B-type, 12-bit)
                        if instr_offset + 4 > section.data.len() { continue; }
                        let mut word = u32::from_le_bytes([
                            section.data[instr_offset],
                            section.data[instr_offset + 1],
                            section.data[instr_offset + 2],
                            section.data[instr_offset + 3],
                        ]);
                        let imm = pc_offset as u32;
                        let bit12 = (imm >> 12) & 1;
                        let bit11 = (imm >> 11) & 1;
                        let bits10_5 = (imm >> 5) & 0x3F;
                        let bits4_1 = (imm >> 1) & 0xF;
                        // Clear existing immediate bits
                        word &= 0x01FFF07F;
                        // Set new immediate bits
                        word |= (bit12 << 31) | (bits10_5 << 25) | (bits4_1 << 8) | (bit11 << 7);
                        section.data[instr_offset..instr_offset + 4].copy_from_slice(&word.to_le_bytes());
                    }
                    17 => {
                        // R_RISCV_JAL (J-type, 20-bit)
                        if instr_offset + 4 > section.data.len() { continue; }
                        let mut word = u32::from_le_bytes([
                            section.data[instr_offset],
                            section.data[instr_offset + 1],
                            section.data[instr_offset + 2],
                            section.data[instr_offset + 3],
                        ]);
                        let imm = pc_offset as u32;
                        let bit20 = (imm >> 20) & 1;
                        let bits10_1 = (imm >> 1) & 0x3FF;
                        let bit11 = (imm >> 11) & 1;
                        let bits19_12 = (imm >> 12) & 0xFF;
                        word &= 0x00000FFF;
                        word |= (bit20 << 31) | (bits10_1 << 21) | (bit11 << 20) | (bits19_12 << 12);
                        section.data[instr_offset..instr_offset + 4].copy_from_slice(&word.to_le_bytes());
                    }
                    19 => {
                        // R_RISCV_CALL_PLT (AUIPC + JALR pair, 8 bytes)
                        if instr_offset + 8 > section.data.len() { continue; }

                        let hi = ((pc_offset as i32 + 0x800) >> 12) as u32;
                        let lo = ((pc_offset as i32) << 20 >> 20) as u32;

                        // Patch AUIPC
                        let mut auipc = u32::from_le_bytes([
                            section.data[instr_offset],
                            section.data[instr_offset + 1],
                            section.data[instr_offset + 2],
                            section.data[instr_offset + 3],
                        ]);
                        auipc = (auipc & 0xFFF) | (hi << 12);
                        section.data[instr_offset..instr_offset + 4].copy_from_slice(&auipc.to_le_bytes());

                        // Patch JALR
                        let mut jalr = u32::from_le_bytes([
                            section.data[instr_offset + 4],
                            section.data[instr_offset + 5],
                            section.data[instr_offset + 6],
                            section.data[instr_offset + 7],
                        ]);
                        jalr = (jalr & 0xFFFFF) | ((lo & 0xFFF) << 20);
                        section.data[instr_offset + 4..instr_offset + 8].copy_from_slice(&jalr.to_le_bytes());
                    }
                    23 => {
                        // R_RISCV_PCREL_HI20 (AUIPC hi20)
                        if instr_offset + 4 > section.data.len() { continue; }
                        let hi = ((pc_offset as i32 + 0x800) >> 12) as u32;
                        let mut word = u32::from_le_bytes([
                            section.data[instr_offset],
                            section.data[instr_offset + 1],
                            section.data[instr_offset + 2],
                            section.data[instr_offset + 3],
                        ]);
                        word = (word & 0xFFF) | (hi << 12);
                        section.data[instr_offset..instr_offset + 4].copy_from_slice(&word.to_le_bytes());
                    }
                    24 => {
                        // R_RISCV_PCREL_LO12_I (ADDI/LD lo12 I-type)
                        if instr_offset + 4 > section.data.len() { continue; }
                        let lo = (pc_offset as i32) & 0xFFF;
                        let mut word = u32::from_le_bytes([
                            section.data[instr_offset],
                            section.data[instr_offset + 1],
                            section.data[instr_offset + 2],
                            section.data[instr_offset + 3],
                        ]);
                        word = (word & 0xFFFFF) | (((lo as u32) & 0xFFF) << 20);
                        section.data[instr_offset..instr_offset + 4].copy_from_slice(&word.to_le_bytes());
                    }
                    25 => {
                        // R_RISCV_PCREL_LO12_S (SW/SD lo12 S-type)
                        if instr_offset + 4 > section.data.len() { continue; }
                        let lo = (pc_offset as i32) & 0xFFF;
                        let mut word = u32::from_le_bytes([
                            section.data[instr_offset],
                            section.data[instr_offset + 1],
                            section.data[instr_offset + 2],
                            section.data[instr_offset + 3],
                        ]);
                        let imm_lo = (lo as u32) & 0x1F;
                        let imm_hi = ((lo as u32) >> 5) & 0x7F;
                        word &= 0x01FFF07F; // Clear imm bits
                        word |= (imm_hi << 25) | (imm_lo << 7);
                        section.data[instr_offset..instr_offset + 4].copy_from_slice(&word.to_le_bytes());
                    }
                    _ => {
                        // Unknown reloc type for local branch - leave as external
                        section.relocs.push(ElfReloc {
                            offset: reloc.offset,
                            reloc_type: reloc.reloc_type,
                            symbol_name: reloc.symbol.clone(),
                            addend: reloc.addend,
                        });
                    }
                }
            }
        }
        Ok(())
    }

    /// Write the final ELF object file.
    ///
    /// Uses shared `build_elf_symbol_table` for symbol table construction
    /// and `write_relocatable_object` for ELF serialization.
    pub fn write_elf(&mut self, output_path: &str) -> Result<(), String> {
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
            include_referenced_locals: true, // RISC-V needs pcrel_hi synthetic labels
        };

        let config = elf::ElfConfig {
            e_machine: EM_RISCV,
            e_flags: EF_RISCV_FLOAT_ABI_DOUBLE | EF_RISCV_RVC,
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

