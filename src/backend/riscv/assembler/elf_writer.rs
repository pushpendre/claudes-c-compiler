//! ELF object file writer for RISC-V.
//!
//! Takes parsed assembly statements and produces an ELF .o (relocatable) file
//! with proper sections, symbols, and relocations for RISC-V 64-bit ELF.

#![allow(dead_code)]

use std::collections::{HashMap, HashSet};
use super::parser::{AsmStatement, Operand};
use super::encoder::{encode_instruction, EncodeResult, RelocType};
use super::compress;

const EM_RISCV: u16 = 243;
const ELFCLASS64: u8 = 2;
const ELFDATA2LSB: u8 = 1;
const EV_CURRENT: u8 = 1;
const ELFOSABI_NONE: u8 = 0;
const ET_REL: u16 = 1;

// ELF flags for RISC-V
const EF_RISCV_RVC: u32 = 0x1;          // compressed extensions used
const EF_RISCV_FLOAT_ABI_DOUBLE: u32 = 0x4;

// Section types
const SHT_NULL: u32 = 0;
const SHT_PROGBITS: u32 = 1;
const SHT_SYMTAB: u32 = 2;
const SHT_STRTAB: u32 = 3;
const SHT_RELA: u32 = 4;
const SHT_NOBITS: u32 = 8;
const SHT_NOTE: u32 = 7;

// Section flags
const SHF_WRITE: u64 = 1;
const SHF_ALLOC: u64 = 2;
const SHF_EXECINSTR: u64 = 4;
const SHF_MERGE: u64 = 0x10;
const SHF_STRINGS: u64 = 0x20;
const SHF_INFO_LINK: u64 = 0x40;
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
const STV_HIDDEN: u8 = 2;
const STV_PROTECTED: u8 = 3;
const STV_INTERNAL: u8 = 1;

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

    /// Try to handle a symbol difference expression like "A - B" or "A - B + C".
    /// For .word/.long (size=4), emits R_RISCV_ADD32 + R_RISCV_SUB32 relocation pairs.
    /// For .quad/.dword (size=8), emits R_RISCV_ADD64 + R_RISCV_SUB64 relocation pairs.
    /// Returns None if the expression is not a symbol difference.
    fn try_emit_symbol_diff(&mut self, expr: &str, size: usize) -> Option<Result<(), String>> {
        let expr = expr.trim();
        if expr.is_empty() {
            return None;
        }

        // Look for "A - B" pattern where A is a symbol reference
        // The first character should be a letter, underscore, or dot (symbol start)
        let first_char = expr.chars().next()?;
        if !first_char.is_ascii_alphabetic() && first_char != '_' && first_char != '.' {
            return None;
        }

        // Find the '-' that separates the two symbols (not at position 0)
        // Be careful with dots in section names and symbol names
        let minus_pos = find_symbol_diff_minus(expr)?;

        let sym_a = expr[..minus_pos].trim();
        let rest = expr[minus_pos + 1..].trim();

        // rest might be "B" or "B + offset"
        // Parse sym_b and optional addend
        let (sym_b, addend) = if let Some(plus_pos) = rest.find('+') {
            let b = rest[..plus_pos].trim();
            let add_str = rest[plus_pos + 1..].trim();
            let add_val: i64 = add_str.parse().unwrap_or(0);
            (b, add_val)
        } else {
            (rest, 0i64)
        };

        // Verify sym_b looks like a symbol
        if sym_b.is_empty() {
            return None;
        }
        let b_first = sym_b.chars().next().unwrap();
        if !b_first.is_ascii_alphabetic() && b_first != '_' && b_first != '.' {
            return None;
        }

        // Emit ADD + SUB relocation pair
        let (add_type, sub_type) = if size == 4 {
            (RelocType::Add32.elf_type(), RelocType::Sub32.elf_type())
        } else {
            (RelocType::Add64.elf_type(), RelocType::Sub64.elf_type())
        };

        self.add_reloc(add_type, sym_a.to_string(), addend);
        // SUB reloc is at the same offset (add_reloc uses current_offset which
        // hasn't changed yet since we haven't emitted the data bytes)
        self.add_reloc(sub_type, sym_b.to_string(), 0);

        // Emit zero bytes as placeholder
        if size == 4 {
            self.emit_bytes(&0u32.to_le_bytes());
        } else {
            self.emit_bytes(&0u64.to_le_bytes());
        }

        Some(Ok(()))
    }

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
                section.data.extend(std::iter::repeat(0u8).take(padding));
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
                    self.ensure_section(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, 4);
                    self.current_section = ".text".to_string();
                }
                let section = self.current_section.clone();
                let offset = self.current_offset();
                self.labels.insert(name.clone(), (section, offset));
                Ok(())
            }

            AsmStatement::Directive { name, args } => {
                self.process_directive(name, args)
            }

            AsmStatement::Instruction { mnemonic, operands, raw_operands } => {
                self.process_instruction(mnemonic, operands, raw_operands)
            }
        }
    }

    fn process_directive(&mut self, name: &str, args: &str) -> Result<(), String> {
        match name {
            ".section" => {
                let (sec_name, flags, sec_type) = parse_section_directive(args);
                let sh_type = match sec_type.as_str() {
                    "@nobits" => SHT_NOBITS,
                    "@note" => SHT_NOTE,
                    _ => SHT_PROGBITS,
                };
                let mut sh_flags = 0u64;
                if flags.contains('a') { sh_flags |= SHF_ALLOC; }
                if flags.contains('w') { sh_flags |= SHF_WRITE; }
                if flags.contains('x') { sh_flags |= SHF_EXECINSTR; }
                if flags.contains('M') { sh_flags |= SHF_MERGE; }
                if flags.contains('S') { sh_flags |= SHF_STRINGS; }
                if flags.contains('T') { sh_flags |= SHF_TLS; }
                if flags.contains('G') { sh_flags |= SHF_GROUP; }

                // Set default flags based on section name
                if sh_flags == 0 {
                    sh_flags = default_section_flags(&sec_name);
                }

                let align = if sec_name == ".text" { 4 } else { 1 };
                self.ensure_section(&sec_name, sh_type, sh_flags, align);
                self.current_section = sec_name;
                Ok(())
            }

            ".text" => {
                self.ensure_section(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, 4);
                self.current_section = ".text".to_string();
                Ok(())
            }

            ".data" => {
                self.ensure_section(".data", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE, 1);
                self.current_section = ".data".to_string();
                Ok(())
            }

            ".bss" => {
                self.ensure_section(".bss", SHT_NOBITS, SHF_ALLOC | SHF_WRITE, 1);
                self.current_section = ".bss".to_string();
                Ok(())
            }

            ".rodata" => {
                self.ensure_section(".rodata", SHT_PROGBITS, SHF_ALLOC, 1);
                self.current_section = ".rodata".to_string();
                Ok(())
            }

            ".globl" | ".global" => {
                let sym = args.trim();
                self.global_symbols.insert(sym.to_string(), true);
                Ok(())
            }

            ".weak" => {
                let sym = args.trim();
                self.weak_symbols.insert(sym.to_string(), true);
                Ok(())
            }

            ".hidden" => {
                let sym = args.trim();
                self.symbol_visibility.insert(sym.to_string(), STV_HIDDEN);
                Ok(())
            }

            ".protected" => {
                let sym = args.trim();
                self.symbol_visibility.insert(sym.to_string(), STV_PROTECTED);
                Ok(())
            }

            ".internal" => {
                let sym = args.trim();
                self.symbol_visibility.insert(sym.to_string(), STV_INTERNAL);
                Ok(())
            }

            ".type" => {
                let parts: Vec<&str> = args.splitn(2, ',').collect();
                if parts.len() == 2 {
                    let sym = parts[0].trim();
                    let ty = parts[1].trim();
                    let st = match ty {
                        "%function" | "@function" => STT_FUNC,
                        "%object" | "@object" => STT_OBJECT,
                        "@tls_object" => STT_TLS,
                        _ => STT_NOTYPE,
                    };
                    self.symbol_types.insert(sym.to_string(), st);
                }
                Ok(())
            }

            ".size" => {
                let parts: Vec<&str> = args.splitn(2, ',').collect();
                if parts.len() == 2 {
                    let sym = parts[0].trim();
                    let size_expr = parts[1].trim();
                    if size_expr.starts_with(".-") {
                        let label = &size_expr[2..];
                        if let Some((section, label_offset)) = self.labels.get(label) {
                            if *section == self.current_section {
                                let current = self.current_offset();
                                let size = current - label_offset;
                                self.symbol_sizes.insert(sym.to_string(), size);
                            }
                        }
                    } else if let Ok(size) = size_expr.parse::<u64>() {
                        self.symbol_sizes.insert(sym.to_string(), size);
                    }
                }
                Ok(())
            }

            ".align" | ".p2align" => {
                let align_val: u64 = args.trim().split(',').next()
                    .and_then(|s| s.trim().parse().ok())
                    .unwrap_or(0);
                // RISC-V .align N means 2^N bytes (same as .p2align)
                let bytes = 1u64 << align_val;
                self.align_to(bytes);
                Ok(())
            }

            ".balign" => {
                let align_val: u64 = args.trim().parse().unwrap_or(1);
                self.align_to(align_val);
                Ok(())
            }

            ".byte" => {
                for part in args.split(',') {
                    let val = parse_data_value(part.trim())? as u8;
                    self.emit_bytes(&[val]);
                }
                Ok(())
            }

            ".short" | ".hword" | ".2byte" | ".half" => {
                for part in args.split(',') {
                    let val = parse_data_value(part.trim())? as u16;
                    self.emit_bytes(&val.to_le_bytes());
                }
                Ok(())
            }

            ".long" | ".4byte" | ".word" => {
                for part in args.split(',') {
                    let trimmed = part.trim();
                    // Handle symbol difference expressions: A - B
                    // Must emit R_RISCV_ADD32 + R_RISCV_SUB32 relocation pair
                    if let Some(result) = self.try_emit_symbol_diff(trimmed, 4) {
                        result?;
                        continue;
                    }
                    if is_symbol_ref(trimmed) {
                        let (sym, addend) = parse_symbol_addend(trimmed);
                        self.add_reloc(RelocType::Abs32.elf_type(), sym, addend);
                        self.emit_bytes(&0u32.to_le_bytes());
                        continue;
                    }
                    let val = parse_data_value(trimmed)? as u32;
                    self.emit_bytes(&val.to_le_bytes());
                }
                Ok(())
            }

            ".quad" | ".8byte" | ".xword" | ".dword" => {
                for part in args.split(',') {
                    let trimmed = part.trim();
                    // Handle symbol difference expressions: A - B
                    if let Some(result) = self.try_emit_symbol_diff(trimmed, 8) {
                        result?;
                        continue;
                    }
                    if is_symbol_ref(trimmed) {
                        let (sym, addend) = parse_symbol_addend(trimmed);
                        self.add_reloc(RelocType::Abs64.elf_type(), sym, addend);
                        self.emit_bytes(&0u64.to_le_bytes());
                        continue;
                    }
                    let val = parse_data_value(trimmed)? as u64;
                    self.emit_bytes(&val.to_le_bytes());
                }
                Ok(())
            }

            ".zero" | ".space" => {
                let parts: Vec<&str> = args.trim().split(',').collect();
                let size: usize = parts[0].trim().parse()
                    .map_err(|_| format!("invalid .zero size: {}", args))?;
                let fill: u8 = if parts.len() > 1 {
                    parse_data_value(parts[1].trim())? as u8
                } else {
                    0
                };
                self.emit_bytes(&vec![fill; size]);
                Ok(())
            }

            ".asciz" | ".string" => {
                let s = parse_string_literal(args)?;
                self.emit_bytes(s.as_bytes());
                self.emit_bytes(&[0]); // null terminator
                Ok(())
            }

            ".ascii" => {
                let s = parse_string_literal(args)?;
                self.emit_bytes(s.as_bytes());
                Ok(())
            }

            ".comm" => {
                let parts: Vec<&str> = args.split(',').collect();
                if parts.len() >= 2 {
                    let sym = parts[0].trim();
                    let size: u64 = parts[1].trim().parse().unwrap_or(0);
                    let align: u64 = if parts.len() > 2 {
                        parts[2].trim().parse().unwrap_or(1)
                    } else {
                        1
                    };

                    self.symbols.push(ElfSymbol {
                        name: sym.to_string(),
                        value: align,
                        size,
                        binding: STB_GLOBAL,
                        sym_type: STT_OBJECT,
                        visibility: STV_DEFAULT,
                        section_name: "*COM*".to_string(),
                    });
                }
                Ok(())
            }

            ".local" => {
                // .local symbol - marks symbol as local (default)
                // Nothing to do since symbols are local by default
                Ok(())
            }

            ".set" | ".equ" => {
                // .set name, value - define a symbol with a value
                // TODO: implement properly
                Ok(())
            }

            ".option" => {
                // RISC-V specific: .option rvc, .option norvc, .option push, .option pop
                // Skip for now
                Ok(())
            }

            ".attribute" => {
                // RISC-V attribute directives
                Ok(())
            }

            // CFI directives - skip them
            ".cfi_startproc" | ".cfi_endproc" | ".cfi_def_cfa_offset"
            | ".cfi_offset" | ".cfi_def_cfa_register" | ".cfi_restore"
            | ".cfi_remember_state" | ".cfi_restore_state"
            | ".cfi_adjust_cfa_offset" | ".cfi_def_cfa"
            | ".cfi_sections" | ".cfi_personality" | ".cfi_lsda"
            | ".cfi_rel_offset" | ".cfi_register" | ".cfi_return_column"
            | ".cfi_undefined" | ".cfi_same_value" | ".cfi_escape" => Ok(()),

            // Other directives we can safely ignore
            ".file" | ".loc" | ".ident" | ".addrsig" | ".addrsig_sym"
            | ".build_attributes" | ".eabi_attribute" => Ok(()),

            _ => {
                // Unknown directive - ignore with a warning
                // TODO: handle more directives
                Ok(())
            }
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

                let is_local = reloc.symbol.starts_with(".L") || reloc.symbol.starts_with(".l");

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

                        if (is_pcrel_lo12_i || is_pcrel_lo12_s) && pcrel_hi_label.is_some() {
                            // Emit pcrel_lo referencing the synthetic label
                            let hi_label = pcrel_hi_label.as_ref().unwrap().clone();
                            self.add_reloc(elf_type, hi_label, 0);
                            self.emit_u32_le(*word);
                            continue;
                        }

                        // Non-pcrel relocation, handle normally
                        let is_local = reloc.symbol.starts_with(".L") || reloc.symbol.starts_with(".l");
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
                return Err(e);
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

            // Update symbol values pointing into this section
            for sym in &mut self.symbols {
                if sym.section_name == *sec_name {
                    sym.value = compress::remap_offset(sym.value, &offset_map);
                }
            }
        }
    }

    /// Resolve local branch labels to PC-relative offsets.
    fn resolve_local_branches(&mut self) -> Result<(), String> {
        for reloc in &self.pending_branch_relocs {
            let (target_section, target_offset) = match self.labels.get(&reloc.symbol) {
                Some(v) => v.clone(),
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
    pub fn write_elf(&mut self, output_path: &str) -> Result<(), String> {
        self.build_symbol_table();

        let mut elf = Vec::new();

        let mut shstrtab = StringTable::new();
        let mut strtab = StringTable::new();

        shstrtab.add("");
        strtab.add("");

        let content_sections: Vec<String> = self.section_order.clone();

        // Build symbol table entries
        let mut sym_entries: Vec<SymEntry> = Vec::new();
        // NULL entry
        sym_entries.push(SymEntry {
            st_name: 0, st_info: 0, st_other: 0,
            st_shndx: 0, st_value: 0, st_size: 0,
        });

        // Section symbols
        for (i, sec_name) in content_sections.iter().enumerate() {
            strtab.add(sec_name);
            sym_entries.push(SymEntry {
                st_name: strtab.offset_of(sec_name),
                st_info: (STB_LOCAL << 4) | STT_SECTION,
                st_other: 0,
                st_shndx: (i + 1) as u16,
                st_value: 0,
                st_size: 0,
            });
        }

        // Local then global symbols
        let mut local_syms: Vec<&ElfSymbol> = Vec::new();
        let mut global_syms: Vec<&ElfSymbol> = Vec::new();

        for sym in &self.symbols {
            if sym.binding == STB_LOCAL {
                local_syms.push(sym);
            } else {
                global_syms.push(sym);
            }
        }

        let first_global_idx = sym_entries.len() + local_syms.len();

        for sym in &local_syms {
            let name_offset = strtab.add(&sym.name);
            let shndx = section_index(&sym.section_name, &content_sections);
            sym_entries.push(SymEntry {
                st_name: name_offset,
                st_info: (sym.binding << 4) | sym.sym_type,
                st_other: sym.visibility,
                st_shndx: shndx,
                st_value: sym.value,
                st_size: sym.size,
            });
        }

        for sym in &global_syms {
            let name_offset = strtab.add(&sym.name);
            let shndx = section_index(&sym.section_name, &content_sections);
            sym_entries.push(SymEntry {
                st_name: name_offset,
                st_info: (sym.binding << 4) | sym.sym_type,
                st_other: sym.visibility,
                st_shndx: shndx,
                st_value: sym.value,
                st_size: sym.size,
            });
        }

        // Add shstrtab names
        shstrtab.add("");
        for sec_name in &content_sections {
            shstrtab.add(sec_name);
        }
        shstrtab.add(".symtab");
        shstrtab.add(".strtab");
        shstrtab.add(".shstrtab");

        let mut rela_sections: Vec<String> = Vec::new();
        for sec_name in &content_sections {
            if let Some(section) = self.sections.get(sec_name) {
                if !section.relocs.is_empty() {
                    let rela_name = format!(".rela{}", sec_name);
                    shstrtab.add(&rela_name);
                    rela_sections.push(rela_name);
                }
            }
        }

        // ── Calculate layout ──
        let ehdr_size = 64usize;
        let mut offset = ehdr_size;

        let mut section_offsets: Vec<usize> = Vec::new();
        for sec_name in &content_sections {
            let section = self.sections.get(sec_name).unwrap();
            let align = section.sh_addralign.max(1) as usize;
            offset = (offset + align - 1) & !(align - 1);
            section_offsets.push(offset);
            if section.sh_type != SHT_NOBITS {
                offset += section.data.len();
            }
        }

        let mut rela_offsets: Vec<usize> = Vec::new();
        for sec_name in &content_sections {
            if let Some(section) = self.sections.get(sec_name) {
                if !section.relocs.is_empty() {
                    offset = (offset + 7) & !7;
                    rela_offsets.push(offset);
                    offset += section.relocs.len() * 24;
                }
            }
        }

        offset = (offset + 7) & !7;
        let symtab_offset = offset;
        let symtab_size = sym_entries.len() * 24;
        offset += symtab_size;

        let strtab_offset = offset;
        let strtab_data = strtab.data();
        offset += strtab_data.len();

        let shstrtab_offset = offset;
        let shstrtab_data = shstrtab.data();
        offset += shstrtab_data.len();

        offset = (offset + 7) & !7;
        let shdr_offset = offset;

        let num_sections = 1 + content_sections.len() + rela_sections.len() + 3;
        let shstrtab_idx = num_sections - 1;

        // ── Write ELF header ──
        elf.extend_from_slice(&[0x7f, b'E', b'L', b'F']); // magic
        elf.push(ELFCLASS64);
        elf.push(ELFDATA2LSB);
        elf.push(EV_CURRENT);
        elf.push(ELFOSABI_NONE);
        elf.extend_from_slice(&[0u8; 8]); // padding
        elf.extend_from_slice(&ET_REL.to_le_bytes());
        elf.extend_from_slice(&EM_RISCV.to_le_bytes());
        elf.extend_from_slice(&1u32.to_le_bytes()); // e_version
        elf.extend_from_slice(&0u64.to_le_bytes()); // e_entry
        elf.extend_from_slice(&0u64.to_le_bytes()); // e_phoff
        elf.extend_from_slice(&(shdr_offset as u64).to_le_bytes());
        elf.extend_from_slice(&(EF_RISCV_FLOAT_ABI_DOUBLE | EF_RISCV_RVC).to_le_bytes()); // e_flags
        elf.extend_from_slice(&(ehdr_size as u16).to_le_bytes());
        elf.extend_from_slice(&0u16.to_le_bytes()); // e_phentsize
        elf.extend_from_slice(&0u16.to_le_bytes()); // e_phnum
        elf.extend_from_slice(&64u16.to_le_bytes()); // e_shentsize
        elf.extend_from_slice(&(num_sections as u16).to_le_bytes());
        elf.extend_from_slice(&(shstrtab_idx as u16).to_le_bytes());

        assert_eq!(elf.len(), ehdr_size);

        // ── Write content section data ──
        for (i, sec_name) in content_sections.iter().enumerate() {
            let section = self.sections.get(sec_name).unwrap();
            while elf.len() < section_offsets[i] {
                elf.push(0);
            }
            if section.sh_type != SHT_NOBITS {
                elf.extend_from_slice(&section.data);
            }
        }

        // ── Write rela section data ──
        let symtab_shndx = 1 + content_sections.len() + rela_sections.len();
        let mut rela_idx = 0;
        for sec_name in &content_sections {
            if let Some(section) = self.sections.get(sec_name) {
                if !section.relocs.is_empty() {
                    while elf.len() < rela_offsets[rela_idx] {
                        elf.push(0);
                    }
                    for reloc in &section.relocs {
                        let sym_idx = self.find_symbol_index(&reloc.symbol_name, &sym_entries, &strtab, &content_sections);
                        elf.extend_from_slice(&reloc.offset.to_le_bytes());
                        let r_info = ((sym_idx as u64) << 32) | (reloc.reloc_type as u64);
                        elf.extend_from_slice(&r_info.to_le_bytes());
                        elf.extend_from_slice(&reloc.addend.to_le_bytes());
                    }
                    rela_idx += 1;
                }
            }
        }

        // ── Write symtab ──
        while elf.len() < symtab_offset {
            elf.push(0);
        }
        for sym in &sym_entries {
            elf.extend_from_slice(&sym.st_name.to_le_bytes());
            elf.push(sym.st_info);
            elf.push(sym.st_other);
            elf.extend_from_slice(&sym.st_shndx.to_le_bytes());
            elf.extend_from_slice(&sym.st_value.to_le_bytes());
            elf.extend_from_slice(&sym.st_size.to_le_bytes());
        }

        // ── Write strtab ──
        assert_eq!(elf.len(), strtab_offset);
        elf.extend_from_slice(&strtab_data);

        // ── Write shstrtab ──
        assert_eq!(elf.len(), shstrtab_offset);
        elf.extend_from_slice(&shstrtab_data);

        // ── Write section headers ──
        while elf.len() < shdr_offset {
            elf.push(0);
        }

        // SHT_NULL entry
        write_shdr(&mut elf, 0, SHT_NULL, 0, 0, 0, 0, 0, 0, 0, 0);

        // Content sections
        for (i, sec_name) in content_sections.iter().enumerate() {
            let section = self.sections.get(sec_name).unwrap();
            let sh_name = shstrtab.offset_of(sec_name);
            let sh_offset = if section.sh_type == SHT_NOBITS { 0 } else { section_offsets[i] as u64 };
            let sh_size = section.data.len() as u64;
            write_shdr(&mut elf, sh_name, section.sh_type, section.sh_flags,
                       0, sh_offset, sh_size, 0, 0, section.sh_addralign, section.sh_entsize);
        }

        // Rela sections
        rela_idx = 0;
        for (i, sec_name) in content_sections.iter().enumerate() {
            if let Some(section) = self.sections.get(sec_name) {
                if !section.relocs.is_empty() {
                    let rela_name = format!(".rela{}", sec_name);
                    let sh_name = shstrtab.offset_of(&rela_name);
                    let sh_offset = rela_offsets[rela_idx] as u64;
                    let sh_size = (section.relocs.len() * 24) as u64;
                    let sh_link = symtab_shndx as u32;
                    let sh_info = (i + 1) as u32;
                    write_shdr(&mut elf, sh_name, SHT_RELA, SHF_INFO_LINK,
                               0, sh_offset, sh_size, sh_link, sh_info, 8, 24);
                    rela_idx += 1;
                }
            }
        }

        // .symtab
        let symtab_name = shstrtab.offset_of(".symtab");
        let strtab_shndx = symtab_shndx + 1;
        write_shdr(&mut elf, symtab_name, SHT_SYMTAB, 0,
                   0, symtab_offset as u64, symtab_size as u64,
                   strtab_shndx as u32, first_global_idx as u32, 8, 24);

        // .strtab
        let strtab_name = shstrtab.offset_of(".strtab");
        write_shdr(&mut elf, strtab_name, SHT_STRTAB, 0,
                   0, strtab_offset as u64, strtab_data.len() as u64, 0, 0, 1, 0);

        // .shstrtab
        let shstrtab_name = shstrtab.offset_of(".shstrtab");
        write_shdr(&mut elf, shstrtab_name, SHT_STRTAB, 0,
                   0, shstrtab_offset as u64, shstrtab_data.len() as u64, 0, 0, 1, 0);

        // Write to file
        std::fs::write(output_path, &elf)
            .map_err(|e| format!("failed to write ELF file: {}", e))?;

        Ok(())
    }

    fn build_symbol_table(&mut self) {
        // Collect local labels that are referenced by relocations (e.g., synthetic
        // pcrel_hi labels used by pcrel_lo12 relocations). These must appear in
        // the symbol table even though they start with ".L".
        let mut referenced_local_labels: HashMap<String, bool> = HashMap::new();
        for sec in self.sections.values() {
            for reloc in &sec.relocs {
                if reloc.symbol_name.starts_with(".L") || reloc.symbol_name.starts_with(".l") {
                    referenced_local_labels.insert(reloc.symbol_name.clone(), true);
                }
            }
        }

        let labels = self.labels.clone();
        for (name, (section, offset)) in &labels {
            if (name.starts_with(".L") || name.starts_with(".l")) && !referenced_local_labels.contains_key(name) {
                continue;
            }

            let binding = if self.weak_symbols.contains_key(name) {
                STB_WEAK
            } else if self.global_symbols.contains_key(name) {
                STB_GLOBAL
            } else {
                STB_LOCAL
            };

            let sym_type = self.symbol_types.get(name).copied().unwrap_or(STT_NOTYPE);
            let size = self.symbol_sizes.get(name).copied().unwrap_or(0);
            let visibility = self.symbol_visibility.get(name).copied().unwrap_or(STV_DEFAULT);

            self.symbols.push(ElfSymbol {
                name: name.clone(),
                value: *offset,
                size,
                binding,
                sym_type,
                visibility,
                section_name: section.clone(),
            });
        }

        // Add undefined symbols
        let mut referenced: HashMap<String, bool> = HashMap::new();
        for sec in self.sections.values() {
            for reloc in &sec.relocs {
                if !reloc.symbol_name.starts_with(".L") && !reloc.symbol_name.starts_with(".l") {
                    referenced.insert(reloc.symbol_name.clone(), true);
                }
            }
        }

        let defined: HashMap<String, bool> = self.symbols.iter()
            .map(|s| (s.name.clone(), true))
            .collect();

        for (name, _) in &referenced {
            if !defined.contains_key(name) {
                let binding = if self.weak_symbols.contains_key(name) {
                    STB_WEAK
                } else {
                    STB_GLOBAL
                };
                let sym_type = self.symbol_types.get(name).copied().unwrap_or(STT_NOTYPE);
                let visibility = self.symbol_visibility.get(name).copied().unwrap_or(STV_DEFAULT);

                self.symbols.push(ElfSymbol {
                    name: name.clone(),
                    value: 0,
                    size: 0,
                    binding,
                    sym_type,
                    visibility,
                    section_name: "*UND*".to_string(),
                });
            }
        }
    }

    fn find_symbol_index(&self, name: &str, sym_entries: &[SymEntry], strtab: &StringTable, content_sections: &[String]) -> u32 {
        for (i, sec_name) in content_sections.iter().enumerate() {
            if sec_name == name {
                return (i + 1) as u32;
            }
        }

        let name_offset = strtab.offset_of(name);
        for (i, entry) in sym_entries.iter().enumerate() {
            if entry.st_name == name_offset && entry.st_info & 0xF != STT_SECTION {
                return i as u32;
            }
        }

        0
    }
}

// ── Helper functions ──────────────────────────────────────────────────

fn write_shdr(
    buf: &mut Vec<u8>,
    sh_name: u32, sh_type: u32, sh_flags: u64,
    sh_addr: u64, sh_offset: u64, sh_size: u64,
    sh_link: u32, sh_info: u32, sh_addralign: u64, sh_entsize: u64,
) {
    buf.extend_from_slice(&sh_name.to_le_bytes());
    buf.extend_from_slice(&sh_type.to_le_bytes());
    buf.extend_from_slice(&sh_flags.to_le_bytes());
    buf.extend_from_slice(&sh_addr.to_le_bytes());
    buf.extend_from_slice(&sh_offset.to_le_bytes());
    buf.extend_from_slice(&sh_size.to_le_bytes());
    buf.extend_from_slice(&sh_link.to_le_bytes());
    buf.extend_from_slice(&sh_info.to_le_bytes());
    buf.extend_from_slice(&sh_addralign.to_le_bytes());
    buf.extend_from_slice(&sh_entsize.to_le_bytes());
}

struct SymEntry {
    st_name: u32,
    st_info: u8,
    st_other: u8,
    st_shndx: u16,
    st_value: u64,
    st_size: u64,
}

struct StringTable {
    data: Vec<u8>,
    offsets: HashMap<String, u32>,
}

impl StringTable {
    fn new() -> Self {
        Self {
            data: vec![0],
            offsets: HashMap::new(),
        }
    }

    fn add(&mut self, s: &str) -> u32 {
        if let Some(&offset) = self.offsets.get(s) {
            return offset;
        }
        if s.is_empty() {
            return 0;
        }
        let offset = self.data.len() as u32;
        self.data.extend_from_slice(s.as_bytes());
        self.data.push(0);
        self.offsets.insert(s.to_string(), offset);
        offset
    }

    fn offset_of(&self, s: &str) -> u32 {
        self.offsets.get(s).copied().unwrap_or(0)
    }

    fn data(&self) -> Vec<u8> {
        self.data.clone()
    }
}

fn section_index(section_name: &str, content_sections: &[String]) -> u16 {
    if section_name == "*COM*" {
        0xFFF2u16 // SHN_COMMON
    } else if section_name == "*UND*" || section_name.is_empty() {
        0u16 // SHN_UNDEF
    } else {
        content_sections.iter().position(|s| s == section_name)
            .map(|i| (i + 1) as u16)
            .unwrap_or(0)
    }
}

fn default_section_flags(name: &str) -> u64 {
    if name == ".text" || name.starts_with(".text.") {
        SHF_ALLOC | SHF_EXECINSTR
    } else if name == ".data" || name.starts_with(".data.") {
        SHF_ALLOC | SHF_WRITE
    } else if name == ".bss" || name.starts_with(".bss.") {
        SHF_ALLOC | SHF_WRITE
    } else if name == ".rodata" || name.starts_with(".rodata.") {
        SHF_ALLOC
    } else if name.starts_with(".note") {
        SHF_ALLOC
    } else if name.starts_with(".tdata") {
        SHF_ALLOC | SHF_WRITE | SHF_TLS
    } else if name.starts_with(".tbss") {
        SHF_ALLOC | SHF_WRITE | SHF_TLS
    } else if name.starts_with(".init") || name.starts_with(".fini") {
        SHF_ALLOC | SHF_EXECINSTR
    } else {
        0
    }
}

fn parse_section_directive(args: &str) -> (String, String, String) {
    let parts: Vec<&str> = args.split(',').collect();
    let name = parts[0].trim().to_string();
    let flags = if parts.len() > 1 {
        parts[1].trim().trim_matches('"').to_string()
    } else {
        String::new()
    };
    let sec_type = if parts.len() > 2 {
        parts[2].trim().to_string()
    } else {
        // Default type based on section name
        if name == ".bss" || name.starts_with(".bss.") || name.starts_with(".tbss") {
            "@nobits".to_string()
        } else {
            "@progbits".to_string()
        }
    };
    (name, flags, sec_type)
}

fn parse_data_value(s: &str) -> Result<i64, String> {
    let s = s.trim();
    if s.is_empty() {
        return Ok(0);
    }

    let (negative, s) = if s.starts_with('-') {
        (true, &s[1..])
    } else {
        (false, s)
    };

    let val = if s.starts_with("0x") || s.starts_with("0X") {
        u64::from_str_radix(&s[2..], 16)
            .map_err(|e| format!("invalid hex: {}: {}", s, e))?
    } else {
        s.parse::<u64>()
            .map_err(|e| format!("invalid integer: {}: {}", s, e))?
    };

    if negative {
        Ok(-(val as i64))
    } else {
        Ok(val as i64)
    }
}

fn parse_string_literal(s: &str) -> Result<String, String> {
    let s = s.trim();
    if !s.starts_with('"') || !s.ends_with('"') {
        return Err(format!("expected string literal: {}", s));
    }
    let inner = &s[1..s.len() - 1];
    let mut result = String::new();
    let mut chars = inner.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('0') => result.push('\0'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some(c) if c.is_ascii_digit() => {
                    let mut octal = String::new();
                    octal.push(c);
                    while octal.len() < 3 {
                        if let Some(&next) = chars.peek() {
                            if next.is_ascii_digit() && next <= '7' {
                                octal.push(chars.next().unwrap());
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                    if let Ok(val) = u8::from_str_radix(&octal, 8) {
                        result.push(val as char);
                    }
                }
                Some('x') => {
                    let mut hex = String::new();
                    while hex.len() < 2 {
                        if let Some(&next) = chars.peek() {
                            if next.is_ascii_hexdigit() {
                                hex.push(chars.next().unwrap());
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                    if let Ok(val) = u8::from_str_radix(&hex, 16) {
                        result.push(val as char);
                    }
                }
                Some(c) => {
                    result.push('\\');
                    result.push(c);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }
    Ok(result)
}

fn is_symbol_ref(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    let first = s.chars().next().unwrap();
    if first.is_ascii_digit() || first == '-' {
        return false;
    }
    // It's a symbol reference if it starts with a letter, underscore, or dot
    first.is_ascii_alphabetic() || first == '_' || first == '.'
}

/// Find the position of the '-' operator in a symbol difference expression
/// like "sym_a - sym_b". Returns None if no valid symbol difference is found.
/// Skips any '-' at position 0 (negative number), and handles '.' in symbol names.
fn find_symbol_diff_minus(expr: &str) -> Option<usize> {
    // Walk through the expression looking for ' - ' or '-' between two symbol-like tokens
    let bytes = expr.as_bytes();
    let len = bytes.len();
    let mut i = 1; // Start at 1 to skip leading '-' (negative)

    while i < len {
        if bytes[i] == b'-' {
            // Check if left side looks like the end of a symbol name
            let left_char = bytes[i - 1];
            let left_ok = left_char.is_ascii_alphanumeric() || left_char == b'_' || left_char == b'.' || left_char == b' ';

            // Check if right side looks like the start of a symbol name (possibly after whitespace)
            let right_start = expr[i + 1..].trim_start();
            if !right_start.is_empty() {
                let right_char = right_start.as_bytes()[0];
                let right_ok = right_char.is_ascii_alphabetic() || right_char == b'_' || right_char == b'.';
                if left_ok && right_ok {
                    return Some(i);
                }
            }
        }
        i += 1;
    }

    None
}

fn parse_symbol_addend(s: &str) -> (String, i64) {
    if let Some(plus_pos) = s.find('+') {
        let sym = s[..plus_pos].trim().to_string();
        let off: i64 = s[plus_pos + 1..].trim().parse().unwrap_or(0);
        (sym, off)
    } else if let Some(minus_pos) = s.find('-') {
        if minus_pos > 0 {
            let sym = s[..minus_pos].trim().to_string();
            let off_str = &s[minus_pos..];
            let off: i64 = off_str.parse().unwrap_or(0);
            (sym, off)
        } else {
            (s.to_string(), 0)
        }
    } else {
        (s.to_string(), 0)
    }
}
