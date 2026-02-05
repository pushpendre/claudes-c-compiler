//! Shared linker infrastructure for all backends.
//!
//! This module extracts the duplicated linker code that was copied across x86,
//! ARM, RISC-V, and (partially) i686 backends. It provides:
//!
//! - **ELF64 object parser**: `parse_elf64_object()` replaces near-identical
//!   `parse_object()` functions in x86, ARM, and RISC-V linkers.
//! - **Shared library parser**: `parse_shared_library_symbols()` and `parse_soname()`
//!   for extracting dynamic symbols from .so files.
//! - **Archive loading**: `load_archive_members()` and `member_resolves_undefined()`
//!   for iterative archive resolution (the --start-group algorithm).
//! - **Section mapping**: `map_section_name()` for input-to-output section mapping.
//! - **DynStrTab**: Dynamic string table builder for dynamic linking.
//! - **GNU hash**: `build_gnu_hash()` for .gnu.hash section generation.
//! - **Program header writer**: `write_phdr64()` is already in elf.rs.
//!
//! Each backend linker still handles its own:
//! - Architecture-specific relocation application
//! - PLT/GOT layout (different instruction sequences per arch)
//! - ELF header emission (different e_machine, base addresses)
//! - Dynamic linking specifics (version tables, etc.)

use std::collections::{HashMap, HashSet};
use std::path::Path;

use crate::backend::elf::{
    ELF_MAGIC, ELFCLASS64, ELFDATA2LSB,
    ET_REL, ET_DYN,
    SHT_NULL, SHT_PROGBITS, SHT_SYMTAB, SHT_STRTAB, SHT_RELA, SHT_REL,
    SHT_DYNAMIC, SHT_NOBITS, SHT_DYNSYM, SHT_GROUP,
    SHT_GNU_VERSYM, SHT_GNU_VERDEF,
    SHF_WRITE, SHF_ALLOC, SHF_EXECINSTR, SHF_TLS, SHF_EXCLUDE,
    STB_LOCAL, STB_GLOBAL, STB_WEAK,
    STT_SECTION, STT_FILE,
    SHN_UNDEF, SHN_COMMON,
    PT_DYNAMIC,
    DT_NULL, DT_SONAME, DT_SYMTAB, DT_STRTAB, DT_STRSZ,
    DT_GNU_HASH, DT_VERSYM,
    read_u16, read_u32, read_u64, read_i64, read_cstr,
    parse_archive_members, parse_thin_archive_members, is_thin_archive,
    parse_linker_script_entries, LinkerScriptEntry,
};

// ── ELF64 object file types ──────────────────────────────────────────────
//
// These types are used by x86, ARM, and RISC-V linkers. The i686 linker uses
// its own ELF32 types since field widths differ (u32 vs u64).

/// Parsed ELF64 section header.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Elf64Section {
    pub name_idx: u32,
    pub name: String,
    pub sh_type: u32,
    pub flags: u64,
    pub addr: u64,
    pub offset: u64,
    pub size: u64,
    pub link: u32,
    pub info: u32,
    pub addralign: u64,
    pub entsize: u64,
}

/// Parsed ELF64 symbol.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Elf64Symbol {
    pub name_idx: u32,
    pub name: String,
    pub info: u8,
    pub other: u8,
    pub shndx: u16,
    pub value: u64,
    pub size: u64,
}

#[allow(dead_code)]
impl Elf64Symbol {
    pub fn binding(&self) -> u8 { self.info >> 4 }
    pub fn sym_type(&self) -> u8 { self.info & 0xf }
    pub fn visibility(&self) -> u8 { self.other & 0x3 }
    pub fn is_undefined(&self) -> bool { self.shndx == SHN_UNDEF }
    pub fn is_global(&self) -> bool { self.binding() == STB_GLOBAL }
    pub fn is_weak(&self) -> bool { self.binding() == STB_WEAK }
    pub fn is_local(&self) -> bool { self.binding() == STB_LOCAL }
}

/// Parsed ELF64 relocation with addend (RELA).
#[derive(Debug, Clone)]
pub struct Elf64Rela {
    pub offset: u64,
    pub sym_idx: u32,
    pub rela_type: u32,
    pub addend: i64,
}

/// Parsed ELF64 object file (.o).
#[derive(Debug)]
pub struct Elf64Object {
    pub sections: Vec<Elf64Section>,
    pub symbols: Vec<Elf64Symbol>,
    pub section_data: Vec<Vec<u8>>,
    /// Relocations indexed by the section they apply to.
    pub relocations: Vec<Vec<Elf64Rela>>,
    pub source_name: String,
}

/// Dynamic symbol from a shared library (.so).
#[derive(Debug, Clone)]
pub struct DynSymbol {
    pub name: String,
    pub info: u8,
    pub value: u64,
    pub size: u64,
    /// GLIBC version string for this symbol (e.g. "GLIBC_2.3"), if any.
    pub version: Option<String>,
    /// Whether this is the default version (@@GLIBC_x.y vs @GLIBC_x.y).
    #[allow(dead_code)]
    pub is_default_ver: bool,
}

#[allow(dead_code)]
impl DynSymbol {
    pub fn sym_type(&self) -> u8 { self.info & 0xf }
}

// ── ELF64 object parsing ─────────────────────────────────────────────────
//
// This single function replaces the near-identical parse_object() functions
// in x86/linker/elf.rs, arm/linker/elf.rs, and riscv/linker/elf_read.rs.
// The only parameter that differed was the expected e_machine value.

/// Parse an ELF64 relocatable object file (.o).
///
/// `expected_machine` is the ELF e_machine value to validate (e.g., EM_X86_64,
/// EM_AARCH64, EM_RISCV). Pass 0 to skip machine validation.
pub fn parse_elf64_object(data: &[u8], source_name: &str, expected_machine: u16) -> Result<Elf64Object, String> {
    if data.len() < 64 {
        return Err(format!("{}: file too small for ELF header", source_name));
    }
    if data[0..4] != ELF_MAGIC {
        return Err(format!("{}: not an ELF file", source_name));
    }
    if data[4] != ELFCLASS64 {
        return Err(format!("{}: not 64-bit ELF", source_name));
    }
    if data[5] != ELFDATA2LSB {
        return Err(format!("{}: not little-endian ELF", source_name));
    }

    let e_type = read_u16(data, 16);
    if e_type != ET_REL {
        return Err(format!("{}: not a relocatable object (type={})", source_name, e_type));
    }

    if expected_machine != 0 {
        let e_machine = read_u16(data, 18);
        if e_machine != expected_machine {
            return Err(format!("{}: wrong machine type (expected={}, got={})",
                source_name, expected_machine, e_machine));
        }
    }

    let e_shoff = read_u64(data, 40) as usize;
    let e_shentsize = read_u16(data, 58) as usize;
    let e_shnum = read_u16(data, 60) as usize;
    let e_shstrndx = read_u16(data, 62) as usize;

    if e_shoff == 0 || e_shnum == 0 {
        return Err(format!("{}: no section headers", source_name));
    }

    // Parse section headers
    let mut sections = Vec::with_capacity(e_shnum);
    for i in 0..e_shnum {
        let off = e_shoff + i * e_shentsize;
        if off + e_shentsize > data.len() {
            return Err(format!("{}: section header {} out of bounds", source_name, i));
        }
        sections.push(Elf64Section {
            name_idx: read_u32(data, off),
            name: String::new(),
            sh_type: read_u32(data, off + 4),
            flags: read_u64(data, off + 8),
            addr: read_u64(data, off + 16),
            offset: read_u64(data, off + 24),
            size: read_u64(data, off + 32),
            link: read_u32(data, off + 40),
            info: read_u32(data, off + 44),
            addralign: read_u64(data, off + 48),
            entsize: read_u64(data, off + 56),
        });
    }

    // Read section name string table
    if e_shstrndx < sections.len() {
        let shstrtab = &sections[e_shstrndx];
        let strtab_off = shstrtab.offset as usize;
        let strtab_size = shstrtab.size as usize;
        if strtab_off + strtab_size <= data.len() {
            let strtab_data = &data[strtab_off..strtab_off + strtab_size];
            for sec in &mut sections {
                sec.name = read_cstr(strtab_data, sec.name_idx as usize);
            }
        }
    }

    // Read section data
    let mut section_data = Vec::with_capacity(e_shnum);
    for sec in &sections {
        if sec.sh_type == SHT_NOBITS || sec.size == 0 {
            section_data.push(Vec::new());
        } else {
            let start = sec.offset as usize;
            let end = start + sec.size as usize;
            if end > data.len() {
                return Err(format!("{}: section '{}' data out of bounds", source_name, sec.name));
            }
            section_data.push(data[start..end].to_vec());
        }
    }

    // Find symbol table and its string table
    let mut symbols = Vec::new();
    for i in 0..sections.len() {
        if sections[i].sh_type == SHT_SYMTAB {
            let strtab_idx = sections[i].link as usize;
            let strtab_data = if strtab_idx < section_data.len() {
                &section_data[strtab_idx]
            } else {
                continue;
            };
            let sym_data = &section_data[i];
            let sym_count = sym_data.len() / 24; // sizeof(Elf64_Sym) = 24
            for j in 0..sym_count {
                let off = j * 24;
                if off + 24 > sym_data.len() {
                    break;
                }
                let name_idx = read_u32(sym_data, off);
                symbols.push(Elf64Symbol {
                    name_idx,
                    name: read_cstr(strtab_data, name_idx as usize),
                    info: sym_data[off + 4],
                    other: sym_data[off + 5],
                    shndx: read_u16(sym_data, off + 6),
                    value: read_u64(sym_data, off + 8),
                    size: read_u64(sym_data, off + 16),
                });
            }
            break;
        }
    }

    // Parse relocations - index by the section they apply to
    let mut relocations = vec![Vec::new(); e_shnum];
    for i in 0..sections.len() {
        if sections[i].sh_type == SHT_RELA {
            let target_sec = sections[i].info as usize;
            let rela_data = &section_data[i];
            let rela_count = rela_data.len() / 24; // sizeof(Elf64_Rela) = 24
            let mut relas = Vec::with_capacity(rela_count);
            for j in 0..rela_count {
                let off = j * 24;
                if off + 24 > rela_data.len() {
                    break;
                }
                let r_info = read_u64(rela_data, off + 8);
                relas.push(Elf64Rela {
                    offset: read_u64(rela_data, off),
                    sym_idx: (r_info >> 32) as u32,
                    rela_type: (r_info & 0xffffffff) as u32,
                    addend: read_i64(rela_data, off + 16),
                });
            }
            if target_sec < relocations.len() {
                relocations[target_sec] = relas;
            }
        }
    }

    Ok(Elf64Object {
        sections,
        symbols,
        section_data,
        relocations,
        source_name: source_name.to_string(),
    })
}

// ── Shared library parsing ───────────────────────────────────────────────

/// Extract dynamic symbols from a shared library (.so) file.
///
/// Reads the .dynsym section to find exported symbols. Used by x86 and RISC-V
/// linkers for dynamic linking resolution.
pub fn parse_shared_library_symbols(data: &[u8], lib_name: &str) -> Result<Vec<DynSymbol>, String> {
    if data.len() < 64 {
        return Err(format!("{}: file too small for ELF header", lib_name));
    }
    if data[0..4] != ELF_MAGIC {
        return Err(format!("{}: not an ELF file", lib_name));
    }
    if data[4] != ELFCLASS64 || data[5] != ELFDATA2LSB {
        return Err(format!("{}: not 64-bit little-endian ELF", lib_name));
    }

    let e_type = read_u16(data, 16);
    if e_type != ET_DYN {
        return Err(format!("{}: not a shared library (type={})", lib_name, e_type));
    }

    let e_shoff = read_u64(data, 40) as usize;
    let e_shentsize = read_u16(data, 58) as usize;
    let e_shnum = read_u16(data, 60) as usize;

    // Try section headers first (the standard approach)
    if e_shoff != 0 && e_shnum != 0 {
        let mut sections = Vec::with_capacity(e_shnum);
        for i in 0..e_shnum {
            let off = e_shoff + i * e_shentsize;
            if off + e_shentsize > data.len() {
                break;
            }
            sections.push((
                read_u32(data, off + 4),  // sh_type
                read_u64(data, off + 24), // offset
                read_u64(data, off + 32), // size
                read_u32(data, off + 40), // link
            ));
        }

        // Locate .gnu.version (SHT_GNU_VERSYM) and .gnu.verdef (SHT_GNU_VERDEF) sections
        let mut versym_shdr: Option<(usize, usize)> = None;  // (offset, size)
        let mut verdef_shdr: Option<(usize, usize, usize)> = None; // (offset, size, link)
        for &(sh_type, offset, size, link) in &sections {
            if sh_type == SHT_GNU_VERSYM {
                versym_shdr = Some((offset as usize, size as usize));
            } else if sh_type == SHT_GNU_VERDEF {
                verdef_shdr = Some((offset as usize, size as usize, link as usize));
            }
        }

        // Parse version definitions to build index -> version string mapping
        let mut ver_names: std::collections::HashMap<u16, String> = std::collections::HashMap::new();
        if let Some((vd_off, vd_size, vd_link)) = verdef_shdr {
            // Get the string table for verdef (typically the dynstr)
            let vd_strtab = if vd_link < sections.len() {
                let (_, s_off, s_sz, _) = sections[vd_link];
                let s_off = s_off as usize;
                let s_sz = s_sz as usize;
                if s_off + s_sz <= data.len() { &data[s_off..s_off + s_sz] } else { &[] as &[u8] }
            } else {
                &[] as &[u8]
            };

            let mut pos = vd_off;
            let end = vd_off + vd_size;
            while pos < end && pos + 20 <= data.len() {
                let vd_ndx = read_u16(data, pos + 4);
                let vd_cnt = read_u16(data, pos + 6);
                let vd_aux = read_u32(data, pos + 12) as usize;
                let vd_next = read_u32(data, pos + 16) as usize;

                // First verdaux entry has the version name
                if vd_cnt > 0 {
                    let aux_pos = pos + vd_aux;
                    if aux_pos + 8 <= data.len() {
                        let vda_name = read_u32(data, aux_pos) as usize;
                        if vda_name < vd_strtab.len() {
                            let name = read_cstr(vd_strtab, vda_name);
                            ver_names.insert(vd_ndx, name);
                        }
                    }
                }

                if vd_next == 0 { break; }
                pos += vd_next;
            }
        }

        // Find .dynsym and its string table
        for i in 0..sections.len() {
            let (sh_type, offset, size, link) = sections[i];
            if sh_type == SHT_DYNSYM {
                let strtab_idx = link as usize;
                if strtab_idx >= sections.len() { continue; }
                let (_, str_off, str_size, _) = sections[strtab_idx];
                let str_off = str_off as usize;
                let str_size = str_size as usize;
                if str_off + str_size > data.len() { continue; }
                let strtab = &data[str_off..str_off + str_size];

                let sym_off = offset as usize;
                let sym_size = size as usize;
                if sym_off + sym_size > data.len() { continue; }
                let sym_data = &data[sym_off..sym_off + sym_size];
                let sym_count = sym_data.len() / 24;

                let mut symbols = Vec::new();
                for j in 1..sym_count {
                    let off = j * 24;
                    if off + 24 > sym_data.len() { break; }
                    let name_idx = read_u32(sym_data, off) as usize;
                    let info = sym_data[off + 4];
                    let shndx = read_u16(sym_data, off + 6);
                    let value = read_u64(sym_data, off + 8);
                    let size = read_u64(sym_data, off + 16);

                    if shndx == SHN_UNDEF { continue; }

                    // Check .gnu.version: if the hidden bit (0x8000) is set and
                    // the version index is >= 2, this is a non-default version
                    // (symbol@VERSION, not symbol@@VERSION). Such symbols should
                    // not be available for linking, matching GNU ld behavior.
                    if let Some((vs_off, vs_size)) = versym_shdr {
                        if vs_size >= sym_count * 2 && vs_off + vs_size <= data.len() {
                            let ver_entry = vs_off + j * 2;
                            let raw_ver = read_u16(data, ver_entry);
                            let hidden = raw_ver & 0x8000 != 0;
                            let ver_idx = raw_ver & 0x7fff;
                            if hidden && ver_idx >= 2 {
                                continue;
                            }
                        }
                    }

                    let name = read_cstr(strtab, name_idx);
                    if name.is_empty() { continue; }

                    // Look up version for this symbol from .gnu.version table
                    let (version, is_default_ver) = if let Some((vs_off, _vs_size)) = versym_shdr {
                        let vs_entry = vs_off + j * 2;
                        if vs_entry + 2 <= data.len() {
                            let raw_ver = read_u16(data, vs_entry);
                            let hidden = raw_ver & 0x8000 != 0;
                            let ver_idx = raw_ver & 0x7fff;
                            if ver_idx >= 2 {
                                (ver_names.get(&ver_idx).cloned(), !hidden)
                            } else {
                                (None, !hidden)
                            }
                        } else {
                            (None, true)
                        }
                    } else {
                        (None, true)
                    };

                    symbols.push(DynSymbol { name, info, value, size, version, is_default_ver });
                }
                return Ok(symbols);
            }
        }
    }

    // Fallback: use PT_DYNAMIC program header to find DT_SYMTAB/DT_STRTAB.
    // This handles shared libraries without section headers (e.g., our own
    // emitted .so files, or stripped libraries).
    parse_shared_library_symbols_from_phdrs(data, lib_name)
}

/// Parse dynamic symbols using program headers (PT_DYNAMIC) instead of section headers.
///
/// When a shared library has no section headers (e_shoff == 0), we can still find
/// the dynamic symbol table by:
/// 1. Locating PT_DYNAMIC in the program header table
/// 2. Reading DT_SYMTAB, DT_STRTAB, DT_STRSZ from the dynamic section
/// 3. Determining symtab size from DT_GNU_HASH (number of symbols) or by
///    scanning until we hit the strtab address
fn parse_shared_library_symbols_from_phdrs(data: &[u8], lib_name: &str) -> Result<Vec<DynSymbol>, String> {
    let e_phoff = read_u64(data, 32) as usize;
    let e_phentsize = read_u16(data, 54) as usize;
    let e_phnum = read_u16(data, 56) as usize;

    if e_phoff == 0 || e_phnum == 0 {
        return Err(format!("{}: no program headers and no section headers", lib_name));
    }

    // Find PT_DYNAMIC
    let mut dyn_offset = 0usize;
    let mut dyn_size = 0usize;
    for i in 0..e_phnum {
        let ph = e_phoff + i * e_phentsize;
        if ph + e_phentsize > data.len() { break; }
        let p_type = read_u32(data, ph);
        if p_type == PT_DYNAMIC {
            dyn_offset = read_u64(data, ph + 8) as usize;
            dyn_size = read_u64(data, ph + 32) as usize;
            break;
        }
    }

    if dyn_offset == 0 {
        return Err(format!("{}: no PT_DYNAMIC segment found", lib_name));
    }

    // Read dynamic entries to find DT_SYMTAB, DT_STRTAB, DT_STRSZ, DT_GNU_HASH, DT_VERSYM
    let mut symtab_addr: u64 = 0;
    let mut strtab_addr: u64 = 0;
    let mut strsz: u64 = 0;
    let mut gnu_hash_addr: u64 = 0;
    let mut versym_addr: u64 = 0;

    let mut pos = dyn_offset;
    let dyn_end = dyn_offset + dyn_size;
    while pos + 16 <= dyn_end && pos + 16 <= data.len() {
        let tag = read_i64(data, pos);
        let val = read_u64(data, pos + 8);
        match tag {
            x if x == DT_NULL => break,
            x if x == DT_SYMTAB => symtab_addr = val,
            x if x == DT_STRTAB => strtab_addr = val,
            x if x == DT_STRSZ => strsz = val,
            x if x == DT_GNU_HASH => gnu_hash_addr = val,
            x if x == DT_VERSYM => versym_addr = val,
            _ => {}
        }
        pos += 16;
    }

    if symtab_addr == 0 || strtab_addr == 0 {
        return Err(format!("{}: missing DT_SYMTAB or DT_STRTAB in dynamic section", lib_name));
    }

    // For shared libraries with base address 0 (PIC), the DT_ values are
    // virtual addresses. We need to convert them to file offsets.
    // For our emitted .so files, vaddr == file offset (base_addr = 0 and
    // segments are identity-mapped). For system .so files loaded at higher
    // addresses, we'd need to use the PT_LOAD mappings. Since we primarily
    // need this for our own .so output, use identity mapping and also try
    // PT_LOAD-based translation.
    let symtab_file_offset = vaddr_to_file_offset(data, e_phoff, e_phentsize, e_phnum, symtab_addr);
    let strtab_file_offset = vaddr_to_file_offset(data, e_phoff, e_phentsize, e_phnum, strtab_addr);

    if strtab_file_offset + strsz as usize > data.len() {
        return Err(format!("{}: strtab extends beyond file", lib_name));
    }
    let strtab = &data[strtab_file_offset..strtab_file_offset + strsz as usize];

    // Determine number of dynamic symbols. We can get this from .gnu.hash
    // (the symoffset + number of hashed symbols), or by scanning symbols
    // until we reach the strtab address.
    let sym_count = if gnu_hash_addr != 0 {
        let gnu_hash_file_offset = vaddr_to_file_offset(data, e_phoff, e_phentsize, e_phnum, gnu_hash_addr);
        count_dynsyms_from_gnu_hash(data, gnu_hash_file_offset)
    } else {
        // Fallback: symtab ends where strtab begins (they're typically adjacent)
        let sym_size = if strtab_file_offset > symtab_file_offset {
            strtab_file_offset - symtab_file_offset
        } else {
            // Can't determine size; try a reasonable max
            1024 * 24
        };
        sym_size / 24
    };

    // Resolve versym file offset if DT_VERSYM was found
    let versym_file_offset = if versym_addr != 0 {
        vaddr_to_file_offset(data, e_phoff, e_phentsize, e_phnum, versym_addr)
    } else {
        0
    };

    let mut symbols = Vec::new();
    for j in 1..sym_count {
        let off = symtab_file_offset + j * 24;
        if off + 24 > data.len() { break; }
        let name_idx = read_u32(data, off) as usize;
        let info = data[off + 4];
        let shndx = read_u16(data, off + 6);
        let value = read_u64(data, off + 8);
        let size = read_u64(data, off + 16);

        if shndx == SHN_UNDEF { continue; }

        // Check versym: skip non-default (hidden) versioned symbols
        if versym_addr != 0 {
            let ver_entry = versym_file_offset + j * 2;
            if ver_entry + 2 <= data.len() {
                let raw_ver = read_u16(data, ver_entry);
                let hidden = raw_ver & 0x8000 != 0;
                let ver_idx = raw_ver & 0x7fff;
                if hidden && ver_idx >= 2 {
                    continue;
                }
            }
        }

        let name = read_cstr(strtab, name_idx);
        if name.is_empty() { continue; }

        symbols.push(DynSymbol { name, info, value, size, version: None, is_default_ver: true });
    }

    Ok(symbols)
}

/// Convert a virtual address to a file offset using PT_LOAD program headers.
fn vaddr_to_file_offset(
    data: &[u8], e_phoff: usize, e_phentsize: usize, e_phnum: usize, vaddr: u64,
) -> usize {
    use crate::backend::elf::PT_LOAD;
    for i in 0..e_phnum {
        let ph = e_phoff + i * e_phentsize;
        if ph + e_phentsize > data.len() { break; }
        let p_type = read_u32(data, ph);
        if p_type != PT_LOAD { continue; }
        let p_offset = read_u64(data, ph + 8);
        let p_vaddr = read_u64(data, ph + 16);
        let p_filesz = read_u64(data, ph + 32);
        if vaddr >= p_vaddr && vaddr < p_vaddr + p_filesz {
            return (p_offset + (vaddr - p_vaddr)) as usize;
        }
    }
    // If no PT_LOAD matches, assume identity mapping (vaddr == file offset)
    vaddr as usize
}

/// Count the total number of dynamic symbols from a .gnu.hash section.
///
/// The .gnu.hash header contains symoffset (first hashed symbol index).
/// We scan the hash chains to find the highest symbol index, then add 1.
fn count_dynsyms_from_gnu_hash(data: &[u8], offset: usize) -> usize {
    if offset + 16 > data.len() { return 0; }
    let nbuckets = read_u32(data, offset) as usize;
    let symoffset = read_u32(data, offset + 4) as usize;
    let bloom_size = read_u32(data, offset + 8) as usize;

    let buckets_off = offset + 16 + bloom_size * 8;
    let chains_off = buckets_off + nbuckets * 4;

    if buckets_off + nbuckets * 4 > data.len() { return symoffset; }

    // Find the maximum bucket value (highest starting symbol index)
    let mut max_sym = symoffset;
    for i in 0..nbuckets {
        let bucket_val = read_u32(data, buckets_off + i * 4) as usize;
        if bucket_val >= max_sym {
            // Walk the chain from this bucket to find the last symbol
            let mut idx = bucket_val;
            loop {
                let chain_pos = chains_off + (idx - symoffset) * 4;
                if chain_pos + 4 > data.len() { break; }
                let chain_val = read_u32(data, chain_pos);
                if idx + 1 > max_sym { max_sym = idx + 1; }
                if chain_val & 1 != 0 { break; } // end of chain
                idx += 1;
            }
        }
    }

    max_sym
}

/// Get the SONAME from a shared library's .dynamic section.
///
/// Tries section headers first, then falls back to program headers (PT_DYNAMIC)
/// for shared libraries that lack section headers (e.g., our own emitted .so files).
pub fn parse_soname(data: &[u8]) -> Option<String> {
    if data.len() < 64 || data[0..4] != ELF_MAGIC {
        return None;
    }


    let e_shoff = read_u64(data, 40) as usize;
    let e_shentsize = read_u16(data, 58) as usize;
    let e_shnum = read_u16(data, 60) as usize;

    // Try section headers first
    if e_shoff != 0 && e_shnum != 0 {
        for i in 0..e_shnum {
            let off = e_shoff + i * e_shentsize;
            if off + 64 > data.len() { break; }
            let sh_type = read_u32(data, off + 4);
            if sh_type == SHT_DYNAMIC {
                let dyn_off = read_u64(data, off + 24) as usize;
                let dyn_size = read_u64(data, off + 32) as usize;
                let link = read_u32(data, off + 40) as usize;

                let str_sec_off = e_shoff + link * e_shentsize;
                if str_sec_off + 64 > data.len() { return None; }
                let str_off = read_u64(data, str_sec_off + 24) as usize;
                let str_size = read_u64(data, str_sec_off + 32) as usize;
                if str_off + str_size > data.len() { return None; }
                let strtab = &data[str_off..str_off + str_size];

                let mut pos = dyn_off;
                while pos + 16 <= dyn_off + dyn_size && pos + 16 <= data.len() {
                    let tag = read_i64(data, pos);
                    let val = read_u64(data, pos + 8);
                    if tag == DT_NULL { break; }
                    if tag == DT_SONAME {
                        return Some(read_cstr(strtab, val as usize));
                    }
                    pos += 16;
                }
                return None;
            }
        }
        return None;
    }

    // Fallback: use program headers (PT_DYNAMIC) to find the dynamic section
    let e_phoff = read_u64(data, 32) as usize;
    let e_phentsize = read_u16(data, 54) as usize;
    let e_phnum = read_u16(data, 56) as usize;

    if e_phoff == 0 || e_phnum == 0 { return None; }

    // Find PT_DYNAMIC
    let mut dyn_file_offset = 0usize;
    let mut dyn_filesz = 0usize;
    for i in 0..e_phnum {
        let ph = e_phoff + i * e_phentsize;
        if ph + e_phentsize > data.len() { break; }
        let p_type = read_u32(data, ph);
        if p_type == PT_DYNAMIC {
            dyn_file_offset = read_u64(data, ph + 8) as usize;
            dyn_filesz = read_u64(data, ph + 32) as usize;
            break;
        }
    }

    if dyn_file_offset == 0 { return None; }

    // First pass: find DT_STRTAB and DT_SONAME offset
    let mut strtab_addr: u64 = 0;
    let mut strsz: u64 = 0;
    let mut soname_offset: Option<u64> = None;

    let mut pos = dyn_file_offset;
    let dyn_end = dyn_file_offset + dyn_filesz;
    while pos + 16 <= dyn_end && pos + 16 <= data.len() {
        let tag = read_i64(data, pos);
        let val = read_u64(data, pos + 8);
        match tag {
            x if x == DT_NULL => break,
            x if x == DT_STRTAB => strtab_addr = val,
            x if x == DT_STRSZ => strsz = val,
            x if x == DT_SONAME => soname_offset = Some(val),
            _ => {}
        }
        pos += 16;
    }

    if strtab_addr == 0 || soname_offset.is_none() { return None; }

    let strtab_file_off = vaddr_to_file_offset(data, e_phoff, e_phentsize, e_phnum, strtab_addr);
    let name_off = soname_offset.unwrap() as usize;
    if strtab_file_off + name_off >= data.len() { return None; }
    if strsz > 0 && strtab_file_off + strsz as usize <= data.len() {
        let strtab = &data[strtab_file_off..strtab_file_off + strsz as usize];
        Some(read_cstr(strtab, name_off))
    } else {
        // Best effort: read from strtab_file_off + name_off
        Some(read_cstr(&data[strtab_file_off..], name_off))
    }
}

// ── Section name mapping ─────────────────────────────────────────────────

/// Map an input section name to the standard output section name.
///
/// This is the shared implementation used by all linker backends. Input sections
/// like `.text.foo` are merged into `.text`, `.rodata.bar` into `.rodata`, etc.
/// RISC-V additionally maps `.sdata`/`.sbss` (via `map_section_name_riscv()`).
pub fn map_section_name(name: &str) -> &str {
    if name.starts_with(".text.") || name == ".text" { return ".text"; }
    if name.starts_with(".data.rel.ro") { return ".data.rel.ro"; }
    if name.starts_with(".data.") || name == ".data" { return ".data"; }
    if name.starts_with(".rodata.") || name == ".rodata" { return ".rodata"; }
    if name.starts_with(".bss.") || name == ".bss" { return ".bss"; }
    if name.starts_with(".init_array") { return ".init_array"; }
    if name.starts_with(".fini_array") { return ".fini_array"; }
    if name.starts_with(".tbss.") || name == ".tbss" { return ".tbss"; }
    if name.starts_with(".tdata.") || name == ".tdata" { return ".tdata"; }
    if name.starts_with(".gcc_except_table") { return ".gcc_except_table"; }
    if name.starts_with(".eh_frame") { return ".eh_frame"; }
    if name.starts_with(".note.") { return name; }
    name
}

// ── Dynamic string table ─────────────────────────────────────────────────

/// Dynamic string table builder.
///
/// Used by linkers that produce dynamically-linked executables (x86, i686, RISC-V).
/// Deduplicates strings and tracks offsets for .dynstr section emission.
pub struct DynStrTab {
    data: Vec<u8>,
    offsets: HashMap<String, usize>,
}

impl DynStrTab {
    pub fn new() -> Self {
        Self { data: vec![0], offsets: HashMap::new() }
    }

    pub fn add(&mut self, s: &str) -> usize {
        if s.is_empty() { return 0; }
        if let Some(&off) = self.offsets.get(s) { return off; }
        let off = self.data.len();
        self.data.extend_from_slice(s.as_bytes());
        self.data.push(0);
        self.offsets.insert(s.to_string(), off);
        off
    }

    pub fn get_offset(&self, s: &str) -> usize {
        if s.is_empty() { 0 } else { self.offsets.get(s).copied().unwrap_or(0) }
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.data
    }
}

// ── GNU hash table ───────────────────────────────────────────────────────

/// Compute the GNU hash of a symbol name.
pub fn gnu_hash(name: &[u8]) -> u32 {
    let mut h: u32 = 5381;
    for &b in name {
        h = h.wrapping_mul(33).wrapping_add(b as u32);
    }
    h
}

/// Compute the SysV ELF hash of a symbol name.
pub fn sysv_hash(name: &[u8]) -> u32 {
    let mut h: u32 = 0;
    for &b in name {
        h = (h << 4).wrapping_add(b as u32);
        let g = h & 0xf0000000;
        if g != 0 {
            h ^= g >> 24;
        }
        h &= !g;
    }
    h
}

// ── Shared linker data structures ────────────────────────────────────────
//
// InputSection, OutputSection, and the GlobalSymbolOps trait are shared across
// the x86 and ARM 64-bit linkers. The i686 linker uses ELF32 variants.

/// Reference to one input section placed within an output section.
pub struct InputSection {
    pub object_idx: usize,
    pub section_idx: usize,
    pub output_offset: u64,
    pub size: u64,
}

/// A merged output section in the final executable or shared library.
pub struct OutputSection {
    pub name: String,
    pub sh_type: u32,
    pub flags: u64,
    pub alignment: u64,
    pub inputs: Vec<InputSection>,
    pub data: Vec<u8>,
    pub addr: u64,
    pub file_offset: u64,
    pub mem_size: u64,
}

/// Trait abstracting over backend-specific GlobalSymbol types.
///
/// The x86 linker's GlobalSymbol has dynamic linking fields (is_dynamic, plt_idx,
/// got_idx, copy_reloc, from_lib, version, lib_sym_value) that the ARM linker's
/// version also has (since it now supports dynamic linking too). This trait
/// extracts the common operations needed by shared functions like register_symbols
/// and merge_sections, allowing the same code to work with both backends.
pub trait GlobalSymbolOps: Clone {
    fn is_defined(&self) -> bool;
    fn is_dynamic(&self) -> bool;
    fn info(&self) -> u8;
    fn section_idx(&self) -> u16;
    fn value(&self) -> u64;
    fn size(&self) -> u64;
    fn new_defined(obj_idx: usize, sym: &Elf64Symbol) -> Self;
    fn new_common(obj_idx: usize, sym: &Elf64Symbol) -> Self;
    fn new_undefined(sym: &Elf64Symbol) -> Self;
    fn set_common_bss(&mut self, bss_offset: u64);
}

// ── Shared linker functions ─────────────────────────────────────────────

/// Merge input sections from all objects into output sections.
///
/// Groups input sections by mapped name (e.g., `.text.foo` -> `.text`),
/// computes output offsets with proper alignment, and sorts output sections
/// by permission profile: RO -> Exec -> RW(progbits) -> RW(nobits).
pub fn merge_sections_elf64(
    objects: &[Elf64Object], output_sections: &mut Vec<OutputSection>,
    section_map: &mut HashMap<(usize, usize), (usize, u64)>,
) {
    let no_dead = HashSet::new();
    merge_sections_elf64_gc(objects, output_sections, section_map, &no_dead);
}

/// Merge input sections into output sections, optionally skipping dead sections.
///
/// When `dead_sections` is non-empty (from --gc-sections), sections in the set
/// are excluded from the output, effectively garbage-collecting unreferenced code.
pub fn merge_sections_elf64_gc(
    objects: &[Elf64Object], output_sections: &mut Vec<OutputSection>,
    section_map: &mut HashMap<(usize, usize), (usize, u64)>,
    dead_sections: &HashSet<(usize, usize)>,
) {
    let mut output_map: HashMap<String, usize> = HashMap::new();

    for obj_idx in 0..objects.len() {
        for sec_idx in 0..objects[obj_idx].sections.len() {
            let sec = &objects[obj_idx].sections[sec_idx];
            if sec.flags & SHF_ALLOC == 0 { continue; }
            if matches!(sec.sh_type, SHT_NULL | SHT_STRTAB | SHT_SYMTAB | SHT_RELA | SHT_REL | SHT_GROUP) { continue; }
            if sec.flags & SHF_EXCLUDE != 0 { continue; }
            if !dead_sections.is_empty() && dead_sections.contains(&(obj_idx, sec_idx)) { continue; }

            let output_name = map_section_name(&sec.name).to_string();
            let alignment = sec.addralign.max(1);

            let out_idx = if let Some(&idx) = output_map.get(&output_name) {
                if alignment > output_sections[idx].alignment {
                    output_sections[idx].alignment = alignment;
                }
                idx
            } else {
                let idx = output_sections.len();
                output_map.insert(output_name.clone(), idx);
                output_sections.push(OutputSection {
                    name: output_name, sh_type: sec.sh_type, flags: sec.flags,
                    alignment, inputs: Vec::new(), data: Vec::new(),
                    addr: 0, file_offset: 0, mem_size: 0,
                });
                idx
            };

            if sec.sh_type == SHT_PROGBITS { output_sections[out_idx].sh_type = SHT_PROGBITS; }
            output_sections[out_idx].flags |= sec.flags & (SHF_WRITE | SHF_EXECINSTR | SHF_ALLOC | SHF_TLS);
            output_sections[out_idx].inputs.push(InputSection {
                object_idx: obj_idx, section_idx: sec_idx, output_offset: 0, size: sec.size,
            });
        }
    }

    for out_sec in output_sections.iter_mut() {
        let mut off: u64 = 0;
        for input in &mut out_sec.inputs {
            let a = objects[input.object_idx].sections[input.section_idx].addralign.max(1);
            off = (off + a - 1) & !(a - 1);
            input.output_offset = off;
            off += input.size;
        }
        out_sec.mem_size = off;
    }

    for (out_idx, out_sec) in output_sections.iter().enumerate() {
        for input in &out_sec.inputs {
            section_map.insert((input.object_idx, input.section_idx), (out_idx, input.output_offset));
        }
    }

    // Sort: RO -> Exec -> RW(progbits) -> RW(nobits)
    let len = output_sections.len();
    let mut opts: Vec<Option<OutputSection>> = output_sections.drain(..).map(Some).collect();
    let mut sort_indices: Vec<usize> = (0..len).collect();
    sort_indices.sort_by_key(|&i| {
        let sec = opts[i].as_ref().unwrap();
        let is_exec = sec.flags & SHF_EXECINSTR != 0;
        let is_write = sec.flags & SHF_WRITE != 0;
        let is_nobits = sec.sh_type == SHT_NOBITS;
        if is_exec { (1u32, is_nobits as u32) }
        else if !is_write { (0, is_nobits as u32) }
        else { (2, is_nobits as u32) }
    });

    let mut index_remap: HashMap<usize, usize> = HashMap::new();
    for (new_idx, &old_idx) in sort_indices.iter().enumerate() {
        index_remap.insert(old_idx, new_idx);
    }
    for &old_idx in &sort_indices {
        output_sections.push(opts[old_idx].take().unwrap());
    }

    let old_map: Vec<_> = section_map.drain().collect();
    for ((obj_idx, sec_idx), (old_out_idx, off)) in old_map {
        if let Some(&new_out_idx) = index_remap.get(&old_out_idx) {
            section_map.insert((obj_idx, sec_idx), (new_out_idx, off));
        }
    }
}

/// Allocate SHN_COMMON symbols into the .bss output section.
pub fn allocate_common_symbols_elf64<G: GlobalSymbolOps>(
    globals: &mut HashMap<String, G>, output_sections: &mut Vec<OutputSection>,
) {
    let common_syms: Vec<(String, u64, u64)> = globals.iter()
        .filter(|(_, sym)| sym.section_idx() == SHN_COMMON && sym.is_defined())
        .map(|(name, sym)| (name.clone(), sym.value().max(1), sym.size())).collect();
    if common_syms.is_empty() { return; }

    let bss_idx = output_sections.iter().position(|s| s.name == ".bss").unwrap_or_else(|| {
        let idx = output_sections.len();
        output_sections.push(OutputSection {
            name: ".bss".to_string(), sh_type: SHT_NOBITS,
            flags: SHF_ALLOC | SHF_WRITE, alignment: 1,
            inputs: Vec::new(), data: Vec::new(),
            addr: 0, file_offset: 0, mem_size: 0,
        });
        idx
    });

    let mut bss_off = output_sections[bss_idx].mem_size;
    for (name, alignment, size) in &common_syms {
        let a = (*alignment).max(1);
        bss_off = (bss_off + a - 1) & !(a - 1);
        if let Some(sym) = globals.get_mut(name) {
            sym.set_common_bss(bss_off);
        }
        if *alignment > output_sections[bss_idx].alignment {
            output_sections[bss_idx].alignment = *alignment;
        }
        bss_off += size;
    }
    output_sections[bss_idx].mem_size = bss_off;
}

/// Register symbols from an object file into the global symbol table.
///
/// Handles defined symbols, COMMON symbols, and undefined references.
/// For defined symbols, a GLOBAL definition replaces a WEAK one.
/// The `should_replace_extra` callback allows x86's linker to also check
/// `is_dynamic` when deciding whether to replace an existing symbol.
pub fn register_symbols_elf64<G: GlobalSymbolOps>(
    obj_idx: usize,
    obj: &Elf64Object,
    globals: &mut HashMap<String, G>,
    should_replace_extra: fn(existing: &G) -> bool,
) {
    for sym in &obj.symbols {
        if sym.sym_type() == STT_SECTION || sym.sym_type() == STT_FILE { continue; }
        if sym.name.is_empty() || sym.is_local() { continue; }

        let is_defined = !sym.is_undefined() && sym.shndx != SHN_COMMON;

        if is_defined {
            let should_replace = match globals.get(&sym.name) {
                None => true,
                Some(e) => !e.is_defined() || should_replace_extra(e)
                    || (e.info() >> 4 == STB_WEAK && sym.is_global()),
            };
            if should_replace {
                globals.insert(sym.name.clone(), G::new_defined(obj_idx, sym));
            }
        } else if sym.shndx == SHN_COMMON {
            let should_insert = match globals.get(&sym.name) {
                None => true,
                Some(e) => !e.is_defined(),
            };
            if should_insert {
                globals.insert(sym.name.clone(), G::new_common(obj_idx, sym));
            }
        } else if !globals.contains_key(&sym.name) {
            globals.insert(sym.name.clone(), G::new_undefined(sym));
        }
    }
}

// ── Archive and file loading ────────────────────────────────────────────

/// Check if an archive member defines any currently-undefined, non-dynamic symbol.
fn member_resolves_undefined_generic<G: GlobalSymbolOps>(
    obj: &Elf64Object, globals: &HashMap<String, G>,
) -> bool {
    for sym in &obj.symbols {
        if sym.is_undefined() || sym.is_local() { continue; }
        if sym.sym_type() == STT_SECTION || sym.sym_type() == STT_FILE { continue; }
        if sym.name.is_empty() { continue; }
        if let Some(existing) = globals.get(&sym.name) {
            if !existing.is_defined() && !existing.is_dynamic() { return true; }
        }
    }
    false
}

/// Iterative archive member resolution (the --start-group algorithm).
///
/// Given a list of parsed archive member objects, pull in members that define
/// any currently-undefined global symbol. Repeat until no more progress.
fn resolve_archive_members<G: GlobalSymbolOps>(
    member_objects: &mut Vec<Elf64Object>,
    objects: &mut Vec<Elf64Object>,
    globals: &mut HashMap<String, G>,
    should_replace_extra: fn(&G) -> bool,
) {
    let mut changed = true;
    while changed {
        changed = false;
        let mut i = 0;
        while i < member_objects.len() {
            if member_resolves_undefined_generic(&member_objects[i], globals) {
                let obj = member_objects.remove(i);
                let obj_idx = objects.len();
                register_symbols_elf64(obj_idx, &obj, globals, should_replace_extra);
                objects.push(obj);
                changed = true;
            } else {
                i += 1;
            }
        }
    }
}

/// Load a regular archive (.a), parsing members and pulling in those that
/// resolve undefined symbols.
pub fn load_archive_elf64<G: GlobalSymbolOps>(
    data: &[u8], archive_path: &str,
    objects: &mut Vec<Elf64Object>, globals: &mut HashMap<String, G>,
    expected_machine: u16, should_replace_extra: fn(&G) -> bool,
) -> Result<(), String> {
    let members = parse_archive_members(data)?;
    let mut member_objects: Vec<Elf64Object> = Vec::new();
    for (name, offset, size) in &members {
        let member_data = &data[*offset..*offset + *size];
        if member_data.len() < 4 || member_data[0..4] != ELF_MAGIC { continue; }
        if expected_machine != 0 && member_data.len() >= 20 {
            let e_machine = read_u16(member_data, 18);
            if e_machine != expected_machine { continue; }
        }
        let full_name = format!("{}({})", archive_path, name);
        if let Ok(obj) = parse_elf64_object(member_data, &full_name, expected_machine) {
            member_objects.push(obj);
        }
    }
    resolve_archive_members(&mut member_objects, objects, globals, should_replace_extra);
    Ok(())
}

/// Load a GNU thin archive. Members are external files referenced by name
/// relative to the archive's directory.
pub fn load_thin_archive_elf64<G: GlobalSymbolOps>(
    data: &[u8], archive_path: &str,
    objects: &mut Vec<Elf64Object>, globals: &mut HashMap<String, G>,
    expected_machine: u16, should_replace_extra: fn(&G) -> bool,
) -> Result<(), String> {
    let member_names = parse_thin_archive_members(data)?;
    let archive_dir = Path::new(archive_path)
        .parent()
        .unwrap_or_else(|| Path::new("."));

    let mut member_objects: Vec<Elf64Object> = Vec::new();
    for name in &member_names {
        let member_path = archive_dir.join(name);
        let member_data = std::fs::read(&member_path).map_err(|e| {
            format!("thin archive {}: failed to read member '{}': {}",
                archive_path, member_path.display(), e)
        })?;
        if member_data.len() < 4 || member_data[0..4] != ELF_MAGIC { continue; }
        let full_name = format!("{}({})", archive_path, name);
        if let Ok(obj) = parse_elf64_object(&member_data, &full_name, expected_machine) {
            member_objects.push(obj);
        }
    }
    resolve_archive_members(&mut member_objects, objects, globals, should_replace_extra);
    Ok(())
}

/// Load a file, dispatching by format (archive, thin archive, linker script,
/// shared library, or object file).
///
/// The `on_shared_lib` callback handles shared libraries (.so files). This allows
/// x86 and ARM to handle dynamic symbol extraction differently. Pass a no-op
/// closure for static-only linking.
pub fn load_file_elf64<G: GlobalSymbolOps>(
    path: &str,
    objects: &mut Vec<Elf64Object>,
    globals: &mut HashMap<String, G>,
    expected_machine: u16,
    lib_paths: &[String],
    prefer_static: bool,
    should_replace_extra: fn(&G) -> bool,
    on_shared_lib: &mut dyn FnMut(&str, &[u8]) -> Result<(), String>,
) -> Result<(), String> {
    if std::env::var("LINKER_DEBUG").is_ok() {
        eprintln!("load_file: {}", path);
    }

    let data = std::fs::read(path).map_err(|e| format!("failed to read '{}': {}", path, e))?;

    // Regular archive
    if data.len() >= 8 && &data[0..8] == b"!<arch>\n" {
        return load_archive_elf64(&data, path, objects, globals, expected_machine, should_replace_extra);
    }

    // Thin archive
    if is_thin_archive(&data) {
        return load_thin_archive_elf64(&data, path, objects, globals, expected_machine, should_replace_extra);
    }

    // Not ELF? Try linker script (handles GROUP and INPUT directives)
    if data.len() >= 4 && data[0..4] != ELF_MAGIC {
        if let Ok(text) = std::str::from_utf8(&data) {
            if let Some(entries) = parse_linker_script_entries(text) {
                let script_dir = Path::new(path).parent().map(|p| p.to_string_lossy().to_string());
                for entry in &entries {
                    match entry {
                        LinkerScriptEntry::Path(lib_path) => {
                            if Path::new(lib_path).exists() {
                                load_file_elf64(lib_path, objects, globals, expected_machine, lib_paths, prefer_static, should_replace_extra, on_shared_lib)?;
                            } else if let Some(ref dir) = script_dir {
                                let resolved = format!("{}/{}", dir, lib_path);
                                if Path::new(&resolved).exists() {
                                    load_file_elf64(&resolved, objects, globals, expected_machine, lib_paths, prefer_static, should_replace_extra, on_shared_lib)?;
                                }
                            }
                        }
                        LinkerScriptEntry::Lib(lib_name) => {
                            if let Some(resolved_path) = resolve_lib(lib_name, lib_paths, prefer_static) {
                                load_file_elf64(&resolved_path, objects, globals, expected_machine, lib_paths, prefer_static, should_replace_extra, on_shared_lib)?;
                            }
                        }
                    }
                }
                return Ok(());
            }
        }
        return Err(format!("{}: not a valid ELF object or archive", path));
    }

    // Shared library
    if data.len() >= 18 {
        let e_type = u16::from_le_bytes([data[16], data[17]]);
        if e_type == ET_DYN {
            return on_shared_lib(path, &data);
        }
    }

    // Regular ELF object
    let obj = parse_elf64_object(&data, path, expected_machine)?;
    let obj_idx = objects.len();
    register_symbols_elf64(obj_idx, &obj, globals, should_replace_extra);
    objects.push(obj);
    Ok(())
}

// ── Library resolution helper ─────────────────────────────────────────────

/// Resolve a library name to a path by searching directories.
///
/// Handles both `-l:filename` (exact match) and `-lfoo` (lib prefix search).
/// When `prefer_static` is true, searches for `.a` before `.so`.
pub fn resolve_lib(name: &str, paths: &[String], prefer_static: bool) -> Option<String> {
    if let Some(exact) = name.strip_prefix(':') {
        for dir in paths {
            let p = format!("{}/{}", dir, exact);
            if Path::new(&p).exists() { return Some(p); }
        }
        return None;
    }
    if prefer_static {
        for dir in paths {
            let a = format!("{}/lib{}.a", dir, name);
            if Path::new(&a).exists() { return Some(a); }
            let so = format!("{}/lib{}.so", dir, name);
            if Path::new(&so).exists() { return Some(so); }
        }
    } else {
        for dir in paths {
            let so = format!("{}/lib{}.so", dir, name);
            if Path::new(&so).exists() { return Some(so); }
            let a = format!("{}/lib{}.a", dir, name);
            if Path::new(&a).exists() { return Some(a); }
        }
    }
    None
}

// ── .eh_frame_hdr builder ──────────────────────────────────────────────
//
// Builds the .eh_frame_hdr section needed for stack unwinding.
// This section is pointed to by PT_GNU_EH_FRAME and contains a binary
// search table that maps PC addresses to their FDE entries in .eh_frame.
//
// Format:
//   u8  version          = 1
//   u8  eh_frame_ptr_enc = DW_EH_PE_pcrel | DW_EH_PE_sdata4 (0x1b)
//   u8  fde_count_enc    = DW_EH_PE_udata4 (0x03)
//   u8  table_enc        = DW_EH_PE_datarel | DW_EH_PE_sdata4 (0x3b)
//   i32 eh_frame_ptr     (PC-relative offset to .eh_frame start)
//   u32 fde_count        (number of FDEs in the table)
//   For each FDE:
//     i32 initial_location (relative to eh_frame_hdr start)
//     i32 fde_address      (relative to eh_frame_hdr start)

/// Count the number of FDE entries in an .eh_frame section by scanning structure.
/// This only reads length and CIE_id fields, so it works on unrelocated data.
/// Used during layout to reserve space for .eh_frame_hdr (12 + 8 * count bytes).
pub fn count_eh_frame_fdes(data: &[u8]) -> usize {
    let mut count = 0;
    let mut pos = 0;
    while pos + 4 <= data.len() {
        let length = read_u32_le(data, pos) as u64;
        if length == 0 {
            // Zero terminator from a merged input section; skip it
            pos += 4;
            continue;
        }
        let (actual_length, header_size) = if length == 0xFFFFFFFF {
            if pos + 12 > data.len() { break; }
            (read_u64_le(data, pos + 4), 12usize)
        } else {
            (length, 4usize)
        };
        let entry_data_start = pos + header_size;
        let entry_end = entry_data_start + actual_length as usize;
        if entry_end > data.len() || entry_data_start + 4 > data.len() { break; }
        let cie_id = if length == 0xFFFFFFFF {
            if entry_data_start + 8 > data.len() { break; }
            read_u64_le(data, entry_data_start)
        } else {
            read_u32_le(data, entry_data_start) as u64
        };
        if cie_id != 0 { count += 1; }
        pos = entry_end;
    }
    count
}

/// Build .eh_frame_hdr data from the merged .eh_frame section.
///
/// `eh_frame_data`: the merged .eh_frame section bytes
/// `eh_frame_vaddr`: virtual address where .eh_frame is loaded
/// `eh_frame_hdr_vaddr`: virtual address where .eh_frame_hdr will be loaded
/// `is_64bit`: true for 64-bit ELF, false for 32-bit
///
/// Returns the .eh_frame_hdr section data, or empty vec if parsing fails.
pub fn build_eh_frame_hdr(
    eh_frame_data: &[u8],
    eh_frame_vaddr: u64,
    eh_frame_hdr_vaddr: u64,
    is_64bit: bool,
) -> Vec<u8> {
    // Parse .eh_frame to find all FDEs and their initial_location values
    let fdes = parse_eh_frame_fdes(eh_frame_data, eh_frame_vaddr, is_64bit);

    // Header: 4 bytes + eh_frame_ptr (4 bytes) + fde_count (4 bytes)
    let header_size = 4 + 4 + 4;
    let table_entry_size = 8; // two i32s per entry
    let total_size = header_size + fdes.len() * table_entry_size;
    let mut data = vec![0u8; total_size];

    // Version
    data[0] = 1;
    // eh_frame_ptr encoding: DW_EH_PE_pcrel | DW_EH_PE_sdata4
    data[1] = 0x1b;
    // fde_count encoding: DW_EH_PE_udata4
    data[2] = 0x03;
    // table encoding: DW_EH_PE_datarel | DW_EH_PE_sdata4
    data[3] = 0x3b;

    // eh_frame_ptr: PC-relative offset from &data[4] to eh_frame
    let eh_frame_ptr = eh_frame_vaddr as i64 - (eh_frame_hdr_vaddr as i64 + 4);
    write_i32_le(&mut data, 4, eh_frame_ptr as i32);

    // fde_count
    write_i32_le(&mut data, 8, fdes.len() as i32);

    // Table entries: sorted by initial_location
    // Each entry is (initial_location - eh_frame_hdr_vaddr, fde_address - eh_frame_hdr_vaddr)
    for (i, fde) in fdes.iter().enumerate() {
        let off = header_size + i * table_entry_size;
        let loc_rel = fde.initial_location as i64 - eh_frame_hdr_vaddr as i64;
        let fde_rel = fde.fde_vaddr as i64 - eh_frame_hdr_vaddr as i64;
        write_i32_le(&mut data, off, loc_rel as i32);
        write_i32_le(&mut data, off + 4, fde_rel as i32);
    }

    data
}

/// An FDE entry parsed from .eh_frame
struct EhFrameFde {
    initial_location: u64,
    fde_vaddr: u64,
}

/// Parse .eh_frame section to extract FDE entries.
///
/// Returns a sorted list of FDEs by initial_location.
fn parse_eh_frame_fdes(data: &[u8], base_vaddr: u64, is_64bit: bool) -> Vec<EhFrameFde> {
    let mut fdes = Vec::new();
    let mut pos = 0;

    while pos + 4 <= data.len() {
        let length = read_u32_le(data, pos) as u64;
        if length == 0 {
            // Zero terminator from a merged input section; skip it
            pos += 4;
            continue;
        }

        let is_extended = length == 0xFFFFFFFF;
        let (actual_length, header_size) = if is_extended {
            if pos + 12 > data.len() { break; }
            (read_u64_le(data, pos + 4), 12usize)
        } else {
            (length, 4usize)
        };

        let entry_start = pos;
        let entry_data_start = pos + header_size;
        let entry_end = entry_data_start + actual_length as usize;
        if entry_end > data.len() { break; }

        // CIE_id field (4 or 8 bytes depending on extended)
        if entry_data_start + 4 > data.len() { break; }
        let cie_id = if is_extended {
            if entry_data_start + 8 > data.len() { break; }
            read_u64_le(data, entry_data_start)
        } else {
            read_u32_le(data, entry_data_start) as u64
        };

        // CIE has cie_id == 0; FDE has cie_id != 0 (it's a pointer back to CIE)
        if cie_id != 0 {
            // This is an FDE
            // The CIE_pointer is relative: entry_data_start - cie_id points to the CIE
            let cie_id_field_size = if is_extended { 8 } else { 4 };
            let cie_pos = (entry_data_start as u64).wrapping_sub(cie_id) as usize;

            // Parse the CIE to get the FDE encoding
            let fde_encoding = parse_cie_fde_encoding(data, cie_pos, is_64bit);

            // After CIE_pointer comes: initial_location, address_range, ...
            let iloc_offset = entry_data_start + cie_id_field_size;
            if iloc_offset + 4 > data.len() { pos = entry_end; continue; }

            let fde_vaddr = base_vaddr + entry_start as u64;

            // Decode initial_location based on the CIE's FDE encoding
            let initial_location = decode_eh_pointer(
                data, iloc_offset, fde_encoding,
                base_vaddr + iloc_offset as u64,
                is_64bit,
            );

            if let Some(iloc) = initial_location {
                fdes.push(EhFrameFde {
                    initial_location: iloc,
                    fde_vaddr,
                });
            }
        }

        pos = entry_end;
    }

    // Sort by initial_location for binary search
    fdes.sort_by_key(|f| f.initial_location);
    fdes
}

/// Parse a CIE to extract the FDE pointer encoding (R augmentation).
///
/// Returns the encoding byte, or 0x00 (DW_EH_PE_absptr) if not found.
fn parse_cie_fde_encoding(data: &[u8], cie_pos: usize, _is_64bit: bool) -> u8 {
    if cie_pos + 4 > data.len() { return 0x00; }

    let length = read_u32_le(data, cie_pos) as u64;
    if length == 0 || length == 0xFFFFFFFF { return 0x00; }

    let header_size = 4usize;
    let cie_data_start = cie_pos + header_size;
    let cie_end = cie_data_start + length as usize;
    if cie_end > data.len() { return 0x00; }

    // CIE_id must be 0
    if cie_data_start + 4 > data.len() { return 0x00; }
    let cie_id = read_u32_le(data, cie_data_start);
    if cie_id != 0 { return 0x00; }

    // version (1 byte)
    if cie_data_start + 5 > data.len() { return 0x00; }
    let _version = data[cie_data_start + 4];

    // augmentation string (null-terminated)
    let aug_start = cie_data_start + 5;
    let mut aug_end = aug_start;
    while aug_end < cie_end && data[aug_end] != 0 {
        aug_end += 1;
    }
    if aug_end >= cie_end { return 0x00; }
    let aug_str: Vec<u8> = data[aug_start..aug_end].to_vec();
    let mut cur = aug_end + 1; // skip null terminator

    // code_alignment_factor (ULEB128)
    let (_, n) = read_uleb128(data, cur);
    cur += n;
    // data_alignment_factor (SLEB128)
    let (_, n) = read_sleb128(data, cur);
    cur += n;
    // return_address_register (ULEB128)
    let (_, n) = read_uleb128(data, cur);
    cur += n;

    // Parse augmentation data
    if !aug_str.is_empty() && aug_str[0] == b'z' {
        // Augmentation data length (ULEB128)
        let (aug_data_len, n) = read_uleb128(data, cur);
        cur += n;
        let aug_data_end = cur + aug_data_len as usize;

        // Walk augmentation string after 'z'
        for &ch in &aug_str[1..] {
            if cur >= aug_data_end { break; }
            match ch {
                b'R' => {
                    // FDE encoding
                    if cur < data.len() {
                        return data[cur];
                    }
                    return 0x00;
                }
                b'L' => {
                    // LSDA encoding (skip 1 byte)
                    cur += 1;
                }
                b'P' => {
                    // Personality encoding + pointer
                    if cur >= data.len() { return 0x00; }
                    let enc = data[cur];
                    cur += 1;
                    let ptr_size = eh_pointer_size(enc, _is_64bit);
                    cur += ptr_size;
                }
                b'S' | b'B' => {
                    // Signal frame / has ABI tag - no data
                }
                _ => break,
            }
        }
    }

    // Default: absolute pointer encoding
    0x00
}

/// Decode an eh_frame pointer value based on its encoding.
fn decode_eh_pointer(data: &[u8], offset: usize, encoding: u8, pc: u64, is_64bit: bool) -> Option<u64> {
    if encoding == 0xFF { return None; } // DW_EH_PE_omit

    let base_enc = encoding & 0x0F;
    let rel = encoding & 0x70;

    let (raw_val, _size) = match base_enc {
        0x00 => { // DW_EH_PE_absptr
            if is_64bit {
                if offset + 8 > data.len() { return None; }
                (read_u64_le(data, offset) as i64, 8)
            } else {
                if offset + 4 > data.len() { return None; }
                (read_u32_le(data, offset) as i32 as i64, 4)
            }
        }
        0x01 => { // DW_EH_PE_uleb128
            let (v, _) = read_uleb128(data, offset);
            (v as i64, 0)
        }
        0x02 => { // DW_EH_PE_udata2
            if offset + 2 > data.len() { return None; }
            (u16::from_le_bytes([data[offset], data[offset+1]]) as i64, 2)
        }
        0x03 => { // DW_EH_PE_udata4
            if offset + 4 > data.len() { return None; }
            (read_u32_le(data, offset) as i64, 4)
        }
        0x04 => { // DW_EH_PE_udata8
            if offset + 8 > data.len() { return None; }
            (read_u64_le(data, offset) as i64, 8)
        }
        0x09 => { // DW_EH_PE_sleb128
            let (v, _) = read_sleb128(data, offset);
            (v, 0)
        }
        0x0A => { // DW_EH_PE_sdata2
            if offset + 2 > data.len() { return None; }
            (i16::from_le_bytes([data[offset], data[offset+1]]) as i64, 2)
        }
        0x0B => { // DW_EH_PE_sdata4
            if offset + 4 > data.len() { return None; }
            (read_i32_le(data, offset) as i64, 4)
        }
        0x0C => { // DW_EH_PE_sdata8
            if offset + 8 > data.len() { return None; }
            (read_u64_le(data, offset) as i64, 8)
        }
        _ => return None,
    };

    let base_val = match rel {
        0x00 => 0i64,       // DW_EH_PE_absptr
        0x10 => pc as i64,  // DW_EH_PE_pcrel
        0x20 => 0i64,       // DW_EH_PE_textrel (not commonly used)
        0x30 => 0i64,       // DW_EH_PE_datarel
        _ => 0i64,
    };

    Some((base_val + raw_val) as u64)
}

/// Return the byte size of an encoded pointer.
fn eh_pointer_size(encoding: u8, is_64bit: bool) -> usize {
    match encoding & 0x0F {
        0x00 => if is_64bit { 8 } else { 4 }, // absptr
        0x02 | 0x0A => 2, // udata2/sdata2
        0x03 | 0x0B => 4, // udata4/sdata4
        0x04 | 0x0C => 8, // udata8/sdata8
        _ => 0,
    }
}

fn read_u32_le(data: &[u8], off: usize) -> u32 {
    u32::from_le_bytes([data[off], data[off+1], data[off+2], data[off+3]])
}

fn read_i32_le(data: &[u8], off: usize) -> i32 {
    i32::from_le_bytes([data[off], data[off+1], data[off+2], data[off+3]])
}

fn read_u64_le(data: &[u8], off: usize) -> u64 {
    u64::from_le_bytes([
        data[off], data[off+1], data[off+2], data[off+3],
        data[off+4], data[off+5], data[off+6], data[off+7],
    ])
}

fn write_i32_le(data: &mut [u8], off: usize, val: i32) {
    let b = val.to_le_bytes();
    data[off..off+4].copy_from_slice(&b);
}

fn read_uleb128(data: &[u8], mut off: usize) -> (u64, usize) {
    let start = off;
    let mut result = 0u64;
    let mut shift = 0;
    loop {
        if off >= data.len() { return (result, off - start); }
        let byte = data[off];
        off += 1;
        result |= ((byte & 0x7F) as u64) << shift;
        if byte & 0x80 == 0 { break; }
        shift += 7;
    }
    (result, off - start)
}

fn read_sleb128(data: &[u8], mut off: usize) -> (i64, usize) {
    let start = off;
    let mut result = 0i64;
    let mut shift = 0;
    let mut byte;
    loop {
        if off >= data.len() { return (result, off - start); }
        byte = data[off];
        off += 1;
        result |= ((byte & 0x7F) as i64) << shift;
        shift += 7;
        if byte & 0x80 == 0 { break; }
    }
    if shift < 64 && byte & 0x40 != 0 {
        result |= -(1i64 << shift);
    }
    (result, off - start)
}
