//! ELF32 linker for i686.
//!
//! Reads relocatable ELF32 object files, resolves symbols, applies i386
//! relocations, and emits a dynamically-linked ELF32 executable.
//!
//! Shared ELF helpers are imported from `crate::backend::elf`.

use std::collections::{HashMap, HashSet};
use std::path::Path;

use crate::backend::elf::{
    ELF_MAGIC, ELFCLASS32, ELFDATA2LSB, EV_CURRENT,
    // Types: u16
    ET_EXEC, ET_DYN, ET_REL, EM_386,
    // Program header types: u32
    PT_LOAD, PT_DYNAMIC, PT_INTERP, PT_PHDR, PT_TLS,
    PT_GNU_STACK,
    // Section header types: u32
    SHT_NULL, SHT_PROGBITS, SHT_SYMTAB, SHT_STRTAB, SHT_RELA,
    SHT_NOBITS, SHT_REL, SHT_DYNSYM, SHT_GROUP,
    SHT_INIT_ARRAY, SHT_FINI_ARRAY,
    // Symbol binding/type: u8
    STB_LOCAL, STB_GLOBAL, STB_WEAK,
    STT_OBJECT, STT_FUNC, STT_SECTION, STT_FILE, STT_TLS, STT_GNU_IFUNC,
    STV_DEFAULT,
    // Special section indices: u16
    SHN_UNDEF, SHN_ABS, SHN_COMMON,
    // Segment flags: u32
    PF_X, PF_W, PF_R,
    // Read helpers
    read_u16, read_u32, read_cstr, read_i32,
    parse_archive_members,
    LinkerSymbolAddresses, get_standard_linker_symbols,
};

// ── ELF32-specific constants ──────────────────────────────────────────────────
// These either differ in type (i32 vs i64 for DT_*) or aren't in the shared module.

// Section header types not in shared module
const SHT_NOTE: u32 = 7;
const SHT_GNU_HASH: u32 = 0x6ffffff6;
const SHT_GNU_VERSYM: u32 = 0x6fffffff;
const SHT_GNU_VERNEED: u32 = 0x6ffffffe;

// Section flags (i686 uses u32 instead of shared module's u64)
const SHF_WRITE: u32 = 0x1;
const SHF_ALLOC: u32 = 0x2;
const SHF_EXECINSTR: u32 = 0x4;
const SHF_MERGE: u32 = 0x10;
const SHF_STRINGS: u32 = 0x20;
const SHF_INFO_LINK: u32 = 0x40;
const SHF_GROUP: u32 = 0x200;
const SHF_TLS: u32 = 0x400;

// i386 relocation types
const R_386_NONE: u32 = 0;
const R_386_32: u32 = 1;
const R_386_PC32: u32 = 2;
const R_386_GOT32: u32 = 3;
const R_386_PLT32: u32 = 4;
const R_386_GOTOFF: u32 = 9;
const R_386_GOTPC: u32 = 10;
const R_386_TLS_TPOFF: u32 = 14;   // S - TLS_end (negative offset from TP)
const R_386_TLS_IE: u32 = 15;      // GOT entry (absolute addr) containing TPOFF
const R_386_TLS_GOTIE: u32 = 16;   // GOT-relative offset to TLS GOT entry
const R_386_TLS_LE: u32 = 17;      // TLS_end - S (positive offset for @NTPOFF)
const R_386_TLS_GD: u32 = 18;      // General Dynamic TLS
const R_386_TLS_LE_32: u32 = 34;   // TLS_end - S (IE-to-LE, @NTPOFF)
const R_386_TLS_DTPMOD32: u32 = 35; // Module ID for GD TLS
const R_386_TLS_DTPOFF32: u32 = 36; // Offset within TLS block for GD TLS
const R_386_TLS_TPOFF32: u32 = 37; // S + A - TLS_end (negative offset, @TPOFF)
const R_386_IRELATIVE: u32 = 42;
const R_386_GOT32X: u32 = 43;

// Dynamic tags
const DT_NULL: i32 = 0;
const DT_NEEDED: i32 = 1;
const DT_PLTGOT: i32 = 3;
const DT_STRTAB: i32 = 5;
const DT_SYMTAB: i32 = 6;
const DT_STRSZ: i32 = 10;
const DT_SYMENT: i32 = 11;
const DT_INIT: i32 = 12;
const DT_FINI: i32 = 13;
const DT_PLTREL: i32 = 20;
const DT_JMPREL: i32 = 23;
const DT_REL: i32 = 17;
const DT_RELSZ: i32 = 18;
const DT_RELENT: i32 = 19;
const DT_DEBUG: i32 = 21;
const DT_PLTRELSZ: i32 = 2;
const DT_GNU_HASH: i32 = 0x6ffffef5u32 as i32;
const DT_INIT_ARRAY: i32 = 25;
const DT_INIT_ARRAYSZ: i32 = 27;
const DT_FINI_ARRAY: i32 = 26;
const DT_FINI_ARRAYSZ: i32 = 28;
const DT_FLAGS_1: i32 = 0x6ffffffb_u32 as i32;
const DT_VERNEED: i32 = 0x6ffffffe_u32 as i32;
const DT_VERNEEDNUM: i32 = 0x6fffffff_u32 as i32;
const DT_VERSYM: i32 = 0x6ffffff0_u32 as i32;

// PF flags imported from shared module above.

const PAGE_SIZE: u32 = 0x1000;
const BASE_ADDR: u32 = 0x08048000;
const INTERP: &[u8] = b"/lib/ld-linux.so.2\0";


// ── ELF32 structures ────────────────────────────────────────────────────────

#[derive(Clone, Debug)]
struct Elf32Sym {
    name: u32,
    value: u32,
    size: u32,
    info: u8,
    other: u8,
    shndx: u16,
}

impl Elf32Sym {
    fn binding(&self) -> u8 { self.info >> 4 }
    fn sym_type(&self) -> u8 { self.info & 0xf }
    fn visibility(&self) -> u8 { self.other & 3 }
}

#[derive(Clone, Debug)]
struct Elf32Rel {
    offset: u32,
    info: u32,
}

impl Elf32Rel {
    fn sym_idx(&self) -> u32 { self.info >> 8 }
    fn rel_type(&self) -> u32 { self.info & 0xff }
}

#[derive(Clone, Debug)]
struct Elf32Shdr {
    name: u32,
    sh_type: u32,
    flags: u32,
    addr: u32,
    offset: u32,
    size: u32,
    link: u32,
    info: u32,
    addralign: u32,
    entsize: u32,
}

// ── Input object parsing ─────────────────────────────────────────────────────

/// A parsed section from an input .o file.
#[derive(Clone)]
struct InputSection {
    name: String,
    sh_type: u32,
    flags: u32,
    data: Vec<u8>,
    align: u32,
    /// Relocations referencing this section.
    relocations: Vec<(u32, u32, u32, i32)>, // (offset, rel_type, sym_idx_in_input, addend)
    /// Index in the input file's section header table.
    input_index: usize,
    entsize: u32,
    link: u32,
    info: u32,
}

/// A parsed symbol from an input .o file.
#[derive(Clone, Debug)]
struct InputSymbol {
    name: String,
    value: u32,
    size: u32,
    binding: u8,
    sym_type: u8,
    visibility: u8,
    section_index: u16,
}

/// A parsed input file.
struct InputObject {
    sections: Vec<InputSection>,
    symbols: Vec<InputSymbol>,
    filename: String,
}


fn parse_elf32(data: &[u8], filename: &str) -> Result<InputObject, String> {
    if data.len() < 52 {
        return Err(format!("{}: too small for ELF header", filename));
    }
    if data[0..4] != ELF_MAGIC {
        return Err(format!("{}: not an ELF file", filename));
    }
    if data[4] != ELFCLASS32 {
        return Err(format!("{}: not ELF32", filename));
    }
    if data[5] != ELFDATA2LSB {
        return Err(format!("{}: not little-endian", filename));
    }
    let e_type = read_u16(data, 16);
    if e_type != ET_REL {
        return Err(format!("{}: not a relocatable object (type={})", filename, e_type));
    }
    let e_machine = read_u16(data, 18);
    if e_machine != EM_386 {
        return Err(format!("{}: not i386 (machine={})", filename, e_machine));
    }

    let e_shoff = read_u32(data, 32) as usize;
    let e_shentsize = read_u16(data, 46) as usize;
    let e_shnum = read_u16(data, 48) as usize;
    let e_shstrndx = read_u16(data, 50) as usize;

    // Parse section headers
    let mut shdrs = Vec::with_capacity(e_shnum);
    for i in 0..e_shnum {
        let off = e_shoff + i * e_shentsize;
        shdrs.push(Elf32Shdr {
            name: read_u32(data, off),
            sh_type: read_u32(data, off + 4),
            flags: read_u32(data, off + 8),
            addr: read_u32(data, off + 12),
            offset: read_u32(data, off + 16),
            size: read_u32(data, off + 20),
            link: read_u32(data, off + 24),
            info: read_u32(data, off + 28),
            addralign: read_u32(data, off + 32),
            entsize: read_u32(data, off + 36),
        });
    }

    // Read section name string table
    let shstrtab = &shdrs[e_shstrndx];
    let shstrtab_data = &data[shstrtab.offset as usize..(shstrtab.offset + shstrtab.size) as usize];

    // Find symtab and strtab
    let mut symtab_idx = None;
    let mut strtab_data: &[u8] = &[];
    for (i, shdr) in shdrs.iter().enumerate() {
        if shdr.sh_type == SHT_SYMTAB {
            symtab_idx = Some(i);
            let str_idx = shdr.link as usize;
            let str_shdr = &shdrs[str_idx];
            strtab_data = &data[str_shdr.offset as usize..(str_shdr.offset + str_shdr.size) as usize];
        }
    }

    // Parse symbols
    let mut symbols = Vec::new();
    if let Some(si) = symtab_idx {
        let sym_shdr = &shdrs[si];
        let sym_count = sym_shdr.size / sym_shdr.entsize;
        for j in 0..sym_count {
            let off = (sym_shdr.offset + j * sym_shdr.entsize) as usize;
            let st_name = read_u32(data, off);
            let st_value = read_u32(data, off + 4);
            let st_size = read_u32(data, off + 8);
            let st_info = data[off + 12];
            let st_other = data[off + 13];
            let st_shndx = read_u16(data, off + 14);
            symbols.push(InputSymbol {
                name: read_cstr(strtab_data, st_name as usize),
                value: st_value,
                size: st_size,
                binding: st_info >> 4,
                sym_type: st_info & 0xf,
                visibility: st_other & 3,
                section_index: st_shndx,
            });
        }
    }

    // Parse sections with their relocations
    let mut sections = Vec::with_capacity(e_shnum);
    // Build a map of section index -> rel section
    let mut rel_map: HashMap<usize, Vec<usize>> = HashMap::new();
    for (i, shdr) in shdrs.iter().enumerate() {
        if shdr.sh_type == SHT_REL {
            let target = shdr.info as usize;
            rel_map.entry(target).or_default().push(i);
        }
    }

    for (i, shdr) in shdrs.iter().enumerate() {
        let sec_name = read_cstr(shstrtab_data, shdr.name as usize);
        let sec_data = if shdr.sh_type != SHT_NOBITS && shdr.size > 0 {
            data[shdr.offset as usize..(shdr.offset + shdr.size) as usize].to_vec()
        } else {
            vec![0u8; shdr.size as usize]
        };

        // Parse relocations for this section
        let mut relocs = Vec::new();
        if let Some(rel_indices) = rel_map.get(&i) {
            for &ri in rel_indices {
                let rel_shdr = &shdrs[ri];
                let count = rel_shdr.size / rel_shdr.entsize.max(8);
                for j in 0..count {
                    let roff = (rel_shdr.offset + j * rel_shdr.entsize.max(8)) as usize;
                    let r_offset = read_u32(data, roff);
                    let r_info = read_u32(data, roff + 4);
                    let sym_idx = r_info >> 8;
                    let rel_type = r_info & 0xff;
                    // For REL (not RELA), the addend is implicit in the section data
                    let addend = if rel_type != R_386_NONE && (r_offset as usize + 4) <= sec_data.len() {
                        read_i32(&sec_data, r_offset as usize)
                    } else {
                        0
                    };
                    relocs.push((r_offset, rel_type, sym_idx, addend));
                }
            }
        }

        sections.push(InputSection {
            name: sec_name,
            sh_type: shdr.sh_type,
            flags: shdr.flags,
            data: sec_data,
            align: shdr.addralign.max(1),
            relocations: relocs,
            input_index: i,
            entsize: shdr.entsize,
            link: shdr.link,
            info: shdr.info,
        });
    }

    Ok(InputObject {
        sections,
        symbols,
        filename: filename.to_string(),
    })
}

// ── Archive (.a) parsing ─────────────────────────────────────────────────────

fn parse_archive(data: &[u8], _filename: &str) -> Result<Vec<(String, Vec<u8>)>, String> {
    let raw_members = parse_archive_members(data)?;
    let mut members = Vec::new();
    for (name, offset, size) in raw_members {
        let content = &data[offset..offset + size];
        // Accept any member that looks like an ELF object (by magic number).
        // Archive members can have various extensions (.o, .oS, .os, .od, etc.)
        if content.len() >= 4 && content[0..4] == ELF_MAGIC {
            members.push((name, content.to_vec()));
        }
    }
    Ok(members)
}

// ── Dynamic symbol resolution from shared libraries ──────────────────────────

/// Info about a dynamic symbol from a shared library.
struct DynSymInfo {
    name: String,
    sym_type: u8,
    size: u32,
    binding: u8,
    /// GLIBC version string for this symbol (e.g. "GLIBC_2.16"), if any.
    version: Option<String>,
    /// Whether this is the default version (@@GLIBC_x.y vs @GLIBC_x.y).
    is_default_ver: bool,
}

/// Read dynamic symbol info from a shared library ELF file.
fn read_dynsyms(path: &str) -> Result<Vec<DynSymInfo>, String> {
    const SHT_GNU_VERSYM: u32 = 0x6fffffff;
    const SHT_GNU_VERDEF: u32 = 0x6ffffffd;

    let data = std::fs::read(path)
        .map_err(|e| format!("cannot read {}: {}", path, e))?;
    if data.len() < 52 || data[0..4] != ELF_MAGIC || data[4] != ELFCLASS32 {
        return Err(format!("{}: not a valid ELF32 file", path));
    }
    let e_type = read_u16(&data, 16);
    if e_type != ET_DYN {
        return Err(format!("{}: not a shared library (type={})", path, e_type));
    }

    let e_shoff = read_u32(&data, 32) as usize;
    let e_shentsize = read_u16(&data, 46) as usize;
    let e_shnum = read_u16(&data, 48) as usize;
    let e_shstrndx = read_u16(&data, 50) as usize;

    // Read section header string table for section name lookups
    let shstrtab_off_hdr = e_shoff + e_shstrndx * e_shentsize;
    let _shstrtab_data = if shstrtab_off_hdr + 40 <= data.len() {
        let off = read_u32(&data, shstrtab_off_hdr + 16) as usize;
        let sz = read_u32(&data, shstrtab_off_hdr + 20) as usize;
        if off + sz <= data.len() { &data[off..off + sz] } else { &[] as &[u8] }
    } else {
        &[] as &[u8]
    };

    // First pass: find dynsym, versym, verdef sections
    let mut dynsym_idx = None;
    let mut versym_shdr: Option<(usize, usize)> = None; // (offset, size)
    let mut verdef_shdr: Option<(usize, usize, usize)> = None; // (offset, size, link)

    for i in 0..e_shnum {
        let off = e_shoff + i * e_shentsize;
        if off + 40 > data.len() { break; }
        let sh_type = read_u32(&data, off + 4);
        match sh_type {
            SHT_DYNSYM => { dynsym_idx = Some(i); }
            SHT_GNU_VERSYM => {
                let sh_offset = read_u32(&data, off + 16) as usize;
                let sh_size = read_u32(&data, off + 20) as usize;
                versym_shdr = Some((sh_offset, sh_size));
            }
            SHT_GNU_VERDEF => {
                let sh_offset = read_u32(&data, off + 16) as usize;
                let sh_size = read_u32(&data, off + 20) as usize;
                let sh_link = read_u32(&data, off + 24) as usize;
                verdef_shdr = Some((sh_offset, sh_size, sh_link));
            }
            _ => {}
        }
    }

    // Parse version definitions to build index -> version string mapping
    let mut ver_names: std::collections::HashMap<u16, String> = std::collections::HashMap::new();
    if let Some((vd_off, vd_size, vd_link)) = verdef_shdr {
        // Get the string table for verdef
        let vd_str_hdr = e_shoff + vd_link * e_shentsize;
        let vd_strtab = if vd_str_hdr + 40 <= data.len() {
            let s_off = read_u32(&data, vd_str_hdr + 16) as usize;
            let s_sz = read_u32(&data, vd_str_hdr + 20) as usize;
            if s_off + s_sz <= data.len() { &data[s_off..s_off + s_sz] } else { &[] as &[u8] }
        } else {
            &[] as &[u8]
        };

        let mut pos = vd_off;
        let end = vd_off + vd_size;
        while pos < end && pos + 20 <= data.len() {
            let vd_ndx = read_u16(&data, pos + 4);
            let vd_cnt = read_u16(&data, pos + 6);
            let vd_aux = read_u32(&data, pos + 12) as usize;
            let vd_next = read_u32(&data, pos + 16) as usize;

            // First verdaux entry has the version name
            if vd_cnt > 0 {
                let aux_pos = pos + vd_aux;
                if aux_pos + 8 <= data.len() {
                    let vda_name = read_u32(&data, aux_pos) as usize;
                    if vda_name < vd_strtab.len() {
                        let name = read_cstr(vd_strtab, vda_name);
                        ver_names.insert(vd_ndx, name.to_string());
                    }
                }
            }

            if vd_next == 0 { break; }
            pos += vd_next;
        }
    }

    let dynsym_i = match dynsym_idx {
        Some(i) => i,
        None => return Ok(Vec::new()),
    };

    let mut syms = Vec::new();

    let off = e_shoff + dynsym_i * e_shentsize;
    if off + 40 > data.len() { return Ok(syms); }

    let sh_offset = read_u32(&data, off + 16) as usize;
    let sh_size = read_u32(&data, off + 20) as usize;
    let sh_link = read_u32(&data, off + 24) as usize;
    let sh_entsize = read_u32(&data, off + 36) as usize;
    if sh_entsize == 0 { return Ok(syms); }

    // Get the string table for this dynsym section
    let str_off = e_shoff + sh_link * e_shentsize;
    if str_off + 40 > data.len() { return Ok(syms); }
    let str_sh_offset = read_u32(&data, str_off + 16) as usize;
    let str_sh_size = read_u32(&data, str_off + 20) as usize;
    if str_sh_offset + str_sh_size > data.len() { return Ok(syms); }
    let strtab = &data[str_sh_offset..str_sh_offset + str_sh_size];

    let count = sh_size / sh_entsize;
    for j in 0..count {
        let sym_off = sh_offset + j * sh_entsize;
        if sym_off + 16 > data.len() { break; }
        let st_name = read_u32(&data, sym_off) as usize;
        let _st_value = read_u32(&data, sym_off + 4);
        let st_size = read_u32(&data, sym_off + 8);
        let st_info = data[sym_off + 12];
        let st_shndx = read_u16(&data, sym_off + 14);
        // Only exported (defined, global/weak) symbols
        if st_shndx == SHN_UNDEF { continue; }
        let binding = st_info >> 4;
        if binding != STB_GLOBAL && binding != STB_WEAK { continue; }

        // Look up version for this symbol
        let (version, is_default_ver) = if let Some((vs_off, _vs_size)) = versym_shdr {
            let vs_entry = vs_off + j * 2;
            if vs_entry + 2 <= data.len() {
                let raw_ver = read_u16(&data, vs_entry);
                let hidden = raw_ver & 0x8000 != 0;
                let ver_idx = raw_ver & 0x7fff;
                if ver_idx >= 2 {
                    (ver_names.get(&ver_idx).cloned(), !hidden)
                } else {
                    (None, !hidden) // 0=local, 1=global (base)
                }
            } else {
                (None, true)
            }
        } else {
            (None, true)
        };

        if st_name < strtab.len() {
            let end = strtab[st_name..].iter().position(|&b| b == 0)
                .map(|p| st_name + p).unwrap_or(strtab.len());
            let name = String::from_utf8_lossy(&strtab[st_name..end]).into_owned();
            if !name.is_empty() {
                syms.push(DynSymInfo {
                    name,
                    sym_type: st_info & 0xf,
                    size: st_size,
                    binding,
                    version,
                    is_default_ver,
                });
            }
        }
    }

    Ok(syms)
}

// ── Linker state ─────────────────────────────────────────────────────────────

/// Resolved symbol info in the linker.
#[derive(Clone, Debug)]
struct LinkerSymbol {
    /// Final virtual address (filled during layout).
    address: u32,
    /// Size of the symbol.
    size: u32,
    /// Symbol type (STT_*).
    sym_type: u8,
    /// Symbol binding (STB_*).
    binding: u8,
    /// Symbol visibility (STV_*).
    visibility: u8,
    /// Whether this symbol is defined in an input object (vs dynamic).
    is_defined: bool,
    /// Whether this symbol needs a PLT entry (called via PLT).
    needs_plt: bool,
    /// Whether this symbol needs a GOT entry.
    needs_got: bool,
    /// Index into the merged output section, or usize::MAX if dynamic.
    output_section: usize,
    /// Offset within the output section.
    section_offset: u32,
    /// PLT entry index (if needs_plt).
    plt_index: usize,
    /// GOT entry index (if needs_got).
    got_index: usize,
    /// Whether this is a dynamic symbol (from shared lib).
    is_dynamic: bool,
    /// Name of the shared library providing this symbol.
    dynlib: String,
    /// Whether this data symbol needs a copy relocation (R_386_COPY).
    needs_copy: bool,
    /// Address of the copy in BSS (for copy relocs).
    copy_addr: u32,
    /// GLIBC version string for dynamic symbols (e.g. "GLIBC_2.16").
    version: Option<String>,
}

/// A merged output section.
struct OutputSection {
    name: String,
    sh_type: u32,
    flags: u32,
    data: Vec<u8>,
    align: u32,
    /// Virtual address (filled during layout).
    addr: u32,
    /// File offset (filled during layout).
    file_offset: u32,
}

/// Maps (object_index, section_index) -> (output_section_index, offset_in_output).
type SectionMap = HashMap<(usize, usize), (usize, u32)>;

// ── Main link function ───────────────────────────────────────────────────────

/// Entry point for the built-in i686 linker, matching the x86-64 linker API.
///
/// CRT objects, library paths, and needed libraries are resolved by the caller
/// (common.rs), keeping the linker itself focused on ELF linking logic.
pub fn link_builtin(
    object_files: &[&str],
    output_path: &str,
    user_args: &[String],
    lib_paths: &[&str],
    needed_libs: &[&str],
    crt_objects_before: &[&str],
    crt_objects_after: &[&str],
) -> Result<(), String> {
    let is_nostdlib = user_args.iter().any(|a| a == "-nostdlib");
    let is_static = user_args.iter().any(|a| a == "-static");

    // Parse user-provided flags from args
    let mut extra_libs: Vec<String> = Vec::new();
    let mut extra_lib_files: Vec<String> = Vec::new(); // -l:filename (exact filenames)
    let mut extra_lib_paths: Vec<String> = Vec::new();
    let mut extra_objects: Vec<String> = Vec::new();
    let mut _rdynamic = false;

    let mut i = 0;
    while i < user_args.len() {
        let arg = &user_args[i];
        if arg == "-nostdlib" || arg == "-shared" || arg == "-static" || arg == "-r" {
            // handled above
        } else if let Some(libarg) = arg.strip_prefix("-l") {
            if let Some(rest) = libarg.strip_prefix(':') {
                extra_lib_files.push(rest.to_string());
            } else {
                extra_libs.push(libarg.to_string());
            }
        } else if let Some(rest) = arg.strip_prefix("-L") {
            extra_lib_paths.push(rest.to_string());
        } else if arg == "-rdynamic" || arg == "--export-dynamic" {
            _rdynamic = true;
        } else if let Some(wl_args) = arg.strip_prefix("-Wl,") {
            for part in wl_args.split(',') {
                if let Some(libarg) = part.strip_prefix("-l") {
                    if let Some(rest) = libarg.strip_prefix(':') {
                        extra_lib_files.push(rest.to_string());
                    } else {
                        extra_libs.push(libarg.to_string());
                    }
                } else if let Some(rest) = part.strip_prefix("-L") {
                    extra_lib_paths.push(rest.to_string());
                } else if part == "--export-dynamic" || part == "-export-dynamic" {
                    _rdynamic = true;
                }
            }
        } else if !arg.starts_with('-') && Path::new(arg.as_str()).exists() {
            // Bare file path: .o object file, .a static archive, or other input file
            // (e.g., .os, .od extensions used by heatshrink build system)
            extra_objects.push(arg.clone());
        }
        i += 1;
    }

    // Build combined library search paths: user paths (from args) + caller-provided paths
    let all_lib_dirs: Vec<String> = extra_lib_paths.iter().cloned()
        .chain(lib_paths.iter().map(|s| s.to_string()))
        .collect();

    // Collect all objects in link order: CRT before, user objects, CRT after
    let mut all_objects: Vec<String> = Vec::new();

    for path in crt_objects_before {
        if Path::new(path).exists() {
            all_objects.push(path.to_string());
        }
    }

    for obj in object_files {
        all_objects.push(obj.to_string());
    }
    for obj in &extra_objects {
        all_objects.push(obj.clone());
    }

    for path in crt_objects_after {
        if Path::new(path).exists() {
            all_objects.push(path.to_string());
        }
    }

    // Add libc_nonshared.a (contains at_quick_exit, atexit, etc.)
    // This must be linked as a static archive alongside libc.so.6
    if !is_nostdlib && !is_static {
        for dir in lib_paths {
            let path = format!("{}/libc_nonshared.a", dir);
            if Path::new(&path).exists() {
                all_objects.push(path);
                break;
            }
        }
    }

    // Build dynamic symbol set from shared libraries
    // Maps symbol name -> (library soname, symbol type, symbol size, glibc version, is_default)
    let mut dynlib_syms: HashMap<String, (String, u8, u32, Option<String>, bool)> = HashMap::new();
    // Static archives found via -l flags (when no .so is available)
    let mut static_lib_objects: Vec<String> = Vec::new();

    if !is_static {
        // Start with caller-provided needed_libs, then add user-specified ones
        let mut libs_to_scan: Vec<String> = needed_libs.iter().map(|s| s.to_string()).collect();
        libs_to_scan.extend(extra_libs.iter().cloned());

        let all_lib_refs: Vec<&str> = all_lib_dirs.iter().map(|s| s.as_str()).collect();

        for lib in &libs_to_scan {
            // Try to find the shared library file (.so, .so.N, .so.N.N, etc.)
            let mut found = false;
            let so_base = format!("lib{}.so", lib);
            {
                'dir_search: for dir in &all_lib_refs {
                    // Build candidate list: lib{name}.so, then scan for lib{name}.so.N
                    let mut candidates: Vec<String> = vec![format!("{}/{}", dir, so_base)];
                    // Also try common versioned names
                    if let Ok(entries) = std::fs::read_dir(dir) {
                        for entry in entries.flatten() {
                            let fname = entry.file_name().to_string_lossy().into_owned();
                            if fname.starts_with(&so_base) && fname.len() > so_base.len()
                                && fname.as_bytes()[so_base.len()] == b'.'
                            {
                                candidates.push(format!("{}/{}", dir, fname));
                            }
                        }
                    }
                    for cand in &candidates {
                        let real_path = std::fs::canonicalize(cand).ok();
                        let check_path = real_path.as_ref()
                            .map(|p| p.to_string_lossy().into_owned())
                            .unwrap_or(cand.clone());
                        if let Ok(syms) = read_dynsyms(&check_path) {
                            let lib_soname = if lib == "c" { "libc.so.6".to_string() }
                                else if lib == "m" { "libm.so.6".to_string() }
                                else { format!("lib{}.so", lib) };
                            for sym in syms {
                                // Prefer default version (@@) over hidden version (@)
                                let entry = dynlib_syms.entry(sym.name.clone());
                                match entry {
                                    std::collections::hash_map::Entry::Vacant(e) => {
                                        e.insert((lib_soname.clone(), sym.sym_type, sym.size, sym.version, sym.is_default_ver));
                                    }
                                    std::collections::hash_map::Entry::Occupied(mut e) => {
                                        // Replace if new entry is default and old is not
                                        if sym.is_default_ver && !e.get().4 {
                                            e.insert((lib_soname.clone(), sym.sym_type, sym.size, sym.version, sym.is_default_ver));
                                        }
                                    }
                                }
                            }
                            found = true;
                            break 'dir_search;
                        }
                    }
                }
            }
            // If no .so found, try to find a static archive (.a)
            if !found {
                let ar_filename = format!("lib{}.a", lib);
                for dir in &all_lib_refs {
                    let path = format!("{}/{}", dir, ar_filename);
                    if Path::new(&path).exists() {
                        static_lib_objects.push(path);
                        break;
                    }
                }
            }
        }
    }

    // Handle -l flags in static linking mode
    // Include both needed_libs (from compiler driver, e.g. "c") and extra_libs (from user -l flags)
    if is_static {
        let all_lib_refs: Vec<&str> = all_lib_dirs.iter().map(|s| s.as_str()).collect();
        let mut libs_to_scan: Vec<String> = needed_libs.iter().map(|s| s.to_string()).collect();
        libs_to_scan.extend(extra_libs.iter().cloned());
        for lib in &libs_to_scan {
            let ar_filename = format!("lib{}.a", lib);
            for dir in &all_lib_refs {
                let path = format!("{}/{}", dir, ar_filename);
                if Path::new(&path).exists() {
                    static_lib_objects.push(path);
                    break;
                }
            }
        }
    }

    // Handle -l:filename (exact filename search in library paths)
    {
        let all_lib_refs: Vec<&str> = all_lib_dirs.iter().map(|s| s.as_str()).collect();
        for filename in &extra_lib_files {
            for dir in &all_lib_refs {
                let path = format!("{}/{}", dir, filename);
                if Path::new(&path).exists() {
                    if filename.ends_with(".a") || filename.ends_with(".o") {
                        static_lib_objects.push(path);
                    } else {
                        // Assume it's a shared library - try to read dynsyms
                        if !is_static {
                            let real_path = std::fs::canonicalize(&path).ok();
                            let check_path = real_path.as_ref()
                                .map(|p| p.to_string_lossy().into_owned())
                                .unwrap_or(path.clone());
                            if let Ok(syms) = read_dynsyms(&check_path) {
                                let lib_soname = filename.clone();
                                for sym in syms {
                                    let entry = dynlib_syms.entry(sym.name.clone());
                                    match entry {
                                        std::collections::hash_map::Entry::Vacant(e) => {
                                            e.insert((lib_soname.clone(), sym.sym_type, sym.size, sym.version, sym.is_default_ver));
                                        }
                                        std::collections::hash_map::Entry::Occupied(mut e) => {
                                            if sym.is_default_ver && !e.get().4 {
                                                e.insert((lib_soname.clone(), sym.sym_type, sym.size, sym.version, sym.is_default_ver));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        // Also add as a static file (it could be an archive)
                        static_lib_objects.push(path);
                    }
                    break;
                }
            }
        }
    }

    // Add static libraries found via -l flags to the object list
    for lib_path in &static_lib_objects {
        all_objects.push(lib_path.clone());
    }

    // Parse all input objects.  Non-archive objects are added directly.
    // Archive members are kept in a pool and pulled in on demand based on
    // which symbols are undefined, iterating until no more members are needed.
    // This avoids pulling in unused archive members (e.g. BID decimal objects
    // from libgcc.a) that reference unsupported TLS models or add huge bloat.
    let mut inputs: Vec<InputObject> = Vec::new();
    let mut archive_pool: Vec<InputObject> = Vec::new();

    for obj_path in &all_objects {
        let data = std::fs::read(obj_path)
            .map_err(|e| format!("cannot read {}: {}", obj_path, e))?;
        if data.len() >= 8 && &data[0..8] == b"!<arch>\n" {
            // Archive file - parse members into the pool
            let members = parse_archive(&data, obj_path)?;
            for (name, mdata) in members {
                let member_name = format!("{}({})", obj_path, name);
                if let Ok(obj) = parse_elf32(&mdata, &member_name) {
                    archive_pool.push(obj);
                }
            }
        } else {
            inputs.push(parse_elf32(&data, obj_path)?);
        }
    }

    // Demand-driven archive member extraction: pull in members that define
    // symbols referenced (undefined) by already-included objects.  Iterate
    // until stable to handle transitive dependencies.
    {
        // Build set of defined and undefined symbol names from current inputs
        let mut defined: HashSet<String> = HashSet::new();
        let mut undefined: HashSet<String> = HashSet::new();
        for obj in &inputs {
            for sym in &obj.symbols {
                if sym.name.is_empty() || sym.sym_type == STT_FILE || sym.sym_type == STT_SECTION {
                    continue;
                }
                if sym.section_index != SHN_UNDEF {
                    defined.insert(sym.name.clone());
                } else {
                    undefined.insert(sym.name.clone());
                }
            }
        }
        // Remove symbols that are already defined
        undefined.retain(|s| !defined.contains(s));

        let mut changed = true;
        while changed {
            changed = false;
            let mut i = 0;
            while i < archive_pool.len() {
                // Check if this member defines any currently-undefined symbol
                let resolves = archive_pool[i].symbols.iter().any(|sym| {
                    !sym.name.is_empty()
                        && sym.sym_type != STT_FILE
                        && sym.sym_type != STT_SECTION
                        && sym.section_index != SHN_UNDEF
                        && undefined.contains(&sym.name)
                });
                if resolves {
                    let obj = archive_pool.remove(i);
                    // Register this member's definitions and new undefined refs
                    for sym in &obj.symbols {
                        if sym.name.is_empty() || sym.sym_type == STT_FILE || sym.sym_type == STT_SECTION {
                            continue;
                        }
                        if sym.section_index != SHN_UNDEF {
                            defined.insert(sym.name.clone());
                            undefined.remove(&sym.name);
                        } else if !defined.contains(&sym.name) {
                            undefined.insert(sym.name.clone());
                        }
                    }
                    inputs.push(obj);
                    changed = true;
                } else {
                    i += 1;
                }
            }
        }
    }

    // ── Merge sections ───────────────────────────────────────────────────────
    // Merge input sections into output sections, grouped by name.
    // Output sections: .text, .rodata, .data, .bss, .init, .fini,
    //                  .init_array, .fini_array, .eh_frame, .note.*

    let mut output_sections: Vec<OutputSection> = Vec::new();
    let mut section_name_to_idx: HashMap<String, usize> = HashMap::new();
    let mut section_map: SectionMap = HashMap::new();

    // Determine the output section name for an input section
    fn output_section_name(name: &str, flags: u32, sh_type: u32) -> Option<String> {
        // Skip non-allocatable sections, symbol tables, relocation sections, etc.
        if sh_type == SHT_NULL || sh_type == SHT_SYMTAB || sh_type == SHT_STRTAB
            || sh_type == SHT_REL || sh_type == SHT_RELA || sh_type == SHT_GROUP {
            return None;
        }
        // note.GNU-stack is not allocated
        if name == ".note.GNU-stack" {
            return None;
        }
        if name == ".comment" {
            return None;
        }

        // Group by canonical output section name
        if name.starts_with(".text") || name == ".init" || name == ".fini" {
            if name == ".init" { return Some(".init".to_string()); }
            if name == ".fini" { return Some(".fini".to_string()); }
            return Some(".text".to_string());
        }
        if name.starts_with(".rodata") || name.starts_with(".rodata.") {
            return Some(".rodata".to_string());
        }
        if name == ".eh_frame" {
            return Some(".eh_frame".to_string());
        }
        if name == ".tbss" || name.starts_with(".tbss.") {
            return Some(".tbss".to_string());
        }
        if name == ".tdata" || name.starts_with(".tdata.") {
            return Some(".tdata".to_string());
        }
        if flags & SHF_TLS != 0 {
            if sh_type == SHT_NOBITS {
                return Some(".tbss".to_string());
            }
            return Some(".tdata".to_string());
        }
        if name.starts_with(".data") || name.starts_with(".data.") {
            return Some(".data".to_string());
        }
        if name.starts_with(".bss") || name.starts_with(".bss.") || sh_type == SHT_NOBITS {
            return Some(".bss".to_string());
        }
        if name == ".init_array" || name.starts_with(".init_array.") {
            return Some(".init_array".to_string());
        }
        if name == ".fini_array" || name.starts_with(".fini_array.") {
            return Some(".fini_array".to_string());
        }
        if name.starts_with(".note.") && sh_type == SHT_NOTE {
            return Some(".note".to_string());
        }
        if name.starts_with(".tm_clone_table") {
            return Some(".data".to_string());
        }

        // For sections with alloc flag, use as-is
        if flags & SHF_ALLOC != 0 {
            // Default grouping based on flags
            if flags & SHF_EXECINSTR != 0 {
                return Some(".text".to_string());
            }
            if flags & SHF_WRITE != 0 {
                if sh_type == SHT_NOBITS {
                    return Some(".bss".to_string());
                }
                return Some(".data".to_string());
            }
            return Some(".rodata".to_string());
        }

        None // Non-allocated section, skip
    }

    for (obj_idx, obj) in inputs.iter().enumerate() {
        for sec in obj.sections.iter() {
            let out_name = match output_section_name(&sec.name, sec.flags, sec.sh_type) {
                Some(n) => n,
                None => continue,
            };

            let out_idx = if let Some(&idx) = section_name_to_idx.get(&out_name) {
                idx
            } else {
                let idx = output_sections.len();
                let (sh_type, flags) = match out_name.as_str() {
                    ".text" => (SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR),
                    ".rodata" => (SHT_PROGBITS, SHF_ALLOC),
                    ".data" => (SHT_PROGBITS, SHF_ALLOC | SHF_WRITE),
                    ".bss" => (SHT_NOBITS, SHF_ALLOC | SHF_WRITE),
                    ".tdata" => (SHT_PROGBITS, SHF_ALLOC | SHF_WRITE | SHF_TLS),
                    ".tbss" => (SHT_NOBITS, SHF_ALLOC | SHF_WRITE | SHF_TLS),
                    ".init" => (SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR),
                    ".fini" => (SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR),
                    ".init_array" => (SHT_INIT_ARRAY, SHF_ALLOC | SHF_WRITE),
                    ".fini_array" => (SHT_FINI_ARRAY, SHF_ALLOC | SHF_WRITE),
                    ".eh_frame" => (SHT_PROGBITS, SHF_ALLOC),
                    ".note" => (SHT_NOTE, SHF_ALLOC),
                    _ => (sec.sh_type, sec.flags & (SHF_ALLOC | SHF_WRITE | SHF_EXECINSTR)),
                };
                section_name_to_idx.insert(out_name.clone(), idx);
                output_sections.push(OutputSection {
                    name: out_name,
                    sh_type,
                    flags,
                    data: Vec::new(),
                    align: 1,
                    addr: 0,
                    file_offset: 0,
                });
                idx
            };

            let out_sec = &mut output_sections[out_idx];
            // Align within the output section
            let align = sec.align.max(1);
            if align > out_sec.align {
                out_sec.align = align;
            }
            let padding = (align - (out_sec.data.len() as u32 % align)) % align;
            out_sec.data.extend(std::iter::repeat_n(0u8, padding as usize));
            let offset = out_sec.data.len() as u32;

            section_map.insert((obj_idx, sec.input_index), (out_idx, offset));

            if sec.sh_type != SHT_NOBITS {
                out_sec.data.extend_from_slice(&sec.data);
            } else {
                out_sec.data.extend(std::iter::repeat_n(0u8, sec.data.len()));
            }
        }
    }

    // ── Symbol resolution ────────────────────────────────────────────────────
    // Build global symbol table. First pass: collect all definitions.
    // Second pass: resolve undefined references.

    let mut global_symbols: HashMap<String, LinkerSymbol> = HashMap::new();
    // Map (obj_idx, sym_idx) -> resolved symbol name
    let mut sym_resolution: HashMap<(usize, usize), String> = HashMap::new();

    // First pass: collect definitions
    for (obj_idx, obj) in inputs.iter().enumerate() {
        for (sym_idx, sym) in obj.symbols.iter().enumerate() {
            if sym.name.is_empty() { continue; }
            if sym.sym_type == STT_FILE { continue; }
            if sym.sym_type == STT_SECTION { continue; }
            if sym.section_index == SHN_UNDEF { continue; }

            let name = &sym.name;
            let (out_sec_idx, sec_offset) = if sym.section_index != SHN_ABS && sym.section_index != SHN_COMMON {
                section_map.get(&(obj_idx, sym.section_index as usize))
                    .copied()
                    .unwrap_or((usize::MAX, 0))
            } else {
                (usize::MAX, 0)
            };

            let new_sym = LinkerSymbol {
                address: 0,
                size: sym.size,
                sym_type: sym.sym_type,
                binding: sym.binding,
                visibility: sym.visibility,
                is_defined: true,
                needs_plt: false,
                needs_got: false,
                output_section: out_sec_idx,
                section_offset: sec_offset + sym.value,
                plt_index: 0,
                got_index: 0,
                is_dynamic: false,
                dynlib: String::new(),
                needs_copy: false,
                copy_addr: 0,
                version: None,
            };

            match global_symbols.get(name) {
                None => {
                    global_symbols.insert(name.clone(), new_sym);
                }
                Some(existing) => {
                    // Global beats weak, defined beats undefined
                    if (sym.binding == STB_GLOBAL && existing.binding == STB_WEAK)
                        || (!existing.is_defined && new_sym.is_defined)
                    {
                        global_symbols.insert(name.clone(), new_sym);
                    }
                    // Otherwise keep existing definition
                }
            }

            sym_resolution.insert((obj_idx, sym_idx), name.clone());
        }
    }

    // Second pass: resolve undefined references against dynamic libraries
    for (obj_idx, obj) in inputs.iter().enumerate() {
        for (sym_idx, sym) in obj.symbols.iter().enumerate() {
            if sym.name.is_empty() { continue; }
            if sym.sym_type == STT_FILE { continue; }

            let name = &sym.name;
            sym_resolution.insert((obj_idx, sym_idx), name.clone());

            if sym.section_index == SHN_UNDEF {
                if global_symbols.contains_key(name) { continue; }

                // Try to resolve from dynamic libraries
                if let Some((lib, dyn_sym_type, dyn_size, dyn_ver, _is_default)) = dynlib_syms.get(name) {
                    let is_func = *dyn_sym_type == STT_FUNC || *dyn_sym_type == STT_GNU_IFUNC;
                    global_symbols.insert(name.clone(), LinkerSymbol {
                        address: 0,
                        size: *dyn_size,
                        sym_type: *dyn_sym_type,
                        binding: STB_GLOBAL,
                        visibility: STV_DEFAULT,
                        is_defined: false,
                        needs_plt: is_func,
                        needs_got: is_func,
                        output_section: usize::MAX,
                        section_offset: 0,
                        plt_index: 0,
                        got_index: 0,
                        is_dynamic: true,
                        dynlib: lib.clone(),
                        needs_copy: !is_func, // Data symbols need copy relocation
                        copy_addr: 0,
                        version: dyn_ver.clone(),
                    });
                } else {
                    // Track undefined symbols so the check below can catch them.
                    // Weak undefined symbols resolve to 0; strong undefined symbols
                    // will trigger an error if no library provides them.
                    global_symbols.entry(name.clone()).or_insert(LinkerSymbol {
                        address: 0,
                        size: 0,
                        sym_type: sym.sym_type,
                        binding: sym.binding,
                        visibility: STV_DEFAULT,
                        is_defined: false,
                        needs_plt: false,
                        needs_got: false,
                        output_section: usize::MAX,
                        section_offset: 0,
                        plt_index: 0,
                        got_index: 0,
                        is_dynamic: false,
                        dynlib: String::new(),
                        needs_copy: false,
                        copy_addr: 0,
                        version: None,
                    });
                }
            }
        }
    }

    // Resolve section symbols
    for (obj_idx, obj) in inputs.iter().enumerate() {
        for (sym_idx, sym) in obj.symbols.iter().enumerate() {
            if sym.sym_type == STT_SECTION && sym.section_index != SHN_UNDEF {
                let key = (obj_idx, sym_idx);
                // Section symbols reference their section directly
                sym_resolution.insert(key, format!("__section_{}_{}", obj_idx, sym.section_index));
            }
        }
    }

    // Determine which symbols need PLT/GOT entries based on relocations
    for obj in &inputs {
        for sec in &obj.sections {
            for &(_, rel_type, _, _) in &sec.relocations {
                match rel_type {
                    R_386_PLT32 | R_386_GOT32 | R_386_GOT32X | R_386_GOTPC | R_386_GOTOFF
                    | R_386_TLS_GOTIE | R_386_TLS_IE => {
                        // These need GOT/PLT infrastructure
                    }
                    _ => {}
                }
            }
        }
    }

    // Mark symbols that need PLT entries (called via R_386_PLT32 and are dynamic)
    // and symbols that need GOT entries (referenced via R_386_GOT32X, R_386_GOT32)
    for obj in inputs.iter() {
        for sec in &obj.sections {
            for &(_, rel_type, sym_idx, _) in &sec.relocations {
                let sym = if (sym_idx as usize) < obj.symbols.len() {
                    &obj.symbols[sym_idx as usize]
                } else { continue; };

                let name = if sym.sym_type == STT_SECTION {
                    continue; // Section symbols don't need PLT/GOT
                } else {
                    &sym.name
                };

                if name.is_empty() { continue; }

                match rel_type {
                    R_386_PLT32 => {
                        if let Some(gs) = global_symbols.get_mut(name) {
                            if gs.is_dynamic {
                                gs.needs_plt = true;
                                gs.needs_got = true;
                            }
                        }
                    }
                    R_386_GOT32 | R_386_GOT32X => {
                        if let Some(gs) = global_symbols.get_mut(name) {
                            gs.needs_got = true;
                        }
                    }
                    R_386_TLS_GOTIE | R_386_TLS_IE => {
                        if let Some(gs) = global_symbols.get_mut(name) {
                            gs.needs_got = true;
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    // Check for truly undefined symbols
    let undefined: Vec<&String> = global_symbols.iter()
        .filter(|(_, s)| !s.is_defined && !s.is_dynamic && s.binding != STB_WEAK)
        .map(|(n, _)| n)
        .collect();

    // Some special symbols are provided by the linker or are allowed to be
    // undefined in static binaries (e.g. ___tls_get_addr from dead archive members).
    let linker_defined = [
        "_GLOBAL_OFFSET_TABLE_",
        "__ehdr_start",
        "__executable_start",
        "_end", "_edata", "_etext",
        "__bss_start", "__bss_start__",
        "end", "edata", "etext",
        "__end__",
        "__dso_handle",
        "_DYNAMIC",
        "__data_start", "data_start",
        "__init_array_start", "__init_array_end",
        "__fini_array_start", "__fini_array_end",
        "__preinit_array_start", "__preinit_array_end",
        "__rel_iplt_start", "__rel_iplt_end",
        // TLS general-dynamic accessor: referenced by some libc.a members that
        // are unconditionally extracted but never called in static binaries.
        "___tls_get_addr",
    ];

    let truly_undefined: Vec<&&String> = undefined.iter()
        .filter(|n| !linker_defined.contains(&n.as_str()))
        .collect();

    if !truly_undefined.is_empty() {
        return Err(format!("undefined symbols: {}", truly_undefined.iter()
            .map(|s| s.as_str()).collect::<Vec<_>>().join(", ")));
    }

    // ── PLT/GOT construction ─────────────────────────────────────────────────
    // Build PLT (Procedure Linkage Table) and GOT (Global Offset Table)
    // for dynamic symbols.

    let mut plt_symbols: Vec<String> = Vec::new();
    let mut got_symbols: Vec<String> = Vec::new(); // non-PLT GOT entries

    for (name, sym) in &global_symbols {
        if sym.needs_plt {
            plt_symbols.push(name.clone());
        } else if sym.needs_got && !sym.needs_plt {
            got_symbols.push(name.clone());
        }
    }
    plt_symbols.sort();
    got_symbols.sort();

    // Assign PLT/GOT indices
    for (i, name) in plt_symbols.iter().enumerate() {
        if let Some(sym) = global_symbols.get_mut(name) {
            sym.plt_index = i;
            sym.got_index = i; // PLT symbols use first GOT entries (after reserved)
        }
    }
    for (i, name) in got_symbols.iter().enumerate() {
        if let Some(sym) = global_symbols.get_mut(name) {
            sym.got_index = plt_symbols.len() + i;
        }
    }

    let num_plt = plt_symbols.len();
    let num_got_total = plt_symbols.len() + got_symbols.len();

    // ── IFUNC support for static linking ─────────────────────────────────────
    // Collect IFUNC symbols that need IPLT entries and IRELATIVE relocations
    let mut ifunc_symbols: Vec<String> = Vec::new();
    if is_static {
        for (name, sym) in &global_symbols {
            if sym.is_defined && sym.sym_type == STT_GNU_IFUNC {
                ifunc_symbols.push(name.clone());
            }
        }
        ifunc_symbols.sort();
    }
    let num_ifunc = ifunc_symbols.len();

    // ── Layout ───────────────────────────────────────────────────────────────
    // ELF32 header: 52 bytes
    // Program headers: count * 32 bytes
    // Then segments with page alignment

    let ehdr_size: u32 = 52;
    let phdr_size: u32 = 32;

    // Check for TLS sections early (needed for program header count)
    let has_tls_sections = output_sections.iter()
        .any(|s| s.flags & SHF_TLS != 0 && s.flags & SHF_ALLOC != 0);

    // Count program headers we need
    let mut num_phdrs: u32 = 0;
    num_phdrs += 1; // PHDR
    if !is_static { num_phdrs += 1; } // INTERP
    num_phdrs += 1; // LOAD (read-only: headers + interp + dynamic tables)
    num_phdrs += 1; // LOAD (text)
    num_phdrs += 1; // LOAD (rodata)
    num_phdrs += 1; // LOAD (data + bss)
    if !is_static { num_phdrs += 1; } // DYNAMIC
    num_phdrs += 1; // GNU_STACK
    if has_tls_sections { num_phdrs += 1; } // PT_TLS
    // Skip GNU_RELRO for now: it conflicts with lazy PLT binding when
    // .got and .got.plt share the same page.

    let phdrs_total_size = num_phdrs * phdr_size;

    // Now lay out the file. We use a simple approach:
    // Segment 0 (RO): ELF header + PHDRs + INTERP + .gnu.hash + .dynsym + .dynstr + .rel.dyn + .rel.plt
    // Segment 1 (RX): .init + .plt + .text + .fini
    // Segment 2 (RO): .rodata + .eh_frame + .note
    // Segment 3 (RW): .init_array + .fini_array + .dynamic + .got + .got.plt + .data + .bss

    // Build synthetic sections

    // .interp
    let interp_data = INTERP.to_vec();

    // Collect needed shared libraries
    let mut needed_libs: Vec<String> = Vec::new();
    if !is_static && !is_nostdlib {
        needed_libs.push("libc.so.6".to_string());
    }
    // Add libraries from dynamic symbols
    for sym in global_symbols.values() {
        if sym.is_dynamic && !sym.dynlib.is_empty() && !needed_libs.contains(&sym.dynlib) {
            needed_libs.push(sym.dynlib.clone());
        }
    }

    // Build .dynsym, .dynstr, .gnu.hash, .rel.plt, .rel.dyn, .gnu.version, .gnu.version_r
    let mut dynstr = DynStrTab::new();
    let _ = dynstr.add(""); // null entry

    // Add library names to dynstr
    let mut needed_offsets: Vec<u32> = Vec::new();
    for lib in &needed_libs {
        needed_offsets.push(dynstr.add(lib));
    }

    // Build dynsym entries
    // For .gnu.hash, unhashed (undefined) symbols must come before hashed (defined) ones.
    // Order: [null] [PLT imports] [GOT-only imports] [copy-reloc defined symbols]
    let mut dynsym_entries: Vec<Elf32Sym> = Vec::new();
    // Entry 0: null symbol
    dynsym_entries.push(Elf32Sym {
        name: 0, value: 0, size: 0, info: 0, other: 0, shndx: 0,
    });

    // dynsym index map: symbol name -> dynsym index
    let mut dynsym_map: HashMap<String, usize> = HashMap::new();
    let mut dynsym_names: Vec<String> = Vec::new();

    // Add PLT symbols to dynsym (unhashed: undefined imports)
    for name in &plt_symbols {
        let idx = dynsym_entries.len();
        let name_off = dynstr.add(name);
        dynsym_entries.push(Elf32Sym {
            name: name_off,
            value: 0,
            size: 0,
            info: (STB_GLOBAL << 4) | STT_FUNC,
            other: 0,
            shndx: SHN_UNDEF,
        });
        dynsym_map.insert(name.clone(), idx);
        dynsym_names.push(name.clone());
    }

    // Add GOT-only symbols to dynsym (unhashed: undefined imports)
    for name in &got_symbols {
        let idx = dynsym_entries.len();
        let name_off = dynstr.add(name);
        let sym = &global_symbols[name];
        dynsym_entries.push(Elf32Sym {
            name: name_off,
            value: 0,
            size: sym.size,
            info: (sym.binding << 4) | sym.sym_type,
            other: 0,
            shndx: if sym.is_dynamic { SHN_UNDEF } else { 1 }, // placeholder
        });
        dynsym_map.insert(name.clone(), idx);
        dynsym_names.push(name.clone());
    }

    // symoffset = index of first hashed symbol (copy-reloc symbols are defined and
    // must be findable through .gnu.hash so the dynamic linker can redirect references)
    let gnu_hash_symoffset = dynsym_entries.len();

    // Add copy-reloc symbols to dynsym (hashed: defined in this executable)
    let mut copy_syms_for_dynsym: Vec<String> = global_symbols.iter()
        .filter(|(_, s)| s.needs_copy && s.is_dynamic)
        .map(|(n, _)| n.clone())
        .collect();
    copy_syms_for_dynsym.sort();
    for name in &copy_syms_for_dynsym {
        let idx = dynsym_entries.len();
        let name_off = dynstr.add(name);
        let sym = &global_symbols[name];
        dynsym_entries.push(Elf32Sym {
            name: name_off,
            value: 0, // Will be patched after layout
            size: sym.size,
            info: (STB_GLOBAL << 4) | STT_OBJECT,
            other: 0,
            shndx: SHN_UNDEF, // Will be patched after layout
        });
        dynsym_map.insert(name.clone(), idx);
        dynsym_names.push(name.clone());
    }

    drop(dynstr); // dynstr will be rebuilt as dynstr2 with version strings

    // Build .gnu.hash section (consistent with x86-64 and RISC-V linkers)
    let (gnu_hash_data, sorted_indices) = build_gnu_hash_32(&copy_syms_for_dynsym, gnu_hash_symoffset as u32);

    // Reorder the hashed portion of dynsym to match .gnu.hash bucket sort order
    if !sorted_indices.is_empty() {
        let hashed_start = gnu_hash_symoffset; // index in dynsym_entries (1-based due to null)
        let hashed_names_start = hashed_start - 1; // index in dynsym_names (0-based)

        // Save original hashed entries/names
        let orig_entries: Vec<Elf32Sym> = (0..sorted_indices.len())
            .map(|i| dynsym_entries[hashed_start + i].clone())
            .collect();
        let orig_names: Vec<String> = (0..sorted_indices.len())
            .map(|i| dynsym_names[hashed_names_start + i].clone())
            .collect();

        // Apply sorted order
        for (new_pos, &orig_idx) in sorted_indices.iter().enumerate() {
            dynsym_entries[hashed_start + new_pos] = orig_entries[orig_idx].clone();
            dynsym_names[hashed_names_start + new_pos] = orig_names[orig_idx].clone();
        }

        // Rebuild dynsym_map for the reordered hashed symbols
        for (i, name) in dynsym_names[hashed_names_start..].iter().enumerate() {
            dynsym_map.insert(name.clone(), hashed_start + i);
        }
    }

    // Collect all unique version strings needed by dynamic symbols, grouped by library.
    // Maps: library soname -> set of version strings
    let mut lib_versions: std::collections::HashMap<String, std::collections::BTreeSet<String>> = std::collections::HashMap::new();
    for name in &dynsym_names {
        if let Some(gs) = global_symbols.get(name) {
            if gs.is_dynamic {
                // Only add version requirement if the symbol actually has a version.
                // Symbols without a version (e.g. from libraries without versioning)
                // use VER_NDX_GLOBAL and don't need a verneed entry.
                if let Some(ref ver) = gs.version {
                    lib_versions.entry(gs.dynlib.clone())
                        .or_default()
                        .insert(ver.clone());
                }
            }
        }
    }

    // Build version index mapping: version string -> version index (starting at 2)
    // Also build the list of (library, versions) sorted for deterministic output
    let mut ver_index_map: std::collections::HashMap<(String, String), u16> = std::collections::HashMap::new();
    let mut ver_idx: u16 = 2;
    let mut lib_ver_list: Vec<(String, Vec<String>)> = Vec::new();
    let mut sorted_libs: Vec<String> = lib_versions.keys().cloned().collect();
    sorted_libs.sort();
    for lib in &sorted_libs {
        let vers: Vec<String> = lib_versions[lib].iter().cloned().collect();
        for v in &vers {
            ver_index_map.insert((lib.clone(), v.clone()), ver_idx);
            ver_idx += 1;
        }
        lib_ver_list.push((lib.clone(), vers));
    }

    // We need to rebuild dynstr with version strings
    let mut dynstr2 = DynStrTab::new();
    let _ = dynstr2.add("");
    for lib in &needed_libs {
        dynstr2.add(lib);
    }
    for name in &plt_symbols {
        dynstr2.add(name);
    }
    for name in &got_symbols {
        dynstr2.add(name);
    }
    for name in &copy_syms_for_dynsym {
        dynstr2.add(name);
    }
    // Add all version strings to dynstr
    for (_, vers) in &lib_ver_list {
        for v in vers {
            dynstr2.add(v);
        }
    }
    let dynstr_data = dynstr2.finish();

    // Rebuild needed_offsets and dynsym name offsets
    let mut needed_offsets: Vec<u32> = Vec::new();
    for lib in &needed_libs {
        needed_offsets.push(dynstr2.offset_of(lib).unwrap_or(0));
    }
    for (i, entry) in dynsym_entries.iter_mut().enumerate() {
        if i == 0 { continue; }
        let name = &dynsym_names[i - 1];
        entry.name = dynstr2.offset_of(name).unwrap_or(0);
    }

    // Build .gnu.version (versym) - one entry per dynsym
    let mut versym_data: Vec<u8> = Vec::new();
    for (i, _) in dynsym_entries.iter().enumerate() {
        if i == 0 {
            versym_data.extend_from_slice(&0u16.to_le_bytes()); // VER_NDX_LOCAL
        } else {
            let sym_name = if i - 1 < dynsym_names.len() { &dynsym_names[i - 1] } else { "" };
            let gs = global_symbols.get(sym_name);
            if let Some(gs) = gs {
                if gs.is_dynamic && !gs.dynlib.is_empty() {
                    if let Some(ref ver) = gs.version {
                        let idx = ver_index_map.get(&(gs.dynlib.clone(), ver.clone()))
                            .copied().unwrap_or(1); // 1 = VER_NDX_GLOBAL
                        versym_data.extend_from_slice(&idx.to_le_bytes());
                    } else {
                        // No version info: use VER_NDX_GLOBAL (1)
                        versym_data.extend_from_slice(&1u16.to_le_bytes());
                    }
                } else {
                    versym_data.extend_from_slice(&0u16.to_le_bytes());
                }
            } else {
                versym_data.extend_from_slice(&0u16.to_le_bytes());
            }
        }
    }

    // Build .gnu.version_r (verneed) section
    let mut verneed_data: Vec<u8> = Vec::new();
    let mut verneed_count: u32 = 0;
    for (lib_i, (lib, vers)) in lib_ver_list.iter().enumerate() {
        if !needed_libs.contains(lib) { continue; }
        let lib_name_off = dynstr2.offset_of(lib).unwrap_or(0);
        let is_last_lib = lib_i == lib_ver_list.len() - 1;

        // Verneed entry header
        verneed_data.extend_from_slice(&1u16.to_le_bytes()); // vn_version = 1
        verneed_data.extend_from_slice(&(vers.len() as u16).to_le_bytes()); // vn_cnt
        verneed_data.extend_from_slice(&lib_name_off.to_le_bytes()); // vn_file
        verneed_data.extend_from_slice(&16u32.to_le_bytes()); // vn_aux (right after this 16-byte header)
        let next_off = if is_last_lib {
            0u32
        } else {
            16 + vers.len() as u32 * 16 // skip this header + all vernaux entries
        };
        verneed_data.extend_from_slice(&next_off.to_le_bytes()); // vn_next
        verneed_count += 1;

        // Vernaux entries for each version
        for (v_i, ver) in vers.iter().enumerate() {
            let ver_name_off = dynstr2.offset_of(ver).unwrap_or(0);
            let ver_idx = ver_index_map[&(lib.clone(), ver.clone())];
            let is_last_ver = v_i == vers.len() - 1;

            verneed_data.extend_from_slice(&elf_hash_str(ver).to_le_bytes()); // vna_hash
            verneed_data.extend_from_slice(&0u16.to_le_bytes()); // vna_flags
            verneed_data.extend_from_slice(&ver_idx.to_le_bytes()); // vna_other
            verneed_data.extend_from_slice(&ver_name_off.to_le_bytes()); // vna_name
            let vna_next: u32 = if is_last_ver { 0 } else { 16 };
            verneed_data.extend_from_slice(&vna_next.to_le_bytes()); // vna_next
        }
    }

    // Now lay out the file
    // Segment 0 (read-only headers): starts at BASE_ADDR
    let mut file_offset: u32 = ehdr_size;
    let mut vaddr: u32 = BASE_ADDR;

    // ELF header
    vaddr += ehdr_size;

    // Program headers
    let phdr_offset = file_offset;
    let phdr_vaddr = vaddr;
    file_offset += phdrs_total_size;
    vaddr += phdrs_total_size;

    // INTERP section
    let interp_offset = file_offset;
    let interp_vaddr = vaddr;
    let interp_size = interp_data.len() as u32;
    if !is_static {
        file_offset += interp_size;
        vaddr += interp_size;
    }

    // Note section
    let note_sec_idx = section_name_to_idx.get(".note").copied();
    let _note_vaddr = vaddr;
    let note_size = note_sec_idx.map(|i| output_sections[i].data.len() as u32).unwrap_or(0);
    if note_size > 0 {
        if let Some(idx) = note_sec_idx {
            output_sections[idx].file_offset = file_offset;
            output_sections[idx].addr = vaddr;
        }
        file_offset += note_size;
        vaddr += note_size;
    }

    // Align for hash table
    file_offset = align_up(file_offset, 4);
    vaddr = align_up(vaddr, 4);

    // .gnu.hash (consistent with x86-64 and RISC-V linkers)
    let gnu_hash_offset = file_offset;
    let gnu_hash_vaddr = vaddr;
    let gnu_hash_size = gnu_hash_data.len() as u32;
    if !is_static {
        file_offset += gnu_hash_size;
        vaddr += gnu_hash_size;
    }

    // .dynsym
    let dynsym_offset = file_offset;
    let dynsym_vaddr = vaddr;
    let dynsym_entsize: u32 = 16;
    let dynsym_size = (dynsym_entries.len() as u32) * dynsym_entsize;
    if !is_static {
        file_offset += dynsym_size;
        vaddr += dynsym_size;
    }

    // .dynstr
    let dynstr_offset = file_offset;
    let dynstr_vaddr = vaddr;
    let dynstr_size = dynstr_data.len() as u32;
    if !is_static {
        file_offset += dynstr_size;
        vaddr += dynstr_size;
    }

    // .gnu.version
    let versym_offset = file_offset;
    let versym_vaddr = vaddr;
    let versym_size = versym_data.len() as u32;
    if !is_static && versym_size > 0 {
        file_offset += versym_size;
        vaddr += versym_size;
    }

    // Align for .gnu.version_r
    file_offset = align_up(file_offset, 4);
    vaddr = align_up(vaddr, 4);

    // .gnu.version_r
    let verneed_offset = file_offset;
    let verneed_vaddr = vaddr;
    let verneed_size = verneed_data.len() as u32;
    if !is_static && verneed_size > 0 {
        file_offset += verneed_size;
        vaddr += verneed_size;
    }

    // .rel.dyn (for GOT-only relocations)
    file_offset = align_up(file_offset, 4);
    vaddr = align_up(vaddr, 4);
    let rel_dyn_offset = file_offset;
    let rel_dyn_vaddr = vaddr;
    let num_copy_relocs = copy_syms_for_dynsym.len();
    let num_rel_dyn = got_symbols.len() + num_copy_relocs;
    let rel_dyn_size = (num_rel_dyn as u32) * 8;
    if !is_static {
        file_offset += rel_dyn_size;
        vaddr += rel_dyn_size;
    }

    // .rel.plt (for PLT relocations)
    let rel_plt_offset = file_offset;
    let rel_plt_vaddr = vaddr;
    let rel_plt_size = (num_plt as u32) * 8;
    if !is_static {
        file_offset += rel_plt_size;
        vaddr += rel_plt_size;
    }

    let ro_headers_end = file_offset;
    let _ro_headers_vaddr_end = vaddr;
    let _ro_headers_size = ro_headers_end; // starts at 0

    // ── Segment 1 (RX): .init + .plt + .text + .fini ──
    file_offset = align_up(file_offset, PAGE_SIZE);
    vaddr = align_up(vaddr, PAGE_SIZE);
    // Ensure vaddr ≡ file_offset (mod PAGE_SIZE)
    vaddr = (vaddr & !0xfff) | (file_offset & 0xfff);
    if vaddr < BASE_ADDR + file_offset {
        vaddr += PAGE_SIZE;
    }

    let text_seg_file_start = file_offset;
    let text_seg_vaddr_start = vaddr;

    // .init section
    let init_sec_idx = section_name_to_idx.get(".init").copied();
    let _init_offset;
    let init_vaddr;
    let init_size;
    if let Some(idx) = init_sec_idx {
        let sec = &mut output_sections[idx];
        let a = sec.align.max(4);
        file_offset = align_up(file_offset, a);
        vaddr = align_up(vaddr, a);
        _init_offset = file_offset;
        init_vaddr = vaddr;
        init_size = sec.data.len() as u32;
        sec.addr = vaddr;
        sec.file_offset = file_offset;
        file_offset += init_size;
        vaddr += init_size;
    } else {
        _init_offset = file_offset;
        init_vaddr = vaddr;
        init_size = 0;
    }

    // .plt section
    // PLT entry 0 (resolver stub): 16 bytes
    // PLT entry N: 16 bytes each
    let plt_entry_size: u32 = 16;
    let plt_header_size: u32 = if num_plt > 0 { 16 } else { 0 };
    let plt_total_size = plt_header_size + (num_plt as u32) * plt_entry_size;

    file_offset = align_up(file_offset, 16);
    vaddr = align_up(vaddr, 16);
    let plt_offset = file_offset;
    let plt_vaddr = vaddr;
    if plt_total_size > 0 {
        file_offset += plt_total_size;
        vaddr += plt_total_size;
    }

    // .text section
    let text_sec_idx = section_name_to_idx.get(".text").copied();
    if let Some(idx) = text_sec_idx {
        let sec = &mut output_sections[idx];
        let a = sec.align.max(16);
        file_offset = align_up(file_offset, a);
        vaddr = align_up(vaddr, a);
        sec.addr = vaddr;
        sec.file_offset = file_offset;
        file_offset += sec.data.len() as u32;
        vaddr += sec.data.len() as u32;
    }

    // .fini section
    let fini_sec_idx = section_name_to_idx.get(".fini").copied();
    let fini_vaddr;
    let fini_size;
    if let Some(idx) = fini_sec_idx {
        let sec = &mut output_sections[idx];
        let a = sec.align.max(4);
        file_offset = align_up(file_offset, a);
        vaddr = align_up(vaddr, a);
        fini_vaddr = vaddr;
        fini_size = sec.data.len() as u32;
        sec.addr = vaddr;
        sec.file_offset = file_offset;
        file_offset += fini_size;
        vaddr += fini_size;
    } else {
        fini_vaddr = 0;
        fini_size = 0;
    }

    // .iplt section (IFUNC PLT entries for static linking)
    // Each entry is 8 bytes: jmp *[abs32]; nop nop
    let iplt_entry_size: u32 = 8;
    let iplt_total_size = (num_ifunc as u32) * iplt_entry_size;
    file_offset = align_up(file_offset, 8);
    vaddr = align_up(vaddr, 8);
    let iplt_offset = file_offset;
    let iplt_vaddr = vaddr;
    if iplt_total_size > 0 {
        file_offset += iplt_total_size;
        vaddr += iplt_total_size;
    }

    let text_seg_file_end = file_offset;
    let text_seg_vaddr_end = vaddr;
    let text_seg_filesz = text_seg_file_end - text_seg_file_start;
    let text_seg_memsz = text_seg_vaddr_end - text_seg_vaddr_start;

    // ── Segment 2 (RO): .rodata + .eh_frame ──
    file_offset = align_up(file_offset, PAGE_SIZE);
    vaddr = align_up(vaddr, PAGE_SIZE);
    vaddr = (vaddr & !0xfff) | (file_offset & 0xfff);
    if vaddr <= text_seg_vaddr_end {
        vaddr = align_up(text_seg_vaddr_end, PAGE_SIZE) | (file_offset & 0xfff);
    }

    let rodata_seg_file_start = file_offset;
    let rodata_seg_vaddr_start = vaddr;

    let rodata_sec_idx = section_name_to_idx.get(".rodata").copied();
    if let Some(idx) = rodata_sec_idx {
        let sec = &mut output_sections[idx];
        let a = sec.align.max(4);
        file_offset = align_up(file_offset, a);
        vaddr = align_up(vaddr, a);
        sec.addr = vaddr;
        sec.file_offset = file_offset;
        file_offset += sec.data.len() as u32;
        vaddr += sec.data.len() as u32;
    }

    let eh_frame_sec_idx = section_name_to_idx.get(".eh_frame").copied();
    if let Some(idx) = eh_frame_sec_idx {
        let sec = &mut output_sections[idx];
        let a = sec.align.max(4);
        file_offset = align_up(file_offset, a);
        vaddr = align_up(vaddr, a);
        sec.addr = vaddr;
        sec.file_offset = file_offset;
        file_offset += sec.data.len() as u32;
        vaddr += sec.data.len() as u32;
    }

    let rodata_seg_file_end = file_offset;
    let rodata_seg_vaddr_end = vaddr;
    let rodata_seg_filesz = rodata_seg_file_end - rodata_seg_file_start;
    let rodata_seg_memsz = rodata_seg_vaddr_end - rodata_seg_vaddr_start;

    // ── Segment 3 (RW): .init_array + .fini_array + .dynamic + .got + .got.plt + .data + .bss ──
    file_offset = align_up(file_offset, PAGE_SIZE);
    vaddr = align_up(vaddr, PAGE_SIZE);
    vaddr = (vaddr & !0xfff) | (file_offset & 0xfff);
    if vaddr <= rodata_seg_vaddr_end {
        vaddr = align_up(rodata_seg_vaddr_end, PAGE_SIZE) | (file_offset & 0xfff);
    }

    let data_seg_file_start = file_offset;
    let data_seg_vaddr_start = vaddr;
    let _relro_start_vaddr = vaddr;

    // .init_array
    let init_array_sec_idx = section_name_to_idx.get(".init_array").copied();
    let init_array_vaddr;
    let init_array_size;
    if let Some(idx) = init_array_sec_idx {
        let sec = &mut output_sections[idx];
        let a = sec.align.max(4);
        file_offset = align_up(file_offset, a);
        vaddr = align_up(vaddr, a);
        init_array_vaddr = vaddr;
        init_array_size = sec.data.len() as u32;
        sec.addr = vaddr;
        sec.file_offset = file_offset;
        file_offset += init_array_size;
        vaddr += init_array_size;
    } else {
        init_array_vaddr = 0;
        init_array_size = 0;
    }

    // .fini_array
    let fini_array_sec_idx = section_name_to_idx.get(".fini_array").copied();
    let fini_array_vaddr;
    let fini_array_size;
    if let Some(idx) = fini_array_sec_idx {
        let sec = &mut output_sections[idx];
        let a = sec.align.max(4);
        file_offset = align_up(file_offset, a);
        vaddr = align_up(vaddr, a);
        fini_array_vaddr = vaddr;
        fini_array_size = sec.data.len() as u32;
        sec.addr = vaddr;
        sec.file_offset = file_offset;
        file_offset += fini_array_size;
        vaddr += fini_array_size;
    } else {
        fini_array_vaddr = 0;
        fini_array_size = 0;
    }

    // .dynamic
    file_offset = align_up(file_offset, 4);
    vaddr = align_up(vaddr, 4);
    let dynamic_offset = file_offset;
    let dynamic_vaddr = vaddr;
    // We'll fill in the dynamic section later; estimate size
    // Each entry is 8 bytes (tag + value)
    let mut num_dynamic_entries: u32 = 0;
    num_dynamic_entries += needed_libs.len() as u32; // DT_NEEDED
    num_dynamic_entries += 1; // DT_GNU_HASH
    num_dynamic_entries += 1; // DT_STRTAB
    num_dynamic_entries += 1; // DT_SYMTAB
    num_dynamic_entries += 1; // DT_STRSZ
    num_dynamic_entries += 1; // DT_SYMENT
    if init_vaddr != 0 && init_size > 0 { num_dynamic_entries += 1; } // DT_INIT
    if fini_vaddr != 0 && fini_size > 0 { num_dynamic_entries += 1; } // DT_FINI
    if init_array_size > 0 { num_dynamic_entries += 2; } // DT_INIT_ARRAY + DT_INIT_ARRAYSZ
    if fini_array_size > 0 { num_dynamic_entries += 2; } // DT_FINI_ARRAY + DT_FINI_ARRAYSZ
    num_dynamic_entries += 1; // DT_DEBUG
    if num_plt > 0 {
        num_dynamic_entries += 1; // DT_PLTGOT
        num_dynamic_entries += 1; // DT_PLTRELSZ
        num_dynamic_entries += 1; // DT_PLTREL
        num_dynamic_entries += 1; // DT_JMPREL
    }
    if num_rel_dyn > 0 {
        num_dynamic_entries += 1; // DT_REL
        num_dynamic_entries += 1; // DT_RELSZ
        num_dynamic_entries += 1; // DT_RELENT
    }
    if verneed_size > 0 {
        num_dynamic_entries += 1; // DT_VERNEED
        num_dynamic_entries += 1; // DT_VERNEEDNUM
        num_dynamic_entries += 1; // DT_VERSYM
    }
    num_dynamic_entries += 1; // DT_NULL
    let dynamic_size = num_dynamic_entries * 8;
    if !is_static {
        file_offset += dynamic_size;
        vaddr += dynamic_size;
    }

    // .got (for non-PLT GOT entries + _GLOBAL_OFFSET_TABLE_)
    file_offset = align_up(file_offset, 4);
    vaddr = align_up(vaddr, 4);
    let got_offset = file_offset;
    let got_vaddr = vaddr;
    // GOT[0] = address of .dynamic (or 0 in static mode)
    // Additional entries for GOT-only symbols
    let got_reserved: usize = 1; // Just _DYNAMIC (or reserved slot)
    let got_non_plt_entries = got_symbols.len();
    let got_entry_size: u32 = 4;
    let got_size = (got_reserved + got_non_plt_entries) as u32 * got_entry_size;
    // Allocate GOT in both static and dynamic modes (static needs it for TLS and GOT-relative accesses)
    let needs_got = !is_static || got_non_plt_entries > 0 || num_got_total > 0;
    if needs_got {
        file_offset += got_size;
        vaddr += got_size;
    }

    // RELRO covers .init_array + .fini_array + .dynamic + .got (non-PLT)
    // It must NOT cover .got.plt (which needs to be writable for lazy binding).
    let _relro_end_vaddr = vaddr; // Not page-aligned: RELRO ends right before .got.plt

    // .got.plt (for PLT GOT entries)
    let gotplt_offset = file_offset;
    let gotplt_vaddr = vaddr;
    // GOT.PLT[0] = address of .dynamic
    // GOT.PLT[1] = link_map (filled by ld.so)
    // GOT.PLT[2] = resolver (filled by ld.so)
    // GOT.PLT[3+] = PLT stub addresses (lazy binding)
    let gotplt_reserved: u32 = 3;
    let gotplt_size = (gotplt_reserved + num_plt as u32) * 4;
    if !is_static && num_plt > 0 {
        file_offset += gotplt_size;
        vaddr += gotplt_size;
    }

    // IFUNC GOT entries (for static linking IRELATIVE resolution)
    let ifunc_got_offset = file_offset;
    let ifunc_got_vaddr = vaddr;
    let ifunc_got_size = (num_ifunc as u32) * 4;
    if ifunc_got_size > 0 {
        file_offset += ifunc_got_size;
        vaddr += ifunc_got_size;
    }

    // .rel.iplt (IRELATIVE relocations for static IFUNC)
    let rel_iplt_offset = file_offset;
    let rel_iplt_vaddr = vaddr;
    let rel_iplt_size = (num_ifunc as u32) * 8; // Each REL entry is 8 bytes
    if rel_iplt_size > 0 {
        file_offset += rel_iplt_size;
        vaddr += rel_iplt_size;
    }

    // .data
    let data_sec_idx = section_name_to_idx.get(".data").copied();
    if let Some(idx) = data_sec_idx {
        let sec = &mut output_sections[idx];
        let a = sec.align.max(4);
        file_offset = align_up(file_offset, a);
        vaddr = align_up(vaddr, a);
        sec.addr = vaddr;
        sec.file_offset = file_offset;
        file_offset += sec.data.len() as u32;
        vaddr += sec.data.len() as u32;
    }

    // TLS sections (.tdata, .tbss) - place after .data, track for PT_TLS
    let mut tls_addr = 0u32;
    let mut tls_file_offset = 0u32;
    let mut tls_file_size = 0u32;
    let mut tls_mem_size = 0u32;
    let mut tls_align = 1u32;

    // .tdata (TLS initialized data)
    if let Some(&idx) = section_name_to_idx.get(".tdata") {
        let sec = &mut output_sections[idx];
        let a = sec.align.max(4);
        file_offset = align_up(file_offset, a);
        vaddr = align_up(vaddr, a);
        sec.addr = vaddr;
        sec.file_offset = file_offset;
        tls_addr = vaddr;
        tls_file_offset = file_offset;
        tls_align = a;
        let sz = sec.data.len() as u32;
        tls_file_size = sz;
        tls_mem_size = sz;
        file_offset += sz;
        vaddr += sz;
    }

    // .tbss (TLS zero-initialized data, NOBITS - no file space)
    if let Some(&idx) = section_name_to_idx.get(".tbss") {
        let sec = &mut output_sections[idx];
        let a = sec.align.max(4);
        let aligned = align_up(tls_mem_size, a);
        if tls_addr == 0 {
            tls_addr = align_up(vaddr, a);
            tls_file_offset = file_offset;
            tls_align = a;
        }
        sec.addr = tls_addr + aligned;
        sec.file_offset = file_offset; // No file space for NOBITS
        tls_mem_size = aligned + sec.data.len() as u32;
        if a > tls_align { tls_align = a; }
    }

    // Align TLS size to TLS alignment
    tls_mem_size = align_up(tls_mem_size, tls_align);
    let has_tls = tls_addr != 0;

    let data_seg_file_end = file_offset;

    // .bss (no file data)
    let bss_sec_idx = section_name_to_idx.get(".bss").copied();
    let bss_vaddr;
    if let Some(idx) = bss_sec_idx {
        let sec = &mut output_sections[idx];
        let a = sec.align.max(4);
        vaddr = align_up(vaddr, a);
        bss_vaddr = vaddr;
        let bss_size = sec.data.len() as u32;
        sec.addr = vaddr;
        sec.file_offset = file_offset; // BSS doesn't occupy file space
        vaddr += bss_size;
    } else {
        bss_vaddr = vaddr;
    }

    // Allocate space in BSS for copy relocations (dynamic data symbols).
    // Collect symbols that need copy relocs and assign them BSS addresses.
    let mut copy_reloc_symbols: Vec<String> = global_symbols.iter()
        .filter(|(_, s)| s.needs_copy && s.is_dynamic)
        .map(|(n, _)| n.clone())
        .collect();
    copy_reloc_symbols.sort();

    for name in &copy_reloc_symbols {
        if let Some(sym) = global_symbols.get_mut(name) {
            let align = if sym.size >= 4 { 4 } else { 1 };
            vaddr = align_up(vaddr, align);
            sym.copy_addr = vaddr;
            sym.address = vaddr;
            vaddr += sym.size.max(4); // At least 4 bytes for pointers
        }
    }

    let data_seg_vaddr_end = vaddr;
    let data_seg_filesz = data_seg_file_end - data_seg_file_start;
    let data_seg_memsz = data_seg_vaddr_end - data_seg_vaddr_start;

    // GOT base address is the start of .got.plt (convention for i386)
    let got_base = if num_plt > 0 { gotplt_vaddr } else { got_vaddr };

    // ── Assign linker-defined symbol addresses ──────────────────────────────
    // _GLOBAL_OFFSET_TABLE_
    global_symbols.entry("_GLOBAL_OFFSET_TABLE_".to_string()).or_insert(LinkerSymbol {
        address: got_base,
        size: 0,
        sym_type: STT_OBJECT,
        binding: STB_LOCAL,
        visibility: STV_DEFAULT,
        is_defined: true,
        needs_plt: false,
        needs_got: false,
        output_section: usize::MAX,
        section_offset: 0,
        plt_index: 0,
        got_index: 0,
        is_dynamic: false,
        dynlib: String::new(),
        needs_copy: false,
        copy_addr: 0,
        version: None,
    });
    if let Some(sym) = global_symbols.get_mut("_GLOBAL_OFFSET_TABLE_") {
        sym.address = got_base;
        sym.is_defined = true;
    }

    // Build standard linker symbol addresses using shared infrastructure
    // (consistent with x86-64/ARM/RISC-V via get_standard_linker_symbols)
    let linker_addrs = LinkerSymbolAddresses {
        base_addr: BASE_ADDR as u64,
        got_addr: got_base as u64,
        dynamic_addr: dynamic_vaddr as u64,
        bss_addr: bss_vaddr as u64,
        bss_size: (data_seg_vaddr_end - bss_vaddr) as u64,
        text_end: text_seg_vaddr_end as u64,
        data_start: data_seg_vaddr_start as u64,
        init_array_start: init_array_vaddr as u64,
        init_array_size: init_array_size as u64,
        fini_array_start: fini_array_vaddr as u64,
        fini_array_size: fini_array_size as u64,
        preinit_array_start: 0,
        preinit_array_size: 0,
        // i686 uses __rel_iplt_* (REL format) instead of __rela_iplt_*
        rela_iplt_start: rel_iplt_vaddr as u64,
        rela_iplt_size: rel_iplt_size as u64,
    };
    let standard_syms = get_standard_linker_symbols(&linker_addrs);
    // i686 uses __rel_iplt_* (REL format), not __rela_iplt_* (RELA format),
    // so filter out the RELA variants from the shared list.
    let linker_sym_map: HashMap<&str, u64> = standard_syms.iter()
        .filter(|s| !s.name.starts_with("__rela_iplt"))
        .map(|s| (s.name, s.value))
        .collect();

    // Resolve symbol addresses
    for (name, sym) in global_symbols.iter_mut() {
        if sym.is_dynamic {
            // Dynamic symbols: PLT address if they have a PLT entry
            if sym.needs_plt {
                sym.address = plt_vaddr + plt_header_size + (sym.plt_index as u32) * plt_entry_size;
            }
            continue;
        }
        if sym.output_section < output_sections.len() {
            sym.address = output_sections[sym.output_section].addr + sym.section_offset;
        }
        // Standard linker-defined symbols from shared infrastructure
        if let Some(&value) = linker_sym_map.get(name.as_str()) {
            sym.address = value as u32;
            if name == "__dso_handle" { sym.is_defined = true; }
        }
        // i686-specific aliases (not in the shared list)
        match name.as_str() {
            "__bss_start__" => sym.address = bss_vaddr,
            "edata" => sym.address = bss_vaddr,
            "end" | "__end__" => sym.address = data_seg_vaddr_end,
            // i686 uses REL (not RELA), so __rel_iplt_* instead of __rela_iplt_*
            "__rel_iplt_start" => sym.address = rel_iplt_vaddr,
            "__rel_iplt_end" => sym.address = rel_iplt_vaddr + rel_iplt_size,
            _ => {}
        }
    }

    // Override IFUNC symbol addresses to point to their IPLT entries.
    // Save resolver addresses first (the original symbol address is the IFUNC resolver).
    let mut ifunc_resolver_addrs: Vec<u32> = Vec::new();
    for (i, name) in ifunc_symbols.iter().enumerate() {
        if let Some(sym) = global_symbols.get_mut(name) {
            ifunc_resolver_addrs.push(sym.address);
            sym.address = iplt_vaddr + (i as u32) * iplt_entry_size;
        }
    }

    // ── Build IPLT entries (IFUNC PLT stubs for static linking) ──────────────
    let mut iplt_data: Vec<u8> = Vec::new();
    if num_ifunc > 0 {
        for i in 0..num_ifunc {
            let got_entry_addr = ifunc_got_vaddr + (i as u32) * 4;
            // jmp *[abs32]  (ff 25 <abs32>)
            iplt_data.push(0xff);
            iplt_data.push(0x25);
            iplt_data.extend_from_slice(&got_entry_addr.to_le_bytes());
            // 2-byte nop padding (66 90)
            iplt_data.push(0x66);
            iplt_data.push(0x90);
        }
    }

    // ── Build IFUNC GOT entries (resolver addresses initially) ───────────────
    let mut ifunc_got_data: Vec<u8> = Vec::new();
    for &resolver_addr in &ifunc_resolver_addrs {
        ifunc_got_data.extend_from_slice(&resolver_addr.to_le_bytes());
    }

    // ── Build .rel.iplt (R_386_IRELATIVE relocations) ────────────────────────
    let mut rel_iplt_data: Vec<u8> = Vec::new();
    for i in 0..num_ifunc {
        let r_offset = ifunc_got_vaddr + (i as u32) * 4;
        let r_info = R_386_IRELATIVE; // type=42, sym=0
        rel_iplt_data.extend_from_slice(&r_offset.to_le_bytes());
        rel_iplt_data.extend_from_slice(&r_info.to_le_bytes());
    }

    // ── Build PLT entries ────────────────────────────────────────────────────
    let mut plt_data: Vec<u8> = Vec::new();
    if num_plt > 0 {
        // PLT[0]: resolver stub
        // pushl GOT[1]          (got.plt + 4)
        // jmp *GOT[2]           (got.plt + 8)
        // nop padding
        let got1 = gotplt_vaddr + 4;
        let got2 = gotplt_vaddr + 8;
        plt_data.push(0xff); plt_data.push(0x35); // pushl [abs32]
        plt_data.extend_from_slice(&got1.to_le_bytes());
        plt_data.push(0xff); plt_data.push(0x25); // jmp [abs32]
        plt_data.extend_from_slice(&got2.to_le_bytes());
        // Pad to 16 bytes
        while plt_data.len() < plt_header_size as usize {
            plt_data.push(0x90); // nop
        }

        // PLT[N]: for each imported symbol
        for i in 0..num_plt {
            let gotplt_entry = gotplt_vaddr + (gotplt_reserved + i as u32) * 4;
            let plt_entry_addr = plt_vaddr + plt_header_size + (i as u32) * plt_entry_size;

            // jmp *GOT[3+i]
            plt_data.push(0xff); plt_data.push(0x25); // jmp [abs32]
            plt_data.extend_from_slice(&gotplt_entry.to_le_bytes());
            // pushl reloc_index
            plt_data.push(0x68); // push imm32
            plt_data.extend_from_slice(&(i as u32 * 8).to_le_bytes()); // byte offset into .rel.plt
            // jmp PLT[0]
            plt_data.push(0xe9); // jmp rel32
            let target = plt_vaddr as i32 - (plt_entry_addr as i32 + plt_entry_size as i32);
            plt_data.extend_from_slice(&target.to_le_bytes());
        }
    }

    // ── Apply relocations ────────────────────────────────────────────────────
    // Now we have all section addresses, we can apply relocations.

    for (obj_idx, obj) in inputs.iter().enumerate() {
        for sec in &obj.sections {
            if sec.relocations.is_empty() { continue; }

            let _out_name = match output_section_name(&sec.name, sec.flags, sec.sh_type) {
                Some(n) => n,
                None => continue,
            };
            let (out_sec_idx, sec_base_offset) = match section_map.get(&(obj_idx, sec.input_index)) {
                Some(&v) => v,
                None => continue,
            };

            for &(rel_offset, rel_type, sym_idx, addend) in &sec.relocations {
                let patch_offset = sec_base_offset + rel_offset;
                let out_sec = &output_sections[out_sec_idx];
                let patch_addr = out_sec.addr + patch_offset;

                // Resolve the symbol
                let sym = if (sym_idx as usize) < obj.symbols.len() {
                    &obj.symbols[sym_idx as usize]
                } else {
                    return Err(format!("invalid symbol index {} in reloc", sym_idx));
                };

                // Resolve the symbol address.
                // LOCAL symbols must always be resolved per-object via section_map
                // to avoid collisions between identically-named locals (e.g. .LC0)
                // from different translation units.
                let sym_addr = if sym.sym_type == STT_SECTION {
                    // Section symbol: resolve to the section's address in output
                    if sym.section_index != SHN_UNDEF && sym.section_index != SHN_ABS {
                        match section_map.get(&(obj_idx, sym.section_index as usize)) {
                            Some(&(sec_out_idx, sec_out_offset)) => {
                                output_sections[sec_out_idx].addr + sec_out_offset
                            }
                            None => 0,
                        }
                    } else {
                        0
                    }
                } else if sym.name.is_empty() {
                    0
                } else if sym.binding == STB_LOCAL {
                    // Local symbols must be resolved per-object via section_map + sym.value.
                    // They must NOT be looked up in global_symbols because local symbols
                    // like .LC0, .LC1 from different object files would collide in the
                    // global HashMap, causing wrong address resolution.
                    if sym.section_index != SHN_UNDEF && sym.section_index != SHN_ABS {
                        match section_map.get(&(obj_idx, sym.section_index as usize)) {
                            Some(&(sec_out_idx, sec_out_offset)) => {
                                output_sections[sec_out_idx].addr + sec_out_offset + sym.value
                            }
                            None => sym.value,
                        }
                    } else if sym.section_index == SHN_ABS {
                        sym.value
                    } else {
                        0
                    }
                } else {
                    match global_symbols.get(&sym.name) {
                        Some(gs) => gs.address,
                        None => {
                            // Symbol not in global_symbols: resolve via section_map + sym.value
                            if sym.section_index != SHN_UNDEF && sym.section_index != SHN_ABS {
                                match section_map.get(&(obj_idx, sym.section_index as usize)) {
                                    Some(&(sec_out_idx, sec_out_offset)) => {
                                        output_sections[sec_out_idx].addr + sec_out_offset + sym.value
                                    }
                                    None => sym.value,
                                }
                            } else if sym.section_index == SHN_ABS {
                                sym.value
                            } else {
                                0
                            }
                        }
                    }
                };

                // Determine if this symbol is dynamic (needs PLT)
                let is_dyn = !sym.name.is_empty() && global_symbols.get(&sym.name)
                    .map(|gs| gs.is_dynamic && gs.needs_plt).unwrap_or(false);

                // Track whether we need to rewrite mov→lea for GOT32X relaxation
                let mut relax_got32x = false;

                let value: u32 = match rel_type {
                    R_386_NONE => continue,
                    R_386_32 => {
                        // S + A
                        (sym_addr as i32 + addend) as u32
                    }
                    R_386_PC32 => {
                        // S + A - P
                        let s = if is_dyn {
                            // Use PLT address for dynamic symbols
                            global_symbols.get(&sym.name).map(|gs| gs.address).unwrap_or(0)
                        } else {
                            sym_addr
                        };
                        (s as i32 + addend - patch_addr as i32) as u32
                    }
                    R_386_PLT32 => {
                        // S + A - P (same as PC32 when we have a PLT entry)
                        let s = if is_dyn {
                            global_symbols.get(&sym.name).map(|gs| gs.address).unwrap_or(0)
                        } else {
                            sym_addr
                        };
                        (s as i32 + addend - patch_addr as i32) as u32
                    }
                    R_386_GOTPC => {
                        // GOT + A - P
                        (got_base as i32 + addend - patch_addr as i32) as u32
                    }
                    R_386_GOTOFF => {
                        // S + A - GOT
                        (sym_addr as i32 + addend - got_base as i32) as u32
                    }
                    R_386_GOT32 | R_386_GOT32X => {
                        // GOT entry offset from GOT base
                        if let Some(gs) = global_symbols.get(&sym.name) {
                            if gs.is_dynamic {
                                // GOT entry for dynamic symbol
                                let got_entry_addr = if gs.needs_plt {
                                    gotplt_vaddr + (gotplt_reserved + gs.plt_index as u32) * 4
                                } else {
                                    got_vaddr + (got_reserved as u32 + (gs.got_index - num_plt) as u32) * 4
                                };
                                (got_entry_addr as i32 + addend - got_base as i32) as u32
                            } else if gs.needs_got {
                                // Local symbol with GOT entry
                                let got_entry_addr = got_vaddr + (got_reserved as u32 + (gs.got_index - num_plt) as u32) * 4;
                                (got_entry_addr as i32 + addend - got_base as i32) as u32
                            } else if rel_type == R_386_GOT32X {
                                // GOT32X relaxation: rewrite mov→lea, use GOTOFF
                                relax_got32x = true;
                                (sym_addr as i32 + addend - got_base as i32) as u32
                            } else {
                                // R_386_GOT32 without GOT entry - treat as GOTOFF
                                (sym_addr as i32 + addend - got_base as i32) as u32
                            }
                        } else if rel_type == R_386_GOT32X {
                            // Section symbol or unknown: relax GOT32X to GOTOFF
                            relax_got32x = true;
                            (sym_addr as i32 + addend - got_base as i32) as u32
                        } else {
                            // R_386_GOT32 for section/unknown symbol - GOTOFF
                            (sym_addr as i32 + addend - got_base as i32) as u32
                        }
                    }
                    R_386_TLS_TPOFF => {
                        // Type 14: S + A - TLS_end (negative offset from TP)
                        // Used with @TPOFF: %gs:tpoff accesses the variable
                        let tpoff = sym_addr as i32 - tls_addr as i32 - tls_mem_size as i32;
                        (tpoff + addend) as u32
                    }
                    R_386_TLS_LE => {
                        // Type 17: S + A - TLS_end (negative offset from TP, @ntpoff)
                        // Used as: mov %gs:@ntpoff(x), %reg  (direct negative offset)
                        let tpoff = sym_addr as i32 - tls_addr as i32 - tls_mem_size as i32;
                        (tpoff + addend) as u32
                    }
                    R_386_TLS_LE_32 => {
                        // Type 34: TLS_end - (S + A) (positive/negated offset, @tpoff)
                        // Used with neg/sub patterns: negl %reg; movl %gs:(%reg), ...
                        let ntpoff = tls_addr as i32 + tls_mem_size as i32 - sym_addr as i32;
                        (ntpoff + addend) as u32
                    }
                    R_386_TLS_TPOFF32 => {
                        // Type 37: TLS_end - (S + A) (positive/negated offset from TP)
                        // Used with sub/neg patterns, opposite sign from R_386_TLS_TPOFF
                        let ntpoff = tls_addr as i32 + tls_mem_size as i32 - sym_addr as i32;
                        (ntpoff + addend) as u32
                    }
                    R_386_TLS_IE => {
                        // Initial Exec TLS via GOT: absolute address of GOT entry
                        // The GOT entry contains the negative TP offset
                        if let Some(gs) = global_symbols.get(&sym.name) {
                            if gs.needs_got {
                                let got_entry_addr = got_vaddr + (got_reserved as u32 + (gs.got_index - num_plt) as u32) * 4;
                                (got_entry_addr as i32 + addend) as u32
                            } else {
                                // Relax to LE: direct TPOFF
                                let tpoff = sym_addr as i32 - tls_addr as i32 - tls_mem_size as i32;
                                (tpoff + addend) as u32
                            }
                        } else {
                            addend as u32
                        }
                    }
                    R_386_TLS_GOTIE => {
                        // GOT-relative offset to TLS GOT entry
                        // Value = GOT_entry_addr - GOT_base
                        if let Some(gs) = global_symbols.get(&sym.name) {
                            if gs.needs_got {
                                let got_entry_addr = got_vaddr + (got_reserved as u32 + (gs.got_index - num_plt) as u32) * 4;
                                (got_entry_addr as i32 + addend - got_base as i32) as u32
                            } else {
                                // No GOT entry; use direct offset
                                let tpoff = sym_addr as i32 - tls_addr as i32 - tls_mem_size as i32;
                                (tpoff + addend) as u32
                            }
                        } else {
                            addend as u32
                        }
                    }
                    R_386_TLS_GD => {
                        // General Dynamic TLS model → relaxed to Local Exec for static linking.
                        // In static binaries these come from dead code in unconditionally
                        // extracted archive members.  Resolve to the LE TP offset so that
                        // if the code IS reached it still works correctly.
                        if has_tls && sym.sym_type == STT_TLS {
                            let tpoff = sym_addr as i32 - tls_addr as i32 - tls_mem_size as i32;
                            (tpoff + addend) as u32
                        } else {
                            addend as u32
                        }
                    }
                    R_386_TLS_DTPMOD32 => {
                        // Module ID for GD TLS.  In a static executable the single
                        // module has ID 1.
                        1u32
                    }
                    R_386_TLS_DTPOFF32 => {
                        // Offset within the TLS block from the module base.
                        // For static linking: sym_addr - tls_addr (base of TLS segment).
                        if has_tls {
                            (sym_addr as i32 - tls_addr as i32 + addend) as u32
                        } else {
                            addend as u32
                        }
                    }
                    other => {
                        return Err(format!(
                            "unsupported i686 relocation type {} at {}:0x{:x}",
                            other, obj.filename, rel_offset
                        ));
                    }
                };

                // Patch the output section data
                let out_sec = &mut output_sections[out_sec_idx];
                let off = patch_offset as usize;
                if off + 4 <= out_sec.data.len() {
                    // For GOT32X relaxation, rewrite mov (0x8b) → lea (0x8d)
                    if relax_got32x && off >= 2 && out_sec.data[off - 2] == 0x8b {
                        out_sec.data[off - 2] = 0x8d;
                    }
                    out_sec.data[off..off + 4].copy_from_slice(&value.to_le_bytes());
                }
            }
        }
    }

    // ── Build GOT entries ────────────────────────────────────────────────────
    let mut got_data: Vec<u8> = Vec::new();
    if needs_got {
        if is_static {
            // GOT[0] = 0 in static mode (no .dynamic)
            got_data.extend_from_slice(&0u32.to_le_bytes());
        } else {
            // GOT[0] = .dynamic address
            got_data.extend_from_slice(&dynamic_vaddr.to_le_bytes());
        }
        // Non-PLT GOT entries
        for name in &got_symbols {
            if let Some(gs) = global_symbols.get(name) {
                if has_tls && gs.sym_type == STT_TLS {
                    // TLS GOT entry: store the TPOFF value (negative offset from TP)
                    // On i386 variant II: TPOFF = sym_addr - tls_addr - tls_mem_size
                    let tpoff = gs.address as i32 - tls_addr as i32 - tls_mem_size as i32;
                    got_data.extend_from_slice(&(tpoff as u32).to_le_bytes());
                } else if gs.is_dynamic {
                    got_data.extend_from_slice(&0u32.to_le_bytes()); // Filled by ld.so
                } else {
                    got_data.extend_from_slice(&gs.address.to_le_bytes());
                }
            } else {
                got_data.extend_from_slice(&0u32.to_le_bytes());
            }
        }
    }

    // Build GOT.PLT entries
    let mut gotplt_data: Vec<u8> = Vec::new();
    if !is_static && num_plt > 0 {
        // GOT.PLT[0] = .dynamic
        gotplt_data.extend_from_slice(&dynamic_vaddr.to_le_bytes());
        // GOT.PLT[1] = 0 (link_map, filled by ld.so)
        gotplt_data.extend_from_slice(&0u32.to_le_bytes());
        // GOT.PLT[2] = 0 (resolver, filled by ld.so)
        gotplt_data.extend_from_slice(&0u32.to_le_bytes());
        // GOT.PLT[3+i] = PLT[i+1] + 6 (lazy binding: push + jmp)
        for i in 0..num_plt {
            let lazy_addr = plt_vaddr + plt_header_size + (i as u32) * plt_entry_size + 6;
            gotplt_data.extend_from_slice(&lazy_addr.to_le_bytes());
        }
    }

    // ── Build .rel.plt entries ───────────────────────────────────────────────
    let mut rel_plt_data: Vec<u8> = Vec::new();
    for (i, name) in plt_symbols.iter().enumerate() {
        let gotplt_entry_addr = gotplt_vaddr + (gotplt_reserved + i as u32) * 4;
        let dynsym_idx = dynsym_map[name] as u32;
        // R_386_JMP_SLOT = 7
        let r_info = (dynsym_idx << 8) | 7;
        rel_plt_data.extend_from_slice(&gotplt_entry_addr.to_le_bytes());
        rel_plt_data.extend_from_slice(&r_info.to_le_bytes());
    }

    // ── Build .rel.dyn entries ───────────────────────────────────────────────
    let mut rel_dyn_data: Vec<u8> = Vec::new();
    // GOT entries
    for (i, name) in got_symbols.iter().enumerate() {
        let got_entry_addr = got_vaddr + (got_reserved as u32 + i as u32) * 4;
        let dynsym_idx = dynsym_map[name] as u32;
        let rel_type: u32 = 6; // R_386_GLOB_DAT
        let r_info = (dynsym_idx << 8) | rel_type;
        rel_dyn_data.extend_from_slice(&got_entry_addr.to_le_bytes());
        rel_dyn_data.extend_from_slice(&r_info.to_le_bytes());
    }
    // Copy relocations for data symbols from shared libs
    for name in &copy_reloc_symbols {
        if let Some(gs) = global_symbols.get(name) {
            if let Some(&dynsym_idx) = dynsym_map.get(name) {
                let rel_type: u32 = 5; // R_386_COPY
                let r_info = ((dynsym_idx as u32) << 8) | rel_type;
                rel_dyn_data.extend_from_slice(&gs.copy_addr.to_le_bytes());
                rel_dyn_data.extend_from_slice(&r_info.to_le_bytes());
            }
        }
    }

    // ── Build .dynamic section ───────────────────────────────────────────────
    let mut dynamic_data: Vec<u8> = Vec::new();
    fn push_dyn(data: &mut Vec<u8>, tag: i32, val: u32) {
        data.extend_from_slice(&tag.to_le_bytes());
        data.extend_from_slice(&val.to_le_bytes());
    }

    if !is_static {
        for &off in &needed_offsets {
            push_dyn(&mut dynamic_data, DT_NEEDED, off);
        }
        push_dyn(&mut dynamic_data, DT_GNU_HASH, gnu_hash_vaddr);
        push_dyn(&mut dynamic_data, DT_STRTAB, dynstr_vaddr);
        push_dyn(&mut dynamic_data, DT_SYMTAB, dynsym_vaddr);
        push_dyn(&mut dynamic_data, DT_STRSZ, dynstr_size);
        push_dyn(&mut dynamic_data, DT_SYMENT, dynsym_entsize);
        if init_vaddr != 0 && init_size > 0 {
            push_dyn(&mut dynamic_data, DT_INIT, init_vaddr);
        }
        if fini_vaddr != 0 && fini_size > 0 {
            push_dyn(&mut dynamic_data, DT_FINI, fini_vaddr);
        }
        if init_array_size > 0 {
            push_dyn(&mut dynamic_data, DT_INIT_ARRAY, init_array_vaddr);
            push_dyn(&mut dynamic_data, DT_INIT_ARRAYSZ, init_array_size);
        }
        if fini_array_size > 0 {
            push_dyn(&mut dynamic_data, DT_FINI_ARRAY, fini_array_vaddr);
            push_dyn(&mut dynamic_data, DT_FINI_ARRAYSZ, fini_array_size);
        }
        push_dyn(&mut dynamic_data, DT_DEBUG, 0);
        if num_plt > 0 {
            push_dyn(&mut dynamic_data, DT_PLTGOT, gotplt_vaddr);
            push_dyn(&mut dynamic_data, DT_PLTRELSZ, rel_plt_size);
            push_dyn(&mut dynamic_data, DT_PLTREL, 17); // DT_REL
            push_dyn(&mut dynamic_data, DT_JMPREL, rel_plt_vaddr);
        }
        if num_rel_dyn > 0 {
            push_dyn(&mut dynamic_data, DT_REL, rel_dyn_vaddr);
            push_dyn(&mut dynamic_data, DT_RELSZ, rel_dyn_size);
            push_dyn(&mut dynamic_data, DT_RELENT, 8);
        }
        if verneed_size > 0 {
            push_dyn(&mut dynamic_data, DT_VERNEED, verneed_vaddr);
            push_dyn(&mut dynamic_data, DT_VERNEEDNUM, verneed_count);
            push_dyn(&mut dynamic_data, DT_VERSYM, versym_vaddr);
        }
        push_dyn(&mut dynamic_data, DT_NULL, 0);
    }

    // ── Find entry point ─────────────────────────────────────────────────────
    let entry_point = global_symbols.get("_start")
        .map(|s| s.address)
        .unwrap_or_else(|| {
            global_symbols.get("main").map(|s| s.address).unwrap_or(BASE_ADDR)
        });

    // ── Patch dynsym entries for copy-reloc symbols with their BSS addresses ──
    for name in &copy_syms_for_dynsym {
        if let Some(sym) = global_symbols.get(name) {
            if let Some(&idx) = dynsym_map.get(name) {
                dynsym_entries[idx].value = sym.copy_addr;
                dynsym_entries[idx].shndx = 1; // non-UNDEF: symbol is defined in this executable
            }
        }
    }

    // ── Build dynsym binary data ─────────────────────────────────────────────
    let mut dynsym_data: Vec<u8> = Vec::new();
    for sym in &dynsym_entries {
        dynsym_data.extend_from_slice(&sym.name.to_le_bytes());
        dynsym_data.extend_from_slice(&sym.value.to_le_bytes());
        dynsym_data.extend_from_slice(&sym.size.to_le_bytes());
        dynsym_data.push(sym.info);
        dynsym_data.push(sym.other);
        dynsym_data.extend_from_slice(&sym.shndx.to_le_bytes());
    }

    // ── Write ELF file ───────────────────────────────────────────────────────
    let total_file_size = file_offset as usize;
    let mut output = vec![0u8; total_file_size];

    // ELF header
    output[0..4].copy_from_slice(&ELF_MAGIC);
    output[4] = ELFCLASS32;
    output[5] = ELFDATA2LSB;
    output[6] = EV_CURRENT;
    output[7] = 0; // ELFOSABI_NONE
    // e_type
    output[16..18].copy_from_slice(&ET_EXEC.to_le_bytes());
    // e_machine
    output[18..20].copy_from_slice(&EM_386.to_le_bytes());
    // e_version
    output[20..24].copy_from_slice(&1u32.to_le_bytes());
    // e_entry
    output[24..28].copy_from_slice(&entry_point.to_le_bytes());
    // e_phoff
    output[28..32].copy_from_slice(&(ehdr_size).to_le_bytes());
    // e_shoff = 0 (no section headers in executable for now)
    output[32..36].copy_from_slice(&0u32.to_le_bytes());
    // e_flags
    output[36..40].copy_from_slice(&0u32.to_le_bytes());
    // e_ehsize
    output[40..42].copy_from_slice(&(ehdr_size as u16).to_le_bytes());
    // e_phentsize
    output[42..44].copy_from_slice(&(phdr_size as u16).to_le_bytes());
    // e_phnum
    output[44..46].copy_from_slice(&(num_phdrs as u16).to_le_bytes());
    // e_shentsize
    output[46..48].copy_from_slice(&40u16.to_le_bytes());
    // e_shnum = 0
    output[48..50].copy_from_slice(&0u16.to_le_bytes());
    // e_shstrndx = 0
    output[50..52].copy_from_slice(&0u16.to_le_bytes());

    // ── Write program headers ────────────────────────────────────────────────
    let mut phdr_pos = phdr_offset as usize;

    // Helper to write a program header
    let write_phdr = |output: &mut Vec<u8>, pos: &mut usize,
                      p_type: u32, p_offset: u32, p_vaddr: u32,
                      p_filesz: u32, p_memsz: u32, p_flags: u32, p_align: u32| {
        output[*pos..*pos + 4].copy_from_slice(&p_type.to_le_bytes());
        output[*pos + 4..*pos + 8].copy_from_slice(&p_offset.to_le_bytes());
        output[*pos + 8..*pos + 12].copy_from_slice(&p_vaddr.to_le_bytes());
        output[*pos + 12..*pos + 16].copy_from_slice(&p_vaddr.to_le_bytes()); // p_paddr = p_vaddr
        output[*pos + 16..*pos + 20].copy_from_slice(&p_filesz.to_le_bytes());
        output[*pos + 20..*pos + 24].copy_from_slice(&p_memsz.to_le_bytes());
        output[*pos + 24..*pos + 28].copy_from_slice(&p_flags.to_le_bytes());
        output[*pos + 28..*pos + 32].copy_from_slice(&p_align.to_le_bytes());
        *pos += phdr_size as usize;
    };

    // PHDR
    write_phdr(&mut output, &mut phdr_pos, PT_PHDR,
        phdr_offset, phdr_vaddr,
        phdrs_total_size, phdrs_total_size,
        PF_R, 4);

    // INTERP
    if !is_static {
        write_phdr(&mut output, &mut phdr_pos, PT_INTERP,
            interp_offset, interp_vaddr,
            interp_size, interp_size,
            PF_R, 1);
    }

    // LOAD segment 0: read-only headers
    write_phdr(&mut output, &mut phdr_pos, PT_LOAD,
        0, BASE_ADDR,
        ro_headers_end, ro_headers_end,
        PF_R, PAGE_SIZE);

    // LOAD segment 1: text (RX)
    write_phdr(&mut output, &mut phdr_pos, PT_LOAD,
        text_seg_file_start, text_seg_vaddr_start,
        text_seg_filesz, text_seg_memsz,
        PF_R | PF_X, PAGE_SIZE);

    // LOAD segment 2: rodata (RO)
    if rodata_seg_filesz > 0 {
        write_phdr(&mut output, &mut phdr_pos, PT_LOAD,
            rodata_seg_file_start, rodata_seg_vaddr_start,
            rodata_seg_filesz, rodata_seg_memsz,
            PF_R, PAGE_SIZE);
    }

    // LOAD segment 3: data + bss (RW)
    write_phdr(&mut output, &mut phdr_pos, PT_LOAD,
        data_seg_file_start, data_seg_vaddr_start,
        data_seg_filesz, data_seg_memsz,
        PF_R | PF_W, PAGE_SIZE);

    // DYNAMIC
    if !is_static {
        write_phdr(&mut output, &mut phdr_pos, PT_DYNAMIC,
            dynamic_offset, dynamic_vaddr,
            dynamic_data.len() as u32, dynamic_data.len() as u32,
            PF_R | PF_W, 4);
    }

    // GNU_STACK
    write_phdr(&mut output, &mut phdr_pos, PT_GNU_STACK,
        0, 0, 0, 0,
        PF_R | PF_W, 0x10);

    // PT_TLS
    if has_tls {
        write_phdr(&mut output, &mut phdr_pos, PT_TLS,
            tls_file_offset, tls_addr,
            tls_file_size, tls_mem_size,
            PF_R, tls_align);
    }

    // GNU_RELRO omitted: see note above about .got/.got.plt page sharing

    // ── Write section data to file ───────────────────────────────────────────

    // Write INTERP
    if !is_static {
        output[interp_offset as usize..interp_offset as usize + interp_data.len()]
            .copy_from_slice(&interp_data);
    }

    // Write .gnu.hash
    if !is_static {
        output[gnu_hash_offset as usize..gnu_hash_offset as usize + gnu_hash_data.len()]
            .copy_from_slice(&gnu_hash_data);
    }

    // Write .dynsym
    if !is_static {
        output[dynsym_offset as usize..dynsym_offset as usize + dynsym_data.len()]
            .copy_from_slice(&dynsym_data);
    }

    // Write .dynstr
    if !is_static {
        output[dynstr_offset as usize..dynstr_offset as usize + dynstr_data.len()]
            .copy_from_slice(&dynstr_data);
    }

    // Write .gnu.version
    if !is_static && !versym_data.is_empty() {
        output[versym_offset as usize..versym_offset as usize + versym_data.len()]
            .copy_from_slice(&versym_data);
    }

    // Write .gnu.version_r
    if !is_static && !verneed_data.is_empty() {
        output[verneed_offset as usize..verneed_offset as usize + verneed_data.len()]
            .copy_from_slice(&verneed_data);
    }

    // Write .rel.dyn
    if !is_static && !rel_dyn_data.is_empty() {
        output[rel_dyn_offset as usize..rel_dyn_offset as usize + rel_dyn_data.len()]
            .copy_from_slice(&rel_dyn_data);
    }

    // Write .rel.plt
    if !is_static && !rel_plt_data.is_empty() {
        output[rel_plt_offset as usize..rel_plt_offset as usize + rel_plt_data.len()]
            .copy_from_slice(&rel_plt_data);
    }

    // Write .plt
    if !plt_data.is_empty() {
        output[plt_offset as usize..plt_offset as usize + plt_data.len()]
            .copy_from_slice(&plt_data);
    }

    // Write .dynamic
    if !is_static && !dynamic_data.is_empty() {
        output[dynamic_offset as usize..dynamic_offset as usize + dynamic_data.len()]
            .copy_from_slice(&dynamic_data);
    }

    // Write .got
    if !got_data.is_empty() {
        output[got_offset as usize..got_offset as usize + got_data.len()]
            .copy_from_slice(&got_data);
    }

    // Write .got.plt
    if !is_static && !gotplt_data.is_empty() {
        output[gotplt_offset as usize..gotplt_offset as usize + gotplt_data.len()]
            .copy_from_slice(&gotplt_data);
    }

    // Write .iplt (IFUNC PLT stubs)
    if !iplt_data.is_empty() {
        output[iplt_offset as usize..iplt_offset as usize + iplt_data.len()]
            .copy_from_slice(&iplt_data);
    }

    // Write IFUNC GOT entries
    if !ifunc_got_data.is_empty() {
        output[ifunc_got_offset as usize..ifunc_got_offset as usize + ifunc_got_data.len()]
            .copy_from_slice(&ifunc_got_data);
    }

    // Write .rel.iplt (IRELATIVE relocations)
    if !rel_iplt_data.is_empty() {
        output[rel_iplt_offset as usize..rel_iplt_offset as usize + rel_iplt_data.len()]
            .copy_from_slice(&rel_iplt_data);
    }

    // Write all output sections (generic loop, matching x86-64 approach)
    for sec in output_sections.iter() {
        if sec.sh_type == SHT_NOBITS || sec.data.is_empty() { continue; }
        let off = sec.file_offset as usize;
        let end = off + sec.data.len();
        if end <= output.len() {
            output[off..end].copy_from_slice(&sec.data);
        }
    }


    // Write to file
    std::fs::write(output_path, &output)
        .map_err(|e| format!("failed to write output: {}", e))?;

    // Make executable
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let perms = std::fs::Permissions::from_mode(0o755);
        std::fs::set_permissions(output_path, perms)
            .map_err(|e| format!("failed to set permissions: {}", e))?;
    }

    Ok(())
}

// ── Helper types and functions ───────────────────────────────────────────────

fn align_up(value: u32, align: u32) -> u32 {
    if align == 0 { return value; }
    (value + align - 1) & !(align - 1)
}

/// Dynamic string table builder.
struct DynStrTab {
    data: Vec<u8>,
    offsets: HashMap<String, u32>,
}

impl DynStrTab {
    fn new() -> Self {
        Self { data: Vec::new(), offsets: HashMap::new() }
    }

    fn add(&mut self, s: &str) -> u32 {
        if let Some(&off) = self.offsets.get(s) {
            return off;
        }
        let off = self.data.len() as u32;
        self.data.extend_from_slice(s.as_bytes());
        self.data.push(0);
        self.offsets.insert(s.to_string(), off);
        off
    }

    fn offset_of(&self, s: &str) -> Option<u32> {
        self.offsets.get(s).copied()
    }

    fn finish(&self) -> Vec<u8> {
        self.data.clone()
    }
}

/// SysV hash function for ELF (used by .gnu.version_r entries).
fn elf_hash(name: &[u8]) -> u32 {
    crate::backend::linker_common::sysv_hash(name)
}

fn elf_hash_str(name: &str) -> u32 {
    elf_hash(name.as_bytes())
}

/// Build a .gnu.hash section for ELF32.
///
/// Uses 32-bit bloom filter words (ELF32 word size) and the GNU hash algorithm.
/// This matches the .gnu.hash format used by x86-64 and RISC-V linkers, adapted
/// for 32-bit ELF. The hashed symbols are copy-reloc and exported symbols that
/// need to be findable by the dynamic linker.
///
/// `hashed_names`: names of symbols that go into the hash (at indices >= symoffset)
/// `symoffset`: first hashed symbol's index in .dynsym
///
/// Returns `(hash_data, sorted_indices)` where `sorted_indices` maps new position
/// to original index in `hashed_names`, so the caller can reorder dynsym entries
/// to match the hash table's expected order.
fn build_gnu_hash_32(hashed_names: &[String], symoffset: u32) -> (Vec<u8>, Vec<usize>) {
    let num_hashed = hashed_names.len();
    let nbuckets = if num_hashed == 0 { 1 } else { num_hashed.next_power_of_two().max(1) } as u32;
    let bloom_size: u32 = 1; // Single 32-bit bloom word for ELF32
    let bloom_shift: u32 = 5; // Shift for second bloom bit (< 32 for 32-bit words)

    // Compute hashes for all hashed symbols
    let orig_hashes: Vec<u32> = hashed_names.iter()
        .map(|name| crate::backend::linker_common::gnu_hash(name.as_bytes()))
        .collect();

    // Sort hashed symbols by bucket for proper chain grouping
    let mut indices: Vec<usize> = (0..num_hashed).collect();
    indices.sort_by_key(|&i| orig_hashes[i] % nbuckets);
    let sym_hashes: Vec<u32> = indices.iter().map(|&i| orig_hashes[i]).collect();

    // Build bloom filter (single 32-bit word for ELF32)
    let mut bloom_word: u32 = 0;
    for &h in &sym_hashes {
        let bit1 = h % 32;
        let bit2 = (h >> bloom_shift) % 32;
        bloom_word |= 1u32 << bit1;
        bloom_word |= 1u32 << bit2;
    }

    // Build buckets and chains
    let mut buckets = vec![0u32; nbuckets as usize];
    let mut chains = vec![0u32; num_hashed];
    for (i, &h) in sym_hashes.iter().enumerate() {
        let bucket = (h % nbuckets) as usize;
        if buckets[bucket] == 0 {
            buckets[bucket] = symoffset + i as u32;
        }
        // Chain value = hash with bit 0 cleared (will set for last in chain)
        chains[i] = h & !1;
    }
    // Mark the last symbol in each bucket chain with bit 0 set
    for bucket_idx in 0..nbuckets as usize {
        if buckets[bucket_idx] == 0 { continue; }
        let mut last_in_bucket = 0;
        for (i, &h) in sym_hashes.iter().enumerate() {
            if (h % nbuckets) as usize == bucket_idx {
                last_in_bucket = i;
            }
        }
        chains[last_in_bucket] |= 1; // end-of-chain bit
    }

    // Serialize: header(16) + bloom(bloom_size*4) + buckets(nbuckets*4) + chains(num_hashed*4)
    let mut data = Vec::new();
    data.extend_from_slice(&nbuckets.to_le_bytes());
    data.extend_from_slice(&symoffset.to_le_bytes());
    data.extend_from_slice(&bloom_size.to_le_bytes());
    data.extend_from_slice(&bloom_shift.to_le_bytes());
    // Bloom filter (32-bit words for ELF32)
    data.extend_from_slice(&bloom_word.to_le_bytes());
    // Buckets
    for &b in &buckets {
        data.extend_from_slice(&b.to_le_bytes());
    }
    // Chains
    for &c in &chains {
        data.extend_from_slice(&c.to_le_bytes());
    }
    (data, indices)
}
