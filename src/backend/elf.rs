//! Shared ELF types, constants, and utilities used by all assembler and linker backends.
//!
//! This module eliminates duplication of ELF infrastructure across x86, i686, ARM,
//! and RISC-V backends. It provides:
//!
//! - ELF format constants (section types, flags, symbol bindings, etc.)
//! - `StringTable` for building ELF string tables (.strtab, .shstrtab, .dynstr)
//! - Binary read/write helpers for little-endian ELF fields
//! - `write_shdr64` / `write_shdr32` for section header emission
//! - `parse_archive_members` for reading `.a` static archives
//! - `parse_thin_archive_members` for reading GNU thin archives
//! - `parse_linker_script` for handling linker script GROUP directives
//! - `ElfWriterBase` for shared assembler ELF writer state (ARM/RISC-V)

use std::collections::{HashMap, HashSet};

// ── ELF identification ───────────────────────────────────────────────────────

pub const ELF_MAGIC: [u8; 4] = [0x7f, b'E', b'L', b'F'];

// ELF class
pub const ELFCLASS32: u8 = 1;
pub const ELFCLASS64: u8 = 2;

// Data encoding
pub const ELFDATA2LSB: u8 = 1;

// Version
pub const EV_CURRENT: u8 = 1;

// OS/ABI
pub const ELFOSABI_NONE: u8 = 0;

// ── ELF object types ─────────────────────────────────────────────────────────

pub const ET_REL: u16 = 1;
pub const ET_EXEC: u16 = 2;
pub const ET_DYN: u16 = 3;

// ── Machine types ────────────────────────────────────────────────────────────

pub const EM_386: u16 = 3;
pub const EM_X86_64: u16 = 62;
pub const EM_AARCH64: u16 = 183;
pub const EM_RISCV: u16 = 243;

// ── Section header types ─────────────────────────────────────────────────────

pub const SHT_NULL: u32 = 0;
pub const SHT_PROGBITS: u32 = 1;
pub const SHT_SYMTAB: u32 = 2;
pub const SHT_STRTAB: u32 = 3;
pub const SHT_RELA: u32 = 4;
pub const SHT_HASH: u32 = 5;
pub const SHT_DYNAMIC: u32 = 6;
pub const SHT_NOTE: u32 = 7;
pub const SHT_NOBITS: u32 = 8;
pub const SHT_REL: u32 = 9;
pub const SHT_DYNSYM: u32 = 11;
pub const SHT_INIT_ARRAY: u32 = 14;
pub const SHT_FINI_ARRAY: u32 = 15;
pub const SHT_PREINIT_ARRAY: u32 = 16;
pub const SHT_GROUP: u32 = 17;

/// COMDAT group flag: sections in this group are deduplicated by the linker.
pub const GRP_COMDAT: u32 = 1;
pub const SHT_GNU_HASH: u32 = 0x6fff_fff6;
pub const SHT_GNU_VERSYM: u32 = 0x6fff_ffff;
pub const SHT_GNU_VERNEED: u32 = 0x6fff_fffe;
pub const SHT_GNU_VERDEF: u32 = 0x6fff_fffd;

// ── Section header flags ─────────────────────────────────────────────────────

pub const SHF_WRITE: u64 = 0x1;
pub const SHF_ALLOC: u64 = 0x2;
pub const SHF_EXECINSTR: u64 = 0x4;
pub const SHF_MERGE: u64 = 0x10;
pub const SHF_STRINGS: u64 = 0x20;
pub const SHF_INFO_LINK: u64 = 0x40;
pub const SHF_GROUP: u64 = 0x200;
pub const SHF_TLS: u64 = 0x400;
pub const SHF_EXCLUDE: u64 = 0x8000_0000;

// ── Symbol binding ───────────────────────────────────────────────────────────

pub const STB_LOCAL: u8 = 0;
pub const STB_GLOBAL: u8 = 1;
pub const STB_WEAK: u8 = 2;

// ── Symbol types ─────────────────────────────────────────────────────────────

pub const STT_NOTYPE: u8 = 0;
pub const STT_OBJECT: u8 = 1;
pub const STT_FUNC: u8 = 2;
pub const STT_SECTION: u8 = 3;
pub const STT_FILE: u8 = 4;
pub const STT_COMMON: u8 = 5;
pub const STT_TLS: u8 = 6;
pub const STT_GNU_IFUNC: u8 = 10;

// ── Symbol visibility ────────────────────────────────────────────────────────

pub const STV_DEFAULT: u8 = 0;
pub const STV_INTERNAL: u8 = 1;
pub const STV_HIDDEN: u8 = 2;
pub const STV_PROTECTED: u8 = 3;

// ── Special section indices ──────────────────────────────────────────────────

pub const SHN_UNDEF: u16 = 0;
pub const SHN_ABS: u16 = 0xfff1;
pub const SHN_COMMON: u16 = 0xfff2;

// ── Program header types ─────────────────────────────────────────────────────

pub const PT_NULL: u32 = 0;
pub const PT_LOAD: u32 = 1;
pub const PT_DYNAMIC: u32 = 2;
pub const PT_INTERP: u32 = 3;
pub const PT_NOTE: u32 = 4;
pub const PT_PHDR: u32 = 6;
pub const PT_TLS: u32 = 7;
pub const PT_GNU_EH_FRAME: u32 = 0x6474_e550;
pub const PT_GNU_STACK: u32 = 0x6474_e551;
pub const PT_GNU_RELRO: u32 = 0x6474_e552;

// ── Program header flags ─────────────────────────────────────────────────────

pub const PF_X: u32 = 0x1;
pub const PF_W: u32 = 0x2;
pub const PF_R: u32 = 0x4;

// ── Dynamic section tags ─────────────────────────────────────────────────────

pub const DT_NULL: i64 = 0;
pub const DT_NEEDED: i64 = 1;
pub const DT_PLTGOT: i64 = 3;
pub const DT_HASH: i64 = 4;
pub const DT_STRTAB: i64 = 5;
pub const DT_SYMTAB: i64 = 6;
pub const DT_RELA: i64 = 7;
pub const DT_RELASZ: i64 = 8;
pub const DT_RELAENT: i64 = 9;
pub const DT_STRSZ: i64 = 10;
pub const DT_SYMENT: i64 = 11;
pub const DT_INIT: i64 = 12;
pub const DT_FINI: i64 = 13;
pub const DT_SONAME: i64 = 14;
pub const DT_RPATH: i64 = 15;
pub const DT_REL: i64 = 17;
pub const DT_RELSZ: i64 = 18;
pub const DT_RELENT: i64 = 19;
pub const DT_JMPREL: i64 = 23;
pub const DT_PLTREL: i64 = 20;
pub const DT_PLTRELSZ: i64 = 2;
pub const DT_DEBUG: i64 = 21;
pub const DT_INIT_ARRAY: i64 = 25;
pub const DT_FINI_ARRAY: i64 = 26;
pub const DT_INIT_ARRAYSZ: i64 = 27;
pub const DT_FINI_ARRAYSZ: i64 = 28;
pub const DT_RUNPATH: i64 = 29;
pub const DT_FLAGS: i64 = 30;
pub const DT_PREINIT_ARRAY: i64 = 32;
pub const DT_PREINIT_ARRAYSZ: i64 = 33;
pub const DT_GNU_HASH: i64 = 0x6fff_fef5;
pub const DT_VERSYM: i64 = 0x6fff_fff0;
pub const DT_VERNEED: i64 = 0x6fff_fffe;
pub const DT_VERNEEDNUM: i64 = 0x6fff_ffff;
pub const DT_RELACOUNT: i64 = 0x6fff_fff9;
pub const DT_FLAGS_1: i64 = 0x6fff_fffb;

// ── ELF sizes ────────────────────────────────────────────────────────────────

/// Size of ELF64 header in bytes.
pub const ELF64_EHDR_SIZE: usize = 64;
/// Size of ELF32 header in bytes.
pub const ELF32_EHDR_SIZE: usize = 52;
/// Size of ELF64 section header in bytes.
pub const ELF64_SHDR_SIZE: usize = 64;
/// Size of ELF32 section header in bytes.
pub const ELF32_SHDR_SIZE: usize = 40;
/// Size of ELF64 symbol table entry in bytes.
pub const ELF64_SYM_SIZE: usize = 24;
/// Size of ELF32 symbol table entry in bytes.
pub const ELF32_SYM_SIZE: usize = 16;
/// Size of ELF64 RELA relocation entry in bytes.
pub const ELF64_RELA_SIZE: usize = 24;
/// Size of ELF32 REL relocation entry in bytes.
pub const ELF32_REL_SIZE: usize = 8;
/// Size of ELF64 program header in bytes.
pub const ELF64_PHDR_SIZE: usize = 56;
/// Size of ELF32 program header in bytes.
pub const ELF32_PHDR_SIZE: usize = 32;

// ── String table ─────────────────────────────────────────────────────────────

/// ELF string table builder. Used for .strtab, .shstrtab, and .dynstr sections.
///
/// Strings are stored as null-terminated entries. The table always starts with
/// a null byte (index 0 = empty string), matching ELF convention.
pub struct StringTable {
    data: Vec<u8>,
    offsets: HashMap<String, u32>,
}

impl StringTable {
    /// Create a new string table with the initial null byte.
    pub fn new() -> Self {
        Self {
            data: vec![0],
            offsets: HashMap::new(),
        }
    }

    /// Add a string to the table and return its offset.
    /// Returns 0 for empty strings. Deduplicates repeated insertions.
    pub fn add(&mut self, s: &str) -> u32 {
        if s.is_empty() {
            return 0;
        }
        if let Some(&offset) = self.offsets.get(s) {
            return offset;
        }
        let offset = self.data.len() as u32;
        self.data.extend_from_slice(s.as_bytes());
        self.data.push(0);
        self.offsets.insert(s.to_string(), offset);
        offset
    }

    /// Look up the offset of a previously-added string. Returns 0 if not found.
    pub fn offset_of(&self, s: &str) -> u32 {
        self.offsets.get(s).copied().unwrap_or(0)
    }

    /// Return the raw table bytes (including the leading null byte).
    pub fn as_bytes(&self) -> &[u8] {
        &self.data
    }

    /// Return the size of the table in bytes.
    pub fn len(&self) -> usize {
        self.data.len()
    }
}

// ── Binary read helpers (little-endian) ──────────────────────────────────────

/// Read a little-endian u16 from `data` at `offset`.
#[inline]
pub fn read_u16(data: &[u8], offset: usize) -> u16 {
    u16::from_le_bytes([data[offset], data[offset + 1]])
}

/// Read a little-endian u32 from `data` at `offset`.
#[inline]
pub fn read_u32(data: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes([
        data[offset], data[offset + 1], data[offset + 2], data[offset + 3],
    ])
}

/// Read a little-endian u64 from `data` at `offset`.
#[inline]
pub fn read_u64(data: &[u8], offset: usize) -> u64 {
    u64::from_le_bytes([
        data[offset], data[offset + 1], data[offset + 2], data[offset + 3],
        data[offset + 4], data[offset + 5], data[offset + 6], data[offset + 7],
    ])
}

/// Read a little-endian i32 from `data` at `offset`.
#[inline]
pub fn read_i32(data: &[u8], offset: usize) -> i32 {
    i32::from_le_bytes([
        data[offset], data[offset + 1], data[offset + 2], data[offset + 3],
    ])
}

/// Read a little-endian i64 from `data` at `offset`.
#[inline]
pub fn read_i64(data: &[u8], offset: usize) -> i64 {
    i64::from_le_bytes([
        data[offset], data[offset + 1], data[offset + 2], data[offset + 3],
        data[offset + 4], data[offset + 5], data[offset + 6], data[offset + 7],
    ])
}

/// Read a null-terminated string from a byte slice starting at `offset`.
pub fn read_cstr(data: &[u8], offset: usize) -> String {
    if offset >= data.len() {
        return String::new();
    }
    let end = data[offset..].iter().position(|&b| b == 0).unwrap_or(data.len() - offset);
    String::from_utf8_lossy(&data[offset..offset + end]).into_owned()
}

// ── Binary write helpers (little-endian, in-place) ───────────────────────────

/// Write a little-endian u16 into `buf` at `offset`. No-op if out of bounds.
#[inline]
pub fn w16(buf: &mut [u8], off: usize, val: u16) {
    if off + 2 <= buf.len() {
        buf[off..off + 2].copy_from_slice(&val.to_le_bytes());
    }
}

/// Write a little-endian u32 into `buf` at `offset`. No-op if out of bounds.
#[inline]
pub fn w32(buf: &mut [u8], off: usize, val: u32) {
    if off + 4 <= buf.len() {
        buf[off..off + 4].copy_from_slice(&val.to_le_bytes());
    }
}

/// Write a little-endian u64 into `buf` at `offset`. No-op if out of bounds.
#[inline]
pub fn w64(buf: &mut [u8], off: usize, val: u64) {
    if off + 8 <= buf.len() {
        buf[off..off + 8].copy_from_slice(&val.to_le_bytes());
    }
}

/// Copy `data` into `buf` starting at `off`. No-op if out of bounds.
#[inline]
pub fn write_bytes(buf: &mut [u8], off: usize, data: &[u8]) {
    let end = off + data.len();
    if end <= buf.len() {
        buf[off..end].copy_from_slice(data);
    }
}

// ── Section header writing ───────────────────────────────────────────────────

/// Append an ELF64 section header to `buf`.
pub fn write_shdr64(
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

/// Append an ELF32 section header to `buf`.
pub fn write_shdr32(
    buf: &mut Vec<u8>,
    sh_name: u32, sh_type: u32, sh_flags: u32,
    sh_addr: u32, sh_offset: u32, sh_size: u32,
    sh_link: u32, sh_info: u32, sh_addralign: u32, sh_entsize: u32,
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

/// Write an ELF64 program header to `buf` at offset `off`.
pub fn write_phdr64(
    buf: &mut [u8], off: usize,
    p_type: u32, p_flags: u32, p_offset: u64,
    p_vaddr: u64, p_paddr: u64, p_filesz: u64, p_memsz: u64, p_align: u64,
) {
    w32(buf, off, p_type);
    w32(buf, off + 4, p_flags);
    w64(buf, off + 8, p_offset);
    w64(buf, off + 16, p_vaddr);
    w64(buf, off + 24, p_paddr);
    w64(buf, off + 32, p_filesz);
    w64(buf, off + 40, p_memsz);
    w64(buf, off + 48, p_align);
}

/// Write an ELF64 symbol table entry to `buf`.
pub fn write_sym64(
    buf: &mut Vec<u8>,
    st_name: u32, st_info: u8, st_other: u8, st_shndx: u16,
    st_value: u64, st_size: u64,
) {
    buf.extend_from_slice(&st_name.to_le_bytes());
    buf.push(st_info);
    buf.push(st_other);
    buf.extend_from_slice(&st_shndx.to_le_bytes());
    buf.extend_from_slice(&st_value.to_le_bytes());
    buf.extend_from_slice(&st_size.to_le_bytes());
}

/// Write an ELF32 symbol table entry to `buf`.
pub fn write_sym32(
    buf: &mut Vec<u8>,
    st_name: u32, st_value: u32, st_size: u32,
    st_info: u8, st_other: u8, st_shndx: u16,
) {
    buf.extend_from_slice(&st_name.to_le_bytes());
    buf.extend_from_slice(&st_value.to_le_bytes());
    buf.extend_from_slice(&st_size.to_le_bytes());
    buf.push(st_info);
    buf.push(st_other);
    buf.extend_from_slice(&st_shndx.to_le_bytes());
}

/// Write an ELF64 RELA relocation entry to `buf`.
pub fn write_rela64(buf: &mut Vec<u8>, r_offset: u64, r_sym: u32, r_type: u32, r_addend: i64) {
    buf.extend_from_slice(&r_offset.to_le_bytes());
    let r_info: u64 = ((r_sym as u64) << 32) | (r_type as u64);
    buf.extend_from_slice(&r_info.to_le_bytes());
    buf.extend_from_slice(&r_addend.to_le_bytes());
}

/// Write an ELF32 REL relocation entry to `buf`.
pub fn write_rel32(buf: &mut Vec<u8>, r_offset: u32, r_sym: u32, r_type: u8) {
    buf.extend_from_slice(&r_offset.to_le_bytes());
    let r_info: u32 = (r_sym << 8) | (r_type as u32);
    buf.extend_from_slice(&r_info.to_le_bytes());
}

// ── Archive (.a) parsing ─────────────────────────────────────────────────────

/// Returns true if `data` starts with the thin archive magic `!<thin>\n`.
pub fn is_thin_archive(data: &[u8]) -> bool {
    data.len() >= 8 && &data[0..8] == b"!<thin>\n"
}

/// Parse a GNU thin archive (.a file with `!<thin>\n` magic), returning member
/// filenames (relative to the archive directory). In thin archives, member data
/// is NOT stored inline — the archive only contains headers and a name table.
/// The caller must read each file from disk.
pub fn parse_thin_archive_members(data: &[u8]) -> Result<Vec<String>, String> {
    if data.len() < 8 || &data[0..8] != b"!<thin>\n" {
        return Err("not a thin archive file".to_string());
    }

    let mut members = Vec::new();
    let mut pos = 8;
    let mut extended_names: Option<&[u8]> = None;

    while pos + 60 <= data.len() {
        let name_raw = &data[pos..pos + 16];
        let size_str = std::str::from_utf8(&data[pos + 48..pos + 58])
            .unwrap_or("")
            .trim();
        let magic = &data[pos + 58..pos + 60];
        if magic != b"`\n" {
            break;
        }

        let size: usize = size_str.parse().unwrap_or(0);
        let data_start = pos + 60;
        let name_str = std::str::from_utf8(name_raw).unwrap_or("").trim_end();

        if name_str == "/" || name_str == "/SYM64/" {
            // Symbol table — data is stored inline even in thin archives
            pos = data_start + size;
            if pos % 2 != 0 { pos += 1; }
            continue;
        } else if name_str == "//" {
            // Extended name table — also stored inline in thin archives
            extended_names = Some(&data[data_start..(data_start + size).min(data.len())]);
            pos = data_start + size;
            if pos % 2 != 0 { pos += 1; }
            continue;
        }

        // Regular member — in thin archives, data is NOT inline
        let member_name = if let Some(rest) = name_str.strip_prefix('/') {
            if let Some(ext) = extended_names {
                // The name field is like "/23607" or "/23607/" — extract just the digits
                let num_str = rest.trim_end_matches('/').trim();
                let name_off: usize = num_str.parse().unwrap_or(0);
                if name_off < ext.len() {
                    // In thin archives, names can contain '/' (path separators),
                    // so the terminator is the two-byte sequence "/\n", not just '/'.
                    let slice = &ext[name_off..];
                    let end = slice.windows(2)
                        .position(|w| w == b"/\n")
                        .unwrap_or_else(|| {
                            // Fall back to null byte or end of table
                            slice.iter()
                                .position(|&b| b == 0)
                                .unwrap_or(slice.len())
                        });
                    String::from_utf8_lossy(&ext[name_off..name_off + end]).to_string()
                } else {
                    name_str.to_string()
                }
            } else {
                name_str.to_string()
            }
        } else {
            name_str.trim_end_matches('/').to_string()
        };

        members.push(member_name);

        // In thin archives, member headers are consecutive (no inline data)
        pos = data_start;
        if pos % 2 != 0 { pos += 1; }
    }

    Ok(members)
}

/// Parse a GNU-format static archive (.a file), returning member entries as
/// `(name, data_offset, data_size)` tuples. The offsets are into the original
/// `data` slice, enabling zero-copy access.
///
/// Handles extended name tables (`//`), symbol tables (`/`, `/SYM64/`), and
/// 2-byte alignment padding between members.
pub fn parse_archive_members(data: &[u8]) -> Result<Vec<(String, usize, usize)>, String> {
    if data.len() < 8 || &data[0..8] != b"!<arch>\n" {
        return Err("not a valid archive file".to_string());
    }

    let mut members = Vec::new();
    let mut pos = 8;
    let mut extended_names: Option<&[u8]> = None;

    while pos + 60 <= data.len() {
        let name_raw = &data[pos..pos + 16];
        let size_str = std::str::from_utf8(&data[pos + 48..pos + 58])
            .unwrap_or("")
            .trim();
        let magic = &data[pos + 58..pos + 60];
        if magic != b"`\n" {
            break;
        }

        let size: usize = size_str.parse().unwrap_or(0);
        let data_start = pos + 60;
        let name_str = std::str::from_utf8(name_raw).unwrap_or("").trim_end();

        if name_str == "/" || name_str == "/SYM64/" {
            // Symbol table — skip
        } else if name_str == "//" {
            // Extended name table
            extended_names = Some(&data[data_start..(data_start + size).min(data.len())]);
        } else {
            let member_name = if let Some(rest) = name_str.strip_prefix('/') {
                // Extended name: /offset into extended names table
                if let Some(ext) = extended_names {
                    let name_off: usize = rest.trim_end_matches('/').parse().unwrap_or(0);
                    if name_off < ext.len() {
                        let end = ext[name_off..]
                            .iter()
                            .position(|&b| b == b'/' || b == b'\n' || b == 0)
                            .unwrap_or(ext.len() - name_off);
                        String::from_utf8_lossy(&ext[name_off..name_off + end]).to_string()
                    } else {
                        name_str.to_string()
                    }
                } else {
                    name_str.to_string()
                }
            } else {
                name_str.trim_end_matches('/').to_string()
            };

            if data_start + size <= data.len() {
                members.push((member_name, data_start, size));
            }
        }

        // Align to 2-byte boundary
        pos = data_start + size;
        if pos % 2 != 0 {
            pos += 1;
        }
    }

    Ok(members)
}

// ── Linker script parsing ────────────────────────────────────────────────────

/// Parse a linker script looking for `GROUP ( ... )` directives.
/// Returns the list of library paths referenced, or `None` if no GROUP found.
///
/// Filters out AS_NEEDED entries (these are optional shared libraries that
/// the static linker does not need to handle).
pub fn parse_linker_script(content: &str) -> Option<Vec<String>> {
    let group_start = content.find("GROUP")?;
    let paren_start = content[group_start..].find('(')?;
    let paren_end = content[group_start..].find(')')?;
    let inside = &content[group_start + paren_start + 1..group_start + paren_end];

    let mut paths = Vec::new();
    let mut in_as_needed = false;
    for token in inside.split_whitespace() {
        match token {
            "AS_NEEDED" => { in_as_needed = true; continue; }
            "(" => continue,
            ")" => { in_as_needed = false; continue; }
            _ => {}
        }
        if (token.starts_with('/') || token.ends_with(".so") || token.ends_with(".a") ||
           token.contains(".so."))
            && !in_as_needed {
                paths.push(token.to_string());
            }
    }

    if paths.is_empty() { None } else { Some(paths) }
}

// ── Linker-defined symbols ────────────────────────────────────────────────────
//
// All four backend linkers (x86-64, i686, ARM, RISC-V) need to define a standard
// set of symbols that programs expect the linker to provide. Previously each
// backend had its own list with inconsistent names and values. This shared
// infrastructure ensures all backends define the same symbols with consistent
// semantics.

/// Addresses that linker backends must provide for linker-defined symbol resolution.
///
/// Each backend computes these from its own layout, then passes them to
/// `get_standard_linker_symbols()` to get the canonical symbol list.
pub struct LinkerSymbolAddresses {
    /// Base address of the ELF executable (e.g., 0x400000 for x86-64).
    pub base_addr: u64,
    /// Address of the GOT or GOT.PLT section.
    pub got_addr: u64,
    /// Address of the .dynamic section (0 if static-only linking).
    pub dynamic_addr: u64,
    /// Start address of the BSS section.
    pub bss_addr: u64,
    /// Size of the BSS section in memory.
    pub bss_size: u64,
    /// End of the text (RX) segment.
    pub text_end: u64,
    /// Start of the data (RW) segment.
    pub data_start: u64,
    /// Start address of .init_array section (0 if absent).
    pub init_array_start: u64,
    /// Size of .init_array section in bytes.
    pub init_array_size: u64,
    /// Start address of .fini_array section (0 if absent).
    pub fini_array_start: u64,
    /// Size of .fini_array section in bytes.
    pub fini_array_size: u64,
    /// Start address of .preinit_array section (0 if absent).
    pub preinit_array_start: u64,
    /// Size of .preinit_array section in bytes.
    pub preinit_array_size: u64,
    /// Start address of .rela.iplt / .rel.iplt section (0 if absent).
    pub rela_iplt_start: u64,
    /// Size of .rela.iplt / .rel.iplt section in bytes.
    pub rela_iplt_size: u64,
}

/// A linker-defined symbol entry with name, value, and binding.
pub struct LinkerDefinedSym {
    pub name: &'static str,
    pub value: u64,
    pub binding: u8,
}

/// Return the standard set of linker-defined symbols that all backends should provide.
///
/// This ensures consistent symbol definitions across x86-64, i686, ARM, and RISC-V.
/// Each backend may also define additional architecture-specific symbols (e.g.,
/// `__global_pointer$` for RISC-V) after calling this function.
///
/// The returned list uses the same semantics as GNU ld:
/// - `_edata` / `__bss_start` = start of BSS (end of initialized data)
/// - `_end` / `__end` = end of BSS (end of all data)
/// - `_etext` / `etext` = end of text segment
/// - `__dso_handle` = start of data segment
/// - `_DYNAMIC` = address of .dynamic section
/// - `data_start` is weak (can be overridden by object files)
pub fn get_standard_linker_symbols(addrs: &LinkerSymbolAddresses) -> Vec<LinkerDefinedSym> {
    let end_addr = addrs.bss_addr + addrs.bss_size;
    vec![
        // GOT / dynamic
        LinkerDefinedSym { name: "_GLOBAL_OFFSET_TABLE_", value: addrs.got_addr, binding: STB_GLOBAL },
        LinkerDefinedSym { name: "_DYNAMIC", value: addrs.dynamic_addr, binding: STB_GLOBAL },
        // BSS / data boundaries
        LinkerDefinedSym { name: "__bss_start", value: addrs.bss_addr, binding: STB_GLOBAL },
        LinkerDefinedSym { name: "_edata", value: addrs.bss_addr, binding: STB_GLOBAL },
        LinkerDefinedSym { name: "_end", value: end_addr, binding: STB_GLOBAL },
        LinkerDefinedSym { name: "__end", value: end_addr, binding: STB_GLOBAL },
        // Text boundaries
        LinkerDefinedSym { name: "_etext", value: addrs.text_end, binding: STB_GLOBAL },
        LinkerDefinedSym { name: "etext", value: addrs.text_end, binding: STB_GLOBAL },
        // ELF header / executable start
        LinkerDefinedSym { name: "__ehdr_start", value: addrs.base_addr, binding: STB_GLOBAL },
        LinkerDefinedSym { name: "__executable_start", value: addrs.base_addr, binding: STB_GLOBAL },
        // Data segment
        LinkerDefinedSym { name: "__dso_handle", value: addrs.data_start, binding: STB_GLOBAL },
        LinkerDefinedSym { name: "__data_start", value: addrs.data_start, binding: STB_GLOBAL },
        LinkerDefinedSym { name: "data_start", value: addrs.data_start, binding: STB_WEAK },
        // Init/fini/preinit arrays
        LinkerDefinedSym { name: "__init_array_start", value: addrs.init_array_start, binding: STB_GLOBAL },
        LinkerDefinedSym { name: "__init_array_end", value: addrs.init_array_start + addrs.init_array_size, binding: STB_GLOBAL },
        LinkerDefinedSym { name: "__fini_array_start", value: addrs.fini_array_start, binding: STB_GLOBAL },
        LinkerDefinedSym { name: "__fini_array_end", value: addrs.fini_array_start + addrs.fini_array_size, binding: STB_GLOBAL },
        LinkerDefinedSym { name: "__preinit_array_start", value: addrs.preinit_array_start, binding: STB_GLOBAL },
        LinkerDefinedSym { name: "__preinit_array_end", value: addrs.preinit_array_start + addrs.preinit_array_size, binding: STB_GLOBAL },
        // IPLT relocation boundaries
        LinkerDefinedSym { name: "__rela_iplt_start", value: addrs.rela_iplt_start, binding: STB_GLOBAL },
        LinkerDefinedSym { name: "__rela_iplt_end", value: addrs.rela_iplt_start + addrs.rela_iplt_size, binding: STB_GLOBAL },
    ]
}

// ── Section name mapping ─────────────────────────────────────────────────────

// ── Assembler helpers ────────────────────────────────────────────────────────

/// Map a symbol's section name to its index in the section header table.
///
/// Handles special pseudo-sections used during assembly:
/// - `*COM*` → `SHN_COMMON` (0xFFF2) for COMMON symbols
/// - `*UND*` or empty → `SHN_UNDEF` (0) for undefined symbols
/// - Otherwise, looks up the section in the content section list (1-based index)
/// `shndx_offset` is the number of section headers before the content sections
/// (typically 1 for NULL, or 1 + num_groups when COMDAT groups are present).
pub fn section_index(section_name: &str, content_sections: &[String], shndx_offset: u16) -> u16 {
    if section_name == "*COM*" {
        SHN_COMMON
    } else if section_name == "*UND*" || section_name.is_empty() {
        SHN_UNDEF
    } else {
        content_sections.iter().position(|s| s == section_name)
            .map(|i| (i as u16) + shndx_offset)
            .unwrap_or(SHN_UNDEF)
    }
}

/// Return default ELF section flags based on section name conventions.
///
/// These are the standard mappings: `.text.*` → alloc+exec, `.data.*` → alloc+write,
/// `.rodata.*` → alloc, `.bss.*` → alloc+write, `.tdata`/`.tbss` → alloc+write+TLS, etc.
pub fn default_section_flags(name: &str) -> u64 {
    if name == ".text" || name.starts_with(".text.") {
        SHF_ALLOC | SHF_EXECINSTR
    } else if name == ".data" || name.starts_with(".data.")
        || name == ".bss" || name.starts_with(".bss.") {
        SHF_ALLOC | SHF_WRITE
    } else if name == ".rodata" || name.starts_with(".rodata.") {
        SHF_ALLOC
    } else if name == ".note.GNU-stack" {
        0 // Non-executable stack marker, no flags
    } else if name.starts_with(".note") {
        SHF_ALLOC
    } else if name.starts_with(".tdata") || name.starts_with(".tbss") {
        SHF_ALLOC | SHF_WRITE | SHF_TLS
    } else if name.starts_with(".init") || name.starts_with(".fini") {
        SHF_ALLOC | SHF_EXECINSTR
    } else {
        0
    }
}

// ── Shared relocatable object writer ─────────────────────────────────────────
//
// This section provides a unified `write_relocatable_object` function that
// serializes an ELF .o file from arch-independent section/symbol/reloc data.
// It handles both ELF64+RELA (x86-64, AArch64, RISC-V) and ELF32+REL (i686)
// formats, eliminating ~1200 lines of duplicated serialization code across
// the four backend assemblers.
//
// Each backend's ElfWriter builds its own section/symbol/reloc data through
// arch-specific logic (instruction encoding, branch resolution, etc.), then
// calls `write_relocatable_object` for the final ELF serialization step.

/// Configuration for ELF object file emission. Parameterizes the format
/// differences between architectures (machine type, ELF class, flags, etc).
pub struct ElfConfig {
    /// ELF machine type (e.g., EM_X86_64, EM_AARCH64, EM_RISCV, EM_386)
    pub e_machine: u16,
    /// ELF flags (e.g., 0 for most, EF_RISCV_RVC | EF_RISCV_FLOAT_ABI_DOUBLE for RISC-V)
    pub e_flags: u32,
    /// ELF class: ELFCLASS64 or ELFCLASS32
    pub elf_class: u8,
}

/// A section in a relocatable object file being built by the assembler.
pub struct ObjSection {
    pub name: String,
    pub sh_type: u32,
    pub sh_flags: u64,
    pub data: Vec<u8>,
    pub sh_addralign: u64,
    /// Relocations targeting this section.
    pub relocs: Vec<ObjReloc>,
    /// If this section is part of a COMDAT group, the group signature symbol name.
    pub comdat_group: Option<String>,
}

/// A relocation entry in a relocatable object file.
///
/// Uses 64-bit offset/addend for all targets; the writer truncates to 32-bit
/// for ELF32/REL when needed.
pub struct ObjReloc {
    pub offset: u64,
    pub reloc_type: u32,
    pub symbol_name: String,
    pub addend: i64,
}

/// A symbol in a relocatable object file.
pub struct ObjSymbol {
    pub name: String,
    pub value: u64,
    pub size: u64,
    pub binding: u8,
    pub sym_type: u8,
    pub visibility: u8,
    /// Section name, or "*COM*" for COMMON, "*UND*" or empty for undefined.
    pub section_name: String,
}

/// Internal symbol table entry used during serialization.
struct SymEntry {
    st_name: u32,
    st_info: u8,
    st_other: u8,
    st_shndx: u16,
    st_value: u64,
    st_size: u64,
}

/// Write a relocatable ELF object file (.o) from assembled sections and symbols.
///
/// This is the shared serialization pipeline used by all four backend assemblers.
/// The caller is responsible for:
/// - Building sections with encoded instructions and data
/// - Resolving local branches and patching instruction bytes
/// - Building the symbol list (defined labels, COMMON, aliases, undefined)
/// - Providing the correct `ElfConfig` for the target architecture
///
/// The function handles the complete ELF layout and serialization:
/// 1. Build shstrtab/strtab string tables
/// 2. Build symbol table entries (NULL, section symbols, local, global)
/// 3. Compute section/rela/symtab/strtab layout offsets
/// 4. Write ELF header, section data, relocations, symtab, strtab, section headers
///
/// Returns the serialized ELF bytes on success.
pub fn write_relocatable_object(
    config: &ElfConfig,
    section_order: &[String],
    sections: &HashMap<String, ObjSection>,
    symbols: &[ObjSymbol],
) -> Result<Vec<u8>, String> {
    let is_32bit = config.elf_class == ELFCLASS32;
    let use_rela = !is_32bit; // ELF64 uses RELA, ELF32 uses REL

    let ehdr_size = if is_32bit { ELF32_EHDR_SIZE } else { ELF64_EHDR_SIZE };
    let shdr_size = if is_32bit { ELF32_SHDR_SIZE } else { ELF64_SHDR_SIZE };
    let sym_entry_size = if is_32bit { ELF32_SYM_SIZE } else { ELF64_SYM_SIZE };
    let reloc_entry_size = if use_rela { ELF64_RELA_SIZE } else { ELF32_REL_SIZE };
    let reloc_prefix = if use_rela { ".rela" } else { ".rel" };
    let reloc_sh_type = if use_rela { SHT_RELA } else { SHT_REL };
    let alignment_mask = if is_32bit { 3usize } else { 7usize }; // 4 or 8 byte alignment

    // ── Collect COMDAT groups ──
    // Map: group_name -> list of member content section names
    let mut comdat_groups: Vec<(String, Vec<String>)> = Vec::new();
    {
        let mut group_map: HashMap<String, Vec<String>> = HashMap::new();
        let mut group_order: Vec<String> = Vec::new();
        for sec_name in section_order {
            if let Some(section) = sections.get(sec_name) {
                if let Some(ref group_name) = section.comdat_group {
                    group_map.entry(group_name.clone()).or_insert_with(|| {
                        group_order.push(group_name.clone());
                        Vec::new()
                    }).push(sec_name.clone());
                }
            }
        }
        for gname in group_order {
            if let Some(members) = group_map.remove(&gname) {
                comdat_groups.push((gname, members));
            }
        }
    }
    let num_groups = comdat_groups.len();

    // ── Build string tables ──
    let mut shstrtab = StringTable::new();
    let mut strtab = StringTable::new();

    let content_sections: &[String] = section_order;

    // Add group section names to shstrtab
    for _ in &comdat_groups {
        shstrtab.add(".group");
    }

    // Add section names to shstrtab
    for sec_name in content_sections {
        shstrtab.add(sec_name);
    }
    shstrtab.add(".symtab");
    shstrtab.add(".strtab");
    shstrtab.add(".shstrtab");

    // Build reloc section names
    let mut reloc_section_names: Vec<String> = Vec::new();
    for sec_name in content_sections {
        if let Some(section) = sections.get(sec_name) {
            if !section.relocs.is_empty() {
                let reloc_name = format!("{}{}", reloc_prefix, sec_name);
                shstrtab.add(&reloc_name);
                reloc_section_names.push(reloc_name);
            }
        }
    }

    // ── Build symbol table entries ──
    let mut sym_entries: Vec<SymEntry> = Vec::new();
    // Content sections start at shdr index: NULL + num_groups + content_index
    let content_shndx_offset = (num_groups + 1) as u16;

    // NULL symbol (index 0)
    sym_entries.push(SymEntry {
        st_name: 0, st_info: 0, st_other: 0,
        st_shndx: 0, st_value: 0, st_size: 0,
    });

    // Section symbols (one per content section)
    for (i, sec_name) in content_sections.iter().enumerate() {
        strtab.add(sec_name);
        sym_entries.push(SymEntry {
            st_name: strtab.offset_of(sec_name),
            st_info: (STB_LOCAL << 4) | STT_SECTION,
            st_other: 0,
            st_shndx: content_shndx_offset + i as u16,
            st_value: 0,
            st_size: 0,
        });
    }

    // Separate local and global symbols
    let mut local_syms: Vec<&ObjSymbol> = Vec::new();
    let mut global_syms: Vec<&ObjSymbol> = Vec::new();
    for sym in symbols {
        if sym.binding == STB_LOCAL {
            local_syms.push(sym);
        } else {
            global_syms.push(sym);
        }
    }

    let first_global_idx = sym_entries.len() + local_syms.len();

    for sym in &local_syms {
        let name_offset = strtab.add(&sym.name);
        let shndx = section_index(&sym.section_name, content_sections, content_shndx_offset);
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
        let shndx = section_index(&sym.section_name, content_sections, content_shndx_offset);
        sym_entries.push(SymEntry {
            st_name: name_offset,
            st_info: (sym.binding << 4) | sym.sym_type,
            st_other: sym.visibility,
            st_shndx: shndx,
            st_value: sym.value,
            st_size: sym.size,
        });
    }

    // ── Build COMDAT group section data ──
    // Each group section contains: GRP_COMDAT flag (u32) + member section indices (u32 each)
    let mut group_section_data: Vec<Vec<u8>> = Vec::new();
    for (_group_name, members) in &comdat_groups {
        let mut data = Vec::with_capacity(4 + 4 * members.len());
        data.extend_from_slice(&GRP_COMDAT.to_le_bytes());
        for member_name in members {
            // Find the section header index of this member
            let member_idx = content_sections.iter().position(|s| s == member_name)
                .map(|i| content_shndx_offset as u32 + i as u32)
                .unwrap_or(0);
            data.extend_from_slice(&member_idx.to_le_bytes());
        }
        group_section_data.push(data);
    }

    // ── Calculate layout ──
    let mut offset = ehdr_size;

    // Group section offsets (come first, before content sections)
    let mut group_offsets: Vec<usize> = Vec::new();
    for gdata in &group_section_data {
        offset = (offset + 3) & !3; // align to 4 bytes
        group_offsets.push(offset);
        offset += gdata.len();
    }

    // Content section offsets
    let mut section_offsets: Vec<usize> = Vec::new();
    for sec_name in content_sections {
        let section = sections.get(sec_name).unwrap();
        let align = section.sh_addralign.max(1) as usize;
        offset = (offset + align - 1) & !(align - 1);
        section_offsets.push(offset);
        if section.sh_type != SHT_NOBITS {
            offset += section.data.len();
        }
    }

    // Reloc section offsets
    let mut reloc_offsets: Vec<usize> = Vec::new();
    for sec_name in content_sections {
        if let Some(section) = sections.get(sec_name) {
            if !section.relocs.is_empty() {
                offset = (offset + alignment_mask) & !alignment_mask;
                reloc_offsets.push(offset);
                offset += section.relocs.len() * reloc_entry_size;
            }
        }
    }

    // Symtab offset
    offset = (offset + alignment_mask) & !alignment_mask;
    let symtab_offset = offset;
    let symtab_size = sym_entries.len() * sym_entry_size;
    offset += symtab_size;

    // Strtab offset
    let strtab_offset = offset;
    let strtab_data = strtab.as_bytes().to_vec();
    offset += strtab_data.len();

    // Shstrtab offset
    let shstrtab_offset = offset;
    let shstrtab_data = shstrtab.as_bytes().to_vec();
    offset += shstrtab_data.len();

    // Section headers offset
    offset = (offset + alignment_mask) & !alignment_mask;
    let shdr_offset = offset;

    // Total section count: NULL + groups + content + relocs + symtab + strtab + shstrtab
    let num_sections = 1 + num_groups + content_sections.len() + reloc_section_names.len() + 3;
    let shstrtab_idx = num_sections - 1;
    let symtab_shndx = 1 + num_groups + content_sections.len() + reloc_section_names.len();

    // ── Write ELF ──
    let total_size = shdr_offset + num_sections * shdr_size;
    let mut elf = Vec::with_capacity(total_size);

    // ELF header (e_ident)
    elf.extend_from_slice(&ELF_MAGIC);
    elf.push(config.elf_class);
    elf.push(ELFDATA2LSB);
    elf.push(EV_CURRENT);
    elf.push(ELFOSABI_NONE);
    elf.extend_from_slice(&[0u8; 8]); // padding

    if is_32bit {
        // ELF32 header
        elf.extend_from_slice(&ET_REL.to_le_bytes());
        elf.extend_from_slice(&config.e_machine.to_le_bytes());
        elf.extend_from_slice(&1u32.to_le_bytes()); // e_version
        elf.extend_from_slice(&0u32.to_le_bytes()); // e_entry
        elf.extend_from_slice(&0u32.to_le_bytes()); // e_phoff
        elf.extend_from_slice(&(shdr_offset as u32).to_le_bytes());
        elf.extend_from_slice(&config.e_flags.to_le_bytes());
        elf.extend_from_slice(&(ehdr_size as u16).to_le_bytes());
        elf.extend_from_slice(&0u16.to_le_bytes()); // e_phentsize
        elf.extend_from_slice(&0u16.to_le_bytes()); // e_phnum
        elf.extend_from_slice(&(shdr_size as u16).to_le_bytes());
        elf.extend_from_slice(&(num_sections as u16).to_le_bytes());
        elf.extend_from_slice(&(shstrtab_idx as u16).to_le_bytes());
    } else {
        // ELF64 header
        elf.extend_from_slice(&ET_REL.to_le_bytes());
        elf.extend_from_slice(&config.e_machine.to_le_bytes());
        elf.extend_from_slice(&1u32.to_le_bytes()); // e_version
        elf.extend_from_slice(&0u64.to_le_bytes()); // e_entry
        elf.extend_from_slice(&0u64.to_le_bytes()); // e_phoff
        elf.extend_from_slice(&(shdr_offset as u64).to_le_bytes());
        elf.extend_from_slice(&config.e_flags.to_le_bytes());
        elf.extend_from_slice(&(ehdr_size as u16).to_le_bytes());
        elf.extend_from_slice(&0u16.to_le_bytes()); // e_phentsize
        elf.extend_from_slice(&0u16.to_le_bytes()); // e_phnum
        elf.extend_from_slice(&(shdr_size as u16).to_le_bytes());
        elf.extend_from_slice(&(num_sections as u16).to_le_bytes());
        elf.extend_from_slice(&(shstrtab_idx as u16).to_le_bytes());
    }

    debug_assert_eq!(elf.len(), ehdr_size);

    // ── Write group section data ──
    for (gi, gdata) in group_section_data.iter().enumerate() {
        while elf.len() < group_offsets[gi] {
            elf.push(0);
        }
        elf.extend_from_slice(gdata);
    }

    // ── Write content section data ──
    for (i, sec_name) in content_sections.iter().enumerate() {
        let section = sections.get(sec_name).unwrap();
        while elf.len() < section_offsets[i] {
            elf.push(0);
        }
        if section.sh_type != SHT_NOBITS {
            elf.extend_from_slice(&section.data);
        }
    }

    // ── Write relocation section data ──
    let mut reloc_idx = 0;
    for sec_name in content_sections {
        if let Some(section) = sections.get(sec_name) {
            if !section.relocs.is_empty() {
                while elf.len() < reloc_offsets[reloc_idx] {
                    elf.push(0);
                }
                for reloc in &section.relocs {
                    let sym_idx = find_symbol_index_shared(
                        &reloc.symbol_name, &sym_entries, &strtab, content_sections,
                    );
                    if use_rela {
                        write_rela64(&mut elf, reloc.offset, sym_idx, reloc.reloc_type, reloc.addend);
                    } else {
                        debug_assert!(reloc.reloc_type <= 255, "ELF32 reloc type must fit in u8");
                        write_rel32(&mut elf, reloc.offset as u32, sym_idx, reloc.reloc_type as u8);
                    }
                }
                reloc_idx += 1;
            }
        }
    }

    // ── Write symtab ──
    while elf.len() < symtab_offset {
        elf.push(0);
    }
    for sym in &sym_entries {
        if is_32bit {
            write_sym32(&mut elf, sym.st_name, sym.st_value as u32, sym.st_size as u32,
                       sym.st_info, sym.st_other, sym.st_shndx);
        } else {
            write_sym64(&mut elf, sym.st_name, sym.st_info, sym.st_other,
                       sym.st_shndx, sym.st_value, sym.st_size);
        }
    }

    // ── Write strtab ──
    debug_assert_eq!(elf.len(), strtab_offset);
    elf.extend_from_slice(&strtab_data);

    // ── Write shstrtab ──
    debug_assert_eq!(elf.len(), shstrtab_offset);
    elf.extend_from_slice(&shstrtab_data);

    // ── Write section headers ──
    while elf.len() < shdr_offset {
        elf.push(0);
    }

    let strtab_shndx = symtab_shndx + 1;

    if is_32bit {
        // NULL
        write_shdr32(&mut elf, 0, SHT_NULL, 0, 0, 0, 0, 0, 0, 0, 0);
        // Group sections (COMDAT)
        for (gi, (group_name, _members)) in comdat_groups.iter().enumerate() {
            let sh_name = shstrtab.offset_of(".group");
            // sh_link = symtab index, sh_info = symbol index of group signature
            let sig_sym_idx = find_symbol_index_shared(group_name, &sym_entries, &strtab, content_sections);
            write_shdr32(&mut elf, sh_name, SHT_GROUP, 0,
                        0, group_offsets[gi] as u32, group_section_data[gi].len() as u32,
                        symtab_shndx as u32, sig_sym_idx,
                        4, 4);
        }
        // Content sections
        for (i, sec_name) in content_sections.iter().enumerate() {
            let section = sections.get(sec_name).unwrap();
            let sh_name = shstrtab.offset_of(sec_name);
            let sh_offset = if section.sh_type == SHT_NOBITS { 0 } else { section_offsets[i] as u32 };
            write_shdr32(&mut elf, sh_name, section.sh_type, section.sh_flags as u32,
                        0, sh_offset, section.data.len() as u32,
                        0, 0, section.sh_addralign as u32, 0);
        }
        // Reloc sections
        reloc_idx = 0;
        for (i, sec_name) in content_sections.iter().enumerate() {
            if let Some(section) = sections.get(sec_name) {
                if !section.relocs.is_empty() {
                    let reloc_name = format!("{}{}", reloc_prefix, sec_name);
                    let sh_name = shstrtab.offset_of(&reloc_name);
                    let sh_offset = reloc_offsets[reloc_idx] as u32;
                    let sh_size = (section.relocs.len() * reloc_entry_size) as u32;
                    write_shdr32(&mut elf, sh_name, reloc_sh_type, 0,
                                0, sh_offset, sh_size,
                                symtab_shndx as u32, content_shndx_offset as u32 + i as u32,
                                4, reloc_entry_size as u32);
                    reloc_idx += 1;
                }
            }
        }
        // .symtab
        write_shdr32(&mut elf, shstrtab.offset_of(".symtab"), SHT_SYMTAB, 0,
                    0, symtab_offset as u32, symtab_size as u32,
                    strtab_shndx as u32, first_global_idx as u32,
                    4, sym_entry_size as u32);
        // .strtab
        write_shdr32(&mut elf, shstrtab.offset_of(".strtab"), SHT_STRTAB, 0,
                    0, strtab_offset as u32, strtab_data.len() as u32, 0, 0, 1, 0);
        // .shstrtab
        write_shdr32(&mut elf, shstrtab.offset_of(".shstrtab"), SHT_STRTAB, 0,
                    0, shstrtab_offset as u32, shstrtab_data.len() as u32, 0, 0, 1, 0);
    } else {
        // NULL
        write_shdr64(&mut elf, 0, SHT_NULL, 0, 0, 0, 0, 0, 0, 0, 0);
        // Group sections (COMDAT)
        for (gi, (group_name, _members)) in comdat_groups.iter().enumerate() {
            let sh_name = shstrtab.offset_of(".group");
            let sig_sym_idx = find_symbol_index_shared(group_name, &sym_entries, &strtab, content_sections);
            write_shdr64(&mut elf, sh_name, SHT_GROUP, 0,
                        0, group_offsets[gi] as u64, group_section_data[gi].len() as u64,
                        symtab_shndx as u32, sig_sym_idx,
                        4, 4);
        }
        // Content sections
        for (i, sec_name) in content_sections.iter().enumerate() {
            let section = sections.get(sec_name).unwrap();
            let sh_name = shstrtab.offset_of(sec_name);
            let sh_offset = if section.sh_type == SHT_NOBITS { 0 } else { section_offsets[i] as u64 };
            write_shdr64(&mut elf, sh_name, section.sh_type, section.sh_flags,
                        0, sh_offset, section.data.len() as u64,
                        0, 0, section.sh_addralign, 0);
        }
        // Reloc sections
        reloc_idx = 0;
        for (i, sec_name) in content_sections.iter().enumerate() {
            if let Some(section) = sections.get(sec_name) {
                if !section.relocs.is_empty() {
                    let reloc_name = format!("{}{}", reloc_prefix, sec_name);
                    let sh_name = shstrtab.offset_of(&reloc_name);
                    let sh_offset = reloc_offsets[reloc_idx] as u64;
                    let sh_size = (section.relocs.len() * reloc_entry_size) as u64;
                    write_shdr64(&mut elf, sh_name, reloc_sh_type, SHF_INFO_LINK,
                                0, sh_offset, sh_size,
                                symtab_shndx as u32, content_shndx_offset as u32 + i as u32,
                                8, reloc_entry_size as u64);
                    reloc_idx += 1;
                }
            }
        }
        // .symtab
        write_shdr64(&mut elf, shstrtab.offset_of(".symtab"), SHT_SYMTAB, 0,
                    0, symtab_offset as u64, symtab_size as u64,
                    strtab_shndx as u32, first_global_idx as u32,
                    8, sym_entry_size as u64);
        // .strtab
        write_shdr64(&mut elf, shstrtab.offset_of(".strtab"), SHT_STRTAB, 0,
                    0, strtab_offset as u64, strtab_data.len() as u64, 0, 0, 1, 0);
        // .shstrtab
        write_shdr64(&mut elf, shstrtab.offset_of(".shstrtab"), SHT_STRTAB, 0,
                    0, shstrtab_offset as u64, shstrtab_data.len() as u64, 0, 0, 1, 0);
    }

    Ok(elf)
}

/// Find a symbol's index in the ELF symbol table.
///
/// Checks section names first (returns section symbol index), then searches
/// by string table offset for named symbols (excluding section symbols).
fn find_symbol_index_shared(
    name: &str,
    sym_entries: &[SymEntry],
    strtab: &StringTable,
    content_sections: &[String],
) -> u32 {
    // Check if it's a section symbol
    for (i, sec_name) in content_sections.iter().enumerate() {
        if sec_name == name {
            return (i + 1) as u32; // +1 for NULL entry
        }
    }
    // Search named symbols
    let name_offset = strtab.offset_of(name);
    for (i, entry) in sym_entries.iter().enumerate() {
        if entry.st_name == name_offset && entry.st_info & 0xF != STT_SECTION {
            return i as u32;
        }
    }
    0 // undefined
}

// ── Shared numeric label resolution ──────────────────────────────────────
//
// GNU assembler numeric labels (e.g., `1:`, `42:`) can be defined multiple
// times. Forward references (`1f`) resolve to the NEXT definition, backward
// references (`1b`) resolve to the MOST RECENT definition.
//
// This module provides the resolution logic shared by x86 and i686 ELF
// writers. Both architectures use the same x86 parser AST types
// (AsmItem, Operand, Displacement, DataValue, etc.), so these functions
// operate on those types directly.
//
// ARM and RISC-V use different parser types and don't have this pattern
// (ARM has no numeric labels; RISC-V has its own pre-pass).

use crate::backend::x86::assembler::parser::{
    AsmItem, Instruction, Operand, MemoryOperand, Displacement, DataValue,
};

/// Check if a string is a numeric local label (just digits, e.g., "1", "42").
pub fn is_numeric_label(name: &str) -> bool {
    !name.is_empty() && name.bytes().all(|b| b.is_ascii_digit())
}

/// Check if a string is a numeric forward/backward reference like "1f" or "2b".
/// Returns Some((number_str, is_forward)) if it is, None otherwise.
pub fn parse_numeric_ref(name: &str) -> Option<(&str, bool)> {
    if name.len() < 2 {
        return None;
    }
    let last = name.as_bytes()[name.len() - 1];
    let num_part = &name[..name.len() - 1];
    if !num_part.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    match last {
        b'f' => Some((num_part, true)),
        b'b' => Some((num_part, false)),
        _ => None,
    }
}

/// Parse a string literal with escape sequences, returning raw bytes.
///
/// This is the **canonical** implementation shared by all assembler backends
/// (x86, i686, ARM, RISC-V). Having one implementation prevents bugs where
/// different backends handle escapes differently (e.g. returning `String`
/// instead of `Vec<u8>` causes multi-byte UTF-8 expansion of byte values > 127).
///
/// Supports the standard C/GNU assembler escape sequences:
///   `\n` `\t` `\r` `\\` `\"` `\a` `\b` `\f` `\v`
///   Octal: `\0` .. `\377` (1-3 digits)
///   Hex:   `\x00` .. `\xFF` (1-2 digits)
///
/// The input `s` should be a trimmed string starting with `"`. The parser scans
/// character-by-character until the closing `"` (rather than assuming it is the
/// last character), which correctly handles edge cases where extra content
/// follows the string literal.
pub fn parse_string_literal(s: &str) -> Result<Vec<u8>, String> {
    let s = s.trim();
    if !s.starts_with('"') {
        return Err(format!("expected string literal: {}", s));
    }

    let mut bytes = Vec::new();
    let mut chars = s[1..].chars();
    loop {
        match chars.next() {
            None => return Err("unterminated string".to_string()),
            Some('"') => break,
            Some('\\') => {
                match chars.next() {
                    None => return Err("unterminated escape".to_string()),
                    Some('n') => bytes.push(b'\n'),
                    Some('t') => bytes.push(b'\t'),
                    Some('r') => bytes.push(b'\r'),
                    Some('\\') => bytes.push(b'\\'),
                    Some('"') => bytes.push(b'"'),
                    Some('a') => bytes.push(7),  // bell
                    Some('b') => bytes.push(8),  // backspace
                    Some('f') => bytes.push(12), // form feed
                    Some('v') => bytes.push(11), // vertical tab
                    Some(c) if ('0'..='7').contains(&c) => {
                        // Octal escape: \N, \NN, or \NNN (up to 3 digits)
                        let mut val = c as u32 - '0' as u32;
                        for _ in 0..2 {
                            if let Some(&next) = chars.as_str().as_bytes().first() {
                                if (b'0'..=b'7').contains(&next) {
                                    val = val * 8 + (next - b'0') as u32;
                                    chars.next();
                                } else {
                                    break;
                                }
                            }
                        }
                        bytes.push(val as u8);
                    }
                    Some('x') => {
                        // Hex escape: \xNN (up to 2 digits)
                        let mut val = 0u32;
                        for _ in 0..2 {
                            if let Some(&next) = chars.as_str().as_bytes().first() {
                                if next.is_ascii_hexdigit() {
                                    val = val * 16 + match next {
                                        b'0'..=b'9' => (next - b'0') as u32,
                                        b'a'..=b'f' => (next - b'a' + 10) as u32,
                                        b'A'..=b'F' => (next - b'A' + 10) as u32,
                                        _ => unreachable!(),
                                    };
                                    chars.next();
                                } else {
                                    break;
                                }
                            }
                        }
                        bytes.push(val as u8);
                    }
                    Some(c) => {
                        // Unknown escape: emit the character as a raw byte.
                        // (GNU as treats unknown \X as literal X.)
                        bytes.push(c as u8);
                    }
                }
            }
            Some(c) => {
                // Regular character - encode as UTF-8
                let mut buf = [0u8; 4];
                let encoded = c.encode_utf8(&mut buf);
                bytes.extend_from_slice(encoded.as_bytes());
            }
        }
    }

    Ok(bytes)
}

/// Resolve numeric local labels (1:, 2:, etc.) and their references (1f, 1b)
/// into unique internal label names.
///
/// GNU assembler numeric labels can be defined multiple times. Each forward
/// reference `Nf` refers to the next definition of `N`, and each backward
/// reference `Nb` refers to the most recent definition of `N`.
///
/// This function renames each definition to a unique `.Lnum_N_K` name and
/// updates all references accordingly. Used by both x86 and i686 ELF writers.
pub fn resolve_numeric_labels(items: &[AsmItem]) -> Vec<AsmItem> {
    // First pass: find all numeric label definitions and assign unique names.
    let mut defs: HashMap<String, Vec<(usize, String)>> = HashMap::new();
    let mut unique_counter: HashMap<String, usize> = HashMap::new();

    for (i, item) in items.iter().enumerate() {
        if let AsmItem::Label(name) = item {
            if is_numeric_label(name) {
                let count = unique_counter.entry(name.clone()).or_insert(0);
                let unique_name = format!(".Lnum_{}_{}", name, *count);
                *count += 1;
                defs.entry(name.clone()).or_default().push((i, unique_name));
            }
        }
    }

    if defs.is_empty() {
        return items.to_vec();
    }

    let mut result = Vec::with_capacity(items.len());
    for (i, item) in items.iter().enumerate() {
        match item {
            AsmItem::Label(name) if is_numeric_label(name) => {
                if let Some(def_list) = defs.get(name) {
                    if let Some((_, unique_name)) = def_list.iter().find(|(idx, _)| *idx == i) {
                        result.push(AsmItem::Label(unique_name.clone()));
                        continue;
                    }
                }
                result.push(item.clone());
            }
            AsmItem::Instruction(instr) => {
                let new_ops: Vec<Operand> = instr.operands.iter().map(|op| {
                    resolve_numeric_operand(op, i, &defs)
                }).collect();
                result.push(AsmItem::Instruction(Instruction {
                    prefix: instr.prefix.clone(),
                    mnemonic: instr.mnemonic.clone(),
                    operands: new_ops,
                }));
            }
            AsmItem::Long(vals) => {
                result.push(AsmItem::Long(resolve_numeric_data_values(vals, i, &defs)));
            }
            AsmItem::Quad(vals) => {
                result.push(AsmItem::Quad(resolve_numeric_data_values(vals, i, &defs)));
            }
            AsmItem::Byte(vals) => {
                result.push(AsmItem::Byte(resolve_numeric_data_values(vals, i, &defs)));
            }
            AsmItem::SkipExpr(expr, fill) => {
                let new_expr = resolve_numeric_refs_in_expr(expr, i, &defs);
                result.push(AsmItem::SkipExpr(new_expr, *fill));
            }
            AsmItem::Org(sym, offset) => {
                if let Some(resolved) = resolve_numeric_name(sym, i, &defs) {
                    result.push(AsmItem::Org(resolved, *offset));
                } else {
                    result.push(item.clone());
                }
            }
            _ => result.push(item.clone()),
        }
    }

    result
}

/// Resolve numeric label references in a single operand.
fn resolve_numeric_operand(
    op: &Operand,
    current_idx: usize,
    defs: &HashMap<String, Vec<(usize, String)>>,
) -> Operand {
    match op {
        Operand::Label(name) => {
            if let Some(resolved) = resolve_numeric_name(name, current_idx, defs) {
                Operand::Label(resolved)
            } else {
                op.clone()
            }
        }
        Operand::Memory(mem) => {
            if let Some(new_disp) = resolve_numeric_displacement(&mem.displacement, current_idx, defs) {
                Operand::Memory(MemoryOperand {
                    segment: mem.segment.clone(),
                    displacement: new_disp,
                    base: mem.base.clone(),
                    index: mem.index.clone(),
                    scale: mem.scale,
                })
            } else {
                op.clone()
            }
        }
        _ => op.clone(),
    }
}

/// Resolve numeric label references in data values (.long, .quad, .byte directives).
fn resolve_numeric_data_values(
    vals: &[DataValue],
    current_idx: usize,
    defs: &HashMap<String, Vec<(usize, String)>>,
) -> Vec<DataValue> {
    vals.iter().map(|val| {
        match val {
            DataValue::Symbol(name) => {
                if let Some(resolved) = resolve_numeric_name(name, current_idx, defs) {
                    DataValue::Symbol(resolved)
                } else {
                    val.clone()
                }
            }
            DataValue::SymbolDiff(lhs, rhs) => {
                let new_lhs = resolve_numeric_name(lhs, current_idx, defs).unwrap_or_else(|| lhs.clone());
                let new_rhs = resolve_numeric_name(rhs, current_idx, defs).unwrap_or_else(|| rhs.clone());
                DataValue::SymbolDiff(new_lhs, new_rhs)
            }
            DataValue::SymbolOffset(name, offset) => {
                if let Some(resolved) = resolve_numeric_name(name, current_idx, defs) {
                    DataValue::SymbolOffset(resolved, *offset)
                } else {
                    val.clone()
                }
            }
            _ => val.clone(),
        }
    }).collect()
}

/// Resolve a numeric label reference name (e.g., "1f" -> ".Lnum_1_0").
pub fn resolve_numeric_name(
    name: &str,
    current_idx: usize,
    defs: &HashMap<String, Vec<(usize, String)>>,
) -> Option<String> {
    let (num, is_forward) = parse_numeric_ref(name)?;
    let def_list = defs.get(num)?;

    if is_forward {
        def_list.iter()
            .find(|(idx, _)| *idx > current_idx)
            .map(|(_, name)| name.clone())
    } else {
        def_list.iter()
            .rev()
            .find(|(idx, _)| *idx < current_idx)
            .map(|(_, name)| name.clone())
    }
}

/// Resolve numeric label references in a displacement.
fn resolve_numeric_displacement(
    disp: &Displacement,
    current_idx: usize,
    defs: &HashMap<String, Vec<(usize, String)>>,
) -> Option<Displacement> {
    match disp {
        Displacement::Symbol(name) => {
            resolve_numeric_name(name, current_idx, defs)
                .map(Displacement::Symbol)
        }
        Displacement::SymbolAddend(name, addend) => {
            resolve_numeric_name(name, current_idx, defs)
                .map(|n| Displacement::SymbolAddend(n, *addend))
        }
        Displacement::SymbolMod(name, modifier) => {
            resolve_numeric_name(name, current_idx, defs)
                .map(|n| Displacement::SymbolMod(n, modifier.clone()))
        }
        Displacement::SymbolPlusOffset(name, offset) => {
            resolve_numeric_name(name, current_idx, defs)
                .map(|n| Displacement::SymbolPlusOffset(n, *offset))
        }
        _ => None,
    }
}

/// Resolve numeric label references (e.g., "6651f", "661b") within an expression string.
/// Scans for patterns like digits followed by 'f' or 'b' and replaces them with unique names.
pub fn resolve_numeric_refs_in_expr(
    expr: &str,
    current_idx: usize,
    defs: &HashMap<String, Vec<(usize, String)>>,
) -> String {
    let mut result = String::with_capacity(expr.len());
    let bytes = expr.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i].is_ascii_digit() {
            let start = i;
            while i < bytes.len() && bytes[i].is_ascii_digit() {
                i += 1;
            }
            if i < bytes.len() && (bytes[i] == b'f' || bytes[i] == b'b') {
                let next = i + 1;
                if next >= bytes.len() || !bytes[next].is_ascii_alphanumeric() {
                    let ref_name = &expr[start..=i];
                    if let Some(resolved) = resolve_numeric_name(ref_name, current_idx, defs) {
                        result.push_str(&resolved);
                        i += 1;
                        continue;
                    }
                }
            }
            result.push_str(&expr[start..i]);
        } else {
            result.push(bytes[i] as char);
            i += 1;
        }
    }
    result
}

// ── Shared section flags parsing ──────────────────────────────────────────
//
// Both x86 and i686 ELF writers need to parse section directives into
// (sh_type, sh_flags) tuples. The logic is identical except for the flags
// type (u64 vs u32). This function works with u64 flags; i686 can cast.

/// Parse section name, flags string, and type into ELF section type and flags.
///
/// Returns `(sh_type, sh_flags)` based on well-known section names (`.text`,
/// `.data`, `.bss`, etc.) and optional explicit flags/type strings from the
/// `.section` directive.
pub fn parse_section_flags(name: &str, flags_str: Option<&str>, type_str: Option<&str>) -> (u32, u64) {
    let (default_type, default_flags) = match name {
        ".text" => (SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR),
        ".data" => (SHT_PROGBITS, SHF_ALLOC | SHF_WRITE),
        ".bss" => (SHT_NOBITS, SHF_ALLOC | SHF_WRITE),
        ".rodata" => (SHT_PROGBITS, SHF_ALLOC),
        ".tdata" => (SHT_PROGBITS, SHF_ALLOC | SHF_WRITE | SHF_TLS),
        ".tbss" => (SHT_NOBITS, SHF_ALLOC | SHF_WRITE | SHF_TLS),
        ".init" => (SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR),
        ".fini" => (SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR),
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

// ── Shared symbol table builder for all backend ELF writers ─────────────
//
// All four backend assemblers (x86-64, i686, ARM, RISC-V) use this shared
// `build_elf_symbol_table` function to construct their symbol tables from
// labels, aliases, and relocation references. This eliminates duplicated
// symbol table construction logic across the backends.
//
// The only architecture-specific difference is that RISC-V needs to include
// referenced local labels (for pcrel_hi synthetic labels) in the symbol table.

/// Parameters for the shared `build_elf_symbol_table` function.
/// Collects the state needed to build the symbol table without requiring
/// a specific ElfWriter struct type.
pub struct SymbolTableInput<'a> {
    pub labels: &'a HashMap<String, (String, u64)>,
    pub global_symbols: &'a HashMap<String, bool>,
    pub weak_symbols: &'a HashMap<String, bool>,
    pub symbol_types: &'a HashMap<String, u8>,
    pub symbol_sizes: &'a HashMap<String, u64>,
    pub symbol_visibility: &'a HashMap<String, u8>,
    pub aliases: &'a HashMap<String, String>,
    pub sections: &'a HashMap<String, ObjSection>,
    /// If true, include .L* local labels that are referenced by relocations
    /// in the symbol table (needed by RISC-V for pcrel_hi/pcrel_lo pairs).
    pub include_referenced_locals: bool,
}

/// Build a symbol table from labels, aliases, and relocation references.
///
/// Returns a list of `ObjSymbol` entries ready for `write_relocatable_object`.
/// Handles:
/// - Defined labels (global, weak, local)
/// - .set/.equ aliases with chain resolution
/// - Undefined symbols (referenced in relocations but not defined)
/// - Optionally, referenced local labels (.L*) for RISC-V pcrel support
pub fn build_elf_symbol_table(input: &SymbolTableInput) -> Vec<ObjSymbol> {
    let mut symbols: Vec<ObjSymbol> = Vec::new();

    // Collect referenced local labels if needed (RISC-V pcrel_hi)
    let mut referenced_local_labels: HashSet<String> = HashSet::new();
    if input.include_referenced_locals {
        for sec in input.sections.values() {
            for reloc in &sec.relocs {
                if reloc.symbol_name.starts_with(".L") || reloc.symbol_name.starts_with(".l") {
                    referenced_local_labels.insert(reloc.symbol_name.clone());
                }
            }
        }
    }

    // Add defined labels as symbols
    for (name, (section, offset)) in input.labels {
        let is_local_label = name.starts_with(".L") || name.starts_with(".l");
        if is_local_label && !referenced_local_labels.contains(name) {
            continue;
        }

        let binding = if input.weak_symbols.contains_key(name) {
            STB_WEAK
        } else if input.global_symbols.contains_key(name) {
            STB_GLOBAL
        } else {
            STB_LOCAL
        };

        symbols.push(ObjSymbol {
            name: name.clone(),
            value: *offset,
            size: input.symbol_sizes.get(name).copied().unwrap_or(0),
            binding,
            sym_type: input.symbol_types.get(name).copied().unwrap_or(STT_NOTYPE),
            visibility: input.symbol_visibility.get(name).copied().unwrap_or(STV_DEFAULT),
            section_name: section.clone(),
        });
    }

    // Add alias symbols from .set/.equ directives
    let defined_names: HashMap<String, usize> = symbols.iter()
        .enumerate()
        .map(|(i, s)| (s.name.clone(), i))
        .collect();

    for (alias, target) in input.aliases {
        // Resolve through alias chains
        let mut resolved = target.as_str();
        let mut seen = HashSet::new();
        seen.insert(target.as_str());
        while let Some(next) = input.aliases.get(resolved) {
            if !seen.insert(next.as_str()) {
                break;
            }
            resolved = next.as_str();
        }

        let alias_binding = if input.weak_symbols.contains_key(alias) {
            Some(STB_WEAK)
        } else if input.global_symbols.contains_key(alias) {
            Some(STB_GLOBAL)
        } else {
            None
        };
        let alias_type = input.symbol_types.get(alias).copied();
        let alias_vis = input.symbol_visibility.get(alias).copied();

        if let Some(&idx) = defined_names.get(resolved) {
            let target_sym = &symbols[idx];
            symbols.push(ObjSymbol {
                name: alias.clone(),
                value: target_sym.value,
                size: target_sym.size,
                binding: alias_binding.unwrap_or(target_sym.binding),
                sym_type: alias_type.unwrap_or(target_sym.sym_type),
                visibility: alias_vis.unwrap_or(target_sym.visibility),
                section_name: target_sym.section_name.clone(),
            });
        } else if let Some((section, offset)) = input.labels.get(resolved) {
            symbols.push(ObjSymbol {
                name: alias.clone(),
                value: *offset,
                size: 0,
                binding: alias_binding.unwrap_or(STB_LOCAL),
                sym_type: alias_type.unwrap_or(STT_NOTYPE),
                visibility: alias_vis.unwrap_or(STV_DEFAULT),
                section_name: section.clone(),
            });
        }
    }

    // Add undefined symbols (referenced in relocations but not defined)
    let mut referenced: HashSet<String> = HashSet::new();
    for sec in input.sections.values() {
        for reloc in &sec.relocs {
            if reloc.symbol_name.is_empty() {
                continue;
            }
            if !reloc.symbol_name.starts_with(".L") && !reloc.symbol_name.starts_with(".l") {
                referenced.insert(reloc.symbol_name.clone());
            }
        }
    }

    let defined: HashSet<String> = symbols.iter().map(|s| s.name.clone()).collect();

    for name in &referenced {
        if input.sections.contains_key(name) {
            continue; // Skip section names
        }
        if !defined.contains(name) {
            let binding = if input.weak_symbols.contains_key(name) {
                STB_WEAK
            } else {
                STB_GLOBAL
            };
            symbols.push(ObjSymbol {
                name: name.clone(),
                value: 0,
                size: 0,
                binding,
                sym_type: input.symbol_types.get(name).copied().unwrap_or(STT_NOTYPE),
                visibility: input.symbol_visibility.get(name).copied().unwrap_or(STV_DEFAULT),
                section_name: "*UND*".to_string(),
            });
        }
    }

    symbols
}


// ── Shared ELF writer base for ARM/RISC-V assemblers ────────────────────
//
// ARM and RISC-V ELF writers share ~400 lines of identical code:
// - Internal types (Section, ElfReloc, ElfSymbol) → replaced by ObjSection/ObjReloc/ObjSymbol
// - Section management (ensure_section, current_offset, emit_bytes, emit_u32_le)
// - Relocation recording (add_reloc)
// - Alignment (align_to with arch-specific NOP fill)
// - Directive processing (.global, .weak, .hidden, .type, .size, .align, etc.)
// - Data emission (.byte, .short, .long, .quad with symbol relocations)
// - ELF serialization (write_elf using build_elf_symbol_table + write_relocatable_object)
// - Local data relocation resolution (resolve_local_data_relocs)
//
// This `ElfWriterBase` struct captures all of that shared state and logic.
// Each arch-specific ElfWriter composes with this base and adds its own
// instruction encoding, branch resolution, and other arch-specific features.

/// Shared ELF writer state used by both ARM and RISC-V assembler backends.
///
/// This struct manages sections, symbols, labels, and relocations using the
/// shared `ObjSection`/`ObjReloc`/`ObjSymbol` types directly, eliminating the
/// per-arch conversion step in `write_elf()`.
///
/// Architecture-specific ElfWriters compose with this base:
/// - ARM adds `pending_sym_diffs` and AArch64-specific branch resolution
/// - RISC-V adds `pcrel_hi_counter`, `numeric_labels`, and RV64C compression
pub struct ElfWriterBase {
    /// Current section we're emitting into
    pub current_section: String,
    /// All sections being built (using shared ObjSection directly)
    pub sections: HashMap<String, ObjSection>,
    /// Section order (for deterministic output)
    pub section_order: Vec<String>,
    /// Extra symbols (e.g., COMMON symbols from .comm directives)
    pub extra_symbols: Vec<ObjSymbol>,
    /// Local labels -> (section, offset) for branch resolution
    pub labels: HashMap<String, (String, u64)>,
    /// Symbols that have been declared .globl
    pub global_symbols: HashMap<String, bool>,
    /// Symbols declared .weak
    pub weak_symbols: HashMap<String, bool>,
    /// Symbol types from .type directives
    pub symbol_types: HashMap<String, u8>,
    /// Symbol sizes from .size directives
    pub symbol_sizes: HashMap<String, u64>,
    /// Symbol visibility from .hidden/.protected/.internal
    pub symbol_visibility: HashMap<String, u8>,
    /// Symbol aliases from .set/.equ directives
    pub aliases: HashMap<String, String>,
    /// Section stack for .pushsection/.popsection
    section_stack: Vec<String>,
    /// NOP instruction bytes for code section alignment padding.
    /// ARM: `[0x1f, 0x20, 0x03, 0xd5]` (d503201f), RISC-V: `[0x13, 0x00, 0x00, 0x00]` (00000013)
    nop_bytes: [u8; 4],
    /// Default text section alignment (4 for ARM, 2 for RISC-V with compressed instructions)
    text_align: u64,
}

impl ElfWriterBase {
    pub fn new(nop_bytes: [u8; 4], text_align: u64) -> Self {
        Self {
            current_section: String::new(),
            sections: HashMap::new(),
            section_order: Vec::new(),
            extra_symbols: Vec::new(),
            labels: HashMap::new(),
            global_symbols: HashMap::new(),
            weak_symbols: HashMap::new(),
            symbol_types: HashMap::new(),
            symbol_sizes: HashMap::new(),
            symbol_visibility: HashMap::new(),
            aliases: HashMap::new(),
            section_stack: Vec::new(),
            nop_bytes,
            text_align,
        }
    }

    /// Ensure a section exists. If it doesn't, create it with the given properties.
    pub fn ensure_section(&mut self, name: &str, sh_type: u32, sh_flags: u64, align: u64) {
        if !self.sections.contains_key(name) {
            self.sections.insert(name.to_string(), ObjSection {
                name: name.to_string(),
                sh_type,
                sh_flags,
                data: Vec::new(),
                sh_addralign: align,
                relocs: Vec::new(),
                comdat_group: None,
            });
            self.section_order.push(name.to_string());
        }
    }

    /// Get the current write offset within the current section.
    pub fn current_offset(&self) -> u64 {
        self.sections.get(&self.current_section)
            .map(|s| s.data.len() as u64)
            .unwrap_or(0)
    }

    /// Append raw bytes to the current section.
    pub fn emit_bytes(&mut self, bytes: &[u8]) {
        if let Some(section) = self.sections.get_mut(&self.current_section) {
            section.data.extend_from_slice(bytes);
        }
    }

    /// Append a 16-bit little-endian value to the current section.
    pub fn emit_u16_le(&mut self, val: u16) {
        self.emit_bytes(&val.to_le_bytes());
    }

    /// Append a 32-bit little-endian value to the current section.
    pub fn emit_u32_le(&mut self, val: u32) {
        self.emit_bytes(&val.to_le_bytes());
    }

    /// Record a relocation at the current offset in the current section.
    pub fn add_reloc(&mut self, reloc_type: u32, symbol: String, addend: i64) {
        let offset = self.current_offset();
        let section = self.current_section.clone();
        if let Some(s) = self.sections.get_mut(&section) {
            s.relocs.push(ObjReloc {
                offset,
                reloc_type,
                symbol_name: symbol,
                addend,
            });
        }
    }

    /// Align the current section's data to the specified byte boundary.
    ///
    /// Code sections are NOP-padded using the architecture's NOP instruction;
    /// data sections are zero-padded.
    pub fn align_to(&mut self, align: u64) {
        if align <= 1 {
            return;
        }
        if let Some(section) = self.sections.get_mut(&self.current_section) {
            let current = section.data.len() as u64;
            let aligned = (current + align - 1) & !(align - 1);
            let padding = (aligned - current) as usize;
            if section.sh_flags & SHF_EXECINSTR != 0 && align >= 4 {
                let full_nops = padding / 4;
                let remainder = padding % 4;
                for _ in 0..full_nops {
                    section.data.extend_from_slice(&self.nop_bytes);
                }
                section.data.extend(std::iter::repeat_n(0u8, remainder));
            } else {
                section.data.extend(std::iter::repeat_n(0u8, padding));
            }
            if align > section.sh_addralign {
                section.sh_addralign = align;
            }
        }
    }

    /// Ensure we're in a text section, creating one if needed.
    pub fn ensure_text_section(&mut self) {
        if self.current_section.is_empty() {
            self.ensure_section(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, self.text_align);
            self.current_section = ".text".to_string();
        }
    }

    /// Process a .section directive with parsed fields.
    ///
    /// `sec_name`: section name, `flags_str`: flag characters ("awx" etc.),
    /// `flags_explicit`: whether flags were explicitly provided (vs default),
    /// `sec_type_str`: optional type string ("@nobits", "@note", etc.)
    pub fn process_section_directive(
        &mut self,
        sec_name: &str,
        flags_str: &str,
        flags_explicit: bool,
        sec_type_str: Option<&str>,
    ) {
        let sh_type = match sec_type_str {
            Some("@nobits") => SHT_NOBITS,
            Some("@note") => SHT_NOTE,
            _ => {
                if sec_name == ".bss" || sec_name.starts_with(".bss.") || sec_name.starts_with(".tbss") {
                    SHT_NOBITS
                } else {
                    SHT_PROGBITS
                }
            }
        };

        let mut sh_flags = 0u64;
        if flags_str.contains('a') { sh_flags |= SHF_ALLOC; }
        if flags_str.contains('w') { sh_flags |= SHF_WRITE; }
        if flags_str.contains('x') { sh_flags |= SHF_EXECINSTR; }
        if flags_str.contains('M') { sh_flags |= SHF_MERGE; }
        if flags_str.contains('S') { sh_flags |= SHF_STRINGS; }
        if flags_str.contains('T') { sh_flags |= SHF_TLS; }
        if flags_str.contains('G') { sh_flags |= SHF_GROUP; }

        if sh_flags == 0 && !flags_explicit {
            sh_flags = default_section_flags(sec_name);
        }

        let align = if sh_flags & SHF_EXECINSTR != 0 { self.text_align } else { 1 };
        self.ensure_section(sec_name, sh_type, sh_flags, align);
        self.current_section = sec_name.to_string();
    }

    /// Switch to a named standard section (.text, .data, .bss, .rodata).
    pub fn switch_to_standard_section(&mut self, name: &str, sh_type: u32, sh_flags: u64) {
        let align = if sh_flags & SHF_EXECINSTR != 0 { self.text_align } else { 1 };
        self.ensure_section(name, sh_type, sh_flags, align);
        self.current_section = name.to_string();
    }

    /// Push current section onto the stack and switch to a new section.
    pub fn push_section(&mut self, name: &str, flags_str: &str, flags_explicit: bool, sec_type: Option<&str>) {
        self.section_stack.push(self.current_section.clone());
        self.process_section_directive(name, flags_str, flags_explicit, sec_type);
    }

    /// Pop the section stack and restore the previous section.
    pub fn pop_section(&mut self) {
        if let Some(prev) = self.section_stack.pop() {
            self.current_section = prev;
        }
    }

    /// Record .globl for a symbol.
    pub fn set_global(&mut self, sym: &str) {
        self.global_symbols.insert(sym.to_string(), true);
    }

    /// Record .weak for a symbol.
    pub fn set_weak(&mut self, sym: &str) {
        self.weak_symbols.insert(sym.to_string(), true);
    }

    /// Record symbol visibility (.hidden, .protected, .internal).
    pub fn set_visibility(&mut self, sym: &str, vis: u8) {
        self.symbol_visibility.insert(sym.to_string(), vis);
    }

    /// Record .type for a symbol (STT_FUNC, STT_OBJECT, etc.).
    pub fn set_symbol_type(&mut self, sym: &str, st: u8) {
        self.symbol_types.insert(sym.to_string(), st);
    }

    /// Record .size for a symbol. If `current_minus_label` is Some, computes
    /// `current_offset - label_offset` in the same section. Otherwise uses the absolute value.
    pub fn set_symbol_size(&mut self, sym: &str, current_minus_label: Option<&str>, absolute: Option<u64>) {
        if let Some(label) = current_minus_label {
            if let Some((section, label_offset)) = self.labels.get(label) {
                if *section == self.current_section {
                    let current = self.current_offset();
                    let size = current - label_offset;
                    self.symbol_sizes.insert(sym.to_string(), size);
                }
            }
        } else if let Some(size) = absolute {
            self.symbol_sizes.insert(sym.to_string(), size);
        }
    }

    /// Emit a .comm symbol (COMMON block).
    pub fn emit_comm(&mut self, sym: &str, size: u64, align: u64) {
        self.extra_symbols.push(ObjSymbol {
            name: sym.to_string(),
            value: align,
            size,
            binding: STB_GLOBAL,
            sym_type: STT_OBJECT,
            visibility: STV_DEFAULT,
            section_name: "*COM*".to_string(),
        });
    }

    /// Record a .set/.equ alias.
    pub fn set_alias(&mut self, alias: &str, target: &str) {
        self.aliases.insert(alias.to_string(), target.to_string());
    }

    /// Resolve .set/.equ aliases in an expression string.
    /// Replaces symbol names (like `.L__gpr_num_t0`) with their numeric values.
    pub fn resolve_expr_aliases(&self, expr: &str) -> String {
        let mut result = String::with_capacity(expr.len());
        let bytes = expr.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            let c = bytes[i];
            // Symbol names start with a letter, underscore, or dot
            if c == b'.' || c == b'_' || c.is_ascii_alphabetic() {
                let start = i;
                i += 1;
                while i < bytes.len() && (bytes[i] == b'.' || bytes[i] == b'_' || bytes[i].is_ascii_alphanumeric()) {
                    i += 1;
                }
                let sym = &expr[start..i];
                // Chase alias chain
                let mut resolved = sym;
                let mut seen = 0;
                while let Some(target) = self.aliases.get(resolved) {
                    resolved = target.as_str();
                    seen += 1;
                    if seen > 20 { break; }
                }
                result.push_str(resolved);
            } else {
                result.push(c as char);
                i += 1;
            }
        }
        result
    }

    /// Resolve label names in an expression to their numeric offsets.
    /// This handles `.Ldot_N` synthetic labels (current position) and any
    /// section-local labels that can be resolved to constant offsets.
    pub fn resolve_expr_labels(&self, expr: &str) -> String {
        let cur_section = &self.current_section;
        let cur_offset = self.current_offset();
        let mut result = String::with_capacity(expr.len());
        let bytes = expr.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            let c = bytes[i];
            if c == b'.' || c == b'_' || c.is_ascii_alphabetic() {
                let start = i;
                i += 1;
                while i < bytes.len() && (bytes[i] == b'.' || bytes[i] == b'_' || bytes[i].is_ascii_alphanumeric()) {
                    i += 1;
                }
                let sym = &expr[start..i];
                // Check if this is a .Ldot_N label (current position)
                if sym.starts_with(".Ldot_") {
                    result.push_str(&cur_offset.to_string());
                } else if let Some((sec, off)) = self.labels.get(sym) {
                    if sec == cur_section {
                        result.push_str(&off.to_string());
                    } else {
                        result.push_str(sym);
                    }
                } else {
                    result.push_str(sym);
                }
            } else {
                result.push(c as char);
                i += 1;
            }
        }
        result
    }

    /// Emit a plain integer value for .byte (size=1), .short (size=2), .long (size=4) or .quad (size=8).
    pub fn emit_data_integer(&mut self, val: i64, size: usize) {
        match size {
            1 => self.emit_bytes(&[val as u8]),
            2 => self.emit_bytes(&(val as u16).to_le_bytes()),
            4 => self.emit_bytes(&(val as u32).to_le_bytes()),
            _ => self.emit_bytes(&(val as u64).to_le_bytes()),
        }
    }

    /// Emit a symbol reference with a relocation.
    pub fn emit_data_symbol_ref(&mut self, sym: &str, addend: i64, size: usize, reloc_type: u32) {
        self.add_reloc(reloc_type, sym.to_string(), addend);
        match size {
            1 => self.emit_bytes(&[0u8]),
            2 => self.emit_bytes(&0u16.to_le_bytes()),
            4 => self.emit_bytes(&0u32.to_le_bytes()),
            _ => self.emit_bytes(&0u64.to_le_bytes()),
        }
    }

    /// Emit placeholder bytes for a deferred value (symbol diff, etc.).
    pub fn emit_placeholder(&mut self, size: usize) {
        match size {
            1 => self.emit_bytes(&[0u8]),
            2 => self.emit_bytes(&0u16.to_le_bytes()),
            4 => self.emit_bytes(&0u32.to_le_bytes()),
            _ => self.emit_bytes(&0u64.to_le_bytes()),
        }
    }

    /// Resolve local label references in data relocations.
    ///
    /// When a data directive like `.xword .Lstr0` references a local label
    /// in a different section, the local label won't be in the symbol table.
    /// Convert these to section_symbol + offset_of_label_in_section.
    pub fn resolve_local_data_relocs(&mut self) {
        let labels = self.labels.clone();
        for sec_name in &self.section_order.clone() {
            if let Some(section) = self.sections.get_mut(sec_name) {
                for reloc in &mut section.relocs {
                    // Skip pcrel_lo12 relocations — they must keep their
                    // .Lpcrel_hi label reference (not section+offset)
                    let is_pcrel_lo = reloc.reloc_type == 24 || reloc.reloc_type == 25;
                    if is_pcrel_lo {
                        continue;
                    }
                    if (reloc.symbol_name.starts_with(".L") || reloc.symbol_name.starts_with(".l"))
                        && !reloc.symbol_name.is_empty()
                    {
                        if let Some((label_section, label_offset)) = labels.get(&reloc.symbol_name) {
                            reloc.addend += *label_offset as i64;
                            reloc.symbol_name = label_section.clone();
                        }
                    }
                }
            }
        }
    }

    /// Build the symbol table and serialize the ELF object file.
    ///
    /// `config`: ELF configuration (machine type, flags, class)
    /// `include_referenced_locals`: whether to include .L* labels referenced
    /// by relocations (needed by RISC-V for pcrel_hi/pcrel_lo pairs)
    pub fn write_elf(&mut self, output_path: &str, config: &ElfConfig, include_referenced_locals: bool) -> Result<(), String> {
        self.resolve_local_data_relocs();

        let symtab_input = SymbolTableInput {
            labels: &self.labels,
            global_symbols: &self.global_symbols,
            weak_symbols: &self.weak_symbols,
            symbol_types: &self.symbol_types,
            symbol_sizes: &self.symbol_sizes,
            symbol_visibility: &self.symbol_visibility,
            aliases: &self.aliases,
            sections: &self.sections,
            include_referenced_locals,
        };

        let mut symbols = build_elf_symbol_table(&symtab_input);
        // Remove UND entries for any symbols that are also in extra_symbols (e.g. COMMON).
        // A symbol that is both referenced in relocations and declared as COMMON should
        // only appear once (as COMMON), not as both UND and COMMON.
        for extra in &self.extra_symbols {
            if extra.section_name == "*COM*" {
                symbols.retain(|s| !(s.name == extra.name && s.section_name == "*UND*"));
            }
        }
        symbols.append(&mut self.extra_symbols);

        let elf_bytes = write_relocatable_object(
            config,
            &self.section_order,
            &self.sections,
            &symbols,
        )?;

        std::fs::write(output_path, &elf_bytes)
            .map_err(|e| format!("failed to write ELF file: {}", e))?;

        Ok(())
    }
}

