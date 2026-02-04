//! Native AArch64 ELF64 linker.
//!
//! Links ELF relocatable object files (.o) and static archives (.a) into
//! a statically-linked ELF64 executable for AArch64 (ARM 64-bit).
//!
//! This is the default linker (used when the `gcc_linker` feature is disabled).
//! CRT object discovery and library path resolution are handled by
//! common.rs's `resolve_builtin_link_setup`.
//!
//! Supported AArch64 relocations:
//! - R_AARCH64_ABS64, ABS32, ABS16 (absolute)
//! - R_AARCH64_PREL64, PREL32, PREL16 (PC-relative)
//! - R_AARCH64_ADR_PREL_PG_HI21 (page-relative ADRP)
//! - R_AARCH64_ADR_PREL_LO21 (ADR)
//! - R_AARCH64_ADD_ABS_LO12_NC (ADD low 12)
//! - R_AARCH64_LDST{8,16,32,64,128}_ABS_LO12_NC (load/store)
//! - R_AARCH64_CALL26, JUMP26 (branch)
//! - R_AARCH64_CONDBR19, TSTBR14 (conditional branch)
//! - R_AARCH64_MOVW_UABS_G{0,1,2,3}{,_NC} (MOVZ/MOVK)

#[allow(dead_code)]
pub mod elf;
pub mod reloc;

use std::collections::HashMap;
use std::path::Path;

use elf::*;

/// Base virtual address for the executable
const BASE_ADDR: u64 = 0x400000;
/// Page size for alignment
const PAGE_SIZE: u64 = 0x10000; // AArch64 uses 64KB pages for linker alignment


/// An output section in the final executable
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

/// Represents a merged input section
pub struct InputSection {
    pub object_idx: usize,
    pub section_idx: usize,
    pub output_offset: u64,
    pub size: u64,
}

/// A resolved global symbol
#[derive(Clone)]
pub struct GlobalSymbol {
    pub value: u64,
    pub size: u64,
    pub info: u8,
    pub defined_in: Option<usize>,
    pub section_idx: u16,
}

// ── Public entry point ─────────────────────────────────────────────────

/// Link AArch64 object files into a static ELF executable (pre-resolved CRT/library variant).
///
/// This is the primary entry point, matching the pattern used by x86-64 and i686 linkers.
/// CRT objects and library paths are resolved by common.rs before being passed in.
///
/// `object_files`: paths to user .o files and .a archives
/// `output_path`: path for the output executable
/// `user_args`: additional linker flags from the user
/// `lib_paths`: pre-resolved library search paths (user -L first, then system)
/// `needed_libs`: pre-resolved default libraries (e.g., ["gcc", "gcc_eh", "c"])
/// `crt_objects_before`: CRT objects to link before user objects (e.g., crt1.o, crti.o, crtbeginT.o)
/// `crt_objects_after`: CRT objects to link after user objects (e.g., crtend.o, crtn.o)
#[allow(clippy::too_many_arguments)]
pub fn link_builtin(
    object_files: &[&str],
    output_path: &str,
    user_args: &[String],
    lib_paths: &[&str],
    needed_libs: &[&str],
    crt_objects_before: &[&str],
    crt_objects_after: &[&str],
) -> Result<(), String> {
    if std::env::var("LINKER_DEBUG").is_ok() {
        eprintln!("arm linker: object_files={:?} output={} user_args={:?}", object_files, output_path, user_args);
    }
    let mut objects: Vec<ElfObject> = Vec::new();
    let mut globals: HashMap<String, GlobalSymbol> = HashMap::new();

    // Use pre-resolved library search paths
    let all_lib_paths: Vec<String> = lib_paths.iter().map(|s| s.to_string()).collect();

    // Load CRT objects before user objects
    for path in crt_objects_before {
        if Path::new(path).exists() {
            load_file(path, &mut objects, &mut globals, &all_lib_paths)?;
        }
    }

    // Load user object files from the object_files slice first
    for path in object_files {
        load_file(path, &mut objects, &mut globals, &all_lib_paths)?;
    }

    // Then load objects, archives and libraries from user_args in order.
    // user_args may contain .o files, .a files, -l flags, and -Wl, flags.
    let args: Vec<&str> = user_args.iter().map(|s| s.as_str()).collect();
    let mut arg_i = 0;
    while arg_i < args.len() {
        let arg = args[arg_i];
        if let Some(lib) = arg.strip_prefix("-l") {
            let l = if lib.is_empty() && arg_i + 1 < args.len() { arg_i += 1; args[arg_i] } else { lib };
            if let Some(lib_path) = resolve_lib(l, &all_lib_paths) {
                load_file(&lib_path, &mut objects, &mut globals, &all_lib_paths)?;
            }
        } else if let Some(wl_arg) = arg.strip_prefix("-Wl,") {
            for part in wl_arg.split(',') {
                if let Some(lib) = part.strip_prefix("-l") {
                    if let Some(lib_path) = resolve_lib(lib, &all_lib_paths) {
                        load_file(&lib_path, &mut objects, &mut globals, &all_lib_paths)?;
                    }
                }
            }
        } else if !arg.starts_with('-') && Path::new(arg).exists() {
            // Bare file path (e.g. .o or .a file)
            load_file(arg, &mut objects, &mut globals, &all_lib_paths)?;
        }
        arg_i += 1;
    }

    // Load CRT objects after user objects
    for path in crt_objects_after {
        if Path::new(path).exists() {
            load_file(path, &mut objects, &mut globals, &all_lib_paths)?;
        }
    }

    // Load default libraries in a group (like ld's --start-group).
    // This iterates all archives until no new symbols are resolved, handling
    // circular dependencies between libgcc, libgcc_eh, and libc.
    // Note: gcc_eh is added by the common.rs wrapper for static linking.
    if !needed_libs.is_empty() {
        let mut lib_paths_resolved: Vec<String> = Vec::new();
        for lib_name in needed_libs {
            if let Some(lib_path) = resolve_lib(lib_name, &all_lib_paths) {
                lib_paths_resolved.push(lib_path);
            }
        }
        // Group loading: iterate all archives until stable
        let mut changed = true;
        while changed {
            changed = false;
            let prev_count = objects.len();
            for lib_path in &lib_paths_resolved {
                load_file(lib_path, &mut objects, &mut globals, &all_lib_paths)?;
            }
            if objects.len() != prev_count {
                changed = true;
            }
        }
    }

    // Debug: show loaded objects
    if std::env::var("LINKER_DEBUG").is_ok() {
        for (i, obj) in objects.iter().enumerate() {
            eprintln!("obj[{}]: {} sections={} syms={}", i, obj.source_name, obj.sections.len(), obj.symbols.len());
            for (si, sec) in obj.sections.iter().enumerate() {
                if sec.flags & SHF_ALLOC != 0 {
                    eprintln!("  sec[{}]: {} type={} flags=0x{:x} size={}", si, sec.name, sec.sh_type, sec.flags, sec.size);
                }
            }
        }
        for (name, gsym) in &globals {
            if gsym.defined_in.is_some() {
                eprintln!("  global: {} val=0x{:x} sec={} obj={:?}", name, gsym.value, gsym.section_idx, gsym.defined_in);
            }
        }
    }

    // Reject truly undefined symbols (weak undefined are allowed)
    let mut unresolved = Vec::new();
    for (name, sym) in &globals {
        if sym.defined_in.is_none() && sym.section_idx == SHN_UNDEF {
            let binding = sym.info >> 4;
            if binding != STB_WEAK {
                // Skip well-known linker-defined symbols
                if !matches!(name.as_str(), "__bss_start" | "_edata" | "_end" | "__end"
                    | "_GLOBAL_OFFSET_TABLE_" | "__dso_handle" | "_DYNAMIC"
                    | "__GNU_EH_FRAME_HDR" | "__ehdr_start" | "__executable_start"
                    | "_init" | "_fini"
                    | "__init_array_start" | "__init_array_end"
                    | "__fini_array_start" | "__fini_array_end"
                    | "__preinit_array_start" | "__preinit_array_end"
                    | "__rela_iplt_start" | "__rela_iplt_end"
                    | "_etext" | "etext" | "__data_start" | "data_start"
                    // TODO: __getauxval is weakly referenced by glibc init; resolves to 0 in static binaries
                    | "__getauxval") {
                    unresolved.push(name.clone());
                }
            }
        }
    }
    if !unresolved.is_empty() {
        unresolved.sort();
        unresolved.truncate(20);
        return Err(format!("undefined symbols: {}",
            unresolved.iter().map(|s| s.as_str()).collect::<Vec<_>>().join(", ")));
    }

    // Merge sections
    let mut output_sections: Vec<OutputSection> = Vec::new();
    let mut section_map: HashMap<(usize, usize), (usize, u64)> = HashMap::new();
    merge_sections(&objects, &mut output_sections, &mut section_map);

    // Allocate COMMON symbols
    allocate_common_symbols(&mut globals, &mut output_sections);

    // Layout and emit
    emit_executable(&objects, &mut globals, &mut output_sections, &section_map, output_path)
}

// ── File loading ───────────────────────────────────────────────────────

fn load_file(
    path: &str,
    objects: &mut Vec<ElfObject>,
    globals: &mut HashMap<String, GlobalSymbol>,
    lib_paths: &[String],
) -> Result<(), String> {
    if std::env::var("LINKER_DEBUG").is_ok() {
        eprintln!("load_file: {}", path);
    }
    let data = std::fs::read(path).map_err(|e| format!("failed to read '{}': {}", path, e))?;

    // Archive?
    if data.len() >= 8 && &data[0..8] == b"!<arch>\n" {
        return load_archive(&data, path, objects, globals, lib_paths);
    }

    // Thin archive?
    if is_thin_archive(&data) {
        return load_thin_archive(&data, path, objects, globals, lib_paths);
    }

    // Not ELF? Try linker script
    if data.len() >= 4 && data[0..4] != ELF_MAGIC {
        if let Ok(text) = std::str::from_utf8(&data) {
            if let Some(paths) = parse_linker_script(text) {
                for lib_path in &paths {
                    if Path::new(lib_path).exists() {
                        load_file(lib_path, objects, globals, lib_paths)?;
                    }
                }
                return Ok(());
            }
        }
        return Err(format!("{}: not a valid ELF object or archive", path));
    }

    // Shared library? Skip for static linking
    if data.len() >= 18 {
        let e_type = read_u16(&data, 16);
        if e_type == ET_DYN {
            return Ok(()); // Skip .so in static linking
        }
    }

    let obj = parse_object(&data, path)?;
    let obj_idx = objects.len();
    register_symbols(obj_idx, &obj, globals);
    objects.push(obj);
    Ok(())
}

fn load_archive(
    data: &[u8],
    archive_path: &str,
    objects: &mut Vec<ElfObject>,
    globals: &mut HashMap<String, GlobalSymbol>,
    _lib_paths: &[String],
) -> Result<(), String> {
    let members = parse_archive_members(data)?;
    let mut member_objects: Vec<ElfObject> = Vec::new();
    for (name, offset, size) in &members {
        let member_data = &data[*offset..*offset + *size];
        if member_data.len() < 4 || member_data[0..4] != ELF_MAGIC { continue; }
        // Check it's AArch64
        if member_data.len() >= 20 {
            let e_machine = read_u16(member_data, 18);
            if e_machine != EM_AARCH64 { continue; }
        }
        let full_name = format!("{}({})", archive_path, name);
        if let Ok(obj) = parse_object(member_data, &full_name) {
            member_objects.push(obj);
        }
    }

    // Pull in members that resolve undefined symbols, iterating until stable
    let mut changed = true;
    while changed {
        changed = false;
        let mut i = 0;
        while i < member_objects.len() {
            if member_resolves_undefined(&member_objects[i], globals) {
                let obj = member_objects.remove(i);
                let obj_idx = objects.len();
                register_symbols(obj_idx, &obj, globals);
                objects.push(obj);
                changed = true;
            } else {
                i += 1;
            }
        }
    }
    Ok(())
}

/// Load a GNU thin archive. Member data lives in external files.
fn load_thin_archive(
    data: &[u8], archive_path: &str, objects: &mut Vec<ElfObject>,
    globals: &mut HashMap<String, GlobalSymbol>, _lib_paths: &[String],
) -> Result<(), String> {
    let member_names = parse_thin_archive_members(data)?;
    let archive_dir = Path::new(archive_path)
        .parent()
        .unwrap_or_else(|| Path::new("."));

    let mut member_objects: Vec<ElfObject> = Vec::new();
    for name in &member_names {
        let member_path = archive_dir.join(name);
        let member_data = std::fs::read(&member_path).map_err(|e| {
            format!("thin archive {}: failed to read member '{}': {}",
                    archive_path, member_path.display(), e)
        })?;
        if member_data.len() < 4 || member_data[0..4] != ELF_MAGIC { continue; }
        if member_data.len() >= 20 {
            let e_machine = read_u16(&member_data, 18);
            if e_machine != EM_AARCH64 { continue; }
        }
        let full_name = format!("{}({})", archive_path, name);
        if let Ok(obj) = parse_object(&member_data, &full_name) {
            member_objects.push(obj);
        }
    }

    let mut changed = true;
    while changed {
        changed = false;
        let mut i = 0;
        while i < member_objects.len() {
            if member_resolves_undefined(&member_objects[i], globals) {
                let obj = member_objects.remove(i);
                let obj_idx = objects.len();
                register_symbols(obj_idx, &obj, globals);
                objects.push(obj);
                changed = true;
            } else {
                i += 1;
            }
        }
    }
    Ok(())
}

fn member_resolves_undefined(obj: &ElfObject, globals: &HashMap<String, GlobalSymbol>) -> bool {
    for sym in &obj.symbols {
        if sym.is_undefined() || sym.is_local() { continue; }
        if sym.sym_type() == STT_SECTION || sym.sym_type() == STT_FILE { continue; }
        if sym.name.is_empty() { continue; }
        if let Some(existing) = globals.get(&sym.name) {
            if existing.defined_in.is_none() { return true; }
        }
    }
    false
}

fn register_symbols(obj_idx: usize, obj: &ElfObject, globals: &mut HashMap<String, GlobalSymbol>) {
    for sym in &obj.symbols {
        if sym.sym_type() == STT_SECTION || sym.sym_type() == STT_FILE { continue; }
        if sym.name.is_empty() || sym.is_local() { continue; }

        let is_defined = !sym.is_undefined() && sym.shndx != SHN_COMMON;

        if is_defined {
            let should_replace = match globals.get(&sym.name) {
                None => true,
                Some(e) => e.defined_in.is_none() || (e.info >> 4 == STB_WEAK && sym.is_global()),
            };
            if should_replace {
                globals.insert(sym.name.clone(), GlobalSymbol {
                    value: sym.value, size: sym.size, info: sym.info,
                    defined_in: Some(obj_idx), section_idx: sym.shndx,
                });
            }
        } else if sym.shndx == SHN_COMMON {
            if !globals.contains_key(&sym.name) {
                globals.insert(sym.name.clone(), GlobalSymbol {
                    value: sym.value, size: sym.size, info: sym.info,
                    defined_in: Some(obj_idx), section_idx: SHN_COMMON,
                });
            }
        } else if !globals.contains_key(&sym.name) {
            globals.insert(sym.name.clone(), GlobalSymbol {
                value: 0, size: 0, info: sym.info,
                defined_in: None, section_idx: SHN_UNDEF,
            });
        }
    }
}

fn resolve_lib(name: &str, paths: &[String]) -> Option<String> {
    crate::backend::linker_common::resolve_lib(name, paths, true)
}

// ── Section merging ────────────────────────────────────────────────────

fn map_section_name(name: &str) -> String {
    crate::backend::linker_common::map_section_name(name).to_string()
}

fn merge_sections(
    objects: &[ElfObject],
    output_sections: &mut Vec<OutputSection>,
    section_map: &mut HashMap<(usize, usize), (usize, u64)>,
) {
    let mut output_map: HashMap<String, usize> = HashMap::new();

    for obj_idx in 0..objects.len() {
        for sec_idx in 0..objects[obj_idx].sections.len() {
            let sec = &objects[obj_idx].sections[sec_idx];
            if sec.flags & SHF_ALLOC == 0 { continue; }
            if matches!(sec.sh_type, SHT_NULL | SHT_STRTAB | SHT_SYMTAB | SHT_RELA | SHT_REL | SHT_GROUP) { continue; }
            if sec.flags & SHF_EXCLUDE != 0 { continue; }

            let output_name = map_section_name(&sec.name);
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

    // Calculate offsets within each output section
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

    // Build section_map
    for (out_idx, out_sec) in output_sections.iter().enumerate() {
        for input in &out_sec.inputs {
            section_map.insert((input.object_idx, input.section_idx), (out_idx, input.output_offset));
        }
    }

    // Sort sections: RO -> Exec -> RW(progbits) -> RW(nobits)
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

    // Update section_map with new indices
    let old_map: Vec<_> = section_map.drain().collect();
    for ((obj_idx, sec_idx), (old_out_idx, off)) in old_map {
        if let Some(&new_out_idx) = index_remap.get(&old_out_idx) {
            section_map.insert((obj_idx, sec_idx), (new_out_idx, off));
        }
    }
}

fn allocate_common_symbols(globals: &mut HashMap<String, GlobalSymbol>, output_sections: &mut Vec<OutputSection>) {
    let common_syms: Vec<(String, u64, u64)> = globals.iter()
        .filter(|(_, sym)| sym.section_idx == SHN_COMMON && sym.defined_in.is_some())
        .map(|(name, sym)| (name.clone(), sym.value.max(1), sym.size))
        .collect();
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
            sym.value = bss_off;
            sym.section_idx = 0xffff; // sentinel for COMMON-in-BSS
        }
        if *alignment > output_sections[bss_idx].alignment {
            output_sections[bss_idx].alignment = *alignment;
        }
        bss_off += size;
    }
    output_sections[bss_idx].mem_size = bss_off;
}

// ── ELF emission ───────────────────────────────────────────────────────

fn emit_executable(
    objects: &[ElfObject],
    globals: &mut HashMap<String, GlobalSymbol>,
    output_sections: &mut [OutputSection],
    section_map: &HashMap<(usize, usize), (usize, u64)>,
    output_path: &str,
) -> Result<(), String> {
    if std::env::var("LINKER_DEBUG").is_ok() {
        eprintln!("output sections:");
        for (i, sec) in output_sections.iter().enumerate() {
            eprintln!("  [{}]: {} type={} flags=0x{:x} size={} align={}", i, sec.name, sec.sh_type, sec.flags, sec.mem_size, sec.alignment);
        }
    }

    // Layout: Single RX LOAD segment from file offset 0 (ELF hdr + phdrs + text + rodata),
    // followed by a RW LOAD segment for data + bss, plus TLS and GNU_STACK phdrs.
    let has_tls = output_sections.iter().any(|s| s.flags & SHF_TLS != 0 && s.mem_size > 0);
    let phdr_count: u64 = 2 + if has_tls { 1 } else { 0 } + 1; // 2 LOAD + optional TLS + GNU_STACK
    let phdr_total_size = phdr_count * 56;
    let debug_layout = std::env::var("LINKER_DEBUG_LAYOUT").is_ok();

    // === Layout: RX segment (starts at file offset 0, vaddr BASE_ADDR) ===
    let mut offset = 64 + phdr_total_size; // After ELF header + phdrs

    // Text sections (executable)
    for sec in output_sections.iter_mut() {
        if sec.flags & SHF_EXECINSTR != 0 && sec.flags & SHF_ALLOC != 0 {
            let a = sec.alignment.max(4);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = BASE_ADDR + offset;
            sec.file_offset = offset;
            offset += sec.mem_size;
        }
    }

    // Rodata sections (read-only, in same RX segment)
    for sec in output_sections.iter_mut() {
        if sec.flags & SHF_ALLOC != 0 && sec.flags & SHF_EXECINSTR == 0 &&
           sec.flags & SHF_WRITE == 0 && sec.sh_type != SHT_NOBITS &&
           sec.flags & SHF_TLS == 0 {
            let a = sec.alignment.max(1);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = BASE_ADDR + offset;
            sec.file_offset = offset;
            if debug_layout {
                eprintln!("  LAYOUT RO: {} addr=0x{:x} foff=0x{:x} sz=0x{:x} flags=0x{:x}",
                    sec.name, sec.addr, sec.file_offset, sec.mem_size, sec.flags);
            }
            offset += sec.mem_size;
        }
    }
    let rx_filesz = offset; // RX segment: [0, rx_filesz)
    let _rx_memsz = rx_filesz;

    // Pre-count IFUNC symbols so we can reserve space for IPLT stubs in the RX gap.
    // Each IPLT stub is 16 bytes (ADRP + LDR + BR + NOP), placed in the gap between
    // the RX segment end and the page-aligned RW segment start.
    let pre_iplt_count = globals.iter()
        .filter(|(_, gsym)| gsym.info & 0xf == STT_GNU_IFUNC && gsym.defined_in.is_some())
        .count() as u64;
    let iplt_stubs_needed = pre_iplt_count * 16;
    if iplt_stubs_needed > 0 {
        // Ensure the gap after rx_filesz is large enough for IPLT stubs.
        // The stubs will be placed at 16-byte aligned offset after rx_filesz.
        let stub_start = (offset + 15) & !15;
        let stub_end = stub_start + iplt_stubs_needed;
        // Make sure offset is at least stub_end so page-alignment leaves enough room
        if offset < stub_end {
            offset = stub_end;
        }
    }

    // === Layout: RW segment (page-aligned) ===
    offset = (offset + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
    let rw_page_offset = offset;
    let rw_page_addr = BASE_ADDR + offset;

    // TLS data (.tdata) first in RW
    let mut tls_addr = 0u64;
    let mut tls_file_offset = 0u64;
    let mut tls_file_size = 0u64;
    let mut tls_mem_size = 0u64;
    let mut tls_align = 1u64;
    for sec in output_sections.iter_mut() {
        if sec.flags & SHF_TLS != 0 && sec.flags & SHF_ALLOC != 0 && sec.sh_type != SHT_NOBITS {
            let a = sec.alignment.max(1);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = BASE_ADDR + offset;
            sec.file_offset = offset;
            if tls_addr == 0 { tls_addr = sec.addr; tls_file_offset = offset; tls_align = a; }
            tls_file_size += sec.mem_size;
            tls_mem_size += sec.mem_size;
            if debug_layout {
                eprintln!("  LAYOUT TLS: {} addr=0x{:x} foff=0x{:x} sz=0x{:x} align={} flags=0x{:x}",
                    sec.name, sec.addr, sec.file_offset, sec.mem_size, a, sec.flags);
            }
            offset += sec.mem_size;
        }
    }
    // If only .tbss (NOBITS TLS) exists with no .tdata, we still need a TLS segment.
    // Set tls_addr/tls_file_offset to the current position so TPOFF calculations work.
    if tls_addr == 0 && has_tls {
        tls_addr = BASE_ADDR + offset;
        tls_file_offset = offset;
    }
    // TLS BSS (.tbss) - doesn't consume file space
    for sec in output_sections.iter_mut() {
        if sec.flags & SHF_TLS != 0 && sec.sh_type == SHT_NOBITS {
            let a = sec.alignment.max(1);
            let aligned = (tls_mem_size + a - 1) & !(a - 1);
            sec.addr = tls_addr + aligned;
            sec.file_offset = offset;
            if debug_layout {
                eprintln!("  LAYOUT TBSS: {} addr=0x{:x} aligned_off=0x{:x} sz=0x{:x} align={} tls_mem_size=0x{:x}",
                    sec.name, sec.addr, aligned, sec.mem_size, a, tls_mem_size);
            }
            tls_mem_size = aligned + sec.mem_size;
            if a > tls_align { tls_align = a; }
        }
    }
    if tls_mem_size > 0 {
        tls_mem_size = (tls_mem_size + tls_align - 1) & !(tls_align - 1);
    }

    // init_array
    for sec in output_sections.iter_mut() {
        if sec.name == ".init_array" {
            let a = sec.alignment.max(8);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = BASE_ADDR + offset;
            sec.file_offset = offset;
            offset += sec.mem_size;
            break;
        }
    }
    // fini_array
    for sec in output_sections.iter_mut() {
        if sec.name == ".fini_array" {
            let a = sec.alignment.max(8);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = BASE_ADDR + offset;
            sec.file_offset = offset;
            offset += sec.mem_size;
            break;
        }
    }

    // .data.rel.ro (relocated read-only data) - must come before .data
    for sec in output_sections.iter_mut() {
        if sec.name == ".data.rel.ro" {
            let a = sec.alignment.max(1);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = BASE_ADDR + offset;
            sec.file_offset = offset;
            if debug_layout {
                eprintln!("  LAYOUT RW: {} addr=0x{:x} foff=0x{:x} sz=0x{:x} flags=0x{:x}",
                    sec.name, sec.addr, sec.file_offset, sec.mem_size, sec.flags);
            }
            offset += sec.mem_size;
        }
    }
    // Remaining data sections (writable, non-BSS, non-TLS)
    for sec in output_sections.iter_mut() {
        if sec.flags & SHF_ALLOC != 0 && sec.flags & SHF_WRITE != 0 &&
           sec.sh_type != SHT_NOBITS && sec.flags & SHF_TLS == 0 &&
           sec.name != ".init_array" && sec.name != ".fini_array" &&
           sec.name != ".data.rel.ro" {
            let a = sec.alignment.max(1);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = BASE_ADDR + offset;
            sec.file_offset = offset;
            if debug_layout {
                eprintln!("  LAYOUT RW: {} addr=0x{:x} foff=0x{:x} sz=0x{:x} flags=0x{:x}",
                    sec.name, sec.addr, sec.file_offset, sec.mem_size, sec.flags);
            }
            offset += sec.mem_size;
        }
    }

    // GOT (Global Offset Table) - needed for R_AARCH64_ADR_GOT_PAGE / LD64_GOT_LO12_NC
    // and TLS IE relocations (which store TP offsets in GOT entries)
    let got_syms = reloc::collect_got_symbols(objects);
    let got_size = got_syms.len() as u64 * 8;
    offset = (offset + 7) & !7; // 8-byte align
    let got_offset = offset;
    let got_addr = BASE_ADDR + offset;
    let mut got_entries = HashMap::new();
    for (idx, (key, _kind)) in got_syms.iter().enumerate() {
        got_entries.insert(key.clone(), idx);
    }
    offset += got_size;

    // Collect IFUNC symbols before address resolution - we need them for layout.
    // Identify them by STT_GNU_IFUNC type in the symbol table.
    let mut ifunc_names: Vec<String> = Vec::new();
    for (name, gsym) in globals.iter() {
        if gsym.info & 0xf == STT_GNU_IFUNC && gsym.defined_in.is_some() {
            ifunc_names.push(name.clone());
        }
    }
    ifunc_names.sort(); // deterministic order

    // IPLT GOT slots for IFUNC symbols (one 8-byte slot per IFUNC)
    let iplt_got_count = ifunc_names.len();
    let iplt_got_size = iplt_got_count as u64 * 8;
    offset = (offset + 7) & !7;
    let iplt_got_offset = offset;
    let iplt_got_addr = BASE_ADDR + offset;
    offset += iplt_got_size;

    // IRELATIVE relocation entries (.rela.iplt) in the RW segment
    // Format: Elf64_Rela { r_offset: u64, r_info: u64, r_addend: i64 } = 24 bytes each
    let rela_iplt_size = iplt_got_count as u64 * 24;
    offset = (offset + 7) & !7;
    let rela_iplt_offset = offset;
    let rela_iplt_addr = BASE_ADDR + offset;
    let rela_iplt_end_addr = rela_iplt_addr + rela_iplt_size;
    offset += rela_iplt_size;

    let rw_filesz = offset - rw_page_offset;

    // BSS (nobits, non-TLS)
    let bss_addr = BASE_ADDR + offset;
    let mut bss_size = 0u64;
    for sec in output_sections.iter_mut() {
        if sec.sh_type == SHT_NOBITS && sec.flags & SHF_ALLOC != 0 && sec.flags & SHF_TLS == 0 {
            let a = sec.alignment.max(1);
            let aligned = (bss_addr + bss_size + a - 1) & !(a - 1);
            bss_size = aligned - bss_addr + sec.mem_size;
            sec.addr = aligned;
            sec.file_offset = offset;
        }
    }
    let rw_memsz = if bss_size > 0 { (bss_addr + bss_size) - rw_page_addr } else { rw_filesz };

    // IPLT stubs go in the RX padding between rx_filesz and rw_page_offset.
    // Each stub: 16 bytes (ADRP + LDR + BR + NOP)
    let iplt_stub_size = iplt_got_count as u64 * 16;
    let iplt_stub_file_off = (rx_filesz + 15) & !15; // 16-byte aligned
    let iplt_stub_addr = BASE_ADDR + iplt_stub_file_off;
    if iplt_stub_size > 0 && iplt_stub_file_off + iplt_stub_size > rw_page_offset {
        return Err(format!("IPLT stubs ({} bytes) don't fit in RX padding (gap={})",
            iplt_stub_size, rw_page_offset - iplt_stub_file_off));
    }

    if std::env::var("LINKER_DEBUG").is_ok() {
        eprintln!("section layout:");
        for sec in output_sections.iter() {
            eprintln!("  {} addr=0x{:x} foff=0x{:x} size=0x{:x}", sec.name, sec.addr, sec.file_offset, sec.mem_size);
        }
        eprintln!("  GOT addr=0x{:x} foff=0x{:x} size=0x{:x} entries={}", got_addr, got_offset, got_size, got_entries.len());
        if iplt_got_count > 0 {
            eprintln!("  IPLT GOT addr=0x{:x} entries={}", iplt_got_addr, iplt_got_count);
            eprintln!("  RELA.IPLT addr=0x{:x}..0x{:x}", rela_iplt_addr, rela_iplt_end_addr);
            eprintln!("  IPLT stubs addr=0x{:x}", iplt_stub_addr);
        }
        eprintln!("  BSS addr=0x{:x} size=0x{:x}", bss_addr, bss_size);
    }

    // Merge section data
    for sec in output_sections.iter_mut() {
        if sec.sh_type == SHT_NOBITS { continue; }
        let mut data = vec![0u8; sec.mem_size as usize];
        for input in &sec.inputs {
            let sd = &objects[input.object_idx].section_data[input.section_idx];
            let s = input.output_offset as usize;
            let e = s + sd.len();
            if e <= data.len() && !sd.is_empty() {
                data[s..e].copy_from_slice(sd);
            }
        }
        sec.data = data;
    }

    // Update global symbol addresses
    for (name, gsym) in globals.iter_mut() {
        if let Some(obj_idx) = gsym.defined_in {
            if obj_idx == usize::MAX { continue; } // linker-defined
            if gsym.section_idx == SHN_COMMON || gsym.section_idx == 0xffff {
                if let Some(bss_sec) = output_sections.iter().find(|s| s.name == ".bss") {
                    gsym.value += bss_sec.addr;
                }
            } else if gsym.section_idx != SHN_UNDEF && gsym.section_idx != SHN_ABS {
                let si = gsym.section_idx as usize;
                if let Some(&(oi, so)) = section_map.get(&(obj_idx, si)) {
                    let old_val = gsym.value;
                    gsym.value += output_sections[oi].addr + so;
                    if std::env::var("LINKER_DEBUG").is_ok() && gsym.info & 0xf == STT_TLS {
                        eprintln!("  TLS sym '{}': old=0x{:x} -> new=0x{:x} (sec={} addr=0x{:x} off=0x{:x})",
                                  name, old_val, gsym.value, output_sections[oi].name, output_sections[oi].addr, so);
                    }
                } else if std::env::var("LINKER_DEBUG").is_ok() && gsym.info & 0xf == STT_TLS {
                    eprintln!("  TLS sym '{}': NO MAPPING for ({}, {})", name, obj_idx, si);
                }
            }
        }
    }

    // Build IFUNC resolver address map (now that addresses are resolved)
    let ifunc_syms: Vec<(String, u64)> = ifunc_names.iter()
        .map(|name| {
            let resolver_addr = globals.get(name).map(|g| g.value).unwrap_or(0);
            (name.clone(), resolver_addr)
        })
        .collect();

    // Redirect IFUNC symbols to their PLT stubs
    for (i, (name, _resolver_addr)) in ifunc_syms.iter().enumerate() {
        let plt_addr = iplt_stub_addr + i as u64 * 16;
        if let Some(gsym) = globals.get_mut(name) {
            gsym.value = plt_addr;
            // Change type from IFUNC to FUNC so relocations treat it normally
            gsym.info = (gsym.info & 0xf0) | STT_FUNC;
        }
    }

    if std::env::var("LINKER_DEBUG").is_ok() && !ifunc_syms.is_empty() {
        for (i, (name, resolver)) in ifunc_syms.iter().enumerate() {
            eprintln!("  IFUNC[{}]: {} resolver=0x{:x} plt=0x{:x} got=0x{:x}",
                      i, name, resolver, iplt_stub_addr + i as u64 * 16,
                      iplt_got_addr + i as u64 * 8);
        }
    }

    // Compute init/fini array boundaries
    let init_array_start = output_sections.iter().find(|s| s.name == ".init_array").map(|s| s.addr).unwrap_or(0);
    let init_array_end = output_sections.iter().find(|s| s.name == ".init_array").map(|s| s.addr + s.mem_size).unwrap_or(0);
    let fini_array_start = output_sections.iter().find(|s| s.name == ".fini_array").map(|s| s.addr).unwrap_or(0);
    let fini_array_end = output_sections.iter().find(|s| s.name == ".fini_array").map(|s| s.addr + s.mem_size).unwrap_or(0);
    let init_addr = output_sections.iter().find(|s| s.name == ".init").map(|s| s.addr).unwrap_or(0);
    let fini_addr = output_sections.iter().find(|s| s.name == ".fini").map(|s| s.addr).unwrap_or(0);

    // Define linker-provided symbols using shared infrastructure (consistent
    // with x86-64/i686/RISC-V via get_standard_linker_symbols)
    let text_seg_end = BASE_ADDR + rx_filesz;
    let linker_addrs = LinkerSymbolAddresses {
        base_addr: BASE_ADDR,
        got_addr,
        dynamic_addr: 0, // ARM uses static linking only (no .dynamic section)
        bss_addr,
        bss_size,
        text_end: text_seg_end,
        data_start: rw_page_addr,
        init_array_start,
        init_array_size: init_array_end - init_array_start,
        fini_array_start,
        fini_array_size: fini_array_end - fini_array_start,
        preinit_array_start: 0,
        preinit_array_size: 0,
        rela_iplt_start: rela_iplt_addr,
        rela_iplt_size,
    };
    for sym in &get_standard_linker_symbols(&linker_addrs) {
        if globals.get(sym.name).map(|g| g.defined_in.is_none()).unwrap_or(true) {
            globals.insert(sym.name.to_string(), GlobalSymbol {
                value: sym.value, size: 0, info: (sym.binding << 4) | STT_OBJECT,
                defined_in: Some(usize::MAX), section_idx: SHN_ABS,
            });
        }
    }
    // ARM-specific linker symbols not in the shared list
    let arm_extra_syms: [(&str, u64); 3] = [
        ("__GNU_EH_FRAME_HDR", 0),
        ("_init", init_addr),
        ("_fini", fini_addr),
    ];
    for (name, val) in &arm_extra_syms {
        if globals.get(*name).map(|g| g.defined_in.is_none()).unwrap_or(true) {
            globals.insert(name.to_string(), GlobalSymbol {
                value: *val, size: 0, info: (STB_GLOBAL << 4) | STT_OBJECT,
                defined_in: Some(usize::MAX), section_idx: SHN_ABS,
            });
        }
    }

    let entry_addr = globals.get("_start").map(|s| s.value).unwrap_or(BASE_ADDR);

    if std::env::var("LINKER_DEBUG").is_ok() {
        if let Some(g) = globals.get("main") { eprintln!("  main resolved to 0x{:x}", g.value); }
        if let Some(g) = globals.get("_start") { eprintln!("  _start resolved to 0x{:x}", g.value); }
        if let Some(g) = globals.get("__libc_start_main") { eprintln!("  __libc_start_main resolved to 0x{:x}", g.value); }
        eprintln!("  entry_addr = 0x{:x}", entry_addr);
    }

    // Build output buffer
    let file_size = offset as usize;
    let mut out = vec![0u8; file_size];

    // Write section data
    for sec in output_sections.iter() {
        if sec.sh_type == SHT_NOBITS || sec.data.is_empty() { continue; }
        write_bytes(&mut out, sec.file_offset as usize, &sec.data);
    }

    // Populate GOT entries with resolved symbol addresses or TP offsets.
    // We re-walk the relocations to resolve each symbol (including locals)
    // rather than only looking up global symbol names.
    let globals_snap = globals.clone();
    let got_info = reloc::GotInfo { got_addr, entries: got_entries };
    let got_kind_map: HashMap<String, reloc::GotEntryKind> = got_syms.iter()
        .map(|(k, kind)| (k.clone(), *kind))
        .collect();
    // Build a resolved address map for GOT entries by walking relocations
    let mut got_resolved: HashMap<String, u64> = HashMap::new();
    for obj_idx in 0..objects.len() {
        for sec_idx in 0..objects[obj_idx].sections.len() {
            for rela in &objects[obj_idx].relocations[sec_idx] {
                match rela.rela_type {
                    R_AARCH64_ADR_GOT_PAGE | R_AARCH64_LD64_GOT_LO12_NC |
                    reloc::R_AARCH64_TLSIE_ADR_GOTTPREL_PAGE21 | reloc::R_AARCH64_TLSIE_LD64_GOTTPREL_LO12_NC => {
                        let si = rela.sym_idx as usize;
                        if si < objects[obj_idx].symbols.len() {
                            let sym = &objects[obj_idx].symbols[si];
                            let key = reloc::got_key(obj_idx, sym);
                            got_resolved.entry(key).or_insert_with(|| {
                                
                                reloc::resolve_sym(obj_idx, sym, &globals_snap,
                                                              section_map, output_sections)
                            });
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    for (key, &idx) in &got_info.entries {
        let sym_addr = got_resolved.get(key).copied().unwrap_or(0);
        let val = match got_kind_map.get(key) {
            Some(&reloc::GotEntryKind::TlsIE) => {
                // GOT entry holds the TP-relative offset for this TLS variable
                // AArch64 variant 1: tp_offset = (sym_addr - tls_base) + 16
                if tls_addr != 0 {
                    let offset = (sym_addr as i64) - (tls_addr as i64) + 16;
                    if std::env::var("LINKER_DEBUG_TLS").is_ok() {
                        eprintln!("  GOT TLS IE: key='{}' sym_addr=0x{:x} tls_addr=0x{:x} -> got_val=0x{:x}",
                            key, sym_addr, tls_addr, offset as u64);
                    }
                    offset as u64
                } else {
                    sym_addr
                }
            }
            _ => sym_addr,
        };
        let entry_off = got_offset as usize + idx * 8;
        w64(&mut out, entry_off, val);
        if std::env::var("LINKER_DEBUG").is_ok() && val == 0 {
            eprintln!("  GOT[{}] = 0 for symbol '{}'", idx, key);
        }
    }

    // Populate IPLT GOT slots (initially with resolver addresses), RELA entries, and PLT stubs
    for (i, (_name, resolver_addr)) in ifunc_syms.iter().enumerate() {
        // IPLT GOT slot: initially contains resolver address (will be overwritten at startup)
        let got_slot_off = iplt_got_offset as usize + i * 8;
        if got_slot_off + 8 <= out.len() {
            w64(&mut out, got_slot_off, *resolver_addr);
        }

        // RELA.IPLT entry: { r_offset, r_info, r_addend }
        let rela_off = rela_iplt_offset as usize + i * 24;
        let got_slot_addr = iplt_got_addr + i as u64 * 8;
        if rela_off + 24 <= out.len() {
            w64(&mut out, rela_off, got_slot_addr);        // r_offset: GOT slot VA
            w64(&mut out, rela_off + 8, 0x408);            // r_info: R_AARCH64_IRELATIVE
            w64(&mut out, rela_off + 16, *resolver_addr);  // r_addend: resolver VA
        }

        // PLT stub: ADRP x16, got_page; LDR x17, [x16, #got_lo]; BR x17; NOP
        let plt_off = iplt_stub_file_off as usize + i * 16;
        let plt_addr = iplt_stub_addr + i as u64 * 16;
        if plt_off + 16 <= out.len() {
            // ADRP x16, page_of(got_slot)
            let page_g = got_slot_addr & !0xFFF;
            let page_p = plt_addr & !0xFFF;
            let page_diff = ((page_g as i64 - page_p as i64) >> 12) as i32;
            let immlo = (page_diff & 3) as u32;
            let immhi = ((page_diff >> 2) & 0x7ffff) as u32;
            let adrp = 0x90000010u32 | (immlo << 29) | (immhi << 5); // ADRP x16
            w32(&mut out, plt_off, adrp);

            // LDR x17, [x16, #lo12(got_slot)]
            let lo12 = (got_slot_addr & 0xFFF) as u32;
            let ldr = 0xf9400211u32 | ((lo12 / 8) << 10); // LDR x17, [x16, #imm]
            w32(&mut out, plt_off + 4, ldr);

            // BR x17
            w32(&mut out, plt_off + 8, 0xd61f0220u32);

            // NOP
            w32(&mut out, plt_off + 12, 0xd503201fu32);
        }
    }

    // Apply relocations
    let tls_info = reloc::TlsInfo { tls_addr, tls_size: tls_mem_size };
    reloc::apply_relocations(objects, &globals_snap, output_sections, section_map,
                             &mut out, &tls_info, &got_info)?;

    // === ELF header ===
    out[0..4].copy_from_slice(&ELF_MAGIC);
    out[4] = ELFCLASS64; out[5] = ELFDATA2LSB; out[6] = 1;
    out[7] = 3; // ELFOSABI_GNU (matches ld output for static exes)
    w16(&mut out, 16, ET_EXEC);
    w16(&mut out, 18, EM_AARCH64);
    w32(&mut out, 20, 1);
    w64(&mut out, 24, entry_addr);
    w64(&mut out, 32, 64); // e_phoff
    w64(&mut out, 40, 0);  // e_shoff
    w32(&mut out, 48, 0);  // e_flags
    w16(&mut out, 52, 64); // e_ehsize
    w16(&mut out, 54, 56); // e_phentsize
    w16(&mut out, 56, phdr_count as u16);
    w16(&mut out, 58, 64); // e_shentsize
    w16(&mut out, 60, 0);  // e_shnum
    w16(&mut out, 62, 0);  // e_shstrndx

    // === Program headers ===
    let mut ph = 64usize;

    // LOAD: RX segment starting from file offset 0 (includes ELF header + PLT stubs)
    let rx_actual_filesz = if iplt_stub_size > 0 { iplt_stub_file_off + iplt_stub_size } else { rx_filesz };
    let rx_actual_memsz = rx_actual_filesz;
    wphdr(&mut out, ph, PT_LOAD, PF_R | PF_X,
          0, BASE_ADDR, rx_actual_filesz, rx_actual_memsz, PAGE_SIZE);
    ph += 56;

    // LOAD: RW segment
    wphdr(&mut out, ph, PT_LOAD, PF_R | PF_W,
          rw_page_offset, rw_page_addr, rw_filesz, rw_memsz, PAGE_SIZE);
    ph += 56;

    // TLS segment
    if has_tls && tls_addr != 0 {
        wphdr(&mut out, ph, PT_TLS, PF_R,
              tls_file_offset, tls_addr, tls_file_size, tls_mem_size, tls_align);
        ph += 56;
    }

    // GNU_STACK
    wphdr(&mut out, ph, PT_GNU_STACK, PF_R | PF_W, 0, 0, 0, 0, 0x10);

    // Write output
    std::fs::write(output_path, &out).map_err(|e| format!("failed to write '{}': {}", output_path, e))?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let _ = std::fs::set_permissions(output_path, std::fs::Permissions::from_mode(0o755));
    }
    Ok(())
}

fn wphdr(buf: &mut [u8], off: usize, pt: u32, flags: u32, foff: u64, va: u64, fsz: u64, msz: u64, align: u64) {
    w32(buf, off, pt);
    w32(buf, off + 4, flags);
    w64(buf, off + 8, foff);
    w64(buf, off + 16, va);
    w64(buf, off + 24, va); // p_paddr = p_vaddr
    w64(buf, off + 32, fsz);
    w64(buf, off + 40, msz);
    w64(buf, off + 48, align);
}
