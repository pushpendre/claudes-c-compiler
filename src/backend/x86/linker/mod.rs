/// Native x86-64 ELF linker.
///
/// Links ELF relocatable object files (.o) and static archives (.a) into
/// a dynamically-linked ELF executable. Resolves undefined symbols against
/// shared libraries (e.g., libc.so.6) and generates PLT/GOT entries for
/// dynamic function calls.
#[allow(dead_code)]
pub mod elf;

use std::collections::HashMap;
use std::path::Path;

use elf::*;

/// Base virtual address for the executable (standard non-PIE x86-64 address)
const BASE_ADDR: u64 = 0x400000;
/// Page size for alignment
const PAGE_SIZE: u64 = 0x1000;
/// Dynamic linker path
const INTERP: &[u8] = b"/lib64/ld-linux-x86-64.so.2\0";

/// Represents a merged input section assigned to an output section
struct InputSection {
    object_idx: usize,
    section_idx: usize,
    output_offset: u64,
    size: u64,
}

/// An output section in the final executable
struct OutputSection {
    name: String,
    sh_type: u32,
    flags: u64,
    alignment: u64,
    inputs: Vec<InputSection>,
    data: Vec<u8>,
    addr: u64,
    file_offset: u64,
    mem_size: u64,
}

/// A resolved global symbol
#[derive(Clone)]
struct GlobalSymbol {
    value: u64,
    size: u64,
    info: u8,
    defined_in: Option<usize>,
    #[allow(dead_code)]
    from_lib: Option<String>,
    plt_idx: Option<usize>,
    got_idx: Option<usize>,
    section_idx: u16,
    is_dynamic: bool,
    copy_reloc: bool,
}

fn map_section_name(name: &str) -> String {
    if name.starts_with(".text.") || name == ".text" { return ".text".to_string(); }
    if name.starts_with(".data.rel.ro") { return ".data.rel.ro".to_string(); }
    if name.starts_with(".data.") || name == ".data" { return ".data".to_string(); }
    if name.starts_with(".rodata.") || name == ".rodata" { return ".rodata".to_string(); }
    if name.starts_with(".bss.") || name == ".bss" { return ".bss".to_string(); }
    if name.starts_with(".init_array") { return ".init_array".to_string(); }
    if name.starts_with(".fini_array") { return ".fini_array".to_string(); }
    if name.starts_with(".tbss.") || name == ".tbss" { return ".tbss".to_string(); }
    if name.starts_with(".tdata.") || name == ".tdata" { return ".tdata".to_string(); }
    if name.starts_with(".gcc_except_table") { return ".gcc_except_table".to_string(); }
    if name.starts_with(".eh_frame") { return ".eh_frame".to_string(); }
    if name.starts_with(".note.") { return name.to_string(); }
    name.to_string()
}

struct DynStrTab {
    data: Vec<u8>,
    offsets: HashMap<String, usize>,
}

impl DynStrTab {
    fn new() -> Self { Self { data: vec![0], offsets: HashMap::new() } }

    fn add(&mut self, s: &str) -> usize {
        if s.is_empty() { return 0; }
        if let Some(&off) = self.offsets.get(s) { return off; }
        let off = self.data.len();
        self.data.extend_from_slice(s.as_bytes());
        self.data.push(0);
        self.offsets.insert(s.to_string(), off);
        off
    }

    fn get_offset(&self, s: &str) -> usize {
        if s.is_empty() { 0 } else { self.offsets.get(s).copied().unwrap_or(0) }
    }
}

/// Public entry point for the built-in linker.
pub fn link_builtin(
    object_files: &[&str],
    output_path: &str,
    user_args: &[String],
    lib_paths: &[&str],
    needed_libs: &[&str],
    crt_objects_before: &[&str],
    crt_objects_after: &[&str],
) -> Result<(), String> {
    let mut objects: Vec<ElfObject> = Vec::new();
    let mut globals: HashMap<String, GlobalSymbol> = HashMap::new();
    let mut needed_sonames: Vec<String> = Vec::new();
    let lib_path_strings: Vec<String> = lib_paths.iter().map(|s| s.to_string()).collect();

    let resolve_lib = |name: &str, paths: &[String]| -> Option<String> {
        // -l:filename means search for exact filename (no lib prefix or .so/.a suffix)
        if let Some(exact) = name.strip_prefix(':') {
            for dir in paths {
                let p = format!("{}/{}", dir, exact);
                if Path::new(&p).exists() { return Some(p); }
            }
            return None;
        }
        for dir in paths {
            let so = format!("{}/lib{}.so", dir, name);
            if Path::new(&so).exists() { return Some(so); }
            let a = format!("{}/lib{}.a", dir, name);
            if Path::new(&a).exists() { return Some(a); }
        }
        None
    };

    // Load CRT objects before user objects
    for path in crt_objects_before {
        if Path::new(path).exists() {
            load_file(path, &mut objects, &mut globals, &mut needed_sonames, &lib_path_strings)?;
        }
    }

    // Load user object files
    for path in object_files {
        load_file(path, &mut objects, &mut globals, &mut needed_sonames, &lib_path_strings)?;
    }

    // Parse user args: extract -L paths, -l libs, and bare .o/.a file paths
    let mut extra_lib_paths: Vec<String> = Vec::new();
    let mut libs_to_load: Vec<String> = Vec::new();
    let mut extra_object_files: Vec<String> = Vec::new();
    let mut i = 0;
    let args: Vec<&str> = user_args.iter().map(|s| s.as_str()).collect();
    while i < args.len() {
        let arg = args[i];
        if let Some(path) = arg.strip_prefix("-L") {
            let p = if path.is_empty() && i + 1 < args.len() { i += 1; args[i] } else { path };
            extra_lib_paths.push(p.to_string());
        } else if let Some(lib) = arg.strip_prefix("-l") {
            let l = if lib.is_empty() && i + 1 < args.len() { i += 1; args[i] } else { lib };
            libs_to_load.push(l.to_string());
        } else if let Some(wl_arg) = arg.strip_prefix("-Wl,") {
            for part in wl_arg.split(',') {
                if let Some(lpath) = part.strip_prefix("-L") {
                    extra_lib_paths.push(lpath.to_string());
                } else if let Some(lib) = part.strip_prefix("-l") {
                    libs_to_load.push(lib.to_string());
                }
            }
        } else if !arg.starts_with('-') && Path::new(arg).exists() {
            // Bare file path: .o object file, .a static archive, or other input file
            extra_object_files.push(arg.to_string());
        }
        i += 1;
    }

    // Load extra object/archive files from user args (these come from
    // linker_ordered_items when the driver passes pre-existing .o/.a files)
    for path in &extra_object_files {
        load_file(path, &mut objects, &mut globals, &mut needed_sonames, &lib_path_strings)?;
    }

    let mut all_lib_paths: Vec<String> = extra_lib_paths;
    all_lib_paths.extend(lib_path_strings.iter().cloned());

    // Load CRT objects after
    for path in crt_objects_after {
        if Path::new(path).exists() {
            load_file(path, &mut objects, &mut globals, &mut needed_sonames, &all_lib_paths)?;
        }
    }

    // Load needed libraries using group resolution (like ld's --start-group/--end-group).
    // This iterates all archives until no new objects are pulled in, handling circular
    // dependencies between archives (e.g., libc.a needing symbols from libgcc.a and vice versa).
    {
        let mut all_lib_names: Vec<String> = needed_libs.iter().map(|s| s.to_string()).collect();
        all_lib_names.extend(libs_to_load.iter().cloned());

        let mut lib_paths_resolved: Vec<String> = Vec::new();
        for lib_name in &all_lib_names {
            if let Some(lib_path) = resolve_lib(lib_name, &all_lib_paths) {
                if !lib_paths_resolved.contains(&lib_path) {
                    lib_paths_resolved.push(lib_path);
                }
            }
        }

        // Group loading: iterate all archives until stable
        let mut changed = true;
        while changed {
            changed = false;
            let prev_count = objects.len();
            for lib_path in &lib_paths_resolved {
                load_file(lib_path, &mut objects, &mut globals, &mut needed_sonames, &all_lib_paths)?;
            }
            if objects.len() != prev_count {
                changed = true;
            }
        }
    }

    // Resolve remaining undefined symbols
    resolve_dynamic_symbols(&mut globals, &mut needed_sonames)?;

    // Merge sections
    let mut output_sections: Vec<OutputSection> = Vec::new();
    let mut section_map: HashMap<(usize, usize), (usize, u64)> = HashMap::new();
    merge_sections(&objects, &mut output_sections, &mut section_map);

    // Allocate COMMON symbols
    allocate_common_symbols(&mut globals, &mut output_sections);

    // Create PLT/GOT
    let (plt_names, got_entries) = create_plt_got(&objects, &mut globals);

    // Emit executable
    emit_executable(
        &objects, &mut globals, &mut output_sections, &section_map,
        &plt_names, &got_entries, &needed_sonames, output_path,
    )
}

fn load_file(
    path: &str, objects: &mut Vec<ElfObject>, globals: &mut HashMap<String, GlobalSymbol>,
    needed_sonames: &mut Vec<String>, lib_paths: &[String],
) -> Result<(), String> {
    let data = std::fs::read(path).map_err(|e| format!("failed to read '{}': {}", path, e))?;

    if data.len() >= 8 && &data[0..8] == b"!<arch>\n" {
        return load_archive(&data, path, objects, globals, needed_sonames, lib_paths);
    }

    if data.len() >= 4 && data[0..4] != ELF_MAGIC {
        if let Ok(text) = std::str::from_utf8(&data) {
            if let Some(paths) = parse_linker_script(text) {
                for lib_path in &paths {
                    if Path::new(lib_path).exists() {
                        load_file(lib_path, objects, globals, needed_sonames, lib_paths)?;
                    }
                }
                return Ok(());
            }
        }
        return Err(format!("{}: not a valid ELF object or archive", path));
    }

    if data.len() >= 18 {
        let e_type = u16::from_le_bytes([data[16], data[17]]);
        if e_type == ET_DYN {
            return load_shared_library(path, globals, needed_sonames);
        }
    }

    let obj = parse_object(&data, path)?;
    let obj_idx = objects.len();
    register_symbols(obj_idx, &obj, globals);
    objects.push(obj);
    Ok(())
}

fn load_archive(
    data: &[u8], archive_path: &str, objects: &mut Vec<ElfObject>,
    globals: &mut HashMap<String, GlobalSymbol>,
    _needed_sonames: &mut Vec<String>, _lib_paths: &[String],
) -> Result<(), String> {
    let members = parse_archive_members(data)?;
    let mut member_objects: Vec<ElfObject> = Vec::new();
    for (name, offset, size) in &members {
        let member_data = &data[*offset..*offset + *size];
        if member_data.len() < 4 || member_data[0..4] != ELF_MAGIC { continue; }
        let full_name = format!("{}({})", archive_path, name);
        if let Ok(obj) = parse_object(member_data, &full_name) {
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
            if existing.defined_in.is_none() && !existing.is_dynamic { return true; }
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
                Some(e) => e.defined_in.is_none() || e.is_dynamic || (e.info >> 4 == STB_WEAK && sym.is_global()),
            };
            if should_replace {
                globals.insert(sym.name.clone(), GlobalSymbol {
                    value: sym.value, size: sym.size, info: sym.info,
                    defined_in: Some(obj_idx), from_lib: None,
                    plt_idx: None, got_idx: None,
                    section_idx: sym.shndx, is_dynamic: false, copy_reloc: false,
                });
            }
        } else if sym.shndx == SHN_COMMON {
            if !globals.contains_key(&sym.name) {
                globals.insert(sym.name.clone(), GlobalSymbol {
                    value: sym.value, size: sym.size, info: sym.info,
                    defined_in: Some(obj_idx), from_lib: None,
                    plt_idx: None, got_idx: None,
                    section_idx: SHN_COMMON, is_dynamic: false, copy_reloc: false,
                });
            }
        } else if !globals.contains_key(&sym.name) {
            globals.insert(sym.name.clone(), GlobalSymbol {
                value: 0, size: 0, info: sym.info,
                defined_in: None, from_lib: None,
                plt_idx: None, got_idx: None,
                section_idx: SHN_UNDEF, is_dynamic: false, copy_reloc: false,
            });
        }
    }
}

fn load_shared_library(
    path: &str, globals: &mut HashMap<String, GlobalSymbol>, needed_sonames: &mut Vec<String>,
) -> Result<(), String> {
    let data = std::fs::read(path).map_err(|e| format!("failed to read '{}': {}", path, e))?;
    if data.len() >= 4 && data[0..4] != ELF_MAGIC {
        if let Ok(text) = std::str::from_utf8(&data) {
            if let Some(paths) = parse_linker_script(text) {
                for lib_path in &paths {
                    if !Path::new(lib_path).exists() { continue; }
                    let lib_data = std::fs::read(lib_path).map_err(|e| format!("failed to read '{}': {}", lib_path, e))?;
                    if lib_data.len() >= 8 && &lib_data[0..8] == b"!<arch>\n" {
                        // Archive files in linker scripts: extract any needed symbols
                        load_archive_for_dynamic(&lib_data, lib_path, globals)?;
                    } else {
                        load_shared_library(lib_path, globals, needed_sonames)?;
                    }
                }
                return Ok(());
            }
        }
        return Err(format!("{}: not a valid ELF shared library", path));
    }

    let soname = parse_soname(&data).unwrap_or_else(|| {
        Path::new(path).file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_else(|| path.to_string())
    });
    if !needed_sonames.contains(&soname) { needed_sonames.push(soname.clone()); }

    let dyn_syms = parse_shared_library_symbols(&data, path)?;
    for dsym in &dyn_syms {
        if let Some(existing) = globals.get(&dsym.name) {
            if existing.defined_in.is_none() && !existing.is_dynamic {
                globals.insert(dsym.name.clone(), GlobalSymbol {
                    value: 0, size: dsym.size, info: dsym.info,
                    defined_in: None, from_lib: Some(soname.clone()),
                    plt_idx: None, got_idx: None,
                    section_idx: SHN_UNDEF, is_dynamic: true, copy_reloc: false,
                });
            }
        }
    }
    Ok(())
}

fn load_archive_for_dynamic(
    data: &[u8], _archive_path: &str, globals: &mut HashMap<String, GlobalSymbol>,
) -> Result<(), String> {
    // Archives in linker scripts (like libc_nonshared.a) are silently ignored
    // when loaded from the shared library path. They provide static overrides
    // that are only relevant when we have objects needing them. Since we handle
    // archives properly when load_file encounters them, this is a no-op.
    let _ = data;
    let _ = globals;
    Ok(())
}

fn resolve_dynamic_symbols(
    globals: &mut HashMap<String, GlobalSymbol>, needed_sonames: &mut Vec<String>,
) -> Result<(), String> {
    // Linker-defined symbols that will be resolved during layout
    let linker_defined = [
        "_GLOBAL_OFFSET_TABLE_", "__bss_start", "_edata", "_end", "__end",
        "__ehdr_start", "__executable_start", "_etext", "etext",
        "__dso_handle", "_DYNAMIC", "__data_start", "data_start",
        "__init_array_start", "__init_array_end",
        "__fini_array_start", "__fini_array_end",
        "__preinit_array_start", "__preinit_array_end",
        "__rela_iplt_start", "__rela_iplt_end",
    ];
    let undefined: Vec<String> = globals.iter()
        .filter(|(name, sym)| sym.defined_in.is_none() && !sym.is_dynamic &&
            !linker_defined.contains(&name.as_str()))
        .map(|(name, _)| name.clone()).collect();
    if undefined.is_empty() { return Ok(()); }

    let libs = [
        "/lib/x86_64-linux-gnu/libc.so.6",
        "/lib/x86_64-linux-gnu/libm.so.6",
        "/lib/x86_64-linux-gnu/libgcc_s.so.1",
    ];

    for lib_path in &libs {
        if !Path::new(lib_path).exists() { continue; }
        let data = match std::fs::read(lib_path) { Ok(d) => d, Err(_) => continue };
        let soname = parse_soname(&data).unwrap_or_else(|| {
            Path::new(lib_path).file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_default()
        });
        let dyn_syms = match parse_shared_library_symbols(&data, lib_path) { Ok(s) => s, Err(_) => continue };

        let mut lib_needed = false;
        for dsym in &dyn_syms {
            if let Some(existing) = globals.get(&dsym.name) {
                if existing.defined_in.is_none() && !existing.is_dynamic {
                    lib_needed = true;
                    globals.insert(dsym.name.clone(), GlobalSymbol {
                        value: 0, size: dsym.size, info: dsym.info,
                        defined_in: None, from_lib: Some(soname.clone()),
                        plt_idx: None, got_idx: None,
                        section_idx: SHN_UNDEF, is_dynamic: true, copy_reloc: false,
                    });
                }
            }
        }
        if lib_needed && !needed_sonames.contains(&soname) { needed_sonames.push(soname); }
    }
    Ok(())
}

fn merge_sections(
    objects: &[ElfObject], output_sections: &mut Vec<OutputSection>,
    section_map: &mut HashMap<(usize, usize), (usize, u64)>,
) {
    let mut output_map: HashMap<String, usize> = HashMap::new();

    for obj_idx in 0..objects.len() {
        for sec_idx in 0..objects[obj_idx].sections.len() {
            let sec = &objects[obj_idx].sections[sec_idx];
            if sec.flags & SHF_ALLOC == 0 { continue; }
            if matches!(sec.sh_type, SHT_NULL | SHT_STRTAB | SHT_SYMTAB | SHT_RELA | SHT_REL | SHT_GROUP) { continue; }
            if sec.flags & SHF_EXCLUDE != 0 { continue; }
            if sec.sh_type == SHT_PROGBITS && sec.size == 0 { continue; }

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

fn allocate_common_symbols(globals: &mut HashMap<String, GlobalSymbol>, output_sections: &mut Vec<OutputSection>) {
    let common_syms: Vec<(String, u64, u64)> = globals.iter()
        .filter(|(_, sym)| sym.section_idx == SHN_COMMON && sym.defined_in.is_some())
        .map(|(name, sym)| (name.clone(), sym.value.max(1), sym.size)).collect();
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
            sym.section_idx = 0xffff;
        }
        if *alignment > output_sections[bss_idx].alignment {
            output_sections[bss_idx].alignment = *alignment;
        }
        bss_off += size;
    }
    output_sections[bss_idx].mem_size = bss_off;
}

fn create_plt_got(
    objects: &[ElfObject], globals: &mut HashMap<String, GlobalSymbol>,
) -> (Vec<String>, Vec<(String, bool)>) {
    let mut plt_names: Vec<String> = Vec::new();
    let mut got_only_names: Vec<String> = Vec::new();
    let mut copy_reloc_names: Vec<String> = Vec::new();

    for obj in objects {
        for sec_idx in 0..obj.sections.len() {
            for rela in &obj.relocations[sec_idx] {
                let si = rela.sym_idx as usize;
                if si >= obj.symbols.len() { continue; }
                let sym = &obj.symbols[si];
                if sym.name.is_empty() { continue; }
                let gsym_info = globals.get(&sym.name).map(|g| (g.is_dynamic, g.info & 0xf));

                match rela.rela_type {
                    R_X86_64_PLT32 | R_X86_64_PC32 if gsym_info.map(|g| g.0).unwrap_or(false) => {
                        let sym_type = gsym_info.map(|g| g.1).unwrap_or(0);
                        if sym_type == STT_OBJECT {
                            // Dynamic data symbol - needs copy relocation
                            if !copy_reloc_names.contains(&sym.name) {
                                copy_reloc_names.push(sym.name.clone());
                            }
                        } else {
                            // Dynamic function symbol - needs PLT
                            if !plt_names.contains(&sym.name) { plt_names.push(sym.name.clone()); }
                        }
                    }
                    R_X86_64_GOTPCREL | R_X86_64_GOTPCRELX | R_X86_64_REX_GOTPCRELX
                    | R_X86_64_GOTTPOFF => {
                        if !got_only_names.contains(&sym.name) && !plt_names.contains(&sym.name) {
                            got_only_names.push(sym.name.clone());
                        }
                    }
                    _ if gsym_info.map(|g| g.0).unwrap_or(false) => {
                        let sym_type = gsym_info.map(|g| g.1).unwrap_or(0);
                        if sym_type != STT_OBJECT && rela.rela_type == R_X86_64_64 {
                            // R_X86_64_64 for dynamic function (e.g. function pointer init) needs PLT
                            if !plt_names.contains(&sym.name) { plt_names.push(sym.name.clone()); }
                        } else if !plt_names.contains(&sym.name) && !got_only_names.contains(&sym.name) {
                            got_only_names.push(sym.name.clone());
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    // Mark copy relocation symbols
    for name in &copy_reloc_names {
        if let Some(gsym) = globals.get_mut(name) {
            gsym.copy_reloc = true;
        }
    }

    let mut got_entries: Vec<(String, bool)> = Vec::new();
    got_entries.push((String::new(), false)); // GOT[0]
    got_entries.push((String::new(), false)); // GOT[1]
    got_entries.push((String::new(), false)); // GOT[2]

    for (plt_idx, name) in plt_names.iter().enumerate() {
        let got_idx = got_entries.len();
        got_entries.push((name.clone(), true));
        if let Some(gsym) = globals.get_mut(name) {
            gsym.plt_idx = Some(plt_idx);
            gsym.got_idx = Some(got_idx);
        }
    }

    for name in &got_only_names {
        let got_idx = got_entries.len();
        got_entries.push((name.clone(), false));
        if let Some(gsym) = globals.get_mut(name) {
            gsym.got_idx = Some(got_idx);
        }
    }

    (plt_names, got_entries)
}

#[allow(clippy::too_many_arguments)]
fn emit_executable(
    objects: &[ElfObject], globals: &mut HashMap<String, GlobalSymbol>,
    output_sections: &mut [OutputSection],
    section_map: &HashMap<(usize, usize), (usize, u64)>,
    plt_names: &[String], got_entries: &[(String, bool)],
    needed_sonames: &[String], output_path: &str,
) -> Result<(), String> {
    let mut dynstr = DynStrTab::new();
    for lib in needed_sonames { dynstr.add(lib); }

    // Build dyn_sym_names in two parts:
    // 1. Non-hashed symbols (PLT imports, GLOB_DAT imports) - these are undefined
    // 2. Hashed symbols (copy-reloc symbols) - these are defined and must be
    //    findable through .gnu.hash so the dynamic linker can redirect references
    let mut dyn_sym_names: Vec<String> = Vec::new();
    for name in plt_names {
        if !dyn_sym_names.contains(name) { dyn_sym_names.push(name.clone()); }
    }
    for (name, is_plt) in got_entries {
        if !name.is_empty() && !*is_plt && !dyn_sym_names.contains(name) {
            if let Some(gsym) = globals.get(name) {
                if gsym.is_dynamic && !gsym.copy_reloc {
                    dyn_sym_names.push(name.clone());
                }
            }
        }
    }
    // symoffset = index of first hashed symbol (1-indexed: null symbol is index 0)
    let gnu_hash_symoffset = 1 + dyn_sym_names.len(); // +1 for null entry

    // Collect copy relocation symbols - these go AFTER non-hashed symbols
    // and are the only symbols included in the .gnu.hash table
    let copy_reloc_syms: Vec<(String, u64)> = globals.iter()
        .filter(|(_, g)| g.copy_reloc)
        .map(|(n, g)| (n.clone(), g.size))
        .collect();
    for (name, _) in &copy_reloc_syms {
        if !dyn_sym_names.contains(name) {
            dyn_sym_names.push(name.clone());
        }
    }

    for name in &dyn_sym_names { dynstr.add(name); }

    let dynsym_count = 1 + dyn_sym_names.len();
    let dynsym_size = dynsym_count as u64 * 24;
    let dynstr_size = dynstr.data.len() as u64;
    let rela_plt_size = plt_names.len() as u64 * 24;
    let rela_dyn_glob_count = got_entries.iter().filter(|(n, p)| {
        !n.is_empty() && !*p && globals.get(n).map(|g| g.is_dynamic && !g.copy_reloc).unwrap_or(false)
    }).count();
    let rela_dyn_count = rela_dyn_glob_count + copy_reloc_syms.len();
    let rela_dyn_size = rela_dyn_count as u64 * 24;

    // Build .gnu.hash table for copy-reloc symbols
    // GNU hash function
    fn gnu_hash(name: &[u8]) -> u32 {
        let mut h: u32 = 5381;
        for &b in name {
            h = h.wrapping_mul(33).wrapping_add(b as u32);
        }
        h
    }

    let num_hashed = copy_reloc_syms.len();
    let gnu_hash_nbuckets = if num_hashed == 0 { 1 } else { num_hashed.next_power_of_two().max(1) } as u32;
    let gnu_hash_bloom_size: u32 = 1;
    let gnu_hash_bloom_shift: u32 = 6;

    // Compute hashes for hashed symbols
    let hashed_sym_hashes: Vec<u32> = dyn_sym_names[gnu_hash_symoffset - 1..]
        .iter()
        .map(|name| gnu_hash(name.as_bytes()))
        .collect();

    // Build bloom filter (single 64-bit word)
    let mut bloom_word: u64 = 0;
    for &h in &hashed_sym_hashes {
        let bit1 = h as u64 % 64;
        let bit2 = (h >> gnu_hash_bloom_shift) as u64 % 64;
        bloom_word |= 1u64 << bit1;
        bloom_word |= 1u64 << bit2;
    }

    // Sort hashed symbols by bucket (hash % nbuckets) for proper chain grouping
    // We need to reorder the hashed portion of dyn_sym_names
    if num_hashed > 0 {
        let hashed_start = gnu_hash_symoffset - 1;
        let mut hashed_with_hash: Vec<(String, u32)> = dyn_sym_names[hashed_start..]
            .iter()
            .zip(hashed_sym_hashes.iter())
            .map(|(n, &h)| (n.clone(), h))
            .collect();
        hashed_with_hash.sort_by_key(|(_, h)| h % gnu_hash_nbuckets);
        for (i, (name, _)) in hashed_with_hash.iter().enumerate() {
            dyn_sym_names[hashed_start + i] = name.clone();
        }
    }

    // Recompute hashes after sorting
    let hashed_sym_hashes: Vec<u32> = dyn_sym_names[gnu_hash_symoffset - 1..]
        .iter()
        .map(|name| gnu_hash(name.as_bytes()))
        .collect();

    // Build buckets and chains
    let mut gnu_hash_buckets = vec![0u32; gnu_hash_nbuckets as usize];
    let mut gnu_hash_chains = vec![0u32; num_hashed];
    for (i, &h) in hashed_sym_hashes.iter().enumerate() {
        let bucket = (h % gnu_hash_nbuckets) as usize;
        if gnu_hash_buckets[bucket] == 0 {
            gnu_hash_buckets[bucket] = (gnu_hash_symoffset + i) as u32;
        }
        // Chain value = hash with bit 0 indicating end of chain
        gnu_hash_chains[i] = h & !1; // clear bit 0 (will set later for last in chain)
    }
    // Mark the last symbol in each bucket chain with bit 0 set
    for bucket_idx in 0..gnu_hash_nbuckets as usize {
        if gnu_hash_buckets[bucket_idx] == 0 { continue; }
        let mut last_in_bucket = 0;
        for (i, &h) in hashed_sym_hashes.iter().enumerate() {
            if (h % gnu_hash_nbuckets) as usize == bucket_idx {
                last_in_bucket = i;
            }
        }
        gnu_hash_chains[last_in_bucket] |= 1; // set end-of-chain bit
    }

    // gnu_hash_size = header(16) + bloom(bloom_size*8) + buckets(nbuckets*4) + chains(num_hashed*4)
    let gnu_hash_size: u64 = 16 + (gnu_hash_bloom_size as u64 * 8)
        + (gnu_hash_nbuckets as u64 * 4) + (num_hashed as u64 * 4);
    let plt_size = if plt_names.is_empty() { 0u64 } else { 16 + 16 * plt_names.len() as u64 };
    let got_plt_count = 3 + plt_names.len();
    let got_plt_size = got_plt_count as u64 * 8;
    let got_globdat_count = got_entries.iter().filter(|(n, p)| !n.is_empty() && !*p).count();
    let got_size = got_globdat_count as u64 * 8;

    let has_init_array = output_sections.iter().any(|s| s.name == ".init_array" && s.mem_size > 0);
    let has_fini_array = output_sections.iter().any(|s| s.name == ".fini_array" && s.mem_size > 0);
    let mut dyn_count = needed_sonames.len() as u64 + 14; // fixed entries + NULL
    if has_init_array { dyn_count += 2; }
    if has_fini_array { dyn_count += 2; }
    let dynamic_size = dyn_count * 16;

    let has_tls_sections = output_sections.iter().any(|s| s.flags & SHF_TLS != 0 && s.flags & SHF_ALLOC != 0);
    let phdr_count: u64 = if has_tls_sections { 9 } else { 8 };
    let phdr_total_size = phdr_count * 56;

    // === Layout ===
    let mut offset = 64 + phdr_total_size;
    let interp_offset = offset;
    let interp_addr = BASE_ADDR + offset;
    offset += INTERP.len() as u64;

    offset = (offset + 7) & !7;
    let gnu_hash_offset = offset; let gnu_hash_addr = BASE_ADDR + offset; offset += gnu_hash_size;
    offset = (offset + 7) & !7;
    let dynsym_offset = offset; let dynsym_addr = BASE_ADDR + offset; offset += dynsym_size;
    let dynstr_offset = offset; let dynstr_addr = BASE_ADDR + offset; offset += dynstr_size;
    offset = (offset + 7) & !7;
    let rela_dyn_offset = offset; let rela_dyn_addr = BASE_ADDR + offset; offset += rela_dyn_size;
    offset = (offset + 7) & !7;
    let rela_plt_offset = offset; let rela_plt_addr = BASE_ADDR + offset; offset += rela_plt_size;

    // Text segment
    offset = (offset + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
    let text_page_offset = offset;
    let text_page_addr = BASE_ADDR + offset;
    for sec in output_sections.iter_mut() {
        if sec.flags & SHF_EXECINSTR != 0 && sec.flags & SHF_ALLOC != 0 {
            let a = sec.alignment.max(1);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = BASE_ADDR + offset;
            sec.file_offset = offset;
            offset += sec.mem_size;
        }
    }
    let (plt_addr, plt_offset) = if plt_size > 0 {
        offset = (offset + 15) & !15;
        let a = BASE_ADDR + offset; let o = offset; offset += plt_size; (a, o)
    } else { (0u64, 0u64) };
    let text_total_size = offset - text_page_offset;

    // Rodata segment
    offset = (offset + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
    let rodata_page_offset = offset;
    let rodata_page_addr = BASE_ADDR + offset;
    for sec in output_sections.iter_mut() {
        if sec.flags & SHF_ALLOC != 0 && sec.flags & SHF_EXECINSTR == 0 &&
           sec.flags & SHF_WRITE == 0 && sec.sh_type != SHT_NOBITS {
            let a = sec.alignment.max(1);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = BASE_ADDR + offset;
            sec.file_offset = offset;
            offset += sec.mem_size;
        }
    }
    let rodata_total_size = offset - rodata_page_offset;

    // RW segment
    offset = (offset + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
    let rw_page_offset = offset;
    let rw_page_addr = BASE_ADDR + offset;

    let mut init_array_addr = 0u64; let mut init_array_size = 0u64;
    let mut fini_array_addr = 0u64; let mut fini_array_size = 0u64;

    for sec in output_sections.iter_mut() {
        if sec.name == ".init_array" {
            let a = sec.alignment.max(8);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = BASE_ADDR + offset; sec.file_offset = offset;
            init_array_addr = sec.addr; init_array_size = sec.mem_size;
            offset += sec.mem_size; break;
        }
    }
    for sec in output_sections.iter_mut() {
        if sec.name == ".fini_array" {
            let a = sec.alignment.max(8);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = BASE_ADDR + offset; sec.file_offset = offset;
            fini_array_addr = sec.addr; fini_array_size = sec.mem_size;
            offset += sec.mem_size; break;
        }
    }

    offset = (offset + 7) & !7;
    let dynamic_offset = offset; let dynamic_addr = BASE_ADDR + offset; offset += dynamic_size;
    offset = (offset + 7) & !7;
    let got_offset = offset; let got_addr = BASE_ADDR + offset; offset += got_size;
    offset = (offset + 7) & !7;
    let got_plt_offset = offset; let got_plt_addr = BASE_ADDR + offset; offset += got_plt_size;

    for sec in output_sections.iter_mut() {
        if sec.flags & SHF_ALLOC != 0 && sec.flags & SHF_WRITE != 0 &&
           sec.sh_type != SHT_NOBITS && sec.name != ".init_array" && sec.name != ".fini_array" &&
           sec.flags & SHF_TLS == 0 {
            let a = sec.alignment.max(1);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = BASE_ADDR + offset; sec.file_offset = offset;
            offset += sec.mem_size;
        }
    }

    // TLS sections (.tdata, .tbss) - place in RW segment, track for PT_TLS
    let mut tls_addr = 0u64;
    let mut tls_file_offset = 0u64;
    let mut tls_file_size = 0u64;
    let mut tls_mem_size = 0u64;
    let mut tls_align = 1u64;
    for sec in output_sections.iter_mut() {
        if sec.flags & SHF_TLS != 0 && sec.flags & SHF_ALLOC != 0 && sec.sh_type != SHT_NOBITS {
            let a = sec.alignment.max(1);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = BASE_ADDR + offset; sec.file_offset = offset;
            if tls_addr == 0 { tls_addr = sec.addr; tls_file_offset = offset; tls_align = a; }
            tls_file_size += sec.mem_size;
            tls_mem_size += sec.mem_size;
            offset += sec.mem_size;
        }
    }
    for sec in output_sections.iter_mut() {
        if sec.flags & SHF_TLS != 0 && sec.sh_type == SHT_NOBITS {
            let a = sec.alignment.max(1);
            let aligned = (tls_mem_size + a - 1) & !(a - 1);
            sec.addr = tls_addr + aligned; sec.file_offset = offset;
            tls_mem_size = aligned + sec.mem_size;
            if a > tls_align { tls_align = a; }
        }
    }
    // Align TLS size to TLS alignment
    tls_mem_size = (tls_mem_size + tls_align - 1) & !(tls_align - 1);
    let has_tls = tls_addr != 0;

    let bss_addr = BASE_ADDR + offset;
    let mut bss_size = 0u64;
    for sec in output_sections.iter_mut() {
        if sec.sh_type == SHT_NOBITS && sec.flags & SHF_ALLOC != 0 && sec.flags & SHF_TLS == 0 {
            let a = sec.alignment.max(1);
            let aligned = (bss_addr + bss_size + a - 1) & !(a - 1);
            bss_size = aligned - bss_addr + sec.mem_size;
            sec.addr = aligned; sec.file_offset = offset;
        }
    }

    // Allocate BSS space for copy-relocated symbols
    for (name, size) in &copy_reloc_syms {
        let aligned = (bss_addr + bss_size + 7) & !7; // 8-byte align
        bss_size = aligned - bss_addr + size;
        if let Some(gsym) = globals.get_mut(name) {
            gsym.value = aligned;
            gsym.defined_in = Some(usize::MAX); // sentinel: defined via copy reloc
        }
    }

    // Merge section data
    for sec in output_sections.iter_mut() {
        if sec.sh_type == SHT_NOBITS { continue; }
        let mut data = vec![0u8; sec.mem_size as usize];
        for input in &sec.inputs {
            let sd = &objects[input.object_idx].section_data[input.section_idx];
            let s = input.output_offset as usize;
            let e = s + sd.len();
            if e <= data.len() && !sd.is_empty() { data[s..e].copy_from_slice(sd); }
        }
        sec.data = data;
    }

    // Update global symbol addresses
    for (_, gsym) in globals.iter_mut() {
        if let Some(obj_idx) = gsym.defined_in {
            if gsym.section_idx == SHN_COMMON || gsym.section_idx == 0xffff {
                if let Some(bss_sec) = output_sections.iter().find(|s| s.name == ".bss") {
                    gsym.value += bss_sec.addr;
                }
            } else if gsym.section_idx != SHN_UNDEF && gsym.section_idx != SHN_ABS {
                let si = gsym.section_idx as usize;
                if let Some(&(oi, so)) = section_map.get(&(obj_idx, si)) {
                    gsym.value += output_sections[oi].addr + so;
                }
            }
        }
    }

    // Define linker-provided symbols (consistent with i686/ARM/RISC-V backends)
    let text_seg_end = text_page_addr + text_total_size;
    let data_seg_start = rw_page_addr;
    let data_seg_end = bss_addr + bss_size;
    let define_linker_sym = |globals: &mut HashMap<String, GlobalSymbol>, name: &str, value: u64| {
        let entry = globals.entry(name.to_string()).or_insert(GlobalSymbol {
            value: 0, size: 0, info: (STB_GLOBAL << 4),
            defined_in: None, from_lib: None, plt_idx: None, got_idx: None,
            section_idx: SHN_ABS, is_dynamic: false, copy_reloc: false,
        });
        if entry.defined_in.is_none() && !entry.is_dynamic {
            entry.value = value;
            entry.defined_in = Some(usize::MAX); // sentinel: linker-defined
            entry.section_idx = SHN_ABS;
        }
    };
    define_linker_sym(globals, "_GLOBAL_OFFSET_TABLE_", got_plt_addr);
    define_linker_sym(globals, "__bss_start", bss_addr);
    define_linker_sym(globals, "_edata", bss_addr);
    define_linker_sym(globals, "_end", data_seg_end);
    define_linker_sym(globals, "__end", data_seg_end);
    define_linker_sym(globals, "__ehdr_start", BASE_ADDR);
    define_linker_sym(globals, "__executable_start", BASE_ADDR);
    define_linker_sym(globals, "_etext", text_seg_end);
    define_linker_sym(globals, "etext", text_seg_end);
    define_linker_sym(globals, "__dso_handle", data_seg_start);
    define_linker_sym(globals, "_DYNAMIC", dynamic_addr);
    define_linker_sym(globals, "__data_start", data_seg_start);
    define_linker_sym(globals, "data_start", data_seg_start);
    define_linker_sym(globals, "__init_array_start", init_array_addr);
    define_linker_sym(globals, "__init_array_end", init_array_addr + init_array_size);
    define_linker_sym(globals, "__fini_array_start", fini_array_addr);
    define_linker_sym(globals, "__fini_array_end", fini_array_addr + fini_array_size);
    define_linker_sym(globals, "__preinit_array_start", 0);
    define_linker_sym(globals, "__preinit_array_end", 0);
    define_linker_sym(globals, "__rela_iplt_start", 0);
    define_linker_sym(globals, "__rela_iplt_end", 0);

    let entry_addr = globals.get("_start").map(|s| s.value).unwrap_or(text_page_addr);

    // === Build output buffer ===
    let file_size = offset as usize;
    let mut out = vec![0u8; file_size];

    // ELF header
    out[0..4].copy_from_slice(&ELF_MAGIC);
    out[4] = ELFCLASS64; out[5] = ELFDATA2LSB; out[6] = 1;
    w16(&mut out, 16, ET_EXEC); w16(&mut out, 18, EM_X86_64); w32(&mut out, 20, 1);
    w64(&mut out, 24, entry_addr); w64(&mut out, 32, 64); w64(&mut out, 40, 0);
    w32(&mut out, 48, 0); w16(&mut out, 52, 64); w16(&mut out, 54, 56);
    w16(&mut out, 56, phdr_count as u16); w16(&mut out, 58, 64); w16(&mut out, 60, 0); w16(&mut out, 62, 0);

    // Program headers
    let mut ph = 64usize;
    wphdr(&mut out, ph, PT_PHDR, PF_R, 64, BASE_ADDR+64, phdr_total_size, phdr_total_size, 8); ph += 56;
    wphdr(&mut out, ph, PT_INTERP, PF_R, interp_offset, interp_addr, INTERP.len() as u64, INTERP.len() as u64, 1); ph += 56;
    let ro_seg_end = rela_plt_offset + rela_plt_size;
    wphdr(&mut out, ph, PT_LOAD, PF_R, 0, BASE_ADDR, ro_seg_end, ro_seg_end, PAGE_SIZE); ph += 56;
    wphdr(&mut out, ph, PT_LOAD, PF_R|PF_X, text_page_offset, text_page_addr, text_total_size, text_total_size, PAGE_SIZE); ph += 56;
    wphdr(&mut out, ph, PT_LOAD, PF_R, rodata_page_offset, rodata_page_addr, rodata_total_size, rodata_total_size, PAGE_SIZE); ph += 56;
    let rw_filesz = offset - rw_page_offset;
    let rw_memsz = if bss_size > 0 { (bss_addr + bss_size) - rw_page_addr } else { rw_filesz };
    wphdr(&mut out, ph, PT_LOAD, PF_R|PF_W, rw_page_offset, rw_page_addr, rw_filesz, rw_memsz, PAGE_SIZE); ph += 56;
    wphdr(&mut out, ph, PT_DYNAMIC, PF_R|PF_W, dynamic_offset, dynamic_addr, dynamic_size, dynamic_size, 8); ph += 56;
    wphdr(&mut out, ph, PT_GNU_STACK, PF_R|PF_W, 0, 0, 0, 0, 0x10); ph += 56;
    if has_tls {
        wphdr(&mut out, ph, PT_TLS, PF_R, tls_file_offset, tls_addr, tls_file_size, tls_mem_size, tls_align);
    }

    // .interp
    write_bytes(&mut out, interp_offset as usize, INTERP);

    // .gnu.hash - proper hash table so dynamic linker can find copy-reloc symbols
    let gh = gnu_hash_offset as usize;
    w32(&mut out, gh, gnu_hash_nbuckets);
    w32(&mut out, gh+4, gnu_hash_symoffset as u32);
    w32(&mut out, gh+8, gnu_hash_bloom_size);
    w32(&mut out, gh+12, gnu_hash_bloom_shift);
    // Bloom filter
    let bloom_off = gh + 16;
    w64(&mut out, bloom_off, bloom_word);
    // Buckets
    let buckets_off = bloom_off + (gnu_hash_bloom_size as usize * 8);
    for (i, &b) in gnu_hash_buckets.iter().enumerate() {
        w32(&mut out, buckets_off + i * 4, b);
    }
    // Chains
    let chains_off = buckets_off + (gnu_hash_nbuckets as usize * 4);
    for (i, &c) in gnu_hash_chains.iter().enumerate() {
        w32(&mut out, chains_off + i * 4, c);
    }

    // .dynsym
    let mut ds = dynsym_offset as usize + 24; // skip null entry
    for name in &dyn_sym_names {
        let no = dynstr.get_offset(name) as u32;
        w32(&mut out, ds, no);
        if let Some(gsym) = globals.get(name) {
            if gsym.copy_reloc {
                // Copy-relocated data symbol: st_shndx MUST be non-zero so the
                // dynamic linker treats this as a defined symbol and redirects
                // all references (including glibc-internal ones) to our BSS copy.
                if ds+5 < out.len() { out[ds+4] = (STB_GLOBAL << 4) | STT_OBJECT; out[ds+5] = 0; }
                w16(&mut out, ds+6, 1); // shndx=1 (non-UNDEF: defined in this executable)
                w64(&mut out, ds+8, gsym.value); // st_value = BSS copy address
                w64(&mut out, ds+16, gsym.size); // st_size = actual symbol size
            } else {
                if ds+5 < out.len() { out[ds+4] = (STB_GLOBAL << 4) | STT_FUNC; out[ds+5] = 0; }
                w16(&mut out, ds+6, 0); w64(&mut out, ds+8, 0); w64(&mut out, ds+16, 0);
            }
        } else {
            if ds+5 < out.len() { out[ds+4] = (STB_GLOBAL << 4) | STT_FUNC; out[ds+5] = 0; }
            w16(&mut out, ds+6, 0); w64(&mut out, ds+8, 0); w64(&mut out, ds+16, 0);
        }
        ds += 24;
    }

    // .dynstr
    write_bytes(&mut out, dynstr_offset as usize, &dynstr.data);

    // .rela.dyn (GLOB_DAT for dynamic GOT symbols, R_X86_64_COPY for copy relocs)
    let mut rd = rela_dyn_offset as usize;
    let mut gd_a = got_addr;
    for (name, is_plt) in got_entries {
        if name.is_empty() || *is_plt { continue; }
        let is_dynamic = globals.get(name).map(|g| g.is_dynamic && !g.copy_reloc).unwrap_or(false);
        if is_dynamic {
            let si = dyn_sym_names.iter().position(|n| n == name).map(|i| i+1).unwrap_or(0) as u64;
            w64(&mut out, rd, gd_a); w64(&mut out, rd+8, (si << 32) | 6); w64(&mut out, rd+16, 0);
            rd += 24;
        }
        gd_a += 8;
    }
    // R_X86_64_COPY relocations for copy-relocated symbols
    for (name, _) in &copy_reloc_syms {
        if let Some(gsym) = globals.get(name) {
            let si = dyn_sym_names.iter().position(|n| n == name).map(|i| i+1).unwrap_or(0) as u64;
            let copy_addr = gsym.value;
            w64(&mut out, rd, copy_addr); w64(&mut out, rd+8, (si << 32) | 5); w64(&mut out, rd+16, 0);
            rd += 24;
        }
    }

    // .rela.plt
    let mut rp = rela_plt_offset as usize;
    let gpb = got_plt_addr + 24;
    for (i, name) in plt_names.iter().enumerate() {
        let gea = gpb + i as u64 * 8;
        let si = dyn_sym_names.iter().position(|n| n == name).map(|j| j+1).unwrap_or(0) as u64;
        w64(&mut out, rp, gea); w64(&mut out, rp+8, (si << 32) | 7); w64(&mut out, rp+16, 0);
        rp += 24;
    }

    // Section data
    for sec in output_sections.iter() {
        if sec.sh_type == SHT_NOBITS || sec.data.is_empty() { continue; }
        write_bytes(&mut out, sec.file_offset as usize, &sec.data);
    }

    // .plt
    if plt_size > 0 {
        let po = plt_offset as usize;
        out[po] = 0xff; out[po+1] = 0x35;
        w32(&mut out, po+2, ((got_plt_addr+8) as i64 - (plt_addr+6) as i64) as u32);
        out[po+6] = 0xff; out[po+7] = 0x25;
        w32(&mut out, po+8, ((got_plt_addr+16) as i64 - (plt_addr+12) as i64) as u32);
        for i in 12..16 { out[po+i] = 0x90; }

        for (i, _) in plt_names.iter().enumerate() {
            let ep = po + 16 + i * 16;
            let pea = plt_addr + 16 + i as u64 * 16;
            let gea = got_plt_addr + 24 + i as u64 * 8;
            out[ep] = 0xff; out[ep+1] = 0x25;
            w32(&mut out, ep+2, (gea as i64 - (pea+6) as i64) as u32);
            out[ep+6] = 0x68; w32(&mut out, ep+7, i as u32);
            out[ep+11] = 0xe9; w32(&mut out, ep+12, (plt_addr as i64 - (pea+16) as i64) as u32);
        }
    }

    // .dynamic
    let mut dd = dynamic_offset as usize;
    for lib in needed_sonames {
        let so = dynstr.get_offset(lib);
        w64(&mut out, dd, DT_NEEDED as u64); w64(&mut out, dd+8, so as u64); dd += 16;
    }
    for &(tag, val) in &[
        (DT_STRTAB, dynstr_addr), (DT_SYMTAB, dynsym_addr), (DT_STRSZ, dynstr_size),
        (DT_SYMENT, 24), (DT_DEBUG, 0), (DT_PLTGOT, got_plt_addr),
        (DT_PLTRELSZ, rela_plt_size), (DT_PLTREL, 7u64), (DT_JMPREL, rela_plt_addr),
        (DT_RELA, rela_dyn_addr), (DT_RELASZ, rela_dyn_size), (DT_RELAENT, 24),
        (DT_GNU_HASH, gnu_hash_addr),
    ] {
        w64(&mut out, dd, tag as u64); w64(&mut out, dd+8, val); dd += 16;
    }
    if has_init_array {
        w64(&mut out, dd, DT_INIT_ARRAY as u64); w64(&mut out, dd+8, init_array_addr); dd += 16;
        w64(&mut out, dd, DT_INIT_ARRAYSZ as u64); w64(&mut out, dd+8, init_array_size); dd += 16;
    }
    if has_fini_array {
        w64(&mut out, dd, DT_FINI_ARRAY as u64); w64(&mut out, dd+8, fini_array_addr); dd += 16;
        w64(&mut out, dd, DT_FINI_ARRAYSZ as u64); w64(&mut out, dd+8, fini_array_size); dd += 16;
    }
    w64(&mut out, dd, DT_NULL as u64); w64(&mut out, dd+8, 0);

    // .got
    let mut go = got_offset as usize;
    for (name, is_plt) in got_entries {
        if name.is_empty() || *is_plt { continue; }
        if let Some(gsym) = globals.get(name) {
            if gsym.defined_in.is_some() && !gsym.is_dynamic {
                let sym_val = gsym.value;
                if has_tls && (gsym.info & 0xf) == STT_TLS {
                    // TLS GOT entry: store the TPOFF value
                    let tpoff = (sym_val as i64 - tls_addr as i64) - tls_mem_size as i64;
                    w64(&mut out, go, tpoff as u64);
                } else {
                    w64(&mut out, go, sym_val);
                }
            }
        }
        go += 8;
    }

    // .got.plt
    let gp = got_plt_offset as usize;
    w64(&mut out, gp, dynamic_addr);
    w64(&mut out, gp+8, 0); w64(&mut out, gp+16, 0);
    for (i, _) in plt_names.iter().enumerate() {
        w64(&mut out, gp+24+i*8, plt_addr + 16 + i as u64 * 16 + 6);
    }

    // === Apply relocations ===
    // Snapshot globals to avoid borrow issues
    let globals_snap: HashMap<String, GlobalSymbol> = globals.clone();

    for obj_idx in 0..objects.len() {
        for sec_idx in 0..objects[obj_idx].sections.len() {
            let relas = &objects[obj_idx].relocations[sec_idx];
            if relas.is_empty() { continue; }
            let (out_idx, sec_off) = match section_map.get(&(obj_idx, sec_idx)) {
                Some(&v) => v, None => continue,
            };
            let sa = output_sections[out_idx].addr;
            let sfo = output_sections[out_idx].file_offset;

            for rela in relas {
                let si = rela.sym_idx as usize;
                if si >= objects[obj_idx].symbols.len() { continue; }
                let sym = &objects[obj_idx].symbols[si];
                let p = sa + sec_off + rela.offset;
                let fp = (sfo + sec_off + rela.offset) as usize;
                let a = rela.addend;
                let s = resolve_sym(obj_idx, sym, &globals_snap, section_map, output_sections,
                                    plt_addr);

                match rela.rela_type {
                    R_X86_64_64 => {
                        let t = if !sym.name.is_empty() {
                            if let Some(g) = globals_snap.get(&sym.name) {
                                if g.is_dynamic && !g.copy_reloc {
                                    if let Some(pi) = g.plt_idx { plt_addr + 16 + pi as u64 * 16 } else { s }
                                } else { s }
                            } else { s }
                        } else { s };
                        w64(&mut out, fp, (t as i64 + a) as u64);
                    }
                    R_X86_64_PC32 | R_X86_64_PLT32 => {
                        let t = if !sym.name.is_empty() {
                            if let Some(g) = globals_snap.get(&sym.name) {
                                if let Some(pi) = g.plt_idx { plt_addr + 16 + pi as u64 * 16 } else { s }
                            } else { s }
                        } else { s };
                        w32(&mut out, fp, (t as i64 + a - p as i64) as u32);
                    }
                    R_X86_64_32 => { w32(&mut out, fp, (s as i64 + a) as u32); }
                    R_X86_64_32S => { w32(&mut out, fp, (s as i64 + a) as u32); }
                    R_X86_64_GOTTPOFF => {
                        // Initial Exec TLS via GOT: GOT entry contains TPOFF value
                        let mut resolved = false;
                        if !sym.name.is_empty() {
                            if let Some(g) = globals_snap.get(&sym.name) {
                                if let Some(gi) = g.got_idx {
                                    let entry = &got_entries[gi];
                                    let gea = if entry.1 {
                                        got_plt_addr + 24 + g.plt_idx.unwrap_or(0) as u64 * 8
                                    } else {
                                        let nb = got_entries[..gi].iter().filter(|(n,p)| !n.is_empty() && !*p).count();
                                        got_addr + nb as u64 * 8
                                    };
                                    w32(&mut out, fp, (gea as i64 + a - p as i64) as u32);
                                    resolved = true;
                                }
                            }
                        }
                        if !resolved {
                            // IE-to-LE relaxation: convert GOT-indirect to immediate TPOFF.
                            // Transform: movq GOT(%rip), %reg -> movq $tpoff, %reg
                            // Encoding: 48 8b XX YY YY YY YY -> 48 c7 CX YY YY YY YY
                            //   where XX encodes the register via ModR/M
                            let tpoff = (s as i64 - tls_addr as i64) - tls_mem_size as i64;
                            if fp >= 2 && fp + 4 <= out.len() && out[fp-2] == 0x8b {
                                // Get the register from ModR/M byte
                                let modrm = out[fp-1];
                                let reg = (modrm >> 3) & 7;
                                // Change mov r/m64,reg to mov $imm32,reg (opcode 0xc7, /0)
                                out[fp-2] = 0xc7;
                                out[fp-1] = 0xc0 | reg;
                                w32(&mut out, fp, (tpoff + a) as u32);
                            } else {
                                return Err(format!(
                                    "GOTTPOFF IE-to-LE relaxation failed: unrecognized instruction pattern at offset 0x{:x} for symbol '{}' (expected movq GOT(%rip), %reg)",
                                    fp, sym.name
                                ));
                            }
                        }
                    }
                    R_X86_64_GOTPCREL | R_X86_64_GOTPCRELX | R_X86_64_REX_GOTPCRELX => {
                        if !sym.name.is_empty() {
                            if let Some(g) = globals_snap.get(&sym.name) {
                                if let Some(gi) = g.got_idx {
                                    let entry = &got_entries[gi];
                                    let gea = if entry.1 {
                                        got_plt_addr + 24 + g.plt_idx.unwrap_or(0) as u64 * 8
                                    } else {
                                        let nb = got_entries[..gi].iter().filter(|(n,p)| !n.is_empty() && !*p).count();
                                        got_addr + nb as u64 * 8
                                    };
                                    w32(&mut out, fp, (gea as i64 + a - p as i64) as u32);
                                    continue;
                                }
                                if (rela.rela_type == R_X86_64_GOTPCRELX || rela.rela_type == R_X86_64_REX_GOTPCRELX)
                                   && g.defined_in.is_some() {
                                    if fp >= 2 && fp < out.len() && out[fp-2] == 0x8b {
                                        out[fp-2] = 0x8d;
                                    }
                                    w32(&mut out, fp, (s as i64 + a - p as i64) as u32);
                                    continue;
                                }
                            }
                        }
                        w32(&mut out, fp, (s as i64 + a - p as i64) as u32);
                    }
                    R_X86_64_PC64 => { w64(&mut out, fp, (s as i64 + a - p as i64) as u64); }
                    R_X86_64_TPOFF32 => {
                        // Initial Exec TLS: value = (sym_addr - tls_addr) - tls_mem_size
                        // %fs:0 points past end of TLS block on x86-64
                        let tpoff = (s as i64 - tls_addr as i64) - tls_mem_size as i64;
                        w32(&mut out, fp, (tpoff + a) as u32);
                    }
                    R_X86_64_NONE => {}
                    other => {
                        return Err(format!(
                            "unsupported x86-64 relocation type {} for '{}' in {}",
                            other, sym.name, objects[obj_idx].source_name
                        ));
                    }
                }
            }
        }
    }

    std::fs::write(output_path, &out).map_err(|e| format!("failed to write '{}': {}", output_path, e))?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let _ = std::fs::set_permissions(output_path, std::fs::Permissions::from_mode(0o755));
    }
    Ok(())
}

fn resolve_sym(
    obj_idx: usize, sym: &Symbol, globals: &HashMap<String, GlobalSymbol>,
    section_map: &HashMap<(usize, usize), (usize, u64)>, output_sections: &[OutputSection],
    plt_addr: u64,
) -> u64 {
    if sym.sym_type() == STT_SECTION {
        let si = sym.shndx as usize;
        return section_map.get(&(obj_idx, si)).map(|&(oi, so)| output_sections[oi].addr + so).unwrap_or(0);
    }
    if !sym.name.is_empty() {
        if let Some(g) = globals.get(&sym.name) {
            if g.defined_in.is_some() { return g.value; }
            if g.is_dynamic {
                return g.plt_idx.map(|pi| plt_addr + 16 + pi as u64 * 16).unwrap_or(0);
            }
        }
        if sym.is_weak() { return 0; }
    }
    if sym.is_undefined() { return 0; }
    if sym.shndx == SHN_ABS { return sym.value; }
    section_map.get(&(obj_idx, sym.shndx as usize))
        .map(|&(oi, so)| output_sections[oi].addr + so + sym.value).unwrap_or(sym.value)
}

fn w16(buf: &mut [u8], off: usize, val: u16) { if off+2 <= buf.len() { buf[off..off+2].copy_from_slice(&val.to_le_bytes()); } }
fn w32(buf: &mut [u8], off: usize, val: u32) { if off+4 <= buf.len() { buf[off..off+4].copy_from_slice(&val.to_le_bytes()); } }
fn w64(buf: &mut [u8], off: usize, val: u64) { if off+8 <= buf.len() { buf[off..off+8].copy_from_slice(&val.to_le_bytes()); } }

fn write_bytes(buf: &mut [u8], off: usize, data: &[u8]) {
    let end = off + data.len();
    if end <= buf.len() { buf[off..end].copy_from_slice(data); }
}

fn wphdr(buf: &mut [u8], off: usize, pt: u32, flags: u32, foff: u64, va: u64, fsz: u64, msz: u64, align: u64) {
    w32(buf, off, pt); w32(buf, off+4, flags);
    w64(buf, off+8, foff); w64(buf, off+16, va); w64(buf, off+24, va);
    w64(buf, off+32, fsz); w64(buf, off+40, msz); w64(buf, off+48, align);
}
