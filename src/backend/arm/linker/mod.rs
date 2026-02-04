//! Native AArch64 ELF64 linker.
//!
//! Links ELF relocatable object files (.o) and static archives (.a) into
//! a dynamically-linked ELF64 executable for AArch64 (ARM 64-bit). Also supports
//! producing shared libraries (ET_DYN) via `link_shared()`.
//!
//! This is the default linker (used when the `gcc_linker` feature is disabled).
//! CRT object discovery and library path resolution are handled by
//! common.rs's `resolve_builtin_link_setup`.

#[allow(dead_code)]
pub mod elf;
pub mod reloc;

use std::collections::HashMap;
use std::path::Path;

use elf::*;
use crate::backend::linker_common;
use linker_common::DynStrTab;

/// Dynamic linker path for AArch64
const INTERP: &[u8] = b"/lib/ld-linux-aarch64.so.1\0";

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
    /// SONAME of the shared library this symbol was resolved from
    pub from_lib: Option<String>,
    /// PLT entry index (for dynamic function symbols)
    pub plt_idx: Option<usize>,
    /// GOT entry index (for dynamic symbols needing GOT slots)
    pub got_idx: Option<usize>,
    /// Whether this symbol is resolved from a shared library
    pub is_dynamic: bool,
    /// Whether this symbol needs a copy relocation
    pub copy_reloc: bool,
    /// Symbol's value in the source shared library (for alias detection)
    pub lib_sym_value: u64,
}

// ── Public entry point ─────────────────────────────────────────────────

/// Link AArch64 object files into an ELF executable (pre-resolved CRT/library variant).
///
/// Supports both static and dynamic linking. When `is_static` is false, shared
/// libraries are loaded and PLT/GOT/.dynamic sections are generated for dynamic
/// symbol references.
#[allow(clippy::too_many_arguments)]
pub fn link_builtin(
    object_files: &[&str],
    output_path: &str,
    user_args: &[String],
    lib_paths: &[&str],
    needed_libs: &[&str],
    crt_objects_before: &[&str],
    crt_objects_after: &[&str],
    is_static: bool,
) -> Result<(), String> {
    if std::env::var("LINKER_DEBUG").is_ok() {
        eprintln!("arm linker: object_files={:?} output={} user_args={:?} static={}", object_files, output_path, user_args, is_static);
    }
    let mut objects: Vec<ElfObject> = Vec::new();
    let mut globals: HashMap<String, GlobalSymbol> = HashMap::new();
    let mut needed_sonames: Vec<String> = Vec::new();

    let all_lib_paths: Vec<String> = lib_paths.iter().map(|s| s.to_string()).collect();

    // Parse user args for export-dynamic flag
    let mut export_dynamic = false;
    for arg in user_args {
        if arg == "-rdynamic" { export_dynamic = true; }
        if let Some(wl_arg) = arg.strip_prefix("-Wl,") {
            for part in wl_arg.split(',') {
                if part == "--export-dynamic" || part == "-E" { export_dynamic = true; }
            }
        }
    }

    // Load CRT objects before user objects
    for path in crt_objects_before {
        if Path::new(path).exists() {
            load_file(path, &mut objects, &mut globals, &mut needed_sonames, &all_lib_paths, is_static)?;
        }
    }

    // Load user object files
    for path in object_files {
        load_file(path, &mut objects, &mut globals, &mut needed_sonames, &all_lib_paths, is_static)?;
    }

    // Parse user_args for -l, -L, bare files, etc.
    let args: Vec<&str> = user_args.iter().map(|s| s.as_str()).collect();
    let mut defsym_defs: Vec<(String, String)> = Vec::new();
    let mut extra_lib_paths: Vec<String> = Vec::new();
    let mut arg_i = 0;
    while arg_i < args.len() {
        let arg = args[arg_i];
        if let Some(path) = arg.strip_prefix("-L") {
            let p = if path.is_empty() && arg_i + 1 < args.len() { arg_i += 1; args[arg_i] } else { path };
            extra_lib_paths.push(p.to_string());
        } else if let Some(lib) = arg.strip_prefix("-l") {
            let l = if lib.is_empty() && arg_i + 1 < args.len() { arg_i += 1; args[arg_i] } else { lib };
            let resolver = if is_static { resolve_lib } else { resolve_lib_prefer_shared };
            let mut combined = extra_lib_paths.clone();
            combined.extend(all_lib_paths.iter().cloned());
            if let Some(lib_path) = resolver(l, &combined) {
                load_file(&lib_path, &mut objects, &mut globals, &mut needed_sonames, &combined, is_static)?;
            }
        } else if let Some(wl_arg) = arg.strip_prefix("-Wl,") {
            let parts: Vec<&str> = wl_arg.split(',').collect();
            let mut j = 0;
            while j < parts.len() {
                let part = parts[j];
                if let Some(lpath) = part.strip_prefix("-L") {
                    extra_lib_paths.push(lpath.to_string());
                } else if let Some(lib) = part.strip_prefix("-l") {
                    let resolver = if is_static { resolve_lib } else { resolve_lib_prefer_shared };
                    let mut combined = extra_lib_paths.clone();
                    combined.extend(all_lib_paths.iter().cloned());
                    if let Some(lib_path) = resolver(lib, &combined) {
                        load_file(&lib_path, &mut objects, &mut globals, &mut needed_sonames, &combined, is_static)?;
                    }
                } else if let Some(defsym_arg) = part.strip_prefix("--defsym=") {
                    // --defsym=SYMBOL=EXPR: define a symbol alias
                    // TODO: only supports symbol-to-symbol aliasing, not arbitrary expressions
                    if let Some(eq_pos) = defsym_arg.find('=') {
                        defsym_defs.push((defsym_arg[..eq_pos].to_string(), defsym_arg[eq_pos + 1..].to_string()));
                    }
                } else if part == "--defsym" && j + 1 < parts.len() {
                    // Two-argument form: --defsym SYM=VAL
                    j += 1;
                    let defsym_arg = parts[j];
                    if let Some(eq_pos) = defsym_arg.find('=') {
                        defsym_defs.push((defsym_arg[..eq_pos].to_string(), defsym_arg[eq_pos + 1..].to_string()));
                    }
                }
                j += 1;
            }
        } else if !arg.starts_with('-') && Path::new(arg).exists() {
            load_file(arg, &mut objects, &mut globals, &mut needed_sonames, &all_lib_paths, is_static)?;
        }
        arg_i += 1;
    }

    // Load CRT objects after user objects
    for path in crt_objects_after {
        if Path::new(path).exists() {
            load_file(path, &mut objects, &mut globals, &mut needed_sonames, &all_lib_paths, is_static)?;
        }
    }

    // Build combined library search paths: user -L first, then system paths
    let mut combined_lib_paths: Vec<String> = extra_lib_paths;
    combined_lib_paths.extend(all_lib_paths.iter().cloned());

    // Load default libraries in a group (like ld's --start-group)
    if !needed_libs.is_empty() {
        let resolver = if is_static { resolve_lib } else { resolve_lib_prefer_shared };
        let mut lib_paths_resolved: Vec<String> = Vec::new();
        for lib_name in needed_libs {
            if let Some(lib_path) = resolver(lib_name, &combined_lib_paths) {
                if !lib_paths_resolved.contains(&lib_path) {
                    lib_paths_resolved.push(lib_path);
                }
            }
        }
        let mut changed = true;
        while changed {
            changed = false;
            let prev_count = objects.len();
            for lib_path in &lib_paths_resolved {
                load_file(lib_path, &mut objects, &mut globals, &mut needed_sonames, &combined_lib_paths, is_static)?;
            }
            if objects.len() != prev_count {
                changed = true;
            }
        }
    }

    // For dynamic linking, resolve remaining undefined symbols against system libs
    if !is_static {
        resolve_dynamic_symbols(&mut globals, &mut needed_sonames, &combined_lib_paths)?;
    }

    // Apply --defsym definitions: alias one symbol to another
    for (alias, target) in &defsym_defs {
        if let Some(target_sym) = globals.get(target).cloned() {
            globals.insert(alias.clone(), target_sym);
        }
    }

    // Reject truly undefined symbols (weak undefined are allowed)
    let mut unresolved = Vec::new();
    for (name, sym) in &globals {
        if sym.defined_in.is_none() && !sym.is_dynamic && sym.section_idx == SHN_UNDEF {
            let binding = sym.info >> 4;
            if binding != STB_WEAK {
                if !matches!(name.as_str(), "__bss_start" | "_edata" | "_end" | "__end"
                    | "_GLOBAL_OFFSET_TABLE_" | "__dso_handle" | "_DYNAMIC"
                    | "__GNU_EH_FRAME_HDR" | "__ehdr_start" | "__executable_start"
                    | "_init" | "_fini"
                    | "__init_array_start" | "__init_array_end"
                    | "__fini_array_start" | "__fini_array_end"
                    | "__preinit_array_start" | "__preinit_array_end"
                    | "__rela_iplt_start" | "__rela_iplt_end"
                    | "_etext" | "etext" | "__data_start" | "data_start"
                    | "__getauxval"
                    | "_start") {
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

    // Check if we have any dynamic symbols
    let has_dynamic_syms = globals.values().any(|g| g.is_dynamic);

    if has_dynamic_syms && !is_static {
        // Create PLT/GOT for dynamic symbols
        let (plt_names, got_entries) = create_plt_got(&objects, &mut globals);

        // Emit dynamically-linked executable
        emit_dynamic_executable(
            &objects, &mut globals, &mut output_sections, &section_map,
            &plt_names, &got_entries, &needed_sonames, output_path,
            export_dynamic,
        )
    } else {
        // Fall back to static emit
        emit_executable(&objects, &mut globals, &mut output_sections, &section_map, output_path)
    }
}

// ── File loading ───────────────────────────────────────────────────────

fn load_file(
    path: &str,
    objects: &mut Vec<ElfObject>,
    globals: &mut HashMap<String, GlobalSymbol>,
    needed_sonames: &mut Vec<String>,
    lib_paths: &[String],
    is_static: bool,
) -> Result<(), String> {
    if std::env::var("LINKER_DEBUG").is_ok() {
        eprintln!("load_file: {}", path);
    }
    let data = std::fs::read(path).map_err(|e| format!("failed to read '{}': {}", path, e))?;

    // Archive?
    if data.len() >= 8 && &data[0..8] == b"!<arch>\n" {
        return load_archive(&data, path, objects, globals, needed_sonames, lib_paths, is_static);
    }

    // Thin archive?
    if is_thin_archive(&data) {
        return load_thin_archive(&data, path, objects, globals, lib_paths);
    }

    // Not ELF? Try linker script (handles both GROUP and INPUT directives)
    if data.len() >= 4 && data[0..4] != ELF_MAGIC {
        if let Ok(text) = std::str::from_utf8(&data) {
            if let Some(entries) = parse_linker_script_entries(text) {
                let script_dir = Path::new(path).parent().map(|p| p.to_string_lossy().to_string());
                for entry in &entries {
                    match entry {
                        LinkerScriptEntry::Path(lib_path) => {
                            if Path::new(lib_path).exists() {
                                load_file(lib_path, objects, globals, needed_sonames, lib_paths, is_static)?;
                            } else if let Some(ref dir) = script_dir {
                                let resolved = format!("{}/{}", dir, lib_path);
                                if Path::new(&resolved).exists() {
                                    load_file(&resolved, objects, globals, needed_sonames, lib_paths, is_static)?;
                                }
                            }
                        }
                        LinkerScriptEntry::Lib(lib_name) => {
                            if let Some(resolved) = resolve_lib(lib_name, lib_paths) {
                                load_file(&resolved, objects, globals, needed_sonames, lib_paths, is_static)?;
                            }
                        }
                    }
                }
                return Ok(());
            }
        }
        return Err(format!("{}: not a valid ELF object or archive", path));
    }

    // Shared library?
    if data.len() >= 18 {
        let e_type = read_u16(&data, 16);
        if e_type == ET_DYN {
            if is_static {
                return Ok(()); // Skip .so in static linking
            }
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
    data: &[u8],
    archive_path: &str,
    objects: &mut Vec<ElfObject>,
    globals: &mut HashMap<String, GlobalSymbol>,
    _needed_sonames: &mut Vec<String>,
    _lib_paths: &[String],
    _is_static: bool,
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
                    defined_in: Some(obj_idx), section_idx: sym.shndx,
                    from_lib: None, plt_idx: None, got_idx: None,
                    is_dynamic: false, copy_reloc: false, lib_sym_value: 0,
                });
            }
        } else if sym.shndx == SHN_COMMON {
            if !globals.contains_key(&sym.name) {
                globals.insert(sym.name.clone(), GlobalSymbol {
                    value: sym.value, size: sym.size, info: sym.info,
                    defined_in: Some(obj_idx), section_idx: SHN_COMMON,
                    from_lib: None, plt_idx: None, got_idx: None,
                    is_dynamic: false, copy_reloc: false, lib_sym_value: 0,
                });
            }
        } else if !globals.contains_key(&sym.name) {
            globals.insert(sym.name.clone(), GlobalSymbol {
                value: 0, size: 0, info: sym.info,
                defined_in: None, section_idx: SHN_UNDEF,
                from_lib: None, plt_idx: None, got_idx: None,
                is_dynamic: false, copy_reloc: false, lib_sym_value: 0,
            });
        }
    }
}

fn resolve_lib(name: &str, paths: &[String]) -> Option<String> {
    crate::backend::linker_common::resolve_lib(name, paths, true)
}

fn resolve_lib_prefer_shared(name: &str, paths: &[String]) -> Option<String> {
    // For dynamic linking, prefer .so over .a
    crate::backend::linker_common::resolve_lib(name, paths, false)
}

fn load_shared_library(
    path: &str, globals: &mut HashMap<String, GlobalSymbol>, needed_sonames: &mut Vec<String>,
) -> Result<(), String> {
    let data = std::fs::read(path).map_err(|e| format!("failed to read '{}': {}", path, e))?;
    if data.len() >= 4 && data[0..4] != ELF_MAGIC {
        if let Ok(text) = std::str::from_utf8(&data) {
            if let Some(entries) = parse_linker_script_entries(text) {
                for entry in &entries {
                    match entry {
                        LinkerScriptEntry::Path(lib_path) => {
                            if Path::new(lib_path).exists() {
                                load_shared_library(lib_path, globals, needed_sonames)?;
                            }
                        }
                        LinkerScriptEntry::Lib(_) => {} // -l not expected in .so linker scripts
                    }
                }
                return Ok(());
            }
        }
        return Err(format!("{}: not a valid ELF shared library", path));
    }

    let soname = linker_common::parse_soname(&data).unwrap_or_else(|| {
        Path::new(path).file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_else(|| path.to_string())
    });
    if !needed_sonames.contains(&soname) { needed_sonames.push(soname.clone()); }

    let dyn_syms = linker_common::parse_shared_library_symbols(&data, path)?;
    let mut matched_weak_objects: Vec<(u64, u64)> = Vec::new();
    for dsym in &dyn_syms {
        if let Some(existing) = globals.get(&dsym.name) {
            if existing.defined_in.is_none() && !existing.is_dynamic {
                globals.insert(dsym.name.clone(), GlobalSymbol {
                    value: 0, size: dsym.size, info: dsym.info,
                    defined_in: None, from_lib: Some(soname.clone()),
                    plt_idx: None, got_idx: None,
                    section_idx: SHN_UNDEF, is_dynamic: true, copy_reloc: false,
                    lib_sym_value: dsym.value,
                });
                let bind = dsym.info >> 4;
                let stype = dsym.info & 0xf;
                if bind == STB_WEAK && stype == STT_OBJECT
                    && !matched_weak_objects.contains(&(dsym.value, dsym.size))
                {
                    matched_weak_objects.push((dsym.value, dsym.size));
                }
            }
        }
    }
    // Register aliases for matched WEAK STT_OBJECT symbols
    if !matched_weak_objects.is_empty() {
        for dsym in &dyn_syms {
            let stype = dsym.info & 0xf;
            if stype == STT_OBJECT && matched_weak_objects.contains(&(dsym.value, dsym.size))
                && !globals.contains_key(&dsym.name)
            {
                globals.insert(dsym.name.clone(), GlobalSymbol {
                    value: 0, size: dsym.size, info: dsym.info,
                    defined_in: None, from_lib: Some(soname.clone()),
                    plt_idx: None, got_idx: None,
                    section_idx: SHN_UNDEF, is_dynamic: true, copy_reloc: false,
                    lib_sym_value: dsym.value,
                });
            }
        }
    }
    Ok(())
}

fn resolve_dynamic_symbols(
    globals: &mut HashMap<String, GlobalSymbol>, needed_sonames: &mut Vec<String>,
    lib_paths: &[String],
) -> Result<(), String> {
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

    // Search for shared libraries in the provided library paths
    let lib_names = ["libc.so.6", "libm.so.6", "libgcc_s.so.1"];
    let mut libs: Vec<String> = Vec::new();
    for lib_name in &lib_names {
        for dir in lib_paths {
            let candidate = format!("{}/{}", dir, lib_name);
            if Path::new(&candidate).exists() {
                libs.push(candidate);
                break;
            }
        }
    }

    for lib_path in &libs {
        if !Path::new(lib_path).exists() { continue; }
        let data = match std::fs::read(lib_path) { Ok(d) => d, Err(_) => continue };
        let soname = linker_common::parse_soname(&data).unwrap_or_else(|| {
            Path::new(lib_path).file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_default()
        });
        let dyn_syms = match linker_common::parse_shared_library_symbols(&data, lib_path) { Ok(s) => s, Err(_) => continue };

        let mut lib_needed = false;
        let mut matched_weak_objects: Vec<(u64, u64)> = Vec::new();
        for dsym in &dyn_syms {
            if let Some(existing) = globals.get(&dsym.name) {
                if existing.defined_in.is_none() && !existing.is_dynamic {
                    lib_needed = true;
                    globals.insert(dsym.name.clone(), GlobalSymbol {
                        value: 0, size: dsym.size, info: dsym.info,
                        defined_in: None, from_lib: Some(soname.clone()),
                        plt_idx: None, got_idx: None,
                        section_idx: SHN_UNDEF, is_dynamic: true, copy_reloc: false,
                        lib_sym_value: dsym.value,
                    });
                    let bind = dsym.info >> 4;
                    let stype = dsym.info & 0xf;
                    if bind == STB_WEAK && stype == STT_OBJECT
                        && !matched_weak_objects.contains(&(dsym.value, dsym.size))
                    {
                        matched_weak_objects.push((dsym.value, dsym.size));
                    }
                }
            }
        }
        if !matched_weak_objects.is_empty() {
            for dsym in &dyn_syms {
                let stype = dsym.info & 0xf;
                if stype == STT_OBJECT && matched_weak_objects.contains(&(dsym.value, dsym.size))
                    && !globals.contains_key(&dsym.name)
                {
                    lib_needed = true;
                    globals.insert(dsym.name.clone(), GlobalSymbol {
                        value: 0, size: dsym.size, info: dsym.info,
                        defined_in: None, from_lib: Some(soname.clone()),
                        plt_idx: None, got_idx: None,
                        section_idx: SHN_UNDEF, is_dynamic: true, copy_reloc: false,
                        lib_sym_value: dsym.value,
                    });
                }
            }
        }
        if lib_needed && !needed_sonames.contains(&soname) { needed_sonames.push(soname); }
    }
    Ok(())
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

// ── PLT/GOT creation ───────────────────────────────────────────────────

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
                    R_AARCH64_CALL26 | R_AARCH64_JUMP26 if gsym_info.map(|g| g.0).unwrap_or(false) => {
                        let sym_type = gsym_info.map(|g| g.1).unwrap_or(0);
                        if sym_type == STT_OBJECT {
                            if !copy_reloc_names.contains(&sym.name) {
                                copy_reloc_names.push(sym.name.clone());
                            }
                        } else {
                            if !plt_names.contains(&sym.name) { plt_names.push(sym.name.clone()); }
                        }
                    }
                    R_AARCH64_ADR_PREL_PG_HI21 | R_AARCH64_ADD_ABS_LO12_NC
                    | R_AARCH64_LDST64_ABS_LO12_NC | R_AARCH64_LDST32_ABS_LO12_NC
                    | R_AARCH64_LDST8_ABS_LO12_NC | R_AARCH64_LDST16_ABS_LO12_NC
                    | R_AARCH64_LDST128_ABS_LO12_NC
                    if gsym_info.map(|g| g.0).unwrap_or(false) => {
                        let sym_type = gsym_info.map(|g| g.1).unwrap_or(0);
                        if sym_type == STT_OBJECT {
                            if !copy_reloc_names.contains(&sym.name) {
                                copy_reloc_names.push(sym.name.clone());
                            }
                        } else {
                            // Function referenced via ADRP+ADD (e.g., taking address)
                            if !plt_names.contains(&sym.name) { plt_names.push(sym.name.clone()); }
                        }
                    }
                    R_AARCH64_ABS64 if gsym_info.map(|g| g.0).unwrap_or(false) => {
                        let sym_type = gsym_info.map(|g| g.1).unwrap_or(0);
                        if sym_type != STT_OBJECT {
                            if !plt_names.contains(&sym.name) { plt_names.push(sym.name.clone()); }
                        } else if !copy_reloc_names.contains(&sym.name) {
                            copy_reloc_names.push(sym.name.clone());
                        }
                    }
                    R_AARCH64_ADR_GOT_PAGE | R_AARCH64_LD64_GOT_LO12_NC => {
                        if !got_only_names.contains(&sym.name) && !plt_names.contains(&sym.name) {
                            got_only_names.push(sym.name.clone());
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    // Mark copy relocation symbols
    let mut copy_reloc_lib_addrs: Vec<(String, u64)> = Vec::new();
    for name in &copy_reloc_names {
        if let Some(gsym) = globals.get_mut(name) {
            gsym.copy_reloc = true;
            if let Some(ref lib) = gsym.from_lib {
                if (gsym.info & 0xf) == STT_OBJECT && gsym.lib_sym_value != 0 {
                    let key = (lib.clone(), gsym.lib_sym_value);
                    if !copy_reloc_lib_addrs.contains(&key) {
                        copy_reloc_lib_addrs.push(key);
                    }
                }
            }
        }
    }
    // Mark aliases
    if !copy_reloc_lib_addrs.is_empty() {
        let alias_names: Vec<String> = globals.iter()
            .filter(|(name, g)| {
                g.is_dynamic && !g.copy_reloc && (g.info & 0xf) == STT_OBJECT
                    && !copy_reloc_names.contains(name)
                    && g.from_lib.is_some() && g.lib_sym_value != 0
                    && copy_reloc_lib_addrs.contains(
                        &(g.from_lib.as_ref().unwrap().clone(), g.lib_sym_value))
            })
            .map(|(n, _)| n.clone())
            .collect();
        for name in alias_names {
            if let Some(gsym) = globals.get_mut(&name) {
                gsym.copy_reloc = true;
            }
        }
    }

    // Build GOT entries: [0]=.dynamic, [1]=reserved, [2]=reserved, then PLT entries, then GOT-only
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

// ── Dynamic executable emission ─────────────────────────────────────────

/// Emit a dynamically-linked AArch64 ELF executable with PLT/GOT/.dynamic support.
#[allow(clippy::too_many_arguments)]
fn emit_dynamic_executable(
    objects: &[ElfObject], globals: &mut HashMap<String, GlobalSymbol>,
    output_sections: &mut [OutputSection],
    section_map: &HashMap<(usize, usize), (usize, u64)>,
    plt_names: &[String], got_entries: &[(String, bool)],
    needed_sonames: &[String], output_path: &str,
    export_dynamic: bool,
) -> Result<(), String> {
    let mut dynstr = DynStrTab::new();
    for lib in needed_sonames { dynstr.add(lib); }

    // Build dynamic symbol name list
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
    let gnu_hash_symoffset = 1 + dyn_sym_names.len();

    // Collect copy relocation symbols
    let copy_reloc_syms: Vec<(String, u64)> = globals.iter()
        .filter(|(_, g)| g.copy_reloc)
        .map(|(n, g)| (n.clone(), g.size))
        .collect();
    for (name, _) in &copy_reloc_syms {
        if !dyn_sym_names.contains(name) {
            dyn_sym_names.push(name.clone());
        }
    }

    if export_dynamic {
        let mut exported: Vec<String> = globals.iter()
            .filter(|(_, g)| {
                g.section_idx != SHN_UNDEF && !g.is_dynamic && !g.copy_reloc
                    && (g.info >> 4) != 0
            })
            .map(|(n, _)| n.clone())
            .collect();
        exported.sort();
        for name in exported {
            if !dyn_sym_names.contains(&name) {
                dyn_sym_names.push(name);
            }
        }
    }

    for name in &dyn_sym_names { dynstr.add(name); }

    let dynsym_count = 1 + dyn_sym_names.len();
    let dynsym_size = dynsym_count as u64 * 24;
    let dynstr_size = dynstr.as_bytes().len() as u64;
    let rela_plt_size = plt_names.len() as u64 * 24;
    let rela_dyn_glob_count = got_entries.iter().filter(|(n, p)| {
        !n.is_empty() && !*p && globals.get(n).map(|g| g.is_dynamic && !g.copy_reloc).unwrap_or(false)
    }).count();
    let rela_dyn_count = rela_dyn_glob_count + copy_reloc_syms.len();
    let rela_dyn_size = rela_dyn_count as u64 * 24;

    // Build .gnu.hash
    let num_hashed = dyn_sym_names.len() - (gnu_hash_symoffset - 1);
    let gnu_hash_nbuckets = if num_hashed == 0 { 1 } else { num_hashed.next_power_of_two().max(1) } as u32;
    let gnu_hash_bloom_size: u32 = 1;
    let gnu_hash_bloom_shift: u32 = 6;

    let hashed_sym_hashes: Vec<u32> = dyn_sym_names[gnu_hash_symoffset - 1..]
        .iter().map(|name| linker_common::gnu_hash(name.as_bytes())).collect();

    let mut bloom_word: u64 = 0;
    for &h in &hashed_sym_hashes {
        bloom_word |= 1u64 << (h as u64 % 64);
        bloom_word |= 1u64 << ((h >> gnu_hash_bloom_shift) as u64 % 64);
    }

    if num_hashed > 0 {
        let hashed_start = gnu_hash_symoffset - 1;
        let mut hashed_with_hash: Vec<(String, u32)> = dyn_sym_names[hashed_start..]
            .iter().zip(hashed_sym_hashes.iter())
            .map(|(n, &h)| (n.clone(), h)).collect();
        hashed_with_hash.sort_by_key(|(_, h)| h % gnu_hash_nbuckets);
        for (i, (name, _)) in hashed_with_hash.iter().enumerate() {
            dyn_sym_names[hashed_start + i] = name.clone();
        }
    }

    let hashed_sym_hashes: Vec<u32> = dyn_sym_names[gnu_hash_symoffset - 1..]
        .iter().map(|name| linker_common::gnu_hash(name.as_bytes())).collect();

    let mut gnu_hash_buckets = vec![0u32; gnu_hash_nbuckets as usize];
    let mut gnu_hash_chains = vec![0u32; num_hashed];
    for (i, &h) in hashed_sym_hashes.iter().enumerate() {
        let bucket = (h % gnu_hash_nbuckets) as usize;
        if gnu_hash_buckets[bucket] == 0 {
            gnu_hash_buckets[bucket] = (gnu_hash_symoffset + i) as u32;
        }
        gnu_hash_chains[i] = h & !1;
    }
    for bucket_idx in 0..gnu_hash_nbuckets as usize {
        if gnu_hash_buckets[bucket_idx] == 0 { continue; }
        let mut last_in_bucket = 0;
        for (i, &h) in hashed_sym_hashes.iter().enumerate() {
            if (h % gnu_hash_nbuckets) as usize == bucket_idx {
                last_in_bucket = i;
            }
        }
        gnu_hash_chains[last_in_bucket] |= 1;
    }

    let gnu_hash_size: u64 = 16 + (gnu_hash_bloom_size as u64 * 8)
        + (gnu_hash_nbuckets as u64 * 4) + (num_hashed as u64 * 4);
    // PLT: 32 bytes header + 16 bytes per entry
    let plt_size = if plt_names.is_empty() { 0u64 } else { 32 + 16 * plt_names.len() as u64 };
    let got_plt_count = 3 + plt_names.len();
    let got_plt_size = got_plt_count as u64 * 8;
    let got_globdat_count = got_entries.iter().filter(|(n, p)| !n.is_empty() && !*p).count();
    let got_size = got_globdat_count as u64 * 8;

    let has_init_array = output_sections.iter().any(|s| s.name == ".init_array" && s.mem_size > 0);
    let has_fini_array = output_sections.iter().any(|s| s.name == ".fini_array" && s.mem_size > 0);
    let mut dyn_count = needed_sonames.len() as u64 + 16; // fixed entries + DT_FLAGS + DT_FLAGS_1 + NULL
    if has_init_array { dyn_count += 2; }
    if has_fini_array { dyn_count += 2; }
    let dynamic_size = dyn_count * 16;

    let has_tls_sections = output_sections.iter().any(|s| s.flags & SHF_TLS != 0 && s.flags & SHF_ALLOC != 0);
    // phdrs: PHDR, INTERP, LOAD(ro), LOAD(text), LOAD(rodata), LOAD(rw), DYNAMIC, GNU_STACK, [TLS]
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
            let a = sec.alignment.max(4);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = BASE_ADDR + offset;
            sec.file_offset = offset;
            offset += sec.mem_size;
        }
    }
    // PLT in text segment
    let (plt_addr, plt_offset) = if plt_size > 0 {
        offset = (offset + 15) & !15;
        let a = BASE_ADDR + offset; let o = offset; offset += plt_size; (a, o)
    } else { (0u64, 0u64) };
    let text_total_size = offset - text_page_offset;

    // Rodata segment (separate LOAD R)
    offset = (offset + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
    let rodata_page_offset = offset;
    let rodata_page_addr = BASE_ADDR + offset;
    for sec in output_sections.iter_mut() {
        if sec.flags & SHF_ALLOC != 0 && sec.flags & SHF_EXECINSTR == 0 &&
           sec.flags & SHF_WRITE == 0 && sec.sh_type != SHT_NOBITS &&
           sec.flags & SHF_TLS == 0 {
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

    // Data.rel.ro
    for sec in output_sections.iter_mut() {
        if sec.name == ".data.rel.ro" {
            let a = sec.alignment.max(1);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = BASE_ADDR + offset; sec.file_offset = offset;
            offset += sec.mem_size;
        }
    }

    // Remaining data sections
    for sec in output_sections.iter_mut() {
        if sec.flags & SHF_ALLOC != 0 && sec.flags & SHF_WRITE != 0 &&
           sec.sh_type != SHT_NOBITS && sec.flags & SHF_TLS == 0 &&
           sec.name != ".init_array" && sec.name != ".fini_array" &&
           sec.name != ".data.rel.ro" {
            let a = sec.alignment.max(1);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = BASE_ADDR + offset; sec.file_offset = offset;
            offset += sec.mem_size;
        }
    }

    // TLS sections
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
    if tls_addr == 0 && has_tls_sections {
        tls_addr = BASE_ADDR + offset;
        tls_file_offset = offset;
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

    // BSS space for copy relocations
    let mut copy_reloc_addr_map: HashMap<(String, u64), u64> = HashMap::new();
    for (name, size) in &copy_reloc_syms {
        let gsym = globals.get(name).cloned();
        let key = gsym.as_ref().and_then(|g| {
            g.from_lib.as_ref().map(|lib| (lib.clone(), g.lib_sym_value))
        });
        let addr = if let Some(ref k) = key {
            if let Some(&existing_addr) = copy_reloc_addr_map.get(k) {
                existing_addr
            } else {
                let aligned = (bss_addr + bss_size + 7) & !7;
                bss_size = aligned - bss_addr + size;
                copy_reloc_addr_map.insert(k.clone(), aligned);
                aligned
            }
        } else {
            let aligned = (bss_addr + bss_size + 7) & !7;
            bss_size = aligned - bss_addr + size;
            aligned
        };
        if let Some(gsym) = globals.get_mut(name) {
            gsym.value = addr;
            gsym.defined_in = Some(usize::MAX);
        }
    }

    let rw_filesz = offset - rw_page_offset;
    let rw_memsz = if bss_size > 0 { (bss_addr + bss_size) - rw_page_addr } else { rw_filesz };

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
            if obj_idx == usize::MAX { continue; } // linker-defined or copy-reloc
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

    // Define linker-provided symbols
    let text_seg_end = text_page_addr + text_total_size;
    let linker_addrs = LinkerSymbolAddresses {
        base_addr: BASE_ADDR,
        got_addr: got_plt_addr,
        dynamic_addr,
        bss_addr,
        bss_size,
        text_end: text_seg_end,
        data_start: rw_page_addr,
        init_array_start: init_array_addr,
        init_array_size,
        fini_array_start: fini_array_addr,
        fini_array_size,
        preinit_array_start: 0,
        preinit_array_size: 0,
        rela_iplt_start: 0,
        rela_iplt_size: 0,
    };
    for sym in &get_standard_linker_symbols(&linker_addrs) {
        let entry = globals.entry(sym.name.to_string()).or_insert(GlobalSymbol {
            value: 0, size: 0, info: (sym.binding << 4),
            defined_in: None, from_lib: None, plt_idx: None, got_idx: None,
            section_idx: SHN_ABS, is_dynamic: false, copy_reloc: false, lib_sym_value: 0,
        });
        if entry.defined_in.is_none() && !entry.is_dynamic {
            entry.value = sym.value;
            entry.defined_in = Some(usize::MAX);
            entry.section_idx = SHN_ABS;
        }
    }

    let entry_addr = globals.get("_start").map(|s| s.value).unwrap_or(text_page_addr);

    // === Build output buffer ===
    let file_size = offset as usize;
    let mut out = vec![0u8; file_size];

    // ELF header
    out[0..4].copy_from_slice(&ELF_MAGIC);
    out[4] = ELFCLASS64; out[5] = ELFDATA2LSB; out[6] = 1;
    out[7] = 0; // ELFOSABI_NONE for dynamic executables
    w16(&mut out, 16, ET_EXEC); w16(&mut out, 18, EM_AARCH64); w32(&mut out, 20, 1);
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
    if rodata_total_size > 0 {
        wphdr(&mut out, ph, PT_LOAD, PF_R, rodata_page_offset, rodata_page_addr, rodata_total_size, rodata_total_size, PAGE_SIZE); ph += 56;
    } else {
        // Empty placeholder segment to keep phdr count consistent
        wphdr(&mut out, ph, PT_LOAD, PF_R, rodata_page_offset, rodata_page_addr, 0, 0, PAGE_SIZE); ph += 56;
    }
    wphdr(&mut out, ph, PT_LOAD, PF_R|PF_W, rw_page_offset, rw_page_addr, rw_filesz, rw_memsz, PAGE_SIZE); ph += 56;
    wphdr(&mut out, ph, PT_DYNAMIC, PF_R|PF_W, dynamic_offset, dynamic_addr, dynamic_size, dynamic_size, 8); ph += 56;
    wphdr(&mut out, ph, PT_GNU_STACK, PF_R|PF_W, 0, 0, 0, 0, 0x10); ph += 56;
    if has_tls {
        wphdr(&mut out, ph, PT_TLS, PF_R, tls_file_offset, tls_addr, tls_file_size, tls_mem_size, tls_align);
    }

    // .interp
    write_bytes(&mut out, interp_offset as usize, INTERP);

    // .gnu.hash
    let gh = gnu_hash_offset as usize;
    w32(&mut out, gh, gnu_hash_nbuckets);
    w32(&mut out, gh+4, gnu_hash_symoffset as u32);
    w32(&mut out, gh+8, gnu_hash_bloom_size);
    w32(&mut out, gh+12, gnu_hash_bloom_shift);
    let bloom_off = gh + 16;
    w64(&mut out, bloom_off, bloom_word);
    let buckets_off = bloom_off + (gnu_hash_bloom_size as usize * 8);
    for (i, &b) in gnu_hash_buckets.iter().enumerate() {
        w32(&mut out, buckets_off + i * 4, b);
    }
    let chains_off = buckets_off + (gnu_hash_nbuckets as usize * 4);
    for (i, &c) in gnu_hash_chains.iter().enumerate() {
        w32(&mut out, chains_off + i * 4, c);
    }

    // .dynsym
    let mut ds = dynsym_offset as usize + 24;
    for name in &dyn_sym_names {
        let no = dynstr.get_offset(name) as u32;
        w32(&mut out, ds, no);
        if let Some(gsym) = globals.get(name) {
            if gsym.copy_reloc {
                if ds+5 < out.len() { out[ds+4] = (STB_GLOBAL << 4) | STT_OBJECT; out[ds+5] = 0; }
                w16(&mut out, ds+6, 1);
                w64(&mut out, ds+8, gsym.value);
                w64(&mut out, ds+16, gsym.size);
            } else if !gsym.is_dynamic && gsym.section_idx != SHN_UNDEF && gsym.value != 0 {
                let stt = gsym.info & 0xf;
                let stb = gsym.info >> 4;
                if ds+5 < out.len() { out[ds+4] = (stb << 4) | stt; out[ds+5] = 0; }
                w16(&mut out, ds+6, 1);
                w64(&mut out, ds+8, gsym.value);
                w64(&mut out, ds+16, gsym.size);
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
    write_bytes(&mut out, dynstr_offset as usize, dynstr.as_bytes());

    // .rela.dyn (GLOB_DAT + COPY)
    // AArch64: R_AARCH64_GLOB_DAT = 1025, R_AARCH64_COPY = 1024
    const R_AARCH64_GLOB_DAT: u64 = 1025;
    const R_AARCH64_COPY: u64 = 1024;
    let mut rd = rela_dyn_offset as usize;
    let mut gd_a = got_addr;
    for (name, is_plt) in got_entries {
        if name.is_empty() || *is_plt { continue; }
        let is_dynamic = globals.get(name).map(|g| g.is_dynamic && !g.copy_reloc).unwrap_or(false);
        if is_dynamic {
            let si = dyn_sym_names.iter().position(|n| n == name).map(|i| i+1).unwrap_or(0) as u64;
            w64(&mut out, rd, gd_a); w64(&mut out, rd+8, (si << 32) | R_AARCH64_GLOB_DAT); w64(&mut out, rd+16, 0);
            rd += 24;
        }
        gd_a += 8;
    }
    for (name, _) in &copy_reloc_syms {
        if let Some(gsym) = globals.get(name) {
            let si = dyn_sym_names.iter().position(|n| n == name).map(|i| i+1).unwrap_or(0) as u64;
            let copy_addr = gsym.value;
            w64(&mut out, rd, copy_addr); w64(&mut out, rd+8, (si << 32) | R_AARCH64_COPY); w64(&mut out, rd+16, 0);
            rd += 24;
        }
    }

    // .rela.plt (R_AARCH64_JUMP_SLOT = 1026)
    const R_AARCH64_JUMP_SLOT: u64 = 1026;
    let mut rp = rela_plt_offset as usize;
    let gpb = got_plt_addr + 24;
    for (i, name) in plt_names.iter().enumerate() {
        let gea = gpb + i as u64 * 8;
        let si = dyn_sym_names.iter().position(|n| n == name).map(|j| j+1).unwrap_or(0) as u64;
        w64(&mut out, rp, gea); w64(&mut out, rp+8, (si << 32) | R_AARCH64_JUMP_SLOT); w64(&mut out, rp+16, 0);
        rp += 24;
    }

    // Section data
    for sec in output_sections.iter() {
        if sec.sh_type == SHT_NOBITS || sec.data.is_empty() { continue; }
        write_bytes(&mut out, sec.file_offset as usize, &sec.data);
    }

    // .plt (AArch64 PLT stubs)
    if plt_size > 0 {
        let po = plt_offset as usize;
        // PLT header (32 bytes):
        // stp x16, x30, [sp, #-16]!   ; save registers
        // adrp x16, GOT+16            ; load page of GOT[2]
        // ldr x17, [x16, #lo12(GOT+16)] ; load GOT[2] (resolver)
        // add x16, x16, #lo12(GOT+16)   ; compute address
        // br x17                        ; jump to resolver
        // nop; nop; nop                  ; padding to 32 bytes

        let got2_addr = got_plt_addr + 16;
        let page_g = got2_addr & !0xFFF;
        let page_p = plt_addr & !0xFFF;
        let page_diff = ((page_g as i64 - page_p as i64) >> 12) as i32;
        let immlo = (page_diff & 3) as u32;
        let immhi = ((page_diff >> 2) & 0x7ffff) as u32;

        // STP x16, x30, [sp, #-16]!
        w32(&mut out, po, 0xa9bf7bf0);
        // ADRP x16, page_of(GOT+16)
        w32(&mut out, po + 4, 0x90000010 | (immlo << 29) | (immhi << 5));
        // LDR x17, [x16, #lo12(GOT+16)]
        let lo12 = (got2_addr & 0xFFF) as u32;
        w32(&mut out, po + 8, 0xf9400211 | ((lo12 / 8) << 10));
        // ADD x16, x16, #lo12(GOT+16)
        w32(&mut out, po + 12, 0x91000210 | ((lo12 & 0xFFF) << 10));
        // BR x17
        w32(&mut out, po + 16, 0xd61f0220);
        // NOP padding
        w32(&mut out, po + 20, 0xd503201f);
        w32(&mut out, po + 24, 0xd503201f);
        w32(&mut out, po + 28, 0xd503201f);

        // Individual PLT entries (16 bytes each):
        // adrp x16, GOT_entry_page
        // ldr x17, [x16, #lo12(GOT_entry)]
        // add x16, x16, #lo12(GOT_entry)
        // br x17
        for (i, _) in plt_names.iter().enumerate() {
            let ep = po + 32 + i * 16;
            let pea = plt_addr + 32 + i as u64 * 16;
            let gea = got_plt_addr + 24 + i as u64 * 8;

            let page_g = gea & !0xFFF;
            let page_p = pea & !0xFFF;
            let page_diff = ((page_g as i64 - page_p as i64) >> 12) as i32;
            let immlo = (page_diff & 3) as u32;
            let immhi = ((page_diff >> 2) & 0x7ffff) as u32;

            // ADRP x16, page_of(GOT entry)
            w32(&mut out, ep, 0x90000010 | (immlo << 29) | (immhi << 5));
            // LDR x17, [x16, #lo12(GOT entry)]
            let lo12 = (gea & 0xFFF) as u32;
            w32(&mut out, ep + 4, 0xf9400211 | ((lo12 / 8) << 10));
            // ADD x16, x16, #lo12(GOT entry)
            w32(&mut out, ep + 8, 0x91000210 | ((lo12 & 0xFFF) << 10));
            // BR x17
            w32(&mut out, ep + 12, 0xd61f0220);
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
        (DT_FLAGS, DF_BIND_NOW as u64), (DT_FLAGS_1, DF_1_NOW as u64),
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

    // .got (GLOB_DAT entries)
    let mut go = got_offset as usize;
    for (name, is_plt) in got_entries {
        if name.is_empty() || *is_plt { continue; }
        if let Some(gsym) = globals.get(name) {
            if gsym.defined_in.is_some() && !gsym.is_dynamic {
                let sym_val = gsym.value;
                if has_tls && (gsym.info & 0xf) == STT_TLS {
                    let tpoff = (sym_val as i64 - tls_addr as i64) + 16;
                    w64(&mut out, go, tpoff as u64);
                } else {
                    w64(&mut out, go, sym_val);
                }
            } else if gsym.copy_reloc && gsym.value != 0 {
                w64(&mut out, go, gsym.value);
            }
        }
        go += 8;
    }

    // .got.plt
    let gp = got_plt_offset as usize;
    w64(&mut out, gp, dynamic_addr);
    w64(&mut out, gp+8, 0); w64(&mut out, gp+16, 0);
    for (i, _) in plt_names.iter().enumerate() {
        // Initialize GOT.plt entries to PLT[0] (resolved eagerly via DF_BIND_NOW)
        w64(&mut out, gp+24+i*8, plt_addr);
    }

    // Apply relocations
    let globals_snap: HashMap<String, GlobalSymbol> = globals.clone();

    // Build GotInfo for GOT-only entries (non-PLT symbols accessed via ADR_GOT_PAGE/LD64_GOT_LO12_NC)
    let mut dyn_got_entries: HashMap<String, usize> = HashMap::new();
    {
        let mut got_only_idx = 0usize;
        for (name, is_plt) in got_entries.iter() {
            if name.is_empty() || *is_plt { continue; }
            // got_key for global symbols is just the name
            dyn_got_entries.insert(name.clone(), got_only_idx);
            got_only_idx += 1;
        }
    }
    let dyn_got_info = reloc::GotInfo { got_addr, entries: dyn_got_entries };

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
                let s = resolve_sym_dynamic(obj_idx, sym, &globals_snap, section_map, output_sections, plt_addr);

                match rela.rela_type {
                    R_AARCH64_ABS64 => {
                        let t = if !sym.name.is_empty() {
                            if let Some(g) = globals_snap.get(&sym.name) {
                                if g.is_dynamic && !g.copy_reloc {
                                    if let Some(pi) = g.plt_idx { plt_addr + 32 + pi as u64 * 16 } else { s }
                                } else { s }
                            } else { s }
                        } else { s };
                        w64(&mut out, fp, (t as i64 + a) as u64);
                    }
                    R_AARCH64_ABS32 => {
                        w32(&mut out, fp, (s as i64 + a) as u32);
                    }
                    R_AARCH64_ABS16 => {
                        w16(&mut out, fp, (s as i64 + a) as u16);
                    }
                    R_AARCH64_PREL64 => {
                        w64(&mut out, fp, (s as i64 + a - p as i64) as u64);
                    }
                    R_AARCH64_PREL32 => {
                        w32(&mut out, fp, (s as i64 + a - p as i64) as u32);
                    }
                    R_AARCH64_PREL16 => {
                        w16(&mut out, fp, (s as i64 + a - p as i64) as u16);
                    }
                    R_AARCH64_ADR_PREL_PG_HI21 => {
                        let sa_val = (s as i64 + a) as u64;
                        let page_s = sa_val & !0xFFF;
                        let page_p = p & !0xFFF;
                        let imm = (page_s as i64 - page_p as i64) >> 12;
                        reloc::encode_adrp(&mut out, fp, imm);
                    }
                    R_AARCH64_ADR_PREL_LO21 => {
                        let offset_val = (s as i64 + a) - (p as i64);
                        reloc::encode_adr(&mut out, fp, offset_val);
                    }
                    R_AARCH64_ADD_ABS_LO12_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_add_imm12(&mut out, fp, (sa_val & 0xFFF) as u32);
                    }
                    R_AARCH64_LDST8_ABS_LO12_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_ldst_imm12(&mut out, fp, (sa_val & 0xFFF) as u32, 0);
                    }
                    R_AARCH64_LDST16_ABS_LO12_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_ldst_imm12(&mut out, fp, (sa_val & 0xFFF) as u32, 1);
                    }
                    R_AARCH64_LDST32_ABS_LO12_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_ldst_imm12(&mut out, fp, (sa_val & 0xFFF) as u32, 2);
                    }
                    R_AARCH64_LDST64_ABS_LO12_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_ldst_imm12(&mut out, fp, (sa_val & 0xFFF) as u32, 3);
                    }
                    R_AARCH64_LDST128_ABS_LO12_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_ldst_imm12(&mut out, fp, (sa_val & 0xFFF) as u32, 4);
                    }
                    R_AARCH64_CALL26 | R_AARCH64_JUMP26 => {
                        if fp + 4 > out.len() { continue; }
                        let t = if !sym.name.is_empty() {
                            if let Some(g) = globals_snap.get(&sym.name) {
                                if let Some(pi) = g.plt_idx { plt_addr + 32 + pi as u64 * 16 } else { s }
                            } else { s }
                        } else { s };
                        let sa_val = (t as i64 + a) as u64;
                        if sa_val == 0 {
                            w32(&mut out, fp, 0xd503201f); // NOP for weak undef
                        } else {
                            let offset_val = (sa_val as i64) - (p as i64);
                            let mut insn = read_u32(&out, fp);
                            let imm26 = ((offset_val >> 2) as u32) & 0x3ffffff;
                            insn = (insn & 0xfc000000) | imm26;
                            w32(&mut out, fp, insn);
                        }
                    }
                    R_AARCH64_CONDBR19 => {
                        let offset_val = (s as i64 + a) - (p as i64);
                        if fp + 4 > out.len() { continue; }
                        let mut insn = read_u32(&out, fp);
                        let imm19 = ((offset_val >> 2) as u32) & 0x7ffff;
                        insn = (insn & 0xff00001f) | (imm19 << 5);
                        w32(&mut out, fp, insn);
                    }
                    R_AARCH64_TSTBR14 => {
                        let offset_val = (s as i64 + a) - (p as i64);
                        if fp + 4 > out.len() { continue; }
                        let mut insn = read_u32(&out, fp);
                        let imm14 = ((offset_val >> 2) as u32) & 0x3fff;
                        insn = (insn & 0xfff8001f) | (imm14 << 5);
                        w32(&mut out, fp, insn);
                    }
                    R_AARCH64_MOVW_UABS_G0 | R_AARCH64_MOVW_UABS_G0_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_movw(&mut out, fp, (sa_val & 0xffff) as u32);
                    }
                    R_AARCH64_MOVW_UABS_G1_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_movw(&mut out, fp, ((sa_val >> 16) & 0xffff) as u32);
                    }
                    R_AARCH64_MOVW_UABS_G2_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_movw(&mut out, fp, ((sa_val >> 32) & 0xffff) as u32);
                    }
                    R_AARCH64_MOVW_UABS_G3 => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_movw(&mut out, fp, ((sa_val >> 48) & 0xffff) as u32);
                    }
                    R_AARCH64_ADR_GOT_PAGE | R_AARCH64_LD64_GOT_LO12_NC => {
                        // Use GOT entry address for symbols with GOT-only entries
                        let gkey = sym.name.clone();
                        let tls_info = reloc::TlsInfo { tls_addr, tls_size: tls_mem_size };
                        reloc::apply_one_reloc(&mut out, fp, rela.rela_type, s, a, p,
                                               &sym.name, &objects[obj_idx].source_name,
                                               &tls_info, &dyn_got_info, &gkey)?;
                    }
                    _ => {
                        // Delegate to the standard reloc handler for TLS etc.
                        let gkey = reloc::got_key(obj_idx, sym);
                        let tls_info = reloc::TlsInfo { tls_addr, tls_size: tls_mem_size };
                        reloc::apply_one_reloc(&mut out, fp, rela.rela_type, s, a, p,
                                               &sym.name, &objects[obj_idx].source_name,
                                               &tls_info, &dyn_got_info, &gkey)?;
                    }
                }
            }
        }
    }

    // Write output
    std::fs::write(output_path, &out).map_err(|e| format!("failed to write '{}': {}", output_path, e))?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let _ = std::fs::set_permissions(output_path, std::fs::Permissions::from_mode(0o755));
    }
    Ok(())
}

/// Resolve a symbol address for dynamic linking. Dynamic symbols go through PLT.
fn resolve_sym_dynamic(
    obj_idx: usize,
    sym: &Symbol,
    globals: &HashMap<String, GlobalSymbol>,
    section_map: &HashMap<(usize, usize), (usize, u64)>,
    output_sections: &[OutputSection],
    plt_addr: u64,
) -> u64 {
    if sym.sym_type() == STT_SECTION {
        let si = sym.shndx as usize;
        return section_map.get(&(obj_idx, si))
            .map(|&(oi, so)| output_sections[oi].addr + so)
            .unwrap_or(0);
    }
    if !sym.name.is_empty() && !sym.is_local() {
        if let Some(g) = globals.get(&sym.name) {
            if g.defined_in.is_some() { return g.value; }
            if g.is_dynamic {
                if let Some(pi) = g.plt_idx { return plt_addr + 32 + pi as u64 * 16; }
                if g.copy_reloc { return g.value; }
            }
        }
        if sym.is_weak() { return 0; }
    }
    if sym.is_undefined() { return 0; }
    if sym.shndx == SHN_ABS { return sym.value; }
    section_map.get(&(obj_idx, sym.shndx as usize))
        .map(|&(oi, so)| output_sections[oi].addr + so + sym.value)
        .unwrap_or(sym.value)
}

// ── Shared library output ────────────────────────────────────────────

/// Create a shared library (.so) from object files.
pub fn link_shared(
    object_files: &[&str],
    output_path: &str,
    user_args: &[String],
    lib_paths: &[&str],
) -> Result<(), String> {
    let mut objects: Vec<ElfObject> = Vec::new();
    let mut globals: HashMap<String, GlobalSymbol> = HashMap::new();
    let mut needed_sonames: Vec<String> = Vec::new();
    let lib_path_strings: Vec<String> = lib_paths.iter().map(|s| s.to_string()).collect();

    // Parse user args
    let mut extra_lib_paths: Vec<String> = Vec::new();
    let mut libs_to_load: Vec<String> = Vec::new();
    let mut extra_object_files: Vec<String> = Vec::new();
    let mut soname: Option<String> = None;
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
            let parts: Vec<&str> = wl_arg.split(',').collect();
            for j in 0..parts.len() {
                let part = parts[j];
                if let Some(sn) = part.strip_prefix("-soname=") {
                    soname = Some(sn.to_string());
                } else if part == "-soname" && j + 1 < parts.len() {
                    soname = Some(parts[j + 1].to_string());
                } else if let Some(lpath) = part.strip_prefix("-L") {
                    extra_lib_paths.push(lpath.to_string());
                } else if let Some(lib) = part.strip_prefix("-l") {
                    libs_to_load.push(lib.to_string());
                }
            }
        } else if arg == "-shared" || arg == "-nostdlib" || arg == "-o" {
            if arg == "-o" { i += 1; }
        } else if !arg.starts_with('-') && Path::new(arg).exists() {
            extra_object_files.push(arg.to_string());
        }
        i += 1;
    }

    for path in object_files {
        load_file(path, &mut objects, &mut globals, &mut needed_sonames, &lib_path_strings, false)?;
    }
    for path in &extra_object_files {
        load_file(path, &mut objects, &mut globals, &mut needed_sonames, &lib_path_strings, false)?;
    }

    let mut all_lib_paths: Vec<String> = extra_lib_paths;
    all_lib_paths.extend(lib_path_strings.iter().cloned());

    if !libs_to_load.is_empty() {
        let mut lib_paths_resolved: Vec<String> = Vec::new();
        for lib_name in &libs_to_load {
            if let Some(lib_path) = resolve_lib_prefer_shared(lib_name, &all_lib_paths) {
                if !lib_paths_resolved.contains(&lib_path) {
                    lib_paths_resolved.push(lib_path);
                }
            }
        }
        let mut changed = true;
        while changed {
            changed = false;
            let prev_count = objects.len();
            for lib_path in &lib_paths_resolved {
                load_file(lib_path, &mut objects, &mut globals, &mut needed_sonames, &all_lib_paths, false)?;
            }
            if objects.len() != prev_count { changed = true; }
        }
    }

    // Merge sections
    let mut output_sections: Vec<OutputSection> = Vec::new();
    let mut section_map: HashMap<(usize, usize), (usize, u64)> = HashMap::new();
    merge_sections(&objects, &mut output_sections, &mut section_map);
    allocate_common_symbols(&mut globals, &mut output_sections);

    // Resolve undefined symbols against system shared libraries to discover
    // NEEDED dependencies. Without this, the shared library would be missing
    // DT_NEEDED entries for libc.so.6 etc., causing the dynamic linker to
    // fail to resolve PLT symbols at runtime.
    resolve_dynamic_symbols_for_shared(&objects, &globals, &mut needed_sonames, &all_lib_paths);

    // Emit shared library
    emit_shared_library(
        &objects, &mut globals, &mut output_sections, &section_map,
        &needed_sonames, output_path, soname,
    )
}

/// Discover NEEDED shared library dependencies for a shared library build.
/// Scans object file relocations for CALL26/JUMP26 references to undefined symbols
/// and searches system libraries to find which .so files provide them.
fn resolve_dynamic_symbols_for_shared(
    objects: &[ElfObject],
    globals: &HashMap<String, GlobalSymbol>,
    needed_sonames: &mut Vec<String>,
    lib_paths: &[String],
) {
    // Collect undefined symbol names referenced by function calls
    let mut undefined: Vec<String> = Vec::new();
    for obj in objects.iter() {
        for sec_relas in &obj.relocations {
            for rela in sec_relas {
                let si = rela.sym_idx as usize;
                if si >= obj.symbols.len() { continue; }
                let sym = &obj.symbols[si];
                if sym.name.is_empty() || sym.is_local() { continue; }
                let is_undef = if let Some(g) = globals.get(&sym.name) {
                    g.is_dynamic || (g.defined_in.is_none() && g.section_idx == SHN_UNDEF)
                } else {
                    sym.is_undefined()
                };
                if is_undef && !undefined.contains(&sym.name) {
                    undefined.push(sym.name.clone());
                }
            }
        }
    }
    if undefined.is_empty() { return; }

    let lib_names = ["libc.so.6", "libm.so.6", "libpthread.so.0", "libdl.so.2", "librt.so.1"];
    let mut libs: Vec<String> = Vec::new();
    for lib_name in &lib_names {
        for dir in lib_paths {
            let candidate = format!("{}/{}", dir, lib_name);
            if Path::new(&candidate).exists() {
                libs.push(candidate);
                break;
            }
        }
    }
    for lib_path in &libs {
        let data = match std::fs::read(lib_path) { Ok(d) => d, Err(_) => continue };
        let soname = linker_common::parse_soname(&data).unwrap_or_else(|| {
            Path::new(lib_path).file_name().map(|n| n.to_string_lossy().to_string()).unwrap_or_default()
        });
        if needed_sonames.contains(&soname) { continue; }
        let dyn_syms = match linker_common::parse_shared_library_symbols(&data, lib_path) {
            Ok(s) => s, Err(_) => continue,
        };
        let provides_any = undefined.iter().any(|name| dyn_syms.iter().any(|ds| ds.name == *name));
        if provides_any {
            needed_sonames.push(soname);
        }
    }
}

/// Emit a shared library (.so) ELF file for AArch64.
#[allow(clippy::too_many_arguments)]
fn emit_shared_library(
    objects: &[ElfObject], globals: &mut HashMap<String, GlobalSymbol>,
    output_sections: &mut [OutputSection],
    section_map: &HashMap<(usize, usize), (usize, u64)>,
    needed_sonames: &[String], output_path: &str,
    soname: Option<String>,
) -> Result<(), String> {
    let base_addr: u64 = 0;

    let mut dynstr = DynStrTab::new();
    for lib in needed_sonames { dynstr.add(lib); }
    if let Some(ref sn) = soname { dynstr.add(sn); }

    // Export all defined global symbols
    let mut dyn_sym_names: Vec<String> = Vec::new();
    let mut exported: Vec<String> = globals.iter()
        .filter(|(_, g)| {
            g.defined_in.is_some() && !g.is_dynamic
                && (g.info >> 4) != 0
                && g.section_idx != SHN_UNDEF
        })
        .map(|(n, _)| n.clone())
        .collect();
    exported.sort();
    for name in exported {
        if !dyn_sym_names.contains(&name) { dyn_sym_names.push(name); }
    }
    for (name, gsym) in globals.iter() {
        if gsym.is_dynamic && !dyn_sym_names.contains(name) {
            dyn_sym_names.push(name.clone());
        }
    }

    // Collect PLT symbols: external functions referenced by CALL26/JUMP26
    let mut so_plt_names: Vec<String> = Vec::new();
    for obj in objects.iter() {
        for sec_relas in &obj.relocations {
            for rela in sec_relas {
                let si = rela.sym_idx as usize;
                if si >= obj.symbols.len() { continue; }
                let sym = &obj.symbols[si];
                if sym.name.is_empty() { continue; }
                match rela.rela_type {
                    R_AARCH64_CALL26 | R_AARCH64_JUMP26 => {
                        let needs_plt = if let Some(g) = globals.get(&sym.name) {
                            g.is_dynamic || (g.defined_in.is_none() && g.section_idx == SHN_UNDEF)
                        } else {
                            sym.is_undefined() && !sym.is_local()
                        };
                        if needs_plt && !so_plt_names.contains(&sym.name) {
                            so_plt_names.push(sym.name.clone());
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    // Add PLT symbols to dyn_sym_names if not already present
    for name in &so_plt_names {
        if !dyn_sym_names.contains(name) {
            dyn_sym_names.push(name.clone());
        }
    }
    // Assign PLT indices
    for (i, name) in so_plt_names.iter().enumerate() {
        if let Some(g) = globals.get_mut(name) {
            g.plt_idx = Some(i);
        }
    }

    // Add undefined/dynamic symbols referenced by ADRP/ADD/LDST to dyn_sym_names
    // so they get GLOB_DAT relocations and the dynamic linker can resolve them.
    for obj in objects.iter() {
        for sec_relas in &obj.relocations {
            for rela in sec_relas {
                let si = rela.sym_idx as usize;
                if si >= obj.symbols.len() { continue; }
                let sym = &obj.symbols[si];
                if sym.name.is_empty() { continue; }
                match rela.rela_type {
                    R_AARCH64_ADR_PREL_PG_HI21 | R_AARCH64_ADD_ABS_LO12_NC
                    | R_AARCH64_LDST64_ABS_LO12_NC | R_AARCH64_LDST32_ABS_LO12_NC
                    | R_AARCH64_LDST8_ABS_LO12_NC | R_AARCH64_LDST16_ABS_LO12_NC => {
                        let sym_needs_got = if let Some(g) = globals.get(&sym.name) {
                            g.is_dynamic || g.defined_in.is_none()
                        } else {
                            sym.is_undefined() && !sym.is_local()
                        };
                        if sym_needs_got && !dyn_sym_names.contains(&sym.name) {
                            dyn_sym_names.push(sym.name.clone());
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    // Reorder dyn_sym_names: undefined/import symbols first, then defined/export symbols.
    // The .gnu.hash table only covers defined symbols (those after symoffset).
    // Undefined symbols must be placed before symoffset so the dynamic linker
    // doesn't incorrectly find them during symbol lookup in this library.
    let mut undef_names: Vec<String> = Vec::new();
    let mut def_names: Vec<String> = Vec::new();
    for name in &dyn_sym_names {
        let is_undef = if let Some(g) = globals.get(name) {
            g.is_dynamic || g.defined_in.is_none() || g.section_idx == SHN_UNDEF
        } else {
            true
        };
        if is_undef {
            undef_names.push(name.clone());
        } else {
            def_names.push(name.clone());
        }
    }
    dyn_sym_names = Vec::new();
    dyn_sym_names.extend(undef_names.iter().cloned());
    let so_undef_count = dyn_sym_names.len();
    dyn_sym_names.extend(def_names.iter().cloned());

    for name in &dyn_sym_names { dynstr.add(name); }

    let dynsym_count = 1 + dyn_sym_names.len();
    let dynsym_size = dynsym_count as u64 * 24;
    let dynstr_size = dynstr.as_bytes().len() as u64;

    // .gnu.hash - only covers defined symbols (after the undefined ones)
    let gnu_hash_symoffset: usize = 1 + so_undef_count;
    let num_hashed = dyn_sym_names.len() - so_undef_count;
    let gnu_hash_nbuckets = if num_hashed == 0 { 1 } else { num_hashed.next_power_of_two().max(1) } as u32;
    let gnu_hash_bloom_size: u32 = 1;
    let gnu_hash_bloom_shift: u32 = 6;

    let hashed_sym_hashes: Vec<u32> = dyn_sym_names[so_undef_count..].iter()
        .map(|name| linker_common::gnu_hash(name.as_bytes())).collect();

    let mut bloom_word: u64 = 0;
    for &h in &hashed_sym_hashes {
        bloom_word |= 1u64 << (h as u64 % 64);
        bloom_word |= 1u64 << ((h >> gnu_hash_bloom_shift) as u64 % 64);
    }

    if num_hashed > 0 {
        let mut hashed_with_hash: Vec<(String, u32)> = dyn_sym_names[so_undef_count..].iter()
            .zip(hashed_sym_hashes.iter())
            .map(|(n, &h)| (n.clone(), h)).collect();
        hashed_with_hash.sort_by_key(|(_, h)| h % gnu_hash_nbuckets);
        for (i, (name, _)) in hashed_with_hash.iter().enumerate() {
            dyn_sym_names[so_undef_count + i] = name.clone();
        }
    }

    let hashed_sym_hashes: Vec<u32> = dyn_sym_names[so_undef_count..].iter()
        .map(|name| linker_common::gnu_hash(name.as_bytes())).collect();

    let mut gnu_hash_buckets = vec![0u32; gnu_hash_nbuckets as usize];
    let mut gnu_hash_chains = vec![0u32; num_hashed];
    for (i, &h) in hashed_sym_hashes.iter().enumerate() {
        let bucket = (h % gnu_hash_nbuckets) as usize;
        if gnu_hash_buckets[bucket] == 0 {
            gnu_hash_buckets[bucket] = (gnu_hash_symoffset + i) as u32;
        }
        gnu_hash_chains[i] = h & !1;
    }
    for bucket_idx in 0..gnu_hash_nbuckets as usize {
        if gnu_hash_buckets[bucket_idx] == 0 { continue; }
        let mut last_in_bucket = 0;
        for (i, &h) in hashed_sym_hashes.iter().enumerate() {
            if (h % gnu_hash_nbuckets) as usize == bucket_idx {
                last_in_bucket = i;
            }
        }
        gnu_hash_chains[last_in_bucket] |= 1;
    }

    let gnu_hash_size: u64 = 16 + (gnu_hash_bloom_size as u64 * 8)
        + (gnu_hash_nbuckets as u64 * 4) + (num_hashed as u64 * 4);

    let has_init_array = output_sections.iter().any(|s| s.name == ".init_array" && s.mem_size > 0);
    let has_fini_array = output_sections.iter().any(|s| s.name == ".fini_array" && s.mem_size > 0);
    let so_plt_size: u64 = if so_plt_names.is_empty() { 0 } else { 32 + 16 * so_plt_names.len() as u64 };
    let so_got_plt_count: u64 = if so_plt_names.is_empty() { 0 } else { 3 + so_plt_names.len() as u64 };
    let so_got_plt_size: u64 = so_got_plt_count * 8;
    let so_rela_plt_size: u64 = so_plt_names.len() as u64 * 24;

    let mut dyn_count = needed_sonames.len() as u64 + 12; // base 10 + 2 for FLAGS/FLAGS_1
    if soname.is_some() { dyn_count += 1; }
    if has_init_array { dyn_count += 2; }
    if has_fini_array { dyn_count += 2; }
    if !so_plt_names.is_empty() { dyn_count += 4; } // PLTGOT, PLTRELSZ, PLTREL, JMPREL
    let dynamic_size = dyn_count * 16;

    let has_tls_sections = output_sections.iter().any(|s| s.flags & SHF_TLS != 0 && s.flags & SHF_ALLOC != 0);

    // Promote read-only sections that have R_AARCH64_ABS64 relocations to writable.
    // These sections contain embedded pointers that become R_AARCH64_RELATIVE dynamic
    // relocations, so the dynamic linker must be able to write to them at load time.
    {
        // Build a set of output section indices that have ABS64 relocs targeting them
        let mut needs_write: std::collections::HashSet<usize> = std::collections::HashSet::new();
        for obj_idx in 0..objects.len() {
            for sec_idx in 0..objects[obj_idx].sections.len() {
                let relas = &objects[obj_idx].relocations[sec_idx];
                let has_abs64 = relas.iter().any(|r| r.rela_type == R_AARCH64_ABS64);
                if has_abs64 {
                    if let Some(&(out_idx, _)) = section_map.get(&(obj_idx, sec_idx)) {
                        needs_write.insert(out_idx);
                    }
                }
            }
        }
        for idx in needs_write {
            if output_sections[idx].flags & SHF_WRITE == 0
                && output_sections[idx].flags & SHF_EXECINSTR == 0
            {
                output_sections[idx].flags |= SHF_WRITE;
            }
        }
    }

    // Check if there are any pure-rodata (non-writable, non-executable) sections remaining
    let has_rodata = output_sections.iter().any(|s|
        s.flags & SHF_ALLOC != 0 && s.flags & SHF_EXECINSTR == 0 &&
        s.flags & SHF_WRITE == 0 && s.sh_type != SHT_NOBITS
    );
    // PHDR, LOAD R, LOAD RX, [LOAD R(rodata)], LOAD RW, DYNAMIC, GNU_STACK, [TLS]
    let mut phdr_count: u64 = 6; // base: PHDR + LOAD R + LOAD RX + LOAD RW + DYNAMIC + GNU_STACK
    if has_rodata { phdr_count += 1; }
    if has_tls_sections { phdr_count += 1; }
    let phdr_total_size = phdr_count * 56;

    // === Layout ===
    let mut offset = 64 + phdr_total_size;
    offset = (offset + 7) & !7;
    let gnu_hash_offset = offset; let gnu_hash_addr = base_addr + offset; offset += gnu_hash_size;
    offset = (offset + 7) & !7;
    let dynsym_offset = offset; let dynsym_addr = base_addr + offset; offset += dynsym_size;
    let dynstr_offset = offset; let dynstr_addr = base_addr + offset; offset += dynstr_size;

    // Text segment
    offset = (offset + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
    let text_page_offset = offset;
    let text_page_addr = base_addr + offset;
    for sec in output_sections.iter_mut() {
        if sec.flags & SHF_EXECINSTR != 0 && sec.flags & SHF_ALLOC != 0 {
            let a = sec.alignment.max(4);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = base_addr + offset; sec.file_offset = offset;
            offset += sec.mem_size;
        }
    }
    // PLT stubs (in text segment, after .text sections)
    let so_plt_offset: u64;
    let so_plt_addr: u64;
    if so_plt_size > 0 {
        offset = (offset + 15) & !15; // align to 16 bytes
        so_plt_offset = offset;
        so_plt_addr = base_addr + offset;
        offset += so_plt_size;
    } else {
        so_plt_offset = 0;
        so_plt_addr = 0;
    }
    let text_total_size = offset - text_page_offset;

    // Rodata
    offset = (offset + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
    let rodata_page_offset = offset;
    let rodata_page_addr = base_addr + offset;
    for sec in output_sections.iter_mut() {
        if sec.flags & SHF_ALLOC != 0 && sec.flags & SHF_EXECINSTR == 0 &&
           sec.flags & SHF_WRITE == 0 && sec.sh_type != SHT_NOBITS {
            let a = sec.alignment.max(1);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = base_addr + offset; sec.file_offset = offset;
            offset += sec.mem_size;
        }
    }
    let rodata_total_size = offset - rodata_page_offset;

    // RW segment
    offset = (offset + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
    let rw_page_offset = offset;
    let rw_page_addr = base_addr + offset;

    let mut init_array_addr_so = 0u64; let mut init_array_size_so = 0u64;
    let mut fini_array_addr_so = 0u64; let mut fini_array_size_so = 0u64;

    for sec in output_sections.iter_mut() {
        if sec.name == ".init_array" {
            let a = sec.alignment.max(8);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = base_addr + offset; sec.file_offset = offset;
            init_array_addr_so = sec.addr; init_array_size_so = sec.mem_size;
            offset += sec.mem_size; break;
        }
    }
    for sec in output_sections.iter_mut() {
        if sec.name == ".fini_array" {
            let a = sec.alignment.max(8);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = base_addr + offset; sec.file_offset = offset;
            fini_array_addr_so = sec.addr; fini_array_size_so = sec.mem_size;
            offset += sec.mem_size; break;
        }
    }

    // Reserve space for .rela.dyn (R_AARCH64_RELATIVE + R_AARCH64_GLOB_DAT entries)
    offset = (offset + 7) & !7;
    let rela_dyn_offset = offset;
    let rela_dyn_addr = base_addr + offset;
    let mut max_rela_count: usize = 0;
    // Count ABS64 relocations (become RELATIVE)
    for obj in objects.iter() {
        for sec_relas in &obj.relocations {
            for rela in sec_relas {
                if rela.rela_type == R_AARCH64_ABS64 {
                    max_rela_count += 1;
                }
            }
        }
    }
    for sec in output_sections.iter() {
        if sec.name == ".init_array" || sec.name == ".fini_array" {
            max_rela_count += (sec.mem_size / 8) as usize;
        }
    }
    // Pre-count GOT entries that will need dynamic relocations (RELATIVE or GLOB_DAT).
    // This must be done before layout to correctly reserve .rela.dyn space.
    {
        let mut got_pre_count: usize = 0;
        let mut got_pre_names: Vec<String> = Vec::new();
        for obj in objects.iter() {
            for sec_relas in &obj.relocations {
                for rela in sec_relas {
                    let si = rela.sym_idx as usize;
                    if si >= obj.symbols.len() { continue; }
                    let sym_name = &obj.symbols[si].name;
                    if sym_name.is_empty() { continue; }
                    let needs_got = match rela.rela_type {
                        R_AARCH64_ADR_GOT_PAGE | R_AARCH64_LD64_GOT_LO12_NC => true,
                        R_AARCH64_ADR_PREL_PG_HI21 | R_AARCH64_ADD_ABS_LO12_NC
                        | R_AARCH64_LDST64_ABS_LO12_NC | R_AARCH64_LDST32_ABS_LO12_NC
                        | R_AARCH64_LDST8_ABS_LO12_NC | R_AARCH64_LDST16_ABS_LO12_NC => {
                            if let Some(g) = globals.get(sym_name.as_str()) {
                                g.is_dynamic || g.defined_in.is_none()
                            } else {
                                obj.symbols[si].is_undefined() && !obj.symbols[si].is_local()
                            }
                        }
                        _ => false,
                    };
                    if needs_got && !got_pre_names.contains(sym_name) {
                        got_pre_names.push(sym_name.clone());
                        got_pre_count += 1;
                    }
                }
            }
        }
        max_rela_count += got_pre_count;
    }
    let rela_dyn_max_size = max_rela_count as u64 * 24;
    offset += rela_dyn_max_size;

    offset = (offset + 7) & !7;
    let dynamic_offset = offset; let dynamic_addr_so = base_addr + offset; offset += dynamic_size;

    // GOT.PLT for PLT symbols
    let so_got_plt_offset: u64;
    let so_got_plt_addr: u64;
    if so_got_plt_size > 0 {
        offset = (offset + 7) & !7;
        so_got_plt_offset = offset;
        so_got_plt_addr = base_addr + offset;
        offset += so_got_plt_size;
    } else {
        so_got_plt_offset = 0;
        so_got_plt_addr = 0;
    }

    // RELA.PLT for JUMP_SLOT relocations
    let so_rela_plt_offset: u64;
    let so_rela_plt_addr: u64;
    if so_rela_plt_size > 0 {
        offset = (offset + 7) & !7;
        so_rela_plt_offset = offset;
        so_rela_plt_addr = base_addr + offset;
        offset += so_rela_plt_size;
    } else {
        so_rela_plt_offset = 0;
        so_rela_plt_addr = 0;
    }

    // GOT for locally-resolved symbols AND undefined/dynamic symbols referenced
    // by ADRP/ADD pairs that need GOT indirection in shared libraries.
    let got_offset = offset; let got_addr = base_addr + offset;
    let mut got_needed: Vec<String> = Vec::new();
    for obj in objects.iter() {
        for sec_relas in &obj.relocations {
            for rela in sec_relas {
                let si = rela.sym_idx as usize;
                if si >= obj.symbols.len() { continue; }
                let sym = &obj.symbols[si];
                if sym.name.is_empty() { continue; }
                match rela.rela_type {
                    R_AARCH64_ADR_GOT_PAGE | R_AARCH64_LD64_GOT_LO12_NC => {
                        if !got_needed.contains(&sym.name) { got_needed.push(sym.name.clone()); }
                    }
                    // In shared libraries, ADRP/ADD for undefined/dynamic symbols must
                    // go through the GOT since the symbol address is only known at runtime.
                    R_AARCH64_ADR_PREL_PG_HI21 | R_AARCH64_ADD_ABS_LO12_NC
                    | R_AARCH64_LDST64_ABS_LO12_NC | R_AARCH64_LDST32_ABS_LO12_NC
                    | R_AARCH64_LDST8_ABS_LO12_NC | R_AARCH64_LDST16_ABS_LO12_NC => {
                        let sym_needs_got = if let Some(g) = globals.get(&sym.name) {
                            g.is_dynamic || g.defined_in.is_none()
                        } else {
                            sym.is_undefined() && !sym.is_local()
                        };
                        if sym_needs_got && !got_needed.contains(&sym.name) {
                            got_needed.push(sym.name.clone());
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    let got_size = got_needed.len() as u64 * 8;
    offset += got_size;

    for sec in output_sections.iter_mut() {
        if sec.flags & SHF_ALLOC != 0 && sec.flags & SHF_WRITE != 0 &&
           sec.sh_type != SHT_NOBITS && sec.name != ".init_array" && sec.name != ".fini_array" &&
           sec.flags & SHF_TLS == 0 {
            let a = sec.alignment.max(1);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = base_addr + offset; sec.file_offset = offset;
            offset += sec.mem_size;
        }
    }

    // TLS
    let mut tls_addr = 0u64;
    let mut tls_file_offset_so = 0u64;
    let mut tls_file_size = 0u64;
    let mut tls_mem_size = 0u64;
    let mut tls_align = 1u64;
    for sec in output_sections.iter_mut() {
        if sec.flags & SHF_TLS != 0 && sec.flags & SHF_ALLOC != 0 && sec.sh_type != SHT_NOBITS {
            let a = sec.alignment.max(1);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = base_addr + offset; sec.file_offset = offset;
            if tls_addr == 0 { tls_addr = sec.addr; tls_file_offset_so = offset; tls_align = a; }
            tls_file_size += sec.mem_size; tls_mem_size += sec.mem_size;
            offset += sec.mem_size;
        }
    }
    if tls_addr == 0 && has_tls_sections {
        tls_addr = base_addr + offset; tls_file_offset_so = offset;
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
    tls_mem_size = (tls_mem_size + tls_align - 1) & !(tls_align - 1);
    let has_tls = tls_addr != 0;

    let bss_addr = base_addr + offset;
    let mut bss_size = 0u64;
    for sec in output_sections.iter_mut() {
        if sec.sh_type == SHT_NOBITS && sec.flags & SHF_ALLOC != 0 && sec.flags & SHF_TLS == 0 {
            let a = sec.alignment.max(1);
            let aligned = (bss_addr + bss_size + a - 1) & !(a - 1);
            bss_size = aligned - bss_addr + sec.mem_size;
            sec.addr = aligned; sec.file_offset = offset;
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

    // Update globals
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

    // Linker-provided symbols for shared library
    let linker_addrs = LinkerSymbolAddresses {
        base_addr, got_addr, dynamic_addr: dynamic_addr_so,
        bss_addr, bss_size, text_end: text_page_addr + text_total_size,
        data_start: rw_page_addr,
        init_array_start: init_array_addr_so, init_array_size: init_array_size_so,
        fini_array_start: fini_array_addr_so, fini_array_size: fini_array_size_so,
        preinit_array_start: 0, preinit_array_size: 0,
        rela_iplt_start: 0, rela_iplt_size: 0,
    };
    for sym in &get_standard_linker_symbols(&linker_addrs) {
        let entry = globals.entry(sym.name.to_string()).or_insert(GlobalSymbol {
            value: 0, size: 0, info: (sym.binding << 4),
            defined_in: None, from_lib: None, plt_idx: None, got_idx: None,
            section_idx: SHN_ABS, is_dynamic: false, copy_reloc: false, lib_sym_value: 0,
        });
        if entry.defined_in.is_none() && !entry.is_dynamic {
            entry.value = sym.value;
            entry.defined_in = Some(usize::MAX);
            entry.section_idx = SHN_ABS;
        }
    }

    // Save RW segment file size before appending section headers
    let rw_end_offset = offset;

    // Section headers: null, .dynsym, .dynstr, .gnu.hash, .dynamic, .rela.dyn, .shstrtab
    // Build .shstrtab
    let mut shstrtab_data: Vec<u8> = vec![0]; // null byte at offset 0
    let shname_dynsym = shstrtab_data.len() as u32;
    shstrtab_data.extend_from_slice(b".dynsym\0");
    let shname_dynstr = shstrtab_data.len() as u32;
    shstrtab_data.extend_from_slice(b".dynstr\0");
    let shname_gnu_hash = shstrtab_data.len() as u32;
    shstrtab_data.extend_from_slice(b".gnu.hash\0");
    let shname_dynamic = shstrtab_data.len() as u32;
    shstrtab_data.extend_from_slice(b".dynamic\0");
    let shname_rela_dyn = shstrtab_data.len() as u32;
    shstrtab_data.extend_from_slice(b".rela.dyn\0");
    let shname_shstrtab = shstrtab_data.len() as u32;
    shstrtab_data.extend_from_slice(b".shstrtab\0");

    // Append .shstrtab data and section headers after file content
    offset = (offset + 7) & !7;
    let shstrtab_offset = offset;
    let shstrtab_size = shstrtab_data.len() as u64;
    offset += shstrtab_size;
    offset = (offset + 7) & !7;
    let shdr_offset = offset;
    let sh_count: u16 = 7; // null + .dynsym + .dynstr + .gnu.hash + .dynamic + .rela.dyn + .shstrtab
    let shdr_total = sh_count as u64 * 64;
    offset += shdr_total;

    // Build output buffer
    let file_size = offset as usize;
    let mut out = vec![0u8; file_size];

    // ELF header
    out[0..4].copy_from_slice(&ELF_MAGIC);
    out[4] = ELFCLASS64; out[5] = ELFDATA2LSB; out[6] = 1;
    w16(&mut out, 16, ET_DYN);
    w16(&mut out, 18, EM_AARCH64); w32(&mut out, 20, 1);
    w64(&mut out, 24, 0); // e_entry = 0
    w64(&mut out, 32, 64); // e_phoff
    w64(&mut out, 40, shdr_offset); // e_shoff
    w32(&mut out, 48, 0); w16(&mut out, 52, 64); w16(&mut out, 54, 56);
    w16(&mut out, 56, phdr_count as u16);
    w16(&mut out, 58, 64); // e_shentsize
    w16(&mut out, 60, sh_count); // e_shnum
    w16(&mut out, 62, (sh_count - 1) as u16); // e_shstrndx (last section)

    // Program headers
    let mut ph = 64usize;
    wphdr(&mut out, ph, PT_PHDR, PF_R, 64, base_addr + 64, phdr_total_size, phdr_total_size, 8); ph += 56;
    let ro_seg_end = dynstr_offset + dynstr_size;
    wphdr(&mut out, ph, PT_LOAD, PF_R, 0, base_addr, ro_seg_end, ro_seg_end, PAGE_SIZE); ph += 56;
    if text_total_size > 0 {
        wphdr(&mut out, ph, PT_LOAD, PF_R|PF_X, text_page_offset, text_page_addr, text_total_size, text_total_size, PAGE_SIZE); ph += 56;
    } else {
        wphdr(&mut out, ph, PT_LOAD, PF_R|PF_X, text_page_offset, text_page_addr, 0, 0, PAGE_SIZE); ph += 56;
    }
    if has_rodata {
        wphdr(&mut out, ph, PT_LOAD, PF_R, rodata_page_offset, rodata_page_addr, rodata_total_size, rodata_total_size, PAGE_SIZE); ph += 56;
    }
    let rw_filesz = rw_end_offset - rw_page_offset;
    let rw_memsz = if bss_size > 0 { (bss_addr + bss_size) - rw_page_addr } else { rw_filesz };
    wphdr(&mut out, ph, PT_LOAD, PF_R|PF_W, rw_page_offset, rw_page_addr, rw_filesz, rw_memsz, PAGE_SIZE); ph += 56;
    wphdr(&mut out, ph, PT_DYNAMIC, PF_R|PF_W, dynamic_offset, dynamic_addr_so, dynamic_size, dynamic_size, 8); ph += 56;
    wphdr(&mut out, ph, PT_GNU_STACK, PF_R|PF_W, 0, 0, 0, 0, 0x10); ph += 56;
    if has_tls {
        wphdr(&mut out, ph, PT_TLS, PF_R, tls_file_offset_so, tls_addr, tls_file_size, tls_mem_size, tls_align);
    }

    // .gnu.hash
    let gh = gnu_hash_offset as usize;
    w32(&mut out, gh, gnu_hash_nbuckets);
    w32(&mut out, gh+4, gnu_hash_symoffset as u32);
    w32(&mut out, gh+8, gnu_hash_bloom_size);
    w32(&mut out, gh+12, gnu_hash_bloom_shift);
    w64(&mut out, gh + 16, bloom_word);
    let buckets_off = gh + 16 + (gnu_hash_bloom_size as usize * 8);
    for (i, &b) in gnu_hash_buckets.iter().enumerate() {
        w32(&mut out, buckets_off + i * 4, b);
    }
    let chains_off = buckets_off + (gnu_hash_nbuckets as usize * 4);
    for (i, &c) in gnu_hash_chains.iter().enumerate() {
        w32(&mut out, chains_off + i * 4, c);
    }

    // .dynsym
    let mut ds = dynsym_offset as usize + 24;
    for name in &dyn_sym_names {
        let no = dynstr.get_offset(name) as u32;
        w32(&mut out, ds, no);
        if let Some(gsym) = globals.get(name) {
            if gsym.defined_in.is_some() && !gsym.is_dynamic && gsym.section_idx != SHN_UNDEF {
                if ds+5 < out.len() { out[ds+4] = gsym.info; out[ds+5] = 0; }
                w16(&mut out, ds+6, 1);
                w64(&mut out, ds+8, gsym.value);
                w64(&mut out, ds+16, gsym.size);
            } else {
                // Undefined/dynamic: use FUNC type for PLT symbols, NOTYPE for data
                let stype = if so_plt_names.contains(name) { STT_FUNC } else { 0u8 /* STT_NOTYPE */ };
                if ds+5 < out.len() { out[ds+4] = (STB_GLOBAL << 4) | stype; out[ds+5] = 0; }
                w16(&mut out, ds+6, 0); w64(&mut out, ds+8, 0); w64(&mut out, ds+16, 0);
            }
        } else {
            let stype = if so_plt_names.contains(name) { STT_FUNC } else { 0u8 /* STT_NOTYPE */ };
            if ds+5 < out.len() { out[ds+4] = (STB_GLOBAL << 4) | stype; out[ds+5] = 0; }
            w16(&mut out, ds+6, 0); w64(&mut out, ds+8, 0); w64(&mut out, ds+16, 0);
        }
        ds += 24;
    }

    // .dynstr
    write_bytes(&mut out, dynstr_offset as usize, dynstr.as_bytes());

    // Section data
    for sec in output_sections.iter() {
        if sec.sh_type == SHT_NOBITS || sec.data.is_empty() { continue; }
        write_bytes(&mut out, sec.file_offset as usize, &sec.data);
    }

    // GOT entries
    let mut got_sym_addrs: HashMap<String, u64> = HashMap::new();
    for (i, name) in got_needed.iter().enumerate() {
        let gea = got_addr + i as u64 * 8;
        got_sym_addrs.insert(name.clone(), gea);
        if let Some(gsym) = globals.get(name) {
            if gsym.defined_in.is_some() && !gsym.is_dynamic {
                w64(&mut out, (got_offset + i as u64 * 8) as usize, gsym.value);
            }
        }
    }
    // For PLT symbols referenced via GOT (ADR_GOT_PAGE/LD64_GOT_LO12_NC),
    // point to the GOT.PLT entry so the dynamic linker resolves them
    for (i, name) in so_plt_names.iter().enumerate() {
        let gea = so_got_plt_addr + 24 + i as u64 * 8;
        if !got_sym_addrs.contains_key(name) {
            got_sym_addrs.insert(name.clone(), gea);
        }
    }

    // Apply relocations and collect dynamic relocation entries
    let globals_snap: HashMap<String, GlobalSymbol> = globals.clone();
    // Each entry: (offset, r_info, addend)
    let mut rela_dyn_entries: Vec<(u64, u64, u64)> = Vec::new();
    const R_AARCH64_RELATIVE_DYN: u64 = 1027;
    const R_AARCH64_GLOB_DAT_DYN: u64 = 1025;

    // RELATIVE for locally-defined GOT entries, GLOB_DAT for undefined/dynamic GOT entries
    for (i, name) in got_needed.iter().enumerate() {
        let gea = got_addr + i as u64 * 8;
        if let Some(gsym) = globals_snap.get(name) {
            if gsym.defined_in.is_some() && !gsym.is_dynamic {
                rela_dyn_entries.push((gea, R_AARCH64_RELATIVE_DYN, gsym.value));
            } else {
                // Dynamic/undefined symbol: emit GLOB_DAT with the dynsym index
                let si = dyn_sym_names.iter().position(|n| n == name).map(|p| p + 1).unwrap_or(0) as u64;
                rela_dyn_entries.push((gea, (si << 32) | R_AARCH64_GLOB_DAT_DYN, 0));
            }
        } else {
            // Symbol not in globals - try to find in dynsym for GLOB_DAT
            let si = dyn_sym_names.iter().position(|n| n == name).map(|p| p + 1).unwrap_or(0) as u64;
            if si != 0 {
                rela_dyn_entries.push((gea, (si << 32) | R_AARCH64_GLOB_DAT_DYN, 0));
            }
        }
    }

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
                let s = reloc::resolve_sym(obj_idx, sym, &globals_snap, section_map, output_sections);

                match rela.rela_type {
                    R_AARCH64_ABS64 => {
                        let val = (s as i64 + a) as u64;
                        w64(&mut out, fp, val);
                        if s != 0 { rela_dyn_entries.push((p, R_AARCH64_RELATIVE_DYN, val)); }
                    }
                    R_AARCH64_ABS32 => { w32(&mut out, fp, (s as i64 + a) as u32); }
                    R_AARCH64_PREL64 => { w64(&mut out, fp, (s as i64 + a - p as i64) as u64); }
                    R_AARCH64_PREL32 | R_AARCH64_PREL16 => {
                        let val = (s as i64 + a - p as i64) as u32;
                        if rela.rela_type == R_AARCH64_PREL32 { w32(&mut out, fp, val); }
                        else { w16(&mut out, fp, val as u16); }
                    }
                    R_AARCH64_ADR_PREL_PG_HI21 => {
                        // For undefined/dynamic symbols in shared libs, redirect through GOT
                        if let Some(&gea) = got_sym_addrs.get(&sym.name) {
                            if s == 0 || globals_snap.get(&sym.name).map_or(false, |g| g.is_dynamic || g.defined_in.is_none()) {
                                let page_g = gea & !0xFFF;
                                let page_p = p & !0xFFF;
                                let imm = (page_g as i64 - page_p as i64) >> 12;
                                reloc::encode_adrp(&mut out, fp, imm);
                            } else {
                                let sa_val = (s as i64 + a) as u64;
                                let page_s = sa_val & !0xFFF;
                                let page_p = p & !0xFFF;
                                let imm = (page_s as i64 - page_p as i64) >> 12;
                                reloc::encode_adrp(&mut out, fp, imm);
                            }
                        } else {
                            let sa_val = (s as i64 + a) as u64;
                            let page_s = sa_val & !0xFFF;
                            let page_p = p & !0xFFF;
                            let imm = (page_s as i64 - page_p as i64) >> 12;
                            reloc::encode_adrp(&mut out, fp, imm);
                        }
                    }
                    R_AARCH64_ADD_ABS_LO12_NC => {
                        // For undefined/dynamic symbols in shared libs, convert ADD to LDR from GOT
                        if let Some(&gea) = got_sym_addrs.get(&sym.name) {
                            if s == 0 || globals_snap.get(&sym.name).map_or(false, |g| g.is_dynamic || g.defined_in.is_none()) {
                                // Convert: ADD Xd, Xn, #imm -> LDR Xd, [Xn, #imm]
                                // The ADD instruction loaded address = base + lo12
                                // We need LDR to dereference the GOT entry instead
                                let lo12 = (gea & 0xFFF) as u32;
                                if fp + 4 <= out.len() {
                                    let insn = read_u32(&out, fp);
                                    let rd = insn & 0x1f;
                                    let rn = (insn >> 5) & 0x1f;
                                    // LDR Xd, [Xn, #imm] = 0xF9400000 | (imm/8 << 10) | (Rn << 5) | Rd
                                    let ldr = 0xf9400000u32 | ((lo12 / 8) << 10) | (rn << 5) | rd;
                                    w32(&mut out, fp, ldr);
                                }
                            } else {
                                let sa_val = (s as i64 + a) as u64;
                                reloc::encode_add_imm12(&mut out, fp, (sa_val & 0xFFF) as u32);
                            }
                        } else {
                            let sa_val = (s as i64 + a) as u64;
                            reloc::encode_add_imm12(&mut out, fp, (sa_val & 0xFFF) as u32);
                        }
                    }
                    R_AARCH64_CALL26 | R_AARCH64_JUMP26 => {
                        if fp + 4 > out.len() { continue; }
                        let mut target = (s as i64 + a) as u64;
                        // If the symbol has a PLT entry, redirect to it
                        if target == 0 && !sym.name.is_empty() {
                            if let Some(g) = globals_snap.get(&sym.name) {
                                if let Some(pi) = g.plt_idx {
                                    target = so_plt_addr + 32 + pi as u64 * 16;
                                }
                            }
                        }
                        if target == 0 {
                            // Weak undefined with no PLT - NOP it
                            w32(&mut out, fp, 0xd503201f);
                        } else {
                            let offset_val = target as i64 - p as i64;
                            let mut insn = read_u32(&out, fp);
                            let imm26 = ((offset_val >> 2) as u32) & 0x3ffffff;
                            insn = (insn & 0xfc000000) | imm26;
                            w32(&mut out, fp, insn);
                        }
                    }
                    R_AARCH64_LDST8_ABS_LO12_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_ldst_imm12(&mut out, fp, (sa_val & 0xFFF) as u32, 0);
                    }
                    R_AARCH64_LDST16_ABS_LO12_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_ldst_imm12(&mut out, fp, (sa_val & 0xFFF) as u32, 1);
                    }
                    R_AARCH64_LDST32_ABS_LO12_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_ldst_imm12(&mut out, fp, (sa_val & 0xFFF) as u32, 2);
                    }
                    R_AARCH64_LDST64_ABS_LO12_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_ldst_imm12(&mut out, fp, (sa_val & 0xFFF) as u32, 3);
                    }
                    R_AARCH64_LDST128_ABS_LO12_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_ldst_imm12(&mut out, fp, (sa_val & 0xFFF) as u32, 4);
                    }
                    R_AARCH64_ADR_GOT_PAGE => {
                        if let Some(&gea) = got_sym_addrs.get(&sym.name) {
                            let page_g = gea & !0xFFF;
                            let page_p = p & !0xFFF;
                            let imm = (page_g as i64 - page_p as i64) >> 12;
                            reloc::encode_adrp(&mut out, fp, imm);
                        } else {
                            let sa_val = (s as i64 + a) as u64;
                            let page_s = sa_val & !0xFFF;
                            let page_p = p & !0xFFF;
                            let imm = (page_s as i64 - page_p as i64) >> 12;
                            reloc::encode_adrp(&mut out, fp, imm);
                        }
                    }
                    R_AARCH64_LD64_GOT_LO12_NC => {
                        if let Some(&gea) = got_sym_addrs.get(&sym.name) {
                            reloc::encode_ldst_imm12(&mut out, fp, (gea & 0xFFF) as u32, 3);
                        } else {
                            let sa_val = (s as i64 + a) as u64;
                            reloc::encode_ldst_imm12(&mut out, fp, (sa_val & 0xFFF) as u32, 3);
                        }
                    }
                    R_AARCH64_MOVW_UABS_G0 | R_AARCH64_MOVW_UABS_G0_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_movw(&mut out, fp, (sa_val & 0xffff) as u32);
                    }
                    R_AARCH64_MOVW_UABS_G1_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_movw(&mut out, fp, ((sa_val >> 16) & 0xffff) as u32);
                    }
                    R_AARCH64_MOVW_UABS_G2_NC => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_movw(&mut out, fp, ((sa_val >> 32) & 0xffff) as u32);
                    }
                    R_AARCH64_MOVW_UABS_G3 => {
                        let sa_val = (s as i64 + a) as u64;
                        reloc::encode_movw(&mut out, fp, ((sa_val >> 48) & 0xffff) as u32);
                    }
                    R_AARCH64_NONE => {}
                    other => {
                        eprintln!("warning: unsupported relocation type {} for '{}' in shared library", other, sym.name);
                    }
                }
            }
        }
    }

    // Write .rela.dyn (RELATIVE + GLOB_DAT entries)
    // Sort: put RELATIVE entries first (for DT_RELACOUNT), then GLOB_DAT
    let relative_count = rela_dyn_entries.iter().filter(|(_, info, _)| *info == R_AARCH64_RELATIVE_DYN).count();
    rela_dyn_entries.sort_by_key(|(_, info, _)| if *info == R_AARCH64_RELATIVE_DYN { 0u8 } else { 1u8 });
    let actual_rela_count = rela_dyn_entries.len();
    let rela_dyn_size = actual_rela_count as u64 * 24;
    let mut rd = rela_dyn_offset as usize;
    for (rel_offset, rel_info, rel_addend) in &rela_dyn_entries {
        if rd + 24 <= out.len() {
            w64(&mut out, rd, *rel_offset);
            w64(&mut out, rd+8, *rel_info);
            w64(&mut out, rd+16, *rel_addend);
            rd += 24;
        }
    }

    // .plt stubs (AArch64 PLT stubs for shared library)
    if so_plt_size > 0 {
        let po = so_plt_offset as usize;
        // PLT header (32 bytes)
        let got2_addr = so_got_plt_addr + 16;
        let page_g = got2_addr & !0xFFF;
        let page_p = so_plt_addr & !0xFFF;
        let page_diff = ((page_g as i64 - page_p as i64) >> 12) as i32;
        let immlo = (page_diff & 3) as u32;
        let immhi = ((page_diff >> 2) & 0x7ffff) as u32;
        w32(&mut out, po, 0xa9bf7bf0u32); // stp x16, x30, [sp, #-16]!
        w32(&mut out, po + 4, 0x90000010 | (immlo << 29) | (immhi << 5)); // adrp x16, GOT+16
        let lo12 = (got2_addr & 0xFFF) as u32;
        w32(&mut out, po + 8, 0xf9400211 | ((lo12 / 8) << 10)); // ldr x17, [x16, #lo12]
        w32(&mut out, po + 12, 0x91000210 | ((lo12 & 0xFFF) << 10)); // add x16, x16, #lo12
        w32(&mut out, po + 16, 0xd61f0220u32); // br x17
        w32(&mut out, po + 20, 0xd503201fu32); // nop
        w32(&mut out, po + 24, 0xd503201fu32); // nop
        w32(&mut out, po + 28, 0xd503201fu32); // nop

        // Individual PLT entries (16 bytes each)
        for (i, _) in so_plt_names.iter().enumerate() {
            let ep = po + 32 + i * 16;
            let pea = so_plt_addr + 32 + i as u64 * 16;
            let gea = so_got_plt_addr + 24 + i as u64 * 8;
            let page_g = gea & !0xFFF;
            let page_p = pea & !0xFFF;
            let page_diff = ((page_g as i64 - page_p as i64) >> 12) as i32;
            let immlo = (page_diff & 3) as u32;
            let immhi = ((page_diff >> 2) & 0x7ffff) as u32;
            w32(&mut out, ep, 0x90000010 | (immlo << 29) | (immhi << 5)); // adrp x16
            let lo12 = (gea & 0xFFF) as u32;
            w32(&mut out, ep + 4, 0xf9400211 | ((lo12 / 8) << 10)); // ldr x17, [x16, #lo12]
            w32(&mut out, ep + 8, 0x91000210 | ((lo12 & 0xFFF) << 10)); // add x16, x16, #lo12
            w32(&mut out, ep + 12, 0xd61f0220u32); // br x17
        }
    }

    // GOT.PLT entries
    if so_got_plt_size > 0 {
        let gp = so_got_plt_offset as usize;
        w64(&mut out, gp, dynamic_addr_so); // GOT[0] = _DYNAMIC
        // GOT[1] and GOT[2] are filled by the dynamic linker
        // GOT[3..] are PLT GOT entries: initialized to PLT[0] (resolved eagerly via DF_BIND_NOW)
        for i in 0..so_plt_names.len() {
            w64(&mut out, gp + 24 + i * 8, so_plt_addr);
        }
    }

    // .rela.plt (R_AARCH64_JUMP_SLOT)
    const R_AARCH64_JUMP_SLOT: u64 = 1026;
    if so_rela_plt_size > 0 {
        let mut rp = so_rela_plt_offset as usize;
        for (i, name) in so_plt_names.iter().enumerate() {
            let gea = so_got_plt_addr + 24 + i as u64 * 8;
            // Find the dynsym index for this symbol
            let si = dyn_sym_names.iter().position(|n| n == name).map(|p| p + 1).unwrap_or(0) as u64;
            w64(&mut out, rp, gea);
            w64(&mut out, rp + 8, (si << 32) | R_AARCH64_JUMP_SLOT);
            w64(&mut out, rp + 16, 0);
            rp += 24;
        }
    }

    // .dynamic
    let mut dd = dynamic_offset as usize;
    for lib in needed_sonames {
        let so = dynstr.get_offset(lib);
        w64(&mut out, dd, DT_NEEDED as u64); w64(&mut out, dd+8, so as u64); dd += 16;
    }
    if let Some(ref sn) = soname {
        let so = dynstr.get_offset(sn);
        w64(&mut out, dd, DT_SONAME as u64); w64(&mut out, dd+8, so as u64); dd += 16;
    }
    for &(tag, val) in &[
        (DT_STRTAB, dynstr_addr), (DT_SYMTAB, dynsym_addr), (DT_STRSZ, dynstr_size),
        (DT_SYMENT, 24),
        (DT_RELA, rela_dyn_addr), (DT_RELASZ, rela_dyn_size), (DT_RELAENT, 24),
        (DT_RELACOUNT as i64, relative_count as u64),
        (DT_GNU_HASH, gnu_hash_addr),
    ] {
        w64(&mut out, dd, tag as u64); w64(&mut out, dd+8, val); dd += 16;
    }
    if !so_plt_names.is_empty() {
        for &(tag, val) in &[
            (DT_PLTGOT, so_got_plt_addr), (DT_PLTRELSZ, so_rela_plt_size),
            (DT_PLTREL, 7u64), (DT_JMPREL, so_rela_plt_addr),
        ] {
            w64(&mut out, dd, tag as u64); w64(&mut out, dd+8, val); dd += 16;
        }
    }
    if has_init_array {
        w64(&mut out, dd, DT_INIT_ARRAY as u64); w64(&mut out, dd+8, init_array_addr_so); dd += 16;
        w64(&mut out, dd, DT_INIT_ARRAYSZ as u64); w64(&mut out, dd+8, init_array_size_so); dd += 16;
    }
    if has_fini_array {
        w64(&mut out, dd, DT_FINI_ARRAY as u64); w64(&mut out, dd+8, fini_array_addr_so); dd += 16;
        w64(&mut out, dd, DT_FINI_ARRAYSZ as u64); w64(&mut out, dd+8, fini_array_size_so); dd += 16;
    }
    // Force eager binding so GOT.PLT entries are resolved before execution
    w64(&mut out, dd, DT_FLAGS as u64); w64(&mut out, dd+8, DF_BIND_NOW as u64); dd += 16;
    w64(&mut out, dd, DT_FLAGS_1 as u64); w64(&mut out, dd+8, DF_1_NOW as u64); dd += 16;
    w64(&mut out, dd, DT_NULL as u64); w64(&mut out, dd+8, 0);

    // Write .shstrtab
    write_bytes(&mut out, shstrtab_offset as usize, &shstrtab_data);

    // Write section headers (64 bytes each for ELF64)
    // Helper to write one section header
    let mut sh = shdr_offset as usize;
    // [0] SHT_NULL
    // (already zeroed)
    sh += 64;
    // [1] .dynsym (SHT_DYNSYM = 11)
    w32(&mut out, sh, shname_dynsym);
    w32(&mut out, sh+4, 11); // SHT_DYNSYM
    w64(&mut out, sh+8, 0x2); // SHF_ALLOC
    w64(&mut out, sh+16, dynsym_addr); // sh_addr
    w64(&mut out, sh+24, dynsym_offset); // sh_offset
    w64(&mut out, sh+32, dynsym_size); // sh_size
    w32(&mut out, sh+40, 2); // sh_link = .dynstr index
    w32(&mut out, sh+44, 1); // sh_info = 1 (one local sym: null)
    w64(&mut out, sh+48, 8); // sh_addralign
    w64(&mut out, sh+56, 24); // sh_entsize
    sh += 64;
    // [2] .dynstr (SHT_STRTAB = 3)
    w32(&mut out, sh, shname_dynstr);
    w32(&mut out, sh+4, 3); // SHT_STRTAB
    w64(&mut out, sh+8, 0x2); // SHF_ALLOC
    w64(&mut out, sh+16, dynstr_addr);
    w64(&mut out, sh+24, dynstr_offset);
    w64(&mut out, sh+32, dynstr_size);
    w64(&mut out, sh+48, 1); // sh_addralign
    sh += 64;
    // [3] .gnu.hash (SHT_GNU_HASH = 0x6ffffff6)
    w32(&mut out, sh, shname_gnu_hash);
    w32(&mut out, sh+4, 0x6ffffff6u32); // SHT_GNU_HASH
    w64(&mut out, sh+8, 0x2); // SHF_ALLOC
    w64(&mut out, sh+16, gnu_hash_addr);
    w64(&mut out, sh+24, gnu_hash_offset);
    w64(&mut out, sh+32, gnu_hash_size);
    w32(&mut out, sh+40, 1); // sh_link = .dynsym index
    w64(&mut out, sh+48, 8);
    sh += 64;
    // [4] .dynamic (SHT_DYNAMIC = 6)
    w32(&mut out, sh, shname_dynamic);
    w32(&mut out, sh+4, 6); // SHT_DYNAMIC
    w64(&mut out, sh+8, 0x3); // SHF_WRITE | SHF_ALLOC
    w64(&mut out, sh+16, dynamic_addr_so);
    w64(&mut out, sh+24, dynamic_offset);
    w64(&mut out, sh+32, dynamic_size);
    w32(&mut out, sh+40, 2); // sh_link = .dynstr index
    w64(&mut out, sh+48, 8);
    w64(&mut out, sh+56, 16); // sh_entsize
    sh += 64;
    // [5] .rela.dyn (SHT_RELA = 4)
    w32(&mut out, sh, shname_rela_dyn);
    w32(&mut out, sh+4, 4); // SHT_RELA
    w64(&mut out, sh+8, 0x2); // SHF_ALLOC
    w64(&mut out, sh+16, rela_dyn_addr);
    w64(&mut out, sh+24, rela_dyn_offset);
    w64(&mut out, sh+32, rela_dyn_size);
    w32(&mut out, sh+40, 1); // sh_link = .dynsym index
    w64(&mut out, sh+48, 8);
    w64(&mut out, sh+56, 24); // sh_entsize
    sh += 64;
    // [6] .shstrtab (SHT_STRTAB = 3)
    w32(&mut out, sh, shname_shstrtab);
    w32(&mut out, sh+4, 3); // SHT_STRTAB
    w64(&mut out, sh+24, shstrtab_offset);
    w64(&mut out, sh+32, shstrtab_size);
    w64(&mut out, sh+48, 1);

    std::fs::write(output_path, &out).map_err(|e| format!("failed to write '{}': {}", output_path, e))?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let _ = std::fs::set_permissions(output_path, std::fs::Permissions::from_mode(0o755));
    }
    Ok(())
}

// ── Static ELF emission ───────────────────────────────────────────────

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
        dynamic_addr: 0, // No .dynamic in static mode (dynamic executables use emit_dynamic_executable)
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
                from_lib: None, plt_idx: None, got_idx: None,
                is_dynamic: false, copy_reloc: false, lib_sym_value: 0,
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
                from_lib: None, plt_idx: None, got_idx: None,
                is_dynamic: false, copy_reloc: false, lib_sym_value: 0,
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
