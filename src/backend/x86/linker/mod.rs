/// Native x86-64 ELF linker.
///
/// Links ELF relocatable object files (.o) and static archives (.a) into
/// a dynamically-linked ELF executable. Resolves undefined symbols against
/// shared libraries (e.g., libc.so.6) and generates PLT/GOT entries for
/// dynamic function calls.
#[allow(dead_code)]
pub mod elf;

use std::collections::{HashMap, HashSet, BTreeSet, VecDeque};
use std::path::Path;

use elf::*;

use crate::backend::linker_common::{self, Elf64Symbol, GlobalSymbolOps, OutputSection};

/// Base virtual address for the executable (standard non-PIE x86-64 address)
const BASE_ADDR: u64 = 0x400000;
/// Page size for alignment
const PAGE_SIZE: u64 = 0x1000;
/// Dynamic linker path
const INTERP: &[u8] = b"/lib64/ld-linux-x86-64.so.2\0";

/// A resolved global symbol.
///
/// This struct has x86-specific dynamic linking fields (plt_idx, got_idx,
/// copy_reloc, from_lib, version, lib_sym_value) in addition to the common
/// fields needed by the shared linker infrastructure.
#[derive(Clone)]
struct GlobalSymbol {
    value: u64,
    size: u64,
    info: u8,
    defined_in: Option<usize>,
    from_lib: Option<String>,
    plt_idx: Option<usize>,
    got_idx: Option<usize>,
    section_idx: u16,
    is_dynamic: bool,
    copy_reloc: bool,
    lib_sym_value: u64,
    version: Option<String>,
}

impl GlobalSymbolOps for GlobalSymbol {
    fn is_defined(&self) -> bool { self.defined_in.is_some() }
    fn is_dynamic(&self) -> bool { self.is_dynamic }
    fn info(&self) -> u8 { self.info }
    fn section_idx(&self) -> u16 { self.section_idx }
    fn value(&self) -> u64 { self.value }
    fn size(&self) -> u64 { self.size }
    fn new_defined(obj_idx: usize, sym: &Elf64Symbol) -> Self {
        GlobalSymbol {
            value: sym.value, size: sym.size, info: sym.info,
            defined_in: Some(obj_idx), from_lib: None,
            plt_idx: None, got_idx: None,
            section_idx: sym.shndx, is_dynamic: false, copy_reloc: false,
            lib_sym_value: 0, version: None,
        }
    }
    fn new_common(obj_idx: usize, sym: &Elf64Symbol) -> Self {
        GlobalSymbol {
            value: sym.value, size: sym.size, info: sym.info,
            defined_in: Some(obj_idx), from_lib: None,
            plt_idx: None, got_idx: None,
            section_idx: SHN_COMMON, is_dynamic: false, copy_reloc: false,
            lib_sym_value: 0, version: None,
        }
    }
    fn new_undefined(sym: &Elf64Symbol) -> Self {
        GlobalSymbol {
            value: 0, size: 0, info: sym.info,
            defined_in: None, from_lib: None,
            plt_idx: None, got_idx: None,
            section_idx: SHN_UNDEF, is_dynamic: false, copy_reloc: false,
            lib_sym_value: 0, version: None,
        }
    }
    fn set_common_bss(&mut self, bss_offset: u64) {
        self.value = bss_offset;
        self.section_idx = 0xffff;
    }
    fn new_dynamic(dsym: &linker_common::DynSymbol, soname: &str) -> Self {
        GlobalSymbol {
            value: 0, size: dsym.size, info: dsym.info,
            defined_in: None, from_lib: Some(soname.to_string()),
            plt_idx: None, got_idx: None,
            section_idx: SHN_UNDEF, is_dynamic: true, copy_reloc: false,
            lib_sym_value: dsym.value, version: dsym.version.clone(),
        }
    }
}

/// For x86, a dynamic definition should be replaced by a static definition.
fn x86_should_replace_extra(existing: &GlobalSymbol) -> bool {
    existing.is_dynamic
}

use linker_common::DynStrTab;

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
    let is_static = user_args.iter().any(|a| a == "-static");
    let mut objects: Vec<ElfObject> = Vec::new();
    let mut globals: HashMap<String, GlobalSymbol> = HashMap::new();
    let mut needed_sonames: Vec<String> = Vec::new();
    let lib_path_strings: Vec<String> = lib_paths.iter().map(|s| s.to_string()).collect();

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

    // Parse user args using shared infrastructure
    let parsed_args = linker_common::parse_linker_args(user_args);
    let extra_lib_paths = parsed_args.extra_lib_paths;
    let libs_to_load = parsed_args.libs_to_load;
    let extra_object_files = parsed_args.extra_object_files;
    let export_dynamic = parsed_args.export_dynamic;
    let rpath_entries = parsed_args.rpath_entries;
    let use_runpath = parsed_args.use_runpath;
    let defsym_defs = parsed_args.defsym_defs;
    let gc_sections = parsed_args.gc_sections;

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
            if let Some(lib_path) = linker_common::resolve_lib(lib_name, &all_lib_paths, is_static) {
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

    // Resolve remaining undefined symbols from default system libraries
    // (only when dynamically linking)
    if !is_static {
        let default_libs = ["libc.so.6", "libm.so.6", "libgcc_s.so.1"];
        linker_common::resolve_dynamic_symbols_elf64(
            &mut globals, &mut needed_sonames, &all_lib_paths, &default_libs,
        )?;
    }

    // Apply --defsym definitions: alias one symbol to another
    for (alias, target) in &defsym_defs {
        if let Some(target_sym) = globals.get(target).cloned() {
            globals.insert(alias.clone(), target_sym);
        }
    }

    // Garbage-collect unreferenced sections when --gc-sections is active.
    // This removes sections not reachable from entry points, which may also
    // eliminate undefined symbol references from dead code.
    let dead_sections: HashSet<(usize, usize)> = if gc_sections {
        gc_collect_sections(&objects)
    } else {
        HashSet::new()
    };

    // When gc-sections is active, remove globals that only exist in dead sections
    // and also remove references from dead sections
    if gc_sections {
        // Build set of symbols referenced only from dead sections
        let mut referenced_from_live: HashSet<String> = HashSet::new();
        for (obj_idx, obj) in objects.iter().enumerate() {
            for (sec_idx, relas) in obj.relocations.iter().enumerate() {
                if dead_sections.contains(&(obj_idx, sec_idx)) { continue; }
                for rela in relas {
                    if (rela.sym_idx as usize) < obj.symbols.len() {
                        let sym = &obj.symbols[rela.sym_idx as usize];
                        if !sym.name.is_empty() {
                            referenced_from_live.insert(sym.name.clone());
                        }
                    }
                }
            }
        }
        // Remove undefined globals that are only referenced from dead sections
        globals.retain(|name, sym| {
            // Keep defined symbols, dynamic symbols, weak symbols, and those referenced from live code
            sym.defined_in.is_some() || sym.is_dynamic
                || (sym.info >> 4) == STB_WEAK
                || referenced_from_live.contains(name)
        });
    }

    // Check for truly undefined (non-weak, non-dynamic, non-linker-defined) symbols
    linker_common::check_undefined_symbols_elf64(&globals, 20)?;

    // Merge sections (skip dead sections when gc-sections is active)
    let mut output_sections: Vec<OutputSection> = Vec::new();
    let mut section_map: HashMap<(usize, usize), (usize, u64)> = HashMap::new();
    linker_common::merge_sections_elf64_gc(&objects, &mut output_sections, &mut section_map, &dead_sections);

    // Allocate COMMON symbols
    linker_common::allocate_common_symbols_elf64(&mut globals, &mut output_sections);

    // Create PLT/GOT
    let (plt_names, got_entries) = create_plt_got(&objects, &mut globals);

    // Collect IFUNC symbols for static linking
    let ifunc_symbols = collect_ifunc_symbols(&globals, is_static);

    // Emit executable
    emit_executable(
        &objects, &mut globals, &mut output_sections, &section_map,
        &plt_names, &got_entries, &needed_sonames, output_path,
        export_dynamic, &rpath_entries, use_runpath, is_static,
        &ifunc_symbols,
    )
}

/// Create a shared library (.so) from object files.
///
/// Produces an ELF `ET_DYN` file with position-independent base address (0),
/// exporting all defined global symbols. Used when the compiler is invoked
/// with `-shared`.
pub fn link_shared(
    object_files: &[&str],
    output_path: &str,
    user_args: &[String],
    lib_paths: &[&str],
    needed_libs: &[&str],
) -> Result<(), String> {
    let mut objects: Vec<ElfObject> = Vec::new();
    let mut globals: HashMap<String, GlobalSymbol> = HashMap::new();
    let mut needed_sonames: Vec<String> = Vec::new();
    let lib_path_strings: Vec<String> = lib_paths.iter().map(|s| s.to_string()).collect();

    // Parse user args for -L, -l, -Wl,-soname=, bare .o/.a files
    let mut extra_lib_paths: Vec<String> = Vec::new();
    let mut libs_to_load: Vec<String> = Vec::new();
    let mut extra_object_files: Vec<String> = Vec::new();
    let mut soname: Option<String> = None;
    let mut rpath_entries: Vec<String> = Vec::new();
    let mut use_runpath = false; // --enable-new-dtags -> DT_RUNPATH instead of DT_RPATH
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
            let mut j = 0;
            while j < parts.len() {
                let part = parts[j];
                if let Some(sn) = part.strip_prefix("-soname=") {
                    soname = Some(sn.to_string());
                } else if part == "-soname" && j + 1 < parts.len() {
                    j += 1;
                    soname = Some(parts[j].to_string());
                } else if let Some(rp) = part.strip_prefix("-rpath=") {
                    rpath_entries.push(rp.to_string());
                } else if part == "-rpath" && j + 1 < parts.len() {
                    j += 1;
                    rpath_entries.push(parts[j].to_string());
                } else if part == "--enable-new-dtags" {
                    use_runpath = true;
                } else if part == "--disable-new-dtags" {
                    use_runpath = false;
                } else if let Some(lpath) = part.strip_prefix("-L") {
                    extra_lib_paths.push(lpath.to_string());
                } else if let Some(lib) = part.strip_prefix("-l") {
                    libs_to_load.push(lib.to_string());
                }
                j += 1;
            }
        } else if arg == "-shared" || arg == "-nostdlib" || arg == "-o" {
            if arg == "-o" { i += 1; } // skip output path
        } else if !arg.starts_with('-') && Path::new(arg).exists() {
            extra_object_files.push(arg.to_string());
        }
        i += 1;
    }

    // Load user object files
    for path in object_files {
        load_file(path, &mut objects, &mut globals, &mut needed_sonames, &lib_path_strings)?;
    }

    // Load extra object/archive files from user args
    for path in &extra_object_files {
        load_file(path, &mut objects, &mut globals, &mut needed_sonames, &lib_path_strings)?;
    }

    let mut all_lib_paths: Vec<String> = extra_lib_paths;
    all_lib_paths.extend(lib_path_strings.iter().cloned());

    // Resolve -l libraries
    if !libs_to_load.is_empty() {
        let mut lib_paths_resolved: Vec<String> = Vec::new();
        for lib_name in &libs_to_load {
            if let Some(lib_path) = linker_common::resolve_lib(lib_name, &all_lib_paths, false) {
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
                load_file(lib_path, &mut objects, &mut globals, &mut needed_sonames, &all_lib_paths)?;
            }
            if objects.len() != prev_count { changed = true; }
        }
    }

    // Resolve implicit libraries (e.g. libgcc.a) to provide compiler runtime
    // functions like __udivti3 that may be referenced by user code.
    if !needed_libs.is_empty() {
        let mut implicit_paths: Vec<String> = Vec::new();
        for lib_name in needed_libs {
            if let Some(lib_path) = linker_common::resolve_lib(lib_name, &all_lib_paths, false) {
                if !implicit_paths.contains(&lib_path) {
                    implicit_paths.push(lib_path);
                }
            }
        }
        let mut changed = true;
        while changed {
            changed = false;
            let prev_count = objects.len();
            for lib_path in &implicit_paths {
                load_file(lib_path, &mut objects, &mut globals, &mut needed_sonames, &all_lib_paths)?;
            }
            if objects.len() != prev_count { changed = true; }
        }
    }

    // Resolve remaining undefined symbols against system libraries (libc, libm,
    // libgcc_s) and add DT_NEEDED entries for any that provide matched symbols.
    let default_libs = ["libc.so.6", "libm.so.6", "libgcc_s.so.1"];
    linker_common::resolve_dynamic_symbols_elf64(
        &mut globals, &mut needed_sonames, &all_lib_paths, &default_libs,
    )?;

    // Merge sections (no gc-sections for shared libraries)
    let mut output_sections: Vec<OutputSection> = Vec::new();
    let mut section_map: HashMap<(usize, usize), (usize, u64)> = HashMap::new();
    linker_common::merge_sections_elf64(&objects, &mut output_sections, &mut section_map);

    // Allocate COMMON symbols
    linker_common::allocate_common_symbols_elf64(&mut globals, &mut output_sections);

    // Emit shared library
    emit_shared_library(
        &objects, &mut globals, &mut output_sections, &section_map,
        &needed_sonames, output_path, soname, &rpath_entries, use_runpath,
    )
}

/// Emit a shared library (.so) ELF file.
///
/// Key differences from emit_executable:
/// - ELF type is ET_DYN (not ET_EXEC)
/// - Base address is 0 (position-independent)
/// - No PT_INTERP segment
/// - All defined global symbols exported to .dynsym
/// - R_X86_64_RELATIVE relocations for internal absolute addresses
#[allow(clippy::too_many_arguments)]
fn emit_shared_library(
    objects: &[ElfObject], globals: &mut HashMap<String, GlobalSymbol>,
    output_sections: &mut [OutputSection],
    section_map: &HashMap<(usize, usize), (usize, u64)>,
    needed_sonames: &[String], output_path: &str,
    soname: Option<String>, rpath_entries: &[String], use_runpath: bool,
) -> Result<(), String> {
    let base_addr: u64 = 0;

    let mut dynstr = DynStrTab::new();
    for lib in needed_sonames { dynstr.add(lib); }
    if let Some(ref sn) = soname { dynstr.add(sn); }
    let rpath_string = if rpath_entries.is_empty() { None } else {
        let s = rpath_entries.join(":");
        dynstr.add(&s);
        Some(s)
    };

    // Identify symbols that need PLT entries: any symbol referenced via
    // R_X86_64_PLT32 or R_X86_64_PC32 that is not defined locally.
    // In shared libraries, undefined symbols are resolved at runtime by the
    // dynamic linker, so we need PLT entries for all of them.
    let mut plt_names: Vec<String> = Vec::new();
    for obj in objects.iter() {
        for sec_relas in &obj.relocations {
            for rela in sec_relas {
                let si = rela.sym_idx as usize;
                if si >= obj.symbols.len() { continue; }
                let sym = &obj.symbols[si];
                if sym.name.is_empty() { continue; }
                // Skip local symbols - they don't need PLT entries
                if sym.is_local() { continue; }
                match rela.rela_type {
                    R_X86_64_PLT32 | R_X86_64_PC32 => {
                        if let Some(gsym) = globals.get(&sym.name) {
                            // Need PLT for: dynamic symbols from shared libs,
                            // or undefined symbols not defined in any loaded object
                            let needs_plt = gsym.is_dynamic
                                || (gsym.defined_in.is_none() && gsym.section_idx == SHN_UNDEF);
                            if needs_plt && !plt_names.contains(&sym.name) {
                                plt_names.push(sym.name.clone());
                            }
                        }
                        // Don't create PLT for symbols not in globals - they are
                        // local/section symbols resolved directly
                    }
                    _ => {}
                }
            }
        }
    }

    // Ensure PLT symbols that are not yet in globals get entries (e.g. libc symbols
    // when libc is not explicitly linked). Create global entries for them so they
    // appear in dynsym and can be resolved by the dynamic linker at runtime.
    for name in &plt_names {
        if !globals.contains_key(name) {
            globals.insert(name.clone(), GlobalSymbol {
                value: 0, size: 0, info: (STB_GLOBAL << 4) | STT_FUNC,
                defined_in: None, from_lib: None, section_idx: SHN_UNDEF,
                is_dynamic: true, copy_reloc: false, lib_sym_value: 0, version: None,
                plt_idx: None, got_idx: None,
            });
        }
    }

    // Assign PLT indices to global symbols
    for (plt_idx, name) in plt_names.iter().enumerate() {
        if let Some(gsym) = globals.get_mut(name) {
            gsym.plt_idx = Some(plt_idx);
        }
    }

    // Collect symbols that need GOT entries (GOTPCREL references).
    // For undefined symbols, these need R_X86_64_GLOB_DAT relocations.
    let mut got_needed_names: Vec<String> = Vec::new();
    for obj in objects.iter() {
        for sec_relas in &obj.relocations {
            for rela in sec_relas {
                let si = rela.sym_idx as usize;
                if si >= obj.symbols.len() { continue; }
                let sym = &obj.symbols[si];
                if sym.name.is_empty() { continue; }
                match rela.rela_type {
                    R_X86_64_GOTPCREL | R_X86_64_GOTPCRELX | R_X86_64_REX_GOTPCRELX | R_X86_64_GOTTPOFF => {
                        if !got_needed_names.contains(&sym.name) {
                            got_needed_names.push(sym.name.clone());
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    // Ensure GOT-referenced undefined symbols are in globals for dynsym
    for name in &got_needed_names {
        if !globals.contains_key(name) {
            globals.insert(name.clone(), GlobalSymbol {
                value: 0, size: 0, info: (STB_GLOBAL << 4) | STT_FUNC,
                defined_in: None, from_lib: None, section_idx: SHN_UNDEF,
                is_dynamic: true, copy_reloc: false, lib_sym_value: 0, version: None,
                plt_idx: None, got_idx: None,
            });
        }
    }

    // Pre-scan: collect named global symbols referenced by R_X86_64_64 relocations.
    // These must appear in the dynamic symbol table so the dynamic linker can
    // resolve them (supporting symbol interposition at runtime).
    let mut abs64_sym_names: BTreeSet<String> = BTreeSet::new();
    for obj in objects.iter() {
        for sec_relas in &obj.relocations {
            for rela in sec_relas {
                if rela.rela_type == R_X86_64_64 {
                    let si = rela.sym_idx as usize;
                    if si >= obj.symbols.len() { continue; }
                    let sym = &obj.symbols[si];
                    if !sym.name.is_empty() && !sym.is_local() && sym.sym_type() != STT_SECTION {
                        abs64_sym_names.insert(sym.name.clone());
                    }
                }
            }
        }
    }

    // Collect all defined global symbols for export
    let mut dyn_sym_names: Vec<String> = Vec::new();
    let mut exported: Vec<String> = globals.iter()
        .filter(|(_, g)| {
            g.defined_in.is_some() && !g.is_dynamic
                && (g.info >> 4) != 0 // not STB_LOCAL
                && g.section_idx != SHN_UNDEF
        })
        .map(|(n, _)| n.clone())
        .collect();
    exported.sort();
    for name in exported {
        if !dyn_sym_names.contains(&name) {
            dyn_sym_names.push(name);
        }
    }

    // Also add undefined/dynamic symbols (from -l libs and PLT imports)
    for (name, gsym) in globals.iter() {
        if (gsym.is_dynamic || (gsym.defined_in.is_none() && gsym.section_idx == SHN_UNDEF))
            && !dyn_sym_names.contains(name)
        {
            dyn_sym_names.push(name.clone());
        }
    }

    // Ensure all symbols referenced by R_X86_64_64 data relocations are in dynsym
    for name in &abs64_sym_names {
        if !dyn_sym_names.contains(name) {
            dyn_sym_names.push(name.clone());
        }
    }

    for name in &dyn_sym_names { dynstr.add(name); }

    let dynsym_count = 1 + dyn_sym_names.len();
    let dynsym_size = dynsym_count as u64 * 24;
    let dynstr_size = dynstr.as_bytes().len() as u64;

    // Build .gnu.hash
    // Separate defined (hashed) from undefined (unhashed) symbols.
    // .gnu.hash only includes defined symbols; undefined symbols must come
    // first in the symbol table (before symoffset).
    let mut undef_syms: Vec<String> = Vec::new();
    let mut defined_syms: Vec<String> = Vec::new();
    for name in &dyn_sym_names {
        if let Some(g) = globals.get(name) {
            if g.defined_in.is_some() && g.section_idx != SHN_UNDEF {
                defined_syms.push(name.clone());
            } else {
                undef_syms.push(name.clone());
            }
        } else {
            undef_syms.push(name.clone());
        }
    }
    // Reorder: undefined first, then defined
    dyn_sym_names.clear();
    dyn_sym_names.extend(undef_syms.iter().cloned());
    dyn_sym_names.extend(defined_syms.iter().cloned());

    let gnu_hash_symoffset: usize = 1 + undef_syms.len(); // 1 for null entry + undefs
    let num_hashed = defined_syms.len();
    let gnu_hash_nbuckets = if num_hashed == 0 { 1 } else { num_hashed.next_power_of_two().max(1) } as u32;
    // Scale bloom filter size with number of symbols for efficient lookup.
    // Each 64-bit bloom word can effectively track ~32 symbols (2 bits each).
    // Use next power of two for the number of words needed, minimum 1.
    let gnu_hash_bloom_size: u32 = if num_hashed <= 32 { 1 }
        else { num_hashed.div_ceil(32).next_power_of_two() as u32 };
    let gnu_hash_bloom_shift: u32 = 6;

    let hashed_sym_hashes: Vec<u32> = defined_syms.iter()
        .map(|name| linker_common::gnu_hash(name.as_bytes()))
        .collect();

    let mut bloom_words: Vec<u64> = vec![0u64; gnu_hash_bloom_size as usize];
    for &h in &hashed_sym_hashes {
        let word_idx = ((h / 64) % gnu_hash_bloom_size) as usize;
        bloom_words[word_idx] |= 1u64 << (h as u64 % 64);
        bloom_words[word_idx] |= 1u64 << ((h >> gnu_hash_bloom_shift) as u64 % 64);
    }

    // Sort hashed (defined) symbols by bucket
    if num_hashed > 0 {
        let mut hashed_with_hash: Vec<(String, u32)> = defined_syms.iter()
            .zip(hashed_sym_hashes.iter())
            .map(|(n, &h)| (n.clone(), h))
            .collect();
        hashed_with_hash.sort_by_key(|(_, h)| h % gnu_hash_nbuckets);
        // Update defined portion of dyn_sym_names
        for (i, (name, _)) in hashed_with_hash.iter().enumerate() {
            dyn_sym_names[undef_syms.len() + i] = name.clone();
        }
    }

    let hashed_sym_hashes: Vec<u32> = dyn_sym_names[undef_syms.len()..].iter()
        .map(|name| linker_common::gnu_hash(name.as_bytes()))
        .collect();

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

    let plt_size = if plt_names.is_empty() { 0u64 } else { 16 + 16 * plt_names.len() as u64 };
    let got_plt_count = if plt_names.is_empty() { 0 } else { 3 + plt_names.len() };
    let got_plt_size = got_plt_count as u64 * 8;
    let rela_plt_size = plt_names.len() as u64 * 24;

    // Count R_X86_64_RELATIVE relocations needed (for internal absolute addresses)
    // We'll collect them during relocation processing
    let has_init_array = output_sections.iter().any(|s| s.name == ".init_array" && s.mem_size > 0);
    let has_fini_array = output_sections.iter().any(|s| s.name == ".fini_array" && s.mem_size > 0);
    let mut dyn_count = needed_sonames.len() as u64 + 10; // 9 fixed entries + DT_NULL
    if soname.is_some() { dyn_count += 1; }
    if has_init_array { dyn_count += 2; }
    if has_fini_array { dyn_count += 2; }
    if !plt_names.is_empty() { dyn_count += 4; } // DT_PLTGOT, DT_PLTRELSZ, DT_PLTREL, DT_JMPREL
    if rpath_string.is_some() { dyn_count += 1; } // DT_RUNPATH or DT_RPATH
    let dynamic_size = dyn_count * 16;

    let has_tls_sections = output_sections.iter().any(|s| s.flags & SHF_TLS != 0 && s.flags & SHF_ALLOC != 0);

    // Identify output sections that have R_X86_64_64 relocations (need RELATIVE
    // relocations at load time). These must go in a writable segment so the
    // dynamic linker can patch them. We track them by output section index.
    let mut sections_with_abs_relocs: std::collections::HashSet<usize> = std::collections::HashSet::new();
    for obj in objects.iter() {
        for (sec_idx, sec_relas) in obj.relocations.iter().enumerate() {
            for rela in sec_relas {
                if rela.rela_type == R_X86_64_64 {
                    // Find which output section this input section maps to
                    let obj_idx_search = objects.iter().position(|o| std::ptr::eq(o, obj));
                    if let Some(oi) = obj_idx_search {
                        if let Some(&(out_idx, _)) = section_map.get(&(oi, sec_idx)) {
                            sections_with_abs_relocs.insert(out_idx);
                        }
                    }
                }
            }
        }
    }

    // A section is "pure rodata" if it's read-only and has no absolute relocations.
    // Sections with absolute relocations go in the RW segment (as .data.rel.ro).
    let is_pure_rodata = |idx: usize, sec: &OutputSection| -> bool {
        sec.flags & SHF_ALLOC != 0
            && sec.flags & SHF_EXECINSTR == 0
            && sec.flags & SHF_WRITE == 0
            && sec.flags & SHF_TLS == 0
            && sec.sh_type != SHT_NOBITS
            && !sections_with_abs_relocs.contains(&idx)
    };
    let is_relro_rodata = |idx: usize, sec: &OutputSection| -> bool {
        sec.flags & SHF_ALLOC != 0
            && sec.flags & SHF_EXECINSTR == 0
            && sec.flags & SHF_WRITE == 0
            && sec.flags & SHF_TLS == 0
            && sec.sh_type != SHT_NOBITS
            && sections_with_abs_relocs.contains(&idx)
    };

    // phdrs: PHDR, LOAD(ro), LOAD(text), LOAD(rodata), LOAD(rw), DYNAMIC, GNU_STACK, [GNU_RELRO], [TLS]
    let has_relro = !sections_with_abs_relocs.is_empty();
    let mut phdr_count: u64 = 7; // base count
    if has_tls_sections { phdr_count += 1; }
    if has_relro { phdr_count += 1; }
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
            let a = sec.alignment.max(1);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = base_addr + offset;
            sec.file_offset = offset;
            offset += sec.mem_size;
        }
    }
    // PLT goes at the end of the text segment
    let (plt_addr, plt_offset) = if plt_size > 0 {
        offset = (offset + 15) & !15;
        let a = base_addr + offset; let o = offset; offset += plt_size; (a, o)
    } else { (0u64, 0u64) };
    let text_total_size = offset - text_page_offset;

    // Rodata segment - only pure rodata (no absolute relocations)
    offset = (offset + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
    let rodata_page_offset = offset;
    let rodata_page_addr = base_addr + offset;
    for (idx, sec) in output_sections.iter_mut().enumerate() {
        if is_pure_rodata(idx, sec) {
            let a = sec.alignment.max(1);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = base_addr + offset;
            sec.file_offset = offset;
            offset += sec.mem_size;
        }
    }
    let rodata_total_size = offset - rodata_page_offset;

    // RW segment - includes RELRO sections (rodata with abs relocs), then linker
    // data structures, then actual writable data
    offset = (offset + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
    let rw_page_offset = offset;
    let rw_page_addr = base_addr + offset;

    // First: RELRO sections (rodata that needs dynamic relocations)
    let _relro_start_offset = offset;
    for (idx, sec) in output_sections.iter_mut().enumerate() {
        if is_relro_rodata(idx, sec) {
            let a = sec.alignment.max(1);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = base_addr + offset;
            sec.file_offset = offset;
            offset += sec.mem_size;
        }
    }

    let mut init_array_addr = 0u64; let mut init_array_size = 0u64;
    let mut fini_array_addr = 0u64; let mut fini_array_size = 0u64;

    for sec in output_sections.iter_mut() {
        if sec.name == ".init_array" {
            let a = sec.alignment.max(8);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = base_addr + offset; sec.file_offset = offset;
            init_array_addr = sec.addr; init_array_size = sec.mem_size;
            offset += sec.mem_size; break;
        }
    }
    for sec in output_sections.iter_mut() {
        if sec.name == ".fini_array" {
            let a = sec.alignment.max(8);
            offset = (offset + a - 1) & !(a - 1);
            sec.addr = base_addr + offset; sec.file_offset = offset;
            fini_array_addr = sec.addr; fini_array_size = sec.mem_size;
            offset += sec.mem_size; break;
        }
    }

    // GOT entries were already collected into got_needed_names above.
    let got_needed = &got_needed_names;

    // Reserve space for .rela.dyn (will be filled later)
    offset = (offset + 7) & !7;
    let rela_dyn_offset = offset;
    let rela_dyn_addr = base_addr + offset;
    // Each R_X86_64_64 reloc in input becomes one R_X86_64_RELATIVE entry.
    let mut max_rela_count: usize = 0;
    for obj in objects.iter() {
        for sec_relas in &obj.relocations {
            for rela in sec_relas {
                if rela.rela_type == R_X86_64_64 {
                    max_rela_count += 1;
                }
            }
        }
    }
    // Also init_array/fini_array entries are pointers
    for sec in output_sections.iter() {
        if sec.name == ".init_array" || sec.name == ".fini_array" {
            max_rela_count += (sec.mem_size / 8) as usize;
        }
    }
    // GOT entries need either RELATIVE (local) or GLOB_DAT (external) relocations
    max_rela_count += got_needed.len();
    let rela_dyn_max_size = max_rela_count as u64 * 24;
    offset += rela_dyn_max_size;

    // .rela.plt (JMPREL) for PLT GOT entries
    offset = (offset + 7) & !7;
    let rela_plt_offset = offset;
    let rela_plt_addr = base_addr + offset;
    offset += rela_plt_size;

    offset = (offset + 7) & !7;
    let dynamic_offset = offset; let dynamic_addr = base_addr + offset; offset += dynamic_size;

    // End of RELRO region (page-aligned up for PT_GNU_RELRO).
    // Everything after this must be on a new page so that mprotect(PROT_READ)
    // on the RELRO region doesn't affect writable data (GOT.PLT, GOT, .data, .bss).
    let relro_end_offset = (offset + PAGE_SIZE - 1) & !(PAGE_SIZE - 1);
    let relro_end_addr = base_addr + relro_end_offset;
    if has_relro {
        offset = relro_end_offset; // advance to page boundary
    }

    // .got.plt entries - MUST be after RELRO boundary since dynamic linker
    // needs to write to them during lazy PLT resolution
    offset = (offset + 7) & !7;
    let got_plt_offset = offset;
    let got_plt_addr = base_addr + offset;
    offset += got_plt_size;

    // GOT for locally-resolved symbols
    let got_offset = offset; let got_addr = base_addr + offset;
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
            sec.addr = base_addr + offset; sec.file_offset = offset;
            if tls_addr == 0 { tls_addr = sec.addr; tls_file_offset = offset; tls_align = a; }
            tls_file_size += sec.mem_size;
            tls_mem_size += sec.mem_size;
            offset += sec.mem_size;
        }
    }
    if tls_addr == 0 && has_tls_sections {
        tls_addr = base_addr + offset;
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

    // Define linker-provided symbols
    let linker_addrs = LinkerSymbolAddresses {
        base_addr,
        got_addr,
        dynamic_addr,
        bss_addr,
        bss_size,
        text_end: text_page_addr + text_total_size,
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
            section_idx: SHN_ABS, is_dynamic: false, copy_reloc: false, lib_sym_value: 0, version: None,
        });
        if entry.defined_in.is_none() && !entry.is_dynamic {
            entry.value = sym.value;
            entry.defined_in = Some(usize::MAX);
            entry.section_idx = SHN_ABS;
        }
    }

    // === Build output buffer ===
    let file_size = offset as usize;
    let mut out = vec![0u8; file_size];

    // ELF header
    out[0..4].copy_from_slice(&ELF_MAGIC);
    out[4] = ELFCLASS64; out[5] = ELFDATA2LSB; out[6] = 1;
    w16(&mut out, 16, ET_DYN); // Shared object
    w16(&mut out, 18, EM_X86_64); w32(&mut out, 20, 1);
    w64(&mut out, 24, 0); // e_entry = 0 for shared libraries
    w64(&mut out, 32, 64); // e_phoff
    w64(&mut out, 40, 0); // e_shoff = 0 (no section headers for now)
    w32(&mut out, 48, 0); w16(&mut out, 52, 64); w16(&mut out, 54, 56);
    w16(&mut out, 56, phdr_count as u16); w16(&mut out, 58, 64); w16(&mut out, 60, 0); w16(&mut out, 62, 0);

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
    wphdr(&mut out, ph, PT_LOAD, PF_R, rodata_page_offset, rodata_page_addr, rodata_total_size, rodata_total_size, PAGE_SIZE); ph += 56;
    let rw_filesz = offset - rw_page_offset;
    let rw_memsz = if bss_size > 0 { (bss_addr + bss_size) - rw_page_addr } else { rw_filesz };
    wphdr(&mut out, ph, PT_LOAD, PF_R|PF_W, rw_page_offset, rw_page_addr, rw_filesz, rw_memsz, PAGE_SIZE); ph += 56;
    wphdr(&mut out, ph, PT_DYNAMIC, PF_R|PF_W, dynamic_offset, dynamic_addr, dynamic_size, dynamic_size, 8); ph += 56;
    wphdr(&mut out, ph, PT_GNU_STACK, PF_R|PF_W, 0, 0, 0, 0, 0x10); ph += 56;
    if has_relro {
        let relro_filesz = relro_end_addr - rw_page_addr;
        wphdr(&mut out, ph, PT_GNU_RELRO, PF_R, rw_page_offset, rw_page_addr, relro_filesz, relro_filesz, 1); ph += 56;
    }
    if has_tls {
        wphdr(&mut out, ph, PT_TLS, PF_R, tls_file_offset, tls_addr, tls_file_size, tls_mem_size, tls_align);
    }

    // .gnu.hash
    let gh = gnu_hash_offset as usize;
    w32(&mut out, gh, gnu_hash_nbuckets);
    w32(&mut out, gh+4, gnu_hash_symoffset as u32);
    w32(&mut out, gh+8, gnu_hash_bloom_size);
    w32(&mut out, gh+12, gnu_hash_bloom_shift);
    let bloom_off = gh + 16;
    for (i, &bw) in bloom_words.iter().enumerate() {
        w64(&mut out, bloom_off + i * 8, bw);
    }
    let buckets_off = bloom_off + (gnu_hash_bloom_size as usize * 8);
    for (i, &b) in gnu_hash_buckets.iter().enumerate() {
        w32(&mut out, buckets_off + i * 4, b);
    }
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
            if gsym.defined_in.is_some() && !gsym.is_dynamic && gsym.section_idx != SHN_UNDEF {
                // Exported defined symbol: preserve original st_info (type + binding)
                if ds+5 < out.len() { out[ds+4] = gsym.info; out[ds+5] = 0; }
                // shndx=1: marks symbol as defined (non-UNDEF). The dynamic linker
                // only checks UNDEF vs defined, not the actual section index.
                w16(&mut out, ds+6, 1);
                w64(&mut out, ds+8, gsym.value);
                w64(&mut out, ds+16, gsym.size);
            } else {
                // Undefined symbol (from -l dependencies or weak refs)
                // Preserve original binding (STB_WEAK vs STB_GLOBAL) and type
                let bind = gsym.info >> 4;
                let stype = gsym.info & 0xf;
                let st_info = (bind << 4) | if stype != 0 { stype } else { STT_FUNC };
                if ds+5 < out.len() { out[ds+4] = st_info; out[ds+5] = 0; }
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

    // Section data
    for sec in output_sections.iter() {
        if sec.sh_type == SHT_NOBITS || sec.data.is_empty() { continue; }
        write_bytes(&mut out, sec.file_offset as usize, &sec.data);
    }

    // .plt - PLT stubs for external dynamic symbols
    if plt_size > 0 {
        let po = plt_offset as usize;
        // PLT[0] - the resolver stub (16 bytes)
        out[po] = 0xff; out[po+1] = 0x35; // push [GOT+8] (link_map)
        w32(&mut out, po+2, ((got_plt_addr+8) as i64 - (plt_addr+6) as i64) as u32);
        out[po+6] = 0xff; out[po+7] = 0x25; // jmp [GOT+16] (resolver)
        w32(&mut out, po+8, ((got_plt_addr+16) as i64 - (plt_addr+12) as i64) as u32);
        for i in 12..16 { out[po+i] = 0x90; } // nop padding

        // PLT[1..N] - per-symbol stubs (16 bytes each)
        for (i, _) in plt_names.iter().enumerate() {
            let ep = po + 16 + i * 16;
            let pea = plt_addr + 16 + i as u64 * 16;
            let gea = got_plt_addr + 24 + i as u64 * 8;
            out[ep] = 0xff; out[ep+1] = 0x25; // jmp [GOT.PLT slot]
            w32(&mut out, ep+2, (gea as i64 - (pea+6) as i64) as u32);
            out[ep+6] = 0x68; w32(&mut out, ep+7, i as u32); // push <plt_index>
            out[ep+11] = 0xe9; // jmp PLT[0]
            w32(&mut out, ep+12, (plt_addr as i64 - (pea+16) as i64) as u32);
        }
    }

    // .got.plt
    if got_plt_size > 0 {
        let gp = got_plt_offset as usize;
        w64(&mut out, gp, dynamic_addr);  // GOT[0] = _DYNAMIC
        w64(&mut out, gp+8, 0);           // GOT[1] = 0 (link_map, filled by ld.so)
        w64(&mut out, gp+16, 0);          // GOT[2] = 0 (resolver, filled by ld.so)
        for (i, _) in plt_names.iter().enumerate() {
            // GOT[3+i] = address of "push <index>" in PLT stub (lazy binding)
            w64(&mut out, gp+24+i*8, plt_addr + 16 + i as u64 * 16 + 6);
        }
    }

    // .rela.plt - JMPREL relocations for GOT.PLT entries
    if rela_plt_size > 0 {
        let mut rp = rela_plt_offset as usize;
        let gpb = got_plt_addr + 24; // base of per-symbol GOT.PLT slots
        for (i, name) in plt_names.iter().enumerate() {
            let gea = gpb + i as u64 * 8;
            // Find symbol index in dynsym
            let si = dyn_sym_names.iter().position(|n| n == name).map(|j| j+1).unwrap_or(0) as u64;
            w64(&mut out, rp, gea);             // r_offset = GOT.PLT slot address
            w64(&mut out, rp+8, (si << 32) | R_X86_64_JUMP_SLOT as u64);
            w64(&mut out, rp+16, 0);            // r_addend = 0
            rp += 24;
        }
    }

    // Build GOT entries map
    let mut got_sym_addrs: HashMap<String, u64> = HashMap::new();
    for (i, name) in got_needed.iter().enumerate() {
        let gea = got_addr + i as u64 * 8;
        got_sym_addrs.insert(name.clone(), gea);
        // Fill GOT with resolved symbol value
        if let Some(gsym) = globals.get(name) {
            if gsym.defined_in.is_some() && !gsym.is_dynamic {
                w64(&mut out, (got_offset + i as u64 * 8) as usize, gsym.value);
            }
        }
    }

    // Apply relocations and collect dynamic relocation entries
    let globals_snap: HashMap<String, GlobalSymbol> = globals.clone();
    let mut rela_dyn_entries: Vec<(u64, u64)> = Vec::new(); // (offset, value) for RELATIVE relocs
    let mut glob_dat_entries: Vec<(u64, String)> = Vec::new(); // (offset, sym_name) for GLOB_DAT relocs
    let mut abs64_entries: Vec<(u64, String, i64)> = Vec::new(); // (offset, sym_name, addend) for R_X86_64_64 relocs

    // Add RELATIVE entries for GOT entries that point to local symbols,
    // and GLOB_DAT entries for GOT entries that point to external symbols
    for (i, name) in got_needed.iter().enumerate() {
        let gea = got_addr + i as u64 * 8;
        if let Some(gsym) = globals_snap.get(name) {
            if gsym.defined_in.is_some() && !gsym.is_dynamic && gsym.section_idx != SHN_UNDEF {
                rela_dyn_entries.push((gea, gsym.value));
            } else {
                // External symbol - needs GLOB_DAT
                glob_dat_entries.push((gea, name.clone()));
            }
        } else {
            // Unknown symbol - needs GLOB_DAT
            glob_dat_entries.push((gea, name.clone()));
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
                let s = resolve_sym(obj_idx, sym, &globals_snap, section_map, output_sections, plt_addr);

                match rela.rela_type {
                    R_X86_64_64 => {
                        let val = (s as i64 + a) as u64;
                        w64(&mut out, fp, val);
                        // Determine what kind of dynamic relocation to emit.
                        // Named global/weak symbols need R_X86_64_64 dynamic relocs
                        // (with symbol index) to support symbol interposition.
                        // Section symbols and local symbols use R_X86_64_RELATIVE.
                        let is_named_global = !sym.name.is_empty()
                            && !sym.is_local()
                            && sym.sym_type() != STT_SECTION;
                        if is_named_global {
                            abs64_entries.push((p, sym.name.clone(), a));
                        } else if s != 0 {
                            rela_dyn_entries.push((p, val));
                        }
                    }
                    R_X86_64_PC32 | R_X86_64_PLT32 => {
                        // For dynamic symbols, redirect through PLT
                        let t = if !sym.name.is_empty() && !sym.is_local() {
                            if let Some(g) = globals_snap.get(&sym.name) {
                                if let Some(pi) = g.plt_idx {
                                    plt_addr + 16 + pi as u64 * 16
                                } else { s }
                            } else { s }
                        } else { s };
                        w32(&mut out, fp, (t as i64 + a - p as i64) as u32);
                    }
                    // TODO: R_X86_64_32/32S are not position-independent and should
                    // ideally emit a diagnostic when used in shared libraries. For now
                    // we apply them statically which works for simple cases but may fail
                    // if the library is loaded at a high address.
                    R_X86_64_32 => { w32(&mut out, fp, (s as i64 + a) as u32); }
                    R_X86_64_32S => { w32(&mut out, fp, (s as i64 + a) as u32); }
                    R_X86_64_GOTPCREL | R_X86_64_GOTPCRELX | R_X86_64_REX_GOTPCRELX => {
                        if let Some(&gea) = got_sym_addrs.get(&sym.name) {
                            w32(&mut out, fp, (gea as i64 + a - p as i64) as u32);
                        } else if (rela.rela_type == R_X86_64_GOTPCRELX || rela.rela_type == R_X86_64_REX_GOTPCRELX)
                                  && !sym.name.is_empty() {
                            // GOT relaxation: convert to LEA
                            if let Some(g) = globals_snap.get(&sym.name) {
                                if g.defined_in.is_some() {
                                    if fp >= 2 && fp < out.len() && out[fp-2] == 0x8b {
                                        out[fp-2] = 0x8d;
                                    }
                                    w32(&mut out, fp, (s as i64 + a - p as i64) as u32);
                                    continue;
                                }
                            }
                            w32(&mut out, fp, (s as i64 + a - p as i64) as u32);
                        } else {
                            w32(&mut out, fp, (s as i64 + a - p as i64) as u32);
                        }
                    }
                    R_X86_64_PC64 => { w64(&mut out, fp, (s as i64 + a - p as i64) as u64); }
                    R_X86_64_GOTTPOFF => {
                        // TLS: try to find GOT entry
                        if let Some(&gea) = got_sym_addrs.get(&sym.name) {
                            let tpoff = (s as i64 - tls_addr as i64) - tls_mem_size as i64;
                            w64(&mut out, (got_offset + (gea - got_addr)) as usize, tpoff as u64);
                            w32(&mut out, fp, (gea as i64 + a - p as i64) as u32);
                        } else {
                            let tpoff = (s as i64 - tls_addr as i64) - tls_mem_size as i64;
                            if fp >= 2 && fp + 4 <= out.len() && out[fp-2] == 0x8b {
                                let modrm = out[fp-1];
                                let reg = (modrm >> 3) & 7;
                                out[fp-2] = 0xc7;
                                out[fp-1] = 0xc0 | reg;
                                w32(&mut out, fp, (tpoff + a) as u32);
                            }
                        }
                    }
                    R_X86_64_TPOFF32 => {
                        let tpoff = (s as i64 - tls_addr as i64) - tls_mem_size as i64;
                        w32(&mut out, fp, (tpoff + a) as u32);
                    }
                    R_X86_64_NONE => {}
                    other => {
                        eprintln!("warning: unsupported relocation type {} for '{}' in shared library", other, sym.name);
                    }
                }
            }
        }
    }

    // Write .rela.dyn entries
    let relative_count = rela_dyn_entries.len();
    let total_rela_count = relative_count + glob_dat_entries.len() + abs64_entries.len();
    let rela_dyn_size = total_rela_count as u64 * 24;
    let mut rd = rela_dyn_offset as usize;
    // First: R_X86_64_RELATIVE entries (type 8, no symbol)
    for (rel_offset, rel_value) in &rela_dyn_entries {
        if rd + 24 <= out.len() {
            w64(&mut out, rd, *rel_offset);     // r_offset
            w64(&mut out, rd+8, R_X86_64_RELATIVE as u64); // r_info (sym 0)
            w64(&mut out, rd+16, *rel_value);   // r_addend = runtime value
            rd += 24;
        }
    }
    // Then: R_X86_64_GLOB_DAT entries (type 6, with symbol index)
    for (rel_offset, sym_name) in &glob_dat_entries {
        let si = dyn_sym_names.iter().position(|n| n == sym_name).map(|j| j + 1).unwrap_or(0) as u64;
        if rd + 24 <= out.len() {
            w64(&mut out, rd, *rel_offset);         // r_offset = GOT entry address
            w64(&mut out, rd+8, (si << 32) | R_X86_64_GLOB_DAT as u64);
            w64(&mut out, rd+16, 0);                 // r_addend = 0
            rd += 24;
        }
    }
    // Then: R_X86_64_64 entries (type 1, with symbol index) for named symbol
    // references in data sections (function pointer tables, vtables, etc.)
    for (rel_offset, sym_name, addend) in &abs64_entries {
        let si = dyn_sym_names.iter().position(|n| n == sym_name).map(|j| j + 1).unwrap_or(0) as u64;
        if rd + 24 <= out.len() {
            w64(&mut out, rd, *rel_offset);         // r_offset
            w64(&mut out, rd+8, (si << 32) | R_X86_64_64 as u64);
            w64(&mut out, rd+16, *addend as u64);   // r_addend
            rd += 24;
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
        (DT_RELACOUNT, relative_count as u64),
        (DT_GNU_HASH, gnu_hash_addr),
        // DT_TEXTREL not needed since we use PIC
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
    if !plt_names.is_empty() {
        w64(&mut out, dd, DT_PLTGOT as u64); w64(&mut out, dd+8, got_plt_addr); dd += 16;
        w64(&mut out, dd, DT_PLTRELSZ as u64); w64(&mut out, dd+8, rela_plt_size); dd += 16;
        w64(&mut out, dd, DT_PLTREL as u64); w64(&mut out, dd+8, DT_RELA as u64); dd += 16;
        w64(&mut out, dd, DT_JMPREL as u64); w64(&mut out, dd+8, rela_plt_addr); dd += 16;
    }
    if let Some(ref rp) = rpath_string {
        let rp_off = dynstr.get_offset(rp) as u64;
        let tag = if use_runpath { DT_RUNPATH } else { DT_RPATH };
        w64(&mut out, dd, tag as u64); w64(&mut out, dd+8, rp_off); dd += 16;
    }
    w64(&mut out, dd, DT_NULL as u64); w64(&mut out, dd+8, 0);

    // === Append section headers ===
    // Build .shstrtab string table
    let mut shstrtab = vec![0u8]; // null byte at offset 0
    let mut shstr_offsets: HashMap<String, u32> = HashMap::new();
    let known_names = [
        ".gnu.hash", ".dynsym", ".dynstr",
        ".rela.dyn", ".rela.plt", ".plt", ".dynamic",
        ".got", ".got.plt", ".init_array", ".fini_array",
        ".tdata", ".tbss", ".bss", ".shstrtab",
    ];
    for name in &known_names {
        let off = shstrtab.len() as u32;
        shstr_offsets.insert(name.to_string(), off);
        shstrtab.extend_from_slice(name.as_bytes());
        shstrtab.push(0);
    }
    // Add merged section names not already in known list
    for sec in output_sections.iter() {
        if !sec.name.is_empty() && !shstr_offsets.contains_key(&sec.name) {
            let off = shstrtab.len() as u32;
            shstr_offsets.insert(sec.name.clone(), off);
            shstrtab.extend_from_slice(sec.name.as_bytes());
            shstrtab.push(0);
        }
    }

    let get_shname = |n: &str| -> u32 { shstr_offsets.get(n).copied().unwrap_or(0) };

    // Helper: write a 64-byte ELF64 section header
    // Use shared write_elf64_shdr from linker_common (aliased locally for brevity)
    let write_shdr_so = linker_common::write_elf64_shdr;

    // Pre-count section indices for cross-references
    let dynsym_shidx: u32 = 2; // NULL=0, .gnu.hash=1, .dynsym=2
    let dynstr_shidx: u32 = 3; // .dynstr=3

    // Count total sections to determine .shstrtab index
    let mut sh_count: u16 = 4; // NULL + .gnu.hash + .dynsym + .dynstr
    if rela_dyn_size > 0 { sh_count += 1; }
    if rela_plt_size > 0 { sh_count += 1; }
    if plt_size > 0 { sh_count += 1; }
    // Merged output sections (non-BSS, non-TLS, non-init/fini)
    for sec in output_sections.iter() {
        if sec.flags & SHF_ALLOC != 0 && sec.sh_type != SHT_NOBITS && sec.flags & SHF_TLS == 0
           && sec.name != ".init_array" && sec.name != ".fini_array" {
            sh_count += 1;
        }
    }
    // TLS data + TLS BSS
    for sec in output_sections.iter() {
        if sec.flags & SHF_TLS != 0 && sec.flags & SHF_ALLOC != 0 && sec.sh_type != SHT_NOBITS { sh_count += 1; }
    }
    for sec in output_sections.iter() {
        if sec.flags & SHF_TLS != 0 && sec.sh_type == SHT_NOBITS { sh_count += 1; }
    }
    if has_init_array { sh_count += 1; }
    if has_fini_array { sh_count += 1; }
    sh_count += 1; // .dynamic
    if got_plt_size > 0 { sh_count += 1; }
    if got_size > 0 { sh_count += 1; }
    // BSS sections (non-TLS)
    for sec in output_sections.iter() {
        if sec.sh_type == SHT_NOBITS && sec.flags & SHF_ALLOC != 0 && sec.flags & SHF_TLS == 0 { sh_count += 1; }
    }
    let shstrtab_shidx = sh_count; // .shstrtab is the last section
    sh_count += 1;

    // Align and append .shstrtab data
    while !out.len().is_multiple_of(8) { out.push(0); }
    let shstrtab_data_offset = out.len() as u64;
    out.extend_from_slice(&shstrtab);

    // Align section header table to 8 bytes
    while !out.len().is_multiple_of(8) { out.push(0); }
    let shdr_offset = out.len() as u64;

    // Write section headers
    // [0] NULL
    write_shdr_so(&mut out, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    // .gnu.hash
    write_shdr_so(&mut out, get_shname(".gnu.hash"), SHT_GNU_HASH, SHF_ALLOC,
               gnu_hash_addr, gnu_hash_offset, gnu_hash_size, dynsym_shidx, 0, 8, 0);
    // .dynsym
    write_shdr_so(&mut out, get_shname(".dynsym"), SHT_DYNSYM, SHF_ALLOC,
               dynsym_addr, dynsym_offset, dynsym_size, dynstr_shidx, 1, 8, 24);
    // .dynstr
    write_shdr_so(&mut out, get_shname(".dynstr"), SHT_STRTAB, SHF_ALLOC,
               dynstr_addr, dynstr_offset, dynstr_size, 0, 0, 1, 0);
    // .rela.dyn
    if rela_dyn_size > 0 {
        write_shdr_so(&mut out, get_shname(".rela.dyn"), SHT_RELA, SHF_ALLOC,
                   rela_dyn_addr, rela_dyn_offset, rela_dyn_size, dynsym_shidx, 0, 8, 24);
    }
    // .rela.plt
    if rela_plt_size > 0 {
        write_shdr_so(&mut out, get_shname(".rela.plt"), SHT_RELA, SHF_ALLOC | 0x40,
                   rela_plt_addr, rela_plt_offset, rela_plt_size, dynsym_shidx, 0, 8, 24);
    }
    // .plt
    if plt_size > 0 {
        write_shdr_so(&mut out, get_shname(".plt"), SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                   plt_addr, plt_offset, plt_size, 0, 0, 16, 16);
    }
    // Merged output sections (text/rodata/data, excluding BSS/TLS/init_array/fini_array)
    for sec in output_sections.iter() {
        if sec.flags & SHF_ALLOC != 0 && sec.sh_type != SHT_NOBITS && sec.flags & SHF_TLS == 0
           && sec.name != ".init_array" && sec.name != ".fini_array" {
            write_shdr_so(&mut out, get_shname(&sec.name), sec.sh_type, sec.flags,
                       sec.addr, sec.file_offset, sec.mem_size, 0, 0, sec.alignment.max(1), 0);
        }
    }
    // TLS data sections (.tdata)
    for sec in output_sections.iter() {
        if sec.flags & SHF_TLS != 0 && sec.flags & SHF_ALLOC != 0 && sec.sh_type != SHT_NOBITS {
            write_shdr_so(&mut out, get_shname(&sec.name), sec.sh_type, sec.flags,
                       sec.addr, sec.file_offset, sec.mem_size, 0, 0, sec.alignment.max(1), 0);
        }
    }
    // TLS BSS sections (.tbss)
    for sec in output_sections.iter() {
        if sec.flags & SHF_TLS != 0 && sec.sh_type == SHT_NOBITS {
            write_shdr_so(&mut out, get_shname(&sec.name), SHT_NOBITS, sec.flags,
                       sec.addr, sec.file_offset, sec.mem_size, 0, 0, sec.alignment.max(1), 0);
        }
    }
    // .init_array
    if has_init_array {
        if let Some(ia_sec) = output_sections.iter().find(|s| s.name == ".init_array") {
            write_shdr_so(&mut out, get_shname(".init_array"), SHT_INIT_ARRAY, SHF_ALLOC | SHF_WRITE,
                       init_array_addr, ia_sec.file_offset, init_array_size, 0, 0, 8, 8);
        }
    }
    // .fini_array
    if has_fini_array {
        if let Some(fa_sec) = output_sections.iter().find(|s| s.name == ".fini_array") {
            write_shdr_so(&mut out, get_shname(".fini_array"), SHT_FINI_ARRAY, SHF_ALLOC | SHF_WRITE,
                       fini_array_addr, fa_sec.file_offset, fini_array_size, 0, 0, 8, 8);
        }
    }
    // .dynamic
    write_shdr_so(&mut out, get_shname(".dynamic"), SHT_DYNAMIC, SHF_ALLOC | SHF_WRITE,
               dynamic_addr, dynamic_offset, dynamic_size, dynstr_shidx, 0, 8, 16);
    // .got.plt
    if got_plt_size > 0 {
        write_shdr_so(&mut out, get_shname(".got.plt"), SHT_PROGBITS, SHF_ALLOC | SHF_WRITE,
                   got_plt_addr, got_plt_offset, got_plt_size, 0, 0, 8, 8);
    }
    // .got
    if got_size > 0 {
        write_shdr_so(&mut out, get_shname(".got"), SHT_PROGBITS, SHF_ALLOC | SHF_WRITE,
                   got_addr, got_offset, got_size, 0, 0, 8, 8);
    }
    // BSS sections (non-TLS)
    for sec in output_sections.iter() {
        if sec.sh_type == SHT_NOBITS && sec.flags & SHF_ALLOC != 0 && sec.flags & SHF_TLS == 0 {
            write_shdr_so(&mut out, get_shname(&sec.name), SHT_NOBITS, sec.flags,
                       sec.addr, sec.file_offset, sec.mem_size, 0, 0, sec.alignment.max(1), 0);
        }
    }
    // .shstrtab (last section)
    write_shdr_so(&mut out, get_shname(".shstrtab"), SHT_STRTAB, 0,
               0, shstrtab_data_offset, shstrtab.len() as u64, 0, 0, 1, 0);

    // Patch ELF header with section header info
    out[40..48].copy_from_slice(&shdr_offset.to_le_bytes());     // e_shoff
    out[58..60].copy_from_slice(&64u16.to_le_bytes());           // e_shentsize
    out[60..62].copy_from_slice(&sh_count.to_le_bytes());        // e_shnum
    out[62..64].copy_from_slice(&shstrtab_shidx.to_le_bytes()); // e_shstrndx

    std::fs::write(output_path, &out).map_err(|e| format!("failed to write '{}': {}", output_path, e))?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let _ = std::fs::set_permissions(output_path, std::fs::Permissions::from_mode(0o755));
    }
    Ok(())
}

/// x86-specific load_file wrapper. Delegates archive and object loading to
/// linker_common, handles shared library loading via the shared
/// `load_shared_library_elf64` implementation.
fn load_file(
    path: &str, objects: &mut Vec<ElfObject>, globals: &mut HashMap<String, GlobalSymbol>,
    needed_sonames: &mut Vec<String>, lib_paths: &[String],
) -> Result<(), String> {
    if std::env::var("LINKER_DEBUG").is_ok() {
        eprintln!("load_file: {}", path);
    }

    let data = std::fs::read(path).map_err(|e| format!("failed to read '{}': {}", path, e))?;

    // Regular archive
    if data.len() >= 8 && &data[0..8] == b"!<arch>\n" {
        return linker_common::load_archive_elf64(&data, path, objects, globals, EM_X86_64, x86_should_replace_extra);
    }

    // Thin archive
    if is_thin_archive(&data) {
        return linker_common::load_thin_archive_elf64(&data, path, objects, globals, EM_X86_64, x86_should_replace_extra);
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
                                load_file(lib_path, objects, globals, needed_sonames, lib_paths)?;
                            } else if let Some(ref dir) = script_dir {
                                let resolved = format!("{}/{}", dir, lib_path);
                                if Path::new(&resolved).exists() {
                                    load_file(&resolved, objects, globals, needed_sonames, lib_paths)?;
                                }
                            }
                        }
                        LinkerScriptEntry::Lib(lib_name) => {
                            if let Some(resolved_path) = linker_common::resolve_lib(lib_name, lib_paths, false) {
                                load_file(&resolved_path, objects, globals, needed_sonames, lib_paths)?;
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
            return linker_common::load_shared_library_elf64(path, globals, needed_sonames, lib_paths);
        }
    }

    // Regular ELF object
    let obj = parse_object(&data, path)?;
    let obj_idx = objects.len();
    linker_common::register_symbols_elf64(obj_idx, &obj, globals, x86_should_replace_extra);
    objects.push(obj);
    Ok(())
}

/// Perform --gc-sections: compute the set of dead (unreachable) sections.
///
/// Starting from entry-point sections (_start, main) and any init/fini arrays,
/// follow relocations transitively to find all reachable sections. Returns the
/// set of (object_idx, section_idx) pairs that are NOT reachable and should be
/// discarded.
fn gc_collect_sections(
    objects: &[ElfObject],
) -> HashSet<(usize, usize)> {
    // Build the set of all allocatable input sections
    let mut all_sections: HashSet<(usize, usize)> = HashSet::new();
    for (obj_idx, obj) in objects.iter().enumerate() {
        for (sec_idx, sec) in obj.sections.iter().enumerate() {
            if sec.flags & SHF_ALLOC == 0 { continue; }
            if matches!(sec.sh_type, SHT_NULL | SHT_STRTAB | SHT_SYMTAB | SHT_RELA | SHT_REL | SHT_GROUP) { continue; }
            if sec.flags & SHF_EXCLUDE != 0 { continue; }
            all_sections.insert((obj_idx, sec_idx));
        }
    }

    // Build a map from symbol name -> (obj_idx, sec_idx) for defined symbols
    let mut sym_to_section: HashMap<&str, (usize, usize)> = HashMap::new();
    for (obj_idx, obj) in objects.iter().enumerate() {
        for sym in &obj.symbols {
            if sym.shndx == SHN_UNDEF || sym.shndx == SHN_ABS || sym.shndx == SHN_COMMON { continue; }
            let binding = sym.info >> 4;
            if binding != STB_GLOBAL && binding != STB_WEAK { continue; }
            if sym.name.is_empty() { continue; }
            let sec_idx = sym.shndx as usize;
            if sec_idx < obj.sections.len() {
                sym_to_section.entry(sym.name.as_str()).or_insert((obj_idx, sec_idx));
            }
        }
    }

    // Seed the worklist with entry-point sections and sections that must be kept
    let mut live: HashSet<(usize, usize)> = HashSet::new();
    let mut worklist: VecDeque<(usize, usize)> = VecDeque::new();

    let mark_live = |key: (usize, usize), live: &mut HashSet<(usize, usize)>, wl: &mut VecDeque<(usize, usize)>| {
        if all_sections.contains(&key) && live.insert(key) {
            wl.push_back(key);
        }
    };

    // Mark sections containing entry-point symbols as live
    let entry_symbols = ["_start", "main", "__libc_csu_init", "__libc_csu_fini"];
    for &entry_name in &entry_symbols {
        if let Some(&key) = sym_to_section.get(entry_name) {
            mark_live(key, &mut live, &mut worklist);
        }
    }

    // Mark init/fini array sections as live (these are called by the runtime)
    for (obj_idx, obj) in objects.iter().enumerate() {
        for (sec_idx, sec) in obj.sections.iter().enumerate() {
            if sec.flags & SHF_ALLOC == 0 { continue; }
            let name = &sec.name;
            // Keep init/fini arrays and .ctors/.dtors (runtime calls these)
            if name == ".init_array" || name.starts_with(".init_array.")
                || name == ".fini_array" || name.starts_with(".fini_array.")
                || name == ".ctors" || name.starts_with(".ctors.")
                || name == ".dtors" || name.starts_with(".dtors.")
                || name == ".preinit_array" || name.starts_with(".preinit_array.")
                || name == ".init" || name == ".fini"
                || name == ".note.GNU-stack"
                || name == ".note.gnu.build-id"
            {
                mark_live((obj_idx, sec_idx), &mut live, &mut worklist);
            }
        }
    }

    // BFS: follow relocations from live sections to discover more live sections
    while let Some((obj_idx, sec_idx)) = worklist.pop_front() {
        let obj = &objects[obj_idx];
        // Follow relocations from this section
        if sec_idx < obj.relocations.len() {
            for rela in &obj.relocations[sec_idx] {
                let sym_idx = rela.sym_idx as usize;
                if sym_idx >= obj.symbols.len() { continue; }
                let sym = &obj.symbols[sym_idx];

                if sym.shndx != SHN_UNDEF && sym.shndx != SHN_ABS && sym.shndx != SHN_COMMON {
                    // Symbol is defined in this object file
                    let target = (obj_idx, sym.shndx as usize);
                    mark_live(target, &mut live, &mut worklist);
                } else if !sym.name.is_empty() {
                    // Symbol is undefined here; look up in global symbol table
                    if let Some(&target) = sym_to_section.get(sym.name.as_str()) {
                        mark_live(target, &mut live, &mut worklist);
                    }
                }
            }
        }
    }

    // Return the dead sections (all sections minus live ones)
    all_sections.difference(&live).copied().collect()
}

/// Collect IFUNC symbols for static linking.
/// In static executables, IFUNC symbols (STT_GNU_IFUNC) need special handling:
/// their resolver functions must be called at startup via IRELATIVE relocations.
fn collect_ifunc_symbols(globals: &HashMap<String, GlobalSymbol>, is_static: bool) -> Vec<String> {
    if !is_static { return Vec::new(); }
    let mut ifunc_symbols: Vec<String> = globals.iter()
        .filter(|(_, g)| g.defined_in.is_some() && (g.info & 0xf) == STT_GNU_IFUNC)
        .map(|(n, _)| n.clone())
        .collect();
    ifunc_symbols.sort();
    ifunc_symbols
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
                if sym.name.is_empty() || sym.is_local() { continue; }
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

    // Mark copy relocation symbols and their aliases.
    // When a symbol like `environ` (WEAK) needs a COPY relocation, we must also
    // mark aliases like `__environ` (GLOBAL) at the same shared library address.
    // This ensures the dynamic linker redirects all references to our BSS copy.
    let mut copy_reloc_lib_addrs: Vec<(String, u64)> = Vec::new(); // (from_lib, lib_sym_value)
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
    // Also mark aliases (other dynamic STT_OBJECT symbols at the same library address)
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
    export_dynamic: bool, rpath_entries: &[String], use_runpath: bool,
    is_static: bool, ifunc_symbols: &[String],
) -> Result<(), String> {
    let mut dynstr = DynStrTab::new();
    for lib in needed_sonames { dynstr.add(lib); }
    let rpath_string = if rpath_entries.is_empty() { None } else {
        let s = rpath_entries.join(":");
        dynstr.add(&s);
        Some(s)
    };

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
    // and are included in the .gnu.hash table
    let copy_reloc_syms: Vec<(String, u64)> = globals.iter()
        .filter(|(_, g)| g.copy_reloc)
        .map(|(n, g)| (n.clone(), g.size))
        .collect();
    for (name, _) in &copy_reloc_syms {
        if !dyn_sym_names.contains(name) {
            dyn_sym_names.push(name.clone());
        }
    }

    // When --export-dynamic is used, add all defined global symbols to the
    // dynamic symbol table so shared libraries loaded at runtime (via dlopen)
    // can find symbols from this executable.
    if export_dynamic {
        let mut exported: Vec<String> = globals.iter()
            .filter(|(_, g)| {
                // Export defined, non-dynamic (local to this executable) global symbols
                g.section_idx != SHN_UNDEF && !g.is_dynamic && !g.copy_reloc
                    && (g.info >> 4) != 0 // not STB_LOCAL
            })
            .map(|(n, _)| n.clone())
            .collect();
        exported.sort(); // deterministic output
        for name in exported {
            if !dyn_sym_names.contains(&name) {
                dyn_sym_names.push(name);
            }
        }
    }

    for name in &dyn_sym_names { dynstr.add(name); }

    // ── Build .gnu.version (versym) and .gnu.version_r (verneed) data ──
    //
    // Collect version requirements from dynamic symbols, grouped by library.
    let mut lib_versions: HashMap<String, BTreeSet<String>> = HashMap::new();
    for name in &dyn_sym_names {
        if let Some(gs) = globals.get(name) {
            if gs.is_dynamic {
                if let Some(ref ver) = gs.version {
                    if let Some(ref lib) = gs.from_lib {
                        lib_versions.entry(lib.clone())
                            .or_default()
                            .insert(ver.clone());
                    }
                }
            }
        }
    }

    // Build version index mapping: (library, version_string) -> version index (starting at 2)
    let mut ver_index_map: HashMap<(String, String), u16> = HashMap::new();
    let mut ver_idx: u16 = 2;
    let mut lib_ver_list: Vec<(String, Vec<String>)> = Vec::new();
    let mut sorted_libs: Vec<String> = lib_versions.keys().cloned().collect();
    sorted_libs.sort();
    for lib in &sorted_libs {
        let vers: Vec<String> = lib_versions[lib].iter().cloned().collect();
        for v in &vers {
            ver_index_map.insert((lib.clone(), v.clone()), ver_idx);
            ver_idx += 1;
            // Add version string to dynstr
            dynstr.add(v);
        }
        lib_ver_list.push((lib.clone(), vers));
    }

    // Build .gnu.version_r (verneed) section
    let mut verneed_data: Vec<u8> = Vec::new();
    let mut verneed_count: u32 = 0;
    // Only include libraries that are in our needed list
    let lib_ver_needed: Vec<(String, Vec<String>)> = lib_ver_list.iter()
        .filter(|(lib, _)| needed_sonames.contains(lib))
        .cloned()
        .collect();
    for (lib_i, (lib, vers)) in lib_ver_needed.iter().enumerate() {
        let lib_name_off = dynstr.get_offset(lib);
        let is_last_lib = lib_i == lib_ver_needed.len() - 1;

        // Verneed entry header (16 bytes)
        verneed_data.extend_from_slice(&1u16.to_le_bytes()); // vn_version = 1
        verneed_data.extend_from_slice(&(vers.len() as u16).to_le_bytes()); // vn_cnt
        verneed_data.extend_from_slice(&(lib_name_off as u32).to_le_bytes()); // vn_file
        verneed_data.extend_from_slice(&16u32.to_le_bytes()); // vn_aux (right after header)
        let next_off = if is_last_lib {
            0u32
        } else {
            16 + vers.len() as u32 * 16
        };
        verneed_data.extend_from_slice(&next_off.to_le_bytes()); // vn_next
        verneed_count += 1;

        // Vernaux entries for each version (16 bytes each)
        for (v_i, ver) in vers.iter().enumerate() {
            let ver_name_off = dynstr.get_offset(ver);
            let vidx = ver_index_map[&(lib.clone(), ver.clone())];
            let is_last_ver = v_i == vers.len() - 1;

            let vna_hash = linker_common::sysv_hash(ver.as_bytes());
            verneed_data.extend_from_slice(&vna_hash.to_le_bytes()); // vna_hash
            verneed_data.extend_from_slice(&0u16.to_le_bytes()); // vna_flags
            verneed_data.extend_from_slice(&vidx.to_le_bytes()); // vna_other
            verneed_data.extend_from_slice(&(ver_name_off as u32).to_le_bytes()); // vna_name
            let vna_next: u32 = if is_last_ver { 0 } else { 16 };
            verneed_data.extend_from_slice(&vna_next.to_le_bytes()); // vna_next
        }
    }

    let verneed_size = verneed_data.len() as u64;

    let dynsym_count = 1 + dyn_sym_names.len();
    let dynsym_size = dynsym_count as u64 * 24;
    let dynstr_size = dynstr.as_bytes().len() as u64;
    let rela_plt_size = plt_names.len() as u64 * 24;
    let rela_dyn_glob_count = got_entries.iter().filter(|(n, p)| {
        !n.is_empty() && !*p && globals.get(n).map(|g| g.is_dynamic && !g.copy_reloc).unwrap_or(false)
    }).count();
    let rela_dyn_count = rela_dyn_glob_count + copy_reloc_syms.len();
    let rela_dyn_size = rela_dyn_count as u64 * 24;

    // Build .gnu.hash table for hashed symbols (copy-reloc + exported)
    // Number of hashed symbols = total symbols after the non-hashed imports
    let num_hashed = dyn_sym_names.len() - (gnu_hash_symoffset - 1);
    let gnu_hash_nbuckets = if num_hashed == 0 { 1 } else { num_hashed.next_power_of_two().max(1) } as u32;
    let gnu_hash_bloom_size: u32 = 1;
    let gnu_hash_bloom_shift: u32 = 6;

    // Compute hashes for hashed symbols
    let hashed_sym_hashes: Vec<u32> = dyn_sym_names[gnu_hash_symoffset - 1..]
        .iter()
        .map(|name| linker_common::gnu_hash(name.as_bytes()))
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

    // Build .gnu.version (versym) - one u16 per dynsym entry
    // Must be built AFTER gnu_hash bucket sort so versym indices match final dynsym order
    let mut versym_data: Vec<u8> = Vec::new();
    // Entry 0: VER_NDX_LOCAL for the null symbol
    versym_data.extend_from_slice(&0u16.to_le_bytes());
    for name in &dyn_sym_names {
        if let Some(gs) = globals.get(name) {
            if gs.is_dynamic {
                if let Some(ref ver) = gs.version {
                    if let Some(ref lib) = gs.from_lib {
                        let idx = ver_index_map.get(&(lib.clone(), ver.clone()))
                            .copied().unwrap_or(1);
                        versym_data.extend_from_slice(&idx.to_le_bytes());
                    } else {
                        versym_data.extend_from_slice(&1u16.to_le_bytes()); // VER_NDX_GLOBAL
                    }
                } else {
                    versym_data.extend_from_slice(&1u16.to_le_bytes()); // VER_NDX_GLOBAL
                }
            } else if gs.section_idx != SHN_UNDEF && gs.value != 0 {
                // Defined/exported symbol: VER_NDX_GLOBAL
                versym_data.extend_from_slice(&1u16.to_le_bytes());
            } else {
                versym_data.extend_from_slice(&0u16.to_le_bytes()); // VER_NDX_LOCAL
            }
        } else {
            versym_data.extend_from_slice(&0u16.to_le_bytes());
        }
    }

    let versym_size = versym_data.len() as u64;

    // Recompute hashes after sorting
    let hashed_sym_hashes: Vec<u32> = dyn_sym_names[gnu_hash_symoffset - 1..]
        .iter()
        .map(|name| linker_common::gnu_hash(name.as_bytes()))
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
    let gnu_hash_size: u64 = if is_static { 0 } else {
        16 + (gnu_hash_bloom_size as u64 * 8)
            + (gnu_hash_nbuckets as u64 * 4) + (num_hashed as u64 * 4)
    };
    let plt_size = if is_static || plt_names.is_empty() { 0u64 } else { 16 + 16 * plt_names.len() as u64 };
    let got_plt_size = if is_static { 0u64 } else { (3 + plt_names.len()) as u64 * 8 };
    let got_globdat_count = got_entries.iter().filter(|(n, p)| !n.is_empty() && !*p).count();
    let got_size = got_globdat_count as u64 * 8; // GOT needed even for static (TLS, GOTPCREL)

    let has_init_array = output_sections.iter().any(|s| s.name == ".init_array" && s.mem_size > 0);
    let has_fini_array = output_sections.iter().any(|s| s.name == ".fini_array" && s.mem_size > 0);
    let dynamic_size = if is_static { 0u64 } else {
        let mut dyn_count = needed_sonames.len() as u64 + 14; // fixed entries + NULL
        if has_init_array { dyn_count += 2; }
        if has_fini_array { dyn_count += 2; }
        if rpath_string.is_some() { dyn_count += 1; }
        if verneed_size > 0 { dyn_count += 3; } // DT_VERSYM + DT_VERNEED + DT_VERNEEDNUM
        dyn_count * 16
    };
    // Override other dynamic sizes for static linking
    let dynsym_size = if is_static { 0u64 } else { dynsym_size };
    let dynstr_size = if is_static { 0u64 } else { dynstr_size };
    let rela_plt_size = if is_static { 0u64 } else { rela_plt_size };
    let rela_dyn_size = if is_static { 0u64 } else { rela_dyn_size };
    let versym_size = if is_static { 0u64 } else { versym_size };
    let verneed_size = if is_static { 0u64 } else { verneed_size };

    let has_tls_sections = output_sections.iter().any(|s| s.flags & SHF_TLS != 0 && s.flags & SHF_ALLOC != 0);
    // Static: PHDR, LOAD(ro), LOAD(text), LOAD(rodata), LOAD(rw), GNU_STACK, [TLS]
    // Dynamic: PHDR, INTERP, LOAD(ro), LOAD(text), LOAD(rodata), LOAD(rw), DYNAMIC, GNU_STACK, [TLS]
    let phdr_count: u64 = if is_static {
        if has_tls_sections { 7 } else { 6 }
    } else {
        if has_tls_sections { 9 } else { 8 }
    };
    let phdr_total_size = phdr_count * 56;

    // === Layout ===
    let mut offset = 64 + phdr_total_size;
    let interp_offset = offset;
    let interp_addr = BASE_ADDR + offset;
    if !is_static { offset += INTERP.len() as u64; }

    offset = (offset + 7) & !7;
    let gnu_hash_offset = offset; let gnu_hash_addr = BASE_ADDR + offset; offset += gnu_hash_size;
    offset = (offset + 7) & !7;
    let dynsym_offset = offset; let dynsym_addr = BASE_ADDR + offset; offset += dynsym_size;
    let dynstr_offset = offset; let dynstr_addr = BASE_ADDR + offset; offset += dynstr_size;
    // .gnu.version (versym) - right after dynstr, aligned to 2
    offset = (offset + 1) & !1;
    let versym_offset = offset; let versym_addr = BASE_ADDR + offset;
    if versym_size > 0 { offset += versym_size; }
    // .gnu.version_r (verneed) - aligned to 4
    offset = (offset + 3) & !3;
    let verneed_offset = offset; let verneed_addr = BASE_ADDR + offset;
    if verneed_size > 0 { offset += verneed_size; }
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

    // .iplt (IFUNC PLT entries for static linking)
    let num_ifunc = ifunc_symbols.len();
    let iplt_entry_size: u64 = 16; // each IPLT entry: jmp *got(%rip) + padding
    let iplt_total_size = num_ifunc as u64 * iplt_entry_size;
    let (iplt_addr, iplt_offset) = if iplt_total_size > 0 {
        offset = (offset + 15) & !15;
        let a = BASE_ADDR + offset; let o = offset; offset += iplt_total_size; (a, o)
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

    // IFUNC GOT (8 bytes per entry, stores resolver addresses initially)
    offset = (offset + 7) & !7;
    let ifunc_got_offset = offset; let ifunc_got_addr = BASE_ADDR + offset;
    let ifunc_got_size = num_ifunc as u64 * 8;
    offset += ifunc_got_size;

    // .rela.iplt (24 bytes per RELA entry for R_X86_64_IRELATIVE)
    offset = (offset + 7) & !7;
    let rela_iplt_offset = offset; let rela_iplt_addr = BASE_ADDR + offset;
    let rela_iplt_size = num_ifunc as u64 * 24;
    offset += rela_iplt_size;

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
    // If only .tbss (NOBITS TLS) exists with no .tdata, we still need a TLS segment.
    // Set tls_addr/tls_file_offset to the current position so TPOFF calculations work.
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

    // Allocate BSS space for copy-relocated symbols.
    // Symbols that are aliases (same from_lib + lib_sym_value) share the same BSS slot.
    let mut copy_reloc_addr_map: HashMap<(String, u64), u64> = HashMap::new(); // (lib, lib_value) -> bss_addr
    for (name, size) in &copy_reloc_syms {
        let gsym = globals.get(name).cloned();
        let key = gsym.as_ref().and_then(|g| {
            g.from_lib.as_ref().map(|lib| (lib.clone(), g.lib_sym_value))
        });
        let addr = if let Some(ref k) = key {
            if let Some(&existing_addr) = copy_reloc_addr_map.get(k) {
                existing_addr // reuse existing BSS slot for alias
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

    // Define linker-provided symbols using shared infrastructure (consistent
    // with i686/ARM/RISC-V backends via get_standard_linker_symbols)
    let text_seg_end = text_page_addr + text_total_size;
    let data_seg_start = rw_page_addr;
    let linker_addrs = LinkerSymbolAddresses {
        base_addr: BASE_ADDR,
        got_addr: got_plt_addr,
        dynamic_addr,
        bss_addr,
        bss_size,
        text_end: text_seg_end,
        data_start: data_seg_start,
        init_array_start: init_array_addr,
        init_array_size,
        fini_array_start: fini_array_addr,
        fini_array_size,
        preinit_array_start: 0,
        preinit_array_size: 0,
        rela_iplt_start: rela_iplt_addr,
        rela_iplt_size: rela_iplt_size,
    };
    for sym in &get_standard_linker_symbols(&linker_addrs) {
        let entry = globals.entry(sym.name.to_string()).or_insert(GlobalSymbol {
            value: 0, size: 0, info: (sym.binding << 4),
            defined_in: None, from_lib: None, plt_idx: None, got_idx: None,
            section_idx: SHN_ABS, is_dynamic: false, copy_reloc: false, lib_sym_value: 0, version: None,
        });
        if entry.defined_in.is_none() && !entry.is_dynamic {
            entry.value = sym.value;
            entry.defined_in = Some(usize::MAX); // sentinel: linker-defined
            entry.section_idx = SHN_ABS;
        }
    }

    // Override IFUNC symbol addresses to point to IPLT entries.
    // Save the original (resolver) addresses for IFUNC GOT initialization.
    let mut ifunc_resolver_addrs: Vec<u64> = Vec::new();
    for (i, name) in ifunc_symbols.iter().enumerate() {
        if let Some(gsym) = globals.get_mut(name) {
            ifunc_resolver_addrs.push(gsym.value);
            gsym.value = iplt_addr + (i as u64) * iplt_entry_size;
        }
    }

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
    if !is_static {
        wphdr(&mut out, ph, PT_INTERP, PF_R, interp_offset, interp_addr, INTERP.len() as u64, INTERP.len() as u64, 1); ph += 56;
    }
    let ro_seg_end = rela_plt_offset + rela_plt_size;
    wphdr(&mut out, ph, PT_LOAD, PF_R, 0, BASE_ADDR, ro_seg_end, ro_seg_end, PAGE_SIZE); ph += 56;
    wphdr(&mut out, ph, PT_LOAD, PF_R|PF_X, text_page_offset, text_page_addr, text_total_size, text_total_size, PAGE_SIZE); ph += 56;
    wphdr(&mut out, ph, PT_LOAD, PF_R, rodata_page_offset, rodata_page_addr, rodata_total_size, rodata_total_size, PAGE_SIZE); ph += 56;
    let rw_filesz = offset - rw_page_offset;
    let rw_memsz = if bss_size > 0 { (bss_addr + bss_size) - rw_page_addr } else { rw_filesz };
    wphdr(&mut out, ph, PT_LOAD, PF_R|PF_W, rw_page_offset, rw_page_addr, rw_filesz, rw_memsz, PAGE_SIZE); ph += 56;
    if !is_static {
        wphdr(&mut out, ph, PT_DYNAMIC, PF_R|PF_W, dynamic_offset, dynamic_addr, dynamic_size, dynamic_size, 8); ph += 56;
    }
    wphdr(&mut out, ph, PT_GNU_STACK, PF_R|PF_W, 0, 0, 0, 0, 0x10); ph += 56;
    if has_tls {
        wphdr(&mut out, ph, PT_TLS, PF_R, tls_file_offset, tls_addr, tls_file_size, tls_mem_size, tls_align);
    }

    // Section data (needed for both static and dynamic)
    for sec in output_sections.iter() {
        if sec.sh_type == SHT_NOBITS || sec.data.is_empty() { continue; }
        write_bytes(&mut out, sec.file_offset as usize, &sec.data);
    }

    // Dynamic linking sections (skipped for static executables)
    if !is_static {
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
                    if ds+5 < out.len() { out[ds+4] = (STB_GLOBAL << 4) | STT_OBJECT; out[ds+5] = 0; }
                    w16(&mut out, ds+6, 1);
                    w64(&mut out, ds+8, gsym.value);
                    w64(&mut out, ds+16, gsym.size);
                } else if !gsym.is_dynamic && gsym.section_idx != SHN_UNDEF && gsym.value != 0 {
                    let stt = gsym.info & 0xf;
                    let stb = gsym.info >> 4;
                    let st_info = (stb << 4) | stt;
                    if ds+5 < out.len() { out[ds+4] = st_info; out[ds+5] = 0; }
                    w16(&mut out, ds+6, 1);
                    w64(&mut out, ds+8, gsym.value);
                    w64(&mut out, ds+16, gsym.size);
                } else {
                    let bind = gsym.info >> 4;
                    let stype = gsym.info & 0xf;
                    let st_info = (bind << 4) | if stype != 0 { stype } else { STT_FUNC };
                    if ds+5 < out.len() { out[ds+4] = st_info; out[ds+5] = 0; }
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

        // .gnu.version (versym)
        if !versym_data.is_empty() {
            write_bytes(&mut out, versym_offset as usize, &versym_data);
        }

        // .gnu.version_r (verneed)
        if !verneed_data.is_empty() {
            write_bytes(&mut out, verneed_offset as usize, &verneed_data);
        }

        // .rela.dyn (GLOB_DAT for dynamic GOT symbols, R_X86_64_COPY for copy relocs)
        let mut rd = rela_dyn_offset as usize;
        let mut gd_a = got_addr;
        for (name, is_plt) in got_entries {
            if name.is_empty() || *is_plt { continue; }
            let is_dynamic = globals.get(name).map(|g| g.is_dynamic && !g.copy_reloc).unwrap_or(false);
            if is_dynamic {
                let si = dyn_sym_names.iter().position(|n| n == name).map(|i| i+1).unwrap_or(0) as u64;
                w64(&mut out, rd, gd_a); w64(&mut out, rd+8, (si << 32) | R_X86_64_GLOB_DAT as u64); w64(&mut out, rd+16, 0);
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
            w64(&mut out, rp, gea); w64(&mut out, rp+8, (si << 32) | R_X86_64_JUMP_SLOT as u64); w64(&mut out, rp+16, 0);
            rp += 24;
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
            (DT_PLTRELSZ, rela_plt_size), (DT_PLTREL, DT_RELA as u64), (DT_JMPREL, rela_plt_addr),
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
        if let Some(ref rp) = rpath_string {
            let rp_off = dynstr.get_offset(rp) as u64;
            let tag = if use_runpath { DT_RUNPATH } else { DT_RPATH };
            w64(&mut out, dd, tag as u64); w64(&mut out, dd+8, rp_off); dd += 16;
        }
        if verneed_size > 0 {
            w64(&mut out, dd, DT_VERSYM as u64); w64(&mut out, dd+8, versym_addr); dd += 16;
            w64(&mut out, dd, DT_VERNEED as u64); w64(&mut out, dd+8, verneed_addr); dd += 16;
            w64(&mut out, dd, DT_VERNEEDNUM as u64); w64(&mut out, dd+8, verneed_count as u64); dd += 16;
        }
        w64(&mut out, dd, DT_NULL as u64); w64(&mut out, dd+8, 0);

        // .got.plt
        let gp = got_plt_offset as usize;
        w64(&mut out, gp, dynamic_addr);
        w64(&mut out, gp+8, 0); w64(&mut out, gp+16, 0);
        for (i, _) in plt_names.iter().enumerate() {
            w64(&mut out, gp+24+i*8, plt_addr + 16 + i as u64 * 16 + 6);
        }
    } // end if !is_static

    // .got (needed for both static and dynamic: TLS GOTTPOFF, GOTPCREL entries)
    if got_size > 0 {
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
                } else if gsym.copy_reloc && gsym.value != 0 {
                    w64(&mut out, go, gsym.value);
                }
            }
            go += 8;
        }
    }

    // IFUNC: write .iplt, IFUNC GOT, and .rela.iplt data
    if num_ifunc > 0 {
        // .iplt - each entry is: jmp *ifunc_got_entry(%rip); nop padding
        for i in 0..num_ifunc {
            let ep = iplt_offset as usize + i * iplt_entry_size as usize;
            let pea = iplt_addr + i as u64 * iplt_entry_size; // address of this IPLT entry
            let gea = ifunc_got_addr + i as u64 * 8; // address of IFUNC GOT entry
            // ff 25 XX XX XX XX = jmp *disp32(%rip)
            out[ep] = 0xff; out[ep+1] = 0x25;
            w32(&mut out, ep+2, (gea as i64 - (pea + 6) as i64) as u32);
            // Pad remaining 10 bytes with NOPs
            for j in 6..iplt_entry_size as usize { out[ep+j] = 0x90; }
        }

        // IFUNC GOT - initialized to resolver function addresses
        for (i, &resolver_addr) in ifunc_resolver_addrs.iter().enumerate() {
            let go = ifunc_got_offset as usize + i * 8;
            w64(&mut out, go, resolver_addr);
        }

        // .rela.iplt - R_X86_64_IRELATIVE relocations
        for i in 0..num_ifunc {
            let rp = rela_iplt_offset as usize + i * 24;
            let r_offset = ifunc_got_addr + i as u64 * 8;
            // r_info: (0 << 32) | R_X86_64_IRELATIVE
            w64(&mut out, rp, r_offset);
            w64(&mut out, rp+8, R_X86_64_IRELATIVE as u64);
            w64(&mut out, rp+16, ifunc_resolver_addrs[i]); // r_addend = resolver address
        }
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
                        let t = if !sym.name.is_empty() && !sym.is_local() {
                            if let Some(g) = globals_snap.get(&sym.name) {
                                if g.is_dynamic && !g.copy_reloc {
                                    if let Some(pi) = g.plt_idx { plt_addr + 16 + pi as u64 * 16 } else { s }
                                } else { s }
                            } else { s }
                        } else { s };
                        w64(&mut out, fp, (t as i64 + a) as u64);
                    }
                    R_X86_64_PC32 | R_X86_64_PLT32 => {
                        let t = if !sym.name.is_empty() && !sym.is_local() {
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
                        if !sym.name.is_empty() && !sym.is_local() {
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
                        if !sym.name.is_empty() && !sym.is_local() {
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

    // === Append section headers ===
    // Build .shstrtab string table
    let mut shstrtab = vec![0u8]; // null byte at offset 0
    let mut shstr_offsets: HashMap<String, u32> = HashMap::new();
    let known_names = [
        ".interp", ".gnu.hash", ".dynsym", ".dynstr",
        ".gnu.version", ".gnu.version_r",
        ".rela.dyn", ".rela.plt", ".plt", ".dynamic",
        ".got", ".got.plt", ".init_array", ".fini_array",
        ".tdata", ".tbss", ".bss", ".shstrtab",
        ".iplt", ".rela.iplt",
    ];
    for name in &known_names {
        let off = shstrtab.len() as u32;
        shstr_offsets.insert(name.to_string(), off);
        shstrtab.extend_from_slice(name.as_bytes());
        shstrtab.push(0);
    }
    // Add merged section names not already in known list
    for sec in output_sections.iter() {
        if !sec.name.is_empty() && !shstr_offsets.contains_key(&sec.name) {
            let off = shstrtab.len() as u32;
            shstr_offsets.insert(sec.name.clone(), off);
            shstrtab.extend_from_slice(sec.name.as_bytes());
            shstrtab.push(0);
        }
    }

    let get_shname = |n: &str| -> u32 { shstr_offsets.get(n).copied().unwrap_or(0) };

    // Use shared write_elf64_shdr from linker_common (aliased locally for brevity)
    let write_shdr = linker_common::write_elf64_shdr;

    // Pre-count section indices for cross-references (dynsym_shidx, dynstr_shidx)
    // These are only meaningful for dynamic linking, but define them unconditionally for convenience
    let dynsym_shidx: u32 = if is_static { 0 } else { 3 }; // NULL=0, .interp=1, .gnu.hash=2, .dynsym=3
    let dynstr_shidx: u32 = if is_static { 0 } else { 4 }; // .dynstr=4

    // Count total sections to determine .shstrtab index
    let mut sh_count: u16 = if is_static {
        1 // NULL only
    } else {
        5 // NULL + .interp + .gnu.hash + .dynsym + .dynstr
    };
    if !is_static && verneed_size > 0 { sh_count += 2; } // .gnu.version + .gnu.version_r
    if !is_static && rela_dyn_size > 0 { sh_count += 1; }
    if !is_static && rela_plt_size > 0 { sh_count += 1; }
    if !is_static && plt_size > 0 { sh_count += 1; }
    // Merged output sections (non-BSS, non-TLS, non-init/fini)
    for sec in output_sections.iter() {
        if sec.flags & SHF_ALLOC != 0 && sec.sh_type != SHT_NOBITS && sec.flags & SHF_TLS == 0
           && sec.name != ".init_array" && sec.name != ".fini_array" {
            sh_count += 1;
        }
    }
    // TLS data + TLS BSS
    for sec in output_sections.iter() {
        if sec.flags & SHF_TLS != 0 && sec.flags & SHF_ALLOC != 0 && sec.sh_type != SHT_NOBITS { sh_count += 1; }
    }
    for sec in output_sections.iter() {
        if sec.flags & SHF_TLS != 0 && sec.sh_type == SHT_NOBITS { sh_count += 1; }
    }
    if has_init_array { sh_count += 1; }
    if has_fini_array { sh_count += 1; }
    if !is_static { sh_count += 1; } // .dynamic
    if got_size > 0 { sh_count += 1; } // .got (needed for static too: TLS, GOTPCREL)
    if !is_static { sh_count += 1; } // .got.plt
    if iplt_total_size > 0 { sh_count += 1; } // .iplt
    if rela_iplt_size > 0 { sh_count += 1; } // .rela.iplt
    // BSS sections (non-TLS)
    for sec in output_sections.iter() {
        if sec.sh_type == SHT_NOBITS && sec.flags & SHF_ALLOC != 0 && sec.flags & SHF_TLS == 0 { sh_count += 1; }
    }
    let shstrtab_shidx = sh_count; // .shstrtab is the last section
    sh_count += 1;

    // Align and append .shstrtab data
    while !out.len().is_multiple_of(8) { out.push(0); }
    let shstrtab_data_offset = out.len() as u64;
    out.extend_from_slice(&shstrtab);

    // Align section header table to 8 bytes
    while !out.len().is_multiple_of(8) { out.push(0); }
    let shdr_offset = out.len() as u64;

    // Write section headers
    // [0] NULL
    write_shdr(&mut out, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    // Dynamic linking section headers (skipped for static executables)
    if !is_static {
        // .interp
        write_shdr(&mut out, get_shname(".interp"), SHT_PROGBITS, SHF_ALLOC,
                   interp_addr, interp_offset, INTERP.len() as u64, 0, 0, 1, 0);
        // .gnu.hash
        write_shdr(&mut out, get_shname(".gnu.hash"), SHT_GNU_HASH, SHF_ALLOC,
                   gnu_hash_addr, gnu_hash_offset, gnu_hash_size, dynsym_shidx, 0, 8, 0);
        // .dynsym
        write_shdr(&mut out, get_shname(".dynsym"), SHT_DYNSYM, SHF_ALLOC,
                   dynsym_addr, dynsym_offset, dynsym_size, dynstr_shidx, 1, 8, 24);
        // .dynstr
        write_shdr(&mut out, get_shname(".dynstr"), SHT_STRTAB, SHF_ALLOC,
                   dynstr_addr, dynstr_offset, dynstr_size, 0, 0, 1, 0);
        // .gnu.version (versym)
        if verneed_size > 0 {
            write_shdr(&mut out, get_shname(".gnu.version"), SHT_GNU_VERSYM, SHF_ALLOC,
                       versym_addr, versym_offset, versym_size, dynsym_shidx, 0, 2, 2);
        }
        // .gnu.version_r (verneed)
        if verneed_size > 0 {
            write_shdr(&mut out, get_shname(".gnu.version_r"), SHT_GNU_VERNEED, SHF_ALLOC,
                       verneed_addr, verneed_offset, verneed_size, dynstr_shidx, verneed_count, 4, 0);
        }
        // .rela.dyn
        if rela_dyn_size > 0 {
            write_shdr(&mut out, get_shname(".rela.dyn"), SHT_RELA, SHF_ALLOC,
                       rela_dyn_addr, rela_dyn_offset, rela_dyn_size, dynsym_shidx, 0, 8, 24);
        }
        // .rela.plt
        if rela_plt_size > 0 {
            write_shdr(&mut out, get_shname(".rela.plt"), SHT_RELA, SHF_ALLOC | 0x40,
                       rela_plt_addr, rela_plt_offset, rela_plt_size, dynsym_shidx, 0, 8, 24);
        }
        // .plt
        if plt_size > 0 {
            write_shdr(&mut out, get_shname(".plt"), SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                       plt_addr, plt_offset, plt_size, 0, 0, 16, 16);
        }
    }
    // Merged output sections (text/rodata/data, excluding BSS/TLS/init_array/fini_array)
    for sec in output_sections.iter() {
        if sec.flags & SHF_ALLOC != 0 && sec.sh_type != SHT_NOBITS && sec.flags & SHF_TLS == 0
           && sec.name != ".init_array" && sec.name != ".fini_array" {
            write_shdr(&mut out, get_shname(&sec.name), sec.sh_type, sec.flags,
                       sec.addr, sec.file_offset, sec.mem_size, 0, 0, sec.alignment.max(1), 0);
        }
    }
    // TLS data sections (.tdata)
    for sec in output_sections.iter() {
        if sec.flags & SHF_TLS != 0 && sec.flags & SHF_ALLOC != 0 && sec.sh_type != SHT_NOBITS {
            write_shdr(&mut out, get_shname(&sec.name), sec.sh_type, sec.flags,
                       sec.addr, sec.file_offset, sec.mem_size, 0, 0, sec.alignment.max(1), 0);
        }
    }
    // TLS BSS sections (.tbss)
    for sec in output_sections.iter() {
        if sec.flags & SHF_TLS != 0 && sec.sh_type == SHT_NOBITS {
            write_shdr(&mut out, get_shname(&sec.name), SHT_NOBITS, sec.flags,
                       sec.addr, sec.file_offset, sec.mem_size, 0, 0, sec.alignment.max(1), 0);
        }
    }
    // .init_array
    if has_init_array {
        if let Some(ia_sec) = output_sections.iter().find(|s| s.name == ".init_array") {
            write_shdr(&mut out, get_shname(".init_array"), SHT_INIT_ARRAY, SHF_ALLOC | SHF_WRITE,
                       init_array_addr, ia_sec.file_offset, init_array_size, 0, 0, 8, 8);
        }
    }
    // .fini_array
    if has_fini_array {
        if let Some(fa_sec) = output_sections.iter().find(|s| s.name == ".fini_array") {
            write_shdr(&mut out, get_shname(".fini_array"), SHT_FINI_ARRAY, SHF_ALLOC | SHF_WRITE,
                       fini_array_addr, fa_sec.file_offset, fini_array_size, 0, 0, 8, 8);
        }
    }
    if !is_static {
        // .dynamic
        write_shdr(&mut out, get_shname(".dynamic"), SHT_DYNAMIC, SHF_ALLOC | SHF_WRITE,
                   dynamic_addr, dynamic_offset, dynamic_size, dynstr_shidx, 0, 8, 16);
    }
    // .got (needed for both static and dynamic: TLS GOTTPOFF, GOTPCREL)
    if got_size > 0 {
        write_shdr(&mut out, get_shname(".got"), SHT_PROGBITS, SHF_ALLOC | SHF_WRITE,
                   got_addr, got_offset, got_size, 0, 0, 8, 8);
    }
    if !is_static {
        // .got.plt
        write_shdr(&mut out, get_shname(".got.plt"), SHT_PROGBITS, SHF_ALLOC | SHF_WRITE,
                   got_plt_addr, got_plt_offset, got_plt_size, 0, 0, 8, 8);
    }
    // .iplt (IFUNC PLT for static linking)
    if iplt_total_size > 0 {
        write_shdr(&mut out, get_shname(".iplt"), SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR,
                   iplt_addr, iplt_offset, iplt_total_size, 0, 0, 16, 16);
    }
    // .rela.iplt (IRELATIVE relocations for static linking)
    if rela_iplt_size > 0 {
        write_shdr(&mut out, get_shname(".rela.iplt"), SHT_RELA, SHF_ALLOC,
                   rela_iplt_addr, rela_iplt_offset, rela_iplt_size, 0, 0, 8, 24);
    }
    // BSS sections (non-TLS)
    for sec in output_sections.iter() {
        if sec.sh_type == SHT_NOBITS && sec.flags & SHF_ALLOC != 0 && sec.flags & SHF_TLS == 0 {
            write_shdr(&mut out, get_shname(&sec.name), SHT_NOBITS, sec.flags,
                       sec.addr, sec.file_offset, sec.mem_size, 0, 0, sec.alignment.max(1), 0);
        }
    }
    // .shstrtab (last section)
    write_shdr(&mut out, get_shname(".shstrtab"), SHT_STRTAB, 0,
               0, shstrtab_data_offset, shstrtab.len() as u64, 0, 0, 1, 0);

    // Patch ELF header with section header info
    // e_shoff at offset 40 (8 bytes)
    out[40..48].copy_from_slice(&shdr_offset.to_le_bytes());
    // e_shnum at offset 60 (2 bytes)
    out[60..62].copy_from_slice(&sh_count.to_le_bytes());
    // e_shstrndx at offset 62 (2 bytes)
    out[62..64].copy_from_slice(&shstrtab_shidx.to_le_bytes());

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
    // Local (STB_LOCAL) symbols must NOT be resolved via globals, since a
    // local symbol named e.g. "opts" must not be confused with a global "opts"
    // from another object file.
    if !sym.name.is_empty() && !sym.is_local() {
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
