//! Native i686 (32-bit x86) ELF linker.
//!
//! Reads ELF32 relocatable object files (.o) and archives (.a), resolves
//! symbols against system shared libraries, applies i386 relocations, and
//! produces a dynamically-linked ELF32 executable.
//!
//! Supported relocations:
//! - R_386_32 (absolute 32-bit)
//! - R_386_PC32 (PC-relative 32-bit)
//! - R_386_PLT32 (PLT-relative, same as PC32 for defined symbols)
//! - R_386_GOTPC (PC-relative offset to GOT base)
//! - R_386_GOTOFF (offset from GOT base)
//! - R_386_GOT32X (relaxable GOT entry reference)
//! - R_386_GOT32 (GOT entry reference)
//! - R_386_NONE (no-op)

mod elf;

/// Built-in linker entry point with pre-resolved CRT objects and library paths.
///
/// This mirrors the x86-64 linker API: the caller (common.rs) discovers CRT
/// objects, GCC lib dirs, and system library paths using DirectLdArchConfig,
/// then passes them in so the linker focuses purely on ELF linking.
pub fn link_builtin(
    object_files: &[&str],
    output_path: &str,
    user_args: &[String],
    lib_paths: &[&str],
    needed_libs: &[&str],
    crt_objects_before: &[&str],
    crt_objects_after: &[&str],
) -> Result<(), String> {
    elf::link_builtin(
        object_files, output_path, user_args,
        lib_paths, needed_libs, crt_objects_before, crt_objects_after,
    )
}
