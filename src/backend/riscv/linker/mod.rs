//! Native RISC-V 64-bit ELF linker.
//!
//! Reads ELF .o relocatable files and .a static archives, resolves symbols,
//! applies RISC-V relocations, generates PLT/GOT for dynamic symbols, and
//! emits a dynamically-linked ELF executable.
//!
//! This is the default linker (used when the `gcc_linker` feature is disabled).
//!
//! CRT object discovery and library path resolution are handled by
//! common.rs's `resolve_builtin_link_setup`.

mod elf_read;
mod link;

pub use link::link_builtin;
pub use link::link_shared;
