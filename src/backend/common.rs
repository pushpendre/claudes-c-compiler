//! Shared backend utilities for assembler, linker, and data emission.
//!
//! All three backends (x86-64, AArch64, RISC-V 64) share identical logic for:
//! - Assembling via an external toolchain (gcc/cross-gcc)
//! - Linking via an external toolchain
//! - Emitting assembly data directives (.data, .bss, .rodata, string literals, constants)
//!
//! This module extracts that shared logic, parameterized only by:
//! - The toolchain command name (e.g., "gcc" vs "aarch64-linux-gnu-gcc")
//! - The 64-bit data directive (`.quad` vs `.xword` vs `.dword`)
//! - Extra assembler/linker flags

use std::process::Command;
use crate::ir::ir::*;
use crate::common::types::IrType;

/// Configuration for an external assembler.
pub struct AssemblerConfig {
    /// The assembler command (e.g., "gcc", "aarch64-linux-gnu-gcc")
    pub command: &'static str,
    /// Extra flags to pass (e.g., ["-march=rv64gc", "-mabi=lp64d"] for RISC-V)
    pub extra_args: &'static [&'static str],
}

/// Configuration for an external linker.
pub struct LinkerConfig {
    /// The linker command (e.g., "gcc", "aarch64-linux-gnu-gcc")
    pub command: &'static str,
    /// Extra flags (e.g., ["-static"] for cross-compiled targets, ["-no-pie"] for x86)
    pub extra_args: &'static [&'static str],
}

/// Assemble text to an object file using an external toolchain.
///
/// Uses a unique temporary file for the assembly source to avoid race conditions
/// when multiple compiler instances target the same output path in parallel builds.
pub fn assemble(config: &AssemblerConfig, asm_text: &str, output_path: &str) -> Result<(), String> {
    // Use a unique temp file to avoid races in parallel builds.
    // Include PID and a counter to guarantee uniqueness even within the same process.
    use std::sync::atomic::{AtomicU64, Ordering};
    static ASM_COUNTER: AtomicU64 = AtomicU64::new(0);
    let unique_id = ASM_COUNTER.fetch_add(1, Ordering::Relaxed);
    let pid = std::process::id();

    let asm_path = if std::env::var("CCC_KEEP_ASM").is_ok() {
        // When keeping assembly, use the predictable name for debugging
        format!("{}.s", output_path)
    } else {
        // Use a unique name to avoid parallel build races
        format!("{}.{}.{}.s", output_path, pid, unique_id)
    };

    std::fs::write(&asm_path, asm_text)
        .map_err(|e| format!("Failed to write assembly: {}", e))?;

    let mut cmd = Command::new(config.command);
    cmd.args(config.extra_args);
    cmd.args(["-c", "-o", output_path, &asm_path]);

    let result = cmd.output()
        .map_err(|e| format!("Failed to run assembler ({}): {}", config.command, e))?;

    if std::env::var("CCC_KEEP_ASM").is_err() {
        let _ = std::fs::remove_file(&asm_path);
    }

    if !result.status.success() {
        let stderr = String::from_utf8_lossy(&result.stderr);
        return Err(format!("Assembly failed ({}): {}", config.command, stderr));
    }

    Ok(())
}

/// Link object files into an executable using an external toolchain.
pub fn link(config: &LinkerConfig, object_files: &[&str], output_path: &str) -> Result<(), String> {
    link_with_args(config, object_files, output_path, &[])
}

/// Link object files into an executable (or shared library), with additional user-provided linker args.
pub fn link_with_args(config: &LinkerConfig, object_files: &[&str], output_path: &str, user_args: &[String]) -> Result<(), String> {
    let is_shared = user_args.iter().any(|a| a == "-shared");
    let is_nostdlib = user_args.iter().any(|a| a == "-nostdlib");

    let mut cmd = Command::new(config.command);
    // Skip -no-pie when building shared libraries (they conflict)
    for arg in config.extra_args {
        if is_shared && (*arg == "-no-pie" || *arg == "-pie") {
            continue;
        }
        cmd.arg(arg);
    }
    cmd.arg("-o").arg(output_path);

    for obj in object_files {
        cmd.arg(obj);
    }

    // Add user-provided linker args (-l, -L, -static, -shared, -Wl, pass-through, etc.)
    for arg in user_args {
        cmd.arg(arg);
    }

    // Default libs (skip for -nostdlib and -shared; only add if not already specified)
    if !is_nostdlib && !is_shared {
        if !user_args.iter().any(|a| a == "-lc") {
            cmd.arg("-lc");
        }
        if !user_args.iter().any(|a| a == "-lm") {
            cmd.arg("-lm");
        }
    }

    let result = cmd.output()
        .map_err(|e| format!("Failed to run linker ({}): {}", config.command, e))?;

    if !result.status.success() {
        let stderr = String::from_utf8_lossy(&result.stderr);
        return Err(format!("Linking failed ({}): {}", config.command, stderr));
    }

    Ok(())
}

/// Assembly output buffer with helpers for emitting text.
pub struct AsmOutput {
    pub buf: String,
}

impl AsmOutput {
    pub fn new() -> Self {
        // Pre-allocate 256KB to avoid repeated reallocations during codegen.
        // A typical large C file (sqlite3.c) produces ~2MB of assembly.
        Self { buf: String::with_capacity(256 * 1024) }
    }

    /// Emit a line of assembly.
    pub fn emit(&mut self, s: &str) {
        self.buf.push_str(s);
        self.buf.push('\n');
    }

    /// Emit formatted assembly directly into the buffer (no temporary String).
    #[inline]
    pub fn emit_fmt(&mut self, args: std::fmt::Arguments<'_>) {
        std::fmt::Write::write_fmt(&mut self.buf, args).unwrap();
        self.buf.push('\n');
    }
}

/// Emit formatted assembly directly into the output buffer, avoiding temporary
/// String allocations from `format!()`. Usage: `emit!(state, "    mov {}, {}", src, dst)`
#[macro_export]
macro_rules! emit {
    ($state:expr, $($arg:tt)*) => {
        $state.out.emit_fmt(format_args!($($arg)*))
    };
}

/// The only arch-specific difference in data emission: the name of the 64-bit pointer directive.
/// x86 uses `.quad`, AArch64 uses `.xword`, RISC-V uses `.dword`.
#[derive(Clone, Copy)]
pub enum PtrDirective {
    Quad,   // x86-64
    Xword,  // AArch64
    Dword,  // RISC-V 64
}

impl PtrDirective {
    pub fn as_str(self) -> &'static str {
        match self {
            PtrDirective::Quad => ".quad",
            PtrDirective::Xword => ".xword",
            PtrDirective::Dword => ".dword",
        }
    }
}

/// Emit all data sections (rodata for string literals, .data and .bss for globals).
pub fn emit_data_sections(out: &mut AsmOutput, module: &IrModule, ptr_dir: PtrDirective) {
    // String literals in .rodata
    if !module.string_literals.is_empty() || !module.wide_string_literals.is_empty() {
        out.emit(".section .rodata");
        for (label, value) in &module.string_literals {
            out.emit_fmt(format_args!("{}:", label));
            emit_string_bytes(out, value);
        }
        // Wide string literals (L"..."): each char is a 4-byte wchar_t value
        for (label, chars) in &module.wide_string_literals {
            out.emit_fmt(format_args!(".align 4"));
            out.emit_fmt(format_args!("{}:", label));
            for &ch in chars {
                out.emit_fmt(format_args!("  .long {}", ch));
            }
        }
        out.emit("");
    }

    // Global variables
    emit_globals(out, &module.globals, ptr_dir);
}

/// Compute effective alignment for a global, promoting to 16 when size >= 16.
/// This matches GCC/Clang behavior on x86-64 and aarch64, enabling aligned SSE/NEON access.
fn effective_align(g: &IrGlobal) -> usize {
    if g.size >= 16 && g.align < 16 {
        16
    } else {
        g.align
    }
}

/// Emit global variable definitions split into .data and .bss sections.
fn emit_globals(out: &mut AsmOutput, globals: &[IrGlobal], ptr_dir: PtrDirective) {
    let mut has_data = false;
    let mut has_bss = false;

    // Initialized globals -> .data
    for g in globals {
        if g.is_extern {
            continue; // extern declarations have no storage
        }
        if matches!(g.init, GlobalInit::Zero) {
            continue;
        }
        // Zero-size globals (e.g., empty arrays like `Type arr[0] = {}`) go to .bss
        // to avoid sharing an address with the next .data global.
        if g.size == 0 {
            continue;
        }
        if !has_data {
            out.emit(".section .data");
            has_data = true;
        }
        emit_global_def(out, g, ptr_dir);
    }
    if has_data {
        out.emit("");
    }

    // Common globals -> .comm directive (weak linkage, linker merges duplicates)
    for g in globals {
        if g.is_extern {
            continue;
        }
        if !g.is_common || !matches!(g.init, GlobalInit::Zero) {
            continue;
        }
        out.emit_fmt(format_args!(".comm {},{},{}", g.name, g.size, effective_align(g)));
    }

    // Zero-initialized globals -> .bss
    // Also includes zero-size globals with empty initializers (e.g., `Type arr[0] = {}`)
    // which were skipped from .data to avoid address overlap.
    for g in globals {
        if g.is_extern {
            continue; // extern declarations have no storage
        }
        let is_zero_init = matches!(g.init, GlobalInit::Zero);
        let is_zero_size_with_init = g.size == 0 && !is_zero_init;
        if !is_zero_init && !is_zero_size_with_init {
            continue;
        }
        if g.is_common {
            continue; // already emitted as .comm
        }
        if !has_bss {
            out.emit(".section .bss");
            has_bss = true;
        }
        if !g.is_static {
            out.emit_fmt(format_args!(".globl {}", g.name));
        }
        out.emit_fmt(format_args!(".align {}", effective_align(g)));
        out.emit_fmt(format_args!(".type {}, @object", g.name));
        out.emit_fmt(format_args!(".size {}, {}", g.name, g.size));
        out.emit_fmt(format_args!("{}:", g.name));
        out.emit_fmt(format_args!("    .zero {}", g.size));
    }
    if has_bss {
        out.emit("");
    }
}

/// Emit a single global variable definition.
fn emit_global_def(out: &mut AsmOutput, g: &IrGlobal, ptr_dir: PtrDirective) {
    if !g.is_static {
        out.emit_fmt(format_args!(".globl {}", g.name));
    }
    out.emit_fmt(format_args!(".align {}", effective_align(g)));
    out.emit_fmt(format_args!(".type {}, @object", g.name));
    out.emit_fmt(format_args!(".size {}, {}", g.name, g.size));
    out.emit_fmt(format_args!("{}:", g.name));

    match &g.init {
        GlobalInit::Zero => {
            out.emit_fmt(format_args!("    .zero {}", g.size));
        }
        GlobalInit::Scalar(c) => {
            emit_const_data(out, c, g.ty, ptr_dir);
        }
        GlobalInit::Array(values) => {
            for val in values {
                // Use the constant's own type for byte-serialized struct/array data
                let elem_ty = match val {
                    IrConst::I8(_) => IrType::I8,
                    IrConst::I16(_) => IrType::I16,
                    IrConst::I32(_) => IrType::I32,
                    IrConst::F32(_) => IrType::F32,
                    IrConst::F64(_) => IrType::F64,
                    IrConst::LongDouble(_) => IrType::F128, // LongDouble emits 16 bytes
                    _ => g.ty,
                };
                emit_const_data(out, val, elem_ty, ptr_dir);
            }
        }
        GlobalInit::String(s) => {
            out.emit_fmt(format_args!("    .asciz \"{}\"", escape_string(s)));
            let string_bytes = s.len() + 1; // string + null terminator
            if g.size > string_bytes {
                out.emit_fmt(format_args!("    .zero {}", g.size - string_bytes));
            }
        }
        GlobalInit::WideString(chars) => {
            // Emit wide string as array of .long values (4 bytes each)
            for &ch in chars {
                out.emit_fmt(format_args!("    .long {}", ch));
            }
            // Null terminator
            out.emit("    .long 0");
            let wide_bytes = (chars.len() + 1) * 4;
            if g.size > wide_bytes {
                out.emit_fmt(format_args!("    .zero {}", g.size - wide_bytes));
            }
        }
        GlobalInit::GlobalAddr(label) => {
            out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), label));
        }
        GlobalInit::GlobalAddrOffset(label, offset) => {
            if *offset >= 0 {
                out.emit_fmt(format_args!("    {} {}+{}", ptr_dir.as_str(), label, offset));
            } else {
                out.emit_fmt(format_args!("    {} {}{}", ptr_dir.as_str(), label, offset));
            }
        }
        GlobalInit::Compound(elements) => {
            for elem in elements {
                match elem {
                    GlobalInit::Scalar(c) => {
                        // Use the const's own type for non-I64 scalars (e.g., I8 in struct byte init)
                        let elem_ty = match c {
                            IrConst::I8(_) => IrType::I8,
                            IrConst::I16(_) => IrType::I16,
                            IrConst::I32(_) => IrType::I32,
                            _ => g.ty,
                        };
                        emit_const_data(out, c, elem_ty, ptr_dir);
                    }
                    GlobalInit::GlobalAddr(label) => {
                        out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), label));
                    }
                    GlobalInit::GlobalAddrOffset(label, offset) => {
                        if *offset >= 0 {
                            out.emit_fmt(format_args!("    {} {}+{}", ptr_dir.as_str(), label, offset));
                        } else {
                            out.emit_fmt(format_args!("    {} {}{}", ptr_dir.as_str(), label, offset));
                        }
                    }
                    GlobalInit::WideString(chars) => {
                        for &ch in chars {
                            out.emit_fmt(format_args!("    .long {}", ch));
                        }
                        out.emit("    .long 0"); // null terminator
                    }
                    GlobalInit::Zero => {
                        // Emit a zero-initialized element of the appropriate size
                        out.emit_fmt(format_args!("    {} 0", ptr_dir.as_str()));
                    }
                    _ => {
                        // Nested compound or other - emit zero as fallback
                        out.emit_fmt(format_args!("    {} 0", ptr_dir.as_str()));
                    }
                }
            }
        }
    }
}

/// Emit a constant data value as an assembly directive.
pub fn emit_const_data(out: &mut AsmOutput, c: &IrConst, ty: IrType, ptr_dir: PtrDirective) {
    // If the constant type is narrower than the global's declared type, widen it
    // (sign-extend for signed, zero-extend for unsigned).
    match c {
        IrConst::I8(v) => {
            match ty {
                IrType::I8 | IrType::U8 => out.emit_fmt(format_args!("    .byte {}", *v as u8)),
                IrType::I16 | IrType::U16 => out.emit_fmt(format_args!("    .short {}", *v as i16 as u16)),
                IrType::I32 | IrType::U32 => out.emit_fmt(format_args!("    .long {}", *v as i32 as u32)),
                _ => out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), *v as i64)),
            }
        }
        IrConst::I16(v) => {
            match ty {
                IrType::I8 | IrType::U8 => out.emit_fmt(format_args!("    .byte {}", *v as u8)),
                IrType::I16 | IrType::U16 => out.emit_fmt(format_args!("    .short {}", *v as u16)),
                IrType::I32 | IrType::U32 => out.emit_fmt(format_args!("    .long {}", *v as i32 as u32)),
                _ => out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), *v as i64)),
            }
        }
        IrConst::I32(v) => {
            match ty {
                IrType::I8 | IrType::U8 => out.emit_fmt(format_args!("    .byte {}", *v as u8)),
                IrType::I16 | IrType::U16 => out.emit_fmt(format_args!("    .short {}", *v as u16)),
                IrType::I32 | IrType::U32 => out.emit_fmt(format_args!("    .long {}", *v as u32)),
                _ => out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), *v as i64)),
            }
        }
        IrConst::I64(v) => {
            match ty {
                IrType::I8 | IrType::U8 => out.emit_fmt(format_args!("    .byte {}", *v as u8)),
                IrType::I16 | IrType::U16 => out.emit_fmt(format_args!("    .short {}", *v as u16)),
                IrType::I32 | IrType::U32 => out.emit_fmt(format_args!("    .long {}", *v as u32)),
                _ => out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), v)),
            }
        }
        IrConst::F32(v) => {
            out.emit_fmt(format_args!("    .long {}", v.to_bits()));
        }
        IrConst::F64(v) => {
            out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), v.to_bits()));
        }
        IrConst::LongDouble(v) => {
            // Store as f64 bit pattern in the lower 8 bytes, with 8 bytes zero padding.
            // The ARM64/RISC-V codegen loads long doubles as f64 values (F128 is treated
            // as F64 at computation level), so the data must match the load format.
            // For function call arguments, f64->f128 conversion is handled by __extenddftf2.
            out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), v.to_bits()));
            out.emit_fmt(format_args!("    {} 0", ptr_dir.as_str()));
        }
        IrConst::I128(v) => {
            // Emit as two 64-bit values (little-endian: low quad first)
            let lo = *v as u64;
            let hi = (*v >> 64) as u64;
            out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), lo as i64));
            out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), hi as i64));
        }
        IrConst::Zero => {
            let size = ty.size();
            out.emit_fmt(format_args!("    .zero {}", if size == 0 { 4 } else { size }));
        }
    }
}

/// Emit string literal as .byte directives with null terminator.
/// Each char in the string is treated as a raw byte value (0-255),
/// not as a UTF-8 encoded character. This is correct for C narrow
/// string literals where \xNN escapes produce single bytes.
pub fn emit_string_bytes(out: &mut AsmOutput, s: &str) {
    let bytes: Vec<String> = s.chars()
        .map(|c| c as u8)
        .chain(std::iter::once(0u8))
        .map(|b| format!("{}", b))
        .collect();
    out.emit_fmt(format_args!("    .byte {}", bytes.join(", ")));
}

/// Escape a string for use in assembly .asciz directives.
pub fn escape_string(s: &str) -> String {
    let mut result = String::new();
    for c in s.chars() {
        match c {
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            '\n' => result.push_str("\\n"),
            '\t' => result.push_str("\\t"),
            '\r' => result.push_str("\\r"),
            '\0' => result.push_str("\\0"),
            c if c.is_ascii_graphic() || c == ' ' => result.push(c),
            c => {
                // Emit the raw byte value (char as u8), not UTF-8 encoding
                result.push_str(&format!("\\{:03o}", c as u8));
            }
        }
    }
    result
}

