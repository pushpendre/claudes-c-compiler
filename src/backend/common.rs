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
        // Use /tmp/ for temp assembly files to handle cases like -o /dev/null
        // where the output directory doesn't allow file creation.
        format!("/tmp/ccc_asm_{}.{}.s", pid, unique_id)
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
    // Tell linker not to require .note.GNU-stack section (suppresses warnings from
    // hand-written .S files like musl's __set_thread_area.S that lack this section)
    cmd.arg("-Wl,-z,noexecstack");

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
///
/// Besides the generic `emit` and `emit_fmt` methods, this provides specialized
/// fast-path emitters for common patterns that avoid `core::fmt` overhead.
/// The fast integer writer (`write_i64`) uses direct digit extraction instead
/// of going through `Display`/`write_fmt` machinery.
pub struct AsmOutput {
    pub buf: String,
}

/// Write an i64 directly into a String buffer using manual digit extraction.
/// This is ~3-4x faster than `write!(buf, "{}", val)` for the common case
/// because it avoids the `core::fmt` vtable dispatch and `pad_integral` overhead.
#[inline]
fn write_i64_fast(buf: &mut String, val: i64) {
    if val == 0 {
        buf.push('0');
        return;
    }
    let mut tmp = [0u8; 20]; // i64 max is 19 digits + sign
    let negative = val < 0;
    // Work with absolute value using wrapping to handle i64::MIN correctly
    let mut v = if negative { (val as u64).wrapping_neg() } else { val as u64 };
    let mut pos = 20;
    while v > 0 {
        pos -= 1;
        tmp[pos] = b'0' + (v % 10) as u8;
        v /= 10;
    }
    if negative {
        pos -= 1;
        tmp[pos] = b'-';
    }
    // SAFETY: tmp[pos..20] contains only ASCII digits and optionally a '-' prefix
    let s = unsafe { std::str::from_utf8_unchecked(&tmp[pos..20]) };
    buf.push_str(s);
}

/// Write a u64 directly into a String buffer.
#[inline]
fn write_u64_fast(buf: &mut String, val: u64) {
    if val == 0 {
        buf.push('0');
        return;
    }
    let mut tmp = [0u8; 20]; // u64 max is 20 digits
    let mut v = val;
    let mut pos = 20;
    while v > 0 {
        pos -= 1;
        tmp[pos] = b'0' + (v % 10) as u8;
        v /= 10;
    }
    let s = unsafe { std::str::from_utf8_unchecked(&tmp[pos..20]) };
    buf.push_str(s);
}

impl AsmOutput {
    pub fn new() -> Self {
        // Pre-allocate 256KB to avoid repeated reallocations during codegen.
        Self { buf: String::with_capacity(256 * 1024) }
    }

    /// Emit a line of assembly.
    #[inline]
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

    // ── Fast-path emitters ──────────────────────────────────────────────
    //
    // These avoid the overhead of `format_args!` + `core::fmt::write` for
    // the most common codegen patterns. Each one directly pushes bytes into
    // the buffer using `push_str` and our fast integer writer.

    /// Emit: `    {mnemonic} ${imm}, %{reg}`
    /// Used for movq/movl/movabsq with immediate to register.
    #[inline]
    pub fn emit_instr_imm_reg(&mut self, mnemonic: &str, imm: i64, reg: &str) {
        self.buf.push_str(mnemonic);
        self.buf.push_str(" $");
        write_i64_fast(&mut self.buf, imm);
        self.buf.push_str(", %");
        self.buf.push_str(reg);
        self.buf.push('\n');
    }

    /// Emit: `    {mnemonic} %{src}, %{dst}`
    /// Used for movq/movl/xorq register-to-register.
    #[inline]
    pub fn emit_instr_reg_reg(&mut self, mnemonic: &str, src: &str, dst: &str) {
        self.buf.push_str(mnemonic);
        self.buf.push_str(" %");
        self.buf.push_str(src);
        self.buf.push_str(", %");
        self.buf.push_str(dst);
        self.buf.push('\n');
    }

    /// Emit: `    {mnemonic} {offset}(%rbp), %{reg}`
    /// Used for loads from stack slots.
    #[inline]
    pub fn emit_instr_rbp_reg(&mut self, mnemonic: &str, offset: i64, reg: &str) {
        self.buf.push_str(mnemonic);
        self.buf.push(' ');
        write_i64_fast(&mut self.buf, offset);
        self.buf.push_str("(%rbp), %");
        self.buf.push_str(reg);
        self.buf.push('\n');
    }

    /// Emit: `    {mnemonic} %{reg}, {offset}(%rbp)`
    /// Used for stores to stack slots.
    #[inline]
    pub fn emit_instr_reg_rbp(&mut self, mnemonic: &str, reg: &str, offset: i64) {
        self.buf.push_str(mnemonic);
        self.buf.push_str(" %");
        self.buf.push_str(reg);
        self.buf.push_str(", ");
        write_i64_fast(&mut self.buf, offset);
        self.buf.push_str("(%rbp)");
        self.buf.push('\n');
    }

    /// Emit: `    {mnemonic} ${imm}, {offset}(%rbp)`
    /// Used for stores of immediates to stack slots.
    #[inline]
    pub fn emit_instr_imm_rbp(&mut self, mnemonic: &str, imm: i64, offset: i64) {
        self.buf.push_str(mnemonic);
        self.buf.push_str(" $");
        write_i64_fast(&mut self.buf, imm);
        self.buf.push_str(", ");
        write_i64_fast(&mut self.buf, offset);
        self.buf.push_str("(%rbp)");
        self.buf.push('\n');
    }

    /// Emit a block label line: `.L{id}:`
    #[inline]
    pub fn emit_block_label(&mut self, block_id: u32) {
        self.buf.push_str(".L");
        write_u64_fast(&mut self.buf, block_id as u64);
        self.buf.push(':');
        self.buf.push('\n');
    }

    /// Write a block label reference (no colon, no newline) into the buffer: `.L{id}`
    #[inline]
    pub fn write_block_ref(&mut self, block_id: u32) {
        self.buf.push_str(".L");
        write_u64_fast(&mut self.buf, block_id as u64);
    }

    /// Emit: `    jmp .L{block_id}`
    #[inline]
    pub fn emit_jmp_block(&mut self, block_id: u32) {
        self.buf.push_str("    jmp .L");
        write_u64_fast(&mut self.buf, block_id as u64);
        self.buf.push('\n');
    }

    /// Emit: `    {jcc} .L{block_id}` (conditional jump to block label)
    #[inline]
    pub fn emit_jcc_block(&mut self, jcc: &str, block_id: u32) {
        self.buf.push_str(jcc);
        self.buf.push_str(" .L");
        write_u64_fast(&mut self.buf, block_id as u64);
        self.buf.push('\n');
    }

    /// Emit: `    {mnemonic} {reg}`  (single-register instruction like push/pop)
    #[inline]
    pub fn emit_instr_reg(&mut self, mnemonic: &str, reg: &str) {
        self.buf.push_str(mnemonic);
        self.buf.push_str(" %");
        self.buf.push_str(reg);
        self.buf.push('\n');
    }

    /// Emit: `    {mnemonic} ${imm}`  (single-immediate instruction like push)
    #[inline]
    pub fn emit_instr_imm(&mut self, mnemonic: &str, imm: i64) {
        self.buf.push_str(mnemonic);
        self.buf.push_str(" $");
        write_i64_fast(&mut self.buf, imm);
        self.buf.push('\n');
    }

    /// Write an i64 into the buffer without newline. Useful for building
    /// custom format patterns that include integers.
    #[inline]
    pub fn write_i64(&mut self, val: i64) {
        write_i64_fast(&mut self.buf, val);
    }

    /// Write a u64 into the buffer without newline.
    #[inline]
    pub fn write_u64(&mut self, val: u64) {
        write_u64_fast(&mut self.buf, val);
    }

    /// Push a string slice without newline.
    #[inline]
    pub fn write_str(&mut self, s: &str) {
        self.buf.push_str(s);
    }

    /// Push a newline to end the current line.
    #[inline]
    pub fn newline(&mut self) {
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

    /// Returns true if this is the x86-64 target directive.
    /// Used to select x87 80-bit extended precision format for long double constants.
    pub fn is_x86(self) -> bool {
        matches!(self, PtrDirective::Quad)
    }

    /// Convert a byte alignment value to the correct `.align` argument for this target.
    /// On x86-64, `.align N` means N bytes. On ARM and RISC-V, `.align N` means 2^N bytes,
    /// so we must emit log2(N) instead.
    pub fn align_arg(self, bytes: usize) -> usize {
        debug_assert!(bytes == 0 || bytes.is_power_of_two(), "alignment must be power of 2");
        match self {
            PtrDirective::Quad => bytes,
            PtrDirective::Xword | PtrDirective::Dword => {
                if bytes <= 1 { 0 } else { bytes.trailing_zeros() as usize }
            }
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
            out.emit_fmt(format_args!(".align {}", ptr_dir.align_arg(4)));
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

    // Globals with custom section attributes -> emit in their custom section first
    for g in globals {
        if g.is_extern || g.section.is_none() {
            continue;
        }
        let sect = g.section.as_ref().unwrap();
        let is_zero_init = matches!(g.init, GlobalInit::Zero);
        // Determine section flags: "aw" for writable, "a" for read-only
        let flags = if sect.contains("rodata") { "a" } else { "aw" };
        out.emit_fmt(format_args!(".section {},\"{}\",@progbits", sect, flags));
        if is_zero_init || g.size == 0 {
            if !g.is_static {
                out.emit_fmt(format_args!(".globl {}", g.name));
            }
            out.emit_fmt(format_args!(".align {}", effective_align(g)));
            out.emit_fmt(format_args!(".type {}, @object", g.name));
            out.emit_fmt(format_args!(".size {}, {}", g.name, g.size));
            out.emit_fmt(format_args!("{}:", g.name));
            out.emit_fmt(format_args!("    .zero {}", g.size));
        } else {
            emit_global_def(out, g, ptr_dir);
        }
        out.emit("");
    }

    // Initialized globals -> .data (skip those with custom sections)
    for g in globals {
        if g.is_extern {
            continue; // extern declarations have no storage
        }
        if g.section.is_some() {
            continue; // already emitted in custom section
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
        if g.section.is_some() {
            continue; // already emitted in custom section
        }
        if !g.is_common || !matches!(g.init, GlobalInit::Zero) {
            continue;
        }
        // .comm alignment is always in bytes on all platforms, unlike .align
        out.emit_fmt(format_args!(".comm {},{},{}", g.name, g.size, effective_align(g)));
    }

    // Zero-initialized globals -> .bss (skip those with custom sections)
    // Also includes zero-size globals with empty initializers (e.g., `Type arr[0] = {}`)
    // which were skipped from .data to avoid address overlap.
    for g in globals {
        if g.is_extern {
            continue; // extern declarations have no storage
        }
        if g.section.is_some() {
            continue; // already emitted in custom section
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
        out.emit_fmt(format_args!(".align {}", ptr_dir.align_arg(effective_align(g))));
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
    out.emit_fmt(format_args!(".align {}", ptr_dir.align_arg(effective_align(g))));
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
                // Determine the emission type for each array element.
                // For byte-serialized struct data (g.ty == I8), use each constant's
                // own type so that I8/I16/I32/F32/F64 fields are correctly sized.
                // For typed arrays (e.g., pointer arrays where g.ty == Ptr), use
                // the global's declared element type when it's wider than the
                // constant's natural type. This ensures that e.g. IrConst::I32(0)
                // in a pointer array emits .quad 0 (8 bytes) not .long 0 (4 bytes).
                let const_ty = match val {
                    IrConst::I8(_) => IrType::I8,
                    IrConst::I16(_) => IrType::I16,
                    IrConst::I32(_) => IrType::I32,
                    IrConst::F32(_) => IrType::F32,
                    IrConst::F64(_) => IrType::F64,
                    IrConst::LongDouble(_) => IrType::F128,
                    _ => g.ty,
                };
                let elem_ty = if g.ty.size() > const_ty.size() {
                    g.ty
                } else {
                    const_ty
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
        GlobalInit::GlobalLabelDiff(lab1, lab2, byte_size) => {
            emit_label_diff(out, lab1, lab2, *byte_size);
        }
        GlobalInit::Compound(elements) => {
            for elem in elements {
                match elem {
                    GlobalInit::Scalar(c) => {
                        // In Compound initializers, each element may have a different
                        // type (e.g., struct with int and pointer fields). Use the
                        // constant's own type, falling back to g.ty for I64 and wider.
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
                    GlobalInit::GlobalLabelDiff(lab1, lab2, byte_size) => {
                        emit_label_diff(out, lab1, lab2, *byte_size);
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

/// Emit a label difference as a sized assembly directive (`.long lab1-lab2`, etc.).
fn emit_label_diff(out: &mut AsmOutput, lab1: &str, lab2: &str, byte_size: usize) {
    let dir = match byte_size {
        1 => ".byte",
        2 => ".short",
        4 => ".long",
        _ => ".quad",
    };
    out.emit_fmt(format_args!("    {} {}-{}", dir, lab1, lab2));
}

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
            if ptr_dir.is_x86() {
                // x86-64: emit x87 80-bit extended precision format (10 bytes + 6 padding).
                // This ensures correct memory representation when code reads the bytes
                // through unions or integer arrays (e.g., TCC's CValue.tab[]).
                let x87_bytes = crate::ir::ir::f64_to_x87_bytes(*v);
                // Emit as two .quad values (16 bytes total, little-endian)
                let lo = u64::from_le_bytes([
                    x87_bytes[0], x87_bytes[1], x87_bytes[2], x87_bytes[3],
                    x87_bytes[4], x87_bytes[5], x87_bytes[6], x87_bytes[7],
                ]);
                let hi = u64::from_le_bytes([
                    x87_bytes[8], x87_bytes[9], 0, 0, 0, 0, 0, 0,
                ]);
                out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), lo as i64));
                out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), hi as i64));
            } else {
                // ARM64/RISC-V: store as f64 bit pattern in the lower 8 bytes + 8 zero padding.
                // Codegen loads long doubles as f64 values; ABI conversion via __extenddftf2.
                out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), v.to_bits()));
                out.emit_fmt(format_args!("    {} 0", ptr_dir.as_str()));
            }
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
///
/// Writes directly into the output buffer without any intermediate
/// heap allocations (no per-byte String, no Vec, no join). Uses
/// a pre-computed lookup table to convert bytes to decimal strings
/// without fmt::Write overhead.
pub fn emit_string_bytes(out: &mut AsmOutput, s: &str) {
    out.buf.push_str("    .byte ");
    let mut first = true;
    for c in s.chars() {
        if !first {
            out.buf.push_str(", ");
        }
        first = false;
        push_u8_decimal(&mut out.buf, c as u8);
    }
    // Null terminator
    if !first {
        out.buf.push_str(", ");
    }
    out.buf.push_str("0\n");
}

/// Append a u8 value as a decimal string directly into the buffer.
/// Avoids fmt::Write overhead by using direct digit extraction.
#[inline]
fn push_u8_decimal(buf: &mut String, v: u8) {
    if v >= 100 {
        buf.push((b'0' + v / 100) as char);
        buf.push((b'0' + (v / 10) % 10) as char);
        buf.push((b'0' + v % 10) as char);
    } else if v >= 10 {
        buf.push((b'0' + v / 10) as char);
        buf.push((b'0' + v % 10) as char);
    } else {
        buf.push((b'0' + v) as char);
    }
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

