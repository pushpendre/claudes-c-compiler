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
    assemble_with_extra(config, asm_text, output_path, &[])
}

/// Assemble text to an object file, with additional dynamic arguments.
///
/// The `extra_dynamic_args` are appended after the config's static extra_args,
/// allowing runtime overrides (e.g., -mabi=lp64 from CLI flags).
pub fn assemble_with_extra(config: &AssemblerConfig, asm_text: &str, output_path: &str, extra_dynamic_args: &[String]) -> Result<(), String> {
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
    cmd.args(extra_dynamic_args);
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
    // Skip flags that conflict with -shared when building shared libraries:
    // -no-pie/-pie conflict with -shared, and -static causes the linker to
    // use static CRT objects (e.g. crtbeginT.o) whose absolute relocations
    // (R_RISCV_HI20, R_AARCH64_ADR_PREL_PG_HI21) are incompatible with PIC.
    for arg in config.extra_args {
        if is_shared && (*arg == "-no-pie" || *arg == "-pie" || *arg == "-static") {
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

    /// Emit: `    {mnemonic} {offset}(%rbp)` (single rbp-offset operand, e.g. fldt/fstpt)
    #[inline]
    pub fn emit_instr_rbp(&mut self, mnemonic: &str, offset: i64) {
        self.buf.push_str(mnemonic);
        self.buf.push(' ');
        write_i64_fast(&mut self.buf, offset);
        self.buf.push_str("(%rbp)");
        self.buf.push('\n');
    }

    /// Emit a named label definition: `{label}:`
    #[inline]
    pub fn emit_named_label(&mut self, label: &str) {
        self.buf.push_str(label);
        self.buf.push(':');
        self.buf.push('\n');
    }

    /// Emit: `    jmp {label}` (jump to named label)
    #[inline]
    pub fn emit_jmp_label(&mut self, label: &str) {
        self.buf.push_str("    jmp ");
        self.buf.push_str(label);
        self.buf.push('\n');
    }

    /// Emit: `    {jcc} {label}` (conditional jump to named label)
    #[inline]
    pub fn emit_jcc_label(&mut self, jcc: &str, label: &str) {
        self.buf.push_str(jcc);
        self.buf.push(' ');
        self.buf.push_str(label);
        self.buf.push('\n');
    }

    /// Emit: `    call {target}` (direct call to named function/label)
    #[inline]
    pub fn emit_call(&mut self, target: &str) {
        self.buf.push_str("    call ");
        self.buf.push_str(target);
        self.buf.push('\n');
    }

    /// Emit: `    {mnemonic} {offset}(%{base}), %{reg}` (memory to register with arbitrary base)
    #[inline]
    pub fn emit_instr_mem_reg(&mut self, mnemonic: &str, offset: i64, base: &str, reg: &str) {
        self.buf.push_str(mnemonic);
        self.buf.push(' ');
        if offset != 0 {
            write_i64_fast(&mut self.buf, offset);
        }
        self.buf.push_str("(%");
        self.buf.push_str(base);
        self.buf.push_str("), %");
        self.buf.push_str(reg);
        self.buf.push('\n');
    }

    /// Emit: `    {mnemonic} %{reg}, {offset}(%{base})` (register to memory with arbitrary base)
    #[inline]
    pub fn emit_instr_reg_mem(&mut self, mnemonic: &str, reg: &str, offset: i64, base: &str) {
        self.buf.push_str(mnemonic);
        self.buf.push_str(" %");
        self.buf.push_str(reg);
        self.buf.push_str(", ");
        if offset != 0 {
            write_i64_fast(&mut self.buf, offset);
        }
        self.buf.push_str("(%");
        self.buf.push_str(base);
        self.buf.push(')');
        self.buf.push('\n');
    }

    /// Emit: `    {mnemonic} ${imm}, {offset}(%{base})` (immediate to memory with arbitrary base)
    #[inline]
    pub fn emit_instr_imm_mem(&mut self, mnemonic: &str, imm: i64, offset: i64, base: &str) {
        self.buf.push_str(mnemonic);
        self.buf.push_str(" $");
        write_i64_fast(&mut self.buf, imm);
        self.buf.push_str(", ");
        if offset != 0 {
            write_i64_fast(&mut self.buf, offset);
        }
        self.buf.push_str("(%");
        self.buf.push_str(base);
        self.buf.push(')');
        self.buf.push('\n');
    }

    /// Emit: `    {mnemonic} {symbol}(%{base}), %{reg}` (symbol-relative addressing)
    /// Used for RIP-relative loads like `leaq table_label(%rip), %rcx`.
    #[inline]
    pub fn emit_instr_sym_base_reg(&mut self, mnemonic: &str, symbol: &str, base: &str, reg: &str) {
        self.buf.push_str(mnemonic);
        self.buf.push(' ');
        self.buf.push_str(symbol);
        self.buf.push_str("(%");
        self.buf.push_str(base);
        self.buf.push_str("), %");
        self.buf.push_str(reg);
        self.buf.push('\n');
    }

    /// Emit: `    {mnemonic} ${symbol}, %{reg}` (symbol as immediate)
    /// Used for absolute symbol addressing like `movq $name, %rax`.
    #[inline]
    pub fn emit_instr_sym_imm_reg(&mut self, mnemonic: &str, symbol: &str, reg: &str) {
        self.buf.push_str(mnemonic);
        self.buf.push_str(" $");
        self.buf.push_str(symbol);
        self.buf.push_str(", %");
        self.buf.push_str(reg);
        self.buf.push('\n');
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
    Long,   // i686 (32-bit)
    Xword,  // AArch64
    Dword,  // RISC-V 64
}

impl PtrDirective {
    pub fn as_str(self) -> &'static str {
        match self {
            PtrDirective::Quad => ".quad",
            PtrDirective::Long => ".long",
            PtrDirective::Xword => ".xword",
            PtrDirective::Dword => ".dword",
        }
    }

    /// Returns true if this is an x86 target directive (x86-64 or i686).
    /// Used to select x87 80-bit extended precision format for long double constants.
    pub fn is_x86(self) -> bool {
        matches!(self, PtrDirective::Quad | PtrDirective::Long)
    }

    /// Returns true if this is a 32-bit pointer directive.
    pub fn is_32bit(self) -> bool {
        matches!(self, PtrDirective::Long)
    }

    /// Returns true if this is the RISC-V target directive.
    /// RISC-V stores full IEEE binary128 long doubles in memory (allocas and globals).
    pub fn is_riscv(self) -> bool {
        matches!(self, PtrDirective::Dword)
    }

    /// Returns true if this is the AArch64 target directive.
    /// AArch64 stores full IEEE binary128 long doubles in memory (allocas and globals).
    pub fn is_arm(self) -> bool {
        matches!(self, PtrDirective::Xword)
    }

    /// Convert a byte alignment value to the correct `.align` argument for this target.
    /// On x86-64, `.align N` means N bytes. On ARM and RISC-V, `.align N` means 2^N bytes,
    /// so we must emit log2(N) instead.
    pub fn align_arg(self, bytes: usize) -> usize {
        debug_assert!(bytes == 0 || bytes.is_power_of_two(), "alignment must be power of 2");
        match self {
            PtrDirective::Quad | PtrDirective::Long => bytes,
            PtrDirective::Xword | PtrDirective::Dword => {
                if bytes <= 1 { 0 } else { bytes.trailing_zeros() as usize }
            }
        }
    }
}

/// Emit all data sections (rodata for string literals, .data and .bss for globals).
pub fn emit_data_sections(out: &mut AsmOutput, module: &IrModule, ptr_dir: PtrDirective) {
    // String literals in .rodata
    if !module.string_literals.is_empty() || !module.wide_string_literals.is_empty()
       || !module.char16_string_literals.is_empty() {
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
        // char16_t string literals (u"..."): each char is a 2-byte char16_t value
        for (label, chars) in &module.char16_string_literals {
            out.emit_fmt(format_args!(".align {}", ptr_dir.align_arg(2)));
            out.emit_fmt(format_args!("{}:", label));
            for &ch in chars {
                out.emit_fmt(format_args!("  .short {}", ch));
            }
        }
        out.emit("");
    }

    // Global variables
    emit_globals(out, &module.globals, ptr_dir);
}

/// Compute effective alignment for a global, promoting to 16 when size >= 16.
/// This matches GCC/Clang behavior on x86-64 and aarch64, enabling aligned SSE/NEON access.
/// Globals placed in custom sections are excluded from promotion because they may
/// form contiguous arrays (e.g. the kernel's __param or .init.setup sections) where
/// the linker expects elements at their natural stride with no extra padding.
/// Additionally, when the user explicitly specified an alignment via __attribute__((aligned(N)))
/// or _Alignas, we respect their choice and don't auto-promote. GCC behaves the same way:
/// explicit aligned(8) on a 24-byte struct gives 8-byte alignment, not 16.
fn effective_align(g: &IrGlobal) -> usize {
    if g.section.is_some() || g.has_explicit_align {
        return g.align;
    }
    if g.size >= 16 && g.align < 16 {
        16
    } else {
        g.align
    }
}

/// Emit a zero-initialized global variable (used in .bss, .tbss, and custom section zero-init).
fn emit_zero_global(out: &mut AsmOutput, g: &IrGlobal, obj_type: &str, ptr_dir: PtrDirective) {
    emit_symbol_directives(out, g);
    out.emit_fmt(format_args!(".align {}", ptr_dir.align_arg(effective_align(g))));
    out.emit_fmt(format_args!(".type {}, {}", g.name, obj_type));
    out.emit_fmt(format_args!(".size {}, {}", g.name, g.size));
    out.emit_fmt(format_args!("{}:", g.name));
    out.emit_fmt(format_args!("    .zero {}", g.size));
}

/// Target section classification for a global variable.
///
/// Each global is classified exactly once into one of these categories,
/// which determines which assembly section it belongs to.
#[derive(PartialEq, Eq)]
enum GlobalSection {
    /// Extern (undefined) symbol -- only needs visibility directive, no storage.
    Extern,
    /// Has `__attribute__((section(...)))` -- emitted in its custom section.
    Custom,
    /// Const-qualified, non-TLS, initialized, non-zero-size -> `.rodata`.
    Rodata,
    /// Thread-local, initialized, non-zero-size -> `.tdata`.
    Tdata,
    /// Non-const, non-TLS, initialized, non-zero-size -> `.data`.
    Data,
    /// Zero-initialized, `is_common` flag set -> `.comm` directive.
    Common,
    /// Thread-local, zero-initialized (or zero-size) -> `.tbss`.
    Tbss,
    /// Non-TLS, zero-initialized (or zero-size with init) -> `.bss`.
    Bss,
}

/// Classify a global variable into the section it should be emitted to.
///
/// The classification priority matches GCC behavior:
/// 1. Extern symbols get no storage (just visibility directives).
/// 2. Custom section overrides all other placement.
/// 3. TLS globals go to .tdata (initialized) or .tbss (zero-init).
/// 4. Const globals go to .rodata.
/// 5. Non-zero initialized non-const globals go to .data.
/// 6. Zero-initialized common globals go to .comm.
/// 7. Zero-initialized non-common globals go to .bss.
fn classify_global(g: &IrGlobal) -> GlobalSection {
    if g.is_extern {
        return GlobalSection::Extern;
    }
    if g.section.is_some() {
        return GlobalSection::Custom;
    }
    let is_zero = matches!(g.init, GlobalInit::Zero);
    let has_nonzero_init = !is_zero && g.size > 0;
    if g.is_thread_local {
        return if has_nonzero_init { GlobalSection::Tdata } else { GlobalSection::Tbss };
    }
    if has_nonzero_init {
        return if g.is_const { GlobalSection::Rodata } else { GlobalSection::Data };
    }
    // Zero-initialized (or zero-size with init)
    if g.is_common && is_zero {
        return GlobalSection::Common;
    }
    GlobalSection::Bss
}

/// Emit global variable definitions, grouped by target section.
///
/// Classifies each global once via `classify_global`, then emits all globals
/// for each section in a fixed order: extern visibility, custom sections,
/// .rodata, .tdata, .data, .comm, .tbss, .bss.
fn emit_globals(out: &mut AsmOutput, globals: &[IrGlobal], ptr_dir: PtrDirective) {
    // Phase 1: classify every global into its target section.
    let classified: Vec<GlobalSection> = globals.iter().map(classify_global).collect();

    // Phase 2: emit each section group in order.

    // Extern visibility directives (needed for PIC code so the assembler/linker knows
    // these symbols are resolved within the link unit).
    for (g, sect) in globals.iter().zip(&classified) {
        if matches!(sect, GlobalSection::Extern) {
            emit_visibility_directive(out, &g.name, &g.visibility);
        }
    }

    // Custom section globals: each gets its own .section directive since they
    // may target different sections.
    for (g, sect) in globals.iter().zip(&classified) {
        if !matches!(sect, GlobalSection::Custom) {
            continue;
        }
        let section_name = g.section.as_ref().unwrap();
        // Use "a" (read-only) for const-qualified globals or rodata sections,
        // "aw" (writable) otherwise. GCC uses the const qualification of the
        // variable to determine section flags, not just the section name.
        // This matters for kernel sections like .modinfo which contain const data.
        let flags = if g.is_const || section_name.contains("rodata") { "a" } else { "aw" };
        // Sections starting with ".bss" are NOBITS (no file space, BSS semantics)
        let section_type = if section_name.starts_with(".bss") { "@nobits" } else { "@progbits" };
        out.emit_fmt(format_args!(".section {},\"{}\",{}", section_name, flags, section_type));
        if matches!(g.init, GlobalInit::Zero) || g.size == 0 {
            emit_zero_global(out, g, "@object", ptr_dir);
        } else {
            emit_global_def(out, g, ptr_dir);
        }
        out.emit("");
    }

    // .rodata: const-qualified initialized globals (matches GCC -fno-PIE behavior;
    // the linker handles relocations in .rodata fine, and kernel linker scripts
    // don't recognize .data.rel.ro).
    emit_section_group(out, globals, &classified, &GlobalSection::Rodata,
        ".section .rodata", false, ptr_dir);

    // .tdata: thread-local initialized globals
    emit_section_group(out, globals, &classified, &GlobalSection::Tdata,
        ".section .tdata,\"awT\",@progbits", false, ptr_dir);

    // .data: non-const initialized globals
    emit_section_group(out, globals, &classified, &GlobalSection::Data,
        ".section .data", false, ptr_dir);

    // .comm: zero-initialized common globals (weak linkage, linker merges duplicates).
    // .comm alignment is always in bytes on all platforms, unlike .align.
    for (g, sect) in globals.iter().zip(&classified) {
        if matches!(sect, GlobalSection::Common) {
            out.emit_fmt(format_args!(".comm {},{},{}", g.name, g.size, effective_align(g)));
        }
    }

    // .tbss: thread-local zero-initialized globals
    emit_section_group(out, globals, &classified, &GlobalSection::Tbss,
        ".section .tbss,\"awT\",@nobits", true, ptr_dir);

    // .bss: non-TLS zero-initialized globals (includes zero-size globals with
    // empty initializers like `Type arr[0] = {}` to avoid address overlap).
    emit_section_group(out, globals, &classified, &GlobalSection::Bss,
        ".section .bss", true, ptr_dir);
}

/// Emit all globals matching `target` section, with a section header on first match.
/// If `is_zero` is true, emits as zero-initialized; otherwise as initialized data.
fn emit_section_group(
    out: &mut AsmOutput,
    globals: &[IrGlobal],
    classified: &[GlobalSection],
    target: &GlobalSection,
    section_header: &str,
    is_zero: bool,
    ptr_dir: PtrDirective,
) {
    let mut emitted_header = false;
    for (g, sect) in globals.iter().zip(classified) {
        if sect != target {
            continue;
        }
        if !emitted_header {
            out.emit(section_header);
            emitted_header = true;
        }
        if is_zero {
            let obj_type = if g.is_thread_local { "@tls_object" } else { "@object" };
            emit_zero_global(out, g, obj_type, ptr_dir);
        } else {
            emit_global_def(out, g, ptr_dir);
        }
    }
    if emitted_header {
        out.emit("");
    }
}

/// Emit a visibility directive (.hidden, .protected, .internal) for a symbol if applicable.
fn emit_visibility_directive(out: &mut AsmOutput, name: &str, visibility: &Option<String>) {
    if let Some(ref vis) = visibility {
        match vis.as_str() {
            "hidden" => out.emit_fmt(format_args!(".hidden {}", name)),
            "protected" => out.emit_fmt(format_args!(".protected {}", name)),
            "internal" => out.emit_fmt(format_args!(".internal {}", name)),
            _ => {} // "default" or unknown: no directive needed
        }
    }
}

/// Emit linkage directives (.globl or .weak) for a non-static symbol.
fn emit_linkage_directive(out: &mut AsmOutput, name: &str, is_static: bool, is_weak: bool) {
    if !is_static {
        if is_weak {
            out.emit_fmt(format_args!(".weak {}", name));
        } else {
            out.emit_fmt(format_args!(".globl {}", name));
        }
    }
}

/// Emit both linkage (.globl/.weak) and visibility (.hidden/.protected/.internal) directives.
fn emit_symbol_directives(out: &mut AsmOutput, g: &IrGlobal) {
    emit_linkage_directive(out, &g.name, g.is_static, g.is_weak);
    emit_visibility_directive(out, &g.name, &g.visibility);
}

/// Emit a single global variable definition.
fn emit_global_def(out: &mut AsmOutput, g: &IrGlobal, ptr_dir: PtrDirective) {
    emit_symbol_directives(out, g);
    out.emit_fmt(format_args!(".align {}", ptr_dir.align_arg(effective_align(g))));
    let obj_type = if g.is_thread_local { "@tls_object" } else { "@object" };
    out.emit_fmt(format_args!(".type {}, {}", g.name, obj_type));
    out.emit_fmt(format_args!(".size {}, {}", g.name, g.size));
    out.emit_fmt(format_args!("{}:", g.name));

    emit_init_data(out, &g.init, g.ty, g.size, ptr_dir);
}

/// Emit the data for a single GlobalInit element.
///
/// Handles all init variants: scalars, arrays, strings, global addresses, label diffs,
/// and compound initializers (which recurse into this function for each element).
/// `fallback_ty` is the declared element type of the enclosing global/array, used to
/// widen narrow constants (e.g., IrConst::I32(0) in a pointer array emits .quad 0).
/// `total_size` is the declared size of the enclosing global for padding calculations.
fn emit_init_data(out: &mut AsmOutput, init: &GlobalInit, fallback_ty: IrType, total_size: usize, ptr_dir: PtrDirective) {
    match init {
        GlobalInit::Zero => {
            out.emit_fmt(format_args!("    .zero {}", total_size));
        }
        GlobalInit::Scalar(c) => {
            emit_const_data(out, c, fallback_ty, ptr_dir);
        }
        GlobalInit::Array(values) => {
            for val in values {
                // Determine the emission type for each array element.
                // For byte-serialized struct data (fallback_ty == I8), use each constant's
                // own type so that I8/I16/I32/F32/F64 fields are correctly sized.
                // For typed arrays (e.g., pointer arrays where fallback_ty == Ptr), use
                // the global's declared element type when it's wider than the
                // constant's natural type. This ensures that e.g. IrConst::I32(0)
                // in a pointer array emits .quad 0 (8 bytes) not .long 0 (4 bytes).
                let const_ty = const_natural_type(val, fallback_ty);
                let elem_ty = if fallback_ty.size() > const_ty.size() {
                    fallback_ty
                } else {
                    const_ty
                };
                emit_const_data(out, val, elem_ty, ptr_dir);
            }
        }
        GlobalInit::String(s) => {
            out.emit_fmt(format_args!("    .asciz \"{}\"", escape_string(s)));
            let string_bytes = s.len() + 1; // string + null terminator
            if total_size > string_bytes {
                out.emit_fmt(format_args!("    .zero {}", total_size - string_bytes));
            }
        }
        GlobalInit::WideString(chars) => {
            emit_wide_string(out, chars);
            let wide_bytes = (chars.len() + 1) * 4;
            if total_size > wide_bytes {
                out.emit_fmt(format_args!("    .zero {}", total_size - wide_bytes));
            }
        }
        GlobalInit::Char16String(chars) => {
            emit_char16_string(out, chars);
            let char16_bytes = (chars.len() + 1) * 2;
            if total_size > char16_bytes {
                out.emit_fmt(format_args!("    .zero {}", total_size - char16_bytes));
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
                // Compound elements are self-typed: each element knows its own size.
                // For Scalar elements, use the constant's natural type (falling back
                // to the enclosing global's type for I64/wider constants).
                emit_compound_element(out, elem, fallback_ty, ptr_dir);
            }
        }
    }
}

/// Emit a single element within a Compound initializer.
///
/// Most variants delegate to the shared emit_init_data. Scalar elements use the
/// constant's natural type rather than the enclosing global's type, since compound
/// elements may have heterogeneous types (e.g., struct with int and pointer fields).
fn emit_compound_element(out: &mut AsmOutput, elem: &GlobalInit, fallback_ty: IrType, ptr_dir: PtrDirective) {
    match elem {
        GlobalInit::Scalar(c) => {
            // In compound initializers, each element may have a different type.
            // Use the constant's own type, falling back to fallback_ty for I64 and wider.
            let elem_ty = const_natural_type(c, fallback_ty);
            emit_const_data(out, c, elem_ty, ptr_dir);
        }
        GlobalInit::Zero => {
            // Zero element in compound: emit a single pointer-sized zero
            out.emit_fmt(format_args!("    {} 0", ptr_dir.as_str()));
        }
        GlobalInit::Compound(_) => {
            // Nested compound: emit pointer-sized zero as fallback
            out.emit_fmt(format_args!("    {} 0", ptr_dir.as_str()));
        }
        // All other variants (GlobalAddr, GlobalAddrOffset, WideString, etc.)
        // delegate to the shared handler with zero total_size (no padding).
        other => emit_init_data(out, other, fallback_ty, 0, ptr_dir),
    }
}

/// Get the natural IR type of a constant, falling back to `default_ty` for
/// types that don't have a narrower representation (I64, I128, etc.).
fn const_natural_type(c: &IrConst, default_ty: IrType) -> IrType {
    match c {
        IrConst::I8(_) => IrType::I8,
        IrConst::I16(_) => IrType::I16,
        IrConst::I32(_) => IrType::I32,
        IrConst::F32(_) => IrType::F32,
        IrConst::F64(_) => IrType::F64,
        IrConst::LongDouble(..) => IrType::F128,
        _ => default_ty,
    }
}

/// Emit a wide string (wchar_t) as .long directives with null terminator.
fn emit_wide_string(out: &mut AsmOutput, chars: &[u32]) {
    for &ch in chars {
        out.emit_fmt(format_args!("    .long {}", ch));
    }
    out.emit("    .long 0"); // null terminator
}

/// Emit a char16_t string as .short directives with null terminator.
fn emit_char16_string(out: &mut AsmOutput, chars: &[u16]) {
    for &ch in chars {
        out.emit_fmt(format_args!("    .short {}", ch));
    }
    out.emit("    .short 0"); // null terminator
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
    match c {
        // Integer constants: all share the same widening/narrowing logic.
        // The value is sign-extended to i64, then emitted at the target type's width.
        IrConst::I8(v) => emit_int_data(out, *v as i64, ty, ptr_dir),
        IrConst::I16(v) => emit_int_data(out, *v as i64, ty, ptr_dir),
        IrConst::I32(v) => emit_int_data(out, *v as i64, ty, ptr_dir),
        IrConst::I64(v) => emit_int_data(out, *v, ty, ptr_dir),
        IrConst::F32(v) => {
            out.emit_fmt(format_args!("    .long {}", v.to_bits()));
        }
        IrConst::F64(v) => {
            out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), v.to_bits()));
        }
        IrConst::LongDouble(f64_val, bytes) => {
            if ptr_dir.is_x86() {
                // x86-64: emit stored x87 80-bit extended precision bytes (full precision).
                // The raw bytes are in bytes[0..10], with bytes[10..16] = 0 padding.
                let lo = u64::from_le_bytes(bytes[0..8].try_into().unwrap());
                let hi = u64::from_le_bytes([bytes[8], bytes[9], 0, 0, 0, 0, 0, 0]);
                out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), lo as i64));
                out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), hi as i64));
            } else if ptr_dir.is_riscv() || ptr_dir.is_arm() {
                // RISC-V and ARM64: convert x87 80-bit bytes to IEEE 754 binary128 format.
                // Both backends store full 16-byte f128 in allocas/memory,
                // so global data must also be proper binary128.
                let f128_bytes = crate::common::long_double::x87_bytes_to_f128_bytes(bytes);
                let lo = u64::from_le_bytes(f128_bytes[0..8].try_into().unwrap());
                let hi = u64::from_le_bytes(f128_bytes[8..16].try_into().unwrap());
                out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), lo as i64));
                out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), hi as i64));
            } else {
                // Fallback: store f64 approximation (should not normally be reached).
                let f64_bits = f64_val.to_bits();
                out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), f64_bits as i64));
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

/// Emit an integer constant at the width specified by `ty`.
/// Truncates or sign-extends `val` (an i64) as needed to match the target width.
fn emit_int_data(out: &mut AsmOutput, val: i64, ty: IrType, ptr_dir: PtrDirective) {
    match ty {
        IrType::I8 | IrType::U8 => out.emit_fmt(format_args!("    .byte {}", val as u8)),
        IrType::I16 | IrType::U16 => out.emit_fmt(format_args!("    .short {}", val as u16)),
        IrType::I32 | IrType::U32 => out.emit_fmt(format_args!("    .long {}", val as u32)),
        _ => out.emit_fmt(format_args!("    {} {}", ptr_dir.as_str(), val)),
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

