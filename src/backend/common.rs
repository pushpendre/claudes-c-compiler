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
pub fn assemble(config: &AssemblerConfig, asm_text: &str, output_path: &str) -> Result<(), String> {
    let asm_path = format!("{}.s", output_path);
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

/// Link object files into an executable, with additional user-provided linker args.
pub fn link_with_args(config: &LinkerConfig, object_files: &[&str], output_path: &str, user_args: &[String]) -> Result<(), String> {
    let mut cmd = Command::new(config.command);
    cmd.args(config.extra_args);
    cmd.arg("-o").arg(output_path);

    for obj in object_files {
        cmd.arg(obj);
    }

    // Add user-provided linker args (-l, -L, -static, etc.)
    for arg in user_args {
        cmd.arg(arg);
    }

    // Default libs (only add if not already in user_args)
    if !user_args.iter().any(|a| a == "-lc") {
        cmd.arg("-lc");
    }
    if !user_args.iter().any(|a| a == "-lm") {
        cmd.arg("-lm");
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
        Self { buf: String::new() }
    }

    /// Emit a line of assembly.
    pub fn emit(&mut self, s: &str) {
        self.buf.push_str(s);
        self.buf.push('\n');
    }
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
    if !module.string_literals.is_empty() {
        out.emit(".section .rodata");
        for (label, value) in &module.string_literals {
            out.emit(&format!("{}:", label));
            emit_string_bytes(out, value);
        }
        out.emit("");
    }

    // Global variables
    emit_globals(out, &module.globals, ptr_dir);
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
        if !has_data {
            out.emit(".section .data");
            has_data = true;
        }
        emit_global_def(out, g, ptr_dir);
    }
    if has_data {
        out.emit("");
    }

    // Zero-initialized globals -> .bss
    for g in globals {
        if g.is_extern {
            continue; // extern declarations have no storage
        }
        if !matches!(g.init, GlobalInit::Zero) {
            continue;
        }
        if !has_bss {
            out.emit(".section .bss");
            has_bss = true;
        }
        if !g.is_static {
            out.emit(&format!(".globl {}", g.name));
        }
        out.emit(&format!(".align {}", g.align));
        out.emit(&format!(".type {}, @object", g.name));
        out.emit(&format!(".size {}, {}", g.name, g.size));
        out.emit(&format!("{}:", g.name));
        out.emit(&format!("    .zero {}", g.size));
    }
    if has_bss {
        out.emit("");
    }
}

/// Emit a single global variable definition.
fn emit_global_def(out: &mut AsmOutput, g: &IrGlobal, ptr_dir: PtrDirective) {
    if !g.is_static {
        out.emit(&format!(".globl {}", g.name));
    }
    out.emit(&format!(".align {}", g.align));
    out.emit(&format!(".type {}, @object", g.name));
    out.emit(&format!(".size {}, {}", g.name, g.size));
    out.emit(&format!("{}:", g.name));

    match &g.init {
        GlobalInit::Zero => {
            out.emit(&format!("    .zero {}", g.size));
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
                    _ => g.ty,
                };
                emit_const_data(out, val, elem_ty, ptr_dir);
            }
        }
        GlobalInit::String(s) => {
            out.emit(&format!("    .asciz \"{}\"", escape_string(s)));
        }
        GlobalInit::GlobalAddr(label) => {
            out.emit(&format!("    {} {}", ptr_dir.as_str(), label));
        }
        GlobalInit::GlobalAddrOffset(label, offset) => {
            if *offset >= 0 {
                out.emit(&format!("    {} {}+{}", ptr_dir.as_str(), label, offset));
            } else {
                out.emit(&format!("    {} {}{}", ptr_dir.as_str(), label, offset));
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
                        out.emit(&format!("    {} {}", ptr_dir.as_str(), label));
                    }
                    GlobalInit::GlobalAddrOffset(label, offset) => {
                        if *offset >= 0 {
                            out.emit(&format!("    {} {}+{}", ptr_dir.as_str(), label, offset));
                        } else {
                            out.emit(&format!("    {} {}{}", ptr_dir.as_str(), label, offset));
                        }
                    }
                    GlobalInit::Zero => {
                        // Emit a zero-initialized element of the appropriate size
                        out.emit(&format!("    {} 0", ptr_dir.as_str()));
                    }
                    _ => {
                        // Nested compound or other - emit zero as fallback
                        out.emit(&format!("    {} 0", ptr_dir.as_str()));
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
                IrType::I8 | IrType::U8 => out.emit(&format!("    .byte {}", *v as u8)),
                IrType::I16 | IrType::U16 => out.emit(&format!("    .short {}", *v as i16 as u16)),
                IrType::I32 | IrType::U32 => out.emit(&format!("    .long {}", *v as i32 as u32)),
                _ => out.emit(&format!("    {} {}", ptr_dir.as_str(), *v as i64)),
            }
        }
        IrConst::I16(v) => {
            match ty {
                IrType::I8 | IrType::U8 => out.emit(&format!("    .byte {}", *v as u8)),
                IrType::I16 | IrType::U16 => out.emit(&format!("    .short {}", *v as u16)),
                IrType::I32 | IrType::U32 => out.emit(&format!("    .long {}", *v as i32 as u32)),
                _ => out.emit(&format!("    {} {}", ptr_dir.as_str(), *v as i64)),
            }
        }
        IrConst::I32(v) => {
            match ty {
                IrType::I8 | IrType::U8 => out.emit(&format!("    .byte {}", *v as u8)),
                IrType::I16 | IrType::U16 => out.emit(&format!("    .short {}", *v as u16)),
                IrType::I32 | IrType::U32 => out.emit(&format!("    .long {}", *v as u32)),
                _ => out.emit(&format!("    {} {}", ptr_dir.as_str(), *v as i64)),
            }
        }
        IrConst::I64(v) => {
            match ty {
                IrType::I8 | IrType::U8 => out.emit(&format!("    .byte {}", *v as u8)),
                IrType::I16 | IrType::U16 => out.emit(&format!("    .short {}", *v as u16)),
                IrType::I32 | IrType::U32 => out.emit(&format!("    .long {}", *v as u32)),
                _ => out.emit(&format!("    {} {}", ptr_dir.as_str(), v)),
            }
        }
        IrConst::F32(v) => {
            out.emit(&format!("    .long {}", v.to_bits()));
        }
        IrConst::F64(v) => {
            out.emit(&format!("    {} {}", ptr_dir.as_str(), v.to_bits()));
        }
        IrConst::Zero => {
            let size = ty.size();
            out.emit(&format!("    .zero {}", if size == 0 { 4 } else { size }));
        }
    }
}

/// Emit string literal as .byte directives with null terminator.
pub fn emit_string_bytes(out: &mut AsmOutput, s: &str) {
    let bytes: Vec<String> = s.bytes()
        .chain(std::iter::once(0u8))
        .map(|b| format!("{}", b))
        .collect();
    out.emit(&format!("    .byte {}", bytes.join(", ")));
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
                for b in c.to_string().bytes() {
                    result.push_str(&format!("\\{:03o}", b));
                }
            }
        }
    }
    result
}

/// Extract destination Value from an IR instruction (shared across all backends).
pub fn instruction_dest(inst: &Instruction) -> Option<Value> {
    match inst {
        Instruction::Alloca { dest, .. }
        | Instruction::Load { dest, .. }
        | Instruction::BinOp { dest, .. }
        | Instruction::UnaryOp { dest, .. }
        | Instruction::Cmp { dest, .. }
        | Instruction::GetElementPtr { dest, .. }
        | Instruction::Cast { dest, .. }
        | Instruction::Copy { dest, .. }
        | Instruction::GlobalAddr { dest, .. } => Some(*dest),
        Instruction::Call { dest, .. }
        | Instruction::CallIndirect { dest, .. } => *dest,
        Instruction::Store { .. } => None,
        Instruction::Memcpy { .. } => None,
    }
}
