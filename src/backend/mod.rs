pub mod common;

// Shared codegen framework, split into focused modules:
pub mod state;       // CodegenState, StackSlot, SlotAddr
pub mod traits;      // ArchCodegen trait with default implementations
pub mod generation;  // Module/function/instruction dispatch
pub mod call_abi;    // Call argument classification and stack computation
pub mod call_emit;   // Callee-side parameter classification (shared by emit_store_params)
pub mod cast;        // Cast and float operation classification
pub mod inline_asm;  // InlineAsmEmitter trait and shared framework

// Re-export shim for backwards compatibility with existing imports
pub mod codegen_shared;

pub mod x86;
pub mod arm;
pub mod riscv;

use crate::ir::ir::IrModule;

/// Target architecture.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Target {
    X86_64,
    Aarch64,
    Riscv64,
}

impl Target {
    /// Return the GCC-style target triple for this architecture.
    /// Used by configure scripts (via -dumpmachine) to detect the target.
    pub fn triple(&self) -> &'static str {
        match self {
            Target::X86_64 => "x86_64-linux-gnu",
            Target::Aarch64 => "aarch64-linux-gnu",
            Target::Riscv64 => "riscv64-linux-gnu",
        }
    }

    /// Get the assembler config for this target.
    pub fn assembler_config(&self) -> common::AssemblerConfig {
        match self {
            Target::X86_64 => common::AssemblerConfig {
                command: "gcc",
                extra_args: &[],
            },
            Target::Aarch64 => common::AssemblerConfig {
                command: "aarch64-linux-gnu-gcc",
                extra_args: &["-march=armv8-a+crc"],
            },
            Target::Riscv64 => common::AssemblerConfig {
                command: "riscv64-linux-gnu-gcc",
                extra_args: &["-march=rv64gc", "-mabi=lp64d"],
            },
        }
    }

    /// Get the linker config for this target.
    pub fn linker_config(&self) -> common::LinkerConfig {
        match self {
            Target::X86_64 => common::LinkerConfig {
                command: "gcc",
                extra_args: &["-no-pie"],
            },
            Target::Aarch64 => common::LinkerConfig {
                command: "aarch64-linux-gnu-gcc",
                extra_args: &["-static"],
            },
            Target::Riscv64 => common::LinkerConfig {
                command: "riscv64-linux-gnu-gcc",
                extra_args: &["-static"],
            },
        }
    }

    /// Generate assembly for an IR module using this target's code generator.
    pub fn generate_assembly(&self, module: &IrModule) -> String {
        self.generate_assembly_with_options(module, false)
    }

    /// Generate assembly with PIC (position-independent code) option.
    pub fn generate_assembly_with_options(&self, module: &IrModule, pic: bool) -> String {
        match self {
            Target::X86_64 => {
                let mut cg = x86::X86Codegen::new();
                cg.set_pic(pic);
                cg.generate(module)
            }
            Target::Aarch64 => arm::ArmCodegen::new().generate(module),
            Target::Riscv64 => riscv::RiscvCodegen::new().generate(module),
        }
    }

    /// Assemble text to object file.
    pub fn assemble(&self, asm_text: &str, output_path: &str) -> Result<(), String> {
        common::assemble(&self.assembler_config(), asm_text, output_path)
    }

    /// Link object files into executable.
    pub fn link(&self, object_files: &[&str], output_path: &str) -> Result<(), String> {
        common::link(&self.linker_config(), object_files, output_path)
    }

    /// Link object files with additional user-provided linker args.
    pub fn link_with_args(&self, object_files: &[&str], output_path: &str, user_args: &[String]) -> Result<(), String> {
        common::link_with_args(&self.linker_config(), object_files, output_path, user_args)
    }
}
