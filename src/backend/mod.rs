pub mod common;

// Shared codegen framework, split into focused modules:
pub mod state;       // CodegenState, StackSlot, SlotAddr
pub mod traits;      // ArchCodegen trait with default implementations
pub mod generation;  // Module/function/instruction dispatch
pub mod call_abi;    // Call argument classification and stack computation
pub mod call_emit;   // Callee-side parameter classification (shared by emit_store_params)
pub mod cast;        // Cast and float operation classification
pub mod inline_asm;  // InlineAsmEmitter trait and shared framework

// Register allocation and liveness analysis
pub mod liveness;     // Live interval computation
pub mod regalloc;     // Linear scan register allocator


pub mod x86;
pub mod arm;
pub mod riscv;

use crate::ir::ir::IrModule;

/// Options that control code generation, parsed from CLI flags.
#[derive(Debug, Clone, Default)]
pub struct CodegenOptions {
    /// Whether to generate position-independent code (-fPIC/-fpic)
    pub pic: bool,
    /// Whether to replace `ret` with `jmp __x86_return_thunk` (-mfunction-return=thunk-extern)
    pub function_return_thunk: bool,
    /// Whether to replace indirect calls/jumps with retpoline thunks (-mindirect-branch=thunk-extern)
    pub indirect_branch_thunk: bool,
    /// Patchable function entry: (total_nops, nops_before_entry).
    /// -fpatchable-function-entry=N[,M] emits NOP padding around function entry points
    /// and records them in __patchable_function_entries for runtime patching (ftrace).
    pub patchable_function_entry: Option<(u32, u32)>,
    /// Whether to emit endbr64 at function entry points (-fcf-protection=branch).
    /// Required for Intel CET/IBT (Indirect Branch Tracking).
    pub cf_protection_branch: bool,
    /// Whether SSE is disabled (-mno-sse). When true, the x86 codegen avoids
    /// SSE/XMM instructions in variadic prologues (XMM register saving) and
    /// va_start sets fp_offset to overflow so va_arg never uses XMM regs.
    /// TODO: Full -mno-sse support would also need to avoid SSE in float
    /// operations, casts, and other FP codegen paths. Currently only the
    /// variadic ABI path is gated, which is sufficient for the Linux kernel.
    pub no_sse: bool,
    /// Whether to use the kernel code model (-mcmodel=kernel). When true,
    /// global symbol addresses are loaded using absolute 32-bit sign-extended
    /// addressing (movq $symbol, %reg) instead of RIP-relative (leaq symbol(%rip), %reg).
    /// This is required for the Linux kernel because early boot code (e.g. __startup_64)
    /// runs at physical addresses different from its linked virtual addresses, making
    /// RIP-relative references resolve incorrectly. The kernel memory model assumes all
    /// kernel symbols reside in the negative 2GB of the virtual address space.
    pub code_model_kernel: bool,
}

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
        self.generate_assembly_with_opts(module, &CodegenOptions::default())
    }

    /// Generate assembly with PIC (position-independent code) option.
    pub fn generate_assembly_with_options(&self, module: &IrModule, pic: bool) -> String {
        let opts = CodegenOptions { pic, ..Default::default() };
        self.generate_assembly_with_opts(module, &opts)
    }

    /// Generate assembly with full codegen options.
    pub fn generate_assembly_with_opts(&self, module: &IrModule, opts: &CodegenOptions) -> String {
        match self {
            Target::X86_64 => {
                let mut cg = x86::X86Codegen::new();
                cg.set_pic(opts.pic);
                cg.set_function_return_thunk(opts.function_return_thunk);
                cg.set_indirect_branch_thunk(opts.indirect_branch_thunk);
                cg.set_patchable_function_entry(opts.patchable_function_entry);
                cg.set_cf_protection_branch(opts.cf_protection_branch);
                cg.set_no_sse(opts.no_sse);
                cg.set_code_model_kernel(opts.code_model_kernel);
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
