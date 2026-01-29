pub mod common;

// Shared codegen framework, split into focused modules:
pub mod state;       // CodegenState, StackSlot, SlotAddr
pub mod traits;      // ArchCodegen trait with default implementations
pub mod generation;    // Module/function/instruction dispatch
pub mod stack_layout;  // Stack layout: slot assignment, alloca coalescing, regalloc helpers
pub mod call_abi;    // Call argument classification and stack computation
pub mod call_emit;   // Callee-side parameter classification (shared by emit_store_params)
pub mod cast;        // Cast and float operation classification
pub mod f128_softfloat; // Shared F128 soft-float orchestration (ARM + RISC-V)
pub mod inline_asm;  // InlineAsmEmitter trait and shared framework

// Register allocation and liveness analysis
pub mod liveness;     // Live interval computation
pub mod regalloc;     // Linear scan register allocator


pub mod x86;
pub mod i686;
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
    /// Whether to use only general-purpose registers (-mgeneral-regs-only).
    /// On AArch64, this prevents FP/SIMD register usage in variadic function
    /// prologues (no q0-q7 saves) and sets __vr_offs=0 in va_start.
    /// The Linux kernel uses this to avoid touching NEON/FP state.
    /// TODO: Full -mgeneral-regs-only support would also need to avoid NEON/FP in
    /// popcount, byte-swap, float casts, and other FP codegen paths. Currently only
    /// the variadic ABI path is gated, which is sufficient for the Linux kernel
    /// (kernel code doesn't use floats or popcount builtins in hot paths).
    pub general_regs_only: bool,
    /// Whether to use the kernel code model (-mcmodel=kernel). All symbols
    /// are assumed to be in the negative 2GB of the virtual address space.
    /// Uses absolute sign-extended 32-bit addressing (movq $symbol) for
    /// global address references, producing R_X86_64_32S relocations.
    pub code_model_kernel: bool,
    /// Whether to disable jump table emission for switch statements (-fno-jump-tables).
    /// When true, all switch statements use compare-and-branch chains instead of
    /// indirect jumps through a jump table. Required by the Linux kernel when building
    /// with retpoline (-mindirect-branch=thunk-extern) to avoid indirect jumps that
    /// objtool would reject.
    pub no_jump_tables: bool,
    /// Whether to suppress linker relaxation (-mno-relax, RISC-V only).
    /// When true, the codegen emits `.option norelax` at the top of the
    /// assembly output, which prevents the GNU assembler from generating
    /// R_RISCV_RELAX relocation entries. This is required for the Linux
    /// kernel's EFI stub, which uses -fpic -mno-relax to ensure no
    /// absolute symbol references are introduced by linker relaxation.
    pub no_relax: bool,
    /// Whether to emit debug info (.file/.loc directives) when compiling with -g.
    /// When true, the codegen emits DWARF line number directives based on
    /// source_spans attached to each IR instruction during lowering.
    pub debug_info: bool,
}

/// Target architecture.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Target {
    X86_64,
    I686,
    Aarch64,
    Riscv64,
}

impl Target {
    /// Return the GCC-style target triple for this architecture.
    /// Used by configure scripts (via -dumpmachine) to detect the target.
    pub fn triple(&self) -> &'static str {
        match self {
            Target::X86_64 => "x86_64-linux-gnu",
            Target::I686 => "i686-linux-gnu",
            Target::Aarch64 => "aarch64-linux-gnu",
            Target::Riscv64 => "riscv64-linux-gnu",
        }
    }

    /// Whether this target uses 32-bit pointers (ILP32 data model).
    pub fn is_32bit(&self) -> bool {
        matches!(self, Target::I686)
    }

    /// Pointer size in bytes for this target.
    pub fn ptr_size(&self) -> usize {
        if self.is_32bit() { 4 } else { 8 }
    }

    /// Get the assembler config for this target.
    pub fn assembler_config(&self) -> common::AssemblerConfig {
        match self {
            Target::X86_64 => common::AssemblerConfig {
                command: "gcc",
                extra_args: &[],
            },
            Target::I686 => common::AssemblerConfig {
                command: "i686-linux-gnu-gcc",
                extra_args: &["-m32"],
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
            Target::I686 => common::LinkerConfig {
                command: "i686-linux-gnu-gcc",
                extra_args: &["-m32", "-no-pie"],
            },
            Target::Aarch64 => common::LinkerConfig {
                command: "aarch64-linux-gnu-gcc",
                // Use -no-pie to match non-PIC code generation.  The previous
                // default of -static prevented dlopen() of shared libraries
                // at runtime, breaking postgres extension loading.  The unit
                // test harness passes -static explicitly for QEMU user-mode.
                extra_args: &["-no-pie"],
            },
            Target::Riscv64 => common::LinkerConfig {
                command: "riscv64-linux-gnu-gcc",
                extra_args: &["-no-pie"],
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
        self.generate_assembly_with_opts_and_debug(module, opts, None)
    }

    /// Generate assembly with full codegen options and optional source manager for debug info.
    /// When `source_mgr` is provided and `opts.debug_info` is true, the codegen emits
    /// .file/.loc directives for DWARF line number information.
    pub fn generate_assembly_with_opts_and_debug(
        &self,
        module: &IrModule,
        opts: &CodegenOptions,
        source_mgr: Option<&crate::common::source::SourceManager>,
    ) -> String {
        match self {
            Target::X86_64 => {
                let mut cg = x86::X86Codegen::new();
                cg.apply_options(opts);
                let raw = generation::generate_module_with_debug(&mut cg, module, opts.debug_info, source_mgr);
                x86::codegen::peephole::peephole_optimize(raw)
            }
            Target::I686 => {
                let mut cg = i686::I686Codegen::new();
                cg.apply_options(opts);
                generation::generate_module_with_debug(&mut cg, module, opts.debug_info, source_mgr)
            }
            Target::Aarch64 => {
                let mut cg = arm::ArmCodegen::new();
                cg.apply_options(opts);
                generation::generate_module_with_debug(&mut cg, module, opts.debug_info, source_mgr)
            }
            Target::Riscv64 => {
                let mut cg = riscv::RiscvCodegen::new();
                cg.apply_options(opts);
                cg.emit_pre_directives();
                generation::generate_module_with_debug(&mut cg, module, opts.debug_info, source_mgr)
            }
        }
    }

    /// Assemble text to object file.
    pub fn assemble(&self, asm_text: &str, output_path: &str) -> Result<(), String> {
        common::assemble(&self.assembler_config(), asm_text, output_path)
    }

    /// Assemble text to object file with dynamic extra arguments.
    /// Used to pass through -mabi= and -march= flags from the CLI.
    pub fn assemble_with_extra(&self, asm_text: &str, output_path: &str, extra_args: &[String]) -> Result<(), String> {
        common::assemble_with_extra(&self.assembler_config(), asm_text, output_path, extra_args)
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
