use crate::backend::Target;
use crate::frontend::preprocessor::Preprocessor;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use crate::frontend::sema::SemanticAnalyzer;
use crate::ir::lowering::Lowerer;
use crate::ir::mem2reg::{promote_allocas, eliminate_phis};
use crate::passes::run_passes;
use crate::common::source::SourceManager;

/// Compilation mode - determines where in the pipeline to stop.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompileMode {
    /// Full compilation: preprocess -> compile -> assemble -> link (default)
    Full,
    /// -S: Stop after generating assembly, output .s file
    AssemblyOnly,
    /// -c: Stop after assembling, output .o file
    ObjectOnly,
    /// -E: Stop after preprocessing, output preprocessed source to stdout
    PreprocessOnly,
}

/// A command-line define: -Dname or -Dname=value
#[derive(Debug, Clone)]
pub struct CliDefine {
    pub name: String,
    pub value: String,
}

/// The compiler driver orchestrates all compilation phases.
pub struct Driver {
    pub target: Target,
    pub output_path: String,
    pub output_path_set: bool,
    pub input_files: Vec<String>,
    pub opt_level: u32,
    pub verbose: bool,
    pub mode: CompileMode,
    pub debug_info: bool,
    pub defines: Vec<CliDefine>,
    pub include_paths: Vec<String>,
    /// Libraries to pass to the linker (from -l flags)
    pub linker_libs: Vec<String>,
    /// Library search paths (from -L flags)
    pub linker_paths: Vec<String>,
    /// Extra linker args (e.g., -Wl,... pass-through)
    pub linker_extra_args: Vec<String>,
    /// Whether to link statically (-static)
    pub static_link: bool,
    /// Whether to produce a shared library (-shared)
    pub shared_lib: bool,
    /// Whether to omit standard library linking (-nostdlib)
    pub nostdlib: bool,
    /// Whether to generate position-independent code (-fPIC/-fpic)
    pub pic: bool,
    /// Files to force-include before the main source (-include flag)
    pub force_includes: Vec<String>,
    /// Whether to replace `ret` with `jmp __x86_return_thunk` (-mfunction-return=thunk-extern)
    pub function_return_thunk: bool,
    /// Whether to replace indirect calls/jumps with retpoline thunks (-mindirect-branch=thunk-extern)
    pub indirect_branch_thunk: bool,
    /// Patchable function entry: (total_nops, nops_before_entry).
    /// Set by -fpatchable-function-entry=N[,M] where N is total NOPs and M is
    /// how many go before the entry point (the rest go after).
    /// Used by the Linux kernel for ftrace and static call patching.
    pub patchable_function_entry: Option<(u32, u32)>,
    /// Whether to emit endbr64 at function entry points (-fcf-protection=branch).
    /// Required by the Linux kernel for Intel CET/IBT (Indirect Branch Tracking).
    pub cf_protection_branch: bool,
    /// Whether SSE is disabled (-mno-sse). When true, the compiler must not emit
    /// any SSE/SSE2/AVX instructions (movdqu, movss, movsd, etc.).
    /// The Linux kernel uses -mno-sse to avoid FPU state in kernel code.
    pub no_sse: bool,
    /// Whether to use only general-purpose registers (-mgeneral-regs-only).
    /// On AArch64, this prevents FP/SIMD register usage. The Linux kernel uses
    /// this to avoid touching NEON/FP state. When set, variadic function prologues
    /// must not save q0-q7, and va_start sets __vr_offs=0 (no FP save area).
    pub general_regs_only: bool,
    /// Whether to use the kernel code model (-mcmodel=kernel). All symbols
    /// are assumed to be in the negative 2GB of the virtual address space.
    /// Uses absolute sign-extended 32-bit addressing (movq $symbol) for
    /// global address references, producing R_X86_64_32S relocations.
    pub code_model_kernel: bool,
    /// Whether to disable jump table emission for switch statements (-fno-jump-tables).
    /// The Linux kernel uses this with -mindirect-branch=thunk-extern (retpoline) to
    /// prevent indirect jumps that objtool would reject.
    pub no_jump_tables: bool,
    /// RISC-V ABI override from -mabi= flag (e.g., "lp64", "lp64d", "lp64f").
    /// When set, overrides the default "lp64d" passed to the assembler.
    /// The Linux kernel uses -mabi=lp64 (soft-float) for kernel code.
    pub riscv_abi: Option<String>,
    /// RISC-V architecture override from -march= flag (e.g., "rv64imac_zicsr_zifencei").
    /// When set, overrides the default "rv64gc" passed to the assembler.
    pub riscv_march: Option<String>,
    /// RISC-V -mno-relax flag: suppress linker relaxation.
    /// When true, the codegen emits `.option norelax` and the assembler is
    /// invoked with `-mno-relax`. This prevents R_RISCV_RELAX relocations
    /// that would allow the linker to introduce absolute symbol references.
    /// Required by the Linux kernel's EFI stub (built with -fpic -mno-relax).
    pub riscv_no_relax: bool,
    /// Explicit language override from -x flag.
    /// When set, overrides file extension detection for input language.
    /// Values: "c", "assembler", "assembler-with-cpp", "none" (reset).
    /// Used for stdin input ("-") and also for files like /dev/null where
    /// the extension doesn't indicate the language.
    pub explicit_language: Option<String>,
    /// Extra arguments to pass to the assembler (from -Wa, flags).
    /// Used by kernel build to query assembler version via -Wa,--version.
    pub assembler_extra_args: Vec<String>,
    /// Path to write dependency file (from -MF or -Wp,-MMD,path or -Wp,-MD,path).
    /// When set, the compiler writes a Make-compatible dependency file listing
    /// the input source as a dependency of the output object. Used by the Linux
    /// kernel build system (fixdep) to track header dependencies.
    pub dep_file: Option<String>,
    /// When true, delegate compilation to the system GCC instead of compiling
    /// ourselves. Set when -m16 or -m32 is passed, since we only support x86-64
    /// code generation. The kernel uses -m16 for boot/realmode code that runs in
    /// 16-bit real mode (with .code16gcc), which requires 32-bit ELF output.
    pub gcc_fallback: bool,
    /// The original command-line arguments (excluding argv[0]), saved so we can
    /// forward them verbatim to GCC when gcc_fallback is true.
    pub original_args: Vec<String>,
    /// Whether to suppress line markers in preprocessor output (-P flag).
    /// When true, `# <line> "<file>"` directives are stripped from -E output.
    /// Used by the Linux kernel's cc-version.sh to detect the compiler.
    pub suppress_line_markers: bool,
}

impl Driver {
    pub fn new() -> Self {
        Self {
            target: Target::X86_64,
            output_path: "a.out".to_string(),
            output_path_set: false,
            input_files: Vec::new(),
            opt_level: 2, // All levels run the same optimizations; default to max
            verbose: false,
            mode: CompileMode::Full,
            debug_info: false,
            defines: Vec::new(),
            include_paths: Vec::new(),
            linker_libs: Vec::new(),
            linker_paths: Vec::new(),
            linker_extra_args: Vec::new(),
            static_link: false,
            shared_lib: false,
            nostdlib: false,
            pic: false,
            force_includes: Vec::new(),
            function_return_thunk: false,
            indirect_branch_thunk: false,
            patchable_function_entry: None,
            cf_protection_branch: false,
            no_sse: false,
            general_regs_only: false,
            code_model_kernel: false,
            no_jump_tables: false,
            riscv_abi: None,
            riscv_march: None,
            riscv_no_relax: false,
            explicit_language: None,
            assembler_extra_args: Vec::new(),
            dep_file: None,
            gcc_fallback: false,
            original_args: Vec::new(),
            suppress_line_markers: false,
        }
    }

    /// Read source from an input file. If the file is "-", reads from stdin.
    fn read_source(input_file: &str) -> Result<String, String> {
        if input_file == "-" {
            use std::io::Read;
            let mut buf = String::new();
            std::io::stdin().read_to_string(&mut buf)
                .map_err(|e| format!("Cannot read from stdin: {}", e))?;
            Ok(buf)
        } else {
            std::fs::read_to_string(input_file)
                .map_err(|e| format!("Cannot read {}: {}", input_file, e))
        }
    }

    /// Check if an input file should be treated as assembly source based on
    /// the -x language override. This is used for stdin ("-") and for files
    /// like /dev/null where the extension doesn't indicate assembly.
    fn is_explicit_assembly(&self) -> bool {
        matches!(self.explicit_language.as_deref(),
            Some("assembler") | Some("assembler-with-cpp"))
    }

    /// Add a -D define from command line.
    pub fn add_define(&mut self, arg: &str) {
        if let Some(eq_pos) = arg.find('=') {
            self.defines.push(CliDefine {
                name: arg[..eq_pos].to_string(),
                value: arg[eq_pos + 1..].to_string(),
            });
        } else {
            self.defines.push(CliDefine {
                name: arg.to_string(),
                value: "1".to_string(),
            });
        }
    }

    /// Add a -I include path from command line.
    pub fn add_include_path(&mut self, path: &str) {
        self.include_paths.push(path.to_string());
    }

    /// Determine the output path for a given input file and mode.
    fn output_for_input(&self, input_file: &str) -> String {
        if self.output_path_set {
            return self.output_path.clone();
        }
        let stem = if input_file == "-" {
            "stdin"
        } else {
            std::path::Path::new(input_file)
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("a")
        };
        match self.mode {
            CompileMode::AssemblyOnly => format!("{}.s", stem),
            CompileMode::ObjectOnly => format!("{}.o", stem),
            CompileMode::PreprocessOnly => String::new(),
            CompileMode::Full => self.output_path.clone(),
        }
    }

    /// Configure the preprocessor with CLI-defined macros and target.
    fn configure_preprocessor(&self, preprocessor: &mut Preprocessor) {
        // Set target architecture macros
        match self.target {
            Target::Aarch64 => preprocessor.set_target("aarch64"),
            Target::Riscv64 => preprocessor.set_target("riscv64"),
            Target::X86_64 => preprocessor.set_target("x86_64"),
        }
        // Apply RISC-V ABI/arch overrides from -mabi= and -march= flags.
        // These must come after set_target() which sets defaults for RV64GC/lp64d.
        if self.target == Target::Riscv64 {
            if let Some(ref abi) = self.riscv_abi {
                preprocessor.set_riscv_abi(abi);
            }
            if let Some(ref march) = self.riscv_march {
                preprocessor.set_riscv_march(march);
            }
        }
        // Set PIC mode: defines __PIC__/__pic__ only when -fPIC is active.
        // This is critical for kernel code where RIP_REL_REF() checks #ifndef __pic__
        // to decide whether to use RIP-relative inline asm for early boot code.
        preprocessor.set_pic(self.pic);
        for def in &self.defines {
            preprocessor.define_macro(&def.name, &def.value);
        }
        for path in &self.include_paths {
            preprocessor.add_include_path(path);
        }
    }

    /// Run the compiler pipeline.
    pub fn run(&self) -> Result<(), String> {
        if self.input_files.is_empty() {
            return Err("No input files".to_string());
        }

        // When -m16 or -m32 is passed, we don't support 32-bit/16-bit x86 codegen.
        // Delegate the entire compilation to the system GCC, forwarding all original args.
        // This is needed for the Linux kernel's boot/realmode code which uses -m16.
        if self.gcc_fallback {
            return self.run_gcc_fallback();
        }

        match self.mode {
            CompileMode::PreprocessOnly => self.run_preprocess_only(),
            CompileMode::AssemblyOnly => self.run_assembly_only(),
            CompileMode::ObjectOnly => self.run_object_only(),
            CompileMode::Full => self.run_full(),
        }
    }

    /// Process force-included files (-include flag) through the preprocessor before
    /// the main source. This matches GCC's behavior where -include files are processed
    /// as if they were #include'd at the top of the source, with paths resolved relative
    /// to the current working directory (unlike regular #include which resolves relative
    /// to the source file's directory).
    fn process_force_includes(&self, preprocessor: &mut Preprocessor) -> Result<(), String> {
        for path in &self.force_includes {
            // Resolve relative to CWD (matching GCC behavior)
            let resolved = std::path::Path::new(path);
            let resolved = if resolved.is_absolute() {
                resolved.to_path_buf()
            } else if let Ok(cwd) = std::env::current_dir() {
                cwd.join(resolved)
            } else {
                resolved.to_path_buf()
            };

            let content = std::fs::read_to_string(&resolved)
                .map_err(|e| format!("{}: {}: No such file or directory", path, e))?;
            preprocessor.preprocess_force_include(&content, &resolved.to_string_lossy());
        }
        Ok(())
    }

    fn run_preprocess_only(&self) -> Result<(), String> {
        for input_file in &self.input_files {
            if Self::is_assembly_source(input_file) || self.is_explicit_assembly() {
                // For .S files, delegate preprocessing to gcc which understands
                // assembly-specific preprocessor behavior
                let config = self.target.assembler_config();
                let mut cmd = std::process::Command::new(config.command);
                cmd.args(config.extra_args);
                for path in &self.include_paths {
                    cmd.arg("-I").arg(path);
                }
                for def in &self.defines {
                    if def.value == "1" {
                        cmd.arg(format!("-D{}", def.name));
                    } else {
                        cmd.arg(format!("-D{}={}", def.name, def.value));
                    }
                }
                cmd.arg("-E");
                if self.suppress_line_markers {
                    cmd.arg("-P");
                }
                cmd.arg(input_file);
                if self.output_path_set {
                    cmd.arg("-o").arg(&self.output_path);
                }
                let result = cmd.output()
                    .map_err(|e| format!("Failed to preprocess {}: {}", input_file, e))?;
                if !self.output_path_set {
                    print!("{}", String::from_utf8_lossy(&result.stdout));
                }
                if !result.status.success() {
                    let stderr = String::from_utf8_lossy(&result.stderr);
                    return Err(format!("Preprocessing {} failed: {}", input_file, stderr));
                }
                continue;
            }

            let source = Self::read_source(input_file)?;

            let mut preprocessor = Preprocessor::new();
            self.configure_preprocessor(&mut preprocessor);
            let filename = if input_file == "-" { "<stdin>" } else { input_file };
            preprocessor.set_filename(filename);
            self.process_force_includes(&mut preprocessor)?;
            let preprocessed = preprocessor.preprocess(&source);

            let output = if self.suppress_line_markers {
                Self::strip_line_markers(&preprocessed)
            } else {
                preprocessed
            };

            if self.output_path_set {
                std::fs::write(&self.output_path, &output)
                    .map_err(|e| format!("Cannot write {}: {}", self.output_path, e))?;
            } else {
                print!("{}", output);
            }
        }
        Ok(())
    }

    fn run_assembly_only(&self) -> Result<(), String> {
        for input_file in &self.input_files {
            let asm = self.compile_to_assembly(input_file)?;
            let out_path = self.output_for_input(input_file);
            std::fs::write(&out_path, &asm)
                .map_err(|e| format!("Cannot write {}: {}", out_path, e))?;
            self.write_dep_file(input_file, &out_path);
            if self.verbose {
                eprintln!("Assembly output: {}", out_path);
            }
        }
        Ok(())
    }

    fn run_object_only(&self) -> Result<(), String> {
        for input_file in &self.input_files {
            let out_path = self.output_for_input(input_file);
            if Self::is_assembly_source(input_file) || self.is_explicit_assembly() {
                // .s/.S files (or -x assembler): pass directly to the assembler (gcc)
                self.assemble_source_file(input_file, &out_path)?;
            } else {
                let asm = self.compile_to_assembly(input_file)?;
                let extra = self.build_asm_extra_args();
                self.target.assemble_with_extra(&asm, &out_path, &extra)?;
            }
            self.write_dep_file(input_file, &out_path);
            if self.verbose {
                eprintln!("Object output: {}", out_path);
            }
        }
        Ok(())
    }

    /// Get a short stem name for an input file (for temp file naming).
    fn input_stem(input_file: &str) -> &str {
        if input_file == "-" {
            "stdin"
        } else {
            std::path::Path::new(input_file)
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("out")
        }
    }

    fn run_full(&self) -> Result<(), String> {
        let mut compiled_object_files = Vec::new();
        let mut passthrough_objects: Vec<String> = Vec::new();

        for input_file in &self.input_files {
            if Self::is_object_or_archive(input_file) {
                // Pass .o and .a files directly to the linker
                passthrough_objects.push(input_file.clone());
            } else if Self::is_assembly_source(input_file) || self.is_explicit_assembly() {
                // .s/.S files (or -x assembler): pass to assembler, then link
                let obj_path = format!("/tmp/ccc_{}_{}.o",
                    std::process::id(), Self::input_stem(input_file));
                self.assemble_source_file(input_file, &obj_path)?;
                compiled_object_files.push(obj_path);
            } else {
                // Compile .c files to .o
                let asm = self.compile_to_assembly(input_file)?;

                let obj_path = format!("/tmp/ccc_{}_{}.o",
                    std::process::id(), Self::input_stem(input_file));
                let extra = self.build_asm_extra_args();
                self.target.assemble_with_extra(&asm, &obj_path, &extra)?;
                compiled_object_files.push(obj_path);
            }
        }

        // Combine all object files for linking
        let mut all_objects: Vec<&str> = compiled_object_files.iter().map(|s| s.as_str()).collect();
        for obj in &passthrough_objects {
            all_objects.push(obj.as_str());
        }

        // Build linker args from -l, -L, -static flags
        let linker_args = self.build_linker_args();

        if linker_args.is_empty() {
            self.target.link(&all_objects, &self.output_path)?;
        } else {
            self.target.link_with_args(&all_objects, &self.output_path, &linker_args)?;
        }

        for obj in &compiled_object_files {
            let _ = std::fs::remove_file(obj);
        }

        if self.verbose {
            eprintln!("Output: {}", self.output_path);
        }

        Ok(())
    }

    /// Check if a file is an object file or archive (pass to linker directly).
    fn is_object_or_archive(path: &str) -> bool {
        path.ends_with(".o") || path.ends_with(".a") || path.ends_with(".so")
    }

    /// Check if a file is an assembly source (.s or .S).
    /// .S files contain assembly with C preprocessor directives.
    /// .s files contain pure assembly.
    /// Both are passed to the target assembler (gcc) directly.
    fn is_assembly_source(path: &str) -> bool {
        path.ends_with(".s") || path.ends_with(".S")
    }

    /// Strip GCC-style line markers (`# <num> "file"`) from preprocessed output.
    /// Used when -P flag is set. Filters out lines matching `# <digit>...` while
    /// preserving all other lines (including blank lines) verbatim.
    fn strip_line_markers(input: &str) -> String {
        fn is_line_marker(line: &str) -> bool {
            let trimmed = line.trim_start();
            if !trimmed.starts_with('#') {
                return false;
            }
            trimmed[1..].trim_start().starts_with(|c: char| c.is_ascii_digit())
        }

        let mut result: String = input.lines()
            .filter(|line| !is_line_marker(line))
            .collect::<Vec<_>>()
            .join("\n");
        if input.ends_with('\n') {
            result.push('\n');
        }
        result
    }

    /// Assemble a .s or .S file to an object file using the target assembler.
    /// For .S files, gcc handles preprocessing (macros, #include, etc.).
    fn assemble_source_file(&self, input_file: &str, output_path: &str) -> Result<(), String> {
        let config = self.target.assembler_config();
        let mut cmd = std::process::Command::new(config.command);
        cmd.args(config.extra_args);

        // Pass through RISC-V ABI/arch overrides (must come after config defaults
        // so GCC uses last-wins semantics for -mabi= and -march=).
        let extra_asm_args = self.build_asm_extra_args();
        cmd.args(&extra_asm_args);

        // Pass through include paths and defines for .S preprocessing
        for path in &self.include_paths {
            cmd.arg("-I").arg(path);
        }
        for def in &self.defines {
            if def.value == "1" {
                cmd.arg(format!("-D{}", def.name));
            } else {
                cmd.arg(format!("-D{}={}", def.name, def.value));
            }
        }

        // Pass through -Wa, assembler flags
        for flag in &self.assembler_extra_args {
            cmd.arg(format!("-Wa,{}", flag));
        }

        // When explicit language is set (from -x flag), tell gcc too
        if let Some(ref lang) = self.explicit_language {
            cmd.arg("-x").arg(lang);
        }

        cmd.args(["-c", "-o", output_path, input_file]);

        // If assembler extra args are present (e.g., -Wa,--version), inherit
        // stdout so the assembler's output flows through to the caller.
        // This is needed by kernel's as-version.sh which captures the output.
        if !self.assembler_extra_args.is_empty() {
            cmd.stdout(std::process::Stdio::inherit());
        }

        let result = cmd.output()
            .map_err(|e| format!("Failed to run assembler for {}: {}", input_file, e))?;

        if !result.status.success() {
            let stderr = String::from_utf8_lossy(&result.stderr);
            return Err(format!("Assembly of {} failed: {}", input_file, stderr));
        }

        Ok(())
    }

    /// Build extra assembler arguments for RISC-V ABI/arch overrides.
    ///
    /// When -mabi= or -march= are specified on the CLI, these override the
    /// defaults hardcoded in the assembler config. This is critical for the
    /// Linux kernel which uses -mabi=lp64 (soft-float) instead of the default
    /// lp64d (double-float), and -march=rv64imac... instead of rv64gc.
    /// The assembler uses these flags to set ELF e_flags (float ABI, RVC, etc.).
    fn build_asm_extra_args(&self) -> Vec<String> {
        let mut args = Vec::new();
        // Only pass RISC-V flags to the RISC-V assembler. Passing -mabi/-march
        // to x86/ARM gcc would cause warnings or errors.
        if self.target == Target::Riscv64 {
            if let Some(ref abi) = self.riscv_abi {
                args.push(format!("-mabi={}", abi));
            }
            if let Some(ref march) = self.riscv_march {
                args.push(format!("-march={}", march));
            }
            if self.riscv_no_relax {
                args.push("-mno-relax".to_string());
            }
            // The RISC-V GNU assembler defaults to PIC mode, which causes
            // `la` pseudo-instructions to expand with R_RISCV_GOT_HI20 (GOT
            // indirection) instead of R_RISCV_PCREL_HI20 (direct PC-relative).
            // The Linux kernel does not have a GOT and expects PCREL relocations,
            // so we must explicitly pass -fno-pic when PIC is not requested.
            if !self.pic {
                args.push("-fno-pic".to_string());
            }
        }
        args
    }

    /// Build linker args from collected -l, -L, -static, -shared, and -nostdlib flags.
    fn build_linker_args(&self) -> Vec<String> {
        let mut args = Vec::new();
        if self.shared_lib {
            args.push("-shared".to_string());
        }
        if self.static_link {
            args.push("-static".to_string());
        }
        if self.nostdlib {
            args.push("-nostdlib".to_string());
        }
        for path in &self.linker_paths {
            args.push(format!("-L{}", path));
        }
        for lib in &self.linker_libs {
            args.push(format!("-l{}", lib));
        }
        args.extend(self.linker_extra_args.clone());
        args
    }

    /// Write a Make-compatible dependency file for the given input/output.
    /// Format: "output: input\n"
    /// This is a minimal dependency file that tells make the object depends
    /// on its source file. A full implementation would also list included headers.
    fn write_dep_file(&self, input_file: &str, output_file: &str) {
        if let Some(ref dep_path) = self.dep_file {
            let dep_path = if dep_path.is_empty() {
                // Derive from output: replace extension with .d
                let p = std::path::Path::new(output_file);
                p.with_extension("d").to_string_lossy().into_owned()
            } else {
                dep_path.clone()
            };
            let input_name = if input_file == "-" { "<stdin>" } else { input_file };
            let content = format!("{}: {}\n", output_file, input_name);
            let _ = std::fs::write(&dep_path, content);
        }
    }

    /// Delegate compilation to the system GCC when we can't handle the target mode
    /// (e.g., -m16 or -m32 for 32-bit/16-bit x86 code).
    /// Forwards all original command-line arguments to GCC verbatim.
    fn run_gcc_fallback(&self) -> Result<(), String> {
        let mut cmd = std::process::Command::new("gcc");
        cmd.args(&self.original_args);

        let result = cmd.output()
            .map_err(|e| format!("Failed to run gcc fallback: {}", e))?;

        // Forward stderr
        let stderr = String::from_utf8_lossy(&result.stderr);
        if !stderr.is_empty() {
            eprint!("{}", stderr);
        }
        // Forward stdout
        let stdout = String::from_utf8_lossy(&result.stdout);
        if !stdout.is_empty() {
            print!("{}", stdout);
        }

        if !result.status.success() {
            return Err(format!("gcc fallback failed (exit code: {:?})", result.status.code()));
        }

        Ok(())
    }

    /// Core pipeline: preprocess, lex, parse, sema, lower, optimize, codegen.
    ///
    /// Set `CCC_TIME_PHASES=1` in the environment to print per-phase timing to stderr.
    fn compile_to_assembly(&self, input_file: &str) -> Result<String, String> {
        let source = Self::read_source(input_file)?;

        let time_phases = std::env::var("CCC_TIME_PHASES").is_ok();
        let t0 = std::time::Instant::now();

        // Preprocess
        let mut preprocessor = Preprocessor::new();
        self.configure_preprocessor(&mut preprocessor);
        let filename = if input_file == "-" { "<stdin>" } else { input_file };
        preprocessor.set_filename(filename);
        self.process_force_includes(&mut preprocessor)?;
        let preprocessed = preprocessor.preprocess(&source);
        if time_phases { eprintln!("[TIME] preprocess: {:.3}s", t0.elapsed().as_secs_f64()); }

        // Check for #error directives
        let pp_errors = preprocessor.errors();
        if !pp_errors.is_empty() {
            for err in pp_errors {
                eprintln!("error: {}", err);
            }
            return Err(format!("{} preprocessor error(s) in {}", pp_errors.len(), input_file));
        }

        // Lex
        let t1 = std::time::Instant::now();
        let mut source_manager = SourceManager::new();
        let file_id = source_manager.add_file(input_file.to_string(), preprocessed.clone());
        // Build line map from preprocessor line markers for source location tracking
        source_manager.build_line_map(&preprocessed);
        let mut lexer = Lexer::new(&preprocessed, file_id);
        let tokens = lexer.tokenize();
        if time_phases { eprintln!("[TIME] lex: {:.3}s ({} tokens)", t1.elapsed().as_secs_f64(), tokens.len()); }

        if self.verbose {
            eprintln!("Lexed {} tokens from {}", tokens.len(), input_file);
        }

        // Parse
        let t2 = std::time::Instant::now();
        let mut parser = Parser::new(tokens);
        parser.set_source_manager(source_manager);
        let ast = parser.parse();
        if time_phases { eprintln!("[TIME] parse: {:.3}s", t2.elapsed().as_secs_f64()); }

        if parser.error_count > 0 {
            return Err(format!("{}: {} parse error(s)", input_file, parser.error_count));
        }

        // Retrieve source manager for debug info emission (-g)
        let source_manager = parser.take_source_manager();

        if self.verbose {
            eprintln!("Parsed {} declarations", ast.decls.len());
        }

        // Semantic analysis
        let t3 = std::time::Instant::now();
        let mut sema = SemanticAnalyzer::new();
        if let Err(errors) = sema.analyze(&ast) {
            for err in &errors {
                eprintln!("error: {}", err);
            }
            return Err(format!("{} error(s) during semantic analysis", errors.len()));
        }
        let sema_result = sema.into_result();
        if time_phases { eprintln!("[TIME] sema: {:.3}s", t3.elapsed().as_secs_f64()); }

        // Lower to IR (target-aware for ABI-specific lowering decisions)
        // Pass sema's TypeContext, function signatures, and expression type annotations
        // to the lowerer so it has pre-populated type info upfront.
        let t4 = std::time::Instant::now();
        let lowerer = Lowerer::with_type_context(
            self.target,
            sema_result.type_context,
            sema_result.functions,
            sema_result.expr_types,
            sema_result.const_values,
        );
        let mut module = lowerer.lower(&ast);

        // Apply #pragma weak directives from the preprocessor.
        for (symbol, target) in &preprocessor.weak_pragmas {
            if let Some(ref alias_target) = target {
                // #pragma weak symbol = alias -> create weak alias
                module.aliases.push((symbol.clone(), alias_target.clone(), true));
            } else {
                // #pragma weak symbol -> mark as weak
                module.symbol_attrs.push((symbol.clone(), true, None));
            }
        }

        // Apply #pragma redefine_extname directives from the preprocessor.
        // TODO: This uses .set aliases which works when both symbols are defined
        // locally, but a proper implementation would rename symbol references
        // during lowering/codegen for the case where new_name is external.
        for (old_name, new_name) in &preprocessor.redefine_extname_pragmas {
            module.aliases.push((old_name.clone(), new_name.clone(), false));
        }

        if time_phases { eprintln!("[TIME] lowering: {:.3}s ({} functions)", t4.elapsed().as_secs_f64(), module.functions.len()); }

        if self.verbose {
            eprintln!("Lowered to {} IR functions", module.functions.len());
        }

        // Run optimization passes
        let t5 = std::time::Instant::now();
        promote_allocas(&mut module);
        if time_phases { eprintln!("[TIME] mem2reg: {:.3}s", t5.elapsed().as_secs_f64()); }

        let t6 = std::time::Instant::now();
        run_passes(&mut module, self.opt_level);
        if time_phases { eprintln!("[TIME] opt passes: {:.3}s", t6.elapsed().as_secs_f64()); }

        // Lower SSA phi nodes to copies before codegen
        let t7 = std::time::Instant::now();
        eliminate_phis(&mut module);
        if time_phases { eprintln!("[TIME] phi elimination: {:.3}s", t7.elapsed().as_secs_f64()); }

        // Note: we intentionally do NOT run copy_prop after phi elimination.
        // The IR is no longer in SSA form at this point - Copy instructions from
        // phi elimination represent moves at specific program points. Propagating
        // through them can change semantics (reading a value before it's defined
        // in the current iteration of a loop). Stack size reduction is handled
        // by copy coalescing in codegen instead.

        // Generate assembly using target-specific codegen
        let t8 = std::time::Instant::now();
        let opts = crate::backend::CodegenOptions {
            pic: self.pic || self.shared_lib,
            function_return_thunk: self.function_return_thunk,
            indirect_branch_thunk: self.indirect_branch_thunk,
            patchable_function_entry: self.patchable_function_entry,
            cf_protection_branch: self.cf_protection_branch,
            no_sse: self.no_sse,
            general_regs_only: self.general_regs_only,
            code_model_kernel: self.code_model_kernel,
            no_jump_tables: self.no_jump_tables,
            no_relax: self.riscv_no_relax,
            debug_info: self.debug_info,
        };
        let asm = self.target.generate_assembly_with_opts_and_debug(
            &module, &opts, source_manager.as_ref(),
        );
        if time_phases { eprintln!("[TIME] codegen: {:.3}s ({} bytes asm)", t8.elapsed().as_secs_f64(), asm.len()); }

        if time_phases { eprintln!("[TIME] total compile {}: {:.3}s", input_file, t0.elapsed().as_secs_f64()); }

        if self.verbose {
            eprintln!("Generated {:?} assembly ({} bytes)", self.target, asm.len());
        }

        Ok(asm)
    }
}

impl Default for Driver {
    fn default() -> Self {
        Self::new()
    }
}
