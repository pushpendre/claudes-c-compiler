use crate::backend::Target;
use crate::common::error::{ColorMode, DiagnosticEngine, WarningConfig};
use crate::common::source::SourceManager;
use crate::frontend::preprocessor::Preprocessor;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
use crate::frontend::sema::SemanticAnalyzer;
use crate::ir::lowering::Lowerer;
use crate::ir::mem2reg::{promote_allocas, eliminate_phis};
use crate::passes::run_passes;

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
pub(crate) struct CliDefine {
    pub(crate) name: String,
    pub(crate) value: String,
}

/// The compiler driver orchestrates all compilation phases.
///
/// All fields are private; configuration is done through `parse_cli_args()`.
/// Use `has_input_files()` to check if input was provided before calling `run()`.
pub struct Driver {
    target: Target,
    output_path: String,
    output_path_set: bool,
    input_files: Vec<String>,
    opt_level: u32,
    verbose: bool,
    mode: CompileMode,
    debug_info: bool,
    defines: Vec<CliDefine>,
    include_paths: Vec<String>,
    /// Library search paths (from -L flags)
    linker_paths: Vec<String>,
    /// Ordered list of linker items preserving command-line order.
    /// Contains passthrough object/archive paths, -l flags, and -Wl, flags
    /// in the order they appeared on the command line. This is critical for
    /// flags like -Wl,--whole-archive which must precede the .a they affect.
    linker_ordered_items: Vec<String>,
    /// Whether to link statically (-static)
    static_link: bool,
    /// Whether to produce a shared library (-shared)
    shared_lib: bool,
    /// Whether to omit standard library linking (-nostdlib)
    nostdlib: bool,
    /// Whether to generate position-independent code (-fPIC/-fpic)
    pic: bool,
    /// Files to force-include before the main source (-include flag)
    force_includes: Vec<String>,
    /// Whether to replace `ret` with `jmp __x86_return_thunk` (-mfunction-return=thunk-extern)
    function_return_thunk: bool,
    /// Whether to replace indirect calls/jumps with retpoline thunks (-mindirect-branch=thunk-extern)
    indirect_branch_thunk: bool,
    /// Patchable function entry: (total_nops, nops_before_entry).
    /// Set by -fpatchable-function-entry=N[,M] where N is total NOPs and M is
    /// how many go before the entry point (the rest go after).
    /// Used by the Linux kernel for ftrace and static call patching.
    patchable_function_entry: Option<(u32, u32)>,
    /// Whether to emit endbr64 at function entry points (-fcf-protection=branch).
    /// Required by the Linux kernel for Intel CET/IBT (Indirect Branch Tracking).
    cf_protection_branch: bool,
    /// Whether SSE is disabled (-mno-sse). When true, the compiler must not emit
    /// any SSE/SSE2/AVX instructions (movdqu, movss, movsd, etc.).
    /// The Linux kernel uses -mno-sse to avoid FPU state in kernel code.
    no_sse: bool,
    /// Whether to use only general-purpose registers (-mgeneral-regs-only).
    /// On AArch64, this prevents FP/SIMD register usage. The Linux kernel uses
    /// this to avoid touching NEON/FP state. When set, variadic function prologues
    /// must not save q0-q7, and va_start sets __vr_offs=0 (no FP save area).
    general_regs_only: bool,
    /// Whether to use the kernel code model (-mcmodel=kernel). All symbols
    /// are assumed to be in the negative 2GB of the virtual address space.
    /// Uses absolute sign-extended 32-bit addressing (movq $symbol) for
    /// global address references, producing R_X86_64_32S relocations.
    code_model_kernel: bool,
    /// Whether to disable jump table emission for switch statements (-fno-jump-tables).
    /// The Linux kernel uses this with -mindirect-branch=thunk-extern (retpoline) to
    /// prevent indirect jumps that objtool would reject.
    no_jump_tables: bool,
    /// RISC-V ABI override from -mabi= flag (e.g., "lp64", "lp64d", "lp64f").
    /// When set, overrides the default "lp64d" passed to the assembler.
    /// The Linux kernel uses -mabi=lp64 (soft-float) for kernel code.
    riscv_abi: Option<String>,
    /// RISC-V architecture override from -march= flag (e.g., "rv64imac_zicsr_zifencei").
    /// When set, overrides the default "rv64gc" passed to the assembler.
    riscv_march: Option<String>,
    /// RISC-V -mno-relax flag: suppress linker relaxation.
    /// When true, the codegen emits `.option norelax` and the assembler is
    /// invoked with `-mno-relax`. This prevents R_RISCV_RELAX relocations
    /// that would allow the linker to introduce absolute symbol references.
    /// Required by the Linux kernel's EFI stub (built with -fpic -mno-relax).
    riscv_no_relax: bool,
    /// Explicit language override from -x flag.
    /// When set, overrides file extension detection for input language.
    /// Values: "c", "assembler", "assembler-with-cpp", "none" (reset).
    /// Used for stdin input ("-") and also for files like /dev/null where
    /// the extension doesn't indicate the language.
    explicit_language: Option<String>,
    /// Extra arguments to pass to the assembler (from -Wa, flags).
    /// Used by kernel build to query assembler version via -Wa,--version.
    assembler_extra_args: Vec<String>,
    /// Path to write dependency file (from -MF or -Wp,-MMD,path or -Wp,-MD,path).
    /// When set, the compiler writes a Make-compatible dependency file listing
    /// the input source as a dependency of the output object. Used by the Linux
    /// kernel build system (fixdep) to track header dependencies.
    dep_file: Option<String>,
    /// When true, delegate compilation to the system GCC instead of compiling
    /// ourselves. Set when -m16 or -m32 is passed, since we only support x86-64
    /// code generation. The kernel uses -m16 for boot/realmode code that runs in
    /// 16-bit real mode (with .code16gcc), which requires 32-bit ELF output.
    gcc_fallback: bool,
    /// The original command-line arguments (excluding argv[0]), saved so we can
    /// forward them verbatim to GCC when gcc_fallback is true.
    original_args: Vec<String>,
    /// Whether to suppress line markers in preprocessor output (-P flag).
    /// When true, `# <line> "<file>"` directives are stripped from -E output.
    /// Used by the Linux kernel's cc-version.sh to detect the compiler.
    suppress_line_markers: bool,
    /// Whether -nostdinc was passed. When delegating to gcc for assembly
    /// preprocessing, this must be forwarded to prevent system header
    /// interference.
    nostdinc: bool,
    /// Macro undefinitions from -U flags. These need to be forwarded when
    /// delegating preprocessing to gcc (e.g., -Uriscv for kernel linker scripts).
    undef_macros: Vec<String>,
    /// Warning configuration parsed from -W flags. Controls which warnings are
    /// enabled, disabled, or promoted to errors. Processed left-to-right from
    /// the command line to match GCC semantics.
    warning_config: WarningConfig,
    /// Whether GNU C extensions are enabled. Defaults to true.
    /// Set to false when -std=c99, -std=c11, etc. (strict ISO C mode) is used.
    /// When false, bare GNU keywords like `typeof` and `asm` are treated as
    /// identifiers (the __typeof__/__asm__ forms always work).
    gnu_extensions: bool,
    /// Whether to place each function in its own section (-ffunction-sections).
    function_sections: bool,
    /// Whether to place each data object in its own section (-fdata-sections).
    data_sections: bool,
    /// Whether GNU89 inline semantics are in effect (-fgnu89-inline or -std=gnu89).
    /// When true, the preprocessor defines __GNUC_GNU_INLINE__ instead of __GNUC_STDC_INLINE__.
    /// This affects how projects like mpack select their inline linkage model.
    gnu89_inline: bool,
    /// Whether to dump preprocessor defines instead of preprocessed output (-dM).
    /// When true with -E, outputs `#define NAME VALUE` for all predefined and
    /// user-defined macros instead of the preprocessed source. Used by build
    /// systems like Meson to detect compiler version via __GNUC__ etc.
    dump_defines: bool,
    /// Color mode for diagnostic output, controlled by -fdiagnostics-color={auto,always,never}.
    /// Defaults to Auto (colorize when stderr is a terminal).
    color_mode: ColorMode,
    /// Whether to use COMMON linkage for tentative definitions (-fcommon).
    /// When true, uninitialized global variables (tentative definitions) are emitted
    /// as COMMON symbols, allowing multiple TUs to define the same global without
    /// linker "multiple definition" errors. GCC defaulted to -fcommon before GCC 10.
    /// When false (-fno-common, the default), tentative definitions go to BSS with
    /// strong linkage, and duplicate definitions cause link errors.
    fcommon: bool,
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
            linker_paths: Vec::new(),
            linker_ordered_items: Vec::new(),
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
            nostdinc: false,
            undef_macros: Vec::new(),
            warning_config: WarningConfig::new(),
            gnu_extensions: true,
            function_sections: false,
            data_sections: false,
            gnu89_inline: false,
            dump_defines: false,
            color_mode: ColorMode::Auto,
            fcommon: false,
        }
    }

    /// Whether the driver has any input files to process.
    pub fn has_input_files(&self) -> bool {
        !self.input_files.is_empty()
    }

    /// Read a C source file, tolerating non-UTF-8 content.
    /// Valid UTF-8 files are returned as-is. Non-UTF-8 bytes (0x80-0xFF) are
    /// encoded as PUA code points (U+E080-U+E0FF) which the lexer decodes
    /// back to raw bytes inside string/character literals.
    fn read_c_source_file(path: &str) -> Result<String, String> {
        let bytes = std::fs::read(path)
            .map_err(|e| format!("Cannot read {}: {}", path, e))?;
        Ok(crate::common::encoding::bytes_to_string(bytes))
    }

    /// Read source from an input file. If the file is "-", reads from stdin.
    fn read_source(input_file: &str) -> Result<String, String> {
        if input_file == "-" {
            use std::io::Read;
            let mut bytes = Vec::new();
            std::io::stdin().read_to_end(&mut bytes)
                .map_err(|e| format!("Cannot read from stdin: {}", e))?;
            Ok(crate::common::encoding::bytes_to_string(bytes))
        } else {
            Self::read_c_source_file(input_file)
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

    /// Parse GCC-compatible command-line arguments and populate driver fields.
    /// Returns `Ok(true)` if early exit was handled (query flags like -dumpmachine),
    /// `Ok(false)` if normal compilation should proceed, or `Err` for invalid args.
    pub fn parse_cli_args(&mut self, args: &[String]) -> Result<bool, String> {
        // Detect target from binary name (argv[0])
        let binary_name = std::path::Path::new(&args[0])
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("ccc");

        self.target = if binary_name.contains("arm") || binary_name.contains("aarch64") {
            Target::Aarch64
        } else if binary_name.contains("riscv") {
            Target::Riscv64
        } else if binary_name.contains("i686") || binary_name.contains("i386") {
            Target::I686
        } else {
            Target::X86_64
        };

        // Handle GCC query flags that exit immediately (before requiring input files).
        // These are used by configure scripts to detect the compiler and target.
        for arg in &args[1..] {
            match arg.as_str() {
                "-dumpmachine" => {
                    println!("{}", self.target.triple());
                    return Ok(true);
                }
                "-dumpversion" => {
                    println!("14");
                    return Ok(true);
                }
                "--version" => {
                    // Meson detects GCC by checking for "Free Software Foundation"
                    // in the --version output. We claim GCC 14.2.0 compatibility
                    // (matching our __GNUC__/__GNUC_MINOR__/__GNUC_PATCHLEVEL__).
                    println!("ccc (GCC-compatible) 14.2.0");
                    println!("GCC is maintained by the Free Software Foundation, Inc.");
                    println!("This program was written by Claude Opus 4.6;");
                    println!("It is not intended for production use.");
                    return Ok(true);
                }
                "-v" if args.len() == 2 => {
                    println!("ccc (GCC-compatible) 14.2.0");
                    println!("Target: {}", self.target.triple());
                    return Ok(true);
                }
                "-print-search-dirs" => {
                    println!("install: /usr/lib/gcc/{}/13/", self.target.triple());
                    println!("programs: /usr/bin/");
                    println!("libraries: /usr/lib/");
                    return Ok(true);
                }
                _ => {}
            }
        }

        // Save original args (excluding argv[0]) for gcc_fallback mode
        self.original_args = args[1..].to_vec();

        let mut explicit_language: Option<String> = None;
        let mut i = 1;
        while i < args.len() {
            match args[i].as_str() {
                // Output file
                "-o" => {
                    i += 1;
                    if i < args.len() {
                        self.output_path = args[i].clone();
                        self.output_path_set = true;
                    } else {
                        return Err("-o requires an argument".to_string());
                    }
                }

                // Compilation mode flags
                "-S" => self.mode = CompileMode::AssemblyOnly,
                "-c" => self.mode = CompileMode::ObjectOnly,
                "-E" => self.mode = CompileMode::PreprocessOnly,
                "-P" => self.suppress_line_markers = true,
                "-dM" => self.dump_defines = true,

                // Optimization levels
                "-O" | "-O0" | "-O1" | "-O2" | "-O3" | "-Os" | "-Oz" => {
                    self.opt_level = 2;
                }

                // Debug info
                "-g" => self.debug_info = true,
                arg if arg.starts_with("-g") && arg.len() > 2 => self.debug_info = true,

                // Verbose/diagnostic flags
                "-v" | "--verbose" => self.verbose = true,

                // Linker library flags: -lfoo
                arg if arg.starts_with("-l") => {
                    self.linker_ordered_items.push(arg.to_string());
                }

                // Linker pass-through: -Wl,flag1,flag2,...
                arg if arg.starts_with("-Wl,") => {
                    for flag in arg[4..].split(',') {
                        if !flag.is_empty() {
                            self.linker_ordered_items.push(format!("-Wl,{}", flag));
                        }
                    }
                }

                // Assembler pass-through: -Wa,flag1,flag2,...
                arg if arg.starts_with("-Wa,") => {
                    for flag in arg[4..].split(',') {
                        if !flag.is_empty() {
                            self.assembler_extra_args.push(flag.to_string());
                        }
                    }
                }

                // Preprocessor pass-through: -Wp,-MMD,path or -Wp,-MD,path
                arg if arg.starts_with("-Wp,") => {
                    let flags: Vec<&str> = arg[4..].splitn(2, ',').collect();
                    if flags.len() == 2 && (flags[0] == "-MMD" || flags[0] == "-MD") {
                        self.dep_file = Some(flags[1].to_string());
                    }
                }

                // Warning flags
                arg if arg.starts_with("-W") => {
                    let flag = &arg[2..];
                    if !flag.is_empty() {
                        self.warning_config.process_flag(flag);
                    }
                }

                // Preprocessor defines
                "-D" => {
                    i += 1;
                    if i < args.len() {
                        self.add_define(&args[i]);
                    } else {
                        return Err("-D requires an argument".to_string());
                    }
                }
                arg if arg.starts_with("-D") => self.add_define(&arg[2..]),

                // Force-include files
                "-include" => {
                    i += 1;
                    if i < args.len() {
                        self.force_includes.push(args[i].clone());
                    } else {
                        return Err("-include requires an argument".to_string());
                    }
                }

                // Include paths
                "-I" => {
                    i += 1;
                    if i < args.len() {
                        self.add_include_path(&args[i]);
                    } else {
                        return Err("-I requires an argument".to_string());
                    }
                }
                arg if arg.starts_with("-I") => self.add_include_path(&arg[2..]),

                // Library search paths
                "-L" => {
                    i += 1;
                    if i < args.len() {
                        self.linker_paths.push(args[i].clone());
                    }
                }
                arg if arg.starts_with("-L") => {
                    self.linker_paths.push(arg[2..].to_string());
                }

                // Undefine macro
                "-U" => {
                    i += 1;
                    if i < args.len() {
                        self.undef_macros.push(args[i].clone());
                    }
                }
                arg if arg.starts_with("-U") => {
                    self.undef_macros.push(arg[2..].to_string());
                }

                // Standard version flag: -std=c99 disables GNU extensions,
                // -std=gnu99 (or no flag) enables them.
                arg if arg.starts_with("-std=") => {
                    let std_value = &arg[5..];
                    // GNU dialects: gnu89, gnu99, gnu11, gnu17, gnu23, etc.
                    // Strict ISO: c89, c99, c11, c17, c23, iso9899:*, etc.
                    self.gnu_extensions = std_value.starts_with("gnu");
                    // gnu89 and c89 use GNU inline semantics by default;
                    // gnu99+ and c99+ use C99 inline semantics.
                    // Note: -fgnu89-inline can override this later on the command line.
                    self.gnu89_inline = matches!(std_value, "gnu89" | "c89" | "gnu90" | "c90"
                        | "iso9899:1990" | "iso9899:199409");
                }

                // Machine/target flags
                "-mfunction-return=thunk-extern" => self.function_return_thunk = true,
                "-mindirect-branch=thunk-extern" => self.indirect_branch_thunk = true,
                "-m16" => self.gcc_fallback = true, // TODO: replace with I686 once -m32 working
                "-m32" => {
                    // When USE_MY_32=1 environment variable is set, use our i686 backend
                    // instead of falling back to GCC. This allows iterative testing of
                    // 32-bit kernel compilation with our compiler.
                    if std::env::var("USE_MY_32").as_deref() == Ok("1") {
                        self.target = Target::I686;
                    } else {
                        self.gcc_fallback = true;
                    }
                }
                "-mno-sse" | "-mno-sse2" | "-mno-mmx" | "-mno-sse3" | "-mno-ssse3"
                | "-mno-sse4" | "-mno-sse4.1" | "-mno-sse4.2" | "-mno-avx"
                | "-mno-avx2" | "-mno-avx512f" | "-mno-3dnow" => {
                    self.no_sse = true;
                }
                "-mgeneral-regs-only" => self.general_regs_only = true,
                "-mcmodel=kernel" => self.code_model_kernel = true,
                "-mcmodel=small" | "-mcmodel=medlow" | "-mcmodel=medium" | "-mcmodel=medany" | "-mcmodel=large" => {
                    self.code_model_kernel = false;
                }
                arg if arg.starts_with("-mabi=") => {
                    self.riscv_abi = Some(arg["-mabi=".len()..].to_string());
                }
                arg if arg.starts_with("-march=") => {
                    self.riscv_march = Some(arg["-march=".len()..].to_string());
                }
                "-mno-relax" => self.riscv_no_relax = true,
                arg if arg.starts_with("-m") => {}

                // Feature flags
                "-fPIC" | "-fpic" | "-fPIE" | "-fpie" => self.pic = true,
                "-fno-PIC" | "-fno-pic" | "-fno-PIE" | "-fno-pie" => self.pic = false,
                "-fcf-protection=branch" | "-fcf-protection=full" => self.cf_protection_branch = true,
                "-fcf-protection=none" => self.cf_protection_branch = false,
                arg if arg.starts_with("-fpatchable-function-entry=") => {
                    let val = &arg["-fpatchable-function-entry=".len()..];
                    let parts: Vec<&str> = val.split(',').collect();
                    let total: u32 = parts[0].parse().unwrap_or(0);
                    let before: u32 = if parts.len() > 1 { parts[1].parse().unwrap_or(0) } else { 0 };
                    self.patchable_function_entry = Some((total, before));
                }
                "-fno-jump-tables" => self.no_jump_tables = true,
                "-ffunction-sections" => self.function_sections = true,
                "-fno-function-sections" => self.function_sections = false,
                "-fdata-sections" => self.data_sections = true,
                "-fno-data-sections" => self.data_sections = false,
                "-fcommon" => self.fcommon = true,
                "-fno-common" => self.fcommon = false,
                "-fgnu89-inline" => self.gnu89_inline = true,
                "-fno-gnu89-inline" => self.gnu89_inline = false,
                // Diagnostic color: -fdiagnostics-color, -fdiagnostics-color={auto,always,never}
                "-fdiagnostics-color" | "-fcolor-diagnostics" => {
                    self.color_mode = ColorMode::Always;
                }
                "-fno-diagnostics-color" | "-fno-color-diagnostics" => {
                    self.color_mode = ColorMode::Never;
                }
                arg if arg.starts_with("-fdiagnostics-color=") => {
                    let value = &arg["-fdiagnostics-color=".len()..];
                    if let Some(mode) = ColorMode::from_flag(value) {
                        self.color_mode = mode;
                    }
                    // Unknown values silently ignored (matching GCC)
                }
                arg if arg.starts_with("-f") => {}

                // Linker flags
                "-static" => self.static_link = true,
                "-shared" => self.shared_lib = true,
                "-no-pie" | "-pie" => {}
                "-nostdlib" => self.nostdlib = true,
                "-nostdinc" => self.nostdinc = true,
                "-nodefaultlibs" => {}

                // Language selection
                "-x" => {
                    i += 1;
                    if i < args.len() {
                        let lang = args[i].as_str();
                        if lang == "none" {
                            explicit_language = None;
                        } else {
                            explicit_language = Some(args[i].clone());
                        }
                    } else {
                        return Err("-x requires an argument".to_string());
                    }
                }

                // Dependency generation flags
                "-MD" | "-MMD" => {
                    if self.dep_file.is_none() {
                        self.dep_file = Some(String::new());
                    }
                }
                "-MP" | "-M" | "-MM" => {}
                "-MF" => {
                    i += 1;
                    if i < args.len() {
                        self.dep_file = Some(args[i].clone());
                    }
                }
                "-MT" | "-MQ" => { i += 1; }

                // Misc flags
                "-rdynamic" => {
                    self.linker_ordered_items.push("-rdynamic".to_string());
                }
                "-pipe" | "-pthread" | "-Xa" | "-Xc" | "-Xt" => {}

                // GCC --param flag: --param <name>=<value> or --param=<name>=<value>
                // Used by nix CC wrapper for hardening flags like ssp-buffer-size=4
                "--param" => {
                    // Skip the next argument (the parameter value)
                    i += 1;
                }
                arg if arg.starts_with("--param=") => {
                    // Single-argument form: --param=ssp-buffer-size=4
                    // Silently ignore
                }

                // Stdin input
                "-" => {
                    self.input_files.push("-".to_string());
                    self.explicit_language = explicit_language.clone();
                }

                // Unknown flags
                arg if arg.starts_with('-') => {
                    if self.verbose {
                        eprintln!("warning: unknown flag: {}", arg);
                    }
                }

                // Input file
                _ => {
                    if explicit_language.is_some() {
                        self.explicit_language = explicit_language.clone();
                    }
                    // Track object/archive files in the ordered linker items list
                    // to preserve their position relative to -l and -Wl, flags.
                    // C source files are compiled to temp objects and placed first.
                    if Self::is_object_or_archive(&args[i]) {
                        self.linker_ordered_items.push(args[i].clone());
                    }
                    self.input_files.push(args[i].clone());
                }
            }
            i += 1;
        }

        // Special case: no input files but -Wl,--version is present.
        // Build systems like Meson run `compiler -Wl,--version` without source files
        // to detect the linker type. Invoke our linker driver (GCC) directly.
        if self.input_files.is_empty()
            && self.linker_ordered_items.iter().any(|a| a.contains("--version"))
        {
            let config = self.target.linker_config();
            let mut cmd = std::process::Command::new(config.command);
            cmd.args(config.extra_args);
            for item in &self.linker_ordered_items {
                cmd.arg(item);
            }
            cmd.stdout(std::process::Stdio::inherit());
            cmd.stderr(std::process::Stdio::inherit());
            let _ = cmd.status();
            return Ok(true);
        }

        Ok(false)
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
            Target::I686 => preprocessor.set_target("i686"),
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
        // Set inline semantics mode: -fgnu89-inline or -std=gnu89 uses GNU89
        // inline semantics (__GNUC_GNU_INLINE__), while the default C99+ mode
        // uses __GNUC_STDC_INLINE__. Projects like mpack use these macros to
        // select the correct inline linkage model.
        if self.gnu89_inline {
            preprocessor.set_gnu89_inline(true);
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

        // Set the thread-local target pointer size for type system queries.
        // Must be done before any CType/IrType size computations.
        crate::common::types::set_target_ptr_size(self.target.ptr_size());
        crate::common::types::set_target_long_double_is_f128(
            matches!(self.target, Target::Aarch64 | Target::Riscv64)
        );

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

            let content = Self::read_c_source_file(&resolved.to_string_lossy())
                .map_err(|e| format!("{}: {}", path, e))?;
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
                // Pass through force-include files (-include). Critical for the
                // Linux kernel where kconfig.h defines CONFIG_* macros needed
                // when preprocessing linker scripts (.lds.S -> .lds).
                for inc in &self.force_includes {
                    cmd.arg("-include").arg(inc);
                }
                // Pass through -nostdinc to prevent system headers from interfering
                if self.nostdinc {
                    cmd.arg("-nostdinc");
                }
                // Pass through -U (undefine) flags (e.g., -Uriscv for kernel scripts)
                for undef in &self.undef_macros {
                    cmd.arg(format!("-U{}", undef));
                }
                cmd.arg("-E");
                if self.suppress_line_markers {
                    cmd.arg("-P");
                }
                // Pass through dependency file generation so that build systems
                // like the Linux kernel's fixdep can track header dependencies.
                if let Some(ref dep_path) = self.dep_file {
                    if !dep_path.is_empty() {
                        cmd.arg(format!("-Wp,-MMD,{}", dep_path));
                    }
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

            if self.dump_defines {
                // -dM mode: preprocess the source (to process #define/#undef
                // directives) then dump all resulting macro definitions.
                let _ = preprocessor.preprocess(&source);
                let output = preprocessor.dump_defines();
                if self.output_path_set {
                    std::fs::write(&self.output_path, format!("{}\n", output))
                        .map_err(|e| format!("Cannot write {}: {}", self.output_path, e))?;
                } else {
                    println!("{}", output);
                }
            } else {
                let preprocessed = preprocessor.preprocess(&source);

                let output = if self.suppress_line_markers {
                    Self::strip_line_markers(&preprocessed)
                } else {
                    preprocessed
                };

                if self.output_path_set {
                    std::fs::write(&self.output_path, &output)
                        .map_err(|e| format!("Cannot write {}: {}", self.output_path, e))?;
                    // Write dependency file if requested (e.g., -Wp,-MMD,<depfile>).
                    // The kernel build uses this when preprocessing linker scripts
                    // (.lds.S -> .lds) and fixdep expects the .d file to exist.
                    self.write_dep_file(input_file, &self.output_path);
                } else {
                    print!("{}", output);
                }
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
        use crate::common::temp_files::TempFile;

        // TempFile guards ensure cleanup on all exit paths (success, error, panic).
        let mut temp_guards: Vec<TempFile> = Vec::new();

        let mut extra_passthrough: Vec<String> = Vec::new();

        for input_file in &self.input_files {
            if Self::is_object_or_archive(input_file) {
                // Object/archive files are passed to the linker via linker_ordered_items
                // (populated during argument parsing) to preserve their position relative
                // to -Wl, and -l flags. This is critical for --whole-archive support.
            } else if Self::is_assembly_source(input_file) || self.is_explicit_assembly() {
                // .s/.S files (or -x assembler): pass to assembler, then link
                let tmp = TempFile::new("ccc", Self::input_stem(input_file), "o");
                self.assemble_source_file(input_file, tmp.to_str())?;
                temp_guards.push(tmp);
            } else if !Self::is_c_source(input_file)
                && Self::looks_like_binary_object(input_file)
            {
                // Unrecognized extension but file has ELF/archive magic bytes -
                // treat as object file. These weren't caught by is_object_or_archive
                // at parse time, so add them to the extra passthrough list.
                extra_passthrough.push(input_file.clone());
            } else {
                // Compile .c files to .o
                let asm = self.compile_to_assembly(input_file)?;

                let tmp = TempFile::new("ccc", Self::input_stem(input_file), "o");
                let extra = self.build_asm_extra_args();
                self.target.assemble_with_extra(&asm, tmp.to_str(), &extra)?;
                // Write dependency file for this source file. When compiling and
                // linking in one step, GCC's -Wp,-MMD uses the .o name as the
                // dependency target. We use the output executable path as target,
                // which is sufficient for kernel build's fixdep processing.
                self.write_dep_file(input_file, &self.output_path);
                temp_guards.push(tmp);
            }
        }

        // Compiled temp objects go first (order relative to linker flags doesn't matter
        // for freshly compiled objects). Passthrough objects, -l flags, and -Wl, flags
        // follow in their original command-line order via linker_ordered_items.
        // Extra passthrough objects (detected by magic bytes at runtime) go after temps.
        let mut all_objects: Vec<&str> = temp_guards.iter().map(|t| t.to_str()).collect();
        for obj in &extra_passthrough {
            all_objects.push(obj.as_str());
        }

        // Build linker args from -l, -L, -static flags, preserving positional ordering
        let linker_args = self.build_linker_args();

        if linker_args.is_empty() {
            self.target.link(&all_objects, &self.output_path)?;
        } else {
            self.target.link_with_args(&all_objects, &self.output_path, &linker_args)?;
        }

        // temp_guards drop here, cleaning up all temp .o files automatically.
        // (Also cleans up on early return via ? above.)

        if self.verbose {
            eprintln!("Output: {}", self.output_path);
        }

        Ok(())
    }

    /// Check if a file is an object file or archive (pass to linker directly).
    /// Recognizes standard extensions (.o, .a, .so) plus common variants used by
    /// build systems (.os, .od, .lo, .obj) and versioned shared libs (.so.*).
    fn is_object_or_archive(path: &str) -> bool {
        // Standard extensions
        if path.ends_with(".o") || path.ends_with(".a") || path.ends_with(".so") {
            return true;
        }
        // Common non-standard extensions used by build systems:
        // .os - heatshrink uses for static-variant objects
        // .od - heatshrink uses for dynamic-variant objects
        // .lo - libtool object files
        // .obj - Windows-style object files sometimes used in cross-platform projects
        if path.ends_with(".os") || path.ends_with(".od")
            || path.ends_with(".lo") || path.ends_with(".obj")
        {
            return true;
        }
        // Versioned shared libraries: .so.1, .so.1.2.3, etc.
        if let Some(pos) = path.rfind(".so.") {
            let after = &path[pos + 4..];
            if after.chars().next().is_some_and(|c| c.is_ascii_digit()) {
                return true;
            }
        }
        false
    }

    /// Check if a file has a known C source extension.
    fn is_c_source(path: &str) -> bool {
        path.ends_with(".c") || path.ends_with(".h") || path.ends_with(".i")
    }

    /// Check if a file appears to be a binary object/archive by inspecting magic bytes.
    /// Returns true for ELF files (\x7fELF) and ar archives (!<arch>\n).
    fn looks_like_binary_object(path: &str) -> bool {
        use std::io::Read;
        let mut buf = [0u8; 8];
        if let Ok(mut f) = std::fs::File::open(path) {
            if let Ok(n) = f.read(&mut buf) {
                if n >= 4 && &buf[..4] == b"\x7fELF" {
                    return true;
                }
                if n >= 8 && &buf[..8] == b"!<arch>\n" {
                    return true;
                }
            }
        }
        false
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
        // Pass through force-include files (-include), -nostdinc, and -U flags
        // for .S preprocessing (needed by kernel for kconfig.h, etc.)
        for inc in &self.force_includes {
            cmd.arg("-include").arg(inc);
        }
        if self.nostdinc {
            cmd.arg("-nostdinc");
        }
        for undef in &self.undef_macros {
            cmd.arg(format!("-U{}", undef));
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
        // Pass through any -Wa, flags from the command line. These are needed
        // when compiling C code that contains inline asm requiring specific
        // assembler settings (e.g., -Wa,-misa-spec=2.2 for RISC-V to enable
        // implicit zicsr in the old ISA spec, required by Linux kernel vDSO).
        for flag in &self.assembler_extra_args {
            args.push(format!("-Wa,{}", flag));
        }
        args
    }

    /// Build linker args from collected flags, preserving command-line ordering.
    ///
    /// Order-independent flags (-shared, -static, -nostdlib, -L paths) go first.
    /// Then linker_ordered_items provides the original CLI ordering of positional
    /// object/archive files, -l flags, and -Wl, pass-through flags. This ordering
    /// is critical for flags like -Wl,--whole-archive which must appear before
    /// the archive they affect.
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
        // Emit objects, -l flags, and -Wl, flags in their original command-line order.
        args.extend_from_slice(&self.linker_ordered_items);
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
    ///
    /// Filters out flags that the system GCC may not support. This is necessary
    /// because ccc silently accepts unknown flags (returning success), so the
    /// kernel's `cc-option` mechanism thinks ccc supports GCC-version-specific
    /// flags. When ccc falls back to the system GCC (which may be a different
    /// version than what ccc impersonates), those flags can cause errors.
    fn run_gcc_fallback(&self) -> Result<(), String> {
        let filtered_args = self.filter_args_for_system_gcc();

        let mut cmd = std::process::Command::new("gcc");
        cmd.args(&filtered_args);

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
            return Err(match result.status.code() {
                Some(code) => format!("gcc fallback failed (exit code: {})", code),
                None => "gcc fallback failed (killed by signal)".to_string(),
            });
        }

        Ok(())
    }

    /// Filter command-line arguments to remove flags the system GCC doesn't support.
    ///
    /// ccc pretends to be GCC 14.2 (__GNUC__=14, __GNUC_MINOR__=2), but the system
    /// GCC is typically a different version. The kernel adds version-specific flags
    /// based on cc-option tests (which always pass with ccc since it silently accepts
    /// unknown flags). We test each potentially problematic flag against the real
    /// system GCC and remove those it rejects.
    fn filter_args_for_system_gcc(&self) -> Vec<String> {
        use std::collections::HashSet;

        // Identify flags that might be version-specific and need testing.
        // These are flags that ccc silently accepts but may not exist in the
        // system GCC: --param=..., -fmin-function-alignment, etc.
        let mut flags_to_test: Vec<&str> = Vec::new();
        for arg in &self.original_args {
            if arg.starts_with("--param=") ||
               arg == "-fmin-function-alignment" ||
               arg.starts_with("-fmin-function-alignment=") {
                flags_to_test.push(arg.as_str());
            }
        }

        // Test each flag against the system GCC
        let mut rejected_flags: HashSet<String> = HashSet::new();
        for flag in &flags_to_test {
            if !Self::system_gcc_accepts_flag(flag) {
                rejected_flags.insert(flag.to_string());
            }
        }

        // Build filtered arg list, skipping rejected flags
        self.original_args.iter()
            .filter(|arg| !rejected_flags.contains(arg.as_str()))
            .cloned()
            .collect()
    }

    /// Test whether the system GCC accepts a given flag by compiling /dev/null.
    fn system_gcc_accepts_flag(flag: &str) -> bool {
        let result = std::process::Command::new("gcc")
            .args(["-Werror", flag, "-c", "-x", "c", "/dev/null", "-o", "/dev/null"])
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status();
        match result {
            Ok(status) => status.success(),
            Err(_) => true, // If we can't run gcc, don't filter (let it fail naturally)
        }
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

        // Create diagnostic engine for structured error/warning reporting
        let mut diagnostics = DiagnosticEngine::new();
        diagnostics.set_warning_config(self.warning_config.clone());
        diagnostics.set_color_mode(self.color_mode);

        // Emit preprocessor warnings through diagnostic engine with Cpp kind
        // so they can be controlled via -Wcpp / -Wno-cpp / -Werror=cpp
        for warn in preprocessor.warnings() {
            diagnostics.warning_with_kind_no_span(
                warn.clone(),
                crate::common::error::WarningKind::Cpp,
            );
        }

        // Check for #error directives
        let pp_errors = preprocessor.errors();
        if !pp_errors.is_empty() {
            for err in pp_errors {
                diagnostics.error_no_span(err.clone());
            }
            return Err(format!("{} preprocessor error(s) in {}", pp_errors.len(), input_file));
        }

        // Lex
        let t1 = std::time::Instant::now();
        let mut source_manager = SourceManager::new();
        // Move preprocessed output into source manager (avoids cloning the entire string).
        // Then borrow a reference back for the lexer.
        let file_id = source_manager.add_file(input_file.to_string(), preprocessed);
        // Build line map from preprocessor line markers for source location tracking.
        // Uses stored content from add_file() and reuses already-computed line offsets.
        source_manager.build_line_map();
        let mut lexer = Lexer::new(source_manager.get_content(file_id), file_id);
        lexer.set_gnu_extensions(self.gnu_extensions);
        let tokens = lexer.tokenize();
        if time_phases { eprintln!("[TIME] lex: {:.3}s ({} tokens)", t1.elapsed().as_secs_f64(), tokens.len()); }

        if self.verbose {
            eprintln!("Lexed {} tokens from {}", tokens.len(), input_file);
        }

        // Parse -- the diagnostic engine holds the source manager for span
        // resolution and snippet rendering. It's also set on the parser for
        // backward-compatible span_to_location() calls.
        let t2 = std::time::Instant::now();
        diagnostics.set_source_manager(source_manager);
        let mut parser = Parser::new(tokens);
        parser.set_diagnostics(diagnostics);
        let ast = parser.parse();
        if time_phases { eprintln!("[TIME] parse: {:.3}s", t2.elapsed().as_secs_f64()); }

        if parser.error_count > 0 {
            return Err(format!("{}: {} parse error(s)", input_file, parser.error_count));
        }

        // Retrieve diagnostic engine (which holds the source manager) for subsequent phases
        let mut diagnostics = parser.take_diagnostics();
        // Extract source manager for debug info emission (-g)
        let source_manager = diagnostics.take_source_manager();

        if self.verbose {
            eprintln!("Parsed {} declarations", ast.decls.len());
        }

        // Semantic analysis -- pass diagnostic engine to sema
        let t3 = std::time::Instant::now();
        let mut sema = SemanticAnalyzer::new();
        sema.set_diagnostics(diagnostics);
        if let Err(error_count) = sema.analyze(&ast) {
            // Errors already emitted through diagnostic engine with source spans
            return Err(format!("{} error(s) during semantic analysis", error_count));
        }
        let diagnostics = sema.take_diagnostics();
        let sema_result = sema.into_result();
        if time_phases { eprintln!("[TIME] sema: {:.3}s", t3.elapsed().as_secs_f64()); }

        // Check for warnings promoted to errors by -Werror / -Werror=<name>.
        // The sema pass may have returned Ok (no hard errors), but the diagnostic
        // engine may have accumulated promoted-warning-errors that should stop compilation.
        if diagnostics.has_errors() {
            return Err(format!("{} error(s) (warnings promoted by -Werror)", diagnostics.error_count()));
        }

        // Log diagnostic summary if there were any warnings
        if self.verbose && diagnostics.warning_count() > 0 {
            eprintln!("{} warning(s) generated", diagnostics.warning_count());
        }

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
            diagnostics,
        );
        let (mut module, diagnostics) = lowerer.lower(&ast);

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

        // Apply -fcommon: mark tentative definitions as COMMON symbols.
        // A tentative definition is a global variable at file scope with no initializer
        // and no extern/static storage class. With -fcommon, these use COMMON linkage
        // so the linker merges duplicates across TUs instead of reporting errors.
        if self.fcommon {
            for global in &mut module.globals {
                if !global.is_common && !global.is_extern && !global.is_static
                    && !global.is_thread_local
                    && matches!(global.init, crate::ir::module::GlobalInit::Zero)
                {
                    global.is_common = true;
                }
            }
        }

        if time_phases { eprintln!("[TIME] lowering: {:.3}s ({} functions)", t4.elapsed().as_secs_f64(), module.functions.len()); }

        // Check for errors emitted during lowering (e.g., unresolved types, invalid constructs)
        if diagnostics.has_errors() {
            return Err(format!("{} error(s) during IR lowering", diagnostics.error_count()));
        }

        // Log diagnostic summary if there were any warnings during lowering
        if self.verbose && diagnostics.warning_count() > 0 {
            eprintln!("{} warning(s) during lowering", diagnostics.warning_count());
        }

        if self.verbose {
            eprintln!("Lowered to {} IR functions", module.functions.len());
        }

        // Run optimization passes
        let t5 = std::time::Instant::now();
        promote_allocas(&mut module);
        if time_phases { eprintln!("[TIME] mem2reg: {:.3}s", t5.elapsed().as_secs_f64()); }

        let t6 = std::time::Instant::now();
        run_passes(&mut module, self.opt_level, self.target);
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
            function_sections: self.function_sections,
            data_sections: self.data_sections,
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
