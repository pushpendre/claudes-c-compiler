use ccc::backend::Target;
use ccc::driver::{Driver, CompileMode};

fn main() {
    // Large generated C files (e.g. Bison parsers) can cause deep recursion
    // in our recursive descent parser and IR lowering. Spawn with a larger stack.
    const STACK_SIZE: usize = 64 * 1024 * 1024; // 64 MB
    let builder = std::thread::Builder::new().stack_size(STACK_SIZE);
    let handler = builder.spawn(|| { real_main(); }).unwrap();
    let result = handler.join();
    if result.is_err() {
        std::process::exit(1);
    }
}

fn real_main() {
    let args: Vec<String> = std::env::args().collect();

    // Detect target from binary name
    let binary_name = std::path::Path::new(&args[0])
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("ccc");

    let target = if binary_name.contains("arm") || binary_name.contains("aarch64") {
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
                println!("{}", target.triple());
                return;
            }
            "-dumpversion" => {
                // Report GCC 6.5 to match __GNUC__/__GNUC_MINOR__ macros.
                // Must be >= 5.1 for Linux kernel, < 7 for glibc _Float* compat.
                println!("6");
                return;
            }
            "--version" | "-v" if args.len() == 2 => {
                println!("ccc 0.1.0 (GCC-compatible C compiler)");
                println!("Target: {}", target.triple());
                return;
            }
            "-print-search-dirs" => {
                // Some configure scripts call this; output minimal info
                println!("install: /usr/lib/gcc/{}/13/", target.triple());
                println!("programs: /usr/bin/");
                println!("libraries: /usr/lib/");
                return;
            }
            _ => {}
        }
    }

    let mut driver = Driver::new();
    driver.target = target;
    // Save original args (excluding argv[0]) for gcc_fallback mode
    driver.original_args = args[1..].to_vec();

    // Track -x language setting. When set, the next "-" argument is
    // treated as stdin input rather than as an unknown flag.
    // GCC semantics: "-x none" resets to auto-detect from extension.
    let mut explicit_language: Option<String> = None;

    // Parse command-line arguments (GCC-compatible)
    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            // Output file
            "-o" => {
                i += 1;
                if i < args.len() {
                    driver.output_path = args[i].clone();
                    driver.output_path_set = true;
                } else {
                    eprintln!("error: -o requires an argument");
                    std::process::exit(1);
                }
            }

            // Compilation mode flags
            "-S" => {
                driver.mode = CompileMode::AssemblyOnly;
            }
            "-c" => {
                driver.mode = CompileMode::ObjectOnly;
            }
            "-E" => {
                driver.mode = CompileMode::PreprocessOnly;
            }
            "-P" => {
                driver.suppress_line_markers = true;
            }

            // Optimization levels: all levels run the same maximum optimizations.
            // While the compiler is still maturing, having separate -O0/-O1/-O2
            // tiers creates hard-to-find bugs where code works at one level but
            // breaks at another. We always run all passes until the compiler is
            // stable enough to warrant differentiated optimization tiers.
            "-O" | "-O0" | "-O1" | "-O2" | "-O3" | "-Os" | "-Oz" => {
                driver.opt_level = 2;
            }

            // Debug info
            "-g" => {
                driver.debug_info = true;
            }
            arg if arg.starts_with("-g") && arg.len() > 2 => {
                // -g1, -g2, -g3, -gdwarf, etc. - all enable debug info
                driver.debug_info = true;
            }

            // Verbose/diagnostic flags
            "-v" | "--verbose" => driver.verbose = true,

            // Linker library flags: -lfoo
            arg if arg.starts_with("-l") => {
                driver.linker_libs.push(arg[2..].to_string());
            }

            // Linker pass-through: -Wl,flag1,flag2,...
            // NOTE: must come before the generic -W catch-all to avoid being swallowed.
            // Since we use gcc as the linker driver, re-wrap each flag with -Wl,
            // so gcc forwards them to the linker correctly.
            arg if arg.starts_with("-Wl,") => {
                for flag in arg[4..].split(',') {
                    if !flag.is_empty() {
                        driver.linker_extra_args.push(format!("-Wl,{}", flag));
                    }
                }
            }

            // Assembler pass-through: -Wa,flag1,flag2,...
            // Passes flags to the assembler. Used by kernel's as-version.sh to
            // query the assembler version via -Wa,--version.
            arg if arg.starts_with("-Wa,") => {
                for flag in arg[4..].split(',') {
                    if !flag.is_empty() {
                        driver.assembler_extra_args.push(flag.to_string());
                    }
                }
            }

            // Preprocessor pass-through: -Wp,-MMD,path or -Wp,-MD,path
            // Used by the Linux kernel build to generate dependency files.
            // Format: -Wp,-MMD,<depfile> or -Wp,-MD,<depfile>
            arg if arg.starts_with("-Wp,") => {
                let flags: Vec<&str> = arg[4..].splitn(2, ',').collect();
                if flags.len() == 2 && (flags[0] == "-MMD" || flags[0] == "-MD") {
                    driver.dep_file = Some(flags[1].to_string());
                }
                // Other -Wp, flags are silently ignored
            }

            // Warning flags: -Werror, -Wall, -Wextra, -Wno-*, -Werror=*, -W<name>
            // Processed left-to-right into driver.warning_config to match GCC semantics.
            // Unrecognized warning names are silently ignored (matching GCC behavior).
            arg if arg.starts_with("-W") => {
                let flag = &arg[2..];
                if !flag.is_empty() {
                    driver.warning_config.process_flag(flag);
                }
            }

            // Preprocessor defines: -DFOO or -DFOO=bar or -D FOO
            "-D" => {
                i += 1;
                if i < args.len() {
                    driver.add_define(&args[i]);
                } else {
                    eprintln!("error: -D requires an argument");
                    std::process::exit(1);
                }
            }
            arg if arg.starts_with("-D") => {
                driver.add_define(&arg[2..]);
            }

            // Force-include files: -include path (must be before -I matching)
            "-include" => {
                i += 1;
                if i < args.len() {
                    driver.force_includes.push(args[i].clone());
                } else {
                    eprintln!("error: -include requires an argument");
                    std::process::exit(1);
                }
            }

            // Include paths: -I path or -Ipath
            "-I" => {
                i += 1;
                if i < args.len() {
                    driver.add_include_path(&args[i]);
                } else {
                    eprintln!("error: -I requires an argument");
                    std::process::exit(1);
                }
            }
            arg if arg.starts_with("-I") => {
                driver.add_include_path(&arg[2..]);
            }

            // Library search paths: -L path or -Lpath
            "-L" => {
                i += 1;
                if i < args.len() {
                    driver.linker_paths.push(args[i].clone());
                }
            }
            arg if arg.starts_with("-L") => {
                driver.linker_paths.push(arg[2..].to_string());
            }

            // Undefine macro
            "-U" => {
                i += 1;
                if i < args.len() {
                    driver.undef_macros.push(args[i].clone());
                }
                // TODO: implement -U in our own preprocessor
            }
            arg if arg.starts_with("-U") => {
                driver.undef_macros.push(arg[2..].to_string());
                // TODO: implement -U in our own preprocessor
            }

            // Standard version flag
            arg if arg.starts_with("-std=") => {
                // TODO: handle standard version selection (c99, c11, c17, c2x)
            }

            // Machine/target flags
            "-mfunction-return=thunk-extern" => {
                driver.function_return_thunk = true;
            }
            "-mindirect-branch=thunk-extern" => {
                driver.indirect_branch_thunk = true;
            }
            "-m16" | "-m32" => {
                // 16-bit and 32-bit x86 code generation is not supported.
                // Delegate the entire compilation to the system GCC.
                // The kernel uses -m16 for boot/realmode code (with .code16gcc).
                driver.gcc_fallback = true;
            }
            "-mno-sse" | "-mno-sse2" | "-mno-mmx" | "-mno-sse3" | "-mno-ssse3"
            | "-mno-sse4" | "-mno-sse4.1" | "-mno-sse4.2" | "-mno-avx"
            | "-mno-avx2" | "-mno-avx512f" | "-mno-3dnow" => {
                // Disable SSE/AVX/MMX. The kernel uses -mno-sse -mno-mmx -mno-sse2 etc.
                // to prevent FPU/SIMD use in kernel code. When any of these are set,
                // we must not emit XMM instructions (e.g., in variadic prologues).
                driver.no_sse = true;
            }
            "-mgeneral-regs-only" => {
                // AArch64: disable FP/SIMD register usage. The Linux kernel uses this
                // to prevent NEON/FP instructions in kernel code (kernel_neon_begin/end
                // is required to use SIMD). When set, variadic function prologues must
                // not save q0-q7 and va_start must set __vr_offs=0 (no FP save area).
                driver.general_regs_only = true;
            }
            "-mcmodel=kernel" => {
                // Kernel code model: all symbols in the negative 2GB of virtual address space.
                // Uses absolute sign-extended 32-bit addressing (movq $symbol) for global
                // address references, producing R_X86_64_32S relocations.
                driver.code_model_kernel = true;
            }
            "-mcmodel=small" | "-mcmodel=medlow" | "-mcmodel=medium" | "-mcmodel=medany" | "-mcmodel=large" => {
                // Non-kernel code models: disable kernel code model if it was previously set.
                // GCC uses last-wins semantics for -mcmodel, so -mcmodel=kernel -mcmodel=small
                // should result in the small code model (e.g., Linux vDSO uses this).
                driver.code_model_kernel = false;
            }
            arg if arg.starts_with("-mabi=") => {
                // RISC-V ABI selection: -mabi=lp64, -mabi=lp64d, -mabi=lp64f
                // Passed through to the assembler to set correct ELF float ABI flags.
                let abi = &arg["-mabi=".len()..];
                driver.riscv_abi = Some(abi.to_string());
            }
            arg if arg.starts_with("-march=") => {
                // Architecture selection (e.g., -march=rv64imac_zicsr_zifencei).
                // Passed through to the assembler to set correct ELF arch attributes.
                let march = &arg["-march=".len()..];
                driver.riscv_march = Some(march.to_string());
            }
            "-mno-relax" => {
                // RISC-V: suppress linker relaxation. Prevents R_RISCV_RELAX
                // relocations. Required by Linux kernel EFI stub (-fpic -mno-relax).
                driver.riscv_no_relax = true;
            }
            arg if arg.starts_with("-m") => {
                // -m64, -mtune=, etc. (ignored for now)
            }

            // Feature flags
            "-fPIC" | "-fpic" | "-fPIE" | "-fpie" => {
                driver.pic = true;
            }
            "-fno-PIC" | "-fno-pic" | "-fno-PIE" | "-fno-pie" => {
                driver.pic = false;
            }
            "-fcf-protection=branch" | "-fcf-protection=full" => {
                driver.cf_protection_branch = true;
            }
            "-fcf-protection=none" => {
                driver.cf_protection_branch = false;
            }
            arg if arg.starts_with("-fpatchable-function-entry=") => {
                // Parse -fpatchable-function-entry=N[,M]
                // N = total NOP bytes, M = NOP bytes before entry point (default 0)
                let val = &arg["-fpatchable-function-entry=".len()..];
                let parts: Vec<&str> = val.split(',').collect();
                let total: u32 = parts[0].parse().unwrap_or(0);
                let before: u32 = if parts.len() > 1 { parts[1].parse().unwrap_or(0) } else { 0 };
                driver.patchable_function_entry = Some((total, before));
            }
            "-fno-jump-tables" => {
                driver.no_jump_tables = true;
            }
            arg if arg.starts_with("-f") => {
                // Other -f flags ignored for now
            }

            // Linker flags
            "-static" => {
                driver.static_link = true;
            }
            "-shared" => {
                driver.shared_lib = true;
            }
            "-no-pie" | "-pie" => {
                // Ignored
            }
            "-nostdlib" => {
                driver.nostdlib = true;
            }
            "-nostdinc" => {
                driver.nostdinc = true;
            }
            "-nodefaultlibs" => {
                // Silently accepted for GCC compatibility
            }

            // Language selection: -x c, -x assembler, -x assembler-with-cpp, -x none
            // Sets the language for subsequent input files. "-x none" resets to
            // auto-detect from file extension. This is needed for reading from
            // stdin (where there is no extension to detect from).
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
                    eprintln!("error: -x requires an argument");
                    std::process::exit(1);
                }
            }

            // Dependency generation flags
            "-MD" | "-MMD" => {
                // Enable dependency generation. If no -MF is given, the dep file
                // defaults to the output filename with .d extension.
                // We'll compute this default later if dep_file is not set explicitly.
                if driver.dep_file.is_none() {
                    // Mark that deps were requested; path will be derived from output
                    driver.dep_file = Some(String::new()); // empty = derive from output
                }
            }
            "-MP" | "-M" | "-MM" => {
                // Standalone flags, no argument to consume (ignored for now)
            }
            "-MF" => {
                i += 1;
                if i < args.len() {
                    driver.dep_file = Some(args[i].clone());
                }
            }
            "-MT" | "-MQ" => {
                // These take an argument: skip the next arg
                i += 1;
            }

            // -rdynamic: export all symbols to dynamic symbol table (needed for dlopen'd modules)
            "-rdynamic" => {
                driver.linker_extra_args.push("-rdynamic".to_string());
            }

            // Miscellaneous ignored flags
            "-pipe" | "-pthread" | "-Xa" | "-Xc" | "-Xt" => {}

            // Stdin input: "-" means read from stdin.
            // Used by kernel build's cc-version.sh: echo '...' | $CC -E -P -x c -
            "-" => {
                driver.input_files.push("-".to_string());
                driver.explicit_language = explicit_language.clone();
            }

            // Unknown flags - warn in verbose mode
            arg if arg.starts_with('-') => {
                if driver.verbose {
                    eprintln!("warning: unknown flag: {}", arg);
                }
            }

            // Input file
            _ => {
                // When -x is active, set the language for the file
                if explicit_language.is_some() {
                    driver.explicit_language = explicit_language.clone();
                }
                driver.input_files.push(args[i].clone());
            }
        }
        i += 1;
    }

    if driver.input_files.is_empty() {
        eprintln!("error: no input files");
        std::process::exit(1);
    }

    match driver.run() {
        Ok(()) => {}
        Err(e) => {
            eprintln!("error: {}", e);
            std::process::exit(1);
        }
    }
}
