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
                // Report GCC 13 to satisfy configure scripts that check compiler version.
                // This matches the system GCC version in our build environment.
                println!("13");
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

            // Warning flags (ignored for now)
            arg if arg.starts_with("-W") => {}

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
                // TODO: implement -U (undefine) support
            }
            arg if arg.starts_with("-U") => {
                // TODO: implement -U (undefine) support
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
            arg if arg.starts_with("-m") => {
                // -m32, -m64, -march=, -mtune=, etc. (ignored for now)
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
            "-nostdinc" | "-nodefaultlibs" => {
                // Silently accepted for GCC compatibility
            }

            // Language selection
            "-x" => {
                i += 1;
                // TODO: handle -x c, -x assembler, etc.
            }

            // Dependency generation flags (ignored, but must consume arguments)
            "-MD" | "-MMD" | "-MP" | "-M" | "-MM" => {
                // Standalone flags, no argument to consume
            }
            "-MF" | "-MT" | "-MQ" => {
                // These take an argument: skip the next arg
                i += 1;
            }

            // -rdynamic: export all symbols to dynamic symbol table (needed for dlopen'd modules)
            "-rdynamic" => {
                driver.linker_extra_args.push("-rdynamic".to_string());
            }

            // Miscellaneous ignored flags
            "-pipe" | "-pthread" | "-Xa" | "-Xc" | "-Xt" => {}

            // Unknown flags - warn in verbose mode
            arg if arg.starts_with('-') => {
                if driver.verbose {
                    eprintln!("warning: unknown flag: {}", arg);
                }
            }

            // Input file
            _ => {
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
