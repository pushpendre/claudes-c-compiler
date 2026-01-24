use ccc::backend::Target;
use ccc::driver::{Driver, CompileMode};

fn main() {
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

            // Optimization levels
            "-O" | "-O1" => driver.opt_level = 1,
            "-O0" => driver.opt_level = 0,
            "-O2" => driver.opt_level = 2,
            "-O3" => driver.opt_level = 3,
            "-Os" => driver.opt_level = 2, // treat size opt as O2 for now
            "-Oz" => driver.opt_level = 2,

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

            // Warning flags (ignored for now)
            // Note: -Wl, is handled below (linker pass-through), so exclude it here
            arg if arg.starts_with("-W") && !arg.starts_with("-Wl,") => {}

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

            // Machine/target flags (ignored for now)
            arg if arg.starts_with("-m") => {
                // -m32, -m64, -march=, -mtune=, etc.
            }

            // Feature flags
            "-fPIC" | "-fpic" | "-fPIE" | "-fpie" => {
                driver.pic = true;
            }
            "-fno-PIC" | "-fno-pic" | "-fno-PIE" | "-fno-pie" => {
                driver.pic = false;
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

            // Linker pass-through: -Wl,flag1,flag2,...
            // Since we use gcc as the linker driver, re-wrap each flag with -Wl,
            // so gcc forwards them to the linker correctly.
            arg if arg.starts_with("-Wl,") => {
                for flag in arg[4..].split(',') {
                    if !flag.is_empty() {
                        driver.linker_extra_args.push(format!("-Wl,{}", flag));
                    }
                }
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

            // -rdynamic: export all symbols to dynamic symbol table
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
