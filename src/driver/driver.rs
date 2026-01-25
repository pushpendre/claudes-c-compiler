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
}

impl Driver {
    pub fn new() -> Self {
        Self {
            target: Target::X86_64,
            output_path: "a.out".to_string(),
            output_path_set: false,
            input_files: Vec::new(),
            opt_level: 0,
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
        }
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
        let stem = std::path::Path::new(input_file)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("a");
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
            let source = std::fs::read_to_string(input_file)
                .map_err(|e| format!("Cannot read {}: {}", input_file, e))?;

            let mut preprocessor = Preprocessor::new();
            self.configure_preprocessor(&mut preprocessor);
            preprocessor.set_filename(input_file);
            self.process_force_includes(&mut preprocessor)?;
            let preprocessed = preprocessor.preprocess(&source);

            if self.output_path_set {
                std::fs::write(&self.output_path, &preprocessed)
                    .map_err(|e| format!("Cannot write {}: {}", self.output_path, e))?;
            } else {
                print!("{}", preprocessed);
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
            if self.verbose {
                eprintln!("Assembly output: {}", out_path);
            }
        }
        Ok(())
    }

    fn run_object_only(&self) -> Result<(), String> {
        for input_file in &self.input_files {
            let asm = self.compile_to_assembly(input_file)?;
            let out_path = self.output_for_input(input_file);
            self.target.assemble(&asm, &out_path)?;
            if self.verbose {
                eprintln!("Object output: {}", out_path);
            }
        }
        Ok(())
    }

    fn run_full(&self) -> Result<(), String> {
        let mut compiled_object_files = Vec::new();
        let mut passthrough_objects: Vec<String> = Vec::new();

        for input_file in &self.input_files {
            if Self::is_object_or_archive(input_file) {
                // Pass .o and .a files directly to the linker
                passthrough_objects.push(input_file.clone());
            } else {
                // Compile .c files to .o
                let asm = self.compile_to_assembly(input_file)?;

                let obj_path = format!("/tmp/ccc_{}_{}.o",
                    std::process::id(),
                    std::path::Path::new(input_file)
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or("out"));
                self.target.assemble(&asm, &obj_path)?;
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

    /// Core pipeline: preprocess, lex, parse, sema, lower, optimize, codegen.
    fn compile_to_assembly(&self, input_file: &str) -> Result<String, String> {
        let source = std::fs::read_to_string(input_file)
            .map_err(|e| format!("Cannot read {}: {}", input_file, e))?;

        // Preprocess
        let mut preprocessor = Preprocessor::new();
        self.configure_preprocessor(&mut preprocessor);
        preprocessor.set_filename(input_file);
        self.process_force_includes(&mut preprocessor)?;
        let preprocessed = preprocessor.preprocess(&source);

        // Lex
        let mut source_manager = SourceManager::new();
        let file_id = source_manager.add_file(input_file.to_string(), preprocessed.clone());
        let mut lexer = Lexer::new(&preprocessed, file_id);
        let tokens = lexer.tokenize();

        if self.verbose {
            eprintln!("Lexed {} tokens from {}", tokens.len(), input_file);
        }

        // Parse
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        if parser.error_count > 0 {
            return Err(format!("{}: {} parse error(s)", input_file, parser.error_count));
        }

        if self.verbose {
            eprintln!("Parsed {} declarations", ast.decls.len());
        }

        // Check for parse errors - must fail with non-zero exit code
        if parser.error_count > 0 {
            return Err(format!("{} parse error(s) in {}", parser.error_count, input_file));
        }

        // Semantic analysis
        let mut sema = SemanticAnalyzer::new();
        if let Err(errors) = sema.analyze(&ast) {
            for err in &errors {
                eprintln!("error: {}", err);
            }
            return Err(format!("{} error(s) during semantic analysis", errors.len()));
        }
        let sema_result = sema.into_result();

        // Lower to IR (target-aware for ABI-specific lowering decisions)
        // Pass sema's TypeContext and function signatures to the lowerer so it has
        // pre-populated type info and knows about all declared functions upfront.
        let lowerer = Lowerer::with_type_context(
            self.target,
            sema_result.type_context,
            sema_result.functions,
        );
        let mut module = lowerer.lower(&ast);

        if self.verbose {
            eprintln!("Lowered to {} IR functions", module.functions.len());
        }

        // Run optimization passes
        promote_allocas(&mut module);
        run_passes(&mut module, self.opt_level);

        // Lower SSA phi nodes to copies before codegen
        eliminate_phis(&mut module);

        // Generate assembly using target-specific codegen
        // PIC mode: enabled by -fPIC/-fpic flag or implicitly when building shared libraries
        let pic = self.pic || self.shared_lib;
        let asm = self.target.generate_assembly_with_options(&module, pic);

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
