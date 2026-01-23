/// Full C preprocessor implementation.
///
/// Handles:
/// - #include directives with file resolution (system and local includes)
/// - #define / #undef (object-like and function-like macros)
/// - #if / #ifdef / #ifndef / #elif / #else / #endif (conditional compilation)
/// - #pragma (once support, others ignored)
/// - #error (emits diagnostic)
/// - Line continuation (backslash-newline)
/// - Macro expansion in non-directive lines
/// - Predefined macros (__LINE__, __FILE__, __STDC__, etc.)

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use super::macro_defs::{MacroDef, MacroTable, parse_define};
use super::conditionals::{ConditionalStack, evaluate_condition};
use super::builtin_macros::define_builtin_macros;
use super::utils::{is_ident_start, is_ident_cont};

pub struct Preprocessor {
    macros: MacroTable,
    conditionals: ConditionalStack,
    includes: Vec<String>,
    filename: String,
    errors: Vec<String>,
    /// Include search paths (from -I flags)
    include_paths: Vec<PathBuf>,
    /// System include paths (default search paths)
    system_include_paths: Vec<PathBuf>,
    /// Files currently being processed (for recursion detection)
    include_stack: Vec<PathBuf>,
    /// Files that have been included with #pragma once
    pragma_once_files: HashSet<PathBuf>,
    /// Whether to actually resolve includes (can be disabled for testing)
    resolve_includes: bool,
    /// Declarations to inject into the output (from #include processing).
    pending_injections: Vec<String>,
}

impl Preprocessor {
    pub fn new() -> Self {
        let mut pp = Self {
            macros: MacroTable::new(),
            conditionals: ConditionalStack::new(),
            includes: Vec::new(),
            filename: String::new(),
            errors: Vec::new(),
            include_paths: Vec::new(),
            system_include_paths: Self::default_system_include_paths(),
            include_stack: Vec::new(),
            pragma_once_files: HashSet::new(),
            resolve_includes: true,
            pending_injections: Vec::new(),
        };
        pp.define_predefined_macros();
        define_builtin_macros(&mut pp.macros);
        pp
    }

    /// Get default system include paths.
    fn default_system_include_paths() -> Vec<PathBuf> {
        let mut paths = Vec::new();
        // Standard system include paths
        let candidates = [
            "/usr/include",
            "/usr/local/include",
            // x86_64 multiarch
            "/usr/include/x86_64-linux-gnu",
            // GCC headers (common versions)
            "/usr/lib/gcc/x86_64-linux-gnu/12/include",
            "/usr/lib/gcc/x86_64-linux-gnu/11/include",
            "/usr/lib/gcc/x86_64-linux-gnu/13/include",
            "/usr/lib/gcc/x86_64-linux-gnu/14/include",
            "/usr/lib/gcc/x86_64-linux-gnu/10/include",
        ];
        for candidate in &candidates {
            let path = PathBuf::from(candidate);
            if path.is_dir() {
                paths.push(path);
            }
        }
        paths
    }

    /// Define standard predefined macros.
    fn define_predefined_macros(&mut self) {
        // Standard C predefined macros
        self.define_simple_macro("__STDC__", "1");
        self.define_simple_macro("__STDC_VERSION__", "201710L"); // C17
        self.define_simple_macro("__STDC_HOSTED__", "1");

        // Platform macros
        self.define_simple_macro("__linux__", "1");
        self.define_simple_macro("__linux", "1");
        self.define_simple_macro("linux", "1");
        self.define_simple_macro("__unix__", "1");
        self.define_simple_macro("__unix", "1");
        self.define_simple_macro("unix", "1");
        self.define_simple_macro("__x86_64__", "1");
        self.define_simple_macro("__x86_64", "1");
        self.define_simple_macro("__amd64__", "1");
        self.define_simple_macro("__amd64", "1");
        self.define_simple_macro("__LP64__", "1");
        self.define_simple_macro("_LP64", "1");

        // GCC compatibility macros
        self.define_simple_macro("__GNUC__", "4");
        self.define_simple_macro("__GNUC_MINOR__", "0");
        self.define_simple_macro("__GNUC_PATCHLEVEL__", "0");

        // Size macros
        self.define_simple_macro("__SIZEOF_POINTER__", "8");
        self.define_simple_macro("__SIZEOF_INT__", "4");
        self.define_simple_macro("__SIZEOF_LONG__", "8");
        self.define_simple_macro("__SIZEOF_LONG_LONG__", "8");
        self.define_simple_macro("__SIZEOF_SHORT__", "2");
        self.define_simple_macro("__SIZEOF_FLOAT__", "4");
        self.define_simple_macro("__SIZEOF_DOUBLE__", "8");
        self.define_simple_macro("__SIZEOF_SIZE_T__", "8");
        self.define_simple_macro("__SIZEOF_PTRDIFF_T__", "8");
        self.define_simple_macro("__SIZEOF_WCHAR_T__", "4");

        // Byte order
        self.define_simple_macro("__BYTE_ORDER__", "__ORDER_LITTLE_ENDIAN__");
        self.define_simple_macro("__ORDER_LITTLE_ENDIAN__", "1234");
        self.define_simple_macro("__ORDER_BIG_ENDIAN__", "4321");

        // Type characteristics
        self.define_simple_macro("__CHAR_BIT__", "8");
        self.define_simple_macro("__INT_MAX__", "2147483647");
        self.define_simple_macro("__LONG_MAX__", "9223372036854775807L");
        self.define_simple_macro("__LONG_LONG_MAX__", "9223372036854775807LL");
        self.define_simple_macro("__SCHAR_MAX__", "127");
        self.define_simple_macro("__SHRT_MAX__", "32767");
        self.define_simple_macro("__SIZE_MAX__", "18446744073709551615UL");
        self.define_simple_macro("__PTRDIFF_MAX__", "9223372036854775807L");
        self.define_simple_macro("__WCHAR_MAX__", "2147483647");
        self.define_simple_macro("__WCHAR_MIN__", "(-2147483647-1)");
        self.define_simple_macro("__WINT_MAX__", "4294967295U");
        self.define_simple_macro("__WINT_MIN__", "0U");
        self.define_simple_macro("__SIG_ATOMIC_MAX__", "2147483647");
        self.define_simple_macro("__SIG_ATOMIC_MIN__", "(-2147483647-1)");

        // Type names (GCC built-in macros)
        self.define_simple_macro("__SIZE_TYPE__", "long unsigned int");
        self.define_simple_macro("__PTRDIFF_TYPE__", "long int");
        self.define_simple_macro("__WCHAR_TYPE__", "int");
        self.define_simple_macro("__WINT_TYPE__", "unsigned int");
        self.define_simple_macro("__INTMAX_TYPE__", "long int");
        self.define_simple_macro("__UINTMAX_TYPE__", "long unsigned int");
        self.define_simple_macro("__INT8_TYPE__", "signed char");
        self.define_simple_macro("__INT16_TYPE__", "short int");
        self.define_simple_macro("__INT32_TYPE__", "int");
        self.define_simple_macro("__INT64_TYPE__", "long int");
        self.define_simple_macro("__UINT8_TYPE__", "unsigned char");
        self.define_simple_macro("__UINT16_TYPE__", "unsigned short int");
        self.define_simple_macro("__UINT32_TYPE__", "unsigned int");
        self.define_simple_macro("__UINT64_TYPE__", "long unsigned int");
        self.define_simple_macro("__INTPTR_TYPE__", "long int");
        self.define_simple_macro("__UINTPTR_TYPE__", "long unsigned int");
        self.define_simple_macro("__INT_LEAST8_TYPE__", "signed char");
        self.define_simple_macro("__INT_LEAST16_TYPE__", "short int");
        self.define_simple_macro("__INT_LEAST32_TYPE__", "int");
        self.define_simple_macro("__INT_LEAST64_TYPE__", "long int");
        self.define_simple_macro("__UINT_LEAST8_TYPE__", "unsigned char");
        self.define_simple_macro("__UINT_LEAST16_TYPE__", "unsigned short int");
        self.define_simple_macro("__UINT_LEAST32_TYPE__", "unsigned int");
        self.define_simple_macro("__UINT_LEAST64_TYPE__", "long unsigned int");
        self.define_simple_macro("__INT_FAST8_TYPE__", "signed char");
        self.define_simple_macro("__INT_FAST16_TYPE__", "long int");
        self.define_simple_macro("__INT_FAST32_TYPE__", "long int");
        self.define_simple_macro("__INT_FAST64_TYPE__", "long int");
        self.define_simple_macro("__UINT_FAST8_TYPE__", "unsigned char");
        self.define_simple_macro("__UINT_FAST16_TYPE__", "long unsigned int");
        self.define_simple_macro("__UINT_FAST32_TYPE__", "unsigned int");
        self.define_simple_macro("__UINT_FAST64_TYPE__", "long unsigned int");

        // FLT/DBL/LDBL characteristics
        self.define_simple_macro("__FLT_MANT_DIG__", "24");
        self.define_simple_macro("__FLT_DIG__", "6");
        self.define_simple_macro("__FLT_MIN_EXP__", "(-125)");
        self.define_simple_macro("__FLT_MAX_EXP__", "128");
        self.define_simple_macro("__FLT_MAX__", "3.40282346638528859811704183484516925e+38F");
        self.define_simple_macro("__FLT_MIN__", "1.17549435082228750796873653722224568e-38F");
        self.define_simple_macro("__FLT_EPSILON__", "1.19209289550781250000000000000000000e-7F");
        self.define_simple_macro("__FLT_RADIX__", "2");
        self.define_simple_macro("__DBL_MANT_DIG__", "53");
        self.define_simple_macro("__DBL_DIG__", "15");
        self.define_simple_macro("__DBL_MIN_EXP__", "(-1021)");
        self.define_simple_macro("__DBL_MAX_EXP__", "1024");
        self.define_simple_macro("__DBL_MAX__", "1.79769313486231570814527423731704357e+308");
        self.define_simple_macro("__DBL_MIN__", "2.22507385850720138309023271733240406e-308");
        self.define_simple_macro("__DBL_EPSILON__", "2.22044604925031308084726333618164062e-16");
        self.define_simple_macro("__LDBL_MANT_DIG__", "64");
        self.define_simple_macro("__LDBL_DIG__", "18");
        self.define_simple_macro("__LDBL_MIN_EXP__", "(-16381)");
        self.define_simple_macro("__LDBL_MAX_EXP__", "16384");
        self.define_simple_macro("__FLT_HAS_INFINITY__", "1");
        self.define_simple_macro("__FLT_HAS_QUIET_NAN__", "1");
        self.define_simple_macro("__DBL_HAS_INFINITY__", "1");
        self.define_simple_macro("__DBL_HAS_QUIET_NAN__", "1");
        self.define_simple_macro("__LDBL_HAS_INFINITY__", "1");
        self.define_simple_macro("__LDBL_HAS_QUIET_NAN__", "1");
        self.define_simple_macro("__FLT_DECIMAL_DIG__", "9");
        self.define_simple_macro("__DBL_DECIMAL_DIG__", "17");
        self.define_simple_macro("__LDBL_DECIMAL_DIG__", "21");
        self.define_simple_macro("__DECIMAL_DIG__", "21");

        // GCC built-in function-like macros
        self.macros.define(MacroDef {
            name: "__builtin_expect".to_string(),
            is_function_like: true,
            params: vec!["exp".to_string(), "c".to_string()],
            is_variadic: false,
            body: "(exp)".to_string(),
            is_predefined: true,
        });
        self.macros.define(MacroDef {
            name: "__builtin_offsetof".to_string(),
            is_function_like: true,
            params: vec!["type".to_string(), "member".to_string()],
            is_variadic: false,
            body: "((unsigned long)&((type *)0)->member)".to_string(),
            is_predefined: true,
        });
        self.macros.define(MacroDef {
            name: "__has_builtin".to_string(),
            is_function_like: true,
            params: vec!["x".to_string()],
            is_variadic: false,
            body: "0".to_string(),
            is_predefined: true,
        });
        self.macros.define(MacroDef {
            name: "__has_attribute".to_string(),
            is_function_like: true,
            params: vec!["x".to_string()],
            is_variadic: false,
            body: "0".to_string(),
            is_predefined: true,
        });
        self.macros.define(MacroDef {
            name: "__has_feature".to_string(),
            is_function_like: true,
            params: vec!["x".to_string()],
            is_variadic: false,
            body: "0".to_string(),
            is_predefined: true,
        });
        self.macros.define(MacroDef {
            name: "__has_include".to_string(),
            is_function_like: true,
            params: vec!["x".to_string()],
            is_variadic: false,
            body: "1".to_string(),
            is_predefined: true,
        });
        self.macros.define(MacroDef {
            name: "__has_include_next".to_string(),
            is_function_like: true,
            params: vec!["x".to_string()],
            is_variadic: false,
            body: "0".to_string(),
            is_predefined: true,
        });
        self.macros.define(MacroDef {
            name: "__has_extension".to_string(),
            is_function_like: true,
            params: vec!["x".to_string()],
            is_variadic: false,
            body: "0".to_string(),
            is_predefined: true,
        });

        // GCC extension macros
        self.define_simple_macro("__GNUC_VA_LIST", "1");
        self.define_simple_macro("__extension__", "");
        self.define_simple_macro("__restrict", "restrict");
        self.define_simple_macro("__restrict__", "restrict");
        self.define_simple_macro("__inline__", "inline");
        self.define_simple_macro("__inline", "inline");
        self.define_simple_macro("__signed__", "signed");
        self.define_simple_macro("__const", "const");
        self.define_simple_macro("__const__", "const");
        self.define_simple_macro("__volatile__", "volatile");
        self.define_simple_macro("__asm__", "asm");
        self.define_simple_macro("__asm", "asm");
        self.define_simple_macro("__typeof__", "typeof");

        // MSVC-compatible integer type specifiers
        self.define_simple_macro("__int8", "char");
        self.define_simple_macro("__int16", "short");
        self.define_simple_macro("__int32", "int");
        self.define_simple_macro("__int64", "long long");

        // ELF ABI: no prefix for user labels on Linux
        self.define_simple_macro("__USER_LABEL_PREFIX__", "");

        // GNU C function declaration attributes
        self.define_simple_macro("__LEAF", "");
        self.define_simple_macro("__LEAF_ATTR", "");
        self.define_simple_macro("__wur", "");

        // __DATE__ and __TIME__
        self.define_simple_macro("__DATE__", "\"Jan  1 2025\"");
        self.define_simple_macro("__TIME__", "\"00:00:00\"");

        // GCC built-in type trait macros
        self.define_simple_macro("__GCC_ATOMIC_BOOL_LOCK_FREE", "2");
        self.define_simple_macro("__GCC_ATOMIC_CHAR_LOCK_FREE", "2");
        self.define_simple_macro("__GCC_ATOMIC_CHAR16_T_LOCK_FREE", "2");
        self.define_simple_macro("__GCC_ATOMIC_CHAR32_T_LOCK_FREE", "2");
        self.define_simple_macro("__GCC_ATOMIC_WCHAR_T_LOCK_FREE", "2");
        self.define_simple_macro("__GCC_ATOMIC_SHORT_LOCK_FREE", "2");
        self.define_simple_macro("__GCC_ATOMIC_INT_LOCK_FREE", "2");
        self.define_simple_macro("__GCC_ATOMIC_LONG_LOCK_FREE", "2");
        self.define_simple_macro("__GCC_ATOMIC_LLONG_LOCK_FREE", "2");
        self.define_simple_macro("__GCC_ATOMIC_POINTER_LOCK_FREE", "2");

        // ELF/Position-independent
        self.define_simple_macro("__ELF__", "1");
        self.define_simple_macro("__PIC__", "2");
        self.define_simple_macro("__pic__", "2");
    }

    /// Helper to define a simple object-like macro.
    fn define_simple_macro(&mut self, name: &str, body: &str) {
        self.macros.define(MacroDef {
            name: name.to_string(),
            is_function_like: false,
            params: Vec::new(),
            is_variadic: false,
            body: body.to_string(),
            is_predefined: true,
        });
    }

    /// Process source code, expanding macros and handling conditionals.
    /// Returns the preprocessed source.
    pub fn preprocess(&mut self, source: &str) -> String {
        // Translation phases: strip block comments, then join continuations
        let source = Self::strip_block_comments(source);
        let source = self.join_continued_lines(&source);

        let mut output = String::with_capacity(source.len());

        // Buffer for accumulating multi-line macro invocations
        let mut pending_line = String::new();
        let mut pending_newlines: usize = 0;

        for (line_num, line) in source.lines().enumerate() {
            let trimmed = line.trim();

            // Update __LINE__
            self.macros.define(MacroDef {
                name: "__LINE__".to_string(),
                is_function_like: false,
                params: Vec::new(),
                is_variadic: false,
                body: (line_num + 1).to_string(),
                is_predefined: true,
            });

            // Check for preprocessor directive.
            // Preprocessor directives take priority even if we're accumulating
            // a multi-line macro invocation (e.g., #ifdef inside an expression).
            if trimmed.starts_with('#') {
                // If we're accumulating a multi-line expression with unbalanced parens,
                // we need to handle directives (#ifdef, #endif, etc.) while preserving
                // the pending accumulation.
                let include_result = self.process_directive(trimmed, line_num + 1);
                if let Some(included_content) = include_result {
                    // An #include was processed; insert the preprocessed content
                    output.push_str(&included_content);
                    output.push('\n');
                }
                // Emit injected declarations from #include processing
                if !self.pending_injections.is_empty() {
                    for decl in std::mem::take(&mut self.pending_injections) {
                        output.push_str(&decl);
                    }
                }
                // Emit a newline to preserve line numbers
                if !pending_line.is_empty() {
                    // We're inside a multi-line accumulation; track newlines
                    pending_newlines += 1;
                } else {
                    output.push('\n');
                }
            } else if self.conditionals.is_active() {
                // Regular line - expand macros if we're in an active conditional
                // Handle multi-line macro invocations by checking for unbalanced parens
                if pending_line.is_empty() {
                    if Self::has_unbalanced_parens(line) {
                        pending_line = line.to_string();
                        pending_newlines = 1;
                    } else {
                        let expanded = self.macros.expand_line(line);
                        output.push_str(&expanded);
                        output.push('\n');
                    }
                } else {
                    // Continue accumulating lines
                    pending_line.push('\n');
                    pending_line.push_str(line);
                    pending_newlines += 1;

                    if !Self::has_unbalanced_parens(&pending_line) {
                        // All parens are balanced - expand the accumulated line
                        let expanded = self.macros.expand_line(&pending_line);
                        output.push_str(&expanded);
                        output.push('\n');
                        // Emit extra newlines to preserve line numbering
                        for _ in 1..pending_newlines {
                            output.push('\n');
                        }
                        pending_line.clear();
                        pending_newlines = 0;
                    } else if pending_newlines > 20 {
                        // Safety: don't accumulate forever
                        let expanded = self.macros.expand_line(&pending_line);
                        output.push_str(&expanded);
                        output.push('\n');
                        for _ in 1..pending_newlines {
                            output.push('\n');
                        }
                        pending_line.clear();
                        pending_newlines = 0;
                    }
                }
            } else {
                // In an inactive conditional block - skip the line
                if !pending_line.is_empty() {
                    // Preserve line count for pending multi-line accumulation
                    pending_newlines += 1;
                } else {
                    output.push('\n');
                }
            }
        }

        // Flush any remaining pending line
        if !pending_line.is_empty() {
            let expanded = self.macros.expand_line(&pending_line);
            output.push_str(&expanded);
            output.push('\n');
        }

        output
    }

    /// Process source code from an included file. This is like preprocess()
    /// but reuses the same preprocessor state (macros, etc.).
    fn preprocess_included(&mut self, source: &str) -> String {
        let source = Self::strip_block_comments(source);
        let source = self.join_continued_lines(&source);
        let mut output = String::with_capacity(source.len());

        // Save and reset conditional stack for included file
        let saved_conditionals = std::mem::replace(&mut self.conditionals, ConditionalStack::new());

        // Buffer for multi-line macro invocations
        let mut pending_line = String::new();
        let mut pending_newlines: usize = 0;

        for (line_num, line) in source.lines().enumerate() {
            let trimmed = line.trim();

            self.macros.define(MacroDef {
                name: "__LINE__".to_string(),
                is_function_like: false,
                params: Vec::new(),
                is_variadic: false,
                body: (line_num + 1).to_string(),
                is_predefined: true,
            });

            if trimmed.starts_with('#') && pending_line.is_empty() {
                let include_result = self.process_directive(trimmed, line_num + 1);
                if let Some(included_content) = include_result {
                    output.push_str(&included_content);
                    output.push('\n');
                } else {
                    output.push('\n');
                }
            } else if self.conditionals.is_active() {
                // Handle multi-line macro invocations
                if pending_line.is_empty() {
                    if Self::has_unbalanced_parens(line) {
                        pending_line = line.to_string();
                        pending_newlines = 1;
                    } else {
                        let expanded = self.macros.expand_line(line);
                        output.push_str(&expanded);
                        output.push('\n');
                    }
                } else {
                    pending_line.push('\n');
                    pending_line.push_str(line);
                    pending_newlines += 1;

                    if !Self::has_unbalanced_parens(&pending_line) {
                        let expanded = self.macros.expand_line(&pending_line);
                        output.push_str(&expanded);
                        output.push('\n');
                        for _ in 1..pending_newlines {
                            output.push('\n');
                        }
                        pending_line.clear();
                        pending_newlines = 0;
                    } else if pending_newlines > 20 {
                        let expanded = self.macros.expand_line(&pending_line);
                        output.push_str(&expanded);
                        output.push('\n');
                        for _ in 1..pending_newlines {
                            output.push('\n');
                        }
                        pending_line.clear();
                        pending_newlines = 0;
                    }
                }
            } else {
                output.push('\n');
            }
        }

        // Flush remaining
        if !pending_line.is_empty() {
            let expanded = self.macros.expand_line(&pending_line);
            output.push_str(&expanded);
            output.push('\n');
        }

        // Restore conditional stack
        self.conditionals = saved_conditionals;

        output
    }

    /// Set the filename for __FILE__ macro and set as the base include directory.
    pub fn set_filename(&mut self, filename: &str) {
        self.filename = filename.to_string();
        self.macros.define(MacroDef {
            name: "__FILE__".to_string(),
            is_function_like: false,
            params: Vec::new(),
            is_variadic: false,
            body: format!("\"{}\"", filename),
            is_predefined: true,
        });
        // Push the file path onto the include stack for relative includes
        // (resolve_include_path uses .parent() to get the directory)
        let path = PathBuf::from(filename);
        let canonical = std::fs::canonicalize(&path)
            .unwrap_or_else(|_| path);
        self.include_stack.push(canonical);
    }

    /// Get the list of includes encountered during preprocessing.
    pub fn includes(&self) -> &[String] {
        &self.includes
    }

    /// Get preprocessing errors.
    pub fn errors(&self) -> &[String] {
        &self.errors
    }

    /// Define a macro from a command-line -D flag.
    /// Takes a name and value (e.g., name="FOO", value="1").
    pub fn define_macro(&mut self, name: &str, value: &str) {
        self.macros.define(MacroDef {
            name: name.to_string(),
            is_function_like: false,
            params: Vec::new(),
            is_variadic: false,
            body: value.to_string(),
            is_predefined: false,
        });
    }

    /// Add an include search path for #include directives (-I flag).
    /// Adds regardless of whether the directory currently exists.
    pub fn add_include_path(&mut self, path: &str) {
        self.include_paths.push(PathBuf::from(path));
    }

    /// Check if a line has unbalanced parentheses, indicating a multi-line
    /// macro invocation that needs to be joined with subsequent lines.
    /// This respects string literals and character literals.
    fn has_unbalanced_parens(line: &str) -> bool {
        let mut depth: i32 = 0;
        let mut in_string = false;
        let mut in_char = false;
        let bytes = line.as_bytes();
        let len = bytes.len();
        let mut i = 0;

        while i < len {
            if in_string {
                if bytes[i] == b'\\' && i + 1 < len {
                    i += 2;
                } else if bytes[i] == b'"' {
                    in_string = false;
                    i += 1;
                } else {
                    i += 1;
                }
            } else if in_char {
                if bytes[i] == b'\\' && i + 1 < len {
                    i += 2;
                } else if bytes[i] == b'\'' {
                    in_char = false;
                    i += 1;
                } else {
                    i += 1;
                }
            } else {
                match bytes[i] {
                    b'"' => { in_string = true; i += 1; }
                    b'\'' => { in_char = true; i += 1; }
                    b'(' => { depth += 1; i += 1; }
                    b')' => { depth -= 1; i += 1; }
                    b'/' if i + 1 < len && bytes[i + 1] == b'/' => {
                        // Line comment - stop processing
                        break;
                    }
                    _ => { i += 1; }
                }
            }
        }

        depth > 0
    }

    /// Strip C-style block comments (/* ... */), preserving newlines
    /// within comments for correct line numbering.
    /// Also strips C++ style line comments (// ...).
    fn strip_block_comments(source: &str) -> String {
        let mut result = String::with_capacity(source.len());
        let bytes = source.as_bytes();
        let len = bytes.len();
        let mut i = 0;
        let mut in_string = false;
        let mut in_char = false;

        while i < len {
            if in_string {
                result.push(bytes[i] as char);
                if bytes[i] == b'\\' && i + 1 < len {
                    i += 1;
                    result.push(bytes[i] as char);
                } else if bytes[i] == b'"' {
                    in_string = false;
                }
                i += 1;
            } else if in_char {
                result.push(bytes[i] as char);
                if bytes[i] == b'\\' && i + 1 < len {
                    i += 1;
                    result.push(bytes[i] as char);
                } else if bytes[i] == b'\'' {
                    in_char = false;
                }
                i += 1;
            } else if bytes[i] == b'"' {
                in_string = true;
                result.push('"');
                i += 1;
            } else if bytes[i] == b'\'' {
                in_char = true;
                result.push('\'');
                i += 1;
            } else if i + 1 < len && bytes[i] == b'/' && bytes[i + 1] == b'*' {
                // Block comment - replace with spaces, preserving newlines
                i += 2;
                result.push(' '); // Replace comment start with space
                while i < len {
                    if i + 1 < len && bytes[i] == b'*' && bytes[i + 1] == b'/' {
                        i += 2;
                        result.push(' '); // Replace comment end with space
                        break;
                    }
                    if bytes[i] == b'\n' {
                        result.push('\n'); // Preserve newlines for line counting
                    }
                    i += 1;
                }
            } else if i + 1 < len && bytes[i] == b'/' && bytes[i + 1] == b'/' {
                // Line comment - skip to end of line (but not the newline itself)
                i += 2;
                while i < len && bytes[i] != b'\n' {
                    i += 1;
                }
                // The newline will be handled in the next iteration
            } else {
                result.push(bytes[i] as char);
                i += 1;
            }
        }

        result
    }

    /// Join lines that end with backslash (line continuation).
    fn join_continued_lines(&self, source: &str) -> String {
        let mut result = String::with_capacity(source.len());
        let mut continuation = false;

        for line in source.lines() {
            if continuation {
                // This line continues from the previous
                if line.ends_with('\\') {
                    result.push_str(&line[..line.len() - 1]);
                    // Still continuing
                } else {
                    result.push_str(line);
                    result.push('\n');
                    continuation = false;
                }
            } else if line.ends_with('\\') {
                result.push_str(&line[..line.len() - 1]);
                continuation = true;
            } else {
                result.push_str(line);
                result.push('\n');
            }
        }

        result
    }

    /// Process a preprocessor directive line.
    /// Returns Some(content) if an #include was processed and should be inserted.
    fn process_directive(&mut self, line: &str, _line_num: usize) -> Option<String> {
        // Strip leading # and whitespace
        let after_hash = line.trim_start_matches('#').trim();

        // Strip trailing comments (// style)
        let after_hash = strip_line_comment(after_hash);

        // Get directive keyword
        let (keyword, rest) = split_first_word(&after_hash);

        // Some directives are processed even in inactive conditional blocks
        match keyword {
            "ifdef" | "ifndef" | "if" => {
                if !self.conditionals.is_active() {
                    // In an inactive block, just push a nested inactive conditional
                    self.conditionals.push_if(false);
                    return None;
                }
            }
            "elif" => {
                self.handle_elif(rest);
                return None;
            }
            "else" => {
                self.conditionals.handle_else();
                return None;
            }
            "endif" => {
                self.conditionals.handle_endif();
                return None;
            }
            _ => {
                // Other directives only processed in active blocks
                if !self.conditionals.is_active() {
                    return None;
                }
            }
        }

        // Process directive in active block
        match keyword {
            "include" => {
                return self.handle_include(rest);
            }
            "include_next" => {
                // GCC extension: include_next searches from the next path in include list
                // For simplicity, treat like include for now
                return self.handle_include(rest);
            }
            "define" => self.handle_define(rest),
            "undef" => self.handle_undef(rest),
            "ifdef" => self.handle_ifdef(rest, false),
            "ifndef" => self.handle_ifdef(rest, true),
            "if" => self.handle_if(rest),
            "pragma" => self.handle_pragma(rest),
            "error" => {
                // Expand macros in error message
                let expanded = self.macros.expand_line(rest);
                self.errors.push(format!("#error {}", expanded));
            }
            "warning" => {
                // GCC extension, emit as warning to stderr
                eprintln!("warning: #warning {}", rest);
            }
            "line" => {
                // TODO: handle #line directives for source mapping
            }
            "" => {
                // Empty # directive (null directive), valid in C
            }
            _ => {
                // Unknown directive, ignore silently
            }
        }

        None
    }

    /// Handle #include directive. Returns the preprocessed content of the included file,
    /// or None if the include couldn't be resolved (falls back to old behavior).
    fn handle_include(&mut self, path: &str) -> Option<String> {
        let path = path.trim();

        // Expand macros in include path (for computed includes)
        let path = if !path.starts_with('<') && !path.starts_with('"') {
            self.macros.expand_line(path)
        } else {
            path.to_string()
        };
        let path = path.trim();

        let (include_path, is_system) = if path.starts_with('<') {
            let end = path.find('>').unwrap_or(path.len());
            (path[1..end].to_string(), true)
        } else if path.starts_with('"') {
            let rest = &path[1..];
            let end = rest.find('"').unwrap_or(rest.len());
            (rest[..end].to_string(), false)
        } else {
            (path.to_string(), false)
        };

        self.includes.push(include_path.clone());

        // Inject declarations for well-known standard headers
        self.inject_header_declarations(&include_path);

        if !self.resolve_includes {
            return None;
        }

        // Resolve the include path to an actual file
        if let Some(resolved_path) = self.resolve_include_path(&include_path, is_system) {
            // Check for #pragma once
            if self.pragma_once_files.contains(&resolved_path) {
                return Some(String::new());
            }

            // Check for recursive inclusion
            if self.include_stack.contains(&resolved_path) {
                // Already including this file; skip to avoid infinite recursion
                return Some(String::new());
            }

            // Read the file
            match std::fs::read_to_string(&resolved_path) {
                Ok(content) => {
                    // Push onto include stack
                    self.include_stack.push(resolved_path.clone());

                    // Update __FILE__
                    let old_file = self.macros.get("__FILE__").map(|m| m.body.clone());
                    self.macros.define(MacroDef {
                        name: "__FILE__".to_string(),
                        is_function_like: false,
                        params: Vec::new(),
                        is_variadic: false,
                        body: format!("\"{}\"", resolved_path.display()),
                        is_predefined: true,
                    });

                    // Preprocess the included content
                    let result = self.preprocess_included(&content);

                    // Restore __FILE__
                    if let Some(old) = old_file {
                        self.macros.define(MacroDef {
                            name: "__FILE__".to_string(),
                            is_function_like: false,
                            params: Vec::new(),
                            is_variadic: false,
                            body: old,
                            is_predefined: true,
                        });
                    }

                    // Pop include stack
                    self.include_stack.pop();

                    Some(result)
                }
                Err(_) => {
                    // Silently skip unresolvable includes (many system headers
                    // may not be needed if builtins provide their macros)
                    None
                }
            }
        } else {
            // Could not resolve; fall back to builtin macro behavior
            None
        }
    }

    /// Resolve an include path to an actual file path.
    /// For "file.h": search current dir, then -I paths, then system paths.
    /// For <file.h>: search -I paths, then system paths.
    fn resolve_include_path(&self, include_path: &str, is_system: bool) -> Option<PathBuf> {
        // For quoted includes, first search relative to the current file's directory
        if !is_system {
            if let Some(current_file) = self.include_stack.last() {
                // Use the parent directory of the current file being processed
                if let Some(current_dir) = current_file.parent() {
                    let candidate = current_dir.join(include_path);
                    if candidate.is_file() {
                        return std::fs::canonicalize(&candidate).ok().or(Some(candidate));
                    }
                }
            }
            // Also try relative to the original source file directory
            if !self.filename.is_empty() {
                if let Some(parent) = Path::new(&self.filename).parent() {
                    let candidate = parent.join(include_path);
                    if candidate.is_file() {
                        return std::fs::canonicalize(&candidate).ok().or(Some(candidate));
                    }
                }
            }
        }

        // Search -I paths
        for dir in &self.include_paths {
            let candidate = dir.join(include_path);
            if candidate.is_file() {
                return std::fs::canonicalize(&candidate).ok().or(Some(candidate));
            }
        }

        // Search system include paths
        for dir in &self.system_include_paths {
            let candidate = dir.join(include_path);
            if candidate.is_file() {
                return std::fs::canonicalize(&candidate).ok().or(Some(candidate));
            }
        }

        None
    }

    /// Inject essential declarations for standard library headers.
    /// Since we don't read actual system headers, we inject the key
    /// type definitions and extern variable declarations that C code
    /// commonly uses from these headers.
    fn inject_header_declarations(&mut self, header: &str) {
        match header {
            "stdio.h" => {
                // FILE type and standard streams
                self.pending_injections.push("typedef struct _IO_FILE FILE;\n".to_string());
                self.pending_injections.push("extern FILE *stdin;\n".to_string());
                self.pending_injections.push("extern FILE *stdout;\n".to_string());
                self.pending_injections.push("extern FILE *stderr;\n".to_string());
            }
            "errno.h" => {
                // errno is typically a macro expanding to (*__errno_location())
                // but for our purposes, treat it as an extern int
                self.pending_injections.push("extern int errno;\n".to_string());
            }
            _ => {}
        }
    }

    fn handle_define(&mut self, rest: &str) {
        if let Some(def) = parse_define(rest) {
            self.macros.define(def);
        }
    }

    fn handle_undef(&mut self, rest: &str) {
        let name = rest.trim().split_whitespace().next().unwrap_or("");
        if !name.is_empty() {
            self.macros.undefine(name);
        }
    }

    fn handle_ifdef(&mut self, rest: &str, negate: bool) {
        let name = rest.trim().split_whitespace().next().unwrap_or("");
        let defined = self.macros.is_defined(name);
        let condition = if negate { !defined } else { defined };
        self.conditionals.push_if(condition);
    }

    fn handle_if(&mut self, expr: &str) {
        // First expand macros, then resolve `defined(X)` and `defined X`
        let resolved = self.resolve_defined_in_expr(expr);
        // Expand macros in the resolved expression
        let expanded = self.macros.expand_line(&resolved);
        // Replace any remaining identifiers with 0 (standard C behavior for #if)
        let final_expr = self.replace_remaining_idents_with_zero(&expanded);
        let condition = evaluate_condition(&final_expr, &self.macros);
        self.conditionals.push_if(condition);
    }

    fn handle_elif(&mut self, expr: &str) {
        let resolved = self.resolve_defined_in_expr(expr);
        let expanded = self.macros.expand_line(&resolved);
        let final_expr = self.replace_remaining_idents_with_zero(&expanded);
        let condition = evaluate_condition(&final_expr, &self.macros);
        self.conditionals.handle_elif(condition);
    }

    fn handle_pragma(&mut self, rest: &str) {
        let rest = rest.trim();
        if rest == "once" {
            // Mark the current file as "include once"
            if let Some(current_file) = self.include_stack.last() {
                self.pragma_once_files.insert(current_file.clone());
            }
        }
        // Other pragmas (GCC, diagnostic, etc.) are silently ignored
    }

    /// Replace remaining identifiers (not keywords) with 0 in a #if expression.
    /// Per C standard, after macro expansion, undefined identifiers in #if evaluate to 0.
    fn replace_remaining_idents_with_zero(&self, expr: &str) -> String {
        let mut result = String::new();
        let chars: Vec<char> = expr.chars().collect();
        let len = chars.len();
        let mut i = 0;

        while i < len {
            if is_ident_start(chars[i]) {
                let start = i;
                while i < len && is_ident_cont(chars[i]) {
                    i += 1;
                }
                let ident: String = chars[start..i].iter().collect();
                // Keep "defined" as-is, replace other identifiers with 0
                if ident == "defined" || ident == "true" || ident == "false" {
                    result.push_str(&ident);
                } else {
                    // Unknown identifier in #if => 0
                    result.push('0');
                }
            } else {
                result.push(chars[i]);
                i += 1;
            }
        }

        result
    }

    /// Replace `defined(X)` and `defined X` with 0 or 1 in an expression.
    fn resolve_defined_in_expr(&self, expr: &str) -> String {
        let mut result = String::new();
        let chars: Vec<char> = expr.chars().collect();
        let len = chars.len();
        let mut i = 0;

        while i < len {
            if i + 7 <= len {
                let word: String = chars[i..i + 7].iter().collect();
                if word == "defined" {
                    // Check that it's not part of a larger identifier
                    let before_ok = i == 0 || !is_ident_cont(chars[i - 1]);
                    let after_ok = i + 7 >= len || !is_ident_cont(chars[i + 7]);
                    if before_ok && after_ok {
                        i += 7;
                        // Skip whitespace
                        while i < len && (chars[i] == ' ' || chars[i] == '\t') {
                            i += 1;
                        }
                        let has_paren = if i < len && chars[i] == '(' {
                            i += 1;
                            // Skip whitespace inside paren
                            while i < len && (chars[i] == ' ' || chars[i] == '\t') {
                                i += 1;
                            }
                            true
                        } else {
                            false
                        };

                        // Read macro name
                        let start = i;
                        while i < len && is_ident_cont(chars[i]) {
                            i += 1;
                        }
                        let name: String = chars[start..i].iter().collect();

                        if has_paren {
                            // Skip whitespace and closing paren
                            while i < len && (chars[i] == ' ' || chars[i] == '\t') {
                                i += 1;
                            }
                            if i < len && chars[i] == ')' {
                                i += 1;
                            }
                        }

                        let is_def = self.macros.is_defined(&name);
                        result.push_str(if is_def { "1" } else { "0" });
                        continue;
                    }
                }
            }

            if is_ident_start(chars[i]) {
                let start = i;
                while i < len && is_ident_cont(chars[i]) {
                    i += 1;
                }
                let ident: String = chars[start..i].iter().collect();
                result.push_str(&ident);
                continue;
            }

            result.push(chars[i]);
            i += 1;
        }

        result
    }
}

impl Default for Preprocessor {
    fn default() -> Self {
        Self::new()
    }
}

/// Strip a // comment from a directive line, but not inside string literals.
fn strip_line_comment(line: &str) -> String {
    let chars: Vec<char> = line.chars().collect();
    let len = chars.len();
    let mut i = 0;
    let mut in_string = false;
    let mut in_char = false;

    while i < len {
        if in_string {
            if chars[i] == '\\' && i + 1 < len {
                i += 2;
            } else if chars[i] == '"' {
                in_string = false;
                i += 1;
            } else {
                i += 1;
            }
        } else if in_char {
            if chars[i] == '\\' && i + 1 < len {
                i += 2;
            } else if chars[i] == '\'' {
                in_char = false;
                i += 1;
            } else {
                i += 1;
            }
        } else {
            if chars[i] == '"' {
                in_string = true;
                i += 1;
            } else if chars[i] == '\'' {
                in_char = true;
                i += 1;
            } else if chars[i] == '/' && i + 1 < len && chars[i + 1] == '/' {
                // Found line comment
                return chars[..i].iter().collect::<String>().trim_end().to_string();
            } else {
                i += 1;
            }
        }
    }

    line.to_string()
}

/// Split a string into the first word and the rest.
fn split_first_word(s: &str) -> (&str, &str) {
    let s = s.trim();
    if let Some(pos) = s.find(|c: char| c.is_whitespace()) {
        (&s[..pos], s[pos..].trim())
    } else {
        (s, "")
    }
}

