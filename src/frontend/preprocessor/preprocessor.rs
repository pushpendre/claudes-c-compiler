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
use super::utils::{is_ident_start, is_ident_cont, skip_literal, skip_literal_bytes, copy_literal_bytes};

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

    /// Get default system include paths (arch-neutral only).
    fn default_system_include_paths() -> Vec<PathBuf> {
        let mut paths = Vec::new();
        // Only include arch-neutral paths here; arch-specific paths are added by set_target
        let candidates = [
            "/usr/local/include",
            // x86_64 multiarch (default, removed by set_target for other arches)
            "/usr/include/x86_64-linux-gnu",
            // GCC headers (common versions)
            "/usr/lib/gcc/x86_64-linux-gnu/12/include",
            "/usr/lib/gcc/x86_64-linux-gnu/11/include",
            "/usr/lib/gcc/x86_64-linux-gnu/13/include",
            "/usr/lib/gcc/x86_64-linux-gnu/14/include",
            "/usr/lib/gcc/x86_64-linux-gnu/10/include",
            "/usr/include",
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
        self.define_simple_macro("__LP64__", "1");
        self.define_simple_macro("_LP64", "1");

        // Default to x86_64 arch macros (overridden by set_target)
        self.define_simple_macro("__x86_64__", "1");
        self.define_simple_macro("__x86_64", "1");
        self.define_simple_macro("__amd64__", "1");
        self.define_simple_macro("__amd64", "1");

        // GCC compatibility macros - claim GCC 4.8 compat since we support
        // all builtins available in GCC 4.8 (bswap16/32/64, clz, ctz, etc.)
        self.define_simple_macro("__GNUC__", "4");
        self.define_simple_macro("__GNUC_MINOR__", "8");
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
        self.define_simple_macro("__FLT_MIN_10_EXP__", "(-37)");
        self.define_simple_macro("__FLT_MAX_EXP__", "128");
        self.define_simple_macro("__FLT_MAX_10_EXP__", "38");
        self.define_simple_macro("__FLT_MAX__", "3.40282346638528859811704183484516925e+38F");
        self.define_simple_macro("__FLT_MIN__", "1.17549435082228750796873653722224568e-38F");
        self.define_simple_macro("__FLT_EPSILON__", "1.19209289550781250000000000000000000e-7F");
        self.define_simple_macro("__FLT_RADIX__", "2");
        self.define_simple_macro("__FLT_DENORM_MIN__", "1.40129846432481707092372958328991613e-45F");
        self.define_simple_macro("__DBL_MANT_DIG__", "53");
        self.define_simple_macro("__DBL_DIG__", "15");
        self.define_simple_macro("__DBL_MIN_EXP__", "(-1021)");
        self.define_simple_macro("__DBL_MIN_10_EXP__", "(-307)");
        self.define_simple_macro("__DBL_MAX_EXP__", "1024");
        self.define_simple_macro("__DBL_MAX_10_EXP__", "308");
        self.define_simple_macro("__DBL_MAX__", "1.79769313486231570814527423731704357e+308");
        self.define_simple_macro("__DBL_MIN__", "2.22507385850720138309023271733240406e-308");
        self.define_simple_macro("__DBL_EPSILON__", "2.22044604925031308084726333618164062e-16");
        self.define_simple_macro("__DBL_DENORM_MIN__", "4.94065645841246544176568792868221372e-324");
        self.define_simple_macro("__LDBL_MANT_DIG__", "64");
        self.define_simple_macro("__LDBL_DIG__", "18");
        self.define_simple_macro("__LDBL_MIN_EXP__", "(-16381)");
        self.define_simple_macro("__LDBL_MIN_10_EXP__", "(-4931)");
        self.define_simple_macro("__LDBL_MAX_EXP__", "16384");
        self.define_simple_macro("__LDBL_MAX_10_EXP__", "4932");
        self.define_simple_macro("__LDBL_MAX__", "1.18973149535723176502e+4932L");
        self.define_simple_macro("__LDBL_MIN__", "3.36210314311209350626e-4932L");
        self.define_simple_macro("__LDBL_EPSILON__", "1.08420217248550443401e-19L");
        self.define_simple_macro("__SIZEOF_LONG_DOUBLE__", "16");
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
        self.define_simple_macro("__alignof", "_Alignof");
        self.define_simple_macro("__alignof__", "_Alignof");

        // GCC 128-bit float type: map to long double since we don't support true 128-bit floats.
        // glibc headers declare _Float128 functions when __HAVE_FLOAT128 is set
        // (__GNUC_PREREQ(4,3) on x86_64). The header does `typedef __float128 _Float128;`
        // for GCC < 7.0, so we just need __float128 as a keyword.
        self.define_simple_macro("__float128", "long double");
        self.define_simple_macro("__SIZEOF_FLOAT128__", "16");

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
        self.preprocess_source(source, false)
    }

    /// Process source code from an included file. Same pipeline as preprocess()
    /// but saves/restores the conditional stack and skips pending_injections.
    fn preprocess_included(&mut self, source: &str) -> String {
        self.preprocess_source(source, true)
    }

    /// Unified preprocessing pipeline for both top-level and included sources.
    ///
    /// When `is_include` is true:
    /// - Saves and restores the conditional stack (each file gets its own)
    /// - Does not emit pending_injections (those only apply to top-level)
    /// - Only processes directives when no multi-line accumulation is pending
    fn preprocess_source(&mut self, source: &str, is_include: bool) -> String {
        // Per C standard (C11 5.1.1.2), translation phases are:
        // Phase 2: Line splicing (backslash-newline removal)
        // Phase 3: Comment replacement
        // So we must join continued lines BEFORE stripping comments.
        let source = self.join_continued_lines(source);
        let source = Self::strip_block_comments(&source);
        let mut output = String::with_capacity(source.len());

        // For included files, save and reset the conditional stack
        let saved_conditionals = if is_include {
            Some(std::mem::replace(&mut self.conditionals, ConditionalStack::new()))
        } else {
            None
        };

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

            // Directive handling: top-level files process directives even during
            // multi-line accumulation; included files only when no pending line.
            let is_directive = trimmed.starts_with('#');
            let process_directive = is_directive && (!is_include || pending_line.is_empty());

            if process_directive {
                let include_result = self.process_directive(trimmed, line_num + 1);
                if let Some(included_content) = include_result {
                    output.push_str(&included_content);
                    output.push('\n');
                } else if is_include {
                    // Included files always emit a newline for non-include directives
                    output.push('\n');
                }
                // Top-level: emit injected declarations from #include processing
                if !is_include && !self.pending_injections.is_empty() {
                    for decl in std::mem::take(&mut self.pending_injections) {
                        output.push_str(&decl);
                    }
                }
                // Preserve line numbering during multi-line accumulation
                if !is_include {
                    if !pending_line.is_empty() {
                        pending_newlines += 1;
                    } else {
                        output.push('\n');
                    }
                }
            } else if self.conditionals.is_active() {
                // Regular line (or directive during include with pending line) -
                // expand macros, handling multi-line macro invocations
                self.accumulate_and_expand(
                    line, &mut pending_line, &mut pending_newlines, &mut output,
                );
            } else {
                // Inactive conditional block - skip the line but preserve numbering
                if !is_include && !pending_line.is_empty() {
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

        // Restore conditional stack for included files
        if let Some(saved) = saved_conditionals {
            self.conditionals = saved;
        }

        output
    }

    /// Accumulate lines for multi-line macro invocations (unbalanced parens)
    /// and expand when complete. This is the shared logic for both preprocess
    /// paths, avoiding the previous duplication of ~40 lines.
    fn accumulate_and_expand(
        &self,
        line: &str,
        pending_line: &mut String,
        pending_newlines: &mut usize,
        output: &mut String,
    ) {
        if pending_line.is_empty() {
            if Self::has_unbalanced_parens(line) {
                *pending_line = line.to_string();
                *pending_newlines = 1;
            } else if self.ends_with_funclike_macro(line) {
                // Line ends with a function-like macro name without '(' on same line.
                // Per C standard, whitespace (including newlines) between macro name
                // and '(' is allowed, so accumulate to check next line for '('.
                *pending_line = line.to_string();
                *pending_newlines = 1;
            } else {
                let expanded = self.macros.expand_line(line);
                output.push_str(&expanded);
                output.push('\n');
            }
        } else {
            // Check if this continuation line starts with '(' (after whitespace)
            // when we were accumulating for a trailing function-like macro name.
            let needs_more = Self::has_unbalanced_parens(pending_line);
            pending_line.push('\n');
            pending_line.push_str(line);
            *pending_newlines += 1;

            if needs_more {
                // Was accumulating for unbalanced parens
                if !Self::has_unbalanced_parens(pending_line) || *pending_newlines > 20 {
                    let expanded = self.macros.expand_line(pending_line);
                    output.push_str(&expanded);
                    output.push('\n');
                    for _ in 1..*pending_newlines {
                        output.push('\n');
                    }
                    pending_line.clear();
                    *pending_newlines = 0;
                }
            } else {
                // Was accumulating for trailing function-like macro name.
                // Now we have the next line joined. Check if parens are balanced.
                if Self::has_unbalanced_parens(pending_line) && *pending_newlines <= 20 {
                    // The joined text has unbalanced parens (macro args span more lines)
                    // Keep accumulating.
                } else {
                    // Parens balanced or next line didn't start with '(' - expand now.
                    let expanded = self.macros.expand_line(pending_line);
                    output.push_str(&expanded);
                    output.push('\n');
                    for _ in 1..*pending_newlines {
                        output.push('\n');
                    }
                    pending_line.clear();
                    *pending_newlines = 0;
                }
            }
        }
    }

    /// Check if a line ends with an identifier that is a defined function-like macro.
    /// This is used to detect cases where the macro arguments '(' might be on the next line.
    fn ends_with_funclike_macro(&self, line: &str) -> bool {
        let trimmed = line.trim_end();
        if trimmed.is_empty() {
            return false;
        }
        // Extract the last identifier from the line
        let bytes = trimmed.as_bytes();
        let end = bytes.len();
        // Walk backwards to find end of last identifier
        if !is_ident_cont(bytes[end - 1] as char) {
            return false;
        }
        let mut start = end - 1;
        while start > 0 && is_ident_cont(bytes[start - 1] as char) {
            start -= 1;
        }
        // Check that the identifier starts with a valid start character
        if !is_ident_start(bytes[start] as char) {
            return false;
        }
        let ident = &trimmed[start..end];
        // Check if this identifier is a defined function-like macro
        if let Some(mac) = self.macros.get(ident) {
            mac.is_function_like
        } else {
            false
        }
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

    /// Set the target architecture, updating predefined macros and include paths.
    pub fn set_target(&mut self, target: &str) {
        match target {
            "aarch64" => {
                // Remove x86 macros
                self.macros.undefine("__x86_64__");
                self.macros.undefine("__x86_64");
                self.macros.undefine("__amd64__");
                self.macros.undefine("__amd64");
                // Define aarch64 macros
                self.define_simple_macro("__aarch64__", "1");
                self.define_simple_macro("__ARM_64BIT_STATE", "1");
                self.define_simple_macro("__ARM_ARCH", "8");
                self.define_simple_macro("__ARM_ARCH_ISA_A64", "1");
                // Replace x86 include paths with aarch64 paths
                self.system_include_paths.retain(|p| {
                    let s = p.to_string_lossy();
                    !s.contains("x86_64")
                });
                let aarch64_paths = [
                    "/usr/lib/gcc-cross/aarch64-linux-gnu/11/include",
                    "/usr/lib/gcc-cross/aarch64-linux-gnu/12/include",
                    "/usr/lib/gcc-cross/aarch64-linux-gnu/13/include",
                    "/usr/lib/gcc-cross/aarch64-linux-gnu/14/include",
                    "/usr/aarch64-linux-gnu/include",
                    "/usr/include/aarch64-linux-gnu",
                ];
                for p in &aarch64_paths {
                    let path = PathBuf::from(p);
                    if path.is_dir() {
                        self.system_include_paths.insert(0, path);
                    }
                }
            }
            "riscv64" => {
                // Remove x86 macros
                self.macros.undefine("__x86_64__");
                self.macros.undefine("__x86_64");
                self.macros.undefine("__amd64__");
                self.macros.undefine("__amd64");
                // Define riscv64 macros
                self.define_simple_macro("__riscv", "1");
                self.define_simple_macro("__riscv_xlen", "64");
                // Replace x86 include paths with riscv64 paths
                self.system_include_paths.retain(|p| {
                    let s = p.to_string_lossy();
                    !s.contains("x86_64")
                });
                let riscv_paths = [
                    "/usr/lib/gcc-cross/riscv64-linux-gnu/11/include",
                    "/usr/lib/gcc-cross/riscv64-linux-gnu/12/include",
                    "/usr/lib/gcc-cross/riscv64-linux-gnu/13/include",
                    "/usr/lib/gcc-cross/riscv64-linux-gnu/14/include",
                    "/usr/riscv64-linux-gnu/include",
                    "/usr/include/riscv64-linux-gnu",
                ];
                for p in &riscv_paths {
                    let path = PathBuf::from(p);
                    if path.is_dir() {
                        self.system_include_paths.insert(0, path);
                    }
                }
            }
            _ => {
                // x86_64 is already the default
            }
        }
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
    /// Skips string/char literals and line comments.
    fn has_unbalanced_parens(line: &str) -> bool {
        let mut depth: i32 = 0;
        let bytes = line.as_bytes();
        let len = bytes.len();
        let mut i = 0;

        while i < len {
            match bytes[i] {
                b'"' | b'\'' => {
                    i = skip_literal_bytes(bytes, i, bytes[i]);
                }
                b'(' => { depth += 1; i += 1; }
                b')' => { depth -= 1; i += 1; }
                b'/' if i + 1 < len && bytes[i + 1] == b'/' => break,
                _ => { i += 1; }
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

        while i < len {
            match bytes[i] {
                b'"' | b'\'' => {
                    // Copy string/char literals verbatim (don't strip comments inside)
                    i = copy_literal_bytes(bytes, i, bytes[i], &mut result);
                }
                b'/' if i + 1 < len && bytes[i + 1] == b'*' => {
                    // Block comment - replace with spaces, preserving newlines
                    i += 2;
                    result.push(' ');
                    while i < len {
                        if i + 1 < len && bytes[i] == b'*' && bytes[i + 1] == b'/' {
                            i += 2;
                            result.push(' ');
                            break;
                        }
                        if bytes[i] == b'\n' {
                            result.push('\n');
                        }
                        i += 1;
                    }
                }
                b'/' if i + 1 < len && bytes[i + 1] == b'/' => {
                    // Line comment - skip to end of line
                    i += 2;
                    while i < len && bytes[i] != b'\n' {
                        i += 1;
                    }
                }
                _ => {
                    result.push(bytes[i] as char);
                    i += 1;
                }
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

        // Handle #include<file> and #include"file" (no space between include and path)
        // This handles the common C pattern: #include<stdio.h>
        let (keyword, rest) = if keyword.starts_with("include<") || keyword.starts_with("include\"") {
            ("include", &after_hash["include".len()..])
        } else if keyword.starts_with("include_next<") || keyword.starts_with("include_next\"") {
            ("include_next", &after_hash["include_next".len()..])
        } else {
            (keyword, rest)
        };

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
                // GCC extension: include_next searches from the next path after the
                // current file's directory in the include search list
                return self.handle_include_next(rest);
            }
            "define" => self.handle_define(rest),
            "undef" => self.handle_undef(rest),
            "ifdef" => self.handle_ifdef(rest, false),
            "ifndef" => self.handle_ifdef(rest, true),
            "if" => self.handle_if(rest),
            "pragma" => {
                return self.handle_pragma(rest);
            }
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

    /// Handle #include_next directive (GCC extension).
    /// Searches for the header starting from the next include path after the one
    /// that contained the current file.
    fn handle_include_next(&mut self, path: &str) -> Option<String> {
        let path = path.trim();

        // Parse the include path
        let (include_path, _is_system) = if path.starts_with('<') {
            let end = path.find('>').unwrap_or(path.len());
            (path[1..end].to_string(), true)
        } else if path.starts_with('"') {
            let rest = &path[1..];
            let end = rest.find('"').unwrap_or(rest.len());
            (rest[..end].to_string(), false)
        } else {
            // Try macro expansion
            let expanded = self.macros.expand_line(path);
            let expanded = expanded.trim().to_string();
            if expanded.starts_with('<') {
                let end = expanded.find('>').unwrap_or(expanded.len());
                (expanded[1..end].to_string(), true)
            } else if expanded.starts_with('"') {
                let rest = &expanded[1..];
                let end = rest.find('"').unwrap_or(rest.len());
                (rest[..end].to_string(), false)
            } else {
                (expanded, false)
            }
        };

        if !self.resolve_includes {
            return None;
        }

        // Find which include path the current file is in
        let current_file_dir = self.include_stack.last()
            .and_then(|p| p.parent())
            .map(|p| p.to_path_buf());

        // Resolve using include_next semantics
        if let Some(resolved_path) = self.resolve_include_next_path(&include_path, current_file_dir.as_ref()) {
            // Check for #pragma once
            if self.pragma_once_files.contains(&resolved_path) {
                return Some(String::new());
            }

            // Check for recursive inclusion
            if self.include_stack.contains(&resolved_path) {
                return Some(String::new());
            }

            // Read and preprocess the file
            match std::fs::read_to_string(&resolved_path) {
                Ok(content) => {
                    self.include_stack.push(resolved_path.clone());

                    let old_file = self.macros.get("__FILE__").map(|m| m.body.clone());
                    self.macros.define(MacroDef {
                        name: "__FILE__".to_string(),
                        is_function_like: false,
                        params: Vec::new(),
                        is_variadic: false,
                        body: format!("\"{}\"", resolved_path.display()),
                        is_predefined: true,
                    });

                    let result = self.preprocess_included(&content);

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

                    self.include_stack.pop();
                    Some(result)
                }
                Err(_) => None,
            }
        } else {
            // Fall back to regular include if include_next can't find it
            self.handle_include(path)
        }
    }

    /// Resolve an include path using #include_next semantics: search from the
    /// next include path after the one containing the current file.
    fn resolve_include_next_path(&self, include_path: &str, current_file_dir: Option<&PathBuf>) -> Option<PathBuf> {
        // Collect all search paths in order
        let all_paths: Vec<&Path> = self.include_paths.iter()
            .chain(self.system_include_paths.iter())
            .map(|p| p.as_path())
            .collect();

        // Find which path contains the current file
        let mut found_current = false;
        if let Some(cur_dir) = current_file_dir {
            let cur_dir_canon = std::fs::canonicalize(cur_dir).unwrap_or_else(|_| cur_dir.clone());
            for search_path in &all_paths {
                let search_canon = std::fs::canonicalize(search_path)
                    .unwrap_or_else(|_| search_path.to_path_buf());
                if search_canon == cur_dir_canon {
                    found_current = true;
                    continue;
                }
                if found_current {
                    let candidate = search_path.join(include_path);
                    if candidate.is_file() {
                        return std::fs::canonicalize(&candidate).ok().or(Some(candidate));
                    }
                }
            }
        }

        // If we didn't find the current path in search paths, search all paths
        // (this handles the case where the file was found relative to the source)
        if !found_current {
            for search_path in &all_paths {
                let candidate = search_path.join(include_path);
                if candidate.is_file() {
                    return std::fs::canonicalize(&candidate).ok().or(Some(candidate));
                }
            }
        }

        None
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
            "stdbool.h" => {
                // Define true/false macros only when stdbool.h is explicitly included
                crate::frontend::preprocessor::builtin_macros::define_stdbool_true_false(&mut self.macros);
            }
            "complex.h" => {
                // C99 <complex.h> support
                // Define standard complex macros
                self.macros.define(parse_define("complex _Complex").unwrap());
                self.macros.define(parse_define("_Complex_I (__extension__ 1.0fi)").unwrap());
                self.macros.define(parse_define("I _Complex_I").unwrap());
                self.macros.define(parse_define("__STDC_IEC_559_COMPLEX__ 1").unwrap());
                // Declare complex math functions
                self.pending_injections.push(concat!(
                    "double creal(double _Complex __z);\n",
                    "float crealf(float _Complex __z);\n",
                    "long double creall(long double _Complex __z);\n",
                    "double cimag(double _Complex __z);\n",
                    "float cimagf(float _Complex __z);\n",
                    "long double cimagl(long double _Complex __z);\n",
                    "double _Complex conj(double _Complex __z);\n",
                    "float _Complex conjf(float _Complex __z);\n",
                    "long double _Complex conjl(long double _Complex __z);\n",
                    "double cabs(double _Complex __z);\n",
                    "float cabsf(float _Complex __z);\n",
                    "double carg(double _Complex __z);\n",
                    "float cargf(float _Complex __z);\n",
                ).to_string());
            }
            "stdarg.h" => {
                // Define va_start/va_arg/va_end/va_copy as macros expanding to builtins.
                // This ensures variadic function support works even if the system stdarg.h
                // is not found (e.g., missing GCC cross-compiler include path).
                self.pending_injections.push("typedef __builtin_va_list va_list;\n".to_string());
                self.macros.define(MacroDef {
                    name: "va_start".to_string(),
                    is_function_like: true,
                    params: vec!["ap".to_string(), "last".to_string()],
                    is_variadic: false,
                    body: "__builtin_va_start(ap,last)".to_string(),
                    is_predefined: true,
                });
                self.macros.define(MacroDef {
                    name: "va_end".to_string(),
                    is_function_like: true,
                    params: vec!["ap".to_string()],
                    is_variadic: false,
                    body: "__builtin_va_end(ap)".to_string(),
                    is_predefined: true,
                });
                self.macros.define(MacroDef {
                    name: "va_copy".to_string(),
                    is_function_like: true,
                    params: vec!["dest".to_string(), "src".to_string()],
                    is_variadic: false,
                    body: "__builtin_va_copy(dest,src)".to_string(),
                    is_predefined: true,
                });
                // va_arg is special syntax: __builtin_va_arg(ap, type)
                // It's handled by the parser as a special built-in, so we define
                // the macro to expand to __builtin_va_arg which the lexer recognizes.
                self.macros.define(MacroDef {
                    name: "va_arg".to_string(),
                    is_function_like: true,
                    params: vec!["ap".to_string(), "type".to_string()],
                    is_variadic: false,
                    body: "__builtin_va_arg(ap,type)".to_string(),
                    is_predefined: true,
                });
                // Also define __gnuc_va_list as a typedef
                self.pending_injections.push("typedef __builtin_va_list __gnuc_va_list;\n".to_string());
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

    fn handle_pragma(&mut self, rest: &str) -> Option<String> {
        let rest = rest.trim();
        if rest == "once" {
            // Mark the current file as "include once"
            if let Some(current_file) = self.include_stack.last() {
                self.pragma_once_files.insert(current_file.clone());
            }
            return None;
        }

        // Handle #pragma pack directives
        if let Some(pack_content) = rest.strip_prefix("pack") {
            return self.handle_pragma_pack(pack_content.trim());
        }

        // Other pragmas (GCC, diagnostic, etc.) are silently ignored
        None
    }

    /// Handle #pragma pack directives and emit synthetic tokens for the parser.
    /// Supported forms:
    ///   #pragma pack(N)        - set alignment to N
    ///   #pragma pack()         - reset to default alignment
    ///   #pragma pack(push, N)  - push current and set to N
    ///   #pragma pack(push)     - push current (no change)
    ///   #pragma pack(pop)      - restore previous alignment
    fn handle_pragma_pack(&mut self, content: &str) -> Option<String> {
        let content = content.trim();
        // Must start with '('
        if !content.starts_with('(') {
            return None;
        }
        let inner = content.trim_start_matches('(').trim_end_matches(')').trim();

        if inner.is_empty() {
            // #pragma pack() - reset
            return Some("__ccc_pack_reset ;\n".to_string());
        }

        // Check for push/pop
        if inner == "pop" {
            return Some("__ccc_pack_pop ;\n".to_string());
        }

        if let Some(rest) = inner.strip_prefix("push") {
            let rest = rest.trim().trim_start_matches(',').trim();
            if rest.is_empty() {
                // #pragma pack(push) - push current alignment, don't change
                // Emit push with 0 as sentinel meaning "push current, no change"
                return Some("__ccc_pack_push_0 ;\n".to_string());
            }
            // #pragma pack(push, N)
            if let Ok(n) = rest.parse::<usize>() {
                return Some(format!("__ccc_pack_push_{} ;\n", n));
            }
            return None;
        }

        // #pragma pack(N) - set alignment
        if let Ok(n) = inner.parse::<usize>() {
            return Some(format!("__ccc_pack_set_{} ;\n", n));
        }

        None
    }

    /// Replace remaining identifiers (not keywords) with 0 in a #if expression.
    /// Per C standard, after macro expansion, undefined identifiers in #if evaluate to 0.
    fn replace_remaining_idents_with_zero(&self, expr: &str) -> String {
        let mut result = String::new();
        let chars: Vec<char> = expr.chars().collect();
        let len = chars.len();
        let mut i = 0;

        while i < len {
            // Skip character literals verbatim (don't treat contents as identifiers)
            if chars[i] == '\'' {
                result.push(chars[i]);
                i += 1;
                // Copy contents of char literal, handling escape sequences
                while i < len && chars[i] != '\'' {
                    if chars[i] == '\\' && i + 1 < len {
                        result.push(chars[i]);
                        i += 1;
                        result.push(chars[i]);
                        i += 1;
                    } else {
                        result.push(chars[i]);
                        i += 1;
                    }
                }
                // Copy closing quote
                if i < len && chars[i] == '\'' {
                    result.push(chars[i]);
                    i += 1;
                }
                continue;
            }
            // Skip string literals verbatim
            if chars[i] == '"' {
                result.push(chars[i]);
                i += 1;
                while i < len && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < len {
                        result.push(chars[i]);
                        i += 1;
                        result.push(chars[i]);
                        i += 1;
                    } else {
                        result.push(chars[i]);
                        i += 1;
                    }
                }
                if i < len && chars[i] == '"' {
                    result.push(chars[i]);
                    i += 1;
                }
                continue;
            }
            if chars[i].is_ascii_digit() {
                // Skip entire number literal (hex 0x..., binary 0b..., octal, decimal)
                // including suffixes like ULL, u, l, etc.
                result.push(chars[i]);
                i += 1;
                // After '0', check for hex/binary prefix
                if i < len && chars[i - 1] == '0' && (chars[i] == 'x' || chars[i] == 'X' || chars[i] == 'b' || chars[i] == 'B') {
                    result.push(chars[i]);
                    i += 1;
                }
                // Consume digits and hex digits
                while i < len && (chars[i].is_ascii_alphanumeric() || chars[i] == '.') {
                    result.push(chars[i]);
                    i += 1;
                }
            } else if is_ident_start(chars[i]) {
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

    /// Replace `defined(X)`, `defined X`, `__has_builtin(X)`, `__has_attribute(X)`,
    /// `__has_feature(X)`, and `__has_extension(X)` with 0 or 1 in a #if expression.
    /// These must be resolved BEFORE macro expansion to prevent spurious expansion.
    fn resolve_defined_in_expr(&self, expr: &str) -> String {
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

                if ident == "defined" {
                    // Handle `defined(X)` and `defined X`
                    // Skip whitespace
                    while i < len && (chars[i] == ' ' || chars[i] == '\t') {
                        i += 1;
                    }
                    let has_paren = if i < len && chars[i] == '(' {
                        i += 1;
                        while i < len && (chars[i] == ' ' || chars[i] == '\t') {
                            i += 1;
                        }
                        true
                    } else {
                        false
                    };

                    let name_start = i;
                    while i < len && is_ident_cont(chars[i]) {
                        i += 1;
                    }
                    let name: String = chars[name_start..i].iter().collect();

                    if has_paren {
                        while i < len && (chars[i] == ' ' || chars[i] == '\t') {
                            i += 1;
                        }
                        if i < len && chars[i] == ')' {
                            i += 1;
                        }
                    }

                    let is_def = self.macros.is_defined(&name);
                    result.push_str(if is_def { "1" } else { "0" });
                } else if ident == "__has_builtin" {
                    // Handle `__has_builtin(name)` - resolve before macro expansion
                    let val = self.resolve_has_builtin_call(&chars, &mut i);
                    result.push_str(val);
                } else if ident == "__has_attribute" {
                    let val = self.resolve_has_attribute_call(&chars, &mut i);
                    result.push_str(val);
                } else if ident == "__has_feature" || ident == "__has_extension" {
                    // Skip the parenthesized argument and return "0"
                    self.skip_paren_arg(&chars, &mut i);
                    result.push_str("0");
                } else {
                    result.push_str(&ident);
                }
                continue;
            }

            result.push(chars[i]);
            i += 1;
        }

        result
    }

    /// Parse `(name)` after `__has_builtin` and return "1" or "0".
    fn resolve_has_builtin_call(&self, chars: &[char], i: &mut usize) -> &'static str {
        let len = chars.len();
        // Skip whitespace
        while *i < len && (chars[*i] == ' ' || chars[*i] == '\t') {
            *i += 1;
        }
        if *i >= len || chars[*i] != '(' {
            return "0";
        }
        *i += 1; // skip '('
        while *i < len && (chars[*i] == ' ' || chars[*i] == '\t') {
            *i += 1;
        }
        let start = *i;
        while *i < len && is_ident_cont(chars[*i]) {
            *i += 1;
        }
        let name: String = chars[start..*i].iter().collect();
        // Skip to closing paren
        while *i < len && chars[*i] != ')' {
            *i += 1;
        }
        if *i < len {
            *i += 1; // skip ')'
        }
        if Self::is_supported_builtin(&name) { "1" } else { "0" }
    }

    /// Parse `(name)` after `__has_attribute` and return "1" or "0".
    fn resolve_has_attribute_call(&self, chars: &[char], i: &mut usize) -> &'static str {
        let len = chars.len();
        while *i < len && (chars[*i] == ' ' || chars[*i] == '\t') {
            *i += 1;
        }
        if *i >= len || chars[*i] != '(' {
            return "0";
        }
        *i += 1;
        while *i < len && (chars[*i] == ' ' || chars[*i] == '\t') {
            *i += 1;
        }
        let start = *i;
        while *i < len && is_ident_cont(chars[*i]) {
            *i += 1;
        }
        let name: String = chars[start..*i].iter().collect();
        while *i < len && chars[*i] != ')' {
            *i += 1;
        }
        if *i < len {
            *i += 1;
        }
        if Self::is_supported_attribute(&name) { "1" } else { "0" }
    }

    /// Skip a parenthesized argument, used for unsupported __has_* operators.
    fn skip_paren_arg(&self, chars: &[char], i: &mut usize) {
        let len = chars.len();
        while *i < len && (chars[*i] == ' ' || chars[*i] == '\t') {
            *i += 1;
        }
        if *i < len && chars[*i] == '(' {
            *i += 1;
            let mut depth = 1;
            while *i < len && depth > 0 {
                if chars[*i] == '(' {
                    depth += 1;
                } else if chars[*i] == ')' {
                    depth -= 1;
                }
                *i += 1;
            }
        }
    }

    /// Check if a builtin function name is supported by this compiler.
    fn is_supported_builtin(name: &str) -> bool {
        matches!(name,
            // Byte swap builtins
            "__builtin_bswap16" | "__builtin_bswap32" | "__builtin_bswap64" |
            // Bit manipulation
            "__builtin_clz" | "__builtin_clzl" | "__builtin_clzll" |
            "__builtin_ctz" | "__builtin_ctzl" | "__builtin_ctzll" |
            "__builtin_popcount" | "__builtin_popcountl" | "__builtin_popcountll" |
            "__builtin_parity" | "__builtin_parityl" | "__builtin_parityll" |
            "__builtin_ffs" | "__builtin_ffsl" | "__builtin_ffsll" |
            // Compiler hints
            "__builtin_expect" | "__builtin_expect_with_probability" |
            "__builtin_unreachable" | "__builtin_trap" |
            "__builtin_assume_aligned" |
            "__builtin_constant_p" |
            "__builtin_types_compatible_p" |
            // Memory operations
            "__builtin_memcpy" | "__builtin_memmove" | "__builtin_memset" |
            "__builtin_memcmp" | "__builtin_strlen" | "__builtin_strcmp" |
            "__builtin_strcpy" | "__builtin_strncpy" | "__builtin_strncmp" |
            "__builtin_strcat" | "__builtin_strchr" | "__builtin_strrchr" |
            "__builtin_strstr" |
            // Math
            "__builtin_abs" | "__builtin_labs" | "__builtin_llabs" |
            "__builtin_fabs" | "__builtin_fabsf" | "__builtin_fabsl" |
            "__builtin_sqrt" | "__builtin_sqrtf" |
            "__builtin_inf" | "__builtin_inff" | "__builtin_infl" |
            "__builtin_huge_val" | "__builtin_huge_valf" | "__builtin_huge_vall" |
            "__builtin_nan" | "__builtin_nanf" | "__builtin_nanl" |
            "__builtin_isnan" | "__builtin_isinf" | "__builtin_isfinite" |
            "__builtin_isnormal" | "__builtin_signbit" | "__builtin_signbitf" |
            "__builtin_fpclassify" |
            "__builtin_isgreater" | "__builtin_isgreaterequal" |
            "__builtin_isless" | "__builtin_islessequal" |
            "__builtin_islessgreater" | "__builtin_isunordered" |
            // I/O
            "__builtin_printf" | "__builtin_fprintf" |
            "__builtin_sprintf" | "__builtin_snprintf" |
            "__builtin_puts" | "__builtin_putchar" |
            // Allocation
            "__builtin_malloc" | "__builtin_calloc" | "__builtin_realloc" |
            "__builtin_free" | "__builtin_alloca" |
            // Other
            "__builtin_abort" | "__builtin_exit" |
            "__builtin_return_address" | "__builtin_frame_address" |
            "__builtin_offsetof" |
            // Complex
            "__builtin_creal" | "__builtin_crealf" | "__builtin_creall" |
            "__builtin_cimag" | "__builtin_cimagf" | "__builtin_cimagl" |
            // Atomics
            "__sync_synchronize" |
            "__atomic_load_n" | "__atomic_store_n" |
            "__atomic_exchange_n" | "__atomic_compare_exchange_n" |
            "__atomic_fetch_add" | "__atomic_fetch_sub" |
            "__atomic_fetch_and" | "__atomic_fetch_or" | "__atomic_fetch_xor" |
            "__atomic_add_fetch" | "__atomic_sub_fetch" |
            "__atomic_and_fetch" | "__atomic_or_fetch" | "__atomic_xor_fetch" |
            "__sync_fetch_and_add" | "__sync_fetch_and_sub" |
            "__sync_fetch_and_and" | "__sync_fetch_and_or" | "__sync_fetch_and_xor" |
            "__sync_add_and_fetch" | "__sync_sub_and_fetch" |
            "__sync_and_and_fetch" | "__sync_or_and_fetch" | "__sync_xor_and_fetch" |
            "__sync_val_compare_and_swap" | "__sync_bool_compare_and_swap" |
            "__sync_lock_test_and_set" | "__sync_lock_release" |
            // Va args
            "__builtin_va_start" | "__builtin_va_end" |
            "__builtin_va_arg" | "__builtin_va_copy"
        )
    }

    /// Check if an attribute is supported by this compiler.
    fn is_supported_attribute(name: &str) -> bool {
        matches!(name,
            "aligned" | "__aligned__" |
            "packed" | "__packed__" |
            "unused" | "__unused__" |
            "used" | "__used__" |
            "weak" | "__weak__" |
            "alias" | "__alias__" |
            "section" | "__section__" |
            "visibility" | "__visibility__" |
            "deprecated" | "__deprecated__" |
            "noreturn" | "__noreturn__" |
            "noinline" | "__noinline__" |
            "always_inline" | "__always_inline__" |
            "constructor" | "__constructor__" |
            "destructor" | "__destructor__" |
            "format" | "__format__" |
            "warn_unused_result" | "__warn_unused_result__" |
            "nonnull" | "__nonnull__" |
            "const" | "__const__" |
            "pure" | "__pure__" |
            "cold" | "__cold__" |
            "hot" | "__hot__" |
            "malloc" | "__malloc__" |
            "sentinel" | "__sentinel__" |
            "may_alias" | "__may_alias__" |
            "transparent_union" | "__transparent_union__"
        )
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

    while i < len {
        match chars[i] {
            '"' | '\'' => {
                i = skip_literal(&chars, i, chars[i]);
            }
            '/' if i + 1 < len && chars[i + 1] == '/' => {
                return chars[..i].iter().collect::<String>().trim_end().to_string();
            }
            _ => i += 1,
        }
    }

    line.to_string()
}

/// Split a string into the first word and the rest.
/// For preprocessor directives, '(' is also a word boundary so that
/// `#if(expr)` is correctly parsed as keyword="if", rest="(expr)".
fn split_first_word(s: &str) -> (&str, &str) {
    let s = s.trim();
    if let Some(pos) = s.find(|c: char| c.is_whitespace() || c == '(') {
        if s.as_bytes()[pos] == b'(' {
            // Don't trim the '(' - it's part of the rest
            (&s[..pos], &s[pos..])
        } else {
            (&s[..pos], s[pos..].trim())
        }
    } else {
        (s, "")
    }
}

