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

use crate::common::fx_hash::{FxHashMap, FxHashSet};
use std::path::PathBuf;

use super::macro_defs::{MacroDef, MacroTable, parse_define};
use super::conditionals::{ConditionalStack, evaluate_condition};
use super::builtin_macros::define_builtin_macros;
use super::utils::{is_ident_start, is_ident_cont, skip_literal, skip_literal_bytes, copy_literal_bytes_raw};

pub struct Preprocessor {
    pub(super) macros: MacroTable,
    conditionals: ConditionalStack,
    pub(super) includes: Vec<String>,
    pub(super) filename: String,
    pub(super) errors: Vec<String>,
    /// Include search paths (from -I flags)
    pub(super) include_paths: Vec<PathBuf>,
    /// System include paths (default search paths)
    pub(super) system_include_paths: Vec<PathBuf>,
    /// Files currently being processed (for recursion detection)
    pub(super) include_stack: Vec<PathBuf>,
    /// Files that have been included with #pragma once
    pub(super) pragma_once_files: FxHashSet<PathBuf>,
    /// Whether to actually resolve includes (can be disabled for testing)
    pub(super) resolve_includes: bool,
    /// Declarations to inject into the output (from #include processing).
    pub(super) pending_injections: Vec<String>,
    /// Stack for #pragma push_macro / pop_macro.
    /// Maps macro name -> stack of saved definitions (None = was undefined).
    macro_save_stack: FxHashMap<String, Vec<Option<MacroDef>>>,
    /// Line offset set by #line directive: effective_line = line_offset + (source_line - line_offset_base)
    /// When None, no #line has been issued and __LINE__ uses the source line directly.
    line_override: Option<(usize, usize)>, // (target_line, source_line_at_directive)
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
            pragma_once_files: FxHashSet::default(),
            resolve_includes: true,
            pending_injections: Vec::new(),
            macro_save_stack: FxHashMap::default(),
            line_override: None,
        };
        pp.define_predefined_macros();
        define_builtin_macros(&mut pp.macros);
        pp
    }

    /// Locate the bundled `include/` directory shipped alongside the binary.
    ///
    /// Walks up to 5 parent directories from the canonicalized executable path
    /// looking for an `include/` directory that contains `emmintrin.h`.
    /// Falls back to the compile-time `CARGO_MANIFEST_DIR/include` path.
    /// Returns `Some(path)` when a valid bundled include directory is found.
    fn bundled_include_dir() -> Option<PathBuf> {
        // Try to find the include dir relative to the running binary.
        if let Ok(exe) = std::env::current_exe() {
            if let Ok(canonical) = exe.canonicalize() {
                let mut dir = canonical.as_path().parent();
                for _ in 0..5 {
                    if let Some(d) = dir {
                        let candidate = d.join("include");
                        if candidate.join("emmintrin.h").is_file() {
                            return Some(candidate);
                        }
                        dir = d.parent();
                    } else {
                        break;
                    }
                }
            }
        }

        // Compile-time fallback: CARGO_MANIFEST_DIR/include
        let fallback = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("include");
        if fallback.join("emmintrin.h").is_file() {
            return Some(fallback);
        }

        None
    }

    /// Get default system include paths (arch-neutral only).
    fn default_system_include_paths() -> Vec<PathBuf> {
        let mut paths = Vec::new();
        // Bundled include directory takes priority over system GCC headers
        if let Some(bundled) = Self::bundled_include_dir() {
            paths.push(bundled);
        }
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
    ///
    /// Object-like macros are defined via a static table to keep this compact.
    /// Function-like macros (with parameters) are defined individually below.
    fn define_predefined_macros(&mut self) {
        // All object-like predefined macros as (name, body) pairs.
        // Grouped by category; order matches GCC's predefined macro output.
        const PREDEFINED_OBJECT_MACROS: &[(&str, &str)] = &[
            // Standard C
            ("__STDC__", "1"),
            ("__STDC_VERSION__", "201710L"), // C17
            ("__STDC_HOSTED__", "1"),
            // Platform
            ("__linux__", "1"), ("__linux", "1"), ("linux", "1"),
            ("__unix__", "1"), ("__unix", "1"), ("unix", "1"),
            ("__LP64__", "1"), ("_LP64", "1"),
            // Default arch: x86_64 (overridden by set_target)
            ("__x86_64__", "1"), ("__x86_64", "1"),
            ("__amd64__", "1"), ("__amd64", "1"),
            // GCC compat: claim GCC 4.8 (we support bswap16/32/64, clz, ctz, etc.)
            ("__GNUC__", "4"), ("__GNUC_MINOR__", "8"), ("__GNUC_PATCHLEVEL__", "0"),
            // sizeof macros
            ("__SIZEOF_POINTER__", "8"), ("__SIZEOF_INT__", "4"),
            ("__SIZEOF_LONG__", "8"), ("__SIZEOF_LONG_LONG__", "8"),
            ("__SIZEOF_SHORT__", "2"), ("__SIZEOF_FLOAT__", "4"),
            ("__SIZEOF_DOUBLE__", "8"), ("__SIZEOF_SIZE_T__", "8"),
            ("__SIZEOF_PTRDIFF_T__", "8"), ("__SIZEOF_WCHAR_T__", "4"),
            ("__SIZEOF_INT128__", "16"), ("__SIZEOF_WINT_T__", "4"),
            // Byte order
            ("__BYTE_ORDER__", "__ORDER_LITTLE_ENDIAN__"),
            ("__ORDER_LITTLE_ENDIAN__", "1234"), ("__ORDER_BIG_ENDIAN__", "4321"),
            // Type limits
            ("__CHAR_BIT__", "8"),
            ("__INT_MAX__", "2147483647"),
            ("__LONG_MAX__", "9223372036854775807L"),
            ("__LONG_LONG_MAX__", "9223372036854775807LL"),
            ("__SCHAR_MAX__", "127"), ("__SHRT_MAX__", "32767"),
            ("__SIZE_MAX__", "18446744073709551615UL"),
            ("__PTRDIFF_MAX__", "9223372036854775807L"),
            ("__WCHAR_MAX__", "2147483647"), ("__WCHAR_MIN__", "(-2147483647-1)"),
            ("__WINT_MAX__", "4294967295U"), ("__WINT_MIN__", "0U"),
            ("__SIG_ATOMIC_MAX__", "2147483647"), ("__SIG_ATOMIC_MIN__", "(-2147483647-1)"),
            // Type names
            ("__SIZE_TYPE__", "long unsigned int"), ("__PTRDIFF_TYPE__", "long int"),
            ("__WCHAR_TYPE__", "int"), ("__WINT_TYPE__", "unsigned int"),
            ("__CHAR16_TYPE__", "short unsigned int"), ("__CHAR32_TYPE__", "unsigned int"),
            ("__INTMAX_TYPE__", "long int"), ("__UINTMAX_TYPE__", "long unsigned int"),
            ("__INT8_TYPE__", "signed char"), ("__INT16_TYPE__", "short int"),
            ("__INT32_TYPE__", "int"), ("__INT64_TYPE__", "long int"),
            ("__UINT8_TYPE__", "unsigned char"), ("__UINT16_TYPE__", "unsigned short int"),
            ("__UINT32_TYPE__", "unsigned int"), ("__UINT64_TYPE__", "long unsigned int"),
            ("__INTPTR_TYPE__", "long int"), ("__UINTPTR_TYPE__", "long unsigned int"),
            ("__INT_LEAST8_TYPE__", "signed char"), ("__INT_LEAST16_TYPE__", "short int"),
            ("__INT_LEAST32_TYPE__", "int"), ("__INT_LEAST64_TYPE__", "long int"),
            ("__UINT_LEAST8_TYPE__", "unsigned char"),
            ("__UINT_LEAST16_TYPE__", "unsigned short int"),
            ("__UINT_LEAST32_TYPE__", "unsigned int"),
            ("__UINT_LEAST64_TYPE__", "long unsigned int"),
            ("__INT_FAST8_TYPE__", "signed char"), ("__INT_FAST16_TYPE__", "long int"),
            ("__INT_FAST32_TYPE__", "long int"), ("__INT_FAST64_TYPE__", "long int"),
            ("__UINT_FAST8_TYPE__", "unsigned char"),
            ("__UINT_FAST16_TYPE__", "long unsigned int"),
            ("__UINT_FAST32_TYPE__", "unsigned int"),
            ("__UINT_FAST64_TYPE__", "long unsigned int"),
            // FLT characteristics
            ("__FLT_MANT_DIG__", "24"), ("__FLT_DIG__", "6"),
            ("__FLT_MIN_EXP__", "(-125)"), ("__FLT_MIN_10_EXP__", "(-37)"),
            ("__FLT_MAX_EXP__", "128"), ("__FLT_MAX_10_EXP__", "38"),
            ("__FLT_MAX__", "3.40282346638528859811704183484516925e+38F"),
            ("__FLT_MIN__", "1.17549435082228750796873653722224568e-38F"),
            ("__FLT_EPSILON__", "1.19209289550781250000000000000000000e-7F"),
            ("__FLT_RADIX__", "2"),
            ("__FLT_DENORM_MIN__", "1.40129846432481707092372958328991613e-45F"),
            // DBL characteristics
            ("__DBL_MANT_DIG__", "53"), ("__DBL_DIG__", "15"),
            ("__DBL_MIN_EXP__", "(-1021)"), ("__DBL_MIN_10_EXP__", "(-307)"),
            ("__DBL_MAX_EXP__", "1024"), ("__DBL_MAX_10_EXP__", "308"),
            ("__DBL_MAX__", "1.79769313486231570814527423731704357e+308"),
            ("__DBL_MIN__", "2.22507385850720138309023271733240406e-308"),
            ("__DBL_EPSILON__", "2.22044604925031308084726333618164062e-16"),
            ("__DBL_DENORM_MIN__", "4.94065645841246544176568792868221372e-324"),
            // LDBL characteristics
            ("__LDBL_MANT_DIG__", "64"), ("__LDBL_DIG__", "18"),
            ("__LDBL_MIN_EXP__", "(-16381)"), ("__LDBL_MIN_10_EXP__", "(-4931)"),
            ("__LDBL_MAX_EXP__", "16384"), ("__LDBL_MAX_10_EXP__", "4932"),
            ("__LDBL_MAX__", "1.18973149535723176502e+4932L"),
            ("__LDBL_MIN__", "3.36210314311209350626e-4932L"),
            ("__LDBL_EPSILON__", "1.08420217248550443401e-19L"),
            ("__LDBL_DENORM_MIN__", "3.64519953188247460252840593361941982e-4951L"),
            ("__SIZEOF_LONG_DOUBLE__", "16"),
            // Float feature flags
            ("__FLT_HAS_INFINITY__", "1"), ("__FLT_HAS_QUIET_NAN__", "1"),
            ("__FLT_HAS_DENORM__", "1"),
            ("__DBL_HAS_INFINITY__", "1"), ("__DBL_HAS_QUIET_NAN__", "1"),
            ("__DBL_HAS_DENORM__", "1"),
            ("__LDBL_HAS_INFINITY__", "1"), ("__LDBL_HAS_QUIET_NAN__", "1"),
            ("__LDBL_HAS_DENORM__", "1"),
            ("__FLT_DECIMAL_DIG__", "9"), ("__DBL_DECIMAL_DIG__", "17"),
            ("__LDBL_DECIMAL_DIG__", "21"), ("__DECIMAL_DIG__", "21"),
            // GCC extensions
            ("__GNUC_VA_LIST", "1"), ("__extension__", ""),
            // NOTE: GNU keyword aliases (__inline__, __volatile__, __asm__, __const__,
            // __restrict__, __signed__, __typeof__) are handled as keyword tokens in
            // the lexer (token.rs), not as macros, because GCC treats them as reserved
            // keywords immune to #define redefinition.
            ("__alignof", "_Alignof"), ("__alignof__", "_Alignof"),
            // Named address spaces (Linux kernel): strip since we don't support them
            ("__seg_gs", ""), ("__seg_fs", ""),
            // __float128 -> long double (glibc compat)
            ("__float128", "long double"), ("__SIZEOF_FLOAT128__", "16"),
            // MSVC integer type specifiers
            ("__int8", "char"), ("__int16", "short"),
            ("__int32", "int"), ("__int64", "long long"),
            // ELF ABI
            ("__USER_LABEL_PREFIX__", ""),
            // GNU C attribute macros (strip)
            ("__LEAF", ""), ("__LEAF_ATTR", ""), ("__wur", ""),
            // Date/time
            ("__DATE__", "\"Jan  1 2025\""), ("__TIME__", "\"00:00:00\""),
            // GCC atomic lock-free macros
            ("__GCC_ATOMIC_BOOL_LOCK_FREE", "2"),
            ("__GCC_ATOMIC_CHAR_LOCK_FREE", "2"),
            ("__GCC_ATOMIC_CHAR16_T_LOCK_FREE", "2"),
            ("__GCC_ATOMIC_CHAR32_T_LOCK_FREE", "2"),
            ("__GCC_ATOMIC_WCHAR_T_LOCK_FREE", "2"),
            ("__GCC_ATOMIC_SHORT_LOCK_FREE", "2"),
            ("__GCC_ATOMIC_INT_LOCK_FREE", "2"),
            ("__GCC_ATOMIC_LONG_LOCK_FREE", "2"),
            ("__GCC_ATOMIC_LLONG_LOCK_FREE", "2"),
            ("__GCC_ATOMIC_POINTER_LOCK_FREE", "2"),
            // ELF/PIC
            ("__ELF__", "1"), ("__PIC__", "2"), ("__pic__", "2"),
        ];

        for &(name, body) in PREDEFINED_OBJECT_MACROS {
            self.define_simple_macro(name, body);
        }

        // Function-like predefined macros: (name, params, body)
        const PREDEFINED_FUNC_MACROS: &[(&str, &[&str], &str)] = &[
            ("__builtin_expect", &["exp", "c"], "(exp)"),
            ("__builtin_offsetof", &["type", "member"], "((unsigned long)&((type *)0)->member)"),
            ("__has_builtin", &["x"], "0"),
            ("__has_attribute", &["x"], "0"),
            ("__has_feature", &["x"], "0"),
            ("__has_include", &["x"], "1"),
            ("__has_include_next", &["x"], "0"),
            ("__has_extension", &["x"], "0"),
        ];

        for &(name, params, body) in PREDEFINED_FUNC_MACROS {
            self.macros.define(MacroDef {
                name: name.to_string(),
                is_function_like: true,
                params: params.iter().map(|s| s.to_string()).collect(),
                is_variadic: false,
                has_named_variadic: false,
                body: body.to_string(),
                is_predefined: true,
            });
        }
    }

    /// Helper to define a simple object-like macro.
    fn define_simple_macro(&mut self, name: &str, body: &str) {
        self.macros.define(MacroDef {
            name: name.to_string(),
            is_function_like: false,
            params: Vec::new(),
            is_variadic: false,
            has_named_variadic: false,
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
    pub(super) fn preprocess_included(&mut self, source: &str) -> String {
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
        let (source, line_map) = Self::strip_block_comments(&source);
        let mut output = String::with_capacity(source.len());

        // For included files, save and reset the conditional stack and line override
        let saved_conditionals = if is_include {
            Some(std::mem::replace(&mut self.conditionals, ConditionalStack::new()))
        } else {
            None
        };
        let saved_line_override = if is_include {
            self.line_override.take()
        } else {
            None
        };

        // Buffer for accumulating multi-line macro invocations
        let mut pending_line = String::new();
        let mut pending_newlines: usize = 0;

        for (line_num, line) in source.lines().enumerate() {
            let trimmed = line.trim();

            // Map output line number to original source line number using the
            // line_map from comment stripping (accounts for removed newlines in
            // block comments).
            let source_line_num = line_map.get(line_num).copied().unwrap_or(line_num);

            // Update __LINE__, accounting for any #line directive override
            let effective_line = if let Some((target_line, source_line_at_directive)) = self.line_override {
                // After #line N, __LINE__ = N + (current_source_line - source_line_of_directive)
                let offset = source_line_num.saturating_sub(source_line_at_directive);
                target_line + offset
            } else {
                source_line_num + 1
            };
            self.macros.define(MacroDef {
                name: "__LINE__".to_string(),
                is_function_like: false,
                params: Vec::new(),
                is_variadic: false,
                has_named_variadic: false,
                body: effective_line.to_string(),
                is_predefined: true,
            });

            // Directive handling: #if/#ifdef/#ifndef/#elif/#else/#endif must always
            // be processed regardless of pending multi-line accumulation. Other
            // directives (#include, #define, etc.) are only processed when there's
            // no pending line in included files.
            let is_directive = trimmed.starts_with('#');
            let is_conditional_directive = if is_directive {
                let after_hash = trimmed[1..].trim_start();
                after_hash.starts_with("if")
                    || after_hash.starts_with("elif")
                    || after_hash.starts_with("else")
                    || after_hash.starts_with("endif")
            } else {
                false
            };
            let process_directive = is_directive
                && (!is_include || pending_line.is_empty() || is_conditional_directive);

            // If we're in an included file with a pending line and hit a conditional
            // directive, flush the pending line first so it doesn't get lost.
            if process_directive && is_include && !pending_line.is_empty() && is_conditional_directive {
                let expanded = self.macros.expand_line(&pending_line);
                output.push_str(&expanded);
                for _ in 0..pending_newlines {
                    output.push('\n');
                }
                pending_line.clear();
                pending_newlines = 0;
            }

            if process_directive {
                let include_result = self.process_directive(trimmed, source_line_num + 1);
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

        // Restore conditional stack and line override for included files
        if let Some(saved) = saved_conditionals {
            self.conditionals = saved;
        }
        if is_include {
            self.line_override = saved_line_override;
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
                if !Self::has_unbalanced_parens(pending_line) || *pending_newlines > 200 {
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
                if Self::has_unbalanced_parens(pending_line) && *pending_newlines <= 200 {
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
            has_named_variadic: false,
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
            has_named_variadic: false,
            body: value.to_string(),
            is_predefined: false,
        });
    }

    /// Add an include search path for #include directives (-I flag).
    /// Adds regardless of whether the directory currently exists.
    pub fn add_include_path(&mut self, path: &str) {
        self.include_paths.push(PathBuf::from(path));
    }

    /// Process a force-included file (-include flag). This preprocesses the file content
    /// as if it were #include'd at the very beginning of the main source file.
    /// All #define directives in the file take effect, and the preprocessed output
    /// is discarded (macros/typedefs persist in the preprocessor state).
    pub fn preprocess_force_include(&mut self, content: &str, resolved_path: &str) {
        let resolved = PathBuf::from(resolved_path);

        // Check for #pragma once
        if self.pragma_once_files.contains(&resolved) {
            return;
        }

        // Push onto include stack
        self.include_stack.push(resolved.clone());

        // Save and set __FILE__
        let old_file = self.macros.get("__FILE__").map(|m| m.body.clone());
        self.macros.define(super::macro_defs::MacroDef {
            name: "__FILE__".to_string(),
            is_function_like: false,
            params: Vec::new(),
            is_variadic: false,
            has_named_variadic: false,
            body: format!("\"{}\"", resolved.display()),
            is_predefined: true,
        });

        // Preprocess the included content (output is discarded, but macros persist)
        let _output = self.preprocess_included(content);

        // Restore __FILE__
        if let Some(old) = old_file {
            self.macros.define(super::macro_defs::MacroDef {
                name: "__FILE__".to_string(),
                is_function_like: false,
                params: Vec::new(),
                is_variadic: false,
                has_named_variadic: false,
                body: old,
                is_predefined: true,
            });
        }

        // Pop include stack
        self.include_stack.pop();
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

    /// Strip C-style block comments (/* ... */) and C++ line comments (// ...).
    /// Returns the stripped source and a mapping from output line numbers to
    /// original source line numbers, for correct __LINE__ tracking.
    ///
    /// Block comments are replaced with a single space (per C11 5.1.1.2 phase 3),
    /// which avoids breaking preprocessor directives that have block comments
    /// between `#` and the directive keyword (e.g., `#/*...\n...*/if 1`).
    /// Uses raw byte operations to preserve UTF-8 sequences in string literals.
    fn strip_block_comments(source: &str) -> (String, Vec<usize>) {
        let mut result: Vec<u8> = Vec::with_capacity(source.len());
        let bytes = source.as_bytes();
        let len = bytes.len();
        let mut i = 0;
        // Track source line number (0-based) as we scan through input
        let mut src_line: usize = 0;
        // For each output line, record which source line it corresponds to
        let mut line_map: Vec<usize> = Vec::new();
        // Track the source line at the start of the current output line
        let mut current_output_line_src = 0usize;

        while i < len {
            match bytes[i] {
                b'"' | b'\'' => {
                    // Copy string/char literals verbatim (don't strip comments inside)
                    let old_i = i;
                    i = copy_literal_bytes_raw(bytes, i, bytes[i], &mut result);
                    // Count newlines in the literal for source line tracking
                    for &b in &bytes[old_i..i] {
                        if b == b'\n' {
                            src_line += 1;
                            // Record mapping for each output line
                            line_map.push(current_output_line_src);
                            current_output_line_src = src_line;
                        }
                    }
                }
                b'/' if i + 1 < len && bytes[i + 1] == b'*' => {
                    // Block comment - replace entire comment with a single space
                    i += 2;
                    result.push(b' ');
                    while i < len {
                        if i + 1 < len && bytes[i] == b'*' && bytes[i + 1] == b'/' {
                            i += 2;
                            break;
                        }
                        if bytes[i] == b'\n' {
                            src_line += 1;
                        }
                        i += 1;
                    }
                    // Don't emit newlines - the comment is fully replaced by one space
                }
                b'/' if i + 1 < len && bytes[i + 1] == b'/' => {
                    // Line comment - skip to end of line
                    i += 2;
                    while i < len && bytes[i] != b'\n' {
                        i += 1;
                    }
                }
                b'\n' => {
                    result.push(b'\n');
                    line_map.push(current_output_line_src);
                    src_line += 1;
                    current_output_line_src = src_line;
                    i += 1;
                }
                _ => {
                    result.push(bytes[i]);
                    i += 1;
                }
            }
        }
        // Record the last line
        line_map.push(current_output_line_src);

        // SAFETY: input was valid UTF-8, and we only removed/replaced ASCII characters
        // (comments with spaces/newlines), so result is still valid UTF-8
        let text = unsafe { String::from_utf8_unchecked(result) };
        (text, line_map)
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
    fn process_directive(&mut self, line: &str, line_num: usize) -> Option<String> {
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
                self.handle_line_directive(rest, line_num);
            }
            "" => {
                // Empty # directive (null directive), valid in C
            }
            _ => {
                // Handle GNU linemarker: # <digit-sequence> ["filename" [flags]]
                // This is equivalent to #line <digit-sequence> ["filename"]
                if keyword.chars().next().map_or(false, |c| c.is_ascii_digit()) {
                    let line_rest = format!("{} {}", keyword, rest);
                    self.handle_line_directive(&line_rest, line_num);
                }
                // Otherwise unknown directive, ignore silently
            }
        }

        None
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

    fn handle_line_directive(&mut self, rest: &str, source_line_num: usize) {
        // #line digit-sequence ["filename"]
        // The argument undergoes macro expansion first
        let expanded = self.macros.expand_line(rest);
        let expanded = expanded.trim();

        // Parse the line number (first token)
        let mut parts = expanded.split_whitespace();
        if let Some(line_str) = parts.next() {
            if let Ok(target_line) = line_str.parse::<usize>() {
                // source_line_num is 1-based (the line where #line appears)
                // The line AFTER #line should be target_line, so we record:
                // (target_line, source_line_num_0based_of_directive)
                // source_line_num is already 1-based from process_directive caller
                self.line_override = Some((target_line, source_line_num));

                // If there's a filename argument, update __FILE__
                if let Some(filename_str) = parts.next() {
                    let filename = filename_str.trim_matches('"');
                    if !filename.is_empty() {
                        self.macros.define(MacroDef {
                            name: "__FILE__".to_string(),
                            is_function_like: false,
                            params: Vec::new(),
                            is_variadic: false,
                            has_named_variadic: false,
                            body: format!("\"{}\"", filename),
                            is_predefined: true,
                        });
                    }
                }
            }
        }
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

        // Handle #pragma push_macro("name") / pop_macro("name")
        if let Some(push_content) = rest.strip_prefix("push_macro") {
            self.handle_pragma_push_macro(push_content.trim());
            return None;
        }
        if let Some(pop_content) = rest.strip_prefix("pop_macro") {
            self.handle_pragma_pop_macro(pop_content.trim());
            return None;
        }

        // Other pragmas (GCC, diagnostic, etc.) are silently ignored
        None
    }

    /// Handle #pragma push_macro("name") - save the current definition of macro.
    fn handle_pragma_push_macro(&mut self, content: &str) {
        if let Some(name) = Self::extract_pragma_macro_name(content) {
            let saved = self.macros.get(&name).cloned();
            self.macro_save_stack
                .entry(name)
                .or_insert_with(Vec::new)
                .push(saved);
        }
    }

    /// Handle #pragma pop_macro("name") - restore the previously saved definition.
    fn handle_pragma_pop_macro(&mut self, content: &str) {
        if let Some(name) = Self::extract_pragma_macro_name(content) {
            if let Some(stack) = self.macro_save_stack.get_mut(&name) {
                if let Some(saved) = stack.pop() {
                    match saved {
                        Some(def) => self.macros.define(def),
                        None => self.macros.undefine(&name),
                    }
                }
            }
        }
    }

    /// Extract macro name from pragma argument like ("name").
    fn extract_pragma_macro_name(content: &str) -> Option<String> {
        let content = content.trim();
        if !content.starts_with('(') {
            return None;
        }
        let inner = content.trim_start_matches('(').trim_end_matches(')').trim();
        // Strip quotes
        let name = inner.trim_matches('"');
        if name.is_empty() {
            return None;
        }
        Some(name.to_string())
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

