//! Preprocessor conditional expression evaluation.
//!
//! This module handles evaluation of preprocessor conditional expressions (`#if`, `#elif`),
//! including `defined()` operator resolution, `__has_builtin()` and `__has_attribute()` detection,
//! and replacing undefined identifiers with 0 per the C standard.
//!
//! All scanning operates on byte slices for performance (no Vec<char> allocation).

use super::preprocessor::Preprocessor;
use super::utils::{is_ident_start_byte, is_ident_cont_byte, bytes_to_str};

impl Preprocessor {
    /// Replace remaining identifiers (not keywords) with 0 in a #if expression.
    /// Per C standard, after macro expansion, undefined identifiers in #if evaluate to 0.
    pub(super) fn replace_remaining_idents_with_zero(&self, expr: &str) -> String {
        let mut result = String::new();
        let bytes = expr.as_bytes();
        let len = bytes.len();
        let mut i = 0;

        while i < len {
            // Skip character literals verbatim
            if bytes[i] == b'\'' {
                result.push(bytes[i] as char);
                i += 1;
                while i < len && bytes[i] != b'\'' {
                    if bytes[i] == b'\\' && i + 1 < len {
                        result.push(bytes[i] as char);
                        i += 1;
                        result.push(bytes[i] as char);
                        i += 1;
                    } else {
                        result.push(bytes[i] as char);
                        i += 1;
                    }
                }
                if i < len && bytes[i] == b'\'' {
                    result.push(bytes[i] as char);
                    i += 1;
                }
                continue;
            }
            // Skip string literals verbatim
            if bytes[i] == b'"' {
                result.push(bytes[i] as char);
                i += 1;
                while i < len && bytes[i] != b'"' {
                    if bytes[i] == b'\\' && i + 1 < len {
                        result.push(bytes[i] as char);
                        i += 1;
                        result.push(bytes[i] as char);
                        i += 1;
                    } else {
                        result.push(bytes[i] as char);
                        i += 1;
                    }
                }
                if i < len && bytes[i] == b'"' {
                    result.push(bytes[i] as char);
                    i += 1;
                }
                continue;
            }
            if bytes[i].is_ascii_digit() {
                // Skip entire number literal
                result.push(bytes[i] as char);
                i += 1;
                if i < len && bytes[i - 1] == b'0' && (bytes[i] == b'x' || bytes[i] == b'X' || bytes[i] == b'b' || bytes[i] == b'B') {
                    result.push(bytes[i] as char);
                    i += 1;
                }
                while i < len && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'.') {
                    result.push(bytes[i] as char);
                    i += 1;
                }
            } else if is_ident_start_byte(bytes[i]) {
                let start = i;
                i += 1;
                while i < len && is_ident_cont_byte(bytes[i]) {
                    i += 1;
                }
                let ident = bytes_to_str(bytes, start, i);
                if ident == "defined" || ident == "true" || ident == "false" {
                    result.push_str(ident);
                } else {
                    result.push('0');
                }
            } else {
                result.push(bytes[i] as char);
                i += 1;
            }
        }

        result
    }

    /// Replace `defined(X)`, `defined X`, `__has_builtin(X)`, `__has_attribute(X)`,
    /// `__has_feature(X)`, and `__has_extension(X)` with 0 or 1 in a #if expression.
    pub(super) fn resolve_defined_in_expr(&self, expr: &str) -> String {
        let mut result = String::new();
        let bytes = expr.as_bytes();
        let len = bytes.len();
        let mut i = 0;

        while i < len {
            if is_ident_start_byte(bytes[i]) {
                let start = i;
                i += 1;
                while i < len && is_ident_cont_byte(bytes[i]) {
                    i += 1;
                }
                let ident = bytes_to_str(bytes, start, i);

                if ident == "defined" {
                    while i < len && (bytes[i] == b' ' || bytes[i] == b'\t') {
                        i += 1;
                    }
                    let has_paren = if i < len && bytes[i] == b'(' {
                        i += 1;
                        while i < len && (bytes[i] == b' ' || bytes[i] == b'\t') {
                            i += 1;
                        }
                        true
                    } else {
                        false
                    };

                    let name_start = i;
                    while i < len && is_ident_cont_byte(bytes[i]) {
                        i += 1;
                    }
                    let name = bytes_to_str(bytes, name_start, i);

                    if has_paren {
                        while i < len && (bytes[i] == b' ' || bytes[i] == b'\t') {
                            i += 1;
                        }
                        if i < len && bytes[i] == b')' {
                            i += 1;
                        }
                    }

                    let is_def = self.macros.is_defined(name);
                    result.push_str(if is_def { "1" } else { "0" });
                } else if ident == "__has_builtin" {
                    let val = self.resolve_has_builtin_call_bytes(bytes, &mut i);
                    result.push_str(val);
                } else if ident == "__has_attribute" {
                    let val = self.resolve_has_attribute_call_bytes(bytes, &mut i);
                    result.push_str(val);
                } else if ident == "__has_feature" || ident == "__has_extension" {
                    self.skip_paren_arg_bytes(bytes, &mut i);
                    result.push_str("0");
                } else {
                    result.push_str(ident);
                }
                continue;
            }

            result.push(bytes[i] as char);
            i += 1;
        }

        result
    }

    /// Parse `(name)` after `__has_builtin` and return "1" or "0" (byte-oriented).
    fn resolve_has_builtin_call_bytes(&self, bytes: &[u8], i: &mut usize) -> &'static str {
        let len = bytes.len();
        while *i < len && (bytes[*i] == b' ' || bytes[*i] == b'\t') {
            *i += 1;
        }
        if *i >= len || bytes[*i] != b'(' {
            return "0";
        }
        *i += 1;
        while *i < len && (bytes[*i] == b' ' || bytes[*i] == b'\t') {
            *i += 1;
        }
        let start = *i;
        while *i < len && is_ident_cont_byte(bytes[*i]) {
            *i += 1;
        }
        let name = bytes_to_str(bytes, start, *i);
        while *i < len && bytes[*i] != b')' {
            *i += 1;
        }
        if *i < len {
            *i += 1;
        }
        if Self::is_supported_builtin(name) { "1" } else { "0" }
    }

    /// Parse `(name)` after `__has_attribute` and return "1" or "0" (byte-oriented).
    fn resolve_has_attribute_call_bytes(&self, bytes: &[u8], i: &mut usize) -> &'static str {
        let len = bytes.len();
        while *i < len && (bytes[*i] == b' ' || bytes[*i] == b'\t') {
            *i += 1;
        }
        if *i >= len || bytes[*i] != b'(' {
            return "0";
        }
        *i += 1;
        while *i < len && (bytes[*i] == b' ' || bytes[*i] == b'\t') {
            *i += 1;
        }
        let start = *i;
        while *i < len && is_ident_cont_byte(bytes[*i]) {
            *i += 1;
        }
        let name = bytes_to_str(bytes, start, *i);
        while *i < len && bytes[*i] != b')' {
            *i += 1;
        }
        if *i < len {
            *i += 1;
        }
        if Self::is_supported_attribute(name) { "1" } else { "0" }
    }

    /// Skip a parenthesized argument (byte-oriented).
    fn skip_paren_arg_bytes(&self, bytes: &[u8], i: &mut usize) {
        let len = bytes.len();
        while *i < len && (bytes[*i] == b' ' || bytes[*i] == b'\t') {
            *i += 1;
        }
        if *i < len && bytes[*i] == b'(' {
            *i += 1;
            let mut depth = 1;
            while *i < len && depth > 0 {
                if bytes[*i] == b'(' {
                    depth += 1;
                } else if bytes[*i] == b')' {
                    depth -= 1;
                }
                *i += 1;
            }
        }
    }

    /// Check if a builtin function name is supported by this compiler.
    pub(super) fn is_supported_builtin(name: &str) -> bool {
        matches!(name,
            // Byte swap builtins
            "__builtin_bswap16" | "__builtin_bswap32" | "__builtin_bswap64" |
            // Bit manipulation
            "__builtin_clz" | "__builtin_clzl" | "__builtin_clzll" |
            "__builtin_ctz" | "__builtin_ctzl" | "__builtin_ctzll" |
            "__builtin_clrsb" | "__builtin_clrsbl" | "__builtin_clrsbll" |
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
    pub(super) fn is_supported_attribute(name: &str) -> bool {
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
