//! Handles `#include` and `#include_next` directive resolution, file path lookup,
//! and synthetic header declaration injection.

use std::path::{Path, PathBuf};

use super::macro_defs::{MacroDef, parse_define};
use super::preprocessor::Preprocessor;

/// Maximum recursive inclusion depth, matching GCC's default of 200.
/// Prevents infinite inclusion loops in files without `#pragma once`.
const MAX_INCLUDE_DEPTH: usize = 200;

/// Read a C source file, tolerating non-UTF-8 content.
/// Valid UTF-8 files are returned as-is. Non-UTF-8 bytes are encoded as PUA
/// code points which the lexer decodes back to raw bytes.
fn read_c_source_file(path: &Path) -> std::io::Result<String> {
    let bytes = std::fs::read(path)?;
    Ok(crate::common::encoding::bytes_to_string(bytes))
}

/// Normalize a computed include path by collapsing anti-paste spaces around '/'.
///
/// During macro expansion, the preprocessor inserts a space between adjacent '/'
/// characters to prevent "//" from being lexed as a line comment start (see
/// `would_paste_tokens` in macro_defs.rs). This is correct for general code but
/// corrupts file paths in computed `#include` directives. For example:
///
///   #define PATH asm/
///   #define INCLUDE(f) __stringify(PATH/f.h)
///   #include INCLUDE(msr-trace)
///
/// produces "asm/ /msr-trace.h" instead of "asm//msr-trace.h". Collapsing
/// "/ /" back to "//" fixes the path (the filesystem treats "//" as "/").
fn normalize_include_path(path: String) -> String {
    if path.contains("/ /") {
        path.replace("/ /", "//")
    } else {
        path
    }
}

impl Preprocessor {
    /// Handle #include directive. Returns the preprocessed content of the included file,
    /// or None if the include couldn't be resolved (falls back to old behavior).
    pub(super) fn handle_include(&mut self, path: &str) -> Option<String> {
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

        let include_path = normalize_include_path(include_path);

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

            // Check for excessive recursive inclusion.
            // Files WITHOUT #pragma once are allowed to be re-included with different
            // macro definitions active (e.g., TCC's x86_64-gen.c includes itself via
            // tcc.h with TARGET_DEFS_ONLY defined). Only block when nesting is excessive.
            {
                let depth = self.include_stack.iter().filter(|p| *p == &resolved_path).count();
                if depth >= MAX_INCLUDE_DEPTH {
                    return Some(String::new());
                }
            }

            // Read the file
            match read_c_source_file(&resolved_path) {
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
                        has_named_variadic: false,
                        body: format!("\"{}\"", resolved_path.display()),
                        is_predefined: true,
                    });

                    // Emit line marker for entering the included file
                    let mut result = format!("# 1 \"{}\"\n", resolved_path.display());

                    // Preprocess the included content
                    result.push_str(&self.preprocess_included(&content));

                    // Restore __FILE__
                    if let Some(old) = old_file {
                        self.macros.define(MacroDef {
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
    pub(super) fn handle_include_next(&mut self, path: &str) -> Option<String> {
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

        let include_path = normalize_include_path(include_path);

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

            // Check for excessive recursive inclusion
            {
                let depth = self.include_stack.iter().filter(|p| *p == &resolved_path).count();
                if depth >= MAX_INCLUDE_DEPTH {
                    return Some(String::new());
                }
            }

            // Read and preprocess the file
            match read_c_source_file(&resolved_path) {
                Ok(content) => {
                    self.include_stack.push(resolved_path.clone());

                    let old_file = self.macros.get("__FILE__").map(|m| m.body.clone());
                    self.macros.define(MacroDef {
                        name: "__FILE__".to_string(),
                        is_function_like: false,
                        params: Vec::new(),
                        is_variadic: false,
                        has_named_variadic: false,
                        body: format!("\"{}\"", resolved_path.display()),
                        is_predefined: true,
                    });

                    // Emit line marker for entering the included file
                    let mut result = format!("# 1 \"{}\"\n", resolved_path.display());

                    result.push_str(&self.preprocess_included(&content));

                    if let Some(old) = old_file {
                        self.macros.define(MacroDef {
                            name: "__FILE__".to_string(),
                            is_function_like: false,
                            params: Vec::new(),
                            is_variadic: false,
                            has_named_variadic: false,
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
    pub(super) fn resolve_include_next_path(&self, include_path: &str, current_file_dir: Option<&PathBuf>) -> Option<PathBuf> {
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

        // If we didn't find the current directory in search paths, the file was
        // included via relative-to-source resolution. For #include_next semantics,
        // search all include paths but skip any candidate that resolves to the same
        // file as the one currently being processed (to avoid infinite self-inclusion).
        if !found_current {
            let current_file_canon = current_file_dir
                .and_then(|d| {
                    let current_candidate = d.join(include_path);
                    std::fs::canonicalize(&current_candidate).ok()
                });
            for search_path in &all_paths {
                let candidate = search_path.join(include_path);
                if candidate.is_file() {
                    let candidate_canon = std::fs::canonicalize(&candidate).ok();
                    // Skip if this resolves to the same file we're currently in
                    if let (Some(ref cur), Some(ref cand)) = (&current_file_canon, &candidate_canon) {
                        if cur == cand {
                            continue;
                        }
                    }
                    return candidate_canon.or(Some(candidate));
                }
            }
        }

        None
    }

    /// Resolve an include path to an actual file path.
    /// For "file.h": search current dir, then -I paths, then system paths.
    /// For <file.h>: search -I paths, then system paths.
    pub(super) fn resolve_include_path(&self, include_path: &str, is_system: bool) -> Option<PathBuf> {
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
    pub(super) fn inject_header_declarations(&mut self, header: &str) {
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
                    has_named_variadic: false,
                    body: "__builtin_va_start(ap,last)".to_string(),
                    is_predefined: true,
                });
                self.macros.define(MacroDef {
                    name: "va_end".to_string(),
                    is_function_like: true,
                    params: vec!["ap".to_string()],
                    is_variadic: false,
                    has_named_variadic: false,
                    body: "__builtin_va_end(ap)".to_string(),
                    is_predefined: true,
                });
                self.macros.define(MacroDef {
                    name: "va_copy".to_string(),
                    is_function_like: true,
                    params: vec!["dest".to_string(), "src".to_string()],
                    is_variadic: false,
                    has_named_variadic: false,
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
                    has_named_variadic: false,
                    body: "__builtin_va_arg(ap,type)".to_string(),
                    is_predefined: true,
                });
                // Also define __gnuc_va_list as a typedef
                self.pending_injections.push("typedef __builtin_va_list __gnuc_va_list;\n".to_string());
            }
            _ => {}
        }
    }
}
