/// Macro definitions and expansion logic for the C preprocessor.
///
/// Supports:
/// - Object-like macros: `#define FOO value`
/// - Function-like macros: `#define MAX(a,b) ((a)>(b)?(a):(b))`
/// - Variadic macros: `#define LOG(fmt, ...) printf(fmt, __VA_ARGS__)`
/// - Stringification: `#param`
/// - Token pasting: `a ## b`
///
/// Performance: All scanning operates on byte slices (`&[u8]`) to avoid
/// the overhead of `Vec<char>` allocation. Since C preprocessor tokens are
/// ASCII, this is safe and correct. UTF-8 multi-byte sequences in string/char
/// literals are copied verbatim without interpretation.

use std::cell::Cell;

use crate::common::fx_hash::{FxHashMap, FxHashSet};

use super::utils::{
    is_ident_start_byte, is_ident_cont_byte, bytes_to_str,
    skip_literal_bytes, copy_literal_bytes_to_string,
};

/// Check if two adjacent bytes would form an unintended multi-character token
/// when concatenated during macro expansion. Returns true if a separating space
/// is needed to prevent accidental token pasting.
fn would_paste_tokens(last: u8, first: u8) -> bool {
    match (last, first) {
        // -- or -=  or -> from separate sources
        (b'-', b'-') | (b'-', b'=') | (b'-', b'>') => true,
        // ++ or +=
        (b'+', b'+') | (b'+', b'=') => true,
        // << or <= or <:
        (b'<', b'<') | (b'<', b'=') | (b'<', b':') | (b'<', b'%') => true,
        // >> or >=
        (b'>', b'>') | (b'>', b'=') => true,
        // == or ending = followed by =
        (b'=', b'=') => true,
        // != &= |= *= /= ^= %=
        (b'!', b'=') | (b'&', b'=') | (b'|', b'=') | (b'*', b'=') |
        (b'/', b'=') | (b'^', b'=') | (b'%', b'=') => true,
        // && ||
        (b'&', b'&') | (b'|', b'|') => true,
        // ## (token paste)
        (b'#', b'#') => true,
        // / followed by * or / (comment start)
        (b'/', b'*') | (b'/', b'/') => true,
        // . followed by digit (e.g., prevent "1." + "5" pasting weirdly)
        (b'.', b'0'..=b'9') => true,
        // Identifier/number continuation: letter/digit/_ followed by letter/digit/_
        (a, b) if is_ident_cont_byte(a) && is_ident_cont_byte(b) => true,
        _ => false,
    }
}

/// Represents a macro definition.
#[derive(Debug, Clone)]
pub struct MacroDef {
    /// Name of the macro
    pub name: String,
    /// Whether this is a function-like macro
    pub is_function_like: bool,
    /// Parameters for function-like macros
    pub params: Vec<String>,
    /// Whether the macro is variadic (last param is ...)
    pub is_variadic: bool,
    /// Whether the variadic is a named parameter (e.g., `args...` vs `...`).
    /// When true, the last entry in `params` is the variadic param name.
    /// When false, variadic args are accessed via `__VA_ARGS__`.
    pub has_named_variadic: bool,
    /// The replacement body (as raw text)
    pub body: String,
    /// Whether this is a predefined macro (cannot be #undef'd in strict mode)
    pub is_predefined: bool,
}

/// Marker byte used to "blue paint" tokens that were suppressed due to
/// self-referential macro expansion (C11 §6.10.3.4).  Prefixed to
/// identifiers during rescanning so they are never re-expanded, then
/// stripped from the final output in `expand_line()`.
const BLUE_PAINT_MARKER: u8 = 0x01;

/// Stores all macro definitions and handles expansion.
#[derive(Debug, Clone)]
pub struct MacroTable {
    macros: FxHashMap<String, MacroDef>,
    /// Counter for the __COUNTER__ built-in macro. Increments on each expansion.
    counter: Cell<usize>,
    /// Cached __LINE__ value. Updated by set_line(), expanded specially in expand_text.
    line_value: Cell<usize>,
}

impl MacroTable {
    pub fn new() -> Self {
        Self {
            macros: FxHashMap::default(),
            counter: Cell::new(0),
            line_value: Cell::new(1),
        }
    }

    /// Define a new macro.
    pub fn define(&mut self, def: MacroDef) {
        self.macros.insert(def.name.clone(), def);
    }

    /// Undefine a macro.
    pub fn undefine(&mut self, name: &str) {
        self.macros.remove(name);
    }

    /// Check if a macro is defined.
    pub fn is_defined(&self, name: &str) -> bool {
        name == "__COUNTER__" || name == "__LINE__" || self.macros.contains_key(name)
    }

    /// Get a macro definition.
    pub fn get(&self, name: &str) -> Option<&MacroDef> {
        self.macros.get(name)
    }

    /// Set the current __LINE__ value without allocating a MacroDef.
    /// This avoids per-line allocation of MacroDef { name: "__LINE__", ... }.
    pub fn set_line(&self, line: usize) {
        self.line_value.set(line);
    }

    /// Expand macros in a line of text.
    /// Returns the expanded text.
    pub fn expand_line(&self, line: &str) -> String {
        let mut expanding = FxHashSet::default();
        let result = self.expand_text(line, &mut expanding);
        // Strip blue-paint marker bytes (0x01) from the final output.
        // These markers prevent re-expansion of self-referential macro tokens
        // per C11 §6.10.3.4 but must not appear in the final result.
        if result.as_bytes().contains(&BLUE_PAINT_MARKER) {
            result.replace(BLUE_PAINT_MARKER as char, "")
        } else {
            result
        }
    }

    /// Recursively expand macros in text, tracking which macros are
    /// currently being expanded to prevent infinite recursion.
    ///
    /// Operates on bytes for performance: avoids allocating Vec<char>.
    fn expand_text(&self, text: &str, expanding: &mut FxHashSet<String>) -> String {
        let mut result = String::with_capacity(text.len());
        let bytes = text.as_bytes();
        let len = bytes.len();
        let mut i = 0;

        while i < len {
            let b = bytes[i];

            // Skip string and char literals (copy them verbatim)
            if b == b'"' || b == b'\'' {
                i = copy_literal_bytes_to_string(bytes, i, b, &mut result);
                continue;
            }

            // Skip blue-painted identifiers: a \x01 marker means the
            // following identifier was suppressed by the blue-paint rule
            // and must be copied verbatim (never re-expanded).
            if b == BLUE_PAINT_MARKER {
                result.push(BLUE_PAINT_MARKER as char);
                i += 1;
                // Copy the identifier that follows
                while i < len && is_ident_cont_byte(bytes[i]) {
                    result.push(bytes[i] as char);
                    i += 1;
                }
                continue;
            }

            // Look for identifiers
            if is_ident_start_byte(b) {
                let start = i;
                i += 1;
                while i < len && is_ident_cont_byte(bytes[i]) {
                    i += 1;
                }
                let ident = bytes_to_str(bytes, start, i);

                // Check if this identifier is part of a pp-number (e.g., 1.I, 15.IF, 0x1p2).
                let is_ppnumber_suffix = {
                    let result_bytes = result.as_bytes();
                    let rlen = result_bytes.len();
                    if rlen == 0 {
                        false
                    } else {
                        let prev = result_bytes[rlen - 1];
                        if prev.is_ascii_digit() {
                            true
                        } else if prev == b'.' && rlen >= 2 && result_bytes[rlen - 2].is_ascii_digit() {
                            true
                        } else if (prev == b'+' || prev == b'-') && rlen >= 3
                            && matches!(result_bytes[rlen - 2], b'e' | b'E' | b'p' | b'P') {
                            is_ppnumber_context(result_bytes, rlen - 3)
                        } else {
                            false
                        }
                    }
                };
                if is_ppnumber_suffix {
                    result.push_str(ident);
                    continue;
                }

                // Handle _Pragma("string") operator (C99 §6.10.9).
                if ident == "_Pragma" {
                    let mut j = i;
                    while j < len && bytes[j].is_ascii_whitespace() {
                        j += 1;
                    }
                    if j < len && bytes[j] == b'(' {
                        let mut depth = 1;
                        j += 1;
                        while j < len && depth > 0 {
                            if bytes[j] == b'(' {
                                depth += 1;
                            } else if bytes[j] == b')' {
                                depth -= 1;
                            } else if bytes[j] == b'"' || bytes[j] == b'\'' {
                                j = skip_literal_bytes(bytes, j, bytes[j]);
                                continue;
                            }
                            j += 1;
                        }
                        i = j;
                        continue;
                    }
                    result.push_str(ident);
                    continue;
                }

                // Handle __COUNTER__ built-in
                if ident == "__COUNTER__" {
                    let val = self.counter.get();
                    self.counter.set(val + 1);
                    // Format number without allocating a String on the heap for small numbers
                    let mut buf = itoa::Buffer::new();
                    result.push_str(buf.format(val));
                    continue;
                }

                // Handle __LINE__ built-in (avoids hash lookup)
                if ident == "__LINE__" {
                    let val = self.line_value.get();
                    let mut buf = itoa::Buffer::new();
                    result.push_str(buf.format(val));
                    continue;
                }

                // Check if this is a macro that's not currently being expanded
                if !expanding.contains(ident) {
                    if let Some(mac) = self.macros.get(ident) {
                        if mac.is_function_like {
                            // Need to find opening paren
                            let mut j = i;
                            while j < len && bytes[j].is_ascii_whitespace() {
                                j += 1;
                            }
                            if j < len && bytes[j] == b'(' {
                                let (args, end_pos) = self.parse_macro_args(bytes, j);
                                i = end_pos;
                                let mut expanded = self.expand_function_macro(mac, &args, expanding);

                                // After expansion, check if the result ends with a function-like
                                // macro name and the remaining source starts with '('.
                                loop {
                                    let trailing = extract_trailing_ident(&expanded);
                                    if let Some(ref trail_ident) = trailing {
                                        if !expanding.contains(trail_ident.as_str()) {
                                            if let Some(trail_mac) = self.macros.get(trail_ident.as_str()) {
                                                if trail_mac.is_function_like {
                                                    let mut k = i;
                                                    while k < len && bytes[k].is_ascii_whitespace() {
                                                        k += 1;
                                                    }
                                                    if k < len && bytes[k] == b'(' {
                                                        let (trail_args, trail_end) = self.parse_macro_args(bytes, k);
                                                        i = trail_end;
                                                        let trail_mac_clone = trail_mac.clone();
                                                        let trimmed_len = expanded.trim_end().len();
                                                        let prefix_len = trimmed_len - trail_ident.len();
                                                        expanded.truncate(prefix_len);
                                                        let trail_expanded = self.expand_function_macro(&trail_mac_clone, &trail_args, expanding);
                                                        expanded.push_str(&trail_expanded);
                                                        continue;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    break;
                                }

                                if !expanded.is_empty() {
                                    if !result.is_empty() && !result.ends_with(' ') && !result.ends_with('\t') && !result.ends_with('\n') {
                                        result.push(' ');
                                    }
                                    result.push_str(&expanded);
                                }
                                continue;
                            } else {
                                result.push_str(ident);
                                continue;
                            }
                        } else {
                            // Object-like macro
                            expanding.insert(ident.to_string());
                            let expanded = self.expand_text(&mac.body, expanding);
                            expanding.remove(ident);

                            // Check if the expansion result is a function-like macro name
                            let expanded_trimmed = expanded.trim();
                            if !expanded_trimmed.is_empty() {
                                let et_bytes = expanded_trimmed.as_bytes();
                                if is_ident_start_byte(et_bytes[0])
                                    && et_bytes.iter().all(|&b| is_ident_cont_byte(b))
                                    && !expanding.contains(expanded_trimmed)
                                {
                                    if let Some(target_mac) = self.macros.get(expanded_trimmed) {
                                        if target_mac.is_function_like {
                                            let mut j = i;
                                            while j < len && bytes[j].is_ascii_whitespace() {
                                                j += 1;
                                            }
                                            if j < len && bytes[j] == b'(' {
                                                let (args, end_pos) = self.parse_macro_args(bytes, j);
                                                i = end_pos;
                                                let target_mac_clone = target_mac.clone();
                                                let func_expanded = self.expand_function_macro(&target_mac_clone, &args, expanding);
                                                result.push_str(&func_expanded);
                                                continue;
                                            }
                                        }
                                    }
                                }
                            }

                            // Insert space to prevent accidental token pasting
                            if !expanded.is_empty() && !result.is_empty() {
                                let last = result.as_bytes()[result.len() - 1];
                                let first = expanded.as_bytes()[0];
                                if would_paste_tokens(last, first) {
                                    result.push(' ');
                                }
                            }
                            result.push_str(&expanded);
                            continue;
                        }
                    }
                }

                // Not a macro, or already expanding (blue-painted).
                // If the identifier is being suppressed because it's in
                // the expanding set, prefix it with the blue-paint marker
                // so it won't be re-expanded during subsequent rescans.
                if expanding.contains(ident) {
                    result.push(BLUE_PAINT_MARKER as char);
                }
                result.push_str(ident);
                continue;
            }

            // Skip C-style comments
            if b == b'/' && i + 1 < len && bytes[i + 1] == b'*' {
                result.push('/');
                result.push('*');
                i += 2;
                while i + 1 < len && !(bytes[i] == b'*' && bytes[i + 1] == b'/') {
                    // SAFETY: We're inside a comment, just push raw bytes.
                    // Comments can only contain ASCII or UTF-8 that we copy verbatim.
                    result.push(bytes[i] as char);
                    i += 1;
                }
                if i + 1 < len {
                    result.push('*');
                    result.push('/');
                    i += 2;
                }
                continue;
            }

            // Skip line comments
            if b == b'/' && i + 1 < len && bytes[i + 1] == b'/' {
                while i < len && bytes[i] != b'\n' {
                    result.push(bytes[i] as char);
                    i += 1;
                }
                continue;
            }

            // Skip numeric literals (pp-numbers)
            if b.is_ascii_digit() || (b == b'.' && i + 1 < len && bytes[i + 1].is_ascii_digit()) {
                while i < len {
                    if bytes[i].is_ascii_alphanumeric() || bytes[i] == b'.' || bytes[i] == b'_' {
                        result.push(bytes[i] as char);
                        i += 1;
                    } else if (bytes[i] == b'+' || bytes[i] == b'-')
                        && i > 0
                        && matches!(bytes[i - 1], b'e' | b'E' | b'p' | b'P')
                    {
                        result.push(bytes[i] as char);
                        i += 1;
                    } else {
                        break;
                    }
                }
                continue;
            }

            // Regular character - for ASCII just push directly
            if b < 0x80 {
                result.push(b as char);
                i += 1;
            } else {
                // Multi-byte UTF-8: copy the full character
                let ch = text[i..].chars().next().unwrap();
                result.push(ch);
                i += ch.len_utf8();
            }
        }

        result
    }

    /// Parse function-like macro arguments from bytes starting at the opening paren.
    /// Returns (args, position after closing paren).
    fn parse_macro_args(&self, bytes: &[u8], start: usize) -> (Vec<String>, usize) {
        let len = bytes.len();
        let mut i = start + 1; // skip '('
        let mut args = Vec::new();
        let mut current_arg = String::new();
        let mut paren_depth = 0;

        while i < len {
            match bytes[i] {
                b'(' => {
                    paren_depth += 1;
                    current_arg.push('(');
                    i += 1;
                }
                b')' => {
                    if paren_depth == 0 {
                        let trimmed = current_arg.trim().to_string();
                        if !trimmed.is_empty() || !args.is_empty() {
                            args.push(trimmed);
                        }
                        return (args, i + 1);
                    } else {
                        paren_depth -= 1;
                        current_arg.push(')');
                        i += 1;
                    }
                }
                b',' if paren_depth == 0 => {
                    args.push(current_arg.trim().to_string());
                    current_arg = String::new();
                    i += 1;
                }
                b'"' | b'\'' => {
                    i = copy_literal_bytes_to_string(bytes, i, bytes[i], &mut current_arg);
                }
                b if b < 0x80 => {
                    current_arg.push(b as char);
                    i += 1;
                }
                _ => {
                    // Multi-byte UTF-8: reconstruct string from raw bytes
                    // SAFETY: the source text is valid UTF-8
                    let s = unsafe { std::str::from_utf8_unchecked(&bytes[i..]) };
                    let ch = s.chars().next().unwrap();
                    current_arg.push(ch);
                    i += ch.len_utf8();
                }
            }
        }

        // Unterminated - return what we have
        let trimmed = current_arg.trim().to_string();
        if !trimmed.is_empty() || !args.is_empty() {
            args.push(trimmed);
        }
        (args, i)
    }

    /// Expand a function-like macro with given arguments.
    ///
    /// Per C11 §6.10.3.1: Arguments are fully macro-expanded before substitution
    /// into the body, EXCEPT when the parameter is used with # (stringify) or
    /// ## (token paste). After substitution, the result is rescanned with the
    /// current macro name suppressed to prevent infinite recursion.
    fn expand_function_macro(
        &self,
        mac: &MacroDef,
        args: &[String],
        expanding: &mut FxHashSet<String>,
    ) -> String {
        // Step 1: Determine which parameters are used with # or ## (these get raw args)
        let paste_params = self.find_paste_and_stringify_params(&mac.body, &mac.params, mac.is_variadic);

        // Step 2: Prescan - expand arguments that are NOT used with # or ##
        let expanded_args: Vec<String> = args.iter().enumerate().map(|(idx, arg)| {
            if idx < mac.params.len() && paste_params.contains(&idx) {
                arg.clone()
            } else {
                self.expand_text(arg, expanding)
            }
        }).collect();

        let mut body = mac.body.clone();

        // Step 3: Handle stringification (#param) and token pasting (##)
        body = self.handle_stringify_and_paste(&body, &mac.params, args, mac.is_variadic, mac.has_named_variadic);

        // Step 4: Substitute parameters with expanded arguments
        body = self.substitute_params(&body, &mac.params, &expanded_args, mac.is_variadic, mac.has_named_variadic);

        // Step 5: Rescan with the current macro name suppressed
        expanding.insert(mac.name.clone());
        let result = self.expand_text(&body, expanding);
        expanding.remove(&mac.name);
        result
    }

    /// Find parameter indices that are used with # (stringify) or ## (token paste).
    fn find_paste_and_stringify_params(&self, body: &str, params: &[String], is_variadic: bool) -> FxHashSet<usize> {
        let mut result = FxHashSet::default();
        let bytes = body.as_bytes();
        let len = bytes.len();
        let mut i = 0;

        while i < len {
            if bytes[i] == b'#' && i + 1 < len && bytes[i + 1] == b'#' {
                // Token paste: ## - mark both the token before and after
                let mut j = i;
                while j > 0 && (bytes[j - 1] == b' ' || bytes[j - 1] == b'\t') {
                    j -= 1;
                }
                let end = j;
                while j > 0 && is_ident_cont_byte(bytes[j - 1]) {
                    j -= 1;
                }
                if j < end {
                    let ident = bytes_to_str(bytes, j, end);
                    if let Some(idx) = params.iter().position(|p| p == ident) {
                        result.insert(idx);
                    }
                    if ident == "__VA_ARGS__" && is_variadic {
                        for idx in params.len()..100 {
                            result.insert(idx);
                        }
                    }
                }

                i += 2;
                while i < len && (bytes[i] == b' ' || bytes[i] == b'\t') {
                    i += 1;
                }
                if i < len && is_ident_start_byte(bytes[i]) {
                    let start = i;
                    while i < len && is_ident_cont_byte(bytes[i]) {
                        i += 1;
                    }
                    let ident = bytes_to_str(bytes, start, i);
                    if let Some(idx) = params.iter().position(|p| p == ident) {
                        result.insert(idx);
                    }
                    if ident == "__VA_ARGS__" && is_variadic {
                        for idx in params.len()..100 {
                            result.insert(idx);
                        }
                    }
                }
                continue;
            }

            if bytes[i] == b'#' && i + 1 < len && bytes[i + 1] != b'#' {
                i += 1;
                while i < len && (bytes[i] == b' ' || bytes[i] == b'\t') {
                    i += 1;
                }
                if i < len && is_ident_start_byte(bytes[i]) {
                    let start = i;
                    while i < len && is_ident_cont_byte(bytes[i]) {
                        i += 1;
                    }
                    let ident = bytes_to_str(bytes, start, i);
                    if let Some(idx) = params.iter().position(|p| p == ident) {
                        result.insert(idx);
                    }
                    if ident == "__VA_ARGS__" && is_variadic {
                        for idx in params.len()..100 {
                            result.insert(idx);
                        }
                    }
                }
                continue;
            }

            if bytes[i] == b'"' || bytes[i] == b'\'' {
                i = skip_literal_bytes(bytes, i, bytes[i]);
                continue;
            }

            i += 1;
        }

        result
    }

    /// Handle # (stringify) and ## (token paste) operators.
    fn handle_stringify_and_paste(
        &self,
        body: &str,
        params: &[String],
        args: &[String],
        is_variadic: bool,
        has_named_variadic: bool,
    ) -> String {
        let bytes = body.as_bytes();
        let len = bytes.len();

        // Short-circuit: if body contains no '#', nothing to do
        if !body.contains('#') {
            return body.to_string();
        }

        let mut result = String::new();
        let mut i = 0;

        while i < len {
            if bytes[i] == b'#' && i + 1 < len && bytes[i + 1] == b'#' {
                // Token paste operator ##
                while result.ends_with(' ') || result.ends_with('\t') {
                    result.pop();
                }

                let left_token = extract_trailing_ident(&result);
                if let Some(ref left_ident) = left_token {
                    if left_ident == "__VA_ARGS__" && is_variadic {
                        let va_args = self.get_va_args(params, args);
                        let trim_len = result.len() - "__VA_ARGS__".len();
                        result.truncate(trim_len);
                        if va_args.is_empty() {
                            while result.ends_with(' ') || result.ends_with('\t') {
                                result.pop();
                            }
                            if result.ends_with(',') {
                                result.pop();
                            }
                        } else {
                            result.push_str(&va_args);
                        }
                    } else if let Some(idx) = params.iter().position(|p| p == left_ident.as_str()) {
                        let trim_len = result.len() - left_ident.len();
                        result.truncate(trim_len);
                        let arg = args.get(idx).map(|s| s.as_str()).unwrap_or("");
                        result.push_str(arg);
                    }
                }

                i += 2; // skip ##

                while i < len && (bytes[i] == b' ' || bytes[i] == b'\t') {
                    i += 1;
                }

                if i < len && is_ident_start_byte(bytes[i]) {
                    let start = i;
                    while i < len && is_ident_cont_byte(bytes[i]) {
                        i += 1;
                    }
                    let right_ident = bytes_to_str(bytes, start, i);

                    if right_ident == "__VA_ARGS__" && is_variadic {
                        let va_args = self.get_va_args(params, args);
                        if va_args.is_empty() {
                            while result.ends_with(' ') || result.ends_with('\t') {
                                result.pop();
                            }
                            if result.ends_with(',') {
                                result.pop();
                            }
                        } else {
                            result.push_str(&va_args);
                        }
                    } else if let Some(idx) = params.iter().position(|p| p == right_ident) {
                        let is_named_variadic_param = is_variadic && has_named_variadic && idx == params.len() - 1;
                        if is_named_variadic_param {
                            let va_args = self.get_named_va_args(idx, args);
                            if va_args.is_empty() {
                                while result.ends_with(' ') || result.ends_with('\t') {
                                    result.pop();
                                }
                                if result.ends_with(',') {
                                    result.pop();
                                }
                            } else {
                                result.push_str(&va_args);
                            }
                        } else {
                            let arg = args.get(idx).map(|s| s.as_str()).unwrap_or("");
                            if arg.is_empty() {
                                if i < len && !result.is_empty() {
                                    result.push(' ');
                                }
                            } else {
                                result.push_str(arg);
                            }
                        }
                    } else {
                        result.push_str(right_ident);
                    }
                } else if i < len {
                    result.push(bytes[i] as char);
                    i += 1;
                }
                continue;
            }

            if bytes[i] == b'#' && i + 1 < len && bytes[i + 1] != b'#' {
                // Stringification operator #
                i += 1;
                while i < len && (bytes[i] == b' ' || bytes[i] == b'\t') {
                    i += 1;
                }
                if i < len && is_ident_start_byte(bytes[i]) {
                    let start = i;
                    while i < len && is_ident_cont_byte(bytes[i]) {
                        i += 1;
                    }
                    let param_name = bytes_to_str(bytes, start, i);

                    if param_name == "__VA_ARGS__" && is_variadic {
                        let va_args = self.get_va_args(params, args);
                        result.push('"');
                        result.push_str(&stringify_arg(&va_args));
                        result.push('"');
                    } else if let Some(idx) = params.iter().position(|p| p == param_name) {
                        let arg_str = if is_variadic && has_named_variadic && idx == params.len() - 1 {
                            self.get_named_va_args(idx, args)
                        } else {
                            args.get(idx).map(|s| s.to_string()).unwrap_or_default()
                        };
                        result.push('"');
                        result.push_str(&stringify_arg(&arg_str));
                        result.push('"');
                    } else {
                        result.push('#');
                        result.push_str(param_name);
                    }
                    continue;
                } else {
                    result.push('#');
                    continue;
                }
            }

            // Regular byte
            if bytes[i] < 0x80 {
                result.push(bytes[i] as char);
            } else {
                let s = unsafe { std::str::from_utf8_unchecked(&bytes[i..]) };
                let ch = s.chars().next().unwrap();
                result.push(ch);
                i += ch.len_utf8();
                continue;
            }
            i += 1;
        }

        result
    }

    /// Substitute parameter names with argument values in macro body.
    fn substitute_params(
        &self,
        body: &str,
        params: &[String],
        args: &[String],
        is_variadic: bool,
        has_named_variadic: bool,
    ) -> String {
        let mut result = String::new();
        let bytes = body.as_bytes();
        let len = bytes.len();
        let mut i = 0;

        while i < len {
            if bytes[i] == b'"' || bytes[i] == b'\'' {
                i = copy_literal_bytes_to_string(bytes, i, bytes[i], &mut result);
                continue;
            }

            if is_ident_start_byte(bytes[i]) {
                let start = i;
                i += 1;
                while i < len && is_ident_cont_byte(bytes[i]) {
                    i += 1;
                }
                let ident = bytes_to_str(bytes, start, i);

                if ident == "__VA_ARGS__" && is_variadic {
                    let va_args = self.get_va_args(params, args);
                    result.push_str(&va_args);
                } else if let Some(idx) = params.iter().position(|p| p == ident) {
                    if is_variadic && has_named_variadic && idx == params.len() - 1 {
                        let va_args = self.get_named_va_args(idx, args);
                        result.push_str(&va_args);
                    } else {
                        let arg = args.get(idx).map(|s| s.as_str()).unwrap_or("");
                        result.push_str(arg);
                    }
                } else {
                    result.push_str(ident);
                }
                continue;
            }

            if bytes[i] < 0x80 {
                result.push(bytes[i] as char);
            } else {
                let s = unsafe { std::str::from_utf8_unchecked(&bytes[i..]) };
                let ch = s.chars().next().unwrap();
                result.push(ch);
                i += ch.len_utf8();
                continue;
            }
            i += 1;
        }

        result
    }

    /// Get variadic arguments (__VA_ARGS__) as a comma-separated string.
    fn get_va_args(&self, params: &[String], args: &[String]) -> String {
        let named_count = params.len();
        if args.len() > named_count {
            args[named_count..].join(", ")
        } else {
            String::new()
        }
    }

    /// Get ALL arguments for a named variadic parameter (e.g., `extra...`).
    fn get_named_va_args(&self, param_idx: usize, args: &[String]) -> String {
        if args.len() > param_idx {
            args[param_idx..].join(", ")
        } else {
            String::new()
        }
    }
}

impl Default for MacroTable {
    fn default() -> Self {
        Self::new()
    }
}

/// Check whether position `pos` in `bytes` is part of a pp-number token.
fn is_ppnumber_context(bytes: &[u8], pos: usize) -> bool {
    let mut j = pos;
    loop {
        let ch = bytes[j];
        if ch.is_ascii_alphanumeric() || ch == b'_' || ch == b'.' {
            if j == 0 {
                return ch.is_ascii_digit();
            }
            j -= 1;
        } else if (ch == b'+' || ch == b'-') && j >= 1
            && matches!(bytes[j - 1], b'e' | b'E' | b'p' | b'P')
        {
            if j < 2 {
                return false;
            }
            j -= 2;
        } else {
            let start = bytes[j + 1];
            return start.is_ascii_digit()
                || (start == b'.' && j + 2 <= pos && bytes[j + 2].is_ascii_digit());
        }
    }
}

/// Extract the trailing identifier from a string, if it ends with one.
/// Returns Some(ident) if the string ends with an identifier (skipping trailing whitespace).
/// Returns None if the trailing identifier is blue-painted (preceded by \x01 marker).
/// Operates on bytes for performance (no Vec<char> allocation).
fn extract_trailing_ident(s: &str) -> Option<String> {
    let bytes = s.trim_end().as_bytes();
    if bytes.is_empty() {
        return None;
    }
    let end = bytes.len();
    if !is_ident_cont_byte(bytes[end - 1]) {
        return None;
    }
    let mut start = end - 1;
    while start > 0 && is_ident_cont_byte(bytes[start - 1]) {
        start -= 1;
    }
    if !is_ident_start_byte(bytes[start]) {
        return None;
    }
    // Check for blue-paint marker immediately before the identifier
    if start > 0 && bytes[start - 1] == BLUE_PAINT_MARKER {
        return None;
    }
    Some(bytes_to_str(bytes, start, end).to_string())
}

/// Stringify a macro argument per C11 6.10.3.2.
/// Operates on bytes to avoid Vec<char> allocation.
fn stringify_arg(arg: &str) -> String {
    let trimmed = arg.trim();
    let mut result = String::new();
    let bytes = trimmed.as_bytes();
    let len = bytes.len();
    let mut i = 0;

    while i < len {
        // Skip blue-paint markers (they must not appear in stringified output)
        if bytes[i] == BLUE_PAINT_MARKER {
            i += 1;
            continue;
        }

        // Collapse whitespace sequences to a single space
        if bytes[i] == b' ' || bytes[i] == b'\t' || bytes[i] == b'\n' {
            if !result.is_empty() {
                result.push(' ');
            }
            while i < len && (bytes[i] == b' ' || bytes[i] == b'\t' || bytes[i] == b'\n') {
                i += 1;
            }
            continue;
        }

        // Handle string literals - escape " and \ inside them
        if bytes[i] == b'"' {
            result.push_str("\\\"");
            i += 1;
            while i < len && bytes[i] != b'"' {
                if bytes[i] == b'\\' {
                    result.push_str("\\\\");
                    i += 1;
                    if i < len {
                        if bytes[i] == b'"' {
                            result.push_str("\\\"");
                        } else if bytes[i] == b'\\' {
                            result.push_str("\\\\");
                        } else {
                            result.push(bytes[i] as char);
                        }
                        i += 1;
                    }
                } else {
                    result.push(bytes[i] as char);
                    i += 1;
                }
            }
            if i < len {
                result.push_str("\\\"");
                i += 1;
            }
            continue;
        }

        // Handle char literals
        if bytes[i] == b'\'' {
            result.push('\'');
            i += 1;
            while i < len && bytes[i] != b'\'' {
                if bytes[i] == b'\\' {
                    result.push_str("\\\\");
                    i += 1;
                    if i < len {
                        if bytes[i] == b'\\' {
                            result.push_str("\\\\");
                        } else if bytes[i] == b'"' {
                            result.push_str("\\\"");
                        } else {
                            result.push(bytes[i] as char);
                        }
                        i += 1;
                    }
                } else if bytes[i] == b'"' {
                    result.push_str("\\\"");
                    i += 1;
                } else {
                    result.push(bytes[i] as char);
                    i += 1;
                }
            }
            if i < len {
                result.push('\'');
                i += 1;
            }
            continue;
        }

        result.push(bytes[i] as char);
        i += 1;
    }

    // Trim trailing space
    if result.ends_with(' ') {
        result.pop();
    }

    result
}

/// Parse a #define directive line and return a MacroDef.
/// The line should be the text after `#define ` (with leading whitespace stripped).
/// Operates on bytes for performance.
pub fn parse_define(line: &str) -> Option<MacroDef> {
    let line = line.trim();
    if line.is_empty() {
        return None;
    }

    let bytes = line.as_bytes();
    let len = bytes.len();
    let mut i = 0;

    // Parse macro name
    if !is_ident_start_byte(bytes[0]) {
        return None;
    }
    while i < len && is_ident_cont_byte(bytes[i]) {
        i += 1;
    }
    let name = bytes_to_str(bytes, 0, i).to_string();

    // Check if function-like (opening paren immediately after name, no space)
    if i < len && bytes[i] == b'(' {
        // Function-like macro
        i += 1; // skip '('
        let mut params = Vec::new();
        let mut is_variadic = false;
        let mut has_named_variadic = false;

        loop {
            while i < len && (bytes[i] == b' ' || bytes[i] == b'\t') {
                i += 1;
            }

            if i >= len {
                break;
            }

            if bytes[i] == b')' {
                i += 1;
                break;
            }

            if bytes[i] == b'.' && i + 2 < len && bytes[i + 1] == b'.' && bytes[i + 2] == b'.' {
                is_variadic = true;
                i += 3;
                while i < len && bytes[i] != b')' {
                    i += 1;
                }
                if i < len {
                    i += 1;
                }
                break;
            }

            if is_ident_start_byte(bytes[i]) {
                let start = i;
                while i < len && is_ident_cont_byte(bytes[i]) {
                    i += 1;
                }
                let param = bytes_to_str(bytes, start, i).to_string();

                if i + 2 < len && bytes[i] == b'.' && bytes[i + 1] == b'.' && bytes[i + 2] == b'.' {
                    is_variadic = true;
                    has_named_variadic = true;
                    params.push(param);
                    i += 3;
                    while i < len && bytes[i] != b')' {
                        i += 1;
                    }
                    if i < len {
                        i += 1;
                    }
                    break;
                }

                params.push(param);
            }

            while i < len && (bytes[i] == b' ' || bytes[i] == b'\t') {
                i += 1;
            }

            if i < len && bytes[i] == b',' {
                i += 1;
            }
        }

        // Rest is the body
        let body = if i < len {
            line[i..].trim().to_string()
        } else {
            String::new()
        };

        Some(MacroDef {
            name,
            is_function_like: true,
            params,
            is_variadic,
            has_named_variadic,
            body,
            is_predefined: false,
        })
    } else {
        // Object-like macro
        let body = if i < len {
            line[i..].trim().to_string()
        } else {
            String::new()
        };

        Some(MacroDef {
            name,
            is_function_like: false,
            params: Vec::new(),
            is_variadic: false,
            has_named_variadic: false,
            body,
            is_predefined: false,
        })
    }
}

/// Inline integer-to-string formatting buffer.
/// Avoids heap allocation for small integers (which __LINE__, __COUNTER__ produce).
mod itoa {
    pub struct Buffer {
        bytes: [u8; 20], // enough for u64::MAX
        len: usize,
    }

    impl Buffer {
        pub fn new() -> Self {
            Buffer { bytes: [0u8; 20], len: 0 }
        }

        pub fn format(&mut self, mut n: usize) -> &str {
            if n == 0 {
                self.bytes[0] = b'0';
                self.len = 1;
                // SAFETY: b'0' is valid UTF-8.
                return unsafe { std::str::from_utf8_unchecked(&self.bytes[..1]) };
            }
            let mut pos = 20;
            while n > 0 {
                pos -= 1;
                self.bytes[pos] = b'0' + (n % 10) as u8;
                n /= 10;
            }
            self.len = 20 - pos;
            // Shift to beginning for simpler return
            self.bytes.copy_within(pos..20, 0);
            // SAFETY: All bytes are ASCII digits (b'0'..=b'9'), which are valid UTF-8.
            unsafe { std::str::from_utf8_unchecked(&self.bytes[..self.len]) }
        }
    }
}
