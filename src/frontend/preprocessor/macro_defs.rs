/// Macro definitions and expansion logic for the C preprocessor.
///
/// Supports:
/// - Object-like macros: `#define FOO value`
/// - Function-like macros: `#define MAX(a,b) ((a)>(b)?(a):(b))`
/// - Variadic macros: `#define LOG(fmt, ...) printf(fmt, __VA_ARGS__)`
/// - Stringification: `#param`
/// - Token pasting: `a ## b`

use std::collections::{HashMap, HashSet};

use super::utils::{is_ident_start, is_ident_cont, copy_literal, skip_literal};

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
    /// The replacement body (as raw text)
    pub body: String,
    /// Whether this is a predefined macro (cannot be #undef'd in strict mode)
    pub is_predefined: bool,
}

/// Stores all macro definitions and handles expansion.
#[derive(Debug, Clone)]
pub struct MacroTable {
    macros: HashMap<String, MacroDef>,
}

impl MacroTable {
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
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
        self.macros.contains_key(name)
    }

    /// Get a macro definition.
    pub fn get(&self, name: &str) -> Option<&MacroDef> {
        self.macros.get(name)
    }

    /// Expand macros in a line of text.
    /// Returns the expanded text.
    pub fn expand_line(&self, line: &str) -> String {
        let mut expanding = HashSet::new();
        self.expand_text(line, &mut expanding)
    }

    /// Recursively expand macros in text, tracking which macros are
    /// currently being expanded to prevent infinite recursion.
    fn expand_text(&self, text: &str, expanding: &mut HashSet<String>) -> String {
        let mut result = String::with_capacity(text.len());
        let chars: Vec<char> = text.chars().collect();
        let len = chars.len();
        let mut i = 0;

        while i < len {
            // Skip string and char literals (copy them verbatim)
            if chars[i] == '"' || chars[i] == '\'' {
                i = copy_literal(&chars, i, chars[i], &mut result);
                continue;
            }

            // Look for identifiers
            if is_ident_start(chars[i]) {
                let start = i;
                while i < len && is_ident_cont(chars[i]) {
                    i += 1;
                }
                let ident: String = chars[start..i].iter().collect();

                // Check if this identifier is part of a pp-number (e.g., 1.I, 15.IF, 0x1p2).
                // In C, pp-numbers include sequences like digit followed by identifier chars.
                // If preceded by a digit or dot-digit, this is a pp-number suffix, not a macro.
                let is_ppnumber_suffix = if start > 0 {
                    let prev = chars[start - 1];
                    prev.is_ascii_digit() || prev == '.' ||
                    // Also handle hex float exponent (0x1p+2) where 'p' precedes sign/digit
                    (prev == '+' || prev == '-') && start >= 2 && (chars[start - 2] == 'e' || chars[start - 2] == 'E' || chars[start - 2] == 'p' || chars[start - 2] == 'P')
                } else {
                    false
                };
                if is_ppnumber_suffix {
                    result.push_str(&ident);
                    continue;
                }

                // Check if this is a macro that's not currently being expanded
                if !expanding.contains(&ident) {
                    if let Some(mac) = self.macros.get(&ident) {
                        if mac.is_function_like {
                            // Need to find opening paren
                            let mut j = i;
                            // Skip whitespace
                            while j < len && chars[j].is_whitespace() {
                                j += 1;
                            }
                            if j < len && chars[j] == '(' {
                                // Parse arguments
                                let (args, end_pos) = self.parse_macro_args(&chars, j);
                                i = end_pos;
                                let expanded = self.expand_function_macro(mac, &args, expanding);
                                // Add spaces around expansion to prevent token pasting
                                // with adjacent characters (e.g., M(-) before --b)
                                if !expanded.is_empty() {
                                    if !result.is_empty() && !result.ends_with(' ') && !result.ends_with('\t') && !result.ends_with('\n') {
                                        result.push(' ');
                                    }
                                    result.push_str(&expanded);
                                    result.push(' ');
                                }
                                continue;
                            } else {
                                // Function-like macro without args - don't expand
                                result.push_str(&ident);
                                continue;
                            }
                        } else {
                            // Object-like macro
                            expanding.insert(ident.clone());
                            let expanded = self.expand_text(&mac.body, expanding);
                            expanding.remove(&ident);

                            // Check if the expansion result is a function-like macro name
                            // followed by '(' in the remaining text. This handles the
                            // pattern: #define BAR FOO  then  BAR(args) -> FOO(args)
                            let expanded_trimmed = expanded.trim();
                            if !expanded_trimmed.is_empty() && is_ident_start(expanded_trimmed.chars().next().unwrap())
                                && expanded_trimmed.chars().all(|c| is_ident_cont(c))
                                && !expanding.contains(expanded_trimmed)
                            {
                                if let Some(target_mac) = self.macros.get(expanded_trimmed) {
                                    if target_mac.is_function_like {
                                        // Check if '(' follows in the remaining text
                                        let mut j = i;
                                        while j < len && chars[j].is_whitespace() {
                                            j += 1;
                                        }
                                        if j < len && chars[j] == '(' {
                                            // Parse arguments and expand as function-like macro
                                            let (args, end_pos) = self.parse_macro_args(&chars, j);
                                            i = end_pos;
                                            let target_mac_clone = target_mac.clone();
                                            let func_expanded = self.expand_function_macro(&target_mac_clone, &args, expanding);
                                            result.push_str(&func_expanded);
                                            continue;
                                        }
                                    }
                                }
                            }

                            result.push_str(&expanded);
                            continue;
                        }
                    }
                }

                // Not a macro or already expanding
                result.push_str(&ident);
                continue;
            }

            // Skip C-style comments
            if i + 1 < len && chars[i] == '/' && chars[i + 1] == '*' {
                result.push('/');
                result.push('*');
                i += 2;
                while i + 1 < len && !(chars[i] == '*' && chars[i + 1] == '/') {
                    result.push(chars[i]);
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
            if i + 1 < len && chars[i] == '/' && chars[i + 1] == '/' {
                while i < len && chars[i] != '\n' {
                    result.push(chars[i]);
                    i += 1;
                }
                continue;
            }

            // Skip numeric literals (pp-numbers): digits, '.', followed by alphanumeric suffix
            // This prevents macro expansion of suffixes like 'I' in '205.0I'
            if chars[i].is_ascii_digit() || (chars[i] == '.' && i + 1 < len && chars[i + 1].is_ascii_digit()) {
                // Copy the entire pp-number token: digits, '.', 'e'/'E'/'p'/'P' with optional sign,
                // hex prefix '0x'/'0X', and alphanumeric suffixes (f, F, l, L, u, U, i, I, j, J, ll, LL)
                while i < len {
                    if chars[i].is_ascii_alphanumeric() || chars[i] == '.' || chars[i] == '_' {
                        result.push(chars[i]);
                        i += 1;
                    } else if (chars[i] == '+' || chars[i] == '-')
                        && i > 0
                        && (chars[i - 1] == 'e' || chars[i - 1] == 'E' || chars[i - 1] == 'p' || chars[i - 1] == 'P')
                    {
                        // Exponent sign: 1.0e+5, 0x1p-3
                        result.push(chars[i]);
                        i += 1;
                    } else {
                        break;
                    }
                }
                continue;
            }

            // Regular character
            result.push(chars[i]);
            i += 1;
        }

        result
    }

    /// Parse function-like macro arguments from chars starting at the opening paren.
    /// Returns (args, position after closing paren).
    fn parse_macro_args(&self, chars: &[char], start: usize) -> (Vec<String>, usize) {
        let len = chars.len();
        let mut i = start + 1; // skip '('
        let mut args = Vec::new();
        let mut current_arg = String::new();
        let mut paren_depth = 0;

        while i < len {
            match chars[i] {
                '(' => {
                    paren_depth += 1;
                    current_arg.push('(');
                    i += 1;
                }
                ')' => {
                    if paren_depth == 0 {
                        // End of arguments
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
                ',' if paren_depth == 0 => {
                    args.push(current_arg.trim().to_string());
                    current_arg = String::new();
                    i += 1;
                }
                '"' | '\'' => {
                    // String/char literal - copy verbatim, don't split on commas inside
                    i = copy_literal(chars, i, chars[i], &mut current_arg);
                }
                _ => {
                    current_arg.push(chars[i]);
                    i += 1;
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
        expanding: &mut HashSet<String>,
    ) -> String {
        // Step 1: Determine which parameters are used with # or ## (these get raw args)
        let paste_params = self.find_paste_and_stringify_params(&mac.body, &mac.params, mac.is_variadic);

        // Step 2: Prescan - expand arguments that are NOT used with # or ##
        // Per the standard, argument prescan uses the current expanding set
        // (but NOT the macro being expanded itself - that's only suppressed during rescan)
        let expanded_args: Vec<String> = args.iter().enumerate().map(|(idx, arg)| {
            if idx < mac.params.len() && paste_params.contains(&idx) {
                // Used with # or ## - don't expand
                arg.clone()
            } else {
                // Normal argument - expand macros in it
                self.expand_text(arg, expanding)
            }
        }).collect();

        let mut body = mac.body.clone();

        // Step 3: Handle stringification (#param) and token pasting (##)
        // These use the RAW (unexpanded) arguments
        body = self.handle_stringify_and_paste(&body, &mac.params, args, mac.is_variadic);

        // Step 4: Substitute parameters with expanded arguments
        body = self.substitute_params(&body, &mac.params, &expanded_args, mac.is_variadic);

        // Step 5: Rescan with the current macro name suppressed
        expanding.insert(mac.name.clone());
        let result = self.expand_text(&body, expanding);
        expanding.remove(&mac.name);
        result
    }

    /// Find parameter indices that are used with # (stringify) or ## (token paste).
    /// These parameters should receive raw (unexpanded) arguments.
    fn find_paste_and_stringify_params(&self, body: &str, params: &[String], is_variadic: bool) -> HashSet<usize> {
        let mut result = HashSet::new();
        let chars: Vec<char> = body.chars().collect();
        let len = chars.len();
        let mut i = 0;

        while i < len {
            if chars[i] == '#' && i + 1 < len && chars[i + 1] == '#' {
                // Token paste: ## - mark both the token before and after
                // Find identifier before ##
                let mut j = i;
                // Skip whitespace before ##
                while j > 0 && (chars[j - 1] == ' ' || chars[j - 1] == '\t') {
                    j -= 1;
                }
                // Read identifier backwards
                let end = j;
                while j > 0 && is_ident_cont(chars[j - 1]) {
                    j -= 1;
                }
                if j < end {
                    let ident: String = chars[j..end].iter().collect();
                    if let Some(idx) = params.iter().position(|p| p == &ident) {
                        result.insert(idx);
                    }
                    if ident == "__VA_ARGS__" && is_variadic {
                        // Mark all variadic args
                        for idx in params.len()..100 {
                            result.insert(idx);
                        }
                    }
                }

                // Find identifier after ##
                i += 2;
                while i < len && (chars[i] == ' ' || chars[i] == '\t') {
                    i += 1;
                }
                if i < len && is_ident_start(chars[i]) {
                    let start = i;
                    while i < len && is_ident_cont(chars[i]) {
                        i += 1;
                    }
                    let ident: String = chars[start..i].iter().collect();
                    if let Some(idx) = params.iter().position(|p| p == &ident) {
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

            if chars[i] == '#' && i + 1 < len && chars[i + 1] != '#' {
                // Stringification: # - mark the parameter after it
                i += 1;
                while i < len && (chars[i] == ' ' || chars[i] == '\t') {
                    i += 1;
                }
                if i < len && is_ident_start(chars[i]) {
                    let start = i;
                    while i < len && is_ident_cont(chars[i]) {
                        i += 1;
                    }
                    let ident: String = chars[start..i].iter().collect();
                    if let Some(idx) = params.iter().position(|p| p == &ident) {
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

            // Skip string/char literals
            if chars[i] == '"' || chars[i] == '\'' {
                i = skip_literal(&chars, i, chars[i]);
                continue;
            }

            i += 1;
        }

        result
    }

    /// Handle # (stringify) and ## (token paste) operators.
    ///
    /// Per C11 §6.10.3.2 and §6.10.3.3:
    /// - `#param` stringifies the raw argument
    /// - `token ## token` pastes tokens together, with parameter substitution
    ///   using raw (unexpanded) arguments for both sides of ##
    fn handle_stringify_and_paste(
        &self,
        body: &str,
        params: &[String],
        args: &[String],
        is_variadic: bool,
    ) -> String {
        let chars: Vec<char> = body.chars().collect();
        let len = chars.len();

        // First, check if body contains ## or #. If not, short-circuit.
        if !body.contains('#') {
            return body.to_string();
        }

        let mut result = String::new();
        let mut i = 0;

        while i < len {
            if chars[i] == '#' && i + 1 < len && chars[i + 1] == '#' {
                // Token paste operator ##
                // Remove trailing whitespace from the left side in result
                while result.ends_with(' ') || result.ends_with('\t') {
                    result.pop();
                }

                // The left side is the last token we added to result.
                // Check if it was a parameter name that we should substitute.
                // Extract the last identifier from result (if any) for parameter substitution
                let left_token = extract_trailing_ident(&result);
                if let Some(ref left_ident) = left_token {
                    if left_ident == "__VA_ARGS__" && is_variadic {
                        // Replace trailing __VA_ARGS__ with raw variadic args
                        let va_args = self.get_va_args(params, args);
                        let trim_len = result.len() - "__VA_ARGS__".len();
                        result.truncate(trim_len);
                        // For GNU extension: if VA_ARGS is empty and result ends with comma, remove comma
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
                    } else if let Some(idx) = params.iter().position(|p| p == left_ident) {
                        // Replace the left-side parameter with its raw argument
                        let trim_len = result.len() - left_ident.len();
                        result.truncate(trim_len);
                        let arg = args.get(idx).map(|s| s.as_str()).unwrap_or("");
                        result.push_str(arg);
                    }
                }

                i += 2; // skip ##

                // Skip whitespace after ##
                while i < len && (chars[i] == ' ' || chars[i] == '\t') {
                    i += 1;
                }

                // Read the right-side token
                if i < len && is_ident_start(chars[i]) {
                    let start = i;
                    while i < len && is_ident_cont(chars[i]) {
                        i += 1;
                    }
                    let right_ident: String = chars[start..i].iter().collect();

                    if right_ident == "__VA_ARGS__" && is_variadic {
                        let va_args = self.get_va_args(params, args);
                        if va_args.is_empty() {
                            // GNU extension: ## __VA_ARGS__ with empty args removes preceding comma
                            while result.ends_with(' ') || result.ends_with('\t') {
                                result.pop();
                            }
                            if result.ends_with(',') {
                                result.pop();
                            }
                        } else {
                            result.push_str(&va_args);
                        }
                    } else if let Some(idx) = params.iter().position(|p| p == &right_ident) {
                        // Substitute parameter with raw argument
                        let arg = args.get(idx).map(|s| s.as_str()).unwrap_or("");
                        result.push_str(arg);
                    } else {
                        // Not a parameter, paste as-is
                        result.push_str(&right_ident);
                    }
                } else if i < len {
                    // Non-identifier token (e.g., number)
                    result.push(chars[i]);
                    i += 1;
                }
                continue;
            }

            if chars[i] == '#' && i + 1 < len && chars[i + 1] != '#' {
                // Check that the next # is not part of a ## later
                // (e.g., "# param" is stringify, but in "a # ## b" it's different)
                // Stringification operator #
                i += 1;
                // Skip whitespace
                while i < len && (chars[i] == ' ' || chars[i] == '\t') {
                    i += 1;
                }
                // Read parameter name
                if i < len && is_ident_start(chars[i]) {
                    let start = i;
                    while i < len && is_ident_cont(chars[i]) {
                        i += 1;
                    }
                    let param_name: String = chars[start..i].iter().collect();

                    if param_name == "__VA_ARGS__" && is_variadic {
                        let va_args = self.get_va_args(params, args);
                        result.push('"');
                        result.push_str(&stringify_arg(&va_args));
                        result.push('"');
                    } else if let Some(idx) = params.iter().position(|p| p == &param_name) {
                        let arg = args.get(idx).map(|s| s.as_str()).unwrap_or("");
                        result.push('"');
                        result.push_str(&stringify_arg(arg));
                        result.push('"');
                    } else {
                        // Not a parameter, keep as-is
                        result.push('#');
                        result.push_str(&param_name);
                    }
                    continue;
                } else {
                    result.push('#');
                    continue;
                }
            }

            result.push(chars[i]);
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
    ) -> String {
        let mut result = String::new();
        let chars: Vec<char> = body.chars().collect();
        let len = chars.len();
        let mut i = 0;

        while i < len {
            // Skip string/char literals (copy verbatim, don't substitute inside)
            if chars[i] == '"' || chars[i] == '\'' {
                i = copy_literal(&chars, i, chars[i], &mut result);
                continue;
            }

            if is_ident_start(chars[i]) {
                let start = i;
                while i < len && is_ident_cont(chars[i]) {
                    i += 1;
                }
                let ident: String = chars[start..i].iter().collect();

                if ident == "__VA_ARGS__" && is_variadic {
                    let va_args = self.get_va_args(params, args);
                    result.push_str(&va_args);
                } else if let Some(idx) = params.iter().position(|p| p == &ident) {
                    let arg = args.get(idx).map(|s| s.as_str()).unwrap_or("");
                    result.push_str(arg);
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

    /// Get variadic arguments (__VA_ARGS__) as a comma-separated string.
    fn get_va_args(&self, params: &[String], args: &[String]) -> String {
        let named_count = params.len();
        if args.len() > named_count {
            args[named_count..].join(", ")
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

/// Extract the trailing identifier from a string, if it ends with one.
/// Returns Some(ident) if the string ends with an identifier.
fn extract_trailing_ident(s: &str) -> Option<String> {
    let chars: Vec<char> = s.chars().collect();
    if chars.is_empty() {
        return None;
    }
    let end = chars.len();
    // The last char must be part of an identifier
    if !is_ident_cont(chars[end - 1]) {
        return None;
    }
    // Walk back to find the start of the identifier
    let mut start = end - 1;
    while start > 0 && is_ident_cont(chars[start - 1]) {
        start -= 1;
    }
    // Verify start is valid ident start
    if is_ident_start(chars[start]) {
        Some(chars[start..end].iter().collect())
    } else {
        None
    }
}

/// Escape a string for stringification.
fn stringify_arg(arg: &str) -> String {
    let mut result = String::new();
    for ch in arg.chars() {
        match ch {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            _ => result.push(ch),
        }
    }
    result
}

/// Parse a #define directive line and return a MacroDef.
/// The line should be the text after `#define ` (with leading whitespace stripped).
pub fn parse_define(line: &str) -> Option<MacroDef> {
    let line = line.trim();
    if line.is_empty() {
        return None;
    }

    let chars: Vec<char> = line.chars().collect();
    let len = chars.len();
    let mut i = 0;

    // Parse macro name
    if !is_ident_start(chars[0]) {
        return None;
    }
    while i < len && is_ident_cont(chars[i]) {
        i += 1;
    }
    let name: String = chars[0..i].iter().collect();

    // Check if function-like (opening paren immediately after name, no space)
    if i < len && chars[i] == '(' {
        // Function-like macro
        i += 1; // skip '('
        let mut params = Vec::new();
        let mut is_variadic = false;

        // Parse parameters
        loop {
            // Skip whitespace
            while i < len && (chars[i] == ' ' || chars[i] == '\t') {
                i += 1;
            }

            if i >= len {
                break;
            }

            if chars[i] == ')' {
                i += 1;
                break;
            }

            if chars[i] == '.' && i + 2 < len && chars[i + 1] == '.' && chars[i + 2] == '.' {
                is_variadic = true;
                i += 3;
                // Skip to closing paren
                while i < len && chars[i] != ')' {
                    i += 1;
                }
                if i < len {
                    i += 1;
                }
                break;
            }

            // Read parameter name
            if is_ident_start(chars[i]) {
                let start = i;
                while i < len && is_ident_cont(chars[i]) {
                    i += 1;
                }
                let param: String = chars[start..i].iter().collect();

                // Check for variadic named param: `name...`
                if i + 2 < len && chars[i] == '.' && chars[i + 1] == '.' && chars[i + 2] == '.' {
                    // Named variadic like `args...` - treat as regular + variadic
                    is_variadic = true;
                    params.push(param);
                    i += 3;
                    while i < len && chars[i] != ')' {
                        i += 1;
                    }
                    if i < len {
                        i += 1;
                    }
                    break;
                }

                params.push(param);
            }

            // Skip whitespace
            while i < len && (chars[i] == ' ' || chars[i] == '\t') {
                i += 1;
            }

            if i < len && chars[i] == ',' {
                i += 1;
            }
        }

        // Rest is the body
        let body = if i < len {
            chars[i..].iter().collect::<String>().trim().to_string()
        } else {
            String::new()
        };

        Some(MacroDef {
            name,
            is_function_like: true,
            params,
            is_variadic,
            body,
            is_predefined: false,
        })
    } else {
        // Object-like macro
        let body = if i < len {
            chars[i..].iter().collect::<String>().trim().to_string()
        } else {
            String::new()
        };

        Some(MacroDef {
            name,
            is_function_like: false,
            params: Vec::new(),
            is_variadic: false,
            body,
            is_predefined: false,
        })
    }
}
