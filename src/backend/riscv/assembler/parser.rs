//! RISC-V assembly parser.
//!
//! Parses the textual assembly format emitted by our RISC-V codegen into
//! structured `AsmStatement` values. The parser handles:
//! - Labels (global and local)
//! - Directives (.section, .globl, .type, .align, .byte, .long, .dword, etc.)
//!   with fully typed representation (no string re-parsing in ELF writer)
//! - RISC-V instructions (add, sub, ld, sd, beq, call, ret, etc.)
//! - CFI directives (passed through as-is for DWARF unwind info)

#![allow(dead_code)]

use crate::backend::asm_expr;
use crate::backend::elf;

/// A parsed assembly operand.
#[derive(Debug, Clone)]
pub enum Operand {
    /// Register: x0-x31, zero, ra, sp, gp, tp, t0-t6, s0-s11, a0-a7,
    ///           f0-f31, ft0-ft11, fs0-fs11, fa0-fa7
    Reg(String),
    /// Immediate value: 42, -1, 0x1000
    Imm(i64),
    /// Symbol reference: function name, label, etc.
    Symbol(String),
    /// Symbol with addend: symbol+offset or symbol-offset
    SymbolOffset(String, i64),
    /// Memory operand: offset(base) e.g., 8(sp) or -16(s0)
    Mem { base: String, offset: i64 },
    /// Memory operand with symbol: %lo(symbol)(base) or similar
    MemSymbol { base: String, symbol: String, modifier: String },
    /// Label reference for branches
    Label(String),
    /// Fence operand: iorw etc.
    FenceArg(String),
    /// CSR register name or number
    Csr(String),
    /// Rounding mode: rne, rtz, rdn, rup, rmm, dyn
    RoundingMode(String),
}

/// A data value in a .byte/.short/.long/.quad directive.
/// Can be a literal integer, a symbol reference (with optional addend),
/// or a symbol difference expression (A - B, with optional addend on A).
#[derive(Debug, Clone)]
pub enum DataValue {
    /// A literal integer value.
    Integer(i64),
    /// A symbol reference, possibly with an addend: `sym` or `sym+4` or `sym-8`.
    Symbol { name: String, addend: i64 },
    /// A symbol difference: `sym_a - sym_b`, possibly with addend on sym_a.
    SymbolDiff { sym_a: String, sym_b: String, addend: i64 },
}

/// Symbol type as parsed from `.type sym, @function` etc.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolType {
    Function,
    Object,
    TlsObject,
    NoType,
}

/// Symbol visibility as parsed from `.hidden`, `.protected`, `.internal`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Hidden,
    Protected,
    Internal,
}

/// A size expression from `.size sym, expr`.
#[derive(Debug, Clone)]
pub enum SizeExpr {
    /// `.- label` — current position minus label
    CurrentMinus(String),
    /// A literal size value.
    Absolute(u64),
}

/// Section type from `.section` directive.
#[derive(Debug, Clone)]
pub struct SectionInfo {
    pub name: String,
    pub flags: String,
    pub sec_type: String,
    /// True when the directive explicitly included a flags field (even if empty).
    pub flags_explicit: bool,
}

/// A typed assembly directive. All argument parsing happens in the parser,
/// so the ELF writer only needs to pattern-match on these variants.
#[derive(Debug, Clone)]
pub enum Directive {
    /// `.section name, "flags", @type`
    Section(SectionInfo),
    /// `.text`
    Text,
    /// `.data`
    Data,
    /// `.bss`
    Bss,
    /// `.rodata`
    Rodata,
    /// `.globl sym` or `.global sym`
    Globl(String),
    /// `.weak sym`
    Weak(String),
    /// `.hidden sym`, `.protected sym`, `.internal sym`
    SymVisibility(String, Visibility),
    /// `.type sym, @function` etc.
    Type(String, SymbolType),
    /// `.size sym, expr`
    Size(String, SizeExpr),
    /// `.align N` or `.p2align N` — power-of-2 alignment
    Align(u64),
    /// `.balign N` — byte alignment
    Balign(u64),
    /// `.byte val, val, ...`
    Byte(Vec<DataValue>),
    /// `.short val, ...` / `.hword` / `.2byte` / `.half`
    Short(Vec<DataValue>),
    /// `.long val, ...` / `.4byte` / `.word`
    Long(Vec<DataValue>),
    /// `.quad val, ...` / `.8byte` / `.xword` / `.dword`
    Quad(Vec<DataValue>),
    /// `.zero N[, fill]` / `.space N[, fill]`
    Zero { size: usize, fill: u8 },
    /// `.asciz "str"` / `.string "str"` — null-terminated string (raw bytes)
    Asciz(Vec<u8>),
    /// `.ascii "str"` — string without null terminator (raw bytes)
    Ascii(Vec<u8>),
    /// `.comm sym, size[, align]`
    Comm { sym: String, size: u64, align: u64 },
    /// `.local sym`
    Local(String),
    /// `.set sym, val` / `.equ sym, val`
    Set(String, String),
    /// `.option ...` — RISC-V specific
    ArchOption(String),
    /// `.attribute ...` — RISC-V attribute
    Attribute(String),
    /// CFI directives — silently ignored
    Cfi,
    /// Other ignorable directives: .file, .loc, .ident, etc.
    Ignored,
    /// `.pushsection name, "flags", @type` — push current section and switch
    PushSection(SectionInfo),
    /// `.popsection` / `.previous` — pop section stack
    PopSection,
    /// `.insn ...` — emit raw instruction encoding
    Insn(String),
    /// Unknown directive — preserved for forward compatibility
    Unknown { name: String, args: String },
}

/// A parsed assembly statement.
#[derive(Debug, Clone)]
pub enum AsmStatement {
    /// A label definition: "name:"
    Label(String),
    /// A typed assembly directive.
    Directive(Directive),
    /// A RISC-V instruction with mnemonic and operands
    Instruction {
        mnemonic: String,
        operands: Vec<Operand>,
        /// The raw text of the operand string (for fallback/debugging)
        raw_operands: String,
    },
    /// An empty line or comment
    Empty,
}

/// Parse assembly text into a list of statements.
/// Expand .rept/.endr blocks by repeating contained lines.
// TODO: extract expand_rept_blocks to shared module (duplicated in ARM, RISC-V, x86 parsers)
fn expand_rept_blocks(lines: &[&str]) -> Result<Vec<String>, String> {
    let mut result = Vec::new();
    let mut i = 0;
    while i < lines.len() {
        let trimmed = strip_comment(lines[i]).trim().to_string();
        if trimmed.starts_with(".rept ") || trimmed.starts_with(".rept\t") {
            let count_str = trimmed[".rept".len()..].trim();
            let count_val = parse_int_literal(count_str)
                .map_err(|e| format!(".rept: bad count '{}': {}", count_str, e))?;
            // Treat negative counts as 0 (matches GNU as behavior)
            let count = if count_val < 0 { 0usize } else { count_val as usize };
            let mut depth = 1;
            let mut body = Vec::new();
            i += 1;
            while i < lines.len() {
                let inner = strip_comment(lines[i]).trim().to_string();
                if inner.starts_with(".rept ") || inner.starts_with(".rept\t") {
                    depth += 1;
                } else if inner == ".endr" {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                body.push(lines[i]);
                i += 1;
            }
            if depth != 0 {
                return Err(".rept without matching .endr".to_string());
            }
            let expanded_body = expand_rept_blocks(&body)?;
            for _ in 0..count {
                result.extend(expanded_body.iter().cloned());
            }
        } else if trimmed == ".endr" {
            // stray .endr without .rept - skip
        } else {
            result.push(lines[i].to_string());
        }
        i += 1;
    }
    Ok(result)
}

pub fn parse_asm(text: &str) -> Result<Vec<AsmStatement>, String> {
    // Pre-process: strip C-style /* ... */ comments (may span multiple lines)
    let text = strip_c_comments(text);

    // Expand .rept/.endr blocks
    let raw_lines: Vec<&str> = text.lines().collect();
    let expanded_lines = expand_rept_blocks(&raw_lines)?;

    let mut statements = Vec::new();
    for (line_num, line) in expanded_lines.iter().enumerate() {
        let line = line.trim();

        // Skip empty lines
        if line.is_empty() {
            statements.push(AsmStatement::Empty);
            continue;
        }

        // Strip comments
        let line = strip_comment(line);
        let line = line.trim();
        if line.is_empty() {
            statements.push(AsmStatement::Empty);
            continue;
        }

        // Handle ';' as statement separator (GAS syntax).
        // Split the line on ';' and parse each part independently.
        let parts = split_on_semicolons(line);
        for part in parts {
            let part = part.trim();
            if part.is_empty() {
                continue;
            }
            match parse_line(part) {
                Ok(stmts) => statements.extend(stmts),
                Err(e) => return Err(format!("Line {}: {}: '{}'", line_num + 1, e, part)),
            }
        }
    }
    Ok(statements)
}

/// Split a line on ';' characters, respecting strings.
/// In GAS syntax, ';' separates multiple statements on the same line.
fn split_on_semicolons(line: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut in_string = false;
    let mut escape = false;
    let mut start = 0;
    for (i, c) in line.char_indices() {
        if escape {
            escape = false;
            continue;
        }
        if c == '\\' && in_string {
            escape = true;
            continue;
        }
        if c == '"' {
            in_string = !in_string;
            continue;
        }
        if c == ';' && !in_string {
            parts.push(&line[start..i]);
            start = i + 1;
        }
    }
    parts.push(&line[start..]);
    parts
}

/// Strip C-style /* ... */ comments from assembly text, handling multi-line spans.
/// Preserves newlines inside comments so line numbers remain correct for error messages.
fn strip_c_comments(text: &str) -> String {
    let mut result = String::with_capacity(text.len());
    let bytes = text.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i + 1] == b'*' {
            i += 2;
            while i + 1 < bytes.len() {
                if bytes[i] == b'*' && bytes[i + 1] == b'/' {
                    i += 2;
                    break;
                }
                if bytes[i] == b'\n' {
                    result.push('\n');
                }
                i += 1;
            }
        } else {
            result.push(bytes[i] as char);
            i += 1;
        }
    }
    result
}

fn strip_comment(line: &str) -> &str {
    // Scan character by character, tracking string state to find comments
    // outside of string literals. This correctly handles escaped quotes (\")
    // inside strings (e.g. .asciz "a\"b#c" should not strip at #).
    let bytes = line.as_bytes();
    let mut in_string = false;
    let mut i = 0;
    while i < bytes.len() {
        if in_string {
            if bytes[i] == b'\\' {
                i += 2; // skip escaped character
                continue;
            }
            if bytes[i] == b'"' {
                in_string = false;
            }
            i += 1;
            continue;
        }
        // Not in string
        if bytes[i] == b'"' {
            in_string = true;
            i += 1;
            continue;
        }
        // Check for # comment (GAS RISC-V comment character)
        if bytes[i] == b'#' {
            return &line[..i];
        }
        // Check for // comment
        if bytes[i] == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
            return &line[..i];
        }
        i += 1;
    }
    line
}

fn parse_line(line: &str) -> Result<Vec<AsmStatement>, String> {
    // Check for label definition (name:)
    // Labels can be at the start of the line, possibly followed by an instruction
    if let Some(colon_pos) = line.find(':') {
        let potential_label = line[..colon_pos].trim();
        // Verify it looks like a valid label (no spaces before colon, alphanumeric + _ + .)
        if !potential_label.is_empty()
            && !potential_label.contains(' ')
            && !potential_label.contains('\t')
            && (!potential_label.starts_with('.')
                || potential_label.starts_with(".L")
                || potential_label.starts_with(".l"))
        {
            // Make sure this isn't a directive
            if !potential_label.starts_with('.')
                || potential_label.starts_with(".L")
                || potential_label.starts_with(".l")
            {
                let mut result = vec![AsmStatement::Label(potential_label.to_string())];
                // Check for instruction/directive after the label on the same line
                let rest = line[colon_pos + 1..].trim();
                if !rest.is_empty() {
                    result.extend(parse_line(rest)?);
                }
                return Ok(result);
            }
        }
    }

    let trimmed = line.trim();

    // Directive: starts with .
    if trimmed.starts_with('.') {
        return Ok(vec![parse_directive(trimmed)?]);
    }

    // Instruction
    Ok(vec![parse_instruction(trimmed)?])
}

/// Split a directive line into name and args, then dispatch to typed parsing.
fn parse_directive(line: &str) -> Result<AsmStatement, String> {
    let (name, args) = if let Some(space_pos) = line.find([' ', '\t']) {
        let name = &line[..space_pos];
        let args = line[space_pos..].trim();
        (name, args)
    } else {
        (line, "")
    };

    let directive = match name {
        ".section" => {
            let info = parse_section_args(args);
            Directive::Section(info)
        }
        ".text" => Directive::Text,
        ".data" => Directive::Data,
        ".bss" => Directive::Bss,
        ".rodata" => Directive::Rodata,

        ".globl" | ".global" => Directive::Globl(args.trim().to_string()),
        ".weak" => Directive::Weak(args.trim().to_string()),
        ".hidden" => Directive::SymVisibility(args.trim().to_string(), Visibility::Hidden),
        ".protected" => Directive::SymVisibility(args.trim().to_string(), Visibility::Protected),
        ".internal" => Directive::SymVisibility(args.trim().to_string(), Visibility::Internal),

        ".type" => parse_type_directive(args),

        ".size" => parse_size_directive(args),

        ".align" | ".p2align" => {
            let val: u64 = args.trim().split(',').next()
                .and_then(|s| s.trim().parse().ok())
                .unwrap_or(0);
            Directive::Align(val)
        }
        ".balign" => {
            let val: u64 = args.trim().parse().unwrap_or(1);
            Directive::Balign(val)
        }

        ".byte" => {
            let values = parse_data_values(args)?;
            Directive::Byte(values)
        }
        ".short" | ".hword" | ".2byte" | ".half" => {
            let values = parse_data_values(args)?;
            Directive::Short(values)
        }
        ".long" | ".4byte" | ".word" => {
            let values = parse_data_values(args)?;
            Directive::Long(values)
        }
        ".quad" | ".8byte" | ".xword" | ".dword" => {
            let values = parse_data_values(args)?;
            Directive::Quad(values)
        }

        ".zero" | ".space" => {
            let parts: Vec<&str> = args.trim().split(',').collect();
            let size: usize = parts[0].trim().parse()
                .map_err(|_| format!("invalid .zero size: {}", args))?;
            let fill: u8 = if parts.len() > 1 {
                parse_data_value_int(parts[1].trim())? as u8
            } else {
                0
            };
            Directive::Zero { size, fill }
        }

        ".asciz" | ".string" => {
            let s = elf::parse_string_literal(args)?;
            Directive::Asciz(s)
        }
        ".ascii" => {
            let s = elf::parse_string_literal(args)?;
            Directive::Ascii(s)
        }

        ".comm" => {
            let parts: Vec<&str> = args.split(',').collect();
            let sym = if !parts.is_empty() { parts[0].trim().to_string() } else { String::new() };
            let size: u64 = if parts.len() >= 2 { parts[1].trim().parse().unwrap_or(0) } else { 0 };
            let align: u64 = if parts.len() > 2 { parts[2].trim().parse().unwrap_or(1) } else { 1 };
            Directive::Comm { sym, size, align }
        }

        ".local" => Directive::Local(args.trim().to_string()),

        ".set" | ".equ" => {
            let parts: Vec<&str> = args.splitn(2, ',').collect();
            let sym = if !parts.is_empty() { parts[0].trim().to_string() } else { String::new() };
            let val = if parts.len() > 1 { parts[1].trim().to_string() } else { String::new() };
            Directive::Set(sym, val)
        }

        ".option" => Directive::ArchOption(args.to_string()),
        ".attribute" => Directive::Attribute(args.to_string()),
        ".insn" => Directive::Insn(args.to_string()),

        // CFI directives
        ".cfi_startproc" | ".cfi_endproc" | ".cfi_def_cfa_offset"
        | ".cfi_offset" | ".cfi_def_cfa_register" | ".cfi_restore"
        | ".cfi_remember_state" | ".cfi_restore_state"
        | ".cfi_adjust_cfa_offset" | ".cfi_def_cfa"
        | ".cfi_sections" | ".cfi_personality" | ".cfi_lsda"
        | ".cfi_rel_offset" | ".cfi_register" | ".cfi_return_column"
        | ".cfi_undefined" | ".cfi_same_value" | ".cfi_escape" => Directive::Cfi,

        ".pushsection" => {
            Directive::PushSection(parse_section_args(args))
        }
        ".popsection" | ".previous" => Directive::PopSection,

        // Other ignorable directives
        ".file" | ".loc" | ".ident" | ".addrsig" | ".addrsig_sym"
        | ".build_attributes" | ".eabi_attribute" => Directive::Ignored,

        _ => Directive::Unknown {
            name: name.to_string(),
            args: args.to_string(),
        },
    };

    Ok(AsmStatement::Directive(directive))
}

/// Parse `.section name, "flags", @type` arguments.
fn parse_section_args(args: &str) -> SectionInfo {
    let parts: Vec<&str> = args.split(',').collect();
    let name = parts[0].trim().to_string();
    let flags_explicit = parts.len() > 1;
    let flags = if flags_explicit {
        parts[1].trim().trim_matches('"').to_string()
    } else {
        String::new()
    };
    let sec_type = if parts.len() > 2 {
        parts[2].trim().to_string()
    } else {
        // Default type based on section name
        if name == ".bss" || name.starts_with(".bss.") || name.starts_with(".tbss") {
            "@nobits".to_string()
        } else {
            "@progbits".to_string()
        }
    };
    SectionInfo { name, flags, sec_type, flags_explicit }
}

/// Parse `.type sym, @function` etc.
fn parse_type_directive(args: &str) -> Directive {
    let parts: Vec<&str> = args.splitn(2, ',').collect();
    if parts.len() == 2 {
        let sym = parts[0].trim().to_string();
        let ty = parts[1].trim();
        let st = match ty {
            "%function" | "@function" => SymbolType::Function,
            "%object" | "@object" => SymbolType::Object,
            "@tls_object" => SymbolType::TlsObject,
            _ => SymbolType::NoType,
        };
        Directive::Type(sym, st)
    } else {
        // Malformed .type directive — treat as no-type
        Directive::Type(args.trim().to_string(), SymbolType::NoType)
    }
}

/// Parse `.size sym, expr`.
fn parse_size_directive(args: &str) -> Directive {
    let parts: Vec<&str> = args.splitn(2, ',').collect();
    if parts.len() == 2 {
        let sym = parts[0].trim().to_string();
        let size_expr = parts[1].trim();
        if let Some(label) = size_expr.strip_prefix(".-") {
            Directive::Size(sym, SizeExpr::CurrentMinus(label.to_string()))
        } else if let Ok(size) = size_expr.parse::<u64>() {
            Directive::Size(sym, SizeExpr::Absolute(size))
        } else {
            // Can't parse — use 0
            Directive::Size(sym, SizeExpr::Absolute(0))
        }
    } else {
        // Malformed .size directive
        Directive::Size(args.trim().to_string(), SizeExpr::Absolute(0))
    }
}

/// Parse a comma-separated list of data values for .byte/.short/.long/.quad.
/// Each value can be an integer, a symbol reference, or a symbol difference.
fn parse_data_values(args: &str) -> Result<Vec<DataValue>, String> {
    let mut values = Vec::new();
    for part in args.split(',') {
        let trimmed = part.trim();
        if trimmed.is_empty() {
            continue;
        }
        values.push(parse_single_data_value(trimmed)?);
    }
    Ok(values)
}

/// Parse a single data value: integer, symbol, symbol+offset, or symbol_diff.
fn parse_single_data_value(s: &str) -> Result<DataValue, String> {
    let s = s.trim();
    if s.is_empty() {
        return Ok(DataValue::Integer(0));
    }

    // First check if this could be a symbol difference: A - B
    // The first char must be a symbol-start character (letter, _, .)
    let first = s.chars().next().unwrap();
    if first.is_ascii_alphabetic() || first == '_' || first == '.' {
        // Try to find a symbol difference expression
        if let Some(minus_pos) = find_symbol_diff_minus(s) {
            let sym_a_raw = s[..minus_pos].trim();
            let rest = s[minus_pos + 1..].trim();

            // rest might be "B" or "B + offset"
            let (sym_b, addend) = if let Some(plus_pos) = rest.find('+') {
                let b = rest[..plus_pos].trim();
                let add_str = rest[plus_pos + 1..].trim();
                let add_val: i64 = add_str.parse().unwrap_or(0);
                (b, add_val)
            } else {
                (rest, 0i64)
            };

            // Verify sym_b looks like a symbol
            if !sym_b.is_empty() {
                let b_first = sym_b.chars().next().unwrap();
                if b_first.is_ascii_alphabetic() || b_first == '_' || b_first == '.' {
                    return Ok(DataValue::SymbolDiff {
                        sym_a: sym_a_raw.to_string(),
                        sym_b: sym_b.to_string(),
                        addend,
                    });
                }
            }
        }

        // Not a symbol diff — parse as symbol reference with optional addend
        let (sym, addend) = parse_symbol_addend(s);
        return Ok(DataValue::Symbol { name: sym, addend });
    }

    // Try to parse as integer
    parse_data_value_int(s).map(DataValue::Integer)
}

/// Parse a data value as a plain integer (used by .byte, .zero fill, etc).
fn parse_data_value_int(s: &str) -> Result<i64, String> {
    let s = s.trim();
    if s.is_empty() {
        return Ok(0);
    }
    asm_expr::parse_integer_expr(s)
}

/// Find the position of the '-' operator in a symbol difference expression
/// like "sym_a - sym_b". Returns None if no valid symbol difference is found.
fn find_symbol_diff_minus(expr: &str) -> Option<usize> {
    let bytes = expr.as_bytes();
    let len = bytes.len();
    let mut i = 1; // Start at 1 to skip leading '-' (negative)

    while i < len {
        if bytes[i] == b'-' {
            let left_char = bytes[i - 1];
            let left_ok = left_char.is_ascii_alphanumeric() || left_char == b'_' || left_char == b'.' || left_char == b' ';

            let right_start = expr[i + 1..].trim_start();
            if !right_start.is_empty() {
                let right_char = right_start.as_bytes()[0];
                let right_ok = right_char.is_ascii_alphabetic() || right_char == b'_' || right_char == b'.';
                if left_ok && right_ok {
                    return Some(i);
                }
            }
        }
        i += 1;
    }

    None
}

fn parse_symbol_addend(s: &str) -> (String, i64) {
    if let Some(plus_pos) = s.find('+') {
        let sym = s[..plus_pos].trim().to_string();
        let off: i64 = s[plus_pos + 1..].trim().parse().unwrap_or(0);
        (sym, off)
    } else if let Some(minus_pos) = s.find('-') {
        if minus_pos > 0 {
            let sym = s[..minus_pos].trim().to_string();
            let off_str = &s[minus_pos..];
            let off: i64 = off_str.parse().unwrap_or(0);
            (sym, off)
        } else {
            (s.to_string(), 0)
        }
    } else {
        (s.to_string(), 0)
    }
}

fn parse_instruction(line: &str) -> Result<AsmStatement, String> {
    // Split mnemonic from operands
    let (mnemonic, operands_str) = if let Some(space_pos) = line.find([' ', '\t']) {
        (&line[..space_pos], line[space_pos..].trim())
    } else {
        (line, "")
    };

    let mnemonic = mnemonic.to_lowercase();
    let operands = parse_operands(operands_str, &mnemonic)?;

    Ok(AsmStatement::Instruction {
        mnemonic,
        operands,
        raw_operands: operands_str.to_string(),
    })
}

/// Determine which operand positions for a given instruction mnemonic must be
/// parsed as symbols rather than registers. This prevents function/variable
/// names that happen to match register names (e.g., `f1`, `a0`, `ra`, `s1`)
/// from being misclassified.
fn symbol_operand_mask(mnemonic: &str) -> u8 {
    match mnemonic {
        // call <symbol> — operand 0 is always a symbol
        "call" | "tail" => 0b0000_0001,
        // la/lla rd, <symbol> — operand 1 is always a symbol
        "la" | "lla" => 0b0000_0010,
        // jump <label>, <temp_reg> — operand 0 is always a symbol
        "jump" => 0b0000_0001,
        _ => 0,
    }
}

/// Parse an operand list separated by commas.
/// `mnemonic`: the instruction mnemonic, used for context-sensitive parsing
/// (e.g., fence operands, symbol-position disambiguation).
fn parse_operands(s: &str, mnemonic: &str) -> Result<Vec<Operand>, String> {
    if s.is_empty() {
        return Ok(Vec::new());
    }

    // Only classify FenceArg operands when the instruction is actually "fence".
    // Otherwise, single-letter variable names like "i", "o", "r", "w" would be
    // misclassified as fence arguments (e.g., "lla t0, i" for a global named "i").
    let is_fence = mnemonic == "fence";
    let sym_mask = symbol_operand_mask(mnemonic);

    let mut operands = Vec::new();
    let mut current = String::new();
    let mut paren_depth = 0;
    let mut operand_idx: u8 = 0;

    for ch in s.chars() {
        match ch {
            '(' => {
                paren_depth += 1;
                current.push('(');
            }
            ')' => {
                paren_depth -= 1;
                current.push(')');
            }
            ',' if paren_depth == 0 => {
                let force_symbol = (sym_mask & (1 << operand_idx)) != 0;
                let op = parse_single_operand(current.trim(), is_fence, force_symbol)?;
                operands.push(op);
                current.clear();
                operand_idx = operand_idx.saturating_add(1);
            }
            _ => {
                current.push(ch);
            }
        }
    }

    // Last operand
    let trimmed = current.trim().to_string();
    if !trimmed.is_empty() {
        let force_symbol = (sym_mask & (1 << operand_idx)) != 0;
        let op = parse_single_operand(&trimmed, is_fence, force_symbol)?;
        operands.push(op);
    }

    Ok(operands)
}

/// Parse a single operand.
/// `is_fence`: when true, classify subsets of "iorw" as FenceArg.
/// `force_symbol`: when true, this operand position is known to be a symbol
/// (e.g., the target of `call` or `tail`), so skip the register check. This
/// prevents function names like `f1`, `a0`, `ra` from being misclassified
/// as register operands.
fn parse_single_operand(s: &str, is_fence: bool, force_symbol: bool) -> Result<Operand, String> {
    let s = s.trim();
    if s.is_empty() {
        return Err("empty operand".to_string());
    }

    // Memory operand: offset(base) e.g., 8(sp), -16(s0), 0(a0)
    // Also handles: %lo(sym)(reg), %hi(sym)
    if !force_symbol {
        if let Some(result) = try_parse_memory_operand(s) {
            return Ok(result);
        }
    }

    // %hi(symbol) or %lo(symbol) - used as immediates in lui/addi
    if s.starts_with("%hi(") || s.starts_with("%lo(") || s.starts_with("%pcrel_hi(")
        || s.starts_with("%pcrel_lo(") || s.starts_with("%tprel_hi(")
        || s.starts_with("%tprel_lo(") || s.starts_with("%tprel_add(")
        || s.starts_with("%got_pcrel_hi(") || s.starts_with("%tls_ie_pcrel_hi(")
        || s.starts_with("%tls_gd_pcrel_hi(")
    {
        return Ok(Operand::Symbol(s.to_string()));
    }

    // Fence operands: iorw, ior, iow, etc.
    // Only classify as FenceArg when we're actually parsing a "fence" instruction,
    // to avoid misclassifying single-letter symbol names like "i", "o", "r", "w".
    if is_fence && is_fence_arg(s) {
        return Ok(Operand::FenceArg(s.to_string()));
    }

    // Rounding modes
    if !force_symbol && is_rounding_mode(s) {
        return Ok(Operand::RoundingMode(s.to_string()));
    }

    // Register — skip when this operand position is known to be a symbol
    if !force_symbol && is_register(s) {
        return Ok(Operand::Reg(s.to_string()));
    }

    // Try to parse as immediate (only when not forcing symbol)
    if !force_symbol {
        if let Ok(val) = parse_int_literal(s) {
            return Ok(Operand::Imm(val));
        }
    }

    // Symbol with offset: sym+offset or sym-offset
    if let Some(plus_pos) = s.find('+') {
        let sym = &s[..plus_pos];
        let off_str = &s[plus_pos + 1..];
        if let Ok(off) = parse_int_literal(off_str) {
            return Ok(Operand::SymbolOffset(sym.to_string(), off));
        }
    }
    if let Some(minus_pos) = s.rfind('-') {
        if minus_pos > 0 {
            let sym = &s[..minus_pos];
            let off_str = &s[minus_pos..];
            if let Ok(off) = parse_int_literal(off_str) {
                return Ok(Operand::SymbolOffset(sym.to_string(), off));
            }
        }
    }

    // Plain symbol/label
    Ok(Operand::Symbol(s.to_string()))
}

/// Try to parse a memory operand like `offset(reg)` or `%lo(sym)(reg)`.
fn try_parse_memory_operand(s: &str) -> Option<Operand> {
    // Look for the pattern: something(reg)
    // But be careful about nested parens like %lo(sym)(reg)

    let last_close = s.rfind(')')?;
    if last_close != s.len() - 1 {
        return None;
    }

    // Find the matching open paren for the last close paren
    let mut depth = 0;
    let mut last_open = None;
    for (i, c) in s.char_indices().rev() {
        match c {
            ')' => depth += 1,
            '(' => {
                depth -= 1;
                if depth == 0 {
                    last_open = Some(i);
                    break;
                }
            }
            _ => {}
        }
    }

    let open_pos = last_open?;
    let base = s[open_pos + 1..last_close].trim();
    let offset_part = s[..open_pos].trim();

    // Verify the base is a register
    if !is_register(base) {
        return None;
    }

    // Check if offset_part is a %lo/%hi modifier
    if offset_part.starts_with('%') {
        return Some(Operand::MemSymbol {
            base: base.to_string(),
            symbol: offset_part.to_string(),
            modifier: String::new(),
        });
    }

    // Try to parse offset as integer
    if offset_part.is_empty() {
        return Some(Operand::Mem {
            base: base.to_string(),
            offset: 0,
        });
    }

    if let Ok(offset) = parse_int_literal(offset_part) {
        return Some(Operand::Mem {
            base: base.to_string(),
            offset,
        });
    }

    // If offset_part is a symbol reference
    Some(Operand::MemSymbol {
        base: base.to_string(),
        symbol: offset_part.to_string(),
        modifier: String::new(),
    })
}

fn is_fence_arg(s: &str) -> bool {
    // Fence ordering: combinations of i, o, r, w
    let s = s.to_lowercase();
    if s.is_empty() || s.len() > 4 {
        return false;
    }
    s.chars().all(|c| matches!(c, 'i' | 'o' | 'r' | 'w'))
}

fn is_rounding_mode(s: &str) -> bool {
    matches!(s.to_lowercase().as_str(), "rne" | "rtz" | "rdn" | "rup" | "rmm" | "dyn")
}

/// Check if a string is a valid RISC-V register name.
pub fn is_register(s: &str) -> bool {
    let s = s.to_lowercase();

    // ABI names
    matches!(s.as_str(),
        "zero" | "ra" | "sp" | "gp" | "tp"
        | "t0" | "t1" | "t2" | "t3" | "t4" | "t5" | "t6"
        | "s0" | "s1" | "s2" | "s3" | "s4" | "s5" | "s6"
        | "s7" | "s8" | "s9" | "s10" | "s11"
        | "a0" | "a1" | "a2" | "a3" | "a4" | "a5" | "a6" | "a7"
        | "fp"  // alias for s0
    ) ||
    // x0-x31
    (s.starts_with('x') && s.len() >= 2 && {
        if let Ok(n) = s[1..].parse::<u32>() { n <= 31 } else { false }
    }) ||
    // Floating-point ABI names
    matches!(s.as_str(),
        "ft0" | "ft1" | "ft2" | "ft3" | "ft4" | "ft5" | "ft6" | "ft7"
        | "ft8" | "ft9" | "ft10" | "ft11"
        | "fs0" | "fs1" | "fs2" | "fs3" | "fs4" | "fs5" | "fs6" | "fs7"
        | "fs8" | "fs9" | "fs10" | "fs11"
        | "fa0" | "fa1" | "fa2" | "fa3" | "fa4" | "fa5" | "fa6" | "fa7"
    ) ||
    // f0-f31
    (s.starts_with('f') && !s.starts_with("ft") && !s.starts_with("fs") && !s.starts_with("fa")
        && s.len() >= 2 && {
        if let Ok(n) = s[1..].parse::<u32>() { n <= 31 } else { false }
    })
}

pub fn parse_int_literal(s: &str) -> Result<i64, String> {
    let s = s.trim();
    if s.is_empty() {
        return Err("empty integer literal".to_string());
    }

    // Use the shared expression evaluator which handles parentheses,
    // operator precedence, bitwise ops (|, &, ^, <<, >>), and arithmetic.
    asm_expr::parse_integer_expr(s)
}
