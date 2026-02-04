//! AArch64 assembly parser.
//!
//! Parses the textual assembly format emitted by our AArch64 codegen into
//! structured `AsmStatement` values. The parser handles:
//! - Labels (global and local)
//! - Directives (.section, .globl, .type, .align, .byte, .long, .xword, etc.)
//! - AArch64 instructions (mov, add, sub, ldr, str, bl, ret, etc.)
//! - CFI directives (passed through as-is for DWARF unwind info)

#![allow(dead_code)]

use crate::backend::asm_expr;
use crate::backend::elf;

/// A parsed assembly operand.
#[derive(Debug, Clone)]
pub enum Operand {
    /// Register: x0-x30, w0-w30, sp, xzr, wzr, d0-d31, s0-s31, q0-q31, v0-v31
    Reg(String),
    /// Immediate value: #42, #-1, #0x1000
    Imm(i64),
    /// Symbol reference: function name, label, etc.
    Symbol(String),
    /// Symbol with addend: symbol+offset or symbol-offset
    SymbolOffset(String, i64),
    /// Memory operand: [base] or [base, #offset]
    Mem { base: String, offset: i64 },
    /// Memory operand with pre-index writeback: [base, #offset]!
    MemPreIndex { base: String, offset: i64 },
    /// Memory operand with post-index writeback: [base], #offset
    MemPostIndex { base: String, offset: i64 },
    /// Memory operand with register offset: [base, Xm]
    MemRegOffset { base: String, index: String, extend: Option<String>, shift: Option<u8> },
    /// :lo12:symbol or :got_lo12:symbol modifier
    Modifier { kind: String, symbol: String },
    /// :lo12:symbol+offset
    ModifierOffset { kind: String, symbol: String, offset: i64 },
    /// Shift: lsl #N, lsr #N, asr #N
    Shift { kind: String, amount: u32 },
    /// Extend: sxtw, uxtw, sxtx, etc. with optional shift amount
    Extend { kind: String, amount: u32 },
    /// Condition code for csel etc.: eq, ne, lt, gt, ...
    Cond(String),
    /// Barrier option for dmb/dsb: ish, ishld, ishst, sy, etc.
    Barrier(String),
    /// Label reference for branches
    Label(String),
    /// Raw expression (for things we can't fully parse yet)
    Expr(String),
    /// NEON register with arrangement specifier: v0.8b, v0.16b, v0.4s, etc.
    RegArrangement { reg: String, arrangement: String },
    /// NEON register with lane index: v0.d[1], v0.b[0], v0.s[2], etc.
    RegLane { reg: String, elem_size: String, index: u32 },
    /// NEON register list: {v0.16b}, {v0.16b, v1.16b}, etc.
    RegList(Vec<Operand>),
    /// NEON register list with element index: {v0.s, v1.s}[0], {v0.d, v1.d}[1], etc.
    RegListIndexed { regs: Vec<Operand>, index: u32 },
}

/// Section directive with optional flags and type.
#[derive(Debug, Clone)]
pub struct SectionDirective {
    pub name: String,
    pub flags: Option<String>,
    pub section_type: Option<String>,
}

/// Symbol kind from `.type` directive.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    Object,
    TlsObject,
    NoType,
}

/// Size expression: either a constant or `.-name` (current position minus symbol).
#[derive(Debug, Clone)]
pub enum SizeExpr {
    Constant(u64),
    CurrentMinusSymbol(String),
}

/// A data value that can be a constant, a symbol, or a symbol expression.
#[derive(Debug, Clone)]
pub enum DataValue {
    Integer(i64),
    Symbol(String),
    /// symbol + offset (e.g., `.quad func+128`)
    SymbolOffset(String, i64),
    /// symbol - symbol (e.g., `.long .LBB3 - .Ljt_0`)
    SymbolDiff(String, String),
    /// symbol - symbol + addend
    SymbolDiffAddend(String, String, i64),
}

/// A typed assembly directive, fully parsed at parse time.
#[derive(Debug, Clone)]
pub enum AsmDirective {
    /// Switch to a named section: `.section .text,"ax",@progbits`
    Section(SectionDirective),
    /// Global symbol: `.globl name`
    Global(String),
    /// Weak symbol: `.weak name`
    Weak(String),
    /// Hidden visibility: `.hidden name`
    Hidden(String),
    /// Protected visibility: `.protected name`
    Protected(String),
    /// Internal visibility: `.internal name`
    Internal(String),
    /// Symbol type: `.type name, %function`
    SymbolType(String, SymbolKind),
    /// Symbol size: `.size name, expr`
    Size(String, SizeExpr),
    /// Alignment: `.align N` or `.p2align N` (stored as byte count, already converted from 2^N)
    Align(u64),
    /// Byte-alignment: `.balign N` (stored as byte count directly)
    Balign(u64),
    /// Emit bytes: `.byte val, val, ...`
    Byte(Vec<u8>),
    /// Emit 16-bit values: `.short val, ...`
    Short(Vec<i16>),
    /// Emit 32-bit values: `.long val, ...` (can be symbol references)
    Long(Vec<DataValue>),
    /// Emit 64-bit values: `.quad val, ...` (can be symbol references)
    Quad(Vec<DataValue>),
    /// Emit zero bytes: `.zero N[, fill]`
    Zero(usize, u8),
    /// NUL-terminated string: `.asciz "str"`
    Asciz(Vec<u8>),
    /// String without NUL: `.ascii "str"`
    Ascii(Vec<u8>),
    /// Common symbol: `.comm name, size, align`
    Comm(String, u64, u64),
    /// Local symbol: `.local name`
    Local(String),
    /// Symbol alias: `.set name, value`
    Set(String, String),
    /// Push current section and switch to a new one: `.pushsection name,"flags",@type`
    PushSection(SectionDirective),
    /// Pop section stack and restore previous section: `.popsection`
    PopSection,
    /// CFI directive (ignored for code generation)
    Cfi,
    /// Other ignored directives (.file, .loc, .ident, etc.)
    Ignored,
}

/// A parsed assembly statement.
#[derive(Debug, Clone)]
pub enum AsmStatement {
    /// A label definition: "name:"
    Label(String),
    /// A typed directive, fully parsed
    Directive(AsmDirective),
    /// An AArch64 instruction with mnemonic and operands
    Instruction {
        mnemonic: String,
        operands: Vec<Operand>,
        /// The raw text of the operand string (for fallback encoding)
        raw_operands: String,
    },
    /// An empty line or comment
    Empty,
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
    // Pre-process: strip C-style /* ... */ comments
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

        // Strip comments (// style)
        let line = strip_comment(line);
        let line = line.trim();
        if line.is_empty() {
            statements.push(AsmStatement::Empty);
            continue;
        }

        // Skip C preprocessor line markers: # <number> "filename" [flags...]
        // These are emitted when .S files are preprocessed before assembly.
        if line.starts_with("# ") {
            let rest = line[2..].trim_start();
            if rest.bytes().next().map_or(false, |b| b.is_ascii_digit()) {
                statements.push(AsmStatement::Empty);
                continue;
            }
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

fn strip_comment(line: &str) -> &str {
    // Scan character by character, tracking string state to find comments
    // outside of string literals. This correctly handles escaped quotes (\")
    // inside strings (e.g. .asciz "a\"b//c" should not strip at //).
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
        // Check for // comment
        if bytes[i] == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
            return &line[..i];
        }
        // Check for @ comment (GAS ARM comment character)
        if bytes[i] == b'@' {
            let after = &line[i + 1..];
            if !after.starts_with("object")
                && !after.starts_with("function")
                && !after.starts_with("progbits")
                && !after.starts_with("nobits")
                && !after.starts_with("tls_object")
                && !after.starts_with("note")
            {
                return &line[..i];
            }
        }
        i += 1;
    }
    line
}

fn parse_line(line: &str) -> Result<Vec<AsmStatement>, String> {
    // Check for label definition (name:)
    // Labels can be at the start of the line, possibly followed by an instruction
    if let Some(colon_pos) = line.find(':') {
        let potential_label = &line[..colon_pos].trim();
        // Verify it looks like a valid label (no spaces before colon, alphanumeric + _ + .)
        if !potential_label.is_empty()
            && !potential_label.contains(' ')
            && !potential_label.contains('\t')
            && !potential_label.starts_with('.')  // Could be a directive
            || potential_label.starts_with(".L") // Local labels start with .L
            || potential_label.starts_with(".Lstr") // String labels
            || potential_label.starts_with(".Lmemcpy")
            || potential_label.starts_with(".Lskip")
        {
            // Check if this is actually a directive like ".section .rodata"
            if potential_label.starts_with('.')
                && !potential_label.starts_with(".L")
                && !potential_label.starts_with(".l")
            {
                // This is a directive, not a label
            } else {
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

fn parse_directive(line: &str) -> Result<AsmStatement, String> {
    // Split directive name from arguments
    let (name, args) = if let Some(space_pos) = line.find([' ', '\t']) {
        let name = &line[..space_pos];
        let args = line[space_pos..].trim();
        (name, args)
    } else {
        (line, "")
    };

    let dir = match name {
        ".section" => parse_section_directive(args)?,
        ".text" => AsmDirective::Section(SectionDirective {
            name: ".text".to_string(),
            flags: None,
            section_type: None,
        }),
        ".data" => AsmDirective::Section(SectionDirective {
            name: ".data".to_string(),
            flags: None,
            section_type: None,
        }),
        ".bss" => AsmDirective::Section(SectionDirective {
            name: ".bss".to_string(),
            flags: None,
            section_type: None,
        }),
        ".rodata" => AsmDirective::Section(SectionDirective {
            name: ".rodata".to_string(),
            flags: None,
            section_type: None,
        }),
        ".globl" | ".global" => AsmDirective::Global(args.trim().to_string()),
        ".weak" => AsmDirective::Weak(args.trim().to_string()),
        ".hidden" => AsmDirective::Hidden(args.trim().to_string()),
        ".protected" => AsmDirective::Protected(args.trim().to_string()),
        ".internal" => AsmDirective::Internal(args.trim().to_string()),
        ".type" => parse_type_directive(args)?,
        ".size" => parse_size_directive(args)?,
        ".align" | ".p2align" => {
            let align_val: u64 = args.trim().split(',').next()
                .and_then(|s| s.trim().parse().ok())
                .unwrap_or(0);
            // AArch64 .align N means 2^N bytes (same as .p2align)
            AsmDirective::Align(1u64 << align_val)
        }
        ".balign" => {
            let align_val: u64 = args.trim().parse().unwrap_or(1);
            AsmDirective::Balign(align_val)
        }
        ".byte" => {
            let mut vals = Vec::new();
            for part in args.split(',') {
                let val = parse_data_value(part.trim())? as u8;
                vals.push(val);
            }
            AsmDirective::Byte(vals)
        }
        ".short" | ".hword" | ".2byte" | ".half" => {
            let mut vals = Vec::new();
            for part in args.split(',') {
                let val = parse_data_value(part.trim())? as i16;
                vals.push(val);
            }
            AsmDirective::Short(vals)
        }
        ".long" | ".4byte" | ".word" => {
            let vals = parse_data_values(args)?;
            AsmDirective::Long(vals)
        }
        ".quad" | ".8byte" | ".xword" | ".dword" => {
            let vals = parse_data_values(args)?;
            AsmDirective::Quad(vals)
        }
        ".zero" | ".space" => {
            let parts: Vec<&str> = args.trim().split(',').collect();
            let size: usize = parts[0].trim().parse()
                .map_err(|_| format!("invalid .zero size: {}", args))?;
            let fill: u8 = if parts.len() > 1 {
                parse_data_value(parts[1].trim())? as u8
            } else {
                0
            };
            AsmDirective::Zero(size, fill)
        }
        ".asciz" | ".string" => {
            let s = elf::parse_string_literal(args)?;
            let mut bytes = s;
            bytes.push(0); // null terminator
            AsmDirective::Asciz(bytes)
        }
        ".ascii" => {
            let s = elf::parse_string_literal(args)?;
            AsmDirective::Ascii(s)
        }
        ".comm" => parse_comm_directive(args)?,
        ".local" => AsmDirective::Local(args.trim().to_string()),
        ".set" | ".equ" => {
            let parts: Vec<&str> = args.splitn(2, ',').collect();
            if parts.len() == 2 {
                AsmDirective::Set(
                    parts[0].trim().to_string(),
                    parts[1].trim().to_string(),
                )
            } else {
                return Err(format!("malformed .set directive: expected 'name, value', got '{}'", args));
            }
        }
        // CFI directives
        ".cfi_startproc" | ".cfi_endproc" | ".cfi_def_cfa_offset"
        | ".cfi_offset" | ".cfi_def_cfa_register" | ".cfi_restore"
        | ".cfi_remember_state" | ".cfi_restore_state"
        | ".cfi_adjust_cfa_offset" | ".cfi_def_cfa"
        | ".cfi_sections" | ".cfi_personality" | ".cfi_lsda"
        | ".cfi_rel_offset" | ".cfi_register" | ".cfi_return_column"
        | ".cfi_undefined" | ".cfi_same_value" | ".cfi_escape" => AsmDirective::Cfi,
        ".pushsection" => {
            // .pushsection name,"flags",@type - same syntax as .section
            match parse_section_directive(args)? {
                AsmDirective::Section(dir) => AsmDirective::PushSection(dir),
                _ => AsmDirective::Ignored,
            }
        }
        ".popsection" | ".previous" => AsmDirective::PopSection,
        // Other directives we can safely ignore
        ".file" | ".loc" | ".ident" | ".addrsig" | ".addrsig_sym"
        | ".build_attributes" | ".eabi_attribute"
        | ".arch" | ".arch_extension" => AsmDirective::Ignored,
        _ => {
            return Err(format!("unsupported AArch64 assembler directive: {} {}", name, args));
        }
    };

    Ok(AsmStatement::Directive(dir))
}

fn parse_instruction(line: &str) -> Result<AsmStatement, String> {
    // Split mnemonic from operands
    let (mnemonic, operands_str) = if let Some(space_pos) = line.find([' ', '\t']) {
        (&line[..space_pos], line[space_pos..].trim())
    } else {
        (line, "")
    };

    let mnemonic = mnemonic.to_lowercase();
    let operands = parse_operands(operands_str)?;

    Ok(AsmStatement::Instruction {
        mnemonic,
        operands,
        raw_operands: operands_str.to_string(),
    })
}

/// Parse an operand list separated by commas, handling brackets and nested expressions.
fn parse_operands(s: &str) -> Result<Vec<Operand>, String> {
    if s.is_empty() {
        return Ok(Vec::new());
    }

    let mut operands = Vec::new();
    let mut current = String::new();
    let mut bracket_depth = 0;
    let mut brace_depth = 0;

    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        match chars[i] {
            '{' => {
                brace_depth += 1;
                current.push('{');
            }
            '}' => {
                brace_depth -= 1;
                current.push('}');
            }
            '[' => {
                bracket_depth += 1;
                current.push('[');
            }
            ']' => {
                bracket_depth -= 1;
                current.push(']');
                // Check for '!' (pre-index writeback)
                if i + 1 < chars.len() && chars[i + 1] == '!' {
                    current.push('!');
                    i += 1;
                }
            }
            ',' if bracket_depth == 0 && brace_depth == 0 => {
                let op = parse_single_operand(current.trim())?;
                operands.push(op);
                current.clear();
            }
            _ => {
                current.push(chars[i]);
            }
        }
        i += 1;
    }

    // Last operand
    let trimmed = current.trim().to_string();
    if !trimmed.is_empty() {
        let op = parse_single_operand(&trimmed)?;
        operands.push(op);
    }

    // Handle memory operands with post-index: [base], #offset
    // This looks like two operands: Mem{base, 0} and Imm(offset)
    // We need to merge them into MemPostIndex
    let mut merged = Vec::new();
    let mut skip_next = false;
    for j in 0..operands.len() {
        if skip_next {
            skip_next = false;
            continue;
        }
        if j + 1 < operands.len() {
            if let (Operand::Mem { base, offset: 0 }, Operand::Imm(off)) = (&operands[j], &operands[j + 1]) {
                merged.push(Operand::MemPostIndex { base: base.clone(), offset: *off });
                skip_next = true;
                continue;
            }
        }
        merged.push(operands[j].clone());
    }

    Ok(merged)
}

fn parse_single_operand(s: &str) -> Result<Operand, String> {
    let s = s.trim();
    if s.is_empty() {
        return Err("empty operand".to_string());
    }

    // Register list: {v0.16b}, {v0.16b, v1.16b}, etc.
    // Register list with optional element index: {v0.s, v1.s}[0]
    if s.starts_with('{') {
        if s.ends_with('}') {
            return parse_register_list(s);
        }
        // Check for {regs}[index] form
        if let Some(close_brace) = s.find('}') {
            let rest = s[close_brace + 1..].trim();
            if rest.starts_with('[') && rest.ends_with(']') {
                let idx_str = &rest[1..rest.len() - 1];
                if let Ok(idx) = idx_str.parse::<u32>() {
                    let list_str = &s[..close_brace + 1];
                    let inner = &list_str[1..list_str.len() - 1];
                    let mut regs = Vec::new();
                    for part in inner.split(',') {
                        let part = part.trim();
                        if !part.is_empty() {
                            let op = parse_single_operand(part)?;
                            regs.push(op);
                        }
                    }
                    if regs.is_empty() {
                        return Err("empty register list".to_string());
                    }
                    return Ok(Operand::RegListIndexed { regs, index: idx });
                }
            }
        }
    }

    // Memory operand: [base, #offset]! (pre-index) or [base, #offset] or [base]
    if s.starts_with('[') {
        return parse_memory_operand(s);
    }

    // Immediate: #value
    if let Some(rest) = s.strip_prefix('#') {
        return parse_immediate(rest);
    }

    // :modifier:symbol
    if s.starts_with(':') {
        return parse_modifier(s);
    }

    // Shift: lsl, lsr, asr, ror
    let lower = s.to_lowercase();
    if lower.starts_with("lsl ") || lower.starts_with("lsr ") || lower.starts_with("asr ") || lower.starts_with("ror ") {
        let kind = &lower[..3];
        let amount_str = s[4..].trim();
        let amount = if let Some(stripped) = amount_str.strip_prefix('#') {
            parse_int_literal(stripped)?
        } else {
            parse_int_literal(amount_str)?
        };
        return Ok(Operand::Shift { kind: kind.to_string(), amount: amount as u32 });
    }

    // Extend specifiers: sxtw, uxtw, sxtx, uxtx, sxth, uxth, sxtb, uxtb
    // May appear alone (sxtw) or with shift (sxtw #2)
    {
        let extend_prefixes = ["sxtw", "sxtx", "sxth", "sxtb", "uxtw", "uxtx", "uxth", "uxtb"];
        for prefix in &extend_prefixes {
            if lower == *prefix {
                return Ok(Operand::Extend { kind: prefix.to_string(), amount: 0 });
            }
            if lower.starts_with(prefix) && lower.as_bytes().get(prefix.len()) == Some(&b' ') {
                let amount_str = s[prefix.len()..].trim();
                let amount = if let Some(stripped) = amount_str.strip_prefix('#') {
                    parse_int_literal(stripped)?
                } else {
                    parse_int_literal(amount_str)?
                };
                return Ok(Operand::Extend { kind: prefix.to_string(), amount: amount as u32 });
            }
        }
    }

    // Barrier options
    match lower.as_str() {
        "ish" | "ishld" | "ishst" | "sy" | "ld" | "st" | "osh" | "oshld" | "oshst"
        | "nsh" | "nshld" | "nshst" => {
            return Ok(Operand::Barrier(lower));
        }
        _ => {}
    }

    // Condition codes (for csel, csinc, etc.)
    match lower.as_str() {
        "eq" | "ne" | "cs" | "hs" | "cc" | "lo" | "mi" | "pl" | "vs" | "vc"
        | "hi" | "ls" | "ge" | "lt" | "gt" | "le" | "al" | "nv" => {
            return Ok(Operand::Cond(lower));
        }
        _ => {}
    }

    // NEON register with lane index: v0.d[1], v0.b[0], v0.s[2], etc.
    if let Some(dot_pos) = s.find('.') {
        let reg_part = &s[..dot_pos];
        let arr_part = &s[dot_pos + 1..];
        if is_register(reg_part) {
            if let Some(bracket_pos) = arr_part.find('[') {
                if arr_part.ends_with(']') {
                    let elem_size = arr_part[..bracket_pos].to_lowercase();
                    let idx_str = &arr_part[bracket_pos + 1..arr_part.len() - 1];
                    if let Ok(idx) = idx_str.parse::<u32>() {
                        if matches!(elem_size.as_str(), "b" | "h" | "s" | "d") {
                            return Ok(Operand::RegLane {
                                reg: reg_part.to_string(),
                                elem_size,
                                index: idx,
                            });
                        }
                    }
                }
            }
        }
    }

    // NEON register with arrangement: v0.8b, v0.16b, v0.4s, v0.2d, etc.
    if let Some(dot_pos) = s.find('.') {
        let reg_part = &s[..dot_pos];
        let arr_part = &s[dot_pos + 1..];
        if is_register(reg_part) {
            let arr_lower = arr_part.to_lowercase();
            if matches!(arr_lower.as_str(), "8b" | "16b" | "4h" | "8h" | "2s" | "4s" | "1d" | "2d" | "1q"
                | "b" | "h" | "s" | "d") {
                return Ok(Operand::RegArrangement {
                    reg: reg_part.to_string(),
                    arrangement: arr_lower,
                });
            }
        }
    }

    // Register
    if is_register(s) {
        return Ok(Operand::Reg(s.to_string()));
    }

    // Bare integer (without # prefix) - some inline asm constraints emit these
    // e.g., "eor w9, w10, 255" or "ccmp x10, x13, 0, eq"
    if s.chars().next().is_some_and(|c| c.is_ascii_digit()) {
        if let Ok(val) = parse_int_literal(s) {
            return Ok(Operand::Imm(val));
        }
    }

    // Label/symbol reference (for branches, adrp, etc.)
    // Could be: .LBB42, func_name, symbol+offset
    if let Some(plus_pos) = s.find('+') {
        let sym = &s[..plus_pos];
        let off_str = &s[plus_pos + 1..];
        if let Ok(off) = parse_int_literal(off_str) {
            return Ok(Operand::SymbolOffset(sym.to_string(), off));
        }
    }
    if let Some(minus_pos) = s.find('-') {
        // Careful: don't confuse with label names containing '-' in label diff expressions
        if minus_pos > 0 {
            let sym = &s[..minus_pos];
            let off_str = &s[minus_pos..]; // includes the '-'
            if let Ok(off) = parse_int_literal(off_str) {
                return Ok(Operand::SymbolOffset(sym.to_string(), off));
            }
        }
    }

    // Plain symbol/label
    Ok(Operand::Symbol(s.to_string()))
}

/// Parse a register list like {v0.16b} or {v0.16b, v1.16b, v2.16b, v3.16b}
fn parse_register_list(s: &str) -> Result<Operand, String> {
    let inner = &s[1..s.len() - 1]; // strip { and }
    let mut regs = Vec::new();
    for part in inner.split(',') {
        let part = part.trim();
        if !part.is_empty() {
            let op = parse_single_operand(part)?;
            regs.push(op);
        }
    }
    if regs.is_empty() {
        return Err("empty register list".to_string());
    }
    Ok(Operand::RegList(regs))
}

fn parse_memory_operand(s: &str) -> Result<Operand, String> {
    let has_writeback = s.ends_with('!');
    let inner = if has_writeback {
        &s[1..s.len() - 2] // strip [ and ]!
    } else {
        // Find the matching ]
        let end = s.find(']').ok_or("missing ] in memory operand")?;
        &s[1..end]
    };

    // Split on comma
    let parts: Vec<&str> = inner.splitn(2, ',').collect();
    let base = parts[0].trim().to_string();

    if parts.len() == 1 {
        // [base]
        if has_writeback {
            return Ok(Operand::MemPreIndex { base, offset: 0 });
        }
        return Ok(Operand::Mem { base, offset: 0 });
    }

    let second = parts[1].trim();

    // [base, #imm] or [base, imm] (bare immediate without # prefix)
    if let Some(imm_str) = second.strip_prefix('#') {
        let offset = parse_int_literal(imm_str)?;
        if has_writeback {
            return Ok(Operand::MemPreIndex { base, offset });
        }
        return Ok(Operand::Mem { base, offset });
    }

    // Handle bare immediate without # prefix (e.g., [sp, -16]! or [x0, 8])
    // Check if the second operand starts with a digit or minus sign followed by a digit
    if second.starts_with('-') || second.starts_with('+') || second.bytes().next().map_or(false, |b| b.is_ascii_digit()) {
        if let Ok(offset) = parse_int_literal(second) {
            if has_writeback {
                return Ok(Operand::MemPreIndex { base, offset });
            }
            return Ok(Operand::Mem { base, offset });
        }
    }

    // [base, :lo12:symbol]
    if second.starts_with(':') {
        // Parse the modifier embedded in memory operand
        // The ] is already stripped, so just parse the modifier
        let mod_op = parse_modifier(second)?;
        // Return a special memory operand - we'll handle this in the encoder
        // For now, return it as a reg+symbol form
        match mod_op {
            Operand::Modifier { kind, symbol } => {
                return Ok(Operand::MemRegOffset {
                    base,
                    index: format!(":{}:{}", kind, symbol),
                    extend: None,
                    shift: None,
                });
            }
            Operand::ModifierOffset { kind, symbol, offset } => {
                return Ok(Operand::MemRegOffset {
                    base,
                    index: format!(":{}:{}+{}", kind, symbol, offset),
                    extend: None,
                    shift: None,
                });
            }
            _ => {}
        }
    }

    // [base, Xm] or [base, Xm, extend #shift]
    // second may be "x0" or "x0, lsl #2" or "w0, sxtw" or "w0, sxtw #2"
    let sub_parts: Vec<&str> = second.splitn(2, ',').collect();
    let index_str = sub_parts[0].trim();
    if is_register(index_str) {
        let (extend, shift) = if sub_parts.len() > 1 {
            parse_extend_shift(sub_parts[1].trim())
        } else {
            (None, None)
        };
        return Ok(Operand::MemRegOffset {
            base,
            index: index_str.to_string(),
            extend,
            shift,
        });
    }

    // Fallback: treat as register offset
    Ok(Operand::MemRegOffset {
        base,
        index: second.to_string(),
        extend: None,
        shift: None,
    })
}

/// Parse an extend/shift specifier like "lsl #2", "sxtw", "sxtw #0", "uxtx #3"
fn parse_extend_shift(s: &str) -> (Option<String>, Option<u8>) {
    let s = s.trim().to_lowercase();
    let parts: Vec<&str> = s.split_whitespace().collect();
    if parts.is_empty() {
        return (None, None);
    }
    let kind = parts[0];
    let shift = if parts.len() > 1 {
        let shift_str = parts[1].trim_start_matches('#');
        shift_str.parse::<u8>().ok()
    } else {
        None
    };
    match kind {
        "lsl" | "lsr" | "asr" | "ror" | "sxtw" | "sxtx" | "sxth" | "sxtb"
        | "uxtw" | "uxtx" | "uxth" | "uxtb" => {
            (Some(kind.to_string()), shift)
        }
        _ => (None, None),
    }
}

fn parse_modifier(s: &str) -> Result<Operand, String> {
    // :kind:symbol or :kind:symbol+offset
    let s = s.trim_start_matches(':');
    let colon_pos = s.find(':').ok_or("malformed modifier, expected :kind:symbol")?;
    let kind = s[..colon_pos].to_string();
    let rest = &s[colon_pos + 1..];

    // Check for symbol+offset
    if let Some(plus_pos) = rest.find('+') {
        let symbol = rest[..plus_pos].to_string();
        let offset_str = &rest[plus_pos + 1..];
        if let Ok(offset) = parse_int_literal(offset_str) {
            return Ok(Operand::ModifierOffset { kind, symbol, offset });
        }
    }

    Ok(Operand::Modifier { kind, symbol: rest.to_string() })
}

fn parse_immediate(s: &str) -> Result<Operand, String> {
    // Handle :modifier:symbol as immediate (e.g., #:lo12:symbol)
    if s.starts_with(':') {
        return parse_modifier(s);
    }

    let val = parse_int_literal(s)?;
    Ok(Operand::Imm(val))
}

fn parse_int_literal(s: &str) -> Result<i64, String> {
    let s = s.trim();
    if s.is_empty() {
        return Err("empty integer literal".to_string());
    }

    // Use the shared expression evaluator which handles parentheses,
    // operator precedence, bitwise ops, and arithmetic expressions.
    asm_expr::parse_integer_expr(s)
}

fn is_register(s: &str) -> bool {
    let s = s.to_lowercase();
    // General purpose: x0-x30, w0-w30
    if (s.starts_with('x') || s.starts_with('w')) && s.len() >= 2 {
        let num = &s[1..];
        if let Ok(n) = num.parse::<u32>() {
            return n <= 30;
        }
    }
    // Special registers
    matches!(s.as_str(),
        "sp" | "wsp" | "xzr" | "wzr" | "lr"
    )
    ||
    // FP/SIMD: d0-d31, s0-s31, q0-q31, v0-v31, h0-h31, b0-b31
    {
        if (s.starts_with('d') || s.starts_with('s') || s.starts_with('q')
            || s.starts_with('v') || s.starts_with('h') || s.starts_with('b'))
            && s.len() >= 2
        {
            let num = &s[1..];
            if let Ok(n) = num.parse::<u32>() {
                return n <= 31;
            }
        }
        false
    }
}

// ── Directive parsing helpers ──────────────────────────────────────────

/// Parse a `.section name,"flags",@type` directive.
fn parse_section_directive(args: &str) -> Result<AsmDirective, String> {
    let parts = split_section_args(args);
    let name = parts.first()
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|| ".text".to_string());
    let flags = parts.get(1).map(|s| s.trim().trim_matches('"').to_string());
    let section_type = parts.get(2).map(|s| s.trim().to_string());
    Ok(AsmDirective::Section(SectionDirective { name, flags, section_type }))
}

/// Split section directive args, respecting quoted strings.
fn split_section_args(s: &str) -> Vec<String> {
    let mut parts = Vec::new();
    let mut current = String::new();
    let mut in_quotes = false;
    for c in s.chars() {
        if c == '"' {
            in_quotes = !in_quotes;
            current.push(c);
        } else if c == ',' && !in_quotes {
            parts.push(current.clone());
            current.clear();
        } else {
            current.push(c);
        }
    }
    if !current.is_empty() {
        parts.push(current);
    }
    parts
}

/// Parse `.type name, %function` or `@object` etc.
fn parse_type_directive(args: &str) -> Result<AsmDirective, String> {
    let parts: Vec<&str> = args.splitn(2, ',').collect();
    if parts.len() != 2 {
        return Err(format!("malformed .type directive: expected 'name, type', got '{}'", args));
    }
    let sym = parts[0].trim().to_string();
    let kind_str = parts[1].trim();
    let kind = match kind_str {
        "%function" | "@function" => SymbolKind::Function,
        "%object" | "@object" => SymbolKind::Object,
        "@tls_object" => SymbolKind::TlsObject,
        _ => SymbolKind::NoType,
    };
    Ok(AsmDirective::SymbolType(sym, kind))
}

/// Parse `.size name, expr`.
fn parse_size_directive(args: &str) -> Result<AsmDirective, String> {
    let parts: Vec<&str> = args.splitn(2, ',').collect();
    if parts.len() != 2 {
        return Err(format!("malformed .size directive: expected 'name, expr', got '{}'", args));
    }
    let sym = parts[0].trim().to_string();
    let expr_str = parts[1].trim();
    if let Some(rest) = expr_str.strip_prefix(".-") {
        let label = rest.trim().to_string();
        Ok(AsmDirective::Size(sym, SizeExpr::CurrentMinusSymbol(label)))
    } else if let Ok(size) = expr_str.parse::<u64>() {
        Ok(AsmDirective::Size(sym, SizeExpr::Constant(size)))
    } else {
        // Size expressions we can't evaluate (e.g. complex expressions) are non-fatal;
        // the symbol size is not critical for code correctness in static linking
        Ok(AsmDirective::Ignored)
    }
}

/// Parse `.comm name, size[, align]`.
fn parse_comm_directive(args: &str) -> Result<AsmDirective, String> {
    let parts: Vec<&str> = args.split(',').collect();
    if parts.len() < 2 {
        return Err(format!("malformed .comm directive: expected 'name, size[, align]', got '{}'", args));
    }
    let sym = parts[0].trim().to_string();
    let size: u64 = parts[1].trim().parse().unwrap_or(0);
    let align: u64 = if parts.len() > 2 {
        parts[2].trim().parse().unwrap_or(1)
    } else {
        1
    };
    Ok(AsmDirective::Comm(sym, size, align))
}

/// Parse comma-separated data values that may be integers, symbols, or symbol expressions.
fn parse_data_values(s: &str) -> Result<Vec<DataValue>, String> {
    let mut vals = Vec::new();
    for part in s.split(',') {
        let trimmed = part.trim();
        if trimmed.is_empty() {
            continue;
        }
        // Check for symbol difference: A - B or A - B + C
        if let Some(dv) = try_parse_symbol_diff(trimmed) {
            vals.push(dv);
            continue;
        }
        // Try integer
        if let Ok(val) = parse_data_value(trimmed) {
            vals.push(DataValue::Integer(val));
            continue;
        }
        // Check for symbol+offset or symbol-offset
        if let Some(dv) = try_parse_symbol_offset(trimmed) {
            vals.push(dv);
            continue;
        }
        // Symbol reference
        vals.push(DataValue::Symbol(trimmed.to_string()));
    }
    Ok(vals)
}

/// Try to parse a symbol difference expression like "A - B" or "A - B + C".
fn try_parse_symbol_diff(expr: &str) -> Option<DataValue> {
    let expr = expr.trim();
    if expr.is_empty() {
        return None;
    }
    let first_char = expr.chars().next()?;
    if !first_char.is_ascii_alphabetic() && first_char != '_' && first_char != '.' {
        return None;
    }
    let minus_pos = find_symbol_diff_minus(expr)?;
    let sym_a = expr[..minus_pos].trim().to_string();
    let rest = expr[minus_pos + 1..].trim();
    // rest might be "B" or "B + offset"
    let (sym_b, extra_addend) = if let Some(plus_pos) = rest.find('+') {
        let b = rest[..plus_pos].trim().to_string();
        let add_str = rest[plus_pos + 1..].trim();
        let add_val: i64 = add_str.parse().unwrap_or(0);
        (b, add_val)
    } else {
        (rest.to_string(), 0i64)
    };
    if sym_b.is_empty() {
        return None;
    }
    let b_first = sym_b.chars().next().unwrap();
    if !b_first.is_ascii_alphabetic() && b_first != '_' && b_first != '.' {
        return None;
    }
    if extra_addend != 0 {
        Some(DataValue::SymbolDiffAddend(sym_a, sym_b, extra_addend))
    } else {
        Some(DataValue::SymbolDiff(sym_a, sym_b))
    }
}

/// Try to parse symbol+offset or symbol-offset.
fn try_parse_symbol_offset(s: &str) -> Option<DataValue> {
    for (i, c) in s.char_indices().skip(1) {
        if c == '+' || c == '-' {
            let sym = s[..i].trim();
            let offset_str = &s[i..]; // includes the sign
            if let Ok(offset) = parse_int_literal(offset_str) {
                if !sym.is_empty() && !sym.contains(' ') {
                    return Some(DataValue::SymbolOffset(sym.to_string(), offset));
                }
            }
        }
    }
    None
}

/// Find the position of the '-' operator in a symbol difference expression.
fn find_symbol_diff_minus(expr: &str) -> Option<usize> {
    let bytes = expr.as_bytes();
    let len = bytes.len();
    let mut i = 1;
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

/// Parse a data value (integer literal, possibly negative).
fn parse_data_value(s: &str) -> Result<i64, String> {
    let s = s.trim();
    if s.is_empty() {
        return Ok(0);
    }
    asm_expr::parse_integer_expr(s)
}
