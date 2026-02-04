//! Parser for AT&T syntax x86-64 assembly as emitted by our codegen.
//!
//! Parses assembly text line-by-line into structured `AsmItem` values.
//! Handles directives, labels, and instructions with AT&T operand ordering
//! (source, destination).

use std::fmt;
use crate::backend::elf;

/// A parsed assembly item (one per line, roughly).
#[derive(Debug, Clone)]
pub enum AsmItem {
    /// Switch to a named section: `.section .text`, `.section .rodata`, etc.
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
    /// Symbol type: `.type name, @function` or `@object` or `@tls_object`
    SymbolType(String, SymbolKind),
    /// Symbol size: `.size name, expr`
    Size(String, SizeExpr),
    /// Label definition: `name:`
    Label(String),
    /// Alignment: `.align N`
    Align(u32),
    /// Emit bytes: `.byte val, val, ...` (can contain label expressions)
    Byte(Vec<DataValue>),
    /// Emit 16-bit values: `.short val, ...` (can be symbol references)
    Short(Vec<DataValue>),
    /// Emit 32-bit values: `.long val, ...` (can be symbol references)
    Long(Vec<DataValue>),
    /// Emit 64-bit values: `.quad val, ...` (can be symbol references)
    Quad(Vec<DataValue>),
    /// Emit zero bytes: `.zero N`
    Zero(u32),
    /// Deferred `.skip` with expression: evaluated after all labels are known.
    /// Used by kernel alternatives framework for label-arithmetic expressions
    /// like `.skip -(((6651f-6641f)-(662b-661b)) > 0) * ((6651f-6641f)-(662b-661b)), 0x90`.
    /// Fields: (expression_string, fill_byte)
    SkipExpr(String, u8),
    /// NUL-terminated string: `.asciz "str"`
    Asciz(Vec<u8>),
    /// String without NUL: `.ascii "str"`
    Ascii(Vec<u8>),
    /// Common symbol: `.comm name, size, align`
    Comm(String, u64, u32),
    /// Symbol alias: `.set alias, target`
    Set(String, String),
    /// CFI directive (ignored for code generation, kept for .eh_frame)
    #[allow(dead_code)]
    Cfi(CfiDirective),
    /// Debug file directive: `.file N "filename"`
    #[allow(dead_code)]
    File(u32, String),
    /// Debug location: `.loc filenum line column`
    #[allow(dead_code)]
    Loc(u32, u32, u32),
    /// x86-64 instruction
    Instruction(Instruction),
    /// `.option norelax` (RISC-V, ignored for x86)
    #[allow(dead_code)]
    OptionDirective(String),
    /// Push current section and switch to a new one: `.pushsection name,"flags",@type`
    PushSection(SectionDirective),
    /// Pop section stack and restore previous section: `.popsection`
    PopSection,
    /// `.org` directive: advance to position symbol + offset within the section.
    Org(String, i64),
    /// Blank line or comment-only line
    Empty,
}

/// Section directive with optional flags and type.
#[derive(Debug, Clone)]
pub struct SectionDirective {
    pub name: String,
    pub flags: Option<String>,
    pub section_type: Option<String>,
    /// For sections with linked-to or group info (4th arg), e.g.
    /// `__patchable_function_entries,"awo",@progbits,.LPFE0`
    pub extra: Option<String>,
    /// COMDAT group name from `.section name,"axG",@progbits,group_name,comdat`
    /// Set when flags contain 'G' and a 5th argument is "comdat".
    pub comdat_group: Option<String>,
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
    /// end_label - start_label (resolved by ELF writer after relaxation)
    SymbolDiff(String, String),
}

/// A data value that can be a constant, a symbol, or a symbol expression.
#[derive(Debug, Clone)]
pub enum DataValue {
    Integer(i64),
    Symbol(String),
    /// symbol + offset (e.g., `.quad GD_struct+128`)
    SymbolOffset(String, i64),
    /// symbol - symbol (e.g., `.long .LBB3 - .Ljt_0`)
    SymbolDiff(String, String),
}

/// CFI directives (call frame information).
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum CfiDirective {
    StartProc,
    EndProc,
    DefCfaOffset(i32),
    DefCfaRegister(String),
    Offset(String, i32),
    Other(String),
}

/// An x86-64 instruction with mnemonic and operands.
#[derive(Debug, Clone)]
pub struct Instruction {
    /// Optional prefix (e.g., "lock", "rep")
    pub prefix: Option<String>,
    /// Instruction mnemonic (e.g., "movq", "addl", "ret")
    pub mnemonic: String,
    /// Operands in AT&T order (source first, destination last)
    pub operands: Vec<Operand>,
}

/// An instruction operand.
#[derive(Debug, Clone)]
pub enum Operand {
    /// Register: %rax, %eax, %al, %xmm0, %st(0), etc.
    Register(Register),
    /// Immediate: $42, $-1, $symbol
    Immediate(ImmediateValue),
    /// Memory reference: disp(%base, %index, scale) with optional segment
    Memory(MemoryOperand),
    /// Direct label/symbol reference (for jmp/call targets)
    Label(String),
    /// Indirect jump/call target: *%reg or *addr
    Indirect(Box<Operand>),
}

/// Register reference.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Register {
    pub name: String,
}

impl Register {
    pub fn new(name: &str) -> Self {
        Register { name: name.to_string() }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.name)
    }
}

/// Immediate value.
#[derive(Debug, Clone)]
pub enum ImmediateValue {
    Integer(i64),
    Symbol(String),
    /// Symbol with @modifier, e.g., symbol@GOTPCREL
    #[allow(dead_code)]
    SymbolMod(String, String),
}

/// Memory operand: optional_segment:disp(%base, %index, scale)
#[derive(Debug, Clone)]
pub struct MemoryOperand {
    pub segment: Option<String>,
    pub displacement: Displacement,
    pub base: Option<Register>,
    pub index: Option<Register>,
    pub scale: Option<u8>,
}

/// Memory displacement.
#[derive(Debug, Clone)]
pub enum Displacement {
    None,
    Integer(i64),
    Symbol(String),
    /// Symbol with an addend offset: symbol+offset or symbol-offset (e.g., GD_struct+128(%rip))
    #[allow(dead_code)]
    SymbolAddend(String, i64),
    /// Symbol with relocation modifier: symbol@GOT, symbol@GOTPCREL, symbol@TPOFF, etc.
    SymbolMod(String, String),
    /// Symbol plus integer offset: symbol+N or symbol-N
    SymbolPlusOffset(String, i64),
}

/// Strip C-style /* */ comments from assembly text.
/// These are used in hand-written assembly (e.g., musl) but not in
/// compiler-generated assembly. Handles multi-line comments.
fn strip_c_comments(text: &str) -> String {
    let mut result = String::with_capacity(text.len());
    let bytes = text.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i + 1] == b'*' {
            // Skip until */
            i += 2;
            while i + 1 < bytes.len() {
                if bytes[i] == b'*' && bytes[i + 1] == b'/' {
                    i += 2;
                    break;
                }
                // Preserve newlines so line numbers stay correct
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

/// Expand .rept/.endr blocks by repeating contained lines.
fn expand_rept_blocks(lines: &[&str]) -> Result<Vec<String>, String> {
    let mut result = Vec::new();
    let mut i = 0;
    while i < lines.len() {
        let trimmed = strip_comment(lines[i]).trim().to_string();
        if trimmed.starts_with(".rept ") || trimmed.starts_with(".rept\t") {
            let count_str = trimmed[".rept".len()..].trim();
            let count = parse_integer_expr(count_str)
                .map_err(|e| format!(".rept: bad count '{}': {}", count_str, e))? as usize;
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

/// Parse assembly text into a list of AsmItems.
pub fn parse_asm(text: &str) -> Result<Vec<AsmItem>, String> {
    let mut items = Vec::new();

    // Strip C-style /* */ comments (used in hand-written assembly like musl)
    let text = strip_c_comments(text);
    let lines: Vec<&str> = text.lines().collect();
    let expanded = expand_rept_blocks(&lines)?;

    for (line_num, line) in expanded.iter().enumerate() {
        let line_num = line_num + 1; // 1-based

        // Strip comments (# to end of line, but not inside strings)
        let stripped = strip_comment(line);
        let trimmed = stripped.trim();

        if trimmed.is_empty() {
            items.push(AsmItem::Empty);
            continue;
        }

        // Handle ';' as instruction separator (GAS syntax)
        // Split the line on ';' and parse each part independently.
        let parts: Vec<&str> = split_on_semicolons(trimmed);
        for part in parts {
            let part = part.trim();
            if part.is_empty() {
                continue;
            }
            match parse_line_items(part) {
                Ok(line_items) => items.extend(line_items),
                Err(e) => {
                    return Err(format!("line {}: {}: '{}'", line_num, e, part));
                }
            }
        }
    }

    Ok(items)
}

/// Split a line on ';' characters, respecting strings.
/// In GAS syntax, ';' separates multiple instructions on the same line.
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

/// Strip trailing comment from a line.
fn strip_comment(line: &str) -> &str {
    // Find '#' that's not inside a string
    let mut in_string = false;
    let mut escape = false;
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
        if c == '#' && !in_string {
            return &line[..i];
        }
    }
    line
}

/// Parse a single non-empty assembly line into one or more AsmItems.
///
/// A line may contain a label followed by an instruction on the same line
/// (e.g., `1: stmxcsr -8(%rsp)`), which produces two items.
fn parse_line_items(line: &str) -> Result<Vec<AsmItem>, String> {
    let mut items = Vec::new();

    // Check for label (may be followed by instruction on same line)
    let rest = if let Some((label, rest)) = try_parse_label(line) {
        items.push(AsmItem::Label(label));
        rest
    } else {
        line.trim()
    };

    // If there's nothing after the label, we're done
    if rest.is_empty() {
        return Ok(items);
    }

    // Parse the remaining content as a directive or instruction
    if rest.starts_with('.') {
        items.push(parse_directive(rest)?);
    } else if rest.starts_with("lock ") || rest.starts_with("rep ") || rest.starts_with("repz ") || rest.starts_with("repe ") || rest.starts_with("repnz ") || rest.starts_with("repne ") || rest.starts_with("notrack ") {
        items.push(parse_prefixed_instruction(rest)?);
    } else {
        items.push(parse_instruction(rest, None)?);
    }

    Ok(items)
}

/// Try to parse a label definition. Returns the label name and any remaining
/// content after the label (which may be an instruction or directive on the same line).
///
/// For example:
///   "foo:"           -> Some(("foo", ""))
///   "1:  stmxcsr -8(%rsp)"  -> Some(("1", "stmxcsr -8(%rsp)"))
///   ".Lfoo: ret"     -> Some((".Lfoo", "ret"))
///   "mov %rax, %rbx" -> None (no label)
fn try_parse_label(line: &str) -> Option<(String, &str)> {
    let trimmed = line.trim();
    if let Some(colon_pos) = trimmed.find(':') {
        let candidate = &trimmed[..colon_pos];
        // Verify it's a valid label (no spaces, starts with letter/dot/digit)
        if !candidate.is_empty()
            && !candidate.contains(' ')
            && !candidate.contains('\t')
            && !candidate.contains(',')
            && !candidate.starts_with('$')
            && !candidate.starts_with('%')
        {
            let rest = trimmed[colon_pos + 1..].trim();
            return Some((candidate.to_string(), rest));
        }
    }
    None
}

/// Parse a directive line (starts with '.').
fn parse_directive(line: &str) -> Result<AsmItem, String> {
    let parts: Vec<&str> = line.splitn(2, |c: char| c.is_whitespace()).collect();
    let directive = parts[0];
    let args = parts.get(1).map(|s| s.trim()).unwrap_or("");

    match directive {
        ".section" => parse_section_directive(args),
        ".text" => Ok(AsmItem::Section(SectionDirective {
            name: ".text".to_string(),
            flags: None,
            section_type: None,
            extra: None,
            comdat_group: None,
        })),
        ".data" => Ok(AsmItem::Section(SectionDirective {
            name: ".data".to_string(),
            flags: None,
            section_type: None,
            extra: None,
            comdat_group: None,
        })),
        ".bss" => Ok(AsmItem::Section(SectionDirective {
            name: ".bss".to_string(),
            flags: None,
            section_type: None,
            extra: None,
            comdat_group: None,
        })),
        ".rodata" => Ok(AsmItem::Section(SectionDirective {
            name: ".rodata".to_string(),
            flags: None,
            section_type: None,
            extra: None,
            comdat_group: None,
        })),
        ".globl" | ".global" => Ok(AsmItem::Global(args.trim().to_string())),
        ".weak" => Ok(AsmItem::Weak(args.trim().to_string())),
        ".hidden" => Ok(AsmItem::Hidden(args.trim().to_string())),
        ".protected" => Ok(AsmItem::Protected(args.trim().to_string())),
        ".internal" => Ok(AsmItem::Internal(args.trim().to_string())),
        ".type" => parse_type_directive(args),
        ".size" => parse_size_directive(args),
        ".align" | ".p2align" | ".balign" => {
            let val_str = args.split(',').next().unwrap_or("1").trim();
            let val: u32 = parse_integer_expr(val_str)
                .map_err(|_| format!("bad alignment: {}", args))? as u32;
            // .p2align is power-of-2, .align/.balign on x86 gas is byte count
            if directive == ".p2align" {
                Ok(AsmItem::Align(1 << val))
            } else {
                Ok(AsmItem::Align(val))
            }
        }
        ".org" => {
            let args = args.trim();
            if let Ok(val) = parse_integer_expr(args) {
                Ok(AsmItem::Org(String::new(), val))
            } else {
                let mut sym = args.to_string();
                let mut offset = 0i64;
                if let Some(plus_pos) = args.find('+') {
                    if plus_pos > 0 {
                        sym = args[..plus_pos].trim().to_string();
                        let offset_str = args[plus_pos + 1..].trim();
                        offset = parse_integer_expr(offset_str)
                            .map_err(|_| format!("bad .org offset: {}", offset_str))?;
                    }
                }
                Ok(AsmItem::Org(sym, offset))
            }
        }
        ".byte" => {
            let vals = parse_data_values(args)?;
            Ok(AsmItem::Byte(vals))
        }
        ".short" | ".value" | ".2byte" => {
            let vals = parse_data_values(args)?;
            Ok(AsmItem::Short(vals))
        }
        ".long" | ".4byte" => {
            let vals = parse_data_values(args)?;
            Ok(AsmItem::Long(vals))
        }
        ".quad" | ".8byte" => {
            let vals = parse_data_values(args)?;
            Ok(AsmItem::Quad(vals))
        }
        ".zero" | ".skip" => {
            let (expr_str, fill) = split_skip_args(args);
            if let Ok(val) = parse_integer_expr(expr_str) {
                if fill == 0 {
                    Ok(AsmItem::Zero(val as u32))
                } else {
                    Ok(AsmItem::SkipExpr(expr_str.to_string(), fill))
                }
            } else {
                // Expression contains labels or complex arithmetic - defer to ELF writer
                Ok(AsmItem::SkipExpr(expr_str.to_string(), fill))
            }
        }
        ".asciz" | ".string" => {
            let s = elf::parse_string_literal(args)?;
            let mut bytes = s;
            bytes.push(0); // NUL terminator
            Ok(AsmItem::Asciz(bytes))
        }
        ".ascii" => {
            let s = elf::parse_string_literal(args)?;
            Ok(AsmItem::Ascii(s))
        }
        ".comm" => parse_comm_directive(args),
        ".set" => parse_set_directive(args),
        ".cfi_startproc" => Ok(AsmItem::Cfi(CfiDirective::StartProc)),
        ".cfi_endproc" => Ok(AsmItem::Cfi(CfiDirective::EndProc)),
        ".cfi_def_cfa_offset" => {
            let val: i32 = args.trim().parse()
                .map_err(|_| format!("bad cfi offset: {}", args))?;
            Ok(AsmItem::Cfi(CfiDirective::DefCfaOffset(val)))
        }
        ".cfi_def_cfa_register" => {
            let reg = args.trim().trim_start_matches('%').to_string();
            Ok(AsmItem::Cfi(CfiDirective::DefCfaRegister(reg)))
        }
        ".cfi_offset" => {
            // .cfi_offset %rbp, -16
            let parts: Vec<&str> = args.splitn(2, ',').collect();
            if parts.len() != 2 {
                return Ok(AsmItem::Cfi(CfiDirective::Other(line.to_string())));
            }
            let reg = parts[0].trim().trim_start_matches('%').to_string();
            let off: i32 = parts[1].trim().parse()
                .map_err(|_| format!("bad cfi offset value: {}", args))?;
            Ok(AsmItem::Cfi(CfiDirective::Offset(reg, off)))
        }
        ".file" => {
            // .file N "filename"
            let parts: Vec<&str> = args.splitn(2, |c: char| c.is_whitespace()).collect();
            if parts.len() == 2 {
                let num: u32 = parts[0].trim().parse().unwrap_or(0);
                let filename = parts[1].trim().trim_matches('"').to_string();
                Ok(AsmItem::File(num, filename))
            } else {
                Ok(AsmItem::Empty) // ignore malformed .file
            }
        }
        ".loc" => {
            // .loc filenum line column
            let nums: Vec<u32> = args.split_whitespace()
                .take(3)
                .filter_map(|s| s.parse().ok())
                .collect();
            if nums.len() >= 2 {
                Ok(AsmItem::Loc(nums[0], nums[1], nums.get(2).copied().unwrap_or(0)))
            } else {
                Ok(AsmItem::Empty)
            }
        }
        ".pushsection" => {
            // .pushsection name,"flags",@type - same syntax as .section
            parse_section_directive(args).map(|item| {
                if let AsmItem::Section(dir) = item {
                    AsmItem::PushSection(dir)
                } else {
                    AsmItem::Empty
                }
            })
        }
        ".popsection" | ".previous" => Ok(AsmItem::PopSection),
        ".code16gcc" => Ok(AsmItem::Empty), // i686 only, ignored
        ".option" => Ok(AsmItem::OptionDirective(args.to_string())),
        _ => {
            // Unknown directive - just ignore it with a warning
            // This handles .ident, .addrsig, etc. that GCC might emit
            Ok(AsmItem::Empty)
        }
    }
}

/// Parse `.section name,"flags",@type` directive.
fn parse_section_directive(args: &str) -> Result<AsmItem, String> {
    // Split by comma, but handle quoted strings
    let parts = split_section_args(args);

    let name = parts.first()
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|| ".text".to_string());

    let flags = parts.get(1).map(|s| s.trim().trim_matches('"').to_string());
    let section_type = parts.get(2).map(|s| s.trim().to_string());
    let extra = parts.get(3).map(|s| s.trim().to_string());

    // If the flags contain 'G' (SHF_GROUP), the 4th arg is the group signature
    // name and the 5th arg should be "comdat".
    let comdat_group = if let Some(ref f) = flags {
        if f.contains('G') {
            if let Some(ref group_name) = extra {
                let fifth = parts.get(4).map(|s| s.trim().to_string());
                if fifth.as_deref() == Some("comdat") || fifth.is_none() {
                    // Accept both explicit "comdat" and implicit (GNU as default)
                    Some(group_name.clone())
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    };

    Ok(AsmItem::Section(SectionDirective {
        name,
        flags,
        section_type,
        extra,
        comdat_group,
    }))
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

/// Parse `.type name, @function` or `@object` or `@tls_object`.
fn parse_type_directive(args: &str) -> Result<AsmItem, String> {
    let parts: Vec<&str> = args.splitn(2, ',').collect();
    if parts.len() != 2 {
        return Err(format!("bad .type directive: {}", args));
    }
    let name = parts[0].trim().to_string();
    let kind_str = parts[1].trim();
    let kind = match kind_str {
        "@function" | "%function" => SymbolKind::Function,
        "@object" | "%object" => SymbolKind::Object,
        "@tls_object" | "%tls_object" => SymbolKind::TlsObject,
        "@notype" | "%notype" => SymbolKind::NoType,
        _ => return Err(format!("unknown symbol type: {}", kind_str)),
    };
    Ok(AsmItem::SymbolType(name, kind))
}

/// Parse `.size name, expr`.
fn parse_size_directive(args: &str) -> Result<AsmItem, String> {
    let parts: Vec<&str> = args.splitn(2, ',').collect();
    if parts.len() != 2 {
        return Err(format!("bad .size directive: {}", args));
    }
    let name = parts[0].trim().to_string();
    let expr_str = parts[1].trim();

    // Match ". - sym" or ".-sym" (current position minus symbol)
    let normalized = expr_str.replace(' ', "");
    if let Some(rest) = normalized.strip_prefix(".-") {
        let sym = rest.trim().to_string();
        Ok(AsmItem::Size(name, SizeExpr::CurrentMinusSymbol(sym)))
    } else {
        let val: u64 = parse_integer_expr(expr_str)
            .map_err(|_| format!("bad size expr: {}", expr_str))? as u64;
        Ok(AsmItem::Size(name, SizeExpr::Constant(val)))
    }
}

/// Parse `.comm name, size, align`.
fn parse_comm_directive(args: &str) -> Result<AsmItem, String> {
    let parts: Vec<&str> = args.split(',').collect();
    if parts.len() < 2 {
        return Err(format!("bad .comm directive: {}", args));
    }
    let name = parts[0].trim().to_string();
    let size: u64 = parts[1].trim().parse()
        .map_err(|_| format!("bad .comm size: {}", args))?;
    let align: u32 = parts.get(2)
        .map(|s| s.trim().parse().unwrap_or(1))
        .unwrap_or(1);
    Ok(AsmItem::Comm(name, size, align))
}

/// Parse `.set alias, target`.
fn parse_set_directive(args: &str) -> Result<AsmItem, String> {
    let parts: Vec<&str> = args.splitn(2, ',').collect();
    if parts.len() != 2 {
        return Err(format!("bad .set directive: {}", args));
    }
    Ok(AsmItem::Set(
        parts[0].trim().to_string(),
        parts[1].trim().to_string(),
    ))
}

/// Parse a prefix instruction like "lock cmpxchgq ..." or "rep movsb".
fn parse_prefixed_instruction(line: &str) -> Result<AsmItem, String> {
    let parts: Vec<&str> = line.splitn(2, |c: char| c.is_whitespace()).collect();
    let prefix = parts[0].to_string();
    let rest = parts.get(1).map(|s| s.trim()).unwrap_or("");
    parse_instruction(rest, Some(prefix))
}

/// Parse an instruction line.
fn parse_instruction(line: &str, prefix: Option<String>) -> Result<AsmItem, String> {
    let trimmed = line.trim();
    if trimmed.is_empty() {
        return Ok(AsmItem::Empty);
    }

    // Split mnemonic from operands
    let (mnemonic, operand_str) = split_mnemonic_operands(trimmed);

    if mnemonic.is_empty() {
        return Err(format!("empty mnemonic in: {}", line));
    }

    let operands = if operand_str.is_empty() {
        Vec::new()
    } else {
        parse_operands(operand_str)?
    };

    Ok(AsmItem::Instruction(Instruction {
        prefix,
        mnemonic: mnemonic.to_string(),
        operands,
    }))
}

/// Split a line into mnemonic and operand string.
fn split_mnemonic_operands(line: &str) -> (&str, &str) {
    if let Some(pos) = line.find(|c: char| c.is_whitespace()) {
        let mnemonic = &line[..pos];
        let rest = line[pos..].trim();
        (mnemonic, rest)
    } else {
        (line, "")
    }
}

/// Parse comma-separated operands.
fn parse_operands(s: &str) -> Result<Vec<Operand>, String> {
    let parts = split_operands(s);
    let mut operands = Vec::new();
    for part in &parts {
        operands.push(parse_operand(part.trim())?);
    }
    Ok(operands)
}

/// Split operand string by commas, respecting parentheses.
fn split_operands(s: &str) -> Vec<String> {
    let mut parts = Vec::new();
    let mut current = String::new();
    let mut paren_depth = 0;

    for c in s.chars() {
        if c == '(' {
            paren_depth += 1;
            current.push(c);
        } else if c == ')' {
            paren_depth -= 1;
            current.push(c);
        } else if c == ',' && paren_depth == 0 {
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

/// Parse a single operand.
fn parse_operand(s: &str) -> Result<Operand, String> {
    let s = s.trim();

    // Indirect: *%reg or *addr
    if let Some(rest) = s.strip_prefix('*') {
        let inner = parse_operand(rest)?;
        return Ok(Operand::Indirect(Box::new(inner)));
    }

    // Register: %rax, %st(0), etc.
    if s.starts_with('%') {
        return parse_register_operand(s);
    }

    // Immediate: $42, $symbol, $symbol@GOTPCREL
    if let Some(rest) = s.strip_prefix('$') {
        return parse_immediate_operand(rest);
    }

    // Memory or label reference
    // Could be: offset(%base), symbol(%rip), (%base,%idx,scale), or just a label
    if s.contains('(') || s.contains(':') {
        return parse_memory_operand(s);
    }

    // Plain label reference (for jmp/call targets)
    // Could be: .LBB42, funcname, funcname@PLT, 1f, 1b
    Ok(Operand::Label(s.to_string()))
}

/// Parse a register operand like %rax, %st(0).
fn parse_register_operand(s: &str) -> Result<Operand, String> {
    let name = &s[1..]; // strip %

    // Handle %st(N)
    if name.starts_with("st(") && name.ends_with(')') {
        return Ok(Operand::Register(Register::new(name)));
    }

    // Handle segment:memory patterns like %fs:0
    if let Some(colon_pos) = name.find(':') {
        let seg = &name[..colon_pos];
        let rest = &s[1 + colon_pos + 1..]; // after the colon
        if seg == "fs" || seg == "gs" {
            let mut mem = parse_memory_inner(rest)?;
            mem.segment = Some(seg.to_string());
            return Ok(Operand::Memory(mem));
        }
    }

    Ok(Operand::Register(Register::new(name)))
}

/// Parse an immediate operand (after the '$').
fn parse_immediate_operand(s: &str) -> Result<Operand, String> {
    let s = s.trim();

    // Try integer
    if let Ok(val) = parse_integer_expr(s) {
        return Ok(Operand::Immediate(ImmediateValue::Integer(val)));
    }

    // Symbol with modifier: symbol@GOTPCREL, etc.
    if let Some(at_pos) = s.find('@') {
        let sym = s[..at_pos].to_string();
        let modifier = s[at_pos + 1..].to_string();
        return Ok(Operand::Immediate(ImmediateValue::SymbolMod(sym, modifier)));
    }

    // Plain symbol
    Ok(Operand::Immediate(ImmediateValue::Symbol(s.to_string())))
}

/// Parse a memory operand like `offset(%base, %index, scale)` or `symbol(%rip)`.
fn parse_memory_operand(s: &str) -> Result<Operand, String> {
    // Check for segment prefix: %fs:..., %gs:...
    if s.starts_with('%') {
        if let Some(colon_pos) = s.find(':') {
            let seg = &s[1..colon_pos];
            if seg == "fs" || seg == "gs" {
                let rest = &s[colon_pos + 1..];
                let mut mem = parse_memory_inner(rest)?;
                mem.segment = Some(seg.to_string());
                return Ok(Operand::Memory(mem));
            }
        }
    }

    let mem = parse_memory_inner(s)?;
    Ok(Operand::Memory(mem))
}

/// Parse memory operand inner part: `offset(%base, %index, scale)`.
fn parse_memory_inner(s: &str) -> Result<MemoryOperand, String> {
    let s = s.trim();

    // Find the register-enclosing parentheses: the LAST balanced (...) group.
    // This handles nested parens in the displacement like ((6*8) + 8*16)(%rsp).
    // We scan backwards from the end to find the matching '(' for the final ')'.
    if let Some(paren_end) = s.rfind(')') {
        let mut depth = 0i32;
        let mut paren_start = None;
        for (i, c) in s[..=paren_end].char_indices().rev() {
            match c {
                ')' => depth += 1,
                '(' => {
                    depth -= 1;
                    if depth == 0 {
                        paren_start = Some(i);
                        break;
                    }
                }
                _ => {}
            }
        }
        let paren_start = paren_start
            .ok_or_else(|| format!("unmatched paren in memory operand: {}", s))?;
        let disp_str = s[..paren_start].trim();
        let inner = &s[paren_start + 1..paren_end];

        let displacement = parse_displacement(disp_str)?;

        // Parse base, index, scale from inside parens
        let parts: Vec<&str> = inner.split(',').map(|p| p.trim()).collect();

        let base = if !parts.is_empty() && !parts[0].is_empty() {
            Some(Register::new(parts[0].trim_start_matches('%')))
        } else {
            None
        };

        let index = if parts.len() > 1 && !parts[1].is_empty() {
            Some(Register::new(parts[1].trim_start_matches('%')))
        } else {
            None
        };

        let scale = if parts.len() > 2 && !parts[2].is_empty() {
            Some(parts[2].parse::<u8>()
                .map_err(|_| format!("bad scale: {}", parts[2]))?)
        } else {
            None
        };

        Ok(MemoryOperand {
            segment: None,
            displacement,
            base,
            index,
            scale,
        })
    } else {
        // No parens - could be just a displacement/symbol
        let displacement = parse_displacement(s)?;
        Ok(MemoryOperand {
            segment: None,
            displacement,
            base: None,
            index: None,
            scale: None,
        })
    }
}

/// Parse a displacement: integer, symbol, symbol+offset, or symbol@modifier.
fn parse_displacement(s: &str) -> Result<Displacement, String> {
    let s = s.trim();
    if s.is_empty() {
        return Ok(Displacement::None);
    }

    // Try integer
    if let Ok(val) = parse_integer_expr(s) {
        return Ok(Displacement::Integer(val));
    }

    // Symbol with modifier: symbol@GOTPCREL, symbol@TPOFF, etc.
    if let Some(at_pos) = s.find('@') {
        let sym = s[..at_pos].to_string();
        let modifier = s[at_pos + 1..].to_string();
        return Ok(Displacement::SymbolMod(sym, modifier));
    }

    // Check for symbol+offset or symbol-offset (e.g., `.Lstr0+1`, `foo-4`)
    // Find the last '+' or '-' that's not at position 0 (to avoid splitting
    // negative numbers or labels starting with '.')
    if let Some(offset_disp) = try_parse_symbol_plus_offset(s) {
        return Ok(offset_disp);
    }

    // Plain symbol
    Ok(Displacement::Symbol(s.to_string()))
}

/// Try to parse a `symbol+offset` or `symbol-offset` expression.
/// Returns None if the string doesn't match this pattern.
fn try_parse_symbol_plus_offset(s: &str) -> Option<Displacement> {
    // Scan for '+' or '-' that separates the symbol from the offset.
    // Skip the first character to avoid splitting on leading sign/dot.
    for (i, c) in s.char_indices().skip(1) {
        if c == '+' || c == '-' {
            let sym = s[..i].trim();
            let offset_str = s[i..].trim(); // includes the + or -
            if sym.is_empty() {
                continue;
            }
            // The offset part must be parseable as an integer
            if let Ok(offset) = parse_integer_expr(offset_str) {
                // Make sure the symbol part looks like a valid symbol name
                if sym.chars().all(|c| c.is_alphanumeric() || c == '_' || c == '.' || c == '$') {
                    return Some(Displacement::SymbolPlusOffset(sym.to_string(), offset));
                }
            }
        }
    }
    None
}


/// Split `.skip expr, fill` arguments, respecting parentheses.
/// Returns the expression string and the fill byte (default 0).
fn split_skip_args(args: &str) -> (&str, u8) {
    let mut depth = 0i32;
    let mut last_comma = None;
    for (i, c) in args.char_indices() {
        match c {
            '(' => depth += 1,
            ')' => depth -= 1,
            ',' if depth == 0 => last_comma = Some(i),
            _ => {}
        }
    }
    if let Some(pos) = last_comma {
        let expr = &args[..pos];
        let fill_str = args[pos + 1..].trim();
        let fill = if let Ok(v) = parse_integer_expr(fill_str) { v as u8 } else { 0u8 };
        (expr, fill)
    } else {
        (args, 0u8)
    }
}

/// Parse a label diff without spaces, e.g., `663b-661b`.
/// Returns a SymbolDiff DataValue if both sides look like labels.
fn parse_label_diff(s: &str) -> Option<DataValue> {
    for (i, c) in s.char_indices().skip(1) {
        if c == '-' {
            let lhs = &s[..i];
            let rhs = &s[i + 1..];
            if !lhs.is_empty() && !rhs.is_empty()
                && is_label_like(lhs) && is_label_like(rhs) {
                return Some(DataValue::SymbolDiff(lhs.to_string(), rhs.to_string()));
            }
        }
    }
    None
}

/// Check if a string looks like a label (alphanumeric, underscore, dot).
fn is_label_like(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    let first = s.as_bytes()[0];
    if !(first.is_ascii_alphabetic() || first == b'_' || first == b'.' || first.is_ascii_digit()) {
        return false;
    }
    s.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_' || b == b'.')
}

/// Parse data values (integers or symbol references).
fn parse_data_values(s: &str) -> Result<Vec<DataValue>, String> {
    let mut vals = Vec::new();
    for part in s.split(',') {
        let trimmed = part.trim();
        if trimmed.is_empty() {
            continue;
        }

        // Check for symbol difference: .LBB3 - .Ljt_0
        if let Some(minus_pos) = trimmed.find(" - ") {
            let lhs = trimmed[..minus_pos].trim().to_string();
            let rhs = trimmed[minus_pos + 3..].trim().to_string();
            vals.push(DataValue::SymbolDiff(lhs, rhs));
            continue;
        }

        // Try integer
        if let Ok(val) = parse_integer_expr(trimmed) {
            vals.push(DataValue::Integer(val));
            continue;
        }

        // Check for label diff without spaces (e.g., 663b-661b)
        if let Some(val) = parse_label_diff(trimmed) {
            vals.push(val);
            continue;
        }

        // Check for symbol+offset or symbol-offset (e.g., GD_struct+128, arr+33)
        if let Some(val) = parse_symbol_offset(trimmed) {
            vals.push(val);
            continue;
        }

        // Symbol reference
        vals.push(DataValue::Symbol(trimmed.to_string()));
    }
    Ok(vals)
}

/// Parse symbol+offset or symbol-offset expressions (e.g., GD_struct+128).
/// Returns a DataValue::SymbolOffset if the string matches this pattern.
fn parse_symbol_offset(s: &str) -> Option<DataValue> {
    // Look for + or - that separates symbol from offset
    // Don't match the leading character (could be .-prefixed label)
    for (i, c) in s.char_indices().skip(1) {
        if c == '+' || c == '-' {
            let sym = s[..i].trim();
            let offset_str = &s[i..]; // includes the sign
            if let Ok(offset) = parse_integer_expr(offset_str) {
                if !sym.is_empty() && !sym.contains(' ') {
                    return Some(DataValue::SymbolOffset(sym.to_string(), offset));
                }
            }
        }
    }
    None
}

/// Parse a single integer token (decimal, hex, octal, binary).
/// This handles only individual numeric literals without arithmetic operators.
/// Expression evaluation (+, -, *) is handled by parse_integer_expr and eval_term.
fn parse_single_integer(s: &str) -> Result<i64, String> {
    let s = s.trim();
    if s.is_empty() {
        return Err("empty integer".to_string());
    }

    // Handle ~ (bitwise NOT) prefix: ~7 = -8, ~0xFFF = -0x1000
    if let Some(rest) = s.strip_prefix('~') {
        let inner = parse_integer_expr(rest)?;
        return Ok(!inner);
    }

    // Try parsing the entire string first (handles i64::MIN correctly)
    if let Ok(val) = s.parse::<i64>() {
        return Ok(val);
    }

    let (negative, s) = if let Some(rest) = s.strip_prefix('-') {
        (true, rest)
    } else {
        (false, s)
    };

    let val = if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        // Parse as u64 to handle values like 0x8000000000000000
        let uval = u64::from_str_radix(hex, 16)
            .map_err(|_| format!("bad hex: {}", s))?;
        if negative {
            return Ok(-(uval as i64));
        }
        return Ok(uval as i64);
    } else if let Some(bin) = s.strip_prefix("0b").or_else(|| s.strip_prefix("0B")) {
        i64::from_str_radix(bin, 2)
            .map_err(|_| format!("bad binary: {}", s))?
    } else if s.starts_with('0') && s.len() > 1 && s.chars().all(|c| c.is_ascii_digit()) {
        i64::from_str_radix(s, 8)
            .map_err(|_| format!("bad octal: {}", s))?
    } else {
        // Try parsing unsigned decimal as u64, then cast (for large values)
        if let Ok(uval) = s.parse::<u64>() {
            if negative {
                return Ok(-(uval as i64));
            }
            return Ok(uval as i64);
        }
        return Err(format!("bad integer: {}", s));
    };

    Ok(if negative { -val } else { val })
}

/// Token type for the expression evaluator.
#[derive(Debug)]
enum ExprToken {
    Num(i64),
    Op(char),
    Op2(&'static str),
}

/// Tokenize an expression string into ExprTokens.
fn tokenize_expr(s: &str) -> Result<Vec<ExprToken>, String> {
    let mut tokens = Vec::new();
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i];
        if c.is_ascii_whitespace() {
            i += 1;
            continue;
        }
        if c == b'(' || c == b')' || c == b'+' || c == b'-' || c == b'*'
            || c == b'/' || c == b'%' || c == b'&' || c == b'|' || c == b'^' || c == b'~'
        {
            tokens.push(ExprToken::Op(c as char));
            i += 1;
        } else if c == b'<' && i + 1 < bytes.len() && bytes[i + 1] == b'<' {
            tokens.push(ExprToken::Op2("<<"));
            i += 2;
        } else if c == b'>' && i + 1 < bytes.len() && bytes[i + 1] == b'>' {
            tokens.push(ExprToken::Op2(">>"));
            i += 2;
        } else if c.is_ascii_digit() || (c == b'0' && i + 1 < bytes.len() && (bytes[i + 1] == b'x' || bytes[i + 1] == b'X')) {
            let start = i;
            if c == b'0' && i + 1 < bytes.len() && (bytes[i + 1] == b'x' || bytes[i + 1] == b'X') {
                i += 2;
                while i < bytes.len() && bytes[i].is_ascii_hexdigit() { i += 1; }
            } else {
                while i < bytes.len() && bytes[i].is_ascii_digit() { i += 1; }
            }
            let num_str = &s[start..i];
            let val = parse_single_integer(num_str)?;
            tokens.push(ExprToken::Num(val));
        } else {
            return Err(format!("unexpected char '{}' in expression", c as char));
        }
    }
    Ok(tokens)
}

fn eval_tokens(tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
    eval_or(tokens, pos)
}

fn eval_or(tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
    let mut val = eval_xor(tokens, pos)?;
    while *pos < tokens.len() {
        if matches!(&tokens[*pos], ExprToken::Op('|')) {
            *pos += 1;
            val |= eval_xor(tokens, pos)?;
        } else {
            break;
        }
    }
    Ok(val)
}

fn eval_xor(tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
    let mut val = eval_and(tokens, pos)?;
    while *pos < tokens.len() {
        if matches!(&tokens[*pos], ExprToken::Op('^')) {
            *pos += 1;
            val ^= eval_and(tokens, pos)?;
        } else {
            break;
        }
    }
    Ok(val)
}

fn eval_and(tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
    let mut val = eval_shift(tokens, pos)?;
    while *pos < tokens.len() {
        if matches!(&tokens[*pos], ExprToken::Op('&')) {
            *pos += 1;
            val &= eval_shift(tokens, pos)?;
        } else {
            break;
        }
    }
    Ok(val)
}

fn eval_shift(tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
    let mut val = eval_add(tokens, pos)?;
    while *pos < tokens.len() {
        match &tokens[*pos] {
            ExprToken::Op2("<<") => { *pos += 1; val <<= eval_add(tokens, pos)?; }
            ExprToken::Op2(">>") => { *pos += 1; val >>= eval_add(tokens, pos)?; }
            _ => break,
        }
    }
    Ok(val)
}

fn eval_add(tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
    let mut val = eval_mul(tokens, pos)?;
    while *pos < tokens.len() {
        match &tokens[*pos] {
            ExprToken::Op('+') => { *pos += 1; val += eval_mul(tokens, pos)?; }
            ExprToken::Op('-') => { *pos += 1; val -= eval_mul(tokens, pos)?; }
            _ => break,
        }
    }
    Ok(val)
}

fn eval_mul(tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
    let mut val = eval_unary(tokens, pos)?;
    while *pos < tokens.len() {
        match &tokens[*pos] {
            ExprToken::Op('*') => { *pos += 1; val *= eval_unary(tokens, pos)?; }
            ExprToken::Op('/') => {
                *pos += 1;
                let rhs = eval_unary(tokens, pos)?;
                if rhs == 0 { return Err("division by zero".to_string()); }
                val /= rhs;
            }
            ExprToken::Op('%') => {
                *pos += 1;
                let rhs = eval_unary(tokens, pos)?;
                if rhs == 0 { return Err("modulo by zero".to_string()); }
                val %= rhs;
            }
            _ => break,
        }
    }
    Ok(val)
}

fn eval_unary(tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
    if *pos >= tokens.len() {
        return Err("unexpected end of expression".to_string());
    }
    match &tokens[*pos] {
        ExprToken::Op('-') => { *pos += 1; Ok(-eval_unary(tokens, pos)?) }
        ExprToken::Op('+') => { *pos += 1; eval_unary(tokens, pos) }
        ExprToken::Op('~') => { *pos += 1; Ok(!eval_unary(tokens, pos)?) }
        ExprToken::Op('(') => {
            *pos += 1;
            let val = eval_tokens(tokens, pos)?;
            if *pos < tokens.len() && matches!(&tokens[*pos], ExprToken::Op(')')) {
                *pos += 1;
            }
            Ok(val)
        }
        ExprToken::Num(v) => { let v = *v; *pos += 1; Ok(v) }
        other => Err(format!("unexpected token in expression: {:?}", other)),
    }
}

/// Parse an integer expression with full operator precedence.
/// Supports: |, ^, &, <<, >>, +, -, *, /, %, ~, parentheses.
fn parse_integer_expr(s: &str) -> Result<i64, String> {
    let s = s.trim();
    if s.is_empty() {
        return Err("empty integer".to_string());
    }
    // Fast path: try simple integer first
    if let Ok(val) = parse_single_integer(s) {
        return Ok(val);
    }
    let tokens = tokenize_expr(s)?;
    if tokens.is_empty() {
        return Err("empty expression".to_string());
    }
    let mut pos = 0;
    let val = eval_tokens(&tokens, &mut pos)?;
    Ok(val)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple() {
        let asm = r#"
.section .text
.globl main
.type main, @function
main:
.cfi_startproc
    pushq %rbp
    movq %rsp, %rbp
    xorl %eax, %eax
    popq %rbp
    ret
.cfi_endproc
.size main, .-main
"#;
        let items = parse_asm(asm).unwrap();
        // Should parse without errors
        let labels: Vec<_> = items.iter().filter(|i| matches!(i, AsmItem::Label(_))).collect();
        assert_eq!(labels.len(), 1);
    }

    #[test]
    fn test_parse_memory_operand() {
        let mem = parse_memory_inner("-8(%rbp)").unwrap();
        assert!(matches!(mem.displacement, Displacement::Integer(-8)));
        assert_eq!(mem.base.as_ref().unwrap().name, "rbp");
    }

    #[test]
    fn test_parse_rip_relative() {
        let mem = parse_memory_inner("x(%rip)").unwrap();
        assert!(matches!(mem.displacement, Displacement::Symbol(ref s) if s == "x"));
        assert_eq!(mem.base.as_ref().unwrap().name, "rip");
    }

    #[test]
    fn test_parse_integer() {
        assert_eq!(parse_integer_expr("42").unwrap(), 42);
        assert_eq!(parse_integer_expr("-1").unwrap(), -1);
        assert_eq!(parse_integer_expr("0xff").unwrap(), 255);
        assert_eq!(parse_integer_expr("0").unwrap(), 0);
    }
}
