//! Parser for AT&T syntax x86-64 assembly as emitted by our codegen.
//!
//! Parses assembly text line-by-line into structured `AsmItem` values.
//! Handles directives, labels, and instructions with AT&T operand ordering
//! (source, destination).

use std::fmt;

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
    /// Emit bytes: `.byte val, val, ...`
    Byte(Vec<u8>),
    /// Emit 16-bit values: `.short val, ...`
    Short(Vec<i16>),
    /// Emit 32-bit values: `.long val, ...` (can be symbol references)
    Long(Vec<DataValue>),
    /// Emit 64-bit values: `.quad val, ...` (can be symbol references)
    Quad(Vec<DataValue>),
    /// Emit zero bytes: `.zero N`
    Zero(u32),
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
    /// Blank line or comment-only line
    Empty,
}

/// Section directive with optional flags and type.
#[derive(Debug, Clone)]
pub struct SectionDirective {
    pub name: String,
    pub flags: Option<String>,
    pub section_type: Option<String>,
    /// For sections like `__patchable_function_entries,"awo",@progbits,.LPFE0`
    #[allow(dead_code)]
    pub extra: Option<String>,
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
    SymbolAddend(String, i64),
    /// Symbol with relocation modifier: symbol@GOT, symbol@GOTPCREL, symbol@TPOFF, etc.
    SymbolMod(String, String),
}

/// Parse assembly text into a list of AsmItems.
pub fn parse_asm(text: &str) -> Result<Vec<AsmItem>, String> {
    let mut items = Vec::new();

    for (line_num, line) in text.lines().enumerate() {
        let line_num = line_num + 1; // 1-based

        // Strip comments (# to end of line, but not inside strings)
        let stripped = strip_comment(line);
        let trimmed = stripped.trim();

        if trimmed.is_empty() {
            items.push(AsmItem::Empty);
            continue;
        }

        match parse_line(trimmed) {
            Ok(item) => items.push(item),
            Err(e) => {
                return Err(format!("line {}: {}: '{}'", line_num, e, trimmed));
            }
        }
    }

    Ok(items)
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

/// Parse a single non-empty assembly line.
fn parse_line(line: &str) -> Result<AsmItem, String> {
    // Check for label (ends with ':' and doesn't start with '.')
    // Labels can be: `name:`, `.LBB42:`, `1:` (numeric labels)
    if let Some(label) = try_parse_label(line) {
        return Ok(AsmItem::Label(label));
    }

    // Check for directive (starts with '.')
    let trimmed = line.trim();
    if trimmed.starts_with('.') {
        return parse_directive(trimmed);
    }

    // Check for instruction with prefix
    if trimmed.starts_with("lock ") || trimmed.starts_with("rep ") || trimmed.starts_with("repz ") || trimmed.starts_with("repnz ") {
        return parse_prefixed_instruction(trimmed);
    }

    // Regular instruction
    parse_instruction(trimmed, None)
}

/// Try to parse a label definition. Returns the label name if successful.
fn try_parse_label(line: &str) -> Option<String> {
    let trimmed = line.trim();
    // A label is at the end of the line (or followed by whitespace/instruction)
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
            // Make sure the colon is at the end (or followed by only whitespace/comment)
            let rest = trimmed[colon_pos + 1..].trim();
            if rest.is_empty() {
                return Some(candidate.to_string());
            }
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
        })),
        ".data" => Ok(AsmItem::Section(SectionDirective {
            name: ".data".to_string(),
            flags: None,
            section_type: None,
            extra: None,
        })),
        ".bss" => Ok(AsmItem::Section(SectionDirective {
            name: ".bss".to_string(),
            flags: None,
            section_type: None,
            extra: None,
        })),
        ".rodata" => Ok(AsmItem::Section(SectionDirective {
            name: ".rodata".to_string(),
            flags: None,
            section_type: None,
            extra: None,
        })),
        ".globl" | ".global" => Ok(AsmItem::Global(args.trim().to_string())),
        ".weak" => Ok(AsmItem::Weak(args.trim().to_string())),
        ".hidden" => Ok(AsmItem::Hidden(args.trim().to_string())),
        ".protected" => Ok(AsmItem::Protected(args.trim().to_string())),
        ".internal" => Ok(AsmItem::Internal(args.trim().to_string())),
        ".type" => parse_type_directive(args),
        ".size" => parse_size_directive(args),
        ".align" | ".p2align" => {
            let val: u32 = args.split(',').next().unwrap_or("1").trim()
                .parse().map_err(|_| format!("bad alignment: {}", args))?;
            // .p2align is power-of-2, .align on x86 gas is byte count
            if directive == ".p2align" {
                Ok(AsmItem::Align(1 << val))
            } else {
                Ok(AsmItem::Align(val))
            }
        }
        ".byte" => {
            let vals = parse_comma_separated_integers(args)?;
            Ok(AsmItem::Byte(vals.iter().map(|v| *v as u8).collect()))
        }
        ".short" | ".value" | ".2byte" => {
            let vals = parse_comma_separated_integers(args)?;
            Ok(AsmItem::Short(vals.iter().map(|v| *v as i16).collect()))
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
            let val: u32 = args.split(',').next().unwrap_or("0").trim()
                .parse().map_err(|_| format!("bad zero count: {}", args))?;
            Ok(AsmItem::Zero(val))
        }
        ".asciz" | ".string" => {
            let s = parse_string_literal(args)?;
            let mut bytes = s;
            bytes.push(0); // NUL terminator
            Ok(AsmItem::Asciz(bytes))
        }
        ".ascii" => {
            let s = parse_string_literal(args)?;
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

    Ok(AsmItem::Section(SectionDirective {
        name,
        flags,
        section_type,
        extra,
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

    if expr_str.starts_with(".-") {
        let sym = expr_str[2..].trim().to_string();
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
    if s.starts_with('*') {
        let inner = parse_operand(&s[1..])?;
        return Ok(Operand::Indirect(Box::new(inner)));
    }

    // Register: %rax, %st(0), etc.
    if s.starts_with('%') {
        return parse_register_operand(s);
    }

    // Immediate: $42, $symbol, $symbol@GOTPCREL
    if s.starts_with('$') {
        return parse_immediate_operand(&s[1..]);
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

    // Find the parenthesized part
    if let Some(paren_start) = s.find('(') {
        let disp_str = s[..paren_start].trim();
        let paren_end = s.rfind(')')
            .ok_or_else(|| format!("unmatched paren in memory operand: {}", s))?;
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

/// Parse a displacement: integer, symbol, or symbol@modifier.
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

    // Check for symbol+offset or symbol-offset (e.g., GD_struct+128)
    // Look for + or - that separates symbol from offset (skip first char for .-prefixed labels)
    for (i, c) in s.char_indices().skip(1) {
        if c == '+' || c == '-' {
            let sym = s[..i].trim();
            let offset_str = &s[i..]; // includes the sign
            if let Ok(offset) = parse_integer_expr(offset_str) {
                if !sym.is_empty() && !sym.contains(' ') {
                    return Ok(Displacement::SymbolAddend(sym.to_string(), offset));
                }
            }
        }
    }

    // Plain symbol
    Ok(Displacement::Symbol(s.to_string()))
}

/// Parse a comma-separated list of integer values.
fn parse_comma_separated_integers(s: &str) -> Result<Vec<i64>, String> {
    let mut vals = Vec::new();
    for part in s.split(',') {
        let trimmed = part.trim();
        if trimmed.is_empty() {
            continue;
        }
        let val = parse_integer_expr(trimmed)
            .map_err(|_| format!("bad integer: {}", trimmed))?;
        vals.push(val);
    }
    Ok(vals)
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

/// Parse an integer expression (decimal, hex, octal, negative).
fn parse_integer_expr(s: &str) -> Result<i64, String> {
    let s = s.trim();
    if s.is_empty() {
        return Err("empty integer".to_string());
    }

    // Try parsing the entire string first (handles i64::MIN correctly)
    if let Ok(val) = s.parse::<i64>() {
        return Ok(val);
    }

    let (negative, s) = if s.starts_with('-') {
        (true, &s[1..])
    } else {
        (false, s)
    };

    let val = if s.starts_with("0x") || s.starts_with("0X") {
        // Parse as u64 to handle values like 0x8000000000000000
        let uval = u64::from_str_radix(&s[2..], 16)
            .map_err(|_| format!("bad hex: {}", s))?;
        if negative {
            return Ok(-(uval as i64));
        }
        return Ok(uval as i64);
    } else if s.starts_with("0b") || s.starts_with("0B") {
        i64::from_str_radix(&s[2..], 2)
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

/// Parse a string literal (with escapes).
fn parse_string_literal(s: &str) -> Result<Vec<u8>, String> {
    let s = s.trim();
    if !s.starts_with('"') {
        return Err(format!("expected string literal: {}", s));
    }

    let mut bytes = Vec::new();
    let mut chars = s[1..].chars();
    loop {
        match chars.next() {
            None => return Err("unterminated string".to_string()),
            Some('"') => break,
            Some('\\') => {
                match chars.next() {
                    None => return Err("unterminated escape".to_string()),
                    Some('n') => bytes.push(b'\n'),
                    Some('t') => bytes.push(b'\t'),
                    Some('r') => bytes.push(b'\r'),
                    Some('0') => bytes.push(0),
                    Some('\\') => bytes.push(b'\\'),
                    Some('"') => bytes.push(b'"'),
                    Some('a') => bytes.push(7),  // bell
                    Some('b') => bytes.push(8),  // backspace
                    Some('f') => bytes.push(12), // form feed
                    Some('v') => bytes.push(11), // vertical tab
                    Some(c) if c.is_ascii_digit() => {
                        // Octal escape: \NNN
                        let mut val = c as u32 - '0' as u32;
                        for _ in 0..2 {
                            if let Some(&next) = chars.as_str().as_bytes().first() {
                                if next >= b'0' && next <= b'7' {
                                    val = val * 8 + (next - b'0') as u32;
                                    chars.next();
                                } else {
                                    break;
                                }
                            }
                        }
                        bytes.push(val as u8);
                    }
                    Some('x') => {
                        // Hex escape: \xNN
                        let mut val = 0u32;
                        for _ in 0..2 {
                            if let Some(&next) = chars.as_str().as_bytes().first() {
                                if next.is_ascii_hexdigit() {
                                    val = val * 16 + match next {
                                        b'0'..=b'9' => (next - b'0') as u32,
                                        b'a'..=b'f' => (next - b'a' + 10) as u32,
                                        b'A'..=b'F' => (next - b'A' + 10) as u32,
                                        _ => unreachable!(),
                                    };
                                    chars.next();
                                } else {
                                    break;
                                }
                            }
                        }
                        bytes.push(val as u8);
                    }
                    Some(c) => bytes.push(c as u8),
                }
            }
            Some(c) => {
                // Regular character - encode as UTF-8
                let mut buf = [0u8; 4];
                let encoded = c.encode_utf8(&mut buf);
                bytes.extend_from_slice(encoded.as_bytes());
            }
        }
    }

    Ok(bytes)
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
        let items = parse(asm).unwrap();
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
