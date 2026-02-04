//! RISC-V assembly parser.
//!
//! Parses the textual assembly format emitted by our RISC-V codegen into
//! structured `AsmStatement` values. The parser handles:
//! - Labels (global and local)
//! - Directives (.section, .globl, .type, .align, .byte, .long, .dword, etc.)
//! - RISC-V instructions (add, sub, ld, sd, beq, call, ret, etc.)
//! - CFI directives (passed through as-is for DWARF unwind info)

#![allow(dead_code)]

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

/// A parsed assembly statement.
#[derive(Debug, Clone)]
pub enum AsmStatement {
    /// A label definition: "name:"
    Label(String),
    /// A directive: .section, .globl, .align, .byte, etc.
    Directive {
        name: String,
        args: String,
    },
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
pub fn parse_asm(text: &str) -> Result<Vec<AsmStatement>, String> {
    let mut statements = Vec::new();
    for (line_num, line) in text.lines().enumerate() {
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

fn strip_comment(line: &str) -> &str {
    // Handle # comments (GAS RISC-V comment character)
    if let Some(pos) = line.find('#') {
        let before = &line[..pos];
        if before.matches('"').count() % 2 == 0 {
            return &line[..pos];
        }
    }
    // Handle // comments
    if let Some(pos) = line.find("//") {
        let before = &line[..pos];
        if before.matches('"').count() % 2 == 0 {
            return &line[..pos];
        }
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

fn parse_directive(line: &str) -> Result<AsmStatement, String> {
    // Split directive name from arguments
    let (name, args) = if let Some(space_pos) = line.find(|c: char| c == ' ' || c == '\t') {
        let name = &line[..space_pos];
        let args = line[space_pos..].trim();
        (name, args)
    } else {
        (line, "")
    };

    Ok(AsmStatement::Directive {
        name: name.to_string(),
        args: args.to_string(),
    })
}

fn parse_instruction(line: &str) -> Result<AsmStatement, String> {
    // Split mnemonic from operands
    let (mnemonic, operands_str) = if let Some(space_pos) = line.find(|c: char| c == ' ' || c == '\t') {
        (&line[..space_pos], line[space_pos..].trim())
    } else {
        (line, "")
    };

    let mnemonic = mnemonic.to_lowercase();
    // Only classify FenceArg operands when the instruction is actually "fence".
    // Otherwise, single-letter variable names like "i", "o", "r", "w" would be
    // misclassified as fence arguments (e.g., "lla t0, i" for a global named "i").
    let is_fence = mnemonic == "fence";
    let operands = parse_operands(operands_str, is_fence)?;

    Ok(AsmStatement::Instruction {
        mnemonic,
        operands,
        raw_operands: operands_str.to_string(),
    })
}

/// Parse an operand list separated by commas.
/// `is_fence`: when true, single-char subsets of "iorw" are parsed as FenceArg.
fn parse_operands(s: &str, is_fence: bool) -> Result<Vec<Operand>, String> {
    if s.is_empty() {
        return Ok(Vec::new());
    }

    let mut operands = Vec::new();
    let mut current = String::new();
    let mut paren_depth = 0;

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
                let op = parse_single_operand(current.trim(), is_fence)?;
                operands.push(op);
                current.clear();
            }
            _ => {
                current.push(ch);
            }
        }
    }

    // Last operand
    let trimmed = current.trim().to_string();
    if !trimmed.is_empty() {
        let op = parse_single_operand(&trimmed, is_fence)?;
        operands.push(op);
    }

    Ok(operands)
}

/// Parse a single operand.
/// `is_fence`: when true, classify subsets of "iorw" as FenceArg.
fn parse_single_operand(s: &str, is_fence: bool) -> Result<Operand, String> {
    let s = s.trim();
    if s.is_empty() {
        return Err("empty operand".to_string());
    }

    // Memory operand: offset(base) e.g., 8(sp), -16(s0), 0(a0)
    // Also handles: %lo(sym)(reg), %hi(sym)
    if let Some(result) = try_parse_memory_operand(s) {
        return Ok(result);
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
    if is_rounding_mode(s) {
        return Ok(Operand::RoundingMode(s.to_string()));
    }

    // Register
    if is_register(s) {
        return Ok(Operand::Reg(s.to_string()));
    }

    // Try to parse as immediate
    if let Ok(val) = parse_int_literal(s) {
        return Ok(Operand::Imm(val));
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

    let (negative, s) = if s.starts_with('-') {
        (true, &s[1..])
    } else {
        (false, s)
    };

    let val = if s.starts_with("0x") || s.starts_with("0X") {
        u64::from_str_radix(&s[2..], 16)
            .map_err(|e| format!("invalid hex literal '{}': {}", s, e))?
    } else if s.starts_with("0b") || s.starts_with("0B") {
        u64::from_str_radix(&s[2..], 2)
            .map_err(|e| format!("invalid binary literal '{}': {}", s, e))?
    } else {
        s.parse::<u64>()
            .map_err(|e| format!("invalid integer literal '{}': {}", s, e))?
    };

    if negative {
        Ok(-(val as i64))
    } else {
        Ok(val as i64)
    }
}
