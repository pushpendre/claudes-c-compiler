//! AArch64 assembly parser.
//!
//! Parses the textual assembly format emitted by our AArch64 codegen into
//! structured `AsmStatement` values. The parser handles:
//! - Labels (global and local)
//! - Directives (.section, .globl, .type, .align, .byte, .long, .xword, etc.)
//! - AArch64 instructions (mov, add, sub, ldr, str, bl, ret, etc.)
//! - CFI directives (passed through as-is for DWARF unwind info)

#![allow(dead_code)]

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

        // Strip comments (// style and /* */ style for single-line)
        let line = strip_comment(line);
        let line = line.trim();
        if line.is_empty() {
            statements.push(AsmStatement::Empty);
            continue;
        }

        match parse_line(line) {
            Ok(stmt) => statements.push(stmt),
            Err(e) => return Err(format!("Line {}: {}: '{}'", line_num + 1, e, line)),
        }
    }
    Ok(statements)
}

fn strip_comment(line: &str) -> &str {
    // Handle // comments
    if let Some(pos) = line.find("//") {
        // Make sure it's not inside a string
        let before = &line[..pos];
        if before.matches('"').count() % 2 == 0 {
            return &line[..pos];
        }
    }
    // Handle @ comments (GAS ARM comment character)
    if let Some(pos) = line.find('@') {
        // Don't strip @object, @function, @progbits, @nobits, @tls_object, @note
        let after = &line[pos + 1..];
        if !after.starts_with("object")
            && !after.starts_with("function")
            && !after.starts_with("progbits")
            && !after.starts_with("nobits")
            && !after.starts_with("tls_object")
            && !after.starts_with("note")
        {
            let before = &line[..pos];
            if before.matches('"').count() % 2 == 0 {
                return &line[..pos];
            }
        }
    }
    line
}

fn parse_line(line: &str) -> Result<AsmStatement, String> {
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
                return Ok(AsmStatement::Label(potential_label.to_string()));
            }
        }
    }

    let trimmed = line.trim();

    // Directive: starts with .
    if trimmed.starts_with('.') {
        return parse_directive(trimmed);
    }

    // Instruction
    parse_instruction(trimmed)
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
    if s.starts_with('{') && s.ends_with('}') {
        return parse_register_list(s);
    }

    // Memory operand: [base, #offset]! (pre-index) or [base, #offset] or [base]
    if s.starts_with('[') {
        return parse_memory_operand(s);
    }

    // Immediate: #value
    if s.starts_with('#') {
        return parse_immediate(&s[1..]);
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
        let amount = if amount_str.starts_with('#') {
            parse_int_literal(&amount_str[1..])?
        } else {
            parse_int_literal(amount_str)?
        };
        return Ok(Operand::Shift { kind: kind.to_string(), amount: amount as u32 });
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

    // [base, #imm]
    if second.starts_with('#') {
        let offset = parse_int_literal(&second[1..])?;
        if has_writeback {
            return Ok(Operand::MemPreIndex { base, offset });
        }
        return Ok(Operand::Mem { base, offset });
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
    if is_register(second) {
        return Ok(Operand::MemRegOffset {
            base,
            index: second.to_string(),
            extend: None,
            shift: None,
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
