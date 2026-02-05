/// Shared assembly expression evaluator for all assembler backends (x86, i686, ARM, RISC-V).
///
/// Supports arithmetic expressions with proper operator precedence:
///   - Parentheses: `(expr)`
///   - Bitwise OR: `|`
///   - Bitwise XOR: `^`
///   - Bitwise AND: `&`
///   - Shifts: `<<`, `>>`
///   - Addition/Subtraction: `+`, `-`
///   - Multiplication/Division/Modulo: `*`, `/`, `%`
///   - Unary: `-`, `+`, `~`, `!` (logical NOT)
///
/// Integer literals: decimal, hex (0x), binary (0b), octal (leading 0).
/// Used by all four assembler backends (x86, i686, ARM, RISC-V).
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
            || c == b'!'
        {
            tokens.push(ExprToken::Op(c as char));
            i += 1;
        } else if c == b'<' && i + 1 < bytes.len() && bytes[i + 1] == b'<' {
            tokens.push(ExprToken::Op2("<<"));
            i += 2;
        } else if c == b'>' && i + 1 < bytes.len() && bytes[i + 1] == b'>' {
            tokens.push(ExprToken::Op2(">>"));
            i += 2;
        } else if c.is_ascii_digit() {
            let start = i;
            if c == b'0' && i + 1 < bytes.len() && (bytes[i + 1] == b'x' || bytes[i + 1] == b'X') {
                i += 2;
                while i < bytes.len() && bytes[i].is_ascii_hexdigit() { i += 1; }
            } else if c == b'0' && i + 1 < bytes.len() && (bytes[i + 1] == b'b' || bytes[i + 1] == b'B') {
                i += 2;
                while i < bytes.len() && (bytes[i] == b'0' || bytes[i] == b'1') { i += 1; }
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
            ExprToken::Op2(">>") => { *pos += 1; val = ((val as u64) >> eval_add(tokens, pos)?) as i64; }
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
        ExprToken::Op('!') => { *pos += 1; Ok(if eval_unary(tokens, pos)? == 0 { 1 } else { 0 }) }
        ExprToken::Op('(') => {
            *pos += 1;
            let val = eval_tokens(tokens, pos)?;
            if *pos < tokens.len() && matches!(&tokens[*pos], ExprToken::Op(')')) {
                *pos += 1;
            } else {
                return Err("missing closing parenthesis".to_string());
            }
            Ok(val)
        }
        ExprToken::Num(v) => { let v = *v; *pos += 1; Ok(v) }
        other => Err(format!("unexpected token in expression: {:?}", other)),
    }
}

/// Parse a single integer value (no arithmetic expressions).
/// Supports decimal, hex (0x/0X), binary (0b/0B), and octal (leading 0).
fn parse_single_integer(s: &str) -> Result<i64, String> {
    let s = s.trim();
    if s.is_empty() {
        return Err("empty integer".to_string());
    }

    let (negative, s) = if let Some(rest) = s.strip_prefix('-') {
        (true, rest)
    } else {
        (false, s)
    };

    let val = if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
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
        // Octal (must be checked before decimal to handle leading-zero literals)
        i64::from_str_radix(s, 8)
            .map_err(|_| format!("bad octal: {}", s))?
    } else {
        // Try decimal, including u64 range for large unsigned values
        if let Ok(val) = s.parse::<i64>() {
            if negative {
                return Ok(-val);
            }
            return Ok(val);
        }
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

/// Parse an integer expression with full operator precedence.
/// Supports: |, ^, &, <<, >>, +, -, *, /, %, ~, parentheses.
/// Falls back to simple integer parsing for plain numbers.
pub fn parse_integer_expr(s: &str) -> Result<i64, String> {
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
    if pos < tokens.len() {
        return Err(format!("unexpected trailing token in expression: {:?}", tokens[pos]));
    }
    Ok(val)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_integers() {
        assert_eq!(parse_integer_expr("42").unwrap(), 42);
        assert_eq!(parse_integer_expr("-1").unwrap(), -1);
        assert_eq!(parse_integer_expr("0xFF").unwrap(), 255);
        assert_eq!(parse_integer_expr("0b1010").unwrap(), 10);
        assert_eq!(parse_integer_expr("0644").unwrap(), 420);
    }

    #[test]
    fn test_arithmetic() {
        assert_eq!(parse_integer_expr("8 * 16 + 8 * 8").unwrap(), 192);
        assert_eq!(parse_integer_expr("(8 * 16 + 8 * 8)").unwrap(), 192);
        assert_eq!(parse_integer_expr("8*2 + (8 * 16 + 8 * 8) + 64").unwrap(), 272);
        assert_eq!(parse_integer_expr("-(8 * 8 + 8 * 8 + 16)").unwrap(), -144);
        assert_eq!(parse_integer_expr("-(8 * 8 + 8 * 8 + 16)+0*8").unwrap(), -144);
    }

    #[test]
    fn test_bitwise() {
        assert_eq!(parse_integer_expr("0x100 | 0x4000 | 17").unwrap(), 0x4111);
        assert_eq!(parse_integer_expr("0xFF & 0x0F").unwrap(), 0x0F);
        assert_eq!(parse_integer_expr("1 << 5").unwrap(), 32);
        assert_eq!(parse_integer_expr("~0").unwrap(), -1);
        assert_eq!(parse_integer_expr("0xFF ^ 0x0F").unwrap(), 0xF0);
    }

    #[test]
    fn test_logical_not() {
        // GAS logical NOT: !0 = 1, !1 = 0, !nonzero = 0
        assert_eq!(parse_integer_expr("!0").unwrap(), 1);
        assert_eq!(parse_integer_expr("!1").unwrap(), 0);
        assert_eq!(parse_integer_expr("!42").unwrap(), 0);
        // Used in FFmpeg: 1+!0 = 2
        assert_eq!(parse_integer_expr("1+!0").unwrap(), 2);
        assert_eq!(parse_integer_expr("1+!1").unwrap(), 1);
    }

    #[test]
    fn test_complex() {
        // libffi CALL_CONTEXT_SIZE = (N_V_ARG_REG * 16 + N_X_ARG_REG * 8) where N_V=8, N_X=8
        assert_eq!(parse_integer_expr("(8 * 16 + 8 * 8)").unwrap(), 192);
        // ffi_closure_SYSV_FS = (8*2 + CALL_CONTEXT_SIZE + 64)
        assert_eq!(parse_integer_expr("(8*2 + (8 * 16 + 8 * 8) + 64)").unwrap(), 272);
        // musl vfork: CLONE_VM | CLONE_VFORK | SIGCHLD
        assert_eq!(parse_integer_expr("0x100 | 0x4000 | 17").unwrap(), 0x4111);
    }

    #[test]
    fn test_operator_precedence() {
        // * binds tighter than +
        assert_eq!(parse_integer_expr("2 + 3 * 4").unwrap(), 14);
        assert_eq!(parse_integer_expr("3 * 4 + 2").unwrap(), 14);
        // | binds looser than +
        assert_eq!(parse_integer_expr("1 | 2 + 4").unwrap(), 7); // 1 | (2+4) = 1 | 6 = 7
    }

}
