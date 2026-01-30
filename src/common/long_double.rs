//! Long double precision support.
//!
//! On x86-64, `long double` is 80-bit x87 extended precision (stored in 16 bytes with 6 padding bytes).
//! On AArch64/RISC-V, `long double` is IEEE 754 binary128 (quad precision, 16 bytes).
//!
//! This module provides:
//! - Parsing decimal/hex strings to x87 80-bit or f128 128-bit format
//! - Conversion between x87, f128, and f64 formats
//! - Full-precision arithmetic on x87 (via inline asm on x86-64) and f128 (software)
//! - Conversion from floating-point bytes to integer types
//!
//! # Architecture
//!
//! Both x87 and f128 formats use 16383-biased exponents (15-bit exponent field).
//! The key structural difference: x87 has an explicit integer bit in the mantissa
//! (64 bits total), while f128 uses an implicit leading 1 (112-bit stored mantissa).
//!
//! Shared decompose helpers (`x87_decompose`, `f64_decompose`, `f128_decompose`)
//! extract (sign, exponent, mantissa) tuples, eliminating boilerplate repetition
//! across the many conversion and arithmetic functions.

/// Result of preprocessing a long double string literal.
enum PreparsedFloat<'a> {
    /// Hex float (sign, text including "0x" prefix)
    Hex(bool, &'a str),
    /// Infinity
    Infinity(bool),
    /// NaN
    NaN(bool),
    /// Decimal float (sign, text without suffix/sign)
    Decimal(bool, &'a str),
}

/// Preprocess a long double string: strip suffix, detect sign, classify format.
/// Shared by both x87 and f128 parsing entry points.
fn preparse_long_double(text: &str) -> PreparsedFloat<'_> {
    let text = text.trim();
    let text = if text.ends_with('L') || text.ends_with('l') {
        &text[..text.len() - 1]
    } else {
        text
    };

    let (negative, text) = if let Some(rest) = text.strip_prefix('-') {
        (true, rest)
    } else if let Some(rest) = text.strip_prefix('+') {
        (false, rest)
    } else {
        (false, text)
    };

    if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
        return PreparsedFloat::Hex(negative, text);
    }

    let text_lower = text.to_ascii_lowercase();
    if text_lower == "inf" || text_lower == "infinity" {
        return PreparsedFloat::Infinity(negative);
    }
    if text_lower == "nan" || text_lower.starts_with("nan(") {
        return PreparsedFloat::NaN(negative);
    }

    PreparsedFloat::Decimal(negative, text)
}

// Simple big integer using Vec<u32> limbs (little-endian: limbs[0] is least significant)
struct BigUint {
    limbs: Vec<u32>,
}

impl BigUint {
    fn from_decimal_digits(digits: &[u8]) -> Self {
        // Convert decimal digits to binary limbs
        let mut limbs = vec![0u32];
        for &d in digits {
            // Multiply by 10 and add digit
            let mut carry: u64 = d as u64;
            for limb in limbs.iter_mut() {
                let val = (*limb as u64) * 10 + carry;
                *limb = val as u32;
                carry = val >> 32;
            }
            if carry > 0 {
                limbs.push(carry as u32);
            }
        }
        BigUint { limbs }
    }

    fn is_zero(&self) -> bool {
        self.limbs.iter().all(|&l| l == 0)
    }

    fn mul_u32(&mut self, factor: u32) {
        let mut carry: u64 = 0;
        for limb in self.limbs.iter_mut() {
            let val = (*limb as u64) * (factor as u64) + carry;
            *limb = val as u32;
            carry = val >> 32;
        }
        if carry > 0 {
            self.limbs.push(carry as u32);
        }
    }

    /// Shift left by n bits.
    fn shl(&mut self, n: u32) {
        if n == 0 || self.is_zero() {
            return;
        }
        let word_shift = (n / 32) as usize;
        let bit_shift = n % 32;

        if word_shift > 0 {
            let old_len = self.limbs.len();
            self.limbs.resize(old_len + word_shift, 0);
            // Shift words
            for i in (0..old_len).rev() {
                self.limbs[i + word_shift] = self.limbs[i];
            }
            for i in 0..word_shift {
                self.limbs[i] = 0;
            }
        }

        if bit_shift > 0 {
            let mut carry: u32 = 0;
            for limb in self.limbs.iter_mut() {
                let new_carry = *limb >> (32 - bit_shift);
                *limb = (*limb << bit_shift) | carry;
                carry = new_carry;
            }
            if carry > 0 {
                self.limbs.push(carry);
            }
        }
    }

    /// Get the number of significant bits.
    fn bit_length(&self) -> u32 {
        if self.is_zero() {
            return 0;
        }
        let top_limb = *self.limbs.last().unwrap();
        let top_bits = 32 - top_limb.leading_zeros();
        (self.limbs.len() as u32 - 1) * 32 + top_bits
    }

    /// Extract the top N bits (up to 128), with the MSB at bit (N-1).
    /// Returns (top_val, bits_shifted) where the value is approximately top_val * 2^bits_shifted.
    fn top_n_bits(&self, n: u32) -> (u128, i32) {
        assert!(n > 0 && n <= 128);
        let bl = self.bit_length();
        if bl == 0 {
            return (0, 0);
        }
        if bl <= n {
            // Value fits in n bits
            let mut val: u128 = 0;
            for (i, &limb) in self.limbs.iter().enumerate() {
                val |= (limb as u128) << (i * 32);
            }
            return (val, 0);
        }

        // We need bits [bl-1 .. bl-n] of the big integer
        let shift = bl - n;
        let word_shift = (shift / 32) as usize;
        let bit_shift = shift % 32;

        let mut val: u128 = 0;
        // We need up to (n/32 + 2) limbs due to bit_shift straddling
        let limbs_needed = (n / 32 + 2) as usize;
        for j in 0..limbs_needed {
            let idx = word_shift + j;
            if idx < self.limbs.len() {
                let limb = self.limbs[idx] as u128;
                if j == 0 {
                    val |= limb >> bit_shift;
                } else {
                    let bit_pos = j as u32 * 32 - bit_shift;
                    if bit_pos < 128 {
                        val |= limb << bit_pos;
                    }
                }
            }
        }
        // Mask to n bits
        if n < 128 {
            val &= (1u128 << n) - 1;
        }

        (val, shift as i32)
    }

    /// Divide self by other (big integer), returning quotient. Self is modified to become quotient.
    /// This is used for dividing by large powers of 10.
    fn div_big(dividend: &BigUint, divisor: &BigUint) -> BigUint {
        if divisor.is_zero() {
            return BigUint { limbs: vec![0] };
        }

        let d_bits = dividend.bit_length();
        let v_bits = divisor.bit_length();

        if d_bits < v_bits {
            return BigUint { limbs: vec![0] };
        }

        // Simple long division: shift divisor up, subtract repeatedly
        // For our purposes, we only need ~70 bits of quotient precision
        let mut quotient_limbs = vec![0u32; ((d_bits - v_bits) / 32 + 2) as usize];
        let mut remainder = dividend.limbs.clone();

        // Process from MSB down
        let shift_max = d_bits - v_bits;
        for shift in (0..=shift_max).rev() {
            // Check if (divisor << shift) <= remainder
            if cmp_shifted(&remainder, &divisor.limbs, shift) {
                // Subtract divisor << shift from remainder
                sub_shifted(&mut remainder, &divisor.limbs, shift);
                // Set bit in quotient
                let word = (shift / 32) as usize;
                let bit = shift % 32;
                if word < quotient_limbs.len() {
                    quotient_limbs[word] |= 1u32 << bit;
                }
            }
        }

        // Strip leading zeros
        while quotient_limbs.len() > 1 && *quotient_limbs.last().unwrap() == 0 {
            quotient_limbs.pop();
        }

        BigUint { limbs: quotient_limbs }
    }
}

/// Compare remainder >= divisor_limbs << shift
fn cmp_shifted(remainder: &[u32], divisor: &[u32], shift: u32) -> bool {
    let word_shift = (shift / 32) as usize;
    let bit_shift = shift % 32;

    // Find the top word of the shifted divisor
    let div_top = divisor.len() + word_shift + if bit_shift > 0 { 1 } else { 0 };

    if remainder.len() > div_top {
        // Remainder has more limbs, check if upper limbs are non-zero
        for i in div_top..remainder.len() {
            if remainder[i] != 0 {
                return true;
            }
        }
    }

    // Compare from MSB
    for i in (0..div_top.max(remainder.len())).rev() {
        let r = if i < remainder.len() { remainder[i] } else { 0 };
        // Get the shifted divisor bit at position i
        let d = shifted_limb(divisor, i, word_shift, bit_shift);
        if r > d {
            return true;
        }
        if r < d {
            return false;
        }
    }
    true // equal
}

/// Get limb `i` of (divisor << shift)
fn shifted_limb(divisor: &[u32], i: usize, word_shift: usize, bit_shift: u32) -> u32 {
    if i < word_shift {
        return 0;
    }
    let di = i - word_shift;
    if bit_shift == 0 {
        if di < divisor.len() { divisor[di] } else { 0 }
    } else {
        let lo = if di < divisor.len() { divisor[di] } else { 0 };
        let hi = if di > 0 && di - 1 < divisor.len() { divisor[di - 1] } else { 0 };
        (lo << bit_shift) | (hi >> (32 - bit_shift))
    }
}

/// Subtract divisor << shift from remainder (in place).
fn sub_shifted(remainder: &mut [u32], divisor: &[u32], shift: u32) {
    let word_shift = (shift / 32) as usize;
    let bit_shift = shift % 32;

    let mut borrow: i64 = 0;
    for i in word_shift..remainder.len() {
        let d = shifted_limb(divisor, i, word_shift, bit_shift) as i64;
        let val = remainder[i] as i64 - d - borrow;
        if val < 0 {
            remainder[i] = (val + (1i64 << 32)) as u32;
            borrow = 1;
        } else {
            remainder[i] = val as u32;
            borrow = 0;
        }
    }
}

/// Build a big integer for 10^n
fn pow10_big(n: u32) -> BigUint {
    let mut result = BigUint { limbs: vec![1] };
    // Multiply by 10 in chunks for efficiency
    let mut remaining = n;
    // Use 10^9 = 1_000_000_000 as a chunk (fits in u32)
    while remaining >= 9 {
        result.mul_u32(1_000_000_000);
        remaining -= 9;
    }
    // Remaining powers
    let small_pow10: [u32; 10] = [1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000];
    if remaining > 0 {
        result.mul_u32(small_pow10[remaining as usize]);
    }
    result
}

/// Parsed decimal float: digits and exponent extracted from a decimal float string.
/// Used by the shared parsing pipeline for both x87 and f128 formats.
struct ParsedDecimal {
    digits: Vec<u8>,
    decimal_exp: i32,
}

/// Parse a decimal float string into digits and decimal exponent.
/// Returns None if the string represents zero.
/// This is the shared front-end for both x87 and f128 decimal parsing.
fn parse_decimal_digits(text: &str, capacity: usize) -> Option<ParsedDecimal> {
    let bytes = text.as_bytes();

    let mut digits: Vec<u8> = Vec::with_capacity(capacity);
    let mut decimal_point_offset: Option<usize> = None;
    let mut i = 0;

    while i < bytes.len() {
        if bytes[i] == b'.' {
            decimal_point_offset = Some(digits.len());
            i += 1;
        } else if bytes[i].is_ascii_digit() {
            digits.push(bytes[i] - b'0');
            i += 1;
        } else {
            break;
        }
    }

    // Parse optional exponent
    let mut exp10: i32 = 0;
    if i < bytes.len() && (bytes[i] == b'e' || bytes[i] == b'E') {
        i += 1;
        let exp_neg = if i < bytes.len() && bytes[i] == b'-' {
            i += 1;
            true
        } else {
            if i < bytes.len() && bytes[i] == b'+' {
                i += 1;
            }
            false
        };
        while i < bytes.len() && bytes[i].is_ascii_digit() {
            exp10 = exp10.saturating_mul(10).saturating_add((bytes[i] - b'0') as i32);
            i += 1;
        }
        if exp_neg {
            exp10 = -exp10;
        }
    }

    let frac_digits = if let Some(dp) = decimal_point_offset {
        (digits.len() - dp) as i32
    } else {
        0
    };
    let decimal_exp = exp10 - frac_digits;

    // Strip leading zeros
    while digits.len() > 1 && digits[0] == 0 {
        digits.remove(0);
    }

    if digits.is_empty() || (digits.len() == 1 && digits[0] == 0) {
        return None;
    }

    Some(ParsedDecimal { digits, decimal_exp })
}

/// Shared decimal-to-float bigint conversion for f128 (113-bit mantissa).
fn decimal_to_float_bigint_f128(negative: bool, digits: &[u8], decimal_exp: i32) -> [u8; 16] {
    if decimal_exp >= 0 {
        let mut big_val = BigUint::from_decimal_digits(digits);
        let p10 = pow10_big(decimal_exp as u32);
        big_val = mul_big(&big_val, &p10);

        if big_val.is_zero() {
            return make_f128_zero(negative);
        }

        let (top113, shift) = big_val.top_n_bits(113);
        if top113 == 0 {
            return make_f128_zero(negative);
        }

        let lz = top113.leading_zeros() - (128 - 113);
        let mantissa113 = top113 << lz;
        let binary_exp = shift + 112 - lz as i32;
        encode_f128(negative, binary_exp, mantissa113)
    } else {
        let neg_exp = (-decimal_exp) as u32;
        let big_d = BigUint::from_decimal_digits(digits);
        if big_d.is_zero() {
            return make_f128_zero(negative);
        }

        let extra_bits = (128 + neg_exp * 4).min(200000);
        let mut shifted_d = big_d;
        shifted_d.shl(extra_bits);

        let p10 = pow10_big(neg_exp);
        let quotient = BigUint::div_big(&shifted_d, &p10);
        if quotient.is_zero() {
            return make_f128_zero(negative);
        }

        let (top113, shift) = quotient.top_n_bits(113);
        if top113 == 0 {
            return make_f128_zero(negative);
        }

        let lz = top113.leading_zeros() - (128 - 113);
        let mantissa113 = top113 << lz;
        let binary_exp = shift + 112 - lz as i32 - extra_bits as i32;
        encode_f128(negative, binary_exp, mantissa113)
    }
}

/// Multiply two big integers.
fn mul_big(a: &BigUint, b: &BigUint) -> BigUint {
    let mut result = vec![0u32; a.limbs.len() + b.limbs.len()];
    for (i, &al) in a.limbs.iter().enumerate() {
        let mut carry: u64 = 0;
        for (j, &bl) in b.limbs.iter().enumerate() {
            let val = result[i + j] as u64 + (al as u64) * (bl as u64) + carry;
            result[i + j] = val as u32;
            carry = val >> 32;
        }
        if carry > 0 {
            result[i + b.limbs.len()] += carry as u32;
        }
    }
    while result.len() > 1 && *result.last().unwrap() == 0 {
        result.pop();
    }
    BigUint { limbs: result }
}

fn make_x87_zero(negative: bool) -> [u8; 16] {
    let mut bytes = [0u8; 16];
    if negative {
        bytes[9] = 0x80;
    }
    bytes
}

fn make_x87_infinity(negative: bool) -> [u8; 16] {
    let mut bytes = [0u8; 16];
    bytes[7] = 0x80; // integer bit set, fraction = 0
    bytes[8] = 0xFF;
    bytes[9] = 0x7F | (if negative { 0x80 } else { 0 });
    bytes
}

fn make_x87_nan(negative: bool) -> [u8; 16] {
    let mut bytes = [0u8; 16];
    // quiet NaN: integer bit set, top fraction bit set
    bytes[7] = 0xC0;
    bytes[8] = 0xFF;
    bytes[9] = 0x7F | (if negative { 0x80 } else { 0 });
    bytes
}

// =============================================================================
// Decompose helpers: extract (sign, exponent, mantissa) from raw bytes
// =============================================================================

/// Decomposed x87 80-bit extended precision value.
/// For normal numbers: `mantissa` has bit 63 set (the explicit integer bit).
/// Biased exponent uses the standard 16383 bias.
struct X87Decomposed {
    sign: bool,
    /// Biased exponent (0 = zero/subnormal, 0x7FFF = inf/NaN).
    biased_exp: u16,
    /// Full 64-bit mantissa including explicit integer bit.
    mantissa: u64,
}

impl X87Decomposed {
    fn is_zero(&self) -> bool { self.biased_exp == 0 && self.mantissa == 0 }
    fn is_special(&self) -> bool { self.biased_exp == 0x7FFF }
    fn is_inf(&self) -> bool { self.is_special() && (self.mantissa & 0x3FFF_FFFF_FFFF_FFFF) == 0 }
    fn unbiased_exp(&self) -> i32 { self.biased_exp as i32 - 16383 }
}

/// Decompose x87 80-bit extended precision bytes into sign, exponent, and mantissa.
/// This is the single extraction point used by all x87 conversion functions.
fn x87_decompose(bytes: &[u8; 16]) -> X87Decomposed {
    let mantissa = u64::from_le_bytes(bytes[0..8].try_into().unwrap());
    let exp_sign = u16::from_le_bytes([bytes[8], bytes[9]]);
    X87Decomposed {
        sign: (exp_sign >> 15) & 1 == 1,
        biased_exp: exp_sign & 0x7FFF,
        mantissa,
    }
}

/// Decomposed f64 value.
struct F64Decomposed {
    sign: bool,
    /// Biased exponent (0 = zero/subnormal, 0x7FF = inf/NaN).
    biased_exp: u16,
    /// 52-bit stored mantissa (no implicit leading 1).
    mantissa: u64,
}

impl F64Decomposed {
    fn is_zero(&self) -> bool { self.biased_exp == 0 && self.mantissa == 0 }
    fn is_special(&self) -> bool { self.biased_exp == 0x7FF }
    fn is_inf(&self) -> bool { self.is_special() && self.mantissa == 0 }
}

/// Decompose an f64 into sign, exponent, and mantissa.
fn f64_decompose(val: f64) -> F64Decomposed {
    let bits = val.to_bits();
    F64Decomposed {
        sign: (bits >> 63) & 1 == 1,
        biased_exp: ((bits >> 52) & 0x7FF) as u16,
        mantissa: bits & 0x000F_FFFF_FFFF_FFFF,
    }
}

/// Encode x87 80-bit extended precision bytes from sign, biased exponent, and mantissa.
fn x87_encode(sign: bool, biased_exp: u16, mantissa64: u64) -> [u8; 16] {
    let mut bytes = [0u8; 16];
    bytes[..8].copy_from_slice(&mantissa64.to_le_bytes());
    let exp_sign = biased_exp | (if sign { 0x8000 } else { 0 });
    bytes[8] = (exp_sign & 0xFF) as u8;
    bytes[9] = (exp_sign >> 8) as u8;
    bytes
}

/// Convert x87 80-bit bytes back to f64 (lossy - for computations that need f64).
/// `bytes[0..10]` contain the x87 extended value in little-endian.
pub fn x87_bytes_to_f64(bytes: &[u8; 16]) -> f64 {
    let d = x87_decompose(bytes);
    let sign_u64 = if d.sign { 1u64 } else { 0u64 };

    if d.is_zero() {
        return if d.sign { -0.0 } else { 0.0 };
    }
    if d.is_special() {
        if d.is_inf() {
            return if d.sign { f64::NEG_INFINITY } else { f64::INFINITY };
        }
        return f64::NAN;
    }

    // Normal number
    // x87: value = mantissa64 * 2^(exp15 - 16383 - 63)
    // f64: value = (1 + mantissa52/2^52) * 2^(exp11 - 1023)
    let unbiased = d.unbiased_exp();

    // f64 exponent range: -1022 to 1023
    if unbiased > 1023 {
        return if d.sign { f64::NEG_INFINITY } else { f64::INFINITY };
    }
    if unbiased < -1074 {
        return if d.sign { -0.0 } else { 0.0 };
    }

    // Extract top 52 mantissa bits (below the integer bit)
    // mantissa64 bit 63 = integer bit (=1 for normals)
    // We want bits 62..11 (52 bits) for the f64 mantissa
    let mantissa52 = (d.mantissa >> 11) & 0x000F_FFFF_FFFF_FFFF;

    // Round to nearest: check bit 10 (the first dropped bit)
    let round_bit = (d.mantissa >> 10) & 1;
    let sticky = d.mantissa & 0x3FF; // bits 9..0
    let mantissa52 = if round_bit == 1 && (sticky != 0 || mantissa52 & 1 != 0) {
        mantissa52 + 1
    } else {
        mantissa52
    };

    // Handle mantissa overflow from rounding
    if mantissa52 > 0x000F_FFFF_FFFF_FFFF {
        let f64_biased_exp = (unbiased + 1024) as u64;
        if f64_biased_exp >= 0x7FF {
            return if d.sign { f64::NEG_INFINITY } else { f64::INFINITY };
        }
        let f64_bits = (sign_u64 << 63) | (f64_biased_exp << 52);
        return f64::from_bits(f64_bits);
    }

    if unbiased >= -1022 {
        let f64_biased_exp = (unbiased + 1023) as u64;
        let f64_bits = (sign_u64 << 63) | (f64_biased_exp << 52) | mantissa52;
        f64::from_bits(f64_bits)
    } else {
        // Subnormal in f64 - not common for constants, just convert approximately
        let val = d.mantissa as f64 * 2.0_f64.powi(unbiased - 63);
        if d.sign { -val } else { val }
    }
}

/// Convert x87 80-bit bytes `[u8; 16]` to IEEE 754 binary128 bytes (16 bytes, little-endian).
/// Used for ARM64/RISC-V long double emission.
pub fn x87_bytes_to_f128_bytes(x87: &[u8; 16]) -> [u8; 16] {
    let d = x87_decompose(x87);

    if d.is_zero() {
        return make_f128_zero(d.sign);
    }
    if d.is_special() {
        return if d.is_inf() { make_f128_infinity(d.sign) } else { make_f128_nan(d.sign) };
    }

    // Normal number
    // x87 and f128 both use exponent bias 16383, so exponent bits are the same!
    // x87 mantissa: 64 bits with explicit integer bit at position 63
    // f128 mantissa: 112 bits, implicit leading 1 (no integer bit stored)
    // Take lower 63 bits of x87 mantissa and shift left by (112-63) = 49
    let mantissa_no_int = d.mantissa & 0x7FFF_FFFF_FFFF_FFFF;
    let mantissa112: u128 = (mantissa_no_int as u128) << 49;
    let sign_bit: u128 = if d.sign { 1u128 << 127 } else { 0 };
    let val: u128 = mantissa112 | ((d.biased_exp as u128) << 112) | sign_bit;
    val.to_le_bytes()
}

// =============================================================================
// IEEE 754 binary128 (f128) native functions
// =============================================================================

/// Parse a float string directly to IEEE 754 binary128 (f128) bytes with full 112-bit
/// mantissa precision. Used for long double constants on ARM64/RISC-V where long double
/// is quad precision.
///
/// f128 has 112 bits of mantissa (vs 64 for x87), so parsing directly to f128
/// preserves more precision than parsing to x87 and converting.
pub fn parse_long_double_to_f128_bytes(text: &str) -> [u8; 16] {
    match preparse_long_double(text) {
        PreparsedFloat::Hex(neg, text) => parse_hex_float_to_f128(neg, text),
        PreparsedFloat::Infinity(neg) => make_f128_infinity(neg),
        PreparsedFloat::NaN(neg) => make_f128_nan(neg),
        PreparsedFloat::Decimal(neg, text) => parse_decimal_float_to_f128(neg, text),
    }
}

/// Parse a decimal float string to IEEE 754 binary128 format.
fn parse_decimal_float_to_f128(negative: bool, text: &str) -> [u8; 16] {
    match parse_decimal_digits(text, 40) {
        Some(parsed) => decimal_to_float_bigint_f128(negative, &parsed.digits, parsed.decimal_exp),
        None => make_f128_zero(negative),
    }
}


/// Encode an IEEE 754 binary128 value from sign, binary exponent, and 113-bit mantissa.
/// `binary_exp` is the exponent of the MSB (bit 112) of `mantissa113`.
/// That is, value = mantissa113 * 2^(binary_exp - 112).
fn encode_f128(negative: bool, binary_exp: i32, mantissa113: u128) -> [u8; 16] {
    if mantissa113 == 0 {
        return make_f128_zero(negative);
    }

    // f128 format: biased_exponent = unbiased_exponent + 16383
    let biased_exp = binary_exp + 16383;

    if biased_exp >= 0x7FFF {
        return make_f128_infinity(negative);
    }

    if biased_exp <= 0 {
        // Subnormal or underflow
        let shift = 1 - biased_exp;
        if shift >= 113 {
            return make_f128_zero(negative);
        }
        let mantissa_denorm = mantissa113 >> shift as u32;
        // f128: implicit bit is NOT stored, mantissa is bits [111:0]
        let mantissa_stored = mantissa_denorm & ((1u128 << 112) - 1);
        let sign_bit = if negative { 1u128 << 127 } else { 0 };
        let val = sign_bit | mantissa_stored;
        return val.to_le_bytes();
    }

    let exp15 = biased_exp as u128;
    // Remove implicit integer bit (bit 112), store only lower 112 bits
    let mantissa_stored = mantissa113 & ((1u128 << 112) - 1);
    let sign_bit = if negative { 1u128 << 127 } else { 0 };
    let val = sign_bit | (exp15 << 112) | mantissa_stored;
    val.to_le_bytes()
}

/// Parse a hex float string (0x..p..) to f128 format.
fn parse_hex_float_to_f128(negative: bool, text: &str) -> [u8; 16] {
    // Skip "0x" or "0X"
    let text = &text[2..];

    let bytes = text.as_bytes();
    let mut hex_digits: Vec<u8> = Vec::with_capacity(32);
    let mut decimal_point_offset: Option<usize> = None;
    let mut i = 0;

    while i < bytes.len() {
        if bytes[i] == b'.' {
            decimal_point_offset = Some(hex_digits.len());
            i += 1;
        } else if bytes[i].is_ascii_hexdigit() {
            let d = if bytes[i] >= b'0' && bytes[i] <= b'9' {
                bytes[i] - b'0'
            } else if bytes[i] >= b'a' && bytes[i] <= b'f' {
                bytes[i] - b'a' + 10
            } else {
                bytes[i] - b'A' + 10
            };
            hex_digits.push(d);
            i += 1;
        } else {
            break;
        }
    }

    // Parse binary exponent (p/P)
    let mut exp2: i32 = 0;
    if i < bytes.len() && (bytes[i] == b'p' || bytes[i] == b'P') {
        i += 1;
        let exp_neg = if i < bytes.len() && bytes[i] == b'-' {
            i += 1;
            true
        } else {
            if i < bytes.len() && bytes[i] == b'+' {
                i += 1;
            }
            false
        };
        while i < bytes.len() && bytes[i].is_ascii_digit() {
            exp2 = exp2.saturating_mul(10).saturating_add((bytes[i] - b'0') as i32);
            i += 1;
        }
        if exp_neg {
            exp2 = -exp2;
        }
    }

    // Build the mantissa from hex digits (at most 32 hex digits = 128 bits)
    let mut mantissa: u128 = 0;
    let mut bits_read: u32 = 0;
    for &d in &hex_digits {
        if bits_read + 4 <= 128 {
            mantissa = (mantissa << 4) | (d as u128);
            bits_read += 4;
        }
    }

    if mantissa == 0 {
        return make_f128_zero(negative);
    }

    // Each hex digit after the point contributes 4 bits of binary fraction
    let frac_hex_digits = if let Some(dp) = decimal_point_offset {
        (hex_digits.len() - dp) as i32
    } else {
        0
    };
    let binary_exp = exp2 - frac_hex_digits * 4;

    // Normalize mantissa to have bit 112 set (for 113-bit mantissa).
    // The unbiased IEEE exponent is: binary_exp + (bl - 1), where bl is the
    // actual bit length of the mantissa. This accounts for the position of the
    // most significant bit relative to the binary point.
    // Note: we use bl (actual bit length) not bits_used (total hex digit bits),
    // since leading zero hex digits don't affect the exponent.
    let bl = 128 - mantissa.leading_zeros();
    let adj_exp = binary_exp + (bl as i32 - 1);
    if bl > 113 {
        // Too many bits, shift right (loses precision)
        let excess = bl - 113;
        mantissa >>= excess;
        encode_f128(negative, adj_exp, mantissa)
    } else if bl < 113 && bl > 0 {
        let deficit = 113 - bl;
        mantissa <<= deficit;
        encode_f128(negative, adj_exp, mantissa)
    } else {
        encode_f128(negative, adj_exp, mantissa)
    }
}

/// Construct raw f128 bytes from sign, biased exponent, and stored mantissa (no implicit bit).
fn make_f128_raw(negative: bool, biased_exp: u16, stored_mantissa: u128) -> [u8; 16] {
    let sign_bit: u128 = if negative { 1u128 << 127 } else { 0 };
    let val: u128 = sign_bit | ((biased_exp as u128) << 112) | stored_mantissa;
    val.to_le_bytes()
}

fn make_f128_zero(negative: bool) -> [u8; 16] {
    make_f128_raw(negative, 0, 0)
}

fn make_f128_infinity(negative: bool) -> [u8; 16] {
    make_f128_raw(negative, 0x7FFF, 0)
}

fn make_f128_nan(negative: bool) -> [u8; 16] {
    make_f128_raw(negative, 0x7FFF, 1u128 << 111)
}

/// Convert f128 bytes to x87 80-bit bytes. This is a lossy narrowing conversion
/// (112-bit mantissa → 64-bit mantissa) used when x87 format is needed (x86 backend,
/// x87 FPU constant folding).
pub fn f128_bytes_to_x87_bytes(f128_bytes: &[u8; 16]) -> [u8; 16] {
    let (sign, biased_exp, mantissa) = f128_decompose(f128_bytes);

    if biased_exp == 0 && mantissa == 0 {
        return make_x87_zero(sign);
    }
    if biased_exp == 0x7FFF {
        return if mantissa == 0 { make_x87_infinity(sign) } else { make_x87_nan(sign) };
    }

    // Normal number
    // f128 mantissa: 112 bits, implicit leading 1 (bit 112 set by f128_decompose)
    // x87 mantissa: 64 bits, explicit leading 1
    // Take top 64 bits of the 113-bit mantissa (mantissa >> 49)
    // Exponent bias is the same for both formats (16383)
    let mantissa64 = (mantissa >> 49) as u64;
    x87_encode(sign, biased_exp as u16, mantissa64)
}

/// Convert f128 bytes to f64 (lossy narrowing).
pub fn f128_bytes_to_f64(f128_bytes: &[u8; 16]) -> f64 {
    let (sign, biased_exp, mantissa) = f128_decompose(f128_bytes);
    let sign_u64 = if sign { 1u64 } else { 0u64 };

    if biased_exp == 0 && mantissa == 0 {
        return if sign { -0.0 } else { 0.0 };
    }
    if biased_exp == 0x7FFF {
        return if mantissa == 0 {
            if sign { f64::NEG_INFINITY } else { f64::INFINITY }
        } else {
            f64::NAN
        };
    }

    // Normal f128: value = (-1)^sign * 2^(biased_exp - 16383) * (1 + stored_mantissa/2^112)
    // Note: f128_decompose already adds the implicit bit, so mantissa has bit 112 set.
    let unbiased = (biased_exp - 16383) as i64;

    if (-1022..=1023).contains(&unbiased) {
        let f64_biased_exp = (unbiased + 1023) as u64;
        // Take top 52 bits of the 112-bit stored mantissa (strip implicit bit first)
        let stored = mantissa & ((1u128 << 112) - 1);
        let mantissa52 = (stored >> 60) as u64;
        let f64_bits = (sign_u64 << 63) | (f64_biased_exp << 52) | mantissa52;
        f64::from_bits(f64_bits)
    } else if unbiased > 1023 {
        if sign { f64::NEG_INFINITY } else { f64::INFINITY }
    } else {
        // Subnormal in f64
        let val = mantissa as f64 * 2.0_f64.powi(unbiased as i32 - 112);
        if sign { -val } else { val }
    }
}

/// Convert a signed i64 to f128 bytes with full precision.
/// f128 has 112-bit mantissa, so all i64 values are representable exactly.
pub fn i64_to_f128_bytes(val: i64) -> [u8; 16] {
    if val == 0 {
        return make_f128_zero(false);
    }
    let negative = val < 0;
    let abs_val: u64 = if val == i64::MIN {
        1u64 << 63
    } else if negative {
        (-val) as u64
    } else {
        val as u64
    };
    u64_to_f128_bytes_with_sign(abs_val, negative)
}

/// Convert an unsigned u64 to f128 bytes with full precision.
pub fn u64_to_f128_bytes(val: u64) -> [u8; 16] {
    if val == 0 {
        return make_f128_zero(false);
    }
    u64_to_f128_bytes_with_sign(val, false)
}

fn u64_to_f128_bytes_with_sign(val: u64, negative: bool) -> [u8; 16] {
    if val == 0 {
        return make_f128_zero(negative);
    }
    let bl = 64 - val.leading_zeros(); // number of significant bits
    // binary_exp = bl - 1 (position of MSB)
    // We need to normalize to 113-bit mantissa with bit 112 set
    let mantissa113: u128 = (val as u128) << (113 - bl);
    let binary_exp = (bl as i32) - 1;
    encode_f128(negative, binary_exp, mantissa113)
}

/// Convert an unsigned u128 to f128 bytes.
/// Values with more than 113 significant bits will be rounded.
pub fn u128_to_f128_bytes(val: u128) -> [u8; 16] {
    if val == 0 {
        return make_f128_zero(false);
    }
    if val <= u64::MAX as u128 {
        return u64_to_f128_bytes(val as u64);
    }
    let bl = 128 - val.leading_zeros(); // number of significant bits
    let mantissa113: u128 = if bl > 113 {
        val >> (bl - 113)
    } else if bl < 113 {
        val << (113 - bl)
    } else {
        val
    };
    let binary_exp = (bl as i32) - 1;
    encode_f128(false, binary_exp, mantissa113)
}

/// Convert a signed i128 to f128 bytes.
pub fn i128_to_f128_bytes(val: i128) -> [u8; 16] {
    if val == 0 {
        return make_f128_zero(false);
    }
    let negative = val < 0;
    let abs_val: u128 = if val == i128::MIN {
        1u128 << 127
    } else if negative {
        (-val) as u128
    } else {
        val as u128
    };
    if abs_val <= u64::MAX as u128 {
        return u64_to_f128_bytes_with_sign(abs_val as u64, negative);
    }
    let bl = 128 - abs_val.leading_zeros();
    let mantissa113: u128 = if bl > 113 {
        abs_val >> (bl - 113)
    } else if bl < 113 {
        abs_val << (113 - bl)
    } else {
        abs_val
    };
    let binary_exp = (bl as i32) - 1;
    encode_f128(negative, binary_exp, mantissa113)
}

/// Convert f64 to f128 bytes (widening, zero-fills extra mantissa bits).
pub fn f64_to_f128_bytes_lossless(val: f64) -> [u8; 16] {
    let d = f64_decompose(val);

    if d.is_zero() {
        return make_f128_zero(d.sign);
    }
    if d.is_special() {
        return if d.is_inf() { make_f128_infinity(d.sign) } else { make_f128_nan(d.sign) };
    }

    // Normal f64: exp = biased_exp - 1023, mantissa = 1.mantissa52
    // f128: exp = biased_exp - 1023 + 16383, mantissa112 = mantissa52 << (112 - 52)
    let exp15 = (d.biased_exp as u128 - 1023 + 16383) as u128;
    let mantissa112: u128 = (d.mantissa as u128) << 60; // 112 - 52 = 60
    let sign_bit: u128 = if d.sign { 1u128 << 127 } else { 0 };
    let val128: u128 = sign_bit | (exp15 << 112) | mantissa112;
    val128.to_le_bytes()
}

// =============================================================================
// f128 to integer conversion: direct extraction without lossy x87 intermediate
// =============================================================================

/// Core f128-to-unsigned-integer conversion. Extracts the absolute value of an f128
/// float as u128, with truncation toward zero. `max_bits` limits the output width.
/// Returns None for inf/NaN/overflow.
fn f128_to_abs_uint(bytes: &[u8; 16], max_bits: u32) -> Option<(bool, u128)> {
    let (sign, biased_exp, mantissa) = f128_decompose(bytes);

    if biased_exp == 0 && mantissa == 0 {
        return Some((sign, 0));
    }
    if biased_exp == 0x7FFF {
        return None; // inf or NaN
    }

    // For normals, f128_decompose returns mantissa with bit 112 set (the implicit bit).
    // value = mantissa * 2^(biased_exp - 16383 - 112)
    let unbiased = biased_exp - 16383;
    let shift = unbiased - 112;

    if shift >= 0 {
        let shift = shift as u32;
        if shift >= max_bits {
            return None; // overflow
        }
        let abs_val = mantissa << shift;
        if max_bits < 128 && abs_val >> max_bits != 0 {
            return None;
        }
        Some((sign, abs_val))
    } else {
        let rshift = (-shift) as u32;
        if rshift >= 128 {
            return Some((sign, 0));
        }
        Some((sign, mantissa >> rshift))
    }
}

/// Convert f128 bytes to i64 (for constant folding). Direct extraction with full
/// 112-bit mantissa precision (no lossy x87 intermediate).
pub fn f128_bytes_to_i64(bytes: &[u8; 16]) -> Option<i64> {
    let (sign, abs_val) = f128_to_abs_uint(bytes, 64)?;
    if sign {
        if abs_val > i64::MAX as u128 + 1 {
            return None;
        }
        Some(-(abs_val as i64))
    } else {
        if abs_val > i64::MAX as u128 {
            return None;
        }
        Some(abs_val as i64)
    }
}

/// Convert f128 bytes to u64 (for constant folding).
pub fn f128_bytes_to_u64(bytes: &[u8; 16]) -> Option<u64> {
    let (sign, abs_val) = f128_to_abs_uint(bytes, 64)?;
    if sign {
        let signed_val = f128_bytes_to_i64(bytes)?;
        return Some(signed_val as u64);
    }
    Some(abs_val as u64)
}

/// Convert f128 bytes to i128 (for constant folding).
pub fn f128_bytes_to_i128(bytes: &[u8; 16]) -> Option<i128> {
    let (sign, abs_val) = f128_to_abs_uint(bytes, 128)?;
    if sign {
        if abs_val > i128::MAX as u128 + 1 {
            return None;
        }
        Some(-(abs_val as i128))
    } else {
        if abs_val > i128::MAX as u128 {
            return None;
        }
        Some(abs_val as i128)
    }
}

/// Convert f128 bytes to u128 (for constant folding).
pub fn f128_bytes_to_u128(bytes: &[u8; 16]) -> Option<u128> {
    let (sign, abs_val) = f128_to_abs_uint(bytes, 128)?;
    if sign {
        let signed_val = f128_bytes_to_i128(bytes)?;
        return Some(signed_val as u128);
    }
    Some(abs_val)
}

/// Create x87 bytes from an f64 value (for when we don't have the original text).
/// This is a widening conversion that zero-fills the extra mantissa bits.
pub fn f64_to_x87_bytes_simple(val: f64) -> [u8; 16] {
    let d = f64_decompose(val);

    if d.is_zero() {
        return make_x87_zero(d.sign);
    }
    if d.is_special() {
        return if d.is_inf() { make_x87_infinity(d.sign) } else { make_x87_nan(d.sign) };
    }

    // Normal f64: x87 mantissa has explicit integer bit at 63, then 52 bits at 62..11
    let exp15 = (d.biased_exp as i32 - 1023 + 16383) as u16;
    let mantissa64 = (1u64 << 63) | (d.mantissa << 11);
    x87_encode(d.sign, exp15, mantissa64)
}

// =============================================================================
// x87 to integer conversion: shared core with type-specific wrappers
// =============================================================================

/// Core x87-to-unsigned-integer conversion. Extracts the absolute value of an x87
/// float as a u128, with truncation toward zero. `max_bits` limits the output width
/// (64 for u64, 128 for u128). Returns None for inf/NaN/overflow.
fn x87_to_abs_uint(d: &X87Decomposed, max_bits: u32) -> Option<u128> {
    if d.is_zero() {
        return Some(0);
    }
    if d.is_special() {
        return None;
    }

    let shift = d.unbiased_exp() - 63;

    if shift >= 0 {
        let shift = shift as u32;
        if shift >= max_bits {
            return None; // overflow
        }
        let abs_val = (d.mantissa as u128) << shift;
        // Check that we didn't overflow: the top bits above max_bits must be zero
        if max_bits < 128 && abs_val >> max_bits != 0 {
            return None;
        }
        Some(abs_val)
    } else {
        let rshift = (-shift) as u32;
        if rshift >= 64 {
            return Some(0); // too small, truncates to 0
        }
        Some((d.mantissa >> rshift) as u128)
    }
}

/// Convert x87 80-bit bytes to i64 with truncation toward zero.
/// This preserves the full 64-bit mantissa precision, unlike going through f64 first.
/// Returns None for infinity, NaN, or values out of i64 range.
pub fn x87_bytes_to_i64(bytes: &[u8; 16]) -> Option<i64> {
    let d = x87_decompose(bytes);
    let abs_val = x87_to_abs_uint(&d, 64)?;
    if d.sign {
        // Special case: abs_val == 2^63 is valid as i64::MIN
        if abs_val > i64::MAX as u128 + 1 {
            return None;
        }
        Some(-(abs_val as i64))
    } else {
        if abs_val > i64::MAX as u128 {
            return None;
        }
        Some(abs_val as i64)
    }
}

/// Convert x87 80-bit bytes to u64 with truncation toward zero.
/// Returns None for infinity, NaN, or values out of u64 range.
/// Negative values: truncate to signed then reinterpret as unsigned (GCC behavior).
pub fn x87_bytes_to_u64(bytes: &[u8; 16]) -> Option<u64> {
    let d = x87_decompose(bytes);
    if d.sign {
        // GCC truncates to signed then reinterprets as unsigned
        let signed_val = x87_bytes_to_i64(bytes)?;
        return Some(signed_val as u64);
    }
    let abs_val = x87_to_abs_uint(&d, 64)?;
    Some(abs_val as u64)
}

/// Convert x87 80-bit bytes to i128 with truncation toward zero.
pub fn x87_bytes_to_i128(bytes: &[u8; 16]) -> Option<i128> {
    let d = x87_decompose(bytes);
    let abs_val = x87_to_abs_uint(&d, 128)?;
    if d.sign {
        if abs_val > i128::MAX as u128 + 1 {
            return None;
        }
        Some(-(abs_val as i128))
    } else {
        if abs_val > i128::MAX as u128 {
            return None;
        }
        Some(abs_val as i128)
    }
}

/// Convert x87 80-bit bytes to u128 with truncation toward zero.
pub fn x87_bytes_to_u128(bytes: &[u8; 16]) -> Option<u128> {
    let d = x87_decompose(bytes);
    if d.sign {
        let signed_val = x87_bytes_to_i128(bytes)?;
        return Some(signed_val as u128);
    }
    x87_to_abs_uint(&d, 128)
}

// ============================================================================
// x87 full-precision arithmetic on raw 80-bit bytes
// ============================================================================
//
// These functions perform arithmetic on x87 80-bit extended precision values
// stored as [u8; 16] byte arrays (10 bytes of x87 data + 6 padding bytes).
// Since the compiler runs on x86-64, we use inline assembly to invoke the
// actual x87 FPU instructions, giving us correct 80-bit precision results
// that match what the generated code will produce at runtime.

/// Emit an x87 binary operation via inline assembly.
/// All four ops (faddp/fsubp/fmulp/fdivp) share the same load/store structure,
/// differing only in the single arithmetic instruction.
macro_rules! x87_asm_binop {
    ($a:expr, $b:expr, $result:expr, $insn:literal) => {
        // SAFETY: Loads two 80-bit x87 values via `fld tbyte ptr`, performs one
        // arithmetic op, stores result via `fstp tbyte ptr`. Stack is balanced
        // (2 loads, 1 fXXXp pop, 1 fstp). No memory aliasing. The `nostack` option
        // is correct since x87 FPU operations don't touch the CPU stack.
        // For sub/div: load a first → ST(0), then b → ST(0) pushes a to ST(1).
        // fsubp: ST(1) - ST(0) = a - b. fdivp: ST(1) / ST(0) = a / b.
        unsafe {
            std::arch::asm!(
                "fld tbyte ptr [{a}]",
                "fld tbyte ptr [{b}]",
                $insn,
                "fstp tbyte ptr [{res}]",
                a = in(reg) $a.as_ptr(),
                b = in(reg) $b.as_ptr(),
                res = in(reg) $result.as_mut_ptr(),
                options(nostack),
            );
        }
    };
}

/// Perform an x87 binary operation on two 80-bit extended precision values.
/// `op` selects the operation: 0=add, 1=sub, 2=mul, 3=div.
#[cfg(target_arch = "x86_64")]
fn x87_binop(a: &[u8; 16], b: &[u8; 16], op: u8) -> [u8; 16] {
    debug_assert!(op <= 3, "x87_binop: invalid op code {op}, expected 0..=3");
    debug_assert!(a[10..16] == [0; 6], "x87_binop: padding bytes of `a` non-zero");
    debug_assert!(b[10..16] == [0; 6], "x87_binop: padding bytes of `b` non-zero");

    let mut result = [0u8; 16];
    match op {
        0 => x87_asm_binop!(a, b, result, "faddp"),
        1 => x87_asm_binop!(a, b, result, "fsubp"),
        2 => x87_asm_binop!(a, b, result, "fmulp"),
        3 => x87_asm_binop!(a, b, result, "fdivp"),
        _ => unreachable!("invalid x87 binop code"),
    }
    result
}

/// Fallback for non-x86 hosts: use f64 arithmetic (lossy).
// TODO: Cross-compilation from non-x86 hosts will produce imprecise long double
// constant folding results (53-bit instead of 64-bit mantissa). To fix this,
// implement software 80-bit float arithmetic using the existing BigUint infrastructure.
#[cfg(not(target_arch = "x86_64"))]
fn x87_binop(a: &[u8; 16], b: &[u8; 16], op: u8) -> [u8; 16] {
    let fa = x87_bytes_to_f64(a);
    let fb = x87_bytes_to_f64(b);
    let result = match op {
        0 => fa + fb,
        1 => fa - fb,
        2 => fa * fb,
        3 => fa / fb,
        _ => unreachable!("invalid x87 binop code"),
    };
    f64_to_x87_bytes_simple(result)
}

/// Add two x87 80-bit extended precision values with full precision.
pub fn x87_add(a: &[u8; 16], b: &[u8; 16]) -> [u8; 16] {
    x87_binop(a, b, 0)
}

/// Subtract two x87 80-bit extended precision values with full precision.
pub fn x87_sub(a: &[u8; 16], b: &[u8; 16]) -> [u8; 16] {
    x87_binop(a, b, 1)
}

/// Multiply two x87 80-bit extended precision values with full precision.
pub fn x87_mul(a: &[u8; 16], b: &[u8; 16]) -> [u8; 16] {
    x87_binop(a, b, 2)
}

/// Divide two x87 80-bit extended precision values with full precision.
pub fn x87_div(a: &[u8; 16], b: &[u8; 16]) -> [u8; 16] {
    x87_binop(a, b, 3)
}

/// Negate an x87 80-bit extended precision value by flipping the sign bit.
/// This preserves full precision since it only changes one bit.
pub fn x87_neg(a: &[u8; 16]) -> [u8; 16] {
    let mut result = *a;
    result[9] ^= 0x80; // flip the sign bit (bit 15 of exponent+sign word)
    result
}

/// Compute the remainder of two x87 80-bit extended precision values.
#[cfg(target_arch = "x86_64")]
pub fn x87_rem(a: &[u8; 16], b: &[u8; 16]) -> [u8; 16] {
    debug_assert!(a[10..16] == [0; 6], "x87_rem: padding bytes of `a` non-zero");
    debug_assert!(b[10..16] == [0; 6], "x87_rem: padding bytes of `b` non-zero");
    let mut result = [0u8; 16];
    // SAFETY: Same safety rationale as x87_binop. Additionally, fprem may require
    // multiple iterations (checked via C2 status bit), but the loop always terminates
    // because the quotient has finite magnitude. The extra `fstp st(0)` pops the
    // divisor left on the stack, maintaining x87 stack balance.
    unsafe {
        // x87 fprem: ST(0) = ST(0) mod ST(1)
        // We need to loop since fprem may produce partial results for large quotients.
        std::arch::asm!(
            "fld tbyte ptr [{b}]",
            "fld tbyte ptr [{a}]",
            "2:",
            "fprem",
            "fnstsw ax",
            "test ax, 0x400",
            "jnz 2b",
            "fstp tbyte ptr [{res}]",
            "fstp st(0)",
            a = in(reg) a.as_ptr(),
            b = in(reg) b.as_ptr(),
            res = in(reg) result.as_mut_ptr(),
            out("ax") _,
            options(nostack),
        );
    }
    result
}

// TODO: Lossy on non-x86 hosts (see x87_binop fallback comment above).
#[cfg(not(target_arch = "x86_64"))]
pub fn x87_rem(a: &[u8; 16], b: &[u8; 16]) -> [u8; 16] {
    let fa = x87_bytes_to_f64(a);
    let fb = x87_bytes_to_f64(b);
    f64_to_x87_bytes_simple(fa % fb)
}

/// Compare two x87 80-bit extended precision values.
/// Returns: -1 if a < b, 0 if a == b, 1 if a > b, i32::MIN if unordered (NaN).
#[cfg(target_arch = "x86_64")]
pub fn x87_cmp(a: &[u8; 16], b: &[u8; 16]) -> i32 {
    debug_assert!(a[10..16] == [0; 6], "x87_cmp: padding bytes of `a` non-zero");
    debug_assert!(b[10..16] == [0; 6], "x87_cmp: padding bytes of `b` non-zero");
    let status: u16;
    // SAFETY: Loads two 80-bit values, fucompp compares and pops both (stack balanced).
    // fnstsw ax stores the FPU status word into the ax register for condition code inspection.
    // No memory writes except to the `status` output variable.
    unsafe {
        // fucompp compares ST(0) and ST(1) and pops both
        std::arch::asm!(
            "fld tbyte ptr [{b}]",
            "fld tbyte ptr [{a}]",
            "fucompp",
            "fnstsw ax",
            a = in(reg) a.as_ptr(),
            b = in(reg) b.as_ptr(),
            out("ax") status,
            options(nostack),
        );
    }
    // x87 status word bits: C3=ZF(bit 14), C2=PF(bit 10), C0=CF(bit 8)
    let c0 = (status >> 8) & 1;
    let c2 = (status >> 10) & 1;
    let c3 = (status >> 14) & 1;

    if c2 == 1 {
        // Unordered (NaN)
        i32::MIN
    } else if c3 == 1 && c0 == 0 {
        // Equal: C3=1, C0=0
        0
    } else if c0 == 1 {
        // a < b: C0=1, C3=0
        -1
    } else {
        // a > b: C0=0, C3=0
        1
    }
}

// TODO: Lossy on non-x86 hosts (see x87_binop fallback comment above).
#[cfg(not(target_arch = "x86_64"))]
pub fn x87_cmp(a: &[u8; 16], b: &[u8; 16]) -> i32 {
    let fa = x87_bytes_to_f64(a);
    let fb = x87_bytes_to_f64(b);
    if fa.is_nan() || fb.is_nan() {
        i32::MIN
    } else if fa < fb {
        -1
    } else if fa > fb {
        1
    } else {
        0
    }
}

/// Get the f64 approximation from x87 bytes, for use when we still need f64.
/// This is a convenience alias for x87_bytes_to_f64.
pub fn x87_to_f64(bytes: &[u8; 16]) -> f64 {
    x87_bytes_to_f64(bytes)
}

// =============================================================================
// IEEE 754 binary128 (f128) software arithmetic
// =============================================================================
//
// These functions perform arithmetic directly on [u8; 16] f128 byte arrays,
// giving full 112-bit mantissa precision. This avoids the precision loss that
// occurs when converting f128 → x87 (64-bit mantissa) → f128 for constant
// folding on ARM/RISC-V targets where long double is f128.

/// Helper: decompose f128 bytes into (sign, biased_exponent, mantissa_with_implicit_bit).
/// For normal numbers, mantissa has bit 112 set (the implicit leading 1).
/// For subnormals, mantissa does NOT have bit 112 set.
/// Returns (sign: bool, biased_exp: i32, mantissa: u128)
fn f128_decompose(bytes: &[u8; 16]) -> (bool, i32, u128) {
    let val = u128::from_le_bytes(*bytes);
    let sign = (val >> 127) != 0;
    let biased_exp = ((val >> 112) & 0x7FFF) as i32;
    let mantissa = val & ((1u128 << 112) - 1);

    if biased_exp == 0 {
        // Zero or subnormal (no implicit bit)
        (sign, 0, mantissa)
    } else if biased_exp == 0x7FFF {
        // Inf or NaN: keep exponent, mantissa distinguishes them
        (sign, 0x7FFF, mantissa)
    } else {
        // Normal: add implicit leading 1 at bit 112
        (sign, biased_exp, mantissa | (1u128 << 112))
    }
}

/// Helper: check if f128 is zero
fn f128_is_zero(bytes: &[u8; 16]) -> bool {
    let val = u128::from_le_bytes(*bytes);
    (val & !(1u128 << 127)) == 0
}

/// Helper: check if f128 is infinity
fn f128_is_inf(bytes: &[u8; 16]) -> bool {
    let val = u128::from_le_bytes(*bytes);
    let exp = (val >> 112) & 0x7FFF;
    let mantissa = val & ((1u128 << 112) - 1);
    exp == 0x7FFF && mantissa == 0
}

/// Helper: check if f128 is NaN
fn f128_is_nan(bytes: &[u8; 16]) -> bool {
    let val = u128::from_le_bytes(*bytes);
    let exp = (val >> 112) & 0x7FFF;
    let mantissa = val & ((1u128 << 112) - 1);
    exp == 0x7FFF && mantissa != 0
}

/// Helper: round a 128-bit mantissa with guard/round/sticky bits and normalize to f128.
/// `mantissa` is the 113+ bit result, `binary_exp` is the unbiased exponent of bit 112.
/// `guard`, `round`, `sticky` are the IEEE rounding bits for round-to-nearest-even.
fn f128_round_and_encode(sign: bool, binary_exp: i32, mantissa: u128, guard: bool, round: bool, sticky: bool) -> [u8; 16] {
    let mut m = mantissa;
    let mut exp = binary_exp;

    // Round to nearest, ties to even
    let lsb = (m & 1) != 0;
    let round_up = guard && (round || sticky || lsb);

    if round_up {
        m = m.wrapping_add(1);
        // Check if rounding caused carry past bit 113 (overflow of 113-bit mantissa)
        if m & (1u128 << 113) != 0 {
            m >>= 1;
            exp += 1;
        }
    }

    encode_f128(sign, exp, m)
}

/// Add two IEEE 754 binary128 values with full 112-bit mantissa precision.
pub fn f128_add(a: &[u8; 16], b: &[u8; 16]) -> [u8; 16] {
    f128_add_sub(a, b, false)
}

/// Subtract two IEEE 754 binary128 values with full 112-bit mantissa precision.
pub fn f128_sub(a: &[u8; 16], b: &[u8; 16]) -> [u8; 16] {
    f128_add_sub(a, b, true)
}

/// Internal add/sub implementation.
fn f128_add_sub(a: &[u8; 16], b: &[u8; 16], subtract: bool) -> [u8; 16] {
    // Handle NaN
    if f128_is_nan(a) { return *a; }
    if f128_is_nan(b) { return *b; }

    let (a_sign, a_exp, a_mant) = f128_decompose(a);
    let (b_sign_orig, b_exp, b_mant) = f128_decompose(b);
    let b_sign = b_sign_orig ^ subtract; // flip sign of b for subtraction

    // Handle infinities
    if a_exp == 0x7FFF {
        if b_exp == 0x7FFF {
            if a_sign == b_sign { return *a; } // inf + inf = inf
            return make_f128_nan(false); // inf - inf = NaN
        }
        return *a;
    }
    if b_exp == 0x7FFF {
        if subtract {
            // Return infinity with flipped sign
            return make_f128_infinity(!b_sign_orig);
        }
        return *b;
    }

    // Handle zeros
    if f128_is_zero(a) && f128_is_zero(b) {
        // -0 + -0 = -0, otherwise +0
        return make_f128_zero(a_sign && b_sign);
    }
    if f128_is_zero(a) {
        if subtract {
            // 0 - b = -b
            let val = u128::from_le_bytes(*b);
            return (val ^ (1u128 << 127)).to_le_bytes();
        }
        return *b;
    }
    if f128_is_zero(b) { return *a; }

    // Get unbiased exponents. For subnormals, effective exponent is 1 (biased) - 16383 = -16382.
    let a_ue = if a_exp == 0 { -16382 } else { a_exp - 16383 };
    let b_ue = if b_exp == 0 { -16382 } else { b_exp - 16383 };

    // Align mantissas. We work with 3 extra bits (guard, round, sticky) for rounding.
    // Mantissas are 113 bits (bit 112 is MSB for normals). Shift left by 3 to make room.
    let mut a_m: u128 = a_mant << 3;
    let mut b_m: u128 = b_mant << 3;
    let mut exp_result = a_ue;
    let mut sticky = false;

    let exp_diff = a_ue - b_ue;
    if exp_diff > 0 {
        // Shift b right
        let shift = exp_diff as u32;
        if shift >= 128 {
            sticky = b_m != 0;
            b_m = 0;
        } else {
            sticky = (b_m & ((1u128 << shift) - 1)) != 0;
            b_m >>= shift;
        }
    } else if exp_diff < 0 {
        // Shift a right
        let shift = (-exp_diff) as u32;
        if shift >= 128 {
            sticky = a_m != 0;
            a_m = 0;
        } else {
            sticky = (a_m & ((1u128 << shift) - 1)) != 0;
            a_m >>= shift;
        }
        exp_result = b_ue;
    }

    // Add or subtract mantissas
    let (result_sign, result_m) = if a_sign == b_sign {
        // Same sign: add magnitudes
        let sum = a_m + b_m;
        (a_sign, sum)
    } else {
        // Different signs: subtract magnitudes
        if a_m > b_m {
            (a_sign, a_m - b_m)
        } else if b_m > a_m {
            (b_sign, b_m - a_m)
        } else {
            // Exact cancellation
            return make_f128_zero(false); // +0 for round-to-nearest
        }
    };

    if result_m == 0 {
        return make_f128_zero(false);
    }

    // Normalize: the result mantissa should have its MSB at bit 115 (= 112 + 3 guard bits).
    let mut m = result_m;
    let target_bit = 115; // bit 112 + 3 guard bits
    let msb = 127 - m.leading_zeros() as i32;

    if msb > target_bit {
        // Overflow: shift right, collect sticky bits
        let shift = (msb - target_bit) as u32;
        let lost = m & ((1u128 << shift) - 1);
        sticky = sticky || (lost != 0);
        m >>= shift;
        exp_result += shift as i32;
    } else if msb < target_bit {
        // Underflow: shift left
        let shift = (target_bit - msb) as u32;
        m <<= shift;
        exp_result -= shift as i32;
    }

    // Extract guard, round, sticky bits
    let guard_bit = (m & 4) != 0;
    let round_bit = (m & 2) != 0;
    let sticky_bit = sticky || (m & 1) != 0;
    m >>= 3; // Remove guard bits, now m is 113-bit mantissa

    f128_round_and_encode(result_sign, exp_result, m, guard_bit, round_bit, sticky_bit)
}

/// Multiply two IEEE 754 binary128 values with full 112-bit mantissa precision.
pub fn f128_mul(a: &[u8; 16], b: &[u8; 16]) -> [u8; 16] {
    // Handle NaN
    if f128_is_nan(a) { return *a; }
    if f128_is_nan(b) { return *b; }

    let (a_sign, a_exp, a_mant) = f128_decompose(a);
    let (b_sign, b_exp, b_mant) = f128_decompose(b);
    let result_sign = a_sign ^ b_sign;

    // Handle infinities
    if a_exp == 0x7FFF || b_exp == 0x7FFF {
        if f128_is_zero(a) || f128_is_zero(b) {
            return make_f128_nan(false); // inf * 0 = NaN
        }
        return make_f128_infinity(result_sign);
    }

    // Handle zeros
    if f128_is_zero(a) || f128_is_zero(b) {
        return make_f128_zero(result_sign);
    }

    // Compute unbiased exponents
    let a_ue = if a_exp == 0 { -16382 } else { a_exp - 16383 };
    let b_ue = if b_exp == 0 { -16382 } else { b_exp - 16383 };

    // Multiply mantissas: 113 bits * 113 bits = up to 226 bits.
    // We need to use 256-bit multiplication (two u128 halves).
    let (prod_hi, prod_lo) = mul_u128(a_mant, b_mant);

    // The product has MSB at bit 224 or 225 (of the 226-bit result).
    // Exponent of the product = a_ue + b_ue (for the MSB of the product, adjusted).
    // Product is in the form: mantissa_a (bit 112 MSB) * mantissa_b (bit 112 MSB)
    // So the product MSB is at bit 224 or 225.
    let result_exp_base = a_ue + b_ue;

    // Normalize: we need a 113-bit mantissa (bit 112 is MSB).
    // The product MSB is at position 224 (= 112+112) or 225.
    // We need to shift right by (MSB_pos - 112) bits and collect sticky.
    let prod_bits = if prod_hi == 0 {
        if prod_lo == 0 { return make_f128_zero(result_sign); }
        127 - prod_lo.leading_zeros() as i32
    } else {
        128 + 127 - prod_hi.leading_zeros() as i32
    };

    // Number of bits to shift right to get mantissa MSB at bit 112
    let shift = prod_bits - 112;
    // The product of two mantissas with MSB at bit 112 has expected MSB at bit 224.
    // Only the excess above 224 adjusts the exponent (e.g. if MSB is at 225, add 1).
    let result_exp = result_exp_base + (prod_bits - 224);

    let (mantissa, guard, round, sticky) = if shift <= 0 {
        // Product fits in 113 bits - shift left
        let s = (-shift) as u32;
        let m = if prod_hi == 0 { prod_lo << s } else { (prod_hi << (s + 128)) | (prod_lo << s) };
        (m, false, false, false)
    } else {
        shift_right_256_with_grs(prod_hi, prod_lo, shift as u32)
    };

    f128_round_and_encode(result_sign, result_exp, mantissa, guard, round, sticky)
}

/// Divide two IEEE 754 binary128 values with full 112-bit mantissa precision.
pub fn f128_div(a: &[u8; 16], b: &[u8; 16]) -> [u8; 16] {
    // Handle NaN
    if f128_is_nan(a) { return *a; }
    if f128_is_nan(b) { return *b; }

    let (a_sign, a_exp, a_mant) = f128_decompose(a);
    let (b_sign, b_exp, b_mant) = f128_decompose(b);
    let result_sign = a_sign ^ b_sign;

    // Handle infinities and zeros
    if a_exp == 0x7FFF {
        if b_exp == 0x7FFF { return make_f128_nan(false); } // inf / inf = NaN
        return make_f128_infinity(result_sign);
    }
    if b_exp == 0x7FFF {
        return make_f128_zero(result_sign); // x / inf = 0
    }
    if f128_is_zero(b) {
        if f128_is_zero(a) { return make_f128_nan(false); } // 0 / 0 = NaN
        return make_f128_infinity(result_sign); // x / 0 = inf
    }
    if f128_is_zero(a) {
        return make_f128_zero(result_sign);
    }

    // Unbiased exponents
    let a_ue = if a_exp == 0 { -16382 } else { a_exp - 16383 };
    let b_ue = if b_exp == 0 { -16382 } else { b_exp - 16383 };

    // Division: we need a_mant / b_mant with 113+ bits of precision in the quotient.
    // Both mantissas are 113-bit (bit 112 is MSB for normals).
    //
    // Strategy: compute (a_mant << 115) / b_mant to get a quotient with 115+ bits.
    // Since both mantissas have MSB at bit 112, their ratio is in [0.5, 2.0),
    // so the quotient MSB is at bit 114 or 115. We normalize to bit 115
    // (= 112 mantissa bits + 3 guard/round/sticky bits).
    //
    // We use 256-bit dividend: (a_mant << 115) is at most 228 bits.
    let shift = 115; // 112 (for mantissa) + 3 (for GRS)
    let (dividend_hi, dividend_lo) = shl_u128(a_mant, shift);

    // Divide 256-bit dividend by 128-bit divisor
    let (quot, rem) = div_256_by_128(dividend_hi, dividend_lo, b_mant);

    let result_exp = a_ue - b_ue;

    if quot == 0 {
        return make_f128_zero(result_sign);
    }

    // Normalize quotient: MSB should be at bit 115 (112 + 3 guard bits)
    let msb = 127 - quot.leading_zeros() as i32;
    let target = 115;
    let (m, exp_adjust, extra_sticky) = if msb > target {
        let s = (msb - target) as u32;
        let lost = quot & ((1u128 << s) - 1);
        (quot >> s, msb - target, lost != 0)
    } else if msb < target {
        let s = (target - msb) as u32;
        (quot << s, -(s as i32), false)
    } else {
        (quot, 0, false)
    };

    let guard = (m & 4) != 0;
    let round = (m & 2) != 0;
    let sticky = extra_sticky || (rem != 0) || (m & 1) != 0;
    let mantissa = m >> 3;

    f128_round_and_encode(result_sign, result_exp + exp_adjust, mantissa, guard, round, sticky)
}

/// Compute the remainder of two IEEE 754 binary128 values.
pub fn f128_rem(a: &[u8; 16], b: &[u8; 16]) -> [u8; 16] {
    // rem(a, b) = a - trunc(a/b) * b
    // For simplicity, compute using the identity: rem = a - q * b where q = trunc(a/b)
    // Handle special cases
    if f128_is_nan(a) { return *a; }
    if f128_is_nan(b) { return *b; }
    if f128_is_inf(a) { return make_f128_nan(false); }
    if f128_is_zero(b) { return make_f128_nan(false); }
    if f128_is_zero(a) { return *a; }
    if f128_is_inf(b) { return *a; }

    // For now, fall back to f64 approximation for remainder
    // (remainder is less commonly used for long double constant folding)
    let fa = f128_bytes_to_f64(a);
    let fb = f128_bytes_to_f64(b);
    let result = fa % fb;
    f64_to_f128_bytes_lossless(result)
}

/// Compare two f128 values. Returns -1, 0, 1, or i32::MIN for unordered (NaN).
pub fn f128_cmp(a: &[u8; 16], b: &[u8; 16]) -> i32 {
    if f128_is_nan(a) || f128_is_nan(b) {
        return i32::MIN;
    }
    let a_val = u128::from_le_bytes(*a);
    let b_val = u128::from_le_bytes(*b);
    let a_sign = (a_val >> 127) != 0;
    let b_sign = (b_val >> 127) != 0;
    let a_mag = a_val & !(1u128 << 127);
    let b_mag = b_val & !(1u128 << 127);

    // Both zero (may differ in sign)
    if a_mag == 0 && b_mag == 0 {
        return 0;
    }

    // Different signs: negative < positive
    if a_sign != b_sign {
        return if a_sign { -1 } else { 1 };
    }

    // Same sign: compare magnitudes
    let cmp = if a_mag < b_mag { -1 } else if a_mag > b_mag { 1 } else { 0 };
    // If negative, reverse the comparison
    if a_sign { -cmp } else { cmp }
}

// --- Helper functions for wide arithmetic ---

/// Multiply two u128 values, returning (hi, lo) of the 256-bit result.
fn mul_u128(a: u128, b: u128) -> (u128, u128) {
    let a_lo = a as u64 as u128;
    let a_hi = (a >> 64) as u64 as u128;
    let b_lo = b as u64 as u128;
    let b_hi = (b >> 64) as u64 as u128;

    let ll = a_lo * b_lo;
    let lh = a_lo * b_hi;
    let hl = a_hi * b_lo;
    let hh = a_hi * b_hi;

    let mid = lh + hl;
    let carry1 = if mid < lh { 1u128 } else { 0 };

    let lo = ll.wrapping_add(mid << 64);
    let carry2 = if lo < ll { 1u128 } else { 0 };
    let hi = hh + (mid >> 64) + (carry1 << 64) + carry2;

    (hi, lo)
}

/// Shift a u128 value left, returning (hi, lo) of the 256-bit result.
fn shl_u128(val: u128, shift: u32) -> (u128, u128) {
    if shift == 0 {
        (0, val)
    } else if shift < 128 {
        let hi = val >> (128 - shift);
        let lo = val << shift;
        (hi, lo)
    } else if shift < 256 {
        let hi = val << (shift - 128);
        (hi, 0)
    } else {
        (0, 0)
    }
}

/// Shift right a 256-bit value (hi:lo) by `shift` bits, extracting guard/round/sticky.
/// Returns (shifted_lo_128, guard, round, sticky).
fn shift_right_256_with_grs(hi: u128, lo: u128, shift: u32) -> (u128, bool, bool, bool) {
    if shift == 0 {
        return (lo, false, false, false);
    }

    // Reconstruct the bits we'll lose for GRS
    // We need to shift right by `shift` bits and extract the top bit lost (guard),
    // next bit (round), and OR of all remaining lost bits (sticky).

    // For very large shifts, everything becomes sticky
    if shift >= 256 {
        let sticky = hi != 0 || lo != 0;
        return (0, false, false, sticky);
    }

    // Compute the shifted value and the lost bits
    let result;
    let guard_bit;
    let round_bit;
    let sticky_bits;

    if shift < 128 {
        // Result comes from lo (shifted right) with bits from hi
        result = (lo >> shift) | (hi << (128 - shift));
        // Guard bit is bit (shift-1) of the original (hi:lo)
        guard_bit = if shift >= 1 { (lo >> (shift - 1)) & 1 != 0 } else { false };
        // Round bit is bit (shift-2)
        round_bit = if shift >= 2 { (lo >> (shift - 2)) & 1 != 0 } else { false };
        // Sticky: OR of all bits below round
        sticky_bits = if shift >= 3 { lo & ((1u128 << (shift - 2)) - 1) != 0 } else { false };
    } else if shift == 128 {
        result = hi;
        guard_bit = (lo >> 127) & 1 != 0;
        round_bit = (lo >> 126) & 1 != 0;
        sticky_bits = lo & ((1u128 << 126) - 1) != 0;
    } else {
        // shift > 128
        let s = shift - 128;
        result = hi >> s;
        if s == 0 {
            guard_bit = (lo >> 127) & 1 != 0;
            round_bit = (lo >> 126) & 1 != 0;
            sticky_bits = lo & ((1u128 << 126) - 1) != 0;
        } else {
            guard_bit = if s >= 1 { (hi >> (s - 1)) & 1 != 0 } else { false };
            round_bit = if s >= 2 { (hi >> (s - 2)) & 1 != 0 } else { lo >> 127 != 0 };
            let hi_sticky = if s >= 3 { hi & ((1u128 << (s - 2)) - 1) != 0 } else { false };
            sticky_bits = hi_sticky || lo != 0;
        }
    }

    (result, guard_bit, round_bit, sticky_bits)
}

/// Divide a 256-bit number (hi:lo) by a 128-bit divisor.
/// Returns (quotient, remainder) both as u128.
/// The quotient must fit in u128 (caller ensures this by appropriate shifting).
fn div_256_by_128(hi: u128, lo: u128, divisor: u128) -> (u128, u128) {
    if divisor == 0 {
        return (u128::MAX, 0); // Division by zero
    }
    if hi == 0 {
        return (lo / divisor, lo % divisor);
    }

    // Long division: divide (hi:lo) by divisor.
    // Process one bit at a time from the most significant bit.
    let mut remainder: u128 = 0;
    let mut quotient: u128 = 0;

    // Process high 128 bits
    for i in (0..128).rev() {
        remainder <<= 1;
        remainder |= (hi >> i) & 1;
        if remainder >= divisor {
            remainder -= divisor;
            // This contributes to upper bits of quotient (beyond 128), but we know
            // the quotient fits in 128 bits, so we just track the remainder.
        }
    }

    // Now process low 128 bits, building the actual quotient
    for i in (0..128).rev() {
        remainder <<= 1;
        remainder |= (lo >> i) & 1;
        if remainder >= divisor {
            remainder -= divisor;
            quotient |= 1u128 << i;
        }
    }

    (quotient, remainder)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_x87_to_f128() {
        // Test zero
        let x87 = [0u8; 16];
        let f128 = x87_bytes_to_f128_bytes(&x87);
        assert!(f128.iter().all(|&b| b == 0));

        // Test 1.0: x87 bytes should be exp=16383=0x3FFF, mantissa=1<<63
        let bytes_1 = f64_to_x87_bytes_simple(1.0);
        let f128_1 = x87_bytes_to_f128_bytes(&bytes_1);
        // f128 for 1.0: sign=0, exp=16383=0x3FFF, mantissa=0
        // Bytes (LE): 00..00 FF 3F
        assert_eq!(f128_1[15], 0x3F);
        assert_eq!(f128_1[14], 0xFF);
    }

    #[test]
    fn test_f64_to_x87_simple() {
        let bytes = f64_to_x87_bytes_simple(1.0);
        let f = x87_bytes_to_f64(&bytes);
        assert!((f - 1.0).abs() < 1e-15);

        let bytes = f64_to_x87_bytes_simple(-0.0);
        assert_eq!(bytes[9] & 0x80, 0x80);
    }
}

#[cfg(test)]
mod f128_tests {
    use super::*;

    #[test]
    fn test_f128_parse_integer() {
        // 9223372036854775807 = 2^63 - 1
        // In f128: exp=62+16383=16445=0x403D, mantissa has all lower bits set
        let bytes = parse_long_double_to_f128_bytes("9223372036854775807.0L");
        let val = u128::from_le_bytes(bytes);
        let exp = (val >> 112) & 0x7FFF;
        let mantissa = val & ((1u128 << 112) - 1);

        // Expected: exp = 0x403D (biased 62)
        assert_eq!(exp, 0x403D, "exponent for 2^63-1 should be 0x403D");
        // Mantissa should have high bits set (not all zeros)
        assert_ne!(mantissa, 0, "mantissa for 2^63-1 should not be zero");
    }

    #[test]
    fn test_f128_parse_pi() {
        let bytes = parse_long_double_to_f128_bytes("3.14159265358979323846264338327950288L");
        let val = u128::from_le_bytes(bytes);
        let exp = (val >> 112) & 0x7FFF;

        // pi: exp = 1 + 16383 = 16384 = 0x4000
        assert_eq!(exp, 0x4000, "exponent for pi should be 0x4000");
    }

    #[test]
    fn test_f128_parse_one() {
        let bytes = parse_long_double_to_f128_bytes("1.0L");
        let val = u128::from_le_bytes(bytes);
        let exp = (val >> 112) & 0x7FFF;
        let mantissa = val & ((1u128 << 112) - 1);

        assert_eq!(exp, 0x3FFF, "exponent for 1.0 should be 0x3FFF");
        assert_eq!(mantissa, 0, "mantissa for 1.0 should be zero");
    }

    #[test]
    fn test_f128_roundtrip_x87() {
        // Parse to f128, convert to x87, and back
        let f128 = parse_long_double_to_f128_bytes("1.0L");
        let x87 = f128_bytes_to_x87_bytes(&f128);
        let f128_back = x87_bytes_to_f128_bytes(&x87);
        assert_eq!(f128, f128_back, "roundtrip should preserve value");
    }
}
