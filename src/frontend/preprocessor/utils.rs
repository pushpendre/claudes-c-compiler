/// Shared utility functions for the preprocessor module.
///
/// Provides both char-oriented and byte-oriented helpers for scanning C source text.
/// The byte-oriented variants (`*_byte`, `*_bytes`) are preferred in hot paths since
/// they avoid the overhead of `Vec<char>` allocation. The char-oriented versions are
/// retained for code that still operates on `&[char]`.

/// Check if a character can start a C identifier.
pub fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

/// Check if a character can continue a C identifier.
pub fn is_ident_cont(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

/// Check if a byte can start a C identifier (ASCII letter or underscore).
#[inline(always)]
pub fn is_ident_start_byte(b: u8) -> bool {
    b.is_ascii_alphabetic() || b == b'_'
}

/// Check if a byte can continue a C identifier (ASCII alphanumeric or underscore).
#[inline(always)]
pub fn is_ident_cont_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

/// Extract a `&str` slice from `bytes[start..end]`.
///
/// # Safety
/// Caller must ensure `bytes[start..end]` contains valid UTF-8. This is guaranteed
/// for C identifiers, which consist only of ASCII letters, digits, and underscores.
#[inline(always)]
pub fn bytes_to_str(bytes: &[u8], start: usize, end: usize) -> &str {
    // SAFETY: C identifiers consist only of ASCII letters, digits, and underscores,
    // which are all valid single-byte UTF-8 characters.
    unsafe { std::str::from_utf8_unchecked(&bytes[start..end]) }
}

/// Skip past a string or character literal in a byte slice, starting at position `i`.
/// Returns the position after the closing quote. Handles backslash escapes.
/// Byte-oriented version for code that processes `&[u8]` directly.
pub fn skip_literal_bytes(bytes: &[u8], start: usize, quote: u8) -> usize {
    let len = bytes.len();
    let mut i = start + 1; // skip opening quote
    while i < len {
        if bytes[i] == b'\\' && i + 1 < len {
            i += 2;
        } else if bytes[i] == quote {
            return i + 1;
        } else {
            i += 1;
        }
    }
    i
}

/// Copy a string or character literal from a byte slice into a byte buffer result.
/// Returns the position after the closing quote. Handles backslash escapes.
/// Copies raw bytes without any char conversion to preserve UTF-8 sequences.
pub fn copy_literal_bytes_raw(bytes: &[u8], start: usize, quote: u8, result: &mut Vec<u8>) -> usize {
    let len = bytes.len();
    result.push(bytes[start]); // opening quote
    let mut i = start + 1;
    while i < len {
        if bytes[i] == b'\\' && i + 1 < len {
            result.push(bytes[i]);
            result.push(bytes[i + 1]);
            i += 2;
        } else if bytes[i] == quote {
            result.push(bytes[i]);
            return i + 1;
        } else {
            result.push(bytes[i]);
            i += 1;
        }
    }
    i
}

/// Copy a string or character literal from a byte slice into a String result.
/// Returns the position after the closing quote. Handles backslash escapes.
/// Copies the entire literal as a single `&str` slice for efficiency, rather than
/// pushing individual bytes.
pub fn copy_literal_bytes_to_string(bytes: &[u8], start: usize, quote: u8, result: &mut String) -> usize {
    let len = bytes.len();
    // Find the end of the literal first, then copy as a single &str slice
    // (the common case), avoiding per-byte push.
    let mut i = start + 1; // skip opening quote
    while i < len {
        if bytes[i] == b'\\' && i + 1 < len {
            i += 2; // skip escape sequence
        } else if bytes[i] == quote {
            i += 1; // include closing quote
            // SAFETY: the source text is valid UTF-8, and we are copying a contiguous
            // substring that includes only the literal and its delimiters.
            let slice = unsafe { std::str::from_utf8_unchecked(&bytes[start..i]) };
            result.push_str(slice);
            return i;
        } else {
            i += 1;
        }
    }
    // Unterminated literal - copy what we have
    // SAFETY: same as above - contiguous substring of valid UTF-8 source.
    let slice = unsafe { std::str::from_utf8_unchecked(&bytes[start..i]) };
    result.push_str(slice);
    i
}
