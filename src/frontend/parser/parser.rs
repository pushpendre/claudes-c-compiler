// Core Parser struct and basic helpers.
//
// The parser is split into focused modules:
//   - expressions.rs: operator precedence climbing (comma through primary)
//   - types.rs: type specifier collection and resolution
//   - statements.rs: all statement types + inline assembly
//   - declarations.rs: external and local declarations, initializers
//   - declarators.rs: C declarator syntax (pointers, arrays, function pointers)
//
// Each module adds methods to the Parser struct via `impl Parser` blocks.
// Methods are pub(super) so they can be called across modules within the parser.

use crate::common::source::Span;
use crate::frontend::lexer::token::{Token, TokenKind};
use super::ast::*;

/// Recursive descent parser for C.
pub struct Parser {
    pub(super) tokens: Vec<Token>,
    pub(super) pos: usize,
    pub(super) typedefs: Vec<String>,
    /// Typedef names shadowed by local variable declarations in the current scope.
    pub(super) shadowed_typedefs: Vec<String>,
    /// Set to true when parse_type_specifier encounters a `typedef` keyword.
    pub(super) parsing_typedef: bool,
    /// Set to true when parse_type_specifier encounters a `static` keyword.
    pub(super) parsing_static: bool,
    /// Set to true when parse_type_specifier encounters an `extern` keyword.
    pub(super) parsing_extern: bool,
    /// Set to true when parse_type_specifier encounters an `inline` keyword.
    pub(super) parsing_inline: bool,
    /// Set to true when parse_type_specifier encounters a `const` qualifier.
    pub(super) parsing_const: bool,
    /// Set to true when parse_type_specifier encounters __attribute__((constructor)).
    pub(super) parsing_constructor: bool,
    /// Set to true when parse_type_specifier encounters __attribute__((destructor)).
    pub(super) parsing_destructor: bool,
    /// Set when parse_type_specifier or consume_post_type_qualifiers encounters _Alignas(N).
    /// Consumed and reset by callers that need it (e.g., struct field parsing).
    pub(super) parsed_alignas: Option<usize>,
    /// Stack for #pragma pack alignment values.
    /// Current effective alignment is the last element (or None for default).
    pub(super) pragma_pack_stack: Vec<Option<usize>>,
    /// Current #pragma pack alignment. None means default (natural) alignment.
    pub(super) pragma_pack_align: Option<usize>,
    /// Count of parse errors encountered (invalid tokens at top level, etc.)
    pub error_count: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            typedefs: Self::builtin_typedefs(),
            shadowed_typedefs: Vec::new(),
            parsing_typedef: false,
            parsing_static: false,
            parsing_extern: false,
            parsing_inline: false,
            parsing_const: false,
            parsing_constructor: false,
            parsing_destructor: false,
            parsed_alignas: None,
            pragma_pack_stack: Vec::new(),
            pragma_pack_align: None,
            error_count: 0,
        }
    }

    /// Standard C typedef names commonly provided by system headers.
    /// Since we don't actually include system headers, we pre-seed these.
    fn builtin_typedefs() -> Vec<String> {
        [
            // <stddef.h>
            "size_t", "ssize_t", "ptrdiff_t", "wchar_t", "wint_t",
            // <stdint.h>
            "int8_t", "int16_t", "int32_t", "int64_t",
            "uint8_t", "uint16_t", "uint32_t", "uint64_t",
            "intptr_t", "uintptr_t",
            "intmax_t", "uintmax_t",
            "int_least8_t", "int_least16_t", "int_least32_t", "int_least64_t",
            "uint_least8_t", "uint_least16_t", "uint_least32_t", "uint_least64_t",
            "int_fast8_t", "int_fast16_t", "int_fast32_t", "int_fast64_t",
            "uint_fast8_t", "uint_fast16_t", "uint_fast32_t", "uint_fast64_t",
            // <stdio.h>
            "FILE", "fpos_t",
            // <signal.h>
            "sig_atomic_t",
            // <time.h>
            "time_t", "clock_t", "timer_t", "clockid_t",
            // <sys/types.h>
            "off_t", "pid_t", "uid_t", "gid_t", "mode_t", "dev_t", "ino_t",
            "nlink_t", "blksize_t", "blkcnt_t",
            // GNU/glibc common types
            "ulong", "ushort", "uint",
            "__u8", "__u16", "__u32", "__u64",
            "__s8", "__s16", "__s32", "__s64",
            // <stdarg.h>
            "va_list", "__builtin_va_list", "__gnuc_va_list",
            // <locale.h>
            "locale_t",
            // <pthread.h>
            "pthread_t", "pthread_mutex_t", "pthread_cond_t",
            "pthread_key_t", "pthread_attr_t", "pthread_once_t",
            "pthread_mutexattr_t", "pthread_condattr_t",
            // <setjmp.h>
            "jmp_buf", "sigjmp_buf",
            // <dirent.h>
            "DIR",
        ].iter().map(|s| s.to_string()).collect()
    }

    pub fn parse(&mut self) -> TranslationUnit {
        let mut decls = Vec::new();
        while !self.at_eof() {
            if let Some(decl) = self.parse_external_decl() {
                decls.push(decl);
            } else {
                // Report error for unrecognized token at top level
                let tok = self.peek().clone();
                if !matches!(tok, TokenKind::Semicolon | TokenKind::Eof) {
                    self.error_count += 1;
                    eprintln!("error: expected declaration, got {:?}", tok);
                }
                self.advance();
            }
        }
        TranslationUnit { decls }
    }

    // === Token access helpers ===

    pub(super) fn at_eof(&self) -> bool {
        self.pos >= self.tokens.len() || matches!(self.tokens[self.pos].kind, TokenKind::Eof)
    }

    pub(super) fn peek(&self) -> &TokenKind {
        if self.pos < self.tokens.len() {
            &self.tokens[self.pos].kind
        } else {
            &TokenKind::Eof
        }
    }

    pub(super) fn peek_span(&self) -> Span {
        if self.pos < self.tokens.len() {
            self.tokens[self.pos].span
        } else {
            Span::dummy()
        }
    }

    pub(super) fn advance(&mut self) -> &Token {
        if self.pos < self.tokens.len() {
            let tok = &self.tokens[self.pos];
            self.pos += 1;
            tok
        } else {
            &self.tokens[self.tokens.len() - 1]
        }
    }

    pub(super) fn expect(&mut self, expected: &TokenKind) -> Span {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(expected) {
            let span = self.peek_span();
            self.advance();
            span
        } else {
            let span = self.peek_span();
            // Show context around error
            let start = if self.pos > 20 { self.pos - 20 } else { 0 };
            let end = std::cmp::min(self.pos + 5, self.tokens.len());
            let context: Vec<_> = self.tokens[start..end].iter().map(|t| {
                match &t.kind {
                    TokenKind::Identifier(name) => format!("Id({})", name),
                    TokenKind::IntLiteral(v) => format!("Int({})", v),
                    TokenKind::StringLiteral(s) => format!("Str({})", s),
                    other => format!("{:?}", other),
                }
            }).collect();
            eprintln!("error: expected {:?}, got {:?}", expected, self.peek());
            self.error_count += 1;
            span
        }
    }

    pub(super) fn consume_if(&mut self, kind: &TokenKind) -> bool {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    // === Type and qualifier helpers ===

    pub(super) fn is_type_specifier(&self) -> bool {
        match self.peek() {
            TokenKind::Void | TokenKind::Char | TokenKind::Short | TokenKind::Int |
            TokenKind::Long | TokenKind::Float | TokenKind::Double | TokenKind::Signed |
            TokenKind::Unsigned | TokenKind::Struct | TokenKind::Union | TokenKind::Enum |
            TokenKind::Const | TokenKind::Volatile | TokenKind::Static | TokenKind::Extern |
            TokenKind::Register | TokenKind::Typedef | TokenKind::Inline | TokenKind::Bool |
            TokenKind::Typeof | TokenKind::Attribute | TokenKind::Extension |
            TokenKind::Noreturn | TokenKind::Restrict | TokenKind::Complex |
            TokenKind::Atomic | TokenKind::Auto | TokenKind::Alignas |
            TokenKind::Builtin | TokenKind::Int128 | TokenKind::UInt128 => true,
            TokenKind::Identifier(name) => self.typedefs.contains(name) && !self.shadowed_typedefs.contains(name),
            _ => false,
        }
    }

    pub(super) fn skip_cv_qualifiers(&mut self) {
        loop {
            match self.peek() {
                TokenKind::Const | TokenKind::Volatile | TokenKind::Restrict => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    /// Skip C99 type qualifiers and 'static' inside array brackets.
    /// In C99 function parameter declarations, array dimensions can include:
    ///   [static restrict const 10], [restrict n], [const], [static 10], etc.
    /// We skip these qualifiers since they only affect optimization hints and
    /// don't change the type semantics (array params decay to pointers).
    pub(super) fn skip_array_qualifiers(&mut self) {
        loop {
            match self.peek() {
                TokenKind::Static | TokenKind::Const | TokenKind::Volatile
                | TokenKind::Restrict | TokenKind::Atomic => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    pub(super) fn skip_array_dimensions(&mut self) {
        while matches!(self.peek(), TokenKind::LBracket) {
            self.advance();
            while !matches!(self.peek(), TokenKind::RBracket | TokenKind::Eof) {
                self.advance();
            }
            self.consume_if(&TokenKind::RBracket);
        }
    }

    pub(super) fn compound_assign_op(&self) -> Option<BinOp> {
        match self.peek() {
            TokenKind::PlusAssign => Some(BinOp::Add),
            TokenKind::MinusAssign => Some(BinOp::Sub),
            TokenKind::StarAssign => Some(BinOp::Mul),
            TokenKind::SlashAssign => Some(BinOp::Div),
            TokenKind::PercentAssign => Some(BinOp::Mod),
            TokenKind::AmpAssign => Some(BinOp::BitAnd),
            TokenKind::PipeAssign => Some(BinOp::BitOr),
            TokenKind::CaretAssign => Some(BinOp::BitXor),
            TokenKind::LessLessAssign => Some(BinOp::Shl),
            TokenKind::GreaterGreaterAssign => Some(BinOp::Shr),
            _ => None,
        }
    }

    // === GCC extension helpers ===

    pub(super) fn skip_gcc_extensions(&mut self) {
        self.parse_gcc_attributes();
    }

    /// Parse __attribute__((...)) and __extension__, returning struct attribute flags.
    /// Returns (is_packed, aligned_value).
    pub(super) fn parse_gcc_attributes(&mut self) -> (bool, Option<usize>, bool, bool) {
        let mut is_packed = false;
        let mut _aligned = None;
        let mut has_mode_ti = false;
        let mut is_common = false;
        loop {
            match self.peek() {
                TokenKind::Extension => { self.advance(); }
                TokenKind::Attribute => {
                    self.advance();
                    if matches!(self.peek(), TokenKind::LParen) {
                        self.advance(); // outer (
                        if matches!(self.peek(), TokenKind::LParen) {
                            self.advance(); // inner (
                            // Parse attribute list
                            loop {
                                match self.peek() {
                                    TokenKind::Identifier(name) if name == "constructor" || name == "__constructor__" => {
                                        self.parsing_constructor = true;
                                        self.advance();
                                        if matches!(self.peek(), TokenKind::LParen) {
                                            self.skip_balanced_parens();
                                        }
                                    }
                                    TokenKind::Identifier(name) if name == "destructor" || name == "__destructor__" => {
                                        self.parsing_destructor = true;
                                        self.advance();
                                        if matches!(self.peek(), TokenKind::LParen) {
                                            self.skip_balanced_parens();
                                        }
                                    }
                                    TokenKind::Identifier(name) if name == "packed" || name == "__packed__" => {
                                        is_packed = true;
                                        self.advance();
                                    }
                                    TokenKind::Identifier(name) if name == "aligned" || name == "__aligned__" => {
                                        self.advance();
                                        if matches!(self.peek(), TokenKind::LParen) {
                                            if let Some(align) = self.parse_alignment_expr() {
                                                _aligned = Some(align);
                                            }
                                        }
                                    }
                                    TokenKind::Identifier(name) if name == "common" || name == "__common__" => {
                                        is_common = true;
                                        self.advance();
                                    }
                                    TokenKind::Identifier(name) if name == "mode" || name == "__mode__" => {
                                        self.advance();
                                        if matches!(self.peek(), TokenKind::LParen) {
                                            self.advance(); // consume (
                                            // Check for TI mode (128-bit integer)
                                            if let TokenKind::Identifier(mode_name) = self.peek() {
                                                if mode_name == "TI" || mode_name == "__TI__" {
                                                    has_mode_ti = true;
                                                }
                                            }
                                            // Skip to closing paren
                                            while !matches!(self.peek(), TokenKind::RParen | TokenKind::Eof) {
                                                self.advance();
                                            }
                                            if matches!(self.peek(), TokenKind::RParen) {
                                                self.advance();
                                            }
                                        }
                                    }
                                    TokenKind::Identifier(_) => {
                                        self.advance();
                                        if matches!(self.peek(), TokenKind::LParen) {
                                            self.skip_balanced_parens();
                                        }
                                    }
                                    TokenKind::Comma => { self.advance(); }
                                    TokenKind::RParen | TokenKind::Eof => break,
                                    _ => { self.advance(); }
                                }
                            }
                            // Inner )
                            if matches!(self.peek(), TokenKind::RParen) {
                                self.advance();
                            }
                        } else {
                            // Single-paren form
                            while !matches!(self.peek(), TokenKind::RParen | TokenKind::Eof) {
                                if let TokenKind::Identifier(name) = self.peek() {
                                    if name == "packed" || name == "__packed__" {
                                        is_packed = true;
                                    }
                                }
                                self.advance();
                            }
                        }
                        // Outer )
                        if matches!(self.peek(), TokenKind::RParen) {
                            self.advance();
                        }
                    }
                }
                _ => break,
            }
        }
        (is_packed, _aligned, has_mode_ti, is_common)
    }

    /// Skip __asm__("..."), __attribute__(...), and __extension__ after declarators.
    /// Returns true if __attribute__((mode(TI))) was found (128-bit integer mode).
    pub(super) fn skip_asm_and_attributes(&mut self) -> bool {
        let (_, _, mode_ti, _) = self.parse_asm_and_attributes();
        mode_ti
    }

    /// Parse __asm__("..."), __attribute__(...), and __extension__ after declarators.
    /// Returns (is_constructor, is_destructor, has_mode_ti, is_common).
    pub(super) fn parse_asm_and_attributes(&mut self) -> (bool, bool, bool, bool) {
        let mut is_constructor = false;
        let mut is_destructor = false;
        let mut has_mode_ti = false;
        let mut has_common = false;
        loop {
            match self.peek() {
                TokenKind::Asm => {
                    self.advance();
                    self.consume_if(&TokenKind::Volatile);
                    if matches!(self.peek(), TokenKind::LParen) {
                        self.skip_balanced_parens();
                    }
                }
                TokenKind::Attribute => {
                    let (_, _, mode_ti, common) = self.parse_gcc_attributes();
                    has_mode_ti = has_mode_ti || mode_ti;
                    has_common = has_common || common;
                    if self.parsing_constructor { is_constructor = true; }
                    if self.parsing_destructor { is_destructor = true; }
                }
                TokenKind::Extension => {
                    self.advance();
                }
                _ => break,
            }
        }
        (is_constructor, is_destructor, has_mode_ti, has_common)
    }

    /// Parse the ((...)) parameter list of __attribute__.
    /// Called after the `__attribute__` token has been consumed.
    /// Returns (is_constructor, is_destructor).
    pub(super) fn parse_attribute_params(&mut self) -> (bool, bool) {
        let mut is_constructor = false;
        let mut is_destructor = false;
        if matches!(self.peek(), TokenKind::LParen) {
            self.advance(); // outer (
            if matches!(self.peek(), TokenKind::LParen) {
                self.advance(); // inner (
                // Parse attribute list looking for constructor/destructor
                loop {
                    match self.peek() {
                        TokenKind::Identifier(name) if name == "constructor" || name == "__constructor__" => {
                            is_constructor = true;
                            self.advance();
                            if matches!(self.peek(), TokenKind::LParen) {
                                self.skip_balanced_parens();
                            }
                        }
                        TokenKind::Identifier(name) if name == "destructor" || name == "__destructor__" => {
                            is_destructor = true;
                            self.advance();
                            if matches!(self.peek(), TokenKind::LParen) {
                                self.skip_balanced_parens();
                            }
                        }
                        TokenKind::Identifier(_) => {
                            self.advance();
                            if matches!(self.peek(), TokenKind::LParen) {
                                self.skip_balanced_parens();
                            }
                        }
                        TokenKind::Comma => { self.advance(); }
                        TokenKind::RParen | TokenKind::Eof => break,
                        _ => { self.advance(); }
                    }
                }
                // Inner )
                if matches!(self.peek(), TokenKind::RParen) {
                    self.advance();
                }
                // Outer )
                if matches!(self.peek(), TokenKind::RParen) {
                    self.advance();
                }
            } else {
                // Single-paren form, just skip
                while !matches!(self.peek(), TokenKind::RParen | TokenKind::Eof) {
                    self.advance();
                }
                if matches!(self.peek(), TokenKind::RParen) {
                    self.advance();
                }
            }
        }
        (is_constructor, is_destructor)
    }

    // === Pragma pack handling ===

    /// Check if current token is a pragma pack directive and handle it.
    /// Returns true if a pragma pack token was consumed.
    pub(super) fn handle_pragma_pack_token(&mut self) -> bool {
        match self.peek().clone() {
            TokenKind::PragmaPackSet(n) => {
                self.advance();
                self.pragma_pack_align = if n == 0 { None } else { Some(n) };
                true
            }
            TokenKind::PragmaPackPush(n) => {
                self.advance();
                // Push current alignment onto stack
                self.pragma_pack_stack.push(self.pragma_pack_align);
                // Set new alignment (0 means push without changing)
                if n != 0 {
                    self.pragma_pack_align = Some(n);
                }
                true
            }
            TokenKind::PragmaPackPop => {
                self.advance();
                // Pop previous alignment from stack
                if let Some(prev) = self.pragma_pack_stack.pop() {
                    self.pragma_pack_align = prev;
                } else {
                    // Stack underflow: reset to default
                    self.pragma_pack_align = None;
                }
                true
            }
            TokenKind::PragmaPackReset => {
                self.advance();
                self.pragma_pack_align = None;
                true
            }
            _ => false,
        }
    }

    pub(super) fn skip_balanced_parens(&mut self) {
        if !matches!(self.peek(), TokenKind::LParen) {
            return;
        }
        let mut depth = 0i32;
        loop {
            match self.peek() {
                TokenKind::LParen => { depth += 1; self.advance(); }
                TokenKind::RParen => {
                    depth -= 1;
                    self.advance();
                    if depth <= 0 { break; }
                }
                TokenKind::Eof => break,
                _ => { self.advance(); }
            }
        }
    }

    /// Parse the parenthesized argument of `aligned(expr)` in __attribute__.
    /// Expects the opening `(` to be the current token (not yet consumed).
    /// Parses and evaluates a constant expression, consuming through the closing `)`.
    /// Returns Some(alignment) on success, None on failure.
    pub(super) fn parse_alignment_expr(&mut self) -> Option<usize> {
        if !matches!(self.peek(), TokenKind::LParen) {
            return None;
        }
        self.advance(); // consume opening (
        let expr = self.parse_assignment_expr();
        // Consume closing )
        if matches!(self.peek(), TokenKind::RParen) {
            self.advance();
        }
        Self::eval_const_int_expr(&expr).map(|v| v as usize)
    }

    /// Parse the parenthesized argument of `_Alignas(...)`.
    /// _Alignas can take either a type-name or a constant expression.
    /// Returns Some(alignment) on success, None on failure.
    pub(super) fn parse_alignas_argument(&mut self) -> Option<usize> {
        if !matches!(self.peek(), TokenKind::LParen) {
            return None;
        }
        // Try type-name first using save/restore
        let save = self.pos;
        let save_typedef = self.parsing_typedef;
        self.advance(); // consume (
        if self.is_type_specifier() {
            if let Some(ts) = self.parse_type_specifier() {
                let _result_type = self.parse_abstract_declarator_suffix(ts);
                if matches!(self.peek(), TokenKind::RParen) {
                    self.advance(); // consume )
                    // TODO: evaluate alignof(type) - for now, we'd need the
                    // full type system to compute alignment. Return None to fall
                    // back to default alignment for type-based _Alignas.
                    // Most real code uses integer expressions for alignment.
                    return None;
                }
            }
        }
        // Backtrack and try as constant expression
        self.pos = save;
        self.parsing_typedef = save_typedef;
        self.advance(); // consume (
        let expr = self.parse_assignment_expr();
        if matches!(self.peek(), TokenKind::RParen) {
            self.advance();
        }
        Self::eval_const_int_expr(&expr).map(|v| v as usize)
    }
}
