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
use crate::common::types::AddressSpace;
use crate::frontend::lexer::token::{Token, TokenKind};
use super::ast::*;

/// GCC __attribute__((mode(...))) integer mode specifier.
/// Controls the bit-width of an integer type regardless of the base type keyword.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ModeKind {
    QI,  // 8-bit (quarter integer)
    HI,  // 16-bit (half integer)
    SI,  // 32-bit (single integer)
    DI,  // 64-bit (double integer)
    TI,  // 128-bit (tetra integer)
}

impl ModeKind {
    /// Apply mode to a type specifier, preserving signedness.
    pub(super) fn apply(self, ts: TypeSpecifier) -> TypeSpecifier {
        let is_unsigned = matches!(ts,
            TypeSpecifier::UnsignedInt | TypeSpecifier::UnsignedLong
            | TypeSpecifier::UnsignedLongLong | TypeSpecifier::Unsigned
            | TypeSpecifier::UnsignedChar | TypeSpecifier::UnsignedShort
        );
        match self {
            ModeKind::QI => if is_unsigned { TypeSpecifier::UnsignedChar } else { TypeSpecifier::Char },
            ModeKind::HI => if is_unsigned { TypeSpecifier::UnsignedShort } else { TypeSpecifier::Short },
            ModeKind::SI => if is_unsigned { TypeSpecifier::UnsignedInt } else { TypeSpecifier::Int },
            ModeKind::DI => if is_unsigned { TypeSpecifier::UnsignedLongLong } else { TypeSpecifier::LongLong },
            ModeKind::TI => if is_unsigned { TypeSpecifier::UnsignedInt128 } else { TypeSpecifier::Int128 },
        }
    }
}

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
    /// Set to true when parse_type_specifier encounters a `volatile` qualifier.
    pub(super) parsing_volatile: bool,
    /// Set to true when parse_type_specifier encounters __attribute__((constructor)).
    pub(super) parsing_constructor: bool,
    /// Set to true when parse_type_specifier encounters __attribute__((destructor)).
    pub(super) parsing_destructor: bool,
    /// Set to true when __attribute__((transparent_union)) is encountered on a typedef.
    pub(super) parsing_transparent_union: bool,
    /// Set to true when __attribute__((weak)) is encountered on a declaration.
    pub(super) parsing_weak: bool,
    /// Set when __attribute__((alias("target"))) is encountered; holds the target symbol name.
    pub(super) parsing_alias_target: Option<String>,
    /// Set when __attribute__((visibility("..."))) is encountered; holds the visibility string.
    pub(super) parsing_visibility: Option<String>,
    /// Set when __attribute__((section("..."))) is encountered; holds the section name.
    pub(super) parsing_section: Option<String>,
    /// Set to true when __attribute__((error("..."))) is encountered.
    /// Functions with this attribute are compile-time error traps: calls to them
    /// should be treated as unreachable (they're linker error assertions).
    pub(super) parsing_error_attr: bool,
    /// Set to true when __attribute__((gnu_inline)) is encountered.
    /// Forces GNU89 inline semantics: extern inline = no external definition emitted.
    pub(super) parsing_gnu_inline: bool,
    /// Set to true when __attribute__((always_inline)) is encountered.
    /// Functions with this attribute must always be inlined and should not be
    /// emitted as standalone functions.
    pub(super) parsing_always_inline: bool,
    /// Set when parse_type_specifier or consume_post_type_qualifiers encounters _Alignas(N).
    /// Consumed and reset by callers that need it (e.g., struct field parsing).
    pub(super) parsed_alignas: Option<usize>,
    /// When _Alignas takes a type argument, store the type specifier so the lowerer can
    /// resolve typedefs and compute the correct alignment (the parser can't resolve typedefs).
    pub(super) parsed_alignas_type: Option<TypeSpecifier>,
    /// Stack for #pragma pack alignment values.
    /// Current effective alignment is the last element (or None for default).
    pub(super) pragma_pack_stack: Vec<Option<usize>>,
    /// Current #pragma pack alignment. None means default (natural) alignment.
    pub(super) pragma_pack_align: Option<usize>,
    /// Count of parse errors encountered (invalid tokens at top level, etc.)
    pub error_count: usize,
    /// Set to the address space when __seg_gs or __seg_fs qualifier is encountered.
    /// Reset after being consumed by pointer type construction.
    pub(super) parsing_address_space: AddressSpace,
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
            parsing_volatile: false,
            parsing_constructor: false,
            parsing_destructor: false,
            parsing_transparent_union: false,
            parsing_weak: false,
            parsing_alias_target: None,
            parsing_visibility: None,
            parsing_section: None,
            parsing_error_attr: false,
            parsing_gnu_inline: false,
            parsing_always_inline: false,
            parsed_alignas: None,
            parsed_alignas_type: None,
            pragma_pack_stack: Vec::new(),
            pragma_pack_align: None,
            error_count: 0,
            parsing_address_space: AddressSpace::Default,
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
                if !matches!(self.peek(), TokenKind::Semicolon | TokenKind::Eof) {
                    self.error_count += 1;
                    eprintln!("error: expected declaration, got {:?}", self.peek());
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
            let _context: Vec<_> = self.tokens[start..end].iter().map(|t| {
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
            TokenKind::Atomic | TokenKind::Auto | TokenKind::AutoType | TokenKind::Alignas |
            TokenKind::Builtin | TokenKind::Int128 | TokenKind::UInt128 |
            TokenKind::ThreadLocal | TokenKind::SegGs | TokenKind::SegFs => true,
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
                TokenKind::SegGs => {
                    self.advance();
                    self.parsing_address_space = AddressSpace::SegGs;
                }
                TokenKind::SegFs => {
                    self.advance();
                    self.parsing_address_space = AddressSpace::SegFs;
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
    /// Returns (is_packed, aligned_value, mode_kind, is_common).
    pub(super) fn parse_gcc_attributes(&mut self) -> (bool, Option<usize>, Option<ModeKind>, bool) {
        let mut is_packed = false;
        let mut _aligned = None;
        let mut mode_kind: Option<ModeKind> = None;
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
                                    TokenKind::Identifier(name) if name == "transparent_union" || name == "__transparent_union__" => {
                                        self.parsing_transparent_union = true;
                                        self.advance();
                                    }
                                    TokenKind::Identifier(name) if name == "weak" || name == "__weak__" => {
                                        self.parsing_weak = true;
                                        self.advance();
                                    }
                                    TokenKind::Identifier(name) if name == "alias" || name == "__alias__" => {
                                        self.advance();
                                        // Parse alias("target_name") - supports string concatenation
                                        if matches!(self.peek(), TokenKind::LParen) {
                                            self.advance(); // consume (
                                            let mut result = String::new();
                                            while let TokenKind::StringLiteral(s) = self.peek() {
                                                result.push_str(&s);
                                                self.advance();
                                            }
                                            if !result.is_empty() {
                                                self.parsing_alias_target = Some(result);
                                            }
                                            // consume )
                                            if matches!(self.peek(), TokenKind::RParen) {
                                                self.advance();
                                            }
                                        }
                                    }
                                    TokenKind::Identifier(name) if name == "visibility" || name == "__visibility__" => {
                                        self.advance();
                                        // Parse visibility("hidden"|"default"|"internal"|"protected") - supports string concatenation
                                        if matches!(self.peek(), TokenKind::LParen) {
                                            self.advance(); // consume (
                                            let mut result = String::new();
                                            while let TokenKind::StringLiteral(s) = self.peek() {
                                                result.push_str(&s);
                                                self.advance();
                                            }
                                            if !result.is_empty() {
                                                self.parsing_visibility = Some(result);
                                            }
                                            if matches!(self.peek(), TokenKind::RParen) {
                                                self.advance();
                                            }
                                        }
                                    }
                                    TokenKind::Identifier(name) if name == "section" || name == "__section__" => {
                                        self.advance();
                                        // Parse section("name") - supports string concatenation
                                        if matches!(self.peek(), TokenKind::LParen) {
                                            self.advance(); // consume (
                                            let mut result = String::new();
                                            while let TokenKind::StringLiteral(s) = self.peek() {
                                                result.push_str(&s);
                                                self.advance();
                                            }
                                            if !result.is_empty() {
                                                self.parsing_section = Some(result);
                                            }
                                            if matches!(self.peek(), TokenKind::RParen) {
                                                self.advance();
                                            }
                                        }
                                    }
                                    TokenKind::Identifier(name) if name == "error" || name == "__error__"
                                        || name == "warning" || name == "__warning__" => {
                                        // __attribute__((error("msg"))) / __attribute__((warning("msg")))
                                        // These are GCC compile-time assertion mechanisms: if a call to
                                        // such a function survives to codegen, GCC emits a compile error.
                                        // The Linux kernel uses this for __compiletime_error() traps.
                                        self.parsing_error_attr = true;
                                        self.advance();
                                        // Skip the ("message") argument
                                        if matches!(self.peek(), TokenKind::LParen) {
                                            self.advance();
                                            while !matches!(self.peek(), TokenKind::RParen | TokenKind::Eof) {
                                                self.advance();
                                            }
                                            if matches!(self.peek(), TokenKind::RParen) {
                                                self.advance();
                                            }
                                        }
                                    }
                                    TokenKind::Identifier(name) if name == "gnu_inline" || name == "__gnu_inline__" => {
                                        self.parsing_gnu_inline = true;
                                        self.advance();
                                    }
                                    TokenKind::Identifier(name) if name == "always_inline" || name == "__always_inline__" => {
                                        self.parsing_always_inline = true;
                                        self.advance();
                                    }
                                    TokenKind::Identifier(name) if name == "mode" || name == "__mode__" => {
                                        self.advance();
                                        if matches!(self.peek(), TokenKind::LParen) {
                                            self.advance(); // consume (
                                            // Parse mode specifier: QI (8-bit), HI (16-bit),
                                            // SI (32-bit), DI (64-bit), TI (128-bit)
                                            if let TokenKind::Identifier(mode_name) = self.peek() {
                                                mode_kind = match mode_name.as_str() {
                                                    "QI" | "__QI__" | "byte" | "__byte__" => Some(ModeKind::QI),
                                                    "HI" | "__HI__" => Some(ModeKind::HI),
                                                    "SI" | "__SI__" => Some(ModeKind::SI),
                                                    "DI" | "__DI__" => Some(ModeKind::DI),
                                                    "TI" | "__TI__" => Some(ModeKind::TI),
                                                    // "word" = machine word size (64-bit on x86-64/aarch64/riscv64)
                                                    "word" | "__word__" => Some(ModeKind::DI),
                                                    // "pointer" = pointer-sized (same as word on 64-bit)
                                                    "pointer" | "__pointer__" => Some(ModeKind::DI),
                                                    _ => mode_kind, // unknown mode, leave unchanged
                                                };
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
        (is_packed, _aligned, mode_kind, is_common)
    }

    /// Skip __asm__("..."), __attribute__(...), and __extension__ after declarators.
    /// Returns (mode_kind, aligned_value, asm_register).
    pub(super) fn skip_asm_and_attributes(&mut self) -> (Option<ModeKind>, Option<usize>, Option<String>) {
        let (_, _, mk, _, aligned, asm_reg) = self.parse_asm_and_attributes();
        (mk, aligned, asm_reg)
    }

    /// Parse __asm__("..."), __attribute__(...), and __extension__ after declarators.
    /// Returns (is_constructor, is_destructor, mode_kind, is_common, aligned_value, asm_register).
    /// The asm_register captures the register name from `register var __asm__("regname")`.
    pub(super) fn parse_asm_and_attributes(&mut self) -> (bool, bool, Option<ModeKind>, bool, Option<usize>, Option<String>) {
        let mut is_constructor = false;
        let mut is_destructor = false;
        let mut mode_kind: Option<ModeKind> = None;
        let mut has_common = false;
        let mut aligned: Option<usize> = None;
        let mut asm_register: Option<String> = None;
        loop {
            match self.peek() {
                TokenKind::Asm => {
                    self.advance();
                    self.consume_if(&TokenKind::Volatile);
                    if matches!(self.peek(), TokenKind::LParen) {
                        // Try to extract the asm register name: __asm__("regname")
                        // This is a single string literal inside parentheses.
                        asm_register = asm_register.or_else(|| self.try_parse_asm_register_name());
                    }
                }
                TokenKind::Attribute => {
                    let (_, attr_aligned, mk, common) = self.parse_gcc_attributes();
                    mode_kind = mode_kind.or(mk);
                    has_common = has_common || common;
                    if let Some(a) = attr_aligned {
                        aligned = Some(aligned.map_or(a, |prev| prev.max(a)));
                    }
                    if self.parsing_constructor { is_constructor = true; }
                    if self.parsing_destructor { is_destructor = true; }
                }
                TokenKind::Extension => {
                    self.advance();
                }
                _ => break,
            }
        }
        (is_constructor, is_destructor, mode_kind, has_common, aligned, asm_register)
    }

    /// Try to extract a label/register name from __asm__("name") on a declaration.
    /// Called when the current token is LParen after __asm__. Handles both:
    /// - Register pinning: `register int x __asm__("rbx")`
    /// - Linker symbol redirect: `extern int foo(...) __asm__("" "__xpg_strerror_r")`
    ///
    /// Supports concatenation of adjacent string literals (e.g., `__asm__("" "name")`).
    /// Returns Some("name") if a non-empty label was found, None otherwise.
    fn try_parse_asm_register_name(&mut self) -> Option<String> {
        // Save position so we can fall back
        let saved_pos = self.pos;
        self.advance(); // consume '('
        // Concatenate adjacent string literals: __asm__("" "name") -> "name"
        let mut combined = String::new();
        let mut found_string = false;
        while let TokenKind::StringLiteral(s) = self.peek() {
            combined.push_str(&s);
            found_string = true;
            self.advance();
        }
        if found_string && matches!(self.peek(), TokenKind::RParen) {
            self.advance(); // consume ')'
            if !combined.is_empty() {
                // Strip leading '%' prefix from register names.
                // In C source, register names in asm() may use the GCC inline asm
                // convention with a '%' prefix (e.g., asm("%rdx") or asm("%" "rdx")),
                // but the actual register name used internally should be just "rdx".
                let combined = combined.trim_start_matches('%').to_string();
                if !combined.is_empty() {
                    return Some(combined);
                }
            }
            return None;
        }
        // Not a simple string literal sequence, restore and skip
        self.pos = saved_pos;
        self.skip_balanced_parens();
        None
    }

    // === Pragma pack handling ===

    /// Check if current token is a pragma pack directive and handle it.
    /// Returns true if a pragma pack token was consumed.
    pub(super) fn handle_pragma_pack_token(&mut self) -> bool {
        match self.peek() {
            TokenKind::PragmaPackSet(n) => {
                let n = *n;
                self.advance();
                // pack(0) means reset to default (natural alignment)
                self.pragma_pack_align = if n == 0 { None } else { Some(n) };
                true
            }
            TokenKind::PragmaPackPush(n) => {
                let n = *n;
                self.advance();
                // Push current alignment onto stack
                self.pragma_pack_stack.push(self.pragma_pack_align);
                // pack(push, 0) means push and reset to default (natural alignment)
                // pack(push, N) means push and set to N
                if n == 0 {
                    self.pragma_pack_align = None;
                } else {
                    self.pragma_pack_align = Some(n);
                }
                true
            }
            TokenKind::PragmaPackPushOnly => {
                self.advance();
                // pack(push) - push current alignment without changing it
                self.pragma_pack_stack.push(self.pragma_pack_align);
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
                let result_type = self.parse_abstract_declarator_suffix(ts);
                if matches!(self.peek(), TokenKind::RParen) {
                    self.advance(); // consume )
                    // Save the type specifier so the lowerer can resolve typedefs
                    // and compute accurate alignment (parser can't resolve typedefs).
                    self.parsed_alignas_type = Some(result_type.clone());
                    return Some(Self::alignof_type_spec(&result_type));
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

    /// Compute sizeof (in bytes) for a type specifier.
    /// Used by sizeof(type) in parser-level constant evaluation.
    pub(super) fn sizeof_type_spec(ts: &TypeSpecifier) -> usize {
        match ts {
            TypeSpecifier::Void | TypeSpecifier::Bool
            | TypeSpecifier::Char | TypeSpecifier::UnsignedChar => 1,
            TypeSpecifier::Short | TypeSpecifier::UnsignedShort => 2,
            TypeSpecifier::Int | TypeSpecifier::UnsignedInt
            | TypeSpecifier::Signed | TypeSpecifier::Unsigned
            | TypeSpecifier::Float => 4,
            TypeSpecifier::Long | TypeSpecifier::UnsignedLong
            | TypeSpecifier::LongLong | TypeSpecifier::UnsignedLongLong
            | TypeSpecifier::Double
            | TypeSpecifier::Pointer(_, _) | TypeSpecifier::FunctionPointer(_, _, _) => 8,
            TypeSpecifier::Int128 | TypeSpecifier::UnsignedInt128
            | TypeSpecifier::LongDouble => 16,
            TypeSpecifier::ComplexFloat => 8,
            TypeSpecifier::ComplexDouble => 16,
            TypeSpecifier::ComplexLongDouble => 32,
            TypeSpecifier::Array(elem, Some(size_expr)) => {
                let elem_size = Self::sizeof_type_spec(elem);
                let count = Self::eval_const_int_expr(size_expr).unwrap_or(0) as usize;
                elem_size * count
            }
            TypeSpecifier::Array(_, None) => 0,
            TypeSpecifier::Enum(_, _) => 4,
            _ => 8, // conservative default for struct/union/typedef
        }
    }

    /// Compute alignment (in bytes) for a type specifier.
    /// Used by _Alignas(type) to determine the alignment value.
    pub(super) fn alignof_type_spec(ts: &TypeSpecifier) -> usize {
        match ts {
            TypeSpecifier::Void | TypeSpecifier::Bool
            | TypeSpecifier::Char | TypeSpecifier::UnsignedChar => 1,
            TypeSpecifier::Short | TypeSpecifier::UnsignedShort => 2,
            TypeSpecifier::Int | TypeSpecifier::UnsignedInt
            | TypeSpecifier::Signed | TypeSpecifier::Unsigned
            | TypeSpecifier::Float => 4,
            TypeSpecifier::Long | TypeSpecifier::UnsignedLong
            | TypeSpecifier::LongLong | TypeSpecifier::UnsignedLongLong
            | TypeSpecifier::Double
            | TypeSpecifier::Pointer(_, _) | TypeSpecifier::FunctionPointer(_, _, _) => 8,
            TypeSpecifier::Int128 | TypeSpecifier::UnsignedInt128
            | TypeSpecifier::LongDouble => 16,
            TypeSpecifier::ComplexFloat => 4,
            TypeSpecifier::ComplexDouble => 8,
            TypeSpecifier::ComplexLongDouble => 16,
            TypeSpecifier::Array(elem, _) => Self::alignof_type_spec(elem),
            TypeSpecifier::Struct(_, fields, is_packed, _, struct_aligned)
            | TypeSpecifier::Union(_, fields, is_packed, _, struct_aligned) => {
                if *is_packed { return 1; }
                // Explicit struct/union-level __attribute__((aligned(N))) overrides
                let mut align = struct_aligned.unwrap_or(0);
                // Compute max alignment from fields if available
                if let Some(field_list) = fields {
                    for field in field_list {
                        let field_align = if let Some(fa) = field.alignment {
                            fa
                        } else {
                            Self::alignof_type_spec(&field.type_spec)
                        };
                        align = align.max(field_align);
                    }
                }
                // Fallback for empty struct/union or tag-only (no fields available)
                if align == 0 { 8 } else { align }
            }
            TypeSpecifier::Enum(_, _) => 4,
            TypeSpecifier::TypedefName(_) => 8, // conservative default
            _ => 8,
        }
    }
}
