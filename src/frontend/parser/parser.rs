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

use crate::common::error::DiagnosticEngine;
use crate::common::fx_hash::{FxHashMap, FxHashSet};
use crate::common::source::{Span, SourceManager};
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

/// Accumulated storage-class specifiers, type qualifiers, and GCC attributes
/// parsed during declaration/type-specifier processing.
///
/// These fields are set by `parse_type_specifier` and `parse_gcc_attribute` as
/// keywords/attributes are encountered, then consumed and reset by declaration
/// builders in `declarations.rs`. Grouping them into a single struct replaces
/// 26 scattered fields on `Parser`, enabling bulk reset via `Default::default()`
/// and making the "set then consume" lifecycle explicit.
#[derive(Default)]
pub(super) struct ParsedDeclAttrs {
    // --- Storage-class specifiers ---
    /// `typedef` keyword encountered.
    pub parsing_typedef: bool,
    /// `static` keyword encountered.
    pub parsing_static: bool,
    /// `extern` keyword encountered.
    pub parsing_extern: bool,
    /// `_Thread_local` or `__thread` encountered.
    pub parsing_thread_local: bool,
    /// `inline` keyword encountered.
    pub parsing_inline: bool,

    // --- Type qualifiers ---
    /// `const` qualifier encountered.
    pub parsing_const: bool,
    /// `volatile` qualifier encountered.
    pub parsing_volatile: bool,
    /// `__seg_gs` or `__seg_fs` qualifier encountered.
    pub parsing_address_space: AddressSpace,

    // --- GCC function attributes ---
    /// `__attribute__((constructor))` encountered.
    pub parsing_constructor: bool,
    /// `__attribute__((destructor))` encountered.
    pub parsing_destructor: bool,
    /// `__attribute__((weak))` encountered.
    pub parsing_weak: bool,
    /// `__attribute__((used))` encountered.
    pub parsing_used: bool,
    /// `__attribute__((gnu_inline))` encountered.
    pub parsing_gnu_inline: bool,
    /// `__attribute__((always_inline))` encountered.
    pub parsing_always_inline: bool,
    /// `__attribute__((noinline))` encountered.
    pub parsing_noinline: bool,
    /// `__attribute__((noreturn))` or `_Noreturn` encountered.
    pub parsing_noreturn: bool,
    /// `__attribute__((error("...")))` encountered.
    pub parsing_error_attr: bool,
    /// `__attribute__((transparent_union))` encountered.
    pub parsing_transparent_union: bool,
    /// `__attribute__((fastcall))` encountered (i386 fastcall calling convention).
    pub parsing_fastcall: bool,

    // --- GCC attributes with values ---
    /// `__attribute__((alias("target")))` target symbol name.
    pub parsing_alias_target: Option<String>,
    /// `__attribute__((visibility("...")))` visibility string.
    pub parsing_visibility: Option<String>,
    /// `__attribute__((section("...")))` section name.
    pub parsing_section: Option<String>,
    /// `__attribute__((cleanup(func)))` cleanup function name.
    pub parsing_cleanup_fn: Option<String>,
    /// `__attribute__((vector_size(N)))` total vector size in bytes.
    pub parsing_vector_size: Option<usize>,

    // --- Alignment ---
    /// `_Alignas(N)` or `__attribute__((aligned(N)))` value.
    pub parsed_alignas: Option<usize>,
    /// `_Alignas(type)` type specifier (for deferred alignment resolution).
    pub parsed_alignas_type: Option<TypeSpecifier>,
    /// `__attribute__((aligned(sizeof(type))))` type (for deferred sizeof).
    pub parsed_alignment_sizeof_type: Option<TypeSpecifier>,
}

/// Recursive descent parser for C.
pub struct Parser {
    pub(super) tokens: Vec<Token>,
    pub(super) pos: usize,
    pub(super) typedefs: FxHashSet<String>,
    /// Typedef names shadowed by local variable declarations in the current scope.
    pub(super) shadowed_typedefs: FxHashSet<String>,
    /// Accumulated declaration attributes from the current parse_type_specifier pass.
    /// Reset at the start of each top-level or local declaration.
    pub(super) attrs: ParsedDeclAttrs,
    /// Stack for #pragma pack alignment values.
    /// Current effective alignment is the last element (or None for default).
    pub(super) pragma_pack_stack: Vec<Option<usize>>,
    /// Current #pragma pack alignment. None means default (natural) alignment.
    pub(super) pragma_pack_align: Option<usize>,
    /// Stack for #pragma GCC visibility push/pop.
    /// Each entry is the visibility string (e.g., "hidden", "default").
    pub(super) pragma_visibility_stack: Vec<String>,
    /// Current default visibility from #pragma GCC visibility push(...).
    /// None means default visibility (no pragma active).
    pub(super) pragma_default_visibility: Option<String>,
    /// Count of parse errors encountered (invalid tokens at top level, etc.)
    pub error_count: usize,
    /// Source manager for resolving spans to file:line:col in error messages.
    source_manager: Option<SourceManager>,
    /// Structured diagnostic engine for error/warning reporting with source snippets.
    pub(super) diagnostics: DiagnosticEngine,
    /// Map of enum constant names to their integer values.
    /// Populated as enum definitions are parsed, so that later constant expressions
    /// (e.g., in __attribute__((aligned(1 << ENUM_CONST)))) can resolve them.
    pub(super) enum_constants: FxHashMap<String, i64>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            typedefs: Self::builtin_typedefs(),
            shadowed_typedefs: FxHashSet::default(),
            attrs: ParsedDeclAttrs::default(),
            pragma_pack_stack: Vec::new(),
            pragma_pack_align: None,
            pragma_visibility_stack: Vec::new(),
            pragma_default_visibility: None,
            error_count: 0,
            source_manager: None,
            diagnostics: DiagnosticEngine::new(),
            enum_constants: FxHashMap::default(),
        }
    }

    /// Set the source manager for resolving spans to file:line:col in errors.
    /// Note: the driver typically sets the SM on the DiagnosticEngine instead
    /// (via set_diagnostics), so this is mainly for backward compatibility.
    pub fn set_source_manager(&mut self, sm: SourceManager) {
        self.source_manager = Some(sm);
    }

    /// Take the source manager back from the parser (transfers ownership).
    /// Checks the parser's own field first, then the diagnostic engine.
    /// Used by the driver to pass the source manager to the backend for
    /// debug info (.file/.loc) emission when compiling with -g.
    pub fn take_source_manager(&mut self) -> Option<SourceManager> {
        self.source_manager.take().or_else(|| self.diagnostics.take_source_manager())
    }

    /// Set a pre-configured diagnostic engine on the parser.
    /// Called by the driver after setting up the source manager on the engine.
    pub fn set_diagnostics(&mut self, engine: DiagnosticEngine) {
        self.diagnostics = engine;
    }

    /// Take the diagnostic engine back from the parser (transfers ownership).
    /// The driver uses this to continue using the same engine across phases.
    pub fn take_diagnostics(&mut self) -> DiagnosticEngine {
        std::mem::take(&mut self.diagnostics)
    }

    /// Format a location prefix for error messages from a span.
    /// Returns "file:line:col: " if source manager is available, "" otherwise.
    /// Checks both the parser's own source_manager and the diagnostic engine's.
    /// Kept for use by parser extensions and future diagnostic improvements.
    #[allow(dead_code)]
    pub(super) fn span_to_location(&self, span: Span) -> String {
        if let Some(ref sm) = self.source_manager {
            let loc = sm.resolve_span(span);
            format!("{}:{}:{}: ", loc.file, loc.line, loc.column)
        } else if let Some(sm) = self.diagnostics.source_manager() {
            let loc = sm.resolve_span(span);
            format!("{}:{}:{}: ", loc.file, loc.line, loc.column)
        } else {
            String::new()
        }
    }

    /// Emit a parse error at the given span. Updates error_count and prints
    /// the error with source location and snippet (if source manager is set).
    pub(super) fn emit_error(&mut self, message: impl Into<String>, span: Span) {
        self.error_count += 1;
        self.diagnostics.error(message, span);
    }

    /// Emit a parse warning at the given span.
    /// Not yet used; available for adding parser warnings (e.g., implicit conversions).
    #[allow(dead_code)]
    pub(super) fn emit_warning(&mut self, message: impl Into<String>, span: Span) {
        self.diagnostics.warning(message, span);
    }

    /// Standard C typedef names commonly provided by system headers.
    /// Since we don't actually include system headers, we pre-seed these.
    fn builtin_typedefs() -> FxHashSet<String> {
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
                    let span = self.peek_span();
                    self.emit_error(format!("expected declaration, got {:?}", self.peek()), span);
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
            self.emit_error(format!("expected {:?}, got {:?}", expected, self.peek()), span);
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
                    self.attrs.parsing_address_space = AddressSpace::SegGs;
                }
                TokenKind::SegFs => {
                    self.advance();
                    self.attrs.parsing_address_space = AddressSpace::SegFs;
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
        let (_, aligned, _, _) = self.parse_gcc_attributes();
        if let Some(a) = aligned {
            self.attrs.parsed_alignas = Some(self.attrs.parsed_alignas.map_or(a, |prev| prev.max(a)));
        }
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
                                        self.attrs.parsing_constructor = true;
                                        self.advance();
                                        if matches!(self.peek(), TokenKind::LParen) {
                                            self.skip_balanced_parens();
                                        }
                                    }
                                    TokenKind::Identifier(name) if name == "destructor" || name == "__destructor__" => {
                                        self.attrs.parsing_destructor = true;
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
                                        self.attrs.parsing_transparent_union = true;
                                        self.advance();
                                    }
                                    TokenKind::Identifier(name) if name == "weak" || name == "__weak__" => {
                                        self.attrs.parsing_weak = true;
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
                                                self.attrs.parsing_alias_target = Some(result);
                                            }
                                            // consume )
                                            if matches!(self.peek(), TokenKind::RParen) {
                                                self.advance();
                                            }
                                        }
                                    }
                                    TokenKind::Identifier(name) if name == "weakref" || name == "__weakref__" => {
                                        // weakref("target") is equivalent to weak + alias("target")
                                        self.attrs.parsing_weak = true;
                                        self.advance();
                                        if matches!(self.peek(), TokenKind::LParen) {
                                            self.advance(); // consume (
                                            let mut result = String::new();
                                            while let TokenKind::StringLiteral(s) = self.peek() {
                                                result.push_str(&s);
                                                self.advance();
                                            }
                                            if !result.is_empty() {
                                                self.attrs.parsing_alias_target = Some(result);
                                            }
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
                                                self.attrs.parsing_visibility = Some(result);
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
                                                self.attrs.parsing_section = Some(result);
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
                                        self.attrs.parsing_error_attr = true;
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
                                    TokenKind::Identifier(name) if name == "noreturn" => {
                                        self.attrs.parsing_noreturn = true;
                                        self.advance();
                                    }
                                    // __noreturn__ is tokenized as TokenKind::Noreturn by the lexer,
                                    // so handle the keyword form here in addition to the identifier "noreturn".
                                    TokenKind::Noreturn => {
                                        self.attrs.parsing_noreturn = true;
                                        self.advance();
                                    }
                                    TokenKind::Identifier(name) if name == "gnu_inline" || name == "__gnu_inline__" => {
                                        self.attrs.parsing_gnu_inline = true;
                                        self.advance();
                                    }
                                    TokenKind::Identifier(name) if name == "always_inline" || name == "__always_inline__" => {
                                        self.attrs.parsing_always_inline = true;
                                        self.advance();
                                    }
                                    TokenKind::Identifier(name) if name == "noinline" || name == "__noinline__" => {
                                        self.attrs.parsing_noinline = true;
                                        self.advance();
                                    }
                                    TokenKind::Identifier(name) if name == "cleanup" || name == "__cleanup__" => {
                                        self.advance();
                                        // Parse cleanup(func_name) - the function to call when variable goes out of scope
                                        if matches!(self.peek(), TokenKind::LParen) {
                                            self.advance(); // consume (
                                            if let TokenKind::Identifier(func_name) = self.peek() {
                                                self.attrs.parsing_cleanup_fn = Some(func_name.clone());
                                                self.advance();
                                            }
                                            if matches!(self.peek(), TokenKind::RParen) {
                                                self.advance(); // consume )
                                            }
                                        }
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
                                    TokenKind::Identifier(name) if name == "vector_size" || name == "__vector_size__" => {
                                        self.advance();
                                        if matches!(self.peek(), TokenKind::LParen) {
                                            self.advance(); // consume (
                                            // Parse the vector size expression (must be a constant)
                                            // TODO: Validate vector_size is a power of 2 and element type is numeric
                                            let expr = self.parse_assignment_expr();
                                            let enums = if self.enum_constants.is_empty() { None } else { Some(&self.enum_constants) };
                                            if let Some(size) = Self::eval_const_int_expr_with_enums(&expr, enums) {
                                                self.attrs.parsing_vector_size = Some(size as usize);
                                            }
                                            if matches!(self.peek(), TokenKind::RParen) {
                                                self.advance();
                                            }
                                        }
                                    }
                                    TokenKind::Identifier(name) if name == "used" || name == "__used__" => {
                                        self.attrs.parsing_used = true;
                                        self.advance();
                                    }
                                    TokenKind::Identifier(name) if name == "fastcall" || name == "__fastcall__" => {
                                        self.attrs.parsing_fastcall = true;
                                        self.advance();
                                    }
                                    TokenKind::Identifier(name) if name == "address_space" || name == "__address_space__" => {
                                        self.advance();
                                        // Parse address_space(__seg_gs) / address_space(__seg_fs)
                                        if matches!(self.peek(), TokenKind::LParen) {
                                            self.advance(); // consume (
                                            match self.peek() {
                                                TokenKind::SegGs => {
                                                    self.advance();
                                                    self.attrs.parsing_address_space = AddressSpace::SegGs;
                                                }
                                                TokenKind::SegFs => {
                                                    self.advance();
                                                    self.attrs.parsing_address_space = AddressSpace::SegFs;
                                                }
                                                _ => {
                                                    // Unknown address space, skip
                                                    self.advance();
                                                }
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
                    if self.attrs.parsing_constructor { is_constructor = true; }
                    if self.attrs.parsing_destructor { is_destructor = true; }
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

    /// Handle #pragma GCC visibility push/pop synthetic tokens.
    /// Returns true if a token was consumed.
    pub(super) fn handle_pragma_visibility_token(&mut self) -> bool {
        match self.peek() {
            TokenKind::PragmaVisibilityPush(vis) => {
                let vis = vis.clone();
                self.advance();
                // Push current visibility and set new default
                if let Some(ref current) = self.pragma_default_visibility {
                    self.pragma_visibility_stack.push(current.clone());
                } else {
                    // Push a sentinel for "no pragma active"
                    self.pragma_visibility_stack.push(String::new());
                }
                if vis == "default" {
                    // "default" means no special visibility
                    self.pragma_default_visibility = None;
                } else {
                    self.pragma_default_visibility = Some(vis);
                }
                true
            }
            TokenKind::PragmaVisibilityPop => {
                self.advance();
                // Pop previous visibility
                if let Some(prev) = self.pragma_visibility_stack.pop() {
                    if prev.is_empty() {
                        self.pragma_default_visibility = None;
                    } else {
                        self.pragma_default_visibility = Some(prev);
                    }
                } else {
                    // Stack underflow: reset to no pragma
                    self.pragma_default_visibility = None;
                }
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

    /// Skip optional GNU label attributes after a label colon.
    /// GNU C allows `label: __attribute__((unused));` to suppress unused-label warnings.
    /// We consume the entire `__attribute__((...))` token sequence and discard it.
    pub(super) fn skip_label_attributes(&mut self) {
        while matches!(self.peek(), TokenKind::Attribute) {
            self.advance(); // consume __attribute__
            // Expect __attribute__((...)) — two levels of parens
            if matches!(self.peek(), TokenKind::LParen) {
                self.skip_balanced_parens();
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
        // If the expression is sizeof(type), capture the type so sema/lowerer can
        // recompute with accurate struct/union layout info (the parser's sizeof
        // uses a conservative default for struct/union types).
        if let Expr::Sizeof(ref arg, _) = expr {
            if let SizeofArg::Type(ref ts) = **arg {
                self.attrs.parsed_alignment_sizeof_type = Some(ts.clone());
            }
        }
        let enums = if self.enum_constants.is_empty() { None } else { Some(&self.enum_constants) };
        Self::eval_const_int_expr_with_enums(&expr, enums).map(|v| v as usize)
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
        let save_typedef = self.attrs.parsing_typedef;
        self.advance(); // consume (
        if self.is_type_specifier() {
            if let Some(ts) = self.parse_type_specifier() {
                let result_type = self.parse_abstract_declarator_suffix(ts);
                if matches!(self.peek(), TokenKind::RParen) {
                    self.advance(); // consume )
                    // Save the type specifier so the lowerer can resolve typedefs
                    // and compute accurate alignment (parser can't resolve typedefs).
                    self.attrs.parsed_alignas_type = Some(result_type.clone());
                    return Some(Self::alignof_type_spec(&result_type));
                }
            }
        }
        // Backtrack and try as constant expression
        self.pos = save;
        self.attrs.parsing_typedef = save_typedef;
        self.advance(); // consume (
        let expr = self.parse_assignment_expr();
        if matches!(self.peek(), TokenKind::RParen) {
            self.advance();
        }
        let enums = if self.enum_constants.is_empty() { None } else { Some(&self.enum_constants) };
        Self::eval_const_int_expr_with_enums(&expr, enums).map(|v| v as usize)
    }

    /// Compute sizeof (in bytes) for a type specifier.
    /// Used by sizeof(type) in parser-level constant evaluation.
    pub(super) fn sizeof_type_spec(ts: &TypeSpecifier) -> usize {
        use crate::common::types::target_ptr_size;
        let ptr_sz = target_ptr_size();
        match ts {
            TypeSpecifier::Void | TypeSpecifier::Bool
            | TypeSpecifier::Char | TypeSpecifier::UnsignedChar => 1,
            TypeSpecifier::Short | TypeSpecifier::UnsignedShort => 2,
            TypeSpecifier::Int | TypeSpecifier::UnsignedInt
            | TypeSpecifier::Signed | TypeSpecifier::Unsigned
            | TypeSpecifier::Float => 4,
            TypeSpecifier::Long | TypeSpecifier::UnsignedLong => ptr_sz,
            TypeSpecifier::LongLong | TypeSpecifier::UnsignedLongLong
            | TypeSpecifier::Double => 8,
            TypeSpecifier::Pointer(_, _) | TypeSpecifier::FunctionPointer(_, _, _) => ptr_sz,
            TypeSpecifier::Int128 | TypeSpecifier::UnsignedInt128 => 16,
            TypeSpecifier::LongDouble => if ptr_sz == 4 { 12 } else { 16 },
            TypeSpecifier::ComplexFloat => 8,
            TypeSpecifier::ComplexDouble => 16,
            TypeSpecifier::ComplexLongDouble => if ptr_sz == 4 { 24 } else { 32 },
            TypeSpecifier::Array(elem, Some(size_expr)) => {
                let elem_size = Self::sizeof_type_spec(elem);
                let count = Self::eval_const_int_expr(size_expr).unwrap_or(0) as usize;
                elem_size * count
            }
            TypeSpecifier::Array(_, None) => 0,
            TypeSpecifier::Enum(_, _, _) => 4,
            _ => ptr_sz, // conservative default for struct/union/typedef
        }
    }

    /// Compute alignment (in bytes) for a type specifier.
    /// Used by _Alignas(type) to determine the alignment value.
    pub(super) fn alignof_type_spec(ts: &TypeSpecifier) -> usize {
        use crate::common::types::target_ptr_size;
        let ptr_sz = target_ptr_size();
        match ts {
            TypeSpecifier::Void | TypeSpecifier::Bool
            | TypeSpecifier::Char | TypeSpecifier::UnsignedChar => 1,
            TypeSpecifier::Short | TypeSpecifier::UnsignedShort => 2,
            TypeSpecifier::Int | TypeSpecifier::UnsignedInt
            | TypeSpecifier::Signed | TypeSpecifier::Unsigned
            | TypeSpecifier::Float => 4,
            TypeSpecifier::Long | TypeSpecifier::UnsignedLong => ptr_sz,
            // On i686 (ILP32), long long and double are aligned to 4 bytes per i386 SysV ABI
            TypeSpecifier::LongLong | TypeSpecifier::UnsignedLongLong
            | TypeSpecifier::Double => if ptr_sz == 4 { 4 } else { 8 },
            TypeSpecifier::Pointer(_, _) | TypeSpecifier::FunctionPointer(_, _, _) => ptr_sz,
            TypeSpecifier::Int128 | TypeSpecifier::UnsignedInt128 => 16,
            // On i686, long double is 80-bit x87 but aligned to 4 bytes
            TypeSpecifier::LongDouble => if ptr_sz == 4 { 4 } else { 16 },
            TypeSpecifier::ComplexFloat => 4,
            TypeSpecifier::ComplexDouble => if ptr_sz == 4 { 4 } else { 8 },
            TypeSpecifier::ComplexLongDouble => if ptr_sz == 4 { 4 } else { 16 },
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
                if align == 0 { ptr_sz } else { align }
            }
            TypeSpecifier::Enum(_, _, _) => 4,
            TypeSpecifier::TypedefName(_) => ptr_sz, // conservative default
            _ => ptr_sz,
        }
    }

    /// Compute preferred (natural) alignment for a type specifier.
    /// Used by GCC's __alignof/__alignof__ which returns preferred alignment.
    /// On i686: __alignof__(long long) == 8, __alignof__(double) == 8,
    /// while _Alignof returns 4 for both (minimum ABI alignment).
    pub(super) fn preferred_alignof_type_spec(ts: &TypeSpecifier) -> usize {
        use crate::common::types::target_ptr_size;
        let ptr_sz = target_ptr_size();
        if ptr_sz != 4 {
            // On 64-bit targets, preferred == ABI alignment
            return Self::alignof_type_spec(ts);
        }
        // On i686: long long and double have preferred alignment of 8
        match ts {
            TypeSpecifier::LongLong | TypeSpecifier::UnsignedLongLong
            | TypeSpecifier::Double => 8,
            TypeSpecifier::ComplexDouble => 8,
            TypeSpecifier::Array(elem, _) => Self::preferred_alignof_type_spec(elem),
            _ => Self::alignof_type_spec(ts),
        }
    }
}
