use crate::common::source::Span;
use crate::frontend::lexer::token::{Token, TokenKind};
use super::ast::*;

/// Recursive descent parser for C.
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    typedefs: Vec<String>,
    /// Typedef names shadowed by local variable declarations in the current scope.
    /// When a variable is declared with the same name as a typedef (e.g., `st02 st02;`),
    /// subsequent uses of that name should be treated as the variable, not the type.
    shadowed_typedefs: Vec<String>,
    /// Set to true when parse_type_specifier encounters a `typedef` keyword.
    /// Used by declaration parsers to register the declared names as typedefs.
    parsing_typedef: bool,
    /// Set to true when parse_type_specifier encounters a `static` keyword.
    /// Used to propagate storage class to Declaration nodes.
    parsing_static: bool,
    /// Set to true when parse_type_specifier encounters an `extern` keyword.
    parsing_extern: bool,
    /// Set to true when parse_type_specifier encounters an `inline` keyword.
    parsing_inline: bool,
    /// Set to true when parse_type_specifier encounters a `const` qualifier.
    parsing_const: bool,
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
        }
    }

    /// Standard C typedef names that are commonly provided by system headers.
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
                // Skip token on error
                self.advance();
            }
        }
        TranslationUnit { decls }
    }

    // === Helpers ===

    fn at_eof(&self) -> bool {
        self.pos >= self.tokens.len() || matches!(self.tokens[self.pos].kind, TokenKind::Eof)
    }

    fn peek(&self) -> &TokenKind {
        if self.pos < self.tokens.len() {
            &self.tokens[self.pos].kind
        } else {
            &TokenKind::Eof
        }
    }

    fn peek_span(&self) -> Span {
        if self.pos < self.tokens.len() {
            self.tokens[self.pos].span
        } else {
            Span::dummy()
        }
    }

    fn advance(&mut self) -> &Token {
        let tok = &self.tokens[self.pos];
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
        tok
    }

    fn expect(&mut self, expected: &TokenKind) -> Span {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(expected) {
            let span = self.peek_span();
            self.advance();
            span
        } else {
            let span = self.peek_span();
            // Try to recover
            eprintln!("parser error: expected {:?}, got {:?}", expected, self.peek());
            span
        }
    }

    fn consume_if(&mut self, kind: &TokenKind) -> bool {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn is_type_specifier(&self) -> bool {
        match self.peek() {
            TokenKind::Void | TokenKind::Char | TokenKind::Short | TokenKind::Int |
            TokenKind::Long | TokenKind::Float | TokenKind::Double | TokenKind::Signed |
            TokenKind::Unsigned | TokenKind::Struct | TokenKind::Union | TokenKind::Enum |
            TokenKind::Const | TokenKind::Volatile | TokenKind::Static | TokenKind::Extern |
            TokenKind::Register | TokenKind::Typedef | TokenKind::Inline | TokenKind::Bool |
            TokenKind::Typeof | TokenKind::Attribute | TokenKind::Extension |
            TokenKind::Noreturn | TokenKind::Restrict | TokenKind::Complex |
            TokenKind::Atomic | TokenKind::Auto | TokenKind::Alignas |
            TokenKind::Builtin => true,  // __builtin_va_list is a type
            TokenKind::Identifier(name) => self.typedefs.contains(name) && !self.shadowed_typedefs.contains(name),
            _ => false,
        }
    }

    /// Skip const/volatile/restrict qualifiers.
    fn skip_cv_qualifiers(&mut self) {
        loop {
            match self.peek() {
                TokenKind::Const | TokenKind::Volatile | TokenKind::Restrict => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    /// Skip array bracket dimensions like [10][20].
    fn skip_array_dimensions(&mut self) {
        while matches!(self.peek(), TokenKind::LBracket) {
            self.advance();
            while !matches!(self.peek(), TokenKind::RBracket | TokenKind::Eof) {
                self.advance();
            }
            self.consume_if(&TokenKind::RBracket);
        }
    }

    /// Try to match a compound assignment operator token, returning the corresponding BinOp.
    fn compound_assign_op(&self) -> Option<BinOp> {
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

    // === Parsing ===

    fn parse_external_decl(&mut self) -> Option<ExternalDecl> {
        // Skip __extension__ and __attribute__
        self.skip_gcc_extensions();

        if self.at_eof() {
            return None;
        }

        // Handle top-level asm("..."); directives
        if matches!(self.peek(), TokenKind::Asm) {
            self.advance();
            // Skip optional volatile
            self.consume_if(&TokenKind::Volatile);
            if matches!(self.peek(), TokenKind::LParen) {
                self.skip_balanced_parens();
            }
            self.consume_if(&TokenKind::Semicolon);
            // Return an empty declaration
            return Some(ExternalDecl::Declaration(Declaration {
                type_spec: TypeSpecifier::Void,
                declarators: Vec::new(),
                is_static: false,
                is_extern: false,
                is_typedef: false,
                is_const: false,
                span: Span::dummy(),
            }));
        }

        // Reset storage class flags before each declaration
        self.parsing_typedef = false;
        self.parsing_static = false;
        self.parsing_extern = false;
        self.parsing_inline = false;
        self.parsing_const = false;

        // Try to parse a type + name, then determine if it's a function def or declaration
        let start = self.peek_span();
        let type_spec = self.parse_type_specifier()?;

        // Check for function definition or declaration
        if self.at_eof() || matches!(self.peek(), TokenKind::Semicolon) {
            // Just a type with no declarator (e.g., struct definition)
            self.consume_if(&TokenKind::Semicolon);
            return Some(ExternalDecl::Declaration(Declaration {
                type_spec,
                declarators: Vec::new(),
                is_static: self.parsing_static,
                is_extern: self.parsing_extern,
                is_typedef: self.parsing_typedef,
                is_const: self.parsing_const,
                span: start,
            }));
        }

        // Handle storage-class specifiers and alignment specifiers that appear after the type.
        // In C, "struct { int i; } typedef name;" and "char _Alignas(16) x;" are valid.
        loop {
            match self.peek() {
                TokenKind::Typedef => { self.advance(); self.parsing_typedef = true; }
                TokenKind::Static => { self.advance(); self.parsing_static = true; }
                TokenKind::Extern => { self.advance(); self.parsing_extern = true; }
                TokenKind::Const => { self.advance(); self.parsing_const = true; }
                TokenKind::Volatile | TokenKind::Restrict
                | TokenKind::Inline | TokenKind::Register | TokenKind::Auto => { self.advance(); }
                TokenKind::Alignas => {
                    self.advance();
                    if matches!(self.peek(), TokenKind::LParen) {
                        self.skip_balanced_parens();
                    }
                }
                TokenKind::Attribute => {
                    self.advance();
                    self.skip_balanced_parens();
                }
                TokenKind::Extension => { self.advance(); }
                _ => break,
            }
        }

        // Parse declarator(s)
        let (name, derived) = self.parse_declarator();

        // Skip GNU asm labels and attributes after declarator:
        // extern int foo(int) __asm__("renamed") __attribute__((noreturn));
        self.skip_asm_and_attributes();

        // Check if this is a function definition (has a body or K&R param decls)
        let is_funcdef = !derived.is_empty()
            && matches!(derived.last(), Some(DerivedDeclarator::Function(_, _)))
            && (matches!(self.peek(), TokenKind::LBrace) || self.is_type_specifier());

        if is_funcdef {
            self.parsing_typedef = false; // function defs are never typedefs
            let (params, variadic) = if let Some(DerivedDeclarator::Function(p, v)) = derived.last() {
                (p.clone(), *v)
            } else {
                (vec![], false)
            };

            // Handle K&R-style parameter declarations:
            // int foo(a, b) int a; int *b; { ... }
            let is_kr_style = !matches!(self.peek(), TokenKind::LBrace);
            let final_params = if is_kr_style {
                // K&R style: params only have names, types come in subsequent declarations
                let mut kr_params = params.clone();
                while self.is_type_specifier() && !matches!(self.peek(), TokenKind::LBrace) {
                    // Parse the K&R parameter type declaration
                    if let Some(type_spec) = self.parse_type_specifier() {
                        // Parse the declarator(s) for this type
                        loop {
                            let (pname, pderived) = self.parse_declarator();
                            if let Some(ref name) = pname {
                                // Apply derived declarators (pointers, arrays) to the type
                                let mut full_type = type_spec.clone();
                                // First apply pointers
                                for d in &pderived {
                                    if let DerivedDeclarator::Pointer = d {
                                        full_type = TypeSpecifier::Pointer(Box::new(full_type));
                                    }
                                }
                                // Collect array dimensions
                                let array_dims: Vec<_> = pderived.iter().filter_map(|d| {
                                    if let DerivedDeclarator::Array(size) = d {
                                        Some(size.clone())
                                    } else {
                                        None
                                    }
                                }).collect();
                                // Array params: outermost dimension decays to pointer,
                                // inner dimensions wrap as Array (same as non-K&R params).
                                if !array_dims.is_empty() {
                                    for dim in array_dims.iter().skip(1).rev() {
                                        full_type = TypeSpecifier::Array(Box::new(full_type), dim.clone());
                                    }
                                    full_type = TypeSpecifier::Pointer(Box::new(full_type));
                                }
                                // Function/FunctionPointer params decay to pointers
                                for d in &pderived {
                                    match d {
                                        DerivedDeclarator::Function(_, _) | DerivedDeclarator::FunctionPointer(_, _) => {
                                            full_type = TypeSpecifier::Pointer(Box::new(full_type));
                                        }
                                        _ => {}
                                    }
                                }
                                // Find the matching param and update its type
                                for param in kr_params.iter_mut() {
                                    if param.name.as_deref() == Some(name.as_str()) {
                                        param.type_spec = full_type.clone();
                                        break;
                                    }
                                }
                            }
                            if !self.consume_if(&TokenKind::Comma) {
                                break;
                            }
                        }
                        self.expect(&TokenKind::Semicolon);
                    } else {
                        break;
                    }
                }
                kr_params
            } else {
                params
            };

            let is_static = self.parsing_static;
            let is_inline = self.parsing_inline;
            // Wrap return type with any pointer derivations before the Function
            // e.g., for `char *func()`, derived = [Pointer, Function(...)]
            // so return_type should be Pointer(Char) not just Char
            let mut return_type = type_spec;
            for d in &derived {
                match d {
                    DerivedDeclarator::Pointer => {
                        return_type = TypeSpecifier::Pointer(Box::new(return_type));
                    }
                    DerivedDeclarator::Function(_, _) => break,
                    _ => {}
                }
            }
            // Shadow typedef names that are used as parameter names,
            // so the function body sees them as variables, not types.
            let saved_shadowed = self.shadowed_typedefs.clone();
            for param in &final_params {
                if let Some(ref pname) = param.name {
                    if self.typedefs.contains(pname) && !self.shadowed_typedefs.contains(pname) {
                        self.shadowed_typedefs.push(pname.clone());
                    }
                }
            }
            let body = self.parse_compound_stmt();
            self.shadowed_typedefs = saved_shadowed;
            Some(ExternalDecl::FunctionDef(FunctionDef {
                return_type,
                name: name.unwrap_or_default(),
                params: final_params,
                variadic,
                body,
                is_static,
                is_inline,
                is_kr: is_kr_style,
                span: start,
            }))
        } else {
            // Declaration
            let mut declarators = Vec::new();
            let init = if self.consume_if(&TokenKind::Assign) {
                Some(self.parse_initializer())
            } else {
                None
            };
            declarators.push(InitDeclarator {
                name: name.unwrap_or_default(),
                derived,
                init,
                span: start,
            });

            // Skip asm/attributes between declarators
            self.skip_asm_and_attributes();

            while self.consume_if(&TokenKind::Comma) {
                let (dname, dderived) = self.parse_declarator();
                self.skip_asm_and_attributes();
                let dinit = if self.consume_if(&TokenKind::Assign) {
                    Some(self.parse_initializer())
                } else {
                    None
                };
                declarators.push(InitDeclarator {
                    name: dname.unwrap_or_default(),
                    derived: dderived,
                    init: dinit,
                    span: start,
                });
                self.skip_asm_and_attributes();
            }

            // Register typedef names if this was a typedef declaration
            let is_typedef = self.parsing_typedef;
            if self.parsing_typedef {
                for decl in &declarators {
                    if !decl.name.is_empty() {
                        self.typedefs.push(decl.name.clone());
                    }
                }
                self.parsing_typedef = false;
            }

            self.expect(&TokenKind::Semicolon);
            Some(ExternalDecl::Declaration(Declaration {
                type_spec,
                declarators,
                is_static: self.parsing_static,
                is_extern: self.parsing_extern,
                is_typedef,
                is_const: self.parsing_const,
                span: start,
            }))
        }
    }

    fn skip_gcc_extensions(&mut self) {
        loop {
            match self.peek() {
                TokenKind::Extension => { self.advance(); }
                TokenKind::Attribute => {
                    self.advance();
                    // Skip __attribute__((..))
                    if matches!(self.peek(), TokenKind::LParen) {
                        self.skip_balanced_parens();
                    }
                }
                _ => break,
            }
        }
    }

    /// Skip __asm__("..."), __attribute__((...)), and __extension__ after declarators.
    /// GNU C allows: extern int foo(int) __asm__("bar") __attribute__((noreturn));
    fn skip_asm_and_attributes(&mut self) {
        loop {
            match self.peek() {
                TokenKind::Asm => {
                    self.advance();
                    // Skip optional volatile keyword
                    self.consume_if(&TokenKind::Volatile);
                    // Skip the parenthesized asm string
                    if matches!(self.peek(), TokenKind::LParen) {
                        self.skip_balanced_parens();
                    }
                }
                TokenKind::Attribute => {
                    self.advance();
                    if matches!(self.peek(), TokenKind::LParen) {
                        self.skip_balanced_parens();
                    }
                }
                TokenKind::Extension => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn skip_balanced_parens(&mut self) {
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

    /// Extract a name from arbitrarily nested parentheses: (name), ((name)), (((name))), etc.
    /// Also handles pointer prefix: (*(name)), (*((name))), etc.
    /// Current position should be at the opening '('.
    /// Consumes the parenthesized group and returns the name if found.
    fn extract_paren_name(&mut self) -> Option<String> {
        if !matches!(self.peek(), TokenKind::LParen) {
            // Not parenthesized - check for direct identifier
            if let TokenKind::Identifier(n) = self.peek().clone() {
                self.advance();
                return Some(n);
            }
            return None;
        }
        self.advance(); // consume '('
        // Skip '*' for pointer inside parens
        if matches!(self.peek(), TokenKind::Star) {
            self.advance();
            self.skip_cv_qualifiers();
        }
        // Recursively extract name from nested parens or get identifier
        let name = if matches!(self.peek(), TokenKind::LParen) {
            self.extract_paren_name()
        } else if let TokenKind::Identifier(n) = self.peek().clone() {
            self.advance();
            Some(n)
        } else {
            None
        };
        self.consume_if(&TokenKind::RParen);
        name
    }

    /// Try to parse a parenthesized abstract declarator that contains pointer(s).
    /// Handles patterns like: (*), ((*)), (*(*(*))), (**)
    /// Returns the total pointer depth if successful, None if not recognized.
    /// Consumes the tokens if successful.
    fn try_parse_paren_abstract_declarator(&mut self) -> Option<u32> {
        if !matches!(self.peek(), TokenKind::LParen) {
            return None;
        }
        let save = self.pos;
        self.advance(); // consume '('

        let mut total_ptrs = 0u32;

        // Count leading stars: (*...) or scan for nested
        while self.consume_if(&TokenKind::Star) {
            total_ptrs += 1;
            self.skip_cv_qualifiers();
        }

        // Check for nested parenthesized abstract declarator: (* (...))
        if matches!(self.peek(), TokenKind::LParen) {
            if let Some(inner_ptrs) = self.try_parse_paren_abstract_declarator() {
                total_ptrs += inner_ptrs;
            } else {
                // Not a valid nested pattern
                self.pos = save;
                return None;
            }
        }

        // Expect closing paren
        if self.consume_if(&TokenKind::RParen) {
            if total_ptrs > 0 {
                Some(total_ptrs)
            } else {
                // Empty parens () with no pointers - not an abstract declarator
                self.pos = save;
                None
            }
        } else {
            self.pos = save;
            None
        }
    }

    #[allow(unused_assignments)]
    fn parse_type_specifier(&mut self) -> Option<TypeSpecifier> {
        self.skip_gcc_extensions();

        // Collect all type specifier tokens in any order.
        // C allows type specifiers in any order: "long unsigned int" == "unsigned long int" == "int unsigned long"
        // We track what we've seen with flags and resolve at the end.
        let mut has_signed = false;
        let mut has_unsigned = false;
        let mut has_short = false;
        let mut long_count: u32 = 0;
        let mut has_int = false;
        let mut has_char = false;
        let mut has_void = false;
        let mut has_float = false;
        let mut has_double = false;
        let mut has_bool = false;
        let mut has_complex = false;
        let mut has_struct = false;
        let mut has_union = false;
        let mut has_enum = false;
        let mut has_typeof = false;
        let mut typedef_name: Option<String> = None;
        let mut any_base_specifier = false;

        // Collect qualifiers, storage classes, and type specifiers in a loop
        loop {
            match self.peek().clone() {
                // Qualifiers
                TokenKind::Const => {
                    self.advance();
                    self.parsing_const = true;
                }
                TokenKind::Volatile | TokenKind::Restrict
                | TokenKind::Register | TokenKind::Noreturn
                | TokenKind::Auto => {
                    self.advance();
                }
                TokenKind::Inline => {
                    self.advance();
                    self.parsing_inline = true;
                }
                // Storage classes
                TokenKind::Static => {
                    self.advance();
                    self.parsing_static = true;
                }
                TokenKind::Extern => {
                    self.advance();
                    self.parsing_extern = true;
                }
                TokenKind::Typedef => {
                    self.advance();
                    self.parsing_typedef = true;
                }
                // _Complex modifier
                TokenKind::Complex => {
                    self.advance();
                    has_complex = true;
                    any_base_specifier = true;
                }
                // GNU extensions
                TokenKind::Attribute => {
                    self.advance();
                    self.skip_balanced_parens();
                }
                TokenKind::Extension => {
                    self.advance();
                }
                // _Atomic
                TokenKind::Atomic => {
                    self.advance();
                    // _Atomic can be followed by (type) for _Atomic(int)
                    if matches!(self.peek(), TokenKind::LParen) {
                        self.skip_balanced_parens();
                        return Some(TypeSpecifier::Int); // TODO: parse the actual type
                    }
                }
                // Alignas
                TokenKind::Alignas => {
                    self.advance();
                    if matches!(self.peek(), TokenKind::LParen) {
                        self.skip_balanced_parens();
                    }
                }
                // Type specifier tokens - collected in any order
                TokenKind::Void => {
                    self.advance();
                    has_void = true;
                    any_base_specifier = true;
                    break; // void can't combine with others
                }
                TokenKind::Char => {
                    self.advance();
                    has_char = true;
                    any_base_specifier = true;
                    break; // char only combines with signed/unsigned
                }
                TokenKind::Short => {
                    self.advance();
                    has_short = true;
                    any_base_specifier = true;
                }
                TokenKind::Int => {
                    self.advance();
                    has_int = true;
                    any_base_specifier = true;
                }
                TokenKind::Long => {
                    self.advance();
                    long_count += 1;
                    any_base_specifier = true;
                }
                TokenKind::Float => {
                    self.advance();
                    has_float = true;
                    any_base_specifier = true;
                    break; // float can't combine with int/long
                }
                TokenKind::Double => {
                    self.advance();
                    has_double = true;
                    any_base_specifier = true;
                    break; // double only combines with long
                }
                TokenKind::Bool => {
                    self.advance();
                    has_bool = true;
                    any_base_specifier = true;
                    break; // _Bool can't combine with others
                }
                TokenKind::Signed => {
                    self.advance();
                    has_signed = true;
                    any_base_specifier = true;
                }
                TokenKind::Unsigned => {
                    self.advance();
                    has_unsigned = true;
                    any_base_specifier = true;
                }
                TokenKind::Struct => {
                    self.advance();
                    has_struct = true;
                    any_base_specifier = true;
                    break; // struct starts a compound type
                }
                TokenKind::Union => {
                    self.advance();
                    has_union = true;
                    any_base_specifier = true;
                    break; // union starts a compound type
                }
                TokenKind::Enum => {
                    self.advance();
                    has_enum = true;
                    any_base_specifier = true;
                    break; // enum starts a compound type
                }
                TokenKind::Typeof => {
                    self.advance();
                    has_typeof = true;
                    any_base_specifier = true;
                    break;
                }
                TokenKind::Builtin => {
                    // __builtin_va_list used as type name
                    if !any_base_specifier {
                        typedef_name = Some("__builtin_va_list".to_string());
                        self.advance();
                        any_base_specifier = true;
                        break;
                    } else {
                        break;
                    }
                }
                TokenKind::Identifier(ref name) if self.typedefs.contains(name) && !self.shadowed_typedefs.contains(name) => {
                    // Only consume typedef name if we haven't seen any other base specifier
                    if !any_base_specifier {
                        typedef_name = Some(name.clone());
                        self.advance();
                        any_base_specifier = true;
                        break;
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }

        // After breaking from the loop (for char/void/float/double/bool/struct/union/enum/typeof),
        // continue collecting additional specifiers that can follow
        if has_char || has_short || has_int || long_count > 0 {
            // Collect any remaining signed/unsigned/int/long/short that might follow
            loop {
                match self.peek() {
                    TokenKind::Signed => {
                        self.advance();
                        has_signed = true;
                    }
                    TokenKind::Unsigned => {
                        self.advance();
                        has_unsigned = true;
                    }
                    TokenKind::Int => {
                        self.advance();
                        has_int = true;
                    }
                    TokenKind::Long => {
                        self.advance();
                        long_count += 1;
                    }
                    TokenKind::Short => {
                        self.advance();
                        has_short = true;
                    }
                    TokenKind::Char => {
                        self.advance();
                        has_char = true;
                    }
                    TokenKind::Complex => {
                        self.advance();
                        has_complex = true;
                    }
                    TokenKind::Const | TokenKind::Volatile | TokenKind::Restrict => {
                        self.advance();
                    }
                    TokenKind::Static => {
                        self.advance();
                        self.parsing_static = true;
                    }
                    TokenKind::Extern => {
                        self.advance();
                        self.parsing_extern = true;
                    }
                    TokenKind::Register | TokenKind::Noreturn => {
                        self.advance();
                    }
                    TokenKind::Inline => {
                        self.advance();
                        self.parsing_inline = true;
                    }
                    TokenKind::Attribute => {
                        self.advance();
                        self.skip_balanced_parens();
                    }
                    TokenKind::Extension => {
                        self.advance();
                    }
                    _ => break,
                }
            }
        } else if has_double {
            // "double" can be preceded/followed by "long"
            loop {
                match self.peek() {
                    TokenKind::Long => {
                        self.advance();
                        long_count += 1;
                    }
                    TokenKind::Complex => {
                        self.advance();
                        has_complex = true;
                    }
                    TokenKind::Const | TokenKind::Volatile | TokenKind::Restrict => {
                        self.advance();
                    }
                    _ => break,
                }
            }
        }

        if !any_base_specifier {
            return None;
        }

        // Now resolve the collected specifiers into a TypeSpecifier
        let base = if has_void {
            TypeSpecifier::Void
        } else if has_bool {
            TypeSpecifier::Bool
        } else if has_float {
            TypeSpecifier::Float
        } else if has_double {
            if long_count > 0 {
                TypeSpecifier::LongDouble
            } else {
                TypeSpecifier::Double
            }
        } else if has_struct {
            self.skip_gcc_extensions();
            let name = if let TokenKind::Identifier(n) = self.peek().clone() {
                self.advance();
                Some(n)
            } else {
                None
            };
            let fields = if matches!(self.peek(), TokenKind::LBrace) {
                Some(self.parse_struct_fields())
            } else {
                None
            };
            TypeSpecifier::Struct(name, fields)
        } else if has_union {
            self.skip_gcc_extensions();
            let name = if let TokenKind::Identifier(n) = self.peek().clone() {
                self.advance();
                Some(n)
            } else {
                None
            };
            let fields = if matches!(self.peek(), TokenKind::LBrace) {
                Some(self.parse_struct_fields())
            } else {
                None
            };
            TypeSpecifier::Union(name, fields)
        } else if has_enum {
            self.skip_gcc_extensions();
            let name = if let TokenKind::Identifier(n) = self.peek().clone() {
                self.advance();
                Some(n)
            } else {
                None
            };
            let variants = if matches!(self.peek(), TokenKind::LBrace) {
                Some(self.parse_enum_variants())
            } else {
                None
            };
            TypeSpecifier::Enum(name, variants)
        } else if has_typeof {
            // TODO: proper typeof support
            self.skip_balanced_parens();
            TypeSpecifier::Int
        } else if let Some(name) = typedef_name {
            TypeSpecifier::TypedefName(name)
        } else if has_char {
            if has_unsigned {
                TypeSpecifier::UnsignedChar
            } else {
                TypeSpecifier::Char // signed char = char
            }
        } else if has_short {
            if has_unsigned {
                TypeSpecifier::UnsignedShort
            } else {
                TypeSpecifier::Short
            }
        } else if long_count >= 2 {
            if has_unsigned {
                TypeSpecifier::UnsignedLongLong
            } else {
                TypeSpecifier::LongLong
            }
        } else if long_count == 1 {
            if has_unsigned {
                TypeSpecifier::UnsignedLong
            } else {
                TypeSpecifier::Long
            }
        } else if has_unsigned {
            TypeSpecifier::UnsignedInt
        } else {
            // signed, int, or signed int all resolve to Int
            TypeSpecifier::Int
        };

        // Skip any trailing type qualifiers, storage class specifiers, and attributes.
        // C allows "int static x;" (storage class after type).
        loop {
            match self.peek() {
                TokenKind::Complex => {
                    // _Complex after base type (e.g., "double _Complex")
                    // TODO: properly support _Complex types
                    self.advance();
                }
                TokenKind::Const | TokenKind::Volatile | TokenKind::Restrict => {
                    self.advance();
                }
                TokenKind::Static => {
                    self.advance();
                    self.parsing_static = true;
                }
                TokenKind::Extern => {
                    self.advance();
                    self.parsing_extern = true;
                }
                TokenKind::Register | TokenKind::Noreturn => {
                    self.advance();
                }
                TokenKind::Inline => {
                    self.advance();
                    self.parsing_inline = true;
                }
                TokenKind::Attribute => {
                    self.advance();
                    self.skip_balanced_parens();
                }
                TokenKind::Extension => {
                    self.advance();
                }
                _ => break,
            }
        }

        Some(base)
    }

    fn parse_struct_fields(&mut self) -> Vec<StructFieldDecl> {
        let mut fields = Vec::new();
        self.expect(&TokenKind::LBrace);
        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            self.skip_gcc_extensions();
            if let Some(type_spec) = self.parse_type_specifier() {
                // Parse field declarators
                if matches!(self.peek(), TokenKind::Semicolon) {
                    // Anonymous field
                    fields.push(StructFieldDecl { type_spec, name: None, bit_width: None });
                } else {
                    loop {
                        // Check for function pointer field or parenthesized name
                        if matches!(self.peek(), TokenKind::LParen) {
                            let save = self.pos;
                            self.advance(); // consume '('
                            if matches!(self.peek(), TokenKind::Star) {
                                self.advance(); // consume '*'
                                self.skip_cv_qualifiers();
                                let name = if let TokenKind::Identifier(n) = self.peek().clone() {
                                    self.advance();
                                    Some(n)
                                } else {
                                    None
                                };
                                self.skip_array_dimensions();
                                self.expect(&TokenKind::RParen);
                                // Skip the parameter list
                                self.skip_balanced_parens();
                                // Function pointer is treated as a pointer type
                                let field_type = TypeSpecifier::Pointer(Box::new(type_spec.clone()));
                                fields.push(StructFieldDecl { type_spec: field_type, name, bit_width: None });
                                if !self.consume_if(&TokenKind::Comma) {
                                    break;
                                }
                                continue;
                            } else if let TokenKind::Identifier(_) = self.peek() {
                                // Parenthesized field name: int (name) or int (name):N
                                let name = if let TokenKind::Identifier(n) = self.peek().clone() {
                                    self.advance();
                                    Some(n)
                                } else {
                                    None
                                };
                                self.expect(&TokenKind::RParen);
                                let field_type = type_spec.clone();
                                // Parse array dimensions after parenthesized name
                                let mut ft = field_type;
                                let mut array_dims: Vec<Option<Box<Expr>>> = Vec::new();
                                while matches!(self.peek(), TokenKind::LBracket) {
                                    self.advance();
                                    let size = if matches!(self.peek(), TokenKind::RBracket) {
                                        None
                                    } else {
                                        Some(Box::new(self.parse_expr()))
                                    };
                                    self.expect(&TokenKind::RBracket);
                                    array_dims.push(size);
                                }
                                for dim in array_dims.into_iter().rev() {
                                    ft = TypeSpecifier::Array(Box::new(ft), dim);
                                }
                                // Bit field
                                let bit_width = if self.consume_if(&TokenKind::Colon) {
                                    Some(Box::new(self.parse_expr()))
                                } else {
                                    None
                                };
                                fields.push(StructFieldDecl { type_spec: ft, name, bit_width });
                                if !self.consume_if(&TokenKind::Comma) {
                                    break;
                                }
                                continue;
                            } else if matches!(self.peek(), TokenKind::LParen) {
                                // Nested parenthesized name: ((name))
                                let name = self.extract_paren_name();
                                self.expect(&TokenKind::RParen);
                                let field_type = type_spec.clone();
                                let bit_width = if self.consume_if(&TokenKind::Colon) {
                                    Some(Box::new(self.parse_expr()))
                                } else {
                                    None
                                };
                                fields.push(StructFieldDecl { type_spec: field_type, name, bit_width });
                                if !self.consume_if(&TokenKind::Comma) {
                                    break;
                                }
                                continue;
                            } else {
                                self.pos = save; // restore, not a recognized pattern
                            }
                        }
                        // Parse pointer declarators: wrap type_spec for each *
                        let mut field_type = type_spec.clone();
                        while self.consume_if(&TokenKind::Star) {
                            field_type = TypeSpecifier::Pointer(Box::new(field_type));
                        }
                        let name = if let TokenKind::Identifier(n) = self.peek().clone() {
                            self.advance();
                            Some(n)
                        } else {
                            None
                        };
                        // Parse array dimensions: collect all dims first, then wrap
                        // in reverse order so that for T field[N][M], the innermost
                        // dimension (M) wraps the base type first, producing
                        // Array(Array(T, M), N) which correctly represents
                        // "array of N arrays of M elements of type T".
                        let mut array_dims: Vec<Option<Box<Expr>>> = Vec::new();
                        while matches!(self.peek(), TokenKind::LBracket) {
                            self.advance(); // consume '['
                            let size = if matches!(self.peek(), TokenKind::RBracket) {
                                None
                            } else {
                                Some(Box::new(self.parse_expr()))
                            };
                            self.expect(&TokenKind::RBracket);
                            array_dims.push(size);
                        }
                        // Apply dimensions in reverse: innermost (rightmost) first
                        for dim in array_dims.into_iter().rev() {
                            field_type = TypeSpecifier::Array(Box::new(field_type), dim);
                        }
                        // Bit field
                        let bit_width = if self.consume_if(&TokenKind::Colon) {
                            Some(Box::new(self.parse_expr()))
                        } else {
                            None
                        };
                        fields.push(StructFieldDecl { type_spec: field_type, name, bit_width });
                        if !self.consume_if(&TokenKind::Comma) {
                            break;
                        }
                    }
                }
                self.skip_gcc_extensions();
                self.expect(&TokenKind::Semicolon);
            } else {
                self.advance(); // skip unknown
            }
        }
        self.expect(&TokenKind::RBrace);
        fields
    }

    fn parse_enum_variants(&mut self) -> Vec<EnumVariant> {
        let mut variants = Vec::new();
        self.expect(&TokenKind::LBrace);
        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            if let TokenKind::Identifier(name) = self.peek().clone() {
                self.advance();
                let value = if self.consume_if(&TokenKind::Assign) {
                    // Use assignment_expr (not full expr) to avoid consuming commas
                    // between enum values as the comma operator
                    Some(Box::new(self.parse_assignment_expr()))
                } else {
                    None
                };
                variants.push(EnumVariant { name, value });
                self.consume_if(&TokenKind::Comma);
            } else {
                self.advance();
            }
        }
        self.expect(&TokenKind::RBrace);
        variants
    }

    fn parse_declarator(&mut self) -> (Option<String>, Vec<DerivedDeclarator>) {
        let mut derived = Vec::new();

        // Skip attributes
        self.skip_gcc_extensions();

        // Parse pointer(s)
        // GCC allows __attribute__ after each pointer and its qualifiers:
        //   int * __attribute__((noinline)) foo(...)
        //   const char * __attribute__((noinline)) bar(...)
        while self.consume_if(&TokenKind::Star) {
            derived.push(DerivedDeclarator::Pointer);
            self.skip_cv_qualifiers();
            self.skip_gcc_extensions();
        }

        // Parse the direct-declarator part (name, parenthesized declarator, or abstract)
        //
        // C grammar:
        //   direct-declarator:
        //     identifier
        //     '(' declarator ')'            <-- recursive!
        //     direct-declarator '[' ... ']'
        //     direct-declarator '(' ... ')'
        //
        // When we see '(', we must disambiguate:
        //   - Parenthesized declarator: starts with '*', '^', '(', or non-type identifier
        //   - Parameter list (abstract declarator suffix): starts with type keyword/typedef, ')', '...'
        let (name, inner_derived) = if let TokenKind::Identifier(n) = self.peek().clone() {
            self.advance();
            (Some(n), Vec::new())
        } else if matches!(self.peek(), TokenKind::LParen) && self.is_paren_declarator() {
            // Parenthesized declarator: ( declarator )
            // Recursively parse the inner declarator
            let save = self.pos;
            self.advance(); // consume '('

            let (inner_name, inner_derived) = self.parse_declarator();
            if !self.consume_if(&TokenKind::RParen) {
                // If we didn't find a closing paren, this wasn't a parenthesized declarator.
                // Restore position and treat as abstract (no name).
                self.pos = save;
                (None, Vec::new())
            } else {
                (inner_name, inner_derived)
            }
        } else {
            (None, Vec::new())
        };

        // Parse outer suffixes: array dimensions and function params
        // These apply OUTSIDE the parenthesized declarator
        let mut outer_suffixes = Vec::new();
        loop {
            match self.peek() {
                TokenKind::LBracket => {
                    self.advance();
                    // Handle 'static' in array params: int a[static 10]
                    self.consume_if(&TokenKind::Static);
                    let size = if matches!(self.peek(), TokenKind::RBracket) {
                        None
                    } else {
                        Some(Box::new(self.parse_expr()))
                    };
                    self.expect(&TokenKind::RBracket);
                    outer_suffixes.push(DerivedDeclarator::Array(size));
                }
                TokenKind::LParen => {
                    let (params, variadic) = self.parse_param_list();
                    outer_suffixes.push(DerivedDeclarator::Function(params, variadic));
                }
                _ => break,
            }
        }

        // Combine: the inner derived modifiers (from inside parens) come first,
        // then the outer suffixes. This is because in C, the inner modifiers bind
        // more tightly to the name.
        //
        // Example: int (*fp)(int)
        //   - derived (before paren): []
        //   - inner_derived: [Pointer] (from the * inside parens)
        //   - outer_suffixes: [Function([int])] (from the (int) after the paren group)
        //   - Result: [Pointer, Function([int])]
        //   - Combined with base type int: fp is Pointer(Function(int)->int)
        //
        // For function pointers, we need to convert Function to FunctionPointer
        // when the inner derived contains a Pointer as its last element.
        let combined = self.combine_declarator_parts(derived, inner_derived, outer_suffixes);

        // Skip trailing attributes
        self.skip_gcc_extensions();

        (name, combined)
    }

    /// Determine if a '(' token at the current position starts a parenthesized declarator
    /// (as opposed to a function parameter list for an abstract declarator).
    ///
    /// Heuristic: after '(' we check what follows:
    /// - '*', '^': definitely a pointer declarator like (*fp)
    /// - '(': nested parenthesized declarator like ((x))
    /// - identifier that is NOT a typedef: likely a parenthesized name like (name)
    /// - type keyword, typedef name, ')', '...': likely a parameter list
    fn is_paren_declarator(&self) -> bool {
        if self.pos + 1 >= self.tokens.len() {
            return false;
        }
        match &self.tokens[self.pos + 1].kind {
            // Definitely a parenthesized declarator
            TokenKind::Star | TokenKind::Caret => true,
            // Nested parenthesized declarator
            TokenKind::LParen => true,
            // GCC attribute inside declarator
            TokenKind::Attribute | TokenKind::Extension => true,
            // Identifier: check if it's a typedef name (= param list) or regular name (= declarator)
            TokenKind::Identifier(name) => {
                // If it's a known typedef, it starts a parameter list
                if self.typedefs.contains(name) && !self.shadowed_typedefs.contains(name) {
                    false
                } else {
                    // Non-typedef identifier => parenthesized declarator name
                    true
                }
            }
            // Empty parens () or '...' or type keywords => parameter list
            TokenKind::RParen | TokenKind::Ellipsis => false,
            // Type specifier keywords => parameter list
            TokenKind::Void | TokenKind::Char | TokenKind::Short | TokenKind::Int |
            TokenKind::Long | TokenKind::Float | TokenKind::Double | TokenKind::Signed |
            TokenKind::Unsigned | TokenKind::Struct | TokenKind::Union | TokenKind::Enum |
            TokenKind::Const | TokenKind::Volatile | TokenKind::Static | TokenKind::Extern |
            TokenKind::Register | TokenKind::Typedef | TokenKind::Inline | TokenKind::Bool |
            TokenKind::Typeof | TokenKind::Noreturn | TokenKind::Restrict | TokenKind::Complex |
            TokenKind::Atomic | TokenKind::Auto | TokenKind::Alignas |
            TokenKind::Builtin => false,
            // Anything else: assume not a declarator
            _ => false,
        }
    }

    /// Combine the parts of a declarator parsed recursively.
    ///
    /// C declarators follow the "inside-out" rule:
    /// - `outer_pointers`: pointer modifiers parsed before the parenthesized group
    /// - `inner_derived`: modifiers from inside the parenthesized group (from recursive parse_declarator)
    /// - `outer_suffixes`: array/function suffixes parsed after the parenthesized group
    ///
    /// The flat DerivedDeclarator list is processed left-to-right, where each entry
    /// wraps the accumulated type. The correct order for the inside-out rule is:
    ///   outer_pointers ++ outer_suffixes ++ inner_derived
    ///
    /// This ensures outer suffixes modify the base type first, then inner modifiers
    /// wrap the result (binding more tightly to the name).
    ///
    /// Special case: for function pointers ((*name)(params)), we convert to the
    /// existing [Pointer, FunctionPointer] convention used by the rest of the codebase.
    fn combine_declarator_parts(
        &self,
        mut outer_pointers: Vec<DerivedDeclarator>,
        inner_derived: Vec<DerivedDeclarator>,
        outer_suffixes: Vec<DerivedDeclarator>,
    ) -> Vec<DerivedDeclarator> {
        if inner_derived.is_empty() && outer_suffixes.is_empty() {
            // Simple case: no parenthesized declarator, just pointers
            return outer_pointers;
        }

        if inner_derived.is_empty() {
            // No inner declarator (e.g., just extra parens like `int (x)` → treat as plain)
            outer_pointers.extend(outer_suffixes);
            return outer_pointers;
        }

        // Check if we have a simple function pointer pattern:
        // inner_derived is only Pointer(s) and optional Array(s), outer_suffixes starts with Function.
        // Convert to the [Array..., Pointer, FunctionPointer] convention.
        let inner_only_ptr_and_array = inner_derived.iter().all(|d|
            matches!(d, DerivedDeclarator::Pointer | DerivedDeclarator::Array(_)));
        let inner_has_pointer = inner_derived.iter().any(|d| matches!(d, DerivedDeclarator::Pointer));
        let outer_starts_with_function = matches!(outer_suffixes.first(), Some(DerivedDeclarator::Function(_, _)));

        if inner_only_ptr_and_array && inner_has_pointer && outer_starts_with_function
            && outer_suffixes.len() == 1
        {
            // Simple function pointer: (*name)(params) or (*name[N])(params)
            let mut result = outer_pointers;
            // Emit inner arrays first (for array of function pointers)
            for d in &inner_derived {
                if matches!(d, DerivedDeclarator::Array(_)) {
                    result.push(d.clone());
                }
            }
            // Emit pointer(s)
            for d in &inner_derived {
                if matches!(d, DerivedDeclarator::Pointer) {
                    result.push(DerivedDeclarator::Pointer);
                }
            }
            // Convert Function to FunctionPointer
            if let Some(DerivedDeclarator::Function(params, variadic)) = outer_suffixes.into_iter().next() {
                result.push(DerivedDeclarator::FunctionPointer(params, variadic));
            }
            return result;
        }

        // Check if we have simple pointer-to-array: (*name)[N]
        // inner_derived is only Pointer(s), outer_suffixes is only Array(s)
        let outer_only_arrays = outer_suffixes.iter().all(|d| matches!(d, DerivedDeclarator::Array(_)));
        if inner_only_ptr_and_array && inner_has_pointer && outer_only_arrays
            && !inner_derived.iter().any(|d| matches!(d, DerivedDeclarator::Array(_)))
        {
            // Pointer to array: (*name)[N]
            // Convention: Array dims first, then Pointer
            let mut result = outer_pointers;
            result.extend(outer_suffixes);
            for d in inner_derived {
                if matches!(d, DerivedDeclarator::Pointer) {
                    result.push(d);
                }
            }
            return result;
        }

        // General case: outer_pointers ++ outer_suffixes ++ inner_derived
        // This implements the inside-out rule: outer suffixes modify the base type,
        // then inner modifiers wrap the result (closer to the name = applied later).
        outer_pointers.extend(outer_suffixes);
        outer_pointers.extend(inner_derived);
        outer_pointers
    }

    fn parse_param_list(&mut self) -> (Vec<ParamDecl>, bool) {
        self.expect(&TokenKind::LParen);
        let mut params = Vec::new();
        let mut variadic = false;

        if matches!(self.peek(), TokenKind::RParen) {
            self.advance();
            return (params, variadic);
        }

        // Handle (void)
        if matches!(self.peek(), TokenKind::Void) {
            let save = self.pos;
            self.advance();
            if matches!(self.peek(), TokenKind::RParen) {
                self.advance();
                return (params, variadic);
            }
            self.pos = save;
        }

        // Check if this is a K&R-style identifier list: foo(a, b, c)
        // Identifiers that are NOT type names appear directly
        if let TokenKind::Identifier(ref name) = self.peek() {
            if (!self.typedefs.contains(name) || self.shadowed_typedefs.contains(name)) && !self.is_type_specifier() {
                // K&R-style identifier list
                loop {
                    if let TokenKind::Identifier(n) = self.peek().clone() {
                        let span = self.peek_span();
                        self.advance();
                        params.push(ParamDecl {
                            type_spec: TypeSpecifier::Int, // K&R default type
                            name: Some(n),
                            span,
                            fptr_params: None,
                        });
                    } else {
                        break;
                    }
                    if !self.consume_if(&TokenKind::Comma) {
                        break;
                    }
                }
                self.expect(&TokenKind::RParen);
                return (params, variadic);
            }
        }

        loop {
            if matches!(self.peek(), TokenKind::Ellipsis) {
                self.advance();
                variadic = true;
                break;
            }

            let span = self.peek_span();
            self.skip_gcc_extensions();
            if let Some(mut type_spec) = self.parse_type_specifier() {
                // Parse parameter declarator (handles pointers, function pointers, arrays)
                let (name, pointer_depth, array_dims, is_func_ptr, ptr_to_array_dims, fptr_param_decls) = self.parse_param_declarator_full();
                self.skip_gcc_extensions();

                // Wrap type with pointer levels from declarator
                // e.g., `int *p` -> base=Int, pointer_depth=1 -> Pointer(Int)
                for _ in 0..pointer_depth {
                    type_spec = TypeSpecifier::Pointer(Box::new(type_spec));
                }

                // Pointer-to-array parameters: int (*p)[N][M]
                // Build Array wrapping from innermost, then wrap in Pointer
                // e.g., int (*p)[3] -> Pointer(Array(Int, 3))
                // e.g., int (*p)[2][3] -> Pointer(Array(Array(Int, 3), 2))
                if !ptr_to_array_dims.is_empty() {
                    for dim in ptr_to_array_dims.iter().rev() {
                        type_spec = TypeSpecifier::Array(Box::new(type_spec), dim.clone());
                    }
                    type_spec = TypeSpecifier::Pointer(Box::new(type_spec));
                }

                // Array parameters: outermost dimension decays to pointer,
                // inner dimensions wrap as Array.
                // e.g., `int arr[3][4]` -> Pointer(Array(Int, 4))
                // e.g., `int arr[][4]` -> Pointer(Array(Int, 4))
                // e.g., `int arr[3]` -> Pointer(Int)
                if !array_dims.is_empty() {
                    // Wrap inner dimensions (from innermost to outermost, skip first)
                    for dim in array_dims.iter().skip(1).rev() {
                        type_spec = TypeSpecifier::Array(Box::new(type_spec), dim.clone());
                    }
                    // Outermost dimension decays to pointer
                    type_spec = TypeSpecifier::Pointer(Box::new(type_spec));
                }

                // Function pointers: treat as Pointer (e.g., `void (*fp)(int)` -> Ptr)
                if is_func_ptr {
                    type_spec = TypeSpecifier::Pointer(Box::new(type_spec));
                }

                params.push(ParamDecl { type_spec, name, span, fptr_params: fptr_param_decls });
            } else {
                break;
            }

            if !self.consume_if(&TokenKind::Comma) {
                break;
            }
        }

        self.expect(&TokenKind::RParen);
        (params, variadic)
    }

    /// Parse a parameter declarator, handling:
    /// - Simple: `int x`
    /// - Pointer: `int *x`
    /// - Function pointer: `void (*fn)(int, int)`
    /// - Array: `int arr[10]`
    /// - Abstract (unnamed): `int *`, `void (*)(int)`
    /// - Parenthesized: `int (x)`
    /// Returns the name if one was found.
    /// Parse a parameter declarator. Returns (name, pointer_depth, array_dims, is_func_ptr, ptr_to_array_dims).
    /// array_dims: list of array dimension sizes (outermost first). Empty = not an array.
    /// The outermost dimension decays to pointer; inner dimensions wrap the type as Array.
    fn parse_param_declarator_full(&mut self) -> (Option<String>, u32, Vec<Option<Box<Expr>>>, bool, Vec<Option<Box<Expr>>>, Option<Vec<ParamDecl>>) {
        // Parse leading pointer(s)
        let mut pointer_depth: u32 = 0;
        while self.consume_if(&TokenKind::Star) {
            pointer_depth += 1;
            self.skip_cv_qualifiers();
        }
        let mut array_dims: Vec<Option<Box<Expr>>> = Vec::new();
        let mut is_func_ptr = false;
        let mut ptr_to_array_dims: Vec<Option<Box<Expr>>> = Vec::new();
        let mut fptr_params: Option<Vec<ParamDecl>> = None;

        // Check for parenthesized declarator: (*name) or (*)
        let name = if matches!(self.peek(), TokenKind::LParen) {
            // Could be:
            // 1. Function pointer: (*name)(params) or (*)(params)
            // 2. Parenthesized name: (name)
            // 3. An abstract function type
            let save = self.pos;
            self.advance(); // consume '('

            if matches!(self.peek(), TokenKind::Star) {
                // (*name), (**name), etc. pattern: could be function pointer (*name)(params)
                // or pointer-to-array (*name)[N]
                // Count all pointer levels inside the parens
                let mut inner_ptr_depth = 0u32;
                while self.consume_if(&TokenKind::Star) {
                    inner_ptr_depth += 1;
                    self.skip_cv_qualifiers();
                }
                let name = if let TokenKind::Identifier(n) = self.peek().clone() {
                    self.advance();
                    Some(n)
                } else if matches!(self.peek(), TokenKind::LParen) {
                    // Nested parenthesized name: (*(name)) or (*((name)))
                    self.extract_paren_name()
                } else {
                    None
                };
                // Add extra pointer levels (first * is handled below via is_func_ptr or pointer_depth)
                pointer_depth += inner_ptr_depth.saturating_sub(1);
                // Skip array dimensions inside parens (e.g., (*name[N]) for array of func ptrs)
                self.skip_array_dimensions();
                self.expect(&TokenKind::RParen);
                if matches!(self.peek(), TokenKind::LParen) {
                    // Function pointer: (*name)(params) - parse param list
                    is_func_ptr = true;
                    let (fp_params, _variadic) = self.parse_param_list();
                    fptr_params = Some(fp_params);
                } else if matches!(self.peek(), TokenKind::LBracket) {
                    // Pointer-to-array: (*p)[N][M]...
                    // Collect dims into ptr_to_array_dims (NOT regular array_dims)
                    while matches!(self.peek(), TokenKind::LBracket) {
                        self.advance(); // consume '['
                        if matches!(self.peek(), TokenKind::RBracket) {
                            ptr_to_array_dims.push(None);
                            self.advance();
                        } else {
                            let dim_expr = self.parse_expr();
                            ptr_to_array_dims.push(Some(Box::new(dim_expr)));
                            self.expect(&TokenKind::RBracket);
                        }
                    }
                } else {
                    // Just (*name) - plain pointer
                    pointer_depth += 1;
                }
                name
            } else if self.consume_if(&TokenKind::Caret) {
                // Block pointer (Apple extension): (^name) or (^)
                let name = if let TokenKind::Identifier(n) = self.peek().clone() {
                    self.advance();
                    Some(n)
                } else {
                    None
                };
                self.expect(&TokenKind::RParen);
                if matches!(self.peek(), TokenKind::LParen) {
                    self.skip_balanced_parens();
                }
                name
            } else if let TokenKind::Identifier(_) = self.peek() {
                // Parenthesized name: (name)
                let name = if let TokenKind::Identifier(n) = self.peek().clone() {
                    self.advance();
                    Some(n)
                } else {
                    None
                };
                // Handle additional nested parens: ((name))
                // Consume all closing parens that match the opening ones
                self.expect(&TokenKind::RParen);
                // Might be followed by array dimensions or param list
                self.skip_array_dimensions();
                if matches!(self.peek(), TokenKind::LParen) {
                    self.skip_balanced_parens();
                }
                name
            } else if matches!(self.peek(), TokenKind::LParen) {
                // Nested parenthesized declarator: ((name)) or ((*name))
                // Recursively extract the name from nested parens
                let name = self.extract_paren_name();
                self.expect(&TokenKind::RParen);
                // Might be followed by array dimensions or param list
                self.skip_array_dimensions();
                if matches!(self.peek(), TokenKind::LParen) {
                    self.skip_balanced_parens();
                }
                name
            } else {
                // Not a recognized pattern, restore position
                self.pos = save;
                None
            }
        } else if let TokenKind::Identifier(n) = self.peek().clone() {
            // Simple name
            self.advance();
            Some(n)
        } else {
            None
        };

        // Parse array dimensions (preserving inner dimensions for multi-dim arrays)
        while matches!(self.peek(), TokenKind::LBracket) {
            self.advance(); // consume '['
            if matches!(self.peek(), TokenKind::RBracket) {
                // Empty brackets: arr[]
                array_dims.push(None);
                self.advance();
            } else {
                // Parse the dimension expression
                let dim_expr = self.parse_expr();
                array_dims.push(Some(Box::new(dim_expr)));
                self.expect(&TokenKind::RBracket);
            }
        }

        // Skip trailing function param list (for function pointer types without name)
        if matches!(self.peek(), TokenKind::LParen) {
            self.skip_balanced_parens();
        }

        (name, pointer_depth, array_dims, is_func_ptr, ptr_to_array_dims, fptr_params)
    }

    fn parse_compound_stmt(&mut self) -> CompoundStmt {
        let start = self.peek_span();
        self.expect(&TokenKind::LBrace);
        let mut items = Vec::new();
        let saved_shadowed = self.shadowed_typedefs.clone();

        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            self.skip_gcc_extensions();
            if self.is_type_specifier() {
                if let Some(decl) = self.parse_local_declaration() {
                    items.push(BlockItem::Declaration(decl));
                }
            } else {
                let stmt = self.parse_stmt();
                items.push(BlockItem::Statement(stmt));
            }
        }

        self.expect(&TokenKind::RBrace);
        self.shadowed_typedefs = saved_shadowed;
        CompoundStmt { items, span: start }
    }

    fn parse_local_declaration(&mut self) -> Option<Declaration> {
        let start = self.peek_span();
        // Reset storage class flags before parsing type specifier
        self.parsing_static = false;
        self.parsing_extern = false;
        self.parsing_typedef = false;
        self.parsing_inline = false;
        self.parsing_const = false;
        let type_spec = self.parse_type_specifier()?;

        // Handle storage-class specifiers and alignment specifiers that appear after the type.
        loop {
            match self.peek() {
                TokenKind::Typedef => { self.advance(); self.parsing_typedef = true; }
                TokenKind::Static => { self.advance(); self.parsing_static = true; }
                TokenKind::Extern => { self.advance(); self.parsing_extern = true; }
                TokenKind::Const => { self.advance(); self.parsing_const = true; }
                TokenKind::Volatile | TokenKind::Restrict
                | TokenKind::Inline | TokenKind::Register | TokenKind::Auto => { self.advance(); }
                TokenKind::Alignas => {
                    self.advance();
                    if matches!(self.peek(), TokenKind::LParen) {
                        self.skip_balanced_parens();
                    }
                }
                TokenKind::Attribute => {
                    self.advance();
                    self.skip_balanced_parens();
                }
                TokenKind::Extension => { self.advance(); }
                _ => break,
            }
        }

        let is_static = self.parsing_static;
        let is_extern = self.parsing_extern;

        let mut declarators = Vec::new();

        // Handle case where type is followed by semicolon (struct/enum/union def)
        if matches!(self.peek(), TokenKind::Semicolon) {
            self.advance();
            return Some(Declaration { type_spec, declarators, is_static, is_extern, is_typedef: self.parsing_typedef, is_const: self.parsing_const, span: start });
        }

        loop {
            let (name, derived) = self.parse_declarator();
            self.skip_asm_and_attributes();
            let init = if self.consume_if(&TokenKind::Assign) {
                Some(self.parse_initializer())
            } else {
                None
            };
            declarators.push(InitDeclarator {
                name: name.unwrap_or_default(),
                derived,
                init,
                span: start,
            });
            self.skip_asm_and_attributes();
            if !self.consume_if(&TokenKind::Comma) {
                break;
            }
        }

        // Register typedef names if this was a typedef declaration
        let is_typedef = self.parsing_typedef;
        if self.parsing_typedef {
            for decl in &declarators {
                if !decl.name.is_empty() {
                    self.typedefs.push(decl.name.clone());
                    // Remove from shadowed if we're re-typedefing
                    self.shadowed_typedefs.retain(|n| n != &decl.name);
                }
            }
            self.parsing_typedef = false;
        } else {
            // Non-typedef declaration: if a declarator name matches a typedef,
            // shadow the typedef so subsequent uses parse as variable references.
            for decl in &declarators {
                if !decl.name.is_empty() && self.typedefs.contains(&decl.name) {
                    if !self.shadowed_typedefs.contains(&decl.name) {
                        self.shadowed_typedefs.push(decl.name.clone());
                    }
                }
            }
        }

        self.expect(&TokenKind::Semicolon);
        Some(Declaration { type_spec, declarators, is_static, is_extern, is_typedef, is_const: self.parsing_const, span: start })
    }

    fn parse_initializer(&mut self) -> Initializer {
        if matches!(self.peek(), TokenKind::LBrace) {
            self.advance();
            let mut items = Vec::new();
            while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
                let mut designators = Vec::new();
                // Parse designators
                loop {
                    if self.consume_if(&TokenKind::LBracket) {
                        let idx = self.parse_expr();
                        self.expect(&TokenKind::RBracket);
                        designators.push(Designator::Index(idx));
                    } else if self.consume_if(&TokenKind::Dot) {
                        if let TokenKind::Identifier(name) = self.peek().clone() {
                            self.advance();
                            designators.push(Designator::Field(name));
                        }
                    } else {
                        break;
                    }
                }
                // GNU old-style designator: field: value (equivalent to .field = value)
                if designators.is_empty() {
                    if let TokenKind::Identifier(name) = self.peek().clone() {
                        if self.pos + 1 < self.tokens.len() && matches!(self.tokens[self.pos + 1].kind, TokenKind::Colon) {
                            self.advance(); // consume identifier
                            self.advance(); // consume colon
                            designators.push(Designator::Field(name));
                        }
                    }
                }
                if !designators.is_empty() {
                    // C99 style uses '=', GNU old-style already consumed ':'
                    if matches!(self.peek(), TokenKind::Assign) {
                        self.advance();
                    }
                }
                let init = self.parse_initializer();
                items.push(InitializerItem { designators, init });
                if !self.consume_if(&TokenKind::Comma) {
                    break;
                }
            }
            self.expect(&TokenKind::RBrace);
            Initializer::List(items)
        } else {
            Initializer::Expr(self.parse_assignment_expr())
        }
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.peek().clone() {
            TokenKind::Return => {
                let span = self.peek_span();
                self.advance();
                let expr = if matches!(self.peek(), TokenKind::Semicolon) {
                    None
                } else {
                    Some(self.parse_expr())
                };
                self.expect(&TokenKind::Semicolon);
                Stmt::Return(expr, span)
            }
            TokenKind::If => {
                let span = self.peek_span();
                self.advance();
                self.expect(&TokenKind::LParen);
                let cond = self.parse_expr();
                self.expect(&TokenKind::RParen);
                let then_stmt = self.parse_stmt();
                let else_stmt = if self.consume_if(&TokenKind::Else) {
                    Some(Box::new(self.parse_stmt()))
                } else {
                    None
                };
                Stmt::If(cond, Box::new(then_stmt), else_stmt, span)
            }
            TokenKind::While => {
                let span = self.peek_span();
                self.advance();
                self.expect(&TokenKind::LParen);
                let cond = self.parse_expr();
                self.expect(&TokenKind::RParen);
                let body = self.parse_stmt();
                Stmt::While(cond, Box::new(body), span)
            }
            TokenKind::Do => {
                let span = self.peek_span();
                self.advance();
                let body = self.parse_stmt();
                self.expect(&TokenKind::While);
                self.expect(&TokenKind::LParen);
                let cond = self.parse_expr();
                self.expect(&TokenKind::RParen);
                self.expect(&TokenKind::Semicolon);
                Stmt::DoWhile(Box::new(body), cond, span)
            }
            TokenKind::For => {
                let span = self.peek_span();
                self.advance();
                self.expect(&TokenKind::LParen);

                // Init
                let init = if matches!(self.peek(), TokenKind::Semicolon) {
                    self.advance();
                    None
                } else if self.is_type_specifier() {
                    let decl = self.parse_local_declaration();
                    decl.map(|d| Box::new(ForInit::Declaration(d)))
                } else {
                    let expr = self.parse_expr();
                    self.expect(&TokenKind::Semicolon);
                    Some(Box::new(ForInit::Expr(expr)))
                };

                // Condition
                let cond = if matches!(self.peek(), TokenKind::Semicolon) {
                    None
                } else {
                    Some(self.parse_expr())
                };
                self.expect(&TokenKind::Semicolon);

                // Increment
                let inc = if matches!(self.peek(), TokenKind::RParen) {
                    None
                } else {
                    Some(self.parse_expr())
                };
                self.expect(&TokenKind::RParen);

                let body = self.parse_stmt();
                Stmt::For(init, cond, inc, Box::new(body), span)
            }
            TokenKind::LBrace => {
                let compound = self.parse_compound_stmt();
                Stmt::Compound(compound)
            }
            TokenKind::Break => {
                let span = self.peek_span();
                self.advance();
                self.expect(&TokenKind::Semicolon);
                Stmt::Break(span)
            }
            TokenKind::Continue => {
                let span = self.peek_span();
                self.advance();
                self.expect(&TokenKind::Semicolon);
                Stmt::Continue(span)
            }
            TokenKind::Switch => {
                let span = self.peek_span();
                self.advance();
                self.expect(&TokenKind::LParen);
                let expr = self.parse_expr();
                self.expect(&TokenKind::RParen);
                let body = self.parse_stmt();
                Stmt::Switch(expr, Box::new(body), span)
            }
            TokenKind::Case => {
                let span = self.peek_span();
                self.advance();
                let expr = self.parse_expr();
                self.expect(&TokenKind::Colon);
                let stmt = self.parse_stmt();
                Stmt::Case(expr, Box::new(stmt), span)
            }
            TokenKind::Default => {
                let span = self.peek_span();
                self.advance();
                self.expect(&TokenKind::Colon);
                let stmt = self.parse_stmt();
                Stmt::Default(Box::new(stmt), span)
            }
            TokenKind::Goto => {
                let span = self.peek_span();
                self.advance();
                // Check for computed goto: goto *expr;
                if matches!(self.peek(), TokenKind::Star) {
                    self.advance(); // consume '*'
                    let expr = self.parse_expr();
                    self.expect(&TokenKind::Semicolon);
                    Stmt::GotoIndirect(Box::new(expr), span)
                } else {
                    let label = if let TokenKind::Identifier(name) = self.peek().clone() {
                        self.advance();
                        name
                    } else {
                        String::new()
                    };
                    self.expect(&TokenKind::Semicolon);
                    Stmt::Goto(label, span)
                }
            }
            TokenKind::Identifier(ref name) => {
                // Check for label (identifier followed by colon)
                let name_clone = name.clone();
                let span = self.peek_span();
                if self.pos + 1 < self.tokens.len() && matches!(self.tokens[self.pos + 1].kind, TokenKind::Colon) {
                    self.advance(); // identifier
                    self.advance(); // colon
                    let stmt = self.parse_stmt();
                    Stmt::Label(name_clone, Box::new(stmt), span)
                } else {
                    // Expression statement
                    let expr = self.parse_expr();
                    self.expect(&TokenKind::Semicolon);
                    Stmt::Expr(Some(expr))
                }
            }
            TokenKind::Asm => {
                self.parse_inline_asm()
            }
            TokenKind::Semicolon => {
                self.advance();
                Stmt::Expr(None)
            }
            _ => {
                // Expression statement
                let expr = self.parse_expr();
                self.expect(&TokenKind::Semicolon);
                Stmt::Expr(Some(expr))
            }
        }
    }

    // === Inline assembly parsing ===

    fn parse_inline_asm(&mut self) -> Stmt {
        use crate::frontend::parser::ast::AsmOperand;
        self.advance(); // consume 'asm' / '__asm__'
        // Skip optional qualifiers: volatile, goto, inline
        while matches!(self.peek(), TokenKind::Volatile)
            || matches!(self.peek(), TokenKind::Goto)
            || matches!(self.peek(), TokenKind::Inline)
        {
            self.advance();
        }
        self.expect(&TokenKind::LParen);

        // Parse template string (may be concatenated string literals)
        let template = self.parse_asm_string();

        // Parse optional sections separated by ':'
        let mut outputs = Vec::new();
        let mut inputs = Vec::new();
        let mut clobbers = Vec::new();

        // First colon: outputs
        if matches!(self.peek(), TokenKind::Colon) {
            self.advance();
            outputs = self.parse_asm_operands();

            // Second colon: inputs
            if matches!(self.peek(), TokenKind::Colon) {
                self.advance();
                inputs = self.parse_asm_operands();

                // Third colon: clobbers
                if matches!(self.peek(), TokenKind::Colon) {
                    self.advance();
                    clobbers = self.parse_asm_clobbers();

                    // Fourth colon: goto labels (just skip)
                    if matches!(self.peek(), TokenKind::Colon) {
                        self.advance();
                        // Skip goto labels
                        while !matches!(self.peek(), TokenKind::RParen | TokenKind::Eof) {
                            self.advance();
                        }
                    }
                }
            }
        }

        self.expect(&TokenKind::RParen);
        self.consume_if(&TokenKind::Semicolon);

        Stmt::InlineAsm { template, outputs, inputs, clobbers }
    }

    fn parse_asm_string(&mut self) -> String {
        let mut result = String::new();
        while let TokenKind::StringLiteral(ref s) = self.peek() {
            result.push_str(s);
            self.advance();
        }
        result
    }

    fn parse_asm_operands(&mut self) -> Vec<crate::frontend::parser::ast::AsmOperand> {
        let mut operands = Vec::new();
        // Check if we have operands (not : or ) )
        if matches!(self.peek(), TokenKind::Colon | TokenKind::RParen) {
            return operands;
        }
        loop {
            let operand = self.parse_one_asm_operand();
            operands.push(operand);
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }
        operands
    }

    fn parse_one_asm_operand(&mut self) -> crate::frontend::parser::ast::AsmOperand {
        // Optional [name]
        let name = if matches!(self.peek(), TokenKind::LBracket) {
            self.advance(); // [
            let n = if let TokenKind::Identifier(ref id) = self.peek() {
                let id = id.clone();
                self.advance();
                Some(id)
            } else {
                None
            };
            self.expect(&TokenKind::RBracket);
            n
        } else {
            None
        };

        // Constraint string
        let constraint = if let TokenKind::StringLiteral(ref s) = self.peek() {
            let c = s.clone();
            self.advance();
            // Concatenate adjacent string literals in constraint
            let mut full = c;
            while let TokenKind::StringLiteral(ref s2) = self.peek() {
                full.push_str(s2);
                self.advance();
            }
            full
        } else {
            String::new()
        };

        // (expr)
        self.expect(&TokenKind::LParen);
        let expr = self.parse_expr();
        self.expect(&TokenKind::RParen);

        crate::frontend::parser::ast::AsmOperand { name, constraint, expr }
    }

    fn parse_asm_clobbers(&mut self) -> Vec<String> {
        let mut clobbers = Vec::new();
        if matches!(self.peek(), TokenKind::Colon | TokenKind::RParen) {
            return clobbers;
        }
        loop {
            if let TokenKind::StringLiteral(ref s) = self.peek() {
                clobbers.push(s.clone());
                self.advance();
            } else {
                break;
            }
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }
        clobbers
    }

    // === Expression parsing (precedence climbing) ===

    fn parse_expr(&mut self) -> Expr {
        let lhs = self.parse_assignment_expr();
        if matches!(self.peek(), TokenKind::Comma) {
            let span = self.peek_span();
            self.advance();
            let rhs = self.parse_expr();
            Expr::Comma(Box::new(lhs), Box::new(rhs), span)
        } else {
            lhs
        }
    }

    fn parse_assignment_expr(&mut self) -> Expr {
        let lhs = self.parse_conditional_expr();

        match self.peek() {
            TokenKind::Assign => {
                let span = self.peek_span();
                self.advance();
                let rhs = self.parse_assignment_expr();
                Expr::Assign(Box::new(lhs), Box::new(rhs), span)
            }
            _ => {
                if let Some(op) = self.compound_assign_op() {
                    let span = self.peek_span();
                    self.advance();
                    let rhs = self.parse_assignment_expr();
                    Expr::CompoundAssign(op, Box::new(lhs), Box::new(rhs), span)
                } else {
                    lhs
                }
            }
        }
    }

    fn parse_conditional_expr(&mut self) -> Expr {
        let cond = self.parse_logical_or_expr();
        if self.consume_if(&TokenKind::Question) {
            let span = cond.span();
            let then_expr = self.parse_expr();
            self.expect(&TokenKind::Colon);
            let else_expr = self.parse_conditional_expr();
            Expr::Conditional(Box::new(cond), Box::new(then_expr), Box::new(else_expr), span)
        } else {
            cond
        }
    }

    fn parse_logical_or_expr(&mut self) -> Expr {
        let mut lhs = self.parse_logical_and_expr();
        while matches!(self.peek(), TokenKind::PipePipe) {
            let span = self.peek_span();
            self.advance();
            let rhs = self.parse_logical_and_expr();
            lhs = Expr::BinaryOp(BinOp::LogicalOr, Box::new(lhs), Box::new(rhs), span);
        }
        lhs
    }

    fn parse_logical_and_expr(&mut self) -> Expr {
        let mut lhs = self.parse_bitwise_or_expr();
        while matches!(self.peek(), TokenKind::AmpAmp) {
            let span = self.peek_span();
            self.advance();
            let rhs = self.parse_bitwise_or_expr();
            lhs = Expr::BinaryOp(BinOp::LogicalAnd, Box::new(lhs), Box::new(rhs), span);
        }
        lhs
    }

    fn parse_bitwise_or_expr(&mut self) -> Expr {
        let mut lhs = self.parse_bitwise_xor_expr();
        while matches!(self.peek(), TokenKind::Pipe) {
            let span = self.peek_span();
            self.advance();
            let rhs = self.parse_bitwise_xor_expr();
            lhs = Expr::BinaryOp(BinOp::BitOr, Box::new(lhs), Box::new(rhs), span);
        }
        lhs
    }

    fn parse_bitwise_xor_expr(&mut self) -> Expr {
        let mut lhs = self.parse_bitwise_and_expr();
        while matches!(self.peek(), TokenKind::Caret) {
            let span = self.peek_span();
            self.advance();
            let rhs = self.parse_bitwise_and_expr();
            lhs = Expr::BinaryOp(BinOp::BitXor, Box::new(lhs), Box::new(rhs), span);
        }
        lhs
    }

    fn parse_bitwise_and_expr(&mut self) -> Expr {
        let mut lhs = self.parse_equality_expr();
        while matches!(self.peek(), TokenKind::Amp) {
            let span = self.peek_span();
            self.advance();
            let rhs = self.parse_equality_expr();
            lhs = Expr::BinaryOp(BinOp::BitAnd, Box::new(lhs), Box::new(rhs), span);
        }
        lhs
    }

    fn parse_equality_expr(&mut self) -> Expr {
        let mut lhs = self.parse_relational_expr();
        loop {
            match self.peek() {
                TokenKind::EqualEqual => {
                    let span = self.peek_span(); self.advance();
                    let rhs = self.parse_relational_expr();
                    lhs = Expr::BinaryOp(BinOp::Eq, Box::new(lhs), Box::new(rhs), span);
                }
                TokenKind::BangEqual => {
                    let span = self.peek_span(); self.advance();
                    let rhs = self.parse_relational_expr();
                    lhs = Expr::BinaryOp(BinOp::Ne, Box::new(lhs), Box::new(rhs), span);
                }
                _ => break,
            }
        }
        lhs
    }

    fn parse_relational_expr(&mut self) -> Expr {
        let mut lhs = self.parse_shift_expr();
        loop {
            match self.peek() {
                TokenKind::Less => { let span = self.peek_span(); self.advance(); let rhs = self.parse_shift_expr(); lhs = Expr::BinaryOp(BinOp::Lt, Box::new(lhs), Box::new(rhs), span); }
                TokenKind::LessEqual => { let span = self.peek_span(); self.advance(); let rhs = self.parse_shift_expr(); lhs = Expr::BinaryOp(BinOp::Le, Box::new(lhs), Box::new(rhs), span); }
                TokenKind::Greater => { let span = self.peek_span(); self.advance(); let rhs = self.parse_shift_expr(); lhs = Expr::BinaryOp(BinOp::Gt, Box::new(lhs), Box::new(rhs), span); }
                TokenKind::GreaterEqual => { let span = self.peek_span(); self.advance(); let rhs = self.parse_shift_expr(); lhs = Expr::BinaryOp(BinOp::Ge, Box::new(lhs), Box::new(rhs), span); }
                _ => break,
            }
        }
        lhs
    }

    fn parse_shift_expr(&mut self) -> Expr {
        let mut lhs = self.parse_additive_expr();
        loop {
            match self.peek() {
                TokenKind::LessLess => { let span = self.peek_span(); self.advance(); let rhs = self.parse_additive_expr(); lhs = Expr::BinaryOp(BinOp::Shl, Box::new(lhs), Box::new(rhs), span); }
                TokenKind::GreaterGreater => { let span = self.peek_span(); self.advance(); let rhs = self.parse_additive_expr(); lhs = Expr::BinaryOp(BinOp::Shr, Box::new(lhs), Box::new(rhs), span); }
                _ => break,
            }
        }
        lhs
    }

    fn parse_additive_expr(&mut self) -> Expr {
        let mut lhs = self.parse_multiplicative_expr();
        loop {
            match self.peek() {
                TokenKind::Plus => { let span = self.peek_span(); self.advance(); let rhs = self.parse_multiplicative_expr(); lhs = Expr::BinaryOp(BinOp::Add, Box::new(lhs), Box::new(rhs), span); }
                TokenKind::Minus => { let span = self.peek_span(); self.advance(); let rhs = self.parse_multiplicative_expr(); lhs = Expr::BinaryOp(BinOp::Sub, Box::new(lhs), Box::new(rhs), span); }
                _ => break,
            }
        }
        lhs
    }

    fn parse_multiplicative_expr(&mut self) -> Expr {
        let mut lhs = self.parse_cast_expr();
        loop {
            match self.peek() {
                TokenKind::Star => { let span = self.peek_span(); self.advance(); let rhs = self.parse_cast_expr(); lhs = Expr::BinaryOp(BinOp::Mul, Box::new(lhs), Box::new(rhs), span); }
                TokenKind::Slash => { let span = self.peek_span(); self.advance(); let rhs = self.parse_cast_expr(); lhs = Expr::BinaryOp(BinOp::Div, Box::new(lhs), Box::new(rhs), span); }
                TokenKind::Percent => { let span = self.peek_span(); self.advance(); let rhs = self.parse_cast_expr(); lhs = Expr::BinaryOp(BinOp::Mod, Box::new(lhs), Box::new(rhs), span); }
                _ => break,
            }
        }
        lhs
    }

    fn parse_cast_expr(&mut self) -> Expr {
        // Try to parse (type-name)expr as a cast expression or compound literal
        if matches!(self.peek(), TokenKind::LParen) {
            let save = self.pos;
            let save_typedef = self.parsing_typedef;
            self.advance();
            if self.is_type_specifier() {
                if let Some(type_spec) = self.parse_type_specifier() {
                    // Parse abstract declarator after the type specifier.
                    // This handles:
                    //   - Simple pointers: (int *)
                    //   - Parenthesized pointers: (int (*)), (int ((*)))
                    //   - Function pointers: (int (*)(int))
                    //   - Pointer to pointer: (int **), (int (*(*)))
                    //   - Arrays: (int [3])
                    let mut result_type = type_spec;
                    // Parse leading pointer(s)
                    while self.consume_if(&TokenKind::Star) {
                        result_type = TypeSpecifier::Pointer(Box::new(result_type));
                        self.skip_cv_qualifiers();
                    }
                    // Handle parenthesized abstract declarators: (type (**)), (type (*)(params))
                    // Patterns: (*), ((*)), (*(*(*))), (*)[N], (*)(params)
                    if matches!(self.peek(), TokenKind::LParen) {
                        let save2 = self.pos;
                        // Try to parse a parenthesized abstract declarator
                        if let Some(ptr_depth) = self.try_parse_paren_abstract_declarator() {
                            // Check what follows the parenthesized group
                            if matches!(self.peek(), TokenKind::LParen) {
                                // Function pointer cast: (*)(params) or (**)(params)
                                self.skip_balanced_parens();
                                // All pointer levels contribute
                                for _ in 0..ptr_depth {
                                    result_type = TypeSpecifier::Pointer(Box::new(result_type));
                                }
                            } else if matches!(self.peek(), TokenKind::LBracket) {
                                // Pointer to array: (*)[N]
                                while matches!(self.peek(), TokenKind::LBracket) {
                                    self.advance();
                                    let size = if matches!(self.peek(), TokenKind::RBracket) {
                                        None
                                    } else {
                                        Some(Box::new(self.parse_expr()))
                                    };
                                    self.expect(&TokenKind::RBracket);
                                    result_type = TypeSpecifier::Array(Box::new(result_type), size);
                                }
                                for _ in 0..ptr_depth {
                                    result_type = TypeSpecifier::Pointer(Box::new(result_type));
                                }
                            } else {
                                // Just parenthesized pointers
                                for _ in 0..ptr_depth {
                                    result_type = TypeSpecifier::Pointer(Box::new(result_type));
                                }
                            }
                        } else {
                            // Not a parenthesized abstract declarator
                            self.pos = save2;
                        }
                    }
                    // Parse array dimensions in abstract declarators e.g. (int [3])
                    while matches!(self.peek(), TokenKind::LBracket) {
                        self.advance(); // consume '['
                        let size = if matches!(self.peek(), TokenKind::RBracket) {
                            None
                        } else {
                            Some(Box::new(self.parse_expr()))
                        };
                        self.expect(&TokenKind::RBracket);
                        result_type = TypeSpecifier::Array(Box::new(result_type), size);
                    }
                    if matches!(self.peek(), TokenKind::RParen) {
                        let span = self.peek_span();
                        self.advance();
                        // Check for compound literal: (type){...}
                        if matches!(self.peek(), TokenKind::LBrace) {
                            let init = self.parse_initializer();
                            return Expr::CompoundLiteral(result_type, Box::new(init), span);
                        }
                        let expr = self.parse_cast_expr();
                        return Expr::Cast(result_type, Box::new(expr), span);
                    }
                }
            }
            self.pos = save;
            self.parsing_typedef = save_typedef;
        }
        self.parse_unary_expr()
    }

    fn parse_unary_expr(&mut self) -> Expr {
        match self.peek().clone() {
            TokenKind::AmpAmp => {
                // GCC extension: &&label (address of label, for computed goto)
                let span = self.peek_span();
                if self.pos + 1 < self.tokens.len() {
                    if let TokenKind::Identifier(ref name) = self.tokens[self.pos + 1].kind {
                        let label_name = name.clone();
                        self.advance(); // consume &&
                        self.advance(); // consume identifier
                        return Expr::LabelAddr(label_name, span);
                    }
                }
                // If not followed by identifier, fall through to parse as expression
                self.parse_postfix_expr()
            }
            TokenKind::PlusPlus => {
                let span = self.peek_span();
                self.advance();
                let expr = self.parse_unary_expr();
                Expr::UnaryOp(UnaryOp::PreInc, Box::new(expr), span)
            }
            TokenKind::MinusMinus => {
                let span = self.peek_span();
                self.advance();
                let expr = self.parse_unary_expr();
                Expr::UnaryOp(UnaryOp::PreDec, Box::new(expr), span)
            }
            TokenKind::Plus => {
                // Unary plus: +expr (C standard, no-op but valid)
                let span = self.peek_span();
                self.advance();
                let expr = self.parse_cast_expr();
                Expr::UnaryOp(UnaryOp::Plus, Box::new(expr), span)
            }
            TokenKind::Minus => {
                let span = self.peek_span();
                self.advance();
                let expr = self.parse_cast_expr();
                Expr::UnaryOp(UnaryOp::Neg, Box::new(expr), span)
            }
            TokenKind::Tilde => {
                let span = self.peek_span();
                self.advance();
                let expr = self.parse_cast_expr();
                Expr::UnaryOp(UnaryOp::BitNot, Box::new(expr), span)
            }
            TokenKind::Bang => {
                let span = self.peek_span();
                self.advance();
                let expr = self.parse_cast_expr();
                Expr::UnaryOp(UnaryOp::LogicalNot, Box::new(expr), span)
            }
            TokenKind::Amp => {
                let span = self.peek_span();
                self.advance();
                let expr = self.parse_cast_expr();
                Expr::AddressOf(Box::new(expr), span)
            }
            TokenKind::Star => {
                let span = self.peek_span();
                self.advance();
                let expr = self.parse_cast_expr();
                Expr::Deref(Box::new(expr), span)
            }
            TokenKind::Sizeof => {
                let span = self.peek_span();
                self.advance();
                if matches!(self.peek(), TokenKind::LParen) {
                    let save = self.pos;
                    let save_typedef = self.parsing_typedef;
                    self.advance();
                    if self.is_type_specifier() {
                        if let Some(ts) = self.parse_type_specifier() {
                            // Parse abstract declarator: wrap type in Pointer for each *
                            let mut result_type = ts;
                            while self.consume_if(&TokenKind::Star) {
                                result_type = TypeSpecifier::Pointer(Box::new(result_type));
                                self.skip_cv_qualifiers();
                            }
                            // Handle parenthesized abstract declarators: sizeof(int(*)[N])
                            if matches!(self.peek(), TokenKind::LParen) {
                                let save_sz = self.pos;
                                if let Some(ptr_depth) = self.try_parse_paren_abstract_declarator() {
                                    // Check what follows
                                    if matches!(self.peek(), TokenKind::LBracket) {
                                        // Pointer to array: sizeof(int(*)[N])
                                        while matches!(self.peek(), TokenKind::LBracket) {
                                            self.advance();
                                            let size_expr = if !matches!(self.peek(), TokenKind::RBracket) {
                                                Some(Box::new(self.parse_assignment_expr()))
                                            } else {
                                                None
                                            };
                                            self.consume_if(&TokenKind::RBracket);
                                            result_type = TypeSpecifier::Array(Box::new(result_type), size_expr);
                                        }
                                        for _ in 0..ptr_depth {
                                            result_type = TypeSpecifier::Pointer(Box::new(result_type));
                                        }
                                    } else if matches!(self.peek(), TokenKind::LParen) {
                                        // Function pointer: sizeof(int(*)(int))
                                        self.skip_balanced_parens();
                                        for _ in 0..ptr_depth {
                                            result_type = TypeSpecifier::Pointer(Box::new(result_type));
                                        }
                                    } else {
                                        // Just parenthesized pointer: sizeof(int(*))
                                        for _ in 0..ptr_depth {
                                            result_type = TypeSpecifier::Pointer(Box::new(result_type));
                                        }
                                    }
                                } else {
                                    self.pos = save_sz;
                                    // Not a paren abstract declarator - might be function pointer
                                    // sizeof(void (*)(int)) - old handling
                                    self.skip_balanced_parens();
                                }
                            }
                            // Parse array dimensions: sizeof(int[10])
                            while matches!(self.peek(), TokenKind::LBracket) {
                                self.advance();
                                // Parse the array size expression
                                let size_expr = if !matches!(self.peek(), TokenKind::RBracket) {
                                    Some(Box::new(self.parse_assignment_expr()))
                                } else {
                                    None
                                };
                                self.consume_if(&TokenKind::RBracket);
                                result_type = TypeSpecifier::Array(Box::new(result_type), size_expr);
                            }
                            if matches!(self.peek(), TokenKind::RParen) {
                                self.expect(&TokenKind::RParen);
                                return Expr::Sizeof(Box::new(SizeofArg::Type(result_type)), span);
                            }
                        }
                    }
                    self.pos = save;
                    self.parsing_typedef = save_typedef;
                }
                let expr = self.parse_unary_expr();
                Expr::Sizeof(Box::new(SizeofArg::Expr(expr)), span)
            }
            TokenKind::Alignof => {
                let span = self.peek_span();
                self.advance();
                // _Alignof(type) - always requires parenthesized type
                self.expect(&TokenKind::LParen);
                if let Some(ts) = self.parse_type_specifier() {
                    let mut result_type = ts;
                    while self.consume_if(&TokenKind::Star) {
                        result_type = TypeSpecifier::Pointer(Box::new(result_type));
                        self.skip_cv_qualifiers();
                    }
                    self.expect(&TokenKind::RParen);
                    Expr::Alignof(result_type, span)
                } else {
                    // Fallback: treat as sizeof-like with expression
                    let expr = self.parse_assignment_expr();
                    self.expect(&TokenKind::RParen);
                    // Use 8 as default alignment (pointer alignment on x86-64)
                    Expr::IntLiteral(8, span)
                }
            }
            _ => self.parse_postfix_expr(),
        }
    }

    fn parse_postfix_expr(&mut self) -> Expr {
        let mut expr = self.parse_primary_expr();

        loop {
            match self.peek() {
                TokenKind::LParen => {
                    // Function call
                    let span = self.peek_span();
                    self.advance();
                    let mut args = Vec::new();
                    if !matches!(self.peek(), TokenKind::RParen) {
                        args.push(self.parse_assignment_expr());
                        while self.consume_if(&TokenKind::Comma) {
                            args.push(self.parse_assignment_expr());
                        }
                    }
                    self.expect(&TokenKind::RParen);
                    expr = Expr::FunctionCall(Box::new(expr), args, span);
                }
                TokenKind::LBracket => {
                    let span = self.peek_span();
                    self.advance();
                    let index = self.parse_expr();
                    self.expect(&TokenKind::RBracket);
                    expr = Expr::ArraySubscript(Box::new(expr), Box::new(index), span);
                }
                TokenKind::Dot => {
                    let span = self.peek_span();
                    self.advance();
                    let field = if let TokenKind::Identifier(name) = self.peek().clone() {
                        self.advance();
                        name
                    } else {
                        String::new()
                    };
                    expr = Expr::MemberAccess(Box::new(expr), field, span);
                }
                TokenKind::Arrow => {
                    let span = self.peek_span();
                    self.advance();
                    let field = if let TokenKind::Identifier(name) = self.peek().clone() {
                        self.advance();
                        name
                    } else {
                        String::new()
                    };
                    expr = Expr::PointerMemberAccess(Box::new(expr), field, span);
                }
                TokenKind::PlusPlus => {
                    let span = self.peek_span();
                    self.advance();
                    expr = Expr::PostfixOp(PostfixOp::PostInc, Box::new(expr), span);
                }
                TokenKind::MinusMinus => {
                    let span = self.peek_span();
                    self.advance();
                    expr = Expr::PostfixOp(PostfixOp::PostDec, Box::new(expr), span);
                }
                _ => break,
            }
        }

        expr
    }

    fn parse_primary_expr(&mut self) -> Expr {
        match self.peek().clone() {
            TokenKind::IntLiteral(val) => {
                let span = self.peek_span();
                self.advance();
                Expr::IntLiteral(val, span)
            }
            TokenKind::UIntLiteral(val) => {
                let span = self.peek_span();
                self.advance();
                Expr::UIntLiteral(val, span)
            }
            TokenKind::LongLiteral(val) => {
                let span = self.peek_span();
                self.advance();
                Expr::LongLiteral(val, span)
            }
            TokenKind::ULongLiteral(val) => {
                let span = self.peek_span();
                self.advance();
                Expr::ULongLiteral(val, span)
            }
            TokenKind::FloatLiteral(val) => {
                let span = self.peek_span();
                self.advance();
                Expr::FloatLiteral(val, span)
            }
            TokenKind::FloatLiteralF32(val) => {
                let span = self.peek_span();
                self.advance();
                Expr::FloatLiteralF32(val, span)
            }
            TokenKind::FloatLiteralLongDouble(val) => {
                let span = self.peek_span();
                self.advance();
                Expr::FloatLiteralLongDouble(val, span)
            }
            TokenKind::StringLiteral(ref s) => {
                let mut result = s.clone();
                let span = self.peek_span();
                self.advance();
                // Concatenate adjacent string literals
                while let TokenKind::StringLiteral(ref s2) = self.peek() {
                    result.push_str(s2);
                    self.advance();
                }
                Expr::StringLiteral(result, span)
            }
            TokenKind::CharLiteral(c) => {
                let span = self.peek_span();
                self.advance();
                Expr::CharLiteral(c, span)
            }
            TokenKind::Identifier(ref name) => {
                let name = name.clone();
                let span = self.peek_span();
                self.advance();
                Expr::Identifier(name, span)
            }
            TokenKind::LParen => {
                self.advance();
                // Check for GCC statement expression: ({ stmt; stmt; expr; })
                if matches!(self.peek(), TokenKind::LBrace) {
                    let span = self.peek_span();
                    // Parse as compound statement, use last expression as value
                    let compound = self.parse_compound_stmt();
                    self.expect(&TokenKind::RParen);
                    // Extract last expression from compound statement
                    // For now, treat as 0 - TODO: properly extract last expr value
                    Expr::StmtExpr(compound, span)
                } else {
                    let expr = self.parse_expr();
                    self.expect(&TokenKind::RParen);
                    expr
                }
            }
            TokenKind::Generic => {
                // _Generic(controlling_expr, type1: expr1, type2: expr2, ..., default: exprN)
                let span = self.peek_span();
                self.advance(); // consume _Generic
                self.expect(&TokenKind::LParen);
                // Parse controlling expression
                let controlling = self.parse_assignment_expr();
                self.expect(&TokenKind::Comma);
                // Parse associations
                let mut associations = Vec::new();
                loop {
                    if matches!(self.peek(), TokenKind::RParen) {
                        break;
                    }
                    let type_spec = if matches!(self.peek(), TokenKind::Default) {
                        self.advance(); // consume 'default'
                        None
                    } else {
                        // Parse type-name: type-specifier followed by optional abstract-declarator
                        if let Some(mut ts) = self.parse_type_specifier() {
                            // Handle pointer declarators (e.g., char *, int **, const int *)
                            while matches!(self.peek(), TokenKind::Star) {
                                self.advance(); // consume '*'
                                // Skip qualifiers after pointer
                                while matches!(self.peek(), TokenKind::Const | TokenKind::Volatile | TokenKind::Restrict) {
                                    self.advance();
                                }
                                ts = TypeSpecifier::Pointer(Box::new(ts));
                            }
                            Some(ts)
                        } else {
                            None
                        }
                    };
                    self.expect(&TokenKind::Colon);
                    let expr = self.parse_assignment_expr();
                    associations.push(GenericAssociation { type_spec, expr });
                    if !self.consume_if(&TokenKind::Comma) {
                        break;
                    }
                }
                self.expect(&TokenKind::RParen);
                Expr::GenericSelection(Box::new(controlling), associations, span)
            }
            TokenKind::Asm => {
                // GCC asm expression: asm [volatile] (string : outputs : inputs : clobbers)
                let span = self.peek_span();
                self.advance();
                // Skip optional volatile
                self.consume_if(&TokenKind::Volatile);
                if matches!(self.peek(), TokenKind::LParen) {
                    self.skip_balanced_parens();
                }
                // TODO: properly handle inline asm
                Expr::IntLiteral(0, span)
            }
            TokenKind::BuiltinVaArg => {
                // __builtin_va_arg(ap_expr, type-name)
                let span = self.peek_span();
                self.advance();
                self.expect(&TokenKind::LParen);
                let ap_expr = self.parse_assignment_expr();
                self.expect(&TokenKind::Comma);
                // Parse type-name (same as in cast/sizeof)
                let type_spec = self.parse_va_arg_type();
                self.expect(&TokenKind::RParen);
                Expr::VaArg(Box::new(ap_expr), type_spec, span)
            }
            TokenKind::Builtin => {
                // __builtin_va_list used as identifier/type - treat as identifier
                let span = self.peek_span();
                self.advance();
                Expr::Identifier("__builtin_va_list".to_string(), span)
            }
            TokenKind::Extension => {
                // __extension__ can prefix expressions
                let span = self.peek_span();
                self.advance();
                self.parse_cast_expr()
            }
            _ => {
                let span = self.peek_span();
                eprintln!("parser error: unexpected token {:?} in expression", self.peek());
                self.advance();
                Expr::IntLiteral(0, span)
            }
        }
    }

    /// Parse a type-name for __builtin_va_arg, similar to how cast expressions parse types.
    fn parse_va_arg_type(&mut self) -> TypeSpecifier {
        if let Some(type_spec) = self.parse_type_specifier() {
            let mut result_type = type_spec;
            // Parse pointer declarators
            while self.consume_if(&TokenKind::Star) {
                result_type = TypeSpecifier::Pointer(Box::new(result_type));
                self.skip_cv_qualifiers();
            }
            // Handle function pointer: type (*)(args)
            if matches!(self.peek(), TokenKind::LParen) {
                let save2 = self.pos;
                self.advance();
                if self.consume_if(&TokenKind::Star) {
                    while !matches!(self.peek(), TokenKind::RParen | TokenKind::Eof) {
                        self.advance();
                    }
                    self.consume_if(&TokenKind::RParen);
                    if matches!(self.peek(), TokenKind::LParen) {
                        self.skip_balanced_parens();
                    }
                    result_type = TypeSpecifier::Pointer(Box::new(result_type));
                } else {
                    self.pos = save2;
                }
            }
            // Parse array dimensions
            while matches!(self.peek(), TokenKind::LBracket) {
                self.advance();
                let size = if matches!(self.peek(), TokenKind::RBracket) {
                    None
                } else {
                    Some(Box::new(self.parse_expr()))
                };
                self.expect(&TokenKind::RBracket);
                result_type = TypeSpecifier::Array(Box::new(result_type), size);
            }
            result_type
        } else {
            eprintln!("parser error: expected type in __builtin_va_arg");
            TypeSpecifier::Int
        }
    }
}
