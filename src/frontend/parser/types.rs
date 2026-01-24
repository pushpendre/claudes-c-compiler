// Type specifier parsing: handles all C type specifiers including struct/union/enum
// definitions, typedef names, and GNU extensions like typeof and _Complex.
//
// The main complexity here is that C allows type specifier tokens in any order
// (e.g., "long unsigned int" == "unsigned long int"), so we collect flags
// and resolve them at the end.

use crate::frontend::lexer::token::TokenKind;
use super::ast::*;
use super::parser::Parser;

impl Parser {
    /// Parse a complete type specifier. Returns None if no type specifier found.
    ///
    /// Handles arbitrary ordering of type keywords (C allows "long unsigned int"
    /// or "unsigned long int"), struct/union/enum definitions, typedef names,
    /// typeof expressions, and _Complex types.
    #[allow(unused_assignments)]
    pub(super) fn parse_type_specifier(&mut self) -> Option<TypeSpecifier> {
        self.skip_gcc_extensions();

        // Flags for collecting type specifier tokens in any order
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

        // Collect qualifiers, storage classes, and type specifiers
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
                // Type specifier tokens
                TokenKind::Void => {
                    self.advance(); has_void = true; any_base_specifier = true;
                    break; // void can't combine with others
                }
                TokenKind::Char => {
                    self.advance(); has_char = true; any_base_specifier = true;
                    break; // char only combines with signed/unsigned
                }
                TokenKind::Short => {
                    self.advance(); has_short = true; any_base_specifier = true;
                }
                TokenKind::Int => {
                    self.advance(); has_int = true; any_base_specifier = true;
                }
                TokenKind::Long => {
                    self.advance(); long_count += 1; any_base_specifier = true;
                }
                TokenKind::Float => {
                    self.advance(); has_float = true; any_base_specifier = true;
                    break;
                }
                TokenKind::Double => {
                    self.advance(); has_double = true; any_base_specifier = true;
                    break;
                }
                TokenKind::Bool => {
                    self.advance(); has_bool = true; any_base_specifier = true;
                    break;
                }
                TokenKind::Signed => {
                    self.advance(); has_signed = true; any_base_specifier = true;
                }
                TokenKind::Unsigned => {
                    self.advance(); has_unsigned = true; any_base_specifier = true;
                }
                TokenKind::Struct => {
                    self.advance(); has_struct = true; any_base_specifier = true;
                    break;
                }
                TokenKind::Union => {
                    self.advance(); has_union = true; any_base_specifier = true;
                    break;
                }
                TokenKind::Enum => {
                    self.advance(); has_enum = true; any_base_specifier = true;
                    break;
                }
                TokenKind::Typeof => {
                    self.advance(); has_typeof = true; any_base_specifier = true;
                    break;
                }
                TokenKind::Builtin => {
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

        // After the main loop, collect trailing specifiers that can follow
        // certain base types (e.g., "short unsigned int", "double long", "float _Complex")
        self.collect_trailing_specifiers(
            &mut has_char, &mut has_short, &mut has_int, &mut long_count,
            &mut has_signed, &mut has_unsigned, &mut has_float, &mut has_double,
            &mut has_complex,
        );

        if !any_base_specifier {
            return None;
        }

        // Resolve collected flags into a TypeSpecifier
        let base = self.resolve_type_flags(
            has_void, has_bool, has_float, has_double, has_complex, has_char,
            has_short, has_int, has_unsigned, has_signed, has_struct, has_union,
            has_enum, has_typeof, long_count, typedef_name,
        );

        // Handle trailing _Complex, qualifiers, and storage classes after the base type
        let base = self.consume_trailing_qualifiers(base);

        Some(base)
    }

    /// Collect additional type specifier tokens that follow the initial base type.
    /// E.g., "short" can be followed by "unsigned int", "double" by "long",
    /// "float" by "_Complex".
    fn collect_trailing_specifiers(
        &mut self,
        has_char: &mut bool, has_short: &mut bool, has_int: &mut bool,
        long_count: &mut u32, has_signed: &mut bool, has_unsigned: &mut bool,
        has_float: &mut bool, has_double: &mut bool, has_complex: &mut bool,
    ) {
        if *has_char || *has_short || *has_int || *long_count > 0 {
            loop {
                match self.peek() {
                    TokenKind::Signed => { self.advance(); *has_signed = true; }
                    TokenKind::Unsigned => { self.advance(); *has_unsigned = true; }
                    TokenKind::Int => { self.advance(); *has_int = true; }
                    TokenKind::Long => { self.advance(); *long_count += 1; }
                    TokenKind::Short => { self.advance(); *has_short = true; }
                    TokenKind::Char => { self.advance(); *has_char = true; }
                    TokenKind::Complex => { self.advance(); *has_complex = true; }
                    TokenKind::Const | TokenKind::Volatile | TokenKind::Restrict => { self.advance(); }
                    TokenKind::Static => { self.advance(); self.parsing_static = true; }
                    TokenKind::Extern => { self.advance(); self.parsing_extern = true; }
                    TokenKind::Register | TokenKind::Noreturn => { self.advance(); }
                    TokenKind::Inline => { self.advance(); self.parsing_inline = true; }
                    TokenKind::Attribute => { self.advance(); self.skip_balanced_parens(); }
                    TokenKind::Extension => { self.advance(); }
                    _ => break,
                }
            }
        } else if *has_float {
            // "float" can be followed by "_Complex"
            loop {
                match self.peek() {
                    TokenKind::Complex => { self.advance(); *has_complex = true; }
                    TokenKind::Const | TokenKind::Volatile | TokenKind::Restrict => { self.advance(); }
                    _ => break,
                }
            }
        } else if *has_double {
            // "double" can be followed by "long" and "_Complex"
            loop {
                match self.peek() {
                    TokenKind::Long => { self.advance(); *long_count += 1; }
                    TokenKind::Complex => { self.advance(); *has_complex = true; }
                    TokenKind::Const | TokenKind::Volatile | TokenKind::Restrict => { self.advance(); }
                    _ => break,
                }
            }
        }
    }

    /// Resolve the collected type specifier flags into a concrete TypeSpecifier.
    fn resolve_type_flags(
        &mut self,
        has_void: bool, has_bool: bool, has_float: bool, has_double: bool,
        has_complex: bool, has_char: bool, has_short: bool, _has_int: bool,
        has_unsigned: bool, _has_signed: bool, has_struct: bool, has_union: bool,
        has_enum: bool, has_typeof: bool, long_count: u32,
        typedef_name: Option<String>,
    ) -> TypeSpecifier {
        if has_void {
            TypeSpecifier::Void
        } else if has_bool {
            TypeSpecifier::Bool
        } else if has_float {
            if has_complex { TypeSpecifier::ComplexFloat } else { TypeSpecifier::Float }
        } else if has_double {
            if has_complex {
                if long_count > 0 { TypeSpecifier::ComplexLongDouble } else { TypeSpecifier::ComplexDouble }
            } else if long_count > 0 {
                TypeSpecifier::LongDouble
            } else {
                TypeSpecifier::Double
            }
        } else if has_complex && !has_struct && !has_union && !has_enum {
            // standalone _Complex defaults to _Complex double
            TypeSpecifier::ComplexDouble
        } else if has_struct {
            self.parse_struct_or_union(true)
        } else if has_union {
            self.parse_struct_or_union(false)
        } else if has_enum {
            self.parse_enum_specifier()
        } else if has_typeof {
            self.parse_typeof_specifier()
        } else if let Some(name) = typedef_name {
            TypeSpecifier::TypedefName(name)
        } else if has_char {
            if has_unsigned { TypeSpecifier::UnsignedChar } else { TypeSpecifier::Char }
        } else if has_short {
            if has_unsigned { TypeSpecifier::UnsignedShort } else { TypeSpecifier::Short }
        } else if long_count >= 2 {
            if has_unsigned { TypeSpecifier::UnsignedLongLong } else { TypeSpecifier::LongLong }
        } else if long_count == 1 {
            if has_unsigned { TypeSpecifier::UnsignedLong } else { TypeSpecifier::Long }
        } else if has_unsigned {
            TypeSpecifier::UnsignedInt
        } else {
            // signed, int, or signed int
            TypeSpecifier::Int
        }
    }

    /// Parse a struct or union definition/reference.
    fn parse_struct_or_union(&mut self, is_struct: bool) -> TypeSpecifier {
        let (mut is_packed, _aligned) = self.parse_gcc_attributes();
        let name = if let TokenKind::Identifier(n) = self.peek().clone() {
            self.advance();
            Some(n)
        } else {
            None
        };
        let (packed2, _) = self.parse_gcc_attributes();
        is_packed = is_packed || packed2;
        let fields = if matches!(self.peek(), TokenKind::LBrace) {
            Some(self.parse_struct_fields())
        } else {
            None
        };
        let (packed3, _) = self.parse_gcc_attributes();
        is_packed = is_packed || packed3;
        if is_struct {
            TypeSpecifier::Struct(name, fields, is_packed)
        } else {
            TypeSpecifier::Union(name, fields, is_packed)
        }
    }

    /// Parse an enum definition/reference.
    fn parse_enum_specifier(&mut self) -> TypeSpecifier {
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
    }

    /// Parse typeof(expr) or typeof(type-name).
    fn parse_typeof_specifier(&mut self) -> TypeSpecifier {
        self.expect(&TokenKind::LParen);
        let save = self.pos;
        // Try parsing as a type first
        if self.is_type_specifier() {
            if let Some(ts) = self.parse_type_specifier() {
                let result_type = self.parse_abstract_declarator_suffix(ts);
                if matches!(self.peek(), TokenKind::RParen) {
                    self.advance();
                    return TypeSpecifier::TypeofType(Box::new(result_type));
                }
            }
            // Didn't work as type, backtrack
            self.pos = save;
            self.expect(&TokenKind::LParen);
        }
        // Parse as expression
        let expr = self.parse_expr();
        self.expect(&TokenKind::RParen);
        TypeSpecifier::Typeof(Box::new(expr))
    }

    /// Consume trailing qualifiers and _Complex that may follow a resolved base type.
    /// C allows "int static x;" and "double _Complex".
    fn consume_trailing_qualifiers(&mut self, mut base: TypeSpecifier) -> TypeSpecifier {
        loop {
            match self.peek() {
                TokenKind::Complex => {
                    self.advance();
                    base = match base {
                        TypeSpecifier::Float => TypeSpecifier::ComplexFloat,
                        TypeSpecifier::Double => TypeSpecifier::ComplexDouble,
                        TypeSpecifier::LongDouble => TypeSpecifier::ComplexLongDouble,
                        _ => TypeSpecifier::ComplexDouble,
                    };
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
        base
    }

    /// Parse struct or union field declarations inside braces.
    pub(super) fn parse_struct_fields(&mut self) -> Vec<StructFieldDecl> {
        let mut fields = Vec::new();
        self.expect(&TokenKind::LBrace);
        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            self.skip_gcc_extensions();
            if let Some(type_spec) = self.parse_type_specifier() {
                if matches!(self.peek(), TokenKind::Semicolon) {
                    // Anonymous field (e.g., anonymous struct/union)
                    fields.push(StructFieldDecl { type_spec, name: None, bit_width: None });
                } else {
                    self.parse_struct_field_declarators(&type_spec, &mut fields);
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

    /// Parse one or more declarators for a struct field.
    fn parse_struct_field_declarators(
        &mut self,
        type_spec: &TypeSpecifier,
        fields: &mut Vec<StructFieldDecl>,
    ) {
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
                    self.skip_balanced_parens();
                    // Function pointer is treated as a pointer type
                    let field_type = TypeSpecifier::Pointer(Box::new(type_spec.clone()));
                    fields.push(StructFieldDecl { type_spec: field_type, name, bit_width: None });
                    if !self.consume_if(&TokenKind::Comma) { break; }
                    continue;
                } else if let TokenKind::Identifier(_) = self.peek() {
                    // Parenthesized field name
                    let name = if let TokenKind::Identifier(n) = self.peek().clone() {
                        self.advance();
                        Some(n)
                    } else {
                        None
                    };
                    self.expect(&TokenKind::RParen);
                    let ft = self.parse_array_dimensions_wrapping(type_spec.clone());
                    let bit_width = if self.consume_if(&TokenKind::Colon) {
                        Some(Box::new(self.parse_expr()))
                    } else {
                        None
                    };
                    fields.push(StructFieldDecl { type_spec: ft, name, bit_width });
                    if !self.consume_if(&TokenKind::Comma) { break; }
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
                    if !self.consume_if(&TokenKind::Comma) { break; }
                    continue;
                } else {
                    self.pos = save; // restore, not a recognized pattern
                }
            }
            // Parse pointer declarators: each * may be followed by
            // cv-qualifiers (const/volatile/restrict), e.g. const char *const *p
            let mut field_type = type_spec.clone();
            while self.consume_if(&TokenKind::Star) {
                field_type = TypeSpecifier::Pointer(Box::new(field_type));
                self.skip_cv_qualifiers();
            }
            // After consuming pointer stars, check for function pointer declarator
            // with pointer return type, e.g. void *(*func)(void) or int *(*f)(int).
            // At this point field_type = Pointer(base_type), next token is '(' from '(*name)(...)'
            if matches!(self.peek(), TokenKind::LParen) {
                let save2 = self.pos;
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
                    self.skip_balanced_parens();
                    // Function pointer with pointer return type
                    fields.push(StructFieldDecl { type_spec: field_type, name, bit_width: None });
                    if !self.consume_if(&TokenKind::Comma) { break; }
                    continue;
                } else {
                    self.pos = save2; // restore
                }
            }
            let name = if let TokenKind::Identifier(n) = self.peek().clone() {
                self.advance();
                Some(n)
            } else {
                None
            };
            field_type = self.parse_array_dimensions_wrapping(field_type);
            let bit_width = if self.consume_if(&TokenKind::Colon) {
                Some(Box::new(self.parse_expr()))
            } else {
                None
            };
            fields.push(StructFieldDecl { type_spec: field_type, name, bit_width });
            if !self.consume_if(&TokenKind::Comma) { break; }
        }
    }

    /// Parse array dimensions and wrap a type with them in reverse order.
    /// For T field[N][M], produces Array(Array(T, M), N).
    pub(super) fn parse_array_dimensions_wrapping(&mut self, mut base_type: TypeSpecifier) -> TypeSpecifier {
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
            base_type = TypeSpecifier::Array(Box::new(base_type), dim);
        }
        base_type
    }

    /// Parse enum variant declarations inside braces.
    pub(super) fn parse_enum_variants(&mut self) -> Vec<EnumVariant> {
        let mut variants = Vec::new();
        self.expect(&TokenKind::LBrace);
        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            if let TokenKind::Identifier(name) = self.peek().clone() {
                self.advance();
                let value = if self.consume_if(&TokenKind::Assign) {
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

    /// Parse a type-name for __builtin_va_arg: type-specifier + abstract declarator.
    pub(super) fn parse_va_arg_type(&mut self) -> TypeSpecifier {
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

    /// Parse an abstract declarator suffix: pointer(s), parenthesized pointer groups,
    /// and array dimensions after a type name. Used by cast expressions, sizeof,
    /// typeof, and _Alignof to avoid duplicating this logic.
    ///
    /// Input: base type already parsed.
    /// Output: type wrapped with pointer/array/function-pointer modifiers.
    pub(super) fn parse_abstract_declarator_suffix(&mut self, mut result_type: TypeSpecifier) -> TypeSpecifier {
        // Parse leading pointer(s)
        while self.consume_if(&TokenKind::Star) {
            result_type = TypeSpecifier::Pointer(Box::new(result_type));
            self.skip_cv_qualifiers();
        }
        // Handle parenthesized abstract declarators: (*), (*)(params), (*)[N]
        if matches!(self.peek(), TokenKind::LParen) {
            let save = self.pos;
            if let Some(ptr_depth) = self.try_parse_paren_abstract_declarator() {
                if matches!(self.peek(), TokenKind::LParen) {
                    // Function pointer cast: (*)(params)
                    self.skip_balanced_parens();
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
                    for _ in 0..ptr_depth {
                        result_type = TypeSpecifier::Pointer(Box::new(result_type));
                    }
                }
            } else {
                self.pos = save;
            }
        }
        // Parse trailing array dimensions
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
    }
}
