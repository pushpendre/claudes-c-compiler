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
        let mut has_mode_ti = false;
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
                // Thread-local storage class (__thread / _Thread_local)
                TokenKind::ThreadLocal => {
                    self.advance();
                    // Treat as regular storage for now (TLS not yet implemented)
                }
                // _Complex modifier
                TokenKind::Complex => {
                    self.advance();
                    has_complex = true;
                    any_base_specifier = true;
                }
                // GNU extensions
                TokenKind::Attribute => {
                    let (_, aligned, mode_ti, _) = self.parse_gcc_attributes();
                    has_mode_ti = has_mode_ti || mode_ti;
                    if let Some(a) = aligned {
                        self.parsed_alignas = Some(self.parsed_alignas.map_or(a, |prev| prev.max(a)));
                    }
                    // parse_gcc_attributes already sets self.parsing_constructor/destructor
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
                    if let Some(align) = self.parse_alignas_argument() {
                        self.parsed_alignas = Some(self.parsed_alignas.map_or(align, |prev| prev.max(align)));
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
                // __int128 can combine with signed/unsigned
                TokenKind::Int128 => {
                    self.advance(); any_base_specifier = true;
                    // __int128 already implies signed unless unsigned is present
                    if has_unsigned {
                        return Some(TypeSpecifier::UnsignedInt128);
                    } else {
                        return Some(TypeSpecifier::Int128);
                    }
                }
                // __uint128_t is always unsigned
                TokenKind::UInt128 => {
                    self.advance(); any_base_specifier = true;
                    return Some(TypeSpecifier::UnsignedInt128);
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
            &mut has_complex, &mut has_mode_ti,
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

        // Apply __attribute__((mode(TI))): transform type to 128-bit
        let base = if has_mode_ti {
            match base {
                TypeSpecifier::Int | TypeSpecifier::Long | TypeSpecifier::LongLong
                | TypeSpecifier::Signed => TypeSpecifier::Int128,
                TypeSpecifier::UnsignedInt | TypeSpecifier::UnsignedLong
                | TypeSpecifier::UnsignedLongLong | TypeSpecifier::Unsigned => TypeSpecifier::UnsignedInt128,
                other => other,
            }
        } else {
            base
        };

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
        has_mode_ti: &mut bool,
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
                    TokenKind::Register | TokenKind::Noreturn | TokenKind::ThreadLocal => { self.advance(); }
                    TokenKind::Inline => { self.advance(); self.parsing_inline = true; }
                    TokenKind::Attribute => {
                        let (_, _, mode_ti, _) = self.parse_gcc_attributes();
                        *has_mode_ti = *has_mode_ti || mode_ti;
                        // parse_gcc_attributes already sets self.parsing_constructor/destructor
                    }
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
        let (mut is_packed, mut struct_aligned, _, _) = self.parse_gcc_attributes();
        let name = if let TokenKind::Identifier(n) = self.peek().clone() {
            self.advance();
            Some(n)
        } else {
            None
        };
        let (packed2, aligned2, _, _) = self.parse_gcc_attributes();
        is_packed = is_packed || packed2;
        if aligned2.is_some() { struct_aligned = aligned2; }
        let fields = if matches!(self.peek(), TokenKind::LBrace) {
            Some(self.parse_struct_fields())
        } else {
            None
        };
        let (packed3, aligned3, _, _) = self.parse_gcc_attributes();
        is_packed = is_packed || packed3;
        if aligned3.is_some() { struct_aligned = aligned3; }
        // Apply current #pragma pack alignment to struct definition
        let max_field_align = self.pragma_pack_align;
        if is_struct {
            TypeSpecifier::Struct(name, fields, is_packed, max_field_align, struct_aligned)
        } else {
            TypeSpecifier::Union(name, fields, is_packed, max_field_align, struct_aligned)
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
                TokenKind::Register | TokenKind::Noreturn | TokenKind::ThreadLocal => {
                    self.advance();
                }
                TokenKind::Inline => {
                    self.advance();
                    self.parsing_inline = true;
                }
                TokenKind::Attribute => {
                    self.advance();
                    let (ctor, dtor) = self.parse_attribute_params();
                    if ctor { self.parsing_constructor = true; }
                    if dtor { self.parsing_destructor = true; }
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
            if matches!(self.peek(), TokenKind::Semicolon) {
                self.advance();
                continue;
            }
            if let Some(type_spec) = self.parse_type_specifier() {
                if matches!(self.peek(), TokenKind::Semicolon) {
                    // Anonymous field (e.g., anonymous struct/union)
                    let alignment = self.parsed_alignas.take();
                    fields.push(StructFieldDecl { type_spec, name: None, bit_width: None, derived: Vec::new(), alignment });
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

    /// Parse one or more declarators for a struct field using the general
    /// declarator parser. This correctly handles complex declarators like
    /// function pointers returning function pointers (e.g.,
    /// `void (*(*xDlSym)(sqlite3_vfs*, void*, const char *))(void)`).
    fn parse_struct_field_declarators(
        &mut self,
        type_spec: &TypeSpecifier,
        fields: &mut Vec<StructFieldDecl>,
    ) {
        // Capture _Alignas value that was parsed during type specifier parsing
        let mut alignas_from_type = self.parsed_alignas.take();

        // Consume post-type qualifiers that may appear between type and declarator
        // in struct field declarations: e.g., "char _Alignas(32) c;"
        self.consume_struct_field_qualifiers(&mut alignas_from_type);

        loop {
            // Handle unnamed bitfield: `: constant-expr`
            if matches!(self.peek(), TokenKind::Colon) {
                self.advance();
                // Use parse_assignment_expr to avoid consuming comma as comma-operator.
                // Bitfield width is a constant expression (no comma operator at top level).
                let bit_width = Some(Box::new(self.parse_assignment_expr()));
                fields.push(StructFieldDecl {
                    type_spec: type_spec.clone(),
                    name: None,
                    bit_width,
                    derived: Vec::new(),
                    alignment: alignas_from_type,
                });
                if !self.consume_if(&TokenKind::Comma) { break; }
                continue;
            }

            // Use the general-purpose declarator parser. This handles all cases:
            // simple pointers, arrays, function pointers, and nested function
            // pointer declarators of arbitrary depth.
            // parse_declarator_with_attrs also consumes trailing __attribute__ and
            // returns the aligned value from __attribute__((aligned(N))).
            let (name, derived, _, _, decl_aligned) = self.parse_declarator_with_attrs();

            // Parse optional bitfield width (constant-expression, not full expr with comma)
            let bit_width = if self.consume_if(&TokenKind::Colon) {
                Some(Box::new(self.parse_assignment_expr()))
            } else {
                None
            };

            // Parse any additional trailing GCC __attribute__ (e.g., after bitfield width)
            let (_, extra_aligned, _, _) = self.parse_gcc_attributes();

            // Combine alignment sources: explicit attribute on declarator,
            // extra attribute after bitfield, or _Alignas from type specifier
            let alignment = decl_aligned.or(extra_aligned).or(alignas_from_type);

            // For backward compatibility with downstream code that reads type_spec
            // directly, fold simple derived declarators (pointers, arrays) into
            // type_spec. Only use the derived field for complex cases with function
            // pointers that require build_full_ctype().
            let (field_type, field_derived) = Self::fold_simple_derived(type_spec, &derived);

            fields.push(StructFieldDecl {
                type_spec: field_type,
                name,
                bit_width,
                derived: field_derived,
                alignment,
            });

            if !self.consume_if(&TokenKind::Comma) { break; }
        }
    }

    /// Consume qualifiers that may appear between type specifier and declarator
    /// in struct field declarations. Handles _Alignas, const, volatile, etc.
    fn consume_struct_field_qualifiers(&mut self, alignas: &mut Option<usize>) {
        loop {
            match self.peek() {
                TokenKind::Const | TokenKind::Volatile | TokenKind::Restrict => {
                    self.advance();
                }
                TokenKind::Alignas => {
                    self.advance();
                    if let Some(align) = self.parse_alignas_argument() {
                        *alignas = Some(alignas.map_or(align, |prev| prev.max(align)));
                    }
                }
                TokenKind::Attribute => {
                    let (_, attr_aligned, _, _) = self.parse_gcc_attributes();
                    if attr_aligned.is_some() {
                        *alignas = attr_aligned;
                    }
                }
                TokenKind::Extension => { self.advance(); }
                _ => break,
            }
        }
    }

    /// For simple derived declarators (just pointers and/or arrays), fold them
    /// into the TypeSpecifier directly to maintain backward compatibility with
    /// downstream code. For complex cases (function pointers), return the
    /// derived list for downstream to process with build_full_ctype().
    fn fold_simple_derived(base: &TypeSpecifier, derived: &[DerivedDeclarator]) -> (TypeSpecifier, Vec<DerivedDeclarator>) {
        // If derived contains any function-related declarators, pass it through
        let has_function = derived.iter().any(|d| matches!(d,
            DerivedDeclarator::Function(_, _) | DerivedDeclarator::FunctionPointer(_, _)));

        if has_function {
            return (base.clone(), derived.to_vec());
        }

        if derived.is_empty() {
            return (base.clone(), Vec::new());
        }

        // Simple case: only Pointer and Array declarators. Fold into type_spec.
        let mut result = base.clone();
        let mut i = 0;
        while i < derived.len() {
            match &derived[i] {
                DerivedDeclarator::Pointer => {
                    result = TypeSpecifier::Pointer(Box::new(result));
                    i += 1;
                }
                DerivedDeclarator::Array(_) => {
                    // Collect consecutive array dims, apply in reverse (innermost first)
                    let start = i;
                    while i < derived.len() && matches!(&derived[i], DerivedDeclarator::Array(_)) {
                        i += 1;
                    }
                    for j in (start..i).rev() {
                        if let DerivedDeclarator::Array(size_expr) = &derived[j] {
                            result = TypeSpecifier::Array(Box::new(result), size_expr.clone());
                        }
                    }
                }
                _ => { i += 1; }
            }
        }
        (result, Vec::new())
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
            eprintln!("error: expected type in __builtin_va_arg");
            self.error_count += 1;
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
