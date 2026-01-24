// Declaration parsing: external (top-level) and local (block-scope) declarations.
//
// External declarations handle both function definitions and variable/type
// declarations. The key challenge is disambiguating function definitions
// (which have a body) from declarations (which end with ';').
//
// K&R-style function parameters are also handled here, where parameter types
// are declared separately after the parameter name list.

use crate::frontend::lexer::token::TokenKind;
use super::ast::*;
use super::parser::Parser;

impl Parser {
    pub(super) fn parse_external_decl(&mut self) -> Option<ExternalDecl> {
        // Reset storage class and attribute flags before parsing
        self.parsing_typedef = false;
        self.parsing_static = false;
        self.parsing_extern = false;
        self.parsing_inline = false;
        self.parsing_const = false;
        self.parsing_constructor = false;
        self.parsing_destructor = false;

        self.skip_gcc_extensions();

        // Handle #pragma pack directives (emitted as synthetic tokens by preprocessor)
        while self.handle_pragma_pack_token() {
            // Consume semicolons after pragma pack synthetic tokens
            self.consume_if(&TokenKind::Semicolon);
        }

        if self.at_eof() {
            return None;
        }

        // Handle top-level asm("..."); directives
        if matches!(self.peek(), TokenKind::Asm) {
            self.advance();
            self.consume_if(&TokenKind::Volatile);
            if matches!(self.peek(), TokenKind::LParen) {
                self.skip_balanced_parens();
            }
            self.consume_if(&TokenKind::Semicolon);
            return Some(ExternalDecl::Declaration(Declaration::empty()));
        }

        // Handle _Static_assert at file scope
        if matches!(self.peek(), TokenKind::StaticAssert) {
            self.advance();
            self.skip_balanced_parens();
            self.consume_if(&TokenKind::Semicolon);
            return Some(ExternalDecl::Declaration(Declaration::empty()));
        }

        let start = self.peek_span();
        let type_spec = self.parse_type_specifier()?;

        // Capture constructor/destructor from type-level attributes
        let type_level_ctor = self.parsing_constructor;
        let type_level_dtor = self.parsing_destructor;

        // Bare type with no declarator (e.g., struct definition)
        if self.at_eof() || matches!(self.peek(), TokenKind::Semicolon) {
            self.consume_if(&TokenKind::Semicolon);
            return Some(ExternalDecl::Declaration(Declaration {
                type_spec,
                declarators: Vec::new(),
                is_static: self.parsing_static,
                is_extern: self.parsing_extern,
                is_typedef: self.parsing_typedef,
                is_const: self.parsing_const,
                is_common: false,
                span: start,
            }));
        }

        // Handle post-type storage class specifiers (C allows "struct S typedef name;")
        self.consume_post_type_qualifiers();

        let (name, derived, decl_mode_ti, decl_common) = self.parse_declarator_with_attrs();
        let (post_ctor, post_dtor, post_mode_ti, post_common) = self.parse_asm_and_attributes();
        let mode_ti = decl_mode_ti || post_mode_ti;
        let is_common = decl_common || post_common;
        // Merge all sources of constructor/destructor: type-level attrs, declarator-level attrs, post-declarator attrs
        let is_constructor = type_level_ctor || self.parsing_constructor || post_ctor;
        let is_destructor = type_level_dtor || self.parsing_destructor || post_dtor;

        // Apply __attribute__((mode(TI))): transform type to 128-bit
        let type_spec = if mode_ti {
            Self::apply_mode_ti(type_spec)
        } else {
            type_spec
        };

        // Determine if this is a function definition
        let is_funcdef = !derived.is_empty()
            && matches!(derived.last(), Some(DerivedDeclarator::Function(_, _)))
            && (matches!(self.peek(), TokenKind::LBrace) || self.is_type_specifier());

        if is_funcdef {
            self.parse_function_def(type_spec, name, derived, start, is_constructor, is_destructor)
        } else {
            self.parse_declaration_rest(type_spec, name, derived, start, is_constructor, is_destructor, is_common)
        }
    }

    /// Parse the rest of a function definition after the declarator.
    fn parse_function_def(
        &mut self,
        type_spec: TypeSpecifier,
        name: Option<String>,
        derived: Vec<DerivedDeclarator>,
        start: crate::common::source::Span,
        is_constructor: bool,
        is_destructor: bool,
    ) -> Option<ExternalDecl> {
        self.parsing_typedef = false; // function defs are never typedefs
        let (params, variadic) = if let Some(DerivedDeclarator::Function(p, v)) = derived.last() {
            (p.clone(), *v)
        } else {
            (vec![], false)
        };

        // Handle K&R-style parameter declarations
        let is_kr_style = !matches!(self.peek(), TokenKind::LBrace);
        let final_params = if is_kr_style {
            self.parse_kr_params(params)
        } else {
            params
        };

        let is_static = self.parsing_static;
        let is_inline = self.parsing_inline;

        // Build return type from derived declarators
        let return_type = self.build_return_type(type_spec, &derived);

        // Shadow typedef names used as parameter names
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
            is_constructor,
            is_destructor,
            span: start,
        }))
    }

    /// Build the return type from derived declarators.
    /// For `int (*func())[3]`, we apply post-Function and pre-Function derivations.
    fn build_return_type(
        &self,
        base_type: TypeSpecifier,
        derived: &[DerivedDeclarator],
    ) -> TypeSpecifier {
        let mut return_type = base_type;
        let func_pos = derived.iter().position(|d|
            matches!(d, DerivedDeclarator::Function(_, _)));

        if let Some(fpos) = func_pos {
            // Apply post-Function derivations (Array/Pointer)
            for d in &derived[fpos+1..] {
                match d {
                    DerivedDeclarator::Array(size_expr) => {
                        return_type = TypeSpecifier::Array(
                            Box::new(return_type),
                            size_expr.clone(),
                        );
                    }
                    DerivedDeclarator::Pointer => {
                        return_type = TypeSpecifier::Pointer(Box::new(return_type));
                    }
                    _ => {}
                }
            }
            // Apply pre-Function derivations
            for d in &derived[..fpos] {
                match d {
                    DerivedDeclarator::Pointer => {
                        return_type = TypeSpecifier::Pointer(Box::new(return_type));
                    }
                    DerivedDeclarator::Array(size_expr) => {
                        return_type = TypeSpecifier::Array(
                            Box::new(return_type),
                            size_expr.clone(),
                        );
                    }
                    _ => {}
                }
            }
        } else {
            // No Function in derived - just apply pointer derivations
            for d in derived {
                match d {
                    DerivedDeclarator::Pointer => {
                        return_type = TypeSpecifier::Pointer(Box::new(return_type));
                    }
                    _ => break,
                }
            }
        }
        return_type
    }

    /// Parse K&R-style parameter declarations.
    /// In K&R style, the parameter list is just names, and type declarations follow.
    fn parse_kr_params(&mut self, mut kr_params: Vec<ParamDecl>) -> Vec<ParamDecl> {
        while self.is_type_specifier() && !matches!(self.peek(), TokenKind::LBrace) {
            if let Some(type_spec) = self.parse_type_specifier() {
                loop {
                    let (pname, pderived) = self.parse_declarator();
                    if let Some(ref name) = pname {
                        let full_type = self.apply_kr_derivations(&type_spec, &pderived);
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
    }

    /// Apply derived declarators to build a K&R parameter's full type.
    fn apply_kr_derivations(
        &self,
        type_spec: &TypeSpecifier,
        pderived: &[DerivedDeclarator],
    ) -> TypeSpecifier {
        let mut full_type = type_spec.clone();
        // Apply pointers
        for d in pderived {
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
        // Array params: outermost dimension decays to pointer
        if !array_dims.is_empty() {
            for dim in array_dims.iter().skip(1).rev() {
                full_type = TypeSpecifier::Array(Box::new(full_type), dim.clone());
            }
            full_type = TypeSpecifier::Pointer(Box::new(full_type));
        }
        // Function/FunctionPointer params decay to pointers
        for d in pderived {
            match d {
                DerivedDeclarator::Function(_, _) | DerivedDeclarator::FunctionPointer(_, _) => {
                    full_type = TypeSpecifier::Pointer(Box::new(full_type));
                }
                _ => {}
            }
        }
        full_type
    }

    /// Parse the rest of a declaration (not a function definition).
    fn parse_declaration_rest(
        &mut self,
        type_spec: TypeSpecifier,
        name: Option<String>,
        derived: Vec<DerivedDeclarator>,
        start: crate::common::source::Span,
        is_constructor: bool,
        is_destructor: bool,
        mut is_common: bool,
    ) -> Option<ExternalDecl> {
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
            is_constructor,
            is_destructor,
            span: start,
        });

        let (extra_ctor, extra_dtor, _, extra_common) = self.parse_asm_and_attributes();
        if extra_ctor {
            declarators.last_mut().unwrap().is_constructor = true;
        }
        if extra_dtor {
            declarators.last_mut().unwrap().is_destructor = true;
        }
        is_common = is_common || extra_common;

        // Parse additional declarators separated by commas
        while self.consume_if(&TokenKind::Comma) {
            let (dname, dderived) = self.parse_declarator();
            let (d_ctor, d_dtor, _, d_common) = self.parse_asm_and_attributes();
            is_common = is_common || d_common;
            let dinit = if self.consume_if(&TokenKind::Assign) {
                Some(self.parse_initializer())
            } else {
                None
            };
            declarators.push(InitDeclarator {
                name: dname.unwrap_or_default(),
                derived: dderived,
                init: dinit,
                is_constructor: d_ctor,
                is_destructor: d_dtor,
                span: start,
            });
            self.skip_asm_and_attributes();
        }

        // Register typedef names
        let is_typedef = self.parsing_typedef;
        self.register_typedefs(&declarators);

        self.expect(&TokenKind::Semicolon);
        Some(ExternalDecl::Declaration(Declaration {
            type_spec,
            declarators,
            is_static: self.parsing_static,
            is_extern: self.parsing_extern,
            is_typedef,
            is_const: self.parsing_const,
            is_common,
            span: start,
        }))
    }

    pub(super) fn parse_local_declaration(&mut self) -> Option<Declaration> {
        let start = self.peek_span();
        self.parsing_static = false;
        self.parsing_extern = false;
        self.parsing_typedef = false;
        self.parsing_inline = false;
        self.parsing_const = false;
        let type_spec = self.parse_type_specifier()?;

        self.consume_post_type_qualifiers();

        let is_static = self.parsing_static;
        let is_extern = self.parsing_extern;

        let mut declarators = Vec::new();

        // Handle bare type with semicolon (struct/enum/union definition)
        if matches!(self.peek(), TokenKind::Semicolon) {
            self.advance();
            return Some(Declaration { type_spec, declarators, is_static, is_extern, is_typedef: self.parsing_typedef, is_const: self.parsing_const, is_common: false, span: start });
        }

        let mut mode_ti = false;
        loop {
            let (name, derived, decl_mode_ti, _) = self.parse_declarator_with_attrs();
            mode_ti = decl_mode_ti || self.skip_asm_and_attributes() || mode_ti;
            let init = if self.consume_if(&TokenKind::Assign) {
                Some(self.parse_initializer())
            } else {
                None
            };
            declarators.push(InitDeclarator {
                name: name.unwrap_or_default(),
                derived,
                init,
                is_constructor: false,
                is_destructor: false,
                span: start,
            });
            self.skip_asm_and_attributes();
            if !self.consume_if(&TokenKind::Comma) {
                break;
            }
        }

        // Apply __attribute__((mode(TI))): transform type to 128-bit
        let type_spec = if mode_ti {
            Self::apply_mode_ti(type_spec)
        } else {
            type_spec
        };

        // Register typedef names or shadow them for variable declarations
        let is_typedef = self.parsing_typedef;
        if self.parsing_typedef {
            for decl in &declarators {
                if !decl.name.is_empty() {
                    self.typedefs.push(decl.name.clone());
                    self.shadowed_typedefs.retain(|n| n != &decl.name);
                }
            }
            self.parsing_typedef = false;
        } else {
            for decl in &declarators {
                if !decl.name.is_empty() && self.typedefs.contains(&decl.name) {
                    if !self.shadowed_typedefs.contains(&decl.name) {
                        self.shadowed_typedefs.push(decl.name.clone());
                    }
                }
            }
        }

        self.expect(&TokenKind::Semicolon);
        Some(Declaration { type_spec, declarators, is_static, is_extern, is_typedef, is_const: self.parsing_const, is_common: false, span: start })
    }

    /// Parse an initializer: either a braced initializer list or a single expression.
    pub(super) fn parse_initializer(&mut self) -> Initializer {
        if matches!(self.peek(), TokenKind::LBrace) {
            self.advance();
            let mut items = Vec::new();
            while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
                let mut designators = Vec::new();
                // Parse designators: [idx] and .field
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
                // GNU old-style designator: field: value
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

    /// Consume post-type storage class specifiers and qualifiers.
    /// C allows "struct { int i; } typedef name;" and "char _Alignas(16) x;".
    /// This is shared between parse_external_decl and parse_local_declaration.
    pub(super) fn consume_post_type_qualifiers(&mut self) {
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
    }

    /// Register typedef names from declarators, if parsing_typedef is set.
    fn register_typedefs(&mut self, declarators: &[InitDeclarator]) {
        if self.parsing_typedef {
            for decl in declarators {
                if !decl.name.is_empty() {
                    self.typedefs.push(decl.name.clone());
                }
            }
            self.parsing_typedef = false;
        }
    }

    /// Apply __attribute__((mode(TI))) to a type specifier: promotes to 128-bit integer.
    fn apply_mode_ti(ts: TypeSpecifier) -> TypeSpecifier {
        match ts {
            TypeSpecifier::Int | TypeSpecifier::Long | TypeSpecifier::LongLong
            | TypeSpecifier::Signed => TypeSpecifier::Int128,
            TypeSpecifier::UnsignedInt | TypeSpecifier::UnsignedLong
            | TypeSpecifier::UnsignedLongLong | TypeSpecifier::Unsigned => TypeSpecifier::UnsignedInt128,
            // If already 128-bit or unknown, leave as is
            other => other,
        }
    }
}
