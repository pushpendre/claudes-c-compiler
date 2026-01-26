// Declaration parsing: external (top-level) and local (block-scope) declarations.
//
// External declarations handle both function definitions and variable/type
// declarations. The key challenge is disambiguating function definitions
// (which have a body) from declarations (which end with ';').
//
// K&R-style function parameters are also handled here, where parameter types
// are declared separately after the parameter name list.

use crate::common::fx_hash::FxHashMap;
use crate::common::source::Span;
use crate::common::types::AddressSpace;
use crate::frontend::lexer::token::TokenKind;
use super::ast::*;
use super::parser::{ModeKind, Parser};

impl Parser {
    pub(super) fn parse_external_decl(&mut self) -> Option<ExternalDecl> {
        // Reset storage class and attribute flags before parsing
        self.parsing_typedef = false;
        self.parsing_static = false;
        self.parsing_extern = false;
        self.parsing_inline = false;
        self.parsing_const = false;
        self.parsing_volatile = false;
        self.parsing_constructor = false;
        self.parsing_destructor = false;
        self.parsing_weak = false;
        self.parsing_alias_target = None;
        self.parsing_visibility = None;
        self.parsing_section = None;
        self.parsing_error_attr = false;
        self.parsing_noreturn = false;
        self.parsing_cleanup_fn = None;
        self.parsing_gnu_inline = false;
        self.parsing_always_inline = false;
        self.parsing_noinline = false;
        self.parsing_address_space = AddressSpace::Default;

        self.skip_gcc_extensions();

        // Handle #pragma pack directives (emitted as synthetic tokens by preprocessor)
        while self.handle_pragma_pack_token() {
            // Consume semicolons after pragma pack synthetic tokens
            self.consume_if(&TokenKind::Semicolon);
        }

        // Handle #pragma GCC visibility push/pop directives
        while self.handle_pragma_visibility_token() {
            self.consume_if(&TokenKind::Semicolon);
        }

        if self.at_eof() {
            return None;
        }

        // Handle top-level asm("..."); directives - emit verbatim in assembly output
        if matches!(self.peek(), TokenKind::Asm) {
            self.advance();
            self.consume_if(&TokenKind::Volatile);
            if matches!(self.peek(), TokenKind::LParen) {
                self.advance(); // consume (
                // Collect all string literal pieces (may be concatenated)
                let mut asm_str = String::new();
                loop {
                    match self.peek() {
                        TokenKind::StringLiteral(s) => {
                            asm_str.push_str(&s);
                            self.advance();
                        }
                        TokenKind::RParen | TokenKind::Eof => break,
                        _ => { self.advance(); }
                    }
                }
                if matches!(self.peek(), TokenKind::RParen) {
                    self.advance();
                }
                self.consume_if(&TokenKind::Semicolon);
                if !asm_str.is_empty() {
                    return Some(ExternalDecl::TopLevelAsm(asm_str));
                }
                return Some(ExternalDecl::Declaration(Declaration::empty()));
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
                is_volatile: self.parsing_volatile,
                is_common: false,
                is_transparent_union: false,
                alignment: None,
                alignas_type: None,
                alignment_sizeof_type: None,
                address_space: self.parsing_address_space,
                span: start,
            }));
        }

        // Handle post-type storage class specifiers (C allows "struct S typedef name;")
        self.consume_post_type_qualifiers();

        let (name, derived, decl_mode, decl_common, decl_aligned) = self.parse_declarator_with_attrs();
        let (post_ctor, post_dtor, post_mode, post_common, post_aligned, first_asm_reg) = self.parse_asm_and_attributes();
        let mode_kind = decl_mode.or(post_mode);
        let is_common = decl_common || post_common;
        // Merge alignment from _Alignas (type specifier), declarator attrs, and post-declarator attrs.
        // Per C11, alignment can only increase (6.7.5), so take the maximum.
        let mut merged_alignment = self.parsed_alignas.take();
        let alignas_type = self.parsed_alignas_type.take();
        let alignment_sizeof_type = self.parsed_alignment_sizeof_type.take();
        for a in [decl_aligned, post_aligned].iter().copied().flatten() {
            merged_alignment = Some(merged_alignment.map_or(a, |prev| prev.max(a)));
        }
        // Merge all sources of constructor/destructor: type-level attrs, declarator-level attrs, post-declarator attrs
        let is_constructor = type_level_ctor || self.parsing_constructor || post_ctor;
        let is_destructor = type_level_dtor || self.parsing_destructor || post_dtor;
        // Capture alias/weak/visibility/section/error attributes
        let is_weak = self.parsing_weak;
        let alias_target = self.parsing_alias_target.take();
        // Use explicit __attribute__((visibility(...))) if present, otherwise fall back
        // to the current #pragma GCC visibility default (if any).
        let visibility = self.parsing_visibility.take()
            .or_else(|| self.pragma_default_visibility.clone());
        let section = self.parsing_section.take();
        let is_error_attr = self.parsing_error_attr;
        let is_noreturn = self.parsing_noreturn;
        let cleanup_fn = self.parsing_cleanup_fn.take();

        // Apply __attribute__((mode(...))): transform type to specified bit-width
        let type_spec = if let Some(mk) = mode_kind {
            mk.apply(type_spec)
        } else {
            type_spec
        };

        // Determine if this is a function definition
        let is_funcdef = !derived.is_empty()
            && matches!(derived.last(), Some(DerivedDeclarator::Function(_, _)))
            && (matches!(self.peek(), TokenKind::LBrace) || self.is_type_specifier());

        if is_funcdef {
            self.parse_function_def(type_spec, name, derived, start, is_constructor, is_destructor, section, visibility, is_weak)
        } else {
            self.parse_declaration_rest(type_spec, name, derived, start, is_constructor, is_destructor, is_common, merged_alignment, alignas_type, alignment_sizeof_type, is_weak, alias_target, visibility, section, first_asm_reg, is_error_attr, is_noreturn, cleanup_fn)
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
        section: Option<String>,
        visibility: Option<String>,
        is_weak: bool,
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
        let is_extern = self.parsing_extern;
        let is_gnu_inline = self.parsing_gnu_inline;
        let is_always_inline = self.parsing_always_inline;
        let is_noinline = self.parsing_noinline;

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
            is_extern,
            is_gnu_inline,
            is_always_inline,
            is_noinline,
            is_kr: is_kr_style,
            is_constructor,
            is_destructor,
            section,
            visibility,
            is_weak,
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
                        return_type = TypeSpecifier::Pointer(Box::new(return_type), AddressSpace::Default);
                    }
                    _ => {}
                }
            }
            // Apply pre-Function derivations
            for d in &derived[..fpos] {
                match d {
                    DerivedDeclarator::Pointer => {
                        return_type = TypeSpecifier::Pointer(Box::new(return_type), AddressSpace::Default);
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
                        return_type = TypeSpecifier::Pointer(Box::new(return_type), AddressSpace::Default);
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
                        let (full_type, fptr_params) = self.apply_kr_derivations(&type_spec, &pderived);
                        for param in kr_params.iter_mut() {
                            if param.name.as_deref() == Some(name.as_str()) {
                                param.type_spec = full_type.clone();
                                param.fptr_params = fptr_params.clone();
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
    /// Returns (type_specifier, optional_fptr_params).
    /// For function pointer parameters like `int (*fp)(int, int)`, this returns
    /// (Pointer(Int), Some([ParamDecl for int, ParamDecl for int])) to match
    /// the modern-style parameter handling path.
    fn apply_kr_derivations(
        &self,
        type_spec: &TypeSpecifier,
        pderived: &[DerivedDeclarator],
    ) -> (TypeSpecifier, Option<Vec<ParamDecl>>) {
        let mut full_type = type_spec.clone();

        // Check if this is a function pointer parameter.
        // For `int (*fp)(int)`, pderived = [Pointer, FunctionPointer([int], false)]
        // The Pointer is the syntax marker from `(*fp)`, not a real pointer level.
        // We need to: (1) not apply the syntax-marker Pointer, (2) extract the
        // FunctionPointer params, and (3) apply one Pointer wrap for decay.
        let fptr_info = pderived.iter().find_map(|d| {
            if let DerivedDeclarator::FunctionPointer(params, variadic) = d {
                Some((params.clone(), *variadic))
            } else {
                None
            }
        });

        if let Some((fptr_params, _variadic)) = fptr_info {
            // Function pointer parameter in K&R style.
            // Count how many Pointer derivations exist. For `int (*fp)()`:
            //   pderived = [Pointer, FunctionPointer([], false)]
            //   One Pointer is the syntax marker - skip it.
            //   Any additional Pointers are return-type pointer levels.
            // For `int *(*fp)()`:
            //   pderived = [Pointer, Pointer, FunctionPointer([], false)]
            //   First Pointer is return-type pointer, second is syntax marker.
            let ptr_count = pderived.iter().filter(|d| matches!(d, DerivedDeclarator::Pointer)).count();
            // Apply all pointers except the syntax marker (last one)
            for _ in 0..ptr_count.saturating_sub(1) {
                full_type = TypeSpecifier::Pointer(Box::new(full_type), AddressSpace::Default);
            }
            // Apply one Pointer wrapping for the function-pointer-to-pointer decay
            full_type = TypeSpecifier::Pointer(Box::new(full_type), AddressSpace::Default);
            return (full_type, Some(fptr_params));
        }

        // Not a function pointer - apply all derivations normally.
        // Apply pointers
        for d in pderived {
            if let DerivedDeclarator::Pointer = d {
                full_type = TypeSpecifier::Pointer(Box::new(full_type), AddressSpace::Default);
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
            full_type = TypeSpecifier::Pointer(Box::new(full_type), AddressSpace::Default);
        }
        // Function params (bare function names) decay to pointers
        for d in pderived {
            if let DerivedDeclarator::Function(_, _) = d {
                full_type = TypeSpecifier::Pointer(Box::new(full_type), AddressSpace::Default);
            }
        }
        (full_type, None)
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
        mut alignment: Option<usize>,
        alignas_type: Option<TypeSpecifier>,
        alignment_sizeof_type: Option<TypeSpecifier>,
        is_weak: bool,
        alias_target: Option<String>,
        visibility: Option<String>,
        section: Option<String>,
        first_asm_register: Option<String>,
        is_error_attr: bool,
        is_noreturn: bool,
        cleanup_fn: Option<String>,
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
            is_weak,
            alias_target,
            visibility,
            section: section.clone(),
            asm_register: first_asm_register,
            is_error_attr,
            is_noreturn,
            cleanup_fn,
            span: start,
        });

        let (extra_ctor, extra_dtor, _, extra_common, extra_aligned, extra_asm_reg) = self.parse_asm_and_attributes();
        // If the asm register was parsed after the declarator (post-declarator position),
        // apply it to the first declarator
        if let Some(reg) = extra_asm_reg {
            declarators.last_mut().unwrap().asm_register = Some(reg);
        }
        if extra_ctor {
            declarators.last_mut().unwrap().is_constructor = true;
        }
        if extra_dtor {
            declarators.last_mut().unwrap().is_destructor = true;
        }
        // Merge weak/alias/visibility from post-declarator attributes
        if self.parsing_weak {
            declarators.last_mut().unwrap().is_weak = true;
        }
        if let Some(ref target) = self.parsing_alias_target {
            declarators.last_mut().unwrap().alias_target = Some(target.clone());
        }
        if let Some(ref vis) = self.parsing_visibility {
            declarators.last_mut().unwrap().visibility = Some(vis.clone());
        }
        if let Some(ref sect) = self.parsing_section {
            declarators.last_mut().unwrap().section = Some(sect.clone());
        }
        if let Some(ref cleanup) = self.parsing_cleanup_fn {
            declarators.last_mut().unwrap().cleanup_fn = Some(cleanup.clone());
        }
        self.parsing_weak = false;
        self.parsing_alias_target = None;
        self.parsing_visibility = None;
        self.parsing_section = None;
        self.parsing_error_attr = false;
        self.parsing_noreturn = false;
        self.parsing_cleanup_fn = None;
        is_common = is_common || extra_common;
        if let Some(a) = extra_aligned {
            alignment = Some(alignment.map_or(a, |prev| prev.max(a)));
        }

        // Parse additional declarators separated by commas
        while self.consume_if(&TokenKind::Comma) {
            let (dname, dderived) = self.parse_declarator();
            let (d_ctor, d_dtor, _, d_common, _, d_asm_reg) = self.parse_asm_and_attributes();
            is_common = is_common || d_common;
            let d_weak = self.parsing_weak;
            let d_alias = self.parsing_alias_target.take();
            let d_vis = self.parsing_visibility.take()
                .or_else(|| self.pragma_default_visibility.clone());
            // Per-declarator section attribute overrides, otherwise inherit from
            // the declaration-level attribute (e.g. __attribute__((section(".x"))) int a, b;)
            let d_section = self.parsing_section.take().or_else(|| section.clone());
            let d_cleanup_fn = self.parsing_cleanup_fn.take();
            self.parsing_weak = false;
            let dinit = if self.consume_if(&TokenKind::Assign) {
                Some(self.parse_initializer())
            } else {
                None
            };
            let d_error_attr = self.parsing_error_attr;
            let d_noreturn = self.parsing_noreturn;
            declarators.push(InitDeclarator {
                name: dname.unwrap_or_default(),
                derived: dderived,
                init: dinit,
                is_constructor: d_ctor,
                is_destructor: d_dtor,
                is_weak: d_weak,
                alias_target: d_alias,
                visibility: d_vis,
                section: d_section,
                asm_register: d_asm_reg,
                is_error_attr: d_error_attr,
                is_noreturn: d_noreturn,
                cleanup_fn: d_cleanup_fn,
                span: start,
            });
            let (_, skip_aligned, skip_asm_reg) = self.skip_asm_and_attributes();
            if let Some(reg) = skip_asm_reg {
                declarators.last_mut().unwrap().asm_register = Some(reg);
            }
            if let Some(a) = skip_aligned {
                alignment = Some(alignment.map_or(a, |prev| prev.max(a)));
            }
        }

        // Register typedef names
        let is_typedef = self.parsing_typedef;
        let is_transparent_union = self.parsing_transparent_union;
        self.parsing_transparent_union = false;
        self.register_typedefs(&declarators);

        self.expect(&TokenKind::Semicolon);
        Some(ExternalDecl::Declaration(Declaration {
            type_spec,
            declarators,
            is_static: self.parsing_static,
            is_extern: self.parsing_extern,
            is_typedef,
            is_const: self.parsing_const,
            is_volatile: self.parsing_volatile,
            is_common,
            is_transparent_union,
            alignment,
            alignas_type,
            alignment_sizeof_type,
            address_space: self.parsing_address_space,
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
        self.parsing_volatile = false;
        self.parsing_address_space = AddressSpace::Default;
        let type_spec = self.parse_type_specifier()?;

        self.consume_post_type_qualifiers();

        let is_static = self.parsing_static;
        let is_extern = self.parsing_extern;

        let mut declarators = Vec::new();

        // Handle bare type with semicolon (struct/enum/union definition)
        if matches!(self.peek(), TokenKind::Semicolon) {
            self.advance();
            return Some(Declaration { type_spec, declarators, is_static, is_extern, is_typedef: self.parsing_typedef, is_const: self.parsing_const, is_volatile: self.parsing_volatile, is_common: false, is_transparent_union: false, alignment: None, alignas_type: None, alignment_sizeof_type: None, address_space: self.parsing_address_space, span: start });
        }

        let mut mode_kind: Option<ModeKind> = None;
        let mut alignment: Option<usize> = None;
        loop {
            let (name, derived, decl_mode, _, decl_aligned) = self.parse_declarator_with_attrs();
            let (skip_mode, skip_aligned, skip_asm_reg) = self.skip_asm_and_attributes();
            let local_cleanup_fn = self.parsing_cleanup_fn.take();
            let local_section = self.parsing_section.take();
            mode_kind = mode_kind.or(decl_mode).or(skip_mode);
            // Merge alignment from declarator and post-declarator attributes
            for a in [decl_aligned, skip_aligned].iter().copied().flatten() {
                alignment = Some(alignment.map_or(a, |prev| prev.max(a)));
            }
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
                is_weak: false,
                alias_target: None,
                visibility: None,
                section: local_section,
                asm_register: skip_asm_reg,
                is_error_attr: false,
                is_noreturn: false,
                cleanup_fn: local_cleanup_fn,
                span: start,
            });
            let (_, post_init_aligned, _post_asm_reg) = self.skip_asm_and_attributes();
            if let Some(a) = post_init_aligned {
                alignment = Some(alignment.map_or(a, |prev| prev.max(a)));
            }
            if !self.consume_if(&TokenKind::Comma) {
                break;
            }
        }

        // Apply __attribute__((mode(...))): transform type to specified bit-width
        let type_spec = if let Some(mk) = mode_kind {
            mk.apply(type_spec)
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
        // Merge alignment from _Alignas (captured in parsed_alignas during type specifier parsing)
        // with alignment from __attribute__((aligned(N))) on declarators
        if let Some(a) = self.parsed_alignas.take() {
            alignment = Some(alignment.map_or(a, |prev| prev.max(a)));
        }
        let alignas_type = self.parsed_alignas_type.take();
        let alignment_sizeof_type = self.parsed_alignment_sizeof_type.take();
        let is_transparent_union = self.parsing_transparent_union;
        self.parsing_transparent_union = false;
        Some(Declaration { type_spec, declarators, is_static, is_extern, is_typedef, is_const: self.parsing_const, is_volatile: self.parsing_volatile, is_common: false, is_transparent_union, alignment, alignas_type, alignment_sizeof_type, address_space: self.parsing_address_space, span: start })
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
                        let lo = self.parse_expr();
                        if self.consume_if(&TokenKind::Ellipsis) {
                            // GCC range designator: [lo ... hi]
                            let hi = self.parse_expr();
                            self.expect(&TokenKind::RBracket);
                            designators.push(Designator::Range(lo, hi));
                        } else {
                            self.expect(&TokenKind::RBracket);
                            designators.push(Designator::Index(lo));
                        }
                    } else if self.consume_if(&TokenKind::Dot) {
                        if let TokenKind::Identifier(name) = self.peek() {
                            let name = name.clone();
                            self.advance();
                            designators.push(Designator::Field(name));
                        }
                    } else {
                        break;
                    }
                }
                // GNU old-style designator: field: value
                if designators.is_empty() {
                    if let TokenKind::Identifier(name) = self.peek() {
                        let name = name.clone();
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
            // Expand range designators [lo ... hi] into individual Index items
            let items = Self::expand_range_designators(items);
            Initializer::List(items)
        } else {
            Initializer::Expr(self.parse_assignment_expr())
        }
    }

    /// Expand GCC range designators `[lo ... hi] = val` into multiple items
    /// `[lo] = val, [lo+1] = val, ..., [hi] = val` so downstream code only
    /// sees `Designator::Index`.
    fn expand_range_designators(items: Vec<InitializerItem>) -> Vec<InitializerItem> {
        let mut result = Vec::with_capacity(items.len());
        for item in items {
            if let Some(range_pos) = item.designators.iter().position(|d| matches!(d, Designator::Range(_, _))) {
                if let Designator::Range(ref lo_expr, ref hi_expr) = item.designators[range_pos] {
                    // Try to evaluate lo and hi as integer constants
                    let lo = Self::eval_const_int_expr(lo_expr);
                    let hi = Self::eval_const_int_expr(hi_expr);
                    if let (Some(lo_val), Some(hi_val)) = (lo, hi) {
                        for idx in lo_val..=hi_val {
                            let mut new_desigs = item.designators.clone();
                            new_desigs[range_pos] = Designator::Index(
                                Expr::IntLiteral(idx, Span::dummy())
                            );
                            result.push(InitializerItem {
                                designators: new_desigs,
                                init: item.init.clone(),
                            });
                        }
                        continue;
                    }
                }
                // If we can't evaluate, keep as-is (will error later)
                result.push(item);
            } else {
                result.push(item);
            }
        }
        result
    }

    /// Constant expression evaluator for compile-time integer contexts.
    /// Handles integer/char literals, all arithmetic/bitwise/shift/logical/relational
    /// binary ops, unary ops (neg, bitwise not, logical not), and ternary expressions.
    /// Used for range designator bounds, alignment expressions, etc.
    /// The optional `enum_consts` map allows resolving enum constant identifiers.
    pub(super) fn eval_const_int_expr(expr: &Expr) -> Option<i64> {
        Self::eval_const_int_expr_with_enums(expr, None)
    }

    /// Evaluate a constant integer expression, with access to known enum constants.
    pub(super) fn eval_const_int_expr_with_enums(
        expr: &Expr,
        enum_consts: Option<&FxHashMap<String, i64>>,
    ) -> Option<i64> {
        match expr {
            Expr::IntLiteral(val, _) => Some(*val),
            Expr::UIntLiteral(val, _) => Some(*val as i64),
            Expr::LongLiteral(val, _) => Some(*val),
            Expr::ULongLiteral(val, _) => Some(*val as i64),
            Expr::CharLiteral(val, _) => Some(*val as i64),
            // Identifiers: look up enum constants if available
            Expr::Identifier(name, _) => {
                enum_consts.and_then(|m| m.get(name.as_str()).copied())
            }
            Expr::BinaryOp(op, lhs, rhs, _) => {
                let l = Self::eval_const_int_expr_with_enums(lhs, enum_consts)?;
                let r = Self::eval_const_int_expr_with_enums(rhs, enum_consts)?;
                match op {
                    BinOp::Add => Some(l.wrapping_add(r)),
                    BinOp::Sub => Some(l.wrapping_sub(r)),
                    BinOp::Mul => Some(l.wrapping_mul(r)),
                    BinOp::Div if r != 0 => Some(l.wrapping_div(r)),
                    BinOp::Mod if r != 0 => Some(l.wrapping_rem(r)),
                    BinOp::Shl => Some(l.wrapping_shl(r as u32)),
                    BinOp::Shr => Some(l.wrapping_shr(r as u32)),
                    BinOp::BitAnd => Some(l & r),
                    BinOp::BitOr => Some(l | r),
                    BinOp::BitXor => Some(l ^ r),
                    BinOp::Eq => Some(if l == r { 1 } else { 0 }),
                    BinOp::Ne => Some(if l != r { 1 } else { 0 }),
                    BinOp::Lt => Some(if l < r { 1 } else { 0 }),
                    BinOp::Le => Some(if l <= r { 1 } else { 0 }),
                    BinOp::Gt => Some(if l > r { 1 } else { 0 }),
                    BinOp::Ge => Some(if l >= r { 1 } else { 0 }),
                    BinOp::LogicalAnd => Some(if l != 0 && r != 0 { 1 } else { 0 }),
                    BinOp::LogicalOr => Some(if l != 0 || r != 0 { 1 } else { 0 }),
                    _ => None,
                }
            }
            Expr::UnaryOp(UnaryOp::Neg, inner, _) => {
                Self::eval_const_int_expr_with_enums(inner, enum_consts).map(|v| v.wrapping_neg())
            }
            Expr::UnaryOp(UnaryOp::BitNot, inner, _) => {
                Self::eval_const_int_expr_with_enums(inner, enum_consts).map(|v| !v)
            }
            Expr::UnaryOp(UnaryOp::LogicalNot, inner, _) => {
                Self::eval_const_int_expr_with_enums(inner, enum_consts).map(|v| if v == 0 { 1 } else { 0 })
            }
            Expr::UnaryOp(UnaryOp::Plus, inner, _) => {
                Self::eval_const_int_expr_with_enums(inner, enum_consts)
            }
            Expr::Conditional(cond, then_expr, else_expr, _) => {
                let c = Self::eval_const_int_expr_with_enums(cond, enum_consts)?;
                if c != 0 {
                    Self::eval_const_int_expr_with_enums(then_expr, enum_consts)
                } else {
                    Self::eval_const_int_expr_with_enums(else_expr, enum_consts)
                }
            }
            Expr::Cast(_, inner, _) => {
                Self::eval_const_int_expr_with_enums(inner, enum_consts)
            }
            Expr::Sizeof(arg, _) => {
                match arg.as_ref() {
                    SizeofArg::Type(ts) => Some(Self::sizeof_type_spec(ts) as i64),
                    SizeofArg::Expr(_) => None,
                }
            }
            Expr::Alignof(ts, _) => {
                Some(Self::alignof_type_spec(ts) as i64)
            }
            // __alignof__(expr): parser-level can't always determine type alignment
            // from an expression, so return None (let sema/lowerer handle it)
            Expr::AlignofExpr(_, _) => None,
            _ => None,
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
                TokenKind::Volatile => { self.advance(); self.parsing_volatile = true; }
                TokenKind::Restrict
                | TokenKind::Inline | TokenKind::Register | TokenKind::Auto => { self.advance(); }
                TokenKind::SegGs => { self.advance(); self.parsing_address_space = AddressSpace::SegGs; }
                TokenKind::SegFs => { self.advance(); self.parsing_address_space = AddressSpace::SegFs; }
                TokenKind::Alignas => {
                    self.advance();
                    if let Some(align) = self.parse_alignas_argument() {
                        self.parsed_alignas = Some(self.parsed_alignas.map_or(align, |prev| prev.max(align)));
                    }
                }
                TokenKind::Attribute => {
                    let (_, aligned, _, _) = self.parse_gcc_attributes();
                    if let Some(a) = aligned {
                        self.parsed_alignas = Some(self.parsed_alignas.map_or(a, |prev| prev.max(a)));
                    }
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

}
