// Declarator parsing: handles the C declarator syntax (the part after the type
// specifier that defines the name and type modifiers like pointers, arrays,
// and function parameters).
//
// C declarators follow an "inside-out" rule: int (*fp)(int) means fp is a
// pointer to a function returning int, read from the name outward. This module
// handles the recursive parsing needed for this grammar.

use crate::frontend::lexer::token::TokenKind;
use super::ast::*;
use super::parser::Parser;

impl Parser {
    pub(super) fn parse_declarator(&mut self) -> (Option<String>, Vec<DerivedDeclarator>) {
        let (name, derived, _, _, _) = self.parse_declarator_with_attrs();
        (name, derived)
    }

    /// Parse a declarator, also returning attribute info:
    /// (name, derived, has_mode_ti, has_common, aligned_value)
    pub(super) fn parse_declarator_with_attrs(&mut self) -> (Option<String>, Vec<DerivedDeclarator>, bool, bool, Option<usize>) {
        let mut derived = Vec::new();

        let mut pre_aligned: Option<usize> = None;
        let (_, pre_align, _, _) = self.parse_gcc_attributes();
        if let Some(a) = pre_align {
            pre_aligned = Some(pre_aligned.map_or(a, |prev: usize| prev.max(a)));
        }

        // Parse pointer(s) with optional qualifiers and attributes
        while self.consume_if(&TokenKind::Star) {
            derived.push(DerivedDeclarator::Pointer);
            self.skip_cv_qualifiers();
            self.skip_gcc_extensions();
        }

        // Parse the direct-declarator part
        let (name, inner_derived) = if let TokenKind::Identifier(n) = self.peek().clone() {
            self.advance();
            (Some(n), Vec::new())
        } else if matches!(self.peek(), TokenKind::LParen) && self.is_paren_declarator() {
            let save = self.pos;
            self.advance(); // consume '('
            let (inner_name, inner_derived) = self.parse_declarator();
            if !self.consume_if(&TokenKind::RParen) {
                self.pos = save;
                (None, Vec::new())
            } else {
                (inner_name, inner_derived)
            }
        } else {
            (None, Vec::new())
        };

        // Parse outer suffixes: array dimensions and function params
        let mut outer_suffixes = Vec::new();
        loop {
            match self.peek() {
                TokenKind::LBracket => {
                    self.advance();
                    // C99 array parameter declarators can have qualifiers and 'static':
                    // [static restrict const 10], [restrict n], [const], etc.
                    // Skip all qualifiers and 'static' before the size expression.
                    self.skip_array_qualifiers();
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

        // Combine using inside-out rule
        let combined = self.combine_declarator_parts(derived, inner_derived, outer_suffixes);

        let (_, post_aligned, has_mode_ti, has_common) = self.parse_gcc_attributes();
        let aligned = match (pre_aligned, post_aligned) {
            (Some(a), Some(b)) => Some(a.max(b)),
            (Some(a), None) => Some(a),
            (None, Some(b)) => Some(b),
            (None, None) => None,
        };

        (name, combined, has_mode_ti, has_common, aligned)
    }

    /// Determine if a '(' starts a parenthesized declarator vs. a parameter list.
    pub(super) fn is_paren_declarator(&self) -> bool {
        if self.pos + 1 >= self.tokens.len() {
            return false;
        }
        match &self.tokens[self.pos + 1].kind {
            TokenKind::Star | TokenKind::Caret => true,
            TokenKind::LParen => true,
            TokenKind::Attribute | TokenKind::Extension => true,
            TokenKind::Identifier(name) => {
                // Typedef name -> parameter list; regular name -> declarator
                !(self.typedefs.contains(name) && !self.shadowed_typedefs.contains(name))
            }
            TokenKind::RParen | TokenKind::Ellipsis => false,
            TokenKind::Void | TokenKind::Char | TokenKind::Short | TokenKind::Int |
            TokenKind::Long | TokenKind::Float | TokenKind::Double | TokenKind::Signed |
            TokenKind::Unsigned | TokenKind::Struct | TokenKind::Union | TokenKind::Enum |
            TokenKind::Const | TokenKind::Volatile | TokenKind::Static | TokenKind::Extern |
            TokenKind::Register | TokenKind::Typedef | TokenKind::Inline | TokenKind::Bool |
            TokenKind::Typeof | TokenKind::Noreturn | TokenKind::Restrict | TokenKind::Complex |
            TokenKind::Atomic | TokenKind::Auto | TokenKind::Alignas |
            TokenKind::Builtin => false,
            _ => false,
        }
    }

    /// Combine declarator parts using C's inside-out rule.
    ///
    /// For `int (*fp)(int)`:
    ///   outer_pointers: []
    ///   inner_derived: [Pointer]
    ///   outer_suffixes: [Function([int])]
    ///   Result: [Pointer, FunctionPointer([int])]
    fn combine_declarator_parts(
        &self,
        mut outer_pointers: Vec<DerivedDeclarator>,
        inner_derived: Vec<DerivedDeclarator>,
        outer_suffixes: Vec<DerivedDeclarator>,
    ) -> Vec<DerivedDeclarator> {
        if inner_derived.is_empty() && outer_suffixes.is_empty() {
            return outer_pointers;
        }

        if inner_derived.is_empty() {
            outer_pointers.extend(outer_suffixes);
            return outer_pointers;
        }

        // Check for function pointer: inner has Pointer(s), outer starts with Function
        let inner_only_ptr_and_array = inner_derived.iter().all(|d|
            matches!(d, DerivedDeclarator::Pointer | DerivedDeclarator::Array(_)));
        let inner_has_pointer = inner_derived.iter().any(|d| matches!(d, DerivedDeclarator::Pointer));
        let outer_starts_with_function = matches!(outer_suffixes.first(), Some(DerivedDeclarator::Function(_, _)));

        if inner_only_ptr_and_array && inner_has_pointer && outer_starts_with_function
            && outer_suffixes.len() == 1
        {
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

        // Check for pointer-to-array: inner contains Pointer(s) with optional arrays,
        // outer is all Array(s).
        // For `int (*p)[3][6]`: inner=[Pointer], outer=[Array(3), Array(6)]
        //   -> result: [Array(3), Array(6), Pointer]
        // For `int ((*(p))[3])[6]`: inner=[Array(3), Pointer], outer=[Array(6)]
        //   -> result: [Array(3), Array(6), Pointer]
        // The rule: inner arrays first, then outer arrays, then pointer(s) last.
        let outer_only_arrays = outer_suffixes.iter().all(|d| matches!(d, DerivedDeclarator::Array(_)));
        if inner_only_ptr_and_array && inner_has_pointer && outer_only_arrays {
            let mut result = outer_pointers;
            // First: inner array dimensions (from deeper nesting levels)
            for d in &inner_derived {
                if matches!(d, DerivedDeclarator::Array(_)) {
                    result.push(d.clone());
                }
            }
            // Then: outer array dimensions (from current level)
            result.extend(outer_suffixes);
            // Finally: pointer(s)
            for d in inner_derived {
                if matches!(d, DerivedDeclarator::Pointer) {
                    result.push(d);
                }
            }
            return result;
        }

        // General case: outer_pointers ++ outer_suffixes ++ inner_derived
        outer_pointers.extend(outer_suffixes);
        outer_pointers.extend(inner_derived);
        outer_pointers
    }

    /// Parse a function parameter list: (params...) or (void) or ()
    pub(super) fn parse_param_list(&mut self) -> (Vec<ParamDecl>, bool) {
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

        // Check for K&R-style identifier list
        if let TokenKind::Identifier(ref name) = self.peek() {
            if (!self.typedefs.contains(name) || self.shadowed_typedefs.contains(name)) && !self.is_type_specifier() {
                return self.parse_kr_identifier_list();
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
                let (name, pointer_depth, array_dims, is_func_ptr, ptr_to_array_dims, fptr_param_decls) =
                    self.parse_param_declarator_full();
                self.skip_gcc_extensions();

                // Apply pointer levels
                for _ in 0..pointer_depth {
                    type_spec = TypeSpecifier::Pointer(Box::new(type_spec));
                }

                // Pointer-to-array: int (*p)[N][M]
                if !ptr_to_array_dims.is_empty() {
                    for dim in ptr_to_array_dims.iter().rev() {
                        type_spec = TypeSpecifier::Array(Box::new(type_spec), dim.clone());
                    }
                    type_spec = TypeSpecifier::Pointer(Box::new(type_spec));
                }

                // Array params: outermost dimension decays to pointer
                if !array_dims.is_empty() {
                    for dim in array_dims.iter().skip(1).rev() {
                        type_spec = TypeSpecifier::Array(Box::new(type_spec), dim.clone());
                    }
                    type_spec = TypeSpecifier::Pointer(Box::new(type_spec));
                }

                // Function pointers decay to pointer
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

    /// Parse a K&R-style identifier list: foo(a, b, c)
    fn parse_kr_identifier_list(&mut self) -> (Vec<ParamDecl>, bool) {
        let mut params = Vec::new();
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
        (params, false)
    }

    /// Parse a parameter declarator with full type information.
    /// Returns (name, pointer_depth, array_dims, is_func_ptr, ptr_to_array_dims, fptr_params).
    pub(super) fn parse_param_declarator_full(&mut self) -> (Option<String>, u32, Vec<Option<Box<Expr>>>, bool, Vec<Option<Box<Expr>>>, Option<Vec<ParamDecl>>) {
        let mut pointer_depth: u32 = 0;
        while self.consume_if(&TokenKind::Star) {
            pointer_depth += 1;
            self.skip_cv_qualifiers();
        }
        let mut array_dims: Vec<Option<Box<Expr>>> = Vec::new();
        let mut is_func_ptr = false;
        let mut ptr_to_array_dims: Vec<Option<Box<Expr>>> = Vec::new();
        let mut fptr_params: Option<Vec<ParamDecl>> = None;

        let name = if matches!(self.peek(), TokenKind::LParen) {
            self.parse_paren_param_declarator(&mut pointer_depth, &mut is_func_ptr, &mut ptr_to_array_dims, &mut fptr_params)
        } else if let TokenKind::Identifier(n) = self.peek().clone() {
            self.advance();
            Some(n)
        } else {
            None
        };

        // Parse trailing array dimensions
        while matches!(self.peek(), TokenKind::LBracket) {
            self.advance();
            // Skip C99 array qualifiers (static, restrict, const, volatile)
            self.skip_array_qualifiers();
            if matches!(self.peek(), TokenKind::RBracket) {
                array_dims.push(None);
                self.advance();
            } else {
                let dim_expr = self.parse_expr();
                array_dims.push(Some(Box::new(dim_expr)));
                self.expect(&TokenKind::RBracket);
            }
        }

        // Skip trailing function param list (for abstract function pointer types)
        if matches!(self.peek(), TokenKind::LParen) {
            self.skip_balanced_parens();
        }

        (name, pointer_depth, array_dims, is_func_ptr, ptr_to_array_dims, fptr_params)
    }

    /// Parse a parenthesized parameter declarator: (*name)(params), (name), etc.
    fn parse_paren_param_declarator(
        &mut self,
        pointer_depth: &mut u32,
        is_func_ptr: &mut bool,
        ptr_to_array_dims: &mut Vec<Option<Box<Expr>>>,
        fptr_params: &mut Option<Vec<ParamDecl>>,
    ) -> Option<String> {
        let save = self.pos;
        self.advance(); // consume '('

        if matches!(self.peek(), TokenKind::Star) {
            // Function pointer or pointer-to-array: (*name)(params) or (*name)[N]
            let mut inner_ptr_depth = 0u32;
            while self.consume_if(&TokenKind::Star) {
                inner_ptr_depth += 1;
                self.skip_cv_qualifiers();
            }
            let name = if let TokenKind::Identifier(n) = self.peek().clone() {
                self.advance();
                Some(n)
            } else if matches!(self.peek(), TokenKind::LParen) {
                self.extract_paren_name()
            } else {
                None
            };
            *pointer_depth += inner_ptr_depth.saturating_sub(1);
            self.skip_array_dimensions();
            self.expect(&TokenKind::RParen);
            if matches!(self.peek(), TokenKind::LParen) {
                // Function pointer
                *is_func_ptr = true;
                let (fp_params, _variadic) = self.parse_param_list();
                *fptr_params = Some(fp_params);
            } else if matches!(self.peek(), TokenKind::LBracket) {
                // Pointer-to-array
                while matches!(self.peek(), TokenKind::LBracket) {
                    self.advance();
                    self.skip_array_qualifiers();
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
                *pointer_depth += 1;
            }
            name
        } else if self.consume_if(&TokenKind::Caret) {
            // Block pointer (Apple extension)
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
            self.expect(&TokenKind::RParen);
            self.skip_array_dimensions();
            if matches!(self.peek(), TokenKind::LParen) {
                self.skip_balanced_parens();
            }
            name
        } else if matches!(self.peek(), TokenKind::LParen) {
            // Nested: ((name)) or ((*name))
            let name = self.extract_paren_name();
            self.expect(&TokenKind::RParen);
            self.skip_array_dimensions();
            if matches!(self.peek(), TokenKind::LParen) {
                self.skip_balanced_parens();
            }
            name
        } else {
            self.pos = save;
            None
        }
    }

    /// Extract a name from nested parentheses: (name), ((name)), (*(name)), etc.
    pub(super) fn extract_paren_name(&mut self) -> Option<String> {
        if !matches!(self.peek(), TokenKind::LParen) {
            if let TokenKind::Identifier(n) = self.peek().clone() {
                self.advance();
                return Some(n);
            }
            return None;
        }
        self.advance(); // consume '('
        if matches!(self.peek(), TokenKind::Star) {
            self.advance();
            self.skip_cv_qualifiers();
        }
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

    /// Try to parse a parenthesized abstract declarator: (*), ((*)), (**)
    /// Returns pointer depth if successful, None otherwise. Restores position on failure.
    pub(super) fn try_parse_paren_abstract_declarator(&mut self) -> Option<u32> {
        if !matches!(self.peek(), TokenKind::LParen) {
            return None;
        }
        let save = self.pos;
        self.advance(); // consume '('

        let mut total_ptrs = 0u32;

        while self.consume_if(&TokenKind::Star) {
            total_ptrs += 1;
            self.skip_cv_qualifiers();
        }

        // Check for nested: (* (...))
        if matches!(self.peek(), TokenKind::LParen) {
            if let Some(inner_ptrs) = self.try_parse_paren_abstract_declarator() {
                total_ptrs += inner_ptrs;
            } else {
                self.pos = save;
                return None;
            }
        }

        if self.consume_if(&TokenKind::RParen) {
            if total_ptrs > 0 {
                Some(total_ptrs)
            } else {
                self.pos = save;
                None
            }
        } else {
            self.pos = save;
            None
        }
    }
}
