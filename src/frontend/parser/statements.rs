// Statement parsing: all C statement types including inline assembly.
//
// Handles: return, if/else, while, do-while, for, switch/case/default,
// break, continue, goto (including computed goto), labels, compound
// statements, and inline assembly (GCC syntax).

use crate::frontend::lexer::token::TokenKind;
use super::ast::*;
use super::parser::Parser;

impl Parser {
    pub(super) fn parse_compound_stmt(&mut self) -> CompoundStmt {
        let start = self.peek_span();
        self.expect(&TokenKind::LBrace);
        let mut items = Vec::new();
        let mut local_labels = Vec::new();

        // Save typedef shadowing state for this scope
        let saved_shadowed = self.shadowed_typedefs.clone();

        // Parse GNU __label__ declarations at the start of the block.
        // These must appear before any statements or declarations.
        // Syntax: __label__ ident1, ident2, ... ;
        while matches!(self.peek(), TokenKind::GnuLabel) {
            self.advance(); // consume __label__
            // Parse comma-separated list of label names
            loop {
                if let TokenKind::Identifier(name) = self.peek() {
                    local_labels.push(name.clone());
                    self.advance();
                }
                if !self.consume_if(&TokenKind::Comma) {
                    break;
                }
            }
            self.expect(&TokenKind::Semicolon);
        }

        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            self.skip_gcc_extensions();
            // Handle #pragma pack directives within function bodies
            while self.handle_pragma_pack_token() {
                self.consume_if(&TokenKind::Semicolon);
            }
            // Handle #pragma GCC visibility push/pop within function bodies
            while self.handle_pragma_visibility_token() {
                self.consume_if(&TokenKind::Semicolon);
            }
            if matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
                break;
            }
            // Handle __label__ declarations that appear after __extension__
            if matches!(self.peek(), TokenKind::GnuLabel) {
                self.advance();
                loop {
                    if let TokenKind::Identifier(name) = self.peek() {
                        local_labels.push(name.clone());
                        self.advance();
                    }
                    if !self.consume_if(&TokenKind::Comma) {
                        break;
                    }
                }
                self.expect(&TokenKind::Semicolon);
                continue;
            }
            if matches!(self.peek(), TokenKind::StaticAssert) {
                // _Static_assert - skip entirely (no codegen needed)
                self.advance();
                self.skip_balanced_parens();
                self.consume_if(&TokenKind::Semicolon);
            } else if self.is_type_specifier() {
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
        CompoundStmt { items, span: start, local_labels }
    }

    pub(super) fn parse_stmt(&mut self) -> Stmt {
        // C23 / GNU extension: declarations are allowed in statement position.
        // This handles declarations after labels (e.g., `label: int x = 5;`),
        // after case/default, and other contexts where parse_stmt() is called.
        // We skip __extension__ first since it can precede declarations.
        self.skip_gcc_extensions();
        if self.is_type_specifier() {
            if let Some(decl) = self.parse_local_declaration() {
                return Stmt::Declaration(decl);
            }
            // If parse_local_declaration returns None (e.g. _Static_assert),
            // fall through to parse a null statement or the next thing
            return Stmt::Expr(None);
        }
        match self.peek() {
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
                self.parse_for_stmt()
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
                if self.consume_if(&TokenKind::Ellipsis) {
                    // GNU case range extension: case low ... high:
                    let high = self.parse_expr();
                    self.expect(&TokenKind::Colon);
                    let stmt = self.parse_stmt();
                    Stmt::CaseRange(expr, high, Box::new(stmt), span)
                } else {
                    self.expect(&TokenKind::Colon);
                    let stmt = self.parse_stmt();
                    Stmt::Case(expr, Box::new(stmt), span)
                }
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
                if matches!(self.peek(), TokenKind::Star) {
                    // Computed goto: goto *expr;
                    self.advance();
                    let expr = self.parse_expr();
                    self.expect(&TokenKind::Semicolon);
                    Stmt::GotoIndirect(Box::new(expr), span)
                } else {
                    let label = if let TokenKind::Identifier(name) = self.peek() {
                        let name = name.clone();
                        self.advance();
                        name
                    } else {
                        String::new()
                    };
                    self.expect(&TokenKind::Semicolon);
                    Stmt::Goto(label, span)
                }
            }
            TokenKind::Identifier(name) => {
                // Check for label (identifier followed by colon)
                let name_clone = name.clone();
                let span = self.peek_span();
                if self.pos + 1 < self.tokens.len() && matches!(self.tokens[self.pos + 1].kind, TokenKind::Colon) {
                    self.advance(); // identifier
                    self.advance(); // colon
                    // Skip optional label attributes: `label: __attribute__((unused));`
                    // In GNU C, labels can have attributes (e.g., unused, hot, cold).
                    // We consume and discard them since they only affect diagnostics.
                    self.skip_label_attributes();
                    let stmt = self.parse_stmt();
                    Stmt::Label(name_clone, Box::new(stmt), span)
                } else {
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
                let expr = self.parse_expr();
                self.expect(&TokenKind::Semicolon);
                Stmt::Expr(Some(expr))
            }
        }
    }

    /// Parse a for statement: for (init; cond; inc) body
    fn parse_for_stmt(&mut self) -> Stmt {
        let span = self.peek_span();
        self.advance();
        self.expect(&TokenKind::LParen);

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

        let cond = if matches!(self.peek(), TokenKind::Semicolon) {
            None
        } else {
            Some(self.parse_expr())
        };
        self.expect(&TokenKind::Semicolon);

        let inc = if matches!(self.peek(), TokenKind::RParen) {
            None
        } else {
            Some(self.parse_expr())
        };
        self.expect(&TokenKind::RParen);

        let body = self.parse_stmt();
        Stmt::For(init, cond, inc, Box::new(body), span)
    }

    // === Inline assembly parsing ===

    fn parse_inline_asm(&mut self) -> Stmt {
        self.advance(); // consume 'asm' / '__asm__'
        // Skip optional qualifiers: volatile, goto, inline
        while matches!(self.peek(), TokenKind::Volatile)
            || matches!(self.peek(), TokenKind::Goto)
            || matches!(self.peek(), TokenKind::Inline)
        {
            self.advance();
        }
        self.expect(&TokenKind::LParen);

        let template = self.parse_asm_string();

        let mut outputs = Vec::new();
        let mut inputs = Vec::new();
        let mut clobbers = Vec::new();
        let mut goto_labels = Vec::new();

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

                    // Fourth colon: goto labels
                    if matches!(self.peek(), TokenKind::Colon) {
                        self.advance();
                        goto_labels = self.parse_asm_goto_labels();
                    }
                }
            }
        }

        self.expect(&TokenKind::RParen);
        self.consume_if(&TokenKind::Semicolon);

        Stmt::InlineAsm { template, outputs, inputs, clobbers, goto_labels }
    }

    fn parse_asm_string(&mut self) -> String {
        let mut result = String::new();
        while let TokenKind::StringLiteral(ref s) = self.peek() {
            result.push_str(s);
            self.advance();
        }
        result
    }

    fn parse_asm_operands(&mut self) -> Vec<AsmOperand> {
        let mut operands = Vec::new();
        if matches!(self.peek(), TokenKind::Colon | TokenKind::RParen) {
            return operands;
        }
        loop {
            let operand = self.parse_one_asm_operand();
            operands.push(operand);
            if !self.consume_if(&TokenKind::Comma) {
                break;
            }
        }
        operands
    }

    fn parse_one_asm_operand(&mut self) -> AsmOperand {
        // Optional [name]
        let name = if matches!(self.peek(), TokenKind::LBracket) {
            self.advance();
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

        // Constraint string (may be concatenated)
        let constraint = if let TokenKind::StringLiteral(ref s) = self.peek() {
            let mut full = s.clone();
            self.advance();
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

        AsmOperand { name, constraint, expr }
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
            if !self.consume_if(&TokenKind::Comma) {
                break;
            }
        }
        clobbers
    }

    /// Parse the goto labels section (fourth colon) of an asm goto statement.
    /// Labels are comma-separated identifiers: `asm goto("..." : : : : label1, label2)`
    fn parse_asm_goto_labels(&mut self) -> Vec<String> {
        let mut labels = Vec::new();
        if matches!(self.peek(), TokenKind::RParen) {
            return labels;
        }
        loop {
            if let TokenKind::Identifier(ref name) = self.peek() {
                labels.push(name.clone());
                self.advance();
            } else {
                break;
            }
            if !self.consume_if(&TokenKind::Comma) {
                break;
            }
        }
        labels
    }
}
