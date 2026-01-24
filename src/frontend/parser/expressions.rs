// Expression parsing: precedence climbing from comma expression down to primary.
//
// The expression parser implements C's operator precedence through a table-driven
// approach. Binary operators (logical, bitwise, relational, arithmetic) are handled
// by a shared `parse_binary_expr` method parameterized on `PrecedenceLevel`, which
// maps tokens to operators and recurses to the next-tighter level. This eliminates
// the repetitive per-level parsing functions while keeping the code clear.
//
// Call hierarchy (loosest to tightest binding):
//   parse_expr -> parse_assignment_expr -> parse_conditional_expr
//   -> parse_binary_expr(LogicalOr) -> ... -> parse_binary_expr(Multiplicative)
//   -> parse_cast_expr -> parse_unary_expr -> parse_postfix_expr
//   -> parse_primary_expr

use crate::common::source::Span;
use crate::frontend::lexer::token::TokenKind;
use super::ast::*;
use super::parser::Parser;

/// C operator precedence levels (loosest to tightest binding).
/// Used by the table-driven binary expression parser.
#[derive(Debug, Clone, Copy)]
enum PrecedenceLevel {
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Equality,
    Relational,
    Shift,
    Additive,
    Multiplicative,
}

impl Parser {
    pub(super) fn parse_expr(&mut self) -> Expr {
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

    pub(super) fn parse_assignment_expr(&mut self) -> Expr {
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
        let cond = self.parse_binary_expr(PrecedenceLevel::LogicalOr);
        if self.consume_if(&TokenKind::Question) {
            let span = cond.span();
            // GNU extension: `cond ? : else_expr` (omitted middle operand)
            // Condition is evaluated once and used as the then-value if truthy.
            if self.peek() == &TokenKind::Colon {
                self.expect(&TokenKind::Colon);
                let else_expr = self.parse_conditional_expr();
                Expr::GnuConditional(Box::new(cond), Box::new(else_expr), span)
            } else {
                let then_expr = self.parse_expr();
                self.expect(&TokenKind::Colon);
                let else_expr = self.parse_conditional_expr();
                Expr::Conditional(Box::new(cond), Box::new(then_expr), Box::new(else_expr), span)
            }
        } else {
            cond
        }
    }

    /// Map a token to a binary operator at the current precedence level, if applicable.
    fn token_to_binop(&self, token: &TokenKind, level: PrecedenceLevel) -> Option<BinOp> {
        match (token, level) {
            (TokenKind::PipePipe, PrecedenceLevel::LogicalOr) => Some(BinOp::LogicalOr),
            (TokenKind::AmpAmp, PrecedenceLevel::LogicalAnd) => Some(BinOp::LogicalAnd),
            (TokenKind::Pipe, PrecedenceLevel::BitwiseOr) => Some(BinOp::BitOr),
            (TokenKind::Caret, PrecedenceLevel::BitwiseXor) => Some(BinOp::BitXor),
            (TokenKind::Amp, PrecedenceLevel::BitwiseAnd) => Some(BinOp::BitAnd),
            (TokenKind::EqualEqual, PrecedenceLevel::Equality) => Some(BinOp::Eq),
            (TokenKind::BangEqual, PrecedenceLevel::Equality) => Some(BinOp::Ne),
            (TokenKind::Less, PrecedenceLevel::Relational) => Some(BinOp::Lt),
            (TokenKind::LessEqual, PrecedenceLevel::Relational) => Some(BinOp::Le),
            (TokenKind::Greater, PrecedenceLevel::Relational) => Some(BinOp::Gt),
            (TokenKind::GreaterEqual, PrecedenceLevel::Relational) => Some(BinOp::Ge),
            (TokenKind::LessLess, PrecedenceLevel::Shift) => Some(BinOp::Shl),
            (TokenKind::GreaterGreater, PrecedenceLevel::Shift) => Some(BinOp::Shr),
            (TokenKind::Plus, PrecedenceLevel::Additive) => Some(BinOp::Add),
            (TokenKind::Minus, PrecedenceLevel::Additive) => Some(BinOp::Sub),
            (TokenKind::Star, PrecedenceLevel::Multiplicative) => Some(BinOp::Mul),
            (TokenKind::Slash, PrecedenceLevel::Multiplicative) => Some(BinOp::Div),
            (TokenKind::Percent, PrecedenceLevel::Multiplicative) => Some(BinOp::Mod),
            _ => None,
        }
    }

    /// Parse a left-associative binary expression at the given precedence level.
    /// This is the shared core that replaces 10 nearly-identical parsing functions.
    fn parse_binary_expr(&mut self, level: PrecedenceLevel) -> Expr {
        let mut lhs = self.parse_next_tighter(level);
        loop {
            let tok = self.peek().clone();
            let op = match self.token_to_binop(&tok, level) {
                Some(op) => op,
                None => break,
            };
            let span = self.peek_span();
            self.advance();
            let rhs = self.parse_next_tighter(level);
            lhs = Expr::BinaryOp(op, Box::new(lhs), Box::new(rhs), span);
        }
        lhs
    }

    /// Parse the next tighter precedence level.
    fn parse_next_tighter(&mut self, level: PrecedenceLevel) -> Expr {
        match level {
            PrecedenceLevel::LogicalOr => self.parse_binary_expr(PrecedenceLevel::LogicalAnd),
            PrecedenceLevel::LogicalAnd => self.parse_binary_expr(PrecedenceLevel::BitwiseOr),
            PrecedenceLevel::BitwiseOr => self.parse_binary_expr(PrecedenceLevel::BitwiseXor),
            PrecedenceLevel::BitwiseXor => self.parse_binary_expr(PrecedenceLevel::BitwiseAnd),
            PrecedenceLevel::BitwiseAnd => self.parse_binary_expr(PrecedenceLevel::Equality),
            PrecedenceLevel::Equality => self.parse_binary_expr(PrecedenceLevel::Relational),
            PrecedenceLevel::Relational => self.parse_binary_expr(PrecedenceLevel::Shift),
            PrecedenceLevel::Shift => self.parse_binary_expr(PrecedenceLevel::Additive),
            PrecedenceLevel::Additive => self.parse_binary_expr(PrecedenceLevel::Multiplicative),
            PrecedenceLevel::Multiplicative => self.parse_cast_expr(),
        }
    }

    /// Parse a cast expression: (type-name)expr, compound literal (type-name){...},
    /// or fall through to unary expression.
    pub(super) fn parse_cast_expr(&mut self) -> Expr {
        if matches!(self.peek(), TokenKind::LParen) {
            let save = self.pos;
            let save_typedef = self.parsing_typedef;
            self.advance();
            if self.is_type_specifier() {
                if let Some(type_spec) = self.parse_type_specifier() {
                    let result_type = self.parse_abstract_declarator_suffix(type_spec);
                    if matches!(self.peek(), TokenKind::RParen) {
                        let span = self.peek_span();
                        self.advance();
                        // Check for compound literal: (type){...}
                        if matches!(self.peek(), TokenKind::LBrace) {
                            let init = self.parse_initializer();
                            let lit = Expr::CompoundLiteral(result_type, Box::new(init), span);
                            return self.parse_postfix_ops(lit);
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
                self.parse_postfix_expr()
            }
            TokenKind::RealPart => {
                let span = self.peek_span();
                self.advance();
                let expr = self.parse_cast_expr();
                Expr::UnaryOp(UnaryOp::RealPart, Box::new(expr), span)
            }
            TokenKind::ImagPart => {
                let span = self.peek_span();
                self.advance();
                let expr = self.parse_cast_expr();
                Expr::UnaryOp(UnaryOp::ImagPart, Box::new(expr), span)
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
                self.parse_sizeof_expr()
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
                    Expr::IntLiteral(8, span)
                }
            }
            _ => self.parse_postfix_expr(),
        }
    }

    /// Parse sizeof expression. Handles both sizeof(type-name) and sizeof expr.
    fn parse_sizeof_expr(&mut self) -> Expr {
        let span = self.peek_span();
        self.advance(); // consume 'sizeof'
        if matches!(self.peek(), TokenKind::LParen) {
            let save = self.pos;
            let save_typedef = self.parsing_typedef;
            self.advance();
            if self.is_type_specifier() {
                if let Some(ts) = self.parse_type_specifier() {
                    let result_type = self.parse_abstract_declarator_suffix(ts);
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

    fn parse_postfix_expr(&mut self) -> Expr {
        let expr = self.parse_primary_expr();
        self.parse_postfix_ops(expr)
    }

    /// Parse postfix operators ([], ., ->, ++, --, function call) applied to an initial expression.
    fn parse_postfix_ops(&mut self, mut expr: Expr) -> Expr {
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
            TokenKind::ImaginaryLiteral(val) => {
                let span = self.peek_span();
                self.advance();
                Expr::ImaginaryLiteral(val, span)
            }
            TokenKind::ImaginaryLiteralF32(val) => {
                let span = self.peek_span();
                self.advance();
                Expr::ImaginaryLiteralF32(val, span)
            }
            TokenKind::ImaginaryLiteralLongDouble(val) => {
                let span = self.peek_span();
                self.advance();
                Expr::ImaginaryLiteralLongDouble(val, span)
            }
            TokenKind::StringLiteral(ref s) => {
                let mut result = s.clone();
                let span = self.peek_span();
                self.advance();
                // Concatenate adjacent string literals. If any is wide, result is wide.
                let mut is_wide = false;
                loop {
                    match self.peek() {
                        TokenKind::StringLiteral(ref s2) => {
                            result.push_str(s2);
                            self.advance();
                        }
                        TokenKind::WideStringLiteral(ref s2) => {
                            result.push_str(s2);
                            is_wide = true;
                            self.advance();
                        }
                        _ => break,
                    }
                }
                if is_wide {
                    Expr::WideStringLiteral(result, span)
                } else {
                    Expr::StringLiteral(result, span)
                }
            }
            TokenKind::WideStringLiteral(ref s) => {
                let mut result = s.clone();
                let span = self.peek_span();
                self.advance();
                // Concatenate adjacent string literals (wide + narrow = wide)
                loop {
                    match self.peek() {
                        TokenKind::StringLiteral(ref s2) | TokenKind::WideStringLiteral(ref s2) => {
                            result.push_str(s2);
                            self.advance();
                        }
                        _ => break,
                    }
                }
                Expr::WideStringLiteral(result, span)
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
                    let compound = self.parse_compound_stmt();
                    self.expect(&TokenKind::RParen);
                    Expr::StmtExpr(compound, span)
                } else {
                    let expr = self.parse_expr();
                    self.expect(&TokenKind::RParen);
                    expr
                }
            }
            TokenKind::Generic => {
                self.parse_generic_selection()
            }
            TokenKind::Asm => {
                // GCC asm expression
                let span = self.peek_span();
                self.advance();
                self.consume_if(&TokenKind::Volatile);
                if matches!(self.peek(), TokenKind::LParen) {
                    self.skip_balanced_parens();
                }
                Expr::IntLiteral(0, span)
            }
            TokenKind::BuiltinVaArg => {
                let span = self.peek_span();
                self.advance();
                self.expect(&TokenKind::LParen);
                let ap_expr = self.parse_assignment_expr();
                self.expect(&TokenKind::Comma);
                let type_spec = self.parse_va_arg_type();
                self.expect(&TokenKind::RParen);
                Expr::VaArg(Box::new(ap_expr), type_spec, span)
            }
            TokenKind::BuiltinTypesCompatibleP => {
                // __builtin_types_compatible_p(type1, type2) - always returns 0 (conservative)
                let span = self.peek_span();
                self.advance();
                self.expect(&TokenKind::LParen);
                self.parse_va_arg_type();
                self.expect(&TokenKind::Comma);
                self.parse_va_arg_type();
                self.expect(&TokenKind::RParen);
                Expr::IntLiteral(0, span)
            }
            TokenKind::Typeof => {
                let span = self.peek_span();
                self.advance();
                if matches!(self.peek(), TokenKind::LParen) {
                    self.skip_balanced_parens();
                }
                Expr::IntLiteral(0, span)
            }
            TokenKind::Builtin => {
                let span = self.peek_span();
                self.advance();
                Expr::Identifier("__builtin_va_list".to_string(), span)
            }
            TokenKind::Extension => {
                self.advance();
                self.parse_cast_expr()
            }
            _ => {
                let span = self.peek_span();
                eprintln!("error: unexpected token {:?} in expression", self.peek());
                self.error_count += 1;
                self.advance();
                Expr::IntLiteral(0, span)
            }
        }
    }

    /// Parse _Generic(controlling_expr, type: expr, ..., default: expr)
    fn parse_generic_selection(&mut self) -> Expr {
        let span = self.peek_span();
        self.advance(); // consume _Generic
        self.expect(&TokenKind::LParen);
        let controlling = self.parse_assignment_expr();
        self.expect(&TokenKind::Comma);
        let mut associations = Vec::new();
        loop {
            if matches!(self.peek(), TokenKind::RParen) {
                break;
            }
            let type_spec = if matches!(self.peek(), TokenKind::Default) {
                self.advance();
                None
            } else {
                if let Some(mut ts) = self.parse_type_specifier() {
                    while matches!(self.peek(), TokenKind::Star) {
                        self.advance();
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
}
