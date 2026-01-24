//! Dead static function elimination via reference collection.
//!
//! This module implements dead static function elimination by walking AST compound
//! statements to collect all referenced function names. Functions that are declared
//! as static but never referenced from any non-static function body or global
//! initializer can be safely skipped during lowering, providing a significant
//! performance win for translation units that include many header-defined static
//! or inline functions.

use std::collections::HashSet;
use crate::frontend::parser::ast::*;
use super::lowering::Lowerer;

impl Lowerer {
    /// Collect all function names referenced from non-static function bodies and
    /// global initializers. Static/inline functions from headers that are never
    /// referenced can be skipped during lowering for a significant performance win.
    pub(super) fn collect_referenced_static_functions(&self, tu: &TranslationUnit) -> HashSet<String> {
        let mut referenced = HashSet::new();

        for decl in &tu.decls {
            match decl {
                ExternalDecl::FunctionDef(func) => {
                    // Non-static functions always reference identifiers that might be static functions
                    // Static functions also reference other static functions
                    self.collect_refs_from_compound(&func.body, &mut referenced);
                }
                ExternalDecl::Declaration(decl) => {
                    // Check global variable initializers for function references
                    for declarator in &decl.declarators {
                        if let Some(ref init) = declarator.init {
                            self.collect_refs_from_initializer(init, &mut referenced);
                        }
                    }
                }
            }
        }

        // Transitively close: if static func A references static func B,
        // and A is referenced, then B is also referenced.
        // We already collected all references from all function bodies above,
        // including from static functions, so the transitive closure is handled.

        referenced
    }

    /// Collect function name references from a compound statement.
    pub(super) fn collect_refs_from_compound(&self, compound: &CompoundStmt, refs: &mut HashSet<String>) {
        for item in &compound.items {
            match item {
                BlockItem::Declaration(decl) => {
                    for declarator in &decl.declarators {
                        if let Some(ref init) = declarator.init {
                            self.collect_refs_from_initializer(init, refs);
                        }
                    }
                }
                BlockItem::Statement(stmt) => {
                    self.collect_refs_from_stmt(stmt, refs);
                }
            }
        }
    }

    /// Collect function name references from a statement.
    pub(super) fn collect_refs_from_stmt(&self, stmt: &Stmt, refs: &mut HashSet<String>) {
        match stmt {
            Stmt::Expr(Some(expr)) => {
                self.collect_refs_from_expr(expr, refs);
            }
            Stmt::Return(Some(expr), _) => {
                self.collect_refs_from_expr(expr, refs);
            }
            Stmt::Compound(compound) => {
                self.collect_refs_from_compound(compound, refs);
            }
            Stmt::If(cond, then_s, else_s, _) => {
                self.collect_refs_from_expr(cond, refs);
                self.collect_refs_from_stmt(then_s, refs);
                if let Some(e) = else_s {
                    self.collect_refs_from_stmt(e, refs);
                }
            }
            Stmt::While(cond, body, _) => {
                self.collect_refs_from_expr(cond, refs);
                self.collect_refs_from_stmt(body, refs);
            }
            Stmt::DoWhile(body, cond, _) => {
                self.collect_refs_from_stmt(body, refs);
                self.collect_refs_from_expr(cond, refs);
            }
            Stmt::For(init, cond, inc, body, _) => {
                if let Some(init) = init {
                    match init.as_ref() {
                        ForInit::Expr(e) => self.collect_refs_from_expr(e, refs),
                        ForInit::Declaration(d) => {
                            for declarator in &d.declarators {
                                if let Some(ref init) = declarator.init {
                                    self.collect_refs_from_initializer(init, refs);
                                }
                            }
                        }
                    }
                }
                if let Some(c) = cond { self.collect_refs_from_expr(c, refs); }
                if let Some(i) = inc { self.collect_refs_from_expr(i, refs); }
                self.collect_refs_from_stmt(body, refs);
            }
            Stmt::Switch(expr, body, _) => {
                self.collect_refs_from_expr(expr, refs);
                self.collect_refs_from_stmt(body, refs);
            }
            Stmt::Case(expr, stmt, _) => {
                self.collect_refs_from_expr(expr, refs);
                self.collect_refs_from_stmt(stmt, refs);
            }
            Stmt::Default(stmt, _) | Stmt::Label(_, stmt, _) => {
                self.collect_refs_from_stmt(stmt, refs);
            }
            Stmt::GotoIndirect(expr, _) => {
                self.collect_refs_from_expr(expr, refs);
            }
            Stmt::InlineAsm { inputs, outputs, .. } => {
                for op in inputs.iter().chain(outputs.iter()) {
                    self.collect_refs_from_expr(&op.expr, refs);
                }
            }
            _ => {}
        }
    }

    /// Collect function name references from an expression.
    pub(super) fn collect_refs_from_expr(&self, expr: &Expr, refs: &mut HashSet<String>) {
        match expr {
            Expr::Identifier(name, _) => {
                if self.known_functions.contains(name) {
                    refs.insert(name.clone());
                }
            }
            Expr::FunctionCall(callee, args, _) => {
                self.collect_refs_from_expr(callee, refs);
                for arg in args { self.collect_refs_from_expr(arg, refs); }
            }
            Expr::BinaryOp(_, lhs, rhs, _) | Expr::Assign(lhs, rhs, _)
            | Expr::CompoundAssign(_, lhs, rhs, _) => {
                self.collect_refs_from_expr(lhs, refs);
                self.collect_refs_from_expr(rhs, refs);
            }
            Expr::UnaryOp(_, operand, _) | Expr::PostfixOp(_, operand, _)
            | Expr::Cast(_, operand, _) => {
                self.collect_refs_from_expr(operand, refs);
            }
            Expr::Conditional(c, t, f, _) => {
                self.collect_refs_from_expr(c, refs);
                self.collect_refs_from_expr(t, refs);
                self.collect_refs_from_expr(f, refs);
            }
            Expr::Comma(lhs, rhs, _) => {
                self.collect_refs_from_expr(lhs, refs);
                self.collect_refs_from_expr(rhs, refs);
            }
            Expr::MemberAccess(base, _, _) | Expr::PointerMemberAccess(base, _, _) => {
                self.collect_refs_from_expr(base, refs);
            }
            Expr::ArraySubscript(base, idx, _) => {
                self.collect_refs_from_expr(base, refs);
                self.collect_refs_from_expr(idx, refs);
            }
            Expr::Deref(inner, _) | Expr::AddressOf(inner, _) | Expr::VaArg(inner, _, _) => {
                self.collect_refs_from_expr(inner, refs);
            }
            Expr::CompoundLiteral(_, init, _) => {
                self.collect_refs_from_initializer(init, refs);
            }
            Expr::StmtExpr(compound, _) => {
                self.collect_refs_from_compound(compound, refs);
            }
            Expr::GenericSelection(ctrl, assocs, _) => {
                self.collect_refs_from_expr(ctrl, refs);
                for assoc in assocs {
                    self.collect_refs_from_expr(&assoc.expr, refs);
                }
            }
            _ => {}
        }
    }

    /// Collect function name references from an initializer.
    pub(super) fn collect_refs_from_initializer(&self, init: &Initializer, refs: &mut HashSet<String>) {
        match init {
            Initializer::Expr(e) => self.collect_refs_from_expr(e, refs),
            Initializer::List(items) => {
                for item in items {
                    self.collect_refs_from_initializer(&item.init, refs);
                }
            }
        }
    }
}
