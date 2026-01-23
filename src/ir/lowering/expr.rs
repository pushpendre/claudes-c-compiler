//! Expression lowering: converts AST Expr nodes to IR instructions.
//!
//! The main entry point is `lower_expr()`, which dispatches to focused helpers
//! for each expression category. This keeps each function small and testable.

use crate::frontend::parser::ast::*;
use crate::frontend::sema::builtins::{self, BuiltinKind, BuiltinIntrinsic};
use crate::ir::ir::*;
use crate::common::types::{IrType, CType};
use super::lowering::Lowerer;

impl Lowerer {
    /// Lower a condition expression, ensuring floating-point values are properly
    /// tested for truthiness. The backend's CondBranch uses integer testq, which
    /// doesn't handle -0.0 correctly (bit pattern 0x8000000000000000 is non-zero
    /// but -0.0 should be falsy). We fix this by masking off the sign bit so that
    /// both +0.0 and -0.0 become zero, while NaN and non-zero values remain truthy.
    pub(super) fn lower_condition_expr(&mut self, expr: &Expr) -> Operand {
        let expr_ty = self.infer_expr_type(expr);
        let val = self.lower_expr(expr);
        if matches!(expr_ty, IrType::F32 | IrType::F64 | IrType::F128) {
            // Mask off the sign bit: AND with 0x7FFF... so that -0.0 becomes +0.0 (all zeros).
            // NaN has non-zero exponent+mantissa bits, so it stays truthy.
            let mask = match expr_ty {
                IrType::F32 => Operand::Const(IrConst::I64(0x7FFFFFFFi64)),
                _ => Operand::Const(IrConst::I64(0x7FFFFFFFFFFFFFFFi64)),
            };
            let result = self.fresh_value();
            self.emit(Instruction::BinOp {
                dest: result,
                op: IrBinOp::And,
                lhs: val,
                rhs: mask,
                ty: IrType::I64,
            });
            Operand::Value(result)
        } else {
            val
        }
    }

    pub(super) fn lower_expr(&mut self, expr: &Expr) -> Operand {
        match expr {
            // Literals
            Expr::IntLiteral(val, _) => Operand::Const(IrConst::I64(*val)),
            Expr::UIntLiteral(val, _) => Operand::Const(IrConst::I64(*val as i64)),
            Expr::LongLiteral(val, _) => Operand::Const(IrConst::I64(*val)),
            Expr::ULongLiteral(val, _) => Operand::Const(IrConst::I64(*val as i64)),
            Expr::FloatLiteral(val, _) => Operand::Const(IrConst::F64(*val)),
            Expr::FloatLiteralF32(val, _) => Operand::Const(IrConst::F32(*val as f32)),
            // Long double literals: use IrConst::LongDouble so the type is tracked as F128
            // for correct ABI handling (16-byte passing in variadic calls, va_arg)
            Expr::FloatLiteralLongDouble(val, _) => Operand::Const(IrConst::LongDouble(*val)),
            Expr::CharLiteral(ch, _) => Operand::Const(IrConst::I32(*ch as i32)),

            Expr::StringLiteral(s, _) => self.lower_string_literal(s),
            Expr::Identifier(name, _) => self.lower_identifier(name),
            Expr::BinaryOp(op, lhs, rhs, _) => self.lower_binary_op(op, lhs, rhs),
            Expr::UnaryOp(op, inner, _) => self.lower_unary_op(*op, inner),
            Expr::PostfixOp(op, inner, _) => self.lower_post_inc_dec(inner, *op),
            Expr::Assign(lhs, rhs, _) => self.lower_assign(lhs, rhs),
            Expr::CompoundAssign(op, lhs, rhs, _) => self.lower_compound_assign(op, lhs, rhs),
            Expr::FunctionCall(func, args, _) => self.lower_function_call(func, args),
            Expr::Conditional(cond, then_expr, else_expr, _) => {
                self.lower_conditional(cond, then_expr, else_expr)
            }
            Expr::Cast(ref target_type, inner, _) => self.lower_cast(target_type, inner),
            Expr::CompoundLiteral(ref type_spec, ref init, _) => {
                self.lower_compound_literal(type_spec, init)
            }
            Expr::Sizeof(arg, _) => self.lower_sizeof(arg),
            Expr::Alignof(ref type_spec, _) => {
                let align = self.alignof_type(type_spec);
                Operand::Const(IrConst::I64(align as i64))
            }
            Expr::AddressOf(inner, _) => self.lower_address_of(inner),
            Expr::Deref(inner, _) => self.lower_deref(inner),
            Expr::ArraySubscript(base, index, _) => self.lower_array_subscript(expr, base, index),
            Expr::MemberAccess(base_expr, field_name, _) => {
                self.lower_member_access(base_expr, field_name)
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                self.lower_pointer_member_access(base_expr, field_name)
            }
            Expr::Comma(lhs, rhs, _) => {
                self.lower_expr(lhs);
                self.lower_expr(rhs)
            }
            Expr::StmtExpr(compound, _) => self.lower_stmt_expr(compound),
            Expr::VaArg(ap_expr, type_spec, _) => self.lower_va_arg(ap_expr, type_spec),
            Expr::GenericSelection(controlling, associations, _) => {
                self.lower_generic_selection(controlling, associations)
            }
            Expr::LabelAddr(label_name, _) => {
                // GCC extension: &&label - get address of a label for computed goto
                let scoped_label = self.get_or_create_user_label(label_name);
                let dest = self.fresh_value();
                self.emit(Instruction::LabelAddr { dest, label: scoped_label });
                Operand::Value(dest)
            }
        }
    }

    // -----------------------------------------------------------------------
    // Literal and identifier helpers
    // -----------------------------------------------------------------------

    fn lower_string_literal(&mut self, s: &str) -> Operand {
        let label = format!(".Lstr{}", self.next_string);
        self.next_string += 1;
        self.module.string_literals.push((label.clone(), s.to_string()));
        let dest = self.fresh_value();
        self.emit(Instruction::GlobalAddr { dest, name: label });
        Operand::Value(dest)
    }

    fn lower_identifier(&mut self, name: &str) -> Operand {
        // Predefined identifiers: __func__, __FUNCTION__, __PRETTY_FUNCTION__
        // These are implicitly defined as static const char[] containing the function name.
        if name == "__func__" || name == "__FUNCTION__" || name == "__PRETTY_FUNCTION__" {
            return self.lower_string_literal(&self.current_function_name.clone());
        }

        // NULL - treat as integer constant 0 (fallback when preprocessor doesn't expand it)
        if name == "NULL" {
            return Operand::Const(IrConst::I64(0));
        }


        // Enum constants are compile-time integer values
        if let Some(&val) = self.enum_constants.get(name) {
            return Operand::Const(IrConst::I64(val));
        }

        // Local variables: arrays/structs decay to address, scalars are loaded.
        // Check locals first so that inner-scope locals shadow outer static locals.
        if let Some(info) = self.locals.get(name).cloned() {
            // Static locals: emit fresh GlobalAddr at point of use (the declaration
            // may be in an unreachable block due to goto/switch skip)
            if let Some(ref global_name) = info.static_global_name {
                let addr = self.fresh_value();
                self.emit(Instruction::GlobalAddr { dest: addr, name: global_name.clone() });
                if info.is_array || info.is_struct {
                    return Operand::Value(addr);
                }
                let dest = self.fresh_value();
                self.emit(Instruction::Load { dest, ptr: addr, ty: info.ty });
                return Operand::Value(dest);
            }
            if info.is_array || info.is_struct {
                return Operand::Value(info.alloca);
            }
            let dest = self.fresh_value();
            self.emit(Instruction::Load { dest, ptr: info.alloca, ty: info.ty });
            return Operand::Value(dest);
        }

        // Static local variables: resolve through static_local_names to their
        // mangled global name. Checked after locals so inner-scope locals can shadow.
        if let Some(mangled) = self.static_local_names.get(name).cloned() {
            if let Some(ginfo) = self.globals.get(&mangled).cloned() {
                let addr = self.fresh_value();
                self.emit(Instruction::GlobalAddr { dest: addr, name: mangled });
                if ginfo.is_array || ginfo.is_struct {
                    return Operand::Value(addr);
                }
                let dest = self.fresh_value();
                self.emit(Instruction::Load { dest, ptr: addr, ty: ginfo.ty });
                return Operand::Value(dest);
            }
        }

        // Global variables
        if let Some(ginfo) = self.globals.get(name).cloned() {
            let addr = self.fresh_value();
            self.emit(Instruction::GlobalAddr { dest: addr, name: name.to_string() });
            if ginfo.is_array || ginfo.is_struct {
                // Arrays and structs decay to their address
                return Operand::Value(addr);
            }
            let dest = self.fresh_value();
            self.emit(Instruction::Load { dest, ptr: addr, ty: ginfo.ty });
            return Operand::Value(dest);
        }

        // Assume function reference (or unknown global)
        let dest = self.fresh_value();
        self.emit(Instruction::GlobalAddr { dest, name: name.to_string() });
        Operand::Value(dest)
    }

    // -----------------------------------------------------------------------
    // Binary operations
    // -----------------------------------------------------------------------

    fn lower_binary_op(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Operand {
        // Short-circuit operators get their own control flow
        match op {
            BinOp::LogicalAnd => return self.lower_short_circuit(lhs, rhs, true),
            BinOp::LogicalOr => return self.lower_short_circuit(lhs, rhs, false),
            _ => {}
        }

        // Pointer arithmetic: ptr +/- int, int + ptr, ptr - ptr
        if matches!(op, BinOp::Add | BinOp::Sub) {
            if let Some(result) = self.try_lower_pointer_arithmetic(op, lhs, rhs) {
                return result;
            }
        }

        // Pointer comparison: compare as integer addresses, not as the pointed-to type.
        // We use lower_expr (not lower_expr_with_type) because pointer arithmetic
        // already produces I64 results, and lower_expr_with_type would insert a
        // spurious float-to-int cast based on get_expr_type returning the element type.
        if matches!(op, BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge) {
            if self.expr_is_pointer(lhs) || self.expr_is_pointer(rhs) {
                let lhs_val = self.lower_expr(lhs);
                let rhs_val = self.lower_expr(rhs);
                let dest = self.fresh_value();
                let cmp_op = Self::binop_to_cmp(*op, true); // pointer comparison is unsigned
                self.emit(Instruction::Cmp { dest, op: cmp_op, lhs: lhs_val, rhs: rhs_val, ty: IrType::I64 });
                return Operand::Value(dest);
            }
        }

        // Regular arithmetic/comparison
        self.lower_arithmetic_binop(op, lhs, rhs)
    }

    /// Try to lower a binary op as pointer arithmetic. Returns Some if either
    /// operand is a pointer, None for regular arithmetic.
    fn try_lower_pointer_arithmetic(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Option<Operand> {
        let lhs_is_ptr = self.expr_is_pointer(lhs);
        let rhs_is_ptr = self.expr_is_pointer(rhs);

        if lhs_is_ptr && !rhs_is_ptr {
            // ptr + int or ptr - int: scale RHS by element size
            let elem_size = self.get_pointer_elem_size_from_expr(lhs);
            let lhs_val = self.lower_expr(lhs);
            let rhs_val = self.lower_expr(rhs);
            let scaled_rhs = self.scale_index(rhs_val, elem_size);
            let dest = self.fresh_value();
            let ir_op = if *op == BinOp::Add { IrBinOp::Add } else { IrBinOp::Sub };
            self.emit(Instruction::BinOp { dest, op: ir_op, lhs: lhs_val, rhs: scaled_rhs, ty: IrType::I64 });
            Some(Operand::Value(dest))
        } else if rhs_is_ptr && !lhs_is_ptr && *op == BinOp::Add {
            // int + ptr: scale LHS by element size
            let elem_size = self.get_pointer_elem_size_from_expr(rhs);
            let lhs_val = self.lower_expr(lhs);
            let rhs_val = self.lower_expr(rhs);
            let scaled_lhs = self.scale_index(lhs_val, elem_size);
            let dest = self.fresh_value();
            self.emit(Instruction::BinOp { dest, op: IrBinOp::Add, lhs: scaled_lhs, rhs: rhs_val, ty: IrType::I64 });
            Some(Operand::Value(dest))
        } else if lhs_is_ptr && rhs_is_ptr && *op == BinOp::Sub {
            // ptr - ptr: byte difference / element size
            let elem_size = self.get_pointer_elem_size_from_expr(lhs);
            let lhs_val = self.lower_expr(lhs);
            let rhs_val = self.lower_expr(rhs);
            let diff = self.fresh_value();
            self.emit(Instruction::BinOp { dest: diff, op: IrBinOp::Sub, lhs: lhs_val, rhs: rhs_val, ty: IrType::I64 });
            if elem_size > 1 {
                let scale = Operand::Const(IrConst::I64(elem_size as i64));
                let dest = self.fresh_value();
                self.emit(Instruction::BinOp { dest, op: IrBinOp::SDiv, lhs: Operand::Value(diff), rhs: scale, ty: IrType::I64 });
                Some(Operand::Value(dest))
            } else {
                Some(Operand::Value(diff))
            }
        } else {
            None // Not pointer arithmetic
        }
    }

    /// Multiply an index value by a scale factor (for pointer arithmetic).
    /// Returns the operand unchanged if scale is 1.
    fn scale_index(&mut self, index: Operand, scale: usize) -> Operand {
        if scale <= 1 {
            return index;
        }
        let scale_const = Operand::Const(IrConst::I64(scale as i64));
        let scaled = self.fresh_value();
        self.emit(Instruction::BinOp {
            dest: scaled,
            op: IrBinOp::Mul,
            lhs: index,
            rhs: scale_const,
            ty: IrType::I64,
        });
        Operand::Value(scaled)
    }

    /// Lower a non-pointer binary operation (arithmetic, bitwise, comparison).
    fn lower_arithmetic_binop(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Operand {
        // Determine common type for the operation
        let lhs_ty = self.infer_expr_type(lhs);
        let rhs_ty = self.infer_expr_type(rhs);
        let lhs_expr_ty = self.get_expr_type(lhs);
        let rhs_expr_ty = self.get_expr_type(rhs);

        // For shift operators (<<, >>), the C standard specifies:
        // - Both operands undergo integer promotion independently
        // - The result type is the promoted type of the LEFT operand
        // - The right operand's type does NOT affect the result type
        let is_shift = matches!(op, BinOp::Shl | BinOp::Shr);

        let (op_ty, is_unsigned, common_ty) = if lhs_expr_ty.is_float() || rhs_expr_ty.is_float() {
            let ft = if lhs_expr_ty == IrType::F64 || rhs_expr_ty == IrType::F64 { IrType::F64 } else { IrType::F32 };
            (ft, false, ft)
        } else if is_shift {
            // For shifts: result type is the promoted left operand type
            let promoted_lhs = Self::integer_promote(lhs_ty);
            (IrType::I64, promoted_lhs.is_unsigned(), promoted_lhs)
        } else {
            let ct = Self::common_type(lhs_ty, rhs_ty);
            (IrType::I64, ct.is_unsigned(), ct)
        };

        // For shifts, promote operands independently (not to common type)
        let (lhs_val, rhs_val) = if is_shift {
            let lhs_val = self.lower_expr_with_type(lhs, IrType::I64);
            let rhs_val = self.lower_expr_with_type(rhs, IrType::I64);
            (lhs_val, rhs_val)
        } else {
            let mut lhs_val = self.lower_expr_with_type(lhs, op_ty);
            let mut rhs_val = self.lower_expr_with_type(rhs, op_ty);
            // When common_ty is U32, operands that are I32 may be sign-extended
            // in their I64 representation. Mask both to 32 bits to ensure
            // consistent zero-extended representation for unsigned operations.
            if common_ty == IrType::U32 {
                if lhs_ty == IrType::I32 || lhs_ty == IrType::I16 || lhs_ty == IrType::I8 {
                    let masked = self.fresh_value();
                    self.emit(Instruction::Cast { dest: masked, src: lhs_val, from_ty: IrType::I64, to_ty: IrType::U32 });
                    lhs_val = Operand::Value(masked);
                }
                if rhs_ty == IrType::I32 || rhs_ty == IrType::I16 || rhs_ty == IrType::I8 {
                    let masked = self.fresh_value();
                    self.emit(Instruction::Cast { dest: masked, src: rhs_val, from_ty: IrType::I64, to_ty: IrType::U32 });
                    rhs_val = Operand::Value(masked);
                }
            }
            (lhs_val, rhs_val)
        };
        let dest = self.fresh_value();

        // Emit comparison or arithmetic instruction
        match op {
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                let cmp_op = Self::binop_to_cmp(*op, is_unsigned);
                // Use common_ty for comparisons so the backend uses the correct width
                // (e.g., U32 triggers 32-bit cmp, avoiding sign-extension mismatch)
                self.emit(Instruction::Cmp { dest, op: cmp_op, lhs: lhs_val, rhs: rhs_val, ty: common_ty });
            }
            _ => {
                let ir_op = Self::binop_to_ir(*op, is_unsigned);
                self.emit(Instruction::BinOp { dest, op: ir_op, lhs: lhs_val, rhs: rhs_val, ty: op_ty });
            }
        }

        // For 32-bit types, insert truncation to ensure correct wraparound
        self.maybe_narrow_binop_result(dest, op, common_ty)
    }

    /// Integer promotion: types narrower than int are promoted to int.
    /// Types that are int or wider are unchanged.
    fn integer_promote(ty: IrType) -> IrType {
        match ty {
            IrType::I8 | IrType::U8 | IrType::I16 | IrType::U16 => IrType::I32,
            _ => ty, // I32, U32, I64, U64 etc. stay the same
        }
    }

    /// Convert a comparison BinOp to the corresponding IrCmpOp.
    fn binop_to_cmp(op: BinOp, is_unsigned: bool) -> IrCmpOp {
        match (op, is_unsigned) {
            (BinOp::Eq, _) => IrCmpOp::Eq,
            (BinOp::Ne, _) => IrCmpOp::Ne,
            (BinOp::Lt, false) => IrCmpOp::Slt,
            (BinOp::Lt, true) => IrCmpOp::Ult,
            (BinOp::Le, false) => IrCmpOp::Sle,
            (BinOp::Le, true) => IrCmpOp::Ule,
            (BinOp::Gt, false) => IrCmpOp::Sgt,
            (BinOp::Gt, true) => IrCmpOp::Ugt,
            (BinOp::Ge, false) => IrCmpOp::Sge,
            (BinOp::Ge, true) => IrCmpOp::Uge,
            _ => unreachable!(),
        }
    }

    /// Convert an arithmetic/bitwise BinOp to the corresponding IrBinOp.
    fn binop_to_ir(op: BinOp, is_unsigned: bool) -> IrBinOp {
        match (op, is_unsigned) {
            (BinOp::Add, _) => IrBinOp::Add,
            (BinOp::Sub, _) => IrBinOp::Sub,
            (BinOp::Mul, _) => IrBinOp::Mul,
            (BinOp::Div, false) => IrBinOp::SDiv,
            (BinOp::Div, true) => IrBinOp::UDiv,
            (BinOp::Mod, false) => IrBinOp::SRem,
            (BinOp::Mod, true) => IrBinOp::URem,
            (BinOp::BitAnd, _) => IrBinOp::And,
            (BinOp::BitOr, _) => IrBinOp::Or,
            (BinOp::BitXor, _) => IrBinOp::Xor,
            (BinOp::Shl, _) => IrBinOp::Shl,
            (BinOp::Shr, false) => IrBinOp::AShr,
            (BinOp::Shr, true) => IrBinOp::LShr,
            _ => unreachable!(),
        }
    }

    /// Insert a narrowing cast after a binop if the common type is 32-bit.
    /// Comparisons always produce 0/1, so they don't need narrowing.
    fn maybe_narrow_binop_result(&mut self, dest: Value, op: &BinOp, common_ty: IrType) -> Operand {
        if (common_ty == IrType::U32 || common_ty == IrType::I32) && !op.is_comparison() {
            let narrowed = self.fresh_value();
            self.emit(Instruction::Cast {
                dest: narrowed,
                src: Operand::Value(dest),
                from_ty: IrType::I64,
                to_ty: common_ty,
            });
            Operand::Value(narrowed)
        } else {
            Operand::Value(dest)
        }
    }

    // -----------------------------------------------------------------------
    // Unary operations
    // -----------------------------------------------------------------------

    fn lower_unary_op(&mut self, op: UnaryOp, inner: &Expr) -> Operand {
        match op {
            UnaryOp::Plus => {
                // Unary plus applies integer promotion (C99 6.5.3.3)
                let val = self.lower_expr(inner);
                let inner_ty = self.infer_expr_type(inner);
                let promoted_ty = Self::integer_promote(inner_ty);
                if promoted_ty != inner_ty && inner_ty.is_integer() {
                    // Widen sub-int types to int
                    let dest = self.fresh_value();
                    self.emit(Instruction::Cast {
                        dest,
                        src: val,
                        from_ty: inner_ty,
                        to_ty: promoted_ty,
                    });
                    Operand::Value(dest)
                } else {
                    val
                }
            }
            UnaryOp::Neg => {
                let ty = self.get_expr_type(inner);
                let inner_ty = self.infer_expr_type(inner);
                let neg_ty = if ty.is_float() { ty } else { IrType::I64 };
                let val = self.lower_expr(inner);
                let dest = self.fresh_value();
                self.emit(Instruction::UnaryOp { dest, op: IrUnaryOp::Neg, src: val, ty: neg_ty });
                if !neg_ty.is_float() {
                    // Apply integer promotion: narrow to promoted type (at least I32)
                    let promoted_ty = Self::integer_promote(inner_ty);
                    self.maybe_narrow(dest, promoted_ty)
                } else {
                    Operand::Value(dest)
                }
            }
            UnaryOp::BitNot => {
                let inner_ty = self.infer_expr_type(inner);
                let val = self.lower_expr(inner);
                let dest = self.fresh_value();
                self.emit(Instruction::UnaryOp { dest, op: IrUnaryOp::Not, src: val, ty: IrType::I64 });
                // Apply integer promotion: narrow to promoted type (at least I32)
                let promoted_ty = Self::integer_promote(inner_ty);
                self.maybe_narrow(dest, promoted_ty)
            }
            UnaryOp::LogicalNot => {
                let inner_ty = self.infer_expr_type(inner);
                let val = self.lower_expr(inner);
                // For floats, mask off sign bit so -0.0 is treated as zero
                let cmp_val = if matches!(inner_ty, IrType::F32 | IrType::F64 | IrType::F128) {
                    let mask = match inner_ty {
                        IrType::F32 => Operand::Const(IrConst::I64(0x7FFFFFFFi64)),
                        _ => Operand::Const(IrConst::I64(0x7FFFFFFFFFFFFFFFi64)),
                    };
                    let masked = self.fresh_value();
                    self.emit(Instruction::BinOp {
                        dest: masked,
                        op: IrBinOp::And,
                        lhs: val,
                        rhs: mask,
                        ty: IrType::I64,
                    });
                    Operand::Value(masked)
                } else {
                    val
                };
                let dest = self.fresh_value();
                self.emit(Instruction::Cmp {
                    dest, op: IrCmpOp::Eq,
                    lhs: cmp_val, rhs: Operand::Const(IrConst::I64(0)),
                    ty: IrType::I64,
                });
                Operand::Value(dest)
            }
            UnaryOp::PreInc | UnaryOp::PreDec => self.lower_pre_inc_dec(inner, op),
        }
    }

    // -----------------------------------------------------------------------
    // Assignment
    // -----------------------------------------------------------------------

    fn lower_assign(&mut self, lhs: &Expr, rhs: &Expr) -> Operand {
        if self.expr_is_struct_value(lhs) {
            return self.lower_struct_assign(lhs, rhs);
        }

        // Check for bitfield assignment
        if let Some(result) = self.try_lower_bitfield_assign(lhs, rhs) {
            return result;
        }

        let rhs_val = self.lower_expr(rhs);
        let lhs_ty = self.get_expr_type(lhs);
        let rhs_ty = self.get_expr_type(rhs);
        let rhs_val = self.emit_implicit_cast(rhs_val, rhs_ty, lhs_ty);

        // _Bool variables clamp any value to 0 or 1
        let rhs_val = if self.is_bool_lvalue(lhs) {
            self.emit_bool_normalize(rhs_val)
        } else {
            rhs_val
        };

        if let Some(lv) = self.lower_lvalue(lhs) {
            self.store_lvalue_typed(&lv, rhs_val.clone(), lhs_ty);
            return rhs_val;
        }
        rhs_val
    }

    /// Resolve a bitfield member access expression, returning the field's address and
    /// bitfield metadata. Returns None if the expression is not a bitfield access.
    /// This helper deduplicates the common preamble in bitfield assign/compound-assign/inc-dec.
    fn resolve_bitfield_lvalue(&mut self, expr: &Expr) -> Option<(Value, IrType, u32, u32)> {
        let (base_expr, field_name, is_pointer) = match expr {
            Expr::MemberAccess(base, field, _) => (base.as_ref(), field.as_str(), false),
            Expr::PointerMemberAccess(base, field, _) => (base.as_ref(), field.as_str(), true),
            _ => return None,
        };

        let (field_offset, storage_ty, bitfield) = if is_pointer {
            self.resolve_pointer_member_access_full(base_expr, field_name)
        } else {
            self.resolve_member_access_full(base_expr, field_name)
        };

        let (bit_offset, bit_width) = bitfield?;

        // Compute base address
        let base_addr = if is_pointer {
            let ptr_val = self.lower_expr(base_expr);
            self.operand_to_value(ptr_val)
        } else {
            self.get_struct_base_addr(base_expr)
        };

        let field_addr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: field_addr,
            base: base_addr,
            offset: Operand::Const(IrConst::I64(field_offset as i64)),
            ty: storage_ty,
        });

        Some((field_addr, storage_ty, bit_offset, bit_width))
    }

    /// Mask a value to bit_width bits and return the masked operand.
    fn mask_to_bitwidth(&mut self, val: Operand, bit_width: u32) -> Operand {
        let mask = (1u64 << bit_width) - 1;
        let masked = self.fresh_value();
        self.emit(Instruction::BinOp {
            dest: masked,
            op: IrBinOp::And,
            ty: IrType::I64,
            lhs: val,
            rhs: Operand::Const(IrConst::I64(mask as i64)),
        });
        Operand::Value(masked)
    }

    /// Try to lower assignment to a bitfield member. Returns Some if the LHS is a bitfield.
    fn try_lower_bitfield_assign(&mut self, lhs: &Expr, rhs: &Expr) -> Option<Operand> {
        let (field_addr, storage_ty, bit_offset, bit_width) = self.resolve_bitfield_lvalue(lhs)?;

        // Evaluate RHS
        let rhs_val = self.lower_expr(rhs);

        // Read-modify-write the storage unit
        self.store_bitfield(field_addr, storage_ty, bit_offset, bit_width, rhs_val.clone());

        // Return the masked value (what was actually stored)
        Some(self.mask_to_bitwidth(rhs_val, bit_width))
    }

    /// Try to lower compound assignment to a bitfield member (e.g., s.bf += val).
    fn try_lower_bitfield_compound_assign(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Option<Operand> {
        let (field_addr, storage_ty, bit_offset, bit_width) = self.resolve_bitfield_lvalue(lhs)?;

        // Load and extract current bitfield value
        let loaded = self.fresh_value();
        self.emit(Instruction::Load { dest: loaded, ptr: field_addr, ty: storage_ty });
        let current_val = self.extract_bitfield(loaded, storage_ty, bit_offset, bit_width);

        // Evaluate RHS
        let rhs_val = self.lower_expr(rhs);

        // Perform the operation
        let is_unsigned = storage_ty.is_unsigned();
        let ir_op = Self::compound_assign_to_ir(op, is_unsigned);
        let result = self.fresh_value();
        self.emit(Instruction::BinOp {
            dest: result,
            op: ir_op,
            lhs: current_val,
            rhs: rhs_val,
            ty: IrType::I64,
        });

        // Store back via read-modify-write
        self.store_bitfield(field_addr, storage_ty, bit_offset, bit_width, Operand::Value(result));

        // Return the new value masked to bit_width
        Some(self.mask_to_bitwidth(Operand::Value(result), bit_width))
    }

    /// Store a value into a bitfield: load storage unit, clear field bits, OR in new value, store back.
    pub(super) fn store_bitfield(&mut self, addr: Value, storage_ty: IrType, bit_offset: u32, bit_width: u32, val: Operand) {
        let mask = (1u64 << bit_width) - 1;

        // Mask the value to bit_width bits (use I64 since backend uses 64-bit regs)
        let masked_val = self.fresh_value();
        self.emit(Instruction::BinOp {
            dest: masked_val,
            op: IrBinOp::And,
            ty: IrType::I64,
            lhs: val,
            rhs: Operand::Const(IrConst::I64(mask as i64)),
        });

        // Shift value to position
        let shifted_val = if bit_offset > 0 {
            let s = self.fresh_value();
            self.emit(Instruction::BinOp {
                dest: s,
                op: IrBinOp::Shl,
                ty: IrType::I64,
                lhs: Operand::Value(masked_val),
                rhs: Operand::Const(IrConst::I64(bit_offset as i64)),
            });
            s
        } else {
            masked_val
        };

        // Load current storage unit
        let old_val = self.fresh_value();
        self.emit(Instruction::Load { dest: old_val, ptr: addr, ty: storage_ty });

        // Clear the bitfield bits: old & ~(mask << bit_offset)
        let clear_mask = !(mask << bit_offset);
        let cleared = self.fresh_value();
        self.emit(Instruction::BinOp {
            dest: cleared,
            op: IrBinOp::And,
            ty: IrType::I64,
            lhs: Operand::Value(old_val),
            rhs: Operand::Const(IrConst::I64(clear_mask as i64)),
        });

        // OR in the new value
        let new_val = self.fresh_value();
        self.emit(Instruction::BinOp {
            dest: new_val,
            op: IrBinOp::Or,
            ty: IrType::I64,
            lhs: Operand::Value(cleared),
            rhs: Operand::Value(shifted_val),
        });

        // Store back
        self.emit(Instruction::Store { val: Operand::Value(new_val), ptr: addr, ty: storage_ty });
    }

    /// Lower struct/union assignment using memcpy.
    fn lower_struct_assign(&mut self, lhs: &Expr, rhs: &Expr) -> Operand {
        let struct_size = self.get_struct_size_for_expr(lhs);

        // For function calls returning small structs (<= 8 bytes),
        // the return value IS the struct data in rax, not an address.
        // Store it directly instead of memcpy.
        if matches!(rhs, Expr::FunctionCall(_, _, _)) && struct_size <= 8 {
            let rhs_val = self.lower_expr(rhs);
            if let Some(lv) = self.lower_lvalue(lhs) {
                let dest_addr = self.lvalue_addr(&lv);
                self.emit(Instruction::Store { val: rhs_val, ptr: dest_addr, ty: IrType::I64 });
                return Operand::Value(dest_addr);
            }
            return rhs_val;
        }

        let rhs_val = self.lower_expr(rhs);
        if let Some(lv) = self.lower_lvalue(lhs) {
            let dest_addr = self.lvalue_addr(&lv);
            let src_addr = self.operand_to_value(rhs_val);
            self.emit(Instruction::Memcpy { dest: dest_addr, src: src_addr, size: struct_size });
            return Operand::Value(dest_addr);
        }
        rhs_val
    }

    // -----------------------------------------------------------------------
    // Function calls
    // -----------------------------------------------------------------------

    fn lower_function_call(&mut self, func: &Expr, args: &[Expr]) -> Operand {
        // Resolve __builtin_* functions first
        if let Expr::Identifier(name, _) = func {
            if let Some(result) = self.try_lower_builtin_call(name, args) {
                return result;
            }
        }

        // Check if this call needs sret (returns struct > 8 bytes)
        let sret_size = if let Expr::Identifier(name, _) = func {
            self.func_meta.sret_functions.get(name).copied()
        } else {
            None
        };

        // Lower arguments with implicit casts
        let (mut arg_vals, mut arg_types) = self.lower_call_arguments(func, args);
        let dest = self.fresh_value();

        // For sret calls, allocate space and prepend hidden pointer argument
        let sret_alloca = if let Some(size) = sret_size {
            let alloca = self.fresh_value();
            self.emit(Instruction::Alloca { dest: alloca, ty: IrType::Ptr, size });
            // Prepend the alloca address as hidden first argument
            arg_vals.insert(0, Operand::Value(alloca));
            arg_types.insert(0, IrType::Ptr);
            Some(alloca)
        } else {
            None
        };

        // Determine variadic status and number of fixed (named) args
        let (call_variadic, num_fixed_args) = if let Expr::Identifier(name, _) = func {
            let variadic = self.is_function_variadic(name);
            let n_fixed = if variadic {
                // Number of declared parameters in the prototype
                self.func_meta.param_types.get(name)
                    .map(|p| p.len())
                    .unwrap_or(arg_vals.len()) // fallback: treat all as fixed
            } else {
                arg_vals.len()
            };
            (variadic, n_fixed)
        } else {
            (false, arg_vals.len())
        };

        // Dispatch: direct call, function pointer call, or indirect call
        let call_ret_ty = self.emit_call_instruction(func, dest, arg_vals, arg_types, call_variadic, num_fixed_args);

        // For sret calls, the struct data is now in the alloca - return its address
        if let Some(alloca) = sret_alloca {
            return Operand::Value(alloca);
        }

        // Narrow the result if the return type is sub-64-bit integer
        self.maybe_narrow_call_result(dest, call_ret_ty)
    }

    /// Try to lower a __builtin_* call. Returns Some(result) if handled.
    fn try_lower_builtin_call(&mut self, name: &str, args: &[Expr]) -> Option<Operand> {
        // Handle va_start/va_end/va_copy specially
        match name {
            "__builtin_va_start" => {
                // __builtin_va_start(va_list, last_named_param)
                // va_list is passed by reference (it's an array type)
                if let Some(ap_expr) = args.first() {
                    let ap_val = self.lower_expr(ap_expr);
                    let ap_ptr = self.operand_to_value(ap_val);
                    self.emit(Instruction::VaStart { va_list_ptr: ap_ptr });
                }
                return Some(Operand::Const(IrConst::I64(0)));
            }
            "__builtin_va_end" => {
                // __builtin_va_end(va_list) - typically a no-op
                if let Some(ap_expr) = args.first() {
                    let ap_val = self.lower_expr(ap_expr);
                    let ap_ptr = self.operand_to_value(ap_val);
                    self.emit(Instruction::VaEnd { va_list_ptr: ap_ptr });
                }
                return Some(Operand::Const(IrConst::I64(0)));
            }
            "__builtin_va_copy" => {
                // __builtin_va_copy(dest, src)
                if args.len() >= 2 {
                    let dest_val = self.lower_expr(&args[0]);
                    let src_val = self.lower_expr(&args[1]);
                    let dest_ptr = self.operand_to_value(dest_val);
                    let src_ptr = self.operand_to_value(src_val);
                    self.emit(Instruction::VaCopy { dest_ptr, src_ptr });
                }
                return Some(Operand::Const(IrConst::I64(0)));
            }
            _ => {}
        }

        // Handle atomic builtins
        if let Some(result) = self.try_lower_atomic_builtin(name, args) {
            return Some(result);
        }

        let builtin_info = builtins::resolve_builtin(name)?;
        match &builtin_info.kind {
            BuiltinKind::LibcAlias(libc_name) => {
                let arg_types: Vec<IrType> = args.iter().map(|a| self.get_expr_type(a)).collect();
                let arg_vals: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                let dest = self.fresh_value();
                let variadic = self.func_meta.variadic.contains(libc_name.as_str());
                let n_fixed = if variadic {
                    self.func_meta.param_types.get(libc_name.as_str()).map(|p| p.len()).unwrap_or(arg_vals.len())
                } else { arg_vals.len() };
                self.emit(Instruction::Call {
                    dest: Some(dest), func: libc_name.clone(),
                    args: arg_vals, arg_types, return_type: IrType::I64, is_variadic: variadic, num_fixed_args: n_fixed,
                });
                Some(Operand::Value(dest))
            }
            BuiltinKind::Identity => {
                Some(args.first().map_or(Operand::Const(IrConst::I64(0)), |a| self.lower_expr(a)))
            }
            BuiltinKind::ConstantI64(val) => Some(Operand::Const(IrConst::I64(*val))),
            BuiltinKind::ConstantF64(_) => Some(Operand::Const(IrConst::I64(0))), // TODO: handle float constants
            BuiltinKind::Intrinsic(intrinsic) => {
                match intrinsic {
                    BuiltinIntrinsic::FpCompare => {
                        // __builtin_isgreater(a,b) -> a > b, etc.
                        // Must use the actual argument types for correct comparison.
                        if args.len() >= 2 {
                            let lhs_ty = self.get_expr_type(&args[0]);
                            let rhs_ty = self.get_expr_type(&args[1]);
                            // Promote to the wider float type (F32 < F64)
                            let cmp_ty = if lhs_ty == IrType::F64 || rhs_ty == IrType::F64 {
                                IrType::F64
                            } else if lhs_ty == IrType::F32 || rhs_ty == IrType::F32 {
                                IrType::F32
                            } else {
                                // Integer args - promote to F64
                                IrType::F64
                            };
                            let mut lhs = self.lower_expr(&args[0]);
                            let mut rhs = self.lower_expr(&args[1]);
                            // Convert args to the comparison type if needed
                            if lhs_ty != cmp_ty {
                                let conv = self.fresh_value();
                                self.emit(Instruction::Cast {
                                    dest: conv, src: lhs, from_ty: lhs_ty, to_ty: cmp_ty,
                                });
                                lhs = Operand::Value(conv);
                            }
                            if rhs_ty != cmp_ty {
                                let conv = self.fresh_value();
                                self.emit(Instruction::Cast {
                                    dest: conv, src: rhs, from_ty: rhs_ty, to_ty: cmp_ty,
                                });
                                rhs = Operand::Value(conv);
                            }
                            let cmp_op = match name {
                                "__builtin_isgreater" => IrCmpOp::Sgt,
                                "__builtin_isgreaterequal" => IrCmpOp::Sge,
                                "__builtin_isless" => IrCmpOp::Slt,
                                "__builtin_islessequal" => IrCmpOp::Sle,
                                "__builtin_islessgreater" => IrCmpOp::Ne,
                                "__builtin_isunordered" => IrCmpOp::Ne, // approximate
                                _ => IrCmpOp::Eq,
                            };
                            let dest = self.fresh_value();
                            self.emit(Instruction::Cmp {
                                dest, op: cmp_op, lhs, rhs, ty: cmp_ty,
                            });
                            return Some(Operand::Value(dest));
                        }
                        Some(Operand::Const(IrConst::I64(0)))
                    }
                    BuiltinIntrinsic::Clz => self.lower_unary_intrinsic(name, args, IrUnaryOp::Clz),
                    BuiltinIntrinsic::Ctz => self.lower_unary_intrinsic(name, args, IrUnaryOp::Ctz),
                    BuiltinIntrinsic::Bswap => self.lower_bswap_intrinsic(name, args),
                    BuiltinIntrinsic::Popcount => self.lower_unary_intrinsic(name, args, IrUnaryOp::Popcount),
                    BuiltinIntrinsic::Parity => self.lower_parity_intrinsic(name, args),
                    _ => {
                        let cleaned_name = name.strip_prefix("__builtin_").unwrap_or(name).to_string();
                        let arg_types: Vec<IrType> = args.iter().map(|a| self.get_expr_type(a)).collect();
                        let arg_vals: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                        let dest = self.fresh_value();
                        let variadic = self.func_meta.variadic.contains(cleaned_name.as_str());
                        let n_fixed = if variadic {
                            self.func_meta.param_types.get(cleaned_name.as_str()).map(|p| p.len()).unwrap_or(arg_vals.len())
                        } else { arg_vals.len() };
                        self.emit(Instruction::Call {
                            dest: Some(dest), func: cleaned_name,
                            args: arg_vals, arg_types, return_type: IrType::I64, is_variadic: variadic, num_fixed_args: n_fixed,
                        });
                        Some(Operand::Value(dest))
                    }
                }
            }
        }
    }

    /// Determine the operand width for a suffix-encoded intrinsic (clz, ctz, popcount, etc.).
    /// Names ending in "ll" or "l" use I64, otherwise I32.
    fn intrinsic_type_from_suffix(name: &str) -> IrType {
        if name.ends_with("ll") || name.ends_with('l') { IrType::I64 } else { IrType::I32 }
    }

    /// Lower a simple unary intrinsic (CLZ, CTZ, Popcount) that takes one integer arg.
    fn lower_unary_intrinsic(&mut self, name: &str, args: &[Expr], ir_op: IrUnaryOp) -> Option<Operand> {
        if args.is_empty() {
            return Some(Operand::Const(IrConst::I64(0)));
        }
        let arg = self.lower_expr(&args[0]);
        let ty = Self::intrinsic_type_from_suffix(name);
        let dest = self.fresh_value();
        self.emit(Instruction::UnaryOp { dest, op: ir_op, src: arg, ty });
        Some(Operand::Value(dest))
    }

    /// Lower __builtin_bswap{16,32,64} - type determined by numeric suffix.
    fn lower_bswap_intrinsic(&mut self, name: &str, args: &[Expr]) -> Option<Operand> {
        if args.is_empty() {
            return Some(Operand::Const(IrConst::I64(0)));
        }
        let arg = self.lower_expr(&args[0]);
        let ty = if name.contains("64") {
            IrType::I64
        } else if name.contains("16") {
            IrType::I16
        } else {
            IrType::I32
        };
        let dest = self.fresh_value();
        self.emit(Instruction::UnaryOp { dest, op: IrUnaryOp::Bswap, src: arg, ty });
        Some(Operand::Value(dest))
    }

    /// Lower __builtin_parity{,l,ll} - implemented as popcount & 1.
    fn lower_parity_intrinsic(&mut self, name: &str, args: &[Expr]) -> Option<Operand> {
        if args.is_empty() {
            return Some(Operand::Const(IrConst::I64(0)));
        }
        let arg = self.lower_expr(&args[0]);
        let ty = Self::intrinsic_type_from_suffix(name);
        let pop = self.fresh_value();
        self.emit(Instruction::UnaryOp { dest: pop, op: IrUnaryOp::Popcount, src: arg, ty });
        let dest = self.fresh_value();
        self.emit(Instruction::BinOp {
            dest, op: IrBinOp::And,
            lhs: Operand::Value(pop), rhs: Operand::Const(IrConst::I64(1)), ty,
        });
        Some(Operand::Value(dest))
    }

    /// Try to lower a GCC atomic builtin (__atomic_* or __sync_*).
    fn try_lower_atomic_builtin(&mut self, name: &str, args: &[Expr]) -> Option<Operand> {
        // Determine the value type from the first argument (pointer type -> pointed-to type)
        // Determine the value type from the first argument (pointer -> pointee type)
        let val_ty = if !args.is_empty() {
            self.get_pointee_ir_type(&args[0]).unwrap_or(IrType::I64)
        } else {
            IrType::I64
        };

        // Parse ordering from a constant argument
        let get_ordering = |arg: &Expr| -> AtomicOrdering {
            // Try to extract the constant value (0=relaxed, 1=consume, 2=acquire, etc.)
            match arg {
                Expr::IntLiteral(v, _) => match *v as i32 {
                    0 => AtomicOrdering::Relaxed,
                    1 | 2 => AtomicOrdering::Acquire,  // consume maps to acquire
                    3 => AtomicOrdering::Release,
                    4 => AtomicOrdering::AcqRel,
                    _ => AtomicOrdering::SeqCst,
                },
                _ => AtomicOrdering::SeqCst, // default to strongest
            }
        };

        // __atomic_fetch_OP(ptr, val, order) -> old value
        if let Some(op) = match name {
            "__atomic_fetch_add" => Some(AtomicRmwOp::Add),
            "__atomic_fetch_sub" => Some(AtomicRmwOp::Sub),
            "__atomic_fetch_and" => Some(AtomicRmwOp::And),
            "__atomic_fetch_or"  => Some(AtomicRmwOp::Or),
            "__atomic_fetch_xor" => Some(AtomicRmwOp::Xor),
            "__atomic_fetch_nand" => Some(AtomicRmwOp::Nand),
            _ => None,
        } {
            if args.len() >= 3 {
                let ptr = self.lower_expr(&args[0]);
                let val = self.lower_expr(&args[1]);
                let ordering = get_ordering(&args[2]);
                let dest = self.fresh_value();
                self.emit(Instruction::AtomicRmw {
                    dest, op, ptr, val, ty: val_ty, ordering,
                });
                return Some(Operand::Value(dest));
            }
        }

        // __atomic_OP_fetch(ptr, val, order) -> new value
        if let Some(op) = match name {
            "__atomic_add_fetch" => Some((AtomicRmwOp::Add, IrBinOp::Add)),
            "__atomic_sub_fetch" => Some((AtomicRmwOp::Sub, IrBinOp::Sub)),
            "__atomic_and_fetch" => Some((AtomicRmwOp::And, IrBinOp::And)),
            "__atomic_or_fetch"  => Some((AtomicRmwOp::Or, IrBinOp::Or)),
            "__atomic_xor_fetch" => Some((AtomicRmwOp::Xor, IrBinOp::Xor)),
            "__atomic_nand_fetch" => Some((AtomicRmwOp::Nand, IrBinOp::And)), // need special handling
            _ => None,
        } {
            if args.len() >= 3 {
                let (rmw_op, bin_op) = op;
                let ptr = self.lower_expr(&args[0]);
                let val_expr = self.lower_expr(&args[1]);
                let ordering = get_ordering(&args[2]);
                // First do the atomic RMW (returns old value)
                let old_val = self.fresh_value();
                self.emit(Instruction::AtomicRmw {
                    dest: old_val, op: rmw_op, ptr, val: val_expr.clone(), ty: val_ty, ordering,
                });
                // Then compute new = old op val
                let new_val = self.fresh_value();
                if name == "__atomic_nand_fetch" {
                    // nand: new = ~(old & val)
                    let and_val = self.fresh_value();
                    self.emit(Instruction::BinOp {
                        dest: and_val, op: IrBinOp::And,
                        lhs: Operand::Value(old_val), rhs: val_expr, ty: val_ty,
                    });
                    self.emit(Instruction::UnaryOp {
                        dest: new_val, op: IrUnaryOp::Not,
                        src: Operand::Value(and_val), ty: val_ty,
                    });
                } else {
                    self.emit(Instruction::BinOp {
                        dest: new_val, op: bin_op,
                        lhs: Operand::Value(old_val), rhs: val_expr, ty: val_ty,
                    });
                }
                return Some(Operand::Value(new_val));
            }
        }

        // __atomic_exchange_n(ptr, val, order) -> old value
        if name == "__atomic_exchange_n" {
            if args.len() >= 3 {
                let ptr = self.lower_expr(&args[0]);
                let val = self.lower_expr(&args[1]);
                let ordering = get_ordering(&args[2]);
                let dest = self.fresh_value();
                self.emit(Instruction::AtomicRmw {
                    dest, op: AtomicRmwOp::Xchg, ptr, val, ty: val_ty, ordering,
                });
                return Some(Operand::Value(dest));
            }
        }

        // __atomic_compare_exchange_n(ptr, expected_ptr, desired, weak, success_order, fail_order) -> bool
        if name == "__atomic_compare_exchange_n" {
            if args.len() >= 6 {
                let ptr = self.lower_expr(&args[0]);
                let expected_ptr_op = self.lower_expr(&args[1]);
                let expected_ptr_val = self.operand_to_value(expected_ptr_op.clone());
                // Load expected value from expected_ptr
                let expected = self.fresh_value();
                self.emit(Instruction::Load { dest: expected, ptr: expected_ptr_val, ty: val_ty });
                let desired = self.lower_expr(&args[2]);
                let success_ordering = get_ordering(&args[4]);
                let failure_ordering = get_ordering(&args[5]);
                // Do cmpxchg returning old value
                let old_val = self.fresh_value();
                self.emit(Instruction::AtomicCmpxchg {
                    dest: old_val, ptr, expected: Operand::Value(expected), desired,
                    ty: val_ty, success_ordering, failure_ordering, returns_bool: false,
                });
                // Store old value back to expected_ptr (updates expected on failure)
                let expected_ptr_val2 = self.operand_to_value(expected_ptr_op);
                self.emit(Instruction::Store {
                    val: Operand::Value(old_val), ptr: expected_ptr_val2, ty: val_ty,
                });
                // Compare old == expected to produce bool result
                let result = self.fresh_value();
                self.emit(Instruction::Cmp {
                    dest: result, op: IrCmpOp::Eq,
                    lhs: Operand::Value(old_val), rhs: Operand::Value(expected),
                    ty: val_ty,
                });
                return Some(Operand::Value(result));
            }
        }

        // __atomic_compare_exchange(ptr, expected_ptr, desired_ptr, weak, success_order, fail_order) -> bool
        if name == "__atomic_compare_exchange" {
            if args.len() >= 6 {
                let ptr = self.lower_expr(&args[0]);
                let expected_ptr_op = self.lower_expr(&args[1]);
                let desired_ptr_val = self.lower_expr(&args[2]);
                // Load expected and desired from their pointers
                let expected_ptr_val = self.operand_to_value(expected_ptr_op.clone());
                let expected = self.fresh_value();
                self.emit(Instruction::Load { dest: expected, ptr: expected_ptr_val, ty: val_ty });
                let desired_ptr_v = self.operand_to_value(desired_ptr_val);
                let desired = self.fresh_value();
                self.emit(Instruction::Load { dest: desired, ptr: desired_ptr_v, ty: val_ty });
                let success_ordering = get_ordering(&args[4]);
                let failure_ordering = get_ordering(&args[5]);
                // Do cmpxchg returning old value
                let old_val = self.fresh_value();
                self.emit(Instruction::AtomicCmpxchg {
                    dest: old_val, ptr, expected: Operand::Value(expected), desired: Operand::Value(desired),
                    ty: val_ty, success_ordering, failure_ordering, returns_bool: false,
                });
                // Store old value back to expected_ptr (updates expected on failure)
                let expected_ptr_val2 = self.operand_to_value(expected_ptr_op);
                self.emit(Instruction::Store {
                    val: Operand::Value(old_val), ptr: expected_ptr_val2, ty: val_ty,
                });
                // Compare old == expected to produce bool result
                let result = self.fresh_value();
                self.emit(Instruction::Cmp {
                    dest: result, op: IrCmpOp::Eq,
                    lhs: Operand::Value(old_val), rhs: Operand::Value(expected),
                    ty: val_ty,
                });
                return Some(Operand::Value(result));
            }
        }

        // __atomic_exchange(ptr, val_ptr, ret_ptr, order) -> void
        if name == "__atomic_exchange" {
            if args.len() >= 4 {
                let ptr = self.lower_expr(&args[0]);
                let val_ptr_op = self.lower_expr(&args[1]);
                let ret_ptr_op = self.lower_expr(&args[2]);
                let ordering = get_ordering(&args[3]);
                // Load value from val_ptr
                let val_ptr_val = self.operand_to_value(val_ptr_op);
                let val = self.fresh_value();
                self.emit(Instruction::Load { dest: val, ptr: val_ptr_val, ty: val_ty });
                // Do exchange
                let old_val = self.fresh_value();
                self.emit(Instruction::AtomicRmw {
                    dest: old_val, op: AtomicRmwOp::Xchg,
                    ptr, val: Operand::Value(val), ty: val_ty, ordering,
                });
                // Store old value to ret_ptr
                let ret_ptr_val = self.operand_to_value(ret_ptr_op);
                self.emit(Instruction::Store {
                    val: Operand::Value(old_val), ptr: ret_ptr_val, ty: val_ty,
                });
                return Some(Operand::Const(IrConst::I64(0)));
            }
        }

        // __atomic_load_n(ptr, order) -> value
        if name == "__atomic_load_n" {
            if args.len() >= 2 {
                let ptr = self.lower_expr(&args[0]);
                let ordering = get_ordering(&args[1]);
                let dest = self.fresh_value();
                self.emit(Instruction::AtomicLoad {
                    dest, ptr, ty: val_ty, ordering,
                });
                return Some(Operand::Value(dest));
            }
        }

        // __atomic_load(ptr, ret_ptr, order) -> void (stores result to ret_ptr)
        if name == "__atomic_load" {
            if args.len() >= 3 {
                let ptr = self.lower_expr(&args[0]);
                let ret_ptr = self.lower_expr(&args[1]);
                let ordering = get_ordering(&args[2]);
                let loaded = self.fresh_value();
                self.emit(Instruction::AtomicLoad {
                    dest: loaded, ptr, ty: val_ty, ordering,
                });
                let ret_ptr_val = self.operand_to_value(ret_ptr);
                self.emit(Instruction::Store {
                    val: Operand::Value(loaded), ptr: ret_ptr_val, ty: val_ty,
                });
                return Some(Operand::Const(IrConst::I64(0)));
            }
        }

        // __atomic_store_n(ptr, val, order) -> void
        if name == "__atomic_store_n" {
            if args.len() >= 3 {
                let ptr = self.lower_expr(&args[0]);
                let val = self.lower_expr(&args[1]);
                let ordering = get_ordering(&args[2]);
                self.emit(Instruction::AtomicStore {
                    ptr, val, ty: val_ty, ordering,
                });
                return Some(Operand::Const(IrConst::I64(0)));
            }
        }

        // __atomic_store(ptr, val_ptr, order) -> void
        if name == "__atomic_store" {
            if args.len() >= 3 {
                let ptr = self.lower_expr(&args[0]);
                let val_ptr = self.lower_expr(&args[1]);
                let ordering = get_ordering(&args[2]);
                // Load value from val_ptr
                let val_ptr_val = self.operand_to_value(val_ptr);
                let loaded = self.fresh_value();
                self.emit(Instruction::Load { dest: loaded, ptr: val_ptr_val, ty: val_ty });
                self.emit(Instruction::AtomicStore {
                    ptr, val: Operand::Value(loaded), ty: val_ty, ordering,
                });
                return Some(Operand::Const(IrConst::I64(0)));
            }
        }

        // __atomic_test_and_set(ptr, order)
        if name == "__atomic_test_and_set" {
            if args.len() >= 2 {
                let ptr = self.lower_expr(&args[0]);
                let ordering = get_ordering(&args[1]);
                let dest = self.fresh_value();
                self.emit(Instruction::AtomicRmw {
                    dest, op: AtomicRmwOp::TestAndSet,
                    ptr, val: Operand::Const(IrConst::I64(1)),
                    ty: IrType::I8, ordering,
                });
                return Some(Operand::Value(dest));
            }
        }

        // __atomic_clear(ptr, order) - atomic store 0 to byte
        if name == "__atomic_clear" {
            if args.len() >= 2 {
                let ptr = self.lower_expr(&args[0]);
                let ordering = get_ordering(&args[1]);
                self.emit(Instruction::AtomicStore {
                    ptr, val: Operand::Const(IrConst::I8(0)),
                    ty: IrType::I8, ordering,
                });
                return Some(Operand::Const(IrConst::I64(0)));
            }
        }

        // __sync_fetch_and_OP(ptr, val) -> old value (seq_cst)
        if let Some(op) = match name {
            "__sync_fetch_and_add" => Some(AtomicRmwOp::Add),
            "__sync_fetch_and_sub" => Some(AtomicRmwOp::Sub),
            "__sync_fetch_and_and" => Some(AtomicRmwOp::And),
            "__sync_fetch_and_or"  => Some(AtomicRmwOp::Or),
            "__sync_fetch_and_xor" => Some(AtomicRmwOp::Xor),
            "__sync_fetch_and_nand" => Some(AtomicRmwOp::Nand),
            _ => None,
        } {
            if args.len() >= 2 {
                let ptr = self.lower_expr(&args[0]);
                let val = self.lower_expr(&args[1]);
                let dest = self.fresh_value();
                self.emit(Instruction::AtomicRmw {
                    dest, op, ptr, val, ty: val_ty, ordering: AtomicOrdering::SeqCst,
                });
                return Some(Operand::Value(dest));
            }
        }

        // __sync_OP_and_fetch(ptr, val) -> new value (seq_cst)
        if let Some(op_info) = match name {
            "__sync_add_and_fetch" => Some((AtomicRmwOp::Add, IrBinOp::Add)),
            "__sync_sub_and_fetch" => Some((AtomicRmwOp::Sub, IrBinOp::Sub)),
            "__sync_and_and_fetch" => Some((AtomicRmwOp::And, IrBinOp::And)),
            "__sync_or_and_fetch"  => Some((AtomicRmwOp::Or, IrBinOp::Or)),
            "__sync_xor_and_fetch" => Some((AtomicRmwOp::Xor, IrBinOp::Xor)),
            "__sync_nand_and_fetch" => Some((AtomicRmwOp::Nand, IrBinOp::And)),
            _ => None,
        } {
            if args.len() >= 2 {
                let (rmw_op, bin_op) = op_info;
                let ptr = self.lower_expr(&args[0]);
                let val_expr = self.lower_expr(&args[1]);
                let old_val = self.fresh_value();
                self.emit(Instruction::AtomicRmw {
                    dest: old_val, op: rmw_op, ptr, val: val_expr.clone(),
                    ty: val_ty, ordering: AtomicOrdering::SeqCst,
                });
                let new_val = self.fresh_value();
                if name == "__sync_nand_and_fetch" {
                    let and_val = self.fresh_value();
                    self.emit(Instruction::BinOp {
                        dest: and_val, op: IrBinOp::And,
                        lhs: Operand::Value(old_val), rhs: val_expr, ty: val_ty,
                    });
                    self.emit(Instruction::UnaryOp {
                        dest: new_val, op: IrUnaryOp::Not,
                        src: Operand::Value(and_val), ty: val_ty,
                    });
                } else {
                    self.emit(Instruction::BinOp {
                        dest: new_val, op: bin_op,
                        lhs: Operand::Value(old_val), rhs: val_expr, ty: val_ty,
                    });
                }
                return Some(Operand::Value(new_val));
            }
        }

        // __sync_val_compare_and_swap(ptr, old, new) -> old value
        if name == "__sync_val_compare_and_swap" {
            if args.len() >= 3 {
                let ptr = self.lower_expr(&args[0]);
                let expected = self.lower_expr(&args[1]);
                let desired = self.lower_expr(&args[2]);
                let dest = self.fresh_value();
                self.emit(Instruction::AtomicCmpxchg {
                    dest, ptr, expected, desired,
                    ty: val_ty,
                    success_ordering: AtomicOrdering::SeqCst,
                    failure_ordering: AtomicOrdering::SeqCst,
                    returns_bool: false,
                });
                return Some(Operand::Value(dest));
            }
        }

        // __sync_bool_compare_and_swap(ptr, old, new) -> bool
        if name == "__sync_bool_compare_and_swap" {
            if args.len() >= 3 {
                let ptr = self.lower_expr(&args[0]);
                let expected = self.lower_expr(&args[1]);
                let desired = self.lower_expr(&args[2]);
                let dest = self.fresh_value();
                self.emit(Instruction::AtomicCmpxchg {
                    dest, ptr, expected, desired,
                    ty: val_ty,
                    success_ordering: AtomicOrdering::SeqCst,
                    failure_ordering: AtomicOrdering::SeqCst,
                    returns_bool: true,
                });
                return Some(Operand::Value(dest));
            }
        }

        // __sync_lock_test_and_set(ptr, val) -> old value (acquire semantics)
        if name == "__sync_lock_test_and_set" {
            if args.len() >= 2 {
                let ptr = self.lower_expr(&args[0]);
                let val = self.lower_expr(&args[1]);
                let dest = self.fresh_value();
                self.emit(Instruction::AtomicRmw {
                    dest, op: AtomicRmwOp::Xchg, ptr, val,
                    ty: val_ty, ordering: AtomicOrdering::Acquire,
                });
                return Some(Operand::Value(dest));
            }
        }

        // __sync_lock_release(ptr) -> void (release store of 0)
        if name == "__sync_lock_release" {
            if !args.is_empty() {
                let ptr = self.lower_expr(&args[0]);
                self.emit(Instruction::AtomicStore {
                    ptr, val: Operand::Const(IrConst::I64(0)),
                    ty: val_ty, ordering: AtomicOrdering::Release,
                });
                return Some(Operand::Const(IrConst::I64(0)));
            }
        }

        // __sync_synchronize() -> void (full memory barrier)
        if name == "__sync_synchronize" {
            self.emit(Instruction::Fence { ordering: AtomicOrdering::SeqCst });
            return Some(Operand::Const(IrConst::I64(0)));
        }

        // __atomic_thread_fence(order) -> void
        if name == "__atomic_thread_fence" {
            let ordering = if !args.is_empty() { get_ordering(&args[0]) } else { AtomicOrdering::SeqCst };
            self.emit(Instruction::Fence { ordering });
            return Some(Operand::Const(IrConst::I64(0)));
        }

        // __atomic_signal_fence(order) -> void (compiler fence, not hardware)
        if name == "__atomic_signal_fence" {
            // Signal fence is a compiler-only barrier, emit as fence for correctness
            let ordering = if !args.is_empty() { get_ordering(&args[0]) } else { AtomicOrdering::SeqCst };
            self.emit(Instruction::Fence { ordering });
            return Some(Operand::Const(IrConst::I64(0)));
        }

        // __atomic_is_lock_free(size, ptr) -> bool
        // For simplicity, always return 1 (lock-free) for sizes <= 8
        if name == "__atomic_is_lock_free" {
            return Some(Operand::Const(IrConst::I64(1)));
        }

        // __atomic_always_lock_free(size, ptr) -> bool
        if name == "__atomic_always_lock_free" {
            return Some(Operand::Const(IrConst::I64(1)));
        }

        None
    }

    /// Get the IR type of the pointee for a pointer expression.
    fn get_pointee_ir_type(&self, expr: &Expr) -> Option<IrType> {
        if let Some(ctype) = self.get_expr_ctype(expr) {
            match ctype {
                CType::Pointer(inner) => {
                    return Some(IrType::from_ctype(&inner));
                }
                _ => {}
            }
        }
        None
    }

    /// Lower function call arguments, applying implicit casts for parameter types
    /// and default argument promotions for variadic args.
    fn lower_call_arguments(&mut self, func: &Expr, args: &[Expr]) -> (Vec<Operand>, Vec<IrType>) {
        let param_types: Option<Vec<IrType>> = if let Expr::Identifier(name, _) = func {
            self.func_meta.param_types.get(name).cloned()
                .or_else(|| self.func_meta.ptr_param_types.get(name).cloned())
        } else {
            None
        };

        // Get _Bool parameter flags for normalization
        let param_bool_flags: Option<Vec<bool>> = if let Expr::Identifier(name, _) = func {
            self.func_meta.param_bool_flags.get(name).cloned()
        } else {
            None
        };

        // Pre-determine variadic status for argument promotion decisions
        let pre_call_variadic = if let Expr::Identifier(name, _) = func {
            self.is_function_variadic(name)
        } else {
            false
        };

        let mut arg_types = Vec::with_capacity(args.len());
        let arg_vals: Vec<Operand> = args.iter().enumerate().map(|(i, a)| {
            let val = self.lower_expr(a);
            let arg_ty = self.get_expr_type(a);

            // Check if this parameter is _Bool and needs normalization to 0/1
            let is_bool_param = param_bool_flags.as_ref()
                .map_or(false, |flags| i < flags.len() && flags[i]);

            // Cast to declared parameter type if known
            if let Some(ref ptypes) = param_types {
                if i < ptypes.len() {
                    let param_ty = ptypes[i];
                    arg_types.push(param_ty);
                    let cast_val = self.emit_implicit_cast(val, arg_ty, param_ty);
                    // For _Bool parameters, normalize to 0/1 after cast
                    if is_bool_param {
                        return self.emit_bool_normalize(cast_val);
                    }
                    return cast_val;
                }
            }
            // Default promotion: float -> double for variadic args or when param types are known.
            // For non-variadic function pointer calls without param type info, preserve F32.
            if arg_ty == IrType::F32 && (pre_call_variadic || param_types.is_some()) {
                arg_types.push(IrType::F64);
                return self.emit_implicit_cast(val, IrType::F32, IrType::F64);
            }
            arg_types.push(arg_ty);
            val
        }).collect();

        (arg_vals, arg_types)
    }

    /// Emit the actual call instruction (direct, indirect via fptr, or general indirect).
    /// Returns the effective return type for narrowing.
    fn emit_call_instruction(
        &mut self,
        func: &Expr,
        dest: Value,
        arg_vals: Vec<Operand>,
        arg_types: Vec<IrType>,
        is_variadic: bool,
        num_fixed_args: usize,
    ) -> IrType {
        // Determine indirect call return type from function pointer CType info
        let indirect_ret_ty = self.get_func_ptr_return_ir_type(func);

        match func {
            Expr::Identifier(name, _) => {
                let is_local_fptr = self.locals.contains_key(name)
                    && !self.known_functions.contains(name);
                let is_global_fptr = !self.locals.contains_key(name)
                    && self.globals.contains_key(name)
                    && !self.known_functions.contains(name);

                if is_local_fptr {
                    let info = self.locals.get(name).unwrap().clone();
                    let base_addr = if let Some(ref global_name) = info.static_global_name {
                        let addr = self.fresh_value();
                        self.emit(Instruction::GlobalAddr { dest: addr, name: global_name.clone() });
                        addr
                    } else {
                        info.alloca
                    };
                    let ptr_val = self.fresh_value();
                    self.emit(Instruction::Load { dest: ptr_val, ptr: base_addr, ty: IrType::Ptr });
                    self.emit(Instruction::CallIndirect {
                        dest: Some(dest), func_ptr: Operand::Value(ptr_val),
                        args: arg_vals, arg_types, return_type: indirect_ret_ty, is_variadic, num_fixed_args,
                    });
                    indirect_ret_ty
                } else if is_global_fptr {
                    let addr = self.fresh_value();
                    self.emit(Instruction::GlobalAddr { dest: addr, name: name.clone() });
                    let ptr_val = self.fresh_value();
                    self.emit(Instruction::Load { dest: ptr_val, ptr: addr, ty: IrType::Ptr });
                    self.emit(Instruction::CallIndirect {
                        dest: Some(dest), func_ptr: Operand::Value(ptr_val),
                        args: arg_vals, arg_types, return_type: indirect_ret_ty, is_variadic, num_fixed_args,
                    });
                    indirect_ret_ty
                } else {
                    // Direct call - look up return type
                    let ret_ty = self.func_meta.return_types.get(name).copied().unwrap_or(IrType::I64);
                    self.emit(Instruction::Call {
                        dest: Some(dest), func: name.clone(),
                        args: arg_vals, arg_types, return_type: ret_ty, is_variadic, num_fixed_args,
                    });
                    ret_ty
                }
            }
            Expr::Deref(inner, _) => {
                // (*func_ptr)(args...) - dereference is a no-op for function pointers
                let n = arg_vals.len();
                let func_ptr = self.lower_expr(inner);
                self.emit(Instruction::CallIndirect {
                    dest: Some(dest), func_ptr, args: arg_vals, arg_types,
                    return_type: indirect_ret_ty, is_variadic: false, num_fixed_args: n,
                });
                indirect_ret_ty
            }
            _ => {
                // General expression as callee (e.g., array[i](...), struct.fptr(...))
                let n = arg_vals.len();
                let func_ptr = self.lower_expr(func);
                self.emit(Instruction::CallIndirect {
                    dest: Some(dest), func_ptr, args: arg_vals, arg_types,
                    return_type: indirect_ret_ty, is_variadic: false, num_fixed_args: n,
                });
                indirect_ret_ty
            }
        }
    }

    /// Narrow call result if return type is sub-64-bit integer.
    fn maybe_narrow_call_result(&mut self, dest: Value, ret_ty: IrType) -> Operand {
        if ret_ty != IrType::I64 && ret_ty != IrType::Ptr
            && ret_ty != IrType::Void && ret_ty.is_integer()
        {
            let narrowed = self.fresh_value();
            self.emit(Instruction::Cast {
                dest: narrowed,
                src: Operand::Value(dest),
                from_ty: IrType::I64,
                to_ty: ret_ty,
            });
            Operand::Value(narrowed)
        } else {
            Operand::Value(dest)
        }
    }

    // -----------------------------------------------------------------------
    // Conditional (ternary) operator
    // -----------------------------------------------------------------------

    fn lower_conditional(&mut self, cond: &Expr, then_expr: &Expr, else_expr: &Expr) -> Operand {
        let cond_val = self.lower_condition_expr(cond);
        let result_alloca = self.fresh_value();
        self.emit(Instruction::Alloca { dest: result_alloca, ty: IrType::I64, size: 8 });

        // Compute the common type of both branches (C "usual arithmetic conversions")
        let mut then_ty = self.get_expr_type(then_expr);
        let mut else_ty = self.get_expr_type(else_expr);
        // If either branch is a pointer or array (decays to pointer), treat as I64 (pointer)
        // to avoid truncating 64-bit pointers when the element type is smaller.
        if self.expr_is_pointer(then_expr) {
            then_ty = IrType::I64;
        }
        if self.expr_is_pointer(else_expr) {
            else_ty = IrType::I64;
        }
        let common_ty = Self::common_type(then_ty, else_ty);

        let then_label = self.fresh_label("ternary_then");
        let else_label = self.fresh_label("ternary_else");
        let end_label = self.fresh_label("ternary_end");

        self.terminate(Terminator::CondBranch {
            cond: cond_val,
            true_label: then_label.clone(),
            false_label: else_label.clone(),
        });

        self.start_block(then_label);
        let then_val = self.lower_expr(then_expr);
        let then_val = self.emit_implicit_cast(then_val, then_ty, common_ty);
        self.emit(Instruction::Store { val: then_val, ptr: result_alloca, ty: IrType::I64 });
        self.terminate(Terminator::Branch(end_label.clone()));

        self.start_block(else_label);
        let else_val = self.lower_expr(else_expr);
        let else_val = self.emit_implicit_cast(else_val, else_ty, common_ty);
        self.emit(Instruction::Store { val: else_val, ptr: result_alloca, ty: IrType::I64 });
        self.terminate(Terminator::Branch(end_label.clone()));

        self.start_block(end_label);
        let result = self.fresh_value();
        self.emit(Instruction::Load { dest: result, ptr: result_alloca, ty: IrType::I64 });
        Operand::Value(result)
    }


    // -----------------------------------------------------------------------
    // Cast expressions
    // -----------------------------------------------------------------------

    fn lower_cast(&mut self, target_type: &TypeSpecifier, inner: &Expr) -> Operand {
        let src = self.lower_expr(inner);
        let mut from_ty = self.get_expr_type(inner);
        let to_ty = self.type_spec_to_ir(target_type);

        // Correct from_ty for array/struct identifiers (which decay to pointers)
        if let Expr::Identifier(name, _) = inner {
            if let Some(info) = self.locals.get(name) {
                if info.is_array || info.is_struct {
                    from_ty = IrType::Ptr;
                }
            } else if let Some(ginfo) = self.globals.get(name) {
                if ginfo.is_array {
                    from_ty = IrType::Ptr;
                }
            }
        }

        // No-op casts: same type, cast to Ptr, or Ptr->64-bit int
        if to_ty == from_ty
            || to_ty == IrType::Ptr
            || (from_ty == IrType::Ptr && to_ty.size() == 8)
        {
            return src;
        }

        // All other casts (float<->int, float<->float, int truncation/extension)
        let dest = self.fresh_value();
        self.emit(Instruction::Cast { dest, src, from_ty, to_ty });
        Operand::Value(dest)
    }

    // -----------------------------------------------------------------------
    // Compound literals
    // -----------------------------------------------------------------------

    fn lower_compound_literal(&mut self, type_spec: &TypeSpecifier, init: &Initializer) -> Operand {
        let ty = self.type_spec_to_ir(type_spec);
        // For incomplete array types (e.g., (int[]){1,2,3}), compute size from the
        // initializer list length since sizeof_type doesn't know the element count.
        let size = match (self.resolve_type_spec(type_spec), init) {
            (TypeSpecifier::Array(ref elem, None), Initializer::List(items)) => {
                let elem_size = self.sizeof_type(elem);
                elem_size * items.len()
            }
            _ => self.sizeof_type(type_spec),
        };
        let alloca = self.fresh_value();
        self.emit(Instruction::Alloca { dest: alloca, size, ty });

        let struct_layout = self.get_struct_layout_for_type(type_spec);

        match init {
            Initializer::Expr(expr) => {
                let val = self.lower_expr(expr);
                self.emit(Instruction::Store { val, ptr: alloca, ty });
            }
            Initializer::List(items) => {
                if let Some(ref layout) = struct_layout {
                    self.init_struct_fields(alloca, items, layout);
                } else {
                    self.init_array_compound_literal(alloca, items, type_spec, ty, size);
                }
            }
        }

        // For scalar compound literals (not struct/array), return the loaded value.
        // Struct and array compound literals return the alloca address (they are lvalues).
        let is_scalar = struct_layout.is_none() && !matches!(type_spec, TypeSpecifier::Array(..));
        if is_scalar {
            let loaded = self.fresh_value();
            self.emit(Instruction::Load { dest: loaded, ptr: alloca, ty });
            Operand::Value(loaded)
        } else {
            Operand::Value(alloca)
        }
    }

    /// Initialize struct fields from an initializer list (used by both compound
    /// literals and local variable initialization).
    pub(super) fn init_struct_fields(
        &mut self,
        base: Value,
        items: &[InitializerItem],
        layout: &crate::common::types::StructLayout,
    ) {
        self.zero_init_alloca(base, layout.size);
        let mut current_field_idx = 0usize;

        for item in items {
            let desig_name = match item.designators.first() {
                Some(Designator::Field(ref name)) => Some(name.as_str()),
                _ => None,
            };
            let field_idx = match layout.resolve_init_field_idx(desig_name, current_field_idx) {
                Some(idx) => idx,
                None => break,
            };
            let field = &layout.fields[field_idx].clone();
            let field_ty = IrType::from_ctype(&field.ty);

            let field_addr = self.fresh_value();
            self.emit(Instruction::GetElementPtr {
                dest: field_addr,
                base,
                offset: Operand::Const(IrConst::I64(field.offset as i64)),
                ty: field_ty,
            });

            // Check if the field is a nested struct/union - use memcpy instead of store
            let is_struct_field = matches!(&field.ty, CType::Struct(_) | CType::Union(_));

            match &item.init {
                Initializer::Expr(expr) => {
                    if is_struct_field {
                        // Struct field: memcpy from source struct address
                        let src_addr = self.lower_expr(expr);
                        let src_val = self.operand_to_value(src_addr);
                        self.emit(Instruction::Memcpy {
                            dest: field_addr,
                            src: src_val,
                            size: field.ty.size(),
                        });
                    } else {
                        let val = self.lower_and_cast_init_expr(expr, field_ty);
                        self.emit(Instruction::Store { val, ptr: field_addr, ty: field_ty });
                    }
                }
                Initializer::List(sub_items) => {
                    if is_struct_field {
                        // Nested struct init list: recursively init the sub-struct
                        if let CType::Struct(ref st) = &field.ty {
                            let sub_layout = crate::common::types::StructLayout::for_struct(&st.fields);
                            self.init_struct_fields(field_addr, sub_items, &sub_layout);
                        }
                    }
                    // For non-struct nested list, fall through (already zero-initialized)
                }
            }

            current_field_idx = field_idx + 1;
        }
    }

    /// Initialize an array or scalar compound literal from an initializer list.
    fn init_array_compound_literal(
        &mut self,
        alloca: Value,
        items: &[InitializerItem],
        type_spec: &TypeSpecifier,
        ty: IrType,
        size: usize,
    ) {
        let elem_size = self.compound_literal_elem_size(type_spec);
        let has_designators = items.iter().any(|item| !item.designators.is_empty());
        if has_designators {
            self.zero_init_alloca(alloca, size);
        }

        let mut current_idx = 0usize;
        for item in items {
            if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                if let Some(idx_val) = self.eval_const_expr_for_designator(idx_expr) {
                    current_idx = idx_val;
                }
            }

            let val = match &item.init {
                Initializer::Expr(expr) => self.lower_expr(expr),
                _ => Operand::Const(IrConst::I64(0)),
            };

            if current_idx == 0 && items.len() == 1 && elem_size == size {
                // Simple scalar in braces
                self.emit(Instruction::Store { val, ptr: alloca, ty });
            } else {
                let offset_val = Operand::Const(IrConst::I64((current_idx * elem_size) as i64));
                let elem_ptr = self.fresh_value();
                self.emit(Instruction::GetElementPtr {
                    dest: elem_ptr, base: alloca, offset: offset_val, ty,
                });
                let store_ty = Self::ir_type_for_size(elem_size);
                self.emit(Instruction::Store { val, ptr: elem_ptr, ty: store_ty });
            }
            current_idx += 1;
        }
    }

    // -----------------------------------------------------------------------
    // Sizeof
    // -----------------------------------------------------------------------

    fn lower_sizeof(&mut self, arg: &SizeofArg) -> Operand {
        let size = match arg {
            SizeofArg::Type(ts) => self.sizeof_type(ts),
            SizeofArg::Expr(expr) => self.sizeof_expr(expr),
        };
        Operand::Const(IrConst::I64(size as i64))
    }

    /// Lower a _Generic selection expression by matching the controlling expression's
    /// type against the type associations.
    fn lower_generic_selection(&mut self, controlling: &Expr, associations: &[GenericAssociation]) -> Operand {
        // Determine the C type of the controlling expression
        let controlling_ctype = self.get_expr_ctype(controlling);
        let controlling_ir_type = self.get_expr_type(controlling);

        // Find the matching association
        let mut default_expr: Option<&Expr> = None;
        let mut matched_expr: Option<&Expr> = None;

        for assoc in associations {
            match &assoc.type_spec {
                None => {
                    // "default" association
                    default_expr = Some(&assoc.expr);
                }
                Some(type_spec) => {
                    let assoc_ctype = self.type_spec_to_ctype(type_spec);
                    // Match based on CType if available
                    if let Some(ref ctrl_ct) = controlling_ctype {
                        if self.ctype_matches_generic(ctrl_ct, &assoc_ctype) {
                            matched_expr = Some(&assoc.expr);
                            break;
                        }
                    } else {
                        // Fall back to IrType matching
                        let assoc_ir_type = self.type_spec_to_ir(type_spec);
                        if assoc_ir_type == controlling_ir_type {
                            matched_expr = Some(&assoc.expr);
                            break;
                        }
                    }
                }
            }
        }

        // Use matched expression, or default, or first available
        let selected = matched_expr.or(default_expr).unwrap_or_else(|| {
            // If no match found, use the first association's expr (shouldn't happen in valid C)
            &associations[0].expr
        });

        self.lower_expr(selected)
    }

    /// Check if a controlling CType matches a _Generic association CType.
    /// This handles the standard C11 type compatibility rules for _Generic.
    pub(super) fn ctype_matches_generic(&self, controlling: &CType, assoc: &CType) -> bool {
        // Direct match
        if controlling == assoc {
            return true;
        }
        // In C, _Generic uses the type of the controlling expression after
        // lvalue conversion, array-to-pointer decay, and function-to-pointer decay.
        // For our purposes, exact match on the CType is sufficient for most cases.
        // Handle some special cases:
        match (controlling, assoc) {
            // char and signed char are distinct in C but map the same in our CType
            (CType::Char, CType::Char) => true,
            // Long and LongLong are same size on LP64 but different types
            (CType::Long, CType::Long) => true,
            (CType::LongLong, CType::LongLong) => true,
            // Pointer types: match if same pointee
            (CType::Pointer(a), CType::Pointer(b)) => a == b,
            _ => false,
        }
    }

    // -----------------------------------------------------------------------
    // Address-of, dereference, subscript
    // -----------------------------------------------------------------------

    fn lower_address_of(&mut self, inner: &Expr) -> Operand {
        // &(compound_literal) - need to get the alloca address directly
        if let Expr::CompoundLiteral(type_spec, init, _) = inner {
            // For scalar compound literals, lower_expr would return the loaded value,
            // so we need to allocate and init directly here to get the address.
            let ty = self.type_spec_to_ir(type_spec);
            let size = self.sizeof_type(type_spec);
            let alloca = self.fresh_value();
            self.emit(Instruction::Alloca { dest: alloca, size, ty });
            let struct_layout = self.get_struct_layout_for_type(type_spec);
            match init.as_ref() {
                Initializer::Expr(expr) => {
                    let val = self.lower_expr(expr);
                    self.emit(Instruction::Store { val, ptr: alloca, ty });
                }
                Initializer::List(items) => {
                    if let Some(ref layout) = struct_layout {
                        self.init_struct_fields(alloca, items, layout);
                    } else {
                        self.init_array_compound_literal(alloca, items, type_spec, ty, size);
                    }
                }
            }
            return Operand::Value(alloca);
        }

        // Try lvalue path: &variable, &array[i], &struct.field, etc.
        if let Some(lv) = self.lower_lvalue(inner) {
            let addr = self.lvalue_addr(&lv);
            let result = self.fresh_value();
            self.emit(Instruction::GetElementPtr {
                dest: result, base: addr,
                offset: Operand::Const(IrConst::I64(0)),
                ty: IrType::Ptr,
            });
            return Operand::Value(result);
        }

        // Fallback for globals
        if let Expr::Identifier(name, _) = inner {
            let dest = self.fresh_value();
            self.emit(Instruction::GlobalAddr { dest, name: name.clone() });
            return Operand::Value(dest);
        }

        // For &*ptr, just evaluate the inner expression
        self.lower_expr(inner)
    }

    fn lower_deref(&mut self, inner: &Expr) -> Operand {
        // Dereferencing a pointer-to-array yields the array itself, which decays
        // to a pointer to its first element (same address). No load needed.
        // Dereferencing a pointer-to-struct/union yields the struct address
        // (aggregate values are represented by their addresses in the IR).
        if let Some(ctype) = self.get_expr_ctype(inner) {
            if let CType::Pointer(ref pointee) = ctype {
                if matches!(pointee.as_ref(), CType::Array(_, _) | CType::Struct(_) | CType::Union(_)) {
                    return self.lower_expr(inner);
                }
                // Dereferencing a pointer-to-struct/union yields the struct/union
                // value, which is represented by its address. No load needed.
                if matches!(pointee.as_ref(), CType::Struct(_) | CType::Union(_)) {
                    return self.lower_expr(inner);
                }
            }
        }

        let ptr = self.lower_expr(inner);
        let dest = self.fresh_value();
        let deref_ty = self.get_pointee_type_of_expr(inner).unwrap_or(IrType::I64);
        let ptr_val = self.operand_to_value(ptr);
        self.emit(Instruction::Load { dest, ptr: ptr_val, ty: deref_ty });
        Operand::Value(dest)
    }

    fn lower_array_subscript(&mut self, expr: &Expr, base: &Expr, index: &Expr) -> Operand {
        // Multi-dim arrays: a[i] where a is int[2][3] returns sub-array address
        if self.subscript_result_is_array(expr) {
            let addr = self.compute_array_element_addr(base, index);
            return Operand::Value(addr);
        }
        // Struct/union elements: return address (don't load), like member access
        if self.expr_is_struct_value(expr) {
            let addr = self.compute_array_element_addr(base, index);
            return Operand::Value(addr);
        }
        // Compute element address and load
        let elem_ty = self.get_expr_type(expr);
        let addr = self.compute_array_element_addr(base, index);
        let dest = self.fresh_value();
        self.emit(Instruction::Load { dest, ptr: addr, ty: elem_ty });
        Operand::Value(dest)
    }

    // -----------------------------------------------------------------------
    // Member access (s.field, p->field)
    // -----------------------------------------------------------------------

    /// Unified member access lowering for both direct (s.field) and pointer (p->field) access.
    fn lower_member_access_impl(&mut self, base_expr: &Expr, field_name: &str, is_pointer: bool) -> Operand {
        // Resolve field metadata and compute base address
        let (field_offset, field_ty, bitfield) = if is_pointer {
            self.resolve_pointer_member_access_full(base_expr, field_name)
        } else {
            self.resolve_member_access_full(base_expr, field_name)
        };

        let base_addr = if is_pointer {
            let ptr_val = self.lower_expr(base_expr);
            self.operand_to_value(ptr_val)
        } else {
            self.get_struct_base_addr(base_expr)
        };

        let field_addr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: field_addr, base: base_addr,
            offset: Operand::Const(IrConst::I64(field_offset as i64)),
            ty: field_ty,
        });

        // Array and struct fields return address (don't load)
        if self.field_is_array(base_expr, field_name, is_pointer)
            || self.field_is_struct(base_expr, field_name, is_pointer)
        {
            return Operand::Value(field_addr);
        }
        let dest = self.fresh_value();
        self.emit(Instruction::Load { dest, ptr: field_addr, ty: field_ty });

        // Bitfield: extract the relevant bits
        if let Some((bit_offset, bit_width)) = bitfield {
            return self.extract_bitfield(dest, field_ty, bit_offset, bit_width);
        }
        Operand::Value(dest)
    }

    fn lower_member_access(&mut self, base_expr: &Expr, field_name: &str) -> Operand {
        self.lower_member_access_impl(base_expr, field_name, false)
    }

    fn lower_pointer_member_access(&mut self, base_expr: &Expr, field_name: &str) -> Operand {
        self.lower_member_access_impl(base_expr, field_name, true)
    }

    /// Extract a bitfield value from a loaded storage unit.
    /// Shifts right by bit_offset, then masks to bit_width bits.
    /// For signed bitfields, sign-extends the result using shl+ashr in 64-bit.
    fn extract_bitfield(&mut self, loaded: Value, storage_ty: IrType, bit_offset: u32, bit_width: u32) -> Operand {
        let is_signed = storage_ty.is_signed();

        if is_signed {
            // For signed bitfields, use shift-left then arithmetic-shift-right in 64-bit
            // to sign-extend properly (x86 backend uses 64-bit registers).
            // shl by (64 - bit_offset - bit_width), then ashr by (64 - bit_width)
            let shl_amount = 64 - bit_offset - bit_width;
            let ashr_amount = 64 - bit_width;

            let mut val = Operand::Value(loaded);

            if shl_amount > 0 {
                let shifted = self.fresh_value();
                self.emit(Instruction::BinOp {
                    dest: shifted,
                    op: IrBinOp::Shl,
                    ty: IrType::I64,
                    lhs: val,
                    rhs: Operand::Const(IrConst::I64(shl_amount as i64)),
                });
                val = Operand::Value(shifted);
            }

            let result = self.fresh_value();
            self.emit(Instruction::BinOp {
                dest: result,
                op: IrBinOp::AShr,
                ty: IrType::I64,
                lhs: val,
                rhs: Operand::Const(IrConst::I64(ashr_amount as i64)),
            });
            Operand::Value(result)
        } else {
            // Unsigned: logical shift right + mask (works fine in 64-bit)
            let mut val = Operand::Value(loaded);

            if bit_offset > 0 {
                let shifted = self.fresh_value();
                self.emit(Instruction::BinOp {
                    dest: shifted,
                    op: IrBinOp::LShr,
                    ty: IrType::I64,
                    lhs: val,
                    rhs: Operand::Const(IrConst::I64(bit_offset as i64)),
                });
                val = Operand::Value(shifted);
            }

            let mask = (1u64 << bit_width) - 1;
            let masked = self.fresh_value();
            self.emit(Instruction::BinOp {
                dest: masked,
                op: IrBinOp::And,
                ty: IrType::I64,
                lhs: val,
                rhs: Operand::Const(IrConst::I64(mask as i64)),
            });
            Operand::Value(masked)
        }
    }

    /// Create an IrConst of the appropriate integer type.
    fn make_int_const(&self, ty: IrType, val: i64) -> IrConst {
        match ty {
            IrType::I8 | IrType::U8 => IrConst::I8(val as i8),
            IrType::I16 | IrType::U16 => IrConst::I16(val as i16),
            IrType::I32 | IrType::U32 => IrConst::I32(val as i32),
            _ => IrConst::I64(val),
        }
    }

    /// Check if a struct field is an array type (for array-to-pointer decay).
    fn field_is_array(&self, base_expr: &Expr, field_name: &str, is_pointer_access: bool) -> bool {
        let ctype = if is_pointer_access {
            self.resolve_pointer_member_field_ctype(base_expr, field_name)
        } else {
            self.resolve_member_field_ctype(base_expr, field_name)
        };
        ctype.map(|ct| matches!(ct, CType::Array(_, _))).unwrap_or(false)
    }

    /// Check if a struct field is a struct/union type (returns address, not loaded value).
    fn field_is_struct(&self, base_expr: &Expr, field_name: &str, is_pointer_access: bool) -> bool {
        let ctype = if is_pointer_access {
            self.resolve_pointer_member_field_ctype(base_expr, field_name)
        } else {
            self.resolve_member_field_ctype(base_expr, field_name)
        };
        ctype.map(|ct| matches!(ct, CType::Struct(_) | CType::Union(_))).unwrap_or(false)
    }

    // -----------------------------------------------------------------------
    // Statement expressions (GCC extension)
    // -----------------------------------------------------------------------

    fn lower_stmt_expr(&mut self, compound: &CompoundStmt) -> Operand {
        let mut last_val = Operand::Const(IrConst::I64(0));
        for item in &compound.items {
            match item {
                BlockItem::Statement(stmt) => {
                    if let Stmt::Expr(Some(expr)) = stmt {
                        last_val = self.lower_expr(expr);
                    } else {
                        self.lower_stmt(stmt);
                    }
                }
                BlockItem::Declaration(decl) => {
                    self.lower_local_decl(decl);
                }
            }
        }
        last_val
    }

    // -----------------------------------------------------------------------
    // Short-circuit logical operators
    // -----------------------------------------------------------------------

    /// Lower short-circuit logical operation (&& or ||).
    fn lower_short_circuit(&mut self, lhs: &Expr, rhs: &Expr, is_and: bool) -> Operand {
        let result_alloca = self.fresh_value();
        self.emit(Instruction::Alloca { dest: result_alloca, ty: IrType::I64, size: 8 });

        let prefix = if is_and { "and" } else { "or" };
        let rhs_label = self.fresh_label(&format!("{}_rhs", prefix));
        let end_label = self.fresh_label(&format!("{}_end", prefix));

        let lhs_val = self.lower_condition_expr(lhs);

        // Store default result (0 for &&, 1 for ||)
        let default_val = if is_and { 0 } else { 1 };
        self.emit(Instruction::Store {
            val: Operand::Const(IrConst::I64(default_val)),
            ptr: result_alloca, ty: IrType::I64,
        });

        let (true_label, false_label) = if is_and {
            (rhs_label.clone(), end_label.clone())
        } else {
            (end_label.clone(), rhs_label.clone())
        };
        self.terminate(Terminator::CondBranch { cond: lhs_val, true_label, false_label });

        // RHS evaluation: use lower_condition_expr to properly handle float rhs
        self.start_block(rhs_label);
        let rhs_val = self.lower_condition_expr(rhs);
        let rhs_bool = self.fresh_value();
        self.emit(Instruction::Cmp {
            dest: rhs_bool, op: IrCmpOp::Ne,
            lhs: rhs_val, rhs: Operand::Const(IrConst::I64(0)),
            ty: IrType::I64,
        });
        self.emit(Instruction::Store { val: Operand::Value(rhs_bool), ptr: result_alloca, ty: IrType::I64 });
        self.terminate(Terminator::Branch(end_label.clone()));

        self.start_block(end_label);
        let result = self.fresh_value();
        self.emit(Instruction::Load { dest: result, ptr: result_alloca, ty: IrType::I64 });
        Operand::Value(result)
    }

    // -----------------------------------------------------------------------
    // Increment/decrement operators
    // -----------------------------------------------------------------------

    pub(super) fn lower_pre_inc_dec(&mut self, inner: &Expr, op: UnaryOp) -> Operand {
        let is_inc = op == UnaryOp::PreInc;
        self.lower_inc_dec_impl(inner, is_inc, true)
    }

    pub(super) fn lower_post_inc_dec(&mut self, inner: &Expr, op: PostfixOp) -> Operand {
        let is_inc = op == PostfixOp::PostInc;
        self.lower_inc_dec_impl(inner, is_inc, false)
    }

    /// Shared implementation for pre/post increment/decrement.
    /// `return_new`: if true, returns the new value (pre-inc/dec); if false, returns original (post-inc/dec).
    fn lower_inc_dec_impl(&mut self, inner: &Expr, is_inc: bool, return_new: bool) -> Operand {
        // Check for bitfield increment/decrement: needs read-modify-write
        if let Some(result) = self.try_lower_bitfield_inc_dec(inner, is_inc, return_new) {
            return result;
        }

        let ty = self.get_expr_type(inner);
        if let Some(lv) = self.lower_lvalue(inner) {
            let loaded = self.load_lvalue_typed(&lv, ty);
            let loaded_val = self.operand_to_value(loaded.clone());
            let (step, binop_ty) = self.inc_dec_step_and_type(ty, inner);
            let result = self.fresh_value();
            let ir_op = if is_inc { IrBinOp::Add } else { IrBinOp::Sub };
            self.emit(Instruction::BinOp {
                dest: result, op: ir_op,
                lhs: Operand::Value(loaded_val), rhs: step, ty: binop_ty,
            });
            // _Bool lvalues normalize the result to 0 or 1
            let store_op = if self.is_bool_lvalue(inner) {
                self.emit_bool_normalize(Operand::Value(result))
            } else {
                Operand::Value(result)
            };
            self.store_lvalue_typed(&lv, store_op.clone(), ty);
            return if return_new { store_op } else { loaded };
        }
        self.lower_expr(inner)
    }

    /// Try to lower bitfield increment/decrement using read-modify-write.
    fn try_lower_bitfield_inc_dec(&mut self, inner: &Expr, is_inc: bool, return_new: bool) -> Option<Operand> {
        let (field_addr, storage_ty, bit_offset, bit_width) = self.resolve_bitfield_lvalue(inner)?;

        // Load and extract current bitfield value
        let loaded = self.fresh_value();
        self.emit(Instruction::Load { dest: loaded, ptr: field_addr, ty: storage_ty });
        let current_val = self.extract_bitfield(loaded, storage_ty, bit_offset, bit_width);

        // Perform inc/dec
        let ir_op = if is_inc { IrBinOp::Add } else { IrBinOp::Sub };
        let result = self.fresh_value();
        self.emit(Instruction::BinOp {
            dest: result,
            op: ir_op,
            lhs: current_val.clone(),
            rhs: Operand::Const(IrConst::I64(1)),
            ty: IrType::I64,
        });

        // Store back via read-modify-write
        self.store_bitfield(field_addr, storage_ty, bit_offset, bit_width, Operand::Value(result));

        // Return value masked to bit_width
        let ret_val = if return_new { Operand::Value(result) } else { current_val };
        Some(self.mask_to_bitwidth(ret_val, bit_width))
    }

    /// Get the step value and operation type for increment/decrement.
    /// For pointers: step = elem_size (I64), type = I64
    /// For floats: step = 1.0, type = F64/F32
    /// For integers: step = 1 (I64), type = I64
    fn inc_dec_step_and_type(&self, ty: IrType, expr: &Expr) -> (Operand, IrType) {
        if ty == IrType::Ptr {
            let elem_size = self.get_pointer_elem_size_from_expr(expr);
            (Operand::Const(IrConst::I64(elem_size as i64)), IrType::I64)
        } else if ty == IrType::F64 {
            (Operand::Const(IrConst::F64(1.0)), IrType::F64)
        } else if ty == IrType::F32 {
            (Operand::Const(IrConst::F32(1.0)), IrType::F32)
        } else {
            (Operand::Const(IrConst::I64(1)), IrType::I64)
        }
    }

    // -----------------------------------------------------------------------
    // Compound assignment (+=, -=, etc.)
    // -----------------------------------------------------------------------

    pub(super) fn lower_compound_assign(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Operand {
        // Check for bitfield compound assignment
        if let Some(result) = self.try_lower_bitfield_compound_assign(op, lhs, rhs) {
            return result;
        }

        let ty = self.get_expr_type(lhs);
        let lhs_ir_ty = self.infer_expr_type(lhs);
        let rhs_ty = self.get_expr_type(rhs);
        // Float promotion if either side is float
        let op_ty = if ty.is_float() || rhs_ty.is_float() {
            if ty == IrType::F64 || rhs_ty == IrType::F64 { IrType::F64 } else { IrType::F32 }
        } else {
            IrType::I64
        };

        let rhs_val = self.lower_expr_with_type(rhs, op_ty);
        if let Some(lv) = self.lower_lvalue(lhs) {
            let loaded = self.load_lvalue_typed(&lv, ty);
            // Cast loaded value to op_ty if needed
            let loaded_promoted = if ty != op_ty && op_ty.is_float() && !ty.is_float() {
                // int -> float promotion
                let dest = self.fresh_value();
                // Use the actual LHS IR type so codegen knows if it's unsigned
                let cast_from = if lhs_ir_ty.size() <= 4 && lhs_ir_ty.is_unsigned() {
                    // For small unsigned types, the value is already zero-extended to I64
                    // but we need to tell codegen it's unsigned for the conversion
                    IrType::U64
                } else if lhs_ir_ty == IrType::U64 {
                    IrType::U64
                } else {
                    IrType::I64
                };
                self.emit(Instruction::Cast { dest, src: loaded, from_ty: cast_from, to_ty: op_ty });
                Operand::Value(dest)
            } else if ty != op_ty && ty.is_float() && op_ty.is_float() {
                // float width promotion (e.g., F32 -> F64)
                let dest = self.fresh_value();
                self.emit(Instruction::Cast { dest, src: loaded, from_ty: ty, to_ty: op_ty });
                Operand::Value(dest)
            } else {
                loaded
            };

            // For compound assignment, use signedness of the LHS type
            let is_unsigned = lhs_ir_ty.is_unsigned();
            let ir_op = Self::compound_assign_to_ir(op, is_unsigned);

            // Scale RHS for pointer += and -=
            let actual_rhs = if ty == IrType::Ptr && matches!(op, BinOp::Add | BinOp::Sub) {
                let elem_size = self.get_pointer_elem_size_from_expr(lhs);
                self.scale_index(rhs_val, elem_size)
            } else {
                rhs_val
            };

            let result = self.fresh_value();
            self.emit(Instruction::BinOp {
                dest: result, op: ir_op, lhs: loaded_promoted, rhs: actual_rhs, ty: op_ty,
            });

            // Cast result back to lhs type if needed
            let store_val = if op_ty.is_float() && !ty.is_float() {
                // Float -> int cast for result
                // Use the actual LHS type so codegen knows if unsigned conversion needed
                let cast_to = if lhs_ir_ty == IrType::U64 || (lhs_ir_ty.is_unsigned() && lhs_ir_ty.size() <= 4) {
                    IrType::U64
                } else {
                    IrType::I64
                };
                let dest = self.fresh_value();
                self.emit(Instruction::Cast { dest, src: Operand::Value(result), from_ty: op_ty, to_ty: cast_to });
                // Also narrow to lhs type if needed (e.g., int += 0.99999 needs I64 -> I32 truncation)
                if lhs_ir_ty.size() < 8 {
                    let narrowed = self.fresh_value();
                    self.emit(Instruction::Cast {
                        dest: narrowed,
                        src: Operand::Value(dest),
                        from_ty: cast_to,
                        to_ty: lhs_ir_ty,
                    });
                    Operand::Value(narrowed)
                } else {
                    Operand::Value(dest)
                }
            } else if op_ty.is_float() && ty.is_float() && op_ty != ty {
                // Float narrowing: e.g., F64 result from `float += double_literal` needs F64→F32 cast
                let dest = self.fresh_value();
                self.emit(Instruction::Cast { dest, src: Operand::Value(result), from_ty: op_ty, to_ty: ty });
                Operand::Value(dest)
            } else if !op_ty.is_float() && (lhs_ir_ty == IrType::I32 || lhs_ir_ty == IrType::U32
                || lhs_ir_ty == IrType::I16 || lhs_ir_ty == IrType::U16
                || lhs_ir_ty == IrType::I8 || lhs_ir_ty == IrType::U8) {
                // Truncate result back to the narrower LHS type
                // This is needed for cases like `uint ^= long` where the result
                // must be truncated to uint before being stored and returned
                let narrowed = self.fresh_value();
                self.emit(Instruction::Cast {
                    dest: narrowed,
                    src: Operand::Value(result),
                    from_ty: IrType::I64,
                    to_ty: lhs_ir_ty,
                });
                Operand::Value(narrowed)
            } else {
                Operand::Value(result)
            };
            // _Bool lvalues normalize the result to 0 or 1
            let store_val = if self.is_bool_lvalue(lhs) {
                self.emit_bool_normalize(store_val)
            } else {
                store_val
            };
            self.store_lvalue_typed(&lv, store_val.clone(), ty);
            return store_val;
        }
        rhs_val
    }

    /// Map a compound assignment operator to the corresponding IrBinOp.
    /// Delegates to binop_to_ir for all known operators, defaults to Add for unknown.
    fn compound_assign_to_ir(op: &BinOp, is_unsigned: bool) -> IrBinOp {
        Self::binop_to_ir(op.clone(), is_unsigned)
    }

    // -----------------------------------------------------------------------
    // Utility helpers
    // -----------------------------------------------------------------------

    /// Convert an Operand to a Value, copying constants to a temp if needed.
    pub(super) fn operand_to_value(&mut self, op: Operand) -> Value {
        match op {
            Operand::Value(v) => v,
            Operand::Const(_) => {
                let tmp = self.fresh_value();
                self.emit(Instruction::Copy { dest: tmp, src: op });
                tmp
            }
        }
    }

    /// Load an lvalue and return the result as a raw Value (not Operand).
    fn load_lvalue_as_value(&mut self, lv: &super::lowering::LValue, ty: IrType) -> Value {
        let loaded = self.load_lvalue_typed(lv, ty);
        self.operand_to_value(loaded)
    }

    /// Insert a narrowing cast if the type is sub-64-bit (I32/U32).
    fn maybe_narrow(&mut self, val: Value, ty: IrType) -> Operand {
        if ty == IrType::U32 || ty == IrType::I32 {
            let narrowed = self.fresh_value();
            self.emit(Instruction::Cast {
                dest: narrowed,
                src: Operand::Value(val),
                from_ty: IrType::I64,
                to_ty: ty,
            });
            Operand::Value(narrowed)
        } else {
            Operand::Value(val)
        }
    }

    /// Map a byte size to the smallest IR integer type that fits.
    fn ir_type_for_size(size: usize) -> IrType {
        if size <= 1 { IrType::I8 }
        else if size <= 2 { IrType::I16 }
        else if size <= 4 { IrType::I32 }
        else { IrType::I64 }
    }

    // -----------------------------------------------------------------------
    // Type inference for binary operations
    // -----------------------------------------------------------------------

    /// Infer the IrType of an expression for signedness and width decisions.
    pub(super) fn infer_expr_type(&self, expr: &Expr) -> IrType {
        match expr {
            Expr::Identifier(name, _) => {
                if name == "__func__" || name == "__FUNCTION__" || name == "__PRETTY_FUNCTION__" {
                    return IrType::Ptr;
                }
                if self.enum_constants.contains_key(name) { return IrType::I32; }
                if let Some(info) = self.locals.get(name) { return info.ty; }
                if let Some(ginfo) = self.globals.get(name) { return ginfo.ty; }
                IrType::I64
            }
            Expr::IntLiteral(val, _) => {
                if *val >= i32::MIN as i64 && *val <= i32::MAX as i64 { IrType::I32 } else { IrType::I64 }
            }
            Expr::UIntLiteral(val, _) => {
                if *val <= u32::MAX as u64 { IrType::U32 } else { IrType::U64 }
            }
            Expr::LongLiteral(_, _) => IrType::I64,
            Expr::ULongLiteral(_, _) => IrType::U64,
            Expr::CharLiteral(_, _) => IrType::I8,
            Expr::FloatLiteral(_, _) => IrType::F64,
            Expr::FloatLiteralF32(_, _) => IrType::F32,
            Expr::FloatLiteralLongDouble(_, _) => IrType::F128, // long double: 16-byte ABI type
            Expr::Cast(ref target_type, _, _) => self.type_spec_to_ir(target_type),
            Expr::BinaryOp(op, lhs, rhs, _) => {
                if op.is_comparison() || matches!(op, BinOp::LogicalAnd | BinOp::LogicalOr) {
                    IrType::I32
                } else if matches!(op, BinOp::Shl | BinOp::Shr) {
                    // Shift operators: result type is promoted type of left operand
                    Self::integer_promote(self.infer_expr_type(lhs))
                } else {
                    Self::common_type(self.infer_expr_type(lhs), self.infer_expr_type(rhs))
                }
            }
            Expr::UnaryOp(op, inner, _) => {
                match op {
                    // Neg, BitNot, and Plus apply integer promotion (C99 6.3.1.1)
                    UnaryOp::Neg | UnaryOp::BitNot | UnaryOp::Plus => {
                        let inner_ty = self.infer_expr_type(inner);
                        if inner_ty.is_float() {
                            inner_ty
                        } else {
                            Self::integer_promote(inner_ty)
                        }
                    }
                    // LogicalNot always produces int
                    UnaryOp::LogicalNot => IrType::I32,
                    // PreInc/PreDec preserve the operand type
                    _ => self.infer_expr_type(inner),
                }
            }
            Expr::Sizeof(_, _) => IrType::U64,  // sizeof returns size_t (unsigned long)
            Expr::FunctionCall(func, _, _) => {
                if let Expr::Identifier(name, _) = func.as_ref() {
                    if let Some(&ret_ty) = self.func_meta.return_types.get(name.as_str()) {
                        return ret_ty;
                    }
                }
                IrType::I64
            }
            Expr::Deref(_, _) | Expr::ArraySubscript(_, _, _)
            | Expr::MemberAccess(_, _, _) | Expr::PointerMemberAccess(_, _, _) => {
                // Delegate to get_expr_type which handles these through CType resolution
                self.get_expr_type(expr)
            }
            Expr::PostfixOp(_, inner, _) => self.infer_expr_type(inner),
            Expr::Assign(lhs, _, _) | Expr::CompoundAssign(_, lhs, _, _) => {
                self.infer_expr_type(lhs)
            }
            Expr::Conditional(_, then_expr, else_expr, _) => {
                Self::common_type(self.infer_expr_type(then_expr), self.infer_expr_type(else_expr))
            }
            Expr::Comma(_, rhs, _) => self.infer_expr_type(rhs),
            Expr::AddressOf(_, _) => IrType::Ptr,
            Expr::StringLiteral(_, _) => IrType::Ptr,
            Expr::VaArg(_, type_spec, _) => self.resolve_va_arg_type(type_spec),
            _ => IrType::I64,
        }
    }

    /// Determine common type for usual arithmetic conversions.
    pub(super) fn common_type(a: IrType, b: IrType) -> IrType {
        if a == IrType::F64 || b == IrType::F64 { return IrType::F64; }
        if a == IrType::F32 || b == IrType::F32 { return IrType::F32; }
        if a == IrType::I64 || a == IrType::U64 || a == IrType::Ptr
            || b == IrType::I64 || b == IrType::U64 || b == IrType::Ptr
        {
            if a == IrType::U64 || b == IrType::U64 { return IrType::U64; }
            return IrType::I64;
        }
        if a == IrType::U32 || b == IrType::U32 { return IrType::U32; }
        if a == IrType::I32 || b == IrType::I32 { return IrType::I32; }
        IrType::I32 // narrow types promote to int
    }

    /// Lower expression and cast to target type if needed.
    pub(super) fn lower_expr_with_type(&mut self, expr: &Expr, target_ty: IrType) -> Operand {
        let src = self.lower_expr(expr);
        let src_ty = self.get_expr_type(expr);
        self.emit_implicit_cast(src, src_ty, target_ty)
    }

    /// Insert an implicit type cast if src_ty differs from target_ty.
    pub(super) fn emit_implicit_cast(&mut self, src: Operand, src_ty: IrType, target_ty: IrType) -> Operand {
        if src_ty == target_ty { return src; }
        if target_ty == IrType::Ptr || target_ty == IrType::Void { return src; }
        if src_ty == IrType::Ptr && target_ty.is_integer() { return src; }

        // Float<->int or float<->float conversions need explicit cast
        if (target_ty.is_float() && !src_ty.is_float())
            || (!target_ty.is_float() && src_ty.is_float())
            || (target_ty.is_float() && src_ty.is_float() && target_ty != src_ty)
        {
            let dest = self.fresh_value();
            self.emit(Instruction::Cast { dest, src, from_ty: src_ty, to_ty: target_ty });
            return Operand::Value(dest);
        }

        // Integer-to-integer conversions: emit Cast when the signedness or size differs,
        // so the backend can apply proper sign/zero extension or truncation.
        if src_ty.is_integer() && target_ty.is_integer() && src_ty != target_ty {
            let dest = self.fresh_value();
            self.emit(Instruction::Cast { dest, src, from_ty: src_ty, to_ty: target_ty });
            return Operand::Value(dest);
        }
        src
    }

    /// Normalize a value for _Bool storage: emit (val != 0) to clamp to 0 or 1.
    pub(super) fn emit_bool_normalize(&mut self, val: Operand) -> Operand {
        let dest = self.fresh_value();
        self.emit(Instruction::Cmp {
            dest,
            op: IrCmpOp::Ne,
            lhs: val,
            rhs: Operand::Const(IrConst::I64(0)),
            ty: IrType::I64,
        });
        Operand::Value(dest)
    }

    /// Check if a function is variadic.
    fn is_function_variadic(&self, name: &str) -> bool {
        if self.func_meta.variadic.contains(name) { return true; }
        if self.func_meta.param_types.contains_key(name) { return false; }
        // Fallback: common variadic libc functions
        matches!(name, "printf" | "fprintf" | "sprintf" | "snprintf" | "scanf" | "sscanf"
            | "fscanf" | "dprintf" | "vprintf" | "vfprintf" | "vsprintf" | "vsnprintf"
            | "syslog" | "err" | "errx" | "warn" | "warnx" | "asprintf" | "vasprintf"
            | "open" | "fcntl" | "ioctl" | "execl" | "execlp" | "execle")
    }

    /// Determine the return type of a function pointer expression for indirect calls.
    /// Falls back to I64 if the type cannot be determined.
    fn get_func_ptr_return_ir_type(&self, func_expr: &Expr) -> IrType {
        // Try to get the CType of the function expression
        if let Some(ctype) = self.get_expr_ctype(func_expr) {
            return Self::extract_return_type_from_ctype(&ctype);
        }
        // For Deref expressions, check the inner expression's type
        if let Expr::Deref(inner, _) = func_expr {
            if let Some(ctype) = self.get_expr_ctype(inner) {
                return Self::extract_return_type_from_ctype(&ctype);
            }
        }
        IrType::I64
    }

    fn extract_return_type_from_ctype(ctype: &CType) -> IrType {
        match ctype {
            CType::Pointer(inner) => {
                match inner.as_ref() {
                    CType::Function(ft) => Self::ir_type_from_func_return(&ft.return_type),
                    // Function pointer declared as (*fp)(params) produces
                    // Pointer(Pointer(ReturnType)) from build_full_ctype.
                    // Peel one more layer to get to the return type.
                    CType::Pointer(ret) => {
                        match ret.as_ref() {
                            CType::Float => IrType::F32,
                            CType::Double => IrType::F64,
                            _ => IrType::I64,
                        }
                    }
                    // For Pointer(ReturnType) from param type_spec encoding
                    CType::Float => IrType::F32,
                    CType::Double => IrType::F64,
                    _ => IrType::I64,
                }
            }
            CType::Function(ft) => Self::ir_type_from_func_return(&ft.return_type),
            _ => IrType::I64,
        }
    }

    /// Convert a function's return CType to IrType, unwrapping the spurious Pointer
    /// layer that build_full_ctype adds for function pointer declarators like (*fp)().
    fn ir_type_from_func_return(return_type: &CType) -> IrType {
        Self::peel_ptr_from_return_type(return_type)
    }

    /// Lower __builtin_va_arg(ap, type) expression.
    fn lower_va_arg(&mut self, ap_expr: &Expr, type_spec: &TypeSpecifier) -> Operand {
        // Get the va_list pointer (ap is passed by reference since va_list is an array)
        let ap_val = self.lower_expr(ap_expr);
        let va_list_ptr = self.operand_to_value(ap_val);

        // Determine the result type from the type specifier
        let result_ty = self.resolve_va_arg_type(type_spec);

        let dest = self.fresh_value();
        self.emit(Instruction::VaArg { dest, va_list_ptr, result_ty: result_ty.clone() });
        Operand::Value(dest)
    }

    /// Resolve the type specified in va_arg to an IrType.
    pub(super) fn resolve_va_arg_type(&self, type_spec: &TypeSpecifier) -> IrType {
        match type_spec {
            TypeSpecifier::Int | TypeSpecifier::Signed => IrType::I32,
            TypeSpecifier::Long | TypeSpecifier::LongLong => IrType::I64,
            TypeSpecifier::Short => IrType::I16,
            TypeSpecifier::Char => IrType::I8,
            TypeSpecifier::UnsignedChar => IrType::U8,
            TypeSpecifier::UnsignedInt | TypeSpecifier::Unsigned => IrType::U32,
            TypeSpecifier::UnsignedLong | TypeSpecifier::UnsignedLongLong => IrType::U64,
            TypeSpecifier::UnsignedShort => IrType::U16,
            TypeSpecifier::Float => IrType::F32,
            TypeSpecifier::Double => IrType::F64,
            TypeSpecifier::LongDouble => IrType::F128,
            TypeSpecifier::Pointer(_) => IrType::Ptr,
            TypeSpecifier::Void => IrType::I64,
            TypeSpecifier::Bool => IrType::I8,
            TypeSpecifier::TypedefName(name) => {
                // Look up typedef to resolve actual type
                if let Some(resolved) = self.typedefs.get(name) {
                    self.resolve_va_arg_type(resolved)
                } else {
                    // Default: assume pointer-sized integer for unknown typedefs
                    IrType::I64
                }
            }
            TypeSpecifier::Struct(_, _) | TypeSpecifier::Union(_, _) => {
                // Structs passed via va_arg: for simplicity, treat as pointer-sized
                // The backend will load the appropriate amount of data
                IrType::I64
            }
            _ => IrType::I64,
        }
    }
}
