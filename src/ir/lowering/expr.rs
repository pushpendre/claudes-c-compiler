//! Expression lowering: converts AST Expr nodes to IR instructions.
//!
//! The main entry point is `lower_expr()`, which dispatches to focused helpers
//! for each expression category. Large subsystems are split into submodules:
//! - `expr_builtins`: __builtin_* intrinsics and FP classification
//! - `expr_atomics`: __atomic_* and __sync_* operations
//! - `expr_calls`: function call lowering, arguments, dispatch
//! - `expr_assign`: assignment, compound assignment, bitfield helpers

use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, CType};
use super::lowering::Lowerer;
use super::definitions::GlobalInfo;

impl Lowerer {
    /// Mask off the sign bit of a float value for truthiness testing.
    /// This ensures -0.0 is treated as falsy (same as +0.0) while NaN remains truthy.
    fn mask_float_sign_for_truthiness(&mut self, val: Operand, float_ty: IrType) -> Operand {
        if !matches!(float_ty, IrType::F32 | IrType::F64 | IrType::F128) {
            return val;
        }
        let (abs_mask, _, _, _, _) = Self::fp_masks(float_ty);
        let result = self.emit_binop_val(IrBinOp::And, val, Operand::Const(IrConst::I64(abs_mask)), IrType::I64);
        Operand::Value(result)
    }

    /// Lower a condition expression, ensuring floating-point values are properly
    /// tested for truthiness (masking sign bit so -0.0 is falsy).
    pub(super) fn lower_condition_expr(&mut self, expr: &Expr) -> Operand {
        let expr_ty = self.infer_expr_type(expr);
        let val = self.lower_expr(expr);
        self.mask_float_sign_for_truthiness(val, expr_ty)
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
            Expr::FloatLiteralLongDouble(val, _) => Operand::Const(IrConst::LongDouble(*val)),
            Expr::CharLiteral(ch, _) => Operand::Const(IrConst::I32(*ch as i32)),

            // Imaginary literals
            Expr::ImaginaryLiteral(val, _) => self.lower_imaginary_literal(*val, &CType::ComplexDouble),
            Expr::ImaginaryLiteralF32(val, _) => self.lower_imaginary_literal(*val, &CType::ComplexFloat),
            Expr::ImaginaryLiteralLongDouble(val, _) => self.lower_imaginary_literal(*val, &CType::ComplexLongDouble),

            Expr::StringLiteral(s, _) => self.lower_string_literal(s),
            Expr::WideStringLiteral(s, _) => self.lower_wide_string_literal(s),
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
            Expr::GnuConditional(cond, else_expr, _) => {
                self.lower_gnu_conditional(cond, else_expr)
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
                let scoped_label = self.get_or_create_user_label(label_name);
                let dest = self.fresh_value();
                self.emit(Instruction::LabelAddr { dest, label: scoped_label });
                Operand::Value(dest)
            }
            Expr::BuiltinTypesCompatibleP(ref type1, ref type2, _) => {
                let result = self.eval_types_compatible(type1, type2);
                Operand::Const(IrConst::I64(result as i64))
            }
        }
    }

    // -----------------------------------------------------------------------
    // Literal and identifier helpers
    // -----------------------------------------------------------------------

    fn lower_string_literal(&mut self, s: &str) -> Operand {
        let label = self.intern_string_literal(s);
        let dest = self.fresh_value();
        self.emit(Instruction::GlobalAddr { dest, name: label });
        Operand::Value(dest)
    }

    fn lower_wide_string_literal(&mut self, s: &str) -> Operand {
        let label = self.intern_wide_string_literal(s);
        let dest = self.fresh_value();
        self.emit(Instruction::GlobalAddr { dest, name: label });
        Operand::Value(dest)
    }

    fn load_global_var(&mut self, global_name: String, ginfo: &GlobalInfo) -> Operand {
        let addr = self.fresh_value();
        self.emit(Instruction::GlobalAddr { dest: addr, name: global_name });
        if ginfo.is_array || ginfo.is_struct {
            return Operand::Value(addr);
        }
        if let Some(ref ct) = ginfo.c_type {
            if ct.is_complex() {
                return Operand::Value(addr);
            }
        }
        let dest = self.fresh_value();
        self.emit(Instruction::Load { dest, ptr: addr, ty: ginfo.ty });
        Operand::Value(dest)
    }

    fn lower_identifier(&mut self, name: &str) -> Operand {
        if name == "__func__" || name == "__FUNCTION__" || name == "__PRETTY_FUNCTION__" {
            let name = self.func().name.clone(); return self.lower_string_literal(&name);
        }
        if name == "NULL" {
            return Operand::Const(IrConst::I64(0));
        }

        // Check locals first (shadows enum constants)
        if let Some(info) = self.func_mut().locals.get(name).cloned() {
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
            if let Some(ref ct) = info.c_type {
                if ct.is_complex() {
                    return Operand::Value(info.alloca);
                }
            }
            let dest = self.fresh_value();
            self.emit(Instruction::Load { dest, ptr: info.alloca, ty: info.ty });
            return Operand::Value(dest);
        }

        if let Some(&val) = self.types.enum_constants.get(name) {
            return Operand::Const(IrConst::I64(val));
        }

        if let Some(mangled) = self.func_mut().static_local_names.get(name).cloned() {
            if let Some(ginfo) = self.globals.get(&mangled).cloned() {
                return self.load_global_var(mangled, &ginfo);
            }
        }

        if let Some(ginfo) = self.globals.get(name).cloned() {
            return self.load_global_var(name.to_string(), &ginfo);
        }

        if !self.known_functions.contains(name) {
            eprintln!("warning: implicit declaration of '{}'", name);
        }
        let dest = self.fresh_value();
        self.emit(Instruction::GlobalAddr { dest, name: name.to_string() });
        Operand::Value(dest)
    }

    // -----------------------------------------------------------------------
    // Binary operations
    // -----------------------------------------------------------------------

    fn lower_binary_op(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Operand {
        match op {
            BinOp::LogicalAnd => return self.lower_short_circuit(lhs, rhs, true),
            BinOp::LogicalOr => return self.lower_short_circuit(lhs, rhs, false),
            _ => {}
        }

        // Complex arithmetic
        if matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div) {
            let lhs_ct = self.expr_ctype(lhs);
            let rhs_ct = self.expr_ctype(rhs);
            if lhs_ct.is_complex() || rhs_ct.is_complex() {
                return self.lower_complex_binary_op(op, lhs, rhs, &lhs_ct, &rhs_ct);
            }
        }

        // Pointer arithmetic
        if matches!(op, BinOp::Add | BinOp::Sub) {
            if let Some(result) = self.try_lower_pointer_arithmetic(op, lhs, rhs) {
                return result;
            }
        }

        // Complex equality/inequality
        if matches!(op, BinOp::Eq | BinOp::Ne) {
            let lhs_ct = self.expr_ctype(lhs);
            let rhs_ct = self.expr_ctype(rhs);
            if lhs_ct.is_complex() || rhs_ct.is_complex() {
                return self.lower_complex_comparison(op, lhs, rhs, &lhs_ct, &rhs_ct);
            }
        }

        // Pointer comparison
        if matches!(op, BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge) {
            if self.expr_is_pointer(lhs) || self.expr_is_pointer(rhs) {
                let lhs_val = self.lower_expr(lhs);
                let rhs_val = self.lower_expr(rhs);
                let cmp_op = Self::binop_to_cmp(*op, true);
                let dest = self.emit_cmp_val(cmp_op, lhs_val, rhs_val, IrType::I64);
                return Operand::Value(dest);
            }
        }

        self.lower_arithmetic_binop(op, lhs, rhs)
    }

    fn lower_complex_binary_op(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr, lhs_ct: &CType, rhs_ct: &CType) -> Operand {
        let result_ct = self.common_complex_type(lhs_ct, rhs_ct);
        let lhs_val = self.lower_expr(lhs);
        let rhs_val = self.lower_expr(rhs);
        let lhs_complex = self.convert_to_complex(lhs_val, lhs_ct, &result_ct);
        let rhs_complex = self.convert_to_complex(rhs_val, rhs_ct, &result_ct);
        let lhs_ptr = self.operand_to_value(lhs_complex);
        let rhs_ptr = self.operand_to_value(rhs_complex);

        match op {
            BinOp::Add => self.lower_complex_add(lhs_ptr, rhs_ptr, &result_ct),
            BinOp::Sub => self.lower_complex_sub(lhs_ptr, rhs_ptr, &result_ct),
            BinOp::Mul => self.lower_complex_mul(lhs_ptr, rhs_ptr, &result_ct),
            BinOp::Div => self.lower_complex_div(lhs_ptr, rhs_ptr, &result_ct),
            _ => unreachable!(),
        }
    }

    fn try_lower_pointer_arithmetic(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Option<Operand> {
        let lhs_is_ptr = self.expr_is_pointer(lhs);
        let rhs_is_ptr = self.expr_is_pointer(rhs);

        if lhs_is_ptr && !rhs_is_ptr {
            let elem_size = self.get_pointer_elem_size_from_expr(lhs);
            let lhs_val = self.lower_expr(lhs);
            let rhs_ty = self.get_expr_type(rhs);
            let rhs_val = self.lower_expr(rhs);
            // Widen the integer index to I64 (sign-extend for signed types,
            // zero-extend for unsigned) before scaling and adding to the pointer.
            let rhs_val = self.emit_implicit_cast(rhs_val, rhs_ty, IrType::I64);
            let scaled_rhs = self.scale_index(rhs_val, elem_size);
            let ir_op = if *op == BinOp::Add { IrBinOp::Add } else { IrBinOp::Sub };
            let dest = self.emit_binop_val(ir_op, lhs_val, scaled_rhs, IrType::I64);
            Some(Operand::Value(dest))
        } else if rhs_is_ptr && !lhs_is_ptr && *op == BinOp::Add {
            let elem_size = self.get_pointer_elem_size_from_expr(rhs);
            let lhs_ty = self.get_expr_type(lhs);
            let lhs_val = self.lower_expr(lhs);
            // Widen the integer index to I64 before scaling and adding to the pointer.
            let lhs_val = self.emit_implicit_cast(lhs_val, lhs_ty, IrType::I64);
            let rhs_val = self.lower_expr(rhs);
            let scaled_lhs = self.scale_index(lhs_val, elem_size);
            let dest = self.emit_binop_val(IrBinOp::Add, scaled_lhs, rhs_val, IrType::I64);
            Some(Operand::Value(dest))
        } else if lhs_is_ptr && rhs_is_ptr && *op == BinOp::Sub {
            let elem_size = self.get_pointer_elem_size_from_expr(lhs);
            let lhs_val = self.lower_expr(lhs);
            let rhs_val = self.lower_expr(rhs);
            let diff = self.emit_binop_val(IrBinOp::Sub, lhs_val, rhs_val, IrType::I64);
            if elem_size > 1 {
                let scale = Operand::Const(IrConst::I64(elem_size as i64));
                let dest = self.emit_binop_val(IrBinOp::SDiv, Operand::Value(diff), scale, IrType::I64);
                Some(Operand::Value(dest))
            } else {
                Some(Operand::Value(diff))
            }
        } else {
            None
        }
    }

    /// Multiply an index value by a scale factor (for pointer arithmetic).
    pub(super) fn scale_index(&mut self, index: Operand, scale: usize) -> Operand {
        if scale <= 1 {
            return index;
        }
        let scaled = self.emit_binop_val(IrBinOp::Mul, index, Operand::Const(IrConst::I64(scale as i64)), IrType::I64);
        Operand::Value(scaled)
    }

    fn lower_arithmetic_binop(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Operand {
        let lhs_ty = self.infer_expr_type(lhs);
        let rhs_ty = self.infer_expr_type(rhs);
        let lhs_expr_ty = self.get_expr_type(lhs);
        let rhs_expr_ty = self.get_expr_type(rhs);

        let is_shift = matches!(op, BinOp::Shl | BinOp::Shr);

        let (op_ty, is_unsigned, common_ty) = if lhs_expr_ty.is_float() || rhs_expr_ty.is_float() {
            let ft = if lhs_expr_ty == IrType::F128 || rhs_expr_ty == IrType::F128 {
                IrType::F128
            } else if lhs_expr_ty == IrType::F64 || rhs_expr_ty == IrType::F64 {
                IrType::F64
            } else {
                IrType::F32
            };
            (ft, false, ft)
        } else if is_shift {
            let promoted_lhs = Self::integer_promote(lhs_ty);
            let shift_op_ty = if promoted_lhs == IrType::I128 || promoted_lhs == IrType::U128 {
                promoted_lhs
            } else {
                IrType::I64
            };
            (shift_op_ty, promoted_lhs.is_unsigned(), promoted_lhs)
        } else {
            let ct = Self::common_type(lhs_ty, rhs_ty);
            let ot = if ct == IrType::I128 || ct == IrType::U128 { ct } else { IrType::I64 };
            (ot, ct.is_unsigned(), ct)
        };

        let (lhs_val, rhs_val) = if is_shift {
            let shift_lhs_ty = if op_ty == IrType::I128 || op_ty == IrType::U128 { op_ty } else { IrType::I64 };
            let lhs_val = self.lower_expr_with_type(lhs, shift_lhs_ty);
            let rhs_val = self.lower_expr_with_type(rhs, IrType::I64);
            (lhs_val, rhs_val)
        } else {
            let mut lhs_val = self.lower_expr_with_type(lhs, op_ty);
            let mut rhs_val = self.lower_expr_with_type(rhs, op_ty);
            if common_ty == IrType::U32 {
                if lhs_ty == IrType::I32 || lhs_ty == IrType::I16 || lhs_ty == IrType::I8 {
                    let masked = self.emit_cast_val(lhs_val, IrType::I64, IrType::U32);
                    lhs_val = Operand::Value(masked);
                }
                if rhs_ty == IrType::I32 || rhs_ty == IrType::I16 || rhs_ty == IrType::I8 {
                    let masked = self.emit_cast_val(rhs_val, IrType::I64, IrType::U32);
                    rhs_val = Operand::Value(masked);
                }
            }
            (lhs_val, rhs_val)
        };
        let dest = self.fresh_value();

        match op {
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                let cmp_op = Self::binop_to_cmp(*op, is_unsigned);
                self.emit(Instruction::Cmp { dest, op: cmp_op, lhs: lhs_val, rhs: rhs_val, ty: common_ty });
            }
            _ => {
                let ir_op = Self::binop_to_ir(*op, is_unsigned);
                self.emit(Instruction::BinOp { dest, op: ir_op, lhs: lhs_val, rhs: rhs_val, ty: op_ty });
            }
        }

        self.maybe_narrow_binop_result(dest, op, common_ty)
    }

    /// Integer promotion: types narrower than int are promoted to int.
    pub(super) fn integer_promote(ty: IrType) -> IrType {
        match ty {
            IrType::I8 | IrType::U8 | IrType::I16 | IrType::U16 => IrType::I32,
            _ => ty,
        }
    }

    /// Convert a comparison BinOp to the corresponding IrCmpOp.
    pub(super) fn binop_to_cmp(op: BinOp, is_unsigned: bool) -> IrCmpOp {
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
    pub(super) fn binop_to_ir(op: BinOp, is_unsigned: bool) -> IrBinOp {
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

    fn maybe_narrow_binop_result(&mut self, dest: Value, op: &BinOp, common_ty: IrType) -> Operand {
        if (common_ty == IrType::U32 || common_ty == IrType::I32) && !op.is_comparison() {
            let narrowed = self.emit_cast_val(Operand::Value(dest), IrType::I64, common_ty);
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
                let inner_ct = self.expr_ctype(inner);
                if inner_ct.is_complex() {
                    return self.lower_expr(inner);
                }
                let val = self.lower_expr(inner);
                let inner_ty = self.infer_expr_type(inner);
                let promoted_ty = Self::integer_promote(inner_ty);
                if promoted_ty != inner_ty && inner_ty.is_integer() {
                    let dest = self.emit_cast_val(val, inner_ty, promoted_ty);
                    Operand::Value(dest)
                } else {
                    val
                }
            }
            UnaryOp::Neg => {
                let inner_ct = self.expr_ctype(inner);
                if inner_ct.is_complex() {
                    let val = self.lower_expr(inner);
                    let ptr = self.operand_to_value(val);
                    return self.lower_complex_neg(ptr, &inner_ct);
                }
                let ty = self.get_expr_type(inner);
                let inner_ty = self.infer_expr_type(inner);
                let neg_ty = if ty.is_float() { ty } else { IrType::I64 };
                let val = self.lower_expr(inner);
                let dest = self.fresh_value();
                self.emit(Instruction::UnaryOp { dest, op: IrUnaryOp::Neg, src: val, ty: neg_ty });
                if !neg_ty.is_float() {
                    let promoted_ty = Self::integer_promote(inner_ty);
                    self.maybe_narrow(dest, promoted_ty)
                } else {
                    Operand::Value(dest)
                }
            }
            UnaryOp::BitNot => {
                let inner_ct = self.expr_ctype(inner);
                if inner_ct.is_complex() {
                    return self.lower_complex_conj(inner);
                }
                let inner_ty = self.infer_expr_type(inner);
                let val = self.lower_expr(inner);
                let dest = self.fresh_value();
                self.emit(Instruction::UnaryOp { dest, op: IrUnaryOp::Not, src: val, ty: IrType::I64 });
                let promoted_ty = Self::integer_promote(inner_ty);
                self.maybe_narrow(dest, promoted_ty)
            }
            UnaryOp::LogicalNot => {
                let inner_ty = self.infer_expr_type(inner);
                let val = self.lower_expr(inner);
                let cmp_val = self.mask_float_sign_for_truthiness(val, inner_ty);
                let dest = self.emit_cmp_val(IrCmpOp::Eq, cmp_val, Operand::Const(IrConst::I64(0)), IrType::I64);
                Operand::Value(dest)
            }
            UnaryOp::PreInc | UnaryOp::PreDec => self.lower_pre_inc_dec(inner, op),
            UnaryOp::RealPart => self.lower_complex_real_part(inner),
            UnaryOp::ImagPart => self.lower_complex_imag_part(inner),
        }
    }

    // -----------------------------------------------------------------------
    // Conditional (ternary) operator
    // -----------------------------------------------------------------------

    fn lower_conditional(&mut self, cond: &Expr, then_expr: &Expr, else_expr: &Expr) -> Operand {
        let cond_val = self.lower_condition_expr(cond);
        let result_alloca = self.fresh_value();
        self.emit(Instruction::Alloca { dest: result_alloca, ty: IrType::I64, size: 8, align: 0 });

        let mut then_ty = self.get_expr_type(then_expr);
        let mut else_ty = self.get_expr_type(else_expr);
        if self.expr_is_pointer(then_expr) { then_ty = IrType::I64; }
        if self.expr_is_pointer(else_expr) { else_ty = IrType::I64; }
        let common_ty = Self::common_type(then_ty, else_ty);

        let then_label = self.fresh_label();
        let else_label = self.fresh_label();
        let end_label = self.fresh_label();

        self.terminate(Terminator::CondBranch {
            cond: cond_val,
            true_label: then_label,
            false_label: else_label,
        });

        self.start_block(then_label);
        let then_val = self.lower_expr(then_expr);
        let then_val = self.emit_implicit_cast(then_val, then_ty, common_ty);
        self.emit(Instruction::Store { val: then_val, ptr: result_alloca, ty: IrType::I64 });
        self.terminate(Terminator::Branch(end_label));

        self.start_block(else_label);
        let else_val = self.lower_expr(else_expr);
        let else_val = self.emit_implicit_cast(else_val, else_ty, common_ty);
        self.emit(Instruction::Store { val: else_val, ptr: result_alloca, ty: IrType::I64 });
        self.terminate(Terminator::Branch(end_label));

        self.start_block(end_label);
        let result = self.fresh_value();
        self.emit(Instruction::Load { dest: result, ptr: result_alloca, ty: IrType::I64 });
        Operand::Value(result)
    }

    /// Lower GNU conditional expression: `cond ? : else_expr`
    fn lower_gnu_conditional(&mut self, cond: &Expr, else_expr: &Expr) -> Operand {
        let cond_val = self.lower_expr(cond);
        let result_alloca = self.fresh_value();
        self.emit(Instruction::Alloca { dest: result_alloca, ty: IrType::I64, size: 8, align: 0 });

        let cond_ty = self.get_expr_type(cond);
        let else_ty = self.get_expr_type(else_expr);
        let common_ty = Self::common_type(cond_ty, else_ty);

        let zero = Operand::Const(IrConst::I64(0));
        let cond_bool = self.fresh_value();
        self.emit(Instruction::Cmp {
            dest: cond_bool, op: IrCmpOp::Ne,
            lhs: cond_val.clone(), rhs: zero, ty: IrType::I64,
        });

        let then_label = self.fresh_label();
        let else_label = self.fresh_label();
        let end_label = self.fresh_label();

        self.terminate(Terminator::CondBranch {
            cond: Operand::Value(cond_bool),
            true_label: then_label,
            false_label: else_label,
        });

        self.start_block(then_label);
        let then_val = self.emit_implicit_cast(cond_val, cond_ty, common_ty);
        self.emit(Instruction::Store { val: then_val, ptr: result_alloca, ty: IrType::I64 });
        self.terminate(Terminator::Branch(end_label));

        self.start_block(else_label);
        let else_val = self.lower_expr(else_expr);
        let else_val = self.emit_implicit_cast(else_val, else_ty, common_ty);
        self.emit(Instruction::Store { val: else_val, ptr: result_alloca, ty: IrType::I64 });
        self.terminate(Terminator::Branch(end_label));

        self.start_block(end_label);
        let result = self.fresh_value();
        self.emit(Instruction::Load { dest: result, ptr: result_alloca, ty: IrType::I64 });
        Operand::Value(result)
    }

    // -----------------------------------------------------------------------
    // Cast expressions
    // -----------------------------------------------------------------------

    fn lower_cast(&mut self, target_type: &TypeSpecifier, inner: &Expr) -> Operand {
        let target_ctype = self.type_spec_to_ctype(target_type);
        let inner_ctype = self.expr_ctype(inner);

        // Handle complex type casts
        if target_ctype.is_complex() && !inner_ctype.is_complex() {
            let val = self.lower_expr(inner);
            return self.real_to_complex(val, &inner_ctype, &target_ctype);
        }
        if target_ctype.is_complex() && inner_ctype.is_complex() {
            if target_ctype == inner_ctype {
                return self.lower_expr(inner);
            }
            let val = self.lower_expr(inner);
            let ptr = self.operand_to_value(val);
            return self.complex_to_complex(ptr, &inner_ctype, &target_ctype);
        }
        if !target_ctype.is_complex() && inner_ctype.is_complex() {
            let val = self.lower_expr(inner);
            let ptr = self.operand_to_value(val);

            if target_ctype == CType::Bool {
                let real = self.load_complex_real(ptr, &inner_ctype);
                let imag = self.load_complex_imag(ptr, &inner_ctype);
                let comp_ty = Self::complex_component_ir_type(&inner_ctype);
                let zero = if comp_ty.is_float() {
                    match comp_ty {
                        IrType::F32 => Operand::Const(IrConst::F32(0.0)),
                        _ => Operand::Const(IrConst::F64(0.0)),
                    }
                } else {
                    Operand::Const(IrConst::I64(0))
                };
                let real_nz = self.emit_cmp_val(IrCmpOp::Ne, real, zero.clone(), comp_ty);
                let imag_nz = self.emit_cmp_val(IrCmpOp::Ne, imag, zero, comp_ty);
                let result = self.emit_binop_val(IrBinOp::Or, Operand::Value(real_nz), Operand::Value(imag_nz), IrType::I64);
                return Operand::Value(result);
            }

            let real = self.load_complex_real(ptr, &inner_ctype);
            let comp_ty = Self::complex_component_ir_type(&inner_ctype);
            let to_ty = self.type_spec_to_ir(target_type);
            if comp_ty != to_ty {
                let dest = self.emit_cast_val(real, comp_ty, to_ty);
                return Operand::Value(dest);
            }
            return real;
        }

        // GCC extension: cast to union type, e.g. (union convert)x
        // Creates a temporary union, stores the value into the first matching member at offset 0.
        if let CType::Union(ref key) = target_ctype {
            let union_size = self.types.struct_layouts.get(key).map(|l| l.size).unwrap_or(0);
            let src = self.lower_expr(inner);
            let from_ty = self.get_expr_type(inner);

            // Allocate stack space for the union and zero-initialize it
            let alloca = self.fresh_value();
            self.emit(Instruction::Alloca { dest: alloca, size: union_size, ty: IrType::Ptr, align: 0 });
            self.zero_init_alloca(alloca, union_size);

            // Store source value at offset 0 (all union members share offset 0)
            let store_ty = from_ty;
            self.emit(Instruction::Store { val: src, ptr: alloca, ty: store_ty });

            return Operand::Value(alloca);
        }

        // GCC extension: cast to struct type (rare, but handle similarly)
        if let CType::Struct(_) = target_ctype {
            // Struct casts are not standard C, but if the source is already a struct pointer, pass through
            let src = self.lower_expr(inner);
            return src;
        }

        let src = self.lower_expr(inner);
        let mut from_ty = self.get_expr_type(inner);
        let to_ty = self.type_spec_to_ir(target_type);

        if let Expr::Identifier(name, _) = inner {
            if let Some(vi) = self.lookup_var_info(name) {
                if vi.is_array || vi.is_struct {
                    from_ty = IrType::Ptr;
                }
            }
        }

        // C standard: conversion to _Bool yields 0 or 1
        if target_ctype == CType::Bool {
            if from_ty.is_float() {
                let zero = match from_ty {
                    IrType::F32 => Operand::Const(IrConst::F32(0.0)),
                    IrType::F64 => Operand::Const(IrConst::F64(0.0)),
                    _ => Operand::Const(IrConst::F64(0.0)),
                };
                let dest = self.emit_cmp_val(IrCmpOp::Ne, src, zero, from_ty);
                return Operand::Value(dest);
            }
            return self.emit_bool_normalize(src);
        }

        if to_ty == from_ty
            || to_ty == IrType::Ptr
            || (from_ty == IrType::Ptr && to_ty.size() == 8)
        {
            return src;
        }

        let dest = self.emit_cast_val(src, from_ty, to_ty);
        Operand::Value(dest)
    }

    // -----------------------------------------------------------------------
    // Compound literals
    // -----------------------------------------------------------------------

    fn lower_compound_literal(&mut self, type_spec: &TypeSpecifier, init: &Initializer) -> Operand {
        let ty = self.type_spec_to_ir(type_spec);
        let ctype = self.type_spec_to_ctype(type_spec);
        let size = match (&ctype, init) {
            (CType::Array(ref elem_ct, None), Initializer::List(items)) => {
                let elem_size = elem_ct.size_ctx(&self.types.struct_layouts).max(1);
                // For char/unsigned char arrays with a single string literal initializer,
                // the array size is the string length + 1 (null terminator)
                if elem_size == 1 && items.len() == 1 {
                    if let Initializer::Expr(ref expr) = items[0].init {
                        if let Expr::StringLiteral(ref s, _) | Expr::WideStringLiteral(ref s, _) = expr {
                            if matches!(expr, Expr::StringLiteral(_, _)) {
                                s.chars().count() + 1
                            } else {
                                (s.chars().count() + 1) * 4
                            }
                        } else {
                            elem_size * items.len()
                        }
                    } else {
                        elem_size * items.len()
                    }
                } else {
                    elem_size * items.len()
                }
            }
            _ => self.sizeof_type(type_spec),
        };
        let alloca = self.alloc_and_init_compound_literal(type_spec, init, ty, size);

        let struct_layout = self.get_struct_layout_for_type(type_spec);
        let is_scalar = struct_layout.is_none() && !matches!(ctype, CType::Array(_, _));
        if is_scalar {
            let loaded = self.fresh_value();
            self.emit(Instruction::Load { dest: loaded, ptr: alloca, ty });
            Operand::Value(loaded)
        } else {
            Operand::Value(alloca)
        }
    }

    fn init_array_compound_literal(
        &mut self, alloca: Value, items: &[InitializerItem], type_spec: &TypeSpecifier,
        ty: IrType, size: usize,
    ) {
        let elem_size = self.compound_literal_elem_size(type_spec);

        // For char/unsigned char array compound literals with a single string literal,
        // copy the string bytes directly instead of storing a pointer.
        if elem_size == 1 && items.len() == 1 && items[0].designators.is_empty() {
            if let Initializer::Expr(ref expr) = items[0].init {
                if let Expr::StringLiteral(ref s, _) = expr {
                    self.emit_string_to_alloca(alloca, s, 0);
                    return;
                }
            }
        }

        // Get the element CType for dispatching struct/union/array element init
        let elem_ctype = {
            let ctype = self.type_spec_to_ctype(type_spec);
            match ctype {
                CType::Array(elem_ct, _) => Some((*elem_ct).clone()),
                _ => None,
            }
        };

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

            let elem_offset = current_idx * elem_size;

            match &item.init {
                Initializer::Expr(expr) => {
                    let val = self.lower_expr(expr);
                    if current_idx == 0 && items.len() == 1 && elem_size == size {
                        self.emit(Instruction::Store { val, ptr: alloca, ty });
                    } else {
                        let offset_val = Operand::Const(IrConst::I64(elem_offset as i64));
                        let elem_ptr = self.fresh_value();
                        self.emit(Instruction::GetElementPtr {
                            dest: elem_ptr, base: alloca, offset: offset_val, ty,
                        });
                        let store_ty = Self::ir_type_for_size(elem_size);
                        self.emit(Instruction::Store { val, ptr: elem_ptr, ty: store_ty });
                    }
                }
                Initializer::List(sub_items) => {
                    // Handle list initializers for array elements (e.g., struct or nested array)
                    match &elem_ctype {
                        Some(CType::Struct(key)) | Some(CType::Union(key)) => {
                            if let Some(sub_layout) = self.types.struct_layouts.get(key).cloned() {
                                // Zero-init the element region first for partial initialization
                                self.zero_init_region(alloca, elem_offset, elem_size);
                                self.emit_struct_init(sub_items, alloca, &sub_layout, elem_offset);
                            }
                        }
                        Some(CType::Array(inner_elem_ty, Some(inner_size))) => {
                            // Nested array: initialize element-by-element
                            let inner_elem_size = self.resolve_ctype_size(inner_elem_ty);
                            let inner_ir_ty = IrType::from_ctype(inner_elem_ty);
                            let inner_is_bool = **inner_elem_ty == CType::Bool;
                            for (si, sub_item) in sub_items.iter().enumerate() {
                                if si >= *inner_size { break; }
                                if let Initializer::Expr(e) = &sub_item.init {
                                    let inner_offset = elem_offset + si * inner_elem_size;
                                    self.emit_init_expr_to_offset_bool(
                                        e, alloca, inner_offset, inner_ir_ty, inner_is_bool,
                                    );
                                }
                            }
                        }
                        _ => {
                            // Scalar array element with list init: use the first expression
                            if let Some(first) = sub_items.first() {
                                if let Initializer::Expr(expr) = &first.init {
                                    let val = self.lower_expr(expr);
                                    let offset_val = Operand::Const(IrConst::I64(elem_offset as i64));
                                    let elem_ptr = self.fresh_value();
                                    self.emit(Instruction::GetElementPtr {
                                        dest: elem_ptr, base: alloca, offset: offset_val, ty,
                                    });
                                    let store_ty = Self::ir_type_for_size(elem_size);
                                    self.emit(Instruction::Store { val, ptr: elem_ptr, ty: store_ty });
                                }
                            }
                        }
                    }
                }
            }
            current_idx += 1;
        }
    }

    // -----------------------------------------------------------------------
    // Sizeof, Generic selection
    // -----------------------------------------------------------------------

    fn lower_sizeof(&mut self, arg: &SizeofArg) -> Operand {
        if let Some(vla_val) = self.get_vla_sizeof(arg) {
            return Operand::Value(vla_val);
        }
        // Check for sizeof(type) where type is or contains VLA dimensions
        if let SizeofArg::Type(ts) = arg {
            if let Some(vla_val) = self.compute_vla_sizeof_for_type(ts) {
                return Operand::Value(vla_val);
            }
        }
        let size = match arg {
            SizeofArg::Type(ts) => self.sizeof_type(ts),
            SizeofArg::Expr(expr) => self.sizeof_expr(expr),
        };
        Operand::Const(IrConst::I64(size as i64))
    }

    fn get_vla_sizeof(&self, arg: &SizeofArg) -> Option<Value> {
        if let SizeofArg::Expr(Expr::Identifier(name, _)) = arg {
            // Check local VLA variables first
            if let Some(info) = self.func().locals.get(name) {
                if info.vla_size.is_some() {
                    return info.vla_size;
                }
            }
            // Then check VLA typedef names (sizeof applied to a typedef identifier)
            if let Some(&vla_size) = self.func().vla_typedef_sizes.get(name) {
                return Some(vla_size);
            }
        }
        None
    }

    /// Compute the runtime sizeof for a type that may contain VLA dimensions.
    /// Handles both typedef names that are VLA types and direct Array types
    /// with non-constant size expressions.
    fn compute_vla_sizeof_for_type(&mut self, ts: &TypeSpecifier) -> Option<Value> {
        // Check if it's a VLA typedef name with a pre-computed runtime size
        if let TypeSpecifier::TypedefName(name) = ts {
            if let Some(&vla_size) = self.func().vla_typedef_sizes.get(name) {
                return Some(vla_size);
            }
        }
        // Check if it's an Array type with non-constant dimensions
        let resolved = self.resolve_type_spec(ts).clone();
        if let TypeSpecifier::Array(ref elem, Some(ref size_expr)) = resolved {
            if self.expr_as_array_size(size_expr).is_none() {
                // Runtime dimension - compute size dynamically
                let elem_clone = elem.clone();
                let size_expr_clone = size_expr.clone();
                let elem_size_opt = self.compute_vla_sizeof_for_type(&elem_clone);
                let dim_val = self.lower_expr(&size_expr_clone);
                let dim_value = self.operand_to_value(dim_val);
                if let Some(elem_sz) = elem_size_opt {
                    // Both element and dimension are runtime
                    let mul = self.emit_binop_val(IrBinOp::Mul, Operand::Value(dim_value), Operand::Value(elem_sz), IrType::I64);
                    return Some(mul);
                } else {
                    // Element is constant, dimension is runtime
                    let elem_size = self.sizeof_type(&elem_clone) as i64;
                    if elem_size > 1 {
                        let mul = self.emit_binop_val(IrBinOp::Mul, Operand::Value(dim_value), Operand::Const(IrConst::I64(elem_size)), IrType::I64);
                        return Some(mul);
                    } else {
                        return Some(dim_value);
                    }
                }
            }
            // Constant outer dim but maybe VLA inner dims
            let elem_size_opt = self.compute_vla_sizeof_for_type(elem);
            if let Some(elem_sz) = elem_size_opt {
                let const_dim = self.expr_as_array_size(size_expr).unwrap_or(1);
                if const_dim > 1 {
                    let mul = self.emit_binop_val(IrBinOp::Mul, Operand::Value(elem_sz), Operand::Const(IrConst::I64(const_dim)), IrType::I64);
                    return Some(mul);
                } else {
                    return Some(elem_sz);
                }
            }
        }
        None
    }

    fn lower_generic_selection(&mut self, controlling: &Expr, associations: &[GenericAssociation]) -> Operand {
        let controlling_ctype = self.get_expr_ctype(controlling);
        let controlling_ir_type = self.get_expr_type(controlling);

        let mut default_expr: Option<&Expr> = None;
        let mut matched_expr: Option<&Expr> = None;

        for assoc in associations {
            match &assoc.type_spec {
                None => { default_expr = Some(&assoc.expr); }
                Some(type_spec) => {
                    let assoc_ctype = self.type_spec_to_ctype(type_spec);
                    if let Some(ref ctrl_ct) = controlling_ctype {
                        if self.ctype_matches_generic(ctrl_ct, &assoc_ctype) {
                            matched_expr = Some(&assoc.expr);
                            break;
                        }
                    } else {
                        let assoc_ir_type = self.type_spec_to_ir(type_spec);
                        if assoc_ir_type == controlling_ir_type {
                            matched_expr = Some(&assoc.expr);
                            break;
                        }
                    }
                }
            }
        }

        let selected = matched_expr.or(default_expr).unwrap_or(&associations[0].expr);
        self.lower_expr(selected)
    }

    pub(super) fn ctype_matches_generic(&self, controlling: &CType, assoc: &CType) -> bool {
        if controlling == assoc { return true; }
        match (controlling, assoc) {
            (CType::Char, CType::Char) => true,
            (CType::Long, CType::Long) => true,
            (CType::LongLong, CType::LongLong) => true,
            (CType::Pointer(a), CType::Pointer(b)) => a == b,
            _ => false,
        }
    }

    // -----------------------------------------------------------------------
    // Address-of, dereference, subscript, member access
    // -----------------------------------------------------------------------

    fn alloc_and_init_compound_literal(
        &mut self, type_spec: &TypeSpecifier, init: &Initializer, ty: IrType, size: usize,
    ) -> Value {
        let alloca = self.fresh_value();
        self.emit(Instruction::Alloca { dest: alloca, size, ty, align: 0 });
        let struct_layout = self.get_struct_layout_for_type(type_spec);
        match init {
            Initializer::Expr(expr) => {
                // For char array compound literals with string literal initializer,
                // copy string bytes instead of storing pointer
                let is_char_array = matches!(ty, IrType::I8 | IrType::U8) && {
                    let ctype = self.type_spec_to_ctype(type_spec);
                    matches!(ctype, CType::Array(_, _))
                };
                if is_char_array {
                    if let Expr::StringLiteral(ref s, _) = expr {
                        self.emit_string_to_alloca(alloca, s, 0);
                    } else {
                        let val = self.lower_expr(expr);
                        self.emit(Instruction::Store { val, ptr: alloca, ty });
                    }
                } else {
                    let val = self.lower_expr(expr);
                    self.emit(Instruction::Store { val, ptr: alloca, ty });
                }
            }
            Initializer::List(items) => {
                // Check if the type is an array — even arrays of structs should
                // use the array init path, not emit_struct_init.
                let ctype = self.type_spec_to_ctype(type_spec);
                let is_array = matches!(ctype, CType::Array(_, _));
                if !is_array {
                    if let Some(ref layout) = struct_layout {
                        self.zero_init_alloca(alloca, layout.size);
                        self.emit_struct_init(items, alloca, layout, 0);
                    } else {
                        self.init_array_compound_literal(alloca, items, type_spec, ty, size);
                    }
                } else {
                    self.init_array_compound_literal(alloca, items, type_spec, ty, size);
                }
            }
        }
        alloca
    }

    pub(super) fn lower_address_of(&mut self, inner: &Expr) -> Operand {
        if let Expr::CompoundLiteral(type_spec, init, _) = inner {
            let ty = self.type_spec_to_ir(type_spec);
            let size = self.sizeof_type(type_spec);
            let alloca = self.alloc_and_init_compound_literal(type_spec, init, ty, size);
            return Operand::Value(alloca);
        }

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

        if let Expr::Identifier(name, _) = inner {
            let dest = self.fresh_value();
            self.emit(Instruction::GlobalAddr { dest, name: name.clone() });
            return Operand::Value(dest);
        }

        self.lower_expr(inner)
    }

    /// Check if dereferencing the given expression is a no-op because the
    /// expression is a function pointer (or function designator). In C:
    /// - *f where f is a function pointer → function designator → decays back
    /// - *add where add is a function name → function designator → decays back
    /// - *(s->fnptr) where fnptr is a function pointer member → same no-op
    /// - **f, ***f etc. are also no-ops (recursive application)
    pub(super) fn is_function_pointer_deref(&self, inner: &Expr) -> bool {
        // First, check if the inner expression is actually a pointer-to-function-pointer.
        // Due to our CType representation, int (**fpp)(int,int) is stored as
        // Pointer(Function(FunctionType { return_type: Pointer(Int), ... })) — the same
        // shape as a direct function pointer. We use the is_ptr_to_func_ptr flag
        // (set during declaration analysis based on pointer count in derived declarators)
        // to correctly distinguish these cases.
        if let Expr::Identifier(name, _) = inner {
            if let Some(vi) = self.lookup_var_info(name) {
                if vi.is_ptr_to_func_ptr {
                    // This is a pointer-to-function-pointer (e.g., int (**fpp)(int,int)).
                    // Dereferencing it requires a real load to get the inner function pointer.
                    return false;
                }
            }
        }

        // Check CType-based detection (works for typedef'd function pointers
        // where param_ctype correctly sets CType::Pointer(CType::Function(...)),
        // and also for struct member function pointers via get_expr_ctype)
        if let Some(ref inner_ct) = self.get_expr_ctype(inner) {
            match inner_ct {
                CType::Pointer(pointee) if matches!(pointee.as_ref(), CType::Function(_)) => {
                    return true;
                }
                CType::Function(_) => {
                    return true;
                }
                _ => {}
            }
        }
        // Check for known function names (e.g., *add where add is a function)
        match inner {
            Expr::Identifier(name, _) => {
                // Only treat as a function name dereference if there is no local variable
                // with the same name. Local variables shadow function names, so if a local
                // variable called "link" exists, *link should dereference the variable,
                // not be treated as a no-op function pointer dereference.
                if self.known_functions.contains(name.as_str()) && self.lookup_var_info(name).is_none() {
                    return true;
                }
                // Check if this variable is a function pointer (deref is no-op).
                // Pointer-to-function-pointer cases are already handled by the
                // is_ptr_to_func_ptr early-return above, so any Pointer(Function(...))
                // at this point is a genuine direct function pointer.
                if let Some(vi) = self.lookup_var_info(name) {
                    let has_ctype = vi.c_type.is_some();
                    if let Some(ref ct) = vi.c_type {
                        match ct {
                            CType::Pointer(pointee) if matches!(pointee.as_ref(), CType::Function(_)) => {
                                return true;
                            }
                            CType::Function(_) => {
                                return true;
                            }
                            _ => {}
                        }
                    }
                    // Fallback: check ptr_sigs only when c_type is unavailable.
                    // When c_type IS available, the match above is authoritative —
                    // ptr_sigs may contain entries for pointer-to-function-pointers
                    // which are NOT no-op derefs.
                    if !has_ctype && self.func_meta.ptr_sigs.contains_key(name.as_str()) {
                        return true;
                    }
                }
                false
            }
            // For nested derefs: *(*f) where *f is also a no-op → check recursively
            Expr::Deref(deeper_inner, _) => self.is_function_pointer_deref(deeper_inner),
            // For function calls that return a function pointer: *(p(args))
            // If the callee is a function pointer variable whose return type is
            // itself a function pointer, then dereferencing the result is a no-op.
            Expr::FunctionCall(func, _, _) => {
                // Strip deref layers from the callee (since *p, **p are equivalent for fptrs)
                let mut callee: &Expr = func.as_ref();
                while let Expr::Deref(inner, _) = callee {
                    callee = inner;
                }
                if let Expr::Identifier(name, _) = callee {
                    // Check if the callee returns a function pointer by examining its CType
                    if let Some(vi) = self.lookup_var_info(name) {
                        if let Some(ref ct) = vi.c_type {
                            // Extract the return type of the function pointer
                            if let Some(ret_ct) = Self::extract_func_ptr_return_ctype_static(ct) {
                                // If the return type is a function pointer, deref is no-op
                                if matches!(&ret_ct, CType::Pointer(p) if matches!(p.as_ref(), CType::Function(_))) {
                                    return true;
                                }
                                if matches!(&ret_ct, CType::Function(_)) {
                                    return true;
                                }
                            }
                        }
                    }
                    // Also check known function signatures
                    if let Some(sig) = self.func_meta.sigs.get(name.as_str()) {
                        if let Some(ref ret_ct) = sig.return_ctype {
                            if matches!(ret_ct, CType::Pointer(p) if matches!(p.as_ref(), CType::Function(_))) {
                                return true;
                            }
                        }
                    }
                }
                false
            }
            _ => false,
        }
    }

    /// Static version of extract_func_ptr_return_ctype for use in is_function_pointer_deref
    fn extract_func_ptr_return_ctype_static(ctype: &CType) -> Option<CType> {
        match ctype {
            CType::Pointer(inner) => match inner.as_ref() {
                CType::Function(ft) => Some(ft.return_type.clone()),
                CType::Pointer(inner2) => match inner2.as_ref() {
                    CType::Function(ft) => Some(ft.return_type.clone()),
                    _ => None,
                },
                _ => None,
            },
            CType::Function(ft) => Some(ft.return_type.clone()),
            _ => None,
        }
    }

    fn lower_deref(&mut self, inner: &Expr) -> Operand {
        // Check if pointee is an aggregate type that doesn't need a Load.
        // Note: Function types are NOT included here — function pointer dereferences
        // are handled by is_function_pointer_deref which correctly distinguishes
        // direct function pointers (no-op deref) from pointer-to-function-pointers
        // (which need a real load despite having similar CType shapes).
        let pointee_is_no_load = |ct: &CType| -> bool {
            if let CType::Pointer(ref pointee) = ct {
                matches!(pointee.as_ref(),
                    CType::Array(_, _) | CType::Struct(_) | CType::Union(_))
                    || pointee.is_complex()
            } else {
                false
            }
        };
        // In C, dereferencing a function pointer yields a function designator which
        // immediately decays back to a function pointer. So *f, **f, ***f etc. are
        // all equivalent when f is a function pointer. No Load should be emitted.
        // This also applies to direct function names: (*add)(x, y) is valid C
        // because the function name decays to a pointer, and dereferencing that
        // gives back the function designator.
        if self.is_function_pointer_deref(inner) {
            return self.lower_expr(inner);
        }
        // Check if dereferencing yields an aggregate type.
        // In these cases, the result is an address (no Load needed).
        if self.get_expr_ctype(inner).map_or(false, |ct| pointee_is_no_load(&ct)) {
            return self.lower_expr(inner);
        }
        {
            let inner_ct = self.expr_ctype(inner);
            if pointee_is_no_load(&inner_ct) {
                return self.lower_expr(inner);
            }
            // Handle *array where array is a multi-dimensional array:
            // e.g., *x where x is int[2][3] has inner CType = Array(Array(Int,3), 2).
            // Dereferencing peels off the outer dimension, yielding Array(Int,3) which
            // is an aggregate — so no Load is needed, just return the base address.
            if let CType::Array(ref elem, _) = inner_ct {
                if elem.is_complex()
                    || matches!(elem.as_ref(), CType::Struct(_) | CType::Union(_) | CType::Array(_, _))
                {
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
        if self.subscript_result_is_array(expr) {
            let addr = self.compute_array_element_addr(base, index);
            return Operand::Value(addr);
        }
        if self.struct_value_size(expr).is_some() {
            let addr = self.compute_array_element_addr(base, index);
            return Operand::Value(addr);
        }
        {
            let elem_ct = self.expr_ctype(expr);
            if elem_ct.is_complex() {
                let addr = self.compute_array_element_addr(base, index);
                return Operand::Value(addr);
            }
        }
        let elem_ty = self.get_expr_type(expr);
        let addr = self.compute_array_element_addr(base, index);
        let dest = self.fresh_value();
        self.emit(Instruction::Load { dest, ptr: addr, ty: elem_ty });
        Operand::Value(dest)
    }

    // -----------------------------------------------------------------------
    // Member access (s.field, p->field)
    // -----------------------------------------------------------------------

    fn lower_member_access_impl(&mut self, base_expr: &Expr, field_name: &str, is_pointer: bool) -> Operand {
        let (field_offset, field_ty, bitfield, field_ctype) = if is_pointer {
            self.resolve_pointer_member_access_with_ctype(base_expr, field_name)
        } else {
            self.resolve_member_access_with_ctype(base_expr, field_name)
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

        let is_addr_type = match &field_ctype {
            Some(ct) => matches!(ct,
                CType::Array(_, _) | CType::Struct(_) | CType::Union(_) |
                CType::ComplexFloat | CType::ComplexDouble | CType::ComplexLongDouble),
            None => {
                self.field_is_array(base_expr, field_name, is_pointer)
                    || self.field_is_struct(base_expr, field_name, is_pointer)
                    || self.field_is_complex(base_expr, field_name, is_pointer)
            }
        };
        if is_addr_type {
            return Operand::Value(field_addr);
        }

        // For bitfields, use extract_bitfield_from_addr which handles split loads
        // (packed bitfields that span storage unit boundaries).
        if let Some((bit_offset, bit_width)) = bitfield {
            return self.extract_bitfield_from_addr(field_addr, field_ty, bit_offset, bit_width);
        }

        let dest = self.fresh_value();
        self.emit(Instruction::Load { dest, ptr: field_addr, ty: field_ty });
        Operand::Value(dest)
    }

    fn lower_member_access(&mut self, base_expr: &Expr, field_name: &str) -> Operand {
        self.lower_member_access_impl(base_expr, field_name, false)
    }

    fn lower_pointer_member_access(&mut self, base_expr: &Expr, field_name: &str) -> Operand {
        self.lower_member_access_impl(base_expr, field_name, true)
    }

    fn field_is_array(&self, base_expr: &Expr, field_name: &str, is_pointer_access: bool) -> bool {
        self.resolve_field_ctype(base_expr, field_name, is_pointer_access)
            .map(|ct| matches!(ct, CType::Array(_, _))).unwrap_or(false)
    }

    fn field_is_struct(&self, base_expr: &Expr, field_name: &str, is_pointer_access: bool) -> bool {
        self.resolve_field_ctype(base_expr, field_name, is_pointer_access)
            .map(|ct| matches!(ct, CType::Struct(_) | CType::Union(_))).unwrap_or(false)
    }

    fn field_is_complex(&self, base_expr: &Expr, field_name: &str, is_pointer_access: bool) -> bool {
        self.resolve_field_ctype(base_expr, field_name, is_pointer_access)
            .map(|ct| matches!(ct, CType::ComplexFloat | CType::ComplexDouble | CType::ComplexLongDouble)).unwrap_or(false)
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

    fn lower_short_circuit(&mut self, lhs: &Expr, rhs: &Expr, is_and: bool) -> Operand {
        let result_alloca = self.fresh_value();
        self.emit(Instruction::Alloca { dest: result_alloca, ty: IrType::I64, size: 8, align: 0 });

        let rhs_label = self.fresh_label();
        let end_label = self.fresh_label();

        let lhs_val = self.lower_condition_expr(lhs);

        let default_val = if is_and { 0 } else { 1 };
        self.emit(Instruction::Store {
            val: Operand::Const(IrConst::I64(default_val)),
            ptr: result_alloca, ty: IrType::I64,
        });

        let (true_label, false_label) = if is_and {
            (rhs_label, end_label)
        } else {
            (end_label, rhs_label)
        };
        self.terminate(Terminator::CondBranch { cond: lhs_val, true_label, false_label });

        self.start_block(rhs_label);
        let rhs_val = self.lower_condition_expr(rhs);
        let rhs_bool = self.emit_cmp_val(IrCmpOp::Ne, rhs_val, Operand::Const(IrConst::I64(0)), IrType::I64);
        self.emit(Instruction::Store { val: Operand::Value(rhs_bool), ptr: result_alloca, ty: IrType::I64 });
        self.terminate(Terminator::Branch(end_label));

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

    fn lower_inc_dec_impl(&mut self, inner: &Expr, is_inc: bool, return_new: bool) -> Operand {
        if let Some(result) = self.try_lower_bitfield_inc_dec(inner, is_inc, return_new) {
            return result;
        }

        let ty = self.get_expr_type(inner);
        if let Some(lv) = self.lower_lvalue(inner) {
            let loaded = self.load_lvalue_typed(&lv, ty);
            let loaded_val = self.operand_to_value(loaded.clone());
            let (step, binop_ty) = self.inc_dec_step_and_type(ty, inner);
            let ir_op = if is_inc { IrBinOp::Add } else { IrBinOp::Sub };
            let result = self.emit_binop_val(ir_op, Operand::Value(loaded_val), step, binop_ty);
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

    fn try_lower_bitfield_inc_dec(&mut self, inner: &Expr, is_inc: bool, return_new: bool) -> Option<Operand> {
        let (field_addr, storage_ty, bit_offset, bit_width) = self.resolve_bitfield_lvalue(inner)?;

        let current_val = self.extract_bitfield_from_addr(field_addr, storage_ty, bit_offset, bit_width);

        let ir_op = if is_inc { IrBinOp::Add } else { IrBinOp::Sub };
        let result = self.emit_binop_val(ir_op, current_val.clone(), Operand::Const(IrConst::I64(1)), IrType::I64);

        self.store_bitfield(field_addr, storage_ty, bit_offset, bit_width, Operand::Value(result));

        let ret_val = if return_new { Operand::Value(result) } else { current_val };
        Some(self.truncate_to_bitfield_value(ret_val, bit_width, storage_ty.is_signed()))
    }

    fn inc_dec_step_and_type(&self, ty: IrType, expr: &Expr) -> (Operand, IrType) {
        if ty == IrType::Ptr {
            let elem_size = self.get_pointer_elem_size_from_expr(expr);
            (Operand::Const(IrConst::I64(elem_size as i64)), IrType::I64)
        } else if ty == IrType::F64 {
            (Operand::Const(IrConst::F64(1.0)), IrType::F64)
        } else if ty == IrType::F32 {
            (Operand::Const(IrConst::F32(1.0)), IrType::F32)
        } else if ty == IrType::F128 {
            (Operand::Const(IrConst::LongDouble(1.0)), IrType::F128)
        } else {
            (Operand::Const(IrConst::I64(1)), IrType::I64)
        }
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

    fn maybe_narrow(&mut self, val: Value, ty: IrType) -> Operand {
        if ty == IrType::U32 || ty == IrType::I32 {
            let narrowed = self.emit_cast_val(Operand::Value(val), IrType::I64, ty);
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

    pub(super) fn infer_expr_type(&self, expr: &Expr) -> IrType {
        match expr {
            Expr::Identifier(name, _) => {
                if name == "__func__" || name == "__FUNCTION__" || name == "__PRETTY_FUNCTION__" {
                    return IrType::Ptr;
                }
                if self.types.enum_constants.contains_key(name) { return IrType::I32; }
                if let Some(vi) = self.lookup_var_info(name) { return vi.ty; }
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
            Expr::FloatLiteralLongDouble(_, _) => IrType::F128,
            Expr::Cast(ref target_type, _, _) => self.type_spec_to_ir(target_type),
            Expr::BinaryOp(op, lhs, rhs, _) => {
                if op.is_comparison() || matches!(op, BinOp::LogicalAnd | BinOp::LogicalOr) {
                    IrType::I32
                } else if matches!(op, BinOp::Shl | BinOp::Shr) {
                    Self::integer_promote(self.infer_expr_type(lhs))
                } else {
                    // Iterate left-skewed chains to avoid O(2^n) recursion
                    let rhs_ty = self.infer_expr_type(rhs);
                    let mut result = rhs_ty;
                    let mut cur = lhs.as_ref();
                    loop {
                        match cur {
                            Expr::BinaryOp(op2, inner_lhs, inner_rhs, _)
                                if !op2.is_comparison()
                                    && !matches!(op2, BinOp::LogicalAnd | BinOp::LogicalOr | BinOp::Shl | BinOp::Shr) =>
                            {
                                let r_ty = self.infer_expr_type(inner_rhs);
                                result = Self::common_type(result, r_ty);
                                cur = inner_lhs.as_ref();
                            }
                            _ => {
                                let l_ty = self.infer_expr_type(cur);
                                result = Self::common_type(result, l_ty);
                                break;
                            }
                        }
                    }
                    result
                }
            }
            Expr::UnaryOp(op, inner, _) => {
                match op {
                    UnaryOp::Neg | UnaryOp::BitNot | UnaryOp::Plus => {
                        let inner_ty = self.infer_expr_type(inner);
                        if inner_ty.is_float() { inner_ty } else { Self::integer_promote(inner_ty) }
                    }
                    UnaryOp::LogicalNot => IrType::I32,
                    _ => self.infer_expr_type(inner),
                }
            }
            Expr::Sizeof(_, _) => IrType::U64,
            Expr::FunctionCall(func, _, _) => {
                self.get_call_return_type(func)
            }
            Expr::Deref(_, _) | Expr::ArraySubscript(_, _, _)
            | Expr::MemberAccess(_, _, _) | Expr::PointerMemberAccess(_, _, _) => {
                self.get_expr_type(expr)
            }
            Expr::PostfixOp(_, inner, _) => self.infer_expr_type(inner),
            Expr::Assign(lhs, _, _) | Expr::CompoundAssign(_, lhs, _, _) => {
                self.infer_expr_type(lhs)
            }
            Expr::Conditional(_, then_expr, else_expr, _) => {
                Self::common_type(self.infer_expr_type(then_expr), self.infer_expr_type(else_expr))
            }
            Expr::GnuConditional(cond, else_expr, _) => {
                Self::common_type(self.infer_expr_type(cond), self.infer_expr_type(else_expr))
            }
            Expr::Comma(_, rhs, _) => self.infer_expr_type(rhs),
            Expr::AddressOf(_, _) => IrType::Ptr,
            Expr::StringLiteral(_, _) | Expr::WideStringLiteral(_, _) => IrType::Ptr,
            Expr::VaArg(_, type_spec, _) => self.resolve_va_arg_type(type_spec),
            _ => IrType::I64,
        }
    }

    /// Determine common type for usual arithmetic conversions.
    pub(super) fn common_type(a: IrType, b: IrType) -> IrType {
        if a == IrType::I128 || a == IrType::U128 || b == IrType::I128 || b == IrType::U128 {
            if a == IrType::U128 || b == IrType::U128 { return IrType::U128; }
            return IrType::I128;
        }
        if a == IrType::F128 || b == IrType::F128 { return IrType::F128; }
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
        IrType::I32
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

        let needs_cast = (target_ty.is_float() && !src_ty.is_float())
            || (!target_ty.is_float() && src_ty.is_float())
            || (target_ty.is_float() && src_ty.is_float() && target_ty != src_ty)
            || (src_ty.is_integer() && target_ty.is_integer() && src_ty != target_ty);

        if needs_cast {
            let dest = self.emit_cast_val(src, src_ty, target_ty);
            return Operand::Value(dest);
        }
        src
    }

    /// Normalize a value for _Bool storage: emit (val != 0) to clamp to 0 or 1.
    /// Uses I64 comparison type (legacy — use emit_bool_normalize_typed for correctness).
    pub(super) fn emit_bool_normalize(&mut self, val: Operand) -> Operand {
        let dest = self.emit_cmp_val(IrCmpOp::Ne, val, Operand::Const(IrConst::I64(0)), IrType::I64);
        Operand::Value(dest)
    }

    /// Normalize a value for _Bool storage at the given source type.
    /// Emits (val != 0) for integers, (val != 0.0) for floats.
    /// This must be called BEFORE any truncation to avoid losing high bits.
    pub(super) fn emit_bool_normalize_typed(&mut self, val: Operand, src_ty: IrType) -> Operand {
        let zero = match src_ty {
            IrType::F32 => Operand::Const(IrConst::F32(0.0)),
            IrType::F64 => Operand::Const(IrConst::F64(0.0)),
            _ => Operand::Const(IrConst::I64(0)),
        };
        let cmp_ty = match src_ty {
            IrType::F32 | IrType::F64 => src_ty,
            // For integer types, compare at the source width to preserve all bits
            _ => src_ty,
        };
        let dest = self.emit_cmp_val(IrCmpOp::Ne, val, zero, cmp_ty);
        Operand::Value(dest)
    }

    // -----------------------------------------------------------------------
    // va_arg
    // -----------------------------------------------------------------------

    fn lower_va_arg(&mut self, ap_expr: &Expr, type_spec: &TypeSpecifier) -> Operand {
        // VaArg instruction expects a pointer to the va_list "object" so it can
        // both read from and advance the va_list state.
        //
        // The correct pointer depends on the target's va_list representation:
        //
        // x86-64 / AArch64: va_list is an array type (char[24] / char[32]).
        //   - LOCAL va_list:  alloca IS the array, lower_expr returns the alloca
        //     address (array-to-pointer decay) → pointer to the struct. ✓
        //   - PARAM va_list:  array decayed to char* when passed; the alloca holds
        //     a pointer to the caller's va_list struct, lower_expr loads that
        //     pointer value → pointer to the struct. ✓
        //   So lower_expr gives the correct va_list pointer on these targets.
        //
        // RISC-V: va_list is a scalar pointer type (void *).
        //   - LOCAL va_list:  alloca holds the void* value. va_arg needs the
        //     ADDRESS of the alloca (to read the current pointer and write back
        //     the advanced one). lower_address_of returns this. ✓
        //   - PARAM va_list:  alloca holds a copy of the void* parameter.
        //     Same as local: need the ADDRESS of the alloca. lower_address_of
        //     returns this. ✓
        //   So lower_address_of gives the correct va_list pointer on RISC-V.
        use crate::backend::Target;
        let ap_val = if self.target == Target::Riscv64 {
            // RISC-V: va_list is void*, need address of the variable holding it
            self.lower_address_of(ap_expr)
        } else {
            // x86-64 / AArch64: va_list is an array type, lower_expr handles
            // both local (array decay) and parameter (load pointer) cases
            self.lower_expr(ap_expr)
        };
        let va_list_ptr = self.operand_to_value(ap_val);
        let result_ty = self.resolve_va_arg_type(type_spec);
        let dest = self.fresh_value();
        self.emit(Instruction::VaArg { dest, va_list_ptr, result_ty: result_ty.clone() });
        Operand::Value(dest)
    }

    /// Get a pointer to the va_list struct from an expression.
    /// Used by va_start, va_end, va_copy builtins.
    ///
    /// Target-dependent behavior (same logic as lower_va_arg):
    /// - x86-64/AArch64: va_list is array type, lower_expr handles both local
    ///   (array decay gives address) and parameter (loads pointer) cases.
    /// - RISC-V: va_list is void*, always need address-of the variable.
    pub(super) fn lower_va_list_pointer(&mut self, ap_expr: &Expr) -> Operand {
        use crate::backend::Target;
        if self.target == Target::Riscv64 {
            self.lower_address_of(ap_expr)
        } else {
            self.lower_expr(ap_expr)
        }
    }

    pub(super) fn resolve_va_arg_type(&self, type_spec: &TypeSpecifier) -> IrType {
        // Use type_spec_to_ctype for typedef resolution, then canonical CType-to-IrType
        let ctype = self.type_spec_to_ctype(type_spec);
        IrType::from_ctype(&ctype)
    }
}
