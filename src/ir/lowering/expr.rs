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
    /// tested for truthiness. The backend's CondBranch uses integer testq, which
    /// doesn't handle -0.0 correctly (bit pattern 0x8000000000000000 is non-zero
    /// but -0.0 should be falsy). We fix this by masking off the sign bit so that
    /// both +0.0 and -0.0 become zero, while NaN and non-zero values remain truthy.
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
            // Long double literals: use IrConst::LongDouble so the type is tracked as F128
            // for correct ABI handling (16-byte passing in variadic calls, va_arg)
            Expr::FloatLiteralLongDouble(val, _) => Operand::Const(IrConst::LongDouble(*val)),
            Expr::CharLiteral(ch, _) => Operand::Const(IrConst::I32(*ch as i32)),

            // Imaginary literals: create complex value {0, imag_val}
            Expr::ImaginaryLiteral(val, _) => {
                self.lower_imaginary_literal(*val, &CType::ComplexDouble)
            }
            Expr::ImaginaryLiteralF32(val, _) => {
                self.lower_imaginary_literal(*val, &CType::ComplexFloat)
            }
            Expr::ImaginaryLiteralLongDouble(val, _) => {
                self.lower_imaginary_literal(*val, &CType::ComplexLongDouble)
            }

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

        // Local variables: arrays/structs/complex decay to address, scalars are loaded.
        // Check locals FIRST so that local variables shadow enum constants of the same name.
        // This is critical for C semantics: a local `int a = 5;` must shadow an enum
        // constant `a` from an enclosing or later scope.
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
            // Complex types return their alloca pointer (like structs)
            if let Some(ref ct) = info.c_type {
                if ct.is_complex() {
                    return Operand::Value(info.alloca);
                }
            }
            let dest = self.fresh_value();
            self.emit(Instruction::Load { dest, ptr: info.alloca, ty: info.ty });
            return Operand::Value(dest);
        }

        // Enum constants are compile-time integer values.
        // Checked after locals so that local variables shadow enum constants of the same name.
        if let Some(&val) = self.enum_constants.get(name) {
            return Operand::Const(IrConst::I64(val));
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
                if let Some(ref ct) = ginfo.c_type {
                    if ct.is_complex() {
                        return Operand::Value(addr);
                    }
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
            // Complex types return their address pointer (like structs)
            if let Some(ref ct) = ginfo.c_type {
                if ct.is_complex() {
                    return Operand::Value(addr);
                }
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

        // Complex arithmetic: if either operand is complex, handle specially.
        // This MUST be checked before pointer arithmetic since complex types use Ptr IR type.
        if matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div) {
            let lhs_ct = self.expr_ctype(lhs);
            let rhs_ct = self.expr_ctype(rhs);
            if lhs_ct.is_complex() || rhs_ct.is_complex() {
                return self.lower_complex_binary_op(op, lhs, rhs, &lhs_ct, &rhs_ct);
            }
        }

        // Pointer arithmetic: ptr +/- int, int + ptr, ptr - ptr
        if matches!(op, BinOp::Add | BinOp::Sub) {
            if let Some(result) = self.try_lower_pointer_arithmetic(op, lhs, rhs) {
                return result;
            }
        }

        // Complex equality/inequality: compare real and imaginary parts separately
        if matches!(op, BinOp::Eq | BinOp::Ne) {
            let lhs_ct = self.expr_ctype(lhs);
            let rhs_ct = self.expr_ctype(rhs);
            if lhs_ct.is_complex() || rhs_ct.is_complex() {
                return self.lower_complex_comparison(op, lhs, rhs, &lhs_ct, &rhs_ct);
            }
        }

        // Pointer comparison: compare as integer addresses, not as the pointed-to type.
        if matches!(op, BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge) {
            if self.expr_is_pointer(lhs) || self.expr_is_pointer(rhs) {
                let lhs_val = self.lower_expr(lhs);
                let rhs_val = self.lower_expr(rhs);
                let cmp_op = Self::binop_to_cmp(*op, true); // pointer comparison is unsigned
                let dest = self.emit_cmp_val(cmp_op, lhs_val, rhs_val, IrType::I64);
                return Operand::Value(dest);
            }
        }

        // Regular arithmetic/comparison
        self.lower_arithmetic_binop(op, lhs, rhs)
    }

    /// Lower a binary operation involving complex numbers.
    fn lower_complex_binary_op(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr, lhs_ct: &CType, rhs_ct: &CType) -> Operand {
        let result_ct = self.common_complex_type(lhs_ct, rhs_ct);

        // Lower and convert both operands to the result complex type
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
            let ir_op = if *op == BinOp::Add { IrBinOp::Add } else { IrBinOp::Sub };
            let dest = self.emit_binop_val(ir_op, lhs_val, scaled_rhs, IrType::I64);
            Some(Operand::Value(dest))
        } else if rhs_is_ptr && !lhs_is_ptr && *op == BinOp::Add {
            // int + ptr: scale LHS by element size
            let elem_size = self.get_pointer_elem_size_from_expr(rhs);
            let lhs_val = self.lower_expr(lhs);
            let rhs_val = self.lower_expr(rhs);
            let scaled_lhs = self.scale_index(lhs_val, elem_size);
            let dest = self.emit_binop_val(IrBinOp::Add, scaled_lhs, rhs_val, IrType::I64);
            Some(Operand::Value(dest))
        } else if lhs_is_ptr && rhs_is_ptr && *op == BinOp::Sub {
            // ptr - ptr: byte difference / element size
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
            None // Not pointer arithmetic
        }
    }

    /// Multiply an index value by a scale factor (for pointer arithmetic).
    /// Returns the operand unchanged if scale is 1.
    pub(super) fn scale_index(&mut self, index: Operand, scale: usize) -> Operand {
        if scale <= 1 {
            return index;
        }
        let scaled = self.emit_binop_val(IrBinOp::Mul, index, Operand::Const(IrConst::I64(scale as i64)), IrType::I64);
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
            // F128 (long double) is computed at F64 precision internally
            let ft = if lhs_expr_ty == IrType::F128 || rhs_expr_ty == IrType::F128 {
                IrType::F128
            } else if lhs_expr_ty == IrType::F64 || rhs_expr_ty == IrType::F64 {
                IrType::F64
            } else {
                IrType::F32
            };
            (ft, false, ft)
        } else if is_shift {
            // For shifts: result type is the promoted left operand type
            let promoted_lhs = Self::integer_promote(lhs_ty);
            let shift_op_ty = if promoted_lhs == IrType::I128 || promoted_lhs == IrType::U128 {
                promoted_lhs
            } else {
                IrType::I64
            };
            (shift_op_ty, promoted_lhs.is_unsigned(), promoted_lhs)
        } else {
            let ct = Self::common_type(lhs_ty, rhs_ty);
            // Use I128/U128 as op_ty when operands are 128-bit
            let ot = if ct == IrType::I128 || ct == IrType::U128 { ct } else { IrType::I64 };
            (ot, ct.is_unsigned(), ct)
        };

        // For shifts, promote operands independently (not to common type)
        let (lhs_val, rhs_val) = if is_shift {
            let shift_lhs_ty = if op_ty == IrType::I128 || op_ty == IrType::U128 { op_ty } else { IrType::I64 };
            let lhs_val = self.lower_expr_with_type(lhs, shift_lhs_ty);
            // Shift amount is always I64 regardless of operand type
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
                // Check for complex type - unary plus is identity for complex
                let inner_ct = self.expr_ctype(inner);
                if inner_ct.is_complex() {
                    return self.lower_expr(inner);
                }
                // Unary plus applies integer promotion (C99 6.5.3.3)
                let val = self.lower_expr(inner);
                let inner_ty = self.infer_expr_type(inner);
                let promoted_ty = Self::integer_promote(inner_ty);
                if promoted_ty != inner_ty && inner_ty.is_integer() {
                    // Widen sub-int types to int
                    let dest = self.emit_cast_val(val, inner_ty, promoted_ty);
                    Operand::Value(dest)
                } else {
                    val
                }
            }
            UnaryOp::Neg => {
                // Check for complex negation
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
                    // Apply integer promotion: narrow to promoted type (at least I32)
                    let promoted_ty = Self::integer_promote(inner_ty);
                    self.maybe_narrow(dest, promoted_ty)
                } else {
                    Operand::Value(dest)
                }
            }
            UnaryOp::BitNot => {
                // GCC extension: ~ on complex type produces complex conjugate
                let inner_ct = self.expr_ctype(inner);
                if inner_ct.is_complex() {
                    return self.lower_complex_conj(inner);
                }
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
    // Assignment
    // -----------------------------------------------------------------------

    fn lower_assign(&mut self, lhs: &Expr, rhs: &Expr) -> Operand {
        // Complex assignment: copy real/imag parts
        let lhs_ct = self.expr_ctype(lhs);
        if lhs_ct.is_complex() {
            return self.lower_complex_assign(lhs, rhs, &lhs_ct);
        }

        if self.struct_value_size(lhs).is_some() {
            return self.lower_struct_assign(lhs, rhs);
        }

        // Check for bitfield assignment
        if let Some(result) = self.try_lower_bitfield_assign(lhs, rhs) {
            return result;
        }

        // When assigning a complex RHS to a non-complex LHS, extract the real part first
        let rhs_ct = self.expr_ctype(rhs);
        let lhs_ty = self.get_expr_type(lhs);
        let rhs_val = if rhs_ct.is_complex() && !lhs_ct.is_complex() {
            let complex_val = self.lower_expr(rhs);
            let ptr = self.operand_to_value(complex_val);
            let real_part = self.load_complex_real(ptr, &rhs_ct);
            let from_ty = Self::complex_component_ir_type(&rhs_ct);
            self.emit_implicit_cast(real_part, from_ty, lhs_ty)
        } else {
            let rhs_val = self.lower_expr(rhs);
            let rhs_ty = self.get_expr_type(rhs);
            self.emit_implicit_cast(rhs_val, rhs_ty, lhs_ty)
        };

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

    /// Truncate a value to bit_width bits and sign-extend if the bitfield is signed.
    /// This returns the value as it would be read back from the bitfield.
    fn truncate_to_bitfield_value(&mut self, val: Operand, bit_width: u32, is_signed: bool) -> Operand {
        if bit_width >= 64 {
            return val;
        }
        if is_signed {
            // Sign-extend: shift left to put the sign bit at bit 63, then arithmetic shift right
            let shl_amount = 64 - bit_width;
            let shifted = self.emit_binop_val(IrBinOp::Shl, val, Operand::Const(IrConst::I64(shl_amount as i64)), IrType::I64);
            let result = self.emit_binop_val(IrBinOp::AShr, Operand::Value(shifted), Operand::Const(IrConst::I64(shl_amount as i64)), IrType::I64);
            Operand::Value(result)
        } else {
            let mask = (1u64 << bit_width) - 1;
            let masked = self.emit_binop_val(IrBinOp::And, val, Operand::Const(IrConst::I64(mask as i64)), IrType::I64);
            Operand::Value(masked)
        }
    }

    /// Try to lower assignment to a bitfield member. Returns Some if the LHS is a bitfield.
    fn try_lower_bitfield_assign(&mut self, lhs: &Expr, rhs: &Expr) -> Option<Operand> {
        let (field_addr, storage_ty, bit_offset, bit_width) = self.resolve_bitfield_lvalue(lhs)?;

        // Evaluate RHS
        let rhs_val = self.lower_expr(rhs);

        // Read-modify-write the storage unit
        self.store_bitfield(field_addr, storage_ty, bit_offset, bit_width, rhs_val.clone());

        // Return the truncated value (what was actually stored), sign-extended for signed bitfields
        Some(self.truncate_to_bitfield_value(rhs_val, bit_width, storage_ty.is_signed()))
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
        let result = self.emit_binop_val(ir_op, current_val, rhs_val, IrType::I64);

        // Store back via read-modify-write
        self.store_bitfield(field_addr, storage_ty, bit_offset, bit_width, Operand::Value(result));

        // Return the new value truncated to bit_width, sign-extended for signed bitfields
        Some(self.truncate_to_bitfield_value(Operand::Value(result), bit_width, storage_ty.is_signed()))
    }

    /// Store a value into a bitfield: load storage unit, clear field bits, OR in new value, store back.
    pub(super) fn store_bitfield(&mut self, addr: Value, storage_ty: IrType, bit_offset: u32, bit_width: u32, val: Operand) {
        // When bitfield occupies the entire storage unit, just store directly
        if bit_width >= 64 && bit_offset == 0 {
            self.emit(Instruction::Store { val, ptr: addr, ty: storage_ty });
            return;
        }

        let mask = if bit_width >= 64 { u64::MAX } else { (1u64 << bit_width) - 1 };

        // Mask the value to bit_width bits
        let masked_val = self.emit_binop_val(IrBinOp::And, val, Operand::Const(IrConst::I64(mask as i64)), IrType::I64);

        // Shift value to position
        let shifted_val = if bit_offset > 0 {
            self.emit_binop_val(IrBinOp::Shl, Operand::Value(masked_val), Operand::Const(IrConst::I64(bit_offset as i64)), IrType::I64)
        } else {
            masked_val
        };

        // Load current storage unit
        let old_val = self.fresh_value();
        self.emit(Instruction::Load { dest: old_val, ptr: addr, ty: storage_ty });

        // Clear the bitfield bits: old & ~(mask << bit_offset)
        let clear_mask = if bit_width >= 64 { 0u64 } else { !(mask << bit_offset) };
        let cleared = self.emit_binop_val(IrBinOp::And, Operand::Value(old_val), Operand::Const(IrConst::I64(clear_mask as i64)), IrType::I64);

        // OR in the new value
        let new_val = self.emit_binop_val(IrBinOp::Or, Operand::Value(cleared), Operand::Value(shifted_val), IrType::I64);

        // Store back
        self.emit(Instruction::Store { val: Operand::Value(new_val), ptr: addr, ty: storage_ty });
    }

    /// Lower struct/union assignment using memcpy.
    fn lower_struct_assign(&mut self, lhs: &Expr, rhs: &Expr) -> Operand {
        let struct_size = self.struct_value_size(lhs).unwrap_or(8);

        // For expressions producing packed struct data (small struct function
        // call returns, ternaries over them, etc.), the value IS the struct
        // data, not an address. Store it directly instead of memcpy.
        if self.expr_produces_packed_struct_data(rhs) && struct_size <= 8 {
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

        // Decompose complex double/float arguments into (real, imag) pairs for ABI compliance.
        // Per SysV ABI: _Complex double -> 2 F64 in XMM registers
        // _Complex float -> 2 F32 (packed in one XMM, but passed as separate F32 values)
        // _Complex long double -> memory (keep as pointer, handled via sret/stack)
        let param_ctypes_for_decompose = if let Expr::Identifier(name, _) = func {
            self.func_meta.param_ctypes.get(name).cloned()
        } else {
            None
        };
        self.decompose_complex_call_args(&mut arg_vals, &mut arg_types, &param_ctypes_for_decompose, args);

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

        // Determine variadic status and number of fixed (named) args.
        // After complex decomposition, each complex float/double param becomes 2 args,
        // so num_fixed_args must reflect the expanded count.
        let (call_variadic, num_fixed_args) = if let Expr::Identifier(name, _) = func {
            let variadic = self.is_function_variadic(name);
            let n_fixed = if variadic {
                // Count decomposed fixed params: complex float/double -> 2, others -> 1
                if let Some(pctypes) = self.func_meta.param_ctypes.get(name) {
                    pctypes.iter().map(|ct| {
                        if matches!(ct, CType::ComplexDouble) { 2 } else { 1 }
                    }).sum()
                } else {
                    self.func_meta.param_types.get(name)
                        .map(|p| p.len())
                        .unwrap_or(arg_vals.len())
                }
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

        // For complex returns (non-sret), the return value(s) need to be stored
        // into a complex alloca so consumers can treat it as a pointer to complex data.
        if sret_size.is_none() {
            if let Expr::Identifier(name, _) = func {
                if let Some(ret_ct) = self.func_return_ctypes.get(name).cloned() {
                    if ret_ct == CType::ComplexFloat {
                        // _Complex float: packed {real, imag} in a single I64 register
                        let alloca = self.fresh_value();
                        self.emit(Instruction::Alloca { dest: alloca, ty: IrType::Ptr, size: 8 });
                        self.emit(Instruction::Store { val: Operand::Value(dest), ptr: alloca, ty: IrType::I64 });
                        return Operand::Value(alloca);
                    } else if ret_ct == CType::ComplexDouble {
                        // _Complex double: real in xmm0 (dest), imag in xmm1 (dest2)
                        // The call returns real part in dest (F64).
                        // We need to get the second return value (imag) from the call.
                        // Use a ReturnImag instruction to capture the second FP return value.
                        let imag_val = self.fresh_value();
                        self.emit(Instruction::GetReturnF64Second { dest: imag_val });
                        let alloca = self.fresh_value();
                        self.emit(Instruction::Alloca { dest: alloca, ty: IrType::Ptr, size: 16 });
                        // Store real part at offset 0
                        self.emit(Instruction::Store { val: Operand::Value(dest), ptr: alloca, ty: IrType::F64 });
                        // Store imag part at offset 8
                        let imag_ptr = self.fresh_value();
                        self.emit(Instruction::BinOp {
                            dest: imag_ptr,
                            op: IrBinOp::Add,
                            lhs: Operand::Value(alloca),
                            rhs: Operand::Const(IrConst::I64(8)),
                            ty: IrType::I64,
                        });
                        self.emit(Instruction::Store { val: Operand::Value(imag_val), ptr: imag_ptr, ty: IrType::F64 });
                        return Operand::Value(alloca);
                    }
                }
            }
        }

        // Narrow the result if the return type is sub-64-bit integer
        self.maybe_narrow_call_result(dest, call_ret_ty)
    }

    /// Determine the return type for creal/crealf/creall/cimag/cimagf/cimagl based on function name.
    fn creal_return_type(name: &str) -> IrType {
        match name {
            "crealf" | "__builtin_crealf" | "cimagf" | "__builtin_cimagf" => IrType::F32,
            "creall" | "__builtin_creall" | "cimagl" | "__builtin_cimagl" => IrType::F128,
            _ => IrType::F64, // creal, cimag, __builtin_creal, __builtin_cimag
        }
    }

    /// Try to lower a __builtin_* call. Returns Some(result) if handled.
    fn try_lower_builtin_call(&mut self, name: &str, args: &[Expr]) -> Option<Operand> {
        // Handle alloca specially - it needs dynamic stack allocation
        match name {
            "__builtin_alloca" | "__builtin_alloca_with_align" => {
                // __builtin_alloca(size) -> dynamic stack allocation
                if let Some(size_expr) = args.first() {
                    let size_operand = self.lower_expr(size_expr);
                    let align = if name == "__builtin_alloca_with_align" && args.len() >= 2 {
                        // align is in bits
                        if let Expr::IntLiteral(bits, _) = &args[1] {
                            (*bits as usize) / 8
                        } else {
                            16
                        }
                    } else {
                        16 // default alignment
                    };
                    let dest = self.fresh_value();
                    self.emit(Instruction::DynAlloca { dest, size: size_operand, align });
                    return Some(Operand::Value(dest));
                }
                return Some(Operand::Const(IrConst::I64(0)));
            }
            _ => {}
        }
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
                // Determine the return type from the builtin name or the libc alias
                let return_type = Self::builtin_return_type(name)
                    .or_else(|| self.func_meta.return_types.get(libc_name.as_str()).copied())
                    .unwrap_or(IrType::I64);
                self.emit(Instruction::Call {
                    dest: Some(dest), func: libc_name.clone(),
                    args: arg_vals, arg_types, return_type, is_variadic: variadic, num_fixed_args: n_fixed,
                });
                Some(Operand::Value(dest))
            }
            BuiltinKind::Identity => {
                Some(args.first().map_or(Operand::Const(IrConst::I64(0)), |a| self.lower_expr(a)))
            }
            BuiltinKind::ConstantI64(val) => Some(Operand::Const(IrConst::I64(*val))),
            BuiltinKind::ConstantF64(val) => {
                // Return the appropriate float type based on the builtin name
                // __builtin_inff, __builtin_huge_valf return float (F32)
                let is_float_variant = name == "__builtin_inff"
                    || name == "__builtin_huge_valf";
                if is_float_variant {
                    Some(Operand::Const(IrConst::F32(*val as f32)))
                } else {
                    Some(Operand::Const(IrConst::F64(*val)))
                }
            }
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
                                let conv = self.emit_cast_val(lhs, lhs_ty, cmp_ty);
                                lhs = Operand::Value(conv);
                            }
                            if rhs_ty != cmp_ty {
                                let conv = self.emit_cast_val(rhs, rhs_ty, cmp_ty);
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
                            let dest = self.emit_cmp_val(cmp_op, lhs, rhs, cmp_ty);
                            return Some(Operand::Value(dest));
                        }
                        Some(Operand::Const(IrConst::I64(0)))
                    }
                    BuiltinIntrinsic::Clz => self.lower_unary_intrinsic(name, args, IrUnaryOp::Clz),
                    BuiltinIntrinsic::Ctz => self.lower_unary_intrinsic(name, args, IrUnaryOp::Ctz),
                    BuiltinIntrinsic::Bswap => self.lower_bswap_intrinsic(name, args),
                    BuiltinIntrinsic::Popcount => self.lower_unary_intrinsic(name, args, IrUnaryOp::Popcount),
                    BuiltinIntrinsic::Parity => self.lower_parity_intrinsic(name, args),
                    BuiltinIntrinsic::ComplexReal => {
                        // creal/crealf/creall: extract real part of complex argument.
                        // When the argument is not complex, convert it to the appropriate float type.
                        if !args.is_empty() {
                            let arg_ctype = self.expr_ctype(&args[0]);
                            if arg_ctype.is_complex() {
                                Some(self.lower_complex_real_part(&args[0]))
                            } else {
                                // Non-complex argument: convert to the target float type
                                let target_ty = Self::creal_return_type(name);
                                let val = self.lower_expr(&args[0]);
                                let val_ty = self.get_expr_type(&args[0]);
                                Some(self.emit_implicit_cast(val, val_ty, target_ty))
                            }
                        } else {
                            Some(Operand::Const(IrConst::F64(0.0)))
                        }
                    }
                    BuiltinIntrinsic::ComplexImag => {
                        // cimag/cimagf/cimagl: extract imaginary part of complex argument.
                        // When the argument is not complex, return 0 in the appropriate float type.
                        if !args.is_empty() {
                            let arg_ctype = self.expr_ctype(&args[0]);
                            if arg_ctype.is_complex() {
                                Some(self.lower_complex_imag_part(&args[0]))
                            } else {
                                // Non-complex argument: imaginary part is 0
                                let target_ty = Self::creal_return_type(name);
                                Some(match target_ty {
                                    IrType::F32 => Operand::Const(IrConst::F32(0.0)),
                                    IrType::F128 => Operand::Const(IrConst::LongDouble(0.0)),
                                    _ => Operand::Const(IrConst::F64(0.0)),
                                })
                            }
                        } else {
                            Some(Operand::Const(IrConst::F64(0.0)))
                        }
                    }
                    BuiltinIntrinsic::ComplexConj => {
                        // conj/conjf/conjl: negate imaginary part
                        if !args.is_empty() {
                            Some(self.lower_complex_conj(&args[0]))
                        } else {
                            Some(Operand::Const(IrConst::F64(0.0)))
                        }
                    }
                    BuiltinIntrinsic::Fence => {
                        // __sync_synchronize: emit a fence/barrier (no-op for now)
                        Some(Operand::Const(IrConst::I64(0)))
                    }
                    BuiltinIntrinsic::FpClassify => {
                        self.lower_builtin_fpclassify(args)
                    }
                    BuiltinIntrinsic::IsNan => {
                        self.lower_builtin_isnan(args)
                    }
                    BuiltinIntrinsic::IsInf => {
                        self.lower_builtin_isinf(args)
                    }
                    BuiltinIntrinsic::IsFinite => {
                        self.lower_builtin_isfinite(args)
                    }
                    BuiltinIntrinsic::IsNormal => {
                        self.lower_builtin_isnormal(args)
                    }
                    BuiltinIntrinsic::SignBit => {
                        self.lower_builtin_signbit(args)
                    }
                    BuiltinIntrinsic::IsInfSign => {
                        self.lower_builtin_isinf_sign(args)
                    }
                    BuiltinIntrinsic::Alloca => {
                        // Handled earlier in try_lower_builtin_call - should not reach here
                        Some(Operand::Const(IrConst::I64(0)))
                    }
                    BuiltinIntrinsic::ComplexConstruct => {
                        // __builtin_complex(real, imag) -> _Complex double
                        // Creates a complex value from two FP arguments.
                        // The result type is determined by the argument types:
                        // double args -> _Complex double, float args -> _Complex float
                        if args.len() >= 2 {
                            let real_val = self.lower_expr(&args[0]);
                            let imag_val = self.lower_expr(&args[1]);
                            let arg_ty = self.get_expr_type(&args[0]);
                            let (comp_ty, complex_size, comp_size) = if arg_ty == IrType::F32 {
                                (IrType::F32, 8usize, 4usize)
                            } else {
                                (IrType::F64, 16usize, 8usize)
                            };
                            // Allocate complex-sized stack slot
                            let alloca = self.fresh_value();
                            self.emit(Instruction::Alloca {
                                dest: alloca,
                                ty: IrType::Ptr,
                                size: complex_size,
                            });
                            // Store real part at offset 0
                            self.emit(Instruction::Store {
                                val: real_val,
                                ptr: alloca,
                                ty: comp_ty,
                            });
                            // Store imag part at offset comp_size
                            let imag_ptr = self.fresh_value();
                            self.emit(Instruction::GetElementPtr {
                                dest: imag_ptr,
                                base: alloca,
                                offset: Operand::Const(IrConst::I64(comp_size as i64)),
                                ty: IrType::I8,
                            });
                            self.emit(Instruction::Store {
                                val: imag_val,
                                ptr: imag_ptr,
                                ty: comp_ty,
                            });
                            Some(Operand::Value(alloca))
                        } else {
                            Some(Operand::Const(IrConst::I64(0)))
                        }
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
        let dest = self.emit_binop_val(IrBinOp::And, Operand::Value(pop), Operand::Const(IrConst::I64(1)), ty);
        Some(Operand::Value(dest))
    }

    // =========================================================================
    // Floating-point classification builtins
    // =========================================================================

    /// Helper: get the type of the last argument for fp classification builtins.
    /// Returns the IrType and the lowered operand.
    fn lower_fp_classify_arg(&mut self, args: &[Expr]) -> (IrType, Operand) {
        let arg_expr = args.last().unwrap();
        let arg_ty = self.get_expr_type(arg_expr);
        let arg_val = self.lower_expr(arg_expr);
        (arg_ty, arg_val)
    }

    /// Bitcast a float/double to its integer bit representation via memory.
    /// F32 -> I32, F64 -> I64. Uses alloca+store+load for a true bitcast
    /// (not a numeric conversion like Cast does).
    fn bitcast_float_to_int(&mut self, val: Operand, float_ty: IrType) -> (Operand, IrType) {
        let int_ty = match float_ty {
            IrType::F32 => IrType::I32,
            _ => IrType::I64, // F64 and F128/long double treated as F64
        };
        let size = if float_ty == IrType::F32 { 4 } else { 8 };

        // Allocate a temporary stack slot
        let tmp_alloca = self.fresh_value();
        self.emit(Instruction::Alloca {
            dest: tmp_alloca, size, ty: float_ty,
        });

        // Store the float value to the slot
        let val_v = self.operand_to_value(val);
        self.emit(Instruction::Store {
            val: Operand::Value(val_v), ptr: tmp_alloca, ty: float_ty,
        });

        // Load it back as integer
        let result = self.fresh_value();
        self.emit(Instruction::Load {
            dest: result, ptr: tmp_alloca, ty: int_ty,
        });

        (Operand::Value(result), int_ty)
    }

    /// Floating-point bit-layout constants for classification.
    /// Returns (abs_mask, exp_only, exp_shift, exp_field_max, mant_mask) for the given float type.
    fn fp_masks(float_ty: IrType) -> (i64, i64, i64, i64, i64) {
        match float_ty {
            IrType::F32 => (
                0x7FFFFFFF_i64,             // abs_mask
                0x7F800000_i64,             // exp_only
                23,                         // exp_shift
                0xFF,                       // exp_field_max
                0x007FFFFF_i64,             // mant_mask
            ),
            _ => (
                0x7FFFFFFFFFFFFFFF_i64,     // abs_mask
                0x7FF0000000000000_u64 as i64, // exp_only
                52,                         // exp_shift
                0x7FF,                      // exp_field_max
                0x000FFFFFFFFFFFFF_u64 as i64, // mant_mask
            ),
        }
    }

    /// Compute the absolute value (sign-stripped) of float bits: bits & abs_mask.
    fn fp_abs_bits(&mut self, bits: &Operand, int_ty: IrType, abs_mask: i64) -> Value {
        self.emit_binop_val(IrBinOp::And, bits.clone(), Operand::Const(IrConst::I64(abs_mask)), int_ty)
    }

    /// Lower __builtin_fpclassify(nan_val, inf_val, norm_val, subnorm_val, zero_val, x)
    /// Returns one of the first 5 arguments based on the classification of x.
    ///
    /// Uses arithmetic select: result = sum of (class_val[i] * is_class[i]).
    fn lower_builtin_fpclassify(&mut self, args: &[Expr]) -> Option<Operand> {
        if args.len() < 6 {
            return Some(Operand::Const(IrConst::I64(0)));
        }

        // Lower the classification constants (args 0-4) and the value to classify (arg 5)
        let class_vals: Vec<_> = (0..5).map(|i| self.lower_expr(&args[i])).collect();
        let arg_ty = self.get_expr_type(&args[5]);
        let arg_val = self.lower_expr(&args[5]);

        // Bitcast to integer to examine bits
        let (bits, int_ty) = self.bitcast_float_to_int(arg_val, arg_ty);
        let (_abs_mask, _exp_only, exp_shift, exp_field_max, mant_mask) = Self::fp_masks(arg_ty);

        // Extract exponent and mantissa fields
        let shifted = self.emit_binop_val(IrBinOp::LShr, bits.clone(), Operand::Const(IrConst::I64(exp_shift)), int_ty);
        let exponent = self.emit_binop_val(IrBinOp::And, Operand::Value(shifted), Operand::Const(IrConst::I64(exp_field_max)), int_ty);
        let mantissa = self.emit_binop_val(IrBinOp::And, bits, Operand::Const(IrConst::I64(mant_mask)), int_ty);

        // Classification conditions
        let exp_is_max = self.emit_cmp_val(IrCmpOp::Eq, Operand::Value(exponent), Operand::Const(IrConst::I64(exp_field_max)), int_ty);
        let exp_is_zero = self.emit_cmp_val(IrCmpOp::Eq, Operand::Value(exponent), Operand::Const(IrConst::I64(0)), int_ty);
        let mant_is_zero = self.emit_cmp_val(IrCmpOp::Eq, Operand::Value(mantissa), Operand::Const(IrConst::I64(0)), int_ty);
        let mant_not_zero = self.emit_cmp_val(IrCmpOp::Ne, Operand::Value(mantissa), Operand::Const(IrConst::I64(0)), int_ty);

        // Compute class flags: is_nan, is_inf, is_zero, is_subnorm
        let is_nan = self.emit_binop_val(IrBinOp::And, Operand::Value(exp_is_max), Operand::Value(mant_not_zero), IrType::I32);
        let is_inf = self.emit_binop_val(IrBinOp::And, Operand::Value(exp_is_max), Operand::Value(mant_is_zero), IrType::I32);
        let is_zero = self.emit_binop_val(IrBinOp::And, Operand::Value(exp_is_zero), Operand::Value(mant_is_zero), IrType::I32);
        let is_subnorm = self.emit_binop_val(IrBinOp::And, Operand::Value(exp_is_zero), Operand::Value(mant_not_zero), IrType::I32);

        // is_normal = !(is_nan | is_inf | is_zero | is_subnorm)
        let mut special = is_nan;
        for &flag in &[is_inf, is_zero, is_subnorm] {
            special = self.emit_binop_val(IrBinOp::Or, Operand::Value(special), Operand::Value(flag), IrType::I32);
        }
        let is_normal = self.emit_binop_val(IrBinOp::Xor, Operand::Value(special), Operand::Const(IrConst::I64(1)), IrType::I32);

        // result = sum of (class_val[i] * is_class[i])
        // Order: nan=0, inf=1, normal=2, subnormal=3, zero=4
        let class_flags = [is_nan, is_inf, is_normal, is_subnorm, is_zero];
        let contribs: Vec<_> = class_vals.iter().zip(class_flags.iter())
            .map(|(val, &flag)| (val.clone(), flag))
            .collect();
        let mut result = self.emit_binop_val(IrBinOp::Mul, contribs[0].0.clone(), Operand::Value(contribs[0].1), IrType::I64);
        for &(ref val, flag) in &contribs[1..] {
            let contrib = self.emit_binop_val(IrBinOp::Mul, val.clone(), Operand::Value(flag), IrType::I64);
            result = self.emit_binop_val(IrBinOp::Add, Operand::Value(result), Operand::Value(contrib), IrType::I64);
        }

        Some(Operand::Value(result))
    }

    /// Lower __builtin_isnan(x) -> 1 if x is NaN, 0 otherwise.
    /// NaN: abs(bits) > exp_only (exponent all 1s AND mantissa non-zero).
    fn lower_builtin_isnan(&mut self, args: &[Expr]) -> Option<Operand> {
        if args.is_empty() { return Some(Operand::Const(IrConst::I64(0))); }
        let (arg_ty, arg_val) = self.lower_fp_classify_arg(args);
        let (bits, int_ty) = self.bitcast_float_to_int(arg_val, arg_ty);
        let (abs_mask, exp_only, _, _, _) = Self::fp_masks(arg_ty);

        let abs_val = self.fp_abs_bits(&bits, int_ty, abs_mask);
        let result = self.emit_cmp_val(IrCmpOp::Ugt, Operand::Value(abs_val), Operand::Const(IrConst::I64(exp_only)), int_ty);
        Some(Operand::Value(result))
    }

    /// Lower __builtin_isinf(x) -> nonzero if x is +/-infinity.
    /// Inf: abs(bits) == exp_only.
    fn lower_builtin_isinf(&mut self, args: &[Expr]) -> Option<Operand> {
        if args.is_empty() { return Some(Operand::Const(IrConst::I64(0))); }
        let (arg_ty, arg_val) = self.lower_fp_classify_arg(args);
        let (bits, int_ty) = self.bitcast_float_to_int(arg_val, arg_ty);
        let (abs_mask, exp_only, _, _, _) = Self::fp_masks(arg_ty);

        let abs_val = self.fp_abs_bits(&bits, int_ty, abs_mask);
        let result = self.emit_cmp_val(IrCmpOp::Eq, Operand::Value(abs_val), Operand::Const(IrConst::I64(exp_only)), int_ty);
        Some(Operand::Value(result))
    }

    /// Lower __builtin_isfinite(x) -> 1 if x is finite (not inf or nan).
    /// Finite: (bits & exp_only) != exp_only.
    fn lower_builtin_isfinite(&mut self, args: &[Expr]) -> Option<Operand> {
        if args.is_empty() { return Some(Operand::Const(IrConst::I64(0))); }
        let (arg_ty, arg_val) = self.lower_fp_classify_arg(args);
        let (bits, int_ty) = self.bitcast_float_to_int(arg_val, arg_ty);
        let (_, exp_only, _, _, _) = Self::fp_masks(arg_ty);

        let exp_bits = self.emit_binop_val(IrBinOp::And, bits, Operand::Const(IrConst::I64(exp_only)), int_ty);
        let result = self.emit_cmp_val(IrCmpOp::Ne, Operand::Value(exp_bits), Operand::Const(IrConst::I64(exp_only)), int_ty);
        Some(Operand::Value(result))
    }

    /// Lower __builtin_isnormal(x) -> 1 if x is a normal number.
    /// Normal: exponent != 0 AND exponent != max.
    fn lower_builtin_isnormal(&mut self, args: &[Expr]) -> Option<Operand> {
        if args.is_empty() { return Some(Operand::Const(IrConst::I64(0))); }
        let (arg_ty, arg_val) = self.lower_fp_classify_arg(args);
        let (bits, int_ty) = self.bitcast_float_to_int(arg_val, arg_ty);
        let (_, exp_only, exp_shift, exp_field_max, _) = Self::fp_masks(arg_ty);

        let exp_bits = self.emit_binop_val(IrBinOp::And, bits, Operand::Const(IrConst::I64(exp_only)), int_ty);
        let exponent = self.emit_binop_val(IrBinOp::LShr, Operand::Value(exp_bits), Operand::Const(IrConst::I64(exp_shift)), int_ty);

        let exp_nonzero = self.emit_cmp_val(IrCmpOp::Ne, Operand::Value(exponent), Operand::Const(IrConst::I64(0)), int_ty);
        let exp_not_max = self.emit_cmp_val(IrCmpOp::Ne, Operand::Value(exponent), Operand::Const(IrConst::I64(exp_field_max)), int_ty);
        let result = self.emit_binop_val(IrBinOp::And, Operand::Value(exp_nonzero), Operand::Value(exp_not_max), IrType::I32);
        Some(Operand::Value(result))
    }

    /// Lower __builtin_signbit(x) -> nonzero if the sign bit of x is set.
    fn lower_builtin_signbit(&mut self, args: &[Expr]) -> Option<Operand> {
        if args.is_empty() { return Some(Operand::Const(IrConst::I64(0))); }
        let (arg_ty, arg_val) = self.lower_fp_classify_arg(args);
        let (bits, int_ty) = self.bitcast_float_to_int(arg_val, arg_ty);
        let sign_shift = if arg_ty == IrType::F32 { 31_i64 } else { 63_i64 };

        let shifted = self.emit_binop_val(IrBinOp::LShr, bits, Operand::Const(IrConst::I64(sign_shift)), int_ty);
        let result = self.emit_binop_val(IrBinOp::And, Operand::Value(shifted), Operand::Const(IrConst::I64(1)), int_ty);
        Some(Operand::Value(result))
    }

    /// Lower __builtin_isinf_sign(x) -> -1 if -inf, +1 if +inf, 0 otherwise.
    fn lower_builtin_isinf_sign(&mut self, args: &[Expr]) -> Option<Operand> {
        if args.is_empty() { return Some(Operand::Const(IrConst::I64(0))); }
        let (arg_ty, arg_val) = self.lower_fp_classify_arg(args);
        let (bits, int_ty) = self.bitcast_float_to_int(arg_val, arg_ty);
        let (abs_mask, exp_only, _, _, _) = Self::fp_masks(arg_ty);
        let sign_shift = if arg_ty == IrType::F32 { 31_i64 } else { 63_i64 };

        // Check if infinite: abs(bits) == exp_only
        let abs_val = self.fp_abs_bits(&bits, int_ty, abs_mask);
        let is_inf = self.emit_cmp_val(IrCmpOp::Eq, Operand::Value(abs_val), Operand::Const(IrConst::I64(exp_only)), int_ty);

        // sign = (bits >> sign_shift) & 1
        let sign_shifted = self.emit_binop_val(IrBinOp::LShr, bits, Operand::Const(IrConst::I64(sign_shift)), int_ty);
        let sign_bit = self.emit_binop_val(IrBinOp::And, Operand::Value(sign_shifted), Operand::Const(IrConst::I64(1)), int_ty);

        // direction = 1 - 2*sign_bit  (+1 for positive, -1 for negative)
        let sign_x2 = self.emit_binop_val(IrBinOp::Mul, Operand::Value(sign_bit), Operand::Const(IrConst::I64(2)), IrType::I64);
        let direction = self.emit_binop_val(IrBinOp::Sub, Operand::Const(IrConst::I64(1)), Operand::Value(sign_x2), IrType::I64);

        // result = is_inf * direction
        let result = self.emit_binop_val(IrBinOp::Mul, Operand::Value(is_inf), Operand::Value(direction), IrType::I64);
        Some(Operand::Value(result))
    }

    /// Try to lower a GCC atomic builtin (__atomic_* or __sync_*).
    ///
    /// Uses table-driven dispatch to avoid repetitive if-name-matches blocks.
    /// The __atomic_* variants take an explicit ordering argument; __sync_* always use SeqCst.
    /// The _n variants pass values directly; non-_n variants pass through pointers.
    fn try_lower_atomic_builtin(&mut self, name: &str, args: &[Expr]) -> Option<Operand> {
        let val_ty = if !args.is_empty() {
            self.get_pointee_ir_type(&args[0]).unwrap_or(IrType::I64)
        } else {
            IrType::I64
        };

        // --- Fetch-op: atomic RMW returning old value ---
        // __atomic_fetch_OP(ptr, val, order) and __sync_fetch_and_OP(ptr, val) [SeqCst]
        if let Some((op, is_sync)) = Self::classify_fetch_op(name) {
            let min_args = if is_sync { 2 } else { 3 };
            if args.len() >= min_args {
                let ptr = self.lower_expr(&args[0]);
                let val = self.lower_expr(&args[1]);
                let ordering = if is_sync {
                    AtomicOrdering::SeqCst
                } else {
                    Self::parse_ordering(&args[2])
                };
                let dest = self.fresh_value();
                self.emit(Instruction::AtomicRmw { dest, op, ptr, val, ty: val_ty, ordering });
                return Some(Operand::Value(dest));
            }
        }

        // --- Op-fetch: atomic RMW returning new value (old op val) ---
        // __atomic_OP_fetch(ptr, val, order) and __sync_OP_and_fetch(ptr, val) [SeqCst]
        if let Some((rmw_op, bin_op, is_nand, is_sync)) = Self::classify_op_fetch(name) {
            let min_args = if is_sync { 2 } else { 3 };
            if args.len() >= min_args {
                let ptr = self.lower_expr(&args[0]);
                let val_expr = self.lower_expr(&args[1]);
                let ordering = if is_sync {
                    AtomicOrdering::SeqCst
                } else {
                    Self::parse_ordering(&args[2])
                };
                let old_val = self.fresh_value();
                self.emit(Instruction::AtomicRmw {
                    dest: old_val, op: rmw_op, ptr, val: val_expr.clone(), ty: val_ty, ordering,
                });
                let new_val = self.emit_post_rmw_compute(old_val, val_expr, bin_op, is_nand, val_ty);
                return Some(Operand::Value(new_val));
            }
        }

        // --- Exchange ---
        // __atomic_exchange_n(ptr, val, order) -> old value
        if name == "__atomic_exchange_n" && args.len() >= 3 {
            let ptr = self.lower_expr(&args[0]);
            let val = self.lower_expr(&args[1]);
            let ordering = Self::parse_ordering(&args[2]);
            return Some(Operand::Value(self.emit_atomic_xchg(ptr, val, val_ty, ordering)));
        }
        // __atomic_exchange(ptr, val_ptr, ret_ptr, order) -> void
        if name == "__atomic_exchange" && args.len() >= 4 {
            let ptr = self.lower_expr(&args[0]);
            let val_ptr_op = self.lower_expr(&args[1]);
            let ret_ptr_op = self.lower_expr(&args[2]);
            let ordering = Self::parse_ordering(&args[3]);
            let val = self.load_through_ptr(val_ptr_op, val_ty);
            let old_val = self.emit_atomic_xchg(ptr, Operand::Value(val), val_ty, ordering);
            self.store_through_ptr(ret_ptr_op, Operand::Value(old_val), val_ty);
            return Some(Operand::Const(IrConst::I64(0)));
        }

        // --- Compare-exchange ---
        // __atomic_compare_exchange_n(ptr, expected_ptr, desired, weak, success_order, fail_order)
        if name == "__atomic_compare_exchange_n" && args.len() >= 6 {
            let ptr = self.lower_expr(&args[0]);
            let expected_ptr_op = self.lower_expr(&args[1]);
            let expected = self.load_through_ptr(expected_ptr_op.clone(), val_ty);
            let desired = self.lower_expr(&args[2]);
            return Some(self.emit_cmpxchg_with_writeback(
                ptr, expected_ptr_op, expected, desired, val_ty,
                Self::parse_ordering(&args[4]), Self::parse_ordering(&args[5]),
            ));
        }
        // __atomic_compare_exchange(ptr, expected_ptr, desired_ptr, weak, success_order, fail_order)
        if name == "__atomic_compare_exchange" && args.len() >= 6 {
            let ptr = self.lower_expr(&args[0]);
            let expected_ptr_op = self.lower_expr(&args[1]);
            let desired_ptr_op = self.lower_expr(&args[2]);
            let expected = self.load_through_ptr(expected_ptr_op.clone(), val_ty);
            let desired = self.load_through_ptr(desired_ptr_op, val_ty);
            return Some(self.emit_cmpxchg_with_writeback(
                ptr, expected_ptr_op, expected, Operand::Value(desired), val_ty,
                Self::parse_ordering(&args[4]), Self::parse_ordering(&args[5]),
            ));
        }

        // --- Load ---
        // __atomic_load_n(ptr, order) -> value
        if name == "__atomic_load_n" && args.len() >= 2 {
            let ptr = self.lower_expr(&args[0]);
            let ordering = Self::parse_ordering(&args[1]);
            let dest = self.fresh_value();
            self.emit(Instruction::AtomicLoad { dest, ptr, ty: val_ty, ordering });
            return Some(Operand::Value(dest));
        }
        // __atomic_load(ptr, ret_ptr, order) -> void
        if name == "__atomic_load" && args.len() >= 3 {
            let ptr = self.lower_expr(&args[0]);
            let ret_ptr = self.lower_expr(&args[1]);
            let ordering = Self::parse_ordering(&args[2]);
            let loaded = self.fresh_value();
            self.emit(Instruction::AtomicLoad { dest: loaded, ptr, ty: val_ty, ordering });
            self.store_through_ptr(ret_ptr, Operand::Value(loaded), val_ty);
            return Some(Operand::Const(IrConst::I64(0)));
        }

        // --- Store ---
        // __atomic_store_n(ptr, val, order) -> void
        if name == "__atomic_store_n" && args.len() >= 3 {
            let ptr = self.lower_expr(&args[0]);
            let val = self.lower_expr(&args[1]);
            let ordering = Self::parse_ordering(&args[2]);
            self.emit(Instruction::AtomicStore { ptr, val, ty: val_ty, ordering });
            return Some(Operand::Const(IrConst::I64(0)));
        }
        // __atomic_store(ptr, val_ptr, order) -> void
        if name == "__atomic_store" && args.len() >= 3 {
            let ptr = self.lower_expr(&args[0]);
            let val_ptr = self.lower_expr(&args[1]);
            let ordering = Self::parse_ordering(&args[2]);
            let loaded = self.load_through_ptr(val_ptr, val_ty);
            self.emit(Instruction::AtomicStore { ptr, val: Operand::Value(loaded), ty: val_ty, ordering });
            return Some(Operand::Const(IrConst::I64(0)));
        }

        // --- Test-and-set / clear ---
        if name == "__atomic_test_and_set" && args.len() >= 2 {
            let ptr = self.lower_expr(&args[0]);
            let ordering = Self::parse_ordering(&args[1]);
            let dest = self.fresh_value();
            self.emit(Instruction::AtomicRmw {
                dest, op: AtomicRmwOp::TestAndSet,
                ptr, val: Operand::Const(IrConst::I64(1)), ty: IrType::I8, ordering,
            });
            return Some(Operand::Value(dest));
        }
        if name == "__atomic_clear" && args.len() >= 2 {
            let ptr = self.lower_expr(&args[0]);
            let ordering = Self::parse_ordering(&args[1]);
            self.emit(Instruction::AtomicStore {
                ptr, val: Operand::Const(IrConst::I8(0)), ty: IrType::I8, ordering,
            });
            return Some(Operand::Const(IrConst::I64(0)));
        }

        // --- Sync compare-and-swap ---
        // __sync_val_compare_and_swap(ptr, old, new) -> old value
        // __sync_bool_compare_and_swap(ptr, old, new) -> bool
        if (name == "__sync_val_compare_and_swap" || name == "__sync_bool_compare_and_swap")
            && args.len() >= 3
        {
            let ptr = self.lower_expr(&args[0]);
            let expected = self.lower_expr(&args[1]);
            let desired = self.lower_expr(&args[2]);
            let returns_bool = name == "__sync_bool_compare_and_swap";
            let dest = self.fresh_value();
            self.emit(Instruction::AtomicCmpxchg {
                dest, ptr, expected, desired, ty: val_ty,
                success_ordering: AtomicOrdering::SeqCst,
                failure_ordering: AtomicOrdering::SeqCst,
                returns_bool,
            });
            return Some(Operand::Value(dest));
        }

        // --- Sync lock operations ---
        if name == "__sync_lock_test_and_set" && args.len() >= 2 {
            let ptr = self.lower_expr(&args[0]);
            let val = self.lower_expr(&args[1]);
            return Some(Operand::Value(
                self.emit_atomic_xchg(ptr, val, val_ty, AtomicOrdering::Acquire),
            ));
        }
        if name == "__sync_lock_release" && !args.is_empty() {
            let ptr = self.lower_expr(&args[0]);
            self.emit(Instruction::AtomicStore {
                ptr, val: Operand::Const(IrConst::I64(0)), ty: val_ty, ordering: AtomicOrdering::Release,
            });
            return Some(Operand::Const(IrConst::I64(0)));
        }

        // --- Fences ---
        if name == "__sync_synchronize" {
            self.emit(Instruction::Fence { ordering: AtomicOrdering::SeqCst });
            return Some(Operand::Const(IrConst::I64(0)));
        }
        if name == "__atomic_thread_fence" || name == "__atomic_signal_fence" {
            let ordering = if !args.is_empty() { Self::parse_ordering(&args[0]) } else { AtomicOrdering::SeqCst };
            self.emit(Instruction::Fence { ordering });
            return Some(Operand::Const(IrConst::I64(0)));
        }

        // --- Lock-free queries (always true for sizes <= 8) ---
        if name == "__atomic_is_lock_free" || name == "__atomic_always_lock_free" {
            return Some(Operand::Const(IrConst::I64(1)));
        }

        None
    }

    // --- Atomic builtin helpers ---

    /// Parse a memory ordering constant from an expression.
    fn parse_ordering(arg: &Expr) -> AtomicOrdering {
        match arg {
            Expr::IntLiteral(v, _) => match *v as i32 {
                0 => AtomicOrdering::Relaxed,
                1 | 2 => AtomicOrdering::Acquire, // consume maps to acquire
                3 => AtomicOrdering::Release,
                4 => AtomicOrdering::AcqRel,
                _ => AtomicOrdering::SeqCst,
            },
            _ => AtomicOrdering::SeqCst,
        }
    }

    /// Classify __atomic_fetch_OP / __sync_fetch_and_OP builtins.
    /// Returns (rmw_op, is_sync).
    fn classify_fetch_op(name: &str) -> Option<(AtomicRmwOp, bool)> {
        match name {
            "__atomic_fetch_add" => Some((AtomicRmwOp::Add, false)),
            "__atomic_fetch_sub" => Some((AtomicRmwOp::Sub, false)),
            "__atomic_fetch_and" => Some((AtomicRmwOp::And, false)),
            "__atomic_fetch_or"  => Some((AtomicRmwOp::Or, false)),
            "__atomic_fetch_xor" => Some((AtomicRmwOp::Xor, false)),
            "__atomic_fetch_nand" => Some((AtomicRmwOp::Nand, false)),
            "__sync_fetch_and_add" => Some((AtomicRmwOp::Add, true)),
            "__sync_fetch_and_sub" => Some((AtomicRmwOp::Sub, true)),
            "__sync_fetch_and_and" => Some((AtomicRmwOp::And, true)),
            "__sync_fetch_and_or"  => Some((AtomicRmwOp::Or, true)),
            "__sync_fetch_and_xor" => Some((AtomicRmwOp::Xor, true)),
            "__sync_fetch_and_nand" => Some((AtomicRmwOp::Nand, true)),
            _ => None,
        }
    }

    /// Classify __atomic_OP_fetch / __sync_OP_and_fetch builtins.
    /// Returns (rmw_op, bin_op_for_recompute, is_nand, is_sync).
    fn classify_op_fetch(name: &str) -> Option<(AtomicRmwOp, IrBinOp, bool, bool)> {
        match name {
            "__atomic_add_fetch" => Some((AtomicRmwOp::Add, IrBinOp::Add, false, false)),
            "__atomic_sub_fetch" => Some((AtomicRmwOp::Sub, IrBinOp::Sub, false, false)),
            "__atomic_and_fetch" => Some((AtomicRmwOp::And, IrBinOp::And, false, false)),
            "__atomic_or_fetch"  => Some((AtomicRmwOp::Or, IrBinOp::Or, false, false)),
            "__atomic_xor_fetch" => Some((AtomicRmwOp::Xor, IrBinOp::Xor, false, false)),
            "__atomic_nand_fetch" => Some((AtomicRmwOp::Nand, IrBinOp::And, true, false)),
            "__sync_add_and_fetch" => Some((AtomicRmwOp::Add, IrBinOp::Add, false, true)),
            "__sync_sub_and_fetch" => Some((AtomicRmwOp::Sub, IrBinOp::Sub, false, true)),
            "__sync_and_and_fetch" => Some((AtomicRmwOp::And, IrBinOp::And, false, true)),
            "__sync_or_and_fetch"  => Some((AtomicRmwOp::Or, IrBinOp::Or, false, true)),
            "__sync_xor_and_fetch" => Some((AtomicRmwOp::Xor, IrBinOp::Xor, false, true)),
            "__sync_nand_and_fetch" => Some((AtomicRmwOp::Nand, IrBinOp::And, true, true)),
            _ => None,
        }
    }

    /// After an atomic RMW that returns the old value, compute the new value.
    /// For nand: ~(old & val). For others: old bin_op val.
    fn emit_post_rmw_compute(
        &mut self, old_val: Value, val_expr: Operand, bin_op: IrBinOp, is_nand: bool, ty: IrType,
    ) -> Value {
        if is_nand {
            let and_val = self.emit_binop_val(IrBinOp::And, Operand::Value(old_val), val_expr, ty);
            let result = self.fresh_value();
            self.emit(Instruction::UnaryOp {
                dest: result, op: IrUnaryOp::Not, src: Operand::Value(and_val), ty,
            });
            result
        } else {
            self.emit_binop_val(bin_op, Operand::Value(old_val), val_expr, ty)
        }
    }

    /// Emit an atomic exchange (xchg) instruction, returning the old value.
    fn emit_atomic_xchg(
        &mut self, ptr: Operand, val: Operand, ty: IrType, ordering: AtomicOrdering,
    ) -> Value {
        let dest = self.fresh_value();
        self.emit(Instruction::AtomicRmw {
            dest, op: AtomicRmwOp::Xchg, ptr, val, ty, ordering,
        });
        dest
    }

    /// Emit a compare-exchange with writeback to expected_ptr and equality comparison.
    /// Shared by __atomic_compare_exchange and __atomic_compare_exchange_n.
    fn emit_cmpxchg_with_writeback(
        &mut self,
        ptr: Operand,
        expected_ptr_op: Operand,
        expected: Value,
        desired: Operand,
        ty: IrType,
        success_ordering: AtomicOrdering,
        failure_ordering: AtomicOrdering,
    ) -> Operand {
        let old_val = self.fresh_value();
        self.emit(Instruction::AtomicCmpxchg {
            dest: old_val, ptr, expected: Operand::Value(expected), desired,
            ty, success_ordering, failure_ordering, returns_bool: false,
        });
        // Store old value back to expected_ptr (updates expected on failure)
        self.store_through_ptr(expected_ptr_op, Operand::Value(old_val), ty);
        // Compare old == expected to produce bool result
        let result = self.emit_cmp_val(IrCmpOp::Eq, Operand::Value(old_val), Operand::Value(expected), ty);
        Operand::Value(result)
    }

    /// Load a value through a pointer operand.
    fn load_through_ptr(&mut self, ptr_op: Operand, ty: IrType) -> Value {
        let ptr_val = self.operand_to_value(ptr_op);
        let dest = self.fresh_value();
        self.emit(Instruction::Load { dest, ptr: ptr_val, ty });
        dest
    }

    /// Store a value through a pointer operand.
    fn store_through_ptr(&mut self, ptr_op: Operand, val: Operand, ty: IrType) {
        let ptr_val = self.operand_to_value(ptr_op);
        self.emit(Instruction::Store { val, ptr: ptr_val, ty });
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
        // Extract function name once and look up all metadata together
        // (previously 4 separate if-let Identifier matches)
        let func_name = if let Expr::Identifier(name, _) = func { Some(name.as_str()) } else { None };
        let param_types: Option<Vec<IrType>> = func_name.and_then(|name|
            self.func_meta.param_types.get(name).cloned()
                .or_else(|| self.func_meta.ptr_param_types.get(name).cloned())
        );
        let param_ctypes: Option<Vec<CType>> = func_name.and_then(|name|
            self.func_meta.param_ctypes.get(name).cloned()
        );
        let param_bool_flags: Option<Vec<bool>> = func_name.and_then(|name|
            self.func_meta.param_bool_flags.get(name).cloned()
        );
        let pre_call_variadic = func_name.map_or(false, |name|
            self.is_function_variadic(name)
        );

        let mut arg_types = Vec::with_capacity(args.len());
        let arg_vals: Vec<Operand> = args.iter().enumerate().map(|(i, a)| {
            let mut val = self.lower_expr(a);
            let arg_ty = self.get_expr_type(a);

            // Convert complex arguments to the declared parameter complex type if they differ.
            // E.g., passing _Complex float to a parameter of _Complex double.
            if let Some(ref pctypes) = param_ctypes {
                if i < pctypes.len() && pctypes[i].is_complex() {
                    let arg_ct = self.expr_ctype(a);
                    if arg_ct.is_complex() && arg_ct != pctypes[i] {
                        let ptr = self.operand_to_value(val);
                        val = self.complex_to_complex(ptr, &arg_ct, &pctypes[i]);
                    } else if !arg_ct.is_complex() {
                        // Real-to-complex: e.g., passing int to _Complex double param
                        val = self.real_to_complex(val, &arg_ct, &pctypes[i]);
                    }
                }
            }

            // When a struct-returning function call's result is passed directly as
            // an argument to another function, the caller passes the raw packed data
            // (in rax) but the callee expects a pointer to the struct data.
            // Spill non-sret struct return values to a temporary alloca.
            if let Expr::FunctionCall(func_expr, _, _) = a {
                let is_struct_ret = matches!(
                    self.get_expr_ctype(a),
                    Some(CType::Struct(_)) | Some(CType::Union(_))
                );
                if is_struct_ret {
                    let is_sret = if let Expr::Identifier(name, _) = func_expr.as_ref() {
                        self.func_meta.sret_functions.contains_key(name)
                    } else {
                        false
                    };
                    if !is_sret {
                        // Non-sret: return value is packed struct data, not a pointer.
                        // Store it to a temporary alloca so we can pass the address.
                        let struct_size = self.struct_value_size(a).unwrap_or(8);
                        let alloc_size = if struct_size > 0 { struct_size } else { 8 };
                        let alloca = self.fresh_value();
                        self.emit(Instruction::Alloca { dest: alloca, size: alloc_size, ty: IrType::I64 });
                        self.emit(Instruction::Store { val, ptr: alloca, ty: IrType::I64 });
                        val = Operand::Value(alloca);
                    }
                }
            }

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
            let narrowed = self.emit_cast_val(Operand::Value(dest), IrType::I64, ret_ty);
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
        let target_resolved = self.resolve_type_spec(target_type);
        let target_ctype = self.type_spec_to_ctype(&target_resolved);
        let inner_ctype = self.expr_ctype(inner);

        // Handle complex type casts
        if target_ctype.is_complex() && !inner_ctype.is_complex() {
            // Cast real to complex: (_Complex double)x -> {x, 0}
            let val = self.lower_expr(inner);
            return self.real_to_complex(val, &inner_ctype, &target_ctype);
        }
        if target_ctype.is_complex() && inner_ctype.is_complex() {
            // Cast complex to complex: (_Complex float)z where z is _Complex double
            if target_ctype == inner_ctype {
                return self.lower_expr(inner);
            }
            let val = self.lower_expr(inner);
            let ptr = self.operand_to_value(val);
            return self.complex_to_complex(ptr, &inner_ctype, &target_ctype);
        }
        if !target_ctype.is_complex() && inner_ctype.is_complex() {
            // Cast complex to real: (double)z -> extract real part
            let val = self.lower_expr(inner);
            let ptr = self.operand_to_value(val);
            let real = self.load_complex_real(ptr, &inner_ctype);
            // May need to cast the real part to the target type
            let comp_ty = Self::complex_component_ir_type(&inner_ctype);
            let to_ty = self.type_spec_to_ir(target_type);
            if comp_ty != to_ty {
                let dest = self.emit_cast_val(real, comp_ty, to_ty);
                return Operand::Value(dest);
            }
            return real;
        }

        let src = self.lower_expr(inner);
        let mut from_ty = self.get_expr_type(inner);
        let to_ty = self.type_spec_to_ir(target_type);

        // Correct from_ty for array/struct identifiers (which decay to pointers)
        if let Expr::Identifier(name, _) = inner {
            if let Some(vi) = self.lookup_var_info(name) {
                if vi.is_array || vi.is_struct {
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
        let dest = self.emit_cast_val(src, from_ty, to_ty);
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
                    // Zero-init the struct first, then fill designated/sequential fields
                    self.zero_init_alloca(alloca, layout.size);
                    self.emit_struct_init(items, alloca, layout, 0);
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
        // Check for VLA runtime sizeof first
        if let Some(vla_val) = self.get_vla_sizeof(arg) {
            return Operand::Value(vla_val);
        }
        let size = match arg {
            SizeofArg::Type(ts) => self.sizeof_type(ts),
            SizeofArg::Expr(expr) => self.sizeof_expr(expr),
        };
        Operand::Const(IrConst::I64(size as i64))
    }

    /// Check if sizeof argument refers to a VLA variable and return its runtime size.
    fn get_vla_sizeof(&self, arg: &SizeofArg) -> Option<Value> {
        match arg {
            SizeofArg::Expr(expr) => {
                // sizeof(identifier) where identifier is a VLA local
                if let Expr::Identifier(name, _) = expr {
                    if let Some(info) = self.locals.get(name) {
                        return info.vla_size;
                    }
                }
                None
            }
            SizeofArg::Type(_) => None,
        }
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
                        self.zero_init_alloca(alloca, layout.size);
                        self.emit_struct_init(items, alloca, layout, 0);
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
        // Dereferencing a pointer-to-complex: return the address (like struct deref).
        // Complex values are aggregates passed by pointer, so *ptr just yields the address.
        if let Some(ctype) = self.get_expr_ctype(inner) {
            if let CType::Pointer(ref pointee) = ctype {
                if matches!(pointee.as_ref(), CType::Array(_, _) | CType::Struct(_) | CType::Union(_)) {
                    return self.lower_expr(inner);
                }
                if pointee.is_complex() {
                    return self.lower_expr(inner);
                }
            }
        }

        // Fallback: use expr_ctype which handles more expression forms (e.g. when
        // get_expr_ctype returns None because c_type wasn't set in LocalInfo).
        // This ensures complex/struct/array types accessed via pointer deref are
        // correctly treated as aggregates (address-only, no scalar load).
        {
            let inner_ct = self.expr_ctype(inner);
            if let CType::Pointer(ref pointee) = inner_ct {
                if matches!(pointee.as_ref(), CType::Array(_, _) | CType::Struct(_) | CType::Union(_)) {
                    return self.lower_expr(inner);
                }
                if pointee.is_complex() {
                    return self.lower_expr(inner);
                }
            }
            // Dereferencing an array that decays to pointer: if element is complex,
            // struct, or union, return the address (no load needed).
            if let CType::Array(ref elem, _) = inner_ct {
                if elem.is_complex() || matches!(elem.as_ref(), CType::Struct(_) | CType::Union(_)) {
                    // Array decays to pointer; *arr == arr[0], which is the first element address
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
        if self.struct_value_size(expr).is_some() {
            let addr = self.compute_array_element_addr(base, index);
            return Operand::Value(addr);
        }
        // Complex array elements: return address (like structs), since complex values
        // are multi-byte aggregates accessed via pointer
        {
            let elem_ct = self.expr_ctype(expr);
            if elem_ct.is_complex() {
                let addr = self.compute_array_element_addr(base, index);
                return Operand::Value(addr);
            }
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
        // Resolve field metadata and CType in a single call to avoid redundant layout lookups
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

        // Array, struct, and complex fields return address (don't load).
        // Complex types are stored as {real, imag} pairs in memory and their
        // IR type is Ptr (pointer to the pair). Returning the field address
        // avoids a double-dereference bug where the real part bits would be
        // misinterpreted as a pointer.
        let is_addr_type = match &field_ctype {
            Some(ct) => matches!(ct,
                CType::Array(_, _) | CType::Struct(_) | CType::Union(_) |
                CType::ComplexFloat | CType::ComplexDouble | CType::ComplexLongDouble),
            None => {
                // Fallback: use the old per-check resolution (shouldn't normally happen)
                self.field_is_array(base_expr, field_name, is_pointer)
                    || self.field_is_struct(base_expr, field_name, is_pointer)
                    || self.field_is_complex(base_expr, field_name, is_pointer)
            }
        };
        if is_addr_type {
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
        // When bitfield is the full 64-bit storage unit, just return the loaded value
        if bit_width >= 64 && bit_offset == 0 {
            return Operand::Value(loaded);
        }

        if storage_ty.is_signed() {
            // Sign-extend: shl by (64 - bit_offset - bit_width), then ashr by (64 - bit_width)
            let shl_amount = 64 - bit_offset - bit_width;
            let ashr_amount = 64 - bit_width;
            let mut val = Operand::Value(loaded);
            if shl_amount > 0 {
                let shifted = self.emit_binop_val(IrBinOp::Shl, val, Operand::Const(IrConst::I64(shl_amount as i64)), IrType::I64);
                val = Operand::Value(shifted);
            }
            if ashr_amount > 0 {
                let result = self.emit_binop_val(IrBinOp::AShr, val, Operand::Const(IrConst::I64(ashr_amount as i64)), IrType::I64);
                Operand::Value(result)
            } else {
                val
            }
        } else {
            // Unsigned: logical shift right + mask
            let mut val = Operand::Value(loaded);
            if bit_offset > 0 {
                let shifted = self.emit_binop_val(IrBinOp::LShr, val, Operand::Const(IrConst::I64(bit_offset as i64)), IrType::I64);
                val = Operand::Value(shifted);
            }
            if bit_width >= 64 {
                val
            } else {
                let mask = (1u64 << bit_width) - 1;
                let masked = self.emit_binop_val(IrBinOp::And, val, Operand::Const(IrConst::I64(mask as i64)), IrType::I64);
                Operand::Value(masked)
            }
        }
    }

    /// Check if a struct field is an array type (for array-to-pointer decay).
    fn field_is_array(&self, base_expr: &Expr, field_name: &str, is_pointer_access: bool) -> bool {
        self.resolve_field_ctype(base_expr, field_name, is_pointer_access)
            .map(|ct| matches!(ct, CType::Array(_, _))).unwrap_or(false)
    }

    /// Check if a struct field is a struct/union type (returns address, not loaded value).
    fn field_is_struct(&self, base_expr: &Expr, field_name: &str, is_pointer_access: bool) -> bool {
        self.resolve_field_ctype(base_expr, field_name, is_pointer_access)
            .map(|ct| matches!(ct, CType::Struct(_) | CType::Union(_))).unwrap_or(false)
    }

    /// Check if a struct field is a complex type (returns address, not loaded value).
    /// Complex types are stored as {real, imag} pairs and use Ptr IR type.
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
        let rhs_bool = self.emit_cmp_val(IrCmpOp::Ne, rhs_val, Operand::Const(IrConst::I64(0)), IrType::I64);
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
            let ir_op = if is_inc { IrBinOp::Add } else { IrBinOp::Sub };
            let result = self.emit_binop_val(ir_op, Operand::Value(loaded_val), step, binop_ty);
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
        let result = self.emit_binop_val(ir_op, current_val.clone(), Operand::Const(IrConst::I64(1)), IrType::I64);

        // Store back via read-modify-write
        self.store_bitfield(field_addr, storage_ty, bit_offset, bit_width, Operand::Value(result));

        // Return value truncated to bit_width, sign-extended for signed bitfields
        let ret_val = if return_new { Operand::Value(result) } else { current_val };
        Some(self.truncate_to_bitfield_value(ret_val, bit_width, storage_ty.is_signed()))
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
        // Complex compound assignment (z += w, z -= w, z *= w, z /= w)
        let lhs_ct = self.expr_ctype(lhs);
        if lhs_ct.is_complex() && matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div) {
            let rhs_ct = self.expr_ctype(rhs);
            let result_ct = self.common_complex_type(&lhs_ct, &rhs_ct);

            // Get LHS pointer (the complex variable address)
            let lhs_ptr = self.lower_complex_lvalue(lhs);

            // If result type differs from LHS type, convert LHS value to result type
            let op_lhs_ptr = if result_ct != lhs_ct {
                let lhs_converted = self.convert_to_complex(
                    Operand::Value(lhs_ptr), &lhs_ct, &result_ct
                );
                self.operand_to_value(lhs_converted)
            } else {
                lhs_ptr
            };

            // Lower and convert RHS to complex
            let rhs_val = self.lower_expr(rhs);
            let rhs_complex = self.convert_to_complex(rhs_val, &rhs_ct, &result_ct);
            let rhs_ptr = self.operand_to_value(rhs_complex);

            // Perform the operation in result_ct
            let result = match op {
                BinOp::Add => self.lower_complex_add(op_lhs_ptr, rhs_ptr, &result_ct),
                BinOp::Sub => self.lower_complex_sub(op_lhs_ptr, rhs_ptr, &result_ct),
                BinOp::Mul => self.lower_complex_mul(op_lhs_ptr, rhs_ptr, &result_ct),
                BinOp::Div => self.lower_complex_div(op_lhs_ptr, rhs_ptr, &result_ct),
                _ => unreachable!(),
            };

            // If result type differs from LHS type, convert back to LHS type before storing
            let store_ptr = if result_ct != lhs_ct {
                let result_ptr = self.operand_to_value(result);
                let converted_back = self.convert_to_complex(
                    Operand::Value(result_ptr), &result_ct, &lhs_ct
                );
                self.operand_to_value(converted_back)
            } else {
                self.operand_to_value(result)
            };
            let comp_size = Self::complex_component_size(&lhs_ct);
            self.emit(Instruction::Memcpy {
                dest: lhs_ptr,
                src: store_ptr,
                size: comp_size * 2,
            });

            return Operand::Value(lhs_ptr);
        }

        // When LHS is non-complex but RHS is complex, extract real part from RHS
        // and perform scalar compound assignment. E.g.: short x -= complex_y;
        let rhs_ct = self.expr_ctype(rhs);
        if !lhs_ct.is_complex() && rhs_ct.is_complex() {
            // Extract real part from RHS
            let rhs_val = self.lower_expr(rhs);
            let rhs_ptr = self.operand_to_value(rhs_val);
            let real_part = self.load_complex_real(rhs_ptr, &rhs_ct);
            let real_ty = Self::complex_component_ir_type(&rhs_ct);

            let ty = self.get_expr_type(lhs);
            // Promote real part to the common type for the operation
            let common_ty = if ty.is_float() || real_ty.is_float() {
                if ty == IrType::F128 || real_ty == IrType::F128 { IrType::F128 }
                else if ty == IrType::F64 || real_ty == IrType::F64 { IrType::F64 }
                else { IrType::F32 }
            } else {
                IrType::I64
            };
            let op_ty = if common_ty.is_float() { common_ty } else { IrType::I64 };

            // Convert real part to operation type
            let rhs_promoted = self.emit_implicit_cast(real_part, real_ty, op_ty);

            if let Some(lv) = self.lower_lvalue(lhs) {
                let loaded = self.load_lvalue_typed(&lv, ty);
                let loaded_promoted = self.emit_implicit_cast(loaded, ty, op_ty);
                let is_unsigned = self.infer_expr_type(lhs).is_unsigned();
                let ir_op = Self::compound_assign_to_ir(op, is_unsigned);
                let result = self.emit_binop_val(ir_op, loaded_promoted, rhs_promoted, op_ty);
                // Convert result back to LHS type
                let result_cast = self.emit_implicit_cast(Operand::Value(result), op_ty, ty);
                self.store_lvalue_typed(&lv, result_cast.clone(), ty);
                return result_cast;
            }
            return Operand::Const(IrConst::I64(0));
        }

        // Check for bitfield compound assignment
        if let Some(result) = self.try_lower_bitfield_compound_assign(op, lhs, rhs) {
            return result;
        }

        let ty = self.get_expr_type(lhs);
        let lhs_ir_ty = self.infer_expr_type(lhs);
        let rhs_ir_ty = self.infer_expr_type(rhs);
        let rhs_ty = self.get_expr_type(rhs);

        // Compute the common type for the operation using usual arithmetic conversions.
        // This determines both the signedness of the operation and the type conversions needed.
        let common_ty = if ty.is_float() || rhs_ty.is_float() {
            if ty == IrType::F128 || rhs_ty == IrType::F128 { IrType::F128 }
            else if ty == IrType::F64 || rhs_ty == IrType::F64 { IrType::F64 }
            else { IrType::F32 }
        } else {
            Self::common_type(
                Self::integer_promote(lhs_ir_ty),
                Self::integer_promote(rhs_ir_ty),
            )
        };

        // The operation type is the common type widened to at least 64 bits for integers
        let op_ty = if common_ty.is_float() {
            common_ty
        } else {
            IrType::I64
        };

        let rhs_val = self.lower_expr_with_type(rhs, op_ty);
        if let Some(lv) = self.lower_lvalue(lhs) {
            let loaded = self.load_lvalue_typed(&lv, ty);
            // Cast loaded value to op_ty if needed, respecting the common type's signedness
            let loaded_promoted = if ty != op_ty && op_ty.is_float() && !ty.is_float() {
                // int -> float promotion: use actual LHS IR type so codegen knows if unsigned
                let cast_from = if lhs_ir_ty.size() <= 4 && lhs_ir_ty.is_unsigned() {
                    IrType::U64
                } else if lhs_ir_ty == IrType::U64 {
                    IrType::U64
                } else {
                    IrType::I64
                };
                let dest = self.emit_cast_val(loaded, cast_from, op_ty);
                Operand::Value(dest)
            } else if ty != op_ty && ty.is_float() && op_ty.is_float() {
                // float width promotion (e.g., F32 -> F64)
                let dest = self.emit_cast_val(loaded, ty, op_ty);
                Operand::Value(dest)
            } else if !op_ty.is_float() && lhs_ir_ty.size() < 8 {
                // Integer promotion to 64-bit for the operation.
                let extend_unsigned = if matches!(op, BinOp::Shl | BinOp::Shr) {
                    lhs_ir_ty.is_unsigned()
                } else {
                    common_ty.is_unsigned() || lhs_ir_ty.is_unsigned()
                };
                let from_ty = if extend_unsigned {
                    match lhs_ir_ty {
                        IrType::I32 => IrType::U32,
                        IrType::I16 => IrType::U16,
                        IrType::I8 => IrType::U8,
                        _ => lhs_ir_ty,
                    }
                } else {
                    lhs_ir_ty
                };
                let dest = self.emit_cast_val(loaded, from_ty, IrType::I64);
                Operand::Value(dest)
            } else {
                loaded
            };

            // Use the common type's signedness for the operation,
            // EXCEPT for shift operators where C standard says the result type
            // is the promoted type of the left operand (not the common type).
            let is_unsigned = if matches!(op, BinOp::Shl | BinOp::Shr) {
                Self::integer_promote(lhs_ir_ty).is_unsigned()
            } else {
                common_ty.is_unsigned()
            };
            let ir_op = Self::compound_assign_to_ir(op, is_unsigned);

            // Scale RHS for pointer += and -=
            let actual_rhs = if ty == IrType::Ptr && matches!(op, BinOp::Add | BinOp::Sub) {
                let elem_size = self.get_pointer_elem_size_from_expr(lhs);
                self.scale_index(rhs_val, elem_size)
            } else {
                rhs_val
            };

            let result = self.emit_binop_val(ir_op, loaded_promoted, actual_rhs, op_ty);

            // Cast result back to lhs type if needed
            let store_val = if op_ty.is_float() && !ty.is_float() {
                // Float -> int cast
                let cast_to = if lhs_ir_ty == IrType::U64 || (lhs_ir_ty.is_unsigned() && lhs_ir_ty.size() <= 4) {
                    IrType::U64
                } else {
                    IrType::I64
                };
                let dest = self.emit_cast_val(Operand::Value(result), op_ty, cast_to);
                if lhs_ir_ty.size() < 8 {
                    let narrowed = self.emit_cast_val(Operand::Value(dest), cast_to, lhs_ir_ty);
                    Operand::Value(narrowed)
                } else {
                    Operand::Value(dest)
                }
            } else if op_ty.is_float() && ty.is_float() && op_ty != ty {
                // Float narrowing (e.g., F64 -> F32)
                let dest = self.emit_cast_val(Operand::Value(result), op_ty, ty);
                Operand::Value(dest)
            } else if !op_ty.is_float() && (lhs_ir_ty == IrType::I32 || lhs_ir_ty == IrType::U32
                || lhs_ir_ty == IrType::I16 || lhs_ir_ty == IrType::U16
                || lhs_ir_ty == IrType::I8 || lhs_ir_ty == IrType::U8) {
                // Truncate result back to narrower LHS type
                let narrowed = self.emit_cast_val(Operand::Value(result), IrType::I64, lhs_ir_ty);
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

    /// Insert a narrowing cast if the type is sub-64-bit (I32/U32).
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

    /// Infer the IrType of an expression for signedness and width decisions.
    pub(super) fn infer_expr_type(&self, expr: &Expr) -> IrType {
        match expr {
            Expr::Identifier(name, _) => {
                if name == "__func__" || name == "__FUNCTION__" || name == "__PRETTY_FUNCTION__" {
                    return IrType::Ptr;
                }
                if self.enum_constants.contains_key(name) { return IrType::I32; }
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
            Expr::StringLiteral(_, _) | Expr::WideStringLiteral(_, _) => IrType::Ptr,
            Expr::VaArg(_, type_spec, _) => self.resolve_va_arg_type(type_spec),
            _ => IrType::I64,
        }
    }

    /// Determine common type for usual arithmetic conversions.
    pub(super) fn common_type(a: IrType, b: IrType) -> IrType {
        // 128-bit integers have highest rank among integer types
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

        // Float<->int, float<->float, or int<->int with differing type need explicit cast
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
    pub(super) fn emit_bool_normalize(&mut self, val: Operand) -> Operand {
        let dest = self.emit_cmp_val(IrCmpOp::Ne, val, Operand::Const(IrConst::I64(0)), IrType::I64);
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
            TypeSpecifier::Struct(_, _, _, _) | TypeSpecifier::Union(_, _, _, _) => {
                // Structs passed via va_arg: for simplicity, treat as pointer-sized
                // The backend will load the appropriate amount of data
                IrType::I64
            }
            _ => IrType::I64,
        }
    }
}
