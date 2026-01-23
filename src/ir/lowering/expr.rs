//! Expression lowering: converts AST Expr nodes to IR instructions.
//!
//! The main entry point is `lower_expr()`, which dispatches to focused helpers
//! for each expression category. This keeps each function small and testable.

use crate::frontend::parser::ast::*;
use crate::frontend::sema::builtins::{self, BuiltinKind};
use crate::ir::ir::*;
use crate::common::types::{IrType, CType};
use super::lowering::Lowerer;

impl Lowerer {
    /// Lower an expression to an IR operand. This is the main dispatch function.
    pub(super) fn lower_expr(&mut self, expr: &Expr) -> Operand {
        match expr {
            // Literals
            Expr::IntLiteral(val, _) => Operand::Const(IrConst::I64(*val)),
            Expr::UIntLiteral(val, _) => Operand::Const(IrConst::I64(*val as i64)),
            Expr::LongLiteral(val, _) => Operand::Const(IrConst::I64(*val)),
            Expr::ULongLiteral(val, _) => Operand::Const(IrConst::I64(*val as i64)),
            Expr::FloatLiteral(val, _) => Operand::Const(IrConst::F64(*val)),
            Expr::FloatLiteralF32(val, _) => Operand::Const(IrConst::F32(*val as f32)),
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
        // Enum constants are compile-time integer values
        if let Some(&val) = self.enum_constants.get(name) {
            return Operand::Const(IrConst::I64(val));
        }

        // Local variables: arrays/structs decay to address, scalars are loaded
        if let Some(info) = self.locals.get(name).cloned() {
            if info.is_array || info.is_struct {
                return Operand::Value(info.alloca);
            }
            let dest = self.fresh_value();
            self.emit(Instruction::Load { dest, ptr: info.alloca, ty: info.ty });
            return Operand::Value(dest);
        }

        // Global variables
        if let Some(ginfo) = self.globals.get(name).cloned() {
            let addr = self.fresh_value();
            self.emit(Instruction::GlobalAddr { dest: addr, name: name.to_string() });
            if ginfo.is_array {
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
            let lhs_val = self.lower_expr_with_type(lhs, op_ty);
            let rhs_val = self.lower_expr_with_type(rhs, op_ty);
            (lhs_val, rhs_val)
        };
        let dest = self.fresh_value();

        // Emit comparison or arithmetic instruction
        match op {
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                let cmp_op = Self::binop_to_cmp(*op, is_unsigned);
                self.emit(Instruction::Cmp { dest, op: cmp_op, lhs: lhs_val, rhs: rhs_val, ty: op_ty });
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
            UnaryOp::Plus => self.lower_expr(inner),
            UnaryOp::Neg => {
                let ty = self.get_expr_type(inner);
                let inner_ty = self.infer_expr_type(inner);
                let neg_ty = if ty.is_float() { ty } else { IrType::I64 };
                let val = self.lower_expr(inner);
                let dest = self.fresh_value();
                self.emit(Instruction::UnaryOp { dest, op: IrUnaryOp::Neg, src: val, ty: neg_ty });
                if !neg_ty.is_float() {
                    self.maybe_narrow(dest, inner_ty)
                } else {
                    Operand::Value(dest)
                }
            }
            UnaryOp::BitNot => {
                let inner_ty = self.infer_expr_type(inner);
                let val = self.lower_expr(inner);
                let dest = self.fresh_value();
                self.emit(Instruction::UnaryOp { dest, op: IrUnaryOp::Not, src: val, ty: IrType::I64 });
                self.maybe_narrow(dest, inner_ty)
            }
            UnaryOp::LogicalNot => {
                let val = self.lower_expr(inner);
                let dest = self.fresh_value();
                self.emit(Instruction::Cmp {
                    dest, op: IrCmpOp::Eq,
                    lhs: val, rhs: Operand::Const(IrConst::I64(0)),
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

    /// Lower struct/union assignment using memcpy.
    fn lower_struct_assign(&mut self, lhs: &Expr, rhs: &Expr) -> Operand {
        let rhs_val = self.lower_expr(rhs);
        let struct_size = self.get_struct_size_for_expr(lhs);

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

        // Lower arguments with implicit casts
        let (arg_vals, arg_types) = self.lower_call_arguments(func, args);
        let dest = self.fresh_value();

        // Determine variadic status
        let call_variadic = if let Expr::Identifier(name, _) = func {
            self.is_function_variadic(name)
        } else {
            false
        };

        // Dispatch: direct call, function pointer call, or indirect call
        let call_ret_ty = self.emit_call_instruction(func, dest, arg_vals, arg_types, call_variadic);

        // Narrow the result if the return type is sub-64-bit integer
        self.maybe_narrow_call_result(dest, call_ret_ty)
    }

    /// Try to lower a __builtin_* call. Returns Some(result) if handled.
    fn try_lower_builtin_call(&mut self, name: &str, args: &[Expr]) -> Option<Operand> {
        let builtin_info = builtins::resolve_builtin(name)?;
        match &builtin_info.kind {
            BuiltinKind::LibcAlias(libc_name) => {
                let arg_types: Vec<IrType> = args.iter().map(|a| self.get_expr_type(a)).collect();
                let arg_vals: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                let dest = self.fresh_value();
                let variadic = self.function_variadic.contains(libc_name.as_str());
                self.emit(Instruction::Call {
                    dest: Some(dest), func: libc_name.clone(),
                    args: arg_vals, arg_types, return_type: IrType::I64, is_variadic: variadic,
                });
                Some(Operand::Value(dest))
            }
            BuiltinKind::Identity => {
                Some(args.first().map_or(Operand::Const(IrConst::I64(0)), |a| self.lower_expr(a)))
            }
            BuiltinKind::ConstantI64(val) => Some(Operand::Const(IrConst::I64(*val))),
            BuiltinKind::ConstantF64(_) => Some(Operand::Const(IrConst::I64(0))), // TODO: handle float constants
            BuiltinKind::Intrinsic(_) => {
                let cleaned_name = name.strip_prefix("__builtin_").unwrap_or(name).to_string();
                let arg_types: Vec<IrType> = args.iter().map(|a| self.get_expr_type(a)).collect();
                let arg_vals: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                let dest = self.fresh_value();
                let variadic = self.function_variadic.contains(cleaned_name.as_str());
                self.emit(Instruction::Call {
                    dest: Some(dest), func: cleaned_name,
                    args: arg_vals, arg_types, return_type: IrType::I64, is_variadic: variadic,
                });
                Some(Operand::Value(dest))
            }
        }
    }

    /// Lower function call arguments, applying implicit casts for parameter types
    /// and default argument promotions for variadic args.
    fn lower_call_arguments(&mut self, func: &Expr, args: &[Expr]) -> (Vec<Operand>, Vec<IrType>) {
        let param_types: Option<Vec<IrType>> = if let Expr::Identifier(name, _) = func {
            self.function_param_types.get(name).cloned()
                .or_else(|| self.function_ptr_param_types.get(name).cloned())
        } else {
            None
        };

        let mut arg_types = Vec::with_capacity(args.len());
        let arg_vals: Vec<Operand> = args.iter().enumerate().map(|(i, a)| {
            let val = self.lower_expr(a);
            let arg_ty = self.get_expr_type(a);

            // Cast to declared parameter type if known
            if let Some(ref ptypes) = param_types {
                if i < ptypes.len() {
                    let param_ty = ptypes[i];
                    arg_types.push(param_ty);
                    return self.emit_implicit_cast(val, arg_ty, param_ty);
                }
            }
            // Default promotion: float -> double for variadic args
            if arg_ty == IrType::F32 {
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
    ) -> IrType {
        match func {
            Expr::Identifier(name, _) => {
                let is_local_fptr = self.locals.contains_key(name)
                    && !self.known_functions.contains(name);
                let is_global_fptr = !self.locals.contains_key(name)
                    && self.globals.contains_key(name)
                    && !self.known_functions.contains(name);

                if is_local_fptr {
                    let info = self.locals.get(name).unwrap().clone();
                    let ptr_val = self.fresh_value();
                    self.emit(Instruction::Load { dest: ptr_val, ptr: info.alloca, ty: IrType::Ptr });
                    let ret_ty = self.function_ptr_return_types.get(name).copied().unwrap_or(IrType::I64);
                    self.emit(Instruction::CallIndirect {
                        dest: Some(dest), func_ptr: Operand::Value(ptr_val),
                        args: arg_vals, arg_types, return_type: ret_ty, is_variadic,
                    });
                    ret_ty
                } else if is_global_fptr {
                    let addr = self.fresh_value();
                    self.emit(Instruction::GlobalAddr { dest: addr, name: name.clone() });
                    let ptr_val = self.fresh_value();
                    self.emit(Instruction::Load { dest: ptr_val, ptr: addr, ty: IrType::Ptr });
                    let ret_ty = self.function_ptr_return_types.get(name).copied().unwrap_or(IrType::I64);
                    self.emit(Instruction::CallIndirect {
                        dest: Some(dest), func_ptr: Operand::Value(ptr_val),
                        args: arg_vals, arg_types, return_type: ret_ty, is_variadic,
                    });
                    ret_ty
                } else {
                    // Direct call - look up return type
                    let ret_ty = self.function_return_types.get(name).copied().unwrap_or(IrType::I64);
                    self.emit(Instruction::Call {
                        dest: Some(dest), func: name.clone(),
                        args: arg_vals, arg_types, return_type: ret_ty, is_variadic,
                    });
                    ret_ty
                }
            }
            Expr::Deref(inner, _) => {
                // (*func_ptr)(args...) - dereference is a no-op for function pointers
                let func_ptr = self.lower_expr(inner);
                self.emit(Instruction::CallIndirect {
                    dest: Some(dest), func_ptr, args: arg_vals, arg_types,
                    return_type: IrType::I64, is_variadic: false,
                });
                IrType::I64
            }
            _ => {
                // General expression as callee (e.g., array[i](...), struct.fptr(...))
                let func_ptr = self.lower_expr(func);
                self.emit(Instruction::CallIndirect {
                    dest: Some(dest), func_ptr, args: arg_vals, arg_types,
                    return_type: IrType::I64, is_variadic: false,
                });
                IrType::I64
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
        let cond_val = self.lower_expr(cond);
        let result_alloca = self.fresh_value();
        self.emit(Instruction::Alloca { dest: result_alloca, ty: IrType::I64, size: 8 });

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
        self.emit(Instruction::Store { val: then_val, ptr: result_alloca, ty: IrType::I64 });
        self.terminate(Terminator::Branch(end_label.clone()));

        self.start_block(else_label);
        let else_val = self.lower_expr(else_expr);
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
        let size = self.sizeof_type(type_spec);
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
            let field_idx = if let Some(Designator::Field(ref name)) = item.designators.first() {
                layout.fields.iter().position(|f| f.name == *name).unwrap_or(current_field_idx)
            } else {
                // Skip unnamed fields (anonymous bitfields) for positional init
                let mut idx = current_field_idx;
                while idx < layout.fields.len() && layout.fields[idx].name.is_empty() {
                    idx += 1;
                }
                idx
            };

            if field_idx >= layout.fields.len() { break; }
            let field = &layout.fields[field_idx].clone();
            let field_ty = self.ir_type_for_elem_size(field.ty.size());

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
                        let val = self.lower_expr(expr);
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

    fn lower_member_access(&mut self, base_expr: &Expr, field_name: &str) -> Operand {
        let (field_offset, field_ty) = self.resolve_member_access(base_expr, field_name);
        let base_addr = self.get_struct_base_addr(base_expr);
        let field_addr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: field_addr, base: base_addr,
            offset: Operand::Const(IrConst::I64(field_offset as i64)),
            ty: field_ty,
        });

        // Array and struct fields return address (don't load)
        if self.field_is_array(base_expr, field_name, false)
            || self.field_is_struct(base_expr, field_name, false)
        {
            return Operand::Value(field_addr);
        }
        let dest = self.fresh_value();
        self.emit(Instruction::Load { dest, ptr: field_addr, ty: field_ty });
        Operand::Value(dest)
    }

    fn lower_pointer_member_access(&mut self, base_expr: &Expr, field_name: &str) -> Operand {
        let ptr_val = self.lower_expr(base_expr);
        let base_addr = self.operand_to_value(ptr_val);
        let (field_offset, field_ty) = self.resolve_pointer_member_access(base_expr, field_name);
        let field_addr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: field_addr, base: base_addr,
            offset: Operand::Const(IrConst::I64(field_offset as i64)),
            ty: field_ty,
        });

        // Array and struct fields return address (don't load)
        if self.field_is_array(base_expr, field_name, true)
            || self.field_is_struct(base_expr, field_name, true)
        {
            return Operand::Value(field_addr);
        }
        let dest = self.fresh_value();
        self.emit(Instruction::Load { dest, ptr: field_addr, ty: field_ty });
        Operand::Value(dest)
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

        let lhs_val = self.lower_expr(lhs);

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

        // RHS evaluation
        self.start_block(rhs_label);
        let rhs_val = self.lower_expr(rhs);
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
        let ty = self.get_expr_type(inner);
        if let Some(lv) = self.lower_lvalue(inner) {
            let loaded_val = self.load_lvalue_as_value(&lv, ty);
            let (step, binop_ty) = self.inc_dec_step_and_type(ty, inner);
            let result = self.fresh_value();
            let ir_op = if op == UnaryOp::PreInc { IrBinOp::Add } else { IrBinOp::Sub };
            self.emit(Instruction::BinOp {
                dest: result, op: ir_op,
                lhs: Operand::Value(loaded_val), rhs: step, ty: binop_ty,
            });
            self.store_lvalue_typed(&lv, Operand::Value(result), ty);
            return Operand::Value(result);
        }
        self.lower_expr(inner)
    }

    pub(super) fn lower_post_inc_dec(&mut self, inner: &Expr, op: PostfixOp) -> Operand {
        let ty = self.get_expr_type(inner);
        if let Some(lv) = self.lower_lvalue(inner) {
            let loaded = self.load_lvalue_typed(&lv, ty);
            let loaded_val = self.operand_to_value(loaded.clone());
            let (step, binop_ty) = self.inc_dec_step_and_type(ty, inner);
            let result = self.fresh_value();
            let ir_op = if op == PostfixOp::PostInc { IrBinOp::Add } else { IrBinOp::Sub };
            self.emit(Instruction::BinOp {
                dest: result, op: ir_op,
                lhs: Operand::Value(loaded_val), rhs: step, ty: binop_ty,
            });
            self.store_lvalue_typed(&lv, Operand::Value(result), ty);
            return loaded; // Return original value
        }
        self.lower_expr(inner)
    }

    /// Get the step value and operation type for increment/decrement.
    /// For pointers: step = elem_size (I64), type = I64
    /// For floats: step = 1.0, type = F64/F32
    /// For integers: step = 1 (I64), type = I64
    fn inc_dec_step_and_type(&self, ty: IrType, expr: &Expr) -> (Operand, IrType) {
        if ty == IrType::Ptr {
            let elem_size = self.get_pointer_elem_size(expr);
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
            } else {
                loaded
            };

            // For compound assignment, use signedness of the LHS type
            let is_unsigned = lhs_ir_ty.is_unsigned();
            let ir_op = Self::compound_assign_to_ir(op, is_unsigned);

            // Scale RHS for pointer += and -=
            let actual_rhs = if ty == IrType::Ptr && matches!(op, BinOp::Add | BinOp::Sub) {
                let elem_size = self.get_pointer_elem_size(lhs);
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
            self.store_lvalue_typed(&lv, store_val.clone(), ty);
            return store_val;
        }
        rhs_val
    }

    fn compound_assign_to_ir(op: &BinOp, is_unsigned: bool) -> IrBinOp {
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
            _ => IrBinOp::Add,
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
            Expr::UnaryOp(_, inner, _) => self.infer_expr_type(inner),
            Expr::FunctionCall(func, _, _) => {
                if let Expr::Identifier(name, _) = func.as_ref() {
                    if let Some(&ret_ty) = self.function_return_types.get(name.as_str()) {
                        return ret_ty;
                    }
                }
                IrType::I64
            }
            _ => IrType::I64,
        }
    }

    /// Determine common type for usual arithmetic conversions.
    fn common_type(a: IrType, b: IrType) -> IrType {
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
        if self.function_variadic.contains(name) { return true; }
        if self.function_param_types.contains_key(name) { return false; }
        // Fallback: common variadic libc functions
        matches!(name, "printf" | "fprintf" | "sprintf" | "snprintf" | "scanf" | "sscanf"
            | "fscanf" | "dprintf" | "vprintf" | "vfprintf" | "vsprintf" | "vsnprintf"
            | "syslog" | "err" | "errx" | "warn" | "warnx" | "asprintf" | "vasprintf"
            | "open" | "fcntl" | "ioctl" | "execl" | "execlp" | "execle")
    }
}
