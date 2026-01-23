use crate::frontend::parser::ast::*;
use crate::frontend::sema::builtins::{self, BuiltinKind};
use crate::ir::ir::*;
use crate::common::types::{IrType, CType};
use super::lowering::Lowerer;

impl Lowerer {
    pub(super) fn lower_expr(&mut self, expr: &Expr) -> Operand {
        match expr {
            Expr::IntLiteral(val, _) => {
                Operand::Const(IrConst::I64(*val))
            }
            Expr::UIntLiteral(val, _) => {
                Operand::Const(IrConst::I64(*val as i64))
            }
            Expr::LongLiteral(val, _) => {
                Operand::Const(IrConst::I64(*val))
            }
            Expr::ULongLiteral(val, _) => {
                Operand::Const(IrConst::I64(*val as i64))
            }
            Expr::FloatLiteral(val, _) => {
                Operand::Const(IrConst::F64(*val))
            }
            Expr::FloatLiteralF32(val, _) => {
                Operand::Const(IrConst::F32(*val as f32))
            }
            Expr::CharLiteral(ch, _) => {
                Operand::Const(IrConst::I32(*ch as i32))
            }
            Expr::StringLiteral(s, _) => {
                let label = format!(".Lstr{}", self.next_string);
                self.next_string += 1;
                self.module.string_literals.push((label.clone(), s.clone()));
                let dest = self.fresh_value();
                self.emit(Instruction::GlobalAddr { dest, name: label });
                Operand::Value(dest)
            }
            Expr::Identifier(name, _) => {
                // Check for enum constants first
                if let Some(&val) = self.enum_constants.get(name) {
                    return Operand::Const(IrConst::I64(val));
                }
                if let Some(info) = self.locals.get(name).cloned() {
                    if info.is_array || info.is_struct {
                        // Arrays and structs decay to address: return the alloca address itself.
                        // For structs, this is the base address used for member access and
                        // for passing by value (caller passes address, callee copies).
                        return Operand::Value(info.alloca);
                    }
                    let dest = self.fresh_value();
                    self.emit(Instruction::Load { dest, ptr: info.alloca, ty: info.ty });
                    Operand::Value(dest)
                } else if let Some(ginfo) = self.globals.get(name).cloned() {
                    // Global variable: get address, then load the value
                    let addr = self.fresh_value();
                    self.emit(Instruction::GlobalAddr { dest: addr, name: name.clone() });
                    if ginfo.is_array {
                        // Arrays decay to pointer: address IS the pointer
                        Operand::Value(addr)
                    } else {
                        let dest = self.fresh_value();
                        self.emit(Instruction::Load { dest, ptr: addr, ty: ginfo.ty });
                        Operand::Value(dest)
                    }
                } else {
                    // Assume it's a function reference (or unknown global)
                    let dest = self.fresh_value();
                    self.emit(Instruction::GlobalAddr { dest, name: name.clone() });
                    Operand::Value(dest)
                }
            }
            Expr::BinaryOp(op, lhs, rhs, _) => {
                match op {
                    BinOp::LogicalAnd => {
                        return self.lower_short_circuit(lhs, rhs, true);
                    }
                    BinOp::LogicalOr => {
                        return self.lower_short_circuit(lhs, rhs, false);
                    }
                    _ => {}
                }

                // Check for pointer arithmetic: ptr + int or int + ptr or ptr - int
                if matches!(op, BinOp::Add | BinOp::Sub) {
                    let lhs_is_ptr = self.expr_is_pointer(lhs);
                    let rhs_is_ptr = self.expr_is_pointer(rhs);

                    if lhs_is_ptr && !rhs_is_ptr {
                        // ptr + int or ptr - int: scale RHS by element size
                        let elem_size = self.get_pointer_elem_size_from_expr(lhs);
                        let lhs_val = self.lower_expr(lhs);
                        let rhs_val = self.lower_expr(rhs);
                        let scaled_rhs = if elem_size > 1 {
                            let scale = Operand::Const(IrConst::I64(elem_size as i64));
                            let scaled = self.fresh_value();
                            self.emit(Instruction::BinOp {
                                dest: scaled,
                                op: IrBinOp::Mul,
                                lhs: rhs_val,
                                rhs: scale,
                                ty: IrType::I64,
                            });
                            Operand::Value(scaled)
                        } else {
                            rhs_val
                        };
                        let dest = self.fresh_value();
                        let ir_op = if *op == BinOp::Add { IrBinOp::Add } else { IrBinOp::Sub };
                        self.emit(Instruction::BinOp { dest, op: ir_op, lhs: lhs_val, rhs: scaled_rhs, ty: IrType::I64 });
                        return Operand::Value(dest);
                    } else if rhs_is_ptr && !lhs_is_ptr && *op == BinOp::Add {
                        // int + ptr: scale LHS by element size
                        let elem_size = self.get_pointer_elem_size_from_expr(rhs);
                        let lhs_val = self.lower_expr(lhs);
                        let rhs_val = self.lower_expr(rhs);
                        let scaled_lhs = if elem_size > 1 {
                            let scale = Operand::Const(IrConst::I64(elem_size as i64));
                            let scaled = self.fresh_value();
                            self.emit(Instruction::BinOp {
                                dest: scaled,
                                op: IrBinOp::Mul,
                                lhs: lhs_val,
                                rhs: scale,
                                ty: IrType::I64,
                            });
                            Operand::Value(scaled)
                        } else {
                            lhs_val
                        };
                        let dest = self.fresh_value();
                        self.emit(Instruction::BinOp { dest, op: IrBinOp::Add, lhs: scaled_lhs, rhs: rhs_val, ty: IrType::I64 });
                        return Operand::Value(dest);
                    } else if lhs_is_ptr && rhs_is_ptr && *op == BinOp::Sub {
                        // ptr - ptr: result is byte difference / element size
                        let elem_size = self.get_pointer_elem_size_from_expr(lhs);
                        let lhs_val = self.lower_expr(lhs);
                        let rhs_val = self.lower_expr(rhs);
                        let diff = self.fresh_value();
                        self.emit(Instruction::BinOp { dest: diff, op: IrBinOp::Sub, lhs: lhs_val, rhs: rhs_val, ty: IrType::I64 });
                        if elem_size > 1 {
                            let scale = Operand::Const(IrConst::I64(elem_size as i64));
                            let dest = self.fresh_value();
                            self.emit(Instruction::BinOp { dest, op: IrBinOp::SDiv, lhs: Operand::Value(diff), rhs: scale, ty: IrType::I64 });
                            return Operand::Value(dest);
                        }
                        return Operand::Value(diff);
                    }
                }

                // Determine type: float promotion first, then signed/unsigned
                let lhs_ty = self.infer_expr_type(lhs);
                let rhs_ty = self.infer_expr_type(rhs);
                let lhs_expr_ty = self.get_expr_type(lhs);
                let rhs_expr_ty = self.get_expr_type(rhs);
                let (op_ty, is_unsigned, common_ty) = if lhs_expr_ty.is_float() || rhs_expr_ty.is_float() {
                    let ft = if lhs_expr_ty == IrType::F64 || rhs_expr_ty == IrType::F64 { IrType::F64 } else { IrType::F32 };
                    (ft, false, ft)
                } else {
                    let ct = Self::common_type(lhs_ty, rhs_ty);
                    (IrType::I64, ct.is_unsigned(), ct)
                };

                let lhs_val = self.lower_expr_with_type(lhs, op_ty);
                let rhs_val = self.lower_expr_with_type(rhs, op_ty);
                let dest = self.fresh_value();

                match op {
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                        let cmp_op = match (op, is_unsigned) {
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
                        };
                        self.emit(Instruction::Cmp { dest, op: cmp_op, lhs: lhs_val, rhs: rhs_val, ty: op_ty });
                    }
                    _ => {
                        let ir_op = match (op, is_unsigned) {
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
                        };
                        self.emit(Instruction::BinOp { dest, op: ir_op, lhs: lhs_val, rhs: rhs_val, ty: op_ty });
                    }
                }

                // For sub-64-bit types (U32, I32, etc.), insert a truncation cast
                // to ensure the result fits in the narrower type.
                // This handles unsigned int wraparound and signed int overflow behavior.
                if common_ty == IrType::U32 || common_ty == IrType::I32 {
                    match op {
                        // Comparisons always produce 0 or 1, no truncation needed
                        BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le |
                        BinOp::Gt | BinOp::Ge | BinOp::LogicalAnd | BinOp::LogicalOr => {
                            Operand::Value(dest)
                        }
                        _ => {
                            let narrowed = self.fresh_value();
                            self.emit(Instruction::Cast {
                                dest: narrowed,
                                src: Operand::Value(dest),
                                from_ty: IrType::I64,
                                to_ty: common_ty,
                            });
                            Operand::Value(narrowed)
                        }
                    }
                } else {
                    Operand::Value(dest)
                }
            }
            Expr::UnaryOp(op, inner, _) => {
                match op {
                    UnaryOp::Plus => {
                        // Unary plus is a no-op (identity), just lower the inner expression
                        self.lower_expr(inner)
                    }
                    UnaryOp::Neg => {
                        let ty = self.get_expr_type(inner);
                        let inner_ty = self.infer_expr_type(inner);
                        let neg_ty = if ty.is_float() { ty } else { IrType::I64 };
                        let val = self.lower_expr(inner);
                        let dest = self.fresh_value();
                        self.emit(Instruction::UnaryOp { dest, op: IrUnaryOp::Neg, src: val, ty: neg_ty });
                        // Truncate to sub-64-bit type if needed (not for float)
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
                        // Truncate to sub-64-bit type if needed (e.g., ~uint must be 32-bit)
                        self.maybe_narrow(dest, inner_ty)
                    }
                    UnaryOp::LogicalNot => {
                        let val = self.lower_expr(inner);
                        let dest = self.fresh_value();
                        self.emit(Instruction::Cmp { dest, op: IrCmpOp::Eq, lhs: val, rhs: Operand::Const(IrConst::I64(0)), ty: IrType::I64 });
                        Operand::Value(dest)
                    }
                    UnaryOp::PreInc | UnaryOp::PreDec => {
                        self.lower_pre_inc_dec(inner, *op)
                    }
                }
            }
            Expr::PostfixOp(op, inner, _) => {
                self.lower_post_inc_dec(inner, *op)
            }
            Expr::Assign(lhs, rhs, _) => {
                // Check if this is a struct/union assignment (needs memcpy)
                let lhs_is_struct = self.expr_is_struct_value(lhs);
                if lhs_is_struct {
                    // Struct assignment: both sides are addresses, use memcpy
                    let rhs_val = self.lower_expr(rhs);
                    let struct_size = self.get_struct_size_for_expr(lhs);
                    if let Some(lv) = self.lower_lvalue(lhs) {
                        let dest_addr = self.lvalue_addr(&lv);
                        let src_addr = match rhs_val {
                            Operand::Value(v) => v,
                            Operand::Const(_) => {
                                let tmp = self.fresh_value();
                                self.emit(Instruction::Copy { dest: tmp, src: rhs_val.clone() });
                                tmp
                            }
                        };
                        self.emit(Instruction::Memcpy {
                            dest: dest_addr,
                            src: src_addr,
                            size: struct_size,
                        });
                        return Operand::Value(dest_addr);
                    }
                    rhs_val
                } else {
                    let rhs_val = self.lower_expr(rhs);
                    let lhs_ty = self.get_expr_type(lhs);
                    let rhs_ty = self.get_expr_type(rhs);
                    // Insert implicit cast for type mismatches (e.g., float = int, int = double)
                    let rhs_val = self.emit_implicit_cast(rhs_val, rhs_ty, lhs_ty);
                    if let Some(lv) = self.lower_lvalue(lhs) {
                        self.store_lvalue_typed(&lv, rhs_val.clone(), lhs_ty);
                        return rhs_val;
                    }
                    // Fallback: just return the rhs value
                    rhs_val
                }
            }
            Expr::CompoundAssign(op, lhs, rhs, _) => {
                self.lower_compound_assign(op, lhs, rhs)
            }
            Expr::FunctionCall(func, args, _) => {
                // First, resolve __builtin_* functions before normal dispatch.
                if let Expr::Identifier(name, _) = func.as_ref() {
                    if let Some(builtin_info) = builtins::resolve_builtin(name) {
                        match &builtin_info.kind {
                            BuiltinKind::LibcAlias(libc_name) => {
                                let builtin_arg_types: Vec<IrType> = args.iter().map(|a| self.get_expr_type(a)).collect();
                                let arg_vals: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                                let dest = self.fresh_value();
                                self.emit(Instruction::Call {
                                    dest: Some(dest),
                                    func: libc_name.clone(),
                                    args: arg_vals,
                                    arg_types: builtin_arg_types,
                                    return_type: IrType::I64,
                                });
                                return Operand::Value(dest);
                            }
                            BuiltinKind::Identity => {
                                // __builtin_expect(x, y) -> just return x
                                if let Some(first_arg) = args.first() {
                                    return self.lower_expr(first_arg);
                                }
                                return Operand::Const(IrConst::I64(0));
                            }
                            BuiltinKind::ConstantI64(val) => {
                                return Operand::Const(IrConst::I64(*val));
                            }
                            BuiltinKind::ConstantF64(_val) => {
                                // TODO: handle float constants properly
                                return Operand::Const(IrConst::I64(0));
                            }
                            BuiltinKind::Intrinsic(_intr) => {
                                // Strip __builtin_ prefix and call the underlying function
                                let cleaned_name = name.strip_prefix("__builtin_").unwrap_or(name).to_string();
                                let intrinsic_arg_types: Vec<IrType> = args.iter().map(|a| self.get_expr_type(a)).collect();
                                let arg_vals: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                                let dest = self.fresh_value();
                                self.emit(Instruction::Call {
                                    dest: Some(dest),
                                    func: cleaned_name,
                                    args: arg_vals,
                                    arg_types: intrinsic_arg_types,
                                    return_type: IrType::I64,
                                });
                                return Operand::Value(dest);
                            }
                        }
                    }
                }

                // Normal call dispatch (direct calls and indirect function pointer calls)
                // Look up parameter types for implicit argument casts
                let param_types: Option<Vec<IrType>> = if let Expr::Identifier(name, _) = func.as_ref() {
                    self.function_param_types.get(name).cloned()
                } else {
                    None
                };

                // Lower arguments with implicit casts to match parameter types
                // Also track the resulting arg types for the backend's calling convention
                let mut computed_arg_types: Vec<IrType> = Vec::new();
                let arg_vals: Vec<Operand> = args.iter().enumerate().map(|(i, a)| {
                    let val = self.lower_expr(a);
                    let arg_ty = self.get_expr_type(a);
                    // If we have parameter type info and this is a non-variadic param,
                    // insert an implicit cast if the types differ
                    if let Some(ref ptypes) = param_types {
                        if i < ptypes.len() {
                            let param_ty = ptypes[i];
                            computed_arg_types.push(param_ty);
                            return self.emit_implicit_cast(val, arg_ty, param_ty);
                        }
                    }
                    // For variadic or unknown params, apply default argument promotions:
                    // float -> double, narrow int types stay as-is (already I64 in our IR)
                    if arg_ty == IrType::F32 {
                        // Promote float to double for variadic args
                        computed_arg_types.push(IrType::F64);
                        return self.emit_implicit_cast(val, IrType::F32, IrType::F64);
                    }
                    computed_arg_types.push(arg_ty);
                    val
                }).collect();
                let dest = self.fresh_value();

                // Look up function return type for narrowing cast after the call.
                // Default to I64 if unknown (e.g., implicitly declared functions).
                let mut call_ret_ty = IrType::I64;

                match func.as_ref() {
                    Expr::Identifier(name, _) => {
                        // Check if this is a local variable (function pointer) rather
                        // than a direct function name.
                        let is_local_fptr = self.locals.contains_key(name)
                            && !self.known_functions.contains(name);
                        let is_global_fptr = !self.locals.contains_key(name)
                            && self.globals.contains_key(name)
                            && !self.known_functions.contains(name);

                        if is_local_fptr {
                            // Load the function pointer from the local variable
                            let info = self.locals.get(name).unwrap().clone();
                            let ptr_val = self.fresh_value();
                            self.emit(Instruction::Load {
                                dest: ptr_val,
                                ptr: info.alloca,
                                ty: IrType::Ptr,
                            });
                            self.emit(Instruction::CallIndirect {
                                dest: Some(dest),
                                func_ptr: Operand::Value(ptr_val),
                                args: arg_vals,
                                arg_types: computed_arg_types.clone(),
                                return_type: IrType::I64,
                            });
                        } else if is_global_fptr {
                            // Load the function pointer from a global variable
                            let addr = self.fresh_value();
                            self.emit(Instruction::GlobalAddr { dest: addr, name: name.clone() });
                            let ptr_val = self.fresh_value();
                            self.emit(Instruction::Load {
                                dest: ptr_val,
                                ptr: addr,
                                ty: IrType::Ptr,
                            });
                            self.emit(Instruction::CallIndirect {
                                dest: Some(dest),
                                func_ptr: Operand::Value(ptr_val),
                                args: arg_vals,
                                arg_types: computed_arg_types.clone(),
                                return_type: IrType::I64,
                            });
                        } else {
                            // Direct function call - look up return type
                            if let Some(&ret_ty) = self.function_return_types.get(name) {
                                call_ret_ty = ret_ty;
                            }
                            self.emit(Instruction::Call {
                                dest: Some(dest),
                                func: name.clone(),
                                args: arg_vals,
                                arg_types: computed_arg_types.clone(),
                                return_type: call_ret_ty,
                            });
                        }
                    }
                    Expr::Deref(inner, _) => {
                        // (*func_ptr)(args...) - dereference is a no-op for function
                        // pointers in C, just evaluate the inner expression to get the pointer
                        let func_ptr = self.lower_expr(inner);
                        self.emit(Instruction::CallIndirect {
                            dest: Some(dest),
                            func_ptr,
                            args: arg_vals,
                            arg_types: computed_arg_types.clone(),
                            return_type: IrType::I64,
                        });
                    }
                    _ => {
                        // General expression as callee (e.g., array[i](...), struct.fptr(...))
                        // Evaluate the expression to get the function pointer value
                        let func_ptr = self.lower_expr(func);
                        self.emit(Instruction::CallIndirect {
                            dest: Some(dest),
                            func_ptr,
                            args: arg_vals,
                            arg_types: computed_arg_types.clone(),
                            return_type: IrType::I64,
                        });
                    }
                }

                // If the function returns a type narrower than I64,
                // insert a narrowing cast to properly truncate/sign-extend the result.
                if call_ret_ty != IrType::I64 && call_ret_ty != IrType::Ptr
                    && call_ret_ty != IrType::Void && call_ret_ty.is_integer()
                {
                    let narrowed = self.fresh_value();
                    self.emit(Instruction::Cast {
                        dest: narrowed,
                        src: Operand::Value(dest),
                        from_ty: IrType::I64,
                        to_ty: call_ret_ty,
                    });
                    Operand::Value(narrowed)
                } else {
                    Operand::Value(dest)
                }
            }
            Expr::Conditional(cond, then_expr, else_expr, _) => {
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
            Expr::Cast(ref target_type, inner, _) => {
                let src = self.lower_expr(inner);
                let mut from_ty = self.get_expr_type(inner);
                let to_ty = self.type_spec_to_ir(target_type);
                // If the inner expression is an array or struct identifier, it decays
                // to a pointer (lower_expr returns the address), but get_expr_type
                // returns the element type. Correct from_ty to reflect the actual
                // runtime value type.
                if let Expr::Identifier(name, _) = inner.as_ref() {
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
                // For pointer casts or same-type casts, just pass through.
                // Casting TO Ptr is always a no-op (reinterpret bits), since lower_expr
                // already produces the correct 64-bit value (address for arrays/structs,
                // loaded value for scalars). Casting FROM Ptr to a 64-bit integer is
                // also a no-op. Ptr <-> I64/U64 are no-ops since they're same size.
                if to_ty == from_ty
                    || to_ty == IrType::Ptr
                    || (from_ty == IrType::Ptr && to_ty.size() == 8) {
                    src
                } else if to_ty.is_float() || from_ty.is_float() {
                    // Float<->int or float<->float cast
                    let dest = self.fresh_value();
                    self.emit(Instruction::Cast {
                        dest,
                        src,
                        from_ty,
                        to_ty,
                    });
                    Operand::Value(dest)
                } else {
                    // Integer truncation/extension cast
                    let dest = self.fresh_value();
                    self.emit(Instruction::Cast {
                        dest,
                        src,
                        from_ty,
                        to_ty,
                    });
                    Operand::Value(dest)
                }
            }
            Expr::CompoundLiteral(ref type_spec, ref init, _) => {
                // Compound literal: (type){initializer}
                // Allocate a local, initialize it, and return its address or value.
                let ty = self.type_spec_to_ir(type_spec);
                let size = self.sizeof_type(type_spec);
                let alloca = self.fresh_value();
                self.emit(Instruction::Alloca { dest: alloca, size, ty });

                // Check if this is a struct compound literal
                let struct_layout = self.get_struct_layout_for_type(type_spec);

                match init.as_ref() {
                    Initializer::Expr(expr) => {
                        let val = self.lower_expr(expr);
                        self.emit(Instruction::Store { val, ptr: alloca, ty });
                    }
                    Initializer::List(items) => {
                        if let Some(layout) = struct_layout {
                            // Struct compound literal with possible designators
                            self.zero_init_alloca(alloca, layout.size);
                            let mut current_field_idx = 0usize;
                            for item in items.iter() {
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
                                let val = match &item.init {
                                    Initializer::Expr(expr) => self.lower_expr(expr),
                                    _ => Operand::Const(IrConst::I64(0)),
                                };
                                let field_addr = self.fresh_value();
                                self.emit(Instruction::GetElementPtr {
                                    dest: field_addr,
                                    base: alloca,
                                    offset: Operand::Const(IrConst::I64(field.offset as i64)),
                                    ty: field_ty,
                                });
                                self.emit(Instruction::Store { val, ptr: field_addr, ty: field_ty });
                                current_field_idx = field_idx + 1;
                            }
                        } else {
                            // Array or scalar compound literal
                            let elem_size = self.compound_literal_elem_size(type_spec);
                            let has_designators = items.iter().any(|item| !item.designators.is_empty());
                            if has_designators {
                                self.zero_init_alloca(alloca, size);
                            }

                            let mut current_idx = 0usize;
                            for item in items.iter() {
                                // Check for index designator
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
                                        dest: elem_ptr,
                                        base: alloca,
                                        offset: offset_val,
                                        ty,
                                    });
                                    let store_ty = if elem_size <= 1 { IrType::I8 }
                                        else if elem_size <= 2 { IrType::I16 }
                                        else if elem_size <= 4 { IrType::I32 }
                                        else { IrType::I64 };
                                    self.emit(Instruction::Store { val, ptr: elem_ptr, ty: store_ty });
                                }
                                current_idx += 1;
                            }
                        }
                    }
                }
                // Return the alloca value (address of the compound literal)
                Operand::Value(alloca)
            }
            Expr::Sizeof(arg, _) => {
                let size = match arg.as_ref() {
                    SizeofArg::Type(ts) => self.sizeof_type(ts),
                    SizeofArg::Expr(expr) => self.sizeof_expr(expr),
                };
                Operand::Const(IrConst::I64(size as i64))
            }
            Expr::AddressOf(inner, _) => {
                // Special case: &(compound_literal) - compound literal creates an alloca,
                // taking its address just returns the alloca address
                if let Expr::CompoundLiteral(..) = inner.as_ref() {
                    // Lower the compound literal (which returns Operand::Value(alloca))
                    let val = self.lower_expr(inner);
                    return val;
                }
                // Try to get the lvalue address
                if let Some(lv) = self.lower_lvalue(inner) {
                    let addr = self.lvalue_addr(&lv);
                    // For alloca values, we need to get the address of the alloca
                    // using GEP with offset 0. This produces a leaq in codegen.
                    let result = self.fresh_value();
                    self.emit(Instruction::GetElementPtr {
                        dest: result,
                        base: addr,
                        offset: Operand::Const(IrConst::I64(0)),
                        ty: IrType::Ptr,
                    });
                    return Operand::Value(result);
                }
                // Fallback for globals
                if let Expr::Identifier(name, _) = inner.as_ref() {
                    let dest = self.fresh_value();
                    self.emit(Instruction::GlobalAddr { dest, name: name.clone() });
                    return Operand::Value(dest);
                }
                // For other expressions (e.g., &*ptr), just lower the inner expression
                let val = self.lower_expr(inner);
                return val;
            }
            Expr::Deref(inner, _) => {
                let ptr = self.lower_expr(inner);
                let dest = self.fresh_value();
                let deref_ty = self.get_pointee_type_of_expr(inner).unwrap_or(IrType::I64);
                match ptr {
                    Operand::Value(v) => {
                        self.emit(Instruction::Load { dest, ptr: v, ty: deref_ty });
                    }
                    Operand::Const(_) => {
                        let tmp = self.fresh_value();
                        self.emit(Instruction::Copy { dest: tmp, src: ptr });
                        self.emit(Instruction::Load { dest, ptr: tmp, ty: deref_ty });
                    }
                }
                Operand::Value(dest)
            }
            Expr::ArraySubscript(base, index, _) => {
                // For multi-dim arrays, a[i] where a is int[2][3] returns the
                // address of the sub-array (don't load). Only load at the innermost subscript.
                if self.subscript_result_is_array(expr) {
                    // Result is a sub-array - return its address as a pointer
                    let addr = self.compute_array_element_addr(base, index);
                    return Operand::Value(addr);
                }
                // Compute element address and load with proper element type
                let elem_ty = self.get_expr_type(expr);
                let addr = self.compute_array_element_addr(base, index);
                let dest = self.fresh_value();
                self.emit(Instruction::Load { dest, ptr: addr, ty: elem_ty });
                Operand::Value(dest)
            }
            Expr::MemberAccess(base_expr, field_name, _) => {
                // s.field -> compute address of struct base + field offset, then load
                let (field_offset, field_ty) = self.resolve_member_access(base_expr, field_name);
                let base_addr = self.get_struct_base_addr(base_expr);
                let field_addr = self.fresh_value();
                self.emit(Instruction::GetElementPtr {
                    dest: field_addr,
                    base: base_addr,
                    offset: Operand::Const(IrConst::I64(field_offset as i64)),
                    ty: field_ty,
                });
                // If the field is an array type, return the address (array decays to pointer).
                // Don't load the value - the address IS the pointer.
                let field_is_array = self.resolve_member_field_ctype(base_expr, field_name)
                    .map(|ct| matches!(ct, CType::Array(_, _)))
                    .unwrap_or(false);
                if field_is_array {
                    return Operand::Value(field_addr);
                }
                let dest = self.fresh_value();
                self.emit(Instruction::Load { dest, ptr: field_addr, ty: field_ty });
                Operand::Value(dest)
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                // p->field -> load pointer, then compute field offset, then load
                let ptr_val = self.lower_expr(base_expr);
                let base_addr = match ptr_val {
                    Operand::Value(v) => v,
                    Operand::Const(_) => {
                        let tmp = self.fresh_value();
                        self.emit(Instruction::Copy { dest: tmp, src: ptr_val });
                        tmp
                    }
                };
                let (field_offset, field_ty) = self.resolve_pointer_member_access(base_expr, field_name);
                let field_addr = self.fresh_value();
                self.emit(Instruction::GetElementPtr {
                    dest: field_addr,
                    base: base_addr,
                    offset: Operand::Const(IrConst::I64(field_offset as i64)),
                    ty: field_ty,
                });
                // If the field is an array type, return the address (array decays to pointer).
                let field_is_array = self.resolve_pointer_member_field_ctype(base_expr, field_name)
                    .map(|ct| matches!(ct, CType::Array(_, _)))
                    .unwrap_or(false);
                if field_is_array {
                    return Operand::Value(field_addr);
                }
                let dest = self.fresh_value();
                self.emit(Instruction::Load { dest, ptr: field_addr, ty: field_ty });
                Operand::Value(dest)
            }
            Expr::Comma(lhs, rhs, _) => {
                self.lower_expr(lhs);
                self.lower_expr(rhs)
            }
            Expr::StmtExpr(compound, _) => {
                // GCC statement expression: ({ stmt; ...; expr; })
                // Lower all block items; the value is the last expression
                let mut last_val = Operand::Const(IrConst::I64(0));
                for item in &compound.items {
                    match item {
                        BlockItem::Statement(stmt) => {
                            // Check if this is an expression statement (value to return)
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
        }
    }

    /// Insert a narrowing cast if the type is sub-64-bit (I32/U32).
    /// Returns the operand unchanged for 64-bit types.
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

    /// Lower short-circuit logical operation (&& or ||).
    /// For &&: if lhs is false, result is 0; otherwise evaluates rhs.
    /// For ||: if lhs is true, result is 1; otherwise evaluates rhs.
    fn lower_short_circuit(&mut self, lhs: &Expr, rhs: &Expr, is_and: bool) -> Operand {
        let result_alloca = self.fresh_value();
        self.emit(Instruction::Alloca { dest: result_alloca, ty: IrType::I64, size: 8 });

        let prefix = if is_and { "and" } else { "or" };
        let rhs_label = self.fresh_label(&format!("{}_rhs", prefix));
        let end_label = self.fresh_label(&format!("{}_end", prefix));

        // Evaluate lhs
        let lhs_val = self.lower_expr(lhs);

        // Short-circuit: store default result (0 for &&, 1 for ||)
        let default_val = if is_and { 0 } else { 1 };
        self.emit(Instruction::Store {
            val: Operand::Const(IrConst::I64(default_val)),
            ptr: result_alloca,
            ty: IrType::I64,
        });

        // Branch: for &&, evaluate rhs when lhs is true; for ||, when lhs is false
        let (true_label, false_label) = if is_and {
            (rhs_label.clone(), end_label.clone())
        } else {
            (end_label.clone(), rhs_label.clone())
        };
        self.terminate(Terminator::CondBranch {
            cond: lhs_val,
            true_label,
            false_label,
        });

        // Evaluate rhs
        self.start_block(rhs_label);
        let rhs_val = self.lower_expr(rhs);
        // Result is (rhs != 0) ? 1 : 0
        let rhs_bool = self.fresh_value();
        self.emit(Instruction::Cmp {
            dest: rhs_bool,
            op: IrCmpOp::Ne,
            lhs: rhs_val,
            rhs: Operand::Const(IrConst::I64(0)),
            ty: IrType::I64,
        });
        self.emit(Instruction::Store { val: Operand::Value(rhs_bool), ptr: result_alloca, ty: IrType::I64 });
        self.terminate(Terminator::Branch(end_label.clone()));

        self.start_block(end_label);
        let result = self.fresh_value();
        self.emit(Instruction::Load { dest: result, ptr: result_alloca, ty: IrType::I64 });
        Operand::Value(result)
    }

    /// Lower pre-increment/decrement for any lvalue.
    pub(super) fn lower_pre_inc_dec(&mut self, inner: &Expr, op: UnaryOp) -> Operand {
        let ty = self.get_expr_type(inner);
        if let Some(lv) = self.lower_lvalue(inner) {
            let loaded = self.load_lvalue_typed(&lv, ty);
            let loaded_val = match loaded {
                Operand::Value(v) => v,
                _ => {
                    let tmp = self.fresh_value();
                    self.emit(Instruction::Copy { dest: tmp, src: loaded });
                    tmp
                }
            };
            // For pointer types, increment by element size instead of 1
            let step = if ty == IrType::Ptr {
                let elem_size = self.get_pointer_elem_size(inner);
                Operand::Const(IrConst::I64(elem_size as i64))
            } else {
                Operand::Const(IrConst::I64(1))
            };
            let result = self.fresh_value();
            let ir_op = if op == UnaryOp::PreInc { IrBinOp::Add } else { IrBinOp::Sub };
            self.emit(Instruction::BinOp {
                dest: result,
                op: ir_op,
                lhs: Operand::Value(loaded_val),
                rhs: step,
                ty: IrType::I64,
            });
            self.store_lvalue_typed(&lv, Operand::Value(result), ty);
            return Operand::Value(result);
        }
        // Fallback
        self.lower_expr(inner)
    }

    /// Lower post-increment/decrement for any lvalue.
    pub(super) fn lower_post_inc_dec(&mut self, inner: &Expr, op: PostfixOp) -> Operand {
        let ty = self.get_expr_type(inner);
        if let Some(lv) = self.lower_lvalue(inner) {
            let loaded = self.load_lvalue_typed(&lv, ty);
            let loaded_val = match loaded.clone() {
                Operand::Value(v) => v,
                _ => {
                    let tmp = self.fresh_value();
                    self.emit(Instruction::Copy { dest: tmp, src: loaded.clone() });
                    tmp
                }
            };
            // For pointer types, increment by element size instead of 1
            let step = if ty == IrType::Ptr {
                let elem_size = self.get_pointer_elem_size(inner);
                Operand::Const(IrConst::I64(elem_size as i64))
            } else {
                Operand::Const(IrConst::I64(1))
            };
            let result = self.fresh_value();
            let ir_op = if op == PostfixOp::PostInc { IrBinOp::Add } else { IrBinOp::Sub };
            self.emit(Instruction::BinOp {
                dest: result,
                op: ir_op,
                lhs: Operand::Value(loaded_val),
                rhs: step,
                ty: IrType::I64,
            });
            self.store_lvalue_typed(&lv, Operand::Value(result), ty);
            // Return original value (before inc/dec)
            return loaded;
        }
        // Fallback
        self.lower_expr(inner)
    }

    /// Lower compound assignment (+=, -=, etc.) for any lvalue.
    pub(super) fn lower_compound_assign(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Operand {
        let ty = self.get_expr_type(lhs);
        let rhs_ty = self.get_expr_type(rhs);
        // Determine operation type (float promotion if either side is float)
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
                self.emit(Instruction::Cast { dest, src: loaded, from_ty: IrType::I64, to_ty: op_ty });
                Operand::Value(dest)
            } else {
                loaded
            };
            let ir_op = match op {
                BinOp::Add => IrBinOp::Add,
                BinOp::Sub => IrBinOp::Sub,
                BinOp::Mul => IrBinOp::Mul,
                BinOp::Div => IrBinOp::SDiv,
                BinOp::Mod => IrBinOp::SRem,
                BinOp::BitAnd => IrBinOp::And,
                BinOp::BitOr => IrBinOp::Or,
                BinOp::BitXor => IrBinOp::Xor,
                BinOp::Shl => IrBinOp::Shl,
                BinOp::Shr => IrBinOp::AShr,
                _ => IrBinOp::Add,
            };
            // For pointer += and -= operations, scale the RHS by element size
            let actual_rhs = if ty == IrType::Ptr && matches!(op, BinOp::Add | BinOp::Sub) {
                let elem_size = self.get_pointer_elem_size(lhs);
                if elem_size > 1 {
                    let scale = Operand::Const(IrConst::I64(elem_size as i64));
                    let scaled = self.fresh_value();
                    self.emit(Instruction::BinOp {
                        dest: scaled,
                        op: IrBinOp::Mul,
                        lhs: rhs_val,
                        rhs: scale,
                        ty: IrType::I64,
                    });
                    Operand::Value(scaled)
                } else {
                    rhs_val
                }
            } else {
                rhs_val
            };
            let result = self.fresh_value();
            self.emit(Instruction::BinOp {
                dest: result,
                op: ir_op,
                lhs: loaded_promoted,
                rhs: actual_rhs,
                ty: op_ty,
            });
            // Cast result back to lhs type if needed
            let store_val = if op_ty.is_float() && !ty.is_float() {
                let dest = self.fresh_value();
                self.emit(Instruction::Cast { dest, src: Operand::Value(result), from_ty: op_ty, to_ty: IrType::I64 });
                Operand::Value(dest)
            } else {
                Operand::Value(result)
            };
            self.store_lvalue_typed(&lv, store_val.clone(), ty);
            return store_val;
        }
        // Fallback
        rhs_val
    }

    /// Infer the IrType of an expression based on available type information.
    /// Returns I64 if the type cannot be determined. This is used to select
    /// unsigned vs signed operations and insert narrowing casts.
    pub(super) fn infer_expr_type(&self, expr: &Expr) -> IrType {
        match expr {
            Expr::Identifier(name, _) => {
                if self.enum_constants.contains_key(name) {
                    return IrType::I32;
                }
                if let Some(info) = self.locals.get(name) {
                    return info.ty;
                }
                if let Some(ginfo) = self.globals.get(name) {
                    return ginfo.ty;
                }
                IrType::I64
            }
            Expr::IntLiteral(val, _) => {
                // Plain integer: type is int if it fits, otherwise long
                if *val >= i32::MIN as i64 && *val <= i32::MAX as i64 {
                    IrType::I32
                } else {
                    IrType::I64
                }
            }
            Expr::UIntLiteral(val, _) => {
                // Unsigned int literal: type is unsigned int if it fits, otherwise unsigned long
                if *val <= u32::MAX as u64 {
                    IrType::U32
                } else {
                    IrType::U64
                }
            }
            Expr::LongLiteral(_, _) => {
                // Long suffix forces I64 type regardless of value
                IrType::I64
            }
            Expr::ULongLiteral(_, _) => {
                // Unsigned long suffix forces U64 type
                IrType::U64
            }
            Expr::CharLiteral(_, _) => IrType::I8,
            Expr::FloatLiteral(_, _) => IrType::F64,
            Expr::FloatLiteralF32(_, _) => IrType::F32,
            Expr::Cast(ref target_type, _, _) => {
                self.type_spec_to_ir(target_type)
            }
            Expr::BinaryOp(op, lhs, rhs, _) => {
                match op {
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le |
                    BinOp::Gt | BinOp::Ge | BinOp::LogicalAnd | BinOp::LogicalOr => {
                        IrType::I32 // comparison results are int
                    }
                    _ => {
                        let lt = self.infer_expr_type(lhs);
                        let rt = self.infer_expr_type(rhs);
                        Self::common_type(lt, rt)
                    }
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

    /// Determine the common type for binary operation (usual arithmetic conversions, simplified).
    fn common_type(a: IrType, b: IrType) -> IrType {
        // Float promotion takes priority
        if a == IrType::F64 || b == IrType::F64 { return IrType::F64; }
        if a == IrType::F32 || b == IrType::F32 { return IrType::F32; }
        // If either is I64/U64/Ptr, result is 64-bit
        if a == IrType::I64 || a == IrType::U64 || a == IrType::Ptr
            || b == IrType::I64 || b == IrType::U64 || b == IrType::Ptr
        {
            if a == IrType::U64 || b == IrType::U64 {
                return IrType::U64;
            }
            return IrType::I64;
        }
        // Both are 32-bit or narrower
        if a == IrType::U32 || b == IrType::U32 {
            return IrType::U32;
        }
        if a == IrType::I32 || b == IrType::I32 {
            return IrType::I32;
        }
        // Narrow types get promoted to int
        IrType::I32
    }

    /// Lower an expression and insert a cast if its type doesn't match the target type.
    /// Used for implicit float/int promotion in binary operations.
    pub(super) fn lower_expr_with_type(&mut self, expr: &Expr, target_ty: IrType) -> Operand {
        let src = self.lower_expr(expr);
        let src_ty = self.get_expr_type(expr);
        self.emit_implicit_cast(src, src_ty, target_ty)
    }

    /// Insert an implicit type cast from src_ty to target_ty if needed.
    /// Handles float<->int conversions, float<->float conversions, and integer
    /// widening/narrowing.
    pub(super) fn emit_implicit_cast(&mut self, src: Operand, src_ty: IrType, target_ty: IrType) -> Operand {
        if src_ty == target_ty {
            return src;
        }
        // Don't cast if both are pointer-like or void
        if target_ty == IrType::Ptr || target_ty == IrType::Void {
            return src;
        }
        if src_ty == IrType::Ptr && target_ty.is_integer() {
            // Pointer to integer is allowed (truncation/zero-ext handled at backend)
            return src;
        }
        if target_ty.is_float() && !src_ty.is_float() {
            // int -> float promotion
            let dest = self.fresh_value();
            self.emit(Instruction::Cast { dest, src, from_ty: src_ty, to_ty: target_ty });
            Operand::Value(dest)
        } else if !target_ty.is_float() && src_ty.is_float() {
            // float -> int demotion
            let dest = self.fresh_value();
            self.emit(Instruction::Cast { dest, src, from_ty: src_ty, to_ty: target_ty });
            Operand::Value(dest)
        } else if target_ty.is_float() && src_ty.is_float() && target_ty != src_ty {
            // float -> float (e.g., F64 -> F32 or F32 -> F64)
            let dest = self.fresh_value();
            self.emit(Instruction::Cast { dest, src, from_ty: src_ty, to_ty: target_ty });
            Operand::Value(dest)
        } else {
            // Integer to integer - only emit cast for narrowing or sign-changing
            src
        }
    }
}
