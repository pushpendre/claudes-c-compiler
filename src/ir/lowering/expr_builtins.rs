//! Builtin function call lowering: __builtin_* dispatch and X86 SSE intrinsics.
//!
//! This module handles the top-level dispatch for all __builtin_* functions,
//! complex number intrinsics, and X86 SSE/CRC intrinsics. Extracted helpers
//! live in sibling modules:
//! - expr_builtins_intrin.rs: integer bit-manipulation (clz, ctz, bswap, etc.)
//! - expr_builtins_overflow.rs: overflow-checking arithmetic
//! - expr_builtins_fpclass.rs: FP classification (fpclassify, isnan, isinf, etc.)

use crate::frontend::parser::ast::*;
use crate::frontend::sema::builtins::{self, BuiltinKind, BuiltinIntrinsic};
use crate::ir::ir::*;
use crate::common::types::{AddressSpace, IrType, CType};
use super::lowering::Lowerer;

impl Lowerer {
    /// Determine the return type for creal/crealf/creall/cimag/cimagf/cimagl based on function name.
    pub(super) fn creal_return_type(name: &str) -> IrType {
        match name {
            "crealf" | "__builtin_crealf" | "cimagf" | "__builtin_cimagf" => IrType::F32,
            "creall" | "__builtin_creall" | "cimagl" | "__builtin_cimagl" => IrType::F128,
            _ => IrType::F64, // creal, cimag, __builtin_creal, __builtin_cimag
        }
    }

    /// Try to lower a __builtin_* call. Returns Some(result) if handled.
    pub(super) fn try_lower_builtin_call(&mut self, name: &str, args: &[Expr]) -> Option<Operand> {
        // Handle __builtin_choose_expr(const_expr, expr1, expr2)
        // This is a compile-time selection: if const_expr is nonzero, returns expr1, else expr2.
        if name == "__builtin_choose_expr" {
            if args.len() >= 3 {
                let condition = self.eval_const_expr(&args[0]);
                let is_nonzero = match condition {
                    Some(IrConst::I64(v)) => v != 0,
                    Some(IrConst::I32(v)) => v != 0,
                    Some(IrConst::I128(v)) => v != 0,
                    Some(IrConst::F64(v)) => v != 0.0,
                    Some(IrConst::F32(v)) => v != 0.0,
                    _ => {
                        // Try lowering to get a constant
                        let cond_val = self.lower_expr(&args[0]);
                        match cond_val {
                            Operand::Const(IrConst::I64(v)) => v != 0,
                            Operand::Const(IrConst::I32(v)) => v != 0,
                            _ => true, // default to first expr if indeterminate
                        }
                    }
                };
                return Some(if is_nonzero {
                    self.lower_expr(&args[1])
                } else {
                    self.lower_expr(&args[2])
                });
            }
            return Some(Operand::Const(IrConst::I64(0)));
        }

        // Handle alloca specially - it needs dynamic stack allocation
        match name {
            "__builtin_alloca" | "__builtin_alloca_with_align" => {
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
        // Handle va_start/va_end/va_copy specially.
        // These builtins need a pointer to the va_list struct.
        // Use lower_va_list_pointer which correctly handles both local va_list
        // variables (array type, needs address-of) and va_list parameters
        // (pointer type after array-to-pointer decay, needs value load).
        match name {
            "__builtin_va_start" => {
                if let Some(ap_expr) = args.first() {
                    let ap_ptr_op = self.lower_va_list_pointer(ap_expr);
                    let ap_ptr = self.operand_to_value(ap_ptr_op);
                    self.emit(Instruction::VaStart { va_list_ptr: ap_ptr });
                }
                return Some(Operand::Const(IrConst::I64(0)));
            }
            "__builtin_va_end" => {
                if let Some(ap_expr) = args.first() {
                    let ap_ptr_op = self.lower_va_list_pointer(ap_expr);
                    let ap_ptr = self.operand_to_value(ap_ptr_op);
                    self.emit(Instruction::VaEnd { va_list_ptr: ap_ptr });
                }
                return Some(Operand::Const(IrConst::I64(0)));
            }
            "__builtin_va_copy" => {
                if args.len() >= 2 {
                    let dest_op = self.lower_va_list_pointer(&args[0]);
                    let src_op = self.lower_va_list_pointer(&args[1]);
                    let dest_ptr = self.operand_to_value(dest_op);
                    let src_ptr = self.operand_to_value(src_op);
                    self.emit(Instruction::VaCopy { dest_ptr, src_ptr });
                }
                return Some(Operand::Const(IrConst::I64(0)));
            }
            _ => {}
        }

        // __builtin_expect(exp, c) - branch prediction hint.
        // Returns exp unchanged, but must evaluate c for side effects.
        // __builtin_expect_with_probability(exp, c, prob) - same with probability.
        if name == "__builtin_expect" || name == "__builtin_expect_with_probability" {
            let result = if let Some(first) = args.first() {
                self.lower_expr(first)
            } else {
                Operand::Const(IrConst::I64(0))
            };
            // Evaluate remaining arguments for their side effects
            for arg in args.iter().skip(1) {
                self.lower_expr(arg);
            }
            return Some(result);
        }

        // __builtin_prefetch(addr, [rw], [locality]) - no-op performance hint
        if name == "__builtin_prefetch" {
            for arg in args {
                self.lower_expr(arg);
            }
            return Some(Operand::Const(IrConst::I64(0)));
        }

        // __builtin_unreachable() - marks code path as unreachable. Emit a trap instruction
        // (ud2 on x86, brk #0 on ARM, ebreak on RISC-V) via Terminator::Unreachable.
        // This must NOT generate a call to abort() because abort() may not exist
        // (e.g., in kernel code).
        if name == "__builtin_unreachable" {
            self.terminate(Terminator::Unreachable);
            // Start a new (unreachable) block so subsequent code can still be lowered
            let dead_label = self.fresh_label();
            self.start_block(dead_label);
            return Some(Operand::Const(IrConst::I64(0)));
        }

        // __builtin_trap() - generates a trap instruction to intentionally crash.
        // Like unreachable, this must emit the hardware trap directly, not call abort().
        if name == "__builtin_trap" {
            self.terminate(Terminator::Unreachable);
            let dead_label = self.fresh_label();
            self.start_block(dead_label);
            return Some(Operand::Const(IrConst::I64(0)));
        }

        // Handle atomic builtins (delegated to expr_atomics.rs)
        if let Some(result) = self.try_lower_atomic_builtin(name, args) {
            return Some(result);
        }

        let builtin_info = builtins::resolve_builtin(name)?;
        match &builtin_info.kind {
            BuiltinKind::LibcAlias(libc_name) => {
                let arg_types: Vec<IrType> = args.iter().map(|a| self.get_expr_type(a)).collect();
                let arg_vals: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                let dest = self.fresh_value();
                let libc_sig = self.func_meta.sigs.get(libc_name.as_str());
                let variadic = libc_sig.map_or(false, |s| s.is_variadic);
                let n_fixed = if variadic {
                    libc_sig.map(|s| s.param_types.len()).unwrap_or(arg_vals.len())
                } else { arg_vals.len() };
                let return_type = Self::builtin_return_type(name)
                    .or_else(|| libc_sig.map(|s| s.return_type))
                    .unwrap_or(crate::common::types::target_int_ir_type());
                let struct_arg_sizes = vec![None; arg_vals.len()];
                self.emit(Instruction::Call {
                    func: libc_name.clone(),
                    info: CallInfo {
                        dest: Some(dest), args: arg_vals, arg_types,
                        return_type, is_variadic: variadic, num_fixed_args: n_fixed,
                        struct_arg_sizes, struct_arg_classes: Vec::new(),
                        is_sret: false, is_fastcall: false,
                    },
                });
                Some(Operand::Value(dest))
            }
            BuiltinKind::Identity => {
                Some(args.first().map_or(Operand::Const(IrConst::I64(0)), |a| self.lower_expr(a)))
            }
            BuiltinKind::ConstantI64(val) => Some(Operand::Const(IrConst::I64(*val))),
            BuiltinKind::ConstantF64(val) => {
                let is_float_variant = name == "__builtin_inff"
                    || name == "__builtin_huge_valf"
                    || name == "__builtin_nanf";
                let is_long_double_variant = name == "__builtin_infl"
                    || name == "__builtin_huge_vall"
                    || name == "__builtin_nanl";
                if is_float_variant {
                    Some(Operand::Const(IrConst::F32(*val as f32)))
                } else if is_long_double_variant {
                    Some(Operand::Const(IrConst::long_double(*val)))
                } else {
                    Some(Operand::Const(IrConst::F64(*val)))
                }
            }
            BuiltinKind::Intrinsic(intrinsic) => {
                self.lower_builtin_intrinsic(intrinsic, name, args)
            }
        }
    }

    /// Lower a builtin intrinsic (the BuiltinKind::Intrinsic arm of try_lower_builtin_call).
    fn lower_builtin_intrinsic(&mut self, intrinsic: &BuiltinIntrinsic, name: &str, args: &[Expr]) -> Option<Operand> {
        match intrinsic {
            BuiltinIntrinsic::FpCompare => {
                if args.len() >= 2 {
                    let lhs_ty = self.get_expr_type(&args[0]);
                    let rhs_ty = self.get_expr_type(&args[1]);
                    // Promote to the widest floating-point type among the two operands.
                    let cmp_ty = if lhs_ty == IrType::F128 || rhs_ty == IrType::F128 {
                        IrType::F128
                    } else if lhs_ty == IrType::F64 || rhs_ty == IrType::F64 {
                        IrType::F64
                    } else if lhs_ty == IrType::F32 || rhs_ty == IrType::F32 {
                        IrType::F32
                    } else {
                        IrType::F64
                    };
                    let mut lhs = self.lower_expr(&args[0]);
                    let mut rhs = self.lower_expr(&args[1]);
                    if lhs_ty != cmp_ty {
                        let conv = self.emit_cast_val(lhs, lhs_ty, cmp_ty);
                        lhs = Operand::Value(conv);
                    }
                    if rhs_ty != cmp_ty {
                        let conv = self.emit_cast_val(rhs, rhs_ty, cmp_ty);
                        rhs = Operand::Value(conv);
                    }

                    // __builtin_isunordered: returns 1 if either operand is NaN.
                    // Emit (a != a) | (b != b) since NaN is the only value where x != x.
                    if name == "__builtin_isunordered" {
                        let lhs_nan = self.emit_cmp_val(IrCmpOp::Ne, lhs.clone(), lhs.clone(), cmp_ty);
                        let rhs_nan = self.emit_cmp_val(IrCmpOp::Ne, rhs.clone(), rhs.clone(), cmp_ty);
                        let dest = self.emit_binop_val(IrBinOp::Or, Operand::Value(lhs_nan), Operand::Value(rhs_nan), IrType::I32);
                        return Some(Operand::Value(dest));
                    }

                    // __builtin_islessgreater: returns 1 if a < b or a > b (NOT for NaN).
                    // This differs from (a != b) because NaN != NaN is true, but
                    // islessgreater(NaN, x) must be false.
                    // Emit (a < b) | (a > b).
                    if name == "__builtin_islessgreater" {
                        let lt = self.emit_cmp_val(IrCmpOp::Slt, lhs.clone(), rhs.clone(), cmp_ty);
                        let gt = self.emit_cmp_val(IrCmpOp::Sgt, lhs, rhs, cmp_ty);
                        let dest = self.emit_binop_val(IrBinOp::Or, Operand::Value(lt), Operand::Value(gt), IrType::I32);
                        return Some(Operand::Value(dest));
                    }

                    let cmp_op = match name {
                        "__builtin_isgreater" => IrCmpOp::Sgt,
                        "__builtin_isgreaterequal" => IrCmpOp::Sge,
                        "__builtin_isless" => IrCmpOp::Slt,
                        "__builtin_islessequal" => IrCmpOp::Sle,
                        _ => IrCmpOp::Eq,
                    };
                    let dest = self.emit_cmp_val(cmp_op, lhs, rhs, cmp_ty);
                    return Some(Operand::Value(dest));
                }
                Some(Operand::Const(IrConst::I64(0)))
            }
            BuiltinIntrinsic::AddOverflow => self.lower_overflow_builtin(name, args, IrBinOp::Add),
            BuiltinIntrinsic::SubOverflow => self.lower_overflow_builtin(name, args, IrBinOp::Sub),
            BuiltinIntrinsic::MulOverflow => self.lower_overflow_builtin(name, args, IrBinOp::Mul),
            BuiltinIntrinsic::Clz => self.lower_unary_intrinsic(name, args, IrUnaryOp::Clz),
            BuiltinIntrinsic::Ctz => self.lower_unary_intrinsic(name, args, IrUnaryOp::Ctz),
            BuiltinIntrinsic::Ffs => self.lower_ffs_intrinsic(name, args),
            BuiltinIntrinsic::Clrsb => self.lower_clrsb_intrinsic(name, args),
            BuiltinIntrinsic::Bswap => self.lower_bswap_intrinsic(name, args),
            BuiltinIntrinsic::Popcount => self.lower_unary_intrinsic(name, args, IrUnaryOp::Popcount),
            BuiltinIntrinsic::Parity => self.lower_parity_intrinsic(name, args),
            BuiltinIntrinsic::ComplexReal => {
                if !args.is_empty() {
                    let arg_ctype = self.expr_ctype(&args[0]);
                    let target_ty = Self::creal_return_type(name);
                    if arg_ctype.is_complex() {
                        let val = self.lower_complex_real_part(&args[0]);
                        // Cast from component type to target return type
                        // e.g., creal(float_complex) extracts F32 but must return F64
                        let comp_ty = Self::complex_component_ir_type(&arg_ctype);
                        if comp_ty != target_ty {
                            Some(self.emit_implicit_cast(val, comp_ty, target_ty))
                        } else {
                            Some(val)
                        }
                    } else {
                        let val = self.lower_expr(&args[0]);
                        let val_ty = self.get_expr_type(&args[0]);
                        Some(self.emit_implicit_cast(val, val_ty, target_ty))
                    }
                } else {
                    Some(Operand::Const(IrConst::F64(0.0)))
                }
            }
            BuiltinIntrinsic::ComplexImag => {
                if !args.is_empty() {
                    let arg_ctype = self.expr_ctype(&args[0]);
                    let target_ty = Self::creal_return_type(name);
                    if arg_ctype.is_complex() {
                        let val = self.lower_complex_imag_part(&args[0]);
                        // Cast from component type to target return type
                        let comp_ty = Self::complex_component_ir_type(&arg_ctype);
                        if comp_ty != target_ty {
                            Some(self.emit_implicit_cast(val, comp_ty, target_ty))
                        } else {
                            Some(val)
                        }
                    } else {
                        Some(match target_ty {
                            IrType::F32 => Operand::Const(IrConst::F32(0.0)),
                            IrType::F128 => Operand::Const(IrConst::long_double(0.0)),
                            _ => Operand::Const(IrConst::F64(0.0)),
                        })
                    }
                } else {
                    Some(Operand::Const(IrConst::F64(0.0)))
                }
            }
            BuiltinIntrinsic::ComplexConj => {
                if !args.is_empty() {
                    Some(self.lower_complex_conj(&args[0]))
                } else {
                    Some(Operand::Const(IrConst::F64(0.0)))
                }
            }
            BuiltinIntrinsic::Fence => {
                Some(Operand::Const(IrConst::I64(0)))
            }
            BuiltinIntrinsic::FpClassify => self.lower_builtin_fpclassify(args),
            BuiltinIntrinsic::IsNan => self.lower_builtin_isnan(args),
            BuiltinIntrinsic::IsInf => self.lower_builtin_isinf(args),
            BuiltinIntrinsic::IsFinite => self.lower_builtin_isfinite(args),
            BuiltinIntrinsic::IsNormal => self.lower_builtin_isnormal(args),
            BuiltinIntrinsic::SignBit => self.lower_builtin_signbit(args),
            BuiltinIntrinsic::IsInfSign => self.lower_builtin_isinf_sign(args),
            BuiltinIntrinsic::Alloca => {
                // Handled earlier in try_lower_builtin_call - should not reach here
                Some(Operand::Const(IrConst::I64(0)))
            }
            BuiltinIntrinsic::ComplexConstruct => {
                if args.len() >= 2 {
                    let real_val = self.lower_expr(&args[0]);
                    let imag_val = self.lower_expr(&args[1]);
                    let arg_ty = self.get_expr_type(&args[0]);
                    let (comp_ty, complex_size, comp_size) = if arg_ty == IrType::F32 {
                        (IrType::F32, 8usize, 4usize)
                    } else {
                        (IrType::F64, 16usize, 8usize)
                    };
                    let alloca = self.fresh_value();
                    self.emit(Instruction::Alloca { dest: alloca, ty: IrType::Ptr, size: complex_size, align: 0, volatile: false });
                    self.emit(Instruction::Store { val: real_val, ptr: alloca, ty: comp_ty , seg_override: AddressSpace::Default });
                    let imag_ptr = self.fresh_value();
                    self.emit(Instruction::GetElementPtr {
                        dest: imag_ptr, base: alloca,
                        offset: Operand::Const(IrConst::I64(comp_size as i64)),
                        ty: IrType::I8,
                    });
                    self.emit(Instruction::Store { val: imag_val, ptr: imag_ptr, ty: comp_ty , seg_override: AddressSpace::Default });
                    Some(Operand::Value(alloca))
                } else {
                    Some(Operand::Const(IrConst::I64(0)))
                }
            }
            BuiltinIntrinsic::VaStart | BuiltinIntrinsic::VaEnd | BuiltinIntrinsic::VaCopy => {
                unreachable!("va builtins handled earlier by name match")
            }
            // __builtin_constant_p(expr) -> 1 if expr is a compile-time constant, 0 otherwise.
            // We emit an IsConstant unary op so that after inlining and constant propagation,
            // the constant_fold pass can resolve it to 1 if the operand became constant.
            // This is critical for the Linux kernel's cpucap_is_possible() pattern where
            // __builtin_constant_p is used inside always_inline functions.
            BuiltinIntrinsic::ConstantP => {
                if let Some(arg) = args.first() {
                    // If already a compile-time constant at lowering time, resolve immediately
                    if self.eval_const_expr(arg).is_some() {
                        return Some(Operand::Const(IrConst::I32(1)));
                    }
                    // In non-inline-candidate functions (not always_inline, inline, or static),
                    // non-constant expressions always resolve to 0. Only inline candidates
                    // need deferred resolution via IsConstant, because after inlining a
                    // parameter may become constant (e.g., kernel's cpucap_is_possible pattern).
                    if !self.func().is_inline_candidate {
                        // Lower the argument expression for side effects, then return 0
                        self.lower_expr(arg);
                        return Some(Operand::Const(IrConst::I32(0)));
                    }
                    // Emit an IsConstant instruction to be resolved after optimization
                    let src = self.lower_expr(arg);
                    let src_ty = self.get_expr_type(arg);
                    let dest = self.fresh_value();
                    self.emit(Instruction::UnaryOp {
                        dest,
                        op: IrUnaryOp::IsConstant,
                        src,
                        ty: src_ty,
                    });
                    Some(Operand::Value(dest))
                } else {
                    Some(Operand::Const(IrConst::I32(0)))
                }
            }
            // __builtin_object_size(ptr, type) -> compile-time object size, or unknown
            // For types 0 and 1: return (size_t)-1 when size is unknown
            // For types 2 and 3: return 0 when size is unknown
            // We conservatively return "unknown" since we don't do points-to analysis.
            BuiltinIntrinsic::ObjectSize => {
                // Evaluate args for side effects
                for arg in args {
                    self.lower_expr(arg);
                }
                let obj_type = if args.len() >= 2 {
                    match self.eval_const_expr(&args[1]) {
                        Some(IrConst::I64(v)) => v,
                        Some(IrConst::I32(v)) => v as i64,
                        _ => 0,
                    }
                } else {
                    0
                };
                // Types 0 and 1: maximum estimate -> -1 (unknown)
                // Types 2 and 3: minimum estimate -> 0 (unknown)
                let result = if obj_type == 2 || obj_type == 3 { 0i64 } else { -1i64 };
                Some(Operand::Const(IrConst::I64(result)))
            }
            // __builtin_classify_type(expr) -> GCC type class integer
            BuiltinIntrinsic::ClassifyType => {
                let result = if let Some(arg) = args.first() {
                    // Special case: bare function identifiers decay to pointer
                    // type (class 5). expr_ctype may return CType::Int as
                    // fallback since functions aren't stored as variables.
                    if let Expr::Identifier(fname, _) = arg {
                        if self.known_functions.contains(fname.as_str()) {
                            5i64 // pointer_type_class (function decays to pointer)
                        } else {
                            let ctype = self.expr_ctype(arg);
                            classify_ctype(&ctype)
                        }
                    } else {
                        let ctype = self.expr_ctype(arg);
                        classify_ctype(&ctype)
                    }
                } else {
                    0i64 // no_type_class
                };
                Some(Operand::Const(IrConst::I64(result)))
            }
            // Nop intrinsic - just evaluate args for side effects and return 0
            BuiltinIntrinsic::Nop => {
                for arg in args {
                    self.lower_expr(arg);
                }
                Some(Operand::Const(IrConst::I64(0)))
            }
            // __builtin_cpu_init() - no-op on glibc systems (auto-initialized)
            BuiltinIntrinsic::CpuInit => {
                Some(Operand::Const(IrConst::I32(0)))
            }
            // __builtin_cpu_supports("feature") - conservatively return 0 (unsupported).
            // This ensures code takes the safe non-optimized fallback path.
            BuiltinIntrinsic::CpuSupports => {
                // Evaluate the string argument for side effects (though it's always a literal)
                for arg in args {
                    self.lower_expr(arg);
                }
                Some(Operand::Const(IrConst::I32(0)))
            }
            // X86 SSE fence/barrier operations (no dest, no meaningful return)
            BuiltinIntrinsic::X86Lfence => {
                self.emit(Instruction::Intrinsic { dest: None, op: IntrinsicOp::Lfence, dest_ptr: None, args: vec![] });
                Some(Operand::Const(IrConst::I64(0)))
            }
            BuiltinIntrinsic::X86Mfence => {
                self.emit(Instruction::Intrinsic { dest: None, op: IntrinsicOp::Mfence, dest_ptr: None, args: vec![] });
                Some(Operand::Const(IrConst::I64(0)))
            }
            BuiltinIntrinsic::X86Sfence => {
                self.emit(Instruction::Intrinsic { dest: None, op: IntrinsicOp::Sfence, dest_ptr: None, args: vec![] });
                Some(Operand::Const(IrConst::I64(0)))
            }
            BuiltinIntrinsic::X86Pause => {
                self.emit(Instruction::Intrinsic { dest: None, op: IntrinsicOp::Pause, dest_ptr: None, args: vec![] });
                Some(Operand::Const(IrConst::I64(0)))
            }
            // clflush(ptr)
            BuiltinIntrinsic::X86Clflush => {
                let arg_ops: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                self.emit(Instruction::Intrinsic { dest: None, op: IntrinsicOp::Clflush, dest_ptr: None, args: arg_ops });
                Some(Operand::Const(IrConst::I64(0)))
            }
            // Non-temporal store: movnti(ptr, val) - 32-bit
            BuiltinIntrinsic::X86Movnti => {
                let arg_ops: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                let ptr_val = self.operand_to_value(arg_ops[0].clone());
                self.emit(Instruction::Intrinsic { dest: None, op: IntrinsicOp::Movnti, dest_ptr: Some(ptr_val), args: vec![arg_ops[1].clone()] });
                Some(Operand::Const(IrConst::I64(0)))
            }
            // Non-temporal store: movnti64(ptr, val) - 64-bit
            BuiltinIntrinsic::X86Movnti64 => {
                let arg_ops: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                let ptr_val = self.operand_to_value(arg_ops[0].clone());
                self.emit(Instruction::Intrinsic { dest: None, op: IntrinsicOp::Movnti64, dest_ptr: Some(ptr_val), args: vec![arg_ops[1].clone()] });
                Some(Operand::Const(IrConst::I64(0)))
            }
            // Non-temporal store 128-bit: movntdq(ptr, src_ptr)
            BuiltinIntrinsic::X86Movntdq => {
                let arg_ops: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                let ptr_val = self.operand_to_value(arg_ops[0].clone());
                self.emit(Instruction::Intrinsic { dest: None, op: IntrinsicOp::Movntdq, dest_ptr: Some(ptr_val), args: vec![arg_ops[1].clone()] });
                Some(Operand::Const(IrConst::I64(0)))
            }
            // Non-temporal store 128-bit double: movntpd(ptr, src_ptr)
            BuiltinIntrinsic::X86Movntpd => {
                let arg_ops: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                let ptr_val = self.operand_to_value(arg_ops[0].clone());
                self.emit(Instruction::Intrinsic { dest: None, op: IntrinsicOp::Movntpd, dest_ptr: Some(ptr_val), args: vec![arg_ops[1].clone()] });
                Some(Operand::Const(IrConst::I64(0)))
            }
            // 128-bit operations that return __m128i (16-byte struct via pointer)
            // These allocate a 16-byte stack slot for the result and return a pointer to it
            BuiltinIntrinsic::X86Loaddqu
            | BuiltinIntrinsic::X86Pcmpeqb128
            | BuiltinIntrinsic::X86Pcmpeqd128
            | BuiltinIntrinsic::X86Psubusb128
            | BuiltinIntrinsic::X86Por128
            | BuiltinIntrinsic::X86Pand128
            | BuiltinIntrinsic::X86Pxor128
            | BuiltinIntrinsic::X86Set1Epi8
            | BuiltinIntrinsic::X86Set1Epi32
            | BuiltinIntrinsic::X86Aesenc128
            | BuiltinIntrinsic::X86Aesenclast128
            | BuiltinIntrinsic::X86Aesdec128
            | BuiltinIntrinsic::X86Aesdeclast128
            | BuiltinIntrinsic::X86Aesimc128
            | BuiltinIntrinsic::X86Aeskeygenassist128
            | BuiltinIntrinsic::X86Pclmulqdq128
            | BuiltinIntrinsic::X86Pslldqi128
            | BuiltinIntrinsic::X86Psrldqi128
            | BuiltinIntrinsic::X86Psllqi128
            | BuiltinIntrinsic::X86Psrlqi128
            | BuiltinIntrinsic::X86Pshufd128
            | BuiltinIntrinsic::X86Loadldi128
            // New SSE2 packed operations returning __m128i
            | BuiltinIntrinsic::X86Paddw128
            | BuiltinIntrinsic::X86Psubw128
            | BuiltinIntrinsic::X86Pmulhw128
            | BuiltinIntrinsic::X86Pmaddwd128
            | BuiltinIntrinsic::X86Pcmpgtw128
            | BuiltinIntrinsic::X86Pcmpgtb128
            | BuiltinIntrinsic::X86Psllwi128
            | BuiltinIntrinsic::X86Psrlwi128
            | BuiltinIntrinsic::X86Psrawi128
            | BuiltinIntrinsic::X86Psradi128
            | BuiltinIntrinsic::X86Pslldi128
            | BuiltinIntrinsic::X86Psrldi128
            | BuiltinIntrinsic::X86Paddd128
            | BuiltinIntrinsic::X86Psubd128
            | BuiltinIntrinsic::X86Packssdw128
            | BuiltinIntrinsic::X86Packuswb128
            | BuiltinIntrinsic::X86Punpcklbw128
            | BuiltinIntrinsic::X86Punpckhbw128
            | BuiltinIntrinsic::X86Punpcklwd128
            | BuiltinIntrinsic::X86Punpckhwd128
            | BuiltinIntrinsic::X86Set1Epi16
            | BuiltinIntrinsic::X86Pinsrw128
            | BuiltinIntrinsic::X86Cvtsi32Si128
            | BuiltinIntrinsic::X86Pshuflw128
            | BuiltinIntrinsic::X86Pshufhw128 => {
                let sse_op = match intrinsic {
                    BuiltinIntrinsic::X86Loaddqu => IntrinsicOp::Loaddqu,
                    BuiltinIntrinsic::X86Pcmpeqb128 => IntrinsicOp::Pcmpeqb128,
                    BuiltinIntrinsic::X86Pcmpeqd128 => IntrinsicOp::Pcmpeqd128,
                    BuiltinIntrinsic::X86Psubusb128 => IntrinsicOp::Psubusb128,
                    BuiltinIntrinsic::X86Por128 => IntrinsicOp::Por128,
                    BuiltinIntrinsic::X86Pand128 => IntrinsicOp::Pand128,
                    BuiltinIntrinsic::X86Pxor128 => IntrinsicOp::Pxor128,
                    BuiltinIntrinsic::X86Set1Epi8 => IntrinsicOp::SetEpi8,
                    BuiltinIntrinsic::X86Set1Epi32 => IntrinsicOp::SetEpi32,
                    BuiltinIntrinsic::X86Aesenc128 => IntrinsicOp::Aesenc128,
                    BuiltinIntrinsic::X86Aesenclast128 => IntrinsicOp::Aesenclast128,
                    BuiltinIntrinsic::X86Aesdec128 => IntrinsicOp::Aesdec128,
                    BuiltinIntrinsic::X86Aesdeclast128 => IntrinsicOp::Aesdeclast128,
                    BuiltinIntrinsic::X86Aesimc128 => IntrinsicOp::Aesimc128,
                    BuiltinIntrinsic::X86Aeskeygenassist128 => IntrinsicOp::Aeskeygenassist128,
                    BuiltinIntrinsic::X86Pclmulqdq128 => IntrinsicOp::Pclmulqdq128,
                    BuiltinIntrinsic::X86Pslldqi128 => IntrinsicOp::Pslldqi128,
                    BuiltinIntrinsic::X86Psrldqi128 => IntrinsicOp::Psrldqi128,
                    BuiltinIntrinsic::X86Psllqi128 => IntrinsicOp::Psllqi128,
                    BuiltinIntrinsic::X86Psrlqi128 => IntrinsicOp::Psrlqi128,
                    BuiltinIntrinsic::X86Pshufd128 => IntrinsicOp::Pshufd128,
                    BuiltinIntrinsic::X86Loadldi128 => IntrinsicOp::Loadldi128,
                    // New SSE2 operations
                    BuiltinIntrinsic::X86Paddw128 => IntrinsicOp::Paddw128,
                    BuiltinIntrinsic::X86Psubw128 => IntrinsicOp::Psubw128,
                    BuiltinIntrinsic::X86Pmulhw128 => IntrinsicOp::Pmulhw128,
                    BuiltinIntrinsic::X86Pmaddwd128 => IntrinsicOp::Pmaddwd128,
                    BuiltinIntrinsic::X86Pcmpgtw128 => IntrinsicOp::Pcmpgtw128,
                    BuiltinIntrinsic::X86Pcmpgtb128 => IntrinsicOp::Pcmpgtb128,
                    BuiltinIntrinsic::X86Psllwi128 => IntrinsicOp::Psllwi128,
                    BuiltinIntrinsic::X86Psrlwi128 => IntrinsicOp::Psrlwi128,
                    BuiltinIntrinsic::X86Psrawi128 => IntrinsicOp::Psrawi128,
                    BuiltinIntrinsic::X86Psradi128 => IntrinsicOp::Psradi128,
                    BuiltinIntrinsic::X86Pslldi128 => IntrinsicOp::Pslldi128,
                    BuiltinIntrinsic::X86Psrldi128 => IntrinsicOp::Psrldi128,
                    BuiltinIntrinsic::X86Paddd128 => IntrinsicOp::Paddd128,
                    BuiltinIntrinsic::X86Psubd128 => IntrinsicOp::Psubd128,
                    BuiltinIntrinsic::X86Packssdw128 => IntrinsicOp::Packssdw128,
                    BuiltinIntrinsic::X86Packuswb128 => IntrinsicOp::Packuswb128,
                    BuiltinIntrinsic::X86Punpcklbw128 => IntrinsicOp::Punpcklbw128,
                    BuiltinIntrinsic::X86Punpckhbw128 => IntrinsicOp::Punpckhbw128,
                    BuiltinIntrinsic::X86Punpcklwd128 => IntrinsicOp::Punpcklwd128,
                    BuiltinIntrinsic::X86Punpckhwd128 => IntrinsicOp::Punpckhwd128,
                    BuiltinIntrinsic::X86Set1Epi16 => IntrinsicOp::SetEpi16,
                    BuiltinIntrinsic::X86Pinsrw128 => IntrinsicOp::Pinsrw128,
                    BuiltinIntrinsic::X86Cvtsi32Si128 => IntrinsicOp::Cvtsi32Si128,
                    BuiltinIntrinsic::X86Pshuflw128 => IntrinsicOp::Pshuflw128,
                    BuiltinIntrinsic::X86Pshufhw128 => IntrinsicOp::Pshufhw128,
                    _ => unreachable!("SSE128 dispatch matched non-SSE128 intrinsic: {:?}", intrinsic),
                };
                let arg_ops: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                // Allocate 16-byte stack slot for result
                let result_alloca = self.fresh_value();
                self.emit(Instruction::Alloca { dest: result_alloca, ty: IrType::Ptr, size: 16, align: 0, volatile: false });
                let dest_val = self.fresh_value();
                self.emit(Instruction::Intrinsic {
                    dest: Some(dest_val),
                    op: sse_op,
                    dest_ptr: Some(result_alloca),
                    args: arg_ops,
                });
                // Return pointer to the 16-byte result (struct return)
                Some(Operand::Value(result_alloca))
            }
            // storedqu(ptr, src_ptr) - store 128 bits unaligned
            BuiltinIntrinsic::X86Storedqu => {
                let arg_ops: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                let ptr_val = self.operand_to_value(arg_ops[0].clone());
                self.emit(Instruction::Intrinsic { dest: None, op: IntrinsicOp::Storedqu, dest_ptr: Some(ptr_val), args: vec![arg_ops[1].clone()] });
                Some(Operand::Const(IrConst::I64(0)))
            }
            // pmovmskb returns i32 scalar
            BuiltinIntrinsic::X86Pmovmskb128 => {
                let arg_ops: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                let dest_val = self.fresh_value();
                self.emit(Instruction::Intrinsic {
                    dest: Some(dest_val),
                    op: IntrinsicOp::Pmovmskb128,
                    dest_ptr: None,
                    args: arg_ops,
                });
                Some(Operand::Value(dest_val))
            }
            // pextrw / cvtsi128_si32 / cvtsi128_si64 return scalar i32/i64
            BuiltinIntrinsic::X86Pextrw128
            | BuiltinIntrinsic::X86Cvtsi128Si32
            | BuiltinIntrinsic::X86Cvtsi128Si64 => {
                let sse_op = match intrinsic {
                    BuiltinIntrinsic::X86Pextrw128 => IntrinsicOp::Pextrw128,
                    BuiltinIntrinsic::X86Cvtsi128Si32 => IntrinsicOp::Cvtsi128Si32,
                    BuiltinIntrinsic::X86Cvtsi128Si64 => IntrinsicOp::Cvtsi128Si64,
                    _ => unreachable!(),
                };
                let arg_ops: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                let dest_val = self.fresh_value();
                self.emit(Instruction::Intrinsic {
                    dest: Some(dest_val),
                    op: sse_op,
                    dest_ptr: None,
                    args: arg_ops,
                });
                Some(Operand::Value(dest_val))
            }
            // storel_epi64(ptr, src_ptr) - store low 64 bits
            BuiltinIntrinsic::X86Storeldi128 => {
                let arg_ops: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                let ptr_val = self.operand_to_value(arg_ops[0].clone());
                self.emit(Instruction::Intrinsic { dest: None, op: IntrinsicOp::Storeldi128, dest_ptr: Some(ptr_val), args: vec![arg_ops[1].clone()] });
                Some(Operand::Const(IrConst::I64(0)))
            }
            // CRC32 operations return scalar i32/i64
            BuiltinIntrinsic::X86Crc32_8 | BuiltinIntrinsic::X86Crc32_16
            | BuiltinIntrinsic::X86Crc32_32 | BuiltinIntrinsic::X86Crc32_64 => {
                let sse_op = match intrinsic {
                    BuiltinIntrinsic::X86Crc32_8 => IntrinsicOp::Crc32_8,
                    BuiltinIntrinsic::X86Crc32_16 => IntrinsicOp::Crc32_16,
                    BuiltinIntrinsic::X86Crc32_32 => IntrinsicOp::Crc32_32,
                    BuiltinIntrinsic::X86Crc32_64 => IntrinsicOp::Crc32_64,
                    _ => unreachable!("CRC32 dispatch matched non-CRC32 intrinsic: {:?}", intrinsic),
                };
                let arg_ops: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                let dest_val = self.fresh_value();
                self.emit(Instruction::Intrinsic {
                    dest: Some(dest_val),
                    op: sse_op,
                    dest_ptr: None,
                    args: arg_ops,
                });
                Some(Operand::Value(dest_val))
            }
            // __builtin_frame_address(level) / __builtin_return_address(level)
            // Only level 0 is supported; higher levels return 0.
            BuiltinIntrinsic::FrameAddress | BuiltinIntrinsic::ReturnAddress => {
                // Evaluate the level argument
                let level = if !args.is_empty() {
                    self.lower_expr(&args[0])
                } else {
                    Operand::Const(IrConst::I64(0))
                };
                // Only level 0 is supported; for other levels return NULL
                let is_level_zero = match &level {
                    Operand::Const(IrConst::I64(0)) => true,
                    Operand::Const(IrConst::I32(0)) => true,
                    _ => false,
                };
                if is_level_zero {
                    let op = if *intrinsic == BuiltinIntrinsic::FrameAddress {
                        IntrinsicOp::FrameAddress
                    } else {
                        IntrinsicOp::ReturnAddress
                    };
                    let dest_val = self.fresh_value();
                    self.emit(Instruction::Intrinsic {
                        dest: Some(dest_val),
                        op,
                        dest_ptr: None,
                        args: vec![],
                    });
                    Some(Operand::Value(dest_val))
                } else {
                    // TODO: support non-zero levels by walking the frame pointer chain
                    // Non-zero levels: return NULL (matching GCC behavior for non-optimized)
                    Some(Operand::Const(IrConst::I64(0)))
                }
            }
        }
    }



}

/// Classify a CType according to GCC's __builtin_classify_type conventions.
/// Returns the GCC type class integer:
///   0 = no_type_class (void)
///   1 = integer_type_class
///   2 = char_type_class
///   3 = enumeral_type_class
///   4 = boolean_type_class
///   5 = pointer_type_class
///   6 = reference_type_class (C++ only, not used in C)
///   8 = real_type_class
///   9 = complex_type_class
///  12 = record_type_class (struct)
///  13 = union_type_class
///  14 = array_type_class
///  16 = lang_type_class
fn classify_ctype(ty: &CType) -> i64 {
    // GCC __builtin_classify_type returns integer type classes.
    // GCC treats char, _Bool, and enum as integer_type_class (1) in practice.
    match ty {
        CType::Void => 0,            // no_type_class
        CType::Bool
        | CType::Char | CType::UChar
        | CType::Short | CType::UShort
        | CType::Int | CType::UInt
        | CType::Long | CType::ULong
        | CType::LongLong | CType::ULongLong
        | CType::Int128 | CType::UInt128
        | CType::Enum(_) => 1,       // integer_type_class
        CType::Float | CType::Double | CType::LongDouble => 8, // real_type_class
        CType::ComplexFloat | CType::ComplexDouble
        | CType::ComplexLongDouble => 9, // complex_type_class
        CType::Pointer(_, _) => 5,      // pointer_type_class
        CType::Array(_, _) => 5,     // GCC decays arrays to pointers
        CType::Function(_) => 5,     // function decays to pointer
        CType::Struct(_) => 12,      // record_type_class
        CType::Union(_) => 13,       // union_type_class
        CType::Vector(_, _) => 14,   // array_type_class (GCC classifies vectors here)
    }
}
