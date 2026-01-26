/// Constant expression evaluation for semantic analysis.
///
/// This module provides compile-time constant evaluation using only sema-available
/// state (TypeContext, SymbolTable, ExprTypeChecker). It returns `IrConst` values
/// matching the lowerer's richer result type, enabling:
/// - Float literal evaluation (F32, F64, LongDouble)
/// - Proper cast chain evaluation with bit-width tracking
/// - Binary operations with type-aware signedness semantics
/// - sizeof/alignof via sema's type resolution
///
/// The lowerer's const_eval.rs handles additional lowering-specific cases:
/// - Global address expressions (&x, func, arr)
/// - func_state const local values
/// - Pointer arithmetic on global addresses
/// These remain in the lowerer since they require IR-level state.
///
/// This is Step 5 of the typed-AST plan (high_sema_expansion_typed_ast.txt):
/// unify constant expression evaluation between sema and lowering.

use crate::common::types::CType;
use crate::common::types::AddressSpace;
use crate::common::const_arith;
use crate::ir::ir::IrConst;
use crate::frontend::parser::ast::*;
use super::type_context::TypeContext;
use super::sema::FunctionInfo;
use crate::common::symbol_table::SymbolTable;
use crate::common::fx_hash::FxHashMap;

/// Map from AST expression node identity to its pre-computed compile-time constant.
///
/// Uses the same raw-pointer-address keying strategy as ExprTypeMap. During sema's
/// AST walk, expressions that can be evaluated at compile time have their IrConst
/// values stored here. The lowerer consults this map as an O(1) fast path before
/// falling back to its own eval_const_expr.
pub type ConstMap = FxHashMap<usize, IrConst>;

/// Constant expression evaluator using only sema-available state.
///
/// This evaluator is created fresh for each expression evaluation, borrowing
/// the sema state it needs. It is stateless and does not modify anything.
///
/// When `const_values` and `expr_types` caches are provided, previously-computed
/// results are returned in O(1) instead of re-traversing the AST. This prevents
/// exponential blowup on deeply nested expressions.
pub struct SemaConstEval<'a> {
    /// Type context for typedef, enum, and struct layout resolution.
    pub types: &'a TypeContext,
    /// Symbol table for variable type lookup.
    pub symbols: &'a SymbolTable,
    /// Function signatures for return type resolution in sizeof(expr).
    pub functions: &'a FxHashMap<String, FunctionInfo>,
    /// Pre-computed constant values from bottom-up sema walk (memoization cache).
    pub const_values: Option<&'a FxHashMap<usize, IrConst>>,
    /// Pre-computed expression types from bottom-up sema walk.
    pub expr_types: Option<&'a FxHashMap<usize, CType>>,
}

impl<'a> SemaConstEval<'a> {
    /// Try to evaluate a constant expression at compile time.
    ///
    /// Returns `Some(IrConst)` for expressions that can be fully evaluated using
    /// sema state. Returns `None` for expressions that require lowering state
    /// (global addresses, runtime values, etc.).
    pub fn eval_const_expr(&self, expr: &Expr) -> Option<IrConst> {
        // Memoization: if this expression's constant value was already computed
        // during the bottom-up sema walk, return it in O(1).
        if let Some(cache) = self.const_values {
            let key = expr as *const Expr as usize;
            if let Some(cached) = cache.get(&key) {
                return Some(cached.clone());
            }
        }

        match expr {
            // Integer literals: preserve the C type width so that
            // ctype_from_ir_const can infer the correct CType.
            // IntLiteral has C type `int` (32-bit) when the value fits, otherwise `long`.
            // This matters for operations like (1 << 31) / N where 32-bit vs 64-bit
            // determines whether the shift result is negative (INT_MIN) or positive.
            // Note: the lexer may produce IntLiteral for values > i32::MAX (decimal
            // literals without suffix), so we must range-check before using I32.
            Expr::IntLiteral(val, _) => {
                if *val >= i32::MIN as i64 && *val <= i32::MAX as i64 {
                    Some(IrConst::I32(*val as i32))
                } else {
                    Some(IrConst::I64(*val))
                }
            }
            Expr::LongLiteral(val, _) => {
                Some(IrConst::I64(*val))
            }
            // UIntLiteral must stay as I64 to preserve the unsigned value.
            // IrConst::I32 is signed and would misrepresent values > i32::MAX
            // (e.g., 0xFFFFFFFA as uint = 4294967290, not -6).
            Expr::UIntLiteral(val, _) => {
                Some(IrConst::I64(*val as i64))
            }
            Expr::ULongLiteral(val, _) => {
                Some(IrConst::I64(*val as i64))
            }
            Expr::CharLiteral(ch, _) => {
                Some(IrConst::I32(*ch as i32))
            }

            // Float literals
            Expr::FloatLiteral(val, _) => {
                Some(IrConst::F64(*val))
            }
            Expr::FloatLiteralF32(val, _) => {
                Some(IrConst::F32(*val as f32))
            }
            Expr::FloatLiteralLongDouble(val, _) => {
                Some(IrConst::LongDouble(*val))
            }

            Expr::UnaryOp(UnaryOp::Plus, inner, _) => {
                self.eval_const_expr(inner)
            }
            Expr::UnaryOp(UnaryOp::Neg, inner, _) => {
                let val = self.eval_const_expr(inner)?;
                // C integer promotion (C11 6.3.1.1): sub-int types are promoted
                // to int before arithmetic. For unsigned sub-int types (unsigned
                // char/short), the I8/I16 bit pattern must be zero-extended to i32
                // (not sign-extended) before negation. Without this, I16(-1)
                // representing unsigned short 65535 would be negated as -(-1) = 1
                // instead of -(65535) = -65535.
                let promoted = self.promote_sub_int_for_unary(inner, val);
                const_arith::negate_const(promoted)
            }
            Expr::UnaryOp(UnaryOp::BitNot, inner, _) => {
                let val = self.eval_const_expr(inner)?;
                let promoted = self.promote_sub_int_for_unary(inner, val);
                const_arith::bitnot_const(promoted)
            }

            // Logical NOT
            Expr::UnaryOp(UnaryOp::LogicalNot, inner, _) => {
                let val = self.eval_const_expr(inner)?;
                Some(IrConst::I64(if val.is_nonzero() { 0 } else { 1 }))
            }

            // Binary operations
            Expr::BinaryOp(op, lhs, rhs, _) => {
                let l = self.eval_const_expr(lhs)?;
                let r = self.eval_const_expr(rhs)?;
                // Derive operand CTypes for signedness/width determination.
                // Priority: (1) pre-annotated expr_types from sema (O(1), preserves
                // unsigned info), (2) ctype_from_ir_const (O(1), but loses unsigned),
                // (3) infer_expr_ctype (expensive, full type inference).
                // Using expr_types first is critical for correct signedness on casts
                // like (unsigned int)(0x80000000) >> 2 -- ctype_from_ir_const would
                // map the IrConst::I64 to signed Long, losing the unsigned cast.
                let lhs_ctype = self.lookup_expr_type(lhs)
                    .or_else(|| Self::ctype_from_ir_const(&l))
                    .or_else(|| self.infer_expr_ctype(lhs));
                let rhs_ctype = self.lookup_expr_type(rhs)
                    .or_else(|| Self::ctype_from_ir_const(&r))
                    .or_else(|| self.infer_expr_ctype(rhs));
                self.eval_const_binop(op, &l, &r, lhs_ctype.as_ref(), rhs_ctype.as_ref())
            }

            // Cast expressions with proper bit-width tracking
            Expr::Cast(ref target_type, inner, _) => {
                let target_ctype = self.type_spec_to_ctype(target_type);
                let src_val = self.eval_const_expr(inner)?;

                // Handle float source types: use value-based conversion
                if let Some(fv) = src_val.to_f64() {
                    if matches!(&src_val, IrConst::F32(_) | IrConst::F64(_) | IrConst::LongDouble(_)) {
                        return self.cast_float_to_ctype(fv, &target_ctype);
                    }
                }

                // Handle I128 source: use full 128-bit value to avoid truncation
                // through the u64-based eval_const_expr_as_bits path
                if let IrConst::I128(v128) = src_val {
                    let target_size = self.ctype_size(&target_ctype);
                    // Determine source signedness for int-to-float conversions
                    let src_unsigned = self.lookup_expr_type(inner)
                        .map_or(false, |ct| ct.is_unsigned());
                    return self.cast_i128_to_ctype(v128, &target_ctype, target_size, src_unsigned);
                }

                // Integer source: use bit-based cast chain evaluation
                let (bits, _src_signed) = self.eval_const_expr_as_bits(inner)?;
                let target_size = self.ctype_size(&target_ctype);
                let target_width = target_size * 8;
                let target_signed = !target_ctype.is_unsigned() && !target_ctype.is_pointer_like();

                // Truncate to target width
                let truncated = if target_width >= 64 {
                    bits
                } else if target_width == 0 {
                    return None; // void cast
                } else {
                    bits & ((1u64 << target_width) - 1)
                };

                // Convert to IrConst based on target CType
                self.bits_to_irconst(truncated, &target_ctype, target_signed)
            }

            // Identifier: enum constants
            Expr::Identifier(name, _) => {
                if let Some(&val) = self.types.enum_constants.get(name) {
                    return Some(IrConst::I64(val));
                }
                None
            }

            // sizeof
            Expr::Sizeof(arg, _) => {
                let size = match arg.as_ref() {
                    SizeofArg::Type(ts) => self.sizeof_type_spec(ts),
                    SizeofArg::Expr(e) => self.sizeof_expr(e),
                };
                size.map(|s| IrConst::I64(s as i64))
            }

            // _Alignof
            Expr::Alignof(ref ts, _) => {
                let align = self.alignof_type_spec(ts);
                Some(IrConst::I64(align as i64))
            }

            // __alignof__(expr) - GCC extension: alignment of expression's type
            Expr::AlignofExpr(ref inner_expr, _) => {
                let ctype = self.infer_expr_ctype(inner_expr)?;
                let align = ctype.align_ctx(&self.types.struct_layouts);
                Some(IrConst::I64(align as i64))
            }

            // Ternary conditional
            Expr::Conditional(cond, then_e, else_e, _) => {
                let cond_val = self.eval_const_expr(cond)?;
                if cond_val.is_nonzero() {
                    self.eval_const_expr(then_e)
                } else {
                    self.eval_const_expr(else_e)
                }
            }

            // GNU conditional (a ?: b)
            Expr::GnuConditional(cond, else_e, _) => {
                let cond_val = self.eval_const_expr(cond)?;
                if cond_val.is_nonzero() {
                    Some(cond_val)
                } else {
                    self.eval_const_expr(else_e)
                }
            }

            // __builtin_types_compatible_p
            Expr::BuiltinTypesCompatibleP(ref type1, ref type2, _) => {
                let ctype1 = self.type_spec_to_ctype(type1);
                let ctype2 = self.type_spec_to_ctype(type2);
                let compatible = self.ctypes_compatible(&ctype1, &ctype2);
                Some(IrConst::I64(compatible as i64))
            }

            // AddressOf for offsetof patterns: &((type*)0)->member
            Expr::AddressOf(inner, _) => {
                self.eval_offsetof_pattern(inner)
            }

            // Handle compile-time builtin function calls in constant expressions.
            Expr::FunctionCall(func, args, _) => {
                if let Expr::Identifier(name, _) = func.as_ref() {
                    match name.as_str() {
                        "__builtin_choose_expr" if args.len() >= 3 => {
                            let cond = self.eval_const_expr(&args[0])?;
                            if cond.is_nonzero() {
                                self.eval_const_expr(&args[1])
                            } else {
                                self.eval_const_expr(&args[2])
                            }
                        }
                        "__builtin_constant_p" => {
                            let is_const = if let Some(arg) = args.first() {
                                self.eval_const_expr(arg).is_some()
                            } else {
                                false
                            };
                            Some(IrConst::I32(if is_const { 1 } else { 0 }))
                        }
                        "__builtin_expect" | "__builtin_expect_with_probability" => {
                            if let Some(arg) = args.first() {
                                self.eval_const_expr(arg)
                            } else {
                                None
                            }
                        }
                        "__builtin_bswap16" => {
                            let val = self.eval_const_expr(args.first()?)?;
                            let v = val.to_i64()? as u16;
                            Some(IrConst::I32(v.swap_bytes() as i32))
                        }
                        "__builtin_bswap32" => {
                            let val = self.eval_const_expr(args.first()?)?;
                            let v = val.to_i64()? as u32;
                            Some(IrConst::I32(v.swap_bytes() as i32))
                        }
                        "__builtin_bswap64" => {
                            let val = self.eval_const_expr(args.first()?)?;
                            let v = val.to_i64()? as u64;
                            Some(IrConst::I64(v.swap_bytes() as i64))
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }

            _ => None,
        }
    }

    /// Evaluate a constant expression, returning raw u64 bits and signedness.
    /// Preserves signedness through cast chains for proper widening.
    fn eval_const_expr_as_bits(&self, expr: &Expr) -> Option<(u64, bool)> {
        match expr {
            Expr::Cast(ref target_type, inner, _) => {
                let (bits, _src_signed) = self.eval_const_expr_as_bits(inner)?;
                let target_ctype = self.type_spec_to_ctype(target_type);
                let target_width = self.ctype_size(&target_ctype) * 8;
                let target_signed = !target_ctype.is_unsigned() && !target_ctype.is_pointer_like();
                Some(const_arith::truncate_and_extend_bits(bits, target_width, target_signed))
            }
            _ => {
                let val = self.eval_const_expr(expr)?;
                let bits = match &val {
                    IrConst::F32(v) => *v as i64 as u64,
                    IrConst::F64(v) => *v as i64 as u64,
                    _ => val.to_i64().unwrap_or(0) as u64,
                };
                Some((bits, true))
            }
        }
    }

    /// Evaluate a binary operation on constant operands.
    /// Uses both LHS and RHS types for C's usual arithmetic conversions (C11 6.3.1.8),
    /// except for shifts where only the LHS type determines the result type (C11 6.5.7).
    /// Delegates arithmetic to the shared implementation in `common::const_arith`.
    fn eval_const_binop(&self, op: &BinOp, lhs: &IrConst, rhs: &IrConst, lhs_ctype: Option<&CType>, rhs_ctype: Option<&CType>) -> Option<IrConst> {
        let lhs_size = lhs_ctype.map_or(4, |ct| self.ctype_size(ct).max(4));
        let lhs_unsigned = lhs_ctype.map_or(false, |ct| ct.is_unsigned());

        let is_shift = matches!(op, BinOp::Shl | BinOp::Shr);

        // For shifts (C11 6.5.7): result type is the promoted LHS type only.
        // For other ops: apply C's usual arithmetic conversions using both operand types.
        let (is_32bit, is_unsigned) = if is_shift {
            (lhs_size <= 4, lhs_unsigned)
        } else {
            let rhs_size = rhs_ctype.map_or(4, |ct| self.ctype_size(ct).max(4));
            let rhs_unsigned = rhs_ctype.map_or(false, |ct| ct.is_unsigned());
            let result_size = lhs_size.max(rhs_size);
            let is_unsigned = if lhs_size == rhs_size {
                lhs_unsigned || rhs_unsigned
            } else if lhs_size > rhs_size {
                lhs_unsigned
            } else {
                rhs_unsigned
            };
            (result_size <= 4, is_unsigned)
        };
        const_arith::eval_const_binop(op, lhs, rhs, is_32bit, is_unsigned)
    }

    /// Evaluate the offsetof pattern: &((type*)0)->member
    /// Also handles nested member access like &((type*)0)->data.x
    fn eval_offsetof_pattern(&self, expr: &Expr) -> Option<IrConst> {
        let (offset, _ty) = self.eval_offsetof_pattern_with_type(expr)?;
        Some(IrConst::I64(offset as i64))
    }

    /// Evaluate an offsetof sub-expression, returning both the accumulated byte offset
    /// and the CType of the resulting expression (needed for chained member access).
    fn eval_offsetof_pattern_with_type(&self, expr: &Expr) -> Option<(usize, CType)> {
        match expr {
            Expr::PointerMemberAccess(base, field_name, _) => {
                let (type_spec, base_offset) = self.extract_null_pointer_cast_with_offset(base)?;
                let layout = self.get_struct_layout_for_type(&type_spec)?;
                let (field_offset, field_ty) = layout.field_offset(field_name, self.types)?;
                Some((base_offset + field_offset, field_ty))
            }
            Expr::MemberAccess(base, field_name, _) => {
                // First try: base is *((type*)0) (deref pattern)
                if let Expr::Deref(inner, _) = base.as_ref() {
                    let (type_spec, base_offset) = self.extract_null_pointer_cast_with_offset(inner)?;
                    let layout = self.get_struct_layout_for_type(&type_spec)?;
                    let (field_offset, field_ty) = layout.field_offset(field_name, self.types)?;
                    return Some((base_offset + field_offset, field_ty));
                }
                // Second try: base is itself an offsetof sub-expression (chained access)
                // e.g., ((type*)0)->data.x where base = ((type*)0)->data
                let (base_offset, base_type) = self.eval_offsetof_pattern_with_type(base)?;
                let struct_key = match &base_type {
                    CType::Struct(key) | CType::Union(key) => key.clone(),
                    _ => return None,
                };
                let layout = self.types.struct_layouts.get(&*struct_key)?;
                let (field_offset, field_ty) = layout.field_offset(field_name, self.types)?;
                Some((base_offset + field_offset, field_ty))
            }
            Expr::ArraySubscript(base, index, _) => {
                let (base_offset, base_type) = self.eval_offsetof_pattern_with_type(base)?;
                let idx_val = self.eval_const_expr(index)?;
                let idx = idx_val.to_i64()?;
                let elem_size = match &base_type {
                    CType::Array(elem, _) => elem.size_ctx(&self.types.struct_layouts),
                    _ => return None,
                };
                let elem_ty = match &base_type {
                    CType::Array(elem, _) => (**elem).clone(),
                    _ => return None,
                };
                Some(((base_offset as i64 + idx * elem_size as i64) as usize, elem_ty))
            }
            _ => None,
        }
    }

    /// Extract the struct type from a (type*)0 pattern.
    fn extract_null_pointer_cast_with_offset(&self, expr: &Expr) -> Option<(TypeSpecifier, usize)> {
        match expr {
            Expr::Cast(ref type_spec, inner, _) => {
                if let TypeSpecifier::Pointer(inner_ts, _) = type_spec {
                    if const_arith::is_zero_expr(inner) {
                        return Some((*inner_ts.clone(), 0));
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Get the struct layout for a type specifier.
    fn get_struct_layout_for_type(&self, type_spec: &TypeSpecifier) -> Option<crate::common::types::RcLayout> {
        let ctype = self.type_spec_to_ctype(type_spec);
        match &ctype {
            CType::Struct(key) | CType::Union(key) => {
                self.types.struct_layouts.get(&**key).cloned()
            }
            _ => None,
        }
    }

    /// Check if two CTypes are compatible (for __builtin_types_compatible_p).
    fn ctypes_compatible(&self, t1: &CType, t2: &CType) -> bool {
        // Strip qualifiers (CType doesn't carry them) and compare
        match (t1, t2) {
            (CType::Pointer(a, _), CType::Pointer(b, _)) => self.ctypes_compatible(a, b),
            (CType::Array(a, _), CType::Array(b, _)) => self.ctypes_compatible(a, b),
            _ => t1 == t2,
        }
    }

    // === Type helper methods ===

    /// Look up the pre-annotated CType for an expression from sema's expr_types map.
    /// This is O(1) and preserves signedness information (e.g. unsigned int from a cast).
    fn lookup_expr_type(&self, expr: &Expr) -> Option<CType> {
        let key = expr as *const Expr as usize;
        self.expr_types.and_then(|m| m.get(&key).cloned())
    }

    /// Derive a CType from an IrConst value.
    /// This is O(1) and avoids the potentially exponential infer_expr_ctype()
    /// when we already have the evaluated constant value.
    /// Note: This loses signedness info -- IrConst::I64 always maps to signed Long.
    fn ctype_from_ir_const(c: &IrConst) -> Option<CType> {
        match c {
            IrConst::I8(_) => Some(CType::Char),
            IrConst::I16(_) => Some(CType::Short),
            IrConst::I32(_) => Some(CType::Int),
            IrConst::I64(_) => Some(CType::Long),
            IrConst::I128(_) => Some(CType::Int128),
            IrConst::F32(_) => Some(CType::Float),
            IrConst::F64(_) => Some(CType::Double),
            IrConst::LongDouble(_) => Some(CType::LongDouble),
            IrConst::Zero => Some(CType::Int),
        }
    }

    /// Convert a TypeSpecifier to CType using sema's type resolution.
    fn type_spec_to_ctype(&self, spec: &TypeSpecifier) -> CType {
        // Handle typeof(expr) which requires expression type inference - the
        // standalone ctype_from_type_spec function can't handle this because it
        // lacks access to the symbol table and expression type checker.
        if let TypeSpecifier::Typeof(expr) = spec {
            return self.infer_expr_ctype(expr).unwrap_or(CType::Int);
        }
        // Delegate to a simple inline conversion for all other cases.
        ctype_from_type_spec(spec, self.types)
    }

    /// Infer the CType of an expression using ExprTypeChecker.
    fn infer_expr_ctype(&self, expr: &Expr) -> Option<CType> {
        let checker = super::type_checker::ExprTypeChecker {
            symbols: self.symbols,
            types: self.types,
            functions: self.functions,
            expr_types: self.expr_types,
        };
        checker.infer_expr_ctype(expr)
    }

    /// Get the size in bytes for a CType.
    fn ctype_size(&self, ctype: &CType) -> usize {
        ctype.size_ctx(&self.types.struct_layouts)
    }

    /// Promote a sub-int IrConst (I8/I16) to I32 for unary arithmetic,
    /// using unsigned zero-extension when the expression has an unsigned type.
    ///
    /// C11 6.3.1.1: Integer promotion converts unsigned char/short to int by
    /// zero-extending, and signed char/short by sign-extending. Without this,
    /// negate_const(I16(-1)) for unsigned short 65535 would compute -(-1) = 1
    /// instead of -(65535) = -65535.
    fn promote_sub_int_for_unary(&self, expr: &Expr, val: IrConst) -> IrConst {
        match &val {
            IrConst::I8(v) => {
                let is_unsigned = self.is_expr_unsigned(expr);
                if is_unsigned {
                    IrConst::I32(*v as u8 as i32)
                } else {
                    IrConst::I32(*v as i32)
                }
            }
            IrConst::I16(v) => {
                let is_unsigned = self.is_expr_unsigned(expr);
                if is_unsigned {
                    IrConst::I32(*v as u16 as i32)
                } else {
                    IrConst::I32(*v as i32)
                }
            }
            _ => val,
        }
    }

    /// Check if an expression has an unsigned type, using the expr_types cache
    /// or falling back to type inference from the AST structure.
    fn is_expr_unsigned(&self, expr: &Expr) -> bool {
        // First try the pre-annotated type from sema
        if let Some(ctype) = self.lookup_expr_type(expr) {
            return ctype.is_unsigned();
        }
        // For cast expressions, check the target type directly
        if let Expr::Cast(ref target_type, _, _) = expr {
            let ctype = self.type_spec_to_ctype(target_type);
            return ctype.is_unsigned();
        }
        // Fall back to type inference
        if let Some(ctype) = self.infer_expr_ctype(expr) {
            return ctype.is_unsigned();
        }
        false
    }

    /// Cast a float value to a target CType.
    fn cast_float_to_ctype(&self, fv: f64, target: &CType) -> Option<IrConst> {
        Some(match target {
            CType::Float => IrConst::F32(fv as f32),
            CType::Double => IrConst::F64(fv),
            CType::LongDouble => IrConst::LongDouble(fv),
            CType::Char => IrConst::I8(fv as i8),
            CType::UChar => IrConst::I32(fv as u8 as i32),
            CType::Short => IrConst::I16(fv as i16),
            CType::UShort => IrConst::I32(fv as u16 as i32),
            CType::Int => IrConst::I32(fv as i32),
            CType::UInt => IrConst::I32(fv as u32 as i32),
            CType::Long | CType::LongLong => IrConst::I64(fv as i64),
            CType::ULong | CType::ULongLong => IrConst::I64(fv as u64 as i64),
            CType::Bool => IrConst::I8(if fv != 0.0 { 1 } else { 0 }),
            _ => return None,
        })
    }

    /// Convert raw bits to an IrConst based on target CType.
    ///
    /// For unsigned sub-int types (UChar, UShort, Bool), we store the value
    /// as IrConst::I32 with zero-extension rather than I8/I16, because IrConst
    /// only has signed I8/I16 variants. Without this, `(unsigned short)(-1)` would
    /// be stored as `I16(-1)`, and `to_i64()` would sign-extend to -1 instead of
    /// 65535 -- causing `(unsigned short)(-1) << 1` to evaluate as -2 instead of
    /// 131070. This matches C's integer promotion (sub-int → int).
    fn bits_to_irconst(&self, bits: u64, target: &CType, target_signed: bool) -> Option<IrConst> {
        let size = self.ctype_size(target);
        Some(match size {
            1 => {
                if !target_signed {
                    // Unsigned char/bool: store as I32 to preserve unsigned value
                    // through integer promotion (e.g., (unsigned char)255 → I32(255), not I8(-1))
                    IrConst::I32(bits as u8 as i32)
                } else {
                    IrConst::I8(bits as i8)
                }
            }
            2 => {
                if !target_signed {
                    // Unsigned short: store as I32 to preserve unsigned value
                    // through integer promotion (e.g., (unsigned short)65535 → I32(65535), not I16(-1))
                    IrConst::I32(bits as u16 as i32)
                } else {
                    IrConst::I16(bits as i16)
                }
            }
            4 => {
                if matches!(target, CType::Float) {
                    let int_val = if target_signed { bits as i64 as f32 } else { bits as u64 as f32 };
                    IrConst::F32(int_val)
                } else if !target_signed {
                    // Unsigned 32-bit: use I64 to preserve unsigned semantics.
                    // IrConst::I32 is signed, so values >= 0x80000000 would sign-extend
                    // incorrectly when converted to i64 for 64-bit comparisons.
                    IrConst::I64(bits as u32 as i64)
                } else {
                    IrConst::I32(bits as i32)
                }
            }
            8 => {
                if matches!(target, CType::Double) {
                    let int_val = if target_signed { bits as i64 as f64 } else { bits as u64 as f64 };
                    IrConst::F64(int_val)
                } else {
                    IrConst::I64(bits as i64)
                }
            }
            16 => {
                if matches!(target, CType::LongDouble) {
                    let int_val = if target_signed { bits as i64 as f64 } else { bits as u64 as f64 };
                    IrConst::LongDouble(int_val)
                } else {
                    IrConst::I128(bits as i128) // __int128 / unsigned __int128
                }
            }
            _ => {
                // Pointer types and other 8-byte types
                if target.is_pointer_like() {
                    IrConst::I64(bits as i64)
                } else {
                    return None;
                }
            }
        })
    }

    /// Cast a full 128-bit integer value to a target CType.
    ///
    /// This handles casts from __int128/unsigned __int128 without going through the
    /// u64-based eval_const_expr_as_bits path, which would truncate the upper 64 bits.
    /// For targets <= 64 bits, extract the lower bits. For 128-bit targets, preserve
    /// the full value.
    fn cast_i128_to_ctype(&self, v128: i128, target: &CType, target_size: usize, src_unsigned: bool) -> Option<IrConst> {
        let bits_lo = v128 as u64; // lower 64 bits
        let target_signed = !target.is_unsigned() && !target.is_pointer_like();
        Some(match target_size {
            0 => return None, // void cast
            1 => {
                if !target_signed {
                    IrConst::I32(bits_lo as u8 as i32)
                } else {
                    IrConst::I8(bits_lo as i8)
                }
            }
            2 => {
                if !target_signed {
                    IrConst::I32(bits_lo as u16 as i32)
                } else {
                    IrConst::I16(bits_lo as i16)
                }
            }
            4 => {
                if matches!(target, CType::Float) {
                    // int-to-float: signedness comes from the source type
                    let fv = if src_unsigned { (v128 as u128) as f32 } else { v128 as f32 };
                    IrConst::F32(fv)
                } else if !target_signed {
                    IrConst::I64(bits_lo as u32 as i64)
                } else {
                    IrConst::I32(bits_lo as i32)
                }
            }
            8 => {
                if matches!(target, CType::Double) {
                    let fv = if src_unsigned { (v128 as u128) as f64 } else { v128 as f64 };
                    IrConst::F64(fv)
                } else {
                    IrConst::I64(bits_lo as i64)
                }
            }
            16 => {
                if matches!(target, CType::LongDouble) {
                    let fv = if src_unsigned { (v128 as u128) as f64 } else { v128 as f64 };
                    IrConst::LongDouble(fv)
                } else {
                    // __int128 / unsigned __int128: preserve full 128-bit value
                    IrConst::I128(v128)
                }
            }
            _ => {
                if target.is_pointer_like() {
                    IrConst::I64(bits_lo as i64)
                } else {
                    return None;
                }
            }
        })
    }

    /// Build an EnumType for a packed enum from its variants or type context.
    fn resolve_packed_enum_type(
        &self,
        name: &Option<String>,
        variants: &Option<Vec<crate::frontend::parser::ast::EnumVariant>>,
    ) -> crate::common::types::EnumType {
        // Try looking up previously registered packed enum
        if let Some(tag) = name {
            if let Some(et) = self.types.packed_enum_types.get(tag) {
                return et.clone();
            }
        }
        // Build from variant list
        let variant_values = if let Some(vars) = variants {
            let mut result = Vec::new();
            let mut next_val: i64 = 0;
            for v in vars {
                if let Some(ref val_expr) = v.value {
                    if let Some(val) = self.eval_const_expr(val_expr) {
                        next_val = val.to_i64().unwrap_or(next_val);
                    }
                }
                result.push((v.name.clone(), next_val));
                next_val += 1;
            }
            result
        } else {
            Vec::new()
        };
        crate::common::types::EnumType {
            name: name.clone(),
            variants: variant_values,
            is_packed: true,
        }
    }

    /// Compute sizeof for a type specifier.
    /// Returns None if the type cannot be sized (e.g., typeof(expr) without expr type info).
    fn sizeof_type_spec(&self, spec: &TypeSpecifier) -> Option<usize> {
        match spec {
            TypeSpecifier::Void => Some(0),
            TypeSpecifier::Char | TypeSpecifier::UnsignedChar => Some(1),
            TypeSpecifier::Short | TypeSpecifier::UnsignedShort => Some(2),
            TypeSpecifier::Int | TypeSpecifier::UnsignedInt
            | TypeSpecifier::Signed | TypeSpecifier::Unsigned => Some(4),
            TypeSpecifier::Long | TypeSpecifier::UnsignedLong
            | TypeSpecifier::LongLong | TypeSpecifier::UnsignedLongLong => Some(8),
            TypeSpecifier::Int128 | TypeSpecifier::UnsignedInt128 => Some(16),
            TypeSpecifier::Float => Some(4),
            TypeSpecifier::Double => Some(8),
            TypeSpecifier::LongDouble => Some(16),
            TypeSpecifier::Bool => Some(1),
            TypeSpecifier::ComplexFloat => Some(8),
            TypeSpecifier::ComplexDouble => Some(16),
            TypeSpecifier::ComplexLongDouble => Some(32),
            TypeSpecifier::Pointer(_, _) | TypeSpecifier::FunctionPointer(_, _, _) => Some(8),
            TypeSpecifier::Array(elem, Some(size)) => {
                let elem_size = self.sizeof_type_spec(elem)?;
                let n = self.eval_const_expr(size)?.to_i64()?;
                Some(elem_size * n as usize)
            }
            TypeSpecifier::Array(_, None) => Some(8), // incomplete array
            TypeSpecifier::Struct(tag, fields, is_packed, pragma_pack, struct_aligned) => {
                // Look up cached layout for tagged structs
                if let Some(tag) = tag {
                    let key = format!("struct.{}", tag);
                    if let Some(layout) = self.types.struct_layouts.get(&key) {
                        return Some(layout.size);
                    }
                }
                if let Some(fields) = fields {
                    let struct_fields = self.convert_struct_fields(fields);
                    if !struct_fields.is_empty() {
                        let max_field_align = if *is_packed { Some(1) } else { *pragma_pack };
                        let mut layout = crate::common::types::StructLayout::for_struct_with_packing(
                            &struct_fields, max_field_align, &self.types.struct_layouts
                        );
                        if let Some(a) = struct_aligned {
                            if *a > layout.align {
                                layout.align = *a;
                                let mask = layout.align - 1;
                                layout.size = (layout.size + mask) & !mask;
                            }
                        }
                        return Some(layout.size);
                    }
                }
                Some(0)
            }
            TypeSpecifier::Union(tag, fields, is_packed, pragma_pack, struct_aligned) => {
                if let Some(tag) = tag {
                    let key = format!("union.{}", tag);
                    if let Some(layout) = self.types.struct_layouts.get(&key) {
                        return Some(layout.size);
                    }
                }
                if let Some(fields) = fields {
                    let union_fields = self.convert_struct_fields(fields);
                    if !union_fields.is_empty() {
                        let mut layout = crate::common::types::StructLayout::for_union(
                            &union_fields, &self.types.struct_layouts
                        );
                        if *is_packed {
                            layout.align = 1;
                            layout.size = layout.fields.iter()
                                .map(|f| f.ty.size_ctx(&self.types.struct_layouts))
                                .max().unwrap_or(0);
                        } else if let Some(pack) = pragma_pack {
                            if *pack < layout.align {
                                layout.align = *pack;
                                let mask = layout.align - 1;
                                layout.size = (layout.size + mask) & !mask;
                            }
                        }
                        if let Some(a) = struct_aligned {
                            if *a > layout.align {
                                layout.align = *a;
                                let mask = layout.align - 1;
                                layout.size = (layout.size + mask) & !mask;
                            }
                        }
                        return Some(layout.size);
                    }
                }
                Some(0)
            }
            TypeSpecifier::Enum(name, variants, is_packed) => {
                // Check if this is a forward reference to a known packed enum
                let effective_packed = *is_packed || name.as_ref()
                    .and_then(|n| self.types.packed_enum_types.get(n))
                    .is_some();
                if !effective_packed {
                    Some(4)
                } else {
                    // Resolve the packed enum to get its correct size
                    let et = self.resolve_packed_enum_type(name, variants);
                    Some(et.packed_size())
                }
            }
            TypeSpecifier::TypedefName(name) => {
                if let Some(ctype) = self.types.typedefs.get(name) {
                    Some(ctype.size_ctx(&self.types.struct_layouts))
                } else {
                    Some(8) // fallback
                }
            }
            TypeSpecifier::TypeofType(inner) => self.sizeof_type_spec(inner),
            TypeSpecifier::Typeof(expr) => {
                let ctype = self.infer_expr_ctype(expr)?;
                Some(ctype.size_ctx(&self.types.struct_layouts))
            }
            _ => None,
        }
    }

    /// Compute sizeof for an expression via its CType.
    ///
    /// Special cases: string literals are arrays, not pointers, inside sizeof
    /// (C11 6.3.2.1p3: array-to-pointer decay does not apply to sizeof operands).
    fn sizeof_expr(&self, expr: &Expr) -> Option<usize> {
        match expr {
            // "hello" is char[6], not char* -- sizeof gives array size.
            // Use chars().count() because high-byte escape sequences like \xff
            // are stored as multi-byte UTF-8 in the Rust String but represent
            // single bytes in C.
            Expr::StringLiteral(s, _) => return Some(s.chars().count() + 1),
            // L"hello" is wchar_t[6] (int[6] on Linux) -- sizeof gives array size
            Expr::WideStringLiteral(s, _) => return Some((s.chars().count() + 1) * 4),
            // u"hello" is char16_t[6] (unsigned short[6]) -- sizeof gives array size
            Expr::Char16StringLiteral(s, _) => return Some((s.chars().count() + 1) * 2),
            _ => {}
        }
        let ctype = self.infer_expr_ctype(expr)?;
        Some(ctype.size_ctx(&self.types.struct_layouts))
    }

    /// Compute alignof for a type specifier.
    fn alignof_type_spec(&self, spec: &TypeSpecifier) -> usize {
        match spec {
            TypeSpecifier::Void | TypeSpecifier::Bool => 1,
            TypeSpecifier::Char | TypeSpecifier::UnsignedChar => 1,
            TypeSpecifier::Short | TypeSpecifier::UnsignedShort => 2,
            TypeSpecifier::Int | TypeSpecifier::UnsignedInt
            | TypeSpecifier::Signed | TypeSpecifier::Unsigned => 4,
            TypeSpecifier::Long | TypeSpecifier::UnsignedLong
            | TypeSpecifier::LongLong | TypeSpecifier::UnsignedLongLong => 8,
            TypeSpecifier::Int128 | TypeSpecifier::UnsignedInt128 => 16,
            TypeSpecifier::Float => 4,
            TypeSpecifier::Double => 8,
            TypeSpecifier::LongDouble => 16,
            TypeSpecifier::ComplexFloat => 4,
            TypeSpecifier::ComplexDouble => 8,
            TypeSpecifier::ComplexLongDouble => 16,
            TypeSpecifier::Pointer(_, _) | TypeSpecifier::FunctionPointer(_, _, _) => 8,
            TypeSpecifier::Array(elem, _) => self.alignof_type_spec(elem),
            TypeSpecifier::Struct(tag, fields, is_packed, pragma_pack, struct_aligned) => {
                if let Some(tag) = tag {
                    let key = format!("struct.{}", tag);
                    if let Some(layout) = self.types.struct_layouts.get(&key) {
                        return layout.align;
                    }
                }
                if let Some(fields) = fields {
                    let struct_fields = self.convert_struct_fields(fields);
                    if !struct_fields.is_empty() {
                        let max_field_align = if *is_packed { Some(1) } else { *pragma_pack };
                        let mut layout = crate::common::types::StructLayout::for_struct_with_packing(
                            &struct_fields, max_field_align, &self.types.struct_layouts
                        );
                        if let Some(a) = struct_aligned {
                            if *a > layout.align {
                                layout.align = *a;
                            }
                        }
                        return layout.align;
                    }
                }
                1
            }
            TypeSpecifier::Union(tag, fields, is_packed, _pragma_pack, struct_aligned) => {
                if let Some(tag) = tag {
                    let key = format!("union.{}", tag);
                    if let Some(layout) = self.types.struct_layouts.get(&key) {
                        return layout.align;
                    }
                }
                if let Some(fields) = fields {
                    let union_fields = self.convert_struct_fields(fields);
                    if !union_fields.is_empty() {
                        let mut layout = crate::common::types::StructLayout::for_union(
                            &union_fields, &self.types.struct_layouts
                        );
                        if *is_packed {
                            layout.align = 1;
                        }
                        if let Some(a) = struct_aligned {
                            if *a > layout.align {
                                layout.align = *a;
                            }
                        }
                        return layout.align;
                    }
                }
                1
            }
            TypeSpecifier::Enum(name, variants, is_packed) => {
                let effective_packed = *is_packed || name.as_ref()
                    .and_then(|n| self.types.packed_enum_types.get(n))
                    .is_some();
                if !effective_packed {
                    4
                } else {
                    let et = self.resolve_packed_enum_type(name, variants);
                    et.packed_size()
                }
            }
            TypeSpecifier::TypedefName(name) => {
                if let Some(ctype) = self.types.typedefs.get(name) {
                    ctype.align_ctx(&self.types.struct_layouts)
                } else {
                    8
                }
            }
            TypeSpecifier::TypeofType(inner) => self.alignof_type_spec(inner),
            TypeSpecifier::Typeof(expr) => {
                if let Some(ctype) = self.infer_expr_ctype(expr) {
                    ctype.align_ctx(&self.types.struct_layouts)
                } else {
                    8 // fallback
                }
            }
            _ => 8,
        }
    }

    /// Convert struct field declarations to StructField for layout computation.
    fn convert_struct_fields(&self, fields: &[StructFieldDecl]) -> Vec<crate::common::types::StructField> {
        fields.iter().filter_map(|f| {
            let ty = ctype_from_type_spec_with_derived(&f.type_spec, &f.derived, self.types);
            let name = f.name.clone().unwrap_or_default();
            let bit_width = f.bit_width.as_ref().and_then(|bw| {
                self.eval_const_expr(bw)?.to_i64().map(|v| v as u32)
            });
            Some(crate::common::types::StructField {
                name,
                ty,
                bit_width,
                alignment: f.alignment,
            })
        }).collect()
    }
}

/// Convert a TypeSpecifier to CType using the TypeContext for typedef/struct resolution.
/// This is a standalone function that doesn't need the full TypeConvertContext trait.
fn ctype_from_type_spec(spec: &TypeSpecifier, types: &TypeContext) -> CType {
    match spec {
        TypeSpecifier::Void => CType::Void,
        TypeSpecifier::Bool => CType::Bool,
        TypeSpecifier::Char => CType::Char,
        TypeSpecifier::UnsignedChar => CType::UChar,
        TypeSpecifier::Short => CType::Short,
        TypeSpecifier::UnsignedShort => CType::UShort,
        TypeSpecifier::Int | TypeSpecifier::Signed => CType::Int,
        TypeSpecifier::UnsignedInt | TypeSpecifier::Unsigned => CType::UInt,
        TypeSpecifier::Long => CType::Long,
        TypeSpecifier::UnsignedLong => CType::ULong,
        TypeSpecifier::LongLong => CType::LongLong,
        TypeSpecifier::UnsignedLongLong => CType::ULongLong,
        TypeSpecifier::Int128 => CType::Int128,
        TypeSpecifier::UnsignedInt128 => CType::UInt128,
        TypeSpecifier::Float => CType::Float,
        TypeSpecifier::Double => CType::Double,
        TypeSpecifier::LongDouble => CType::LongDouble,
        TypeSpecifier::Pointer(inner, addr_space) => CType::Pointer(Box::new(ctype_from_type_spec(inner, types)), *addr_space),
        TypeSpecifier::Array(elem, size) => {
            let elem_ty = ctype_from_type_spec(elem, types);
            // TODO: evaluate array size expression when available
            let array_size = size.as_ref().and_then(|s| {
                // Try simple literal evaluation for array sizes
                match s.as_ref() {
                    Expr::IntLiteral(n, _) | Expr::LongLiteral(n, _) => Some(*n as usize),
                    Expr::UIntLiteral(n, _) | Expr::ULongLiteral(n, _) => Some(*n as usize),
                    _ => None,
                }
            });
            CType::Array(Box::new(elem_ty), array_size)
        }
        TypeSpecifier::TypedefName(name) => {
            if let Some(resolved) = types.typedefs.get(name) {
                resolved.clone()
            } else {
                CType::Int // fallback
            }
        }
        TypeSpecifier::Struct(tag, _, _, _, _) => {
            if let Some(tag) = tag {
                CType::Struct(format!("struct.{}", tag).into())
            } else {
                CType::Int // anonymous struct without context
            }
        }
        TypeSpecifier::Union(tag, _, _, _, _) => {
            if let Some(tag) = tag {
                CType::Union(format!("union.{}", tag).into())
            } else {
                CType::Int // anonymous union without context
            }
        }
        TypeSpecifier::Enum(_, _, false) => CType::Int, // non-packed enums are int-sized
        TypeSpecifier::Enum(name, variants, true) => {
            // Packed enum: look up from type context or compute from variants
            if let Some(tag) = name {
                if let Some(et) = types.packed_enum_types.get(tag) {
                    return CType::Enum(et.clone());
                }
            }
            // Inline definition: compute from variants
            if let Some(vars) = variants {
                let mut variant_values = Vec::new();
                let mut next_val: i64 = 0;
                for v in vars {
                    if let Some(ref _val_expr) = v.value {
                        // Can't easily eval here, but this path is rare
                    }
                    variant_values.push((v.name.clone(), next_val));
                    next_val += 1;
                }
                CType::Enum(crate::common::types::EnumType {
                    name: name.clone(),
                    variants: variant_values,
                    is_packed: true,
                })
            } else {
                CType::Int // packed enum forward ref without definition
            }
        }
        TypeSpecifier::TypeofType(inner) => ctype_from_type_spec(inner, types),
        TypeSpecifier::FunctionPointer(_, _, _) => {
            CType::Pointer(Box::new(CType::Void), AddressSpace::Default) // function pointers are pointer-sized
        }
        _ => CType::Int, // fallback
    }
}

/// Convert a TypeSpecifier with derived declarators to CType.
fn ctype_from_type_spec_with_derived(
    spec: &TypeSpecifier,
    derived: &[DerivedDeclarator],
    types: &TypeContext,
) -> CType {
    let mut ty = ctype_from_type_spec(spec, types);
    if derived.is_empty() {
        return ty;
    }
    for d in derived {
        match d {
            DerivedDeclarator::Pointer => {
                ty = CType::Pointer(Box::new(ty), AddressSpace::Default);
            }
            DerivedDeclarator::Array(Some(size_expr)) => {
                let expr: &Expr = size_expr;
                let size = match expr {
                    Expr::IntLiteral(n, _) | Expr::LongLiteral(n, _) => Some(*n as usize),
                    Expr::UIntLiteral(n, _) | Expr::ULongLiteral(n, _) => Some(*n as usize),
                    _ => None,
                };
                ty = CType::Array(Box::new(ty), size);
            }
            DerivedDeclarator::Array(None) => {
                ty = CType::Array(Box::new(ty), None);
            }
            DerivedDeclarator::Function(_, _) | DerivedDeclarator::FunctionPointer(_, _) => {
                ty = CType::Pointer(Box::new(CType::Void), AddressSpace::Default); // function -> pointer
            }
        }
    }
    ty
}
