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
use crate::common::const_arith::{wrap_result, unsigned_op, bool_to_i64};
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
            // Integer literals
            Expr::IntLiteral(val, _) | Expr::LongLiteral(val, _) => {
                Some(IrConst::I64(*val))
            }
            Expr::UIntLiteral(val, _) | Expr::ULongLiteral(val, _) => {
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

            // Unary plus: identity
            Expr::UnaryOp(UnaryOp::Plus, inner, _) => {
                self.eval_const_expr(inner)
            }

            // Unary negation
            Expr::UnaryOp(UnaryOp::Neg, inner, _) => {
                let val = self.eval_const_expr(inner)?;
                match val {
                    IrConst::I64(v) => Some(IrConst::I64(-v)),
                    IrConst::I32(v) => Some(IrConst::I32(-v)),
                    // Integer promotion: sub-int types promote to int before negation
                    IrConst::I8(v) => Some(IrConst::I32(-(v as i32))),
                    IrConst::I16(v) => Some(IrConst::I32(-(v as i32))),
                    IrConst::F64(v) => Some(IrConst::F64(-v)),
                    IrConst::F32(v) => Some(IrConst::F32(-v)),
                    IrConst::LongDouble(v) => Some(IrConst::LongDouble(-v)),
                    _ => None,
                }
            }

            // Bitwise NOT
            Expr::UnaryOp(UnaryOp::BitNot, inner, _) => {
                let val = self.eval_const_expr(inner)?;
                match val {
                    IrConst::I64(v) => Some(IrConst::I64(!v)),
                    IrConst::I32(v) => Some(IrConst::I32(!v)),
                    // Integer promotion: sub-int types promote to int before complement
                    IrConst::I8(v) => Some(IrConst::I32(!(v as i32))),
                    IrConst::I16(v) => Some(IrConst::I32(!(v as i32))),
                    _ => None,
                }
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
                // Use CType to determine signedness and width for proper semantics
                let lhs_ctype = self.infer_expr_ctype(lhs);
                self.eval_const_binop(op, &l, &r, lhs_ctype.as_ref())
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
                let target_size = self.ctype_size(&target_ctype);
                let target_width = target_size * 8;
                let target_signed = !target_ctype.is_unsigned() && !target_ctype.is_pointer_like();

                // Truncate to target width
                let truncated = if target_width >= 64 || target_width == 0 {
                    bits
                } else {
                    bits & ((1u64 << target_width) - 1)
                };

                // If target is signed, sign-extend to 64 bits
                let result = if target_signed && target_width < 64 && target_width > 0 {
                    let sign_bit = 1u64 << (target_width - 1);
                    if truncated & sign_bit != 0 {
                        truncated | !((1u64 << target_width) - 1)
                    } else {
                        truncated
                    }
                } else {
                    truncated
                };

                Some((result, target_signed))
            }
            _ => {
                // For non-cast expressions, evaluate normally and convert to bits
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
    fn eval_const_binop(&self, op: &BinOp, lhs: &IrConst, rhs: &IrConst, lhs_ctype: Option<&CType>) -> Option<IrConst> {
        // Check if either operand is floating-point
        let lhs_is_float = matches!(lhs, IrConst::F32(_) | IrConst::F64(_) | IrConst::LongDouble(_));
        let rhs_is_float = matches!(rhs, IrConst::F32(_) | IrConst::F64(_) | IrConst::LongDouble(_));

        if lhs_is_float || rhs_is_float {
            return self.eval_const_binop_float(op, lhs, rhs);
        }

        let l = lhs.to_i64()?;
        let r = rhs.to_i64()?;

        // Determine if the LHS type is 32-bit or unsigned
        let (is_32bit, is_unsigned) = if let Some(ctype) = lhs_ctype {
            let size = self.ctype_size(ctype);
            (size <= 4, ctype.is_unsigned())
        } else {
            (false, false)
        };

        let result = match op {
            BinOp::Add => wrap_result(l.wrapping_add(r), is_32bit),
            BinOp::Sub => wrap_result(l.wrapping_sub(r), is_32bit),
            BinOp::Mul => wrap_result(l.wrapping_mul(r), is_32bit),
            BinOp::Div => {
                if r == 0 { return None; }
                if is_unsigned {
                    unsigned_op(l, r, is_32bit, u64::wrapping_div)
                } else {
                    wrap_result(l.wrapping_div(r), is_32bit)
                }
            }
            BinOp::Mod => {
                if r == 0 { return None; }
                if is_unsigned {
                    unsigned_op(l, r, is_32bit, u64::wrapping_rem)
                } else {
                    wrap_result(l.wrapping_rem(r), is_32bit)
                }
            }
            BinOp::BitAnd => l & r,
            BinOp::BitOr => l | r,
            BinOp::BitXor => l ^ r,
            BinOp::Shl => wrap_result(l.wrapping_shl(r as u32), is_32bit),
            BinOp::Shr => {
                if is_unsigned {
                    unsigned_op(l, r, is_32bit, |a, b| a.wrapping_shr(b as u32))
                } else if is_32bit {
                    (l as i32).wrapping_shr(r as u32) as i64
                } else {
                    l.wrapping_shr(r as u32)
                }
            }
            BinOp::Eq => bool_to_i64(l == r),
            BinOp::Ne => bool_to_i64(l != r),
            BinOp::Lt => {
                if is_unsigned { bool_to_i64((l as u64) < (r as u64)) }
                else { bool_to_i64(l < r) }
            }
            BinOp::Gt => {
                if is_unsigned { bool_to_i64((l as u64) > (r as u64)) }
                else { bool_to_i64(l > r) }
            }
            BinOp::Le => {
                if is_unsigned { bool_to_i64((l as u64) <= (r as u64)) }
                else { bool_to_i64(l <= r) }
            }
            BinOp::Ge => {
                if is_unsigned { bool_to_i64((l as u64) >= (r as u64)) }
                else { bool_to_i64(l >= r) }
            }
            BinOp::LogicalAnd => bool_to_i64(l != 0 && r != 0),
            BinOp::LogicalOr => bool_to_i64(l != 0 || r != 0),
        };
        Some(IrConst::I64(result))
    }

    /// Evaluate a binary operation on floating-point constant operands.
    /// Promotes both operands to the wider float type (f64 if either is f64,
    /// LongDouble if either is LongDouble).
    fn eval_const_binop_float(&self, op: &BinOp, lhs: &IrConst, rhs: &IrConst) -> Option<IrConst> {
        let use_long_double = matches!(lhs, IrConst::LongDouble(_)) || matches!(rhs, IrConst::LongDouble(_));
        let use_f32 = matches!(lhs, IrConst::F32(_)) && matches!(rhs, IrConst::F32(_));

        let l = lhs.to_f64()?;
        let r = rhs.to_f64()?;

        let make_result = |v: f64| -> IrConst {
            if use_long_double {
                IrConst::LongDouble(v)
            } else if use_f32 {
                IrConst::F32(v as f32)
            } else {
                IrConst::F64(v)
            }
        };

        match op {
            BinOp::Add => Some(make_result(l + r)),
            BinOp::Sub => Some(make_result(l - r)),
            BinOp::Mul => Some(make_result(l * r)),
            BinOp::Div => Some(make_result(l / r)),
            BinOp::Eq => Some(IrConst::I64(if l == r { 1 } else { 0 })),
            BinOp::Ne => Some(IrConst::I64(if l != r { 1 } else { 0 })),
            BinOp::Lt => Some(IrConst::I64(if l < r { 1 } else { 0 })),
            BinOp::Gt => Some(IrConst::I64(if l > r { 1 } else { 0 })),
            BinOp::Le => Some(IrConst::I64(if l <= r { 1 } else { 0 })),
            BinOp::Ge => Some(IrConst::I64(if l >= r { 1 } else { 0 })),
            BinOp::LogicalAnd => Some(IrConst::I64(if l != 0.0 && r != 0.0 { 1 } else { 0 })),
            BinOp::LogicalOr => Some(IrConst::I64(if l != 0.0 || r != 0.0 { 1 } else { 0 })),
            _ => None, // Bitwise/shift not valid on floats
        }
    }

    /// Evaluate the offsetof pattern: &((type*)0)->member
    fn eval_offsetof_pattern(&self, expr: &Expr) -> Option<IrConst> {
        match expr {
            Expr::PointerMemberAccess(base, field_name, _) => {
                let (type_spec, base_offset) = self.extract_null_pointer_cast_with_offset(base)?;
                let layout = self.get_struct_layout_for_type(&type_spec)?;
                let (field_offset, _field_ty) = layout.field_offset(field_name, self.types)?;
                Some(IrConst::I64((base_offset + field_offset) as i64))
            }
            Expr::MemberAccess(base, field_name, _) => {
                if let Expr::Deref(inner, _) = base.as_ref() {
                    let (type_spec, base_offset) = self.extract_null_pointer_cast_with_offset(inner)?;
                    let layout = self.get_struct_layout_for_type(&type_spec)?;
                    let (field_offset, _field_ty) = layout.field_offset(field_name, self.types)?;
                    Some(IrConst::I64((base_offset + field_offset) as i64))
                } else {
                    None
                }
            }
            Expr::ArraySubscript(base, index, _) => {
                let base_offset = self.eval_offsetof_pattern(base)?;
                if let IrConst::I64(boff) = base_offset {
                    if let Some(idx_val) = self.eval_const_expr(index) {
                        if let IrConst::I64(idx) = idx_val {
                            // Get element size from the base expression's type
                            if let Some(ctype) = self.infer_expr_ctype(base) {
                                let elem_size = match &ctype {
                                    CType::Array(elem, _) => elem.size_ctx(&self.types.struct_layouts),
                                    _ => return None,
                                };
                                return Some(IrConst::I64(boff + idx * elem_size as i64));
                            }
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Extract the struct type from a (type*)0 pattern.
    fn extract_null_pointer_cast_with_offset(&self, expr: &Expr) -> Option<(TypeSpecifier, usize)> {
        match expr {
            Expr::Cast(ref type_spec, inner, _) => {
                if let TypeSpecifier::Pointer(inner_ts) = type_spec {
                    if self.is_zero_expr(inner) {
                        return Some((*inner_ts.clone(), 0));
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Check if an expression evaluates to 0.
    fn is_zero_expr(&self, expr: &Expr) -> bool {
        match expr {
            Expr::IntLiteral(0, _) | Expr::UIntLiteral(0, _)
            | Expr::LongLiteral(0, _) | Expr::ULongLiteral(0, _) => true,
            Expr::Cast(_, inner, _) => self.is_zero_expr(inner),
            _ => false,
        }
    }

    /// Get the struct layout for a type specifier.
    fn get_struct_layout_for_type(&self, type_spec: &TypeSpecifier) -> Option<crate::common::types::StructLayout> {
        let ctype = self.type_spec_to_ctype(type_spec);
        match &ctype {
            CType::Struct(key) | CType::Union(key) => {
                self.types.struct_layouts.get(key).cloned()
            }
            _ => None,
        }
    }

    /// Check if two CTypes are compatible (for __builtin_types_compatible_p).
    fn ctypes_compatible(&self, t1: &CType, t2: &CType) -> bool {
        // Strip qualifiers (CType doesn't carry them) and compare
        match (t1, t2) {
            (CType::Pointer(a), CType::Pointer(b)) => self.ctypes_compatible(a, b),
            (CType::Array(a, _), CType::Array(b, _)) => self.ctypes_compatible(a, b),
            _ => t1 == t2,
        }
    }

    // === Type helper methods ===

    /// Convert a TypeSpecifier to CType using sema's type resolution.
    fn type_spec_to_ctype(&self, spec: &TypeSpecifier) -> CType {
        // Delegate to a simple inline conversion. For sema const_eval we need
        // the basic type mapping but don't need the full TypeConvertContext trait.
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

    /// Cast a float value to a target CType.
    fn cast_float_to_ctype(&self, fv: f64, target: &CType) -> Option<IrConst> {
        Some(match target {
            CType::Float => IrConst::F32(fv as f32),
            CType::Double => IrConst::F64(fv),
            CType::LongDouble => IrConst::LongDouble(fv),
            CType::Char => IrConst::I8(fv as i8),
            CType::UChar => IrConst::I8(fv as u8 as i8),
            CType::Short => IrConst::I16(fv as i16),
            CType::UShort => IrConst::I16(fv as u16 as i16),
            CType::Int => IrConst::I32(fv as i32),
            CType::UInt => IrConst::I32(fv as u32 as i32),
            CType::Long | CType::LongLong => IrConst::I64(fv as i64),
            CType::ULong | CType::ULongLong => IrConst::I64(fv as u64 as i64),
            CType::Bool => IrConst::I8(if fv != 0.0 { 1 } else { 0 }),
            _ => return None,
        })
    }

    /// Convert raw bits to an IrConst based on target CType.
    fn bits_to_irconst(&self, bits: u64, target: &CType, target_signed: bool) -> Option<IrConst> {
        let size = self.ctype_size(target);
        Some(match size {
            1 => IrConst::I8(bits as i8),
            2 => IrConst::I16(bits as i16),
            4 => {
                if matches!(target, CType::Float) {
                    let int_val = if target_signed { bits as i64 as f32 } else { bits as u64 as f32 };
                    IrConst::F32(int_val)
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
                    IrConst::I64(bits as i64) // fallback for __int128 etc
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
            TypeSpecifier::Pointer(_) | TypeSpecifier::FunctionPointer(_, _, _) => Some(8),
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
            TypeSpecifier::Enum(_, _) => Some(4),
            TypeSpecifier::TypedefName(name) => {
                if let Some(ctype) = self.types.typedefs.get(name) {
                    Some(ctype.size_ctx(&self.types.struct_layouts))
                } else {
                    Some(8) // fallback
                }
            }
            TypeSpecifier::TypeofType(inner) => self.sizeof_type_spec(inner),
            _ => None,
        }
    }

    /// Compute sizeof for an expression via its CType.
    fn sizeof_expr(&self, expr: &Expr) -> Option<usize> {
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
            TypeSpecifier::Pointer(_) | TypeSpecifier::FunctionPointer(_, _, _) => 8,
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
            TypeSpecifier::Enum(_, _) => 4,
            TypeSpecifier::TypedefName(name) => {
                if let Some(ctype) = self.types.typedefs.get(name) {
                    ctype.align_ctx(&self.types.struct_layouts)
                } else {
                    8
                }
            }
            TypeSpecifier::TypeofType(inner) => self.alignof_type_spec(inner),
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
        TypeSpecifier::Pointer(inner) => CType::Pointer(Box::new(ctype_from_type_spec(inner, types))),
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
                CType::Struct(format!("struct.{}", tag))
            } else {
                CType::Int // anonymous struct without context
            }
        }
        TypeSpecifier::Union(tag, _, _, _, _) => {
            if let Some(tag) = tag {
                CType::Union(format!("union.{}", tag))
            } else {
                CType::Int // anonymous union without context
            }
        }
        TypeSpecifier::Enum(_, _) => CType::Int, // enums are int-sized
        TypeSpecifier::TypeofType(inner) => ctype_from_type_spec(inner, types),
        TypeSpecifier::FunctionPointer(_, _, _) => {
            CType::Pointer(Box::new(CType::Void)) // function pointers are pointer-sized
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
                ty = CType::Pointer(Box::new(ty));
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
                ty = CType::Pointer(Box::new(CType::Void)); // function -> pointer
            }
        }
    }
    ty
}
