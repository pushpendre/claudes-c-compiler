//! Algebraic simplification and strength reduction pass.
//!
//! Applies algebraic identities to simplify instructions (integer types):
//! - x + 0 => x, 0 + x => x
//! - x - 0 => x, x - x => 0
//! - x * 0 => 0, x * 1 => x
//! - x / 1 => x, x / x => 1
//! - x % 1 => 0, x % x => 0
//! - x & 0 => 0, x & x => x
//! - x | 0 => x, x | x => x
//! - x ^ 0 => x, x ^ x => 0
//! - x << 0 => x, x >> 0 => x
//!
//! Float-unsafe simplifications (x + 0, 0 + x, x * 0, x - x, x / x) are
//! restricted to integer types to preserve IEEE 754 semantics (signed zeros,
//! NaN propagation, infinity arithmetic).
//!
//! Strength reduction (integer types only):
//! - x * 2^k => x << k  (multiply by power-of-2 to shift)
//! - x * 2 => x + x     (slightly cheaper on some uarches)
//! - x / 2^k => x >> k  (unsigned divide by power-of-2 to logical shift)
//!
//! Redundant instruction elimination:
//! - Cast where from_ty == to_ty => Copy
//! - GetElementPtr chain: GEP(GEP(base, c1), c2) => GEP(base, c1+c2)
//! - GetElementPtr with constant zero offset => Copy of base
//!
//! Cast chain optimization (requires def lookup):
//! - Cast(Cast(x, A->B), B->A) where A fits in B => Copy of x (widen-then-narrow)
//! - Cast(Cast(x, A->B), B->C) => Cast(x, A->C) (double widen/narrow)
//! - Cast of constant => constant (fold at compile time)

use crate::common::types::IrType;
use crate::ir::ir::*;

/// Run algebraic simplification on the module.
/// Returns the number of instructions simplified.
pub fn run(module: &mut IrModule) -> usize {
    module.for_each_function(simplify_function)
}

fn simplify_function(func: &mut IrFunction) -> usize {
    let mut total = 0;

    // Build def maps for chain optimizations: Value -> defining instruction
    // We use flat Vecs indexed by Value ID for O(1) lookup.
    let max_id = func.max_value_id() as usize;
    let mut cast_defs: Vec<Option<CastDef>> = vec![None; max_id + 1];
    let mut gep_defs: Vec<Option<GepDef>> = vec![None; max_id + 1];

    // Collect definitions
    for block in &func.blocks {
        for inst in &block.instructions {
            match inst {
                Instruction::Cast { dest, src, from_ty, to_ty } => {
                    let id = dest.0 as usize;
                    if id < cast_defs.len() {
                        cast_defs[id] = Some(CastDef {
                            src: *src,
                            from_ty: *from_ty,
                            to_ty: *to_ty,
                        });
                    }
                }
                Instruction::GetElementPtr { dest, base, offset, .. } => {
                    let id = dest.0 as usize;
                    if id < gep_defs.len() {
                        gep_defs[id] = Some(GepDef {
                            base: *base,
                            offset: *offset,
                        });
                    }
                }
                _ => {}
            }
        }
    }

    for block in &mut func.blocks {
        for inst in &mut block.instructions {
            if let Some(simplified) = try_simplify(inst, &cast_defs, &gep_defs) {
                *inst = simplified;
                total += 1;
            }
        }
    }
    total
}

/// Cached information about a Cast instruction for chain elimination.
#[derive(Clone, Copy)]
struct CastDef {
    src: Operand,
    from_ty: IrType,
    to_ty: IrType,
}

/// Cached information about a GEP instruction for chain folding.
#[derive(Clone, Copy)]
struct GepDef {
    base: Value,
    offset: Operand,
}

/// Try to simplify an instruction using algebraic identities and strength reduction.
fn try_simplify(
    inst: &Instruction,
    cast_defs: &[Option<CastDef>],
    gep_defs: &[Option<GepDef>],
) -> Option<Instruction> {
    match inst {
        Instruction::BinOp { dest, op, lhs, rhs, ty } => {
            simplify_binop(*dest, *op, lhs, rhs, *ty)
        }
        Instruction::Cast { dest, src, from_ty, to_ty } => {
            simplify_cast(*dest, src, *from_ty, *to_ty, cast_defs)
        }
        Instruction::GetElementPtr { dest, base, offset, ty } => {
            simplify_gep(*dest, *base, offset, *ty, gep_defs)
        }
        Instruction::Select { dest, true_val, false_val, .. } => {
            // select cond, x, x => x (both arms are the same)
            if same_operand(true_val, false_val) {
                return Some(Instruction::Copy { dest: *dest, src: *true_val });
            }
            None
        }
        _ => None,
    }
}

/// Simplify a Cast instruction.
///
/// Handles:
/// - Cast from type T to same type T => Copy
/// - Cast chain: Cast(Cast(x, A->B), B->C) optimizations
/// - Cast of constant => constant (fold at compile time)
fn simplify_cast(
    dest: Value,
    src: &Operand,
    from_ty: IrType,
    to_ty: IrType,
    def_map: &[Option<CastDef>],
) -> Option<Instruction> {
    // Identity cast: same type
    if from_ty == to_ty {
        return Some(Instruction::Copy { dest, src: *src });
    }

    // Constant folding: cast of a constant => new constant
    if let Operand::Const(c) = src {
        if let Some(folded) = fold_const_cast(c, from_ty, to_ty) {
            return Some(Instruction::Copy {
                dest,
                src: Operand::Const(folded),
            });
        }
    }

    // Cast chain optimization: if src is defined by another Cast, try to fold.
    // Only handle the widen-then-narrow-back case which is safe and common.
    if let Operand::Value(v) = src {
        let idx = v.0 as usize;
        if let Some(Some(inner_cast)) = def_map.get(idx) {
            let inner_from = inner_cast.from_ty;
            let inner_to = inner_cast.to_ty;
            let inner_src = inner_cast.src;

            // Verify chain consistency: inner output type must match our input type
            if inner_to == from_ty {
                // Widen then narrow back to exact same type (most common C pattern).
                // E.g., Cast(Cast(x:I32, I32->I64), I64->I32) => Copy of x
                // Safe because widening preserves all bits, then narrowing discards
                // the high bits we added - yielding the original value unchanged.
                // The size guard (inner_from.size() <= from_ty.size()) ensures the
                // first cast was a widening, not a narrowing (which loses bits).
                if inner_from == to_ty
                    && inner_from.is_integer() && from_ty.is_integer()
                    && inner_from.size() <= from_ty.size()
                {
                    return Some(Instruction::Copy { dest, src: inner_src });
                }

                // Double widen: Cast(Cast(x, A->B), B->C) where A < B < C (all ints)
                // => Cast(x, A->C) - skip intermediate.
                // ONLY safe when A and B have the same signedness, because the
                // extension from B to C uses B's signedness. If we skip B and go
                // directly A->C, the extension uses A's signedness instead. When
                // A and B differ in signedness (e.g., I8->U16->I32), the
                // intermediate unsigned type changes the extension behavior and
                // skipping it would be incorrect.
                if inner_from.is_integer() && from_ty.is_integer() && to_ty.is_integer() {
                    let a = inner_from.size();
                    let b = from_ty.size();
                    let c = to_ty.size();
                    if a < b && b < c {
                        // Only safe when inner source and intermediate have same signedness
                        let same_sign = inner_from.is_signed() == from_ty.is_signed()
                            || inner_from.is_unsigned() == from_ty.is_unsigned();
                        if same_sign {
                            return Some(Instruction::Cast {
                                dest,
                                src: inner_src,
                                from_ty: inner_from,
                                to_ty,
                            });
                        }
                    }

                    // Double narrow: Cast(Cast(x, A->B), B->C) where A > B > C (all ints)
                    // => Cast(x, A->C) - skip intermediate narrow.
                    // Safe because narrowing only keeps the low bits, so
                    // narrow(narrow(x, A->B), B->C) = narrow(x, A->C) regardless
                    // of signedness (both just truncate to C bits).
                    if a > b && b > c {
                        return Some(Instruction::Cast {
                            dest,
                            src: inner_src,
                            from_ty: inner_from,
                            to_ty,
                        });
                    }
                }
            }
        }
    }

    None
}

/// Fold a cast of a constant at compile time.
/// Returns the new constant if the cast can be folded, None otherwise.
///
/// `from_ty` is needed because IrConst::to_i64() always sign-extends from
/// the storage type (I8/I16/I32/I64), but unsigned source types require
/// zero-extension. For example, IrConst::I8(-1) with from_ty=U8 represents
/// the value 255, not -1.
fn fold_const_cast(c: &IrConst, from_ty: IrType, to_ty: IrType) -> Option<IrConst> {
    // Integer-to-integer/float constant cast
    if let Some(raw_val) = c.to_i64() {
        // Normalize source value according to from_ty signedness.
        // Signed types sign-extend; unsigned types zero-extend.
        let val = match from_ty {
            IrType::I8 => raw_val as i8 as i64,
            IrType::U8 => raw_val as u8 as i64,
            IrType::I16 => raw_val as i16 as i64,
            IrType::U16 => raw_val as u16 as i64,
            IrType::I32 => raw_val as i32 as i64,
            IrType::U32 => raw_val as u32 as i64,
            _ => raw_val,
        };

        return Some(match to_ty {
            IrType::I8 | IrType::U8 => IrConst::I8(val as i8),
            IrType::I16 | IrType::U16 => IrConst::I16(val as i16),
            IrType::I32 => IrConst::I32(val as i32),
            // U32: keep as I64 with zero-extended value so that loading into
            // a 64-bit register preserves the unsigned 32-bit value. If we
            // stored as I32(val as i32), the constant load would sign-extend
            // (e.g. -13 → 0xFFFFFFFFFFFFFFF3) which corrupts 64-bit unsigned
            // operations like divq.
            IrType::U32 => IrConst::I64(val as u32 as i64),
            IrType::I64 | IrType::U64 | IrType::Ptr => IrConst::I64(val),
            IrType::I128 | IrType::U128 => {
                if from_ty.is_unsigned() {
                    IrConst::I128(val as u64 as u128 as i128)
                } else {
                    IrConst::I128(val as i128)
                }
            }
            IrType::F32 => {
                if from_ty.is_unsigned() {
                    IrConst::F32(val as u64 as f32)
                } else {
                    IrConst::F32(val as f32)
                }
            }
            IrType::F64 => {
                if from_ty.is_unsigned() {
                    IrConst::F64(val as u64 as f64)
                } else {
                    IrConst::F64(val as f64)
                }
            }
            IrType::F128 => {
                if from_ty.is_unsigned() {
                    IrConst::LongDouble(val as u64 as f64)
                } else {
                    IrConst::LongDouble(val as f64)
                }
            }
            _ => return None,
        });
    }

    // Float-to-other constant cast
    if let Some(fval) = c.to_f64() {
        return IrConst::cast_float_to_target(fval, to_ty);
    }

    None
}

/// Simplify a GetElementPtr instruction.
///
/// - GEP with constant zero offset => Copy of base pointer
/// - GEP chain: GEP(GEP(base, c1), c2) => GEP(base, c1 + c2) when both offsets are constants
fn simplify_gep(
    dest: Value,
    base: Value,
    offset: &Operand,
    ty: IrType,
    gep_defs: &[Option<GepDef>],
) -> Option<Instruction> {
    if is_zero(offset) {
        return Some(Instruction::Copy { dest, src: Operand::Value(base) });
    }

    // GEP chain folding: GEP(GEP(inner_base, c1), c2) => GEP(inner_base, c1 + c2)
    // Only safe when both offsets are constants (we can compute the sum at compile time).
    if let Operand::Const(outer_c) = offset {
        if let Some(outer_val) = outer_c.to_i64() {
            let base_idx = base.0 as usize;
            if let Some(Some(inner_gep)) = gep_defs.get(base_idx) {
                if let Operand::Const(inner_c) = &inner_gep.offset {
                    if let Some(inner_val) = inner_c.to_i64() {
                        let combined = inner_val.wrapping_add(outer_val);
                        if combined == 0 {
                            // Combined offset is zero => just copy the original base
                            return Some(Instruction::Copy {
                                dest,
                                src: Operand::Value(inner_gep.base),
                            });
                        }
                        return Some(Instruction::GetElementPtr {
                            dest,
                            base: inner_gep.base,
                            offset: Operand::Const(IrConst::I64(combined)),
                            ty,
                        });
                    }
                }
            }
        }
    }

    None
}

/// Return the log2 of a positive i64 value if it is a power of 2, or None otherwise.
fn const_power_of_two(op: &Operand) -> Option<u32> {
    match op {
        Operand::Const(c) => {
            let val = c.to_i64()?;
            if val > 0 && (val & (val - 1)) == 0 {
                Some(val.trailing_zeros())
            } else {
                None
            }
        }
        _ => None,
    }
}

fn simplify_binop(
    dest: Value,
    op: IrBinOp,
    lhs: &Operand,
    rhs: &Operand,
    ty: IrType,
) -> Option<Instruction> {
    let lhs_zero = is_zero(lhs);
    let rhs_zero = is_zero(rhs);
    let lhs_one = is_one(lhs);
    let rhs_one = is_one(rhs);
    let same_value = same_value_operands(lhs, rhs);
    let is_float = ty.is_float();

    match op {
        IrBinOp::Add => {
            // For floats, x + 0 => x is invalid because -0.0 + 0.0 = +0.0, not -0.0.
            // IEEE 754: the sign of a sum differs from at most one addend.
            if !is_float {
                if rhs_zero {
                    // x + 0 => x (integers only)
                    return Some(Instruction::Copy { dest, src: *lhs });
                }
                if lhs_zero {
                    // 0 + x => x (integers only)
                    return Some(Instruction::Copy { dest, src: *rhs });
                }
            }
        }
        IrBinOp::Sub => {
            if rhs_zero && (!is_float || is_positive_zero(rhs)) {
                // x - 0 => x (for floats, only valid when rhs is +0.0; -0.0 - (-0.0) = +0.0)
                return Some(Instruction::Copy { dest, src: *lhs });
            }
            if !is_float && same_value {
                // x - x => 0 (integers only; for floats, Inf - Inf = NaN)
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::zero(ty)),
                });
            }
        }
        IrBinOp::Mul => {
            if !is_float && (rhs_zero || lhs_zero) {
                // x * 0 or 0 * x => 0 (integers only; for floats, NaN*0=NaN, -x*0=-0)
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::zero(ty)),
                });
            }
            if rhs_one {
                // x * 1 => x (valid for both int and float)
                return Some(Instruction::Copy { dest, src: *lhs });
            }
            if lhs_one {
                // 1 * x => x (valid for both int and float)
                return Some(Instruction::Copy { dest, src: *rhs });
            }
            // Strength reduction: multiply by power-of-2 => shift left
            // Only for integer types (not floats)
            if ty.is_integer() || ty == IrType::Ptr {
                if let Some(shift) = const_power_of_two(rhs) {
                    if shift == 1 {
                        // x * 2 => x + x
                        return Some(Instruction::BinOp {
                            dest,
                            op: IrBinOp::Add,
                            lhs: *lhs,
                            rhs: *lhs,
                            ty,
                        });
                    }
                    // x * 2^k => x << k
                    return Some(Instruction::BinOp {
                        dest,
                        op: IrBinOp::Shl,
                        lhs: *lhs,
                        rhs: Operand::Const(IrConst::from_i64(shift as i64, ty)),
                        ty,
                    });
                }
                if let Some(shift) = const_power_of_two(lhs) {
                    if shift == 1 {
                        // 2 * x => x + x
                        return Some(Instruction::BinOp {
                            dest,
                            op: IrBinOp::Add,
                            lhs: *rhs,
                            rhs: *rhs,
                            ty,
                        });
                    }
                    // 2^k * x => x << k
                    return Some(Instruction::BinOp {
                        dest,
                        op: IrBinOp::Shl,
                        lhs: *rhs,
                        rhs: Operand::Const(IrConst::from_i64(shift as i64, ty)),
                        ty,
                    });
                }
            }
        }
        IrBinOp::SDiv | IrBinOp::UDiv => {
            if rhs_one {
                // x / 1 => x (valid for both int and float)
                return Some(Instruction::Copy { dest, src: *lhs });
            }
            if !is_float && same_value {
                // x / x => 1 (integers only; for floats, 0/0=NaN, Inf/Inf=NaN)
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::one(ty)),
                });
            }
            // Strength reduction: unsigned divide by power-of-2 => logical shift right
            if op == IrBinOp::UDiv {
                if (ty.is_integer() || ty == IrType::Ptr) && ty.is_unsigned() {
                    if let Some(shift) = const_power_of_two(rhs) {
                        return Some(Instruction::BinOp {
                            dest,
                            op: IrBinOp::LShr,
                            lhs: *lhs,
                            rhs: Operand::Const(IrConst::from_i64(shift as i64, ty)),
                            ty,
                        });
                    }
                }
            }
        }
        IrBinOp::SRem | IrBinOp::URem => {
            if rhs_one {
                // x % 1 => 0
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::zero(ty)),
                });
            }
            if same_value {
                // x % x => 0
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::zero(ty)),
                });
            }
            // Strength reduction: unsigned rem by power-of-2 => bitwise AND with mask
            if op == IrBinOp::URem {
                if (ty.is_integer() || ty == IrType::Ptr) && ty.is_unsigned() {
                    if let Some(shift) = const_power_of_two(rhs) {
                        // x % 2^k => x & (2^k - 1)
                        let mask = (1i64 << shift) - 1;
                        return Some(Instruction::BinOp {
                            dest,
                            op: IrBinOp::And,
                            lhs: *lhs,
                            rhs: Operand::Const(IrConst::from_i64(mask, ty)),
                            ty,
                        });
                    }
                }
            }
        }
        IrBinOp::And => {
            if rhs_zero || lhs_zero {
                // x & 0 => 0
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::zero(ty)),
                });
            }
            if same_value {
                // x & x => x
                return Some(Instruction::Copy { dest, src: *lhs });
            }
        }
        IrBinOp::Or => {
            if rhs_zero {
                // x | 0 => x
                return Some(Instruction::Copy { dest, src: *lhs });
            }
            if lhs_zero {
                // 0 | x => x
                return Some(Instruction::Copy { dest, src: *rhs });
            }
            if same_value {
                // x | x => x
                return Some(Instruction::Copy { dest, src: *lhs });
            }
        }
        IrBinOp::Xor => {
            if rhs_zero {
                // x ^ 0 => x
                return Some(Instruction::Copy { dest, src: *lhs });
            }
            if lhs_zero {
                // 0 ^ x => x
                return Some(Instruction::Copy { dest, src: *rhs });
            }
            if same_value {
                // x ^ x => 0
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::zero(ty)),
                });
            }
        }
        IrBinOp::Shl | IrBinOp::AShr | IrBinOp::LShr => {
            if rhs_zero {
                // x << 0 or x >> 0 => x
                return Some(Instruction::Copy { dest, src: *lhs });
            }
            if lhs_zero {
                // 0 << x or 0 >> x => 0
                return Some(Instruction::Copy {
                    dest,
                    src: Operand::Const(IrConst::zero(ty)),
                });
            }
        }
    }

    None
}

/// Check if an operand is zero (including both +0.0 and -0.0 for floats).
fn is_zero(op: &Operand) -> bool {
    matches!(op, Operand::Const(c) if c.is_zero())
}

/// Check if a float operand is positive zero (+0.0), using the bit pattern.
/// Returns false for -0.0, non-zero values, and non-float types.
fn is_positive_zero(op: &Operand) -> bool {
    match op {
        Operand::Const(IrConst::F32(v)) => *v == 0.0 && !v.is_sign_negative(),
        Operand::Const(IrConst::F64(v)) => *v == 0.0 && !v.is_sign_negative(),
        Operand::Const(IrConst::LongDouble(v)) => *v == 0.0 && !v.is_sign_negative(),
        _ => false,
    }
}

/// Check if an operand is one.
fn is_one(op: &Operand) -> bool {
    matches!(op, Operand::Const(c) if c.is_one())
}

/// Check if two operands refer to the same value.
fn same_value_operands(lhs: &Operand, rhs: &Operand) -> bool {
    match (lhs, rhs) {
        (Operand::Value(a), Operand::Value(b)) => a.0 == b.0,
        _ => false,
    }
}

/// Check if two operands are identical (same value or same constant).
fn same_operand(a: &Operand, b: &Operand) -> bool {
    match (a, b) {
        (Operand::Value(va), Operand::Value(vb)) => va.0 == vb.0,
        (Operand::Const(ca), Operand::Const(cb)) => ca.to_hash_key() == cb.to_hash_key(),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn no_defs() -> Vec<Option<CastDef>> {
        vec![]
    }

    fn no_gep_defs() -> Vec<Option<GepDef>> {
        vec![]
    }

    #[test]
    fn test_add_zero() {
        let empty_defs = no_defs();
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Add,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I32(0)),
            ty: IrType::I32,
        };
        let result = try_simplify(&inst, &empty_defs, &no_gep_defs()).unwrap();
        match result {
            Instruction::Copy { src: Operand::Value(v), .. } => assert_eq!(v.0, 1),
            _ => panic!("Expected Copy"),
        }
    }

    #[test]
    fn test_mul_zero() {
        let empty_defs = no_defs();
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Mul,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I32(0)),
            ty: IrType::I32,
        };
        let result = try_simplify(&inst, &empty_defs, &no_gep_defs()).unwrap();
        match result {
            Instruction::Copy { src: Operand::Const(IrConst::I32(0)), .. } => {}
            _ => panic!("Expected Copy with zero"),
        }
    }

    #[test]
    fn test_mul_one() {
        let empty_defs = no_defs();
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Mul,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I32(1)),
            ty: IrType::I32,
        };
        let result = try_simplify(&inst, &empty_defs, &no_gep_defs()).unwrap();
        match result {
            Instruction::Copy { src: Operand::Value(v), .. } => assert_eq!(v.0, 1),
            _ => panic!("Expected Copy of lhs"),
        }
    }

    #[test]
    fn test_sub_self() {
        let empty_defs = no_defs();
        let inst = Instruction::BinOp {
            dest: Value(2),
            op: IrBinOp::Sub,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Value(Value(1)),
            ty: IrType::I32,
        };
        let result = try_simplify(&inst, &empty_defs, &no_gep_defs()).unwrap();
        match result {
            Instruction::Copy { src: Operand::Const(IrConst::I32(0)), .. } => {}
            _ => panic!("Expected Copy with zero"),
        }
    }

    #[test]
    fn test_xor_self() {
        let empty_defs = no_defs();
        let inst = Instruction::BinOp {
            dest: Value(2),
            op: IrBinOp::Xor,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Value(Value(1)),
            ty: IrType::I32,
        };
        let result = try_simplify(&inst, &empty_defs, &no_gep_defs()).unwrap();
        match result {
            Instruction::Copy { src: Operand::Const(IrConst::I32(0)), .. } => {}
            _ => panic!("Expected Copy with zero"),
        }
    }

    #[test]
    fn test_and_self() {
        let empty_defs = no_defs();
        let inst = Instruction::BinOp {
            dest: Value(2),
            op: IrBinOp::And,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Value(Value(1)),
            ty: IrType::I32,
        };
        let result = try_simplify(&inst, &empty_defs, &no_gep_defs()).unwrap();
        match result {
            Instruction::Copy { src: Operand::Value(v), .. } => assert_eq!(v.0, 1),
            _ => panic!("Expected Copy of operand"),
        }
    }

    #[test]
    fn test_no_simplify() {
        let empty_defs = no_defs();
        // x + y (non-trivial) should not simplify
        let inst = Instruction::BinOp {
            dest: Value(2),
            op: IrBinOp::Add,
            lhs: Operand::Value(Value(0)),
            rhs: Operand::Value(Value(1)),
            ty: IrType::I32,
        };
        assert!(try_simplify(&inst, &empty_defs, &no_gep_defs()).is_none());
    }

    #[test]
    fn test_mul_power_of_two_to_shift() {
        let empty_defs = no_defs();
        // x * 4 => x << 2
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Mul,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I64(4)),
            ty: IrType::I64,
        };
        let result = try_simplify(&inst, &empty_defs, &no_gep_defs()).unwrap();
        match result {
            Instruction::BinOp { op: IrBinOp::Shl, rhs: Operand::Const(IrConst::I64(2)), .. } => {}
            _ => panic!("Expected Shl by 2, got {:?}", result),
        }
    }

    #[test]
    fn test_mul_two_to_add() {
        let empty_defs = no_defs();
        // x * 2 => x + x
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Mul,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I64(2)),
            ty: IrType::I64,
        };
        let result = try_simplify(&inst, &empty_defs, &no_gep_defs()).unwrap();
        match result {
            Instruction::BinOp { op: IrBinOp::Add, lhs: Operand::Value(a), rhs: Operand::Value(b), .. } => {
                assert_eq!(a.0, 1);
                assert_eq!(b.0, 1);
            }
            _ => panic!("Expected Add x,x, got {:?}", result),
        }
    }

    #[test]
    fn test_mul_power_of_two_i32() {
        let empty_defs = no_defs();
        // x * 8 => x << 3 (I32)
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Mul,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I32(8)),
            ty: IrType::I32,
        };
        let result = try_simplify(&inst, &empty_defs, &no_gep_defs()).unwrap();
        match result {
            Instruction::BinOp { op: IrBinOp::Shl, rhs: Operand::Const(IrConst::I32(3)), .. } => {}
            _ => panic!("Expected Shl by 3, got {:?}", result),
        }
    }

    #[test]
    fn test_mul_non_power_of_two_no_change() {
        let empty_defs = no_defs();
        // x * 3 should NOT be simplified to shift
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Mul,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I64(3)),
            ty: IrType::I64,
        };
        assert!(try_simplify(&inst, &empty_defs, &no_gep_defs()).is_none());
    }

    #[test]
    fn test_mul_float_no_strength_reduction() {
        let empty_defs = no_defs();
        // x * 2.0 should NOT be simplified (float type)
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Mul,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I64(2)),
            ty: IrType::F64,
        };
        assert!(try_simplify(&inst, &empty_defs, &no_gep_defs()).is_none());
    }

    #[test]
    fn test_udiv_power_of_two() {
        let empty_defs = no_defs();
        // x /u 8 => x >> 3  (unsigned)
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::UDiv,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I64(8)),
            ty: IrType::U64,
        };
        let result = try_simplify(&inst, &empty_defs, &no_gep_defs()).unwrap();
        match result {
            Instruction::BinOp { op: IrBinOp::LShr, rhs: Operand::Const(IrConst::I64(3)), .. } => {}
            _ => panic!("Expected LShr by 3, got {:?}", result),
        }
    }

    #[test]
    fn test_urem_power_of_two() {
        let empty_defs = no_defs();
        // x %u 8 => x & 7  (unsigned)
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::URem,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::I64(8)),
            ty: IrType::U64,
        };
        let result = try_simplify(&inst, &empty_defs, &no_gep_defs()).unwrap();
        match result {
            Instruction::BinOp { op: IrBinOp::And, rhs: Operand::Const(IrConst::I64(7)), .. } => {}
            _ => panic!("Expected And with 7, got {:?}", result),
        }
    }

    #[test]
    fn test_cast_same_type() {
        let empty_defs: Vec<Option<CastDef>> = vec![];
        let inst = Instruction::Cast {
            dest: Value(0),
            src: Operand::Value(Value(1)),
            from_ty: IrType::I32,
            to_ty: IrType::I32,
        };
        let result = try_simplify(&inst, &empty_defs, &no_gep_defs()).unwrap();
        match result {
            Instruction::Copy { src: Operand::Value(v), .. } => assert_eq!(v.0, 1),
            _ => panic!("Expected Copy"),
        }
    }

    #[test]
    fn test_cast_chain_widen_narrow() {
        // Cast(Cast(x, I32->I64), I64->I32) => Copy of x
        let mut defs: Vec<Option<CastDef>> = vec![None; 3];
        defs[1] = Some(CastDef {
            src: Operand::Value(Value(0)),
            from_ty: IrType::I32,
            to_ty: IrType::I64,
        });
        let inst = Instruction::Cast {
            dest: Value(2),
            src: Operand::Value(Value(1)),
            from_ty: IrType::I64,
            to_ty: IrType::I32,
        };
        let result = try_simplify(&inst, &defs, &no_gep_defs()).unwrap();
        match result {
            Instruction::Copy { src: Operand::Value(v), .. } => assert_eq!(v.0, 0),
            _ => panic!("Expected Copy of original value"),
        }
    }

    #[test]
    fn test_cast_const_fold() {
        let empty_defs: Vec<Option<CastDef>> = vec![];
        // Cast const I32(42) to I64 => const I64(42)
        let inst = Instruction::Cast {
            dest: Value(0),
            src: Operand::Const(IrConst::I32(42)),
            from_ty: IrType::I32,
            to_ty: IrType::I64,
        };
        let result = try_simplify(&inst, &empty_defs, &no_gep_defs()).unwrap();
        match result {
            Instruction::Copy { src: Operand::Const(IrConst::I64(42)), .. } => {}
            _ => panic!("Expected Copy with I64(42)"),
        }
    }

    #[test]
    fn test_cast_different_type_no_change() {
        let empty_defs: Vec<Option<CastDef>> = vec![];
        let inst = Instruction::Cast {
            dest: Value(0),
            src: Operand::Value(Value(1)),
            from_ty: IrType::I32,
            to_ty: IrType::I64,
        };
        assert!(try_simplify(&inst, &empty_defs, &no_gep_defs()).is_none());
    }

    #[test]
    fn test_gep_zero_offset() {
        let empty_defs = no_defs();
        let inst = Instruction::GetElementPtr {
            dest: Value(0),
            base: Value(1),
            offset: Operand::Const(IrConst::I64(0)),
            ty: IrType::Ptr,
        };
        let result = try_simplify(&inst, &empty_defs, &no_gep_defs()).unwrap();
        match result {
            Instruction::Copy { src: Operand::Value(v), .. } => assert_eq!(v.0, 1),
            _ => panic!("Expected Copy of base"),
        }
    }

    #[test]
    fn test_gep_nonzero_no_change() {
        let empty_defs = no_defs();
        let inst = Instruction::GetElementPtr {
            dest: Value(0),
            base: Value(1),
            offset: Operand::Const(IrConst::I64(4)),
            ty: IrType::Ptr,
        };
        assert!(try_simplify(&inst, &empty_defs, &no_gep_defs()).is_none());
    }

    #[test]
    fn test_gep_chain_fold() {
        // GEP(GEP(base, 8), 4) => GEP(base, 12)
        let empty_defs = no_defs();
        let mut gep_defs: Vec<Option<GepDef>> = vec![None; 3];
        gep_defs[1] = Some(GepDef {
            base: Value(0),
            offset: Operand::Const(IrConst::I64(8)),
        });
        let inst = Instruction::GetElementPtr {
            dest: Value(2),
            base: Value(1),
            offset: Operand::Const(IrConst::I64(4)),
            ty: IrType::Ptr,
        };
        let result = try_simplify(&inst, &empty_defs, &gep_defs).unwrap();
        match result {
            Instruction::GetElementPtr { base, offset: Operand::Const(IrConst::I64(12)), .. } => {
                assert_eq!(base.0, 0, "Should use original base");
            }
            _ => panic!("Expected GEP with combined offset 12, got {:?}", result),
        }
    }

    #[test]
    fn test_gep_chain_fold_to_zero() {
        // GEP(GEP(base, 4), -4) => Copy of base (offsets cancel)
        let empty_defs = no_defs();
        let mut gep_defs: Vec<Option<GepDef>> = vec![None; 3];
        gep_defs[1] = Some(GepDef {
            base: Value(0),
            offset: Operand::Const(IrConst::I64(4)),
        });
        let inst = Instruction::GetElementPtr {
            dest: Value(2),
            base: Value(1),
            offset: Operand::Const(IrConst::I64(-4)),
            ty: IrType::Ptr,
        };
        let result = try_simplify(&inst, &empty_defs, &gep_defs).unwrap();
        match result {
            Instruction::Copy { src: Operand::Value(v), .. } => {
                assert_eq!(v.0, 0, "Should copy original base when offsets cancel");
            }
            _ => panic!("Expected Copy of base, got {:?}", result),
        }
    }

    // === IEEE 754 float safety tests ===

    #[test]
    fn test_float_add_zero_not_simplified() {
        // -0.0 + 0.0 must NOT be simplified to -0.0 (result should be +0.0)
        let empty_defs = no_defs();
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Add,
            lhs: Operand::Const(IrConst::F64(-0.0)),
            rhs: Operand::Const(IrConst::F64(0.0)),
            ty: IrType::F64,
        };
        // Should NOT simplify (float add with zero)
        assert!(try_simplify(&inst, &empty_defs, &no_gep_defs()).is_none());
    }

    #[test]
    fn test_float_add_zero_lhs_not_simplified() {
        // 0.0 + (-0.0) must NOT simplify to -0.0
        let empty_defs = no_defs();
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Add,
            lhs: Operand::Const(IrConst::F64(0.0)),
            rhs: Operand::Const(IrConst::F64(-0.0)),
            ty: IrType::F64,
        };
        assert!(try_simplify(&inst, &empty_defs, &no_gep_defs()).is_none());
    }

    #[test]
    fn test_float_mul_zero_not_simplified() {
        // x * 0.0 must NOT simplify for floats (NaN*0=NaN, -5.0*0=-0.0)
        let empty_defs = no_defs();
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Mul,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::F64(0.0)),
            ty: IrType::F64,
        };
        assert!(try_simplify(&inst, &empty_defs, &no_gep_defs()).is_none());
    }

    #[test]
    fn test_float_sub_self_not_simplified() {
        // x - x must NOT simplify for floats (Inf - Inf = NaN)
        let empty_defs = no_defs();
        let inst = Instruction::BinOp {
            dest: Value(2),
            op: IrBinOp::Sub,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Value(Value(1)),
            ty: IrType::F64,
        };
        assert!(try_simplify(&inst, &empty_defs, &no_gep_defs()).is_none());
    }

    #[test]
    fn test_float_div_self_not_simplified() {
        // x / x must NOT simplify for floats (0/0=NaN, Inf/Inf=NaN)
        let empty_defs = no_defs();
        let inst = Instruction::BinOp {
            dest: Value(2),
            op: IrBinOp::SDiv,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Value(Value(1)),
            ty: IrType::F64,
        };
        assert!(try_simplify(&inst, &empty_defs, &no_gep_defs()).is_none());
    }

    #[test]
    fn test_float_sub_positive_zero_ok() {
        // x - (+0.0) is safe (subtracting +0 preserves value)
        let empty_defs = no_defs();
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Sub,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::F64(0.0)),
            ty: IrType::F64,
        };
        let result = try_simplify(&inst, &empty_defs, &no_gep_defs()).unwrap();
        match result {
            Instruction::Copy { src: Operand::Value(v), .. } => assert_eq!(v.0, 1),
            _ => panic!("Expected Copy"),
        }
    }

    #[test]
    fn test_float_sub_negative_zero_not_simplified() {
        // x - (-0.0) is NOT safe (equivalent to x + 0.0, changes -0.0 to +0.0)
        let empty_defs = no_defs();
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Sub,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::F64(-0.0)),
            ty: IrType::F64,
        };
        assert!(try_simplify(&inst, &empty_defs, &no_gep_defs()).is_none());
    }

    #[test]
    fn test_float_mul_one_not_simplified() {
        // x * F64(1.0) is not simplified because is_one() only matches integer 1.
        // This is fine -- the constant folder handles float-float folding separately.
        let empty_defs = no_defs();
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::Mul,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::F64(1.0)),
            ty: IrType::F64,
        };
        assert!(try_simplify(&inst, &empty_defs, &no_gep_defs()).is_none());
    }

    #[test]
    fn test_float_div_one_not_simplified() {
        // x / F64(1.0) is not simplified because is_one() only matches integer 1.
        let empty_defs = no_defs();
        let inst = Instruction::BinOp {
            dest: Value(0),
            op: IrBinOp::SDiv,
            lhs: Operand::Value(Value(1)),
            rhs: Operand::Const(IrConst::F64(1.0)),
            ty: IrType::F64,
        };
        assert!(try_simplify(&inst, &empty_defs, &no_gep_defs()).is_none());
    }
}
