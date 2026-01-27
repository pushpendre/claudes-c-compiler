//! Integer narrowing optimization pass.
//!
//! The C compiler's lowering always promotes sub-64-bit operations to I64
//! (via C's integer promotion rules), then narrows the result back. This
//! produces a widen-operate-narrow pattern that generates unnecessary
//! sign-extension instructions and wastes registers.
//!
//! This pass detects the pattern:
//!   %w = Cast %x, T -> I64       (widening)
//!   %r = BinOp op %w, rhs, I64   (operation in I64)
//!   %n = Cast %r, I64 -> T       (narrowing)
//!
//! And converts it to:
//!   %r = BinOp op %x, narrow(rhs), T  (direct operation in T)
//!   (the narrowing Cast %n becomes dead, removed by DCE)
//!
//! Phase 4 (with explicit narrowing Cast) is safe for arithmetic ops
//! (Add, Sub, Mul, And, Or, Xor, Shl) because the Cast truncates the
//! result, and the low bits are identical regardless of operation width.
//! Phase 5 (no Cast) is restricted to bitwise ops (And, Or, Xor) since
//! arithmetic ops can produce different upper bits due to carries.
//!
//! Similarly, comparisons (Cmp) where both operands are widened from
//! the same type can be narrowed, since sign/zero extension preserves
//! the ordering of values.

use crate::ir::ir::*;
use crate::common::types::IrType;

/// Run integer narrowing on the entire module.
/// Returns the number of instructions narrowed.
pub fn run(module: &mut IrModule) -> usize {
    module.for_each_function(narrow_function)
}

/// Information about a Cast instruction (widening).
#[derive(Clone)]
struct CastInfo {
    src: Operand,
    from_ty: IrType,
}

/// Information about a BinOp instruction.
#[derive(Clone)]
struct BinOpDef {
    op: IrBinOp,
    lhs: Operand,
    rhs: Operand,
}

/// Narrow operations in a single function.
pub(crate) fn narrow_function(func: &mut IrFunction) -> usize {
    // Early exit: the pass can only do work if the function contains I64/U64
    // BinOps or Cmps (which are the targets of narrowing). Skip the expensive
    // multi-pass analysis if none exist.
    let has_narrowable = func.blocks.iter().any(|block| {
        block.instructions.iter().any(|inst| match inst {
            Instruction::BinOp { ty, .. } => {
                matches!(ty, IrType::I64 | IrType::U64)
            }
            Instruction::Cmp { ty, .. } => {
                matches!(ty, IrType::I64 | IrType::U64)
            }
            _ => false,
        })
    });
    if !has_narrowable {
        return 0;
    }

    let max_id = func.max_value_id() as usize;
    let mut changes = 0;

    // Phase 1: Build a map of Value -> CastInfo for widening casts.
    // We only care about casts from a smaller integer type to I64/U64.
    let mut widen_map: Vec<Option<CastInfo>> = vec![None; max_id + 1];
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::Cast { dest, src, from_ty, to_ty } = inst {
                let is_widen = from_ty.is_integer() && (*to_ty == IrType::I64 || *to_ty == IrType::U64)
                    && from_ty.size() < to_ty.size();
                if is_widen {
                    let id = dest.0 as usize;
                    if id <= max_id {
                        widen_map[id] = Some(CastInfo {
                            src: *src,
                            from_ty: *from_ty,
                        });
                    }
                }
            }
        }
    }

    // Phase 2: Build a map of Value -> defining BinOp instruction info.
    // We only care about BinOps in I64 that use widened operands.
    let mut binop_map: Vec<Option<BinOpDef>> = vec![None; max_id + 1];
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::BinOp { dest, op, lhs, rhs, ty } = inst {
                if *ty == IrType::I64 || *ty == IrType::U64 {
                    let id = dest.0 as usize;
                    if id <= max_id {
                        binop_map[id] = Some(BinOpDef {
                            op: *op,
                            lhs: *lhs,
                            rhs: *rhs,
                        });
                    }
                }
            }
        }
    }

    // Phase 3: Find narrowing casts whose source is a BinOp that uses
    // widened operands, and narrow the BinOp.
    //
    // We need to be careful: the BinOp result might have other uses
    // besides the narrowing cast. In that case, we cannot change the BinOp's
    // type. Instead, we only narrow when the BinOp result is ONLY used
    // by narrowing casts back to the original type.
    //
    // For simplicity, we use a two-step approach:
    // 1. Count uses of each value
    // 2. Only narrow when the BinOp has exactly one use (the narrowing cast)

    // Count uses of each value
    let mut use_counts: Vec<u32> = vec![0; max_id + 1];
    for block in &func.blocks {
        for inst in &block.instructions {
            for_each_used_value(inst, &mut |v: Value| {
                let id = v.0 as usize;
                if id < use_counts.len() {
                    use_counts[id] = use_counts[id].saturating_add(1);
                }
            });
        }
        for_each_used_value_terminator(&block.terminator, &mut |v: Value| {
            let id = v.0 as usize;
            if id < use_counts.len() {
                use_counts[id] = use_counts[id].saturating_add(1);
            }
        });
    }

    // Phase 4: Apply narrowing transformations.
    // We look for Cast instructions that narrow from I64 to a smaller type,
    // where the source is a BinOp in I64 whose operands are widened from
    // the same smaller type (or are themselves already-narrowed values).
    //
    // narrowed_map tracks values that Phase 4 has already narrowed, so
    // subsequent iterations can recognize them as valid narrow operands.
    let mut narrowed_map: Vec<Option<IrType>> = vec![None; max_id + 1];

    for block in &mut func.blocks {
        for inst in &mut block.instructions {
            if let Instruction::Cast { dest, src: Operand::Value(src_val), from_ty, to_ty } = inst {
                // Must be a narrowing cast from I64 to a smaller integer type
                if !((*from_ty == IrType::I64 || *from_ty == IrType::U64)
                    && to_ty.is_integer()
                    && to_ty.size() < from_ty.size()) {
                    continue;
                }

                let src_id = src_val.0 as usize;
                if src_id > max_id {
                    continue;
                }

                // The source must be a BinOp in I64
                let binop_info = match &binop_map[src_id] {
                    Some(info) => info.clone(),
                    None => continue,
                };

                // The BinOp must use a safe operation (bit-preserving in low bits).
                // Shl is safe because shifting left only affects higher bits;
                // the low N bits of (x << k) are the same at any width >= N+k.
                // AShr/LShr are NOT safe because they bring in bits from above.
                let is_safe_op = matches!(binop_info.op,
                    IrBinOp::Add | IrBinOp::Sub | IrBinOp::Mul |
                    IrBinOp::And | IrBinOp::Or | IrBinOp::Xor |
                    IrBinOp::Shl
                );
                if !is_safe_op {
                    continue;
                }

                // The BinOp result must only be used by this narrowing cast
                // (otherwise we'd change the type for other users too)
                if use_counts[src_id] != 1 {
                    continue;
                }

                // Check that both operands of the BinOp are either:
                // (a) widened from the target type, or
                // (b) constants that fit in the target type, or
                // (c) already narrowed to the target type by a previous Phase 4 step
                let narrow_lhs = try_narrow_operand_with_narrowed(&binop_info.lhs, *to_ty, &widen_map, &narrowed_map);
                let narrow_rhs = try_narrow_operand_with_narrowed(&binop_info.rhs, *to_ty, &widen_map, &narrowed_map);

                if let (Some(new_lhs), Some(new_rhs)) = (narrow_lhs, narrow_rhs) {
                    let narrow_dest = *dest;
                    let narrow_ty = *to_ty;

                    // Replace this Cast with a BinOp at the narrow type
                    *inst = Instruction::BinOp {
                        dest: narrow_dest,
                        op: binop_info.op,
                        lhs: new_lhs,
                        rhs: new_rhs,
                        ty: narrow_ty,
                    };
                    changes += 1;

                    // Record that this dest value is now at the narrow type
                    let dest_id = narrow_dest.0 as usize;
                    if dest_id <= max_id {
                        narrowed_map[dest_id] = Some(narrow_ty);
                    }
                }
            }
        }
    }

    // Phase 5: Narrow I64 BinOps whose operands are all sub-64-bit values.
    // Handles the case where the frontend emits a BinOp at I64 but the
    // operands come from I32 Loads (no explicit widening Cast). Pattern:
    //   %x = Load ptr, I32
    //   %r = BinOp op %x, const, I64   (BinOp type wider than operands)
    // Only safe for bitwise ops (And/Or/Xor) since unlike Phase 4 there is
    // no explicit narrowing Cast guaranteeing the consumer truncates the result.
    //
    // Build load_type_map: Value -> type for Load instructions
    let mut load_type_map: Vec<Option<IrType>> = vec![None; max_id + 1];
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::Load { dest, ty, .. } = inst {
                let id = dest.0 as usize;
                if id <= max_id && ty.is_integer() && ty.size() < 8 {
                    load_type_map[id] = Some(*ty);
                }
            }
        }
    }

    // Find I64 BinOps where all operands are sub-64-bit (loads, constants, or
    // widen_map values) and the result has exactly one use (a Store to I32).
    // Check: result used only by stores to a sub-64-bit type.
    for block in &mut func.blocks {
        for inst in &mut block.instructions {
            if let Instruction::BinOp { dest, op, lhs, rhs, ty } = inst {
                if !(*ty == IrType::I64 || *ty == IrType::U64) {
                    continue;
                }
                let dest_id = dest.0 as usize;
                // Must be single-use (will be a Store or Cast)
                if dest_id >= use_counts.len() || use_counts[dest_id] != 1 {
                    continue;
                }
                // Only bitwise ops (And/Or/Xor) are safe: low N bits are always
                // preserved regardless of operand width. Arithmetic ops (Add/Sub/
                // Mul/Shl) are NOT safe because carries or shift semantics can
                // produce different results in the upper bits, and this phase does
                // not verify the consumer only uses the low bits.
                let is_safe_op = matches!(op,
                    IrBinOp::And | IrBinOp::Or | IrBinOp::Xor
                );
                if !is_safe_op {
                    continue;
                }

                // Try to determine the narrow type from operands
                let lhs_narrow_ty = operand_narrow_type(lhs, &load_type_map, &widen_map, &narrowed_map);
                let rhs_narrow_ty = operand_narrow_type(rhs, &load_type_map, &widen_map, &narrowed_map);

                let target_ty = match (lhs_narrow_ty, rhs_narrow_ty) {
                    (Some(lt), Some(rt)) if lt == rt => lt,
                    (Some(lt), Some(rt)) if lt.size() == rt.size() => lt,
                    (Some(t), None) => {
                        // RHS must be a constant that fits
                        if try_narrow_const_operand(rhs, t).is_some() { t } else { continue; }
                    }
                    (None, Some(t)) => {
                        if try_narrow_const_operand(lhs, t).is_some() { t } else { continue; }
                    }
                    _ => continue,
                };

                // Phase 5 changes the BinOp's result type without inserting a
                // widening Cast for consumers.  Narrowing to I32/U32 is safe on
                // x86-64 because 32-bit register operations implicitly zero-extend
                // to 64-bit, and codegen already handles I32/U32 results correctly.
                // Sub-int types (I8/U8/I16/U16) are NOT safe: consumers still
                // expect a 64-bit value, and without an explicit Cast the upper
                // bits may carry stale sign-extended data.  For example, narrowing
                // `(unsigned char)0xFF ^ 0` to U8 produces IrConst::I8(-1) after
                // constant folding, which sign-extends to -1 instead of 255 when
                // the consumer reads it as an i64.
                // Phase 4 handles sub-int narrowing correctly because it replaces
                // an existing narrowing Cast (so consumers already expect the
                // narrow type).
                if target_ty.size() < 4 {
                    continue;
                }

                // Narrow the operands
                let new_lhs = narrow_operand_by_type(lhs, target_ty, &load_type_map, &widen_map, &narrowed_map);
                let new_rhs = narrow_operand_by_type(rhs, target_ty, &load_type_map, &widen_map, &narrowed_map);
                if let (Some(nl), Some(nr)) = (new_lhs, new_rhs) {
                    *lhs = nl;
                    *rhs = nr;
                    *ty = target_ty;
                    narrowed_map[dest_id] = Some(target_ty);
                    changes += 1;
                }
            }
        }
    }

    // Phase 6: Also narrow Cmp instructions where both operands are widened
    // from the same type. Cmp(Sge, widen(x), widen(y)) == Cmp(Sge, x, y)
    // when the widen is from a signed type and both have the same source type.
    for block in &mut func.blocks {
        for inst in &mut block.instructions {
            if let Instruction::Cmp { dest, op, lhs, rhs, ty } = inst {
                if !(*ty == IrType::I64 || *ty == IrType::U64) {
                    continue;
                }

                // Try to narrow both operands
                // For Cmp, we need both operands to be widened from the SAME type
                let lhs_cast = match lhs {
                    Operand::Value(v) => {
                        let id = v.0 as usize;
                        if id <= max_id { widen_map[id].as_ref() } else { None }
                    }
                    _ => None,
                };
                let rhs_cast = match rhs {
                    Operand::Value(v) => {
                        let id = v.0 as usize;
                        if id <= max_id { widen_map[id].as_ref() } else { None }
                    }
                    _ => None,
                };

                // Both operands widened from the same type, OR one is widened
                // and the other is a constant that fits
                let narrow_ty = if let Some(lhs_info) = lhs_cast {
                    lhs_info.from_ty
                } else {
                    continue;
                };

                // Determine if it's a signed or unsigned comparison
                let is_signed_cmp = matches!(op,
                    IrCmpOp::Slt | IrCmpOp::Sle | IrCmpOp::Sgt | IrCmpOp::Sge);
                let is_unsigned_cmp = matches!(op,
                    IrCmpOp::Ult | IrCmpOp::Ule | IrCmpOp::Ugt | IrCmpOp::Uge);

                // Narrowing a comparison is only safe when the extension kind matches
                // the comparison kind. A signed comparison on zero-extended values
                // can give wrong results because zero-extension doesn't preserve the
                // sign-bit ordering (e.g., -1 as u8=0xFF zero-extends to 255, not -1).
                // Similarly, an unsigned comparison on sign-extended values can fail
                // (e.g., 128 as i8=-128 sign-extends to a large negative number).
                // Eq/Ne are safe with either extension kind since they compare bit patterns.
                if is_signed_cmp && narrow_ty.is_unsigned() {
                    continue;
                }
                if is_unsigned_cmp && narrow_ty.is_signed() {
                    continue;
                }

                let new_lhs = if let Some(info) = lhs_cast {
                    if info.from_ty == narrow_ty { info.src } else { continue; }
                } else {
                    continue;
                };

                let new_rhs = if let Some(info) = rhs_cast {
                    if info.from_ty == narrow_ty { info.src } else { continue; }
                } else if let Operand::Const(c) = rhs {
                    // Constant: check if it fits in the narrow type
                    if let Some(narrow_c) = try_narrow_const(c, narrow_ty) {
                        Operand::Const(narrow_c)
                    } else {
                        continue;
                    }
                } else {
                    continue;
                };

                *lhs = new_lhs;
                *rhs = new_rhs;
                *ty = narrow_ty;
                let _ = dest; // dest type unchanged (always produces I8 for bool)
                changes += 1;
            }
        }
    }

    changes
}

/// Try to narrow an operand from I64 to a target type.
/// Returns the narrowed operand if possible, None otherwise.
///
/// Checks three cases:
/// (a) The value is a widening cast from the target type → return the original
/// (b) The value was already narrowed to the target type by Phase 4 → return as-is
/// (c) The operand is a constant that fits in the target type → return narrowed const
fn try_narrow_operand_with_narrowed(
    op: &Operand,
    target_ty: IrType,
    widen_map: &[Option<CastInfo>],
    narrowed_map: &[Option<IrType>],
) -> Option<Operand> {
    match op {
        Operand::Value(v) => {
            let id = v.0 as usize;
            // (a) Check if it's a widening cast from the target type
            if id < widen_map.len() {
                if let Some(info) = &widen_map[id] {
                    if info.from_ty == target_ty
                       || (info.from_ty.size() == target_ty.size()
                           && info.from_ty.is_unsigned() == target_ty.is_unsigned()) {
                        return Some(info.src);
                    }
                }
            }
            // (b) Check if it was already narrowed to the target type
            if id < narrowed_map.len() {
                if let Some(nt) = &narrowed_map[id] {
                    if *nt == target_ty
                       || (nt.size() == target_ty.size()
                           && nt.is_unsigned() == target_ty.is_unsigned()) {
                        return Some(Operand::Value(*v));
                    }
                }
            }
            None
        }
        Operand::Const(c) => {
            try_narrow_const(c, target_ty).map(Operand::Const)
        }
    }
}

/// Determine the narrow type of an operand (from loads, widen_map, or narrowed_map).
/// Returns None for constants or if the operand's narrow type can't be determined.
fn operand_narrow_type(
    op: &Operand,
    load_type_map: &[Option<IrType>],
    widen_map: &[Option<CastInfo>],
    narrowed_map: &[Option<IrType>],
) -> Option<IrType> {
    match op {
        Operand::Value(v) => {
            let id = v.0 as usize;
            // Check load_type_map
            if id < load_type_map.len() {
                if let Some(ty) = &load_type_map[id] {
                    return Some(*ty);
                }
            }
            // Check widen_map
            if id < widen_map.len() {
                if let Some(info) = &widen_map[id] {
                    return Some(info.from_ty);
                }
            }
            // Check narrowed_map
            if id < narrowed_map.len() {
                if let Some(ty) = &narrowed_map[id] {
                    return Some(*ty);
                }
            }
            None
        }
        Operand::Const(_) => None,
    }
}

/// Check if an operand is a constant that can be narrowed to target_ty.
fn try_narrow_const_operand(op: &Operand, target_ty: IrType) -> Option<IrConst> {
    match op {
        Operand::Const(c) => try_narrow_const(c, target_ty),
        _ => None,
    }
}

/// Narrow an operand to target_ty. The operand may be:
/// - A Value that's an I32 Load (already the right type, return as-is)
/// - A Value in widen_map (return original source)
/// - A Value in narrowed_map (already narrowed, return as-is)
/// - A constant (narrow it)
fn narrow_operand_by_type(
    op: &Operand,
    target_ty: IrType,
    load_type_map: &[Option<IrType>],
    widen_map: &[Option<CastInfo>],
    narrowed_map: &[Option<IrType>],
) -> Option<Operand> {
    match op {
        Operand::Value(v) => {
            let id = v.0 as usize;
            // If it's a Load of the target type, use as-is
            if id < load_type_map.len() {
                if let Some(ty) = &load_type_map[id] {
                    if *ty == target_ty || (ty.size() == target_ty.size() && ty.is_unsigned() == target_ty.is_unsigned()) {
                        return Some(Operand::Value(*v));
                    }
                }
            }
            // If it's a widening cast, use the original source
            if id < widen_map.len() {
                if let Some(info) = &widen_map[id] {
                    if info.from_ty == target_ty
                       || (info.from_ty.size() == target_ty.size()
                           && info.from_ty.is_unsigned() == target_ty.is_unsigned()) {
                        return Some(info.src);
                    }
                }
            }
            // If it's already narrowed
            if id < narrowed_map.len() {
                if let Some(nt) = &narrowed_map[id] {
                    if *nt == target_ty || (nt.size() == target_ty.size() && nt.is_unsigned() == target_ty.is_unsigned()) {
                        return Some(Operand::Value(*v));
                    }
                }
            }
            None
        }
        Operand::Const(c) => {
            try_narrow_const(c, target_ty).map(Operand::Const)
        }
    }
}

/// Try to narrow a constant to a smaller type.
/// Returns the narrowed constant if the value fits, None otherwise.
fn try_narrow_const(c: &IrConst, target_ty: IrType) -> Option<IrConst> {
    let val = match c {
        IrConst::I64(v) => *v,
        IrConst::I32(v) => *v as i64,
        IrConst::I16(v) => *v as i64,
        IrConst::I8(v) => *v as i64,
        IrConst::I128(v) => *v as i64,
        IrConst::Zero => 0,
        _ => return None, // Float constants can't be narrowed
    };

    match target_ty {
        IrType::I32 => {
            if val >= i32::MIN as i64 && val <= i32::MAX as i64 {
                Some(IrConst::I32(val as i32))
            } else {
                None
            }
        }
        IrType::U32 => {
            // For bit-preserving ops, only the low 32 bits matter.
            // Accept any i64 value whose low 32 bits are meaningful.
            // Use IrConst::from_i64 which stores U32 as I64 with zero-extension,
            // matching the rest of the codebase's convention and ensuring
            // correct behavior when the constant is later read via to_i64().
            if val >= i32::MIN as i64 && val <= u32::MAX as i64 {
                Some(IrConst::from_i64(val, IrType::U32))
            } else {
                None
            }
        }
        IrType::I16 => {
            if val >= i16::MIN as i64 && val <= i16::MAX as i64 {
                Some(IrConst::I16(val as i16))
            } else {
                None
            }
        }
        IrType::U16 => {
            if val >= 0 && val <= u16::MAX as i64 {
                // Use from_i64 to get zero-extended I64 representation for unsigned
                Some(IrConst::from_i64(val, IrType::U16))
            } else {
                None
            }
        }
        IrType::I8 => {
            if val >= i8::MIN as i64 && val <= i8::MAX as i64 {
                Some(IrConst::I8(val as i8))
            } else {
                None
            }
        }
        IrType::U8 => {
            if val >= 0 && val <= u8::MAX as i64 {
                // Use from_i64 to get zero-extended I64 representation for unsigned
                Some(IrConst::from_i64(val, IrType::U8))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Helper to call f for a Value if the operand is a Value.
#[inline]
fn visit_operand(op: &Operand, f: &mut impl FnMut(Value)) {
    if let Operand::Value(v) = op {
        f(*v);
    }
}

/// Call a function for each Value used (read) by an instruction.
fn for_each_used_value(inst: &Instruction, f: &mut impl FnMut(Value)) {
    match inst {
        Instruction::Alloca { .. } | Instruction::GlobalAddr { .. }
        | Instruction::LabelAddr { .. } | Instruction::StackSave { .. }
        | Instruction::Fence { .. } | Instruction::GetReturnF64Second { .. }
        | Instruction::GetReturnF32Second { .. }
        | Instruction::GetReturnF128Second { .. } => {}

        Instruction::DynAlloca { size, .. } => visit_operand(size, f),
        Instruction::Store { val, ptr, .. } => { visit_operand(val, f); f(*ptr); }
        Instruction::Load { ptr, .. } => f(*ptr),
        Instruction::BinOp { lhs, rhs, .. } => { visit_operand(lhs, f); visit_operand(rhs, f); }
        Instruction::UnaryOp { src, .. } => visit_operand(src, f),
        Instruction::Cmp { lhs, rhs, .. } => { visit_operand(lhs, f); visit_operand(rhs, f); }
        Instruction::Call { args, .. } => { for a in args { visit_operand(a, f); } }
        Instruction::CallIndirect { func_ptr, args, .. } => {
            visit_operand(func_ptr, f);
            for a in args { visit_operand(a, f); }
        }
        Instruction::GetElementPtr { base, offset, .. } => { f(*base); visit_operand(offset, f); }
        Instruction::Cast { src, .. } | Instruction::Copy { src, .. } => visit_operand(src, f),
        Instruction::Memcpy { dest, src, .. } => { f(*dest); f(*src); }
        Instruction::VaArg { va_list_ptr, .. } | Instruction::VaStart { va_list_ptr }
        | Instruction::VaEnd { va_list_ptr } => f(*va_list_ptr),
        Instruction::VaCopy { dest_ptr, src_ptr } => { f(*dest_ptr); f(*src_ptr); }
        Instruction::AtomicRmw { ptr, val, .. } => { visit_operand(ptr, f); visit_operand(val, f); }
        Instruction::AtomicCmpxchg { ptr, expected, desired, .. } => {
            visit_operand(ptr, f); visit_operand(expected, f); visit_operand(desired, f);
        }
        Instruction::AtomicLoad { ptr, .. } => visit_operand(ptr, f),
        Instruction::AtomicStore { ptr, val, .. } => { visit_operand(ptr, f); visit_operand(val, f); }
        Instruction::Phi { incoming, .. } => {
            for (op, _) in incoming { visit_operand(op, f); }
        }
        Instruction::SetReturnF64Second { src } | Instruction::SetReturnF32Second { src } | Instruction::SetReturnF128Second { src } => visit_operand(src, f),
        Instruction::InlineAsm { outputs, inputs, .. } => {
            for (_, ptr, _) in outputs { f(*ptr); }
            for (_, op, _) in inputs { visit_operand(op, f); }
        }
        Instruction::Intrinsic { dest_ptr, args, .. } => {
            if let Some(ptr) = dest_ptr { f(*ptr); }
            for a in args { visit_operand(a, f); }
        }
        Instruction::Select { cond, true_val, false_val, .. } => {
            visit_operand(cond, f); visit_operand(true_val, f); visit_operand(false_val, f);
        }
        Instruction::StackRestore { ptr } => f(*ptr),
        Instruction::ParamRef { .. } => {}
    }
}

/// Call a function for each Value used by a terminator.
fn for_each_used_value_terminator(term: &Terminator, f: &mut impl FnMut(Value)) {
    match term {
        Terminator::Return(Some(op)) => {
            if let Operand::Value(v) = op { f(*v); }
        }
        Terminator::CondBranch { cond, .. } => {
            if let Operand::Value(v) = cond { f(*v); }
        }
        Terminator::IndirectBranch { target, .. } => {
            if let Operand::Value(v) = target { f(*v); }
        }
        Terminator::Switch { val, .. } => {
            if let Operand::Value(v) = val { f(*v); }
        }
        Terminator::Return(None) | Terminator::Branch(_) | Terminator::Unreachable => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_func_with_blocks(blocks: Vec<BasicBlock>) -> IrFunction {
        let mut func = IrFunction::new("test".to_string(), IrType::I32, vec![], false);
        func.blocks = blocks;
        func.next_value_id = 100;
        func
    }

    #[test]
    fn test_narrow_add_i32() {
        // %1 = Cast %0, I32->I64     (widen)
        // %2 = BinOp Add %1, I64(5), I64   (add in I64)
        // %3 = Cast %2, I64->I32     (narrow)
        // => Should become:
        // %3 = BinOp Add %0, I32(5), I32
        let mut func = make_func_with_blocks(vec![BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Cast {
                    dest: Value(1),
                    src: Operand::Value(Value(0)),
                    from_ty: IrType::I32,
                    to_ty: IrType::I64,
                },
                Instruction::BinOp {
                    dest: Value(2),
                    op: IrBinOp::Add,
                    lhs: Operand::Value(Value(1)),
                    rhs: Operand::Const(IrConst::I64(5)),
                    ty: IrType::I64,
                },
                Instruction::Cast {
                    dest: Value(3),
                    src: Operand::Value(Value(2)),
                    from_ty: IrType::I64,
                    to_ty: IrType::I32,
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(3)))),
            source_spans: Vec::new(),
        }]);

        let changes = narrow_function(&mut func);
        assert!(changes > 0, "Should narrow the add");

        // The last instruction should now be a BinOp in I32
        let last = &func.blocks[0].instructions[2];
        match last {
            Instruction::BinOp { op: IrBinOp::Add, ty: IrType::I32, lhs, rhs, .. } => {
                // LHS should be the original %0 (unwrapped from the widening cast)
                assert!(matches!(lhs, Operand::Value(Value(0))));
                // RHS should be I32(5) (narrowed constant)
                assert!(matches!(rhs, Operand::Const(IrConst::I32(5))));
            }
            other => panic!("Expected narrowed BinOp Add I32, got {:?}", other),
        }
    }

    #[test]
    fn test_narrow_cmp_sge() {
        // %1 = Cast %0, I32->I64     (widen)
        // %2 = Cmp Sge %1, I64(256), I64
        // => Should become:
        // %2 = Cmp Sge %0, I32(256), I32
        let mut func = make_func_with_blocks(vec![BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Cast {
                    dest: Value(1),
                    src: Operand::Value(Value(0)),
                    from_ty: IrType::I32,
                    to_ty: IrType::I64,
                },
                Instruction::Cmp {
                    dest: Value(2),
                    op: IrCmpOp::Sge,
                    lhs: Operand::Value(Value(1)),
                    rhs: Operand::Const(IrConst::I64(256)),
                    ty: IrType::I64,
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(2)))),
            source_spans: Vec::new(),
        }]);

        let changes = narrow_function(&mut func);
        assert!(changes > 0, "Should narrow the comparison");

        let cmp = &func.blocks[0].instructions[1];
        match cmp {
            Instruction::Cmp { op: IrCmpOp::Sge, ty: IrType::I32, lhs, rhs, .. } => {
                assert!(matches!(lhs, Operand::Value(Value(0))));
                assert!(matches!(rhs, Operand::Const(IrConst::I32(256))));
            }
            other => panic!("Expected narrowed Cmp Sge I32, got {:?}", other),
        }
    }

    #[test]
    fn test_no_narrow_when_multiple_uses() {
        // %1 = Cast %0, I32->I64
        // %2 = BinOp Add %1, I64(5), I64
        // %3 = Cast %2, I64->I32  (one use)
        // return %2               (another use of %2!)
        // => Should NOT narrow because %2 has multiple uses
        let mut func = make_func_with_blocks(vec![BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Cast {
                    dest: Value(1),
                    src: Operand::Value(Value(0)),
                    from_ty: IrType::I32,
                    to_ty: IrType::I64,
                },
                Instruction::BinOp {
                    dest: Value(2),
                    op: IrBinOp::Add,
                    lhs: Operand::Value(Value(1)),
                    rhs: Operand::Const(IrConst::I64(5)),
                    ty: IrType::I64,
                },
                Instruction::Cast {
                    dest: Value(3),
                    src: Operand::Value(Value(2)),
                    from_ty: IrType::I64,
                    to_ty: IrType::I32,
                },
            ],
            // Return %2 (I64 value), so %2 has two uses
            terminator: Terminator::Return(Some(Operand::Value(Value(2)))),
            source_spans: Vec::new(),
        }]);

        let changes = narrow_function(&mut func);
        assert_eq!(changes, 0, "Should not narrow when BinOp has multiple uses");
    }

    #[test]
    fn test_no_narrow_right_shift() {
        // Right shifts are not safe to narrow (sign bits differ)
        let mut func = make_func_with_blocks(vec![BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Cast {
                    dest: Value(1),
                    src: Operand::Value(Value(0)),
                    from_ty: IrType::I32,
                    to_ty: IrType::I64,
                },
                Instruction::BinOp {
                    dest: Value(2),
                    op: IrBinOp::AShr,
                    lhs: Operand::Value(Value(1)),
                    rhs: Operand::Const(IrConst::I64(3)),
                    ty: IrType::I64,
                },
                Instruction::Cast {
                    dest: Value(3),
                    src: Operand::Value(Value(2)),
                    from_ty: IrType::I64,
                    to_ty: IrType::I32,
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(3)))),
            source_spans: Vec::new(),
        }]);

        let changes = narrow_function(&mut func);
        assert_eq!(changes, 0, "Should not narrow right shifts");
    }
}
