//! x86-64 F128 (long double) operations via x87 FPU.
//!
//! Functions for loading/storing F128 values, converting between x87 80-bit
//! extended precision and other types, and emitting x86-specific cast instructions.

use crate::ir::ir::*;
use crate::common::types::IrType;
use crate::backend::cast::{CastKind, classify_cast};
use crate::backend::traits::ArchCodegen;
use super::codegen::X86Codegen;

impl X86Codegen {
    /// Prepare %rcx to point at a SlotAddr, applying the given byte offset.
    /// After this call, Direct addresses are accessed as `(rbp_offset)(%rbp)`,
    /// while OverAligned/Indirect addresses are in %rcx (with offset applied).
    /// Returns the rbp-relative offset for Direct slots, or None if %rcx holds the address.
    pub(super) fn emit_f128_resolve_addr(&mut self, addr: &crate::backend::state::SlotAddr, ptr_id: u32, offset: i64) -> Option<i64> {
        use crate::backend::state::SlotAddr;
        match addr {
            SlotAddr::Direct(slot) => Some(slot.0 + offset),
            SlotAddr::OverAligned(slot, id) => {
                self.emit_alloca_aligned_addr(*slot, *id);
                if offset != 0 {
                    self.state.out.emit_instr_imm_reg("    addq", offset, "rcx");
                }
                None
            }
            SlotAddr::Indirect(slot) => {
                self.emit_load_ptr_from_slot(*slot, ptr_id);
                if offset != 0 {
                    self.state.out.emit_instr_imm_reg("    addq", offset, "rcx");
                }
                None
            }
        }
    }

    /// Emit `fldt` from a resolved address (loading x87 80-bit to ST0).
    pub(super) fn emit_f128_fldt(&mut self, addr: &crate::backend::state::SlotAddr, ptr_id: u32, offset: i64) {
        match self.emit_f128_resolve_addr(addr, ptr_id, offset) {
            Some(rbp_off) => self.state.emit_fmt(format_args!("    fldt {}(%rbp)", rbp_off)),
            None => self.state.emit("    fldt (%rcx)"),
        }
    }

    /// Emit `fstpt` to a resolved address (storing x87 ST0 as 80-bit).
    pub(super) fn emit_f128_fstpt(&mut self, addr: &crate::backend::state::SlotAddr, ptr_id: u32, offset: i64) {
        match self.emit_f128_resolve_addr(addr, ptr_id, offset) {
            Some(rbp_off) => self.state.emit_fmt(format_args!("    fstpt {}(%rbp)", rbp_off)),
            None => self.state.emit("    fstpt (%rcx)"),
        }
    }

    /// Store raw x87 LongDouble bytes (10 bytes as lo:u64 + hi:u64) to a resolved address.
    pub(super) fn emit_f128_store_raw_bytes(&mut self, addr: &crate::backend::state::SlotAddr, ptr_id: u32, offset: i64, lo: u64, hi: u64) {
        use crate::backend::state::SlotAddr;
        match addr {
            SlotAddr::Direct(slot) => {
                let rbp_off = slot.0 + offset;
                self.state.out.emit_instr_imm_reg("    movabsq", lo as i64, "rax");
                self.state.out.emit_instr_reg_rbp("    movq", "rax", rbp_off);
                if hi != 0 {
                    self.state.out.emit_instr_imm_reg("    movq", hi as i64, "rax");
                } else {
                    self.state.emit("    xorl %eax, %eax");
                }
                self.state.out.emit_instr_reg_rbp("    movq", "rax", rbp_off + 8);
            }
            SlotAddr::OverAligned(slot, id) => {
                self.emit_alloca_aligned_addr(*slot, *id);
                if offset != 0 {
                    self.state.out.emit_instr_imm_reg("    addq", offset, "rcx");
                }
                self.state.out.emit_instr_imm_reg("    movabsq", lo as i64, "rax");
                self.state.emit("    movq %rax, (%rcx)");
                if hi != 0 {
                    self.state.out.emit_instr_imm_reg("    movq", hi as i64, "rax");
                } else {
                    self.state.emit("    xorl %eax, %eax");
                }
                self.state.emit("    movq %rax, 8(%rcx)");
            }
            SlotAddr::Indirect(slot) => {
                self.emit_load_ptr_from_slot(*slot, ptr_id);
                if offset != 0 {
                    self.state.out.emit_instr_imm_reg("    addq", offset, "rcx");
                }
                self.state.out.emit_instr_imm_reg("    movabsq", lo as i64, "rax");
                self.state.emit("    movq %rax, (%rcx)");
                if hi != 0 {
                    self.state.out.emit_instr_imm_reg("    movq", hi as i64, "rax");
                } else {
                    self.state.emit("    xorl %eax, %eax");
                }
                self.state.emit("    movq %rax, 8(%rcx)");
            }
        }
    }

    /// Convert f64 in %rax to x87 80-bit and store to a resolved address via fstpt.
    /// This is the fallback path when we don't have full-precision x87 data.
    pub(super) fn emit_f128_store_f64_via_x87(&mut self, addr: &crate::backend::state::SlotAddr, ptr_id: u32, offset: i64) {
        use crate::backend::state::SlotAddr;
        match addr {
            SlotAddr::Direct(slot) => {
                let rbp_off = slot.0 + offset;
                self.state.emit("    pushq %rax");
                self.state.emit("    fldl (%rsp)");
                self.state.emit("    addq $8, %rsp");
                self.state.out.emit_instr_rbp("    fstpt", rbp_off);
            }
            SlotAddr::OverAligned(slot, id) => {
                self.state.emit("    movq %rax, %rdx");
                self.emit_alloca_aligned_addr(*slot, *id);
                if offset != 0 {
                    self.state.out.emit_instr_imm_reg("    addq", offset, "rcx");
                }
                self.state.emit("    pushq %rdx");
                self.state.emit("    fldl (%rsp)");
                self.state.emit("    addq $8, %rsp");
                self.state.emit("    fstpt (%rcx)");
            }
            SlotAddr::Indirect(slot) => {
                self.state.emit("    movq %rax, %rdx");
                self.emit_load_ptr_from_slot(*slot, ptr_id);
                if offset != 0 {
                    self.state.out.emit_instr_imm_reg("    addq", offset, "rcx");
                }
                self.state.emit("    pushq %rdx");
                self.state.emit("    fldl (%rsp)");
                self.state.emit("    addq $8, %rsp");
                self.state.emit("    fstpt (%rcx)");
            }
        }
    }

    /// Complete an F128 load after fldt has placed the value in x87 ST0.
    /// Stores full 80-bit precision to dest's slot (if available) and puts
    /// a truncated f64 copy in %rax for backward compatibility.
    pub(super) fn emit_f128_load_finish(&mut self, dest: &Value) {
        if let Some(dest_slot) = self.state.get_slot(dest.0) {
            self.state.out.emit_instr_rbp("    fstpt", dest_slot.0);
            self.state.out.emit_instr_rbp("    fldt", dest_slot.0);
            self.state.emit("    subq $8, %rsp");
            self.state.emit("    fstpl (%rsp)");
            self.state.emit("    popq %rax");
            self.state.reg_cache.set_acc(dest.0, false);
            self.state.f128_direct_slots.insert(dest.0);
        } else {
            self.state.emit("    subq $8, %rsp");
            self.state.emit("    fstpl (%rsp)");
            self.state.emit("    popq %rax");
            self.emit_store_result(dest);
        }
    }

    /// Load full-precision x87 80-bit from the given memory slot and convert to integer.
    /// Uses `fldt` directly from memory instead of going through the f64 intermediate
    /// in %rax, preserving all 64 mantissa bits for the conversion.
    pub(super) fn emit_f128_to_int_from_memory(&mut self, addr: &crate::backend::state::SlotAddr, to_ty: IrType) {
        use crate::backend::state::SlotAddr;
        match addr {
            SlotAddr::Direct(slot) => {
                self.state.out.emit_instr_rbp("    fldt", slot.0);
            }
            SlotAddr::OverAligned(slot, id) => {
                self.emit_alloca_aligned_addr(*slot, *id);
                self.state.emit("    fldt (%rcx)");
            }
            SlotAddr::Indirect(slot) => {
                self.emit_load_ptr_from_slot(*slot, 0);
                self.state.emit("    fldt (%rcx)");
            }
        }
        // ST0 now has full 80-bit precision value
        self.emit_f128_st0_to_int(to_ty);
    }

    /// Convert the x87 ST0 value (full 80-bit precision) to an integer in %rax.
    /// Handles signed, unsigned, and smaller-than-64-bit integer types.
    pub(super) fn emit_f128_st0_to_int(&mut self, to_ty: IrType) {
        if to_ty.is_signed() || to_ty == IrType::Ptr {
            // Signed: direct fisttpq
            self.state.emit("    subq $8, %rsp");
            self.state.emit("    fisttpq (%rsp)");
            self.state.emit("    movq (%rsp), %rax");
            self.state.emit("    addq $8, %rsp");
            if to_ty.size() < 8 && to_ty != IrType::Ptr {
                match to_ty {
                    IrType::I8 => self.state.emit("    movsbq %al, %rax"),
                    IrType::I16 => self.state.emit("    movswq %ax, %rax"),
                    IrType::I32 => self.state.emit("    movslq %eax, %rax"),
                    _ => {}
                }
            }
        } else if to_ty == IrType::U64 {
            // Unsigned 64-bit: handle values >= 2^63
            let big_label = self.state.fresh_label("ld2u_big");
            let done_label = self.state.fresh_label("ld2u_done");
            // Load 2^63 as x87 extended precision for comparison
            self.state.emit("    subq $8, %rsp");
            self.state.emit("    movabsq $4890909195324358656, %rcx"); // 2^63 as f64 bits
            self.state.emit("    movq %rcx, (%rsp)");
            self.state.emit("    fldl (%rsp)"); // ST0 = 2^63 (f64), ST1 = value (80-bit)
            self.state.emit("    fcomip %st(1), %st"); // compare and pop 2^63
            self.state.out.emit_jcc_label("    jbe", &big_label);
            // Small case: value < 2^63
            self.state.emit("    fisttpq (%rsp)");
            self.state.emit("    movq (%rsp), %rax");
            self.state.emit("    addq $8, %rsp");
            self.state.out.emit_jmp_label(&done_label);
            // Big case: value >= 2^63
            self.state.out.emit_named_label(&big_label);
            self.state.emit("    movabsq $4890909195324358656, %rcx");
            self.state.emit("    movq %rcx, (%rsp)");
            self.state.emit("    fldl (%rsp)"); // ST0 = 2^63, ST1 = value
            self.state.emit("    fsubrp %st, %st(1)"); // ST0 = value - 2^63
            self.state.emit("    fisttpq (%rsp)");
            self.state.emit("    movq (%rsp), %rax");
            self.state.emit("    addq $8, %rsp");
            self.state.emit("    movabsq $9223372036854775808, %rcx");
            self.state.emit("    addq %rcx, %rax");
            self.state.out.emit_named_label(&done_label);
        } else {
            // Smaller unsigned types: fisttpq then truncate
            self.state.emit("    subq $8, %rsp");
            self.state.emit("    fisttpq (%rsp)");
            self.state.emit("    movq (%rsp), %rax");
            self.state.emit("    addq $8, %rsp");
            match to_ty {
                IrType::U8 => self.state.emit("    movzbq %al, %rax"),
                IrType::U16 => self.state.emit("    movzwq %ax, %rax"),
                IrType::U32 => self.state.emit("    movl %eax, %eax"),
                _ => {}
            }
        }
    }

    /// Emit x86-64 instructions for a type cast that operates on the
    /// primary accumulator (%rax). Used internally by both `emit_cast_instrs`
    /// (trait method) and the i128 special-case paths in `emit_cast`.
    pub(super) fn emit_cast_instrs_x86(&mut self, from_ty: IrType, to_ty: IrType) {
        // Handle F128 (long double) casts specially using x87 FPU instructions.
        // The generic classify_cast() reduces F128 to F64, losing the extra precision
        // that x87 80-bit extended precision provides. For integer <-> F128 casts,
        // we must use x87 FILD/FISTTP to preserve full 64-bit integer precision.
        if to_ty == IrType::F128 && !from_ty.is_float() {
            // Integer -> F128: use x87 FILD for exact conversion, then store as f64 in rax.
            // We use subq/movq/addq instead of push/pop to avoid peephole push/pop elimination.
            if from_ty.is_signed() || from_ty.size() < 8 {
                // For signed types (or unsigned types smaller than 8 bytes that fit in signed i64):
                // Sign/zero-extend to 64-bit first if needed
                if from_ty.size() < 8 {
                    if from_ty.is_unsigned() {
                        match from_ty {
                            IrType::U8 => self.state.emit("    movzbq %al, %rax"),
                            IrType::U16 => self.state.emit("    movzwq %ax, %rax"),
                            IrType::U32 => self.state.emit("    movl %eax, %eax"),
                            _ => {}
                        }
                    } else {
                        match from_ty {
                            IrType::I8 => self.state.emit("    movsbq %al, %rax"),
                            IrType::I16 => self.state.emit("    movswq %ax, %rax"),
                            IrType::I32 => self.state.emit("    cltq"),
                            _ => {}
                        }
                    }
                }
                // Store i64 to stack scratch space, FILD loads it into x87, FSTPL stores as f64
                self.state.emit("    subq $8, %rsp");
                self.state.emit("    movq %rax, (%rsp)");
                self.state.emit("    fildq (%rsp)");
                self.state.emit("    fstpl (%rsp)");
                self.state.emit("    movq (%rsp), %rax");
                self.state.emit("    addq $8, %rsp");
            } else {
                // Unsigned 64-bit: FILD treats as signed, so handle high-bit case.
                // If value < 2^63, FILD works directly. Otherwise, use shift+round trick.
                let big_label = self.state.fresh_label("u2ld_big");
                let done_label = self.state.fresh_label("u2ld_done");
                self.state.emit("    testq %rax, %rax");
                self.state.out.emit_jcc_label("    js", &big_label);
                // Positive (< 2^63): FILD directly
                self.state.emit("    subq $8, %rsp");
                self.state.emit("    movq %rax, (%rsp)");
                self.state.emit("    fildq (%rsp)");
                self.state.emit("    fstpl (%rsp)");
                self.state.emit("    movq (%rsp), %rax");
                self.state.emit("    addq $8, %rsp");
                self.state.out.emit_jmp_label(&done_label);
                self.state.out.emit_named_label(&big_label);
                // High bit set: split into halved value + rounding bit, then double
                self.state.emit("    movq %rax, %rcx");
                self.state.emit("    shrq $1, %rax");
                self.state.emit("    andq $1, %rcx");
                self.state.emit("    orq %rcx, %rax");
                self.state.emit("    subq $8, %rsp");
                self.state.emit("    movq %rax, (%rsp)");
                self.state.emit("    fildq (%rsp)");
                self.state.emit("    fld %st(0)");          // duplicate ST0
                self.state.emit("    faddp %st, %st(1)");   // ST0 = ST0 + ST1, pop = doubled value
                self.state.emit("    fstpl (%rsp)");
                self.state.emit("    movq (%rsp), %rax");
                self.state.emit("    addq $8, %rsp");
                self.state.out.emit_named_label(&done_label);
            }
            return;
        }
        if from_ty == IrType::F128 && !to_ty.is_float() {
            // F128 -> Integer: use x87 FISTTP for truncation toward zero.
            // rax holds f64 bits. Load as f64 into x87, then FISTTP to integer.
            // We use subq/movq/addq instead of push/pop to avoid peephole elimination.
            if to_ty.is_signed() || to_ty == IrType::Ptr {
                self.state.emit("    subq $8, %rsp");
                self.state.emit("    movq %rax, (%rsp)");
                self.state.emit("    fldl (%rsp)");
                self.state.emit("    fisttpq (%rsp)");
                self.state.emit("    movq (%rsp), %rax");
                self.state.emit("    addq $8, %rsp");
                // Narrow if target is smaller than 64-bit
                if to_ty.size() < 8 && to_ty != IrType::Ptr {
                    match to_ty {
                        IrType::I8 => self.state.emit("    movsbq %al, %rax"),
                        IrType::I16 => self.state.emit("    movswq %ax, %rax"),
                        IrType::I32 => self.state.emit("    movslq %eax, %rax"),
                        _ => {}
                    }
                }
            } else {
                // F128 -> unsigned integer
                if to_ty == IrType::U64 {
                    // Handle values >= 2^63 that overflow signed FISTTP
                    let big_label = self.state.fresh_label("ld2u_big");
                    let done_label = self.state.fresh_label("ld2u_done");
                    // Load f64 into x87 and compare with 2^63
                    self.state.emit("    subq $8, %rsp");
                    self.state.emit("    movq %rax, (%rsp)");
                    self.state.emit("    fldl (%rsp)");
                    // Load 2^63 as f64 for comparison
                    self.state.emit("    movabsq $4890909195324358656, %rcx"); // 2^63 as f64 bits
                    self.state.emit("    movq %rcx, (%rsp)");
                    self.state.emit("    fldl (%rsp)");   // ST0 = 2^63, ST1 = value
                    self.state.emit("    fcomip %st(1), %st");  // compare and pop 2^63
                    self.state.out.emit_jcc_label("    jbe", &big_label); // if 2^63 <= value
                    // Small case: value < 2^63
                    self.state.emit("    fisttpq (%rsp)");
                    self.state.emit("    movq (%rsp), %rax");
                    self.state.emit("    addq $8, %rsp");
                    self.state.out.emit_jmp_label(&done_label);
                    // Big case: value >= 2^63
                    self.state.out.emit_named_label(&big_label);
                    self.state.emit("    movabsq $4890909195324358656, %rcx");
                    self.state.emit("    movq %rcx, (%rsp)");
                    self.state.emit("    fldl (%rsp)");   // ST0 = 2^63, ST1 = value
                    self.state.emit("    fsubrp %st, %st(1)"); // ST0 = value - 2^63
                    self.state.emit("    fisttpq (%rsp)");
                    self.state.emit("    movq (%rsp), %rax");
                    self.state.emit("    addq $8, %rsp");
                    self.state.emit("    movabsq $9223372036854775808, %rcx");
                    self.state.emit("    addq %rcx, %rax");
                    self.state.out.emit_named_label(&done_label);
                } else {
                    // Smaller unsigned types: FISTTP then truncate
                    self.state.emit("    subq $8, %rsp");
                    self.state.emit("    movq %rax, (%rsp)");
                    self.state.emit("    fldl (%rsp)");
                    self.state.emit("    fisttpq (%rsp)");
                    self.state.emit("    movq (%rsp), %rax");
                    self.state.emit("    addq $8, %rsp");
                    match to_ty {
                        IrType::U8 => self.state.emit("    movzbq %al, %rax"),
                        IrType::U16 => self.state.emit("    movzwq %ax, %rax"),
                        IrType::U32 => self.state.emit("    movl %eax, %eax"),
                        _ => {}
                    }
                }
            }
            return;
        }
        if from_ty == IrType::F128 && to_ty == IrType::F32 {
            // F128 -> F32: load f64 into x87, convert to f32
            self.state.emit("    subq $8, %rsp");
            self.state.emit("    movq %rax, (%rsp)");
            self.state.emit("    fldl (%rsp)");
            self.state.emit("    fstps (%rsp)");
            self.state.emit("    movl (%rsp), %eax");
            self.state.emit("    addq $8, %rsp");
            return;
        }
        if from_ty == IrType::F32 && to_ty == IrType::F128 {
            // F32 -> F128: widen f32 to f64 (same as F32->F64 since F128 is stored as f64 in regs)
            self.state.emit("    movd %eax, %xmm0");
            self.state.emit("    cvtss2sd %xmm0, %xmm0");
            self.state.emit("    movq %xmm0, %rax");
            return;
        }
        // Note: F64 <-> F128 falls through to classify_cast() which returns Noop,
        // since F128 is stored as f64 bit-pattern in registers. This is correct.
        match classify_cast(from_ty, to_ty) {
            CastKind::Noop => {}

            CastKind::FloatToSigned { from_f64 } => {
                if from_f64 {
                    self.state.emit("    movq %rax, %xmm0");
                    self.state.emit("    cvttsd2siq %xmm0, %rax");
                } else {
                    self.state.emit("    movd %eax, %xmm0");
                    self.state.emit("    cvttss2siq %xmm0, %rax");
                }
            }

            CastKind::FloatToUnsigned { from_f64, to_u64 } => {
                if from_f64 {
                    self.state.emit("    movq %rax, %xmm0");
                    if to_u64 {
                        // Handle values >= 2^63 that overflow signed conversion
                        let big_label = self.state.fresh_label("f2u_big");
                        let done_label = self.state.fresh_label("f2u_done");
                        self.state.emit("    movabsq $4890909195324358656, %rcx"); // 2^63 as f64 bits
                        self.state.emit("    movq %rcx, %xmm1");
                        self.state.emit("    ucomisd %xmm1, %xmm0");
                        self.state.out.emit_jcc_label("    jae", &big_label);
                        self.state.emit("    cvttsd2siq %xmm0, %rax");
                        self.state.out.emit_jmp_label(&done_label);
                        self.state.out.emit_named_label(&big_label);
                        self.state.emit("    subsd %xmm1, %xmm0");
                        self.state.emit("    cvttsd2siq %xmm0, %rax");
                        self.state.emit("    movabsq $9223372036854775808, %rcx"); // 2^63 as int
                        self.state.emit("    addq %rcx, %rax");
                        self.state.out.emit_named_label(&done_label);
                    } else {
                        self.state.emit("    cvttsd2siq %xmm0, %rax");
                    }
                } else {
                    self.state.emit("    movd %eax, %xmm0");
                    self.state.emit("    cvttss2siq %xmm0, %rax");
                }
            }

            CastKind::SignedToFloat { to_f64, from_ty } => {
                // For sub-64-bit signed sources, sign-extend to 64 bits first.
                // The value in rax may be zero-extended (e.g., from a U32->I32 noop cast),
                // so we need explicit sign-extension before the 64-bit int-to-float conversion.
                match from_ty.size() {
                    1 => self.state.emit("    movsbq %al, %rax"),
                    2 => self.state.emit("    movswq %ax, %rax"),
                    4 => self.state.emit("    movslq %eax, %rax"),
                    _ => {} // 8 bytes (I64): already full width
                }
                if to_f64 {
                    self.state.emit("    cvtsi2sdq %rax, %xmm0");
                    self.state.emit("    movq %xmm0, %rax");
                } else {
                    self.state.emit("    cvtsi2ssq %rax, %xmm0");
                    self.state.emit("    movd %xmm0, %eax");
                }
            }

            CastKind::UnsignedToFloat { to_f64, from_u64 } => {
                if from_u64 {
                    // Handle U64 values >= 2^63 via shift+round trick
                    let big_label = self.state.fresh_label("u2f_big");
                    let done_label = self.state.fresh_label("u2f_done");
                    self.state.emit("    testq %rax, %rax");
                    self.state.out.emit_jcc_label("    js", &big_label);
                    if to_f64 {
                        self.state.emit("    cvtsi2sdq %rax, %xmm0");
                    } else {
                        self.state.emit("    cvtsi2ssq %rax, %xmm0");
                    }
                    self.state.out.emit_jmp_label(&done_label);
                    self.state.out.emit_named_label(&big_label);
                    self.state.emit("    movq %rax, %rcx");
                    self.state.emit("    shrq $1, %rax");
                    self.state.emit("    andq $1, %rcx");
                    self.state.emit("    orq %rcx, %rax");
                    if to_f64 {
                        self.state.emit("    cvtsi2sdq %rax, %xmm0");
                        self.state.emit("    addsd %xmm0, %xmm0");
                    } else {
                        self.state.emit("    cvtsi2ssq %rax, %xmm0");
                        self.state.emit("    addss %xmm0, %xmm0");
                    }
                    self.state.out.emit_named_label(&done_label);
                    if to_f64 {
                        self.state.emit("    movq %xmm0, %rax");
                    } else {
                        self.state.emit("    movd %xmm0, %eax");
                    }
                } else {
                    // U32 or smaller: zero-extended in rax, fits in signed i64
                    if to_f64 {
                        self.state.emit("    cvtsi2sdq %rax, %xmm0");
                        self.state.emit("    movq %xmm0, %rax");
                    } else {
                        self.state.emit("    cvtsi2ssq %rax, %xmm0");
                        self.state.emit("    movd %xmm0, %eax");
                    }
                }
            }

            CastKind::FloatToFloat { widen } => {
                if widen {
                    self.state.emit("    movd %eax, %xmm0");
                    self.state.emit("    cvtss2sd %xmm0, %xmm0");
                    self.state.emit("    movq %xmm0, %rax");
                } else {
                    self.state.emit("    movq %rax, %xmm0");
                    self.state.emit("    cvtsd2ss %xmm0, %xmm0");
                    self.state.emit("    movd %xmm0, %eax");
                }
            }

            CastKind::SignedToUnsignedSameSize { to_ty } => {
                // Clear sign-extended upper bits
                match to_ty {
                    IrType::U8 => self.state.emit("    movzbq %al, %rax"),
                    IrType::U16 => self.state.emit("    movzwq %ax, %rax"),
                    IrType::U32 => self.state.emit("    movl %eax, %eax"),
                    _ => {} // U64: no masking needed
                }
            }

            CastKind::IntWiden { from_ty, to_ty } => {
                if from_ty.is_unsigned() {
                    match from_ty {
                        IrType::U8 => self.state.emit("    movzbq %al, %rax"),
                        IrType::U16 => self.state.emit("    movzwq %ax, %rax"),
                        IrType::U32 => self.state.emit("    movl %eax, %eax"),
                        _ => {}
                    }
                } else if to_ty == IrType::U32 {
                    match from_ty {
                        IrType::I8 => self.state.emit("    movsbl %al, %eax"),
                        IrType::I16 => self.state.emit("    movswl %ax, %eax"),
                        _ => {}
                    }
                } else {
                    match from_ty {
                        IrType::I8 => self.state.emit("    movsbq %al, %rax"),
                        IrType::I16 => self.state.emit("    movswq %ax, %rax"),
                        IrType::I32 => self.state.emit("    cltq"),
                        _ => {}
                    }
                }
            }

            CastKind::IntNarrow { to_ty } => {
                // Truncate then sign/zero-extend to fill 64-bit register correctly
                match to_ty {
                    IrType::I8 => self.state.emit("    movsbq %al, %rax"),
                    IrType::U8 => self.state.emit("    movzbq %al, %rax"),
                    IrType::I16 => self.state.emit("    movswq %ax, %rax"),
                    IrType::U16 => self.state.emit("    movzwq %ax, %rax"),
                    IrType::I32 => self.state.emit("    movslq %eax, %rax"),
                    IrType::U32 => self.state.emit("    movl %eax, %eax"),
                    _ => {}
                }
            }

            // F128 cast kinds: unreachable on x86 because classify_cast() reduces
            // F128 to F64, and F128-specific casts are handled before this match.
            CastKind::SignedToF128 { .. }
            | CastKind::UnsignedToF128 { .. }
            | CastKind::F128ToSigned { .. }
            | CastKind::F128ToUnsigned { .. }
            | CastKind::FloatToF128 { .. }
            | CastKind::F128ToFloat { .. } => {
                // On x86, F128 is x87 80-bit and handled by special-case code above.
            }
        }
    }

    /// Load an F128 operand into x87 ST(0), using full 80-bit precision when available.
    /// This is used for F128 arithmetic and for pushing F128 call arguments.
    pub(super) fn emit_f128_load_to_x87(&mut self, operand: &Operand) {
        match operand {
            Operand::Const(ref c) => {
                match c {
                    IrConst::LongDouble(_, f128_raw) => {
                        // Convert f128 bytes to x87, push to stack and fldt
                        let x87 = crate::common::long_double::f128_bytes_to_x87_bytes(f128_raw);
                        let lo = u64::from_le_bytes(x87[0..8].try_into().unwrap());
                        let hi = u16::from_le_bytes(x87[8..10].try_into().unwrap());
                        self.state.emit("    subq $16, %rsp");
                        self.state.out.emit_instr_imm_reg("    movabsq", lo as i64, "rax");
                        self.state.emit("    movq %rax, (%rsp)");
                        self.state.out.emit_instr_imm_reg("    movq", hi as i64, "rax");
                        self.state.emit("    movq %rax, 8(%rsp)");
                        self.state.emit("    fldt (%rsp)");
                        self.state.emit("    addq $16, %rsp");
                        self.state.reg_cache.invalidate_all();
                    }
                    _ => {
                        // f64 constant: push and fldl
                        let f64_val = c.to_f64().unwrap_or(0.0);
                        let bits = f64_val.to_bits();
                        self.state.emit("    subq $8, %rsp");
                        self.state.out.emit_instr_imm_reg("    movabsq", bits as i64, "rax");
                        self.state.emit("    movq %rax, (%rsp)");
                        self.state.emit("    fldl (%rsp)");
                        self.state.emit("    addq $8, %rsp");
                        self.state.reg_cache.invalidate_all();
                    }
                }
            }
            Operand::Value(ref v) => {
                if self.state.f128_direct_slots.contains(&v.0) {
                    // Full 80-bit x87 precision in the slot; use fldt
                    if let Some(slot) = self.state.get_slot(v.0) {
                        self.state.out.emit_instr_rbp("    fldt", slot.0);
                    } else {
                        // Fallback: load f64 from rax
                        self.operand_to_rax(operand);
                        self.state.emit("    subq $8, %rsp");
                        self.state.emit("    movq %rax, (%rsp)");
                        self.state.emit("    fldl (%rsp)");
                        self.state.emit("    addq $8, %rsp");
                    }
                } else if let Some(slot) = self.state.get_slot(v.0) {
                    if self.state.is_alloca(v.0) {
                        // Alloca containing a long double; use fldt from alloca
                        self.state.out.emit_instr_rbp("    fldt", slot.0);
                    } else {
                        // Regular f64 value in slot; push and fldl
                        self.state.out.emit_instr_rbp_reg("    movq", slot.0, "rax");
                        self.state.emit("    subq $8, %rsp");
                        self.state.emit("    movq %rax, (%rsp)");
                        self.state.emit("    fldl (%rsp)");
                        self.state.emit("    addq $8, %rsp");
                        self.state.reg_cache.invalidate_all();
                    }
                } else {
                    // No slot; use operand_to_rax which gets f64 bit pattern
                    self.operand_to_rax(operand);
                    self.state.emit("    subq $8, %rsp");
                    self.state.emit("    movq %rax, (%rsp)");
                    self.state.emit("    fldl (%rsp)");
                    self.state.emit("    addq $8, %rsp");
                }
            }
        }
    }
}
