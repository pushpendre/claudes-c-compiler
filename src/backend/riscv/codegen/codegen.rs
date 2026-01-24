use crate::ir::ir::*;
use crate::common::types::IrType;
use crate::backend::common::PtrDirective;
use crate::backend::codegen_shared::*;
use super::inline_asm::{RvConstraintKind, classify_rv_constraint};

/// RISC-V 64 code generator. Implements the ArchCodegen trait for the shared framework.
/// Uses standard RISC-V calling convention with stack-based allocation.
pub struct RiscvCodegen {
    pub(super) state: CodegenState,
    current_return_type: IrType,
    /// Number of named integer params for current variadic function.
    va_named_gp_count: usize,
    /// Current frame size (below s0, not including the register save area above s0).
    current_frame_size: i64,
    /// Whether the current function is variadic.
    is_variadic: bool,
    /// Scratch register indices for inline asm allocation.
    asm_gp_scratch_idx: usize,
    asm_fp_scratch_idx: usize,
}

impl RiscvCodegen {
    pub fn new() -> Self {
        Self {
            state: CodegenState::new(),
            current_return_type: IrType::I64,
            va_named_gp_count: 0,
            current_frame_size: 0,
            is_variadic: false,
            asm_gp_scratch_idx: 0,
            asm_fp_scratch_idx: 0,
        }
    }

    pub fn generate(mut self, module: &IrModule) -> String {
        generate_module(&mut self, module)
    }

    // --- RISC-V helpers ---

    /// Check if an immediate fits in a 12-bit signed field.
    fn fits_imm12(val: i64) -> bool {
        val >= -2048 && val <= 2047
    }

    /// Emit: store `reg` to `offset(s0)`, handling large offsets via t6.
    /// Uses t6 as scratch to avoid conflicts with t3-t5 call argument temps.
    fn emit_store_to_s0(&mut self, reg: &str, offset: i64, store_instr: &str) {
        if Self::fits_imm12(offset) {
            self.state.emit(&format!("    {} {}, {}(s0)", store_instr, reg, offset));
        } else {
            self.state.emit(&format!("    li t6, {}", offset));
            self.state.emit("    add t6, s0, t6");
            self.state.emit(&format!("    {} {}, 0(t6)", store_instr, reg));
        }
    }

    /// Emit: load from `offset(s0)` into `reg`, handling large offsets via t6.
    /// Uses t6 as scratch to avoid conflicts with t3-t5 call argument temps.
    fn emit_load_from_s0(&mut self, reg: &str, offset: i64, load_instr: &str) {
        if Self::fits_imm12(offset) {
            self.state.emit(&format!("    {} {}, {}(s0)", load_instr, reg, offset));
        } else {
            self.state.emit(&format!("    li t6, {}", offset));
            self.state.emit("    add t6, s0, t6");
            self.state.emit(&format!("    {} {}, 0(t6)", load_instr, reg));
        }
    }

    /// Emit: `dest_reg = s0 + offset`, handling large offsets.
    fn emit_addi_s0(&mut self, dest_reg: &str, offset: i64) {
        if Self::fits_imm12(offset) {
            self.state.emit(&format!("    addi {}, s0, {}", dest_reg, offset));
        } else {
            self.state.emit(&format!("    li {}, {}", dest_reg, offset));
            self.state.emit(&format!("    add {}, s0, {}", dest_reg, dest_reg));
        }
    }

    /// Emit: store `reg` to `offset(sp)`, handling large offsets via t6.
    /// Used for stack overflow arguments in emit_call.
    fn emit_store_to_sp(&mut self, reg: &str, offset: i64, store_instr: &str) {
        if Self::fits_imm12(offset) {
            self.state.emit(&format!("    {} {}, {}(sp)", store_instr, reg, offset));
        } else {
            self.state.emit(&format!("    li t6, {}", offset));
            self.state.emit("    add t6, sp, t6");
            self.state.emit(&format!("    {} {}, 0(t6)", store_instr, reg));
        }
    }

    /// Emit: load from `offset(sp)` into `reg`, handling large offsets via t6.
    /// Used for loading stack overflow arguments in emit_call.
    fn emit_load_from_sp(&mut self, reg: &str, offset: i64, load_instr: &str) {
        if Self::fits_imm12(offset) {
            self.state.emit(&format!("    {} {}, {}(sp)", load_instr, reg, offset));
        } else {
            self.state.emit(&format!("    li t6, {}", offset));
            self.state.emit("    add t6, sp, t6");
            self.state.emit(&format!("    {} {}, 0(t6)", load_instr, reg));
        }
    }

    /// Emit: `sp = sp + imm`, handling large immediates via t6.
    /// Positive imm deallocates stack, negative allocates.
    fn emit_addi_sp(&mut self, imm: i64) {
        if Self::fits_imm12(imm) {
            self.state.emit(&format!("    addi sp, sp, {}", imm));
        } else if imm > 0 {
            self.state.emit(&format!("    li t6, {}", imm));
            self.state.emit("    add sp, sp, t6");
        } else {
            self.state.emit(&format!("    li t6, {}", -imm));
            self.state.emit("    sub sp, sp, t6");
        }
    }

    /// Emit prologue: allocate stack and save ra/s0.
    ///
    /// Stack layout (s0 points to top of frame = old sp):
    ///   s0 - 8:  saved ra
    ///   s0 - 16: saved s0
    ///   s0 - 16 - ...: local data (allocas and value slots)
    ///   sp: bottom of frame
    fn emit_prologue_riscv(&mut self, frame_size: i64) {
        // For variadic functions, the register save area (64 bytes for a0-a7) is
        // placed ABOVE s0, contiguous with the caller's stack-passed arguments.
        // Layout: s0+0..s0+56 = a0..a7, s0+64+ = caller stack args.
        // This means total_alloc = frame_size + 64 for variadic, but s0 = sp + frame_size.
        let total_alloc = if self.is_variadic { frame_size + 64 } else { frame_size };

        // Small-frame path requires ALL immediates to fit in 12 bits:
        // -total_alloc (sp adjust), and frame_size (s0 setup).
        if Self::fits_imm12(-total_alloc) && Self::fits_imm12(total_alloc) {
            // Small frame: all offsets fit in 12-bit immediates
            self.state.emit(&format!("    addi sp, sp, -{}", total_alloc));
            // ra and s0 are saved relative to s0, which is sp + frame_size
            // (NOT sp + total_alloc for variadic functions!)
            self.state.emit(&format!("    sd ra, {}(sp)", frame_size - 8));
            self.state.emit(&format!("    sd s0, {}(sp)", frame_size - 16));
            self.state.emit(&format!("    addi s0, sp, {}", frame_size));
        } else {
            // Large frame: use t0 for offsets
            self.state.emit(&format!("    li t0, {}", total_alloc));
            self.state.emit("    sub sp, sp, t0");
            // Compute s0 = sp + frame_size (NOT total_alloc)
            self.state.emit(&format!("    li t0, {}", frame_size));
            self.state.emit("    add t0, sp, t0");
            // Save ra and old s0 at s0-8, s0-16
            self.state.emit("    sd ra, -8(t0)");
            self.state.emit("    sd s0, -16(t0)");
            self.state.emit("    mv s0, t0");
        }
    }

    /// Emit epilogue: restore ra/s0 and deallocate stack.
    fn emit_epilogue_riscv(&mut self, frame_size: i64) {
        let total_alloc = if self.is_variadic { frame_size + 64 } else { frame_size };
        if Self::fits_imm12(-total_alloc) && Self::fits_imm12(total_alloc) {
            // Small frame: restore from known sp offsets
            // ra/s0 saved at sp + frame_size - 8/16 (relative to current sp)
            self.state.emit(&format!("    ld ra, {}(sp)", frame_size - 8));
            self.state.emit(&format!("    ld s0, {}(sp)", frame_size - 16));
            self.state.emit(&format!("    addi sp, sp, {}", total_alloc));
        } else {
            // Large frame: restore from s0-relative offsets (always fit in imm12).
            self.state.emit("    ld ra, -8(s0)");
            self.state.emit("    ld t0, -16(s0)");
            // For variadic functions, s0 + 64 = old_sp, so sp = s0 + 64
            if self.is_variadic {
                self.state.emit("    addi sp, s0, 64");
            } else {
                self.state.emit("    mv sp, s0");
            }
            self.state.emit("    mv s0, t0");
        }
    }

    /// Load an operand into t0.
    fn operand_to_t0(&mut self, op: &Operand) {
        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::I8(v) => self.state.emit(&format!("    li t0, {}", v)),
                    IrConst::I16(v) => self.state.emit(&format!("    li t0, {}", v)),
                    IrConst::I32(v) => self.state.emit(&format!("    li t0, {}", v)),
                    IrConst::I64(v) => self.state.emit(&format!("    li t0, {}", v)),
                    IrConst::F32(v) => {
                        let bits = v.to_bits() as u64;
                        self.state.emit(&format!("    li t0, {}", bits as i64));
                    }
                    IrConst::F64(v) => {
                        let bits = v.to_bits();
                        self.state.emit(&format!("    li t0, {}", bits as i64));
                    }
                    // LongDouble at computation level is treated as F64
                    IrConst::LongDouble(v) => {
                        let bits = v.to_bits();
                        self.state.emit(&format!("    li t0, {}", bits as i64));
                    }
                    IrConst::I128(v) => self.state.emit(&format!("    li t0, {}", *v as i64)), // truncate
                    IrConst::Zero => self.state.emit("    li t0, 0"),
                }
            }
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    if self.state.is_alloca(v.0) {
                        self.emit_addi_s0("t0", slot.0);
                    } else {
                        self.emit_load_from_s0("t0", slot.0, "ld");
                    }
                } else {
                    self.state.emit("    li t0, 0");
                }
            }
        }
    }

    /// Store t0 to a value's stack slot.
    fn store_t0_to(&mut self, dest: &Value) {
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.emit_store_to_s0("t0", slot.0, "sd");
        }
    }

    // --- 128-bit integer helpers ---
    // Convention: 128-bit values use t0 (low 64 bits) and t1 (high 64 bits).
    // Stack slots for 128-bit values are 16 bytes: slot(s0) = low, slot+8(s0) = high.

    /// Load a 128-bit operand into t0 (low) : t1 (high).
    fn operand_to_t0_t1(&mut self, op: &Operand) {
        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::I128(v) => {
                        let low = *v as u64 as i64;
                        let high = (*v >> 64) as u64 as i64;
                        self.state.emit(&format!("    li t0, {}", low));
                        self.state.emit(&format!("    li t1, {}", high));
                    }
                    IrConst::Zero => {
                        self.state.emit("    li t0, 0");
                        self.state.emit("    li t1, 0");
                    }
                    _ => {
                        self.operand_to_t0(op);
                        self.state.emit("    li t1, 0");
                    }
                }
            }
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    if self.state.is_alloca(v.0) {
                        self.emit_addi_s0("t0", slot.0);
                        self.state.emit("    li t1, 0");
                    } else {
                        // 128-bit value in 16-byte stack slot
                        self.emit_load_from_s0("t0", slot.0, "ld");
                        self.emit_load_from_s0("t1", slot.0 + 8, "ld");
                    }
                } else {
                    self.state.emit("    li t0, 0");
                    self.state.emit("    li t1, 0");
                }
            }
        }
    }

    /// Store t0 (low) : t1 (high) to a 128-bit value's stack slot.
    fn store_t0_t1_to(&mut self, dest: &Value) {
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.emit_store_to_s0("t0", slot.0, "sd");
            self.emit_store_to_s0("t1", slot.0 + 8, "sd");
        }
    }

    /// Prepare a 128-bit binary operation: load lhs into t3:t4, rhs into t5:t6.
    fn prep_i128_binop(&mut self, lhs: &Operand, rhs: &Operand) {
        self.operand_to_t0_t1(lhs);
        self.state.emit("    mv t3, t0");
        self.state.emit("    mv t4, t1");
        self.operand_to_t0_t1(rhs);
        self.state.emit("    mv t5, t0");
        self.state.emit("    mv t6, t1");
    }

    /// Emit a 128-bit integer binary operation.
    fn emit_i128_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand) {
        match op {
            IrBinOp::Add => {
                // 128-bit add with carry: result_lo = a_lo + b_lo, carry = (result_lo < a_lo)
                // result_hi = a_hi + b_hi + carry
                self.prep_i128_binop(lhs, rhs);
                self.state.emit("    add t0, t3, t5");        // t0 = a_lo + b_lo
                self.state.emit("    sltu t2, t0, t3");       // t2 = carry
                self.state.emit("    add t1, t4, t6");        // t1 = a_hi + b_hi
                self.state.emit("    add t1, t1, t2");        // t1 += carry
            }
            IrBinOp::Sub => {
                // 128-bit sub with borrow: result_lo = a_lo - b_lo, borrow = (a_lo < b_lo)
                // result_hi = a_hi - b_hi - borrow
                self.prep_i128_binop(lhs, rhs);
                self.state.emit("    sltu t2, t3, t5");       // t2 = borrow (a_lo < b_lo)
                self.state.emit("    sub t0, t3, t5");        // t0 = a_lo - b_lo
                self.state.emit("    sub t1, t4, t6");        // t1 = a_hi - b_hi
                self.state.emit("    sub t1, t1, t2");        // t1 -= borrow
            }
            IrBinOp::Mul => {
                // 128-bit multiply using mulhu for widening
                // result_lo = a_lo * b_lo (low 64 bits)
                // result_hi = mulhu(a_lo, b_lo) + a_hi*b_lo + a_lo*b_hi
                self.prep_i128_binop(lhs, rhs);
                // t3:t4 = lhs, t5:t6 = rhs
                self.state.emit("    mul t0, t3, t5");        // t0 = lo(a_lo * b_lo)
                self.state.emit("    mulhu t1, t3, t5");      // t1 = hi(a_lo * b_lo)
                self.state.emit("    mul t2, t4, t5");        // t2 = a_hi * b_lo (low 64)
                self.state.emit("    add t1, t1, t2");        // t1 += a_hi * b_lo
                self.state.emit("    mul t2, t3, t6");        // t2 = a_lo * b_hi (low 64)
                self.state.emit("    add t1, t1, t2");        // t1 += a_lo * b_hi
            }
            IrBinOp::And => {
                self.prep_i128_binop(lhs, rhs);
                self.state.emit("    and t0, t3, t5");
                self.state.emit("    and t1, t4, t6");
            }
            IrBinOp::Or => {
                self.prep_i128_binop(lhs, rhs);
                self.state.emit("    or t0, t3, t5");
                self.state.emit("    or t1, t4, t6");
            }
            IrBinOp::Xor => {
                self.prep_i128_binop(lhs, rhs);
                self.state.emit("    xor t0, t3, t5");
                self.state.emit("    xor t1, t4, t6");
            }
            IrBinOp::Shl => {
                // 128-bit left shift
                self.prep_i128_binop(lhs, rhs);
                // t3:t4 = value, t5 = shift amount
                let lbl = self.state.fresh_label("shl128");
                let done = self.state.fresh_label("shl128_done");
                let noop = self.state.fresh_label("shl128_noop");
                self.state.emit("    andi t5, t5, 127");        // mask to 0-127
                self.state.emit(&format!("    beqz t5, {}", noop));
                self.state.emit("    li t2, 64");
                self.state.emit(&format!("    bge t5, t2, {}", lbl));
                // shift < 64: hi = (hi << n) | (lo >> (64-n)), lo = lo << n
                self.state.emit("    sll t1, t4, t5");
                self.state.emit("    sub t2, t2, t5");  // t2 = 64 - n
                self.state.emit("    srl t6, t3, t2");
                self.state.emit("    or t1, t1, t6");
                self.state.emit("    sll t0, t3, t5");
                self.state.emit(&format!("    j {}", done));
                // shift >= 64: hi = lo << (n-64), lo = 0
                self.state.emit(&format!("{}:", lbl));
                self.state.emit("    li t2, 64");
                self.state.emit("    sub t5, t5, t2");
                self.state.emit("    sll t1, t3, t5");
                self.state.emit("    li t0, 0");
                self.state.emit(&format!("    j {}", done));
                // shift == 0: just copy
                self.state.emit(&format!("{}:", noop));
                self.state.emit("    mv t0, t3");
                self.state.emit("    mv t1, t4");
                self.state.emit(&format!("{}:", done));
            }
            IrBinOp::LShr => {
                // 128-bit logical right shift
                self.prep_i128_binop(lhs, rhs);
                let lbl = self.state.fresh_label("lshr128");
                let done = self.state.fresh_label("lshr128_done");
                let noop = self.state.fresh_label("lshr128_noop");
                self.state.emit("    andi t5, t5, 127");
                self.state.emit(&format!("    beqz t5, {}", noop));
                self.state.emit("    li t2, 64");
                self.state.emit(&format!("    bge t5, t2, {}", lbl));
                // shift < 64: lo = (lo >> n) | (hi << (64-n)), hi = hi >> n
                self.state.emit("    srl t0, t3, t5");
                self.state.emit("    sub t2, t2, t5");
                self.state.emit("    sll t6, t4, t2");
                self.state.emit("    or t0, t0, t6");
                self.state.emit("    srl t1, t4, t5");
                self.state.emit(&format!("    j {}", done));
                // shift >= 64: lo = hi >> (n-64), hi = 0
                self.state.emit(&format!("{}:", lbl));
                self.state.emit("    li t2, 64");
                self.state.emit("    sub t5, t5, t2");
                self.state.emit("    srl t0, t4, t5");
                self.state.emit("    li t1, 0");
                self.state.emit(&format!("    j {}", done));
                self.state.emit(&format!("{}:", noop));
                self.state.emit("    mv t0, t3");
                self.state.emit("    mv t1, t4");
                self.state.emit(&format!("{}:", done));
            }
            IrBinOp::AShr => {
                // 128-bit arithmetic right shift
                self.prep_i128_binop(lhs, rhs);
                let lbl = self.state.fresh_label("ashr128");
                let done = self.state.fresh_label("ashr128_done");
                let noop = self.state.fresh_label("ashr128_noop");
                self.state.emit("    andi t5, t5, 127");
                self.state.emit(&format!("    beqz t5, {}", noop));
                self.state.emit("    li t2, 64");
                self.state.emit(&format!("    bge t5, t2, {}", lbl));
                // shift < 64: lo = (lo >> n) | (hi << (64-n)), hi = hi >>a n
                self.state.emit("    srl t0, t3, t5");
                self.state.emit("    sub t2, t2, t5");
                self.state.emit("    sll t6, t4, t2");
                self.state.emit("    or t0, t0, t6");
                self.state.emit("    sra t1, t4, t5");
                self.state.emit(&format!("    j {}", done));
                // shift >= 64: lo = hi >>a (n-64), hi = hi >>a 63
                self.state.emit(&format!("{}:", lbl));
                self.state.emit("    li t2, 64");
                self.state.emit("    sub t5, t5, t2");
                self.state.emit("    sra t0, t4, t5");
                self.state.emit("    srai t1, t4, 63");
                self.state.emit(&format!("    j {}", done));
                self.state.emit(&format!("{}:", noop));
                self.state.emit("    mv t0, t3");
                self.state.emit("    mv t1, t4");
                self.state.emit(&format!("{}:", done));
            }
            IrBinOp::SDiv | IrBinOp::UDiv | IrBinOp::SRem | IrBinOp::URem => {
                // Call compiler-rt helper functions
                let func_name = match op {
                    IrBinOp::SDiv => "__divti3",
                    IrBinOp::UDiv => "__udivti3",
                    IrBinOp::SRem => "__modti3",
                    IrBinOp::URem => "__umodti3",
                    _ => unreachable!(),
                };
                // RISC-V LP64D: first 128-bit arg in a0:a1, second in a2:a3
                self.operand_to_t0_t1(lhs);
                self.state.emit("    mv a0, t0");
                self.state.emit("    mv a1, t1");
                self.operand_to_t0_t1(rhs);
                self.state.emit("    mv a2, t0");
                self.state.emit("    mv a3, t1");
                self.state.emit(&format!("    call {}", func_name));
                // Result in a0:a1
                self.state.emit("    mv t0, a0");
                self.state.emit("    mv t1, a1");
            }
        }
        self.store_t0_t1_to(dest);
    }

    /// Emit a 128-bit comparison.
    fn emit_i128_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand) {
        self.prep_i128_binop(lhs, rhs);
        // t3:t4 = lhs, t5:t6 = rhs
        match op {
            IrCmpOp::Eq => {
                // XOR both halves and OR the differences
                self.state.emit("    xor t0, t3, t5");
                self.state.emit("    xor t1, t4, t6");
                self.state.emit("    or t0, t0, t1");
                self.state.emit("    seqz t0, t0");
            }
            IrCmpOp::Ne => {
                self.state.emit("    xor t0, t3, t5");
                self.state.emit("    xor t1, t4, t6");
                self.state.emit("    or t0, t0, t1");
                self.state.emit("    snez t0, t0");
            }
            _ => {
                // Ordered comparisons: compare high halves first
                let hi_differ = self.state.fresh_label("cmp128_hi_diff");
                let hi_equal = self.state.fresh_label("cmp128_hi_eq");
                let done = self.state.fresh_label("cmp128_done");
                // Compare high halves
                self.state.emit(&format!("    bne t4, t6, {}", hi_differ));
                // High halves equal, compare low halves (always unsigned)
                self.state.emit(&format!("    j {}", hi_equal));
                // hi != hi: set result based on high comparison
                self.state.emit(&format!("{}:", hi_differ));
                match op {
                    IrCmpOp::Slt | IrCmpOp::Sle => self.state.emit("    slt t0, t4, t6"),
                    IrCmpOp::Sgt | IrCmpOp::Sge => self.state.emit("    slt t0, t6, t4"),
                    IrCmpOp::Ult | IrCmpOp::Ule => self.state.emit("    sltu t0, t4, t6"),
                    IrCmpOp::Ugt | IrCmpOp::Uge => self.state.emit("    sltu t0, t6, t4"),
                    _ => unreachable!(),
                }
                self.state.emit(&format!("    j {}", done));
                // High halves equal: compare low halves (unsigned for all orderings)
                self.state.emit(&format!("{}:", hi_equal));
                match op {
                    IrCmpOp::Slt | IrCmpOp::Ult => self.state.emit("    sltu t0, t3, t5"),
                    IrCmpOp::Sle | IrCmpOp::Ule => {
                        self.state.emit("    sltu t0, t5, t3");
                        self.state.emit("    xori t0, t0, 1");
                    }
                    IrCmpOp::Sgt | IrCmpOp::Ugt => self.state.emit("    sltu t0, t5, t3"),
                    IrCmpOp::Sge | IrCmpOp::Uge => {
                        self.state.emit("    sltu t0, t3, t5");
                        self.state.emit("    xori t0, t0, 1");
                    }
                    _ => unreachable!(),
                }
                self.state.emit(&format!("{}:", done));
            }
        }
        self.store_t0_to(dest);
    }

    fn store_for_type(ty: IrType) -> &'static str {
        match ty {
            IrType::I8 | IrType::U8 => "sb",
            IrType::I16 | IrType::U16 => "sh",
            IrType::I32 | IrType::U32 | IrType::F32 => "sw",
            _ => "sd",
        }
    }

    fn load_for_type(ty: IrType) -> &'static str {
        match ty {
            IrType::I8 => "lb",
            IrType::U8 => "lbu",
            IrType::I16 => "lh",
            IrType::U16 => "lhu",
            IrType::I32 => "lw",
            IrType::U32 | IrType::F32 => "lwu",
            _ => "ld",
        }
    }

}

const RISCV_ARG_REGS: [&str; 8] = ["a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"];

impl ArchCodegen for RiscvCodegen {
    fn state(&mut self) -> &mut CodegenState { &mut self.state }
    fn state_ref(&self) -> &CodegenState { &self.state }
    fn ptr_directive(&self) -> PtrDirective { PtrDirective::Dword }

    fn jump_mnemonic(&self) -> &'static str { "j" }
    fn trap_instruction(&self) -> &'static str { "ebreak" }

    /// Override emit_branch to use `jump <label>, t6` instead of `j <label>`.
    /// The `j` pseudo (JAL with rd=x0) has only ±1MB range. For large functions
    /// (e.g., oniguruma's match_at), intra-function branches can exceed this.
    /// The `jump` pseudo generates `auipc t6, ... ; jr t6` with ±2GB range.
    /// t6 is safe to clobber here because emit_branch is only called at block
    /// terminators where all scratch registers are dead.
    fn emit_branch(&mut self, label: &str) {
        self.state.emit(&format!("    jump {}, t6", label));
    }

    fn emit_branch_nonzero(&mut self, label: &str) {
        self.state.emit(&format!("    bnez t0, {}", label));
    }

    fn emit_jump_indirect(&mut self) {
        self.state.emit("    jr t0");
    }

    fn calculate_stack_space(&mut self, func: &IrFunction) -> i64 {
        let space = calculate_stack_space_common(&mut self.state, func, 16, |space, alloc_size, align| {
            // RISC-V uses negative offsets from s0 (frame pointer)
            // Honor alignment: round up space to alignment boundary before allocating
            let effective_align = if align > 0 { align.max(8) } else { 8 };
            let alloc = ((alloc_size + 7) & !7).max(8);
            let new_space = ((space + alloc + effective_align - 1) / effective_align) * effective_align;
            (-(new_space as i64), new_space)
        });

        // For variadic functions, count named GP params.
        // The register save area (64 bytes for a0-a7) is placed ABOVE s0
        // (at positive offsets from s0) so it's contiguous with the caller's
        // stack-passed arguments. This means the frame_size below s0 does NOT
        // include the register save area; it's accounted for in the prologue.
        if func.is_variadic {
            self.va_named_gp_count = func.params.len().min(8);
            self.is_variadic = true;
        } else {
            self.is_variadic = false;
        }

        space
    }

    fn aligned_frame_size(&self, raw_space: i64) -> i64 {
        (raw_space + 15) & !15
    }

    fn emit_prologue(&mut self, func: &IrFunction, frame_size: i64) {
        self.current_return_type = func.return_type;
        self.current_frame_size = frame_size;
        self.emit_prologue_riscv(frame_size);
    }

    fn emit_epilogue(&mut self, frame_size: i64) {
        self.emit_epilogue_riscv(frame_size);
    }

    fn emit_store_params(&mut self, func: &IrFunction) {
        // For variadic functions: save all integer register args (a0-a7) to the
        // register save area at POSITIVE offsets from s0 (above the frame pointer).
        // Layout: a0 at s0+0, a1 at s0+8, ..., a7 at s0+56.
        // This makes the register save area contiguous with the caller's stack-passed
        // arguments at s0+64, s0+72, etc.
        if func.is_variadic {
            for i in 0..8usize {
                let offset = (i as i64) * 8;
                self.emit_store_to_s0(RISCV_ARG_REGS[i], offset, "sd");
            }
        }

        let float_arg_regs = ["fa0", "fa1", "fa2", "fa3", "fa4", "fa5", "fa6", "fa7"];
        let mut int_reg_idx = 0usize;
        let mut float_reg_idx = 0usize;
        // Stack-passed params are at positive offsets from s0.
        // For non-variadic: s0 = old_sp, stack params at s0+0, s0+8, ...
        // For variadic: s0 = old_sp - 64, stack params at s0+64, s0+72, ...
        //   (because the register save area occupies s0+0..s0+56)
        let mut stack_param_offset: i64 = if func.is_variadic { 64 } else { 0 };

        // Phase 1: If there are any F128 params in GP registers, save all GP and FP arg regs
        // to the stack first, because __trunctfdf2 calls will clobber them.
        // Save area layout: a0-a7 (64 bytes) then fa0-fa7 (64 bytes) = 128 bytes total.
        let has_f128_reg_params = func.params.iter().any(|p| {
            p.ty.is_long_double()
        });
        let f128_save_offset: i64 = if has_f128_reg_params && !func.is_variadic {
            self.state.emit("    addi sp, sp, -128");
            // Save GP arg regs a0-a7
            for i in 0..8usize {
                self.state.emit(&format!("    sd {}, {}(sp)", RISCV_ARG_REGS[i], i * 8));
            }
            // Save FP arg regs fa0-fa7
            for i in 0..8usize {
                self.state.emit(&format!("    fsd fa{}, {}(sp)", i, 64 + i * 8));
            }
            0i64 // base offset from sp where a0 is saved; fa0 at offset 64
        } else {
            0
        };

        for (_i, param) in func.params.iter().enumerate() {
            let is_long_double = param.ty.is_long_double();
            let is_i128_param = is_i128_type(param.ty);
            let is_float = param.ty.is_float() && !is_long_double;

            // For variadic functions: ALL args (including floats) go through GP registers.
            // For non-variadic: FP args go to fa0-fa7 first, then spill to GP regs.
            let is_float_in_gpr = if func.is_variadic {
                false // In variadic functions, floats are always in GP regs (handled in else branch)
            } else {
                is_float && float_reg_idx >= 8 && int_reg_idx < 8
            };

            let is_stack_passed = if is_i128_param {
                // 128-bit integer needs aligned register pair
                let aligned = (int_reg_idx + 1) & !1;
                aligned + 1 >= 8
            } else if func.is_variadic {
                if is_long_double {
                    let aligned = (int_reg_idx + 1) & !1;
                    aligned + 1 >= 8
                } else {
                    int_reg_idx >= 8
                }
            } else if is_long_double {
                let aligned = (int_reg_idx + 1) & !1;
                aligned + 1 >= 8
            } else if is_float {
                float_reg_idx >= 8 && int_reg_idx >= 8
            } else {
                int_reg_idx >= 8
            };

            if param.name.is_empty() {
                if is_stack_passed {
                    if is_i128_param || is_long_double {
                        stack_param_offset = (stack_param_offset + 15) & !15;
                        stack_param_offset += 16;
                        int_reg_idx = 8;
                    } else {
                        stack_param_offset += 8;
                    }
                } else if is_i128_param {
                    if int_reg_idx % 2 != 0 { int_reg_idx += 1; }
                    int_reg_idx += 2;
                } else if is_long_double {
                    if int_reg_idx % 2 != 0 { int_reg_idx += 1; }
                    int_reg_idx += 2;
                } else if func.is_variadic {
                    int_reg_idx += 1;
                } else if is_float_in_gpr {
                    int_reg_idx += 1;
                } else if is_float {
                    float_reg_idx += 1;
                } else {
                    int_reg_idx += 1;
                }
                continue;
            }
            if let Some((dest, ty)) = find_param_alloca(func, _i) {
                if let Some(slot) = self.state.get_slot(dest.0) {
                    // Handle i128 parameters first (both variadic and non-variadic)
                    if is_i128_param && !is_stack_passed {
                        // 128-bit integer in aligned GP register pair
                        if int_reg_idx % 2 != 0 { int_reg_idx += 1; }
                        if func.is_variadic {
                            // Load from register save area at s0+idx*8
                            let lo_off = (int_reg_idx as i64) * 8;
                            let hi_off = ((int_reg_idx + 1) as i64) * 8;
                            self.emit_load_from_s0("t0", lo_off, "ld");
                            self.emit_store_to_s0("t0", slot.0, "sd");
                            self.emit_load_from_s0("t0", hi_off, "ld");
                            self.emit_store_to_s0("t0", slot.0 + 8, "sd");
                        } else if has_f128_reg_params {
                            // Load from GP save area
                            let lo_off = f128_save_offset + (int_reg_idx as i64) * 8;
                            let hi_off = f128_save_offset + ((int_reg_idx + 1) as i64) * 8;
                            self.state.emit(&format!("    ld t0, {}(sp)", lo_off));
                            self.emit_store_to_s0("t0", slot.0, "sd");
                            self.state.emit(&format!("    ld t0, {}(sp)", hi_off));
                            self.emit_store_to_s0("t0", slot.0 + 8, "sd");
                        } else {
                            // Direct from registers
                            self.emit_store_to_s0(RISCV_ARG_REGS[int_reg_idx], slot.0, "sd");
                            self.emit_store_to_s0(RISCV_ARG_REGS[int_reg_idx + 1], slot.0 + 8, "sd");
                        }
                        int_reg_idx += 2;
                    } else if is_i128_param && is_stack_passed {
                        // 128-bit integer on stack
                        stack_param_offset = (stack_param_offset + 15) & !15;
                        self.emit_load_from_s0("t0", stack_param_offset, "ld");
                        self.emit_store_to_s0("t0", slot.0, "sd");
                        self.emit_load_from_s0("t0", stack_param_offset + 8, "ld");
                        self.emit_store_to_s0("t0", slot.0 + 8, "sd");
                        stack_param_offset += 16;
                        int_reg_idx = 8;
                    } else if func.is_variadic {
                        // In variadic functions, ALL named params come through GP registers.
                        // We already saved a0-a7 at s0+0..s0+56. Load from the save area.
                        if is_long_double && !is_stack_passed {
                            // F128 arrives in aligned GP register pair
                            if int_reg_idx % 2 != 0 { int_reg_idx += 1; }
                            // Load from register save area at s0 + idx*8
                            let lo_off = (int_reg_idx as i64) * 8;
                            let hi_off = ((int_reg_idx + 1) as i64) * 8;
                            self.emit_load_from_s0("a0", lo_off, "ld");
                            self.emit_load_from_s0("a1", hi_off, "ld");
                            self.state.emit("    call __trunctfdf2");
                            self.state.emit("    fmv.x.d t0, fa0");
                            self.emit_store_to_s0("t0", slot.0, "sd");
                            int_reg_idx += 2;
                        } else if is_long_double && is_stack_passed {
                            // F128 on stack
                            stack_param_offset = (stack_param_offset + 15) & !15;
                            self.emit_load_from_s0("a0", stack_param_offset, "ld");
                            self.emit_load_from_s0("a1", stack_param_offset + 8, "ld");
                            self.state.emit("    call __trunctfdf2");
                            self.state.emit("    fmv.x.d t0, fa0");
                            self.emit_store_to_s0("t0", slot.0, "sd");
                            stack_param_offset += 16;
                            int_reg_idx = 8;
                        } else if is_stack_passed {
                            self.emit_load_from_s0("t0", stack_param_offset, "ld");
                            let store_instr = Self::store_for_type(ty);
                            self.emit_store_to_s0("t0", slot.0, store_instr);
                            stack_param_offset += 8;
                        } else {
                            // Named param in GP register (including float params).
                            // For variadic, a0-a7 were saved to s0+0..s0+56. Load from there.
                            // We can also just use the register directly since we saved it first.
                            let store_instr = Self::store_for_type(ty);
                            self.emit_store_to_s0(RISCV_ARG_REGS[int_reg_idx], slot.0, store_instr);
                            int_reg_idx += 1;
                        }
                    } else if is_long_double && !is_stack_passed {
                        // F128 arrives in GP register pair (aligned to even).
                        // GP regs were saved to stack in phase 1; load from saved area.
                        if int_reg_idx % 2 != 0 { int_reg_idx += 1; }
                        // Load saved lo:hi from the save area
                        let lo_off = f128_save_offset + (int_reg_idx as i64) * 8;
                        let hi_off = f128_save_offset + ((int_reg_idx + 1) as i64) * 8;
                        self.state.emit(&format!("    ld a0, {}(sp)", lo_off));
                        self.state.emit(&format!("    ld a1, {}(sp)", hi_off));
                        self.state.emit("    call __trunctfdf2");
                        // Result is in fa0, move to GP reg and store
                        self.state.emit("    fmv.x.d t0, fa0");
                        self.emit_store_to_s0("t0", slot.0, "sd");
                        int_reg_idx += 2;
                    } else if is_long_double && is_stack_passed {
                        // F128 stack-passed: 16 bytes, 16-byte aligned.
                        // Stack params are at positive s0 offsets regardless of
                        // whether there's an f128 register save area (which is
                        // at sp, below s0).
                        stack_param_offset = (stack_param_offset + 15) & !15;
                        // Load the f128 from caller's stack and call __trunctfdf2
                        self.emit_load_from_s0("a0", stack_param_offset, "ld");
                        self.emit_load_from_s0("a1", stack_param_offset + 8, "ld");
                        self.state.emit("    call __trunctfdf2");
                        self.state.emit("    fmv.x.d t0, fa0");
                        self.emit_store_to_s0("t0", slot.0, "sd");
                        stack_param_offset += 16;
                        int_reg_idx = 8;
                    } else if is_stack_passed {
                        // Stack-passed parameter: load from positive s0 offset
                        self.emit_load_from_s0("t0", stack_param_offset, "ld");
                        let store_instr = Self::store_for_type(ty);
                        self.emit_store_to_s0("t0", slot.0, store_instr);
                        stack_param_offset += 8;
                    } else if is_float_in_gpr {
                        // FP arg that spilled to a GP register (fa0-fa7 exhausted)
                        // The value arrives as raw bits in an integer register
                        if has_f128_reg_params {
                            let off = f128_save_offset + (int_reg_idx as i64) * 8;
                            self.state.emit(&format!("    ld t0, {}(sp)", off));
                        } else {
                            self.state.emit(&format!("    mv t0, {}", RISCV_ARG_REGS[int_reg_idx]));
                        }
                        self.emit_store_to_s0("t0", slot.0, "sd");
                        int_reg_idx += 1;
                    } else if is_float {
                        // Float params arrive in fa0-fa7 per RISC-V calling convention
                        if has_f128_reg_params {
                            // FP regs were saved to stack; load from save area
                            let fp_off = f128_save_offset + 64 + (float_reg_idx as i64) * 8;
                            if ty == IrType::F32 {
                                self.state.emit(&format!("    flw ft0, {}(sp)", fp_off));
                                self.state.emit("    fmv.x.w t0, ft0");
                            } else {
                                self.state.emit(&format!("    fld ft0, {}(sp)", fp_off));
                                self.state.emit("    fmv.x.d t0, ft0");
                            }
                        } else if ty == IrType::F32 {
                            // F32 param: extract 32-bit float from fa-reg
                            self.state.emit(&format!("    fmv.x.w t0, {}", float_arg_regs[float_reg_idx]));
                        } else {
                            // F64 param: extract 64-bit double from fa-reg
                            self.state.emit(&format!("    fmv.x.d t0, {}", float_arg_regs[float_reg_idx]));
                        }
                        self.emit_store_to_s0("t0", slot.0, "sd");
                        float_reg_idx += 1;
                    } else {
                        // GP register param - load from save area if we have F128 params
                        if has_f128_reg_params {
                            let off = f128_save_offset + (int_reg_idx as i64) * 8;
                            self.state.emit(&format!("    ld t0, {}(sp)", off));
                            let store_instr = Self::store_for_type(ty);
                            self.emit_store_to_s0("t0", slot.0, store_instr);
                        } else {
                            let store_instr = Self::store_for_type(ty);
                            self.emit_store_to_s0(RISCV_ARG_REGS[int_reg_idx], slot.0, store_instr);
                        }
                        int_reg_idx += 1;
                    }
                }
            } else {
                if is_long_double {
                    if is_stack_passed {
                        stack_param_offset = (stack_param_offset + 15) & !15;
                        stack_param_offset += 16;
                        int_reg_idx = 8;
                    } else {
                        if int_reg_idx % 2 != 0 { int_reg_idx += 1; }
                        int_reg_idx += 2;
                    }
                } else if is_stack_passed {
                    stack_param_offset += 8;
                } else if func.is_variadic {
                    // Variadic: all args in GP regs
                    int_reg_idx += 1;
                } else if is_float_in_gpr {
                    // FP arg spilled to GPR - consume a GPR slot
                    int_reg_idx += 1;
                } else if is_float {
                    float_reg_idx += 1;
                } else {
                    int_reg_idx += 1;
                }
            }
        }

        // Phase 2: Clean up the F128 save area (128 bytes: 64 GP + 64 FP)
        if has_f128_reg_params && !func.is_variadic {
            self.state.emit("    addi sp, sp, 128");
        }
    }

    fn emit_load_operand(&mut self, op: &Operand) {
        self.operand_to_t0(op);
    }

    fn emit_store_result(&mut self, dest: &Value) {
        self.store_t0_to(dest);
    }

    // ---- Primitives for shared default implementations ----

    fn emit_load_acc_pair(&mut self, op: &Operand) {
        self.operand_to_t0_t1(op);
    }

    fn emit_store_acc_pair(&mut self, dest: &Value) {
        self.store_t0_t1_to(dest);
    }

    fn emit_store_pair_to_slot(&mut self, slot: StackSlot) {
        self.emit_store_to_s0("t0", slot.0, "sd");
        self.emit_store_to_s0("t1", slot.0 + 8, "sd");
    }

    fn emit_load_pair_from_slot(&mut self, slot: StackSlot) {
        self.emit_load_from_s0("t0", slot.0, "ld");
        self.emit_load_from_s0("t1", slot.0 + 8, "ld");
    }

    fn emit_save_acc_pair(&mut self) {
        self.state.emit("    mv t3, t0");
        self.state.emit("    mv t4, t1");
    }

    fn emit_store_pair_indirect(&mut self) {
        // pair saved to t3:t4, ptr in t5
        self.state.emit("    sd t3, 0(t5)");
        self.state.emit("    sd t4, 8(t5)");
    }

    fn emit_load_pair_indirect(&mut self) {
        // ptr in t5
        self.state.emit("    ld t0, 0(t5)");
        self.state.emit("    ld t1, 8(t5)");
    }

    fn emit_i128_neg(&mut self) {
        self.state.emit("    not t0, t0");
        self.state.emit("    not t1, t1");
        self.state.emit("    addi t0, t0, 1");
        self.state.emit("    seqz t2, t0");
        self.state.emit("    add t1, t1, t2");
    }

    fn emit_i128_not(&mut self) {
        self.state.emit("    not t0, t0");
        self.state.emit("    not t1, t1");
    }

    fn store_instr_for_type(&self, ty: IrType) -> &'static str {
        Self::store_for_type(ty)
    }

    fn load_instr_for_type(&self, ty: IrType) -> &'static str {
        Self::load_for_type(ty)
    }

    fn emit_typed_store_to_slot(&mut self, instr: &'static str, _ty: IrType, slot: StackSlot) {
        self.emit_store_to_s0("t0", slot.0, instr);
    }

    fn emit_typed_load_from_slot(&mut self, instr: &'static str, slot: StackSlot) {
        self.emit_load_from_s0("t0", slot.0, instr);
    }

    fn emit_save_acc(&mut self) {
        self.state.emit("    mv t3, t0");
    }

    fn emit_load_ptr_from_slot(&mut self, slot: StackSlot) {
        self.emit_load_from_s0("t5", slot.0, "ld");
    }

    fn emit_typed_store_indirect(&mut self, instr: &'static str, _ty: IrType) {
        // val saved in t3, ptr in t5
        self.state.emit(&format!("    {} t3, 0(t5)", instr));
    }

    fn emit_typed_load_indirect(&mut self, instr: &'static str) {
        // ptr in t5
        self.state.emit(&format!("    {} t0, 0(t5)", instr));
    }

    fn emit_slot_addr_to_secondary(&mut self, slot: StackSlot, is_alloca: bool) {
        if is_alloca {
            self.emit_addi_s0("t1", slot.0);
        } else {
            self.emit_load_from_s0("t1", slot.0, "ld");
        }
    }

    fn emit_add_secondary_to_acc(&mut self) {
        self.state.emit("    add t0, t1, t0");
    }

    fn emit_round_up_acc_to_16(&mut self) {
        self.state.emit("    addi t0, t0, 15");
        self.state.emit("    andi t0, t0, -16");
    }

    fn emit_sub_sp_by_acc(&mut self) {
        self.state.emit("    sub sp, sp, t0");
    }

    fn emit_mov_sp_to_acc(&mut self) {
        self.state.emit("    mv t0, sp");
    }

    fn emit_align_acc(&mut self, align: usize) {
        self.state.emit(&format!("    addi t0, t0, {}", align - 1));
        self.state.emit(&format!("    andi t0, t0, -{}", align));
    }

    fn emit_memcpy_load_dest_addr(&mut self, slot: StackSlot, is_alloca: bool) {
        if is_alloca {
            self.emit_addi_s0("t1", slot.0);
        } else {
            self.emit_load_from_s0("t1", slot.0, "ld");
        }
    }

    fn emit_memcpy_load_src_addr(&mut self, slot: StackSlot, is_alloca: bool) {
        if is_alloca {
            self.emit_addi_s0("t2", slot.0);
        } else {
            self.emit_load_from_s0("t2", slot.0, "ld");
        }
    }

    fn emit_alloca_aligned_addr(&mut self, slot: StackSlot, val_id: u32) {
        let align = self.state.alloca_over_align(val_id).unwrap();
        // Compute s0 + slot_offset into t5 (pointer register)
        self.emit_addi_s0("t5", slot.0);
        // Align: t5 = (t5 + align-1) & -align
        self.state.emit(&format!("    li t6, {}", align - 1));
        self.state.emit("    add t5, t5, t6");
        self.state.emit(&format!("    li t6, -{}", align));
        self.state.emit("    and t5, t5, t6");
    }

    fn emit_alloca_aligned_addr_to_acc(&mut self, slot: StackSlot, val_id: u32) {
        let align = self.state.alloca_over_align(val_id).unwrap();
        // Compute s0 + slot_offset into t0 (accumulator)
        self.emit_addi_s0("t0", slot.0);
        // Align: t0 = (t0 + align-1) & -align
        self.state.emit(&format!("    li t6, {}", align - 1));
        self.state.emit("    add t0, t0, t6");
        self.state.emit(&format!("    li t6, -{}", align));
        self.state.emit("    and t0, t0, t6");
    }

    fn emit_acc_to_secondary(&mut self) {
        self.state.emit("    mv t1, t0");
    }

    fn emit_memcpy_store_dest_from_acc(&mut self) {
        // Move from pointer register (t5) to memcpy dest register (t1)
        self.state.emit("    mv t1, t5");
    }

    fn emit_memcpy_store_src_from_acc(&mut self) {
        // Move from pointer register (t5) to memcpy src register (t2)
        self.state.emit("    mv t2, t5");
    }

    fn emit_memcpy_impl(&mut self, size: usize) {
        let label_id = self.state.next_label_id();
        let loop_label = format!(".Lmemcpy_loop_{}", label_id);
        let done_label = format!(".Lmemcpy_done_{}", label_id);
        self.state.emit(&format!("    li t3, {}", size));
        self.state.emit(&format!("{}:", loop_label));
        self.state.emit(&format!("    beqz t3, {}", done_label));
        self.state.emit("    lbu t4, 0(t2)");
        self.state.emit("    sb t4, 0(t1)");
        self.state.emit("    addi t1, t1, 1");
        self.state.emit("    addi t2, t2, 1");
        self.state.emit("    addi t3, t3, -1");
        self.state.emit(&format!("    j {}", loop_label));
        self.state.emit(&format!("{}:", done_label));
    }

    fn emit_float_neg(&mut self, ty: IrType) {
        if ty == IrType::F64 || ty == IrType::F128 {
            self.state.emit("    fmv.d.x ft0, t0");
            self.state.emit("    fneg.d ft0, ft0");
            self.state.emit("    fmv.x.d t0, ft0");
        } else {
            self.state.emit("    fmv.w.x ft0, t0");
            self.state.emit("    fneg.s ft0, ft0");
            self.state.emit("    fmv.x.w t0, ft0");
        }
    }

    fn emit_int_neg(&mut self, _ty: IrType) {
        self.state.emit("    neg t0, t0");
    }

    fn emit_int_not(&mut self, _ty: IrType) {
        self.state.emit("    not t0, t0");
    }

    fn emit_int_clz(&mut self, ty: IrType) {
        self.emit_clz(ty);
    }

    fn emit_int_ctz(&mut self, ty: IrType) {
        self.emit_ctz(ty);
    }

    fn emit_int_bswap(&mut self, ty: IrType) {
        self.emit_bswap(ty);
    }

    fn emit_int_popcount(&mut self, ty: IrType) {
        self.emit_popcount(ty);
    }

    fn emit_float_binop(&mut self, dest: &Value, op: FloatOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        let mnemonic = match op {
            FloatOp::Add => "fadd",
            FloatOp::Sub => "fsub",
            FloatOp::Mul => "fmul",
            FloatOp::Div => "fdiv",
        };
        self.operand_to_t0(lhs);
        self.state.emit("    mv t1, t0");
        self.operand_to_t0(rhs);
        self.state.emit("    mv t2, t0");
        // F128 uses F64 instructions (long double computed at double precision)
        if ty == IrType::F64 || ty == IrType::F128 {
            self.state.emit("    fmv.d.x ft0, t1");
            self.state.emit("    fmv.d.x ft1, t2");
            self.state.emit(&format!("    {}.d ft0, ft0, ft1", mnemonic));
            self.state.emit("    fmv.x.d t0, ft0");
        } else {
            self.state.emit("    fmv.w.x ft0, t1");
            self.state.emit("    fmv.w.x ft1, t2");
            self.state.emit(&format!("    {}.s ft0, ft0, ft1", mnemonic));
            self.state.emit("    fmv.x.w t0, ft0");
        }
        self.store_t0_to(dest);
    }

    fn emit_int_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if is_i128_type(ty) {
            self.emit_i128_binop(dest, op, lhs, rhs);
            return;
        }

        self.operand_to_t0(lhs);
        self.state.emit("    mv t1, t0");
        self.operand_to_t0(rhs);
        self.state.emit("    mv t2, t0");

        let use_32bit = ty == IrType::I32 || ty == IrType::U32;
        let w = if use_32bit { "w" } else { "" };

        let mnemonic = match op {
            IrBinOp::Add => format!("add{}", w),
            IrBinOp::Sub => format!("sub{}", w),
            IrBinOp::Mul => format!("mul{}", w),
            IrBinOp::SDiv => format!("div{}", w),
            IrBinOp::UDiv => format!("divu{}", w),
            IrBinOp::SRem => format!("rem{}", w),
            IrBinOp::URem => format!("remu{}", w),
            IrBinOp::And => "and".to_string(),
            IrBinOp::Or => "or".to_string(),
            IrBinOp::Xor => "xor".to_string(),
            IrBinOp::Shl => format!("sll{}", w),
            IrBinOp::AShr => format!("sra{}", w),
            IrBinOp::LShr => format!("srl{}", w),
        };
        self.state.emit(&format!("    {} t0, t1, t2", mnemonic));

        self.store_t0_to(dest);
    }

    fn emit_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if is_i128_type(ty) {
            self.emit_i128_cmp(dest, op, lhs, rhs);
            return;
        }

        self.operand_to_t0(lhs);
        self.state.emit("    mv t1, t0");
        self.operand_to_t0(rhs);
        self.state.emit("    mv t2, t0");

        if ty.is_float() {
            // F128 uses F64 instructions (long double computed at double precision)
            let s = if ty == IrType::F64 || ty == IrType::F128 { "d" } else { "s" };
            let fmv = if s == "d" { "fmv.d.x" } else { "fmv.w.x" };
            self.state.emit(&format!("    {} ft0, t1", fmv));
            self.state.emit(&format!("    {} ft1, t2", fmv));
            match op {
                IrCmpOp::Eq => self.state.emit(&format!("    feq.{} t0, ft0, ft1", s)),
                IrCmpOp::Ne => {
                    self.state.emit(&format!("    feq.{} t0, ft0, ft1", s));
                    self.state.emit("    xori t0, t0, 1");
                }
                IrCmpOp::Slt | IrCmpOp::Ult => self.state.emit(&format!("    flt.{} t0, ft0, ft1", s)),
                IrCmpOp::Sle | IrCmpOp::Ule => self.state.emit(&format!("    fle.{} t0, ft0, ft1", s)),
                IrCmpOp::Sgt | IrCmpOp::Ugt => self.state.emit(&format!("    flt.{} t0, ft1, ft0", s)),
                IrCmpOp::Sge | IrCmpOp::Uge => self.state.emit(&format!("    fle.{} t0, ft1, ft0", s)),
            }
            self.store_t0_to(dest);
            return;
        }

        // For sub-64-bit types, sign/zero-extend before comparison
        let is_32bit = ty == IrType::I32 || ty == IrType::U32
            || ty == IrType::I8 || ty == IrType::U8
            || ty == IrType::I16 || ty == IrType::U16;
        if is_32bit && ty.is_unsigned() {
            self.state.emit("    slli t1, t1, 32");
            self.state.emit("    srli t1, t1, 32");
            self.state.emit("    slli t2, t2, 32");
            self.state.emit("    srli t2, t2, 32");
        } else if is_32bit {
            self.state.emit("    sext.w t1, t1");
            self.state.emit("    sext.w t2, t2");
        }

        match op {
            IrCmpOp::Eq => {
                self.state.emit("    sub t0, t1, t2");
                self.state.emit("    seqz t0, t0");
            }
            IrCmpOp::Ne => {
                self.state.emit("    sub t0, t1, t2");
                self.state.emit("    snez t0, t0");
            }
            IrCmpOp::Slt => self.state.emit("    slt t0, t1, t2"),
            IrCmpOp::Ult => self.state.emit("    sltu t0, t1, t2"),
            IrCmpOp::Sge => {
                self.state.emit("    slt t0, t1, t2");
                self.state.emit("    xori t0, t0, 1");
            }
            IrCmpOp::Uge => {
                self.state.emit("    sltu t0, t1, t2");
                self.state.emit("    xori t0, t0, 1");
            }
            IrCmpOp::Sgt => self.state.emit("    slt t0, t2, t1"),
            IrCmpOp::Ugt => self.state.emit("    sltu t0, t2, t1"),
            IrCmpOp::Sle => {
                self.state.emit("    slt t0, t2, t1");
                self.state.emit("    xori t0, t0, 1");
            }
            IrCmpOp::Ule => {
                self.state.emit("    sltu t0, t2, t1");
                self.state.emit("    xori t0, t0, 1");
            }
        }

        self.store_t0_to(dest);
    }

    fn emit_call(&mut self, args: &[Operand], arg_types: &[IrType], direct_name: Option<&str>,
                 func_ptr: Option<&Operand>, dest: Option<Value>, return_type: IrType,
                 is_variadic: bool, _num_fixed_args: usize) {
        let float_arg_regs = ["fa0", "fa1", "fa2", "fa3", "fa4", "fa5", "fa6", "fa7"];

        // Phase 1: Classify all args into register assignments or stack overflow.
        // 'I' = GP register, 'F' = FP register, 'Q' = F128 GP register pair,
        // 'W' = i128 GP register pair, 'S' = stack, 'T' = F128 stack, 'X' = i128 stack
        let mut arg_classes: Vec<char> = Vec::new();
        let mut arg_int_indices: Vec<usize> = Vec::new(); // GP reg index for 'I' or base index for 'Q'/'W'
        let mut arg_fp_indices: Vec<usize> = Vec::new();  // FP reg index for 'F'
        let mut int_idx = 0usize;
        let mut float_idx = 0usize;

        for (i, _arg) in args.iter().enumerate() {
            let is_long_double = if i < arg_types.len() {
                arg_types[i].is_long_double()
            } else {
                false
            };
            let is_i128_arg = if i < arg_types.len() {
                is_i128_type(arg_types[i])
            } else {
                false
            };
            let is_float_arg = if i < arg_types.len() {
                arg_types[i].is_float()
            } else {
                matches!(args[i], Operand::Const(IrConst::F32(_) | IrConst::F64(_)))
            };

            if is_i128_arg {
                // 128-bit integer: needs aligned register pair
                if int_idx % 2 != 0 { int_idx += 1; }
                if int_idx + 1 < 8 {
                    arg_classes.push('W'); // i128 register pair
                    arg_int_indices.push(int_idx);
                    arg_fp_indices.push(0);
                    int_idx += 2;
                } else {
                    arg_classes.push('X'); // i128 stack overflow
                    arg_int_indices.push(0);
                    arg_fp_indices.push(0);
                    int_idx = 8;
                }
            } else if is_long_double {
                // Align int_idx to even for register pair
                if int_idx % 2 != 0 {
                    int_idx += 1;
                }
                if int_idx + 1 < 8 {
                    arg_classes.push('Q');
                    arg_int_indices.push(int_idx);
                    arg_fp_indices.push(0);
                    int_idx += 2;
                } else {
                    arg_classes.push('T'); // F128 stack overflow
                    arg_int_indices.push(0);
                    arg_fp_indices.push(0);
                    int_idx = 8;
                }
            } else if is_float_arg && !is_variadic && float_idx < 8 {
                arg_classes.push('F');
                arg_int_indices.push(0);
                arg_fp_indices.push(float_idx);
                float_idx += 1;
            } else if int_idx < 8 {
                arg_classes.push('I');
                arg_int_indices.push(int_idx);
                arg_fp_indices.push(0);
                int_idx += 1;
            } else {
                arg_classes.push('S');
                arg_int_indices.push(0);
                arg_fp_indices.push(0);
            }
        }

        // Phase 2: Handle F128 variable args that need __extenddftf2.
        // Call __extenddftf2 for each, save results to stack temporaries.
        // Count how many F128 variable reg args we have.
        let mut f128_var_temps: Vec<(usize, i64)> = Vec::new(); // (arg_index, sp_offset of saved lo:hi)
        let mut f128_temp_space: i64 = 0;
        for (i, arg) in args.iter().enumerate() {
            if arg_classes[i] == 'Q' {
                if let Operand::Value(_) = arg {
                    f128_temp_space += 16;
                    f128_var_temps.push((i, 0)); // offset filled below
                }
            }
        }

        if f128_temp_space > 0 {
            // Allocate temp space for F128 conversion results
            self.emit_addi_sp(-f128_temp_space);
            let mut temp_offset: i64 = 0;
            for item in &mut f128_var_temps {
                item.1 = temp_offset;
                let arg = &args[item.0];
                // Load f64 value, call __extenddftf2, save result
                self.operand_to_t0(arg);
                self.state.emit("    fmv.d.x fa0, t0");
                self.state.emit("    call __extenddftf2");
                // Save a0:a1 to temp space
                self.emit_store_to_sp("a0", temp_offset, "sd");
                self.emit_store_to_sp("a1", temp_offset + 8, "sd");
                temp_offset += 16;
            }
        }

        // Phase 3: Handle stack overflow args.
        // 'S' = normal stack, 'T' = F128 stack, 'X' = i128 stack
        let stack_args: Vec<(usize, char)> = args.iter().enumerate()
            .filter(|(i, _)| arg_classes[*i] == 'S' || arg_classes[*i] == 'T' || arg_classes[*i] == 'X')
            .map(|(i, _)| (i, arg_classes[i]))
            .collect();

        let mut stack_arg_space: usize = 0;
        if !stack_args.is_empty() {
            for &(_, cls) in &stack_args {
                if cls == 'T' || cls == 'X' {
                    stack_arg_space = (stack_arg_space + 15) & !15;
                    stack_arg_space += 16;
                } else {
                    stack_arg_space += 8;
                }
            }
            stack_arg_space = (stack_arg_space + 15) & !15;
            self.emit_addi_sp(-(stack_arg_space as i64));
            let mut offset: usize = 0;
            for &(arg_i, cls) in &stack_args {
                if cls == 'X' {
                    // 128-bit integer stack arg
                    offset = (offset + 15) & !15;
                    match &args[arg_i] {
                        Operand::Const(c) => {
                            if let IrConst::I128(v) = c {
                                let low = *v as u64 as i64;
                                let high = (*v >> 64) as u64 as i64;
                                self.state.emit(&format!("    li t0, {}", low));
                                self.emit_store_to_sp("t0", offset as i64, "sd");
                                self.state.emit(&format!("    li t0, {}", high));
                                self.emit_store_to_sp("t0", (offset + 8) as i64, "sd");
                            } else {
                                self.operand_to_t0(&args[arg_i]);
                                self.emit_store_to_sp("t0", offset as i64, "sd");
                                self.emit_store_to_sp("zero", (offset + 8) as i64, "sd");
                            }
                        }
                        Operand::Value(v) => {
                            if let Some(slot) = self.state.get_slot(v.0) {
                                if self.state.is_alloca(v.0) {
                                    self.emit_addi_s0("t0", slot.0);
                                    self.emit_store_to_sp("t0", offset as i64, "sd");
                                    self.emit_store_to_sp("zero", (offset + 8) as i64, "sd");
                                } else {
                                    self.emit_load_from_s0("t0", slot.0, "ld");
                                    self.emit_store_to_sp("t0", offset as i64, "sd");
                                    self.emit_load_from_s0("t0", slot.0 + 8, "ld");
                                    self.emit_store_to_sp("t0", (offset + 8) as i64, "sd");
                                }
                            }
                        }
                    }
                    offset += 16;
                } else if cls == 'T' {
                    offset = (offset + 15) & !15;
                    match &args[arg_i] {
                        Operand::Const(ref c) => {
                            let f64_val = match c {
                                IrConst::LongDouble(v) => *v,
                                IrConst::F64(v) => *v,
                                _ => c.to_f64().unwrap_or(0.0),
                            };
                            let bytes = crate::ir::ir::f64_to_f128_bytes(f64_val);
                            let lo = i64::from_le_bytes(bytes[0..8].try_into().unwrap());
                            let hi = i64::from_le_bytes(bytes[8..16].try_into().unwrap());
                            self.state.emit(&format!("    li t0, {}", lo));
                            self.emit_store_to_sp("t0", offset as i64, "sd");
                            self.state.emit(&format!("    li t0, {}", hi));
                            self.emit_store_to_sp("t0", (offset + 8) as i64, "sd");
                        }
                        Operand::Value(ref _v) => {
                            self.operand_to_t0(&args[arg_i]);
                            self.state.emit("    fmv.d.x fa0, t0");
                            self.state.emit("    call __extenddftf2");
                            self.emit_store_to_sp("a0", offset as i64, "sd");
                            self.emit_store_to_sp("a1", (offset + 8) as i64, "sd");
                        }
                    }
                    offset += 16;
                } else {
                    self.operand_to_t0(&args[arg_i]);
                    self.emit_store_to_sp("t0", offset as i64, "sd");
                    offset += 8;
                }
            }
        }

        // Phase 4: Load non-F128 register args.
        // Load all non-F128 args into their target registers.
        // Process GP args into temps first, then FP args, then move GP from temp to aX.
        // Note: t6 is reserved as the large-offset stack scratch register
        // (used by emit_load_from_s0/emit_store_to_s0), so it must NOT be in this pool.
        let mut gp_temps: Vec<(usize, &str)> = Vec::new(); // (target_reg_idx, temp_reg)
        let temp_regs = ["t3", "t4", "t5", "s2", "s3", "s4", "s5", "s6"];
        let mut temp_i = 0usize;
        for (i, arg) in args.iter().enumerate() {
            if arg_classes[i] == 'I' {
                self.operand_to_t0(arg);
                if temp_i < temp_regs.len() {
                    self.state.emit(&format!("    mv {}, t0", temp_regs[temp_i]));
                    gp_temps.push((arg_int_indices[i], temp_regs[temp_i]));
                    temp_i += 1;
                }
            } else if arg_classes[i] == 'F' {
                let fp_i = arg_fp_indices[i];
                self.operand_to_t0(arg);
                let arg_ty = if i < arg_types.len() { Some(arg_types[i]) } else { None };
                if arg_ty == Some(IrType::F32) {
                    self.state.emit(&format!("    fmv.w.x {}, t0", float_arg_regs[fp_i]));
                } else {
                    self.state.emit(&format!("    fmv.d.x {}, t0", float_arg_regs[fp_i]));
                }
            }
        }

        // Phase 5: Move GP args from temps to actual arg registers.
        for (target_idx, temp_reg) in &gp_temps {
            self.state.emit(&format!("    mv {}, {}", RISCV_ARG_REGS[*target_idx], temp_reg));
        }

        // Phase 6: Load F128 register args into their target register pairs.
        for (i, _arg) in args.iter().enumerate() {
            if arg_classes[i] != 'Q' { continue; }
            let base_reg = arg_int_indices[i];
            match &args[i] {
                Operand::Const(ref c) => {
                    let f64_val = match c {
                        IrConst::LongDouble(v) => *v,
                        IrConst::F64(v) => *v,
                        _ => c.to_f64().unwrap_or(0.0),
                    };
                    let bytes = crate::ir::ir::f64_to_f128_bytes(f64_val);
                    let lo = i64::from_le_bytes(bytes[0..8].try_into().unwrap());
                    let hi = i64::from_le_bytes(bytes[8..16].try_into().unwrap());
                    self.state.emit(&format!("    li {}, {}", RISCV_ARG_REGS[base_reg], lo));
                    self.state.emit(&format!("    li {}, {}", RISCV_ARG_REGS[base_reg + 1], hi));
                }
                Operand::Value(_) => {
                    // Load from temp space (saved in Phase 2)
                    let temp_info = f128_var_temps.iter().find(|t| t.0 == i).unwrap();
                    // Adjust offset for stack_arg_space
                    let offset = temp_info.1 + stack_arg_space as i64;
                    self.emit_load_from_sp(RISCV_ARG_REGS[base_reg], offset, "ld");
                    self.emit_load_from_sp(RISCV_ARG_REGS[base_reg + 1], offset + 8, "ld");
                }
            }
        }

        // Phase 7: Load i128 register pair args into their target register pairs.
        for (i, _arg) in args.iter().enumerate() {
            if arg_classes[i] != 'W' { continue; }
            let base_reg = arg_int_indices[i];
            match &args[i] {
                Operand::Const(c) => {
                    if let IrConst::I128(v) = c {
                        let low = *v as u64 as i64;
                        let high = (*v >> 64) as u64 as i64;
                        self.state.emit(&format!("    li {}, {}", RISCV_ARG_REGS[base_reg], low));
                        self.state.emit(&format!("    li {}, {}", RISCV_ARG_REGS[base_reg + 1], high));
                    } else {
                        self.operand_to_t0(&args[i]);
                        self.state.emit(&format!("    mv {}, t0", RISCV_ARG_REGS[base_reg]));
                        self.state.emit(&format!("    mv {}, zero", RISCV_ARG_REGS[base_reg + 1]));
                    }
                }
                Operand::Value(v) => {
                    if let Some(slot) = self.state.get_slot(v.0) {
                        if self.state.is_alloca(v.0) {
                            self.emit_addi_s0(RISCV_ARG_REGS[base_reg], slot.0);
                            self.state.emit(&format!("    mv {}, zero", RISCV_ARG_REGS[base_reg + 1]));
                        } else {
                            self.emit_load_from_s0(RISCV_ARG_REGS[base_reg], slot.0, "ld");
                            self.emit_load_from_s0(RISCV_ARG_REGS[base_reg + 1], slot.0 + 8, "ld");
                        }
                    }
                }
            }
        }

        // Clean up F128 temp space before the call (only if no stack overflow args below it)
        if f128_temp_space > 0 && stack_arg_space == 0 {
            self.emit_addi_sp(f128_temp_space);
        }

        if let Some(name) = direct_name {
            self.state.emit(&format!("    call {}", name));
        } else if let Some(ptr) = func_ptr {
            self.operand_to_t0(ptr);
            self.state.emit("    mv t2, t0");
            self.state.emit("    jalr ra, t2, 0");
        }

        // Clean up stack space after call
        if stack_arg_space > 0 {
            // Both stack overflow args and f128 temp space (if any) need cleanup
            let cleanup = (stack_arg_space as i64) + f128_temp_space;
            self.emit_addi_sp(cleanup);
        }

        if let Some(dest) = dest {
            if let Some(slot) = self.state.get_slot(dest.0) {
                if is_i128_type(return_type) {
                    // 128-bit return: a0 = low, a1 = high per RISC-V LP64D ABI
                    self.emit_store_to_s0("a0", slot.0, "sd");
                    self.emit_store_to_s0("a1", slot.0 + 8, "sd");
                } else if return_type.is_long_double() {
                    // F128 return value is in a0:a1 (GP register pair).
                    // Convert from f128 back to f64 using __trunctfdf2.
                    self.state.emit("    call __trunctfdf2");
                    self.state.emit("    fmv.x.d t0, fa0");
                    self.emit_store_to_s0("t0", slot.0, "sd");
                } else if return_type == IrType::F32 {
                    // F32 return value is in fa0 as single-precision
                    self.state.emit("    fmv.x.w t0, fa0");
                    self.emit_store_to_s0("t0", slot.0, "sd");
                } else if return_type.is_float() {
                    // F64 return value is in fa0 as double-precision
                    self.state.emit("    fmv.x.d t0, fa0");
                    self.emit_store_to_s0("t0", slot.0, "sd");
                } else {
                    self.emit_store_to_s0("a0", slot.0, "sd");
                }
            }
        }
    }

    fn emit_global_addr(&mut self, dest: &Value, name: &str) {
        self.state.emit(&format!("    la t0, {}", name));
        self.store_t0_to(dest);
    }

    fn emit_cast_instrs(&mut self, from_ty: IrType, to_ty: IrType) {
        match classify_cast(from_ty, to_ty) {
            CastKind::Noop => {}

            CastKind::FloatToSigned { from_f64 } => {
                if from_f64 {
                    self.state.emit("    fmv.d.x ft0, t0");
                    self.state.emit("    fcvt.l.d t0, ft0, rtz");
                } else {
                    self.state.emit("    fmv.w.x ft0, t0");
                    self.state.emit("    fcvt.l.s t0, ft0, rtz");
                }
            }

            CastKind::FloatToUnsigned { from_f64, .. } => {
                if from_f64 {
                    self.state.emit("    fmv.d.x ft0, t0");
                    self.state.emit("    fcvt.lu.d t0, ft0, rtz");
                } else {
                    self.state.emit("    fmv.w.x ft0, t0");
                    self.state.emit("    fcvt.lu.s t0, ft0, rtz");
                }
            }

            CastKind::SignedToFloat { to_f64 } => {
                if to_f64 {
                    self.state.emit("    fcvt.d.l ft0, t0");
                    self.state.emit("    fmv.x.d t0, ft0");
                } else {
                    self.state.emit("    fcvt.s.l ft0, t0");
                    self.state.emit("    fmv.x.w t0, ft0");
                }
            }

            CastKind::UnsignedToFloat { to_f64, .. } => {
                if to_f64 {
                    self.state.emit("    fcvt.d.lu ft0, t0");
                    self.state.emit("    fmv.x.d t0, ft0");
                } else {
                    self.state.emit("    fcvt.s.lu ft0, t0");
                    self.state.emit("    fmv.x.w t0, ft0");
                }
            }

            CastKind::FloatToFloat { widen } => {
                if widen {
                    self.state.emit("    fmv.w.x ft0, t0");
                    self.state.emit("    fcvt.d.s ft0, ft0");
                    self.state.emit("    fmv.x.d t0, ft0");
                } else {
                    self.state.emit("    fmv.d.x ft0, t0");
                    self.state.emit("    fcvt.s.d ft0, ft0");
                    self.state.emit("    fmv.x.w t0, ft0");
                }
            }

            CastKind::SignedToUnsignedSameSize { to_ty } => {
                match to_ty {
                    IrType::U8 => self.state.emit("    andi t0, t0, 0xff"),
                    IrType::U16 => {
                        self.state.emit("    slli t0, t0, 48");
                        self.state.emit("    srli t0, t0, 48");
                    }
                    IrType::U32 => {
                        self.state.emit("    slli t0, t0, 32");
                        self.state.emit("    srli t0, t0, 32");
                    }
                    _ => {}
                }
            }

            CastKind::IntWiden { from_ty, .. } => {
                if from_ty.is_unsigned() {
                    match from_ty {
                        IrType::U8 => self.state.emit("    andi t0, t0, 0xff"),
                        IrType::U16 => {
                            self.state.emit("    slli t0, t0, 48");
                            self.state.emit("    srli t0, t0, 48");
                        }
                        IrType::U32 => {
                            self.state.emit("    slli t0, t0, 32");
                            self.state.emit("    srli t0, t0, 32");
                        }
                        _ => {}
                    }
                } else {
                    match from_ty {
                        IrType::I8 => {
                            self.state.emit("    slli t0, t0, 56");
                            self.state.emit("    srai t0, t0, 56");
                        }
                        IrType::I16 => {
                            self.state.emit("    slli t0, t0, 48");
                            self.state.emit("    srai t0, t0, 48");
                        }
                        IrType::I32 => self.state.emit("    sext.w t0, t0"),
                        _ => {}
                    }
                }
            }

            CastKind::IntNarrow { to_ty } => {
                match to_ty {
                    IrType::I8 => {
                        self.state.emit("    slli t0, t0, 56");
                        self.state.emit("    srai t0, t0, 56");
                    }
                    IrType::U8 => self.state.emit("    andi t0, t0, 0xff"),
                    IrType::I16 => {
                        self.state.emit("    slli t0, t0, 48");
                        self.state.emit("    srai t0, t0, 48");
                    }
                    IrType::U16 => {
                        self.state.emit("    slli t0, t0, 48");
                        self.state.emit("    srli t0, t0, 48");
                    }
                    IrType::I32 => self.state.emit("    sext.w t0, t0"),
                    IrType::U32 => {
                        self.state.emit("    slli t0, t0, 32");
                        self.state.emit("    srli t0, t0, 32");
                    }
                    _ => {}
                }
            }
        }
    }

    /// Override emit_cast to handle 128-bit widening/narrowing.
    fn emit_cast(&mut self, dest: &Value, src: &Operand, from_ty: IrType, to_ty: IrType) {
        if is_i128_type(to_ty) && !is_i128_type(from_ty) {
            // Widening to 128-bit
            self.operand_to_t0(src);
            // First widen to 64-bit if needed
            if from_ty.size() < 8 {
                self.emit_cast_instrs(from_ty, if from_ty.is_signed() { IrType::I64 } else { IrType::U64 });
            }
            if from_ty.is_signed() {
                // Sign-extend: t1 = t0 >> 63 (arithmetic)
                self.state.emit("    srai t1, t0, 63");
            } else {
                self.state.emit("    li t1, 0");
            }
            self.store_t0_t1_to(dest);
            return;
        }
        if is_i128_type(from_ty) && !is_i128_type(to_ty) {
            // Narrowing from 128-bit: use low 64 bits
            self.operand_to_t0_t1(src);
            // t0 already has the low 64 bits
            if to_ty.size() < 8 {
                self.emit_cast_instrs(IrType::I64, to_ty);
            }
            self.store_t0_to(dest);
            return;
        }
        if is_i128_type(from_ty) && is_i128_type(to_ty) {
            // I128 <-> U128: same representation, just copy
            self.operand_to_t0_t1(src);
            self.store_t0_t1_to(dest);
            return;
        }
        // Default path
        self.emit_load_operand(src);
        self.emit_cast_instrs(from_ty, to_ty);
        self.emit_store_result(dest);
    }

    fn emit_va_arg(&mut self, dest: &Value, va_list_ptr: &Value, result_ty: IrType) {
        // RISC-V LP64D: va_list is just a void* (pointer to the next arg on stack).
        // Load va_list pointer address into t1
        if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
            if self.state.is_alloca(va_list_ptr.0) {
                self.emit_addi_s0("t1", slot.0);
            } else {
                self.emit_load_from_s0("t1", slot.0, "ld");
            }
        }
        // Load the current va_list pointer value (points to next arg)
        self.state.emit("    ld t2, 0(t1)");

        if result_ty.is_long_double() {
            // F128 (long double): 16 bytes, 16-byte aligned.
            // Align t2 to 16 bytes: t2 = (t2 + 15) & ~15
            self.state.emit("    addi t2, t2, 15");
            self.state.emit("    andi t2, t2, -16");
            // Load 16 bytes (f128) into a0:a1 for __trunctfdf2
            self.state.emit("    ld a0, 0(t2)");    // lo 8 bytes
            self.state.emit("    ld a1, 8(t2)");    // hi 8 bytes
            // Advance pointer by 16
            self.state.emit("    addi t2, t2, 16");
            self.state.emit("    sd t2, 0(t1)");
            // Convert f128 (in a0:a1) to f64 using __trunctfdf2
            // __trunctfdf2 on RISC-V: takes f128 in a0:a1, returns f64 in fa0
            self.state.emit("    call __trunctfdf2");
            // Move f64 result from fa0 to t0 (bit pattern)
            self.state.emit("    fmv.x.d t0, fa0");
        } else {
            // Standard 8-byte arg
            self.state.emit("    ld t0, 0(t2)");
            // Advance pointer by 8
            self.state.emit("    addi t2, t2, 8");
            self.state.emit("    sd t2, 0(t1)");
        }
        // Store result
        self.store_t0_to(dest);
    }

    fn emit_va_start(&mut self, va_list_ptr: &Value) {
        // RISC-V LP64D: va_list = pointer to first variadic arg.
        // The register save area (a0-a7) is at s0+0..s0+56, and the caller's
        // stack-passed args are at s0+64, s0+72, etc. They form a contiguous
        // array of 8-byte slots. va_start sets va_list to point to the first
        // variadic argument.
        if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
            if self.state.is_alloca(va_list_ptr.0) {
                self.emit_addi_s0("t0", slot.0);
            } else {
                self.emit_load_from_s0("t0", slot.0, "ld");
            }
        }

        // Variadic args start at a[named_gp_count] in the register save area.
        // a0 is at s0+0, a1 at s0+8, ..., a7 at s0+56.
        // If all 8 GP regs are used by named params, variadic args start on the
        // caller's stack at s0+64.
        let vararg_offset = (self.va_named_gp_count as i64) * 8;
        self.emit_addi_s0("t1", vararg_offset);
        self.state.emit("    sd t1, 0(t0)");
    }

    // emit_va_end: uses default no-op implementation

    fn emit_va_copy(&mut self, dest_ptr: &Value, src_ptr: &Value) {
        // Copy va_list (just 8 bytes on RISC-V - a single pointer)
        if let Some(src_slot) = self.state.get_slot(src_ptr.0) {
            if self.state.is_alloca(src_ptr.0) {
                self.emit_addi_s0("t1", src_slot.0);
            } else {
                self.emit_load_from_s0("t1", src_slot.0, "ld");
            }
        }
        self.state.emit("    ld t2, 0(t1)");
        if let Some(dest_slot) = self.state.get_slot(dest_ptr.0) {
            if self.state.is_alloca(dest_ptr.0) {
                self.emit_addi_s0("t0", dest_slot.0);
            } else {
                self.emit_load_from_s0("t0", dest_slot.0, "ld");
            }
        }
        self.state.emit("    sd t2, 0(t0)");
    }

    fn emit_return(&mut self, val: Option<&Operand>, frame_size: i64) {
        if let Some(val) = val {
            if is_i128_type(self.current_return_type) {
                // 128-bit return: a0 = low, a1 = high per RISC-V LP64D ABI
                self.operand_to_t0_t1(val);
                self.state.emit("    mv a0, t0");
                self.state.emit("    mv a1, t1");
                self.emit_epilogue_riscv(frame_size);
                self.state.emit("    ret");
                return;
            }
            self.operand_to_t0(val);
            if self.current_return_type.is_long_double() {
                // F128 return: convert f64 bit pattern to f128 via __extenddftf2.
                // Result goes in a0:a1 (GP register pair) per RISC-V LP64D ABI.
                self.state.emit("    fmv.d.x fa0, t0");
                self.state.emit("    call __extenddftf2");
                // __extenddftf2 returns f128 in a0:a1 which is the correct return convention
            } else if self.current_return_type == IrType::F32 {
                // F32 return: bit pattern in t0, move to fa0 as single-precision
                self.state.emit("    fmv.w.x fa0, t0");
            } else if self.current_return_type.is_float() {
                // F64 return: bit pattern in t0, move to fa0 as double-precision
                self.state.emit("    fmv.d.x fa0, t0");
            } else {
                self.state.emit("    mv a0, t0");
            }
        }
        self.emit_epilogue_riscv(frame_size);
        self.state.emit("    ret");
    }

    // emit_branch, emit_cond_branch, emit_unreachable, emit_indirect_branch:
    // use default implementations from ArchCodegen trait

    // emit_label_addr: uses default implementation (delegates to emit_global_addr)

    fn emit_get_return_f64_second(&mut self, dest: &Value) {
        // After a function call, the second F64 return value is in fa1.
        // Store it to the dest stack slot, handling large offsets.
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.emit_store_to_s0("fa1", slot.0, "fsd");
        }
    }

    fn emit_set_return_f64_second(&mut self, src: &Operand) {
        // Load src into fa1 for the second return value.
        match src {
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    self.emit_load_from_s0("fa1", slot.0, "fld");
                }
            }
            Operand::Const(IrConst::F64(f)) => {
                let bits = f.to_bits() as i64;
                self.state.emit(&format!("    li t0, {}", bits));
                self.state.emit("    fmv.d.x fa1, t0");
            }
            _ => {
                self.operand_to_t0(src);
                self.state.emit("    fmv.d.x fa1, t0");
            }
        }
    }

    fn emit_get_return_f32_second(&mut self, dest: &Value) {
        // After a function call, the second F32 return value is in fa1 (LP64D).
        // Store it to the dest stack slot.
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.emit_store_to_s0("fa1", slot.0, "fsw");
        }
    }

    fn emit_set_return_f32_second(&mut self, src: &Operand) {
        // Load src into fa1 for the second F32 return value (LP64D).
        match src {
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    self.emit_load_from_s0("fa1", slot.0, "flw");
                }
            }
            Operand::Const(IrConst::F32(f)) => {
                let bits = f.to_bits() as i64;
                self.state.emit(&format!("    li t0, {}", bits));
                self.state.emit("    fmv.w.x fa1, t0");
            }
            _ => {
                self.operand_to_t0(src);
                self.state.emit("    fmv.w.x fa1, t0");
            }
        }
    }

    fn emit_atomic_rmw(&mut self, dest: &Value, op: AtomicRmwOp, ptr: &Operand, val: &Operand, ty: IrType, ordering: AtomicOrdering) {
        // Load ptr into t1, val into t2
        self.operand_to_t0(ptr);
        self.state.emit("    mv t1, t0"); // t1 = ptr
        self.operand_to_t0(val);
        self.state.emit("    mv t2, t0"); // t2 = val

        let aq_rl = Self::amo_ordering(ordering);

        if Self::is_subword_type(ty) {
            // RISC-V has no byte/halfword atomic instructions.
            // Use word-aligned LR.W/SC.W with bit masking.
            self.emit_subword_atomic_rmw(op, ty, aq_rl);
        } else {
            let suffix = Self::amo_width_suffix(ty);
            match op {
                AtomicRmwOp::Add => {
                    self.state.emit(&format!("    amoadd.{}{} t0, t2, (t1)", suffix, aq_rl));
                }
                AtomicRmwOp::Sub => {
                    // No amosub; negate and use amoadd
                    self.state.emit("    neg t2, t2");
                    self.state.emit(&format!("    amoadd.{}{} t0, t2, (t1)", suffix, aq_rl));
                }
                AtomicRmwOp::And => {
                    self.state.emit(&format!("    amoand.{}{} t0, t2, (t1)", suffix, aq_rl));
                }
                AtomicRmwOp::Or => {
                    self.state.emit(&format!("    amoor.{}{} t0, t2, (t1)", suffix, aq_rl));
                }
                AtomicRmwOp::Xor => {
                    self.state.emit(&format!("    amoxor.{}{} t0, t2, (t1)", suffix, aq_rl));
                }
                AtomicRmwOp::Xchg => {
                    self.state.emit(&format!("    amoswap.{}{} t0, t2, (t1)", suffix, aq_rl));
                }
                AtomicRmwOp::Nand => {
                    // No amonand; use lr/sc loop
                    let loop_label = self.state.fresh_label("atomic_nand");
                    self.state.emit(&format!("{}:", loop_label));
                    self.state.emit(&format!("    lr.{}{} t0, (t1)", suffix, aq_rl));
                    self.state.emit("    and t3, t0, t2");
                    self.state.emit("    not t3, t3");
                    self.state.emit(&format!("    sc.{}{} t4, t3, (t1)", suffix, aq_rl));
                    self.state.emit(&format!("    bnez t4, {}", loop_label));
                }
                AtomicRmwOp::TestAndSet => {
                    // test_and_set: set byte to 1, return old
                    self.state.emit("    li t2, 1");
                    self.state.emit(&format!("    amoswap.{}{} t0, t2, (t1)", suffix, aq_rl));
                }
            }
        }
        // Sign-extend result for sub-word types
        Self::sign_extend_riscv(&mut self.state, ty);
        self.store_t0_to(dest);
    }

    fn emit_atomic_cmpxchg(&mut self, dest: &Value, ptr: &Operand, expected: &Operand, desired: &Operand, ty: IrType, ordering: AtomicOrdering, _failure_ordering: AtomicOrdering, returns_bool: bool) {
        // t1 = ptr, t2 = expected, t3 = desired
        self.operand_to_t0(ptr);
        self.state.emit("    mv t1, t0");
        self.operand_to_t0(desired);
        self.state.emit("    mv t3, t0");
        self.operand_to_t0(expected);
        self.state.emit("    mv t2, t0");

        let aq_rl = Self::amo_ordering(ordering);

        if Self::is_subword_type(ty) {
            self.emit_subword_atomic_cmpxchg(ty, aq_rl, returns_bool);
        } else {
            let suffix = Self::amo_width_suffix(ty);

            let loop_label = self.state.fresh_label("cas_loop");
            let fail_label = self.state.fresh_label("cas_fail");
            let done_label = self.state.fresh_label("cas_done");

            self.state.emit(&format!("{}:", loop_label));
            self.state.emit(&format!("    lr.{}{} t0, (t1)", suffix, aq_rl));
            self.state.emit(&format!("    bne t0, t2, {}", fail_label));
            self.state.emit(&format!("    sc.{}{} t4, t3, (t1)", suffix, aq_rl));
            self.state.emit(&format!("    bnez t4, {}", loop_label));
            if returns_bool {
                self.state.emit("    li t0, 1");
            }
            self.state.emit(&format!("    j {}", done_label));
            self.state.emit(&format!("{}:", fail_label));
            if returns_bool {
                self.state.emit("    li t0, 0");
            }
            // t0 has old value if !returns_bool
            self.state.emit(&format!("{}:", done_label));
        }
        self.store_t0_to(dest);
    }

    fn emit_atomic_load(&mut self, dest: &Value, ptr: &Operand, ty: IrType, _ordering: AtomicOrdering) {
        self.operand_to_t0(ptr);
        if Self::is_subword_type(ty) {
            // For sub-word atomic loads, use regular load + fence.
            // On RISC-V, aligned byte/halfword loads are naturally atomic for
            // single-copy atomicity. Use fence for ordering.
            self.state.emit("    fence rw, rw");
            match ty {
                IrType::I8 => self.state.emit("    lb t0, 0(t0)"),
                IrType::U8 => self.state.emit("    lbu t0, 0(t0)"),
                IrType::I16 => self.state.emit("    lh t0, 0(t0)"),
                IrType::U16 => self.state.emit("    lhu t0, 0(t0)"),
                _ => unreachable!(),
            }
            self.state.emit("    fence rw, rw");
        } else {
            // Use lr for word/doubleword atomic load
            let suffix = Self::amo_width_suffix(ty);
            self.state.emit(&format!("    lr.{}.aq t0, (t0)", suffix));
            Self::sign_extend_riscv(&mut self.state, ty);
        }
        self.store_t0_to(dest);
    }

    fn emit_atomic_store(&mut self, ptr: &Operand, val: &Operand, ty: IrType, ordering: AtomicOrdering) {
        self.operand_to_t0(val);
        self.state.emit("    mv t1, t0"); // t1 = val
        self.operand_to_t0(ptr);
        if Self::is_subword_type(ty) {
            // For sub-word atomic stores, use fence + regular store + fence.
            // Aligned byte/halfword stores are naturally atomic on RISC-V.
            self.state.emit("    fence rw, rw");
            match ty {
                IrType::I8 | IrType::U8 => self.state.emit("    sb t1, 0(t0)"),
                IrType::I16 | IrType::U16 => self.state.emit("    sh t1, 0(t0)"),
                _ => unreachable!(),
            }
            self.state.emit("    fence rw, rw");
        } else {
            // Use amoswap with zero dest for atomic store
            let aq_rl = Self::amo_ordering(ordering);
            let suffix = Self::amo_width_suffix(ty);
            self.state.emit(&format!("    amoswap.{}{} zero, t1, (t0)", suffix, aq_rl));
        }
    }

    fn emit_fence(&mut self, _ordering: AtomicOrdering) {
        self.state.emit("    fence rw, rw");
    }

    fn emit_inline_asm(&mut self, template: &str, outputs: &[(String, Value, Option<String>)], inputs: &[(String, Operand, Option<String>)], _clobbers: &[String], operand_types: &[IrType]) {
        emit_inline_asm_common(self, template, outputs, inputs, operand_types);
    }

    fn emit_copy_i128(&mut self, dest: &Value, src: &Operand) {
        // Full 128-bit copy: load src into t0:t1, store to dest
        self.operand_to_t0_t1(src);
        self.store_t0_t1_to(dest);
    }
}

impl Default for RiscvCodegen {
    fn default() -> Self {
        Self::new()
    }
}

/// RISC-V scratch registers for inline asm.
const RISCV_GP_SCRATCH: &[&str] = &["t0", "t1", "t2", "t3", "t4", "t5", "t6", "a2", "a3", "a4", "a5", "a6", "a7"];
const RISCV_FP_SCRATCH: &[&str] = &["ft0", "ft1", "ft2", "ft3", "ft4", "ft5", "ft6", "ft7"];

impl InlineAsmEmitter for RiscvCodegen {
    fn asm_state(&mut self) -> &mut CodegenState { &mut self.state }
    fn asm_state_ref(&self) -> &CodegenState { &self.state }

    fn classify_constraint(&self, constraint: &str) -> AsmOperandKind {
        // Map RISC-V's richer constraint types into the shared AsmOperandKind.
        let rv_kind = classify_rv_constraint(constraint);
        match rv_kind {
            RvConstraintKind::GpReg => AsmOperandKind::GpReg,
            RvConstraintKind::FpReg => AsmOperandKind::FpReg,
            RvConstraintKind::Memory => AsmOperandKind::Memory,
            RvConstraintKind::Address => AsmOperandKind::Address,
            RvConstraintKind::Immediate => AsmOperandKind::Immediate,
            RvConstraintKind::ZeroOrReg => AsmOperandKind::ZeroOrReg,
            RvConstraintKind::Specific(reg) => AsmOperandKind::Specific(reg),
            RvConstraintKind::Tied(n) => AsmOperandKind::Tied(n),
        }
    }

    fn setup_operand_metadata(&self, op: &mut AsmOperand, val: &Operand, _is_output: bool) {
        match &op.kind {
            AsmOperandKind::Memory | AsmOperandKind::Address => {
                if let Operand::Value(v) = val {
                    if let Some(slot) = self.state.get_slot(v.0) {
                        if self.state.is_alloca(v.0) {
                            // Alloca: stack slot IS the memory location
                            op.mem_offset = slot.0;
                        } else {
                            // Non-alloca: slot holds a pointer that needs indirection.
                            // Mark with empty mem_addr; resolve_memory_operand will handle it.
                            op.mem_addr = String::new();
                            op.mem_offset = 0;
                        }
                    }
                }
            }
            AsmOperandKind::Immediate => {
                if let Operand::Const(c) = val {
                    op.imm_value = Some(c.to_i64().unwrap_or(0));
                } else {
                    // Value operand for immediate constraint: fall back to GP register
                    op.kind = AsmOperandKind::GpReg;
                }
            }
            AsmOperandKind::ZeroOrReg => {
                // If the value is a constant 0, use "zero" register
                if let Operand::Const(c) = val {
                    if c.to_i64() == Some(0) {
                        op.reg = "zero".to_string();
                    }
                }
            }
            _ => {}
        }
    }

    fn resolve_memory_operand(&mut self, op: &mut AsmOperand, val: &Operand) -> bool {
        // If mem_addr is set or mem_offset is non-zero (alloca case), nothing to do
        if !op.mem_addr.is_empty() || op.mem_offset != 0 {
            return false;
        }
        // Load the pointer value into a temporary register for indirect addressing
        if let Operand::Value(v) = val {
            if let Some(slot) = self.state.get_slot(v.0) {
                let tmp_reg = "t0";
                self.state.emit(&format!("    ld {}, {}(s0)", tmp_reg, slot.0));
                op.mem_addr = format!("0({})", tmp_reg);
                return true;
            }
        }
        false
    }

    fn assign_scratch_reg(&mut self, kind: &AsmOperandKind) -> String {
        match kind {
            AsmOperandKind::FpReg => {
                let idx = self.asm_fp_scratch_idx;
                self.asm_fp_scratch_idx += 1;
                if idx < RISCV_FP_SCRATCH.len() {
                    RISCV_FP_SCRATCH[idx].to_string()
                } else {
                    format!("fs{}", idx - RISCV_FP_SCRATCH.len())
                }
            }
            _ => {
                let idx = self.asm_gp_scratch_idx;
                self.asm_gp_scratch_idx += 1;
                if idx < RISCV_GP_SCRATCH.len() {
                    RISCV_GP_SCRATCH[idx].to_string()
                } else {
                    format!("s{}", 2 + idx - RISCV_GP_SCRATCH.len())
                }
            }
        }
    }

    fn load_input_to_reg(&mut self, op: &AsmOperand, val: &Operand, constraint: &str) {
        let reg = &op.reg;
        let is_fp = matches!(op.kind, AsmOperandKind::FpReg);
        let is_addr = matches!(op.kind, AsmOperandKind::Address);

        match val {
            Operand::Const(c) => {
                if is_fp {
                    let imm = c.to_i64().unwrap_or(0);
                    self.state.emit(&format!("    li t5, {}", imm));
                    if constraint.contains('f') && !constraint.contains("64") {
                        self.state.emit(&format!("    fmv.w.x {}, t5", reg));
                    } else {
                        self.state.emit(&format!("    fmv.d.x {}, t5", reg));
                    }
                } else {
                    let imm = c.to_i64().unwrap_or(0);
                    self.state.emit(&format!("    li {}, {}", reg, imm));
                }
            }
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    if is_addr || (!is_addr && self.state.is_alloca(v.0)) {
                        self.emit_addi_s0(reg, slot.0);
                    } else if is_fp {
                        self.emit_load_from_s0(reg, slot.0, "fld");
                    } else {
                        self.emit_load_from_s0(reg, slot.0, "ld");
                    }
                }
            }
        }
    }

    fn preload_readwrite_output(&mut self, op: &AsmOperand, ptr: &Value) {
        let reg = op.reg.clone();
        if let Some(slot) = self.state.get_slot(ptr.0) {
            match &op.kind {
                AsmOperandKind::Address => {
                    self.emit_addi_s0(&reg, slot.0);
                }
                AsmOperandKind::FpReg => {
                    self.emit_load_from_s0(&reg, slot.0, "fld");
                }
                AsmOperandKind::Memory => {} // No preload for memory
                _ => {
                    self.emit_load_from_s0(&reg, slot.0, "ld");
                }
            }
        }
    }

    fn substitute_template_line(&self, line: &str, operands: &[AsmOperand], gcc_to_internal: &[usize], _operand_types: &[IrType]) -> String {
        // Build parallel arrays for the RISC-V substitution function
        let op_regs: Vec<String> = operands.iter().map(|o| o.reg.clone()).collect();
        let op_names: Vec<Option<String>> = operands.iter().map(|o| o.name.clone()).collect();
        let op_mem_offsets: Vec<i64> = operands.iter().map(|o| o.mem_offset).collect();
        let op_mem_addrs: Vec<String> = operands.iter().map(|o| o.mem_addr.clone()).collect();
        let op_imm_values: Vec<Option<i64>> = operands.iter().map(|o| o.imm_value).collect();

        // Convert AsmOperandKind back to RvConstraintKind for the substitution function
        let op_kinds: Vec<RvConstraintKind> = operands.iter().map(|o| match &o.kind {
            AsmOperandKind::GpReg => RvConstraintKind::GpReg,
            AsmOperandKind::FpReg => RvConstraintKind::FpReg,
            AsmOperandKind::Memory => RvConstraintKind::Memory,
            AsmOperandKind::Address => RvConstraintKind::Address,
            AsmOperandKind::Immediate => RvConstraintKind::Immediate,
            AsmOperandKind::ZeroOrReg => RvConstraintKind::ZeroOrReg,
            AsmOperandKind::Specific(r) => RvConstraintKind::Specific(r.clone()),
            AsmOperandKind::Tied(n) => RvConstraintKind::Tied(*n),
        }).collect();

        Self::substitute_riscv_asm_operands(line, &op_regs, &op_names, &op_kinds, &op_mem_offsets, &op_mem_addrs, &op_imm_values, gcc_to_internal)
    }

    fn store_output_from_reg(&mut self, op: &AsmOperand, ptr: &Value, _constraint: &str) {
        match &op.kind {
            AsmOperandKind::Memory => return,
            AsmOperandKind::Address => return, // AMO/LR/SC wrote through the pointer
            AsmOperandKind::FpReg => {
                let reg = op.reg.clone();
                if let Some(slot) = self.state.get_slot(ptr.0) {
                    if self.state.is_alloca(ptr.0) {
                        self.emit_store_to_s0(&reg, slot.0, "fsd");
                    } else {
                        // Non-alloca: slot holds a pointer, store through it
                        self.state.emit(&format!("    ld t0, {}(s0)", slot.0));
                        self.state.emit(&format!("    fsd {}, 0(t0)", reg));
                    }
                }
            }
            _ => {
                let reg = op.reg.clone();
                if let Some(slot) = self.state.get_slot(ptr.0) {
                    if self.state.is_alloca(ptr.0) {
                        self.emit_store_to_s0(&reg, slot.0, "sd");
                    } else {
                        // Non-alloca: slot holds a pointer, store through it
                        let scratch = if reg != "t0" { "t0" } else { "t1" };
                        self.state.emit(&format!("    ld {}, {}(s0)", scratch, slot.0));
                        self.state.emit(&format!("    sd {}, 0({})", reg, scratch));
                    }
                }
            }
        }
    }

    fn reset_scratch_state(&mut self) {
        self.asm_gp_scratch_idx = 0;
        self.asm_fp_scratch_idx = 0;
    }
}
