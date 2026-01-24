use crate::ir::ir::*;
use crate::common::types::IrType;
use crate::backend::common::PtrDirective;
use crate::backend::codegen_shared::*;

/// AArch64 code generator. Implements the ArchCodegen trait for the shared framework.
/// Uses AAPCS64 calling convention with stack-based allocation.
pub struct ArmCodegen {
    state: CodegenState,
    /// Frame size for the current function (needed for epilogue in terminators).
    current_frame_size: i64,
    current_return_type: IrType,
    /// For variadic functions: offset from SP where the GP register save area starts (x0-x7).
    va_gp_save_offset: i64,
    /// For variadic functions: offset from SP where the FP register save area starts (q0-q7).
    va_fp_save_offset: i64,
    /// Number of named (non-variadic) GP params for current variadic function.
    va_named_gp_count: usize,
    /// Number of named (non-variadic) FP params for current variadic function.
    va_named_fp_count: usize,
    /// Number of named GP params that are passed on the stack (beyond 8 register args).
    va_named_stack_gp_count: usize,
    /// Scratch register index for inline asm allocation
    asm_scratch_idx: usize,
}

impl ArmCodegen {
    pub fn new() -> Self {
        Self {
            state: CodegenState::new(),
            current_frame_size: 0,
            current_return_type: IrType::I64,
            va_gp_save_offset: 0,
            va_fp_save_offset: 0,
            va_named_gp_count: 0,
            va_named_fp_count: 0,
            va_named_stack_gp_count: 0,
            asm_scratch_idx: 0,
        }
    }

    pub fn generate(mut self, module: &IrModule) -> String {
        generate_module(&mut self, module)
    }

    // --- AArch64 large-offset helpers ---

    /// Emit a large immediate subtraction from sp. Uses x17 (IP1) as scratch.
    fn emit_sub_sp(&mut self, n: i64) {
        if n == 0 { return; }
        if n <= 4095 {
            self.state.emit(&format!("    sub sp, sp, #{}", n));
        } else {
            self.emit_load_imm64("x17", n);
            self.state.emit("    sub sp, sp, x17");
        }
    }

    /// Emit a large immediate addition to sp. Uses x17 (IP1) as scratch.
    fn emit_add_sp(&mut self, n: i64) {
        if n == 0 { return; }
        if n <= 4095 {
            self.state.emit(&format!("    add sp, sp, #{}", n));
        } else {
            self.emit_load_imm64("x17", n);
            self.state.emit("    add sp, sp, x17");
        }
    }

    /// Get the access size in bytes for an AArch64 load/store instruction and register.
    /// For str/ldr, the access size depends on the register (w=4, x=8).
    fn access_size_for_instr(instr: &str, reg: &str) -> i64 {
        match instr {
            "strb" | "ldrb" | "ldrsb" => 1,
            "strh" | "ldrh" | "ldrsh" => 2,
            "ldrsw" => 4,
            "str" | "ldr" => {
                if reg.starts_with('w') { 4 } else { 8 }
            }
            _ => 1, // conservative default
        }
    }

    /// Check if an offset is valid for unsigned immediate addressing on AArch64.
    /// The unsigned offset is a 12-bit field scaled by access size: max = 4095 * access_size.
    /// The offset must also be naturally aligned to the access size.
    fn is_valid_imm_offset(offset: i64, instr: &str, reg: &str) -> bool {
        if offset < 0 { return false; }
        let access_size = Self::access_size_for_instr(instr, reg);
        let max_offset = 4095 * access_size;
        offset <= max_offset && offset % access_size == 0
    }

    /// Emit store to [sp, #offset], handling large offsets via x17.
    /// Uses x17 (IP1) as scratch to avoid conflicts with x9-x16 call argument temps.
    fn emit_store_to_sp(&mut self, reg: &str, offset: i64, instr: &str) {
        if Self::is_valid_imm_offset(offset, instr, reg) {
            self.state.emit(&format!("    {} {}, [sp, #{}]", instr, reg, offset));
        } else {
            self.load_large_imm("x17", offset);
            self.state.emit("    add x17, sp, x17");
            self.state.emit(&format!("    {} {}, [x17]", instr, reg));
        }
    }

    /// Emit load from [sp, #offset], handling large offsets via x17.
    /// Uses x17 (IP1) as scratch to avoid conflicts with x9-x16 call argument temps.
    fn emit_load_from_sp(&mut self, reg: &str, offset: i64, instr: &str) {
        if Self::is_valid_imm_offset(offset, instr, reg) {
            self.state.emit(&format!("    {} {}, [sp, #{}]", instr, reg, offset));
        } else {
            self.load_large_imm("x17", offset);
            self.state.emit("    add x17, sp, x17");
            self.state.emit(&format!("    {} {}, [x17]", instr, reg));
        }
    }

    /// Emit `stp reg1, reg2, [sp, #offset]` handling large offsets.
    fn emit_stp_to_sp(&mut self, reg1: &str, reg2: &str, offset: i64) {
        // stp supports signed offsets in [-512, 504] range (multiples of 8)
        if offset >= -512 && offset <= 504 {
            self.state.emit(&format!("    stp {}, {}, [sp, #{}]", reg1, reg2, offset));
        } else {
            self.load_large_imm("x17", offset);
            self.state.emit("    add x17, sp, x17");
            self.state.emit(&format!("    stp {}, {}, [x17]", reg1, reg2));
        }
    }

    /// Emit `add dest, sp, #offset` handling large offsets.
    /// Uses x17 (IP1) as scratch to avoid conflicts with x9-x16 call argument temps.
    fn emit_add_sp_offset(&mut self, dest: &str, offset: i64) {
        if offset >= 0 && offset <= 4095 {
            self.state.emit(&format!("    add {}, sp, #{}", dest, offset));
        } else {
            self.load_large_imm("x17", offset);
            self.state.emit(&format!("    add {}, sp, x17", dest));
        }
    }

    /// Emit `add dest, x29, #offset` handling large offsets.
    /// Uses x17 (IP1) as scratch for offsets > 4095.
    fn emit_add_fp_offset(&mut self, dest: &str, offset: i64) {
        if offset >= 0 && offset <= 4095 {
            self.state.emit(&format!("    add {}, x29, #{}", dest, offset));
        } else {
            self.load_large_imm("x17", offset);
            self.state.emit(&format!("    add {}, x29, x17", dest));
        }
    }

    /// Load a large immediate into a register.
    fn load_large_imm(&mut self, reg: &str, val: i64) {
        if val <= 65535 {
            self.state.emit(&format!("    mov {}, #{}", reg, val));
        } else {
            self.state.emit(&format!("    movz {}, #{}", reg, val & 0xFFFF));
            if (val >> 16) & 0xFFFF != 0 {
                self.state.emit(&format!("    movk {}, #{}, lsl #16", reg, (val >> 16) & 0xFFFF));
            }
        }
    }

    /// Load a 64-bit immediate value into a register using mov/movk sequence.
    fn emit_load_imm64(&mut self, reg: &str, val: i64) {
        let bits = val as u64;
        if bits == 0 {
            self.state.emit(&format!("    mov {}, #0", reg));
            return;
        }
        self.state.emit(&format!("    mov {}, #{}", reg, bits & 0xffff));
        if (bits >> 16) & 0xffff != 0 {
            self.state.emit(&format!("    movk {}, #{}, lsl #16", reg, (bits >> 16) & 0xffff));
        }
        if (bits >> 32) & 0xffff != 0 {
            self.state.emit(&format!("    movk {}, #{}, lsl #32", reg, (bits >> 32) & 0xffff));
        }
        if (bits >> 48) & 0xffff != 0 {
            self.state.emit(&format!("    movk {}, #{}, lsl #48", reg, (bits >> 48) & 0xffff));
        }
    }

    /// Emit function prologue: allocate stack and save fp/lr.
    fn emit_prologue_arm(&mut self, frame_size: i64) {
        if frame_size > 0 && frame_size <= 504 {
            self.state.emit(&format!("    stp x29, x30, [sp, #-{}]!", frame_size));
        } else {
            self.emit_sub_sp(frame_size);
            self.state.emit("    stp x29, x30, [sp]");
        }
        self.state.emit("    mov x29, sp");
    }

    /// Emit function epilogue: restore fp/lr and deallocate stack.
    fn emit_epilogue_arm(&mut self, frame_size: i64) {
        if frame_size > 0 && frame_size <= 504 {
            self.state.emit(&format!("    ldp x29, x30, [sp], #{}", frame_size));
        } else {
            self.state.emit("    ldp x29, x30, [sp]");
            self.emit_add_sp(frame_size);
        }
    }

    /// Load an operand into x0.
    fn operand_to_x0(&mut self, op: &Operand) {
        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::I8(v) => self.state.emit(&format!("    mov x0, #{}", v)),
                    IrConst::I16(v) => self.state.emit(&format!("    mov x0, #{}", v)),
                    IrConst::I32(v) => {
                        if *v >= 0 && *v <= 65535 {
                            self.state.emit(&format!("    mov x0, #{}", v));
                        } else if *v < 0 && *v >= -65536 {
                            self.state.emit(&format!("    mov x0, #{}", v));
                        } else {
                            self.state.emit(&format!("    mov x0, #{}", *v as u32 & 0xffff));
                            self.state.emit(&format!("    movk x0, #{}, lsl #16", (*v as u32 >> 16) & 0xffff));
                        }
                    }
                    IrConst::I64(v) => {
                        if *v >= 0 && *v <= 65535 {
                            self.state.emit(&format!("    mov x0, #{}", v));
                        } else if *v < 0 && *v >= -65536 {
                            self.state.emit(&format!("    mov x0, #{}", v));
                        } else {
                            self.emit_load_imm64("x0", *v);
                        }
                    }
                    IrConst::F32(v) => self.emit_load_imm64("x0", v.to_bits() as i64),
                    IrConst::F64(v) => self.emit_load_imm64("x0", v.to_bits() as i64),
                    IrConst::LongDouble(v) => self.emit_load_imm64("x0", v.to_bits() as i64),
                    IrConst::I128(v) => self.emit_load_imm64("x0", *v as i64), // truncate to 64-bit
                    IrConst::Zero => self.state.emit("    mov x0, #0"),
                }
            }
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    if self.state.is_alloca(v.0) {
                        self.emit_add_sp_offset("x0", slot.0);
                    } else {
                        self.emit_load_from_sp("x0", slot.0, "ldr");
                    }
                } else {
                    self.state.emit("    mov x0, #0");
                }
            }
        }
    }

    /// Store x0 to a value's stack slot.
    fn store_x0_to(&mut self, dest: &Value) {
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.emit_store_to_sp("x0", slot.0, "str");
        }
    }

    // --- 128-bit integer helpers ---
    // Convention: 128-bit values use x0 (low 64 bits) and x1 (high 64 bits).
    // Stack slots for 128-bit values are 16 bytes: slot(sp) = low, slot+8(sp) = high.

    /// Load a 128-bit operand into x0 (low) : x1 (high).
    fn operand_to_x0_x1(&mut self, op: &Operand) {
        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::I128(v) => {
                        let low = *v as u64;
                        let high = (*v >> 64) as u64;
                        self.emit_load_imm64("x0", low as i64);
                        self.emit_load_imm64("x1", high as i64);
                    }
                    IrConst::Zero => {
                        self.state.emit("    mov x0, #0");
                        self.state.emit("    mov x1, #0");
                    }
                    _ => {
                        // Other consts: load into x0, zero-extend high half
                        self.operand_to_x0(op);
                        self.state.emit("    mov x1, #0");
                    }
                }
            }
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    if self.state.is_alloca(v.0) {
                        // Alloca: address, not a 128-bit value itself
                        self.emit_add_sp_offset("x0", slot.0);
                        self.state.emit("    mov x1, #0");
                    } else {
                        // 128-bit value in 16-byte stack slot
                        self.emit_load_from_sp("x0", slot.0, "ldr");
                        self.emit_load_from_sp("x1", slot.0 + 8, "ldr");
                    }
                } else {
                    self.state.emit("    mov x0, #0");
                    self.state.emit("    mov x1, #0");
                }
            }
        }
    }

    /// Store x0 (low) : x1 (high) to a 128-bit value's stack slot.
    fn store_x0_x1_to(&mut self, dest: &Value) {
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.emit_store_to_sp("x0", slot.0, "str");
            self.emit_store_to_sp("x1", slot.0 + 8, "str");
        }
    }

    /// Prepare a 128-bit binary operation: load lhs into x2:x3, rhs into x4:x5.
    /// (Uses x0:x1 as temporaries during loading.)
    fn prep_i128_binop(&mut self, lhs: &Operand, rhs: &Operand) {
        self.operand_to_x0_x1(lhs);
        self.state.emit("    mov x2, x0");
        self.state.emit("    mov x3, x1");
        self.operand_to_x0_x1(rhs);
        self.state.emit("    mov x4, x0");
        self.state.emit("    mov x5, x1");
    }

    /// Emit a 128-bit integer binary operation.
    fn emit_i128_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand) {
        match op {
            IrBinOp::Add => {
                self.prep_i128_binop(lhs, rhs);
                self.state.emit("    adds x0, x2, x4");
                self.state.emit("    adc x1, x3, x5");
            }
            IrBinOp::Sub => {
                self.prep_i128_binop(lhs, rhs);
                self.state.emit("    subs x0, x2, x4");
                self.state.emit("    sbc x1, x3, x5");
            }
            IrBinOp::Mul => {
                // 128-bit multiply: result_lo = a_lo * b_lo (full widening)
                // result_hi = a_hi * b_lo + a_lo * b_hi + umulh(a_lo, b_lo)
                self.prep_i128_binop(lhs, rhs);
                // x2:x3 = lhs (lo:hi), x4:x5 = rhs (lo:hi)
                self.state.emit("    mul x0, x2, x4");       // x0 = lo(a_lo * b_lo)
                self.state.emit("    umulh x1, x2, x4");     // x1 = hi(a_lo * b_lo)
                self.state.emit("    madd x1, x3, x4, x1");  // x1 += a_hi * b_lo
                self.state.emit("    madd x1, x2, x5, x1");  // x1 += a_lo * b_hi
            }
            IrBinOp::And => {
                self.prep_i128_binop(lhs, rhs);
                self.state.emit("    and x0, x2, x4");
                self.state.emit("    and x1, x3, x5");
            }
            IrBinOp::Or => {
                self.prep_i128_binop(lhs, rhs);
                self.state.emit("    orr x0, x2, x4");
                self.state.emit("    orr x1, x3, x5");
            }
            IrBinOp::Xor => {
                self.prep_i128_binop(lhs, rhs);
                self.state.emit("    eor x0, x2, x4");
                self.state.emit("    eor x1, x3, x5");
            }
            IrBinOp::Shl => {
                // 128-bit left shift by amount in x4 (low 64 bits of rhs)
                self.prep_i128_binop(lhs, rhs);
                // x2:x3 = value, x4 = shift amount
                let lbl = self.state.fresh_label("shl128");
                let done = self.state.fresh_label("shl128_done");
                let noop = self.state.fresh_label("shl128_noop");
                self.state.emit("    and x4, x4, #127");        // mask to 0-127
                self.state.emit(&format!("    cbz x4, {}", noop)); // shift 0 = copy
                self.state.emit("    cmp x4, #64");
                self.state.emit(&format!("    b.ge {}", lbl));
                // shift < 64: hi = (hi << n) | (lo >> (64-n)), lo = lo << n
                self.state.emit("    lsl x1, x3, x4");
                self.state.emit("    mov x5, #64");
                self.state.emit("    sub x5, x5, x4");
                self.state.emit("    lsr x6, x2, x5");
                self.state.emit("    orr x1, x1, x6");
                self.state.emit("    lsl x0, x2, x4");
                self.state.emit(&format!("    b {}", done));
                // shift >= 64: hi = lo << (n-64), lo = 0
                self.state.emit(&format!("{}:", lbl));
                self.state.emit("    sub x4, x4, #64");
                self.state.emit("    lsl x1, x2, x4");
                self.state.emit("    mov x0, #0");
                self.state.emit(&format!("    b {}", done));
                // shift == 0: copy input to output
                self.state.emit(&format!("{}:", noop));
                self.state.emit("    mov x0, x2");
                self.state.emit("    mov x1, x3");
                self.state.emit(&format!("{}:", done));
            }
            IrBinOp::LShr => {
                // 128-bit logical right shift
                self.prep_i128_binop(lhs, rhs);
                let lbl = self.state.fresh_label("lshr128");
                let done = self.state.fresh_label("lshr128_done");
                let noop = self.state.fresh_label("lshr128_noop");
                self.state.emit("    and x4, x4, #127");
                self.state.emit(&format!("    cbz x4, {}", noop));
                self.state.emit("    cmp x4, #64");
                self.state.emit(&format!("    b.ge {}", lbl));
                // shift < 64: lo = (lo >> n) | (hi << (64-n)), hi = hi >> n
                self.state.emit("    lsr x0, x2, x4");
                self.state.emit("    mov x5, #64");
                self.state.emit("    sub x5, x5, x4");
                self.state.emit("    lsl x6, x3, x5");
                self.state.emit("    orr x0, x0, x6");
                self.state.emit("    lsr x1, x3, x4");
                self.state.emit(&format!("    b {}", done));
                // shift >= 64: lo = hi >> (n-64), hi = 0
                self.state.emit(&format!("{}:", lbl));
                self.state.emit("    sub x4, x4, #64");
                self.state.emit("    lsr x0, x3, x4");
                self.state.emit("    mov x1, #0");
                self.state.emit(&format!("    b {}", done));
                // shift == 0: copy input to output
                self.state.emit(&format!("{}:", noop));
                self.state.emit("    mov x0, x2");
                self.state.emit("    mov x1, x3");
                self.state.emit(&format!("{}:", done));
            }
            IrBinOp::AShr => {
                // 128-bit arithmetic right shift
                self.prep_i128_binop(lhs, rhs);
                let lbl = self.state.fresh_label("ashr128");
                let done = self.state.fresh_label("ashr128_done");
                let noop = self.state.fresh_label("ashr128_noop");
                self.state.emit("    and x4, x4, #127");
                self.state.emit(&format!("    cbz x4, {}", noop));
                self.state.emit("    cmp x4, #64");
                self.state.emit(&format!("    b.ge {}", lbl));
                // shift < 64: lo = (lo >> n) | (hi << (64-n)), hi = hi >>a n
                self.state.emit("    lsr x0, x2, x4");
                self.state.emit("    mov x5, #64");
                self.state.emit("    sub x5, x5, x4");
                self.state.emit("    lsl x6, x3, x5");
                self.state.emit("    orr x0, x0, x6");
                self.state.emit("    asr x1, x3, x4");
                self.state.emit(&format!("    b {}", done));
                // shift >= 64: lo = hi >>a (n-64), hi = hi >>a 63
                self.state.emit(&format!("{}:", lbl));
                self.state.emit("    sub x4, x4, #64");
                self.state.emit("    asr x0, x3, x4");
                self.state.emit("    asr x1, x3, #63");
                self.state.emit(&format!("    b {}", done));
                // shift == 0: copy input to output
                self.state.emit(&format!("{}:", noop));
                self.state.emit("    mov x0, x2");
                self.state.emit("    mov x1, x3");
                self.state.emit(&format!("{}:", done));
            }
            IrBinOp::SDiv | IrBinOp::UDiv | IrBinOp::SRem | IrBinOp::URem => {
                // Call compiler-rt helper functions for 128-bit division
                let func_name = match op {
                    IrBinOp::SDiv => "__divti3",
                    IrBinOp::UDiv => "__udivti3",
                    IrBinOp::SRem => "__modti3",
                    IrBinOp::URem => "__umodti3",
                    _ => unreachable!(),
                };
                // AAPCS64: first 128-bit arg in x0:x1, second in x2:x3
                self.operand_to_x0_x1(lhs);
                self.state.emit("    mov x2, x0");
                self.state.emit("    mov x3, x1");
                self.operand_to_x0_x1(rhs);
                self.state.emit("    mov x4, x0");
                self.state.emit("    mov x5, x1");
                // Move to correct argument registers: lhs in x0:x1, rhs in x2:x3
                self.state.emit("    mov x0, x2");
                self.state.emit("    mov x1, x3");
                self.state.emit("    mov x2, x4");
                self.state.emit("    mov x3, x5");
                self.state.emit(&format!("    bl {}", func_name));
                // Result in x0:x1
            }
        }
        self.store_x0_x1_to(dest);
    }

    /// Emit a 128-bit comparison.
    fn emit_i128_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand) {
        self.prep_i128_binop(lhs, rhs);
        // x2:x3 = lhs, x4:x5 = rhs
        match op {
            IrCmpOp::Eq => {
                // XOR both halves and OR the differences
                self.state.emit("    eor x0, x2, x4");
                self.state.emit("    eor x1, x3, x5");
                self.state.emit("    orr x0, x0, x1");
                self.state.emit("    cmp x0, #0");
                self.state.emit("    cset x0, eq");
            }
            IrCmpOp::Ne => {
                self.state.emit("    eor x0, x2, x4");
                self.state.emit("    eor x1, x3, x5");
                self.state.emit("    orr x0, x0, x1");
                self.state.emit("    cmp x0, #0");
                self.state.emit("    cset x0, ne");
            }
            _ => {
                // Ordered comparisons: compare high halves first, fall through to low if equal
                let done = self.state.fresh_label("cmp128_done");
                // Compare high halves
                self.state.emit("    cmp x3, x5");
                let (hi_cond, lo_cond) = match op {
                    IrCmpOp::Slt | IrCmpOp::Sle => ("lt", if op == IrCmpOp::Slt { "lo" } else { "ls" }),
                    IrCmpOp::Sgt | IrCmpOp::Sge => ("gt", if op == IrCmpOp::Sgt { "hi" } else { "hs" }),
                    IrCmpOp::Ult | IrCmpOp::Ule => ("lo", if op == IrCmpOp::Ult { "lo" } else { "ls" }),
                    IrCmpOp::Ugt | IrCmpOp::Uge => ("hi", if op == IrCmpOp::Ugt { "hi" } else { "hs" }),
                    _ => unreachable!(),
                };
                self.state.emit(&format!("    cset x0, {}", hi_cond));
                self.state.emit(&format!("    b.ne {}", done));
                // High halves equal: compare low halves (always unsigned)
                self.state.emit("    cmp x2, x4");
                self.state.emit(&format!("    cset x0, {}", lo_cond));
                self.state.emit(&format!("{}:", done));
            }
        }
        self.store_x0_to(dest);
    }

    fn str_for_type(ty: IrType) -> &'static str {
        match ty {
            IrType::I8 | IrType::U8 => "strb",
            IrType::I16 | IrType::U16 => "strh",
            IrType::I32 | IrType::U32 | IrType::F32 => "str",  // 32-bit store with w register
            _ => "str",  // 64-bit store with x register
        }
    }

    fn ldr_for_type(ty: IrType) -> &'static str {
        match ty {
            IrType::I8 => "ldrsb",
            IrType::U8 => "ldrb",
            IrType::I16 => "ldrsh",
            IrType::U16 => "ldrh",
            IrType::I32 => "ldrsw",
            IrType::U32 | IrType::F32 => "ldr",  // 32-bit load
            _ => "ldr",
        }
    }

    fn load_dest_reg(ty: IrType) -> &'static str {
        match ty {
            IrType::U8 | IrType::U16 | IrType::U32 | IrType::F32 => "w0",
            _ => "x0",
        }
    }

    /// Get the appropriate register name for a given base and type.
    fn reg_for_type(base: &str, ty: IrType) -> &'static str {
        let use_w = matches!(ty,
            IrType::I8 | IrType::U8 | IrType::I16 | IrType::U16 |
            IrType::I32 | IrType::U32 | IrType::F32
        );
        match base {
            "x0" => if use_w { "w0" } else { "x0" },
            "x1" => if use_w { "w1" } else { "x1" },
            "x2" => if use_w { "w2" } else { "x2" },
            "x3" => if use_w { "w3" } else { "x3" },
            "x4" => if use_w { "w4" } else { "x4" },
            "x5" => if use_w { "w5" } else { "x5" },
            "x6" => if use_w { "w6" } else { "x6" },
            "x7" => if use_w { "w7" } else { "x7" },
            _ => "x0",
        }
    }

    /// Parse a load instruction token into the actual ARM instruction and destination register.
    /// ARM's "ldr" instruction is width-polymorphic (the register determines access width),
    /// so load_instr_for_type returns "ldr32"/"ldr64" tokens to distinguish 32-bit from 64-bit.
    fn arm_parse_load(instr: &'static str) -> (&'static str, &'static str) {
        match instr {
            "ldr32" => ("ldr", "w0"),
            "ldr64" => ("ldr", "x0"),
            "ldrb" | "ldrh" => (instr, "w0"),
            // ldrsb, ldrsh, ldrsw all sign-extend into x0
            _ => (instr, "x0"),
        }
    }

}

const ARM_ARG_REGS: [&str; 8] = ["x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"];
const ARM_TMP_REGS: [&str; 8] = ["x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16"];

impl ArchCodegen for ArmCodegen {
    fn state(&mut self) -> &mut CodegenState { &mut self.state }
    fn state_ref(&self) -> &CodegenState { &self.state }

    fn jump_mnemonic(&self) -> &'static str { "b" }
    fn trap_instruction(&self) -> &'static str { "brk #0" }

    fn emit_branch_nonzero(&mut self, label: &str) {
        self.state.emit(&format!("    cbnz x0, {}", label));
    }

    fn emit_jump_indirect(&mut self) {
        self.state.emit("    br x0");
    }
    fn ptr_directive(&self) -> PtrDirective { PtrDirective::Xword }
    fn function_type_directive(&self) -> &'static str { "%function" }

    fn calculate_stack_space(&mut self, func: &IrFunction) -> i64 {
        let mut space = calculate_stack_space_common(&mut self.state, func, 16, |space, alloc_size, align| {
            // ARM uses positive offsets from sp, starting at 16 (after fp/lr)
            // Honor alignment: round up slot offset to alignment boundary
            let effective_align = if align > 0 { align.max(8) } else { 8 };
            let slot = (space + effective_align - 1) & !(effective_align - 1);
            let new_space = slot + ((alloc_size + 7) & !7).max(8);
            (slot, new_space)
        });

        // For variadic functions, reserve space for register save areas:
        // - GP save area: x0-x7 = 64 bytes (8 regs * 8 bytes)
        // - FP save area: q0-q7 = 128 bytes (8 regs * 16 bytes each)
        if func.is_variadic {
            // GP register save area (64 bytes, 8-byte aligned)
            space = (space + 7) & !7;
            self.va_gp_save_offset = space;
            space += 64; // 8 GP registers * 8 bytes

            // FP register save area (128 bytes, 16-byte aligned)
            space = (space + 15) & !15;
            self.va_fp_save_offset = space;
            space += 128; // 8 FP/SIMD registers * 16 bytes (q0-q7)

            // Count named GP and FP params to know where variadic args start
            let mut named_gp = 0usize;
            let mut named_fp = 0usize;
            for param in &func.params {
                if param.ty.is_float() {
                    named_fp += 1;
                } else {
                    named_gp += 1;
                }
            }
            self.va_named_gp_count = named_gp.min(8);
            self.va_named_fp_count = named_fp.min(8);
            // Track stack-passed named args (GP args beyond 8 register slots)
            self.va_named_stack_gp_count = named_gp.saturating_sub(8);
        }

        space
    }

    fn aligned_frame_size(&self, raw_space: i64) -> i64 {
        (raw_space + 15) & !15
    }

    fn emit_prologue(&mut self, func: &IrFunction, frame_size: i64) {
        self.current_return_type = func.return_type;
        self.current_frame_size = frame_size;
        self.emit_prologue_arm(frame_size);
    }

    fn emit_epilogue(&mut self, frame_size: i64) {
        self.emit_epilogue_arm(frame_size);
    }

    fn emit_store_params(&mut self, func: &IrFunction) {
        let frame_size = self.current_frame_size;

        // For variadic functions: save all register args to save areas first,
        // before any other processing. This allows va_arg to access register-passed
        // variadic arguments.
        if func.is_variadic {
            // Save x0-x7 to GP register save area using stp pairs
            let gp_base = self.va_gp_save_offset;
            for i in (0..8).step_by(2) {
                let offset = gp_base + (i as i64) * 8;
                self.emit_stp_to_sp(&format!("x{}", i), &format!("x{}", i + 1), offset);
            }
            // Save q0-q7 to FP register save area using stp pairs (128-bit each)
            let fp_base = self.va_fp_save_offset;
            for i in (0..8).step_by(2) {
                let offset = fp_base + (i as i64) * 16;
                self.emit_stp_to_sp(&format!("q{}", i), &format!("q{}", i + 1), offset);
            }
        }

        // Classify each param: int reg, float reg, i128 pair, or stack-passed
        let mut int_reg_idx = 0usize;
        let mut float_reg_idx = 0usize;
        let mut param_class: Vec<char> = Vec::new(); // 'i', 'f', 's', 'p' (i128 pair)
        let mut param_int_reg: Vec<usize> = Vec::new();
        let mut param_float_reg: Vec<usize> = Vec::new();
        let mut stack_offsets: Vec<i64> = Vec::new();
        let mut stack_offset: i64 = 0;

        for param in func.params.iter() {
            let is_float = param.ty.is_float();
            let is_i128 = is_i128_type(param.ty);
            if is_i128 {
                // AAPCS64: 128-bit integers in even-aligned GP register pair
                if int_reg_idx % 2 != 0 { int_reg_idx += 1; }
                if int_reg_idx + 1 < 8 {
                    param_class.push('p');
                    param_int_reg.push(int_reg_idx);
                    param_float_reg.push(0);
                    stack_offsets.push(0);
                    int_reg_idx += 2;
                } else {
                    param_class.push('s');
                    param_int_reg.push(0);
                    param_float_reg.push(0);
                    stack_offsets.push(stack_offset);
                    stack_offset += 16;
                    int_reg_idx = 8;
                }
            } else if is_float && float_reg_idx < 8 {
                param_class.push('f');
                param_float_reg.push(float_reg_idx);
                param_int_reg.push(0);
                stack_offsets.push(0);
                float_reg_idx += 1;
            } else if !is_float && int_reg_idx < 8 {
                param_class.push('i');
                param_int_reg.push(int_reg_idx);
                param_float_reg.push(0);
                stack_offsets.push(0);
                int_reg_idx += 1;
            } else {
                param_class.push('s');
                param_int_reg.push(0);
                param_float_reg.push(0);
                stack_offsets.push(stack_offset);
                stack_offset += 8;
            }
        }

        // Phase 1: Store all INTEGER register params first (before x0 gets clobbered by float moves).
        for (i, param) in func.params.iter().enumerate() {
            if param_class[i] == 'p' {
                // 128-bit integer param: arrives in even-aligned GP register pair
                if param.name.is_empty() { continue; }
                if let Some((dest, _ty)) = find_param_alloca(func, i) {
                    if let Some(slot) = self.state.get_slot(dest.0) {
                        let lo_reg = ARM_ARG_REGS[param_int_reg[i]];
                        let hi_reg = ARM_ARG_REGS[param_int_reg[i] + 1];
                        self.emit_store_to_sp(lo_reg, slot.0, "str");
                        self.emit_store_to_sp(hi_reg, slot.0 + 8, "str");
                    }
                }
                continue;
            }
            if param_class[i] != 'i' || param.name.is_empty() { continue; }
            if let Some((dest, ty)) = find_param_alloca(func, i) {
                if let Some(slot) = self.state.get_slot(dest.0) {
                    let store_instr = Self::str_for_type(ty);
                    let reg = Self::reg_for_type(ARM_ARG_REGS[param_int_reg[i]], ty);
                    self.emit_store_to_sp(reg, slot.0, store_instr);
                }
            }
        }

        // Phase 2: Store all FLOAT register params (uses x0 as scratch, but int regs already saved).
        // Check if any F128 params need __trunctfdf2 conversion, which clobbers Q regs.
        let has_f128_params = func.params.iter().enumerate().any(|(i, p)| {
            param_class[i] == 'f' && p.ty.is_long_double()
        });

        if has_f128_params {
            // Save all Q registers to the stack before F128 conversions clobber them.
            // Save q0-q7 (8 regs * 16 bytes = 128 bytes).
            self.emit_sub_sp(128);
            for i in 0..8usize {
                self.state.emit(&format!("    str q{}, [sp, #{}]", i, i * 16));
            }

            // Process non-F128 float params first (from saved Q area)
            for (i, param) in func.params.iter().enumerate() {
                if param_class[i] != 'f' || param.name.is_empty() || param.ty.is_long_double() { continue; }
                if let Some((dest, ty)) = find_param_alloca(func, i) {
                    if let Some(slot) = self.state.get_slot(dest.0) {
                        let fp_reg_off = (param_float_reg[i] * 16) as i64;
                        if ty == IrType::F32 {
                            self.state.emit(&format!("    ldr s0, [sp, #{}]", fp_reg_off));
                            self.state.emit("    fmov w0, s0");
                        } else {
                            self.state.emit(&format!("    ldr d0, [sp, #{}]", fp_reg_off));
                            self.state.emit("    fmov x0, d0");
                        }
                        self.emit_store_to_sp("x0", slot.0 + 128, "str");
                    }
                }
            }

            // Process F128 params: load Q register from save area, call __trunctfdf2
            for (i, param) in func.params.iter().enumerate() {
                if param_class[i] != 'f' || param.name.is_empty() || !param.ty.is_long_double() { continue; }
                if let Some((dest, _ty)) = find_param_alloca(func, i) {
                    if let Some(slot) = self.state.get_slot(dest.0) {
                        let fp_reg_off = (param_float_reg[i] * 16) as i64;
                        // Load the saved Q register value into q0 for __trunctfdf2
                        self.state.emit(&format!("    ldr q0, [sp, #{}]", fp_reg_off));
                        self.state.emit("    bl __trunctfdf2");
                        // __trunctfdf2 returns f64 in d0
                        self.state.emit("    fmov x0, d0");
                        self.emit_store_to_sp("x0", slot.0 + 128, "str");
                    }
                }
            }

            // Restore stack
            self.emit_add_sp(128);
        } else {
            // No F128 params: use the simple path
            for (i, param) in func.params.iter().enumerate() {
                if param_class[i] != 'f' || param.name.is_empty() { continue; }
                if let Some((dest, ty)) = find_param_alloca(func, i) {
                    if let Some(slot) = self.state.get_slot(dest.0) {
                        if ty == IrType::F32 {
                            self.state.emit(&format!("    fmov w0, s{}", param_float_reg[i]));
                            self.emit_store_to_sp("x0", slot.0, "str");
                        } else {
                            self.state.emit(&format!("    fmov x0, d{}", param_float_reg[i]));
                            self.emit_store_to_sp("x0", slot.0, "str");
                        }
                    }
                }
            }
        }

        // Phase 3: Store stack-passed params (above callee's frame).
        for (i, param) in func.params.iter().enumerate() {
            if param_class[i] != 's' || param.name.is_empty() { continue; }
            if let Some((dest, ty)) = find_param_alloca(func, i) {
                if let Some(slot) = self.state.get_slot(dest.0) {
                    let caller_offset = frame_size + stack_offsets[i];
                    if is_i128_type(ty) {
                        // 128-bit stack param: load both halves
                        self.emit_load_from_sp("x0", caller_offset, "ldr");
                        self.emit_store_to_sp("x0", slot.0, "str");
                        self.emit_load_from_sp("x0", caller_offset + 8, "ldr");
                        self.emit_store_to_sp("x0", slot.0 + 8, "str");
                    } else {
                        self.emit_load_from_sp("x0", caller_offset, "ldr");
                        let store_instr = Self::str_for_type(ty);
                        let reg = Self::reg_for_type("x0", ty);
                        self.emit_store_to_sp(reg, slot.0, store_instr);
                    }
                }
            }
        }
    }

    fn emit_load_operand(&mut self, op: &Operand) {
        self.operand_to_x0(op);
    }

    fn emit_store_result(&mut self, dest: &Value) {
        self.store_x0_to(dest);
    }

    // ---- Primitives for shared default implementations ----

    fn emit_load_acc_pair(&mut self, op: &Operand) {
        self.operand_to_x0_x1(op);
    }

    fn emit_store_acc_pair(&mut self, dest: &Value) {
        self.store_x0_x1_to(dest);
    }

    fn emit_store_pair_to_slot(&mut self, slot: StackSlot) {
        self.emit_store_to_sp("x0", slot.0, "str");
        self.emit_store_to_sp("x1", slot.0 + 8, "str");
    }

    fn emit_load_pair_from_slot(&mut self, slot: StackSlot) {
        self.emit_load_from_sp("x0", slot.0, "ldr");
        self.emit_load_from_sp("x1", slot.0 + 8, "ldr");
    }

    fn emit_save_acc_pair(&mut self) {
        self.state.emit("    mov x2, x0");
        self.state.emit("    mov x3, x1");
    }

    fn emit_store_pair_indirect(&mut self) {
        // pair saved to x2:x3, ptr in x9
        self.state.emit("    str x2, [x9]");
        self.state.emit("    str x3, [x9, #8]");
    }

    fn emit_load_pair_indirect(&mut self) {
        // ptr in x9
        self.state.emit("    ldr x0, [x9]");
        self.state.emit("    ldr x1, [x9, #8]");
    }

    fn emit_i128_neg(&mut self) {
        self.state.emit("    mvn x0, x0");
        self.state.emit("    mvn x1, x1");
        self.state.emit("    adds x0, x0, #1");
        self.state.emit("    adc x1, x1, xzr");
    }

    fn emit_i128_not(&mut self) {
        self.state.emit("    mvn x0, x0");
        self.state.emit("    mvn x1, x1");
    }

    fn store_instr_for_type(&self, ty: IrType) -> &'static str {
        Self::str_for_type(ty)
    }

    fn load_instr_for_type(&self, ty: IrType) -> &'static str {
        // Use distinct tokens so emit_typed_load_from_slot/emit_typed_load_indirect
        // can determine the correct destination register for ARM's width-polymorphic "ldr".
        // "ldr" on ARM uses the register operand to determine access width (w0=32-bit, x0=64-bit),
        // so we encode the width in the token: "ldr32" for 32-bit, "ldr64" for 64-bit.
        match ty {
            IrType::I8 => "ldrsb",
            IrType::U8 => "ldrb",
            IrType::I16 => "ldrsh",
            IrType::U16 => "ldrh",
            IrType::I32 => "ldrsw",
            IrType::U32 | IrType::F32 => "ldr32",
            _ => "ldr64",
        }
    }

    fn emit_typed_store_to_slot(&mut self, instr: &'static str, ty: IrType, slot: StackSlot) {
        let reg = Self::reg_for_type("x0", ty);
        self.emit_store_to_sp(reg, slot.0, instr);
    }

    fn emit_typed_load_from_slot(&mut self, instr: &'static str, slot: StackSlot) {
        let (actual_instr, dest_reg) = Self::arm_parse_load(instr);
        self.emit_load_from_sp(dest_reg, slot.0, actual_instr);
    }

    fn emit_save_acc(&mut self) {
        self.state.emit("    mov x1, x0");
    }

    fn emit_load_ptr_from_slot(&mut self, slot: StackSlot) {
        self.emit_load_from_sp("x9", slot.0, "ldr");
    }

    fn emit_typed_store_indirect(&mut self, instr: &'static str, ty: IrType) {
        let reg = Self::reg_for_type("x1", ty);
        self.state.emit(&format!("    {} {}, [x9]", instr, reg));
    }

    fn emit_typed_load_indirect(&mut self, instr: &'static str) {
        let (actual_instr, dest_reg) = Self::arm_parse_load(instr);
        self.state.emit(&format!("    {} {}, [x9]", actual_instr, dest_reg));
    }

    fn emit_slot_addr_to_secondary(&mut self, slot: StackSlot, is_alloca: bool) {
        if is_alloca {
            self.emit_add_sp_offset("x1", slot.0);
        } else {
            self.emit_load_from_sp("x1", slot.0, "ldr");
        }
    }

    fn emit_add_secondary_to_acc(&mut self) {
        self.state.emit("    add x0, x1, x0");
    }

    fn emit_round_up_acc_to_16(&mut self) {
        self.state.emit("    add x0, x0, #15");
        self.state.emit("    and x0, x0, #-16");
    }

    fn emit_sub_sp_by_acc(&mut self) {
        self.state.emit("    sub sp, sp, x0");
    }

    fn emit_mov_sp_to_acc(&mut self) {
        self.state.emit("    mov x0, sp");
    }

    fn emit_align_acc(&mut self, align: usize) {
        self.state.emit(&format!("    add x0, x0, #{}", align - 1));
        self.state.emit(&format!("    and x0, x0, #{}", -(align as i64)));
    }

    fn emit_memcpy_load_dest_addr(&mut self, slot: StackSlot, is_alloca: bool) {
        if is_alloca {
            self.emit_add_sp_offset("x9", slot.0);
        } else {
            self.emit_load_from_sp("x9", slot.0, "ldr");
        }
    }

    fn emit_memcpy_load_src_addr(&mut self, slot: StackSlot, is_alloca: bool) {
        if is_alloca {
            self.emit_add_sp_offset("x10", slot.0);
        } else {
            self.emit_load_from_sp("x10", slot.0, "ldr");
        }
    }

    fn emit_alloca_aligned_addr(&mut self, slot: StackSlot, val_id: u32) {
        let align = self.state.alloca_over_align(val_id).unwrap();
        // Compute sp + slot_offset into x9 (pointer register)
        self.emit_add_sp_offset("x9", slot.0);
        // Align: x9 = (x9 + align-1) & -align
        self.load_large_imm("x17", (align - 1) as i64);
        self.state.emit("    add x9, x9, x17");
        self.load_large_imm("x17", -(align as i64));
        self.state.emit("    and x9, x9, x17");
    }

    fn emit_alloca_aligned_addr_to_acc(&mut self, slot: StackSlot, val_id: u32) {
        let align = self.state.alloca_over_align(val_id).unwrap();
        // Compute sp + slot_offset into x0 (accumulator)
        self.emit_add_sp_offset("x0", slot.0);
        // Align: x0 = (x0 + align-1) & -align
        self.load_large_imm("x17", (align - 1) as i64);
        self.state.emit("    add x0, x0, x17");
        self.load_large_imm("x17", -(align as i64));
        self.state.emit("    and x0, x0, x17");
    }

    fn emit_acc_to_secondary(&mut self) {
        self.state.emit("    mov x1, x0");
    }

    fn emit_memcpy_store_dest_from_acc(&mut self) {
        // x9 already has the aligned addr from emit_alloca_aligned_addr
        // (which puts result in x9, the pointer/memcpy-dest register)
    }

    fn emit_memcpy_store_src_from_acc(&mut self) {
        // Move from pointer register (x9) to memcpy src register (x10)
        self.state.emit("    mov x10, x9");
    }

    fn emit_memcpy_impl(&mut self, size: usize) {
        let label_id = self.state.next_label_id();
        let loop_label = format!(".Lmemcpy_loop_{}", label_id);
        let done_label = format!(".Lmemcpy_done_{}", label_id);
        self.load_large_imm("x11", size as i64);
        self.state.emit(&format!("{}:", loop_label));
        self.state.emit(&format!("    cbz x11, {}", done_label));
        self.state.emit("    ldrb w12, [x10], #1");
        self.state.emit("    strb w12, [x9], #1");
        self.state.emit("    sub x11, x11, #1");
        self.state.emit(&format!("    b {}", loop_label));
        self.state.emit(&format!("{}:", done_label));
    }

    fn emit_float_neg(&mut self, ty: IrType) {
        if ty == IrType::F32 {
            self.state.emit("    fmov s0, w0");
            self.state.emit("    fneg s0, s0");
            self.state.emit("    fmov w0, s0");
            self.state.emit("    mov w0, w0"); // zero-extend
        } else {
            self.state.emit("    fmov d0, x0");
            self.state.emit("    fneg d0, d0");
            self.state.emit("    fmov x0, d0");
        }
    }

    fn emit_int_neg(&mut self, _ty: IrType) {
        self.state.emit("    neg x0, x0");
    }

    fn emit_int_not(&mut self, _ty: IrType) {
        self.state.emit("    mvn x0, x0");
    }

    fn emit_int_clz(&mut self, ty: IrType) {
        if ty == IrType::I32 || ty == IrType::U32 {
            self.state.emit("    clz w0, w0");
        } else {
            self.state.emit("    clz x0, x0");
        }
    }

    fn emit_int_ctz(&mut self, ty: IrType) {
        if ty == IrType::I32 || ty == IrType::U32 {
            self.state.emit("    rbit w0, w0");
            self.state.emit("    clz w0, w0");
        } else {
            self.state.emit("    rbit x0, x0");
            self.state.emit("    clz x0, x0");
        }
    }

    fn emit_int_bswap(&mut self, ty: IrType) {
        if ty == IrType::I16 || ty == IrType::U16 {
            self.state.emit("    rev w0, w0");
            self.state.emit("    lsr w0, w0, #16");
        } else if ty == IrType::I32 || ty == IrType::U32 {
            self.state.emit("    rev w0, w0");
        } else {
            self.state.emit("    rev x0, x0");
        }
    }

    fn emit_int_popcount(&mut self, _ty: IrType) {
        self.state.emit("    fmov d0, x0");
        self.state.emit("    cnt v0.8b, v0.8b");
        self.state.emit("    uaddlv h0, v0.8b");
        self.state.emit("    fmov w0, s0");
    }

    fn emit_float_binop(&mut self, dest: &Value, op: FloatOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        let mnemonic = match op {
            FloatOp::Add => "fadd",
            FloatOp::Sub => "fsub",
            FloatOp::Mul => "fmul",
            FloatOp::Div => "fdiv",
        };
        self.operand_to_x0(lhs);
        self.state.emit("    mov x1, x0");
        self.operand_to_x0(rhs);
        self.state.emit("    mov x2, x0");
        // F128 uses F64 instructions (long double computed at double precision)
        if ty == IrType::F32 {
            self.state.emit("    fmov s0, w1");
            self.state.emit("    fmov s1, w2");
            self.state.emit(&format!("    {} s0, s0, s1", mnemonic));
            self.state.emit("    fmov w0, s0");
            self.state.emit("    mov w0, w0"); // zero-extend
        } else {
            self.state.emit("    fmov d0, x1");
            self.state.emit("    fmov d1, x2");
            self.state.emit(&format!("    {} d0, d0, d1", mnemonic));
            self.state.emit("    fmov x0, d0");
        }
        self.store_x0_to(dest);
    }

    fn emit_int_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if is_i128_type(ty) {
            self.emit_i128_binop(dest, op, lhs, rhs);
            return;
        }

        self.operand_to_x0(lhs);
        self.state.emit("    mov x1, x0");
        self.operand_to_x0(rhs);
        self.state.emit("    mov x2, x0");

        let use_32bit = ty == IrType::I32 || ty == IrType::U32;
        let is_unsigned = ty.is_unsigned();

        if use_32bit {
            match op {
                IrBinOp::Add => {
                    self.state.emit("    add w0, w1, w2");
                    if !is_unsigned { self.state.emit("    sxtw x0, w0"); }
                }
                IrBinOp::Sub => {
                    self.state.emit("    sub w0, w1, w2");
                    if !is_unsigned { self.state.emit("    sxtw x0, w0"); }
                }
                IrBinOp::Mul => {
                    self.state.emit("    mul w0, w1, w2");
                    if !is_unsigned { self.state.emit("    sxtw x0, w0"); }
                }
                IrBinOp::SDiv => {
                    self.state.emit("    sdiv w0, w1, w2");
                    self.state.emit("    sxtw x0, w0");
                }
                IrBinOp::UDiv => self.state.emit("    udiv w0, w1, w2"),
                IrBinOp::SRem => {
                    self.state.emit("    sdiv w3, w1, w2");
                    self.state.emit("    msub w0, w3, w2, w1");
                    self.state.emit("    sxtw x0, w0");
                }
                IrBinOp::URem => {
                    self.state.emit("    udiv w3, w1, w2");
                    self.state.emit("    msub w0, w3, w2, w1");
                }
                IrBinOp::And => self.state.emit("    and w0, w1, w2"),
                IrBinOp::Or => self.state.emit("    orr w0, w1, w2"),
                IrBinOp::Xor => self.state.emit("    eor w0, w1, w2"),
                IrBinOp::Shl => self.state.emit("    lsl w0, w1, w2"),
                IrBinOp::AShr => self.state.emit("    asr w0, w1, w2"),
                IrBinOp::LShr => self.state.emit("    lsr w0, w1, w2"),
            }
        } else {
            match op {
                IrBinOp::Add => self.state.emit("    add x0, x1, x2"),
                IrBinOp::Sub => self.state.emit("    sub x0, x1, x2"),
                IrBinOp::Mul => self.state.emit("    mul x0, x1, x2"),
                IrBinOp::SDiv => self.state.emit("    sdiv x0, x1, x2"),
                IrBinOp::UDiv => self.state.emit("    udiv x0, x1, x2"),
                IrBinOp::SRem => {
                    self.state.emit("    sdiv x3, x1, x2");
                    self.state.emit("    msub x0, x3, x2, x1");
                }
                IrBinOp::URem => {
                    self.state.emit("    udiv x3, x1, x2");
                    self.state.emit("    msub x0, x3, x2, x1");
                }
                IrBinOp::And => self.state.emit("    and x0, x1, x2"),
                IrBinOp::Or => self.state.emit("    orr x0, x1, x2"),
                IrBinOp::Xor => self.state.emit("    eor x0, x1, x2"),
                IrBinOp::Shl => self.state.emit("    lsl x0, x1, x2"),
                IrBinOp::AShr => self.state.emit("    asr x0, x1, x2"),
                IrBinOp::LShr => self.state.emit("    lsr x0, x1, x2"),
            }
        }

        self.store_x0_to(dest);
    }

    fn emit_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if is_i128_type(ty) {
            self.emit_i128_cmp(dest, op, lhs, rhs);
            return;
        }
        if ty.is_float() {
            self.operand_to_x0(lhs);
            self.state.emit("    mov x1, x0");
            self.operand_to_x0(rhs);
            if ty == IrType::F32 {
                // F32: bit pattern in low 32 bits, use s-registers
                self.state.emit("    fmov s0, w1");
                self.state.emit("    fmov s1, w0");
                self.state.emit("    fcmp s0, s1");
            } else {
                // F64: full 64-bit bit pattern, use d-registers
                self.state.emit("    fmov d0, x1");
                self.state.emit("    fmov d1, x0");
                self.state.emit("    fcmp d0, d1");
            }
            let cond = match op {
                IrCmpOp::Eq => "eq",
                IrCmpOp::Ne => "ne",
                IrCmpOp::Slt | IrCmpOp::Ult => "mi",
                IrCmpOp::Sle | IrCmpOp::Ule => "ls",
                IrCmpOp::Sgt | IrCmpOp::Ugt => "gt",
                IrCmpOp::Sge | IrCmpOp::Uge => "ge",
            };
            self.state.emit(&format!("    cset x0, {}", cond));
            self.store_x0_to(dest);
            return;
        }

        self.operand_to_x0(lhs);
        self.state.emit("    mov x1, x0");
        self.operand_to_x0(rhs);
        let use_32bit = ty == IrType::I32 || ty == IrType::U32
            || ty == IrType::I8 || ty == IrType::U8
            || ty == IrType::I16 || ty == IrType::U16;
        if use_32bit {
            self.state.emit("    cmp w1, w0");
        } else {
            self.state.emit("    cmp x1, x0");
        }

        let cond = match op {
            IrCmpOp::Eq => "eq",
            IrCmpOp::Ne => "ne",
            IrCmpOp::Slt => "lt",
            IrCmpOp::Sle => "le",
            IrCmpOp::Sgt => "gt",
            IrCmpOp::Sge => "ge",
            IrCmpOp::Ult => "lo",
            IrCmpOp::Ule => "ls",
            IrCmpOp::Ugt => "hi",
            IrCmpOp::Uge => "hs",
        };
        self.state.emit(&format!("    cset x0, {}", cond));
        self.store_x0_to(dest);
    }

    fn emit_call(&mut self, args: &[Operand], arg_types: &[IrType], direct_name: Option<&str>,
                 func_ptr: Option<&Operand>, dest: Option<Value>, return_type: IrType,
                 is_variadic: bool, num_fixed_args: usize) {
        // For indirect calls, spill the function pointer to a dedicated stack slot.
        // We cannot use x17 to hold it across argument setup because x17 is used as
        // a scratch register by emit_load_from_sp/emit_store_to_sp/emit_add_sp_offset
        // for large stack offsets (> 4095). Instead, push the function pointer onto
        // the stack and reload it right before the blr instruction.
        let indirect_call = func_ptr.is_some() && direct_name.is_none();
        if indirect_call {
            let ptr = func_ptr.unwrap();
            self.operand_to_x0(ptr);
            // Spill function pointer to stack (pre-decrement SP by 16 for alignment)
            self.state.emit("    str x0, [sp, #-16]!");
        }

        // Classify args: determine which go in registers vs stack.
        // On AArch64 (AAPCS64), ALL float args (both named and variadic) go in FP registers
        // (d0-d7 / q0-q7), and all int args go in GP registers (x0-x7). Unlike x86-64 and
        // RISC-V where variadic float args must go through GP registers, AAPCS64 passes them
        // in FP registers. The callee saves both GP and FP register sets in variadic functions
        // and va_arg uses __gr_offs/__vr_offs to locate args in the correct save area.
        // F128 (long double) uses Q registers (128-bit) and consumes one FP register slot.
        // Arg classes: 'f' = float reg, 'i' = int reg, 's' = stack, 'q' = F128 in Q reg,
        // 'S' = stack quad (overflow), 'p' = i128 in GP register pair, 'P' = i128 stack
        let mut arg_classes: Vec<char> = Vec::new();
        let mut fi = 0usize;
        let mut ii = 0usize;
        let _ = is_variadic; // AAPCS64: variadic status doesn't affect register assignment
        let _ = num_fixed_args; // AAPCS64: all args use same classification regardless
        for (i, _arg) in args.iter().enumerate() {
            let arg_ty = if i < arg_types.len() { Some(arg_types[i]) } else { None };
            let is_long_double = arg_ty == Some(IrType::F128);
            let is_i128 = arg_ty.map(|t| is_i128_type(t)).unwrap_or(false);
            let is_float = if let Some(ty) = arg_ty {
                ty.is_float()
            } else {
                matches!(args[i], Operand::Const(IrConst::F32(_) | IrConst::F64(_)))
            };
            if is_i128 {
                // AAPCS64: 128-bit integers go in even-aligned GP register pair
                if ii % 2 != 0 { ii += 1; } // align to even register
                if ii + 1 < 8 {
                    arg_classes.push('p'); // GP register pair
                    ii += 2;
                } else {
                    arg_classes.push('P'); // stack (16 bytes)
                    ii = 8;
                }
            } else if is_long_double {
                if fi < 8 {
                    arg_classes.push('q');
                    fi += 1;
                } else {
                    arg_classes.push('S');
                }
            } else if is_float && fi < 8 {
                arg_classes.push('f');
                fi += 1;
            } else if !is_float && ii < 8 {
                arg_classes.push('i');
                ii += 1;
            } else {
                arg_classes.push('s');
            }
        }

        // Count stack arguments - 'S'/'P' need 16 bytes, 's' args need 8 bytes
        let stack_arg_indices: Vec<usize> = args.iter().enumerate()
            .filter(|(i, _)| arg_classes[*i] == 's' || arg_classes[*i] == 'S' || arg_classes[*i] == 'P')
            .map(|(i, _)| i)
            .collect();
        let mut stack_arg_space: usize = 0;
        for &idx in &stack_arg_indices {
            if arg_classes[idx] == 'S' || arg_classes[idx] == 'P' {
                // Align to 16 bytes for quad/i128
                stack_arg_space = (stack_arg_space + 15) & !15;
                stack_arg_space += 16;
            } else {
                stack_arg_space += 8;
            }
        }
        stack_arg_space = (stack_arg_space + 15) & !15; // Final 16-byte alignment

        // For indirect calls, the function pointer was spilled to [sp] with a 16-byte
        // pre-decrement. All stack slot references must account for this extra offset.
        let fptr_spill: i64 = if indirect_call { 16 } else { 0 };

        // Phase 1: Handle stack args FIRST (before GP temp regs are populated).
        // Stack arg loading uses x17 as scratch for large offsets (not x16,
        // since x16 is the last GP temp register for call arguments).
        if stack_arg_space > 0 {
            // Pre-decrement SP and store stack args
            self.emit_sub_sp(stack_arg_space as i64);
            // Now we need to load operands with adjusted SP offsets
            let mut stack_offset = 0i64;
            for &arg_idx in &stack_arg_indices {
                let is_stack_quad = arg_classes[arg_idx] == 'S';
                let is_stack_i128 = arg_classes[arg_idx] == 'P';
                if is_stack_quad || is_stack_i128 {
                    // Align stack_offset to 16 bytes for quad-precision or i128
                    stack_offset = (stack_offset + 15) & !15;
                }
                if is_stack_i128 {
                    // 128-bit integer stack arg
                    // Load 128-bit value, adjusting for SP movement
                    match &args[arg_idx] {
                        Operand::Const(c) => {
                            if let IrConst::I128(v) = c {
                                let low = *v as u64;
                                let high = (*v >> 64) as u64;
                                self.emit_load_imm64("x0", low as i64);
                                self.emit_store_to_sp("x0", stack_offset, "str");
                                self.emit_load_imm64("x0", high as i64);
                                self.emit_store_to_sp("x0", stack_offset + 8, "str");
                            } else {
                                self.operand_to_x0(&args[arg_idx]);
                                self.emit_store_to_sp("x0", stack_offset, "str");
                                self.state.emit("    mov x0, #0");
                                self.emit_store_to_sp("x0", stack_offset + 8, "str");
                            }
                        }
                        Operand::Value(v) => {
                            if let Some(slot) = self.state.get_slot(v.0) {
                                let adjusted = slot.0 + stack_arg_space as i64 + fptr_spill;
                                if self.state.is_alloca(v.0) {
                                    self.emit_add_sp_offset("x0", adjusted);
                                    self.emit_store_to_sp("x0", stack_offset, "str");
                                    self.state.emit("    mov x0, #0");
                                    self.emit_store_to_sp("x0", stack_offset + 8, "str");
                                } else {
                                    // 128-bit value: load both halves
                                    self.emit_load_from_sp("x0", adjusted, "ldr");
                                    self.emit_store_to_sp("x0", stack_offset, "str");
                                    self.emit_load_from_sp("x0", adjusted + 8, "ldr");
                                    self.emit_store_to_sp("x0", stack_offset + 8, "str");
                                }
                            } else {
                                self.state.emit("    mov x0, #0");
                                self.emit_store_to_sp("x0", stack_offset, "str");
                                self.emit_store_to_sp("x0", stack_offset + 8, "str");
                            }
                        }
                    }
                    stack_offset += 16;
                    continue;
                }
                // For constants, no SP adjustment needed
                match &args[arg_idx] {
                    Operand::Const(c) => {
                        if is_stack_quad {
                            // F128 overflow to stack: emit the quad-precision constant directly
                            let f64_val = match c {
                                IrConst::LongDouble(v) => *v,
                                IrConst::F64(v) => *v,
                                _ => c.to_f64().unwrap_or(0.0),
                            };
                            let bytes = crate::ir::ir::f64_to_f128_bytes(f64_val);
                            let lo = u64::from_le_bytes(bytes[0..8].try_into().unwrap());
                            let hi = u64::from_le_bytes(bytes[8..16].try_into().unwrap());
                            self.emit_load_imm64("x0", lo as i64);
                            self.emit_store_to_sp("x0", stack_offset, "str");
                            self.emit_load_imm64("x0", hi as i64);
                            self.emit_store_to_sp("x0", stack_offset + 8, "str");
                        } else {
                            self.operand_to_x0(&args[arg_idx]);
                            self.emit_store_to_sp("x0", stack_offset, "str");
                        }
                    }
                    Operand::Value(v) => {
                        // SP moved by stack_arg_space, adjust slot offset
                        if let Some(slot) = self.state.get_slot(v.0) {
                            let adjusted = slot.0 + stack_arg_space as i64 + fptr_spill;
                            if self.state.is_alloca(v.0) {
                                self.emit_add_sp_offset("x0", adjusted);
                            } else {
                                self.emit_load_from_sp("x0", adjusted, "ldr");
                            }
                        } else {
                            self.state.emit("    mov x0, #0");
                        }
                        if is_stack_quad {
                            // F128 overflow: convert f64 to f128 via __extenddftf2 (returns in q0)
                            self.state.emit("    fmov d0, x0");
                            self.state.emit("    stp x9, x10, [sp, #-16]!");
                            self.state.emit("    bl __extenddftf2");
                            self.state.emit("    ldp x9, x10, [sp], #16");
                            // Store q0 (f128 result) to stack arg location
                            self.state.emit(&format!("    str q0, [sp, #{}]", stack_offset));
                        } else {
                            self.emit_store_to_sp("x0", stack_offset, "str");
                        }
                    }
                }
                stack_offset += if is_stack_quad { 16 } else { 8 };
            }
        }

        // Total SP adjustment: stack args + function pointer spill (if indirect call)
        let total_sp_adjust = stack_arg_space as i64 + fptr_spill;

        // Phase 2a: Load GP register args into temp registers (x9-x16).
        // operand_to_x0 may use x17 as scratch for large SP offsets (via
        // emit_load_from_sp), which is safe since x17 is not in ARM_TMP_REGS.
        let mut gp_tmp_idx = 0usize;
        for (i, arg) in args.iter().enumerate() {
            if arg_classes[i] != 'i' { continue; }
            if gp_tmp_idx >= 8 { break; }
            // If SP was adjusted (stack args or fptr spill), adjust alloca/value offsets
            if total_sp_adjust > 0 {
                match arg {
                    Operand::Value(v) => {
                        if let Some(slot) = self.state.get_slot(v.0) {
                            let adjusted = slot.0 + total_sp_adjust;
                            if self.state.is_alloca(v.0) {
                                self.emit_add_sp_offset("x0", adjusted);
                            } else {
                                self.emit_load_from_sp("x0", adjusted, "ldr");
                            }
                        } else {
                            self.state.emit("    mov x0, #0");
                        }
                    }
                    Operand::Const(_) => {
                        self.operand_to_x0(arg);
                    }
                }
            } else {
                self.operand_to_x0(arg);
            }
            self.state.emit(&format!("    mov {}, x0", ARM_TMP_REGS[gp_tmp_idx]));
            gp_tmp_idx += 1;
        }

        // Phase 2b: Load FP register args into d0-d7 (or s0-s7, q0-q7 for F128).
        // FP regs don't conflict with GP temp regs, so this is safe.
        //
        // IMPORTANT: __extenddftf2 (used for F128 variable args) clobbers all
        // caller-saved FP registers (q0-q7). So we must:
        // 1. First convert all F128 variable args and save results to temp stack slots
        // 2. Then load F128 constants directly into their target Q registers
        // 3. Then load the saved F128 results into their target Q registers
        // 4. Finally load non-F128 float/double args (which won't be clobbered)
        let mut fp_reg_idx = 0usize;

        // First pass: assign FP register indices to all 'f' and 'q' args
        let mut fp_reg_assignments: Vec<(usize, usize)> = Vec::new(); // (arg_index, fp_reg_index)
        for (i, _arg) in args.iter().enumerate() {
            if arg_classes[i] != 'f' && arg_classes[i] != 'q' { continue; }
            if fp_reg_idx >= 8 { break; }
            fp_reg_assignments.push((i, fp_reg_idx));
            fp_reg_idx += 1;
        }

        // Count F128 variable args that need __extenddftf2
        let f128_var_count: usize = fp_reg_assignments.iter()
            .filter(|&&(arg_i, _)| arg_classes[arg_i] == 'q' && matches!(&args[arg_i], Operand::Value(_)))
            .count();

        // If we have F128 variable args, allocate temp stack space for their results.
        // Each F128 value is 16 bytes. We save to temp stack because __extenddftf2
        // clobbers ALL caller-saved FP registers (q0-q7).
        let f128_temp_space = f128_var_count * 16;
        let f128_temp_space_aligned = (f128_temp_space + 15) & !15;
        if f128_temp_space_aligned > 0 {
            self.emit_sub_sp(f128_temp_space_aligned as i64);
        }

        // Second pass: Convert F128 variable args via __extenddftf2 and save to temp stack
        let mut f128_temp_idx = 0usize;
        let mut f128_temp_slots: Vec<(usize, usize)> = Vec::new(); // (fp_reg_index, temp_slot_offset)
        for &(arg_i, reg_i) in &fp_reg_assignments {
            if arg_classes[arg_i] != 'q' { continue; }
            if let Operand::Value(v) = &args[arg_i] {
                if total_sp_adjust > 0 || f128_temp_space_aligned > 0 {
                    if let Some(slot) = self.state.get_slot(v.0) {
                        let adjusted = slot.0 + total_sp_adjust + f128_temp_space_aligned as i64;
                        self.emit_load_from_sp("x0", adjusted, "ldr");
                    } else {
                        self.state.emit("    mov x0, #0");
                    }
                } else {
                    self.operand_to_x0(&args[arg_i]);
                }
                self.state.emit("    fmov d0, x0");
                self.state.emit("    stp x9, x10, [sp, #-16]!");
                self.state.emit("    bl __extenddftf2");
                self.state.emit("    ldp x9, x10, [sp], #16");
                // Save the f128 result (in q0) to temp stack slot
                let temp_off = f128_temp_idx * 16;
                self.state.emit(&format!("    str q0, [sp, #{}]", temp_off));
                f128_temp_slots.push((reg_i, temp_off));
                f128_temp_idx += 1;
            }
        }

        // Third pass: Load F128 constants directly into target Q registers
        for &(arg_i, reg_i) in &fp_reg_assignments {
            if arg_classes[arg_i] != 'q' { continue; }
            if let Operand::Const(c) = &args[arg_i] {
                let f64_val = match c {
                    IrConst::LongDouble(v) => *v,
                    IrConst::F64(v) => *v,
                    _ => c.to_f64().unwrap_or(0.0),
                };
                let bytes = crate::ir::ir::f64_to_f128_bytes(f64_val);
                let lo = u64::from_le_bytes(bytes[0..8].try_into().unwrap());
                let hi = u64::from_le_bytes(bytes[8..16].try_into().unwrap());
                self.emit_load_imm64("x0", lo as i64);
                self.emit_load_imm64("x1", hi as i64);
                self.state.emit("    stp x0, x1, [sp, #-16]!");
                self.state.emit(&format!("    ldr q{}, [sp]", reg_i));
                self.state.emit("    add sp, sp, #16");
            }
        }

        // Fourth pass: Load saved F128 variable results from temp stack into target Q regs
        for &(reg_i, temp_off) in &f128_temp_slots {
            self.state.emit(&format!("    ldr q{}, [sp, #{}]", reg_i, temp_off));
        }

        // Deallocate F128 temp space
        if f128_temp_space_aligned > 0 {
            self.emit_add_sp(f128_temp_space_aligned as i64);
        }

        // Fifth pass: Load non-F128 FP args (float/double) into their target s/d registers.
        // This is done LAST because __extenddftf2 clobbers all caller-saved FP regs (q0-q7).
        for &(arg_i, reg_i) in &fp_reg_assignments {
            if arg_classes[arg_i] == 'q' { continue; }
            let arg_ty = if arg_i < arg_types.len() { Some(arg_types[arg_i]) } else { None };
            if total_sp_adjust > 0 {
                match &args[arg_i] {
                    Operand::Value(v) => {
                        if let Some(slot) = self.state.get_slot(v.0) {
                            let adjusted = slot.0 + total_sp_adjust;
                            self.emit_load_from_sp("x0", adjusted, "ldr");
                        } else {
                            self.state.emit("    mov x0, #0");
                        }
                    }
                    Operand::Const(_) => {
                        self.operand_to_x0(&args[arg_i]);
                    }
                }
            } else {
                self.operand_to_x0(&args[arg_i]);
            }
            if arg_ty == Some(IrType::F32) {
                self.state.emit(&format!("    fmov s{}, w0", reg_i));
            } else {
                self.state.emit(&format!("    fmov d{}, x0", reg_i));
            }
        }

        // Phase 3: Move GP args from temp regs to actual arg registers (x0-x7).
        // Track which GP registers are assigned for 'i' and 'p' classes.
        let mut int_reg_idx = 0usize;
        gp_tmp_idx = 0;
        for (i, _arg) in args.iter().enumerate() {
            if arg_classes[i] == 'p' {
                // i128 pair: skip 2 registers (handled below)
                if int_reg_idx % 2 != 0 { int_reg_idx += 1; }
                int_reg_idx += 2;
                continue;
            }
            if arg_classes[i] != 'i' { continue; }
            if gp_tmp_idx >= 8 { break; }
            if int_reg_idx < 8 {
                self.state.emit(&format!("    mov {}, {}", ARM_ARG_REGS[int_reg_idx], ARM_TMP_REGS[gp_tmp_idx]));
                int_reg_idx += 1;
            }
            gp_tmp_idx += 1;
        }

        // Phase 3b: Load i128 register pair args directly into their target registers.
        // Must be done last since it writes to x0-x7 directly.
        {
            let mut pair_reg_idx = 0usize;
            for (i, _arg) in args.iter().enumerate() {
                if arg_classes[i] == 'p' {
                    if pair_reg_idx % 2 != 0 { pair_reg_idx += 1; }
                    // Load 128-bit value, adjusting for SP if stack args or fptr spill present
                    if total_sp_adjust > 0 {
                        match &args[i] {
                            Operand::Value(v) => {
                                if let Some(slot) = self.state.get_slot(v.0) {
                                    let adjusted = slot.0 + total_sp_adjust;
                                    if self.state.is_alloca(v.0) {
                                        // Alloca pointer, not 128-bit value
                                        self.emit_load_from_sp(ARM_ARG_REGS[pair_reg_idx], adjusted, "ldr");
                                        self.state.emit(&format!("    mov {}, #0", ARM_ARG_REGS[pair_reg_idx + 1]));
                                    } else {
                                        self.emit_load_from_sp(ARM_ARG_REGS[pair_reg_idx], adjusted, "ldr");
                                        self.emit_load_from_sp(ARM_ARG_REGS[pair_reg_idx + 1], adjusted + 8, "ldr");
                                    }
                                }
                            }
                            Operand::Const(c) => {
                                if let IrConst::I128(v) = c {
                                    self.emit_load_imm64(ARM_ARG_REGS[pair_reg_idx], *v as u64 as i64);
                                    self.emit_load_imm64(ARM_ARG_REGS[pair_reg_idx + 1], (*v >> 64) as u64 as i64);
                                } else {
                                    self.operand_to_x0(&args[i]);
                                    if pair_reg_idx != 0 {
                                        self.state.emit(&format!("    mov {}, x0", ARM_ARG_REGS[pair_reg_idx]));
                                    }
                                    self.state.emit(&format!("    mov {}, #0", ARM_ARG_REGS[pair_reg_idx + 1]));
                                }
                            }
                        }
                    } else {
                        match &args[i] {
                            Operand::Value(v) => {
                                if let Some(slot) = self.state.get_slot(v.0) {
                                    if self.state.is_alloca(v.0) {
                                        self.emit_add_sp_offset(ARM_ARG_REGS[pair_reg_idx], slot.0);
                                        self.state.emit(&format!("    mov {}, #0", ARM_ARG_REGS[pair_reg_idx + 1]));
                                    } else {
                                        self.emit_load_from_sp(ARM_ARG_REGS[pair_reg_idx], slot.0, "ldr");
                                        self.emit_load_from_sp(ARM_ARG_REGS[pair_reg_idx + 1], slot.0 + 8, "ldr");
                                    }
                                }
                            }
                            Operand::Const(c) => {
                                if let IrConst::I128(v) = c {
                                    self.emit_load_imm64(ARM_ARG_REGS[pair_reg_idx], *v as u64 as i64);
                                    self.emit_load_imm64(ARM_ARG_REGS[pair_reg_idx + 1], (*v >> 64) as u64 as i64);
                                } else {
                                    self.operand_to_x0(&args[i]);
                                    if pair_reg_idx != 0 {
                                        self.state.emit(&format!("    mov {}, x0", ARM_ARG_REGS[pair_reg_idx]));
                                    }
                                    self.state.emit(&format!("    mov {}, #0", ARM_ARG_REGS[pair_reg_idx + 1]));
                                }
                            }
                        }
                    }
                    pair_reg_idx += 2;
                } else if arg_classes[i] == 'i' {
                    pair_reg_idx += 1;
                }
            }
        }
        // FP args already in d0-d7/q0-q7 from Phase 2b.

        if let Some(name) = direct_name {
            self.state.emit(&format!("    bl {}", name));
        } else if indirect_call {
            // Reload function pointer from spill slot. The spill slot is at
            // [sp + stack_arg_space] because stack args were pushed after the spill.
            let spill_offset = stack_arg_space as i64;
            self.emit_load_from_sp("x17", spill_offset, "ldr");
            self.state.emit("    blr x17");
        }

        // Clean up stack args + function pointer spill slot
        let total_call_stack = stack_arg_space as i64 + fptr_spill;
        if total_call_stack > 0 {
            self.emit_add_sp(total_call_stack);
        }

        if let Some(dest) = dest {
            if is_i128_type(return_type) {
                // 128-bit return: x0 = low, x1 = high per AAPCS64
                self.store_x0_x1_to(&dest);
            } else if return_type.is_long_double() {
                // F128 return value is in q0 per AAPCS64.
                self.state.emit("    bl __trunctfdf2");
                self.state.emit("    fmov x0, d0");
                self.store_x0_to(&dest);
            } else if return_type == IrType::F32 {
                self.state.emit("    fmov w0, s0");
                self.store_x0_to(&dest);
            } else if return_type.is_float() {
                self.state.emit("    fmov x0, d0");
                self.store_x0_to(&dest);
            } else {
                self.store_x0_to(&dest);
            }
        }
    }

    fn emit_global_addr(&mut self, dest: &Value, name: &str) {
        self.state.emit(&format!("    adrp x0, {}", name));
        self.state.emit(&format!("    add x0, x0, :lo12:{}", name));
        self.store_x0_to(dest);
    }

    fn emit_cast_instrs(&mut self, from_ty: IrType, to_ty: IrType) {
        match classify_cast(from_ty, to_ty) {
            CastKind::Noop => {}

            CastKind::FloatToSigned { from_f64 } => {
                if from_f64 {
                    self.state.emit("    fmov d0, x0");
                    self.state.emit("    fcvtzs x0, d0");
                } else {
                    self.state.emit("    fmov s0, w0");
                    self.state.emit("    fcvtzs x0, s0");
                }
            }

            CastKind::FloatToUnsigned { from_f64, .. } => {
                if from_f64 {
                    self.state.emit("    fmov d0, x0");
                    self.state.emit("    fcvtzu x0, d0");
                } else {
                    self.state.emit("    fmov s0, w0");
                    self.state.emit("    fcvtzu x0, s0");
                }
            }

            CastKind::SignedToFloat { to_f64 } => {
                if to_f64 {
                    self.state.emit("    scvtf d0, x0");
                    self.state.emit("    fmov x0, d0");
                } else {
                    self.state.emit("    scvtf s0, x0");
                    self.state.emit("    fmov w0, s0");
                }
            }

            CastKind::UnsignedToFloat { to_f64, .. } => {
                if to_f64 {
                    self.state.emit("    ucvtf d0, x0");
                    self.state.emit("    fmov x0, d0");
                } else {
                    self.state.emit("    ucvtf s0, x0");
                    self.state.emit("    fmov w0, s0");
                }
            }

            CastKind::FloatToFloat { widen } => {
                if widen {
                    self.state.emit("    fmov s0, w0");
                    self.state.emit("    fcvt d0, s0");
                    self.state.emit("    fmov x0, d0");
                } else {
                    self.state.emit("    fmov d0, x0");
                    self.state.emit("    fcvt s0, d0");
                    self.state.emit("    fmov w0, s0");
                }
            }

            CastKind::SignedToUnsignedSameSize { to_ty } => {
                match to_ty {
                    IrType::U8 => self.state.emit("    and x0, x0, #0xff"),
                    IrType::U16 => self.state.emit("    and x0, x0, #0xffff"),
                    IrType::U32 => self.state.emit("    mov w0, w0"),
                    _ => {}
                }
            }

            CastKind::IntWiden { from_ty, .. } => {
                if from_ty.is_unsigned() {
                    match from_ty {
                        IrType::U8 => self.state.emit("    and x0, x0, #0xff"),
                        IrType::U16 => self.state.emit("    and x0, x0, #0xffff"),
                        IrType::U32 => self.state.emit("    mov w0, w0"),
                        _ => {}
                    }
                } else {
                    match from_ty {
                        IrType::I8 => self.state.emit("    sxtb x0, w0"),
                        IrType::I16 => self.state.emit("    sxth x0, w0"),
                        IrType::I32 => self.state.emit("    sxtw x0, w0"),
                        _ => {}
                    }
                }
            }

            CastKind::IntNarrow { to_ty } => {
                match to_ty {
                    IrType::I8 => self.state.emit("    sxtb x0, w0"),
                    IrType::U8 => self.state.emit("    and x0, x0, #0xff"),
                    IrType::I16 => self.state.emit("    sxth x0, w0"),
                    IrType::U16 => self.state.emit("    and x0, x0, #0xffff"),
                    IrType::I32 => self.state.emit("    sxtw x0, w0"),
                    IrType::U32 => self.state.emit("    mov w0, w0"),
                    _ => {}
                }
            }
        }
    }

    /// Override emit_cast to handle 128-bit widening/narrowing.
    fn emit_cast(&mut self, dest: &Value, src: &Operand, from_ty: IrType, to_ty: IrType) {
        if is_i128_type(to_ty) && !is_i128_type(from_ty) {
            // Widening to 128-bit
            self.operand_to_x0(src);
            // First widen to 64-bit if needed
            if from_ty.size() < 8 {
                self.emit_cast_instrs(from_ty, if from_ty.is_signed() { IrType::I64 } else { IrType::U64 });
            }
            if from_ty.is_signed() {
                // Sign-extend: x1 = x0 >> 63 (arithmetic)
                self.state.emit("    asr x1, x0, #63");
            } else {
                // Zero-extend
                self.state.emit("    mov x1, #0");
            }
            self.store_x0_x1_to(dest);
            return;
        }
        if is_i128_type(from_ty) && !is_i128_type(to_ty) {
            // Narrowing from 128-bit: use low 64 bits
            self.operand_to_x0_x1(src);
            // x0 already has the low 64 bits
            if to_ty.size() < 8 {
                self.emit_cast_instrs(IrType::I64, to_ty);
            }
            self.store_x0_to(dest);
            return;
        }
        if is_i128_type(from_ty) && is_i128_type(to_ty) {
            // I128 <-> U128: same representation, just copy
            self.operand_to_x0_x1(src);
            self.store_x0_x1_to(dest);
            return;
        }
        // Default path for non-128-bit casts
        self.emit_load_operand(src);
        self.emit_cast_instrs(from_ty, to_ty);
        self.emit_store_result(dest);
    }

    fn emit_va_arg(&mut self, dest: &Value, va_list_ptr: &Value, result_ty: IrType) {
        // AArch64 AAPCS64 va_arg implementation.
        // va_list layout:
        //   [x1+0]  __stack    (void*)  - next stack overflow arg
        //   [x1+8]  __gr_top   (void*)  - end of GP save area
        //   [x1+16] __vr_top   (void*)  - end of FP/SIMD save area
        //   [x1+24] __gr_offs  (int32)  - offset from __gr_top (negative means regs available)
        //   [x1+28] __vr_offs  (int32)  - offset from __vr_top (negative means regs available)

        let is_fp = result_ty.is_float();
        let is_f128 = result_ty.is_long_double();

        // x1 = pointer to va_list struct
        if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
            if self.state.is_alloca(va_list_ptr.0) {
                self.emit_add_fp_offset("x1", slot.0);
            } else {
                self.emit_load_from_sp("x1", slot.0, "ldr");
            }
        }

        if is_f128 {
            // F128 (long double) on AArch64: passed in Q registers, accessed via VR save area.
            // Same path as F32/F64 but reads 16 bytes (one Q register slot).
            let label_id = self.state.next_label_id();
            let label_stack = format!(".Lva_stack_{}", label_id);
            let label_done = format!(".Lva_done_{}", label_id);

            // Check __vr_offs (offset 28 in va_list)
            self.state.emit("    ldrsw x2, [x1, #28]");  // x2 = __vr_offs (sign-extended)
            self.state.emit(&format!("    tbz x2, #63, {}", label_stack)); // if >= 0, go to stack
            // Register save area path: addr = __vr_top + __vr_offs
            self.state.emit("    ldr x3, [x1, #16]");     // x3 = __vr_top
            self.state.emit("    add x3, x3, x2");         // x3 = __vr_top + __vr_offs
            // Advance __vr_offs by 16 (one Q register slot)
            self.state.emit("    add w2, w2, #16");
            self.state.emit("    str w2, [x1, #28]");
            // Load the f128 value from save area and convert to f64
            // Save x1 (va_list ptr) since __trunctfdf2 may clobber it
            self.state.emit("    mov x4, x1");
            self.state.emit("    ldr q0, [x3]");           // load f128 into q0
            self.state.emit("    bl __trunctfdf2");         // q0 -> d0
            self.state.emit("    fmov x0, d0");
            self.state.emit(&format!("    b {}", label_done));

            // Stack overflow path (when all 8 VR slots exhausted)
            self.state.emit(&format!("{}:", label_stack));
            self.state.emit("    ldr x3, [x1]");           // x3 = __stack
            // Align x3 to 16 bytes
            self.state.emit("    add x3, x3, #15");
            self.state.emit("    and x3, x3, #-16");
            // Save va_list ptr
            self.state.emit("    mov x4, x1");
            self.state.emit("    ldr q0, [x3]");           // load f128 into q0
            // Advance __stack by 16
            self.state.emit("    add x3, x3, #16");
            self.state.emit("    str x3, [x4]");           // store new __stack
            self.state.emit("    bl __trunctfdf2");         // q0 -> d0
            self.state.emit("    fmov x0, d0");

            self.state.emit(&format!("{}:", label_done));
        } else if is_fp {
            let label_id = self.state.next_label_id();
            let label_stack = format!(".Lva_stack_{}", label_id);
            let label_done = format!(".Lva_done_{}", label_id);

            // FP type: check __vr_offs
            self.state.emit("    ldrsw x2, [x1, #28]");  // x2 = __vr_offs (sign-extended)
            self.state.emit(&format!("    tbz x2, #63, {}", label_stack)); // if >= 0, go to stack
            // Register save area path: addr = __vr_top + __vr_offs
            self.state.emit("    ldr x3, [x1, #16]");     // x3 = __vr_top
            self.state.emit("    add x3, x3, x2");         // x3 = __vr_top + __vr_offs
            // Advance __vr_offs by 16
            self.state.emit("    add w2, w2, #16");
            self.state.emit("    str w2, [x1, #28]");
            // Load the value from the register save area
            if result_ty == IrType::F32 {
                self.state.emit("    ldr w0, [x3]");
            } else {
                self.state.emit("    ldr x0, [x3]");
            }
            self.state.emit(&format!("    b {}", label_done));

            // Stack overflow path
            self.state.emit(&format!("{}:", label_stack));
            self.state.emit("    ldr x3, [x1]");
            if result_ty == IrType::F32 {
                self.state.emit("    ldr w0, [x3]");
            } else {
                self.state.emit("    ldr x0, [x3]");
            }
            self.state.emit("    add x3, x3, #8");
            self.state.emit("    str x3, [x1]");

            self.state.emit(&format!("{}:", label_done));
        } else {
            let label_id = self.state.next_label_id();
            let label_stack = format!(".Lva_stack_{}", label_id);
            let label_done = format!(".Lva_done_{}", label_id);

            // GP type: check __gr_offs
            self.state.emit("    ldrsw x2, [x1, #24]");  // x2 = __gr_offs (sign-extended)
            self.state.emit(&format!("    tbz x2, #63, {}", label_stack)); // if >= 0, go to stack
            // Register save area path: addr = __gr_top + __gr_offs
            self.state.emit("    ldr x3, [x1, #8]");      // x3 = __gr_top
            self.state.emit("    add x3, x3, x2");         // x3 = __gr_top + __gr_offs
            // Advance __gr_offs by 8
            self.state.emit("    add w2, w2, #8");
            self.state.emit("    str w2, [x1, #24]");
            // Load the value from the register save area
            self.state.emit("    ldr x0, [x3]");
            self.state.emit(&format!("    b {}", label_done));

            // Stack overflow path
            self.state.emit(&format!("{}:", label_stack));
            self.state.emit("    ldr x3, [x1]");
            self.state.emit("    ldr x0, [x3]");
            self.state.emit("    add x3, x3, #8");
            self.state.emit("    str x3, [x1]");

            self.state.emit(&format!("{}:", label_done));
        }

        // Store result to dest
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.emit_store_to_sp("x0", slot.0, "str");
        }
    }

    fn emit_va_start(&mut self, va_list_ptr: &Value) {
        // AArch64 AAPCS64 va_start: initialize the full va_list struct.
        // va_list layout (32 bytes):
        //   offset 0:  void *__stack     - next stack overflow arg
        //   offset 8:  void *__gr_top    - end of GP register save area
        //   offset 16: void *__vr_top    - end of FP/SIMD register save area
        //   offset 24: int __gr_offs     - offset from __gr_top to next GP reg arg (negative)
        //   offset 28: int __vr_offs     - offset from __vr_top to next FP reg arg (negative)

        // x0 = pointer to va_list struct
        if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
            if self.state.is_alloca(va_list_ptr.0) {
                self.emit_add_fp_offset("x0", slot.0);
            } else {
                self.emit_load_from_sp("x0", slot.0, "ldr");
            }
        }

        // __stack: pointer to the first variadic stack argument.
        // After prologue: sp = x29, [x29] = saved fp, [x29+8] = saved lr,
        // [x29+16..x29+frame_size-1] = local slots.
        // Caller's stack args are at x29 + frame_size (above our frame).
        // If there are named args on the stack (more than 8 GP params), we must
        // skip past them to point to the first variadic stack arg.
        let stack_offset = self.current_frame_size + (self.va_named_stack_gp_count as i64 * 8);
        if stack_offset <= 4095 {
            self.state.emit(&format!("    add x1, x29, #{}", stack_offset));
        } else {
            self.load_large_imm("x1", stack_offset);
            self.state.emit("    add x1, x29, x1");
        }
        self.state.emit("    str x1, [x0]");  // __stack at offset 0

        // __gr_top: pointer to the end (one past last) of the GP register save area
        let gr_top_offset = self.va_gp_save_offset + 64; // end of x0-x7 save area
        self.emit_add_sp_offset("x1", gr_top_offset);
        self.state.emit("    str x1, [x0, #8]");  // __gr_top at offset 8

        // __vr_top: pointer to the end (one past last) of the FP/SIMD register save area
        let vr_top_offset = self.va_fp_save_offset + 128; // end of q0-q7 save area
        self.emit_add_sp_offset("x1", vr_top_offset);
        self.state.emit("    str x1, [x0, #16]");  // __vr_top at offset 16

        // __gr_offs: negative offset from __gr_top to next unnamed GP reg
        // = -(8 - named_gp_count) * 8
        // If all 8 GP regs used by named params, __gr_offs = 0 (meaning overflow to stack)
        let gr_offs: i32 = -((8 - self.va_named_gp_count as i32) * 8);
        self.state.emit(&format!("    mov w1, #{}", gr_offs));
        self.state.emit("    str w1, [x0, #24]");  // __gr_offs at offset 24

        // __vr_offs: negative offset from __vr_top to next unnamed FP/SIMD reg
        // = -(8 - named_fp_count) * 16
        let vr_offs: i32 = -((8 - self.va_named_fp_count as i32) * 16);
        self.state.emit(&format!("    mov w1, #{}", vr_offs));
        self.state.emit("    str w1, [x0, #28]");  // __vr_offs at offset 28
    }

    // emit_va_end: uses default no-op implementation

    fn emit_va_copy(&mut self, dest_ptr: &Value, src_ptr: &Value) {
        // Copy va_list struct (32 bytes on AArch64)
        if let Some(src_slot) = self.state.get_slot(src_ptr.0) {
            if self.state.is_alloca(src_ptr.0) {
                self.emit_add_fp_offset("x1", src_slot.0);
            } else {
                self.emit_load_from_sp("x1", src_slot.0, "ldr");
            }
        }
        if let Some(dest_slot) = self.state.get_slot(dest_ptr.0) {
            if self.state.is_alloca(dest_ptr.0) {
                self.emit_add_fp_offset("x0", dest_slot.0);
            } else {
                self.emit_load_from_sp("x0", dest_slot.0, "ldr");
            }
        }
        // Copy 32 bytes
        self.state.emit("    ldp x2, x3, [x1]");
        self.state.emit("    stp x2, x3, [x0]");
        self.state.emit("    ldp x2, x3, [x1, #16]");
        self.state.emit("    stp x2, x3, [x0, #16]");
    }

    fn emit_return(&mut self, val: Option<&Operand>, frame_size: i64) {
        if let Some(val) = val {
            if is_i128_type(self.current_return_type) {
                // 128-bit return: x0 = low, x1 = high per AAPCS64
                self.operand_to_x0_x1(val);
                self.emit_epilogue_arm(frame_size);
                self.state.emit("    ret");
                return;
            }
            self.operand_to_x0(val);
            if self.current_return_type.is_long_double() {
                // F128 return: convert f64 bit pattern in x0 to f128 in q0 via __extenddftf2.
                // __extenddftf2 takes input in d0, returns f128 in q0.
                self.state.emit("    fmov d0, x0");
                self.state.emit("    bl __extenddftf2");
                // Result now in q0, which is the AAPCS64 return register for f128
            } else if self.current_return_type == IrType::F32 {
                // F32 return: bit pattern in w0, move to s0 per AAPCS64
                self.state.emit("    fmov s0, w0");
            } else if self.current_return_type.is_float() {
                // F64 return: bit pattern in x0, move to d0 per AAPCS64
                self.state.emit("    fmov d0, x0");
            }
        }
        self.emit_epilogue_arm(frame_size);
        self.state.emit("    ret");
    }

    // emit_branch, emit_cond_branch, emit_unreachable, emit_indirect_branch:
    // use default implementations from ArchCodegen trait

    // emit_label_addr: uses default implementation (delegates to emit_global_addr)

    fn emit_get_return_f64_second(&mut self, dest: &Value) {
        // After a function call, the second F64 return value is in d1.
        // Store it to the dest stack slot, handling large offsets.
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.emit_store_to_sp("d1", slot.0, "str");
        }
    }

    fn emit_set_return_f64_second(&mut self, src: &Operand) {
        // Load src into d1 for the second return value.
        match src {
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    self.emit_load_from_sp("d1", slot.0, "ldr");
                }
            }
            Operand::Const(IrConst::F64(f)) => {
                let bits = f.to_bits();
                self.state.emit(&format!("    mov x0, #{}", bits));
                self.state.emit("    fmov d1, x0");
            }
            _ => {
                self.operand_to_x0(src);
                self.state.emit("    fmov d1, x0");
            }
        }
    }

    fn emit_get_return_f32_second(&mut self, dest: &Value) {
        // After a function call, the second F32 return value is in s1 (AAPCS64).
        // Store it to the dest stack slot.
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.emit_store_to_sp("s1", slot.0, "str");
        }
    }

    fn emit_set_return_f32_second(&mut self, src: &Operand) {
        // Load src into s1 for the second F32 return value (AAPCS64).
        match src {
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    self.emit_load_from_sp("s1", slot.0, "ldr");
                }
            }
            Operand::Const(IrConst::F32(f)) => {
                let bits = f.to_bits();
                self.state.emit(&format!("    mov w0, #{}", bits));
                self.state.emit("    fmov s1, w0");
            }
            _ => {
                self.operand_to_x0(src);
                self.state.emit("    fmov s1, w0");
            }
        }
    }

    fn emit_atomic_rmw(&mut self, dest: &Value, op: AtomicRmwOp, ptr: &Operand, val: &Operand, ty: IrType, _ordering: AtomicOrdering) {
        // Load ptr into x1, val into x2
        self.operand_to_x0(ptr);
        self.state.emit("    mov x1, x0"); // x1 = ptr
        self.operand_to_x0(val);
        self.state.emit("    mov x2, x0"); // x2 = val

        let (ldxr, stxr, reg_prefix) = Self::exclusive_instrs(ty);
        let val_reg = format!("{}2", reg_prefix);
        let old_reg = format!("{}0", reg_prefix);
        let tmp_reg = format!("{}3", reg_prefix);

        match op {
            AtomicRmwOp::Xchg => {
                // Simple exchange: old = *ptr; *ptr = val
                let label_id = self.state.next_label_id();
                let loop_label = format!(".Latomic_{}", label_id);
                self.state.emit(&format!("{}:", loop_label));
                self.state.emit(&format!("    {} {}, [x1]", ldxr, old_reg));
                self.state.emit(&format!("    {} w4, {}, [x1]", stxr, val_reg));
                self.state.emit(&format!("    cbnz w4, {}", loop_label));
            }
            AtomicRmwOp::TestAndSet => {
                let label_id = self.state.next_label_id();
                let loop_label = format!(".Latomic_{}", label_id);
                self.state.emit(&format!("{}:", loop_label));
                self.state.emit(&format!("    {} {}, [x1]", ldxr, old_reg));
                self.state.emit("    mov w3, #1");
                self.state.emit(&format!("    {} w4, w3, [x1]", stxr));
                self.state.emit(&format!("    cbnz w4, {}", loop_label));
            }
            _ => {
                // Generic RMW: old = *ptr; new = op(old, val); *ptr = new
                let label_id = self.state.next_label_id();
                let loop_label = format!(".Latomic_{}", label_id);
                self.state.emit(&format!("{}:", loop_label));
                self.state.emit(&format!("    {} {}, [x1]", ldxr, old_reg));
                // Compute x3 = op(x0, x2)
                Self::emit_atomic_op_arm(&mut self.state, op, &tmp_reg, &old_reg, &val_reg);
                self.state.emit(&format!("    {} w4, {}, [x1]", stxr, tmp_reg));
                self.state.emit(&format!("    cbnz w4, {}", loop_label));
            }
        }
        // Result is in x0 (old value)
        self.store_x0_to(dest);
    }

    fn emit_atomic_cmpxchg(&mut self, dest: &Value, ptr: &Operand, expected: &Operand, desired: &Operand, ty: IrType, _success_ordering: AtomicOrdering, _failure_ordering: AtomicOrdering, returns_bool: bool) {
        // x1 = ptr, x2 = expected, x3 = desired
        self.operand_to_x0(ptr);
        self.state.emit("    mov x1, x0");
        self.operand_to_x0(desired);
        self.state.emit("    mov x3, x0");
        self.operand_to_x0(expected);
        self.state.emit("    mov x2, x0");
        // x2 = expected

        let (ldxr, stxr, reg_prefix) = Self::exclusive_instrs(ty);
        let old_reg = format!("{}0", reg_prefix);
        let desired_reg = format!("{}3", reg_prefix);
        let expected_reg = format!("{}2", reg_prefix);

        let label_id = self.state.next_label_id();
        let loop_label = format!(".Lcas_loop_{}", label_id);
        let fail_label = format!(".Lcas_fail_{}", label_id);
        let done_label = format!(".Lcas_done_{}", label_id);

        self.state.emit(&format!("{}:", loop_label));
        self.state.emit(&format!("    {} {}, [x1]", ldxr, old_reg));
        self.state.emit(&format!("    cmp {}, {}", old_reg, expected_reg));
        self.state.emit(&format!("    b.ne {}", fail_label));
        self.state.emit(&format!("    {} w4, {}, [x1]", stxr, desired_reg));
        self.state.emit(&format!("    cbnz w4, {}", loop_label));
        if returns_bool {
            self.state.emit("    mov x0, #1");
        }
        // If !returns_bool, x0 already has old value (which equals expected on success)
        self.state.emit(&format!("    b {}", done_label));
        self.state.emit(&format!("{}:", fail_label));
        if returns_bool {
            self.state.emit("    mov x0, #0");
            // Clear exclusive monitor
            self.state.emit("    clrex");
        } else {
            // x0 has the old value (not equal to expected)
            self.state.emit("    clrex");
        }
        self.state.emit(&format!("{}:", done_label));
        self.store_x0_to(dest);
    }

    fn emit_atomic_load(&mut self, dest: &Value, ptr: &Operand, ty: IrType, _ordering: AtomicOrdering) {
        self.operand_to_x0(ptr);
        // Use ldar for acquire semantics (safe for all orderings)
        let instr = match ty {
            IrType::I8 | IrType::U8 => "ldarb",
            IrType::I16 | IrType::U16 => "ldarh",
            IrType::I32 | IrType::U32 => "ldar",
            _ => "ldar",
        };
        let dest_reg = match ty {
            IrType::I8 | IrType::U8 | IrType::I16 | IrType::U16 | IrType::I32 | IrType::U32 => "w0",
            _ => "x0",
        };
        self.state.emit(&format!("    {} {}, [x0]", instr, dest_reg));
        // Sign-extend if needed
        match ty {
            IrType::I8 => self.state.emit("    sxtb x0, w0"),
            IrType::I16 => self.state.emit("    sxth x0, w0"),
            IrType::I32 => self.state.emit("    sxtw x0, w0"),
            _ => {}
        }
        self.store_x0_to(dest);
    }

    fn emit_atomic_store(&mut self, ptr: &Operand, val: &Operand, ty: IrType, _ordering: AtomicOrdering) {
        self.operand_to_x0(val);
        self.state.emit("    mov x1, x0"); // x1 = val
        self.operand_to_x0(ptr);
        // Use stlr for release semantics
        let instr = match ty {
            IrType::I8 | IrType::U8 => "stlrb",
            IrType::I16 | IrType::U16 => "stlrh",
            IrType::I32 | IrType::U32 => "stlr",
            _ => "stlr",
        };
        let val_reg = match ty {
            IrType::I8 | IrType::U8 | IrType::I16 | IrType::U16 | IrType::I32 | IrType::U32 => "w1",
            _ => "x1",
        };
        self.state.emit(&format!("    {} {}, [x0]", instr, val_reg));
    }

    fn emit_fence(&mut self, _ordering: AtomicOrdering) {
        self.state.emit("    dmb ish");
    }

    fn emit_inline_asm(&mut self, template: &str, outputs: &[(String, Value, Option<String>)], inputs: &[(String, Operand, Option<String>)], _clobbers: &[String], operand_types: &[IrType]) {
        emit_inline_asm_common(self, template, outputs, inputs, operand_types);
    }

    fn emit_copy_i128(&mut self, dest: &Value, src: &Operand) {
        // Full 128-bit copy: load src into x0:x1, store to dest
        self.operand_to_x0_x1(src);
        self.store_x0_x1_to(dest);
    }
}

impl Default for ArmCodegen {
    fn default() -> Self {
        Self::new()
    }
}

/// AArch64 scratch registers for inline asm (caller-saved temporaries).
const ARM_GP_SCRATCH: &[&str] = &["x9", "x10", "x11", "x12", "x13", "x14", "x15", "x19", "x20", "x21"];

impl InlineAsmEmitter for ArmCodegen {
    fn asm_state(&mut self) -> &mut CodegenState { &mut self.state }
    fn asm_state_ref(&self) -> &CodegenState { &self.state }

    fn classify_constraint(&self, constraint: &str) -> AsmOperandKind {
        let c = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
        if !c.is_empty() && c.chars().all(|ch| ch.is_ascii_digit()) {
            if let Ok(n) = c.parse::<usize>() {
                return AsmOperandKind::Tied(n);
            }
        }
        if c == "m" { return AsmOperandKind::Memory; }
        // ARM doesn't use specific single-letter constraints like x86,
        // all "r" constraints get GP scratch registers.
        AsmOperandKind::GpReg
    }

    fn setup_operand_metadata(&self, op: &mut AsmOperand, val: &Operand, _is_output: bool) {
        if matches!(op.kind, AsmOperandKind::Memory) {
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
    }

    fn resolve_memory_operand(&mut self, op: &mut AsmOperand, val: &Operand) -> bool {
        if !op.mem_addr.is_empty() || op.mem_offset != 0 {
            return false;
        }
        if let Operand::Value(v) = val {
            if let Some(slot) = self.state.get_slot(v.0) {
                let tmp_reg = "x9";
                self.emit_load_from_sp(tmp_reg, slot.0, "ldr");
                op.mem_addr = format!("[{}]", tmp_reg);
                return true;
            }
        }
        false
    }

    fn assign_scratch_reg(&mut self, _kind: &AsmOperandKind) -> String {
        let idx = self.asm_scratch_idx;
        self.asm_scratch_idx += 1;
        if idx < ARM_GP_SCRATCH.len() {
            ARM_GP_SCRATCH[idx].to_string()
        } else {
            format!("x{}", 9 + idx)
        }
    }

    fn load_input_to_reg(&mut self, op: &AsmOperand, val: &Operand, _constraint: &str) {
        let reg = &op.reg;
        match val {
            Operand::Const(c) => {
                self.emit_load_imm64(reg, c.to_i64().unwrap_or(0));
            }
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    self.emit_load_from_sp(reg, slot.0, "ldr");
                }
            }
        }
    }

    fn preload_readwrite_output(&mut self, op: &AsmOperand, ptr: &Value) {
        let reg = &op.reg;
        if let Some(slot) = self.state.get_slot(ptr.0) {
            self.emit_load_from_sp(reg, slot.0, "ldr");
        }
    }

    fn substitute_template_line(&self, line: &str, operands: &[AsmOperand], _gcc_to_internal: &[usize], _operand_types: &[IrType]) -> String {
        let op_regs: Vec<String> = operands.iter().map(|o| o.reg.clone()).collect();
        let op_names: Vec<Option<String>> = operands.iter().map(|o| o.name.clone()).collect();
        Self::substitute_asm_operands_static(line, &op_regs, &op_names)
    }

    fn store_output_from_reg(&mut self, op: &AsmOperand, ptr: &Value, _constraint: &str) {
        if matches!(op.kind, AsmOperandKind::Memory) {
            return;
        }
        let reg = &op.reg;
        if let Some(slot) = self.state.get_slot(ptr.0) {
            if self.state.is_alloca(ptr.0) {
                self.emit_store_to_sp(reg, slot.0, "str");
            } else {
                // Non-alloca: slot holds a pointer, store through it
                let scratch = if reg != "x9" { "x9" } else { "x10" };
                self.emit_load_from_sp(scratch, slot.0, "ldr");
                self.state.emit(&format!("    str {}, [{}]", reg, scratch));
            }
        }
    }

    fn reset_scratch_state(&mut self) {
        self.asm_scratch_idx = 0;
    }
}
