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
        }
    }

    pub fn generate(mut self, module: &IrModule) -> String {
        generate_module(&mut self, module)
    }

    // --- AArch64 large-offset helpers ---

    /// Emit a large immediate subtraction from sp. For values > 4095, uses x16 as scratch.
    fn emit_sub_sp(&mut self, n: i64) {
        if n == 0 { return; }
        if n <= 4095 {
            self.state.emit(&format!("    sub sp, sp, #{}", n));
        } else if n <= 65535 {
            self.state.emit(&format!("    mov x16, #{}", n));
            self.state.emit("    sub sp, sp, x16");
        } else {
            self.state.emit(&format!("    movz x16, #{}", n & 0xFFFF));
            if (n >> 16) & 0xFFFF != 0 {
                self.state.emit(&format!("    movk x16, #{}, lsl #16", (n >> 16) & 0xFFFF));
            }
            if (n >> 32) & 0xFFFF != 0 {
                self.state.emit(&format!("    movk x16, #{}, lsl #32", (n >> 32) & 0xFFFF));
            }
            self.state.emit("    sub sp, sp, x16");
        }
    }

    /// Emit a large immediate addition to sp.
    fn emit_add_sp(&mut self, n: i64) {
        if n == 0 { return; }
        if n <= 4095 {
            self.state.emit(&format!("    add sp, sp, #{}", n));
        } else if n <= 65535 {
            self.state.emit(&format!("    mov x16, #{}", n));
            self.state.emit("    add sp, sp, x16");
        } else {
            self.state.emit(&format!("    movz x16, #{}", n & 0xFFFF));
            if (n >> 16) & 0xFFFF != 0 {
                self.state.emit(&format!("    movk x16, #{}, lsl #16", (n >> 16) & 0xFFFF));
            }
            if (n >> 32) & 0xFFFF != 0 {
                self.state.emit(&format!("    movk x16, #{}, lsl #32", (n >> 32) & 0xFFFF));
            }
            self.state.emit("    add sp, sp, x16");
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

    /// Emit store to [sp, #offset], handling large offsets via x16.
    fn emit_store_to_sp(&mut self, reg: &str, offset: i64, instr: &str) {
        if Self::is_valid_imm_offset(offset, instr, reg) {
            self.state.emit(&format!("    {} {}, [sp, #{}]", instr, reg, offset));
        } else {
            self.load_large_imm("x16", offset);
            self.state.emit("    add x16, sp, x16");
            self.state.emit(&format!("    {} {}, [x16]", instr, reg));
        }
    }

    /// Emit load from [sp, #offset], handling large offsets via x16.
    fn emit_load_from_sp(&mut self, reg: &str, offset: i64, instr: &str) {
        if Self::is_valid_imm_offset(offset, instr, reg) {
            self.state.emit(&format!("    {} {}, [sp, #{}]", instr, reg, offset));
        } else {
            self.load_large_imm("x16", offset);
            self.state.emit("    add x16, sp, x16");
            self.state.emit(&format!("    {} {}, [x16]", instr, reg));
        }
    }

    /// Emit `stp reg1, reg2, [sp, #offset]` handling large offsets.
    fn emit_stp_to_sp(&mut self, reg1: &str, reg2: &str, offset: i64) {
        // stp supports signed offsets in [-512, 504] range (multiples of 8)
        if offset >= -512 && offset <= 504 {
            self.state.emit(&format!("    stp {}, {}, [sp, #{}]", reg1, reg2, offset));
        } else {
            self.load_large_imm("x16", offset);
            self.state.emit("    add x16, sp, x16");
            self.state.emit(&format!("    stp {}, {}, [x16]", reg1, reg2));
        }
    }

    /// Emit `add dest, sp, #offset` handling large offsets.
    fn emit_add_sp_offset(&mut self, dest: &str, offset: i64) {
        if offset >= 0 && offset <= 4095 {
            self.state.emit(&format!("    add {}, sp, #{}", dest, offset));
        } else {
            self.load_large_imm("x16", offset);
            self.state.emit(&format!("    add {}, sp, x16", dest));
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
                            self.state.emit(&format!("    mov x0, #{}", *v as u64 & 0xffff));
                            if (*v as u64 >> 16) & 0xffff != 0 {
                                self.state.emit(&format!("    movk x0, #{}, lsl #16", (*v as u64 >> 16) & 0xffff));
                            }
                            if (*v as u64 >> 32) & 0xffff != 0 {
                                self.state.emit(&format!("    movk x0, #{}, lsl #32", (*v as u64 >> 32) & 0xffff));
                            }
                            if (*v as u64 >> 48) & 0xffff != 0 {
                                self.state.emit(&format!("    movk x0, #{}, lsl #48", (*v as u64 >> 48) & 0xffff));
                            }
                        }
                    }
                    IrConst::F32(v) => {
                        let bits = v.to_bits() as u64;
                        if bits == 0 {
                            self.state.emit("    mov x0, #0");
                        } else if bits <= 65535 {
                            self.state.emit(&format!("    mov x0, #{}", bits));
                        } else {
                            self.state.emit(&format!("    mov x0, #{}", bits & 0xffff));
                            if (bits >> 16) & 0xffff != 0 {
                                self.state.emit(&format!("    movk x0, #{}, lsl #16", (bits >> 16) & 0xffff));
                            }
                            if (bits >> 32) & 0xffff != 0 {
                                self.state.emit(&format!("    movk x0, #{}, lsl #32", (bits >> 32) & 0xffff));
                            }
                            if (bits >> 48) & 0xffff != 0 {
                                self.state.emit(&format!("    movk x0, #{}, lsl #48", (bits >> 48) & 0xffff));
                            }
                        }
                    }
                    IrConst::F64(v) => {
                        let bits = v.to_bits();
                        if bits == 0 {
                            self.state.emit("    mov x0, #0");
                        } else if bits <= 65535 {
                            self.state.emit(&format!("    mov x0, #{}", bits));
                        } else {
                            self.state.emit(&format!("    mov x0, #{}", bits & 0xffff));
                            if (bits >> 16) & 0xffff != 0 {
                                self.state.emit(&format!("    movk x0, #{}, lsl #16", (bits >> 16) & 0xffff));
                            }
                            if (bits >> 32) & 0xffff != 0 {
                                self.state.emit(&format!("    movk x0, #{}, lsl #32", (bits >> 32) & 0xffff));
                            }
                            if (bits >> 48) & 0xffff != 0 {
                                self.state.emit(&format!("    movk x0, #{}, lsl #48", (bits >> 48) & 0xffff));
                            }
                        }
                    }
                    // LongDouble at computation level is treated as F64
                    IrConst::LongDouble(v) => {
                        let bits = v.to_bits();
                        if bits == 0 {
                            self.state.emit("    mov x0, #0");
                        } else {
                            self.state.emit(&format!("    mov x0, #{}", bits & 0xffff));
                            if (bits >> 16) & 0xffff != 0 {
                                self.state.emit(&format!("    movk x0, #{}, lsl #16", (bits >> 16) & 0xffff));
                            }
                            if (bits >> 32) & 0xffff != 0 {
                                self.state.emit(&format!("    movk x0, #{}, lsl #32", (bits >> 32) & 0xffff));
                            }
                            if (bits >> 48) & 0xffff != 0 {
                                self.state.emit(&format!("    movk x0, #{}, lsl #48", (bits >> 48) & 0xffff));
                            }
                        }
                    }
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

    /// Convert an x-register name to its w-register counterpart.
    fn w_for_x(xreg: &str) -> &'static str {
        match xreg {
            "x0" => "w0", "x1" => "w1", "x2" => "w2", "x3" => "w3",
            "x4" => "w4", "x5" => "w5", "x6" => "w6", "x7" => "w7",
            "x8" => "w8", "x9" => "w9", "x10" => "w10", "x11" => "w11",
            "x12" => "w12", "x13" => "w13", "x14" => "w14", "x15" => "w15",
            "x16" => "w16", "x17" => "w17",
            _ => "w0",
        }
    }

    /// Emit a type cast instruction sequence for AArch64.
    fn emit_cast_instrs(&mut self, from_ty: IrType, to_ty: IrType) {
        if from_ty == to_ty { return; }

        // Ptr is equivalent to I64/U64 on AArch64 (both 8 bytes, no conversion needed)
        // Treat Ptr <-> I64/U64 and Ptr <-> Ptr as no-ops
        if (from_ty == IrType::Ptr || to_ty == IrType::Ptr) && !from_ty.is_float() && !to_ty.is_float() {
            // Ptr -> integer smaller than 64-bit: truncate
            // Integer smaller than 64-bit -> Ptr: zero/sign-extend
            // Ptr <-> I64/U64: no-op (same size)
            let effective_from = if from_ty == IrType::Ptr { IrType::U64 } else { from_ty };
            let effective_to = if to_ty == IrType::Ptr { IrType::U64 } else { to_ty };
            if effective_from == effective_to || (effective_from.size() == 8 && effective_to.size() == 8) {
                return;
            }
            // Delegate to integer cast logic below with effective types
            return self.emit_cast_instrs(effective_from, effective_to);
        }

        // Float-to-int cast
        if from_ty.is_float() && !to_ty.is_float() {
            let is_unsigned_dest = to_ty.is_unsigned() || to_ty == IrType::Ptr;
            if from_ty == IrType::F32 {
                // F32 bits in w0 -> convert to int
                self.state.emit("    fmov s0, w0");
                if is_unsigned_dest {
                    self.state.emit("    fcvtzu x0, s0");
                } else {
                    self.state.emit("    fcvtzs x0, s0");
                }
            } else {
                // F64 bits in x0 -> convert to int
                self.state.emit("    fmov d0, x0");
                if is_unsigned_dest {
                    self.state.emit("    fcvtzu x0, d0");
                } else {
                    self.state.emit("    fcvtzs x0, d0");
                }
            }
            return;
        }

        // Int-to-float cast
        if !from_ty.is_float() && to_ty.is_float() {
            let is_unsigned_src = from_ty.is_unsigned();
            if to_ty == IrType::F32 {
                if is_unsigned_src {
                    self.state.emit("    ucvtf s0, x0");
                } else {
                    self.state.emit("    scvtf s0, x0");
                }
                self.state.emit("    fmov w0, s0");
            } else {
                if is_unsigned_src {
                    self.state.emit("    ucvtf d0, x0");
                } else {
                    self.state.emit("    scvtf d0, x0");
                }
                self.state.emit("    fmov x0, d0");
            }
            return;
        }

        // Float-to-float cast (F32 <-> F64)
        if from_ty.is_float() && to_ty.is_float() {
            if from_ty == IrType::F32 && to_ty == IrType::F64 {
                self.state.emit("    fmov s0, w0");
                self.state.emit("    fcvt d0, s0");
                self.state.emit("    fmov x0, d0");
            } else {
                self.state.emit("    fmov d0, x0");
                self.state.emit("    fcvt s0, d0");
                self.state.emit("    fmov w0, s0");
            }
            return;
        }

        if from_ty.size() == to_ty.size() {
            return;
        }

        let from_size = from_ty.size();
        let to_size = to_ty.size();

        if to_size > from_size {
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
        } else if to_size < from_size {
            // Narrowing: truncate and sign/zero-extend back to 64-bit
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
        // Same size: pointer <-> integer conversions (no-op on AArch64)
    }
}

const ARM_ARG_REGS: [&str; 8] = ["x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"];
const ARM_TMP_REGS: [&str; 8] = ["x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16"];

impl ArchCodegen for ArmCodegen {
    fn state(&mut self) -> &mut CodegenState { &mut self.state }
    fn state_ref(&self) -> &CodegenState { &self.state }
    fn ptr_directive(&self) -> PtrDirective { PtrDirective::Xword }
    fn function_type_directive(&self) -> &'static str { "%function" }

    fn calculate_stack_space(&mut self, func: &IrFunction) -> i64 {
        let mut space = calculate_stack_space_common(&mut self.state, func, 16, |space, alloc_size| {
            // ARM uses positive offsets from sp, starting at 16 (after fp/lr)
            let slot = space;
            let new_space = space + ((alloc_size + 7) & !7).max(8);
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

        // Classify each param: int reg, float reg, or stack-passed
        let mut int_reg_idx = 0usize;
        let mut float_reg_idx = 0usize;
        let mut param_class: Vec<char> = Vec::new(); // 'i', 'f', 's'
        let mut param_int_reg: Vec<usize> = Vec::new();
        let mut param_float_reg: Vec<usize> = Vec::new();
        let mut stack_offsets: Vec<i64> = Vec::new();
        let mut stack_offset: i64 = 0;

        for param in func.params.iter() {
            let is_float = param.ty.is_float();
            if is_float && float_reg_idx < 8 {
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

        // Phase 3: Store stack-passed params (above callee's frame).
        for (i, param) in func.params.iter().enumerate() {
            if param_class[i] != 's' || param.name.is_empty() { continue; }
            if let Some((dest, ty)) = find_param_alloca(func, i) {
                if let Some(slot) = self.state.get_slot(dest.0) {
                    let caller_offset = frame_size + stack_offsets[i];
                    self.emit_load_from_sp("x0", caller_offset, "ldr");
                    let store_instr = Self::str_for_type(ty);
                    let reg = Self::reg_for_type("x0", ty);
                    self.emit_store_to_sp(reg, slot.0, store_instr);
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

    fn emit_store(&mut self, val: &Operand, ptr: &Value, ty: IrType) {
        self.operand_to_x0(val);
        if let Some(slot) = self.state.get_slot(ptr.0) {
            if self.state.is_alloca(ptr.0) {
                let store_instr = Self::str_for_type(ty);
                let reg = Self::reg_for_type("x0", ty);
                self.emit_store_to_sp(reg, slot.0, store_instr);
            } else {
                self.state.emit("    mov x1, x0");
                self.emit_load_from_sp("x2", slot.0, "ldr");
                let store_instr = Self::str_for_type(ty);
                let reg = Self::reg_for_type("x1", ty);
                self.state.emit(&format!("    {} {}, [x2]", store_instr, reg));
            }
        }
    }

    fn emit_load(&mut self, dest: &Value, ptr: &Value, ty: IrType) {
        if let Some(slot) = self.state.get_slot(ptr.0) {
            let load_instr = Self::ldr_for_type(ty);
            let dest_reg = Self::load_dest_reg(ty);
            if self.state.is_alloca(ptr.0) {
                self.emit_load_from_sp(dest_reg, slot.0, load_instr);
            } else {
                self.emit_load_from_sp("x0", slot.0, "ldr");
                self.state.emit("    mov x1, x0");
                self.state.emit(&format!("    {} {}, [x1]", load_instr, dest_reg));
            }
            self.store_x0_to(dest);
        }
    }

    fn emit_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if ty.is_float() {
            self.operand_to_x0(lhs);
            self.state.emit("    mov x1, x0");
            self.operand_to_x0(rhs);
            self.state.emit("    mov x2, x0");
            if ty == IrType::F32 {
                // F32: bit pattern in low 32 bits of x-reg, use s-registers
                self.state.emit("    fmov s0, w1");
                self.state.emit("    fmov s1, w2");
                match op {
                    IrBinOp::Add => self.state.emit("    fadd s0, s0, s1"),
                    IrBinOp::Sub => self.state.emit("    fsub s0, s0, s1"),
                    IrBinOp::Mul => self.state.emit("    fmul s0, s0, s1"),
                    IrBinOp::SDiv | IrBinOp::UDiv => self.state.emit("    fdiv s0, s0, s1"),
                    _ => self.state.emit("    fadd s0, s0, s1"),
                }
                self.state.emit("    fmov w0, s0");
                // Zero-extend to 64-bit so upper bits are clean
                self.state.emit("    mov w0, w0");
            } else {
                // F64: full 64-bit bit pattern in x-reg, use d-registers
                self.state.emit("    fmov d0, x1");
                self.state.emit("    fmov d1, x2");
                match op {
                    IrBinOp::Add => self.state.emit("    fadd d0, d0, d1"),
                    IrBinOp::Sub => self.state.emit("    fsub d0, d0, d1"),
                    IrBinOp::Mul => self.state.emit("    fmul d0, d0, d1"),
                    IrBinOp::SDiv | IrBinOp::UDiv => self.state.emit("    fdiv d0, d0, d1"),
                    _ => self.state.emit("    fadd d0, d0, d1"),
                }
                self.state.emit("    fmov x0, d0");
            }
            self.store_x0_to(dest);
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

    fn emit_unaryop(&mut self, dest: &Value, op: IrUnaryOp, src: &Operand, ty: IrType) {
        self.operand_to_x0(src);
        if ty.is_float() {
            match op {
                IrUnaryOp::Neg => {
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
                IrUnaryOp::Not => self.state.emit("    mvn x0, x0"),
            }
        } else {
            match op {
                IrUnaryOp::Neg => self.state.emit("    neg x0, x0"),
                IrUnaryOp::Not => self.state.emit("    mvn x0, x0"),
            }
        }
        self.store_x0_to(dest);
    }

    fn emit_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
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
        // For indirect calls, load the function pointer into x17 BEFORE
        // setting up arguments, to avoid clobbering x0 (first arg register).
        if func_ptr.is_some() && direct_name.is_none() {
            let ptr = func_ptr.unwrap();
            self.operand_to_x0(ptr);
            self.state.emit("    mov x17, x0");
        }

        // Classify args: determine which go in registers vs stack.
        // On AArch64 (AAPCS64), both named and variadic float args go in FP registers (d0-d7),
        // and int args go in GP registers (x0-x7). The callee saves both register sets
        // and uses va_list with __gr_offs/__vr_offs to access variadic args.
        let mut arg_classes: Vec<char> = Vec::new(); // 'f' = float reg, 'i' = int reg, 's' = stack
        let mut fi = 0usize;
        let mut ii = 0usize;
        for (i, _arg) in args.iter().enumerate() {
            let arg_ty = if i < arg_types.len() { Some(arg_types[i]) } else { None };
            let is_float = if let Some(ty) = arg_ty {
                ty.is_float()
            } else {
                matches!(args[i], Operand::Const(IrConst::F32(_) | IrConst::F64(_)))
            };
            if is_float && fi < 8 {
                arg_classes.push('f');
                fi += 1;
            } else if !is_float && ii < 8 {
                arg_classes.push('i');
                ii += 1;
            } else {
                arg_classes.push('s');
            }
        }

        // Count stack arguments
        let stack_arg_indices: Vec<usize> = args.iter().enumerate()
            .filter(|(i, _)| arg_classes[*i] == 's')
            .map(|(i, _)| i)
            .collect();
        let num_stack_args = stack_arg_indices.len();
        let stack_arg_space = (((num_stack_args * 8) + 15) / 16) * 16;

        // Phase 1a: Load GP register args into temp registers (x9-x16).
        // FP args are handled separately since they use a different register bank.
        let mut gp_tmp_idx = 0usize;
        for (i, arg) in args.iter().enumerate() {
            if arg_classes[i] != 'i' { continue; }
            if gp_tmp_idx >= 8 { break; }
            self.operand_to_x0(arg);
            self.state.emit(&format!("    mov {}, x0", ARM_TMP_REGS[gp_tmp_idx]));
            gp_tmp_idx += 1;
        }

        // Phase 1b: Load FP register args directly into d0-d7 (or s0-s7).
        // FP regs don't conflict with GP temp regs, so this is safe.
        let mut fp_reg_idx = 0usize;
        for (i, arg) in args.iter().enumerate() {
            if arg_classes[i] != 'f' { continue; }
            if fp_reg_idx >= 8 { break; }
            let arg_ty = if i < arg_types.len() { Some(arg_types[i]) } else { None };
            self.operand_to_x0(arg);
            if arg_ty == Some(IrType::F32) {
                self.state.emit(&format!("    fmov s{}, w0", fp_reg_idx));
            } else {
                self.state.emit(&format!("    fmov d{}, x0", fp_reg_idx));
            }
            fp_reg_idx += 1;
        }

        // For stack args: load them all before adjusting SP, save to stack
        if stack_arg_space > 0 {
            // Pre-decrement SP and store stack args
            self.emit_sub_sp(stack_arg_space as i64);
            // Now we need to load operands with adjusted SP offsets
            let mut stack_offset = 0i64;
            for &arg_idx in &stack_arg_indices {
                // For constants, no SP adjustment needed
                match &args[arg_idx] {
                    Operand::Const(_) => {
                        self.operand_to_x0(&args[arg_idx]);
                    }
                    Operand::Value(v) => {
                        // SP moved by stack_arg_space, adjust slot offset
                        if let Some(slot) = self.state.get_slot(v.0) {
                            let adjusted = slot.0 + stack_arg_space as i64;
                            if self.state.is_alloca(v.0) {
                                self.emit_add_sp_offset("x0", adjusted);
                            } else {
                                self.emit_load_from_sp("x0", adjusted, "ldr");
                            }
                        } else {
                            self.state.emit("    mov x0, #0");
                        }
                    }
                }
                self.emit_store_to_sp("x0", stack_offset as i64, "str");
                stack_offset += 8;
            }
        }

        // Phase 2: Move GP args from temp regs to actual arg registers (x0-x7).
        let mut int_reg_idx = 0usize;
        gp_tmp_idx = 0;
        for (i, _arg) in args.iter().enumerate() {
            if arg_classes[i] != 'i' { continue; }
            if gp_tmp_idx >= 8 { break; }
            if int_reg_idx < 8 {
                self.state.emit(&format!("    mov {}, {}", ARM_ARG_REGS[int_reg_idx], ARM_TMP_REGS[gp_tmp_idx]));
                int_reg_idx += 1;
            }
            gp_tmp_idx += 1;
        }
        // FP args already in d0-d7 from Phase 1b.

        if let Some(name) = direct_name {
            self.state.emit(&format!("    bl {}", name));
        } else if func_ptr.is_some() {
            self.state.emit("    blr x17");
        }

        // Clean up stack args
        if stack_arg_space > 0 {
            self.emit_add_sp(stack_arg_space as i64);
        }

        if let Some(dest) = dest {
            if return_type == IrType::F32 {
                // F32 return value is in s0 per AAPCS64
                self.state.emit("    fmov w0, s0");
            } else if return_type.is_float() {
                // F64 return value is in d0 per AAPCS64
                self.state.emit("    fmov x0, d0");
            }
            self.store_x0_to(&dest);
        }
    }

    fn emit_global_addr(&mut self, dest: &Value, name: &str) {
        self.state.emit(&format!("    adrp x0, {}", name));
        self.state.emit(&format!("    add x0, x0, :lo12:{}", name));
        self.store_x0_to(dest);
    }

    fn emit_gep(&mut self, dest: &Value, base: &Value, offset: &Operand) {
        if let Some(slot) = self.state.get_slot(base.0) {
            if self.state.is_alloca(base.0) {
                self.emit_add_sp_offset("x1", slot.0);
            } else {
                self.emit_load_from_sp("x1", slot.0, "ldr");
            }
        }
        self.operand_to_x0(offset);
        self.state.emit("    add x0, x1, x0");
        self.store_x0_to(dest);
    }

    fn emit_cast(&mut self, dest: &Value, src: &Operand, from_ty: IrType, to_ty: IrType) {
        self.operand_to_x0(src);
        self.emit_cast_instrs(from_ty, to_ty);
        self.store_x0_to(dest);
    }

    fn emit_memcpy(&mut self, dest: &Value, src: &Value, size: usize) {
        // Load dest address into x9, src address into x10
        if let Some(dst_slot) = self.state.get_slot(dest.0) {
            if self.state.is_alloca(dest.0) {
                self.emit_add_sp_offset("x9", dst_slot.0);
            } else {
                self.emit_load_from_sp("x9", dst_slot.0, "ldr");
            }
        }
        if let Some(src_slot) = self.state.get_slot(src.0) {
            if self.state.is_alloca(src.0) {
                self.emit_add_sp_offset("x10", src_slot.0);
            } else {
                self.emit_load_from_sp("x10", src_slot.0, "ldr");
            }
        }
        // Inline byte-by-byte copy using a loop
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

    fn emit_va_arg(&mut self, dest: &Value, va_list_ptr: &Value, result_ty: IrType) {
        // AArch64 AAPCS64 va_arg implementation.
        // va_list layout:
        //   [x1+0]  __stack    (void*)  - next stack overflow arg
        //   [x1+8]  __gr_top   (void*)  - end of GP save area
        //   [x1+16] __vr_top   (void*)  - end of FP/SIMD save area
        //   [x1+24] __gr_offs  (int32)  - offset from __gr_top (negative means regs available)
        //   [x1+28] __vr_offs  (int32)  - offset from __vr_top (negative means regs available)
        //
        // Algorithm for GP types (int, ptr, etc.):
        //   if __gr_offs < 0:
        //     addr = __gr_top + __gr_offs
        //     __gr_offs += 8
        //   else:
        //     addr = __stack; __stack += 8
        //
        // Algorithm for FP types (float, double):
        //   if __vr_offs < 0:
        //     addr = __vr_top + __vr_offs
        //     __vr_offs += 16
        //   else:
        //     addr = __stack; __stack += 8

        let is_fp = result_ty.is_float();
        let label_id = self.state.next_label_id();
        let label_stack = format!(".Lva_stack_{}", label_id);
        let label_done = format!(".Lva_done_{}", label_id);

        // x1 = pointer to va_list struct
        if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
            if self.state.is_alloca(va_list_ptr.0) {
                self.state.emit(&format!("    add x1, x29, #{}", slot.0));
            } else {
                self.state.emit(&format!("    ldr x1, [x29, #{}]", slot.0));
            }
        }

        if is_fp {
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
                // F32: stored in lower 4 bytes of 16-byte Q register slot
                self.state.emit("    ldr w0, [x3]");
            } else {
                // F64: stored in lower 8 bytes of 16-byte Q register slot
                self.state.emit("    ldr x0, [x3]");
            }
            self.state.emit(&format!("    b {}", label_done));
        } else {
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
        }

        // Stack overflow path
        self.state.emit(&format!("{}:", label_stack));
        self.state.emit("    ldr x3, [x1]");           // x3 = __stack
        if result_ty == IrType::F32 {
            self.state.emit("    ldr w0, [x3]");
        } else {
            self.state.emit("    ldr x0, [x3]");
        }
        self.state.emit("    add x3, x3, #8");         // advance __stack by 8
        self.state.emit("    str x3, [x1]");

        self.state.emit(&format!("{}:", label_done));
        // Store result to dest
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.state.emit(&format!("    str x0, [x29, #{}]", slot.0));
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
                self.state.emit(&format!("    add x0, x29, #{}", slot.0));
            } else {
                self.state.emit(&format!("    ldr x0, [x29, #{}]", slot.0));
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

    fn emit_va_end(&mut self, _va_list_ptr: &Value) {
        // va_end is a no-op on AArch64
    }

    fn emit_va_copy(&mut self, dest_ptr: &Value, src_ptr: &Value) {
        // Copy va_list struct (32 bytes on AArch64)
        if let Some(src_slot) = self.state.get_slot(src_ptr.0) {
            if self.state.is_alloca(src_ptr.0) {
                self.state.emit(&format!("    add x1, x29, #{}", src_slot.0));
            } else {
                self.state.emit(&format!("    ldr x1, [x29, #{}]", src_slot.0));
            }
        }
        if let Some(dest_slot) = self.state.get_slot(dest_ptr.0) {
            if self.state.is_alloca(dest_ptr.0) {
                self.state.emit(&format!("    add x0, x29, #{}", dest_slot.0));
            } else {
                self.state.emit(&format!("    ldr x0, [x29, #{}]", dest_slot.0));
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
            self.operand_to_x0(val);
            if self.current_return_type == IrType::F32 {
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

    fn emit_branch(&mut self, label: &str) {
        self.state.emit(&format!("    b {}", label));
    }

    fn emit_cond_branch(&mut self, cond: &Operand, true_label: &str, false_label: &str) {
        self.operand_to_x0(cond);
        self.state.emit(&format!("    cbnz x0, {}", true_label));
        self.state.emit(&format!("    b {}", false_label));
    }

    fn emit_unreachable(&mut self) {
        self.state.emit("    brk #0");
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
}

impl ArmCodegen {
    /// Get the exclusive load/store instructions and register prefix for a type.
    fn exclusive_instrs(ty: IrType) -> (&'static str, &'static str, &'static str) {
        match ty {
            IrType::I8 | IrType::U8 => ("ldxrb", "stxrb", "w"),
            IrType::I16 | IrType::U16 => ("ldxrh", "stxrh", "w"),
            IrType::I32 | IrType::U32 => ("ldxr", "stxr", "w"),
            _ => ("ldxr", "stxr", "x"),
        }
    }

    /// Emit the arithmetic operation for an atomic RMW.
    fn emit_atomic_op_arm(state: &mut CodegenState, op: AtomicRmwOp, dest_reg: &str, old_reg: &str, val_reg: &str) {
        match op {
            AtomicRmwOp::Add => state.emit(&format!("    add {}, {}, {}", dest_reg, old_reg, val_reg)),
            AtomicRmwOp::Sub => state.emit(&format!("    sub {}, {}, {}", dest_reg, old_reg, val_reg)),
            AtomicRmwOp::And => state.emit(&format!("    and {}, {}, {}", dest_reg, old_reg, val_reg)),
            AtomicRmwOp::Or  => state.emit(&format!("    orr {}, {}, {}", dest_reg, old_reg, val_reg)),
            AtomicRmwOp::Xor => state.emit(&format!("    eor {}, {}, {}", dest_reg, old_reg, val_reg)),
            AtomicRmwOp::Nand => {
                state.emit(&format!("    and {}, {}, {}", dest_reg, old_reg, val_reg));
                state.emit(&format!("    mvn {}, {}", dest_reg, dest_reg));
            }
            AtomicRmwOp::Xchg | AtomicRmwOp::TestAndSet => {
                // Handled separately in emit_atomic_rmw
                state.emit(&format!("    mov {}, {}", dest_reg, val_reg));
            }
        }
    }
}

impl Default for ArmCodegen {
    fn default() -> Self {
        Self::new()
    }
}
