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
}

impl ArmCodegen {
    pub fn new() -> Self {
        Self {
            state: CodegenState::new(),
            current_frame_size: 0,
            current_return_type: IrType::I64,
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

    /// Emit store to [sp, #offset], handling large offsets via x16.
    fn emit_store_to_sp(&mut self, reg: &str, offset: i64, instr: &str) {
        if offset >= 0 && offset <= 32760 {
            self.state.emit(&format!("    {} {}, [sp, #{}]", instr, reg, offset));
        } else {
            self.load_large_imm("x16", offset);
            self.state.emit("    add x16, sp, x16");
            self.state.emit(&format!("    {} {}, [x16]", instr, reg));
        }
    }

    /// Emit load from [sp, #offset], handling large offsets via x16.
    fn emit_load_from_sp(&mut self, reg: &str, offset: i64, instr: &str) {
        if offset >= 0 && offset <= 32760 {
            self.state.emit(&format!("    {} {}, [sp, #{}]", instr, reg, offset));
        } else {
            self.load_large_imm("x16", offset);
            self.state.emit("    add x16, sp, x16");
            self.state.emit(&format!("    {} {}, [x16]", instr, reg));
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
        calculate_stack_space_common(&mut self.state, func, 16, |space, alloc_size| {
            // ARM uses positive offsets from sp, starting at 16 (after fp/lr)
            let slot = space;
            let new_space = space + ((alloc_size + 7) & !7).max(8);
            (slot, new_space)
        })
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
        let mut int_reg_idx = 0usize;
        let mut float_reg_idx = 0usize;
        // Stack-passed params are above the callee's frame at [sp + frame_size + offset]
        let mut stack_param_offset: i64 = 0;
        let frame_size = self.current_frame_size;
        for (i, param) in func.params.iter().enumerate() {
            let is_float = param.ty.is_float();
            let is_stack_passed = if is_float { float_reg_idx >= 8 } else { int_reg_idx >= 8 };

            if param.name.is_empty() {
                if is_stack_passed {
                    stack_param_offset += 8;
                } else if is_float {
                    float_reg_idx += 1;
                } else {
                    int_reg_idx += 1;
                }
                continue;
            }
            if let Some((dest, ty)) = find_param_alloca(func, i) {
                if let Some(slot) = self.state.get_slot(dest.0) {
                    if is_stack_passed {
                        // Stack-passed parameter: load from caller's stack above our frame
                        let caller_offset = frame_size + stack_param_offset;
                        self.emit_load_from_sp("x0", caller_offset, "ldr");
                        let store_instr = Self::str_for_type(ty);
                        let reg = Self::reg_for_type("x0", ty);
                        self.emit_store_to_sp(reg, slot.0, store_instr);
                        stack_param_offset += 8;
                    } else if is_float {
                        // Float params arrive in sN/dN per AAPCS64
                        if ty == IrType::F32 {
                            self.state.emit(&format!("    fmov w0, s{}", float_reg_idx));
                            self.emit_store_to_sp("x0", slot.0, "str");
                        } else {
                            self.state.emit(&format!("    fmov x0, d{}", float_reg_idx));
                            self.emit_store_to_sp("x0", slot.0, "str");
                        }
                        float_reg_idx += 1;
                    } else {
                        let store_instr = Self::str_for_type(ty);
                        let reg = Self::reg_for_type(ARM_ARG_REGS[int_reg_idx], ty);
                        self.emit_store_to_sp(reg, slot.0, store_instr);
                        int_reg_idx += 1;
                    }
                }
            } else {
                if is_stack_passed {
                    stack_param_offset += 8;
                } else if is_float {
                    float_reg_idx += 1;
                } else {
                    int_reg_idx += 1;
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
                 _is_variadic: bool) {
        let num_args = args.len().min(8);
        let mut float_reg_idx = 0usize;
        let mut int_reg_idx = 0usize;

        // For indirect calls, load the function pointer into x17 BEFORE
        // setting up arguments, to avoid clobbering x0 (first arg register).
        if func_ptr.is_some() && direct_name.is_none() {
            let ptr = func_ptr.unwrap();
            self.operand_to_x0(ptr);
            self.state.emit("    mov x17, x0");
        }

        // Load all args into temp registers first (to avoid clobbering)
        for (i, arg) in args.iter().enumerate().take(num_args) {
            self.operand_to_x0(arg);
            self.state.emit(&format!("    mov {}, x0", ARM_TMP_REGS[i]));
        }
        // Move from temps to arg registers using type info for float detection.
        // Float args go to dN; integer args go to xN.
        // For AAPCS64 (Linux): float/double args use d0-d7, int args use x0-x7,
        // even for variadic functions.
        for (i, _arg) in args.iter().enumerate().take(num_args) {
            let arg_ty = if i < arg_types.len() {
                Some(arg_types[i])
            } else {
                None
            };
            let is_float_arg = if let Some(ty) = arg_ty {
                ty.is_float()
            } else {
                matches!(args[i], Operand::Const(IrConst::F32(_) | IrConst::F64(_)))
            };
            if is_float_arg && float_reg_idx < 8 {
                if arg_ty == Some(IrType::F32) {
                    // F32 value: bit pattern in low 32 bits of temp reg
                    self.state.emit(&format!("    fmov s{}, {}",
                        float_reg_idx, Self::w_for_x(ARM_TMP_REGS[i])));
                } else {
                    // F64 value: full 64-bit bit pattern in temp reg
                    self.state.emit(&format!("    fmov d{}, {}", float_reg_idx, ARM_TMP_REGS[i]));
                }
                float_reg_idx += 1;
            } else if int_reg_idx < 8 {
                self.state.emit(&format!("    mov {}, {}", ARM_ARG_REGS[int_reg_idx], ARM_TMP_REGS[i]));
                int_reg_idx += 1;
            }
        }

        if let Some(name) = direct_name {
            self.state.emit(&format!("    bl {}", name));
        } else if func_ptr.is_some() {
            // x17 was loaded above before argument setup
            self.state.emit("    blr x17");
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
}

impl Default for ArmCodegen {
    fn default() -> Self {
        Self::new()
    }
}
