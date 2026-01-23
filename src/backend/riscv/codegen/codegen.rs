use crate::ir::ir::*;
use crate::common::types::IrType;
use crate::backend::common::PtrDirective;
use crate::backend::codegen_shared::*;

/// RISC-V 64 code generator. Implements the ArchCodegen trait for the shared framework.
/// Uses standard RISC-V calling convention with stack-based allocation.
pub struct RiscvCodegen {
    state: CodegenState,
    current_return_type: IrType,
}

impl RiscvCodegen {
    pub fn new() -> Self {
        Self { state: CodegenState::new(), current_return_type: IrType::I64 }
    }

    pub fn generate(mut self, module: &IrModule) -> String {
        generate_module(&mut self, module)
    }

    // --- RISC-V helpers ---

    /// Check if an immediate fits in a 12-bit signed field.
    fn fits_imm12(val: i64) -> bool {
        val >= -2048 && val <= 2047
    }

    /// Emit: store `reg` to `offset(s0)`, handling large offsets via t5.
    fn emit_store_to_s0(&mut self, reg: &str, offset: i64, store_instr: &str) {
        if Self::fits_imm12(offset) {
            self.state.emit(&format!("    {} {}, {}(s0)", store_instr, reg, offset));
        } else {
            self.state.emit(&format!("    li t5, {}", offset));
            self.state.emit("    add t5, s0, t5");
            self.state.emit(&format!("    {} {}, 0(t5)", store_instr, reg));
        }
    }

    /// Emit: load from `offset(s0)` into `reg`, handling large offsets via t5.
    fn emit_load_from_s0(&mut self, reg: &str, offset: i64, load_instr: &str) {
        if Self::fits_imm12(offset) {
            self.state.emit(&format!("    {} {}, {}(s0)", load_instr, reg, offset));
        } else {
            self.state.emit(&format!("    li t5, {}", offset));
            self.state.emit("    add t5, s0, t5");
            self.state.emit(&format!("    {} {}, 0(t5)", load_instr, reg));
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

    /// Emit prologue: allocate stack and save ra/s0.
    fn emit_prologue_riscv(&mut self, frame_size: i64) {
        if Self::fits_imm12(-frame_size) {
            self.state.emit(&format!("    addi sp, sp, -{}", frame_size));
            self.state.emit(&format!("    sd ra, {}(sp)", frame_size - 8));
            self.state.emit(&format!("    sd s0, {}(sp)", frame_size - 16));
            self.state.emit(&format!("    addi s0, sp, {}", frame_size));
        } else {
            self.state.emit(&format!("    li t0, {}", frame_size));
            self.state.emit("    sub sp, sp, t0");
            self.state.emit("    sd ra, 0(sp)");
            self.state.emit("    sd s0, 8(sp)");
            self.state.emit(&format!("    li t0, {}", frame_size));
            self.state.emit("    add s0, sp, t0");
        }
    }

    /// Emit epilogue: restore ra/s0 and deallocate stack.
    fn emit_epilogue_riscv(&mut self, frame_size: i64) {
        if Self::fits_imm12(-frame_size) {
            self.state.emit(&format!("    ld ra, {}(sp)", frame_size - 8));
            self.state.emit(&format!("    ld s0, {}(sp)", frame_size - 16));
            self.state.emit(&format!("    addi sp, sp, {}", frame_size));
        } else {
            self.state.emit("    ld ra, 0(sp)");
            self.state.emit("    ld s0, 8(sp)");
            self.state.emit(&format!("    li t0, {}", frame_size));
            self.state.emit("    add sp, sp, t0");
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

    fn store_for_type(ty: IrType) -> &'static str {
        match ty {
            IrType::I8 | IrType::U8 => "sb",
            IrType::I16 | IrType::U16 => "sh",
            IrType::I32 | IrType::U32 => "sw",
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
            IrType::U32 => "lwu",
            _ => "ld",
        }
    }

    /// Emit a type cast instruction sequence for RISC-V 64.
    fn emit_cast_instrs(&mut self, from_ty: IrType, to_ty: IrType) {
        if from_ty == to_ty { return; }

        // Ptr is equivalent to I64/U64 on RISC-V 64 (both 8 bytes, no conversion needed)
        if (from_ty == IrType::Ptr || to_ty == IrType::Ptr) && !from_ty.is_float() && !to_ty.is_float() {
            let effective_from = if from_ty == IrType::Ptr { IrType::U64 } else { from_ty };
            let effective_to = if to_ty == IrType::Ptr { IrType::U64 } else { to_ty };
            if effective_from == effective_to || (effective_from.size() == 8 && effective_to.size() == 8) {
                return;
            }
            return self.emit_cast_instrs(effective_from, effective_to);
        }

        // Float-to-int cast
        if from_ty.is_float() && !to_ty.is_float() {
            let is_unsigned_dest = to_ty.is_unsigned() || to_ty == IrType::Ptr;
            if from_ty == IrType::F64 {
                self.state.emit("    fmv.d.x ft0, t0");
                if is_unsigned_dest {
                    self.state.emit("    fcvt.lu.d t0, ft0, rtz");
                } else {
                    self.state.emit("    fcvt.l.d t0, ft0, rtz");
                }
            } else {
                self.state.emit("    fmv.w.x ft0, t0");
                if is_unsigned_dest {
                    self.state.emit("    fcvt.lu.s t0, ft0, rtz");
                } else {
                    self.state.emit("    fcvt.l.s t0, ft0, rtz");
                }
            }
            return;
        }

        // Int-to-float cast
        if !from_ty.is_float() && to_ty.is_float() {
            let is_unsigned_src = from_ty.is_unsigned();
            if to_ty == IrType::F64 {
                if is_unsigned_src {
                    self.state.emit("    fcvt.d.lu ft0, t0");
                } else {
                    self.state.emit("    fcvt.d.l ft0, t0");
                }
                self.state.emit("    fmv.x.d t0, ft0");
            } else {
                if is_unsigned_src {
                    self.state.emit("    fcvt.s.lu ft0, t0");
                } else {
                    self.state.emit("    fcvt.s.l ft0, t0");
                }
                self.state.emit("    fmv.x.w t0, ft0");
            }
            return;
        }

        // Float-to-float cast
        if from_ty.is_float() && to_ty.is_float() {
            if from_ty == IrType::F32 && to_ty == IrType::F64 {
                self.state.emit("    fmv.w.x ft0, t0");
                self.state.emit("    fcvt.d.s ft0, ft0");
                self.state.emit("    fmv.x.d t0, ft0");
            } else if from_ty == IrType::F64 && to_ty == IrType::F32 {
                self.state.emit("    fmv.d.x ft0, t0");
                self.state.emit("    fcvt.s.d ft0, ft0");
                self.state.emit("    fmv.x.w t0, ft0");
            }
            return;
        }

        if from_ty.size() == to_ty.size() && (from_ty.is_integer() || from_ty == IrType::Ptr) && (to_ty.is_integer() || to_ty == IrType::Ptr) {
            return;
        }

        // Ptr is 64-bit; treat as I64/U64 for cast purposes
        let from_ty = if from_ty == IrType::Ptr { IrType::I64 } else { from_ty };
        let to_ty = if to_ty == IrType::Ptr { IrType::I64 } else { to_ty };

        let from_size = from_ty.size();
        let to_size = to_ty.size();

        if to_size > from_size {
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
        } else if to_size < from_size {
            // Narrowing: truncate and sign/zero-extend back to 64-bit
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
        // Same size: pointer <-> integer conversions (no-op on RISC-V 64)
    }
}

const RISCV_ARG_REGS: [&str; 8] = ["a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"];

impl ArchCodegen for RiscvCodegen {
    fn state(&mut self) -> &mut CodegenState { &mut self.state }
    fn state_ref(&self) -> &CodegenState { &self.state }
    fn ptr_directive(&self) -> PtrDirective { PtrDirective::Dword }

    fn calculate_stack_space(&mut self, func: &IrFunction) -> i64 {
        calculate_stack_space_common(&mut self.state, func, 16, |space, alloc_size| {
            // RISC-V uses negative offsets from s0 (frame pointer)
            let new_space = space + ((alloc_size + 7) & !7).max(8);
            (-(new_space as i64), new_space)
        })
    }

    fn aligned_frame_size(&self, raw_space: i64) -> i64 {
        (raw_space + 15) & !15
    }

    fn emit_prologue(&mut self, func: &IrFunction, frame_size: i64) {
        self.current_return_type = func.return_type;
        self.emit_prologue_riscv(frame_size);
    }

    fn emit_epilogue(&mut self, frame_size: i64) {
        self.emit_epilogue_riscv(frame_size);
    }

    fn emit_store_params(&mut self, func: &IrFunction) {
        let float_arg_regs = ["fa0", "fa1", "fa2", "fa3", "fa4", "fa5", "fa6", "fa7"];
        let mut int_reg_idx = 0usize;
        let mut float_reg_idx = 0usize;
        // Stack-passed params are at positive offsets from s0 (s0 = old sp)
        let mut stack_param_offset: i64 = 0;
        for (_i, param) in func.params.iter().enumerate() {
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
            if let Some((dest, ty)) = find_param_alloca(func, _i) {
                if let Some(slot) = self.state.get_slot(dest.0) {
                    if is_stack_passed {
                        // Stack-passed parameter: load from positive s0 offset
                        self.emit_load_from_s0("t0", stack_param_offset, "ld");
                        let store_instr = Self::store_for_type(ty);
                        self.emit_store_to_s0("t0", slot.0, store_instr);
                        stack_param_offset += 8;
                    } else if is_float {
                        // Float params arrive in fa0-fa7 per RISC-V calling convention
                        if ty == IrType::F32 {
                            // F32 param: extract 32-bit float from fa-reg
                            self.state.emit(&format!("    fmv.x.w t0, {}", float_arg_regs[float_reg_idx]));
                        } else {
                            // F64 param: extract 64-bit double from fa-reg
                            self.state.emit(&format!("    fmv.x.d t0, {}", float_arg_regs[float_reg_idx]));
                        }
                        self.emit_store_to_s0("t0", slot.0, "sd");
                        float_reg_idx += 1;
                    } else {
                        let store_instr = Self::store_for_type(ty);
                        self.emit_store_to_s0(RISCV_ARG_REGS[int_reg_idx], slot.0, store_instr);
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
        self.operand_to_t0(op);
    }

    fn emit_store_result(&mut self, dest: &Value) {
        self.store_t0_to(dest);
    }

    fn emit_store(&mut self, val: &Operand, ptr: &Value, ty: IrType) {
        self.operand_to_t0(val);
        if let Some(slot) = self.state.get_slot(ptr.0) {
            if self.state.is_alloca(ptr.0) {
                let store_instr = Self::store_for_type(ty);
                self.emit_store_to_s0("t0", slot.0, store_instr);
            } else {
                self.state.emit("    mv t3, t0");
                self.emit_load_from_s0("t4", slot.0, "ld");
                let store_instr = Self::store_for_type(ty);
                self.state.emit(&format!("    {} t3, 0(t4)", store_instr));
            }
        }
    }

    fn emit_load(&mut self, dest: &Value, ptr: &Value, ty: IrType) {
        if let Some(slot) = self.state.get_slot(ptr.0) {
            if self.state.is_alloca(ptr.0) {
                let load_instr = Self::load_for_type(ty);
                self.emit_load_from_s0("t0", slot.0, load_instr);
            } else {
                self.emit_load_from_s0("t0", slot.0, "ld");
                let load_instr = Self::load_for_type(ty);
                self.state.emit(&format!("    {} t0, 0(t0)", load_instr));
            }
            self.store_t0_to(dest);
        }
    }

    fn emit_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if ty.is_float() {
            self.operand_to_t0(lhs);
            self.state.emit("    mv t1, t0");
            self.operand_to_t0(rhs);
            self.state.emit("    mv t2, t0");
            if ty == IrType::F64 {
                self.state.emit("    fmv.d.x ft0, t1");
                self.state.emit("    fmv.d.x ft1, t2");
                match op {
                    IrBinOp::Add => self.state.emit("    fadd.d ft0, ft0, ft1"),
                    IrBinOp::Sub => self.state.emit("    fsub.d ft0, ft0, ft1"),
                    IrBinOp::Mul => self.state.emit("    fmul.d ft0, ft0, ft1"),
                    IrBinOp::SDiv | IrBinOp::UDiv => self.state.emit("    fdiv.d ft0, ft0, ft1"),
                    _ => self.state.emit("    fadd.d ft0, ft0, ft1"),
                }
                self.state.emit("    fmv.x.d t0, ft0");
            } else {
                self.state.emit("    fmv.w.x ft0, t1");
                self.state.emit("    fmv.w.x ft1, t2");
                match op {
                    IrBinOp::Add => self.state.emit("    fadd.s ft0, ft0, ft1"),
                    IrBinOp::Sub => self.state.emit("    fsub.s ft0, ft0, ft1"),
                    IrBinOp::Mul => self.state.emit("    fmul.s ft0, ft0, ft1"),
                    IrBinOp::SDiv | IrBinOp::UDiv => self.state.emit("    fdiv.s ft0, ft0, ft1"),
                    _ => self.state.emit("    fadd.s ft0, ft0, ft1"),
                }
                self.state.emit("    fmv.x.w t0, ft0");
            }
            self.store_t0_to(dest);
            return;
        }

        self.operand_to_t0(lhs);
        self.state.emit("    mv t1, t0");
        self.operand_to_t0(rhs);
        self.state.emit("    mv t2, t0");

        let use_32bit = ty == IrType::I32 || ty == IrType::U32;

        if use_32bit {
            match op {
                IrBinOp::Add => self.state.emit("    addw t0, t1, t2"),
                IrBinOp::Sub => self.state.emit("    subw t0, t1, t2"),
                IrBinOp::Mul => self.state.emit("    mulw t0, t1, t2"),
                IrBinOp::SDiv => self.state.emit("    divw t0, t1, t2"),
                IrBinOp::UDiv => self.state.emit("    divuw t0, t1, t2"),
                IrBinOp::SRem => self.state.emit("    remw t0, t1, t2"),
                IrBinOp::URem => self.state.emit("    remuw t0, t1, t2"),
                IrBinOp::And => self.state.emit("    and t0, t1, t2"),
                IrBinOp::Or => self.state.emit("    or t0, t1, t2"),
                IrBinOp::Xor => self.state.emit("    xor t0, t1, t2"),
                IrBinOp::Shl => self.state.emit("    sllw t0, t1, t2"),
                IrBinOp::AShr => self.state.emit("    sraw t0, t1, t2"),
                IrBinOp::LShr => self.state.emit("    srlw t0, t1, t2"),
            }
        } else {
            match op {
                IrBinOp::Add => self.state.emit("    add t0, t1, t2"),
                IrBinOp::Sub => self.state.emit("    sub t0, t1, t2"),
                IrBinOp::Mul => self.state.emit("    mul t0, t1, t2"),
                IrBinOp::SDiv => self.state.emit("    div t0, t1, t2"),
                IrBinOp::UDiv => self.state.emit("    divu t0, t1, t2"),
                IrBinOp::SRem => self.state.emit("    rem t0, t1, t2"),
                IrBinOp::URem => self.state.emit("    remu t0, t1, t2"),
                IrBinOp::And => self.state.emit("    and t0, t1, t2"),
                IrBinOp::Or => self.state.emit("    or t0, t1, t2"),
                IrBinOp::Xor => self.state.emit("    xor t0, t1, t2"),
                IrBinOp::Shl => self.state.emit("    sll t0, t1, t2"),
                IrBinOp::AShr => self.state.emit("    sra t0, t1, t2"),
                IrBinOp::LShr => self.state.emit("    srl t0, t1, t2"),
            }
        }

        self.store_t0_to(dest);
    }

    fn emit_unaryop(&mut self, dest: &Value, op: IrUnaryOp, src: &Operand, ty: IrType) {
        self.operand_to_t0(src);
        if ty.is_float() {
            match op {
                IrUnaryOp::Neg => {
                    if ty == IrType::F64 {
                        self.state.emit("    fmv.d.x ft0, t0");
                        self.state.emit("    fneg.d ft0, ft0");
                        self.state.emit("    fmv.x.d t0, ft0");
                    } else {
                        self.state.emit("    fmv.w.x ft0, t0");
                        self.state.emit("    fneg.s ft0, ft0");
                        self.state.emit("    fmv.x.w t0, ft0");
                    }
                }
                IrUnaryOp::Not => self.state.emit("    not t0, t0"),
            }
        } else {
            match op {
                IrUnaryOp::Neg => self.state.emit("    neg t0, t0"),
                IrUnaryOp::Not => self.state.emit("    not t0, t0"),
            }
        }
        self.store_t0_to(dest);
    }

    fn emit_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        self.operand_to_t0(lhs);
        self.state.emit("    mv t1, t0");
        self.operand_to_t0(rhs);
        self.state.emit("    mv t2, t0");

        if ty.is_float() {
            if ty == IrType::F64 {
                self.state.emit("    fmv.d.x ft0, t1");
                self.state.emit("    fmv.d.x ft1, t2");
                match op {
                    IrCmpOp::Eq => self.state.emit("    feq.d t0, ft0, ft1"),
                    IrCmpOp::Ne => {
                        self.state.emit("    feq.d t0, ft0, ft1");
                        self.state.emit("    xori t0, t0, 1");
                    }
                    IrCmpOp::Slt | IrCmpOp::Ult => self.state.emit("    flt.d t0, ft0, ft1"),
                    IrCmpOp::Sle | IrCmpOp::Ule => self.state.emit("    fle.d t0, ft0, ft1"),
                    IrCmpOp::Sgt | IrCmpOp::Ugt => self.state.emit("    flt.d t0, ft1, ft0"),
                    IrCmpOp::Sge | IrCmpOp::Uge => self.state.emit("    fle.d t0, ft1, ft0"),
                }
            } else {
                self.state.emit("    fmv.w.x ft0, t1");
                self.state.emit("    fmv.w.x ft1, t2");
                match op {
                    IrCmpOp::Eq => self.state.emit("    feq.s t0, ft0, ft1"),
                    IrCmpOp::Ne => {
                        self.state.emit("    feq.s t0, ft0, ft1");
                        self.state.emit("    xori t0, t0, 1");
                    }
                    IrCmpOp::Slt | IrCmpOp::Ult => self.state.emit("    flt.s t0, ft0, ft1"),
                    IrCmpOp::Sle | IrCmpOp::Ule => self.state.emit("    fle.s t0, ft0, ft1"),
                    IrCmpOp::Sgt | IrCmpOp::Ugt => self.state.emit("    flt.s t0, ft1, ft0"),
                    IrCmpOp::Sge | IrCmpOp::Uge => self.state.emit("    fle.s t0, ft1, ft0"),
                }
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
                 is_variadic: bool) {
        let float_arg_regs = ["fa0", "fa1", "fa2", "fa3", "fa4", "fa5", "fa6", "fa7"];
        let mut int_idx = 0usize;
        let mut float_idx = 0usize;

        for (i, arg) in args.iter().enumerate() {
            let is_float_arg = if i < arg_types.len() {
                arg_types[i].is_float()
            } else {
                matches!(arg, Operand::Const(IrConst::F32(_) | IrConst::F64(_)))
            };
            // For variadic functions, ALL arguments go through integer registers (a0-a7)
            if is_float_arg && !is_variadic && float_idx < 8 {
                self.operand_to_t0(arg);
                let arg_ty = if i < arg_types.len() { Some(arg_types[i]) } else { None };
                if arg_ty == Some(IrType::F32) {
                    self.state.emit(&format!("    fmv.w.x {}, t0", float_arg_regs[float_idx]));
                } else {
                    self.state.emit(&format!("    fmv.d.x {}, t0", float_arg_regs[float_idx]));
                }
                float_idx += 1;
            } else if int_idx < 8 {
                self.operand_to_t0(arg);
                self.state.emit(&format!("    mv {}, t0", RISCV_ARG_REGS[int_idx]));
                int_idx += 1;
            }
        }

        if let Some(name) = direct_name {
            self.state.emit(&format!("    call {}", name));
        } else if let Some(ptr) = func_ptr {
            self.operand_to_t0(ptr);
            self.state.emit("    mv t2, t0");
            self.state.emit("    jalr ra, t2, 0");
        }

        if let Some(dest) = dest {
            if let Some(slot) = self.state.get_slot(dest.0) {
                if return_type == IrType::F32 {
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

    fn emit_gep(&mut self, dest: &Value, base: &Value, offset: &Operand) {
        if let Some(slot) = self.state.get_slot(base.0) {
            if self.state.is_alloca(base.0) {
                self.emit_addi_s0("t1", slot.0);
            } else {
                self.emit_load_from_s0("t1", slot.0, "ld");
            }
        }
        self.operand_to_t0(offset);
        self.state.emit("    add t0, t1, t0");
        self.store_t0_to(dest);
    }

    fn emit_cast(&mut self, dest: &Value, src: &Operand, from_ty: IrType, to_ty: IrType) {
        self.operand_to_t0(src);
        self.emit_cast_instrs(from_ty, to_ty);
        self.store_t0_to(dest);
    }

    fn emit_memcpy(&mut self, dest: &Value, src: &Value, size: usize) {
        // Load dest address into t1, src address into t2
        if let Some(dst_slot) = self.state.get_slot(dest.0) {
            if self.state.is_alloca(dest.0) {
                self.emit_addi_s0("t1", dst_slot.0);
            } else {
                self.emit_load_from_s0("t1", dst_slot.0, "ld");
            }
        }
        if let Some(src_slot) = self.state.get_slot(src.0) {
            if self.state.is_alloca(src.0) {
                self.emit_addi_s0("t2", src_slot.0);
            } else {
                self.emit_load_from_s0("t2", src_slot.0, "ld");
            }
        }
        // Inline byte-by-byte copy using a loop
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

    fn emit_return(&mut self, val: Option<&Operand>, frame_size: i64) {
        if let Some(val) = val {
            self.operand_to_t0(val);
            if self.current_return_type == IrType::F32 {
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

    fn emit_branch(&mut self, label: &str) {
        self.state.emit(&format!("    j {}", label));
    }

    fn emit_cond_branch(&mut self, cond: &Operand, true_label: &str, false_label: &str) {
        self.operand_to_t0(cond);
        self.state.emit(&format!("    bnez t0, {}", true_label));
        self.state.emit(&format!("    j {}", false_label));
    }

    fn emit_unreachable(&mut self) {
        self.state.emit("    ebreak");
    }
}

impl Default for RiscvCodegen {
    fn default() -> Self {
        Self::new()
    }
}
