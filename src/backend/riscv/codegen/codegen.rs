use crate::ir::ir::*;
use crate::common::types::IrType;
use crate::backend::common::PtrDirective;
use crate::backend::codegen_shared::*;

/// Constraint classification for RISC-V inline asm operands.
#[derive(Clone, PartialEq)]
enum RvConstraintKind {
    GpReg,           // "r" - general purpose register
    FpReg,           // "f" - floating point register
    Memory,          // "m" - memory (offset(s0))
    Address,         // "A" - address for AMO instructions (produces (reg) format)
    Immediate,       // "I", "i" - immediate value
    ZeroOrReg,       // "J" in "rJ" - zero register or GP reg
    Specific(String),// specific register name
    Tied(usize),     // tied to output operand N
}

/// Classify a RISC-V inline asm constraint string into its kind.
fn classify_rv_constraint(constraint: &str) -> RvConstraintKind {
    let c = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
    // Check for tied operand (all digits)
    if !c.is_empty() && c.chars().all(|ch| ch.is_ascii_digit()) {
        if let Ok(n) = c.parse::<usize>() {
            return RvConstraintKind::Tied(n);
        }
    }
    match c {
        "m" => RvConstraintKind::Memory,
        "A" => RvConstraintKind::Address,
        "f" => RvConstraintKind::FpReg,
        "I" | "i" | "n" => RvConstraintKind::Immediate,
        "J" => RvConstraintKind::ZeroOrReg,
        "rJ" => RvConstraintKind::ZeroOrReg,
        "a0" | "a1" | "a2" | "a3" | "a4" | "a5" | "a6" | "a7"
        | "ra" | "t0" | "t1" | "t2" => RvConstraintKind::Specific(c.to_string()),
        _ if c.starts_with("ft") || c.starts_with("fa") || c.starts_with("fs") => {
            RvConstraintKind::Specific(c.to_string())
        }
        _ => RvConstraintKind::GpReg,
    }
}

/// RISC-V 64 code generator. Implements the ArchCodegen trait for the shared framework.
/// Uses standard RISC-V calling convention with stack-based allocation.
pub struct RiscvCodegen {
    state: CodegenState,
    current_return_type: IrType,
    /// For variadic functions: offset from SP where the register save area starts.
    va_save_area_offset: i64,
    /// Number of named integer params for current variadic function.
    va_named_gp_count: usize,
    /// Current frame size.
    current_frame_size: i64,
}

impl RiscvCodegen {
    pub fn new() -> Self {
        Self {
            state: CodegenState::new(),
            current_return_type: IrType::I64,
            va_save_area_offset: 0,
            va_named_gp_count: 0,
            current_frame_size: 0,
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
    ///
    /// Stack layout (s0 points to top of frame = old sp):
    ///   s0 - 8:  saved ra
    ///   s0 - 16: saved s0
    ///   s0 - 16 - ...: local data (allocas and value slots)
    ///   sp: bottom of frame
    fn emit_prologue_riscv(&mut self, frame_size: i64) {
        // Small-frame path requires ALL immediates to fit in 12 bits:
        // -frame_size (sp adjust), frame_size-8 and frame_size-16 (save offsets),
        // and frame_size (s0 setup). Since fits_imm12 checks [-2048, 2047],
        // we check both -frame_size AND frame_size.
        if Self::fits_imm12(-frame_size) && Self::fits_imm12(frame_size) {
            // Small frame: all offsets fit in 12-bit immediates
            self.state.emit(&format!("    addi sp, sp, -{}", frame_size));
            self.state.emit(&format!("    sd ra, {}(sp)", frame_size - 8));
            self.state.emit(&format!("    sd s0, {}(sp)", frame_size - 16));
            self.state.emit(&format!("    addi s0, sp, {}", frame_size));
        } else {
            // Large frame: save ra/s0 at top of frame (s0-8, s0-16) to avoid
            // collision with local data that grows downward from s0-16.
            self.state.emit(&format!("    li t0, {}", frame_size));
            self.state.emit("    sub sp, sp, t0");
            // t0 still has frame_size; compute s0 = sp + frame_size = old_sp
            self.state.emit("    add t0, sp, t0");
            // Save ra and old s0 at top of frame (relative to new s0)
            self.state.emit("    sd ra, -8(t0)");
            self.state.emit("    sd s0, -16(t0)");
            self.state.emit("    mv s0, t0");
        }
    }

    /// Emit epilogue: restore ra/s0 and deallocate stack.
    fn emit_epilogue_riscv(&mut self, frame_size: i64) {
        if Self::fits_imm12(-frame_size) && Self::fits_imm12(frame_size) {
            // Small frame: restore from known sp offsets
            self.state.emit(&format!("    ld ra, {}(sp)", frame_size - 8));
            self.state.emit(&format!("    ld s0, {}(sp)", frame_size - 16));
            self.state.emit(&format!("    addi sp, sp, {}", frame_size));
        } else {
            // Large frame: restore from s0-relative offsets (always fit in imm12).
            // Load saved values before adjusting sp to avoid reading below sp.
            self.state.emit("    ld ra, -8(s0)");
            self.state.emit("    ld t0, -16(s0)");
            self.state.emit("    mv sp, s0");
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

    /// Emit a type cast instruction sequence for RISC-V 64.
    /// Emit RISC-V instructions for a type cast, using shared cast classification.
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

            CastKind::SignedToUnsignedSameSize { .. } => {
                // On RISC-V 64, same-size signed->unsigned is a noop
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
}

const RISCV_ARG_REGS: [&str; 8] = ["a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"];

impl ArchCodegen for RiscvCodegen {
    fn state(&mut self) -> &mut CodegenState { &mut self.state }
    fn state_ref(&self) -> &CodegenState { &self.state }
    fn ptr_directive(&self) -> PtrDirective { PtrDirective::Dword }

    fn calculate_stack_space(&mut self, func: &IrFunction) -> i64 {
        let mut space = calculate_stack_space_common(&mut self.state, func, 16, |space, alloc_size| {
            // RISC-V uses negative offsets from s0 (frame pointer)
            let new_space = space + ((alloc_size + 7) & !7).max(8);
            (-(new_space as i64), new_space)
        });

        // For variadic functions, reserve space for the register save area (a0-a7 = 64 bytes)
        if func.is_variadic {
            space = (space + 7) & !7; // align
            self.va_save_area_offset = space;
            space += 64; // 8 registers * 8 bytes

            // Count named params. On RISC-V, all params in variadic functions
            // go through integer registers (a0-a7), including named float params.
            self.va_named_gp_count = func.params.len().min(8);
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
        // For variadic functions: save all integer register args (a0-a7) to the save area.
        // Layout: a0 at lowest offset, a7 at highest offset, so va_arg can advance by +8.
        // save_area starts at -(va_save_area_offset + 64) and ends at -(va_save_area_offset + 8).
        if func.is_variadic {
            for i in 0..8usize {
                let offset = -(self.va_save_area_offset as i64) - 64 + (i as i64) * 8;
                self.emit_store_to_s0(RISCV_ARG_REGS[i], offset, "sd");
            }
        }

        let float_arg_regs = ["fa0", "fa1", "fa2", "fa3", "fa4", "fa5", "fa6", "fa7"];
        let mut int_reg_idx = 0usize;
        let mut float_reg_idx = 0usize;
        // Stack-passed params are at positive offsets from s0 (s0 = old sp)
        let mut stack_param_offset: i64 = 0;

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
            let is_float = param.ty.is_float() && !is_long_double;
            // RISC-V LP64D ABI: FP args go to fa0-fa7, then spill to a0-a7 (GPRs),
            // then to the stack. This matches the caller's classification in emit_call.
            let is_float_in_gpr = is_float && float_reg_idx >= 8 && int_reg_idx < 8;
            let is_stack_passed = if is_long_double {
                // F128 needs an aligned pair of GP regs
                let aligned = (int_reg_idx + 1) & !1; // align to even
                aligned + 1 >= 8
            } else if is_float {
                // FP args spill to GPRs first, then to stack
                float_reg_idx >= 8 && int_reg_idx >= 8
            } else {
                int_reg_idx >= 8
            };

            if param.name.is_empty() {
                if is_stack_passed {
                    if is_long_double {
                        stack_param_offset = (stack_param_offset + 15) & !15;
                        stack_param_offset += 16;
                        int_reg_idx = 8; // consumed all GP regs
                    } else {
                        stack_param_offset += 8;
                    }
                } else if is_long_double {
                    // Align to even GP register
                    if int_reg_idx % 2 != 0 { int_reg_idx += 1; }
                    int_reg_idx += 2;
                } else if is_float_in_gpr {
                    // FP arg spilled to GPR - consume a GPR slot
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
                    if is_long_double && !is_stack_passed {
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
                        // F128 stack-passed: 16 bytes, 16-byte aligned
                        // Stack params are at positive offsets from s0, but if we pushed
                        // the save area, add 64 to account for it.
                        let extra = if has_f128_reg_params && !func.is_variadic { 64 } else { 0 };
                        let adj_offset = stack_param_offset + extra;
                        stack_param_offset = (stack_param_offset + 15) & !15;
                        // Load the f128 from stack and call __trunctfdf2
                        self.emit_load_from_s0(
                            "a0",
                            (adj_offset + 15) & !15,
                            "ld",
                        );
                        self.emit_load_from_s0(
                            "a1",
                            ((adj_offset + 15) & !15) + 8,
                            "ld",
                        );
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
                        if has_f128_reg_params && !func.is_variadic {
                            let off = f128_save_offset + (int_reg_idx as i64) * 8;
                            self.state.emit(&format!("    ld t0, {}(sp)", off));
                        } else {
                            self.state.emit(&format!("    mv t0, {}", RISCV_ARG_REGS[int_reg_idx]));
                        }
                        self.emit_store_to_s0("t0", slot.0, "sd");
                        int_reg_idx += 1;
                    } else if is_float {
                        // Float params arrive in fa0-fa7 per RISC-V calling convention
                        if has_f128_reg_params && !func.is_variadic {
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
                        if has_f128_reg_params && !func.is_variadic {
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
            let float_op = classify_float_binop(op).unwrap_or(FloatOp::Add);
            let mnemonic = match float_op {
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
                    // F128 uses F64 instructions (long double computed at double precision)
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
                IrUnaryOp::Not => self.state.emit("    not t0, t0"),
                _ => {} // Clz/Ctz/Bswap/Popcount not applicable to floats
            }
        } else {
            match op {
                IrUnaryOp::Neg => self.state.emit("    neg t0, t0"),
                IrUnaryOp::Not => self.state.emit("    not t0, t0"),
                IrUnaryOp::Clz => {
                    self.emit_clz(ty);
                }
                IrUnaryOp::Ctz => {
                    self.emit_ctz(ty);
                }
                IrUnaryOp::Bswap => {
                    self.emit_bswap(ty);
                }
                IrUnaryOp::Popcount => {
                    self.emit_popcount(ty);
                }
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
            // F128 uses F64 instructions (long double computed at double precision)
            if ty == IrType::F64 || ty == IrType::F128 {
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
                 is_variadic: bool, _num_fixed_args: usize) {
        let float_arg_regs = ["fa0", "fa1", "fa2", "fa3", "fa4", "fa5", "fa6", "fa7"];

        // Phase 1: Classify all args into register assignments or stack overflow.
        // 'I' = GP register, 'F' = FP register, 'Q' = F128 GP register pair, 'S' = stack, 'T' = F128 stack
        let mut arg_classes: Vec<char> = Vec::new();
        let mut arg_int_indices: Vec<usize> = Vec::new(); // GP reg index for 'I' or base index for 'Q'
        let mut arg_fp_indices: Vec<usize> = Vec::new();  // FP reg index for 'F'
        let mut int_idx = 0usize;
        let mut float_idx = 0usize;

        for (i, _arg) in args.iter().enumerate() {
            let is_long_double = if i < arg_types.len() {
                arg_types[i].is_long_double()
            } else {
                false
            };
            let is_float_arg = if i < arg_types.len() {
                arg_types[i].is_float()
            } else {
                matches!(args[i], Operand::Const(IrConst::F32(_) | IrConst::F64(_)))
            };

            if is_long_double {
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
            self.state.emit(&format!("    addi sp, sp, -{}", f128_temp_space));
            let mut temp_offset: i64 = 0;
            for item in &mut f128_var_temps {
                item.1 = temp_offset;
                let arg = &args[item.0];
                // Load f64 value, call __extenddftf2, save result
                self.operand_to_t0(arg);
                self.state.emit("    fmv.d.x fa0, t0");
                self.state.emit("    call __extenddftf2");
                // Save a0:a1 to temp space
                self.state.emit(&format!("    sd a0, {}(sp)", temp_offset));
                self.state.emit(&format!("    sd a1, {}(sp)", temp_offset + 8));
                temp_offset += 16;
            }
        }

        // Phase 3: Handle stack overflow args.
        let stack_args: Vec<(usize, bool)> = args.iter().enumerate()
            .filter(|(i, _)| arg_classes[*i] == 'S' || arg_classes[*i] == 'T')
            .map(|(i, _)| (i, arg_classes[i] == 'T'))
            .collect();

        let mut stack_arg_space: usize = 0;
        if !stack_args.is_empty() {
            for &(_, is_f128) in &stack_args {
                if is_f128 {
                    stack_arg_space = (stack_arg_space + 15) & !15;
                    stack_arg_space += 16;
                } else {
                    stack_arg_space += 8;
                }
            }
            stack_arg_space = (stack_arg_space + 15) & !15;
            self.state.emit(&format!("    addi sp, sp, -{}", stack_arg_space));
            let mut offset: usize = 0;
            for &(arg_i, is_f128) in &stack_args {
                if is_f128 {
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
                            self.state.emit(&format!("    sd t0, {}(sp)", offset));
                            self.state.emit(&format!("    li t0, {}", hi));
                            self.state.emit(&format!("    sd t0, {}(sp)", offset + 8));
                        }
                        Operand::Value(ref _v) => {
                            self.operand_to_t0(&args[arg_i]);
                            self.state.emit("    fmv.d.x fa0, t0");
                            self.state.emit("    call __extenddftf2");
                            self.state.emit(&format!("    sd a0, {}(sp)", offset));
                            self.state.emit(&format!("    sd a1, {}(sp)", offset + 8));
                        }
                    }
                    offset += 16;
                } else {
                    self.operand_to_t0(&args[arg_i]);
                    self.state.emit(&format!("    sd t0, {}(sp)", offset));
                    offset += 8;
                }
            }
        }

        // Phase 4: Load non-F128 register args.
        // Load all non-F128 args into their target registers.
        // Process GP args into t3-t6 first (temp), then FP args, then move GP from temp.
        let mut gp_temps: Vec<(usize, &str)> = Vec::new(); // (target_reg_idx, temp_reg)
        let temp_regs = ["t3", "t4", "t5", "t6", "s2", "s3", "s4", "s5"];
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
                    self.state.emit(&format!("    ld {}, {}(sp)", RISCV_ARG_REGS[base_reg], offset));
                    self.state.emit(&format!("    ld {}, {}(sp)", RISCV_ARG_REGS[base_reg + 1], offset + 8));
                }
            }
        }

        // Clean up F128 temp space before the call (only if no stack overflow args below it)
        if f128_temp_space > 0 && stack_arg_space == 0 {
            self.state.emit(&format!("    addi sp, sp, {}", f128_temp_space));
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
            let cleanup = stack_arg_space + f128_temp_space as usize;
            self.state.emit(&format!("    addi sp, sp, {}", cleanup));
        }

        if let Some(dest) = dest {
            if let Some(slot) = self.state.get_slot(dest.0) {
                if return_type.is_long_double() {
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
        // For variadic functions, we save a0-a7 to the register save area.
        // The variadic args start after the named GP params.
        if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
            if self.state.is_alloca(va_list_ptr.0) {
                self.emit_addi_s0("t0", slot.0);
            } else {
                self.emit_load_from_s0("t0", slot.0, "ld");
            }
        }

        if self.va_named_gp_count < 8 {
            // Variadic args start in the register save area, after named params.
            // a0 is at -(va_save_area_offset + 64), a1 at -(va_save_area_offset + 56), etc.
            // Variadic args start at a[named_gp_count], which is at:
            let vararg_offset = -(self.va_save_area_offset as i64) - 64 + (self.va_named_gp_count as i64) * 8;
            self.emit_addi_s0("t1", vararg_offset);
        } else {
            // All registers used by named params; variadic args are on the caller's stack.
            // s0 points to the old sp, and stack-passed args start at s0 + 0 (the return
            // address slot in the caller's frame is above, args are at positive offsets).
            self.state.emit("    mv t1, s0");
        }
        self.state.emit("    sd t1, 0(t0)");
    }

    fn emit_va_end(&mut self, _va_list_ptr: &Value) {
        // va_end is a no-op on RISC-V
    }

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

    fn emit_label_addr(&mut self, dest: &Value, label: &str) {
        // Load address of a label for computed goto (GCC &&label extension)
        self.state.emit(&format!("    la t0, {}", label));
        self.store_t0_to(dest);
    }

    fn emit_indirect_branch(&mut self, target: &Operand) {
        // Computed goto: goto *target
        self.operand_to_t0(target);
        self.state.emit("    jr t0");
    }

    fn emit_atomic_rmw(&mut self, dest: &Value, op: AtomicRmwOp, ptr: &Operand, val: &Operand, ty: IrType, ordering: AtomicOrdering) {
        // Load ptr into t1, val into t2
        self.operand_to_t0(ptr);
        self.state.emit("    mv t1, t0"); // t1 = ptr
        self.operand_to_t0(val);
        self.state.emit("    mv t2, t0"); // t2 = val

        let aq_rl = Self::amo_ordering(ordering);
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
                let label_id = self.state.next_label_id();
                let loop_label = format!(".Latomic_{}", label_id);
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
        let suffix = Self::amo_width_suffix(ty);

        let label_id = self.state.next_label_id();
        let loop_label = format!(".Lcas_loop_{}", label_id);
        let fail_label = format!(".Lcas_fail_{}", label_id);
        let done_label = format!(".Lcas_done_{}", label_id);

        self.state.emit(&format!("{}:", loop_label));
        self.state.emit(&format!("    lr.{}{} t0, (t1)", suffix, aq_rl));
        // Mask for sub-word comparison
        Self::mask_for_cmp(&mut self.state, ty);
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
        self.store_t0_to(dest);
    }

    fn emit_atomic_load(&mut self, dest: &Value, ptr: &Operand, ty: IrType, _ordering: AtomicOrdering) {
        self.operand_to_t0(ptr);
        // Use lr for atomic load (conservative but correct)
        let suffix = Self::amo_width_suffix(ty);
        self.state.emit(&format!("    lr.{}.aq t0, (t0)", suffix));
        Self::sign_extend_riscv(&mut self.state, ty);
        self.store_t0_to(dest);
    }

    fn emit_atomic_store(&mut self, ptr: &Operand, val: &Operand, ty: IrType, ordering: AtomicOrdering) {
        self.operand_to_t0(val);
        self.state.emit("    mv t1, t0"); // t1 = val
        self.operand_to_t0(ptr);
        // Use amoswap with zero dest for atomic store
        let aq_rl = Self::amo_ordering(ordering);
        let suffix = Self::amo_width_suffix(ty);
        self.state.emit(&format!("    amoswap.{}{} zero, t1, (t0)", suffix, aq_rl));
    }

    fn emit_fence(&mut self, _ordering: AtomicOrdering) {
        self.state.emit("    fence rw, rw");
    }

    fn emit_inline_asm(&mut self, template: &str, outputs: &[(String, Value, Option<String>)], inputs: &[(String, Operand, Option<String>)], _clobbers: &[String], _operand_types: &[IrType]) {
        // RISC-V inline assembly support.
        // Handles: r (GP reg), f (FP reg), A (address for AMO/LR/SC),
        // m (memory), I/i (immediate), J (zero constant), rJ (reg or zero),
        // tied operands (0, 1, ...), specific regs (a0-a7, t0-t2, ra).

        let gp_scratch: &[&str] = &["t0", "t1", "t2", "t3", "t4", "t5", "t6", "a2", "a3", "a4", "a5", "a6", "a7"];
        let fp_scratch: &[&str] = &["ft0", "ft1", "ft2", "ft3", "ft4", "ft5", "ft6", "ft7"];
        let mut scratch_idx = 0;
        let mut fp_scratch_idx = 0;

        let total_operands = outputs.len() + inputs.len();
        let mut op_regs: Vec<String> = vec![String::new(); total_operands];
        let mut op_kinds: Vec<RvConstraintKind> = vec![RvConstraintKind::GpReg; total_operands];
        let mut op_names: Vec<Option<String>> = vec![None; total_operands];
        let mut op_mem_offsets: Vec<i64> = vec![0; total_operands];
        // For immediate operands, store the constant value
        let mut op_imm_values: Vec<Option<i64>> = vec![None; total_operands];

        // First pass: classify output constraints
        for (i, (constraint, ptr, name)) in outputs.iter().enumerate() {
            op_names[i] = name.clone();
            let kind = classify_rv_constraint(constraint);
            match &kind {
                RvConstraintKind::Memory => {
                    if let Some(slot) = self.state.get_slot(ptr.0) {
                        op_mem_offsets[i] = slot.0;
                    }
                }
                RvConstraintKind::Address => {
                    // Address constraint: we load the address into a GP register
                    // but format it as (reg) during substitution
                    if let Some(slot) = self.state.get_slot(ptr.0) {
                        op_mem_offsets[i] = slot.0;
                    }
                }
                RvConstraintKind::Specific(reg) => {
                    op_regs[i] = reg.clone();
                }
                _ => {}
            }
            op_kinds[i] = kind;
        }

        // First pass: classify input constraints
        for (i, (constraint, val, name)) in inputs.iter().enumerate() {
            let op_idx = outputs.len() + i;
            op_names[op_idx] = name.clone();
            let kind = classify_rv_constraint(constraint);
            match &kind {
                RvConstraintKind::Memory => {
                    if let Operand::Value(v) = val {
                        if let Some(slot) = self.state.get_slot(v.0) {
                            op_mem_offsets[op_idx] = slot.0;
                        }
                    }
                }
                RvConstraintKind::Address => {
                    if let Operand::Value(v) = val {
                        if let Some(slot) = self.state.get_slot(v.0) {
                            op_mem_offsets[op_idx] = slot.0;
                        }
                    }
                }
                RvConstraintKind::Specific(reg) => {
                    op_regs[op_idx] = reg.clone();
                }
                RvConstraintKind::Immediate => {
                    // Store the immediate value for direct substitution
                    if let Operand::Const(c) = val {
                        op_imm_values[op_idx] = Some(c.to_i64().unwrap_or(0));
                    } else {
                        // Value operand for immediate constraint: fall back to GP register
                        op_kinds[op_idx] = RvConstraintKind::GpReg;
                    }
                }
                RvConstraintKind::ZeroOrReg => {
                    // If the value is a constant 0, use "zero" register
                    if let Operand::Const(c) = val {
                        if c.to_i64() == Some(0) {
                            op_regs[op_idx] = "zero".to_string();
                        }
                    }
                }
                _ => {}
            }
            op_kinds[op_idx] = kind;
        }

        // Second pass: assign scratch registers for operands that need them
        for i in 0..total_operands {
            if !op_regs[i].is_empty() {
                continue; // Already assigned (specific or zero)
            }
            match &op_kinds[i] {
                RvConstraintKind::Memory | RvConstraintKind::Immediate => continue,
                RvConstraintKind::Tied(_) => continue, // Resolve later
                RvConstraintKind::FpReg => {
                    if fp_scratch_idx < fp_scratch.len() {
                        op_regs[i] = fp_scratch[fp_scratch_idx].to_string();
                        fp_scratch_idx += 1;
                    }
                }
                RvConstraintKind::Address => {
                    // Address operands need a GP register to hold the address
                    if scratch_idx < gp_scratch.len() {
                        op_regs[i] = gp_scratch[scratch_idx].to_string();
                        scratch_idx += 1;
                    }
                }
                RvConstraintKind::GpReg | RvConstraintKind::ZeroOrReg | RvConstraintKind::Specific(_) => {
                    if scratch_idx < gp_scratch.len() {
                        op_regs[i] = gp_scratch[scratch_idx].to_string();
                        scratch_idx += 1;
                    } else {
                        op_regs[i] = format!("s{}", 2 + scratch_idx - gp_scratch.len());
                        scratch_idx += 1;
                    }
                }
            }
        }

        // Third pass: resolve tied operands
        for i in 0..total_operands {
            if let RvConstraintKind::Tied(tied_to) = op_kinds[i].clone() {
                if tied_to < op_regs.len() {
                    op_regs[i] = op_regs[tied_to].clone();
                    // Inherit the kind for substitution purposes
                    if op_kinds[tied_to] == RvConstraintKind::Address {
                        op_kinds[i] = RvConstraintKind::Address;
                        op_mem_offsets[i] = op_mem_offsets[tied_to];
                    }
                }
            }
        }

        // Handle "+" read-write: synthetic inputs share register with their output
        let num_plus = outputs.iter().filter(|(c,_,_)| c.contains('+')).count();
        let mut plus_idx = 0;
        for (i, (constraint, _, _)) in outputs.iter().enumerate() {
            if constraint.contains('+') {
                let plus_input_idx = outputs.len() + plus_idx;
                if plus_input_idx < total_operands {
                    op_regs[plus_input_idx] = op_regs[i].clone();
                    op_kinds[plus_input_idx] = op_kinds[i].clone();
                    op_mem_offsets[plus_input_idx] = op_mem_offsets[i];
                }
                plus_idx += 1;
            }
        }

        // Build GCC operand number → internal index mapping.
        let num_plus = outputs.iter().filter(|(c,_,_)| c.contains('+')).count();
        let num_gcc_operands = outputs.len() + (inputs.len() - num_plus);
        let mut gcc_to_internal: Vec<usize> = Vec::with_capacity(num_gcc_operands);
        for i in 0..outputs.len() {
            gcc_to_internal.push(i);
        }
        for i in num_plus..inputs.len() {
            gcc_to_internal.push(outputs.len() + i);
        }

        // Phase 2: Load input values into their assigned registers
        for (i, (constraint, val, _)) in inputs.iter().enumerate() {
            let op_idx = outputs.len() + i;
            match &op_kinds[op_idx] {
                RvConstraintKind::Memory | RvConstraintKind::Immediate => continue,
                _ => {}
            }
            let reg = &op_regs[op_idx];
            if reg.is_empty() {
                continue;
            }

            let is_fp = op_kinds[op_idx] == RvConstraintKind::FpReg;
            let is_addr = op_kinds[op_idx] == RvConstraintKind::Address;

            match val {
                Operand::Const(c) => {
                    if is_fp {
                        // Load float constant: li to temp GP, then fmv to FP reg
                        let imm = c.to_i64().unwrap_or(0);
                        self.state.emit(&format!("    li t6, {}", imm));
                        if constraint.contains('f') && !constraint.contains("64") {
                            self.state.emit(&format!("    fmv.w.x {}, t6", reg));
                        } else {
                            self.state.emit(&format!("    fmv.d.x {}, t6", reg));
                        }
                    } else {
                        let imm = c.to_i64().unwrap_or(0);
                        self.state.emit(&format!("    li {}, {}", reg, imm));
                    }
                }
                Operand::Value(v) => {
                    if let Some(slot) = self.state.get_slot(v.0) {
                        if is_addr || (!is_addr && self.state.is_alloca(v.0)) {
                            // For address constraints OR alloca values, load the address
                            self.emit_addi_s0(reg, slot.0);
                        } else if is_fp {
                            // Load FP value from stack
                            self.emit_load_from_s0(reg, slot.0, "fld");
                        } else {
                            self.emit_load_from_s0(reg, slot.0, "ld");
                        }
                    }
                }
            }
        }

        // Pre-load read-write output values
        for (i, (constraint, ptr, _)) in outputs.iter().enumerate() {
            if constraint.contains('+') {
                let reg = &op_regs[i].clone();
                if let Some(slot) = self.state.get_slot(ptr.0) {
                    match &op_kinds[i] {
                        RvConstraintKind::Address => {
                            // For +A: load the address of the memory location
                            self.emit_addi_s0(reg, slot.0);
                        }
                        RvConstraintKind::FpReg => {
                            self.emit_load_from_s0(reg, slot.0, "fld");
                        }
                        RvConstraintKind::Memory => {}
                        _ => {
                            self.emit_load_from_s0(reg, slot.0, "ld");
                        }
                    }
                }
            }
        }

        // Phase 3: Substitute operand references in template and emit
        let lines: Vec<&str> = template.split('\n').collect();
        for line in &lines {
            let line = line.trim().trim_start_matches('\t').trim();
            if line.is_empty() {
                continue;
            }
            let resolved = Self::substitute_riscv_asm_operands(line, &op_regs, &op_names, &op_kinds, &op_mem_offsets, &op_imm_values, &gcc_to_internal);
            self.state.emit(&format!("    {}", resolved));
        }

        // Phase 4: Store output register values back to their stack slots
        for (i, (constraint, ptr, _)) in outputs.iter().enumerate() {
            if constraint.contains('=') || constraint.contains('+') {
                match &op_kinds[i] {
                    RvConstraintKind::Memory => continue,
                    RvConstraintKind::Address => {
                        // For "=A" or "+A": the value was stored via the address in the asm.
                        // No store needed since the asm wrote through the pointer directly.
                        continue;
                    }
                    RvConstraintKind::FpReg => {
                        let reg = &op_regs[i].clone();
                        if let Some(slot) = self.state.get_slot(ptr.0) {
                            self.emit_store_to_s0(reg, slot.0, "fsd");
                        }
                    }
                    _ => {
                        let reg = &op_regs[i].clone();
                        if let Some(slot) = self.state.get_slot(ptr.0) {
                            self.emit_store_to_s0(reg, slot.0, "sd");
                        }
                    }
                }
            }
        }
    }
}

impl RiscvCodegen {
    /// Format an operand for substitution based on its constraint kind.
    fn format_operand(
        idx: usize,
        op_regs: &[String],
        op_kinds: &[RvConstraintKind],
        op_mem_offsets: &[i64],
        op_imm_values: &[Option<i64>],
        use_addr_format: bool, // true for Address kind
    ) -> String {
        if idx >= op_kinds.len() {
            return String::new();
        }
        match &op_kinds[idx] {
            RvConstraintKind::Memory => {
                format!("{}(s0)", op_mem_offsets[idx])
            }
            RvConstraintKind::Address => {
                // Address operands produce (register) format for AMO/LR/SC
                if use_addr_format {
                    format!("({})", &op_regs[idx])
                } else {
                    op_regs[idx].clone()
                }
            }
            RvConstraintKind::Immediate => {
                // Emit the immediate value directly
                if let Some(imm) = op_imm_values[idx] {
                    format!("{}", imm)
                } else {
                    "0".to_string()
                }
            }
            _ => {
                op_regs[idx].clone()
            }
        }
    }

    /// Substitute %0, %1, %[name], %z0, etc. in RISC-V asm template.
    fn substitute_riscv_asm_operands(
        line: &str,
        op_regs: &[String],
        op_names: &[Option<String>],
        op_kinds: &[RvConstraintKind],
        op_mem_offsets: &[i64],
        op_imm_values: &[Option<i64>],
        gcc_to_internal: &[usize],
    ) -> String {
        let mut result = String::new();
        let chars: Vec<char> = line.chars().collect();
        let mut i = 0;
        while i < chars.len() {
            if chars[i] == '%' && i + 1 < chars.len() {
                i += 1;
                // %% -> literal %
                if chars[i] == '%' {
                    result.push('%');
                    i += 1;
                    continue;
                }

                // Check for modifiers: %z (zero-if-zero), %lo, %hi
                if chars[i] == 'z' && i + 1 < chars.len() {
                    // %z modifier: emit "zero" if operand value is 0, else register name
                    i += 1;
                    if chars[i] == '[' {
                        // %z[name]
                        i += 1;
                        let name_start = i;
                        while i < chars.len() && chars[i] != ']' {
                            i += 1;
                        }
                        let name: String = chars[name_start..i].iter().collect();
                        if i < chars.len() { i += 1; }
                        let mut found = false;
                        for (idx, op_name) in op_names.iter().enumerate() {
                            if let Some(ref n) = op_name {
                                if n == &name {
                                    // Check if operand is zero
                                    if let Some(imm) = op_imm_values[idx] {
                                        if imm == 0 {
                                            result.push_str("zero");
                                        } else {
                                            result.push_str(&op_regs[idx]);
                                        }
                                    } else {
                                        result.push_str(&op_regs[idx]);
                                    }
                                    found = true;
                                    break;
                                }
                            }
                        }
                        if !found {
                            result.push_str("%z[");
                            result.push_str(&name);
                            result.push(']');
                        }
                    } else if chars[i].is_ascii_digit() {
                        // %z0, %z1, etc.
                        let mut num = 0usize;
                        while i < chars.len() && chars[i].is_ascii_digit() {
                            num = num * 10 + (chars[i] as usize - '0' as usize);
                            i += 1;
                        }
                        let internal_idx = if num < gcc_to_internal.len() {
                            gcc_to_internal[num]
                        } else {
                            num
                        };
                        if internal_idx < op_regs.len() {
                            // Check if the operand is a constant zero (rJ constraint with zero value)
                            if op_regs[internal_idx] == "zero" {
                                result.push_str("zero");
                            } else if let Some(imm) = op_imm_values.get(internal_idx).and_then(|v| *v) {
                                if imm == 0 {
                                    result.push_str("zero");
                                } else {
                                    result.push_str(&op_regs[internal_idx]);
                                }
                            } else {
                                result.push_str(&op_regs[internal_idx]);
                            }
                        } else {
                            result.push_str(&format!("%z{}", num));
                        }
                    } else {
                        // Not a valid %z pattern, emit as-is
                        result.push('%');
                        result.push('z');
                    }
                    continue;
                }

                // %lo and %hi modifiers (pass through as assembler directives)
                if chars[i] == 'l' && i + 1 < chars.len() && chars[i + 1] == 'o' {
                    result.push_str("%lo");
                    i += 2;
                    continue;
                }
                if chars[i] == 'h' && i + 1 < chars.len() && chars[i + 1] == 'i' {
                    result.push_str("%hi");
                    i += 2;
                    continue;
                }

                if chars[i] == '[' {
                    // Named operand: %[name]
                    i += 1;
                    let name_start = i;
                    while i < chars.len() && chars[i] != ']' {
                        i += 1;
                    }
                    let name: String = chars[name_start..i].iter().collect();
                    if i < chars.len() { i += 1; }

                    let mut found = false;
                    for (idx, op_name) in op_names.iter().enumerate() {
                        if let Some(ref n) = op_name {
                            if n == &name {
                                result.push_str(&Self::format_operand(idx, op_regs, op_kinds, op_mem_offsets, op_imm_values, true));
                                found = true;
                                break;
                            }
                        }
                    }
                    if !found {
                        result.push('%');
                        result.push('[');
                        result.push_str(&name);
                        result.push(']');
                    }
                } else if chars[i].is_ascii_digit() {
                    // Positional operand: %0, %1, etc.
                    let mut num = 0usize;
                    while i < chars.len() && chars[i].is_ascii_digit() {
                        num = num * 10 + (chars[i] as usize - '0' as usize);
                        i += 1;
                    }
                    let internal_idx = if num < gcc_to_internal.len() {
                        gcc_to_internal[num]
                    } else {
                        num
                    };
                    if internal_idx < op_regs.len() {
                        result.push_str(&Self::format_operand(internal_idx, op_regs, op_kinds, op_mem_offsets, op_imm_values, true));
                    } else {
                        result.push_str(&format!("%{}", num));
                    }
                } else {
                    // Not recognized, emit as-is (e.g., %pcrel_lo, %pcrel_hi, etc.)
                    result.push('%');
                    result.push(chars[i]);
                    i += 1;
                }
            } else {
                result.push(chars[i]);
                i += 1;
            }
        }
        result
    }

    /// Get the AMO ordering suffix.
    /// Get the AMO ordering suffix.
    fn amo_ordering(ordering: AtomicOrdering) -> &'static str {
        match ordering {
            AtomicOrdering::Relaxed => "",
            AtomicOrdering::Acquire => ".aq",
            AtomicOrdering::Release => ".rl",
            AtomicOrdering::AcqRel => ".aqrl",
            AtomicOrdering::SeqCst => ".aqrl",
        }
    }

    /// Get the AMO width suffix.
    fn amo_width_suffix(ty: IrType) -> &'static str {
        match ty {
            IrType::I32 | IrType::U32 | IrType::I8 | IrType::U8 | IrType::I16 | IrType::U16 => "w",
            _ => "d",
        }
    }

    /// Sign-extend result for sub-word types after atomic ops.
    fn sign_extend_riscv(state: &mut CodegenState, ty: IrType) {
        match ty {
            IrType::I8 => {
                state.emit("    slli t0, t0, 56");
                state.emit("    srai t0, t0, 56");
            }
            IrType::U8 => {
                state.emit("    andi t0, t0, 0xff");
            }
            IrType::I16 => {
                state.emit("    slli t0, t0, 48");
                state.emit("    srai t0, t0, 48");
            }
            IrType::U16 => {
                state.emit("    slli t0, t0, 48");
                state.emit("    srli t0, t0, 48");
            }
            IrType::I32 => {
                state.emit("    sext.w t0, t0");
            }
            _ => {}
        }
    }

    /// Mask values for sub-word CAS comparison.
    fn mask_for_cmp(state: &mut CodegenState, ty: IrType) {
        match ty {
            IrType::I8 | IrType::U8 => {
                state.emit("    andi t0, t0, 0xff");
            }
            IrType::I16 | IrType::U16 => {
                // Mask to 16 bits
                state.emit("    slli t0, t0, 48");
                state.emit("    srli t0, t0, 48");
            }
            _ => {}
        }
    }

    /// Software CLZ (count leading zeros). Input in t0, result in t0.
    /// For 32-bit types, counts leading zeros in the lower 32 bits.
    fn emit_clz(&mut self, ty: IrType) {
        let bits: u64 = match ty {
            IrType::I32 | IrType::U32 => 32,
            _ => 64,
        };
        let loop_label = self.state.fresh_label("clz_loop");
        let done_label = self.state.fresh_label("clz_done");
        let zero_label = self.state.fresh_label("clz_zero");

        if bits == 32 {
            // Mask to 32 bits to avoid counting upper bits
            self.state.emit("    slli t0, t0, 32");
            self.state.emit("    srli t0, t0, 32");
        }

        // Handle zero case: clz(0) = bits
        self.state.emit(&format!("    beqz t0, {}", zero_label));
        // t1 = count = 0, scan from MSB
        self.state.emit("    li t1, 0");
        // t2 = mask = 1 << (bits-1)
        self.state.emit("    li t2, 1");
        self.state.emit(&format!("    slli t2, t2, {}", bits - 1));
        self.state.emit(&format!("{}:", loop_label));
        self.state.emit("    and t3, t0, t2");
        self.state.emit(&format!("    bnez t3, {}", done_label));
        self.state.emit("    srli t2, t2, 1"); // shift mask right
        self.state.emit("    addi t1, t1, 1");
        self.state.emit(&format!("    j {}", loop_label));
        self.state.emit(&format!("{}:", zero_label));
        self.state.emit(&format!("    li t1, {}", bits));
        self.state.emit(&format!("{}:", done_label));
        self.state.emit("    mv t0, t1");
    }

    /// Software CTZ (count trailing zeros). Input in t0, result in t0.
    fn emit_ctz(&mut self, ty: IrType) {
        let bits = match ty {
            IrType::I32 | IrType::U32 => 32u64,
            _ => 64u64,
        };
        let loop_label = self.state.fresh_label("ctz_loop");
        let done_label = self.state.fresh_label("ctz_done");

        // t1 = count, starts at 0
        self.state.emit("    li t1, 0");
        self.state.emit(&format!("{}:", loop_label));
        self.state.emit(&format!("    li t2, {}", bits));
        self.state.emit("    beq t1, t2, .+12"); // if counted all bits, done
        self.state.emit("    andi t3, t0, 1");
        self.state.emit(&format!("    bnez t3, {}", done_label)); // found a 1 bit
        self.state.emit("    srli t0, t0, 1");
        self.state.emit("    addi t1, t1, 1");
        self.state.emit(&format!("    j {}", loop_label));
        self.state.emit(&format!("{}:", done_label));
        self.state.emit("    mv t0, t1");
    }

    /// Software BSWAP (byte swap). Input in t0, result in t0.
    fn emit_bswap(&mut self, ty: IrType) {
        match ty {
            IrType::I16 | IrType::U16 => {
                // Swap bytes of a 16-bit value
                // t1 = (t0 >> 8) & 0xFF, t2 = (t0 & 0xFF) << 8
                self.state.emit("    andi t1, t0, 0xff");
                self.state.emit("    slli t1, t1, 8");
                self.state.emit("    srli t0, t0, 8");
                self.state.emit("    andi t0, t0, 0xff");
                self.state.emit("    or t0, t0, t1");
            }
            IrType::I32 | IrType::U32 => {
                // Swap 4 bytes: ABCD -> DCBA
                self.state.emit("    mv t1, t0");
                // Byte 0 -> byte 3
                self.state.emit("    andi t2, t1, 0xff");
                self.state.emit("    slli t0, t2, 24");
                // Byte 1 -> byte 2
                self.state.emit("    srli t2, t1, 8");
                self.state.emit("    andi t2, t2, 0xff");
                self.state.emit("    slli t2, t2, 16");
                self.state.emit("    or t0, t0, t2");
                // Byte 2 -> byte 1
                self.state.emit("    srli t2, t1, 16");
                self.state.emit("    andi t2, t2, 0xff");
                self.state.emit("    slli t2, t2, 8");
                self.state.emit("    or t0, t0, t2");
                // Byte 3 -> byte 0
                self.state.emit("    srli t2, t1, 24");
                self.state.emit("    andi t2, t2, 0xff");
                self.state.emit("    or t0, t0, t2");
                // Zero-extend to 32 bits (clear upper 32 bits)
                self.state.emit("    slli t0, t0, 32");
                self.state.emit("    srli t0, t0, 32");
            }
            _ => {
                // 64-bit byte swap: reverse all 8 bytes
                self.state.emit("    mv t1, t0");
                self.state.emit("    li t0, 0");
                // Use a shift-based approach: extract each byte and place it
                for i in 0..8u64 {
                    let src_shift = i * 8;
                    let dst_shift = (7 - i) * 8;
                    self.state.emit(&format!("    srli t2, t1, {}", src_shift));
                    self.state.emit("    andi t2, t2, 0xff");
                    if dst_shift > 0 {
                        self.state.emit(&format!("    slli t2, t2, {}", dst_shift));
                    }
                    self.state.emit("    or t0, t0, t2");
                }
            }
        }
    }

    /// Software POPCOUNT (population count / count set bits). Input in t0, result in t0.
    /// Uses Brian Kernighan's algorithm: repeatedly clear the lowest set bit.
    fn emit_popcount(&mut self, ty: IrType) {
        let loop_label = self.state.fresh_label("popcnt_loop");
        let done_label = self.state.fresh_label("popcnt_done");

        if ty == IrType::I32 || ty == IrType::U32 {
            // Mask to 32 bits
            self.state.emit("    slli t0, t0, 32");
            self.state.emit("    srli t0, t0, 32");
        }

        // t1 = count
        self.state.emit("    li t1, 0");
        self.state.emit(&format!("{}:", loop_label));
        self.state.emit(&format!("    beqz t0, {}", done_label));
        self.state.emit("    addi t2, t0, -1"); // t2 = n - 1
        self.state.emit("    and t0, t0, t2");   // n &= n - 1 (clear lowest set bit)
        self.state.emit("    addi t1, t1, 1");
        self.state.emit(&format!("    j {}", loop_label));
        self.state.emit(&format!("{}:", done_label));
        self.state.emit("    mv t0, t1");
    }
}

impl Default for RiscvCodegen {
    fn default() -> Self {
        Self::new()
    }
}
