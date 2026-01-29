use crate::ir::ir::*;
use crate::common::types::IrType;
use crate::common::fx_hash::FxHashMap;
use crate::backend::common::PtrDirective;
use crate::backend::state::{CodegenState, StackSlot, SlotAddr};
use crate::backend::traits::ArchCodegen;
use crate::backend::generation::{generate_module, is_i128_type, calculate_stack_space_common, find_param_alloca};
use crate::backend::call_abi::{CallAbiConfig, CallArgClass, compute_stack_arg_space};
use crate::backend::call_emit::{ParamClass, classify_params};
use crate::backend::cast::{CastKind, classify_cast, FloatOp};
use crate::backend::inline_asm::emit_inline_asm_common;
use crate::backend::regalloc::PhysReg;

/// RISC-V callee-saved registers always available for register allocation.
/// s0 is the frame pointer.
/// These 6 registers are always safe to allocate: s1, s7-s11.
/// PhysReg encoding: 1=s1, 7=s7, 8=s8, 9=s9, 10=s10, 11=s11.
const RISCV_CALLEE_SAVED: [PhysReg; 6] = [
    PhysReg(1), PhysReg(7), PhysReg(8), PhysReg(9), PhysReg(10), PhysReg(11),
];

/// Callee-saved registers that are conditionally available for allocation.
/// s2-s6 are used as staging temporaries in emit_call_reg_args when a call
/// has >= 4 GP register arguments. Any that are NOT needed for call staging
/// can be allocated by the register allocator, giving up to 11 callee-saved
/// registers total (vs. 6 when all are reserved).
/// PhysReg(2)=s2, PhysReg(3)=s3, PhysReg(4)=s4, PhysReg(5)=s5, PhysReg(6)=s6.
const CALL_TEMP_CALLEE_SAVED: [PhysReg; 5] = [
    PhysReg(2), PhysReg(3), PhysReg(4), PhysReg(5), PhysReg(6),
];

/// Map a PhysReg index to its RISC-V register name.
fn callee_saved_name(reg: PhysReg) -> &'static str {
    match reg.0 {
        1 => "s1", 2 => "s2", 3 => "s3", 4 => "s4", 5 => "s5",
        6 => "s6", 7 => "s7", 8 => "s8", 9 => "s9", 10 => "s10", 11 => "s11",
        _ => unreachable!("invalid RISC-V callee-saved register index"),
    }
}

/// Scan all call/call_indirect instructions in a function and return the maximum
/// number of GP register arguments across all calls. This is used to determine
/// which callee-saved temporaries (s2-s6) emit_call_reg_args will use, so that
/// they can be saved/restored in the prologue/epilogue.
fn max_gp_reg_args_in_calls(func: &IrFunction, config: &CallAbiConfig) -> usize {
    use crate::backend::call_abi::classify_call_args;
    use crate::backend::call_abi::CallArgClass;

    let mut max_gp = 0usize;
    for block in &func.blocks {
        for inst in &block.instructions {
            let info = match inst {
                Instruction::Call { info, .. } | Instruction::CallIndirect { info, .. } => info,
                _ => continue,
            };
            let classes = classify_call_args(&info.args, &info.arg_types, &info.struct_arg_sizes, &info.struct_arg_aligns, &info.struct_arg_classes, info.is_variadic, config);
            let gp_count = classes.iter().filter(|c| matches!(c, CallArgClass::IntReg { .. })).count();
            if gp_count > max_gp {
                max_gp = gp_count;
            }
        }
    }
    max_gp
}

/// Scan inline asm instructions in a function and collect any callee-saved
/// registers that are used via specific constraints or listed in clobbers.
/// These must be saved/restored in the function prologue/epilogue.
fn collect_inline_asm_callee_saved_riscv(func: &IrFunction, used: &mut Vec<PhysReg>) {
    crate::backend::generation::collect_inline_asm_callee_saved(
        func, used, constraint_to_callee_saved_riscv, riscv_reg_to_callee_saved,
    );
}

/// Map a RISC-V register name to its PhysReg index, if it is callee-saved.
fn riscv_reg_to_callee_saved(name: &str) -> Option<PhysReg> {
    match name {
        "s1" | "x9" => Some(PhysReg(1)),
        "s2" | "x18" => Some(PhysReg(2)),
        "s3" | "x19" => Some(PhysReg(3)),
        "s4" | "x20" => Some(PhysReg(4)),
        "s5" | "x21" => Some(PhysReg(5)),
        "s6" | "x22" => Some(PhysReg(6)),
        "s7" | "x23" => Some(PhysReg(7)),
        "s8" | "x24" => Some(PhysReg(8)),
        "s9" | "x25" => Some(PhysReg(9)),
        "s10" | "x26" => Some(PhysReg(10)),
        "s11" | "x27" => Some(PhysReg(11)),
        _ => None,
    }
}

/// Check if a constraint string refers to a specific RISC-V callee-saved register.
fn constraint_to_callee_saved_riscv(constraint: &str) -> Option<PhysReg> {
    // Handle explicit register constraint: {regname}
    if constraint.starts_with('{') && constraint.ends_with('}') {
        let reg = &constraint[1..constraint.len()-1];
        return riscv_reg_to_callee_saved(reg);
    }
    // Direct register name (e.g., "s1", "x9")
    riscv_reg_to_callee_saved(constraint)
}

/// RISC-V 64 code generator. Implements the ArchCodegen trait for the shared framework.
/// Uses standard RISC-V calling convention with register allocation for hot values.
pub struct RiscvCodegen {
    pub(super) state: CodegenState,
    current_return_type: IrType,
    /// Number of named integer params for current variadic function.
    /// This is the effective GP register index after classification (including
    /// alignment gaps for I128/F128 pairs), capped at 8.
    va_named_gp_count: usize,
    /// Total bytes of named params that overflow to the caller's stack.
    va_named_stack_bytes: usize,
    /// Current frame size (below s0, not including the register save area above s0).
    current_frame_size: i64,
    /// Whether the current function is variadic.
    is_variadic: bool,
    /// Scratch register indices for inline asm allocation.
    pub(super) asm_gp_scratch_idx: usize,
    pub(super) asm_fp_scratch_idx: usize,
    /// Register allocation results for the current function.
    /// Maps value ID -> callee-saved register assignment.
    reg_assignments: FxHashMap<u32, PhysReg>,
    /// Which callee-saved registers are used and need save/restore.
    used_callee_saved: Vec<PhysReg>,
    /// Whether to suppress linker relaxation (-mno-relax).
    /// When true, emits `.option norelax` at the top of the assembly output
    /// to prevent R_RISCV_RELAX relocations. Required for EFI stub code.
    no_relax: bool,
}

impl RiscvCodegen {
    pub fn new() -> Self {
        Self {
            state: CodegenState::new(),
            current_return_type: IrType::I64,
            va_named_gp_count: 0,
            va_named_stack_bytes: 0,
            current_frame_size: 0,
            is_variadic: false,
            asm_gp_scratch_idx: 0,
            asm_fp_scratch_idx: 0,
            reg_assignments: FxHashMap::default(),
            used_callee_saved: Vec::new(),
            no_relax: false,
        }
    }

    /// Disable jump table emission (-fno-jump-tables).
    pub fn set_no_jump_tables(&mut self, enabled: bool) {
        self.state.no_jump_tables = enabled;
    }

    /// Enable position-independent code generation (-fPIC/-fpie).
    pub fn set_pic(&mut self, pic: bool) {
        self.state.pic_mode = pic;
    }

    /// Suppress linker relaxation (-mno-relax).
    pub fn set_no_relax(&mut self, enabled: bool) {
        self.no_relax = enabled;
    }

    /// Apply all relevant options from a `CodegenOptions` struct.
    pub fn apply_options(&mut self, opts: &crate::backend::CodegenOptions) {
        self.set_pic(opts.pic);
        self.set_no_jump_tables(opts.no_jump_tables);
        self.set_no_relax(opts.no_relax);
    }

    /// Emit `.option norelax` if -mno-relax is set. Must be called once
    /// before any code is generated. This prevents the GNU assembler from
    /// generating R_RISCV_RELAX relocation entries, which is required for
    /// EFI stub code that forbids absolute symbol references from linker
    /// relaxation.
    pub fn emit_pre_directives(&mut self) {
        if self.no_relax {
            self.state.emit(".option norelax");
        }
    }

    /// Load comparison operands into t1 and t2, then sign/zero-extend
    /// sub-64-bit types. Shared by emit_cmp and emit_fused_cmp_branch.
    fn emit_cmp_operand_load(&mut self, lhs: &Operand, rhs: &Operand, ty: IrType) {
        self.operand_to_t0(lhs);
        self.state.emit("    mv t1, t0");
        self.operand_to_t0(rhs);
        self.state.emit("    mv t2, t0");

        // Sign/zero-extend operands to 64 bits based on their actual type width.
        // The narrow optimization pass can produce I8/I16/U8/U16 typed comparisons,
        // so we must extend at the correct width, not just 32-bit for all sub-64 types.
        match ty {
            IrType::U8 => {
                self.state.emit("    andi t1, t1, 0xff");
                self.state.emit("    andi t2, t2, 0xff");
            }
            IrType::U16 => {
                self.state.emit("    slli t1, t1, 48");
                self.state.emit("    srli t1, t1, 48");
                self.state.emit("    slli t2, t2, 48");
                self.state.emit("    srli t2, t2, 48");
            }
            IrType::U32 => {
                self.state.emit("    slli t1, t1, 32");
                self.state.emit("    srli t1, t1, 32");
                self.state.emit("    slli t2, t2, 32");
                self.state.emit("    srli t2, t2, 32");
            }
            IrType::I8 => {
                self.state.emit("    slli t1, t1, 56");
                self.state.emit("    srai t1, t1, 56");
                self.state.emit("    slli t2, t2, 56");
                self.state.emit("    srai t2, t2, 56");
            }
            IrType::I16 => {
                self.state.emit("    slli t1, t1, 48");
                self.state.emit("    srai t1, t1, 48");
                self.state.emit("    slli t2, t2, 48");
                self.state.emit("    srai t2, t2, 48");
            }
            IrType::I32 => {
                self.state.emit("    sext.w t1, t1");
                self.state.emit("    sext.w t2, t2");
            }
            _ => {} // I64/U64/Ptr: no extension needed
        }
    }

    // --- RISC-V helpers ---

    /// Check if an immediate fits in a 12-bit signed field.
    fn fits_imm12(val: i64) -> bool {
        val >= -2048 && val <= 2047
    }

    /// Emit: store `reg` to `offset(s0)`, handling large offsets via t6.
    /// Uses t6 as scratch to avoid conflicts with t3-t5 call argument temps.
    pub(super) fn emit_store_to_s0(&mut self, reg: &str, offset: i64, store_instr: &str) {
        if Self::fits_imm12(offset) {
            self.state.emit_fmt(format_args!("    {} {}, {}(s0)", store_instr, reg, offset));
        } else {
            self.state.emit_fmt(format_args!("    li t6, {}", offset));
            self.state.emit("    add t6, s0, t6");
            self.state.emit_fmt(format_args!("    {} {}, 0(t6)", store_instr, reg));
        }
    }

    /// Emit: load from `offset(base)` into `dest`, handling large offsets via t6.
    /// For arbitrary base registers (not s0/sp). Uses t6 as scratch when offset
    /// exceeds RISC-V's 12-bit signed immediate range (-2048..2047).
    fn emit_load_from_reg(state: &mut crate::backend::state::CodegenState, dest: &str, base: &str, offset: i64, load_instr: &str) {
        if Self::fits_imm12(offset) {
            state.emit_fmt(format_args!("    {} {}, {}({})", load_instr, dest, offset, base));
        } else {
            state.emit_fmt(format_args!("    li t6, {}", offset));
            state.emit_fmt(format_args!("    add t6, {}, t6", base));
            state.emit_fmt(format_args!("    {} {}, 0(t6)", load_instr, dest));
        }
    }

    /// Emit: load from `offset(s0)` into `reg`, handling large offsets via t6.
    /// Uses t6 as scratch to avoid conflicts with t3-t5 call argument temps.
    pub(super) fn emit_load_from_s0(&mut self, reg: &str, offset: i64, load_instr: &str) {
        if Self::fits_imm12(offset) {
            self.state.emit_fmt(format_args!("    {} {}, {}(s0)", load_instr, reg, offset));
        } else {
            self.state.emit_fmt(format_args!("    li t6, {}", offset));
            self.state.emit("    add t6, s0, t6");
            self.state.emit_fmt(format_args!("    {} {}, 0(t6)", load_instr, reg));
        }
    }

    /// Emit: `dest_reg = s0 + offset`, handling large offsets.
    pub(super) fn emit_addi_s0(&mut self, dest_reg: &str, offset: i64) {
        if Self::fits_imm12(offset) {
            self.state.emit_fmt(format_args!("    addi {}, s0, {}", dest_reg, offset));
        } else {
            self.state.emit_fmt(format_args!("    li {}, {}", dest_reg, offset));
            self.state.emit_fmt(format_args!("    add {}, s0, {}", dest_reg, dest_reg));
        }
    }

    /// Emit: store `reg` to `offset(sp)`, handling large offsets via t6.
    /// Used for stack overflow arguments in emit_call.
    fn emit_store_to_sp(&mut self, reg: &str, offset: i64, store_instr: &str) {
        if Self::fits_imm12(offset) {
            self.state.emit_fmt(format_args!("    {} {}, {}(sp)", store_instr, reg, offset));
        } else {
            self.state.emit_fmt(format_args!("    li t6, {}", offset));
            self.state.emit("    add t6, sp, t6");
            self.state.emit_fmt(format_args!("    {} {}, 0(t6)", store_instr, reg));
        }
    }

    /// Emit: load from `offset(sp)` into `reg`, handling large offsets via t6.
    /// Used for loading stack overflow arguments in emit_call.
    fn emit_load_from_sp(&mut self, reg: &str, offset: i64, load_instr: &str) {
        if Self::fits_imm12(offset) {
            self.state.emit_fmt(format_args!("    {} {}, {}(sp)", load_instr, reg, offset));
        } else {
            self.state.emit_fmt(format_args!("    li t6, {}", offset));
            self.state.emit("    add t6, sp, t6");
            self.state.emit_fmt(format_args!("    {} {}, 0(t6)", load_instr, reg));
        }
    }

    /// Emit: `sp = sp + imm`, handling large immediates via t6.
    /// Positive imm deallocates stack, negative allocates.
    pub(super) fn emit_addi_sp(&mut self, imm: i64) {
        if Self::fits_imm12(imm) {
            self.state.emit_fmt(format_args!("    addi sp, sp, {}", imm));
        } else if imm > 0 {
            self.state.emit_fmt(format_args!("    li t6, {}", imm));
            self.state.emit("    add sp, sp, t6");
        } else {
            self.state.emit_fmt(format_args!("    li t6, {}", -imm));
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

        const PAGE_SIZE: i64 = 4096;

        // Small-frame path requires ALL immediates to fit in 12 bits:
        // -total_alloc (sp adjust), and frame_size (s0 setup).
        if Self::fits_imm12(-total_alloc) && Self::fits_imm12(total_alloc) {
            // Small frame: all offsets fit in 12-bit immediates
            self.state.emit_fmt(format_args!("    addi sp, sp, -{}", total_alloc));
            // ra and s0 are saved relative to s0, which is sp + frame_size
            // (NOT sp + total_alloc for variadic functions!)
            self.state.emit_fmt(format_args!("    sd ra, {}(sp)", frame_size - 8));
            self.state.emit_fmt(format_args!("    sd s0, {}(sp)", frame_size - 16));
            self.state.emit_fmt(format_args!("    addi s0, sp, {}", frame_size));
        } else if total_alloc > PAGE_SIZE {
            // Stack probing: for large frames, touch each page so the kernel
            // can grow the stack mapping. Without this, a single large sub
            // can skip guard pages and cause a segfault.
            let probe_label = self.state.fresh_label("stack_probe");
            self.state.emit_fmt(format_args!("    li t1, {}", total_alloc));
            self.state.emit_fmt(format_args!("    li t2, {}", PAGE_SIZE));
            self.state.emit_fmt(format_args!("{}:", probe_label));
            self.state.emit("    sub sp, sp, t2");
            self.state.emit("    sd zero, 0(sp)");
            self.state.emit("    sub t1, t1, t2");
            self.state.emit_fmt(format_args!("    bgt t1, t2, {}", probe_label));
            self.state.emit("    sub sp, sp, t1");
            self.state.emit("    sd zero, 0(sp)");
            // Compute s0 = sp + frame_size (NOT total_alloc)
            self.state.emit_fmt(format_args!("    li t0, {}", frame_size));
            self.state.emit("    add t0, sp, t0");
            // Save ra and old s0 at s0-8, s0-16
            self.state.emit("    sd ra, -8(t0)");
            self.state.emit("    sd s0, -16(t0)");
            self.state.emit("    mv s0, t0");
        } else {
            // Large frame: use t0 for offsets
            self.state.emit_fmt(format_args!("    li t0, {}", total_alloc));
            self.state.emit("    sub sp, sp, t0");
            // Compute s0 = sp + frame_size (NOT total_alloc)
            self.state.emit_fmt(format_args!("    li t0, {}", frame_size));
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
        // When DynAlloca is used, SP was modified at runtime, so we must restore
        // from s0 (frame pointer) rather than using SP-relative offsets.
        if !self.state.has_dyn_alloca && Self::fits_imm12(-total_alloc) && Self::fits_imm12(total_alloc) {
            // Small frame: restore from known sp offsets
            // ra/s0 saved at sp + frame_size - 8/16 (relative to current sp)
            self.state.emit_fmt(format_args!("    ld ra, {}(sp)", frame_size - 8));
            self.state.emit_fmt(format_args!("    ld s0, {}(sp)", frame_size - 16));
            self.state.emit_fmt(format_args!("    addi sp, sp, {}", total_alloc));
        } else {
            // Large frame or DynAlloca: restore from s0-relative offsets (always fit in imm12).
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
    pub(super) fn operand_to_t0(&mut self, op: &Operand) {
        match op {
            Operand::Const(c) => {
                self.state.reg_cache.invalidate_acc();
                match c {
                    IrConst::I8(v) => self.state.emit_fmt(format_args!("    li t0, {}", v)),
                    IrConst::I16(v) => self.state.emit_fmt(format_args!("    li t0, {}", v)),
                    IrConst::I32(v) => self.state.emit_fmt(format_args!("    li t0, {}", v)),
                    IrConst::I64(v) => self.state.emit_fmt(format_args!("    li t0, {}", v)),
                    IrConst::F32(v) => {
                        let bits = v.to_bits() as u64;
                        self.state.emit_fmt(format_args!("    li t0, {}", bits as i64));
                    }
                    IrConst::F64(v) => {
                        let bits = v.to_bits();
                        self.state.emit_fmt(format_args!("    li t0, {}", bits as i64));
                    }
                    // LongDouble at computation level is treated as F64
                    IrConst::LongDouble(v, _) => {
                        let bits = v.to_bits();
                        self.state.emit_fmt(format_args!("    li t0, {}", bits as i64));
                    }
                    IrConst::I128(v) => self.state.emit_fmt(format_args!("    li t0, {}", *v as i64)), // truncate
                    IrConst::Zero => self.state.emit("    li t0, 0"),
                }
            }
            Operand::Value(v) => {
                let is_alloca = self.state.is_alloca(v.0);
                if self.state.reg_cache.acc_has(v.0, is_alloca) {
                    return; // Cache hit — t0 already holds this value.
                }
                // Check if this value is register-allocated.
                if let Some(&reg) = self.reg_assignments.get(&v.0) {
                    let reg_name = callee_saved_name(reg);
                    self.state.emit_fmt(format_args!("    mv t0, {}", reg_name));
                    self.state.reg_cache.set_acc(v.0, false);
                } else if let Some(slot) = self.state.get_slot(v.0) {
                    if is_alloca {
                        if let Some(align) = self.state.alloca_over_align(v.0) {
                            // Over-aligned alloca: compute aligned address.
                            // t0 = (slot_addr + align-1) & -align
                            self.emit_addi_s0("t0", slot.0);
                            self.state.emit_fmt(format_args!("    li t6, {}", align - 1));
                            self.state.emit("    add t0, t0, t6");
                            self.state.emit_fmt(format_args!("    li t6, -{}", align));
                            self.state.emit("    and t0, t0, t6");
                        } else {
                            self.emit_addi_s0("t0", slot.0);
                        }
                    } else {
                        self.emit_load_from_s0("t0", slot.0, "ld");
                    }
                    self.state.reg_cache.set_acc(v.0, is_alloca);
                } else {
                    self.state.emit("    li t0, 0");
                    self.state.reg_cache.invalidate_acc();
                }
            }
        }
    }

    /// Store t0 to a value's location (register or stack slot).
    /// Register-only strategy: if the value has a callee-saved register assignment,
    /// store ONLY to the register (skip the stack write). This eliminates redundant
    /// memory stores for register-allocated values.
    pub(super) fn store_t0_to(&mut self, dest: &Value) {
        if let Some(&reg) = self.reg_assignments.get(&dest.0) {
            // Value has a callee-saved register: store only to register, skip stack.
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mv {}, t0", reg_name));
        } else if let Some(slot) = self.state.get_slot(dest.0) {
            // No register: store to stack slot.
            self.emit_store_to_s0("t0", slot.0, "sd");
        }
        self.state.reg_cache.set_acc(dest.0, false);
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
                        self.state.emit_fmt(format_args!("    li t0, {}", low));
                        self.state.emit_fmt(format_args!("    li t1, {}", high));
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
                    } else if self.state.is_i128_value(v.0) {
                        // 128-bit value in 16-byte stack slot
                        self.emit_load_from_s0("t0", slot.0, "ld");
                        self.emit_load_from_s0("t1", slot.0 + 8, "ld");
                    } else {
                        // Non-i128 value (e.g. shift amount): load 8 bytes, zero high
                        // Check register allocation first, since register-allocated values
                        // may not have their stack slot written.
                        if let Some(&reg) = self.reg_assignments.get(&v.0) {
                            let reg_name = callee_saved_name(reg);
                            self.state.emit_fmt(format_args!("    mv t0, {}", reg_name));
                        } else {
                            self.emit_load_from_s0("t0", slot.0, "ld");
                        }
                        self.state.emit("    li t1, 0");
                    }
                } else {
                    // No stack slot: check register allocation
                    if let Some(&reg) = self.reg_assignments.get(&v.0) {
                        let reg_name = callee_saved_name(reg);
                        self.state.emit_fmt(format_args!("    mv t0, {}", reg_name));
                        self.state.emit("    li t1, 0");
                    } else {
                        self.state.emit("    li t0, 0");
                        self.state.emit("    li t1, 0");
                    }
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
    // emit_i128_binop and emit_i128_cmp use the shared default implementations
    // via ArchCodegen trait defaults, with per-op primitives defined in the trait impl above.

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

    // --- Intrinsic helpers (scalar emulation) ---

    /// Load the address of a pointer Value into the given register.
    pub(super) fn load_ptr_to_reg_rv(&mut self, ptr: &Value, reg: &str) {
        if let Some(slot) = self.state.get_slot(ptr.0) {
            if self.state.is_alloca(ptr.0) {
                self.emit_addi_s0(reg, slot.0);
            } else {
                self.emit_load_from_s0(reg, slot.0, "ld");
            }
        }
    }

}

const RISCV_ARG_REGS: [&str; 8] = ["a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"];

impl ArchCodegen for RiscvCodegen {
    fn state(&mut self) -> &mut CodegenState { &mut self.state }
    fn state_ref(&self) -> &CodegenState { &self.state }
    fn ptr_directive(&self) -> PtrDirective { PtrDirective::Dword }

    fn get_phys_reg_for_value(&self, val_id: u32) -> Option<PhysReg> {
        self.reg_assignments.get(&val_id).copied()
    }

    fn emit_reg_to_reg_move(&mut self, src: PhysReg, dest: PhysReg) {
        let s_name = callee_saved_name(src);
        let d_name = callee_saved_name(dest);
        self.state.emit_fmt(format_args!("    mv {}, {}", d_name, s_name));
    }

    fn emit_acc_to_phys_reg(&mut self, dest: PhysReg) {
        let d_name = callee_saved_name(dest);
        self.state.emit_fmt(format_args!("    mv {}, t0", d_name));
    }

    fn phys_reg_name(&self, reg: PhysReg) -> &'static str {
        callee_saved_name(reg)
    }

    fn jump_mnemonic(&self) -> &'static str { "j" }
    fn trap_instruction(&self) -> &'static str { "ebreak" }

    /// Override emit_branch to use `jump <label>, t6` instead of `j <label>`.
    /// The `j` pseudo (JAL with rd=x0) has only +-1MB range. For large functions
    /// (e.g., Lua's luaV_execute, oniguruma's match_at), intra-function branches
    /// can exceed this. The `jump` pseudo generates `auipc t6, ... ; jr t6`
    /// with +-2GB range. t6 is safe to clobber here because emit_branch is only
    /// called at block terminators where all scratch registers are dead.
    fn emit_branch(&mut self, label: &str) {
        self.state.emit_fmt(format_args!("    jump {}, t6", label));
    }

    /// Override emit_branch_to_block to use `jump` pseudo for +-2GB range,
    /// matching emit_branch. Without this, the default trait implementation
    /// uses `j` which can't reach labels >1MB away.
    fn emit_branch_to_block(&mut self, block: BlockId) {
        let out = &mut self.state.out;
        out.write_str("    jump .L");
        out.write_u64(block.0 as u64);
        out.write_str(", t6");
        out.newline();
    }

    /// Emit branch-if-nonzero using a relaxed sequence.
    /// `bnez` has +-4KB range which is easily exceeded in large functions.
    /// We use `beqz t0, .Lskip; jump target, t6; .Lskip:` instead,
    /// where `jump` has +-2GB range.
    fn emit_branch_nonzero(&mut self, label: &str) {
        let skip = self.state.fresh_label("skip");
        self.state.emit_fmt(format_args!("    beqz t0, {}", skip));
        self.state.emit_fmt(format_args!("    jump {}, t6", label));
        self.state.emit_fmt(format_args!("{}:", skip));
    }

    fn emit_jump_indirect(&mut self) {
        self.state.emit("    jr t0");
    }

    fn emit_switch_case_branch(&mut self, case_val: i64, label: &str) {
        // Load case value into t1, compare, and branch if equal.
        // Use bne-skip + jump pattern for +-2GB range instead of beq which has
        // only +-4KB range and can overflow in large functions (e.g. PostgreSQL).
        let skip = self.state.fresh_label("sw_skip");
        self.state.emit_fmt(format_args!("    li t1, {}", case_val));
        self.state.emit_fmt(format_args!("    bne t0, t1, {}", skip));
        self.state.emit_fmt(format_args!("    jump {}, t6", label));
        self.state.emit_fmt(format_args!("{}:", skip));
    }

    fn emit_switch_jump_table(&mut self, val: &Operand, cases: &[(i64, BlockId)], default: &BlockId) {
        use crate::backend::traits::build_jump_table;
        let (table, min_val, range) = build_jump_table(cases, default);
        let table_label = self.state.fresh_label("jt");
        let default_label = default.as_label();

        // Load switch value into t0
        self.operand_to_t0(val);

        // Subtract min_val to normalize index
        if min_val != 0 {
            let neg_min = -min_val;
            if neg_min >= -2048 && neg_min <= 2047 {
                self.state.emit_fmt(format_args!("    addi t0, t0, {}", neg_min));
            } else {
                self.state.emit_fmt(format_args!("    li t1, {}", neg_min));
                self.state.emit("    add t0, t0, t1");
            }
        }
        // Range check (unsigned): branch to default if index >= range
        // Use bltu-skip + jump pattern for +-2GB range instead of bgeu which has
        // only +-4KB range and can overflow in large functions.
        let range_ok = self.state.fresh_label("range_ok");
        self.state.emit_fmt(format_args!("    li t1, {}", range));
        self.state.emit_fmt(format_args!("    bltu t0, t1, {}", range_ok));
        self.state.emit_fmt(format_args!("    jump {}, t6", default_label));
        self.state.emit_fmt(format_args!("{}:", range_ok));

        // Always use relative 32-bit offsets for jump tables on RISC-V.
        // Absolute .dword entries produce R_RISCV_64 relocations in .rodata
        // which are not resolved by the Linux kernel linker, leaving jump table
        // entries as zeros and causing branches to address 0 (boot hang).
        // Relative entries (.word target - table_base) are resolved at link time.
        // Use `lla` for the local jump table label to avoid GOT indirection.
        self.state.emit_fmt(format_args!("    lla t1, {}", table_label));
        self.state.emit("    slli t0, t0, 2");  // index * 4 (32-bit entries)
        self.state.emit("    add t1, t1, t0");
        self.state.emit("    lw t0, 0(t1)");    // load 32-bit signed offset
        self.state.emit_fmt(format_args!("    lla t1, {}", table_label));
        self.state.emit("    add t1, t1, t0");   // base + offset
        self.state.emit("    jr t1");

        // Emit jump table with relative .word entries
        self.state.emit(".section .rodata");
        self.state.emit(".align 2");
        self.state.emit_fmt(format_args!("{}:", table_label));
        for target in &table {
            let target_label = target.as_label();
            self.state.emit_fmt(format_args!("    .word {} - {}", target_label, table_label));
        }
        let sect = self.state.current_text_section.clone();
        self.state.emit_fmt(format_args!(".section {},\"ax\",@progbits", sect));
        self.state.reg_cache.invalidate_all();
    }

    fn calculate_stack_space(&mut self, func: &IrFunction) -> i64 {
        // For variadic functions, count the actual GP registers used by named
        // parameters. A struct that occupies 2 GP regs counts as 2, not 1.
        // This is critical for va_start to correctly point to the first variadic arg.
        if func.is_variadic {
            // For variadic callee, call_abi_config() already has variadic_floats_in_gp: true.
            let classification = crate::backend::call_emit::classify_params_full(func, &self.call_abi_config());
            // Use the effective GP register index (includes alignment gaps for I128/F128 pairs)
            // rather than summing gp_reg_count(), so we correctly skip over alignment padding.
            self.va_named_gp_count = classification.int_reg_idx.min(8);
            // Track stack bytes consumed by named params that overflowed to the caller's stack.
            self.va_named_stack_bytes = crate::backend::call_emit::named_params_stack_bytes(&classification.classes);
            self.is_variadic = true;
        } else {
            self.is_variadic = false;
        }

        // Run register allocator BEFORE stack space computation so we can
        // skip allocating stack slots for values assigned to registers.
        //
        // For functions with inline asm, collect callee-saved registers that are
        // clobbered or used as explicit constraints, then filter them from the
        // allocation pool. This allows register allocation to proceed using the
        // remaining registers, rather than disabling it entirely. Many kernel
        // functions contain inline asm from inlined spin_lock/spin_unlock; without
        // this, they get no register allocation and enormous stack frames (4KB+),
        // causing kernel stack overflows.
        let mut asm_clobbered_regs: Vec<PhysReg> = Vec::new();
        collect_inline_asm_callee_saved_riscv(func, &mut asm_clobbered_regs);

        // Determine which s2-s6 registers are needed as call staging temporaries.
        // emit_call_reg_args uses s2-s6 when a call has >= 4 GP register arguments.
        // Any s2-s6 registers NOT needed for call staging can be used by the
        // register allocator, giving us up to 11 callee-saved registers instead of 6.
        let max_gp = max_gp_reg_args_in_calls(func, &self.call_abi_config());
        let num_s_regs_for_calls = if max_gp > 3 {
            (max_gp - 3).min(CALL_TEMP_CALLEE_SAVED.len())
        } else {
            0
        };

        // Build the full set of available callee-saved registers:
        // Always available: s1, s7-s11 (RISCV_CALLEE_SAVED)
        // Conditionally available: s2-s6 that are NOT reserved for call staging
        let mut all_available: Vec<PhysReg> = RISCV_CALLEE_SAVED.to_vec();
        for i in num_s_regs_for_calls..CALL_TEMP_CALLEE_SAVED.len() {
            all_available.push(CALL_TEMP_CALLEE_SAVED[i]);
        }

        let available_regs = crate::backend::generation::filter_available_regs(&all_available, &asm_clobbered_regs);
        // NOTE: RISC-V caller-saved register allocation (t2-t6) is not yet feasible.
        // All five temporary registers are hardcoded as scratch throughout the codegen:
        // t2 = comparison RHS, binop RHS, indirect call target, atomic val/expected
        // t3 = accumulator save, i128 lhs_lo, call arg staging (1st GP)
        // t4 = i128 lhs_hi, memcpy byte, call arg staging (2nd GP)
        // t5 = primary pointer/address register for all indirect loads/stores
        // t6 = large-offset scratch, long-range branch target, i128 rhs_hi
        // Adding caller-saved allocation would require refactoring these to use
        // dedicated non-allocatable scratch registers (or a separate scratch pool).
        let (reg_assigned, cached_liveness) = crate::backend::generation::run_regalloc_and_merge_clobbers(
            func, available_regs, Vec::new(), &asm_clobbered_regs,
            &mut self.reg_assignments, &mut self.used_callee_saved,
        );
        // f128_load_sources is cleared by state.reset_for_function() at the start of each function.

        // Add the s2-s6 registers that ARE needed for call staging to used_callee_saved,
        // so they are saved/restored in the prologue/epilogue.
        for i in 0..num_s_regs_for_calls {
            let reg = CALL_TEMP_CALLEE_SAVED[i];
            if !self.used_callee_saved.contains(&reg) {
                self.used_callee_saved.push(reg);
            }
        }

        let space = calculate_stack_space_common(&mut self.state, func, 16, |space, alloc_size, align| {
            // RISC-V uses negative offsets from s0 (frame pointer)
            // Honor alignment: round up space to alignment boundary before allocating
            let effective_align = if align > 0 { align.max(8) } else { 8 };
            let alloc = ((alloc_size + 7) & !7).max(8);
            let new_space = ((space + alloc + effective_align - 1) / effective_align) * effective_align;
            (-(new_space as i64), new_space)
        }, &reg_assigned, cached_liveness);

        // Add space for saving callee-saved registers.
        // Each callee-saved register needs 8 bytes on the stack.
        let callee_save_space = (self.used_callee_saved.len() as i64) * 8;
        space + callee_save_space
    }

    fn aligned_frame_size(&self, raw_space: i64) -> i64 {
        (raw_space + 15) & !15
    }

    fn emit_prologue(&mut self, func: &IrFunction, frame_size: i64) {
        self.current_return_type = func.return_type;
        self.current_frame_size = frame_size;
        self.emit_prologue_riscv(frame_size);

        // Save callee-saved registers used by the register allocator.
        // They are saved at the bottom of the frame (highest negative offsets from s0).
        let used_regs = self.used_callee_saved.clone();
        for (i, &reg) in used_regs.iter().enumerate() {
            let offset = -(frame_size as i64) + (i as i64 * 8);
            let reg_name = callee_saved_name(reg);
            self.emit_store_to_s0(reg_name, offset, "sd");
        }
    }

    fn emit_epilogue(&mut self, frame_size: i64) {
        // Restore callee-saved registers before epilogue.
        let used_regs = self.used_callee_saved.clone();
        for (i, &reg) in used_regs.iter().enumerate() {
            let offset = -(frame_size as i64) + (i as i64 * 8);
            let reg_name = callee_saved_name(reg);
            self.emit_load_from_s0(reg_name, offset, "ld");
        }
        self.emit_epilogue_riscv(frame_size);
    }

    fn emit_store_params(&mut self, func: &IrFunction) {
        let float_arg_regs = ["fa0", "fa1", "fa2", "fa3", "fa4", "fa5", "fa6", "fa7"];

        // For variadic functions: save all integer register args (a0-a7) to the
        // register save area at POSITIVE offsets from s0.
        if func.is_variadic {
            for i in 0..8usize {
                self.emit_store_to_s0(RISCV_ARG_REGS[i], (i as i64) * 8, "sd");
            }
        }

        // Use shared parameter classification. On RISC-V, variadic functions
        // route float params through GP regs; non-variadic use FP regs.
        let mut config = self.call_abi_config();
        config.variadic_floats_in_gp = func.is_variadic;
        let param_classes = classify_params(func, &config);
        // Save param classes for ParamRef instructions
        self.state.param_classes = param_classes.clone();
        self.state.num_params = func.params.len();
        self.state.func_is_variadic = func.is_variadic;

        // Pre-compute param alloca slots for emit_param_ref
        self.state.param_alloca_slots = (0..func.params.len()).map(|i| {
            find_param_alloca(func, i).and_then(|(dest, ty)| {
                self.state.get_slot(dest.0).map(|slot| (slot, ty))
            })
        }).collect();

        // Stack-passed params are at positive s0 offsets.
        // For variadic: register save area occupies s0+0..s0+56, stack params at s0+64+.
        let stack_base: i64 = if func.is_variadic { 64 } else { 0 };

        // Check if any F128 params exist (need to save all regs before __trunctfdf2).
        let has_f128_reg_params = param_classes.iter().any(|c| matches!(c, ParamClass::F128GpPair { .. }));
        let f128_save_offset: i64 = if has_f128_reg_params && !func.is_variadic {
            self.state.emit("    addi sp, sp, -128");
            for i in 0..8usize {
                self.state.emit_fmt(format_args!("    sd {}, {}(sp)", RISCV_ARG_REGS[i], i * 8));
            }
            for i in 0..8usize {
                self.state.emit_fmt(format_args!("    fsd fa{}, {}(sp)", i, 64 + i * 8));
            }
            0i64
        } else {
            0
        };

        for (i, _param) in func.params.iter().enumerate() {
            let class = param_classes[i];

            let (slot, ty) = match find_param_alloca(func, i) {
                Some((dest, ty)) => match self.state.get_slot(dest.0) {
                    Some(slot) => (slot, ty),
                    None => continue,
                },
                None => continue,
            };

            match class {
                ParamClass::IntReg { reg_idx } => {
                    // GP register param - load from save area if F128 regs were saved.
                    if has_f128_reg_params && !func.is_variadic {
                        let off = f128_save_offset + (reg_idx as i64) * 8;
                        self.state.emit_fmt(format_args!("    ld t0, {}(sp)", off));
                        let store_instr = Self::store_for_type(ty);
                        self.emit_store_to_s0("t0", slot.0, store_instr);
                    } else if func.is_variadic {
                        // For variadic, use register directly (already saved to save area).
                        let store_instr = Self::store_for_type(ty);
                        self.emit_store_to_s0(RISCV_ARG_REGS[reg_idx], slot.0, store_instr);
                    } else {
                        let store_instr = Self::store_for_type(ty);
                        self.emit_store_to_s0(RISCV_ARG_REGS[reg_idx], slot.0, store_instr);
                    }
                }
                ParamClass::FloatReg { reg_idx } => {
                    if has_f128_reg_params && !func.is_variadic {
                        // FP regs were saved to stack; load from save area.
                        let fp_off = f128_save_offset + 64 + (reg_idx as i64) * 8;
                        if ty == IrType::F32 {
                            self.state.emit_fmt(format_args!("    flw ft0, {}(sp)", fp_off));
                            self.state.emit("    fmv.x.w t0, ft0");
                        } else {
                            self.state.emit_fmt(format_args!("    fld ft0, {}(sp)", fp_off));
                            self.state.emit("    fmv.x.d t0, ft0");
                        }
                    } else if ty == IrType::F32 {
                        self.state.emit_fmt(format_args!("    fmv.x.w t0, {}", float_arg_regs[reg_idx]));
                    } else {
                        self.state.emit_fmt(format_args!("    fmv.x.d t0, {}", float_arg_regs[reg_idx]));
                    }
                    self.emit_store_to_s0("t0", slot.0, "sd");
                }
                ParamClass::I128RegPair { base_reg_idx } => {
                    if func.is_variadic {
                        let lo_off = (base_reg_idx as i64) * 8;
                        let hi_off = ((base_reg_idx + 1) as i64) * 8;
                        self.emit_load_from_s0("t0", lo_off, "ld");
                        self.emit_store_to_s0("t0", slot.0, "sd");
                        self.emit_load_from_s0("t0", hi_off, "ld");
                        self.emit_store_to_s0("t0", slot.0 + 8, "sd");
                    } else if has_f128_reg_params {
                        let lo_off = f128_save_offset + (base_reg_idx as i64) * 8;
                        let hi_off = f128_save_offset + ((base_reg_idx + 1) as i64) * 8;
                        self.state.emit_fmt(format_args!("    ld t0, {}(sp)", lo_off));
                        self.emit_store_to_s0("t0", slot.0, "sd");
                        self.state.emit_fmt(format_args!("    ld t0, {}(sp)", hi_off));
                        self.emit_store_to_s0("t0", slot.0 + 8, "sd");
                    } else {
                        self.emit_store_to_s0(RISCV_ARG_REGS[base_reg_idx], slot.0, "sd");
                        self.emit_store_to_s0(RISCV_ARG_REGS[base_reg_idx + 1], slot.0 + 8, "sd");
                    }
                }
                ParamClass::StructByValReg { base_reg_idx, size } => {
                    if has_f128_reg_params && !func.is_variadic {
                        let lo_off = f128_save_offset + (base_reg_idx as i64) * 8;
                        self.state.emit_fmt(format_args!("    ld t0, {}(sp)", lo_off));
                        self.emit_store_to_s0("t0", slot.0, "sd");
                        if size > 8 {
                            let hi_off = f128_save_offset + ((base_reg_idx + 1) as i64) * 8;
                            self.state.emit_fmt(format_args!("    ld t0, {}(sp)", hi_off));
                            self.emit_store_to_s0("t0", slot.0 + 8, "sd");
                        }
                    } else if func.is_variadic {
                        let lo_off = (base_reg_idx as i64) * 8;
                        self.emit_load_from_s0("t0", lo_off, "ld");
                        self.emit_store_to_s0("t0", slot.0, "sd");
                        if size > 8 {
                            let hi_off = ((base_reg_idx + 1) as i64) * 8;
                            self.emit_load_from_s0("t0", hi_off, "ld");
                            self.emit_store_to_s0("t0", slot.0 + 8, "sd");
                        }
                    } else {
                        self.emit_store_to_s0(RISCV_ARG_REGS[base_reg_idx], slot.0, "sd");
                        if size > 8 {
                            self.emit_store_to_s0(RISCV_ARG_REGS[base_reg_idx + 1], slot.0 + 8, "sd");
                        }
                    }
                }
                ParamClass::F128GpPair { lo_reg_idx, hi_reg_idx } => {
                    // F128 in GP register pair: store full 16-byte f128 directly to alloca.
                    // This preserves quad precision (e.g., LDBL_MIN != 0).
                    if func.is_variadic {
                        let lo_off = (lo_reg_idx as i64) * 8;
                        let hi_off = (hi_reg_idx as i64) * 8;
                        self.emit_load_from_s0("t0", lo_off, "ld");
                        self.emit_store_to_s0("t0", slot.0, "sd");
                        self.emit_load_from_s0("t0", hi_off, "ld");
                        self.emit_store_to_s0("t0", slot.0 + 8, "sd");
                    } else {
                        // Load from F128 save area.
                        let lo_off = f128_save_offset + (lo_reg_idx as i64) * 8;
                        let hi_off = f128_save_offset + (hi_reg_idx as i64) * 8;
                        self.state.emit_fmt(format_args!("    ld t0, {}(sp)", lo_off));
                        self.emit_store_to_s0("t0", slot.0, "sd");
                        self.state.emit_fmt(format_args!("    ld t0, {}(sp)", hi_off));
                        self.emit_store_to_s0("t0", slot.0 + 8, "sd");
                    }
                }
                ParamClass::F128Stack { offset } | ParamClass::F128AlwaysStack { offset } => {
                    // F128 from stack: store full 16-byte f128 directly to alloca.
                    let src = stack_base + offset;
                    self.emit_load_from_s0("t0", src, "ld");
                    self.emit_store_to_s0("t0", slot.0, "sd");
                    self.emit_load_from_s0("t0", src + 8, "ld");
                    self.emit_store_to_s0("t0", slot.0 + 8, "sd");
                }
                ParamClass::I128Stack { offset } => {
                    let src = stack_base + offset;
                    self.emit_load_from_s0("t0", src, "ld");
                    self.emit_store_to_s0("t0", slot.0, "sd");
                    self.emit_load_from_s0("t0", src + 8, "ld");
                    self.emit_store_to_s0("t0", slot.0 + 8, "sd");
                }
                ParamClass::StackScalar { offset } => {
                    let src = stack_base + offset;
                    self.emit_load_from_s0("t0", src, "ld");
                    let store_instr = Self::store_for_type(ty);
                    self.emit_store_to_s0("t0", slot.0, store_instr);
                }
                ParamClass::StructStack { offset, size } | ParamClass::LargeStructStack { offset, size } => {
                    let src = stack_base + offset;
                    let n_dwords = (size + 7) / 8;
                    for qi in 0..n_dwords {
                        let src_off = src + (qi as i64 * 8);
                        let dst_off = slot.0 + (qi as i64 * 8);
                        self.emit_load_from_s0("t0", src_off, "ld");
                        self.emit_store_to_s0("t0", dst_off, "sd");
                    }
                }
                ParamClass::LargeStructByRefReg { reg_idx, size } => {
                    // RISC-V LP64: register holds a pointer to the struct data.
                    // Copy size bytes from the pointer into the local alloca.
                    let src_reg = if has_f128_reg_params && !func.is_variadic {
                        let off = f128_save_offset + (reg_idx as i64) * 8;
                        self.state.emit_fmt(format_args!("    ld t0, {}(sp)", off));
                        "t0"
                    } else if func.is_variadic {
                        let off = (reg_idx as i64) * 8;
                        self.emit_load_from_s0("t0", off, "ld");
                        "t0"
                    } else {
                        RISCV_ARG_REGS[reg_idx]
                    };
                    // t1 = pointer to struct data
                    if src_reg != "t1" {
                        self.state.emit_fmt(format_args!("    mv t1, {}", src_reg));
                    }
                    // Large structs (>2048 bytes) have dword offsets exceeding
                    // RISC-V's 12-bit signed immediate; emit_load_from_reg handles this.
                    let n_dwords = (size + 7) / 8;
                    for qi in 0..n_dwords {
                        let src_off = (qi * 8) as i64;
                        let dst_off = slot.0 + src_off;
                        Self::emit_load_from_reg(&mut self.state, "t0", "t1", src_off, "ld");
                        self.emit_store_to_s0("t0", dst_off, "sd");
                    }
                }
                ParamClass::LargeStructByRefStack { offset, size } => {
                    // RISC-V LP64: stack slot holds a pointer to the struct data.
                    // Load the pointer, then copy the struct data into the alloca.
                    let caller_offset = stack_base + offset;
                    self.emit_load_from_s0("t1", caller_offset, "ld");
                    let n_dwords = (size + 7) / 8;
                    for qi in 0..n_dwords {
                        let src_off = (qi * 8) as i64;
                        let dst_off = slot.0 + src_off;
                        Self::emit_load_from_reg(&mut self.state, "t0", "t1", src_off, "ld");
                        self.emit_store_to_s0("t0", dst_off, "sd");
                    }
                }
                // F128 in FP reg doesn't happen on RISC-V. SysV SSE-class structs don't happen on RISC-V.
                ParamClass::F128FpReg { .. } |
                ParamClass::StructSseReg { .. } | ParamClass::StructMixedIntSseReg { .. } | ParamClass::StructMixedSseIntReg { .. } |
                ParamClass::ZeroSizeSkip => {}
            }
        }

        // Clean up the F128 save area.
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

    fn emit_param_ref(&mut self, dest: &Value, param_idx: usize, ty: IrType) {
        if param_idx >= self.state.param_classes.len() {
            return;
        }

        // Prefer loading from the param alloca slot (where emit_store_params
        // already saved the incoming register value). This avoids issues where
        // ABI registers get clobbered during emit_store_params' processing.
        if param_idx < self.state.param_alloca_slots.len() {
            if let Some((slot, alloca_ty)) = self.state.param_alloca_slots[param_idx] {
                let load_instr = Self::load_for_type(alloca_ty);
                self.emit_load_from_s0("t0", slot.0, load_instr);
                self.emit_store_result(dest);
                return;
            }
        }

        // Fallback: read directly from ABI location
        let class = self.state.param_classes[param_idx];
        let stack_base: i64 = if self.state.func_is_variadic { 64 } else { 0 };

        match class {
            ParamClass::IntReg { reg_idx } => {
                self.state.emit_fmt(format_args!("    mv t0, {}", RISCV_ARG_REGS[reg_idx]));
                self.emit_store_result(dest);
            }
            ParamClass::FloatReg { reg_idx } => {
                let float_arg_regs = ["fa0", "fa1", "fa2", "fa3", "fa4", "fa5", "fa6", "fa7"];
                if ty == IrType::F32 {
                    self.state.emit_fmt(format_args!("    fmv.x.w t0, {}", float_arg_regs[reg_idx]));
                } else {
                    self.state.emit_fmt(format_args!("    fmv.x.d t0, {}", float_arg_regs[reg_idx]));
                }
                self.store_t0_to(dest);
            }
            ParamClass::StackScalar { offset } => {
                let src = stack_base + offset;
                let load_instr = Self::load_for_type(ty);
                self.emit_load_from_s0("t0", src, load_instr);
                self.emit_store_result(dest);
            }
            // Struct/i128/F128 params remain in allocas
            _ => {}
        }
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

    fn emit_sign_extend_acc_high(&mut self) {
        self.state.emit("    srai t1, t0, 63");
    }

    fn emit_zero_acc_high(&mut self) {
        self.state.emit("    li t1, 0");
    }

    fn current_return_type(&self) -> IrType {
        self.current_return_type
    }

    fn emit_return_i128_to_regs(&mut self) {
        // i128 return: t0:t1 -> a0:a1 per RISC-V LP64D ABI
        self.state.emit("    mv a0, t0");
        self.state.emit("    mv a1, t1");
    }

    fn emit_return_f128_to_reg(&mut self) {
        // F128 return: convert f64 bit pattern to f128 via __extenddftf2.
        // Result goes in a0:a1 (GP register pair) per RISC-V LP64D ABI.
        self.state.emit("    fmv.d.x fa0, t0");
        self.state.emit("    call __extenddftf2");
    }

    fn emit_return(&mut self, val: Option<&Operand>, frame_size: i64) {
        if let Some(val) = val {
            let ret_ty = self.current_return_type();
            if ret_ty.is_long_double() {
                // Full-precision F128 return: load the full 128-bit value into a0:a1
                // (RISC-V LP64D ABI returns f128 in GP register pairs).
                // This avoids the default path which would lose precision by going
                // through an f64 approximation and __extenddftf2.
                self.emit_f128_operand_to_a0_a1(val);
                self.emit_epilogue_and_ret(frame_size);
                return;
            }
        }
        // For all non-F128 return types, use the default implementation.
        crate::backend::traits::emit_return_default(self, val, frame_size);
    }

    fn emit_return_f32_to_reg(&mut self) {
        self.state.emit("    fmv.w.x fa0, t0");
    }

    fn emit_return_f64_to_reg(&mut self) {
        self.state.emit("    fmv.d.x fa0, t0");
    }

    fn emit_return_int_to_reg(&mut self) {
        self.state.emit("    mv a0, t0");
    }

    fn emit_epilogue_and_ret(&mut self, frame_size: i64) {
        // Restore callee-saved registers before frame teardown.
        let used_regs = self.used_callee_saved.clone();
        for (i, &reg) in used_regs.iter().enumerate() {
            let offset = -(frame_size as i64) + (i as i64 * 8);
            let reg_name = callee_saved_name(reg);
            self.emit_load_from_s0(reg_name, offset, "ld");
        }
        self.emit_epilogue_riscv(frame_size);
        self.state.emit("    ret");
    }

    fn store_instr_for_type(&self, ty: IrType) -> &'static str {
        Self::store_for_type(ty)
    }

    fn load_instr_for_type(&self, ty: IrType) -> &'static str {
        Self::load_for_type(ty)
    }

    /// Override emit_store for F128: store full 16 bytes to preserve quad precision.
    /// For F128 constants, converts x87 bytes to f128 bytes directly.
    /// For F128 runtime values (f64 approx in t0), extends to f128 via __extenddftf2.
    fn emit_store(&mut self, val: &Operand, ptr: &Value, ty: IrType) {
        if ty == IrType::F128 {
            let is_indirect = !self.state.is_alloca(ptr.0);
            let addr = self.state.resolve_slot_addr(ptr.0);
            if let Some(addr) = addr {
                match addr {
                    SlotAddr::Direct(slot) if !is_indirect => {
                        // Alloca: the F128 data lives directly at the slot.
                        self.emit_f128_store_to_slot(val, slot);
                    }
                    SlotAddr::Direct(slot) => {
                        // Non-alloca pointer (e.g. GEP result): the slot holds
                        // an 8-byte pointer; dereference it to store F128.
                        self.emit_f128_store_indirect(val, slot, ptr.0);
                    }
                    SlotAddr::OverAligned(slot, id) => {
                        // Compute aligned address into t5, then store 16 bytes
                        self.emit_f128_store_to_slot_aligned(val, slot, id);
                    }
                    SlotAddr::Indirect(slot) => {
                        // ptr is in a slot; load it, then store 16 bytes
                        self.emit_f128_store_indirect(val, slot, ptr.0);
                    }
                }
            }
            return;
        }
        crate::backend::traits::emit_store_default(self, val, ptr, ty);
    }

    /// Override emit_load for F128: load full 16 bytes from source, copy to dest's
    /// 16-byte slot. Also convert to f64 in t0 for non-F128-aware consumers.
    fn emit_load(&mut self, dest: &Value, ptr: &Value, ty: IrType) {
        if ty == IrType::F128 {
            // Track which alloca+offset this F128 value was loaded from, so that
            // emit_f128_operand_to_a0_a1 can load the full 16-byte f128 directly
            // from the original memory for comparisons (avoiding f64 precision loss).
            // Track source for full-precision reload. Automatically determines
            // is_indirect based on whether ptr is an alloca.
            self.state.track_f128_load(dest.0, ptr.0, 0);
            let is_indirect = !self.state.is_alloca(ptr.0);

            let addr = self.state.resolve_slot_addr(ptr.0);
            if let Some(addr) = addr {
                // Load the full 16-byte f128 from the source into a0:a1
                match addr {
                    SlotAddr::Direct(slot) if !is_indirect => {
                        // Alloca: the F128 data lives directly at the slot.
                        self.emit_load_from_s0("a0", slot.0, "ld");
                        self.emit_load_from_s0("a1", slot.0 + 8, "ld");
                    }
                    SlotAddr::Direct(slot) => {
                        // Non-alloca pointer (e.g. GEP result): the slot holds
                        // an 8-byte pointer that must be dereferenced to get F128.
                        self.emit_load_from_s0("t5", slot.0, "ld");
                        self.state.emit("    ld a0, 0(t5)");
                        self.state.emit("    ld a1, 8(t5)");
                    }
                    SlotAddr::OverAligned(slot, id) => {
                        self.emit_alloca_aligned_addr(slot, id);
                        self.state.emit("    ld a0, 0(t5)");
                        self.state.emit("    ld a1, 8(t5)");
                    }
                    SlotAddr::Indirect(slot) => {
                        self.emit_load_ptr_from_slot(slot, ptr.0);
                        self.state.emit("    ld a0, 0(t5)");
                        self.state.emit("    ld a1, 8(t5)");
                    }
                }
                // Convert f128 (a0:a1) to f64 via __trunctfdf2, store f64 in dest slot.
                // This keeps the normal t0-based data flow working.
                self.state.emit("    call __trunctfdf2");
                self.state.emit("    fmv.x.d t0, fa0");
                self.state.reg_cache.invalidate_all();
                self.store_t0_to(dest);
            }
            return;
        }
        crate::backend::traits::emit_load_default(self, dest, ptr, ty);
    }

    /// Override emit_store_with_const_offset for F128: store full 16 bytes at offset.
    fn emit_store_with_const_offset(&mut self, val: &Operand, base: &Value, offset: i64, ty: IrType) {
        if ty == IrType::F128 {
            let is_indirect = !self.state.is_alloca(base.0);

            // Check if the base pointer lives in a callee-saved register.
            if let Some(&reg) = self.reg_assignments.get(&base.0) {
                let reg_name = callee_saved_name(reg);
                self.state.emit_fmt(format_args!("    mv t5, {}", reg_name));
                if offset != 0 {
                    self.emit_add_offset_to_addr_reg(offset);
                }
                self.emit_f128_store_to_addr_in_t5(val);
                return;
            }

            let addr = self.state.resolve_slot_addr(base.0);
            if let Some(addr) = addr {
                match addr {
                    SlotAddr::Direct(slot) if !is_indirect => {
                        // Alloca: the F128 data lives directly at the slot.
                        let folded_slot = StackSlot(slot.0 + offset);
                        self.emit_f128_store_to_slot(val, folded_slot);
                    }
                    SlotAddr::Direct(slot) => {
                        // Non-alloca pointer: dereference, then add offset.
                        self.emit_load_from_s0("t5", slot.0, "ld");
                        if offset != 0 {
                            self.emit_add_offset_to_addr_reg(offset);
                        }
                        self.emit_f128_store_to_addr_in_t5(val);
                    }
                    SlotAddr::OverAligned(slot, id) => {
                        self.emit_alloca_aligned_addr(slot, id);
                        self.emit_add_offset_to_addr_reg(offset);
                        self.emit_f128_store_to_addr_in_t5(val);
                    }
                    SlotAddr::Indirect(slot) => {
                        self.emit_load_ptr_from_slot(slot, base.0);
                        if offset != 0 {
                            self.emit_add_offset_to_addr_reg(offset);
                        }
                        self.emit_f128_store_to_addr_in_t5(val);
                    }
                }
            }
            return;
        }
        // For non-F128, emit the operand then use the default path
        self.emit_load_operand(val);
        let addr = self.state_ref().resolve_slot_addr(base.0);
        if let Some(addr) = addr {
            let store_instr = self.store_instr_for_type(ty);
            match addr {
                SlotAddr::OverAligned(slot, id) => {
                    self.emit_save_acc();
                    self.emit_alloca_aligned_addr(slot, id);
                    self.emit_add_offset_to_addr_reg(offset);
                    self.emit_typed_store_indirect(store_instr, ty);
                }
                SlotAddr::Direct(slot) => {
                    let folded_slot = StackSlot(slot.0 + offset);
                    self.emit_typed_store_to_slot(store_instr, ty, folded_slot);
                }
                SlotAddr::Indirect(slot) => {
                    self.emit_save_acc();
                    self.emit_load_ptr_from_slot(slot, base.0);
                    if offset != 0 {
                        self.emit_add_offset_to_addr_reg(offset);
                    }
                    self.emit_typed_store_indirect(store_instr, ty);
                }
            }
        }
    }

    /// Override emit_load_with_const_offset for F128: load full 16 bytes at offset.
    fn emit_load_with_const_offset(&mut self, dest: &Value, base: &Value, offset: i64, ty: IrType) {
        if ty == IrType::F128 {
            // Track the source alloca + offset for full-precision comparison access.
            self.state.track_f128_load(dest.0, base.0, offset);
            let is_indirect = !self.state.is_alloca(base.0);

            // Check if the base pointer lives in a callee-saved register.
            let loaded = if let Some(&reg) = self.reg_assignments.get(&base.0) {
                let reg_name = callee_saved_name(reg);
                self.state.emit_fmt(format_args!("    mv t5, {}", reg_name));
                if offset != 0 {
                    self.emit_add_offset_to_addr_reg(offset);
                }
                self.emit_f128_load_from_addr_in_t5();
                true
            } else {
                let addr = self.state.resolve_slot_addr(base.0);
                if let Some(addr) = addr {
                    match addr {
                        SlotAddr::Direct(slot) if !is_indirect => {
                            // Alloca: the F128 data lives directly at the slot.
                            let folded_slot = StackSlot(slot.0 + offset);
                            self.emit_f128_load_from_slot(folded_slot);
                        }
                        SlotAddr::Direct(slot) => {
                            // Non-alloca pointer: dereference, then add offset.
                            self.emit_load_from_s0("t5", slot.0, "ld");
                            if offset != 0 {
                                self.emit_add_offset_to_addr_reg(offset);
                            }
                            self.emit_f128_load_from_addr_in_t5();
                        }
                        SlotAddr::OverAligned(slot, id) => {
                            self.emit_alloca_aligned_addr(slot, id);
                            self.emit_add_offset_to_addr_reg(offset);
                            self.emit_f128_load_from_addr_in_t5();
                        }
                        SlotAddr::Indirect(slot) => {
                            self.emit_load_ptr_from_slot(slot, base.0);
                            if offset != 0 {
                                self.emit_add_offset_to_addr_reg(offset);
                            }
                            self.emit_f128_load_from_addr_in_t5();
                        }
                    }
                    true
                } else {
                    false
                }
            };
            if loaded {
                // t0 now has f64 bits from __trunctfdf2
                self.store_t0_to(dest);
            }
            return;
        }
        // Default path for non-F128
        let addr = self.state_ref().resolve_slot_addr(base.0);
        if let Some(addr) = addr {
            let load_instr = self.load_instr_for_type(ty);
            match addr {
                SlotAddr::OverAligned(slot, id) => {
                    self.emit_alloca_aligned_addr(slot, id);
                    self.emit_add_offset_to_addr_reg(offset);
                    self.emit_typed_load_indirect(load_instr);
                }
                SlotAddr::Direct(slot) => {
                    let folded_slot = StackSlot(slot.0 + offset);
                    self.emit_typed_load_from_slot(load_instr, folded_slot);
                }
                SlotAddr::Indirect(slot) => {
                    self.emit_load_ptr_from_slot(slot, base.0);
                    if offset != 0 {
                        self.emit_add_offset_to_addr_reg(offset);
                    }
                    self.emit_typed_load_indirect(load_instr);
                }
            }
            self.emit_store_result(dest);
        }
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

    fn emit_load_ptr_from_slot(&mut self, slot: StackSlot, val_id: u32) {
        // Check register allocation: use callee-saved register if available.
        if let Some(&reg) = self.reg_assignments.get(&val_id) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mv t5, {}", reg_name));
        } else {
            self.emit_load_from_s0("t5", slot.0, "ld");
        }
    }

    fn emit_typed_store_indirect(&mut self, instr: &'static str, _ty: IrType) {
        // val saved in t3, ptr in t5
        self.state.emit_fmt(format_args!("    {} t3, 0(t5)", instr));
    }

    fn emit_typed_load_indirect(&mut self, instr: &'static str) {
        // ptr in t5
        self.state.emit_fmt(format_args!("    {} t0, 0(t5)", instr));
    }

    fn emit_add_offset_to_addr_reg(&mut self, offset: i64) {
        if Self::fits_imm12(offset) {
            self.state.emit_fmt(format_args!("    addi t5, t5, {}", offset));
        } else {
            self.state.emit_fmt(format_args!("    li t6, {}", offset));
            self.state.emit("    add t5, t5, t6");
        }
    }

    fn emit_slot_addr_to_secondary(&mut self, slot: StackSlot, is_alloca: bool, val_id: u32) {
        if is_alloca {
            self.emit_addi_s0("t1", slot.0);
        } else if let Some(&reg) = self.reg_assignments.get(&val_id) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mv t1, {}", reg_name));
        } else {
            self.emit_load_from_s0("t1", slot.0, "ld");
        }
    }

    fn emit_add_secondary_to_acc(&mut self) {
        self.state.emit("    add t0, t1, t0");
    }

    /// Optimized GEP for alloca base + constant offset.
    /// Computes s0 + (slot_offset + gep_offset) directly into t0 (accumulator).
    fn emit_gep_direct_const(&mut self, slot: StackSlot, offset: i64) {
        let folded = slot.0 + offset;
        // Reuse emit_addi_s0 which handles large offsets via li+add
        self.emit_addi_s0("t0", folded);
    }

    /// Optimized GEP for pointer-in-slot + constant offset.
    /// Loads the pointer, then adds the constant offset using addi when possible.
    fn emit_gep_indirect_const(&mut self, slot: StackSlot, offset: i64, val_id: u32) {
        // Load the base pointer into t0
        if let Some(&reg) = self.reg_assignments.get(&val_id) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mv t0, {}", reg_name));
        } else {
            self.emit_load_from_s0("t0", slot.0, "ld");
        }
        // Add constant offset using addi (skip if zero)
        if offset != 0 {
            self.emit_add_imm_to_acc(offset);
        }
    }

    /// Add a constant offset to the accumulator (t0) using addi.
    fn emit_gep_add_const_to_acc(&mut self, offset: i64) {
        if offset != 0 {
            self.emit_add_imm_to_acc(offset);
        }
    }

    fn emit_add_imm_to_acc(&mut self, imm: i64) {
        if imm >= -2048 && imm <= 2047 {
            self.state.emit_fmt(format_args!("    addi t0, t0, {}", imm));
        } else {
            // Large immediate: load into t1 (secondary), then add
            self.state.emit_fmt(format_args!("    li t1, {}", imm));
            self.state.emit("    add t0, t0, t1");
        }
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

    fn emit_mov_acc_to_sp(&mut self) {
        self.state.emit("    mv sp, t0");
    }

    fn emit_align_acc(&mut self, align: usize) {
        self.state.emit_fmt(format_args!("    addi t0, t0, {}", align - 1));
        self.state.emit_fmt(format_args!("    andi t0, t0, -{}", align));
    }

    fn emit_memcpy_load_dest_addr(&mut self, slot: StackSlot, is_alloca: bool, val_id: u32) {
        if is_alloca {
            self.emit_addi_s0("t1", slot.0);
        } else if let Some(&reg) = self.reg_assignments.get(&val_id) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mv t1, {}", reg_name));
        } else {
            self.emit_load_from_s0("t1", slot.0, "ld");
        }
    }

    fn emit_memcpy_load_src_addr(&mut self, slot: StackSlot, is_alloca: bool, val_id: u32) {
        if is_alloca {
            self.emit_addi_s0("t2", slot.0);
        } else if let Some(&reg) = self.reg_assignments.get(&val_id) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mv t2, {}", reg_name));
        } else {
            self.emit_load_from_s0("t2", slot.0, "ld");
        }
    }

    fn emit_alloca_aligned_addr(&mut self, slot: StackSlot, val_id: u32) {
        let align = self.state.alloca_over_align(val_id).unwrap();
        // Compute s0 + slot_offset into t5 (pointer register)
        self.emit_addi_s0("t5", slot.0);
        // Align: t5 = (t5 + align-1) & -align
        self.state.emit_fmt(format_args!("    li t6, {}", align - 1));
        self.state.emit("    add t5, t5, t6");
        self.state.emit_fmt(format_args!("    li t6, -{}", align));
        self.state.emit("    and t5, t5, t6");
    }

    fn emit_alloca_aligned_addr_to_acc(&mut self, slot: StackSlot, val_id: u32) {
        let align = self.state.alloca_over_align(val_id).unwrap();
        // Compute s0 + slot_offset into t0 (accumulator)
        self.emit_addi_s0("t0", slot.0);
        // Align: t0 = (t0 + align-1) & -align
        self.state.emit_fmt(format_args!("    li t6, {}", align - 1));
        self.state.emit("    add t0, t0, t6");
        self.state.emit_fmt(format_args!("    li t6, -{}", align));
        self.state.emit("    and t0, t0, t6");
        // t0 now holds an aligned address, not any previous SSA value.
        self.state.reg_cache.invalidate_acc();
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
        self.state.emit_fmt(format_args!("    li t3, {}", size));
        self.state.emit_fmt(format_args!("{}:", loop_label));
        self.state.emit_fmt(format_args!("    beqz t3, {}", done_label));
        self.state.emit("    lbu t4, 0(t2)");
        self.state.emit("    sb t4, 0(t1)");
        self.state.emit("    addi t1, t1, 1");
        self.state.emit("    addi t2, t2, 1");
        self.state.emit("    addi t3, t3, -1");
        self.state.emit_fmt(format_args!("    j {}", loop_label));
        self.state.emit_fmt(format_args!("{}:", done_label));
    }

    fn emit_float_neg(&mut self, ty: IrType) {
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

    fn emit_f128_neg(&mut self, dest: &Value, src: &Operand) {
        self.emit_f128_neg_full(dest, src);
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

    // emit_float_binop uses the shared default implementation

    fn emit_int_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        // Note: i128 dispatch is handled by the shared emit_binop default in traits.rs.
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
        self.state.emit_fmt(format_args!("    {} t0, t1, t2", mnemonic));

        self.store_t0_to(dest);
    }

    fn emit_float_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        // Float comparison (F32/F64): load operands into t1/t2, then move to float regs.
        self.operand_to_t0(lhs);
        self.state.emit("    mv t1, t0");
        self.operand_to_t0(rhs);
        self.state.emit("    mv t2, t0");
        let s = if ty == IrType::F64 { "d" } else { "s" };
        let fmv = if s == "d" { "fmv.d.x" } else { "fmv.w.x" };
        self.state.emit_fmt(format_args!("    {} ft0, t1", fmv));
        self.state.emit_fmt(format_args!("    {} ft1, t2", fmv));
        match op {
            IrCmpOp::Eq => self.state.emit_fmt(format_args!("    feq.{} t0, ft0, ft1", s)),
            IrCmpOp::Ne => {
                self.state.emit_fmt(format_args!("    feq.{} t0, ft0, ft1", s));
                self.state.emit("    xori t0, t0, 1");
            }
            IrCmpOp::Slt | IrCmpOp::Ult => self.state.emit_fmt(format_args!("    flt.{} t0, ft0, ft1", s)),
            IrCmpOp::Sle | IrCmpOp::Ule => self.state.emit_fmt(format_args!("    fle.{} t0, ft0, ft1", s)),
            IrCmpOp::Sgt | IrCmpOp::Ugt => self.state.emit_fmt(format_args!("    flt.{} t0, ft1, ft0", s)),
            IrCmpOp::Sge | IrCmpOp::Uge => self.state.emit_fmt(format_args!("    fle.{} t0, ft1, ft0", s)),
        }
        self.store_t0_to(dest);
    }

    fn emit_f128_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand) {
        crate::backend::f128_softfloat::f128_cmp(self, dest, op, lhs, rhs);
    }

    fn emit_int_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        // Integer comparison: load + sign/zero-extend, then compare
        self.emit_cmp_operand_load(lhs, rhs, ty);
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

    /// Fused compare-and-branch for RISC-V: emit branch instructions directly.
    /// RISC-V B-type branches (beq/bne/blt/bge/bltu/bgeu) have +-4KB range,
    /// which is easily exceeded in large functions (e.g., oniguruma's match_at).
    /// We invert the condition to skip over a long-range `jump` pseudo.
    fn emit_fused_cmp_branch(
        &mut self,
        op: IrCmpOp,
        lhs: &Operand,
        rhs: &Operand,
        ty: IrType,
        true_label: &str,
        false_label: &str,
    ) {
        // Load operands into t1, t2 (with sign/zero-extension for sub-64-bit types)
        self.emit_cmp_operand_load(lhs, rhs, ty);

        // Emit inverted branch to skip over the true-path jump.
        // Original: beq t1, t2, true_label  (+-4KB range, can fail)
        // Relaxed:  bne t1, t2, .Lskip      (short forward skip)
        //           jump true_label, t6      (+-2GB range)
        //           .Lskip:
        //           jump false_label, t6     (+-2GB range)
        let (inv_branch, r1, r2) = match op {
            IrCmpOp::Eq  => ("bne",  "t1", "t2"),
            IrCmpOp::Ne  => ("beq",  "t1", "t2"),
            IrCmpOp::Slt => ("bge",  "t1", "t2"),
            IrCmpOp::Sge => ("blt",  "t1", "t2"),
            IrCmpOp::Ult => ("bgeu", "t1", "t2"),
            IrCmpOp::Uge => ("bltu", "t1", "t2"),
            // Swap operands for > and <=, and invert
            IrCmpOp::Sgt => ("bge",  "t2", "t1"),  // NOT(a > b)  ≡  b >= a
            IrCmpOp::Sle => ("blt",  "t2", "t1"),  // NOT(a <= b) ≡  b < a
            IrCmpOp::Ugt => ("bgeu", "t2", "t1"),  // NOT(a > b)  ≡  b >= a (unsigned)
            IrCmpOp::Ule => ("bltu", "t2", "t1"),  // NOT(a <= b) ≡  b < a (unsigned)
        };
        let skip = self.state.fresh_label("skip");
        self.state.emit_fmt(format_args!("    {} {}, {}, {}", inv_branch, r1, r2, skip));
        self.state.emit_fmt(format_args!("    jump {}, t6", true_label));
        self.state.emit_fmt(format_args!("{}:", skip));
        self.state.emit_fmt(format_args!("    jump {}, t6", false_label));
        self.state.reg_cache.invalidate_all();
    }

    /// Emit conditional select for RISC-V using a compact branch sequence.
    ///
    /// RISC-V doesn't have cmov. We use a 4-instruction branchless-ish sequence:
    ///   1. Load false_val into t0 (default)
    ///   2. Load condition into t1
    ///   3. beqz t1, skip  (if cond == 0, keep false_val)
    ///   4. Load true_val into t0
    ///   skip:
    ///   5. Store t0 to dest
    fn emit_select(&mut self, dest: &Value, cond: &Operand, true_val: &Operand, false_val: &Operand, _ty: IrType) {
        let label_id = self.state.next_label_id();
        let skip_label = format!(".Lsel_skip_{}", label_id);

        // Load false_val (default) into t0
        self.operand_to_t0(false_val);
        // Save to t2 so we can test the condition
        self.state.emit("    mv t2, t0");

        // Load condition
        self.operand_to_t0(cond);
        // Branch to skip if condition is zero (keep false_val)
        self.state.emit_fmt(format_args!("    beqz t0, {}", skip_label));

        // Load true_val into t2 (overrides the false_val)
        self.operand_to_t0(true_val);
        self.state.emit("    mv t2, t0");

        // Skip label
        self.state.emit_fmt(format_args!("{}:", skip_label));

        // Move result to t0 and store
        self.state.emit("    mv t0, t2");
        self.state.reg_cache.invalidate_acc();
        self.store_t0_to(dest);
    }

    // emit_call: uses shared default from ArchCodegen trait (traits.rs)

    fn call_abi_config(&self) -> CallAbiConfig {
        CallAbiConfig {
            max_int_regs: 8, max_float_regs: 8,
            align_i128_pairs: true,
            f128_in_fp_regs: false, f128_in_gp_pairs: true,
            variadic_floats_in_gp: true,
            large_struct_by_ref: true, // RISC-V LP64: composites > 16 bytes passed by reference (pointer in GP reg)
            use_sysv_struct_classification: false, // RISC-V uses its own ABI, not SysV
        }
    }

    fn emit_call_compute_stack_space(&self, arg_classes: &[CallArgClass], _arg_types: &[IrType]) -> usize {
        compute_stack_arg_space(arg_classes)
    }

    // emit_call_spill_fptr: uses default no-op (RISC-V loads fptr inline before jalr)
    // emit_call_fptr_spill_size: uses default 0

    fn emit_call_f128_pre_convert(&mut self, args: &[Operand], arg_classes: &[CallArgClass],
                                   _arg_types: &[IrType], _stack_arg_space: usize) -> usize {
        // RISC-V: pre-convert F128 value args to full 128-bit form, saving to temp stack.
        // We use emit_f128_operand_to_a0_a1 which preserves full precision by checking
        // the f128 source tracking, rather than truncating to f64 and re-extending.
        let mut f128_temp_space: i64 = 0;
        for (i, arg) in args.iter().enumerate() {
            if matches!(arg_classes[i], CallArgClass::F128Reg { .. }) {
                if let Operand::Value(_) = arg {
                    f128_temp_space += 16;
                }
            }
        }
        if f128_temp_space > 0 {
            self.emit_addi_sp(-f128_temp_space);
            let mut temp_offset: i64 = 0;
            for (i, arg) in args.iter().enumerate() {
                if !matches!(arg_classes[i], CallArgClass::F128Reg { .. }) { continue; }
                if let Operand::Value(_) = arg {
                    // Use the full-precision f128 loading path (checks tracking, falls back to extend)
                    self.emit_f128_operand_to_a0_a1(arg);
                    self.emit_store_to_sp("a0", temp_offset, "sd");
                    self.emit_store_to_sp("a1", temp_offset + 8, "sd");
                    temp_offset += 16;
                }
            }
        }
        f128_temp_space as usize
    }

    fn emit_call_stack_args(&mut self, args: &[Operand], arg_classes: &[CallArgClass],
                            _arg_types: &[IrType], stack_arg_space: usize, _fptr_spill: usize, _f128_temp_space: usize) -> i64 {
        if stack_arg_space > 0 {
            self.emit_addi_sp(-(stack_arg_space as i64));
            let mut offset: usize = 0;
            for (arg_i, arg) in args.iter().enumerate() {
                if !arg_classes[arg_i].is_stack() { continue; }
                match arg_classes[arg_i] {
                    CallArgClass::StructByValStack { size } | CallArgClass::LargeStructStack { size } => {
                        let n_dwords = (size + 7) / 8;
                        match arg {
                            Operand::Value(v) => {
                                if let Some(&reg) = self.reg_assignments.get(&v.0) {
                                    // Value is register-allocated: move from callee-saved reg to t0
                                    let reg_name = callee_saved_name(reg);
                                    self.state.emit_fmt(format_args!("    mv t0, {}", reg_name));
                                } else if let Some(slot) = self.state.get_slot(v.0) {
                                    if self.state.is_alloca(v.0) {
                                        self.emit_addi_s0("t0", slot.0);
                                    } else {
                                        self.emit_load_from_s0("t0", slot.0, "ld");
                                    }
                                } else {
                                    self.state.emit("    li t0, 0");
                                }
                            }
                            Operand::Const(_) => { self.operand_to_t0(arg); }
                        }
                        for qi in 0..n_dwords {
                            let src_off = (qi * 8) as i64;
                            Self::emit_load_from_reg(&mut self.state, "t1", "t0", src_off, "ld");
                            self.emit_store_to_sp("t1", offset as i64 + src_off, "sd");
                        }
                        offset += n_dwords * 8;
                    }
                    CallArgClass::I128Stack => {
                        offset = (offset + 15) & !15;
                        match arg {
                            Operand::Const(c) => {
                                if let IrConst::I128(v) = c {
                                    self.state.emit_fmt(format_args!("    li t0, {}", *v as u64 as i64));
                                    self.emit_store_to_sp("t0", offset as i64, "sd");
                                    self.state.emit_fmt(format_args!("    li t0, {}", (*v >> 64) as u64 as i64));
                                    self.emit_store_to_sp("t0", (offset + 8) as i64, "sd");
                                } else {
                                    self.operand_to_t0(arg);
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
                    }
                    CallArgClass::F128Stack => {
                        offset = (offset + 15) & !15;
                        match arg {
                            Operand::Const(ref c) => {
                                // Use full f128 bytes from the LongDouble constant when available
                                let bytes = match c {
                                    IrConst::LongDouble(_, f128_bytes) => f128_bytes.clone(),
                                    _ => {
                                        let f64_val = c.to_f64().unwrap_or(0.0);
                                        crate::ir::ir::f64_to_f128_bytes(f64_val)
                                    }
                                };
                                let lo = i64::from_le_bytes(bytes[0..8].try_into().unwrap());
                                let hi = i64::from_le_bytes(bytes[8..16].try_into().unwrap());
                                self.state.emit_fmt(format_args!("    li t0, {}", lo));
                                self.emit_store_to_sp("t0", offset as i64, "sd");
                                self.state.emit_fmt(format_args!("    li t0, {}", hi));
                                self.emit_store_to_sp("t0", (offset + 8) as i64, "sd");
                            }
                            Operand::Value(_) => {
                                // Load full f128 using precision-preserving path (checks tracking)
                                self.emit_f128_operand_to_a0_a1(arg);
                                self.emit_store_to_sp("a0", offset as i64, "sd");
                                self.emit_store_to_sp("a1", (offset + 8) as i64, "sd");
                            }
                        }
                        offset += 16;
                    }
                    CallArgClass::Stack => {
                        self.operand_to_t0(arg);
                        self.emit_store_to_sp("t0", offset as i64, "sd");
                        offset += 8;
                    }
                    _ => {}
                }
            }
        }
        // RISC-V doesn't spill fptr to stack, so total_sp_adjust is just stack + f128 temp
        0 // RISC-V loads operands from s0-relative slots (not SP-relative), so no SP adjust needed
    }

    fn emit_call_reg_args(&mut self, args: &[Operand], arg_classes: &[CallArgClass],
                          arg_types: &[IrType], _total_sp_adjust: i64, _f128_temp_space: usize, stack_arg_space: usize) {
        let float_arg_regs = ["fa0", "fa1", "fa2", "fa3", "fa4", "fa5", "fa6", "fa7"];

        // GP args go to temps first, then FP args, then move GP from temp to aX.
        let temp_regs = ["t3", "t4", "t5", "s2", "s3", "s4", "s5", "s6"];
        let mut gp_temps: Vec<(usize, &str)> = Vec::new();
        let mut temp_i = 0usize;
        for (i, arg) in args.iter().enumerate() {
            match arg_classes[i] {
                CallArgClass::IntReg { reg_idx } => {
                    self.operand_to_t0(arg);
                    if temp_i < temp_regs.len() {
                        self.state.emit_fmt(format_args!("    mv {}, t0", temp_regs[temp_i]));
                        gp_temps.push((reg_idx, temp_regs[temp_i]));
                        temp_i += 1;
                    }
                }
                CallArgClass::FloatReg { reg_idx } => {
                    self.operand_to_t0(arg);
                    let arg_ty = if i < arg_types.len() { Some(arg_types[i]) } else { None };
                    if arg_ty == Some(IrType::F32) {
                        self.state.emit_fmt(format_args!("    fmv.w.x {}, t0", float_arg_regs[reg_idx]));
                    } else {
                        self.state.emit_fmt(format_args!("    fmv.d.x {}, t0", float_arg_regs[reg_idx]));
                    }
                }
                _ => {}
            }
        }

        // Move GP args from temps to actual arg registers
        for (target_idx, temp_reg) in &gp_temps {
            self.state.emit_fmt(format_args!("    mv {}, {}", RISCV_ARG_REGS[*target_idx], temp_reg));
        }

        // Load F128 register pair args
        // Need to rebuild f128_var_temps info to find temp stack offsets
        let mut f128_var_temp_offset: i64 = 0;
        for (i, arg) in args.iter().enumerate() {
            if let CallArgClass::F128Reg { reg_idx: base_reg } = arg_classes[i] {
                match arg {
                    Operand::Const(ref c) => {
                        // Use full f128 bytes from the LongDouble constant when available
                        let bytes = match c {
                            IrConst::LongDouble(_, f128_bytes) => f128_bytes.clone(),
                            _ => {
                                let f64_val = c.to_f64().unwrap_or(0.0);
                                crate::ir::ir::f64_to_f128_bytes(f64_val)
                            }
                        };
                        let lo = i64::from_le_bytes(bytes[0..8].try_into().unwrap());
                        let hi = i64::from_le_bytes(bytes[8..16].try_into().unwrap());
                        self.state.emit_fmt(format_args!("    li {}, {}", RISCV_ARG_REGS[base_reg], lo));
                        self.state.emit_fmt(format_args!("    li {}, {}", RISCV_ARG_REGS[base_reg + 1], hi));
                    }
                    Operand::Value(_) => {
                        let offset = f128_var_temp_offset + stack_arg_space as i64;
                        self.emit_load_from_sp(RISCV_ARG_REGS[base_reg], offset, "ld");
                        self.emit_load_from_sp(RISCV_ARG_REGS[base_reg + 1], offset + 8, "ld");
                        f128_var_temp_offset += 16;
                    }
                }
            }
        }

        // Load i128 register pair args
        for (i, arg) in args.iter().enumerate() {
            if let CallArgClass::I128RegPair { base_reg_idx } = arg_classes[i] {
                match arg {
                    Operand::Const(c) => {
                        if let IrConst::I128(v) = c {
                            self.state.emit_fmt(format_args!("    li {}, {}", RISCV_ARG_REGS[base_reg_idx], *v as u64 as i64));
                            self.state.emit_fmt(format_args!("    li {}, {}", RISCV_ARG_REGS[base_reg_idx + 1], (*v >> 64) as u64 as i64));
                        } else {
                            self.operand_to_t0(arg);
                            self.state.emit_fmt(format_args!("    mv {}, t0", RISCV_ARG_REGS[base_reg_idx]));
                            self.state.emit_fmt(format_args!("    mv {}, zero", RISCV_ARG_REGS[base_reg_idx + 1]));
                        }
                    }
                    Operand::Value(v) => {
                        if let Some(slot) = self.state.get_slot(v.0) {
                            if self.state.is_alloca(v.0) {
                                self.emit_addi_s0(RISCV_ARG_REGS[base_reg_idx], slot.0);
                                self.state.emit_fmt(format_args!("    mv {}, zero", RISCV_ARG_REGS[base_reg_idx + 1]));
                            } else {
                                self.emit_load_from_s0(RISCV_ARG_REGS[base_reg_idx], slot.0, "ld");
                                self.emit_load_from_s0(RISCV_ARG_REGS[base_reg_idx + 1], slot.0 + 8, "ld");
                            }
                        }
                    }
                }
            }
        }

        // Load struct-by-value register args
        for (i, arg) in args.iter().enumerate() {
            if let CallArgClass::StructByValReg { base_reg_idx, size } = arg_classes[i] {
                let regs_needed = if size <= 8 { 1 } else { 2 };
                match arg {
                    Operand::Value(v) => {
                        if let Some(&reg) = self.reg_assignments.get(&v.0) {
                            // Value is register-allocated: move from callee-saved reg to t0
                            let reg_name = callee_saved_name(reg);
                            self.state.emit_fmt(format_args!("    mv t0, {}", reg_name));
                        } else if let Some(slot) = self.state.get_slot(v.0) {
                            if self.state.is_alloca(v.0) {
                                self.emit_addi_s0("t0", slot.0);
                            } else {
                                self.emit_load_from_s0("t0", slot.0, "ld");
                            }
                        } else {
                            self.state.emit("    li t0, 0");
                        }
                    }
                    Operand::Const(_) => { self.operand_to_t0(arg); }
                }
                self.state.emit_fmt(format_args!("    ld {}, 0(t0)", RISCV_ARG_REGS[base_reg_idx]));
                if regs_needed > 1 {
                    self.state.emit_fmt(format_args!("    ld {}, 8(t0)", RISCV_ARG_REGS[base_reg_idx + 1]));
                }
            }
        }
    }

    fn emit_call_instruction(&mut self, direct_name: Option<&str>, func_ptr: Option<&Operand>, _indirect: bool, _stack_arg_space: usize) {
        if let Some(name) = direct_name {
            self.state.emit_fmt(format_args!("    call {}", name));
        } else if let Some(ptr) = func_ptr {
            self.operand_to_t0(ptr);
            self.state.emit("    mv t2, t0");
            self.state.emit("    jalr ra, t2, 0");
        }
    }

    fn emit_call_cleanup(&mut self, stack_arg_space: usize, f128_temp_space: usize, _indirect: bool) {
        // Clean up F128 temp space if no stack args below it (already cleaned in that case)
        if f128_temp_space > 0 && stack_arg_space == 0 {
            self.emit_addi_sp(f128_temp_space as i64);
        }
        if stack_arg_space > 0 {
            let cleanup = stack_arg_space as i64 + f128_temp_space as i64;
            self.emit_addi_sp(cleanup);
        }
    }

    fn emit_call_store_result(&mut self, dest: &Value, return_type: IrType) {
        // RISC-V returns integers in a0 (not t0), so we need custom handling
        // to store a0 directly without an unnecessary a0->t0 move.
        if is_i128_type(return_type) {
            if let Some(slot) = self.state.get_slot(dest.0) {
                self.emit_store_to_s0("a0", slot.0, "sd");
                self.emit_store_to_s0("a1", slot.0 + 8, "sd");
            }
        } else if return_type.is_long_double() {
            // F128 return value is in a0:a1 (lo:hi GP register pair).
            // Store full 16-byte f128 to the stack slot first (before any calls clobber a0:a1).
            if let Some(slot) = self.state.get_slot(dest.0) {
                self.emit_store_to_s0("a0", slot.0, "sd");
                self.emit_store_to_s0("a1", slot.0 + 8, "sd");
            }
            // Produce f64 approximation for register-based data flow
            self.state.emit("    call __trunctfdf2");
            self.state.emit("    fmv.x.d t0, fa0");
            self.state.reg_cache.invalidate_all();
            // Track that dest has full f128 in its slot
            self.state.track_f128_self(dest.0);
            // Store f64 approx only to callee-saved register (not stack slot)
            if let Some(&reg) = self.reg_assignments.get(&dest.0) {
                let reg_name = callee_saved_name(reg);
                self.state.emit_fmt(format_args!("    mv {}, t0", reg_name));
            }
        } else if return_type == IrType::F32 {
            if let Some(slot) = self.state.get_slot(dest.0) {
                self.state.emit("    fmv.x.w t0, fa0");
                self.emit_store_to_s0("t0", slot.0, "sd");
            }
        } else if return_type.is_float() {
            if let Some(slot) = self.state.get_slot(dest.0) {
                self.state.emit("    fmv.x.d t0, fa0");
                self.emit_store_to_s0("t0", slot.0, "sd");
            }
        } else {
            // Integer return value in a0. Check for register allocation first:
            // if the value has a callee-saved register, move a0 directly to it
            // (avoiding a stack spill). Otherwise store to the stack slot.
            if let Some(&reg) = self.reg_assignments.get(&dest.0) {
                let reg_name = callee_saved_name(reg);
                self.state.emit_fmt(format_args!("    mv {}, a0", reg_name));
            } else if let Some(slot) = self.state.get_slot(dest.0) {
                self.emit_store_to_s0("a0", slot.0, "sd");
            }
        }
    }

    fn emit_call_store_i128_result(&mut self, _dest: &Value) {
        // Handled by the override above
        unreachable!("RISC-V uses custom emit_call_store_result");
    }

    fn emit_call_store_f128_result(&mut self, _dest: &Value) {
        unreachable!("RISC-V uses custom emit_call_store_result");
    }

    fn emit_call_move_f32_to_acc(&mut self) {
        self.state.emit("    fmv.x.w t0, fa0");
    }

    fn emit_call_move_f64_to_acc(&mut self) {
        self.state.emit("    fmv.x.d t0, fa0");
    }

    fn emit_global_addr(&mut self, dest: &Value, name: &str) {
        if self.state.needs_got(name) {
            // PIC mode, external symbol: use `la` which expands to GOT-indirect
            // auipc + ld with R_RISCV_GOT_HI20 relocation.
            self.state.emit_fmt(format_args!("    la t0, {}", name));
        } else {
            // Non-PIC mode or local symbol: use `lla` (local load address) which
            // expands to PC-relative auipc + addi with R_RISCV_PCREL_HI20.
            // This is critical for kernel PI (position-independent) early boot
            // code compiled with -fpie, where no GOT exists.
            self.state.emit_fmt(format_args!("    lla t0, {}", name));
        }
        self.store_t0_to(dest);
    }

    fn emit_label_addr(&mut self, dest: &Value, label: &str) {
        // Labels are always local symbols -- use lla (local load address) to avoid
        // GOT indirection that `la` may produce in PIC mode.
        self.state.emit_fmt(format_args!("    lla t0, {}", label));
        self.store_t0_to(dest);
    }

    fn emit_tls_global_addr(&mut self, dest: &Value, name: &str) {
        // Local Exec TLS model for RISC-V:
        // lui t0, %tprel_hi(x)
        // add t0, t0, tp, %tprel_add(x)
        // addi t0, t0, %tprel_lo(x)
        self.state.emit_fmt(format_args!("    lui t0, %tprel_hi({})", name));
        self.state.emit_fmt(format_args!("    add t0, t0, tp, %tprel_add({})", name));
        self.state.emit_fmt(format_args!("    addi t0, t0, %tprel_lo({})", name));
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

            CastKind::SignedToFloat { to_f64, from_ty } => {
                // For sub-64-bit signed sources, sign-extend to 64 bits first.
                // The value in t0 may be zero-extended (e.g., from a U32->I32 noop cast).
                match from_ty.size() {
                    1 => {
                        self.state.emit("    slli t0, t0, 56");
                        self.state.emit("    srai t0, t0, 56");
                    }
                    2 => {
                        self.state.emit("    slli t0, t0, 48");
                        self.state.emit("    srai t0, t0, 48");
                    }
                    4 => self.state.emit("    sext.w t0, t0"),
                    _ => {} // 8 bytes: already full width
                }
                if to_f64 {
                    self.state.emit("    fcvt.d.l ft0, t0");
                    self.state.emit("    fmv.x.d t0, ft0");
                } else {
                    self.state.emit("    fcvt.s.l ft0, t0");
                    self.state.emit("    fmv.x.w t0, ft0");
                }
            }

            CastKind::UnsignedToFloat { to_f64, from_ty } => {
                let from_u64 = from_ty == IrType::U64;
                if from_u64 {
                    // U64: use 64-bit unsigned conversion
                    if to_f64 {
                        self.state.emit("    fcvt.d.lu ft0, t0");
                        self.state.emit("    fmv.x.d t0, ft0");
                    } else {
                        self.state.emit("    fcvt.s.lu ft0, t0");
                        self.state.emit("    fmv.x.w t0, ft0");
                    }
                } else {
                    // U8/U16/U32: use 32-bit unsigned conversion.
                    // On RISC-V, W-suffix instructions (sllw, addw, etc.) sign-extend
                    // the 32-bit result to 64 bits, so a U32 value with bit 31 set
                    // would have all upper 32 bits set. fcvt.d.wu correctly reads
                    // only the low 32 bits as unsigned.
                    if to_f64 {
                        self.state.emit("    fcvt.d.wu ft0, t0");
                        self.state.emit("    fmv.x.d t0, ft0");
                    } else {
                        self.state.emit("    fcvt.s.wu ft0, t0");
                        self.state.emit("    fmv.x.w t0, ft0");
                    }
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

            // F128 cast kinds should never be reached here; they are intercepted
            // by emit_cast() before reaching emit_cast_instrs(). classify_cast()
            // (without f128_is_native) reduces F128 to F64, so these arms are
            // unreachable, but we handle them for safety.
            CastKind::SignedToF128 { .. }
            | CastKind::UnsignedToF128 { .. }
            | CastKind::F128ToSigned { .. }
            | CastKind::F128ToUnsigned { .. }
            | CastKind::FloatToF128 { .. }
            | CastKind::F128ToFloat { .. } => {
                unreachable!("F128 casts should be handled by emit_cast override");
            }
        }
    }

    /// Override emit_cast to handle F128 (IEEE binary128) casts with full precision.
    /// On RISC-V, long double is 128-bit IEEE, so int<->F128 casts require softfloat
    /// library calls (__floatditf, __floatunditf, __fixtfdi, __fixunstfdi) to avoid
    /// precision loss from going through f64 intermediate.
    fn emit_cast(&mut self, dest: &Value, src: &Operand, from_ty: IrType, to_ty: IrType) {
        // Intercept casts TO F128 from integer types: produce full-precision f128
        // in the destination slot via softfloat library call.
        if to_ty == IrType::F128 && !from_ty.is_float() && !is_i128_type(from_ty) {
            // Load source integer into t0
            self.operand_to_t0(src);
            // Sign/zero extend sub-64-bit integers
            if from_ty.is_signed() {
                match from_ty.size() {
                    1 => {
                        self.state.emit("    slli t0, t0, 56");
                        self.state.emit("    srai t0, t0, 56");
                    }
                    2 => {
                        self.state.emit("    slli t0, t0, 48");
                        self.state.emit("    srai t0, t0, 48");
                    }
                    4 => self.state.emit("    sext.w t0, t0"),
                    _ => {}
                }
                // Call __floatditf: i64 in a0 -> f128 in a0:a1
                self.state.emit("    mv a0, t0");
                self.state.emit("    call __floatditf");
            } else {
                match from_ty.size() {
                    1 => self.state.emit("    andi t0, t0, 0xff"),
                    2 => {
                        self.state.emit("    slli t0, t0, 48");
                        self.state.emit("    srli t0, t0, 48");
                    }
                    4 => {
                        self.state.emit("    slli t0, t0, 32");
                        self.state.emit("    srli t0, t0, 32");
                    }
                    _ => {}
                }
                // Call __floatunditf: u64 in a0 -> f128 in a0:a1
                self.state.emit("    mv a0, t0");
                self.state.emit("    call __floatunditf");
            }
            // Result f128 is in a0:a1 (lo, hi halves).
            // Store full 16-byte f128 directly to dest slot for full precision.
            if let Some(slot) = self.state.get_slot(dest.0) {
                self.emit_store_to_s0("a0", slot.0, "sd");
                self.emit_store_to_s0("a1", slot.0 + 8, "sd");
            }
            // Produce f64 approximation in t0 for register-based data flow,
            // but do NOT store it back to the slot (that would overwrite the f128).
            self.state.emit("    call __trunctfdf2");
            self.state.emit("    fmv.x.d t0, fa0");
            self.state.reg_cache.invalidate_all();
            // Track that this value has full f128 in its slot (not indirect; data is in slot).
            self.state.track_f128_self(dest.0);
            // Store f64 approx only to register (if register-allocated), not to stack.
            if let Some(&reg) = self.reg_assignments.get(&dest.0) {
                let reg_name = callee_saved_name(reg);
                self.state.emit_fmt(format_args!("    mv {}, t0", reg_name));
            }
            self.state.reg_cache.set_acc(dest.0, false);
            return;
        }

        // Intercept casts FROM F128 to integer types: load full f128 from source
        // slot and convert directly via softfloat, avoiding f64 precision loss.
        if from_ty == IrType::F128 && !to_ty.is_float() && !is_i128_type(to_ty) {
            // Load full f128 from source into a0:a1
            self.emit_f128_operand_to_a0_a1(src);
            // Call appropriate softfloat conversion
            if to_ty.is_unsigned() || to_ty == IrType::Ptr {
                self.state.emit("    call __fixunstfdi");
            } else {
                self.state.emit("    call __fixtfdi");
            }
            self.state.emit("    mv t0, a0");
            self.state.reg_cache.invalidate_all();
            // Narrow if needed
            if to_ty.size() < 8 {
                self.emit_cast_instrs(IrType::I64, to_ty);
            }
            self.store_t0_to(dest);
            return;
        }

        // For all other casts, use the default implementation.
        crate::backend::traits::emit_cast_default(self, dest, src, from_ty, to_ty);
    }

    fn emit_va_arg(&mut self, dest: &Value, va_list_ptr: &Value, result_ty: IrType) {
        // RISC-V LP64D: va_list is just a void* (pointer to the next arg on stack).
        // Load va_list pointer address into t1.
        // Must check register allocation FIRST: store_t0_to skips the stack
        // for register-allocated values, so the slot would be stale.
        if self.state.is_alloca(va_list_ptr.0) {
            if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
                self.emit_addi_s0("t1", slot.0);
            }
        } else if let Some(&reg) = self.reg_assignments.get(&va_list_ptr.0) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mv t1, {}", reg_name));
        } else if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
            self.emit_load_from_s0("t1", slot.0, "ld");
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
        //
        // Must check register allocation FIRST: store_t0_to skips the stack
        // for register-allocated values, so the slot would be stale.
        if self.state.is_alloca(va_list_ptr.0) {
            if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
                self.emit_addi_s0("t0", slot.0);
            }
        } else if let Some(&reg) = self.reg_assignments.get(&va_list_ptr.0) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mv t0, {}", reg_name));
        } else if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
            self.emit_load_from_s0("t0", slot.0, "ld");
        }

        // Variadic args start after all named params in the contiguous
        // register-save-area + caller-stack layout:
        //   s0+0..s0+56  = register save area (a0-a7)
        //   s0+64+       = caller's stack-passed arguments
        //
        // If named params consumed N GP regs (including alignment gaps), the
        // first variadic arg is at s0 + N*8 (if N < 8, it's in the save area).
        // If all 8 GP regs are consumed by named params AND some named params
        // also overflowed to the stack, the variadic args follow the stack-passed
        // named args at s0 + 64 + named_stack_bytes.
        let vararg_offset = if self.va_named_gp_count >= 8 {
            // All GP regs consumed; variadic args are on the stack after named stack args
            64 + self.va_named_stack_bytes as i64
        } else {
            // Some GP regs still available; variadic args start in the register save area
            (self.va_named_gp_count as i64) * 8
        };
        self.emit_addi_s0("t1", vararg_offset);
        self.state.emit("    sd t1, 0(t0)");
    }

    // emit_va_end: uses default no-op implementation

    fn emit_va_copy(&mut self, dest_ptr: &Value, src_ptr: &Value) {
        // Copy va_list (just 8 bytes on RISC-V - a single pointer).
        // Must check register allocation FIRST: store_t0_to skips the stack
        // for register-allocated values, so the slot would be stale.
        if self.state.is_alloca(src_ptr.0) {
            if let Some(src_slot) = self.state.get_slot(src_ptr.0) {
                self.emit_addi_s0("t1", src_slot.0);
            }
        } else if let Some(&reg) = self.reg_assignments.get(&src_ptr.0) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mv t1, {}", reg_name));
        } else if let Some(src_slot) = self.state.get_slot(src_ptr.0) {
            self.emit_load_from_s0("t1", src_slot.0, "ld");
        }
        self.state.emit("    ld t2, 0(t1)");
        if self.state.is_alloca(dest_ptr.0) {
            if let Some(dest_slot) = self.state.get_slot(dest_ptr.0) {
                self.emit_addi_s0("t0", dest_slot.0);
            }
        } else if let Some(&reg) = self.reg_assignments.get(&dest_ptr.0) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mv t0, {}", reg_name));
        } else if let Some(dest_slot) = self.state.get_slot(dest_ptr.0) {
            self.emit_load_from_s0("t0", dest_slot.0, "ld");
        }
        self.state.emit("    sd t2, 0(t0)");
    }

    fn emit_va_arg_struct(&mut self, _dest_ptr: &Value, _va_list_ptr: &Value, _size: usize) {
        // On RISC-V, large structs (>16 bytes) are passed by reference, and the
        // lowering reads the pointer via a regular VaArg instruction. Small
        // structs are decomposed into individual slot reads. VaArgStruct should
        // never be emitted for this target.
        panic!("VaArgStruct should not be emitted for RISC-V target");
    }

    // emit_return: overridden above (near emit_return_f128_to_reg) for F128 full-precision return

    // emit_branch, emit_cond_branch, emit_unreachable, emit_indirect_branch:
    // use default implementations from ArchCodegen trait

    // emit_label_addr: overridden above (near emit_global_addr) for local label addressing

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
                self.state.emit_fmt(format_args!("    li t0, {}", bits));
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
                self.state.emit_fmt(format_args!("    li t0, {}", bits));
                self.state.emit("    fmv.w.x fa1, t0");
            }
            _ => {
                self.operand_to_t0(src);
                self.state.emit("    fmv.w.x fa1, t0");
            }
        }
    }

    fn emit_get_return_f128_second(&mut self, _dest: &Value) {
        // RISC-V: _Complex long double is passed by reference, not via register pair return.
        // This should not be reached. If it is, do nothing.
    }

    fn emit_set_return_f128_second(&mut self, _src: &Operand) {
        // RISC-V: _Complex long double is passed by reference, not via register pair return.
        // This should not be reached. If it is, do nothing.
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
                    self.state.emit_fmt(format_args!("    amoadd.{}{} t0, t2, (t1)", suffix, aq_rl));
                }
                AtomicRmwOp::Sub => {
                    // No amosub; negate and use amoadd
                    self.state.emit("    neg t2, t2");
                    self.state.emit_fmt(format_args!("    amoadd.{}{} t0, t2, (t1)", suffix, aq_rl));
                }
                AtomicRmwOp::And => {
                    self.state.emit_fmt(format_args!("    amoand.{}{} t0, t2, (t1)", suffix, aq_rl));
                }
                AtomicRmwOp::Or => {
                    self.state.emit_fmt(format_args!("    amoor.{}{} t0, t2, (t1)", suffix, aq_rl));
                }
                AtomicRmwOp::Xor => {
                    self.state.emit_fmt(format_args!("    amoxor.{}{} t0, t2, (t1)", suffix, aq_rl));
                }
                AtomicRmwOp::Xchg => {
                    self.state.emit_fmt(format_args!("    amoswap.{}{} t0, t2, (t1)", suffix, aq_rl));
                }
                AtomicRmwOp::Nand => {
                    // No amonand; use lr/sc loop
                    let loop_label = self.state.fresh_label("atomic_nand");
                    self.state.emit_fmt(format_args!("{}:", loop_label));
                    self.state.emit_fmt(format_args!("    lr.{}{} t0, (t1)", suffix, aq_rl));
                    self.state.emit("    and t3, t0, t2");
                    self.state.emit("    not t3, t3");
                    self.state.emit_fmt(format_args!("    sc.{}{} t4, t3, (t1)", suffix, aq_rl));
                    self.state.emit_fmt(format_args!("    bnez t4, {}", loop_label));
                }
                AtomicRmwOp::TestAndSet => {
                    // test_and_set: set byte to 1, return old
                    self.state.emit("    li t2, 1");
                    self.state.emit_fmt(format_args!("    amoswap.{}{} t0, t2, (t1)", suffix, aq_rl));
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

            self.state.emit_fmt(format_args!("{}:", loop_label));
            self.state.emit_fmt(format_args!("    lr.{}{} t0, (t1)", suffix, aq_rl));
            self.state.emit_fmt(format_args!("    bne t0, t2, {}", fail_label));
            self.state.emit_fmt(format_args!("    sc.{}{} t4, t3, (t1)", suffix, aq_rl));
            self.state.emit_fmt(format_args!("    bnez t4, {}", loop_label));
            if returns_bool {
                self.state.emit("    li t0, 1");
            }
            self.state.emit_fmt(format_args!("    j {}", done_label));
            self.state.emit_fmt(format_args!("{}:", fail_label));
            if returns_bool {
                self.state.emit("    li t0, 0");
            }
            // t0 has old value if !returns_bool
            self.state.emit_fmt(format_args!("{}:", done_label));
        }
        self.store_t0_to(dest);
    }

    fn emit_atomic_load(&mut self, dest: &Value, ptr: &Operand, ty: IrType, ordering: AtomicOrdering) {
        self.operand_to_t0(ptr);
        if Self::is_subword_type(ty) {
            // For sub-word atomic loads, use regular load + fences appropriate to ordering.
            // On RISC-V, aligned byte/halfword loads are naturally atomic for
            // single-copy atomicity. Use fences for ordering guarantees.
            if matches!(ordering, AtomicOrdering::SeqCst) {
                self.state.emit("    fence rw, rw");
            }
            match ty {
                IrType::I8 => self.state.emit("    lb t0, 0(t0)"),
                IrType::U8 => self.state.emit("    lbu t0, 0(t0)"),
                IrType::I16 => self.state.emit("    lh t0, 0(t0)"),
                IrType::U16 => self.state.emit("    lhu t0, 0(t0)"),
                _ => unreachable!("non-subword type in subword atomic load: {:?}", ty),
            }
            if matches!(ordering, AtomicOrdering::Acquire | AtomicOrdering::AcqRel | AtomicOrdering::SeqCst) {
                self.state.emit("    fence r, rw");
            }
        } else {
            // Use lr for word/doubleword atomic load with appropriate ordering.
            // lr supports .aq and .aqrl suffixes (not .rl alone).
            let suffix = Self::amo_width_suffix(ty);
            let lr_suffix = match ordering {
                AtomicOrdering::Relaxed | AtomicOrdering::Release => "",
                AtomicOrdering::Acquire => ".aq",
                AtomicOrdering::AcqRel | AtomicOrdering::SeqCst => ".aqrl",
            };
            self.state.emit_fmt(format_args!("    lr.{}{} t0, (t0)", suffix, lr_suffix));
            Self::sign_extend_riscv(&mut self.state, ty);
        }
        self.store_t0_to(dest);
    }

    fn emit_atomic_store(&mut self, ptr: &Operand, val: &Operand, ty: IrType, ordering: AtomicOrdering) {
        self.operand_to_t0(val);
        self.state.emit("    mv t1, t0"); // t1 = val
        self.operand_to_t0(ptr);
        if Self::is_subword_type(ty) {
            // For sub-word atomic stores, use fences appropriate to ordering + regular store.
            // Aligned byte/halfword stores are naturally atomic on RISC-V.
            if matches!(ordering, AtomicOrdering::Release | AtomicOrdering::AcqRel | AtomicOrdering::SeqCst) {
                self.state.emit("    fence rw, w");
            }
            match ty {
                IrType::I8 | IrType::U8 => self.state.emit("    sb t1, 0(t0)"),
                IrType::I16 | IrType::U16 => self.state.emit("    sh t1, 0(t0)"),
                _ => unreachable!("non-subword type in subword atomic store: {:?}", ty),
            }
            if matches!(ordering, AtomicOrdering::SeqCst) {
                self.state.emit("    fence rw, rw");
            }
        } else {
            // Use amoswap with zero dest for atomic store
            let aq_rl = Self::amo_ordering(ordering);
            let suffix = Self::amo_width_suffix(ty);
            self.state.emit_fmt(format_args!("    amoswap.{}{} zero, t1, (t0)", suffix, aq_rl));
        }
    }

    fn emit_fence(&mut self, ordering: AtomicOrdering) {
        // RISC-V fence instructions with appropriate ordering:
        // - Relaxed: no fence needed
        // - Acquire: fence r, rw (reads before fence complete before reads/writes after)
        // - Release: fence rw, w (reads/writes before fence complete before writes after)
        // - AcqRel/SeqCst: fence rw, rw (full barrier)
        match ordering {
            AtomicOrdering::Relaxed => {} // no-op
            AtomicOrdering::Acquire => self.state.emit("    fence r, rw"),
            AtomicOrdering::Release => self.state.emit("    fence rw, w"),
            AtomicOrdering::AcqRel | AtomicOrdering::SeqCst => self.state.emit("    fence rw, rw"),
        }
    }

    fn emit_inline_asm(&mut self, template: &str, outputs: &[(String, Value, Option<String>)], inputs: &[(String, Operand, Option<String>)], clobbers: &[String], operand_types: &[IrType], goto_labels: &[(String, BlockId)], input_symbols: &[Option<String>]) {
        emit_inline_asm_common(self, template, outputs, inputs, clobbers, operand_types, goto_labels, input_symbols);
    }

    fn emit_copy_i128(&mut self, dest: &Value, src: &Operand) {
        self.operand_to_t0_t1(src);
        self.store_t0_t1_to(dest);
    }

    fn emit_intrinsic(&mut self, dest: &Option<Value>, op: &IntrinsicOp, dest_ptr: &Option<Value>, args: &[Operand]) {
        self.emit_intrinsic_rv(dest, op, dest_ptr, args);
    }

    // ---- Float binop primitives ----

    /// Override emit_float_binop for F128 to use full-precision operands.
    ///
    /// The default implementation loads operands via emit_load_operand (operand_to_t0)
    /// which gets the f64 approximation in t0/t1. For F128, the slot holds the full
    /// 16-byte f128, not an f64 approximation, so loading 8 bytes from the slot would
    /// yield the f128 low half -- not an f64 value. This override loads operands
    /// using emit_f128_operand_to_a0_a1 which correctly accesses the full f128 data.
    fn emit_float_binop(&mut self, dest: &Value, op: FloatOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if ty == IrType::F128 {
            let libcall = match op {
                FloatOp::Add => "__addtf3",
                FloatOp::Sub => "__subtf3",
                FloatOp::Mul => "__multf3",
                FloatOp::Div => "__divtf3",
            };

            // Allocate stack space for saving the LHS f128 (16 bytes)
            self.emit_addi_sp(-16);

            // Load LHS as full f128 into a0:a1
            self.emit_f128_operand_to_a0_a1(lhs);
            // Save LHS f128 to temp stack space
            self.state.emit("    sd a0, 0(sp)");
            self.state.emit("    sd a1, 8(sp)");

            // Load RHS as full f128 into a0:a1, then move to a2:a3
            self.emit_f128_operand_to_a0_a1(rhs);
            self.state.emit("    mv a2, a0");
            self.state.emit("    mv a3, a1");

            // Reload LHS f128 into a0:a1
            self.state.emit("    ld a0, 0(sp)");
            self.state.emit("    ld a1, 8(sp)");

            // Call the arithmetic libcall: result f128 in a0:a1
            self.state.emit_fmt(format_args!("    call {}", libcall));

            // Free temp stack space
            self.emit_addi_sp(16);

            // Store full f128 result to dest slot
            if let Some(slot) = self.state.get_slot(dest.0) {
                self.emit_store_to_s0("a0", slot.0, "sd");
                self.emit_store_to_s0("a1", slot.0 + 8, "sd");
            }

            // Convert result to f64 approximation for register-based data flow
            self.state.emit("    call __trunctfdf2");
            self.state.emit("    fmv.x.d t0, fa0");
            self.state.reg_cache.invalidate_all();

            // Track that dest has full f128 in its slot
            self.state.track_f128_self(dest.0);

            // Store f64 approx only to register (if register-allocated), not to slot
            if let Some(&reg) = self.reg_assignments.get(&dest.0) {
                let reg_name = callee_saved_name(reg);
                self.state.emit_fmt(format_args!("    mv {}, t0", reg_name));
            }
            self.state.reg_cache.set_acc(dest.0, false);
            return;
        }

        // Non-F128: use the default implementation (loads f64/f32 via operand_to_t0)
        let mnemonic = self.emit_float_binop_mnemonic(op);
        self.emit_load_operand(lhs);
        self.emit_acc_to_secondary();
        self.emit_load_operand(rhs);
        self.emit_float_binop_impl(mnemonic, ty);
        self.emit_store_result(dest);
    }

    fn emit_float_binop_impl(&mut self, mnemonic: &str, ty: IrType) {
        // On RISC-V: emit_acc_to_secondary does "mv t1, t0", acc (rhs) stays in t0
        // So after shared default: t1 = lhs, t0 = rhs
        if ty == IrType::F128 {
            // F128: use soft-float library calls for full quad-precision arithmetic.
            self.emit_f128_binop_softfloat(mnemonic);
            return;
        }
        self.state.emit("    mv t2, t0"); // t2 = rhs
        if ty == IrType::F64 {
            self.state.emit("    fmv.d.x ft0, t1");
            self.state.emit("    fmv.d.x ft1, t2");
            self.state.emit_fmt(format_args!("    {}.d ft0, ft0, ft1", mnemonic));
            self.state.emit("    fmv.x.d t0, ft0");
        } else {
            self.state.emit("    fmv.w.x ft0, t1");
            self.state.emit("    fmv.w.x ft1, t2");
            self.state.emit_fmt(format_args!("    {}.s ft0, ft0, ft1", mnemonic));
            self.state.emit("    fmv.x.w t0, ft0");
        }
    }

    // ---- i128 binop primitives ----

    fn emit_i128_prep_binop(&mut self, lhs: &Operand, rhs: &Operand) {
        self.prep_i128_binop(lhs, rhs);
    }

    fn emit_i128_add(&mut self) {
        self.state.emit("    add t0, t3, t5");        // t0 = a_lo + b_lo
        self.state.emit("    sltu t2, t0, t3");       // t2 = carry
        self.state.emit("    add t1, t4, t6");        // t1 = a_hi + b_hi
        self.state.emit("    add t1, t1, t2");        // t1 += carry
    }

    fn emit_i128_sub(&mut self) {
        self.state.emit("    sltu t2, t3, t5");       // t2 = borrow (a_lo < b_lo)
        self.state.emit("    sub t0, t3, t5");        // t0 = a_lo - b_lo
        self.state.emit("    sub t1, t4, t6");        // t1 = a_hi - b_hi
        self.state.emit("    sub t1, t1, t2");        // t1 -= borrow
    }

    fn emit_i128_mul(&mut self) {
        // t3:t4 = lhs, t5:t6 = rhs
        self.state.emit("    mul t0, t3, t5");        // t0 = lo(a_lo * b_lo)
        self.state.emit("    mulhu t1, t3, t5");      // t1 = hi(a_lo * b_lo)
        self.state.emit("    mul t2, t4, t5");        // t2 = a_hi * b_lo (low 64)
        self.state.emit("    add t1, t1, t2");        // t1 += a_hi * b_lo
        self.state.emit("    mul t2, t3, t6");        // t2 = a_lo * b_hi (low 64)
        self.state.emit("    add t1, t1, t2");        // t1 += a_lo * b_hi
    }

    fn emit_i128_and(&mut self) {
        self.state.emit("    and t0, t3, t5");
        self.state.emit("    and t1, t4, t6");
    }

    fn emit_i128_or(&mut self) {
        self.state.emit("    or t0, t3, t5");
        self.state.emit("    or t1, t4, t6");
    }

    fn emit_i128_xor(&mut self) {
        self.state.emit("    xor t0, t3, t5");
        self.state.emit("    xor t1, t4, t6");
    }

    fn emit_i128_shl(&mut self) {
        let lbl = self.state.fresh_label("shl128");
        let done = self.state.fresh_label("shl128_done");
        let noop = self.state.fresh_label("shl128_noop");
        self.state.emit("    andi t5, t5, 127");
        self.state.emit_fmt(format_args!("    beqz t5, {}", noop));
        self.state.emit("    li t2, 64");
        self.state.emit_fmt(format_args!("    bge t5, t2, {}", lbl));
        self.state.emit("    sll t1, t4, t5");
        self.state.emit("    sub t2, t2, t5");
        self.state.emit("    srl t6, t3, t2");
        self.state.emit("    or t1, t1, t6");
        self.state.emit("    sll t0, t3, t5");
        self.state.emit_fmt(format_args!("    j {}", done));
        self.state.emit_fmt(format_args!("{}:", lbl));
        self.state.emit("    li t2, 64");
        self.state.emit("    sub t5, t5, t2");
        self.state.emit("    sll t1, t3, t5");
        self.state.emit("    li t0, 0");
        self.state.emit_fmt(format_args!("    j {}", done));
        self.state.emit_fmt(format_args!("{}:", noop));
        self.state.emit("    mv t0, t3");
        self.state.emit("    mv t1, t4");
        self.state.emit_fmt(format_args!("{}:", done));
    }

    fn emit_i128_lshr(&mut self) {
        let lbl = self.state.fresh_label("lshr128");
        let done = self.state.fresh_label("lshr128_done");
        let noop = self.state.fresh_label("lshr128_noop");
        self.state.emit("    andi t5, t5, 127");
        self.state.emit_fmt(format_args!("    beqz t5, {}", noop));
        self.state.emit("    li t2, 64");
        self.state.emit_fmt(format_args!("    bge t5, t2, {}", lbl));
        self.state.emit("    srl t0, t3, t5");
        self.state.emit("    sub t2, t2, t5");
        self.state.emit("    sll t6, t4, t2");
        self.state.emit("    or t0, t0, t6");
        self.state.emit("    srl t1, t4, t5");
        self.state.emit_fmt(format_args!("    j {}", done));
        self.state.emit_fmt(format_args!("{}:", lbl));
        self.state.emit("    li t2, 64");
        self.state.emit("    sub t5, t5, t2");
        self.state.emit("    srl t0, t4, t5");
        self.state.emit("    li t1, 0");
        self.state.emit_fmt(format_args!("    j {}", done));
        self.state.emit_fmt(format_args!("{}:", noop));
        self.state.emit("    mv t0, t3");
        self.state.emit("    mv t1, t4");
        self.state.emit_fmt(format_args!("{}:", done));
    }

    fn emit_i128_ashr(&mut self) {
        let lbl = self.state.fresh_label("ashr128");
        let done = self.state.fresh_label("ashr128_done");
        let noop = self.state.fresh_label("ashr128_noop");
        self.state.emit("    andi t5, t5, 127");
        self.state.emit_fmt(format_args!("    beqz t5, {}", noop));
        self.state.emit("    li t2, 64");
        self.state.emit_fmt(format_args!("    bge t5, t2, {}", lbl));
        self.state.emit("    srl t0, t3, t5");
        self.state.emit("    sub t2, t2, t5");
        self.state.emit("    sll t6, t4, t2");
        self.state.emit("    or t0, t0, t6");
        self.state.emit("    sra t1, t4, t5");
        self.state.emit_fmt(format_args!("    j {}", done));
        self.state.emit_fmt(format_args!("{}:", lbl));
        self.state.emit("    li t2, 64");
        self.state.emit("    sub t5, t5, t2");
        self.state.emit("    sra t0, t4, t5");
        self.state.emit("    srai t1, t4, 63");
        self.state.emit_fmt(format_args!("    j {}", done));
        self.state.emit_fmt(format_args!("{}:", noop));
        self.state.emit("    mv t0, t3");
        self.state.emit("    mv t1, t4");
        self.state.emit_fmt(format_args!("{}:", done));
    }

    fn emit_i128_prep_shift_lhs(&mut self, lhs: &Operand) {
        // Load LHS into t3:t4 for constant-amount shifts
        self.operand_to_t0_t1(lhs);
        self.state.emit("    mv t3, t0");
        self.state.emit("    mv t4, t1");
    }

    fn emit_i128_shl_const(&mut self, amount: u32) {
        // Input: t3 (low), t4 (high). Output: t0 (low), t1 (high).
        let amount = amount & 127;
        if amount == 0 {
            self.state.emit("    mv t0, t3");
            self.state.emit("    mv t1, t4");
        } else if amount == 64 {
            self.state.emit("    mv t1, t3");
            self.state.emit("    li t0, 0");
        } else if amount > 64 {
            self.state.emit_fmt(format_args!("    slli t1, t3, {}", amount - 64));
            self.state.emit("    li t0, 0");
        } else {
            self.state.emit_fmt(format_args!("    slli t1, t4, {}", amount));
            self.state.emit_fmt(format_args!("    srli t2, t3, {}", 64 - amount));
            self.state.emit("    or t1, t1, t2");
            self.state.emit_fmt(format_args!("    slli t0, t3, {}", amount));
        }
    }

    fn emit_i128_lshr_const(&mut self, amount: u32) {
        let amount = amount & 127;
        if amount == 0 {
            self.state.emit("    mv t0, t3");
            self.state.emit("    mv t1, t4");
        } else if amount == 64 {
            self.state.emit("    mv t0, t4");
            self.state.emit("    li t1, 0");
        } else if amount > 64 {
            self.state.emit_fmt(format_args!("    srli t0, t4, {}", amount - 64));
            self.state.emit("    li t1, 0");
        } else {
            self.state.emit_fmt(format_args!("    srli t0, t3, {}", amount));
            self.state.emit_fmt(format_args!("    slli t2, t4, {}", 64 - amount));
            self.state.emit("    or t0, t0, t2");
            self.state.emit_fmt(format_args!("    srli t1, t4, {}", amount));
        }
    }

    fn emit_i128_ashr_const(&mut self, amount: u32) {
        let amount = amount & 127;
        if amount == 0 {
            self.state.emit("    mv t0, t3");
            self.state.emit("    mv t1, t4");
        } else if amount == 64 {
            self.state.emit("    mv t0, t4");
            self.state.emit("    srai t1, t4, 63");
        } else if amount > 64 {
            self.state.emit_fmt(format_args!("    srai t0, t4, {}", amount - 64));
            self.state.emit("    srai t1, t4, 63");
        } else {
            self.state.emit_fmt(format_args!("    srli t0, t3, {}", amount));
            self.state.emit_fmt(format_args!("    slli t2, t4, {}", 64 - amount));
            self.state.emit("    or t0, t0, t2");
            self.state.emit_fmt(format_args!("    srai t1, t4, {}", amount));
        }
    }

    fn emit_i128_divrem_call(&mut self, func_name: &str, lhs: &Operand, rhs: &Operand) {
        // RISC-V LP64D: first 128-bit arg in a0:a1, second in a2:a3
        self.operand_to_t0_t1(lhs);
        self.state.emit("    mv a0, t0");
        self.state.emit("    mv a1, t1");
        self.operand_to_t0_t1(rhs);
        self.state.emit("    mv a2, t0");
        self.state.emit("    mv a3, t1");
        self.state.emit_fmt(format_args!("    call {}", func_name));
        // Result in a0:a1
        self.state.emit("    mv t0, a0");
        self.state.emit("    mv t1, a1");
    }

    fn emit_i128_store_result(&mut self, dest: &Value) {
        self.store_t0_t1_to(dest);
    }

    fn emit_i128_to_float_call(&mut self, src: &Operand, from_signed: bool, to_ty: IrType) {
        // Load i128 src into t0:t1 (acc pair), then move to a0:a1 for LP64D ABI
        self.operand_to_t0_t1(src);
        self.state.emit("    mv a0, t0");
        self.state.emit("    mv a1, t1");
        let func_name = match (from_signed, to_ty) {
            (true, IrType::F64)  => "__floattidf",
            (true, IrType::F32)  => "__floattisf",
            (false, IrType::F64) => "__floatuntidf",
            (false, IrType::F32) => "__floatuntisf",
            _ => panic!("unsupported i128-to-float conversion: {:?}", to_ty),
        };
        self.state.emit_fmt(format_args!("    call {}", func_name));
        self.state.reg_cache.invalidate_all();
        // Result in fa0; move to t0 as bit pattern
        if to_ty == IrType::F32 {
            self.state.emit("    fmv.x.w t0, fa0");
        } else {
            self.state.emit("    fmv.x.d t0, fa0");
        }
    }

    fn emit_float_to_i128_call(&mut self, src: &Operand, to_signed: bool, from_ty: IrType) {
        // Load float operand into t0 (as bit pattern), then move to fa0
        self.operand_to_t0(src);
        if from_ty == IrType::F32 {
            self.state.emit("    fmv.w.x fa0, t0");
        } else {
            self.state.emit("    fmv.d.x fa0, t0");
        }
        let func_name = match (to_signed, from_ty) {
            (true, IrType::F64)  => "__fixdfti",
            (true, IrType::F32)  => "__fixsfti",
            (false, IrType::F64) => "__fixunsdfti",
            (false, IrType::F32) => "__fixunssfti",
            _ => panic!("unsupported float-to-i128 conversion: {:?}", from_ty),
        };
        self.state.emit_fmt(format_args!("    call {}", func_name));
        self.state.reg_cache.invalidate_all();
        // Result i128 in a0:a1; move to t0:t1 (acc pair)
        self.state.emit("    mv t0, a0");
        self.state.emit("    mv t1, a1");
    }

    // ---- i128 cmp primitives ----

    fn emit_i128_cmp_eq(&mut self, is_ne: bool) {
        // t3:t4 = lhs, t5:t6 = rhs (after prep)
        self.state.emit("    xor t0, t3, t5");
        self.state.emit("    xor t1, t4, t6");
        self.state.emit("    or t0, t0, t1");
        if is_ne {
            self.state.emit("    snez t0, t0");
        } else {
            self.state.emit("    seqz t0, t0");
        }
    }

    fn emit_i128_cmp_ordered(&mut self, op: IrCmpOp) {
        let hi_differ = self.state.fresh_label("cmp128_hi_diff");
        let hi_equal = self.state.fresh_label("cmp128_hi_eq");
        let done = self.state.fresh_label("cmp128_done");
        self.state.emit_fmt(format_args!("    bne t4, t6, {}", hi_differ));
        self.state.emit_fmt(format_args!("    j {}", hi_equal));
        self.state.emit_fmt(format_args!("{}:", hi_differ));
        match op {
            IrCmpOp::Slt | IrCmpOp::Sle => self.state.emit("    slt t0, t4, t6"),
            IrCmpOp::Sgt | IrCmpOp::Sge => self.state.emit("    slt t0, t6, t4"),
            IrCmpOp::Ult | IrCmpOp::Ule => self.state.emit("    sltu t0, t4, t6"),
            IrCmpOp::Ugt | IrCmpOp::Uge => self.state.emit("    sltu t0, t6, t4"),
            _ => unreachable!("i128 ordered cmp (high word) got equality op: {:?}", op),
        }
        self.state.emit_fmt(format_args!("    j {}", done));
        self.state.emit_fmt(format_args!("{}:", hi_equal));
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
            _ => unreachable!("i128 ordered cmp (low word) got equality op: {:?}", op),
        }
        self.state.emit_fmt(format_args!("{}:", done));
    }

    fn emit_i128_cmp_store_result(&mut self, dest: &Value) {
        self.store_t0_to(dest);
    }
}

impl Default for RiscvCodegen {
    fn default() -> Self {
        Self::new()
    }
}

