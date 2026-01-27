use crate::ir::ir::*;
use crate::common::types::IrType;
use crate::common::fx_hash::{FxHashMap, FxHashSet};
use crate::backend::common::PtrDirective;
use crate::backend::state::{CodegenState, StackSlot};
use crate::backend::traits::ArchCodegen;
use crate::backend::generation::{generate_module, is_i128_type, calculate_stack_space_common, find_param_alloca};
use crate::backend::call_abi::{CallAbiConfig, CallArgClass, compute_stack_arg_space};
use crate::backend::call_emit::{ParamClass, classify_params};
use crate::backend::cast::{CastKind, classify_cast, FloatOp};
use crate::backend::inline_asm::{InlineAsmEmitter, AsmOperandKind, AsmOperand, emit_inline_asm_common};
use crate::backend::regalloc::{self, PhysReg, RegAllocConfig};

/// Callee-saved registers available for register allocation: x20-x28.
/// x19 is reserved (some ABIs use it), x29=fp, x30=lr.
const ARM_CALLEE_SAVED: [PhysReg; 9] = [
    PhysReg(20), PhysReg(21), PhysReg(22), PhysReg(23), PhysReg(24),
    PhysReg(25), PhysReg(26), PhysReg(27), PhysReg(28),
];

fn callee_saved_name(reg: PhysReg) -> &'static str {
    match reg.0 {
        19 => "x19", 20 => "x20", 21 => "x21", 22 => "x22", 23 => "x23", 24 => "x24",
        25 => "x25", 26 => "x26", 27 => "x27", 28 => "x28",
        _ => unreachable!("invalid ARM callee-saved register index"),
    }
}

fn callee_saved_name_32(reg: PhysReg) -> &'static str {
    match reg.0 {
        19 => "w19", 20 => "w20", 21 => "w21", 22 => "w22", 23 => "w23", 24 => "w24",
        25 => "w25", 26 => "w26", 27 => "w27", 28 => "w28",
        _ => unreachable!("invalid ARM callee-saved register index"),
    }
}

/// Map IrBinOp to AArch64 mnemonic for simple ALU ops.
fn arm_alu_mnemonic(op: IrBinOp) -> &'static str {
    match op {
        IrBinOp::Add => "add",
        IrBinOp::Sub => "sub",
        IrBinOp::And => "and",
        IrBinOp::Or => "orr",
        IrBinOp::Xor => "eor",
        IrBinOp::Mul => "mul",
        _ => unreachable!(),
    }
}

/// Map an IrCmpOp to its AArch64 integer condition code suffix.
fn arm_int_cond_code(op: IrCmpOp) -> &'static str {
    match op {
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
    }
}

/// Return the inverted AArch64 condition code suffix.
fn arm_invert_cond_code(cc: &str) -> &'static str {
    match cc {
        "eq" => "ne",
        "ne" => "eq",
        "lt" => "ge",
        "ge" => "lt",
        "gt" => "le",
        "le" => "gt",
        "lo" => "hs",
        "hs" => "lo",
        "hi" => "ls",
        "ls" => "hi",
        "mi" => "pl",
        "pl" => "mi",
        "vs" => "vc",
        "vc" => "vs",
        _ => unreachable!("unknown ARM condition code: {}", cc),
    }
}

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
    /// Total bytes of named (non-variadic) params passed on the stack.
    /// This includes all stack-passed scalars, F128, I128, and structs with alignment.
    va_named_stack_bytes: usize,
    /// Scratch register index for inline asm GP register allocation
    asm_scratch_idx: usize,
    /// Scratch register index for inline asm FP register allocation
    asm_fp_scratch_idx: usize,
    /// Register allocator: value ID -> physical callee-saved register.
    reg_assignments: FxHashMap<u32, PhysReg>,
    /// Which callee-saved registers are actually used (for save/restore).
    used_callee_saved: Vec<PhysReg>,
    /// SP offset where callee-saved registers are stored.
    callee_save_offset: i64,
    /// For large stack frames: reserved for future x19 frame base optimization.
    /// Currently always None (optimization disabled due to correctness issue).
    frame_base_offset: Option<i64>,
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
            va_named_stack_bytes: 0,
            asm_scratch_idx: 0,
            asm_fp_scratch_idx: 0,
            reg_assignments: FxHashMap::default(),
            used_callee_saved: Vec::new(),
            callee_save_offset: 0,
            frame_base_offset: None,
        }
    }

    /// Disable jump table emission (-fno-jump-tables).
    pub fn set_no_jump_tables(&mut self, enabled: bool) {
        self.state.no_jump_tables = enabled;
    }

    pub fn generate(mut self, module: &IrModule) -> String {
        generate_module(&mut self, module)
    }

    /// Get the physical register assigned to an operand (if it's a Value with a register).
    fn operand_reg(&self, op: &Operand) -> Option<PhysReg> {
        match op {
            Operand::Value(v) => self.reg_assignments.get(&v.0).copied(),
            _ => None,
        }
    }

    /// Get the physical register assigned to a destination value.
    fn dest_reg(&self, dest: &Value) -> Option<PhysReg> {
        self.reg_assignments.get(&dest.0).copied()
    }

    /// Load an operand into a specific callee-saved register.
    fn operand_to_callee_reg(&mut self, op: &Operand, reg: PhysReg) {
        let reg_name = callee_saved_name(reg);
        match op {
            Operand::Const(_) => {
                self.operand_to_x0(op);
                self.state.emit_fmt(format_args!("    mov {}, x0", reg_name));
            }
            Operand::Value(v) => {
                if let Some(&src_reg) = self.reg_assignments.get(&v.0) {
                    if src_reg.0 != reg.0 {
                        let src_name = callee_saved_name(src_reg);
                        self.state.emit_fmt(format_args!("    mov {}, {}", reg_name, src_name));
                    }
                } else {
                    self.operand_to_x0(op);
                    self.state.emit_fmt(format_args!("    mov {}, x0", reg_name));
                }
            }
        }
    }

    /// Try to extract an immediate value suitable for ARM imm12 encoding.
    fn const_as_imm12(op: &Operand) -> Option<i64> {
        match op {
            Operand::Const(c) => {
                let val = match c {
                    IrConst::I8(v) => *v as i64,
                    IrConst::I16(v) => *v as i64,
                    IrConst::I32(v) => *v as i64,
                    IrConst::I64(v) => *v,
                    IrConst::Zero => 0,
                    _ => return None,
                };
                // ARM add/sub imm12: 0..4095
                if val >= 0 && val <= 4095 {
                    Some(val)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// If `op` is a constant that is a power of two, return its log2 (shift amount).
    fn const_as_power_of_2(op: &Operand) -> Option<u32> {
        match op {
            Operand::Const(c) => {
                let val: u64 = match c {
                    IrConst::I8(v) => *v as u8 as u64,
                    IrConst::I16(v) => *v as u16 as u64,
                    IrConst::I32(v) => *v as u32 as u64,
                    IrConst::I64(v) => *v as u64,
                    IrConst::Zero => return None,
                    _ => return None,
                };
                if val > 0 && val.is_power_of_two() {
                    Some(val.trailing_zeros())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Pre-scan all inline asm instructions in a function to predict which
    /// callee-saved registers will be needed as scratch registers.
    ///
    /// The inline asm scratch allocator (`assign_scratch_reg`) walks through
    /// `ARM_GP_SCRATCH` = [x9..x15, x19, x20, x21], skipping registers that
    /// appear in the clobber/excluded list. When enough caller-saved scratch regs
    /// (x9-x15) are clobbered, the allocator falls through to callee-saved
    /// registers (x19, x20, x21). These must be saved/restored in the prologue,
    /// but the prologue is emitted before inline asm codegen runs. This function
    /// simulates the allocation to discover the callee-saved registers early.
    fn prescan_inline_asm_callee_saved(func: &IrFunction, used_callee_saved: &mut Vec<PhysReg>) {
        for block in &func.blocks {
            for instr in &block.instructions {
                if let Instruction::InlineAsm {
                    outputs, inputs, clobbers, ..
                } = instr {
                    // Build excluded set: clobber registers + specific constraint regs
                    let mut excluded: Vec<String> = Vec::new();
                    for clobber in clobbers {
                        if clobber == "cc" || clobber == "memory" {
                            continue;
                        }
                        excluded.push(clobber.clone());
                        // Also exclude the alternate width alias (wN <-> xN)
                        if let Some(suffix) = clobber.strip_prefix('w') {
                            if suffix.chars().all(|c| c.is_ascii_digit()) {
                                excluded.push(format!("x{}", suffix));
                            }
                        } else if let Some(suffix) = clobber.strip_prefix('x') {
                            if suffix.chars().all(|c| c.is_ascii_digit()) {
                                excluded.push(format!("w{}", suffix));
                            }
                        }
                    }

                    // Count GP scratch registers needed:
                    // 1. GpReg operands (outputs + inputs that are "r" type, not tied, not specific)
                    // 2. Memory operands that need indirection (non-alloca pointers get a scratch reg)
                    let mut gp_scratch_needed = 0usize;

                    for (constraint, _, _) in outputs {
                        let c = constraint.trim_start_matches(|ch: char| ch == '=' || ch == '+' || ch == '&');
                        if c.starts_with('{') && c.ends_with('}') {
                            let reg_name = &c[1..c.len()-1];
                            excluded.push(reg_name.to_string());
                        } else if c == "m" || c == "Q" || c.contains('Q') || c.contains('m') {
                            // Memory operands may need a scratch reg for indirection.
                            // Conservatively count each one.
                            gp_scratch_needed += 1;
                        } else if c == "w" {
                            // FP register, doesn't consume GP scratch
                        } else if !c.is_empty() && c.chars().all(|ch| ch.is_ascii_digit()) {
                            // Tied operand, doesn't need its own scratch
                        } else {
                            // GpReg
                            gp_scratch_needed += 1;
                        }
                    }

                    // Count "+" read-write outputs that generate synthetic inputs.
                    // Synthetic inputs from "+r" have constraint "r" and consume a
                    // GP scratch slot in phase 1 (even though the register is later
                    // overwritten by copy_metadata_from). We must count these too.
                    let num_plus = outputs.iter().filter(|(c,_,_)| c.contains('+')).count();
                    {
                        let mut plus_idx = 0;
                        for (constraint, _, _) in outputs.iter() {
                            if constraint.contains('+') {
                                let c = constraint.trim_start_matches(|ch: char| ch == '=' || ch == '+' || ch == '&');
                                // Synthetic input inherits constraint with '+' stripped
                                // "+r" → "r" (GpReg, consumes scratch), "+m" → "m" (Memory, skip)
                                if c != "m" && c != "Q" && !c.contains('Q') && !c.contains('m') && c != "w"
                                    && !(c.starts_with('{') && c.ends_with('}'))
                                    && !(c.chars().all(|ch| ch.is_ascii_digit()) && !c.is_empty())
                                {
                                    gp_scratch_needed += 1;
                                }
                                plus_idx += 1;
                            }
                        }
                        let _ = plus_idx;
                    }

                    for (i, (constraint, val, _)) in inputs.iter().enumerate() {
                        // Skip synthetic inputs (they're already counted above)
                        if i < num_plus {
                            continue;
                        }
                        let c = constraint.trim_start_matches(|ch: char| ch == '=' || ch == '+' || ch == '&');
                        if c.starts_with('{') && c.ends_with('}') {
                            let reg_name = &c[1..c.len()-1];
                            excluded.push(reg_name.to_string());
                        } else if c == "m" || c == "Q" || c.contains('Q') || c.contains('m') {
                            gp_scratch_needed += 1;
                        } else if c == "w" {
                            // FP register
                        } else if !c.is_empty() && c.chars().all(|ch| ch.is_ascii_digit()) {
                            // Tied operand
                        } else {
                            // Check if constant input with immediate-capable constraint
                            // would be promoted to Immediate (no scratch needed)
                            let is_const = matches!(val, Operand::Const(_));
                            let has_imm_alt = c.contains('i') || c.contains('I') || c.contains('n');
                            if is_const && has_imm_alt {
                                // Would be promoted to Immediate, no GP scratch needed
                            } else {
                                gp_scratch_needed += 1;
                            }
                        }
                    }

                    // Simulate walking through ARM_GP_SCRATCH, skipping excluded regs
                    let mut scratch_idx = 0;
                    let mut assigned = 0;
                    while assigned < gp_scratch_needed && scratch_idx < ARM_GP_SCRATCH.len() {
                        let reg = ARM_GP_SCRATCH[scratch_idx];
                        scratch_idx += 1;
                        if excluded.iter().any(|e| e == reg) {
                            continue;
                        }
                        assigned += 1;
                        // Check if this is a callee-saved register
                        if let Some(num_str) = reg.strip_prefix('x') {
                            if let Ok(n) = num_str.parse::<u8>() {
                                if (19..=28).contains(&n) {
                                    let phys = PhysReg(n);
                                    if !used_callee_saved.contains(&phys) {
                                        used_callee_saved.push(phys);
                                    }
                                }
                            }
                        }
                    }

                    // Also handle overflow beyond ARM_GP_SCRATCH (format!("x{}", 9 + idx))
                    while assigned < gp_scratch_needed {
                        let idx = scratch_idx;
                        scratch_idx += 1;
                        let reg_num = 9 + idx;
                        let reg_name = format!("x{}", reg_num);
                        if excluded.iter().any(|e| e == &reg_name) {
                            continue;
                        }
                        assigned += 1;
                        if (19..=28).contains(&reg_num) {
                            let phys = PhysReg(reg_num as u8);
                            if !used_callee_saved.contains(&phys) {
                                used_callee_saved.push(phys);
                            }
                        }
                    }
                }
            }
        }
        // Sort for deterministic prologue/epilogue emission
        used_callee_saved.sort_by_key(|r| r.0);
    }

    /// Restore callee-saved registers before epilogue.
    fn emit_restore_callee_saved(&mut self) {
        let used_regs = self.used_callee_saved.clone();
        let base = self.callee_save_offset;
        let n = used_regs.len();
        let mut i = 0;
        while i + 1 < n {
            let r1 = callee_saved_name(used_regs[i]);
            let r2 = callee_saved_name(used_regs[i + 1]);
            let offset = base + (i as i64) * 8;
            self.emit_ldp_from_sp(r1, r2, offset);
            i += 2;
        }
        if i < n {
            let r = callee_saved_name(used_regs[i]);
            let offset = base + (i as i64) * 8;
            self.emit_load_from_sp(r, offset, "ldr");
        }
    }

    /// Check if an IrConst is a small unsigned immediate that fits in AArch64
    /// `cmp Xn, #imm12` instruction (0..=4095).
    fn const_as_cmp_imm12(c: &IrConst) -> Option<u64> {
        let v = match c {
            IrConst::I8(v) => *v as i64,
            IrConst::I16(v) => *v as i64,
            IrConst::I32(v) => *v as i64,
            IrConst::I64(v) => *v,
            IrConst::Zero => 0,
            _ => return None,
        };
        // AArch64 cmp (alias of subs) accepts unsigned 12-bit immediate (0..4095),
        // optionally shifted left by 12. We only use the unshifted form.
        if v >= 0 && v <= 4095 {
            Some(v as u64)
        } else {
            None
        }
    }

    /// Check if an IrConst is a small negative value that can use `cmn Xn, #imm12`
    /// (i.e., the negated value fits in 0..=4095).
    fn const_as_cmn_imm12(c: &IrConst) -> Option<u64> {
        let v = match c {
            IrConst::I8(v) => *v as i64,
            IrConst::I16(v) => *v as i64,
            IrConst::I32(v) => *v as i64,
            IrConst::I64(v) => *v,
            _ => return None,
        };
        if v < 0 && (-v) >= 1 && (-v) <= 4095 {
            Some((-v) as u64)
        } else {
            None
        }
    }

    /// Get the register name for a Value if it has a register assignment.
    /// Returns (64-bit name, 32-bit name) pair.
    fn value_reg_name(&self, v: &Value) -> Option<(&'static str, &'static str)> {
        self.reg_assignments.get(&v.0).map(|&reg| {
            (callee_saved_name(reg), callee_saved_name_32(reg))
        })
    }

    /// Emit the integer comparison preamble.
    /// Optimized paths:
    ///   1. reg vs #imm12 → `cmp wN/xN, #imm` (1 instruction)
    ///   2. reg vs #neg_imm12 → `cmn wN/xN, #imm` (1 instruction)
    ///   3. reg vs reg → `cmp wN/xN, wM/xM` (1 instruction)
    ///   4. fallback → load lhs→x1, rhs→x0, `cmp w1/x1, w0/x0`
    /// Used by both emit_cmp and emit_fused_cmp_branch.
    fn emit_int_cmp_insn(&mut self, lhs: &Operand, rhs: &Operand, ty: IrType) {
        let use_32bit = ty == IrType::I32 || ty == IrType::U32
            || ty == IrType::I8 || ty == IrType::U8
            || ty == IrType::I16 || ty == IrType::U16;

        // Try optimized path: lhs in register, rhs is immediate
        if let Operand::Value(lv) = lhs {
            if let Some((lhs_x, lhs_w)) = self.value_reg_name(lv) {
                let lhs_reg = if use_32bit { lhs_w } else { lhs_x };

                // cmp reg, #imm12
                if let Operand::Const(c) = rhs {
                    if let Some(imm) = Self::const_as_cmp_imm12(c) {
                        self.state.emit_fmt(format_args!("    cmp {}, #{}", lhs_reg, imm));
                        return;
                    }
                    // cmn reg, #imm12 (for negative constants)
                    if let Some(imm) = Self::const_as_cmn_imm12(c) {
                        self.state.emit_fmt(format_args!("    cmn {}, #{}", lhs_reg, imm));
                        return;
                    }
                }

                // cmp reg, reg
                if let Operand::Value(rv) = rhs {
                    if let Some((rhs_x, rhs_w)) = self.value_reg_name(rv) {
                        let rhs_reg = if use_32bit { rhs_w } else { rhs_x };
                        self.state.emit_fmt(format_args!("    cmp {}, {}", lhs_reg, rhs_reg));
                        return;
                    }
                }

                // lhs in register, rhs needs loading into x0
                self.operand_to_x0(rhs);
                if use_32bit {
                    self.state.emit_fmt(format_args!("    cmp {}, w0", lhs_reg));
                } else {
                    self.state.emit_fmt(format_args!("    cmp {}, x0", lhs_reg));
                }
                return;
            }
        }

        // Try: lhs needs loading, rhs in register
        if let Operand::Value(rv) = rhs {
            if let Some((rhs_x, rhs_w)) = self.value_reg_name(rv) {
                self.operand_to_x0(lhs);
                let rhs_reg = if use_32bit { rhs_w } else { rhs_x };
                if use_32bit {
                    self.state.emit_fmt(format_args!("    cmp w0, {}", rhs_reg));
                } else {
                    self.state.emit_fmt(format_args!("    cmp x0, {}", rhs_reg));
                }
                return;
            }
        }

        // Try: lhs in x0 (accumulator), rhs is immediate
        if let Operand::Const(c) = rhs {
            if let Some(imm) = Self::const_as_cmp_imm12(c) {
                self.operand_to_x0(lhs);
                if use_32bit {
                    self.state.emit_fmt(format_args!("    cmp w0, #{}", imm));
                } else {
                    self.state.emit_fmt(format_args!("    cmp x0, #{}", imm));
                }
                return;
            }
            if let Some(imm) = Self::const_as_cmn_imm12(c) {
                self.operand_to_x0(lhs);
                if use_32bit {
                    self.state.emit_fmt(format_args!("    cmn w0, #{}", imm));
                } else {
                    self.state.emit_fmt(format_args!("    cmn x0, #{}", imm));
                }
                return;
            }
        }

        // Fallback: load both into x0/x1
        self.operand_to_x0(lhs);
        self.state.emit("    mov x1, x0");
        self.operand_to_x0(rhs);
        if use_32bit {
            self.state.emit("    cmp w1, w0");
        } else {
            self.state.emit("    cmp x1, x0");
        }
    }

    // --- AArch64 large-offset helpers ---

    /// Emit a large immediate subtraction from sp. Uses x17 (IP1) as scratch.
    fn emit_sub_sp(&mut self, n: i64) {
        if n == 0 { return; }
        if n <= 4095 {
            self.state.emit_fmt(format_args!("    sub sp, sp, #{}", n));
        } else {
            self.emit_load_imm64("x17", n);
            self.state.emit("    sub sp, sp, x17");
        }
    }

    /// Emit a large immediate addition to sp. Uses x17 (IP1) as scratch.
    fn emit_add_sp(&mut self, n: i64) {
        if n == 0 { return; }
        if n <= 4095 {
            self.state.emit_fmt(format_args!("    add sp, sp, #{}", n));
        } else {
            self.emit_load_imm64("x17", n);
            self.state.emit("    add sp, sp, x17");
        }
    }

    /// Get the access size in bytes for an AArch64 load/store instruction and register.
    /// For str/ldr, the access size depends on the register:
    /// w registers = 4 bytes, x registers = 8 bytes,
    /// s (single-precision float) = 4 bytes, d (double-precision float) = 8 bytes,
    /// q (SIMD/quad) = 16 bytes.
    fn access_size_for_instr(instr: &str, reg: &str) -> i64 {
        match instr {
            "strb" | "ldrb" | "ldrsb" => 1,
            "strh" | "ldrh" | "ldrsh" => 2,
            "ldrsw" => 4,
            "str" | "ldr" => {
                if reg.starts_with('w') || reg.starts_with('s') {
                    4
                } else if reg.starts_with('q') {
                    16
                } else {
                    // x registers and d registers are both 8 bytes
                    8
                }
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

    /// Emit store to [base, #offset], handling large offsets.
    /// For large frames with x19 as frame base register, tries x19-relative addressing
    /// before falling back to the expensive movz+movk+add sequence.
    fn emit_store_to_sp(&mut self, reg: &str, offset: i64, instr: &str) {
        // When DynAlloca is present, use x29 (frame pointer) as base.
        let base = if self.state.has_dyn_alloca { "x29" } else { "sp" };
        if Self::is_valid_imm_offset(offset, instr, reg) {
            self.state.emit_fmt(format_args!("    {} {}, [{}, #{}]", instr, reg, base, offset));
        } else if let Some(fb_offset) = self.frame_base_offset {
            // Try x19-relative addressing (x19 = sp + frame_base_offset)
            let rel_offset = offset - fb_offset;
            if Self::is_valid_imm_offset(rel_offset, instr, reg) {
                self.state.emit_fmt(format_args!("    {} {}, [x19, #{}]", instr, reg, rel_offset));
            } else {
                self.load_large_imm("x17", offset);
                self.state.emit_fmt(format_args!("    add x17, {}, x17", base));
                self.state.emit_fmt(format_args!("    {} {}, [x17]", instr, reg));
            }
        } else {
            self.load_large_imm("x17", offset);
            self.state.emit_fmt(format_args!("    add x17, {}, x17", base));
            self.state.emit_fmt(format_args!("    {} {}, [x17]", instr, reg));
        }
    }

    /// Emit load from [base, #offset], handling large offsets.
    /// For large frames with x19 as frame base register, tries x19-relative addressing.
    fn emit_load_from_sp(&mut self, reg: &str, offset: i64, instr: &str) {
        let base = if self.state.has_dyn_alloca { "x29" } else { "sp" };
        if Self::is_valid_imm_offset(offset, instr, reg) {
            self.state.emit_fmt(format_args!("    {} {}, [{}, #{}]", instr, reg, base, offset));
        } else if let Some(fb_offset) = self.frame_base_offset {
            let rel_offset = offset - fb_offset;
            if Self::is_valid_imm_offset(rel_offset, instr, reg) {
                self.state.emit_fmt(format_args!("    {} {}, [x19, #{}]", instr, reg, rel_offset));
            } else {
                self.load_large_imm("x17", offset);
                self.state.emit_fmt(format_args!("    add x17, {}, x17", base));
                self.state.emit_fmt(format_args!("    {} {}, [x17]", instr, reg));
            }
        } else {
            self.load_large_imm("x17", offset);
            self.state.emit_fmt(format_args!("    add x17, {}, x17", base));
            self.state.emit_fmt(format_args!("    {} {}, [x17]", instr, reg));
        }
    }

    /// Emit store to [sp, #offset] using the REAL sp register, even when alloca is present.
    /// Used for storing into dynamically-allocated call stack arg areas that live at the
    /// current sp, NOT in the frame (x29-relative).
    fn emit_store_to_raw_sp(&mut self, reg: &str, offset: i64, instr: &str) {
        if Self::is_valid_imm_offset(offset, instr, reg) {
            self.state.emit_fmt(format_args!("    {} {}, [sp, #{}]", instr, reg, offset));
        } else {
            self.load_large_imm("x17", offset);
            self.state.emit("    add x17, sp, x17");
            self.state.emit_fmt(format_args!("    {} {}, [x17]", instr, reg));
        }
    }

    /// Emit `stp reg1, reg2, [base, #offset]` handling large offsets.
    /// Uses x19 frame base for large frames when possible.
    fn emit_stp_to_sp(&mut self, reg1: &str, reg2: &str, offset: i64) {
        let base = if self.state.has_dyn_alloca { "x29" } else { "sp" };
        // stp supports signed offsets in [-512, 504] range (multiples of 8)
        if offset >= -512 && offset <= 504 {
            self.state.emit_fmt(format_args!("    stp {}, {}, [{}, #{}]", reg1, reg2, base, offset));
        } else if let Some(fb_offset) = self.frame_base_offset {
            let rel = offset - fb_offset;
            if rel >= -512 && rel <= 504 {
                self.state.emit_fmt(format_args!("    stp {}, {}, [x19, #{}]", reg1, reg2, rel));
            } else {
                self.load_large_imm("x17", offset);
                self.state.emit_fmt(format_args!("    add x17, {}, x17", base));
                self.state.emit_fmt(format_args!("    stp {}, {}, [x17]", reg1, reg2));
            }
        } else {
            self.load_large_imm("x17", offset);
            self.state.emit_fmt(format_args!("    add x17, {}, x17", base));
            self.state.emit_fmt(format_args!("    stp {}, {}, [x17]", reg1, reg2));
        }
    }

    fn emit_ldp_from_sp(&mut self, reg1: &str, reg2: &str, offset: i64) {
        let base = if self.state.has_dyn_alloca { "x29" } else { "sp" };
        if offset >= -512 && offset <= 504 {
            self.state.emit_fmt(format_args!("    ldp {}, {}, [{}, #{}]", reg1, reg2, base, offset));
        } else if let Some(fb_offset) = self.frame_base_offset {
            let rel = offset - fb_offset;
            if rel >= -512 && rel <= 504 {
                self.state.emit_fmt(format_args!("    ldp {}, {}, [x19, #{}]", reg1, reg2, rel));
            } else {
                self.load_large_imm("x17", offset);
                self.state.emit_fmt(format_args!("    add x17, {}, x17", base));
                self.state.emit_fmt(format_args!("    ldp {}, {}, [x17]", reg1, reg2));
            }
        } else {
            self.load_large_imm("x17", offset);
            self.state.emit_fmt(format_args!("    add x17, {}, x17", base));
            self.state.emit_fmt(format_args!("    ldp {}, {}, [x17]", reg1, reg2));
        }
    }

    /// Emit `add dest, sp, #offset` handling large offsets.
    /// Uses x19 frame base when available, falls back to x17 scratch.
    fn emit_add_sp_offset(&mut self, dest: &str, offset: i64) {
        let base = if self.state.has_dyn_alloca { "x29" } else { "sp" };
        if offset >= 0 && offset <= 4095 {
            self.state.emit_fmt(format_args!("    add {}, {}, #{}", dest, base, offset));
        } else if let Some(fb_offset) = self.frame_base_offset {
            let rel = offset - fb_offset;
            if rel >= 0 && rel <= 4095 {
                self.state.emit_fmt(format_args!("    add {}, x19, #{}", dest, rel));
            } else if rel >= -4095 && rel < 0 {
                self.state.emit_fmt(format_args!("    sub {}, x19, #{}", dest, -rel));
            } else {
                self.load_large_imm("x17", offset);
                self.state.emit_fmt(format_args!("    add {}, {}, x17", dest, base));
            }
        } else {
            self.load_large_imm("x17", offset);
            self.state.emit_fmt(format_args!("    add {}, {}, x17", dest, base));
        }
    }

    /// Emit `add dest, x29, #offset` handling large offsets.
    /// Uses x17 (IP1) as scratch for offsets > 4095.
    fn emit_add_fp_offset(&mut self, dest: &str, offset: i64) {
        if offset >= 0 && offset <= 4095 {
            self.state.emit_fmt(format_args!("    add {}, x29, #{}", dest, offset));
        } else {
            self.load_large_imm("x17", offset);
            self.state.emit_fmt(format_args!("    add {}, x29, x17", dest));
        }
    }

    /// Emit load from an arbitrary base register with offset, handling large offsets via x17.
    /// For offsets that exceed the ARM64 unsigned immediate range, materializes the
    /// effective address into x17 and loads from [x17].
    fn emit_load_from_reg(&mut self, dest: &str, base: &str, offset: i64, instr: &str) {
        if Self::is_valid_imm_offset(offset, instr, dest) {
            self.state.emit_fmt(format_args!("    {} {}, [{}, #{}]", instr, dest, base, offset));
        } else {
            self.load_large_imm("x17", offset);
            self.state.emit_fmt(format_args!("    add x17, {}, x17", base));
            self.state.emit_fmt(format_args!("    {} {}, [x17]", instr, dest));
        }
    }

    /// Emit store to an arbitrary base register with offset, handling large offsets via x17.
    #[allow(dead_code)]
    fn emit_store_to_reg(&mut self, src: &str, base: &str, offset: i64, instr: &str) {
        if Self::is_valid_imm_offset(offset, instr, src) {
            self.state.emit_fmt(format_args!("    {} {}, [{}, #{}]", instr, src, base, offset));
        } else {
            self.load_large_imm("x17", offset);
            self.state.emit_fmt(format_args!("    add x17, {}, x17", base));
            self.state.emit_fmt(format_args!("    {} {}, [x17]", instr, src));
        }
    }

    /// Load an immediate into a register using the most efficient sequence.
    /// Handles all 64-bit values including negatives via MOVZ/MOVK or MOVN/MOVK.
    fn load_large_imm(&mut self, reg: &str, val: i64) {
        self.emit_load_imm64(reg, val);
    }

    /// Load a 64-bit immediate value into a register using movz/movn + movk sequence.
    /// Uses MOVN (move-not) for values where most halfwords are 0xFFFF, which
    /// gives shorter sequences for negative numbers and large values.
    fn emit_load_imm64(&mut self, reg: &str, val: i64) {
        let bits = val as u64;
        if bits == 0 {
            self.state.emit_fmt(format_args!("    mov {}, #0", reg));
            return;
        }
        if bits == 0xFFFFFFFF_FFFFFFFF {
            // All-ones: MOVN reg, #0 produces NOT(0) = 0xFFFFFFFFFFFFFFFF
            self.state.emit_fmt(format_args!("    movn {}, #0", reg));
            return;
        }

        // Extract 16-bit halfwords
        let hw: [u16; 4] = [
            (bits & 0xffff) as u16,
            ((bits >> 16) & 0xffff) as u16,
            ((bits >> 32) & 0xffff) as u16,
            ((bits >> 48) & 0xffff) as u16,
        ];

        // Count how many halfwords are 0x0000 vs 0xFFFF to pick MOVZ vs MOVN
        let zeros = hw.iter().filter(|&&h| h == 0x0000).count();
        let ones = hw.iter().filter(|&&h| h == 0xFFFF).count();

        if ones > zeros {
            // Use MOVN (move-not) strategy: start with all-ones, patch non-0xFFFF halfwords
            // MOVN sets the register to NOT(imm16 << shift)
            let mut first = true;
            for (i, &h) in hw.iter().enumerate() {
                if h != 0xFFFF {
                    let shift = i * 16;
                    let not_h = (!h) as u64 & 0xffff;
                    if first {
                        if shift == 0 {
                            self.state.emit_fmt(format_args!("    movn {}, #{}", reg, not_h));
                        } else {
                            self.state.emit_fmt(format_args!("    movn {}, #{}, lsl #{}", reg, not_h, shift));
                        }
                        first = false;
                    } else {
                        if shift == 0 {
                            self.state.emit_fmt(format_args!("    movk {}, #{}", reg, h as u64));
                        } else {
                            self.state.emit_fmt(format_args!("    movk {}, #{}, lsl #{}", reg, h as u64, shift));
                        }
                    }
                }
            }
        } else {
            // Use MOVZ (move-zero) strategy: start with all-zeros, patch non-0x0000 halfwords
            let mut first = true;
            for (i, &h) in hw.iter().enumerate() {
                if h != 0x0000 {
                    let shift = i * 16;
                    if first {
                        if shift == 0 {
                            self.state.emit_fmt(format_args!("    movz {}, #{}", reg, h as u64));
                        } else {
                            self.state.emit_fmt(format_args!("    movz {}, #{}, lsl #{}", reg, h as u64, shift));
                        }
                        first = false;
                    } else {
                        if shift == 0 {
                            self.state.emit_fmt(format_args!("    movk {}, #{}", reg, h as u64));
                        } else {
                            self.state.emit_fmt(format_args!("    movk {}, #{}, lsl #{}", reg, h as u64, shift));
                        }
                    }
                }
            }
        }
    }

    /// Emit function prologue: allocate stack and save fp/lr.
    fn emit_prologue_arm(&mut self, frame_size: i64) {
        const PAGE_SIZE: i64 = 4096;
        if frame_size > 0 && frame_size <= 504 {
            self.state.emit_fmt(format_args!("    stp x29, x30, [sp, #-{}]!", frame_size));
        } else if frame_size > PAGE_SIZE {
            // Stack probing: for large frames, touch each page so the kernel
            // can grow the stack mapping. Without this, a single large sub
            // can skip guard pages and cause a segfault.
            let probe_label = self.state.fresh_label("stack_probe");
            self.emit_load_imm64("x17", frame_size);
            self.state.emit_fmt(format_args!("{}:", probe_label));
            self.state.emit_fmt(format_args!("    sub sp, sp, #{}", PAGE_SIZE));
            self.state.emit("    str xzr, [sp]");
            self.state.emit_fmt(format_args!("    sub x17, x17, #{}", PAGE_SIZE));
            self.state.emit_fmt(format_args!("    cmp x17, #{}", PAGE_SIZE));
            self.state.emit_fmt(format_args!("    b.hi {}", probe_label));
            self.state.emit("    sub sp, sp, x17");
            self.state.emit("    str xzr, [sp]");
            self.state.emit("    stp x29, x30, [sp]");
        } else {
            self.emit_sub_sp(frame_size);
            self.state.emit("    stp x29, x30, [sp]");
        }
        self.state.emit("    mov x29, sp");
    }

    /// Emit function epilogue: restore fp/lr and deallocate stack.
    fn emit_epilogue_arm(&mut self, frame_size: i64) {
        if self.state.has_dyn_alloca {
            // DynAlloca modified SP at runtime; restore from frame pointer.
            self.state.emit("    mov sp, x29");
        }
        if frame_size > 0 && frame_size <= 504 {
            self.state.emit_fmt(format_args!("    ldp x29, x30, [sp], #{}", frame_size));
        } else {
            self.state.emit("    ldp x29, x30, [sp]");
            self.emit_add_sp(frame_size);
        }
    }

    /// Load an operand into x0.
    fn operand_to_x0(&mut self, op: &Operand) {
        match op {
            Operand::Const(c) => {
                self.state.reg_cache.invalidate_acc();
                match c {
                    IrConst::I8(v) => self.state.emit_fmt(format_args!("    mov x0, #{}", v)),
                    IrConst::I16(v) => self.state.emit_fmt(format_args!("    mov x0, #{}", v)),
                    IrConst::I32(v) => {
                        if *v >= 0 && *v <= 65535 {
                            self.state.emit_fmt(format_args!("    mov x0, #{}", v));
                        } else if *v < 0 && *v >= -65536 {
                            self.state.emit_fmt(format_args!("    mov x0, #{}", v));
                        } else {
                            // Sign-extend to 64-bit before loading into x0.
                            // Using the i64 path ensures negative I32 values get
                            // proper sign extension (upper 32 bits = 0xFFFFFFFF).
                            self.emit_load_imm64("x0", *v as i64);
                        }
                    }
                    IrConst::I64(v) => {
                        if *v >= 0 && *v <= 65535 {
                            self.state.emit_fmt(format_args!("    mov x0, #{}", v));
                        } else if *v < 0 && *v >= -65536 {
                            self.state.emit_fmt(format_args!("    mov x0, #{}", v));
                        } else {
                            self.emit_load_imm64("x0", *v);
                        }
                    }
                    IrConst::F32(v) => self.emit_load_imm64("x0", v.to_bits() as i64),
                    IrConst::F64(v) => self.emit_load_imm64("x0", v.to_bits() as i64),
                    IrConst::LongDouble(v, _) => self.emit_load_imm64("x0", v.to_bits() as i64),
                    IrConst::I128(v) => self.emit_load_imm64("x0", *v as i64), // truncate to 64-bit
                    IrConst::Zero => self.state.emit("    mov x0, #0"),
                }
            }
            Operand::Value(v) => {
                let is_alloca = self.state.is_alloca(v.0);
                if self.state.reg_cache.acc_has(v.0, is_alloca) {
                    return; // Cache hit — x0 already holds this value.
                }
                // Check for callee-saved register assignment.
                if let Some(&reg) = self.reg_assignments.get(&v.0) {
                    let reg_name = callee_saved_name(reg);
                    self.state.emit_fmt(format_args!("    mov x0, {}", reg_name));
                    self.state.reg_cache.set_acc(v.0, false);
                    return;
                }
                if let Some(slot) = self.state.get_slot(v.0) {
                    if is_alloca {
                        if let Some(align) = self.state.alloca_over_align(v.0) {
                            // Over-aligned alloca: compute aligned address.
                            // x0 = (slot_addr + align-1) & -align
                            self.emit_add_sp_offset("x0", slot.0);
                            self.load_large_imm("x17", (align - 1) as i64);
                            self.state.emit("    add x0, x0, x17");
                            self.load_large_imm("x17", -(align as i64));
                            self.state.emit("    and x0, x0, x17");
                        } else {
                            self.emit_add_sp_offset("x0", slot.0);
                        }
                    } else {
                        self.emit_load_from_sp("x0", slot.0, "ldr");
                    }
                    self.state.reg_cache.set_acc(v.0, is_alloca);
                } else {
                    self.state.emit("    mov x0, #0");
                    self.state.reg_cache.invalidate_acc();
                }
            }
        }
    }

    /// Store x0 to a value's destination (register or stack slot).
    fn store_x0_to(&mut self, dest: &Value) {
        if let Some(&reg) = self.reg_assignments.get(&dest.0) {
            // Value has a callee-saved register: store only to register, skip stack.
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mov {}, x0", reg_name));
        } else if let Some(slot) = self.state.get_slot(dest.0) {
            self.emit_store_to_sp("x0", slot.0, "str");
        }
        self.state.reg_cache.set_acc(dest.0, false);
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
                    } else if self.state.is_i128_value(v.0) {
                        // 128-bit value in 16-byte stack slot
                        self.emit_load_from_sp("x0", slot.0, "ldr");
                        self.emit_load_from_sp("x1", slot.0 + 8, "ldr");
                    } else {
                        // Non-i128 value (e.g. shift amount): load 8 bytes, zero high
                        // Check register allocation first, since register-allocated values
                        // may not have their stack slot written.
                        if let Some(&reg) = self.reg_assignments.get(&v.0) {
                            let reg_name = callee_saved_name(reg);
                            self.state.emit_fmt(format_args!("    mov x0, {}", reg_name));
                        } else {
                            self.emit_load_from_sp("x0", slot.0, "ldr");
                        }
                        self.state.emit("    mov x1, #0");
                    }
                } else {
                    // No stack slot: check register allocation
                    if let Some(&reg) = self.reg_assignments.get(&v.0) {
                        let reg_name = callee_saved_name(reg);
                        self.state.emit_fmt(format_args!("    mov x0, {}", reg_name));
                        self.state.emit("    mov x1, #0");
                    } else {
                        self.state.emit("    mov x0, #0");
                        self.state.emit("    mov x1, #0");
                    }
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
    // emit_i128_binop and emit_i128_cmp use the shared default implementations
    // via ArchCodegen trait defaults, with per-op primitives defined in the trait impl above.

    fn str_for_type(ty: IrType) -> &'static str {
        match ty {
            IrType::I8 | IrType::U8 => "strb",
            IrType::I16 | IrType::U16 => "strh",
            IrType::I32 | IrType::U32 | IrType::F32 => "str",  // 32-bit store with w register
            _ => "str",  // 64-bit store with x register
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

    // --- Intrinsic helpers (NEON) ---

    /// Load the address represented by a pointer Value into the given register.
    /// For alloca values, computes the address; for others, loads the stored pointer.
    fn load_ptr_to_reg(&mut self, ptr: &Value, reg: &str) {
        if let Some(slot) = self.state.get_slot(ptr.0) {
            if self.state.is_alloca(ptr.0) {
                self.emit_add_sp_offset(reg, slot.0);
            } else {
                self.emit_load_from_sp(reg, slot.0, "ldr");
            }
        }
    }

    /// Emit a NEON binary 128-bit operation: load from args[0] and args[1] pointers,
    /// apply the NEON instruction, store result to dest_ptr.
    fn emit_neon_binary_128(&mut self, dest_ptr: &Value, args: &[Operand], neon_inst: &str) {
        // Load first 128-bit operand pointer into x0, then load q0
        self.operand_to_x0(&args[0]);
        self.state.emit("    ldr q0, [x0]");
        // Load second 128-bit operand pointer into x1, then load q1
        match &args[1] {
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    if self.state.is_alloca(v.0) {
                        self.emit_add_sp_offset("x1", slot.0);
                    } else {
                        self.emit_load_from_sp("x1", slot.0, "ldr");
                    }
                }
            }
            Operand::Const(_) => {
                self.operand_to_x0(&args[1]);
                self.state.emit("    mov x1, x0");
            }
        }
        self.state.emit("    ldr q1, [x1]");
        // Apply the binary NEON operation
        self.state.emit_fmt(format_args!("    {} v0.16b, v0.16b, v1.16b", neon_inst));
        // Store result to dest_ptr
        self.load_ptr_to_reg(dest_ptr, "x0");
        self.state.emit("    str q0, [x0]");
    }

    fn emit_intrinsic_arm(&mut self, dest: &Option<Value>, op: &IntrinsicOp, dest_ptr: &Option<Value>, args: &[Operand]) {
        match op {
            IntrinsicOp::Lfence | IntrinsicOp::Mfence => {
                self.state.emit("    dmb ish");
            }
            IntrinsicOp::Sfence => {
                self.state.emit("    dmb ishst");
            }
            IntrinsicOp::Pause => {
                self.state.emit("    yield");
            }
            IntrinsicOp::Clflush => {
                // ARM has no direct clflush; use dc civac (clean+invalidate to PoC)
                self.operand_to_x0(&args[0]);
                self.state.emit("    dc civac, x0");
            }
            IntrinsicOp::Movnti => {
                // Non-temporal 32-bit store: dest_ptr = target address, args[0] = value
                if let Some(ptr) = dest_ptr {
                    self.operand_to_x0(&args[0]);
                    self.state.emit("    mov w9, w0");
                    self.load_ptr_to_reg(ptr, "x0");
                    self.state.emit("    str w9, [x0]");
                }
            }
            IntrinsicOp::Movnti64 => {
                // Non-temporal 64-bit store
                if let Some(ptr) = dest_ptr {
                    self.operand_to_x0(&args[0]);
                    self.state.emit("    mov x9, x0");
                    self.load_ptr_to_reg(ptr, "x0");
                    self.state.emit("    str x9, [x0]");
                }
            }
            IntrinsicOp::Movntdq | IntrinsicOp::Movntpd => {
                // Non-temporal 128-bit store: dest_ptr = target, args[0] = source ptr
                if let Some(ptr) = dest_ptr {
                    self.operand_to_x0(&args[0]);
                    self.state.emit("    ldr q0, [x0]");
                    self.load_ptr_to_reg(ptr, "x0");
                    self.state.emit("    str q0, [x0]");
                }
            }
            IntrinsicOp::Loaddqu => {
                // Load 128-bit unaligned: args[0] = source ptr, dest_ptr = result storage
                if let Some(dptr) = dest_ptr {
                    self.operand_to_x0(&args[0]);
                    self.state.emit("    ldr q0, [x0]");
                    self.load_ptr_to_reg(dptr, "x0");
                    self.state.emit("    str q0, [x0]");
                }
            }
            IntrinsicOp::Storedqu => {
                // Store 128-bit unaligned: dest_ptr = target ptr, args[0] = source data ptr
                if let Some(ptr) = dest_ptr {
                    self.operand_to_x0(&args[0]);
                    self.state.emit("    ldr q0, [x0]");
                    self.load_ptr_to_reg(ptr, "x0");
                    self.state.emit("    str q0, [x0]");
                }
            }
            IntrinsicOp::Pcmpeqb128 => {
                if let Some(dptr) = dest_ptr {
                    // cmeq compares and sets all bits in each lane on equality
                    self.emit_neon_binary_128(dptr, args, "cmeq");
                }
            }
            IntrinsicOp::Pcmpeqd128 => {
                if let Some(dptr) = dest_ptr {
                    // For 32-bit lane equality, load q regs, use cmeq with .4s arrangement
                    self.operand_to_x0(&args[0]);
                    self.state.emit("    ldr q0, [x0]");
                    if let Operand::Value(v) = &args[1] {
                        self.load_ptr_to_reg(v, "x1");
                    } else {
                        self.operand_to_x0(&args[1]);
                        self.state.emit("    mov x1, x0");
                    }
                    self.state.emit("    ldr q1, [x1]");
                    self.state.emit("    cmeq v0.4s, v0.4s, v1.4s");
                    self.load_ptr_to_reg(dptr, "x0");
                    self.state.emit("    str q0, [x0]");
                }
            }
            IntrinsicOp::Psubusb128 => {
                if let Some(dptr) = dest_ptr {
                    self.emit_neon_binary_128(dptr, args, "uqsub");
                }
            }
            IntrinsicOp::Por128 => {
                if let Some(dptr) = dest_ptr {
                    self.emit_neon_binary_128(dptr, args, "orr");
                }
            }
            IntrinsicOp::Pand128 => {
                if let Some(dptr) = dest_ptr {
                    self.emit_neon_binary_128(dptr, args, "and");
                }
            }
            IntrinsicOp::Pxor128 => {
                if let Some(dptr) = dest_ptr {
                    self.emit_neon_binary_128(dptr, args, "eor");
                }
            }
            IntrinsicOp::Pmovmskb128 => {
                // Extract the high bit of each byte in a 128-bit vector into a 16-bit mask.
                // NEON has no pmovmskb equivalent, so we use a multi-step sequence:
                //   1. Load 128-bit data into v0
                //   2. Shift right each byte by 7 to isolate the sign bit (ushr v0.16b, v0.16b, #7)
                //   3. Collect bits using successive narrowing and shifts
                // Efficient approach: multiply by power-of-2 bit positions, then add across lanes.
                self.operand_to_x0(&args[0]);
                self.state.emit("    ldr q0, [x0]");
                // Shift right by 7 to get 0 or 1 in each byte lane
                self.state.emit("    ushr v0.16b, v0.16b, #7");
                // Load the bit position constants: [1,2,4,8,16,32,64,128, 1,2,4,8,16,32,64,128]
                // 0x8040201008040201 loaded via movz/movk sequence
                self.state.emit("    movz x0, #0x0201");
                self.state.emit("    movk x0, #0x0804, lsl #16");
                self.state.emit("    movk x0, #0x2010, lsl #32");
                self.state.emit("    movk x0, #0x8040, lsl #48");
                self.state.emit("    fmov d1, x0");
                self.state.emit("    mov v1.d[1], x0");
                // Multiply each byte: v0[i] * v1[i] gives the bit contribution
                self.state.emit("    mul v0.16b, v0.16b, v1.16b");
                // Now sum bytes 0-7 into low byte, and bytes 8-15 into high byte
                // addv sums all lanes into a scalar - but we need two separate sums
                // Use ext to split, then addv each half
                self.state.emit("    ext v1.16b, v0.16b, v0.16b, #8");
                // v0 has low 8 bytes, v1 has high 8 bytes (shifted)
                // Sum low 8 bytes
                self.state.emit("    addv b0, v0.8b");
                self.state.emit("    umov w0, v0.b[0]");
                // Sum high 8 bytes
                self.state.emit("    addv b1, v1.8b");
                self.state.emit("    umov w1, v1.b[0]");
                // Combine: result = low_sum | (high_sum << 8)
                self.state.emit("    orr w0, w0, w1, lsl #8");
                // Store scalar result
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        self.emit_store_to_sp("x0", slot.0, "str");
                    }
                }
            }
            IntrinsicOp::SetEpi8 => {
                // Splat a byte value to all 16 bytes: args[0] = byte value
                if let Some(dptr) = dest_ptr {
                    self.operand_to_x0(&args[0]);
                    self.state.emit("    dup v0.16b, w0");
                    self.load_ptr_to_reg(dptr, "x0");
                    self.state.emit("    str q0, [x0]");
                }
            }
            IntrinsicOp::SetEpi32 => {
                // Splat a 32-bit value to all 4 lanes: args[0] = 32-bit value
                if let Some(dptr) = dest_ptr {
                    self.operand_to_x0(&args[0]);
                    self.state.emit("    dup v0.4s, w0");
                    self.load_ptr_to_reg(dptr, "x0");
                    self.state.emit("    str q0, [x0]");
                }
            }
            IntrinsicOp::Crc32_8 | IntrinsicOp::Crc32_16
            | IntrinsicOp::Crc32_32 | IntrinsicOp::Crc32_64 => {
                let is_64 = matches!(op, IntrinsicOp::Crc32_64);
                let (save_reg, crc_inst) = match op {
                    IntrinsicOp::Crc32_8  => ("w9", "crc32cb w9, w9, w0"),
                    IntrinsicOp::Crc32_16 => ("w9", "crc32ch w9, w9, w0"),
                    IntrinsicOp::Crc32_32 => ("w9", "crc32cw w9, w9, w0"),
                    IntrinsicOp::Crc32_64 => ("x9", "crc32cx w9, w9, x0"),
                    _ => unreachable!(),
                };
                self.operand_to_x0(&args[0]);
                self.state.emit_fmt(format_args!("    mov {}, {}", save_reg, if is_64 { "x0" } else { "w0" }));
                self.operand_to_x0(&args[1]);
                self.state.emit_fmt(format_args!("    {}", crc_inst));
                self.state.emit("    mov x0, x9");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        self.emit_store_to_sp("x0", slot.0, "str");
                    }
                }
            }
            IntrinsicOp::FrameAddress => {
                // __builtin_frame_address(0): return current frame pointer (x29)
                self.state.emit("    mov x0, x29");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        self.emit_store_to_sp("x0", slot.0, "str");
                    }
                }
            }
            IntrinsicOp::ReturnAddress => {
                // __builtin_return_address(0): return address saved at [x29, #8]
                // x30 (lr) is clobbered by bl instructions, so read from stack
                self.state.emit("    ldr x0, [x29, #8]");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        self.emit_store_to_sp("x0", slot.0, "str");
                    }
                }
            }
            IntrinsicOp::SqrtF64 => {
                self.operand_to_x0(&args[0]);
                self.state.emit("    fmov d0, x0");
                self.state.emit("    fsqrt d0, d0");
                self.state.emit("    fmov x0, d0");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        self.emit_store_to_sp("x0", slot.0, "str");
                    }
                }
            }
            IntrinsicOp::SqrtF32 => {
                self.operand_to_x0(&args[0]);
                self.state.emit("    fmov s0, w0");
                self.state.emit("    fsqrt s0, s0");
                self.state.emit("    fmov w0, s0");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        self.emit_store_to_sp("w0", slot.0, "str");
                    }
                }
            }
            IntrinsicOp::FabsF64 => {
                self.operand_to_x0(&args[0]);
                self.state.emit("    fmov d0, x0");
                self.state.emit("    fabs d0, d0");
                self.state.emit("    fmov x0, d0");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        self.emit_store_to_sp("x0", slot.0, "str");
                    }
                }
            }
            IntrinsicOp::FabsF32 => {
                self.operand_to_x0(&args[0]);
                self.state.emit("    fmov s0, w0");
                self.state.emit("    fabs s0, s0");
                self.state.emit("    fmov w0, s0");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        self.emit_store_to_sp("w0", slot.0, "str");
                    }
                }
            }
        }
    }

    // ---- F128 (long double / IEEE quad precision) soft-float helpers ----
    //
    // On AArch64, long double is IEEE 754 binary128 (16 bytes).
    // Hardware has no quad-precision FP ops, so we use compiler-rt/libgcc soft-float:
    //   Comparison: __eqtf2, __lttf2, __letf2, __gttf2, __getf2
    //   Arithmetic: __addtf3, __subtf3, __multf3, __divtf3
    //   Conversion: __extenddftf2 (f64->f128), __trunctfdf2 (f128->f64)
    // ABI: f128 passed/returned in Q registers (q0, q1). Int result in w0/x0.

    /// Load an F128 operand into Q0 via f64->f128 conversion.
    /// Since ARM F128 values are stored internally as f64 approximations (the
    /// backend doesn't store full 16-byte f128 in allocas), we consistently use
    /// __extenddftf2 to convert the f64 to f128 for both values and constants.
    /// This ensures that `x == 1.2L` works correctly when x was stored as f64.
    fn emit_f128_operand_to_q0(&mut self, op: &Operand) {
        // Load the f64 approximation into x0, then convert to f128 via __extenddftf2.
        self.operand_to_x0(op);
        self.state.emit("    fmov d0, x0");
        self.state.emit("    bl __extenddftf2");
    }

    /// Emit F128 comparison via soft-float libcalls.
    fn emit_f128_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand) {
        // We need to load both LHS and RHS f128 values, but loading each may
        // involve calls (e.g., __extenddftf2) that clobber Q registers.
        // Strategy: sub sp for temp space, but force x29-relative slot access
        // (since x29 = fp is stable) so sp changes don't break slot offsets.
        let saved_dyn_alloca = self.state.has_dyn_alloca;
        self.state.has_dyn_alloca = true; // Force x29-relative addressing

        // Step 1: Load LHS f128 into Q0, save to stack temp (16 bytes).
        self.emit_sub_sp(16);
        self.emit_f128_operand_to_q0(lhs);
        self.state.emit("    str q0, [sp]");

        // Step 2: Load RHS f128 into Q0, move to Q1.
        self.emit_f128_operand_to_q0(rhs);
        self.state.emit("    mov v1.16b, v0.16b");

        // Step 3: Load saved LHS f128 from stack temp into Q0.
        self.state.emit("    ldr q0, [sp]");

        // Free temp stack space and restore dyn_alloca flag.
        self.emit_add_sp(16);
        self.state.has_dyn_alloca = saved_dyn_alloca;

        // Step 4: Call the appropriate comparison libcall and map result to boolean.
        // All __*tf2 functions take f128 in Q0 (lhs) and Q1 (rhs), return int in W0.
        match op {
            IrCmpOp::Eq => {
                // __eqtf2 returns 0 if equal
                self.state.emit("    bl __eqtf2");
                self.state.emit("    cmp w0, #0");
                self.state.emit("    cset x0, eq");
            }
            IrCmpOp::Ne => {
                // __eqtf2 returns 0 if equal, non-zero otherwise
                self.state.emit("    bl __eqtf2");
                self.state.emit("    cmp w0, #0");
                self.state.emit("    cset x0, ne");
            }
            IrCmpOp::Slt | IrCmpOp::Ult => {
                // __lttf2 returns <0 if a<b (1 if unordered)
                self.state.emit("    bl __lttf2");
                self.state.emit("    cmp w0, #0");
                self.state.emit("    cset x0, lt");
            }
            IrCmpOp::Sle | IrCmpOp::Ule => {
                // __letf2 returns <=0 if a<=b (1 if unordered)
                self.state.emit("    bl __letf2");
                self.state.emit("    cmp w0, #0");
                self.state.emit("    cset x0, le");
            }
            IrCmpOp::Sgt | IrCmpOp::Ugt => {
                // __gttf2 returns >0 if a>b (-1 if unordered)
                self.state.emit("    bl __gttf2");
                self.state.emit("    cmp w0, #0");
                self.state.emit("    cset x0, gt");
            }
            IrCmpOp::Sge | IrCmpOp::Uge => {
                // __getf2 returns >=0 if a>=b (-1 if unordered)
                self.state.emit("    bl __getf2");
                self.state.emit("    cmp w0, #0");
                self.state.emit("    cset x0, ge");
            }
        }
        self.state.reg_cache.invalidate_all();
        self.store_x0_to(dest);
    }

    /// Emit F128 arithmetic via soft-float libcalls.
    /// Called from emit_float_binop_impl when ty == F128.
    /// At entry: x1 = lhs f64 bits, x0 = rhs f64 bits (from shared float binop dispatch).
    fn emit_f128_binop_softfloat(&mut self, mnemonic: &str) {
        let libcall = match mnemonic {
            "fadd" => "__addtf3",
            "fsub" => "__subtf3",
            "fmul" => "__multf3",
            "fdiv" => "__divtf3",
            _ => {
                // Unknown op: fall back to f64 hardware path
                self.state.emit("    mov x2, x0");
                self.state.emit("    fmov d0, x1");
                self.state.emit("    fmov d1, x2");
                self.state.emit_fmt(format_args!("    {} d0, d0, d1", mnemonic));
                self.state.emit("    fmov x0, d0");
                return;
            }
        };

        // At entry from shared emit_float_binop: x1=lhs(f64 bits), x0=rhs(f64 bits)
        // Save rhs to stack, convert lhs to f128, save, convert rhs to f128.
        // Use raw sp addressing for our temp area (not x29-relative) since we
        // adjust sp ourselves and these are OUR temp slots, not frame slots.
        self.state.emit("    sub sp, sp, #32");
        // Save rhs f64
        self.state.emit("    str x0, [sp, #16]");
        // Convert lhs (x1) from f64 to f128
        self.state.emit("    fmov d0, x1");
        self.state.emit("    bl __extenddftf2");
        // Save lhs f128 (Q0)
        self.state.emit("    str q0, [sp]");
        // Load rhs f64 and convert to f128
        self.state.emit("    ldr x0, [sp, #16]");
        self.state.emit("    fmov d0, x0");
        self.state.emit("    bl __extenddftf2");
        // RHS f128 now in Q0, move to Q1 (second arg)
        self.state.emit("    mov v1.16b, v0.16b");
        // Load LHS f128 back to Q0 (first arg)
        self.state.emit("    ldr q0, [sp]");
        // Call the arithmetic libcall: result f128 in Q0
        self.state.emit_fmt(format_args!("    bl {}", libcall));
        // Convert result f128 back to f64 via __trunctfdf2
        self.state.emit("    bl __trunctfdf2");
        // f64 result in D0, move to x0
        self.state.emit("    fmov x0, d0");
        // Free temp space
        self.state.emit("    add sp, sp, #32");
        self.state.reg_cache.invalidate_all();
    }
}

const ARM_ARG_REGS: [&str; 8] = ["x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"];
const ARM_TMP_REGS: [&str; 8] = ["x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16"];

impl ArchCodegen for ArmCodegen {
    fn state(&mut self) -> &mut CodegenState { &mut self.state }
    fn state_ref(&self) -> &CodegenState { &self.state }

    fn jump_mnemonic(&self) -> &'static str { "b" }
    fn trap_instruction(&self) -> &'static str { "brk #0" }

    /// Emit a branch-if-nonzero using a relaxed sequence.
    /// `cbnz` has +-1MB range which can be exceeded in large functions.
    /// We use `cbz x0, .Lskip; b target; .Lskip:` instead, where
    /// the unconditional `b` has +-128MB range.
    fn emit_branch_nonzero(&mut self, label: &str) {
        let skip = self.state.fresh_label("skip");
        self.state.emit_fmt(format_args!("    cbz x0, {}", skip));
        self.state.emit_fmt(format_args!("    b {}", label));
        self.state.emit_fmt(format_args!("{}:", skip));
    }

    fn emit_jump_indirect(&mut self) {
        self.state.emit("    br x0");
    }

    fn emit_switch_case_branch(&mut self, case_val: i64, label: &str) {
        // Load case value into x1, compare, and branch if equal.
        self.emit_load_imm64("x1", case_val);
        self.state.emit("    cmp x0, x1");
        self.state.emit_fmt(format_args!("    b.eq {}", label));
    }

    fn emit_switch_jump_table(&mut self, val: &Operand, cases: &[(i64, BlockId)], default: &BlockId) {
        let min_val = cases.iter().map(|&(v, _)| v).min().unwrap();
        let max_val = cases.iter().map(|&(v, _)| v).max().unwrap();
        let range = (max_val - min_val + 1) as usize;

        // Build the table: for each index in [min..max], find the target or use default
        let mut table = vec![*default; range];
        for &(case_val, target) in cases {
            let idx = (case_val - min_val) as usize;
            table[idx] = target;
        }

        let table_label = self.state.fresh_label("jt");
        let default_label = default.as_label();

        // Load switch value into x0
        self.operand_to_x0(val);

        // Range check: if val < min or val > max, branch to default.
        // Use unsigned comparison: sub x0, x0, #min; cmp x0, #range -> branch if above
        if min_val != 0 {
            if min_val > 0 && min_val <= 4095 {
                self.state.emit_fmt(format_args!("    sub x0, x0, #{}", min_val));
            } else if min_val < 0 && (-min_val) <= 4095 {
                self.state.emit_fmt(format_args!("    add x0, x0, #{}", -min_val));
            } else {
                self.load_large_imm("x17", min_val);
                self.state.emit("    sub x0, x0, x17");
            }
        }
        // Now x0 = val - min_val. Compare unsigned against range.
        if range <= 4095 {
            self.state.emit_fmt(format_args!("    cmp x0, #{}", range));
        } else {
            self.load_large_imm("x17", range as i64);
            self.state.emit("    cmp x0, x17");
        }
        self.state.emit_fmt(format_args!("    b.hs {}", default_label));

        // Load jump table base address and compute target
        // adrp x17, table_label; add x17, x17, :lo12:table_label
        self.state.emit_fmt(format_args!("    adrp x17, {}", table_label));
        self.state.emit_fmt(format_args!("    add x17, x17, :lo12:{}", table_label));
        // Load target address: ldr x17, [x17, x0, lsl #3]
        self.state.emit("    ldr x17, [x17, x0, lsl #3]");
        // Branch to target
        self.state.emit("    br x17");

        // Emit the jump table in .rodata
        self.state.emit(".section .rodata");
        self.state.emit_fmt(format_args!(".align 3"));
        self.state.emit_fmt(format_args!("{}:", table_label));
        for target in &table {
            let target_label = target.as_label();
            self.state.emit_fmt(format_args!("    .xword {}", target_label));
        }
        // Restore the function's text section (may be custom, e.g. .init.text)
        let sect = self.state.current_text_section.clone();
        self.state.emit_fmt(format_args!(".section {},\"ax\",@progbits", sect));

        self.state.reg_cache.invalidate_all();
    }

    fn ptr_directive(&self) -> PtrDirective { PtrDirective::Xword }
    fn function_type_directive(&self) -> &'static str { "%function" }

    fn calculate_stack_space(&mut self, func: &IrFunction) -> i64 {
        // Run register allocator BEFORE stack space computation so we can
        // skip allocating stack slots for values assigned to registers.
        // Skip variadic functions to avoid callee-save area conflicting with VA save areas.
        //
        // For functions with inline asm, pre-scan to find which callee-saved registers
        // are clobbered or used as scratch, then filter them from the allocation pool.
        // This allows register allocation to proceed using the remaining registers,
        // rather than disabling it entirely. Many kernel functions contain inline asm
        // from inlined spin_lock/spin_unlock; without this, they get no register
        // allocation and enormous stack frames (4KB+), causing kernel stack overflows.
        let mut asm_clobbered_regs: Vec<PhysReg> = Vec::new();
        Self::prescan_inline_asm_callee_saved(func, &mut asm_clobbered_regs);

        let mut available_regs = if func.is_variadic { Vec::new() } else { ARM_CALLEE_SAVED.to_vec() };
        if !asm_clobbered_regs.is_empty() {
            let clobbered_set: FxHashSet<u8> = asm_clobbered_regs.iter().map(|r| r.0).collect();
            available_regs.retain(|r| !clobbered_set.contains(&r.0));
        }
        let config = RegAllocConfig {
            available_regs,
        };
        let alloc_result = regalloc::allocate_registers(func, &config);
        self.reg_assignments = alloc_result.assignments;
        self.used_callee_saved = alloc_result.used_regs;
        let cached_liveness = alloc_result.liveness;

        // Add inline asm clobbered callee-saved registers to the save/restore list
        // (they need to be preserved per the ABI even though we don't allocate
        // values to them).
        for phys in &asm_clobbered_regs {
            if !self.used_callee_saved.iter().any(|r| r.0 == phys.0) {
                self.used_callee_saved.push(*phys);
            }
        }
        self.used_callee_saved.sort_by_key(|r| r.0);

        // Build set of register-assigned value IDs to skip stack slot allocation.
        let reg_assigned: FxHashSet<u32> = self.reg_assignments.keys().copied().collect();

        let mut space = calculate_stack_space_common(&mut self.state, func, 16, |space, alloc_size, align| {
            // ARM uses positive offsets from sp, starting at 16 (after fp/lr)
            // Honor alignment: round up slot offset to alignment boundary
            let effective_align = if align > 0 { align.max(8) } else { 8 };
            let slot = (space + effective_align - 1) & !(effective_align - 1);
            let new_space = slot + ((alloc_size + 7) & !7).max(8);
            (slot, new_space)
        }, &reg_assigned, cached_liveness);

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

            // Count named GP and FP params using the ABI classification
            // to properly account for 2-register structs and by-ref structs.
            let config = self.call_abi_config();
            let param_classes = crate::backend::call_emit::classify_params(func, &config);
            let mut named_gp = 0usize;
            let mut named_fp = 0usize;
            for class in &param_classes {
                named_gp += class.gp_reg_count();
                if matches!(class, crate::backend::call_emit::ParamClass::FloatReg { .. }
                    | crate::backend::call_emit::ParamClass::F128FpReg { .. }) {
                    named_fp += 1;
                }
            }
            self.va_named_gp_count = named_gp.min(8);
            self.va_named_fp_count = named_fp.min(8);
            // Compute total stack bytes consumed by named params (scalars, F128, structs, etc.)
            // This is needed so va_start's __stack pointer skips past all named stack args.
            self.va_named_stack_bytes = crate::backend::call_emit::named_params_stack_bytes(&param_classes);
        }

        // Reserve space for saving callee-saved registers (8 bytes each).
        let save_count = self.used_callee_saved.len() as i64;
        if save_count > 0 {
            space = (space + 7) & !7;
            self.callee_save_offset = space;
            space += save_count * 8;
        }

        space
    }

    fn aligned_frame_size(&self, raw_space: i64) -> i64 {
        (raw_space + 15) & !15
    }

    fn emit_prologue(&mut self, func: &IrFunction, frame_size: i64) {
        self.current_return_type = func.return_type;
        self.current_frame_size = frame_size;
        self.frame_base_offset = None;
        self.emit_prologue_arm(frame_size);

        // Save callee-saved registers used by register allocator.
        let used_regs = self.used_callee_saved.clone();
        let base = self.callee_save_offset;
        let n = used_regs.len();
        let mut i = 0;
        while i + 1 < n {
            let r1 = callee_saved_name(used_regs[i]);
            let r2 = callee_saved_name(used_regs[i + 1]);
            let offset = base + (i as i64) * 8;
            self.emit_stp_to_sp(r1, r2, offset);
            i += 2;
        }
        if i < n {
            let r = callee_saved_name(used_regs[i]);
            let offset = base + (i as i64) * 8;
            self.emit_store_to_sp(r, offset, "str");
        }

        // NOTE: x19 frame base register optimization is disabled due to a
        // correctness issue in SQLite's VdbeExec. The x19-relative addressing
        // generates valid offsets but causes crashes in aggregate queries.
        // TODO: investigate root cause (possibly related to x19 being modified
        // by called functions or an ABI issue with static linking).
    }

    fn emit_epilogue(&mut self, frame_size: i64) {
        self.emit_restore_callee_saved();
        self.emit_epilogue_arm(frame_size);
    }

    fn emit_store_params(&mut self, func: &IrFunction) {
        let frame_size = self.current_frame_size;

        // For variadic functions: save all register args to save areas first.
        if func.is_variadic {
            let gp_base = self.va_gp_save_offset;
            for i in (0..8).step_by(2) {
                let offset = gp_base + (i as i64) * 8;
                self.emit_stp_to_sp(&format!("x{}", i), &format!("x{}", i + 1), offset);
            }
            let fp_base = self.va_fp_save_offset;
            for i in (0..8).step_by(2) {
                let offset = fp_base + (i as i64) * 16;
                self.emit_stp_to_sp(&format!("q{}", i), &format!("q{}", i + 1), offset);
            }
        }

        // Use shared parameter classification (same ABI config as emit_call).
        let config = self.call_abi_config();
        let param_classes = classify_params(func, &config);

        // Phase 1: Store all GP register params first (before x0 gets clobbered by float moves).
        for (i, _param) in func.params.iter().enumerate() {
            let class = param_classes[i];
            if !class.uses_gp_reg() { continue; }

            let (slot, ty) = match find_param_alloca(func, i) {
                Some((dest, ty)) => match self.state.get_slot(dest.0) {
                    Some(slot) => (slot, ty),
                    None => continue,
                },
                None => continue,
            };

            match class {
                ParamClass::IntReg { reg_idx } => {
                    let store_instr = Self::str_for_type(ty);
                    let reg = Self::reg_for_type(ARM_ARG_REGS[reg_idx], ty);
                    self.emit_store_to_sp(reg, slot.0, store_instr);
                }
                ParamClass::I128RegPair { base_reg_idx } => {
                    self.emit_store_to_sp(ARM_ARG_REGS[base_reg_idx], slot.0, "str");
                    self.emit_store_to_sp(ARM_ARG_REGS[base_reg_idx + 1], slot.0 + 8, "str");
                }
                ParamClass::StructByValReg { base_reg_idx, size } => {
                    self.emit_store_to_sp(ARM_ARG_REGS[base_reg_idx], slot.0, "str");
                    if size > 8 {
                        self.emit_store_to_sp(ARM_ARG_REGS[base_reg_idx + 1], slot.0 + 8, "str");
                    }
                }
                ParamClass::LargeStructByRefReg { reg_idx, size } => {
                    // AAPCS64: register holds a pointer to the struct data.
                    // Copy size bytes from the pointer into the local alloca.
                    let src_reg = ARM_ARG_REGS[reg_idx];
                    // Copy 8-byte chunks using x9/x10 as scratch
                    let n_dwords = (size + 7) / 8;
                    for qi in 0..n_dwords {
                        let src_off = (qi * 8) as i64;
                        let dst_off = slot.0 + src_off;
                        self.emit_load_from_reg("x9", src_reg, src_off, "ldr");
                        self.emit_store_to_sp("x9", dst_off, "str");
                    }
                }
                _ => {} // Non-GP classes handled below.
            }
        }

        // Phase 2: Store FP register params.
        // Check if any F128 params arrived in FP registers and need __trunctfdf2 conversion.
        let has_f128_fp_params = param_classes.iter().enumerate().any(|(i, c)| {
            matches!(c, ParamClass::F128FpReg { .. }) &&
            find_param_alloca(func, i).is_some()
        });

        if has_f128_fp_params {
            // Save q0-q7 (128 bytes) before __trunctfdf2 clobbers them.
            self.emit_sub_sp(128);
            for i in 0..8usize {
                self.state.emit_fmt(format_args!("    str q{}, [sp, #{}]", i, i * 16));
            }

            // Process non-F128 float params first (from saved Q area).
            for (i, _param) in func.params.iter().enumerate() {
                let reg_idx = match param_classes[i] {
                    ParamClass::FloatReg { reg_idx } => reg_idx,
                    _ => continue,
                };
                let (slot, ty) = match find_param_alloca(func, i) {
                    Some((dest, ty)) => match self.state.get_slot(dest.0) {
                        Some(slot) => (slot, ty),
                        None => continue,
                    },
                    None => continue,
                };
                let fp_reg_off = (reg_idx * 16) as i64;
                if ty == IrType::F32 {
                    self.state.emit_fmt(format_args!("    ldr s0, [sp, #{}]", fp_reg_off));
                    self.state.emit("    fmov w0, s0");
                } else {
                    self.state.emit_fmt(format_args!("    ldr d0, [sp, #{}]", fp_reg_off));
                    self.state.emit("    fmov x0, d0");
                }
                self.emit_store_to_sp("x0", slot.0 + 128, "str");
            }

            // Process F128 FP reg params: load from save area, call __trunctfdf2.
            for (i, _param) in func.params.iter().enumerate() {
                let reg_idx = match param_classes[i] {
                    ParamClass::F128FpReg { reg_idx } => reg_idx,
                    _ => continue,
                };
                let slot = match find_param_alloca(func, i) {
                    Some((dest, _)) => match self.state.get_slot(dest.0) {
                        Some(slot) => slot,
                        None => continue,
                    },
                    None => continue,
                };
                let fp_reg_off = (reg_idx * 16) as i64;
                self.state.emit_fmt(format_args!("    ldr q0, [sp, #{}]", fp_reg_off));
                self.state.emit("    bl __trunctfdf2");
                self.state.emit("    fmov x0, d0");
                self.emit_store_to_sp("x0", slot.0 + 128, "str");
            }

            self.emit_add_sp(128);
        } else {
            // No F128 FP params: simple path.
            for (i, _param) in func.params.iter().enumerate() {
                let reg_idx = match param_classes[i] {
                    ParamClass::FloatReg { reg_idx } => reg_idx,
                    _ => continue,
                };
                let (slot, ty) = match find_param_alloca(func, i) {
                    Some((dest, ty)) => match self.state.get_slot(dest.0) {
                        Some(slot) => (slot, ty),
                        None => continue,
                    },
                    None => continue,
                };
                if ty == IrType::F32 {
                    self.state.emit_fmt(format_args!("    fmov w0, s{}", reg_idx));
                } else {
                    self.state.emit_fmt(format_args!("    fmov x0, d{}", reg_idx));
                }
                self.emit_store_to_sp("x0", slot.0, "str");
            }
        }

        // Phase 3: Store stack-passed params (above callee's frame).
        for (i, _param) in func.params.iter().enumerate() {
            let class = param_classes[i];
            if !class.is_stack() { continue; }

            let (slot, ty) = match find_param_alloca(func, i) {
                Some((dest, ty)) => match self.state.get_slot(dest.0) {
                    Some(slot) => (slot, ty),
                    None => continue,
                },
                None => continue,
            };

            match class {
                ParamClass::StructStack { offset, size } | ParamClass::LargeStructStack { offset, size } => {
                    let caller_offset = frame_size + offset;
                    let n_dwords = (size + 7) / 8;
                    for qi in 0..n_dwords {
                        let src_off = caller_offset + (qi as i64 * 8);
                        let dst_off = slot.0 + (qi as i64 * 8);
                        self.emit_load_from_sp("x0", src_off, "ldr");
                        self.emit_store_to_sp("x0", dst_off, "str");
                    }
                }
                ParamClass::F128Stack { offset } => {
                    let caller_offset = frame_size + offset;
                    self.emit_load_from_sp("x0", caller_offset, "ldr");
                    self.emit_load_from_sp("x1", caller_offset + 8, "ldr");
                    self.state.emit("    fmov d0, x0");
                    self.state.emit("    mov v0.d[1], x1");
                    self.state.emit("    bl __trunctfdf2");
                    self.state.emit("    fmov x0, d0");
                    self.emit_store_to_sp("x0", slot.0, "str");
                }
                ParamClass::I128Stack { offset } => {
                    let caller_offset = frame_size + offset;
                    self.emit_load_from_sp("x0", caller_offset, "ldr");
                    self.emit_store_to_sp("x0", slot.0, "str");
                    self.emit_load_from_sp("x0", caller_offset + 8, "ldr");
                    self.emit_store_to_sp("x0", slot.0 + 8, "str");
                }
                ParamClass::StackScalar { offset } => {
                    let caller_offset = frame_size + offset;
                    self.emit_load_from_sp("x0", caller_offset, "ldr");
                    let store_instr = Self::str_for_type(ty);
                    let reg = Self::reg_for_type("x0", ty);
                    self.emit_store_to_sp(reg, slot.0, store_instr);
                }
                ParamClass::LargeStructByRefStack { offset, size } => {
                    // AAPCS64: stack slot holds a pointer to the struct data.
                    // Load the pointer, then copy the struct data into the alloca.
                    let caller_offset = frame_size + offset;
                    self.emit_load_from_sp("x0", caller_offset, "ldr");
                    let n_dwords = (size + 7) / 8;
                    for qi in 0..n_dwords {
                        let src_off = (qi * 8) as i64;
                        let dst_off = slot.0 + src_off;
                        self.emit_load_from_reg("x1", "x0", src_off, "ldr");
                        self.emit_store_to_sp("x1", dst_off, "str");
                    }
                }
                _ => {} // Non-stack classes already handled.
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

    fn emit_sign_extend_acc_high(&mut self) {
        self.state.emit("    asr x1, x0, #63");
    }

    fn emit_zero_acc_high(&mut self) {
        self.state.emit("    mov x1, #0");
    }

    fn current_return_type(&self) -> IrType {
        self.current_return_type
    }

    fn emit_return_i128_to_regs(&mut self) {
        // x0:x1 already hold the i128 return value per AAPCS64 — noop
    }

    fn emit_return_f128_to_reg(&mut self) {
        // F128 return: convert f64 bit pattern in x0 to f128 in q0 via __extenddftf2.
        self.state.emit("    fmov d0, x0");
        self.state.emit("    bl __extenddftf2");
    }

    fn emit_return_f32_to_reg(&mut self) {
        self.state.emit("    fmov s0, w0");
    }

    fn emit_return_f64_to_reg(&mut self) {
        self.state.emit("    fmov d0, x0");
    }

    fn emit_return_int_to_reg(&mut self) {
        // x0 already holds the return value per AAPCS64 — noop
    }

    fn emit_epilogue_and_ret(&mut self, frame_size: i64) {
        self.emit_restore_callee_saved();
        self.emit_epilogue_arm(frame_size);
        self.state.emit("    ret");
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

    fn emit_load_ptr_from_slot(&mut self, slot: StackSlot, val_id: u32) {
        if let Some(&reg) = self.reg_assignments.get(&val_id) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mov x9, {}", reg_name));
        } else {
            self.emit_load_from_sp("x9", slot.0, "ldr");
        }
    }

    fn emit_typed_store_indirect(&mut self, instr: &'static str, ty: IrType) {
        let reg = Self::reg_for_type("x1", ty);
        self.state.emit_fmt(format_args!("    {} {}, [x9]", instr, reg));
    }

    fn emit_typed_load_indirect(&mut self, instr: &'static str) {
        let (actual_instr, dest_reg) = Self::arm_parse_load(instr);
        self.state.emit_fmt(format_args!("    {} {}, [x9]", actual_instr, dest_reg));
    }

    fn emit_add_offset_to_addr_reg(&mut self, offset: i64) {
        if offset >= 0 && offset <= 4095 {
            self.state.emit_fmt(format_args!("    add x9, x9, #{}", offset));
        } else if offset < 0 && (-offset) <= 4095 {
            self.state.emit_fmt(format_args!("    sub x9, x9, #{}", -offset));
        } else {
            self.load_large_imm("x17", offset);
            self.state.emit("    add x9, x9, x17");
        }
    }

    fn emit_slot_addr_to_secondary(&mut self, slot: StackSlot, is_alloca: bool, val_id: u32) {
        if is_alloca {
            self.emit_add_sp_offset("x1", slot.0);
        } else if let Some(&reg) = self.reg_assignments.get(&val_id) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mov x1, {}", reg_name));
        } else {
            self.emit_load_from_sp("x1", slot.0, "ldr");
        }
    }

    fn emit_add_secondary_to_acc(&mut self) {
        self.state.emit("    add x0, x1, x0");
    }

    /// Optimized GEP for alloca base + constant offset.
    /// Computes sp/x29 + (slot_offset + gep_offset) directly into x0 (accumulator).
    fn emit_gep_direct_const(&mut self, slot: StackSlot, offset: i64) {
        let folded = slot.0 + offset;
        // Reuse emit_add_sp_offset which handles large offsets via x19 frame base or x17 scratch
        self.emit_add_sp_offset("x0", folded);
    }

    /// Optimized GEP for pointer-in-slot + constant offset.
    /// Loads the pointer, then adds the constant offset using immediate add when possible.
    fn emit_gep_indirect_const(&mut self, slot: StackSlot, offset: i64, val_id: u32) {
        // Load the base pointer into x0
        if let Some(&reg) = self.reg_assignments.get(&val_id) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mov x0, {}", reg_name));
        } else {
            self.emit_load_from_sp("x0", slot.0, "ldr");
        }
        // Add constant offset using immediate add (skip if zero)
        if offset != 0 {
            self.emit_add_imm_to_acc(offset);
        }
    }

    /// Add a constant offset to the accumulator (x0) using immediate add.
    fn emit_gep_add_const_to_acc(&mut self, offset: i64) {
        if offset != 0 {
            self.emit_add_imm_to_acc(offset);
        }
    }

    fn emit_add_imm_to_acc(&mut self, imm: i64) {
        if imm >= 0 && imm <= 4095 {
            self.state.emit_fmt(format_args!("    add x0, x0, #{}", imm));
        } else if imm < 0 && (-imm) <= 4095 {
            self.state.emit_fmt(format_args!("    sub x0, x0, #{}", -imm));
        } else {
            // Large immediate: load into x1 (secondary), then add
            self.emit_load_imm64("x1", imm);
            self.state.emit("    add x0, x0, x1");
        }
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

    fn emit_mov_acc_to_sp(&mut self) {
        self.state.emit("    mov sp, x0");
    }

    fn emit_align_acc(&mut self, align: usize) {
        self.state.emit_fmt(format_args!("    add x0, x0, #{}", align - 1));
        self.state.emit_fmt(format_args!("    and x0, x0, #{}", -(align as i64)));
    }

    fn emit_memcpy_load_dest_addr(&mut self, slot: StackSlot, is_alloca: bool, val_id: u32) {
        if is_alloca {
            self.emit_add_sp_offset("x9", slot.0);
        } else if let Some(&reg) = self.reg_assignments.get(&val_id) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mov x9, {}", reg_name));
        } else {
            self.emit_load_from_sp("x9", slot.0, "ldr");
        }
    }

    fn emit_memcpy_load_src_addr(&mut self, slot: StackSlot, is_alloca: bool, val_id: u32) {
        if is_alloca {
            self.emit_add_sp_offset("x10", slot.0);
        } else if let Some(&reg) = self.reg_assignments.get(&val_id) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mov x10, {}", reg_name));
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
        // x0 now holds an aligned address, not any previous SSA value.
        self.state.reg_cache.invalidate_acc();
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
        self.state.emit_fmt(format_args!("{}:", loop_label));
        self.state.emit_fmt(format_args!("    cbz x11, {}", done_label));
        self.state.emit("    ldrb w12, [x10], #1");
        self.state.emit("    strb w12, [x9], #1");
        self.state.emit("    sub x11, x11, #1");
        self.state.emit_fmt(format_args!("    b {}", loop_label));
        self.state.emit_fmt(format_args!("{}:", done_label));
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

    fn emit_int_popcount(&mut self, ty: IrType) {
        // For 32-bit types, use fmov s0, w0 to zero-extend to 32 bits only,
        // preventing sign-extended upper 32 bits from being counted.
        // For 64-bit types, use fmov d0, x0 to count all 64 bits.
        if ty == IrType::I32 || ty == IrType::U32 {
            self.state.emit("    fmov s0, w0");
        } else {
            self.state.emit("    fmov d0, x0");
        }
        self.state.emit("    cnt v0.8b, v0.8b");
        self.state.emit("    uaddlv h0, v0.8b");
        self.state.emit("    fmov w0, s0");
    }

    // emit_float_binop uses the shared default implementation

    fn emit_copy_value(&mut self, dest: &Value, src: &Operand) {
        let dest_phys = self.dest_reg(dest);
        let src_phys = self.operand_reg(src);

        match (dest_phys, src_phys) {
            (Some(d), Some(s)) => {
                if d.0 != s.0 {
                    let d_name = callee_saved_name(d);
                    let s_name = callee_saved_name(s);
                    self.state.emit_fmt(format_args!("    mov {}, {}", d_name, s_name));
                }
                self.state.reg_cache.invalidate_acc();
            }
            (Some(d), None) => {
                self.operand_to_x0(src);
                let d_name = callee_saved_name(d);
                self.state.emit_fmt(format_args!("    mov {}, x0", d_name));
                self.state.reg_cache.invalidate_acc();
            }
            _ => {
                self.operand_to_x0(src);
                self.store_x0_to(dest);
            }
        }
    }

    fn emit_int_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        // Note: i128 dispatch is handled by the shared emit_binop default in traits.rs.
        let use_32bit = ty == IrType::I32 || ty == IrType::U32;
        let is_unsigned = ty.is_unsigned();

        // Strength reduction: UDiv/URem by power-of-2 constant → shift/mask.
        if let Some(shift) = Self::const_as_power_of_2(rhs) {
            if op == IrBinOp::UDiv {
                self.emit_load_operand(lhs);
                if use_32bit {
                    self.state.emit_fmt(format_args!("    lsr w0, w0, #{}", shift));
                } else {
                    self.state.emit_fmt(format_args!("    lsr x0, x0, #{}", shift));
                }
                self.store_x0_to(dest);
                return;
            }
            if op == IrBinOp::URem {
                self.emit_load_operand(lhs);
                let mask = (1u64 << shift) - 1;
                if use_32bit {
                    self.state.emit_fmt(format_args!("    and w0, w0, #{}", mask));
                } else {
                    self.state.emit_fmt(format_args!("    and x0, x0, #{}", mask));
                }
                self.store_x0_to(dest);
                return;
            }
        }

        // Register-direct path: use ARM 3-operand instructions with callee-saved dest.
        if let Some(dest_phys) = self.dest_reg(dest) {
            let dest_name = callee_saved_name(dest_phys);
            let dest_name_32 = callee_saved_name_32(dest_phys);

            let is_simple_alu = matches!(op, IrBinOp::Add | IrBinOp::Sub | IrBinOp::And
                | IrBinOp::Or | IrBinOp::Xor | IrBinOp::Mul);
            if is_simple_alu {
                let mnemonic = arm_alu_mnemonic(op);

                // Immediate form for add/sub: op Xd, Xn, #imm12
                if matches!(op, IrBinOp::Add | IrBinOp::Sub) {
                    if let Some(imm) = Self::const_as_imm12(rhs) {
                        self.operand_to_callee_reg(lhs, dest_phys);
                        if use_32bit {
                            self.state.emit_fmt(format_args!("    {} {}, {}, #{}", mnemonic, dest_name_32, dest_name_32, imm));
                            if !is_unsigned { self.state.emit_fmt(format_args!("    sxtw {}, {}", dest_name, dest_name_32)); }
                        } else {
                            self.state.emit_fmt(format_args!("    {} {}, {}, #{}", mnemonic, dest_name, dest_name, imm));
                        }
                        self.state.reg_cache.invalidate_acc();
                        return;
                    }
                }

                // Register-register form: op Xd, Xn, Xm
                let rhs_phys = self.operand_reg(rhs);
                let rhs_conflicts = rhs_phys.map_or(false, |r| r.0 == dest_phys.0);
                let rhs_reg: String = if rhs_conflicts {
                    self.operand_to_x0(rhs);
                    self.operand_to_callee_reg(lhs, dest_phys);
                    "x0".to_string()
                } else {
                    self.operand_to_callee_reg(lhs, dest_phys);
                    if let Some(rhs_phys) = rhs_phys {
                        callee_saved_name(rhs_phys).to_string()
                    } else {
                        self.operand_to_x0(rhs);
                        "x0".to_string()
                    }
                };
                let rhs_32: String = if rhs_reg == "x0" { "w0".to_string() }
                    else { rhs_reg.replace('x', "w") };

                if use_32bit {
                    self.state.emit_fmt(format_args!("    {} {}, {}, {}", mnemonic, dest_name_32, dest_name_32, rhs_32));
                    if !is_unsigned { self.state.emit_fmt(format_args!("    sxtw {}, {}", dest_name, dest_name_32)); }
                } else {
                    self.state.emit_fmt(format_args!("    {} {}, {}, {}", mnemonic, dest_name, dest_name, rhs_reg));
                }
                self.state.reg_cache.invalidate_acc();
                return;
            }
        }

        // Fallback: accumulator path
        self.operand_to_x0(lhs);
        self.state.emit("    mov x1, x0");
        self.operand_to_x0(rhs);
        self.state.emit("    mov x2, x0");

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
            // Use shared i128 cmp dispatch
            ArchCodegen::emit_i128_cmp(self, dest, op, lhs, rhs);
            return;
        }
        if ty == IrType::F128 {
            // F128 comparison via soft-float libcalls.
            // AArch64 has no hardware quad-precision; we must call __eqtf2/__letf2/etc.
            self.emit_f128_cmp(dest, op, lhs, rhs);
            return;
        }
        if ty.is_float() {
            self.operand_to_x0(lhs);
            self.state.emit("    mov x1, x0");
            self.operand_to_x0(rhs);
            if ty == IrType::F32 {
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
            self.state.emit_fmt(format_args!("    cset x0, {}", cond));
            self.store_x0_to(dest);
            return;
        }

        self.emit_int_cmp_insn(lhs, rhs, ty);

        let cond = arm_int_cond_code(op);
        self.state.emit_fmt(format_args!("    cset x0, {}", cond));
        self.store_x0_to(dest);
    }

    /// Fused compare-and-branch for ARM: emit cmp + relaxed conditional branch.
    /// Uses inverted condition to skip over the true-branch jump, giving the
    /// true-branch target +-128MB range instead of the +-1MB limit of b.cond.
    fn emit_fused_cmp_branch(
        &mut self,
        op: IrCmpOp,
        lhs: &Operand,
        rhs: &Operand,
        ty: IrType,
        true_label: &str,
        false_label: &str,
    ) {
        self.emit_int_cmp_insn(lhs, rhs, ty);

        let cc = arm_int_cond_code(op);
        let inv_cc = arm_invert_cond_code(cc);
        // b.inv_cc .Lskip  (short forward skip, always in range)
        // b true_label      (+-128MB range)
        // .Lskip:
        // b false_label     (+-128MB range)
        let skip = self.state.fresh_label("skip");
        self.state.emit_fmt(format_args!("    b.{} {}", inv_cc, skip));
        self.state.emit_fmt(format_args!("    b {}", true_label));
        self.state.emit_fmt(format_args!("{}:", skip));
        self.state.emit_fmt(format_args!("    b {}", false_label));
        self.state.reg_cache.invalidate_all();
    }

    /// Emit conditional select using AArch64 csel instruction.
    ///
    /// Strategy:
    ///   1. Load false_val into x0
    ///   2. Save to x1
    ///   3. Load true_val into x0
    ///   4. Save to x2
    ///   5. Load condition into x0, compare against zero
    ///   6. csel x0, x2, x1, ne (select true_val if cond != 0, else false_val)
    ///   7. Store x0 to dest
    fn emit_select(&mut self, dest: &Value, cond: &Operand, true_val: &Operand, false_val: &Operand, _ty: IrType) {
        // Load false_val into x1
        self.operand_to_x0(false_val);
        self.state.emit("    mov x1, x0");

        // Load true_val into x2
        self.operand_to_x0(true_val);
        self.state.emit("    mov x2, x0");

        // Load condition and compare against zero
        self.operand_to_x0(cond);
        self.state.emit("    cmp x0, #0");

        // csel: select x2 (true_val) if ne, else x1 (false_val)
        self.state.emit("    csel x0, x2, x1, ne");

        // Store result
        self.state.reg_cache.invalidate_acc();
        self.store_x0_to(dest);
    }

    // emit_call: uses shared default from ArchCodegen trait (traits.rs)

    fn call_abi_config(&self) -> CallAbiConfig {
        CallAbiConfig {
            max_int_regs: 8, max_float_regs: 8,
            align_i128_pairs: true,
            f128_in_fp_regs: true, f128_in_gp_pairs: false,
            variadic_floats_in_gp: false,
            large_struct_by_ref: true, // AAPCS64: composites > 16 bytes passed by reference
            use_sysv_struct_classification: false, // ARM uses AAPCS64, not SysV
        }
    }

    fn emit_call_compute_stack_space(&self, arg_classes: &[CallArgClass]) -> usize {
        compute_stack_arg_space(arg_classes)
    }

    fn emit_call_spill_fptr(&mut self, func_ptr: &Operand) {
        self.operand_to_x0(func_ptr);
        self.state.emit("    str x0, [sp, #-16]!");
    }

    fn emit_call_fptr_spill_size(&self) -> usize { 16 }

    fn emit_call_f128_pre_convert(&mut self, args: &[Operand], arg_classes: &[CallArgClass],
                                   _arg_types: &[IrType], _stack_arg_space: usize) -> usize {
        // ARM F128 pre-conversion is handled inside emit_call_reg_args for this arch
        // because it's interleaved with FP register loading.
        let _ = (args, arg_classes);
        0
    }

    fn emit_call_stack_args(&mut self, args: &[Operand], arg_classes: &[CallArgClass],
                            _arg_types: &[IrType], stack_arg_space: usize, fptr_spill: usize, _f128_temp_space: usize) -> i64 {
        if stack_arg_space > 0 {
            self.emit_sub_sp(stack_arg_space as i64);
            // When has_dyn_alloca, emit_load_from_sp/emit_add_sp_offset use x29 (fixed frame
            // pointer) as base — frame slot offsets don't need SP adjustment.
            let src_adjust = if self.state.has_dyn_alloca { 0 } else { stack_arg_space as i64 + fptr_spill as i64 };
            let mut stack_offset = 0i64;
            for (arg_idx, arg) in args.iter().enumerate() {
                if !arg_classes[arg_idx].is_stack() { continue; }
                let cls = arg_classes[arg_idx];
                if matches!(cls, CallArgClass::F128Stack | CallArgClass::I128Stack) {
                    stack_offset = (stack_offset + 15) & !15;
                }
                match cls {
                    CallArgClass::StructByValStack { size } | CallArgClass::LargeStructStack { size } => {
                        let n_dwords = (size + 7) / 8;
                        match arg {
                            Operand::Value(v) => {
                                if let Some(&reg) = self.reg_assignments.get(&v.0) {
                                    let reg_name = callee_saved_name(reg);
                                    self.state.emit_fmt(format_args!("    mov x0, {}", reg_name));
                                } else if let Some(slot) = self.state.get_slot(v.0) {
                                    let adjusted = slot.0 + src_adjust;
                                    if self.state.is_alloca(v.0) {
                                        self.emit_add_sp_offset("x0", adjusted);
                                    } else {
                                        self.emit_load_from_sp("x0", adjusted, "ldr");
                                    }
                                } else {
                                    self.state.emit("    mov x0, #0");
                                }
                            }
                            Operand::Const(_) => { self.operand_to_x0(arg); }
                        }
                        for qi in 0..n_dwords {
                            let src_off = (qi * 8) as i64;
                            self.emit_load_from_reg("x1", "x0", src_off, "ldr");
                            // Stack arg destinations are at current sp, use raw sp.
                            self.emit_store_to_raw_sp("x1", stack_offset + src_off, "str");
                        }
                        stack_offset += (n_dwords as i64) * 8;
                    }
                    CallArgClass::I128Stack => {
                        match arg {
                            Operand::Const(c) => {
                                if let IrConst::I128(v) = c {
                                    self.emit_load_imm64("x0", *v as u64 as i64);
                                    self.emit_store_to_raw_sp("x0", stack_offset, "str");
                                    self.emit_load_imm64("x0", (*v >> 64) as u64 as i64);
                                    self.emit_store_to_raw_sp("x0", stack_offset + 8, "str");
                                } else {
                                    self.operand_to_x0(arg);
                                    self.emit_store_to_raw_sp("x0", stack_offset, "str");
                                    self.state.emit("    mov x0, #0");
                                    self.emit_store_to_raw_sp("x0", stack_offset + 8, "str");
                                }
                            }
                            Operand::Value(v) => {
                                if let Some(slot) = self.state.get_slot(v.0) {
                                    let adjusted = slot.0 + src_adjust;
                                    if self.state.is_alloca(v.0) {
                                        self.emit_add_sp_offset("x0", adjusted);
                                        self.emit_store_to_raw_sp("x0", stack_offset, "str");
                                        self.state.emit("    mov x0, #0");
                                        self.emit_store_to_raw_sp("x0", stack_offset + 8, "str");
                                    } else {
                                        self.emit_load_from_sp("x0", adjusted, "ldr");
                                        self.emit_store_to_raw_sp("x0", stack_offset, "str");
                                        self.emit_load_from_sp("x0", adjusted + 8, "ldr");
                                        self.emit_store_to_raw_sp("x0", stack_offset + 8, "str");
                                    }
                                } else {
                                    self.state.emit("    mov x0, #0");
                                    self.emit_store_to_raw_sp("x0", stack_offset, "str");
                                    self.emit_store_to_raw_sp("x0", stack_offset + 8, "str");
                                }
                            }
                        }
                        stack_offset += 16;
                    }
                    CallArgClass::F128Stack => {
                        match arg {
                            Operand::Const(c) => {
                                let f64_val = match c {
                                    IrConst::LongDouble(v, _) => *v,
                                    IrConst::F64(v) => *v,
                                    _ => c.to_f64().unwrap_or(0.0),
                                };
                                let bytes = crate::ir::ir::f64_to_f128_bytes(f64_val);
                                let lo = u64::from_le_bytes(bytes[0..8].try_into().unwrap());
                                let hi = u64::from_le_bytes(bytes[8..16].try_into().unwrap());
                                self.emit_load_imm64("x0", lo as i64);
                                self.emit_store_to_raw_sp("x0", stack_offset, "str");
                                self.emit_load_imm64("x0", hi as i64);
                                self.emit_store_to_raw_sp("x0", stack_offset + 8, "str");
                            }
                            Operand::Value(v) => {
                                if let Some(&reg) = self.reg_assignments.get(&v.0) {
                                    let reg_name = callee_saved_name(reg);
                                    self.state.emit_fmt(format_args!("    mov x0, {}", reg_name));
                                } else if let Some(slot) = self.state.get_slot(v.0) {
                                    let adjusted = slot.0 + src_adjust;
                                    if self.state.is_alloca(v.0) {
                                        self.emit_add_sp_offset("x0", adjusted);
                                    } else {
                                        self.emit_load_from_sp("x0", adjusted, "ldr");
                                    }
                                } else {
                                    self.state.emit("    mov x0, #0");
                                }
                                self.state.emit("    fmov d0, x0");
                                self.state.emit("    stp x9, x10, [sp, #-16]!");
                                self.state.emit("    bl __extenddftf2");
                                self.state.emit("    ldp x9, x10, [sp], #16");
                                self.emit_store_to_raw_sp("q0", stack_offset, "str");
                            }
                        }
                        stack_offset += 16;
                    }
                    CallArgClass::Stack => {
                        match arg {
                            Operand::Value(v) => {
                                if let Some(&reg) = self.reg_assignments.get(&v.0) {
                                    let reg_name = callee_saved_name(reg);
                                    self.state.emit_fmt(format_args!("    mov x0, {}", reg_name));
                                } else if let Some(slot) = self.state.get_slot(v.0) {
                                    let adjusted = slot.0 + src_adjust;
                                    if self.state.is_alloca(v.0) {
                                        self.emit_add_sp_offset("x0", adjusted);
                                    } else {
                                        self.emit_load_from_sp("x0", adjusted, "ldr");
                                    }
                                } else {
                                    self.state.emit("    mov x0, #0");
                                }
                            }
                            Operand::Const(_) => { self.operand_to_x0(arg); }
                        }
                        self.emit_store_to_raw_sp("x0", stack_offset, "str");
                        stack_offset += 8;
                    }
                    _ => {}
                }
            }
        }
        stack_arg_space as i64 + fptr_spill as i64
    }

    fn emit_call_reg_args(&mut self, args: &[Operand], arg_classes: &[CallArgClass],
                          arg_types: &[IrType], total_sp_adjust: i64, _f128_temp_space: usize, _stack_arg_space: usize) {
        // When has_dyn_alloca is true, emit_load_from_sp/emit_add_sp_offset use x29 as base.
        // Frame slots are at fixed x29-relative offsets, so total_sp_adjust (which accounts
        // for sp movement from fptr spill + stack args) must NOT be added.
        let slot_adjust = if self.state.has_dyn_alloca { 0 } else { total_sp_adjust };

        // Phase 2a: Load GP integer register args into temp registers (x9-x16).
        let mut gp_tmp_idx = 0usize;
        for (i, arg) in args.iter().enumerate() {
            if !matches!(arg_classes[i], CallArgClass::IntReg { .. }) { continue; }
            if gp_tmp_idx >= 8 { break; }
            if total_sp_adjust > 0 {
                match arg {
                    Operand::Value(v) => {
                        if let Some(&reg) = self.reg_assignments.get(&v.0) {
                            let reg_name = callee_saved_name(reg);
                            self.state.emit_fmt(format_args!("    mov x0, {}", reg_name));
                        } else if let Some(slot) = self.state.get_slot(v.0) {
                            let adjusted = slot.0 + slot_adjust;
                            if self.state.is_alloca(v.0) {
                                self.emit_add_sp_offset("x0", adjusted);
                            } else {
                                self.emit_load_from_sp("x0", adjusted, "ldr");
                            }
                        } else {
                            self.state.emit("    mov x0, #0");
                        }
                    }
                    Operand::Const(_) => { self.operand_to_x0(arg); }
                }
            } else {
                self.operand_to_x0(arg);
            }
            self.state.emit_fmt(format_args!("    mov {}, x0", ARM_TMP_REGS[gp_tmp_idx]));
            gp_tmp_idx += 1;
        }

        // Phase 2b: Load FP register args (F128 needs __extenddftf2 which clobbers FP regs).
        let fp_reg_assignments: Vec<(usize, usize)> = args.iter().enumerate()
            .filter(|(i, _)| matches!(arg_classes[*i], CallArgClass::FloatReg { .. } | CallArgClass::F128Reg { .. }))
            .map(|(i, _)| {
                let reg_idx = match arg_classes[i] {
                    CallArgClass::FloatReg { reg_idx } | CallArgClass::F128Reg { reg_idx } => reg_idx,
                    _ => 0,
                };
                (i, reg_idx)
            })
            .collect();

        let f128_var_count: usize = fp_reg_assignments.iter()
            .filter(|&&(arg_i, _)| matches!(arg_classes[arg_i], CallArgClass::F128Reg { .. }) && matches!(&args[arg_i], Operand::Value(_)))
            .count();
        let f128_temp_space_aligned = (f128_var_count * 16 + 15) & !15;
        if f128_temp_space_aligned > 0 {
            self.emit_sub_sp(f128_temp_space_aligned as i64);
        }

        // Convert F128 variable args via __extenddftf2, save to temp stack
        let mut f128_temp_idx = 0usize;
        let mut f128_temp_slots: Vec<(usize, usize)> = Vec::new();
        for &(arg_i, reg_i) in &fp_reg_assignments {
            if !matches!(arg_classes[arg_i], CallArgClass::F128Reg { .. }) { continue; }
            if let Operand::Value(v) = &args[arg_i] {
                if total_sp_adjust > 0 || f128_temp_space_aligned > 0 {
                    if let Some(&reg) = self.reg_assignments.get(&v.0) {
                        let reg_name = callee_saved_name(reg);
                        self.state.emit_fmt(format_args!("    mov x0, {}", reg_name));
                    } else if let Some(slot) = self.state.get_slot(v.0) {
                        let adjusted = slot.0 + slot_adjust + f128_temp_space_aligned as i64;
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
                let temp_off = f128_temp_idx * 16;
                self.state.emit_fmt(format_args!("    str q0, [sp, #{}]", temp_off));
                f128_temp_slots.push((reg_i, temp_off));
                f128_temp_idx += 1;
            }
        }

        // Load F128 constants directly into target Q registers
        for &(arg_i, reg_i) in &fp_reg_assignments {
            if !matches!(arg_classes[arg_i], CallArgClass::F128Reg { .. }) { continue; }
            if let Operand::Const(c) = &args[arg_i] {
                let f64_val = match c {
                    IrConst::LongDouble(v, _) => *v,
                    IrConst::F64(v) => *v,
                    _ => c.to_f64().unwrap_or(0.0),
                };
                let bytes = crate::ir::ir::f64_to_f128_bytes(f64_val);
                let lo = u64::from_le_bytes(bytes[0..8].try_into().unwrap());
                let hi = u64::from_le_bytes(bytes[8..16].try_into().unwrap());
                self.emit_load_imm64("x0", lo as i64);
                self.emit_load_imm64("x1", hi as i64);
                self.state.emit("    stp x0, x1, [sp, #-16]!");
                self.state.emit_fmt(format_args!("    ldr q{}, [sp]", reg_i));
                self.state.emit("    add sp, sp, #16");
            }
        }

        // Restore F128 variable results from temp stack
        for &(reg_i, temp_off) in &f128_temp_slots {
            self.state.emit_fmt(format_args!("    ldr q{}, [sp, #{}]", reg_i, temp_off));
        }
        if f128_temp_space_aligned > 0 {
            self.emit_add_sp(f128_temp_space_aligned as i64);
        }

        // Load non-F128 FP args last (after __extenddftf2 clobbers)
        for &(arg_i, reg_i) in &fp_reg_assignments {
            if matches!(arg_classes[arg_i], CallArgClass::F128Reg { .. }) { continue; }
            let arg_ty = if arg_i < arg_types.len() { Some(arg_types[arg_i]) } else { None };
            if total_sp_adjust > 0 {
                match &args[arg_i] {
                    Operand::Value(v) => {
                        if let Some(&reg) = self.reg_assignments.get(&v.0) {
                            let reg_name = callee_saved_name(reg);
                            self.state.emit_fmt(format_args!("    mov x0, {}", reg_name));
                        } else if let Some(slot) = self.state.get_slot(v.0) {
                            self.emit_load_from_sp("x0", slot.0 + slot_adjust, "ldr");
                        } else {
                            self.state.emit("    mov x0, #0");
                        }
                    }
                    Operand::Const(_) => { self.operand_to_x0(&args[arg_i]); }
                }
            } else {
                self.operand_to_x0(&args[arg_i]);
            }
            if arg_ty == Some(IrType::F32) {
                self.state.emit_fmt(format_args!("    fmov s{}, w0", reg_i));
            } else {
                self.state.emit_fmt(format_args!("    fmov d{}, x0", reg_i));
            }
        }

        // Phase 3: Move GP int args from temp regs to actual arg registers.
        let mut int_reg_idx = 0usize;
        gp_tmp_idx = 0;
        for (i, _) in args.iter().enumerate() {
            match arg_classes[i] {
                CallArgClass::I128RegPair { .. } => {
                    if int_reg_idx % 2 != 0 { int_reg_idx += 1; }
                    int_reg_idx += 2;
                }
                CallArgClass::StructByValReg { size, .. } => {
                    int_reg_idx += if size <= 8 { 1 } else { 2 };
                }
                CallArgClass::IntReg { .. } => {
                    if gp_tmp_idx < 8 && int_reg_idx < 8 {
                        self.state.emit_fmt(format_args!("    mov {}, {}", ARM_ARG_REGS[int_reg_idx], ARM_TMP_REGS[gp_tmp_idx]));
                        int_reg_idx += 1;
                    }
                    gp_tmp_idx += 1;
                }
                _ => {}
            }
        }

        // Phase 3b: Load i128 register pair args.
        for (i, arg) in args.iter().enumerate() {
            if let CallArgClass::I128RegPair { base_reg_idx } = arg_classes[i] {
                if total_sp_adjust > 0 {
                    match arg {
                        Operand::Value(v) => {
                            if let Some(slot) = self.state.get_slot(v.0) {
                                let adjusted = slot.0 + slot_adjust;
                                if self.state.is_alloca(v.0) {
                                    self.emit_load_from_sp(ARM_ARG_REGS[base_reg_idx], adjusted, "ldr");
                                    self.state.emit_fmt(format_args!("    mov {}, #0", ARM_ARG_REGS[base_reg_idx + 1]));
                                } else {
                                    self.emit_load_from_sp(ARM_ARG_REGS[base_reg_idx], adjusted, "ldr");
                                    self.emit_load_from_sp(ARM_ARG_REGS[base_reg_idx + 1], adjusted + 8, "ldr");
                                }
                            }
                        }
                        Operand::Const(c) => {
                            if let IrConst::I128(v) = c {
                                self.emit_load_imm64(ARM_ARG_REGS[base_reg_idx], *v as u64 as i64);
                                self.emit_load_imm64(ARM_ARG_REGS[base_reg_idx + 1], (*v >> 64) as u64 as i64);
                            } else {
                                self.operand_to_x0(arg);
                                if base_reg_idx != 0 {
                                    self.state.emit_fmt(format_args!("    mov {}, x0", ARM_ARG_REGS[base_reg_idx]));
                                }
                                self.state.emit_fmt(format_args!("    mov {}, #0", ARM_ARG_REGS[base_reg_idx + 1]));
                            }
                        }
                    }
                } else {
                    match arg {
                        Operand::Value(v) => {
                            if let Some(slot) = self.state.get_slot(v.0) {
                                if self.state.is_alloca(v.0) {
                                    self.emit_add_sp_offset(ARM_ARG_REGS[base_reg_idx], slot.0);
                                    self.state.emit_fmt(format_args!("    mov {}, #0", ARM_ARG_REGS[base_reg_idx + 1]));
                                } else {
                                    self.emit_load_from_sp(ARM_ARG_REGS[base_reg_idx], slot.0, "ldr");
                                    self.emit_load_from_sp(ARM_ARG_REGS[base_reg_idx + 1], slot.0 + 8, "ldr");
                                }
                            }
                        }
                        Operand::Const(c) => {
                            if let IrConst::I128(v) = c {
                                self.emit_load_imm64(ARM_ARG_REGS[base_reg_idx], *v as u64 as i64);
                                self.emit_load_imm64(ARM_ARG_REGS[base_reg_idx + 1], (*v >> 64) as u64 as i64);
                            } else {
                                self.operand_to_x0(arg);
                                if base_reg_idx != 0 {
                                    self.state.emit_fmt(format_args!("    mov {}, x0", ARM_ARG_REGS[base_reg_idx]));
                                }
                                self.state.emit_fmt(format_args!("    mov {}, #0", ARM_ARG_REGS[base_reg_idx + 1]));
                            }
                        }
                    }
                }
            }
        }

        // Phase 3c: Load struct-by-value register args.
        // The struct arg operand is a pointer to the struct data. We need to get
        // that pointer into x17, then load struct data from [x17] into arg regs.
        // The pointer may live in a callee-saved register (via reg_assignments)
        // or on the stack. We must check reg_assignments first, matching Phase 2a.
        for (i, arg) in args.iter().enumerate() {
            if let CallArgClass::StructByValReg { base_reg_idx, size } = arg_classes[i] {
                let regs_needed = if size <= 8 { 1 } else { 2 };
                if total_sp_adjust > 0 {
                    match arg {
                        Operand::Value(v) => {
                            if let Some(&reg) = self.reg_assignments.get(&v.0) {
                                // Value is register-allocated (e.g., GEP result in callee-saved reg)
                                let reg_name = callee_saved_name(reg);
                                self.state.emit_fmt(format_args!("    mov x17, {}", reg_name));
                            } else if let Some(slot) = self.state.get_slot(v.0) {
                                let adjusted = slot.0 + slot_adjust;
                                if self.state.is_alloca(v.0) {
                                    self.emit_add_sp_offset("x17", adjusted);
                                } else {
                                    self.emit_load_from_sp("x17", adjusted, "ldr");
                                }
                            } else {
                                self.state.emit("    mov x17, #0");
                            }
                        }
                        Operand::Const(_) => {
                            self.operand_to_x0(arg);
                            self.state.emit("    mov x17, x0");
                        }
                    }
                } else {
                    match arg {
                        Operand::Value(v) => {
                            if let Some(&reg) = self.reg_assignments.get(&v.0) {
                                // Value is register-allocated (e.g., GEP result in callee-saved reg)
                                let reg_name = callee_saved_name(reg);
                                self.state.emit_fmt(format_args!("    mov x17, {}", reg_name));
                            } else if let Some(slot) = self.state.get_slot(v.0) {
                                if self.state.is_alloca(v.0) {
                                    self.emit_add_sp_offset("x17", slot.0);
                                } else {
                                    self.emit_load_from_sp("x17", slot.0, "ldr");
                                }
                            } else {
                                self.state.emit("    mov x17, #0");
                            }
                        }
                        Operand::Const(_) => {
                            self.operand_to_x0(arg);
                            self.state.emit("    mov x17, x0");
                        }
                    }
                }
                self.state.emit_fmt(format_args!("    ldr {}, [x17]", ARM_ARG_REGS[base_reg_idx]));
                if regs_needed > 1 {
                    self.state.emit_fmt(format_args!("    ldr {}, [x17, #8]", ARM_ARG_REGS[base_reg_idx + 1]));
                }
            }
        }
    }

    fn emit_call_instruction(&mut self, direct_name: Option<&str>, _func_ptr: Option<&Operand>, indirect: bool, stack_arg_space: usize) {
        if let Some(name) = direct_name {
            self.state.emit_fmt(format_args!("    bl {}", name));
        } else if indirect {
            // fptr was spilled with "str x0, [sp, #-16]!" before stack args were allocated.
            // After sub_sp(stack_arg_space), the fptr is at sp + stack_arg_space.
            // IMPORTANT: Always use raw sp here, NOT emit_load_from_sp which switches
            // to x29 under alloca. The fptr was pushed to the runtime sp (which may be
            // below x29 after alloca), so we must reload relative to current sp.
            let spill_offset = stack_arg_space as i64;
            if Self::is_valid_imm_offset(spill_offset, "ldr", "x17") {
                self.state.emit_fmt(format_args!("    ldr x17, [sp, #{}]", spill_offset));
            } else {
                self.load_large_imm("x17", spill_offset);
                self.state.emit("    add x17, sp, x17");
                self.state.emit("    ldr x17, [x17]");
            }
            self.state.emit("    blr x17");
        }
    }

    fn emit_call_cleanup(&mut self, stack_arg_space: usize, _f128_temp_space: usize, indirect: bool) {
        let fptr_spill = if indirect { 16usize } else { 0 };
        let total = stack_arg_space + fptr_spill;
        if total > 0 {
            self.emit_add_sp(total as i64);
        }
    }

    fn emit_call_store_result(&mut self, dest: &Value, return_type: IrType) {
        if is_i128_type(return_type) {
            self.store_x0_x1_to(dest);
        } else if return_type.is_long_double() {
            self.state.emit("    bl __trunctfdf2");
            self.state.emit("    fmov x0, d0");
            self.store_x0_to(dest);
        } else if return_type == IrType::F32 {
            self.state.emit("    fmov w0, s0");
            self.store_x0_to(dest);
        } else if return_type.is_float() {
            self.state.emit("    fmov x0, d0");
            self.store_x0_to(dest);
        } else {
            self.store_x0_to(dest);
        }
    }

    fn emit_global_addr(&mut self, dest: &Value, name: &str) {
        self.state.emit_fmt(format_args!("    adrp x0, {}", name));
        self.state.emit_fmt(format_args!("    add x0, x0, :lo12:{}", name));
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

            CastKind::SignedToFloat { to_f64, from_ty } => {
                // For sub-64-bit signed sources, sign-extend to 64 bits first.
                // The value in x0 may be zero-extended (e.g., from a U32->I32 noop cast).
                match from_ty.size() {
                    1 => self.state.emit("    sxtb x0, w0"),
                    2 => self.state.emit("    sxth x0, w0"),
                    4 => self.state.emit("    sxtw x0, w0"),
                    _ => {} // 8 bytes: already full width
                }
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

    // emit_cast: uses default implementation from ArchCodegen trait (handles i128 via primitives)

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
        // Must check register allocation first: store_x0_to skips the stack
        // for register-allocated values, so the slot would be stale.
        if self.state.is_alloca(va_list_ptr.0) {
            if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
                self.emit_add_fp_offset("x1", slot.0);
            }
        } else if let Some(&reg) = self.reg_assignments.get(&va_list_ptr.0) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mov x1, {}", reg_name));
        } else if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
            self.emit_load_from_sp("x1", slot.0, "ldr");
        }

        if is_f128 {
            // F128 (long double) on AArch64: passed in Q registers, accessed via VR save area.
            // Same path as F32/F64 but reads 16 bytes (one Q register slot).
            let label_id = self.state.next_label_id();
            let label_stack = format!(".Lva_stack_{}", label_id);
            let label_done = format!(".Lva_done_{}", label_id);

            // Check __vr_offs (offset 28 in va_list)
            self.state.emit("    ldrsw x2, [x1, #28]");  // x2 = __vr_offs (sign-extended)
            self.state.emit_fmt(format_args!("    tbz x2, #63, {}", label_stack)); // if >= 0, go to stack
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
            self.state.emit_fmt(format_args!("    b {}", label_done));

            // Stack overflow path (when all 8 VR slots exhausted)
            self.state.emit_fmt(format_args!("{}:", label_stack));
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

            self.state.emit_fmt(format_args!("{}:", label_done));
        } else if is_fp {
            let label_id = self.state.next_label_id();
            let label_stack = format!(".Lva_stack_{}", label_id);
            let label_done = format!(".Lva_done_{}", label_id);

            // FP type: check __vr_offs
            self.state.emit("    ldrsw x2, [x1, #28]");  // x2 = __vr_offs (sign-extended)
            self.state.emit_fmt(format_args!("    tbz x2, #63, {}", label_stack)); // if >= 0, go to stack
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
            self.state.emit_fmt(format_args!("    b {}", label_done));

            // Stack overflow path
            self.state.emit_fmt(format_args!("{}:", label_stack));
            self.state.emit("    ldr x3, [x1]");
            if result_ty == IrType::F32 {
                self.state.emit("    ldr w0, [x3]");
            } else {
                self.state.emit("    ldr x0, [x3]");
            }
            self.state.emit("    add x3, x3, #8");
            self.state.emit("    str x3, [x1]");

            self.state.emit_fmt(format_args!("{}:", label_done));
        } else {
            let label_id = self.state.next_label_id();
            let label_stack = format!(".Lva_stack_{}", label_id);
            let label_done = format!(".Lva_done_{}", label_id);

            // GP type: check __gr_offs
            self.state.emit("    ldrsw x2, [x1, #24]");  // x2 = __gr_offs (sign-extended)
            self.state.emit_fmt(format_args!("    tbz x2, #63, {}", label_stack)); // if >= 0, go to stack
            // Register save area path: addr = __gr_top + __gr_offs
            self.state.emit("    ldr x3, [x1, #8]");      // x3 = __gr_top
            self.state.emit("    add x3, x3, x2");         // x3 = __gr_top + __gr_offs
            // Advance __gr_offs by 8
            self.state.emit("    add w2, w2, #8");
            self.state.emit("    str w2, [x1, #24]");
            // Load the value from the register save area
            self.state.emit("    ldr x0, [x3]");
            self.state.emit_fmt(format_args!("    b {}", label_done));

            // Stack overflow path
            self.state.emit_fmt(format_args!("{}:", label_stack));
            self.state.emit("    ldr x3, [x1]");
            self.state.emit("    ldr x0, [x3]");
            self.state.emit("    add x3, x3, #8");
            self.state.emit("    str x3, [x1]");

            self.state.emit_fmt(format_args!("{}:", label_done));
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
        // Must check register allocation first: store_x0_to skips the stack
        // for register-allocated values, so the slot would be stale.
        if self.state.is_alloca(va_list_ptr.0) {
            if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
                self.emit_add_fp_offset("x0", slot.0);
            }
        } else if let Some(&reg) = self.reg_assignments.get(&va_list_ptr.0) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mov x0, {}", reg_name));
        } else if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
            self.emit_load_from_sp("x0", slot.0, "ldr");
        }

        // __stack: pointer to the first variadic stack argument.
        // After prologue: sp = x29, [x29] = saved fp, [x29+8] = saved lr,
        // [x29+16..x29+frame_size-1] = local slots.
        // Caller's stack args are at x29 + frame_size (above our frame).
        // If named params overflow to the stack (e.g., ints beyond 8 GP regs, or
        // F128 beyond 8 FP regs), we must skip past all of them.
        let stack_offset = self.current_frame_size + self.va_named_stack_bytes as i64;
        if stack_offset <= 4095 {
            self.state.emit_fmt(format_args!("    add x1, x29, #{}", stack_offset));
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
        self.state.emit_fmt(format_args!("    mov w1, #{}", gr_offs));
        self.state.emit("    str w1, [x0, #24]");  // __gr_offs at offset 24

        // __vr_offs: negative offset from __vr_top to next unnamed FP/SIMD reg
        // = -(8 - named_fp_count) * 16
        let vr_offs: i32 = -((8 - self.va_named_fp_count as i32) * 16);
        self.state.emit_fmt(format_args!("    mov w1, #{}", vr_offs));
        self.state.emit("    str w1, [x0, #28]");  // __vr_offs at offset 28
    }

    // emit_va_end: uses default no-op implementation

    fn emit_va_copy(&mut self, dest_ptr: &Value, src_ptr: &Value) {
        // Copy va_list struct (32 bytes on AArch64)
        // Must check register allocation first: store_x0_to skips the stack
        // for register-allocated values, so the slot would be stale.
        if self.state.is_alloca(src_ptr.0) {
            if let Some(src_slot) = self.state.get_slot(src_ptr.0) {
                self.emit_add_fp_offset("x1", src_slot.0);
            }
        } else if let Some(&reg) = self.reg_assignments.get(&src_ptr.0) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mov x1, {}", reg_name));
        } else if let Some(src_slot) = self.state.get_slot(src_ptr.0) {
            self.emit_load_from_sp("x1", src_slot.0, "ldr");
        }
        if self.state.is_alloca(dest_ptr.0) {
            if let Some(dest_slot) = self.state.get_slot(dest_ptr.0) {
                self.emit_add_fp_offset("x0", dest_slot.0);
            }
        } else if let Some(&reg) = self.reg_assignments.get(&dest_ptr.0) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    mov x0, {}", reg_name));
        } else if let Some(dest_slot) = self.state.get_slot(dest_ptr.0) {
            self.emit_load_from_sp("x0", dest_slot.0, "ldr");
        }
        // Copy 32 bytes
        self.state.emit("    ldp x2, x3, [x1]");
        self.state.emit("    stp x2, x3, [x0]");
        self.state.emit("    ldp x2, x3, [x1, #16]");
        self.state.emit("    stp x2, x3, [x0, #16]");
    }

    // emit_return: uses default implementation from ArchCodegen trait

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
                self.emit_load_imm64("x0", bits as i64);
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
                // Use x0 (64-bit) for the load sequence since w-register MOVZ/MOVK
                // only supports shifts of 0 and 16. fmov s1, w0 reads the low 32 bits.
                self.emit_load_imm64("x0", bits as i64);
                self.state.emit("    fmov s1, w0");
            }
            _ => {
                self.operand_to_x0(src);
                self.state.emit("    fmov s1, w0");
            }
        }
    }

    fn emit_atomic_rmw(&mut self, dest: &Value, op: AtomicRmwOp, ptr: &Operand, val: &Operand, ty: IrType, ordering: AtomicOrdering) {
        // Load ptr into x1, val into x2
        self.operand_to_x0(ptr);
        self.state.emit("    mov x1, x0"); // x1 = ptr
        self.operand_to_x0(val);
        self.state.emit("    mov x2, x0"); // x2 = val

        let (ldxr, stxr, reg_prefix) = Self::exclusive_instrs(ty, ordering);
        let val_reg = format!("{}2", reg_prefix);
        let old_reg = format!("{}0", reg_prefix);
        let tmp_reg = format!("{}3", reg_prefix);

        match op {
            AtomicRmwOp::Xchg => {
                // Simple exchange: old = *ptr; *ptr = val
                let label_id = self.state.next_label_id();
                let loop_label = format!(".Latomic_{}", label_id);
                self.state.emit_fmt(format_args!("{}:", loop_label));
                self.state.emit_fmt(format_args!("    {} {}, [x1]", ldxr, old_reg));
                self.state.emit_fmt(format_args!("    {} w4, {}, [x1]", stxr, val_reg));
                self.state.emit_fmt(format_args!("    cbnz w4, {}", loop_label));
            }
            AtomicRmwOp::TestAndSet => {
                let label_id = self.state.next_label_id();
                let loop_label = format!(".Latomic_{}", label_id);
                self.state.emit_fmt(format_args!("{}:", loop_label));
                self.state.emit_fmt(format_args!("    {} {}, [x1]", ldxr, old_reg));
                self.state.emit("    mov w3, #1");
                self.state.emit_fmt(format_args!("    {} w4, w3, [x1]", stxr));
                self.state.emit_fmt(format_args!("    cbnz w4, {}", loop_label));
            }
            _ => {
                // Generic RMW: old = *ptr; new = op(old, val); *ptr = new
                let label_id = self.state.next_label_id();
                let loop_label = format!(".Latomic_{}", label_id);
                self.state.emit_fmt(format_args!("{}:", loop_label));
                self.state.emit_fmt(format_args!("    {} {}, [x1]", ldxr, old_reg));
                // Compute x3 = op(x0, x2)
                Self::emit_atomic_op_arm(&mut self.state, op, &tmp_reg, &old_reg, &val_reg);
                self.state.emit_fmt(format_args!("    {} w4, {}, [x1]", stxr, tmp_reg));
                self.state.emit_fmt(format_args!("    cbnz w4, {}", loop_label));
            }
        }
        // Result is in x0 (old value)
        self.store_x0_to(dest);
    }

    fn emit_atomic_cmpxchg(&mut self, dest: &Value, ptr: &Operand, expected: &Operand, desired: &Operand, ty: IrType, success_ordering: AtomicOrdering, _failure_ordering: AtomicOrdering, returns_bool: bool) {
        // x1 = ptr, x2 = expected, x3 = desired
        self.operand_to_x0(ptr);
        self.state.emit("    mov x1, x0");
        self.operand_to_x0(desired);
        self.state.emit("    mov x3, x0");
        self.operand_to_x0(expected);
        self.state.emit("    mov x2, x0");
        // x2 = expected

        let (ldxr, stxr, reg_prefix) = Self::exclusive_instrs(ty, success_ordering);
        let old_reg = format!("{}0", reg_prefix);
        let desired_reg = format!("{}3", reg_prefix);
        let expected_reg = format!("{}2", reg_prefix);

        let label_id = self.state.next_label_id();
        let loop_label = format!(".Lcas_loop_{}", label_id);
        let fail_label = format!(".Lcas_fail_{}", label_id);
        let done_label = format!(".Lcas_done_{}", label_id);

        self.state.emit_fmt(format_args!("{}:", loop_label));
        self.state.emit_fmt(format_args!("    {} {}, [x1]", ldxr, old_reg));
        self.state.emit_fmt(format_args!("    cmp {}, {}", old_reg, expected_reg));
        self.state.emit_fmt(format_args!("    b.ne {}", fail_label));
        self.state.emit_fmt(format_args!("    {} w4, {}, [x1]", stxr, desired_reg));
        self.state.emit_fmt(format_args!("    cbnz w4, {}", loop_label));
        if returns_bool {
            self.state.emit("    mov x0, #1");
        }
        // If !returns_bool, x0 already has old value (which equals expected on success)
        self.state.emit_fmt(format_args!("    b {}", done_label));
        self.state.emit_fmt(format_args!("{}:", fail_label));
        if returns_bool {
            self.state.emit("    mov x0, #0");
            // Clear exclusive monitor
            self.state.emit("    clrex");
        } else {
            // x0 has the old value (not equal to expected)
            self.state.emit("    clrex");
        }
        self.state.emit_fmt(format_args!("{}:", done_label));
        self.store_x0_to(dest);
    }

    fn emit_atomic_load(&mut self, dest: &Value, ptr: &Operand, ty: IrType, ordering: AtomicOrdering) {
        self.operand_to_x0(ptr);
        // Relaxed: plain load (aligned loads are naturally atomic on AArch64)
        // Acquire/SeqCst: use ldar (load-acquire) for ordering
        let need_acquire = matches!(ordering, AtomicOrdering::Acquire | AtomicOrdering::AcqRel | AtomicOrdering::SeqCst);
        let instr = match (ty, need_acquire) {
            (IrType::I8 | IrType::U8, true) => "ldarb",
            (IrType::I8 | IrType::U8, false) => "ldrb",
            (IrType::I16 | IrType::U16, true) => "ldarh",
            (IrType::I16 | IrType::U16, false) => "ldrh",
            (IrType::I32 | IrType::U32, true) => "ldar",
            (IrType::I32 | IrType::U32, false) => "ldr",
            (_, true) => "ldar",
            (_, false) => "ldr",
        };
        let dest_reg = match ty {
            IrType::I8 | IrType::U8 | IrType::I16 | IrType::U16 | IrType::I32 | IrType::U32 => "w0",
            _ => "x0",
        };
        // ldar/ldr with register indirect both use [x0] form
        self.state.emit_fmt(format_args!("    {} {}, [x0]", instr, dest_reg));
        // Sign-extend if needed
        match ty {
            IrType::I8 => self.state.emit("    sxtb x0, w0"),
            IrType::I16 => self.state.emit("    sxth x0, w0"),
            IrType::I32 => self.state.emit("    sxtw x0, w0"),
            _ => {}
        }
        self.store_x0_to(dest);
    }

    fn emit_atomic_store(&mut self, ptr: &Operand, val: &Operand, ty: IrType, ordering: AtomicOrdering) {
        self.operand_to_x0(val);
        self.state.emit("    mov x1, x0"); // x1 = val
        self.operand_to_x0(ptr);
        // Relaxed: plain store (aligned stores are naturally atomic on AArch64)
        // Release/SeqCst: use stlr (store-release) for ordering
        let need_release = matches!(ordering, AtomicOrdering::Release | AtomicOrdering::AcqRel | AtomicOrdering::SeqCst);
        let instr = match (ty, need_release) {
            (IrType::I8 | IrType::U8, true) => "stlrb",
            (IrType::I8 | IrType::U8, false) => "strb",
            (IrType::I16 | IrType::U16, true) => "stlrh",
            (IrType::I16 | IrType::U16, false) => "strh",
            (IrType::I32 | IrType::U32, true) => "stlr",
            (IrType::I32 | IrType::U32, false) => "str",
            (_, true) => "stlr",
            (_, false) => "str",
        };
        let val_reg = match ty {
            IrType::I8 | IrType::U8 | IrType::I16 | IrType::U16 | IrType::I32 | IrType::U32 => "w1",
            _ => "x1",
        };
        self.state.emit_fmt(format_args!("    {} {}, [x0]", instr, val_reg));
    }

    fn emit_fence(&mut self, ordering: AtomicOrdering) {
        // AArch64 fence instructions:
        // - Relaxed: no fence needed
        // - Acquire: dmb ishld (load-load and load-store ordering)
        // - Release: dmb ishst (store-store ordering)
        // - AcqRel/SeqCst: dmb ish (full inner-shareable barrier)
        match ordering {
            AtomicOrdering::Relaxed => {} // no-op
            AtomicOrdering::Acquire => self.state.emit("    dmb ishld"),
            AtomicOrdering::Release => self.state.emit("    dmb ishst"),
            AtomicOrdering::AcqRel | AtomicOrdering::SeqCst => self.state.emit("    dmb ish"),
        }
    }

    fn emit_inline_asm(&mut self, template: &str, outputs: &[(String, Value, Option<String>)], inputs: &[(String, Operand, Option<String>)], clobbers: &[String], operand_types: &[IrType], goto_labels: &[(String, BlockId)], input_symbols: &[Option<String>]) {
        emit_inline_asm_common(self, template, outputs, inputs, clobbers, operand_types, goto_labels, input_symbols);
    }

    fn emit_copy_i128(&mut self, dest: &Value, src: &Operand) {
        self.operand_to_x0_x1(src);
        self.store_x0_x1_to(dest);
    }

    fn emit_intrinsic(&mut self, dest: &Option<Value>, op: &IntrinsicOp, dest_ptr: &Option<Value>, args: &[Operand]) {
        self.emit_intrinsic_arm(dest, op, dest_ptr, args);
    }

    // ---- Float binop primitives ----

    fn emit_float_binop_mnemonic(&self, op: FloatOp) -> &'static str {
        match op {
            FloatOp::Add => "fadd",
            FloatOp::Sub => "fsub",
            FloatOp::Mul => "fmul",
            FloatOp::Div => "fdiv",
        }
    }

    fn emit_float_binop_impl(&mut self, mnemonic: &str, ty: IrType) {
        if ty == IrType::F128 {
            // F128: use soft-float library calls for full quad-precision arithmetic.
            self.emit_f128_binop_softfloat(mnemonic);
            return;
        }
        // secondary = lhs (in x1 via acc_to_secondary = mov x1, x0), acc = rhs (in x0)
        // After the shared default: lhs loaded to acc, pushed to secondary, rhs loaded to acc
        // On ARM, emit_acc_to_secondary does "mov x1, x0" so: x1=lhs, x0=rhs
        // Move to x2 so we have x1=lhs, x2=rhs (matching original convention)
        self.state.emit("    mov x2, x0");
        if ty == IrType::F32 {
            self.state.emit("    fmov s0, w1");
            self.state.emit("    fmov s1, w2");
            self.state.emit_fmt(format_args!("    {} s0, s0, s1", mnemonic));
            self.state.emit("    fmov w0, s0");
            self.state.emit("    mov w0, w0"); // zero-extend
        } else {
            self.state.emit("    fmov d0, x1");
            self.state.emit("    fmov d1, x2");
            self.state.emit_fmt(format_args!("    {} d0, d0, d1", mnemonic));
            self.state.emit("    fmov x0, d0");
        }
    }

    // ---- i128 binop primitives ----

    fn emit_i128_prep_binop(&mut self, lhs: &Operand, rhs: &Operand) {
        self.prep_i128_binop(lhs, rhs);
    }

    fn emit_i128_add(&mut self) {
        self.state.emit("    adds x0, x2, x4");
        self.state.emit("    adc x1, x3, x5");
    }

    fn emit_i128_sub(&mut self) {
        self.state.emit("    subs x0, x2, x4");
        self.state.emit("    sbc x1, x3, x5");
    }

    fn emit_i128_mul(&mut self) {
        // x2:x3 = lhs (lo:hi), x4:x5 = rhs (lo:hi)
        self.state.emit("    mul x0, x2, x4");       // x0 = lo(a_lo * b_lo)
        self.state.emit("    umulh x1, x2, x4");     // x1 = hi(a_lo * b_lo)
        self.state.emit("    madd x1, x3, x4, x1");  // x1 += a_hi * b_lo
        self.state.emit("    madd x1, x2, x5, x1");  // x1 += a_lo * b_hi
    }

    fn emit_i128_and(&mut self) {
        self.state.emit("    and x0, x2, x4");
        self.state.emit("    and x1, x3, x5");
    }

    fn emit_i128_or(&mut self) {
        self.state.emit("    orr x0, x2, x4");
        self.state.emit("    orr x1, x3, x5");
    }

    fn emit_i128_xor(&mut self) {
        self.state.emit("    eor x0, x2, x4");
        self.state.emit("    eor x1, x3, x5");
    }

    fn emit_i128_shl(&mut self) {
        let lbl = self.state.fresh_label("shl128");
        let done = self.state.fresh_label("shl128_done");
        let noop = self.state.fresh_label("shl128_noop");
        self.state.emit("    and x4, x4, #127");
        self.state.emit_fmt(format_args!("    cbz x4, {}", noop));
        self.state.emit("    cmp x4, #64");
        self.state.emit_fmt(format_args!("    b.ge {}", lbl));
        self.state.emit("    lsl x1, x3, x4");
        self.state.emit("    mov x5, #64");
        self.state.emit("    sub x5, x5, x4");
        self.state.emit("    lsr x6, x2, x5");
        self.state.emit("    orr x1, x1, x6");
        self.state.emit("    lsl x0, x2, x4");
        self.state.emit_fmt(format_args!("    b {}", done));
        self.state.emit_fmt(format_args!("{}:", lbl));
        self.state.emit("    sub x4, x4, #64");
        self.state.emit("    lsl x1, x2, x4");
        self.state.emit("    mov x0, #0");
        self.state.emit_fmt(format_args!("    b {}", done));
        self.state.emit_fmt(format_args!("{}:", noop));
        self.state.emit("    mov x0, x2");
        self.state.emit("    mov x1, x3");
        self.state.emit_fmt(format_args!("{}:", done));
    }

    fn emit_i128_lshr(&mut self) {
        let lbl = self.state.fresh_label("lshr128");
        let done = self.state.fresh_label("lshr128_done");
        let noop = self.state.fresh_label("lshr128_noop");
        self.state.emit("    and x4, x4, #127");
        self.state.emit_fmt(format_args!("    cbz x4, {}", noop));
        self.state.emit("    cmp x4, #64");
        self.state.emit_fmt(format_args!("    b.ge {}", lbl));
        self.state.emit("    lsr x0, x2, x4");
        self.state.emit("    mov x5, #64");
        self.state.emit("    sub x5, x5, x4");
        self.state.emit("    lsl x6, x3, x5");
        self.state.emit("    orr x0, x0, x6");
        self.state.emit("    lsr x1, x3, x4");
        self.state.emit_fmt(format_args!("    b {}", done));
        self.state.emit_fmt(format_args!("{}:", lbl));
        self.state.emit("    sub x4, x4, #64");
        self.state.emit("    lsr x0, x3, x4");
        self.state.emit("    mov x1, #0");
        self.state.emit_fmt(format_args!("    b {}", done));
        self.state.emit_fmt(format_args!("{}:", noop));
        self.state.emit("    mov x0, x2");
        self.state.emit("    mov x1, x3");
        self.state.emit_fmt(format_args!("{}:", done));
    }

    fn emit_i128_ashr(&mut self) {
        let lbl = self.state.fresh_label("ashr128");
        let done = self.state.fresh_label("ashr128_done");
        let noop = self.state.fresh_label("ashr128_noop");
        self.state.emit("    and x4, x4, #127");
        self.state.emit_fmt(format_args!("    cbz x4, {}", noop));
        self.state.emit("    cmp x4, #64");
        self.state.emit_fmt(format_args!("    b.ge {}", lbl));
        self.state.emit("    lsr x0, x2, x4");
        self.state.emit("    mov x5, #64");
        self.state.emit("    sub x5, x5, x4");
        self.state.emit("    lsl x6, x3, x5");
        self.state.emit("    orr x0, x0, x6");
        self.state.emit("    asr x1, x3, x4");
        self.state.emit_fmt(format_args!("    b {}", done));
        self.state.emit_fmt(format_args!("{}:", lbl));
        self.state.emit("    sub x4, x4, #64");
        self.state.emit("    asr x0, x3, x4");
        self.state.emit("    asr x1, x3, #63");
        self.state.emit_fmt(format_args!("    b {}", done));
        self.state.emit_fmt(format_args!("{}:", noop));
        self.state.emit("    mov x0, x2");
        self.state.emit("    mov x1, x3");
        self.state.emit_fmt(format_args!("{}:", done));
    }

    fn emit_i128_prep_shift_lhs(&mut self, lhs: &Operand) {
        // Load only LHS into x2:x3 for constant-amount shifts
        self.operand_to_x0_x1(lhs);
        self.state.emit("    mov x2, x0");
        self.state.emit("    mov x3, x1");
    }

    fn emit_i128_shl_const(&mut self, amount: u32) {
        // Input: x2 (low), x3 (high). Output: x0 (low), x1 (high).
        let amount = amount & 127;
        if amount == 0 {
            self.state.emit("    mov x0, x2");
            self.state.emit("    mov x1, x3");
        } else if amount == 64 {
            self.state.emit("    mov x1, x2");
            self.state.emit("    mov x0, #0");
        } else if amount > 64 {
            self.state.emit_fmt(format_args!("    lsl x1, x2, #{}", amount - 64));
            self.state.emit("    mov x0, #0");
        } else {
            // 0 < amount < 64
            self.state.emit_fmt(format_args!("    lsl x1, x3, #{}", amount));
            self.state.emit_fmt(format_args!("    orr x1, x1, x2, lsr #{}", 64 - amount));
            self.state.emit_fmt(format_args!("    lsl x0, x2, #{}", amount));
        }
    }

    fn emit_i128_lshr_const(&mut self, amount: u32) {
        // Input: x2 (low), x3 (high). Output: x0 (low), x1 (high).
        let amount = amount & 127;
        if amount == 0 {
            self.state.emit("    mov x0, x2");
            self.state.emit("    mov x1, x3");
        } else if amount == 64 {
            self.state.emit("    mov x0, x3");
            self.state.emit("    mov x1, #0");
        } else if amount > 64 {
            self.state.emit_fmt(format_args!("    lsr x0, x3, #{}", amount - 64));
            self.state.emit("    mov x1, #0");
        } else {
            // 0 < amount < 64
            self.state.emit_fmt(format_args!("    lsr x0, x2, #{}", amount));
            self.state.emit_fmt(format_args!("    orr x0, x0, x3, lsl #{}", 64 - amount));
            self.state.emit_fmt(format_args!("    lsr x1, x3, #{}", amount));
        }
    }

    fn emit_i128_ashr_const(&mut self, amount: u32) {
        // Input: x2 (low), x3 (high). Output: x0 (low), x1 (high).
        let amount = amount & 127;
        if amount == 0 {
            self.state.emit("    mov x0, x2");
            self.state.emit("    mov x1, x3");
        } else if amount == 64 {
            self.state.emit("    mov x0, x3");
            self.state.emit("    asr x1, x3, #63");
        } else if amount > 64 {
            self.state.emit_fmt(format_args!("    asr x0, x3, #{}", amount - 64));
            self.state.emit("    asr x1, x3, #63");
        } else {
            // 0 < amount < 64
            self.state.emit_fmt(format_args!("    lsr x0, x2, #{}", amount));
            self.state.emit_fmt(format_args!("    orr x0, x0, x3, lsl #{}", 64 - amount));
            self.state.emit_fmt(format_args!("    asr x1, x3, #{}", amount));
        }
    }

    fn emit_i128_divrem_call(&mut self, func_name: &str, lhs: &Operand, rhs: &Operand) {
        // AAPCS64: first 128-bit arg in x0:x1, second in x2:x3
        self.operand_to_x0_x1(lhs);
        self.state.emit("    mov x2, x0");
        self.state.emit("    mov x3, x1");
        self.operand_to_x0_x1(rhs);
        self.state.emit("    mov x4, x0");
        self.state.emit("    mov x5, x1");
        self.state.emit("    mov x0, x2");
        self.state.emit("    mov x1, x3");
        self.state.emit("    mov x2, x4");
        self.state.emit("    mov x3, x5");
        self.state.emit_fmt(format_args!("    bl {}", func_name));
        // Result in x0:x1
    }

    fn emit_i128_store_result(&mut self, dest: &Value) {
        self.store_x0_x1_to(dest);
    }

    // ---- i128 cmp primitives ----

    fn emit_i128_cmp_eq(&mut self, is_ne: bool) {
        // x2:x3 = lhs, x4:x5 = rhs (after prep)
        self.state.emit("    eor x0, x2, x4");
        self.state.emit("    eor x1, x3, x5");
        self.state.emit("    orr x0, x0, x1");
        self.state.emit("    cmp x0, #0");
        if is_ne {
            self.state.emit("    cset x0, ne");
        } else {
            self.state.emit("    cset x0, eq");
        }
    }

    fn emit_i128_cmp_ordered(&mut self, op: IrCmpOp) {
        let done = self.state.fresh_label("cmp128_done");
        self.state.emit("    cmp x3, x5");
        let (hi_cond, lo_cond) = match op {
            IrCmpOp::Slt | IrCmpOp::Sle => ("lt", if op == IrCmpOp::Slt { "lo" } else { "ls" }),
            IrCmpOp::Sgt | IrCmpOp::Sge => ("gt", if op == IrCmpOp::Sgt { "hi" } else { "hs" }),
            IrCmpOp::Ult | IrCmpOp::Ule => ("lo", if op == IrCmpOp::Ult { "lo" } else { "ls" }),
            IrCmpOp::Ugt | IrCmpOp::Uge => ("hi", if op == IrCmpOp::Ugt { "hi" } else { "hs" }),
            _ => unreachable!(),
        };
        self.state.emit_fmt(format_args!("    cset x0, {}", hi_cond));
        self.state.emit_fmt(format_args!("    b.ne {}", done));
        self.state.emit("    cmp x2, x4");
        self.state.emit_fmt(format_args!("    cset x0, {}", lo_cond));
        self.state.emit_fmt(format_args!("{}:", done));
    }

    fn emit_i128_cmp_store_result(&mut self, dest: &Value) {
        self.store_x0_to(dest);
    }
}

impl Default for ArmCodegen {
    fn default() -> Self {
        Self::new()
    }
}

/// AArch64 scratch registers for inline asm (caller-saved temporaries).
const ARM_GP_SCRATCH: &[&str] = &["x9", "x10", "x11", "x12", "x13", "x14", "x15", "x19", "x20", "x21"];
/// AArch64 FP/SIMD scratch registers for inline asm (d8-d15 are callee-saved,
/// d16-d31 are caller-saved; we use d16+ as scratch to avoid save/restore).
const ARM_FP_SCRATCH: &[&str] = &["d16", "d17", "d18", "d19", "d20", "d21", "d22", "d23", "d24", "d25"];

impl InlineAsmEmitter for ArmCodegen {
    fn asm_state(&mut self) -> &mut CodegenState { &mut self.state }
    fn asm_state_ref(&self) -> &CodegenState { &self.state }

    // TODO: Support multi-alternative constraint parsing (e.g., "rm", "ri") like x86.
    // TODO: Support ARM-specific immediate constraints ("I", "J", "K", "L", etc.).
    fn classify_constraint(&self, constraint: &str) -> AsmOperandKind {
        let c = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
        // Explicit register constraint from register variable: {regname}
        if c.starts_with('{') && c.ends_with('}') {
            let reg_name = &c[1..c.len()-1];
            return AsmOperandKind::Specific(reg_name.to_string());
        }
        // TODO: ARM =@cc not fully implemented — needs CSET/CSINC in store_output_from_reg.
        // Currently stores incorrect results (just a GP register value, no condition capture).
        if let Some(cond) = c.strip_prefix("@cc") {
            return AsmOperandKind::ConditionCode(cond.to_string());
        }
        if !c.is_empty() && c.chars().all(|ch| ch.is_ascii_digit()) {
            if let Ok(n) = c.parse::<usize>() {
                return AsmOperandKind::Tied(n);
            }
        }
        if c == "m" || c == "Q" { return AsmOperandKind::Memory; }
        // Multi-char constraints containing Q (e.g., "Qm") — treat as memory if Q is present
        if c.contains('Q') || c.contains('m') { return AsmOperandKind::Memory; }
        // "w" = AArch64 floating-point/SIMD register (d0-d31/s0-s31)
        if c == "w" { return AsmOperandKind::FpReg; }
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

    fn resolve_memory_operand(&mut self, op: &mut AsmOperand, val: &Operand, excluded: &[String]) -> bool {
        if !op.mem_addr.is_empty() || op.mem_offset != 0 {
            return false;
        }
        // Each memory operand gets its own unique register via assign_scratch_reg,
        // so multiple "=m" outputs don't overwrite each other's addresses.
        match val {
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    let tmp_reg = self.assign_scratch_reg(&AsmOperandKind::GpReg, excluded);
                    self.emit_load_from_sp(&tmp_reg, slot.0, "ldr");
                    op.mem_addr = format!("[{}]", tmp_reg);
                    return true;
                }
            }
            Operand::Const(c) => {
                // Constant address (e.g., from MMIO reads at compile-time constant addresses).
                // Copy propagation can replace Value operands with Const in inline asm inputs.
                // Load the constant into a scratch register for indirect addressing.
                if let Some(addr) = c.to_i64() {
                    let tmp_reg = self.assign_scratch_reg(&AsmOperandKind::GpReg, excluded);
                    self.emit_load_imm64(&tmp_reg, addr);
                    op.mem_addr = format!("[{}]", tmp_reg);
                    return true;
                }
            }
        }
        false
    }

    fn assign_scratch_reg(&mut self, kind: &AsmOperandKind, excluded: &[String]) -> String {
        if matches!(kind, AsmOperandKind::FpReg) {
            let idx = self.asm_fp_scratch_idx;
            self.asm_fp_scratch_idx += 1;
            if idx < ARM_FP_SCRATCH.len() {
                ARM_FP_SCRATCH[idx].to_string()
            } else {
                format!("d{}", 16 + idx)
            }
        } else {
            loop {
                let idx = self.asm_scratch_idx;
                self.asm_scratch_idx += 1;
                let reg = if idx < ARM_GP_SCRATCH.len() {
                    ARM_GP_SCRATCH[idx].to_string()
                } else {
                    format!("x{}", 9 + idx)
                };
                if !excluded.iter().any(|e| e == &reg) {
                    // If this is a callee-saved register (x19-x28), ensure it is
                    // saved/restored in the prologue/epilogue.
                    let reg_num = reg.strip_prefix('x')
                        .and_then(|s| s.parse::<u8>().ok());
                    if let Some(n) = reg_num {
                        if (19..=28).contains(&n) {
                            let phys = PhysReg(n);
                            if !self.used_callee_saved.contains(&phys) {
                                self.used_callee_saved.push(phys);
                                self.used_callee_saved.sort_by_key(|r| r.0);
                            }
                        }
                    }
                    return reg;
                }
            }
        }
    }

    fn load_input_to_reg(&mut self, op: &AsmOperand, val: &Operand, _constraint: &str) {
        let reg = &op.reg;
        let is_fp = reg.starts_with('d') || reg.starts_with('s');
        match val {
            Operand::Const(c) => {
                if is_fp {
                    // Load FP constant: move to GP reg first, then fmov to FP reg
                    let bits = c.to_i64().unwrap_or(0);
                    self.emit_load_imm64("x9", bits);
                    if op.operand_type == IrType::F32 {
                        // Convert d-register name to s-register for single-precision
                        let s_reg = Self::d_to_s_reg(reg);
                        self.state.emit_fmt(format_args!("    fmov {}, w9", s_reg));
                    } else {
                        self.state.emit_fmt(format_args!("    fmov {}, x9", reg));
                    }
                } else {
                    self.emit_load_imm64(reg, c.to_i64().unwrap_or(0));
                }
            }
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    if is_fp {
                        // Load FP value from stack: load raw bits to GP reg, then fmov
                        if op.operand_type == IrType::F32 {
                            self.state.emit_fmt(format_args!("    ldr w9, [sp, #{}]", slot.0));
                            let s_reg = Self::d_to_s_reg(reg);
                            self.state.emit_fmt(format_args!("    fmov {}, w9", s_reg));
                        } else {
                            self.state.emit_fmt(format_args!("    ldr x9, [sp, #{}]", slot.0));
                            self.state.emit_fmt(format_args!("    fmov {}, x9", reg));
                        }
                    } else if self.state.is_alloca(v.0) {
                        // Alloca: the stack slot IS the variable's memory;
                        // compute its address instead of loading from it.
                        self.emit_add_sp_offset(reg, slot.0);
                    } else {
                        self.emit_load_from_sp(reg, slot.0, "ldr");
                    }
                }
            }
        }
    }

    fn preload_readwrite_output(&mut self, op: &AsmOperand, ptr: &Value) {
        let reg = &op.reg;
        let is_fp = reg.starts_with('d') || reg.starts_with('s');
        if let Some(slot) = self.state.get_slot(ptr.0) {
            if is_fp {
                // Load current FP value for read-write constraint
                if op.operand_type == IrType::F32 {
                    self.state.emit_fmt(format_args!("    ldr w9, [sp, #{}]", slot.0));
                    let s_reg = Self::d_to_s_reg(reg);
                    self.state.emit_fmt(format_args!("    fmov {}, w9", s_reg));
                } else {
                    self.state.emit_fmt(format_args!("    ldr x9, [sp, #{}]", slot.0));
                    self.state.emit_fmt(format_args!("    fmov {}, x9", reg));
                }
            } else {
                self.emit_load_from_sp(reg, slot.0, "ldr");
            }
        }
    }

    fn substitute_template_line(&self, line: &str, operands: &[AsmOperand], gcc_to_internal: &[usize], _operand_types: &[IrType], goto_labels: &[(String, BlockId)]) -> String {
        // For memory operands (Q/m constraints), use mem_addr (e.g., "[x9]") or
        // format as [sp, #offset] for stack-based memory. For register operands,
        // use the register name directly.
        let op_regs: Vec<String> = operands.iter().map(|o| {
            if matches!(o.kind, AsmOperandKind::Memory) {
                if !o.mem_addr.is_empty() {
                    // Non-alloca pointer: mem_addr already formatted as "[xN]"
                    o.mem_addr.clone()
                } else if o.mem_offset != 0 {
                    // Alloca: stack-relative address
                    format!("[sp, #{}]", o.mem_offset)
                } else {
                    // Fallback: wrap register in brackets
                    if o.reg.is_empty() {
                        "[sp]".to_string()
                    } else {
                        format!("[{}]", o.reg)
                    }
                }
            } else {
                o.reg.clone()
            }
        }).collect();
        let op_names: Vec<Option<String>> = operands.iter().map(|o| o.name.clone()).collect();
        let mut result = Self::substitute_asm_operands_static(line, &op_regs, &op_names, gcc_to_internal);
        // Substitute %l[name] goto label references
        result = crate::backend::inline_asm::substitute_goto_labels(&result, goto_labels, operands.len());
        result
    }

    fn store_output_from_reg(&mut self, op: &AsmOperand, ptr: &Value, _constraint: &str) {
        if matches!(op.kind, AsmOperandKind::Memory) {
            return;
        }
        let reg = &op.reg;
        let is_fp = reg.starts_with('d') || reg.starts_with('s');
        if let Some(slot) = self.state.get_slot(ptr.0) {
            if is_fp {
                // Store FP register output: fmov to GP reg, then store to stack
                if op.operand_type == IrType::F32 {
                    let s_reg = Self::d_to_s_reg(reg);
                    self.state.emit_fmt(format_args!("    fmov w9, {}", s_reg));
                    self.state.emit_fmt(format_args!("    str w9, [sp, #{}]", slot.0));
                } else {
                    self.state.emit_fmt(format_args!("    fmov x9, {}", reg));
                    self.state.emit_fmt(format_args!("    str x9, [sp, #{}]", slot.0));
                }
            } else if self.state.is_alloca(ptr.0) {
                self.emit_store_to_sp(reg, slot.0, "str");
            } else {
                // Non-alloca: slot holds a pointer, store through it
                let scratch = if reg != "x9" { "x9" } else { "x10" };
                self.emit_load_from_sp(scratch, slot.0, "ldr");
                self.state.emit_fmt(format_args!("    str {}, [{}]", reg, scratch));
            }
        }
    }

    fn reset_scratch_state(&mut self) {
        self.asm_scratch_idx = 0;
        self.asm_fp_scratch_idx = 0;
    }
}
