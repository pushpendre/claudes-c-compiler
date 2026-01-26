use crate::ir::ir::*;
use crate::common::types::{AddressSpace, IrType};
use crate::common::fx_hash::{FxHashMap, FxHashSet};
use crate::backend::common::PtrDirective;
use crate::backend::state::{CodegenState, StackSlot};
use crate::backend::traits::ArchCodegen;
use crate::backend::generation::{generate_module, is_i128_type, calculate_stack_space_common, find_param_alloca};
use crate::backend::call_abi::{CallAbiConfig, CallArgClass, compute_stack_push_bytes};
use crate::backend::call_emit::{ParamClass, classify_params};
use crate::backend::cast::{CastKind, classify_cast, FloatOp};
use crate::backend::inline_asm::{InlineAsmEmitter, AsmOperandKind, AsmOperand, emit_inline_asm_common};
use crate::backend::regalloc::{self, PhysReg, RegAllocConfig};

/// x86-64 callee-saved registers available for register allocation.
/// System V AMD64 ABI callee-saved: rbx, r12, r13, r14, r15.
/// rbp is the frame pointer and cannot be allocated.
/// PhysReg encoding: 1=rbx, 2=r12, 3=r13, 4=r14, 5=r15.
const X86_CALLEE_SAVED: [PhysReg; 5] = [
    PhysReg(1), PhysReg(2), PhysReg(3), PhysReg(4), PhysReg(5),
];

/// Map a PhysReg index to its x86-64 register name.
fn callee_saved_name(reg: PhysReg) -> &'static str {
    match reg.0 {
        1 => "rbx", 2 => "r12", 3 => "r13", 4 => "r14", 5 => "r15",
        _ => unreachable!("invalid x86 callee-saved register index"),
    }
}

/// Map a PhysReg index to its x86-64 32-bit sub-register name.
fn callee_saved_name_32(reg: PhysReg) -> &'static str {
    match reg.0 {
        1 => "ebx", 2 => "r12d", 3 => "r13d", 4 => "r14d", 5 => "r15d",
        _ => unreachable!("invalid x86 callee-saved register index"),
    }
}

/// Scan inline asm instructions in a function and collect any callee-saved
/// registers that are used via specific constraints or listed in clobbers.
/// These must be saved/restored in the function prologue/epilogue.
fn collect_inline_asm_callee_saved_x86(func: &IrFunction, used: &mut Vec<PhysReg>) {
    // x86-64 callee-saved: rbx (PhysReg(1)), r12-r15 (PhysReg(2)-PhysReg(5))
    // Map register name -> PhysReg index
    fn reg_to_phys(name: &str) -> Option<PhysReg> {
        match name {
            "rbx" | "ebx" | "bx" | "bl" | "bh" => Some(PhysReg(1)),
            "r12" | "r12d" | "r12w" | "r12b" => Some(PhysReg(2)),
            "r13" | "r13d" | "r13w" | "r13b" => Some(PhysReg(3)),
            "r14" | "r14d" | "r14w" | "r14b" => Some(PhysReg(4)),
            "r15" | "r15d" | "r15w" | "r15b" => Some(PhysReg(5)),
            _ => None,
        }
    }

    let mut already: FxHashSet<u8> = used.iter().map(|r| r.0).collect();

    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::InlineAsm { outputs, inputs, clobbers, .. } = inst {
                // Check output constraints for specific callee-saved registers
                for (constraint, _, _) in outputs {
                    let c = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
                    if let Some(phys) = constraint_to_callee_saved_x86(c) {
                        if already.insert(phys.0) {
                            used.push(phys);
                        }
                    }
                }
                // Check input constraints for specific callee-saved registers
                for (constraint, _, _) in inputs {
                    let c = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
                    if let Some(phys) = constraint_to_callee_saved_x86(c) {
                        if already.insert(phys.0) {
                            used.push(phys);
                        }
                    }
                }
                // Check clobber list for callee-saved register names
                for clobber in clobbers {
                    if let Some(phys) = reg_to_phys(clobber.as_str()) {
                        if already.insert(phys.0) {
                            used.push(phys);
                        }
                    }
                }
            }
        }
    }
}

/// Check if a constraint string refers to a specific x86-64 callee-saved register.
fn constraint_to_callee_saved_x86(constraint: &str) -> Option<PhysReg> {
    // Handle explicit register constraint: {regname}
    if constraint.starts_with('{') && constraint.ends_with('}') {
        let reg = &constraint[1..constraint.len()-1];
        return match reg {
            "rbx" | "ebx" | "bx" | "bl" | "bh" => Some(PhysReg(1)),
            "r12" | "r12d" | "r12w" | "r12b" => Some(PhysReg(2)),
            "r13" | "r13d" | "r13w" | "r13b" => Some(PhysReg(3)),
            "r14" | "r14d" | "r14w" | "r14b" => Some(PhysReg(4)),
            "r15" | "r15d" | "r15w" | "r15b" => Some(PhysReg(5)),
            _ => None,
        };
    }
    // Check single-character constraint letters
    for ch in constraint.chars() {
        match ch {
            'b' => return Some(PhysReg(1)), // rbx
            // Note: 'a','c','d','S','D' are caller-saved, no save needed
            _ => {}
        }
    }
    None
}

/// Map an ALU binary op (Add/Sub/And/Or/Xor) to its x86 mnemonic base.
fn alu_mnemonic(op: IrBinOp) -> &'static str {
    match op {
        IrBinOp::Add => "add",
        IrBinOp::Sub => "sub",
        IrBinOp::And => "and",
        IrBinOp::Or => "or",
        IrBinOp::Xor => "xor",
        _ => unreachable!("not a simple ALU op"),
    }
}

/// Map a shift op to its x86 32-bit and 64-bit mnemonic.
fn shift_mnemonic(op: IrBinOp) -> (&'static str, &'static str) {
    match op {
        IrBinOp::Shl => ("shll", "shlq"),
        IrBinOp::AShr => ("sarl", "sarq"),
        IrBinOp::LShr => ("shrl", "shrq"),
        _ => unreachable!("not a shift op"),
    }
}

/// x86-64 code generator. Implements the ArchCodegen trait for the shared framework.
/// Uses System V AMD64 ABI with linear scan register allocation for callee-saved registers.
pub struct X86Codegen {
    state: CodegenState,
    current_return_type: IrType,
    /// For variadic functions: number of named integer/pointer parameters (excluding long double)
    num_named_int_params: usize,
    /// For variadic functions: number of named float/double parameters (excluding long double)
    num_named_fp_params: usize,
    /// For variadic functions: total bytes of named parameters that are always stack-passed
    /// (e.g. long double = 16 bytes each, struct params passed by value on stack)
    num_named_stack_bytes: usize,
    /// For variadic functions: stack offset of the register save area (negative from rbp)
    reg_save_area_offset: i64,
    /// Whether the current function is variadic
    is_variadic: bool,
    /// Scratch register index for inline asm allocation (GP registers)
    asm_scratch_idx: usize,
    /// Scratch register index for inline asm allocation (XMM registers)
    asm_xmm_scratch_idx: usize,
    /// Register allocation results for the current function.
    /// Maps value ID -> callee-saved register assignment.
    reg_assignments: FxHashMap<u32, PhysReg>,
    /// Which callee-saved registers are used and need save/restore.
    used_callee_saved: Vec<PhysReg>,
    /// Whether SSE is disabled (-mno-sse). When true, variadic prologues skip
    /// XMM saves and va_start sets fp_offset to overflow immediately.
    no_sse: bool,
}

impl X86Codegen {
    pub fn new() -> Self {
        Self {
            state: CodegenState::new(),
            current_return_type: IrType::I64,
            num_named_int_params: 0,
            num_named_fp_params: 0,
            num_named_stack_bytes: 0,
            reg_save_area_offset: 0,
            is_variadic: false,
            asm_scratch_idx: 0,
            asm_xmm_scratch_idx: 0,
            reg_assignments: FxHashMap::default(),
            used_callee_saved: Vec::new(),
            no_sse: false,
        }
    }

    pub fn new_with_pic(pic: bool) -> Self {
        let mut s = Self::new();
        s.state.pic_mode = pic;
        s
    }

    /// Enable position-independent code generation.
    pub fn set_pic(&mut self, pic: bool) {
        self.state.pic_mode = pic;
    }

    /// Enable function return thunk (-mfunction-return=thunk-extern).
    pub fn set_function_return_thunk(&mut self, enabled: bool) {
        self.state.function_return_thunk = enabled;
    }

    /// Enable indirect branch thunk (-mindirect-branch=thunk-extern).
    pub fn set_indirect_branch_thunk(&mut self, enabled: bool) {
        self.state.indirect_branch_thunk = enabled;
    }

    /// Set patchable function entry configuration (-fpatchable-function-entry=N,M).
    pub fn set_patchable_function_entry(&mut self, entry: Option<(u32, u32)>) {
        self.state.patchable_function_entry = entry;
    }

    /// Enable CF protection branch (-fcf-protection=branch) to emit endbr64.
    pub fn set_cf_protection_branch(&mut self, enabled: bool) {
        self.state.cf_protection_branch = enabled;
    }

    /// Disable SSE (-mno-sse). Prevents emission of any SSE/XMM instructions.
    pub fn set_no_sse(&mut self, enabled: bool) {
        self.no_sse = enabled;
    }

    pub fn generate(mut self, module: &IrModule) -> String {
        let raw = generate_module(&mut self, module);
        super::peephole::peephole_optimize(raw)
    }

    // --- x86 helper methods ---

    /// Get the callee-saved register assigned to an operand, if any.
    fn operand_reg(&self, op: &Operand) -> Option<PhysReg> {
        match op {
            Operand::Value(v) => self.reg_assignments.get(&v.0).copied(),
            _ => None,
        }
    }

    /// Get the callee-saved register assigned to a destination value, if any.
    fn dest_reg(&self, dest: &Value) -> Option<PhysReg> {
        self.reg_assignments.get(&dest.0).copied()
    }

    /// Emit sign-extension from 32-bit to 64-bit register if the type is signed.
    /// Used after 32-bit ALU operations on callee-saved registers.
    fn emit_sext32_if_needed(&mut self, name_32: &str, name_64: &str, is_unsigned: bool) {
        if !is_unsigned {
            self.state.emit_fmt(format_args!("    movslq %{}, %{}", name_32, name_64));
        }
    }

    /// Emit a cmpq instruction for two integer operands, choosing the best form
    /// based on register allocation (register-direct, immediate, or accumulator fallback).
    /// Used by both emit_cmp and emit_fused_cmp_branch.
    fn emit_int_cmp_insn(&mut self, lhs: &Operand, rhs: &Operand) {
        self.emit_int_cmp_insn_typed(lhs, rhs, false);
    }

    /// Emit a comparison instruction, optionally using 32-bit form for I32/U32 types.
    /// When `use_32bit` is true, emits `cmpl` with 32-bit register names instead of `cmpq`.
    fn emit_int_cmp_insn_typed(&mut self, lhs: &Operand, rhs: &Operand, use_32bit: bool) {
        let cmp_instr = if use_32bit { "cmpl" } else { "cmpq" };
        let lhs_phys = self.operand_reg(lhs);
        let rhs_phys = self.operand_reg(rhs);
        if let (Some(lhs_r), Some(rhs_r)) = (lhs_phys, rhs_phys) {
            // Both in callee-saved registers: compare directly
            let lhs_name = if use_32bit { callee_saved_name_32(lhs_r) } else { callee_saved_name(lhs_r) };
            let rhs_name = if use_32bit { callee_saved_name_32(rhs_r) } else { callee_saved_name(rhs_r) };
            self.state.emit_fmt(format_args!("    {} %{}, %{}", cmp_instr, rhs_name, lhs_name));
        } else if let Some(imm) = Self::const_as_imm32(rhs) {
            if let Some(lhs_r) = lhs_phys {
                let lhs_name = if use_32bit { callee_saved_name_32(lhs_r) } else { callee_saved_name(lhs_r) };
                self.state.emit_fmt(format_args!("    {} ${}, %{}", cmp_instr, imm, lhs_name));
            } else {
                self.operand_to_rax(lhs);
                let reg = if use_32bit { "eax" } else { "rax" };
                self.state.emit_fmt(format_args!("    {} ${}, %{}", cmp_instr, imm, reg));
            }
        } else if let Some(lhs_r) = lhs_phys {
            let lhs_name = if use_32bit { callee_saved_name_32(lhs_r) } else { callee_saved_name(lhs_r) };
            self.operand_to_rcx(rhs);
            let rcx = if use_32bit { "ecx" } else { "rcx" };
            self.state.emit_fmt(format_args!("    {} %{}, %{}", cmp_instr, rcx, lhs_name));
        } else if let Some(rhs_r) = rhs_phys {
            let rhs_name = if use_32bit { callee_saved_name_32(rhs_r) } else { callee_saved_name(rhs_r) };
            self.operand_to_rax(lhs);
            let reg = if use_32bit { "eax" } else { "rax" };
            self.state.emit_fmt(format_args!("    {} %{}, %{}", cmp_instr, rhs_name, reg));
        } else {
            self.operand_to_rax(lhs);
            self.operand_to_rcx(rhs);
            let (rcx, rax) = if use_32bit { ("ecx", "eax") } else { ("rcx", "rax") };
            self.state.emit_fmt(format_args!("    {} %{}, %{}", cmp_instr, rcx, rax));
        }
    }

    /// Load an operand into a specific callee-saved register.
    /// Handles constants, register-allocated values, and stack values.
    fn operand_to_callee_reg(&mut self, op: &Operand, target: PhysReg) {
        let target_name = callee_saved_name(target);
        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::I8(v) => self.state.emit_fmt(format_args!("    movq ${}, %{}", *v as i64, target_name)),
                    IrConst::I16(v) => self.state.emit_fmt(format_args!("    movq ${}, %{}", *v as i64, target_name)),
                    IrConst::I32(v) => self.state.emit_fmt(format_args!("    movq ${}, %{}", *v as i64, target_name)),
                    IrConst::I64(v) => {
                        if *v >= i32::MIN as i64 && *v <= i32::MAX as i64 {
                            self.state.emit_fmt(format_args!("    movq ${}, %{}", v, target_name));
                        } else {
                            self.state.emit_fmt(format_args!("    movabsq ${}, %{}", v, target_name));
                        }
                    }
                    IrConst::Zero => self.state.emit_fmt(format_args!("    xorq %{}, %{}", target_name, target_name)),
                    _ => {
                        // For float/i128 constants, fall back to loading to rax and moving
                        self.operand_to_rax(op);
                        self.state.emit_fmt(format_args!("    movq %rax, %{}", target_name));
                    }
                }
            }
            Operand::Value(v) => {
                if let Some(&reg) = self.reg_assignments.get(&v.0) {
                    if reg.0 != target.0 {
                        let src_name = callee_saved_name(reg);
                        self.state.emit_fmt(format_args!("    movq %{}, %{}", src_name, target_name));
                    }
                    // If same register, nothing to do
                } else if let Some(slot) = self.state.get_slot(v.0) {
                    if self.state.is_alloca(v.0) {
                        self.state.emit_fmt(format_args!("    leaq {}(%rbp), %{}", slot.0, target_name));
                    } else {
                        self.state.emit_fmt(format_args!("    movq {}(%rbp), %{}", slot.0, target_name));
                    }
                } else {
                    self.state.emit_fmt(format_args!("    xorq %{}, %{}", target_name, target_name));
                }
            }
        }
    }

    /// Load an operand into %rax. Uses the register cache to skip the load
    /// if the value is already in %rax.
    fn operand_to_rax(&mut self, op: &Operand) {
        match op {
            Operand::Const(c) => {
                self.state.reg_cache.invalidate_acc();
                match c {
                    IrConst::I8(v) => self.state.out.emit_instr_imm_reg("    movq", *v as i64, "rax"),
                    IrConst::I16(v) => self.state.out.emit_instr_imm_reg("    movq", *v as i64, "rax"),
                    IrConst::I32(v) => self.state.out.emit_instr_imm_reg("    movq", *v as i64, "rax"),
                    IrConst::I64(v) => {
                        if *v >= i32::MIN as i64 && *v <= i32::MAX as i64 {
                            self.state.out.emit_instr_imm_reg("    movq", *v, "rax");
                        } else {
                            self.state.out.emit_instr_imm_reg("    movabsq", *v, "rax");
                        }
                    }
                    IrConst::F32(v) => {
                        let bits = v.to_bits() as u64;
                        self.state.out.emit_instr_imm_reg("    movq", bits as i64, "rax");
                    }
                    IrConst::F64(v) => {
                        let bits = v.to_bits();
                        if bits == 0 {
                            self.state.emit("    xorq %rax, %rax");
                        } else {
                            self.state.out.emit_instr_imm_reg("    movabsq", bits as i64, "rax");
                        }
                    }
                    // LongDouble at computation level is treated as F64
                    IrConst::LongDouble(v, _) => {
                        let bits = v.to_bits();
                        if bits == 0 {
                            self.state.emit("    xorq %rax, %rax");
                        } else {
                            self.state.out.emit_instr_imm_reg("    movabsq", bits as i64, "rax");
                        }
                    }
                    IrConst::I128(v) => {
                        // Truncate to low 64 bits for rax-only path
                        let low = *v as i64;
                        if low >= i32::MIN as i64 && low <= i32::MAX as i64 {
                            self.state.out.emit_instr_imm_reg("    movq", low, "rax");
                        } else {
                            self.state.out.emit_instr_imm_reg("    movabsq", low, "rax");
                        }
                    }
                    IrConst::Zero => self.state.emit("    xorq %rax, %rax"),
                }
            }
            Operand::Value(v) => {
                let is_alloca = self.state.is_alloca(v.0);
                // Check cache: skip load if value is already in %rax
                if self.state.reg_cache.acc_has(v.0, is_alloca) {
                    return;
                }
                // Check register allocation: load from callee-saved register
                if let Some(&reg) = self.reg_assignments.get(&v.0) {
                    let reg_name = callee_saved_name(reg);
                    self.state.out.emit_instr_reg_reg("    movq", reg_name, "rax");
                    self.state.reg_cache.set_acc(v.0, false);
                } else if self.state.get_slot(v.0).is_some() {
                    self.value_to_reg(v, "rax");
                    self.state.reg_cache.set_acc(v.0, is_alloca);
                } else {
                    self.state.emit("    xorq %rax, %rax");
                    self.state.reg_cache.invalidate_acc();
                }
            }
        }
    }

    /// Store %rax to a value's location (register or stack slot).
    /// Register-only strategy: if the value has a callee-saved register assignment,
    /// store ONLY to the register (skip the stack write). This eliminates redundant
    /// memory stores for register-allocated values. Values without a register
    /// assignment are stored to their stack slot as before.
    fn store_rax_to(&mut self, dest: &Value) {
        if let Some(&reg) = self.reg_assignments.get(&dest.0) {
            // Value has a callee-saved register: store only to register, skip stack.
            let reg_name = callee_saved_name(reg);
            self.state.out.emit_instr_reg_reg("    movq", "rax", reg_name);
        } else if let Some(slot) = self.state.get_slot(dest.0) {
            // No register: store to stack slot.
            self.state.out.emit_instr_reg_rbp("    movq", "rax", slot.0);
        }
        // After storing to dest, %rax still holds dest's value
        self.state.reg_cache.set_acc(dest.0, false);
    }

    /// Load an operand directly into %rcx, avoiding the push/pop pattern.
    /// This is the key optimization: instead of loading to rax, pushing, loading
    /// the other operand to rax, moving rax->rcx, then popping rax, we load
    /// directly to rcx with a single instruction.
    fn operand_to_rcx(&mut self, op: &Operand) {
        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::I8(v) => self.state.out.emit_instr_imm_reg("    movq", *v as i64, "rcx"),
                    IrConst::I16(v) => self.state.out.emit_instr_imm_reg("    movq", *v as i64, "rcx"),
                    IrConst::I32(v) => self.state.out.emit_instr_imm_reg("    movq", *v as i64, "rcx"),
                    IrConst::I64(v) => {
                        if *v >= i32::MIN as i64 && *v <= i32::MAX as i64 {
                            self.state.out.emit_instr_imm_reg("    movq", *v, "rcx");
                        } else {
                            self.state.out.emit_instr_imm_reg("    movabsq", *v, "rcx");
                        }
                    }
                    IrConst::F32(v) => {
                        let bits = v.to_bits() as u64;
                        self.state.out.emit_instr_imm_reg("    movq", bits as i64, "rcx");
                    }
                    IrConst::F64(v) => {
                        let bits = v.to_bits();
                        if bits == 0 {
                            self.state.emit("    xorq %rcx, %rcx");
                        } else {
                            self.state.out.emit_instr_imm_reg("    movabsq", bits as i64, "rcx");
                        }
                    }
                    IrConst::LongDouble(v, _) => {
                        let bits = v.to_bits();
                        if bits == 0 {
                            self.state.emit("    xorq %rcx, %rcx");
                        } else {
                            self.state.out.emit_instr_imm_reg("    movabsq", bits as i64, "rcx");
                        }
                    }
                    IrConst::I128(v) => {
                        let low = *v as i64;
                        if low >= i32::MIN as i64 && low <= i32::MAX as i64 {
                            self.state.out.emit_instr_imm_reg("    movq", low, "rcx");
                        } else {
                            self.state.out.emit_instr_imm_reg("    movabsq", low, "rcx");
                        }
                    }
                    IrConst::Zero => self.state.emit("    xorq %rcx, %rcx"),
                }
            }
            Operand::Value(v) => {
                // Check register allocation: load from callee-saved register
                if let Some(&reg) = self.reg_assignments.get(&v.0) {
                    let reg_name = callee_saved_name(reg);
                    self.state.out.emit_instr_reg_reg("    movq", reg_name, "rcx");
                } else if self.state.get_slot(v.0).is_some() {
                    self.value_to_reg(v, "rcx");
                } else {
                    self.state.emit("    xorq %rcx, %rcx");
                }
            }
        }
    }

    /// Check if an operand is a small constant that fits in a 32-bit immediate.
    /// Returns the immediate value if it fits, None otherwise.
    fn const_as_imm32(op: &Operand) -> Option<i64> {
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
                if val >= i32::MIN as i64 && val <= i32::MAX as i64 {
                    Some(val)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Load a Value into a named register. For allocas, loads the address (leaq);
    /// for register-allocated values, copies from the callee-saved register;
    /// for regular values, loads the data (movq) from the stack slot.
    fn value_to_reg(&mut self, val: &Value, reg: &str) {
        // Check register allocation first (allocas are never register-allocated)
        if let Some(&phys_reg) = self.reg_assignments.get(&val.0) {
            let reg_name = callee_saved_name(phys_reg);
            if reg_name != reg {
                self.state.out.emit_instr_reg_reg("    movq", reg_name, reg);
            }
            return;
        }
        if let Some(slot) = self.state.get_slot(val.0) {
            if self.state.is_alloca(val.0) {
                if let Some(align) = self.state.alloca_over_align(val.0) {
                    // Over-aligned alloca: compute aligned address within the
                    // oversized stack slot. The slot has (align - 1) extra bytes
                    // to guarantee we can find an aligned address within it.
                    self.state.out.emit_instr_rbp_reg("    leaq", slot.0, reg);
                    self.state.out.emit_instr_imm_reg("    addq", (align - 1) as i64, reg);
                    self.state.out.emit_instr_imm_reg("    andq", -(align as i64), reg);
                } else {
                    self.state.out.emit_instr_rbp_reg("    leaq", slot.0, reg);
                }
            } else {
                self.state.out.emit_instr_rbp_reg("    movq", slot.0, reg);
            }
        }
    }

    // --- 128-bit integer helpers ---
    // Convention: 128-bit values use %rax (low 64 bits) and %rdx (high 64 bits).
    // Stack slots for 128-bit values are 16 bytes: slot(%rbp) = low, slot+8(%rbp) = high.

    /// Load a 128-bit operand into %rax (low) and %rdx (high).
    fn operand_to_rax_rdx(&mut self, op: &Operand) {
        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::I128(v) => {
                        let low = *v as u64 as i64;
                        let high = (*v >> 64) as u64 as i64;
                        if low == 0 {
                            self.state.emit("    xorq %rax, %rax");
                        } else if low >= i32::MIN as i64 && low <= i32::MAX as i64 {
                            self.state.emit_fmt(format_args!("    movq ${}, %rax", low));
                        } else {
                            self.state.emit_fmt(format_args!("    movabsq ${}, %rax", low));
                        }
                        if high == 0 {
                            self.state.emit("    xorq %rdx, %rdx");
                        } else if high >= i32::MIN as i64 && high <= i32::MAX as i64 {
                            self.state.emit_fmt(format_args!("    movq ${}, %rdx", high));
                        } else {
                            self.state.emit_fmt(format_args!("    movabsq ${}, %rdx", high));
                        }
                    }
                    IrConst::Zero => {
                        self.state.emit("    xorq %rax, %rax");
                        self.state.emit("    xorq %rdx, %rdx");
                    }
                    _ => {
                        // Smaller constant: load into rax, zero/sign-extend to rdx
                        self.operand_to_rax(op);
                        self.state.emit("    xorq %rdx, %rdx");
                    }
                }
            }
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    if self.state.is_alloca(v.0) {
                        // Alloca: load the address (not a 128-bit value itself)
                        self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rax", slot.0));
                        self.state.emit("    xorq %rdx, %rdx");
                    } else if self.state.is_i128_value(v.0) {
                        // 128-bit value in 16-byte stack slot
                        self.state.emit_fmt(format_args!("    movq {}(%rbp), %rax", slot.0));
                        self.state.emit_fmt(format_args!("    movq {}(%rbp), %rdx", slot.0 + 8));
                    } else {
                        // Non-i128 value (e.g. shift amount): load 8 bytes, zero-extend rdx
                        // Check register allocation first, since register-allocated values
                        // may not have their stack slot written.
                        if let Some(&reg) = self.reg_assignments.get(&v.0) {
                            let reg_name = callee_saved_name(reg);
                            self.state.emit_fmt(format_args!("    movq %{}, %rax", reg_name));
                        } else {
                            self.state.emit_fmt(format_args!("    movq {}(%rbp), %rax", slot.0));
                        }
                        self.state.emit("    xorq %rdx, %rdx");
                    }
                } else {
                    // No stack slot: check register allocation
                    if let Some(&reg) = self.reg_assignments.get(&v.0) {
                        let reg_name = callee_saved_name(reg);
                        self.state.emit_fmt(format_args!("    movq %{}, %rax", reg_name));
                        self.state.emit("    xorq %rdx, %rdx");
                    } else {
                        self.state.emit("    xorq %rax, %rax");
                        self.state.emit("    xorq %rdx, %rdx");
                    }
                }
            }
        }
    }

    /// Store %rax:%rdx (128-bit) to a value's 16-byte stack slot.
    fn store_rax_rdx_to(&mut self, dest: &Value) {
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.state.emit_fmt(format_args!("    movq %rax, {}(%rbp)", slot.0));
            self.state.emit_fmt(format_args!("    movq %rdx, {}(%rbp)", slot.0 + 8));
        }
        // rax holds only the low 64 bits of an i128, not a valid scalar IR value.
        self.state.reg_cache.invalidate_all();
    }

    /// Get the store instruction mnemonic for a given type.
    fn mov_store_for_type(ty: IrType) -> &'static str {
        match ty {
            IrType::I8 | IrType::U8 => "movb",
            IrType::I16 | IrType::U16 => "movw",
            IrType::I32 | IrType::U32 | IrType::F32 => "movl",
            _ => "movq",
        }
    }

    /// Get the load instruction mnemonic for a given type.
    fn mov_load_for_type(ty: IrType) -> &'static str {
        match ty {
            IrType::I8 => "movsbq",
            IrType::U8 => "movzbq",
            IrType::I16 => "movswq",
            IrType::U16 => "movzwq",
            IrType::I32 => "movslq",
            IrType::U32 | IrType::F32 => "movl",     // movl zero-extends to 64-bit implicitly
            _ => "movq",
        }
    }

    /// Destination register for loads. U32/F32 use movl which needs %eax.
    fn load_dest_reg(ty: IrType) -> &'static str {
        match ty {
            IrType::U32 | IrType::F32 => "%eax",
            _ => "%rax",
        }
    }

    /// Map base register name + type to sized sub-register.
    fn reg_for_type(base_reg: &str, ty: IrType) -> &'static str {
        let (r8, r16, r32, r64) = match base_reg {
            "rax" => ("al", "ax", "eax", "rax"),
            "rcx" => ("cl", "cx", "ecx", "rcx"),
            "rdx" => ("dl", "dx", "edx", "rdx"),
            "rdi" => ("dil", "di", "edi", "rdi"),
            "rsi" => ("sil", "si", "esi", "rsi"),
            "r8"  => ("r8b", "r8w", "r8d", "r8"),
            "r9"  => ("r9b", "r9w", "r9d", "r9"),
            _ => return "rax",
        };
        match ty {
            IrType::I8 | IrType::U8 => r8,
            IrType::I16 | IrType::U16 => r16,
            IrType::I32 | IrType::U32 | IrType::F32 => r32,
            _ => r64,
        }
    }

    /// Load full-precision x87 80-bit from the given memory slot and convert to integer.
    /// Uses `fldt` directly from memory instead of going through the f64 intermediate
    /// in %rax, preserving all 64 mantissa bits for the conversion.
    fn emit_f128_to_int_from_memory(&mut self, addr: &crate::backend::state::SlotAddr, to_ty: IrType) {
        use crate::backend::state::SlotAddr;
        match addr {
            SlotAddr::Direct(slot) => {
                self.state.emit_fmt(format_args!("    fldt {}(%rbp)", slot.0));
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
    fn emit_f128_st0_to_int(&mut self, to_ty: IrType) {
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
            self.state.emit_fmt(format_args!("    jbe {}", big_label));
            // Small case: value < 2^63
            self.state.emit("    fisttpq (%rsp)");
            self.state.emit("    movq (%rsp), %rax");
            self.state.emit("    addq $8, %rsp");
            self.state.emit_fmt(format_args!("    jmp {}", done_label));
            // Big case: value >= 2^63
            self.state.emit_fmt(format_args!("{}:", big_label));
            self.state.emit("    movabsq $4890909195324358656, %rcx");
            self.state.emit("    movq %rcx, (%rsp)");
            self.state.emit("    fldl (%rsp)"); // ST0 = 2^63, ST1 = value
            self.state.emit("    fsubrp %st, %st(1)"); // ST0 = value - 2^63
            self.state.emit("    fisttpq (%rsp)");
            self.state.emit("    movq (%rsp), %rax");
            self.state.emit("    addq $8, %rsp");
            self.state.emit("    movabsq $9223372036854775808, %rcx");
            self.state.emit("    addq %rcx, %rax");
            self.state.emit_fmt(format_args!("{}:", done_label));
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
    fn emit_cast_instrs_x86(&mut self, from_ty: IrType, to_ty: IrType) {
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
                self.state.emit_fmt(format_args!("    js {}", big_label));
                // Positive (< 2^63): FILD directly
                self.state.emit("    subq $8, %rsp");
                self.state.emit("    movq %rax, (%rsp)");
                self.state.emit("    fildq (%rsp)");
                self.state.emit("    fstpl (%rsp)");
                self.state.emit("    movq (%rsp), %rax");
                self.state.emit("    addq $8, %rsp");
                self.state.emit_fmt(format_args!("    jmp {}", done_label));
                self.state.emit_fmt(format_args!("{}:", big_label));
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
                self.state.emit_fmt(format_args!("{}:", done_label));
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
                    self.state.emit_fmt(format_args!("    jbe {}", big_label)); // if 2^63 <= value
                    // Small case: value < 2^63
                    self.state.emit("    fisttpq (%rsp)");
                    self.state.emit("    movq (%rsp), %rax");
                    self.state.emit("    addq $8, %rsp");
                    self.state.emit_fmt(format_args!("    jmp {}", done_label));
                    // Big case: value >= 2^63
                    self.state.emit_fmt(format_args!("{}:", big_label));
                    self.state.emit("    movabsq $4890909195324358656, %rcx");
                    self.state.emit("    movq %rcx, (%rsp)");
                    self.state.emit("    fldl (%rsp)");   // ST0 = 2^63, ST1 = value
                    self.state.emit("    fsubrp %st, %st(1)"); // ST0 = value - 2^63
                    self.state.emit("    fisttpq (%rsp)");
                    self.state.emit("    movq (%rsp), %rax");
                    self.state.emit("    addq $8, %rsp");
                    self.state.emit("    movabsq $9223372036854775808, %rcx");
                    self.state.emit("    addq %rcx, %rax");
                    self.state.emit_fmt(format_args!("{}:", done_label));
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
                        self.state.emit_fmt(format_args!("    jae {}", big_label));
                        self.state.emit("    cvttsd2siq %xmm0, %rax");
                        self.state.emit_fmt(format_args!("    jmp {}", done_label));
                        self.state.emit_fmt(format_args!("{}:", big_label));
                        self.state.emit("    subsd %xmm1, %xmm0");
                        self.state.emit("    cvttsd2siq %xmm0, %rax");
                        self.state.emit("    movabsq $9223372036854775808, %rcx"); // 2^63 as int
                        self.state.emit("    addq %rcx, %rax");
                        self.state.emit_fmt(format_args!("{}:", done_label));
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
                    self.state.emit_fmt(format_args!("    js {}", big_label));
                    if to_f64 {
                        self.state.emit("    cvtsi2sdq %rax, %xmm0");
                    } else {
                        self.state.emit("    cvtsi2ssq %rax, %xmm0");
                    }
                    self.state.emit_fmt(format_args!("    jmp {}", done_label));
                    self.state.emit_fmt(format_args!("{}:", big_label));
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
                    self.state.emit_fmt(format_args!("{}:", done_label));
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
        }
    }

    /// Get the type suffix for lock-prefixed instructions (b, w, l, q).
    fn type_suffix(ty: IrType) -> &'static str {
        match ty {
            IrType::I8 | IrType::U8 => "b",
            IrType::I16 | IrType::U16 => "w",
            IrType::I32 | IrType::U32 => "l",
            _ => "q",
        }
    }

    /// Emit a cmpxchg-based loop for atomic sub/and/or/xor/nand.
    /// Expects: rax = operand val, rcx = ptr address.
    /// After: rax = old value.
    fn emit_x86_atomic_op_loop(&mut self, ty: IrType, op: &str) {
        // Save val to r8
        self.state.emit("    movq %rax, %r8"); // r8 = val
        // Load old value
        let load_instr = Self::mov_load_for_type(ty);
        let load_dest = Self::load_dest_reg(ty);
        self.state.emit_fmt(format_args!("    {} (%rcx), {}", load_instr, load_dest));
        // Loop: rax = old, compute new = op(old, val), try cmpxchg
        let label_id = self.state.next_label_id();
        let loop_label = format!(".Latomic_loop_{}", label_id);
        self.state.emit_fmt(format_args!("{}:", loop_label));
        // rdx = rax (old)
        self.state.emit("    movq %rax, %rdx");
        // Apply operation: rdx = op(rdx, r8)
        let size_suffix = Self::type_suffix(ty);
        let rdx_reg = Self::reg_for_type("rdx", ty);
        let r8_reg = match ty {
            IrType::I8 | IrType::U8 => "r8b",
            IrType::I16 | IrType::U16 => "r8w",
            IrType::I32 | IrType::U32 => "r8d",
            _ => "r8",
        };
        match op {
            "sub" => self.state.emit_fmt(format_args!("    sub{} %{}, %{}", size_suffix, r8_reg, rdx_reg)),
            "and" => self.state.emit_fmt(format_args!("    and{} %{}, %{}", size_suffix, r8_reg, rdx_reg)),
            "or"  => self.state.emit_fmt(format_args!("    or{} %{}, %{}", size_suffix, r8_reg, rdx_reg)),
            "xor" => self.state.emit_fmt(format_args!("    xor{} %{}, %{}", size_suffix, r8_reg, rdx_reg)),
            "nand" => {
                self.state.emit_fmt(format_args!("    and{} %{}, %{}", size_suffix, r8_reg, rdx_reg));
                self.state.emit_fmt(format_args!("    not{} %{}", size_suffix, rdx_reg));
            }
            _ => {}
        }
        // Try cmpxchg: if [rcx] == rax (old), set [rcx] = rdx (new), else rax = [rcx]
        self.state.emit_fmt(format_args!("    lock cmpxchg{} %{}, (%rcx)", size_suffix, rdx_reg));
        self.state.emit_fmt(format_args!("    jne {}", loop_label));
        // rax = old value on success
    }

    /// Load i128 operands for binary ops: lhs → rax:rdx, rhs → rcx:rsi.
    fn prep_i128_binop(&mut self, lhs: &Operand, rhs: &Operand) {
        self.operand_to_rax_rdx(lhs);
        self.state.emit("    pushq %rdx");
        self.state.emit("    pushq %rax");
        self.operand_to_rax_rdx(rhs);
        self.state.emit("    movq %rax, %rcx");
        self.state.emit("    movq %rdx, %rsi");
        self.state.emit("    popq %rax");
        self.state.emit("    popq %rdx");
        self.state.reg_cache.invalidate_all();
    }

    /// Load an operand value into any GP register (returned as string).
    /// Uses rcx as the scratch register.
    fn operand_to_reg(&mut self, op: &Operand, reg: &str) {
        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::I8(v) => self.state.emit_fmt(format_args!("    movq ${}, %{}", *v as i64, reg)),
                    IrConst::I16(v) => self.state.emit_fmt(format_args!("    movq ${}, %{}", *v as i64, reg)),
                    IrConst::I32(v) => self.state.emit_fmt(format_args!("    movq ${}, %{}", *v as i64, reg)),
                    IrConst::I64(v) => {
                        if *v >= i32::MIN as i64 && *v <= i32::MAX as i64 {
                            self.state.emit_fmt(format_args!("    movq ${}, %{}", v, reg));
                        } else {
                            self.state.emit_fmt(format_args!("    movabsq ${}, %{}", v, reg));
                        }
                    }
                    _ => self.state.emit_fmt(format_args!("    xorq %{0}, %{0}", reg)),
                }
            }
            Operand::Value(v) => {
                self.value_to_reg(v, reg);
            }
        }
    }

    /// Load a float operand into %xmm0. Handles both Value operands (from stack)
    /// and float constants (loaded via their bit pattern into rax first).
    fn float_operand_to_xmm0(&mut self, op: &Operand, is_f32: bool) {
        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::F64(v) => {
                        let bits = v.to_bits() as i64;
                        if bits == 0 {
                            self.state.emit("    xorpd %xmm0, %xmm0");
                        } else if bits >= i32::MIN as i64 && bits <= i32::MAX as i64 {
                            self.state.emit_fmt(format_args!("    movq ${}, %rax", bits));
                            self.state.emit("    movq %rax, %xmm0");
                        } else {
                            self.state.emit_fmt(format_args!("    movabsq ${}, %rax", bits));
                            self.state.emit("    movq %rax, %xmm0");
                        }
                    }
                    IrConst::F32(v) => {
                        let bits = v.to_bits() as i32;
                        if bits == 0 {
                            self.state.emit("    xorps %xmm0, %xmm0");
                        } else {
                            self.state.emit_fmt(format_args!("    movl ${}, %eax", bits));
                            self.state.emit("    movd %eax, %xmm0");
                        }
                    }
                    _ => {
                        // Integer or other constants - load to rax and move to xmm
                        self.operand_to_reg(op, "rax");
                        if is_f32 {
                            self.state.emit("    movd %eax, %xmm0");
                        } else {
                            self.state.emit("    movq %rax, %xmm0");
                        }
                    }
                }
            }
            Operand::Value(_) => {
                // Load from stack slot to rax, then to xmm0
                self.operand_to_reg(op, "rax");
                if is_f32 {
                    self.state.emit("    movd %eax, %xmm0");
                } else {
                    self.state.emit("    movq %rax, %xmm0");
                }
            }
        }
    }

    /// Emit SSE binary 128-bit op: load xmm0 from arg0 ptr, xmm1 from arg1 ptr,
    /// apply the given SSE instruction, store result xmm0 to dest_ptr.
    fn emit_sse_binary_128(&mut self, dest_ptr: &Value, args: &[Operand], sse_inst: &str) {
        self.operand_to_reg(&args[0], "rax");
        self.state.emit("    movdqu (%rax), %xmm0");
        self.operand_to_reg(&args[1], "rcx");
        self.state.emit("    movdqu (%rcx), %xmm1");
        self.state.emit_fmt(format_args!("    {} %xmm1, %xmm0", sse_inst));
        self.value_to_reg(dest_ptr, "rax");
        self.state.emit("    movdqu %xmm0, (%rax)");
    }

    fn emit_intrinsic_impl(&mut self, dest: &Option<Value>, op: &IntrinsicOp, dest_ptr: &Option<Value>, args: &[Operand]) {
        match op {
            IntrinsicOp::Lfence => { self.state.emit("    lfence"); }
            IntrinsicOp::Mfence => { self.state.emit("    mfence"); }
            IntrinsicOp::Sfence => { self.state.emit("    sfence"); }
            IntrinsicOp::Pause => { self.state.emit("    pause"); }
            IntrinsicOp::Clflush => {
                // args[0] = pointer to flush
                self.operand_to_reg(&args[0], "rax");
                self.state.emit("    clflush (%rax)");
            }
            IntrinsicOp::Movnti => {
                if let Some(ptr) = dest_ptr {
                    self.operand_to_reg(&args[0], "rcx");
                    self.value_to_reg(ptr, "rax");
                    self.state.emit("    movnti %ecx, (%rax)");
                }
            }
            IntrinsicOp::Movnti64 => {
                if let Some(ptr) = dest_ptr {
                    self.operand_to_reg(&args[0], "rcx");
                    self.value_to_reg(ptr, "rax");
                    self.state.emit("    movnti %rcx, (%rax)");
                }
            }
            IntrinsicOp::Movntdq => {
                if let Some(ptr) = dest_ptr {
                    self.operand_to_reg(&args[0], "rcx");
                    self.state.emit("    movdqu (%rcx), %xmm0");
                    self.value_to_reg(ptr, "rax");
                    self.state.emit("    movntdq %xmm0, (%rax)");
                }
            }
            IntrinsicOp::Movntpd => {
                if let Some(ptr) = dest_ptr {
                    self.operand_to_reg(&args[0], "rcx");
                    self.state.emit("    movupd (%rcx), %xmm0");
                    self.value_to_reg(ptr, "rax");
                    self.state.emit("    movntpd %xmm0, (%rax)");
                }
            }
            IntrinsicOp::Loaddqu => {
                if let Some(dptr) = dest_ptr {
                    self.operand_to_reg(&args[0], "rax");
                    self.state.emit("    movdqu (%rax), %xmm0");
                    self.value_to_reg(dptr, "rax");
                    self.state.emit("    movdqu %xmm0, (%rax)");
                }
            }
            IntrinsicOp::Storedqu => {
                if let Some(ptr) = dest_ptr {
                    self.operand_to_reg(&args[0], "rcx");
                    self.state.emit("    movdqu (%rcx), %xmm0");
                    self.value_to_reg(ptr, "rax");
                    self.state.emit("    movdqu %xmm0, (%rax)");
                }
            }
            IntrinsicOp::Pcmpeqb128 => {
                if let Some(dptr) = dest_ptr {
                    self.emit_sse_binary_128(dptr, args, "pcmpeqb");
                }
            }
            IntrinsicOp::Pcmpeqd128 => {
                if let Some(dptr) = dest_ptr {
                    self.emit_sse_binary_128(dptr, args, "pcmpeqd");
                }
            }
            IntrinsicOp::Psubusb128 => {
                if let Some(dptr) = dest_ptr {
                    self.emit_sse_binary_128(dptr, args, "psubusb");
                }
            }
            IntrinsicOp::Por128 => {
                if let Some(dptr) = dest_ptr {
                    self.emit_sse_binary_128(dptr, args, "por");
                }
            }
            IntrinsicOp::Pand128 => {
                if let Some(dptr) = dest_ptr {
                    self.emit_sse_binary_128(dptr, args, "pand");
                }
            }
            IntrinsicOp::Pxor128 => {
                if let Some(dptr) = dest_ptr {
                    self.emit_sse_binary_128(dptr, args, "pxor");
                }
            }
            IntrinsicOp::Pmovmskb128 => {
                self.operand_to_reg(&args[0], "rax");
                self.state.emit("    movdqu (%rax), %xmm0");
                self.state.emit("    pmovmskb %xmm0, %eax");
                if let Some(d) = dest {
                    self.store_rax_to(d);
                }
            }
            IntrinsicOp::SetEpi8 => {
                if let Some(dptr) = dest_ptr {
                    self.operand_to_reg(&args[0], "rax");
                    self.state.emit("    movd %eax, %xmm0");
                    self.state.emit("    punpcklbw %xmm0, %xmm0");
                    self.state.emit("    punpcklwd %xmm0, %xmm0");
                    self.state.emit("    pshufd $0, %xmm0, %xmm0");
                    self.value_to_reg(dptr, "rax");
                    self.state.emit("    movdqu %xmm0, (%rax)");
                }
            }
            IntrinsicOp::SetEpi32 => {
                if let Some(dptr) = dest_ptr {
                    self.operand_to_reg(&args[0], "rax");
                    self.state.emit("    movd %eax, %xmm0");
                    self.state.emit("    pshufd $0, %xmm0, %xmm0");
                    self.value_to_reg(dptr, "rax");
                    self.state.emit("    movdqu %xmm0, (%rax)");
                }
            }
            IntrinsicOp::Crc32_8 | IntrinsicOp::Crc32_16
            | IntrinsicOp::Crc32_32 | IntrinsicOp::Crc32_64 => {
                self.operand_to_reg(&args[0], "rax");
                self.operand_to_reg(&args[1], "rcx");
                let inst = match op {
                    IntrinsicOp::Crc32_8  => "crc32b %cl, %eax",
                    IntrinsicOp::Crc32_16 => "crc32w %cx, %eax",
                    IntrinsicOp::Crc32_32 => "crc32l %ecx, %eax",
                    IntrinsicOp::Crc32_64 => "crc32q %rcx, %rax",
                    _ => unreachable!(),
                };
                self.state.emit_fmt(format_args!("    {}", inst));
                if let Some(d) = dest {
                    self.store_rax_to(d);
                }
            }
            IntrinsicOp::FrameAddress => {
                // __builtin_frame_address(0): return current frame pointer (rbp)
                self.state.emit("    movq %rbp, %rax");
                if let Some(d) = dest {
                    self.store_rax_to(d);
                }
            }
            IntrinsicOp::ReturnAddress => {
                // __builtin_return_address(0): return address is at (%rbp)+8
                self.state.emit("    movq 8(%rbp), %rax");
                if let Some(d) = dest {
                    self.store_rax_to(d);
                }
            }
            IntrinsicOp::SqrtF64 => {
                // sqrtsd: scalar double-precision square root
                self.float_operand_to_xmm0(&args[0], false);
                self.state.emit("    sqrtsd %xmm0, %xmm0");
                self.state.emit("    movq %xmm0, %rax");
                if let Some(d) = dest {
                    self.store_rax_to(d);
                }
            }
            IntrinsicOp::SqrtF32 => {
                // sqrtss: scalar single-precision square root
                self.float_operand_to_xmm0(&args[0], true);
                self.state.emit("    sqrtss %xmm0, %xmm0");
                self.state.emit("    movd %xmm0, %eax");
                if let Some(d) = dest {
                    self.store_rax_to(d);
                }
            }
            IntrinsicOp::FabsF64 => {
                // Clear sign bit for double-precision absolute value
                self.float_operand_to_xmm0(&args[0], false);
                self.state.emit("    movabsq $0x7FFFFFFFFFFFFFFF, %rcx");
                self.state.emit("    movq %rcx, %xmm1");
                self.state.emit("    andpd %xmm1, %xmm0");
                self.state.emit("    movq %xmm0, %rax");
                if let Some(d) = dest {
                    self.store_rax_to(d);
                }
            }
            IntrinsicOp::FabsF32 => {
                // Clear sign bit for single-precision absolute value
                self.float_operand_to_xmm0(&args[0], true);
                self.state.emit("    movl $0x7FFFFFFF, %ecx");
                self.state.emit("    movd %ecx, %xmm1");
                self.state.emit("    andps %xmm1, %xmm0");
                self.state.emit("    movd %xmm0, %eax");
                if let Some(d) = dest {
                    self.store_rax_to(d);
                }
            }
        }
    }

    /// Load an F128 operand into x87 ST(0), using full 80-bit precision when available.
    /// This is used for F128 arithmetic and for pushing F128 call arguments.
    fn emit_f128_load_to_x87(&mut self, operand: &Operand) {
        match operand {
            Operand::Const(ref c) => {
                match c {
                    IrConst::LongDouble(_, raw) => {
                        // Full-precision x87 bytes available; push to stack and fldt
                        let lo = u64::from_le_bytes(raw[0..8].try_into().unwrap());
                        let hi = u16::from_le_bytes(raw[8..10].try_into().unwrap());
                        self.state.emit("    subq $16, %rsp");
                        self.state.emit_fmt(format_args!("    movabsq ${}, %rax", lo as i64));
                        self.state.emit("    movq %rax, (%rsp)");
                        self.state.emit_fmt(format_args!("    movq ${}, %rax", hi as i64));
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
                        self.state.emit_fmt(format_args!("    movabsq ${}, %rax", bits as i64));
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
                        self.state.emit_fmt(format_args!("    fldt {}(%rbp)", slot.0));
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
                        self.state.emit_fmt(format_args!("    fldt {}(%rbp)", slot.0));
                    } else {
                        // Regular f64 value in slot; push and fldl
                        self.state.emit_fmt(format_args!("    movq {}(%rbp), %rax", slot.0));
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

const X86_ARG_REGS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

impl ArchCodegen for X86Codegen {
    fn state(&mut self) -> &mut CodegenState { &mut self.state }
    fn state_ref(&self) -> &CodegenState { &self.state }
    fn ptr_directive(&self) -> PtrDirective { PtrDirective::Quad }

    fn jump_mnemonic(&self) -> &'static str { "jmp" }
    fn trap_instruction(&self) -> &'static str { "ud2" }

    fn emit_branch_nonzero(&mut self, label: &str) {
        self.state.emit("    testq %rax, %rax");
        self.state.emit_fmt(format_args!("    jne {}", label));
    }

    fn emit_jump_indirect(&mut self) {
        if self.state.indirect_branch_thunk {
            self.state.emit("    jmp __x86_indirect_thunk_rax");
        } else {
            self.state.emit("    jmpq *%rax");
        }
    }

    fn emit_switch_case_branch(&mut self, case_val: i64, label: &str) {
        if case_val >= i32::MIN as i64 && case_val <= i32::MAX as i64 {
            self.state.emit_fmt(format_args!("    cmpq ${}, %rax", case_val));
        } else {
            // Value doesn't fit in sign-extended 32-bit immediate; load into %rcx first
            self.state.emit_fmt(format_args!("    movabsq ${}, %rcx", case_val));
            self.state.emit("    cmpq %rcx, %rax");
        }
        self.state.emit_fmt(format_args!("    je {}", label));
    }

    fn emit_switch_jump_table(&mut self, val: &Operand, cases: &[(i64, BlockId)], default: &BlockId) {
        let min_val = cases.iter().map(|&(v, _)| v).min().unwrap();
        let max_val = cases.iter().map(|&(v, _)| v).max().unwrap();
        let range = (max_val - min_val + 1) as usize;

        // Build the table
        let mut table = vec![*default; range];
        for &(case_val, target) in cases {
            let idx = (case_val - min_val) as usize;
            table[idx] = target;
        }

        let table_label = self.state.fresh_label("jt");
        let default_label = default.as_label();

        // Load switch value into %rax
        self.operand_to_rax(val);

        // Subtract min_val to normalize index
        if min_val != 0 {
            if min_val >= i32::MIN as i64 && min_val <= i32::MAX as i64 {
                self.state.emit_fmt(format_args!("    subq ${}, %rax", min_val));
            } else {
                self.state.emit_fmt(format_args!("    movabsq ${}, %rcx", min_val));
                self.state.emit("    subq %rcx, %rax");
            }
        }
        // Range check (unsigned): if index >= range, jump to default
        if (range as i64) >= i32::MIN as i64 && (range as i64) <= i32::MAX as i64 {
            self.state.emit_fmt(format_args!("    cmpq ${}, %rax", range));
        } else {
            self.state.emit_fmt(format_args!("    movabsq ${}, %rcx", range));
            self.state.emit("    cmpq %rcx, %rax");
        }
        self.state.emit_fmt(format_args!("    jae {}", default_label));

        // Jump through the table: jmp *table(,%rax,8)
        self.state.emit_fmt(format_args!("    leaq {}(%rip), %rcx", table_label));
        self.state.emit("    movq (%rcx,%rax,8), %rcx");
        self.state.emit("    jmp *%rcx");

        // Emit jump table in .rodata
        self.state.emit(".section .rodata");
        self.state.emit(".align 8");
        self.state.emit_fmt(format_args!("{}:", table_label));
        for target in &table {
            let target_label = target.as_label();
            self.state.emit_fmt(format_args!("    .quad {}", target_label));
        }
        // Restore the function's text section (may be custom, e.g. .init.text)
        let sect = self.state.current_text_section.clone();
        self.state.emit_fmt(format_args!(".section {},\"ax\",@progbits", sect));

        self.state.reg_cache.invalidate_all();
    }

    fn calculate_stack_space(&mut self, func: &IrFunction) -> i64 {
        // Track variadic function info
        self.is_variadic = func.is_variadic;
        // Count named params by classification, matching classify_params() logic.
        // On x86-64 System V ABI:
        //   - large structs (>16 bytes) are ALWAYS passed on stack
        //   - small structs (<=16 bytes) consume 1-2 GP register slots
        //   - long double (F128/x87) is ALWAYS passed on stack (16 bytes each)
        //   - float/double passed in XMM registers (up to 8)
        //   - integer/pointer passed in GP registers (up to 6)
        {
            let mut gp_count = 0usize;
            let mut fp_count = 0usize;
            // First pass: count total GP/FP register usage to know classification
            for p in &func.params {
                if let Some(size) = p.struct_size {
                    if size <= 16 {
                        let regs_needed = if size <= 8 { 1 } else { 2 };
                        gp_count += regs_needed;
                    }
                    // Large structs don't consume registers
                } else if p.ty.is_long_double() {
                    // Long double always on stack, no register
                } else if p.ty.is_float() {
                    fp_count += 1;
                } else {
                    gp_count += 1;
                }
            }
            self.num_named_int_params = gp_count;
            self.num_named_fp_params = fp_count;

            // Second pass: simulate forward stack layout of all stack-passed named params,
            // including alignment padding for F128/I128 (matches compute_stack_arg_padding).
            // This is necessary because the caller inserts alignment padding before 16-byte
            // aligned args, and we must account for it in overflow_arg_area.
            let mut stack_offset = 0usize;
            let mut gp_idx = 0usize;
            let mut fp_idx = 0usize;
            for p in &func.params {
                if let Some(size) = p.struct_size {
                    if size <= 16 {
                        let regs_needed = if size <= 8 { 1 } else { 2 };
                        if gp_idx + regs_needed <= 6 {
                            gp_idx += regs_needed;
                        } else {
                            // Overflows to stack
                            stack_offset += (size + 7) & !7;
                            gp_idx = 6; // All remaining GP go to stack
                        }
                    } else {
                        // Large struct always on stack (8-byte aligned)
                        stack_offset += (size + 7) & !7;
                    }
                } else if p.ty.is_long_double() {
                    // F128: always on stack, 16-byte aligned
                    let align_pad = (16 - (stack_offset % 16)) % 16;
                    stack_offset += align_pad;
                    stack_offset += 16;
                } else if p.ty.is_128bit() {
                    // I128: check if fits in GP regs, else stack with 16-byte alignment
                    if gp_idx + 2 <= 6 {
                        gp_idx += 2;
                    } else {
                        let align_pad = (16 - (stack_offset % 16)) % 16;
                        stack_offset += align_pad;
                        stack_offset += 16;
                        gp_idx = 6;
                    }
                } else if p.ty.is_float() {
                    if fp_idx < 8 {
                        fp_idx += 1;
                    } else {
                        stack_offset += 8;
                    }
                } else {
                    if gp_idx < 6 {
                        gp_idx += 1;
                    } else {
                        stack_offset += 8;
                    }
                }
            }
            self.num_named_stack_bytes = stack_offset;
        }

        // Run register allocator BEFORE stack space computation so we can
        // skip allocating stack slots for values assigned to registers.
        // This significantly reduces frame sizes (e.g., 160 -> 16 bytes for
        // simple recursive functions), which fixes postgres plpgsql stack
        // depth tests and improves overall performance.
        let config = RegAllocConfig {
            available_regs: X86_CALLEE_SAVED.to_vec(),
        };
        let alloc_result = regalloc::allocate_registers(func, &config);
        self.reg_assignments = alloc_result.assignments;
        self.used_callee_saved = alloc_result.used_regs;

        // Scan inline asm instructions for callee-saved register usage.
        // When inline asm uses specific register constraints (e.g., "b" for rbx)
        // or lists callee-saved registers in clobbers, those registers must be
        // saved/restored in the function prologue/epilogue to preserve the ABI.
        collect_inline_asm_callee_saved_x86(func, &mut self.used_callee_saved);

        // Build set of register-assigned value IDs to skip stack slot allocation.
        let reg_assigned: FxHashSet<u32> = self.reg_assignments.keys().copied().collect();

        let mut space = calculate_stack_space_common(&mut self.state, func, 0, |space, alloc_size, align| {
            // x86 uses negative offsets from rbp
            // Honor alignment: round up space to alignment boundary before allocating
            let effective_align = if align > 0 { align.max(8) } else { 8 };
            let alloc = (alloc_size + 7) & !7;
            let new_space = ((space + alloc + effective_align - 1) / effective_align) * effective_align;
            (-new_space, new_space)
        }, &reg_assigned);

        // For variadic functions, reserve space for the register save area:
        // 48 bytes for 6 integer registers (rdi, rsi, rdx, rcx, r8, r9)
        // +128 bytes for 8 XMM registers (xmm0-xmm7, 16 bytes each) when SSE is enabled
        if func.is_variadic {
            if self.no_sse {
                space += 48; // Only GP registers, no XMM saves
            } else {
                space += 176; // GP + XMM registers
            }
            self.reg_save_area_offset = -space;
        }

        // Add space for saving callee-saved registers.
        // Each callee-saved register needs 8 bytes on the stack.
        let callee_save_space = (self.used_callee_saved.len() as i64) * 8;
        space + callee_save_space
    }

    fn aligned_frame_size(&self, raw_space: i64) -> i64 {
        if raw_space > 0 { (raw_space + 15) & !15 } else { 0 }
    }

    fn emit_prologue(&mut self, func: &IrFunction, frame_size: i64) {
        self.current_return_type = func.return_type;
        // Emit endbr64 for CET/IBT (-fcf-protection=branch).
        // This must be the first instruction at the function entry point.
        // When -fpatchable-function-entry=N,M is also active, the NOP padding
        // is placed before the function label by generation.rs, so endbr64
        // remains the first instruction at the actual entry address.
        if self.state.cf_protection_branch {
            self.state.emit("    endbr64");
        }
        self.state.emit("    pushq %rbp");
        self.state.emit("    movq %rsp, %rbp");
        if frame_size > 0 {
            const PAGE_SIZE: i64 = 4096;
            if frame_size > PAGE_SIZE {
                // Stack probing: for large frames, touch each page so the kernel
                // can grow the stack mapping. Without this, a single large sub
                // can skip guard pages and cause a segfault.
                let probe_label = self.state.fresh_label("stack_probe");
                self.state.emit_fmt(format_args!("    movq ${}, %r11", frame_size));
                self.state.emit_fmt(format_args!("{}:", probe_label));
                self.state.emit_fmt(format_args!("    subq ${}, %rsp", PAGE_SIZE));
                self.state.emit("    orl $0, (%rsp)");
                self.state.emit_fmt(format_args!("    subq ${}, %r11", PAGE_SIZE));
                self.state.emit_fmt(format_args!("    cmpq ${}, %r11", PAGE_SIZE));
                self.state.emit_fmt(format_args!("    ja {}", probe_label));
                self.state.emit("    subq %r11, %rsp");
                self.state.emit("    orl $0, (%rsp)");
            } else {
                self.state.emit_fmt(format_args!("    subq ${}, %rsp", frame_size));
            }
        }

        // Save callee-saved registers used by the register allocator.
        // They are saved at the bottom of the frame (highest negative offsets from rbp).
        let used_regs = self.used_callee_saved.clone();
        for (i, &reg) in used_regs.iter().enumerate() {
            let offset = -(frame_size as i64) + (i as i64 * 8);
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    movq %{}, {}(%rbp)", reg_name, offset));
        }

        // For variadic functions, save all arg registers to the register save area.
        // This allows va_arg to retrieve register-passed arguments.
        // Layout: [0..47] = integer regs, [48..175] = XMM regs (16 bytes each, only when SSE enabled)
        if func.is_variadic {
            let base = self.reg_save_area_offset;
            // Save 6 integer argument registers
            self.state.emit_fmt(format_args!("    movq %rdi, {}(%rbp)", base));
            self.state.emit_fmt(format_args!("    movq %rsi, {}(%rbp)", base + 8));
            self.state.emit_fmt(format_args!("    movq %rdx, {}(%rbp)", base + 16));
            self.state.emit_fmt(format_args!("    movq %rcx, {}(%rbp)", base + 24));
            self.state.emit_fmt(format_args!("    movq %r8, {}(%rbp)", base + 32));
            self.state.emit_fmt(format_args!("    movq %r9, {}(%rbp)", base + 40));
            // Save 8 XMM argument registers (16 bytes each) — only when SSE is enabled.
            // With -mno-sse, XMM registers are not used for argument passing.
            if !self.no_sse {
                for i in 0..8i64 {
                    self.state.emit_fmt(format_args!("    movdqu %xmm{}, {}(%rbp)", i, base + 48 + i * 16));
                }
            }
        }
    }

    fn emit_epilogue(&mut self, frame_size: i64) {
        // Restore callee-saved registers before frame teardown.
        let used_regs = self.used_callee_saved.clone();
        for (i, &reg) in used_regs.iter().enumerate() {
            let offset = -(frame_size as i64) + (i as i64 * 8);
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    movq {}(%rbp), %{}", offset, reg_name));
        }
        self.state.emit("    movq %rbp, %rsp");
        self.state.emit("    popq %rbp");
    }

    fn emit_store_params(&mut self, func: &IrFunction) {
        let xmm_regs = ["xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"];
        // Use shared parameter classification (same ABI config as emit_call).
        let config = self.call_abi_config();
        let param_classes = classify_params(func, &config);
        // Stack-passed parameters start at 16(%rbp) (after saved rbp + return addr).
        let stack_base: i64 = 16;

        for (i, _param) in func.params.iter().enumerate() {
            let class = param_classes[i];

            // Find the alloca for this parameter.
            let (slot, ty) = if let Some((dest, ty)) = find_param_alloca(func, i) {
                if let Some(slot) = self.state.get_slot(dest.0) {
                    (slot, ty)
                } else {
                    continue; // Slot not assigned, skip.
                }
            } else {
                continue; // No alloca: classification already accounts for this param.
            };

            match class {
                ParamClass::IntReg { reg_idx } => {
                    let store_instr = Self::mov_store_for_type(ty);
                    let reg = Self::reg_for_type(X86_ARG_REGS[reg_idx], ty);
                    self.state.emit_fmt(format_args!("    {} %{}, {}(%rbp)", store_instr, reg, slot.0));
                }
                ParamClass::FloatReg { reg_idx } => {
                    if ty == IrType::F32 {
                        self.state.emit_fmt(format_args!("    movd %{}, %eax", xmm_regs[reg_idx]));
                        self.state.emit_fmt(format_args!("    movq %rax, {}(%rbp)", slot.0));
                    } else {
                        self.state.emit_fmt(format_args!("    movq %{}, {}(%rbp)", xmm_regs[reg_idx], slot.0));
                    }
                }
                ParamClass::I128RegPair { base_reg_idx } => {
                    self.state.emit_fmt(format_args!("    movq %{}, {}(%rbp)", X86_ARG_REGS[base_reg_idx], slot.0));
                    self.state.emit_fmt(format_args!("    movq %{}, {}(%rbp)", X86_ARG_REGS[base_reg_idx + 1], slot.0 + 8));
                }
                ParamClass::StructByValReg { base_reg_idx, size } => {
                    self.state.emit_fmt(format_args!("    movq %{}, {}(%rbp)", X86_ARG_REGS[base_reg_idx], slot.0));
                    if size > 8 {
                        self.state.emit_fmt(format_args!("    movq %{}, {}(%rbp)", X86_ARG_REGS[base_reg_idx + 1], slot.0 + 8));
                    }
                }
                ParamClass::F128AlwaysStack { offset } => {
                    // x86: F128 (long double) always passes on stack via x87.
                    // Load the 80-bit extended value from caller's stack and store
                    // it in 80-bit format to the local slot (fstpt), so that later
                    // emit_load can read it back with fldt.
                    let src = stack_base + offset;
                    self.state.emit_fmt(format_args!("    fldt {}(%rbp)", src));
                    self.state.emit_fmt(format_args!("    fstpt {}(%rbp)", slot.0));
                }
                ParamClass::I128Stack { offset } => {
                    let src = stack_base + offset;
                    self.state.emit_fmt(format_args!("    movq {}(%rbp), %rax", src));
                    self.state.emit_fmt(format_args!("    movq %rax, {}(%rbp)", slot.0));
                    self.state.emit_fmt(format_args!("    movq {}(%rbp), %rax", src + 8));
                    self.state.emit_fmt(format_args!("    movq %rax, {}(%rbp)", slot.0 + 8));
                }
                ParamClass::StackScalar { offset } => {
                    let src = stack_base + offset;
                    self.state.emit_fmt(format_args!("    movq {}(%rbp), %rax", src));
                    let store_instr = Self::mov_store_for_type(ty);
                    let reg = Self::reg_for_type("rax", ty);
                    self.state.emit_fmt(format_args!("    {} %{}, {}(%rbp)", store_instr, reg, slot.0));
                }
                ParamClass::StructStack { offset, size } | ParamClass::LargeStructStack { offset, size } => {
                    let src = stack_base + offset;
                    let n_qwords = (size + 7) / 8;
                    for qi in 0..n_qwords {
                        let src_off = src + (qi as i64 * 8);
                        let dst_off = slot.0 + (qi as i64 * 8);
                        self.state.emit_fmt(format_args!("    movq {}(%rbp), %rax", src_off));
                        self.state.emit_fmt(format_args!("    movq %rax, {}(%rbp)", dst_off));
                    }
                }
                // These variants don't occur for x86 (no F128 in FP/GP pair regs, no by-ref structs).
                ParamClass::F128FpReg { .. } | ParamClass::F128GpPair { .. } | ParamClass::F128Stack { .. } |
                ParamClass::LargeStructByRefReg { .. } | ParamClass::LargeStructByRefStack { .. } => {}
            }
        }
    }

    // ---- Primitives for shared default implementations ----

    fn emit_load_acc_pair(&mut self, op: &Operand) {
        self.operand_to_rax_rdx(op);
    }

    fn emit_store_acc_pair(&mut self, dest: &Value) {
        self.store_rax_rdx_to(dest);
    }

    fn emit_store_pair_to_slot(&mut self, slot: StackSlot) {
        self.state.emit_fmt(format_args!("    movq %rax, {}(%rbp)", slot.0));
        self.state.emit_fmt(format_args!("    movq %rdx, {}(%rbp)", slot.0 + 8));
    }

    fn emit_load_pair_from_slot(&mut self, slot: StackSlot) {
        self.state.emit_fmt(format_args!("    movq {}(%rbp), %rax", slot.0));
        self.state.emit_fmt(format_args!("    movq {}(%rbp), %rdx", slot.0 + 8));
    }

    fn emit_save_acc_pair(&mut self) {
        self.state.emit("    movq %rax, %rsi");
        self.state.emit("    movq %rdx, %rdi");
    }

    fn emit_store_pair_indirect(&mut self) {
        // pair saved to rsi:rdi by emit_save_acc_pair, ptr in rcx via emit_load_ptr_from_slot
        self.state.emit("    movq %rsi, (%rcx)");
        self.state.emit("    movq %rdi, 8(%rcx)");
    }

    fn emit_load_pair_indirect(&mut self) {
        // ptr in rcx via emit_load_ptr_from_slot, load pair from it
        self.state.emit("    movq (%rcx), %rax");
        self.state.emit("    movq 8(%rcx), %rdx");
    }

    fn emit_i128_neg(&mut self) {
        self.state.emit("    notq %rax");
        self.state.emit("    notq %rdx");
        self.state.emit("    addq $1, %rax");
        self.state.emit("    adcq $0, %rdx");
    }

    fn emit_i128_not(&mut self) {
        self.state.emit("    notq %rax");
        self.state.emit("    notq %rdx");
    }

    fn emit_sign_extend_acc_high(&mut self) {
        self.state.emit("    cqto"); // sign-extends rax into rdx
    }

    fn emit_zero_acc_high(&mut self) {
        self.state.emit("    xorq %rdx, %rdx");
    }

    fn current_return_type(&self) -> IrType {
        self.current_return_type
    }

    fn emit_return_i128_to_regs(&mut self) {
        // rax:rdx already hold the i128 return value per SysV ABI — noop
    }

    fn emit_return_f128_to_reg(&mut self) {
        // F128 (long double) must be returned in x87 st(0) per SysV ABI.
        // rax has f64 bit pattern; push to stack, load with fldl.
        self.state.emit("    pushq %rax");
        self.state.emit("    fldl (%rsp)");
        self.state.emit("    addq $8, %rsp");
    }

    fn emit_return_f32_to_reg(&mut self) {
        self.state.emit("    movd %eax, %xmm0");
    }

    fn emit_return_f64_to_reg(&mut self) {
        self.state.emit("    movq %rax, %xmm0");
    }

    fn emit_return_int_to_reg(&mut self) {
        // rax already holds the return value per SysV ABI — noop
    }

    fn emit_epilogue_and_ret(&mut self, frame_size: i64) {
        self.emit_epilogue(frame_size);
        if self.state.function_return_thunk {
            self.state.emit("    jmp __x86_return_thunk");
        } else {
            self.state.emit("    ret");
        }
    }

    fn store_instr_for_type(&self, ty: IrType) -> &'static str {
        Self::mov_store_for_type(ty)
    }

    fn load_instr_for_type(&self, ty: IrType) -> &'static str {
        Self::mov_load_for_type(ty)
    }

    /// Override emit_store to handle F128 (long double) with x87 80-bit memory format.
    /// On x86-64, long double must be stored as 80-bit extended precision in memory
    /// so that code reading through unions or integer overlays sees correct x87 bytes.
    /// Non-F128 types delegate to the shared default implementation.
    fn emit_store(&mut self, val: &Operand, ptr: &Value, ty: IrType) {
        use crate::backend::state::SlotAddr;
        if ty == IrType::F128 {
            // Check if we have a LongDouble constant with full-precision x87 bytes.
            // If so, store the raw bytes directly to memory to preserve precision.
            if let Operand::Const(IrConst::LongDouble(_, bytes)) = val {
                let lo = u64::from_le_bytes(bytes[0..8].try_into().unwrap());
                let hi_bytes: [u8; 8] = [bytes[8], bytes[9], 0, 0, 0, 0, 0, 0];
                let hi = u64::from_le_bytes(hi_bytes);
                let addr = self.state.resolve_slot_addr(ptr.0);
                if let Some(addr) = addr {
                    match addr {
                        SlotAddr::Direct(slot) => {
                            // Store low 8 bytes
                            self.state.out.emit_instr_imm_reg("    movabsq", lo as i64, "rax");
                            self.state.emit_fmt(format_args!("    movq %rax, {}(%rbp)", slot.0));
                            // Store high 2 bytes (exponent + sign) as a word, zero-extended
                            if hi != 0 {
                                self.state.out.emit_instr_imm_reg("    movq", hi as i64, "rax");
                            } else {
                                self.state.emit("    xorq %rax, %rax");
                            }
                            self.state.emit_fmt(format_args!("    movq %rax, {}(%rbp)", slot.0 + 8));
                        }
                        SlotAddr::OverAligned(slot, id) => {
                            self.emit_alloca_aligned_addr(slot, id);
                            self.state.out.emit_instr_imm_reg("    movabsq", lo as i64, "rax");
                            self.state.emit("    movq %rax, (%rcx)");
                            if hi != 0 {
                                self.state.out.emit_instr_imm_reg("    movq", hi as i64, "rax");
                            } else {
                                self.state.emit("    xorq %rax, %rax");
                            }
                            self.state.emit("    movq %rax, 8(%rcx)");
                        }
                        SlotAddr::Indirect(slot) => {
                            self.emit_load_ptr_from_slot(slot, ptr.0);
                            self.state.out.emit_instr_imm_reg("    movabsq", lo as i64, "rax");
                            self.state.emit("    movq %rax, (%rcx)");
                            if hi != 0 {
                                self.state.out.emit_instr_imm_reg("    movq", hi as i64, "rax");
                            } else {
                                self.state.emit("    xorq %rax, %rax");
                            }
                            self.state.emit("    movq %rax, 8(%rcx)");
                        }
                    }
                }
                return;
            }
            // F128 store: If the source value has full x87 precision in its direct slot,
            // use fldt from that slot instead of the f64 intermediate in rax.
            if let Operand::Value(v) = val {
                if self.state.f128_direct_slots.contains(&v.0) {
                    if let Some(src_slot) = self.state.get_slot(v.0) {
                        let dest_addr = self.state.resolve_slot_addr(ptr.0);
                        if let Some(dest_addr) = dest_addr {
                            // fldt from source slot (full 80-bit precision)
                            self.state.emit_fmt(format_args!("    fldt {}(%rbp)", src_slot.0));
                            match dest_addr {
                                SlotAddr::Direct(slot) => {
                                    self.state.emit_fmt(format_args!("    fstpt {}(%rbp)", slot.0));
                                }
                                SlotAddr::OverAligned(slot, id) => {
                                    self.emit_alloca_aligned_addr(slot, id);
                                    self.state.emit("    fstpt (%rcx)");
                                }
                                SlotAddr::Indirect(slot) => {
                                    self.emit_load_ptr_from_slot(slot, ptr.0);
                                    self.state.emit("    fstpt (%rcx)");
                                }
                            }
                            return;
                        }
                    }
                }
            }
            // F128 store: convert f64 in rax to x87 80-bit and store to memory.
            // Steps:
            //   1. Load the f64 value into %rax
            //   2. Push %rax, fldl (%rsp), addq $8, %rsp  (f64 -> x87 ST0)
            //   3. fstpt <dest>  (store ST0 as 80-bit extended precision)
            self.emit_load_operand(val);
            let addr = self.state.resolve_slot_addr(ptr.0);
            if let Some(addr) = addr {
                match addr {
                    SlotAddr::Direct(slot) => {
                        self.state.emit("    pushq %rax");
                        self.state.emit("    fldl (%rsp)");
                        self.state.emit("    addq $8, %rsp");
                        self.state.emit_fmt(format_args!("    fstpt {}(%rbp)", slot.0));
                    }
                    SlotAddr::OverAligned(slot, id) => {
                        self.state.emit("    movq %rax, %rdx");
                        self.emit_alloca_aligned_addr(slot, id);
                        self.state.emit("    pushq %rdx");
                        self.state.emit("    fldl (%rsp)");
                        self.state.emit("    addq $8, %rsp");
                        self.state.emit("    fstpt (%rcx)");
                    }
                    SlotAddr::Indirect(slot) => {
                        self.state.emit("    movq %rax, %rdx");
                        self.emit_load_ptr_from_slot(slot, ptr.0);
                        self.state.emit("    pushq %rdx");
                        self.state.emit("    fldl (%rsp)");
                        self.state.emit("    addq $8, %rsp");
                        self.state.emit("    fstpt (%rcx)");
                    }
                }
            }
            return;
        }
        // Non-F128: delegate to shared default implementation.
        crate::backend::traits::emit_store_default(self, val, ptr, ty);
    }

    /// Override emit_load to handle F128 (long double) with x87 80-bit memory format.
    /// Loads 80-bit extended precision from memory and converts to f64 in %rax.
    /// Non-F128 types delegate to the shared default implementation.
    fn emit_load(&mut self, dest: &Value, ptr: &Value, ty: IrType) {
        use crate::backend::state::SlotAddr;
        if ty == IrType::F128 {
            // F128 load: read x87 80-bit from memory.
            // Store the full 80-bit value into the dest's 16-byte slot (preserving precision),
            // and also put a truncated f64 copy in rax for backward compatibility.
            let addr = self.state.resolve_slot_addr(ptr.0);
            if let Some(addr) = addr {
                match &addr {
                    SlotAddr::Direct(slot) => {
                        self.state.emit_fmt(format_args!("    fldt {}(%rbp)", slot.0));
                    }
                    SlotAddr::OverAligned(slot, id) => {
                        self.emit_alloca_aligned_addr(*slot, *id);
                        self.state.emit("    fldt (%rcx)");
                    }
                    SlotAddr::Indirect(slot) => {
                        self.emit_load_ptr_from_slot(*slot, ptr.0);
                        self.state.emit("    fldt (%rcx)");
                    }
                }
                // ST0 now has full 80-bit precision.
                // Store full precision to dest's 16-byte slot if available
                if let Some(dest_slot) = self.state.get_slot(dest.0) {
                    self.state.emit_fmt(format_args!("    fstpt {}(%rbp)", dest_slot.0));
                    // Reload and convert to f64 for rax
                    self.state.emit_fmt(format_args!("    fldt {}(%rbp)", dest_slot.0));
                    self.state.emit("    subq $8, %rsp");
                    self.state.emit("    fstpl (%rsp)");
                    self.state.emit("    popq %rax");
                    self.state.reg_cache.set_acc(dest.0, false);
                    self.state.f128_direct_slots.insert(dest.0);
                } else {
                    // No slot: just truncate to f64 in rax
                    self.state.emit("    subq $8, %rsp");
                    self.state.emit("    fstpl (%rsp)");
                    self.state.emit("    popq %rax");
                    self.emit_store_result(dest);
                }
                // Also record the source pointer for backward compat
                self.state.f128_load_sources.insert(dest.0, ptr.0);
            }
            return;
        }
        // Non-F128: delegate to shared default implementation.
        crate::backend::traits::emit_load_default(self, dest, ptr, ty);
    }

    /// Override emit_load_with_const_offset to handle F128 (long double) via x87.
    /// The default implementation uses movq which is wrong for F128 - we need fldt.
    fn emit_load_with_const_offset(&mut self, dest: &Value, base: &Value, offset: i64, ty: IrType) {
        use crate::backend::state::SlotAddr;
        if ty == IrType::F128 {
            let addr = self.state.resolve_slot_addr(base.0);
            if let Some(addr) = addr {
                match addr {
                    SlotAddr::Direct(slot) => {
                        let folded = slot.0 + offset;
                        self.state.emit_fmt(format_args!("    fldt {}(%rbp)", folded));
                    }
                    SlotAddr::OverAligned(slot, id) => {
                        self.emit_alloca_aligned_addr(slot, id);
                        if offset != 0 {
                            self.state.emit_fmt(format_args!("    addq ${}, %rcx", offset));
                        }
                        self.state.emit("    fldt (%rcx)");
                    }
                    SlotAddr::Indirect(_) => {
                        unreachable!("emit_load_with_const_offset called for non-alloca base");
                    }
                }
                // ST0 has full 80-bit precision. Store to dest's 16-byte slot if available.
                if let Some(dest_slot) = self.state.get_slot(dest.0) {
                    self.state.emit_fmt(format_args!("    fstpt {}(%rbp)", dest_slot.0));
                    self.state.emit_fmt(format_args!("    fldt {}(%rbp)", dest_slot.0));
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
            return;
        }
        // Non-F128: use the default GEP fold logic.
        let addr = self.state.resolve_slot_addr(base.0);
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
                SlotAddr::Indirect(_) => {
                    unreachable!("emit_load_with_const_offset called for non-alloca base");
                }
            }
            self.emit_store_result(dest);
        }
    }

    /// Emit a load with x86 segment override prefix (%gs: or %fs:).
    /// Used for GCC named address space: *(typeof(x) __seg_gs *)addr
    /// Emits: movl %gs:(%rcx), %eax  (or appropriate sized variant).
    fn emit_seg_load(&mut self, dest: &Value, ptr: &Value, ty: IrType, seg: AddressSpace) {
        let seg_prefix = match seg {
            AddressSpace::SegGs => "%gs:",
            AddressSpace::SegFs => "%fs:",
            AddressSpace::Default => unreachable!(),
        };
        // Load the pointer (address) into %rcx
        self.emit_load_operand(&Operand::Value(*ptr));
        self.state.emit("    movq %rax, %rcx");
        // Emit segment-prefixed load
        let load_instr = Self::mov_load_for_type(ty);
        let dest_reg = Self::load_dest_reg(ty);
        self.state.emit_fmt(format_args!("    {} {}(%rcx), {}", load_instr, seg_prefix, dest_reg));
        self.emit_store_result(dest);
    }

    /// Emit a segment-overridden load using a direct symbol(%rip) reference.
    /// Emits: movl %gs:symbol(%rip), %eax  (or appropriate sized variant).
    fn emit_seg_load_symbol(&mut self, dest: &Value, sym: &str, ty: IrType, seg: AddressSpace) {
        let seg_prefix = match seg {
            AddressSpace::SegGs => "%gs:",
            AddressSpace::SegFs => "%fs:",
            AddressSpace::Default => unreachable!(),
        };
        let load_instr = Self::mov_load_for_type(ty);
        let dest_reg = Self::load_dest_reg(ty);
        self.state.emit_fmt(format_args!("    {} {}{}(%rip), {}", load_instr, seg_prefix, sym, dest_reg));
        self.emit_store_result(dest);
    }

    /// Emit a store with x86 segment override prefix (%gs: or %fs:).
    /// Used for GCC named address space: *(typeof(x) __seg_gs *)addr = val
    /// Emits: movl %edx, %gs:(%rcx)  (or appropriate sized variant).
    fn emit_seg_store(&mut self, val: &Operand, ptr: &Value, ty: IrType, seg: AddressSpace) {
        let seg_prefix = match seg {
            AddressSpace::SegGs => "%gs:",
            AddressSpace::SegFs => "%fs:",
            AddressSpace::Default => unreachable!(),
        };
        // Load value into %rax, then save to %rdx
        self.emit_load_operand(val);
        self.state.emit("    movq %rax, %rdx");
        // Load the pointer (address) into %rcx
        self.emit_load_operand(&Operand::Value(*ptr));
        self.state.emit("    movq %rax, %rcx");
        // Emit segment-prefixed store
        let store_instr = Self::mov_store_for_type(ty);
        let store_reg = Self::reg_for_type("rdx", ty);
        self.state.emit_fmt(format_args!("    {} %{}, {}(%rcx)", store_instr, store_reg, seg_prefix));
    }

    /// Emit a segment-overridden store using a direct symbol(%rip) reference.
    /// Emits: movl %edx, %gs:symbol(%rip)  (or appropriate sized variant).
    fn emit_seg_store_symbol(&mut self, val: &Operand, sym: &str, ty: IrType, seg: AddressSpace) {
        let seg_prefix = match seg {
            AddressSpace::SegGs => "%gs:",
            AddressSpace::SegFs => "%fs:",
            AddressSpace::Default => unreachable!(),
        };
        self.emit_load_operand(val);
        let store_instr = Self::mov_store_for_type(ty);
        let store_reg = Self::reg_for_type("rax", ty);
        self.state.emit_fmt(format_args!("    {} %{}, {}{}(%rip)", store_instr, store_reg, seg_prefix, sym));
    }

    /// Override emit_store_with_const_offset to handle F128 (long double) via x87.
    /// The default implementation uses movq which is wrong for F128 - we need fstpt.
    fn emit_store_with_const_offset(&mut self, val: &Operand, base: &Value, offset: i64, ty: IrType) {
        use crate::backend::state::SlotAddr;
        if ty == IrType::F128 {
            // Check if we have a LongDouble constant with full-precision x87 bytes.
            // If so, store the raw bytes directly to memory to preserve precision.
            if let Operand::Const(IrConst::LongDouble(_, bytes)) = val {
                let lo = u64::from_le_bytes(bytes[0..8].try_into().unwrap());
                let hi_bytes: [u8; 8] = [bytes[8], bytes[9], 0, 0, 0, 0, 0, 0];
                let hi = u64::from_le_bytes(hi_bytes);
                let addr = self.state.resolve_slot_addr(base.0);
                if let Some(addr) = addr {
                    match addr {
                        SlotAddr::Direct(slot) => {
                            let folded = slot.0 + offset;
                            self.state.out.emit_instr_imm_reg("    movabsq", lo as i64, "rax");
                            self.state.emit_fmt(format_args!("    movq %rax, {}(%rbp)", folded));
                            if hi != 0 {
                                self.state.out.emit_instr_imm_reg("    movq", hi as i64, "rax");
                            } else {
                                self.state.emit("    xorq %rax, %rax");
                            }
                            self.state.emit_fmt(format_args!("    movq %rax, {}(%rbp)", folded + 8));
                        }
                        SlotAddr::OverAligned(slot, id) => {
                            self.emit_alloca_aligned_addr(slot, id);
                            if offset != 0 {
                                self.state.emit_fmt(format_args!("    addq ${}, %rcx", offset));
                            }
                            self.state.out.emit_instr_imm_reg("    movabsq", lo as i64, "rax");
                            self.state.emit("    movq %rax, (%rcx)");
                            if hi != 0 {
                                self.state.out.emit_instr_imm_reg("    movq", hi as i64, "rax");
                            } else {
                                self.state.emit("    xorq %rax, %rax");
                            }
                            self.state.emit("    movq %rax, 8(%rcx)");
                        }
                        SlotAddr::Indirect(_) => {
                            unreachable!("emit_store_with_const_offset called for non-alloca base");
                        }
                    }
                }
                return;
            }
            // F128 store: If the source value has full x87 precision in its direct slot,
            // use fldt from that slot instead of the f64 intermediate in rax.
            if let Operand::Value(v) = val {
                if self.state.f128_direct_slots.contains(&v.0) {
                    if let Some(src_slot) = self.state.get_slot(v.0) {
                        let addr = self.state.resolve_slot_addr(base.0);
                        if let Some(addr) = addr {
                            self.state.emit_fmt(format_args!("    fldt {}(%rbp)", src_slot.0));
                            match addr {
                                SlotAddr::Direct(slot) => {
                                    let folded = slot.0 + offset;
                                    self.state.emit_fmt(format_args!("    fstpt {}(%rbp)", folded));
                                }
                                SlotAddr::OverAligned(slot, id) => {
                                    self.emit_alloca_aligned_addr(slot, id);
                                    if offset != 0 {
                                        self.state.emit_fmt(format_args!("    addq ${}, %rcx", offset));
                                    }
                                    self.state.emit("    fstpt (%rcx)");
                                }
                                SlotAddr::Indirect(_) => {
                                    unreachable!("emit_store_with_const_offset called for non-alloca base");
                                }
                            }
                            return;
                        }
                    }
                }
            }
            // Fallback: load f64 approximation into rax, convert through x87 to 80-bit.
            self.emit_load_operand(val);
            let addr = self.state.resolve_slot_addr(base.0);
            if let Some(addr) = addr {
                match addr {
                    SlotAddr::Direct(slot) => {
                        let folded = slot.0 + offset;
                        self.state.emit("    pushq %rax");
                        self.state.emit("    fldl (%rsp)");
                        self.state.emit("    addq $8, %rsp");
                        self.state.emit_fmt(format_args!("    fstpt {}(%rbp)", folded));
                    }
                    SlotAddr::OverAligned(slot, id) => {
                        self.state.emit("    movq %rax, %rdx");
                        self.emit_alloca_aligned_addr(slot, id);
                        if offset != 0 {
                            self.state.emit_fmt(format_args!("    addq ${}, %rcx", offset));
                        }
                        self.state.emit("    pushq %rdx");
                        self.state.emit("    fldl (%rsp)");
                        self.state.emit("    addq $8, %rsp");
                        self.state.emit("    fstpt (%rcx)");
                    }
                    SlotAddr::Indirect(_) => {
                        unreachable!("emit_store_with_const_offset called for non-alloca base");
                    }
                }
            }
            return;
        }
        // Non-F128: use the default GEP fold logic.
        self.emit_load_operand(val);
        let addr = self.state.resolve_slot_addr(base.0);
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
                SlotAddr::Indirect(_) => {
                    unreachable!("emit_store_with_const_offset called for non-alloca base");
                }
            }
        }
    }

    fn emit_typed_store_to_slot(&mut self, instr: &'static str, ty: IrType, slot: StackSlot) {
        let reg = Self::reg_for_type("rax", ty);
        // Build: "    {instr} %{reg}, {offset}(%rbp)"
        let out = &mut self.state.out;
        out.write_str("    ");
        out.write_str(instr);
        out.write_str(" %");
        out.write_str(reg);
        out.write_str(", ");
        out.write_i64(slot.0);
        out.write_str("(%rbp)");
        out.newline();
    }

    fn emit_typed_load_from_slot(&mut self, instr: &'static str, slot: StackSlot) {
        let dest_reg = if instr == "movl" { "%eax" } else { "%rax" };
        // Build: "    {instr} {offset}(%rbp), {dest_reg}"
        let out = &mut self.state.out;
        out.write_str("    ");
        out.write_str(instr);
        out.write_str(" ");
        out.write_i64(slot.0);
        out.write_str("(%rbp), ");
        out.write_str(dest_reg);
        out.newline();
    }

    fn emit_save_acc(&mut self) {
        self.state.emit("    movq %rax, %rdx");
    }

    fn emit_load_ptr_from_slot(&mut self, slot: StackSlot, val_id: u32) {
        // Load pointer to rcx. Used by all indirect paths:
        // - i128 store: pair saved to rsi:rdi, ptr to rcx (no conflict)
        // - regular store: val saved to rdx, ptr to rcx (no conflict)
        // - i128 load: ptr to rcx, then load pair through rcx
        // - regular load: ptr to rcx, then load through rcx
        // Check register allocation: use callee-saved register if available.
        if let Some(&reg) = self.reg_assignments.get(&val_id) {
            let reg_name = callee_saved_name(reg);
            self.state.out.emit_instr_reg_reg("    movq", reg_name, "rcx");
        } else {
            self.state.out.emit_instr_rbp_reg("    movq", slot.0, "rcx");
        }
    }

    fn emit_typed_store_indirect(&mut self, instr: &'static str, ty: IrType) {
        // val was saved to rdx by emit_save_acc, ptr in rcx via emit_load_ptr_from_slot
        let store_reg = Self::reg_for_type("rdx", ty);
        self.state.emit_fmt(format_args!("    {} %{}, (%rcx)", instr, store_reg));
    }

    fn emit_typed_load_indirect(&mut self, instr: &'static str) {
        // ptr in rcx via emit_load_ptr_from_slot, load through it into accumulator
        let dest_reg = if instr == "movl" { "%eax" } else { "%rax" };
        self.state.emit_fmt(format_args!("    {} (%rcx), {}", instr, dest_reg));
    }

    fn emit_add_offset_to_addr_reg(&mut self, offset: i64) {
        self.state.emit_fmt(format_args!("    addq ${}, %rcx", offset));
    }

    fn emit_slot_addr_to_secondary(&mut self, slot: StackSlot, is_alloca: bool, val_id: u32) {
        // Load base address directly into rcx (no push/pop needed).
        if is_alloca {
            self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rcx", slot.0));
        } else if let Some(&reg) = self.reg_assignments.get(&val_id) {
            // Register-allocated: use callee-saved register directly.
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    movq %{}, %rcx", reg_name));
        } else {
            self.state.emit_fmt(format_args!("    movq {}(%rbp), %rcx", slot.0));
        }
    }

    fn emit_add_secondary_to_acc(&mut self) {
        // rcx has the base address, rax has the offset; add them.
        self.state.emit("    addq %rcx, %rax");
        // rax now has a computed address, not a value from any slot.
        self.state.reg_cache.invalidate_acc();
    }

    fn emit_gep_direct_const(&mut self, slot: StackSlot, offset: i64) {
        // Alloca base + constant offset: single lea instruction.
        // leaq (slot+offset)(%rbp), %rax
        let folded = slot.0 + offset;
        self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rax", folded));
        self.state.reg_cache.invalidate_acc();
    }

    fn emit_gep_indirect_const(&mut self, slot: StackSlot, offset: i64, val_id: u32) {
        // Pointer base + constant offset: load ptr, then lea offset(ptr), %rax.
        // First load the base pointer into rax.
        if let Some(&reg) = self.reg_assignments.get(&val_id) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    movq %{}, %rax", reg_name));
        } else {
            self.state.emit_fmt(format_args!("    movq {}(%rbp), %rax", slot.0));
        }
        // Then add constant offset. Use leaq for non-zero, skip for zero.
        if offset != 0 {
            self.state.emit_fmt(format_args!("    leaq {}(%rax), %rax", offset));
        }
        self.state.reg_cache.invalidate_acc();
    }

    fn emit_gep_add_const_to_acc(&mut self, offset: i64) {
        // After computing base addr in rax, add constant offset.
        if offset != 0 {
            self.state.emit_fmt(format_args!("    addq ${}, %rax", offset));
        }
        self.state.reg_cache.invalidate_acc();
    }

    fn emit_add_imm_to_acc(&mut self, imm: i64) {
        self.state.emit_fmt(format_args!("    addq ${}, %rax", imm));
        self.state.reg_cache.invalidate_acc();
    }

    fn emit_round_up_acc_to_16(&mut self) {
        self.state.emit("    addq $15, %rax");
        self.state.emit("    andq $-16, %rax");
        self.state.reg_cache.invalidate_all();
    }

    fn emit_sub_sp_by_acc(&mut self) {
        self.state.emit("    subq %rax, %rsp");
    }

    fn emit_mov_sp_to_acc(&mut self) {
        self.state.emit("    movq %rsp, %rax");
        self.state.reg_cache.invalidate_all();
    }

    fn emit_mov_acc_to_sp(&mut self) {
        self.state.emit("    movq %rax, %rsp");
        self.state.reg_cache.invalidate_all();
    }

    fn emit_align_acc(&mut self, align: usize) {
        self.state.emit_fmt(format_args!("    addq ${}, %rax", align - 1));
        self.state.emit_fmt(format_args!("    andq ${}, %rax", -(align as i64)));
        self.state.reg_cache.invalidate_all();
    }

    fn emit_memcpy_load_dest_addr(&mut self, slot: StackSlot, is_alloca: bool, val_id: u32) {
        if is_alloca {
            self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rdi", slot.0));
        } else if let Some(&reg) = self.reg_assignments.get(&val_id) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    movq %{}, %rdi", reg_name));
        } else {
            self.state.emit_fmt(format_args!("    movq {}(%rbp), %rdi", slot.0));
        }
    }

    fn emit_memcpy_load_src_addr(&mut self, slot: StackSlot, is_alloca: bool, val_id: u32) {
        if is_alloca {
            self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rsi", slot.0));
        } else if let Some(&reg) = self.reg_assignments.get(&val_id) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    movq %{}, %rsi", reg_name));
        } else {
            self.state.emit_fmt(format_args!("    movq {}(%rbp), %rsi", slot.0));
        }
    }

    fn emit_alloca_aligned_addr(&mut self, slot: StackSlot, val_id: u32) {
        let align = self.state.alloca_over_align(val_id).unwrap();
        self.state.out.emit_instr_rbp_reg("    leaq", slot.0, "rcx");
        self.state.out.emit_instr_imm_reg("    addq", (align - 1) as i64, "rcx");
        self.state.out.emit_instr_imm_reg("    andq", -(align as i64), "rcx");
    }

    fn emit_alloca_aligned_addr_to_acc(&mut self, slot: StackSlot, val_id: u32) {
        let align = self.state.alloca_over_align(val_id).unwrap();
        self.state.out.emit_instr_rbp_reg("    leaq", slot.0, "rax");
        self.state.out.emit_instr_imm_reg("    addq", (align - 1) as i64, "rax");
        self.state.out.emit_instr_imm_reg("    andq", -(align as i64), "rax");
        self.state.reg_cache.invalidate_acc();
    }

    fn emit_acc_to_secondary(&mut self) {
        // Move accumulator (rax) to secondary register (rcx) instead of pushing
        // to the stack. This avoids the push/pop overhead.
        self.state.emit("    movq %rax, %rcx");
        // The cache for rax is still valid since we just copied it to rcx.
    }

    fn emit_memcpy_store_dest_from_acc(&mut self) {
        self.state.emit("    movq %rcx, %rdi");
    }

    fn emit_memcpy_store_src_from_acc(&mut self) {
        self.state.emit("    movq %rcx, %rsi");
    }

    fn emit_memcpy_impl(&mut self, size: usize) {
        self.state.emit_fmt(format_args!("    movq ${}, %rcx", size));
        self.state.emit("    rep movsb");
    }

    fn emit_float_neg(&mut self, ty: IrType) {
        if ty == IrType::F32 {
            self.state.emit("    movd %eax, %xmm0");
            self.state.emit("    movl $0x80000000, %ecx");
            self.state.emit("    movd %ecx, %xmm1");
            self.state.emit("    xorps %xmm1, %xmm0");
            self.state.emit("    movd %xmm0, %eax");
        } else {
            self.state.emit("    movq %rax, %xmm0");
            self.state.emit("    movabsq $-9223372036854775808, %rcx");
            self.state.emit("    movq %rcx, %xmm1");
            self.state.emit("    xorpd %xmm1, %xmm0");
            self.state.emit("    movq %xmm0, %rax");
        }
    }

    fn emit_int_neg(&mut self, _ty: IrType) {
        self.state.emit("    negq %rax");
    }

    fn emit_int_not(&mut self, _ty: IrType) {
        self.state.emit("    notq %rax");
    }

    fn emit_int_clz(&mut self, ty: IrType) {
        if ty == IrType::I32 || ty == IrType::U32 {
            self.state.emit("    lzcntl %eax, %eax");
        } else {
            self.state.emit("    lzcntq %rax, %rax");
        }
    }

    fn emit_int_ctz(&mut self, ty: IrType) {
        if ty == IrType::I32 || ty == IrType::U32 {
            self.state.emit("    tzcntl %eax, %eax");
        } else {
            self.state.emit("    tzcntq %rax, %rax");
        }
    }

    fn emit_int_bswap(&mut self, ty: IrType) {
        if ty == IrType::I16 || ty == IrType::U16 {
            self.state.emit("    rolw $8, %ax");
        } else if ty == IrType::I32 || ty == IrType::U32 {
            self.state.emit("    bswapl %eax");
        } else {
            self.state.emit("    bswapq %rax");
        }
    }

    fn emit_int_popcount(&mut self, ty: IrType) {
        if ty == IrType::I32 || ty == IrType::U32 {
            self.state.emit("    popcntl %eax, %eax");
        } else {
            self.state.emit("    popcntq %rax, %rax");
        }
    }

    // ---- Standard trait methods ----

    fn emit_load_operand(&mut self, op: &Operand) {
        self.operand_to_rax(op);
    }

    fn emit_store_result(&mut self, dest: &Value) {
        self.store_rax_to(dest);
    }

    /// Override copy to emit direct register-to-register moves when possible,
    /// avoiding the accumulator roundtrip (movq %rbx, %rax; movq %rax, %r12
    /// becomes movq %rbx, %r12).
    fn emit_copy_value(&mut self, dest: &Value, src: &Operand) {
        // For F128 values with full x87 data in their slots, copy the full x87
        // extended precision value via fldt/fstpt to preserve 80-bit precision.
        if let Operand::Value(v) = src {
            if self.state.f128_direct_slots.contains(&v.0) {
                if let (Some(src_slot), Some(dest_slot)) = (self.state.get_slot(v.0), self.state.get_slot(dest.0)) {
                    // Full-precision F128 copy: fldt from source, fstpt to dest
                    self.state.emit_fmt(format_args!("    fldt {}(%rbp)", src_slot.0));
                    self.state.emit_fmt(format_args!("    fstpt {}(%rbp)", dest_slot.0));
                    // Store f64 approximation in rax for consumers that read via operand_to_rax
                    self.state.emit_fmt(format_args!("    fldt {}(%rbp)", dest_slot.0));
                    self.state.emit("    subq $8, %rsp");
                    self.state.emit("    fstpl (%rsp)");
                    self.state.emit("    popq %rax");
                    self.state.reg_cache.set_acc(dest.0, false);
                    self.state.f128_direct_slots.insert(dest.0);
                    return;
                }
            }
        }

        let dest_phys = self.dest_reg(dest);
        let src_phys = self.operand_reg(src);

        match (dest_phys, src_phys) {
            (Some(d), Some(s)) => {
                if d.0 != s.0 {
                    let d_name = callee_saved_name(d);
                    let s_name = callee_saved_name(s);
                    self.state.out.emit_instr_reg_reg("    movq", s_name, d_name);
                }
                // Same register = no-op
                self.state.reg_cache.invalidate_acc();
            }
            (Some(d), None) => {
                // Dest in callee-saved register, src on stack or constant.
                // Load src into rax first (preserving reg_cache semantics),
                // then move to dest register.
                self.operand_to_rax(src);
                let d_name = callee_saved_name(d);
                self.state.out.emit_instr_reg_reg("    movq", "rax", d_name);
                self.state.reg_cache.invalidate_acc();
            }
            _ => {
                // Src in callee-saved or neither: use accumulator path
                self.operand_to_rax(src);
                self.store_rax_to(dest);
            }
        }
    }

    // emit_binop uses the shared default implementation (handles i128/float/int dispatch).
    // emit_float_binop is overridden below to avoid push/pop (loads lhs->xmm0, rhs->xmm1 directly).

    fn emit_int_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        let use_32bit = ty == IrType::I32 || ty == IrType::U32;
        let is_unsigned = ty.is_unsigned();

        // --- Register-direct path: operate on callee-saved destination register ---
        // Avoids going through %rax, eliminating 1-3 mov instructions per operation.
        if let Some(dest_phys) = self.dest_reg(dest) {
            let dest_name = callee_saved_name(dest_phys);
            let dest_name_32 = callee_saved_name_32(dest_phys);

            // Simple ALU ops (add/sub/and/or/xor/mul) with register destination
            let is_simple_alu = matches!(op, IrBinOp::Add | IrBinOp::Sub | IrBinOp::And
                | IrBinOp::Or | IrBinOp::Xor | IrBinOp::Mul);
            if is_simple_alu {
                // Immediate form: op $imm, %dest_reg
                if let Some(imm) = Self::const_as_imm32(rhs) {
                    self.operand_to_callee_reg(lhs, dest_phys);
                    if op == IrBinOp::Mul {
                        // imul has 3-operand immediate form: imulq $imm, %src, %dst
                        if use_32bit {
                            self.state.emit_fmt(format_args!("    imull ${}, %{}, %{}", imm, dest_name_32, dest_name_32));
                            self.emit_sext32_if_needed(dest_name_32, dest_name, is_unsigned);
                        } else {
                            self.state.emit_fmt(format_args!("    imulq ${}, %{}, %{}", imm, dest_name, dest_name));
                        }
                    } else {
                        let mnemonic = alu_mnemonic(op);
                        if use_32bit && matches!(op, IrBinOp::Add | IrBinOp::Sub) {
                            self.state.emit_fmt(format_args!("    {}l ${}, %{}", mnemonic, imm, dest_name_32));
                            self.emit_sext32_if_needed(dest_name_32, dest_name, is_unsigned);
                        } else {
                            self.state.emit_fmt(format_args!("    {}q ${}, %{}", mnemonic, imm, dest_name));
                        }
                    }
                    self.state.reg_cache.invalidate_acc();
                    return;
                }

                // Register-register form: load LHS to dest, then op RHS into dest.
                // If RHS is in the same register as dest, load RHS to scratch first.
                let rhs_phys = self.operand_reg(rhs);
                let rhs_conflicts = rhs_phys.map_or(false, |r| r.0 == dest_phys.0);
                let (rhs_reg_name, rhs_reg_name_32): (String, String) = if rhs_conflicts {
                    self.operand_to_rax(rhs);
                    self.operand_to_callee_reg(lhs, dest_phys);
                    ("rax".to_string(), "eax".to_string())
                } else {
                    self.operand_to_callee_reg(lhs, dest_phys);
                    if let Some(rhs_phys) = rhs_phys {
                        (callee_saved_name(rhs_phys).to_string(), callee_saved_name_32(rhs_phys).to_string())
                    } else {
                        self.operand_to_rax(rhs);
                        ("rax".to_string(), "eax".to_string())
                    }
                };

                if op == IrBinOp::Mul {
                    if use_32bit {
                        self.state.emit_fmt(format_args!("    imull %{}, %{}", rhs_reg_name_32, dest_name_32));
                        self.emit_sext32_if_needed(dest_name_32, dest_name, is_unsigned);
                    } else {
                        self.state.emit_fmt(format_args!("    imulq %{}, %{}", rhs_reg_name, dest_name));
                    }
                } else {
                    let mnemonic = alu_mnemonic(op);
                    if use_32bit && matches!(op, IrBinOp::Add | IrBinOp::Sub) {
                        self.state.emit_fmt(format_args!("    {}l %{}, %{}", mnemonic, rhs_reg_name_32, dest_name_32));
                        self.emit_sext32_if_needed(dest_name_32, dest_name, is_unsigned);
                    } else {
                        self.state.emit_fmt(format_args!("    {}q %{}, %{}", mnemonic, rhs_reg_name, dest_name));
                    }
                }
                self.state.reg_cache.invalidate_acc();
                return;
            }

            // Shift ops with register destination
            if matches!(op, IrBinOp::Shl | IrBinOp::AShr | IrBinOp::LShr) {
                let (mnem32, mnem64) = shift_mnemonic(op);
                if let Some(imm) = Self::const_as_imm32(rhs) {
                    // Immediate shift
                    self.operand_to_callee_reg(lhs, dest_phys);
                    if use_32bit {
                        let shift_amount = (imm as u32) & 31;
                        self.state.emit_fmt(format_args!("    {} ${}, %{}", mnem32, shift_amount, dest_name_32));
                        if !is_unsigned && matches!(op, IrBinOp::Shl | IrBinOp::AShr) {
                            self.state.emit_fmt(format_args!("    movslq %{}, %{}", dest_name_32, dest_name));
                        }
                    } else {
                        let shift_amount = (imm as u64) & 63;
                        self.state.emit_fmt(format_args!("    {} ${}, %{}", mnem64, shift_amount, dest_name));
                    }
                } else {
                    // Variable shift: load shift amount into %cl.
                    // If rhs is in the same register as dest, load rhs first.
                    let rhs_conflicts = self.operand_reg(rhs).map_or(false, |r| r.0 == dest_phys.0);
                    if rhs_conflicts {
                        self.operand_to_rcx(rhs);
                        self.operand_to_callee_reg(lhs, dest_phys);
                    } else {
                        self.operand_to_callee_reg(lhs, dest_phys);
                        self.operand_to_rcx(rhs);
                    }
                    if use_32bit {
                        self.state.emit_fmt(format_args!("    {} %cl, %{}", mnem32, dest_name_32));
                        if !is_unsigned && matches!(op, IrBinOp::Shl | IrBinOp::AShr) {
                            self.state.emit_fmt(format_args!("    movslq %{}, %{}", dest_name_32, dest_name));
                        }
                    } else {
                        self.state.emit_fmt(format_args!("    {} %cl, %{}", mnem64, dest_name));
                    }
                }
                self.state.reg_cache.invalidate_acc();
                return;
            }
        }

        // --- Accumulator-based fallback (no register dest, or div/rem) ---

        // Immediate optimization for ALU ops
        if matches!(op, IrBinOp::Add | IrBinOp::Sub | IrBinOp::And | IrBinOp::Or | IrBinOp::Xor) {
            if let Some(imm) = Self::const_as_imm32(rhs) {
                self.operand_to_rax(lhs);
                let mnemonic = alu_mnemonic(op);
                if use_32bit && matches!(op, IrBinOp::Add | IrBinOp::Sub) {
                    self.state.emit_fmt(format_args!("    {}l ${}, %eax", mnemonic, imm));
                    if !is_unsigned { self.state.emit("    cltq"); }
                } else {
                    self.state.emit_fmt(format_args!("    {}q ${}, %rax", mnemonic, imm));
                }
                self.state.reg_cache.invalidate_acc();
                self.store_rax_to(dest);
                return;
            }
        }

        // Immediate multiply: imul has 3-operand form
        if op == IrBinOp::Mul {
            if let Some(imm) = Self::const_as_imm32(rhs) {
                self.operand_to_rax(lhs);
                if use_32bit {
                    self.state.emit_fmt(format_args!("    imull ${}, %eax, %eax", imm));
                    if !is_unsigned { self.state.emit("    cltq"); }
                } else {
                    self.state.emit_fmt(format_args!("    imulq ${}, %rax, %rax", imm));
                }
                self.state.reg_cache.invalidate_acc();
                self.store_rax_to(dest);
                return;
            }
        }

        // Immediate shift
        if matches!(op, IrBinOp::Shl | IrBinOp::AShr | IrBinOp::LShr) {
            if let Some(imm) = Self::const_as_imm32(rhs) {
                self.operand_to_rax(lhs);
                let (mnem32, mnem64) = shift_mnemonic(op);
                if use_32bit {
                    let shift_amount = (imm as u32) & 31;
                    self.state.emit_fmt(format_args!("    {} ${}, %eax", mnem32, shift_amount));
                    if !is_unsigned && matches!(op, IrBinOp::Shl | IrBinOp::AShr) {
                        self.state.emit("    cltq");
                    }
                } else {
                    let shift_amount = (imm as u64) & 63;
                    self.state.emit_fmt(format_args!("    {} ${}, %rax", mnem64, shift_amount));
                }
                self.state.reg_cache.invalidate_acc();
                self.store_rax_to(dest);
                return;
            }
        }

        // General case: load lhs to rax, rhs to rcx
        self.operand_to_rax(lhs);
        self.operand_to_rcx(rhs);

        match op {
            IrBinOp::Add | IrBinOp::Sub | IrBinOp::Mul => {
                let mnem = match op {
                    IrBinOp::Add => "add",
                    IrBinOp::Sub => "sub",
                    IrBinOp::Mul => "imul",
                    _ => unreachable!(),
                };
                if use_32bit {
                    self.state.emit_fmt(format_args!("    {}l %ecx, %eax", mnem));
                    if !is_unsigned { self.state.emit("    cltq"); }
                } else {
                    self.state.emit_fmt(format_args!("    {}q %rcx, %rax", mnem));
                }
            }
            IrBinOp::SDiv => {
                if use_32bit {
                    self.state.emit("    cltd");
                    self.state.emit("    idivl %ecx");
                    self.state.emit("    cltq");
                } else {
                    self.state.emit("    cqto");
                    self.state.emit("    idivq %rcx");
                }
            }
            IrBinOp::UDiv => {
                if use_32bit {
                    self.state.emit("    xorl %edx, %edx");
                    self.state.emit("    divl %ecx");
                } else {
                    self.state.emit("    xorq %rdx, %rdx");
                    self.state.emit("    divq %rcx");
                }
            }
            IrBinOp::SRem => {
                if use_32bit {
                    self.state.emit("    cltd");
                    self.state.emit("    idivl %ecx");
                    self.state.emit("    movl %edx, %eax");
                    self.state.emit("    cltq");
                } else {
                    self.state.emit("    cqto");
                    self.state.emit("    idivq %rcx");
                    self.state.emit("    movq %rdx, %rax");
                }
            }
            IrBinOp::URem => {
                if use_32bit {
                    self.state.emit("    xorl %edx, %edx");
                    self.state.emit("    divl %ecx");
                    self.state.emit("    movl %edx, %eax");
                } else {
                    self.state.emit("    xorq %rdx, %rdx");
                    self.state.emit("    divq %rcx");
                    self.state.emit("    movq %rdx, %rax");
                }
            }
            IrBinOp::And => self.state.emit("    andq %rcx, %rax"),
            IrBinOp::Or => self.state.emit("    orq %rcx, %rax"),
            IrBinOp::Xor => self.state.emit("    xorq %rcx, %rax"),
            IrBinOp::Shl | IrBinOp::AShr | IrBinOp::LShr => {
                let (mnem32, mnem64) = shift_mnemonic(op);
                if use_32bit {
                    self.state.emit_fmt(format_args!("    {} %cl, %eax", mnem32));
                    if !is_unsigned && op != IrBinOp::LShr { self.state.emit("    cltq"); }
                } else {
                    self.state.emit_fmt(format_args!("    {} %cl, %rax", mnem64));
                }
            }
        }

        self.state.reg_cache.invalidate_acc();
        self.store_rax_to(dest);
    }

    fn emit_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if is_i128_type(ty) {
            self.emit_i128_cmp(dest, op, lhs, rhs);
            return;
        }
        if ty == IrType::F128 {
            // F128 (long double) comparison using x87 fucomip for full 80-bit precision.
            // fucomip compares ST(0) with ST(1) and sets EFLAGS (CF, ZF, PF) just like ucomisd,
            // then pops ST(0). We then fstp to clean the remaining ST(0).
            //
            // fucomip flag semantics (same as ucomisd):
            //   ST(0) > ST(1): CF=0, ZF=0, PF=0
            //   ST(0) < ST(1): CF=1, ZF=0, PF=0
            //   ST(0) = ST(1): CF=0, ZF=1, PF=0
            //   Unordered (NaN): CF=1, ZF=1, PF=1
            //
            // For Gt/Ge: load rhs first (→ST1), then lhs (→ST0).
            //   fucomip compares ST(0)=lhs vs ST(1)=rhs. seta = lhs > rhs. ✓
            // For Lt/Le: load lhs first (→ST1), then rhs (→ST0).
            //   fucomip compares ST(0)=rhs vs ST(1)=lhs. seta = rhs > lhs = lhs < rhs. ✓
            // For Eq/Ne: order doesn't matter, load lhs first then rhs.
            let swap_x87 = matches!(op, IrCmpOp::Slt | IrCmpOp::Ult | IrCmpOp::Sle | IrCmpOp::Ule);
            let (first_x87, second_x87) = if swap_x87 { (lhs, rhs) } else { (rhs, lhs) };
            // Load first operand → ST(0), then second → ST(0) (pushes first to ST(1))
            self.emit_f128_load_to_x87(first_x87);
            self.emit_f128_load_to_x87(second_x87);
            // ST(0) = second, ST(1) = first
            // fucomip compares ST(0) with ST(1), pops ST(0)
            self.state.emit("    fucomip %st(1), %st");
            // Clean up remaining x87 stack entry
            self.state.emit("    fstp %st(0)");
            match op {
                IrCmpOp::Eq => {
                    self.state.emit("    setnp %al");
                    self.state.emit("    sete %cl");
                    self.state.emit("    andb %cl, %al");
                }
                IrCmpOp::Ne => {
                    self.state.emit("    setp %al");
                    self.state.emit("    setne %cl");
                    self.state.emit("    orb %cl, %al");
                }
                IrCmpOp::Slt | IrCmpOp::Ult | IrCmpOp::Sgt | IrCmpOp::Ugt => {
                    self.state.emit("    seta %al");
                }
                IrCmpOp::Sle | IrCmpOp::Ule | IrCmpOp::Sge | IrCmpOp::Uge => {
                    self.state.emit("    setae %al");
                }
            }
            self.state.emit("    movzbq %al, %rax");
            self.state.reg_cache.invalidate_acc();
            self.store_rax_to(dest);
            return;
        }
        if ty.is_float() {
            // Float comparison using SSE ucomisd/ucomiss.
            // NaN handling: ucomisd sets CF=1, ZF=1, PF=1 for unordered (NaN) operands.
            // C ordered comparisons must return false for NaN (except !=).
            // Strategy:
            //   Eq:  setnp + sete → AND (both ZF=1 and PF=0 required)
            //   Ne:  setp + setne → OR (either ZF=0 or PF=1 sufficient)
            //   Lt/Le: reverse operand order, use seta/setae (NaN → CF=1 → false)
            //   Gt/Ge: seta/setae directly (NaN → CF=1 → false)
            let (mov_to_xmm0, mov_to_xmm1_from_rcx) = if ty == IrType::F32 {
                ("movd %eax, %xmm0", "movd %ecx, %xmm1")
            } else {
                ("movq %rax, %xmm0", "movq %rcx, %xmm1")
            };
            // For Lt/Le, we swap operand loading order so we can use seta/setae
            let swap_operands = matches!(op, IrCmpOp::Slt | IrCmpOp::Ult | IrCmpOp::Sle | IrCmpOp::Ule);
            let (first, second) = if swap_operands { (rhs, lhs) } else { (lhs, rhs) };
            // Load first operand to rax -> xmm0, second to rcx -> xmm1 (no push/pop)
            self.operand_to_rax(first);
            self.state.emit_fmt(format_args!("    {}", mov_to_xmm0));
            self.operand_to_rcx(second);
            self.state.emit_fmt(format_args!("    {}", mov_to_xmm1_from_rcx));
            // ucomisd %xmm1, %xmm0 compares xmm0 vs xmm1 (AT&T: src, dst → compares dst to src)
            if ty == IrType::F64 {
                self.state.emit("    ucomisd %xmm1, %xmm0");
            } else {
                self.state.emit("    ucomiss %xmm1, %xmm0");
            }
            match op {
                IrCmpOp::Eq => {
                    // Ordered equal: ZF=1 AND PF=0 (not unordered)
                    self.state.emit("    setnp %al");
                    self.state.emit("    sete %cl");
                    self.state.emit("    andb %cl, %al");
                }
                IrCmpOp::Ne => {
                    // Unordered not-equal: ZF=0 OR PF=1 (NaN → true)
                    self.state.emit("    setp %al");
                    self.state.emit("    setne %cl");
                    self.state.emit("    orb %cl, %al");
                }
                IrCmpOp::Slt | IrCmpOp::Ult | IrCmpOp::Sgt | IrCmpOp::Ugt => {
                    // With operands swapped for Lt, seta gives correct ordered result
                    // seta: CF=0 AND ZF=0 (NaN sets CF=1, so returns false)
                    self.state.emit("    seta %al");
                }
                IrCmpOp::Sle | IrCmpOp::Ule | IrCmpOp::Sge | IrCmpOp::Uge => {
                    // With operands swapped for Le, setae gives correct ordered result
                    // setae: CF=0 (NaN sets CF=1, so returns false)
                    self.state.emit("    setae %al");
                }
            }
            self.state.emit("    movzbq %al, %rax");
            self.state.reg_cache.invalidate_acc();
            self.store_rax_to(dest);
            return;
        }

        // Integer comparison: use 32-bit compare for I32/U32 types
        let use_32bit = ty == IrType::I32 || ty == IrType::U32;
        self.emit_int_cmp_insn_typed(lhs, rhs, use_32bit);

        let set_instr = match op {
            IrCmpOp::Eq => "sete",
            IrCmpOp::Ne => "setne",
            IrCmpOp::Slt => "setl",
            IrCmpOp::Sle => "setle",
            IrCmpOp::Sgt => "setg",
            IrCmpOp::Sge => "setge",
            IrCmpOp::Ult => "setb",
            IrCmpOp::Ule => "setbe",
            IrCmpOp::Ugt => "seta",
            IrCmpOp::Uge => "setae",
        };
        self.state.emit_fmt(format_args!("    {} %al", set_instr));
        self.state.emit("    movzbq %al, %rax");
        self.state.reg_cache.invalidate_acc();
        self.store_rax_to(dest);
    }

    /// Fused compare-and-branch: emit cmpq + jCC directly without materializing
    /// the boolean result to a register or stack slot.
    ///
    /// Before fusion (7+ instructions):
    ///   cmpq %rcx, %rax; setl %al; movzbq %al, %rax; movq %rax, -N(%rbp);
    ///   movq -N(%rbp), %rax; testq %rax, %rax; jne .Ltrue; jmp .Lfalse
    ///
    /// After fusion (3 instructions):
    ///   cmpq %rcx, %rax; jl .Ltrue; jmp .Lfalse
    fn emit_fused_cmp_branch(
        &mut self,
        op: IrCmpOp,
        lhs: &Operand,
        rhs: &Operand,
        ty: IrType,
        true_label: &str,
        false_label: &str,
    ) {
        // Emit the integer comparison, using 32-bit form for I32/U32
        let use_32bit = ty == IrType::I32 || ty == IrType::U32;
        self.emit_int_cmp_insn_typed(lhs, rhs, use_32bit);

        // Emit fused conditional jump directly (no setCC, no movzbq, no store/load/test)
        let jcc = match op {
            IrCmpOp::Eq  => "je",
            IrCmpOp::Ne  => "jne",
            IrCmpOp::Slt => "jl",
            IrCmpOp::Sle => "jle",
            IrCmpOp::Sgt => "jg",
            IrCmpOp::Sge => "jge",
            IrCmpOp::Ult => "jb",
            IrCmpOp::Ule => "jbe",
            IrCmpOp::Ugt => "ja",
            IrCmpOp::Uge => "jae",
        };
        self.state.emit_fmt(format_args!("    {} {}", jcc, true_label));
        self.state.emit_fmt(format_args!("    jmp {}", false_label));
        self.state.reg_cache.invalidate_all();
    }

    fn emit_fused_cmp_branch_blocks(
        &mut self,
        op: IrCmpOp,
        lhs: &Operand,
        rhs: &Operand,
        ty: IrType,
        true_block: BlockId,
        false_block: BlockId,
    ) {
        // Use 32-bit compare for I32/U32 types (avoids unnecessary sign-extension)
        let use_32bit = ty == IrType::I32 || ty == IrType::U32;
        self.emit_int_cmp_insn_typed(lhs, rhs, use_32bit);

        // Emit fused conditional jump using fast block-id path
        let jcc = match op {
            IrCmpOp::Eq  => "    je",
            IrCmpOp::Ne  => "    jne",
            IrCmpOp::Slt => "    jl",
            IrCmpOp::Sle => "    jle",
            IrCmpOp::Sgt => "    jg",
            IrCmpOp::Sge => "    jge",
            IrCmpOp::Ult => "    jb",
            IrCmpOp::Ule => "    jbe",
            IrCmpOp::Ugt => "    ja",
            IrCmpOp::Uge => "    jae",
        };
        self.state.out.emit_jcc_block(jcc, true_block.0);
        self.state.out.emit_jmp_block(false_block.0);
        self.state.reg_cache.invalidate_all();
    }

    fn emit_cond_branch_blocks(&mut self, cond: &Operand, true_block: BlockId, false_block: BlockId) {
        self.operand_to_rax(cond);
        self.state.emit("    testq %rax, %rax");
        self.state.out.emit_jcc_block("    jne", true_block.0);
        self.state.out.emit_jmp_block(false_block.0);
    }

    /// Emit a conditional select using x86 cmov instructions.
    ///
    /// Strategy:
    ///   1. Load false_val into %rax (the default/fallback value)
    ///   2. Load true_val into %rcx
    ///   3. Load condition into %rdx and test it (sets flags)
    ///   4. cmovneq %rcx, %rax (if cond != 0, select true_val)
    ///   5. Store result from %rax to dest
    ///
    /// We load the operands first and test the condition last, because
    /// operand loading may use xorq (for zero constants) which clobbers flags.
    /// The movq instructions used for stack/register loads don't affect flags.
    fn emit_select(&mut self, dest: &Value, cond: &Operand, true_val: &Operand, false_val: &Operand, _ty: IrType) {
        // Step 1: Load false_val into %rax (the default value if cond == 0).
        self.operand_to_rax(false_val);

        // Step 2: Load true_val into %rcx.
        self.operand_to_rcx(true_val);

        // Step 3: Load condition and test. We use %rdx as a scratch register
        // to avoid clobbering %rax or %rcx.
        match cond {
            Operand::Const(c) => {
                let val = match c {
                    IrConst::I8(v) => *v as i64,
                    IrConst::I16(v) => *v as i64,
                    IrConst::I32(v) => *v as i64,
                    IrConst::I64(v) => *v,
                    IrConst::Zero => 0,
                    _ => 0,
                };
                if val >= i32::MIN as i64 && val <= i32::MAX as i64 {
                    self.state.emit_fmt(format_args!("    movq ${}, %rdx", val));
                } else {
                    self.state.emit_fmt(format_args!("    movabsq ${}, %rdx", val));
                }
            }
            Operand::Value(v) => {
                // Load condition value into %rdx. Use value_to_reg which
                // correctly handles alloca values (LEA for address computation
                // instead of MOV which would load stack contents) and
                // over-aligned allocas.
                if let Some(&reg) = self.reg_assignments.get(&v.0) {
                    let reg_name = callee_saved_name(reg);
                    self.state.emit_fmt(format_args!("    movq %{}, %rdx", reg_name));
                } else if self.state.get_slot(v.0).is_some() {
                    self.value_to_reg(v, "rdx");
                } else {
                    self.state.emit("    xorq %rdx, %rdx");
                }
            }
        }
        self.state.emit("    testq %rdx, %rdx");

        // Step 4: cmovne - if condition was nonzero, select true_val (rcx) over false_val (rax)
        self.state.emit("    cmovneq %rcx, %rax");

        // Step 5: Store result
        self.state.reg_cache.invalidate_acc();
        self.store_rax_to(dest);
    }

    // emit_call: uses shared default from ArchCodegen trait (traits.rs)

    fn call_abi_config(&self) -> CallAbiConfig {
        CallAbiConfig {
            max_int_regs: 6, max_float_regs: 8,
            align_i128_pairs: false,
            f128_in_fp_regs: false, f128_in_gp_pairs: false,
            variadic_floats_in_gp: false,
            large_struct_by_ref: false, // x86-64 SysV: large structs passed on stack by value
        }
    }

    fn emit_call_compute_stack_space(&self, arg_classes: &[CallArgClass]) -> usize {
        // x86 uses pushq instructions; compute raw bytes pushed (alignment handled separately).
        compute_stack_push_bytes(arg_classes)
    }

    // emit_call_spill_fptr: uses default no-op (x86 uses r10 scratch, no spill needed)
    // emit_call_fptr_spill_size: uses default 0

    fn emit_call_stack_args(&mut self, args: &[Operand], arg_classes: &[CallArgClass],
                            _arg_types: &[IrType], stack_arg_space: usize, _fptr_spill: usize, _f128_temp_space: usize) -> i64 {
        // x86 pushes in reverse order, with alignment padding if needed.
        let need_align_pad = stack_arg_space % 16 != 0;
        if need_align_pad {
            self.state.emit("    subq $8, %rsp");
        }
        let arg_padding = crate::backend::call_abi::compute_stack_arg_padding(arg_classes);
        let stack_indices: Vec<usize> = (0..args.len())
            .filter(|&i| arg_classes[i].is_stack())
            .collect();
        for &si in stack_indices.iter().rev() {
            match arg_classes[si] {
                CallArgClass::F128Stack => {
                    match &args[si] {
                        Operand::Const(ref c) => {
                            let x87_bytes: [u8; 10] = match c {
                                IrConst::LongDouble(_, raw) => {
                                    // Use the stored full-precision x87 bytes
                                    let mut b = [0u8; 10];
                                    b.copy_from_slice(&raw[..10]);
                                    b
                                }
                                _ => {
                                    let f64_val = c.to_f64().unwrap_or(0.0);
                                    crate::ir::ir::f64_to_x87_bytes(f64_val)
                                }
                            };
                            let lo = u64::from_le_bytes(x87_bytes[0..8].try_into().unwrap());
                            let hi_2bytes = u16::from_le_bytes(x87_bytes[8..10].try_into().unwrap());
                            self.state.emit_fmt(format_args!("    pushq ${}", hi_2bytes as i64));
                            self.state.emit_fmt(format_args!("    movabsq ${}, %rax", lo as i64));
                            self.state.emit("    pushq %rax");
                            self.state.reg_cache.invalidate_all(); // rax clobbered by movabsq
                        }
                        Operand::Value(ref v) => {
                            // Check if this value has full 80-bit x87 precision
                            // stored directly in its slot (f128_direct_slots), or
                            // if it's an alloca containing a long double.
                            if self.state.f128_direct_slots.contains(&v.0) {
                                // Slot contains full 80-bit x87 extended precision data.
                                // Use fldt to load it directly, then fstpt to the stack arg area.
                                if let Some(slot) = self.state.get_slot(v.0) {
                                    self.state.emit("    subq $16, %rsp");
                                    self.state.emit_fmt(format_args!("    fldt {}(%rbp)", slot.0));
                                    self.state.emit("    fstpt (%rsp)");
                                } else {
                                    // Fallback: push zero
                                    self.state.emit("    subq $16, %rsp");
                                }
                            } else if let Some(slot) = self.state.get_slot(v.0) {
                                if self.state.is_alloca(v.0) {
                                    // Alloca contains a long double in memory.
                                    // Use fldt to load from the alloca address.
                                    self.state.emit("    subq $16, %rsp");
                                    self.state.emit_fmt(format_args!("    fldt {}(%rbp)", slot.0));
                                    self.state.emit("    fstpt (%rsp)");
                                } else {
                                    // Regular f64 value: convert via x87 to 80-bit
                                    self.state.emit_fmt(format_args!("    movq {}(%rbp), %rax", slot.0));
                                    self.state.reg_cache.invalidate_all();
                                    self.state.emit("    subq $16, %rsp");
                                    self.state.emit("    pushq %rax");
                                    self.state.emit("    fldl (%rsp)");
                                    self.state.emit("    addq $8, %rsp");
                                    self.state.emit("    fstpt (%rsp)");
                                }
                            } else {
                                // No slot: push zeroed 16 bytes
                                self.state.emit("    subq $16, %rsp");
                            }
                            self.state.reg_cache.invalidate_all();
                        }
                    }
                }
                CallArgClass::I128Stack => {
                    self.operand_to_rax_rdx(&args[si]);
                    self.state.emit("    pushq %rdx");
                    self.state.emit("    pushq %rax");
                }
                CallArgClass::StructByValStack { size } | CallArgClass::LargeStructStack { size } => {
                    self.operand_to_rax(&args[si]);
                    let n_qwords = (size + 7) / 8;
                    for qi in (0..n_qwords).rev() {
                        let offset = qi * 8;
                        if offset + 8 <= size {
                            self.state.emit_fmt(format_args!("    pushq {}(%rax)", offset));
                        } else {
                            self.state.emit_fmt(format_args!("    movq {}(%rax), %rcx", offset));
                            self.state.emit("    pushq %rcx");
                        }
                    }
                }
                CallArgClass::Stack => {
                    self.operand_to_rax(&args[si]);
                    self.state.emit("    pushq %rax");
                }
                _ => {}
            }
            // In reverse push order, forward-layout padding (before arg in memory)
            // must be emitted after the arg push (subq to reserve padding space).
            let pad = arg_padding[si];
            if pad > 0 {
                self.state.emit_fmt(format_args!("    subq ${}, %rsp", pad));
            }
        }
        // Return total SP adjustment (for x86 this doesn't affect register arg loading)
        0
    }

    fn emit_call_reg_args(&mut self, args: &[Operand], arg_classes: &[CallArgClass],
                          _arg_types: &[IrType], _total_sp_adjust: i64, _f128_temp_space: usize, _stack_arg_space: usize) {
        let xmm_regs = ["xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"];
        let mut float_count = 0usize;
        for (i, arg) in args.iter().enumerate() {
            match arg_classes[i] {
                CallArgClass::I128RegPair { base_reg_idx } => {
                    self.operand_to_rax_rdx(arg);
                    let lo_reg = X86_ARG_REGS[base_reg_idx];
                    let hi_reg = X86_ARG_REGS[base_reg_idx + 1];
                    if lo_reg == "rdx" {
                        self.state.emit_fmt(format_args!("    movq %rdx, %{}", hi_reg));
                        self.state.emit_fmt(format_args!("    movq %rax, %{}", lo_reg));
                    } else {
                        self.state.emit_fmt(format_args!("    movq %rax, %{}", lo_reg));
                        self.state.emit_fmt(format_args!("    movq %rdx, %{}", hi_reg));
                    }
                }
                CallArgClass::StructByValReg { base_reg_idx, size } => {
                    self.operand_to_rax(arg);
                    let lo_reg = X86_ARG_REGS[base_reg_idx];
                    self.state.emit_fmt(format_args!("    movq (%rax), %{}", lo_reg));
                    if size > 8 {
                        let hi_reg = X86_ARG_REGS[base_reg_idx + 1];
                        self.state.emit_fmt(format_args!("    movq 8(%rax), %{}", hi_reg));
                    }
                }
                CallArgClass::FloatReg { reg_idx } => {
                    self.operand_to_rax(arg);
                    self.state.emit_fmt(format_args!("    movq %rax, %{}", xmm_regs[reg_idx]));
                    float_count += 1;
                }
                CallArgClass::IntReg { reg_idx } => {
                    self.operand_to_rax(arg);
                    self.state.emit_fmt(format_args!("    movq %rax, %{}", X86_ARG_REGS[reg_idx]));
                }
                _ => {}
            }
        }
        // Set AL = number of float args for variadic functions (SysV ABI requirement).
        // Both paths clobber rax (movb to %al modifies low byte; xorl zeros eax entirely).
        if float_count > 0 {
            self.state.emit_fmt(format_args!("    movb ${}, %al", float_count));
        } else {
            self.state.emit("    xorl %eax, %eax");
        }
        // All the operand_to_rax calls above and the xorl/movb clobber rax.
        // Invalidate so emit_call_instruction's operand_to_rax for indirect
        // calls doesn't skip loading the function pointer.
        self.state.reg_cache.invalidate_all();
    }

    fn emit_call_instruction(&mut self, direct_name: Option<&str>, func_ptr: Option<&Operand>, _indirect: bool, _stack_arg_space: usize) {
        if let Some(name) = direct_name {
            if self.state.needs_plt(name) {
                self.state.emit_fmt(format_args!("    call {}@PLT", name));
            } else {
                self.state.emit_fmt(format_args!("    call {}", name));
            }
        } else if let Some(ptr) = func_ptr {
            self.state.emit("    pushq %rax"); // save AL (float count)
            self.operand_to_rax(ptr);
            self.state.emit("    movq %rax, %r10");
            self.state.emit("    popq %rax"); // restore AL
            if self.state.indirect_branch_thunk {
                self.state.emit("    call __x86_indirect_thunk_r10");
            } else {
                self.state.emit("    call *%r10");
            }
        }
        // Call clobbers rax (return value will be stored by emit_call_store_result)
        self.state.reg_cache.invalidate_all();
    }

    fn emit_call_cleanup(&mut self, stack_arg_space: usize, _f128_temp_space: usize, _indirect: bool) {
        let need_align_pad = stack_arg_space % 16 != 0;
        let total_cleanup = stack_arg_space + if need_align_pad { 8 } else { 0 };
        if total_cleanup > 0 {
            self.state.emit_fmt(format_args!("    addq ${}, %rsp", total_cleanup));
        }
    }

    fn emit_call_store_result(&mut self, dest: &Value, return_type: IrType) {
        if is_i128_type(return_type) {
            self.store_rax_rdx_to(dest);
        } else if return_type == IrType::F32 {
            self.state.emit("    movd %xmm0, %eax");
            self.store_rax_to(dest);
        } else if return_type == IrType::F128 {
            // Store full 80-bit x87 value directly to the 16-byte dest slot
            // using fstpt, preserving full precision instead of truncating to f64.
            if let Some(slot) = self.state.get_slot(dest.0) {
                self.state.emit_fmt(format_args!("    fstpt {}(%rbp)", slot.0));
                // Also store a truncated f64 copy in rax for operations that need it
                self.state.emit_fmt(format_args!("    fldt {}(%rbp)", slot.0));
                self.state.emit("    subq $8, %rsp");
                self.state.emit("    fstpl (%rsp)");
                self.state.emit("    popq %rax");
                self.state.reg_cache.set_acc(dest.0, false);
                // Mark this slot as containing direct F128 x87 data
                self.state.f128_direct_slots.insert(dest.0);
            } else {
                // Fallback: store as f64 if no slot available (e.g., register-assigned)
                self.state.emit("    subq $8, %rsp");
                self.state.emit("    fstpl (%rsp)");
                self.state.emit("    popq %rax");
                self.store_rax_to(dest);
            }
        } else if return_type == IrType::F64 {
            self.state.emit("    movq %xmm0, %rax");
            self.store_rax_to(dest);
        } else {
            self.store_rax_to(dest);
        }
    }

    fn emit_global_addr(&mut self, dest: &Value, name: &str) {
        if self.state.needs_got(name) {
            // PIC mode: load the address from the GOT
            self.state.emit_fmt(format_args!("    movq {}@GOTPCREL(%rip), %rax", name));
        } else {
            // Non-PIC or local symbol: direct RIP-relative LEA
            self.state.emit_fmt(format_args!("    leaq {}(%rip), %rax", name));
        }
        self.store_rax_to(dest);
    }

    fn emit_cast_instrs(&mut self, from_ty: IrType, to_ty: IrType) {
        self.emit_cast_instrs_x86(from_ty, to_ty);
    }

    /// Override emit_cast to handle F128 conversions with full x87 80-bit precision.
    ///
    /// F128 -> integer: When the source is an F128 value with a known memory slot,
    /// we use `fldt` directly from that slot instead of the f64 intermediate in %rax,
    /// preserving the full 64-bit mantissa for conversions like
    /// `(unsigned long long)922337203685477580.7L`.
    ///
    /// any -> F128: When casting to F128, we load the source into x87 via the
    /// appropriate fld instruction and store with fstpt to produce a full 80-bit
    /// value in the destination slot. This ensures subsequent F128 operations
    /// (arithmetic, comparison) that use fldt get correct data.
    fn emit_cast(&mut self, dest: &Value, src: &Operand, from_ty: IrType, to_ty: IrType) {
        use crate::backend::state::SlotAddr;

        // Intercept casts TO F128: produce full 80-bit x87 value in dest slot.
        // classify_cast treats F64->F128 as Noop and F32->F128 as F32->F64,
        // but we need to ensure the dest slot has a proper 80-bit x87 value
        // so that subsequent fldt loads get correct data.
        if to_ty == IrType::F128 && from_ty != IrType::F128 && !is_i128_type(from_ty) {
            if let Some(dest_slot) = self.state.get_slot(dest.0) {
                // Load source into x87 ST(0) via appropriate conversion
                if from_ty == IrType::F64 {
                    self.operand_to_rax(src);
                    self.state.emit("    subq $8, %rsp");
                    self.state.emit("    movq %rax, (%rsp)");
                    self.state.emit("    fldl (%rsp)");
                    self.state.emit("    addq $8, %rsp");
                } else if from_ty == IrType::F32 {
                    self.operand_to_rax(src);
                    self.state.emit("    subq $4, %rsp");
                    self.state.emit("    movl %eax, (%rsp)");
                    self.state.emit("    flds (%rsp)");
                    self.state.emit("    addq $4, %rsp");
                } else if from_ty.is_signed() || (!from_ty.is_float() && !from_ty.is_unsigned()) {
                    // Signed integer to F128: load operand, sign-extend, push as i64, fild
                    self.operand_to_rax(src);
                    // Ensure proper sign extension for sub-64-bit types
                    if from_ty.size() < 8 {
                        self.emit_cast_instrs_x86(from_ty, IrType::I64);
                    }
                    self.state.emit("    subq $8, %rsp");
                    self.state.emit("    movq %rax, (%rsp)");
                    self.state.emit("    fildq (%rsp)");
                    self.state.emit("    addq $8, %rsp");
                } else {
                    // Unsigned integer to F128: load operand, convert to f64 first via
                    // the standard cast path, then fldl
                    self.operand_to_rax(src);
                    self.emit_cast_instrs_x86(from_ty, IrType::F64);
                    self.state.emit("    movq %rax, %xmm0");
                    self.state.emit("    subq $8, %rsp");
                    self.state.emit("    movsd %xmm0, (%rsp)");
                    self.state.emit("    fldl (%rsp)");
                    self.state.emit("    addq $8, %rsp");
                }
                // ST(0) now has the value in 80-bit extended precision.
                // Store full 80-bit to dest slot
                self.state.emit_fmt(format_args!("    fstpt {}(%rbp)", dest_slot.0));
                // Also keep f64 copy in rax for backward compat
                self.state.emit_fmt(format_args!("    fldt {}(%rbp)", dest_slot.0));
                self.state.emit("    subq $8, %rsp");
                self.state.emit("    fstpl (%rsp)");
                self.state.emit("    popq %rax");
                self.state.reg_cache.set_acc(dest.0, false);
                self.state.f128_direct_slots.insert(dest.0);
                return;
            }
            // TODO: Handle F128 cast without dest slot - currently loses precision by falling back to f64
        }

        // Intercept F128 -> F64/F32 casts: load the full 80-bit value via fldt and
        // convert to f64/f32 using fstpl/fstps. This is necessary because when a value
        // is in f128_direct_slots, its stack slot contains raw x87 bytes (not f64),
        // so reading it via movq would get garbage.
        if from_ty == IrType::F128 && (to_ty == IrType::F64 || to_ty == IrType::F32) {
            // Load the F128 source into x87 ST(0)
            self.emit_f128_load_to_x87(src);
            if to_ty == IrType::F64 {
                // Convert x87 80-bit to f64 in rax
                self.state.emit("    subq $8, %rsp");
                self.state.emit("    fstpl (%rsp)");
                self.state.emit("    movq (%rsp), %rax");
                self.state.emit("    addq $8, %rsp");
            } else {
                // Convert x87 80-bit to f32 in eax
                self.state.emit("    subq $4, %rsp");
                self.state.emit("    fstps (%rsp)");
                self.state.emit("    movl (%rsp), %eax");
                self.state.emit("    addq $4, %rsp");
            }
            self.state.reg_cache.invalidate_acc();
            self.emit_store_result(dest);
            return;
        }

        // Intercept F128 -> integer casts when we know the source's memory location
        if from_ty == IrType::F128 && !to_ty.is_float() && !is_i128_type(to_ty) {
            if let Operand::Value(v) = src {
                // Check if the value has full F128 x87 data directly in its slot
                // (e.g., from a function call that returned long double)
                if self.state.f128_direct_slots.contains(&v.0) {
                    if let Some(slot) = self.state.get_slot(v.0) {
                        let addr = crate::backend::state::SlotAddr::Direct(slot);
                        self.emit_f128_to_int_from_memory(&addr, to_ty);
                        self.emit_store_result(dest);
                        return;
                    }
                }
                // Check if loaded from a known F128 alloca
                if let Some(ptr_id) = self.state.f128_load_sources.get(&v.0).copied() {
                    if let Some(addr) = self.state.resolve_slot_addr(ptr_id) {
                        self.emit_f128_to_int_from_memory(&addr, to_ty);
                        self.emit_store_result(dest);
                        return;
                    }
                }
            }
            // Also handle constant LongDouble operands with raw bytes
            if let Operand::Const(IrConst::LongDouble(_, bytes)) = src {
                // Store x87 bytes to stack, fldt, then fisttp
                self.state.emit("    subq $16, %rsp");
                let lo = u64::from_le_bytes(bytes[0..8].try_into().unwrap());
                let hi = u16::from_le_bytes(bytes[8..10].try_into().unwrap());
                self.state.emit_fmt(format_args!("    movabsq ${}, %rax", lo as i64));
                self.state.emit("    movq %rax, (%rsp)");
                self.state.emit_fmt(format_args!("    movq ${}, %rax", hi as i64));
                self.state.emit("    movq %rax, 8(%rsp)");
                self.state.emit("    fldt (%rsp)");
                self.state.emit("    addq $16, %rsp");
                // Now ST0 has the full-precision value, do the integer conversion
                self.emit_f128_st0_to_int(to_ty);
                self.emit_store_result(dest);
                return;
            }
        }
        // Fall through to default implementation for all other cases
        crate::backend::traits::emit_cast_default(self, dest, src, from_ty, to_ty);
    }

    fn emit_va_arg(&mut self, dest: &Value, va_list_ptr: &Value, result_ty: IrType) {
        // x86-64 System V ABI va_arg implementation.
        // va_list is: { u32 gp_offset, u32 fp_offset, void* overflow_arg_area, void* reg_save_area }
        //
        // For integer/pointer types:
        //   if gp_offset < 48:
        //     result = *(reg_save_area + gp_offset)
        //     gp_offset += 8
        //   else:
        //     result = *overflow_arg_area
        //     overflow_arg_area += 8
        //
        // For float/double types:
        //   if fp_offset < 176 (48 + 8*16):
        //     result = *(reg_save_area + fp_offset)
        //     fp_offset += 16
        //   else:
        //     result = *overflow_arg_area
        //     overflow_arg_area += 8

        let is_fp = result_ty.is_float();
        let is_f128 = result_ty.is_long_double();
        let label_reg = self.state.fresh_label("va_arg_reg");
        let label_mem = self.state.fresh_label("va_arg_mem");
        let label_end = self.state.fresh_label("va_arg_end");

        // Load va_list pointer into %rcx (register-aware).
        if let Some(&reg) = self.reg_assignments.get(&va_list_ptr.0) {
            // Value is in a callee-saved register.
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    movq %{}, %rcx", reg_name));
        } else if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
            if self.state.is_alloca(va_list_ptr.0) {
                self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rcx", slot.0));
            } else {
                self.state.emit_fmt(format_args!("    movq {}(%rbp), %rcx", slot.0));
            }
        }

        if is_f128 {
            // F128 (long double) on x86-64: always from overflow_arg_area, 16-byte aligned.
            // Per ABI, align overflow_arg_area to 16 before reading long double.
            self.state.emit("    movq 8(%rcx), %rdx");       // overflow_arg_area
            self.state.emit("    addq $15, %rdx");           // align up to 16
            self.state.emit("    andq $-16, %rdx");
            // Use x87 to load 80-bit extended and convert to f64
            self.state.emit("    fldt (%rdx)");              // load 80-bit extended from [rdx]
            self.state.emit("    subq $8, %rsp");            // temp space for f64
            self.state.emit("    fstpl (%rsp)");             // store as f64 to [rsp]
            self.state.emit("    movq (%rsp), %rax");        // load f64 bit pattern into rax
            self.state.emit("    addq $8, %rsp");            // free temp space
            // Advance overflow_arg_area past this 16-byte long double (from aligned position)
            self.state.emit("    addq $16, %rdx");
            self.state.emit("    movq %rdx, 8(%rcx)");
            // Store result via store_rax_to to handle register-allocated destinations.
            self.store_rax_to(dest);
            self.state.reg_cache.invalidate_all();
            return;
        } else if is_fp {
            // Check fp_offset < 176
            self.state.emit("    movl 4(%rcx), %eax");  // fp_offset
            self.state.emit("    cmpl $176, %eax");
            self.state.emit_fmt(format_args!("    jb {}", label_reg));
            self.state.emit_fmt(format_args!("    jmp {}", label_mem));

            // Register path
            self.state.emit_fmt(format_args!("{}:", label_reg));
            self.state.emit("    movl 4(%rcx), %eax");       // fp_offset
            self.state.emit("    movslq %eax, %rdx");
            self.state.emit("    movq 16(%rcx), %rsi");      // reg_save_area
            if result_ty == IrType::F32 {
                self.state.emit("    movss (%rsi,%rdx), %xmm0");
                self.state.emit("    movd %xmm0, %eax");
            } else {
                self.state.emit("    movsd (%rsi,%rdx), %xmm0");
                self.state.emit("    movq %xmm0, %rax");
            }
            self.state.emit("    addl $16, 4(%rcx)");       // fp_offset += 16
            self.state.emit_fmt(format_args!("    jmp {}", label_end));
        } else {
            // Check gp_offset < 48
            self.state.emit("    movl (%rcx), %eax");  // gp_offset
            self.state.emit("    cmpl $48, %eax");
            self.state.emit_fmt(format_args!("    jb {}", label_reg));
            self.state.emit_fmt(format_args!("    jmp {}", label_mem));

            // Register path
            self.state.emit_fmt(format_args!("{}:", label_reg));
            self.state.emit("    movl (%rcx), %eax");        // gp_offset
            self.state.emit("    movslq %eax, %rdx");
            self.state.emit("    movq 16(%rcx), %rsi");      // reg_save_area
            self.state.emit("    movq (%rsi,%rdx), %rax");   // load value
            self.state.emit("    addl $8, (%rcx)");          // gp_offset += 8
            self.state.emit_fmt(format_args!("    jmp {}", label_end));
        }

        // Memory (overflow) path
        self.state.emit_fmt(format_args!("{}:", label_mem));
        self.state.emit("    movq 8(%rcx), %rdx");       // overflow_arg_area
        if is_fp && result_ty == IrType::F32 {
            self.state.emit("    movss (%rdx), %xmm0");
            self.state.emit("    movd %xmm0, %eax");
        } else if is_fp {
            self.state.emit("    movsd (%rdx), %xmm0");
            self.state.emit("    movq %xmm0, %rax");
        } else {
            self.state.emit("    movq (%rdx), %rax");    // load value
        }
        self.state.emit("    addq $8, 8(%rcx)");         // overflow_arg_area += 8

        // End
        self.state.emit_fmt(format_args!("{}:", label_end));
        // Store result via store_rax_to to handle register-allocated destinations.
        self.store_rax_to(dest);
        self.state.reg_cache.invalidate_all(); // complex control flow, don't track
    }

    fn emit_va_start(&mut self, va_list_ptr: &Value) {
        // x86-64 System V ABI va_start implementation.
        // The prologue saves all 6 integer arg registers to the register save area.
        // va_list struct layout:
        //   [0]  u32 gp_offset:         byte offset into reg_save_area for next GP arg
        //   [4]  u32 fp_offset:         byte offset into reg_save_area for next FP arg
        //   [8]  void* overflow_arg_area: pointer to next stack-passed arg
        //   [16] void* reg_save_area:   pointer to saved register args

        // Load va_list pointer into %rax (register-aware).
        if let Some(&reg) = self.reg_assignments.get(&va_list_ptr.0) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    movq %{}, %rax", reg_name));
        } else if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
            if self.state.is_alloca(va_list_ptr.0) {
                self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rax", slot.0));
            } else {
                self.state.emit_fmt(format_args!("    movq {}(%rbp), %rax", slot.0));
            }
        }
        // gp_offset = min(num_named_int_params, 6) * 8 (skip named params in reg save area)
        // Cap at 48 (6 registers * 8 bytes) since only 6 GP regs are saved
        let gp_offset = self.num_named_int_params.min(6) * 8;
        self.state.emit_fmt(format_args!("    movl ${}, (%rax)", gp_offset));
        // fp_offset: when SSE is disabled (-mno-sse), set to 176 so va_arg always takes
        // the overflow (stack) path for FP args, since no XMM registers are saved.
        // When SSE is enabled: 48 + min(num_named_fp_params, 8) * 16
        let fp_offset = if self.no_sse {
            176 // Overflow immediately — no XMM save area exists
        } else {
            48 + self.num_named_fp_params.min(8) * 16
        };
        self.state.emit_fmt(format_args!("    movl ${}, 4(%rax)", fp_offset));
        // overflow_arg_area = rbp + 16 + total stack bytes for named params (including alignment padding)
        // num_named_stack_bytes is computed in calculate_stack_space() by simulating the forward
        // stack layout of all stack-passed named params (GP overflow, FP overflow, always-stack),
        // including 16-byte alignment padding before F128/I128 args.
        let overflow_offset = 16 + self.num_named_stack_bytes;
        self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rcx", overflow_offset));
        self.state.emit("    movq %rcx, 8(%rax)");
        // reg_save_area = address of the saved registers in the prologue
        let reg_save = self.reg_save_area_offset;
        self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rcx", reg_save));
        self.state.emit("    movq %rcx, 16(%rax)");
        self.state.reg_cache.invalidate_all(); // rax has va_list ptr, not IR value
    }

    // emit_va_end: uses default no-op implementation

    fn emit_va_copy(&mut self, dest_ptr: &Value, src_ptr: &Value) {
        // Copy 24 bytes from src va_list to dest va_list (register-aware).
        if let Some(&reg) = self.reg_assignments.get(&src_ptr.0) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    movq %{}, %rsi", reg_name));
        } else if let Some(src_slot) = self.state.get_slot(src_ptr.0) {
            if self.state.is_alloca(src_ptr.0) {
                self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rsi", src_slot.0));
            } else {
                self.state.emit_fmt(format_args!("    movq {}(%rbp), %rsi", src_slot.0));
            }
        }
        if let Some(&reg) = self.reg_assignments.get(&dest_ptr.0) {
            let reg_name = callee_saved_name(reg);
            self.state.emit_fmt(format_args!("    movq %{}, %rdi", reg_name));
        } else if let Some(dest_slot) = self.state.get_slot(dest_ptr.0) {
            if self.state.is_alloca(dest_ptr.0) {
                self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rdi", dest_slot.0));
            } else {
                self.state.emit_fmt(format_args!("    movq {}(%rbp), %rdi", dest_slot.0));
            }
        }
        // Copy 24 bytes (sizeof va_list = 24 on x86-64)
        self.state.emit("    movq (%rsi), %rax");
        self.state.emit("    movq %rax, (%rdi)");
        self.state.emit("    movq 8(%rsi), %rax");
        self.state.emit("    movq %rax, 8(%rdi)");
        self.state.emit("    movq 16(%rsi), %rax");
        self.state.emit("    movq %rax, 16(%rdi)");
        self.state.reg_cache.invalidate_all(); // rax has memory data, not IR value
    }

    /// Override emit_return to handle F128 return with full x87 precision.
    /// When returning a long double, if the value was loaded from a known F128 slot,
    /// use `fldt` directly instead of going through the f64 intermediate in %rax.
    fn emit_return(&mut self, val: Option<&Operand>, frame_size: i64) {
        use crate::backend::state::SlotAddr;
        if let Some(val) = val {
            let ret_ty = self.current_return_type();
            if ret_ty.is_long_double() {
                // Try to return full-precision F128 via fldt from original memory location
                if let Operand::Value(v) = val {
                    // Check if value has direct F128 data in its slot
                    if self.state.f128_direct_slots.contains(&v.0) {
                        if let Some(slot) = self.state.get_slot(v.0) {
                            self.state.emit_fmt(format_args!("    fldt {}(%rbp)", slot.0));
                            self.emit_epilogue_and_ret(frame_size);
                            return;
                        }
                    }
                    // Check if loaded from a known F128 alloca
                    if let Some(ptr_id) = self.state.f128_load_sources.get(&v.0).copied() {
                        if let Some(addr) = self.state.resolve_slot_addr(ptr_id) {
                            match addr {
                                SlotAddr::Direct(slot) => {
                                    self.state.emit_fmt(format_args!("    fldt {}(%rbp)", slot.0));
                                }
                                SlotAddr::OverAligned(slot, id) => {
                                    self.emit_alloca_aligned_addr(slot, id);
                                    self.state.emit("    fldt (%rcx)");
                                }
                                SlotAddr::Indirect(slot) => {
                                    self.emit_load_ptr_from_slot(slot, ptr_id);
                                    self.state.emit("    fldt (%rcx)");
                                }
                            }
                            self.emit_epilogue_and_ret(frame_size);
                            return;
                        }
                    }
                }
                // Also handle constant LongDouble returns
                if let Operand::Const(IrConst::LongDouble(_, bytes)) = val {
                    // Store x87 bytes to stack and fldt
                    self.state.emit("    subq $16, %rsp");
                    let lo = u64::from_le_bytes(bytes[0..8].try_into().unwrap());
                    let hi = u16::from_le_bytes(bytes[8..10].try_into().unwrap());
                    self.state.emit_fmt(format_args!("    movabsq ${}, %rax", lo as i64));
                    self.state.emit("    movq %rax, (%rsp)");
                    self.state.emit_fmt(format_args!("    movq ${}, %rax", hi as i64));
                    self.state.emit("    movq %rax, 8(%rsp)");
                    self.state.emit("    fldt (%rsp)");
                    self.state.emit("    addq $16, %rsp");
                    self.emit_epilogue_and_ret(frame_size);
                    return;
                }
            }
        }
        // Fall through to default return implementation
        crate::backend::traits::emit_return_default(self, val, frame_size);
    }

    // emit_branch, emit_cond_branch, emit_unreachable, emit_indirect_branch:
    // use default implementations from ArchCodegen trait

    fn emit_label_addr(&mut self, dest: &Value, label: &str) {
        // Labels are always local, never use GOTPCREL even in PIC mode
        self.state.emit_fmt(format_args!("    leaq {}(%rip), %rax", label));
        self.store_rax_to(dest);
    }

    fn emit_get_return_f64_second(&mut self, dest: &Value) {
        // After a function call, the second F64 return value is in xmm1.
        // Store it to the dest stack slot.
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.state.emit_fmt(format_args!("    movsd %xmm1, {}(%rbp)", slot.0));
        }
    }

    fn emit_set_return_f64_second(&mut self, src: &Operand) {
        // Load src into xmm1 for the second return value.
        match src {
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    self.state.emit_fmt(format_args!("    movsd {}(%rbp), %xmm1", slot.0));
                }
            }
            Operand::Const(IrConst::F64(f)) => {
                let bits = f.to_bits();
                self.state.emit_fmt(format_args!("    movabsq ${}, %rax", bits as i64));
                self.state.emit("    movq %rax, %xmm1");
                self.state.reg_cache.invalidate_all(); // rax clobbered by movabsq
            }
            _ => {
                self.operand_to_rax(src);
                self.state.emit("    movq %rax, %xmm1");
                self.state.reg_cache.invalidate_all(); // rax repurposed for xmm1 transfer
            }
        }
    }

    fn emit_get_return_f32_second(&mut self, dest: &Value) {
        // x86-64 packs _Complex float into one xmm0 register, so this is unused.
        // If somehow called, treat as F64 second return for safety.
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.state.emit_fmt(format_args!("    movss %xmm1, {}(%rbp)", slot.0));
        }
    }

    fn emit_set_return_f32_second(&mut self, src: &Operand) {
        // x86-64 packs _Complex float into one xmm0 register, so this is unused.
        // If somehow called, treat as F32 second return for safety.
        match src {
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    self.state.emit_fmt(format_args!("    movss {}(%rbp), %xmm1", slot.0));
                }
            }
            Operand::Const(IrConst::F32(f)) => {
                let bits = f.to_bits();
                self.state.emit_fmt(format_args!("    movl ${}, %eax", bits));
                self.state.emit("    movd %eax, %xmm1");
                self.state.reg_cache.invalidate_all(); // eax clobbered by movl
            }
            _ => {
                self.operand_to_rax(src);
                self.state.emit("    movd %eax, %xmm1");
                self.state.reg_cache.invalidate_all(); // rax repurposed for xmm1 transfer
            }
        }
    }

    fn emit_atomic_rmw(&mut self, dest: &Value, op: AtomicRmwOp, ptr: &Operand, val: &Operand, ty: IrType, _ordering: AtomicOrdering) {
        // Load ptr into rcx, val into rax/rdx
        self.operand_to_rax(ptr);
        self.state.emit("    movq %rax, %rcx"); // rcx = ptr
        self.operand_to_rax(val);
        // rax = val, rcx = ptr
        self.state.reg_cache.invalidate_all(); // atomic ops clobber rax with old value
        let size_suffix = Self::type_suffix(ty);
        let val_reg = Self::reg_for_type("rax", ty);
        match op {
            AtomicRmwOp::Add => {
                // lock xadd stores old value in source reg, adds to dest
                self.state.emit_fmt(format_args!("    lock xadd{} %{}, (%rcx)", size_suffix, val_reg));
                // After xadd, rax has the OLD value. Result = old + val.
                // But we want the NEW value for __atomic_add_fetch. The caller handles this.
                // Actually for fetch_and_add we want old value, for add_and_fetch we want new.
                // The IR op is always "return old value" (AtomicRmwOp::Add means fetch_add).
                // The lowering will handle computing new = old + val if needed.
            }
            AtomicRmwOp::Xchg => {
                // xchg is implicitly locked
                self.state.emit_fmt(format_args!("    xchg{} %{}, (%rcx)", size_suffix, val_reg));
                // rax now has old value
            }
            AtomicRmwOp::TestAndSet => {
                // test_and_set sets byte to 1, returns old
                self.state.emit("    movb $1, %al");
                self.state.emit("    xchgb %al, (%rcx)");
            }
            AtomicRmwOp::Sub => {
                // No lock xsub exists; use lock cmpxchg loop
                self.emit_x86_atomic_op_loop(ty, "sub");
            }
            AtomicRmwOp::And => {
                self.emit_x86_atomic_op_loop(ty, "and");
            }
            AtomicRmwOp::Or => {
                self.emit_x86_atomic_op_loop(ty, "or");
            }
            AtomicRmwOp::Xor => {
                self.emit_x86_atomic_op_loop(ty, "xor");
            }
            AtomicRmwOp::Nand => {
                self.emit_x86_atomic_op_loop(ty, "nand");
            }
        }
        self.store_rax_to(dest);
    }

    fn emit_atomic_cmpxchg(&mut self, dest: &Value, ptr: &Operand, expected: &Operand, desired: &Operand, ty: IrType, _success_ordering: AtomicOrdering, _failure_ordering: AtomicOrdering, returns_bool: bool) {
        // For cmpxchg: rax = expected, rdx = desired, rcx = ptr
        // lock cmpxchg compares [ptr] with rax, if equal sets [ptr]=desired and ZF=1
        // Otherwise loads [ptr] into rax and ZF=0
        self.operand_to_rax(ptr);
        self.state.emit("    movq %rax, %rcx"); // rcx = ptr
        self.operand_to_rax(desired);
        self.state.emit("    movq %rax, %rdx"); // rdx = desired
        self.operand_to_rax(expected);
        // Now: rax = expected, rdx = desired, rcx = ptr
        self.state.reg_cache.invalidate_all(); // cmpxchg clobbers rax with old value
        let size_suffix = Self::type_suffix(ty);
        let desired_reg = Self::reg_for_type("rdx", ty);
        self.state.emit_fmt(format_args!("    lock cmpxchg{} %{}, (%rcx)", size_suffix, desired_reg));
        if returns_bool {
            // Return 1 if exchange succeeded (ZF set), 0 otherwise
            self.state.emit("    sete %al");
            self.state.emit("    movzbl %al, %eax");
        }
        // If !returns_bool, rax contains the old value (either expected if success, or actual)
        self.store_rax_to(dest);
    }

    fn emit_atomic_load(&mut self, dest: &Value, ptr: &Operand, ty: IrType, _ordering: AtomicOrdering) {
        // On x86, aligned loads are naturally atomic
        self.operand_to_rax(ptr);
        self.state.reg_cache.invalidate_all(); // rax will be overwritten with loaded value
        let load_instr = Self::mov_load_for_type(ty);
        let dest_reg = Self::load_dest_reg(ty);
        self.state.emit_fmt(format_args!("    {} (%rax), {}", load_instr, dest_reg));
        self.store_rax_to(dest);
    }

    fn emit_atomic_store(&mut self, ptr: &Operand, val: &Operand, ty: IrType, ordering: AtomicOrdering) {
        // On x86-64 TSO, aligned stores are naturally atomic with release semantics.
        // Only SeqCst requires an additional fence (mfence after store).
        self.operand_to_rax(val);
        self.state.emit("    movq %rax, %rdx"); // rdx = val
        self.operand_to_rax(ptr);
        self.state.reg_cache.invalidate_all(); // rax has ptr address, not useful IR value
        // rax = ptr, rdx = val
        let store_reg = Self::reg_for_type("rdx", ty);
        let store_instr = Self::mov_store_for_type(ty);
        self.state.emit_fmt(format_args!("    {} %{}, (%rax)", store_instr, store_reg));
        if matches!(ordering, AtomicOrdering::SeqCst) {
            self.state.emit("    mfence");
        }
    }

    fn emit_fence(&mut self, ordering: AtomicOrdering) {
        // On x86-64 TSO:
        // - Relaxed: no fence needed
        // - Acquire/Release/AcqRel: already guaranteed by TSO (loads are acquire, stores are release)
        //   but we emit mfence to also act as a compiler barrier and be safe for non-temporal stores
        // - SeqCst: full memory fence required
        match ordering {
            AtomicOrdering::Relaxed => {} // no-op
            _ => self.state.emit("    mfence"),
        }
    }

    fn emit_inline_asm(&mut self, template: &str, outputs: &[(String, Value, Option<String>)], inputs: &[(String, Operand, Option<String>)], clobbers: &[String], operand_types: &[IrType], goto_labels: &[(String, BlockId)], input_symbols: &[Option<String>]) {
        emit_inline_asm_common(self, template, outputs, inputs, clobbers, operand_types, goto_labels, input_symbols);
        self.state.reg_cache.invalidate_all(); // inline asm may clobber rax
    }

    fn emit_inline_asm_with_segs(&mut self, template: &str, outputs: &[(String, Value, Option<String>)], inputs: &[(String, Operand, Option<String>)], clobbers: &[String], operand_types: &[IrType], goto_labels: &[(String, BlockId)], input_symbols: &[Option<String>], seg_overrides: &[AddressSpace]) {
        crate::backend::inline_asm::emit_inline_asm_common_impl(self, template, outputs, inputs, clobbers, operand_types, goto_labels, input_symbols, seg_overrides);
        self.state.reg_cache.invalidate_all();
    }

    fn emit_copy_i128(&mut self, dest: &Value, src: &Operand) {
        self.operand_to_rax_rdx(src);
        self.store_rax_rdx_to(dest);
    }

    fn emit_intrinsic(&mut self, dest: &Option<Value>, op: &IntrinsicOp, dest_ptr: &Option<Value>, args: &[Operand]) {
        self.emit_intrinsic_impl(dest, op, dest_ptr, args);
        self.state.reg_cache.invalidate_all(); // intrinsics may clobber rax
    }

    // ---- Float binop primitives ----

    fn emit_float_binop_mnemonic(&self, op: FloatOp) -> &'static str {
        match op {
            FloatOp::Add => "add",
            FloatOp::Sub => "sub",
            FloatOp::Mul => "mul",
            FloatOp::Div => "div",
        }
    }

    /// Override emit_unaryop to handle F128 (long double) negation via x87 fchs.
    /// The default trait implementation loads the F128 value into rax as a truncated f64,
    /// then uses xorpd to flip the f64 sign bit — but for F128 the full 80-bit x87
    /// representation is stored in the slot, so the sign bit is at bit 79 (not bit 63).
    /// We must use the x87 fchs instruction to negate the full-precision value.
    fn emit_unaryop(&mut self, dest: &Value, op: IrUnaryOp, src: &Operand, ty: IrType) {
        if ty == IrType::F128 && op == IrUnaryOp::Neg {
            // Load the full 80-bit value into x87 ST(0)
            self.emit_f128_load_to_x87(src);
            // Negate ST(0) in-place
            self.state.emit("    fchs");
            // Store full 80-bit precision to dest slot if available
            if let Some(dest_slot) = self.state.get_slot(dest.0) {
                self.state.emit_fmt(format_args!("    fstpt {}(%rbp)", dest_slot.0));
                // Also keep f64 copy in rax for non-F128 consumers
                self.state.emit_fmt(format_args!("    fldt {}(%rbp)", dest_slot.0));
                self.state.emit("    subq $8, %rsp");
                self.state.emit("    fstpl (%rsp)");
                self.state.emit("    popq %rax");
                self.state.reg_cache.set_acc(dest.0, false);
                self.state.f128_direct_slots.insert(dest.0);
            } else {
                // No slot: just store as f64 in rax
                self.state.emit("    subq $8, %rsp");
                self.state.emit("    fstpl (%rsp)");
                self.state.emit("    popq %rax");
                self.state.reg_cache.invalidate_acc();
                self.emit_store_result(dest);
            }
            return;
        }
        // Delegate to default trait implementation for all other cases
        crate::backend::traits::emit_unaryop_default(self, dest, op, src, ty);
    }

    /// Override the default trait emit_float_binop to avoid push/pop.
    /// For F32/F64: loads lhs to rax -> xmm0, rhs to rcx -> xmm1, uses SSE.
    /// For F128: uses x87 FPU instructions for 80-bit extended precision.
    fn emit_float_binop(&mut self, dest: &Value, op: FloatOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if ty == IrType::F128 {
            // F128 (long double) arithmetic: use x87 FPU for 80-bit extended precision.
            // x87 operand order: we load lhs first, then rhs, giving ST0=rhs, ST1=lhs.
            // For sub/div, we need lhs op rhs = ST1 op ST0, which requires the
            // "reverse" variants: fsubrp computes ST1-ST0, fdivrp computes ST1/ST0.
            // faddp and fmulp are commutative, so order doesn't matter.
            let x87_op = match op {
                FloatOp::Add => "faddp",
                FloatOp::Sub => "fsubrp",   // ST1 - ST0 = lhs - rhs
                FloatOp::Mul => "fmulp",
                FloatOp::Div => "fdivrp",   // ST1 / ST0 = lhs / rhs
            };
            // Load lhs into x87 ST0 (using full precision when available)
            self.emit_f128_load_to_x87(lhs);
            // Load rhs into x87 ST0 (pushes lhs to ST1)
            self.emit_f128_load_to_x87(rhs);
            // ST0 = rhs, ST1 = lhs.
            self.state.emit_fmt(format_args!("    {} %st, %st(1)", x87_op));
            // Result is now in ST0. Store full 80-bit precision to dest slot if available.
            if let Some(dest_slot) = self.state.get_slot(dest.0) {
                self.state.emit_fmt(format_args!("    fstpt {}(%rbp)", dest_slot.0));
                // Also keep f64 copy in rax for non-F128 consumers
                self.state.emit_fmt(format_args!("    fldt {}(%rbp)", dest_slot.0));
                self.state.emit("    subq $8, %rsp");
                self.state.emit("    fstpl (%rsp)");
                self.state.emit("    popq %rax");
                self.state.reg_cache.set_acc(dest.0, false);
                self.state.f128_direct_slots.insert(dest.0);
            } else {
                // No slot: just store as f64 in rax
                self.state.emit("    subq $8, %rsp");
                self.state.emit("    fstpl (%rsp)");
                self.state.emit("    popq %rax");
                self.state.reg_cache.invalidate_acc();
                self.emit_store_result(dest);
            }
            return;
        }
        let mnemonic = self.emit_float_binop_mnemonic(op);
        let (mov_rax_to_xmm0, mov_rcx_to_xmm1, mov_xmm0_to_rax) = if ty == IrType::F32 {
            ("movd %eax, %xmm0", "movd %ecx, %xmm1", "movd %xmm0, %eax")
        } else {
            ("movq %rax, %xmm0", "movq %rcx, %xmm1", "movq %xmm0, %rax")
        };
        // Load lhs to rax, then rhs to rcx (no push/pop needed)
        self.operand_to_rax(lhs);
        self.state.emit_fmt(format_args!("    {}", mov_rax_to_xmm0));
        self.operand_to_rcx(rhs);
        self.state.emit_fmt(format_args!("    {}", mov_rcx_to_xmm1));
        let suffix = if ty == IrType::F64 { "sd" } else { "ss" };
        self.state.emit_fmt(format_args!("    {}{} %xmm1, %xmm0", mnemonic, suffix));
        self.state.emit_fmt(format_args!("    {}", mov_xmm0_to_rax));
        self.state.reg_cache.invalidate_acc();
        self.emit_store_result(dest);
    }

    fn emit_float_binop_impl(&mut self, _mnemonic: &str, _ty: IrType) {
        // This is no longer called on x86 since we override emit_float_binop above.
        // Kept for trait compatibility.
        unreachable!("x86 emit_float_binop_impl should not be called directly");
    }

    // ---- i128 binop primitives ----

    fn emit_i128_prep_binop(&mut self, lhs: &Operand, rhs: &Operand) {
        self.prep_i128_binop(lhs, rhs);
    }

    fn emit_i128_add(&mut self) {
        self.state.emit("    addq %rcx, %rax");
        self.state.emit("    adcq %rsi, %rdx");
    }

    fn emit_i128_sub(&mut self) {
        self.state.emit("    subq %rcx, %rax");
        self.state.emit("    sbbq %rsi, %rdx");
    }

    fn emit_i128_mul(&mut self) {
        // After prep: lhs in rax:rdx, rhs in rcx:rsi
        // Save to stack and rearrange for widening multiply
        self.state.emit("    pushq %rdx");  // a_hi
        self.state.emit("    pushq %rax");  // a_lo
        self.state.emit("    movq %rcx, %r8"); // r8 = b_lo (from prep rhs low)
        self.state.emit("    movq %rsi, %r9"); // r9 = b_hi (from prep rhs high)
        self.state.emit("    popq %rax");       // rax = a_lo
        self.state.emit("    popq %rdi");       // rdi = a_hi
        self.state.emit("    movq %rdi, %rcx");
        self.state.emit("    imulq %r8, %rcx");    // rcx = a_hi * b_lo
        self.state.emit("    movq %rax, %rsi");
        self.state.emit("    imulq %r9, %rsi");    // rsi = a_lo * b_hi
        self.state.emit("    mulq %r8");           // rdx:rax = a_lo * b_lo
        self.state.emit("    addq %rcx, %rdx");
        self.state.emit("    addq %rsi, %rdx");
    }

    fn emit_i128_and(&mut self) {
        self.state.emit("    andq %rcx, %rax");
        self.state.emit("    andq %rsi, %rdx");
    }

    fn emit_i128_or(&mut self) {
        self.state.emit("    orq %rcx, %rax");
        self.state.emit("    orq %rsi, %rdx");
    }

    fn emit_i128_xor(&mut self) {
        self.state.emit("    xorq %rcx, %rax");
        self.state.emit("    xorq %rsi, %rdx");
    }

    fn emit_i128_shl(&mut self) {
        // After prep: value in rax:rdx, shift amount low bits in rcx
        // But prep puts rhs in rcx:rsi - we need shift amount in cl
        self.state.emit("    shldq %cl, %rax, %rdx");
        self.state.emit("    shlq %cl, %rax");
        self.state.emit("    testb $64, %cl");
        self.state.emit("    je 1f");
        self.state.emit("    movq %rax, %rdx");
        self.state.emit("    xorq %rax, %rax");
        self.state.emit("1:");
    }

    fn emit_i128_lshr(&mut self) {
        self.state.emit("    shrdq %cl, %rdx, %rax");
        self.state.emit("    shrq %cl, %rdx");
        self.state.emit("    testb $64, %cl");
        self.state.emit("    je 1f");
        self.state.emit("    movq %rdx, %rax");
        self.state.emit("    xorq %rdx, %rdx");
        self.state.emit("1:");
    }

    fn emit_i128_ashr(&mut self) {
        self.state.emit("    shrdq %cl, %rdx, %rax");
        self.state.emit("    sarq %cl, %rdx");
        self.state.emit("    testb $64, %cl");
        self.state.emit("    je 1f");
        self.state.emit("    movq %rdx, %rax");
        self.state.emit("    sarq $63, %rdx");
        self.state.emit("1:");
    }

    fn emit_i128_divrem_call(&mut self, func_name: &str, lhs: &Operand, rhs: &Operand) {
        // x86-64 SysV: args in rdi:rsi (lhs), rdx:rcx (rhs)
        self.operand_to_rax_rdx(rhs);
        self.state.emit("    pushq %rdx");
        self.state.emit("    pushq %rax");
        self.operand_to_rax_rdx(lhs);
        self.state.emit("    movq %rax, %rdi");
        self.state.emit("    movq %rdx, %rsi");
        self.state.emit("    popq %rdx");
        self.state.emit("    popq %rcx");
        self.state.emit_fmt(format_args!("    call {}@PLT", func_name));
    }

    fn emit_i128_store_result(&mut self, dest: &Value) {
        self.store_rax_rdx_to(dest);
    }

    // ---- i128 cmp primitives ----

    fn emit_i128_cmp_eq(&mut self, is_ne: bool) {
        // After prep: lhs in rax:rdx, rhs in rcx:rsi
        self.state.emit("    xorq %rcx, %rax");   // low diff
        self.state.emit("    xorq %rsi, %rdx");   // high diff
        self.state.emit("    orq %rdx, %rax");     // combine
        if is_ne {
            self.state.emit("    setne %al");
        } else {
            self.state.emit("    sete %al");
        }
        self.state.emit("    movzbq %al, %rax");
    }

    fn emit_i128_cmp_ordered(&mut self, op: IrCmpOp) {
        // After prep: lhs in rax:rdx, rhs in rcx:rsi
        self.state.emit("    cmpq %rsi, %rdx");   // cmp lhs_hi, rhs_hi
        let set_hi = match op {
            IrCmpOp::Slt | IrCmpOp::Sle => "setl",
            IrCmpOp::Sgt | IrCmpOp::Sge => "setg",
            IrCmpOp::Ult | IrCmpOp::Ule => "setb",
            IrCmpOp::Ugt | IrCmpOp::Uge => "seta",
            _ => unreachable!(),
        };
        self.state.emit_fmt(format_args!("    {} %r8b", set_hi));
        self.state.emit("    jne 1f");
        // High halves equal: compare low halves (always unsigned)
        self.state.emit("    cmpq %rcx, %rax");
        let set_lo = match op {
            IrCmpOp::Slt | IrCmpOp::Ult => "setb",
            IrCmpOp::Sle | IrCmpOp::Ule => "setbe",
            IrCmpOp::Sgt | IrCmpOp::Ugt => "seta",
            IrCmpOp::Sge | IrCmpOp::Uge => "setae",
            _ => unreachable!(),
        };
        self.state.emit_fmt(format_args!("    {} %r8b", set_lo));
        self.state.emit("1:");
        self.state.emit("    movzbq %r8b, %rax");
    }

    fn emit_i128_cmp_store_result(&mut self, dest: &Value) {
        self.store_rax_to(dest);
    }
}

impl Default for X86Codegen {
    fn default() -> Self {
        Self::new()
    }
}

/// x86-64 scratch registers for inline asm "r" constraints (caller-saved, not rax/rsp/rbp).
const X86_GP_SCRATCH: &[&str] = &["rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11"];

/// x86-64 scratch XMM registers for inline asm "x" constraints (SSE registers, caller-saved).
const X86_XMM_SCRATCH: &[&str] = &["xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"];

impl InlineAsmEmitter for X86Codegen {
    fn asm_state(&mut self) -> &mut CodegenState { &mut self.state }
    fn asm_state_ref(&self) -> &CodegenState { &self.state }

    // TODO: ARM and RISC-V backends should also support multi-alternative constraint
    // parsing (e.g., "rm", "ri") similar to the x86 implementation below. Currently
    // they only recognize single-alternative constraints.
    fn classify_constraint(&self, constraint: &str) -> AsmOperandKind {
        let c = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
        // Explicit register constraint from register variable: {regname}
        // Generated by the IR lowering for `register long x __asm__("regname")`.
        if c.starts_with('{') && c.ends_with('}') {
            let reg_name = &c[1..c.len()-1];
            return AsmOperandKind::Specific(reg_name.to_string());
        }
        // GCC condition code output: =@cc<cond> (e.g. =@cce, =@ccne, =@ccs)
        if let Some(cond) = c.strip_prefix("@cc") {
            return AsmOperandKind::ConditionCode(cond.to_string());
        }
        // Check for tied operand (all digits)
        if !c.is_empty() && c.chars().all(|ch| ch.is_ascii_digit()) {
            if let Ok(n) = c.parse::<usize>() {
                return AsmOperandKind::Tied(n);
            }
        }
        // x87 FPU stack constraints: "t" = st(0), "u" = st(1)
        // These are always single-character constraints, so check before multi-alt parsing.
        if c == "t" {
            return AsmOperandKind::X87St0;
        }
        if c == "u" {
            return AsmOperandKind::X87St1;
        }

        // GCC allows multi-alternative constraints like "Ir" (immediate or register),
        // "rm" (register or memory), "qm" (byte register or memory), etc.
        // Parse each character as a constraint alternative and pick the best one.
        // Priority: specific register > GP register > FP register > memory > immediate
        // Registers are preferred over memory for performance. Immediate is the fallback
        // because the shared framework promotes GpReg to Immediate when the value is
        // a compile-time constant and the constraint allows it (see emit_inline_asm_common).
        // NOTE: The immediate letters here are x86-specific and intentionally a superset
        // of the architecture-neutral set in constraint_has_immediate_alt().
        let mut has_gp = false;
        let mut has_fp = false;
        let mut has_mem = false;
        let mut has_imm = false;
        let mut specific: Option<String> = None;

        for ch in c.chars() {
            match ch {
                'r' | 'q' | 'R' | 'Q' | 'l' => has_gp = true,
                'g' => { has_gp = true; has_mem = true; has_imm = true; } // "general operand"
                'x' | 'v' | 'Y' => has_fp = true,
                'm' | 'o' | 'V' | 'p' => has_mem = true, // 'p' = valid memory address
                'i' | 'I' | 'n' | 'N' | 'e' | 'E' | 'K' | 'M' | 'G' | 'H' | 'J' | 'L' | 'O' => has_imm = true,
                'a' if specific.is_none() => specific = Some("rax".to_string()),
                'b' if specific.is_none() => specific = Some("rbx".to_string()),
                'c' if specific.is_none() => specific = Some("rcx".to_string()),
                'd' if specific.is_none() => specific = Some("rdx".to_string()),
                'S' if specific.is_none() => specific = Some("rsi".to_string()),
                'D' if specific.is_none() => specific = Some("rdi".to_string()),
                _ => {}
            }
        }

        // Return the most appropriate classification.
        // For multi-alternative constraints like "Ir", the IR lowering will have
        // already evaluated the operand as a constant if possible, so the actual
        // substitution code will check imm_value to decide whether to emit $N or %reg.
        if let Some(reg) = specific {
            AsmOperandKind::Specific(reg)
        } else if has_gp {
            AsmOperandKind::GpReg
        } else if has_fp {
            AsmOperandKind::FpReg
        } else if has_mem {
            AsmOperandKind::Memory
        } else if has_imm {
            AsmOperandKind::Immediate
        } else {
            AsmOperandKind::GpReg
        }
    }

    fn setup_operand_metadata(&self, op: &mut AsmOperand, val: &Operand, _is_output: bool) {
        if matches!(op.kind, AsmOperandKind::Memory) {
            match val {
                Operand::Value(v) => {
                    if let Some(slot) = self.state.get_slot(v.0) {
                        if self.state.is_alloca(v.0) {
                            // Alloca: stack slot IS the memory location
                            op.mem_addr = format!("{}(%rbp)", slot.0);
                        } else {
                            // Non-alloca: slot holds a pointer that needs indirection.
                            // Mark with empty mem_addr; resolve_memory_operand will load
                            // the pointer into a register and set up the indirect address.
                            op.mem_addr = String::new();
                        }
                    }
                }
                _ => {}
            }
        }
        // Extract immediate constant value.
        // For pure Immediate constraints, this provides the value for $N substitution.
        if matches!(op.kind, AsmOperandKind::Immediate) {
            if let Operand::Const(c) = val {
                op.imm_value = c.to_i64();
            }
        }
    }

    fn resolve_memory_operand(&mut self, op: &mut AsmOperand, val: &Operand, excluded: &[String]) -> bool {
        // If mem_addr is already set (alloca case), nothing to do
        if !op.mem_addr.is_empty() {
            return false;
        }
        // If we have a symbol name for this memory operand, use a direct RIP-relative
        // symbol reference. This is critical for correctness when the inline asm template
        // adds a segment prefix (e.g., %%gs:) before the operand — using a register
        // indirect like (%rcx) would use the absolute address as a segment offset,
        // but a direct symbol(%rip) reference provides the correct per-CPU offset.
        if let Some(ref sym) = op.imm_symbol {
            op.mem_addr = format!("{}(%rip)", sym);
            return false; // no scratch register used
        }
        // Load the pointer value into a temporary register for indirect addressing.
        // Each memory operand gets its own unique register via assign_scratch_reg,
        // so multiple "=m" outputs don't overwrite each other's addresses.
        match val {
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    let tmp_reg = self.assign_scratch_reg(&AsmOperandKind::GpReg, excluded);
                    self.state.emit_fmt(format_args!("    movq {}(%rbp), %{}", slot.0, tmp_reg));
                    op.mem_addr = format!("(%{})", tmp_reg);
                    return true;
                }
            }
            Operand::Const(c) => {
                // Constant address (e.g., from volatile MMIO reads like readl() in the kernel).
                // Copy propagation can replace Value operands with Const when the address
                // was originally a compile-time constant (e.g., fix_to_virt(X) + offset).
                // Load the constant address into a scratch register for indirect addressing.
                if let Some(addr) = c.to_i64() {
                    let tmp_reg = self.assign_scratch_reg(&AsmOperandKind::GpReg, excluded);
                    self.state.emit_fmt(format_args!("    movabsq ${}, %{}", addr, tmp_reg));
                    op.mem_addr = format!("(%{})", tmp_reg);
                    return true;
                }
            }
        }
        false
    }

    fn assign_scratch_reg(&mut self, kind: &AsmOperandKind, excluded: &[String]) -> String {
        if matches!(kind, AsmOperandKind::FpReg) {
            let idx = self.asm_xmm_scratch_idx;
            self.asm_xmm_scratch_idx += 1;
            if idx < X86_XMM_SCRATCH.len() {
                X86_XMM_SCRATCH[idx].to_string()
            } else {
                format!("xmm{}", idx)
            }
        } else {
            // Skip registers that are claimed by specific-register constraints
            loop {
                let idx = self.asm_scratch_idx;
                self.asm_scratch_idx += 1;
                let reg = if idx < X86_GP_SCRATCH.len() {
                    X86_GP_SCRATCH[idx].to_string()
                } else {
                    format!("r{}", 12 + idx - X86_GP_SCRATCH.len())
                };
                if !excluded.iter().any(|e| e == &reg) {
                    return reg;
                }
            }
        }
    }

    fn load_input_to_reg(&mut self, op: &AsmOperand, val: &Operand, _constraint: &str) {
        let reg = &op.reg;
        let ty = op.operand_type;

        // x87 FPU stack: load value from memory onto the x87 stack with fld
        if matches!(op.kind, AsmOperandKind::X87St0 | AsmOperandKind::X87St1) {
            match val {
                Operand::Value(v) => {
                    if let Some(slot) = self.state.get_slot(v.0) {
                        let fld_instr = match ty {
                            IrType::F32 => "flds",
                            IrType::F128 => "fldt",
                            _ => "fldl", // F64 and default
                        };
                        self.state.emit_fmt(format_args!("    {} {}(%rbp)", fld_instr, slot.0));
                    }
                }
                Operand::Const(c) => {
                    // x87 can't load immediates directly; materialize via GP reg + stack scratch.
                    // Use the bits of the float constant as an integer, store to a scratch
                    // location on the stack, then fld from it. We use subq/addq to allocate
                    // scratch space instead of push/pop to avoid the peephole optimizer
                    // incorrectly eliminating the stack adjustment as a dead push/pop pair.
                    let bits = match ty {
                        IrType::F32 => {
                            let f = c.to_f64().unwrap_or(0.0) as f32;
                            f.to_bits() as u64
                        }
                        _ => {
                            let f = c.to_f64().unwrap_or(0.0);
                            f.to_bits()
                        }
                    };
                    self.state.emit("    subq $8, %rsp");
                    self.state.emit_fmt(format_args!("    movabsq ${}, %rax", bits as i64));
                    self.state.emit("    movq %rax, (%rsp)");
                    let fld_instr = match ty {
                        IrType::F32 => "flds",
                        _ => "fldl",
                    };
                    self.state.emit_fmt(format_args!("    {} (%rsp)", fld_instr));
                    self.state.emit("    addq $8, %rsp");
                }
            }
            return;
        }

        let is_xmm = reg.starts_with("xmm");

        if is_xmm {
            // XMM register: use SSE load instructions
            match val {
                Operand::Value(v) => {
                    if let Some(slot) = self.state.get_slot(v.0) {
                        let load_instr = match ty {
                            IrType::F32 => "movss",
                            _ => "movsd",
                        };
                        self.state.emit_fmt(format_args!("    {} {}(%rbp), %{}", load_instr, slot.0, reg));
                    }
                }
                Operand::Const(_) => {
                    // TODO: non-zero float constants need to be materialized via
                    // memory (e.g., load from .rodata). For now, zero the register.
                    // In practice, inline asm "x" inputs are almost always variables.
                    self.state.emit_fmt(format_args!("    xorpd %{}, %{}", reg, reg));
                }
            }
            return;
        }

        match val {
            Operand::Const(c) => {
                let imm = c.to_i64().unwrap_or(0);
                if imm == 0 {
                    self.state.emit_fmt(format_args!("    xorq %{}, %{}", reg, reg));
                } else {
                    self.state.emit_fmt(format_args!("    movabsq ${}, %{}", imm, reg));
                }
            }
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    if self.state.is_alloca(v.0) {
                        self.state.emit_fmt(format_args!("    leaq {}(%rbp), %{}", slot.0, reg));
                    } else {
                        // Use type-appropriate load to avoid reading garbage from
                        // stack slots of smaller-than-8-byte variables
                        let load_instr = Self::mov_load_for_type(ty);
                        let dest_reg = match ty {
                            IrType::U32 | IrType::F32 => Self::reg_to_32(reg),
                            _ => format!("%{}", reg),
                        };
                        let dest_reg_str = if matches!(ty, IrType::U32 | IrType::F32) {
                            format!("%{}", dest_reg)
                        } else {
                            dest_reg
                        };
                        self.state.emit_fmt(format_args!("    {} {}(%rbp), {}", load_instr, slot.0, dest_reg_str));
                    }
                }
            }
        }
    }

    fn preload_readwrite_output(&mut self, op: &AsmOperand, ptr: &Value) {
        // x87 FPU stack: preload with fld (same as input loading)
        if matches!(op.kind, AsmOperandKind::X87St0 | AsmOperandKind::X87St1) {
            let ty = op.operand_type;
            if let Some(slot) = self.state.get_slot(ptr.0) {
                let fld_instr = match ty {
                    IrType::F32 => "flds",
                    IrType::F128 => "fldt",
                    _ => "fldl",
                };
                if self.state.is_alloca(ptr.0) {
                    self.state.emit_fmt(format_args!("    {} {}(%rbp)", fld_instr, slot.0));
                } else {
                    let scratch = "rcx";
                    self.state.emit_fmt(format_args!("    pushq %{}", scratch));
                    self.state.emit_fmt(format_args!("    movq {}(%rbp), %{}", slot.0, scratch));
                    self.state.emit_fmt(format_args!("    {} (%{})", fld_instr, scratch));
                    self.state.emit_fmt(format_args!("    popq %{}", scratch));
                }
            }
            return;
        }
        let reg = &op.reg;
        let ty = op.operand_type;
        let is_xmm = reg.starts_with("xmm");
        if let Some(slot) = self.state.get_slot(ptr.0) {
            if self.state.is_alloca(ptr.0) {
                // Alloca: stack slot IS the variable's storage — load directly
                if is_xmm {
                    let load_instr = match ty {
                        IrType::F32 => "movss",
                        _ => "movsd",
                    };
                    self.state.emit_fmt(format_args!("    {} {}(%rbp), %{}", load_instr, slot.0, reg));
                } else {
                    let load_instr = Self::mov_load_for_type(ty);
                    let dest_reg = match ty {
                        IrType::U32 | IrType::F32 => format!("%{}", Self::reg_to_32(reg)),
                        _ => format!("%{}", reg),
                    };
                    self.state.emit_fmt(format_args!("    {} {}(%rbp), {}", load_instr, slot.0, dest_reg));
                }
            } else {
                // Non-alloca: stack slot holds a pointer — do indirect load
                self.state.emit_fmt(format_args!("    movq {}(%rbp), %{}", slot.0, reg));
                if is_xmm {
                    let load_instr = match ty {
                        IrType::F32 => "movss",
                        _ => "movsd",
                    };
                    self.state.emit_fmt(format_args!("    {} (%{}), %{}", load_instr, reg, reg));
                } else {
                    let load_instr = Self::mov_load_for_type(ty);
                    let dest_reg = match ty {
                        IrType::U32 | IrType::F32 => format!("%{}", Self::reg_to_32(reg)),
                        _ => format!("%{}", reg),
                    };
                    self.state.emit_fmt(format_args!("    {} (%{}), {}", load_instr, reg, dest_reg));
                }
            }
        }
    }

    fn substitute_template_line(&self, line: &str, operands: &[AsmOperand], gcc_to_internal: &[usize], operand_types: &[IrType], goto_labels: &[(String, BlockId)]) -> String {
        // Build the parallel arrays that substitute_x86_asm_operands expects
        let op_regs: Vec<String> = operands.iter().map(|o| o.reg.clone()).collect();
        let op_names: Vec<Option<String>> = operands.iter().map(|o| o.name.clone()).collect();
        let op_is_memory: Vec<bool> = operands.iter().map(|o| matches!(o.kind, AsmOperandKind::Memory)).collect();
        let op_mem_addrs: Vec<String> = operands.iter().map(|o| {
            if o.seg_prefix.is_empty() {
                o.mem_addr.clone()
            } else {
                format!("{}{}", o.seg_prefix, o.mem_addr)
            }
        }).collect();
        let op_imm_values: Vec<Option<i64>> = operands.iter().map(|o| o.imm_value).collect();
        let op_imm_symbols: Vec<Option<String>> = operands.iter().map(|o| o.imm_symbol.clone()).collect();

        // Build operand type array for register size selection
        let total = operands.len();
        let mut op_types: Vec<IrType> = vec![IrType::I64; total];
        for (i, ty) in operand_types.iter().enumerate() {
            if i < total { op_types[i] = *ty; }
        }
        // Inherit types for tied operands
        for (i, op) in operands.iter().enumerate() {
            if let AsmOperandKind::Tied(tied_to) = &op.kind {
                if *tied_to < op_types.len() && i < op_types.len() {
                    op_types[i] = op_types[*tied_to];
                }
            }
        }

        Self::substitute_x86_asm_operands(line, &op_regs, &op_names, &op_is_memory, &op_mem_addrs, &op_types, gcc_to_internal, goto_labels, &op_imm_values, &op_imm_symbols)
    }

    fn store_output_from_reg(&mut self, op: &AsmOperand, ptr: &Value, _constraint: &str) {
        if matches!(op.kind, AsmOperandKind::Memory) {
            return;
        }
        // x87 FPU stack outputs: store using fstp (store and pop)
        // The shared framework calls store_output_from_reg for outputs in order (index 0 first).
        // For "=t" (st(0)), fstp pops the top. For "=u" (st(1)), after st(0) was popped,
        // the old st(1) is now st(0), so another fstp stores it correctly.
        if matches!(op.kind, AsmOperandKind::X87St0 | AsmOperandKind::X87St1) {
            if let Some(slot) = self.state.get_slot(ptr.0) {
                let ty = op.operand_type;
                let fstp_instr = match ty {
                    IrType::F32 => "fstps",
                    IrType::F128 => "fstpt",
                    _ => "fstpl", // F64 and default
                };
                if self.state.is_alloca(ptr.0) {
                    self.state.emit_fmt(format_args!("    {} {}(%rbp)", fstp_instr, slot.0));
                } else {
                    // Non-alloca: slot holds a pointer, store through it
                    let scratch = "rcx";
                    self.state.emit_fmt(format_args!("    pushq %{}", scratch));
                    self.state.emit_fmt(format_args!("    movq {}(%rbp), %{}", slot.0, scratch));
                    self.state.emit_fmt(format_args!("    {} (%{})", fstp_instr, scratch));
                    self.state.emit_fmt(format_args!("    popq %{}", scratch));
                }
            }
            return;
        }
        // Handle =@cc<cond> condition code outputs: emit SETcc + movzbl
        if let AsmOperandKind::ConditionCode(ref cond) = op.kind {
            let reg = &op.reg;
            let reg8 = Self::reg_to_8l(reg);
            // Map GCC condition suffix to x86 SETcc instruction suffix
            let x86_cond = Self::gcc_cc_to_x86(cond);
            self.state.emit_fmt(format_args!("    set{} %{}", x86_cond, reg8));
            self.state.emit_fmt(format_args!("    movzbl %{}, %{}", reg8, Self::reg_to_32(reg)));
            // Store the result (0 or 1) to the output variable
            if let Some(slot) = self.state.get_slot(ptr.0) {
                let ty = op.operand_type;
                if self.state.is_alloca(ptr.0) {
                    let store_instr = Self::mov_store_for_type(ty);
                    let src_reg = match ty {
                        IrType::I8 | IrType::U8 => format!("%{}", Self::reg_to_8l(reg)),
                        IrType::I16 | IrType::U16 => format!("%{}", Self::reg_to_16(reg)),
                        IrType::I32 | IrType::U32 | IrType::F32 => format!("%{}", Self::reg_to_32(reg)),
                        _ => format!("%{}", reg),
                    };
                    self.state.emit_fmt(format_args!("    {} {}, {}(%rbp)", store_instr, src_reg, slot.0));
                } else {
                    let scratch = if reg != "rcx" { "rcx" } else { "rdx" };
                    self.state.emit_fmt(format_args!("    pushq %{}", scratch));
                    self.state.emit_fmt(format_args!("    movq {}(%rbp), %{}", slot.0, scratch));
                    let store_instr = Self::mov_store_for_type(ty);
                    let src_reg = match ty {
                        IrType::I8 | IrType::U8 => format!("%{}", Self::reg_to_8l(reg)),
                        IrType::I16 | IrType::U16 => format!("%{}", Self::reg_to_16(reg)),
                        IrType::I32 | IrType::U32 | IrType::F32 => format!("%{}", Self::reg_to_32(reg)),
                        _ => format!("%{}", reg),
                    };
                    self.state.emit_fmt(format_args!("    {} {}, (%{})", store_instr, src_reg, scratch));
                    self.state.emit_fmt(format_args!("    popq %{}", scratch));
                }
            }
            return;
        }
        let reg = &op.reg;
        let ty = op.operand_type;
        let is_xmm = reg.starts_with("xmm");
        if let Some(slot) = self.state.get_slot(ptr.0) {
            if is_xmm {
                // XMM register: use SSE store instructions
                let store_instr = match ty {
                    IrType::F32 => "movss",
                    _ => "movsd",
                };
                if self.state.is_alloca(ptr.0) {
                    self.state.emit_fmt(format_args!("    {} %{}, {}(%rbp)", store_instr, reg, slot.0));
                } else {
                    let scratch = "rcx";
                    self.state.emit_fmt(format_args!("    pushq %{}", scratch));
                    self.state.emit_fmt(format_args!("    movq {}(%rbp), %{}", slot.0, scratch));
                    self.state.emit_fmt(format_args!("    {} %{}, (%{})", store_instr, reg, scratch));
                    self.state.emit_fmt(format_args!("    popq %{}", scratch));
                }
            } else if self.state.is_alloca(ptr.0) {
                // Alloca: store directly to the stack slot with type-appropriate size
                let store_instr = Self::mov_store_for_type(ty);
                let src_reg = match ty {
                    IrType::I8 | IrType::U8 => format!("%{}", Self::reg_to_8l(reg)),
                    IrType::I16 | IrType::U16 => format!("%{}", Self::reg_to_16(reg)),
                    IrType::I32 | IrType::U32 | IrType::F32 => format!("%{}", Self::reg_to_32(reg)),
                    _ => format!("%{}", reg),
                };
                self.state.emit_fmt(format_args!("    {} {}, {}(%rbp)", store_instr, src_reg, slot.0));
            } else {
                // Non-alloca: slot holds a pointer, store through it.
                let scratch = if reg != "rcx" { "rcx" } else { "rdx" };
                self.state.emit_fmt(format_args!("    pushq %{}", scratch));
                self.state.emit_fmt(format_args!("    movq {}(%rbp), %{}", slot.0, scratch));
                let store_instr = Self::mov_store_for_type(ty);
                let src_reg = match ty {
                    IrType::I8 | IrType::U8 => format!("%{}", Self::reg_to_8l(reg)),
                    IrType::I16 | IrType::U16 => format!("%{}", Self::reg_to_16(reg)),
                    IrType::I32 | IrType::U32 | IrType::F32 => format!("%{}", Self::reg_to_32(reg)),
                    _ => format!("%{}", reg),
                };
                self.state.emit_fmt(format_args!("    {} {}, (%{})", store_instr, src_reg, scratch));
                self.state.emit_fmt(format_args!("    popq %{}", scratch));
            }
        }
    }

    fn reset_scratch_state(&mut self) {
        self.asm_scratch_idx = 0;
        self.asm_xmm_scratch_idx = 0;
    }

}
