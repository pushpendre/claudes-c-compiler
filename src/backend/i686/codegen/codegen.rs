//! i686 (32-bit x86) code generator. Implements the ArchCodegen trait.
//!
//! Uses the cdecl calling convention (System V i386 ABI):
//! - All arguments passed on the stack, pushed right-to-left
//! - Return values: eax (32-bit), eax:edx (64-bit), st(0) for float/double/long double
//! - Callee-saved: ebx, esi, edi, ebp
//! - Caller-saved: eax, ecx, edx
//! - No register-based argument passing (unlike x86-64 SysV ABI)
//! - Stack aligned to 16 bytes at call sites (modern i386 ABI)

use crate::backend::traits::ArchCodegen;
use crate::backend::common::PtrDirective;
use crate::backend::state::{CodegenState, StackSlot};
use crate::backend::regalloc::PhysReg;
use crate::backend::generation::{generate_module, is_i128_type, calculate_stack_space_common,
                                  run_regalloc_and_merge_clobbers, filter_available_regs,
                                  find_param_alloca, collect_inline_asm_callee_saved};
use crate::backend::call_abi;
use crate::backend::call_emit::{ParamClass, classify_params};
use crate::ir::ir::*;
use crate::common::types::{AddressSpace, IrType};
use crate::common::fx_hash::FxHashMap;
use crate::{emit};

/// i686 code generator. Implements the ArchCodegen trait for the shared framework.
/// Uses cdecl calling convention with no register allocation (accumulator-based).
pub struct I686Codegen {
    pub(super) state: CodegenState,
    current_return_type: IrType,
    /// Whether the current function is variadic
    is_variadic: bool,
    /// Register allocation results (callee-saved registers: ebx, esi, edi)
    reg_assignments: FxHashMap<u32, PhysReg>,
    /// Which callee-saved registers are used and need save/restore
    used_callee_saved: Vec<PhysReg>,
    /// Total stack bytes consumed by named parameters (for va_start computation).
    /// On i686 cdecl, all parameters are stack-passed, so this equals the total
    /// bytes of all named parameters including F64/I64 which take 8 bytes each.
    va_named_stack_bytes: usize,
}

// Callee-saved physical register indices for i686
// PhysReg(0) = ebx, PhysReg(1) = esi, PhysReg(2) = edi
const I686_CALLEE_SAVED: &[PhysReg] = &[PhysReg(0), PhysReg(1), PhysReg(2)];
// No caller-saved registers available for allocation (eax/ecx/edx are scratch)
const I686_CALLER_SAVED: &[PhysReg] = &[];

fn phys_reg_name(reg: PhysReg) -> &'static str {
    match reg.0 {
        0 => "ebx",
        1 => "esi",
        2 => "edi",
        _ => panic!("invalid i686 phys reg: {:?}", reg),
    }
}

/// Map inline asm constraint register names to callee-saved PhysReg indices.
fn i686_constraint_to_phys(constraint: &str) -> Option<PhysReg> {
    match constraint {
        "b" | "{ebx}" | "ebx" => Some(PhysReg(0)),
        "S" | "{esi}" | "esi" => Some(PhysReg(1)),
        "D" | "{edi}" | "edi" => Some(PhysReg(2)),
        _ => None,
    }
}

/// Map inline asm clobber register names to callee-saved PhysReg indices.
fn i686_clobber_to_phys(clobber: &str) -> Option<PhysReg> {
    match clobber {
        "ebx" | "~{ebx}" => Some(PhysReg(0)),
        "esi" | "~{esi}" => Some(PhysReg(1)),
        "edi" | "~{edi}" => Some(PhysReg(2)),
        _ => None,
    }
}

impl I686Codegen {
    pub fn new() -> Self {
        Self {
            state: CodegenState::new(),
            current_return_type: IrType::I32,
            is_variadic: false,
            reg_assignments: FxHashMap::default(),
            used_callee_saved: Vec::new(),
            va_named_stack_bytes: 0,
        }
    }

    pub fn set_pic(&mut self, pic: bool) {
        self.state.pic_mode = pic;
    }

    pub fn set_no_jump_tables(&mut self, enabled: bool) {
        self.state.no_jump_tables = enabled;
    }

    pub fn generate(mut self, module: &IrModule) -> String {
        generate_module(&mut self, module, None)
    }

    // --- i686 helper methods ---

    #[allow(dead_code)]
    fn operand_reg(&self, op: &Operand) -> Option<PhysReg> {
        match op {
            Operand::Value(v) => self.reg_assignments.get(&v.0).copied(),
            _ => None,
        }
    }

    fn dest_reg(&self, dest: &Value) -> Option<PhysReg> {
        self.reg_assignments.get(&dest.0).copied()
    }

    /// Load an operand into %eax.
    fn operand_to_eax(&mut self, op: &Operand) {
        // Check register cache - skip load if value is already in eax
        if let Operand::Value(v) = op {
            let is_alloca = self.state.is_alloca(v.0);
            if self.state.reg_cache.acc_has(v.0, is_alloca) {
                return;
            }
        }

        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::I8(v) => emit!(self.state, "    movl ${}, %eax", *v as i32),
                    IrConst::I16(v) => emit!(self.state, "    movl ${}, %eax", *v as i32),
                    IrConst::I32(v) => {
                        if *v == 0 {
                            self.state.emit("    xorl %eax, %eax");
                        } else {
                            emit!(self.state, "    movl ${}, %eax", v);
                        }
                    }
                    IrConst::I64(v) => {
                        // On i686, we can only hold 32 bits in eax
                        // Truncate to low 32 bits
                        let low = *v as i32;
                        if low == 0 {
                            self.state.emit("    xorl %eax, %eax");
                        } else {
                            emit!(self.state, "    movl ${}, %eax", low);
                        }
                    }
                    IrConst::I128(v) => {
                        let low = *v as i32;
                        emit!(self.state, "    movl ${}, %eax", low);
                    }
                    IrConst::F32(fval) => emit!(self.state, "    movl ${}, %eax", fval.to_bits() as i32),
                    IrConst::F64(fval) => {
                        // Store low 32 bits of the f64 bit pattern
                        let low = fval.to_bits() as i32;
                        emit!(self.state, "    movl ${}, %eax", low);
                    }
                    IrConst::LongDouble(_, bytes) => {
                        // Load first 4 bytes of long double
                        let low = i32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
                        emit!(self.state, "    movl ${}, %eax", low);
                    }
                    IrConst::Zero => {
                        self.state.emit("    xorl %eax, %eax");
                    }
                }
                self.state.reg_cache.invalidate_acc();
            }
            Operand::Value(v) => {
                let is_alloca = self.state.is_alloca(v.0);
                // Check if value is in a callee-saved register (allocas are never register-allocated)
                if let Some(phys) = self.reg_assignments.get(&v.0).copied() {
                    let reg = phys_reg_name(phys);
                    emit!(self.state, "    movl %{}, %eax", reg);
                    self.state.reg_cache.set_acc(v.0, false);
                } else if let Some(slot) = self.state.get_slot(v.0) {
                    if is_alloca {
                        // Alloca: the slot IS the data; load the address of the slot
                        if let Some(align) = self.state.alloca_over_align(v.0) {
                            // Over-aligned alloca: compute aligned address
                            emit!(self.state, "    leal {}(%ebp), %eax", slot.0);
                            emit!(self.state, "    addl ${}, %eax", align - 1);
                            emit!(self.state, "    andl ${}, %eax", -(align as i32));
                        } else {
                            emit!(self.state, "    leal {}(%ebp), %eax", slot.0);
                        }
                    } else {
                        // Regular value: load the value from the slot
                        emit!(self.state, "    movl {}(%ebp), %eax", slot.0);
                    }
                    self.state.reg_cache.set_acc(v.0, is_alloca);
                }
            }
        }
    }

    /// Load an operand into %ecx.
    fn operand_to_ecx(&mut self, op: &Operand) {
        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::I8(v) => emit!(self.state, "    movl ${}, %ecx", *v as i32),
                    IrConst::I16(v) => emit!(self.state, "    movl ${}, %ecx", *v as i32),
                    IrConst::I32(v) => {
                        if *v == 0 {
                            self.state.emit("    xorl %ecx, %ecx");
                        } else {
                            emit!(self.state, "    movl ${}, %ecx", v);
                        }
                    }
                    IrConst::I64(v) => {
                        let low = *v as i32;
                        if low == 0 {
                            self.state.emit("    xorl %ecx, %ecx");
                        } else {
                            emit!(self.state, "    movl ${}, %ecx", low);
                        }
                    }
                    IrConst::I128(v) => {
                        let low = *v as i32;
                        emit!(self.state, "    movl ${}, %ecx", low);
                    }
                    IrConst::F32(fval) => emit!(self.state, "    movl ${}, %ecx", fval.to_bits() as i32),
                    IrConst::F64(fval) => {
                        let low = fval.to_bits() as i32;
                        emit!(self.state, "    movl ${}, %ecx", low);
                    }
                    IrConst::LongDouble(_, bytes) => {
                        let low = i32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
                        emit!(self.state, "    movl ${}, %ecx", low);
                    }
                    IrConst::Zero => {
                        self.state.emit("    xorl %ecx, %ecx");
                    }
                }
            }
            Operand::Value(v) => {
                let is_alloca = self.state.is_alloca(v.0);
                if let Some(phys) = self.reg_assignments.get(&v.0).copied() {
                    let reg = phys_reg_name(phys);
                    emit!(self.state, "    movl %{}, %ecx", reg);
                } else if let Some(slot) = self.state.get_slot(v.0) {
                    if is_alloca {
                        // Alloca: load the address of the slot
                        if let Some(align) = self.state.alloca_over_align(v.0) {
                            emit!(self.state, "    leal {}(%ebp), %ecx", slot.0);
                            emit!(self.state, "    addl ${}, %ecx", align - 1);
                            emit!(self.state, "    andl ${}, %ecx", -(align as i32));
                        } else {
                            emit!(self.state, "    leal {}(%ebp), %ecx", slot.0);
                        }
                    } else {
                        emit!(self.state, "    movl {}(%ebp), %ecx", slot.0);
                    }
                }
            }
        }
    }

    /// Store %eax to a value's destination (callee-saved register or stack slot).
    fn store_eax_to(&mut self, dest: &Value) {
        if let Some(phys) = self.dest_reg(dest) {
            let reg = phys_reg_name(phys);
            emit!(self.state, "    movl %eax, %{}", reg);
            self.state.reg_cache.invalidate_acc();
        } else if let Some(slot) = self.state.get_slot(dest.0) {
            emit!(self.state, "    movl %eax, {}(%ebp)", slot.0);
            self.state.reg_cache.set_acc(dest.0, false);
        }
    }

    /// Return the store mnemonic for a given type.
    fn mov_store_for_type(&self, ty: IrType) -> &'static str {
        match ty {
            IrType::I8 | IrType::U8 => "movb",
            IrType::I16 | IrType::U16 => "movw",
            // On i686, pointer-sized types use movl (32-bit)
            _ => "movl",
        }
    }

    /// Return the load mnemonic for a given type.
    fn mov_load_for_type(&self, ty: IrType) -> &'static str {
        match ty {
            IrType::I8 => "movsbl",    // sign-extend byte to 32-bit
            IrType::U8 => "movzbl",    // zero-extend byte to 32-bit
            IrType::I16 => "movswl",   // sign-extend word to 32-bit
            IrType::U16 => "movzwl",   // zero-extend word to 32-bit
            // Everything 32-bit or larger uses movl
            _ => "movl",
        }
    }

    /// Return the type suffix for an operation.
    fn type_suffix(&self, ty: IrType) -> &'static str {
        match ty {
            IrType::I8 | IrType::U8 => "b",
            IrType::I16 | IrType::U16 => "w",
            // On i686, the default (pointer-sized) is "l" (32-bit)
            _ => "l",
        }
    }

    /// Return the register name for eax sub-register based on type size.
    fn eax_for_type(&self, ty: IrType) -> &'static str {
        match ty {
            IrType::I8 | IrType::U8 => "%al",
            IrType::I16 | IrType::U16 => "%ax",
            _ => "%eax",
        }
    }

    /// Check if an operand is a constant that fits in an i32 immediate.
    fn const_as_imm32(op: &Operand) -> Option<i64> {
        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::I8(v) => Some(*v as i64),
                    IrConst::I16(v) => Some(*v as i64),
                    IrConst::I32(v) => Some(*v as i64),
                    IrConst::I64(v) => {
                        // On i686, check if the value fits in 32 bits
                        if *v >= i32::MIN as i64 && *v <= i32::MAX as i64 {
                            Some(*v)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Load an F128 (long double) operand onto the x87 FPU stack.
    fn emit_f128_load_to_x87(&mut self, op: &Operand) {
        match op {
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    emit!(self.state, "    fldt {}(%ebp)", slot.0);
                }
            }
            Operand::Const(IrConst::LongDouble(_, bytes)) => {
                // Push raw bytes to stack, then fld from there
                // Push in reverse order (high bytes first) for little-endian stack
                let dword0 = i32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
                let dword1 = i32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]);
                let word2 = i16::from_le_bytes([bytes[8], bytes[9]]) as i32;
                self.state.emit("    subl $12, %esp");
                emit!(self.state, "    movl ${}, (%esp)", dword0);
                emit!(self.state, "    movl ${}, 4(%esp)", dword1);
                emit!(self.state, "    movw ${}, 8(%esp)", word2);
                self.state.emit("    fldt (%esp)");
                self.state.emit("    addl $12, %esp");
            }
            Operand::Const(IrConst::F64(fval)) => {
                // Convert f64 to x87: push to stack as f64, fld, convert
                let bits = fval.to_bits();
                let low = (bits & 0xFFFFFFFF) as i32;
                let high = ((bits >> 32) & 0xFFFFFFFF) as i32;
                self.state.emit("    subl $8, %esp");
                emit!(self.state, "    movl ${}, (%esp)", low);
                emit!(self.state, "    movl ${}, 4(%esp)", high);
                self.state.emit("    fldl (%esp)");
                self.state.emit("    addl $8, %esp");
            }
            Operand::Const(IrConst::F32(fval)) => {
                emit!(self.state, "    movl ${}, %eax", fval.to_bits() as i32);
                self.state.emit("    pushl %eax");
                self.state.emit("    flds (%esp)");
                self.state.emit("    addl $4, %esp");
            }
            _ => {
                self.operand_to_eax(op);
                // Fallback: treat as integer, push to stack
                self.state.emit("    pushl %eax");
                self.state.emit("    flds (%esp)");
                self.state.emit("    addl $4, %esp");
            }
        }
    }

    /// Load an F64 (double) operand onto the x87 FPU stack.
    /// F64 values occupy 8-byte stack slots on i686, so we use fldl to load
    /// them directly from memory rather than going through the 32-bit accumulator.
    fn emit_f64_load_to_x87(&mut self, op: &Operand) {
        match op {
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    emit!(self.state, "    fldl {}(%ebp)", slot.0);
                }
            }
            Operand::Const(IrConst::F64(fval)) => {
                let bits = fval.to_bits();
                let low = (bits & 0xFFFFFFFF) as i32;
                let high = ((bits >> 32) & 0xFFFFFFFF) as i32;
                self.state.emit("    subl $8, %esp");
                emit!(self.state, "    movl ${}, (%esp)", low);
                emit!(self.state, "    movl ${}, 4(%esp)", high);
                self.state.emit("    fldl (%esp)");
                self.state.emit("    addl $8, %esp");
            }
            Operand::Const(IrConst::F32(fval)) => {
                emit!(self.state, "    movl ${}, %eax", fval.to_bits() as i32);
                self.state.emit("    pushl %eax");
                self.state.emit("    flds (%esp)");
                self.state.emit("    addl $4, %esp");
            }
            Operand::Const(IrConst::Zero) => {
                self.state.emit("    fldz");
            }
            _ => {
                // Fallback: load integer bits and convert
                self.operand_to_eax(op);
                self.state.emit("    pushl %eax");
                self.state.emit("    fildl (%esp)");
                self.state.emit("    addl $4, %esp");
            }
        }
    }

    /// Store the x87 st(0) value as F64 into a destination stack slot.
    /// Pops st(0).
    fn emit_f64_store_from_x87(&mut self, dest: &Value) {
        if let Some(slot) = self.state.get_slot(dest.0) {
            emit!(self.state, "    fstpl {}(%ebp)", slot.0);
        } else {
            // No slot available, pop x87 stack to discard
            self.state.emit("    fstp %st(0)");
        }
    }

    /// Load an operand into a named register.
    #[allow(dead_code)]
    fn operand_to_reg(&mut self, op: &Operand, reg: &str) {
        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::I8(v) => emit!(self.state, "    movl ${}, %{}", *v as i32, reg),
                    IrConst::I16(v) => emit!(self.state, "    movl ${}, %{}", *v as i32, reg),
                    IrConst::I32(v) => {
                        if *v == 0 && (reg == "eax" || reg == "ecx" || reg == "edx") {
                            emit!(self.state, "    xorl %{}, %{}", reg, reg);
                        } else {
                            emit!(self.state, "    movl ${}, %{}", v, reg);
                        }
                    }
                    IrConst::I64(v) => {
                        let low = *v as i32;
                        if low == 0 && (reg == "eax" || reg == "ecx" || reg == "edx") {
                            emit!(self.state, "    xorl %{}, %{}", reg, reg);
                        } else {
                            emit!(self.state, "    movl ${}, %{}", low, reg);
                        }
                    }
                    _ => {
                        self.operand_to_eax(op);
                        if reg != "eax" {
                            emit!(self.state, "    movl %eax, %{}", reg);
                        }
                    }
                }
            }
            Operand::Value(v) => {
                if let Some(phys) = self.reg_assignments.get(&v.0).copied() {
                    let src_reg = phys_reg_name(phys);
                    if src_reg != reg {
                        emit!(self.state, "    movl %{}, %{}", src_reg, reg);
                    }
                } else if let Some(slot) = self.state.get_slot(v.0) {
                    emit!(self.state, "    movl {}(%ebp), %{}", slot.0, reg);
                }
            }
        }
    }
}

// Helper functions for ALU mnemonics
fn alu_mnemonic(op: IrBinOp) -> &'static str {
    match op {
        IrBinOp::Add => "add",
        IrBinOp::Sub => "sub",
        IrBinOp::And => "and",
        IrBinOp::Or => "or",
        IrBinOp::Xor => "xor",
        _ => panic!("not a simple ALU op: {:?}", op),
    }
}

fn shift_mnemonic(op: IrBinOp) -> &'static str {
    match op {
        IrBinOp::Shl => "shll",
        IrBinOp::AShr => "sarl",
        IrBinOp::LShr => "shrl",
        _ => panic!("not a shift op: {:?}", op),
    }
}

// ─── ArchCodegen trait implementation ────────────────────────────────────────

impl ArchCodegen for I686Codegen {
    fn state(&mut self) -> &mut CodegenState { &mut self.state }
    fn state_ref(&self) -> &CodegenState { &self.state }

    fn ptr_directive(&self) -> PtrDirective { PtrDirective::Long }

    fn get_phys_reg_for_value(&self, val_id: u32) -> Option<PhysReg> {
        self.reg_assignments.get(&val_id).copied()
    }

    fn emit_reg_to_reg_move(&mut self, src: PhysReg, dest: PhysReg) {
        let src_name = phys_reg_name(src);
        let dest_name = phys_reg_name(dest);
        emit!(self.state, "    movl %{}, %{}", src_name, dest_name);
    }

    fn emit_acc_to_phys_reg(&mut self, dest: PhysReg) {
        let dest_name = phys_reg_name(dest);
        emit!(self.state, "    movl %eax, %{}", dest_name);
    }

    fn phys_reg_name(&self, reg: PhysReg) -> &'static str {
        phys_reg_name(reg)
    }

    fn calculate_stack_space(&mut self, func: &IrFunction) -> i64 {
        self.is_variadic = func.is_variadic;
        self.current_return_type = func.return_type;

        // Compute named parameter stack bytes for va_start (variadic functions).
        // On i686 cdecl, all parameters are stack-passed, so total_stack_bytes
        // gives us exactly how many bytes the named params occupy.
        if func.is_variadic {
            let config = self.call_abi_config();
            let classification = crate::backend::call_emit::classify_params_full(func, &config);
            self.va_named_stack_bytes = classification.total_stack_bytes;
        }

        // Run register allocator before stack space computation.
        // Filter out asm-clobbered callee-saved registers.
        let mut asm_clobbered_regs: Vec<PhysReg> = Vec::new();
        collect_inline_asm_callee_saved(
            func, &mut asm_clobbered_regs,
            |constraint| i686_constraint_to_phys(constraint),
            |clobber| i686_clobber_to_phys(clobber),
        );
        let available_regs = filter_available_regs(I686_CALLEE_SAVED, &asm_clobbered_regs);

        // No caller-saved registers on i686 (eax/ecx/edx are all scratch)
        let caller_saved_regs = I686_CALLER_SAVED.to_vec();

        let (reg_assigned, cached_liveness) = run_regalloc_and_merge_clobbers(
            func, available_regs, caller_saved_regs, &asm_clobbered_regs,
            &mut self.reg_assignments, &mut self.used_callee_saved,
        );

        // Calculate stack space using the shared framework.
        // i686 uses negative offsets from ebp, 4-byte minimum alignment.
        // Reserve space for callee-saved register pushes at the top of the frame
        // (between ebp and locals). Each push takes 4 bytes.
        let callee_saved_bytes = self.used_callee_saved.len() as i64 * 4;
        let space = calculate_stack_space_common(&mut self.state, func, callee_saved_bytes, |space, alloc_size, align| {
            let effective_align = if align > 0 { align.max(4) } else { 4 };
            let alloc = (alloc_size + 3) & !3; // round up to 4-byte boundary
            let new_space = ((space + alloc + effective_align - 1) / effective_align) * effective_align;
            (-new_space, new_space)
        }, &reg_assigned, cached_liveness);

        space
    }

    fn aligned_frame_size(&self, raw_space: i64) -> i64 {
        // raw_space includes callee_saved_bytes as initial_offset from
        // calculate_stack_space, so subtract it to get raw locals space.
        let callee_saved_bytes = self.used_callee_saved.len() as i64 * 4;
        let raw_locals = raw_space - callee_saved_bytes;
        // Must be 16-byte aligned at call sites.
        // At function entry: esp points to return address (4 bytes).
        // After push %ebp: 8 bytes used.
        // After pushing callee-saved regs: 8 + callee_saved_bytes used.
        // After sub $frame_size: 8 + callee_saved_bytes + frame_size used.
        // We need (8 + callee_saved_bytes + frame_size) % 16 == 0.
        let fixed_overhead = callee_saved_bytes + 8;
        let needed = raw_locals + fixed_overhead;
        let aligned = (needed + 15) & !15;
        aligned - fixed_overhead
    }

    fn emit_prologue(&mut self, _func: &IrFunction, frame_size: i64) {
        // Function entry
        self.state.emit("    pushl %ebp");
        self.state.emit("    movl %esp, %ebp");

        // Save callee-saved registers using push (before allocating locals).
        // This ensures saves are always above esp and within the valid stack.
        for &reg in self.used_callee_saved.iter() {
            let name = phys_reg_name(reg);
            emit!(self.state, "    pushl %{}", name);
        }

        // Allocate stack space for locals
        if frame_size > 0 {
            emit!(self.state, "    subl ${}, %esp", frame_size);
        }
    }

    fn emit_epilogue(&mut self, _frame_size: i64) {
        // Restore esp to point at the callee-saved register area.
        // Use lea from ebp (robust against dynamic alloca changes to esp).
        let callee_saved_bytes = self.used_callee_saved.len() as i64 * 4;
        if callee_saved_bytes > 0 {
            emit!(self.state, "    leal -{}(%ebp), %esp", callee_saved_bytes);
        } else {
            self.state.emit("    movl %ebp, %esp");
        }

        // Restore callee-saved registers (reverse order of saves)
        for &reg in self.used_callee_saved.iter().rev() {
            let name = phys_reg_name(reg);
            emit!(self.state, "    popl %{}", name);
        }

        self.state.emit("    popl %ebp");
    }

    fn emit_store_params(&mut self, func: &IrFunction) {
        // Use shared parameter classification (same ABI config as emit_call).
        let config = self.call_abi_config();
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

        // Stack-passed parameters start at 8(%ebp) (after saved ebp + return addr).
        let stack_base: i64 = 8;

        for (i, _param) in func.params.iter().enumerate() {
            let class = param_classes[i];

            // Find the alloca for this parameter.
            let (slot, ty) = if let Some((dest, ty)) = find_param_alloca(func, i) {
                if let Some(slot) = self.state.get_slot(dest.0) {
                    (slot, ty)
                } else {
                    continue;
                }
            } else {
                continue;
            };

            match class {
                ParamClass::StackScalar { offset } => {
                    let src_offset = stack_base + offset;
                    if ty == IrType::F64 || ty == IrType::I64 || ty == IrType::U64 {
                        // 8-byte types: copy both halves
                        emit!(self.state, "    movl {}(%ebp), %eax", src_offset);
                        emit!(self.state, "    movl %eax, {}(%ebp)", slot.0);
                        emit!(self.state, "    movl {}(%ebp), %eax", src_offset + 4);
                        emit!(self.state, "    movl %eax, {}(%ebp)", slot.0 + 4);
                    } else {
                        let load_instr = self.mov_load_for_type(ty);
                        let store_instr = self.mov_store_for_type(ty);
                        emit!(self.state, "    {} {}(%ebp), %eax", load_instr, src_offset);
                        let dest_reg = self.eax_for_type(ty);
                        emit!(self.state, "    {} {}, {}(%ebp)", store_instr, dest_reg, slot.0);
                    }
                }
                ParamClass::StructStack { offset, size } => {
                    let src = stack_base + offset;
                    let mut copied = 0usize;
                    while copied + 4 <= size {
                        emit!(self.state, "    movl {}(%ebp), %eax", src + copied as i64);
                        emit!(self.state, "    movl %eax, {}(%ebp)", slot.0 + copied as i64);
                        copied += 4;
                    }
                    while copied < size {
                        emit!(self.state, "    movb {}(%ebp), %al", src + copied as i64);
                        emit!(self.state, "    movb %al, {}(%ebp)", slot.0 + copied as i64);
                        copied += 1;
                    }
                }
                ParamClass::LargeStructStack { offset, size } => {
                    let src = stack_base + offset;
                    let mut copied = 0usize;
                    while copied + 4 <= size {
                        emit!(self.state, "    movl {}(%ebp), %eax", src + copied as i64);
                        emit!(self.state, "    movl %eax, {}(%ebp)", slot.0 + copied as i64);
                        copied += 4;
                    }
                    while copied < size {
                        emit!(self.state, "    movb {}(%ebp), %al", src + copied as i64);
                        emit!(self.state, "    movb %al, {}(%ebp)", slot.0 + copied as i64);
                        copied += 1;
                    }
                }
                ParamClass::F128AlwaysStack { offset } => {
                    let src = stack_base + offset;
                    emit!(self.state, "    fldt {}(%ebp)", src);
                    emit!(self.state, "    fstpt {}(%ebp)", slot.0);
                }
                ParamClass::I128Stack { offset } => {
                    let src = stack_base + offset;
                    for j in (0..16).step_by(4) {
                        emit!(self.state, "    movl {}(%ebp), %eax", src + j as i64);
                        emit!(self.state, "    movl %eax, {}(%ebp)", slot.0 + j as i64);
                    }
                }
                ParamClass::F128Stack { offset } => {
                    let src = stack_base + offset;
                    emit!(self.state, "    fldt {}(%ebp)", src);
                    emit!(self.state, "    fstpt {}(%ebp)", slot.0);
                }
                _ => {
                    // IntReg/FloatReg classes don't apply to i686 cdecl
                }
            }
        }
    }

    fn emit_param_ref(&mut self, dest: &Value, param_idx: usize, ty: IrType) {
        // On i686 cdecl, all parameters are on the stack.
        // Use the classified parameter offsets from emit_store_params for accuracy.
        use crate::backend::call_emit::ParamClass;

        let stack_base: i64 = 8; // after saved ebp + return address
        let param_offset = if param_idx < self.state.param_classes.len() {
            match self.state.param_classes[param_idx] {
                ParamClass::StackScalar { offset } |
                ParamClass::StructStack { offset, .. } |
                ParamClass::LargeStructStack { offset, .. } |
                ParamClass::F128AlwaysStack { offset } |
                ParamClass::I128Stack { offset } |
                ParamClass::F128Stack { offset } |
                ParamClass::LargeStructByRefStack { offset, .. } => stack_base + offset,
                _ => stack_base + (param_idx as i64) * 4, // fallback
            }
        } else {
            stack_base + (param_idx as i64) * 4 // fallback
        };

        if is_i128_type(ty) {
            // Load 16 bytes
            if let Some(slot) = self.state.get_slot(dest.0) {
                for i in (0..16).step_by(4) {
                    emit!(self.state, "    movl {}(%ebp), %eax", param_offset + i as i64);
                    emit!(self.state, "    movl %eax, {}(%ebp)", slot.0 + i as i64);
                }
            }
        } else if ty == IrType::F64 || ty == IrType::I64 || ty == IrType::U64 {
            // 8-byte types: load both halves
            if let Some(slot) = self.state.get_slot(dest.0) {
                emit!(self.state, "    movl {}(%ebp), %eax", param_offset);
                emit!(self.state, "    movl %eax, {}(%ebp)", slot.0);
                emit!(self.state, "    movl {}(%ebp), %eax", param_offset + 4);
                emit!(self.state, "    movl %eax, {}(%ebp)", slot.0 + 4);
            }
        } else {
            let load_instr = self.mov_load_for_type(ty);
            emit!(self.state, "    {} {}(%ebp), %eax", load_instr, param_offset);
            self.store_eax_to(dest);
        }
    }

    fn emit_load_operand(&mut self, op: &Operand) {
        self.operand_to_eax(op);
    }

    fn emit_store_result(&mut self, dest: &Value) {
        self.store_eax_to(dest);
    }

    fn emit_alloca_aligned_addr(&mut self, slot: StackSlot, _val_id: u32) {
        // Compute aligned alloca address into ecx
        emit!(self.state, "    leal {}(%ebp), %ecx", slot.0);
        let align = (-slot.0) as usize; // slot offset encodes alignment info
        if align > 1 {
            // Runtime alignment: round up to alignment boundary
            emit!(self.state, "    addl ${}, %ecx", align - 1);
            emit!(self.state, "    andl ${}, %ecx", -(align as i32));
        }
    }

    fn emit_alloca_aligned_addr_to_acc(&mut self, slot: StackSlot, _val_id: u32) {
        emit!(self.state, "    leal {}(%ebp), %eax", slot.0);
        let align = (-slot.0) as usize;
        if align > 1 {
            emit!(self.state, "    addl ${}, %eax", align - 1);
            emit!(self.state, "    andl ${}, %eax", -(align as i32));
        }
        self.state.reg_cache.invalidate_acc();
    }

    fn emit_add_offset_to_addr_reg(&mut self, offset: i64) {
        if offset != 0 {
            emit!(self.state, "    addl ${}, %ecx", offset as i32);
        }
    }

    fn emit_int_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        // 64-bit operations on i686 need special handling (register pairs)
        let is_64bit = matches!(ty, IrType::I64 | IrType::U64 | IrType::Ptr) && !crate::common::types::target_is_32bit()
            || matches!(ty, IrType::I64 | IrType::U64);

        if is_64bit {
            // TODO: Implement 64-bit arithmetic via eax:edx register pairs
            // For now, just use 32-bit operations (truncating)
            self.operand_to_eax(lhs);
            self.operand_to_ecx(rhs);

            match op {
                IrBinOp::Add => self.state.emit("    addl %ecx, %eax"),
                IrBinOp::Sub => self.state.emit("    subl %ecx, %eax"),
                IrBinOp::Mul => self.state.emit("    imull %ecx, %eax"),
                IrBinOp::And => self.state.emit("    andl %ecx, %eax"),
                IrBinOp::Or => self.state.emit("    orl %ecx, %eax"),
                IrBinOp::Xor => self.state.emit("    xorl %ecx, %eax"),
                IrBinOp::Shl => self.state.emit("    shll %cl, %eax"),
                IrBinOp::AShr => self.state.emit("    sarl %cl, %eax"),
                IrBinOp::LShr => self.state.emit("    shrl %cl, %eax"),
                IrBinOp::SDiv => {
                    self.state.emit("    cltd");
                    self.state.emit("    idivl %ecx");
                }
                IrBinOp::UDiv => {
                    self.state.emit("    xorl %edx, %edx");
                    self.state.emit("    divl %ecx");
                }
                IrBinOp::SRem => {
                    self.state.emit("    cltd");
                    self.state.emit("    idivl %ecx");
                    self.state.emit("    movl %edx, %eax");
                }
                IrBinOp::URem => {
                    self.state.emit("    xorl %edx, %edx");
                    self.state.emit("    divl %ecx");
                    self.state.emit("    movl %edx, %eax");
                }
            }
            self.state.reg_cache.invalidate_acc();
            self.store_eax_to(dest);
            return;
        }

        // Immediate optimization for ALU ops
        if matches!(op, IrBinOp::Add | IrBinOp::Sub | IrBinOp::And | IrBinOp::Or | IrBinOp::Xor) {
            if let Some(imm) = Self::const_as_imm32(rhs) {
                self.operand_to_eax(lhs);
                let mnem = alu_mnemonic(op);
                emit!(self.state, "    {}l ${}, %eax", mnem, imm);
                self.state.reg_cache.invalidate_acc();
                self.store_eax_to(dest);
                return;
            }
        }

        // Immediate multiply
        if op == IrBinOp::Mul {
            if let Some(imm) = Self::const_as_imm32(rhs) {
                self.operand_to_eax(lhs);
                emit!(self.state, "    imull ${}, %eax, %eax", imm);
                self.state.reg_cache.invalidate_acc();
                self.store_eax_to(dest);
                return;
            }
        }

        // Immediate shift
        if matches!(op, IrBinOp::Shl | IrBinOp::AShr | IrBinOp::LShr) {
            if let Some(imm) = Self::const_as_imm32(rhs) {
                self.operand_to_eax(lhs);
                let mnem = shift_mnemonic(op);
                let shift_amount = (imm as u32) & 31;
                emit!(self.state, "    {} ${}, %eax", mnem, shift_amount);
                self.state.reg_cache.invalidate_acc();
                self.store_eax_to(dest);
                return;
            }
        }

        // General case: load lhs to eax, rhs to ecx
        self.operand_to_eax(lhs);
        self.operand_to_ecx(rhs);

        match op {
            IrBinOp::Add => self.state.emit("    addl %ecx, %eax"),
            IrBinOp::Sub => self.state.emit("    subl %ecx, %eax"),
            IrBinOp::Mul => self.state.emit("    imull %ecx, %eax"),
            IrBinOp::And => self.state.emit("    andl %ecx, %eax"),
            IrBinOp::Or => self.state.emit("    orl %ecx, %eax"),
            IrBinOp::Xor => self.state.emit("    xorl %ecx, %eax"),
            IrBinOp::Shl => self.state.emit("    shll %cl, %eax"),
            IrBinOp::AShr => self.state.emit("    sarl %cl, %eax"),
            IrBinOp::LShr => self.state.emit("    shrl %cl, %eax"),
            IrBinOp::SDiv => {
                self.state.emit("    cltd");
                self.state.emit("    idivl %ecx");
            }
            IrBinOp::UDiv => {
                self.state.emit("    xorl %edx, %edx");
                self.state.emit("    divl %ecx");
            }
            IrBinOp::SRem => {
                self.state.emit("    cltd");
                self.state.emit("    idivl %ecx");
                self.state.emit("    movl %edx, %eax");
            }
            IrBinOp::URem => {
                self.state.emit("    xorl %edx, %edx");
                self.state.emit("    divl %ecx");
                self.state.emit("    movl %edx, %eax");
            }
        }
        self.state.reg_cache.invalidate_acc();
        self.store_eax_to(dest);
    }

    /// Override float binop to use x87 for F64 (the default path uses emit_load_operand
    /// which only loads 32 bits into eax, losing the high half of F64 values).
    fn emit_float_binop(&mut self, dest: &Value, op: crate::backend::cast::FloatOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if ty == IrType::F64 {
            // Use x87 FPU for F64 binary operations.
            // Load lhs onto x87 stack first, then rhs, then operate.
            let mnemonic = self.emit_float_binop_mnemonic(op);
            self.emit_f64_load_to_x87(lhs);
            self.emit_f64_load_to_x87(rhs);
            // x87 stack: st(0)=rhs, st(1)=lhs
            // faddp/fmulp: st(1) = st(1) op st(0), pop st(0)
            // fsubp/fdivp: st(1) = st(1) op st(0), pop st(0) (lhs - rhs / lhs / rhs)
            emit!(self.state, "    f{}p %st, %st(1)", mnemonic);
            // Result is in st(0), store to dest's 8-byte stack slot
            self.emit_f64_store_from_x87(dest);
            self.state.reg_cache.invalidate_acc();
            return;
        }
        if ty == IrType::F128 {
            // Use x87 FPU for F128 (long double) binary operations
            let mnemonic = self.emit_float_binop_mnemonic(op);
            self.emit_f128_load_to_x87(lhs);
            self.emit_f128_load_to_x87(rhs);
            emit!(self.state, "    f{}p %st, %st(1)", mnemonic);
            // Store F128 result to dest
            if let Some(slot) = self.state.get_slot(dest.0) {
                emit!(self.state, "    fstpt {}(%ebp)", slot.0);
            }
            self.state.reg_cache.invalidate_acc();
            return;
        }
        // F32: use default path (SSE via eax)
        let mnemonic = self.emit_float_binop_mnemonic(op);
        self.emit_load_operand(lhs);
        self.emit_acc_to_secondary();
        self.emit_load_operand(rhs);
        self.emit_float_binop_impl(mnemonic, ty);
        self.emit_store_result(dest);
    }

    fn emit_float_binop_mnemonic(&self, op: crate::backend::cast::FloatOp) -> &'static str {
        match op {
            crate::backend::cast::FloatOp::Add => "add",
            crate::backend::cast::FloatOp::Sub => "sub",
            crate::backend::cast::FloatOp::Mul => "mul",
            crate::backend::cast::FloatOp::Div => "div",
        }
    }

    fn emit_float_binop_impl(&mut self, mnemonic: &str, ty: IrType) {
        // Use SSE for F32, x87 for F64 (handled via emit_float_binop override)
        if ty == IrType::F32 {
            self.state.emit("    movd %ecx, %xmm1");
            self.state.emit("    movd %eax, %xmm0");
            emit!(self.state, "    {}ss %xmm1, %xmm0", mnemonic);
            self.state.emit("    movd %xmm0, %eax");
        } else if ty == IrType::F64 {
            // F64 binops are handled by emit_float_binop override which uses x87.
            // This path shouldn't normally be reached for F64, but handle it defensively.
            // The x87 stack should have: st(0)=rhs, st(1)=lhs after the override loads.
            emit!(self.state, "    f{}p %st, %st(1)", mnemonic);
        } else if ty == IrType::F128 {
            // F128 (x87 long double) - use x87 directly
            // Both operands should already be on x87 stack from the load path
            emit!(self.state, "    f{}p %st, %st(1)", mnemonic);
        }
        self.state.reg_cache.invalidate_acc();
    }

    fn emit_acc_to_secondary(&mut self) {
        self.state.emit("    movl %eax, %ecx");
    }

    /// Override emit_cast to handle F64 source/destination specially on i686.
    /// F64 values are 8 bytes but the accumulator is only 32 bits, so we use
    /// x87 FPU for all F64 conversions, bypassing the default emit_load_operand path.
    fn emit_cast(&mut self, dest: &Value, src: &Operand, from_ty: IrType, to_ty: IrType) {
        use crate::backend::cast::{CastKind, classify_cast};

        // Let the default handle i128 conversions
        if crate::backend::generation::is_i128_type(from_ty) || crate::backend::generation::is_i128_type(to_ty) {
            crate::backend::traits::emit_cast_default(self, dest, src, from_ty, to_ty);
            return;
        }

        match classify_cast(from_ty, to_ty) {
            // --- Casts where F64 is the destination (result needs 8-byte slot) ---
            CastKind::SignedToFloat { to_f64: true, from_ty: src_ty } => {
                // int → F64: load int into eax, convert via x87, store 8-byte result
                self.operand_to_eax(src);
                match src_ty {
                    IrType::I8 => self.state.emit("    movsbl %al, %eax"),
                    IrType::I16 => self.state.emit("    movswl %ax, %eax"),
                    _ => {}
                }
                self.state.emit("    pushl %eax");
                self.state.emit("    fildl (%esp)");
                self.state.emit("    addl $4, %esp");
                // st(0) = F64 result, store to dest's 8-byte slot
                self.emit_f64_store_from_x87(dest);
                self.state.reg_cache.invalidate_acc();
            }
            CastKind::UnsignedToFloat { to_f64: true, from_u64 } => {
                // unsigned → F64
                self.operand_to_eax(src);
                if from_u64 {
                    // U64: load both halves from slot, use fildq
                    self.emit_load_acc_pair(src);
                    self.state.emit("    pushl %edx");
                    self.state.emit("    pushl %eax");
                    self.state.emit("    fildq (%esp)");
                    self.state.emit("    addl $8, %esp");
                } else {
                    // U32: handle high-bit-set values
                    let big_label = self.state.fresh_label("u2f_big");
                    let done_label = self.state.fresh_label("u2f_done");
                    self.state.emit("    testl %eax, %eax");
                    self.state.out.emit_jcc_label("    js", &big_label);
                    // Positive (< 2^31): fildl works directly
                    self.state.emit("    pushl %eax");
                    self.state.emit("    fildl (%esp)");
                    self.state.emit("    addl $4, %esp");
                    self.state.out.emit_jmp_label(&done_label);
                    self.state.out.emit_named_label(&big_label);
                    // Bit 31 set: push as u64 (zero-extend), use fildq
                    self.state.emit("    pushl $0");
                    self.state.emit("    pushl %eax");
                    self.state.emit("    fildq (%esp)");
                    self.state.emit("    addl $8, %esp");
                    self.state.out.emit_named_label(&done_label);
                }
                self.emit_f64_store_from_x87(dest);
                self.state.reg_cache.invalidate_acc();
            }
            CastKind::FloatToFloat { widen: true } => {
                // F32 → F64: load F32 from eax, x87 will auto-extend
                self.operand_to_eax(src);
                self.state.emit("    pushl %eax");
                self.state.emit("    flds (%esp)");
                self.state.emit("    addl $4, %esp");
                // st(0) is now the F64 value, store to 8-byte slot
                self.emit_f64_store_from_x87(dest);
                self.state.reg_cache.invalidate_acc();
            }

            // --- Casts where F64 is the source (need to load 8-byte value) ---
            CastKind::FloatToSigned { from_f64: true } => {
                // F64 → signed int: load full 8-byte F64, convert via x87 fisttpl
                self.emit_f64_load_to_x87(src);
                self.state.emit("    subl $4, %esp");
                self.state.emit("    fisttpl (%esp)");
                self.state.emit("    movl (%esp), %eax");
                self.state.emit("    addl $4, %esp");
                self.state.reg_cache.invalidate_acc();
                self.store_eax_to(dest);
            }
            CastKind::FloatToUnsigned { from_f64: true, to_u64 } => {
                if to_u64 {
                    // F64 → U64: load F64 via x87, convert to 64-bit int
                    self.emit_f64_load_to_x87(src);
                    self.state.emit("    subl $8, %esp");
                    self.state.emit("    fisttpq (%esp)");
                    self.state.emit("    movl (%esp), %eax");
                    self.state.emit("    movl 4(%esp), %edx");
                    self.state.emit("    addl $8, %esp");
                    self.emit_store_acc_pair(dest);
                } else {
                    // F64 → unsigned 32-bit: load F64, convert via x87
                    // Use fisttpq to get full range (values > 2^31)
                    self.emit_f64_load_to_x87(src);
                    self.state.emit("    subl $8, %esp");
                    self.state.emit("    fisttpq (%esp)");
                    self.state.emit("    movl (%esp), %eax");
                    self.state.emit("    addl $8, %esp");
                    self.state.reg_cache.invalidate_acc();
                    self.store_eax_to(dest);
                }
            }
            CastKind::FloatToFloat { widen: false } => {
                // F64 → F32: load full 8-byte F64, convert to F32 on x87
                self.emit_f64_load_to_x87(src);
                self.state.emit("    subl $4, %esp");
                self.state.emit("    fstps (%esp)");
                self.state.emit("    movl (%esp), %eax");
                self.state.emit("    addl $4, %esp");
                self.state.reg_cache.invalidate_acc();
                self.store_eax_to(dest);
            }

            // --- F128 ↔ F64/F32 conversions (native F128 variants from ARM/RISC-V) ---
            // On x86/i686, classify_cast treats F128 as F64, so these native variants
            // are only reached when classify_cast_with_f128(..., true) is used.
            // Handle them properly in case they appear.
            CastKind::FloatToF128 { from_f32 } => {
                if from_f32 {
                    // F32 → F128: load F32 onto x87, store as F128
                    self.operand_to_eax(src);
                    self.state.emit("    pushl %eax");
                    self.state.emit("    flds (%esp)");
                    self.state.emit("    addl $4, %esp");
                } else {
                    // F64 → F128: load F64 onto x87
                    self.emit_f64_load_to_x87(src);
                }
                if let Some(slot) = self.state.get_slot(dest.0) {
                    emit!(self.state, "    fstpt {}(%ebp)", slot.0);
                }
                self.state.reg_cache.invalidate_acc();
            }
            CastKind::F128ToFloat { to_f32 } => {
                self.emit_f128_load_to_x87(src);
                if to_f32 {
                    // F128 → F32
                    self.state.emit("    subl $4, %esp");
                    self.state.emit("    fstps (%esp)");
                    self.state.emit("    movl (%esp), %eax");
                    self.state.emit("    addl $4, %esp");
                    self.state.reg_cache.invalidate_acc();
                    self.store_eax_to(dest);
                } else {
                    // F128 → F64
                    self.emit_f64_store_from_x87(dest);
                    self.state.reg_cache.invalidate_acc();
                }
            }

            // --- F128 ↔ int conversions ---
            CastKind::SignedToF128 { from_ty: src_ty } => {
                self.operand_to_eax(src);
                match src_ty {
                    IrType::I8 => self.state.emit("    movsbl %al, %eax"),
                    IrType::I16 => self.state.emit("    movswl %ax, %eax"),
                    _ => {}
                }
                self.state.emit("    pushl %eax");
                self.state.emit("    fildl (%esp)");
                self.state.emit("    addl $4, %esp");
                if let Some(slot) = self.state.get_slot(dest.0) {
                    emit!(self.state, "    fstpt {}(%ebp)", slot.0);
                }
                self.state.reg_cache.invalidate_acc();
            }
            CastKind::UnsignedToF128 { .. } => {
                self.operand_to_eax(src);
                let big_label = self.state.fresh_label("u2f128_big");
                let done_label = self.state.fresh_label("u2f128_done");
                self.state.emit("    testl %eax, %eax");
                self.state.out.emit_jcc_label("    js", &big_label);
                self.state.emit("    pushl %eax");
                self.state.emit("    fildl (%esp)");
                self.state.emit("    addl $4, %esp");
                self.state.out.emit_jmp_label(&done_label);
                self.state.out.emit_named_label(&big_label);
                self.state.emit("    pushl $0");
                self.state.emit("    pushl %eax");
                self.state.emit("    fildq (%esp)");
                self.state.emit("    addl $8, %esp");
                self.state.out.emit_named_label(&done_label);
                if let Some(slot) = self.state.get_slot(dest.0) {
                    emit!(self.state, "    fstpt {}(%ebp)", slot.0);
                }
                self.state.reg_cache.invalidate_acc();
            }
            CastKind::F128ToSigned { .. } => {
                self.emit_f128_load_to_x87(src);
                self.state.emit("    subl $4, %esp");
                self.state.emit("    fisttpl (%esp)");
                self.state.emit("    movl (%esp), %eax");
                self.state.emit("    addl $4, %esp");
                self.state.reg_cache.invalidate_acc();
                self.store_eax_to(dest);
            }
            CastKind::F128ToUnsigned { .. } => {
                self.emit_f128_load_to_x87(src);
                self.state.emit("    subl $8, %esp");
                self.state.emit("    fisttpq (%esp)");
                self.state.emit("    movl (%esp), %eax");
                self.state.emit("    addl $8, %esp");
                self.state.reg_cache.invalidate_acc();
                self.store_eax_to(dest);
            }

            // --- All other casts use the default path (emit_load_operand → eax → cast → store) ---
            _ => {
                self.operand_to_eax(src);
                self.emit_cast_instrs(from_ty, to_ty);
                self.store_eax_to(dest);
            }
        }
    }

    fn emit_cast_instrs(&mut self, from_ty: IrType, to_ty: IrType) {
        use crate::backend::cast::{CastKind, classify_cast};

        match classify_cast(from_ty, to_ty) {
            CastKind::Noop => {}

            CastKind::IntNarrow { .. } => {
                // Truncation: no-op on x86 (use sub-register)
            }

            CastKind::IntWiden { from_ty, .. } => {
                if from_ty.is_unsigned() {
                    match from_ty {
                        IrType::U8 => self.state.emit("    movzbl %al, %eax"),
                        IrType::U16 => self.state.emit("    movzwl %ax, %eax"),
                        // U32 -> I64/U64: no-op on i686 (eax already has 32 bits)
                        _ => {}
                    }
                } else {
                    match from_ty {
                        IrType::I8 => self.state.emit("    movsbl %al, %eax"),
                        IrType::I16 => self.state.emit("    movswl %ax, %eax"),
                        // I32 -> I64/U64: no-op on i686 (eax already has 32 bits)
                        _ => {}
                    }
                }
            }

            CastKind::SignedToUnsignedSameSize { to_ty } => {
                // On i686, same-size signed->unsigned: mask for sub-32-bit types
                match to_ty {
                    IrType::U8 => self.state.emit("    movzbl %al, %eax"),
                    IrType::U16 => self.state.emit("    movzwl %ax, %eax"),
                    _ => {} // U32, U64: no-op
                }
            }

            CastKind::SignedToFloat { to_f64: false, .. } => {
                // Signed int -> F32 via SSE
                self.state.emit("    cvtsi2ssl %eax, %xmm0");
                self.state.emit("    movd %xmm0, %eax");
            }

            CastKind::UnsignedToFloat { to_f64: false, from_u64: false } => {
                // U8/U16/U32 -> F32
                let big_label = self.state.fresh_label("u2f_big");
                let done_label = self.state.fresh_label("u2f_done");
                self.state.emit("    testl %eax, %eax");
                self.state.out.emit_jcc_label("    js", &big_label);
                self.state.emit("    cvtsi2ssl %eax, %xmm0");
                self.state.emit("    movd %xmm0, %eax");
                self.state.out.emit_jmp_label(&done_label);
                self.state.out.emit_named_label(&big_label);
                self.state.emit("    pushl $0");
                self.state.emit("    pushl %eax");
                self.state.emit("    fildq (%esp)");
                self.state.emit("    fstps (%esp)");
                self.state.emit("    popl %eax");
                self.state.emit("    addl $4, %esp");
                self.state.out.emit_named_label(&done_label);
            }

            CastKind::UnsignedToFloat { to_f64: false, from_u64: true } => {
                // U64 -> F32: use x87
                self.state.emit("    subl $8, %esp");
                self.state.emit("    movl %eax, (%esp)");
                self.state.emit("    fildl (%esp)");
                self.state.emit("    fstps (%esp)");
                self.state.emit("    movl (%esp), %eax");
                self.state.emit("    addl $8, %esp");
            }

            CastKind::FloatToSigned { from_f64: false } => {
                // F32 -> signed int via SSE
                self.state.emit("    movd %eax, %xmm0");
                self.state.emit("    cvttss2si %xmm0, %eax");
            }

            CastKind::FloatToUnsigned { from_f64: false, to_u64 } => {
                if to_u64 {
                    // F32 -> U64: use x87
                    self.state.emit("    pushl %eax");
                    self.state.emit("    flds (%esp)");
                    self.state.emit("    addl $4, %esp");
                    self.state.emit("    subl $8, %esp");
                    self.state.emit("    fisttpq (%esp)");
                    self.state.emit("    movl (%esp), %eax");
                    self.state.emit("    movl 4(%esp), %edx");
                    self.state.emit("    addl $8, %esp");
                } else {
                    // F32 -> unsigned int: cvttss2si treats result as signed
                    self.state.emit("    movd %eax, %xmm0");
                    self.state.emit("    cvttss2si %xmm0, %eax");
                }
            }

            // F64/F128 casts are handled by emit_cast override above
            _ => {}
        }
    }

    // --- Global address ---

    fn emit_global_addr(&mut self, dest: &Value, name: &str) {
        if self.state.pic_mode {
            // PIC: use GOT-relative addressing
            // TODO: proper PIC support for i686
            emit!(self.state, "    leal {}@GOTOFF(%ebx), %eax", name);
        } else {
            emit!(self.state, "    movl ${}, %eax", name);
        }
        self.state.reg_cache.invalidate_acc();
        self.store_eax_to(dest);
    }

    fn emit_tls_global_addr(&mut self, dest: &Value, name: &str) {
        // TLS local exec: %gs:0 + offset
        self.state.emit("    movl %gs:0, %eax");
        emit!(self.state, "    addl ${}@NTPOFF, %eax", name);
        self.state.reg_cache.invalidate_acc();
        self.store_eax_to(dest);
    }

    // --- Control flow ---

    fn jump_mnemonic(&self) -> &'static str { "jmp" }
    fn trap_instruction(&self) -> &'static str { "ud2" }

    fn emit_branch_nonzero(&mut self, label: &str) {
        self.state.emit("    testl %eax, %eax");
        emit!(self.state, "    jne {}", label);
    }

    fn emit_jump_indirect(&mut self) {
        self.state.emit("    jmp *%eax");
    }

    fn emit_switch_case_branch(&mut self, case_val: i64, label: &str) {
        let val = case_val as i32;
        if val == 0 {
            self.state.emit("    testl %eax, %eax");
        } else {
            emit!(self.state, "    cmpl ${}, %eax", val);
        }
        emit!(self.state, "    je {}", label);
    }

    fn emit_switch_jump_table(&mut self, val: &Operand, cases: &[(i64, BlockId)], default: &BlockId) {
        use crate::backend::traits::{build_jump_table, emit_jump_table_rodata};

        let (table, min_val, range) = build_jump_table(cases, default);
        let table_id = self.state.next_label_id();
        let table_label = format!(".Ljmptab_{}", table_id);
        let default_label = default.as_label();

        // Load value and range-check
        self.operand_to_eax(val);
        if min_val != 0 {
            emit!(self.state, "    subl ${}, %eax", min_val as i32);
        }
        emit!(self.state, "    cmpl ${}, %eax", range as i32);
        emit!(self.state, "    jae {}", default_label);

        // Load from jump table and indirect jump
        emit!(self.state, "    movl {}(,%eax,4), %eax", table_label);
        self.state.emit("    jmp *%eax");

        // Emit the jump table in .rodata
        emit_jump_table_rodata(self, &table_label, &table);
    }

    // --- Comparison ---

    fn emit_float_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if ty == IrType::F64 {
            // F64: use x87 fucomip for proper 8-byte double comparison
            let swap = matches!(op, IrCmpOp::Slt | IrCmpOp::Ult | IrCmpOp::Sle | IrCmpOp::Ule);
            let (first, second) = if swap { (lhs, rhs) } else { (rhs, lhs) };
            self.emit_f64_load_to_x87(first);
            self.emit_f64_load_to_x87(second);
            // x87 stack: st(0)=second, st(1)=first
            // fucomip compares st(0) with st(1), sets EFLAGS, pops st(0)
            self.state.emit("    fucomip %st(1), %st");
            self.state.emit("    fstp %st(0)"); // pop remaining st(0)

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
            self.state.emit("    movzbl %al, %eax");
            self.state.reg_cache.invalidate_acc();
            self.store_eax_to(dest);
            return;
        }
        // F32: Use SSE for float comparisons
        let swap_operands = matches!(op, IrCmpOp::Slt | IrCmpOp::Ult | IrCmpOp::Sle | IrCmpOp::Ule);
        let (first, second) = if swap_operands { (rhs, lhs) } else { (lhs, rhs) };

        self.operand_to_eax(first);
        self.state.emit("    movd %eax, %xmm0");
        self.operand_to_ecx(second);
        self.state.emit("    movd %ecx, %xmm1");
        self.state.emit("    ucomiss %xmm1, %xmm0");

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
        self.state.emit("    movzbl %al, %eax");
        self.state.reg_cache.invalidate_acc();
        self.store_eax_to(dest);
    }

    fn emit_f128_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand) {
        // x87 comparison using fucomip
        let swap = matches!(op, IrCmpOp::Slt | IrCmpOp::Ult | IrCmpOp::Sle | IrCmpOp::Ule);
        let (first, second) = if swap { (lhs, rhs) } else { (rhs, lhs) };
        self.emit_f128_load_to_x87(first);
        self.emit_f128_load_to_x87(second);
        self.state.emit("    fucomip %st(1), %st");
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
        self.state.emit("    movzbl %al, %eax");
        self.state.reg_cache.invalidate_acc();
        self.store_eax_to(dest);
    }

    fn emit_int_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand, _ty: IrType) {
        self.operand_to_eax(lhs);
        self.operand_to_ecx(rhs);
        self.state.emit("    cmpl %ecx, %eax");

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
        emit!(self.state, "    {} %al", set_instr);
        self.state.emit("    movzbl %al, %eax");
        self.state.reg_cache.invalidate_acc();
        self.store_eax_to(dest);
    }

    fn emit_fused_cmp_branch(
        &mut self,
        op: IrCmpOp,
        lhs: &Operand,
        rhs: &Operand,
        _ty: IrType,
        true_label: &str,
        false_label: &str,
    ) {
        self.operand_to_eax(lhs);
        self.operand_to_ecx(rhs);
        self.state.emit("    cmpl %ecx, %eax");

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
        emit!(self.state, "    {} {}", jcc, true_label);
        emit!(self.state, "    jmp {}", false_label);
        self.state.reg_cache.invalidate_all();
    }

    // --- Unary operations ---

    /// Override unary op to handle F64/F128 negation properly on i686.
    /// The default path uses emit_load_operand which only gives 32 bits for F64.
    fn emit_unaryop(&mut self, dest: &Value, op: IrUnaryOp, src: &Operand, ty: IrType) {
        if ty == IrType::F64 && op == IrUnaryOp::Neg {
            // F64 negation: load onto x87, negate, store back
            self.emit_f64_load_to_x87(src);
            self.state.emit("    fchs");
            self.emit_f64_store_from_x87(dest);
            self.state.reg_cache.invalidate_acc();
            return;
        }
        // Delegate to default for all other types/ops
        crate::backend::traits::emit_unaryop_default(self, dest, op, src, ty);
    }

    /// F128 (long double) negation on i686 using x87 fchs.
    fn emit_f128_neg(&mut self, dest: &Value, src: &Operand) {
        self.emit_f128_load_to_x87(src);
        self.state.emit("    fchs");
        if let Some(slot) = self.state.get_slot(dest.0) {
            emit!(self.state, "    fstpt {}(%ebp)", slot.0);
        }
        self.state.reg_cache.invalidate_acc();
    }

    fn emit_float_neg(&mut self, ty: IrType) {
        if ty == IrType::F32 {
            self.state.emit("    movd %eax, %xmm0");
            self.state.emit("    movl $0x80000000, %ecx");
            self.state.emit("    movd %ecx, %xmm1");
            self.state.emit("    xorps %xmm1, %xmm0");
            self.state.emit("    movd %xmm0, %eax");
        } else {
            // F64/F128 handled by emit_unaryop/emit_f128_neg overrides above.
            // Fallback: flip sign bit in eax (only works for low 32 bits).
            self.state.emit("    xorl $0x80000000, %eax");
        }
    }

    fn emit_int_neg(&mut self, _ty: IrType) {
        self.state.emit("    negl %eax");
    }

    fn emit_int_not(&mut self, _ty: IrType) {
        self.state.emit("    notl %eax");
    }

    fn emit_int_clz(&mut self, ty: IrType) {
        if matches!(ty, IrType::I32 | IrType::U32 | IrType::Ptr) {
            self.state.emit("    lzcntl %eax, %eax");
        } else if matches!(ty, IrType::I16 | IrType::U16) {
            self.state.emit("    lzcntw %ax, %ax");
            // lzcntw counts leading zeros in 16-bit, result in ax
        } else {
            self.state.emit("    lzcntl %eax, %eax");
        }
    }

    fn emit_int_ctz(&mut self, ty: IrType) {
        if matches!(ty, IrType::I32 | IrType::U32 | IrType::Ptr) {
            self.state.emit("    tzcntl %eax, %eax");
        } else {
            self.state.emit("    tzcntl %eax, %eax");
        }
    }

    fn emit_int_bswap(&mut self, ty: IrType) {
        match ty {
            IrType::I16 | IrType::U16 => self.state.emit("    rolw $8, %ax"),
            IrType::I32 | IrType::U32 | IrType::Ptr => self.state.emit("    bswapl %eax"),
            _ => self.state.emit("    bswapl %eax"),
        }
    }

    fn emit_int_popcount(&mut self, _ty: IrType) {
        self.state.emit("    popcntl %eax, %eax");
    }

    // --- Call ---

    fn call_abi_config(&self) -> call_abi::CallAbiConfig {
        call_abi::CallAbiConfig {
            max_int_regs: 0,  // cdecl: no register args
            max_float_regs: 0,
            align_i128_pairs: false,
            f128_in_fp_regs: false,
            f128_in_gp_pairs: false,
            variadic_floats_in_gp: false,
            large_struct_by_ref: false,
            use_sysv_struct_classification: false,
        }
    }

    fn emit_call_compute_stack_space(&self, arg_classes: &[call_abi::CallArgClass], arg_types: &[IrType]) -> usize {
        // Count total bytes needed for all arguments (all go on stack in cdecl).
        // On i686 cdecl, each arg occupies at least 4 bytes (one stack slot).
        // F64 (double) and I64/U64 occupy 8 bytes.
        let mut total = 0;
        for (i, ac) in arg_classes.iter().enumerate() {
            let ty = if i < arg_types.len() { arg_types[i] } else { IrType::I32 };
            match ac {
                call_abi::CallArgClass::Stack => {
                    // F64 and I64/U64 need 8 bytes; everything else needs 4
                    match ty {
                        IrType::F64 | IrType::I64 | IrType::U64 => total += 8,
                        _ => total += 4,
                    }
                }
                call_abi::CallArgClass::F128Stack => total += 12, // x87 long double = 12 bytes
                call_abi::CallArgClass::I128Stack => total += 16,
                call_abi::CallArgClass::StructByValStack { size } => total += (*size + 3) & !3,
                call_abi::CallArgClass::LargeStructStack { size } => total += (*size + 3) & !3,
                _ => total += 4, // fallback for any remaining classes
            }
        }
        // Align to 16 bytes
        (total + 15) & !15
    }

    fn emit_call_stack_args(&mut self, args: &[Operand], arg_classes: &[call_abi::CallArgClass],
                            arg_types: &[IrType], stack_arg_space: usize,
                            _fptr_spill: usize, _f128_temp_space: usize) -> i64 {
        if stack_arg_space > 0 {
            emit!(self.state, "    subl ${}, %esp", stack_arg_space);
        }

        // Place arguments onto the stack (cdecl: all args on stack)
        let mut stack_offset: usize = 0;
        for (i, ac) in arg_classes.iter().enumerate() {
            let ty = arg_types[i];
            match ac {
                call_abi::CallArgClass::I128Stack => {
                    // 128-bit: copy 16 bytes
                    if let Operand::Value(v) = &args[i] {
                        if let Some(slot) = self.state.get_slot(v.0) {
                            for j in (0..16).step_by(4) {
                                emit!(self.state, "    movl {}(%ebp), %eax", slot.0 + j as i64);
                                emit!(self.state, "    movl %eax, {}(%esp)", stack_offset + j);
                            }
                        } else {
                            // Register-allocated: store low 32 bits, zero the rest
                            self.operand_to_eax(&args[i]);
                            emit!(self.state, "    movl %eax, {}(%esp)", stack_offset);
                            for j in (4..16).step_by(4) {
                                emit!(self.state, "    movl $0, {}(%esp)", stack_offset + j);
                            }
                        }
                    }
                    stack_offset += 16;
                }
                call_abi::CallArgClass::F128Stack => {
                    // Long double: copy 12 bytes
                    if let Operand::Value(v) = &args[i] {
                        if let Some(slot) = self.state.get_slot(v.0) {
                            for j in (0..12).step_by(4) {
                                emit!(self.state, "    movl {}(%ebp), %eax", slot.0 + j as i64);
                                emit!(self.state, "    movl %eax, {}(%esp)", stack_offset + j);
                            }
                        } else {
                            // Register-allocated: store low 32 bits, zero the rest
                            self.operand_to_eax(&args[i]);
                            emit!(self.state, "    movl %eax, {}(%esp)", stack_offset);
                            for j in (4..12).step_by(4) {
                                emit!(self.state, "    movl $0, {}(%esp)", stack_offset + j);
                            }
                        }
                    }
                    stack_offset += 12;
                }
                call_abi::CallArgClass::StructByValStack { size } |
                call_abi::CallArgClass::LargeStructStack { size } => {
                    // Copy struct data to the call stack.
                    // The operand is a pointer to the struct data. For allocas,
                    // the data is directly in the stack slot. For other pointers
                    // (globals, register-allocated), we load through the pointer.
                    let sz = *size;
                    if let Operand::Value(v) = &args[i] {
                        if self.state.is_alloca(v.0) {
                            // Alloca: struct data is directly in the slot
                            if let Some(slot) = self.state.get_slot(v.0) {
                                let mut copied = 0usize;
                                while copied + 4 <= sz {
                                    emit!(self.state, "    movl {}(%ebp), %eax", slot.0 + copied as i64);
                                    emit!(self.state, "    movl %eax, {}(%esp)", stack_offset + copied);
                                    copied += 4;
                                }
                                while copied < sz {
                                    emit!(self.state, "    movb {}(%ebp), %al", slot.0 + copied as i64);
                                    emit!(self.state, "    movb %al, {}(%esp)", stack_offset + copied);
                                    copied += 1;
                                }
                            }
                        } else {
                            // Non-alloca: value is a pointer to struct data.
                            // Load pointer into ecx, then copy through it.
                            self.operand_to_eax(&args[i]);
                            self.state.emit("    movl %eax, %ecx");
                            let mut copied = 0usize;
                            while copied + 4 <= sz {
                                emit!(self.state, "    movl {}(%ecx), %eax", copied);
                                emit!(self.state, "    movl %eax, {}(%esp)", stack_offset + copied);
                                copied += 4;
                            }
                            while copied < sz {
                                emit!(self.state, "    movb {}(%ecx), %al", copied);
                                emit!(self.state, "    movb %al, {}(%esp)", stack_offset + copied);
                                copied += 1;
                            }
                        }
                    }
                    stack_offset += (sz + 3) & !3;
                }
                call_abi::CallArgClass::Stack => {
                    // cdecl: all args are passed as full stack slots.
                    // F64 and I64/U64 take 8 bytes; everything else takes 4 bytes.
                    // Sub-int types (I8, I16) are promoted to full 4-byte words.
                    if ty == IrType::F64 {
                        // Double: copy 8 bytes
                        if let Operand::Value(v) = &args[i] {
                            if let Some(slot) = self.state.get_slot(v.0) {
                                emit!(self.state, "    movl {}(%ebp), %eax", slot.0);
                                emit!(self.state, "    movl %eax, {}(%esp)", stack_offset);
                                emit!(self.state, "    movl {}(%ebp), %eax", slot.0 + 4);
                                emit!(self.state, "    movl %eax, {}(%esp)", stack_offset + 4);
                            } else {
                                // Register-allocated F64: on i686, only the low 32 bits
                                // are in the register. Store low word; high word is zero.
                                self.operand_to_eax(&args[i]);
                                emit!(self.state, "    movl %eax, {}(%esp)", stack_offset);
                                emit!(self.state, "    movl $0, {}(%esp)", stack_offset + 4);
                            }
                        } else if let Operand::Const(IrConst::F64(f)) = &args[i] {
                            // F64 constant: store both 32-bit halves
                            let bits = f.to_bits();
                            let lo = (bits & 0xFFFF_FFFF) as u32;
                            let hi = (bits >> 32) as u32;
                            emit!(self.state, "    movl ${}, {}(%esp)", lo as i32, stack_offset);
                            emit!(self.state, "    movl ${}, {}(%esp)", hi as i32, stack_offset + 4);
                        } else {
                            // Fallback: only low 32 bits available via accumulator
                            self.operand_to_eax(&args[i]);
                            emit!(self.state, "    movl %eax, {}(%esp)", stack_offset);
                            emit!(self.state, "    movl $0, {}(%esp)", stack_offset + 4);
                        }
                        stack_offset += 8;
                    } else if ty == IrType::I64 || ty == IrType::U64 {
                        // 64-bit integer: copy 8 bytes
                        if let Operand::Value(v) = &args[i] {
                            if let Some(slot) = self.state.get_slot(v.0) {
                                emit!(self.state, "    movl {}(%ebp), %eax", slot.0);
                                emit!(self.state, "    movl %eax, {}(%esp)", stack_offset);
                                emit!(self.state, "    movl {}(%ebp), %eax", slot.0 + 4);
                                emit!(self.state, "    movl %eax, {}(%esp)", stack_offset + 4);
                            } else {
                                // Register-allocated I64/U64: on i686, the register holds
                                // only the low 32 bits; high 32 bits are zero.
                                // This commonly happens for comparison/logical results
                                // which the IR types as I64 but are really boolean values.
                                self.operand_to_eax(&args[i]);
                                emit!(self.state, "    movl %eax, {}(%esp)", stack_offset);
                                emit!(self.state, "    movl $0, {}(%esp)", stack_offset + 4);
                            }
                        } else {
                            self.operand_to_eax(&args[i]);
                            emit!(self.state, "    movl %eax, {}(%esp)", stack_offset);
                            // High 32 bits: for constants, compute from i64 value
                            if let Operand::Const(IrConst::I64(v)) = &args[i] {
                                let hi = ((*v as u64) >> 32) as i32;
                                emit!(self.state, "    movl ${}, {}(%esp)", hi, stack_offset + 4);
                            } else {
                                emit!(self.state, "    movl $0, {}(%esp)", stack_offset + 4);
                            }
                        }
                        stack_offset += 8;
                    } else {
                        // All other scalars (I8, I16, I32, U8, U16, U32, F32, Ptr):
                        // Always store as a full 4-byte word via movl %eax.
                        // operand_to_eax already loads sub-int types with sign/zero extension.
                        self.operand_to_eax(&args[i]);
                        emit!(self.state, "    movl %eax, {}(%esp)", stack_offset);
                        stack_offset += 4;
                    }
                }
                _ => {
                    // Fallback: put on stack as 4-byte value
                    self.operand_to_eax(&args[i]);
                    emit!(self.state, "    movl %eax, {}(%esp)", stack_offset);
                    stack_offset += 4;
                }
            }
        }

        stack_arg_space as i64
    }

    fn emit_call_reg_args(&mut self, _args: &[Operand], _arg_classes: &[call_abi::CallArgClass],
                          _arg_types: &[IrType], _total_sp_adjust: i64,
                          _f128_temp_space: usize, _stack_arg_space: usize) {
        // cdecl: no register args, nothing to do
    }

    fn emit_call_instruction(&mut self, direct_name: Option<&str>, func_ptr: Option<&Operand>,
                             indirect: bool, _stack_arg_space: usize) {
        if let Some(name) = direct_name {
            emit!(self.state, "    call {}", name);
        } else if indirect {
            if let Some(fptr) = func_ptr {
                self.operand_to_eax(fptr);
            }
            self.state.emit("    call *%eax");
        }
    }

    fn emit_call_cleanup(&mut self, stack_arg_space: usize, _f128_temp_space: usize, _indirect: bool) {
        // cdecl: caller cleans up the stack
        if stack_arg_space > 0 {
            emit!(self.state, "    addl ${}, %esp", stack_arg_space);
        }
    }

    fn emit_call_store_result(&mut self, dest: &Value, return_type: IrType) {
        if return_type == IrType::I64 || return_type == IrType::U64 {
            // I64/U64 returned in eax:edx on i686 — save both halves
            if let Some(slot) = self.state.get_slot(dest.0) {
                emit!(self.state, "    movl %eax, {}(%ebp)", slot.0);
                emit!(self.state, "    movl %edx, {}(%ebp)", slot.0 + 4);
            }
            self.state.reg_cache.invalidate_acc();
        } else if crate::backend::generation::is_i128_type(return_type) {
            self.emit_call_store_i128_result(dest);
        } else if return_type.is_long_double() {
            self.emit_call_store_f128_result(dest);
        } else if return_type == IrType::F32 {
            self.emit_call_move_f32_to_acc();
            self.emit_store_result(dest);
        } else if return_type == IrType::F64 {
            // F64 returned in st(0) on i686 cdecl — store full 8 bytes to dest slot
            self.emit_f64_store_from_x87(dest);
            self.state.reg_cache.invalidate_acc();
        } else {
            self.emit_store_result(dest);
        }
    }

    fn emit_call_store_i128_result(&mut self, dest: &Value) {
        // i686: only low 64 bits (eax:edx) stored here. Full i128 uses sret.
        if let Some(slot) = self.state.get_slot(dest.0) {
            emit!(self.state, "    movl %eax, {}(%ebp)", slot.0);
            emit!(self.state, "    movl %edx, {}(%ebp)", slot.0 + 4);
        }
    }

    fn emit_call_store_f128_result(&mut self, dest: &Value) {
        // F128 returned in st(0) on x87
        if let Some(slot) = self.state.get_slot(dest.0) {
            emit!(self.state, "    fstpt {}(%ebp)", slot.0);
        }
    }

    fn emit_call_move_f32_to_acc(&mut self) {
        // F32 returned in st(0) on i686 cdecl
        self.state.emit("    subl $4, %esp");
        self.state.emit("    fstps (%esp)");
        self.state.emit("    movl (%esp), %eax");
        self.state.emit("    addl $4, %esp");
    }

    fn emit_call_move_f64_to_acc(&mut self) {
        // F64 returned in st(0) on i686 cdecl
        self.state.emit("    subl $8, %esp");
        self.state.emit("    fstpl (%esp)");
        self.state.emit("    movl (%esp), %eax");
        self.state.emit("    addl $8, %esp");
    }

    // --- Return ---

    fn current_return_type(&self) -> IrType { self.current_return_type }

    fn emit_return(&mut self, val: Option<&Operand>, frame_size: i64) {
        if let Some(val) = val {
            let ret_ty = self.current_return_type;
            // I64/U64 returns on i686: load both halves into eax:edx.
            // This is used for small struct returns (5-8 bytes packed as I64)
            // and for long long return values.
            if ret_ty == IrType::I64 || ret_ty == IrType::U64 {
                self.emit_load_acc_pair(val);
                self.emit_epilogue_and_ret(frame_size);
                return;
            }
            // F64 returns on i686: load full 8-byte double onto x87 st(0).
            // We use emit_f64_load_to_x87 to load from the 8-byte stack slot,
            // bypassing the default emit_load_operand which only gives 32 bits.
            if ret_ty == IrType::F64 {
                self.emit_f64_load_to_x87(val);
                self.emit_epilogue_and_ret(frame_size);
                return;
            }
            // F128 returns on i686: load long double onto x87 st(0).
            if ret_ty.is_long_double() {
                self.emit_f128_load_to_x87(val);
                self.emit_epilogue_and_ret(frame_size);
                return;
            }
        }
        // Delegate all other cases (I128, F32, scalar int) to default
        crate::backend::traits::emit_return_default(self, val, frame_size);
    }

    fn emit_return_i128_to_regs(&mut self) {
        // eax:edx already holds the low 64 bits
    }

    fn emit_return_f128_to_reg(&mut self) {
        // F128 (long double) is 10/12 bytes. The value should be in the stack slot.
        // The emit_return_default path calls emit_load_operand (which gets eax) then
        // this function. But for F128, the value is already in the slot from x87 ops.
        // We need to load it from the source's slot. Since the default path has already
        // loaded into eax (just low 32 bits), we push both halves and use x87.
        // Actually, F128 return is handled via the slot-based path in emit_return.
        // This function is called after emit_load_operand, which only puts low 32 bits
        // into eax. For proper F128 return, the caller should use the slot directly.
        // For now, just push as integer (lossy fallback).
        self.state.emit("    pushl %eax");
        self.state.emit("    fildl (%esp)");
        self.state.emit("    addl $4, %esp");
    }

    fn emit_return_f32_to_reg(&mut self) {
        // Put F32 onto x87 st(0) for return
        self.state.emit("    pushl %eax");
        self.state.emit("    flds (%esp)");
        self.state.emit("    addl $4, %esp");
    }

    fn emit_return_f64_to_reg(&mut self) {
        // Put F64 onto x87 st(0) for return.
        // The F64 value is in eax:edx (both halves loaded by emit_load_acc_pair
        // or available from the 8-byte stack slot).
        // Push both halves and use fldl to load as 8-byte double.
        self.state.emit("    pushl %edx");
        self.state.emit("    pushl %eax");
        self.state.emit("    fldl (%esp)");
        self.state.emit("    addl $8, %esp");
    }

    fn emit_return_int_to_reg(&mut self) {
        // eax already holds the return value
    }

    fn emit_epilogue_and_ret(&mut self, frame_size: i64) {
        self.emit_epilogue(frame_size);
        self.state.emit("    ret");
    }

    // --- Typed store/load ---

    /// Override emit_load for I64/U64/F64: load 8 bytes via eax:edx pair, then store both halves.
    fn emit_load(&mut self, dest: &Value, ptr: &Value, ty: IrType) {
        if ty == IrType::I64 || ty == IrType::U64 || ty == IrType::F64 {
            let addr = self.state.resolve_slot_addr(ptr.0);
            if let Some(addr) = addr {
                match addr {
                    crate::backend::state::SlotAddr::OverAligned(slot, id) => {
                        self.emit_alloca_aligned_addr(slot, id);
                        // Load 8 bytes from (%ecx)
                        self.state.emit("    movl (%ecx), %eax");
                        self.state.emit("    movl 4(%ecx), %edx");
                    }
                    crate::backend::state::SlotAddr::Direct(slot) => {
                        emit!(self.state, "    movl {}(%ebp), %eax", slot.0);
                        emit!(self.state, "    movl {}(%ebp), %edx", slot.0 + 4);
                    }
                    crate::backend::state::SlotAddr::Indirect(slot) => {
                        self.emit_load_ptr_from_slot(slot, ptr.0);
                        self.state.emit("    movl (%ecx), %eax");
                        self.state.emit("    movl 4(%ecx), %edx");
                    }
                }
                // Store the full 8-byte value to dest's slot
                self.emit_store_acc_pair(dest);
            }
            return;
        }
        crate::backend::traits::emit_load_default(self, dest, ptr, ty);
    }

    /// Override emit_store for I64/U64/F64: store 8 bytes via eax:edx pair.
    fn emit_store(&mut self, val: &Operand, ptr: &Value, ty: IrType) {
        if ty == IrType::I64 || ty == IrType::U64 || ty == IrType::F64 {
            let addr = self.state.resolve_slot_addr(ptr.0);
            // Load 8-byte value into eax:edx
            self.emit_load_acc_pair(val);
            if let Some(addr) = addr {
                match addr {
                    crate::backend::state::SlotAddr::OverAligned(slot, id) => {
                        self.state.emit("    pushl %edx");
                        self.state.emit("    pushl %eax");
                        self.emit_alloca_aligned_addr(slot, id);
                        self.state.emit("    popl %eax");
                        self.state.emit("    movl %eax, (%ecx)");
                        self.state.emit("    popl %edx");
                        self.state.emit("    movl %edx, 4(%ecx)");
                    }
                    crate::backend::state::SlotAddr::Direct(slot) => {
                        emit!(self.state, "    movl %eax, {}(%ebp)", slot.0);
                        emit!(self.state, "    movl %edx, {}(%ebp)", slot.0 + 4);
                    }
                    crate::backend::state::SlotAddr::Indirect(slot) => {
                        self.state.emit("    pushl %edx");
                        self.state.emit("    pushl %eax");
                        self.emit_load_ptr_from_slot(slot, ptr.0);
                        self.state.emit("    popl %eax");
                        self.state.emit("    movl %eax, (%ecx)");
                        self.state.emit("    popl %edx");
                        self.state.emit("    movl %edx, 4(%ecx)");
                    }
                }
            }
            self.state.reg_cache.invalidate_acc();
            return;
        }
        crate::backend::traits::emit_store_default(self, val, ptr, ty);
    }

    /// Override load with GEP-folded constant offset for I64/U64/F64 on i686.
    /// The default path uses emit_load_operand which only handles 32 bits.
    fn emit_load_with_const_offset(&mut self, dest: &Value, base: &Value, offset: i64, ty: IrType) {
        if ty == IrType::I64 || ty == IrType::U64 || ty == IrType::F64 {
            let addr = self.state.resolve_slot_addr(base.0);
            if let Some(addr) = addr {
                match addr {
                    crate::backend::state::SlotAddr::OverAligned(slot, id) => {
                        self.emit_alloca_aligned_addr(slot, id);
                        if offset != 0 {
                            self.emit_add_offset_to_addr_reg(offset);
                        }
                        self.state.emit("    movl (%ecx), %eax");
                        self.state.emit("    movl 4(%ecx), %edx");
                    }
                    crate::backend::state::SlotAddr::Direct(slot) => {
                        let folded = slot.0 + offset;
                        emit!(self.state, "    movl {}(%ebp), %eax", folded);
                        emit!(self.state, "    movl {}(%ebp), %edx", folded + 4);
                    }
                    crate::backend::state::SlotAddr::Indirect(slot) => {
                        self.emit_load_ptr_from_slot(slot, base.0);
                        if offset != 0 {
                            self.emit_add_offset_to_addr_reg(offset);
                        }
                        self.state.emit("    movl (%ecx), %eax");
                        self.state.emit("    movl 4(%ecx), %edx");
                    }
                }
                self.emit_store_acc_pair(dest);
            }
            return;
        }
        // Delegate to default for other types
        let addr = self.state.resolve_slot_addr(base.0);
        if let Some(addr) = addr {
            let load_instr = self.load_instr_for_type(ty);
            match addr {
                crate::backend::state::SlotAddr::OverAligned(slot, id) => {
                    self.emit_alloca_aligned_addr(slot, id);
                    self.emit_add_offset_to_addr_reg(offset);
                    self.emit_typed_load_indirect(load_instr);
                }
                crate::backend::state::SlotAddr::Direct(slot) => {
                    let folded_slot = StackSlot(slot.0 + offset);
                    self.emit_typed_load_from_slot(load_instr, folded_slot);
                }
                crate::backend::state::SlotAddr::Indirect(slot) => {
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

    /// Override store with GEP-folded constant offset for I64/U64/F64 on i686.
    /// The default path uses emit_load_operand which only handles 32 bits.
    fn emit_store_with_const_offset(&mut self, val: &Operand, base: &Value, offset: i64, ty: IrType) {
        if ty == IrType::I64 || ty == IrType::U64 || ty == IrType::F64 {
            self.emit_load_acc_pair(val);
            let addr = self.state.resolve_slot_addr(base.0);
            if let Some(addr) = addr {
                match addr {
                    crate::backend::state::SlotAddr::OverAligned(slot, id) => {
                        self.state.emit("    pushl %edx");
                        self.state.emit("    pushl %eax");
                        self.emit_alloca_aligned_addr(slot, id);
                        if offset != 0 {
                            self.emit_add_offset_to_addr_reg(offset);
                        }
                        self.state.emit("    popl %eax");
                        self.state.emit("    movl %eax, (%ecx)");
                        self.state.emit("    popl %edx");
                        self.state.emit("    movl %edx, 4(%ecx)");
                    }
                    crate::backend::state::SlotAddr::Direct(slot) => {
                        let folded = slot.0 + offset;
                        emit!(self.state, "    movl %eax, {}(%ebp)", folded);
                        emit!(self.state, "    movl %edx, {}(%ebp)", folded + 4);
                    }
                    crate::backend::state::SlotAddr::Indirect(slot) => {
                        self.state.emit("    pushl %edx");
                        self.state.emit("    pushl %eax");
                        self.emit_load_ptr_from_slot(slot, base.0);
                        if offset != 0 {
                            self.emit_add_offset_to_addr_reg(offset);
                        }
                        self.state.emit("    popl %eax");
                        self.state.emit("    movl %eax, (%ecx)");
                        self.state.emit("    popl %edx");
                        self.state.emit("    movl %edx, 4(%ecx)");
                    }
                }
            }
            self.state.reg_cache.invalidate_acc();
            return;
        }
        // Delegate to default for other types
        self.operand_to_eax(val);
        let addr = self.state.resolve_slot_addr(base.0);
        if let Some(addr) = addr {
            let store_instr = self.store_instr_for_type(ty);
            match addr {
                crate::backend::state::SlotAddr::OverAligned(slot, id) => {
                    self.emit_save_acc();
                    self.emit_alloca_aligned_addr(slot, id);
                    self.emit_add_offset_to_addr_reg(offset);
                    self.emit_typed_store_indirect(store_instr, ty);
                }
                crate::backend::state::SlotAddr::Direct(slot) => {
                    let folded_slot = StackSlot(slot.0 + offset);
                    self.emit_typed_store_to_slot(store_instr, ty, folded_slot);
                }
                crate::backend::state::SlotAddr::Indirect(slot) => {
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

    fn store_instr_for_type(&self, ty: IrType) -> &'static str {
        self.mov_store_for_type(ty)
    }

    fn load_instr_for_type(&self, ty: IrType) -> &'static str {
        self.mov_load_for_type(ty)
    }

    fn emit_typed_store_to_slot(&mut self, instr: &'static str, ty: IrType, slot: StackSlot) {
        let reg = self.eax_for_type(ty);
        emit!(self.state, "    {} {}, {}(%ebp)", instr, reg, slot.0);
    }

    fn emit_typed_load_from_slot(&mut self, instr: &'static str, slot: StackSlot) {
        emit!(self.state, "    {} {}(%ebp), %eax", instr, slot.0);
    }

    fn emit_save_acc(&mut self) {
        self.state.emit("    movl %eax, %edx");
    }

    fn emit_load_ptr_from_slot(&mut self, slot: StackSlot, val_id: u32) {
        if let Some(phys) = self.reg_assignments.get(&val_id).copied() {
            let reg = phys_reg_name(phys);
            emit!(self.state, "    movl %{}, %ecx", reg);
        } else {
            emit!(self.state, "    movl {}(%ebp), %ecx", slot.0);
        }
    }

    fn emit_typed_store_indirect(&mut self, instr: &'static str, ty: IrType) {
        // Store from edx through ecx
        let reg = match ty {
            IrType::I8 | IrType::U8 => "%dl",
            IrType::I16 | IrType::U16 => "%dx",
            _ => "%edx",
        };
        emit!(self.state, "    {} {}, (%ecx)", instr, reg);
    }

    fn emit_typed_load_indirect(&mut self, instr: &'static str) {
        emit!(self.state, "    {} (%ecx), %eax", instr);
    }

    // --- GEP primitives ---

    fn emit_slot_addr_to_secondary(&mut self, slot: StackSlot, is_alloca: bool, val_id: u32) {
        if is_alloca {
            emit!(self.state, "    leal {}(%ebp), %ecx", slot.0);
        } else if let Some(phys) = self.reg_assignments.get(&val_id).copied() {
            let reg = phys_reg_name(phys);
            emit!(self.state, "    movl %{}, %ecx", reg);
        } else {
            emit!(self.state, "    movl {}(%ebp), %ecx", slot.0);
        }
    }

    fn emit_add_secondary_to_acc(&mut self) {
        self.state.emit("    addl %ecx, %eax");
    }

    fn emit_gep_direct_const(&mut self, slot: StackSlot, offset: i64) {
        let total = slot.0 + offset;
        emit!(self.state, "    leal {}(%ebp), %eax", total);
    }

    fn emit_gep_indirect_const(&mut self, slot: StackSlot, offset: i64, val_id: u32) {
        if let Some(phys) = self.reg_assignments.get(&val_id).copied() {
            let reg = phys_reg_name(phys);
            if offset == 0 {
                emit!(self.state, "    movl %{}, %eax", reg);
            } else {
                emit!(self.state, "    leal {}(%{}), %eax", offset, reg);
            }
        } else {
            emit!(self.state, "    movl {}(%ebp), %eax", slot.0);
            if offset != 0 {
                emit!(self.state, "    addl ${}, %eax", offset as i32);
            }
        }
    }

    fn emit_gep_add_const_to_acc(&mut self, offset: i64) {
        if offset != 0 {
            emit!(self.state, "    addl ${}, %eax", offset as i32);
        }
    }

    // --- Dynamic alloca ---

    fn emit_add_imm_to_acc(&mut self, imm: i64) {
        emit!(self.state, "    addl ${}, %eax", imm as i32);
    }

    fn emit_round_up_acc_to_16(&mut self) {
        self.state.emit("    addl $15, %eax");
        self.state.emit("    andl $-16, %eax");
    }

    fn emit_sub_sp_by_acc(&mut self) {
        self.state.emit("    subl %eax, %esp");
    }

    fn emit_mov_sp_to_acc(&mut self) {
        self.state.emit("    movl %esp, %eax");
        self.state.reg_cache.invalidate_acc();
    }

    fn emit_mov_acc_to_sp(&mut self) {
        self.state.emit("    movl %eax, %esp");
    }

    fn emit_align_acc(&mut self, align: usize) {
        emit!(self.state, "    addl ${}, %eax", align - 1);
        emit!(self.state, "    andl ${}, %eax", -(align as i32));
    }

    // --- Memcpy ---

    fn emit_memcpy_load_dest_addr(&mut self, slot: StackSlot, is_alloca: bool, val_id: u32) {
        if is_alloca {
            emit!(self.state, "    leal {}(%ebp), %edi", slot.0);
        } else if let Some(phys) = self.reg_assignments.get(&val_id).copied() {
            let reg = phys_reg_name(phys);
            emit!(self.state, "    movl %{}, %edi", reg);
        } else {
            emit!(self.state, "    movl {}(%ebp), %edi", slot.0);
        }
    }

    fn emit_memcpy_load_src_addr(&mut self, slot: StackSlot, is_alloca: bool, val_id: u32) {
        if is_alloca {
            emit!(self.state, "    leal {}(%ebp), %esi", slot.0);
        } else if let Some(phys) = self.reg_assignments.get(&val_id).copied() {
            let reg = phys_reg_name(phys);
            emit!(self.state, "    movl %{}, %esi", reg);
        } else {
            emit!(self.state, "    movl {}(%ebp), %esi", slot.0);
        }
    }

    fn emit_memcpy_store_dest_from_acc(&mut self) {
        self.state.emit("    movl %eax, %edi");
    }

    fn emit_memcpy_store_src_from_acc(&mut self) {
        self.state.emit("    movl %eax, %esi");
    }

    fn emit_memcpy_impl(&mut self, size: usize) {
        emit!(self.state, "    movl ${}, %ecx", size);
        self.state.emit("    rep movsb");
    }

    // --- va_arg ---

    fn emit_va_arg(&mut self, dest: &Value, va_list_ptr: &Value, result_ty: IrType) {
        // i686 cdecl: va_list is just a char* pointer to the stack
        // va_arg increments the pointer by the argument size
        if let Some(va_slot) = self.state.get_slot(va_list_ptr.0) {
            // Load current va_list pointer
            emit!(self.state, "    movl {}(%ebp), %ecx", va_slot.0);
            // Load the value at the pointer
            if is_i128_type(result_ty) {
                if let Some(dest_slot) = self.state.get_slot(dest.0) {
                    for i in (0..16).step_by(4) {
                        emit!(self.state, "    movl {}(%ecx), %eax", i);
                        emit!(self.state, "    movl %eax, {}(%ebp)", dest_slot.0 + i as i64);
                    }
                }
                emit!(self.state, "    addl $16, %ecx");
            } else if result_ty == IrType::F128 {
                if let Some(dest_slot) = self.state.get_slot(dest.0) {
                    emit!(self.state, "    fldt (%ecx)");
                    emit!(self.state, "    fstpt {}(%ebp)", dest_slot.0);
                }
                emit!(self.state, "    addl $12, %ecx");
            } else if result_ty == IrType::F64 {
                if let Some(dest_slot) = self.state.get_slot(dest.0) {
                    emit!(self.state, "    movl (%ecx), %eax");
                    emit!(self.state, "    movl %eax, {}(%ebp)", dest_slot.0);
                    emit!(self.state, "    movl 4(%ecx), %eax");
                    emit!(self.state, "    movl %eax, {}(%ebp)", dest_slot.0 + 4);
                }
                emit!(self.state, "    addl $8, %ecx");
            } else {
                let load_instr = self.mov_load_for_type(result_ty);
                emit!(self.state, "    {} (%ecx), %eax", load_instr);
                self.store_eax_to(dest);
                // Advance pointer by 4 (minimum push size on i686 stack)
                let advance = result_ty.size().max(4);
                emit!(self.state, "    addl ${}, %ecx", advance);
            }
            // Store updated va_list pointer back
            emit!(self.state, "    movl %ecx, {}(%ebp)", va_slot.0);
        }
    }

    fn emit_va_start(&mut self, va_list_ptr: &Value) {
        // i686 cdecl: va_list = pointer to first unnamed arg on the stack.
        // Stack layout above ebp: [ret addr (4)] [params...]
        // Named params start at 8(%ebp) and occupy va_named_stack_bytes bytes.
        // Variadic args start immediately after: 8 + va_named_stack_bytes.
        let vararg_offset = 8 + self.va_named_stack_bytes as i64;
        if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
            emit!(self.state, "    leal {}(%ebp), %eax", vararg_offset);
            emit!(self.state, "    movl %eax, {}(%ebp)", slot.0);
        }
    }

    fn emit_va_copy(&mut self, dest_ptr: &Value, src_ptr: &Value) {
        // i686: va_list is just a pointer, so copy the pointer value
        if let (Some(src_slot), Some(dest_slot)) = (
            self.state.get_slot(src_ptr.0),
            self.state.get_slot(dest_ptr.0),
        ) {
            emit!(self.state, "    movl {}(%ebp), %eax", src_slot.0);
            emit!(self.state, "    movl %eax, {}(%ebp)", dest_slot.0);
        }
    }

    // --- Atomics ---

    fn emit_atomic_rmw(&mut self, dest: &Value, op: AtomicRmwOp, ptr: &Operand, val: &Operand,
                       ty: IrType, _ordering: AtomicOrdering) {
        self.operand_to_eax(val);
        self.state.emit("    movl %eax, %edx"); // save value
        self.operand_to_eax(ptr);
        self.state.emit("    movl %eax, %ecx"); // ptr in ecx

        match op {
            AtomicRmwOp::Xchg => {
                let suffix = self.type_suffix(ty);
                let reg = self.eax_for_type(ty);
                self.state.emit("    movl %edx, %eax");
                emit!(self.state, "    xchg{} {}, (%ecx)", suffix, reg);
            }
            AtomicRmwOp::TestAndSet => {
                // test_and_set sets byte to 1, returns old value
                self.state.emit("    movb $1, %al");
                self.state.emit("    xchgb %al, (%ecx)");
            }
            AtomicRmwOp::Add => {
                let suffix = self.type_suffix(ty);
                let reg = match ty {
                    IrType::I8 | IrType::U8 => "%dl",
                    IrType::I16 | IrType::U16 => "%dx",
                    _ => "%edx",
                };
                emit!(self.state, "    lock xadd{} {}, (%ecx)", suffix, reg);
                self.state.emit("    movl %edx, %eax");
            }
            _ => {
                // For other ops (Sub, And, Or, Xor, Nand), use a cmpxchg loop.
                // Register usage:
                //   ecx = pointer to atomic variable
                //   eax = old/expected value (cmpxchg reads/writes this)
                //   edx = new/desired value (cmpxchg stores this on success)
                //   (%esp) = saved operand value (preserved across loop iterations)
                let suffix = self.type_suffix(ty);
                let edx_reg = match ty {
                    IrType::I8 | IrType::U8 => "%dl",
                    IrType::I16 | IrType::U16 => "%dx",
                    _ => "%edx",
                };
                // Save the operand value on the stack so it survives the loop
                self.state.emit("    pushl %edx");
                // Load current value from the atomic variable
                let load_instr = self.mov_load_for_type(ty);
                emit!(self.state, "    {} (%ecx), %eax", load_instr);
                let loop_label = format!(".Latomic_{}", self.state.next_label_id());
                emit!(self.state, "{}:", loop_label);
                // edx = old value (copy eax), then apply operation: edx = op(old, val)
                self.state.emit("    movl %eax, %edx");
                match op {
                    AtomicRmwOp::Sub => {
                        emit!(self.state, "    sub{} (%esp), {}", suffix, edx_reg);
                    }
                    AtomicRmwOp::And => {
                        emit!(self.state, "    and{} (%esp), {}", suffix, edx_reg);
                    }
                    AtomicRmwOp::Or => {
                        emit!(self.state, "    or{} (%esp), {}", suffix, edx_reg);
                    }
                    AtomicRmwOp::Xor => {
                        emit!(self.state, "    xor{} (%esp), {}", suffix, edx_reg);
                    }
                    AtomicRmwOp::Nand => {
                        emit!(self.state, "    and{} (%esp), {}", suffix, edx_reg);
                        emit!(self.state, "    not{} {}", suffix, edx_reg);
                    }
                    _ => {}
                }
                // cmpxchg: if [ecx] == eax (old), store edx (new) to [ecx]
                // otherwise, load [ecx] into eax and retry
                emit!(self.state, "    lock cmpxchg{} {}, (%ecx)", suffix, edx_reg);
                emit!(self.state, "    jne {}", loop_label);
                // Restore stack (pop the saved operand)
                self.state.emit("    addl $4, %esp");
            }
        }
        self.state.reg_cache.invalidate_acc();
        self.store_eax_to(dest);
    }

    fn emit_atomic_cmpxchg(&mut self, dest: &Value, ptr: &Operand, expected: &Operand,
                           desired: &Operand, ty: IrType, _success: AtomicOrdering,
                           _failure: AtomicOrdering, returns_bool: bool) {
        self.operand_to_eax(expected);
        self.state.emit("    movl %eax, %edx"); // save expected
        self.operand_to_eax(desired);
        self.state.emit("    pushl %eax"); // save desired on stack
        self.operand_to_eax(ptr);
        self.state.emit("    movl %eax, %ecx"); // ptr in ecx
        self.state.emit("    movl %edx, %eax"); // expected in eax
        self.state.emit("    popl %edx");        // desired in edx
        let suffix = self.type_suffix(ty);
        let reg = match ty {
            IrType::I8 | IrType::U8 => "%dl",
            IrType::I16 | IrType::U16 => "%dx",
            _ => "%edx",
        };
        emit!(self.state, "    lock cmpxchg{} {}, (%ecx)", suffix, reg);
        if returns_bool {
            self.state.emit("    sete %al");
            self.state.emit("    movzbl %al, %eax");
        }
        self.state.reg_cache.invalidate_acc();
        self.store_eax_to(dest);
    }

    fn emit_atomic_load(&mut self, dest: &Value, ptr: &Operand, ty: IrType, _ordering: AtomicOrdering) {
        self.operand_to_eax(ptr);
        let load_instr = self.mov_load_for_type(ty);
        emit!(self.state, "    {} (%eax), %eax", load_instr);
        self.state.reg_cache.invalidate_acc();
        self.store_eax_to(dest);
    }

    fn emit_atomic_store(&mut self, ptr: &Operand, val: &Operand, ty: IrType, _ordering: AtomicOrdering) {
        self.operand_to_eax(val);
        self.state.emit("    movl %eax, %edx"); // save value
        self.operand_to_eax(ptr);
        self.state.emit("    movl %eax, %ecx"); // ptr in ecx
        let store_instr = self.mov_store_for_type(ty);
        let reg = match ty {
            IrType::I8 | IrType::U8 => "%dl",
            IrType::I16 | IrType::U16 => "%dx",
            _ => "%edx",
        };
        emit!(self.state, "    {} {}, (%ecx)", store_instr, reg);
    }

    fn emit_fence(&mut self, ordering: AtomicOrdering) {
        match ordering {
            AtomicOrdering::SeqCst => self.state.emit("    mfence"),
            _ => self.state.emit("    mfence"), // conservative
        }
    }

    // --- Inline asm ---

    fn emit_inline_asm(&mut self, template: &str, _outputs: &[(String, Value, Option<String>)],
                       _inputs: &[(String, Operand, Option<String>)], _clobbers: &[String],
                       _operand_types: &[IrType], _goto_labels: &[(String, BlockId)],
                       _input_symbols: &[Option<String>]) {
        // Simplified inline asm emission for i686
        // TODO: proper inline asm support
        self.state.emit_fmt(format_args!("    {}", template));
    }

    // --- 128-bit operations ---
    // On i686, i128 operations use 4 x 32-bit registers/slots.
    // For now, we implement minimal stubs using eax:edx for the low 64 bits.

    fn emit_sign_extend_acc_high(&mut self) {
        // Sign-extend eax into edx (32->64 bit)
        self.state.emit("    cltd");
    }

    fn emit_zero_acc_high(&mut self) {
        self.state.emit("    xorl %edx, %edx");
    }

    fn emit_load_acc_pair(&mut self, op: &Operand) {
        match op {
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    emit!(self.state, "    movl {}(%ebp), %eax", slot.0);
                    emit!(self.state, "    movl {}(%ebp), %edx", slot.0 + 4);
                } else if let Some(phys) = self.reg_assignments.get(&v.0).copied() {
                    // Register-allocated value: only 32-bit register available.
                    // This should only happen for I32-sized values in i128 context;
                    // I64 values should be excluded from register allocation on i686.
                    let reg = phys_reg_name(phys);
                    emit!(self.state, "    movl %{}, %eax", reg);
                    self.state.emit("    xorl %edx, %edx");
                }
            }
            Operand::Const(IrConst::I128(v)) => {
                let low = (*v & 0xFFFFFFFF) as i32;
                let high = ((*v >> 32) & 0xFFFFFFFF) as i32;
                emit!(self.state, "    movl ${}, %eax", low);
                emit!(self.state, "    movl ${}, %edx", high);
            }
            Operand::Const(IrConst::I64(v)) => {
                let low = (*v & 0xFFFFFFFF) as i32;
                let high = ((*v >> 32) & 0xFFFFFFFF) as i32;
                emit!(self.state, "    movl ${}, %eax", low);
                emit!(self.state, "    movl ${}, %edx", high);
            }
            Operand::Const(IrConst::F64(f)) => {
                let bits = f.to_bits();
                let low = (bits & 0xFFFFFFFF) as i32;
                let high = (bits >> 32) as i32;
                emit!(self.state, "    movl ${}, %eax", low);
                emit!(self.state, "    movl ${}, %edx", high);
            }
            Operand::Const(IrConst::Zero) => {
                self.state.emit("    xorl %eax, %eax");
                self.state.emit("    xorl %edx, %edx");
            }
            _ => {
                self.operand_to_eax(op);
                self.state.emit("    xorl %edx, %edx");
            }
        }
    }

    fn emit_store_acc_pair(&mut self, dest: &Value) {
        if let Some(slot) = self.state.get_slot(dest.0) {
            emit!(self.state, "    movl %eax, {}(%ebp)", slot.0);
            emit!(self.state, "    movl %edx, {}(%ebp)", slot.0 + 4);
        }
    }

    fn emit_store_pair_to_slot(&mut self, slot: StackSlot) {
        emit!(self.state, "    movl %eax, {}(%ebp)", slot.0);
        emit!(self.state, "    movl %edx, {}(%ebp)", slot.0 + 4);
    }

    fn emit_load_pair_from_slot(&mut self, slot: StackSlot) {
        emit!(self.state, "    movl {}(%ebp), %eax", slot.0);
        emit!(self.state, "    movl {}(%ebp), %edx", slot.0 + 4);
    }

    fn emit_save_acc_pair(&mut self) {
        self.state.emit("    movl %eax, %esi");
        self.state.emit("    movl %edx, %edi");
    }

    fn emit_store_pair_indirect(&mut self) {
        self.state.emit("    movl %esi, (%ecx)");
        self.state.emit("    movl %edi, 4(%ecx)");
    }

    fn emit_load_pair_indirect(&mut self) {
        self.state.emit("    movl (%ecx), %eax");
        self.state.emit("    movl 4(%ecx), %edx");
    }

    fn emit_i128_neg(&mut self) {
        self.state.emit("    notl %eax");
        self.state.emit("    notl %edx");
        self.state.emit("    addl $1, %eax");
        self.state.emit("    adcl $0, %edx");
    }

    fn emit_i128_not(&mut self) {
        self.state.emit("    notl %eax");
        self.state.emit("    notl %edx");
    }

    fn emit_i128_to_float_call(&mut self, src: &Operand, _from_signed: bool, to_ty: IrType) {
        // TODO: proper i128->float via compiler-rt
        self.emit_load_acc_pair(src);
        // Fallback: just convert low 32 bits
        if to_ty == IrType::F32 {
            self.state.emit("    cvtsi2ssl %eax, %xmm0");
            self.state.emit("    movd %xmm0, %eax");
        } else {
            self.state.emit("    pushl %eax");
            self.state.emit("    fildl (%esp)");
            self.state.emit("    fstpl (%esp)");
            self.state.emit("    movl (%esp), %eax");
            self.state.emit("    addl $4, %esp");
        }
    }

    fn emit_float_to_i128_call(&mut self, src: &Operand, _to_signed: bool, from_ty: IrType) {
        // TODO: proper float->i128 via compiler-rt
        self.operand_to_eax(src);
        if from_ty == IrType::F32 {
            self.state.emit("    movd %eax, %xmm0");
            self.state.emit("    cvttss2si %xmm0, %eax");
        }
        self.state.emit("    xorl %edx, %edx");
    }

    fn emit_i128_prep_binop(&mut self, lhs: &Operand, rhs: &Operand) {
        // Load lhs into eax:edx, rhs onto stack
        self.emit_load_acc_pair(rhs);
        self.state.emit("    pushl %edx");
        self.state.emit("    pushl %eax");
        self.emit_load_acc_pair(lhs);
    }

    fn emit_i128_add(&mut self) {
        self.state.emit("    addl (%esp), %eax");
        self.state.emit("    adcl 4(%esp), %edx");
        self.state.emit("    addl $8, %esp");
    }

    fn emit_i128_sub(&mut self) {
        self.state.emit("    subl (%esp), %eax");
        self.state.emit("    sbbl 4(%esp), %edx");
        self.state.emit("    addl $8, %esp");
    }

    fn emit_i128_mul(&mut self) {
        // TODO: proper 64-bit multiply
        // For now, just multiply low 32 bits
        self.state.emit("    imull (%esp), %eax");
        self.state.emit("    addl $8, %esp");
    }

    fn emit_i128_and(&mut self) {
        self.state.emit("    andl (%esp), %eax");
        self.state.emit("    andl 4(%esp), %edx");
        self.state.emit("    addl $8, %esp");
    }

    fn emit_i128_or(&mut self) {
        self.state.emit("    orl (%esp), %eax");
        self.state.emit("    orl 4(%esp), %edx");
        self.state.emit("    addl $8, %esp");
    }

    fn emit_i128_xor(&mut self) {
        self.state.emit("    xorl (%esp), %eax");
        self.state.emit("    xorl 4(%esp), %edx");
        self.state.emit("    addl $8, %esp");
    }

    fn emit_i128_shl(&mut self) {
        // Shift eax:edx left by (%esp) amount
        // TODO: proper 64-bit shift
        self.state.emit("    movl (%esp), %ecx");
        self.state.emit("    shldl %cl, %eax, %edx");
        self.state.emit("    shll %cl, %eax");
        self.state.emit("    addl $8, %esp");
    }

    fn emit_i128_lshr(&mut self) {
        self.state.emit("    movl (%esp), %ecx");
        self.state.emit("    shrdl %cl, %edx, %eax");
        self.state.emit("    shrl %cl, %edx");
        self.state.emit("    addl $8, %esp");
    }

    fn emit_i128_ashr(&mut self) {
        self.state.emit("    movl (%esp), %ecx");
        self.state.emit("    shrdl %cl, %edx, %eax");
        self.state.emit("    sarl %cl, %edx");
        self.state.emit("    addl $8, %esp");
    }

    fn emit_i128_divrem_call(&mut self, _func_name: &str, lhs: &Operand, _rhs: &Operand) {
        // TODO: Call compiler-rt for 128-bit div/rem
        self.emit_load_acc_pair(lhs);
    }

    fn emit_i128_store_result(&mut self, dest: &Value) {
        self.emit_store_acc_pair(dest);
    }

    fn emit_i128_shl_const(&mut self, amount: u32) {
        if amount == 0 { return; }
        if amount >= 32 {
            self.state.emit("    movl %eax, %edx");
            self.state.emit("    xorl %eax, %eax");
            if amount > 32 && amount < 64 {
                emit!(self.state, "    shll ${}, %edx", amount - 32);
            }
        } else {
            emit!(self.state, "    shldl ${}, %eax, %edx", amount);
            emit!(self.state, "    shll ${}, %eax", amount);
        }
    }

    fn emit_i128_lshr_const(&mut self, amount: u32) {
        if amount == 0 { return; }
        if amount >= 32 {
            self.state.emit("    movl %edx, %eax");
            self.state.emit("    xorl %edx, %edx");
            if amount > 32 && amount < 64 {
                emit!(self.state, "    shrl ${}, %eax", amount - 32);
            }
        } else {
            emit!(self.state, "    shrdl ${}, %edx, %eax", amount);
            emit!(self.state, "    shrl ${}, %edx", amount);
        }
    }

    fn emit_i128_ashr_const(&mut self, amount: u32) {
        if amount == 0 { return; }
        if amount >= 32 {
            self.state.emit("    movl %edx, %eax");
            self.state.emit("    sarl $31, %edx");
            if amount > 32 && amount < 64 {
                emit!(self.state, "    sarl ${}, %eax", amount - 32);
            }
        } else {
            emit!(self.state, "    shrdl ${}, %edx, %eax", amount);
            emit!(self.state, "    sarl ${}, %edx", amount);
        }
    }

    fn emit_i128_cmp_eq(&mut self, is_ne: bool) {
        // Compare eax:edx (lhs) with (%esp) (rhs)
        self.state.emit("    cmpl (%esp), %eax");
        self.state.emit("    sete %al");
        self.state.emit("    cmpl 4(%esp), %edx");
        self.state.emit("    sete %cl");
        if is_ne {
            self.state.emit("    orb %cl, %al");
            self.state.emit("    xorb $1, %al");
        } else {
            self.state.emit("    andb %cl, %al");
        }
        self.state.emit("    movzbl %al, %eax");
        self.state.emit("    addl $8, %esp");
    }

    fn emit_i128_cmp_ordered(&mut self, op: IrCmpOp) {
        // Compare 64-bit values: high 32 bits first, then low
        let label_id = self.state.next_label_id();
        let high_decided = format!(".Li128_high_{}", label_id);

        // Compare high 32 bits first
        self.state.emit("    cmpl 4(%esp), %edx");
        emit!(self.state, "    jne {}", high_decided);
        // High equal, compare low
        self.state.emit("    cmpl (%esp), %eax");
        emit!(self.state, "{}:", high_decided);

        let set_instr = match op {
            IrCmpOp::Slt => "setl",
            IrCmpOp::Sle => "setle",
            IrCmpOp::Sgt => "setg",
            IrCmpOp::Sge => "setge",
            IrCmpOp::Ult => "setb",
            IrCmpOp::Ule => "setbe",
            IrCmpOp::Ugt => "seta",
            IrCmpOp::Uge => "setae",
            _ => "sete",
        };
        emit!(self.state, "    {} %al", set_instr);
        self.state.emit("    movzbl %al, %eax");
        self.state.emit("    addl $8, %esp");
    }

    fn emit_i128_cmp_store_result(&mut self, dest: &Value) {
        self.state.reg_cache.invalidate_acc();
        self.store_eax_to(dest);
    }

    // --- Second return value (F64/F32/F128) ---

    fn emit_get_return_f64_second(&mut self, dest: &Value) {
        // TODO: handle second F64 return from x87
        self.store_eax_to(dest);
    }

    fn emit_set_return_f64_second(&mut self, src: &Operand) {
        // TODO: set second F64 return
        self.operand_to_eax(src);
    }

    fn emit_get_return_f32_second(&mut self, dest: &Value) {
        self.store_eax_to(dest);
    }

    fn emit_set_return_f32_second(&mut self, src: &Operand) {
        self.operand_to_eax(src);
    }

    fn emit_get_return_f128_second(&mut self, dest: &Value) {
        // x87 st(0) has the second long double
        if let Some(slot) = self.state.get_slot(dest.0) {
            emit!(self.state, "    fstpt {}(%ebp)", slot.0);
        }
    }

    fn emit_set_return_f128_second(&mut self, src: &Operand) {
        self.emit_f128_load_to_x87(src);
    }

    // --- Segment overrides (x86-specific) ---

    fn emit_seg_load(&mut self, dest: &Value, ptr: &Value, ty: IrType, seg: AddressSpace) {
        self.operand_to_eax(&Operand::Value(*ptr));
        self.state.emit("    movl %eax, %ecx");
        let seg_prefix = match seg {
            AddressSpace::SegGs => "%gs:",
            AddressSpace::SegFs => "%fs:",
            _ => "",
        };
        let load_instr = self.mov_load_for_type(ty);
        emit!(self.state, "    {} {}(%ecx), %eax", load_instr, seg_prefix);
        self.state.reg_cache.invalidate_acc();
        self.store_eax_to(dest);
    }

    fn emit_seg_store(&mut self, val: &Operand, ptr: &Value, ty: IrType, seg: AddressSpace) {
        self.operand_to_eax(val);
        self.state.emit("    movl %eax, %edx");
        self.operand_to_eax(&Operand::Value(*ptr));
        self.state.emit("    movl %eax, %ecx");
        let seg_prefix = match seg {
            AddressSpace::SegGs => "%gs:",
            AddressSpace::SegFs => "%fs:",
            _ => "",
        };
        let store_instr = self.mov_store_for_type(ty);
        let reg = match ty {
            IrType::I8 | IrType::U8 => "%dl",
            IrType::I16 | IrType::U16 => "%dx",
            _ => "%edx",
        };
        emit!(self.state, "    {} {}, {}(%ecx)", store_instr, reg, seg_prefix);
    }
}
