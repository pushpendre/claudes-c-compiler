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
    /// Scratch register allocation index for inline asm GP registers.
    pub(super) asm_scratch_idx: usize,
    /// Scratch register allocation index for inline asm XMM registers.
    pub(super) asm_xmm_scratch_idx: usize,
    /// Whether the current function uses the fastcall calling convention.
    /// When true, the first two DWORD integer/pointer args are passed in ecx/edx.
    is_fastcall: bool,
    /// For fastcall functions, the number of bytes of stack args the callee must pop on return.
    fastcall_stack_cleanup: usize,
    /// For fastcall functions, how many leading params are passed in registers (0, 1, or 2).
    fastcall_reg_param_count: usize,
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
            asm_scratch_idx: 0,
            asm_xmm_scratch_idx: 0,
            is_fastcall: false,
            fastcall_stack_cleanup: 0,
            fastcall_reg_param_count: 0,
        }
    }

    pub fn set_pic(&mut self, pic: bool) {
        self.state.pic_mode = pic;
    }

    pub fn set_no_jump_tables(&mut self, enabled: bool) {
        self.state.no_jump_tables = enabled;
    }

    /// Apply all relevant options from a `CodegenOptions` struct.
    pub fn apply_options(&mut self, opts: &crate::backend::CodegenOptions) {
        self.set_pic(opts.pic);
        self.set_no_jump_tables(opts.no_jump_tables);
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

    /// Load the address of va_list storage into %edx.
    ///
    /// va_list_ptr is an IR value that holds a pointer to the va_list storage.
    /// - If va_list_ptr is an alloca (local va_list variable), we LEA the slot
    ///   address into %edx (the alloca IS the va_list storage).
    /// - If va_list_ptr is a regular value (e.g., loaded pointer from va_list*),
    ///   we load its value into %edx (the value IS the address of va_list storage).
    fn load_va_list_addr_to_edx(&mut self, va_list_ptr: &Value) {
        let is_alloca = self.state.is_alloca(va_list_ptr.0);
        if let Some(phys) = self.reg_assignments.get(&va_list_ptr.0).copied() {
            // Value is in a callee-saved register (non-alloca pointer value)
            let reg = phys_reg_name(phys);
            emit!(self.state, "    movl %{}, %edx", reg);
        } else if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
            if is_alloca {
                // Alloca: the slot IS the va_list; get the address of the slot
                emit!(self.state, "    leal {}(%ebp), %edx", slot.0);
            } else {
                // Regular value: the slot holds a pointer to the va_list storage
                emit!(self.state, "    movl {}(%ebp), %edx", slot.0);
            }
        }
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

    /// Load a 64-bit value's slot into %eax by OR'ing both 32-bit halves.
    /// Used for truthiness testing of I64/U64/F64 values on i686, where a value
    /// is nonzero iff either half is nonzero.
    fn emit_wide_value_to_eax_ored(&mut self, value_id: u32) {
        if let Some(slot) = self.state.get_slot(value_id) {
            emit!(self.state, "    movl {}(%ebp), %eax", slot.0);
            emit!(self.state, "    orl {}(%ebp), %eax", slot.0 + 4);
        } else {
            // Wide values (I64/F64) on i686 should always have stack slots since
            // they can't fit in a single 32-bit register. Fall back to loading
            // the low 32 bits only as a last resort.
            self.operand_to_eax(&Operand::Value(Value(value_id)));
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
            // If this dest is a wide value (I64/U64/F64), zero the upper 4 bytes.
            // Wide values occupy 8-byte slots, and other paths (e.g. Copy from
            // IrConst::I64) may write all 8 bytes. If we only write the low 4,
            // the upper half retains stack garbage, which corrupts truthiness
            // checks that OR both halves (emit_wide_value_to_eax_ored).
            if self.state.wide_values.contains(&dest.0) {
                emit!(self.state, "    movl $0, {}(%ebp)", slot.0 + 4);
            }
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

    /// Return the register name for ecx sub-register based on type size.
    fn ecx_for_type(&self, ty: IrType) -> &'static str {
        match ty {
            IrType::I8 | IrType::U8 => "%cl",
            IrType::I16 | IrType::U16 => "%cx",
            _ => "%ecx",
        }
    }

    /// Return the register name for edx sub-register based on type size.
    fn edx_for_type(&self, ty: IrType) -> &'static str {
        match ty {
            IrType::I8 | IrType::U8 => "%dl",
            IrType::I16 | IrType::U16 => "%dx",
            _ => "%edx",
        }
    }

    /// Check if a param type is eligible for fastcall register passing.
    /// Only DWORD-sized or smaller integer/pointer types qualify.
    fn is_fastcall_reg_eligible(&self, ty: IrType) -> bool {
        matches!(ty, IrType::I8 | IrType::U8 | IrType::I16 | IrType::U16 |
                     IrType::I32 | IrType::U32 | IrType::Ptr)
    }

    /// Count how many leading params are passed in registers for fastcall (max 2).
    fn count_fastcall_reg_params(&self, func: &IrFunction) -> usize {
        let mut count = 0;
        for param in &func.params {
            if count >= 2 { break; }
            let ty = param.ty;
            if self.is_fastcall_reg_eligible(ty) {
                count += 1;
            } else {
                break; // non-eligible param stops register assignment
            }
        }
        count
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

    /// Extract an immediate integer value from an operand.
    /// Used for SSE/AES instructions that require compile-time immediate operands.
    fn operand_to_imm_i64(op: &Operand) -> i64 {
        match op {
            Operand::Const(c) => match c {
                IrConst::I8(v) => *v as i64,
                IrConst::I16(v) => *v as i64,
                IrConst::I32(v) => *v as i64,
                IrConst::I64(v) => *v,
                _ => 0,
            },
            Operand::Value(_) => 0,
        }
    }

    /// Emit a binary SSE 128-bit operation: load two 128-bit operands from
    /// pointers, apply the operation, and store the result to dest_ptr.
    fn emit_sse_binary_128(&mut self, dptr: &Value, args: &[Operand], sse_inst: &str) {
        // Load first 128-bit operand pointer into eax, then load xmm0
        self.operand_to_eax(&args[0]);
        self.state.emit("    movdqu (%eax), %xmm0");
        // Load second 128-bit operand pointer into eax, then load xmm1
        self.operand_to_eax(&args[1]);
        self.state.emit("    movdqu (%eax), %xmm1");
        // Apply the binary SSE operation
        self.state.emit_fmt(format_args!("    {} %xmm1, %xmm0", sse_inst));
        // Store result to dest_ptr
        self.operand_to_eax(&Operand::Value(*dptr));
        self.state.emit("    movdqu %xmm0, (%eax)");
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
                // Convert f128 (IEEE binary128) bytes to x87 80-bit format for fldt
                let x87 = crate::common::long_double::f128_bytes_to_x87_bytes(bytes);
                let dword0 = i32::from_le_bytes([x87[0], x87[1], x87[2], x87[3]]);
                let dword1 = i32::from_le_bytes([x87[4], x87[5], x87[6], x87[7]]);
                let word2 = i16::from_le_bytes([x87[8], x87[9]]) as i32;
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

    // --- 64-bit atomic helpers using cmpxchg8b (for I64/U64/F64 on i686) ---

    /// Check if a type requires 64-bit atomic handling on i686 (needs cmpxchg8b).
    fn is_atomic_wide(&self, ty: IrType) -> bool {
        matches!(ty, IrType::I64 | IrType::U64 | IrType::F64)
    }

    /// 64-bit atomic RMW using lock cmpxchg8b loop.
    ///
    /// cmpxchg8b compares edx:eax with 8 bytes at memory location.
    /// If equal, stores ecx:ebx to memory. If not, loads memory into edx:eax.
    /// We use a loop: load old value, compute new value, try cmpxchg8b.
    ///
    /// Register plan:
    ///   esi = pointer to atomic variable (saved/restored)
    ///   edx:eax = old (expected) value
    ///   ecx:ebx = new (desired) value
    ///   Stack: saved operand value (8 bytes)
    fn emit_atomic_rmw_wide(&mut self, dest: &Value, op: AtomicRmwOp, ptr: &Operand,
                            val: &Operand) {
        // Save callee-saved registers we need to clobber
        self.state.emit("    pushl %ebx");
        self.state.emit("    pushl %esi");

        // Load pointer into esi
        self.operand_to_eax(ptr);
        self.state.emit("    movl %eax, %esi");

        // Load 64-bit operand value onto stack (8 bytes)
        self.emit_load_acc_pair(val);
        self.state.emit("    pushl %edx");  // high word at 4(%esp)
        self.state.emit("    pushl %eax");  // low word at (%esp)

        // Load current value from memory into edx:eax
        self.state.emit("    movl (%esi), %eax");
        self.state.emit("    movl 4(%esi), %edx");

        match op {
            AtomicRmwOp::Xchg => {
                // For exchange, the desired value is the operand (constant across retries)
                self.state.emit("    movl (%esp), %ebx");
                self.state.emit("    movl 4(%esp), %ecx");
                let loop_label = format!(".Latomic_{}", self.state.next_label_id());
                emit!(self.state, "{}:", loop_label);
                self.state.emit("    lock cmpxchg8b (%esi)");
                emit!(self.state, "    jne {}", loop_label);
            }
            AtomicRmwOp::Add => {
                let loop_label = format!(".Latomic_{}", self.state.next_label_id());
                emit!(self.state, "{}:", loop_label);
                self.state.emit("    movl %eax, %ebx");
                self.state.emit("    movl %edx, %ecx");
                self.state.emit("    addl (%esp), %ebx");
                self.state.emit("    adcl 4(%esp), %ecx");
                self.state.emit("    lock cmpxchg8b (%esi)");
                emit!(self.state, "    jne {}", loop_label);
            }
            AtomicRmwOp::Sub => {
                let loop_label = format!(".Latomic_{}", self.state.next_label_id());
                emit!(self.state, "{}:", loop_label);
                self.state.emit("    movl %eax, %ebx");
                self.state.emit("    movl %edx, %ecx");
                self.state.emit("    subl (%esp), %ebx");
                self.state.emit("    sbbl 4(%esp), %ecx");
                self.state.emit("    lock cmpxchg8b (%esi)");
                emit!(self.state, "    jne {}", loop_label);
            }
            AtomicRmwOp::And => {
                let loop_label = format!(".Latomic_{}", self.state.next_label_id());
                emit!(self.state, "{}:", loop_label);
                self.state.emit("    movl %eax, %ebx");
                self.state.emit("    movl %edx, %ecx");
                self.state.emit("    andl (%esp), %ebx");
                self.state.emit("    andl 4(%esp), %ecx");
                self.state.emit("    lock cmpxchg8b (%esi)");
                emit!(self.state, "    jne {}", loop_label);
            }
            AtomicRmwOp::Or => {
                let loop_label = format!(".Latomic_{}", self.state.next_label_id());
                emit!(self.state, "{}:", loop_label);
                self.state.emit("    movl %eax, %ebx");
                self.state.emit("    movl %edx, %ecx");
                self.state.emit("    orl (%esp), %ebx");
                self.state.emit("    orl 4(%esp), %ecx");
                self.state.emit("    lock cmpxchg8b (%esi)");
                emit!(self.state, "    jne {}", loop_label);
            }
            AtomicRmwOp::Xor => {
                let loop_label = format!(".Latomic_{}", self.state.next_label_id());
                emit!(self.state, "{}:", loop_label);
                self.state.emit("    movl %eax, %ebx");
                self.state.emit("    movl %edx, %ecx");
                self.state.emit("    xorl (%esp), %ebx");
                self.state.emit("    xorl 4(%esp), %ecx");
                self.state.emit("    lock cmpxchg8b (%esi)");
                emit!(self.state, "    jne {}", loop_label);
            }
            AtomicRmwOp::Nand => {
                let loop_label = format!(".Latomic_{}", self.state.next_label_id());
                emit!(self.state, "{}:", loop_label);
                self.state.emit("    movl %eax, %ebx");
                self.state.emit("    movl %edx, %ecx");
                self.state.emit("    andl (%esp), %ebx");
                self.state.emit("    andl 4(%esp), %ecx");
                self.state.emit("    notl %ebx");
                self.state.emit("    notl %ecx");
                self.state.emit("    lock cmpxchg8b (%esi)");
                emit!(self.state, "    jne {}", loop_label);
            }
            AtomicRmwOp::TestAndSet => {
                // For 64-bit test-and-set, set the low byte to 1, rest to 0
                self.state.emit("    movl $1, %ebx");
                self.state.emit("    xorl %ecx, %ecx");
                let loop_label = format!(".Latomic_{}", self.state.next_label_id());
                emit!(self.state, "{}:", loop_label);
                self.state.emit("    lock cmpxchg8b (%esi)");
                emit!(self.state, "    jne {}", loop_label);
            }
        }

        // Clean up stack (remove 8-byte operand value)
        self.state.emit("    addl $8, %esp");
        // Restore callee-saved registers
        self.state.emit("    popl %esi");
        self.state.emit("    popl %ebx");

        // Result (old value) is in edx:eax — store to dest's 64-bit stack slot
        self.state.reg_cache.invalidate_acc();
        self.emit_store_acc_pair(dest);
    }

    /// 64-bit atomic compare-exchange using lock cmpxchg8b.
    ///
    /// cmpxchg8b: compares edx:eax with 8 bytes at memory.
    /// If equal, stores ecx:ebx to memory and sets ZF.
    /// If not equal, loads memory into edx:eax and clears ZF.
    fn emit_atomic_cmpxchg_wide(&mut self, dest: &Value, ptr: &Operand, expected: &Operand,
                                desired: &Operand, returns_bool: bool) {
        // Save callee-saved registers
        self.state.emit("    pushl %ebx");
        self.state.emit("    pushl %esi");

        // Load pointer into esi
        self.operand_to_eax(ptr);
        self.state.emit("    movl %eax, %esi");

        // Load expected into edx:eax, save on stack temporarily
        self.emit_load_acc_pair(expected);
        self.state.emit("    pushl %edx");
        self.state.emit("    pushl %eax");

        // Load desired into ecx:ebx
        self.emit_load_acc_pair(desired);
        self.state.emit("    movl %eax, %ebx");
        self.state.emit("    movl %edx, %ecx");

        // Restore expected into edx:eax
        self.state.emit("    popl %eax");
        self.state.emit("    popl %edx");

        // Execute cmpxchg8b
        self.state.emit("    lock cmpxchg8b (%esi)");

        if returns_bool {
            self.state.emit("    sete %al");
            self.state.emit("    movzbl %al, %eax");
            // Restore callee-saved registers
            self.state.emit("    popl %esi");
            self.state.emit("    popl %ebx");
            self.state.reg_cache.invalidate_acc();
            self.store_eax_to(dest);
        } else {
            // Result (old value) is in edx:eax
            // Restore callee-saved registers
            self.state.emit("    popl %esi");
            self.state.emit("    popl %ebx");
            self.state.reg_cache.invalidate_acc();
            self.emit_store_acc_pair(dest);
        }
    }

    /// 64-bit atomic load using cmpxchg8b with expected == desired == 0.
    ///
    /// cmpxchg8b always loads the current memory value into edx:eax on failure,
    /// so we set edx:eax = ecx:ebx = 0 and execute cmpxchg8b. If the memory
    /// happens to be 0, the exchange writes 0 (no change). If non-zero,
    /// we get the current value in edx:eax without modifying memory.
    fn emit_atomic_load_wide(&mut self, dest: &Value, ptr: &Operand) {
        self.state.emit("    pushl %ebx");
        self.state.emit("    pushl %esi");

        self.operand_to_eax(ptr);
        self.state.emit("    movl %eax, %esi");

        // Set all registers to zero: edx:eax = ecx:ebx = 0
        self.state.emit("    xorl %eax, %eax");
        self.state.emit("    xorl %edx, %edx");
        self.state.emit("    xorl %ebx, %ebx");
        self.state.emit("    xorl %ecx, %ecx");
        // lock cmpxchg8b: if (%esi) == 0 -> store 0 (no change), else load into edx:eax
        self.state.emit("    lock cmpxchg8b (%esi)");

        self.state.emit("    popl %esi");
        self.state.emit("    popl %ebx");

        self.state.reg_cache.invalidate_acc();
        self.emit_store_acc_pair(dest);
    }

    /// 64-bit atomic store using a cmpxchg8b loop.
    ///
    /// There is no single instruction for atomic 64-bit stores on i686, so we
    /// use a cmpxchg8b loop: read current value, try to replace with desired.
    fn emit_atomic_store_wide(&mut self, ptr: &Operand, val: &Operand) {
        self.state.emit("    pushl %ebx");
        self.state.emit("    pushl %esi");

        // Load pointer into esi
        self.operand_to_eax(ptr);
        self.state.emit("    movl %eax, %esi");

        // Load desired value into ecx:ebx
        self.emit_load_acc_pair(val);
        self.state.emit("    movl %eax, %ebx");
        self.state.emit("    movl %edx, %ecx");

        // Load current value from memory into edx:eax (initial guess for cmpxchg8b)
        self.state.emit("    movl (%esi), %eax");
        self.state.emit("    movl 4(%esi), %edx");

        let loop_label = format!(".Latomic_{}", self.state.next_label_id());
        emit!(self.state, "{}:", loop_label);
        self.state.emit("    lock cmpxchg8b (%esi)");
        emit!(self.state, "    jne {}", loop_label);

        self.state.emit("    popl %esi");
        self.state.emit("    popl %ebx");
        self.state.reg_cache.invalidate_acc();
    }

    /// Emit comments for callee-saved registers clobbered by inline asm.
    fn emit_callee_saved_clobber_annotations(&mut self, clobbers: &[String]) {
        for clobber in clobbers {
            let reg_name = match clobber.as_str() {
                "ebx" | "bx" | "bl" | "bh" => Some("%ebx"),
                "esi" | "si" => Some("%esi"),
                "edi" | "di" => Some("%edi"),
                _ => None,
            };
            if let Some(reg) = reg_name {
                self.state.emit_fmt(format_args!("    # asm clobber {}", reg));
            }
        }
    }

    /// Emit a fastcall function call on i686.
    /// First two DWORD (int/ptr) args go in ECX, EDX.
    /// Remaining args go on the stack (right-to-left push order).
    /// The callee pops stack args, so caller does NOT adjust ESP after call.
    fn emit_fastcall(&mut self, args: &[Operand], arg_types: &[IrType],
                     direct_name: Option<&str>, func_ptr: Option<&Operand>,
                     dest: Option<Value>, return_type: IrType) {
        let indirect = func_ptr.is_some() && direct_name.is_none();

        // Determine which args go in registers vs stack.
        let mut reg_count = 0usize;
        for ty in arg_types.iter() {
            if reg_count >= 2 { break; }
            if self.is_fastcall_reg_eligible(*ty) {
                reg_count += 1;
            } else {
                break;
            }
        }

        // Compute stack space for overflow args (args beyond the register ones).
        let mut stack_bytes = 0usize;
        for i in reg_count..args.len() {
            let ty = if i < arg_types.len() { arg_types[i] } else { IrType::I32 };
            match ty {
                IrType::F64 | IrType::I64 | IrType::U64 => stack_bytes += 8,
                IrType::F128 => stack_bytes += 12,
                _ if is_i128_type(ty) => stack_bytes += 16,
                _ => stack_bytes += 4,
            }
        }
        // Align to 16 bytes
        let stack_arg_space = (stack_bytes + 15) & !15;

        // Spill indirect function pointer before stack manipulation.
        if indirect {
            self.emit_call_spill_fptr(func_ptr.unwrap());
        }

        // Phase 1: Allocate stack space and write stack args.
        if stack_arg_space > 0 {
            emit!(self.state, "    subl ${}, %esp", stack_arg_space);
        }

        // Write stack args (skipping register args).
        let mut offset = 0i64;
        for i in reg_count..args.len() {
            let ty = if i < arg_types.len() { arg_types[i] } else { IrType::I32 };
            let arg = &args[i];

            match ty {
                IrType::I64 | IrType::U64 | IrType::F64 => {
                    self.emit_load_acc_pair(arg);
                    emit!(self.state, "    movl %eax, {}(%esp)", offset);
                    emit!(self.state, "    movl %edx, {}(%esp)", offset + 4);
                    offset += 8;
                }
                IrType::F128 => {
                    // Load F128 value to x87 and store to stack
                    self.emit_f128_load_to_x87(arg);
                    emit!(self.state, "    fstpt {}(%esp)", offset);
                    offset += 12;
                }
                _ if is_i128_type(ty) => {
                    // Copy 16 bytes
                    if let Operand::Value(v) = arg {
                        if let Some(slot) = self.state.get_slot(v.0) {
                            for j in (0..16).step_by(4) {
                                emit!(self.state, "    movl {}(%ebp), %eax", slot.0 + j as i64);
                                emit!(self.state, "    movl %eax, {}(%esp)", offset + j as i64);
                            }
                        }
                    }
                    offset += 16;
                }
                _ => {
                    self.emit_load_operand(arg);
                    emit!(self.state, "    movl %eax, {}(%esp)", offset);
                    offset += 4;
                }
            }
        }

        // Phase 2: Load register args into ECX and EDX.
        // Load EDX first (arg 1) then ECX (arg 0), because loading arg 0
        // may clobber EDX if it involves function calls.
        if reg_count >= 2 {
            self.emit_load_operand(&args[1]);
            self.state.emit("    movl %eax, %edx");
        }
        if reg_count >= 1 {
            self.emit_load_operand(&args[0]);
            self.state.emit("    movl %eax, %ecx");
        }

        // Phase 3: Emit the call.
        if indirect {
            // Reload function pointer from spill slot
            let fptr_offset = stack_arg_space as i64;
            emit!(self.state, "    movl {}(%esp), %eax", fptr_offset);
            self.state.emit("    call *%eax");
        } else if let Some(name) = direct_name {
            emit!(self.state, "    call {}", name);
        }

        // Phase 4: For indirect calls, pop the spilled function pointer.
        // Note: callee already cleaned up the stack args, so we only need
        // to handle the fptr spill and alignment padding.
        if indirect {
            self.state.emit("    addl $4, %esp"); // pop fptr spill
        }
        // Clean up alignment padding (the difference between actual stack bytes and aligned)
        let padding = stack_arg_space - stack_bytes;
        if padding > 0 {
            emit!(self.state, "    addl ${}, %esp", padding);
        }

        // Phase 5: Store return value.
        if let Some(dest) = dest {
            self.emit_call_store_result(&dest, return_type);
        }

        self.state.reg_cache.invalidate_acc();
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

// --- 64-bit bit-manipulation helpers ---
// On i686, 64-bit values are in the eax:edx register pair (eax=low, edx=high).
// The result of clz/ctz/popcount is a small integer (0-64) that fits in eax,
// so we zero edx to produce a proper I64 result.
impl I686Codegen {
    /// clzll(x): Count leading zeros of 64-bit value in eax:edx.
    /// If high half (edx) != 0, result = lzcnt(edx).
    /// Otherwise, result = 32 + lzcnt(eax).
    fn emit_i64_clz(&mut self) {
        let done = self.state.fresh_label("clz64_done");
        let hi_zero = self.state.fresh_label("clz64_hi_zero");
        // Test high half
        self.state.emit("    testl %edx, %edx");
        emit!(self.state, "    je {}", hi_zero);
        // High half is non-zero: result = lzcnt(edx)
        self.state.emit("    lzcntl %edx, %eax");
        self.state.emit("    xorl %edx, %edx");
        emit!(self.state, "    jmp {}", done);
        // High half is zero: result = 32 + lzcnt(eax)
        emit!(self.state, "{}:", hi_zero);
        self.state.emit("    lzcntl %eax, %eax");
        self.state.emit("    addl $32, %eax");
        self.state.emit("    xorl %edx, %edx");
        emit!(self.state, "{}:", done);
    }

    /// ctzll(x): Count trailing zeros of 64-bit value in eax:edx.
    /// If low half (eax) != 0, result = tzcnt(eax).
    /// Otherwise, result = 32 + tzcnt(edx).
    fn emit_i64_ctz(&mut self) {
        let done = self.state.fresh_label("ctz64_done");
        let lo_zero = self.state.fresh_label("ctz64_lo_zero");
        // Test low half
        self.state.emit("    testl %eax, %eax");
        emit!(self.state, "    je {}", lo_zero);
        // Low half is non-zero: result = tzcnt(eax)
        self.state.emit("    tzcntl %eax, %eax");
        self.state.emit("    xorl %edx, %edx");
        emit!(self.state, "    jmp {}", done);
        // Low half is zero: result = 32 + tzcnt(edx)
        emit!(self.state, "{}:", lo_zero);
        self.state.emit("    tzcntl %edx, %eax");
        self.state.emit("    addl $32, %eax");
        self.state.emit("    xorl %edx, %edx");
        emit!(self.state, "{}:", done);
    }

    /// popcountll(x): Population count of 64-bit value in eax:edx.
    /// result = popcount(eax) + popcount(edx)
    fn emit_i64_popcount(&mut self) {
        self.state.emit("    popcntl %edx, %ecx");
        self.state.emit("    popcntl %eax, %eax");
        self.state.emit("    addl %ecx, %eax");
        self.state.emit("    xorl %edx, %edx");
    }

    /// bswap64(x): Byte-swap 64-bit value in eax:edx.
    /// result_lo = bswap(high), result_hi = bswap(low)
    fn emit_i64_bswap(&mut self) {
        // eax=low, edx=high
        // bswap each half, then swap: new_eax = bswap(edx), new_edx = bswap(eax)
        self.state.emit("    bswapl %eax");
        self.state.emit("    bswapl %edx");
        self.state.emit("    xchgl %eax, %edx");
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
        self.is_fastcall = func.is_fastcall;
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
            let required = space + alloc;
            let new_space = if effective_align >= 16 {
                // On i686, %ebp mod 16 == 8 (ABI: entry %esp is 16-aligned, call
                // pushes 4-byte return addr, push %ebp pushes another 4 bytes).
                // For a variable at (%ebp - offset) to be A-byte aligned, we need:
                //   offset mod A == %ebp mod A == 8  (for A >= 16, power of 2)
                // Round up `required` to the next value satisfying offset mod A == 8.
                let bias = 8i64;
                let a = effective_align;
                // Compute smallest n >= required such that n mod a == bias
                let rem = ((required % a) + a) % a; // current remainder (always >= 0)
                let needed = if rem <= bias { bias - rem } else { a - rem + bias };
                required + needed
            } else {
                ((required + effective_align - 1) / effective_align) * effective_align
            };
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

        // For fastcall, determine how many params go in registers (max 2 DWORD int/ptr params).
        // Track which params are register-passed and the stack offset adjustment.
        let fastcall_reg_count = if self.is_fastcall {
            self.count_fastcall_reg_params(func)
        } else {
            0
        };
        self.fastcall_reg_param_count = fastcall_reg_count;

        // For fastcall, compute the total stack-passed parameter bytes for callee cleanup.
        if self.is_fastcall {
            // The classify_params assigns offsets as if all params are on stack.
            // The stack cleanup is total param stack size minus the register-passed params.
            // We can compute it from the last param's offset + size.
            let mut total_stack_bytes: usize = 0;
            for (i, _p) in func.params.iter().enumerate() {
                if i < fastcall_reg_count { continue; }
                let ty = func.params[i].ty;
                let size = match ty {
                    IrType::I64 | IrType::U64 | IrType::F64 => 8,
                    IrType::F128 => 12,
                    _ if is_i128_type(ty) => 16,
                    _ => 4,
                };
                total_stack_bytes += size;
            }
            self.fastcall_stack_cleanup = total_stack_bytes;
        } else {
            self.fastcall_stack_cleanup = 0;
        }

        // Stack-passed parameters start at 8(%ebp) (after saved ebp + return addr).
        let stack_base: i64 = 8;
        let mut fastcall_reg_idx = 0usize; // which reg param we're on (0=ecx, 1=edx)

        for (i, _param) in func.params.iter().enumerate() {
            let class = param_classes[i];

            // Find the alloca for this parameter.
            let (slot, ty, dest_id) = if let Some((dest, ty)) = find_param_alloca(func, i) {
                if let Some(slot) = self.state.get_slot(dest.0) {
                    (slot, ty, dest.0)
                } else {
                    if self.is_fastcall && fastcall_reg_idx < fastcall_reg_count && i < func.params.len() {
                        if self.is_fastcall_reg_eligible(ty) {
                            fastcall_reg_idx += 1;
                        }
                    }
                    continue;
                }
            } else {
                if self.is_fastcall && fastcall_reg_idx < fastcall_reg_count && i < func.params.len() {
                    let param_ty = func.params[i].ty;
                    if self.is_fastcall_reg_eligible(param_ty) {
                        fastcall_reg_idx += 1;
                    }
                }
                continue;
            };

            // Fastcall: first eligible params come from ecx/edx
            if self.is_fastcall && fastcall_reg_idx < fastcall_reg_count && self.is_fastcall_reg_eligible(ty) {
                let store_instr = self.mov_store_for_type(ty);
                let src_reg = if fastcall_reg_idx == 0 {
                    self.ecx_for_type(ty)
                } else {
                    self.edx_for_type(ty)
                };
                emit!(self.state, "    {} {}, {}(%ebp)", store_instr, src_reg, slot.0);
                fastcall_reg_idx += 1;
                continue;
            }

            // For fastcall stack params, adjust offsets: the stack params don't include
            // the register-passed params, so the offset from classify_params already
            // accounts for all params. We need to subtract the space that would have
            // been used by register params.
            let stack_offset_adjust = if self.is_fastcall { fastcall_reg_count as i64 * 4 } else { 0 };

            match class {
                ParamClass::StackScalar { offset } => {
                    let src_offset = stack_base + offset - stack_offset_adjust;
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
                    let src = stack_base + offset - stack_offset_adjust;
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
                    let src = stack_base + offset - stack_offset_adjust;
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
                    let src = stack_base + offset - stack_offset_adjust;
                    emit!(self.state, "    fldt {}(%ebp)", src);
                    emit!(self.state, "    fstpt {}(%ebp)", slot.0);
                    self.state.f128_direct_slots.insert(dest_id);
                }
                ParamClass::I128Stack { offset } => {
                    let src = stack_base + offset - stack_offset_adjust;
                    for j in (0..16).step_by(4) {
                        emit!(self.state, "    movl {}(%ebp), %eax", src + j as i64);
                        emit!(self.state, "    movl %eax, {}(%ebp)", slot.0 + j as i64);
                    }
                }
                ParamClass::F128Stack { offset } => {
                    let src = stack_base + offset - stack_offset_adjust;
                    emit!(self.state, "    fldt {}(%ebp)", src);
                    emit!(self.state, "    fstpt {}(%ebp)", slot.0);
                    self.state.f128_direct_slots.insert(dest_id);
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

        // For fastcall register params, the value was already stored to the param's
        // alloca slot by emit_store_params (from ecx/edx). Read from that slot.
        if self.is_fastcall && param_idx < self.fastcall_reg_param_count {
            if let Some(Some((slot, _slot_ty))) = self.state.param_alloca_slots.get(param_idx) {
                let load_instr = self.mov_load_for_type(ty);
                emit!(self.state, "    {} {}(%ebp), %eax", load_instr, slot.0);
                self.store_eax_to(dest);
            }
            return;
        }

        let stack_base: i64 = 8; // after saved ebp + return address
        let stack_offset_adjust = if self.is_fastcall { self.fastcall_reg_param_count as i64 * 4 } else { 0 };
        let param_offset = if param_idx < self.state.param_classes.len() {
            match self.state.param_classes[param_idx] {
                ParamClass::StackScalar { offset } |
                ParamClass::StructStack { offset, .. } |
                ParamClass::LargeStructStack { offset, .. } |
                ParamClass::F128AlwaysStack { offset } |
                ParamClass::I128Stack { offset } |
                ParamClass::F128Stack { offset } |
                ParamClass::LargeStructByRefStack { offset, .. } => stack_base + offset - stack_offset_adjust,
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
        } else if ty == IrType::F128 {
            // F128 (long double): load full 80-bit from stack via x87
            if let Some(dest_slot) = self.state.get_slot(dest.0) {
                emit!(self.state, "    fldt {}(%ebp)", param_offset);
                emit!(self.state, "    fstpt {}(%ebp)", dest_slot.0);
                self.state.f128_direct_slots.insert(dest.0);
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

    /// Override emit_binop to route I64/U64 through register-pair (eax:edx) arithmetic.
    /// On i686, I64/U64 are "wide" types that need the same pair-based arithmetic
    /// as I128/U128 on 64-bit targets. The emit_i128_* methods on i686 implement
    /// 64-bit operations using 32-bit register pairs.
    fn emit_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if matches!(ty, IrType::I64 | IrType::U64) {
            self.emit_i128_binop(dest, op, lhs, rhs);
            self.state.reg_cache.invalidate_all();
            return;
        }
        if crate::backend::generation::is_i128_type(ty) {
            self.emit_i128_binop(dest, op, lhs, rhs);
            return;
        }
        if ty.is_float() {
            let float_op = crate::backend::cast::classify_float_binop(op)
                .unwrap_or_else(|| panic!("unsupported float binop: {:?} on type {:?}", op, ty));
            self.emit_float_binop(dest, float_op, lhs, rhs, ty);
            return;
        }
        self.emit_int_binop(dest, op, lhs, rhs, ty);
    }

    fn emit_int_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, _ty: IrType) {
        // On i686, only I32/U32 (and smaller) BinOps reach here now.
        // I64/U64 are intercepted by emit_binop above and routed to
        // the register-pair (eax:edx) arithmetic in emit_i128_binop.

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
            // fsubrp/fdivrp: st(1) = st(1) op st(0), pop (correct order for lhs-rhs)
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
                self.state.f128_direct_slots.insert(dest.0);
            }
            self.state.reg_cache.invalidate_acc();
            return;
        }
        // F32: use SSE with standard mnemonics (sub, div, add, mul).
        // Load order: lhs→eax→ecx, rhs→eax. In emit_float_binop_impl,
        // ecx→xmm0 (lhs), eax→xmm1 (rhs), so subss %xmm1,%xmm0 = lhs-rhs.
        let mnemonic = match op {
            crate::backend::cast::FloatOp::Add => "add",
            crate::backend::cast::FloatOp::Sub => "sub",
            crate::backend::cast::FloatOp::Mul => "mul",
            crate::backend::cast::FloatOp::Div => "div",
        };
        self.emit_load_operand(lhs);
        self.emit_acc_to_secondary();
        self.emit_load_operand(rhs);
        self.emit_float_binop_impl(mnemonic, ty);
        self.emit_store_result(dest);
    }

    /// Override to return x87-appropriate mnemonics for fsubp/fdivp.
    ///
    /// x87 stack: st(0)=rhs, st(1)=lhs (loaded in that order).
    /// We want lhs OP rhs, i.e., st(1) OP st(0).
    ///
    /// GAS AT&T syntax has a historical quirk where fsubp/fdivp with two
    /// explicit operands swap the subtraction/division order:
    ///   fsubp  %st, %st(1) → st(1) = st(0) - st(1)  (reversed!)
    ///   fsubrp %st, %st(1) → st(1) = st(1) - st(0)  (the order we want)
    /// Same applies to fdivp vs fdivrp.
    /// faddp and fmulp are commutative so order doesn't matter.
    ///
    /// For F32 (SSE), the emit_float_binop_impl uses swapped xmm registers
    /// to compensate, so these mnemonics are only correct for x87 paths.
    fn emit_float_binop_mnemonic(&self, op: crate::backend::cast::FloatOp) -> &'static str {
        match op {
            crate::backend::cast::FloatOp::Add => "add",
            crate::backend::cast::FloatOp::Sub => "subr",
            crate::backend::cast::FloatOp::Mul => "mul",
            crate::backend::cast::FloatOp::Div => "divr",
        }
    }

    fn emit_float_binop_impl(&mut self, mnemonic: &str, ty: IrType) {
        // F32: Use SSE. F64/F128 are handled by emit_float_binop override (x87).
        // Load order: lhs→ecx, rhs→eax. Put lhs in xmm0, rhs in xmm1 so that
        // subss/divss %xmm1, %xmm0 computes xmm0 = lhs - rhs (correct order).
        if ty == IrType::F32 {
            self.state.emit("    movd %ecx, %xmm0");
            self.state.emit("    movd %eax, %xmm1");
            emit!(self.state, "    {}ss %xmm1, %xmm0", mnemonic);
            self.state.emit("    movd %xmm0, %eax");
        }
        self.state.reg_cache.invalidate_acc();
    }

    fn emit_acc_to_secondary(&mut self) {
        self.state.emit("    movl %eax, %ecx");
    }

    fn emit_copy_value(&mut self, dest: &Value, src: &Operand) {
        // For F128 values with full x87 data in their slots, copy via fldt/fstpt
        if let Operand::Value(v) = src {
            if self.state.f128_direct_slots.contains(&v.0) {
                if let (Some(src_slot), Some(dest_slot)) = (self.state.get_slot(v.0), self.state.get_slot(dest.0)) {
                    emit!(self.state, "    fldt {}(%ebp)", src_slot.0);
                    emit!(self.state, "    fstpt {}(%ebp)", dest_slot.0);
                    self.state.f128_direct_slots.insert(dest.0);
                    return;
                }
            }
            // Also check alloca_types for F128
            if let Some(&alloca_ty) = self.state.alloca_types.get(&v.0) {
                if alloca_ty == IrType::F128 {
                    if let (Some(src_slot), Some(dest_slot)) = (self.state.get_slot(v.0), self.state.get_slot(dest.0)) {
                        emit!(self.state, "    fldt {}(%ebp)", src_slot.0);
                        emit!(self.state, "    fstpt {}(%ebp)", dest_slot.0);
                        self.state.f128_direct_slots.insert(dest.0);
                        return;
                    }
                }
            }
        }

        // Wide values (F64, I64/U64) on i686 need 8-byte copies.
        // The 32-bit accumulator (eax) can only hold 4 bytes, so we must
        // copy both halves of the 64-bit value.
        let is_wide = match src {
            Operand::Value(v) => self.state.is_wide_value(v.0),
            Operand::Const(IrConst::F64(_)) => true,
            Operand::Const(IrConst::I64(_)) => true,
            _ => false,
        };
        if is_wide {
            if let Some(dest_slot) = self.state.get_slot(dest.0) {
                match src {
                    Operand::Value(v) => {
                        if let Some(src_slot) = self.state.get_slot(v.0) {
                            // Copy 8 bytes: two 32-bit moves
                            emit!(self.state, "    movl {}(%ebp), %eax", src_slot.0);
                            emit!(self.state, "    movl %eax, {}(%ebp)", dest_slot.0);
                            emit!(self.state, "    movl {}(%ebp), %eax", src_slot.0 + 4);
                            emit!(self.state, "    movl %eax, {}(%ebp)", dest_slot.0 + 4);
                        }
                    }
                    Operand::Const(IrConst::F64(val)) => {
                        let bits = val.to_bits();
                        let lo = bits as u32;
                        let hi = (bits >> 32) as u32;
                        emit!(self.state, "    movl ${}, {}(%ebp)", lo as i32, dest_slot.0);
                        emit!(self.state, "    movl ${}, {}(%ebp)", hi as i32, dest_slot.0 + 4);
                    }
                    Operand::Const(IrConst::I64(val)) => {
                        let lo = *val as u32;
                        let hi = (*val >> 32) as u32;
                        emit!(self.state, "    movl ${}, {}(%ebp)", lo as i32, dest_slot.0);
                        emit!(self.state, "    movl ${}, {}(%ebp)", hi as i32, dest_slot.0 + 4);
                    }
                    _ => unreachable!(),
                }
                self.state.reg_cache.invalidate_all();
                return;
            }
        }

        // Default path for non-F128, non-wide copies (32-bit values).
        self.emit_load_operand(src);
        self.emit_store_result(dest);
    }

    /// Override emit_cast to handle F64 source/destination specially on i686.
    /// F64 values are 8 bytes but the accumulator is only 32 bits, so we use
    /// x87 FPU for all F64 conversions, bypassing the default emit_load_operand path.
    fn emit_cast(&mut self, dest: &Value, src: &Operand, from_ty: IrType, to_ty: IrType) {
        use crate::backend::cast::{CastKind, classify_cast_with_f128};

        // Let the default handle i128 conversions
        if crate::backend::generation::is_i128_type(from_ty) || crate::backend::generation::is_i128_type(to_ty) {
            crate::backend::traits::emit_cast_default(self, dest, src, from_ty, to_ty);
            return;
        }

        // On i686, F128 (long double) is native x87 80-bit extended precision,
        // stored as 12 bytes. We must use f128_is_native=true so that F128 casts
        // go through the dedicated SignedToF128/UnsignedToF128/F128ToSigned/etc.
        // paths that use fstpt (12-byte store), not through the F64 paths that
        // use fstpl (8-byte store) which would corrupt F128 values.
        match classify_cast_with_f128(from_ty, to_ty, true) {
            // --- Casts where F64 is the destination (result needs 8-byte slot) ---
            CastKind::SignedToFloat { to_f64: true, from_ty: src_ty } => {
                // int → F64: load int, convert via x87, store 8-byte result
                if src_ty == IrType::I64 {
                    // I64 → F64: load full 64-bit value, use fildq
                    self.emit_load_acc_pair(src);
                    self.state.emit("    pushl %edx");
                    self.state.emit("    pushl %eax");
                    self.state.emit("    fildq (%esp)");
                    self.state.emit("    addl $8, %esp");
                } else {
                    self.operand_to_eax(src);
                    match src_ty {
                        IrType::I8 => self.state.emit("    movsbl %al, %eax"),
                        IrType::I16 => self.state.emit("    movswl %ax, %eax"),
                        _ => {}
                    }
                    self.state.emit("    pushl %eax");
                    self.state.emit("    fildl (%esp)");
                    self.state.emit("    addl $4, %esp");
                }
                // st(0) = F64 result, store to dest's 8-byte slot
                self.emit_f64_store_from_x87(dest);
                self.state.reg_cache.invalidate_acc();
            }
            CastKind::UnsignedToFloat { to_f64: true, from_ty } => {
                let from_u64 = from_ty == IrType::U64;
                // unsigned → F64
                if from_u64 {
                    // U64 → F64: fildq treats the value as signed, so values
                    // >= 2^63 need correction by adding float constant 2^64.
                    self.emit_load_acc_pair(src);
                    self.state.emit("    pushl %edx");
                    self.state.emit("    pushl %eax");
                    self.state.emit("    fildq (%esp)");
                    self.state.emit("    addl $8, %esp");
                    self.state.emit("    testl %edx, %edx");
                    let done_label = self.state.fresh_label("u64_f64_done");
                    self.state.out.emit_jcc_label("    jns", &done_label);
                    // High bit set: add 2^64 (float 0x5F800000) to fix sign
                    self.state.emit("    pushl $0x5F800000");
                    self.state.emit("    fadds (%esp)");
                    self.state.emit("    addl $4, %esp");
                    self.state.out.emit_named_label(&done_label);
                } else {
                    // U8/U16/U32 → F64: handle high-bit-set U32 values
                    self.operand_to_eax(src);
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
                self.emit_f64_load_to_x87(src);
                if to_ty == IrType::I64 {
                    // F64 → I64: use fisttpq for full 64-bit conversion
                    self.state.emit("    subl $8, %esp");
                    self.state.emit("    fisttpq (%esp)");
                    self.state.emit("    movl (%esp), %eax");
                    self.state.emit("    movl 4(%esp), %edx");
                    self.state.emit("    addl $8, %esp");
                    self.emit_store_acc_pair(dest);
                } else {
                    // F64 → I32/I16/I8: use fisttpl for 32-bit conversion
                    self.state.emit("    subl $4, %esp");
                    self.state.emit("    fisttpl (%esp)");
                    self.state.emit("    movl (%esp), %eax");
                    self.state.emit("    addl $4, %esp");
                    self.state.reg_cache.invalidate_acc();
                    self.store_eax_to(dest);
                }
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

            // --- F128 ↔ F64/F32 conversions ---
            // On i686, F128 is native x87 80-bit, so we use classify_cast_with_f128(true)
            // and these paths are actively used for F32/F64 ↔ F128 conversions.
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
                    self.state.f128_direct_slots.insert(dest.0);
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
                if src_ty == IrType::I64 {
                    // I64 → F128: load full 64-bit value via register pair, use fildq
                    self.emit_load_acc_pair(src);
                    self.state.emit("    pushl %edx");
                    self.state.emit("    pushl %eax");
                    self.state.emit("    fildq (%esp)");
                    self.state.emit("    addl $8, %esp");
                } else {
                    self.operand_to_eax(src);
                    match src_ty {
                        IrType::I8 => self.state.emit("    movsbl %al, %eax"),
                        IrType::I16 => self.state.emit("    movswl %ax, %eax"),
                        _ => {}
                    }
                    self.state.emit("    pushl %eax");
                    self.state.emit("    fildl (%esp)");
                    self.state.emit("    addl $4, %esp");
                }
                if let Some(slot) = self.state.get_slot(dest.0) {
                    emit!(self.state, "    fstpt {}(%ebp)", slot.0);
                    self.state.f128_direct_slots.insert(dest.0);
                }
                self.state.reg_cache.invalidate_acc();
            }
            CastKind::UnsignedToF128 { from_ty: src_ty } => {
                if src_ty == IrType::U64 {
                    // U64 → F128 (x87 80-bit long double):
                    // fildq treats the value as signed. For values >= 2^63
                    // (high bit set), fildq gives a negative result. We fix
                    // this by adding 2^64 (as a float constant 0x5F800000).
                    // This is exact because x87 extended precision has a
                    // 64-bit significand, enough for all uint64 values.
                    self.emit_load_acc_pair(src);
                    self.state.emit("    pushl %edx");
                    self.state.emit("    pushl %eax");
                    self.state.emit("    fildq (%esp)");
                    self.state.emit("    addl $8, %esp");
                    self.state.emit("    testl %edx, %edx");
                    let done_label = self.state.fresh_label("u64_f128_done");
                    self.state.out.emit_jcc_label("    jns", &done_label);
                    // High bit set: add 2^64 to compensate for signed interpretation.
                    // Float constant 0x5F800000 = 2^64 = 18446744073709551616.0f
                    self.state.emit("    pushl $0x5F800000");
                    self.state.emit("    fadds (%esp)");
                    self.state.emit("    addl $4, %esp");
                    self.state.out.emit_named_label(&done_label);
                } else {
                    // U8/U16/U32 → F128: handle high-bit-set U32 values
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
                    // Bit 31 set: zero-extend to 64-bit and use fildq
                    self.state.emit("    pushl $0");
                    self.state.emit("    pushl %eax");
                    self.state.emit("    fildq (%esp)");
                    self.state.emit("    addl $8, %esp");
                    self.state.out.emit_named_label(&done_label);
                }
                if let Some(slot) = self.state.get_slot(dest.0) {
                    emit!(self.state, "    fstpt {}(%ebp)", slot.0);
                    self.state.f128_direct_slots.insert(dest.0);
                }
                self.state.reg_cache.invalidate_acc();
            }
            CastKind::F128ToSigned { to_ty: dest_ty } => {
                self.emit_f128_load_to_x87(src);
                if dest_ty == IrType::I64 {
                    // F128 → I64: use fisttpq for full 64-bit conversion
                    self.state.emit("    subl $8, %esp");
                    self.state.emit("    fisttpq (%esp)");
                    self.state.emit("    movl (%esp), %eax");
                    self.state.emit("    movl 4(%esp), %edx");
                    self.state.emit("    addl $8, %esp");
                    self.emit_store_acc_pair(dest);
                } else {
                    // F128 → I32/I16/I8: use fisttpl for 32-bit conversion
                    self.state.emit("    subl $4, %esp");
                    self.state.emit("    fisttpl (%esp)");
                    self.state.emit("    movl (%esp), %eax");
                    self.state.emit("    addl $4, %esp");
                    self.state.reg_cache.invalidate_acc();
                    self.store_eax_to(dest);
                }
            }
            CastKind::F128ToUnsigned { to_ty: dest_ty } => {
                self.emit_f128_load_to_x87(src);
                if dest_ty == IrType::U64 {
                    // F128 → U64: use fisttpq for full 64-bit conversion
                    self.state.emit("    subl $8, %esp");
                    self.state.emit("    fisttpq (%esp)");
                    self.state.emit("    movl (%esp), %eax");
                    self.state.emit("    movl 4(%esp), %edx");
                    self.state.emit("    addl $8, %esp");
                    self.emit_store_acc_pair(dest);
                } else {
                    // F128 → U32/U16/U8: use fisttpq then take low 32 bits
                    self.state.emit("    subl $8, %esp");
                    self.state.emit("    fisttpq (%esp)");
                    self.state.emit("    movl (%esp), %eax");
                    self.state.emit("    addl $8, %esp");
                    self.state.reg_cache.invalidate_acc();
                    self.store_eax_to(dest);
                }
            }

            // --- I64 → F32: use x87 fildq for full 64-bit precision ---
            CastKind::SignedToFloat { to_f64: false, from_ty: IrType::I64 } => {
                self.emit_load_acc_pair(src);
                self.state.emit("    pushl %edx");
                self.state.emit("    pushl %eax");
                self.state.emit("    fildq (%esp)");
                self.state.emit("    fstps (%esp)");
                self.state.emit("    movl (%esp), %eax");
                self.state.emit("    addl $8, %esp");
                self.state.reg_cache.invalidate_acc();
                self.store_eax_to(dest);
            }
            // --- U64 → F32: use x87 with unsigned handling ---
            CastKind::UnsignedToFloat { to_f64: false, from_ty: IrType::U64 } => {
                self.emit_load_acc_pair(src);
                self.state.emit("    pushl %edx");
                self.state.emit("    pushl %eax");
                self.state.emit("    fildq (%esp)");
                self.state.emit("    addl $8, %esp");
                // If high bit was set, fildq gave a negative result; add 2^64
                self.state.emit("    testl %edx, %edx");
                let done_label = self.state.fresh_label("u64_f32_done");
                self.state.out.emit_jcc_label("    jns", &done_label);
                // Float constant 0x5F800000 = 2^64
                self.state.emit("    pushl $0x5F800000");
                self.state.emit("    fadds (%esp)");
                self.state.emit("    addl $4, %esp");
                self.state.out.emit_named_label(&done_label);
                self.state.emit("    subl $4, %esp");
                self.state.emit("    fstps (%esp)");
                self.state.emit("    movl (%esp), %eax");
                self.state.emit("    addl $4, %esp");
                self.state.reg_cache.invalidate_acc();
                self.store_eax_to(dest);
            }
            // --- F32 → I64: use x87 fisttpq ---
            CastKind::FloatToSigned { from_f64: false } if to_ty == IrType::I64 => {
                self.operand_to_eax(src);
                self.state.emit("    subl $8, %esp");
                self.state.emit("    movl %eax, (%esp)");
                self.state.emit("    flds (%esp)");
                self.state.emit("    fisttpq (%esp)");
                self.state.emit("    movl (%esp), %eax");
                self.state.emit("    movl 4(%esp), %edx");
                self.state.emit("    addl $8, %esp");
                self.emit_store_acc_pair(dest);
                self.state.reg_cache.invalidate_all();
            }
            // --- F32 → U64: use x87 fisttpq ---
            CastKind::FloatToUnsigned { from_f64: false, to_u64: true } => {
                self.operand_to_eax(src);
                self.state.emit("    subl $8, %esp");
                self.state.emit("    movl %eax, (%esp)");
                self.state.emit("    flds (%esp)");
                self.state.emit("    fisttpq (%esp)");
                self.state.emit("    movl (%esp), %eax");
                self.state.emit("    movl 4(%esp), %edx");
                self.state.emit("    addl $8, %esp");
                self.emit_store_acc_pair(dest);
                self.state.reg_cache.invalidate_all();
            }

            // --- Same-size cast between I64 and U64: copy all 8 bytes ---
            CastKind::SignedToUnsignedSameSize { to_ty: IrType::U64 }
            | CastKind::Noop if matches!((from_ty, to_ty), (IrType::I64, IrType::U64) | (IrType::U64, IrType::I64) | (IrType::I64, IrType::I64) | (IrType::U64, IrType::U64)) => {
                self.emit_load_acc_pair(src);
                self.emit_store_acc_pair(dest);
                self.state.reg_cache.invalidate_all();
            }

            // --- Widening casts to I64/U64 need full 8-byte store ---
            CastKind::IntWiden { .. } if matches!(to_ty, IrType::I64 | IrType::U64) => {
                self.operand_to_eax(src);
                self.emit_cast_instrs(from_ty, to_ty);
                // Set high half: sign-extend for signed sources, zero-extend for unsigned
                if from_ty.is_signed() {
                    self.state.emit("    cltd"); // sign-extend eax into edx:eax
                } else {
                    self.state.emit("    xorl %edx, %edx");
                }
                self.emit_store_acc_pair(dest);
                self.state.reg_cache.invalidate_all();
            }
            // --- I64/U64 narrowing to smaller types ---
            CastKind::IntNarrow { .. } if matches!(from_ty, IrType::I64 | IrType::U64) => {
                // Load only the low 32 bits (truncation)
                self.operand_to_eax(src);
                self.emit_cast_instrs(from_ty, to_ty);
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

            CastKind::UnsignedToFloat { to_f64: false, .. } => {
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
        // Always use absolute addressing on i686.
        // Proper i686 PIC requires setting up %ebx as the GOT base register
        // via __x86.get_pc_thunk, which is not yet implemented. Since we link
        // with -no-pie, absolute addressing works correctly — the linker resolves
        // all relocations at link time.
        emit!(self.state, "    movl ${}, %eax", name);
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

    /// On i686, 64-bit conditions (F64/I64/U64) need both 32-bit halves tested.
    /// The default only loads the low 32 bits into eax, missing nonzero values
    /// where only the high 32 bits are set (e.g., double 1.0 = 0x3FF00000_00000000).
    fn emit_cond_branch_blocks(&mut self, cond: &Operand, true_block: BlockId, false_block: BlockId) {
        // Try constant-folding wide conditions at compile time
        match cond {
            Operand::Const(IrConst::I64(v)) => {
                if *v != 0 {
                    self.emit_branch_to_block(true_block);
                } else {
                    self.emit_branch_to_block(false_block);
                }
                return;
            }
            Operand::Const(IrConst::F64(fval)) => {
                // In C, -0.0 is falsy (compares equal to 0.0), so use IEEE 754
                // comparison rather than bit equality.
                if *fval != 0.0 {
                    self.emit_branch_to_block(true_block);
                } else {
                    self.emit_branch_to_block(false_block);
                }
                return;
            }
            _ => {}
        }

        // For runtime 64-bit values, OR both halves and test the result
        if let Operand::Value(v) = cond {
            if self.state.is_wide_value(v.0) {
                self.emit_wide_value_to_eax_ored(v.0);
                self.state.reg_cache.invalidate_acc();
                let true_label = true_block.as_label();
                self.emit_branch_nonzero(&true_label);
                self.emit_branch_to_block(false_block);
                return;
            }
        }

        // Default path for 32-bit values
        self.operand_to_eax(cond);
        let true_label = true_block.as_label();
        self.emit_branch_nonzero(&true_label);
        self.emit_branch_to_block(false_block);
    }

    /// On i686, selects with 64-bit conditions or results need special handling.
    /// Uses emit_copy_value (which handles wide values) instead of the default
    /// emit_load_operand/emit_store_result path that only handles 32 bits.
    fn emit_select(&mut self, dest: &Value, cond: &Operand, true_val: &Operand, false_val: &Operand, ty: IrType) {
        // Constant-fold wide conditions at compile time
        match cond {
            Operand::Const(IrConst::I64(v)) => {
                self.emit_copy_value(dest, if *v != 0 { true_val } else { false_val });
                return;
            }
            Operand::Const(IrConst::F64(fval)) => {
                self.emit_copy_value(dest, if *fval != 0.0 { true_val } else { false_val });
                return;
            }
            _ => {}
        }

        let cond_is_wide = matches!(cond, Operand::Value(v) if self.state.is_wide_value(v.0));
        let result_is_wide = matches!(ty, IrType::F64 | IrType::I64 | IrType::U64);

        // If neither condition nor result is wide, use the default 32-bit path
        if !cond_is_wide && !result_is_wide {
            let label_id = self.state.next_label_id();
            let true_label = format!(".Lsel_true_{}", label_id);
            let end_label = format!(".Lsel_end_{}", label_id);
            self.emit_load_operand(cond);
            self.emit_branch_nonzero(&true_label);
            self.emit_load_operand(false_val);
            self.emit_store_result(dest);
            self.emit_branch(&end_label);
            self.state.emit_fmt(format_args!("{}:", true_label));
            self.emit_load_operand(true_val);
            self.emit_store_result(dest);
            self.state.emit_fmt(format_args!("{}:", end_label));
            return;
        }

        let label_id = self.state.next_label_id();
        let true_label = format!(".Lsel_true_{}", label_id);
        let end_label = format!(".Lsel_end_{}", label_id);

        // Load condition into eax, OR'ing both halves for wide conditions
        if cond_is_wide {
            if let Operand::Value(v) = cond {
                self.emit_wide_value_to_eax_ored(v.0);
                self.state.reg_cache.invalidate_acc();
            }
        } else {
            self.operand_to_eax(cond);
        }

        self.emit_branch_nonzero(&true_label);

        // False path: copy false_val to dest, jump to end
        self.emit_copy_value(dest, false_val);
        self.emit_branch(&end_label);

        // True path: copy true_val to dest
        self.state.emit_fmt(format_args!("{}:", true_label));
        self.emit_copy_value(dest, true_val);

        // End
        self.state.emit_fmt(format_args!("{}:", end_label));
    }

    fn emit_jump_indirect(&mut self) {
        self.state.emit("    jmp *%eax");
    }

    /// Override emit_switch to handle 64-bit (I64/U64) switch values on i686.
    /// The default trait implementation only loads/compares 32 bits, which is
    /// incorrect for long long switches where case values may differ only in
    /// the high 32 bits (e.g., 4294967295LL vs -1LL).
    fn emit_switch(&mut self, val: &Operand, cases: &[(i64, BlockId)], default: &BlockId) {
        // Check if the switch value is a 64-bit (wide) value
        let is_wide = match val {
            Operand::Value(v) => self.state.is_wide_value(v.0),
            Operand::Const(IrConst::I64(_)) => true,
            _ => false,
        };

        if !is_wide {
            // 32-bit switch: use the default trait implementation path
            use crate::backend::traits::{MIN_JUMP_TABLE_CASES, MAX_JUMP_TABLE_RANGE, MIN_JUMP_TABLE_DENSITY_PERCENT};
            let use_jump_table = if self.state.no_jump_tables {
                false
            } else if cases.len() >= MIN_JUMP_TABLE_CASES {
                let min_val = cases.iter().map(|&(v, _)| v).min().unwrap();
                let max_val = cases.iter().map(|&(v, _)| v).max().unwrap();
                let range = (max_val - min_val + 1) as usize;
                range <= MAX_JUMP_TABLE_RANGE && cases.len() * 100 / range >= MIN_JUMP_TABLE_DENSITY_PERCENT
            } else {
                false
            };
            if use_jump_table {
                self.emit_switch_jump_table(val, cases, default);
            } else {
                self.emit_load_operand(val);
                for &(case_val, target) in cases {
                    let label = target.as_label();
                    self.emit_switch_case_branch(case_val, &label);
                }
                self.emit_branch_to_block(*default);
            }
            return;
        }

        // 64-bit switch: compare both 32-bit halves for each case.
        // Load the 64-bit value into eax (low) and edx (high).
        self.emit_load_acc_pair(val);

        // For each case value, compare both halves:
        //   1. Compare low word (eax) against case_low
        //   2. If not equal, skip to next case
        //   3. Compare high word (edx) against case_high
        //   4. If equal, branch to case target
        for &(case_val, target) in cases {
            let case_low = case_val as i32;
            let case_high = (case_val >> 32) as i32;
            let label = target.as_label();
            let skip_label = format!(".Lswskip_{}", self.state.next_label_id());

            // Compare low 32 bits
            if case_low == 0 {
                self.state.emit("    testl %eax, %eax");
            } else {
                emit!(self.state, "    cmpl ${}, %eax", case_low);
            }
            emit!(self.state, "    jne {}", skip_label);

            // Low matches; compare high 32 bits
            if case_high == 0 {
                self.state.emit("    testl %edx, %edx");
            } else {
                emit!(self.state, "    cmpl ${}, %edx", case_high);
            }
            emit!(self.state, "    je {}", label);

            emit!(self.state, "{}:", skip_label);
        }
        self.emit_branch_to_block(*default);
        self.state.reg_cache.invalidate_all();
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

    /// Override emit_cmp to route I64/U64 comparisons through register-pair path.
    /// On i686, 64-bit comparisons need to compare both halves (high word first,
    /// then low word if equal). The emit_i128_cmp methods implement this correctly.
    fn emit_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if matches!(ty, IrType::I64 | IrType::U64) {
            self.emit_i128_cmp(dest, op, lhs, rhs);
            self.state.reg_cache.invalidate_all();
            return;
        }
        if crate::backend::generation::is_i128_type(ty) {
            self.emit_i128_cmp(dest, op, lhs, rhs);
            return;
        }
        if ty == IrType::F128 {
            self.emit_f128_cmp(dest, op, lhs, rhs);
            return;
        }
        if ty.is_float() {
            self.emit_float_cmp(dest, op, lhs, rhs, ty);
            return;
        }
        self.emit_int_cmp(dest, op, lhs, rhs, ty);
    }

    fn emit_int_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand, _ty: IrType) {
        // Only I32/U32 and smaller comparisons reach here now.
        // I64/U64 comparisons are intercepted by emit_cmp above.
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

    /// Override unary op to handle I64/U64 and F64/F128 properly on i686.
    /// I64/U64 need register-pair (eax:edx) operations for Neg/Not.
    /// F64 needs x87 for negation since the accumulator is only 32 bits.
    fn emit_unaryop(&mut self, dest: &Value, op: IrUnaryOp, src: &Operand, ty: IrType) {
        if matches!(ty, IrType::I64 | IrType::U64) {
            if op == IrUnaryOp::IsConstant {
                self.emit_load_operand(&Operand::Const(IrConst::I32(0)));
                self.emit_store_result(dest);
                return;
            }
            self.emit_load_acc_pair(src);
            match op {
                IrUnaryOp::Neg => self.emit_i128_neg(),
                IrUnaryOp::Not => self.emit_i128_not(),
                IrUnaryOp::Clz => self.emit_i64_clz(),
                IrUnaryOp::Ctz => self.emit_i64_ctz(),
                IrUnaryOp::Popcount => self.emit_i64_popcount(),
                IrUnaryOp::Bswap => self.emit_i64_bswap(),
                _ => {}
            }
            self.emit_store_acc_pair(dest);
            self.state.reg_cache.invalidate_all();
            return;
        }
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
            self.state.f128_direct_slots.insert(dest.0);
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

    /// Override emit_call to handle the fastcall calling convention on i686.
    /// For fastcall, the first two DWORD integer/pointer args go in ecx/edx,
    /// remaining args go on the stack, and the callee cleans up the stack.
    fn emit_call(&mut self, args: &[Operand], arg_types: &[IrType], direct_name: Option<&str>,
                 func_ptr: Option<&Operand>, dest: Option<Value>, return_type: IrType,
                 is_variadic: bool, num_fixed_args: usize, struct_arg_sizes: &[Option<usize>],
                 struct_arg_classes: &[Vec<crate::common::types::EightbyteClass>],
                 is_sret: bool,
                 is_fastcall: bool) {
        if !is_fastcall {
            // Delegate to the default trait implementation for cdecl calls.
            // We can't call the default method directly because Rust doesn't support
            // calling default trait methods from an overriding method. So we duplicate
            // the call to the shared skeleton.
            use crate::backend::call_abi::*;
            let config = self.call_abi_config();
            let arg_classes_vec = classify_call_args(args, arg_types, struct_arg_sizes, struct_arg_classes, is_variadic, &config);
            let indirect = func_ptr.is_some() && direct_name.is_none();
            if indirect {
                self.emit_call_spill_fptr(func_ptr.unwrap());
            }
            let stack_arg_space = self.emit_call_compute_stack_space(&arg_classes_vec, arg_types);
            let f128_temp_space = self.emit_call_f128_pre_convert(args, &arg_classes_vec, arg_types, stack_arg_space);
            self.state().reg_cache.invalidate_acc();
            let total_sp_adjust = self.emit_call_stack_args(args, &arg_classes_vec, arg_types, stack_arg_space,
                                                            if indirect { self.emit_call_fptr_spill_size() } else { 0 },
                                                            f128_temp_space);
            self.state().reg_cache.invalidate_acc();
            self.emit_call_reg_args(args, &arg_classes_vec, arg_types, total_sp_adjust, f128_temp_space, stack_arg_space);
            self.emit_call_instruction(direct_name, func_ptr, indirect, stack_arg_space);
            // On i386 SysV, sret calls have the callee pop the hidden pointer with
            // `ret $4`, so subtract those bytes from the caller's stack cleanup.
            let callee_pops = self.callee_pops_bytes_for_sret(is_sret);
            let effective_stack_cleanup = stack_arg_space.saturating_sub(callee_pops);
            self.emit_call_cleanup(effective_stack_cleanup, f128_temp_space, indirect);
            if let Some(dest) = dest {
                self.emit_call_store_result(&dest, return_type);
            }
            return;
        }

        // Fastcall calling convention:
        // - First two DWORD (int/ptr) args in ECX, EDX
        // - Remaining args pushed right-to-left on stack
        // - Callee pops the stack args (caller does NOT adjust ESP after call)
        self.emit_fastcall(args, arg_types, direct_name, func_ptr, dest, return_type);
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
                            self.state.reg_cache.invalidate_acc();
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
                    // Long double: copy 12 bytes via x87 for full precision
                    match &args[i] {
                        Operand::Value(v) => {
                            if self.state.f128_direct_slots.contains(&v.0) {
                                // Full x87 data in slot: use fldt/fstpt for precision
                                if let Some(slot) = self.state.get_slot(v.0) {
                                    emit!(self.state, "    fldt {}(%ebp)", slot.0);
                                    emit!(self.state, "    fstpt {}(%esp)", stack_offset);
                                }
                            } else if let Some(slot) = self.state.get_slot(v.0) {
                                for j in (0..12).step_by(4) {
                                    emit!(self.state, "    movl {}(%ebp), %eax", slot.0 + j as i64);
                                    emit!(self.state, "    movl %eax, {}(%esp)", stack_offset + j);
                                }
                                self.state.reg_cache.invalidate_acc();
                            } else {
                                self.operand_to_eax(&args[i]);
                                emit!(self.state, "    movl %eax, {}(%esp)", stack_offset);
                                for j in (4..12).step_by(4) {
                                    emit!(self.state, "    movl $0, {}(%esp)", stack_offset + j);
                                }
                            }
                        }
                        Operand::Const(IrConst::LongDouble(_, bytes)) => {
                            let x87 = crate::common::long_double::f128_bytes_to_x87_bytes(bytes);
                            let dword0 = i32::from_le_bytes([x87[0], x87[1], x87[2], x87[3]]);
                            let dword1 = i32::from_le_bytes([x87[4], x87[5], x87[6], x87[7]]);
                            let word2 = i16::from_le_bytes([x87[8], x87[9]]) as i32;
                            emit!(self.state, "    movl ${}, {}(%esp)", dword0, stack_offset);
                            emit!(self.state, "    movl ${}, {}(%esp)", dword1, stack_offset + 4);
                            emit!(self.state, "    movw ${}, {}(%esp)", word2, stack_offset + 8);
                        }
                        Operand::Const(IrConst::F64(fval)) => {
                            // Push f64 as long double via x87
                            let bits = fval.to_bits();
                            let low = (bits & 0xFFFFFFFF) as i32;
                            let high = ((bits >> 32) & 0xFFFFFFFF) as i32;
                            self.state.emit("    subl $8, %esp");
                            emit!(self.state, "    movl ${}, (%esp)", low);
                            emit!(self.state, "    movl ${}, 4(%esp)", high);
                            self.state.emit("    fldl (%esp)");
                            self.state.emit("    addl $8, %esp");
                            emit!(self.state, "    fstpt {}(%esp)", stack_offset);
                        }
                        _ => {
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
                                self.state.reg_cache.invalidate_acc();
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
                            self.state.reg_cache.invalidate_acc();
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
                                self.state.reg_cache.invalidate_acc();
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
                                self.state.reg_cache.invalidate_acc();
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

    fn callee_pops_bytes_for_sret(&self, is_sret: bool) -> usize {
        // i386 SysV ABI: when a function returns a struct via hidden pointer (sret),
        // the callee pops the hidden pointer with `ret $4`. The caller must subtract
        // 4 from its stack cleanup to compensate.
        if is_sret { 4 } else { 0 }
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
            self.state.f128_direct_slots.insert(dest.0);
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
        // i386 SysV ABI: functions returning structs via hidden pointer (sret)
        // must pop the hidden pointer argument with `ret $4`.
        if self.state.uses_sret {
            self.state.emit("    ret $4");
        } else if self.is_fastcall && self.fastcall_stack_cleanup > 0 {
            emit!(self.state, "    ret ${}", self.fastcall_stack_cleanup);
        } else {
            self.state.emit("    ret");
        }
    }

    // --- Typed store/load ---

    /// Override emit_load for I64/U64/F64/F128.
    fn emit_load(&mut self, dest: &Value, ptr: &Value, ty: IrType) {
        if ty == IrType::F128 {
            // F128 (long double): load 80-bit value via x87 fldt/fstpt
            let addr = self.state.resolve_slot_addr(ptr.0);
            if let Some(addr) = addr {
                match addr {
                    crate::backend::state::SlotAddr::OverAligned(slot, id) => {
                        self.emit_alloca_aligned_addr(slot, id);
                        self.state.emit("    fldt (%ecx)");
                    }
                    crate::backend::state::SlotAddr::Direct(slot) => {
                        // Direct: alloca data at slot(%ebp), fldt directly
                        emit!(self.state, "    fldt {}(%ebp)", slot.0);
                    }
                    crate::backend::state::SlotAddr::Indirect(slot) => {
                        self.emit_load_ptr_from_slot(slot, ptr.0);
                        self.state.emit("    fldt (%ecx)");
                    }
                }
                if let Some(dest_slot) = self.state.get_slot(dest.0) {
                    emit!(self.state, "    fstpt {}(%ebp)", dest_slot.0);
                    self.state.f128_direct_slots.insert(dest.0);
                }
            }
            return;
        }
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
            self.state.reg_cache.invalidate_acc();
            return;
        }
        crate::backend::traits::emit_load_default(self, dest, ptr, ty);
    }

    /// Override emit_store for I64/U64/F64/F128.
    fn emit_store(&mut self, val: &Operand, ptr: &Value, ty: IrType) {
        if ty == IrType::F128 {
            // F128 (long double): load onto x87, store via fstpt to target address
            self.emit_f128_load_to_x87(val);
            let addr = self.state.resolve_slot_addr(ptr.0);
            if let Some(addr) = addr {
                match addr {
                    crate::backend::state::SlotAddr::OverAligned(slot, id) => {
                        self.emit_alloca_aligned_addr(slot, id);
                        self.state.emit("    fstpt (%ecx)");
                    }
                    crate::backend::state::SlotAddr::Direct(slot) => {
                        // Direct: alloca data at slot(%ebp), fstpt directly
                        emit!(self.state, "    fstpt {}(%ebp)", slot.0);
                    }
                    crate::backend::state::SlotAddr::Indirect(slot) => {
                        self.emit_load_ptr_from_slot(slot, ptr.0);
                        self.state.emit("    fstpt (%ecx)");
                    }
                }
            }
            self.state.reg_cache.invalidate_acc();
            return;
        }
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

    /// Override load with GEP-folded constant offset for I64/U64/F64/F128.
    fn emit_load_with_const_offset(&mut self, dest: &Value, base: &Value, offset: i64, ty: IrType) {
        if ty == IrType::F128 {
            let addr = self.state.resolve_slot_addr(base.0);
            if let Some(addr) = addr {
                match addr {
                    crate::backend::state::SlotAddr::OverAligned(slot, id) => {
                        self.emit_alloca_aligned_addr(slot, id);
                        if offset != 0 {
                            self.emit_add_offset_to_addr_reg(offset);
                        }
                        self.state.emit("    fldt (%ecx)");
                    }
                    crate::backend::state::SlotAddr::Direct(slot) => {
                        let folded = slot.0 + offset;
                        emit!(self.state, "    fldt {}(%ebp)", folded);
                    }
                    crate::backend::state::SlotAddr::Indirect(slot) => {
                        self.emit_load_ptr_from_slot(slot, base.0);
                        if offset != 0 {
                            self.emit_add_offset_to_addr_reg(offset);
                        }
                        self.state.emit("    fldt (%ecx)");
                    }
                }
                if let Some(dest_slot) = self.state.get_slot(dest.0) {
                    emit!(self.state, "    fstpt {}(%ebp)", dest_slot.0);
                    self.state.f128_direct_slots.insert(dest.0);
                }
            }
            return;
        }
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
        if ty == IrType::F128 {
            self.emit_f128_load_to_x87(val);
            let addr = self.state.resolve_slot_addr(base.0);
            if let Some(addr) = addr {
                match addr {
                    crate::backend::state::SlotAddr::OverAligned(slot, id) => {
                        self.emit_alloca_aligned_addr(slot, id);
                        if offset != 0 {
                            self.emit_add_offset_to_addr_reg(offset);
                        }
                        self.state.emit("    fstpt (%ecx)");
                    }
                    crate::backend::state::SlotAddr::Direct(slot) => {
                        let folded = slot.0 + offset;
                        emit!(self.state, "    fstpt {}(%ebp)", folded);
                    }
                    crate::backend::state::SlotAddr::Indirect(slot) => {
                        self.emit_load_ptr_from_slot(slot, base.0);
                        if offset != 0 {
                            self.emit_add_offset_to_addr_reg(offset);
                        }
                        self.state.emit("    fstpt (%ecx)");
                    }
                }
            }
            self.state.reg_cache.invalidate_acc();
            return;
        }
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
    //
    // On i686, `rep movsb` implicitly clobbers esi, edi, and ecx.
    // Since esi (PhysReg 1) and edi (PhysReg 2) are callee-saved registers
    // that may be allocated to hold live values, we must save and restore
    // them around the entire memcpy operation.

    fn emit_memcpy(&mut self, dest: &Value, src: &Value, size: usize) {
        use crate::backend::state::SlotAddr;
        // Always save esi and edi around rep movsb.
        // These are callee-saved registers in the System V i386 ABI, so we must
        // preserve them even if the register allocator didn't assign any values
        // to them in this function. A caller may be relying on their preservation
        // across a call to this function.
        self.state.emit("    pushl %esi");
        self.state.emit("    pushl %edi");

        // Load dest address into edi
        if let Some(addr) = self.state.resolve_slot_addr(dest.0) {
            match addr {
                SlotAddr::OverAligned(slot, id) => {
                    // Use _to_acc variant which computes into eax
                    self.emit_alloca_aligned_addr_to_acc(slot, id);
                    self.state.emit("    movl %eax, %edi");
                }
                SlotAddr::Direct(slot) => self.emit_memcpy_load_dest_addr(slot, true, dest.0),
                SlotAddr::Indirect(slot) => self.emit_memcpy_load_dest_addr(slot, false, dest.0),
            }
        }
        // Load src address into esi
        if let Some(addr) = self.state.resolve_slot_addr(src.0) {
            match addr {
                SlotAddr::OverAligned(slot, id) => {
                    self.emit_alloca_aligned_addr_to_acc(slot, id);
                    self.state.emit("    movl %eax, %esi");
                }
                SlotAddr::Direct(slot) => self.emit_memcpy_load_src_addr(slot, true, src.0),
                SlotAddr::Indirect(slot) => self.emit_memcpy_load_src_addr(slot, false, src.0),
            }
        }
        // Perform the copy
        emit!(self.state, "    movl ${}, %ecx", size);
        self.state.emit("    rep movsb");

        // Restore edi and esi (reverse order of push)
        self.state.emit("    popl %edi");
        self.state.emit("    popl %esi");
    }

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
        // i686 cdecl: va_list is just a char* pointer to the stack.
        // va_list_ptr is a pointer to the memory location holding the va_list value.
        // We need to:
        // 1. Load va_list_ptr address into %edx
        // 2. Load the va_list value (char*) from (%edx) into %ecx
        // 3. Read the argument from (%ecx)
        // 4. Advance %ecx
        // 5. Store updated va_list back through (%edx)

        // Step 1: Get address of va_list storage into %edx
        self.load_va_list_addr_to_edx(va_list_ptr);
        // Step 2: Load current va_list pointer from that address
        self.state.emit("    movl (%edx), %ecx");

        // Step 3 & 4: Load the value and advance
        if is_i128_type(result_ty) {
            if let Some(dest_slot) = self.state.get_slot(dest.0) {
                for i in (0..16).step_by(4) {
                    emit!(self.state, "    movl {}(%ecx), %eax", i);
                    emit!(self.state, "    movl %eax, {}(%ebp)", dest_slot.0 + i as i64);
                }
            }
            self.state.emit("    addl $16, %ecx");
        } else if result_ty == IrType::F128 {
            if let Some(dest_slot) = self.state.get_slot(dest.0) {
                self.state.emit("    fldt (%ecx)");
                emit!(self.state, "    fstpt {}(%ebp)", dest_slot.0);
                self.state.f128_direct_slots.insert(dest.0);
            }
            self.state.emit("    addl $12, %ecx");
        } else if result_ty == IrType::F64 || result_ty == IrType::I64 || result_ty == IrType::U64 {
            // 8-byte types: read two dwords from the va_list
            if let Some(dest_slot) = self.state.get_slot(dest.0) {
                self.state.emit("    movl (%ecx), %eax");
                emit!(self.state, "    movl %eax, {}(%ebp)", dest_slot.0);
                self.state.emit("    movl 4(%ecx), %eax");
                emit!(self.state, "    movl %eax, {}(%ebp)", dest_slot.0 + 4);
            }
            self.state.emit("    addl $8, %ecx");
        } else {
            let load_instr = self.mov_load_for_type(result_ty);
            emit!(self.state, "    {} (%ecx), %eax", load_instr);
            self.store_eax_to(dest);
            // Advance pointer by 4 (minimum push size on i686 stack)
            let advance = result_ty.size().max(4);
            emit!(self.state, "    addl ${}, %ecx", advance);
        }
        // Step 5: Store updated va_list pointer back through the address in %edx
        // Note: %edx was set in step 1 and hasn't been clobbered (we only use %eax/%ecx above).
        // However, store_eax_to may use %edx in some cases, so we need to reload %edx
        // if store_eax_to was called (the scalar case). For safety, reload %edx.
        self.load_va_list_addr_to_edx(va_list_ptr);
        self.state.emit("    movl %ecx, (%edx)");
    }

    fn emit_va_start(&mut self, va_list_ptr: &Value) {
        // i686 cdecl: va_list = pointer to first unnamed arg on the stack.
        // Stack layout above ebp: [ret addr (4)] [params...]
        // Named params start at 8(%ebp) and occupy va_named_stack_bytes bytes.
        // Variadic args start immediately after: 8 + va_named_stack_bytes.
        //
        // va_list_ptr is a pointer to the va_list storage location. We compute
        // the address of the first vararg and store it through va_list_ptr.
        let vararg_offset = 8 + self.va_named_stack_bytes as i64;
        self.load_va_list_addr_to_edx(va_list_ptr);
        emit!(self.state, "    leal {}(%ebp), %eax", vararg_offset);
        self.state.emit("    movl %eax, (%edx)");
    }

    fn emit_va_copy(&mut self, dest_ptr: &Value, src_ptr: &Value) {
        // i686: va_list is just a 4-byte pointer. Copy the va_list value from
        // src to dest. Both pointers point to the va_list storage location.
        self.load_va_list_addr_to_edx(src_ptr);
        self.state.emit("    movl (%edx), %eax");
        self.load_va_list_addr_to_edx(dest_ptr);
        self.state.emit("    movl %eax, (%edx)");
    }

    // --- Atomics ---

    fn emit_atomic_rmw(&mut self, dest: &Value, op: AtomicRmwOp, ptr: &Operand, val: &Operand,
                       ty: IrType, _ordering: AtomicOrdering) {
        if self.is_atomic_wide(ty) {
            self.emit_atomic_rmw_wide(dest, op, ptr, val);
            return;
        }

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
                let suffix = self.type_suffix(ty);
                let edx_reg = match ty {
                    IrType::I8 | IrType::U8 => "%dl",
                    IrType::I16 | IrType::U16 => "%dx",
                    _ => "%edx",
                };
                self.state.emit("    pushl %edx");
                let load_instr = self.mov_load_for_type(ty);
                emit!(self.state, "    {} (%ecx), %eax", load_instr);
                let loop_label = format!(".Latomic_{}", self.state.next_label_id());
                emit!(self.state, "{}:", loop_label);
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
                emit!(self.state, "    lock cmpxchg{} {}, (%ecx)", suffix, edx_reg);
                emit!(self.state, "    jne {}", loop_label);
                self.state.emit("    addl $4, %esp");
            }
        }
        self.state.reg_cache.invalidate_acc();
        self.store_eax_to(dest);
    }

    fn emit_atomic_cmpxchg(&mut self, dest: &Value, ptr: &Operand, expected: &Operand,
                           desired: &Operand, ty: IrType, _success: AtomicOrdering,
                           _failure: AtomicOrdering, returns_bool: bool) {
        if self.is_atomic_wide(ty) {
            self.emit_atomic_cmpxchg_wide(dest, ptr, expected, desired, returns_bool);
            return;
        }

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
        if self.is_atomic_wide(ty) {
            self.emit_atomic_load_wide(dest, ptr);
            return;
        }

        self.operand_to_eax(ptr);
        let load_instr = self.mov_load_for_type(ty);
        emit!(self.state, "    {} (%eax), %eax", load_instr);
        self.state.reg_cache.invalidate_acc();
        self.store_eax_to(dest);
    }

    fn emit_atomic_store(&mut self, ptr: &Operand, val: &Operand, ty: IrType, ordering: AtomicOrdering) {
        if self.is_atomic_wide(ty) {
            self.emit_atomic_store_wide(ptr, val);
            if matches!(ordering, AtomicOrdering::SeqCst) {
                self.state.emit("    mfence");
            }
            return;
        }

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
        if matches!(ordering, AtomicOrdering::SeqCst) {
            self.state.emit("    mfence");
        }
    }

    fn emit_fence(&mut self, ordering: AtomicOrdering) {
        match ordering {
            AtomicOrdering::Relaxed => {} // no fence needed for relaxed
            _ => self.state.emit("    mfence"),
        }
    }

    // --- Inline asm ---

    fn emit_inline_asm(&mut self, template: &str, outputs: &[(String, Value, Option<String>)],
                       inputs: &[(String, Operand, Option<String>)], clobbers: &[String],
                       operand_types: &[IrType], goto_labels: &[(String, BlockId)],
                       input_symbols: &[Option<String>]) {
        crate::backend::inline_asm::emit_inline_asm_common(self, template, outputs, inputs, clobbers, operand_types, goto_labels, input_symbols);
        self.emit_callee_saved_clobber_annotations(clobbers);
        self.state.reg_cache.invalidate_all();
    }

    fn emit_intrinsic(&mut self, dest: &Option<Value>, op: &IntrinsicOp, dest_ptr: &Option<Value>, args: &[Operand]) {
        match op {
            // --- Memory fences (same x86 instructions as x86-64) ---
            IntrinsicOp::Lfence => { self.state.emit("    lfence"); }
            IntrinsicOp::Mfence => { self.state.emit("    mfence"); }
            IntrinsicOp::Sfence => { self.state.emit("    sfence"); }
            IntrinsicOp::Pause  => { self.state.emit("    pause"); }
            IntrinsicOp::Clflush => {
                self.operand_to_eax(&args[0]);
                self.state.emit("    clflush (%eax)");
            }

            // --- Non-temporal stores ---
            IntrinsicOp::Movnti => {
                if let Some(ptr) = dest_ptr {
                    self.operand_to_eax(&args[0]);
                    self.state.emit("    movl %eax, %ecx");
                    self.operand_to_eax(&Operand::Value(*ptr));
                    self.state.emit("    movnti %ecx, (%eax)");
                }
            }
            IntrinsicOp::Movnti64 => {
                // On i686, 64-bit non-temporal store needs two 32-bit movnti
                if let Some(ptr) = dest_ptr {
                    self.operand_to_eax(&Operand::Value(*ptr));
                    self.state.emit("    movl %eax, %ecx");
                    // Load the 64-bit value and store both halves
                    if let Operand::Value(v) = &args[0] {
                        if let Some(slot) = self.state.get_slot(v.0) {
                            emit!(self.state, "    movl {}(%ebp), %eax", slot.0);
                            self.state.emit("    movnti %eax, (%ecx)");
                            emit!(self.state, "    movl {}(%ebp), %eax", slot.0 + 4);
                            self.state.emit("    movnti %eax, 4(%ecx)");
                        } else {
                            // Register-allocated: store low 32 bits, zero high
                            self.operand_to_eax(&args[0]);
                            self.state.emit("    movnti %eax, (%ecx)");
                            self.state.emit("    xorl %eax, %eax");
                            self.state.emit("    movnti %eax, 4(%ecx)");
                        }
                    } else {
                        // Constant: store low 32 bits, zero high
                        self.operand_to_eax(&args[0]);
                        self.state.emit("    movnti %eax, (%ecx)");
                        self.state.emit("    xorl %eax, %eax");
                        self.state.emit("    movnti %eax, 4(%ecx)");
                    }
                }
            }
            IntrinsicOp::Movntdq => {
                if let Some(ptr) = dest_ptr {
                    self.operand_to_eax(&args[0]);
                    self.state.emit("    movdqu (%eax), %xmm0");
                    self.operand_to_eax(&Operand::Value(*ptr));
                    self.state.emit("    movntdq %xmm0, (%eax)");
                }
            }
            IntrinsicOp::Movntpd => {
                if let Some(ptr) = dest_ptr {
                    self.operand_to_eax(&args[0]);
                    self.state.emit("    movupd (%eax), %xmm0");
                    self.operand_to_eax(&Operand::Value(*ptr));
                    self.state.emit("    movntpd %xmm0, (%eax)");
                }
            }

            // --- SSE 128-bit load/store ---
            IntrinsicOp::Loaddqu => {
                if let Some(dptr) = dest_ptr {
                    self.operand_to_eax(&args[0]);
                    self.state.emit("    movdqu (%eax), %xmm0");
                    self.operand_to_eax(&Operand::Value(*dptr));
                    self.state.emit("    movdqu %xmm0, (%eax)");
                }
            }
            IntrinsicOp::Storedqu => {
                if let Some(ptr) = dest_ptr {
                    self.operand_to_eax(&args[0]);
                    self.state.emit("    movdqu (%eax), %xmm0");
                    self.operand_to_eax(&Operand::Value(*ptr));
                    self.state.emit("    movdqu %xmm0, (%eax)");
                }
            }

            // --- SSE 128-bit binary operations ---
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
                self.operand_to_eax(&args[0]);
                self.state.emit("    movdqu (%eax), %xmm0");
                self.state.emit("    pmovmskb %xmm0, %eax");
                self.state.reg_cache.invalidate_acc();
                if let Some(d) = dest {
                    self.store_eax_to(d);
                }
            }
            IntrinsicOp::SetEpi8 => {
                if let Some(dptr) = dest_ptr {
                    self.operand_to_eax(&args[0]);
                    self.state.emit("    movd %eax, %xmm0");
                    self.state.emit("    punpcklbw %xmm0, %xmm0");
                    self.state.emit("    punpcklwd %xmm0, %xmm0");
                    self.state.emit("    pshufd $0, %xmm0, %xmm0");
                    self.operand_to_eax(&Operand::Value(*dptr));
                    self.state.emit("    movdqu %xmm0, (%eax)");
                }
            }
            IntrinsicOp::SetEpi32 => {
                if let Some(dptr) = dest_ptr {
                    self.operand_to_eax(&args[0]);
                    self.state.emit("    movd %eax, %xmm0");
                    self.state.emit("    pshufd $0, %xmm0, %xmm0");
                    self.operand_to_eax(&Operand::Value(*dptr));
                    self.state.emit("    movdqu %xmm0, (%eax)");
                }
            }

            // --- CRC32 ---
            IntrinsicOp::Crc32_8 | IntrinsicOp::Crc32_16
            | IntrinsicOp::Crc32_32 => {
                self.operand_to_eax(&args[0]);
                self.state.emit("    movl %eax, %ecx");
                self.operand_to_eax(&args[1]);
                // Need accumulator in ecx and data in eax, but crc32 writes to first operand
                self.state.emit("    xchgl %eax, %ecx");
                let inst = match op {
                    IntrinsicOp::Crc32_8  => "crc32b %cl, %eax",
                    IntrinsicOp::Crc32_16 => "crc32w %cx, %eax",
                    IntrinsicOp::Crc32_32 => "crc32l %ecx, %eax",
                    _ => unreachable!(),
                };
                self.state.emit_fmt(format_args!("    {}", inst));
                self.state.reg_cache.invalidate_acc();
                if let Some(d) = dest {
                    self.store_eax_to(d);
                }
            }
            IntrinsicOp::Crc32_64 => {
                // On i686, there's no 64-bit CRC32 instruction; do two 32-bit CRC32s
                self.operand_to_eax(&args[0]);
                self.state.emit("    movl %eax, %edx");  // edx = accumulator
                // Load 64-bit operand low/high
                if let Operand::Value(v) = &args[1] {
                    if let Some(slot) = self.state.get_slot(v.0) {
                        emit!(self.state, "    movl {}(%ebp), %ecx", slot.0);
                        self.state.emit("    movl %edx, %eax");
                        self.state.emit("    crc32l %ecx, %eax");
                        emit!(self.state, "    movl {}(%ebp), %ecx", slot.0 + 4);
                        self.state.emit("    crc32l %ecx, %eax");
                    } else {
                        // Register-allocated: only 32 bits available, CRC32 on low word
                        self.operand_to_ecx(&args[1]);
                        self.state.emit("    movl %edx, %eax");
                        self.state.emit("    crc32l %ecx, %eax");
                    }
                } else {
                    // Constant operand: load low 32 bits and CRC32
                    self.operand_to_ecx(&args[1]);
                    self.state.emit("    movl %edx, %eax");
                    self.state.emit("    crc32l %ecx, %eax");
                }
                self.state.reg_cache.invalidate_acc();
                if let Some(d) = dest {
                    self.store_eax_to(d);
                }
            }

            // --- Frame and return address ---
            IntrinsicOp::FrameAddress => {
                self.state.emit("    movl %ebp, %eax");
                self.state.reg_cache.invalidate_acc();
                if let Some(d) = dest {
                    self.store_eax_to(d);
                }
            }
            IntrinsicOp::ReturnAddress => {
                // On i686, return address is at 4(%ebp) (32-bit stack frame)
                self.state.emit("    movl 4(%ebp), %eax");
                self.state.reg_cache.invalidate_acc();
                if let Some(d) = dest {
                    self.store_eax_to(d);
                }
            }

            // --- Floating-point intrinsics via x87 FPU ---
            IntrinsicOp::SqrtF64 => {
                // Load F64 to x87, compute sqrt, store back
                self.emit_f64_load_to_x87(&args[0]);
                self.state.emit("    fsqrt");
                if let Some(d) = dest {
                    // Store F64 result back to stack slot
                    if let Some(slot) = self.state.get_slot(d.0) {
                        emit!(self.state, "    fstpl {}(%ebp)", slot.0);
                    } else {
                        self.state.emit("    fstp %st(0)");
                    }
                } else {
                    self.state.emit("    fstp %st(0)");
                }
            }
            IntrinsicOp::SqrtF32 => {
                // Load F32 to x87, compute sqrt, store back as 32-bit
                match &args[0] {
                    Operand::Value(v) if self.state.get_slot(v.0).is_some() => {
                        let slot = self.state.get_slot(v.0).unwrap();
                        emit!(self.state, "    flds {}(%ebp)", slot.0);
                    }
                    Operand::Const(IrConst::F32(fval)) => {
                        emit!(self.state, "    movl ${}, %eax", fval.to_bits() as i32);
                        self.state.emit("    pushl %eax");
                        self.state.emit("    flds (%esp)");
                        self.state.emit("    addl $4, %esp");
                    }
                    _ => {
                        // Handles register-allocated values and other constants
                        self.operand_to_eax(&args[0]);
                        self.state.emit("    pushl %eax");
                        self.state.emit("    flds (%esp)");
                        self.state.emit("    addl $4, %esp");
                    }
                }
                self.state.emit("    fsqrt");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        emit!(self.state, "    fstps {}(%ebp)", slot.0);
                    } else {
                        self.state.emit("    fstp %st(0)");
                    }
                } else {
                    self.state.emit("    fstp %st(0)");
                }
            }
            IntrinsicOp::FabsF64 => {
                // Load F64 to x87, compute fabs, store back
                self.emit_f64_load_to_x87(&args[0]);
                self.state.emit("    fabs");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        emit!(self.state, "    fstpl {}(%ebp)", slot.0);
                    } else {
                        self.state.emit("    fstp %st(0)");
                    }
                } else {
                    self.state.emit("    fstp %st(0)");
                }
            }
            IntrinsicOp::FabsF32 => {
                // Load F32 to x87, compute fabs, store back as 32-bit
                match &args[0] {
                    Operand::Value(v) if self.state.get_slot(v.0).is_some() => {
                        let slot = self.state.get_slot(v.0).unwrap();
                        emit!(self.state, "    flds {}(%ebp)", slot.0);
                    }
                    Operand::Const(IrConst::F32(fval)) => {
                        emit!(self.state, "    movl ${}, %eax", fval.to_bits() as i32);
                        self.state.emit("    pushl %eax");
                        self.state.emit("    flds (%esp)");
                        self.state.emit("    addl $4, %esp");
                    }
                    _ => {
                        // Handles register-allocated values and other constants
                        self.operand_to_eax(&args[0]);
                        self.state.emit("    pushl %eax");
                        self.state.emit("    flds (%esp)");
                        self.state.emit("    addl $4, %esp");
                    }
                }
                self.state.emit("    fabs");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        emit!(self.state, "    fstps {}(%ebp)", slot.0);
                    } else {
                        self.state.emit("    fstp %st(0)");
                    }
                } else {
                    self.state.emit("    fstp %st(0)");
                }
            }

            // --- AES-NI (requires AES-NI extension, works on i686 processors that support it) ---
            IntrinsicOp::Aesenc128 | IntrinsicOp::Aesenclast128
            | IntrinsicOp::Aesdec128 | IntrinsicOp::Aesdeclast128 => {
                if let Some(dptr) = dest_ptr {
                    let inst = match op {
                        IntrinsicOp::Aesenc128 => "aesenc",
                        IntrinsicOp::Aesenclast128 => "aesenclast",
                        IntrinsicOp::Aesdec128 => "aesdec",
                        IntrinsicOp::Aesdeclast128 => "aesdeclast",
                        _ => unreachable!(),
                    };
                    self.emit_sse_binary_128(dptr, args, inst);
                }
            }
            IntrinsicOp::Aesimc128 => {
                if let Some(dptr) = dest_ptr {
                    self.operand_to_eax(&args[0]);
                    self.state.emit("    movdqu (%eax), %xmm0");
                    self.state.emit("    aesimc %xmm0, %xmm0");
                    self.operand_to_eax(&Operand::Value(*dptr));
                    self.state.emit("    movdqu %xmm0, (%eax)");
                }
            }
            IntrinsicOp::Aeskeygenassist128 => {
                if let Some(dptr) = dest_ptr {
                    self.operand_to_eax(&args[0]);
                    self.state.emit("    movdqu (%eax), %xmm0");
                    let imm = Self::operand_to_imm_i64(&args[1]);
                    self.state.emit_fmt(format_args!("    aeskeygenassist ${}, %xmm0, %xmm0", imm));
                    self.operand_to_eax(&Operand::Value(*dptr));
                    self.state.emit("    movdqu %xmm0, (%eax)");
                }
            }
            IntrinsicOp::Pclmulqdq128 => {
                if let Some(dptr) = dest_ptr {
                    self.operand_to_eax(&args[0]);
                    self.state.emit("    movdqu (%eax), %xmm0");
                    self.operand_to_eax(&args[1]);
                    self.state.emit("    movdqu (%eax), %xmm1");
                    let imm = Self::operand_to_imm_i64(&args[2]);
                    self.state.emit_fmt(format_args!("    pclmulqdq ${}, %xmm1, %xmm0", imm));
                    self.operand_to_eax(&Operand::Value(*dptr));
                    self.state.emit("    movdqu %xmm0, (%eax)");
                }
            }

            // --- SSE2 shift/shuffle operations ---
            IntrinsicOp::Pslldqi128 => {
                if let Some(dptr) = dest_ptr {
                    self.operand_to_eax(&args[0]);
                    self.state.emit("    movdqu (%eax), %xmm0");
                    let imm = Self::operand_to_imm_i64(&args[1]);
                    self.state.emit_fmt(format_args!("    pslldq ${}, %xmm0", imm));
                    self.operand_to_eax(&Operand::Value(*dptr));
                    self.state.emit("    movdqu %xmm0, (%eax)");
                }
            }
            IntrinsicOp::Psrldqi128 => {
                if let Some(dptr) = dest_ptr {
                    self.operand_to_eax(&args[0]);
                    self.state.emit("    movdqu (%eax), %xmm0");
                    let imm = Self::operand_to_imm_i64(&args[1]);
                    self.state.emit_fmt(format_args!("    psrldq ${}, %xmm0", imm));
                    self.operand_to_eax(&Operand::Value(*dptr));
                    self.state.emit("    movdqu %xmm0, (%eax)");
                }
            }
            IntrinsicOp::Psllqi128 => {
                if let Some(dptr) = dest_ptr {
                    self.operand_to_eax(&args[0]);
                    self.state.emit("    movdqu (%eax), %xmm0");
                    let imm = Self::operand_to_imm_i64(&args[1]);
                    self.state.emit_fmt(format_args!("    psllq ${}, %xmm0", imm));
                    self.operand_to_eax(&Operand::Value(*dptr));
                    self.state.emit("    movdqu %xmm0, (%eax)");
                }
            }
            IntrinsicOp::Psrlqi128 => {
                if let Some(dptr) = dest_ptr {
                    self.operand_to_eax(&args[0]);
                    self.state.emit("    movdqu (%eax), %xmm0");
                    let imm = Self::operand_to_imm_i64(&args[1]);
                    self.state.emit_fmt(format_args!("    psrlq ${}, %xmm0", imm));
                    self.operand_to_eax(&Operand::Value(*dptr));
                    self.state.emit("    movdqu %xmm0, (%eax)");
                }
            }
            IntrinsicOp::Pshufd128 => {
                if let Some(dptr) = dest_ptr {
                    self.operand_to_eax(&args[0]);
                    self.state.emit("    movdqu (%eax), %xmm0");
                    let imm = Self::operand_to_imm_i64(&args[1]);
                    self.state.emit_fmt(format_args!("    pshufd ${}, %xmm0, %xmm0", imm));
                    self.operand_to_eax(&Operand::Value(*dptr));
                    self.state.emit("    movdqu %xmm0, (%eax)");
                }
            }
            IntrinsicOp::Loadldi128 => {
                if let Some(dptr) = dest_ptr {
                    self.operand_to_eax(&args[0]);
                    self.state.emit("    movq (%eax), %xmm0");
                    self.operand_to_eax(&Operand::Value(*dptr));
                    self.state.emit("    movdqu %xmm0, (%eax)");
                }
            }
        }
        self.state.reg_cache.invalidate_acc();
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
            // Sign-extend smaller integer constants to 64-bit eax:edx pair.
            // IrConst::I8/I16/I32 store signed values; `as i64` sign-extends.
            Operand::Const(c) if matches!(c, IrConst::I8(_) | IrConst::I16(_) | IrConst::I32(_)) => {
                if let Some(ext) = c.to_i64() {
                    let low = (ext & 0xFFFFFFFF) as i32;
                    let high = ((ext >> 32) & 0xFFFFFFFF) as i32;
                    emit!(self.state, "    movl ${}, %eax", low);
                    emit!(self.state, "    movl ${}, %edx", high);
                }
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

    fn emit_i128_to_float_call(&mut self, src: &Operand, from_signed: bool, to_ty: IrType) {
        // I64/U64 -> F32/F64 conversion on i686
        // Load the 64-bit value into eax:edx, push onto stack, use x87 fildq
        self.emit_load_acc_pair(src);
        if from_signed {
            // Signed I64 -> float: push as 64-bit, use fildq
            self.state.emit("    pushl %edx");      // high 32 bits
            self.state.emit("    pushl %eax");       // low 32 bits
            self.state.emit("    fildq (%esp)");     // load as signed 64-bit int
            if to_ty == IrType::F32 {
                self.state.emit("    fstps (%esp)");
                self.state.emit("    movl (%esp), %eax");
            } else {
                self.state.emit("    fstpl (%esp)");
                self.state.emit("    movl (%esp), %eax");
            }
            self.state.emit("    addl $8, %esp");
        } else {
            // Unsigned U64 -> float: need special handling for values >= 2^63
            // because fildq treats the value as signed
            let label_id = self.state.next_label_id();
            let big_label = format!(".Lu64_to_f_big_{}", label_id);
            let done_label = format!(".Lu64_to_f_done_{}", label_id);

            self.state.emit("    pushl %edx");
            self.state.emit("    pushl %eax");
            // Check if high bit of edx is set (value >= 2^63)
            self.state.emit("    testl %edx, %edx");
            emit!(self.state, "    js {}", big_label);
            // Value < 2^63: fildq works directly
            self.state.emit("    fildq (%esp)");
            emit!(self.state, "    jmp {}", done_label);
            // Value >= 2^63: convert as (value/2) then multiply by 2
            emit!(self.state, "{}:", big_label);
            // Shift right by 1, preserving the low bit
            self.state.emit("    movl (%esp), %eax");
            self.state.emit("    movl 4(%esp), %edx");
            self.state.emit("    shrl $1, %eax");          // logical shift low
            // Set bit 31 of eax from bit 0 of edx (the carry)
            self.state.emit("    movl %edx, %ecx");
            self.state.emit("    shrl $1, %edx");
            self.state.emit("    andl $1, %ecx");
            self.state.emit("    shll $31, %ecx");
            self.state.emit("    orl %ecx, %eax");
            // Round: if original low bit was set, add 1 before halving
            // (we handle this by OR-ing the original low bit into the halved value)
            self.state.emit("    movl %eax, (%esp)");
            self.state.emit("    movl %edx, 4(%esp)");
            self.state.emit("    fildq (%esp)");
            self.state.emit("    fadd %st(0), %st(0)");    // multiply by 2
            emit!(self.state, "{}:", done_label);
            if to_ty == IrType::F32 {
                self.state.emit("    fstps (%esp)");
                self.state.emit("    movl (%esp), %eax");
            } else {
                self.state.emit("    fstpl (%esp)");
                self.state.emit("    movl (%esp), %eax");
            }
            self.state.emit("    addl $8, %esp");
        }
    }

    fn emit_float_to_i128_call(&mut self, src: &Operand, to_signed: bool, from_ty: IrType) {
        // F32/F64 -> I64/U64 conversion on i686
        // Use x87 fisttpq (truncate-toward-zero) to convert float to 64-bit integer
        self.operand_to_eax(src);
        self.state.emit("    subl $8, %esp");
        if from_ty == IrType::F32 {
            self.state.emit("    movl %eax, (%esp)");
            self.state.emit("    flds (%esp)");
        } else {
            // F64: only low 32 bits in eax; store and load as F32 (lossy fallback)
            // TODO: proper F64 -> I64 requires full 64-bit F64 value
            self.state.emit("    movl %eax, (%esp)");
            self.state.emit("    flds (%esp)");
        }
        if to_signed {
            self.state.emit("    fisttpq (%esp)");
        } else {
            // For unsigned, fisttpq gives signed result; need special handling for large values
            // For now, use fisttpq (works for values < 2^63)
            self.state.emit("    fisttpq (%esp)");
        }
        self.state.emit("    movl (%esp), %eax");
        self.state.emit("    movl 4(%esp), %edx");
        self.state.emit("    addl $8, %esp");
    }

    fn emit_i128_prep_binop(&mut self, lhs: &Operand, rhs: &Operand) {
        // Load lhs into eax:edx, rhs onto stack
        self.emit_load_acc_pair(rhs);
        self.state.emit("    pushl %edx");
        self.state.emit("    pushl %eax");
        self.emit_load_acc_pair(lhs);
    }

    fn emit_i128_prep_shift_lhs(&mut self, lhs: &Operand) {
        // For constant-amount shifts, only load LHS into eax:edx.
        // Do NOT push a dummy RHS onto the stack — the const shift functions
        // don't pop it, which would cause a stack leak.
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
        // 64-bit multiply: (eax:edx) * ((%esp):4(%esp))
        // result_lo = a_lo * b_lo (low 32 bits)
        // result_hi = a_hi * b_lo + a_lo * b_hi + high32(a_lo * b_lo)
        //
        // We have lhs in eax:edx, rhs at (%esp):4(%esp)
        // Save a_hi (edx) to ecx so we can use edx for mull result
        self.state.emit("    movl %edx, %ecx");       // ecx = a_hi
        self.state.emit("    imull (%esp), %ecx");      // ecx = a_hi * b_lo (low 32 bits)
        self.state.emit("    movl %eax, %edx");        // edx = a_lo (save)
        self.state.emit("    imull 4(%esp), %edx");     // edx = a_lo * b_hi (low 32 bits)
        self.state.emit("    addl %edx, %ecx");        // ecx = a_hi*b_lo + a_lo*b_hi
        self.state.emit("    mull (%esp)");             // edx:eax = a_lo * b_lo (full 64-bit)
        self.state.emit("    addl %ecx, %edx");        // edx += cross terms
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
        // 64-bit left shift: eax:edx <<= (%esp)
        // When shift >= 32, low word becomes 0 and high = low << (shift-32)
        let label_id = self.state.next_label_id();
        let done_label = format!(".Lshl64_done_{}", label_id);
        self.state.emit("    movl (%esp), %ecx");
        self.state.emit("    addl $8, %esp");
        self.state.emit("    shldl %cl, %eax, %edx");
        self.state.emit("    shll %cl, %eax");
        self.state.emit("    testb $32, %cl");
        emit!(self.state, "    je {}", done_label);
        // shift >= 32: high = low, low = 0
        self.state.emit("    movl %eax, %edx");
        self.state.emit("    xorl %eax, %eax");
        emit!(self.state, "{}:", done_label);
    }

    fn emit_i128_lshr(&mut self) {
        // 64-bit logical right shift: eax:edx >>= (%esp)
        let label_id = self.state.next_label_id();
        let done_label = format!(".Llshr64_done_{}", label_id);
        self.state.emit("    movl (%esp), %ecx");
        self.state.emit("    addl $8, %esp");
        self.state.emit("    shrdl %cl, %edx, %eax");
        self.state.emit("    shrl %cl, %edx");
        self.state.emit("    testb $32, %cl");
        emit!(self.state, "    je {}", done_label);
        // shift >= 32: low = high, high = 0
        self.state.emit("    movl %edx, %eax");
        self.state.emit("    xorl %edx, %edx");
        emit!(self.state, "{}:", done_label);
    }

    fn emit_i128_ashr(&mut self) {
        // 64-bit arithmetic right shift: eax:edx >>= (%esp)
        let label_id = self.state.next_label_id();
        let done_label = format!(".Lashr64_done_{}", label_id);
        self.state.emit("    movl (%esp), %ecx");
        self.state.emit("    addl $8, %esp");
        self.state.emit("    shrdl %cl, %edx, %eax");
        self.state.emit("    sarl %cl, %edx");
        self.state.emit("    testb $32, %cl");
        emit!(self.state, "    je {}", done_label);
        // shift >= 32: low = sign-extended high, high = sign fill
        self.state.emit("    movl %edx, %eax");
        self.state.emit("    sarl $31, %edx");
        emit!(self.state, "{}:", done_label);
    }

    fn emit_i128_divrem_call(&mut self, func_name: &str, lhs: &Operand, rhs: &Operand) {
        // On i686, 64-bit division uses libgcc functions:
        //   __divdi3(i64, i64) -> i64     (signed div)
        //   __udivdi3(u64, u64) -> u64    (unsigned div)
        //   __moddi3(i64, i64) -> i64     (signed mod)
        //   __umoddi3(u64, u64) -> u64    (unsigned mod)
        //
        // Map the 128-bit names to 64-bit names on i686:
        let di_func = match func_name {
            "__divti3" => "__divdi3",
            "__udivti3" => "__udivdi3",
            "__modti3" => "__moddi3",
            "__umodti3" => "__umoddi3",
            _ => func_name,
        };

        // cdecl: push rhs (high then low), then lhs (high then low)
        // Stack layout for call: [lhs_lo, lhs_hi, rhs_lo, rhs_hi]
        self.emit_load_acc_pair(rhs);
        self.state.emit("    pushl %edx");  // rhs_hi
        self.state.emit("    pushl %eax");  // rhs_lo
        self.emit_load_acc_pair(lhs);
        self.state.emit("    pushl %edx");  // lhs_hi
        self.state.emit("    pushl %eax");  // lhs_lo
        emit!(self.state, "    call {}", di_func);
        self.state.emit("    addl $16, %esp");
        // Result is in eax:edx (64-bit return value)
    }

    fn emit_i128_store_result(&mut self, dest: &Value) {
        self.emit_store_acc_pair(dest);
    }

    fn emit_i128_shl_const(&mut self, amount: u32) {
        if amount == 0 { return; }
        if amount >= 64 {
            // Shifting 64-bit value left by >= 64: result is zero
            self.state.emit("    xorl %eax, %eax");
            self.state.emit("    xorl %edx, %edx");
        } else if amount >= 32 {
            self.state.emit("    movl %eax, %edx");
            self.state.emit("    xorl %eax, %eax");
            if amount > 32 {
                emit!(self.state, "    shll ${}, %edx", amount - 32);
            }
        } else {
            emit!(self.state, "    shldl ${}, %eax, %edx", amount);
            emit!(self.state, "    shll ${}, %eax", amount);
        }
    }

    fn emit_i128_lshr_const(&mut self, amount: u32) {
        if amount == 0 { return; }
        if amount >= 64 {
            // Shifting 64-bit value right by >= 64: result is zero
            self.state.emit("    xorl %eax, %eax");
            self.state.emit("    xorl %edx, %edx");
        } else if amount >= 32 {
            self.state.emit("    movl %edx, %eax");
            self.state.emit("    xorl %edx, %edx");
            if amount > 32 {
                emit!(self.state, "    shrl ${}, %eax", amount - 32);
            }
        } else {
            emit!(self.state, "    shrdl ${}, %edx, %eax", amount);
            emit!(self.state, "    shrl ${}, %edx", amount);
        }
    }

    fn emit_i128_ashr_const(&mut self, amount: u32) {
        if amount == 0 { return; }
        if amount >= 64 {
            // Arithmetic right shift by >= 64: result is sign-extended
            self.state.emit("    sarl $31, %edx");
            self.state.emit("    movl %edx, %eax");
        } else if amount >= 32 {
            self.state.emit("    movl %edx, %eax");
            self.state.emit("    sarl $31, %edx");
            if amount > 32 {
                emit!(self.state, "    sarl ${}, %eax", amount - 32);
            }
        } else {
            emit!(self.state, "    shrdl ${}, %edx, %eax", amount);
            emit!(self.state, "    sarl ${}, %edx", amount);
        }
    }

    fn emit_i128_cmp_eq(&mut self, is_ne: bool) {
        // Compare eax:edx (lhs) with (%esp):4(%esp) (rhs)
        // eq: (low_eq AND high_eq)
        // ne: !(low_eq AND high_eq) = (!low_eq OR !high_eq)
        self.state.emit("    cmpl (%esp), %eax");
        self.state.emit("    sete %al");
        self.state.emit("    cmpl 4(%esp), %edx");
        self.state.emit("    sete %cl");
        self.state.emit("    andb %cl, %al");  // al = both halves equal
        if is_ne {
            self.state.emit("    xorb $1, %al"); // flip for ne
        }
        self.state.emit("    movzbl %al, %eax");
        self.state.emit("    addl $8, %esp");
    }

    fn emit_i128_cmp_ordered(&mut self, op: IrCmpOp) {
        // Compare 64-bit values in eax:edx (lhs) vs (%esp):4(%esp) (rhs).
        // For signed comparisons (Slt/Sle/Sgt/Sge):
        //   - High 32-bit words are compared as SIGNED
        //   - Low 32-bit words (when high words equal) are compared as UNSIGNED
        // For unsigned comparisons (Ult/Ule/Ugt/Uge):
        //   - Both high and low words are compared as UNSIGNED
        let is_signed = matches!(op, IrCmpOp::Slt | IrCmpOp::Sle | IrCmpOp::Sgt | IrCmpOp::Sge);

        if is_signed {
            // Signed 64-bit comparison: need separate paths for high-word
            // decided vs high-words-equal cases, because the high words use
            // signed comparison but the low words use unsigned comparison.
            let label_id = self.state.next_label_id();
            let label_hi_decided = format!(".Li128_hidec_{}", label_id);
            let label_done = format!(".Li128_done_{}", label_id);

            // Compare high 32 bits (signed)
            self.state.emit("    cmpl 4(%esp), %edx");
            emit!(self.state, "    jne {}", label_hi_decided);

            // High words equal: compare low 32 bits as UNSIGNED
            self.state.emit("    cmpl (%esp), %eax");
            let low_set = match op {
                IrCmpOp::Slt => "setb",   // unsigned below
                IrCmpOp::Sle => "setbe",  // unsigned below or equal
                IrCmpOp::Sgt => "seta",   // unsigned above
                IrCmpOp::Sge => "setae",  // unsigned above or equal
                _ => unreachable!(),
            };
            emit!(self.state, "    {} %al", low_set);
            emit!(self.state, "    jmp {}", label_done);

            // High words differ: use signed comparison result from high words
            emit!(self.state, "{}:", label_hi_decided);
            let high_set = match op {
                IrCmpOp::Slt => "setl",
                IrCmpOp::Sle => "setl",   // if high < high, result is true (<=)
                IrCmpOp::Sgt => "setg",
                IrCmpOp::Sge => "setg",   // if high > high, result is true (>=)
                _ => unreachable!(),
            };
            emit!(self.state, "    {} %al", high_set);

            emit!(self.state, "{}:", label_done);
        } else {
            // Unsigned 64-bit comparison: both high and low use unsigned flags,
            // so we can use the simple fall-through approach.
            let label_id = self.state.next_label_id();
            let high_decided = format!(".Li128_high_{}", label_id);

            self.state.emit("    cmpl 4(%esp), %edx");
            emit!(self.state, "    jne {}", high_decided);
            self.state.emit("    cmpl (%esp), %eax");
            emit!(self.state, "{}:", high_decided);

            let set_instr = match op {
                IrCmpOp::Ult => "setb",
                IrCmpOp::Ule => "setbe",
                IrCmpOp::Ugt => "seta",
                IrCmpOp::Uge => "setae",
                _ => unreachable!(),
            };
            emit!(self.state, "    {} %al", set_instr);
        }
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
            self.state.f128_direct_slots.insert(dest.0);
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
