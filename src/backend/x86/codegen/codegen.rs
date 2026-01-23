use crate::ir::ir::*;
use crate::common::types::IrType;
use crate::backend::common::PtrDirective;
use crate::backend::codegen_shared::*;

/// x86-64 code generator. Implements the ArchCodegen trait for the shared framework.
/// Uses System V AMD64 ABI with stack-based allocation (no register allocator yet).
pub struct X86Codegen {
    state: CodegenState,
    current_return_type: IrType,
}

impl X86Codegen {
    pub fn new() -> Self {
        Self { state: CodegenState::new(), current_return_type: IrType::I64 }
    }

    pub fn generate(mut self, module: &IrModule) -> String {
        generate_module(&mut self, module)
    }

    // --- x86 helper methods ---

    /// Load an operand into %rax.
    fn operand_to_rax(&mut self, op: &Operand) {
        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::I8(v) => self.state.emit(&format!("    movq ${}, %rax", *v as i64)),
                    IrConst::I16(v) => self.state.emit(&format!("    movq ${}, %rax", *v as i64)),
                    IrConst::I32(v) => self.state.emit(&format!("    movq ${}, %rax", *v as i64)),
                    IrConst::I64(v) => {
                        if *v >= i32::MIN as i64 && *v <= i32::MAX as i64 {
                            self.state.emit(&format!("    movq ${}, %rax", v));
                        } else {
                            self.state.emit(&format!("    movabsq ${}, %rax", v));
                        }
                    }
                    IrConst::F32(v) => {
                        let bits = v.to_bits() as u64;
                        self.state.emit(&format!("    movq ${}, %rax", bits as i64));
                    }
                    IrConst::F64(v) => {
                        let bits = v.to_bits();
                        if bits == 0 {
                            self.state.emit("    xorq %rax, %rax");
                        } else {
                            self.state.emit(&format!("    movabsq ${}, %rax", bits as i64));
                        }
                    }
                    IrConst::Zero => self.state.emit("    xorq %rax, %rax"),
                }
            }
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    if self.state.is_alloca(v.0) {
                        self.state.emit(&format!("    leaq {}(%rbp), %rax", slot.0));
                    } else {
                        self.state.emit(&format!("    movq {}(%rbp), %rax", slot.0));
                    }
                } else {
                    self.state.emit("    xorq %rax, %rax");
                }
            }
        }
    }

    /// Store %rax to a value's stack slot.
    fn store_rax_to(&mut self, dest: &Value) {
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.state.emit(&format!("    movq %rax, {}(%rbp)", slot.0));
        }
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

    /// Emit a type cast instruction sequence.
    fn emit_cast_instrs(&mut self, from_ty: IrType, to_ty: IrType) {
        if from_ty == to_ty {
            return;
        }

        // Ptr is equivalent to I64/U64 on x86-64 (both 8 bytes, no conversion needed)
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
            // For unsigned targets, we need special handling for large values
            let is_unsigned_dest = to_ty.is_unsigned() || to_ty == IrType::Ptr;
            if from_ty == IrType::F64 {
                self.state.emit("    movq %rax, %xmm0");
                if is_unsigned_dest && (to_ty == IrType::U64 || to_ty == IrType::Ptr) {
                    // For U64: handle values >= 2^63 that overflow signed conversion
                    // If value < 2^63, use normal signed conversion
                    // If value >= 2^63, subtract 2^63, convert, then add 2^63 as integer
                    let label_id = self.state.next_label_id();
                    let big_label = format!(".Lf2u_big_{}", label_id);
                    let done_label = format!(".Lf2u_done_{}", label_id);
                    // Load 2^63 as double (0x43E0000000000000)
                    self.state.emit("    movabsq $4890909195324358656, %rcx"); // 2^63 as double bits
                    self.state.emit("    movq %rcx, %xmm1");
                    self.state.emit("    ucomisd %xmm1, %xmm0");
                    self.state.emit(&format!("    jae {}", big_label));
                    // Small path: value < 2^63, normal signed conversion works
                    self.state.emit("    cvttsd2siq %xmm0, %rax");
                    self.state.emit(&format!("    jmp {}", done_label));
                    // Big path: value >= 2^63
                    self.state.emit(&format!("{}:", big_label));
                    self.state.emit("    subsd %xmm1, %xmm0");
                    self.state.emit("    cvttsd2siq %xmm0, %rax");
                    self.state.emit("    movabsq $9223372036854775808, %rcx"); // 2^63 as integer
                    self.state.emit("    addq %rcx, %rax");
                    self.state.emit(&format!("{}:", done_label));
                } else {
                    self.state.emit("    cvttsd2siq %xmm0, %rax");
                }
            } else {
                self.state.emit("    movd %eax, %xmm0");
                self.state.emit("    cvttss2siq %xmm0, %rax");
            }
            return;
        }

        // Int-to-float cast
        if !from_ty.is_float() && to_ty.is_float() {
            let is_unsigned_src = from_ty.is_unsigned();
            if to_ty == IrType::F64 {
                if is_unsigned_src && (from_ty == IrType::U64) {
                    // For U64 -> F64: handle values >= 2^63
                    let label_id = self.state.next_label_id();
                    let big_label = format!(".Lu2f_big_{}", label_id);
                    let done_label = format!(".Lu2f_done_{}", label_id);
                    self.state.emit("    testq %rax, %rax");
                    self.state.emit(&format!("    js {}", big_label));
                    // Small path: value < 2^63, fits in signed i64
                    self.state.emit("    cvtsi2sdq %rax, %xmm0");
                    self.state.emit(&format!("    jmp {}", done_label));
                    // Big path: value >= 2^63
                    self.state.emit(&format!("{}:", big_label));
                    // Shift right by 1, preserving the low bit for rounding
                    self.state.emit("    movq %rax, %rcx");
                    self.state.emit("    shrq $1, %rax");
                    self.state.emit("    andq $1, %rcx");
                    self.state.emit("    orq %rcx, %rax");
                    self.state.emit("    cvtsi2sdq %rax, %xmm0");
                    self.state.emit("    addsd %xmm0, %xmm0");
                    self.state.emit(&format!("{}:", done_label));
                    self.state.emit("    movq %xmm0, %rax");
                } else {
                    // Signed or U32 (zero-extended, so always fits in i64)
                    self.state.emit("    cvtsi2sdq %rax, %xmm0");
                    self.state.emit("    movq %xmm0, %rax");
                }
            } else {
                if is_unsigned_src && (from_ty == IrType::U64) {
                    // For U64 -> F32: same strategy as F64 but with single-precision
                    let label_id = self.state.next_label_id();
                    let big_label = format!(".Lu2f_big_{}", label_id);
                    let done_label = format!(".Lu2f_done_{}", label_id);
                    self.state.emit("    testq %rax, %rax");
                    self.state.emit(&format!("    js {}", big_label));
                    self.state.emit("    cvtsi2ssq %rax, %xmm0");
                    self.state.emit(&format!("    jmp {}", done_label));
                    self.state.emit(&format!("{}:", big_label));
                    self.state.emit("    movq %rax, %rcx");
                    self.state.emit("    shrq $1, %rax");
                    self.state.emit("    andq $1, %rcx");
                    self.state.emit("    orq %rcx, %rax");
                    self.state.emit("    cvtsi2ssq %rax, %xmm0");
                    self.state.emit("    addss %xmm0, %xmm0");
                    self.state.emit(&format!("{}:", done_label));
                    self.state.emit("    movd %xmm0, %eax");
                } else {
                    self.state.emit("    cvtsi2ssq %rax, %xmm0");
                    self.state.emit("    movd %xmm0, %eax");
                }
            }
            return;
        }

        // Float-to-float cast (F32 <-> F64)
        if from_ty.is_float() && to_ty.is_float() {
            if from_ty == IrType::F32 && to_ty == IrType::F64 {
                self.state.emit("    movd %eax, %xmm0");
                self.state.emit("    cvtss2sd %xmm0, %xmm0");
                self.state.emit("    movq %xmm0, %rax");
            } else {
                self.state.emit("    movq %rax, %xmm0");
                self.state.emit("    cvtsd2ss %xmm0, %xmm0");
                self.state.emit("    movd %xmm0, %eax");
            }
            return;
        }

        // At this point, only integer types remain (Ptr and float handled above)
        if from_ty.size() == to_ty.size() {
            return;
        }

        let from_size = from_ty.size();
        let to_size = to_ty.size();

        if to_size > from_size {
            if from_ty.is_unsigned() {
                match from_ty {
                    IrType::U8 => self.state.emit("    movzbq %al, %rax"),
                    IrType::U16 => self.state.emit("    movzwq %ax, %rax"),
                    IrType::U32 => self.state.emit("    movl %eax, %eax"),
                    _ => {}
                }
            } else if to_ty == IrType::U32 {
                // Sign-extend to 32-bit (which clears upper 32 bits on x86-64)
                match from_ty {
                    IrType::I8 => self.state.emit("    movsbl %al, %eax"),
                    IrType::I16 => self.state.emit("    movswl %ax, %eax"),
                    _ => {}
                }
            } else {
                // Sign-extend to 64-bit
                match from_ty {
                    IrType::I8 => self.state.emit("    movsbq %al, %rax"),
                    IrType::I16 => self.state.emit("    movswq %ax, %rax"),
                    IrType::I32 => self.state.emit("    cltq"),
                    _ => {}
                }
            }
        } else {
            // Narrowing: truncate then sign/zero-extend back to 64-bit
            // so that the full 64-bit register has the correct narrowed value.
            // This is necessary because we store all values as 64-bit movq.
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

const X86_ARG_REGS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

impl ArchCodegen for X86Codegen {
    fn state(&mut self) -> &mut CodegenState { &mut self.state }
    fn state_ref(&self) -> &CodegenState { &self.state }
    fn ptr_directive(&self) -> PtrDirective { PtrDirective::Quad }

    fn calculate_stack_space(&mut self, func: &IrFunction) -> i64 {
        calculate_stack_space_common(&mut self.state, func, 0, |space, alloc_size| {
            // x86 uses negative offsets from rbp
            let new_space = space + ((alloc_size + 7) & !7);
            (-new_space, new_space)
        })
    }

    fn aligned_frame_size(&self, raw_space: i64) -> i64 {
        if raw_space > 0 { (raw_space + 15) & !15 } else { 0 }
    }

    fn emit_prologue(&mut self, func: &IrFunction, frame_size: i64) {
        self.current_return_type = func.return_type;
        self.state.emit("    pushq %rbp");
        self.state.emit("    movq %rsp, %rbp");
        if frame_size > 0 {
            self.state.emit(&format!("    subq ${}, %rsp", frame_size));
        }
    }

    fn emit_epilogue(&mut self, _frame_size: i64) {
        self.state.emit("    movq %rbp, %rsp");
        self.state.emit("    popq %rbp");
    }

    fn emit_store_params(&mut self, func: &IrFunction) {
        let xmm_regs = ["xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"];
        let mut int_reg_idx = 0usize;
        let mut float_reg_idx = 0usize;
        // Stack-passed parameters start at 16(%rbp) (after saved rbp + return addr)
        let mut stack_param_offset: i64 = 16;
        for (_i, param) in func.params.iter().enumerate() {
            let is_float = param.ty.is_float();
            let is_stack_passed = if is_float { float_reg_idx >= 8 } else { int_reg_idx >= 6 };

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
                        // Stack-passed parameter: copy from positive rbp offset to local slot
                        self.state.emit(&format!("    movq {}(%rbp), %rax", stack_param_offset));
                        let store_instr = Self::mov_store_for_type(ty);
                        let reg = Self::reg_for_type("rax", ty);
                        self.state.emit(&format!("    {} %{}, {}(%rbp)", store_instr, reg, slot.0));
                        stack_param_offset += 8;
                    } else if is_float {
                        // Float params arrive in xmm0-xmm7 per SysV ABI
                        if ty == IrType::F32 {
                            // Store F32: movd to eax (zero-extends to rax), then movq to slot
                            self.state.emit(&format!("    movd %{}, %eax", xmm_regs[float_reg_idx]));
                            self.state.emit(&format!("    movq %rax, {}(%rbp)", slot.0));
                        } else {
                            self.state.emit(&format!("    movq %{}, {}(%rbp)",
                                xmm_regs[float_reg_idx], slot.0));
                        }
                        float_reg_idx += 1;
                    } else {
                        let store_instr = Self::mov_store_for_type(ty);
                        let reg = Self::reg_for_type(X86_ARG_REGS[int_reg_idx], ty);
                        self.state.emit(&format!("    {} %{}, {}(%rbp)", store_instr, reg, slot.0));
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
        self.operand_to_rax(op);
    }

    fn emit_store_result(&mut self, dest: &Value) {
        self.store_rax_to(dest);
    }

    fn emit_store(&mut self, val: &Operand, ptr: &Value, ty: IrType) {
        self.operand_to_rax(val);
        if let Some(slot) = self.state.get_slot(ptr.0) {
            if self.state.is_alloca(ptr.0) {
                let store_instr = Self::mov_store_for_type(ty);
                let reg = Self::reg_for_type("rax", ty);
                self.state.emit(&format!("    {} %{}, {}(%rbp)", store_instr, reg, slot.0));
            } else {
                let store_instr = Self::mov_store_for_type(ty);
                self.state.emit("    movq %rax, %rcx");
                self.state.emit(&format!("    movq {}(%rbp), %rdx", slot.0));
                let store_reg = Self::reg_for_type("rcx", ty);
                self.state.emit(&format!("    {} %{}, (%rdx)", store_instr, store_reg));
            }
        }
    }

    fn emit_load(&mut self, dest: &Value, ptr: &Value, ty: IrType) {
        if let Some(slot) = self.state.get_slot(ptr.0) {
            let load_instr = Self::mov_load_for_type(ty);
            let dest_reg = Self::load_dest_reg(ty);
            if self.state.is_alloca(ptr.0) {
                self.state.emit(&format!("    {} {}(%rbp), {}", load_instr, slot.0, dest_reg));
            } else {
                self.state.emit(&format!("    movq {}(%rbp), %rax", slot.0));
                self.state.emit(&format!("    {} (%rax), {}", load_instr, dest_reg));
            }
            self.store_rax_to(dest);
        }
    }

    fn emit_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if ty.is_float() {
            // Float binary operation using SSE
            let (mov_to_xmm, mov_from_xmm) = if ty == IrType::F32 {
                ("movd %eax, %xmm0", "movd %xmm0, %eax")
            } else {
                ("movq %rax, %xmm0", "movq %xmm0, %rax")
            };
            let mov_to_xmm1 = if ty == IrType::F32 { "movd %eax, %xmm1" } else { "movq %rax, %xmm1" };
            self.operand_to_rax(lhs);
            self.state.emit(&format!("    {}", mov_to_xmm));
            self.state.emit("    pushq %rax");
            self.operand_to_rax(rhs);
            self.state.emit(&format!("    {}", mov_to_xmm1));
            self.state.emit("    popq %rax"); // balance stack
            let (add, sub, mul, div) = if ty == IrType::F64 {
                ("addsd", "subsd", "mulsd", "divsd")
            } else {
                ("addss", "subss", "mulss", "divss")
            };
            match op {
                IrBinOp::Add => self.state.emit(&format!("    {} %xmm1, %xmm0", add)),
                IrBinOp::Sub => self.state.emit(&format!("    {} %xmm1, %xmm0", sub)),
                IrBinOp::Mul => self.state.emit(&format!("    {} %xmm1, %xmm0", mul)),
                IrBinOp::SDiv | IrBinOp::UDiv => self.state.emit(&format!("    {} %xmm1, %xmm0", div)),
                _ => self.state.emit(&format!("    {} %xmm1, %xmm0", add)),
            }
            self.state.emit(&format!("    {}", mov_from_xmm));
            self.store_rax_to(dest);
            return;
        }

        self.operand_to_rax(lhs);
        self.state.emit("    pushq %rax");
        self.operand_to_rax(rhs);
        self.state.emit("    movq %rax, %rcx");
        self.state.emit("    popq %rax");

        let use_32bit = ty == IrType::I32 || ty == IrType::U32;
        let is_unsigned = ty.is_unsigned();

        match op {
            IrBinOp::Add => {
                if use_32bit {
                    self.state.emit("    addl %ecx, %eax");
                    if !is_unsigned { self.state.emit("    cltq"); }
                } else {
                    self.state.emit("    addq %rcx, %rax");
                }
            }
            IrBinOp::Sub => {
                if use_32bit {
                    self.state.emit("    subl %ecx, %eax");
                    if !is_unsigned { self.state.emit("    cltq"); }
                } else {
                    self.state.emit("    subq %rcx, %rax");
                }
            }
            IrBinOp::Mul => {
                if use_32bit {
                    self.state.emit("    imull %ecx, %eax");
                    if !is_unsigned { self.state.emit("    cltq"); }
                } else {
                    self.state.emit("    imulq %rcx, %rax");
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
            IrBinOp::Shl => self.state.emit("    shlq %cl, %rax"),
            IrBinOp::AShr => self.state.emit("    sarq %cl, %rax"),
            IrBinOp::LShr => self.state.emit("    shrq %cl, %rax"),
        }

        self.store_rax_to(dest);
    }

    fn emit_unaryop(&mut self, dest: &Value, op: IrUnaryOp, src: &Operand, ty: IrType) {
        self.operand_to_rax(src);
        if ty.is_float() {
            match op {
                IrUnaryOp::Neg => {
                    if ty == IrType::F32 {
                        // F32: XOR the 32-bit sign bit (bit 31)
                        self.state.emit("    movd %eax, %xmm0");
                        self.state.emit("    movl $0x80000000, %ecx");
                        self.state.emit("    movd %ecx, %xmm1");
                        self.state.emit("    xorps %xmm1, %xmm0");
                        self.state.emit("    movd %xmm0, %eax");
                    } else {
                        // F64: XOR the 64-bit sign bit (bit 63)
                        self.state.emit("    movq %rax, %xmm0");
                        self.state.emit("    movabsq $-9223372036854775808, %rcx"); // 0x8000000000000000
                        self.state.emit("    movq %rcx, %xmm1");
                        self.state.emit("    xorpd %xmm1, %xmm0");
                        self.state.emit("    movq %xmm0, %rax");
                    }
                }
                IrUnaryOp::Not => self.state.emit("    notq %rax"),
            }
        } else {
            match op {
                IrUnaryOp::Neg => self.state.emit("    negq %rax"),
                IrUnaryOp::Not => self.state.emit("    notq %rax"),
            }
        }
        self.store_rax_to(dest);
    }

    fn emit_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if ty.is_float() {
            // Float comparison using SSE
            let (mov_to_xmm0, mov_to_xmm1) = if ty == IrType::F32 {
                ("movd %eax, %xmm0", "movd %eax, %xmm1")
            } else {
                ("movq %rax, %xmm0", "movq %rax, %xmm1")
            };
            self.operand_to_rax(lhs);
            self.state.emit(&format!("    {}", mov_to_xmm0));
            self.state.emit("    pushq %rax");
            self.operand_to_rax(rhs);
            self.state.emit(&format!("    {}", mov_to_xmm1));
            self.state.emit("    popq %rax");
            if ty == IrType::F64 {
                self.state.emit("    ucomisd %xmm1, %xmm0");
            } else {
                self.state.emit("    ucomiss %xmm1, %xmm0");
            }
            // For float: use unsigned-style setcc (above/below) since ucomisd sets CF/ZF
            let set_instr = match op {
                IrCmpOp::Eq => "sete",
                IrCmpOp::Ne => "setne",
                IrCmpOp::Slt | IrCmpOp::Ult => "setb",
                IrCmpOp::Sle | IrCmpOp::Ule => "setbe",
                IrCmpOp::Sgt | IrCmpOp::Ugt => "seta",
                IrCmpOp::Sge | IrCmpOp::Uge => "setae",
            };
            self.state.emit(&format!("    {} %al", set_instr));
            self.state.emit("    movzbq %al, %rax");
            self.store_rax_to(dest);
            return;
        }

        self.operand_to_rax(lhs);
        self.state.emit("    pushq %rax");
        self.operand_to_rax(rhs);
        self.state.emit("    movq %rax, %rcx");
        self.state.emit("    popq %rax");
        self.state.emit("    cmpq %rcx, %rax");

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
        self.state.emit(&format!("    {} %al", set_instr));
        self.state.emit("    movzbq %al, %rax");
        self.store_rax_to(dest);
    }

    fn emit_call(&mut self, args: &[Operand], arg_types: &[IrType], direct_name: Option<&str>,
                 func_ptr: Option<&Operand>, dest: Option<Value>, return_type: IrType,
                 _is_variadic: bool) {
        // Classify args: float args go in xmm regs, others in int regs
        let mut stack_args: Vec<&Operand> = Vec::new();
        let mut int_idx = 0usize;
        let mut float_idx = 0usize;
        let mut arg_assignments: Vec<(&Operand, bool, usize)> = Vec::new(); // (arg, is_float, reg_idx)

        for (i, arg) in args.iter().enumerate() {
            let is_float_arg = if i < arg_types.len() {
                arg_types[i].is_float()
            } else {
                matches!(arg, Operand::Const(IrConst::F32(_) | IrConst::F64(_)))
            };
            if is_float_arg && float_idx < 8 {
                arg_assignments.push((arg, true, float_idx));
                float_idx += 1;
            } else if !is_float_arg && int_idx < 6 {
                arg_assignments.push((arg, false, int_idx));
                int_idx += 1;
            } else {
                stack_args.push(arg);
            }
        }

        // Ensure 16-byte stack alignment before call when pushing stack args.
        // The stack is 16-byte aligned before this emit_call. Each pushq adds 8 bytes.
        // If odd number of stack args, we need an extra 8-byte pad to maintain alignment.
        let need_align_pad = stack_args.len() % 2 != 0;
        if need_align_pad {
            self.state.emit("    subq $8, %rsp");
        }

        // Push stack args in reverse order
        for arg in stack_args.iter().rev() {
            self.operand_to_rax(arg);
            self.state.emit("    pushq %rax");
        }

        // Load register args
        let xmm_regs = ["xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"];
        for (arg, is_float, idx) in &arg_assignments {
            self.operand_to_rax(arg);
            if *is_float {
                self.state.emit(&format!("    movq %rax, %{}", xmm_regs[*idx]));
            } else {
                self.state.emit(&format!("    movq %rax, %{}", X86_ARG_REGS[*idx]));
            }
        }

        // Set AL = number of float args for variadic functions (SysV ABI)
        if float_idx > 0 {
            self.state.emit(&format!("    movb ${}, %al", float_idx));
        } else {
            self.state.emit("    xorl %eax, %eax");
        }

        if let Some(name) = direct_name {
            self.state.emit(&format!("    call {}", name));
        } else if let Some(ptr) = func_ptr {
            self.state.emit("    pushq %rax"); // save AL
            self.operand_to_rax(ptr);
            self.state.emit("    movq %rax, %r10");
            self.state.emit("    popq %rax"); // restore AL
            self.state.emit("    call *%r10");
        }

        if !stack_args.is_empty() {
            let cleanup = stack_args.len() * 8 + if need_align_pad { 8 } else { 0 };
            self.state.emit(&format!("    addq ${}, %rsp", cleanup));
        }

        if let Some(dest) = dest {
            if return_type == IrType::F32 {
                // F32 return value is in xmm0 (low 32 bits)
                self.state.emit("    movd %xmm0, %eax");
            } else if return_type == IrType::F64 {
                // F64 return value is in xmm0 (full 64 bits)
                self.state.emit("    movq %xmm0, %rax");
            }
            self.store_rax_to(&dest);
        }
    }

    fn emit_global_addr(&mut self, dest: &Value, name: &str) {
        self.state.emit(&format!("    leaq {}(%rip), %rax", name));
        self.store_rax_to(dest);
    }

    fn emit_gep(&mut self, dest: &Value, base: &Value, offset: &Operand) {
        if let Some(slot) = self.state.get_slot(base.0) {
            if self.state.is_alloca(base.0) {
                self.state.emit(&format!("    leaq {}(%rbp), %rax", slot.0));
            } else {
                self.state.emit(&format!("    movq {}(%rbp), %rax", slot.0));
            }
        }
        self.state.emit("    pushq %rax");
        self.operand_to_rax(offset);
        self.state.emit("    movq %rax, %rcx");
        self.state.emit("    popq %rax");
        self.state.emit("    addq %rcx, %rax");
        self.store_rax_to(dest);
    }

    fn emit_cast(&mut self, dest: &Value, src: &Operand, from_ty: IrType, to_ty: IrType) {
        self.operand_to_rax(src);
        self.emit_cast_instrs(from_ty, to_ty);
        self.store_rax_to(dest);
    }

    fn emit_memcpy(&mut self, dest: &Value, src: &Value, size: usize) {
        // Load dest address into rdi, src address into rsi
        if let Some(dst_slot) = self.state.get_slot(dest.0) {
            if self.state.is_alloca(dest.0) {
                self.state.emit(&format!("    leaq {}(%rbp), %rdi", dst_slot.0));
            } else {
                self.state.emit(&format!("    movq {}(%rbp), %rdi", dst_slot.0));
            }
        }
        if let Some(src_slot) = self.state.get_slot(src.0) {
            if self.state.is_alloca(src.0) {
                self.state.emit(&format!("    leaq {}(%rbp), %rsi", src_slot.0));
            } else {
                self.state.emit(&format!("    movq {}(%rbp), %rsi", src_slot.0));
            }
        }
        // Inline memcpy using rep movsb
        self.state.emit(&format!("    movq ${}, %rcx", size));
        self.state.emit("    rep movsb");
    }

    fn emit_return(&mut self, val: Option<&Operand>, _frame_size: i64) {
        if let Some(val) = val {
            self.operand_to_rax(val);
            if self.current_return_type == IrType::F32 {
                self.state.emit("    movd %eax, %xmm0");
            } else if self.current_return_type == IrType::F64 {
                self.state.emit("    movq %rax, %xmm0");
            }
        }
        self.emit_epilogue(0); // frame_size not needed for x86 epilogue
        self.state.emit("    ret");
    }

    fn emit_branch(&mut self, label: &str) {
        self.state.emit(&format!("    jmp {}", label));
    }

    fn emit_cond_branch(&mut self, cond: &Operand, true_label: &str, false_label: &str) {
        self.operand_to_rax(cond);
        self.state.emit("    testq %rax, %rax");
        self.state.emit(&format!("    jne {}", true_label));
        self.state.emit(&format!("    jmp {}", false_label));
    }

    fn emit_unreachable(&mut self) {
        self.state.emit("    ud2");
    }
}

impl Default for X86Codegen {
    fn default() -> Self {
        Self::new()
    }
}
