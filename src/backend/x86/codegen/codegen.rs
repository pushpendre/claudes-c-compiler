use crate::ir::ir::*;
use crate::common::types::IrType;
use crate::backend::common::PtrDirective;
use crate::backend::codegen_shared::*;

/// x86-64 code generator. Implements the ArchCodegen trait for the shared framework.
/// Uses System V AMD64 ABI with stack-based allocation (no register allocator yet).
pub struct X86Codegen {
    state: CodegenState,
    current_return_type: IrType,
    /// For variadic functions: number of named integer/pointer parameters
    num_named_int_params: usize,
    /// For variadic functions: number of named floating-point parameters
    num_named_fp_params: usize,
    /// For variadic functions: stack offset of the register save area (negative from rbp)
    reg_save_area_offset: i64,
    /// Whether the current function is variadic
    is_variadic: bool,
}

impl X86Codegen {
    pub fn new() -> Self {
        Self {
            state: CodegenState::new(),
            current_return_type: IrType::I64,
            num_named_int_params: 0,
            num_named_fp_params: 0,
            reg_save_area_offset: 0,
            is_variadic: false,
        }
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
                    // LongDouble at computation level is treated as F64
                    IrConst::LongDouble(v) => {
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

    /// Emit x86-64 instructions for a type cast, using shared cast classification.
    fn emit_cast_instrs(&mut self, from_ty: IrType, to_ty: IrType) {
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
                        self.state.emit(&format!("    jae {}", big_label));
                        self.state.emit("    cvttsd2siq %xmm0, %rax");
                        self.state.emit(&format!("    jmp {}", done_label));
                        self.state.emit(&format!("{}:", big_label));
                        self.state.emit("    subsd %xmm1, %xmm0");
                        self.state.emit("    cvttsd2siq %xmm0, %rax");
                        self.state.emit("    movabsq $9223372036854775808, %rcx"); // 2^63 as int
                        self.state.emit("    addq %rcx, %rax");
                        self.state.emit(&format!("{}:", done_label));
                    } else {
                        self.state.emit("    cvttsd2siq %xmm0, %rax");
                    }
                } else {
                    self.state.emit("    movd %eax, %xmm0");
                    self.state.emit("    cvttss2siq %xmm0, %rax");
                }
            }

            CastKind::SignedToFloat { to_f64 } => {
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
                    self.state.emit(&format!("    js {}", big_label));
                    if to_f64 {
                        self.state.emit("    cvtsi2sdq %rax, %xmm0");
                    } else {
                        self.state.emit("    cvtsi2ssq %rax, %xmm0");
                    }
                    self.state.emit(&format!("    jmp {}", done_label));
                    self.state.emit(&format!("{}:", big_label));
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
                    self.state.emit(&format!("{}:", done_label));
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
        self.state.emit(&format!("    {} (%rcx), {}", load_instr, load_dest));
        // Loop: rax = old, compute new = op(old, val), try cmpxchg
        let label_id = self.state.next_label_id();
        let loop_label = format!(".Latomic_loop_{}", label_id);
        self.state.emit(&format!("{}:", loop_label));
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
            "sub" => self.state.emit(&format!("    sub{} %{}, %{}", size_suffix, r8_reg, rdx_reg)),
            "and" => self.state.emit(&format!("    and{} %{}, %{}", size_suffix, r8_reg, rdx_reg)),
            "or"  => self.state.emit(&format!("    or{} %{}, %{}", size_suffix, r8_reg, rdx_reg)),
            "xor" => self.state.emit(&format!("    xor{} %{}, %{}", size_suffix, r8_reg, rdx_reg)),
            "nand" => {
                self.state.emit(&format!("    and{} %{}, %{}", size_suffix, r8_reg, rdx_reg));
                self.state.emit(&format!("    not{} %{}", size_suffix, rdx_reg));
            }
            _ => {}
        }
        // Try cmpxchg: if [rcx] == rax (old), set [rcx] = rdx (new), else rax = [rcx]
        self.state.emit(&format!("    lock cmpxchg{} %{}, (%rcx)", size_suffix, rdx_reg));
        self.state.emit(&format!("    jne {}", loop_label));
        // rax = old value on success
    }
}

const X86_ARG_REGS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

impl ArchCodegen for X86Codegen {
    fn state(&mut self) -> &mut CodegenState { &mut self.state }
    fn state_ref(&self) -> &CodegenState { &self.state }
    fn ptr_directive(&self) -> PtrDirective { PtrDirective::Quad }

    fn calculate_stack_space(&mut self, func: &IrFunction) -> i64 {
        // Track variadic function info
        self.is_variadic = func.is_variadic;
        self.num_named_int_params = func.params.iter()
            .filter(|p| !p.ty.is_float())
            .count();
        self.num_named_fp_params = func.params.iter()
            .filter(|p| p.ty.is_float())
            .count();

        let mut space = calculate_stack_space_common(&mut self.state, func, 0, |space, alloc_size| {
            // x86 uses negative offsets from rbp
            let new_space = space + ((alloc_size + 7) & !7);
            (-new_space, new_space)
        });

        // For variadic functions, reserve 176 bytes for the register save area:
        // 48 bytes for 6 integer registers (rdi, rsi, rdx, rcx, r8, r9)
        // 128 bytes for 8 XMM registers (xmm0-xmm7, 16 bytes each)
        if func.is_variadic {
            space += 176;
            self.reg_save_area_offset = -space;
        }

        space
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

        // For variadic functions, save all arg registers to the register save area.
        // This allows va_arg to retrieve register-passed arguments.
        // Layout: [0..47] = integer regs, [48..175] = XMM regs (16 bytes each)
        if func.is_variadic {
            let base = self.reg_save_area_offset;
            // Save 6 integer argument registers
            self.state.emit(&format!("    movq %rdi, {}(%rbp)", base));
            self.state.emit(&format!("    movq %rsi, {}(%rbp)", base + 8));
            self.state.emit(&format!("    movq %rdx, {}(%rbp)", base + 16));
            self.state.emit(&format!("    movq %rcx, {}(%rbp)", base + 24));
            self.state.emit(&format!("    movq %r8, {}(%rbp)", base + 32));
            self.state.emit(&format!("    movq %r9, {}(%rbp)", base + 40));
            // Save 8 XMM argument registers (16 bytes each)
            for i in 0..8i64 {
                self.state.emit(&format!("    movdqu %xmm{}, {}(%rbp)", i, base + 48 + i * 16));
            }
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
            let is_long_double = param.ty.is_long_double();
            let is_float = param.ty.is_float() && !is_long_double;
            // F128 (long double) is always passed on the stack on x86-64 (X87 class)
            let is_stack_passed = if is_long_double {
                true
            } else if is_float {
                float_reg_idx >= 8
            } else {
                int_reg_idx >= 6
            };

            if param.name.is_empty() {
                if is_long_double {
                    // F128 takes 16 bytes on stack
                    stack_param_offset += 16;
                } else if is_stack_passed {
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
                    if is_long_double {
                        // F128 (long double) on x86-64: passed on stack as 16-byte x87 extended.
                        // Load the x87 80-bit extended precision value and convert to f64 bit pattern.
                        self.state.emit(&format!("    fldt {}(%rbp)", stack_param_offset));
                        self.state.emit("    subq $8, %rsp");
                        self.state.emit("    fstpl (%rsp)");
                        self.state.emit("    movq (%rsp), %rax");
                        self.state.emit("    addq $8, %rsp");
                        self.state.emit(&format!("    movq %rax, {}(%rbp)", slot.0));
                        stack_param_offset += 16;
                    } else if is_stack_passed {
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
                if is_long_double {
                    stack_param_offset += 16;
                } else if is_stack_passed {
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
            let float_op = classify_float_binop(op).unwrap_or(FloatOp::Add);
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
            self.state.emit("    popq %rax");
            // F128 uses F64 instructions (long double computed at double precision)
            let suffix = if ty == IrType::F64 || ty == IrType::F128 { "sd" } else { "ss" };
            let mnemonic = match float_op {
                FloatOp::Add => "add",
                FloatOp::Sub => "sub",
                FloatOp::Mul => "mul",
                FloatOp::Div => "div",
            };
            self.state.emit(&format!("    {}{} %xmm1, %xmm0", mnemonic, suffix));
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
                _ => {} // Clz/Ctz/Bswap/Popcount not applicable to floats
            }
        } else {
            match op {
                IrUnaryOp::Neg => self.state.emit("    negq %rax"),
                IrUnaryOp::Not => self.state.emit("    notq %rax"),
                IrUnaryOp::Clz => {
                    // Use lzcnt which correctly returns 32/64 for zero input.
                    // (bsr is undefined for zero input)
                    if ty == IrType::I32 || ty == IrType::U32 {
                        self.state.emit("    lzcntl %eax, %eax");
                    } else {
                        self.state.emit("    lzcntq %rax, %rax");
                    }
                }
                IrUnaryOp::Ctz => {
                    if ty == IrType::I32 || ty == IrType::U32 {
                        self.state.emit("    tzcntl %eax, %eax");
                    } else {
                        self.state.emit("    tzcntq %rax, %rax");
                    }
                }
                IrUnaryOp::Bswap => {
                    if ty == IrType::I16 || ty == IrType::U16 {
                        self.state.emit("    rolw $8, %ax");
                    } else if ty == IrType::I32 || ty == IrType::U32 {
                        self.state.emit("    bswapl %eax");
                    } else {
                        self.state.emit("    bswapq %rax");
                    }
                }
                IrUnaryOp::Popcount => {
                    if ty == IrType::I32 || ty == IrType::U32 {
                        self.state.emit("    popcntl %eax, %eax");
                    } else {
                        self.state.emit("    popcntq %rax, %rax");
                    }
                }
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
            // F128 uses F64 instructions (long double computed at double precision)
            if ty == IrType::F64 || ty == IrType::F128 {
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
                 _is_variadic: bool, _num_fixed_args: usize) {
        // Classify args: float args go in xmm regs, others in int regs
        let mut stack_args: Vec<&Operand> = Vec::new();
        let mut int_idx = 0usize;
        let mut float_idx = 0usize;
        let mut arg_assignments: Vec<(&Operand, bool, usize)> = Vec::new(); // (arg, is_float, reg_idx)

        // Track which stack args are F128 (long double, 16-byte x87 extended)
        let mut stack_arg_is_f128: Vec<bool> = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            let is_long_double = if i < arg_types.len() {
                arg_types[i].is_long_double()
            } else {
                false
            };
            let is_float_arg = if i < arg_types.len() {
                arg_types[i].is_float()
            } else {
                matches!(arg, Operand::Const(IrConst::F32(_) | IrConst::F64(_)))
            };
            if is_long_double {
                // F128 (long double): always goes on the stack as 16-byte x87 extended precision
                stack_args.push(arg);
                stack_arg_is_f128.push(true);
            } else if is_float_arg && float_idx < 8 {
                arg_assignments.push((arg, true, float_idx));
                float_idx += 1;
            } else if !is_float_arg && int_idx < 6 {
                arg_assignments.push((arg, false, int_idx));
                int_idx += 1;
            } else {
                stack_args.push(arg);
                stack_arg_is_f128.push(false);
            }
        }

        // Ensure 16-byte stack alignment before call when pushing stack args.
        // F128 args take 16 bytes (already aligned), non-F128 args take 8 bytes.
        let mut total_stack_bytes: usize = 0;
        for (si, _) in stack_args.iter().enumerate() {
            if si < stack_arg_is_f128.len() && stack_arg_is_f128[si] {
                total_stack_bytes += 16;
            } else {
                total_stack_bytes += 8;
            }
        }
        let need_align_pad = total_stack_bytes % 16 != 0;
        if need_align_pad {
            self.state.emit("    subq $8, %rsp");
        }

        // Push stack args in reverse order.
        // F128 (long double) args are pushed as 16 bytes in x87 80-bit extended format.
        let n_stack = stack_args.len();
        for ri in 0..n_stack {
            let si = n_stack - 1 - ri; // reverse index
            let is_f128 = si < stack_arg_is_f128.len() && stack_arg_is_f128[si];
            if is_f128 {
                // Push 16 bytes of x87 extended precision format
                match stack_args[si] {
                    Operand::Const(ref c) => {
                        let f64_val = match c {
                            IrConst::LongDouble(v) => *v,
                            IrConst::F64(v) => *v,
                            _ => c.to_f64().unwrap_or(0.0),
                        };
                        let x87_bytes = crate::ir::ir::f64_to_x87_bytes(f64_val);
                        // x87 format: 10 bytes of data, padded to 16 with 6 zero bytes
                        // Push hi 8 bytes first (padding + top 2 bytes of x87), then lo 8 bytes
                        let lo = u64::from_le_bytes(x87_bytes[0..8].try_into().unwrap());
                        let hi_2bytes = u16::from_le_bytes(x87_bytes[8..10].try_into().unwrap());
                        // Push 8 bytes: 6 bytes padding (zeros) + 2 bytes (exp+sign)
                        self.state.emit(&format!("    pushq ${}", hi_2bytes as i64));
                        // Push 8 bytes: mantissa
                        self.state.emit(&format!("    movabsq ${}, %rax", lo as i64));
                        self.state.emit("    pushq %rax");
                    }
                    Operand::Value(ref v) => {
                        // Variable: f64 value in slot, convert to x87 80-bit at runtime.
                        // Load f64 into xmm0, store to temp, use fld/fstp to convert.
                        if let Some(slot) = self.state.get_slot(v.0) {
                            if self.state.is_alloca(v.0) {
                                self.state.emit(&format!("    leaq {}(%rbp), %rax", slot.0));
                            } else {
                                self.state.emit(&format!("    movq {}(%rbp), %rax", slot.0));
                            }
                        } else {
                            self.state.emit("    xorq %rax, %rax");
                        }
                        // rax has f64 bit pattern; use x87 to convert f64 -> 80-bit extended
                        // Store f64 to temp memory, load with fldl, allocate 16 bytes, store with fstpt
                        self.state.emit("    subq $16, %rsp");    // allocate 16 bytes for result
                        self.state.emit("    pushq %rax");        // store f64 at [rsp] (temp)
                        self.state.emit("    fldl (%rsp)");       // load f64 onto x87 stack
                        self.state.emit("    addq $8, %rsp");     // pop temp f64
                        self.state.emit("    fstpt (%rsp)");      // store 80-bit to [rsp] (16 bytes allocated)
                        // 10 bytes written at [rsp], 6 bytes of padding at [rsp+10..rsp+15] are whatever
                        // That's fine per ABI: only 10 bytes are significant
                    }
                }
            } else {
                self.operand_to_rax(stack_args[si]);
                self.state.emit("    pushq %rax");
            }
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

        if total_stack_bytes > 0 || need_align_pad {
            let cleanup = total_stack_bytes + if need_align_pad { 8 } else { 0 };
            self.state.emit(&format!("    addq ${}, %rsp", cleanup));
        }

        if let Some(dest) = dest {
            if return_type == IrType::F32 {
                // F32 return value is in xmm0 (low 32 bits)
                self.state.emit("    movd %xmm0, %eax");
            } else if return_type == IrType::F128 {
                // F128 (long double) return value is in x87 st(0) per SysV ABI.
                // Convert from 80-bit extended to f64 bit pattern in rax.
                self.state.emit("    subq $8, %rsp");
                self.state.emit("    fstpl (%rsp)");
                self.state.emit("    movq (%rsp), %rax");
                self.state.emit("    addq $8, %rsp");
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

        // Load va_list pointer into %rcx
        if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
            if self.state.is_alloca(va_list_ptr.0) {
                self.state.emit(&format!("    leaq {}(%rbp), %rcx", slot.0));
            } else {
                self.state.emit(&format!("    movq {}(%rbp), %rcx", slot.0));
            }
        }

        if is_f128 {
            // F128 (long double) on x86-64: always from overflow_arg_area, 16 bytes.
            // Read 10-byte x87 extended precision from overflow area, convert to f64.
            self.state.emit("    movq 8(%rcx), %rdx");       // overflow_arg_area
            // Use x87 to load 80-bit extended and convert to f64
            self.state.emit("    fldt (%rdx)");              // load 80-bit extended from [rdx]
            self.state.emit("    subq $8, %rsp");            // temp space for f64
            self.state.emit("    fstpl (%rsp)");             // store as f64 to [rsp]
            self.state.emit("    movq (%rsp), %rax");        // load f64 bit pattern into rax
            self.state.emit("    addq $8, %rsp");            // free temp space
            // Advance overflow_arg_area by 16 (x87 long double is 16-byte aligned on stack)
            self.state.emit("    addq $16, 8(%rcx)");
            // Store result and jump to end
            if let Some(slot) = self.state.get_slot(dest.0) {
                self.state.emit(&format!("    movq %rax, {}(%rbp)", slot.0));
            }
            return;
        } else if is_fp {
            // Check fp_offset < 176
            self.state.emit("    movl 4(%rcx), %eax");  // fp_offset
            self.state.emit("    cmpl $176, %eax");
            self.state.emit(&format!("    jb {}", label_reg));
            self.state.emit(&format!("    jmp {}", label_mem));

            // Register path
            self.state.emit(&format!("{}:", label_reg));
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
            self.state.emit(&format!("    jmp {}", label_end));
        } else {
            // Check gp_offset < 48
            self.state.emit("    movl (%rcx), %eax");  // gp_offset
            self.state.emit("    cmpl $48, %eax");
            self.state.emit(&format!("    jb {}", label_reg));
            self.state.emit(&format!("    jmp {}", label_mem));

            // Register path
            self.state.emit(&format!("{}:", label_reg));
            self.state.emit("    movl (%rcx), %eax");        // gp_offset
            self.state.emit("    movslq %eax, %rdx");
            self.state.emit("    movq 16(%rcx), %rsi");      // reg_save_area
            self.state.emit("    movq (%rsi,%rdx), %rax");   // load value
            self.state.emit("    addl $8, (%rcx)");          // gp_offset += 8
            self.state.emit(&format!("    jmp {}", label_end));
        }

        // Memory (overflow) path
        self.state.emit(&format!("{}:", label_mem));
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
        self.state.emit(&format!("{}:", label_end));
        // Store result
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.state.emit(&format!("    movq %rax, {}(%rbp)", slot.0));
        }
    }

    fn emit_va_start(&mut self, va_list_ptr: &Value) {
        // x86-64 System V ABI va_start implementation.
        // The prologue saves all 6 integer arg registers to the register save area.
        // va_list struct layout:
        //   [0]  u32 gp_offset:         byte offset into reg_save_area for next GP arg
        //   [4]  u32 fp_offset:         byte offset into reg_save_area for next FP arg
        //   [8]  void* overflow_arg_area: pointer to next stack-passed arg
        //   [16] void* reg_save_area:   pointer to saved register args

        // Load va_list pointer into %rax
        if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
            if self.state.is_alloca(va_list_ptr.0) {
                self.state.emit(&format!("    leaq {}(%rbp), %rax", slot.0));
            } else {
                self.state.emit(&format!("    movq {}(%rbp), %rax", slot.0));
            }
        }
        // gp_offset = min(num_named_int_params, 6) * 8 (skip named params in reg save area)
        // Cap at 48 (6 registers * 8 bytes) since only 6 GP regs are saved
        let gp_offset = self.num_named_int_params.min(6) * 8;
        self.state.emit(&format!("    movl ${}, (%rax)", gp_offset));
        // fp_offset = 48 + min(num_named_fp_params, 8) * 16 (skip named FP params in XMM save area)
        // Each XMM register occupies 16 bytes in the register save area
        let fp_offset = 48 + self.num_named_fp_params.min(8) * 16;
        self.state.emit(&format!("    movl ${}, 4(%rax)", fp_offset));
        // overflow_arg_area = rbp + 16 + num_stack_named_params * 8
        // Stack-passed named params are those beyond the 6 GP register params and 8 FP register params
        let num_stack_int = if self.num_named_int_params > 6 { self.num_named_int_params - 6 } else { 0 };
        let num_stack_fp = if self.num_named_fp_params > 8 { self.num_named_fp_params - 8 } else { 0 };
        let overflow_offset = 16 + (num_stack_int + num_stack_fp) * 8;
        self.state.emit(&format!("    leaq {}(%rbp), %rcx", overflow_offset));
        self.state.emit("    movq %rcx, 8(%rax)");
        // reg_save_area = address of the saved registers in the prologue
        let reg_save = self.reg_save_area_offset;
        self.state.emit(&format!("    leaq {}(%rbp), %rcx", reg_save));
        self.state.emit("    movq %rcx, 16(%rax)");
    }

    fn emit_va_end(&mut self, _va_list_ptr: &Value) {
        // va_end is a no-op on x86-64
    }

    fn emit_va_copy(&mut self, dest_ptr: &Value, src_ptr: &Value) {
        // Copy 24 bytes from src va_list to dest va_list
        if let Some(src_slot) = self.state.get_slot(src_ptr.0) {
            if self.state.is_alloca(src_ptr.0) {
                self.state.emit(&format!("    leaq {}(%rbp), %rsi", src_slot.0));
            } else {
                self.state.emit(&format!("    movq {}(%rbp), %rsi", src_slot.0));
            }
        }
        if let Some(dest_slot) = self.state.get_slot(dest_ptr.0) {
            if self.state.is_alloca(dest_ptr.0) {
                self.state.emit(&format!("    leaq {}(%rbp), %rdi", dest_slot.0));
            } else {
                self.state.emit(&format!("    movq {}(%rbp), %rdi", dest_slot.0));
            }
        }
        // Copy 24 bytes (sizeof va_list = 24 on x86-64)
        self.state.emit("    movq (%rsi), %rax");
        self.state.emit("    movq %rax, (%rdi)");
        self.state.emit("    movq 8(%rsi), %rax");
        self.state.emit("    movq %rax, 8(%rdi)");
        self.state.emit("    movq 16(%rsi), %rax");
        self.state.emit("    movq %rax, 16(%rdi)");
    }

    fn emit_return(&mut self, val: Option<&Operand>, _frame_size: i64) {
        if let Some(val) = val {
            self.operand_to_rax(val);
            if self.current_return_type == IrType::F32 {
                self.state.emit("    movd %eax, %xmm0");
            } else if self.current_return_type == IrType::F128 {
                // F128 (long double) must be returned in x87 st(0) per SysV ABI.
                // rax has f64 bit pattern; push to stack, load with fldl.
                self.state.emit("    pushq %rax");
                self.state.emit("    fldl (%rsp)");
                self.state.emit("    addq $8, %rsp");
            } else if self.current_return_type == IrType::F64 {
                // F64 return value goes in xmm0
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

    fn emit_label_addr(&mut self, dest: &Value, label: &str) {
        // Load address of a label for computed goto (GCC &&label extension)
        self.state.emit(&format!("    leaq {}(%rip), %rax", label));
        self.store_rax_to(dest);
    }

    fn emit_indirect_branch(&mut self, target: &Operand) {
        // Computed goto: goto *target
        self.operand_to_rax(target);
        self.state.emit("    jmpq *%rax");
    }

    fn emit_dyn_alloca(&mut self, dest: &Value, size: &Operand, align: usize) {
        // Dynamic stack allocation: subtract size from rsp, align, store pointer
        // 1. Load size into rax
        self.operand_to_rax(size);
        // 2. Round up size to 16-byte alignment for stack alignment
        self.state.emit("    addq $15, %rax");
        self.state.emit("    andq $-16, %rax");
        // 3. Subtract from stack pointer
        self.state.emit("    subq %rax, %rsp");
        // 4. Align result pointer if needed
        if align > 16 {
            self.state.emit(&format!("    movq %rsp, %rax"));
            self.state.emit(&format!("    addq ${}, %rax", align - 1));
            self.state.emit(&format!("    andq ${}, %rax", -(align as i64)));
            self.store_rax_to(dest);
        } else {
            // rsp is already 16-byte aligned
            self.state.emit("    movq %rsp, %rax");
            self.store_rax_to(dest);
        }
    }

    fn emit_atomic_rmw(&mut self, dest: &Value, op: AtomicRmwOp, ptr: &Operand, val: &Operand, ty: IrType, _ordering: AtomicOrdering) {
        // Load ptr into rcx, val into rax/rdx
        self.operand_to_rax(ptr);
        self.state.emit("    movq %rax, %rcx"); // rcx = ptr
        self.operand_to_rax(val);
        // rax = val, rcx = ptr
        let size_suffix = Self::type_suffix(ty);
        let val_reg = Self::reg_for_type("rax", ty);
        match op {
            AtomicRmwOp::Add => {
                // lock xadd stores old value in source reg, adds to dest
                self.state.emit(&format!("    lock xadd{} %{}, (%rcx)", size_suffix, val_reg));
                // After xadd, rax has the OLD value. Result = old + val.
                // But we want the NEW value for __atomic_add_fetch. The caller handles this.
                // Actually for fetch_and_add we want old value, for add_and_fetch we want new.
                // The IR op is always "return old value" (AtomicRmwOp::Add means fetch_add).
                // The lowering will handle computing new = old + val if needed.
            }
            AtomicRmwOp::Xchg => {
                // xchg is implicitly locked
                self.state.emit(&format!("    xchg{} %{}, (%rcx)", size_suffix, val_reg));
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
        let size_suffix = Self::type_suffix(ty);
        let desired_reg = Self::reg_for_type("rdx", ty);
        self.state.emit(&format!("    lock cmpxchg{} %{}, (%rcx)", size_suffix, desired_reg));
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
        let load_instr = Self::mov_load_for_type(ty);
        let dest_reg = Self::load_dest_reg(ty);
        self.state.emit(&format!("    {} (%rax), {}", load_instr, dest_reg));
        self.store_rax_to(dest);
    }

    fn emit_atomic_store(&mut self, ptr: &Operand, val: &Operand, ty: IrType, _ordering: AtomicOrdering) {
        // On x86, aligned stores are naturally atomic; use xchg for seq_cst
        self.operand_to_rax(val);
        self.state.emit("    movq %rax, %rdx"); // rdx = val
        self.operand_to_rax(ptr);
        // rax = ptr, rdx = val
        let store_reg = Self::reg_for_type("rdx", ty);
        let store_instr = Self::mov_store_for_type(ty);
        self.state.emit(&format!("    {} %{}, (%rax)", store_instr, store_reg));
        // Full fence for seq_cst
        self.state.emit("    mfence");
    }

    fn emit_fence(&mut self, _ordering: AtomicOrdering) {
        self.state.emit("    mfence");
    }

    fn emit_inline_asm(&mut self, template: &str, outputs: &[(String, Value, Option<String>)], inputs: &[(String, Operand, Option<String>)], _clobbers: &[String], operand_types: &[IrType]) {
        // x86-64 inline assembly support.
        // Allocate registers for operands, substitute %0/%1/%[name] in template,
        // load inputs and store outputs.

        // Scratch registers for generic "r" constraints (caller-saved, not rax/rsp/rbp)
        let gp_scratch: &[&str] = &["rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11"];
        let mut scratch_idx = 0;

        // Map from specific constraint letters to registers
        fn specific_reg(constraint: &str) -> Option<&'static str> {
            let c = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
            match c {
                "a" => Some("rax"),
                "b" => Some("rbx"),
                "c" => Some("rcx"),
                "d" => Some("rdx"),
                "S" => Some("rsi"),
                "D" => Some("rdi"),
                _ => None,
            }
        }

        // Determine if constraint is a pure memory constraint (not "rm" which prefers register)
        fn is_memory_constraint(constraint: &str) -> bool {
            let c = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
            // Only "m" is pure memory. "rm", "qm", "g" allow register (prefer register).
            c == "m"
        }

        // Determine if constraint is a tied operand ("0", "1", etc.)
        fn tied_operand(constraint: &str) -> Option<usize> {
            let c = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
            if c.len() >= 1 && c.chars().all(|ch| ch.is_ascii_digit()) {
                c.parse::<usize>().ok()
            } else {
                None
            }
        }

        // Phase 1: Assign registers/memory to all operands
        // Order: outputs first, then inputs (matching GCC operand numbering)
        let total_operands = outputs.len() + inputs.len();
        let mut op_regs: Vec<String> = vec![String::new(); total_operands]; // register name or empty for memory
        let mut op_is_memory: Vec<bool> = vec![false; total_operands];
        let mut op_names: Vec<Option<String>> = vec![None; total_operands];
        let mut op_mem_addrs: Vec<String> = vec![String::new(); total_operands]; // "offset(%rbp)" for memory ops

        // First pass: assign specific registers and mark memory operands
        for (i, (constraint, ptr, name)) in outputs.iter().enumerate() {
            op_names[i] = name.clone();
            if let Some(reg) = specific_reg(constraint) {
                op_regs[i] = reg.to_string();
            } else if is_memory_constraint(constraint) {
                op_is_memory[i] = true;
                // For memory output, we need the address of the pointer target
                if let Some(slot) = self.state.get_slot(ptr.0) {
                    if self.state.is_alloca(ptr.0) {
                        op_mem_addrs[i] = format!("{}(%rbp)", slot.0);
                    } else {
                        // ptr itself is a pointer; load it to get the actual address
                        op_mem_addrs[i] = format!("{}(%rbp)", slot.0);
                    }
                }
            }
        }

        // Track which inputs are tied (to avoid assigning them scratch regs)
        let mut input_tied_to: Vec<Option<usize>> = vec![None; inputs.len()];

        for (i, (constraint, _val, name)) in inputs.iter().enumerate() {
            let op_idx = outputs.len() + i;
            op_names[op_idx] = name.clone();
            if let Some(tied_to) = tied_operand(constraint) {
                input_tied_to[i] = Some(tied_to);
                // Don't assign yet - will resolve after scratch allocation
            } else if let Some(reg) = specific_reg(constraint) {
                op_regs[op_idx] = reg.to_string();
            } else if is_memory_constraint(constraint) {
                op_is_memory[op_idx] = true;
                match _val {
                    Operand::Value(v) => {
                        if let Some(slot) = self.state.get_slot(v.0) {
                            if self.state.is_alloca(v.0) {
                                op_mem_addrs[op_idx] = format!("{}(%rbp)", slot.0);
                            } else {
                                op_mem_addrs[op_idx] = format!("{}(%rbp)", slot.0);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        // Second pass: assign scratch registers to operands that need "r" and don't have one yet
        // Skip tied inputs (they'll get their register from the tied target)
        for i in 0..total_operands {
            if op_regs[i].is_empty() && !op_is_memory[i] {
                // Check if this is a tied input
                let is_tied = if i >= outputs.len() {
                    input_tied_to[i - outputs.len()].is_some()
                } else {
                    false
                };
                if !is_tied {
                    if scratch_idx < gp_scratch.len() {
                        op_regs[i] = gp_scratch[scratch_idx].to_string();
                        scratch_idx += 1;
                    } else {
                        op_regs[i] = format!("r{}", 12 + scratch_idx - gp_scratch.len());
                        scratch_idx += 1;
                    }
                }
            }
        }

        // Third pass: resolve tied operands now that all targets have registers
        for (i, tied_to) in input_tied_to.iter().enumerate() {
            if let Some(tied_to) = tied_to {
                let op_idx = outputs.len() + i;
                if *tied_to < op_regs.len() {
                    op_regs[op_idx] = op_regs[*tied_to].clone();
                    op_is_memory[op_idx] = op_is_memory[*tied_to];
                    op_mem_addrs[op_idx] = op_mem_addrs[*tied_to].clone();
                }
            }
        }

        // Handle "+" (read-write) constraints: the synthetic input shares the output's register.
        // The lowering adds synthetic inputs at the BEGINNING of the inputs list
        // (one for each "+" output, in order).
        let mut plus_idx = 0;
        for (i, (constraint, _, _)) in outputs.iter().enumerate() {
            if constraint.contains('+') {
                let plus_input_idx = outputs.len() + plus_idx;
                if plus_input_idx < total_operands {
                    op_regs[plus_input_idx] = op_regs[i].clone();
                    op_is_memory[plus_input_idx] = op_is_memory[i];
                    op_mem_addrs[plus_input_idx] = op_mem_addrs[i].clone();
                }
                plus_idx += 1;
            }
        }

        // Build GCC operand number → internal index mapping.
        // GCC numbers: outputs first, then EXPLICIT inputs (synthetic "+" inputs are hidden).
        // Internal: outputs, then synthetic "+" inputs, then explicit inputs.
        let num_plus = outputs.iter().filter(|(c,_,_)| c.contains('+')).count();
        let num_gcc_operands = outputs.len() + (inputs.len() - num_plus);
        let mut gcc_to_internal: Vec<usize> = Vec::with_capacity(num_gcc_operands);
        // Outputs map directly
        for i in 0..outputs.len() {
            gcc_to_internal.push(i);
        }
        // Explicit inputs (skip synthetic "+" inputs which are at the beginning of inputs)
        for i in num_plus..inputs.len() {
            gcc_to_internal.push(outputs.len() + i);
        }

        // Phase 2: Load input values into their assigned registers
        for (i, (constraint, val, _)) in inputs.iter().enumerate() {
            let op_idx = outputs.len() + i;
            if op_is_memory[op_idx] {
                continue; // Memory operands don't need loading into registers
            }
            let reg = &op_regs[op_idx];
            if reg.is_empty() {
                continue;
            }
            // For ALL inputs (including tied), load the input value into the register.
            // Tied operands ("0", "1") already share the same register as their target,
            // so loading the input value into that register is correct.
            match val {
                Operand::Const(c) => {
                    let imm = c.to_i64().unwrap_or(0);
                    if imm == 0 {
                        self.state.emit(&format!("    xorq %{}, %{}", reg, reg));
                    } else {
                        self.state.emit(&format!("    movabsq ${}, %{}", imm, reg));
                    }
                }
                Operand::Value(v) => {
                    if let Some(slot) = self.state.get_slot(v.0) {
                        if self.state.is_alloca(v.0) {
                            self.state.emit(&format!("    leaq {}(%rbp), %{}", slot.0, reg));
                        } else {
                            self.state.emit(&format!("    movq {}(%rbp), %{}", slot.0, reg));
                        }
                    }
                }
            }
        }

        // Also pre-load read-write output values
        for (i, (constraint, ptr, _)) in outputs.iter().enumerate() {
            if constraint.contains('+') && !op_is_memory[i] {
                let reg = &op_regs[i];
                if let Some(slot) = self.state.get_slot(ptr.0) {
                    self.state.emit(&format!("    movq {}(%rbp), %{}", slot.0, reg));
                }
            }
        }

        // Build operand type array for register size selection (default to I64 if missing)
        let mut op_types: Vec<IrType> = vec![IrType::I64; total_operands];
        for (i, ty) in operand_types.iter().enumerate() {
            if i < total_operands {
                op_types[i] = *ty;
            }
        }
        // For tied operands, inherit the type from the operand they're tied to
        for (i, (constraint, _, _)) in inputs.iter().enumerate() {
            let op_idx = outputs.len() + i;
            if let Some(tied_to) = tied_operand(constraint) {
                if tied_to < op_types.len() && op_idx < op_types.len() {
                    op_types[op_idx] = op_types[tied_to];
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
            let resolved = Self::substitute_x86_asm_operands(line, &op_regs, &op_names, &op_is_memory, &op_mem_addrs, &op_types, &gcc_to_internal);
            self.state.emit(&format!("    {}", resolved));
        }

        // Phase 4: Store output register values back to their stack slots
        for (i, (constraint, ptr, _)) in outputs.iter().enumerate() {
            if (constraint.contains('=') || constraint.contains('+')) && !op_is_memory[i] {
                let reg = &op_regs[i];
                if let Some(slot) = self.state.get_slot(ptr.0) {
                    self.state.emit(&format!("    movq %{}, {}(%rbp)", reg, slot.0));
                }
            }
        }
    }
}

impl X86Codegen {
    /// Substitute %0, %1, %[name], %k0, %b1, %w2, %q3, %h4 etc. in x86 asm template.
    /// Uses operand_types to determine the default register size when no modifier is given.
    fn substitute_x86_asm_operands(
        line: &str,
        op_regs: &[String],
        op_names: &[Option<String>],
        op_is_memory: &[bool],
        op_mem_addrs: &[String],
        op_types: &[IrType],
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
                // Check for x86 size modifier: k (32), w (16), b (8-low), h (8-high), q (64), l (32-alt)
                let mut modifier = None;
                if matches!(chars[i], 'k' | 'w' | 'b' | 'h' | 'q' | 'l') {
                    if i + 1 < chars.len() && (chars[i + 1].is_ascii_digit() || chars[i + 1] == '[') {
                        modifier = Some(chars[i]);
                        i += 1;
                    }
                }

                if chars[i] == '[' {
                    // Named operand: %[name] or %k[name]
                    i += 1;
                    let name_start = i;
                    while i < chars.len() && chars[i] != ']' {
                        i += 1;
                    }
                    let name: String = chars[name_start..i].iter().collect();
                    if i < chars.len() { i += 1; } // skip ]

                    let mut found = false;
                    for (idx, op_name) in op_names.iter().enumerate() {
                        if let Some(ref n) = op_name {
                            if n == &name {
                                if op_is_memory[idx] {
                                    result.push_str(&op_mem_addrs[idx]);
                                } else {
                                    let effective_mod = modifier.or_else(|| Self::default_modifier_for_type(op_types.get(idx).copied()));
                                    result.push('%');
                                    result.push_str(&Self::format_x86_reg(&op_regs[idx], effective_mod));
                                }
                                found = true;
                                break;
                            }
                        }
                    }
                    if !found {
                        result.push('%');
                        if let Some(m) = modifier { result.push(m); }
                        result.push('[');
                        result.push_str(&name);
                        result.push(']');
                    }
                } else if chars[i].is_ascii_digit() {
                    // Positional operand: %0, %1, %k2, etc.
                    // GCC operand numbers skip synthetic "+" inputs, so map through gcc_to_internal
                    let mut num = 0usize;
                    while i < chars.len() && chars[i].is_ascii_digit() {
                        num = num * 10 + (chars[i] as usize - '0' as usize);
                        i += 1;
                    }
                    let internal_idx = if num < gcc_to_internal.len() {
                        gcc_to_internal[num]
                    } else {
                        num // fallback: direct mapping
                    };
                    if internal_idx < op_regs.len() {
                        if op_is_memory[internal_idx] {
                            result.push_str(&op_mem_addrs[internal_idx]);
                        } else {
                            let effective_mod = modifier.or_else(|| Self::default_modifier_for_type(op_types.get(internal_idx).copied()));
                            result.push('%');
                            result.push_str(&Self::format_x86_reg(&op_regs[internal_idx], effective_mod));
                        }
                    } else {
                        result.push('%');
                        if let Some(m) = modifier { result.push(m); }
                        result.push_str(&format!("{}", num));
                    }
                } else {
                    // Not a recognized pattern, emit as-is (e.g. %rax, %eax, etc.)
                    result.push('%');
                    if let Some(m) = modifier { result.push(m); }
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

    /// Determine the default register size modifier based on the operand's IR type.
    /// In GCC inline asm, the default register size matches the C type:
    /// - 8-bit types -> %al (modifier 'b')
    /// - 16-bit types -> %ax (modifier 'w')
    /// - 32-bit types -> %eax (modifier 'k')
    /// - 64-bit types -> %rax (no modifier / 'q')
    fn default_modifier_for_type(ty: Option<IrType>) -> Option<char> {
        match ty {
            Some(IrType::I8) | Some(IrType::U8) => Some('b'),
            Some(IrType::I16) | Some(IrType::U16) => Some('w'),
            Some(IrType::I32) | Some(IrType::U32) | Some(IrType::F32) => Some('k'),
            // I64, U64, Ptr, F64 all use 64-bit registers (default)
            _ => None,
        }
    }

    /// Format x86 register with size modifier.
    /// Modifiers: k (32-bit), w (16-bit), b (8-bit low), h (8-bit high), q (64-bit), l (32-bit alt)
    fn format_x86_reg(reg: &str, modifier: Option<char>) -> String {
        match modifier {
            Some('k') | Some('l') => {
                // 32-bit version
                Self::reg_to_32(reg)
            }
            Some('w') => {
                // 16-bit version
                Self::reg_to_16(reg)
            }
            Some('b') => {
                // 8-bit low version
                Self::reg_to_8l(reg)
            }
            Some('h') => {
                // 8-bit high version
                Self::reg_to_8h(reg)
            }
            Some('q') | None => {
                // 64-bit (default for x86-64)
                reg.to_string()
            }
            _ => reg.to_string(),
        }
    }

    /// Convert 64-bit register name to 32-bit variant.
    fn reg_to_32(reg: &str) -> String {
        match reg {
            "rax" => "eax".to_string(),
            "rbx" => "ebx".to_string(),
            "rcx" => "ecx".to_string(),
            "rdx" => "edx".to_string(),
            "rsi" => "esi".to_string(),
            "rdi" => "edi".to_string(),
            "rbp" => "ebp".to_string(),
            "rsp" => "esp".to_string(),
            _ if reg.starts_with('r') => format!("{}d", reg), // r8 -> r8d, r10 -> r10d
            _ => reg.to_string(),
        }
    }

    /// Convert 64-bit register name to 16-bit variant.
    fn reg_to_16(reg: &str) -> String {
        match reg {
            "rax" => "ax".to_string(),
            "rbx" => "bx".to_string(),
            "rcx" => "cx".to_string(),
            "rdx" => "dx".to_string(),
            "rsi" => "si".to_string(),
            "rdi" => "di".to_string(),
            "rbp" => "bp".to_string(),
            "rsp" => "sp".to_string(),
            _ if reg.starts_with('r') => format!("{}w", reg), // r8 -> r8w
            _ => reg.to_string(),
        }
    }

    /// Convert 64-bit register name to 8-bit low variant.
    fn reg_to_8l(reg: &str) -> String {
        match reg {
            "rax" => "al".to_string(),
            "rbx" => "bl".to_string(),
            "rcx" => "cl".to_string(),
            "rdx" => "dl".to_string(),
            "rsi" => "sil".to_string(),
            "rdi" => "dil".to_string(),
            _ if reg.starts_with('r') => format!("{}b", reg), // r8 -> r8b
            _ => reg.to_string(),
        }
    }

    /// Convert 64-bit register name to 8-bit high variant.
    fn reg_to_8h(reg: &str) -> String {
        match reg {
            "rax" => "ah".to_string(),
            "rbx" => "bh".to_string(),
            "rcx" => "ch".to_string(),
            "rdx" => "dh".to_string(),
            _ => Self::reg_to_8l(reg), // fallback to low byte
        }
    }
}

impl Default for X86Codegen {
    fn default() -> Self {
        Self::new()
    }
}
