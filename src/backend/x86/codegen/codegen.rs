use crate::ir::ir::*;
use crate::common::types::IrType;
use crate::backend::common::PtrDirective;
use crate::backend::codegen_shared::*;

/// x86-64 code generator. Implements the ArchCodegen trait for the shared framework.
/// Uses System V AMD64 ABI with stack-based allocation (no register allocator yet).
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

    pub fn generate(mut self, module: &IrModule) -> String {
        let raw = generate_module(&mut self, module);
        super::peephole::peephole_optimize(raw)
    }

    // --- x86 helper methods ---

    /// Load an operand into %rax. Uses the register cache to skip the load
    /// if the value is already in %rax.
    fn operand_to_rax(&mut self, op: &Operand) {
        match op {
            Operand::Const(c) => {
                self.state.reg_cache.invalidate_acc();
                match c {
                    IrConst::I8(v) => self.state.emit_fmt(format_args!("    movq ${}, %rax", *v as i64)),
                    IrConst::I16(v) => self.state.emit_fmt(format_args!("    movq ${}, %rax", *v as i64)),
                    IrConst::I32(v) => self.state.emit_fmt(format_args!("    movq ${}, %rax", *v as i64)),
                    IrConst::I64(v) => {
                        if *v >= i32::MIN as i64 && *v <= i32::MAX as i64 {
                            self.state.emit_fmt(format_args!("    movq ${}, %rax", v));
                        } else {
                            self.state.emit_fmt(format_args!("    movabsq ${}, %rax", v));
                        }
                    }
                    IrConst::F32(v) => {
                        let bits = v.to_bits() as u64;
                        self.state.emit_fmt(format_args!("    movq ${}, %rax", bits as i64));
                    }
                    IrConst::F64(v) => {
                        let bits = v.to_bits();
                        if bits == 0 {
                            self.state.emit("    xorq %rax, %rax");
                        } else {
                            self.state.emit_fmt(format_args!("    movabsq ${}, %rax", bits as i64));
                        }
                    }
                    // LongDouble at computation level is treated as F64
                    IrConst::LongDouble(v) => {
                        let bits = v.to_bits();
                        if bits == 0 {
                            self.state.emit("    xorq %rax, %rax");
                        } else {
                            self.state.emit_fmt(format_args!("    movabsq ${}, %rax", bits as i64));
                        }
                    }
                    IrConst::I128(v) => {
                        // Truncate to low 64 bits for rax-only path
                        let low = *v as i64;
                        if low >= i32::MIN as i64 && low <= i32::MAX as i64 {
                            self.state.emit_fmt(format_args!("    movq ${}, %rax", low));
                        } else {
                            self.state.emit_fmt(format_args!("    movabsq ${}, %rax", low));
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
                if self.state.get_slot(v.0).is_some() {
                    self.value_to_reg(v, "rax");
                    self.state.reg_cache.set_acc(v.0, is_alloca);
                } else {
                    self.state.emit("    xorq %rax, %rax");
                    self.state.reg_cache.invalidate_acc();
                }
            }
        }
    }

    /// Store %rax to a value's stack slot. Updates the register cache to record
    /// that %rax now holds dest's value.
    fn store_rax_to(&mut self, dest: &Value) {
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.state.emit_fmt(format_args!("    movq %rax, {}(%rbp)", slot.0));
            // After storing to dest, %rax still holds dest's value
            self.state.reg_cache.set_acc(dest.0, false);
        }
    }

    /// Load a Value into a named register. For allocas, loads the address (leaq);
    /// for regular values, loads the data (movq).
    fn value_to_reg(&mut self, val: &Value, reg: &str) {
        if let Some(slot) = self.state.get_slot(val.0) {
            if self.state.is_alloca(val.0) {
                self.state.emit_fmt(format_args!("    leaq {}(%rbp), %{}", slot.0, reg));
            } else {
                self.state.emit_fmt(format_args!("    movq {}(%rbp), %{}", slot.0, reg));
            }
        }
    }

    /// Load an operand directly into %rcx, bypassing %rax.
    /// Used by emit_int_binop and emit_cmp to avoid the push/pop dance when
    /// loading the RHS operand. For constants, emits movq $IMM, %rcx.
    /// For values, loads directly from the stack slot into %rcx.
    fn load_rhs_to_rcx(&mut self, op: &Operand) {
        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::I8(v) => self.state.emit_fmt(format_args!("    movq ${}, %rcx", *v as i64)),
                    IrConst::I16(v) => self.state.emit_fmt(format_args!("    movq ${}, %rcx", *v as i64)),
                    IrConst::I32(v) => self.state.emit_fmt(format_args!("    movq ${}, %rcx", *v as i64)),
                    IrConst::I64(v) => {
                        if *v >= i32::MIN as i64 && *v <= i32::MAX as i64 {
                            self.state.emit_fmt(format_args!("    movq ${}, %rcx", v));
                        } else {
                            self.state.emit_fmt(format_args!("    movabsq ${}, %rcx", v));
                        }
                    }
                    IrConst::I128(v) => {
                        let low = *v as i64;
                        if low >= i32::MIN as i64 && low <= i32::MAX as i64 {
                            self.state.emit_fmt(format_args!("    movq ${}, %rcx", low));
                        } else {
                            self.state.emit_fmt(format_args!("    movabsq ${}, %rcx", low));
                        }
                    }
                    IrConst::Zero => self.state.emit("    xorq %rcx, %rcx"),
                    // Float constants: unlikely as RHS of int binop, but handle gracefully
                    IrConst::F32(v) => {
                        let bits = v.to_bits() as u64;
                        self.state.emit_fmt(format_args!("    movq ${}, %rcx", bits as i64));
                    }
                    IrConst::F64(v) => {
                        let bits = v.to_bits();
                        if bits == 0 {
                            self.state.emit("    xorq %rcx, %rcx");
                        } else {
                            self.state.emit_fmt(format_args!("    movabsq ${}, %rcx", bits as i64));
                        }
                    }
                    IrConst::LongDouble(v) => {
                        let bits = v.to_bits();
                        if bits == 0 {
                            self.state.emit("    xorq %rcx, %rcx");
                        } else {
                            self.state.emit_fmt(format_args!("    movabsq ${}, %rcx", bits as i64));
                        }
                    }
                }
            }
            Operand::Value(v) => {
                if self.state.get_slot(v.0).is_some() {
                    self.value_to_reg(v, "rcx");
                } else {
                    self.state.emit("    xorq %rcx, %rcx");
                }
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
                    } else {
                        // 128-bit value in 16-byte stack slot
                        self.state.emit_fmt(format_args!("    movq {}(%rbp), %rax", slot.0));
                        self.state.emit_fmt(format_args!("    movq {}(%rbp), %rdx", slot.0 + 8));
                    }
                } else {
                    self.state.emit("    xorq %rax, %rax");
                    self.state.emit("    xorq %rdx, %rdx");
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

    /// Emit x86-64 instructions for a type cast that operates on the
    /// primary accumulator (%rax). Used internally by both `emit_cast_instrs`
    /// (trait method) and the i128 special-case paths in `emit_cast`.
    fn emit_cast_instrs_x86(&mut self, from_ty: IrType, to_ty: IrType) {
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
        self.state.emit("    jmpq *%rax");
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
            let mut stack_bytes = 0usize;
            for p in &func.params {
                if let Some(size) = p.struct_size {
                    if size <= 16 {
                        // Small struct uses 1 or 2 GP register slots
                        let regs_needed = if size <= 8 { 1 } else { 2 };
                        gp_count += regs_needed;
                    } else {
                        // Large struct always goes on stack
                        stack_bytes += (size + 7) & !7;
                    }
                } else if p.ty.is_long_double() {
                    stack_bytes += 16;
                } else if p.ty.is_float() {
                    fp_count += 1;
                } else {
                    gp_count += 1;
                }
            }
            self.num_named_int_params = gp_count;
            self.num_named_fp_params = fp_count;
            self.num_named_stack_bytes = stack_bytes;
        }

        let mut space = calculate_stack_space_common(&mut self.state, func, 0, |space, alloc_size, align| {
            // x86 uses negative offsets from rbp
            // Honor alignment: round up space to alignment boundary before allocating
            let effective_align = if align > 0 { align.max(8) } else { 8 };
            let alloc = (alloc_size + 7) & !7;
            let new_space = ((space + alloc + effective_align - 1) / effective_align) * effective_align;
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
            self.state.emit_fmt(format_args!("    subq ${}, %rsp", frame_size));
        }

        // For variadic functions, save all arg registers to the register save area.
        // This allows va_arg to retrieve register-passed arguments.
        // Layout: [0..47] = integer regs, [48..175] = XMM regs (16 bytes each)
        if func.is_variadic {
            let base = self.reg_save_area_offset;
            // Save 6 integer argument registers
            self.state.emit_fmt(format_args!("    movq %rdi, {}(%rbp)", base));
            self.state.emit_fmt(format_args!("    movq %rsi, {}(%rbp)", base + 8));
            self.state.emit_fmt(format_args!("    movq %rdx, {}(%rbp)", base + 16));
            self.state.emit_fmt(format_args!("    movq %rcx, {}(%rbp)", base + 24));
            self.state.emit_fmt(format_args!("    movq %r8, {}(%rbp)", base + 32));
            self.state.emit_fmt(format_args!("    movq %r9, {}(%rbp)", base + 40));
            // Save 8 XMM argument registers (16 bytes each)
            for i in 0..8i64 {
                self.state.emit_fmt(format_args!("    movdqu %xmm{}, {}(%rbp)", i, base + 48 + i * 16));
            }
        }
    }

    fn emit_epilogue(&mut self, _frame_size: i64) {
        self.state.emit("    movq %rbp, %rsp");
        self.state.emit("    popq %rbp");
    }

    fn emit_store_params(&mut self, func: &IrFunction) {
        let xmm_regs = ["xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"];
        // Use shared parameter classification (same ABI config as emit_call).
        let config = CallAbiConfig {
            max_int_regs: 6, max_float_regs: 8,
            align_i128_pairs: false,
            f128_in_fp_regs: false, f128_in_gp_pairs: false,
            variadic_floats_in_gp: false,
        };
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
                    let src = stack_base + offset;
                    self.state.emit_fmt(format_args!("    fldt {}(%rbp)", src));
                    self.state.emit("    subq $8, %rsp");
                    self.state.emit("    fstpl (%rsp)");
                    self.state.emit("    movq (%rsp), %rax");
                    self.state.emit("    addq $8, %rsp");
                    self.state.emit_fmt(format_args!("    movq %rax, {}(%rbp)", slot.0));
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
                // These variants don't occur for x86 (no F128 in FP/GP pair regs).
                ParamClass::F128FpReg { .. } | ParamClass::F128GpPair { .. } | ParamClass::F128Stack { .. } => {}
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

    fn emit_epilogue_and_ret(&mut self, _frame_size: i64) {
        self.emit_epilogue(0);
        self.state.emit("    ret");
    }

    fn store_instr_for_type(&self, ty: IrType) -> &'static str {
        Self::mov_store_for_type(ty)
    }

    fn load_instr_for_type(&self, ty: IrType) -> &'static str {
        Self::mov_load_for_type(ty)
    }

    fn emit_typed_store_to_slot(&mut self, instr: &'static str, ty: IrType, slot: StackSlot) {
        let reg = Self::reg_for_type("rax", ty);
        self.state.emit_fmt(format_args!("    {} %{}, {}(%rbp)", instr, reg, slot.0));
    }

    fn emit_typed_load_from_slot(&mut self, instr: &'static str, slot: StackSlot) {
        let dest_reg = if instr == "movl" { "%eax" } else { "%rax" };
        self.state.emit_fmt(format_args!("    {} {}(%rbp), {}", instr, slot.0, dest_reg));
    }

    fn emit_save_acc(&mut self) {
        self.state.emit("    movq %rax, %rdx");
    }

    fn emit_load_ptr_from_slot(&mut self, slot: StackSlot) {
        // Load pointer to rcx. Used by all indirect paths:
        // - i128 store: pair saved to rsi:rdi, ptr to rcx (no conflict)
        // - regular store: val saved to rdx, ptr to rcx (no conflict)
        // - i128 load: ptr to rcx, then load pair through rcx
        // - regular load: ptr to rcx, then load through rcx
        self.state.emit_fmt(format_args!("    movq {}(%rbp), %rcx", slot.0));
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

    fn emit_slot_addr_to_secondary(&mut self, slot: StackSlot, is_alloca: bool) {
        // Clobbers rax: invalidate cache before the leaq/movq + push.
        self.state.reg_cache.invalidate_acc();
        if is_alloca {
            self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rax", slot.0));
        } else {
            self.state.emit_fmt(format_args!("    movq {}(%rbp), %rax", slot.0));
        }
        self.state.emit("    pushq %rax");
        self.state.reg_cache.invalidate_all(); // rax has an address, not an IR value
    }

    fn emit_add_secondary_to_acc(&mut self) {
        self.state.emit("    movq %rax, %rcx");
        self.state.emit("    popq %rax");
        self.state.emit("    addq %rcx, %rax");
        // rax now has a computed address, not a value from any slot.
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

    fn emit_align_acc(&mut self, align: usize) {
        self.state.emit_fmt(format_args!("    addq ${}, %rax", align - 1));
        self.state.emit_fmt(format_args!("    andq ${}, %rax", -(align as i64)));
        self.state.reg_cache.invalidate_all();
    }

    fn emit_memcpy_load_dest_addr(&mut self, slot: StackSlot, is_alloca: bool) {
        if is_alloca {
            self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rdi", slot.0));
        } else {
            self.state.emit_fmt(format_args!("    movq {}(%rbp), %rdi", slot.0));
        }
    }

    fn emit_memcpy_load_src_addr(&mut self, slot: StackSlot, is_alloca: bool) {
        if is_alloca {
            self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rsi", slot.0));
        } else {
            self.state.emit_fmt(format_args!("    movq {}(%rbp), %rsi", slot.0));
        }
    }

    fn emit_alloca_aligned_addr(&mut self, slot: StackSlot, val_id: u32) {
        let align = self.state.alloca_over_align(val_id).unwrap();
        self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rcx", slot.0));
        self.state.emit_fmt(format_args!("    addq ${}, %rcx", align - 1));
        self.state.emit_fmt(format_args!("    andq ${}, %rcx", -(align as i64)));
    }

    fn emit_alloca_aligned_addr_to_acc(&mut self, slot: StackSlot, val_id: u32) {
        let align = self.state.alloca_over_align(val_id).unwrap();
        self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rax", slot.0));
        self.state.emit_fmt(format_args!("    addq ${}, %rax", align - 1));
        self.state.emit_fmt(format_args!("    andq ${}, %rax", -(align as i64)));
        self.state.reg_cache.invalidate_acc();
    }

    fn emit_acc_to_secondary(&mut self) {
        self.state.emit("    pushq %rax");
        // After push, the next emit_load_operand will overwrite rax. The pushed
        // value is preserved on the stack but is not what the cache claims is in rax.
        // We don't invalidate here because operand_to_rax will set the cache when
        // it loads the next value. However, if the next operand matches the cached
        // value, it would skip the load while rax actually has the pushed value.
        // The pushed value IS the cached value (it was in rax), so the cache is
        // still correct: rax hasn't changed yet. The cache becomes stale only when
        // the float_binop_impl does popq, which will invalidate.
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

    // emit_binop uses the shared default implementation (handles i128/float/int dispatch).
    // emit_float_binop uses the shared default implementation.

    fn emit_int_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        let use_32bit = ty == IrType::I32 || ty == IrType::U32;
        let is_unsigned = ty.is_unsigned();

        // Try immediate operand optimization: when RHS is a small constant,
        // many x86 instructions accept an immediate operand directly, avoiding
        // the need to load the constant into a register (saves 3+ instructions).
        if let Operand::Const(c) = rhs {
            if let Some(imm) = c.to_i64() {
                let fits_i32 = imm >= i32::MIN as i64 && imm <= i32::MAX as i64;

                // Shift operations always use immediate (shift amount fits in u8)
                if matches!(op, IrBinOp::Shl | IrBinOp::AShr | IrBinOp::LShr) {
                    let shift = (imm as u32) & 63;
                    self.operand_to_rax(lhs);
                    match op {
                        IrBinOp::Shl => {
                            if use_32bit {
                                self.state.emit_fmt(format_args!("    shll ${}, %eax", shift & 31));
                                if !is_unsigned { self.state.emit("    cltq"); }
                            } else {
                                self.state.emit_fmt(format_args!("    shlq ${}, %rax", shift));
                            }
                        }
                        IrBinOp::AShr => {
                            if use_32bit {
                                self.state.emit_fmt(format_args!("    sarl ${}, %eax", shift & 31));
                                if !is_unsigned { self.state.emit("    cltq"); }
                            } else {
                                self.state.emit_fmt(format_args!("    sarq ${}, %rax", shift));
                            }
                        }
                        IrBinOp::LShr => {
                            if use_32bit {
                                self.state.emit_fmt(format_args!("    shrl ${}, %eax", shift & 31));
                            } else {
                                self.state.emit_fmt(format_args!("    shrq ${}, %rax", shift));
                            }
                        }
                        _ => unreachable!(),
                    }
                    self.state.reg_cache.invalidate_acc();
                    self.store_rax_to(dest);
                    return;
                }

                // Add/Sub/And/Or/Xor with immediate (must fit in i32 for x86 encoding)
                if fits_i32 && matches!(op, IrBinOp::Add | IrBinOp::Sub | IrBinOp::And | IrBinOp::Or | IrBinOp::Xor) {
                    self.operand_to_rax(lhs);
                    match op {
                        IrBinOp::Add => {
                            if use_32bit {
                                self.state.emit_fmt(format_args!("    addl ${}, %eax", imm as i32));
                                if !is_unsigned { self.state.emit("    cltq"); }
                            } else {
                                self.state.emit_fmt(format_args!("    addq ${}, %rax", imm));
                            }
                        }
                        IrBinOp::Sub => {
                            if use_32bit {
                                self.state.emit_fmt(format_args!("    subl ${}, %eax", imm as i32));
                                if !is_unsigned { self.state.emit("    cltq"); }
                            } else {
                                self.state.emit_fmt(format_args!("    subq ${}, %rax", imm));
                            }
                        }
                        IrBinOp::And => {
                            if use_32bit {
                                self.state.emit_fmt(format_args!("    andl ${}, %eax", imm as i32));
                                if !is_unsigned { self.state.emit("    cltq"); }
                            } else {
                                self.state.emit_fmt(format_args!("    andq ${}, %rax", imm));
                            }
                        }
                        IrBinOp::Or => {
                            if use_32bit {
                                self.state.emit_fmt(format_args!("    orl ${}, %eax", imm as i32));
                                if !is_unsigned { self.state.emit("    cltq"); }
                            } else {
                                self.state.emit_fmt(format_args!("    orq ${}, %rax", imm));
                            }
                        }
                        IrBinOp::Xor => {
                            if use_32bit {
                                self.state.emit_fmt(format_args!("    xorl ${}, %eax", imm as i32));
                                if !is_unsigned { self.state.emit("    cltq"); }
                            } else {
                                self.state.emit_fmt(format_args!("    xorq ${}, %rax", imm));
                            }
                        }
                        _ => unreachable!(),
                    }
                    self.state.reg_cache.invalidate_acc();
                    self.store_rax_to(dest);
                    return;
                }

                // Mul with immediate: use imulq $imm, %rax, %rax (3-operand form)
                if fits_i32 && op == IrBinOp::Mul {
                    self.operand_to_rax(lhs);
                    if use_32bit {
                        self.state.emit_fmt(format_args!("    imull ${}, %eax, %eax", imm as i32));
                        if !is_unsigned { self.state.emit("    cltq"); }
                    } else {
                        self.state.emit_fmt(format_args!("    imulq ${}, %rax, %rax", imm));
                    }
                    self.state.reg_cache.invalidate_acc();
                    self.store_rax_to(dest);
                    return;
                }
            }
        }

        // Optimized path: load LHS into rax, then load RHS directly into rcx.
        // This avoids the push/pop dance when RHS is a Value or non-i32 Const.
        self.operand_to_rax(lhs);
        self.load_rhs_to_rcx(rhs);

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

        // The binary operation modified %rax, invalidate the acc cache
        self.state.reg_cache.invalidate_acc();
        self.store_rax_to(dest);
    }

    fn emit_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if is_i128_type(ty) {
            self.emit_i128_cmp(dest, op, lhs, rhs);
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
            let (mov_to_xmm0, mov_to_xmm1) = if ty == IrType::F32 {
                ("movd %eax, %xmm0", "movd %eax, %xmm1")
            } else {
                ("movq %rax, %xmm0", "movq %rax, %xmm1")
            };
            // For Lt/Le, we swap operand loading order so we can use seta/setae
            let swap_operands = matches!(op, IrCmpOp::Slt | IrCmpOp::Ult | IrCmpOp::Sle | IrCmpOp::Ule);
            let (first, second) = if swap_operands { (rhs, lhs) } else { (lhs, rhs) };
            self.operand_to_rax(first);
            self.state.emit_fmt(format_args!("    {}", mov_to_xmm0));
            self.state.emit("    pushq %rax");
            self.state.reg_cache.invalidate_acc();
            self.operand_to_rax(second);
            self.state.emit_fmt(format_args!("    {}", mov_to_xmm1));
            self.state.emit("    popq %rax");
            self.state.reg_cache.invalidate_acc();
            // F128 uses F64 instructions (long double computed at double precision)
            // ucomisd %xmm1, %xmm0 compares xmm0 vs xmm1 (AT&T: src, dst → compares dst to src)
            if ty == IrType::F64 || ty == IrType::F128 {
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
            self.store_rax_to(dest);
            return;
        }

        // Optimized comparison: avoid push/pop dance.
        // When RHS is a small constant, use cmpq $IMM, %rax directly.
        // Otherwise, load LHS to rax and RHS directly to rcx.
        self.operand_to_rax(lhs);
        if let Operand::Const(c) = rhs {
            if let Some(imm) = c.to_i64() {
                if imm >= i32::MIN as i64 && imm <= i32::MAX as i64 {
                    self.state.emit_fmt(format_args!("    cmpq ${}, %rax", imm));
                } else {
                    self.load_rhs_to_rcx(rhs);
                    self.state.emit("    cmpq %rcx, %rax");
                }
            } else {
                self.load_rhs_to_rcx(rhs);
                self.state.emit("    cmpq %rcx, %rax");
            }
        } else {
            self.load_rhs_to_rcx(rhs);
            self.state.emit("    cmpq %rcx, %rax");
        }

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
        self.store_rax_to(dest);
    }

    // emit_call: uses shared default from ArchCodegen trait (traits.rs)

    fn call_abi_config(&self) -> CallAbiConfig {
        CallAbiConfig {
            max_int_regs: 6, max_float_regs: 8,
            align_i128_pairs: false,
            f128_in_fp_regs: false, f128_in_gp_pairs: false,
            variadic_floats_in_gp: false,
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
                            let f64_val = match c {
                                IrConst::LongDouble(v) => *v,
                                IrConst::F64(v) => *v,
                                _ => c.to_f64().unwrap_or(0.0),
                            };
                            let x87_bytes = crate::ir::ir::f64_to_x87_bytes(f64_val);
                            let lo = u64::from_le_bytes(x87_bytes[0..8].try_into().unwrap());
                            let hi_2bytes = u16::from_le_bytes(x87_bytes[8..10].try_into().unwrap());
                            self.state.emit_fmt(format_args!("    pushq ${}", hi_2bytes as i64));
                            self.state.emit_fmt(format_args!("    movabsq ${}, %rax", lo as i64));
                            self.state.emit("    pushq %rax");
                            self.state.reg_cache.invalidate_all(); // rax clobbered by movabsq
                        }
                        Operand::Value(ref v) => {
                            if let Some(slot) = self.state.get_slot(v.0) {
                                if self.state.is_alloca(v.0) {
                                    self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rax", slot.0));
                                } else {
                                    self.state.emit_fmt(format_args!("    movq {}(%rbp), %rax", slot.0));
                                }
                            } else {
                                self.state.emit("    xorq %rax, %rax");
                            }
                            self.state.reg_cache.invalidate_all(); // rax clobbered by F128 load
                            self.state.emit("    subq $16, %rsp");
                            self.state.emit("    pushq %rax");
                            self.state.emit("    fldl (%rsp)");
                            self.state.emit("    addq $8, %rsp");
                            self.state.emit("    fstpt (%rsp)");
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
            self.state.emit("    call *%r10");
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
            self.state.emit("    subq $8, %rsp");
            self.state.emit("    fstpl (%rsp)");
            self.state.emit("    movq (%rsp), %rax");
            self.state.emit("    addq $8, %rsp");
            self.store_rax_to(dest);
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

    // emit_cast: uses default implementation from ArchCodegen trait (handles i128 via primitives)

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
            // Store result and jump to end
            if let Some(slot) = self.state.get_slot(dest.0) {
                self.state.emit_fmt(format_args!("    movq %rax, {}(%rbp)", slot.0));
            }
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
        // Store result
        if let Some(slot) = self.state.get_slot(dest.0) {
            self.state.emit_fmt(format_args!("    movq %rax, {}(%rbp)", slot.0));
        }
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

        // Load va_list pointer into %rax
        if let Some(slot) = self.state.get_slot(va_list_ptr.0) {
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
        // fp_offset = 48 + min(num_named_fp_params, 8) * 16 (skip named FP params in XMM save area)
        // Each XMM register occupies 16 bytes in the register save area
        // Note: long double (F128) is NOT included here - it's always stack-passed via x87
        let fp_offset = 48 + self.num_named_fp_params.min(8) * 16;
        self.state.emit_fmt(format_args!("    movl ${}, 4(%rax)", fp_offset));
        // overflow_arg_area = rbp + 16 + stack_bytes_for_named_params
        // Stack-passed named params include:
        //   - GP params beyond 6 registers (8 bytes each)
        //   - FP params beyond 8 XMM registers (8 bytes each)
        //   - long double params (always stack-passed, 16 bytes each)
        let num_stack_int = if self.num_named_int_params > 6 { self.num_named_int_params - 6 } else { 0 };
        let num_stack_fp = if self.num_named_fp_params > 8 { self.num_named_fp_params - 8 } else { 0 };
        let overflow_offset = 16 + (num_stack_int + num_stack_fp) * 8 + self.num_named_stack_bytes;
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
        // Copy 24 bytes from src va_list to dest va_list
        if let Some(src_slot) = self.state.get_slot(src_ptr.0) {
            if self.state.is_alloca(src_ptr.0) {
                self.state.emit_fmt(format_args!("    leaq {}(%rbp), %rsi", src_slot.0));
            } else {
                self.state.emit_fmt(format_args!("    movq {}(%rbp), %rsi", src_slot.0));
            }
        }
        if let Some(dest_slot) = self.state.get_slot(dest_ptr.0) {
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

    // emit_return: uses default implementation from ArchCodegen trait

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

    fn emit_inline_asm(&mut self, template: &str, outputs: &[(String, Value, Option<String>)], inputs: &[(String, Operand, Option<String>)], _clobbers: &[String], operand_types: &[IrType], goto_labels: &[(String, BlockId)]) {
        emit_inline_asm_common(self, template, outputs, inputs, operand_types, goto_labels);
        self.state.reg_cache.invalidate_all(); // inline asm may clobber rax
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

    fn emit_float_binop_impl(&mut self, mnemonic: &str, ty: IrType) {
        // secondary = lhs (pushed to stack), acc = rhs
        let (mov_to_xmm0, mov_from_xmm0) = if ty == IrType::F32 {
            ("movd %eax, %xmm0", "movd %xmm0, %eax")
        } else {
            ("movq %rax, %xmm0", "movq %xmm0, %rax")
        };
        let mov_to_xmm1 = if ty == IrType::F32 { "movd %eax, %xmm1" } else { "movq %rax, %xmm1" };
        // rhs is in rax (acc), lhs was pushed (secondary)
        self.state.emit_fmt(format_args!("    {}", mov_to_xmm1)); // rhs -> xmm1
        self.state.emit("    popq %rax"); // lhs from stack
        self.state.reg_cache.invalidate_all();
        self.state.emit_fmt(format_args!("    {}", mov_to_xmm0)); // lhs -> xmm0
        let suffix = if ty == IrType::F64 || ty == IrType::F128 { "sd" } else { "ss" };
        self.state.emit_fmt(format_args!("    {}{} %xmm1, %xmm0", mnemonic, suffix));
        self.state.emit_fmt(format_args!("    {}", mov_from_xmm0));
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

    fn classify_constraint(&self, constraint: &str) -> AsmOperandKind {
        let c = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
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
        // Pure memory constraint
        if c == "m" {
            return AsmOperandKind::Memory;
        }
        // Immediate constraints: "i", "I", "n" — value must be a compile-time constant
        if c == "i" || c == "I" || c == "n" {
            return AsmOperandKind::Immediate;
        }
        // SSE register constraint: "x" means any XMM register
        if c == "x" {
            return AsmOperandKind::FpReg;
        }
        // Specific register constraints
        match c {
            "a" => AsmOperandKind::Specific("rax".to_string()),
            "b" => AsmOperandKind::Specific("rbx".to_string()),
            "c" => AsmOperandKind::Specific("rcx".to_string()),
            "d" => AsmOperandKind::Specific("rdx".to_string()),
            "S" => AsmOperandKind::Specific("rsi".to_string()),
            "D" => AsmOperandKind::Specific("rdi".to_string()),
            _ => AsmOperandKind::GpReg,
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
        if matches!(op.kind, AsmOperandKind::Immediate) {
            // Extract the immediate constant value
            match val {
                Operand::Const(c) => {
                    op.imm_value = c.to_i64();
                }
                _ => {}
            }
        }
    }

    fn resolve_memory_operand(&mut self, op: &mut AsmOperand, val: &Operand) -> bool {
        // If mem_addr is already set (alloca case), nothing to do
        if !op.mem_addr.is_empty() {
            return false;
        }
        // Load the pointer value into a temporary register for indirect addressing
        match val {
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    // Use rax as temporary (saved/restored by caller convention for inline asm)
                    // Actually, use a register that won't conflict - pick from scratch regs
                    let tmp_reg = "r11"; // r11 is caller-saved and unlikely to conflict
                    self.state.emit_fmt(format_args!("    movq {}(%rbp), %{}", slot.0, tmp_reg));
                    op.mem_addr = format!("(%{})", tmp_reg);
                    return true;
                }
            }
            _ => {}
        }
        false
    }

    fn assign_scratch_reg(&mut self, kind: &AsmOperandKind) -> String {
        if matches!(kind, AsmOperandKind::FpReg) {
            let idx = self.asm_xmm_scratch_idx;
            self.asm_xmm_scratch_idx += 1;
            if idx < X86_XMM_SCRATCH.len() {
                X86_XMM_SCRATCH[idx].to_string()
            } else {
                format!("xmm{}", idx)
            }
        } else {
            let idx = self.asm_scratch_idx;
            self.asm_scratch_idx += 1;
            if idx < X86_GP_SCRATCH.len() {
                X86_GP_SCRATCH[idx].to_string()
            } else {
                format!("r{}", 12 + idx - X86_GP_SCRATCH.len())
            }
        }
    }

    fn load_input_to_reg(&mut self, op: &AsmOperand, val: &Operand, _constraint: &str) {
        let reg = &op.reg;
        let ty = op.operand_type;
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
        let reg = &op.reg;
        let ty = op.operand_type;
        let is_xmm = reg.starts_with("xmm");
        if let Some(slot) = self.state.get_slot(ptr.0) {
            if is_xmm {
                let load_instr = match ty {
                    IrType::F32 => "movss",
                    _ => "movsd",
                };
                self.state.emit_fmt(format_args!("    {} {}(%rbp), %{}", load_instr, slot.0, reg));
            } else {
                // Use type-appropriate load to correctly handle byte/word/dword variables
                let load_instr = Self::mov_load_for_type(ty);
                let dest_reg = match ty {
                    IrType::U32 | IrType::F32 => format!("%{}", Self::reg_to_32(reg)),
                    _ => format!("%{}", reg),
                };
                self.state.emit_fmt(format_args!("    {} {}(%rbp), {}", load_instr, slot.0, dest_reg));
            }
        }
    }

    fn substitute_template_line(&self, line: &str, operands: &[AsmOperand], gcc_to_internal: &[usize], operand_types: &[IrType], goto_labels: &[(String, BlockId)]) -> String {
        // Build the parallel arrays that substitute_x86_asm_operands expects
        let op_regs: Vec<String> = operands.iter().map(|o| o.reg.clone()).collect();
        let op_names: Vec<Option<String>> = operands.iter().map(|o| o.name.clone()).collect();
        let op_is_memory: Vec<bool> = operands.iter().map(|o| matches!(o.kind, AsmOperandKind::Memory)).collect();
        let op_mem_addrs: Vec<String> = operands.iter().map(|o| o.mem_addr.clone()).collect();
        let op_imm_values: Vec<Option<i64>> = operands.iter().map(|o| o.imm_value).collect();

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

        Self::substitute_x86_asm_operands(line, &op_regs, &op_names, &op_is_memory, &op_mem_addrs, &op_types, gcc_to_internal, goto_labels, &op_imm_values)
    }

    fn store_output_from_reg(&mut self, op: &AsmOperand, ptr: &Value, _constraint: &str) {
        if matches!(op.kind, AsmOperandKind::Memory) {
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
