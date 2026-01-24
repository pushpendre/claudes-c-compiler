//! Shared code generation framework for all backends.
//!
//! All three backends (x86-64, AArch64, RISC-V 64) use the same stack-based
//! code generation strategy: each IR value is assigned a stack slot, and
//! instructions move values through a primary accumulator register.
//!
//! This module extracts the shared instruction dispatch and function generation
//! logic. Each architecture implements `ArchCodegen` to provide its specific
//! register names, instruction mnemonics, and ABI details.

use std::collections::{HashMap, HashSet};
use crate::ir::ir::*;
use crate::common::types::IrType;
use super::common::{self, AsmOutput, PtrDirective};

/// Stack slot location for a value. Interpretation varies by arch:
/// - x86: negative offset from %rbp
/// - ARM: positive offset from sp
/// - RISC-V: negative offset from s0
#[derive(Debug, Clone, Copy)]
pub struct StackSlot(pub i64);

/// Shared codegen state, used by all backends.
pub struct CodegenState {
    pub out: AsmOutput,
    pub stack_offset: i64,
    pub value_locations: HashMap<u32, StackSlot>,
    /// Values that are allocas (their stack slot IS the data, not a pointer to data).
    pub alloca_values: HashSet<u32>,
    /// Type associated with each alloca (for type-aware loads/stores).
    pub alloca_types: HashMap<u32, IrType>,
    /// Counter for generating unique labels (e.g., memcpy loops).
    label_counter: u32,
}

impl CodegenState {
    pub fn new() -> Self {
        Self {
            out: AsmOutput::new(),
            stack_offset: 0,
            value_locations: HashMap::new(),
            alloca_values: HashSet::new(),
            alloca_types: HashMap::new(),
            label_counter: 0,
        }
    }

    pub fn next_label_id(&mut self) -> u32 {
        let id = self.label_counter;
        self.label_counter += 1;
        id
    }

    /// Generate a fresh label with the given prefix.
    pub fn fresh_label(&mut self, prefix: &str) -> String {
        let id = self.next_label_id();
        format!(".L{}_{}", prefix, id)
    }

    pub fn emit(&mut self, s: &str) {
        self.out.emit(s);
    }

    pub fn reset_for_function(&mut self) {
        self.stack_offset = 0;
        self.value_locations.clear();
        self.alloca_values.clear();
        self.alloca_types.clear();
    }

    pub fn is_alloca(&self, v: u32) -> bool {
        self.alloca_values.contains(&v)
    }

    pub fn get_slot(&self, v: u32) -> Option<StackSlot> {
        self.value_locations.get(&v).copied()
    }
}

/// Trait that each architecture implements to provide its specific code generation.
///
/// The shared framework calls these methods during instruction dispatch.
/// Each method should emit the appropriate assembly instructions.
pub trait ArchCodegen {
    /// Mutable access to the shared codegen state.
    fn state(&mut self) -> &mut CodegenState;
    /// Immutable access to the shared codegen state.
    fn state_ref(&self) -> &CodegenState;

    /// The pointer directive for this architecture's data emission.
    fn ptr_directive(&self) -> PtrDirective;

    /// Calculate stack space and assign locations for all values in the function.
    /// Returns the raw stack space needed (before alignment).
    fn calculate_stack_space(&mut self, func: &IrFunction) -> i64;

    /// Emit function prologue (save frame pointer, allocate stack).
    fn emit_prologue(&mut self, func: &IrFunction, frame_size: i64);

    /// Emit function epilogue (restore frame pointer, deallocate stack).
    fn emit_epilogue(&mut self, frame_size: i64);

    /// Store function parameters from argument registers to their stack slots.
    fn emit_store_params(&mut self, func: &IrFunction);

    /// Load an operand into the primary accumulator register (rax/x0/t0).
    fn emit_load_operand(&mut self, op: &Operand);

    /// Store the primary accumulator to a value's stack slot.
    fn emit_store_result(&mut self, dest: &Value);

    /// Emit a store instruction: store val to the address in ptr.
    fn emit_store(&mut self, val: &Operand, ptr: &Value, ty: IrType);

    /// Emit a load instruction: load from the address in ptr to dest.
    fn emit_load(&mut self, dest: &Value, ptr: &Value, ty: IrType);

    /// Emit a binary operation.
    fn emit_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, ty: IrType);

    /// Emit a unary operation.
    fn emit_unaryop(&mut self, dest: &Value, op: IrUnaryOp, src: &Operand, ty: IrType);

    /// Emit a comparison operation.
    fn emit_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand, ty: IrType);

    /// Emit a function call (direct or indirect).
    /// `arg_types` provides the IR type of each argument for proper calling convention handling.
    /// `is_variadic` indicates the called function uses variadic arguments (affects calling convention).
    /// `num_fixed_args` is the number of named (non-variadic) parameters. For variadic calls on
    /// AArch64, arguments beyond this index must be passed in GP registers even if they are floats.
    fn emit_call(&mut self, args: &[Operand], arg_types: &[IrType], direct_name: Option<&str>,
                 func_ptr: Option<&Operand>, dest: Option<Value>, return_type: IrType,
                 is_variadic: bool, num_fixed_args: usize);

    /// Emit a global address load.
    fn emit_global_addr(&mut self, dest: &Value, name: &str);

    /// Emit a get-element-pointer (base + offset).
    fn emit_gep(&mut self, dest: &Value, base: &Value, offset: &Operand);

    /// Emit a type cast.
    fn emit_cast(&mut self, dest: &Value, src: &Operand, from_ty: IrType, to_ty: IrType);

    /// Emit a memory copy: copy `size` bytes from src address to dest address.
    fn emit_memcpy(&mut self, dest: &Value, src: &Value, size: usize);

    /// Emit va_arg: extract next variadic argument from va_list and store to dest.
    fn emit_va_arg(&mut self, dest: &Value, va_list_ptr: &Value, result_ty: IrType);

    /// Emit va_start: initialize a va_list for variadic argument access.
    fn emit_va_start(&mut self, va_list_ptr: &Value);

    /// Emit va_end: clean up a va_list (typically no-op).
    fn emit_va_end(&mut self, va_list_ptr: &Value);

    /// Emit va_copy: copy src va_list to dest va_list.
    fn emit_va_copy(&mut self, dest_ptr: &Value, src_ptr: &Value);

    /// Emit an atomic read-modify-write operation.
    fn emit_atomic_rmw(&mut self, dest: &Value, op: AtomicRmwOp, ptr: &Operand, val: &Operand, ty: IrType, ordering: AtomicOrdering);

    /// Emit an atomic compare-and-exchange operation.
    fn emit_atomic_cmpxchg(&mut self, dest: &Value, ptr: &Operand, expected: &Operand, desired: &Operand, ty: IrType, success_ordering: AtomicOrdering, failure_ordering: AtomicOrdering, returns_bool: bool);

    /// Emit an atomic load.
    fn emit_atomic_load(&mut self, dest: &Value, ptr: &Operand, ty: IrType, ordering: AtomicOrdering);

    /// Emit an atomic store.
    fn emit_atomic_store(&mut self, ptr: &Operand, val: &Operand, ty: IrType, ordering: AtomicOrdering);

    /// Emit a memory fence.
    fn emit_fence(&mut self, ordering: AtomicOrdering);

    /// Emit inline assembly.
    fn emit_inline_asm(&mut self, template: &str, outputs: &[(String, Value, Option<String>)], inputs: &[(String, Operand, Option<String>)], clobbers: &[String], operand_types: &[IrType]);

    /// Emit a return terminator.
    fn emit_return(&mut self, val: Option<&Operand>, frame_size: i64);

    /// Emit an unconditional branch.
    fn emit_branch(&mut self, label: &str);

    /// Emit a conditional branch.
    fn emit_cond_branch(&mut self, cond: &Operand, true_label: &str, false_label: &str);

    /// Emit an unreachable trap instruction.
    fn emit_unreachable(&mut self);

    /// Emit a label address load (GCC &&label extension for computed goto).
    fn emit_label_addr(&mut self, dest: &Value, label: &str);

    /// Emit an indirect branch (computed goto: goto *addr).
    fn emit_indirect_branch(&mut self, target: &Operand);

    /// Emit the function directive for the function type attribute.
    /// x86 uses "@function", ARM uses "%function".
    fn function_type_directive(&self) -> &'static str { "@function" }

    /// Frame size including alignment and saved registers.
    fn aligned_frame_size(&self, raw_space: i64) -> i64;
}

/// Generate assembly for a module using the given architecture's codegen.
pub fn generate_module(cg: &mut dyn ArchCodegen, module: &IrModule) -> String {
    // Emit data sections
    let ptr_dir = cg.ptr_directive();
    common::emit_data_sections(&mut cg.state().out, module, ptr_dir);

    // Text section
    cg.state().emit(".section .text");
    for func in &module.functions {
        if !func.is_declaration {
            generate_function(cg, func);
        }
    }

    // Emit .note.GNU-stack section to indicate non-executable stack
    cg.state().emit("");
    cg.state().emit(".section .note.GNU-stack,\"\",@progbits");

    std::mem::take(&mut cg.state().out.buf)
}

/// Generate code for a single function.
fn generate_function(cg: &mut dyn ArchCodegen, func: &IrFunction) {
    cg.state().reset_for_function();

    let type_dir = cg.function_type_directive();
    if !func.is_static {
        cg.state().emit(&format!(".globl {}", func.name));
    }
    cg.state().emit(&format!(".type {}, {}", func.name, type_dir));
    cg.state().emit(&format!("{}:", func.name));

    // Calculate stack space and emit prologue
    let raw_space = cg.calculate_stack_space(func);
    let frame_size = cg.aligned_frame_size(raw_space);
    cg.emit_prologue(func, frame_size);

    // Store parameters
    cg.emit_store_params(func);

    // Generate basic blocks
    for block in &func.blocks {
        if block.label != "entry" {
            cg.state().emit(&format!("{}:", block.label));
        }
        for inst in &block.instructions {
            generate_instruction(cg, inst);
        }
        generate_terminator(cg, &block.terminator, frame_size);
    }

    cg.state().emit(&format!(".size {}, .-{}", func.name, func.name));
    cg.state().emit("");
}

/// Dispatch a single IR instruction to the appropriate arch method.
fn generate_instruction(cg: &mut dyn ArchCodegen, inst: &Instruction) {
    match inst {
        Instruction::Alloca { .. } => {
            // Space already allocated in prologue
        }
        Instruction::Store { val, ptr, ty } => {
            cg.emit_store(val, ptr, *ty);
        }
        Instruction::Load { dest, ptr, ty } => {
            cg.emit_load(dest, ptr, *ty);
        }
        Instruction::BinOp { dest, op, lhs, rhs, ty } => {
            cg.emit_binop(dest, *op, lhs, rhs, *ty);
        }
        Instruction::UnaryOp { dest, op, src, ty } => {
            cg.emit_unaryop(dest, *op, src, *ty);
        }
        Instruction::Cmp { dest, op, lhs, rhs, ty } => {
            cg.emit_cmp(dest, *op, lhs, rhs, *ty);
        }
        Instruction::Call { dest, func, args, arg_types, return_type, is_variadic, num_fixed_args } => {
            cg.emit_call(args, arg_types, Some(func), None, *dest, *return_type, *is_variadic, *num_fixed_args);
        }
        Instruction::CallIndirect { dest, func_ptr, args, arg_types, return_type, is_variadic, num_fixed_args } => {
            cg.emit_call(args, arg_types, None, Some(func_ptr), *dest, *return_type, *is_variadic, *num_fixed_args);
        }
        Instruction::GlobalAddr { dest, name } => {
            cg.emit_global_addr(dest, name);
        }
        Instruction::Copy { dest, src } => {
            cg.emit_load_operand(src);
            cg.emit_store_result(dest);
        }
        Instruction::Cast { dest, src, from_ty, to_ty } => {
            cg.emit_cast(dest, src, *from_ty, *to_ty);
        }
        Instruction::GetElementPtr { dest, base, offset, .. } => {
            cg.emit_gep(dest, base, offset);
        }
        Instruction::Memcpy { dest, src, size } => {
            cg.emit_memcpy(dest, src, *size);
        }
        Instruction::VaArg { dest, va_list_ptr, result_ty } => {
            cg.emit_va_arg(dest, va_list_ptr, *result_ty);
        }
        Instruction::VaStart { va_list_ptr } => {
            cg.emit_va_start(va_list_ptr);
        }
        Instruction::VaEnd { va_list_ptr } => {
            cg.emit_va_end(va_list_ptr);
        }
        Instruction::VaCopy { dest_ptr, src_ptr } => {
            cg.emit_va_copy(dest_ptr, src_ptr);
        }
        Instruction::AtomicRmw { dest, op, ptr, val, ty, ordering } => {
            cg.emit_atomic_rmw(dest, *op, ptr, val, *ty, *ordering);
        }
        Instruction::AtomicCmpxchg { dest, ptr, expected, desired, ty, success_ordering, failure_ordering, returns_bool } => {
            cg.emit_atomic_cmpxchg(dest, ptr, expected, desired, *ty, *success_ordering, *failure_ordering, *returns_bool);
        }
        Instruction::AtomicLoad { dest, ptr, ty, ordering } => {
            cg.emit_atomic_load(dest, ptr, *ty, *ordering);
        }
        Instruction::AtomicStore { ptr, val, ty, ordering } => {
            cg.emit_atomic_store(ptr, val, *ty, *ordering);
        }
        Instruction::Fence { ordering } => {
            cg.emit_fence(*ordering);
        }
        Instruction::Phi { .. } => {
            // Phi nodes are resolved before codegen by lowering them to copies
            // at the end of predecessor blocks. This case should not be reached
            // in normal operation, but is a no-op for safety.
        }
        Instruction::LabelAddr { dest, label } => {
            cg.emit_label_addr(dest, label);
        }
        Instruction::InlineAsm { template, outputs, inputs, clobbers, operand_types } => {
            cg.emit_inline_asm(template, outputs, inputs, clobbers, operand_types);
        }
    }
}

/// Dispatch a terminator to the appropriate arch method.
fn generate_terminator(cg: &mut dyn ArchCodegen, term: &Terminator, frame_size: i64) {
    match term {
        Terminator::Return(val) => {
            cg.emit_return(val.as_ref(), frame_size);
        }
        Terminator::Branch(label) => {
            cg.emit_branch(label);
        }
        Terminator::CondBranch { cond, true_label, false_label } => {
            cg.emit_cond_branch(cond, true_label, false_label);
        }
        Terminator::IndirectBranch { target, .. } => {
            cg.emit_indirect_branch(target);
        }
        Terminator::Unreachable => {
            cg.emit_unreachable();
        }
    }
}

/// Shared stack space calculation: iterates over all instructions, assigns stack
/// slots for allocas and value results. Arch-specific offset direction is handled
/// by the `assign_slot` closure.
///
/// `initial_offset`: starting offset (e.g., 0 for x86, 16 for ARM/RISC-V to skip saved regs)
/// `assign_slot`: maps (current_space, raw_alloca_size) -> (slot_offset, new_space)
///   - raw_alloca_size is the alloca byte count (0 means pointer-sized, i.e., 8)
///   - For non-alloca values, raw_alloca_size is always 8
///   - The closure is responsible for alignment
pub fn calculate_stack_space_common(
    state: &mut CodegenState,
    func: &IrFunction,
    initial_offset: i64,
    assign_slot: impl Fn(i64, i64) -> (i64, i64),
) -> i64 {
    let mut space = initial_offset;
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::Alloca { dest, size, ty, .. } = inst {
                let raw_size = if *size == 0 { 8 } else { *size as i64 };
                let (slot, new_space) = assign_slot(space, raw_size);
                state.value_locations.insert(dest.0, StackSlot(slot));
                state.alloca_values.insert(dest.0);
                state.alloca_types.insert(dest.0, *ty);
                space = new_space;
            } else if let Some(dest) = inst.dest() {
                let (slot, new_space) = assign_slot(space, 8);
                state.value_locations.insert(dest.0, StackSlot(slot));
                space = new_space;
            }
        }
    }
    space
}

// ============================================================================
// Shared cast classification
// ============================================================================

/// Classification of type casts. All three backends use the same control flow
/// to decide which kind of cast to emit; only the actual instructions differ.
/// By classifying the cast once in shared code, we eliminate duplicated
/// Ptr-normalization and F128-reduction logic from each backend.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CastKind {
    /// No conversion needed (same type, or Ptr <-> I64/U64, or F128 <-> F64).
    Noop,
    /// Float to signed integer (from_ty is F32 or F64).
    FloatToSigned { from_f64: bool },
    /// Float to unsigned integer (from_ty is F32 or F64).
    FloatToUnsigned { from_f64: bool, to_u64: bool },
    /// Signed integer to float (to_ty is F32 or F64).
    SignedToFloat { to_f64: bool },
    /// Unsigned integer to float. `from_u64` indicates U64 source needing
    /// special overflow handling on x86.
    UnsignedToFloat { to_f64: bool, from_u64: bool },
    /// Float-to-float conversion (F32 <-> F64).
    FloatToFloat { widen: bool },
    /// Integer widening: sign- or zero-extend a smaller type to a larger one.
    IntWiden { from_ty: IrType, to_ty: IrType },
    /// Integer narrowing: truncate a larger type to a smaller one.
    IntNarrow { to_ty: IrType },
    /// Same-size signed-to-unsigned (need to mask/clear upper bits).
    SignedToUnsignedSameSize { to_ty: IrType },
}

/// Classify a cast between two IR types. This captures the shared decision logic
/// that all three backends use identically. Backends then match on the returned
/// `CastKind` to emit architecture-specific instructions.
///
/// Handles Ptr normalization (Ptr treated as U64) and F128 reduction (F128 treated
/// as F64 for computation purposes) before classification.
pub fn classify_cast(from_ty: IrType, to_ty: IrType) -> CastKind {
    if from_ty == to_ty {
        return CastKind::Noop;
    }

    // F128 (long double) is computed as F64. Treat F128 <-> F64 as noop,
    // and F128 <-> other as F64 <-> other.
    if from_ty == IrType::F128 || to_ty == IrType::F128 {
        let effective_from = if from_ty == IrType::F128 { IrType::F64 } else { from_ty };
        let effective_to = if to_ty == IrType::F128 { IrType::F64 } else { to_ty };
        if effective_from == effective_to {
            return CastKind::Noop;
        }
        return classify_cast(effective_from, effective_to);
    }

    // Ptr is equivalent to U64 on all 64-bit targets.
    if (from_ty == IrType::Ptr || to_ty == IrType::Ptr) && !from_ty.is_float() && !to_ty.is_float() {
        let effective_from = if from_ty == IrType::Ptr { IrType::U64 } else { from_ty };
        let effective_to = if to_ty == IrType::Ptr { IrType::U64 } else { to_ty };
        if effective_from == effective_to || (effective_from.size() == 8 && effective_to.size() == 8) {
            return CastKind::Noop;
        }
        return classify_cast(effective_from, effective_to);
    }

    // Float-to-int
    if from_ty.is_float() && !to_ty.is_float() {
        let is_unsigned_dest = to_ty.is_unsigned() || to_ty == IrType::Ptr;
        let from_f64 = from_ty == IrType::F64;
        if is_unsigned_dest {
            let to_u64 = to_ty == IrType::U64 || to_ty == IrType::Ptr;
            return CastKind::FloatToUnsigned { from_f64, to_u64 };
        } else {
            return CastKind::FloatToSigned { from_f64 };
        }
    }

    // Int-to-float
    if !from_ty.is_float() && to_ty.is_float() {
        let is_unsigned_src = from_ty.is_unsigned();
        let to_f64 = to_ty == IrType::F64;
        if is_unsigned_src {
            let from_u64 = from_ty == IrType::U64;
            return CastKind::UnsignedToFloat { to_f64, from_u64 };
        } else {
            return CastKind::SignedToFloat { to_f64 };
        }
    }

    // Float-to-float
    if from_ty.is_float() && to_ty.is_float() {
        let widen = from_ty == IrType::F32 && to_ty == IrType::F64;
        return CastKind::FloatToFloat { widen };
    }

    // Integer-to-integer
    let from_size = from_ty.size();
    let to_size = to_ty.size();

    if from_size == to_size {
        // Same size, different signedness
        if from_ty.is_signed() && to_ty.is_unsigned() {
            return CastKind::SignedToUnsignedSameSize { to_ty };
        }
        return CastKind::Noop;
    }

    if to_size > from_size {
        return CastKind::IntWiden { from_ty, to_ty };
    }

    CastKind::IntNarrow { to_ty }
}

// ============================================================================
// Shared float operation helpers
// ============================================================================

/// Classify a binary operation on floats. Returns None if the operation is not
/// meaningful on floats (e.g., bitwise And, Or, Xor, shifts, integer remainder).
/// This prevents the latent bug where all three backends silently emit `fadd`
/// for unsupported float operations.
pub fn classify_float_binop(op: IrBinOp) -> Option<FloatOp> {
    match op {
        IrBinOp::Add => Some(FloatOp::Add),
        IrBinOp::Sub => Some(FloatOp::Sub),
        IrBinOp::Mul => Some(FloatOp::Mul),
        IrBinOp::SDiv | IrBinOp::UDiv => Some(FloatOp::Div),
        // Bitwise ops, shifts, and remainder are not meaningful for floats.
        // Falling through to Add was a latent bug in all three backends.
        _ => None,
    }
}

/// Float arithmetic operations that all three backends support.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatOp {
    Add,
    Sub,
    Mul,
    Div,
}

/// Find the nth alloca instruction in the entry block (used for parameter storage).
pub fn find_param_alloca(func: &IrFunction, param_idx: usize) -> Option<(Value, IrType)> {
    func.blocks.first().and_then(|block| {
        block.instructions.iter()
            .filter(|i| matches!(i, Instruction::Alloca { .. }))
            .nth(param_idx)
            .and_then(|inst| {
                if let Instruction::Alloca { dest, ty, .. } = inst {
                    Some((*dest, *ty))
                } else {
                    None
                }
            })
    })
}
