//! ArchCodegen trait: the interface each backend implements.
//!
//! This trait defines ~100 methods that each architecture must implement to provide
//! its specific register names, instruction mnemonics, and ABI details. The trait
//! also provides ~20 default method implementations that capture shared codegen
//! patterns (store/load dispatch, cast handling, i128 operations, control flow).
//!
//! The default implementations are built from small "primitive" methods that each
//! backend overrides with 1-4 line arch-specific implementations. This design lets
//! the shared framework express the algorithm once while backends only provide the
//! instruction-level differences.

use crate::ir::ir::*;
use crate::common::types::IrType;
use super::common::PtrDirective;
use super::state::{CodegenState, SlotAddr, StackSlot};
use super::cast::{FloatOp, classify_float_binop};
use super::generation::is_i128_type;

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

    /// Compute the runtime-aligned address of an over-aligned alloca into the
    /// pointer register (same register used by emit_load_ptr_from_slot: rcx on x86).
    fn emit_alloca_aligned_addr(&mut self, slot: StackSlot, val_id: u32);

    /// Compute aligned alloca address into the accumulator (rax/x0/a0).
    fn emit_alloca_aligned_addr_to_acc(&mut self, slot: StackSlot, val_id: u32);

    /// Emit a store instruction: store val to the address in ptr.
    /// Default implementation uses `SlotAddr` to dispatch the 3-way
    /// alloca/over-aligned/indirect pattern once for both i128 and typed stores.
    fn emit_store(&mut self, val: &Operand, ptr: &Value, ty: IrType) {
        let addr = self.state_ref().resolve_slot_addr(ptr.0);
        if is_i128_type(ty) {
            self.emit_load_acc_pair(val);
            if let Some(addr) = addr {
                match addr {
                    SlotAddr::OverAligned(slot, id) => {
                        self.emit_save_acc_pair();
                        self.emit_alloca_aligned_addr(slot, id);
                        self.emit_store_pair_indirect();
                    }
                    SlotAddr::Direct(slot) => self.emit_store_pair_to_slot(slot),
                    SlotAddr::Indirect(slot) => {
                        self.emit_save_acc_pair();
                        self.emit_load_ptr_from_slot(slot);
                        self.emit_store_pair_indirect();
                    }
                }
            }
            return;
        }
        self.emit_load_operand(val);
        if let Some(addr) = addr {
            let store_instr = self.store_instr_for_type(ty);
            match addr {
                SlotAddr::OverAligned(slot, id) => {
                    self.emit_save_acc();
                    self.emit_alloca_aligned_addr(slot, id);
                    self.emit_typed_store_indirect(store_instr, ty);
                }
                SlotAddr::Direct(slot) => self.emit_typed_store_to_slot(store_instr, ty, slot),
                SlotAddr::Indirect(slot) => {
                    self.emit_save_acc();
                    self.emit_load_ptr_from_slot(slot);
                    self.emit_typed_store_indirect(store_instr, ty);
                }
            }
        }
    }

    /// Emit a load instruction: load from the address in ptr to dest.
    /// Default implementation uses `SlotAddr` to dispatch the 3-way pattern.
    fn emit_load(&mut self, dest: &Value, ptr: &Value, ty: IrType) {
        let addr = self.state_ref().resolve_slot_addr(ptr.0);
        if is_i128_type(ty) {
            if let Some(addr) = addr {
                match addr {
                    SlotAddr::OverAligned(slot, id) => {
                        self.emit_alloca_aligned_addr(slot, id);
                        self.emit_load_pair_indirect();
                    }
                    SlotAddr::Direct(slot) => self.emit_load_pair_from_slot(slot),
                    SlotAddr::Indirect(slot) => {
                        self.emit_load_ptr_from_slot(slot);
                        self.emit_load_pair_indirect();
                    }
                }
                self.emit_store_acc_pair(dest);
            }
            return;
        }
        if let Some(addr) = addr {
            let load_instr = self.load_instr_for_type(ty);
            match addr {
                SlotAddr::OverAligned(slot, id) => {
                    self.emit_alloca_aligned_addr(slot, id);
                    self.emit_typed_load_indirect(load_instr);
                }
                SlotAddr::Direct(slot) => self.emit_typed_load_from_slot(load_instr, slot),
                SlotAddr::Indirect(slot) => {
                    self.emit_load_ptr_from_slot(slot);
                    self.emit_typed_load_indirect(load_instr);
                }
            }
            self.emit_store_result(dest);
        }
    }

    /// Emit a binary operation. Default: dispatches i128/float/integer ops.
    fn emit_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if is_i128_type(ty) {
            self.emit_i128_binop(dest, op, lhs, rhs);
            return;
        }
        if ty.is_float() {
            let float_op = classify_float_binop(op)
                .unwrap_or_else(|| panic!("unsupported float binop: {:?} on type {:?}", op, ty));
            self.emit_float_binop(dest, float_op, lhs, rhs, ty);
            return;
        }
        self.emit_int_binop(dest, op, lhs, rhs, ty);
    }

    /// Emit a float binary operation (add/sub/mul/div).
    fn emit_float_binop(&mut self, dest: &Value, op: FloatOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        let mnemonic = self.emit_float_binop_mnemonic(op);
        self.emit_load_operand(lhs);
        self.emit_acc_to_secondary();
        self.emit_load_operand(rhs);
        self.emit_float_binop_impl(mnemonic, ty);
        self.emit_store_result(dest);
    }

    /// Get the instruction mnemonic for a float binary op.
    fn emit_float_binop_mnemonic(&self, op: FloatOp) -> &'static str;

    /// Emit the arch-specific float binop instructions.
    fn emit_float_binop_impl(&mut self, mnemonic: &str, ty: IrType);

    /// Emit an integer binary operation (all IrBinOp variants).
    fn emit_int_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, ty: IrType);

    /// Emit a unary operation.
    /// Default dispatches i128/float/int to arch-specific primitives.
    fn emit_unaryop(&mut self, dest: &Value, op: IrUnaryOp, src: &Operand, ty: IrType) {
        if is_i128_type(ty) {
            self.emit_load_acc_pair(src);
            match op {
                IrUnaryOp::Neg => self.emit_i128_neg(),
                IrUnaryOp::Not => self.emit_i128_not(),
                _ => {}
            }
            self.emit_store_acc_pair(dest);
            return;
        }
        self.emit_load_operand(src);
        if ty.is_float() {
            match op {
                IrUnaryOp::Neg => self.emit_float_neg(ty),
                IrUnaryOp::Not => self.emit_int_not(ty),
                _ => {}
            }
        } else {
            match op {
                IrUnaryOp::Neg => self.emit_int_neg(ty),
                IrUnaryOp::Not => self.emit_int_not(ty),
                IrUnaryOp::Clz => self.emit_int_clz(ty),
                IrUnaryOp::Ctz => self.emit_int_ctz(ty),
                IrUnaryOp::Bswap => self.emit_int_bswap(ty),
                IrUnaryOp::Popcount => self.emit_int_popcount(ty),
            }
        }
        self.emit_store_result(dest);
    }

    /// Emit a comparison operation.
    fn emit_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand, ty: IrType);

    /// Emit a function call (direct or indirect).
    ///
    /// The default implementation provides the shared algorithmic skeleton that all three
    /// architectures follow: classify args → emit stack args → load register args → call → cleanup → store result.
    /// Backends override the small `emit_call_*` hook methods instead of reimplementing this entire method.
    fn emit_call(&mut self, args: &[Operand], arg_types: &[IrType], direct_name: Option<&str>,
                 func_ptr: Option<&Operand>, dest: Option<Value>, return_type: IrType,
                 is_variadic: bool, _num_fixed_args: usize, struct_arg_sizes: &[Option<usize>]) {
        use super::call_abi::*;
        let config = self.call_abi_config();
        let arg_classes = classify_call_args(args, arg_types, struct_arg_sizes, is_variadic, &config);

        // Phase 0: Spill indirect function pointer before any stack manipulation.
        let indirect = func_ptr.is_some() && direct_name.is_none();
        if indirect {
            self.emit_call_spill_fptr(func_ptr.unwrap());
        }

        // Compute stack space needed for overflow args.
        let stack_arg_space = self.emit_call_compute_stack_space(&arg_classes);

        // Phase 1: Pre-convert F128 values that need helper calls (before stack args clobber regs).
        let f128_temp_space = self.emit_call_f128_pre_convert(args, &arg_classes, arg_types, stack_arg_space);

        // Phase 2: Emit stack overflow args.
        let total_sp_adjust = self.emit_call_stack_args(args, &arg_classes, arg_types, stack_arg_space,
                                                        if indirect { self.emit_call_fptr_spill_size() } else { 0 },
                                                        f128_temp_space);

        // Phase 3: Load register args (GP, FP, i128, struct-by-val, F128).
        self.emit_call_reg_args(args, &arg_classes, arg_types, total_sp_adjust, f128_temp_space, stack_arg_space);

        // Phase 4: Emit the actual call instruction.
        self.emit_call_instruction(direct_name, func_ptr, indirect, stack_arg_space);

        // Phase 5: Clean up stack.
        self.emit_call_cleanup(stack_arg_space, f128_temp_space, indirect);

        // Phase 6: Store return value.
        if let Some(dest) = dest {
            self.emit_call_store_result(&dest, return_type);
        }
    }

    // ---- Call hook methods (overridden by each backend) ----

    /// Return the ABI configuration for this architecture's function calls.
    fn call_abi_config(&self) -> super::call_abi::CallAbiConfig;

    /// Compute how much stack space to allocate for overflow arguments.
    /// x86 returns raw push bytes; ARM/RISC-V return pre-allocated SP space.
    fn emit_call_compute_stack_space(&self, arg_classes: &[super::call_abi::CallArgClass]) -> usize;

    /// Spill an indirect function pointer to a safe location before stack manipulation.
    /// No-op on x86 (uses r10). ARM/RISC-V spill to stack.
    fn emit_call_spill_fptr(&mut self, func_ptr: &Operand) { let _ = func_ptr; }

    /// Size of the function pointer spill slot (0 for x86, 16 for ARM).
    fn emit_call_fptr_spill_size(&self) -> usize { 0 }

    /// Pre-convert F128 variable arguments that need __extenddftf2/__trunctfdf2.
    /// Returns the temp stack space allocated for converted results.
    fn emit_call_f128_pre_convert(&mut self, _args: &[Operand], _arg_classes: &[super::call_abi::CallArgClass],
                                   _arg_types: &[IrType], _stack_arg_space: usize) -> usize { 0 }

    /// Emit stack overflow arguments. Returns total SP adjustment (stack_arg_space + fptr_spill + f128_temp).
    fn emit_call_stack_args(&mut self, args: &[Operand], arg_classes: &[super::call_abi::CallArgClass],
                            arg_types: &[IrType], stack_arg_space: usize, fptr_spill: usize, f128_temp_space: usize) -> i64;

    /// Load arguments into registers (GP, FP, i128, struct-by-val, F128).
    fn emit_call_reg_args(&mut self, args: &[Operand], arg_classes: &[super::call_abi::CallArgClass],
                          arg_types: &[IrType], total_sp_adjust: i64, f128_temp_space: usize, stack_arg_space: usize);

    /// Emit the call/bl/jalr instruction.
    /// `stack_arg_space` is passed so ARM can reload the spilled fptr at the correct offset.
    fn emit_call_instruction(&mut self, direct_name: Option<&str>, func_ptr: Option<&Operand>, indirect: bool, stack_arg_space: usize);

    /// Clean up stack space after the call returns.
    fn emit_call_cleanup(&mut self, stack_arg_space: usize, f128_temp_space: usize, indirect: bool);

    /// Store the function's return value from ABI registers to the destination slot.
    fn emit_call_store_result(&mut self, dest: &Value, return_type: IrType);

    /// Emit a global address load.
    fn emit_global_addr(&mut self, dest: &Value, name: &str);

    /// Emit a get-element-pointer (base + offset).
    fn emit_gep(&mut self, dest: &Value, base: &Value, offset: &Operand) {
        if let Some(addr) = self.state_ref().resolve_slot_addr(base.0) {
            match addr {
                SlotAddr::OverAligned(slot, id) => {
                    self.emit_alloca_aligned_addr_to_acc(slot, id);
                    self.emit_acc_to_secondary();
                }
                SlotAddr::Direct(slot) => self.emit_slot_addr_to_secondary(slot, true),
                SlotAddr::Indirect(slot) => self.emit_slot_addr_to_secondary(slot, false),
            }
        }
        self.emit_load_operand(offset);
        self.emit_add_secondary_to_acc();
        self.emit_store_result(dest);
    }

    /// Move accumulator to secondary register (push on x86).
    fn emit_acc_to_secondary(&mut self);

    /// Emit architecture-specific instructions for a type cast.
    fn emit_cast_instrs(&mut self, from_ty: IrType, to_ty: IrType);

    /// Emit a type cast. Handles i128 widening/narrowing/copy using accumulator
    /// pair primitives, and delegates non-i128 casts to emit_cast_instrs.
    fn emit_cast(&mut self, dest: &Value, src: &Operand, from_ty: IrType, to_ty: IrType) {
        if is_i128_type(to_ty) && !is_i128_type(from_ty) {
            self.emit_load_operand(src);
            if from_ty.size() < 8 {
                let widen_to = if from_ty.is_signed() { IrType::I64 } else { IrType::U64 };
                self.emit_cast_instrs(from_ty, widen_to);
            }
            if from_ty.is_signed() {
                self.emit_sign_extend_acc_high();
            } else {
                self.emit_zero_acc_high();
            }
            self.emit_store_acc_pair(dest);
            return;
        }
        if is_i128_type(from_ty) && !is_i128_type(to_ty) {
            self.emit_load_acc_pair(src);
            if to_ty.size() < 8 {
                self.emit_cast_instrs(IrType::I64, to_ty);
            }
            self.emit_store_result(dest);
            return;
        }
        if is_i128_type(from_ty) && is_i128_type(to_ty) {
            self.emit_load_acc_pair(src);
            self.emit_store_acc_pair(dest);
            return;
        }
        self.emit_load_operand(src);
        self.emit_cast_instrs(from_ty, to_ty);
        self.emit_store_result(dest);
    }

    /// Emit a memory copy.
    fn emit_memcpy(&mut self, dest: &Value, src: &Value, size: usize) {
        if let Some(addr) = self.state_ref().resolve_slot_addr(dest.0) {
            match addr {
                SlotAddr::OverAligned(slot, id) => {
                    self.emit_alloca_aligned_addr(slot, id);
                    self.emit_memcpy_store_dest_from_acc();
                }
                SlotAddr::Direct(slot) => self.emit_memcpy_load_dest_addr(slot, true),
                SlotAddr::Indirect(slot) => self.emit_memcpy_load_dest_addr(slot, false),
            }
        }
        if let Some(addr) = self.state_ref().resolve_slot_addr(src.0) {
            match addr {
                SlotAddr::OverAligned(slot, id) => {
                    self.emit_alloca_aligned_addr(slot, id);
                    self.emit_memcpy_store_src_from_acc();
                }
                SlotAddr::Direct(slot) => self.emit_memcpy_load_src_addr(slot, true),
                SlotAddr::Indirect(slot) => self.emit_memcpy_load_src_addr(slot, false),
            }
        }
        self.emit_memcpy_impl(size);
    }

    /// Store accumulator to memcpy dest register.
    fn emit_memcpy_store_dest_from_acc(&mut self);
    /// Store accumulator to memcpy src register.
    fn emit_memcpy_store_src_from_acc(&mut self);

    /// Emit va_arg: extract next variadic argument from va_list and store to dest.
    fn emit_va_arg(&mut self, dest: &Value, va_list_ptr: &Value, result_ty: IrType);

    /// Emit va_start: initialize a va_list for variadic argument access.
    fn emit_va_start(&mut self, va_list_ptr: &Value);

    /// Emit va_end: clean up a va_list. No-op on all current targets.
    fn emit_va_end(&mut self, _va_list_ptr: &Value) {}

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
    fn emit_return(&mut self, val: Option<&Operand>, frame_size: i64) {
        if let Some(val) = val {
            let ret_ty = self.current_return_type();
            if is_i128_type(ret_ty) {
                self.emit_load_acc_pair(val);
                self.emit_return_i128_to_regs();
                self.emit_epilogue_and_ret(frame_size);
                return;
            }
            self.emit_load_operand(val);
            if ret_ty.is_long_double() {
                self.emit_return_f128_to_reg();
            } else if ret_ty == IrType::F32 {
                self.emit_return_f32_to_reg();
            } else if ret_ty.is_float() {
                self.emit_return_f64_to_reg();
            } else {
                self.emit_return_int_to_reg();
            }
        }
        self.emit_epilogue_and_ret(frame_size);
    }

    // ---- Architecture-specific instruction primitives ----

    // --- 128-bit (accumulator pair) primitives ---

    /// Sign-extend the accumulator into the high half of the accumulator pair.
    fn emit_sign_extend_acc_high(&mut self);

    /// Zero the high half of the accumulator pair.
    fn emit_zero_acc_high(&mut self);

    /// Load an operand into the accumulator pair.
    fn emit_load_acc_pair(&mut self, op: &Operand);

    /// Store the accumulator pair to a value's 16-byte stack slot.
    fn emit_store_acc_pair(&mut self, dest: &Value);

    /// Store the accumulator pair directly to a stack slot.
    fn emit_store_pair_to_slot(&mut self, slot: StackSlot);

    /// Load the accumulator pair from a stack slot.
    fn emit_load_pair_from_slot(&mut self, slot: StackSlot);

    /// Save the accumulator pair to scratch regs before loading a pointer.
    fn emit_save_acc_pair(&mut self);

    /// Store the saved accumulator pair through the pointer now in the addr reg.
    fn emit_store_pair_indirect(&mut self);

    /// Load the accumulator pair through the pointer now in the addr reg.
    fn emit_load_pair_indirect(&mut self);

    /// Emit 128-bit negate on the accumulator pair.
    fn emit_i128_neg(&mut self);

    /// Emit 128-bit bitwise NOT on the accumulator pair.
    fn emit_i128_not(&mut self);

    // --- Return primitives ---

    /// Get the current function's return type.
    fn current_return_type(&self) -> IrType;

    /// Move the i128 accumulator pair into the ABI return registers.
    fn emit_return_i128_to_regs(&mut self);

    /// Move the accumulator into the ABI f128 return register/format.
    fn emit_return_f128_to_reg(&mut self);

    /// Move the accumulator into the ABI float return register (f32).
    fn emit_return_f32_to_reg(&mut self);

    /// Move the accumulator into the ABI float return register (f64).
    fn emit_return_f64_to_reg(&mut self);

    /// Move the accumulator into the ABI integer return register.
    fn emit_return_int_to_reg(&mut self);

    /// Emit function epilogue and return instruction.
    fn emit_epilogue_and_ret(&mut self, frame_size: i64);

    // --- Typed store/load primitives ---

    /// Return the store instruction mnemonic for a type.
    fn store_instr_for_type(&self, ty: IrType) -> &'static str;

    /// Return the load instruction mnemonic for a type.
    fn load_instr_for_type(&self, ty: IrType) -> &'static str;

    /// Store the accumulator to a slot using a typed instruction.
    fn emit_typed_store_to_slot(&mut self, instr: &'static str, ty: IrType, slot: StackSlot);

    /// Load from a slot into the accumulator using a typed instruction.
    fn emit_typed_load_from_slot(&mut self, instr: &'static str, slot: StackSlot);

    /// Save the accumulator to a scratch register.
    fn emit_save_acc(&mut self);

    /// Load a pointer value from a non-alloca slot into the address register.
    fn emit_load_ptr_from_slot(&mut self, slot: StackSlot);

    /// Store the saved accumulator through the address register.
    fn emit_typed_store_indirect(&mut self, instr: &'static str, ty: IrType);

    /// Load through the address register into the accumulator.
    fn emit_typed_load_indirect(&mut self, instr: &'static str);

    // --- GEP primitives ---

    /// Load a slot's effective address into a secondary register.
    fn emit_slot_addr_to_secondary(&mut self, slot: StackSlot, is_alloca: bool);

    /// Add the secondary register to the accumulator.
    fn emit_add_secondary_to_acc(&mut self);

    // --- Dynamic alloca primitives ---

    /// Round up the accumulator to 16-byte alignment.
    fn emit_round_up_acc_to_16(&mut self);

    /// Subtract the accumulator from the stack pointer.
    fn emit_sub_sp_by_acc(&mut self);

    /// Move the stack pointer value into the accumulator.
    fn emit_mov_sp_to_acc(&mut self);

    /// Align the accumulator to the given alignment.
    fn emit_align_acc(&mut self, align: usize);

    // --- Memcpy primitives ---

    /// Load the dest address for memcpy into the arch-specific dest register.
    fn emit_memcpy_load_dest_addr(&mut self, slot: StackSlot, is_alloca: bool);

    /// Load the src address for memcpy into the arch-specific src register.
    fn emit_memcpy_load_src_addr(&mut self, slot: StackSlot, is_alloca: bool);

    /// Emit the actual copy loop/instruction for memcpy.
    fn emit_memcpy_impl(&mut self, size: usize);

    // --- Unary operation primitives ---

    fn emit_float_neg(&mut self, ty: IrType);
    fn emit_int_neg(&mut self, ty: IrType);
    fn emit_int_not(&mut self, ty: IrType);
    fn emit_int_clz(&mut self, ty: IrType);
    fn emit_int_ctz(&mut self, ty: IrType);
    fn emit_int_bswap(&mut self, ty: IrType);
    fn emit_int_popcount(&mut self, ty: IrType);

    // --- Control flow primitives ---

    /// The unconditional jump mnemonic.
    fn jump_mnemonic(&self) -> &'static str;

    /// The trap/unreachable instruction.
    fn trap_instruction(&self) -> &'static str;

    /// Emit a branch-if-nonzero instruction.
    fn emit_branch_nonzero(&mut self, label: &str);

    /// Emit an indirect jump through the accumulator register.
    fn emit_jump_indirect(&mut self);

    // --- Default control flow implementations ---

    /// Emit an unconditional branch.
    fn emit_branch(&mut self, label: &str) {
        let mnemonic = self.jump_mnemonic();
        self.state().emit(&format!("    {} {}", mnemonic, label));
    }

    /// Emit an unreachable trap instruction.
    fn emit_unreachable(&mut self) {
        let trap = self.trap_instruction();
        self.state().emit(&format!("    {}", trap));
    }

    /// Emit a conditional branch.
    fn emit_cond_branch(&mut self, cond: &Operand, true_label: &str, false_label: &str) {
        self.emit_load_operand(cond);
        self.emit_branch_nonzero(true_label);
        self.emit_branch(false_label);
    }

    /// Emit an indirect branch (computed goto).
    fn emit_indirect_branch(&mut self, target: &Operand) {
        self.emit_load_operand(target);
        self.emit_jump_indirect();
    }

    /// Emit a label address load (GCC &&label extension).
    fn emit_label_addr(&mut self, dest: &Value, label: &str) {
        self.emit_global_addr(dest, label);
    }

    /// Emit code to capture the second F64 return value after a function call.
    fn emit_get_return_f64_second(&mut self, dest: &Value);

    /// Emit code to set the second F64 return value before a return.
    fn emit_set_return_f64_second(&mut self, src: &Operand);

    /// Emit code to capture the second F32 return value.
    fn emit_get_return_f32_second(&mut self, dest: &Value);

    /// Emit code to set the second F32 return value.
    fn emit_set_return_f32_second(&mut self, src: &Operand);

    /// Emit the function directive for the function type attribute.
    fn function_type_directive(&self) -> &'static str { "@function" }

    /// Emit dynamic stack allocation.
    fn emit_dyn_alloca(&mut self, dest: &Value, size: &Operand, align: usize) {
        self.emit_load_operand(size);
        self.emit_round_up_acc_to_16();
        self.emit_sub_sp_by_acc();
        self.emit_mov_sp_to_acc();
        if align > 16 {
            self.emit_align_acc(align);
        }
        self.emit_store_result(dest);
    }

    /// Emit a 128-bit value copy.
    fn emit_copy_i128(&mut self, dest: &Value, src: &Operand) {
        self.emit_load_operand(src);
        self.emit_store_result(dest);
    }

    /// Frame size including alignment and saved registers.
    fn aligned_frame_size(&self, raw_space: i64) -> i64;

    // ---- 128-bit binary operation dispatch ----

    /// Prepare operands for a 128-bit binary operation.
    fn emit_i128_prep_binop(&mut self, lhs: &Operand, rhs: &Operand);

    fn emit_i128_add(&mut self);
    fn emit_i128_sub(&mut self);
    fn emit_i128_mul(&mut self);
    fn emit_i128_and(&mut self);
    fn emit_i128_or(&mut self);
    fn emit_i128_xor(&mut self);
    fn emit_i128_shl(&mut self);
    fn emit_i128_lshr(&mut self);
    fn emit_i128_ashr(&mut self);

    /// Emit an i128 division/remainder via compiler-rt call.
    fn emit_i128_divrem_call(&mut self, func_name: &str, lhs: &Operand, rhs: &Operand);

    /// Store the i128 result pair to a destination value.
    fn emit_i128_store_result(&mut self, dest: &Value);

    /// Emit an i128 binary operation. Default dispatches to per-op primitives.
    fn emit_i128_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand) {
        match op {
            IrBinOp::SDiv | IrBinOp::UDiv | IrBinOp::SRem | IrBinOp::URem => {
                let func_name = match op {
                    IrBinOp::SDiv => "__divti3",
                    IrBinOp::UDiv => "__udivti3",
                    IrBinOp::SRem => "__modti3",
                    IrBinOp::URem => "__umodti3",
                    _ => unreachable!(),
                };
                self.emit_i128_divrem_call(func_name, lhs, rhs);
            }
            _ => {
                self.emit_i128_prep_binop(lhs, rhs);
                match op {
                    IrBinOp::Add => self.emit_i128_add(),
                    IrBinOp::Sub => self.emit_i128_sub(),
                    IrBinOp::Mul => self.emit_i128_mul(),
                    IrBinOp::And => self.emit_i128_and(),
                    IrBinOp::Or => self.emit_i128_or(),
                    IrBinOp::Xor => self.emit_i128_xor(),
                    IrBinOp::Shl => self.emit_i128_shl(),
                    IrBinOp::LShr => self.emit_i128_lshr(),
                    IrBinOp::AShr => self.emit_i128_ashr(),
                    _ => unreachable!(),
                }
            }
        }
        self.emit_i128_store_result(dest);
    }

    // ---- 128-bit comparison dispatch ----

    /// Prepare operands for a 128-bit comparison.
    fn emit_i128_prep_cmp(&mut self, lhs: &Operand, rhs: &Operand) {
        self.emit_i128_prep_binop(lhs, rhs);
    }

    /// Emit an i128 equality comparison (Eq or Ne).
    fn emit_i128_cmp_eq(&mut self, is_ne: bool);

    /// Emit an ordered i128 comparison.
    fn emit_i128_cmp_ordered(&mut self, op: IrCmpOp);

    /// Store the i128 comparison result to dest.
    fn emit_i128_cmp_store_result(&mut self, dest: &Value);

    /// Emit an i128 comparison. Default dispatches Eq/Ne vs ordered.
    fn emit_i128_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand) {
        self.emit_i128_prep_cmp(lhs, rhs);
        match op {
            IrCmpOp::Eq => self.emit_i128_cmp_eq(false),
            IrCmpOp::Ne => self.emit_i128_cmp_eq(true),
            _ => self.emit_i128_cmp_ordered(op),
        }
        self.emit_i128_cmp_store_result(dest);
    }

    /// Emit an X86 SSE operation. Default is no-op.
    fn emit_x86_sse_op(&mut self, _dest: &Option<Value>, _op: &X86SseOpKind, _dest_ptr: &Option<Value>, _args: &[Operand]) {}
}
