//! AArch64 F128 (IEEE 754 binary128 / quad-precision) full-precision helpers.
//!
//! On AArch64, `long double` is IEEE 754 binary128 (16 bytes). Hardware has
//! no quad-precision FP ops, so all F128 arithmetic and conversion uses
//! compiler-rt / libgcc soft-float library calls.
//!
//! This file implements the `F128SoftFloat` trait for AArch64, providing the
//! arch-specific primitives (register names, instruction mnemonics, Q-register
//! representation). The shared orchestration logic lives in `backend/f128_softfloat.rs`.
//!
//! ABI: F128 values are passed/returned in Q registers (q0, q1).
//! Key design:
//! - Stack slots for F128 are 16 bytes (same as I128).
//! - f128_load_sources tracks which alloca/offset each F128 value was loaded
//!   from, enabling full-precision reloads for comparisons and casts.

use crate::ir::ir::*;
use crate::backend::state::{StackSlot, SlotAddr};
use crate::backend::traits::ArchCodegen;
use crate::backend::f128_softfloat::F128SoftFloat;
use super::codegen::ArmCodegen;

impl F128SoftFloat for ArmCodegen {
    fn state(&mut self) -> &mut crate::backend::state::CodegenState {
        &mut self.state
    }

    fn f128_get_slot(&self, val_id: u32) -> Option<StackSlot> {
        self.state.get_slot(val_id)
    }

    fn f128_get_source(&self, val_id: u32) -> Option<(u32, i64, bool)> {
        self.state.get_f128_source(val_id)
    }

    fn f128_resolve_slot_addr(&self, val_id: u32) -> Option<SlotAddr> {
        self.state.resolve_slot_addr(val_id)
    }

    fn f128_is_alloca(&self, val_id: u32) -> bool {
        self.state.is_alloca(val_id)
    }

    fn f128_load_const_to_arg1(&mut self, lo: u64, hi: u64) {
        self.emit_load_imm64("x0", lo as i64);
        self.emit_load_imm64("x1", hi as i64);
        self.state.emit("    fmov d0, x0");
        self.state.emit("    mov v0.d[1], x1");
    }

    fn f128_load_16b_from_addr_reg_to_arg1(&mut self) {
        // x17 holds the address; load 16 bytes into q0
        self.state.emit("    ldr q0, [x17]");
    }

    fn f128_load_from_frame_offset_to_arg1(&mut self, offset: i64) {
        self.emit_load_from_sp("q0", offset, "ldr");
    }

    fn f128_load_ptr_to_addr_reg(&mut self, slot: StackSlot, val_id: u32) {
        if self.state.is_alloca(val_id) {
            self.emit_add_sp_offset("x17", slot.0);
        } else {
            self.emit_load_from_sp("x17", slot.0, "ldr");
        }
    }

    fn f128_add_offset_to_addr_reg(&mut self, offset: i64) {
        if offset > 0 && offset <= 4095 {
            self.state.emit_fmt(format_args!("    add x17, x17, #{}", offset));
        } else {
            self.load_large_imm("x16", offset);
            self.state.emit("    add x17, x17, x16");
        }
    }

    fn f128_alloca_aligned_addr(&mut self, slot: StackSlot, val_id: u32) {
        self.emit_alloca_aligned_addr(slot, val_id);
    }

    fn f128_load_operand_and_extend(&mut self, op: &Operand) {
        self.operand_to_x0(op);
        self.state.emit("    fmov d0, x0");
        self.state.emit("    bl __extenddftf2");
    }

    fn f128_move_arg1_to_arg2(&mut self) {
        // Move q0 -> q1 (128-bit NEON register move)
        self.state.emit("    mov v1.16b, v0.16b");
    }

    fn f128_save_arg1_to_sp(&mut self) {
        self.state.emit("    str q0, [sp]");
    }

    fn f128_reload_arg1_from_sp(&mut self) {
        self.state.emit("    ldr q0, [sp]");
    }

    fn f128_alloc_temp_16(&mut self) {
        self.emit_sub_sp(16);
    }

    fn f128_free_temp_16(&mut self) {
        self.emit_add_sp(16);
    }

    fn f128_call(&mut self, name: &str) {
        self.state.emit_fmt(format_args!("    bl {}", name));
    }

    fn f128_truncate_result_to_acc(&mut self) {
        self.state.emit("    bl __trunctfdf2");
        self.state.emit("    fmov x0, d0");
    }

    fn f128_store_const_halves_to_slot(&mut self, lo: u64, hi: u64, slot: StackSlot) {
        self.emit_load_imm64("x0", lo as i64);
        self.emit_store_to_sp("x0", slot.0, "str");
        self.emit_load_imm64("x0", hi as i64);
        self.emit_store_to_sp("x0", slot.0 + 8, "str");
    }

    fn f128_store_arg1_to_slot(&mut self, slot: StackSlot) {
        // Store q0 (16 bytes) to slot
        self.emit_store_to_sp("q0", slot.0, "str");
    }

    fn f128_copy_slot_to_slot(&mut self, src_offset: i64, dest_slot: StackSlot) {
        // Load 16 bytes from source into q0, store to dest
        self.emit_load_from_sp("q0", src_offset, "ldr");
        self.emit_store_to_sp("q0", dest_slot.0, "str");
    }

    fn f128_copy_addr_reg_to_slot(&mut self, dest_slot: StackSlot) {
        // Load from x17 (addr reg) into q0, store to slot
        self.state.emit("    ldr q0, [x17]");
        self.emit_store_to_sp("q0", dest_slot.0, "str");
    }

    fn f128_store_const_halves_to_addr(&mut self, lo: u64, hi: u64) {
        // x17 holds dest address; use x16 as scratch
        self.state.emit("    mov x16, x17");
        self.emit_load_imm64("x0", lo as i64);
        self.state.emit("    str x0, [x16]");
        self.emit_load_imm64("x0", hi as i64);
        self.state.emit("    str x0, [x16, #8]");
    }

    fn f128_save_addr_reg(&mut self) {
        // Save x17 to x16
        self.state.emit("    mov x16, x17");
    }

    fn f128_copy_slot_to_saved_addr(&mut self, src_offset: i64) {
        // Load 16 bytes from source slot, store to saved addr (x16)
        self.emit_load_from_sp("q0", src_offset, "ldr");
        self.state.emit("    str q0, [x16]");
    }

    fn f128_copy_addr_reg_to_saved_addr(&mut self) {
        // Load 16 bytes from x17, store to x16
        self.state.emit("    ldr q0, [x17]");
        self.state.emit("    str q0, [x16]");
    }

    fn f128_store_arg1_to_saved_addr(&mut self) {
        // Store q0 (f128 in arg1) to saved addr (x16)
        self.state.emit("    str q0, [x16]");
    }

    fn f128_flip_sign_bit(&mut self) {
        // Extract high 64 bits of q0, XOR sign bit, reinsert
        self.state.emit("    mov x0, v0.d[1]");
        self.state.emit("    eor x0, x0, #0x8000000000000000");
        self.state.emit("    mov v0.d[1], x0");
    }

    fn f128_cmp_result_to_bool(&mut self, kind: crate::backend::cast::F128CmpKind) {
        use crate::backend::cast::F128CmpKind;
        self.state.emit("    cmp w0, #0");
        let cond = match kind {
            F128CmpKind::EqZero => "eq",
            F128CmpKind::NeZero => "ne",
            F128CmpKind::LtZero => "lt",
            F128CmpKind::LeZero => "le",
            F128CmpKind::GtZero => "gt",
            F128CmpKind::GeZero => "ge",
        };
        self.state.emit_fmt(format_args!("    cset x0, {}", cond));
    }

    fn f128_store_acc_to_dest(&mut self, dest: &Value) {
        self.store_x0_to(dest);
    }

    fn f128_track_self(&mut self, dest_id: u32) {
        self.state.track_f128_self(dest_id);
    }

    fn f128_set_acc_cache(&mut self, dest_id: u32) {
        self.state.reg_cache.set_acc(dest_id, false);
    }

    fn f128_set_dyn_alloca(&mut self, val: bool) -> bool {
        let saved = self.state.has_dyn_alloca;
        self.state.has_dyn_alloca = val;
        saved
    }
}

// =============================================================================
// Public helpers that delegate to shared orchestration
// =============================================================================

impl ArmCodegen {
    /// Load an F128 operand into Q0 with full precision.
    pub(super) fn emit_f128_operand_to_q0_full(&mut self, op: &Operand) {
        crate::backend::f128_softfloat::f128_operand_to_arg1(self, op);
    }

    /// Store an F128 value (16 bytes) to a direct stack slot.
    pub(super) fn emit_f128_store_to_slot(&mut self, val: &Operand, slot: StackSlot) {
        crate::backend::f128_softfloat::f128_store_to_slot(self, val, slot);
    }

    /// Store Q0 (16-byte f128) to a stack slot.
    pub(super) fn emit_f128_store_q0_to_slot(&mut self, slot: StackSlot) {
        self.emit_store_to_sp("q0", slot.0, "str");
    }

    /// Store an F128 value to an address in x17.
    pub(super) fn emit_f128_store_to_addr_in_x17(&mut self, val: &Operand) {
        crate::backend::f128_softfloat::f128_store_to_addr_reg(self, val);
    }

    /// Emit F128 binary operation via soft-float library calls with full precision.
    pub(super) fn emit_f128_binop_softfloat_full(&mut self, mnemonic: &str, dest: &Value, lhs: &Operand, rhs: &Operand) {
        let libcall = match crate::backend::cast::f128_binop_libcall(mnemonic) {
            Some(lc) => lc,
            None => {
                self.emit_f128_binop_softfloat(mnemonic);
                return;
            }
        };

        // Use dest slot as temp storage for LHS f128.
        let dest_slot = self.state.get_slot(dest.0);
        // Step 1: Load LHS f128 into Q0, save to dest slot (temp).
        self.emit_f128_operand_to_q0_full(lhs);
        if let Some(slot) = dest_slot {
            self.emit_f128_store_q0_to_slot(slot);
        }
        // Step 2: Load RHS f128 into Q0, move to Q1.
        self.emit_f128_operand_to_q0_full(rhs);
        self.state.emit("    mov v1.16b, v0.16b");
        // Step 3: Load saved LHS f128 from dest slot back to Q0.
        if let Some(slot) = dest_slot {
            self.emit_load_from_sp("q0", slot.0, "ldr");
        }
        // Step 4: Call the arithmetic libcall.
        self.state.emit_fmt(format_args!("    bl {}", libcall));
        // Step 5: Convert result to f64.
        self.state.emit("    bl __trunctfdf2");
        self.state.emit("    fmov x0, d0");
        self.state.reg_cache.invalidate_all();
        self.store_x0_to(dest);
    }

    /// Negate an F128 value with full precision.
    pub(super) fn emit_f128_neg_full(&mut self, dest: &Value, src: &Operand) {
        crate::backend::f128_softfloat::f128_neg(self, dest, src);
    }
}
