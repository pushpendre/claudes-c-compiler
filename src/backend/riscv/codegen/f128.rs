//! RISC-V F128 (quad-precision / long double) soft-float helpers.
//!
//! IEEE 754 binary128 operations via compiler-rt/libgcc soft-float libcalls.
//! RISC-V LP64D ABI: f128 passed in GP register pairs (a0:a1, a2:a3).
//!
//! This file implements the `F128SoftFloat` trait for RISC-V, providing the
//! arch-specific primitives (GP register pair representation, instruction
//! mnemonics, S0-relative addressing). The shared orchestration logic lives
//! in `backend/f128_softfloat.rs`.

use crate::ir::ir::*;
use crate::backend::state::{StackSlot, SlotAddr};
use crate::backend::traits::ArchCodegen;
use crate::backend::f128_softfloat::F128SoftFloat;
use super::codegen::RiscvCodegen;

impl F128SoftFloat for RiscvCodegen {
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
        self.state.emit_fmt(format_args!("    li a0, {}", lo as i64));
        self.state.emit_fmt(format_args!("    li a1, {}", hi as i64));
    }

    fn f128_load_16b_from_addr_reg_to_arg1(&mut self) {
        // t5 holds the address; load 16 bytes into a0:a1
        self.state.emit("    ld a0, 0(t5)");
        self.state.emit("    ld a1, 8(t5)");
    }

    fn f128_load_from_frame_offset_to_arg1(&mut self, offset: i64) {
        self.emit_load_from_s0("a0", offset, "ld");
        self.emit_load_from_s0("a1", offset + 8, "ld");
    }

    fn f128_load_ptr_to_addr_reg(&mut self, slot: StackSlot, val_id: u32) {
        self.emit_load_ptr_from_slot(slot, val_id);
    }

    fn f128_add_offset_to_addr_reg(&mut self, offset: i64) {
        self.emit_add_offset_to_addr_reg(offset);
    }

    fn f128_alloca_aligned_addr(&mut self, slot: StackSlot, val_id: u32) {
        self.emit_alloca_aligned_addr(slot, val_id);
    }

    fn f128_load_operand_and_extend(&mut self, op: &Operand) {
        self.operand_to_t0(op);
        self.state.emit("    fmv.d.x fa0, t0");
        self.state.emit("    call __extenddftf2");
    }

    fn f128_move_arg1_to_arg2(&mut self) {
        // Move a0:a1 -> a2:a3 (GP register pair move)
        self.state.emit("    mv a2, a0");
        self.state.emit("    mv a3, a1");
    }

    fn f128_save_arg1_to_sp(&mut self) {
        self.state.emit("    sd a0, 0(sp)");
        self.state.emit("    sd a1, 8(sp)");
    }

    fn f128_reload_arg1_from_sp(&mut self) {
        self.state.emit("    ld a0, 0(sp)");
        self.state.emit("    ld a1, 8(sp)");
    }

    fn f128_alloc_temp_16(&mut self) {
        self.emit_addi_sp(-16);
    }

    fn f128_free_temp_16(&mut self) {
        self.emit_addi_sp(16);
    }

    fn f128_call(&mut self, name: &str) {
        self.state.emit_fmt(format_args!("    call {}", name));
    }

    fn f128_truncate_result_to_acc(&mut self) {
        self.state.emit("    call __trunctfdf2");
        self.state.emit("    fmv.x.d t0, fa0");
    }

    fn f128_store_const_halves_to_slot(&mut self, lo: u64, hi: u64, slot: StackSlot) {
        self.state.emit_fmt(format_args!("    li t0, {}", lo as i64));
        self.emit_store_to_s0("t0", slot.0, "sd");
        self.state.emit_fmt(format_args!("    li t0, {}", hi as i64));
        self.emit_store_to_s0("t0", slot.0 + 8, "sd");
    }

    fn f128_store_arg1_to_slot(&mut self, slot: StackSlot) {
        // Store a0:a1 (f128 in GP pair) to slot
        self.emit_store_to_s0("a0", slot.0, "sd");
        self.emit_store_to_s0("a1", slot.0 + 8, "sd");
    }

    fn f128_copy_slot_to_slot(&mut self, src_offset: i64, dest_slot: StackSlot) {
        // Load 16 bytes from source into t0 (two loads), store to dest
        self.emit_load_from_s0("t0", src_offset, "ld");
        self.emit_store_to_s0("t0", dest_slot.0, "sd");
        self.emit_load_from_s0("t0", src_offset + 8, "ld");
        self.emit_store_to_s0("t0", dest_slot.0 + 8, "sd");
    }

    fn f128_copy_addr_reg_to_slot(&mut self, dest_slot: StackSlot) {
        // Load from t5 (addr reg), store to slot
        self.state.emit("    ld t0, 0(t5)");
        self.emit_store_to_s0("t0", dest_slot.0, "sd");
        self.state.emit("    ld t0, 8(t5)");
        self.emit_store_to_s0("t0", dest_slot.0 + 8, "sd");
    }

    fn f128_store_const_halves_to_addr(&mut self, lo: u64, hi: u64) {
        // t5 holds dest address
        self.state.emit_fmt(format_args!("    li t0, {}", lo as i64));
        self.state.emit("    sd t0, 0(t5)");
        self.state.emit_fmt(format_args!("    li t0, {}", hi as i64));
        self.state.emit("    sd t0, 8(t5)");
    }

    fn f128_save_addr_reg(&mut self) {
        // Save t5 to t3
        self.state.emit("    mv t3, t5");
    }

    fn f128_copy_slot_to_saved_addr(&mut self, src_offset: i64) {
        // Load 16 bytes from source slot, store to saved addr (t3)
        self.emit_load_from_s0("t0", src_offset, "ld");
        self.state.emit("    sd t0, 0(t3)");
        self.emit_load_from_s0("t0", src_offset + 8, "ld");
        self.state.emit("    sd t0, 8(t3)");
    }

    fn f128_copy_addr_reg_to_saved_addr(&mut self) {
        // Load 16 bytes from t5, store to t3
        self.state.emit("    ld t0, 0(t5)");
        self.state.emit("    sd t0, 0(t3)");
        self.state.emit("    ld t0, 8(t5)");
        self.state.emit("    sd t0, 8(t3)");
    }

    fn f128_store_arg1_to_saved_addr(&mut self) {
        // Store a0:a1 (f128 in arg1) to saved addr (t3)
        self.state.emit("    sd a0, 0(t3)");
        self.state.emit("    sd a1, 8(t3)");
    }

    fn f128_flip_sign_bit(&mut self) {
        // Flip bit 63 of a1 (which is bit 127 of the IEEE f128 representation)
        self.state.emit("    li t0, 1");
        self.state.emit("    slli t0, t0, 63");
        self.state.emit("    xor a1, a1, t0");
    }

    fn f128_cmp_result_to_bool(&mut self, kind: crate::backend::cast::F128CmpKind) {
        use crate::backend::cast::F128CmpKind;
        match kind {
            F128CmpKind::EqZero => self.state.emit("    seqz t0, a0"),
            F128CmpKind::NeZero => self.state.emit("    snez t0, a0"),
            F128CmpKind::LtZero => self.state.emit("    slti t0, a0, 0"),
            F128CmpKind::LeZero => {
                // t0 = (a0 <= 0) = (a0 < 1)
                self.state.emit("    slti t0, a0, 1");
            }
            F128CmpKind::GtZero => {
                // t0 = (a0 > 0): 0 < a0
                self.state.emit("    li t0, 0");
                self.state.emit("    slt t0, t0, a0");
            }
            F128CmpKind::GeZero => {
                // t0 = (a0 >= 0) = !(a0 < 0)
                self.state.emit("    slti t0, a0, 0");
                self.state.emit("    xori t0, t0, 1");
            }
        }
    }

    fn f128_store_acc_to_dest(&mut self, dest: &Value) {
        self.store_t0_to(dest);
    }

    fn f128_track_self(&mut self, dest_id: u32) {
        self.state.track_f128_self(dest_id);
    }

    fn f128_set_acc_cache(&mut self, dest_id: u32) {
        self.state.reg_cache.set_acc(dest_id, false);
    }

    fn f128_set_dyn_alloca(&mut self, _val: bool) -> bool {
        // RISC-V uses s0 (frame pointer) for all slot addressing, so the
        // dyn_alloca flag doesn't affect addressing. Return false as no-op.
        false
    }
}

// =============================================================================
// Public helpers that delegate to shared orchestration
// =============================================================================

impl RiscvCodegen {
    /// Load an F128 operand into a0:a1 with full precision.
    pub(super) fn emit_f128_operand_to_a0_a1(&mut self, op: &Operand) {
        crate::backend::f128_softfloat::f128_operand_to_arg1(self, op);
    }

    /// Store an F128 value (16 bytes) to a direct stack slot.
    pub(super) fn emit_f128_store_to_slot(&mut self, val: &Operand, slot: StackSlot) {
        crate::backend::f128_softfloat::f128_store_to_slot(self, val, slot);
    }

    /// Store an F128 value to an over-aligned alloca slot.
    pub(super) fn emit_f128_store_to_slot_aligned(&mut self, val: &Operand, slot: StackSlot, id: u32) {
        self.emit_alloca_aligned_addr(slot, id);
        // t5 now has the aligned address
        self.emit_f128_store_to_addr_in_t5(val);
    }

    /// Store an F128 value to the address in t5.
    pub(super) fn emit_f128_store_to_addr_in_t5(&mut self, val: &Operand) {
        crate::backend::f128_softfloat::f128_store_to_addr_reg(self, val);
    }

    /// Store an F128 value via an indirect pointer (ptr in a slot).
    pub(super) fn emit_f128_store_indirect(&mut self, val: &Operand, slot: StackSlot, ptr_id: u32) {
        self.emit_load_ptr_from_slot(slot, ptr_id);
        // t5 now has the pointer
        self.emit_f128_store_to_addr_in_t5(val);
    }

    /// Load an F128 value (16 bytes) from a direct stack slot.
    /// Loads the 16-byte f128 into a0:a1, calls __trunctfdf2 to get f64 in t0.
    pub(super) fn emit_f128_load_from_slot(&mut self, slot: StackSlot) {
        self.emit_load_from_s0("a0", slot.0, "ld");
        self.emit_load_from_s0("a1", slot.0 + 8, "ld");
        self.state.emit("    call __trunctfdf2");
        self.state.emit("    fmv.x.d t0, fa0");
        self.state.reg_cache.invalidate_all();
    }

    /// Load an F128 value from the address in t5. Converts f128 to f64 via __trunctfdf2.
    pub(super) fn emit_f128_load_from_addr_in_t5(&mut self) {
        self.state.emit("    ld a0, 0(t5)");
        self.state.emit("    ld a1, 8(t5)");
        self.state.emit("    call __trunctfdf2");
        self.state.emit("    fmv.x.d t0, fa0");
        self.state.reg_cache.invalidate_all();
    }

    /// Negate an F128 value with full precision.
    pub(super) fn emit_f128_neg_full(&mut self, dest: &Value, src: &Operand) {
        crate::backend::f128_softfloat::f128_neg(self, dest, src);
    }

    /// Legacy F128 binop via soft-float, operating on f64 approximations.
    /// Called when t1 = lhs (f64 bits), t0 = rhs (f64 bits).
    /// Converts both from f64 to f128, calls libcall, converts result back to f64.
    pub(super) fn emit_f128_binop_softfloat(&mut self, mnemonic: &str) {
        let libcall = match crate::backend::cast::f128_binop_libcall(mnemonic) {
            Some(lc) => lc,
            None => {
                // Unknown op: fall back to f64 hardware path
                self.state.emit("    mv t2, t0");
                self.state.emit("    fmv.d.x ft0, t1");
                self.state.emit("    fmv.d.x ft1, t2");
                self.state.emit_fmt(format_args!("    {}.d ft0, ft0, ft1", mnemonic));
                self.state.emit("    fmv.x.d t0, ft0");
                return;
            }
        };

        self.emit_addi_sp(-24);
        self.state.emit("    sd t0, 16(sp)");
        self.state.emit("    fmv.d.x fa0, t1");
        self.state.emit("    call __extenddftf2");
        self.state.emit("    sd a0, 0(sp)");
        self.state.emit("    sd a1, 8(sp)");
        self.state.emit("    ld t0, 16(sp)");
        self.state.emit("    fmv.d.x fa0, t0");
        self.state.emit("    call __extenddftf2");
        self.state.emit("    mv a2, a0");
        self.state.emit("    mv a3, a1");
        self.state.emit("    ld a0, 0(sp)");
        self.state.emit("    ld a1, 8(sp)");
        self.state.emit_fmt(format_args!("    call {}", libcall));
        self.state.emit("    call __trunctfdf2");
        self.state.emit("    fmv.x.d t0, fa0");
        self.emit_addi_sp(24);
        self.state.reg_cache.invalidate_all();
    }
}
