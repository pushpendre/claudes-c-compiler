//! RISC-V F128 (quad-precision / long double) soft-float helpers.
//!
//! IEEE 754 binary128 operations via compiler-rt/libgcc soft-float libcalls.
//! RISC-V LP64D ABI: f128 passed in GP register pairs (a0:a1, a2:a3).

use crate::ir::ir::*;
use crate::backend::state::{StackSlot, SlotAddr};
use crate::backend::traits::ArchCodegen;
use super::codegen::RiscvCodegen;

impl RiscvCodegen {
    // ---- F128 soft-float helpers ----

    /// Load an F128 operand into a0:a1 (RISC-V GP register pair) with full precision.
    /// For LongDouble constants, uses the x87 bytes converted to f128 directly (no f64 roundtrip).
    /// For Value operands in 16-byte stack slots, loads the full f128 directly from the slot.
    /// For other runtime values, converts the f64 approximation via __extenddftf2.
    pub(super) fn emit_f128_operand_to_a0_a1(&mut self, op: &Operand) {
        if let Operand::Const(IrConst::LongDouble(_, x87_bytes)) = op {
            // Full-precision path: convert x87 bytes to IEEE f128 bytes and load directly.
            let f128_bytes = crate::common::long_double::x87_bytes_to_f128_bytes(x87_bytes);
            let lo = u64::from_le_bytes(f128_bytes[0..8].try_into().unwrap());
            let hi = u64::from_le_bytes(f128_bytes[8..16].try_into().unwrap());
            self.state.emit_fmt(format_args!("    li a0, {}", lo as i64));
            self.state.emit_fmt(format_args!("    li a1, {}", hi as i64));
        } else if let Operand::Value(v) = op {
            // Try to load the full 16-byte f128 from the original memory location
            // that this value was loaded from (tracked by f128_load_sources with offset).
            // The alloca stores the full IEEE f128 (16 bytes), preserving quad precision.
            if let Some(&(src_id, offset, is_indirect)) = self.f128_load_sources.get(&v.0) {
                if is_indirect {
                    // Source is a pointer (e.g., GEP result): slot holds a pointer
                    // that must be dereferenced to access the F128 data.
                    if let Some(src_slot) = self.state.get_slot(src_id) {
                        self.emit_load_ptr_from_slot(src_slot, src_id);
                        if offset != 0 {
                            self.emit_add_offset_to_addr_reg(offset);
                        }
                        self.state.emit("    ld a0, 0(t5)");
                        self.state.emit("    ld a1, 8(t5)");
                        return;
                    }
                } else {
                    // Source is direct: data is in the slot at the given offset.
                    // This covers allocas (direct or over-aligned) and self-referential
                    // cast results.
                    let addr = self.state.resolve_slot_addr(src_id);
                    if let Some(addr) = addr {
                        match addr {
                            SlotAddr::Direct(slot) | SlotAddr::Indirect(slot) => {
                                // For Direct allocas: data is at slot + offset.
                                // For Indirect with is_indirect=false: data was stored
                                // directly in the slot (e.g., cast result).
                                let effective = slot.0 + offset;
                                self.emit_load_from_s0("a0", effective, "ld");
                                self.emit_load_from_s0("a1", effective + 8, "ld");
                            }
                            SlotAddr::OverAligned(slot, id) => {
                                self.emit_alloca_aligned_addr(slot, id);
                                if offset != 0 {
                                    self.emit_add_offset_to_addr_reg(offset);
                                }
                                self.state.emit("    ld a0, 0(t5)");
                                self.state.emit("    ld a1, 8(t5)");
                            }
                        }
                        return;
                    }
                }
            }
            // Fallback: load f64 approximation and convert to f128 via libcall.
            self.operand_to_t0(op);
            self.state.emit("    fmv.d.x fa0, t0");
            self.state.emit("    call __extenddftf2");
        } else {
            // Other operands: load f64 approximation and convert to f128 via libcall.
            self.operand_to_t0(op);
            self.state.emit("    fmv.d.x fa0, t0");
            self.state.emit("    call __extenddftf2");
        }
    }

    // ---- F128 soft-float arithmetic ----

    /// Emit F128 (long double) binary operation via soft-float library calls.
    /// Called when t1 = lhs (f64 bits), t0 = rhs (f64 bits).
    /// Converts both operands from f64 to f128, calls the libcall, converts result back to f64.
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

        // Allocate 24 bytes: 8 for rhs f64 save, 16 for lhs f128 save
        self.emit_addi_sp(-24);
        // Save rhs f64 bit pattern
        self.state.emit("    sd t0, 16(sp)");
        // Convert lhs (t1) from f64 to f128
        self.state.emit("    fmv.d.x fa0, t1");
        self.state.emit("    call __extenddftf2");
        // Save lhs f128 (a0:a1)
        self.state.emit("    sd a0, 0(sp)");
        self.state.emit("    sd a1, 8(sp)");
        // Load rhs f64 and convert to f128
        self.state.emit("    ld t0, 16(sp)");
        self.state.emit("    fmv.d.x fa0, t0");
        self.state.emit("    call __extenddftf2");
        // RHS f128 now in a0:a1, move to a2:a3 (second arg)
        self.state.emit("    mv a2, a0");
        self.state.emit("    mv a3, a1");
        // Load LHS f128 back to a0:a1 (first arg)
        self.state.emit("    ld a0, 0(sp)");
        self.state.emit("    ld a1, 8(sp)");
        // Call the arithmetic libcall: result f128 in a0:a1
        self.state.emit_fmt(format_args!("    call {}", libcall));
        // Convert result f128 back to f64 via __trunctfdf2
        self.state.emit("    call __trunctfdf2");
        // f64 result in fa0, move to t0
        self.state.emit("    fmv.x.d t0, fa0");
        // Free temp space
        self.emit_addi_sp(24);
        self.state.reg_cache.invalidate_all();
    }

    // ---- F128 store/load helpers ----

    /// Get the full IEEE f128 low/high u64 halves for an F128 operand.
    /// For LongDouble constants, converts x87 bytes to f128 directly (full precision).
    /// For runtime values, returns None (caller must use __extenddftf2).
    pub(super) fn f128_const_halves(op: &Operand) -> Option<(u64, u64)> {
        if let Operand::Const(IrConst::LongDouble(_, x87_bytes)) = op {
            let f128_bytes = crate::common::long_double::x87_bytes_to_f128_bytes(x87_bytes);
            let lo = u64::from_le_bytes(f128_bytes[0..8].try_into().unwrap());
            let hi = u64::from_le_bytes(f128_bytes[8..16].try_into().unwrap());
            Some((lo, hi))
        } else {
            None
        }
    }

    /// Store an F128 value (16 bytes) to a direct stack slot.
    pub(super) fn emit_f128_store_to_slot(&mut self, val: &Operand, slot: StackSlot) {
        if let Some((lo, hi)) = Self::f128_const_halves(val) {
            // Full-precision constant: store both halves directly.
            self.state.emit_fmt(format_args!("    li t0, {}", lo as i64));
            self.emit_store_to_s0("t0", slot.0, "sd");
            self.state.emit_fmt(format_args!("    li t0, {}", hi as i64));
            self.emit_store_to_s0("t0", slot.0 + 8, "sd");
        } else if let Operand::Value(v) = val {
            // Check if this value has full f128 data in a tracked source
            // (e.g., from an alloca load or int->F128 cast).
            // Uses is_indirect flag to distinguish between:
            // - Direct sources (allocas, cast results): data is in the slot itself
            // - Indirect sources (GEP results): slot holds a pointer to the data
            if let Some(&(src_id, offset, is_indirect)) = self.f128_load_sources.get(&v.0) {
                if is_indirect {
                    // Source slot holds a pointer; dereference to get F128 data.
                    if let Some(src_slot) = self.state.get_slot(src_id) {
                        self.emit_load_ptr_from_slot(src_slot, src_id);
                        if offset != 0 {
                            self.emit_add_offset_to_addr_reg(offset);
                        }
                        self.state.emit("    ld t0, 0(t5)");
                        self.emit_store_to_s0("t0", slot.0, "sd");
                        self.state.emit("    ld t0, 8(t5)");
                        self.emit_store_to_s0("t0", slot.0 + 8, "sd");
                        return;
                    }
                } else {
                    // Source data is directly in the slot (alloca or cast result).
                    if let Some(src_slot) = self.state.get_slot(src_id) {
                        let src_off = src_slot.0 + offset;
                        self.emit_load_from_s0("t0", src_off, "ld");
                        self.emit_store_to_s0("t0", slot.0, "sd");
                        self.emit_load_from_s0("t0", src_off + 8, "ld");
                        self.emit_store_to_s0("t0", slot.0 + 8, "sd");
                        return;
                    }
                }
            }
            // Fallback: convert f64 in t0 to f128 via __extenddftf2, store 16 bytes.
            self.operand_to_t0(val);
            self.state.emit("    fmv.d.x fa0, t0");
            self.state.emit("    call __extenddftf2");
            // Result f128 in a0:a1
            self.emit_store_to_s0("a0", slot.0, "sd");
            self.emit_store_to_s0("a1", slot.0 + 8, "sd");
            self.state.reg_cache.invalidate_all();
        } else {
            // Runtime value without tracking: convert f64 to f128 via __extenddftf2.
            self.operand_to_t0(val);
            self.state.emit("    fmv.d.x fa0, t0");
            self.state.emit("    call __extenddftf2");
            // Result f128 in a0:a1
            self.emit_store_to_s0("a0", slot.0, "sd");
            self.emit_store_to_s0("a1", slot.0 + 8, "sd");
            self.state.reg_cache.invalidate_all();
        }
    }

    /// Store an F128 value to an over-aligned alloca slot.
    pub(super) fn emit_f128_store_to_slot_aligned(&mut self, val: &Operand, slot: StackSlot, id: u32) {
        self.emit_alloca_aligned_addr(slot, id);
        // t5 now has the aligned address
        self.emit_f128_store_to_addr_in_t5(val);
    }

    /// Store an F128 value to the address in t5.
    pub(super) fn emit_f128_store_to_addr_in_t5(&mut self, val: &Operand) {
        if let Some((lo, hi)) = Self::f128_const_halves(val) {
            self.state.emit_fmt(format_args!("    li t0, {}", lo as i64));
            self.state.emit("    sd t0, 0(t5)");
            self.state.emit_fmt(format_args!("    li t0, {}", hi as i64));
            self.state.emit("    sd t0, 8(t5)");
        } else if let Operand::Value(v) = val {
            // Check if value has full f128 data in a tracked source.
            // Uses is_indirect flag to distinguish pointer vs data sources.
            if let Some(&(src_id, offset, is_indirect)) = self.f128_load_sources.get(&v.0) {
                if is_indirect {
                    // Source slot holds a pointer; save dest addr, dereference src pointer.
                    if let Some(src_slot) = self.state.get_slot(src_id) {
                        self.state.emit("    mv t3, t5");
                        self.emit_load_ptr_from_slot(src_slot, src_id);
                        if offset != 0 {
                            self.emit_add_offset_to_addr_reg(offset);
                        }
                        self.state.emit("    ld t0, 0(t5)");
                        self.state.emit("    sd t0, 0(t3)");
                        self.state.emit("    ld t0, 8(t5)");
                        self.state.emit("    sd t0, 8(t3)");
                        return;
                    }
                } else {
                    // Source data is directly in the slot.
                    if let Some(src_slot) = self.state.get_slot(src_id) {
                        let src_off = src_slot.0 + offset;
                        self.state.emit("    mv t3, t5");
                        self.emit_load_from_s0("t0", src_off, "ld");
                        self.state.emit("    sd t0, 0(t3)");
                        self.emit_load_from_s0("t0", src_off + 8, "ld");
                        self.state.emit("    sd t0, 8(t3)");
                        return;
                    }
                }
            }
            // Fallback: use f64 approximation
            self.state.emit("    mv t3, t5");
            self.operand_to_t0(val);
            self.state.emit("    fmv.d.x fa0, t0");
            self.state.emit("    call __extenddftf2");
            self.state.emit("    sd a0, 0(t3)");
            self.state.emit("    sd a1, 8(t3)");
            self.state.reg_cache.invalidate_all();
        } else {
            // Save t5 (ptr) before operand_to_t0, which may clobber registers.
            self.state.emit("    mv t3, t5");
            self.operand_to_t0(val);
            self.state.emit("    fmv.d.x fa0, t0");
            self.state.emit("    call __extenddftf2");
            self.state.emit("    sd a0, 0(t3)");
            self.state.emit("    sd a1, 8(t3)");
            self.state.reg_cache.invalidate_all();
        }
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
        // f64 result in fa0
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

    /// Negate an F128 value with full precision by flipping the IEEE 754 sign bit.
    /// Loads the full 128-bit value into a0:a1, XORs bit 63 of a1 (which is
    /// bit 127 of the IEEE 754 binary128 representation), stores the full f128
    /// result back to dest's slot, then converts to an f64 approximation in t0.
    pub(super) fn emit_f128_neg_full(&mut self, dest: &Value, src: &Operand) {
        // Step 1: Load full-precision f128 into a0:a1.
        self.emit_f128_operand_to_a0_a1(src);
        // Step 2: Flip the sign bit (bit 127 = bit 63 of a1, the high word).
        // Use li + xor since RISC-V andi/xori only supports 12-bit immediates.
        self.state.emit("    li t0, 1");
        self.state.emit("    slli t0, t0, 63");
        self.state.emit("    xor a1, a1, t0");
        // Step 3: Store full f128 result to dest slot (if available) so that
        // subsequent reads preserve full precision.
        if let Some(dest_slot) = self.state.get_slot(dest.0) {
            self.emit_store_to_s0("a0", dest_slot.0, "sd");
            self.emit_store_to_s0("a1", dest_slot.0 + 8, "sd");
            // Track dest as having full f128 data.
            self.f128_load_sources.insert(dest.0, (dest.0, 0, false));
        }
        // Step 4: Convert full f128 result (a0:a1) to f64 approximation in t0.
        self.state.emit("    call __trunctfdf2");
        self.state.emit("    fmv.x.d t0, fa0");
        // Invalidate all cached register state (call clobbers caller-saved regs),
        // then mark accumulator as holding dest's f64 approximation without
        // writing back to the slot (which would overwrite the full f128).
        self.state.reg_cache.invalidate_all();
        self.state.reg_cache.set_acc(dest.0, false);
    }
}
