//! AArch64 F128 (IEEE 754 binary128 / quad-precision) full-precision helpers.
//!
//! On AArch64, `long double` is IEEE 754 binary128 (16 bytes). Hardware has
//! no quad-precision FP ops, so all F128 arithmetic and conversion uses
//! compiler-rt / libgcc soft-float library calls.
//!
//! ABI: F128 values are passed/returned in Q registers (q0, q1).
//! This module stores full 16-byte IEEE binary128 in stack slots, similar
//! to the RISC-V backend, avoiding the f64 precision loss that occurred
//! when F128 was approximated as f64.
//!
//! Key design:
//! - Stack slots for F128 are 16 bytes (same as I128).
//! - f128_load_sources tracks which alloca/offset each F128 value was loaded
//!   from, enabling full-precision reloads for comparisons and casts.
//! - Constants use x87-to-f128 byte conversion for full precision.
//! - Arithmetic and casts go through soft-float libcalls.
//! - The f64 approximation in x0 is kept for the register-based data flow,
//!   but the authoritative value lives in the 16-byte stack slot.

use crate::ir::ir::*;
use crate::backend::state::{StackSlot, SlotAddr};
use crate::backend::traits::ArchCodegen;
use super::codegen::ArmCodegen;

impl ArmCodegen {
    // ---- F128 soft-float helpers (full precision) ----

    /// Load an F128 operand into Q0 with full precision.
    /// For LongDouble constants, converts x87 bytes to IEEE f128 and loads directly.
    /// For Value operands with f128_load_sources tracking, loads full 16 bytes from memory.
    /// Falls back to f64->f128 conversion via __extenddftf2 for untracked values.
    pub(super) fn emit_f128_operand_to_q0_full(&mut self, op: &Operand) {
        if let Operand::Const(IrConst::LongDouble(_, x87_bytes)) = op {
            // Full-precision path: convert x87 bytes to IEEE f128 and load directly.
            let f128_bytes = crate::common::long_double::x87_bytes_to_f128_bytes(x87_bytes);
            let lo = u64::from_le_bytes(f128_bytes[0..8].try_into().unwrap());
            let hi = u64::from_le_bytes(f128_bytes[8..16].try_into().unwrap());
            self.emit_load_imm64("x0", lo as i64);
            self.emit_load_imm64("x1", hi as i64);
            // Build q0 from x0:x1
            self.state.emit("    fmov d0, x0");
            self.state.emit("    mov v0.d[1], x1");
            return;
        }
        if let Operand::Value(v) = op {
            // Try to load full 16-byte f128 from tracked source.
            if let Some(&(src_id, offset, is_indirect)) = self.f128_load_sources.get(&v.0) {
                if is_indirect {
                    // Source slot holds a pointer; dereference to get F128 data.
                    if let Some(slot) = self.state.get_slot(src_id) {
                        if self.state.is_alloca(src_id) {
                            self.emit_add_sp_offset("x17", slot.0);
                        } else {
                            self.emit_load_from_sp("x17", slot.0, "ldr");
                        }
                        if offset != 0 {
                            if offset > 0 && offset <= 4095 {
                                self.state.emit_fmt(format_args!("    add x17, x17, #{}", offset));
                            } else {
                                self.load_large_imm("x16", offset);
                                self.state.emit("    add x17, x17, x16");
                            }
                        }
                        self.state.emit("    ldr q0, [x17]");
                        return;
                    }
                } else {
                    // Source data is directly in the slot at the given offset.
                    // When is_indirect=false, the f128 bytes are stored directly
                    // in the slot regardless of whether resolve_slot_addr returns
                    // Direct or Indirect.
                    let addr = self.state.resolve_slot_addr(src_id);
                    if let Some(addr) = addr {
                        match addr {
                            SlotAddr::Direct(slot) | SlotAddr::Indirect(slot) => {
                                let effective = slot.0 + offset;
                                self.emit_load_from_sp("q0", effective, "ldr");
                            }
                            SlotAddr::OverAligned(slot, id) => {
                                self.emit_alloca_aligned_addr(slot, id);
                                if offset != 0 {
                                    self.emit_add_offset_to_addr_reg(offset);
                                }
                                self.state.emit("    ldr q0, [x17]");
                            }
                        }
                        return;
                    }
                }
            }
        }
        // Fallback: load f64 approximation and convert to f128 via __extenddftf2.
        self.operand_to_x0(op);
        self.state.emit("    fmov d0, x0");
        self.state.emit("    bl __extenddftf2");
    }

    /// Get the full IEEE f128 low/high u64 halves for an F128 constant.
    /// Returns None for non-constant operands.
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
            self.emit_load_imm64("x0", lo as i64);
            self.emit_store_to_sp("x0", slot.0, "str");
            self.emit_load_imm64("x0", hi as i64);
            self.emit_store_to_sp("x0", slot.0 + 8, "str");
        } else if let Operand::Value(v) = val {
            // Check if this value has full f128 data in a tracked source.
            if let Some(&(src_id, offset, is_indirect)) = self.f128_load_sources.get(&v.0) {
                if self.emit_f128_copy_from_source(src_id, offset, is_indirect, slot) {
                    return;
                }
            }
            // Fallback: convert f64 to f128, store 16 bytes.
            self.operand_to_x0(val);
            self.state.emit("    fmov d0, x0");
            self.state.emit("    bl __extenddftf2");
            self.emit_f128_store_q0_to_slot(slot);
            self.state.reg_cache.invalidate_all();
        } else {
            // Runtime value without tracking: convert f64 to f128.
            self.operand_to_x0(val);
            self.state.emit("    fmov d0, x0");
            self.state.emit("    bl __extenddftf2");
            self.emit_f128_store_q0_to_slot(slot);
            self.state.reg_cache.invalidate_all();
        }
    }

    /// Copy full 16-byte f128 from a tracked source to a destination slot.
    /// Returns true if the copy was done, false if the source couldn't be resolved.
    fn emit_f128_copy_from_source(&mut self, src_id: u32, offset: i64, is_indirect: bool, dest_slot: StackSlot) -> bool {
        if is_indirect {
            if let Some(src_slot) = self.state.get_slot(src_id) {
                if self.state.is_alloca(src_id) {
                    self.emit_add_sp_offset("x17", src_slot.0);
                } else {
                    self.emit_load_from_sp("x17", src_slot.0, "ldr");
                }
                if offset != 0 {
                    if offset > 0 && offset <= 4095 {
                        self.state.emit_fmt(format_args!("    add x17, x17, #{}", offset));
                    } else {
                        self.load_large_imm("x16", offset);
                        self.state.emit("    add x17, x17, x16");
                    }
                }
                self.state.emit("    ldr q0, [x17]");
                self.emit_f128_store_q0_to_slot(dest_slot);
                return true;
            }
        } else {
            // is_indirect=false: data is stored directly in the slot.
            // Both Direct and Indirect slot types are treated the same.
            let addr = self.state.resolve_slot_addr(src_id);
            if let Some(addr) = addr {
                match addr {
                    SlotAddr::Direct(slot) | SlotAddr::Indirect(slot) => {
                        let effective = slot.0 + offset;
                        self.emit_load_from_sp("q0", effective, "ldr");
                        self.emit_f128_store_q0_to_slot(dest_slot);
                    }
                    SlotAddr::OverAligned(slot, id) => {
                        self.emit_alloca_aligned_addr(slot, id);
                        if offset != 0 {
                            self.emit_add_offset_to_addr_reg(offset);
                        }
                        self.state.emit("    ldr q0, [x17]");
                        self.emit_f128_store_q0_to_slot(dest_slot);
                    }
                }
                return true;
            }
        }
        false
    }

    /// Store Q0 (16-byte f128) to a stack slot.
    pub(super) fn emit_f128_store_q0_to_slot(&mut self, slot: StackSlot) {
        self.emit_store_to_sp("q0", slot.0, "str");
    }

    /// Store an F128 value to an address in x17.
    pub(super) fn emit_f128_store_to_addr_in_x17(&mut self, val: &Operand) {
        if let Some((lo, hi)) = Self::f128_const_halves(val) {
            self.state.emit("    mov x16, x17"); // save addr
            self.emit_load_imm64("x0", lo as i64);
            self.state.emit("    str x0, [x16]");
            self.emit_load_imm64("x0", hi as i64);
            self.state.emit("    str x0, [x16, #8]");
        } else if let Operand::Value(v) = val {
            if let Some(&(src_id, offset, is_indirect)) = self.f128_load_sources.get(&v.0) {
                // Save dest addr
                self.state.emit("    mov x16, x17");
                if self.emit_f128_load_source_to_q0(src_id, offset, is_indirect) {
                    self.state.emit("    str q0, [x16]");
                    return;
                }
                // Fallback if source couldn't be loaded
                self.operand_to_x0(val);
                self.state.emit("    fmov d0, x0");
                self.state.emit("    bl __extenddftf2");
                self.state.emit("    str q0, [x16]");
                self.state.reg_cache.invalidate_all();
            } else {
                self.state.emit("    mov x16, x17"); // save addr
                self.operand_to_x0(val);
                self.state.emit("    fmov d0, x0");
                self.state.emit("    bl __extenddftf2");
                self.state.emit("    str q0, [x16]");
                self.state.reg_cache.invalidate_all();
            }
        } else {
            self.state.emit("    mov x16, x17"); // save addr
            self.operand_to_x0(val);
            self.state.emit("    fmov d0, x0");
            self.state.emit("    bl __extenddftf2");
            self.state.emit("    str q0, [x16]");
            self.state.reg_cache.invalidate_all();
        }
    }

    /// Load full f128 from tracked source into Q0.
    /// Returns true on success, false if source could not be resolved.
    fn emit_f128_load_source_to_q0(&mut self, src_id: u32, offset: i64, is_indirect: bool) -> bool {
        if is_indirect {
            if let Some(src_slot) = self.state.get_slot(src_id) {
                if self.state.is_alloca(src_id) {
                    self.emit_add_sp_offset("x17", src_slot.0);
                } else {
                    self.emit_load_from_sp("x17", src_slot.0, "ldr");
                }
                if offset != 0 {
                    if offset > 0 && offset <= 4095 {
                        self.state.emit_fmt(format_args!("    add x17, x17, #{}", offset));
                    } else {
                        self.load_large_imm("x15", offset);
                        self.state.emit("    add x17, x17, x15");
                    }
                }
                self.state.emit("    ldr q0, [x17]");
                return true;
            }
        } else {
            // is_indirect=false: data is stored directly in the slot.
            let addr = self.state.resolve_slot_addr(src_id);
            if let Some(addr) = addr {
                match addr {
                    SlotAddr::Direct(slot) | SlotAddr::Indirect(slot) => {
                        let effective = slot.0 + offset;
                        self.emit_load_from_sp("q0", effective, "ldr");
                    }
                    SlotAddr::OverAligned(slot, id) => {
                        self.emit_alloca_aligned_addr(slot, id);
                        if offset != 0 {
                            self.emit_add_offset_to_addr_reg(offset);
                        }
                        self.state.emit("    ldr q0, [x17]");
                    }
                }
                return true;
            }
        }
        false
    }

    // ---- F128 soft-float arithmetic (full precision) ----

    /// Emit F128 binary operation via soft-float library calls with full precision.
    /// Loads both operands as full-precision f128 into Q0 and Q1, calls the
    /// libcall, stores the full f128 result, and produces an f64 approximation in x0.
    pub(super) fn emit_f128_binop_softfloat_full(&mut self, mnemonic: &str, dest: &Value, lhs: &Operand, rhs: &Operand) {
        let libcall = match crate::backend::cast::f128_binop_libcall(mnemonic) {
            Some(lc) => lc,
            None => {
                // Unknown op: fall back to old f64 path
                self.emit_f128_binop_softfloat(mnemonic);
                return;
            }
        };

        // Use dest slot as temp storage for LHS f128 (avoids sub sp which
        // breaks sp-relative slot addressing).
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
        // Step 4: Call the arithmetic libcall: result f128 in Q0.
        self.state.emit_fmt(format_args!("    bl {}", libcall));
        // Step 5: Convert result to f64 and store normally.
        // Full precision is not preserved for binop results (no source alloca).
        self.state.emit("    bl __trunctfdf2");
        self.state.emit("    fmov x0, d0");
        self.state.reg_cache.invalidate_all();
        self.store_x0_to(dest);
    }

    /// Negate an F128 value with full precision by flipping the IEEE 754 sign bit.
    /// Loads the full 128-bit value into Q0, XORs bit 127 (sign bit), stores the
    /// full f128 result back to dest's slot, and produces an f64 approximation in x0.
    /// This method handles its own store_result (caller must NOT call emit_store_result).
    pub(super) fn emit_f128_neg_full(&mut self, dest: &Value, src: &Operand) {
        // Step 1: Load full-precision f128 into Q0.
        self.emit_f128_operand_to_q0_full(src);
        // Step 2: Flip the sign bit (bit 127 = MSB of high 64-bit lane).
        // Extract high 64 bits, XOR with sign bit mask, reinsert.
        self.state.emit("    mov x0, v0.d[1]");
        self.state.emit("    eor x0, x0, #0x8000000000000000");
        self.state.emit("    mov v0.d[1], x0");
        // Step 3: Store full f128 result to dest slot (if available) so that
        // subsequent reads preserve full precision.
        if let Some(dest_slot) = self.state.get_slot(dest.0) {
            self.emit_f128_store_q0_to_slot(dest_slot);
            // Track dest as having full f128 data for subsequent operations.
            self.f128_load_sources.insert(dest.0, (dest.0, 0, false));
        }
        // Step 4: Convert full f128 result to f64 approximation in x0.
        // This is needed for the register-based data flow (x0 = accumulator).
        self.state.emit("    bl __trunctfdf2");
        self.state.emit("    fmov x0, d0");
        // Invalidate all cached register state (bl clobbers caller-saved regs),
        // then mark accumulator as holding dest's f64 approximation without
        // writing back to the slot (which would overwrite the full f128).
        self.state.reg_cache.invalidate_all();
        self.state.reg_cache.set_acc(dest.0, false);
    }

}
