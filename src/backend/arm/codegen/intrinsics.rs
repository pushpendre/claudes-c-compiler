//! AArch64 NEON/SIMD intrinsic emission and F128 (quad-precision) soft-float helpers.
//!
//! NEON intrinsics: SSE-equivalent operations via 128-bit NEON instructions.
//! F128: IEEE 754 binary128 via compiler-rt/libgcc soft-float libcalls.

use crate::ir::ir::*;
use super::codegen::ArmCodegen;

impl ArmCodegen {
    pub(super) fn emit_neon_binary_128(&mut self, dest_ptr: &Value, args: &[Operand], neon_inst: &str) {
        // Load first 128-bit operand pointer into x0, then load q0
        self.operand_to_x0(&args[0]);
        self.state.emit("    ldr q0, [x0]");
        // Load second 128-bit operand pointer into x1, then load q1
        match &args[1] {
            Operand::Value(v) => {
                if let Some(slot) = self.state.get_slot(v.0) {
                    if self.state.is_alloca(v.0) {
                        self.emit_add_sp_offset("x1", slot.0);
                    } else {
                        self.emit_load_from_sp("x1", slot.0, "ldr");
                    }
                }
            }
            Operand::Const(_) => {
                self.operand_to_x0(&args[1]);
                self.state.emit("    mov x1, x0");
            }
        }
        self.state.emit("    ldr q1, [x1]");
        // Apply the binary NEON operation
        self.state.emit_fmt(format_args!("    {} v0.16b, v0.16b, v1.16b", neon_inst));
        // Store result to dest_ptr
        self.load_ptr_to_reg(dest_ptr, "x0");
        self.state.emit("    str q0, [x0]");
    }

    pub(super) fn emit_intrinsic_arm(&mut self, dest: &Option<Value>, op: &IntrinsicOp, dest_ptr: &Option<Value>, args: &[Operand]) {
        match op {
            IntrinsicOp::Lfence | IntrinsicOp::Mfence => {
                self.state.emit("    dmb ish");
            }
            IntrinsicOp::Sfence => {
                self.state.emit("    dmb ishst");
            }
            IntrinsicOp::Pause => {
                self.state.emit("    yield");
            }
            IntrinsicOp::Clflush => {
                // ARM has no direct clflush; use dc civac (clean+invalidate to PoC)
                self.operand_to_x0(&args[0]);
                self.state.emit("    dc civac, x0");
            }
            IntrinsicOp::Movnti => {
                // Non-temporal 32-bit store: dest_ptr = target address, args[0] = value
                if let Some(ptr) = dest_ptr {
                    self.operand_to_x0(&args[0]);
                    self.state.emit("    mov w9, w0");
                    self.load_ptr_to_reg(ptr, "x0");
                    self.state.emit("    str w9, [x0]");
                }
            }
            IntrinsicOp::Movnti64 => {
                // Non-temporal 64-bit store
                if let Some(ptr) = dest_ptr {
                    self.operand_to_x0(&args[0]);
                    self.state.emit("    mov x9, x0");
                    self.load_ptr_to_reg(ptr, "x0");
                    self.state.emit("    str x9, [x0]");
                }
            }
            IntrinsicOp::Movntdq | IntrinsicOp::Movntpd => {
                // Non-temporal 128-bit store: dest_ptr = target, args[0] = source ptr
                if let Some(ptr) = dest_ptr {
                    self.operand_to_x0(&args[0]);
                    self.state.emit("    ldr q0, [x0]");
                    self.load_ptr_to_reg(ptr, "x0");
                    self.state.emit("    str q0, [x0]");
                }
            }
            IntrinsicOp::Loaddqu => {
                // Load 128-bit unaligned: args[0] = source ptr, dest_ptr = result storage
                if let Some(dptr) = dest_ptr {
                    self.operand_to_x0(&args[0]);
                    self.state.emit("    ldr q0, [x0]");
                    self.load_ptr_to_reg(dptr, "x0");
                    self.state.emit("    str q0, [x0]");
                }
            }
            IntrinsicOp::Storedqu => {
                // Store 128-bit unaligned: dest_ptr = target ptr, args[0] = source data ptr
                if let Some(ptr) = dest_ptr {
                    self.operand_to_x0(&args[0]);
                    self.state.emit("    ldr q0, [x0]");
                    self.load_ptr_to_reg(ptr, "x0");
                    self.state.emit("    str q0, [x0]");
                }
            }
            IntrinsicOp::Pcmpeqb128 => {
                if let Some(dptr) = dest_ptr {
                    // cmeq compares and sets all bits in each lane on equality
                    self.emit_neon_binary_128(dptr, args, "cmeq");
                }
            }
            IntrinsicOp::Pcmpeqd128 => {
                if let Some(dptr) = dest_ptr {
                    // For 32-bit lane equality, load q regs, use cmeq with .4s arrangement
                    self.operand_to_x0(&args[0]);
                    self.state.emit("    ldr q0, [x0]");
                    if let Operand::Value(v) = &args[1] {
                        self.load_ptr_to_reg(v, "x1");
                    } else {
                        self.operand_to_x0(&args[1]);
                        self.state.emit("    mov x1, x0");
                    }
                    self.state.emit("    ldr q1, [x1]");
                    self.state.emit("    cmeq v0.4s, v0.4s, v1.4s");
                    self.load_ptr_to_reg(dptr, "x0");
                    self.state.emit("    str q0, [x0]");
                }
            }
            IntrinsicOp::Psubusb128 => {
                if let Some(dptr) = dest_ptr {
                    self.emit_neon_binary_128(dptr, args, "uqsub");
                }
            }
            IntrinsicOp::Por128 => {
                if let Some(dptr) = dest_ptr {
                    self.emit_neon_binary_128(dptr, args, "orr");
                }
            }
            IntrinsicOp::Pand128 => {
                if let Some(dptr) = dest_ptr {
                    self.emit_neon_binary_128(dptr, args, "and");
                }
            }
            IntrinsicOp::Pxor128 => {
                if let Some(dptr) = dest_ptr {
                    self.emit_neon_binary_128(dptr, args, "eor");
                }
            }
            IntrinsicOp::Pmovmskb128 => {
                // Extract the high bit of each byte in a 128-bit vector into a 16-bit mask.
                // NEON has no pmovmskb equivalent, so we use a multi-step sequence:
                //   1. Load 128-bit data into v0
                //   2. Shift right each byte by 7 to isolate the sign bit (ushr v0.16b, v0.16b, #7)
                //   3. Collect bits using successive narrowing and shifts
                // Efficient approach: multiply by power-of-2 bit positions, then add across lanes.
                self.operand_to_x0(&args[0]);
                self.state.emit("    ldr q0, [x0]");
                // Shift right by 7 to get 0 or 1 in each byte lane
                self.state.emit("    ushr v0.16b, v0.16b, #7");
                // Load the bit position constants: [1,2,4,8,16,32,64,128, 1,2,4,8,16,32,64,128]
                // 0x8040201008040201 loaded via movz/movk sequence
                self.state.emit("    movz x0, #0x0201");
                self.state.emit("    movk x0, #0x0804, lsl #16");
                self.state.emit("    movk x0, #0x2010, lsl #32");
                self.state.emit("    movk x0, #0x8040, lsl #48");
                self.state.emit("    fmov d1, x0");
                self.state.emit("    mov v1.d[1], x0");
                // Multiply each byte: v0[i] * v1[i] gives the bit contribution
                self.state.emit("    mul v0.16b, v0.16b, v1.16b");
                // Now sum bytes 0-7 into low byte, and bytes 8-15 into high byte
                // addv sums all lanes into a scalar - but we need two separate sums
                // Use ext to split, then addv each half
                self.state.emit("    ext v1.16b, v0.16b, v0.16b, #8");
                // v0 has low 8 bytes, v1 has high 8 bytes (shifted)
                // Sum low 8 bytes
                self.state.emit("    addv b0, v0.8b");
                self.state.emit("    umov w0, v0.b[0]");
                // Sum high 8 bytes
                self.state.emit("    addv b1, v1.8b");
                self.state.emit("    umov w1, v1.b[0]");
                // Combine: result = low_sum | (high_sum << 8)
                self.state.emit("    orr w0, w0, w1, lsl #8");
                // Store scalar result
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        self.emit_store_to_sp("x0", slot.0, "str");
                    }
                }
            }
            IntrinsicOp::SetEpi8 => {
                // Splat a byte value to all 16 bytes: args[0] = byte value
                if let Some(dptr) = dest_ptr {
                    self.operand_to_x0(&args[0]);
                    self.state.emit("    dup v0.16b, w0");
                    self.load_ptr_to_reg(dptr, "x0");
                    self.state.emit("    str q0, [x0]");
                }
            }
            IntrinsicOp::SetEpi32 => {
                // Splat a 32-bit value to all 4 lanes: args[0] = 32-bit value
                if let Some(dptr) = dest_ptr {
                    self.operand_to_x0(&args[0]);
                    self.state.emit("    dup v0.4s, w0");
                    self.load_ptr_to_reg(dptr, "x0");
                    self.state.emit("    str q0, [x0]");
                }
            }
            IntrinsicOp::Crc32_8 | IntrinsicOp::Crc32_16
            | IntrinsicOp::Crc32_32 | IntrinsicOp::Crc32_64 => {
                let is_64 = matches!(op, IntrinsicOp::Crc32_64);
                let (save_reg, crc_inst) = match op {
                    IntrinsicOp::Crc32_8  => ("w9", "crc32cb w9, w9, w0"),
                    IntrinsicOp::Crc32_16 => ("w9", "crc32ch w9, w9, w0"),
                    IntrinsicOp::Crc32_32 => ("w9", "crc32cw w9, w9, w0"),
                    IntrinsicOp::Crc32_64 => ("x9", "crc32cx w9, w9, x0"),
                    _ => unreachable!("CRC32 dispatch matched non-CRC32 op: {:?}", op),
                };
                self.operand_to_x0(&args[0]);
                self.state.emit_fmt(format_args!("    mov {}, {}", save_reg, if is_64 { "x0" } else { "w0" }));
                self.operand_to_x0(&args[1]);
                self.state.emit_fmt(format_args!("    {}", crc_inst));
                self.state.emit("    mov x0, x9");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        self.emit_store_to_sp("x0", slot.0, "str");
                    }
                }
            }
            IntrinsicOp::FrameAddress => {
                // __builtin_frame_address(0): return current frame pointer (x29)
                self.state.emit("    mov x0, x29");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        self.emit_store_to_sp("x0", slot.0, "str");
                    }
                }
            }
            IntrinsicOp::ReturnAddress => {
                // __builtin_return_address(0): return address saved at [x29, #8]
                // x30 (lr) is clobbered by bl instructions, so read from stack
                self.state.emit("    ldr x0, [x29, #8]");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        self.emit_store_to_sp("x0", slot.0, "str");
                    }
                }
            }
            IntrinsicOp::SqrtF64 => {
                self.operand_to_x0(&args[0]);
                self.state.emit("    fmov d0, x0");
                self.state.emit("    fsqrt d0, d0");
                self.state.emit("    fmov x0, d0");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        self.emit_store_to_sp("x0", slot.0, "str");
                    }
                }
            }
            IntrinsicOp::SqrtF32 => {
                self.operand_to_x0(&args[0]);
                self.state.emit("    fmov s0, w0");
                self.state.emit("    fsqrt s0, s0");
                self.state.emit("    fmov w0, s0");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        self.emit_store_to_sp("w0", slot.0, "str");
                    }
                }
            }
            IntrinsicOp::FabsF64 => {
                self.operand_to_x0(&args[0]);
                self.state.emit("    fmov d0, x0");
                self.state.emit("    fabs d0, d0");
                self.state.emit("    fmov x0, d0");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        self.emit_store_to_sp("x0", slot.0, "str");
                    }
                }
            }
            IntrinsicOp::FabsF32 => {
                self.operand_to_x0(&args[0]);
                self.state.emit("    fmov s0, w0");
                self.state.emit("    fabs s0, s0");
                self.state.emit("    fmov w0, s0");
                if let Some(d) = dest {
                    if let Some(slot) = self.state.get_slot(d.0) {
                        self.emit_store_to_sp("w0", slot.0, "str");
                    }
                }
            }
            // x86-specific SSE/AES-NI/CLMUL intrinsics - these are x86-only and should
            // not appear in ARM codegen in practice. Cross-compiled code that conditionally
            // uses these behind #ifdef __x86_64__ will have the calls dead-code eliminated.
            // TODO: consider emitting a runtime trap instead of silent zeros
            IntrinsicOp::Aesenc128 | IntrinsicOp::Aesenclast128
            | IntrinsicOp::Aesdec128 | IntrinsicOp::Aesdeclast128
            | IntrinsicOp::Aesimc128 | IntrinsicOp::Aeskeygenassist128
            | IntrinsicOp::Pclmulqdq128
            | IntrinsicOp::Pslldqi128 | IntrinsicOp::Psrldqi128
            | IntrinsicOp::Psllqi128 | IntrinsicOp::Psrlqi128
            | IntrinsicOp::Pshufd128 | IntrinsicOp::Loadldi128
            | IntrinsicOp::Paddw128 | IntrinsicOp::Psubw128
            | IntrinsicOp::Pmulhw128 | IntrinsicOp::Pmaddwd128
            | IntrinsicOp::Pcmpgtw128 | IntrinsicOp::Pcmpgtb128
            | IntrinsicOp::Psllwi128 | IntrinsicOp::Psrlwi128
            | IntrinsicOp::Psrawi128 | IntrinsicOp::Psradi128
            | IntrinsicOp::Pslldi128 | IntrinsicOp::Psrldi128
            | IntrinsicOp::Paddd128 | IntrinsicOp::Psubd128
            | IntrinsicOp::Packssdw128 | IntrinsicOp::Packuswb128
            | IntrinsicOp::Punpcklbw128 | IntrinsicOp::Punpckhbw128
            | IntrinsicOp::Punpcklwd128 | IntrinsicOp::Punpckhwd128
            | IntrinsicOp::SetEpi16 | IntrinsicOp::Pinsrw128
            | IntrinsicOp::Pextrw128 | IntrinsicOp::Storeldi128
            | IntrinsicOp::Cvtsi128Si32 | IntrinsicOp::Cvtsi32Si128
            | IntrinsicOp::Cvtsi128Si64
            | IntrinsicOp::Pshuflw128 | IntrinsicOp::Pshufhw128 => {
                // x86-only: zero dest if present
                if let Some(dptr) = dest_ptr {
                    if let Some(slot) = self.state.get_slot(dptr.0) {
                        self.state.emit_fmt(format_args!("    add x9, sp, #{}", slot.0));
                        self.state.emit("    stp xzr, xzr, [x9]");
                    }
                }
            }
        }
    }

    // ---- F128 (long double / IEEE quad precision) soft-float helpers ----
    //
    // On AArch64, long double is IEEE 754 binary128 (16 bytes).
    // Hardware has no quad-precision FP ops, so we use compiler-rt/libgcc soft-float:
    //   Comparison: __eqtf2, __lttf2, __letf2, __gttf2, __getf2
    //   Arithmetic: __addtf3, __subtf3, __multf3, __divtf3
    //   Conversion: __extenddftf2 (f64->f128), __trunctfdf2 (f128->f64)
    // ABI: f128 passed/returned in Q registers (q0, q1). Int result in w0/x0.

    /// Emit F128 arithmetic via soft-float libcalls.
    /// Called from emit_float_binop_impl when ty == F128.
    /// At entry: x1 = lhs f64 bits, x0 = rhs f64 bits (from shared float binop dispatch).
    pub(super) fn emit_f128_binop_softfloat(&mut self, mnemonic: &str) {
        let libcall = match crate::backend::cast::f128_binop_libcall(mnemonic) {
            Some(lc) => lc,
            None => {
                // Unknown op: fall back to f64 hardware path
                self.state.emit("    mov x2, x0");
                self.state.emit("    fmov d0, x1");
                self.state.emit("    fmov d1, x2");
                self.state.emit_fmt(format_args!("    {} d0, d0, d1", mnemonic));
                self.state.emit("    fmov x0, d0");
                return;
            }
        };

        // At entry from shared emit_float_binop: x1=lhs(f64 bits), x0=rhs(f64 bits)
        // Save rhs to stack, convert lhs to f128, save, convert rhs to f128.
        // Use raw sp addressing for our temp area (not x29-relative) since we
        // adjust sp ourselves and these are OUR temp slots, not frame slots.
        self.state.emit("    sub sp, sp, #32");
        // Save rhs f64
        self.state.emit("    str x0, [sp, #16]");
        // Convert lhs (x1) from f64 to f128
        self.state.emit("    fmov d0, x1");
        self.state.emit("    bl __extenddftf2");
        // Save lhs f128 (Q0)
        self.state.emit("    str q0, [sp]");
        // Load rhs f64 and convert to f128
        self.state.emit("    ldr x0, [sp, #16]");
        self.state.emit("    fmov d0, x0");
        self.state.emit("    bl __extenddftf2");
        // RHS f128 now in Q0, move to Q1 (second arg)
        self.state.emit("    mov v1.16b, v0.16b");
        // Load LHS f128 back to Q0 (first arg)
        self.state.emit("    ldr q0, [sp]");
        // Call the arithmetic libcall: result f128 in Q0
        self.state.emit_fmt(format_args!("    bl {}", libcall));
        // Convert result f128 back to f64 via __trunctfdf2
        self.state.emit("    bl __trunctfdf2");
        // f64 result in D0, move to x0
        self.state.emit("    fmov x0, d0");
        // Free temp space
        self.state.emit("    add sp, sp, #32");
        self.state.reg_cache.invalidate_all();
    }
}
