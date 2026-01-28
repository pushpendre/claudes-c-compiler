# RISC-V 64 Backend

Code generation targeting the RISC-V 64-bit (RV64GC) architecture.

## Structure

- `codegen/` - Code generation implementation
  - `codegen.rs` - Main `RiscvCodegen` struct implementing the `ArchCodegen` trait. Handles instruction selection, stack frame management (s0 frame pointer), calling convention (a0-a7 for integer args, fa0-fa7 for FP), varargs, and register allocation integration.
  - `asm_emitter.rs` - `InlineAsmEmitter` trait implementation: constraint classification, scratch register allocation (GP and FP), operand loading/storing for inline asm.
  - `atomics.rs` - Sub-word atomic RMW/CAS via LR.W/SC.W loops with bit masking, and software implementations of CLZ, CTZ, BSWAP, POPCOUNT builtins.
  - `f128.rs` - F128 (quad-precision) soft-float helpers: load/store operations, binary arithmetic via compiler-rt libcalls, and constant handling for IEEE binary128.
  - `inline_asm.rs` - RISC-V inline assembly constraint classification and template substitution (%0, %[name], %lo/%hi operands).
  - `intrinsics.rs` - Software emulation of SSE-equivalent 128-bit SIMD operations (bitwise, byte compare, saturating subtract, pmovmskb) using scalar RISC-V instructions, plus hardware intrinsics (fences, CRC32, sqrt, fabs).

## Register Allocation

The RISC-V backend uses the shared linear scan register allocator that assigns callee-saved and caller-saved registers to frequently-used IR values. All four backends (x86, i686, ARM, RISC-V) have register allocation.

**Allocated registers**: s1, s7-s11 (always available, 6 registers) plus s2-s6 (conditionally available, up to 5 more). s0 is the frame pointer. s2-s6 are used as staging temporaries in `emit_call_reg_args` when a call has >= 4 GP register arguments; any not needed for staging are available for allocation, giving up to 11 callee-saved registers total.

**Strategy**: Register-only -- values with a register assignment are stored only to the register, skipping the stack slot entirely. All load paths (including `operand_to_t0`, pointer loads for Store/Load/GEP/Memcpy) check register assignments before falling back to stack loads.

**Eligible instructions**: BinOp, UnaryOp, Cmp, Cast, Load, GEP, Copy, Call, CallIndirect, Select, GlobalAddr, LabelAddr, AtomicLoad, AtomicRmw, AtomicCmpxchg. Float, i128, and long-double results are excluded.
