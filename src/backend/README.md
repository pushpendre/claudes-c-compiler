# Backend

Code generation from IR to target-specific assembly, followed by assembling and linking.

## Architecture

```
IR Module → Codegen → Assembly text → Assembler → Object file → Linker → Executable
```

Each target architecture (x86-64, AArch64, RISC-V 64) implements the `ArchCodegen` trait, providing arch-specific instruction emission. The shared codegen framework handles instruction dispatch, function structure, and stack slot assignment.

## Module Layout

The shared framework is split into focused modules to keep each under ~400 lines:

- **`state.rs`** — `CodegenState` (stack slots, alloca tracking, PIC/GOT/PLT), `StackSlot`, `SlotAddr` enum with `resolve_slot_addr()` for unified alloca address resolution. All mutable state shared across backends lives here.
- **`traits.rs`** — `ArchCodegen` trait: ~100 required methods + ~20 default implementations built from small primitives. This is the interface each backend implements. Default methods handle store/load dispatch, cast handling, i128 operations, control flow, memcpy, GEP, dynamic alloca, and **function call emission**. The `emit_call` default orchestrates the 6-phase call sequence (classify → spill fptr → stack args → reg args → call → cleanup → store result) via ~10 arch-specific hook methods.
- **`generation.rs`** — `generate_module()`, `generate_function()`, `generate_instruction()`, `generate_terminator()`: arch-independent entry points that dispatch IR to trait methods. Also `calculate_stack_space_common()` and `find_param_alloca()`.
- **`call_abi.rs`** — `CallArgClass` enum, `CallAbiConfig`, `classify_call_args()`, `compute_stack_arg_space()`, `compute_stack_push_bytes()`. Shared function call argument classification parameterized by arch-specific register counts.
- **`cast.rs`** — `CastKind` enum, `classify_cast()`, `FloatOp`, `classify_float_binop()`. Shared cast decision logic (Ptr normalization, F128 reduction, float↔int, widen/narrow).
- **`inline_asm.rs`** — `InlineAsmEmitter` trait, `AsmOperandKind`, `AsmOperand`, `emit_inline_asm_common()`. Shared 4-phase inline asm framework (classify→load→emit→store).
- **`codegen_shared.rs`** — Thin re-export shim. Existing `use crate::backend::codegen_shared::*` imports continue to work unchanged.
- **`common.rs`** — Assembly output buffer, data section emission, assembler/linker invocation via GCC toolchain.
- **`mod.rs`** — `Target` enum for target dispatch, module declarations.

Per-architecture backends:
- **`x86/`** — x86-64 codegen (SysV AMD64 ABI)
- **`arm/`** — AArch64 codegen (AAPCS64)
- **`riscv/`** — RISC-V 64 codegen (LP64D calling convention)

## Key Design Decisions

- **Trait-based deduplication**: The `ArchCodegen` trait eliminates structural duplication between backends. The shared `generate_module()` handles instruction dispatch, calling arch-specific methods for each operation. New IR instructions only require changes in `generation.rs` (dispatch) and each arch's trait implementation.

- **Default implementations via primitives**: Most codegen methods have default implementations that compose small arch-specific primitives (1-4 lines each). Backends only implement the primitives. Key defaults include:
  - **Store/Load**: Use `SlotAddr` to dispatch the 3-way alloca pattern uniformly
  - **Casts**: Handle i128 widening/narrowing via pair primitives, delegate others to `emit_cast_instrs`
  - **Returns**: Dispatch i128/f128/f32/f64/int to arch-specific return primitives
  - **i128 ops**: Dispatch to per-op primitives, shared divrem function-name selection
  - **Control flow**: Compose `jump_mnemonic()`, `emit_branch_nonzero()`, etc.

- **Call argument classification**: `classify_call_args()` captures the shared arg-walking algorithm. `CallAbiConfig` parameterizes per-arch details (register counts, pair alignment, F128 handling, variadic float rules). Each backend implements `call_abi_config()` to provide its ABI parameters.

- **Shared emit_call default**: The trait provides a default `emit_call` that implements the shared 6-phase call algorithm. Each backend implements ~10 small hook methods (`emit_call_compute_stack_space`, `emit_call_spill_fptr`, `emit_call_stack_args`, `emit_call_reg_args`, `emit_call_instruction`, `emit_call_cleanup`, `emit_call_store_result`, etc.) that provide the arch-specific instruction emission for each phase. This eliminates ~300 lines of structural duplication per backend.

- **Stack-based codegen**: All backends use a stack-based strategy (no register allocator yet). Each IR value gets a stack slot. Instructions load to accumulator, operate, store back.

- **Assembler/linker via GCC**: Currently delegates to the system's GCC toolchain. Will eventually be replaced by a native ELF writer.
