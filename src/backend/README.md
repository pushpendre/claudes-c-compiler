# Backend

Code generation from IR to target-specific assembly, followed by assembling and linking.

## Architecture

```
IR Module → Codegen → Assembly text → Assembler → Object file → Linker → Executable
```

Each target architecture (x86-64, i686, AArch64, RISC-V 64) implements the `ArchCodegen` trait, providing arch-specific instruction emission. The shared codegen framework handles instruction dispatch, function structure, and stack slot assignment.

## Module Layout

The shared framework is split into focused modules:

- **`state.rs`** — `CodegenState` (stack slots, alloca tracking, PIC/GOT/PLT), `StackSlot`, `SlotAddr` enum with `resolve_slot_addr()` for unified alloca address resolution, and `RegCache` for tracking which IR value is in the accumulator register to skip redundant loads. All mutable state shared across backends lives here.
- **`traits.rs`** — `ArchCodegen` trait: ~120 required methods + ~55 default implementations built from small primitives. This is the interface each backend implements. Default methods handle store/load dispatch, cast handling, i128 operations, control flow, memcpy, GEP, dynamic alloca, and **function call emission**. The `emit_call` default orchestrates the 6-phase call sequence (classify → spill fptr → stack args → reg args → call → cleanup → store result) via ~10 arch-specific hook methods. Also provides `emit_store_default`/`emit_load_default`/`emit_cast_default`/`emit_unaryop_default`/`emit_return_default` free functions so backends that override these for special types (e.g., x86 F128) can delegate non-special cases without code duplication. Trait default methods delegate to these free functions to ensure a single source of truth.
- **`generation.rs`** — `generate_module()`, `generate_function()`, `generate_instruction()`, `generate_terminator()`: arch-independent entry points that dispatch IR to trait methods. Also `is_i128_type()`, GEP fold map construction, compare-branch fusion detection, and re-exports of public items from `stack_layout.rs`.
- **`stack_layout.rs`** — Stack layout: `calculate_stack_space_common()` (three-tier slot allocation with alloca coalescing and liveness-based packing), `compute_coalescable_allocas()` (escape analysis), `find_param_alloca()`, and shared regalloc setup helpers: `collect_inline_asm_callee_saved()` (generic ASM clobber scan), `run_regalloc_and_merge_clobbers()` (allocator + clobber merge), `filter_available_regs()` (callee-saved filtering).
- **`call_abi.rs`** — `CallArgClass` enum, `CallAbiConfig`, `classify_call_args()`, `compute_stack_arg_space()`, `compute_stack_push_bytes()`. Shared function call argument classification parameterized by arch-specific register counts. Also provides `SysvStructRegClass` and `classify_sysv_struct()` — the shared SysV AMD64 per-eightbyte struct classification logic used by both caller (`classify_call_args`) and callee (`classify_params_full`) sides.
- **`call_emit.rs`** — `ParamClass` enum, `classify_params()`. Callee-side parameter classification: determines how each function parameter arrives (GP register, FP register, stack, i128 pair, F128, etc.) for `emit_store_params`.
- **`cast.rs`** — `CastKind` enum, `classify_cast()`, `FloatOp`, `classify_float_binop()`, `f128_binop_libcall()`, `F128CmpKind`, `f128_cmp_libcall()`. Shared cast decision logic (Ptr normalization, F128 reduction, float↔int, widen/narrow) and F128 soft-float libcall mapping for ARM/RISC-V.
- **`f128_softfloat.rs`** — `F128SoftFloat` trait and shared orchestration for IEEE binary128 soft-float operations (ARM + RISC-V). Both architectures lack hardware quad-precision, so F128 operations go through compiler-rt libcalls with identical orchestration logic. The trait captures ~34 arch-specific primitives (register names, instruction mnemonics, f128 register representation), and 5 shared orchestration functions (`f128_operand_to_arg1`, `f128_store_to_slot`, `f128_store_to_addr_reg`, `f128_neg`, `f128_cmp`) implement the common algorithms once. This eliminates ~350 lines of structural duplication.
- **`inline_asm.rs`** — `InlineAsmEmitter` trait, `AsmOperandKind`, `AsmOperand`, `emit_inline_asm_common()`. Shared 4-phase inline asm framework (classify→load→emit→store).
- **`liveness.rs`** — Live interval computation for IR values. Assigns sequential program points and computes [def, last_use] intervals per value. Also computes loop nesting depth per block via DFS-based back-edge detection, exported for loop-aware register allocation scoring. Provides the canonical instruction/terminator operand iterators (`for_each_operand_in_instruction`, `for_each_value_use_in_instruction`, `for_each_operand_in_terminator`) used by generation, register allocation, and liveness analysis — ensuring new IR instructions only need operand traversal updates in one place.
- **`regalloc.rs`** — Linear scan register allocator. Assigns callee-saved and caller-saved registers to eligible values using loop-depth-weighted use counts for prioritization (uses inside loops are weighted by 10^depth). Uses shared operand iterators from `liveness.rs` for use-counting.
- **`common.rs`** — Assembly output buffer, data section emission, assembler/linker invocation via GCC toolchain. Global variables are classified into sections via `classify_global()` → `GlobalSection` enum (Extern/Custom/Rodata/Tdata/Data/Common/Tbss/Bss), then emitted per-section by `emit_section_group()`. Data initialization is handled by `emit_init_data()` (all GlobalInit variants, recursing into Compound elements), `emit_symbol_directives()` (linkage+visibility), and `emit_zero_global()` (zero-init BSS pattern).
- **`mod.rs`** — `Target` enum for target dispatch, module declarations.

Per-architecture backends:
- **`x86/`** — x86-64 codegen (SysV AMD64 ABI)
- **`i686/`** — i686 codegen (cdecl, ILP32)
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
  - **Call result storage**: `emit_call_store_result` dispatches by return type (i128/F128/F32/F64/int) to 4 primitives (`emit_call_store_i128_result`, `emit_call_store_f128_result`, `emit_call_move_f32_to_acc`, `emit_call_move_f64_to_acc`). x86 overrides for F128 x87 handling.
  - **Value copy**: `emit_copy_value` is register-allocation-aware via `get_phys_reg_for_value()`, `emit_reg_to_reg_move()`, `emit_acc_to_phys_reg()`. Direct reg-to-reg copies avoid accumulator round-trips. x86 overrides for F128 x87 precision.
  - **Jump tables**: `build_jump_table()` shared free function extracts the table construction. All 64-bit backends (x86-64, ARM, RISC-V) always use relative 32-bit offsets (.long/.word target - table_base) to avoid unresolved R_*_ABS64 relocations in .rodata (which the Linux kernel linker does not resolve). Only i686 uses `emit_jump_table_rodata()` with absolute 4-byte entries.

- **Call argument classification**: `classify_call_args()` captures the shared arg-walking algorithm. `CallAbiConfig` parameterizes per-arch details (register counts, pair alignment, F128 handling, variadic float rules). Each backend implements `call_abi_config()` to provide its ABI parameters.

- **Shared emit_call default**: The trait provides a default `emit_call` that implements the shared 6-phase call algorithm. Each backend implements ~10 small hook methods (`emit_call_compute_stack_space`, `emit_call_spill_fptr`, `emit_call_stack_args`, `emit_call_reg_args`, `emit_call_instruction`, `emit_call_cleanup`, `emit_call_store_result`, etc.) that provide the arch-specific instruction emission for each phase. This eliminates ~300 lines of structural duplication per backend.

- **Three-tier stack slot allocation**: Stack slots are assigned in three tiers to minimize frame size. **Tier 1 (Allocas)**: address-taken values get permanent slots. **Tier 2 (Multi-block values)**: values spanning multiple basic blocks are packed via liveness-based interval coloring using `compute_live_intervals()` — non-overlapping live intervals share the same slot. **Tier 3 (Single-block values)**: values defined and used within a single block share a per-block coalesced pool. Multi-definition values (from phi elimination creating Copy instructions in multiple predecessors) are always routed to Tier 2 to avoid incorrect block-local sharing.

- **Stack-based codegen with register allocation**: x86 and ARM backends use a stack-based strategy where each IR value gets a stack slot. Instructions load to accumulator, operate, store back. The `RegCache` tracks which value is in the accumulator register, skipping redundant loads. On x86, binary operations and comparisons load the RHS operand directly to `%rcx` instead of using push/pop, and use immediate operands for small constants where possible. All four backends use a linear scan register allocator (see `liveness.rs` and `regalloc.rs`) that assigns callee-saved and caller-saved registers to frequently-used values. Register-allocated values are stored only to their assigned register (not the stack slot), and all load paths (including pointer loads for Store/Load/GEP/Memcpy and va_arg operations) check register assignments before falling back to stack loads.

- **GEP constant offset folding**: When a GEP has a compile-time constant offset, `emit_gep` uses efficient addressing modes (e.g., single `leaq folded(%rbp), %rax` for allocas). When the GEP is only used as a Load/Store pointer and its base is an alloca, the GEP is skipped entirely and the offset is folded into the memory operand. This yields ~5% assembly size reduction on zlib.

- **Assembler/linker via GCC**: Currently delegates to the system's GCC toolchain. Will eventually be replaced by a native ELF writer.
