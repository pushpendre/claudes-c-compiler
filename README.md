# CCC - C Compiler Collection

A C compiler written from scratch in Rust, targeting x86-64, AArch64, and RISC-V 64.

## Status

**Basic compilation pipeline functional with SSA.** ~81% of x86-64 tests passing.

### Working Features
- Preprocessor with `#include` file resolution (system headers, -I paths, include guards, #pragma once)
- Recursive descent parser with typedef tracking
- Type-aware IR lowering and code generation
- **SSA construction via mem2reg** (dominator tree, dominance frontiers, phi insertion, variable renaming)
- Phi elimination for backend codegen (parallel copy lowering)
- Optimization passes (constant folding, DCE, GVN, algebraic simplification) operating on SSA form
- Three backend targets with correct ABI handling

### Test Results (10% sample, ratio 10)
- x86-64: ~81.4% passing (2436/2991)
- AArch64: ~81.2% passing (2331/2869)
- RISC-V 64: ~80.1% passing (2293/2861)

### What Works
- `int main() { return N; }` for any integer N
- `printf()` with string literal arguments (via libc linking)
- Basic arithmetic (`+`, `-`, `*`, `/`, `%`)
- Local variable declarations and assignments
- `if`/`else`, `while`, `for`, `do-while` control flow
- Function calls with up to 6/8 arguments
- Comparison operators
- **Type-aware code generation**: correct-sized load/store instructions for all types
  - `char` uses byte operations (movb/strb/sb)
  - `short` uses 16-bit operations (movw/strh/sh)
  - `int` uses 32-bit operations (movl/str w/sw)
  - `long`/pointers use 64-bit operations (movq/str x/sd)
  - Sign extension on loads for smaller types
- Array declarations with correct element sizes (4 bytes for int[], 1 for char[], etc.)
- Array subscript read/write (`arr[i]`, `arr[i] = val`)
- Array initializer lists (`int arr[] = {1, 2, 3}`)
- Pointer dereference assignment (`*p = val`, `val = *p`)
- Address-of operator (`&x`, `&arr[i]`)
- Compound assignment on arrays/pointers (`arr[i] += val`, `*p -= val`)
- Pre/post increment/decrement on arrays/pointers (`arr[i]++`, `++(*p)`)
- Short-circuit evaluation for `&&` and `||`
- Proper `sizeof` for basic types and arrays
- 32-bit arithmetic operations for `int` type (addl/subl/imull/idivl on x86)
- **Switch statements**: proper dispatch via if-else comparison chain
  - Case with break, fallthrough, default
  - Nested switch statements
  - Constant case expressions (integer literals, char literals, arithmetic)
- **Global variables**: declarations with initializers, arrays, zero-initialized (.bss)
  - Global scalar initializers (`int x = 42;`)
  - Global array initializers (`int arr[5] = {1, 2, 3, 4, 5};`)
  - Global pointer-to-string (`char *msg = "hello";`)
  - Read/write access to globals from any function
  - Constant expression evaluation for initializers

### Recent Additions
- **RISC-V bit manipulation ops**: Implemented software fallbacks for CLZ, CTZ, BSWAP,
  and POPCOUNT on RISC-V (previously returned 0). Also fixed x86 to use correct
  32-bit instruction variants (bsrl, bswapl, etc.) for 32-bit types.
- **RISC-V inline asm constraints**: Added support for "A" (address for AMO/LR/SC),
  "f" (FP register), "I"/"i" (immediate), "J"/"rJ" (zero register), and %z modifier.
  Fixes ~22 RISC-V inline assembly test cases.
- **Function typedef declarations**: Fixed declarations using typedef'd function types
  (e.g., `typedef int func_t(int); func_t add;`) being emitted as BSS variables instead of
  being recognized as function forward declarations. Fixes duplicate symbol errors in mbedtls.
- **Parser EOF crash fix**: Fixed parser `advance()` panicking with index-out-of-bounds when
  reaching end of token stream during error recovery on very large files.
- **SSA mem2reg pass**: Full SSA construction implemented via the standard iterated dominance
  frontier algorithm. Promotes scalar stack allocas to SSA registers with phi nodes. Includes:
  dominator tree computation (Cooper-Harvey-Kennedy algorithm), dominance frontier calculation,
  phi insertion at join points, variable renaming via dominator tree DFS, and phi elimination
  (lowering to parallel copies) before backend codegen. The IR now has a Phi instruction variant.
  All optimization passes operate on proper SSA form. Unlocks future SSA-based optimizations
  (SCCP, dominator-based GVN, copy propagation, LICM, etc.).
- **Integer promotion for unary operators and switch**: Unary operators (`-`, `~`, `+`) now
  correctly apply C99 integer promotion rules, promoting `char`/`short` operands to `int` before
  the operation. Also fixed switch statement controlling expression promotion. Fixed binary op
  type inference (`get_expr_type`) to return `common_type` instead of falling through. Fixed
  constant folding for `Neg`/`BitNot` on `I8`/`I16` to produce `I32` results.
- **GCC atomic builtins**: Full implementation of `__atomic_*` (C11-style) and `__sync_*` (legacy)
  builtin families across all three backends. Includes `__atomic_fetch_add/sub/and/or/xor`,
  `__atomic_add/sub/and/or/xor_fetch`, `__atomic_compare_exchange_n`, `__atomic_exchange_n`,
  `__atomic_load_n`, `__atomic_store_n`, `__atomic_thread_fence`, `__sync_fetch_and_add/sub/and/or/xor`,
  `__sync_val_compare_and_swap`, `__sync_bool_compare_and_swap`, `__sync_lock_test_and_set/release`,
  and `__sync_synchronize`. x86 uses `lock` prefix instructions, ARM64 uses ldxr/stxr exclusive
  access, RISC-V uses AMO instructions and lr/sc.
- **Flat struct/array initialization fix**: Fixed struct initialization with flat (non-braced)
  initializer lists when the struct contains array fields. Now correctly consumes multiple
  items from the init list for array fields (e.g., `struct { int a[10]; int b; } x = {1,2,...,10,11}`).
  Also fixed global struct init for the same case.
- **Multi-dimensional array init fix**: Fixed boundary snapping for bare expressions in
  multi-dimensional array initializers. Previously, bare scalar values at multi-dim levels
  were incorrectly snapped to sub-array boundaries, causing values to be placed in wrong positions.
- **va_arg register save area (all architectures)**: Fixed variadic function argument passing
  on all three backends. Variadic functions now save register-passed arguments to a register
  save area so `va_arg` can access them. Previously, variadic args passed in registers were
  inaccessible because `va_start` only pointed to the stack overflow area.
- **CType::Bool and _Bool normalization**: Added `CType::Bool` variant to distinguish `_Bool` from
  `unsigned char` at the type level. Stores to `_Bool` lvalues through pointer dereference,
  array subscript, struct/union member access, compound assignment, and increment/decrement
  now correctly normalize values to 0 or 1.
- **#include file resolution**: Full `#include` support reading actual system headers from
  `/usr/include`, with search path support (-I), include guard recognition, #pragma once,
  block comment stripping (C-style `/* */` comments no longer confuse the preprocessor),
  recursive include processing, and GCC-compatible predefined macros.
- **Static local variables**: `static` locals are emitted as globals with mangled names
  (e.g., `func.varname`), preserving values across function calls. Works with
  scalars, arrays, and initializers. Storage class tracking in parser and AST.
- **Typedef tracking**: parser correctly registers typedef names from `typedef` declarations
  (both top-level and local), enabling cast expressions like `(mytype)expr` with user-defined types
- **Built-in type names**: standard C type names (`size_t`, `int32_t`, `FILE`, etc.) pre-seeded
  for correct parsing without full header inclusion
- **Cast expression lowering**: emits proper IR Cast instructions for type-narrowing casts
- **_Complex type handling**: parses and skips `_Complex`/`__complex__` type modifier
- **Inline asm skipping**: parses and skips `asm`/`__asm__` statements and expressions
- **GCC extension keywords**: `__volatile__`, `__const__`, `__inline__`, `__restrict__`,
  `__signed__`, `__noreturn__` recognized as their standard equivalents

### Project Build Status

| Project | Status | Notes |
|---------|--------|-------|
| lua | PASS | All 6 tests pass |
| zlib | PASS | Build + self-test + minigzip roundtrip pass |
| mbedtls | FAIL | Preprocessor macro expansion issues in large files (psa_crypto.c) |
| libpng | FAIL | Preprocessor conditional issues (PNG_FLOATING_POINT_SUPPORTED) |
| jq | FAIL | Build timeout |
| sqlite | FAIL | Not yet tested |
| libjpeg-turbo | FAIL | Not yet tested |
| redis | FAIL | Not yet tested |

### What's Not Yet Implemented
- Parser support for GNU C extensions in system headers (`__attribute__`, `__asm__` renames)
- Floating point (partial - float32/64 work, long double incomplete)
- Full cast semantics (truncation/sign-extension in some cases)
- Inline assembly (parsed and executed on x86, skipped on other backends)
- Native assembler/linker (currently uses gcc)

## Building

```bash
cargo build --release
# Produces: target/release/ccc (x86), ccc-arm, ccc-riscv
```

## Usage

```bash
target/release/ccc -o output input.c       # x86-64
target/release/ccc-arm -o output input.c   # AArch64
target/release/ccc-riscv -o output input.c # RISC-V 64

# GCC-compatible flags: -S, -c, -E, -O0..3, -g, -D, -I
```

## Architecture

```
src/
  frontend/              C source → AST
    preprocessor/        Macro expansion, #include, #ifdef
    lexer/               Tokenization with source locations
    parser/              Recursive descent, produces AST
    sema/                Semantic analysis, symbol table

  ir/                    Target-independent SSA IR
    ir.rs                Core data structures (IrModule, Instructions, BasicBlock)
    lowering/            AST → alloca-based IR (split into expr/stmt/lvalue/types/structs)
    mem2reg/             SSA promotion (stub)

  passes/                Optimization: constant_fold, dce, gvn, simplify

  backend/               IR → assembly → object → executable
    common.rs            Shared data emission, assembler/linker invocation
    x86/codegen/         x86-64 instruction selection (SysV ABI)
    arm/codegen/         AArch64 instruction selection (AAPCS64)
    riscv/codegen/       RISC-V 64 instruction selection

  common/                Shared types (CType, IrType), symbol table, diagnostics
  driver/                CLI argument parsing, pipeline orchestration
```

Each subdirectory has its own README.md explaining the design and relationships.

## Testing

```bash
python3 /verify/verify_compiler.py --compiler target/release/ccc --arch x86 --ratio 10  # Quick (10%)
python3 /verify/verify_compiler.py --compiler target/release/ccc --arch x86              # Full suite
```
