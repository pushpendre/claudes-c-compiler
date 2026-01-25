# CCC - C Compiler Collection

A C compiler written from scratch in Rust, targeting x86-64, AArch64, and RISC-V 64.

## Status

**Basic compilation pipeline functional with SSA.** ~97.7% of tests passing across all architectures (full suite, 29906 tests).

### Working Features
- Preprocessor with `#include` file resolution (system headers, -I paths, include guards, #pragma once)
- Recursive descent parser with typedef tracking (modular: expressions, types, statements, declarations, declarators)
- Type-aware IR lowering and code generation
- **SSA construction via mem2reg** (dominator tree, dominance frontiers, phi insertion, variable renaming)
- Phi elimination for backend codegen (parallel copy lowering)
- Optimization passes (constant folding, DCE, GVN, algebraic simplification, copy propagation) operating on SSA form
- x86-64 peephole optimizer (eliminates redundant store/load, push/pop, and jump patterns)
- Three backend targets with correct ABI handling

### Test Results (full suite)
- x86-64: 97.7% passing (29223/29906)
- AArch64: 98.2% passing (2817/2869, ratio 10 sample)
- RISC-V 64: 97.8% passing (2798/2861, ratio 10 sample)

### What Works
- `int main() { return N; }` for any integer N
- `printf()` with string literal arguments (via libc linking)
- Basic arithmetic (`+`, `-`, `*`, `/`, `%`)
- Local variable declarations and assignments
- `if`/`else`, `while`, `for`, `do-while` control flow
- Function calls with arbitrary arguments (including stack-overflow float/double args)
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

### Recent Work
See `git log` for full history. Key milestones:
- Refactored backend to trait-based architecture (`ArchCodegen`) with shared defaults
- Unified CType-to-IrType conversion to single canonical function
- SSA construction via iterated dominance frontier (mem2reg)
- Full `__atomic_*` and `__sync_*` builtins across all three backends
- Position-independent code (`-fPIC`) for x86-64 shared libraries
- Inline assembly support for x86, ARM, and RISC-V
- Transparent union ABI, `__int128`, `_Complex` arithmetic
- Compiles Lua, zlib, mbedtls, libpng, jq, SQLite, libjpeg-turbo successfully
- GCC-compatible query flags (-dumpmachine, -dumpversion) for autoconf support
- Assembly file (.S/.s) passthrough to target assembler
- XMM register "x" constraint support for x86 inline assembly

### Project Build Status

| Project | Status | Notes |
|---------|--------|-------|
| lua | PASS | All 6 tests pass |
| zlib | PASS | Build + self-test + minigzip roundtrip all pass |
| mbedtls | PASS | All 7 selftests pass (md5, sha256, sha512, aes, rsa, ecp) |
| libpng | PASS | Builds and pngtest passes |
| jq | PASS | All 12 tests pass |
| sqlite | PASS | All 622 sqllogictest pass (100%) |
| libjpeg-turbo | PASS | Builds; cjpeg/djpeg roundtrip and jpegtran pass |
| libuv | PASS | All 7 tests pass (version, loop, timer, idle, async, tcp_bind, fs) |
| libsodium | PASS | All 7 tests pass |
| redis | PASS | All 3 tests pass (version, cli, SET/GET roundtrip) |
| liburing | PARTIAL | Builds successfully; tests require io_uring kernel support |
| mquickjs | PASS | All 5 tests pass (closure, language, loop, builtin, bytecode roundtrip) |
| postgres | PARTIAL | Build succeeds; `make check` initdb fails during regression |
| musl | PARTIAL | Builds successfully; test programs fail to link (missing `__attribute__((alias))` support) |
| libffi | PARTIAL | Builds with .S passthrough; runtime tests crash (complex asm/stack manipulation) |
| tcc | FAIL | Build fails: preprocessor/conditional compilation issues with arch-specific defines |

### What's Not Yet Implemented
- Some GNU C extensions in system headers (partial `__attribute__` support)
- Long double: partial support (x86 80-bit semantics not fully covered)
- Register allocator (all values currently go to stack slots)
- Native ELF writer (currently shells out to gcc for assembly + linking)
- Some edge cases in complex number arithmetic

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
    sema/                Semantic analysis, symbol table, type context

  ir/                    Target-independent SSA IR
    ir.rs                Core data structures (IrModule, Instructions, BasicBlock)
    lowering/            AST → alloca-based IR (24 files: expr/stmt/types/structs/globals)
    mem2reg/             SSA promotion (dominator tree, phi insertion, variable renaming)

  passes/                Optimization: constant_fold, copy_prop, dce, gvn, simplify

  backend/               IR → assembly → object → executable
    traits.rs            ArchCodegen trait (~100 methods, ~20 shared default impls)
    generation.rs        Instruction dispatch loop (IR → trait method calls)
    call_abi.rs          Parameterized call argument classification
    call_emit.rs         Callee-side parameter store classification
    cast.rs              CastKind/FloatOp classification for cross-type casts
    state.rs             CodegenState, StackSlot, SlotAddr (alloca vs indirect)
    common.rs            Data emission, assembler/linker invocation
    inline_asm.rs        Shared inline assembly framework
    x86/codegen/         x86-64 instruction selection (SysV AMD64 ABI)
    arm/codegen/         AArch64 instruction selection (AAPCS64)
    riscv/codegen/       RISC-V 64 instruction selection (LP64D)

  common/                Shared types (CType, IrType), type_builder, symbol table, diagnostics
  driver/                CLI argument parsing, pipeline orchestration
```

Each subdirectory has its own README.md explaining the design and relationships.

## Testing

```bash
python3 /verify/verify_compiler.py --compiler target/release/ccc --arch x86 --ratio 10  # Quick (10%)
python3 /verify/verify_compiler.py --compiler target/release/ccc --arch x86              # Full suite
```
