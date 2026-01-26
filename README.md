# CCC - C Compiler Collection

A C compiler written from scratch in Rust, targeting x86-64, AArch64, and RISC-V 64.

## Status

**Basic compilation pipeline functional with SSA.** ~98.5% of tests passing across all architectures (ratio 10 sample, ~2900 tests per arch).

### Working Features
- Preprocessor with `#include` file resolution (system headers, -I paths, include guards, #pragma once)
- Recursive descent parser with typedef tracking (modular: expressions, types, statements, declarations, declarators)
- Type-aware IR lowering and code generation
- **SSA construction via mem2reg** (dominator tree, dominance frontiers, phi insertion, variable renaming)
- Phi elimination for backend codegen (parallel copy lowering)
- Optimization passes (constant folding, DCE, GVN, LICM, algebraic simplification, copy propagation, CFG simplification) operating on SSA form
- x86-64 peephole optimizer (eliminates redundant store/load, push/pop, and jump patterns)
- **Compare-branch fusion**: single-use Cmp+CondBranch patterns emit direct conditional jumps (all backends)
- **Linear scan register allocator** with loop-aware liveness analysis (x86-64 and RISC-V backends)
- Three backend targets with correct ABI handling

### Test Results (ratio 10 sample)
- x86-64: 99.0% passing (2959/2990)
- AArch64: 98.2% passing (2815/2868)
- RISC-V 64: 99.5% passing (2846/2859)

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
- **Switch statements**: proper dispatch with jump table optimization
  - Dense switch (≥5 contiguous cases) compiled to indexed jump tables (all backends)
  - Sparse switch uses compare-and-branch chains
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
- Has compiled Lua, zlib, mbedtls, libpng, jq, SQLite, libjpeg-turbo
- Switch statement jump table optimization: dense switch IR terminator with indexed dispatch (all backends)
- AArch64 compare-immediate optimization: `cmp Xn, #imm12` instead of loading constants into registers
- **setjmp/longjmp liveness fix**: Extend live intervals of values live at setjmp call points to span the entire function, preventing stack slot reuse that corrupted callee-saved state after longjmp (fixed mquickjs x86 crash)
- Phi cost limiting in mem2reg prevents stack overflow from phi explosion in large switch/computed-goto functions
- GCC-compatible query flags (-dumpmachine, -dumpversion) for autoconf support
- Assembly file (.S/.s) passthrough to target assembler
- XMM register "x" constraint support for x86 inline assembly
- `__attribute__((alias, weak, visibility))` and top-level `asm()` support (musl libc)
- `register` variable `__asm__("regname")` support for pinning variables to specific registers in inline asm
- Performance: Rc<StructLayout> eliminates deep cloning in lowering (~18% compile speedup on sqlite3.c)
- AArch64 inline asm: `"Q"` memory constraint (single base register, `[Xn]` syntax) and `"w"` FP/SIMD register constraint with `%d`/`%s`/`%q` modifiers (needed for musl atomic ops and math functions)
- libffi fixes: `__builtin___clear_cache` support, tied FP register constraint propagation (RISC-V), alloca+indirect call frame addressing (ARM)
- Fix typedef pointer cast arithmetic: `((TypedefPtr) p) - 1` now correctly scales by pointee size (fixes 132 postgres test failures)
- Stdin input support (`-` filename): read source from stdin, needed by kernel cc-version.sh
- `-x` language flag: specify input language (`-x c`, `-x assembler-with-cpp`, `-x none`)
- `-Wa,` assembler pass-through: forward flags to assembler (needed by kernel as-version.sh)
- `-Wp,-MMD,path` and `-MD`/`-MF` dependency file generation: needed by kernel build system
- Bumped `__GNUC__` from 4.8 to 6.5 (satisfies kernel ≥5.1 minimum, stays <7 for glibc compat)
- Fix enum constants in designated array initializers: `expr_might_be_addr()` now excludes enum identifiers, fixing musl vfprintf's states[][] table
- Fix ARM inline asm `load_input_to_reg`: check `is_alloca` and emit `add` (address computation) instead of `ldr` (load) for alloca values, matching x86 `leaq` and RISC-V `addi` behavior (fixed musl tmpfile EFAULT on ARM)

### Project Build Status

| Project | Status | Notes |
|---------|--------|-------|
| zlib | PASS | Build + self-test + minigzip roundtrip all pass |
| lua | PASS | All 6 tests pass (version, math, strings, tables, functions, bytecode) |
| libsodium | PASS | All 7 tests pass on all architectures (init, random, sha256, secretbox, sign, box, generichash) |
| mquickjs | PASS | All 5 tests pass (closure, language, loop, builtin, bytecode roundtrip) |
| libpng | PASS | pngtest passes |
| libjpeg-turbo | PASS | Builds; cjpeg/djpeg roundtrip and jpegtran pass |
| sqlite | PASS | All 622 sqllogictest tests pass (x86, ARM, RISC-V) |
| libuv | PASS | All 7 tests pass (version, loop, timer, idle, async, tcp_bind, fs) |
| redis | PASS | All 3 tests pass (version, cli version, SET/GET roundtrip) |
| libffi | PASS | All 6 tests pass (call_int, call_double, call_pointer, call_void, call_many_args, closure) |
| musl | PASS | All 6 tests pass (hello, malloc, string_ops, math, io, environ) |
| tcc | PASS | All 78 tests pass (version, hello world, tests2 suite) |
| mbedtls | PASS | All 7 tests pass (md5, sha256, sha512, aes, rsa, ecp, selftest including ARIA) |
| jq | PASS | All 12 tests pass on all architectures (x86, ARM, RISC-V) |
| kernel | PASS (x86, ARM) | Linux 6.9 kernel builds and boots on x86 and ARM; RISC-V build fails (vDSO) |
| mquickjs-clang | PASS | All architectures pass |
| liburing | FAIL | Builds but all 5 runtime tests fail (io_uring init returns -1) |
| postgres | PARTIAL | 211/216 tests pass (5 remaining: strerror, stats, stack depth) |

See `ideas/project_triage.txt` for detailed failure analysis and fix priorities.

### What's Not Yet Implemented
- Some GNU C extensions in system headers (partial `__attribute__` support)
- Long double: partial support (x86 80-bit semantics not fully covered)
- Full register allocator (linear scan with callee-saved registers on x86 and RISC-V; further optimization possible)
- Native ELF writer (currently shells out to gcc for assembly + linking)
- ARM NEON intrinsics (arm_neon.h) - `__ARM_NEON` not defined to avoid parse failures
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

# GCC-compatible flags: -S, -c, -E, -O0..3, -g, -D, -I, -x, -MD, -MF
# Read from stdin: echo 'int main(){}' | ccc -E -x c -
```

### Recent Bug Fixes
- **Sub-int unary Neg/BitNot integer promotion**: Fixed missing sign-extension for sub-int types (`signed char`, `short`) in unary negation and bitwise NOT operations. The operations were performed in I64 without first widening the operand, causing zero-extension of signed values (e.g., `signed char -13` (0xF3) was zero-extended to 243 instead of sign-extended to -13). Fixed by inserting a Cast from the inner type to I64 before the operation, using the correct signedness for sign/zero extension.
- **Pointer-to-array struct member access**: Fixed miscompilation where accessing a struct member (e.g., `->bits`) through a dereferenced pointer-to-array-of-struct produced a 32-bit load instead of returning the field address. This caused kernel boot failures in `arch/x86/kernel/process.c` where the `cpumask_var_t` typedef (`struct cpumask[1]`) is used with the per-CPU `ACCESS_PRIVATE` macro pattern involving `typeof`/inline-asm pointer casts. The fix adds array-of-struct handling in `get_pointed_struct_layout` so the struct layout is correctly resolved for member access after array decay.

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

  passes/                Optimization: constant_fold, copy_prop, dce, gvn, licm, simplify

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
