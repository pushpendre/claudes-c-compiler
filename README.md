# CCC - The Claude C Compiler

A C compiler written from scratch in Rust, targeting x86-64, i686, AArch64, and RISC-V 64.
No compiler-specific dependencies -- the frontend, IR, optimizer, and code generator are
all implemented from scratch. Assembly and linking currently delegate to the GNU toolchain;
a native ELF writer is planned.

## Building

```bash
cargo build --release
```

This produces five binaries in `target/release/`, all sharing the same source.
The target architecture is determined by the binary name at runtime.

| Binary | Target |
|--------|--------|
| `ccc` | x86-64 (default) |
| `ccc-x86` | x86-64 |
| `ccc-arm` | AArch64 |
| `ccc-riscv` | RISC-V 64 |
| `ccc-i686` | i686 (32-bit x86) |

## Usage

```bash
# Compile and link
target/release/ccc -o output input.c           # x86-64
target/release/ccc-arm -o output input.c       # AArch64
target/release/ccc-riscv -o output input.c     # RISC-V 64
target/release/ccc-i686 -o output input.c      # i686

# GCC-compatible flags
ccc -S input.c              # Emit assembly
ccc -c input.c              # Compile to object file
ccc -E input.c              # Preprocess only
ccc -O2 -o output input.c   # Optimize (O0-O3)
ccc -g -o output input.c    # Debug info
ccc -DFOO=1 -Iinclude/ input.c
ccc -Werror -Wall input.c    # Warning control
ccc -Werror=implicit-function-declaration input.c
ccc -fdiagnostics-color=always input.c  # Force colored diagnostics
ccc -x c -E -                # Read from stdin

# Build system integration (reports as GCC 6.5 for compatibility)
ccc -dumpmachine             # x86_64-linux-gnu / aarch64-linux-gnu / riscv64-linux-gnu / i686-linux-gnu
ccc -dumpversion             # 6
```

## Status

The compiler can build and run real-world C projects including the Linux kernel
and PostgreSQL.

### Projects successfully compiled

| Project | Notes |
|---------|-------|
| zlib | Build + self-test + minigzip roundtrip |
| Lua | All interpreter tests |
| libsodium | Crypto tests on all architectures |
| QuickJS | Closure, language, loop, builtin, bytecode tests |
| libpng | pngtest |
| libjpeg-turbo | cjpeg/djpeg roundtrip, jpegtran |
| SQLite | 622 sqllogictest tests |
| libuv | Loop, timer, idle, async, tcp, fs tests |
| Redis | SET/GET roundtrip |
| libffi | Call + closure tests; RISC-V: call_double returns 0 |
| musl libc | hello, malloc, string, math, io, environ |
| tcc | 78 tests including tests2 suite; i686: build succeeds but tcc can't find 32-bit system headers at runtime |
| mbedTLS | AES, RSA, ECP, SHA, ARIA self-tests |
| jq | All 12 tests on all architectures (x86, ARM, RISC-V, i686) |
| Linux kernel | Builds and boots on x86-64 and AArch64 |
| PostgreSQL | x86: 216/216; ARM: 216/216; RISC-V: 216/216 |

### Known limitations

- **External toolchain required**: Assembly and linking delegate to `gcc` (or the
  appropriate cross-compiler). The compiler produces textual assembly, not machine code
  or ELF directly. A native assembler/linker is planned.
- **Long double**: x86 80-bit extended precision is partially supported (stored as f64
  internally, losing precision for values beyond f64 range).
- **Complex numbers**: `_Complex` arithmetic has some edge-case failures.
- **GNU extensions**: Partial `__attribute__` support. ARM NEON intrinsics are partially
  implemented (core 128-bit operations work; some SSE-equivalent stubs remain).
- **i686**: The 32-bit x86 backend is new. Inline assembly with full
  operand substitution and register constraints is supported. The
  `__attribute__((fastcall))` calling convention is supported (first two
  DWORD int/ptr args in ECX/EDX, callee cleans stack). 64-bit builtins
  (`__builtin_clzll`, `__builtin_ctzll`, `__builtin_popcountll`,
  `__builtin_bswap64`) correctly split operations across eax:edx.
  SQLite passes all 622 sqllogictest tests. SSE2 intrinsics
  (including `_mm_shuffle_epi32` and AES-NI) work correctly via bundled
  headers. Redis builds and passes SET/GET roundtrip tests. libffi builds
  and passes all tests including closures. Projects with C++ dependencies
  (e.g., Redis's fast_float) or configure-based projects (e.g., TCC) require
  running `sudo scripts/setup_i686_cross.sh ~/.cargo/bin` once to set up the
  cross-compilation environment (i386 sysroot symlinks, uname wrapper for
  configure script architecture detection).

## Architecture

```
src/
  frontend/                C source -> AST
    preprocessor/          Macro expansion, #include, #ifdef, #pragma once, include guard detection
    lexer/                 Tokenization with source locations
    parser/                Recursive descent, produces AST
    sema/                  Type checking, symbol table, const evaluation

  ir/                      Target-independent SSA IR
    lowering/              AST -> alloca-based IR
    mem2reg/               SSA promotion (dominator tree, phi insertion)

  passes/                  SSA optimization passes
    constant_fold          Constant folding and propagation
    copy_prop              Copy propagation
    dce                    Dead code elimination
    gvn                    Global value numbering
    licm                   Loop-invariant code motion
    simplify               Algebraic simplification, compare-branch fusion
    cfg_simplify           CFG cleanup (unreachable blocks, empty blocks)
    inline                 Function inlining (always_inline + small static)
    if_convert             Diamond if-conversion to select
    narrow                 Integer narrowing
    div_by_const           Division strength reduction
    ipcp                   Interprocedural constant propagation
    iv_strength_reduce     Induction variable strength reduction
    loop_analysis          Shared natural loop detection (used by LICM, IVSR)

  backend/                 IR -> textual assembly (delegates to gcc for object/link)
    traits.rs              ArchCodegen trait with shared default implementations
    generation.rs          IR instruction dispatch to trait methods
    x86_common.rs          Shared x86/i686 register names, condition codes, asm templates
    x86/codegen/           x86-64 (SysV AMD64 ABI) + peephole optimizer
    arm/codegen/           AArch64 (AAPCS64)
    riscv/codegen/         RISC-V 64 (LP64D)
    i686/codegen/          i686 (cdecl, ILP32)

  common/                  Shared types, symbol table, diagnostics
  driver/                  CLI parsing, pipeline orchestration
```

Each subdirectory has its own `README.md` with design details.

### Compilation pipeline

```
C source
  -> Preprocessor (macro expansion, includes, conditionals)
  -> Lexer (tokens with source locations)
  -> Parser (recursive descent -> AST)
  -> Sema (type checking, symbol resolution)
  -> IR lowering (AST -> alloca-based IR)
  -> mem2reg (SSA promotion via iterated dominance frontier)
  -> Optimization passes (3 iterations of the pass pipeline)
  -> Code generation (IR -> textual assembly)
  -> [external gcc -c] -> object file
  -> [external gcc] -> linked executable
```

### Key design decisions

- **SSA IR**: The IR uses SSA form with phi nodes, constructed via mem2reg over
  alloca-based lowering. This is the same approach as LLVM.
- **Trait-based backends**: All four backends implement the `ArchCodegen` trait.
  Shared logic (call ABI classification, inline asm framework, f128 soft-float)
  lives in default trait methods and shared modules.
- **Linear scan register allocation**: Loop-aware liveness analysis feeds a
  three-phase allocator (callee-saved, caller-saved, spill) on all backends.
- **Text-to-text preprocessor**: The preprocessor operates on raw text, emitting
  GCC-style `# line "file"` markers for source location tracking. Include guard
  detection avoids re-processing headers with `#ifndef`/`#define`/`#endif` guards.

## Testing

```bash
# Run the verification suite (unit tests + project builds)
python3 /verify/run_all_verify.py --compiler-path target/release/

# Run unit tests only (10% sample for quick iteration)
python3 /verify/verify_compiler.py --compiler target/release/ccc --arch x86 --ratio 10

# Full unit test suite for a specific architecture
python3 /verify/verify_compiler.py --compiler target/release/ccc --arch x86
```

## Project organization

- `src/` -- Compiler source code (Rust)
- `include/` -- Bundled C headers (SSE/AVX intrinsic stubs)
- `tests/` -- Test suite (~797 test directories)
- `ideas/` -- Design docs and future work proposals
- `current_tasks/` -- Active work items (lock files for coordination)
- `completed_tasks/` -- Finished work items (for reference)
