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
- Optimization passes (constant folding, DCE, GVN, algebraic simplification) operating on SSA form
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

### Recent Additions
- **Fix long double `++`/`--` increment/decrement**: The `inc_dec_step_and_type`
  function had no branch for `IrType::F128` (long double), causing it to fall
  through to the integer path and emit `Add(f128_value, I64(1))` - a type mismatch
  that produced a no-op. This caused all `for(long_double_var = 0; var < N; var++)`
  loops to run forever (21 timeouts). Added the missing `F128` branch returning
  `IrConst::LongDouble(1.0)`.
- **Fix extra braces around scalar initializers**: C allows redundant braces around
  scalar initializers (e.g. `int x = {{{42}}}`), but four code paths only peeled
  one layer: `lower_scalar_braced_init`, `lower_global_init` scalar case,
  `eval_init_scalar`, and `emit_struct_init` scalar field case. Two or more extra
  brace levels silently produced zero. Fixed all paths to recursively unwrap nested
  brace lists using `unwrap_nested_init_expr`. Fixes ~15-20 tests.
- **Fix K&R old-style default argument promotions in IR**: K&R-style function
  definitions require default argument promotions (`float`→`double`,
  `char`/`short`→`int`) per C calling conventions, but `build_ir_params` used
  the raw AST types instead of promoted types. This caused backends to emit
  wrong-sized stores (e.g. `movd` instead of `movq` for floats, byte stores for
  chars). Fixed `build_ir_params` to promote types for K&R functions,
  `register_function_meta` to promote char/short in caller-side signatures, and
  extended `handle_kr_float_promotion` to also narrow int back to char/short.
  Fixes 100+ x86 tests.
- **Fix floating-point stack overflow argument passing (all 3 backends)**: When
  a function has more than 8 float/double parameters, the overflow parameters
  (9th+) must go on the stack. The callee-side `classify_params` function was
  falling through to GP register assignment instead of stack assignment when FP
  registers were exhausted. Also added `force_gp` handling for RISC-V variadic
  float args. Fixes ~13 many-parameters test failures.
- **Fix sret hidden pointer not stored to stack (all 3 backends)**: The hidden
  struct-return pointer parameter (for returns > 16 bytes) was never stored from
  its argument register (%rdi/x0/a0) to the stack alloca because `emit_store_params`
  skipped unnamed parameters. The return path then loaded garbage from the
  uninitialized stack slot, segfaulting on all large struct returns. Fixed all three
  backends (x86, ARM, RISC-V). Fixes 205+ tests.
- **Fix typedef _Complex global array/scalar init**: `resolve_type_spec` only resolves
  `TypeofType`, not `TypedefName`, so dispatching complex init on its result misrouted
  typedef'd `_Complex float` to the default F64 path. Switched to use resolved `CType`
  for correct size dispatch.
- **Fix pointer-to-function-pointer dereference calls**: Calling through a
  dereferenced pointer-to-function-pointer (`(*fpp)(a,b)` where `fpp` is
  `int (**)()`) was segfaulting due to three interacting bugs: (1) the parser's
  `combine_declarator_parts` placed all inner Pointers before FunctionPointer,
  making `int (**fpp)()` produce the same derived list as `int *(*fp)()`;
  (2) `is_function_pointer_deref` used `ptr_sigs` fallback too broadly, treating
  pointer-to-function-pointers as no-op derefs; (3) `build_full_ctype` silently
  skipped Array declarators after the function pointer core. Fixed all three:
  parser now places extra indirection Pointers after FunctionPointer, the
  `ptr_sigs` fallback is only used when CType is unavailable, and the type builder
  handles post-core Array declarators.
- **Unify type computation between sema and lowering**: Extracted the duplicated
  `build_full_ctype`, `find_function_pointer_core`, and `convert_param_decls_to_ctypes`
  functions into a new shared `common/type_builder.rs` module. Both sema and lowering now
  delegate to these canonical implementations via a `TypeConvertContext` trait, eliminating
  the correctness timebomb where the two modules could diverge on declarator application
  logic. This also fixed sema's `build_full_ctype` which had a latent bug in prefix pointer
  handling for function pointer return types (applying them as outer wrappers instead of
  folding into the return type). Sema's `FunctionPointer` type spec case now produces full
  `Pointer(Function(...))` types matching the lowerer.
- **Fix function pointer struct field return type prefix pointers**: In `build_full_ctype`,
  Pointer declarators before the function pointer core (e.g., `Page *(*xFetch)(...)`)
  were incorrectly applied as outer wrappers on the function pointer type instead of
  being folded into the function's return type. This produced
  `Pointer(Pointer(Function(ret=Struct)))` instead of `Pointer(Function(ret=Pointer(Struct)))`,
  causing calls through struct member function pointers returning pointers to be classified
  as struct-by-value returns. This was the root cause of both the SQLite regression
  (622 tests segfaulting in sqlite3PcacheFetchFinish via pcache vtable) and the
  libjpeg-turbo regression.
- **Fix typedef function pointer return type signedness**: When calling through a
  typedef'd function pointer (e.g., `typedef int (*cmp_fn)(...)`), the `(*)` syntax
  marker in derived declarators was incorrectly included in the return type, making it
  `Pointer(Int)` instead of `Int`. This caused the compiler to classify the call result
  as a pointer, forcing unsigned comparison operators (`seta` instead of `setg`). Fixed
  by excluding the last `Pointer` derived (the `(*)` marker) when building the
  `FunctionTypedefInfo` return type. This resolved PostgreSQL's initdb PANIC "could not
  open critical system index 2672" — the qsort comparators used via typedef'd function
  pointers were producing incorrect sort orderings.
- **Fix va_list parameter passing (postgres zic segfault)**: Fixed `va_arg`, `va_start`,
  `va_end`, and `va_copy` when operating on a `va_list` function parameter. On x86-64,
  `va_list` is `__va_list_tag[1]` (array type), which decays to a pointer when passed as
  a parameter. The lowering code used `lower_address_of` to get the va_list address, which
  works for local va_list variables (takes address of the array) but is wrong for parameters
  (takes address of the pointer variable, giving a pointer-to-pointer). Added
  `lower_va_list_pointer` helper that checks `is_array` to distinguish: local va_list (array)
  uses address-of, parameter va_list (pointer) loads the value directly. This was the root
  cause of PostgreSQL's `zic` timezone compiler segfaulting on `--version` — the internal
  `pg_printf` → `dopr` call chain passes va_list through function parameters.
- **Fix 2D global array initializer corruption**: Fixed `flatten_global_array_init` using
  `<=` instead of `<` in the padding loop (`while values.len() <= current_idx`), which
  inserted a spurious zero element before every real value when processing inner dimensions
  of multi-dimensional global arrays. For `int arr[2][12] = {{31,28,...}, {31,29,...}}`, this
  produced `[0,31,0,28,...]` instead of `[31,28,31,30,...]`. This caused PostgreSQL's `zic`
  to report "invalid day of month" on all timezone data entries (comparing day numbers against
  `len_months[1][month]` which returned 0 for all months).
- **Increase compiler stack size to 64MB**: Large generated C files (e.g. Bison parser outputs
  like PostgreSQL's 68K-line `preproc.c`) caused stack overflow in our recursive descent parser.
  Spawn the main compilation in a thread with 64MB stack.
- **Fix forward-declared struct type resolution in pointer chains**: Fixed a bug where
  struct types captured from forward declarations (e.g., `struct B;` used as a pointer
  member before `struct B` was fully defined) retained empty field lists in the CType
  cache. When resolving field accesses through pointer chains like `pC->pB->pA->flags[0]`,
  the empty struct caused `get_field_ctype` to fail, falling back to an 8-byte (I64) load
  instead of the correct 1-byte load for `unsigned char`. Fixed by adding
  `resolve_forward_declared_ctype()` which looks up the complete struct definition from
  the ctype_cache when encountering an empty forward-declared type.
- **Fix union pointer arithmetic with stale CType sizes**: Fixed pointer arithmetic on union
  members accessed through forward-declared union pointers (e.g., `(&obj->ts) + 1`). The
  `AddressOf` handler in `get_pointer_elem_size_from_expr` was using `get_expr_type().size()`
  which returns 8 (pointer size) for all struct/union types since they map to `IrType::Ptr`,
  instead of the actual `sizeof` the union. Also fixed `resolve_ctype_size` to always prefer
  the authoritative `struct_layouts` HashMap over potentially stale `cached_size` values in
  `CType` objects. This was the root cause of Redis server failing to start — the Lua 5.1
  `getstr(ts)` macro does `(const char*)((ts) + 1)` to get string data after a `TString`
  header, and the wrong stride (8 instead of 24) caused Lua to read type metadata as string
  content, producing empty string keys in the globals table and crashing during sandbox setup.
- **Global init nested struct pointer collection and address-of member chains**: Fixed two bugs
  in global/static initializer system. (1) Nested struct init lists with braced sub-initializers
  (e.g., `.interface = { .init = myInit }`) were not collecting pointer/function-pointer
  relocations, producing NULL pointers. Added recursive scanning of nested struct init lists.
  (2) `&global.a.b` address-of expressions with multi-level member access were not resolved as
  global address offsets. Added `resolve_member_access_chain` for recursive offset computation.
  These fixes enable Redis config initialization which uses deeply nested struct/union designated
  initializers with function pointers and `&server.tls_ctx_config.session_cache_size` patterns.
- **Self-referential struct pointer arithmetic fix**: Fixed pointer arithmetic on members of
  self-referential struct types (e.g., `struct S { struct S *next; ... }`). When resolving the
  pointee type for `node->next + n`, the CType for the self-referential pointer had `cached_size=0`
  because the struct hadn't finished registering when its own fields were being typed. This caused
  pointer stride of 1 instead of `sizeof(struct S)`. Added `resolve_ctype_size()` helper that looks
  up the actual struct layout when encountering a zero-sized struct/union CType, fixing pointer
  arithmetic, array indexing, sizeof, and subscript operations on self-referential struct pointers.
  This was the root cause of redis-server crashing on startup (iterating `subcommands` array
  with stride 1 instead of 312).
- **Suppress va_start/va_end/va_copy implicit declaration warnings**: Registered `__builtin_va_start`,
  `__builtin_va_end`, and `__builtin_va_copy` in the sema builtin map so they are recognized as valid
  builtins. Previously these produced "implicit declaration" warnings on stderr, which caused configure
  scripts (like zlib's) that check for any compiler output to incorrectly detect features as unavailable.
  Specifically, zlib's configure set `-DNO_vsnprintf -DHAS_vsprintf_void` because the `va_start` warning
  made it think vsnprintf was broken, causing the `gzprintf` self-test to fail. This fix makes zlib fully
  pass all tests and may improve configure detection in other projects.
- **128-bit function return value fix**: Fixed `maybe_narrow_call_result` to exclude I128/U128
  types from the sub-64-bit narrowing cast. Previously, 128-bit return values from function calls
  had their high 64 bits destroyed by a spurious `cqto` sign-extension (cast from I64 to I128).
  Also added missing `__SIZEOF_INT128__` (=16) and `__SIZEOF_WINT_T__` (=4) predefined macros,
  enabling mbedtls Everest curve25519 and other crypto code to select 128-bit arithmetic paths.
- **Two-register struct return ABI (9-16 byte structs)**: Fixed SysV AMD64 ABI compliance for
  structs between 9-16 bytes. These must be returned in rax+rdx registers, not via hidden sret
  pointer (which is only for >16 bytes). The fix covers direct calls, explicit function pointer
  calls (`S16 (*fn)(long, long) = make; fn(10, 20)`), and typedef'd function pointer calls
  (`typedef S16 (*fn_t)(long, long); fn_t fn = make; fn(30, 40)`). Also fixed `get_expr_ctype`
  to extract return types from function pointer variable CTypes for indirect calls, and
  `extract_func_ptr_return_ctype` to handle typedef'd function pointer CTypes (which lack the
  `Function` wrapper). This was the root cause of jq runtime crashes — jq's core `jv` type is a
  16-byte struct passed by value through function pointers, and the ABI mismatch corrupted
  comparison results (making `1 > 2` evaluate to `true`).
- **128-bit compound assignment fix**: Fixed `usual_arithmetic_conversions` to use I128/U128
  as the operation type for 128-bit integers (was always forcing I64). This caused compound
  assignments (`|=`, `+=`, `&=`, `<<=`, etc.) on `__uint128_t`/`__int128` to silently
  truncate to 64 bits, losing the upper half. Fixes mbedtls bignum division (which uses
  `__attribute__((mode(TI)))` for 128-bit math), unblocking MPI/RSA crypto selftests.
- **_Complex long double argument passing**: Fixed `_Complex long double` ABI compliance.
  Previously CLD values were passed by pointer instead of being decomposed into two F128
  stack arguments (x87 extended format). Now both caller and callee decompose CLD params
  into real/imag F128 parts with proper `fldl`/`fstpt` conversion. Also fixes `_Complex double`
  to `_Complex long double` promotion which produced garbage values.
- **Parser error exit codes and diagnostics**: Parser errors now cause non-zero exit code
  (previously the compiler silently continued with exit 0). Added warnings for implicit function
  declarations and undeclared identifiers. This fixes autoconf configure script compatibility -
  enabling correct detection of byte ordering, function availability, struct members, and other
  feature checks. PostgreSQL configure now completes successfully.
- **Flexible array member string init**: Fixed FAM fields initialized with string literals
  (e.g., `struct { int len; char data[]; } x = {5, "hello"}`) where the string was not
  written to the FAM bytes and the extra allocation size was wrong.
- **Struct arrays with pointer-array fields**: Fixed global arrays of structs where fields
  are arrays of pointers (e.g., `int (*fptrs[2])()`) not emitting address relocations,
  causing segfaults when accessing function pointer or data pointer array fields.
- **Preprocessor translation phase ordering fix**: Fixed `strip_block_comments` running
  before `join_continued_lines`, which violated C11 5.1.1.2 translation phase ordering.
  Multi-line `/* ... */` comments inside `#define` macros with `\` continuations caused
  the macro body to be silently truncated. This was the root cause of mbedtls psa_crypto.c
  failing with duplicate assembly labels (65 copies of `.Luser_exit_1149`).
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
- **Compound literal postfix ops and typedef array fixes**: Compound literals now support postfix
  operators (subscript, member access, `++`/`--`) — `(int[]){1,2,3}[1]` and `(struct S){x,y}.field`
  now parse and compile correctly. Fixed typedef'd array compound literals (`typedef int a[];
  (a){1,2,3}`) to return array address instead of loading first element. Fixed `sizeof` on
  compound literals with incomplete array types to compute size from initializer list length.
  Added `CompoundLiteral` to `get_expr_ctype` so subscript element sizes are computed correctly.
- **GCC atomic builtins**: Full implementation of `__atomic_*` (C11-style) and `__sync_*` (legacy)
  builtin families across all three backends. Includes `__atomic_fetch_add/sub/and/or/xor`,
  `__atomic_add/sub/and/or/xor_fetch`, `__atomic_compare_exchange_n`, `__atomic_exchange_n`,
  `__atomic_load_n`, `__atomic_store_n`, `__atomic_thread_fence`, `__sync_fetch_and_add/sub/and/or/xor`,
  `__sync_val_compare_and_swap`, `__sync_bool_compare_and_swap`, `__sync_lock_test_and_set/release`,
  and `__sync_synchronize`. x86 uses `lock` prefix instructions, ARM64 uses ldxr/stxr exclusive
  access, RISC-V uses AMO instructions and lr/sc.
- **Position-independent code (-fPIC)**: Full PIC codegen for x86-64. External symbol access
  goes through GOT (`movq sym@GOTPCREL(%rip), %rax`), external function calls use PLT
  (`call sym@PLT`), and static/local symbols use direct RIP-relative addressing. Shared
  library compilation (`-fPIC -shared`) produces correct `.so` files. This unblocks
  PostgreSQL's shared library build (dict_snowball.so, libpq.so, etc.).
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
| zlib | PASS | Build + self-test + minigzip roundtrip all pass |
| mbedtls | PASS | All 7 selftests pass (md5, sha256, sha512, aes, rsa, ecp) |
| libpng | PASS | Builds and pngtest passes |
| jq | PASS | All 12 tests pass |
| sqlite | PASS | All 622 sqllogictest pass (100%) |
| libjpeg-turbo | PASS | Builds; cjpeg/djpeg roundtrip and jpegtran pass |
| redis | PARTIAL | Builds; version/cli work; server has bind EFAULT issue (same as postgres) |
| postgres | PARTIAL | Build succeeds; initdb fully works; `make check` temp-install succeeds but postmaster has bind EFAULT issue |

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
