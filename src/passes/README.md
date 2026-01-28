# Optimization Passes

SSA-based optimization passes that improve the IR before code generation.

## Available Passes

- **cfg_simplify.rs** - CFG simplification: constant branch/switch folding, dead block elimination, jump chain threading, redundant branch simplification, trivial phi simplification (single-entry phi to Copy)
- **constant_fold.rs** - Evaluates constant expressions at compile time for both integers and floats (e.g., `3 + 4` -> `7`, `-0.0 + 0.0` -> `+0.0`)
- **copy_prop.rs** - Copy propagation: replaces uses of copies with original values, follows transitive chains
- **dce.rs** - Dead code elimination: removes instructions whose results are never used
- **gvn.rs** - Dominator-based global value numbering: eliminates redundant BinOp, UnaryOp, Cmp, Cast, and GetElementPtr computations across all dominated blocks
- **licm.rs** - Loop-invariant code motion: hoists loop-invariant computations and safe loads to loop preheaders. Includes load hoisting for non-address-taken alloca-based loads that are not modified within the loop (e.g., function parameter loads). Requires single-entry preheaders for soundness
- **narrow.rs** - Integer narrowing: eliminates widen-operate-narrow patterns from C integer promotion (e.g., `Cast I32->I64, BinOp I64, Cast I64->I32` becomes `BinOp I32`). Also narrows comparisons and BinOps with sub-64-bit Load operands
- **if_convert.rs** - If-conversion: converts simple diamond-shaped branch+phi patterns to Select instructions, enabling cmov/csel emission
- **ipcp.rs** - Interprocedural constant propagation with three phases: (1) constant return propagation — identifies static functions that always return the same constant on every path (and have no side effects), then replaces calls with the constant value; (2) dead call elimination — removes calls to void functions with empty bodies (no side effects); (3) constant argument propagation — for static functions where every call site passes the same constant for a parameter, replaces the parameter with that constant inside the function body. Critical for Linux kernel config stubs and dead code elimination
- **iv_strength_reduce.rs** - Loop induction variable strength reduction: replaces expensive per-iteration multiply/shift-based array index computations (`GEP(base, iv * stride)`) with pointer induction variables that increment by stride each iteration (`ptr += stride`). Detects basic IVs (phi + add with cast lookthrough for C integer promotion), finds derived expressions (mul/shl of IV used in GEP offsets), and replaces them with pointer phi nodes. Run after LICM so loop-invariant bases are already in preheaders
- **simplify.rs** - Algebraic simplification: identity removal (`x + 0` -> `x`), strength reduction (`x * 2` -> `x << 1`), boolean simplification, math call optimization (`sqrt`/`fabs` -> hardware intrinsics, `pow(x,2)` -> `x*x`, `pow(x,0.5)` -> `sqrt`). Float-unsafe simplifications (e.g., `x + 0`, `x * 0`, `x - x`) are restricted to integer types to preserve IEEE 754 semantics
- **div_by_const.rs** - Division by constant strength reduction: replaces div/idiv by constants with multiply-and-shift sequences
- **inline.rs** - Function inlining: always_inline, small static inline, and small static (non-inline) functions. See Phase 0 below for details

## Pass Pipeline

All optimization levels (`-O0` through `-O3`, `-Os`, `-Oz`) run the same full pipeline.
While the compiler is maturing, this maximizes test coverage and avoids tier-specific bugs.

### Phase 0: Inlining (pre-loop)

Before the main optimization loop:

1. **Inline** small static/always_inline functions. Normal static inline functions are inlined up to 60 instructions / 6 blocks. Small static (non-`inline`) functions are inlined up to 30 instructions / 4 blocks — this matches GCC's -O2 behavior of inlining small static functions even without the `inline` keyword, which is critical to avoid linker errors when a static function references an undefined symbol in a conditionally-compiled dead code path. `always_inline` functions use relaxed limits (500 instructions / 500 blocks). When the **caller** has a custom section attribute (e.g., `.head.text`, `.noinstr.text`), static inline callees use the relaxed always_inline limits to prevent dangerous cross-section calls. When the caller exceeds the soft size limit (200 instructions), non-always_inline callees are skipped but always_inline callees continue. When the caller exceeds the hard cap (500 instructions), non-always_inline inlining stops to prevent kernel stack overflow from bloated frames — only tiny callees (≤5 instructions) and always_inline callees are still inlined. `always_inline` must always be inlined per C semantics (not doing so causes linker errors when the function body contains unresolvable inline asm operands). Inline asm is permitted in all inlined functions.
2. **mem2reg** re-run to promote allocas from inlined callee entry blocks (now non-entry blocks in caller)
3. **constant_fold + copy_prop + simplify + constant_fold + copy_prop** to resolve arithmetic on inlined constants
4. **resolve_inline_asm_symbols** traces GlobalAddr+GEP/Add/Cast def chains to resolve "i" constraint symbol+offset strings (e.g., `boot_cpu_data+74`) for inline asm inputs that became resolvable after inlining

### Phase 0.5: Resolve remaining `__builtin_constant_p` (post-inline, pre-loop)

5. **resolve_remaining_is_constant** resolves any `IsConstant` instructions that were not resolved to 1 during the post-inline constant folding. These have genuinely non-constant operands (global variables, non-inlined parameters) and are resolved to `Copy(0)`. This enables the main loop's cfg_simplify to fold branches guarded by `__builtin_constant_p` and eliminate dead code paths. Without this, unreachable calls to intentionally-undefined symbols (like the kernel's `__bad_udelay`) would survive to link time.

### Main loop (up to 3 iterations)

The pipeline runs up to 3 iterations, early-exiting if no changes are made:

CFG simplify -> copy prop -> narrow -> simplify -> constant fold -> GVN/CSE -> LICM -> [IVSR on iter 0] -> if-convert -> copy prop -> DCE -> CFG simplify -> IPCP

IPCP runs every iteration to propagate constants across function boundaries. The diminishing-returns early exit is suppressed when IPCP made changes, ensuring subsequent iterations can clean up the resulting dead code.

## Architecture

- **loop_analysis.rs** - Shared loop analysis utilities: natural loop detection via back edges in the dominator tree, loop body computation, loop header merging, and preheader identification. Used by LICM and IVSR
- All passes use `IrModule::for_each_function()` to iterate over defined functions
- `Instruction::dest()` provides the canonical way to extract a value defined by an instruction
- `IrConst::is_zero()`, `IrConst::is_one()`, `IrConst::zero(ty)`, `IrConst::one(ty)` provide shared constant helpers
- `IrBinOp`, `IrUnaryOp`, `IrCmpOp` derive `Hash`/`Eq` so they can be used directly as HashMap keys (e.g., in GVN)
- `IrConst::to_hash_key()` converts float constants to bit-pattern keys for hashing
- `IrBinOp::is_commutative()` identifies commutative ops for canonical ordering in CSE

## Adding New Passes

Each pass implements `fn run(module: &mut IrModule) -> usize` returning the count of changes made.
Use `module.for_each_function(|func| { ... })` to skip declarations.
Register new passes in `mod.rs::run_passes()` at the appropriate pipeline position.
