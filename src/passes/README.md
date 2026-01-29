# Optimization Passes

SSA-based optimization passes that improve the IR before code generation.

## Module Layout

### Pipeline orchestration (`mod.rs`)

Contains the `run_passes` entry point, per-function dirty tracking, and the
pass dependency graph that determines which passes to skip on each iteration.

### Individual passes

- **cfg_simplify.rs** - CFG simplification: constant branch/switch folding, dead block elimination, jump chain threading, redundant branch simplification, trivial phi simplification (single-entry phi to Copy)
- **constant_fold.rs** - Evaluates constant expressions at compile time for both integers and floats (e.g., `3 + 4` -> `7`, `-0.0 + 0.0` -> `+0.0`)
- **copy_prop.rs** - Copy propagation: replaces uses of copies with original values, follows transitive chains
- **dce.rs** - Dead code elimination: removes instructions whose results are never used
- **gvn.rs** - Dominator-based global value numbering: eliminates redundant BinOp, UnaryOp, Cmp, Cast, and GetElementPtr computations across all dominated blocks
- **licm.rs** - Loop-invariant code motion: hoists loop-invariant computations and safe loads to loop preheaders. Includes load hoisting for non-address-taken alloca-based loads that are not modified within the loop (e.g., function parameter loads). Requires single-entry preheaders for soundness
- **narrow.rs** - Integer narrowing: eliminates widen-operate-narrow patterns from C integer promotion (e.g., `Cast I32->I64, BinOp I64, Cast I64->I32` becomes `BinOp I32`). Also narrows comparisons and BinOps with sub-64-bit Load operands
- **if_convert.rs** - If-conversion: converts simple diamond-shaped branch+phi patterns to Select instructions, enabling cmov/csel emission
- **ipcp.rs** - Interprocedural constant propagation: constant return propagation, dead call elimination, and constant argument propagation. Critical for Linux kernel config stubs and dead code elimination
- **iv_strength_reduce.rs** - Loop induction variable strength reduction: replaces expensive per-iteration multiply/shift-based array index computations with pointer induction variables
- **simplify.rs** - Algebraic simplification: identity removal, strength reduction, boolean simplification, math call optimization. Float-unsafe simplifications are restricted to integer types to preserve IEEE 754 semantics
- **div_by_const.rs** - Division by constant strength reduction: replaces div/idiv by constants with multiply-and-shift sequences (disabled on i686)
- **inline.rs** - Function inlining: always_inline, small static inline, and small static (non-inline) functions

### Infrastructure modules

- **dead_statics.rs** - Dead static function/global elimination: BFS reachability analysis from non-static roots, removing unreachable internal-linkage symbols. Also filters `symbol_attrs` to prevent assembler errors from visibility directives on dead symbols
- **resolve_asm.rs** - Post-inline asm symbol resolution: traces `GlobalAddr + GEP/Add/Cast` def chains to resolve "i" constraint symbol+offset strings (e.g., `boot_cpu_data+74`) for inline asm inputs that became resolvable after inlining
- **loop_analysis.rs** - Shared loop analysis utilities: natural loop detection, loop body computation, preheader identification. Used by LICM and IVSR

## Pass Pipeline

All optimization levels (`-O0` through `-O3`, `-Os`, `-Oz`) run the same full pipeline.
While the compiler is maturing, this maximizes test coverage and avoids tier-specific bugs.

### Phase 0: Inlining (pre-loop)

1. **Inline** small static/always_inline functions
2. **mem2reg** re-run to promote allocas from inlined callee entry blocks
3. **constant_fold + copy_prop + simplify + constant_fold + copy_prop** to resolve arithmetic on inlined constants
4. **resolve_inline_asm_symbols** traces def chains to resolve "i" constraint symbol+offset strings

### Phase 0.5: Resolve remaining `__builtin_constant_p`

Resolve any `IsConstant` instructions that were not resolved to 1 during post-inline
constant folding. This enables cfg_simplify to fold branches guarded by
`__builtin_constant_p` and eliminate dead code paths.

### Main loop (up to 3 iterations)

CFG simplify -> copy prop -> [div_by_const on iter 0, non-i686] -> narrow -> simplify -> constant fold -> GVN/CSE + LICM + [IVSR on iter 0] -> if-convert -> copy prop -> DCE -> CFG simplify -> IPCP

Per-function dirty tracking avoids re-processing unchanged functions. Per-pass
skip tracking uses a dependency graph to skip passes when no upstream pass created
new optimization opportunities.

### Phase 11: Dead static elimination (post-loop)

Removes internal-linkage functions and globals that became unreferenced after optimization.

## Shared Utilities

Comparison evaluation and integer truncation logic lives on the IR types themselves
rather than being duplicated across passes:

- `IrCmpOp::eval_i64()` / `IrCmpOp::eval_i128()` - evaluate comparisons on constants
- `IrType::truncate_i64()` - normalize an i64 to a type's width with proper sign/zero extension

## Adding New Passes

Each pass implements `fn run(module: &mut IrModule) -> usize` returning the count of changes made.
Use `module.for_each_function(|func| { ... })` to skip declarations.
Register new passes in `mod.rs::run_passes()` at the appropriate pipeline position.
