# Optimization Passes

SSA-based optimization passes that improve the IR before code generation.

## Available Passes

- **cfg_simplify.rs** - CFG simplification: dead block elimination, jump chain threading, redundant branch simplification
- **constant_fold.rs** - Evaluates constant expressions at compile time for both integers and floats (e.g., `3 + 4` -> `7`, `-0.0 + 0.0` -> `+0.0`)
- **copy_prop.rs** - Copy propagation: replaces uses of copies with original values, follows transitive chains
- **dce.rs** - Dead code elimination: removes instructions whose results are never used
- **gvn.rs** - Dominator-based global value numbering: eliminates redundant BinOp, UnaryOp, Cmp, Cast, and GetElementPtr computations across all dominated blocks
- **licm.rs** - Loop-invariant code motion: hoists loop-invariant computations and safe loads to loop preheaders. Includes load hoisting for non-address-taken alloca-based loads that are not modified within the loop (e.g., function parameter loads). Requires single-entry preheaders for soundness
- **if_convert.rs** - If-conversion: converts simple diamond-shaped branch+phi patterns to Select instructions, enabling cmov/csel emission
- **simplify.rs** - Algebraic simplification: identity removal (`x + 0` -> `x`), strength reduction (`x * 2` -> `x << 1`), boolean simplification. Float-unsafe simplifications (e.g., `x + 0`, `x * 0`, `x - x`) are restricted to integer types to preserve IEEE 754 semantics

## Pass Pipeline

All optimization levels (`-O0` through `-O3`, `-Os`, `-Oz`) run the same full pipeline.
While the compiler is maturing, this maximizes test coverage and avoids tier-specific bugs.

The pipeline runs up to 2 iterations, early-exiting if no changes are made:

CFG simplify -> copy prop -> simplify -> constant fold -> GVN/CSE -> LICM -> if-convert -> copy prop -> DCE -> CFG simplify

## Architecture

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
