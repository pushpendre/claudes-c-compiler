# IR (Intermediate Representation)

The IR is a low-level SSA-style representation used between the frontend AST and backend code generation.

## Structure

An `IrModule` contains:
- **Functions** (`IrFunction`) - each with typed parameters and a list of basic blocks
- **Globals** (`IrGlobal`) - global variable definitions with initializers
- **String literals** - collected during lowering for the `.rodata` section

Each basic block has a list of `Instruction`s and a `Terminator` (branch, conditional branch, return, unreachable).

## Modules

- **ir.rs** - Core IR data structures: `IrModule`, `IrFunction`, `BasicBlock`, `Instruction`, `Terminator`, `Operand`, `Value`, `IrConst`. `IrConst` provides type-safe conversion helpers (`to_i64()`, `to_u64()`, `to_u32()`, `coerce_to()`, `push_le_bytes()`, `from_i64()`, `zero()`), eliminating manual match-on-variant patterns throughout the codebase.
- **lowering/** - AST → IR lowering (see lowering/README.md)
- **mem2reg/** - Stub for SSA promotion (promotes allocas to SSA values with phi insertion)

## Key Design Decisions

- Instructions use an alloca-based memory model initially (like LLVM's `-O0`). The mem2reg pass promotes stack allocations to SSA registers.
- Values are identified by numeric IDs (`Value(u32)`), not names. This simplifies the IR and avoids name collision issues.
- The IR is target-independent: it uses abstract types (I8, I16, I32, I64, Ptr) and delegates ABI details to the backend.
