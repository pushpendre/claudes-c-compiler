# IR Lowering

Translates the parsed AST into alloca-based IR. This is the largest module because it
handles every C language construct. The `mem2reg` pass later promotes allocas to SSA.

## Module Organization

| File | Responsibility |
|---|---|
| `definitions.rs` | Shared data structures: `VarInfo`, `LocalInfo`, `GlobalInfo`, `DeclAnalysis`, `LValue`, `SwitchFrame`, `FuncSig`, `FunctionMeta`, `ParamKind`, `IrParamBuildResult` |
| `func_state.rs` | `FunctionBuildState` (per-function build state) and `FuncScopeFrame` (undo-log scope tracking for locals/statics/consts) |
| `type_context.rs` | Re-exports `TypeContext`, `TypeScopeFrame`, `FunctionTypedefInfo` from `frontend::sema::type_context` |
| `lowering.rs` | `Lowerer` struct, `lower()` entry point, `lower_function` pipeline, `DeclAnalysis` computation, IR emission helpers |
| `stmt.rs` | Statement dispatch (`lower_stmt`), `lower_local_decl`, `emit_struct_init`, control flow |
| `stmt_init.rs` | Local variable init helpers: expr-init, list-init, extern/func-decl handling |
| `stmt_return.rs` | Return statement: sret, two-reg struct, complex decomposition, scalar returns |
| `expr.rs` | Expression lowering: binary/unary ops, casts, ternary, sizeof, pointer arithmetic |
| `expr_builtins.rs` | `__builtin_*` intrinsics (fpclassify, clz, ctz, bswap, popcount, etc.) |
| `expr_atomics.rs` | `__atomic_*` and `__sync_*` operations via table-driven dispatch |
| `expr_calls.rs` | Function call lowering with ABI handling (sret, complex args, struct passing) |
| `expr_assign.rs` | Assignment/compound-assign, bitfield read-modify-write, type promotions |
| `expr_types.rs` | Expression type inference (`get_expr_type`, `expr_ctype`) |
| `lvalue.rs` | L-value resolution and array address computation |
| `types.rs` | `TypeSpecifier` to `IrType`/`CType`, sizeof/alignof |
| `structs.rs` | Struct/union layout cache, field offset resolution |
| `complex.rs` | `_Complex` arithmetic, assignment, and conversions |
| `global_init.rs` | Global initializer dispatch: routes to byte or compound path based on pointer content |
| `global_init_bytes.rs` | Byte-level global init serialization; shared `drill_designators` for nested designator resolution |
| `global_init_compound.rs` | Relocation-aware global init for structs/arrays containing pointer fields |
| `const_eval.rs` | Compile-time constant expression evaluation |
| `pointer_analysis.rs` | Tracks expressions producing struct addresses vs packed data |
| `ref_collection.rs` | Pre-pass to collect referenced static/extern symbols |

## Architecture

The `Lowerer` processes a `TranslationUnit` in multiple passes:

1. **Pass 0**: Collect typedefs so function signatures can resolve aliased types
2. **Pass 1**: Register function signatures (return types, param types, sret/variadic)
3. **Pass 2**: Lower each function body and global initializer into IR

### Key Types

- **`VarInfo`** - Shared type metadata (ty, elem_size, is_array, struct_layout, etc.)
  embedded in both `LocalInfo` and `GlobalInfo` via `Deref`
- **`DeclAnalysis`** - Computed once per declaration, bundles all type properties.
  Used by both local and global lowering to avoid duplicating type analysis
- **`FuncSig`** - Consolidated ABI-adjusted function signature (IrType return/params, sret/two-reg info)
- **`FunctionMeta`** - Maps function names to `FuncSig` (direct calls) and `ptr_sigs` (function pointers)
- **`FunctionInfo`** (`sema::sema`) - Sema-provided C-level function signature (CType return/params, variadic).
  Stored in `Lowerer::sema_functions`. Provides authoritative CType info that the lowerer falls back to
  when its own ABI-adjusted `FuncSig` doesn't have the info (e.g., non-pointer return types, function
  identifiers used as expressions). Also pre-populates `known_functions` during construction.
- **`FunctionTypedefInfo`** (`sema::type_context`) - Function/fptr typedef metadata (return type, params, variadic)
- **`ParamKind`** - Classifies how each C parameter maps to IR params after ABI decomposition
  (Normal, Struct, ComplexDecomposed, ComplexFloatPacked)
- **`IrParamBuildResult`** - Result of `build_ir_params`: IR param list, param kinds, sret flag
- **`FunctionBuildState`** (`func_state.rs`) - Per-function state (blocks, instrs, locals, break/continue
  labels, switch stack, user labels). Created fresh per function, discarded after
- **`TypeContext`** (`sema::type_context`) - Module-level type state (struct layouts, typedefs, enum constants,
  ctype cache). Persists across functions. Uses `TypeScopeFrame` undo-log for scoping
- **`FuncScopeFrame`** / **`TypeScopeFrame`** - Records additions/shadows per block scope.
  `pop_scope()` undoes changes in O(changes) rather than cloning entire HashMaps

### Key Helpers

- `extract_fptr_typedef_info(base, derived)` (`sema::type_context`) - Extract function-pointer typedef info
- `shadow_local_for_scope(name)` - Remove local and track for scope restoration
- `register_block_func_meta(name, ...)` - Register function metadata for block-scope declarations
- `lower_return_expr(e)` - Handles all return conventions (sret, two-reg, complex, scalar)
- `lower_local_init_expr(...)` / `lower_local_init_list(...)` - Dispatch init by type

### Function Lowering Pipeline

`lower_function` orchestrates function lowering via focused sub-methods:

1. `compute_function_return_type` - Resolve return type, register complex return CType
2. `build_ir_params` - Build IR parameter list with ABI decomposition (complex → real/imag,
   struct pass-by-value, packed complex float)
3. `allocate_function_params` - 3-phase parameter alloca emission: (1) sret pointer,
   (2) normal/struct params, (3) complex decomposed/packed reconstruction
4. `handle_kr_float_promotion` - K&R float→double narrowing stores
5. `finalize_function` - Emit implicit return, register IR function

### Global Initialization Subsystem

The global init code handles C's complex initialization rules for global/static variables.
The key architectural decision is the **two-path split** based on whether a struct contains
pointer fields:

```
global_init.rs: lower_struct_global_init()
    ├── struct_init_has_addr_fields() → true  → global_init_compound.rs (relocation-aware)
    └── struct_init_has_addr_fields() → false → global_init_bytes.rs (flat byte array)
```

**Why two paths?** Pointer fields need `GlobalAddr` entries that become linker relocations
(e.g., `const char *s = "hello"` needs a `.quad .L.str` directive). Non-pointer structs can
be fully serialized to a `Vec<u8>` byte buffer, which is simpler and more efficient.

**Shared helpers** (in `global_init_bytes.rs`):
- `drill_designators(designators, start_ty)` — Walks a chain of field/index designators
  to resolve the target type and byte offset. Used by both paths for nested designators
  like `.u.keyword` or `[3].field.subfield`. This is the single implementation point for
  designator chain resolution, avoiding duplication across the byte and compound paths.
- `resolve_struct_init_field_idx()` — Maps a positional or designated initializer to its
  target struct field index.
- `write_const_to_bytes()`, `write_bitfield_to_bytes()` — Low-level byte buffer operations.

The **compound path** (`global_init_compound.rs`) also handles struct arrays with pointer
fields via a hybrid approach: serializes scalar fields to bytes, collects pointer relocations
separately, then merges them into a single `GlobalInit::Compound` vector. Key helpers:
- `write_expr_to_bytes_or_ptrs()` — Writes a scalar expression to either the byte buffer
  (for non-pointer types) or the ptr_ranges relocation list (for pointer/function types).
  Handles bitfields too. Eliminates duplicated is-ptr/bitfield/scalar dispatch.
- `fill_composite_or_array_with_ptrs()` — Routes a braced init list through either the
  pointer-aware path (`fill_nested_struct_with_ptrs`) or the plain byte path
  (`fill_struct_global_bytes`) based on `StructLayout::has_pointer_fields()`.

## Design Decisions

- **Alloca-based**: Every local gets an `Alloca`. `mem2reg` promotes to SSA later.
- **Scope stack**: Push/pop `ScopeFrame`s instead of cloning HashMaps at block boundaries.
- **Complex ABI**: `_Complex double` decomposes to two FP registers, `_Complex float`
  packs into I64, larger types use sret. Handled in `stmt_return.rs`.
- **Short-circuit**: `&&`/`||` use conditional branches, not boolean ops.

## Relationship to Other Modules

```
parser/AST + sema/types  →  lowering  →  ir::Module  →  mem2reg → passes → codegen
```

### Data flow from sema to lowerer

The lowerer receives two things from sema:
- **`TypeContext`** (ownership transfer): typedefs, struct layouts, enum constants, function typedef info
- **`FxHashMap<String, FunctionInfo>`** (sema_functions): C-level function signatures with CType return
  types and parameter types

The lowerer uses sema's function signatures in two ways:
1. **Pre-population**: `known_functions` is seeded from sema's function map at construction time
2. **Fallback CType resolution**: `register_function_meta` uses sema's CType as source-of-truth
   for return types and param CTypes instead of re-deriving from AST. Expression type inference
   (`get_expr_ctype`, `get_call_return_type`, `get_function_return_ctype`) falls back to
   sema_functions when the lowerer's own `func_meta.sigs` doesn't have the info.

Note: The lowerer's `FuncSig` contains ABI-adjusted information (IrType, sret_size, param_struct_sizes)
that sema doesn't compute. Sema provides C-level CTypes; the lowerer adds target-specific ABI details.
