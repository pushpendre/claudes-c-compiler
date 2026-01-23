# Common

Shared data types and utilities used across frontend, IR, and backend.

## Modules

- **types.rs** - `CType` (C language types), `IrType` (IR types: I8/I16/I32/I64/U*/Ptr/F32/F64), `StructLayout` (computed struct field offsets and sizes). `IrType` includes `.size()`, `.align()`, signedness queries, and type conversion. `StructLayout` provides `resolve_init_field_idx()` for designated/positional initializer resolution.
- **symbol_table.rs** - Scoped symbol table used by sema and lowering. Push/pop scope, insert/lookup symbols by name.
- **source.rs** - Source location tracking: `Span`, `SourceLocation`, `SourceManager` for mapping byte offsets back to file/line/column.
- **error.rs** - Diagnostic infrastructure: `Diagnostic` with severity levels, spans, and notes.

## Why Types are Split

`CType` represents the C language type system (struct tags, function pointer signatures, array dimensions). `IrType` is a simpler flat enumeration for the IR. The lowering phase converts `CType` → `IrType` during code generation.
