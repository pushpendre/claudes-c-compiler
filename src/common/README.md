# Common

Shared data types and utilities used across frontend, IR, and backend.

## Modules

- **types.rs** - `CType` (C language types), `IrType` (IR types: I8/I16/I32/I64/U*/Ptr/F32/F64), `StructLayout` (computed struct field offsets and sizes). `IrType` includes `.size()`, `.align()`, signedness queries, and type conversion. `CType` provides type query helpers: `is_function_pointer()`, `is_struct_or_union()`, `is_complex()`, `pointee()`, `array_element()`, `func_ptr_return_type(strict)`, and usual arithmetic conversions. `StructLayout` provides `resolve_init_field_idx()` for designated/positional initializer resolution and `has_pointer_fields()` for checking if any field recursively contains pointer/function types.
- **symbol_table.rs** - Scoped symbol table used by sema and lowering. Push/pop scope, insert/lookup symbols by name.
- **source.rs** - Source location tracking: `Span`, `SourceLocation`, `SourceManager` for mapping byte offsets back to file/line/column.
- **type_builder.rs** - `TypeConvertContext` trait with a default `resolve_type_spec_to_ctype` method that handles all 22 primitive C type mappings, pointers, arrays, and function pointers. Sema and lowering implement only 4 divergent methods (typedef, struct/union, enum, typeof), ensuring primitive type mapping can never silently diverge. Also provides shared `build_full_ctype` and `find_function_pointer_core` for converting declarator chains into `CType`.
- **const_arith.rs** - Shared constant-expression evaluation. Contains the single canonical implementations of integer and float binary operations (`eval_const_binop`, `eval_const_binop_float`), unary operations (`negate_const`, `bitnot_const`), cast chain bit manipulation (`truncate_and_extend_bits`), and zero-expression detection (`is_zero_expr`). Both `sema::const_eval` and `lowering::const_eval` delegate here. Each caller determines width/signedness from their own type system (CType vs IrType) and passes it as parameters, keeping the arithmetic itself in one place.

## Why Types are Split

`CType` represents the C language type system (struct tags, function pointer signatures, array dimensions). `IrType` is a simpler flat enumeration for the IR. The lowering phase converts `CType` → `IrType` during code generation.
