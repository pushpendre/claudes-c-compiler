# Parser

Recursive descent parser for C, producing a spanned AST.

## Module Organization

| File | Responsibility |
|------|---------------|
| `parser.rs` | `Parser` struct, constructor, entry point, token helpers, GCC compat |
| `expressions.rs` | Precedence climbing: comma → assignment → ... → primary |
| `types.rs` | Type specifier collection/resolution, struct/union/enum fields |
| `statements.rs` | All statement types, inline assembly (GCC syntax) |
| `declarations.rs` | External + local declarations, K&R params, initializers |
| `declarators.rs` | C declarator syntax (inside-out rule), parameter lists |
| `ast.rs` | AST node type definitions |

## Key Design Decisions

- **All modules extend `Parser` via `impl Parser` blocks** with `pub(super)` visibility.
  This avoids the overhead of trait dispatch while keeping the code well-organized.

- **Type specifier parsing collects boolean flags** because C allows specifier tokens
  in any order (`long unsigned int` == `unsigned long int`). Flags are resolved to a
  concrete `TypeSpecifier` at the end.

- **Abstract declarator suffix** (`parse_abstract_declarator_suffix` in `types.rs`)
  is a shared helper that handles pointer/array/function-pointer type wrappers after
  a type name. Previously this logic was duplicated in `parse_cast_expr`, `sizeof`,
  `typeof`, and `_Alignof`.

- **Post-type qualifier loop** (`consume_post_type_qualifiers` in `declarations.rs`)
  handles the C quirk where storage classes can appear after the type specifier.
  Previously duplicated between external and local declaration parsing.

- **K&R parameter declarations** are handled in `declarations.rs` since they only
  occur in function definitions (which are a kind of external declaration).

## What's Not Implemented

- `_Atomic(type)` is parsed and resolved to the underlying type (atomic qualifier not tracked)
- `_Generic` selection distinguishes const-qualified pointer types via `is_const` flags on associations and variables (CType still lacks qualifiers, but const is tracked separately for _Generic matching; global variables and complex expressions like casts/subscripts are not yet tracked)
