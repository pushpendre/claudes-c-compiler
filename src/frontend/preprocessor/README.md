# Preprocessor

Text-to-text pass that handles C preprocessing directives before lexing.

## Architecture

The preprocessor operates on raw source text (not tokens), producing expanded
text that the lexer can then tokenize. This is simpler than integrating
preprocessing into the lexer but means macro source locations are lost.

### File layout

- **preprocessor.rs** - `Preprocessor` struct: orchestrates the pipeline (strip
  comments, join continuations, process directives, expand macros). Both
  top-level and `#include`d files share a single `preprocess_source()` method.
- **macro_defs.rs** - `MacroTable` and `MacroDef`: stores macro definitions and
  handles expansion including function-like macros, `#`/`##` operators, and
  `__VA_ARGS__`. Recursive expansion uses a "currently expanding" set to prevent
  infinite loops (C11 6.10.3.4).
- **conditionals.rs** - `ConditionalStack`: tracks `#if`/`#ifdef`/`#elif`/`#else`/
  `#endif` nesting. Includes a simple constant expression evaluator for `#if`.
- **builtin_macros.rs** - Defines GCC-compatible builtin function-like macros
  (e.g., `__builtin_va_start`, `__builtin_types_compatible_p`).
- **utils.rs** - Shared helpers for literal skipping (`skip_literal_bytes`,
  `copy_literal_bytes_to_string`, `copy_literal_bytes_raw`), identifier
  byte classification (`is_ident_start_byte`, `is_ident_cont_byte`), and
  zero-copy string extraction (`bytes_to_str`). All hot-path scanning
  operates on byte slices (`&[u8]`) to avoid `Vec<char>` allocation overhead.

### Important design notes

- **Multi-line macro invocations**: When a line has unbalanced parentheses, the
  preprocessor accumulates subsequent lines before expanding (up to 20 lines).
  This handles macros like `FOO(\n  arg1,\n  arg2)`.
- **Include resolution**: Searches `-I` paths then system paths. Each included
  file gets its own conditional stack (saved/restored around inclusion).
- **`#pragma once`**: Tracked by canonical file path to prevent re-inclusion.
- **Predefined macros**: `__LINE__` uses a `Cell<usize>` cache in `MacroTable`
  (set via `set_line()`) to avoid per-line `MacroDef` allocation. `__FILE__`
  is updated around includes. Target-specific macros (`__x86_64__` etc.)
  are set by `set_target()`.
