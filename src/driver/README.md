# Driver

Orchestrates the compilation pipeline from command-line arguments through to final output.

## Responsibilities

- Parse GCC-compatible command-line flags (`-o`, `-S`, `-c`, `-E`, `-P`, `-D`, `-I`, `-O`, `-g`, etc.)
- Determine compilation mode (preprocess-only, assembly, object, full executable)
- Chain together: preprocessor → lexer → parser → sema → lowering → optimization → codegen → assembler → linker
- Select target architecture based on binary name (ccc-arm, ccc-riscv, or default x86)

## CLI Compatibility

The driver accepts a broad set of GCC flags, silently ignoring unsupported ones (like `-W` warnings, `-f` flags, `-m` arch flags). This allows using ccc as a drop-in replacement for GCC in test harnesses.
