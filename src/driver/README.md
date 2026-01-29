# Driver

Orchestrates the compilation pipeline from command-line arguments through to final output.

## Responsibilities

- Parse GCC-compatible command-line flags (`-o`, `-S`, `-c`, `-E`, `-P`, `-D`, `-I`, `-O`, `-g`, etc.)
- Determine compilation mode (preprocess-only, assembly, object, full executable)
- Chain together: preprocessor -> lexer -> parser -> sema -> lowering -> optimization -> codegen -> assembler -> linker
- Select target architecture based on binary name (ccc-arm/ccc-aarch64, ccc-riscv, ccc-i686/ccc-i386, or default x86-64)

## CLI Compatibility

The driver accepts a broad set of GCC flags, silently ignoring unrecognized ones (e.g., unknown `-f` flags, `-m` arch flags). Some flags like `-Werror`, `-Wall`, and `-Werror=implicit-function-declaration` are functional. This allows using ccc as a drop-in replacement for GCC in build systems.
