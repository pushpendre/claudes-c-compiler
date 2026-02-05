Fix x86 assembler README: correct factual errors and update to match current code.

Issues to fix:
- Line counts are wrong (parser.rs, encoder.rs, elf_writer.rs)
- AsmItem has 30 variants (not 29), missing Incbin
- SectionDirective missing comdat_group field
- SizeExpr missing SymbolRef variant
- ImmediateValue missing SymbolDiff variant
- elf_writer.rs is now 89-line thin adapter over shared ElfWriterCore
- ElfWriterCore fields count wrong, missing PhantomData
- Section struct wrong (has align_markers/comdat_group, no index)
- infer_suffix line number wrong
- GAS macro expansion is in parser.rs, not asm_preprocess
- Missing instruction families: SSE3/SSSE3, AVX2, EVEX/AVX-512, I/O, etc.
- Missing .incbin and .int directives in tables
