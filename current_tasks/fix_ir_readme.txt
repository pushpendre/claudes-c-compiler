Fix IR README (src/ir/README.md): improve readability and accuracy

Changes planned:
- Condense intrinsics section from 175 lines of individual variant tables to a compact
  category summary (~20 lines) with a note about is_pure() semantics
- Trim constants section: keep variant table but condense method listing; remove
  ConstHashKey detail (implementation detail, not design-relevant)
- Add missing content: AddressSpace type explanation, pass interface overview,
  IR debugging/printing note
- Add cross-reference to passes/README.md
- Ensure level of detail is appropriate for a higher-level design doc
