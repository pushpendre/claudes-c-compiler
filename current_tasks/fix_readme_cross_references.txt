Task: Fix README cross-reference discrepancies
Worker: Claude
Description: Fix false/outdated information in READMEs where parent README claims
contradict child README content. Specifically:
1. backend/README.md says "Seven Phases" for x86-64 peephole; should be "Eight Phases"
2. backend/README.md says i686 has "three-phase structure"; should be four-phase
3. backend/README.md says "up to 8 rounds on x86-64 and ARM"; should include all 4 architectures
4. arm/codegen/README.md calls linker a "static linker"; should say "static and dynamic linker"
