Fix code/README.md: correct factual errors and improve accuracy.

Key issues:
- "16 passes" claim: 16 files but loop_analysis is shared infrastructure, not a pass
- Project Organization tree says "SSA optimization passes (16 passes)" — needs correction
- include/ description says "SSE/AVX/NEON intrinsic stubs" but also has FMA, SHA, BMI2, AES-NI headers
- Status section could mention more of the actually-passing projects (SQLite, QEMU, PostgreSQL, etc.)
- Known Limitations may be outdated
- DESIGN_DOC.md table says AArch64 linker is "Static" but it supports dynamic linking (separate task already)
