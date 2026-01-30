# x86-64 Peephole Optimizer

Post-codegen assembly-text optimizer that eliminates redundant patterns from the
stack-based code generator. Operates on generated AT&T-syntax x86-64 assembly.

## Architecture

The optimizer works in two phases:

1. **Pre-parse** (`types.rs`): Assembly lines are classified into compact `LineInfo`
   structs with pre-computed metadata (line kind, register references, stack offsets).
   This avoids repeated string parsing in hot loops.

2. **Optimization passes** (`passes/`): A pipeline of passes transforms the assembly,
   using integer/enum comparisons on `LineInfo` instead of raw string manipulation.

## Module Layout

```
peephole/
├── mod.rs              # Module root, re-exports peephole_optimize
├── types.rs            # LineInfo, LineKind, LineStore, classify_line, register tables
└── passes/
    ├── mod.rs           # Pass orchestrator (peephole_optimize entry point)
    ├── helpers.rs       # Shared utilities: register rewriting, label parsing,
    │                    # epilogue detection, instruction analysis
    ├── local_patterns.rs  # Phase 1: combined local pass (self-move, reverse-move,
    │                      # redundant jump, branch inversion, store/load,
    │                      # redundant extensions) + movq/ext fusion
    ├── push_pop.rs      # Phase 1: push/pop pair and binop/push/pop elimination
    ├── store_forwarding.rs  # Phase 2: global store→load forwarding across
    │                        # fallthrough labels (tracks slot→register mappings)
    ├── copy_propagation.rs  # Phase 2: register copy propagation across basic blocks
    ├── dead_code.rs     # Phase 2+5: dead register moves, dead stores (windowed),
    │                    # and never-read store elimination (whole-function)
    ├── compare_branch.rs  # Phase 2: cmp+setCC+test+jCC → single jCC fusion
    ├── memory_fold.rs   # Phase 2: fold stack loads into ALU memory operands
    ├── loop_trampoline.rs # Phase 4: SSA loop backedge trampoline coalescing
    └── callee_saves.rs  # Phase 6: unused callee-saved register save/restore removal
```

## Pass Pipeline (in execution order)

1. **Local passes** (iterative, up to 8 rounds): `combined_local_pass` + `fuse_movq_ext_truncation` + push/pop elimination. These are O(n) single-scan passes.

2. **Global passes** (once): store forwarding → copy propagation → dead move elimination → dead store elimination → compare-branch fusion. These track state across basic blocks.

3. **Post-global cleanup** (up to 4 rounds): Re-run local + dead code passes to clean up opportunities from step 2.

4. **Loop trampoline elimination** (once): Detects SSA loop backedge shuffle blocks and coalesces register copies to modify loop variables in-place.

5. **Never-read store elimination** (once): Whole-function analysis removes stores to stack slots that no instruction ever reads.

6. **Callee-save elimination** (once): Removes save/restore for callee-saved registers that are never referenced in the function body.

## Key Design Decisions

- **Text-based**: The optimizer works on assembly text (not a structured IR) because it
  runs after code generation. This is intentional — it catches patterns from the
  accumulator-based codegen that would be harder to eliminate at the IR level.

- **Pre-classification**: `classify_line` runs once per line and extracts all metadata
  needed by every pass. The `LineStore` keeps raw text in a flat buffer for cache
  locality while `LineInfo` provides O(1) lookups for kind, register references,
  stack offsets, and extension type.

- **NOP marking**: Eliminated instructions are marked as NOP in `LineInfo` rather than
  removed from the array. This avoids O(n²) array shifting and preserves index stability
  across passes.
