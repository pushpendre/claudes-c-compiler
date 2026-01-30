//! x86-64 peephole optimizer for assembly text.
//!
//! Operates on generated assembly text to eliminate redundant patterns from the
//! stack-based codegen. Lines are pre-parsed into `LineInfo` structs so hot-path
//! pattern matching uses integer/enum comparisons instead of string parsing.
//!
//! ## Pass structure
//!
//! 1. **Local passes** (iterative, up to 8 rounds): `combined_local_pass` merges
//!    5 single-scan patterns (adjacent store/load, redundant jumps, self-moves,
//!    redundant cltq, redundant zero/sign extensions) plus push/pop elimination
//!    and binary-op push/pop rewriting.
//!
//! 2. **Global passes** (once): global store forwarding (across fallthrough
//!    labels), register copy propagation, dead register move elimination,
//!    dead store elimination, compare-and-branch fusion, and memory operand
//!    folding.
//!
//! 3. **Local cleanup** (up to 4 rounds): re-run local passes to clean up
//!    opportunities exposed by global passes.

mod types;
mod passes;

pub(crate) use passes::peephole_optimize;
