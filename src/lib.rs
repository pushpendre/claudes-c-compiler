#![recursion_limit = "512"]
// Compiler functions naturally accumulate parameters (context, types, spans, flags).
// Refactoring every one into a struct would add boilerplate without improving clarity.
#![allow(clippy::too_many_arguments)]
// We use mod.rs files that re-export from a same-named child module for organization.
#![allow(clippy::module_inception)]
// Intentional: separate branches for different semantic conditions that happen to
// produce the same code today. Merging them would lose the conceptual distinction.
#![allow(clippy::if_same_then_else)]
// Complex return types arise naturally in compiler data structures; type aliases
// would just move the complexity elsewhere.
#![allow(clippy::type_complexity)]
// Peephole passes use index-based iteration over instruction arrays where the loop
// variable is used as both an index and for bounds arithmetic. Converting to iterators
// would obscure the sliding-window logic.
#![allow(clippy::needless_range_loop)]
// Methods like to_ir_type() on Copy types take &self for consistency with non-Copy
// siblings and trait method signatures.
#![allow(clippy::wrong_self_convention)]

pub(crate) mod common;
pub(crate) mod frontend;
pub(crate) mod ir;
pub(crate) mod passes;
pub mod backend;
pub mod driver;
