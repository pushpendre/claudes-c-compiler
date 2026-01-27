//! Interprocedural Constant Propagation (IPCP) - constant return propagation.
//!
//! This pass identifies static functions that always return the same constant
//! value on every return path, and replaces calls to those functions with
//! the constant. This is critical for the Linux kernel where `static inline`
//! stub functions often return a compile-time constant (e.g., `false` or `0`
//! when a config option is disabled), and callers need to see through the call
//! to eliminate dead branches that reference undefined symbols.
//!
//! This pass runs after the first iteration of the optimization pipeline
//! (so constant folding has simplified return values), and subsequent
//! constant folding, DCE, and CFG simplification clean up the dead code.

use crate::common::fx_hash::FxHashMap;
use crate::ir::ir::{IrConst, IrModule, Instruction, Operand, Terminator};

/// Run interprocedural constant return propagation on the module.
///
/// Returns the number of call sites replaced with constants.
pub fn run(module: &mut IrModule) -> usize {
    // Phase 1: Find static functions that always return the same constant.
    let const_returns = find_constant_return_functions(module);

    if const_returns.is_empty() {
        return 0;
    }

    // Phase 2: Replace calls to those functions with the constant value.
    let mut replacements = 0;
    for func in &mut module.functions {
        if func.is_declaration {
            continue;
        }
        for block in &mut func.blocks {
            let mut i = 0;
            while i < block.instructions.len() {
                let replace = match &block.instructions[i] {
                    Instruction::Call { dest: Some(dest), func: callee, .. } => {
                        if let Some(const_val) = const_returns.get(callee.as_str()) {
                            // Safe to replace: the callee is verified side-effect-free,
                            // and in SSA form arguments are already evaluated as separate
                            // instructions before the call.
                            Some((*dest, *const_val))
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                if let Some((dest, const_val)) = replace {
                    // Replace the call with a copy of the constant
                    block.instructions[i] = Instruction::Copy {
                        dest,
                        src: Operand::Const(const_val),
                    };
                    replacements += 1;
                }
                i += 1;
            }
        }
    }

    replacements
}

/// Analyze all static (internal-linkage) functions in the module and return
/// a map from function name to constant value for those that always return
/// the same constant on every path.
fn find_constant_return_functions(module: &IrModule) -> FxHashMap<String, IrConst> {
    let mut result = FxHashMap::default();

    for func in &module.functions {
        // Only analyze defined functions whose body we can see.
        // Both static and non-static functions are eligible: we're not removing
        // the function, just replacing calls within this TU with the constant.
        // Non-static (external linkage) functions still keep their definition
        // for other TUs to call. In C, having two strong definitions of the
        // same function is a linker error, so we can trust the body we see.
        if func.is_declaration {
            continue;
        }

        // Skip weak functions: they can be overridden by a strong definition
        // in another TU, so we can't trust the body we see.
        if func.is_weak {
            continue;
        }

        // Skip functions with no blocks (shouldn't happen for definitions)
        if func.blocks.is_empty() {
            continue;
        }

        // Skip variadic functions (they might have complex behavior)
        if func.is_variadic {
            continue;
        }

        // Check if the function body could have side effects.
        // We only want to replace calls to pure functions (no stores, no calls,
        // no inline asm, no atomics, etc.) that always return the same constant.
        if !is_side_effect_free(func) {
            continue;
        }

        // Collect all return values across all blocks
        let mut return_const: Option<IrConst> = None;
        let mut all_same = true;
        let mut has_return = false;

        for block in &func.blocks {
            if let Terminator::Return(Some(operand)) = &block.terminator {
                has_return = true;
                match operand {
                    Operand::Const(c) => {
                        if let Some(ref existing) = return_const {
                            if !const_equal(existing, c) {
                                all_same = false;
                                break;
                            }
                        } else {
                            return_const = Some(*c);
                        }
                    }
                    Operand::Value(_) => {
                        // Return value is computed, not a constant
                        all_same = false;
                        break;
                    }
                }
            } else if let Terminator::Return(None) = &block.terminator {
                // Void return - skip, we only care about value-returning functions
                has_return = true;
                all_same = false;
                break;
            }
            // Other terminators (Branch, CondBranch, Unreachable) don't affect this analysis
        }

        if has_return && all_same {
            if let Some(const_val) = return_const {
                result.insert(func.name.clone(), const_val);
            }
        }
    }

    result
}

/// Check if a function is pure (no observable side effects and result depends
/// only on inputs/constants). A pure function has no stores, no calls, no loads,
/// no inline asm, no atomics, etc. This is intentionally conservative: the target
/// use case is kernel config stubs that return literal constants without any
/// memory access.
fn is_side_effect_free(func: &crate::ir::ir::IrFunction) -> bool {
    for block in &func.blocks {
        for inst in &block.instructions {
            match inst {
                // These instructions have side effects:
                Instruction::Store { .. }
                | Instruction::Call { .. }
                | Instruction::CallIndirect { .. }
                | Instruction::InlineAsm { .. }
                | Instruction::AtomicRmw { .. }
                | Instruction::AtomicCmpxchg { .. }
                | Instruction::AtomicStore { .. }
                | Instruction::Fence { .. }
                | Instruction::Memcpy { .. }
                | Instruction::VaStart { .. }
                | Instruction::VaEnd { .. }
                | Instruction::VaCopy { .. }
                | Instruction::DynAlloca { .. }
                | Instruction::StackSave { .. }
                | Instruction::StackRestore { .. }
                | Instruction::Intrinsic { .. }
                | Instruction::VaArg { .. }
                | Instruction::SetReturnF64Second { .. }
                | Instruction::SetReturnF32Second { .. }
                | Instruction::SetReturnF128Second { .. }
                | Instruction::Load { .. }
                | Instruction::AtomicLoad { .. } => {
                    // Loads read memory that could change between calls,
                    // so functions with loads aren't truly pure.
                    return false;
                }

                // These are pure (compute a value only from inputs/constants):
                Instruction::Alloca { .. }
                | Instruction::BinOp { .. }
                | Instruction::UnaryOp { .. }
                | Instruction::Cmp { .. }
                | Instruction::GetElementPtr { .. }
                | Instruction::Cast { .. }
                | Instruction::Copy { .. }
                | Instruction::GlobalAddr { .. }
                | Instruction::Phi { .. }
                | Instruction::Select { .. }
                | Instruction::LabelAddr { .. }
                | Instruction::ParamRef { .. } => {
                    // Pure: result depends only on operands, no memory access
                }

                // GetReturn* read implicit register state from a preceding Call,
                // but Call is already rejected above, so these are unreachable.
                // Classify as side-effecting for correctness if that ever changes.
                Instruction::GetReturnF64Second { .. }
                | Instruction::GetReturnF32Second { .. }
                | Instruction::GetReturnF128Second { .. } => {
                    return false;
                }
            }
        }
    }
    true
}

/// Compare two IR constants for equality using hash keys (consistent with GVN).
fn const_equal(a: &IrConst, b: &IrConst) -> bool {
    a.to_hash_key() == b.to_hash_key()
}
