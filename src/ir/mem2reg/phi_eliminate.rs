//! Phi elimination: lower SSA phi nodes to copies in predecessor blocks.
//!
//! This pass runs after all SSA optimizations and before backend codegen.
//! It converts each Phi instruction into Copy instructions placed at the end
//! of each predecessor block (before the terminator).
//!
//! For correctness with parallel copies (when multiple phis exist in the same
//! block), we use fresh temporary values to avoid lost-copy problems.
//! The pattern is:
//!   pred_block:
//!     ... existing code ...
//!     %tmp1 = copy src1  // for phi1
//!     %tmp2 = copy src2  // for phi2
//!     <terminator>
//!   target_block:
//!     %phi1_dest = copy %tmp1
//!     %phi2_dest = copy %tmp2
//!     ... rest of block ...

use crate::common::fx_hash::FxHashMap;
use crate::ir::ir::*;

/// Eliminate all phi nodes in the module by lowering them to copies.
pub fn eliminate_phis(module: &mut IrModule) {
    for func in &mut module.functions {
        if func.is_declaration || func.blocks.is_empty() {
            continue;
        }
        eliminate_phis_in_function(func);
    }
}

fn eliminate_phis_in_function(func: &mut IrFunction) {
    // First, find the max value ID so we can mint fresh temporaries
    let mut max_value: u32 = 0;
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Some(v) = inst.dest() {
                max_value = max_value.max(v.0);
            }
        }
    }
    let mut next_value = max_value + 1;

    // Build label -> block index map
    let label_to_idx: FxHashMap<BlockId, usize> = func.blocks
        .iter()
        .enumerate()
        .map(|(i, b)| (b.label, i))
        .collect();

    // Collect phi information from all blocks.
    // For each block, collect its phis: Vec<(dest, Vec<(src_operand, pred_block_id)>)>
    struct PhiInfo {
        dest: Value,
        incoming: Vec<(Operand, BlockId)>,
    }

    let mut block_phis: Vec<Vec<PhiInfo>> = Vec::new();
    for block in &func.blocks {
        let mut phis = Vec::new();
        for inst in &block.instructions {
            if let Instruction::Phi { dest, incoming, .. } = inst {
                phis.push(PhiInfo {
                    dest: *dest,
                    incoming: incoming.clone(),
                });
            }
        }
        block_phis.push(phis);
    }

    // For each block with phis, insert copies in predecessor blocks.
    // We use a two-step approach to handle parallel copies correctly:
    // 1. In each predecessor, copy each phi source to a fresh temporary
    // 2. At the start of the target block, copy each temporary to the phi dest

    // Collect copies to insert: pred_block_idx -> Vec<Copy instructions to insert before terminator>
    let mut pred_copies: FxHashMap<usize, Vec<Instruction>> = FxHashMap::default();
    // Collect copies to insert at start of target blocks (after removing phis)
    let mut target_copies: Vec<Vec<Instruction>> = vec![Vec::new(); func.blocks.len()];

    for (block_idx, phis) in block_phis.iter().enumerate() {
        if phis.is_empty() {
            continue;
        }

        // For single-phi blocks, we can skip the temporary and copy directly
        // in the predecessor. For multi-phi blocks, we need temporaries.
        let use_temporaries = phis.len() > 1;

        for phi in phis {
            if use_temporaries {
                let tmp = Value(next_value);
                next_value += 1;

                // In each predecessor, copy source to temporary
                for (src, pred_label) in &phi.incoming {
                    if let Some(&pred_idx) = label_to_idx.get(&pred_label) {
                        pred_copies
                            .entry(pred_idx)
                            .or_default()
                            .push(Instruction::Copy {
                                dest: tmp,
                                src: src.clone(),
                            });
                    }
                }

                // At start of target block, copy temporary to phi dest
                target_copies[block_idx].push(Instruction::Copy {
                    dest: phi.dest,
                    src: Operand::Value(tmp),
                });
            } else {
                // Single phi: copy directly in predecessors, no temporary needed
                for (src, pred_label) in &phi.incoming {
                    if let Some(&pred_idx) = label_to_idx.get(&pred_label) {
                        pred_copies
                            .entry(pred_idx)
                            .or_default()
                            .push(Instruction::Copy {
                                dest: phi.dest,
                                src: src.clone(),
                            });
                    }
                }
            }
        }
    }

    // Now apply the transformations:
    // 1. Remove phi instructions from all blocks
    // 2. Insert copies in predecessor blocks (before terminators)
    // 3. Insert copies at start of target blocks (where phis were)

    for (block_idx, block) in func.blocks.iter_mut().enumerate() {
        // Remove phi instructions
        block.instructions.retain(|inst| !matches!(inst, Instruction::Phi { .. }));

        // Prepend target copies (these go at the start, replacing the phis)
        if !target_copies[block_idx].is_empty() {
            let mut new_insts = target_copies[block_idx].clone();
            new_insts.append(&mut block.instructions);
            block.instructions = new_insts;
        }

        // Insert predecessor copies before terminator
        if let Some(copies) = pred_copies.remove(&block_idx) {
            // Insert all copies before the last instruction position
            // (they go at the end of the block, before the terminator)
            block.instructions.extend(copies);
        }
    }
}

