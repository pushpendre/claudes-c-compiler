//! Dominator-based Global Value Numbering (GVN) pass.
//!
//! This pass assigns "value numbers" to expressions and replaces redundant
//! computations with references to previously computed values (CSE).
//!
//! The pass walks the dominator tree in DFS order with scoped hash tables,
//! so expressions computed in dominating blocks are visible to all dominated
//! blocks. On backtracking, the hash tables are restored to their previous
//! state (same scoping pattern as rename_block in mem2reg).
//!
//! Value-numbered instruction types:
//! - BinOp (with commutative operand canonicalization)
//! - UnaryOp
//! - Cmp
//! - Cast (type-to-type conversions)
//! - GetElementPtr (base + offset address computation)
//! - Load (redundant load elimination within dominator scope, invalidated
//!   by stores, calls, and other memory-clobbering instructions)

use crate::common::fx_hash::FxHashMap;
use crate::common::types::{AddressSpace, IrType};
use crate::ir::ir::*;
use crate::ir::analysis;

/// A value number expression key. Two instructions with the same ExprKey
/// compute the same value (assuming their operands are equivalent).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ExprKey {
    BinOp { op: IrBinOp, lhs: VNOperand, rhs: VNOperand },
    UnaryOp { op: IrUnaryOp, src: VNOperand },
    Cmp { op: IrCmpOp, lhs: VNOperand, rhs: VNOperand },
    Cast { src: VNOperand, from_ty: IrType, to_ty: IrType },
    Gep { base: VNOperand, offset: VNOperand, ty: IrType },
    /// Load CSE key: two loads from the same pointer with the same type
    /// produce the same value if no intervening memory modification occurs.
    Load { ptr: VNOperand, ty: IrType },
}

/// Returns true if the ExprKey represents a Load (memory-dependent expression).
impl ExprKey {
    fn is_load(&self) -> bool {
        matches!(self, ExprKey::Load { .. })
    }
}

/// A value-numbered operand: either a constant or a value number.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum VNOperand {
    Const(ConstHashKey),
    ValueNum(u32),
}

/// Run dominator-based GVN on the entire module.
/// Returns the number of instructions eliminated.
pub fn run(module: &mut IrModule) -> usize {
    module.for_each_function(run_gvn_function)
}

/// Run dominator-based GVN on a single function.
pub(crate) fn run_gvn_function(func: &mut IrFunction) -> usize {
    let num_blocks = func.blocks.len();
    if num_blocks == 0 {
        return 0;
    }

    // Allocate value number table indexed by Value ID
    let max_id = func.max_value_id() as usize;
    let mut value_numbers: Vec<u32> = vec![u32::MAX; max_id + 1];
    let mut next_vn: u32 = 0;

    // Scoped expression-to-value map using a rollback log.
    // Pure expressions (BinOp, Cmp, etc.) map ExprKey -> Value.
    // Load expressions also store a "load generation" tag: the load entry is
    // only valid if its generation matches the current load_generation counter.
    // On memory clobber, we simply bump load_generation (O(1)) instead of
    // iterating all accumulated load keys to remove them (O(n)).
    let mut expr_to_value: FxHashMap<ExprKey, Value> = FxHashMap::default();
    let mut load_expr_to_value: FxHashMap<ExprKey, (Value, u32)> = FxHashMap::default();
    let mut load_generation: u32 = 0;

    // Fast path for single-block functions: skip CFG/dominator computation
    // entirely. We only need to do CSE within the single block.
    if num_blocks == 1 {
        let mut rollback_log: Vec<(ExprKey, Option<Value>)> = Vec::new();
        let mut load_rollback_log: Vec<(ExprKey, Option<(Value, u32)>)> = Vec::new();
        let mut vn_log: Vec<(usize, u32)> = Vec::new();
        return process_block(
            0, func, &mut value_numbers, &mut next_vn,
            &mut expr_to_value, &mut load_expr_to_value, &mut load_generation,
            &mut rollback_log, &mut load_rollback_log, &mut vn_log,
        );
    }

    // Build CFG and dominator tree
    let label_to_idx = analysis::build_label_map(func);
    let (preds, succs) = analysis::build_cfg(func, &label_to_idx);
    let idom = analysis::compute_dominators(num_blocks, &preds, &succs);
    let dom_children = analysis::build_dom_tree_children(num_blocks, &idom);

    // Rollback log: tracks (key, old_value) pairs pushed at each scope
    let mut rollback_log: Vec<(ExprKey, Option<Value>)> = Vec::new();
    let mut load_rollback_log: Vec<(ExprKey, Option<(Value, u32)>)> = Vec::new();
    // Track value_numbers slots assigned, for rollback
    let mut vn_log: Vec<(usize, u32)> = Vec::new();

    let mut total_eliminated = 0;

    // DFS over the dominator tree
    gvn_dfs(
        0, // start from entry block
        func,
        &dom_children,
        &preds,
        &mut value_numbers,
        &mut next_vn,
        &mut expr_to_value,
        &mut load_expr_to_value,
        &mut load_generation,
        &mut rollback_log,
        &mut load_rollback_log,
        &mut vn_log,
        &mut total_eliminated,
    );

    total_eliminated
}

/// Recursive DFS over the dominator tree for GVN.
/// Processes block_idx, then recurses into dominated children.
/// Uses rollback logs to restore state on backtracking.
fn gvn_dfs(
    block_idx: usize,
    func: &mut IrFunction,
    dom_children: &[Vec<usize>],
    preds: &analysis::FlatAdj,
    value_numbers: &mut Vec<u32>,
    next_vn: &mut u32,
    expr_to_value: &mut FxHashMap<ExprKey, Value>,
    load_expr_to_value: &mut FxHashMap<ExprKey, (Value, u32)>,
    load_generation: &mut u32,
    rollback_log: &mut Vec<(ExprKey, Option<Value>)>,
    load_rollback_log: &mut Vec<(ExprKey, Option<(Value, u32)>)>,
    vn_log: &mut Vec<(usize, u32)>,
    total_eliminated: &mut usize,
) {
    // Save state for rollback
    let rollback_start = rollback_log.len();
    let load_rollback_start = load_rollback_log.len();
    let vn_log_start = vn_log.len();
    let saved_load_generation = *load_generation;

    // At block entry, decide whether to invalidate inherited Load CSE entries.
    // Load CSE across blocks is safe when this block has exactly one CFG
    // predecessor (straight-line code). At merge points (multiple predecessors),
    // conservatively invalidate all Load entries because a non-dominating
    // predecessor may have stored to memory, making cached loads stale.
    //
    // Invalidation is O(1): just bump load_generation. Entries with older
    // generations are ignored during lookup.
    if block_idx != 0 && preds.len(block_idx) > 1 {
        *load_generation += 1;
    }

    // Process instructions in this block
    let eliminated = process_block(
        block_idx,
        func,
        value_numbers,
        next_vn,
        expr_to_value,
        load_expr_to_value,
        load_generation,
        rollback_log,
        load_rollback_log,
        vn_log,
    );
    *total_eliminated += eliminated;

    // Recurse into dominator tree children.
    // Iterate by index to avoid cloning the children Vec.
    let num_children = dom_children[block_idx].len();
    for ci in 0..num_children {
        let child = dom_children[block_idx][ci];
        gvn_dfs(
            child,
            func,
            dom_children,
            preds,
            value_numbers,
            next_vn,
            expr_to_value,
            load_expr_to_value,
            load_generation,
            rollback_log,
            load_rollback_log,
            vn_log,
            total_eliminated,
        );
    }

    // Rollback: restore expr_to_value to state before this block
    while rollback_log.len() > rollback_start {
        let (key, old_val) = rollback_log.pop().unwrap();
        if let Some(val) = old_val {
            expr_to_value.insert(key, val);
        } else {
            expr_to_value.remove(&key);
        }
    }

    // Rollback: restore load_expr_to_value to state before this block
    while load_rollback_log.len() > load_rollback_start {
        let (key, old_val) = load_rollback_log.pop().unwrap();
        if let Some(val) = old_val {
            load_expr_to_value.insert(key, val);
        } else {
            load_expr_to_value.remove(&key);
        }
    }

    // Rollback: restore value_numbers to state before this block
    while vn_log.len() > vn_log_start {
        let (idx, old_vn) = vn_log.pop().unwrap();
        value_numbers[idx] = old_vn;
    }

    // Rollback: restore load_generation to state before this block
    *load_generation = saved_load_generation;
}

/// Check if an instruction may modify memory, invalidating cached load values.
/// This is conservative: any instruction that could write to memory or call
/// external code (which could write to memory) returns true.
fn clobbers_memory(inst: &Instruction) -> bool {
    matches!(
        inst,
        Instruction::Store { .. }
            | Instruction::Call { .. }
            | Instruction::CallIndirect { .. }
            | Instruction::Memcpy { .. }
            | Instruction::AtomicRmw { .. }
            | Instruction::AtomicCmpxchg { .. }
            | Instruction::AtomicStore { .. }
            | Instruction::Fence { .. }
            | Instruction::InlineAsm { .. }
            | Instruction::VaStart { .. }
            | Instruction::VaEnd { .. }
            | Instruction::VaCopy { .. }
    )
}

/// Process a single basic block for GVN.
/// Returns the number of instructions eliminated.
///
/// Load CSE entries are stored separately in `load_expr_to_value`, tagged with
/// a generation counter for O(1) invalidation on memory clobber. Cross-block
/// Load CSE propagation is controlled by `gvn_dfs` which invalidates Load
/// entries at merge points (blocks with multiple CFG predecessors).
fn process_block(
    block_idx: usize,
    func: &mut IrFunction,
    value_numbers: &mut Vec<u32>,
    next_vn: &mut u32,
    expr_to_value: &mut FxHashMap<ExprKey, Value>,
    load_expr_to_value: &mut FxHashMap<ExprKey, (Value, u32)>,
    load_generation: &mut u32,
    rollback_log: &mut Vec<(ExprKey, Option<Value>)>,
    load_rollback_log: &mut Vec<(ExprKey, Option<(Value, u32)>)>,
    vn_log: &mut Vec<(usize, u32)>,
) -> usize {
    let mut eliminated = 0;
    let mut new_instructions = Vec::with_capacity(func.blocks[block_idx].instructions.len());
    // GVN replaces instructions 1:1 (original or Copy), so spans stay parallel
    let new_spans = std::mem::take(&mut func.blocks[block_idx].source_spans);

    for inst in func.blocks[block_idx].instructions.drain(..) {
        // Before processing the instruction, check if it clobbers memory.
        // If so, invalidate all cached Load CSE entries by bumping the
        // generation counter. This is O(1) instead of iterating all load keys.
        if clobbers_memory(&inst) {
            *load_generation += 1;
        }

        match make_expr_key(&inst, value_numbers, next_vn, vn_log) {
            Some((expr_key, dest)) => {
                let is_load = expr_key.is_load();

                // Look up: check pure expr map, or load map with generation check
                let existing = if is_load {
                    load_expr_to_value.get(&expr_key).and_then(|&(val, gen)| {
                        if gen == *load_generation { Some(val) } else { None }
                    })
                } else {
                    expr_to_value.get(&expr_key).copied()
                };

                if let Some(existing_value) = existing {
                    // This expression was already computed
                    let idx = existing_value.0 as usize;
                    let existing_vn = if idx < value_numbers.len() && value_numbers[idx] != u32::MAX {
                        value_numbers[idx]
                    } else {
                        let vn = *next_vn;
                        *next_vn += 1;
                        vn
                    };
                    let dest_idx = dest.0 as usize;
                    if dest_idx < value_numbers.len() {
                        let old_vn = value_numbers[dest_idx];
                        vn_log.push((dest_idx, old_vn));
                        value_numbers[dest_idx] = existing_vn;
                    }
                    new_instructions.push(Instruction::Copy {
                        dest,
                        src: Operand::Value(existing_value),
                    });
                    eliminated += 1;
                } else {
                    // New expression - assign value number and record it
                    let vn = *next_vn;
                    *next_vn += 1;
                    let dest_idx = dest.0 as usize;
                    if dest_idx < value_numbers.len() {
                        let old_vn = value_numbers[dest_idx];
                        vn_log.push((dest_idx, old_vn));
                        value_numbers[dest_idx] = vn;
                    }
                    // Record in appropriate map with rollback
                    if is_load {
                        let key_for_log = expr_key.clone();
                        let old_val = load_expr_to_value.insert(expr_key, (dest, *load_generation));
                        load_rollback_log.push((key_for_log, old_val));
                    } else {
                        let key_for_log = expr_key.clone();
                        let old_val = expr_to_value.insert(expr_key, dest);
                        rollback_log.push((key_for_log, old_val));
                    }
                    new_instructions.push(inst);
                }
            }
            None => {
                // Not a numberable expression (store, call, alloca, etc.)
                if let Some(dest) = inst.dest() {
                    let vn = *next_vn;
                    *next_vn += 1;
                    let dest_idx = dest.0 as usize;
                    if dest_idx < value_numbers.len() {
                        let old_vn = value_numbers[dest_idx];
                        vn_log.push((dest_idx, old_vn));
                        value_numbers[dest_idx] = vn;
                    }
                }
                new_instructions.push(inst);
            }
        }
    }

    func.blocks[block_idx].instructions = new_instructions;
    func.blocks[block_idx].source_spans = new_spans;
    eliminated
}

/// Try to create an ExprKey for an instruction (for value numbering).
/// Returns the expression key and the destination value, or None if
/// the instruction is not eligible for value numbering.
fn make_expr_key(
    inst: &Instruction,
    value_numbers: &mut Vec<u32>,
    next_vn: &mut u32,
    vn_log: &mut Vec<(usize, u32)>,
) -> Option<(ExprKey, Value)> {
    match inst {
        Instruction::BinOp { dest, op, lhs, rhs, .. } => {
            let lhs_vn = operand_to_vn(lhs, value_numbers, next_vn, vn_log);
            let rhs_vn = operand_to_vn(rhs, value_numbers, next_vn, vn_log);

            // For commutative operations, canonicalize operand order
            let (lhs_vn, rhs_vn) = if op.is_commutative() {
                canonical_order(lhs_vn, rhs_vn)
            } else {
                (lhs_vn, rhs_vn)
            };

            Some((ExprKey::BinOp { op: *op, lhs: lhs_vn, rhs: rhs_vn }, *dest))
        }
        Instruction::UnaryOp { dest, op, src, .. } => {
            let src_vn = operand_to_vn(src, value_numbers, next_vn, vn_log);
            Some((ExprKey::UnaryOp { op: *op, src: src_vn }, *dest))
        }
        Instruction::Cmp { dest, op, lhs, rhs, .. } => {
            let lhs_vn = operand_to_vn(lhs, value_numbers, next_vn, vn_log);
            let rhs_vn = operand_to_vn(rhs, value_numbers, next_vn, vn_log);
            Some((ExprKey::Cmp { op: *op, lhs: lhs_vn, rhs: rhs_vn }, *dest))
        }
        Instruction::Cast { dest, src, from_ty, to_ty } => {
            // Don't CSE casts to/from 128-bit types (complex codegen)
            if from_ty.is_128bit() || to_ty.is_128bit() {
                return None;
            }
            let src_vn = operand_to_vn(src, value_numbers, next_vn, vn_log);
            Some((ExprKey::Cast { src: src_vn, from_ty: *from_ty, to_ty: *to_ty }, *dest))
        }
        Instruction::GetElementPtr { dest, base, offset, ty } => {
            let base_vn = operand_to_vn(&Operand::Value(*base), value_numbers, next_vn, vn_log);
            let offset_vn = operand_to_vn(offset, value_numbers, next_vn, vn_log);
            Some((ExprKey::Gep { base: base_vn, offset: offset_vn, ty: *ty }, *dest))
        }
        // Load CSE: two loads from the same pointer with the same type can be
        // CSE'd if no intervening memory modification occurred. The caller
        // (process_block) handles invalidating Load entries on memory clobbers.
        //
        // Excluded from CSE:
        // - Segment-overridden loads: access thread-local or CPU-local storage
        //   that may differ between accesses even without visible stores
        // - Float, long double, i128 types: use different register paths in
        //   codegen that complicate Copy instruction handling
        // - AtomicLoad: has ordering semantics (falls through to _ => None)
        Instruction::Load { dest, ptr, ty, seg_override } => {
            if *seg_override != AddressSpace::Default {
                return None;
            }
            if ty.is_float() || ty.is_long_double() || ty.is_128bit() {
                return None;
            }
            let ptr_vn = operand_to_vn(&Operand::Value(*ptr), value_numbers, next_vn, vn_log);
            Some((ExprKey::Load { ptr: ptr_vn, ty: *ty }, *dest))
        }
        // Other instructions (Store, Call, AtomicLoad, etc.) are not eligible.
        // AtomicLoad is excluded because it has memory ordering semantics that
        // require the load to actually execute.
        _ => None,
    }
}

/// Convert an Operand to a VNOperand for hashing.
/// If the value hasn't been assigned a value number yet (e.g. a function
/// parameter or an alloca whose definition appears later in the block),
/// assign it a fresh unique VN on the spot to avoid collisions between
/// different un-numbered values and already-assigned VNs.
fn operand_to_vn(
    op: &Operand,
    value_numbers: &mut Vec<u32>,
    next_vn: &mut u32,
    vn_log: &mut Vec<(usize, u32)>,
) -> VNOperand {
    match op {
        Operand::Const(c) => VNOperand::Const(c.to_hash_key()),
        Operand::Value(v) => {
            let idx = v.0 as usize;
            // Ensure the table is large enough
            if idx >= value_numbers.len() {
                value_numbers.resize(idx + 1, u32::MAX);
            }
            if value_numbers[idx] != u32::MAX {
                VNOperand::ValueNum(value_numbers[idx])
            } else {
                // Assign a fresh VN to this previously un-numbered value
                let vn = *next_vn;
                *next_vn += 1;
                let old_vn = value_numbers[idx];
                vn_log.push((idx, old_vn));
                value_numbers[idx] = vn;
                VNOperand::ValueNum(vn)
            }
        }
    }
}

/// Canonicalize operand order for commutative operations.
/// Ensures (a + b) and (b + a) hash to the same key.
fn canonical_order(lhs: VNOperand, rhs: VNOperand) -> (VNOperand, VNOperand) {
    if should_swap(&lhs, &rhs) {
        (rhs, lhs)
    } else {
        (lhs, rhs)
    }
}

fn should_swap(lhs: &VNOperand, rhs: &VNOperand) -> bool {
    match (lhs, rhs) {
        (VNOperand::ValueNum(_), VNOperand::Const(_)) => true,
        (VNOperand::ValueNum(a), VNOperand::ValueNum(b)) => a > b,
        (VNOperand::Const(a), VNOperand::Const(b)) => a > b,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_commutative_cse() {
        // Test that a + b and b + a are recognized as the same expression
        let mut block = BasicBlock {
            label: BlockId(0),
            instructions: vec![
                // %0 = add %a, %b
                Instruction::BinOp {
                    dest: Value(2),
                    op: IrBinOp::Add,
                    lhs: Operand::Value(Value(0)),
                    rhs: Operand::Value(Value(1)),
                    ty: IrType::I32,
                },
                // %1 = add %b, %a  (same expression, reversed operands)
                Instruction::BinOp {
                    dest: Value(3),
                    op: IrBinOp::Add,
                    lhs: Operand::Value(Value(1)),
                    rhs: Operand::Value(Value(0)),
                    ty: IrType::I32,
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(3)))),
            source_spans: Vec::new(),
        };

        let mut func = IrFunction {
            name: "test".to_string(),
            params: vec![],
            return_type: IrType::I32,
            blocks: vec![block],
            is_variadic: false,
            is_static: false,
            is_inline: false,
            is_always_inline: false,
            is_noinline: false,
            is_declaration: false,
            stack_size: 0,
            next_value_id: 4,
            section: None,
            visibility: None,
            is_weak: false,
            is_used: false,
            has_inlined_calls: false,
            param_alloca_values: Vec::new(),
        };

        let mut module = IrModule {
            functions: vec![func],
            globals: vec![],
            string_literals: vec![],
            wide_string_literals: vec![],
            constructors: vec![],
            destructors: vec![],
            aliases: vec![],
            toplevel_asm: vec![],
            symbol_attrs: vec![],
            char16_string_literals: vec![],
        };

        let eliminated = run(&mut module);
        assert_eq!(eliminated, 1);

        // Second instruction should be a Copy
        match &module.functions[0].blocks[0].instructions[1] {
            Instruction::Copy { dest, src: Operand::Value(v) } => {
                assert_eq!(dest.0, 3);
                assert_eq!(v.0, 2);
            }
            other => panic!("Expected Copy instruction, got {:?}", other),
        }
    }

    #[test]
    fn test_non_commutative_not_cse() {
        // Test that a - b and b - a are NOT treated as the same
        let mut func = IrFunction {
            name: "test".to_string(),
            params: vec![],
            return_type: IrType::I32,
            blocks: vec![BasicBlock {
                label: BlockId(0),
                instructions: vec![
                    Instruction::BinOp {
                        dest: Value(2),
                        op: IrBinOp::Sub,
                        lhs: Operand::Value(Value(0)),
                        rhs: Operand::Value(Value(1)),
                        ty: IrType::I32,
                    },
                    Instruction::BinOp {
                        dest: Value(3),
                        op: IrBinOp::Sub,
                        lhs: Operand::Value(Value(1)),
                        rhs: Operand::Value(Value(0)),
                        ty: IrType::I32,
                    },
                ],
                terminator: Terminator::Return(Some(Operand::Value(Value(3)))),
                source_spans: Vec::new(),
            }],
            is_variadic: false,
            is_static: false,
            is_inline: false,
            is_always_inline: false,
            is_noinline: false,
            is_declaration: false,
            stack_size: 0,
            next_value_id: 4,
            section: None,
            visibility: None,
            is_weak: false,
            is_used: false,
            has_inlined_calls: false,
            param_alloca_values: Vec::new(),
        };

        let mut module = IrModule {
            functions: vec![func],
            globals: vec![],
            string_literals: vec![],
            wide_string_literals: vec![],
            constructors: vec![],
            destructors: vec![],
            aliases: vec![],
            toplevel_asm: vec![],
            symbol_attrs: vec![],
            char16_string_literals: vec![],
        };

        let eliminated = run(&mut module);
        assert_eq!(eliminated, 0);
    }

    #[test]
    fn test_constant_cse() {
        // Two identical constant expressions should be CSE'd
        let mut func = IrFunction {
            name: "test".to_string(),
            params: vec![],
            return_type: IrType::I32,
            blocks: vec![BasicBlock {
                label: BlockId(0),
                instructions: vec![
                    Instruction::BinOp {
                        dest: Value(0),
                        op: IrBinOp::Add,
                        lhs: Operand::Const(IrConst::I32(3)),
                        rhs: Operand::Const(IrConst::I32(4)),
                        ty: IrType::I32,
                    },
                    Instruction::BinOp {
                        dest: Value(1),
                        op: IrBinOp::Add,
                        lhs: Operand::Const(IrConst::I32(3)),
                        rhs: Operand::Const(IrConst::I32(4)),
                        ty: IrType::I32,
                    },
                ],
                terminator: Terminator::Return(Some(Operand::Value(Value(1)))),
                source_spans: Vec::new(),
            }],
            is_variadic: false,
            is_static: false,
            is_inline: false,
            is_always_inline: false,
            is_noinline: false,
            is_declaration: false,
            stack_size: 0,
            next_value_id: 2,
            section: None,
            visibility: None,
            is_weak: false,
            is_used: false,
            has_inlined_calls: false,
            param_alloca_values: Vec::new(),
        };

        let mut module = IrModule {
            functions: vec![func],
            globals: vec![],
            string_literals: vec![],
            wide_string_literals: vec![],
            constructors: vec![],
            destructors: vec![],
            aliases: vec![],
            toplevel_asm: vec![],
            symbol_attrs: vec![],
            char16_string_literals: vec![],
        };

        let eliminated = run(&mut module);
        assert_eq!(eliminated, 1);
    }

    #[test]
    fn test_is_commutative() {
        assert!(IrBinOp::Add.is_commutative());
        assert!(IrBinOp::Mul.is_commutative());
        assert!(!IrBinOp::Sub.is_commutative());
        assert!(!IrBinOp::SDiv.is_commutative());
    }

    #[test]
    fn test_cast_cse() {
        // Two identical casts should be CSE'd
        let mut func = IrFunction {
            name: "test".to_string(),
            params: vec![],
            return_type: IrType::I64,
            blocks: vec![BasicBlock {
                label: BlockId(0),
                instructions: vec![
                    Instruction::Cast {
                        dest: Value(1),
                        src: Operand::Value(Value(0)),
                        from_ty: IrType::I32,
                        to_ty: IrType::I64,
                    },
                    Instruction::Cast {
                        dest: Value(2),
                        src: Operand::Value(Value(0)),
                        from_ty: IrType::I32,
                        to_ty: IrType::I64,
                    },
                ],
                terminator: Terminator::Return(Some(Operand::Value(Value(2)))),
                source_spans: Vec::new(),
            }],
            is_variadic: false,
            is_static: false,
            is_inline: false,
            is_always_inline: false,
            is_noinline: false,
            is_declaration: false,
            stack_size: 0,
            next_value_id: 3,
            section: None,
            visibility: None,
            is_weak: false,
            is_used: false,
            has_inlined_calls: false,
            param_alloca_values: Vec::new(),
        };

        let mut module = IrModule {
            functions: vec![func],
            globals: vec![],
            string_literals: vec![],
            wide_string_literals: vec![],
            constructors: vec![],
            destructors: vec![],
            aliases: vec![],
            toplevel_asm: vec![],
            symbol_attrs: vec![],
            char16_string_literals: vec![],
        };

        let eliminated = run(&mut module);
        assert_eq!(eliminated, 1);

        match &module.functions[0].blocks[0].instructions[1] {
            Instruction::Copy { dest, src: Operand::Value(v) } => {
                assert_eq!(dest.0, 2);
                assert_eq!(v.0, 1);
            }
            other => panic!("Expected Copy instruction, got {:?}", other),
        }
    }

    #[test]
    fn test_gep_cse() {
        // Two identical GEPs should be CSE'd
        let mut func = IrFunction {
            name: "test".to_string(),
            params: vec![],
            return_type: IrType::Ptr,
            blocks: vec![BasicBlock {
                label: BlockId(0),
                instructions: vec![
                    Instruction::GetElementPtr {
                        dest: Value(2),
                        base: Value(0),
                        offset: Operand::Value(Value(1)),
                        ty: IrType::Ptr,
                    },
                    Instruction::GetElementPtr {
                        dest: Value(3),
                        base: Value(0),
                        offset: Operand::Value(Value(1)),
                        ty: IrType::Ptr,
                    },
                ],
                terminator: Terminator::Return(Some(Operand::Value(Value(3)))),
                source_spans: Vec::new(),
            }],
            is_variadic: false,
            is_static: false,
            is_inline: false,
            is_always_inline: false,
            is_noinline: false,
            is_declaration: false,
            stack_size: 0,
            next_value_id: 4,
            section: None,
            visibility: None,
            is_weak: false,
            is_used: false,
            has_inlined_calls: false,
            param_alloca_values: Vec::new(),
        };

        let mut module = IrModule {
            functions: vec![func],
            globals: vec![],
            string_literals: vec![],
            wide_string_literals: vec![],
            constructors: vec![],
            destructors: vec![],
            aliases: vec![],
            toplevel_asm: vec![],
            symbol_attrs: vec![],
            char16_string_literals: vec![],
        };

        let eliminated = run(&mut module);
        assert_eq!(eliminated, 1);
    }

    #[test]
    fn test_cross_block_cse() {
        // Test that expressions in dominating blocks are visible to dominated blocks
        // CFG: block0 -> block1 (block0 dominates block1)
        let mut func = IrFunction {
            name: "test".to_string(),
            params: vec![],
            return_type: IrType::I32,
            blocks: vec![
                BasicBlock {
                    label: BlockId(0),
                    instructions: vec![
                        Instruction::BinOp {
                            dest: Value(2),
                            op: IrBinOp::Add,
                            lhs: Operand::Value(Value(0)),
                            rhs: Operand::Value(Value(1)),
                            ty: IrType::I32,
                        },
                    ],
                    terminator: Terminator::Branch(BlockId(1)),
                    source_spans: Vec::new(),
                },
                BasicBlock {
                    label: BlockId(1),
                    instructions: vec![
                        // Same expression as in block0 - should be CSE'd
                        Instruction::BinOp {
                            dest: Value(3),
                            op: IrBinOp::Add,
                            lhs: Operand::Value(Value(0)),
                            rhs: Operand::Value(Value(1)),
                            ty: IrType::I32,
                        },
                    ],
                    terminator: Terminator::Return(Some(Operand::Value(Value(3)))),
                    source_spans: Vec::new(),
                },
            ],
            is_variadic: false,
            is_static: false,
            is_inline: false,
            is_always_inline: false,
            is_noinline: false,
            is_declaration: false,
            stack_size: 0,
            next_value_id: 4,
            section: None,
            visibility: None,
            is_weak: false,
            is_used: false,
            has_inlined_calls: false,
            param_alloca_values: Vec::new(),
        };

        let mut module = IrModule {
            functions: vec![func],
            globals: vec![],
            string_literals: vec![],
            wide_string_literals: vec![],
            constructors: vec![],
            destructors: vec![],
            aliases: vec![],
            toplevel_asm: vec![],
            symbol_attrs: vec![],
            char16_string_literals: vec![],
        };

        let eliminated = run(&mut module);
        assert_eq!(eliminated, 1);

        // The expression in block1 should be replaced with a Copy
        match &module.functions[0].blocks[1].instructions[0] {
            Instruction::Copy { dest, src: Operand::Value(v) } => {
                assert_eq!(dest.0, 3);
                assert_eq!(v.0, 2);
            }
            other => panic!("Expected Copy instruction, got {:?}", other),
        }
    }

    #[test]
    fn test_diamond_no_cse_between_branches() {
        // Diamond CFG: block0 -> {block1, block2} -> block3
        // Expressions in block1 and block2 should NOT be CSE'd with each other,
        // since neither dominates the other.
        let mut func = IrFunction {
            name: "test".to_string(),
            params: vec![],
            return_type: IrType::I32,
            blocks: vec![
                // block0: entry, branches to block1 or block2
                BasicBlock {
                    label: BlockId(0),
                    instructions: vec![],
                    terminator: Terminator::CondBranch {
                        cond: Operand::Value(Value(0)),
                        true_label: BlockId(1),
                        false_label: BlockId(2),
                    },
                    source_spans: Vec::new(),
                },
                // block1: compute add (only reached via true branch)
                BasicBlock {
                    label: BlockId(1),
                    instructions: vec![
                        Instruction::BinOp {
                            dest: Value(2),
                            op: IrBinOp::Add,
                            lhs: Operand::Value(Value(0)),
                            rhs: Operand::Const(IrConst::I32(1)),
                            ty: IrType::I32,
                        },
                    ],
                    terminator: Terminator::Branch(BlockId(3)),
                    source_spans: Vec::new(),
                },
                // block2: compute same add (only reached via false branch)
                BasicBlock {
                    label: BlockId(2),
                    instructions: vec![
                        Instruction::BinOp {
                            dest: Value(3),
                            op: IrBinOp::Add,
                            lhs: Operand::Value(Value(0)),
                            rhs: Operand::Const(IrConst::I32(1)),
                            ty: IrType::I32,
                        },
                    ],
                    terminator: Terminator::Branch(BlockId(3)),
                    source_spans: Vec::new(),
                },
                // block3: merge
                BasicBlock {
                    label: BlockId(3),
                    instructions: vec![],
                    terminator: Terminator::Return(Some(Operand::Const(IrConst::I32(0)))),
                    source_spans: Vec::new(),
                },
            ],
            is_variadic: false,
            is_static: false,
            is_inline: false,
            is_always_inline: false,
            is_noinline: false,
            is_declaration: false,
            stack_size: 0,
            next_value_id: 4,
            section: None,
            visibility: None,
            is_weak: false,
            is_used: false,
            has_inlined_calls: false,
            param_alloca_values: Vec::new(),
        };

        let mut module = IrModule {
            functions: vec![func],
            globals: vec![],
            string_literals: vec![],
            wide_string_literals: vec![],
            constructors: vec![],
            destructors: vec![],
            aliases: vec![],
            toplevel_asm: vec![],
            symbol_attrs: vec![],
            char16_string_literals: vec![],
        };

        let eliminated = run(&mut module);
        // Neither branch dominates the other, so NO CSE should happen
        assert_eq!(eliminated, 0);

        // Both blocks should still have their original BinOp instructions
        assert!(matches!(
            &module.functions[0].blocks[1].instructions[0],
            Instruction::BinOp { op: IrBinOp::Add, .. }
        ));
        assert!(matches!(
            &module.functions[0].blocks[2].instructions[0],
            Instruction::BinOp { op: IrBinOp::Add, .. }
        ));
    }

    /// Helper to create a minimal IrFunction with given blocks.
    fn make_func(blocks: Vec<BasicBlock>, next_value_id: u32) -> IrFunction {
        IrFunction {
            name: "test".to_string(),
            params: vec![],
            return_type: IrType::I32,
            blocks,
            is_variadic: false,
            is_static: false,
            is_inline: false,
            is_always_inline: false,
            is_noinline: false,
            is_declaration: false,
            stack_size: 0,
            next_value_id,
            section: None,
            visibility: None,
            is_weak: false,
            is_used: false,
            has_inlined_calls: false,
            param_alloca_values: Vec::new(),
        }
    }

    fn make_module(func: IrFunction) -> IrModule {
        IrModule {
            functions: vec![func],
            globals: vec![],
            string_literals: vec![],
            wide_string_literals: vec![],
            constructors: vec![],
            destructors: vec![],
            aliases: vec![],
            toplevel_asm: vec![],
            symbol_attrs: vec![],
            char16_string_literals: vec![],
        }
    }

    #[test]
    fn test_load_cse_same_block() {
        // Two loads from the same pointer in the same block should be CSE'd
        let func = make_func(vec![BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Load {
                    dest: Value(1),
                    ptr: Value(0),
                    ty: IrType::I32,
                    seg_override: AddressSpace::Default,
                },
                Instruction::Load {
                    dest: Value(2),
                    ptr: Value(0),
                    ty: IrType::I32,
                    seg_override: AddressSpace::Default,
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(2)))),
            source_spans: Vec::new(),
        }], 3);

        let mut module = make_module(func);
        let eliminated = run(&mut module);
        assert_eq!(eliminated, 1);

        // Second load should be replaced with Copy
        match &module.functions[0].blocks[0].instructions[1] {
            Instruction::Copy { dest, src: Operand::Value(v) } => {
                assert_eq!(dest.0, 2);
                assert_eq!(v.0, 1);
            }
            other => panic!("Expected Copy, got {:?}", other),
        }
    }

    #[test]
    fn test_load_cse_invalidated_by_store() {
        // A store between two loads should prevent CSE
        let func = make_func(vec![BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Load {
                    dest: Value(1),
                    ptr: Value(0),
                    ty: IrType::I32,
                    seg_override: AddressSpace::Default,
                },
                Instruction::Store {
                    val: Operand::Const(IrConst::I32(42)),
                    ptr: Value(0),
                    ty: IrType::I32,
                    seg_override: AddressSpace::Default,
                },
                Instruction::Load {
                    dest: Value(2),
                    ptr: Value(0),
                    ty: IrType::I32,
                    seg_override: AddressSpace::Default,
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(2)))),
            source_spans: Vec::new(),
        }], 3);

        let mut module = make_module(func);
        let eliminated = run(&mut module);
        assert_eq!(eliminated, 0); // No CSE: store invalidates the load
    }

    #[test]
    fn test_load_cse_invalidated_by_call() {
        // A call between two loads should prevent CSE
        let func = make_func(vec![BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Load {
                    dest: Value(1),
                    ptr: Value(0),
                    ty: IrType::I32,
                    seg_override: AddressSpace::Default,
                },
                Instruction::Call {
                    dest: Some(Value(2)),
                    func: "side_effect".to_string(),
                    args: vec![],
                    arg_types: vec![],
                    return_type: IrType::Void,
                    is_variadic: false,
                    num_fixed_args: 0,
                    struct_arg_sizes: vec![],
                    struct_arg_classes: vec![],
                },
                Instruction::Load {
                    dest: Value(3),
                    ptr: Value(0),
                    ty: IrType::I32,
                    seg_override: AddressSpace::Default,
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(3)))),
            source_spans: Vec::new(),
        }], 4);

        let mut module = make_module(func);
        let eliminated = run(&mut module);
        assert_eq!(eliminated, 0); // No CSE: call may modify memory
    }

    #[test]
    fn test_load_cse_across_dominating_block() {
        // Load in block0 should CSE with load in block1 (block0 dominates block1,
        // single predecessor, no memory clobber)
        let func = make_func(vec![
            BasicBlock {
                label: BlockId(0),
                instructions: vec![
                    Instruction::Load {
                        dest: Value(1),
                        ptr: Value(0),
                        ty: IrType::I32,
                        seg_override: AddressSpace::Default,
                    },
                ],
                terminator: Terminator::Branch(BlockId(1)),
                source_spans: Vec::new(),
            },
            BasicBlock {
                label: BlockId(1),
                instructions: vec![
                    Instruction::Load {
                        dest: Value(2),
                        ptr: Value(0),
                        ty: IrType::I32,
                        seg_override: AddressSpace::Default,
                    },
                ],
                terminator: Terminator::Return(Some(Operand::Value(Value(2)))),
                source_spans: Vec::new(),
            },
        ], 3);

        let mut module = make_module(func);
        let eliminated = run(&mut module);
        assert_eq!(eliminated, 1);

        match &module.functions[0].blocks[1].instructions[0] {
            Instruction::Copy { dest, src: Operand::Value(v) } => {
                assert_eq!(dest.0, 2);
                assert_eq!(v.0, 1);
            }
            other => panic!("Expected Copy, got {:?}", other),
        }
    }

    #[test]
    fn test_load_cse_invalidated_at_merge_point() {
        // Diamond CFG: block0 -> {block1, block2} -> block3
        // block1 stores to memory, so Load CSE should be invalidated at block3
        let func = make_func(vec![
            // block0: entry, load and branch
            BasicBlock {
                label: BlockId(0),
                instructions: vec![
                    Instruction::Load {
                        dest: Value(2),
                        ptr: Value(0),
                        ty: IrType::I32,
                        seg_override: AddressSpace::Default,
                    },
                ],
                terminator: Terminator::CondBranch {
                    cond: Operand::Value(Value(1)),
                    true_label: BlockId(1),
                    false_label: BlockId(2),
                },
                source_spans: Vec::new(),
            },
            // block1: stores to memory
            BasicBlock {
                label: BlockId(1),
                instructions: vec![
                    Instruction::Store {
                        val: Operand::Const(IrConst::I32(42)),
                        ptr: Value(0),
                        ty: IrType::I32,
                        seg_override: AddressSpace::Default,
                    },
                ],
                terminator: Terminator::Branch(BlockId(3)),
                source_spans: Vec::new(),
            },
            // block2: no memory modification
            BasicBlock {
                label: BlockId(2),
                instructions: vec![],
                terminator: Terminator::Branch(BlockId(3)),
                source_spans: Vec::new(),
            },
            // block3: merge point - loads from same pointer
            BasicBlock {
                label: BlockId(3),
                instructions: vec![
                    Instruction::Load {
                        dest: Value(3),
                        ptr: Value(0),
                        ty: IrType::I32,
                        seg_override: AddressSpace::Default,
                    },
                ],
                terminator: Terminator::Return(Some(Operand::Value(Value(3)))),
                source_spans: Vec::new(),
            },
        ], 4);

        let mut module = make_module(func);
        let eliminated = run(&mut module);
        // Load in block3 should NOT be CSE'd because block3 is a merge point
        // and block1 (a predecessor) stores to memory.
        assert_eq!(eliminated, 0);

        // block3's load should remain as-is
        assert!(matches!(
            &module.functions[0].blocks[3].instructions[0],
            Instruction::Load { .. }
        ));
    }
}
