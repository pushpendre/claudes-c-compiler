//! Copy propagation optimization pass.
//!
//! This pass eliminates redundant Copy instructions by replacing uses of a
//! Copy's destination with the Copy's source operand. This is particularly
//! important because:
//! - Phi elimination generates many Copy instructions
//! - Mem2reg creates Copy instructions when replacing loads
//! - Other optimization passes (simplify, GVN) create Copy instructions
//!
//! Without copy propagation, each Copy becomes a load-to-accumulator then
//! store-to-new-slot in codegen, wasting both instructions and stack space.
//!
//! After this pass runs, the dead Copy instructions are cleaned up by DCE.

use crate::common::fx_hash::FxHashMap;
use crate::ir::ir::*;

/// Run copy propagation on the entire module.
/// Returns the number of operand replacements made.
pub fn run(module: &mut IrModule) -> usize {
    module.for_each_function(propagate_copies)
}

/// Propagate copies within a single function.
fn propagate_copies(func: &mut IrFunction) -> usize {
    // Phase 1: Build the copy map (dest -> resolved source)
    let copy_map = build_copy_map(func);

    if copy_map.is_empty() {
        return 0;
    }

    // Phase 2: Replace all uses of copied values
    let mut replacements = 0;

    for block in &mut func.blocks {
        for inst in &mut block.instructions {
            replacements += replace_operands_in_instruction(inst, &copy_map);
        }
        replacements += replace_operands_in_terminator(&mut block.terminator, &copy_map);
    }

    replacements
}

/// Build a map from Copy destinations to their ultimate sources.
/// Follows chains: if %a = Copy %b and %b = Copy %c, resolves %a -> %c.
fn build_copy_map(func: &IrFunction) -> FxHashMap<u32, Operand> {
    // First pass: collect direct copy relationships
    let mut direct_copies: FxHashMap<u32, Operand> = FxHashMap::default();

    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::Copy { dest, src } = inst {
                direct_copies.insert(dest.0, src.clone());
            }
        }
    }

    if direct_copies.is_empty() {
        return direct_copies;
    }

    // Second pass: resolve chains with cycle detection
    let mut resolved: FxHashMap<u32, Operand> = FxHashMap::default();

    for &start_dest in direct_copies.keys() {
        let resolved_src = resolve_chain(start_dest, &direct_copies);
        resolved.insert(start_dest, resolved_src);
    }

    resolved
}

/// Follow a chain of copies to find the ultimate source.
/// Handles chains like: %a = Copy %b, %b = Copy %c => %a resolves to %c's source.
/// Limits depth to prevent infinite loops from cycles.
fn resolve_chain(start: u32, copies: &FxHashMap<u32, Operand>) -> Operand {
    let mut current = start;
    let mut depth = 0;
    const MAX_DEPTH: usize = 64;

    loop {
        if depth >= MAX_DEPTH {
            // Safety limit: return what we have
            return Operand::Value(Value(current));
        }

        match copies.get(&current) {
            Some(Operand::Value(v)) => {
                if v.0 == current {
                    // Self-copy, stop here
                    return Operand::Value(Value(current));
                }
                // Follow the chain
                current = v.0;
                depth += 1;
            }
            Some(Operand::Const(c)) => {
                // Chain ends at a constant
                return Operand::Const(c.clone());
            }
            None => {
                // Chain ends at a non-copy value
                return Operand::Value(Value(current));
            }
        }
    }
}

/// Replace operands in an instruction that reference copied values.
/// Returns the number of replacements made.
fn replace_operands_in_instruction(inst: &mut Instruction, copy_map: &FxHashMap<u32, Operand>) -> usize {
    let mut count = 0;

    match inst {
        Instruction::Alloca { .. } => {}
        Instruction::DynAlloca { size, .. } => {
            count += replace_operand(size, copy_map);
        }
        Instruction::Store { val, ptr, .. } => {
            count += replace_operand(val, copy_map);
            count += replace_value_in_place(ptr, copy_map);
        }
        Instruction::Load { ptr, .. } => {
            count += replace_value_in_place(ptr, copy_map);
        }
        Instruction::BinOp { lhs, rhs, .. } => {
            count += replace_operand(lhs, copy_map);
            count += replace_operand(rhs, copy_map);
        }
        Instruction::UnaryOp { src, .. } => {
            count += replace_operand(src, copy_map);
        }
        Instruction::Cmp { lhs, rhs, .. } => {
            count += replace_operand(lhs, copy_map);
            count += replace_operand(rhs, copy_map);
        }
        Instruction::Call { args, .. } => {
            for arg in args.iter_mut() {
                count += replace_operand(arg, copy_map);
            }
        }
        Instruction::CallIndirect { func_ptr, args, .. } => {
            count += replace_operand(func_ptr, copy_map);
            for arg in args.iter_mut() {
                count += replace_operand(arg, copy_map);
            }
        }
        Instruction::GetElementPtr { base, offset, .. } => {
            count += replace_value_in_place(base, copy_map);
            count += replace_operand(offset, copy_map);
        }
        Instruction::Cast { src, .. } => {
            count += replace_operand(src, copy_map);
        }
        Instruction::Copy { src, .. } => {
            count += replace_operand(src, copy_map);
        }
        Instruction::GlobalAddr { .. } => {}
        Instruction::Memcpy { dest, src, .. } => {
            count += replace_value_in_place(dest, copy_map);
            count += replace_value_in_place(src, copy_map);
        }
        Instruction::VaArg { va_list_ptr, .. } => {
            count += replace_value_in_place(va_list_ptr, copy_map);
        }
        Instruction::VaStart { va_list_ptr } => {
            count += replace_value_in_place(va_list_ptr, copy_map);
        }
        Instruction::VaEnd { va_list_ptr } => {
            count += replace_value_in_place(va_list_ptr, copy_map);
        }
        Instruction::VaCopy { dest_ptr, src_ptr } => {
            count += replace_value_in_place(dest_ptr, copy_map);
            count += replace_value_in_place(src_ptr, copy_map);
        }
        Instruction::AtomicRmw { ptr, val, .. } => {
            count += replace_operand(ptr, copy_map);
            count += replace_operand(val, copy_map);
        }
        Instruction::AtomicCmpxchg { ptr, expected, desired, .. } => {
            count += replace_operand(ptr, copy_map);
            count += replace_operand(expected, copy_map);
            count += replace_operand(desired, copy_map);
        }
        Instruction::AtomicLoad { ptr, .. } => {
            count += replace_operand(ptr, copy_map);
        }
        Instruction::AtomicStore { ptr, val, .. } => {
            count += replace_operand(ptr, copy_map);
            count += replace_operand(val, copy_map);
        }
        Instruction::Fence { .. } => {}
        Instruction::Phi { incoming, .. } => {
            for (op, _label) in incoming.iter_mut() {
                count += replace_operand(op, copy_map);
            }
        }
        Instruction::LabelAddr { .. } => {}
        Instruction::GetReturnF64Second { .. } => {}
        Instruction::GetReturnF32Second { .. } => {}
        Instruction::SetReturnF64Second { src } => {
            count += replace_operand(src, copy_map);
        }
        Instruction::SetReturnF32Second { src } => {
            count += replace_operand(src, copy_map);
        }
        Instruction::InlineAsm { inputs, .. } => {
            for (_constraint, op, _name) in inputs.iter_mut() {
                count += replace_operand(op, copy_map);
            }
        }
        Instruction::Intrinsic { args, .. } => {
            for arg in args.iter_mut() {
                count += replace_operand(arg, copy_map);
            }
        }
    }

    count
}

/// Replace operands in a terminator.
fn replace_operands_in_terminator(term: &mut Terminator, copy_map: &FxHashMap<u32, Operand>) -> usize {
    let mut count = 0;
    match term {
        Terminator::Return(Some(val)) => {
            count += replace_operand(val, copy_map);
        }
        Terminator::Return(None) => {}
        Terminator::Branch(_) => {}
        Terminator::CondBranch { cond, .. } => {
            count += replace_operand(cond, copy_map);
        }
        Terminator::IndirectBranch { target, .. } => {
            count += replace_operand(target, copy_map);
        }
        Terminator::Unreachable => {}
    }
    count
}

/// Replace an Operand if it references a copied value.
/// Returns 1 if a replacement was made, 0 otherwise.
fn replace_operand(op: &mut Operand, copy_map: &FxHashMap<u32, Operand>) -> usize {
    if let Operand::Value(v) = op {
        if let Some(replacement) = copy_map.get(&v.0) {
            *op = replacement.clone();
            return 1;
        }
    }
    0
}

/// Replace a Value in-place if it references a copied value.
/// Only replaces if the resolved source is also a Value (not a Const).
/// Returns 1 if a replacement was made, 0 otherwise.
fn replace_value_in_place(val: &mut Value, copy_map: &FxHashMap<u32, Operand>) -> usize {
    if let Some(replacement) = copy_map.get(&val.0) {
        if let Operand::Value(new_val) = replacement {
            *val = *new_val;
            return 1;
        }
        // Can't replace a Value field with a Const - skip
    }
    0
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::types::IrType;

    #[test]
    fn test_simple_copy_propagation() {
        // %1 = Copy %0
        // %2 = Add %1, const(1)
        // Should become:
        // %1 = Copy %0 (dead, will be removed by DCE)
        // %2 = Add %0, const(1)
        let mut func = IrFunction::new("test".to_string(), IrType::I32, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Copy {
                    dest: Value(1),
                    src: Operand::Value(Value(0)),
                },
                Instruction::BinOp {
                    dest: Value(2),
                    op: IrBinOp::Add,
                    lhs: Operand::Value(Value(1)),
                    rhs: Operand::Const(IrConst::I32(1)),
                    ty: IrType::I32,
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(2)))),
        });

        let replacements = propagate_copies(&mut func);
        assert!(replacements > 0);

        // The BinOp should now reference %0 directly
        match &func.blocks[0].instructions[1] {
            Instruction::BinOp { lhs: Operand::Value(v), .. } => {
                assert_eq!(v.0, 0, "Should reference original value %0");
            }
            other => panic!("Expected BinOp, got {:?}", other),
        }
    }

    #[test]
    fn test_chain_copy_propagation() {
        // %1 = Copy %0
        // %2 = Copy %1
        // %3 = Add %2, const(1)
        // Should resolve %2 -> %0
        let mut func = IrFunction::new("test".to_string(), IrType::I32, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Copy {
                    dest: Value(1),
                    src: Operand::Value(Value(0)),
                },
                Instruction::Copy {
                    dest: Value(2),
                    src: Operand::Value(Value(1)),
                },
                Instruction::BinOp {
                    dest: Value(3),
                    op: IrBinOp::Add,
                    lhs: Operand::Value(Value(2)),
                    rhs: Operand::Const(IrConst::I32(1)),
                    ty: IrType::I32,
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(3)))),
        });

        let replacements = propagate_copies(&mut func);
        assert!(replacements > 0);

        // The BinOp should now reference %0 directly
        match &func.blocks[0].instructions[2] {
            Instruction::BinOp { lhs: Operand::Value(v), .. } => {
                assert_eq!(v.0, 0, "Should resolve chain to original value %0");
            }
            other => panic!("Expected BinOp, got {:?}", other),
        }
    }

    #[test]
    fn test_const_copy_propagation() {
        // %0 = Copy const(42)
        // %1 = Add %0, const(1)
        // Should propagate const(42) into the Add
        let mut func = IrFunction::new("test".to_string(), IrType::I32, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Copy {
                    dest: Value(0),
                    src: Operand::Const(IrConst::I32(42)),
                },
                Instruction::BinOp {
                    dest: Value(1),
                    op: IrBinOp::Add,
                    lhs: Operand::Value(Value(0)),
                    rhs: Operand::Const(IrConst::I32(1)),
                    ty: IrType::I32,
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(1)))),
        });

        let replacements = propagate_copies(&mut func);
        assert!(replacements > 0);

        // The BinOp should now have const(42) as lhs
        match &func.blocks[0].instructions[1] {
            Instruction::BinOp { lhs: Operand::Const(IrConst::I32(42)), .. } => {}
            other => panic!("Expected BinOp with const 42, got {:?}", other),
        }
    }

    #[test]
    fn test_terminator_propagation() {
        // %1 = Copy %0
        // return %1
        // Should become return %0
        let mut func = IrFunction::new("test".to_string(), IrType::I32, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Copy {
                    dest: Value(1),
                    src: Operand::Value(Value(0)),
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(1)))),
        });

        let replacements = propagate_copies(&mut func);
        assert!(replacements > 0);

        match &func.blocks[0].terminator {
            Terminator::Return(Some(Operand::Value(v))) => {
                assert_eq!(v.0, 0, "Return should reference %0 directly");
            }
            other => panic!("Expected Return with %0, got {:?}", other),
        }
    }

    #[test]
    fn test_no_propagation_when_no_copies() {
        let mut func = IrFunction::new("test".to_string(), IrType::I32, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::BinOp {
                    dest: Value(0),
                    op: IrBinOp::Add,
                    lhs: Operand::Const(IrConst::I32(1)),
                    rhs: Operand::Const(IrConst::I32(2)),
                    ty: IrType::I32,
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(0)))),
        });

        let replacements = propagate_copies(&mut func);
        assert_eq!(replacements, 0);
    }
}
