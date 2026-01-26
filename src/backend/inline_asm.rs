//! Shared inline assembly framework.
//!
//! All three backends use the same 4-phase inline assembly processing:
//! 1. Classify constraints and assign registers (specific first, then scratch)
//! 2. Load input values into registers, pre-load read-write outputs
//! 3. Substitute operand references in template and emit
//! 4. Store output registers back to stack slots
//!
//! Each backend implements `InlineAsmEmitter` to provide arch-specific register
//! classification, loading, and storage. The shared `emit_inline_asm_common`
//! orchestrates the phases.

use crate::ir::ir::{BlockId, Operand, Value};
use crate::common::types::{AddressSpace, IrType};
use super::state::CodegenState;

/// Operand classification for inline asm. Each backend classifies its constraints
/// into these categories so the shared framework can orchestrate register
/// assignment, tied operand resolution, and GCC numbering.
#[derive(Debug, Clone, PartialEq)]
pub enum AsmOperandKind {
    /// General-purpose register (e.g., x86 "r", ARM "r", RISC-V "r").
    GpReg,
    /// Floating-point register (RISC-V "f").
    FpReg,
    /// Memory operand (all arches "m").
    Memory,
    /// Specific named register (x86 "a"→"rax", RISC-V "a0", etc.).
    Specific(String),
    /// Tied to another operand by index (e.g., "0", "1").
    Tied(usize),
    /// Immediate value (RISC-V "I", "i", "n").
    Immediate,
    /// Address for atomic ops (RISC-V "A").
    Address,
    /// Zero-or-register (RISC-V "rJ", "J").
    ZeroOrReg,
    /// Condition code output (GCC =@cc<cond>, e.g. =@cce, =@ccne).
    /// The string is the condition suffix (e.g. "e", "ne", "s", "ns").
    ConditionCode(String),
    /// x87 FPU stack top register st(0), selected by "t" constraint.
    X87St0,
    /// x87 FPU stack second register st(1), selected by "u" constraint.
    X87St1,
}

/// Per-operand state tracked by the shared inline asm framework.
/// Backends populate arch-specific fields (mem_addr, mem_offset, imm_value)
/// during constraint classification.
#[derive(Debug, Clone)]
pub struct AsmOperand {
    pub kind: AsmOperandKind,
    pub reg: String,
    pub name: Option<String>,
    /// x86: memory address string like "offset(%rbp)".
    pub mem_addr: String,
    /// RISC-V/ARM: stack offset for memory/address operands.
    pub mem_offset: i64,
    /// Immediate value for "I"/"i" constraints.
    pub imm_value: Option<i64>,
    /// Symbol name for "i" constraint operands that reference global/function addresses.
    /// Used by %P and %a modifiers to emit raw symbol names in inline asm templates.
    pub imm_symbol: Option<String>,
    /// IR type of this operand, used for correctly-sized loads/stores.
    pub operand_type: IrType,
    /// Original constraint string, used for fallback decisions.
    pub constraint: String,
    /// Segment prefix for memory operands (e.g., "%gs:" or "%fs:").
    /// Set from AddressSpace for __seg_gs/__seg_fs pointer dereferences.
    pub seg_prefix: String,
}

impl AsmOperand {
    pub fn new(kind: AsmOperandKind, name: Option<String>) -> Self {
        Self { kind, reg: String::new(), name, mem_addr: String::new(), mem_offset: 0, imm_value: None, imm_symbol: None, operand_type: IrType::I64, constraint: String::new(), seg_prefix: String::new() }
    }

    /// Copy register assignment and addressing metadata from another operand.
    /// Used for tied operands and "+" read-write propagation.
    pub fn copy_metadata_from(&mut self, source: &AsmOperand) {
        self.reg = source.reg.clone();
        self.mem_addr = source.mem_addr.clone();
        self.mem_offset = source.mem_offset;
        if matches!(source.kind, AsmOperandKind::Memory) {
            self.kind = AsmOperandKind::Memory;
        } else if matches!(source.kind, AsmOperandKind::Address) {
            self.kind = AsmOperandKind::Address;
        } else if matches!(source.kind, AsmOperandKind::FpReg) {
            self.kind = AsmOperandKind::FpReg;
        } else if matches!(source.kind, AsmOperandKind::X87St0) {
            self.kind = AsmOperandKind::X87St0;
        } else if matches!(source.kind, AsmOperandKind::X87St1) {
            self.kind = AsmOperandKind::X87St1;
        }
    }
}

/// Trait that backends implement to provide architecture-specific inline asm behavior.
/// The shared `emit_inline_asm_common` function calls these methods to handle the
/// architecture-dependent parts of inline assembly processing.
pub trait InlineAsmEmitter {
    /// Mutable access to the codegen state (for emitting instructions).
    fn asm_state(&mut self) -> &mut CodegenState;
    /// Immutable access to the codegen state.
    fn asm_state_ref(&self) -> &CodegenState;

    /// Classify a constraint string into an AsmOperandKind, and optionally
    /// return the specific register name for Specific constraints.
    fn classify_constraint(&self, constraint: &str) -> AsmOperandKind;

    /// Set up arch-specific operand metadata after classification.
    /// Called once per operand. For memory/address operands, set mem_addr or mem_offset.
    /// For immediate operands, set imm_value.
    fn setup_operand_metadata(&self, op: &mut AsmOperand, val: &Operand, is_output: bool);

    /// Assign the next available scratch register for the given operand kind.
    /// `excluded` contains register names that are already claimed by specific-register
    /// constraints (e.g., "rcx" from "c" constraint) and must not be reused.
    fn assign_scratch_reg(&mut self, kind: &AsmOperandKind, excluded: &[String]) -> String;

    /// Load an input value into its assigned register. Called during Phase 2.
    fn load_input_to_reg(&mut self, op: &AsmOperand, val: &Operand, constraint: &str);

    /// Pre-load a read-write ("+") output's current value into its register.
    fn preload_readwrite_output(&mut self, op: &AsmOperand, ptr: &Value);

    /// Substitute operand references in a single template line and return the result.
    fn substitute_template_line(&self, line: &str, operands: &[AsmOperand], gcc_to_internal: &[usize], operand_types: &[IrType], goto_labels: &[(String, BlockId)]) -> String;

    /// Store an output register value back to its stack slot after the asm executes.
    fn store_output_from_reg(&mut self, op: &AsmOperand, ptr: &Value, constraint: &str);

    /// Resolve memory operand addresses that require indirection (non-alloca pointers).
    /// `excluded` contains registers claimed by specific-register constraints,
    /// used to avoid conflicts when allocating a temp register for the address.
    fn resolve_memory_operand(&mut self, _op: &mut AsmOperand, _val: &Operand, _excluded: &[String]) -> bool {
        false
    }

    /// Reset scratch register allocation state (called at start of each inline asm).
    fn reset_scratch_state(&mut self);
}

/// Check whether a constraint string contains an immediate alternative character.
/// Used by both IR lowering (to decide whether to try constant evaluation) and
/// the shared inline asm framework (to promote GpReg operands to Immediate when
/// the input is a compile-time constant).
///
/// This covers the architecture-neutral immediate constraint letters ('I', 'i', 'n').
/// Architecture-specific immediate letters (e.g., x86 'N', 'e', 'K') are handled
/// separately by each backend's `classify_constraint`.
pub fn constraint_has_immediate_alt(constraint: &str) -> bool {
    let stripped = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
    // Named tied operands ("[name]") don't have immediates
    if stripped.starts_with('[') && stripped.ends_with(']') {
        return false;
    }
    stripped.chars().any(|c| matches!(c, 'I' | 'i' | 'n'))
}

/// Check whether a constraint is purely immediate (only "i", "I", "n", or similar
/// immediate letters, with no register or memory alternatives). When such a constraint
/// can't be resolved at compile time, we must still emit a dummy immediate to produce
/// valid assembly (e.g., `testb $0, mem` instead of `testb %edx, mem`).
/// This occurs in static inline functions whose "i" constraint expressions depend on
/// parameters — the standalone function body can't evaluate them, but the function
/// is only ever called with constant arguments at call sites.
pub fn constraint_is_immediate_only(constraint: &str) -> bool {
    let stripped = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
    if stripped.is_empty() {
        return false;
    }
    if stripped.starts_with('[') && stripped.ends_with(']') {
        return false;
    }
    // Must have at least one immediate letter
    let has_imm = stripped.chars().any(|c| matches!(c,
        'i' | 'I' | 'n' | 'N' | 'e' | 'E' | 'K' | 'M' | 'G' | 'H' | 'J' | 'L' | 'O'
    ));
    if !has_imm {
        return false;
    }
    // Must NOT have any register or memory alternative
    let has_reg_or_mem = stripped.chars().any(|c| matches!(c,
        'r' | 'q' | 'R' | 'l' |           // GP register
        'g' |                              // general (reg + mem + imm)
        'x' | 'v' | 'Y' |                 // FP register
        'a' | 'b' | 'c' | 'd' | 'S' | 'D' | // specific register
        'm' | 'o' | 'V' | 'p' | 'Q'       // memory (Q = AArch64 base-register memory)
    ));
    !has_reg_or_mem && !stripped.chars().any(|c| c.is_ascii_digit())
}

/// Check whether a constraint string contains a memory alternative character.
/// Handles both single-character ("m") and multi-character constraints ("rm", "mq").
/// Also recognizes "Q" which is an AArch64-specific memory constraint meaning
/// "a memory address with a single base register" (used for atomic ops like ldaxr/stlxr).
pub fn constraint_has_memory_alt(constraint: &str) -> bool {
    let stripped = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
    // Named tied operands ("[name]") are not memory constraints
    if stripped.starts_with('[') && stripped.ends_with(']') {
        return false;
    }
    stripped.chars().any(|c| c == 'm' || c == 'Q')
}

/// Check whether a constraint is memory-only (has memory alternative but no register
/// alternative). For constraints like "rm", "qm", "g" that allow both register and
/// memory, returns false — the backend will prefer registers, so the IR lowering
/// should provide a value (not an address). Only pure "m"/"o"/"V"/"Q" constraints need
/// the address for memory operand formatting.
/// Note: "Q" is AArch64-specific meaning "single base register memory address" and is
/// always memory-only (no register alternative), used for atomic ops like ldaxr/stlxr.
pub fn constraint_is_memory_only(constraint: &str) -> bool {
    let stripped = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
    // Named tied operands ("[name]") are never memory-only
    if stripped.starts_with('[') && stripped.ends_with(']') {
        return false;
    }
    // "Q" is an AArch64 memory-only constraint (single base register addressing)
    let has_mem = stripped.chars().any(|c| matches!(c, 'm' | 'o' | 'V' | 'p' | 'Q'));
    if !has_mem {
        return false;
    }
    // Check for any register alternative (GP, FP, or specific register)
    // Note: 'Q' is NOT listed as a register here — on AArch64 it's memory, and on x86
    // it's rarely used (legacy "a,b,c,d" registers). The backend classify_constraint
    // handles x86 Q correctly regardless.
    let has_reg = stripped.chars().any(|c| matches!(c,
        'r' | 'q' | 'R' | 'l' |           // GP register
        'g' |                              // general (reg + mem + imm)
        'x' | 'v' | 'Y' |                 // FP register
        'a' | 'b' | 'c' | 'd' | 'S' | 'D' // specific register
    ));
    // Also check for tied operand (digits) — those get a register
    let has_tied = stripped.chars().any(|c| c.is_ascii_digit());
    !has_reg && !has_tied
}

/// Shared inline assembly emission logic. All three backends call this from their
/// `emit_inline_asm` implementation, providing an `InlineAsmEmitter` to handle
/// arch-specific details.
pub fn emit_inline_asm_common(
    emitter: &mut dyn InlineAsmEmitter,
    template: &str,
    outputs: &[(String, Value, Option<String>)],
    inputs: &[(String, Operand, Option<String>)],
    clobbers: &[String],
    operand_types: &[IrType],
    goto_labels: &[(String, BlockId)],
    input_symbols: &[Option<String>],
) {
    emit_inline_asm_common_impl(emitter, template, outputs, inputs, clobbers, operand_types, goto_labels, input_symbols, &[]);
}

pub fn emit_inline_asm_common_impl(
    emitter: &mut dyn InlineAsmEmitter,
    template: &str,
    outputs: &[(String, Value, Option<String>)],
    inputs: &[(String, Operand, Option<String>)],
    clobbers: &[String],
    operand_types: &[IrType],
    goto_labels: &[(String, BlockId)],
    input_symbols: &[Option<String>],
    seg_overrides: &[AddressSpace],
) {
    emitter.reset_scratch_state();
    let total_operands = outputs.len() + inputs.len();

    // Phase 1: Classify all operands and assign registers
    let mut operands: Vec<AsmOperand> = Vec::with_capacity(total_operands);

    // Classify outputs
    for (constraint, ptr, name) in outputs {
        let kind = emitter.classify_constraint(constraint);
        let mut op = AsmOperand::new(kind, name.clone());
        op.constraint = constraint.clone();
        if let AsmOperandKind::Specific(ref reg) = op.kind {
            op.reg = reg.clone();
        }
        let val = Operand::Value(*ptr);
        emitter.setup_operand_metadata(&mut op, &val, true);
        operands.push(op);
    }

    // Track which inputs are tied (to avoid assigning scratch regs)
    let mut input_tied_to: Vec<Option<usize>> = Vec::with_capacity(inputs.len());

    // Classify inputs
    for (constraint, val, name) in inputs {
        // Handle named tied operands: "[name]" resolves to the output with that name
        let kind = if constraint.starts_with('[') && constraint.ends_with(']') {
            let tied_name = &constraint[1..constraint.len()-1];
            let tied_idx = outputs.iter().position(|(_, _, oname)| {
                oname.as_deref() == Some(tied_name)
            });
            if let Some(idx) = tied_idx {
                AsmOperandKind::Tied(idx)
            } else {
                emitter.classify_constraint(constraint)
            }
        } else {
            emitter.classify_constraint(constraint)
        };
        let mut op = AsmOperand::new(kind.clone(), name.clone());
        op.constraint = constraint.clone();
        if let AsmOperandKind::Specific(ref reg) = op.kind {
            op.reg = reg.clone();
        }
        if let AsmOperandKind::Tied(idx) = &kind {
            input_tied_to.push(Some(*idx));
        } else {
            input_tied_to.push(None);
        }
        emitter.setup_operand_metadata(&mut op, val, false);

        // For multi-alternative constraints (e.g., "Ir", "ri", "In") that were classified
        // as GpReg but have a constant input value, promote to Immediate so the value
        // is emitted as $value instead of loaded into a register. Only do this when
        // the constraint actually contains an immediate alternative character.
        if matches!(op.kind, AsmOperandKind::GpReg) {
            if let Operand::Const(c) = val {
                if constraint_has_immediate_alt(constraint) {
                    op.imm_value = c.to_i64();
                    op.kind = AsmOperandKind::Immediate;
                }
            }
        }

        operands.push(op);
    }

    // Populate symbol names for input operands from input_symbols
    for (i, sym) in input_symbols.iter().enumerate() {
        let op_idx = outputs.len() + i;
        if op_idx < operands.len() {
            if let Some(ref s) = sym {
                operands[op_idx].imm_symbol = Some(s.clone());
            }
        }
    }

    // For Immediate operands that have neither an imm_value nor an imm_symbol,
    // the value is a runtime expression (e.g., &struct.member) that couldn't be
    // resolved to a constant or symbol.
    let mut has_unsatisfiable_imm = false;
    for op in operands.iter_mut() {
        if matches!(op.kind, AsmOperandKind::Immediate) && op.imm_value.is_none() && op.imm_symbol.is_none() {
            if constraint_is_immediate_only(&op.constraint) {
                // Pure immediate constraint (e.g., "i", "n") with no register or memory
                // alternative, but the expression couldn't be evaluated at compile time.
                // This happens in static inline functions where the "i" operand depends
                // on a function parameter — the standalone body can't evaluate it, but
                // GCC always inlines such functions. Emit $0 as a placeholder to produce
                // valid assembly. The function won't produce correct results if called
                // at runtime, but these functions are only called with constant args.
                // TODO: Implement function inlining so these static inline functions
                // are inlined at call sites with constant arguments, making the "i"
                // constraint evaluable. Once inlining is implemented, this placeholder
                // path should be replaced with a proper diagnostic/error.
                op.imm_value = Some(0);
                has_unsatisfiable_imm = true;
            } else {
                // Multi-alternative constraint (e.g., "Ir" classified as Immediate but
                // value is runtime). Fall back to GpReg so the value gets loaded into
                // a register for use by %P/%a modifiers.
                op.kind = AsmOperandKind::GpReg;
            }
        }
    }

    // If we have unsatisfiable immediate constraints AND the template creates
    // section data (via .pushsection), skip the entire inline asm block.
    // This prevents emitting corrupt metadata (e.g., __jump_table entries with
    // null key pointers) that would crash the kernel during boot.
    // The function will still be emitted but won't execute the inline asm,
    // falling through to the default code path (e.g., `return false` for
    // arch_static_branch).
    // TODO: Replace this heuristic with proper function inlining support.
    // Once always_inline functions are inlined at call sites, the "i" constraints
    // will be evaluable and this skip path will no longer be needed.
    if has_unsatisfiable_imm && template.contains(".pushsection") {
        return;
    }

    // Collect registers claimed by specific-register constraints (e.g., "c" -> rcx)
    // and explicit clobber registers so the scratch allocator avoids them.
    let mut specific_regs: Vec<String> = operands.iter()
        .filter(|op| matches!(op.kind, AsmOperandKind::Specific(_)))
        .map(|op| op.reg.clone())
        .collect();
    // Also exclude registers listed in the clobber list (e.g., "x9", "t0", "rcx").
    // Without this, the scratch allocator might assign a clobbered register to an
    // operand, causing the inline asm to corrupt operand values.
    for clobber in clobbers {
        // Skip non-register clobbers like "cc" and "memory"
        if clobber == "cc" || clobber == "memory" {
            continue;
        }
        specific_regs.push(clobber.clone());
        // On ARM64, wN and xN refer to the same physical register (32-bit vs 64-bit view).
        // The scratch allocator uses xN notation, so also exclude xN when wN is clobbered
        // and vice versa, to prevent register conflicts.
        if let Some(suffix) = clobber.strip_prefix('w') {
            if suffix.chars().all(|c| c.is_ascii_digit()) {
                specific_regs.push(format!("x{}", suffix));
            }
        } else if let Some(suffix) = clobber.strip_prefix('x') {
            if suffix.chars().all(|c| c.is_ascii_digit()) {
                specific_regs.push(format!("w{}", suffix));
            }
        }
    }

    // Assign scratch registers to operands that need them
    for i in 0..total_operands {
        if !operands[i].reg.is_empty() {
            continue;
        }
        match &operands[i].kind {
            AsmOperandKind::Memory | AsmOperandKind::Immediate => continue,
            AsmOperandKind::Tied(_) => continue,
            AsmOperandKind::X87St0 => { operands[i].reg = "st(0)".to_string(); continue; }
            AsmOperandKind::X87St1 => { operands[i].reg = "st(1)".to_string(); continue; }
            kind => {
                let is_tied = if i >= outputs.len() {
                    input_tied_to[i - outputs.len()].is_some()
                } else {
                    false
                };
                if !is_tied {
                    operands[i].reg = emitter.assign_scratch_reg(kind, &specific_regs);
                }
            }
        }
    }

    // Resolve tied operands: copy register and metadata from the target operand.
    for i in 0..total_operands {
        let tied_target = if let AsmOperandKind::Tied(tied_to) = operands[i].kind {
            Some(tied_to)
        } else if i >= outputs.len() {
            let input_idx = i - outputs.len();
            if operands[i].reg.is_empty() {
                input_tied_to[input_idx]
            } else {
                None
            }
        } else {
            None
        };
        if let Some(target) = tied_target {
            if target < operands.len() {
                let source = operands[target].clone();
                operands[i].copy_metadata_from(&source);
            }
        }
    }

    // Populate operand types
    for (i, ty) in operand_types.iter().enumerate() {
        if i < operands.len() {
            operands[i].operand_type = *ty;
        }
    }

    // Resolve memory operand addresses for outputs
    for (i, (_, ptr, _)) in outputs.iter().enumerate() {
        if matches!(operands[i].kind, AsmOperandKind::Memory) {
            let val = Operand::Value(*ptr);
            emitter.resolve_memory_operand(&mut operands[i], &val, &specific_regs);
        }
    }

    // Handle "+" read-write constraints: synthetic inputs share the output's register.
    let num_plus = outputs.iter().filter(|(c,_,_)| c.contains('+')).count();
    let mut plus_idx = 0;
    for (i, (constraint, _, _)) in outputs.iter().enumerate() {
        if constraint.contains('+') {
            let plus_input_idx = outputs.len() + plus_idx;
            if plus_input_idx < total_operands {
                let source = operands[i].clone();
                operands[plus_input_idx].copy_metadata_from(&source);
                operands[plus_input_idx].kind = source.kind;
                operands[plus_input_idx].operand_type = source.operand_type;
            }
            plus_idx += 1;
        }
    }

    // Build GCC operand number → internal index mapping.
    let num_gcc_operands = outputs.len() + (inputs.len() - num_plus);
    let mut gcc_to_internal: Vec<usize> = Vec::with_capacity(num_gcc_operands);
    for i in 0..outputs.len() {
        gcc_to_internal.push(i);
    }
    for i in num_plus..inputs.len() {
        gcc_to_internal.push(outputs.len() + i);
    }

    // Resolve memory operand addresses for non-synthetic input operands
    for (i, (_, val, _)) in inputs.iter().enumerate() {
        if i < num_plus { continue; }
        let op_idx = outputs.len() + i;
        if matches!(operands[op_idx].kind, AsmOperandKind::Memory) {
            emitter.resolve_memory_operand(&mut operands[op_idx], val, &specific_regs);
        }
    }

    // Apply segment prefixes to memory operands (for __seg_gs/__seg_fs)
    if !seg_overrides.is_empty() {
        for (i, op) in operands.iter_mut().enumerate() {
            if i < seg_overrides.len() {
                match seg_overrides[i] {
                    AddressSpace::SegGs => op.seg_prefix = "%gs:".to_string(),
                    AddressSpace::SegFs => op.seg_prefix = "%fs:".to_string(),
                    AddressSpace::Default => {}
                }
            }
        }
    }

    // Phase 2: Load input values into their assigned registers
    // First pass: load non-x87 inputs
    for (i, (constraint, val, _)) in inputs.iter().enumerate() {
        let op_idx = outputs.len() + i;
        match &operands[op_idx].kind {
            AsmOperandKind::Memory | AsmOperandKind::Immediate => continue,
            AsmOperandKind::X87St0 | AsmOperandKind::X87St1 => continue, // handled below
            _ => {}
        }
        if operands[op_idx].reg.is_empty() {
            continue;
        }
        emitter.load_input_to_reg(&operands[op_idx], val, constraint);
    }

    // Pre-load read-write output values (non-x87)
    // Skip Specific register constraints (e.g., {rsp}) because the synthetic + input
    // already loaded the correct register value via load_input_to_reg. Preloading from
    // the output alloca would overwrite the register with uninitialized data, which is
    // catastrophic for registers like RSP (global register variables bound to the stack pointer).
    for (i, (constraint, ptr, _)) in outputs.iter().enumerate() {
        if constraint.contains('+') {
            if !matches!(operands[i].kind, AsmOperandKind::Memory | AsmOperandKind::X87St0 | AsmOperandKind::X87St1 | AsmOperandKind::Specific(_)) {
                emitter.preload_readwrite_output(&operands[i], ptr);
            }
        }
    }

    // x87 FPU stack inputs must be loaded in reverse stack order: st(1) first, then st(0),
    // because each fld pushes onto the stack (LIFO). Collect x87 inputs, sort by stack
    // position descending, then load them.
    {
        let mut x87_inputs: Vec<(usize, usize)> = Vec::new(); // (input_index, stack_position: 0=st0, 1=st1)
        for (i, (_, _, _)) in inputs.iter().enumerate() {
            let op_idx = outputs.len() + i;
            match &operands[op_idx].kind {
                AsmOperandKind::X87St0 => x87_inputs.push((i, 0)),
                AsmOperandKind::X87St1 => x87_inputs.push((i, 1)),
                _ => {}
            }
        }
        // Also collect x87 read-write outputs that need preloading
        let mut x87_rw_outputs: Vec<(usize, usize)> = Vec::new(); // (output_index, stack_position)
        for (i, (constraint, _, _)) in outputs.iter().enumerate() {
            if constraint.contains('+') {
                match &operands[i].kind {
                    AsmOperandKind::X87St0 => x87_rw_outputs.push((i, 0)),
                    AsmOperandKind::X87St1 => x87_rw_outputs.push((i, 1)),
                    _ => {}
                }
            }
        }
        // Sort by stack position descending (st(1) loaded first, then st(0))
        x87_inputs.sort_by(|a, b| b.1.cmp(&a.1));
        x87_rw_outputs.sort_by(|a, b| b.1.cmp(&a.1));
        // Load x87 read-write outputs first (preload), then regular x87 inputs
        for (out_idx, _) in &x87_rw_outputs {
            emitter.preload_readwrite_output(&operands[*out_idx], &outputs[*out_idx].1);
        }
        for (inp_idx, _) in &x87_inputs {
            let op_idx = outputs.len() + inp_idx;
            emitter.load_input_to_reg(&operands[op_idx], &inputs[*inp_idx].1, &inputs[*inp_idx].0);
        }
    }

    // Phase 3: Substitute operand references in template and emit
    let lines: Vec<&str> = template.split('\n').collect();
    for line in &lines {
        let line = line.trim().trim_start_matches('\t').trim();
        if line.is_empty() {
            continue;
        }
        let resolved = emitter.substitute_template_line(line, &operands, &gcc_to_internal, operand_types, goto_labels);
        emitter.asm_state().emit_fmt(format_args!("    {}", resolved));
    }

    // Phase 4: Store output register values back to their stack slots
    for (i, (constraint, ptr, _)) in outputs.iter().enumerate() {
        if constraint.contains('=') || constraint.contains('+') {
            emitter.store_output_from_reg(&operands[i], ptr, constraint);
        }
    }
}

/// Substitute `%l[name]` and `%lN` goto label references in an already-substituted line.
/// In GCC asm goto, `%l[name]` resolves to the assembly label for the C goto label `name`,
/// and `%lN` resolves to the label at index N (relative to the total number of
/// output+input operands, so label 0 is at GCC operand index = num_operands).
///
/// This is called as a post-processing step after regular operand substitution,
/// to handle any remaining `%l[...]` or `%l<digit>` patterns that weren't consumed.
pub fn substitute_goto_labels(line: &str, goto_labels: &[(String, BlockId)], num_operands: usize) -> String {
    if goto_labels.is_empty() {
        return line.to_string();
    }
    let mut result = String::new();
    let chars: Vec<char> = line.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        if chars[i] == '%' && i + 1 < chars.len() && chars[i + 1] == 'l' {
            // Check for %l[name] or %l<digit>
            if i + 2 < chars.len() && chars[i + 2] == '[' {
                // %l[name] - named goto label reference
                let mut j = i + 3;
                while j < chars.len() && chars[j] != ']' {
                    j += 1;
                }
                let name: String = chars[i + 3..j].iter().collect();
                if j < chars.len() { j += 1; } // skip ]
                // Look up the label name
                if let Some((_, block_id)) = goto_labels.iter().find(|(n, _)| n == &name) {
                    result.push_str(&block_id.to_string());
                    i = j;
                    continue;
                }
                // Not found - emit as-is
                result.push(chars[i]);
                i += 1;
            } else if i + 2 < chars.len() && chars[i + 2].is_ascii_digit() {
                // %l<digit> - positional goto label reference
                // In GCC, %l0 refers to the first goto label (GCC numbers labels after operands)
                let mut j = i + 2;
                let mut num = 0usize;
                while j < chars.len() && chars[j].is_ascii_digit() {
                    num = num * 10 + (chars[j] as usize - '0' as usize);
                    j += 1;
                }
                // GCC numbers goto labels after all output+input operands.
                // %l<N> where N >= num_operands refers to label (N - num_operands).
                // If N < num_operands, this is not a valid label reference.
                let label_idx = num.wrapping_sub(num_operands);
                if label_idx < goto_labels.len() {
                    result.push_str(&goto_labels[label_idx].1.to_string());
                    i = j;
                    continue;
                }
                // Not found - emit as-is
                result.push(chars[i]);
                i += 1;
            } else {
                result.push(chars[i]);
                i += 1;
            }
        } else {
            result.push(chars[i]);
            i += 1;
        }
    }
    result
}
