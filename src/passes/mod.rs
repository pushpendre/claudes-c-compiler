//! Optimization passes for the IR.
//!
//! This module contains various optimization passes that transform the IR
//! to produce better code.
//!
//! All optimization levels (-O0 through -O3, -Os, -Oz) run the same full set
//! of passes. While the compiler is still maturing, having separate tiers
//! creates hard-to-find bugs where code works at one level but breaks at
//! another. We always run all passes to maximize test coverage of the
//! optimizer and catch issues early.

pub mod cfg_simplify;
pub mod constant_fold;
pub mod copy_prop;
pub mod dce;
pub mod gvn;
pub mod if_convert;
pub mod inline;
pub mod ipcp;
pub mod licm;
pub mod narrow;
pub mod simplify;

use crate::common::fx_hash::{FxHashMap, FxHashSet};
use crate::ir::ir::{Instruction, IrModule, IrFunction, GlobalInit, Operand, Value};

/// Run a per-function pass only on functions in the visit set.
///
/// `visit` indicates which functions to process in this iteration.
/// `changed` accumulates which functions were modified by any pass
/// (so the next iteration knows what to re-visit).
fn run_on_visited<F>(
    module: &mut IrModule,
    visit: &[bool],
    changed: &mut [bool],
    mut f: F,
) -> usize
where
    F: FnMut(&mut IrFunction) -> usize,
{
    let mut total = 0;
    for (i, func) in module.functions.iter_mut().enumerate() {
        if func.is_declaration {
            continue;
        }
        if i < visit.len() && !visit[i] {
            continue;
        }
        let n = f(func);
        if n > 0 {
            if i < changed.len() {
                changed[i] = true;
            }
            total += n;
        }
    }
    total
}

/// Run all optimization passes on the module.
///
/// The pass pipeline is:
/// 1. CFG simplification (remove dead blocks, thread jump chains, simplify branches)
/// 2. Copy propagation (replace uses of copies with original values)
/// 3. Algebraic simplification (strength reduction)
/// 4. Constant folding (evaluate const exprs at compile time)
/// 5. GVN / CSE (dominator-based value numbering, eliminates redundant
///    BinOp, UnaryOp, Cmp, Cast, GetElementPtr, and Load across dominated blocks)
/// 6. LICM (hoist loop-invariant code to preheaders)
/// 7. If-conversion (convert branch+phi diamonds to Select)
/// 8. Copy propagation (clean up copies from GVN/simplify/LICM)
/// 9. Dead code elimination (remove dead instructions)
/// 10. CFG simplification (clean up after DCE may have made blocks dead)
/// 11. Dead static function elimination (remove unreferenced internal-linkage functions)
///
/// All optimization levels run the same pipeline with the same number of
/// iterations. The `opt_level` parameter is accepted for API compatibility
/// but currently ignored -- all levels behave identically. This is intentional:
/// while the compiler is still maturing, running all optimizations at every
/// level maximizes test coverage and avoids bugs that only surface at specific
/// optimization tiers.
// TODO: Restore per-level optimization tiers once the compiler is stable enough
// to warrant differentiated behavior (e.g., -O0 skipping passes for faster builds).
pub fn run_passes(module: &mut IrModule, _opt_level: u32) {
    // Debug support: set CCC_DISABLE_PASSES=pass1,pass2,... to skip specific passes.
    // Useful for bisecting miscompilation bugs to a specific pass.
    // Pass names: cfg, copyprop, simplify, constfold, gvn, licm, ifconv, dce, all
    let disabled = std::env::var("CCC_DISABLE_PASSES").unwrap_or_default();
    if disabled.contains("all") {
        return;
    }

    // Phase 0: Function inlining (before the optimization loop).
    // Inline small static/static-inline functions so that subsequent passes
    // can see through constant-returning helpers and eliminate dead branches.
    // This is critical for kernel code patterns like IS_ENABLED() guards.
    if !disabled.contains("inline") {
        inline::run(module);

        // Re-run mem2reg after inlining. The inliner creates parameter allocas
        // in the callee's entry block, which becomes a non-entry block in the
        // caller. These store/load pairs through stack slots must be promoted
        // to SSA values so that constant arguments (e.g., feature bit numbers)
        // propagate through to inline asm "i" (immediate) constraints.
        // Without this, expressions like `1 << (bit & 7)` in _static_cpu_has()
        // cannot be constant-folded and inline asm emits $0 placeholders.
        crate::ir::mem2reg::promote_allocas(module);

        // After mem2reg, run scalar optimization passes so that arithmetic
        // on inlined constants is fully resolved before we try to resolve
        // inline asm symbols. The simplify pass reduces expressions like
        // cast chains, and constant folding evaluates the final values.
        // Two rounds of constant_fold+copy_prop are needed: the first resolves
        // simple arithmetic, simplify reduces remaining expressions, and the
        // second round folds the simplified results to constants.
        constant_fold::run(module);
        copy_prop::run(module);
        simplify::run(module);
        constant_fold::run(module);
        copy_prop::run(module);

        // Resolve InlineAsm input symbols that became resolvable after inlining.
        // When a callee like _static_cpu_has(596) is inlined, its "i" constraint
        // operand `&boot_cpu_data.x86_capability[bit >> 3]` becomes a GlobalAddr +
        // constant GEP chain. This pass traces the def chain and sets input_symbols
        // to "boot_cpu_data+74" so the backend can emit correct symbol references.
        resolve_inline_asm_symbols(module);
    }

    // Run up to 3 iterations of the full pipeline. After the first full pass,
    // subsequent iterations only process functions that were modified (dirty
    // tracking), avoiding redundant work on already-optimized functions.
    let iterations = 3;

    // Per-function dirty tracking: dirty[i] == true means function i needs
    // reprocessing. All functions start dirty for the first iteration.
    let num_funcs = module.functions.len();
    let mut dirty = vec![true; num_funcs];

    // Pre-compute disabled pass flags once to avoid repeated string searches.
    let dis_cfg = disabled.contains("cfg");
    let dis_copyprop = disabled.contains("copyprop");
    let dis_narrow = disabled.contains("narrow");
    let dis_simplify = disabled.contains("simplify");
    let dis_constfold = disabled.contains("constfold");
    let dis_gvn = disabled.contains("gvn");
    let dis_licm = disabled.contains("licm");
    let dis_ifconv = disabled.contains("ifconv");
    let dis_dce = disabled.contains("dce");
    let dis_ipcp = disabled.contains("ipcp");

    // `changed` accumulates which functions were modified during each iteration.
    let mut changed = vec![false; num_funcs];

    for iter in 0..iterations {
        let mut total_changes = 0usize;

        // Clear the changed accumulator for this iteration
        changed.iter_mut().for_each(|c| *c = false);

        // Phase 1: CFG simplification (remove dead blocks, thread jump chains,
        // simplify redundant conditional branches). Running early eliminates
        // dead code before other passes waste time analyzing it.
        if !dis_cfg {
            total_changes += run_on_visited(module, &dirty, &mut changed, cfg_simplify::run_function);
        }

        // Phase 2: Copy propagation (early - propagate copies from phi elimination
        // and lowering so subsequent passes see through them)
        if !dis_copyprop {
            total_changes += run_on_visited(module, &dirty, &mut changed, copy_prop::propagate_copies);
        }

        // Phase 2b: Integer narrowing (widen-operate-narrow => direct narrow operation)
        // Must run after copy propagation so widening casts are visible,
        // and before other optimizations to reduce instruction count.
        if !dis_narrow {
            total_changes += run_on_visited(module, &dirty, &mut changed, narrow::narrow_function);
        }

        // Phase 3: Algebraic simplification (x+0 => x, x*1 => x, etc.)
        if !dis_simplify {
            total_changes += run_on_visited(module, &dirty, &mut changed, simplify::simplify_function);
        }

        // Phase 4: Constant folding (evaluate const exprs at compile time)
        if !dis_constfold {
            total_changes += run_on_visited(module, &dirty, &mut changed, constant_fold::fold_function);
        }

        // Phase 5: GVN / Common Subexpression Elimination (dominator-based)
        // Eliminates redundant computations both within and across basic blocks.
        if !dis_gvn {
            total_changes += run_on_visited(module, &dirty, &mut changed, gvn::run_gvn_function);
        }

        // Phase 6: LICM - hoist loop-invariant code to preheaders.
        // Runs after scalar opts have simplified expressions, so we can
        // identify more invariants. Particularly helps inner loops with
        // redundant index computations (e.g., i*n in matrix multiply).
        if !dis_licm {
            total_changes += run_on_visited(module, &dirty, &mut changed, licm::licm_function);
        }

        // Phase 7: If-conversion - convert simple branch+phi diamonds to Select
        // instructions. Runs after scalar optimizations have simplified the CFG,
        // enabling cmov/csel emission instead of branches for simple conditionals.
        if !dis_ifconv {
            total_changes += run_on_visited(module, &dirty, &mut changed, if_convert::if_convert_function);
        }

        // Phase 8: Copy propagation again (clean up copies created by GVN/simplify)
        if !dis_copyprop {
            total_changes += run_on_visited(module, &dirty, &mut changed, copy_prop::propagate_copies);
        }

        // Phase 9: Dead code elimination (clean up dead instructions including dead copies)
        if !dis_dce {
            total_changes += run_on_visited(module, &dirty, &mut changed, dce::eliminate_dead_code);
        }

        // Phase 10: CFG simplification again (DCE + constant folding may have
        // simplified conditions, creating dead blocks or redundant branches)
        if !dis_cfg {
            total_changes += run_on_visited(module, &dirty, &mut changed, cfg_simplify::run_function);
        }

        // Phase 10.5: Interprocedural constant return propagation (IPCP).
        // After the first iteration of intra-procedural optimizations, static
        // functions that always return a constant should have been simplified to
        // `return const`. Now we can replace calls to those functions with the
        // constant value, enabling subsequent DCE/CFG passes to eliminate dead
        // branches that reference undefined symbols.
        // Run after iteration 0 (first pass) so returns have been simplified,
        // and the result feeds into iteration 1 for cleanup.
        if iter == 0 && !dis_ipcp {
            // IPCP is interprocedural - it can change callers of constant-returning
            // functions, so mark all functions as changed if it modifies any.
            let ipcp_changes = ipcp::run(module);
            if ipcp_changes > 0 {
                changed.iter_mut().for_each(|c| *c = true);
            }
            total_changes += ipcp_changes;
        }

        // Early exit: if no passes changed anything, additional iterations are useless
        if total_changes == 0 {
            break;
        }

        // Prepare dirty set for next iteration: only re-visit functions that changed.
        std::mem::swap(&mut dirty, &mut changed);
    }

    // Phase 11: Dead static function elimination.
    // After all optimizations, remove internal-linkage (static) functions that are
    // never referenced by any other function or global initializer. This is critical
    // for `static inline` functions from headers: after intra-procedural optimizations
    // eliminate dead code paths (e.g., `if (1 || expr)` removes the else branch),
    // some static inline callees may become completely unreferenced and can be removed.
    // Without this, the dead functions may reference undefined external symbols
    // (e.g., kernel's `___siphash_aligned` calling `__siphash_aligned` which doesn't
    // exist on x86 where CONFIG_HAVE_EFFICIENT_UNALIGNED_ACCESS is set).
    eliminate_dead_static_functions(module);
}

/// Remove internal-linkage (static) functions and globals that are unreachable.
///
/// After optimization passes eliminate dead code paths, some static inline functions
/// and static const globals from headers may no longer be referenced. Keeping them
/// would waste code size and may cause linker errors if they reference symbols that
/// don't exist in this translation unit.
///
/// Uses reachability analysis from roots (non-static symbols) to find all live symbols,
/// then removes unreachable static functions and globals.
fn eliminate_dead_static_functions(module: &mut IrModule) {
    // Build a map of symbol -> references for reachability analysis.
    // We need to do a proper reachability walk because:
    // - An unused static global may reference a static function (via function pointer init)
    // - Removing that global should also allow removing the function
    // - Simply collecting all references from all globals/functions over-approximates

    // First, collect references per function
    let mut func_refs: FxHashMap<String, Vec<String>> = FxHashMap::default();
    for func in &module.functions {
        if func.is_declaration {
            continue;
        }
        let mut refs = Vec::new();
        for block in &func.blocks {
            for inst in &block.instructions {
                match inst {
                    Instruction::Call { func: callee, .. } => {
                        refs.push(callee.clone());
                    }
                    Instruction::GlobalAddr { name, .. } => {
                        refs.push(name.clone());
                    }
                    Instruction::InlineAsm { input_symbols, .. } => {
                        // Inline asm input_symbols reference globals/functions
                        for sym in input_symbols {
                            if let Some(s) = sym {
                                // The symbol may be "name+offset", extract just the name
                                let base = s.split('+').next().unwrap_or(s);
                                refs.push(base.to_string());
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        func_refs.insert(func.name.clone(), refs);
    }

    // Collect references per global (from initializers)
    let mut global_refs: FxHashMap<String, Vec<String>> = FxHashMap::default();
    for global in &module.globals {
        let mut refs = Vec::new();
        collect_global_init_refs_vec(&global.init, &mut refs);
        global_refs.insert(global.name.clone(), refs);
    }

    // Identify root symbols (always reachable):
    // - Non-static functions (external linkage, callable from other TUs)
    // - Non-static globals (external linkage, visible to other TUs)
    // - Alias targets
    // - Constructors and destructors
    let mut reachable: FxHashSet<String> = FxHashSet::default();
    let mut worklist: Vec<String> = Vec::new();

    for func in &module.functions {
        if func.is_declaration {
            continue;
        }
        if !func.is_static || func.is_used {
            reachable.insert(func.name.clone());
            worklist.push(func.name.clone());
        }
    }

    for global in &module.globals {
        if global.is_extern {
            continue;
        }
        if !global.is_static || global.is_common || global.is_used {
            reachable.insert(global.name.clone());
            worklist.push(global.name.clone());
        }
    }

    for (_, target, _) in &module.aliases {
        if reachable.insert(target.clone()) {
            worklist.push(target.clone());
        }
    }
    // Alias names themselves are roots (they have external linkage)
    for (alias_name, _, _) in &module.aliases {
        if reachable.insert(alias_name.clone()) {
            worklist.push(alias_name.clone());
        }
    }

    for ctor in &module.constructors {
        if reachable.insert(ctor.clone()) {
            worklist.push(ctor.clone());
        }
    }
    for dtor in &module.destructors {
        if reachable.insert(dtor.clone()) {
            worklist.push(dtor.clone());
        }
    }

    // If there's toplevel asm, scan for potential symbol references.
    // Top-level asm may reference symbols by name, so we conservatively mark
    // any static symbol whose name appears in the asm strings as reachable.
    // Non-static symbols are already roots and don't need this treatment.
    if !module.toplevel_asm.is_empty() {
        // Collect all static symbol names for lookup
        let mut static_names: FxHashSet<String> = FxHashSet::default();
        for func in &module.functions {
            if func.is_static && !func.is_declaration {
                static_names.insert(func.name.clone());
            }
        }
        for global in &module.globals {
            if global.is_static && !global.is_extern {
                static_names.insert(global.name.clone());
            }
        }
        // Check each asm string for references to static symbols
        for asm_str in &module.toplevel_asm {
            for name in &static_names {
                if asm_str.contains(name.as_str()) {
                    if reachable.insert(name.clone()) {
                        worklist.push(name.clone());
                    }
                }
            }
        }
    }

    // BFS/worklist reachability: walk from roots following references
    while let Some(sym) = worklist.pop() {
        // If this symbol is a function, add its references
        if let Some(refs) = func_refs.get(&sym) {
            for r in refs {
                if reachable.insert(r.clone()) {
                    worklist.push(r.clone());
                }
            }
        }
        // If this symbol is a global, add its initializer references
        if let Some(refs) = global_refs.get(&sym) {
            for r in refs {
                if reachable.insert(r.clone()) {
                    worklist.push(r.clone());
                }
            }
        }
    }

    // Build a set of always_inline functions whose addresses are taken.
    // A function's address is "taken" if it appears in a GlobalAddr instruction
    // (not a Call), in an inline asm input_symbols, or in a global initializer.
    // These functions need standalone bodies even though they're always_inline,
    // because the address must resolve to a real symbol at link time.
    let mut address_taken: FxHashSet<String> = FxHashSet::default();

    // Check GlobalAddr instructions in all functions (address-of-function in code)
    for func in &module.functions {
        if func.is_declaration {
            continue;
        }
        for block in &func.blocks {
            for inst in &block.instructions {
                match inst {
                    Instruction::GlobalAddr { name, .. } => {
                        address_taken.insert(name.clone());
                    }
                    Instruction::InlineAsm { input_symbols, .. } => {
                        for sym in input_symbols {
                            if let Some(s) = sym {
                                let base = s.split('+').next().unwrap_or(s);
                                address_taken.insert(base.to_string());
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    // Check global initializers (address-of-function stored in globals)
    for global in &module.globals {
        collect_global_init_addr_taken(&global.init, &mut address_taken);
    }

    // Remove unreachable static functions and static always_inline functions.
    // Static always_inline functions should never be emitted as standalone bodies —
    // GCC never emits them. Their bodies exist only to be inlined at call sites.
    // EXCEPTION: if the function's address is taken (used as a function pointer),
    // the standalone body must be kept so the linker can resolve the reference.
    // If they're still present after inlining, their standalone bodies may contain
    // unresolvable inline asm operands (e.g., "i" constraint with %c modifier
    // referencing function parameters that are only known after inlining), which
    // produce invalid assembly like `.quad x9 - .` instead of `.quad symbol - .`.
    module.functions.retain(|func| {
        if func.is_declaration {
            return true;
        }
        // Remove static always_inline functions — unless their address is taken
        // (used as a function pointer in code or global initializers), in which case
        // we need the standalone body so the linker can resolve the reference.
        // Also keep them if they're still in the reachable set (e.g., if the inliner
        // couldn't inline all call sites due to budget limits).
        if func.is_static && func.is_always_inline {
            return address_taken.contains(&func.name) || reachable.contains(&func.name);
        }
        if !func.is_static {
            return true;
        }
        reachable.contains(&func.name)
    });

    // Remove unreachable static globals
    module.globals.retain(|global| {
        // Keep extern declarations
        if global.is_extern {
            return true;
        }
        // Keep non-static globals (external linkage)
        if !global.is_static {
            return true;
        }
        // Keep common globals (linker-merged)
        if global.is_common {
            return true;
        }
        // Keep reachable static globals
        reachable.contains(&global.name)
    });

    // Filter symbol_attrs to only include symbols that are actually reachable
    // by the emitted code. Without this, unused extern declarations (e.g. from
    // headers) with visibility attributes (e.g. from #pragma GCC visibility
    // push(hidden)) generate .hidden directives that create undefined hidden
    // symbol entries in the object file, causing linker errors.
    module.symbol_attrs.retain(|(name, _, _)| {
        reachable.contains(name)
    });
}

/// Collect symbol references from a global initializer into a Vec.
fn collect_global_init_refs_vec(init: &GlobalInit, refs: &mut Vec<String>) {
    match init {
        GlobalInit::GlobalAddr(name) | GlobalInit::GlobalAddrOffset(name, _) => {
            refs.push(name.clone());
        }
        GlobalInit::GlobalLabelDiff(label1, label2, _) => {
            refs.push(label1.clone());
            refs.push(label2.clone());
        }
        GlobalInit::Compound(fields) => {
            for field in fields {
                collect_global_init_refs_vec(field, refs);
            }
        }
        _ => {}
    }
}

/// Collect function/symbol addresses referenced from a global initializer.
/// Used to determine which always_inline functions have their address taken
/// via global initializers (e.g., function pointers in struct literals).
fn collect_global_init_addr_taken(init: &GlobalInit, addr_taken: &mut FxHashSet<String>) {
    match init {
        GlobalInit::GlobalAddr(name) | GlobalInit::GlobalAddrOffset(name, _) => {
            addr_taken.insert(name.clone());
        }
        GlobalInit::GlobalLabelDiff(label1, label2, _) => {
            addr_taken.insert(label1.clone());
            addr_taken.insert(label2.clone());
        }
        GlobalInit::Compound(fields) => {
            for field in fields {
                collect_global_init_addr_taken(field, addr_taken);
            }
        }
        _ => {}
    }
}

/// Resolve InlineAsm input symbols by tracing Value operands back through their
/// def chain to find GlobalAddr + constant GEP patterns. This is needed after
/// inlining: callee functions like `_static_cpu_has(u16 bit)` have "i" constraint
/// operands like `&boot_cpu_data.x86_capability[bit >> 3]` that couldn't be resolved
/// at lowering time (bit was a parameter). After inlining with a constant argument
/// and running mem2reg + constant folding, the IR contains a chain like:
///   v1 = GlobalAddr @boot_cpu_data
///   v2 = GEP v1, Const(74)
/// This function recognizes that pattern and sets `input_symbols[i] = "boot_cpu_data+74"`.
fn resolve_inline_asm_symbols(module: &mut IrModule) {
    for func in &mut module.functions {
        if func.is_declaration || func.blocks.is_empty() {
            continue;
        }
        resolve_asm_symbols_in_function(func);
    }
}

fn resolve_asm_symbols_in_function(func: &mut IrFunction) {
    // Build a map from Value ID to its defining instruction for fast lookup.
    let mut value_defs: FxHashMap<u32, DefInfo> = FxHashMap::default();
    for block in &func.blocks {
        for inst in &block.instructions {
            match inst {
                Instruction::GlobalAddr { dest, name } => {
                    value_defs.insert(dest.0, DefInfo::GlobalAddr(name.clone()));
                }
                Instruction::GetElementPtr { dest, base, offset, .. } => {
                    value_defs.insert(dest.0, DefInfo::Gep(*base, offset.clone()));
                }
                Instruction::BinOp { dest, op: crate::ir::ir::IrBinOp::Add, lhs, rhs, .. } => {
                    value_defs.insert(dest.0, DefInfo::Add(lhs.clone(), rhs.clone()));
                }
                Instruction::Cast { dest, src, .. } => {
                    value_defs.insert(dest.0, DefInfo::Cast(src.clone()));
                }
                Instruction::Copy { dest, src } => {
                    value_defs.insert(dest.0, DefInfo::Cast(src.clone()));
                }
                _ => {}
            }
        }
    }

    // Now scan InlineAsm instructions and try to resolve unresolved input_symbols.
    for block in &mut func.blocks {
        for inst in &mut block.instructions {
            if let Instruction::InlineAsm { inputs, input_symbols, .. } = inst {
                for (i, (constraint, operand, _)) in inputs.iter().enumerate() {
                    // Only process "i" constraint inputs that don't have a symbol yet
                    if i >= input_symbols.len() { break; }
                    if input_symbols[i].is_some() { continue; }
                    // Only for immediate-only constraints
                    if !crate::backend::inline_asm::constraint_is_immediate_only(constraint) {
                        continue;
                    }
                    // Try to resolve the operand to a symbol+offset
                    if let Operand::Value(v) = operand {
                        if let Some(sym) = try_resolve_global_symbol(v, &value_defs) {
                            input_symbols[i] = Some(sym);
                        }
                    }
                }
            }
        }
    }
}

/// Information about a value's defining instruction, used for symbol resolution.
enum DefInfo {
    GlobalAddr(String),
    Gep(Value, Operand),
    Add(Operand, Operand),
    Cast(Operand),
}

/// Try to resolve a Value to a global symbol + constant offset string.
/// Returns e.g., "boot_cpu_data" or "boot_cpu_data+74".
fn try_resolve_global_symbol(val: &Value, defs: &FxHashMap<u32, DefInfo>) -> Option<String> {
    let (name, offset) = try_resolve_global_with_offset(val, defs, 0)?;
    if offset == 0 {
        Some(name)
    } else if offset > 0 {
        Some(format!("{}+{}", name, offset))
    } else {
        Some(format!("{}{}", name, offset))
    }
}

/// Recursively trace a Value back through its def chain to find
/// GlobalAddr + constant offsets (from GEP, Add, Cast/Copy).
fn try_resolve_global_with_offset(val: &Value, defs: &FxHashMap<u32, DefInfo>, accum_offset: i64) -> Option<(String, i64)> {
    let def = defs.get(&val.0)?;
    match def {
        DefInfo::GlobalAddr(name) => Some((name.clone(), accum_offset)),
        DefInfo::Gep(base, offset) => {
            let off = match offset {
                Operand::Const(c) => c.to_i64()?,
                Operand::Value(_) => {
                    // Non-constant GEP offset - can't resolve to symbol+offset
                    return None;
                }
            };
            try_resolve_global_with_offset(base, defs, accum_offset + off)
        }
        DefInfo::Add(lhs, rhs) => {
            // Pattern: base + const_offset or const_offset + base
            match (lhs, rhs) {
                (Operand::Value(base), Operand::Const(c)) => {
                    let off = c.to_i64()?;
                    try_resolve_global_with_offset(base, defs, accum_offset + off)
                }
                (Operand::Const(c), Operand::Value(base)) => {
                    let off = c.to_i64()?;
                    try_resolve_global_with_offset(base, defs, accum_offset + off)
                }
                _ => None,
            }
        }
        DefInfo::Cast(src) => {
            // Look through casts and copies
            match src {
                Operand::Value(v) => try_resolve_global_with_offset(v, defs, accum_offset),
                _ => None,
            }
        }
    }
}
