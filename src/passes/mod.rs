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
pub mod div_by_const;
pub mod gvn;
pub mod if_convert;
pub mod inline;
pub mod ipcp;
pub mod iv_strength_reduce;
pub mod licm;
pub mod loop_analysis;
pub mod narrow;
pub mod simplify;

use crate::common::fx_hash::{FxHashMap, FxHashSet};
use crate::ir::analysis::CfgAnalysis;
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

/// Run GVN, LICM, and IVSR with shared CFG analysis per function.
///
/// For each dirty function, builds CFG/dominator/loop analysis once and passes
/// it to all three passes. This eliminates redundant analysis computation that
/// previously occurred when each pass independently computed build_label_map +
/// build_cfg + compute_dominators (+ find_natural_loops for LICM/IVSR).
///
/// Returns (gvn_changes, licm_changes, ivsr_changes).
fn run_gvn_licm_ivsr_shared(
    module: &mut IrModule,
    visit: &[bool],
    changed: &mut [bool],
    run_gvn: bool,
    run_licm: bool,
    run_ivsr: bool,
    time_passes: bool,
    iter: usize,
) -> (usize, usize, usize) {
    let mut gvn_total = 0usize;
    let mut licm_total = 0usize;
    let mut ivsr_total = 0usize;

    for (i, func) in module.functions.iter_mut().enumerate() {
        if func.is_declaration {
            continue;
        }
        if i < visit.len() && !visit[i] {
            continue;
        }
        let num_blocks = func.blocks.len();
        if num_blocks == 0 {
            continue;
        }

        // GVN fast path: single-block functions don't need CFG analysis.
        if num_blocks == 1 {
            if run_gvn {
                let n = gvn::run_gvn_function(func);
                if n > 0 {
                    gvn_total += n;
                    if i < changed.len() { changed[i] = true; }
                }
            }
            // LICM and IVSR need loops (>= 2 blocks), so skip.
            continue;
        }

        // Build CFG analysis once for this function.
        let cfg = CfgAnalysis::build(func);

        // Run GVN with shared analysis.
        if run_gvn {
            let t0 = if time_passes { Some(std::time::Instant::now()) } else { None };
            let n = gvn::run_gvn_with_analysis(func, &cfg);
            if let Some(t0) = t0 {
                eprintln!("[PASS] iter={} gvn (func {}): {:.4}s ({} changes)", iter, func.name, t0.elapsed().as_secs_f64(), n);
            }
            if n > 0 {
                gvn_total += n;
                if i < changed.len() { changed[i] = true; }
            }
        }

        // Run LICM with shared analysis.
        // GVN does not modify the CFG (only replaces operands), so analysis is still valid.
        if run_licm {
            let t0 = if time_passes { Some(std::time::Instant::now()) } else { None };
            let n = licm::licm_with_analysis(func, &cfg);
            if let Some(t0) = t0 {
                eprintln!("[PASS] iter={} licm (func {}): {:.4}s ({} changes)", iter, func.name, t0.elapsed().as_secs_f64(), n);
            }
            if n > 0 {
                licm_total += n;
                if i < changed.len() { changed[i] = true; }
            }
        }

        // Run IVSR with shared analysis.
        // LICM hoists instructions to preheaders but does not add/remove blocks,
        // so CFG analysis is still valid.
        if run_ivsr {
            let t0 = if time_passes { Some(std::time::Instant::now()) } else { None };
            let n = iv_strength_reduce::ivsr_with_analysis(func, &cfg);
            if let Some(t0) = t0 {
                eprintln!("[PASS] iter={} iv_strength_reduce (func {}): {:.4}s ({} changes)", iter, func.name, t0.elapsed().as_secs_f64(), n);
            }
            if n > 0 {
                ivsr_total += n;
                if i < changed.len() { changed[i] = true; }
            }
        }
    }

    if time_passes {
        eprintln!("[PASS] iter={} gvn_total: {} changes", iter, gvn_total);
        eprintln!("[PASS] iter={} licm_total: {} changes", iter, licm_total);
        eprintln!("[PASS] iter={} ivsr_total: {} changes", iter, ivsr_total);
    }

    (gvn_total, licm_total, ivsr_total)
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
pub fn run_passes(module: &mut IrModule, _opt_level: u32, target: crate::backend::Target) {
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
        crate::ir::mem2reg::promote_allocas_with_params(module);

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

    // Phase 0.5: Resolve remaining IsConstant (__builtin_constant_p) to 0.
    // After inlining and the post-inline constant_fold+copy_prop passes above,
    // any IsConstant whose operand became constant through inlining has already
    // been resolved to Copy(1). The remaining IsConstant instructions have
    // operands that are genuinely non-constant (global variables, non-inlined
    // parameters, etc.) and must resolve to 0.
    //
    // This must happen before the main optimization loop so that cfg_simplify
    // can fold CondBranch instructions testing __builtin_constant_p results and
    // eliminate dead code paths. Without this, unreachable calls to intentionally
    // undefined symbols (like the kernel's __bad_udelay) survive and cause link errors.
    constant_fold::resolve_remaining_is_constant(module);

    // Run up to 3 iterations of the full pipeline. After the first full pass,
    // subsequent iterations only process functions that were modified (dirty
    // tracking), avoiding redundant work on already-optimized functions.
    //
    // Per-pass skip tracking (iterations >= 1): We track which passes made changes
    // in each iteration. In subsequent iterations, a pass is skipped if:
    // - It made 0 changes in the previous iteration, AND
    // - None of its "upstream" passes (which could create new opportunities) made
    //   changes in the previous iteration.
    // This avoids running expensive passes (GVN, LICM) when no upstream pass
    // generated new optimization opportunities for them.
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

    let time_passes = std::env::var("CCC_TIME_PASSES").is_ok();

    // Per-pass change counts from the previous iteration, used for skip decisions.
    // Pass indices: 0=cfg1, 1=copyprop1, 2=narrow, 3=simplify, 4=constfold,
    //               5=gvn, 6=licm, 7=ifconv, 8=copyprop2, 9=dce, 10=cfg2
    const NUM_PASSES: usize = 11;
    let mut prev_pass_changes = [usize::MAX; NUM_PASSES]; // MAX = "assume changed" for iter 0

    // Track first iteration's total changes for diminishing-returns early exit.
    let mut iter0_total_changes = 0usize;

    for iter in 0..iterations {
        let mut total_changes = 0usize;
        let mut total_changes_excl_dce = 0usize; // Exclude DCE for diminishing-returns check
        let mut cur_pass_changes = [0usize; NUM_PASSES];

        // Clear the changed accumulator for this iteration
        changed.iter_mut().for_each(|c| *c = false);

        macro_rules! timed_pass {
            ($name:expr, $body:expr) => {{
                if time_passes {
                    let t0 = std::time::Instant::now();
                    let n = $body;
                    let elapsed = t0.elapsed().as_secs_f64();
                    eprintln!("[PASS] iter={} {}: {:.4}s ({} changes)", iter, $name, elapsed, n);
                    n
                } else {
                    $body
                }
            }};
        }

        // Helper: check if a pass should run based on upstream pass changes.
        // A pass runs if it or any of its upstream passes made changes last iteration.
        // On iteration 0, all passes run (prev_pass_changes are MAX).
        //
        // Pass dependency graph (which passes create opportunities for which):
        //   cfg_simplify → copy_prop, gvn, dce (simpler CFG)
        //   copy_prop → simplify, constfold, gvn, narrow (propagated values)
        //   narrow → simplify, constfold (smaller types)
        //   simplify → constfold, copy_prop, gvn (reduced expressions, folded casts to copies)
        //   constfold → cfg_simplify, copy_prop, dce (constant branches/dead code, folded exprs to copies)
        //   gvn → copy_prop, dce (eliminated redundant computations)
        //   licm → copy_prop, dce (hoisted code)
        //   if_convert → copy_prop, dce (eliminated branches)
        //   dce → cfg_simplify (empty blocks)
        macro_rules! should_run {
            ($self_idx:expr, $($upstream:expr),*) => {{
                prev_pass_changes[$self_idx] > 0 $(|| prev_pass_changes[$upstream] > 0)*
            }};
        }

        // Phase 1: CFG simplification
        // Upstream: constfold (constant branches), dce (empty blocks)
        if !dis_cfg && should_run!(0, 4, 9) {
            let n = timed_pass!("cfg_simplify1", run_on_visited(module, &dirty, &mut changed, cfg_simplify::run_function));
            cur_pass_changes[0] = n;
            total_changes += n;
            total_changes_excl_dce += n;
        }

        // Phase 2: Copy propagation
        // Upstream: cfg_simplify (simpler CFG), gvn (eliminated exprs), licm (hoisted code), if_convert
        if !dis_copyprop && should_run!(1, 0, 5, 6, 7) {
            let n = timed_pass!("copy_prop1", run_on_visited(module, &dirty, &mut changed, copy_prop::propagate_copies));
            cur_pass_changes[1] = n;
            total_changes += n;
            total_changes_excl_dce += n;
        }

        // Phase 2a: Division-by-constant strength reduction (first iteration only).
        // Replaces slow div/idiv instructions with fast multiply-and-shift sequences.
        // Run early so subsequent passes (narrowing, simplify, constant folding, DCE)
        // can optimize the expanded instruction sequences.
        //
        // Disabled on i686: the pass generates I64 multiply + shift-right-32 sequences
        // to extract the high 32 bits of a widened multiplication. The i686 backend
        // cannot execute 64-bit arithmetic correctly (it truncates to 32 bits), so these
        // sequences produce wrong results. Fall back to hardware idiv/div instead.
        // TODO: Re-enable once i686 has proper 64-bit arithmetic support, or implement
        // a 32-bit-aware variant that uses single-operand imull for mulhi.
        if iter == 0 && !disabled.contains("divconst") && !target.is_32bit() {
            let n = timed_pass!("div_by_const", run_on_visited(module, &dirty, &mut changed, div_by_const::div_by_const_function));
            total_changes += n;
            total_changes_excl_dce += n;
        }

        // Phase 2b: Integer narrowing
        // Upstream: copy_prop (propagated values expose narrowing)
        if !dis_narrow && should_run!(2, 1) {
            let n = timed_pass!("narrow", run_on_visited(module, &dirty, &mut changed, narrow::narrow_function));
            cur_pass_changes[2] = n;
            total_changes += n;
            total_changes_excl_dce += n;
        }

        // Phase 3: Algebraic simplification
        // Upstream: copy_prop (propagated values), narrow (smaller types)
        if !dis_simplify && should_run!(3, 1, 2) {
            let n = timed_pass!("simplify", run_on_visited(module, &dirty, &mut changed, simplify::simplify_function));
            cur_pass_changes[3] = n;
            total_changes += n;
            total_changes_excl_dce += n;
        }

        // Phase 4: Constant folding
        // Upstream: copy_prop (propagated constants), narrow, simplify (reduced exprs),
        //           if_convert (creates Select that constfold can fold with known-constant cond),
        //           copy_prop2 (propagates constants into Select/Cmp operands after if_convert)
        if !dis_constfold && should_run!(4, 1, 2, 3, 7, 8) {
            let n = timed_pass!("constfold", run_on_visited(module, &dirty, &mut changed, constant_fold::fold_function));
            cur_pass_changes[4] = n;
            total_changes += n;
            total_changes_excl_dce += n;
        }

        // Phases 5-6a: GVN + LICM + IVSR with shared CFG analysis.
        //
        // These three passes all need CFG + dominator + loop analysis. Since GVN
        // does not modify the CFG (it only replaces instruction operands within
        // existing blocks), the analysis computed for GVN remains valid for LICM
        // and IVSR. We compute it once per function and share it across all three.
        {
            let run_gvn = !dis_gvn && should_run!(5, 0, 1, 3);
            let run_licm = !dis_licm && should_run!(6, 0, 1, 5);
            let run_ivsr = iter == 0 && !disabled.contains("ivsr");

            if run_gvn || run_licm || run_ivsr {
                let (gvn_n, licm_n, ivsr_n) = run_gvn_licm_ivsr_shared(
                    module, &dirty, &mut changed,
                    run_gvn, run_licm, run_ivsr,
                    time_passes, iter,
                );
                cur_pass_changes[5] = gvn_n;
                total_changes += gvn_n;
                total_changes_excl_dce += gvn_n;
                cur_pass_changes[6] = licm_n;
                total_changes += licm_n;
                total_changes_excl_dce += licm_n;
                total_changes += ivsr_n;
                total_changes_excl_dce += ivsr_n;
            }
        }

        // Phase 7: If-conversion
        // Upstream: cfg_simplify (simpler CFG), constfold (simplified conditions)
        if !dis_ifconv && should_run!(7, 0, 4) {
            let n = timed_pass!("if_convert", run_on_visited(module, &dirty, &mut changed, if_convert::if_convert_function));
            cur_pass_changes[7] = n;
            total_changes += n;
            total_changes_excl_dce += n;
        }

        // Phase 8: Copy propagation again
        // Upstream: simplify (folded casts to copies), constfold (folded exprs to copies),
        //           gvn (produced copies), licm (hoisted code), if_convert (select values)
        // Note: simplify and constfold run earlier in this iteration, so we check
        // cur_pass_changes for them (not just prev_pass_changes via should_run!).
        if !dis_copyprop && (should_run!(8, 5, 6, 7) || cur_pass_changes[3] > 0 || cur_pass_changes[4] > 0) {
            let n = timed_pass!("copy_prop2", run_on_visited(module, &dirty, &mut changed, copy_prop::propagate_copies));
            cur_pass_changes[8] = n;
            total_changes += n;
            total_changes_excl_dce += n;
        }

        // Phase 9: Dead code elimination
        // Upstream: gvn, licm, if_convert, copy_prop2 (produced dead instructions)
        // Note: DCE changes are excluded from the diminishing-returns comparison
        // (total_changes_excl_dce) because DCE is a cleanup pass that removes dead
        // instructions. Its large change count (often 5000+ in iteration 0) inflates
        // iter0_total_changes, and by removing instructions, DCE actually reduces
        // the work subsequent passes can do in later iterations. This combination
        // causes the diminishing-returns heuristic to exit too early, preventing
        // the optimizer from completing multi-iteration constant propagation chains
        // (e.g., kernel's cpucap_is_possible switch folding through inlined
        // system_supports_sme -> alternative_has_cap_unlikely -> cpucap_is_possible).
        if !dis_dce && should_run!(9, 5, 6, 7, 8) {
            let n = timed_pass!("dce", run_on_visited(module, &dirty, &mut changed, dce::eliminate_dead_code));
            cur_pass_changes[9] = n;
            total_changes += n;
            // Intentionally NOT added to total_changes_excl_dce
        }

        // Phase 10: CFG simplification again
        // Upstream: constfold (constant branches), dce (dead blocks), if_convert
        if !dis_cfg && should_run!(10, 4, 7, 9) {
            let n = timed_pass!("cfg_simplify2", run_on_visited(module, &dirty, &mut changed, cfg_simplify::run_function));
            cur_pass_changes[10] = n;
            total_changes += n;
            total_changes_excl_dce += n;
        }

        // Phase 10.5: Interprocedural constant propagation (IPCP).
        // Run on every iteration, not just iter 0, because later iterations may
        // have simplified call arguments to constants (e.g., phi nodes collapsed
        // after CFG simplification resolved dead branches from IS_ENABLED() checks).
        let mut ipcp_changes = 0;
        if !dis_ipcp {
            ipcp_changes = timed_pass!("ipcp", ipcp::run(module));
            if ipcp_changes > 0 {
                changed.iter_mut().for_each(|c| *c = true);
            }
            total_changes += ipcp_changes;
            total_changes_excl_dce += ipcp_changes;
        }

        if iter == 0 {
            iter0_total_changes = total_changes_excl_dce;
        }

        // Early exit: if no passes changed anything, additional iterations are useless.
        if total_changes == 0 {
            break;
        }

        // Diminishing returns: if this iteration produced very few changes relative
        // to the first iteration, another iteration is unlikely to be worthwhile.
        // The optimizer converges quickly: typically iter 0 finds ~264K changes,
        // iter 1 finds ~10K, iter 2 finds ~200. Stopping when an iteration yields
        // less than 5% of the first iteration's output saves one full pipeline
        // iteration with negligible impact on optimization quality.
        //
        // We use total_changes_excl_dce for this comparison because DCE is a
        // cleanup pass whose large change count (removing dead instructions)
        // inflates iter0's total and makes subsequent iterations look like
        // diminishing returns even when they're still making meaningful progress.
        // DCE also reduces the number of instructions available for other passes,
        // naturally lowering their change counts in later iterations.
        //
        // Exception: if IPCP made changes this iteration, always run another
        // iteration regardless of diminishing returns. IPCP changes (constant
        // argument propagation, dead call elimination) create opportunities for
        // constant folding, DCE, and CFG simplification that require a full pass
        // to clean up. Without this, dead code referencing undefined symbols
        // (like the kernel's convert_to_fxsr) would survive.
        // We require iter > 1 (at least 2 full iterations) because multi-step
        // constant propagation chains (e.g., kernel's switch folding through
        // inlined cpucap_is_possible -> alternative_has_cap_unlikely) need at
        // least 2 iterations to complete: iter0 for initial folding, iter1 for
        // propagating results through the control flow.
        const DIMINISHING_RETURNS_FACTOR: usize = 20; // 1/20 = 5% threshold
        if iter > 1 && ipcp_changes == 0 && iter0_total_changes > 0
            && total_changes_excl_dce * DIMINISHING_RETURNS_FACTOR < iter0_total_changes
        {
            break;
        }

        // Save per-pass change counts for next iteration's skip decisions.
        prev_pass_changes = cur_pass_changes;

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
    // Index-based reachability analysis. Instead of cloning symbol name strings
    // for hash map keys and worklist entries, we assign each unique symbol name
    // a compact integer ID and do all reachability work with u32 indices.
    // This dramatically reduces heap allocations for large translation units.

    // Phase 1: Build name-to-index mapping for all symbols.
    // Assign unique IDs to all function and global names. A symbol name can
    // refer to both a function and a global (C allows this), so we track
    // function and global indices separately per symbol ID.
    let mut name_to_id: FxHashMap<&str, u32> = FxHashMap::default();
    let mut next_id: u32 = 0;

    // Per-ID mappings: which function/global index (if any) this symbol ID maps to.
    // Uses Option<usize> since a name might be only a function, only a global, or both.
    let mut id_func_idx: Vec<Option<usize>> = Vec::new();
    let mut id_global_idx: Vec<Option<usize>> = Vec::new();

    // Map function names to IDs
    let mut func_id: Vec<u32> = Vec::with_capacity(module.functions.len());
    for (i, func) in module.functions.iter().enumerate() {
        let id = *name_to_id.entry(func.name.as_str()).or_insert_with(|| {
            let id = next_id;
            next_id += 1;
            id_func_idx.push(None);
            id_global_idx.push(None);
            id
        });
        id_func_idx[id as usize] = Some(i);
        func_id.push(id);
    }

    // Map global names to IDs
    let mut global_id: Vec<u32> = Vec::with_capacity(module.globals.len());
    for (i, global) in module.globals.iter().enumerate() {
        let id = *name_to_id.entry(global.name.as_str()).or_insert_with(|| {
            let id = next_id;
            next_id += 1;
            id_func_idx.push(None);
            id_global_idx.push(None);
            id
        });
        id_global_idx[id as usize] = Some(i);
        global_id.push(id);
    }

    // Helper: look up or create an ID for a name that may not already exist.
    // Used for references that might point to external/undeclared symbols.
    let get_or_create_id = |name: &str, name_to_id: &mut FxHashMap<&str, u32>,
                                  next_id: &mut u32| -> u32 {
        if let Some(&id) = name_to_id.get(name) {
            id
        } else {
            // External symbol not in our function/global lists; give it an ID
            // but it won't have references of its own.
            let id = *next_id;
            *next_id += 1;
            id
        }
    };

    // Phase 2: Build reference lists per function (using symbol IDs).
    let mut func_refs: Vec<Vec<u32>> = Vec::with_capacity(module.functions.len());
    for func in &module.functions {
        if func.is_declaration {
            func_refs.push(Vec::new());
            continue;
        }
        let mut refs = Vec::new();
        for block in &func.blocks {
            for inst in &block.instructions {
                match inst {
                    Instruction::Call { func: callee, .. } => {
                        refs.push(get_or_create_id(callee, &mut name_to_id, &mut next_id));
                    }
                    Instruction::GlobalAddr { name, .. } => {
                        refs.push(get_or_create_id(name, &mut name_to_id, &mut next_id));
                    }
                    Instruction::InlineAsm { input_symbols, .. } => {
                        for sym in input_symbols {
                            if let Some(s) = sym {
                                let base = s.split('+').next().unwrap_or(s);
                                refs.push(get_or_create_id(base, &mut name_to_id, &mut next_id));
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        func_refs.push(refs);
    }

    // Build reference lists per global (from initializers).
    let mut global_refs_lists: Vec<Vec<u32>> = Vec::with_capacity(module.globals.len());
    for global in &module.globals {
        let mut id_refs = Vec::new();
        global.init.for_each_ref(&mut |name| {
            id_refs.push(get_or_create_id(name, &mut name_to_id, &mut next_id));
        });
        global_refs_lists.push(id_refs);
    }

    let total_symbols = next_id as usize;

    // Phase 3: Reachability using bit vectors for O(1) membership test.
    let mut reachable = vec![false; total_symbols];
    let mut worklist: Vec<u32> = Vec::new();

    // Roots: non-static functions
    for (i, func) in module.functions.iter().enumerate() {
        if func.is_declaration { continue; }
        if !func.is_static || func.is_used {
            let id = func_id[i] as usize;
            if !reachable[id] {
                reachable[id] = true;
                worklist.push(id as u32);
            }
        }
    }

    // Roots: non-static globals
    for (i, global) in module.globals.iter().enumerate() {
        if global.is_extern { continue; }
        if !global.is_static || global.is_common || global.is_used {
            let id = global_id[i] as usize;
            if !reachable[id] {
                reachable[id] = true;
                worklist.push(id as u32);
            }
        }
    }

    // Roots: aliases (both the alias name and its target are reachable)
    for (alias_name, target, _) in &module.aliases {
        let tid = get_or_create_id(target, &mut name_to_id, &mut next_id);
        if (tid as usize) >= reachable.len() { reachable.resize(next_id as usize, false); }
        if !reachable[tid as usize] {
            reachable[tid as usize] = true;
            worklist.push(tid);
        }
        let aid = get_or_create_id(alias_name, &mut name_to_id, &mut next_id);
        if (aid as usize) >= reachable.len() { reachable.resize(next_id as usize, false); }
        if !reachable[aid as usize] {
            reachable[aid as usize] = true;
            worklist.push(aid);
        }
    }

    // Roots: constructors and destructors
    for ctor in &module.constructors {
        let id = get_or_create_id(ctor, &mut name_to_id, &mut next_id);
        if (id as usize) >= reachable.len() { reachable.resize(next_id as usize, false); }
        if !reachable[id as usize] {
            reachable[id as usize] = true;
            worklist.push(id);
        }
    }
    for dtor in &module.destructors {
        let id = get_or_create_id(dtor, &mut name_to_id, &mut next_id);
        if (id as usize) >= reachable.len() { reachable.resize(next_id as usize, false); }
        if !reachable[id as usize] {
            reachable[id as usize] = true;
            worklist.push(id);
        }
    }

    // Ensure reachable vec is big enough for all symbols
    if reachable.len() < next_id as usize {
        reachable.resize(next_id as usize, false);
    }

    // Toplevel asm: conservatively mark static symbols whose names appear in asm
    if !module.toplevel_asm.is_empty() {
        for (i, func) in module.functions.iter().enumerate() {
            if func.is_static && !func.is_declaration {
                let fid = func_id[i] as usize;
                if !reachable[fid] {
                    for asm_str in &module.toplevel_asm {
                        if asm_str.contains(func.name.as_str()) {
                            reachable[fid] = true;
                            worklist.push(fid as u32);
                            break;
                        }
                    }
                }
            }
        }
        for (i, global) in module.globals.iter().enumerate() {
            if global.is_static && !global.is_extern {
                let gid = global_id[i] as usize;
                if !reachable[gid] {
                    for asm_str in &module.toplevel_asm {
                        if asm_str.contains(global.name.as_str()) {
                            reachable[gid] = true;
                            worklist.push(gid as u32);
                            break;
                        }
                    }
                }
            }
        }
    }

    // BFS reachability using integer worklist (no String allocation).
    // For each reachable symbol, check both its function refs and global refs
    // since a name can refer to both a function and a global in C.
    while let Some(sym_id) = worklist.pop() {
        let sid = sym_id as usize;
        // Check function references (if this symbol has a function definition)
        if sid < id_func_idx.len() {
            if let Some(fi) = id_func_idx[sid] {
                if fi < func_refs.len() {
                    for &ref_id in &func_refs[fi] {
                        let rid = ref_id as usize;
                        if rid < reachable.len() && !reachable[rid] {
                            reachable[rid] = true;
                            worklist.push(ref_id);
                        }
                    }
                }
            }
        }
        // Check global initializer references (if this symbol has a global definition)
        if sid < id_global_idx.len() {
            if let Some(gi) = id_global_idx[sid] {
                if gi < global_refs_lists.len() {
                    for &ref_id in &global_refs_lists[gi] {
                        let rid = ref_id as usize;
                        if rid < reachable.len() && !reachable[rid] {
                            reachable[rid] = true;
                            worklist.push(ref_id);
                        }
                    }
                }
            }
        }
    }

    // Phase 4: Build address_taken set (still using IDs for efficiency).
    let mut address_taken = vec![false; reachable.len()];

    for func in &module.functions {
        if func.is_declaration { continue; }
        for block in &func.blocks {
            for inst in &block.instructions {
                match inst {
                    Instruction::GlobalAddr { name, .. } => {
                        if let Some(&id) = name_to_id.get(name.as_str()) {
                            if (id as usize) < address_taken.len() {
                                address_taken[id as usize] = true;
                            }
                        }
                    }
                    Instruction::InlineAsm { input_symbols, .. } => {
                        for sym in input_symbols {
                            if let Some(s) = sym {
                                let base = s.split('+').next().unwrap_or(s);
                                if let Some(&id) = name_to_id.get(base) {
                                    if (id as usize) < address_taken.len() {
                                        address_taken[id as usize] = true;
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    for global in &module.globals {
        global.init.for_each_ref(&mut |name| {
            if let Some(&id) = name_to_id.get(name) {
                if (id as usize) < address_taken.len() {
                    address_taken[id as usize] = true;
                }
            }
        });
    }

    // Drop the borrow on module strings so we can mutate module below.
    drop(name_to_id);

    // Phase 6: Remove unreachable symbols using positional index lookups.
    // func_id[i] / global_id[i] map position to symbol ID (no borrows needed).
    let mut func_pos = 0usize;
    module.functions.retain(|func| {
        let pos = func_pos;
        func_pos += 1;
        if func.is_declaration { return true; }
        let id = func_id[pos] as usize;
        if func.is_static && func.is_always_inline {
            return (id < address_taken.len() && address_taken[id])
                || (id < reachable.len() && reachable[id]);
        }
        if !func.is_static { return true; }
        id < reachable.len() && reachable[id]
    });

    let mut global_pos = 0usize;
    module.globals.retain(|global| {
        let pos = global_pos;
        global_pos += 1;
        if global.is_extern { return true; }
        if !global.is_static { return true; }
        if global.is_common { return true; }
        let id = global_id[pos] as usize;
        id < reachable.len() && reachable[id]
    });

    // Filter symbol_attrs: visibility directives (.hidden, .protected, .internal) for
    // unreferenced symbols cause the assembler to emit undefined symbol entries, which
    // can cause linker errors (e.g., kernel's hidden vdso symbols). Only emit visibility
    // directives for symbols that are actually referenced by surviving code.
    // .weak directives for unreferenced symbols are harmless — the assembler ignores them.
    {
        // Collect all symbol names referenced by surviving functions and globals.
        let mut referenced_symbols: FxHashSet<&str> = FxHashSet::default();
        for func in &module.functions {
            if func.is_declaration { continue; }
            for block in &func.blocks {
                for inst in &block.instructions {
                    match inst {
                        Instruction::Call { func: callee, .. } => {
                            referenced_symbols.insert(callee.as_str());
                        }
                        Instruction::GlobalAddr { name, .. } => {
                            referenced_symbols.insert(name.as_str());
                        }
                        Instruction::InlineAsm { input_symbols, .. } => {
                            for sym in input_symbols {
                                if let Some(s) = sym {
                                    let base = s.split('+').next().unwrap_or(s);
                                    referenced_symbols.insert(base);
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
        for global in &module.globals {
            collect_global_init_refs_set(&global.init, &mut referenced_symbols);
        }
        // Also include names of surviving non-extern globals and non-declaration functions,
        // since they might be targets of symbol_attrs directives.
        for func in &module.functions {
            referenced_symbols.insert(func.name.as_str());
        }
        for global in &module.globals {
            referenced_symbols.insert(global.name.as_str());
        }

        module.symbol_attrs.retain(|(name, is_weak, visibility)| {
            // Always keep .weak-only directives (no visibility) — they're harmless
            if *is_weak && visibility.is_none() {
                return true;
            }
            // For entries with visibility (.hidden, .protected, .internal),
            // only keep if the symbol is actually referenced
            referenced_symbols.contains(name.as_str())
        });
    }
}

/// Collect symbol references from a global initializer into a HashSet of borrowed strings.
/// This requires an explicit lifetime annotation since the borrowed `&str` references come
/// from the `GlobalInit`'s owned String fields, which is what `GlobalInit::for_each_ref`
/// cannot express through its closure-based API.
fn collect_global_init_refs_set<'a>(init: &'a GlobalInit, refs: &mut FxHashSet<&'a str>) {
    match init {
        GlobalInit::GlobalAddr(name) | GlobalInit::GlobalAddrOffset(name, _) => {
            refs.insert(name.as_str());
        }
        GlobalInit::GlobalLabelDiff(label1, label2, _) => {
            refs.insert(label1.as_str());
            refs.insert(label2.as_str());
        }
        GlobalInit::Compound(fields) => {
            for field in fields {
                collect_global_init_refs_set(field, refs);
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
