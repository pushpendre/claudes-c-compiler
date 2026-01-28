//! Core lowering logic: AST -> alloca-based IR.
//!
//! This file contains the `Lowerer` struct and its primary methods:
//! - Construction and top-level orchestration (`lower()`)
//! - IR emission helpers (fresh_value, emit, terminate, etc.)
//! - Scope management delegation
//! - Enum/label helpers and string/array init helpers
//!
//! Function lowering pipeline is in `func_lowering.rs`, global declaration
//! lowering and declaration analysis are in `global_decl.rs`.
//!
//! Data structure definitions are in `definitions.rs`, per-function state in
//! `func_state.rs`, and type-system state in `frontend::sema::type_context`.

use std::cell::RefCell;
use std::mem::Discriminant;
use crate::common::fx_hash::{FxHashMap, FxHashSet};
use crate::frontend::parser::ast::*;
use crate::frontend::sema::{FunctionInfo, ExprTypeMap, ConstMap};
use crate::ir::ir::*;
use crate::common::types::{AddressSpace, IrType, CType};
use crate::backend::Target;
use super::definitions::*;
use super::func_state::FunctionBuildState;
use crate::frontend::sema::type_context::TypeContext;

/// Lowers AST to IR (alloca-based, not yet SSA).
pub struct Lowerer {
    /// Target architecture, used for ABI-specific lowering decisions
    pub(super) target: Target,
    pub(super) next_label: u32,
    pub(super) next_string: u32,
    pub(super) next_anon_struct: u32,
    /// Counter for unique static local variable names
    pub(super) next_static_local: u32,
    pub(super) module: IrModule,
    /// Per-function build state. None between functions, Some during lowering.
    pub(super) func_state: Option<FunctionBuildState>,
    // Global variable tracking
    pub(super) globals: FxHashMap<String, GlobalInfo>,
    // Set of known function names
    pub(super) known_functions: FxHashSet<String>,
    // Set of already-defined function bodies
    pub(super) defined_functions: FxHashSet<String>,
    // Set of function names declared with static linkage
    pub(super) static_functions: FxHashSet<String>,
    /// Set of function names declared with __attribute__((error("..."))) or __attribute__((warning("..."))).
    /// Calls to these functions should be treated as unreachable (they are compile-time assertion traps).
    pub(super) error_functions: FxHashSet<String>,
    /// Set of function names declared with __attribute__((noreturn)) or _Noreturn.
    /// After calls to these functions, emit Unreachable to avoid generating dead epilogue code.
    pub(super) noreturn_functions: FxHashSet<String>,
    /// Type-system state (struct layouts, typedefs, enum constants, type caches)
    pub(super) types: TypeContext,
    /// Metadata about known functions (consolidated FuncSig)
    pub(super) func_meta: FunctionMeta,
    /// Set of emitted global variable names (O(1) dedup)
    pub(super) emitted_global_names: FxHashSet<String>,
    /// Function signatures from semantic analysis.
    /// Used as authoritative source for function return types and parameter types,
    /// reducing the lowerer's need to re-derive type information from the raw AST.
    pub(super) sema_functions: FxHashMap<String, FunctionInfo>,
    /// Expression type annotations from semantic analysis.
    /// Maps AST Expr node addresses (as usize) to their sema-inferred CTypes.
    /// Consulted as a fast O(1) fallback in get_expr_ctype() before the lowerer
    /// does its own (more expensive) type inference using lowering-specific state.
    pub(super) sema_expr_types: ExprTypeMap,
    /// Pre-computed constant expression values from semantic analysis.
    /// Maps AST Expr node addresses (as usize) to their IrConst values.
    /// Consulted as an O(1) fast path in eval_const_expr() before the lowerer
    /// falls back to its own evaluation (which handles lowering-specific cases
    /// like global addresses, func_state const locals, etc.).
    pub(super) sema_const_values: ConstMap,
    /// Stack of local label scopes from GNU __label__ declarations.
    /// Each entry maps a label name to its scope-qualified name.
    /// When resolving a label, the stack is searched top-down so that
    /// inner __label__ declarations shadow outer ones.
    pub(super) local_label_scopes: Vec<FxHashMap<String, String>>,
    /// Counter for generating unique local label scope IDs.
    pub(super) next_local_label_scope: u32,
    /// Maps C function/variable names to linker symbol overrides from __asm__("label").
    /// E.g., `extern int strerror_r(...) __asm__("__xpg_strerror_r")` maps
    /// "strerror_r" -> "__xpg_strerror_r". Used to redirect calls/references at IR emission.
    pub(super) asm_label_map: FxHashMap<String, String>,
    /// Memoization cache for get_expr_ctype().
    /// Maps AST Expr node addresses (as usize) to their resolved CType plus
    /// the Expr discriminant at insertion time. The discriminant is checked on
    /// cache hit to detect address reuse (ABA): expressions inside TypeSpecifier
    /// trees (typeof, _Generic) can share addresses with different Expr variants
    /// in the main AST, so a stale hit from a prior discriminant must be treated
    /// as a miss to avoid returning the wrong type.
    pub(super) expr_ctype_cache: RefCell<FxHashMap<usize, (Discriminant<Expr>, Option<CType>)>>,
}

impl Lowerer {
    /// Create a new Lowerer with pre-populated state from semantic analysis.
    ///
    /// The sema-provided TypeContext contains typedefs, enum constants, struct layouts,
    /// and function typedef info collected during the sema pass.
    ///
    /// The sema-provided function map contains function signatures (return types,
    /// parameter types, variadic status) collected during sema. This is used to:
    /// - Pre-populate `known_functions` so function names are recognized immediately
    /// - Pre-populate `func_return_ctypes` for function call return type resolution
    /// - Provide authoritative CType info for `get_expr_ctype` fallback
    ///
    /// The sema-provided expr_types map contains CType annotations for AST expression
    /// nodes, keyed by their pointer address. The lowerer consults this as a fast
    /// fallback in get_expr_ctype() before doing its own type inference.
    pub fn with_type_context(
        target: Target,
        type_context: TypeContext,
        sema_functions: FxHashMap<String, FunctionInfo>,
        sema_expr_types: ExprTypeMap,
        sema_const_values: ConstMap,
    ) -> Self {
        // Pre-populate known_functions from sema's function map.
        // This means the lowerer knows about all functions before the first pass,
        // which helps with early identifier resolution.
        let mut known_functions = FxHashSet::default();
        for name in sema_functions.keys() {
            known_functions.insert(name.clone());
        }

        Self {
            target,
            next_label: 0,
            next_string: 0,
            next_anon_struct: 0,
            next_static_local: 0,
            module: IrModule::new(),
            func_state: None,
            globals: FxHashMap::default(),
            known_functions,
            defined_functions: FxHashSet::default(),
            static_functions: FxHashSet::default(),
            error_functions: FxHashSet::default(),
            noreturn_functions: FxHashSet::default(),
            types: type_context,
            func_meta: FunctionMeta::default(),
            emitted_global_names: FxHashSet::default(),
            sema_functions,
            sema_expr_types,
            sema_const_values,
            local_label_scopes: Vec::new(),
            next_local_label_scope: 0,
            asm_label_map: FxHashMap::default(),
            expr_ctype_cache: RefCell::new(FxHashMap::default()),
        }
    }

    // --- State accessors ---

    /// Access the current function build state (panics if not inside a function).
    #[inline]
    pub(super) fn func(&self) -> &FunctionBuildState {
        self.func_state.as_ref().expect("not inside a function")
    }

    /// Mutably access the current function build state (panics if not inside a function).
    #[inline]
    pub(super) fn func_mut(&mut self) -> &mut FunctionBuildState {
        self.func_state.as_mut().expect("not inside a function")
    }

    /// Returns true if the target is x86 (either x86-64 or i686).
    /// Used to select x87 80-bit extended precision format for long double memory layout.
    pub(super) fn is_x86(&self) -> bool {
        self.target == Target::X86_64 || self.target == Target::I686
    }

    /// Returns true if the target is RISC-V 64.
    /// RISC-V stores full IEEE binary128 long doubles in memory.
    pub(super) fn is_riscv(&self) -> bool {
        self.target == Target::Riscv64
    }

    /// Returns true if the target is AArch64.
    /// ARM64 stores full IEEE binary128 long doubles in memory.
    pub(super) fn is_arm(&self) -> bool {
        self.target == Target::Aarch64
    }

    /// Returns true if the target uses x86 style packed _Complex float ABI
    /// (two F32s packed into a single F64/xmm register).
    /// Returns false for ARM/RISC-V which pass _Complex float as two separate F32 registers.
    pub(super) fn uses_packed_complex_float(&self) -> bool {
        self.target == Target::X86_64 || self.target == Target::I686
    }

    /// Returns true if the target packs _Complex float into a single 8-byte value
    /// for variadic function calls. On RISC-V, variadic float args go through GP
    /// registers, so _Complex float (2xF32) must be packed into one I64 GP register.
    pub(super) fn packs_complex_float_variadic(&self) -> bool {
        self.target == Target::Riscv64
    }

    /// Returns true if the target decomposes _Complex long double into 2 F128 scalar
    /// components for function argument/parameter passing.
    /// On ARM64 (AAPCS64): _Complex long double is an HFA passed in Q0/Q1 registers,
    ///   so we decompose into 2 F128 values.
    /// On x86-64: _Complex long double is passed on the stack (MEMORY class), not decomposed.
    /// On RISC-V: _Complex long double is passed by reference (pointer), not decomposed.
    pub(super) fn decomposes_complex_long_double(&self) -> bool {
        self.target == Target::Aarch64
    }

    /// Returns true if the target returns _Complex long double via register pairs
    /// (x87 st(0)/st(1) on x86) rather than via sret hidden pointer.
    /// On x86-64/i686: true (COMPLEX_X87 class, returned in st(0) and st(1)).
    /// On ARM64: false (returned via decomposition into q0/q1, handled separately).
    /// On RISC-V: false (returned via sret hidden pointer).
    pub(super) fn returns_complex_long_double_in_regs(&self) -> bool {
        self.target == Target::X86_64 || self.target == Target::I686
    }

    /// Look up the shared type metadata for a variable by name.
    ///
    /// Checks locals first, then globals. Returns `&VarInfo` which provides
    /// access to the shared fields (ty, elem_size, is_array, pointee_type,
    /// struct_layout, is_struct, array_dim_strides, c_type).
    pub(super) fn lookup_var_info(&self, name: &str) -> Option<&VarInfo> {
        if let Some(ref fs) = self.func_state {
            if let Some(info) = fs.locals.get(name) {
                return Some(&info.var);
            }
        }
        if let Some(ginfo) = self.globals.get(name) {
            return Some(&ginfo.var);
        }
        None
    }

    /// Resolve the CType of a struct/union field, handling both direct member access
    /// (s.field) and pointer member access (p->field) through a single entry point.
    pub(super) fn resolve_field_ctype(&self, base_expr: &Expr, field_name: &str, is_pointer_access: bool) -> Option<CType> {
        self.resolve_member_field_ctype_impl(base_expr, field_name, is_pointer_access)
    }

    // --- Scope management delegation ---

    /// Push a new scope frame onto both TypeContext and FunctionBuildState scope stacks.
    /// Call this at the start of a compound statement or function body.
    pub(super) fn push_scope(&mut self) {
        self.types.push_scope();
        self.func_mut().push_scope();
    }

    /// Pop the top scope frame from both TypeContext and FunctionBuildState,
    /// undoing all scoped changes made in that scope.
    /// Emits cleanup function calls for variables with __attribute__((cleanup(func)))
    /// in reverse declaration order, then restores the stack pointer for VLAs.
    pub(super) fn pop_scope(&mut self) {
        let (scope_stack_save, cleanup_vars) = self.func_mut().pop_scope();
        self.types.pop_scope();
        // Emit cleanup calls in reverse declaration order (C semantics: last declared = first cleaned up)
        self.emit_cleanup_calls(&cleanup_vars);
        // If this scope contained VLA declarations, restore the stack pointer
        // to reclaim the dynamically-allocated stack space.
        if let Some(save_val) = scope_stack_save {
            self.emit(Instruction::StackRestore { ptr: save_val });
        }
    }

    /// Emit cleanup function calls for variables with __attribute__((cleanup(func))).
    /// Calls func(&var) for each cleanup variable, in reverse declaration order.
    pub(super) fn emit_cleanup_calls(&mut self, cleanup_vars: &[(String, Value)]) {
        for (func_name, alloca_val) in cleanup_vars.iter().rev() {
            let dest = Some(self.fresh_value());
            self.emit(Instruction::Call {
                dest,
                func: func_name.clone(),
                args: vec![Operand::Value(*alloca_val)],
                arg_types: vec![IrType::Ptr],
                return_type: IrType::Void,
                is_variadic: false,
                num_fixed_args: 1,
                struct_arg_sizes: vec![None],
                struct_arg_classes: Vec::new(),
            });
        }
    }

    /// Collect all cleanup variables from all active scopes (for return statements).
    /// Returns cleanup vars from innermost scope to outermost scope, each scope's
    /// vars in reverse declaration order.
    pub(super) fn collect_all_scope_cleanup_vars(&self) -> Vec<(String, Value)> {
        self.collect_scope_cleanup_vars_above_depth(0)
    }

    /// Collect cleanup variables from scopes above `target_depth` (for break/continue).
    /// This collects from the innermost scope down to (but not including) the scope at
    /// target_depth, with each scope's vars in reverse declaration order.
    pub(super) fn collect_scope_cleanup_vars_above_depth(&self, target_depth: usize) -> Vec<(String, Value)> {
        let func = self.func();
        let mut all_cleanups = Vec::new();
        // Walk scopes from innermost to outermost, stopping at target_depth
        for frame in func.scope_stack[target_depth..].iter().rev() {
            for (func_name, alloca_val) in frame.cleanup_vars.iter().rev() {
                all_cleanups.push((func_name.clone(), *alloca_val));
            }
        }
        all_cleanups
    }

    /// Remove a local variable, tracking the removal in the current scope frame.
    pub(super) fn shadow_local_for_scope(&mut self, name: &str) {
        self.func_mut().shadow_local_for_scope(name);
    }

    /// Remove a static local name, tracking the removal in the current scope frame.
    pub(super) fn shadow_static_for_scope(&mut self, name: &str) {
        self.func_mut().shadow_static_for_scope(name);
    }

    /// Insert a local variable, tracking the change in the current scope frame.
    pub(super) fn insert_local_scoped(&mut self, name: String, info: LocalInfo) {
        self.func_mut().insert_local_scoped(name, info);
    }

    /// Insert an enum constant, tracking the change in the current scope frame.
    pub(super) fn insert_enum_scoped(&mut self, name: String, value: i64) {
        self.types.insert_enum_scoped(name, value);
    }

    /// Insert a static local name, tracking the change in the current scope frame.
    pub(super) fn insert_static_local_scoped(&mut self, name: String, mangled: String) {
        self.func_mut().insert_static_local_scoped(name, mangled);
    }

    /// Insert a const local value, tracking the change in the current scope frame.
    pub(super) fn insert_const_local_scoped(&mut self, name: String, value: i64) {
        self.func_mut().insert_const_local_scoped(name, value);
    }

    // --- Top-level orchestration ---

    pub fn lower(mut self, tu: &TranslationUnit) -> IrModule {
        // Sema has already populated TypeContext with typedefs, enum constants,
        // struct/union layouts, function typedefs, and function pointer typedefs.
        // We only need to seed target-dependent builtin typedefs (va_list, size_t, etc.)
        // and libc math function signatures that sema doesn't know about.
        self.seed_builtin_typedefs();
        self.seed_libc_math_functions();

        // Mark transparent_union on union StructLayouts before the first pass,
        // so that register_function_meta can exclude them from param_struct_sizes.
        for decl in &tu.decls {
            if let ExternalDecl::Declaration(decl) = decl {
                if decl.is_transparent_union {
                    self.mark_transparent_union(decl);
                }
            }
        }

        // First pass: collect all function signatures (return types, param types,
        // variadic status, sret) so we can distinguish functions from globals and
        // insert proper casts/ABI handling during lowering.
        for decl in &tu.decls {
            if let ExternalDecl::FunctionDef(func) = decl {
                self.register_function_meta(
                    &func.name, &func.return_type, 0,
                    &func.params, func.variadic, func.is_static, func.is_kr,
                );
            }
            if let ExternalDecl::Declaration(decl) = decl {
                for declarator in &decl.declarators {
                    // Collect __asm__("label") linker symbol redirects on function declarations.
                    // When a function declaration has __asm__("symbol"), calls to that function
                    // should emit the asm label as the linker symbol instead of the C name.
                    // E.g.: extern int strerror_r(...) __asm__("__xpg_strerror_r");
                    // Note: register variables also use asm_register for register pinning
                    // (e.g., `register int x __asm__("rbx")`), which is handled separately
                    // in lower_global_decl. We only redirect function declarations here.
                    if let Some(ref asm_label) = declarator.asm_register {
                        let is_function_decl = declarator.derived.iter().any(|d|
                            matches!(d, DerivedDeclarator::Function(_, _))
                        );
                        if is_function_decl {
                            self.asm_label_map.insert(
                                declarator.name.clone(),
                                asm_label.clone(),
                            );
                        }
                    }

                    // Find the Function derived declarator and count preceding Pointer derivations
                    let mut ptr_count = 0;
                    let mut func_info = None;
                    for d in &declarator.derived {
                        match d {
                            DerivedDeclarator::Pointer => ptr_count += 1,
                            DerivedDeclarator::Function(p, v) => {
                                func_info = Some((p.clone(), *v));
                                break;
                            }
                            _ => {}
                        }
                    }
                    if let Some((params, variadic)) = func_info {
                        self.register_function_meta(
                            &declarator.name, &decl.type_spec, ptr_count,
                            &params, variadic, decl.is_static, false,
                        );
                    } else if declarator.derived.is_empty() {
                        // Check if the base type is a function typedef
                        // (e.g., `func_t add;` where func_t is typedef int func_t(int);)
                        // Only when derived is empty — `func_t *callback;` is a variable,
                        // not a function declaration.
                        if let TypeSpecifier::TypedefName(tname) = &decl.type_spec {
                            if let Some(fti) = self.types.function_typedefs.get(tname).cloned() {
                                self.register_function_meta(
                                    &declarator.name, &fti.return_type, 0,
                                    &fti.params, fti.variadic, false, false,
                                );
                            }
                        }
                    }
                }
            }
        }

        // Collect constructor/destructor/alias attributes from function definitions and declarations
        for decl in &tu.decls {
            match decl {
                ExternalDecl::FunctionDef(func) => {
                    if func.is_constructor && !self.module.constructors.contains(&func.name) {
                        self.module.constructors.push(func.name.clone());
                    }
                    if func.is_destructor && !self.module.destructors.contains(&func.name) {
                        self.module.destructors.push(func.name.clone());
                    }
                }
                ExternalDecl::Declaration(decl) => {
                    for declarator in &decl.declarators {
                        if declarator.is_constructor && !declarator.name.is_empty()
                            && !self.module.constructors.contains(&declarator.name)
                        {
                            self.module.constructors.push(declarator.name.clone());
                        }
                        if declarator.is_destructor && !declarator.name.is_empty()
                            && !self.module.destructors.contains(&declarator.name)
                        {
                            self.module.destructors.push(declarator.name.clone());
                        }
                        // Collect __attribute__((alias("target"))) declarations
                        if let Some(ref target) = declarator.alias_target {
                            if !declarator.name.is_empty() {
                                self.module.aliases.push((
                                    declarator.name.clone(),
                                    target.clone(),
                                    declarator.is_weak,
                                ));
                            }
                        }
                        // Collect __attribute__((error("..."))) / __attribute__((warning("...")))
                        if declarator.is_error_attr && !declarator.name.is_empty() {
                            self.error_functions.insert(declarator.name.clone());
                        }
                        // Collect __attribute__((noreturn)) / _Noreturn
                        if declarator.is_noreturn && !declarator.name.is_empty() {
                            self.noreturn_functions.insert(declarator.name.clone());
                        }
                        // Collect weak/visibility attributes on extern declarations (not aliases).
                        // Skip typedefs: they are not linker symbols and should never get
                        // .weak/.hidden directives. This matters when #pragma GCC visibility
                        // push(hidden) is active (e.g., kernel EFI stub) because it would
                        // otherwise emit .hidden for thousands of typedef names.
                        if !decl.is_typedef
                            && declarator.alias_target.is_none()
                            && !declarator.name.is_empty()
                            && (declarator.is_weak || declarator.visibility.is_some())
                        {
                            self.module.symbol_attrs.push((
                                declarator.name.clone(),
                                declarator.is_weak,
                                declarator.visibility.clone(),
                            ));
                        }
                    }
                }
                ExternalDecl::TopLevelAsm(_) => {
                    // Handled in the third pass
                }
            }
        }

        // Pass 2.5: collect referenced static functions so we can skip unreferenced ones.
        // Static/inline functions from headers that are never called don't need to be lowered.
        let referenced_statics = self.collect_referenced_static_functions(tu);

        // Third pass: lower everything
        for decl in tu.decls.iter() {
            match decl {
                ExternalDecl::FunctionDef(func) => {
                    // Skip unreferenced static/static-inline functions (e.g., static inline
                    // from headers that are never called).
                    //
                    // __attribute__((gnu_inline)) forces GNU89 inline semantics:
                    //   extern inline + gnu_inline = inline definition only, no external def
                    //   → treat like static inline (can skip if unreferenced, local if emitted)
                    //   inline + gnu_inline (no extern) = external definition (must emit, global)
                    //
                    // Without gnu_inline (C99 semantics):
                    //   extern inline = provides external definition (must emit, global)
                    //   inline (alone) = inline definition only (no external def)
                    let is_gnu_inline_no_extern_def = func.is_gnu_inline && func.is_inline
                        && func.is_extern;
                    let can_skip = if func.is_static {
                        // static or static inline: internal linkage, safe to skip if unreferenced
                        true
                    } else if is_gnu_inline_no_extern_def {
                        // extern inline with gnu_inline: no external definition, skip if unreferenced
                        true
                    } else if func.is_inline {
                        // C99: plain inline = inline definition only (no external def from this)
                        // gnu_inline without extern: provides external definition (must emit)
                        false
                    } else {
                        false
                    };
                    if can_skip && !func.is_used && !referenced_statics.contains(&func.name) {
                        continue;
                    }
                    self.lower_function(func);
                }
                ExternalDecl::Declaration(decl) => {
                    self.lower_global_decl(decl);
                }
                ExternalDecl::TopLevelAsm(asm_str) => {
                    self.module.toplevel_asm.push(asm_str.clone());
                }
            }
        }
        self.module
    }

    /// Register function metadata (return type, param types, variadic, sret) for
    /// a function name. This shared helper eliminates the triplicated pattern in `lower()`
    /// where function definitions, extern declarations, and typedef-based declarations
    /// all needed to register the same metadata fields.
    ///
    /// When sema has already collected the function's CType info (via FunctionInfo),
    /// this method uses sema's return CType as source-of-truth instead of re-computing
    /// it from the AST TypeSpecifier. This reduces duplicated work and establishes
    /// sema as the authority on function type information.
    fn register_function_meta(
        &mut self,
        name: &str,
        ret_type_spec: &TypeSpecifier,
        ptr_count: usize,
        params: &[ParamDecl],
        variadic: bool,
        is_static: bool,
        is_kr: bool,
    ) {
        self.known_functions.insert(name.to_string());
        if is_static {
            self.static_functions.insert(name.to_string());
        }

        // Compute the return CType once. Prefer sema's authoritative CType if available,
        // falling back to re-computing from the AST TypeSpecifier.
        // When sema provides the return type, it already includes pointer levels from
        // the declarator chain, so we must NOT add ptr_count layers again.
        // Only the AST fallback path needs ptr_count wrapping, since type_spec_to_ctype
        // only resolves the base type specifier without pointer derivators.
        let full_ret_ctype = if let Some(func_info) = self.sema_functions.get(name) {
            func_info.return_type.clone()
        } else {
            let mut ct = self.type_spec_to_ctype(ret_type_spec);
            for _ in 0..ptr_count {
                ct = CType::Pointer(Box::new(ct), AddressSpace::Default);
            }
            ct
        };

        // Compute return type, wrapping with pointer levels if needed
        let mut ret_ty = IrType::from_ctype(&full_ret_ctype);
        // Complex return types need special IR type overrides:
        // _Complex double: real in first FP register (F64), imag in second
        // _Complex float:
        //   x86-64: packed two F32 in one xmm register -> F64
        //   ARM/RISC-V: real in first FP register -> F32, imag in second FP register
        if ptr_count == 0 {
            if matches!(full_ret_ctype, CType::ComplexLongDouble) && self.is_x86() {
                // x86-64: _Complex long double returns real part in x87 st(0)
                ret_ty = IrType::F128;
            } else if matches!(full_ret_ctype, CType::ComplexDouble) {
                ret_ty = IrType::F64;
            } else if matches!(full_ret_ctype, CType::ComplexFloat) {
                if self.uses_packed_complex_float() {
                    ret_ty = IrType::F64;
                } else {
                    ret_ty = IrType::F32;
                }
            } else if matches!(full_ret_ctype, CType::ComplexLongDouble) && self.returns_complex_long_double_in_regs() {
                // x86-64: _Complex long double returned via x87 st(0)/st(1)
                // IR return type is F128 (real part); imag part via SetReturnF128Second
                ret_ty = IrType::F128;
            }
        }

        // Track CType for pointer-returning functions
        let return_ctype = if ret_ty == IrType::Ptr {
            Some(full_ret_ctype.clone())
        } else {
            None
        };

        // Record complex return types for expr_ctype resolution
        if ptr_count == 0 && full_ret_ctype.is_complex() {
            self.types.func_return_ctypes.insert(name.to_string(), full_ret_ctype.clone());
        }

        // Detect struct/complex returns that need special ABI handling.
        let mut sret_size = None;
        let mut two_reg_ret_size = None;
        if ptr_count == 0 {
            if full_ret_ctype.is_struct_or_union() {
                let size = self.sizeof_type(ret_type_spec);
                if size > 16 {
                    sret_size = Some(size);
                } else if size > 8 {
                    two_reg_ret_size = Some(size);
                }
            }
            if matches!(full_ret_ctype, CType::ComplexLongDouble) {
                // On x86-64, _Complex long double is returned via x87 st(0)/st(1),
                // not via hidden pointer (sret). On other targets, use sret.
                if !self.returns_complex_long_double_in_regs() {
                    let size = self.sizeof_type(ret_type_spec);
                    sret_size = Some(size);
                }
            }
        }

        // Collect parameter types, with K&R default argument promotions.
        // Use sema's param CTypes when available to avoid re-computing from AST.
        let sema_param_ctypes = self.sema_functions.get(name)
            .map(|fi| fi.params.iter().map(|(ct, _)| ct.clone()).collect::<Vec<_>>());

        let param_tys: Vec<IrType> = params.iter().enumerate().map(|(i, p)| {
            let ty = if let Some(ref sema_cts) = sema_param_ctypes {
                if let Some(ct) = sema_cts.get(i) {
                    IrType::from_ctype(ct)
                } else {
                    self.type_spec_to_ir(&p.type_spec)
                }
            } else {
                self.type_spec_to_ir(&p.type_spec)
            };
            if is_kr {
                match ty {
                    IrType::F32 => IrType::F64,
                    IrType::I8 | IrType::U8 | IrType::I16 | IrType::U16 => IrType::I32,
                    other => other,
                }
            } else { ty }
        }).collect();
        let param_bool_flags: Vec<bool> = params.iter().map(|p| {
            self.is_type_bool(&p.type_spec)
        }).collect();
        // Collect parameter CTypes for complex argument conversion.
        // Prefer sema's authoritative CTypes when available.
        let param_ctypes: Vec<CType> = if let Some(sema_cts) = sema_param_ctypes {
            params.iter().enumerate().map(|(i, p)| {
                if let Some(ct) = sema_cts.get(i) {
                    ct.clone()
                } else {
                    self.type_spec_to_ctype(&p.type_spec)
                }
            }).collect()
        } else {
            params.iter().map(|p| {
                self.type_spec_to_ctype(&p.type_spec)
            }).collect()
        };

        // Collect per-parameter struct sizes for by-value struct passing ABI.
        // ComplexLongDouble is included as a struct on platforms that don't decompose it
        // (x86-64, RISC-V) since it's passed like a struct (on stack / by reference).
        // Transparent unions are excluded — they are passed as their first member.
        let decomposes_cld = self.decomposes_complex_long_double();
        let param_struct_sizes: Vec<Option<usize>> = params.iter().map(|p| {
            if self.is_type_struct_or_union(&p.type_spec) && !self.is_transparent_union(&p.type_spec) {
                Some(self.sizeof_type(&p.type_spec))
            } else if !decomposes_cld && matches!(self.type_spec_to_ctype(&p.type_spec), CType::ComplexLongDouble) {
                Some(self.sizeof_type(&p.type_spec))
            } else {
                None
            }
        }).collect();

        // Compute per-eightbyte SysV ABI classification for struct params
        let param_struct_classes: Vec<Vec<crate::common::types::EightbyteClass>> = params.iter().enumerate().map(|(i, p)| {
            if param_struct_sizes.get(i).copied().flatten().is_some() {
                if let Some(layout) = self.get_struct_layout_for_type(&p.type_spec) {
                    layout.classify_sysv_eightbytes(&self.types)
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            }
        }).collect();

        let sig = if !variadic || !param_tys.is_empty() {
            FuncSig {
                return_type: ret_ty,
                return_ctype,
                param_types: param_tys,
                param_ctypes,
                param_bool_flags,
                is_variadic: variadic,
                sret_size,
                two_reg_ret_size,
                param_struct_sizes,
                param_struct_classes,
            }
        } else {
            FuncSig {
                return_type: ret_ty,
                return_ctype,
                param_types: Vec::new(),
                param_ctypes: Vec::new(),
                param_bool_flags: Vec::new(),
                is_variadic: variadic,
                sret_size,
                two_reg_ret_size,
                param_struct_sizes: Vec::new(),
                param_struct_classes: Vec::new(),
            }
        };
        self.func_meta.sigs.insert(name.to_string(), sig);
    }

    // --- IR emission helpers ---

    pub(super) fn fresh_value(&mut self) -> Value {
        let v = Value(self.func_mut().next_value);
        self.func_mut().next_value += 1;
        v
    }

    pub(super) fn fresh_label(&mut self) -> BlockId {
        let l = BlockId(self.next_label);
        self.next_label += 1;
        l
    }

    /// Intern a string literal: add it to the module's .rodata string table and
    /// return its unique label.
    pub(super) fn intern_string_literal(&mut self, s: &str) -> String {
        let label = format!(".Lstr{}", self.next_string);
        self.next_string += 1;
        self.module.string_literals.push((label.clone(), s.to_string()));
        label
    }

    /// Intern a wide string literal (L"...") and return its label.
    /// Each character is stored as a u32 (wchar_t), plus a null terminator.
    pub(super) fn intern_wide_string_literal(&mut self, s: &str) -> String {
        let label = format!(".Lwstr{}", self.next_string);
        self.next_string += 1;
        let mut chars: Vec<u32> = s.chars().map(|c| c as u32).collect();
        chars.push(0); // null terminator
        self.module.wide_string_literals.push((label.clone(), chars));
        label
    }

    /// Intern a char16_t string literal (u"...") and return its label.
    /// Each character is stored as a u16 (char16_t), plus a null terminator.
    pub(super) fn intern_char16_string_literal(&mut self, s: &str) -> String {
        let label = format!(".Lc16str{}", self.next_string);
        self.next_string += 1;
        let mut chars: Vec<u16> = s.chars().map(|c| c as u16).collect();
        chars.push(0); // null terminator
        self.module.char16_string_literals.push((label.clone(), chars));
        label
    }

    pub(super) fn emit(&mut self, inst: Instruction) {
        let span = self.func_mut().current_span;
        self.func_mut().instrs.push(inst);
        self.func_mut().instr_spans.push(span);
    }

    /// Emit an alloca into the entry block buffer.
    /// Used for local variable declarations so that variables whose
    /// declarations are skipped by `goto` still have valid stack slots.
    pub(super) fn emit_entry_alloca(&mut self, ty: IrType, size: usize, align: usize, volatile: bool) -> Value {
        let dest = self.fresh_value();
        self.func_mut().entry_allocas.push(Instruction::Alloca {
            dest, ty, size, align, volatile,
        });
        dest
    }

    /// Emit a binary operation and return the result Value.
    pub(super) fn emit_binop_val(&mut self, op: IrBinOp, lhs: Operand, rhs: Operand, ty: IrType) -> Value {
        let dest = self.fresh_value();
        self.emit(Instruction::BinOp { dest, op, lhs, rhs, ty });
        dest
    }

    /// Emit a comparison and return the result Value (I32: 0 or 1).
    pub(super) fn emit_cmp_val(&mut self, op: IrCmpOp, lhs: Operand, rhs: Operand, ty: IrType) -> Value {
        let dest = self.fresh_value();
        self.emit(Instruction::Cmp { dest, op, lhs, rhs, ty });
        dest
    }

    /// Emit a type cast and return the result Value.
    pub(super) fn emit_cast_val(&mut self, src: Operand, from_ty: IrType, to_ty: IrType) -> Value {
        let dest = self.fresh_value();
        self.emit(Instruction::Cast { dest, src, from_ty, to_ty });
        dest
    }

    /// Emit a GEP + Store: store `val` at `base + byte_offset` with the given type.
    pub(super) fn emit_store_at_offset(&mut self, base: Value, byte_offset: usize, val: Operand, ty: IrType) {
        let addr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: addr,
            base,
            offset: Operand::Const(IrConst::I64(byte_offset as i64)),
            ty,
        });
        self.emit(Instruction::Store { val, ptr: addr, ty , seg_override: AddressSpace::Default });
    }

    /// Lower an expression, cast to target type, then store at base + byte_offset.
    /// When target_is_bool is true, normalizes the value (any nonzero -> 1) per C11 6.3.1.2.
    pub(super) fn emit_init_expr_to_offset_bool(&mut self, e: &Expr, base: Value, byte_offset: usize, target_ty: IrType, target_is_bool: bool) {
        let expr_ty = self.get_expr_type(e);
        let val = self.lower_expr(e);
        let val = if target_is_bool {
            self.emit_bool_normalize_typed(val, expr_ty)
        } else {
            self.emit_implicit_cast(val, expr_ty, target_ty)
        };
        self.emit_store_at_offset(base, byte_offset, val, target_ty);
    }

    /// Emit a GEP to compute base + byte_offset and return the address Value.
    pub(super) fn emit_gep_offset(&mut self, base: Value, byte_offset: usize, ty: IrType) -> Value {
        let addr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: addr,
            base,
            offset: Operand::Const(IrConst::I64(byte_offset as i64)),
            ty,
        });
        addr
    }

    /// Emit memcpy from src to base + byte_offset.
    pub(super) fn emit_memcpy_at_offset(&mut self, base: Value, byte_offset: usize, src: Value, size: usize) {
        let dest = self.emit_gep_offset(base, byte_offset, IrType::Ptr);
        self.emit(Instruction::Memcpy { dest, src, size });
    }

    pub(super) fn terminate(&mut self, term: Terminator) {
        let block = BasicBlock {
            label: self.func_mut().current_label,
            instructions: std::mem::take(&mut self.func_mut().instrs),
            source_spans: std::mem::take(&mut self.func_mut().instr_spans),
            terminator: term,
        };
        self.func_mut().blocks.push(block);
    }

    pub(super) fn start_block(&mut self, label: BlockId) {
        self.func_mut().current_label = label;
        self.func_mut().instrs.clear();
        self.func_mut().instr_spans.clear();
    }

    // --- Enum and label helpers ---

    /// Collect enum constants from a type specifier.
    fn collect_enum_constants_impl(&mut self, ts: &TypeSpecifier, scoped: bool) {
        match ts {
            TypeSpecifier::Enum(name, Some(variants), is_packed) => {
                let mut next_val: i64 = 0;
                let mut variant_values = Vec::new();
                for variant in variants {
                    if let Some(ref expr) = variant.value {
                        if let Some(val) = self.eval_const_expr(expr) {
                            if let Some(v) = self.const_to_i64(&val) {
                                next_val = v;
                            }
                        }
                    }
                    if scoped {
                        self.insert_enum_scoped(variant.name.clone(), next_val);
                    } else {
                        self.types.enum_constants.insert(variant.name.clone(), next_val);
                    }
                    variant_values.push((variant.name.clone(), next_val));
                    next_val += 1;
                }
                // Store packed enum info for forward-reference lookups
                if *is_packed {
                    if let Some(tag) = name {
                        self.types.packed_enum_types.insert(
                            tag.clone(),
                            crate::common::types::EnumType {
                                name: Some(tag.clone()),
                                variants: variant_values,
                                is_packed: true,
                            },
                        );
                    }
                }
            }
            TypeSpecifier::Struct(_, Some(fields), _, _, _) | TypeSpecifier::Union(_, Some(fields), _, _, _) => {
                for field in fields {
                    self.collect_enum_constants_impl(&field.type_spec, scoped);
                }
            }
            TypeSpecifier::Array(inner, _) | TypeSpecifier::Pointer(inner, _) => {
                self.collect_enum_constants_impl(inner, scoped);
            }
            _ => {}
        }
    }

    /// Collect enum constants from a type specifier (file-scope, direct insertion).
    pub(super) fn collect_enum_constants(&mut self, ts: &TypeSpecifier) {
        self.collect_enum_constants_impl(ts, false);
    }

    /// Collect enum constants from a type specifier, using scoped insertion.
    pub(super) fn collect_enum_constants_scoped(&mut self, ts: &TypeSpecifier) {
        self.collect_enum_constants_impl(ts, true);
    }

    /// Check if a TypeSpecifier refers to an enum type (directly or via typedef).
    pub(super) fn is_enum_type_spec(&self, ts: &TypeSpecifier) -> bool {
        match ts {
            TypeSpecifier::Enum(..) => true,
            TypeSpecifier::TypedefName(name) => self.types.enum_typedefs.contains(name),
            TypeSpecifier::TypeofType(inner) => self.is_enum_type_spec(inner),
            _ => false,
        }
    }

    /// Get or create a unique IR label for a user-defined goto label.
    /// If the label name is declared via __label__ in a local scope,
    /// uses the scope-qualified name so that different invocations of
    /// a macro with __label__ don't collide.
    pub(super) fn get_or_create_user_label(&mut self, name: &str) -> BlockId {
        // Check local label scopes from innermost to outermost
        let resolved_name = self.resolve_local_label(name);
        let func_name = self.func_mut().name.clone();
        let key = format!("{}::{}", func_name, resolved_name);
        if let Some(&label) = self.func_mut().user_labels.get(&key) {
            label
        } else {
            let label = self.fresh_label();
            self.func_mut().user_labels.insert(key, label);
            label
        }
    }

    /// Resolve a label name through the local label scope stack.
    /// Returns a scope-qualified name if the label is declared via __label__,
    /// or the original name if not.
    fn resolve_local_label(&self, name: &str) -> String {
        // Search scopes from innermost to outermost
        for scope in self.local_label_scopes.iter().rev() {
            if let Some(qualified) = scope.get(name) {
                return qualified.clone();
            }
        }
        name.to_string()
    }

    // --- String and array init helpers ---

    /// Copy a string literal's bytes into an alloca at a given byte offset,
    /// followed by a null terminator.
    pub(super) fn emit_string_to_alloca(&mut self, alloca: Value, s: &str, base_offset: usize) {
        let str_bytes: Vec<u8> = s.chars().map(|c| c as u8).collect();
        for (j, &byte) in str_bytes.iter().enumerate() {
            let val = Operand::Const(IrConst::I8(byte as i8));
            let offset = Operand::Const(IrConst::I64((base_offset + j) as i64));
            let addr = self.fresh_value();
            self.emit(Instruction::GetElementPtr {
                dest: addr, base: alloca, offset, ty: IrType::I8,
            });
            self.emit(Instruction::Store { val, ptr: addr, ty: IrType::I8 , seg_override: AddressSpace::Default });
        }
        // Null terminator
        let null_offset = Operand::Const(IrConst::I64((base_offset + str_bytes.len()) as i64));
        let null_addr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: null_addr, base: alloca, offset: null_offset, ty: IrType::I8,
        });
        self.emit(Instruction::Store { val: Operand::Const(IrConst::I8(0)), ptr: null_addr, ty: IrType::I8,
         seg_override: AddressSpace::Default });
    }

    /// Emit a wide string (L"...") to a local alloca. Each character is stored as I32 (wchar_t).
    pub(super) fn emit_wide_string_to_alloca(&mut self, alloca: Value, s: &str, base_offset: usize) {
        for (j, ch) in s.chars().enumerate() {
            let val = Operand::Const(IrConst::I32(ch as i32));
            let byte_offset = base_offset + j * 4;
            let offset = Operand::Const(IrConst::I64(byte_offset as i64));
            let addr = self.fresh_value();
            self.emit(Instruction::GetElementPtr {
                dest: addr, base: alloca, offset, ty: IrType::I8,
            });
            self.emit(Instruction::Store { val, ptr: addr, ty: IrType::I32 , seg_override: AddressSpace::Default });
        }
        // Null terminator
        let null_byte_offset = base_offset + s.chars().count() * 4;
        let null_offset = Operand::Const(IrConst::I64(null_byte_offset as i64));
        let null_addr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: null_addr, base: alloca, offset: null_offset, ty: IrType::I8,
        });
        self.emit(Instruction::Store { val: Operand::Const(IrConst::I32(0)), ptr: null_addr, ty: IrType::I32,
         seg_override: AddressSpace::Default });
    }

    /// Emit a char16_t string (u"...") to a local alloca. Each character is stored as U16.
    pub(super) fn emit_char16_string_to_alloca(&mut self, alloca: Value, s: &str, base_offset: usize) {
        for (j, ch) in s.chars().enumerate() {
            let val = Operand::Const(IrConst::I16(ch as u16 as i16));
            let byte_offset = base_offset + j * 2;
            let offset = Operand::Const(IrConst::I64(byte_offset as i64));
            let addr = self.fresh_value();
            self.emit(Instruction::GetElementPtr {
                dest: addr, base: alloca, offset, ty: IrType::I8,
            });
            self.emit(Instruction::Store { val, ptr: addr, ty: IrType::U16, seg_override: AddressSpace::Default });
        }
        // Null terminator
        let null_byte_offset = base_offset + s.chars().count() * 2;
        let null_offset = Operand::Const(IrConst::I64(null_byte_offset as i64));
        let null_addr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: null_addr, base: alloca, offset: null_offset, ty: IrType::I8,
        });
        self.emit(Instruction::Store { val: Operand::Const(IrConst::I16(0)), ptr: null_addr, ty: IrType::U16,
         seg_override: AddressSpace::Default });
    }

    /// Emit a single element store at a given byte offset in an alloca.
    pub(super) fn emit_array_element_store(
        &mut self, alloca: Value, val: Operand, offset: usize, ty: IrType,
    ) {
        let offset_val = Operand::Const(IrConst::I64(offset as i64));
        let elem_addr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: elem_addr, base: alloca, offset: offset_val, ty,
        });
        self.emit(Instruction::Store { val, ptr: elem_addr, ty , seg_override: AddressSpace::Default });
    }

    /// Zero-initialize a region of memory within an alloca at the given byte offset.
    pub(super) fn zero_init_region(&mut self, alloca: Value, base_offset: usize, region_size: usize) {
        let mut offset = base_offset;
        let end = base_offset + region_size;
        while offset + 8 <= end {
            let addr = self.fresh_value();
            self.emit(Instruction::GetElementPtr {
                dest: addr,
                base: alloca,
                offset: Operand::Const(IrConst::I64(offset as i64)),
                ty: IrType::I64,
            });
            self.emit(Instruction::Store { val: Operand::Const(IrConst::I64(0)), ptr: addr, ty: IrType::I64,
             seg_override: AddressSpace::Default });
            offset += 8;
        }
        while offset < end {
            let addr = self.fresh_value();
            self.emit(Instruction::GetElementPtr {
                dest: addr,
                base: alloca,
                offset: Operand::Const(IrConst::I64(offset as i64)),
                ty: IrType::I8,
            });
            self.emit(Instruction::Store { val: Operand::Const(IrConst::I8(0)), ptr: addr, ty: IrType::I8,
             seg_override: AddressSpace::Default });
            offset += 1;
        }
    }

    /// Zero-initialize an entire alloca.
    pub(super) fn zero_init_alloca(&mut self, alloca: Value, total_size: usize) {
        self.zero_init_region(alloca, 0, total_size);
    }

}
