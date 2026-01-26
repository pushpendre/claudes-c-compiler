//! Core lowering logic: AST -> alloca-based IR.
//!
//! This file contains the `Lowerer` struct and its primary methods:
//! - Construction and top-level orchestration (`lower()`)
//! - Function lowering pipeline (build IR params, allocate locals, finalize)
//! - Global declaration lowering
//! - Declaration analysis (shared between local and global paths)
//! - IR emission helpers (fresh_value, emit, terminate, etc.)
//! - Scope management delegation
//!
//! Data structure definitions are in `definitions.rs`, per-function state in
//! `func_state.rs`, and type-system state in `frontend::sema::type_context`.

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

    /// Returns true if the target is x86-64.
    /// Used to select x87 80-bit extended precision format for long double memory layout.
    pub(super) fn is_x86(&self) -> bool {
        self.target == Target::X86_64
    }

    /// Returns true if the target uses x86-64 style packed _Complex float ABI
    /// (two F32s packed into a single F64/xmm register).
    /// Returns false for ARM/RISC-V which pass _Complex float as two separate F32 registers.
    pub(super) fn uses_packed_complex_float(&self) -> bool {
        self.target == Target::X86_64
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
                    } else if declarator.derived.is_empty() || !declarator.derived.iter().any(|d|
                        matches!(d, DerivedDeclarator::Function(_, _) | DerivedDeclarator::FunctionPointer(_, _)))
                    {
                        // Check if the base type is a function typedef
                        // (e.g., `func_t add;` where func_t is typedef int func_t(int);)
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
                        // Collect weak/visibility attributes on extern declarations (not aliases)
                        if declarator.alias_target.is_none()
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
                    if can_skip && !referenced_statics.contains(&func.name) {
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
            if matches!(full_ret_ctype, CType::ComplexDouble) {
                ret_ty = IrType::F64;
            } else if matches!(full_ret_ctype, CType::ComplexFloat) {
                if self.uses_packed_complex_float() {
                    ret_ty = IrType::F64;
                } else {
                    ret_ty = IrType::F32;
                }
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
                let size = self.sizeof_type(ret_type_spec);
                sret_size = Some(size);
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
        self.func_mut().instrs.push(inst);
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
            terminator: term,
        };
        self.func_mut().blocks.push(block);
    }

    pub(super) fn start_block(&mut self, label: BlockId) {
        self.func_mut().current_label = label;
        self.func_mut().instrs.clear();
    }

    // --- Function lowering pipeline ---

    /// Lower a function definition to IR.
    ///
    /// Orchestrates the function lowering pipeline:
    /// 1. Set up return type (handling sret, two-reg, complex ABI overrides)
    /// 2. Build IR parameter list (decomposing complex params for ABI)
    /// 3. Allocate parameters as locals (3-phase: basic allocas, struct setup, complex reconstruction)
    /// 4. Handle K&R float promotion
    /// 5. Lower the function body
    /// 6. Finalize (implicit return, emit IrFunction)
    fn lower_function(&mut self, func: &FunctionDef) {
        if self.defined_functions.contains(&func.name) {
            return;
        }
        self.defined_functions.insert(func.name.clone());

        let return_is_bool = self.is_type_bool(&func.return_type);
        let base_return_type = self.type_spec_to_ir(&func.return_type);
        self.func_state = Some(FunctionBuildState::new(
            func.name.clone(), base_return_type, return_is_bool,
        ));
        self.push_scope();

        // Step 1: Compute ABI-adjusted return type
        let return_type = self.compute_function_return_type(func);
        self.func_mut().return_type = return_type;

        // Step 2: Build IR parameter list with ABI decomposition
        let param_info = self.build_ir_params(func);

        // Step 3: Allocate parameters as locals (3-phase)
        let entry_label = self.fresh_label();
        self.start_block(entry_label);
        self.func_mut().sret_ptr = None;
        self.allocate_function_params(func, &param_info);

        // Step 4: K&R float promotion
        self.handle_kr_float_promotion(func);

        // Step 5: Lower body
        self.lower_compound_stmt(&func.body);

        // Step 6: Finalize
        self.finalize_function(func, return_type, param_info.params);
    }

    /// Compute the IR return type for a function, applying ABI overrides.
    fn compute_function_return_type(&mut self, func: &FunctionDef) -> IrType {
        // Record complex return type for expr_ctype resolution
        let ret_ctype = self.type_spec_to_ctype(&func.return_type);
        if ret_ctype.is_complex() {
            self.types.func_return_ctypes.insert(func.name.clone(), ret_ctype.clone());
        }

        // Use the return type from the already-registered signature, which has
        // complex ABI overrides applied (e.g., ComplexDouble -> F64, ComplexFloat -> F64/F32).
        if let Some(sig) = self.func_meta.sigs.get(&func.name) {
            // Two-register struct returns (9-16 bytes) are packed into I128 by the
            // IR lowering (Shl+Or), so the function's return type must be I128 to
            // ensure the codegen uses the register-pair return path (a0+a1).
            if sig.two_reg_ret_size.is_some() {
                return IrType::I128;
            }
            return sig.return_type;
        }

        // Fallback: compute directly (shouldn't normally be reached)
        self.type_spec_to_ir(&func.return_type)
    }

    /// Build the IR parameter list for a function, handling ABI decomposition.
    fn build_ir_params(&mut self, func: &FunctionDef) -> IrParamBuildResult {
        let mut params: Vec<IrParam> = Vec::new();
        let mut param_kinds: Vec<ParamKind> = Vec::new();
        let mut uses_sret = false;

        // Check if function returns a large struct via sret
        if let Some(sig) = self.func_meta.sigs.get(&func.name) {
            if let Some(sret_size) = sig.sret_size {
                params.push(IrParam { name: String::new(), ty: IrType::Ptr, struct_size: None });
                uses_sret = true;
                let _ = sret_size; // used for alloca sizing in allocate_function_params
            }
        }

        for (_orig_idx, param) in func.params.iter().enumerate() {
            let param_name = param.name.clone().unwrap_or_default();
            let param_ctype = self.type_spec_to_ctype(&param.type_spec);

            // Complex parameter decomposition
            let decompose_cld = self.decomposes_complex_long_double();
            if param_ctype.is_complex() {
                // ComplexLongDouble: only decompose on ARM64 (HFA in Q regs);
                // on x86-64/RISC-V it's passed as a struct (on stack / by reference).
                if matches!(param_ctype, CType::ComplexLongDouble) && !decompose_cld {
                    // Fall through to struct handling below
                } else if matches!(param_ctype, CType::ComplexFloat) && self.uses_packed_complex_float() {
                    // x86-64: _Complex float packed into single F64
                    let ir_idx = params.len();
                    params.push(IrParam { name: param_name, ty: IrType::F64, struct_size: None });
                    param_kinds.push(ParamKind::ComplexFloatPacked(ir_idx));
                    continue;
                } else {
                    // Decompose into two FP params (ComplexFloat/ComplexDouble on all,
                    // ComplexLongDouble on ARM64 only)
                    let comp_ty = Self::complex_component_ir_type(&param_ctype);
                    let real_idx = params.len();
                    params.push(IrParam { name: format!("{}.real", param_name), ty: comp_ty, struct_size: None });
                    let imag_idx = params.len();
                    params.push(IrParam { name: format!("{}.imag", param_name), ty: comp_ty, struct_size: None });
                    param_kinds.push(ParamKind::ComplexDecomposed(real_idx, imag_idx));
                    continue;
                }
            }

            // Struct/union parameter (pass by value), including ComplexLongDouble
            // on x86-64/RISC-V where it's not decomposed.
            // Transparent unions are passed as their first member (a pointer),
            // not as a by-value aggregate, so struct_size is None for them.
            if self.is_type_struct_or_union(&param.type_spec)
                || matches!(param_ctype, CType::ComplexLongDouble)
            {
                let ir_idx = params.len();
                let struct_size = if self.is_transparent_union(&param.type_spec) {
                    None
                } else {
                    Some(self.sizeof_type(&param.type_spec))
                };
                params.push(IrParam { name: param_name, ty: IrType::Ptr, struct_size });
                param_kinds.push(ParamKind::Struct(ir_idx));
                continue;
            }

            // Normal scalar parameter
            let ir_idx = params.len();
            let mut ty = self.type_spec_to_ir(&param.type_spec);
            // K&R default argument promotions: float->double, char/short->int
            if func.is_kr {
                ty = match ty {
                    IrType::F32 => IrType::F64,
                    IrType::I8 | IrType::U8 | IrType::I16 | IrType::U16 => IrType::I32,
                    other => other,
                };
            }
            params.push(IrParam { name: param_name, ty, struct_size: None });
            param_kinds.push(ParamKind::Normal(ir_idx));
        }

        IrParamBuildResult { params, param_kinds, uses_sret }
    }

    /// Allocate function parameters as local variables (3-phase process).
    fn allocate_function_params(&mut self, func: &FunctionDef, info: &IrParamBuildResult) {
        // Phase 1: Emit allocas for all IR params
        let mut ir_allocas: Vec<Value> = Vec::new();
        for param in &info.params {
            let alloca = self.fresh_value();
            if info.uses_sret && ir_allocas.is_empty() {
                self.emit(Instruction::Alloca { dest: alloca, ty: IrType::Ptr, size: 8, align: 0, volatile: false });
                self.func_mut().sret_ptr = Some(alloca);
                ir_allocas.push(alloca);
                continue;
            }
            let size = param.ty.size().max(param.struct_size.unwrap_or(param.ty.size()));
            self.emit(Instruction::Alloca { dest: alloca, ty: param.ty, size, align: 0, volatile: false });
            ir_allocas.push(alloca);
        }

        // Save param alloca values for the backend to detect unused param allocas.
        self.func_mut().param_alloca_values = ir_allocas.clone();

        // Phase 2 & 3: Process each original parameter by kind
        for (orig_idx, kind) in info.param_kinds.iter().enumerate() {
            let orig_param = &func.params[orig_idx];
            match kind {
                ParamKind::Normal(ir_idx) => {
                    self.register_normal_param(orig_param, &info.params[*ir_idx], ir_allocas[*ir_idx]);
                }
                ParamKind::Struct(ir_idx) => {
                    self.register_struct_or_complex_param(orig_param, ir_allocas[*ir_idx]);
                }
                ParamKind::ComplexFloatPacked(ir_idx) => {
                    self.register_packed_complex_float_param(orig_param, ir_allocas[*ir_idx]);
                }
                ParamKind::ComplexDecomposed(real_ir_idx, imag_ir_idx) => {
                    self.reconstruct_decomposed_complex_param(
                        orig_param, ir_allocas[*real_ir_idx], ir_allocas[*imag_ir_idx],
                    );
                }
            }
        }

        self.compute_vla_param_strides(func);
    }

    /// Register a normal (non-struct, non-complex) parameter as a local variable.
    fn register_normal_param(&mut self, orig_param: &ParamDecl, ir_param: &IrParam, alloca: Value) {
        let ty = ir_param.ty;
        let param_size = self.sizeof_type(&orig_param.type_spec).max(ty.size());
        let elem_size = if ty == IrType::Ptr { self.pointee_elem_size(&orig_param.type_spec) } else { 0 };
        let pointee_type = if ty == IrType::Ptr { self.pointee_ir_type(&orig_param.type_spec) } else { None };
        let struct_layout = if ty == IrType::Ptr { self.get_struct_layout_for_pointer_param(&orig_param.type_spec) } else { None };
        let c_type = Some(self.param_ctype(orig_param));
        let is_bool = self.is_type_bool(&orig_param.type_spec);
        let array_dim_strides = if ty == IrType::Ptr { self.compute_ptr_array_strides(&orig_param.type_spec) } else { vec![] };

        // Detect pointer-to-function-pointer parameters: these have fptr_params
        // AND 2+ pointer levels in the type_spec (e.g., int (**fpp)(int, int)
        // has type_spec = Pointer(Pointer(Int)), yielding CType depth >= 2).
        // This is needed because our CType representation can conflate
        // pointer-to-function-pointers with direct function pointers.
        let is_ptr_to_func_ptr = if orig_param.fptr_params.is_some() {
            let ct = self.type_spec_to_ctype(&orig_param.type_spec);
            let mut depth = 0usize;
            let mut t = &ct;
            while let CType::Pointer(inner, _) = t {
                depth += 1;
                t = inner.as_ref();
            }
            depth >= 2
        } else {
            false
        };

        let name = orig_param.name.clone().unwrap_or_default();
        self.insert_local_scoped(name, LocalInfo {
            var: VarInfo { ty, elem_size, is_array: false, pointee_type, struct_layout, is_struct: false, array_dim_strides, c_type, is_ptr_to_func_ptr, address_space: AddressSpace::Default },
            alloca, alloc_size: param_size, is_bool, static_global_name: None, vla_strides: vec![], vla_size: None, asm_register: None, cleanup_fn: None,
            is_const: orig_param.is_const,
        });

        // Register function pointer parameter signatures for indirect calls
        if let Some(ref fptr_params) = orig_param.fptr_params {
            let ret_ty = match &orig_param.type_spec {
                TypeSpecifier::Pointer(inner, _) => self.type_spec_to_ir(inner),
                _ => self.type_spec_to_ir(&orig_param.type_spec),
            };
            if let Some(ref name) = orig_param.name {
                let param_tys: Vec<IrType> = fptr_params.iter().map(|fp| self.type_spec_to_ir(&fp.type_spec)).collect();
                self.func_meta.ptr_sigs.insert(name.clone(), FuncSig::for_ptr(ret_ty, param_tys));
            }
        }
    }

    /// Register a struct/union or non-decomposed complex parameter as a local variable.
    fn register_struct_or_complex_param(&mut self, orig_param: &ParamDecl, alloca: Value) {
        let is_struct = self.is_type_struct_or_union(&orig_param.type_spec);
        let layout = if is_struct { self.get_struct_layout_for_type(&orig_param.type_spec) } else { None };
        let size = if is_struct { layout.as_ref().map_or(8, |l| l.size) } else { self.sizeof_type(&orig_param.type_spec) };
        let c_type = Some(self.type_spec_to_ctype(&orig_param.type_spec));

        let name = orig_param.name.clone().unwrap_or_default();
        self.insert_local_scoped(name, LocalInfo {
            var: VarInfo { ty: IrType::Ptr, elem_size: 0, is_array: false, pointee_type: None, struct_layout: layout, is_struct: true, array_dim_strides: vec![], c_type, is_ptr_to_func_ptr: false, address_space: AddressSpace::Default },
            alloca, alloc_size: size, is_bool: false, static_global_name: None, vla_strides: vec![], vla_size: None, asm_register: None, cleanup_fn: None,
            is_const: orig_param.is_const,
        });
    }

    /// Register a packed complex float parameter (x86-64 only) as a local variable.
    fn register_packed_complex_float_param(&mut self, orig_param: &ParamDecl, alloca: Value) {
        let ct = self.type_spec_to_ctype(&orig_param.type_spec);
        let name = orig_param.name.clone().unwrap_or_default();
        self.insert_local_scoped(name, LocalInfo {
            var: VarInfo { ty: IrType::Ptr, elem_size: 0, is_array: false, pointee_type: None, struct_layout: None, is_struct: true, array_dim_strides: vec![], c_type: Some(ct), is_ptr_to_func_ptr: false, address_space: AddressSpace::Default },
            alloca, alloc_size: 8, is_bool: false, static_global_name: None, vla_strides: vec![], vla_size: None, asm_register: None, cleanup_fn: None,
            is_const: orig_param.is_const,
        });
    }

    /// Reconstruct a decomposed complex parameter from its real/imag Phase 1 allocas.
    fn reconstruct_decomposed_complex_param(&mut self, orig_param: &ParamDecl, real_alloca: Value, imag_alloca: Value) {
        let ct = self.type_spec_to_ctype(&orig_param.type_spec);
        let comp_ty = Self::complex_component_ir_type(&ct);
        let comp_size = Self::complex_component_size(&ct);
        let complex_size = ct.size();

        let complex_alloca = self.fresh_value();
        self.emit(Instruction::Alloca { dest: complex_alloca, ty: IrType::Ptr, size: complex_size, align: 0, volatile: false });

        let real_val = self.fresh_value();
        self.emit(Instruction::Load { dest: real_val, ptr: real_alloca, ty: comp_ty , seg_override: AddressSpace::Default });
        self.emit(Instruction::Store { val: Operand::Value(real_val), ptr: complex_alloca, ty: comp_ty , seg_override: AddressSpace::Default });

        let imag_val = self.fresh_value();
        self.emit(Instruction::Load { dest: imag_val, ptr: imag_alloca, ty: comp_ty , seg_override: AddressSpace::Default });
        let imag_ptr = self.fresh_value();
        self.emit(Instruction::GetElementPtr { dest: imag_ptr, base: complex_alloca, offset: Operand::Const(IrConst::I64(comp_size as i64)), ty: IrType::I8 });
        self.emit(Instruction::Store { val: Operand::Value(imag_val), ptr: imag_ptr, ty: comp_ty , seg_override: AddressSpace::Default });

        let name = orig_param.name.clone().unwrap_or_default();
        self.func_mut().locals.insert(name, LocalInfo {
            var: VarInfo { ty: IrType::Ptr, elem_size: 0, is_array: false, pointee_type: None, struct_layout: None, is_struct: true, array_dim_strides: vec![], c_type: Some(ct), is_ptr_to_func_ptr: false, address_space: AddressSpace::Default },
            alloca: complex_alloca, alloc_size: complex_size, is_bool: false, static_global_name: None, vla_strides: vec![], vla_size: None, asm_register: None, cleanup_fn: None,
            is_const: orig_param.is_const,
        });
    }

    /// Handle K&R default argument promotions: narrow promoted params back to declared types.
    /// float->double promotion: narrow double back to float.
    /// char/short->int promotion: narrow int back to char/short.
    fn handle_kr_float_promotion(&mut self, func: &FunctionDef) {
        if !func.is_kr { return; }
        for param in &func.params {
            let declared_ty = self.type_spec_to_ir(&param.type_spec);
            let name = param.name.clone().unwrap_or_default();
            let local_info = match self.func_mut().locals.get(&name).cloned() { Some(i) => i, None => continue };
            match declared_ty {
                IrType::F32 => {
                    // Received as F64, narrow to F32
                    let f64_val = self.fresh_value();
                    self.emit(Instruction::Load { dest: f64_val, ptr: local_info.alloca, ty: IrType::F64 , seg_override: AddressSpace::Default });
                    let f32_val = self.emit_cast_val(Operand::Value(f64_val), IrType::F64, IrType::F32);
                    let f32_alloca = self.fresh_value();
                    self.emit(Instruction::Alloca { dest: f32_alloca, ty: IrType::F32, size: 4, align: 0, volatile: false });
                    self.emit(Instruction::Store { val: Operand::Value(f32_val), ptr: f32_alloca, ty: IrType::F32 , seg_override: AddressSpace::Default });
                    if let Some(local) = self.func_mut().locals.get_mut(&name) {
                        local.alloca = f32_alloca; local.ty = IrType::F32; local.alloc_size = 4;
                    }
                }
                IrType::I8 | IrType::U8 | IrType::I16 | IrType::U16 => {
                    // Received as I32, narrow to declared type
                    let i32_val = self.fresh_value();
                    self.emit(Instruction::Load { dest: i32_val, ptr: local_info.alloca, ty: IrType::I32 , seg_override: AddressSpace::Default });
                    let narrow_val = self.emit_cast_val(Operand::Value(i32_val), IrType::I32, declared_ty);
                    let narrow_alloca = self.fresh_value();
                    let size = declared_ty.size().max(1);
                    self.emit(Instruction::Alloca { dest: narrow_alloca, ty: declared_ty, size, align: 0, volatile: false });
                    self.emit(Instruction::Store { val: Operand::Value(narrow_val), ptr: narrow_alloca, ty: declared_ty , seg_override: AddressSpace::Default });
                    if let Some(local) = self.func_mut().locals.get_mut(&name) {
                        local.alloca = narrow_alloca; local.ty = declared_ty; local.alloc_size = size;
                    }
                }
                _ => {}
            }
        }
    }

    /// Finalize a function: add implicit return, build IrFunction, push to module.
    fn finalize_function(&mut self, func: &FunctionDef, return_type: IrType, params: Vec<IrParam>) {
        if !self.func_mut().instrs.is_empty() || self.func_mut().blocks.is_empty()
           || !matches!(self.func_mut().blocks.last().map(|b| &b.terminator), Some(Terminator::Return(_)))
        {
            let ret_op = if return_type == IrType::Void { None } else { Some(Operand::Const(IrConst::I32(0))) };
            self.terminate(Terminator::Return(ret_op));
        }

        // Merge deferred entry-block allocas into blocks[0], right after
        // the existing parameter allocas. mem2reg skips the first N allocas
        // (where N = param count) so deferred local allocas must come after them.
        let entry_allocas = std::mem::take(&mut self.func_mut().entry_allocas);
        if !entry_allocas.is_empty() {
            if let Some(entry_block) = self.func_mut().blocks.first_mut() {
                // Find the insertion point: right after the last Alloca instruction
                // in the entry block (these are parameter allocas).
                let insert_pos = entry_block.instructions.iter()
                    .rposition(|inst| matches!(inst, Instruction::Alloca { .. }))
                    .map(|pos| pos + 1)
                    .unwrap_or(0);
                // Splice deferred allocas at the insertion point.
                entry_block.instructions.splice(insert_pos..insert_pos, entry_allocas);
            }
        }

        // extern inline with __attribute__((gnu_inline)) should be treated as local
        // (no external definition emitted), even if the function is referenced and lowered.
        // But plain inline + gnu_inline (without extern) provides an external definition
        // in GNU89 semantics, so it should be GLOBAL.
        let is_gnu_inline_no_extern_def = func.is_gnu_inline && func.is_inline
            && func.is_extern;
        let is_static = func.is_static || self.static_functions.contains(&func.name)
            || is_gnu_inline_no_extern_def;
        let next_val = self.func_mut().next_value;
        let param_alloca_vals = std::mem::take(&mut self.func_mut().param_alloca_values);
        let ir_func = IrFunction {
            name: func.name.clone(), return_type, params,
            blocks: std::mem::take(&mut self.func_mut().blocks),
            is_variadic: func.variadic, is_declaration: false, is_static,
            is_inline: func.is_inline,
            is_always_inline: func.is_always_inline,
            is_noinline: func.is_noinline,
            stack_size: 0,
            next_value_id: next_val,
            section: func.section.clone(),
            visibility: func.visibility.clone(),
            is_weak: func.is_weak,
            has_inlined_calls: false,
            param_alloca_values: param_alloca_vals,
        };
        self.module.functions.push(ir_func);
        self.pop_scope();
        self.func_state = None;
    }

    /// For pointer-to-array function parameters with VLA (runtime) dimensions,
    /// compute strides at runtime and store them in the LocalInfo.
    fn compute_vla_param_strides(&mut self, func: &FunctionDef) {
        // Collect VLA info first, then emit code (avoids borrow issues)
        let mut vla_params: Vec<(String, Vec<VlaDimInfo>)> = Vec::new();

        for param in &func.params {
            let param_name = match &param.name {
                Some(n) => n.clone(),
                None => continue,
            };

            // Check if this parameter is a pointer-to-array with VLA dimensions
            let ts = self.resolve_type_spec(&param.type_spec);
            if let TypeSpecifier::Pointer(inner, _) = ts {
                let dim_infos = self.collect_vla_dims(inner);
                if dim_infos.iter().any(|d| d.is_vla) {
                    vla_params.push((param_name, dim_infos));
                }
            } else {
                // Check CType for typedef'd pointer-to-array
                let ctype = self.type_spec_to_ctype(&param.type_spec);
                if let CType::Pointer(ref inner_ct, _) = ctype {
                    if matches!(inner_ct.as_ref(), CType::Array(_, _)) {
                        // TypeSpecifier-based VLA detection won't work for typedef'd types,
                        // but VLA dimensions in typedef'd pointers are rare
                    }
                }
            }
        }

        // Now emit runtime stride computations
        for (param_name, dim_infos) in vla_params {
            let num_strides = dim_infos.len() + 1; // +1 for base element size
            let mut vla_strides: Vec<Option<Value>> = vec![None; num_strides];

            let base_elem_size = dim_infos.last().map_or(1, |d| d.base_elem_size);
            let mut current_stride: Option<Value> = None;
            let mut current_const_stride = base_elem_size;

            // Process dimensions from innermost to outermost
            for (i, dim_info) in dim_infos.iter().enumerate().rev() {
                if dim_info.is_vla {
                    let dim_val = self.load_vla_dim_value(&dim_info.dim_expr_name);
                    let stride_val = if let Some(prev) = current_stride {
                        self.emit_binop_val(IrBinOp::Mul, Operand::Value(dim_val), Operand::Value(prev), IrType::I64)
                    } else {
                        self.emit_binop_val(IrBinOp::Mul, Operand::Value(dim_val), Operand::Const(IrConst::I64(current_const_stride as i64)), IrType::I64)
                    };
                    vla_strides[i] = Some(stride_val);
                    current_stride = Some(stride_val);
                    current_const_stride = 0;
                } else {
                    let const_dim = dim_info.const_size.unwrap_or(1) as usize;
                    if let Some(prev) = current_stride {
                        let result = self.emit_binop_val(IrBinOp::Mul, Operand::Value(prev), Operand::Const(IrConst::I64(const_dim as i64)), IrType::I64);
                        vla_strides[i] = Some(result);
                        current_stride = Some(result);
                    } else {
                        current_const_stride *= const_dim;
                    }
                }
            }

            if let Some(local) = self.func_mut().locals.get_mut(&param_name) {
                local.vla_strides = vla_strides;
            }
        }
    }

    /// Load the value of a VLA dimension variable (a function parameter).
    fn load_vla_dim_value(&mut self, dim_name: &str) -> Value {
        if let Some(info) = self.func_mut().locals.get(dim_name).cloned() {
            let loaded = self.fresh_value();
            self.emit(Instruction::Load { dest: loaded, ptr: info.alloca, ty: info.ty,
             seg_override: AddressSpace::Default });
            loaded
        } else {
            // Fallback: use constant 1
            let val = self.fresh_value();
            self.emit(Instruction::Copy {
                dest: val,
                src: Operand::Const(IrConst::I64(1)),
            });
            val
        }
    }

    /// Collect VLA dimension information from a pointer-to-array type.
    fn collect_vla_dims(&self, inner: &TypeSpecifier) -> Vec<VlaDimInfo> {
        let mut dims = Vec::new();
        let mut current = inner;
        loop {
            let resolved = self.resolve_type_spec(current);
            if let TypeSpecifier::Array(elem, size_expr) = resolved {
                let (is_vla, dim_name, const_size) = if let Some(expr) = size_expr {
                    if let Some(val) = self.expr_as_array_size(expr) {
                        (false, String::new(), Some(val))
                    } else {
                        let name = Self::extract_dim_expr_name(expr);
                        (true, name, None)
                    }
                } else {
                    (false, String::new(), None)
                };

                let base_elem_size = self.sizeof_type(elem);

                dims.push(VlaDimInfo {
                    is_vla,
                    dim_expr_name: dim_name,
                    const_size,
                    base_elem_size,
                });
                current = elem;
            } else {
                break;
            }
        }
        dims
    }

    /// Extract variable name from a VLA dimension expression.
    fn extract_dim_expr_name(expr: &Expr) -> String {
        match expr {
            Expr::Identifier(name, _) => name.clone(),
            _ => String::new(),
        }
    }

    // --- Global declaration lowering ---

    fn lower_global_decl(&mut self, decl: &Declaration) {
        // Register any struct/union definitions
        self.register_struct_type(&decl.type_spec);

        // Collect enum constants from top-level enum type declarations
        self.collect_enum_constants(&decl.type_spec);

        // If this is a typedef, register the mapping and skip variable emission
        if decl.is_typedef {
            // Check if this typedef aliases an enum type (directly or transitively).
            // Used to treat enum-typedef bitfields as unsigned (GCC compat).
            let is_enum_type = self.is_enum_type_spec(&decl.type_spec);
            for declarator in &decl.declarators {
                if !declarator.name.is_empty() {
                    let resolved_ctype = self.build_full_ctype(&decl.type_spec, &declarator.derived);
                    self.types.typedefs.insert(declarator.name.clone(), resolved_ctype);
                    if is_enum_type && declarator.derived.is_empty() {
                        self.types.enum_typedefs.insert(declarator.name.clone());
                    }
                    // Preserve alignment override from __attribute__((aligned(N))) on the typedef.
                    // Recompute sizeof for aligned(sizeof(type)) with full layout info.
                    let effective_alignment = {
                        let mut align = decl.alignment;
                        if let Some(ref sizeof_ts) = decl.alignment_sizeof_type {
                            let real_sizeof = self.sizeof_type(sizeof_ts);
                            align = Some(align.map_or(real_sizeof, |a| a.max(real_sizeof)));
                        }
                        align.or_else(|| {
                            decl.alignas_type.as_ref().map(|ts| self.alignof_type(ts))
                        })
                    };
                    if let Some(align) = effective_alignment {
                        self.types.typedef_alignments.insert(declarator.name.clone(), align);
                    }
                }
            }
            // Mark transparent_union on the union's StructLayout
            if decl.is_transparent_union {
                self.mark_transparent_union(decl);
            }
            return;
        }

        for declarator in &decl.declarators {
            if declarator.name.is_empty() {
                continue;
            }

            // Skip function declarations (prototypes), but NOT function pointer variables.
            if declarator.derived.iter().any(|d| matches!(d, DerivedDeclarator::Function(_, _)))
                && !declarator.derived.iter().any(|d| matches!(d, DerivedDeclarator::FunctionPointer(_, _)))
                && declarator.init.is_none()
            {
                continue;
            }

            // Skip declarations using function typedefs
            if declarator.init.is_none() {
                if let TypeSpecifier::TypedefName(tname) = &decl.type_spec {
                    if self.types.function_typedefs.contains_key(tname) {
                        continue;
                    }
                }
            }

            // Global register variable: `register <type> <name> __asm__("reg")`
            // No storage is emitted; reads/writes map directly to the named register.
            if let Some(ref reg_name) = declarator.asm_register {
                let da = self.analyze_declaration(&decl.type_spec, &declarator.derived);
                let mut ginfo = GlobalInfo::from_analysis(&da);
                ginfo.asm_register = Some(reg_name.clone());
                ginfo.var.address_space = decl.address_space;
                self.globals.insert(declarator.name.clone(), ginfo);
                continue;
            }

            // extern without initializer: track the type but don't emit a .bss entry
            if decl.is_extern && declarator.init.is_none() {
                if !self.globals.contains_key(&declarator.name) {
                    let da = self.analyze_declaration(&decl.type_spec, &declarator.derived);
                    let mut ginfo = GlobalInfo::from_analysis(&da);
                    ginfo.var.address_space = decl.address_space;
                    self.globals.insert(declarator.name.clone(), ginfo);
                }
                continue;
            }

            // Handle tentative definitions and re-declarations
            if self.globals.contains_key(&declarator.name) {
                if declarator.init.is_none() {
                    if self.emitted_global_names.contains(&declarator.name) {
                        continue;
                    }
                } else {
                    self.module.globals.retain(|g| g.name != declarator.name);
                    self.emitted_global_names.remove(&declarator.name);
                }
            }

            let mut da = self.analyze_declaration(&decl.type_spec, &declarator.derived);

            // For global arrays-of-pointers, clear struct_layout
            if da.is_array_of_pointers || da.is_array_of_func_ptrs {
                da.struct_layout = None;
                da.is_struct = false;
            }

            // For unsized arrays, compute actual size from initializer
            self.fixup_unsized_array(&mut da, &decl.type_spec, &declarator.derived, &declarator.init);

            // For scalar globals, use the actual C type size (handles long double = 16 bytes)
            if !da.is_array && !da.is_pointer && da.struct_layout.is_none() {
                let c_size = self.sizeof_type(&decl.type_spec);
                da.actual_alloc_size = c_size.max(da.var_ty.size());
            }

            let is_extern_decl = decl.is_extern && declarator.init.is_none();

            // Register the global before evaluating its initializer so that
            // self-referential initializers (e.g., `struct Node n = {&n}`)
            // can resolve &n via eval_global_addr_expr.
            let mut ginfo = GlobalInfo::from_analysis(&da);
            ginfo.var.address_space = decl.address_space;
            self.globals.insert(declarator.name.clone(), ginfo);

            let init = if let Some(ref initializer) = declarator.init {
                // For pointer globals (char *p = ...) and pointer arrays (char *a[] = ...),
                // use Ptr as the base type for initializer coercion, not the pointee type.
                // base_ty is the pointee type (e.g., I8 for char*), but the stored value
                // is always pointer-sized. For non-pointer scalars and non-pointer arrays,
                // base_ty is correct.
                let init_base_ty = if da.is_pointer && !da.is_array {
                    da.var_ty  // scalar pointer: var_ty = Ptr
                } else if da.is_array_of_pointers || da.is_array_of_func_ptrs {
                    IrType::Ptr  // pointer array: each element is Ptr
                } else {
                    da.base_ty
                };
                self.lower_global_init(initializer, &decl.type_spec, init_base_ty, da.is_array, da.elem_size, da.actual_alloc_size, &da.struct_layout, &da.array_dim_strides)
            } else {
                GlobalInit::Zero
            };

            let (align, has_explicit_align) = {
                let c_align = self.alignof_type(&decl.type_spec);
                let natural = if c_align > 0 { c_align.max(da.var_ty.align()) } else { da.var_ty.align() };
                // Resolve _Alignas alignment: use the lowerer's alignof_type (which can
                // resolve typedefs) for the alignas type specifier, falling back to the
                // parser's pre-computed numeric alignment.
                let mut explicit = if let Some(ref alignas_ts) = decl.alignas_type {
                    Some(self.alignof_type(alignas_ts))
                } else {
                    decl.alignment
                };
                // Recompute sizeof for aligned(sizeof(type)) with full layout info
                if let Some(ref sizeof_ts) = decl.alignment_sizeof_type {
                    let real_sizeof = self.sizeof_type(sizeof_ts);
                    explicit = Some(explicit.map_or(real_sizeof, |a| a.max(real_sizeof)));
                }
                // Also incorporate alignment from typedef
                if let Some(&td_align) = self.typedef_alignment_for_type_spec(&decl.type_spec) {
                    explicit = Some(explicit.map_or(td_align, |a| a.max(td_align)));
                }
                if let Some(explicit_val) = explicit {
                    (natural.max(explicit_val), true)
                } else {
                    (natural, false)
                }
            };

            let is_static = decl.is_static;

            let global_ty = da.resolve_global_ty(&init);

            let final_size = match &init {
                GlobalInit::Array(vals) if da.is_struct && vals.len() > da.actual_alloc_size => vals.len(),
                GlobalInit::Compound(_) if da.is_struct => {
                    let emitted = init.emitted_byte_size();
                    emitted.max(da.actual_alloc_size)
                }
                _ => da.actual_alloc_size,
            };

            // For pointer variables, decl.is_const refers to the pointee type
            // (e.g., `const char *p` means p points to const char, but p itself is mutable).
            // Only non-pointer variables get is_const from the declaration.
            // Our parser doesn't track `* const` (pointer-to-const qualifier), so for
            // pointers we conservatively mark as non-const.
            let var_is_const = decl.is_const && !da.is_pointer
                && !da.is_array_of_pointers && !da.is_array_of_func_ptrs;

            self.emitted_global_names.insert(declarator.name.clone());
            self.module.globals.push(IrGlobal {
                name: declarator.name.clone(),
                ty: global_ty,
                size: final_size,
                align,
                init,
                is_static,
                is_extern: is_extern_decl,
                is_common: decl.is_common,
                section: declarator.section.clone(),
                is_weak: declarator.is_weak,
                visibility: declarator.visibility.clone(),
                has_explicit_align,
                is_const: var_is_const,
            });
        }
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

    // --- Declaration analysis ---

    /// Perform shared declaration analysis for both local and global variable declarations.
    ///
    /// Computes all type-related properties (base type, array info, pointer info, struct layout,
    /// pointee type, etc.) that both `lower_local_decl` and `lower_global_decl` need.
    pub(super) fn analyze_declaration(&self, type_spec: &TypeSpecifier, derived: &[DerivedDeclarator]) -> DeclAnalysis {
        let mut base_ty = self.type_spec_to_ir(type_spec);
        let (alloc_size, elem_size, is_array, is_pointer, array_dim_strides) =
            self.compute_decl_info(type_spec, derived);

        // For typedef'd array types, type_spec_to_ir returns Ptr but we need the element type
        if is_array && base_ty == IrType::Ptr && !is_pointer {
            let mut resolved = self.resolve_type_spec(type_spec);
            let mut found = false;
            while let TypeSpecifier::Array(ref inner, _) = resolved {
                let inner_resolved = self.resolve_type_spec(inner);
                if matches!(inner_resolved, TypeSpecifier::Array(_, _)) {
                    resolved = inner_resolved;
                } else {
                    base_ty = self.type_spec_to_ir(inner);
                    found = true;
                    break;
                }
            }
            if !found {
                let ctype = self.type_spec_to_ctype(type_spec);
                let mut ct = &ctype;
                while let CType::Array(inner, _) = ct {
                    ct = inner.as_ref();
                }
                base_ty = IrType::from_ctype(ct);
            }
        }

        // Detect arrays-of-pointers and arrays-of-function-pointers.
        // For declarations like `int (*ap[3])[4]` (array of pointers-to-arrays),
        // derived = [Array(4), Pointer, Array(3)]. The last element is Array(3),
        // making this an array-of-pointers. We check:
        //   1. Any pointer before the LAST array in derived (not just the first array),
        //      matching the logic in compute_decl_info which checks `last_is_array`.
        //   2. Typedef'd pointer with array dimensions.
        let is_array_of_pointers = is_array && {
            let ptr_pos = derived.iter().position(|d| matches!(d, DerivedDeclarator::Pointer));
            let last_arr_pos = derived.iter().rposition(|d| matches!(d, DerivedDeclarator::Array(_)));
            let has_derived_ptr_before_last_arr = matches!((ptr_pos, last_arr_pos), (Some(pp), Some(ap)) if pp < ap);
            let typedef_ptr_array = ptr_pos.is_none() && last_arr_pos.is_some() &&
                self.is_type_pointer(type_spec);
            has_derived_ptr_before_last_arr || typedef_ptr_array
        };
        let is_array_of_func_ptrs = is_array && derived.iter().any(|d|
            matches!(d, DerivedDeclarator::FunctionPointer(_, _) | DerivedDeclarator::Function(_, _)));
        let var_ty = if is_pointer || is_array_of_pointers || is_array_of_func_ptrs {
            IrType::Ptr
        } else {
            base_ty
        };

        // Compute element IR type for arrays
        let elem_ir_ty = if is_array && base_ty == IrType::Ptr && !is_array_of_pointers {
            let ctype = self.type_spec_to_ctype(type_spec);
            if let CType::Array(ref elem_ct, _) = ctype {
                IrType::from_ctype(elem_ct)
            } else {
                base_ty
            }
        } else {
            base_ty
        };

        let has_derived_ptr = derived.iter().any(|d| matches!(d, DerivedDeclarator::Pointer));
        let is_bool = self.is_type_bool(type_spec) && !has_derived_ptr && !is_array;

        let struct_layout = self.get_struct_layout_for_type(type_spec)
            .or_else(|| {
                let ctype = self.type_spec_to_ctype(type_spec);
                match &ctype {
                    CType::Pointer(inner, _) => self.struct_layout_from_ctype(inner),
                    CType::Array(_, _) => {
                        // Unwrap all Array levels to find the innermost element type.
                        // This handles multi-dimensional typedef'd arrays like:
                        // typedef struct s tyst[2][2]; -> CType::Array(Array(Struct, 2), 2)
                        let mut ct = &ctype;
                        while let CType::Array(inner, _) = ct {
                            ct = inner.as_ref();
                        }
                        self.struct_layout_from_ctype(ct)
                    }
                    _ => None,
                }
            });
        let is_struct = struct_layout.is_some() && !is_pointer && !is_array;

        let actual_alloc_size = if let Some(ref layout) = struct_layout {
            if is_array || is_pointer { alloc_size } else { layout.size }
        } else {
            alloc_size
        };

        let pointee_type = self.compute_pointee_type(type_spec, derived);
        let c_type = Some(self.build_full_ctype(type_spec, derived));

        // Detect pointer-to-function-pointer: declared with 2+ consecutive Pointer
        // entries before the first FunctionPointer (e.g., int (**fpp)(int, int)
        // has derived=[Pointer, Pointer, FunctionPointer(...)]).
        // This is NOT triggered for function-pointer-returning-function-pointer like
        // int (*(*p)(int,int))(int,int) where Pointer entries are separated by
        // FunctionPointer entries.
        // This detection is needed because build_full_ctype absorbs extra pointer
        // levels into the function's return type, making the CType shape ambiguous.
        let is_ptr_to_func_ptr = {
            // A pointer-to-function-pointer (e.g., int (**fpp)(int, int)) has derived
            // = [Pointer, FunctionPointer, Pointer]. The extra indirection Pointer
            // appears AFTER the FunctionPointer entry (added by combine_declarator_parts
            // as "extra indirection" from inner_derived).
            //
            // In contrast, int *(*fnc)() has derived = [Pointer, Pointer, FunctionPointer]
            // where pointers BEFORE FunctionPointer are return-type pointers, not extra
            // indirection. The syntax-marker Pointer is always immediately before FunctionPointer.
            //
            // So the correct check is: has any Pointer entries AFTER the FunctionPointer entry.
            let fptr_count = derived.iter().filter(|d|
                matches!(d, DerivedDeclarator::FunctionPointer(_, _) | DerivedDeclarator::Function(_, _))).count();
            if fptr_count == 1 {
                let mut found_fptr = false;
                let mut ptrs_after_fptr = 0usize;
                for d in derived {
                    match d {
                        DerivedDeclarator::FunctionPointer(_, _) | DerivedDeclarator::Function(_, _) => {
                            found_fptr = true;
                        }
                        DerivedDeclarator::Pointer if found_fptr => {
                            ptrs_after_fptr += 1;
                        }
                        _ => {}
                    }
                }
                ptrs_after_fptr >= 1
            } else {
                false
            }
        };

        DeclAnalysis {
            base_ty, var_ty, alloc_size, elem_size, is_array, is_pointer,
            array_dim_strides, is_array_of_pointers, is_array_of_func_ptrs,
            struct_layout, is_struct, actual_alloc_size, pointee_type, c_type,
            is_bool, elem_ir_ty, is_ptr_to_func_ptr,
        }
    }

    /// Fix up allocation size and strides for unsized arrays (int a[] = {...}).
    /// Also updates c_type from Array(elem, None) to Array(elem, Some(count))
    /// so that sizeof() returns the correct array size rather than pointer size.
    pub(super) fn fixup_unsized_array(
        &self,
        da: &mut DeclAnalysis,
        type_spec: &TypeSpecifier,
        derived: &[DerivedDeclarator],
        init: &Option<Initializer>,
    ) {
        let is_unsized = da.is_array && (
            derived.iter().any(|d| matches!(d, DerivedDeclarator::Array(None)))
            || matches!(self.type_spec_to_ctype(type_spec), CType::Array(_, None))
        );
        if !is_unsized {
            return;
        }
        if let Some(ref initializer) = init {
            match initializer {
                Initializer::Expr(expr) => {
                    if da.base_ty == IrType::I8 || da.base_ty == IrType::U8 {
                        if let Expr::StringLiteral(s, _) = expr {
                            da.alloc_size = s.chars().count() + 1;
                            da.actual_alloc_size = da.alloc_size;
                        }
                        if let Expr::WideStringLiteral(s, _) | Expr::Char16StringLiteral(s, _) = expr {
                            da.alloc_size = s.chars().count() + 1;
                            da.actual_alloc_size = da.alloc_size;
                        }
                    }
                    if da.base_ty == IrType::I32 || da.base_ty == IrType::U32 {
                        if let Expr::WideStringLiteral(s, _) = expr {
                            let char_count = s.chars().count() + 1;
                            da.alloc_size = char_count * 4;
                            da.actual_alloc_size = da.alloc_size;
                        }
                        if let Expr::StringLiteral(s, _) | Expr::Char16StringLiteral(s, _) = expr {
                            let char_count = s.chars().count() + 1;
                            da.alloc_size = char_count * 4;
                            da.actual_alloc_size = da.alloc_size;
                        }
                    }
                    if da.base_ty == IrType::I16 || da.base_ty == IrType::U16 {
                        if let Expr::Char16StringLiteral(s, _)
                            | Expr::StringLiteral(s, _)
                            | Expr::WideStringLiteral(s, _) = expr {
                            let char_count = s.chars().count() + 1;
                            da.alloc_size = char_count * 2;
                            da.actual_alloc_size = da.alloc_size;
                        }
                    }
                }
                Initializer::List(items) => {
                    let actual_count = if let Some(ref layout) = da.struct_layout {
                        self.compute_struct_array_init_count(items, layout)
                    } else {
                        self.compute_init_list_array_size_for_char_array(items, da.base_ty)
                    };
                    if da.elem_size > 0 {
                        // For multi-dimensional arrays with nested brace init
                        // (e.g., int*[][3] = {{&x,&y,&z}, {&w,0,0}}), each
                        // top-level item spans the outermost stride.
                        // For flat init (e.g., {&x,0,0,&y,0,0}), each item
                        // is one element.
                        let has_nested_lists = items.iter().any(|item| {
                            matches!(&item.init, Initializer::List(_))
                        });
                        let item_size = if da.array_dim_strides.len() > 1 && has_nested_lists {
                            da.array_dim_strides[0]
                        } else {
                            da.elem_size
                        };
                        da.alloc_size = actual_count * item_size;
                        da.actual_alloc_size = da.alloc_size;
                        if da.array_dim_strides.len() == 1 {
                            da.array_dim_strides = vec![da.elem_size];
                        }
                    }
                }
            }
            // Update c_type from Array(elem, None) to Array(elem, Some(count))
            // so that sizeof() on this array returns the correct total size.
            if da.elem_size > 0 && da.alloc_size > 0 {
                let resolved_count = da.alloc_size / da.elem_size;
                if let Some(ref ct) = da.c_type {
                    if let CType::Array(elem, None) = ct {
                        da.c_type = Some(CType::Array(elem.clone(), Some(resolved_count)));
                    }
                }
            }
        }
    }
}
