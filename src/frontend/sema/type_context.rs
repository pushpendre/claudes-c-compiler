//! Shared type-system state for semantic analysis and IR lowering.
//!
//! `TypeContext` holds all type information that persists across function boundaries:
//! struct/union layouts, typedefs, enum constants, function typedef metadata, and
//! a type cache. It is populated by the sema pass and consumed/extended by the lowerer.
//!
//! This module lives in `frontend/sema/` because it is semantically part of the
//! sema output: sema creates, populates, and owns the TypeContext until it is
//! transferred to the lowerer. The lowerer imports from here, which is the correct
//! dependency direction (IR depends on frontend output, not the reverse).
//!
//! Scope management uses an undo-log pattern (`TypeScopeFrame`) rather than cloning
//! entire HashMaps at scope boundaries. This gives O(changes-in-scope) cost for
//! scope push/pop instead of O(total-map-size).

use crate::common::fx_hash::{FxHashMap, FxHashSet};
use std::rc::Rc;
use crate::common::types::{AddressSpace, StructLayout, RcLayout, CType};
use crate::frontend::parser::ast::{TypeSpecifier, ParamDecl, DerivedDeclarator};

/// Information about a function typedef (e.g., `typedef int func_t(int, int);`).
/// Used to detect when a declaration like `func_t add;` is a function declaration
/// rather than a variable declaration.
///
/// Shared between sema (which collects typedef info) and lowering (which uses it
/// to resolve function declarations through typedefs).
#[derive(Debug, Clone)]
pub struct FunctionTypedefInfo {
    /// The return TypeSpecifier of the function typedef
    pub return_type: TypeSpecifier,
    /// Parameters of the function typedef
    pub params: Vec<ParamDecl>,
    /// Whether the function is variadic
    pub variadic: bool,
}

/// Extract function pointer typedef info from a declarator with `FunctionPointer`
/// derived declarators.
///
/// For typedefs like `typedef void *(*lua_Alloc)(void *, ...)`, finds the
/// `FunctionPointer` derived and builds the return type. The last `Pointer` before
/// `FunctionPointer` is the `(*)` indirection, not a return-type pointer.
pub fn extract_fptr_typedef_info(
    base_type: &TypeSpecifier,
    derived: &[DerivedDeclarator],
) -> Option<FunctionTypedefInfo> {
    let (params, variadic) = derived.iter().find_map(|d| {
        if let DerivedDeclarator::FunctionPointer(p, v) = d { Some((p, v)) } else { None }
    })?;
    let ptr_count_before_fptr = derived.iter()
        .take_while(|d| !matches!(d, DerivedDeclarator::FunctionPointer(_, _)))
        .filter(|d| matches!(d, DerivedDeclarator::Pointer))
        .count();
    let ret_ptr_count = ptr_count_before_fptr.saturating_sub(1);
    let mut return_type = base_type.clone();
    for _ in 0..ret_ptr_count {
        return_type = TypeSpecifier::Pointer(Box::new(return_type), AddressSpace::Default);
    }
    Some(FunctionTypedefInfo {
        return_type,
        params: params.clone(),
        variadic: *variadic,
    })
}

/// Records undo operations for type-system scoped state.
///
/// Pushed on scope entry, popped on scope exit. Tracks both newly-added keys
/// (removed on undo) and shadowed keys (restored to previous value on undo)
/// across enum_constants, struct_layouts, ctype_cache, and typedefs.
#[derive(Debug)]
pub struct TypeScopeFrame {
    /// Keys newly inserted into `enum_constants`.
    pub enums_added: Vec<String>,
    /// Keys newly inserted into `struct_layouts`.
    pub struct_layouts_added: Vec<String>,
    /// Keys that were overwritten in `struct_layouts`: (key, previous_value).
    /// Uses Rc<StructLayout> so saving/restoring is a cheap refcount bump.
    pub struct_layouts_shadowed: Vec<(String, RcLayout)>,
    /// Keys newly inserted into `ctype_cache`.
    pub ctype_cache_added: Vec<String>,
    /// Keys that were overwritten in `ctype_cache`: (key, previous_value).
    pub ctype_cache_shadowed: Vec<(String, CType)>,
    /// Keys newly inserted into `typedefs`.
    pub typedefs_added: Vec<String>,
    /// Keys that were overwritten in `typedefs`: (key, previous_value).
    pub typedefs_shadowed: Vec<(String, CType)>,
    /// Keys newly inserted into `typedef_alignments`.
    pub typedef_alignments_added: Vec<String>,
    /// Keys that were overwritten in `typedef_alignments`: (key, previous_value).
    pub typedef_alignments_shadowed: Vec<(String, usize)>,
}

impl TypeScopeFrame {
    fn new() -> Self {
        Self {
            enums_added: Vec::new(),
            struct_layouts_added: Vec::new(),
            struct_layouts_shadowed: Vec::new(),
            ctype_cache_added: Vec::new(),
            ctype_cache_shadowed: Vec::new(),
            typedefs_added: Vec::new(),
            typedefs_shadowed: Vec::new(),
            typedef_alignments_added: Vec::new(),
            typedef_alignments_shadowed: Vec::new(),
        }
    }
}

/// Module-level type-system state produced by sema and consumed by lowering.
///
/// Holds struct/union layouts, typedefs, enum constants, and type caches.
/// Populated by the sema pass during semantic analysis, then transferred
/// to the lowerer by ownership for IR emission.
#[derive(Debug)]
pub struct TypeContext {
    /// Struct/union layouts indexed by tag name.
    /// Uses Rc<StructLayout> so lookups/clones are cheap refcount bumps
    /// instead of deep-copying all field names and types.
    pub struct_layouts: FxHashMap<String, RcLayout>,
    /// Enum constant values
    pub enum_constants: FxHashMap<String, i64>,
    /// Typedef mappings (name -> resolved CType)
    pub typedefs: FxHashMap<String, CType>,
    /// Per-typedef alignment overrides from `__attribute__((aligned(N)))` on typedef
    /// declarations.  E.g. `typedef struct S aligned_S __attribute__((aligned(32)));`
    /// stores `"aligned_S" -> 32`.  Consulted when computing field / variable alignment
    /// for declarations that use the typedef name.
    pub typedef_alignments: FxHashMap<String, usize>,
    /// Function typedef info (bare function typedefs like `typedef int func_t(int)`)
    pub function_typedefs: FxHashMap<String, FunctionTypedefInfo>,
    /// Set of typedef names that are function pointer types
    /// (e.g., `typedef void *(*lua_Alloc)(void *, ...)`)
    pub func_ptr_typedefs: FxHashSet<String>,
    /// Function pointer typedef info (return type, params, variadic)
    pub func_ptr_typedef_info: FxHashMap<String, FunctionTypedefInfo>,
    /// Set of typedef names that alias enum types.
    /// Used to treat enum-typedef bitfields as unsigned (GCC compat).
    pub enum_typedefs: FxHashSet<String>,
    /// Packed enum type info, keyed by tag name.
    /// Stored when a packed enum definition is processed so that forward
    /// references can look up the correct size.
    pub packed_enum_types: FxHashMap<String, crate::common::types::EnumType>,
    /// Return CType for known functions
    pub func_return_ctypes: FxHashMap<String, CType>,
    /// Cache for CType of named struct/union types.
    /// Uses RefCell because type_spec_to_ctype takes &self.
    pub ctype_cache: std::cell::RefCell<FxHashMap<String, CType>>,
    /// Scope stack for type-system undo tracking (enum_constants, struct_layouts, ctype_cache)
    pub scope_stack: Vec<TypeScopeFrame>,
    /// Counter for anonymous struct/union CType keys generated from &self contexts.
    /// Uses Cell for interior mutability since type_spec_to_ctype takes &self.
    anon_ctype_counter: std::cell::Cell<u32>,
}

impl crate::common::types::StructLayoutProvider for TypeContext {
    fn get_struct_layout(&self, key: &str) -> Option<&StructLayout> {
        self.struct_layouts.get(key).map(|rc| rc.as_ref())
    }
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            struct_layouts: FxHashMap::default(),
            enum_constants: FxHashMap::default(),
            typedefs: FxHashMap::default(),
            typedef_alignments: FxHashMap::default(),
            function_typedefs: FxHashMap::default(),
            func_ptr_typedefs: FxHashSet::default(),
            func_ptr_typedef_info: FxHashMap::default(),
            enum_typedefs: FxHashSet::default(),
            packed_enum_types: FxHashMap::default(),
            func_return_ctypes: FxHashMap::default(),
            ctype_cache: std::cell::RefCell::new(FxHashMap::default()),
            scope_stack: Vec::new(),
            anon_ctype_counter: std::cell::Cell::new(0),
        }
    }

    /// Get the next anonymous struct/union ID for CType key generation.
    /// Safe to call from &self contexts (uses Cell for interior mutability).
    pub fn next_anon_struct_id(&self) -> u32 {
        let id = self.anon_ctype_counter.get();
        self.anon_ctype_counter.set(id + 1);
        id
    }

    /// Set the anonymous struct counter to avoid key collisions between phases.
    /// Called after sema to ensure the lowerer's counter starts past sema's IDs.
    pub fn set_anon_ctype_counter(&self, value: u32) {
        self.anon_ctype_counter.set(value);
    }

    /// Insert a struct layout from a &self context (interior mutability).
    /// This is safe because we are single-threaded and do not hold references
    /// into struct_layouts across this call.
    pub fn insert_struct_layout_from_ref(&self, key: &str, layout: StructLayout) {
        // SAFETY: We are single-threaded and no references into struct_layouts
        // are held across this call. The &self reference to TypeContext does not
        // create a mutable alias because we use a raw pointer for the mutation.
        let ptr = &self.struct_layouts as *const FxHashMap<String, RcLayout>
            as *mut FxHashMap<String, RcLayout>;
        unsafe { (*ptr).insert(key.to_string(), Rc::new(layout)); }
    }

    /// Invalidate a ctype_cache entry from a &self context.
    pub fn invalidate_ctype_cache_from_ref(&self, key: &str) {
        self.ctype_cache.borrow_mut().remove(key);
    }

    /// Push a new type-system scope frame.
    pub fn push_scope(&mut self) {
        self.scope_stack.push(TypeScopeFrame::new());
    }

    /// Pop the top type-system scope frame and undo changes to
    /// enum_constants, struct_layouts, ctype_cache, and typedefs.
    pub fn pop_scope(&mut self) {
        if let Some(frame) = self.scope_stack.pop() {
            for key in frame.enums_added {
                self.enum_constants.remove(&key);
            }
            for key in frame.struct_layouts_added {
                self.struct_layouts.remove(&key);
            }
            for (key, val) in frame.struct_layouts_shadowed {
                // Don't restore an empty forward-declaration layout over a full
                // definition.
                if val.fields.is_empty() {
                    if let Some(current) = self.struct_layouts.get(&key) {
                        if !current.fields.is_empty() {
                            continue;
                        }
                    }
                }
                self.struct_layouts.insert(key, val);
            }
            {
                let mut cache = self.ctype_cache.borrow_mut();
                for key in frame.ctype_cache_added {
                    cache.remove(&key);
                }
                for (key, val) in frame.ctype_cache_shadowed {
                    cache.insert(key, val);
                }
            }
            for key in frame.typedefs_added {
                self.typedefs.remove(&key);
            }
            for (key, val) in frame.typedefs_shadowed {
                self.typedefs.insert(key, val);
            }
            for key in frame.typedef_alignments_added {
                self.typedef_alignments.remove(&key);
            }
            for (key, val) in frame.typedef_alignments_shadowed {
                self.typedef_alignments.insert(key, val);
            }
        }
    }

    /// Insert an enum constant, tracking the change in the current scope frame.
    pub fn insert_enum_scoped(&mut self, name: String, value: i64) {
        let track = !self.enum_constants.contains_key(&name);
        if track {
            if let Some(frame) = self.scope_stack.last_mut() {
                frame.enums_added.push(name.clone());
            }
        }
        self.enum_constants.insert(name, value);
    }

    /// Insert a struct layout, tracking the change in the current scope frame
    /// so it can be undone on scope exit.
    pub fn insert_struct_layout_scoped(&mut self, key: String, layout: StructLayout) {
        if let Some(frame) = self.scope_stack.last_mut() {
            if let Some(prev) = self.struct_layouts.get(&key).cloned() {
                frame.struct_layouts_shadowed.push((key.clone(), prev));
            } else {
                frame.struct_layouts_added.push(key.clone());
            }
        }
        self.struct_layouts.insert(key, Rc::new(layout));
    }

    /// Insert a typedef, tracking the change in the current scope frame
    /// so it can be undone on scope exit.
    pub fn insert_typedef_scoped(&mut self, name: String, ctype: CType) {
        if let Some(frame) = self.scope_stack.last_mut() {
            if let Some(prev) = self.typedefs.get(&name).cloned() {
                frame.typedefs_shadowed.push((name.clone(), prev));
            } else {
                frame.typedefs_added.push(name.clone());
            }
        }
        self.typedefs.insert(name, ctype);
    }

    /// Insert a typedef alignment, tracking the change in the current scope frame
    /// so it can be undone on scope exit.
    pub fn insert_typedef_alignment_scoped(&mut self, name: String, align: usize) {
        if let Some(frame) = self.scope_stack.last_mut() {
            if let Some(prev) = self.typedef_alignments.get(&name).copied() {
                frame.typedef_alignments_shadowed.push((name.clone(), prev));
            } else {
                frame.typedef_alignments_added.push(name.clone());
            }
        }
        self.typedef_alignments.insert(name, align);
    }

    /// Invalidate a ctype_cache entry, tracking the change in the current scope frame
    /// so it can be restored on scope exit.
    pub fn invalidate_ctype_cache_scoped(&mut self, key: &str) {
        let prev = {
            let mut cache = self.ctype_cache.borrow_mut();
            cache.remove(key)
        };
        if let Some(frame) = self.scope_stack.last_mut() {
            if let Some(prev) = prev {
                frame.ctype_cache_shadowed.push((key.to_string(), prev));
            } else {
                frame.ctype_cache_added.push(key.to_string());
            }
        }
    }

    /// Insert a struct layout from a &self context (interior mutability),
    /// tracking the change in the current scope frame so it can be undone
    /// on scope exit. This is the scoped equivalent of `insert_struct_layout_from_ref`.
    ///
    /// Used by `type_spec_to_ctype` which takes &self but still needs to
    /// properly scope struct layout insertions within function bodies.
    pub fn insert_struct_layout_scoped_from_ref(&self, key: &str, layout: StructLayout) {
        // SAFETY: Single-threaded; no references into struct_layouts or scope_stack
        // are held across this call.
        let layouts_ptr = &self.struct_layouts as *const FxHashMap<String, RcLayout>
            as *mut FxHashMap<String, RcLayout>;
        let stack_ptr = &self.scope_stack as *const Vec<TypeScopeFrame>
            as *mut Vec<TypeScopeFrame>;
        unsafe {
            if let Some(frame) = (*stack_ptr).last_mut() {
                if let Some(prev) = (*layouts_ptr).get(key).cloned() {
                    frame.struct_layouts_shadowed.push((key.to_string(), prev));
                } else {
                    frame.struct_layouts_added.push(key.to_string());
                }
            }
            (*layouts_ptr).insert(key.to_string(), Rc::new(layout));
        }
    }

    /// Invalidate a ctype_cache entry from a &self context, tracking the change
    /// in the current scope frame so it can be restored on scope exit.
    pub fn invalidate_ctype_cache_scoped_from_ref(&self, key: &str) {
        let prev = {
            let mut cache = self.ctype_cache.borrow_mut();
            cache.remove(key)
        };
        // SAFETY: Single-threaded; no references into scope_stack held across this call.
        let stack_ptr = &self.scope_stack as *const Vec<TypeScopeFrame>
            as *mut Vec<TypeScopeFrame>;
        unsafe {
            if let Some(frame) = (*stack_ptr).last_mut() {
                if let Some(prev) = prev {
                    frame.ctype_cache_shadowed.push((key.to_string(), prev));
                } else {
                    frame.ctype_cache_added.push(key.to_string());
                }
            }
        }
    }
}
