//! Module-level type system state for IR lowering.
//!
//! `TypeContext` holds all type information that persists across function boundaries:
//! struct/union layouts, typedefs, enum constants, function typedef metadata, and
//! a type cache. It is populated by the sema pass and consumed/extended by the lowerer.
//!
//! Scope management uses an undo-log pattern (`TypeScopeFrame`) rather than cloning
//! entire HashMaps at scope boundaries. This gives O(changes-in-scope) cost for
//! scope push/pop instead of O(total-map-size).

use crate::common::fx_hash::{FxHashMap, FxHashSet};
use crate::common::types::{StructLayout, CType};
use super::definitions::FunctionTypedefInfo;

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
    pub struct_layouts_shadowed: Vec<(String, StructLayout)>,
    /// Keys newly inserted into `ctype_cache`.
    pub ctype_cache_added: Vec<String>,
    /// Keys that were overwritten in `ctype_cache`: (key, previous_value).
    pub ctype_cache_shadowed: Vec<(String, CType)>,
    /// Keys newly inserted into `typedefs`.
    pub typedefs_added: Vec<String>,
    /// Keys that were overwritten in `typedefs`: (key, previous_value).
    pub typedefs_shadowed: Vec<(String, CType)>,
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
        }
    }
}

/// Type-system state extracted from Lowerer.
/// Holds struct/union layouts, typedefs, enum constants, and type caches.
/// This is module-level state that persists across functions.
#[derive(Debug)]
pub struct TypeContext {
    /// Struct/union layouts indexed by tag name
    pub struct_layouts: FxHashMap<String, StructLayout>,
    /// Enum constant values
    pub enum_constants: FxHashMap<String, i64>,
    /// Typedef mappings (name -> resolved CType)
    pub typedefs: FxHashMap<String, CType>,
    /// Function typedef info (bare function typedefs like `typedef int func_t(int)`)
    pub function_typedefs: FxHashMap<String, FunctionTypedefInfo>,
    /// Set of typedef names that are function pointer types
    /// (e.g., `typedef void *(*lua_Alloc)(void *, ...)`)
    pub func_ptr_typedefs: FxHashSet<String>,
    /// Function pointer typedef info (return type, params, variadic)
    pub func_ptr_typedef_info: FxHashMap<String, FunctionTypedefInfo>,
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
        self.struct_layouts.get(key)
    }
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            struct_layouts: FxHashMap::default(),
            enum_constants: FxHashMap::default(),
            typedefs: FxHashMap::default(),
            function_typedefs: FxHashMap::default(),
            func_ptr_typedefs: FxHashSet::default(),
            func_ptr_typedef_info: FxHashMap::default(),
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

    /// Insert a struct layout from a &self context (interior mutability).
    /// This is safe because we are single-threaded and do not hold references
    /// into struct_layouts across this call.
    pub fn insert_struct_layout_from_ref(&self, key: &str, layout: StructLayout) {
        // SAFETY: We are single-threaded and no references into struct_layouts
        // are held across this call. The &self reference to TypeContext does not
        // create a mutable alias because we use a raw pointer for the mutation.
        let ptr = &self.struct_layouts as *const FxHashMap<String, StructLayout>
            as *mut FxHashMap<String, StructLayout>;
        unsafe { (*ptr).insert(key.to_string(), layout); }
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
        self.struct_layouts.insert(key, layout);
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
}
