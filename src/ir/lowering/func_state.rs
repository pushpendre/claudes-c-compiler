//! Per-function build state for IR lowering.
//!
//! `FunctionBuildState` holds all state that is created fresh for each function
//! being lowered and discarded afterward. This makes the function-vs-module
//! lifecycle boundary explicit: the Lowerer wraps this in `Option<FunctionBuildState>`,
//! which is `None` between functions.
//!
//! Scope management uses an undo-log pattern (`FuncScopeFrame`) rather than
//! cloning entire HashMaps at scope boundaries. On scope exit, only the changes
//! made within that scope are undone, giving O(changes) cost instead of O(total).

use std::collections::HashMap;
use crate::ir::ir::*;
use crate::common::types::{IrType, CType};
use super::definitions::{LocalInfo, SwitchFrame};

/// Records undo operations for function-local scoped variables.
///
/// Pushed on scope entry, popped on scope exit to restore previous state.
/// Tracks both newly-added keys (removed on undo) and shadowed keys
/// (restored to previous value on undo).
#[derive(Debug)]
pub(super) struct FuncScopeFrame {
    /// Keys that were newly inserted into `locals` (not present before scope entry).
    pub locals_added: Vec<String>,
    /// Keys that were overwritten in `locals`: (key, previous_value).
    pub locals_shadowed: Vec<(String, LocalInfo)>,
    /// Keys newly inserted into `static_local_names`.
    pub statics_added: Vec<String>,
    /// Keys that were overwritten in `static_local_names`: (key, previous_value).
    pub statics_shadowed: Vec<(String, String)>,
    /// Keys newly inserted into `const_local_values`.
    pub consts_added: Vec<String>,
    /// Keys that were overwritten in `const_local_values`: (key, previous_value).
    pub consts_shadowed: Vec<(String, i64)>,
    /// Keys newly inserted into `var_ctypes`.
    pub var_ctypes_added: Vec<String>,
    /// Keys that were overwritten in `var_ctypes`: (key, previous_value).
    pub var_ctypes_shadowed: Vec<(String, CType)>,
    /// Keys newly inserted into `vla_typedef_sizes`.
    pub vla_typedef_sizes_added: Vec<String>,
    /// Keys that were overwritten in `vla_typedef_sizes`: (key, previous_value).
    pub vla_typedef_sizes_shadowed: Vec<(String, Value)>,
}

impl FuncScopeFrame {
    fn new() -> Self {
        Self {
            locals_added: Vec::new(),
            locals_shadowed: Vec::new(),
            statics_added: Vec::new(),
            statics_shadowed: Vec::new(),
            consts_added: Vec::new(),
            consts_shadowed: Vec::new(),
            var_ctypes_added: Vec::new(),
            var_ctypes_shadowed: Vec::new(),
            vla_typedef_sizes_added: Vec::new(),
            vla_typedef_sizes_shadowed: Vec::new(),
        }
    }
}

/// Per-function build state, extracted from Lowerer to make the function-vs-module
/// state boundary explicit. Created fresh at the start of each function, replacing
/// the old pattern of clearing individual fields.
#[derive(Debug)]
pub(super) struct FunctionBuildState {
    /// Basic blocks accumulated for the current function
    pub blocks: Vec<BasicBlock>,
    /// Instructions for the current basic block being built
    pub instrs: Vec<Instruction>,
    /// Label of the current basic block
    pub current_label: BlockId,
    /// Name of the function currently being lowered
    pub name: String,
    /// Return type of the function currently being lowered
    pub return_type: IrType,
    /// Whether the current function returns _Bool
    pub return_is_bool: bool,
    /// sret pointer alloca for current function (struct returns > 16 bytes)
    pub sret_ptr: Option<Value>,
    /// Variable -> alloca mapping with metadata
    pub locals: HashMap<String, LocalInfo>,
    /// Loop context: labels to jump to on `break`
    pub break_labels: Vec<BlockId>,
    /// Loop context: labels to jump to on `continue`
    pub continue_labels: Vec<BlockId>,
    /// Stack of switch statement contexts
    pub switch_stack: Vec<SwitchFrame>,
    /// User-defined goto labels -> unique IR labels
    pub user_labels: HashMap<String, BlockId>,
    /// Scope stack for function-local variable undo tracking
    pub scope_stack: Vec<FuncScopeFrame>,
    /// Static local variable name -> mangled global name
    pub static_local_names: HashMap<String, String>,
    /// Const-qualified local variable values
    pub const_local_values: HashMap<String, i64>,
    /// CType for each local variable
    pub var_ctypes: HashMap<String, CType>,
    /// Runtime sizeof Values for VLA typedef types (e.g., `typedef char buf[n][m]`).
    /// Keyed by typedef name, value is the IR Value holding the runtime byte size.
    pub vla_typedef_sizes: HashMap<String, Value>,
    /// Per-function value counter (reset for each function)
    pub next_value: u32,
}

impl FunctionBuildState {
    /// Create a new function build state for the given function.
    pub fn new(name: String, return_type: IrType, return_is_bool: bool) -> Self {
        Self {
            blocks: Vec::new(),
            instrs: Vec::new(),
            current_label: BlockId(0),
            name,
            return_type,
            return_is_bool,
            sret_ptr: None,
            locals: HashMap::new(),
            break_labels: Vec::new(),
            continue_labels: Vec::new(),
            switch_stack: Vec::new(),
            user_labels: HashMap::new(),
            scope_stack: Vec::new(),
            static_local_names: HashMap::new(),
            const_local_values: HashMap::new(),
            var_ctypes: HashMap::new(),
            vla_typedef_sizes: HashMap::new(),
            next_value: 0,
        }
    }

    /// Push a new function-local scope frame.
    pub fn push_scope(&mut self) {
        self.scope_stack.push(FuncScopeFrame::new());
    }

    /// Pop the top function-local scope frame and undo changes to locals,
    /// static_local_names, const_local_values, and var_ctypes.
    pub fn pop_scope(&mut self) {
        if let Some(frame) = self.scope_stack.pop() {
            for key in frame.locals_added {
                self.locals.remove(&key);
            }
            for (key, val) in frame.locals_shadowed {
                self.locals.insert(key, val);
            }
            for key in frame.statics_added {
                self.static_local_names.remove(&key);
            }
            for (key, val) in frame.statics_shadowed {
                self.static_local_names.insert(key, val);
            }
            for key in frame.consts_added {
                self.const_local_values.remove(&key);
            }
            for (key, val) in frame.consts_shadowed {
                self.const_local_values.insert(key, val);
            }
            for key in frame.var_ctypes_added {
                self.var_ctypes.remove(&key);
            }
            for (key, val) in frame.var_ctypes_shadowed {
                self.var_ctypes.insert(key, val);
            }
            for key in frame.vla_typedef_sizes_added {
                self.vla_typedef_sizes.remove(&key);
            }
            for (key, val) in frame.vla_typedef_sizes_shadowed {
                self.vla_typedef_sizes.insert(key, val);
            }
        }
    }

    /// Insert a VLA typedef runtime size, tracking for scope management.
    pub fn insert_vla_typedef_size_scoped(&mut self, name: String, size: Value) {
        if let Some(frame) = self.scope_stack.last_mut() {
            if let Some(prev) = self.vla_typedef_sizes.remove(&name) {
                frame.vla_typedef_sizes_shadowed.push((name.clone(), prev));
            } else {
                frame.vla_typedef_sizes_added.push(name.clone());
            }
        }
        self.vla_typedef_sizes.insert(name, size);
    }

    /// Insert a local variable, tracking the change in the current scope frame.
    pub fn insert_local_scoped(&mut self, name: String, info: LocalInfo) {
        if let Some(frame) = self.scope_stack.last_mut() {
            if let Some(prev) = self.locals.remove(&name) {
                frame.locals_shadowed.push((name.clone(), prev));
            } else {
                frame.locals_added.push(name.clone());
            }
        }
        self.locals.insert(name, info);
    }

    /// Insert a static local name, tracking the change in the current scope frame.
    pub fn insert_static_local_scoped(&mut self, name: String, mangled: String) {
        if let Some(frame) = self.scope_stack.last_mut() {
            if let Some(prev) = self.static_local_names.remove(&name) {
                frame.statics_shadowed.push((name.clone(), prev));
            } else {
                frame.statics_added.push(name.clone());
            }
        }
        self.static_local_names.insert(name, mangled);
    }

    /// Insert a const local value, tracking the change in the current scope frame.
    pub fn insert_const_local_scoped(&mut self, name: String, value: i64) {
        if let Some(frame) = self.scope_stack.last_mut() {
            if let Some(prev) = self.const_local_values.remove(&name) {
                frame.consts_shadowed.push((name.clone(), prev));
            } else {
                frame.consts_added.push(name.clone());
            }
        }
        self.const_local_values.insert(name, value);
    }

    /// Insert a var ctype, tracking the change in the current scope frame.
    pub fn insert_var_ctype_scoped(&mut self, name: String, ctype: CType) {
        if let Some(frame) = self.scope_stack.last_mut() {
            if let Some(prev) = self.var_ctypes.remove(&name) {
                frame.var_ctypes_shadowed.push((name.clone(), prev));
            } else {
                frame.var_ctypes_added.push(name.clone());
            }
        }
        self.var_ctypes.insert(name, ctype);
    }

    /// Remove a local variable from `locals`, tracking the removal in the
    /// current scope frame so `pop_scope()` restores it.
    pub fn shadow_local_for_scope(&mut self, name: &str) {
        if let Some(prev_local) = self.locals.remove(name) {
            if let Some(frame) = self.scope_stack.last_mut() {
                frame.locals_shadowed.push((name.to_string(), prev_local));
            }
        }
    }

    /// Remove a static local name, tracking the removal in the current scope frame.
    pub fn shadow_static_for_scope(&mut self, name: &str) {
        if let Some(prev_static) = self.static_local_names.remove(name) {
            if let Some(frame) = self.scope_stack.last_mut() {
                frame.statics_shadowed.push((name.to_string(), prev_static));
            }
        }
    }
}
