use std::collections::HashMap;
use std::collections::HashSet;
use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, StructLayout, CType};
use crate::backend::Target;

/// Resolve a typedef's derived declarators into the final TypeSpecifier.
/// Still used for FunctionTypedefInfo return_type (which stores TypeSpecifier for now).
pub(super) fn resolve_typedef_derived(base: &TypeSpecifier, derived: &[DerivedDeclarator]) -> TypeSpecifier {
    let mut resolved_type = base.clone();
    let mut i = 0;
    while i < derived.len() {
        match &derived[i] {
            DerivedDeclarator::Pointer => {
                resolved_type = TypeSpecifier::Pointer(Box::new(resolved_type));
                i += 1;
            }
            DerivedDeclarator::Array(_) => {
                let mut array_sizes: Vec<Option<Box<Expr>>> = Vec::new();
                while i < derived.len() {
                    if let DerivedDeclarator::Array(size) = &derived[i] {
                        array_sizes.push(size.clone());
                        i += 1;
                    } else {
                        break;
                    }
                }
                for size in array_sizes.into_iter().rev() {
                    resolved_type = TypeSpecifier::Array(Box::new(resolved_type), size);
                }
            }
            _ => { i += 1; }
        }
    }
    resolved_type
}

/// Type metadata shared between local and global variables.
///
/// Both `LocalInfo` and `GlobalInfo` embed this struct via `Deref`, so field
/// access like `info.ty` or `info.is_array` works transparently on either type.
/// The `Lowerer::lookup_var_info()` helper returns `&VarInfo` for cases that
/// only need these shared fields, eliminating the duplicated locals-then-globals
/// lookup pattern.
#[derive(Debug, Clone)]
pub(super) struct VarInfo {
    /// The IR type of the variable (I8 for char, I32 for int, I64 for long, Ptr for pointers).
    pub ty: IrType,
    /// Element size for arrays (used for pointer arithmetic on subscript).
    /// For non-arrays this is 0.
    pub elem_size: usize,
    /// Whether this is an array (the alloca IS the base address, not a pointer to one).
    pub is_array: bool,
    /// For pointers and arrays, the type of the pointed-to/element type.
    /// Used for correct loads through pointer dereference and subscript.
    pub pointee_type: Option<IrType>,
    /// If this is a struct/union variable, its layout for member access.
    pub struct_layout: Option<StructLayout>,
    /// Whether this variable is a struct (not a pointer to struct).
    pub is_struct: bool,
    /// For multi-dimensional arrays: stride (in bytes) per dimension level.
    /// E.g., for int a[2][3][4], strides = [48, 16, 4] (row_size, inner_row, elem).
    /// Empty for non-arrays or 1D arrays (use elem_size instead).
    pub array_dim_strides: Vec<usize>,
    /// Full C type for precise multi-level pointer type resolution.
    pub c_type: Option<CType>,
}

/// Information about a local variable stored in an alloca.
/// Derefs to `VarInfo` for shared field access.
#[derive(Debug, Clone)]
pub(super) struct LocalInfo {
    /// Shared type metadata (ty, elem_size, is_array, pointee_type, etc.)
    pub var: VarInfo,
    /// The Value (alloca) holding the address of this local.
    pub alloca: Value,
    /// The total allocation size of this variable (for sizeof).
    pub alloc_size: usize,
    /// Whether this variable has _Bool type (needs value clamping to 0/1).
    pub is_bool: bool,
    /// For static local variables: the mangled global name. When set, accesses should
    /// emit a fresh GlobalAddr instruction instead of using `alloca`, because the
    /// declaration may be in an unreachable basic block (skipped by goto/switch).
    pub static_global_name: Option<String>,
    /// For VLA function parameters: runtime stride Values per dimension level.
    /// Parallel to `array_dim_strides`. When `Some(value)`, use the runtime Value
    /// instead of the compile-time stride. This supports parameters like
    /// `int m[rows][cols]` where `cols` is a runtime variable.
    pub vla_strides: Vec<Option<Value>>,
    /// For VLA local variables: the runtime Value holding sizeof(this_variable).
    /// Used when sizeof is applied to a VLA local variable.
    pub vla_size: Option<Value>,
}

impl std::ops::Deref for LocalInfo {
    type Target = VarInfo;
    fn deref(&self) -> &VarInfo { &self.var }
}

impl std::ops::DerefMut for LocalInfo {
    fn deref_mut(&mut self) -> &mut VarInfo { &mut self.var }
}

/// Information about a global variable tracked by the lowerer.
/// Derefs to `VarInfo` for shared field access.
#[derive(Debug, Clone)]
pub(super) struct GlobalInfo {
    /// Shared type metadata (ty, elem_size, is_array, pointee_type, etc.)
    pub var: VarInfo,
}

impl std::ops::Deref for GlobalInfo {
    type Target = VarInfo;
    fn deref(&self) -> &VarInfo { &self.var }
}

impl std::ops::DerefMut for GlobalInfo {
    fn deref_mut(&mut self) -> &mut VarInfo { &mut self.var }
}

/// Pre-computed declaration analysis shared between `lower_local_decl` and
/// `lower_global_decl`. Extracts the common type analysis (base type, array info,
/// pointer info, struct layout, etc.) that both paths need, eliminating the
/// ~80 lines of duplicated computation.
#[derive(Debug)]
pub(super) struct DeclAnalysis {
    /// The base IR type from the type specifier (before pointer/array derivation).
    pub base_ty: IrType,
    /// The final variable IR type (Ptr for pointers/arrays-of-pointers, else base_ty).
    pub var_ty: IrType,
    /// Total allocation size in bytes.
    pub alloc_size: usize,
    /// Element size for arrays (stride for indexing).
    pub elem_size: usize,
    /// Whether this declaration is an array.
    pub is_array: bool,
    /// Whether this declaration is a pointer.
    pub is_pointer: bool,
    /// Per-dimension strides for multi-dimensional arrays.
    pub array_dim_strides: Vec<usize>,
    /// Whether this is an array of pointers (int *arr[N]).
    pub is_array_of_pointers: bool,
    /// Whether this is an array of function pointers.
    pub is_array_of_func_ptrs: bool,
    /// Struct/union layout (for struct variables or pointer-to-struct).
    pub struct_layout: Option<StructLayout>,
    /// Whether this is a direct struct variable (not pointer-to or array-of).
    pub is_struct: bool,
    /// Actual allocation size (uses struct layout size for non-array structs).
    pub actual_alloc_size: usize,
    /// Pointee type for pointer/array types.
    pub pointee_type: Option<IrType>,
    /// Full C type for multi-level pointer resolution.
    pub c_type: Option<CType>,
    /// Whether this is a _Bool variable (not pointer or array of _Bool).
    pub is_bool: bool,
    /// The element IR type for arrays (accounts for typedef'd arrays).
    pub elem_ir_ty: IrType,
}

/// Information about a VLA dimension in a function parameter type.
#[derive(Debug)]
struct VlaDimInfo {
    /// Whether this dimension is a VLA (runtime variable).
    is_vla: bool,
    /// The name of the variable providing the dimension (e.g., "cols").
    dim_expr_name: String,
    /// If not VLA, the constant size value.
    const_size: Option<i64>,
    /// The sizeof the element type at this level (for computing strides).
    base_elem_size: usize,
}

/// Represents an lvalue - something that can be assigned to.
/// Contains the address (as an IR Value) where the data resides.
#[derive(Debug, Clone)]
pub(super) enum LValue {
    /// A direct variable: the alloca is the address.
    Variable(Value),
    /// An address computed at runtime (e.g., arr[i], *ptr).
    Address(Value),
}

/// A single level of switch statement context, pushed/popped as switches nest.
#[derive(Debug)]
pub(super) struct SwitchFrame {
    pub cases: Vec<(i64, String)>,
    pub default_label: Option<String>,
    pub expr_type: IrType,
}

/// Information about a function typedef (e.g., `typedef int func_t(int, int);`).
/// Used to detect when a declaration like `func_t add;` is a function declaration
/// rather than a variable declaration.
#[derive(Debug, Clone)]
pub struct FunctionTypedefInfo {
    /// The return TypeSpecifier of the function typedef
    pub return_type: TypeSpecifier,
    /// Parameters of the function typedef
    pub params: Vec<ParamDecl>,
    /// Whether the function is variadic
    pub variadic: bool,
}

/// Extract function typedef info from a declarator with `Function` derived declarators.
///
/// For typedefs like `typedef int func_t(int x);`, finds the `Function` derived
/// and builds the return type by counting leading `Pointer` deriveds. Returns None
/// if no `Function` derived is present or if `FunctionPointer` is also present.
pub(super) fn extract_func_typedef_info(
    base_type: &TypeSpecifier,
    derived: &[DerivedDeclarator],
) -> Option<FunctionTypedefInfo> {
    let has_func = derived.iter().any(|d| matches!(d, DerivedDeclarator::Function(_, _)));
    let has_fptr = derived.iter().any(|d| matches!(d, DerivedDeclarator::FunctionPointer(_, _)));
    if !has_func || has_fptr {
        return None;
    }
    let (params, variadic) = derived.iter().find_map(|d| {
        if let DerivedDeclarator::Function(p, v) = d { Some((p, v)) } else { None }
    })?;
    let ptr_count = derived.iter()
        .take_while(|d| matches!(d, DerivedDeclarator::Pointer))
        .count();
    let mut return_type = base_type.clone();
    for _ in 0..ptr_count {
        return_type = TypeSpecifier::Pointer(Box::new(return_type));
    }
    Some(FunctionTypedefInfo {
        return_type,
        params: params.clone(),
        variadic: *variadic,
    })
}

/// Extract function pointer typedef info from a declarator with `FunctionPointer`
/// derived declarators.
///
/// For typedefs like `typedef void *(*lua_Alloc)(void *, ...)`, finds the
/// `FunctionPointer` derived and builds the return type. The last `Pointer` before
/// `FunctionPointer` is the `(*)` indirection, not a return-type pointer.
pub(super) fn extract_fptr_typedef_info(
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
        return_type = TypeSpecifier::Pointer(Box::new(return_type));
    }
    Some(FunctionTypedefInfo {
        return_type,
        params: params.clone(),
        variadic: *variadic,
    })
}

/// Consolidated function signature metadata.
/// Replaces 10 parallel HashMaps with a single struct per function.
#[derive(Debug, Clone)]
pub(super) struct FuncSig {
    /// IR return type for inserting narrowing casts after calls.
    pub return_type: IrType,
    /// CType of the return value (for pointer-returning and struct-returning functions).
    pub return_ctype: Option<CType>,
    /// IR types of each parameter, for inserting implicit argument casts.
    pub param_types: Vec<IrType>,
    /// CTypes of each parameter, for complex type argument conversions.
    pub param_ctypes: Vec<CType>,
    /// Flags indicating which parameters are _Bool (need normalization to 0/1).
    pub param_bool_flags: Vec<bool>,
    /// Whether this function is variadic.
    pub is_variadic: bool,
    /// If the function returns a struct > 16 bytes, the struct size (uses hidden sret pointer).
    pub sret_size: Option<usize>,
    /// If the function returns a struct of 9-16 bytes via two registers, the struct size.
    pub two_reg_ret_size: Option<usize>,
    /// Per-parameter struct sizes for by-value struct passing ABI.
    /// Each entry is Some(size) if that parameter is a struct/union, None otherwise.
    pub param_struct_sizes: Vec<Option<usize>>,
}

/// Metadata about known functions (signatures, variadic status, ABI handling).
/// Uses a consolidated FuncSig per function instead of parallel HashMaps.
#[derive(Debug, Default)]
pub(super) struct FunctionMeta {
    /// Function name -> consolidated signature.
    pub sigs: HashMap<String, FuncSig>,
    /// Function pointer variable name -> signature (return type + param types).
    pub ptr_sigs: HashMap<String, FuncSig>,
}

/// Records undo operations for scope-based variable management.
/// Instead of cloning entire HashMaps on scope entry, we track what was changed
/// and undo it on scope exit. This reduces O(total_map_size) clone cost to
/// O(number_of_changes_in_scope).
///
/// Scope tracking is split into two frame types:
/// - `TypeScopeFrame`: tracks undo ops for TypeContext fields (enum_constants,
///   struct_layouts, ctype_cache). Managed by TypeContext::push_scope/pop_scope.
/// - `FuncScopeFrame`: tracks undo ops for FunctionBuildState fields (locals,
///   static_local_names, const_local_values, var_ctypes). Managed by
///   FunctionBuildState::push_scope/pop_scope.
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
    pub current_label: String,
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
    pub break_labels: Vec<String>,
    /// Loop context: labels to jump to on `continue`
    pub continue_labels: Vec<String>,
    /// Stack of switch statement contexts
    pub switch_stack: Vec<SwitchFrame>,
    /// User-defined goto labels -> unique IR labels
    pub user_labels: HashMap<String, String>,
    /// Scope stack for function-local variable undo tracking
    pub scope_stack: Vec<FuncScopeFrame>,
    /// Static local variable name -> mangled global name
    pub static_local_names: HashMap<String, String>,
    /// Const-qualified local variable values
    pub const_local_values: HashMap<String, i64>,
    /// CType for each local variable
    pub var_ctypes: HashMap<String, CType>,
    /// Per-function value counter (reset for each function)
    pub next_value: u32,
}

impl FunctionBuildState {
    /// Create a new function build state for the given function.
    pub fn new(name: String, return_type: IrType, return_is_bool: bool) -> Self {
        Self {
            blocks: Vec::new(),
            instrs: Vec::new(),
            current_label: String::new(),
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
        }
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

/// Type-system state extracted from Lowerer.
/// Holds struct/union layouts, typedefs, enum constants, and type caches.
/// This is module-level state that persists across functions.
#[derive(Debug)]
pub struct TypeContext {
    /// Struct/union layouts indexed by tag name
    pub struct_layouts: HashMap<String, StructLayout>,
    /// Enum constant values
    pub enum_constants: HashMap<String, i64>,
    /// Typedef mappings (name -> resolved CType)
    pub typedefs: HashMap<String, CType>,
    /// Function typedef info (bare function typedefs like `typedef int func_t(int)`)
    pub function_typedefs: HashMap<String, FunctionTypedefInfo>,
    /// Set of typedef names that are function pointer types
    /// (e.g., `typedef void *(*lua_Alloc)(void *, ...)`)
    pub func_ptr_typedefs: HashSet<String>,
    /// Function pointer typedef info (return type, params, variadic)
    pub func_ptr_typedef_info: HashMap<String, FunctionTypedefInfo>,
    /// Return CType for known functions
    pub func_return_ctypes: HashMap<String, CType>,
    /// Cache for CType of named struct/union types
    /// Uses RefCell because type_spec_to_ctype takes &self.
    pub ctype_cache: std::cell::RefCell<HashMap<String, CType>>,
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
            struct_layouts: HashMap::new(),
            enum_constants: HashMap::new(),
            typedefs: HashMap::new(),
            function_typedefs: HashMap::new(),
            func_ptr_typedefs: HashSet::new(),
            func_ptr_typedef_info: HashMap::new(),
            func_return_ctypes: HashMap::new(),
            ctype_cache: std::cell::RefCell::new(HashMap::new()),
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
        let ptr = &self.struct_layouts as *const HashMap<String, StructLayout>
            as *mut HashMap<String, StructLayout>;
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
    pub(super) globals: HashMap<String, GlobalInfo>,
    // Set of known function names
    pub(super) known_functions: HashSet<String>,
    // Set of already-defined function bodies
    pub(super) defined_functions: HashSet<String>,
    // Set of function names declared with static linkage
    pub(super) static_functions: HashSet<String>,
    /// Type-system state (struct layouts, typedefs, enum constants, type caches)
    pub(super) types: TypeContext,
    /// Metadata about known functions (consolidated FuncSig)
    pub(super) func_meta: FunctionMeta,
    /// Set of emitted global variable names (O(1) dedup)
    pub(super) emitted_global_names: HashSet<String>,
}

/// Tracks how each original C parameter maps to IR parameters after ABI decomposition.
///
/// Used by `build_ir_params` to record what happened to each original parameter,
/// so `allocate_function_params` knows which registration method to call.
#[derive(Debug)]
enum ParamKind {
    /// Normal parameter: 1 IR param at the given index
    Normal(usize),
    /// Struct/union or non-decomposed complex parameter passed by value
    Struct(usize),
    /// Complex parameter passed as two decomposed FP params (real_ir_idx, imag_ir_idx)
    ComplexDecomposed(usize, usize),
    /// Complex float packed into single F64 (x86-64 only)
    ComplexFloatPacked(usize),
}

/// Result of building the IR parameter list for a function.
///
/// Produced by `build_ir_params`, consumed by `allocate_function_params` and
/// `finalize_function`.
struct IrParamBuildResult {
    /// The IR parameters to use for the function signature
    params: Vec<IrParam>,
    /// Per-original-parameter: how it maps to IR params (indexed by original param index)
    param_kinds: Vec<ParamKind>,
    /// Whether the function uses sret (hidden first pointer param)
    uses_sret: bool,
}

impl Lowerer {
    pub fn new(target: Target) -> Self {
        Self {
            target,
            next_label: 0,
            next_string: 0,
            next_anon_struct: 0,
            next_static_local: 0,
            module: IrModule::new(),
            func_state: None,
            globals: HashMap::new(),
            known_functions: HashSet::new(),
            defined_functions: HashSet::new(),
            static_functions: HashSet::new(),
            types: TypeContext::new(),
            func_meta: FunctionMeta::default(),
            emitted_global_names: HashSet::new(),
        }
    }

    /// Create a new Lowerer with a pre-populated TypeContext from semantic analysis.
    /// The sema-provided TypeContext contains typedefs, enum constants, struct layouts,
    /// and function typedef info collected during the sema pass.
    pub fn with_type_context(target: Target, type_context: TypeContext) -> Self {
        Self {
            target,
            next_label: 0,
            next_string: 0,
            next_anon_struct: 0,
            next_static_local: 0,
            module: IrModule::new(),
            func_state: None,
            globals: HashMap::new(),
            known_functions: HashSet::new(),
            defined_functions: HashSet::new(),
            static_functions: HashSet::new(),
            types: type_context,
            func_meta: FunctionMeta::default(),
            emitted_global_names: HashSet::new(),
        }
    }

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

    /// Returns true if the target uses x86-64 style packed _Complex float ABI
    /// (two F32s packed into a single F64/xmm register).
    /// Returns false for ARM/RISC-V which pass _Complex float as two separate F32 registers.
    pub(super) fn uses_packed_complex_float(&self) -> bool {
        self.target == Target::X86_64
    }

    /// Look up the shared type metadata for a variable by name.
    ///
    /// Checks locals first, then globals. Returns `&VarInfo` which provides
    /// access to the 8 shared fields (ty, elem_size, is_array, pointee_type,
    /// struct_layout, is_struct, array_dim_strides, c_type).
    ///
    /// Use this instead of duplicating `if let Some(info) = self.func_mut().locals.get(name)
    /// ... if let Some(ginfo) = self.globals.get(name)` when only shared fields
    /// are needed.
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
    ///
    /// Replaces the previous pattern of dispatching between `resolve_member_field_ctype`
    /// and `resolve_pointer_member_field_ctype` at every call site.
    pub(super) fn resolve_field_ctype(&self, base_expr: &Expr, field_name: &str, is_pointer_access: bool) -> Option<CType> {
        if is_pointer_access {
            self.resolve_pointer_member_field_ctype(base_expr, field_name)
        } else {
            self.resolve_member_field_ctype(base_expr, field_name)
        }
    }


    /// Push a new scope frame onto both TypeContext and FunctionBuildState scope stacks.
    /// Call this at the start of a compound statement or function body.
    pub(super) fn push_scope(&mut self) {
        self.types.push_scope();
        self.func_mut().push_scope();
    }

    /// Pop the top scope frame from both TypeContext and FunctionBuildState,
    /// undoing all scoped changes made in that scope.
    pub(super) fn pop_scope(&mut self) {
        self.func_mut().pop_scope();
        self.types.pop_scope();
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

    /// Insert a var ctype, tracking the change in the current scope frame.
    pub(super) fn insert_var_ctype_scoped(&mut self, name: String, ctype: CType) {
        self.func_mut().insert_var_ctype_scoped(name, ctype);
    }

    pub fn lower(mut self, tu: &TranslationUnit) -> IrModule {
        // Snapshot sema's pre-populated type info for debug assertions.
        // After the lowerer's own pass-0 collection, we verify agreement on
        // named types. Anonymous struct keys use independent counters so they
        // may legitimately differ between sema and lowerer.
        #[cfg(debug_assertions)]
        let sema_enum_constants = self.types.enum_constants.clone();
        #[cfg(debug_assertions)]
        let sema_struct_layouts: HashMap<String, StructLayout> = self.types.struct_layouts.clone();

        // Seed builtin typedefs (matching the parser's pre-seeded typedef names)
        self.seed_builtin_typedefs();
        // Seed known libc math function signatures for correct calling convention
        self.seed_libc_math_functions();

        // Collect all enum constants FIRST, before struct registration.
        // Struct layouts depend on enum constants for array sizes
        // (e.g., `void *arr[ENUM_COUNT]`), so enum constants must be available
        // before any struct layout computation occurs.
        self.collect_all_enum_constants(tu);

        // Pass 0: Collect all global typedef declarations so that function return
        // types and parameter types that use typedefs can be resolved in pass 1.
        self.collect_all_typedefs(tu);

        // Debug assertions: verify sema's pre-populated type info matches what the
        // lowerer independently computed. This validates that sema and the lowerer
        // agree on type resolution before we trust sema's output in commit 8.
        // Note: anonymous struct keys (like __anon_struct_N) use independent counters
        // in sema and the lowerer, so they may differ. Only check named types.
        #[cfg(debug_assertions)]
        {
            // Check enum constants: sema should have collected all file-scope enums
            for (name, sema_val) in &sema_enum_constants {
                if let Some(lowerer_val) = self.types.enum_constants.get(name) {
                    if sema_val != lowerer_val {
                        eprintln!(
                            "WARNING: sema/lowerer enum constant mismatch for '{}': sema={}, lowerer={}",
                            name, sema_val, lowerer_val
                        );
                    }
                }
            }

            // Check struct layouts for named (non-anonymous) structs/unions.
            for (key, sema_layout) in &sema_struct_layouts {
                // Skip anonymous struct layouts (counter divergence is expected)
                if key.starts_with("__anon_struct_") {
                    continue;
                }
                if let Some(lowerer_layout) = self.types.struct_layouts.get(key) {
                    if sema_layout.size != lowerer_layout.size {
                        eprintln!(
                            "WARNING: sema/lowerer struct layout size mismatch for '{}': sema={}, lowerer={}",
                            key, sema_layout.size, lowerer_layout.size
                        );
                    }
                    if sema_layout.align != lowerer_layout.align {
                        eprintln!(
                            "WARNING: sema/lowerer struct layout align mismatch for '{}': sema={}, lowerer={}",
                            key, sema_layout.align, lowerer_layout.align
                        );
                    }
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

        // Collect constructor/destructor attributes from function definitions and declarations
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
                    }
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
                    // from headers that are never called). Non-static inline functions must
                    // still be emitted because they have external linkage (GNU inline semantics)
                    // and may be referenced from other translation units.
                    let can_skip = if func.is_static {
                        // static or static inline: internal linkage, safe to skip if unreferenced
                        true
                    } else if func.is_inline {
                        // plain inline (non-static): external linkage, must emit
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
            }
        }
        self.module
    }

    /// Register function metadata (return type, param types, variadic, sret) for
    /// a function name. This shared helper eliminates the triplicated pattern in `lower()`
    /// where function definitions, extern declarations, and typedef-based declarations
    /// all needed to register the same metadata fields.
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

        // Compute return type, wrapping with pointer levels if needed
        let mut ret_ty = self.type_spec_to_ir(ret_type_spec);
        if ptr_count > 0 {
            ret_ty = IrType::Ptr;
        }
        // Complex return types need special IR type overrides:
        // _Complex double: real in first FP register (F64), imag in second
        // _Complex float:
        //   x86-64: packed two F32 in one xmm register -> F64
        //   ARM/RISC-V: real in first FP register -> F32, imag in second FP register
        if ptr_count == 0 {
            let ret_ctype = self.type_spec_to_ctype(ret_type_spec);
            if matches!(ret_ctype, CType::ComplexDouble) {
                ret_ty = IrType::F64;
            } else if matches!(ret_ctype, CType::ComplexFloat) {
                if self.uses_packed_complex_float() {
                    ret_ty = IrType::F64;
                } else {
                    ret_ty = IrType::F32;
                }
            }
        }
        // Track CType for pointer-returning functions
        let mut return_ctype = None;
        if ret_ty == IrType::Ptr {
            let base_ctype = self.type_spec_to_ctype(ret_type_spec);
            let ret_ctype = if ptr_count > 0 {
                let mut ct = base_ctype;
                for _ in 0..ptr_count {
                    ct = CType::Pointer(Box::new(ct));
                }
                ct
            } else {
                base_ctype
            };
            return_ctype = Some(ret_ctype);
        }

        // Record complex return types for expr_ctype resolution
        if ptr_count == 0 {
            let ret_ct = self.type_spec_to_ctype(ret_type_spec);
            if ret_ct.is_complex() {
                self.types.func_return_ctypes.insert(name.to_string(), ret_ct);
            }
        }

        // Detect struct/complex returns that need special ABI handling.
        let mut sret_size = None;
        let mut two_reg_ret_size = None;
        if ptr_count == 0 {
            let ret_ct = self.type_spec_to_ctype(ret_type_spec);
            if matches!(ret_ct, CType::Struct(_) | CType::Union(_)) {
                let size = self.sizeof_type(ret_type_spec);
                if size > 16 {
                    sret_size = Some(size);
                } else if size > 8 {
                    two_reg_ret_size = Some(size);
                }
            }
            if matches!(ret_ct, CType::ComplexLongDouble) {
                let size = self.sizeof_type(ret_type_spec);
                sret_size = Some(size);
            }
        }

        // Collect parameter types, with K&R float->double promotion
        let param_tys: Vec<IrType> = params.iter().map(|p| {
            let ty = self.type_spec_to_ir(&p.type_spec);
            if is_kr && ty == IrType::F32 { IrType::F64 } else { ty }
        }).collect();
        let param_bool_flags: Vec<bool> = params.iter().map(|p| {
            self.is_type_bool(&p.type_spec)
        }).collect();
        // Collect parameter CTypes for complex argument conversion
        let param_ctypes: Vec<CType> = params.iter().map(|p| {
            self.type_spec_to_ctype(&p.type_spec)
        }).collect();

        // Collect per-parameter struct sizes for by-value struct passing ABI
        let param_struct_sizes: Vec<Option<usize>> = params.iter().map(|p| {
            if self.is_type_struct_or_union(&p.type_spec) {
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

    pub(super) fn fresh_value(&mut self) -> Value {
        let v = Value(self.func_mut().next_value);
        self.func_mut().next_value += 1;
        v
    }

    pub(super) fn fresh_label(&mut self, prefix: &str) -> String {
        let l = format!(".L{}_{}", prefix, self.next_label);
        self.next_label += 1;
        l
    }

    /// Intern a string literal: add it to the module's .rodata string table and
    /// return its unique label. Deduplicates the pattern of creating .Lstr{N}
    /// labels that appeared at 6+ call sites.
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

    pub(super) fn emit(&mut self, inst: Instruction) {
        self.func_mut().instrs.push(inst);
    }

    // --- IR emission helpers ---
    // These reduce the common 4-line fresh_value+emit(Instruction::*) pattern to 1 line.

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
        self.emit(Instruction::Store { val, ptr: addr, ty });
    }

    /// Lower an expression, cast to target type, then store at base + byte_offset.
    pub(super) fn emit_init_expr_to_offset(&mut self, e: &Expr, base: Value, byte_offset: usize, target_ty: IrType) {
        let expr_ty = self.get_expr_type(e);
        let val = self.lower_expr(e);
        let val = self.emit_implicit_cast(val, expr_ty, target_ty);
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
            label: self.func_mut().current_label.clone(),
            instructions: std::mem::take(&mut self.func_mut().instrs),
            terminator: term,
        };
        self.func_mut().blocks.push(block);
    }

    pub(super) fn start_block(&mut self, label: String) {
        self.func_mut().current_label = label;
        self.func_mut().instrs.clear();
    }

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
        self.start_block("entry".to_string());
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
        let return_type = self.type_spec_to_ir(&func.return_type);

        let ret_ctype = self.type_spec_to_ctype(&func.return_type);
        if ret_ctype.is_complex() {
            self.types.func_return_ctypes.insert(func.name.clone(), ret_ctype.clone());
        }
        return_type
    }

    /// Build the IR parameter list for a function, handling ABI decomposition.
    fn build_ir_params(&mut self, func: &FunctionDef) -> IrParamBuildResult {
        let uses_sret = self.func_meta.sigs.get(&func.name).and_then(|s| s.sret_size).is_some();
        let uses_packed_cf = self.uses_packed_complex_float();
        let mut params: Vec<IrParam> = Vec::new();
        let mut param_kinds: Vec<ParamKind> = Vec::new();

        if uses_sret {
            params.push(IrParam { name: "__sret_ptr".to_string(), ty: IrType::Ptr, struct_size: None });
        }

        for (_orig_idx, p) in func.params.iter().enumerate() {
            let param_ct = self.type_spec_to_ctype(&p.type_spec);
            let name = p.name.clone().unwrap_or_default();

            let is_complex_decomposed = if uses_packed_cf {
                matches!(param_ct, CType::ComplexDouble | CType::ComplexLongDouble)
            } else {
                param_ct.is_complex()
            };
            let is_complex_float_packed = uses_packed_cf && matches!(param_ct, CType::ComplexFloat);

            if is_complex_decomposed {
                let comp_ty = Self::complex_component_ir_type(&param_ct);
                let real_idx = params.len();
                params.push(IrParam { name: format!("{}_real", name), ty: comp_ty, struct_size: None });
                let imag_idx = params.len();
                params.push(IrParam { name: format!("{}_imag", name), ty: comp_ty, struct_size: None });
                param_kinds.push(ParamKind::ComplexDecomposed(real_idx, imag_idx));
            } else if is_complex_float_packed {
                let ir_idx = params.len();
                params.push(IrParam { name: format!("{}_packed", name), ty: IrType::F64, struct_size: None });
                param_kinds.push(ParamKind::ComplexFloatPacked(ir_idx));
            } else {
                let ty = self.type_spec_to_ir(&p.type_spec);
                let ty = if func.is_kr && ty == IrType::F32 { IrType::F64 } else { ty };
                let struct_size = if matches!(param_ct, CType::Struct(_) | CType::Union(_)) {
                    Some(self.sizeof_type(&p.type_spec))
                } else {
                    None
                };
                let ir_idx = params.len();
                params.push(IrParam { name, ty, struct_size });
                if struct_size.is_some() || param_ct.is_complex() {
                    param_kinds.push(ParamKind::Struct(ir_idx));
                } else {
                    param_kinds.push(ParamKind::Normal(ir_idx));
                }
            }
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
                self.emit(Instruction::Alloca { dest: alloca, ty: IrType::Ptr, size: 8, align: 0 });
                self.func_mut().sret_ptr = Some(alloca);
                ir_allocas.push(alloca);
                continue;
            }
            let size = param.ty.size().max(param.struct_size.unwrap_or(param.ty.size()));
            self.emit(Instruction::Alloca { dest: alloca, ty: param.ty, size, align: 0 });
            ir_allocas.push(alloca);
        }

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

        let name = orig_param.name.clone().unwrap_or_default();
        self.insert_local_scoped(name, LocalInfo {
            var: VarInfo { ty, elem_size, is_array: false, pointee_type, struct_layout, is_struct: false, array_dim_strides, c_type },
            alloca, alloc_size: param_size, is_bool, static_global_name: None, vla_strides: vec![], vla_size: None,
        });

        // Register function pointer parameter signatures for indirect calls
        if let Some(ref fptr_params) = orig_param.fptr_params {
            let ret_ty = match &orig_param.type_spec {
                TypeSpecifier::Pointer(inner) => self.type_spec_to_ir(inner),
                _ => self.type_spec_to_ir(&orig_param.type_spec),
            };
            if let Some(ref name) = orig_param.name {
                let param_tys: Vec<IrType> = fptr_params.iter().map(|fp| self.type_spec_to_ir(&fp.type_spec)).collect();
                self.func_meta.ptr_sigs.insert(name.clone(), FuncSig {
                    return_type: ret_ty, return_ctype: None, param_types: param_tys,
                    param_ctypes: Vec::new(), param_bool_flags: Vec::new(), is_variadic: false,
                    sret_size: None, two_reg_ret_size: None, param_struct_sizes: Vec::new(),
                });
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
            var: VarInfo { ty: IrType::Ptr, elem_size: 0, is_array: false, pointee_type: None, struct_layout: layout, is_struct: true, array_dim_strides: vec![], c_type },
            alloca, alloc_size: size, is_bool: false, static_global_name: None, vla_strides: vec![], vla_size: None,
        });
    }

    /// Register a packed complex float parameter (x86-64 only) as a local variable.
    fn register_packed_complex_float_param(&mut self, orig_param: &ParamDecl, alloca: Value) {
        let ct = self.type_spec_to_ctype(&orig_param.type_spec);
        let name = orig_param.name.clone().unwrap_or_default();
        self.insert_local_scoped(name, LocalInfo {
            var: VarInfo { ty: IrType::Ptr, elem_size: 0, is_array: false, pointee_type: None, struct_layout: None, is_struct: true, array_dim_strides: vec![], c_type: Some(ct) },
            alloca, alloc_size: 8, is_bool: false, static_global_name: None, vla_strides: vec![], vla_size: None,
        });
    }

    /// Reconstruct a decomposed complex parameter from its real/imag Phase 1 allocas.
    fn reconstruct_decomposed_complex_param(&mut self, orig_param: &ParamDecl, real_alloca: Value, imag_alloca: Value) {
        let ct = self.type_spec_to_ctype(&orig_param.type_spec);
        let comp_ty = Self::complex_component_ir_type(&ct);
        let comp_size = Self::complex_component_size(&ct);
        let complex_size = ct.size();

        let complex_alloca = self.fresh_value();
        self.emit(Instruction::Alloca { dest: complex_alloca, ty: IrType::Ptr, size: complex_size, align: 0 });

        let real_val = self.fresh_value();
        self.emit(Instruction::Load { dest: real_val, ptr: real_alloca, ty: comp_ty });
        self.emit(Instruction::Store { val: Operand::Value(real_val), ptr: complex_alloca, ty: comp_ty });

        let imag_val = self.fresh_value();
        self.emit(Instruction::Load { dest: imag_val, ptr: imag_alloca, ty: comp_ty });
        let imag_ptr = self.fresh_value();
        self.emit(Instruction::GetElementPtr { dest: imag_ptr, base: complex_alloca, offset: Operand::Const(IrConst::I64(comp_size as i64)), ty: IrType::I8 });
        self.emit(Instruction::Store { val: Operand::Value(imag_val), ptr: imag_ptr, ty: comp_ty });

        let name = orig_param.name.clone().unwrap_or_default();
        self.func_mut().locals.insert(name, LocalInfo {
            var: VarInfo { ty: IrType::Ptr, elem_size: 0, is_array: false, pointee_type: None, struct_layout: None, is_struct: true, array_dim_strides: vec![], c_type: Some(ct) },
            alloca: complex_alloca, alloc_size: complex_size, is_bool: false, static_global_name: None, vla_strides: vec![], vla_size: None,
        });
    }

    /// Handle K&R float promotion: narrow double params back to declared float type.
    fn handle_kr_float_promotion(&mut self, func: &FunctionDef) {
        if !func.is_kr { return; }
        for param in &func.params {
            if self.type_spec_to_ir(&param.type_spec) != IrType::F32 { continue; }
            let name = param.name.clone().unwrap_or_default();
            let local_info = match self.func_mut().locals.get(&name).cloned() { Some(i) => i, None => continue };
            let f64_val = self.fresh_value();
            self.emit(Instruction::Load { dest: f64_val, ptr: local_info.alloca, ty: IrType::F64 });
            let f32_val = self.emit_cast_val(Operand::Value(f64_val), IrType::F64, IrType::F32);
            let f32_alloca = self.fresh_value();
            self.emit(Instruction::Alloca { dest: f32_alloca, ty: IrType::F32, size: 4, align: 0 });
            self.emit(Instruction::Store { val: Operand::Value(f32_val), ptr: f32_alloca, ty: IrType::F32 });
            if let Some(local) = self.func_mut().locals.get_mut(&name) {
                local.alloca = f32_alloca; local.ty = IrType::F32; local.alloc_size = 4;
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

        let is_static = func.is_static || self.static_functions.contains(&func.name);
        let ir_func = IrFunction {
            name: func.name.clone(), return_type, params,
            blocks: std::mem::take(&mut self.func_mut().blocks),
            is_variadic: func.variadic, is_declaration: false, is_static, stack_size: 0,
        };
        self.module.functions.push(ir_func);
        self.pop_scope();
        self.func_state = None;
    }

    /// For pointer-to-array function parameters with VLA (runtime) dimensions,
    /// compute strides at runtime and store them in the LocalInfo.
    /// Example: `void f(int rows, int cols, int m[rows][cols])`
    /// The parameter `m` has type `Pointer(Array(Int, cols))` where `cols` is a runtime variable.
    /// We need to compute stride[0] = cols * sizeof(int) at runtime.
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
            if let TypeSpecifier::Pointer(inner) = ts {
                let dim_infos = self.collect_vla_dims(inner);
                if dim_infos.iter().any(|d| d.is_vla) {
                    vla_params.push((param_name, dim_infos));
                }
            } else {
                // Check CType for typedef'd pointer-to-array
                let ctype = self.type_spec_to_ctype(&param.type_spec);
                if let CType::Pointer(ref inner_ct) = ctype {
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

            // Compute strides from innermost to outermost
            // For int m[rows][cols]: dims = [cols], base_elem_size = 4
            // stride[1] = 4 (base element)
            // stride[0] = cols * 4 (row stride)
            //
            // For int m[a][b][c]: dims = [b, c], base_elem_size = 4
            // stride[2] = 4
            // stride[1] = c * 4
            // stride[0] = b * c * 4

            // Find the base element size (product of all constant inner dims * scalar size)
            let base_elem_size = dim_infos.last().map_or(1, |d| d.base_elem_size);

            // Start with base element stride
            let mut current_stride: Option<Value> = None;
            let mut current_const_stride = base_elem_size;

            // Process dimensions from innermost to outermost
            for (i, dim_info) in dim_infos.iter().enumerate().rev() {
                if dim_info.is_vla {
                    // Load the VLA dimension variable
                    let dim_val = self.load_vla_dim_value(&dim_info.dim_expr_name);

                    // Compute stride = dim_val * current_stride
                    let stride_val = if let Some(prev) = current_stride {
                        // Runtime stride * runtime dim
                        self.emit_binop_val(IrBinOp::Mul, Operand::Value(dim_val), Operand::Value(prev), IrType::I64)
                    } else {
                        // Constant stride * runtime dim
                        self.emit_binop_val(IrBinOp::Mul, Operand::Value(dim_val), Operand::Const(IrConst::I64(current_const_stride as i64)), IrType::I64)
                    };

                    // stride[i] is the stride for subscript at depth i
                    // which is used when accessing a[i] where a is the array at this level
                    vla_strides[i] = Some(stride_val);
                    current_stride = Some(stride_val);
                    current_const_stride = 0; // no longer constant
                } else {
                    // Constant dimension
                    let const_dim = dim_info.const_size.unwrap_or(1) as usize;
                    if let Some(prev) = current_stride {
                        // Multiply runtime stride by constant dim
                        let result = self.emit_binop_val(IrBinOp::Mul, Operand::Value(prev), Operand::Const(IrConst::I64(const_dim as i64)), IrType::I64);
                        vla_strides[i] = Some(result);
                        current_stride = Some(result);
                    } else {
                        current_const_stride *= const_dim;
                        // This level's stride is still compile-time constant
                        // vla_strides[i] remains None (use array_dim_strides)
                    }
                }
            }

            // Update the LocalInfo with VLA strides
            if let Some(local) = self.func_mut().locals.get_mut(&param_name) {
                local.vla_strides = vla_strides;
            }
        }
    }

    /// Load the value of a VLA dimension variable (a function parameter).
    fn load_vla_dim_value(&mut self, dim_name: &str) -> Value {
        if let Some(info) = self.func_mut().locals.get(dim_name).cloned() {
            let loaded = self.fresh_value();
            self.emit(Instruction::Load {
                dest: loaded,
                ptr: info.alloca,
                ty: info.ty,
            });
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
    /// For `Pointer(Array(Array(Int, c), b))`, returns [{name:"b", is_vla:true}, {name:"c", is_vla:true}]
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
                        // Non-constant dimension - extract the variable name
                        let name = Self::extract_dim_expr_name(expr);
                        (true, name, None)
                    }
                } else {
                    (false, String::new(), None)
                };

                // Compute base_elem_size for this level
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
    /// Handles simple cases like Identifier("cols").
    fn extract_dim_expr_name(expr: &Expr) -> String {
        match expr {
            Expr::Identifier(name, _) => name.clone(),
            _ => String::new(),
        }
    }

    fn lower_global_decl(&mut self, decl: &Declaration) {
        // Register any struct/union definitions
        self.register_struct_type(&decl.type_spec);

        // Collect enum constants from top-level enum type declarations
        self.collect_enum_constants(&decl.type_spec);

        // If this is a typedef, register the mapping and skip variable emission
        if decl.is_typedef {
            for declarator in &decl.declarators {
                if !declarator.name.is_empty() {
                    // Store CType directly in typedefs map
                    let resolved_ctype = self.build_full_ctype(&decl.type_spec, &declarator.derived);
                    self.types.typedefs.insert(declarator.name.clone(), resolved_ctype);
                }
            }
            return;
        }

        for declarator in &decl.declarators {
            if declarator.name.is_empty() {
                continue; // Skip anonymous declarations (e.g., struct definitions)
            }

            // Skip function declarations (prototypes), but NOT function pointer variables.
            // Function declarations use DerivedDeclarator::Function (e.g., int func(int);
            // or char *func(int);). Function pointer variables use FunctionPointer
            // (e.g., int (*fp)(int) = add;).
            if declarator.derived.iter().any(|d| matches!(d, DerivedDeclarator::Function(_, _)))
                && !declarator.derived.iter().any(|d| matches!(d, DerivedDeclarator::FunctionPointer(_, _)))
                && declarator.init.is_none()
            {
                continue;
            }

            // Skip declarations using function typedefs (e.g., `func_t add;` where
            // func_t is `typedef int func_t(int);`). These declare functions, not variables.
            if declarator.init.is_none() {
                if let TypeSpecifier::TypedefName(tname) = &decl.type_spec {
                    if self.types.function_typedefs.contains_key(tname) {
                        continue;
                    }
                }
            }

            // extern without initializer: track the type but don't emit a .bss entry
            // (the definition will come from another translation unit)
            if decl.is_extern && declarator.init.is_none() {
                if !self.globals.contains_key(&declarator.name) {
                    let da = self.analyze_declaration(&decl.type_spec, &declarator.derived);
                    self.globals.insert(declarator.name.clone(), GlobalInfo::from_analysis(&da));
                }
                continue;
            }

            // If this global already exists (e.g., `extern int a; int a = 0;`),
            // handle tentative definitions and re-declarations correctly.
            if self.globals.contains_key(&declarator.name) {
                if declarator.init.is_none() {
                    // Check if this is a tentative definition (non-extern without init)
                    // that needs to be emitted because only an extern was previously tracked
                    if self.emitted_global_names.contains(&declarator.name) {
                        // Already defined in .data/.bss, skip duplicate
                        continue;
                    }
                    // Not yet emitted: this is a tentative definition after an extern declaration.
                    // Fall through to emit it as zero-initialized.
                } else {
                    // Has initializer: remove the previous zero-init/extern global and re-emit with init
                    self.module.globals.retain(|g| g.name != declarator.name);
                    self.emitted_global_names.remove(&declarator.name);
                }
            }

            let mut da = self.analyze_declaration(&decl.type_spec, &declarator.derived);

            // For global arrays-of-pointers, clear struct_layout so the array is treated
            // as a pointer array, not a struct array.
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

            // Extern declarations without initializers: track but don't emit storage
            let is_extern_decl = decl.is_extern && declarator.init.is_none();

            // Determine initializer
            let init = if let Some(ref initializer) = declarator.init {
                self.lower_global_init(initializer, &decl.type_spec, da.base_ty, da.is_array, da.elem_size, da.actual_alloc_size, &da.struct_layout, &da.array_dim_strides)
            } else {
                GlobalInit::Zero
            };

            // Track this global variable
            self.globals.insert(declarator.name.clone(), GlobalInfo::from_analysis(&da));

            // Use C type alignment for long double (16) instead of IrType::F64 alignment (8).
            // Also respect explicit __attribute__((aligned(N))) or _Alignas(N) overrides.
            let align = {
                let c_align = self.alignof_type(&decl.type_spec);
                let natural = if c_align > 0 { c_align.max(da.var_ty.align()) } else { da.var_ty.align() };
                if let Some(explicit) = decl.alignment {
                    natural.max(explicit)
                } else {
                    natural
                }
            };

            let is_static = decl.is_static;

            // For struct initializers emitted as byte arrays, set element type to I8
            // so the backend emits .byte directives for each element.
            let global_ty = if matches!(&init, GlobalInit::Array(vals) if !vals.is_empty() && matches!(vals[0], IrConst::I8(_))) {
                IrType::I8
            } else if da.is_struct && matches!(init, GlobalInit::Array(_)) {
                IrType::I8
            } else {
                da.var_ty
            };

            // For structs with FAMs, the init byte array may be larger than layout.size.
            // Use the actual init data size if it exceeds the computed alloc size.
            let final_size = match &init {
                GlobalInit::Array(vals) if da.is_struct && vals.len() > da.actual_alloc_size => vals.len(),
                _ => da.actual_alloc_size,
            };

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
            });
        }
    }

    /// Collect all global typedef declarations so function return types and
    /// parameter types that use typedefs can be resolved in later passes.
    fn collect_all_typedefs(&mut self, tu: &TranslationUnit) {
        for decl in &tu.decls {
            if let ExternalDecl::Declaration(decl) = decl {
                if decl.is_typedef {
                    for declarator in &decl.declarators {
                        if declarator.name.is_empty() {
                            continue;
                        }
                        if let Some(fti) = extract_func_typedef_info(&decl.type_spec, &declarator.derived) {
                            self.types.function_typedefs.insert(declarator.name.clone(), fti);
                        }
                        if let Some(fti) = extract_fptr_typedef_info(&decl.type_spec, &declarator.derived) {
                            self.types.func_ptr_typedefs.insert(declarator.name.clone());
                            self.types.func_ptr_typedef_info.insert(declarator.name.clone(), fti);
                        }
                        let resolved_ctype = self.build_full_ctype(&decl.type_spec, &declarator.derived);
                        self.types.typedefs.insert(declarator.name.clone(), resolved_ctype);
                    }
                }
                self.register_struct_type(&decl.type_spec);
            }
        }
    }

    /// Collect all enum constants from the entire translation unit.
    fn collect_all_enum_constants(&mut self, tu: &TranslationUnit) {
        // Only collect file-scope (global) enum constants in the pre-pass.
        // Function-body enum constants are collected during lowering with proper
        // scope tracking (save/restore in lower_compound_stmt), so they don't
        // leak across block scopes.
        for decl in &tu.decls {
            match decl {
                ExternalDecl::Declaration(d) => {
                    self.collect_enum_constants(&d.type_spec);
                }
                ExternalDecl::FunctionDef(func) => {
                    // Only collect enums from the return type (file-scope),
                    // not from the function body (those are block-scoped).
                    self.collect_enum_constants(&func.return_type);
                }
            }
        }
    }

    /// Collect enum constants from a type specifier.
    /// When `scoped` is true, uses scope-tracked insertion (for block-scoped enums
    /// that need undo on scope exit). When false, inserts directly (for file-scope).
    fn collect_enum_constants_impl(&mut self, ts: &TypeSpecifier, scoped: bool) {
        match ts {
            TypeSpecifier::Enum(_, Some(variants)) => {
                let mut next_val: i64 = 0;
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
                    next_val += 1;
                }
            }
            // Recurse into struct/union fields to find enum definitions within them
            TypeSpecifier::Struct(_, Some(fields), _, _, _) | TypeSpecifier::Union(_, Some(fields), _, _, _) => {
                for field in fields {
                    self.collect_enum_constants_impl(&field.type_spec, scoped);
                }
            }
            // Unwrap Array and Pointer wrappers to find nested enum definitions.
            // This handles cases like: enum { A, B } volatile arr[2][2]; inside structs,
            // where the field type becomes Array(Array(Enum(...))).
            TypeSpecifier::Array(inner, _) | TypeSpecifier::Pointer(inner) => {
                self.collect_enum_constants_impl(inner, scoped);
            }
            _ => {}
        }
    }

    /// Collect enum constants from a type specifier (file-scope, direct insertion).
    pub(super) fn collect_enum_constants(&mut self, ts: &TypeSpecifier) {
        self.collect_enum_constants_impl(ts, false);
    }

    /// Collect enum constants from a type specifier, using scoped insertion
    /// so that constants are tracked in the current scope frame for undo on scope exit.
    pub(super) fn collect_enum_constants_scoped(&mut self, ts: &TypeSpecifier) {
        self.collect_enum_constants_impl(ts, true);
    }

    // NOTE: collect_enum_constants_from_compound and collect_enum_constants_from_stmt
    // were removed. Enum constants inside function bodies are now collected during
    // lowering (in lower_compound_stmt) with proper scope save/restore, rather than
    // pre-collected globally. This fixes enum constant scope leakage across blocks.

    /// Get or create a unique IR label for a user-defined goto label.
    pub(super) fn get_or_create_user_label(&mut self, name: &str) -> String {
        let key = format!("{}::{}", self.func_mut().name, name);
        if let Some(label) = self.func_mut().user_labels.get(&key) {
            label.clone()
        } else {
            let label = self.fresh_label(&format!("user_{}", name));
            self.func_mut().user_labels.insert(key, label.clone());
            label
        }
    }

    /// Copy a string literal's bytes into an alloca at a given byte offset,
    /// followed by a null terminator. Used for `char s[] = "hello"` and
    /// string elements in array initializer lists.
    pub(super) fn emit_string_to_alloca(&mut self, alloca: Value, s: &str, base_offset: usize) {
        // Use chars() to get raw byte values - each char is U+0000..U+00FF representing a C byte
        let str_bytes: Vec<u8> = s.chars().map(|c| c as u8).collect();
        for (j, &byte) in str_bytes.iter().enumerate() {
            let val = Operand::Const(IrConst::I8(byte as i8));
            let offset = Operand::Const(IrConst::I64((base_offset + j) as i64));
            let addr = self.fresh_value();
            self.emit(Instruction::GetElementPtr {
                dest: addr, base: alloca, offset, ty: IrType::I8,
            });
            self.emit(Instruction::Store { val, ptr: addr, ty: IrType::I8 });
        }
        // Null terminator
        let null_offset = Operand::Const(IrConst::I64((base_offset + str_bytes.len()) as i64));
        let null_addr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: null_addr, base: alloca, offset: null_offset, ty: IrType::I8,
        });
        self.emit(Instruction::Store {
            val: Operand::Const(IrConst::I8(0)), ptr: null_addr, ty: IrType::I8,
        });
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
            self.emit(Instruction::Store { val, ptr: addr, ty: IrType::I32 });
        }
        // Null terminator (4 bytes of zero)
        let null_byte_offset = base_offset + s.chars().count() * 4;
        let null_offset = Operand::Const(IrConst::I64(null_byte_offset as i64));
        let null_addr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: null_addr, base: alloca, offset: null_offset, ty: IrType::I8,
        });
        self.emit(Instruction::Store {
            val: Operand::Const(IrConst::I32(0)), ptr: null_addr, ty: IrType::I32,
        });
    }

    /// Emit a single element store at a given byte offset in an alloca.
    /// Handles implicit type cast from the expression type to the target type.
    pub(super) fn emit_array_element_store(
        &mut self, alloca: Value, val: Operand, offset: usize, ty: IrType,
    ) {
        let offset_val = Operand::Const(IrConst::I64(offset as i64));
        let elem_addr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: elem_addr, base: alloca, offset: offset_val, ty,
        });
        self.emit(Instruction::Store { val, ptr: elem_addr, ty });
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
            self.emit(Instruction::Store {
                val: Operand::Const(IrConst::I64(0)),
                ptr: addr,
                ty: IrType::I64,
            });
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
            self.emit(Instruction::Store {
                val: Operand::Const(IrConst::I8(0)),
                ptr: addr,
                ty: IrType::I8,
            });
            offset += 1;
        }
    }

    /// Zero-initialize an entire alloca. Delegates to zero_init_region with offset 0.
    pub(super) fn zero_init_alloca(&mut self, alloca: Value, total_size: usize) {
        self.zero_init_region(alloca, 0, total_size);
    }

    /// Perform shared declaration analysis for both local and global variable declarations.
    ///
    /// Computes all type-related properties (base type, array info, pointer info, struct layout,
    /// pointee type, etc.) that both `lower_local_decl` and `lower_global_decl` need.
    /// This eliminates the ~80 lines of duplicated type analysis that previously existed
    /// in both functions.
    ///
    /// Does NOT handle unsized array fixup from initializers (caller must do that since
    /// the initializer processing differs between local and global declarations).
    pub(super) fn analyze_declaration(&self, type_spec: &TypeSpecifier, derived: &[DerivedDeclarator]) -> DeclAnalysis {
        let mut base_ty = self.type_spec_to_ir(type_spec);
        let (alloc_size, elem_size, is_array, is_pointer, array_dim_strides) =
            self.compute_decl_info(type_spec, derived);

        // For typedef'd array types (e.g., typedef int a[]; a x = {...}),
        // type_spec_to_ir returns Ptr (array decays to pointer), but we need
        // the element type for correct storage/initialization.
        if is_array && base_ty == IrType::Ptr && !is_pointer {
            // First try TypeSpecifier::Array peeling
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
            // Fall back to CType for typedef'd arrays (e.g., va_list)
            if !found {
                let ctype = self.type_spec_to_ctype(type_spec);
                let mut ct = &ctype;
                while let CType::Array(inner, _) = ct {
                    ct = inner.as_ref();
                }
                base_ty = Self::ctype_to_ir(ct);
            }
        }

        // For array-of-pointers (int *arr[N]) or array-of-function-pointers,
        // the element type is Ptr, not the base type.
        // Also detect typedef'd pointer arrays: typedef int *intptr_t; intptr_t arr[N];
        let is_array_of_pointers = is_array && {
            let ptr_pos = derived.iter().position(|d| matches!(d, DerivedDeclarator::Pointer));
            let arr_pos = derived.iter().position(|d| matches!(d, DerivedDeclarator::Array(_)));
            let has_derived_ptr_before_arr = matches!((ptr_pos, arr_pos), (Some(pp), Some(ap)) if pp < ap);
            // Check if the type spec itself resolves to a pointer type (typedef'd pointer)
            let typedef_ptr_array = ptr_pos.is_none() && arr_pos.is_some() &&
                self.is_type_pointer(type_spec);
            has_derived_ptr_before_arr || typedef_ptr_array
        };
        let is_array_of_func_ptrs = is_array && derived.iter().any(|d|
            matches!(d, DerivedDeclarator::FunctionPointer(_, _) | DerivedDeclarator::Function(_, _)));
        let var_ty = if is_pointer || is_array_of_pointers || is_array_of_func_ptrs {
            IrType::Ptr
        } else {
            base_ty
        };

        // Compute element IR type for arrays (accounts for typedef'd arrays)
        let elem_ir_ty = if is_array && base_ty == IrType::Ptr && !is_array_of_pointers {
            let ctype = self.type_spec_to_ctype(type_spec);
            if let CType::Array(ref elem_ct, _) = ctype {
                Self::ctype_to_ir(elem_ct)
            } else {
                base_ty
            }
        } else {
            base_ty
        };

        // _Bool type: only for direct scalar variables, not pointers or arrays
        let has_derived_ptr = derived.iter().any(|d| matches!(d, DerivedDeclarator::Pointer));
        let is_bool = self.is_type_bool(type_spec) && !has_derived_ptr && !is_array;

        // Struct/union layout. For pointer-to-struct, we still store the layout
        // so p->field works.
        let struct_layout = self.get_struct_layout_for_type(type_spec)
            .or_else(|| {
                // For typedef'd pointer-to-struct or array-of-struct, peel CType wrappers
                let ctype = self.type_spec_to_ctype(type_spec);
                match &ctype {
                    CType::Pointer(inner) => self.struct_layout_from_ctype(inner),
                    CType::Array(inner, _) => self.struct_layout_from_ctype(inner),
                    _ => None,
                }
            });
        let is_struct = struct_layout.is_some() && !is_pointer && !is_array;

        // Actual allocation size: use struct layout size only for non-array, non-pointer structs.
        // For pointers (e.g., `struct Foo *ptr;`), alloc_size is already sizeof(pointer) = 8,
        // and we must NOT override it with the struct's layout size.
        let actual_alloc_size = if let Some(ref layout) = struct_layout {
            if is_array || is_pointer { alloc_size } else { layout.size }
        } else {
            alloc_size
        };

        let pointee_type = self.compute_pointee_type(type_spec, derived);
        let c_type = Some(self.build_full_ctype(type_spec, derived));

        DeclAnalysis {
            base_ty,
            var_ty,
            alloc_size,
            elem_size,
            is_array,
            is_pointer,
            array_dim_strides,
            is_array_of_pointers,
            is_array_of_func_ptrs,
            struct_layout,
            is_struct,
            actual_alloc_size,
            pointee_type,
            c_type,
            is_bool,
            elem_ir_ty,
        }
    }

    /// Fix up allocation size and strides for unsized arrays (int a[] = {...}).
    /// Mutates the DeclAnalysis in place based on the initializer.
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
                    // Fix alloc size for unsized char arrays initialized with string literals
                    if da.base_ty == IrType::I8 || da.base_ty == IrType::U8 {
                        if let Expr::StringLiteral(s, _) = expr {
                            da.alloc_size = s.chars().count() + 1;
                            da.actual_alloc_size = da.alloc_size;
                        }
                        // Wide string assigned to char array: use char count
                        if let Expr::WideStringLiteral(s, _) = expr {
                            da.alloc_size = s.chars().count() + 1;
                            da.actual_alloc_size = da.alloc_size;
                        }
                    }
                    // Fix alloc size for unsized wchar_t (I32) arrays with wide strings
                    if da.base_ty == IrType::I32 {
                        if let Expr::WideStringLiteral(s, _) = expr {
                            let char_count = s.chars().count() + 1; // +1 for null terminator
                            da.alloc_size = char_count * 4;
                            da.actual_alloc_size = da.alloc_size;
                        }
                        // Narrow string assigned to wchar_t array
                        if let Expr::StringLiteral(s, _) = expr {
                            let char_count = s.chars().count() + 1; // each byte becomes a wchar_t
                            da.alloc_size = char_count * 4;
                            da.actual_alloc_size = da.alloc_size;
                        }
                    }
                }
                Initializer::List(items) => {
                    let actual_count = if let Some(ref layout) = da.struct_layout {
                        // Array of structs: need to figure out how many struct
                        // elements the flat initializer list fills
                        self.compute_struct_array_init_count(items, layout)
                    } else {
                        self.compute_init_list_array_size_for_char_array(items, da.base_ty)
                    };
                    if da.elem_size > 0 {
                        da.alloc_size = actual_count * da.elem_size;
                        da.actual_alloc_size = da.alloc_size;
                        if da.array_dim_strides.len() == 1 {
                            da.array_dim_strides = vec![da.elem_size];
                        }
                    }
                }
            }
        }
    }
}

impl VarInfo {
    /// Construct VarInfo from a DeclAnalysis (shared by both LocalInfo and GlobalInfo).
    pub(super) fn from_analysis(da: &DeclAnalysis) -> Self {
        VarInfo {
            ty: da.var_ty,
            elem_size: da.elem_size,
            is_array: da.is_array,
            pointee_type: da.pointee_type,
            struct_layout: da.struct_layout.clone(),
            is_struct: da.is_struct,
            array_dim_strides: da.array_dim_strides.clone(),
            c_type: da.c_type.clone(),
        }
    }
}

impl GlobalInfo {
    /// Construct a GlobalInfo from a DeclAnalysis, avoiding repeated field construction.
    pub(super) fn from_analysis(da: &DeclAnalysis) -> Self {
        GlobalInfo { var: VarInfo::from_analysis(da) }
    }
}

impl LocalInfo {
    /// Construct a LocalInfo for a regular (non-static) local variable from DeclAnalysis.
    pub(super) fn from_analysis(da: &DeclAnalysis, alloca: Value) -> Self {
        LocalInfo {
            var: VarInfo::from_analysis(da),
            alloca,
            alloc_size: da.actual_alloc_size,
            is_bool: da.is_bool,
            static_global_name: None,
            vla_strides: vec![],
            vla_size: None,
        }
    }

    /// Construct a LocalInfo for a static local variable from DeclAnalysis.
    pub(super) fn for_static(da: &DeclAnalysis, static_name: String) -> Self {
        LocalInfo {
            var: VarInfo::from_analysis(da),
            alloca: Value(0), // placeholder; not used for static locals
            alloc_size: da.actual_alloc_size,
            is_bool: da.is_bool,
            static_global_name: Some(static_name),
            vla_strides: vec![],
            vla_size: None,
        }
    }
}

impl Default for Lowerer {
    fn default() -> Self {
        Self::new(Target::X86_64)
    }
}
