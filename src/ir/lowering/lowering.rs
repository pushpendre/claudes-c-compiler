use std::collections::HashMap;
use std::collections::HashSet;
use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, StructLayout, CType};

/// Resolve a typedef's derived declarators into the final TypeSpecifier.
///
/// Applies Pointer wrapping and Array dim collection (in reverse order for
/// correct multi-dimensional nesting). For example:
/// - `typedef int *intptr;` (Pointer derived) -> `Pointer(Int)`
/// - `typedef int arr[2][3];` (Array deriveds) -> `Array(Array(Int, 3), 2)`
///
/// This is used in both pass 0 (typedef collection) and `lower_global_decl`
/// to avoid duplicating the derived-declarator resolution logic.
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

/// Information about a local variable stored in an alloca.
#[derive(Debug, Clone)]
pub(super) struct LocalInfo {
    /// The Value (alloca) holding the address of this local.
    pub alloca: Value,
    /// Element size for arrays (used for pointer arithmetic on subscript).
    /// For non-arrays this is 0.
    pub elem_size: usize,
    /// Whether this is an array (the alloca IS the base address, not a pointer to one).
    pub is_array: bool,
    /// The IR type of the variable (I8 for char, I32 for int, I64 for long, Ptr for pointers).
    pub ty: IrType,
    /// For pointers and arrays, the type of the pointed-to/element type.
    /// Used for correct loads through pointer dereference and subscript.
    pub pointee_type: Option<IrType>,
    /// If this is a struct/union variable, its layout for member access.
    pub struct_layout: Option<StructLayout>,
    /// Whether this variable is a struct (not a pointer to struct).
    pub is_struct: bool,
    /// The total allocation size of this variable (for sizeof).
    pub alloc_size: usize,
    /// For multi-dimensional arrays: stride (in bytes) per dimension level.
    /// E.g., for int a[2][3][4], strides = [48, 16, 4] (row_size, inner_row, elem).
    /// Empty for non-arrays or 1D arrays (use elem_size instead).
    pub array_dim_strides: Vec<usize>,
    /// Full C type for precise multi-level pointer type resolution.
    pub c_type: Option<CType>,
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

/// Information about a global variable tracked by the lowerer.
#[derive(Debug, Clone)]
pub(super) struct GlobalInfo {
    /// The IR type of the global variable.
    pub ty: IrType,
    /// Element size for array globals.
    pub elem_size: usize,
    /// Whether this is an array.
    pub is_array: bool,
    /// For pointers and arrays, the type of the pointed-to/element type.
    pub pointee_type: Option<IrType>,
    /// If this is a struct/union variable, its layout for member access.
    pub struct_layout: Option<StructLayout>,
    /// Whether this variable is a struct (not a pointer to struct).
    pub is_struct: bool,
    /// For multi-dimensional arrays: stride per dimension level.
    pub array_dim_strides: Vec<usize>,
    /// Full C type for precise multi-level pointer type resolution.
    pub c_type: Option<CType>,
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
    pub end_label: String,
    pub cases: Vec<(i64, String)>,
    pub default_label: Option<String>,
    pub val_alloca: Value,
    pub expr_type: IrType,
}

/// Information about a function typedef (e.g., `typedef int func_t(int, int);`).
/// Used to detect when a declaration like `func_t add;` is a function declaration
/// rather than a variable declaration.
#[derive(Debug, Clone)]
pub(super) struct FunctionTypedefInfo {
    /// The return TypeSpecifier of the function typedef
    pub return_type: TypeSpecifier,
    /// Parameters of the function typedef
    pub params: Vec<ParamDecl>,
    /// Whether the function is variadic
    pub variadic: bool,
}

/// Metadata about known functions (return types, param types, variadic status, etc.).
/// Tracks function signatures so that calls can insert proper casts and ABI handling.
#[derive(Debug, Default)]
pub(super) struct FunctionMeta {
    /// Function name -> return type mapping for inserting narrowing casts after calls.
    pub return_types: HashMap<String, IrType>,
    /// Function name -> parameter types mapping for inserting implicit argument casts.
    pub param_types: HashMap<String, Vec<IrType>>,
    /// Function name -> parameter CType mapping for complex type argument conversions.
    pub param_ctypes: HashMap<String, Vec<CType>>,
    /// Function name -> flags indicating which parameters are _Bool (need normalization to 0/1).
    pub param_bool_flags: HashMap<String, Vec<bool>>,
    /// Function name -> is_variadic flag for calling convention handling.
    pub variadic: HashSet<String>,
    /// Function pointer variable name -> return type mapping.
    pub ptr_return_types: HashMap<String, IrType>,
    /// Function pointer variable name -> parameter types mapping.
    pub ptr_param_types: HashMap<String, Vec<IrType>>,
    /// Function name -> return CType mapping (for pointer-returning functions).
    pub return_ctypes: HashMap<String, CType>,
    /// Functions that return structs > 8 bytes and need hidden sret pointer.
    /// Maps function name to the struct return size.
    pub sret_functions: HashMap<String, usize>,
}

/// Lowers AST to IR (alloca-based, not yet SSA).
pub struct Lowerer {
    pub(super) next_value: u32,
    pub(super) next_label: u32,
    pub(super) next_string: u32,
    pub(super) next_anon_struct: u32,
    /// Counter for unique static local variable names
    pub(super) next_static_local: u32,
    pub(super) module: IrModule,
    // Current function state
    pub(super) current_blocks: Vec<BasicBlock>,
    pub(super) current_instrs: Vec<Instruction>,
    pub(super) current_label: String,
    /// Name of the function currently being lowered (for static local mangling and scoping user labels)
    pub(super) current_function_name: String,
    /// Return type of the function currently being lowered (for narrowing casts on return)
    pub(super) current_return_type: IrType,
    /// Whether the current function returns _Bool (for value clamping on return)
    pub(super) current_return_is_bool: bool,
    // Variable -> alloca mapping with metadata
    pub(super) locals: HashMap<String, LocalInfo>,
    // Global variable tracking (name -> info)
    pub(super) globals: HashMap<String, GlobalInfo>,
    // Set of known function names (to distinguish globals from functions in Identifier)
    pub(super) known_functions: HashSet<String>,
    // Set of already-defined function bodies (to avoid duplicate definitions)
    pub(super) defined_functions: HashSet<String>,
    // Set of function names declared with static (internal) linkage
    pub(super) static_functions: HashSet<String>,
    // Loop context for break/continue
    pub(super) break_labels: Vec<String>,
    pub(super) continue_labels: Vec<String>,
    /// Stack of switch statement contexts (one frame per nesting level).
    pub(super) switch_stack: Vec<SwitchFrame>,
    /// Struct/union layouts indexed by tag name (or anonymous id).
    pub(super) struct_layouts: HashMap<String, StructLayout>,
    /// Enum constant values collected from enum definitions.
    pub(super) enum_constants: HashMap<String, i64>,
    /// Const-qualified local variable values for compile-time evaluation.
    /// Maps variable name -> constant value (for `const int len = 5000;` etc.)
    pub(super) const_local_values: HashMap<String, i64>,
    /// User-defined goto labels mapped to unique IR labels (scoped per function).
    pub(super) user_labels: HashMap<String, String>,
    /// Typedef mappings (name -> underlying TypeSpecifier).
    pub(super) typedefs: HashMap<String, TypeSpecifier>,
    /// Function typedef info (typedef name -> function signature).
    /// Used to detect declarations like `func_t add;` as function declarations.
    pub(super) function_typedefs: HashMap<String, FunctionTypedefInfo>,
    /// Metadata about known functions (signatures, variadic status, etc.)
    pub(super) func_meta: FunctionMeta,
    /// Mapping from bare static local variable names to their mangled global names.
    /// e.g., "x" -> "main.x.0" for `static int x;` inside `main()`.
    pub(super) static_local_names: HashMap<String, String>,
    /// In the current function being lowered, the alloca holding the sret pointer
    /// (hidden first parameter). None if the function does not use sret.
    pub(super) current_sret_ptr: Option<Value>,
    /// CType for each local variable (needed for complex number operations).
    pub(super) var_ctypes: HashMap<String, CType>,
    /// Return CType for known functions (needed for complex function calls).
    pub(super) func_return_ctypes: HashMap<String, CType>,
}

impl Lowerer {
    pub fn new() -> Self {
        Self {
            next_value: 0,
            next_label: 0,
            next_string: 0,
            next_anon_struct: 0,
            next_static_local: 0,
            module: IrModule::new(),
            current_blocks: Vec::new(),
            current_instrs: Vec::new(),
            current_label: String::new(),
            current_function_name: String::new(),
            current_return_type: IrType::I64,
            current_return_is_bool: false,
            locals: HashMap::new(),
            globals: HashMap::new(),
            known_functions: HashSet::new(),
            defined_functions: HashSet::new(),
            break_labels: Vec::new(),
            continue_labels: Vec::new(),
            switch_stack: Vec::new(),
            struct_layouts: HashMap::new(),
            enum_constants: HashMap::new(),
            const_local_values: HashMap::new(),
            user_labels: HashMap::new(),
            typedefs: HashMap::new(),
            function_typedefs: HashMap::new(),
            func_meta: FunctionMeta::default(),
            static_local_names: HashMap::new(),
            static_functions: HashSet::new(),
            current_sret_ptr: None,
            var_ctypes: HashMap::new(),
            func_return_ctypes: HashMap::new(),
        }
    }

    pub fn lower(mut self, tu: &TranslationUnit) -> IrModule {
        // Seed builtin typedefs (matching the parser's pre-seeded typedef names)
        self.seed_builtin_typedefs();
        // Seed known libc math function signatures for correct calling convention
        self.seed_libc_math_functions();

        // Pass 0: Collect all global typedef declarations so that function return
        // types and parameter types that use typedefs can be resolved in pass 1.
        for decl in &tu.decls {
            if let ExternalDecl::Declaration(decl) = decl {
                if decl.is_typedef {
                    for declarator in &decl.declarators {
                        if !declarator.name.is_empty() {
                            // Check if this typedef defines a function type
                            // (e.g., typedef int func_t(int, int);)
                            let has_func_derived = declarator.derived.iter().any(|d|
                                matches!(d, DerivedDeclarator::Function(_, _)));
                            let has_fptr_derived = declarator.derived.iter().any(|d|
                                matches!(d, DerivedDeclarator::FunctionPointer(_, _)));

                            if has_func_derived && !has_fptr_derived {
                                // This is a function typedef like typedef int func_t(int x);
                                // Extract params and variadic from the Function derived
                                if let Some(DerivedDeclarator::Function(params, variadic)) =
                                    declarator.derived.iter().find(|d| matches!(d, DerivedDeclarator::Function(_, _)))
                                {
                                    // Count pointer levels before the Function derived
                                    let ptr_count = declarator.derived.iter()
                                        .take_while(|d| matches!(d, DerivedDeclarator::Pointer))
                                        .count();
                                    let mut return_type = decl.type_spec.clone();
                                    for _ in 0..ptr_count {
                                        return_type = TypeSpecifier::Pointer(Box::new(return_type));
                                    }
                                    self.function_typedefs.insert(declarator.name.clone(), FunctionTypedefInfo {
                                        return_type,
                                        params: params.clone(),
                                        variadic: *variadic,
                                    });
                                }
                            }

                            let resolved_type = resolve_typedef_derived(&decl.type_spec, &declarator.derived);
                            self.typedefs.insert(declarator.name.clone(), resolved_type);
                        }
                    }
                }
                // Also register struct/union type definitions so sizeof works for typedefs
                self.register_struct_type(&decl.type_spec);
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
                            if let Some(fti) = self.function_typedefs.get(tname).cloned() {
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

        // Second pass: collect all enum constants from the entire AST
        self.collect_all_enum_constants(tu);

        // Third pass: lower everything
        for decl in &tu.decls {
            match decl {
                ExternalDecl::FunctionDef(func) => {
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
        self.func_meta.return_types.insert(name.to_string(), ret_ty);

        // Track CType for pointer-returning functions
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
            self.func_meta.return_ctypes.insert(name.to_string(), ret_ctype);
        }

        // Record complex return types for expr_ctype resolution
        if ptr_count == 0 {
            let resolved = self.resolve_type_spec(ret_type_spec);
            let ret_ct = self.type_spec_to_ctype(&resolved);
            if ret_ct.is_complex() {
                self.func_return_ctypes.insert(name.to_string(), ret_ct);
            }
        }

        // Detect struct/complex returns > 8 bytes that need sret (hidden pointer) convention
        if ptr_count == 0 {
            let resolved = self.resolve_type_spec(ret_type_spec).clone();
            if matches!(resolved, TypeSpecifier::Struct(_, _, _) | TypeSpecifier::Union(_, _, _)) {
                let size = self.sizeof_type(ret_type_spec);
                if size > 8 {
                    self.func_meta.sret_functions.insert(name.to_string(), size);
                }
            }
            if matches!(resolved, TypeSpecifier::ComplexDouble | TypeSpecifier::ComplexLongDouble) {
                let size = self.sizeof_type(ret_type_spec);
                self.func_meta.sret_functions.insert(name.to_string(), size);
            }
        }

        // Collect parameter types, with K&R float->double promotion
        let param_tys: Vec<IrType> = params.iter().map(|p| {
            let ty = self.type_spec_to_ir(&p.type_spec);
            if is_kr && ty == IrType::F32 { IrType::F64 } else { ty }
        }).collect();
        let param_bool_flags: Vec<bool> = params.iter().map(|p| {
            matches!(self.resolve_type_spec(&p.type_spec), TypeSpecifier::Bool)
        }).collect();
        // Collect parameter CTypes for complex argument conversion
        let param_ctypes: Vec<CType> = params.iter().map(|p| {
            self.type_spec_to_ctype(&self.resolve_type_spec(&p.type_spec).clone())
        }).collect();

        if !variadic || !param_tys.is_empty() {
            self.func_meta.param_types.insert(name.to_string(), param_tys);
            self.func_meta.param_bool_flags.insert(name.to_string(), param_bool_flags);
            self.func_meta.param_ctypes.insert(name.to_string(), param_ctypes);
        }
        if variadic {
            self.func_meta.variadic.insert(name.to_string());
        }
    }

    pub(super) fn fresh_value(&mut self) -> Value {
        let v = Value(self.next_value);
        self.next_value += 1;
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

    pub(super) fn emit(&mut self, inst: Instruction) {
        self.current_instrs.push(inst);
    }

    pub(super) fn terminate(&mut self, term: Terminator) {
        let block = BasicBlock {
            label: self.current_label.clone(),
            instructions: std::mem::take(&mut self.current_instrs),
            terminator: term,
        };
        self.current_blocks.push(block);
    }

    pub(super) fn start_block(&mut self, label: String) {
        self.current_label = label;
        self.current_instrs.clear();
    }

    fn lower_function(&mut self, func: &FunctionDef) {
        // Skip duplicate function definitions (can happen with static inline in headers)
        if self.defined_functions.contains(&func.name) {
            return;
        }
        self.defined_functions.insert(func.name.clone());

        self.next_value = 0;
        self.current_blocks.clear();
        self.locals.clear();
        self.static_local_names.clear();
        self.const_local_values.clear();
        self.break_labels.clear();
        self.continue_labels.clear();
        self.user_labels.clear();
        // Save global enum constants before function body. Enum constants declared
        // inside function bodies should not leak to other functions.
        let saved_enum_constants = self.enum_constants.clone();
        self.current_function_name = func.name.clone();

        let return_type = self.type_spec_to_ir(&func.return_type);
        self.current_return_type = return_type;
        self.current_return_is_bool = matches!(self.resolve_type_spec(&func.return_type), TypeSpecifier::Bool);

        // Record return CType for complex-returning functions
        let ret_ctype = self.type_spec_to_ctype(&self.resolve_type_spec(&func.return_type).clone());
        if ret_ctype.is_complex() {
            self.func_return_ctypes.insert(func.name.clone(), ret_ctype);
        }

        // Check if this function uses sret (returns struct > 8 bytes via hidden pointer)
        let uses_sret = self.func_meta.sret_functions.contains_key(&func.name);

        let mut params: Vec<IrParam> = Vec::new();
        // If sret, prepend hidden pointer parameter
        if uses_sret {
            params.push(IrParam { name: "__sret_ptr".to_string(), ty: IrType::Ptr });
        }
        // For K&R functions, float params are promoted to double (default argument promotion)
        params.extend(func.params.iter().map(|p| {
            let ty = self.type_spec_to_ir(&p.type_spec);
            let ty = if func.is_kr && ty == IrType::F32 { IrType::F64 } else { ty };
            IrParam {
                name: p.name.clone().unwrap_or_default(),
                ty,
            }
        }));

        // Start entry block
        self.start_block("entry".to_string());
        self.current_sret_ptr = None;

        // Allocate params as local variables.
        //
        // For struct/union pass-by-value params, the caller passes a pointer to its struct.
        // We use a two-phase approach:
        // Phase 1: Emit one alloca per param (ptr-sized for struct params, normal for others).
        //          This ensures find_param_alloca(n) returns the nth param's receiving alloca.
        // Phase 2: Emit struct-sized allocas and Memcpy for struct params.

        // Phase 1: one alloca per parameter for receiving the argument register value
        struct StructParamInfo {
            ptr_alloca: Value,
            struct_size: usize,
            struct_layout: Option<StructLayout>,
            param_name: String,
            is_complex: bool,
            c_type: Option<CType>,
        }
        let mut struct_params: Vec<StructParamInfo> = Vec::new();

        // sret_offset: maps IR param index to func.params index (skip hidden sret param)
        let sret_offset: usize = if uses_sret { 1 } else { 0 };

        for (i, param) in params.iter().enumerate() {
            // For sret, index 0 is the hidden sret pointer param
            if uses_sret && i == 0 {
                // Emit alloca for hidden sret pointer, don't register as local
                let alloca = self.fresh_value();
                self.emit(Instruction::Alloca { dest: alloca, ty: IrType::Ptr, size: 8 });
                self.current_sret_ptr = Some(alloca);
                continue;
            }

            let orig_idx = i - sret_offset;  // index into func.params

            if !param.name.is_empty() {
                let is_struct_param = if let Some(orig_param) = func.params.get(orig_idx) {
                    let resolved = self.resolve_type_spec(&orig_param.type_spec);
                    matches!(resolved, TypeSpecifier::Struct(_, _, _) | TypeSpecifier::Union(_, _, _))
                } else {
                    false
                };

                let is_complex_param = if let Some(orig_param) = func.params.get(orig_idx) {
                    let resolved = self.resolve_type_spec(&orig_param.type_spec);
                    matches!(resolved, TypeSpecifier::ComplexFloat | TypeSpecifier::ComplexDouble | TypeSpecifier::ComplexLongDouble)
                } else {
                    false
                };

                // Emit the alloca that receives the argument value from the register
                let alloca = self.fresh_value();
                let ty = param.ty;
                // Use sizeof from TypeSpecifier for correct long double size (16 bytes)
                let param_size = func.params.get(orig_idx)
                    .map(|p| self.sizeof_type(&p.type_spec))
                    .unwrap_or(ty.size())
                    .max(ty.size());
                self.emit(Instruction::Alloca {
                    dest: alloca,
                    ty,
                    size: param_size,
                });

                if is_struct_param || is_complex_param {
                    // Record that we need to create a struct/complex copy for this param
                    let layout = if is_struct_param {
                        func.params.get(orig_idx)
                            .and_then(|p| self.get_struct_layout_for_type(&p.type_spec))
                    } else {
                        None
                    };
                    let struct_size = if is_complex_param {
                        func.params.get(orig_idx)
                            .map(|p| self.sizeof_type(&p.type_spec))
                            .unwrap_or(16)
                    } else {
                        layout.as_ref().map_or(8, |l| l.size)
                    };
                    let param_ctype = if is_complex_param {
                        func.params.get(orig_idx).map(|p| self.type_spec_to_ctype(&p.type_spec))
                    } else {
                        None
                    };
                    struct_params.push(StructParamInfo {
                        ptr_alloca: alloca,
                        struct_size,
                        struct_layout: layout,
                        param_name: param.name.clone(),
                        is_complex: is_complex_param,
                        c_type: param_ctype,
                    });
                } else {
                    // Normal parameter: register as local immediately
                    let elem_size = if ty == IrType::Ptr {
                        func.params.get(orig_idx).map_or(0, |p| self.pointee_elem_size(&p.type_spec))
                    } else { 0 };

                    let pointee_type = if ty == IrType::Ptr {
                        func.params.get(orig_idx).and_then(|p| self.pointee_ir_type(&p.type_spec))
                    } else { None };

                    let struct_layout = if ty == IrType::Ptr {
                        func.params.get(orig_idx).and_then(|p| self.get_struct_layout_for_pointer_param(&p.type_spec))
                    } else { None };

                    let c_type = func.params.get(orig_idx).map(|p| self.type_spec_to_ctype(&p.type_spec));
                    let is_bool = func.params.get(orig_idx).map_or(false, |p| {
                        matches!(self.resolve_type_spec(&p.type_spec), TypeSpecifier::Bool)
                    });

                    // For pointer-to-array params (e.g., int (*)[3] from int arr[N][3]),
                    // compute array_dim_strides so multi-dim subscripts work.
                    let array_dim_strides = if ty == IrType::Ptr {
                        func.params.get(orig_idx).map_or(vec![], |p| self.compute_ptr_array_strides(&p.type_spec))
                    } else { vec![] };

                    self.locals.insert(param.name.clone(), LocalInfo {
                        alloca,
                        elem_size,
                        is_array: false,
                        ty,
                        pointee_type,
                        struct_layout,
                        is_struct: false,
                        alloc_size: param_size,
                        array_dim_strides,
                        c_type,
                        is_bool,
                        static_global_name: None,
                        vla_strides: vec![],
                        vla_size: None,
                    });

                    // For function pointer parameters, register their return type and
                    // parameter types so indirect calls can perform correct argument casts
                    if let Some(p) = func.params.get(orig_idx) {
                        if let Some(ref fptr_params) = p.fptr_params {
                            let ret_ty = self.type_spec_to_ir(&p.type_spec);
                            // Strip the Pointer wrapper: type_spec is Pointer(ReturnType)
                            let ret_ty = match &p.type_spec {
                                TypeSpecifier::Pointer(inner) => self.type_spec_to_ir(inner),
                                _ => ret_ty,
                            };
                            if let Some(ref name) = p.name {
                                self.func_meta.ptr_return_types.insert(name.clone(), ret_ty);
                                let param_tys: Vec<IrType> = fptr_params.iter().map(|fp| {
                                    self.type_spec_to_ir(&fp.type_spec)
                                }).collect();
                                self.func_meta.ptr_param_types.insert(name.clone(), param_tys);
                            }
                        }
                    }
                }
            }
        }

        // Phase 2: For struct params, emit the struct-sized alloca + memcpy
        for sp in struct_params {
            let struct_alloca = self.fresh_value();
            self.emit(Instruction::Alloca {
                dest: struct_alloca,
                ty: IrType::Ptr,
                size: sp.struct_size,
            });

            // Load the incoming pointer from the ptr_alloca (stored by emit_store_params)
            let src_ptr = self.fresh_value();
            self.emit(Instruction::Load {
                dest: src_ptr,
                ptr: sp.ptr_alloca,
                ty: IrType::Ptr,
            });

            // Copy struct data from the caller's struct to our local alloca
            self.emit(Instruction::Memcpy {
                dest: struct_alloca,
                src: src_ptr,
                size: sp.struct_size,
            });

            // Register the struct/complex alloca as the local variable
            self.locals.insert(sp.param_name, LocalInfo {
                alloca: struct_alloca,
                elem_size: 0,
                is_array: false,
                ty: IrType::Ptr,
                pointee_type: None,
                struct_layout: sp.struct_layout,
                is_struct: true,
                alloc_size: sp.struct_size,
                array_dim_strides: vec![],
                c_type: sp.c_type,
                is_bool: false,
                static_global_name: None,
                vla_strides: vec![],
                vla_size: None,
            });
        }

        // VLA stride computation: for pointer-to-array parameters with runtime dimensions
        // (e.g., int m[rows][cols]), compute strides at runtime using the dimension parameters.
        self.compute_vla_param_strides(func);

        // K&R float promotion: for K&R functions with float params (promoted to double for ABI),
        // load the double value, narrow to float, and update the local to use the float alloca.
        if func.is_kr {
            for (i, param) in func.params.iter().enumerate() {
                let declared_ty = self.type_spec_to_ir(&param.type_spec);
                if declared_ty == IrType::F32 {
                    // The param alloca currently holds an F64 (double) value
                    if let Some(local_info) = self.locals.get(&param.name.clone().unwrap_or_default()).cloned() {
                        let f64_alloca = local_info.alloca;
                        // Load the F64 value
                        let f64_val = self.fresh_value();
                        self.emit(Instruction::Load {
                            dest: f64_val,
                            ptr: f64_alloca,
                            ty: IrType::F64,
                        });
                        // Cast F64 -> F32
                        let f32_val = self.fresh_value();
                        self.emit(Instruction::Cast {
                            dest: f32_val,
                            src: Operand::Value(f64_val),
                            from_ty: IrType::F64,
                            to_ty: IrType::F32,
                        });
                        // Create a new F32 alloca
                        let f32_alloca = self.fresh_value();
                        self.emit(Instruction::Alloca {
                            dest: f32_alloca,
                            ty: IrType::F32,
                            size: 4,
                        });
                        // Store F32 value
                        self.emit(Instruction::Store {
                            val: Operand::Value(f32_val),
                            ptr: f32_alloca,
                            ty: IrType::F32,
                        });
                        // Update local to point to F32 alloca
                        let name = param.name.clone().unwrap_or_default();
                        if let Some(local) = self.locals.get_mut(&name) {
                            local.alloca = f32_alloca;
                            local.ty = IrType::F32;
                            local.alloc_size = 4;
                        }
                    }
                }
            }
        }

        // Lower body
        self.lower_compound_stmt(&func.body);

        // If no terminator, add implicit return
        if !self.current_instrs.is_empty() || self.current_blocks.is_empty()
           || !matches!(self.current_blocks.last().map(|b| &b.terminator), Some(Terminator::Return(_)))
        {
            let ret_op = if return_type == IrType::Void {
                None
            } else {
                Some(Operand::Const(IrConst::I32(0)))
            };
            self.terminate(Terminator::Return(ret_op));
        }

        // A function has internal linkage if it was declared static anywhere
        // in the translation unit (C99 6.2.2p5: once declared with internal
        // linkage, all subsequent declarations also have internal linkage).
        let is_static = func.is_static || self.static_functions.contains(&func.name);
        let ir_func = IrFunction {
            name: func.name.clone(),
            return_type,
            params,
            blocks: std::mem::take(&mut self.current_blocks),
            is_variadic: func.variadic,
            is_declaration: false,
            is_static,
            stack_size: 0,
        };
        self.module.functions.push(ir_func);

        // Restore enum constants to global-only state so function-body enum constants
        // don't leak to subsequent functions.
        self.enum_constants = saved_enum_constants;
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
                        let result = self.fresh_value();
                        self.emit(Instruction::BinOp {
                            dest: result,
                            op: IrBinOp::Mul,
                            lhs: Operand::Value(dim_val),
                            rhs: Operand::Value(prev),
                            ty: IrType::I64,
                        });
                        result
                    } else {
                        // Constant stride * runtime dim
                        let result = self.fresh_value();
                        self.emit(Instruction::BinOp {
                            dest: result,
                            op: IrBinOp::Mul,
                            lhs: Operand::Value(dim_val),
                            rhs: Operand::Const(IrConst::I64(current_const_stride as i64)),
                            ty: IrType::I64,
                        });
                        result
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
                        let result = self.fresh_value();
                        self.emit(Instruction::BinOp {
                            dest: result,
                            op: IrBinOp::Mul,
                            lhs: Operand::Value(prev),
                            rhs: Operand::Const(IrConst::I64(const_dim as i64)),
                            ty: IrType::I64,
                        });
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
            if let Some(local) = self.locals.get_mut(&param_name) {
                local.vla_strides = vla_strides;
            }
        }
    }

    /// Load the value of a VLA dimension variable (a function parameter).
    fn load_vla_dim_value(&mut self, dim_name: &str) -> Value {
        if let Some(info) = self.locals.get(dim_name).cloned() {
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
                    let resolved_type = resolve_typedef_derived(&decl.type_spec, &declarator.derived);
                    self.typedefs.insert(declarator.name.clone(), resolved_type);
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
                    if self.function_typedefs.contains_key(tname) {
                        continue;
                    }
                }
            }

            // extern without initializer: track the type but don't emit a .bss entry
            // (the definition will come from another translation unit)
            if decl.is_extern && declarator.init.is_none() {
                if !self.globals.contains_key(&declarator.name) {
                    let base_ty = self.type_spec_to_ir(&decl.type_spec);
                    let (_, elem_size, is_array, is_pointer, array_dim_strides) = self.compute_decl_info(&decl.type_spec, &declarator.derived);
                    let is_array_of_func_ptrs = is_array && declarator.derived.iter().any(|d|
                        matches!(d, DerivedDeclarator::FunctionPointer(_, _) | DerivedDeclarator::Function(_, _)));
                    let var_ty = if is_pointer || is_array_of_func_ptrs { IrType::Ptr } else { base_ty };
                    let struct_layout = self.get_struct_layout_for_type(&decl.type_spec);
                    let is_struct = struct_layout.is_some() && !is_pointer && !is_array;
                    let pointee_type = self.compute_pointee_type(&decl.type_spec, &declarator.derived);
                    let c_type = Some(self.build_full_ctype(&decl.type_spec, &declarator.derived));
                    self.globals.insert(declarator.name.clone(), GlobalInfo {
                        ty: var_ty,
                        elem_size,
                        is_array,
                        pointee_type,
                        struct_layout,
                        is_struct,
                        array_dim_strides,
                        c_type,
                    });
                }
                continue;
            }

            // If this global already exists (e.g., `extern int a; int a = 0;`),
            // handle tentative definitions and re-declarations correctly.
            if self.globals.contains_key(&declarator.name) {
                if declarator.init.is_none() {
                    // Check if this is a tentative definition (non-extern without init)
                    // that needs to be emitted because only an extern was previously tracked
                    let already_emitted = self.module.globals.iter().any(|g| g.name == declarator.name);
                    if already_emitted {
                        // Already defined in .data/.bss, skip duplicate
                        continue;
                    }
                    // Not yet emitted: this is a tentative definition after an extern declaration.
                    // Fall through to emit it as zero-initialized.
                } else {
                    // Has initializer: remove the previous zero-init/extern global and re-emit with init
                    self.module.globals.retain(|g| g.name != declarator.name);
                }
            }

            let mut base_ty = self.type_spec_to_ir(&decl.type_spec);
            let (mut alloc_size, elem_size, is_array, is_pointer, mut array_dim_strides) = self.compute_decl_info(&decl.type_spec, &declarator.derived);
            // For typedef'd array types (e.g., typedef int a[]; a x = {...}),
            // type_spec_to_ir returns Ptr (array decays to pointer), but we need
            // the element type for correct data emission.
            if is_array && base_ty == IrType::Ptr && !is_pointer {
                if let TypeSpecifier::Array(ref elem, _) = self.resolve_type_spec(&decl.type_spec) {
                    base_ty = self.type_spec_to_ir(elem);
                }
            }
            // For array-of-pointers or array-of-function-pointers, element type is Ptr
            let is_array_of_pointers = is_array && {
                let ptr_pos = declarator.derived.iter().position(|d| matches!(d, DerivedDeclarator::Pointer));
                let arr_pos = declarator.derived.iter().position(|d| matches!(d, DerivedDeclarator::Array(_)));
                matches!((ptr_pos, arr_pos), (Some(pp), Some(ap)) if pp < ap)
            };
            let is_array_of_func_ptrs = is_array && declarator.derived.iter().any(|d|
                matches!(d, DerivedDeclarator::FunctionPointer(_, _) | DerivedDeclarator::Function(_, _)));
            let var_ty = if is_pointer || is_array_of_pointers || is_array_of_func_ptrs { IrType::Ptr } else { base_ty };

            // For unsized arrays (int a[] = {...} or typedef int a[]; a x = {1,2,3}),
            // compute actual size from initializer
            let is_unsized_array = is_array && (
                declarator.derived.iter().any(|d| {
                    matches!(d, DerivedDeclarator::Array(None))
                })
                || matches!(self.resolve_type_spec(&decl.type_spec), TypeSpecifier::Array(_, None))
            );
            if is_unsized_array {
                if let Some(ref init) = declarator.init {
                    match init {
                        Initializer::Expr(expr) => {
                            // Fix alloc size for unsized char arrays initialized with string literals
                            if base_ty == IrType::I8 || base_ty == IrType::U8 {
                                if let Expr::StringLiteral(s, _) = expr {
                                    alloc_size = s.as_bytes().len() + 1;
                                }
                            }
                        }
                        Initializer::List(items) => {
                            let actual_count = self.compute_init_list_array_size_for_char_array(items, base_ty);
                            if elem_size > 0 {
                                alloc_size = actual_count * elem_size;
                                if array_dim_strides.len() == 1 {
                                    array_dim_strides = vec![elem_size];
                                }
                            }
                        }
                    }
                }
            }

            // Determine struct layout for global struct/pointer-to-struct variables.
            // For arrays of pointers (e.g., struct foo *arr[]), the struct layout describes
            // the pointee, not the array element (which is a pointer). Clear it so the array
            // is treated as a pointer array, not a struct array.
            let struct_layout = if is_array_of_pointers || is_array_of_func_ptrs {
                None
            } else {
                self.get_struct_layout_for_type(&decl.type_spec)
            };
            let is_struct = struct_layout.is_some() && !is_pointer && !is_array;

            let actual_alloc_size = if let Some(ref layout) = struct_layout {
                if is_array {
                    alloc_size
                } else {
                    layout.size
                }
            } else if !is_array && !is_pointer {
                // For scalar globals, use the actual C type size (handles long double = 16 bytes)
                let c_size = self.sizeof_type(&decl.type_spec);
                c_size.max(var_ty.size())
            } else {
                alloc_size
            };

            // Extern declarations without initializers: track but don't emit storage
            let is_extern_decl = decl.is_extern && declarator.init.is_none();

            // Determine initializer
            let init = if let Some(ref initializer) = declarator.init {
                self.lower_global_init(initializer, &decl.type_spec, base_ty, is_array, elem_size, actual_alloc_size, &struct_layout, &array_dim_strides)
            } else {
                GlobalInit::Zero
            };

            // Track this global variable
            let pointee_type = self.compute_pointee_type(&decl.type_spec, &declarator.derived);
            let c_type = Some(self.build_full_ctype(&decl.type_spec, &declarator.derived));
            self.globals.insert(declarator.name.clone(), GlobalInfo {
                ty: var_ty,
                elem_size,
                is_array,
                pointee_type,
                struct_layout,
                is_struct,
                array_dim_strides,
                c_type,
            });

            // Use C type alignment for long double (16) instead of IrType::F64 alignment (8)
            let align = {
                let c_align = self.alignof_type(&decl.type_spec);
                if c_align > 0 { c_align.max(var_ty.align()) } else { var_ty.align() }
            };

            let is_static = decl.is_static;

            // For struct initializers emitted as byte arrays, set element type to I8
            // so the backend emits .byte directives for each element.
            // This applies to both single structs and arrays of structs.
            // Detect by checking if the Array init contains I8 constants (byte-serialized struct).
            let global_ty = if matches!(&init, GlobalInit::Array(vals) if !vals.is_empty() && matches!(vals[0], IrConst::I8(_))) {
                IrType::I8
            } else if is_struct && matches!(init, GlobalInit::Array(_)) {
                IrType::I8
            } else {
                var_ty
            };

            // For structs with FAMs, the init byte array may be larger than layout.size.
            // Use the actual init data size if it exceeds the computed alloc size.
            let final_size = match &init {
                GlobalInit::Array(vals) if is_struct && vals.len() > actual_alloc_size => vals.len(),
                _ => actual_alloc_size,
            };

            self.module.globals.push(IrGlobal {
                name: declarator.name.clone(),
                ty: global_ty,
                size: final_size,
                align,
                init,
                is_static,
                is_extern: is_extern_decl,
            });
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
    pub(super) fn collect_enum_constants(&mut self, ts: &TypeSpecifier) {
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
                    self.enum_constants.insert(variant.name.clone(), next_val);
                    next_val += 1;
                }
            }
            // Recurse into struct/union fields to find enum definitions within them
            TypeSpecifier::Struct(_, Some(fields), _) | TypeSpecifier::Union(_, Some(fields), _) => {
                for field in fields {
                    self.collect_enum_constants(&field.type_spec);
                }
            }
            // Unwrap Array and Pointer wrappers to find nested enum definitions.
            // This handles cases like: enum { A, B } volatile arr[2][2]; inside structs,
            // where the field type becomes Array(Array(Enum(...))).
            TypeSpecifier::Array(inner, _) | TypeSpecifier::Pointer(inner) => {
                self.collect_enum_constants(inner);
            }
            _ => {}
        }
    }

    // NOTE: collect_enum_constants_from_compound and collect_enum_constants_from_stmt
    // were removed. Enum constants inside function bodies are now collected during
    // lowering (in lower_compound_stmt) with proper scope save/restore, rather than
    // pre-collected globally. This fixes enum constant scope leakage across blocks.

    /// For a pointer-to-struct parameter type (e.g., `struct TAG *p`), get the
    /// pointed-to struct's layout. This enables `p->field` access.
    pub(super) fn get_struct_layout_for_pointer_param(&self, type_spec: &TypeSpecifier) -> Option<StructLayout> {
        // Resolve typedefs first (e.g., typedef union Outer *OuterPtr)
        let resolved = self.resolve_type_spec(type_spec);
        match resolved {
            TypeSpecifier::Pointer(inner) => self.get_struct_layout_for_type(inner),
            _ => None,
        }
    }

    /// Compute the IR type of the pointee for a pointer/array type specifier.
    /// For `Pointer(Char)`, returns Some(I8).
    /// For `Pointer(Int)`, returns Some(I32).
    /// For `Array(Int, _)`, returns Some(I32).
    /// Resolves typedef names before pattern matching.
    pub(super) fn pointee_ir_type(&self, type_spec: &TypeSpecifier) -> Option<IrType> {
        let resolved = self.resolve_type_spec(type_spec);
        match &resolved {
            TypeSpecifier::Pointer(inner) => Some(self.type_spec_to_ir(inner)),
            TypeSpecifier::Array(inner, _) => Some(self.type_spec_to_ir(inner)),
            _ => None,
        }
    }

    /// Compute the pointee type for a declaration, considering both the base type
    /// specifier and derived declarators (pointer/array).
    /// For `char *s` (type_spec=Char, derived=[Pointer]): returns Some(I8)
    /// For `int *p` (type_spec=Int, derived=[Pointer]): returns Some(I32)
    /// For `int **pp` (type_spec=Int, derived=[Pointer, Pointer]): returns Some(Ptr)
    /// For `int a[10]` (type_spec=Int, derived=[Array(10)]): returns Some(I32)
    pub(super) fn compute_pointee_type(&self, type_spec: &TypeSpecifier, derived: &[DerivedDeclarator]) -> Option<IrType> {
        // Count pointer and array levels
        let ptr_count = derived.iter().filter(|d| matches!(d, DerivedDeclarator::Pointer)).count();
        let has_array = derived.iter().any(|d| matches!(d, DerivedDeclarator::Array(_)));

        if ptr_count > 1 {
            // Multi-level pointer (e.g., int **pp) - pointee is a pointer
            Some(IrType::Ptr)
        } else if ptr_count == 1 {
            // Single pointer - pointee is the base type
            if has_array {
                Some(IrType::Ptr)
            } else {
                match type_spec {
                    TypeSpecifier::Pointer(inner) => Some(self.type_spec_to_ir(inner)),
                    _ => Some(self.type_spec_to_ir(type_spec)),
                }
            }
        } else if has_array {
            // Array (e.g., int a[10]) - element type is the base type
            Some(self.type_spec_to_ir(type_spec))
        } else {
            // Check if the type_spec itself is a pointer
            self.pointee_ir_type(type_spec)
        }
    }

    /// Check if an lvalue expression targets a _Bool variable (requires normalization).
    /// Handles direct identifiers, pointer dereferences (*pval), array subscripts (arr[i]),
    /// and member accesses (s.field, p->field) where the target type is _Bool.
    pub(super) fn is_bool_lvalue(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(info) = self.locals.get(name) {
                    return info.is_bool;
                }
                // Check global variables for _Bool type
                if let Some(ginfo) = self.globals.get(name) {
                    if let Some(ref ct) = ginfo.c_type {
                        return matches!(ct, CType::Bool);
                    }
                }
                false
            }
            Expr::Deref(_, _)
            | Expr::ArraySubscript(_, _, _)
            | Expr::MemberAccess(_, _, _)
            | Expr::PointerMemberAccess(_, _, _) => {
                // Use CType resolution to check if the lvalue target is _Bool
                if let Some(ct) = self.get_expr_ctype(expr) {
                    return matches!(ct, CType::Bool);
                }
                false
            }
            _ => false,
        }
    }

    /// Check if an expression has pointer type (for pointer arithmetic).
    pub(super) fn expr_is_pointer(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(info) = self.locals.get(name) {
                    // Arrays decay to pointers in expression context
                    return (info.ty == IrType::Ptr || info.is_array) && !info.is_struct;
                }
                if let Some(ginfo) = self.globals.get(name) {
                    return (ginfo.ty == IrType::Ptr || ginfo.is_array) && !ginfo.is_struct;
                }
                false
            }
            Expr::AddressOf(_, _) => true,
            Expr::PostfixOp(_, inner, _) => self.expr_is_pointer(inner),
            Expr::UnaryOp(op, inner, _) => {
                match op {
                    UnaryOp::PreInc | UnaryOp::PreDec => self.expr_is_pointer(inner),
                    _ => false,
                }
            }
            Expr::ArraySubscript(base, _, _) => {
                // Result of subscript on pointer-to-pointer
                if let Some(pt) = self.get_pointee_type_of_expr(base) {
                    return pt == IrType::Ptr;
                }
                false
            }
            Expr::Cast(ref type_spec, _, _) => {
                matches!(type_spec, TypeSpecifier::Pointer(_))
            }
            Expr::StringLiteral(_, _) => true,
            Expr::BinaryOp(op, lhs, rhs, _) => {
                match op {
                    BinOp::Add => {
                        // ptr + int or int + ptr yields a pointer
                        self.expr_is_pointer(lhs) || self.expr_is_pointer(rhs)
                    }
                    BinOp::Sub => {
                        // ptr - int yields a pointer; ptr - ptr yields an integer
                        let lhs_ptr = self.expr_is_pointer(lhs);
                        let rhs_ptr = self.expr_is_pointer(rhs);
                        lhs_ptr && !rhs_ptr
                    }
                    _ => false,
                }
            }
            Expr::Conditional(_, then_expr, else_expr, _) => {
                self.expr_is_pointer(then_expr) || self.expr_is_pointer(else_expr)
            }
            Expr::Comma(_, rhs, _) => self.expr_is_pointer(rhs),
            Expr::FunctionCall(func, _, _) => {
                // Check CType for function call return
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    return matches!(ctype, CType::Pointer(_));
                }
                // Fallback: check IrType return type
                if let Expr::Identifier(name, _) = func.as_ref() {
                    if let Some(&ret_ty) = self.func_meta.return_types.get(name.as_str()) {
                        return ret_ty == IrType::Ptr;
                    }
                }
                false
            }
            Expr::MemberAccess(base_expr, field_name, _) => {
                // Struct member that is an array (decays to pointer) or pointer type
                if let Some(ctype) = self.resolve_member_field_ctype(base_expr, field_name) {
                    return matches!(ctype, CType::Array(_, _) | CType::Pointer(_));
                }
                false
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                // Pointer member access: p->field where field is array or pointer
                if let Some(ctype) = self.resolve_pointer_member_field_ctype(base_expr, field_name) {
                    return matches!(ctype, CType::Array(_, _) | CType::Pointer(_));
                }
                false
            }
            Expr::Deref(_, _) => {
                // Dereferencing a pointer-to-array yields an array which decays to pointer.
                // Dereferencing a pointer-to-pointer yields a pointer.
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    return matches!(ctype, CType::Array(_, _) | CType::Pointer(_));
                }
                false
            }
            Expr::Assign(_, _, _) | Expr::CompoundAssign(_, _, _, _) => {
                // Assignment result has the type of the LHS
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    return matches!(ctype, CType::Array(_, _) | CType::Pointer(_));
                }
                false
            }
            _ => false,
        }
    }

    /// Get the element size for a pointer expression (for scaling in pointer arithmetic).
    /// For `int *p`, returns 4. For `char *s`, returns 1.
    pub(super) fn get_pointer_elem_size_from_expr(&self, expr: &Expr) -> usize {
        // Try CType-based resolution first for accurate type information
        if let Some(ctype) = self.get_expr_ctype(expr) {
            match &ctype {
                CType::Pointer(pointee) => return pointee.size().max(1),
                CType::Array(elem, _) => return elem.size().max(1),
                _ => {}
            }
        }
        match expr {
            Expr::Identifier(name, _) => {
                // Check locals then globals for pointee_type or elem_size.
                if let Some(info) = self.locals.get(name) {
                    if let Some(pt) = info.pointee_type {
                        return pt.size();
                    }
                    if info.elem_size > 0 {
                        return info.elem_size;
                    }
                }
                if let Some(ginfo) = self.globals.get(name) {
                    if let Some(pt) = ginfo.pointee_type {
                        return pt.size();
                    }
                    if ginfo.elem_size > 0 {
                        return ginfo.elem_size;
                    }
                }
                8
            }
            Expr::PostfixOp(_, inner, _) => self.get_pointer_elem_size_from_expr(inner),
            Expr::UnaryOp(op, inner, _) => {
                match op {
                    UnaryOp::PreInc | UnaryOp::PreDec => self.get_pointer_elem_size_from_expr(inner),
                    _ => 8,
                }
            }
            Expr::BinaryOp(op, lhs, rhs, _) => {
                // ptr + int or ptr - int: get elem size from the pointer operand
                match op {
                    BinOp::Add => {
                        if self.expr_is_pointer(lhs) {
                            self.get_pointer_elem_size_from_expr(lhs)
                        } else if self.expr_is_pointer(rhs) {
                            self.get_pointer_elem_size_from_expr(rhs)
                        } else {
                            8
                        }
                    }
                    BinOp::Sub => {
                        if self.expr_is_pointer(lhs) {
                            self.get_pointer_elem_size_from_expr(lhs)
                        } else {
                            8
                        }
                    }
                    _ => 8,
                }
            }
            Expr::Conditional(_, then_expr, _, _) => self.get_pointer_elem_size_from_expr(then_expr),
            Expr::Comma(_, rhs, _) => self.get_pointer_elem_size_from_expr(rhs),
            Expr::FunctionCall(_, _, _) => {
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    if let CType::Pointer(pointee) = &ctype {
                        return pointee.size().max(1);
                    }
                }
                8
            }
            Expr::AddressOf(inner, _) => {
                // &x: pointer to typeof(x)
                let ty = self.get_expr_type(inner);
                ty.size()
            }
            Expr::Cast(ref type_spec, _, _) => {
                if let TypeSpecifier::Pointer(ref inner) = type_spec {
                    self.sizeof_type(inner)
                } else {
                    8
                }
            }
            Expr::MemberAccess(base_expr, field_name, _) => {
                if let Some(ctype) = self.resolve_member_field_ctype(base_expr, field_name) {
                    match &ctype {
                        CType::Array(elem_ty, _) => return elem_ty.size(),
                        CType::Pointer(pointee_ty) => return pointee_ty.size(),
                        _ => {}
                    }
                }
                8
            }
            Expr::PointerMemberAccess(base_expr, field_name, _) => {
                if let Some(ctype) = self.resolve_pointer_member_field_ctype(base_expr, field_name) {
                    match &ctype {
                        CType::Array(elem_ty, _) => return elem_ty.size(),
                        CType::Pointer(pointee_ty) => return pointee_ty.size(),
                        _ => {}
                    }
                }
                8
            }
            _ => {
                // Try using get_pointee_type_of_expr as a fallback
                if let Some(pt) = self.get_pointee_type_of_expr(expr) {
                    return pt.size();
                }
                8
            }
        }
    }

    /// Get the pointee type for a pointer expression - i.e., what type you get when dereferencing it.
    pub(super) fn get_pointee_type_of_expr(&self, expr: &Expr) -> Option<IrType> {
        // First try CType-based resolution (handles multi-level pointers correctly)
        if let Some(ctype) = self.get_expr_ctype(expr) {
            match ctype {
                CType::Pointer(inner) => return Some(IrType::from_ctype(&inner)),
                CType::Array(elem, _) => return Some(IrType::from_ctype(&elem)),
                _ => {}
            }
        }
        match expr {
            Expr::Identifier(name, _) => {
                if let Some(info) = self.locals.get(name) {
                    return info.pointee_type;
                }
                if let Some(ginfo) = self.globals.get(name) {
                    return ginfo.pointee_type;
                }
                None
            }
            Expr::PostfixOp(_, inner, _) => {
                self.get_pointee_type_of_expr(inner)
            }
            Expr::UnaryOp(op, inner, _) => {
                match op {
                    UnaryOp::PreInc | UnaryOp::PreDec => {
                        self.get_pointee_type_of_expr(inner)
                    }
                    _ => None,
                }
            }
            Expr::BinaryOp(_, lhs, rhs, _) => {
                if let Some(pt) = self.get_pointee_type_of_expr(lhs) {
                    return Some(pt);
                }
                self.get_pointee_type_of_expr(rhs)
            }
            Expr::Cast(ref type_spec, inner, _) => {
                if let TypeSpecifier::Pointer(ref pointee_ts) = type_spec {
                    let pt = self.type_spec_to_ir(pointee_ts);
                    return Some(pt);
                }
                self.get_pointee_type_of_expr(inner)
            }
            Expr::Conditional(_, then_expr, else_expr, _) => {
                if let Some(pt) = self.get_pointee_type_of_expr(then_expr) {
                    return Some(pt);
                }
                self.get_pointee_type_of_expr(else_expr)
            }
            Expr::Comma(_, last, _) => {
                self.get_pointee_type_of_expr(last)
            }
            Expr::AddressOf(inner, _) => {
                let ty = self.get_expr_type(inner);
                Some(ty)
            }
            Expr::Assign(_, rhs, _) => {
                self.get_pointee_type_of_expr(rhs)
            }
            _ => None,
        }
    }

    /// Get or create a unique IR label for a user-defined goto label.
    pub(super) fn get_or_create_user_label(&mut self, name: &str) -> String {
        let key = format!("{}::{}", self.current_function_name, name);
        if let Some(label) = self.user_labels.get(&key) {
            label.clone()
        } else {
            let label = self.fresh_label(&format!("user_{}", name));
            self.user_labels.insert(key, label.clone());
            label
        }
    }

    /// Copy a string literal's bytes into an alloca at a given byte offset,
    /// followed by a null terminator. Used for `char s[] = "hello"` and
    /// string elements in array initializer lists.
    pub(super) fn emit_string_to_alloca(&mut self, alloca: Value, s: &str, base_offset: usize) {
        let bytes = s.as_bytes();
        for (j, &byte) in bytes.iter().enumerate() {
            let val = Operand::Const(IrConst::I8(byte as i8));
            let offset = Operand::Const(IrConst::I64((base_offset + j) as i64));
            let addr = self.fresh_value();
            self.emit(Instruction::GetElementPtr {
                dest: addr, base: alloca, offset, ty: IrType::I8,
            });
            self.emit(Instruction::Store { val, ptr: addr, ty: IrType::I8 });
        }
        // Null terminator
        let null_offset = Operand::Const(IrConst::I64((base_offset + bytes.len()) as i64));
        let null_addr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: null_addr, base: alloca, offset: null_offset, ty: IrType::I8,
        });
        self.emit(Instruction::Store {
            val: Operand::Const(IrConst::I8(0)), ptr: null_addr, ty: IrType::I8,
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
}

impl Default for Lowerer {
    fn default() -> Self {
        Self::new()
    }
}
