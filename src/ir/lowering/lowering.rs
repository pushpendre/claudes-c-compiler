use std::collections::HashMap;
use std::collections::HashSet;
use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, StructLayout, CType};

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

/// Represents an lvalue - something that can be assigned to.
/// Contains the address (as an IR Value) where the data resides.
#[derive(Debug, Clone)]
pub(super) enum LValue {
    /// A direct variable: the alloca is the address.
    Variable(Value),
    /// An address computed at runtime (e.g., arr[i], *ptr).
    Address(Value),
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
    // Loop context for break/continue
    pub(super) break_labels: Vec<String>,
    pub(super) continue_labels: Vec<String>,
    // Switch context
    pub(super) switch_end_labels: Vec<String>,
    pub(super) switch_cases: Vec<Vec<(i64, String)>>,
    pub(super) switch_default: Vec<Option<String>>,
    /// Alloca holding the switch expression value for the current switch.
    pub(super) switch_val_allocas: Vec<Value>,
    /// The type of the controlling expression for each nested switch.
    pub(super) switch_expr_types: Vec<IrType>,
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
    /// Function name -> return type mapping for inserting narrowing casts after calls.
    pub(super) function_return_types: HashMap<String, IrType>,
    /// Function name -> parameter types mapping for inserting implicit argument casts.
    pub(super) function_param_types: HashMap<String, Vec<IrType>>,
    /// Function name -> flags indicating which parameters are _Bool (need normalization to 0/1).
    pub(super) function_param_bool_flags: HashMap<String, Vec<bool>>,
    /// Function name -> is_variadic flag for calling convention handling.
    pub(super) function_variadic: HashSet<String>,
    /// Function pointer variable name -> return type mapping.
    pub(super) function_ptr_return_types: HashMap<String, IrType>,
    /// Function pointer variable name -> parameter types mapping.
    pub(super) function_ptr_param_types: HashMap<String, Vec<IrType>>,
    /// Mapping from bare static local variable names to their mangled global names.
    /// e.g., "x" -> "main.x.0" for `static int x;` inside `main()`.
    pub(super) static_local_names: HashMap<String, String>,
    /// Function name -> return CType mapping (for pointer-returning functions).
    pub(super) function_return_ctypes: HashMap<String, CType>,
    /// Functions that return structs > 8 bytes and need hidden sret pointer.
    /// Maps function name to the struct return size.
    pub(super) sret_functions: HashMap<String, usize>,
    /// In the current function being lowered, the alloca holding the sret pointer
    /// (hidden first parameter). None if the function does not use sret.
    pub(super) current_sret_ptr: Option<Value>,
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
            switch_end_labels: Vec::new(),
            switch_cases: Vec::new(),
            switch_default: Vec::new(),
            switch_val_allocas: Vec::new(),
            switch_expr_types: Vec::new(),
            struct_layouts: HashMap::new(),
            enum_constants: HashMap::new(),
            const_local_values: HashMap::new(),
            user_labels: HashMap::new(),
            typedefs: HashMap::new(),
            function_return_types: HashMap::new(),
            function_param_types: HashMap::new(),
            function_param_bool_flags: HashMap::new(),
            function_variadic: HashSet::new(),
            function_ptr_return_types: HashMap::new(),
            function_ptr_param_types: HashMap::new(),
            static_local_names: HashMap::new(),
            function_return_ctypes: HashMap::new(),
            sret_functions: HashMap::new(),
            current_sret_ptr: None,
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
                            let mut resolved_type = decl.type_spec.clone();
                            for d in &declarator.derived {
                                match d {
                                    DerivedDeclarator::Pointer => {
                                        resolved_type = TypeSpecifier::Pointer(Box::new(resolved_type));
                                    }
                                    DerivedDeclarator::Array(size) => {
                                        resolved_type = TypeSpecifier::Array(Box::new(resolved_type), size.clone());
                                    }
                                    _ => {}
                                }
                            }
                            self.typedefs.insert(declarator.name.clone(), resolved_type);
                        }
                    }
                }
                // Also register struct/union type definitions so sizeof works for typedefs
                self.register_struct_type(&decl.type_spec);
            }
        }

        // First pass: collect all function names, return types, and parameter types
        // so we can distinguish function references from global variable references,
        // insert proper narrowing casts after calls, and insert implicit argument casts.
        for decl in &tu.decls {
            if let ExternalDecl::FunctionDef(func) = decl {
                self.known_functions.insert(func.name.clone());
                let ret_ty = self.type_spec_to_ir(&func.return_type);
                self.function_return_types.insert(func.name.clone(), ret_ty);
                // Track CType for pointer-returning functions
                if ret_ty == IrType::Ptr {
                    let ret_ctype = self.type_spec_to_ctype(&func.return_type);
                    self.function_return_ctypes.insert(func.name.clone(), ret_ctype);
                }
                // Detect struct returns > 8 bytes that need sret (hidden pointer) convention
                {
                    let resolved = self.resolve_type_spec(&func.return_type);
                    if matches!(resolved, TypeSpecifier::Struct(_, _) | TypeSpecifier::Union(_, _)) {
                        let size = self.sizeof_type(&func.return_type);
                        if size > 8 {
                            self.sret_functions.insert(func.name.clone(), size);
                        }
                    }
                }
                // Collect parameter types (skip variadic portion)
                // For K&R functions, float params are promoted to double (default argument promotion)
                let param_tys: Vec<IrType> = func.params.iter().map(|p| {
                    let ty = self.type_spec_to_ir(&p.type_spec);
                    if func.is_kr && ty == IrType::F32 {
                        IrType::F64
                    } else {
                        ty
                    }
                }).collect();
                let param_bool_flags: Vec<bool> = func.params.iter().map(|p| {
                    matches!(self.resolve_type_spec(&p.type_spec), TypeSpecifier::Bool)
                }).collect();
                if !func.variadic || !param_tys.is_empty() {
                    self.function_param_types.insert(func.name.clone(), param_tys);
                    self.function_param_bool_flags.insert(func.name.clone(), param_bool_flags);
                }
                if func.variadic {
                    self.function_variadic.insert(func.name.clone());
                }
            }
            // Also detect function declarations (extern prototypes)
            if let ExternalDecl::Declaration(decl) = decl {
                for declarator in &decl.declarators {
                    // Find the Function derived declarator and count preceding Pointer derivations
                    let mut ptr_count = 0;
                    let mut func_info = None;
                    for d in &declarator.derived {
                        match d {
                            DerivedDeclarator::Pointer => ptr_count += 1,
                            DerivedDeclarator::Function(p, v) => {
                                func_info = Some((p, v));
                                break;
                            }
                            _ => {}
                        }
                    }
                    if let Some((params, variadic)) = func_info {
                        self.known_functions.insert(declarator.name.clone());
                        // Wrap return type with pointer levels from derived declarators
                        let mut ret_ty = self.type_spec_to_ir(&decl.type_spec);
                        if ptr_count > 0 {
                            ret_ty = IrType::Ptr;
                        }
                        self.function_return_types.insert(declarator.name.clone(), ret_ty);
                        // Track CType for pointer-returning functions
                        if ret_ty == IrType::Ptr {
                            let base_ctype = self.type_spec_to_ctype(&decl.type_spec);
                            let ret_ctype = if ptr_count > 0 {
                                let mut ct = base_ctype;
                                for _ in 0..ptr_count {
                                    ct = CType::Pointer(Box::new(ct));
                                }
                                ct
                            } else {
                                base_ctype
                            };
                            self.function_return_ctypes.insert(declarator.name.clone(), ret_ctype);
                        }
                        // Detect struct returns > 8 bytes that need sret
                        if ptr_count == 0 {
                            let resolved = self.resolve_type_spec(&decl.type_spec);
                            if matches!(resolved, TypeSpecifier::Struct(_, _) | TypeSpecifier::Union(_, _)) {
                                let size = self.sizeof_type(&decl.type_spec);
                                if size > 8 {
                                    self.sret_functions.insert(declarator.name.clone(), size);
                                }
                            }
                        }
                        let param_tys: Vec<IrType> = params.iter().map(|p| {
                            self.type_spec_to_ir(&p.type_spec)
                        }).collect();
                        let param_bool_flags: Vec<bool> = params.iter().map(|p| {
                            matches!(self.resolve_type_spec(&p.type_spec), TypeSpecifier::Bool)
                        }).collect();
                        if !variadic || !param_tys.is_empty() {
                            self.function_param_types.insert(declarator.name.clone(), param_tys);
                            self.function_param_bool_flags.insert(declarator.name.clone(), param_bool_flags);
                        }
                        if *variadic {
                            self.function_variadic.insert(declarator.name.clone());
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
        self.current_function_name = func.name.clone();

        let return_type = self.type_spec_to_ir(&func.return_type);
        self.current_return_type = return_type;
        self.current_return_is_bool = matches!(self.resolve_type_spec(&func.return_type), TypeSpecifier::Bool);

        // Check if this function uses sret (returns struct > 8 bytes via hidden pointer)
        let uses_sret = self.sret_functions.contains_key(&func.name);

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
                    matches!(resolved, TypeSpecifier::Struct(_, _) | TypeSpecifier::Union(_, _))
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

                if is_struct_param {
                    // Record that we need to create a struct copy for this param
                    let layout = func.params.get(orig_idx)
                        .and_then(|p| self.get_struct_layout_for_type(&p.type_spec));
                    let struct_size = layout.as_ref().map_or(8, |l| l.size);
                    struct_params.push(StructParamInfo {
                        ptr_alloca: alloca,
                        struct_size,
                        struct_layout: layout,
                        param_name: param.name.clone(),
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
                                self.function_ptr_return_types.insert(name.clone(), ret_ty);
                                let param_tys: Vec<IrType> = fptr_params.iter().map(|fp| {
                                    self.type_spec_to_ir(&fp.type_spec)
                                }).collect();
                                self.function_ptr_param_types.insert(name.clone(), param_tys);
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

            // Register the struct alloca as the local variable
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
                c_type: None,
                is_bool: false,
            });
        }

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

        let ir_func = IrFunction {
            name: func.name.clone(),
            return_type,
            params,
            blocks: std::mem::take(&mut self.current_blocks),
            is_variadic: func.variadic,
            is_declaration: false,
            is_static: func.is_static,
            stack_size: 0,
        };
        self.module.functions.push(ir_func);
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
                    // For pointer typedefs (e.g., typedef int *intptr;), wrap in Pointer
                    let mut resolved_type = decl.type_spec.clone();
                    for d in &declarator.derived {
                        match d {
                            DerivedDeclarator::Pointer => {
                                resolved_type = TypeSpecifier::Pointer(Box::new(resolved_type));
                            }
                            DerivedDeclarator::Array(size) => {
                                resolved_type = TypeSpecifier::Array(Box::new(resolved_type), size.clone());
                            }
                            _ => {}
                        }
                    }
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

            let base_ty = self.type_spec_to_ir(&decl.type_spec);
            let (mut alloc_size, elem_size, is_array, is_pointer, mut array_dim_strides) = self.compute_decl_info(&decl.type_spec, &declarator.derived);
            // For array-of-pointers or array-of-function-pointers, element type is Ptr
            let is_array_of_pointers = is_array && {
                let ptr_pos = declarator.derived.iter().position(|d| matches!(d, DerivedDeclarator::Pointer));
                let arr_pos = declarator.derived.iter().position(|d| matches!(d, DerivedDeclarator::Array(_)));
                matches!((ptr_pos, arr_pos), (Some(pp), Some(ap)) if pp < ap)
            };
            let is_array_of_func_ptrs = is_array && declarator.derived.iter().any(|d|
                matches!(d, DerivedDeclarator::FunctionPointer(_, _) | DerivedDeclarator::Function(_, _)));
            let var_ty = if is_pointer || is_array_of_pointers || is_array_of_func_ptrs { IrType::Ptr } else { base_ty };

            // For unsized arrays (int a[] = {...}), compute actual size from initializer
            let is_unsized_array = is_array && declarator.derived.iter().any(|d| {
                matches!(d, DerivedDeclarator::Array(None))
            });
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

            // Determine struct layout for global struct/pointer-to-struct variables
            let struct_layout = self.get_struct_layout_for_type(&decl.type_spec);
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

            self.module.globals.push(IrGlobal {
                name: declarator.name.clone(),
                ty: global_ty,
                size: actual_alloc_size,
                align,
                init,
                is_static,
                is_extern: is_extern_decl,
            });
        }
    }

    /// Lower a global initializer to a GlobalInit value.
    pub(super) fn lower_global_init(
        &mut self,
        init: &Initializer,
        _type_spec: &TypeSpecifier,
        base_ty: IrType,
        is_array: bool,
        elem_size: usize,
        total_size: usize,
        struct_layout: &Option<StructLayout>,
        array_dim_strides: &[usize],
    ) -> GlobalInit {
        // Check if the target type is long double (need to emit as x87 80-bit)
        let is_long_double_target = self.is_type_spec_long_double(_type_spec);

        match init {
            Initializer::Expr(expr) => {
                // Try to evaluate as a constant
                if let Some(val) = self.eval_const_expr(expr) {
                    // Convert integer constants to float if target type is float/double
                    let val = self.coerce_const_to_type(val, base_ty);
                    // If target is long double, promote F64 to LongDouble for proper encoding
                    let val = if is_long_double_target {
                        match val {
                            IrConst::F64(v) => IrConst::LongDouble(v),
                            IrConst::F32(v) => IrConst::LongDouble(v as f64),
                            IrConst::I64(v) => IrConst::LongDouble(v as f64),
                            IrConst::I32(v) => IrConst::LongDouble(v as f64),
                            other => other, // LongDouble already, or other type
                        }
                    } else {
                        val
                    };
                    return GlobalInit::Scalar(val);
                }
                // String literal initializer
                if let Expr::StringLiteral(s, _) = expr {
                    if is_array && (base_ty == IrType::I8 || base_ty == IrType::U8) {
                        // Char array: char s[] = "hello" -> inline the string bytes
                        return GlobalInit::String(s.clone());
                    } else {
                        // Pointer: const char *s = "hello" -> reference .rodata label
                        let label = format!(".Lstr{}", self.next_string);
                        self.next_string += 1;
                        self.module.string_literals.push((label.clone(), s.clone()));
                        return GlobalInit::GlobalAddr(label);
                    }
                }
                // Handle &(compound_literal) at file scope: create anonymous global
                if let Expr::AddressOf(inner, _) = expr {
                    if let Expr::CompoundLiteral(ref cl_type_spec, ref cl_init, _) = inner.as_ref() {
                        return self.create_compound_literal_global(cl_type_spec, cl_init);
                    }
                }
                // Handle (compound_literal) used as struct initializer value
                if let Expr::CompoundLiteral(ref cl_type_spec, ref cl_init, _) = expr {
                    if base_ty == IrType::Ptr {
                        // Pointer: create anonymous global and return address
                        return self.create_compound_literal_global(cl_type_spec, cl_init);
                    }
                }
                // Try to evaluate as a global address expression (e.g., &x, func, arr, &arr[3], &s.field)
                if let Some(addr_init) = self.eval_global_addr_expr(expr) {
                    return addr_init;
                }
                // Can't evaluate - zero init as fallback
                GlobalInit::Zero
            }
            Initializer::List(items) => {
                // Handle brace-wrapped string literal for char arrays:
                // char c[] = {"hello"} or static char c[] = {"hello"}
                if is_array && (base_ty == IrType::I8 || base_ty == IrType::U8) {
                    if items.len() == 1 && items[0].designators.is_empty() {
                        if let Initializer::Expr(Expr::StringLiteral(s, _)) = &items[0].init {
                            return GlobalInit::String(s.clone());
                        }
                    }
                }

                if is_array && elem_size > 0 {
                    // For struct arrays, elem_size is the actual struct size (from sizeof_type),
                    // whereas base_ty.size() may return Ptr size (8). Use elem_size for structs.
                    let num_elems = if struct_layout.is_some() {
                        total_size / elem_size.max(1)
                    } else {
                        let base_type_size = base_ty.size().max(1);
                        total_size / base_type_size
                    };

                    // Array of structs: emit as byte array using struct layout.
                    // But skip byte-serialization if any struct field is a pointer type
                    // (pointers need .quad directives for address relocations).
                    let has_ptr_fields = struct_layout.as_ref().map_or(false, |layout| {
                        layout.fields.iter().any(|f| matches!(f.ty, CType::Pointer(_)))
                    });
                    if let Some(ref layout) = struct_layout {
                        if has_ptr_fields {
                            // Use Compound approach for struct arrays with pointer fields
                            return self.lower_struct_array_with_ptrs(items, layout, num_elems);
                        }
                        let struct_size = layout.size;
                        let mut bytes = vec![0u8; total_size];
                        let mut current_idx = 0usize;
                        let mut item_idx = 0usize;
                        while item_idx < items.len() {
                            let item = &items[item_idx];
                            // Check for index designator
                            if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                                if let Some(idx) = self.eval_const_expr(idx_expr).and_then(|c| c.to_usize()) {
                                    current_idx = idx;
                                }
                            }
                            if current_idx >= num_elems {
                                break;
                            }
                            let base_offset = current_idx * struct_size;
                            // Check for field designator after index designator: [idx].field = val
                            let field_designator_name = item.designators.iter().find_map(|d| {
                                if let Designator::Field(ref name) = d {
                                    Some(name.clone())
                                } else {
                                    None
                                }
                            });
                            match &item.init {
                                Initializer::List(sub_items) => {
                                    // Sub-list initializer for struct element
                                    self.write_struct_init_to_bytes(&mut bytes, base_offset, sub_items, layout);
                                    item_idx += 1;
                                }
                                Initializer::Expr(expr) => {
                                    if let Some(ref fname) = field_designator_name {
                                        // [idx].field = val: write to specific field
                                        if let Some(val) = self.eval_const_expr(expr) {
                                            if let Some(field) = layout.fields.iter().find(|f| &f.name == fname) {
                                                let field_ir_ty = IrType::from_ctype(&field.ty);
                                                self.write_const_to_bytes(&mut bytes, base_offset + field.offset, &val, field_ir_ty);
                                            }
                                        }
                                        item_idx += 1;
                                    } else {
                                        // Flat init: consume items for all fields of this struct
                                        let consumed = self.fill_struct_global_bytes(&items[item_idx..], layout, &mut bytes, base_offset);
                                        item_idx += consumed;
                                    }
                                }
                            }
                            // Only advance current_idx if no field designator (sequential init)
                            if field_designator_name.is_none() {
                                current_idx += 1;
                            }
                        }
                        let values: Vec<IrConst> = bytes.iter().map(|&b| IrConst::I8(b as i8)).collect();
                        return GlobalInit::Array(values);
                    }

                    // Check if any element is an address expression or string literal
                    // (for pointer arrays like char *arr[] or func_ptr arr[])
                    // For multi-dim char arrays (char a[2][4] = {"abc", "xyz"}), string literals
                    // should be inlined as bytes, not treated as address expressions.
                    // Distinguish from char *arr[] by checking array_dim_strides.len() > 1.
                    let is_multidim_char_array = matches!(base_ty, IrType::I8 | IrType::U8)
                        && array_dim_strides.len() > 1;
                    let has_addr_exprs = items.iter().any(|item| {
                        if let Initializer::Expr(expr) = &item.init {
                            if matches!(expr, Expr::StringLiteral(_, _)) {
                                !is_multidim_char_array
                            } else {
                                self.eval_const_expr(expr).is_none() && self.eval_global_addr_expr(expr).is_some()
                            }
                        } else {
                            false
                        }
                    });

                    if has_addr_exprs {
                        // Use Compound initializer for arrays containing address expressions
                        let mut elements = Vec::with_capacity(num_elems);
                        for item in items {
                            if let Initializer::Expr(expr) = &item.init {
                                if let Expr::StringLiteral(s, _) = expr {
                                    // String literal: create .rodata entry and use its label
                                    let label = format!(".Lstr{}", self.next_string);
                                    self.next_string += 1;
                                    self.module.string_literals.push((label.clone(), s.clone()));
                                    elements.push(GlobalInit::GlobalAddr(label));
                                } else if let Some(val) = self.eval_const_expr(expr) {
                                    elements.push(GlobalInit::Scalar(val));
                                } else if let Some(addr) = self.eval_global_addr_expr(expr) {
                                    elements.push(addr);
                                } else {
                                    elements.push(GlobalInit::Zero);
                                }
                            } else {
                                elements.push(GlobalInit::Zero);
                            }
                        }
                        // Zero-fill remaining
                        while elements.len() < num_elems {
                            elements.push(GlobalInit::Zero);
                        }
                        return GlobalInit::Compound(elements);
                    }

                    let mut values = vec![self.zero_const(base_ty); num_elems];
                    // For multi-dim arrays, flatten nested init lists
                    if array_dim_strides.len() > 1 {
                        // For multi-dim arrays, num_elems is outer dimension count,
                        // but we need total scalar elements for the flattened init
                        let innermost_stride = array_dim_strides.last().copied().unwrap_or(1).max(1);
                        let total_scalar_elems = total_size / innermost_stride;
                        let mut values_flat = vec![self.zero_const(base_ty); total_scalar_elems];
                        let mut flat = Vec::with_capacity(total_scalar_elems);
                        self.flatten_global_array_init(items, array_dim_strides, base_ty, &mut flat);
                        for (i, v) in flat.into_iter().enumerate() {
                            if i < total_scalar_elems {
                                values_flat[i] = v;
                            }
                        }
                        return GlobalInit::Array(values_flat);
                    } else {
                        // Support designated initializers: [idx] = val
                        let mut current_idx = 0usize;
                        for item in items {
                            // Check for index designator
                            if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                                if let Some(idx) = self.eval_const_expr(idx_expr).and_then(|c| c.to_usize()) {
                                    current_idx = idx;
                                }
                            }
                            if current_idx < num_elems {
                                let val = match &item.init {
                                    Initializer::Expr(expr) => {
                                        let raw = self.eval_const_expr(expr).unwrap_or(self.zero_const(base_ty));
                                        self.coerce_const_to_type(raw, base_ty)
                                    }
                                    Initializer::List(sub_items) => {
                                        // Nested list - flatten
                                        let mut sub_vals = Vec::new();
                                        for sub in sub_items {
                                            self.flatten_global_init_item(&sub.init, base_ty, &mut sub_vals);
                                        }
                                        let raw = sub_vals.into_iter().next().unwrap_or(self.zero_const(base_ty));
                                        self.coerce_const_to_type(raw, base_ty)
                                    }
                                };
                                values[current_idx] = val;
                            }
                            current_idx += 1;
                        }
                    }
                    return GlobalInit::Array(values);
                }

                // Struct/union initializer list: emit field-by-field constants
                if let Some(ref layout) = struct_layout {
                    return self.lower_struct_global_init(items, layout);
                }

                // Scalar with braces: int x = { 1 };
                // C11 6.7.9: A scalar can be initialized with a single braced expression.
                if !is_array && items.len() >= 1 {
                    if let Initializer::Expr(expr) = &items[0].init {
                        if let Some(val) = self.eval_const_expr(expr) {
                            return GlobalInit::Scalar(self.coerce_const_to_type(val, base_ty));
                        }
                        // Try address expression
                        if let Some(addr_init) = self.eval_global_addr_expr(expr) {
                            return addr_init;
                        }
                    }
                }

                // Fallback: try to emit as an array of I32 constants
                // (handles cases like plain `{1, 2, 3}` without type info)
                let mut values = Vec::new();
                for item in items {
                    if let Initializer::Expr(expr) = &item.init {
                        if let Some(val) = self.eval_const_expr(expr) {
                            values.push(val);
                        } else {
                            values.push(IrConst::I32(0));
                        }
                    } else {
                        values.push(IrConst::I32(0));
                    }
                }
                if !values.is_empty() {
                    return GlobalInit::Array(values);
                }
                GlobalInit::Zero
            }
        }
    }

    /// Create an anonymous global for a compound literal at file scope.
    /// Used for: struct S *s = &(struct S){1, 2};
    fn create_compound_literal_global(
        &mut self,
        type_spec: &TypeSpecifier,
        init: &Initializer,
    ) -> GlobalInit {
        let label = format!(".Lcompound_lit_{}", self.next_anon_struct);
        self.next_anon_struct += 1;

        let base_ty = self.type_spec_to_ir(type_spec);
        let struct_layout = self.get_struct_layout_for_type(type_spec);
        let alloc_size = if let Some(ref layout) = struct_layout {
            layout.size
        } else {
            self.sizeof_type(type_spec)
        };

        let align = if let Some(ref layout) = struct_layout {
            layout.align.max(base_ty.align())
        } else {
            base_ty.align()
        };

        let global_init = self.lower_global_init(init, type_spec, base_ty, false, 0, alloc_size, &struct_layout, &[]);

        let global_ty = if matches!(&global_init, GlobalInit::Array(vals) if !vals.is_empty() && matches!(vals[0], IrConst::I8(_))) {
            IrType::I8
        } else if struct_layout.is_some() && matches!(global_init, GlobalInit::Array(_)) {
            IrType::I8
        } else {
            base_ty
        };

        self.module.globals.push(IrGlobal {
            name: label.clone(),
            ty: global_ty,
            size: alloc_size,
            align,
            init: global_init,
            is_static: true, // compound literal globals are local to translation unit
            is_extern: false,
        });

        GlobalInit::GlobalAddr(label)
    }

    /// Lower a struct initializer list to a GlobalInit::Array of field values.
    /// Emits each field's value at its appropriate position, with padding bytes as zeros.
    /// Supports designated initializers like {.b = 2, .a = 1}.
    fn lower_struct_global_init(
        &mut self,
        items: &[InitializerItem],
        layout: &StructLayout,
    ) -> GlobalInit {
        // Check if any field has an address expression (needs relocation).
        // This includes string literals used to initialize pointer fields.
        let has_addr_fields = items.iter().enumerate().any(|(idx, item)| {
            if let Initializer::Expr(expr) = &item.init {
                // String literals initializing pointer fields need relocations
                if matches!(expr, Expr::StringLiteral(_, _)) {
                    // Check if the corresponding field is a pointer type
                    let field_idx = self.resolve_struct_init_field_idx(item, layout, idx);
                    if field_idx < layout.fields.len() {
                        if matches!(layout.fields[field_idx].ty, CType::Pointer(_)) {
                            return true;
                        }
                    }
                }
                self.eval_const_expr(expr).is_none() && self.eval_global_addr_expr(expr).is_some()
            } else {
                false
            }
        });

        if has_addr_fields {
            return self.lower_struct_global_init_compound(items, layout);
        }

        let total_size = layout.size;
        let mut bytes = vec![0u8; total_size];
        self.fill_struct_global_bytes(items, layout, &mut bytes, 0);

        // Emit as array of I8 (byte) constants
        let values: Vec<IrConst> = bytes.iter().map(|&b| IrConst::I8(b as i8)).collect();
        GlobalInit::Array(values)
    }

    /// Recursively fill byte buffer for struct global initialization.
    /// Returns the number of initializer items consumed.
    fn fill_struct_global_bytes(
        &self,
        items: &[InitializerItem],
        layout: &StructLayout,
        bytes: &mut [u8],
        base_offset: usize,
    ) -> usize {
        let mut item_idx = 0usize;
        let mut current_field_idx = 0usize;

        while item_idx < items.len() {
            let item = &items[item_idx];

            let designator_name = match item.designators.first() {
                Some(Designator::Field(ref name)) => Some(name.as_str()),
                _ => None,
            };
            // Check for array index designator after field designator: .field[idx]
            let array_start_idx: Option<usize> = if designator_name.is_some() {
                item.designators.iter().find_map(|d| {
                    if let Designator::Index(ref idx_expr) = d {
                        self.eval_const_expr(idx_expr).and_then(|c| c.to_usize())
                    } else {
                        None
                    }
                })
            } else {
                // Check for bare index designator on current field (when field is an array)
                match item.designators.first() {
                    Some(Designator::Index(ref idx_expr)) => {
                        self.eval_const_expr(idx_expr).and_then(|c| c.to_usize())
                    }
                    _ => None,
                }
            };
            let field_idx = match layout.resolve_init_field_idx(designator_name, current_field_idx) {
                Some(idx) => idx,
                None => {
                    if designator_name.is_some() { item_idx += 1; continue; }
                    break;
                }
            };
            let field_layout = &layout.fields[field_idx];
            let field_offset = base_offset + field_layout.offset;

            match &field_layout.ty {
                CType::Struct(st) => {
                    let sub_layout = StructLayout::for_struct(&st.fields);
                    match &item.init {
                        Initializer::List(sub_items) => {
                            // Nested braces: recursively fill inner struct
                            self.fill_struct_global_bytes(sub_items, &sub_layout, bytes, field_offset);
                            item_idx += 1;
                        }
                        Initializer::Expr(_) => {
                            // Flat init: consume items for inner struct
                            let consumed = self.fill_struct_global_bytes(&items[item_idx..], &sub_layout, bytes, field_offset);
                            item_idx += consumed;
                        }
                    }
                }
                CType::Union(st) => {
                    let sub_layout = StructLayout::for_union(&st.fields);
                    match &item.init {
                        Initializer::List(sub_items) => {
                            self.fill_struct_global_bytes(sub_items, &sub_layout, bytes, field_offset);
                            item_idx += 1;
                        }
                        Initializer::Expr(_) => {
                            let consumed = self.fill_struct_global_bytes(&items[item_idx..], &sub_layout, bytes, field_offset);
                            item_idx += consumed;
                        }
                    }
                }
                CType::Array(elem_ty, Some(arr_size)) => {
                    let elem_size = elem_ty.size();
                    let elem_ir_ty = IrType::from_ctype(elem_ty);
                    match &item.init {
                        Initializer::List(sub_items) => {
                            // Check for brace-wrapped string literal: { "hello" }
                            if sub_items.len() == 1 && sub_items[0].designators.is_empty() {
                                if let Initializer::Expr(Expr::StringLiteral(s, _)) = &sub_items[0].init {
                                    if matches!(elem_ty.as_ref(), CType::Char | CType::UChar) {
                                        let str_bytes = s.as_bytes();
                                        for (i, &b) in str_bytes.iter().enumerate() {
                                            if i >= *arr_size { break; }
                                            if field_offset + i < bytes.len() {
                                                bytes[field_offset + i] = b;
                                            }
                                        }
                                        // null terminator
                                        if str_bytes.len() < *arr_size && field_offset + str_bytes.len() < bytes.len() {
                                            bytes[field_offset + str_bytes.len()] = 0;
                                        }
                                        item_idx += 1;
                                        current_field_idx = field_idx + 1;
                                        continue;
                                    }
                                }
                            }
                            if matches!(elem_ty.as_ref(), CType::Struct(_) | CType::Union(_)) {
                                // Array of structs: each sub_item is a struct element initializer.
                                // Sub-items may be Lists (braced struct init) or Exprs (flat filling).
                                if let CType::Struct(st) = elem_ty.as_ref() {
                                    let sub_layout = StructLayout::for_struct(&st.fields);
                                    let mut sub_idx = 0usize;
                                    let mut ai = 0usize;
                                    while ai < *arr_size && sub_idx < sub_items.len() {
                                        let elem_offset = field_offset + ai * elem_size;
                                        match &sub_items[sub_idx].init {
                                            Initializer::List(inner_items) => {
                                                self.fill_struct_global_bytes(inner_items, &sub_layout, bytes, elem_offset);
                                                sub_idx += 1;
                                            }
                                            Initializer::Expr(_) => {
                                                let consumed = self.fill_struct_global_bytes(&sub_items[sub_idx..], &sub_layout, bytes, elem_offset);
                                                sub_idx += consumed;
                                            }
                                        }
                                        ai += 1;
                                    }
                                } else if let CType::Union(st) = elem_ty.as_ref() {
                                    let sub_layout = StructLayout::for_union(&st.fields);
                                    let mut sub_idx = 0usize;
                                    for ai in 0..*arr_size {
                                        if sub_idx >= sub_items.len() { break; }
                                        let elem_offset = field_offset + ai * elem_size;
                                        match &sub_items[sub_idx].init {
                                            Initializer::List(inner_items) => {
                                                self.fill_struct_global_bytes(inner_items, &sub_layout, bytes, elem_offset);
                                                sub_idx += 1;
                                            }
                                            Initializer::Expr(_) => {
                                                let consumed = self.fill_struct_global_bytes(&sub_items[sub_idx..], &sub_layout, bytes, elem_offset);
                                                sub_idx += consumed;
                                            }
                                        }
                                    }
                                }
                            } else {
                                // Array of scalars: each sub_item is a scalar
                                // Supports [idx]=val designators within the sub-list
                                let mut ai = 0usize;
                                for sub_item in sub_items.iter() {
                                    // Check for index designator: [idx]=val
                                    if let Some(Designator::Index(ref idx_expr)) = sub_item.designators.first() {
                                        if let Some(idx) = self.eval_const_expr(idx_expr).and_then(|c| c.to_usize()) {
                                            ai = idx;
                                        }
                                    }
                                    if ai >= *arr_size { break; }
                                    let elem_offset = field_offset + ai * elem_size;
                                    if let Initializer::Expr(expr) = &sub_item.init {
                                        let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                                        self.write_const_to_bytes(bytes, elem_offset, &val, elem_ir_ty);
                                    } else if let Initializer::List(inner) = &sub_item.init {
                                        // Brace-wrapped scalar: {5}
                                        if let Some(first) = inner.first() {
                                            if let Initializer::Expr(expr) = &first.init {
                                                let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                                                self.write_const_to_bytes(bytes, elem_offset, &val, elem_ir_ty);
                                            }
                                        }
                                    }
                                    ai += 1;
                                }
                            }
                            item_idx += 1;
                        }
                        Initializer::Expr(expr) => {
                            // String literal initializer for char array field
                            if let Expr::StringLiteral(s, _) = expr {
                                if matches!(elem_ty.as_ref(), CType::Char | CType::UChar) {
                                    let str_bytes = s.as_bytes();
                                    for (i, &b) in str_bytes.iter().enumerate() {
                                        if i >= *arr_size { break; }
                                        if field_offset + i < bytes.len() {
                                            bytes[field_offset + i] = b;
                                        }
                                    }
                                    // null terminator
                                    if str_bytes.len() < *arr_size && field_offset + str_bytes.len() < bytes.len() {
                                        bytes[field_offset + str_bytes.len()] = 0;
                                    }
                                    item_idx += 1;
                                    current_field_idx = field_idx + 1;
                                    continue;
                                }
                            }
                            // Flat initializer: fill array elements from consecutive items
                            // in the init list. E.g., struct { int a[3]; } x = {1, 2, 3};
                            // The items 1, 2, 3 fill a[0], a[1], a[2] respectively.
                            // If an index designator is present (e.g., .a[1]=val), start from that index.
                            let start_ai = array_start_idx.unwrap_or(0);
                            if matches!(elem_ty.as_ref(), CType::Struct(_) | CType::Union(_)) {
                                // Array of structs: consume items for each struct element
                                if let CType::Struct(st) = elem_ty.as_ref() {
                                    let sub_layout = StructLayout::for_struct(&st.fields);
                                    for ai in start_ai..*arr_size {
                                        if item_idx >= items.len() { break; }
                                        let elem_offset = field_offset + ai * elem_size;
                                        let consumed = self.fill_struct_global_bytes(&items[item_idx..], &sub_layout, bytes, elem_offset);
                                        item_idx += consumed;
                                    }
                                } else {
                                    // Union: take one item per element
                                    for ai in start_ai..*arr_size {
                                        if item_idx >= items.len() { break; }
                                        let elem_offset = field_offset + ai * elem_size;
                                        if let Initializer::Expr(e) = &items[item_idx].init {
                                            let val = self.eval_const_expr(e).unwrap_or(IrConst::I64(0));
                                            self.write_const_to_bytes(bytes, elem_offset, &val, elem_ir_ty);
                                        }
                                        item_idx += 1;
                                    }
                                }
                            } else {
                                // Array of scalars: consume up to arr_size items starting from start_ai
                                let mut consumed = 0usize;
                                let mut ai = start_ai;
                                while ai < *arr_size && (item_idx + consumed) < items.len() {
                                    let cur_item = &items[item_idx + consumed];
                                    // Stop if we hit a designator (targeting a different field)
                                    if !cur_item.designators.is_empty() && consumed > 0 { break; }
                                    if let Initializer::Expr(e) = &cur_item.init {
                                        let val = self.eval_const_expr(e).unwrap_or(IrConst::I64(0));
                                        let elem_offset = field_offset + ai * elem_size;
                                        self.write_const_to_bytes(bytes, elem_offset, &val, elem_ir_ty);
                                        consumed += 1;
                                        ai += 1;
                                    } else {
                                        break;
                                    }
                                }
                                item_idx += consumed.max(1);
                            }
                            // Don't increment item_idx here; it was already advanced in the loop
                            current_field_idx = field_idx + 1;
                            continue;
                        }
                    }
                }
                _ => {
                    // Scalar field (possibly bitfield)
                    let field_ir_ty = IrType::from_ctype(&field_layout.ty);
                    let val = match &item.init {
                        Initializer::Expr(expr) => {
                            self.eval_const_expr(expr).unwrap_or(IrConst::I64(0))
                        }
                        Initializer::List(sub_items) => {
                            // Scalar with braces: int x = {5};
                            if let Some(first) = sub_items.first() {
                                if let Initializer::Expr(expr) = &first.init {
                                    self.eval_const_expr(expr).unwrap_or(IrConst::I64(0))
                                } else { IrConst::I64(0) }
                            } else { IrConst::I64(0) }
                        }
                    };

                    if let (Some(bit_offset), Some(bit_width)) = (field_layout.bit_offset, field_layout.bit_width) {
                        // Bitfield: pack into the storage unit bytes using read-modify-write
                        self.write_bitfield_to_bytes(bytes, field_offset, &val, field_ir_ty, bit_offset, bit_width);
                    } else {
                        self.write_const_to_bytes(bytes, field_offset, &val, field_ir_ty);
                    }
                    item_idx += 1;
                }
            }
            current_field_idx = field_idx + 1;

            // For unions, only one member can be active. If this was a flat init
            // (no field designator), stop after the first field. Designated inits
            // (.a = 1, .c = 2) are allowed to overwrite - the last designator wins.
            if layout.is_union && designator_name.is_none() {
                break;
            }
        }
        item_idx
    }

    /// Lower a struct global init that contains address expressions.
    /// Emits field-by-field using Compound, with padding bytes between fields.
    fn lower_struct_global_init_compound(
        &mut self,
        items: &[InitializerItem],
        layout: &StructLayout,
    ) -> GlobalInit {
        let total_size = layout.size;
        let mut elements: Vec<GlobalInit> = Vec::new();
        let mut current_offset = 0usize;
        let mut current_field_idx = 0usize;

        // Build a map of field_idx -> initializer value
        let mut field_inits: Vec<Option<&InitializerItem>> = vec![None; layout.fields.len()];
        for item in items {
            let field_idx = self.resolve_struct_init_field_idx(item, layout, current_field_idx);
            if field_idx >= layout.fields.len() { continue; }
            field_inits[field_idx] = Some(item);
            current_field_idx = field_idx + 1;
            // For unions with flat init, stop after first member.
            // Designated inits can overwrite (last wins).
            let has_designator = !item.designators.is_empty();
            if layout.is_union && !has_designator { break; }
        }

        for fi in 0..layout.fields.len() {
            let field_offset = layout.fields[fi].offset;
            let field_size = layout.fields[fi].ty.size();
            let field_is_pointer = matches!(layout.fields[fi].ty, CType::Pointer(_));

            // Emit padding before this field
            if field_offset > current_offset {
                let pad = field_offset - current_offset;
                for _ in 0..pad {
                    elements.push(GlobalInit::Scalar(IrConst::I8(0)));
                }
                current_offset = field_offset;
            }

            if let Some(item) = field_inits[fi] {
                match &item.init {
                    Initializer::Expr(expr) => {
                        if let Expr::StringLiteral(s, _) = expr {
                            if field_is_pointer {
                                // String literal initializing a pointer field:
                                // create a .rodata string entry and emit GlobalAddr
                                let label = format!(".Lstr{}", self.next_string);
                                self.next_string += 1;
                                self.module.string_literals.push((label.clone(), s.clone()));
                                elements.push(GlobalInit::GlobalAddr(label));
                            } else {
                                // String literal initializing a char array field
                                let s_bytes = s.as_bytes();
                                for (i, &b) in s_bytes.iter().enumerate() {
                                    if i >= field_size { break; }
                                    elements.push(GlobalInit::Scalar(IrConst::I8(b as i8)));
                                }
                                // null terminator + remaining zero fill
                                for _ in s_bytes.len()..field_size {
                                    elements.push(GlobalInit::Scalar(IrConst::I8(0)));
                                }
                            }
                        } else if let Some(val) = self.eval_const_expr(expr) {
                            // Scalar constant - emit as bytes
                            self.push_const_as_bytes(&mut elements, &val, field_size);
                        } else if let Some(addr_init) = self.eval_global_addr_expr(expr) {
                            // Address expression - emit as relocation
                            elements.push(addr_init);
                        } else {
                            // Unknown - zero fill
                            for _ in 0..field_size {
                                elements.push(GlobalInit::Scalar(IrConst::I8(0)));
                            }
                        }
                    }
                    Initializer::List(nested_items) => {
                        // Nested struct/array init - serialize to bytes
                        let mut bytes = vec![0u8; field_size];
                        let field_ty = layout.fields[fi].ty.clone();
                        if let Some(nested_layout) = self.get_struct_layout_for_ctype(&field_ty) {
                            self.fill_struct_global_bytes(nested_items, &nested_layout, &mut bytes, 0);
                        } else {
                            // Simple array or scalar list
                            let mut byte_offset = 0;
                            let elem_ty = match &field_ty {
                                CType::Array(inner, _) => IrType::from_ctype(inner),
                                other => IrType::from_ctype(other),
                            };
                            let elem_size = elem_ty.size().max(1);
                            for ni in nested_items {
                                if byte_offset >= field_size { break; }
                                if let Initializer::Expr(ref e) = ni.init {
                                    if let Some(val) = self.eval_const_expr(e) {
                                        let val = self.coerce_const_to_type(val, elem_ty);
                                        self.write_const_to_bytes(&mut bytes, byte_offset, &val, elem_ty);
                                    }
                                }
                                byte_offset += elem_size;
                            }
                        }
                        for b in &bytes {
                            elements.push(GlobalInit::Scalar(IrConst::I8(*b as i8)));
                        }
                    }
                }
            } else {
                // No initializer for this field - zero fill
                for _ in 0..field_size {
                    elements.push(GlobalInit::Scalar(IrConst::I8(0)));
                }
            }
            current_offset += field_size;
        }

        // Trailing padding
        while current_offset < total_size {
            elements.push(GlobalInit::Scalar(IrConst::I8(0)));
            current_offset += 1;
        }

        GlobalInit::Compound(elements)
    }

    /// Push a constant value as individual bytes into a compound init element list.
    fn push_const_as_bytes(&self, elements: &mut Vec<GlobalInit>, val: &IrConst, size: usize) {
        let mut bytes = Vec::with_capacity(size);
        val.push_le_bytes(&mut bytes, size);
        for b in bytes {
            elements.push(GlobalInit::Scalar(IrConst::I8(b as i8)));
        }
    }

    /// Resolve which struct field a positional or designated initializer targets.
    fn resolve_struct_init_field_idx(
        &self,
        item: &InitializerItem,
        layout: &StructLayout,
        current_field_idx: usize,
    ) -> usize {
        let designator_name = match item.designators.first() {
            Some(Designator::Field(ref name)) => Some(name.as_str()),
            _ => None,
        };
        layout.resolve_init_field_idx(designator_name, current_field_idx)
            .unwrap_or(current_field_idx)
    }

    /// Write an initializer item to a byte buffer at the given offset.
    fn write_init_item_to_bytes(&self, bytes: &mut [u8], offset: usize, init: &Initializer, field_ty: &CType) {
        match init {
            Initializer::Expr(expr) => {
                let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                let field_ir_ty = IrType::from_ctype(field_ty);
                self.write_const_to_bytes(bytes, offset, &val, field_ir_ty);
            }
            Initializer::List(sub_items) => {
                // Try struct/union layout first
                if let Some(sub_layout) = self.get_struct_layout_for_ctype(field_ty) {
                    self.write_struct_init_to_bytes(bytes, offset, sub_items, &sub_layout);
                } else if let CType::Array(elem_ty, count) = field_ty {
                    let elem_size = elem_ty.size();
                    for (idx, sub_item) in sub_items.iter().enumerate() {
                        if count.map_or(false, |c| idx >= c) { break; }
                        if let Initializer::Expr(expr) = &sub_item.init {
                            let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                            let elem_ir_ty = IrType::from_ctype(elem_ty);
                            self.write_const_to_bytes(bytes, offset + idx * elem_size, &val, elem_ir_ty);
                        }
                    }
                } else {
                    // Scalar in braces
                    if let Some(first) = sub_items.first() {
                        if let Initializer::Expr(expr) = &first.init {
                            let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                            let field_ir_ty = IrType::from_ctype(field_ty);
                            self.write_const_to_bytes(bytes, offset, &val, field_ir_ty);
                        }
                    }
                }
            }
        }
    }

    /// Write an IrConst value to a byte buffer at the given offset using the field's IR type.
    fn write_const_to_bytes(&self, bytes: &mut [u8], offset: usize, val: &IrConst, ty: IrType) {
        let coerced = val.coerce_to(ty);
        let size = ty.size();
        let mut le_buf = Vec::with_capacity(size);
        coerced.push_le_bytes(&mut le_buf, size);
        for (i, &b) in le_buf.iter().enumerate() {
            if offset + i < bytes.len() {
                bytes[offset + i] = b;
            }
        }
    }

    /// Write a bitfield value into a byte buffer at the given offset.
    /// Uses read-modify-write to pack the value at the correct bit position.
    fn write_bitfield_to_bytes(&self, bytes: &mut [u8], offset: usize, val: &IrConst, ty: IrType, bit_offset: u32, bit_width: u32) {
        let int_val = val.to_u64().unwrap_or(0);

        let size = ty.size();
        let mask = if bit_width >= 64 { u64::MAX } else { (1u64 << bit_width) - 1 };
        let field_val = (int_val & mask) << bit_offset;
        let clear_mask = !(mask << bit_offset);

        // Read current storage unit value (little-endian)
        let mut current = 0u64;
        for i in 0..size {
            if offset + i < bytes.len() {
                current |= (bytes[offset + i] as u64) << (i * 8);
            }
        }

        // Modify: clear field bits and OR in new value
        let new_val = (current & clear_mask) | field_val;

        // Write back (little-endian)
        let le = new_val.to_le_bytes();
        for i in 0..size {
            if offset + i < bytes.len() {
                bytes[offset + i] = le[i];
            }
        }
    }

    /// Write a struct initializer list to a byte buffer at the given base offset.
    /// Handles nested struct fields recursively.
    fn write_struct_init_to_bytes(
        &self,
        bytes: &mut [u8],
        base_offset: usize,
        items: &[InitializerItem],
        layout: &StructLayout,
    ) {
        let mut current_field_idx = 0usize;
        for item in items {
            let designator_name = match item.designators.first() {
                Some(Designator::Field(ref name)) => Some(name.as_str()),
                _ => None,
            };
            let field_idx = match layout.resolve_init_field_idx(designator_name, current_field_idx) {
                Some(idx) => idx,
                None => break,
            };

            let field_layout = &layout.fields[field_idx];
            let field_offset = base_offset + field_layout.offset;

            match &item.init {
                Initializer::Expr(expr) => {
                    if let Expr::StringLiteral(s, _) = expr {
                        // String literal initializing a char array field
                        if let CType::Array(_, Some(arr_size)) = &field_layout.ty {
                            let s_bytes = s.as_bytes();
                            for (i, &b) in s_bytes.iter().enumerate() {
                                if i >= *arr_size { break; }
                                if field_offset + i < bytes.len() {
                                    bytes[field_offset + i] = b;
                                }
                            }
                            if s_bytes.len() < *arr_size && field_offset + s_bytes.len() < bytes.len() {
                                bytes[field_offset + s_bytes.len()] = 0;
                            }
                        }
                    } else {
                        let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                        let field_ir_ty = IrType::from_ctype(&field_layout.ty);
                        if let (Some(bit_offset), Some(bit_width)) = (field_layout.bit_offset, field_layout.bit_width) {
                            self.write_bitfield_to_bytes(bytes, field_offset, &val, field_ir_ty, bit_offset, bit_width);
                        } else {
                            self.write_const_to_bytes(bytes, field_offset, &val, field_ir_ty);
                        }
                    }
                }
                Initializer::List(sub_items) => {
                    // Nested struct/union: recurse if the field is a struct type
                    if let Some(sub_layout) = self.get_struct_layout_for_ctype(&field_layout.ty) {
                        self.write_struct_init_to_bytes(bytes, field_offset, sub_items, &sub_layout);
                    } else {
                        // Array field or other nested init: try to write elements sequentially
                        if let CType::Array(ref elem_ty, _) = field_layout.ty {
                            let elem_ir_ty = IrType::from_ctype(elem_ty);
                            let elem_size = elem_ty.size();
                            for (i, sub_item) in sub_items.iter().enumerate() {
                                if let Initializer::Expr(expr) = &sub_item.init {
                                    let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                                    self.write_const_to_bytes(bytes, field_offset + i * elem_size, &val, elem_ir_ty);
                                }
                            }
                        } else if let Some(first) = sub_items.first() {
                            if let Initializer::Expr(expr) = &first.init {
                                let val = self.eval_const_expr(expr).unwrap_or(IrConst::I64(0));
                                let field_ir_ty = IrType::from_ctype(&field_layout.ty);
                                self.write_const_to_bytes(bytes, field_offset, &val, field_ir_ty);
                            }
                        }
                    }
                }
            }

            current_field_idx = field_idx + 1;

            // For unions with flat init, only the first member can be initialized.
            // Designated inits can overwrite (last designator wins).
            if layout.is_union && designator_name.is_none() {
                break;
            }
        }
    }

    /// Resolve a pointer field's initializer expression to a GlobalInit.
    /// Handles string literals, global addresses, function pointers, etc.
    fn resolve_ptr_field_init(&mut self, expr: &Expr) -> Option<GlobalInit> {
        // String literal: create a .rodata entry and reference it
        if let Expr::StringLiteral(s, _) = expr {
            let label = format!(".Lstr{}", self.next_string);
            self.next_string += 1;
            self.module.string_literals.push((label.clone(), s.clone()));
            return Some(GlobalInit::GlobalAddr(label));
        }
        // Try as a global address expression (&x, func name, array name, etc.)
        if let Some(addr) = self.eval_global_addr_expr(expr) {
            return Some(addr);
        }
        // Integer constant 0 -> null pointer
        if let Some(val) = self.eval_const_expr(expr) {
            if let Some(v) = self.const_to_i64(&val) {
                if v == 0 {
                    return None; // Will be zero in the byte buffer
                }
            }
            // Non-zero constant pointer (unusual but possible)
            return Some(GlobalInit::Scalar(val));
        }
        None
    }

    /// Get a StructLayout for a CType if it's a struct or union.
    fn get_struct_layout_for_ctype(&self, ty: &CType) -> Option<StructLayout> {
        match ty {
            CType::Struct(st) => {
                // First try to look up by name in registered struct_layouts
                if let Some(ref name) = st.name {
                    let key = format!("struct.{}", name);
                    if let Some(layout) = self.struct_layouts.get(&key) {
                        return Some(layout.clone());
                    }
                }
                // Compute from fields
                Some(StructLayout::for_struct(&st.fields))
            }
            CType::Union(st) => {
                if let Some(ref name) = st.name {
                    let key = format!("union.{}", name);
                    if let Some(layout) = self.struct_layouts.get(&key) {
                        return Some(layout.clone());
                    }
                }
                Some(StructLayout::for_union(&st.fields))
            }
            _ => None,
        }
    }

    /// Lower an array of structs where some fields are pointers.
    /// Uses byte-level serialization but with Compound for address elements.
    /// Each struct element is emitted as a mix of byte constants and pointer-sized addresses.
    fn lower_struct_array_with_ptrs(
        &mut self,
        items: &[InitializerItem],
        layout: &StructLayout,
        num_elems: usize,
    ) -> GlobalInit {
        // We emit the entire array as a sequence of Compound elements.
        // Each element is either:
        //   - Scalar(I8) for byte-level constant data
        //   - GlobalAddr for pointer fields
        let struct_size = layout.size;
        let total_size = num_elems * struct_size;
        let mut compound_elements: Vec<GlobalInit> = Vec::new();

        // Initialize with zero bytes
        let mut bytes = vec![0u8; total_size];
        // Track which byte ranges are pointer fields that need address relocations
        let mut ptr_ranges: Vec<(usize, GlobalInit)> = Vec::new(); // (byte_offset, addr_init)

        for elem_idx in 0..num_elems {
            let item = items.get(elem_idx);
            let base_offset = elem_idx * struct_size;

            if let Some(item) = item {
                match &item.init {
                    Initializer::List(sub_items) => {
                        let mut current_field_idx = 0usize;
                        for sub_item in sub_items {
                            let desig_name = match sub_item.designators.first() {
                                Some(Designator::Field(ref name)) => Some(name.as_str()),
                                _ => None,
                            };
                            let field_idx = match layout.resolve_init_field_idx(desig_name, current_field_idx) {
                                Some(idx) => idx,
                                None => break,
                            };
                            let field = &layout.fields[field_idx];
                            let field_offset = base_offset + field.offset;
                            let field_ir_ty = IrType::from_ctype(&field.ty);
                            let is_ptr_field = matches!(field.ty, CType::Pointer(_));

                            if let Initializer::Expr(expr) = &sub_item.init {
                                if is_ptr_field {
                                    if let Some(addr_init) = self.resolve_ptr_field_init(expr) {
                                        ptr_ranges.push((field_offset, addr_init));
                                    } else if let Some(val) = self.eval_const_expr(expr) {
                                        self.write_const_to_bytes(&mut bytes, field_offset, &val, field_ir_ty);
                                    }
                                } else {
                                    if let Some(val) = self.eval_const_expr(expr) {
                                        self.write_const_to_bytes(&mut bytes, field_offset, &val, field_ir_ty);
                                    }
                                }
                            }
                            current_field_idx = field_idx + 1;
                        }
                    }
                    Initializer::Expr(expr) => {
                        // Single expression for first field
                        if !layout.fields.is_empty() {
                            let field = &layout.fields[0];
                            let field_offset = base_offset + field.offset;
                            let field_ir_ty = IrType::from_ctype(&field.ty);
                            let is_ptr_field = matches!(field.ty, CType::Pointer(_));
                            if is_ptr_field {
                                if let Some(addr_init) = self.resolve_ptr_field_init(expr) {
                                    ptr_ranges.push((field_offset, addr_init));
                                }
                            } else if let Some(val) = self.eval_const_expr(expr) {
                                self.write_const_to_bytes(&mut bytes, field_offset, &val, field_ir_ty);
                            }
                        }
                    }
                }
            }
        }

        // Sort ptr_ranges by offset
        ptr_ranges.sort_by_key(|&(off, _)| off);

        // Emit the byte stream, replacing pointer-sized regions with GlobalAddr elements
        let mut pos = 0;
        for (ptr_off, ref addr_init) in &ptr_ranges {
            // Emit bytes before this pointer
            while pos < *ptr_off {
                compound_elements.push(GlobalInit::Scalar(IrConst::I8(bytes[pos] as i8)));
                pos += 1;
            }
            // Emit the pointer reference
            compound_elements.push(addr_init.clone());
            pos += 8; // pointer is 8 bytes
        }
        // Emit remaining bytes
        while pos < total_size {
            compound_elements.push(GlobalInit::Scalar(IrConst::I8(bytes[pos] as i8)));
            pos += 1;
        }

        GlobalInit::Compound(compound_elements)
    }

    /// Collect all enum constants from the entire translation unit.
    fn collect_all_enum_constants(&mut self, tu: &TranslationUnit) {
        for decl in &tu.decls {
            match decl {
                ExternalDecl::Declaration(d) => {
                    self.collect_enum_constants(&d.type_spec);
                }
                ExternalDecl::FunctionDef(func) => {
                    self.collect_enum_constants(&func.return_type);
                    self.collect_enum_constants_from_compound(&func.body);
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
            TypeSpecifier::Struct(_, Some(fields)) | TypeSpecifier::Union(_, Some(fields)) => {
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

    /// Recursively collect enum constants from a compound statement.
    fn collect_enum_constants_from_compound(&mut self, compound: &CompoundStmt) {
        for item in &compound.items {
            match item {
                BlockItem::Declaration(decl) => {
                    self.collect_enum_constants(&decl.type_spec);
                }
                BlockItem::Statement(stmt) => {
                    self.collect_enum_constants_from_stmt(stmt);
                }
            }
        }
    }

    /// Recursively collect enum constants from a statement.
    fn collect_enum_constants_from_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Compound(compound) => self.collect_enum_constants_from_compound(compound),
            Stmt::If(_, then_stmt, else_stmt, _) => {
                self.collect_enum_constants_from_stmt(then_stmt);
                if let Some(else_s) = else_stmt {
                    self.collect_enum_constants_from_stmt(else_s);
                }
            }
            Stmt::While(_, body, _) | Stmt::DoWhile(body, _, _) => {
                self.collect_enum_constants_from_stmt(body);
            }
            Stmt::For(_, _, _, body, _) => {
                self.collect_enum_constants_from_stmt(body);
            }
            Stmt::Switch(_, body, _) => {
                self.collect_enum_constants_from_stmt(body);
            }
            Stmt::Case(_, stmt, _) | Stmt::Default(stmt, _) | Stmt::Label(_, stmt, _) => {
                self.collect_enum_constants_from_stmt(stmt);
            }
            _ => {}
        }
    }


    /// For a pointer-to-struct parameter type (e.g., `struct TAG *p`), get the
    /// pointed-to struct's layout. This enables `p->field` access.
    pub(super) fn get_struct_layout_for_pointer_param(&self, type_spec: &TypeSpecifier) -> Option<StructLayout> {
        match type_spec {
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
                    if let Some(&ret_ty) = self.function_return_types.get(name.as_str()) {
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

    /// Flatten a multi-dimensional array initializer list for global arrays.
    /// Handles C initialization rules:
    /// - Braced sub-lists map to the next sub-array dimension, padded with zeros
    /// - Bare scalar expressions fill base elements left-to-right without sub-array padding
    fn flatten_global_array_init(
        &self,
        items: &[InitializerItem],
        array_dim_strides: &[usize],
        base_ty: IrType,
        values: &mut Vec<IrConst>,
    ) {
        let base_type_size = base_ty.size().max(1);
        if array_dim_strides.len() <= 1 {
            for item in items {
                self.flatten_global_init_item(&item.init, base_ty, values);
            }
            return;
        }
        // Number of base elements per sub-array at this dimension level
        let sub_elem_count = if array_dim_strides[0] > 0 && base_type_size > 0 {
            array_dim_strides[0] / base_type_size
        } else {
            1
        };
        for item in items {
            match &item.init {
                Initializer::List(sub_items) => {
                    // Braced sub-list: recurse into next dimension, then pad to sub_elem_count
                    let start_len = values.len();
                    self.flatten_global_array_init(sub_items, &array_dim_strides[1..], base_ty, values);
                    while values.len() < start_len + sub_elem_count {
                        values.push(self.zero_const(base_ty));
                    }
                }
                Initializer::Expr(expr) => {
                    if let Expr::StringLiteral(s, _) = expr {
                        // String literal initializing a char sub-array: inline the bytes
                        let start_len = values.len();
                        for &byte in s.as_bytes() {
                            values.push(IrConst::I64(byte as i64));
                        }
                        // Add null terminator if room
                        if values.len() < start_len + sub_elem_count {
                            values.push(IrConst::I64(0));
                        }
                        // Pad to sub_elem_count
                        while values.len() < start_len + sub_elem_count {
                            values.push(self.zero_const(base_ty));
                        }
                        // Truncate if string was too long for the sub-array
                        if values.len() > start_len + sub_elem_count {
                            values.truncate(start_len + sub_elem_count);
                        }
                    } else if let Some(val) = self.eval_const_expr(expr) {
                        // Bare scalar: fills one base element, no sub-array padding
                        values.push(self.coerce_const_to_type(val, base_ty));
                    } else {
                        values.push(self.zero_const(base_ty));
                    }
                }
            }
        }
    }

    /// Flatten a single initializer item, recursing into nested lists.
    fn flatten_global_init_item(&self, init: &Initializer, base_ty: IrType, values: &mut Vec<IrConst>) {
        match init {
            Initializer::Expr(expr) => {
                if let Some(val) = self.eval_const_expr(expr) {
                    values.push(self.coerce_const_to_type(val, base_ty));
                } else {
                    values.push(self.zero_const(base_ty));
                }
            }
            Initializer::List(items) => {
                for item in items {
                    self.flatten_global_init_item(&item.init, base_ty, values);
                }
            }
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

    /// Zero-initialize an alloca by emitting stores of zero.
    pub(super) fn zero_init_alloca(&mut self, alloca: Value, total_size: usize) {
        let mut offset = 0usize;
        while offset + 8 <= total_size {
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
        while offset < total_size {
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
}

impl Default for Lowerer {
    fn default() -> Self {
        Self::new()
    }
}
