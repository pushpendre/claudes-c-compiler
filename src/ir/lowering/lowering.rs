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
    /// Struct/union layouts indexed by tag name (or anonymous id).
    pub(super) struct_layouts: HashMap<String, StructLayout>,
    /// Enum constant values collected from enum definitions.
    pub(super) enum_constants: HashMap<String, i64>,
    /// User-defined goto labels mapped to unique IR labels (scoped per function).
    pub(super) user_labels: HashMap<String, String>,
    /// Typedef mappings (name -> underlying TypeSpecifier).
    pub(super) typedefs: HashMap<String, TypeSpecifier>,
    /// Function name -> return type mapping for inserting narrowing casts after calls.
    pub(super) function_return_types: HashMap<String, IrType>,
    /// Function name -> parameter types mapping for inserting implicit argument casts.
    pub(super) function_param_types: HashMap<String, Vec<IrType>>,
    /// Function name -> is_variadic flag for calling convention handling.
    pub(super) function_variadic: HashSet<String>,
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
            struct_layouts: HashMap::new(),
            enum_constants: HashMap::new(),
            user_labels: HashMap::new(),
            typedefs: HashMap::new(),
            function_return_types: HashMap::new(),
            function_param_types: HashMap::new(),
            function_variadic: HashSet::new(),
        }
    }

    pub fn lower(mut self, tu: &TranslationUnit) -> IrModule {
        // Seed builtin typedefs (matching the parser's pre-seeded typedef names)
        self.seed_builtin_typedefs();

        // First pass: collect all function names, return types, and parameter types
        // so we can distinguish function references from global variable references,
        // insert proper narrowing casts after calls, and insert implicit argument casts.
        for decl in &tu.decls {
            if let ExternalDecl::FunctionDef(func) = decl {
                self.known_functions.insert(func.name.clone());
                let ret_ty = self.type_spec_to_ir(&func.return_type);
                self.function_return_types.insert(func.name.clone(), ret_ty);
                // Collect parameter types (skip variadic portion)
                let param_tys: Vec<IrType> = func.params.iter().map(|p| {
                    self.type_spec_to_ir(&p.type_spec)
                }).collect();
                if !func.variadic || !param_tys.is_empty() {
                    self.function_param_types.insert(func.name.clone(), param_tys);
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
                        let param_tys: Vec<IrType> = params.iter().map(|p| {
                            self.type_spec_to_ir(&p.type_spec)
                        }).collect();
                        if !variadic || !param_tys.is_empty() {
                            self.function_param_types.insert(declarator.name.clone(), param_tys);
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
        self.break_labels.clear();
        self.continue_labels.clear();
        self.user_labels.clear();
        self.current_function_name = func.name.clone();

        let return_type = self.type_spec_to_ir(&func.return_type);
        self.current_return_type = return_type;
        self.current_return_is_bool = matches!(self.resolve_type_spec(&func.return_type), TypeSpecifier::Bool);
        let params: Vec<IrParam> = func.params.iter().map(|p| IrParam {
            name: p.name.clone().unwrap_or_default(),
            ty: self.type_spec_to_ir(&p.type_spec),
        }).collect();

        // Start entry block
        self.start_block("entry".to_string());

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

        for (i, param) in params.iter().enumerate() {
            if !param.name.is_empty() {
                let is_struct_param = if let Some(orig_param) = func.params.get(i) {
                    let resolved = self.resolve_type_spec(&orig_param.type_spec);
                    matches!(resolved, TypeSpecifier::Struct(_, _) | TypeSpecifier::Union(_, _))
                } else {
                    false
                };

                // Emit the alloca that receives the argument value from the register
                let alloca = self.fresh_value();
                let ty = param.ty;
                self.emit(Instruction::Alloca {
                    dest: alloca,
                    ty,
                    size: ty.size(),
                });

                if is_struct_param {
                    // Record that we need to create a struct copy for this param
                    let layout = func.params.get(i)
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
                        func.params.get(i).map_or(0, |p| self.pointee_elem_size(&p.type_spec))
                    } else { 0 };

                    let pointee_type = if ty == IrType::Ptr {
                        func.params.get(i).and_then(|p| self.pointee_ir_type(&p.type_spec))
                    } else { None };

                    let struct_layout = if ty == IrType::Ptr {
                        func.params.get(i).and_then(|p| self.get_struct_layout_for_pointer_param(&p.type_spec))
                    } else { None };

                    let c_type = func.params.get(i).map(|p| self.type_spec_to_ctype(&p.type_spec));
                    let is_bool = func.params.get(i).map_or(false, |p| {
                        matches!(self.resolve_type_spec(&p.type_spec), TypeSpecifier::Bool)
                    });
                    self.locals.insert(param.name.clone(), LocalInfo {
                        alloca,
                        elem_size,
                        is_array: false,
                        ty,
                        pointee_type,
                        struct_layout,
                        is_struct: false,
                        alloc_size: ty.size(),
                        array_dim_strides: vec![],
                        c_type,
                        is_bool,
                    });
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
                    let var_ty = if is_pointer { IrType::Ptr } else { base_ty };
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
            let var_ty = if is_pointer { IrType::Ptr } else { base_ty };

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
                            let actual_count = self.compute_init_list_array_size(items);
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

            // Determine alignment based on type
            let align = match var_ty {
                IrType::I8 | IrType::U8 => 1,
                IrType::I16 | IrType::U16 => 2,
                IrType::I32 | IrType::U32 => 4,
                IrType::I64 | IrType::U64 | IrType::Ptr => 8,
                IrType::F32 => 4,
                IrType::F64 => 8,
                IrType::Void => 1,
            };

            let is_static = decl.is_static;

            // For struct initializers emitted as byte arrays, set element type to I8
            // so the backend emits .byte directives for each element
            let global_ty = if is_struct && matches!(init, GlobalInit::Array(_)) {
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
        match init {
            Initializer::Expr(expr) => {
                // Try to evaluate as a constant
                if let Some(val) = self.eval_const_expr(expr) {
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
                // Try to evaluate as a global address expression (e.g., &x, func, arr, &arr[3], &s.field)
                if let Some(addr_init) = self.eval_global_addr_expr(expr) {
                    return addr_init;
                }
                // Can't evaluate - zero init as fallback
                GlobalInit::Zero
            }
            Initializer::List(items) => {
                if is_array && elem_size > 0 {
                    let num_elems = total_size / elem_size;

                    // Check if any element is an address expression (for pointer arrays)
                    let has_addr_exprs = base_ty == IrType::Ptr && items.iter().any(|item| {
                        if let Initializer::Expr(expr) = &item.init {
                            self.eval_const_expr(expr).is_none() && self.eval_global_addr_expr(expr).is_some()
                        } else {
                            false
                        }
                    });

                    if has_addr_exprs {
                        // Use Compound initializer for arrays containing address expressions
                        let mut elements = Vec::with_capacity(num_elems);
                        for item in items {
                            if let Initializer::Expr(expr) = &item.init {
                                if let Some(val) = self.eval_const_expr(expr) {
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
                        let mut flat = Vec::with_capacity(num_elems);
                        self.flatten_global_array_init(items, array_dim_strides, base_ty, &mut flat);
                        for (i, v) in flat.into_iter().enumerate() {
                            if i < num_elems {
                                values[i] = v;
                            }
                        }
                    } else {
                        // Support designated initializers: [idx] = val
                        let mut current_idx = 0usize;
                        for item in items {
                            // Check for index designator
                            if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                                if let Some(idx) = self.eval_const_expr(idx_expr) {
                                    current_idx = match idx {
                                        IrConst::I8(v) => v as usize,
                                        IrConst::I16(v) => v as usize,
                                        IrConst::I32(v) => v as usize,
                                        IrConst::I64(v) => v as usize,
                                        _ => current_idx,
                                    };
                                }
                            }
                            if current_idx < num_elems {
                                let val = match &item.init {
                                    Initializer::Expr(expr) => {
                                        self.eval_const_expr(expr).unwrap_or(self.zero_const(base_ty))
                                    }
                                    Initializer::List(sub_items) => {
                                        // Nested list - flatten
                                        let mut sub_vals = Vec::new();
                                        for sub in sub_items {
                                            self.flatten_global_init_item(&sub.init, base_ty, &mut sub_vals);
                                        }
                                        sub_vals.into_iter().next().unwrap_or(self.zero_const(base_ty))
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

    /// Lower a struct initializer list to a GlobalInit::Array of field values.
    /// Emits each field's value at its appropriate position, with padding bytes as zeros.
    /// Supports designated initializers like {.b = 2, .a = 1}.
    fn lower_struct_global_init(
        &self,
        items: &[InitializerItem],
        layout: &StructLayout,
    ) -> GlobalInit {
        let total_size = layout.size;
        let mut bytes = vec![0u8; total_size];

        // Track current field index for positional (non-designated) initializers
        let mut current_field_idx = 0usize;

        for item in items {
            // Determine which field this initializer targets
            let field_idx = if let Some(Designator::Field(ref name)) = item.designators.first() {
                // Designated initializer: find field by name
                layout.fields.iter().position(|f| f.name == *name).unwrap_or(current_field_idx)
            } else {
                // Positional: skip unnamed fields (anonymous bitfields)
                let mut idx = current_field_idx;
                while idx < layout.fields.len() && layout.fields[idx].name.is_empty() {
                    idx += 1;
                }
                idx
            };

            if field_idx >= layout.fields.len() {
                break;
            }

            let field_layout = &layout.fields[field_idx];

            // Evaluate the initializer expression
            let val = match &item.init {
                Initializer::Expr(expr) => {
                    self.eval_const_expr(expr).unwrap_or(IrConst::I64(0))
                }
                Initializer::List(sub_items) => {
                    // Nested struct initializer or nested list
                    // For now, try to evaluate first item as scalar
                    if let Some(first) = sub_items.first() {
                        if let Initializer::Expr(expr) = &first.init {
                            self.eval_const_expr(expr).unwrap_or(IrConst::I64(0))
                        } else {
                            IrConst::I64(0)
                        }
                    } else {
                        IrConst::I64(0)
                    }
                }
            };

            // Convert value to bytes and place at field offset
            let field_offset = field_layout.offset;
            let field_ir_ty = IrType::from_ctype(&field_layout.ty);
            self.write_const_to_bytes(&mut bytes, field_offset, &val, field_ir_ty);

            // Advance positional counter past this field
            current_field_idx = field_idx + 1;
        }

        // Emit as array of I8 (byte) constants
        let values: Vec<IrConst> = bytes.iter().map(|&b| IrConst::I8(b as i8)).collect();
        GlobalInit::Array(values)
    }

    /// Write an IrConst value to a byte buffer at the given offset using the field's IR type.
    fn write_const_to_bytes(&self, bytes: &mut [u8], offset: usize, val: &IrConst, ty: IrType) {
        let int_val = match val {
            IrConst::I8(v) => *v as i64,
            IrConst::I16(v) => *v as i64,
            IrConst::I32(v) => *v as i64,
            IrConst::I64(v) => *v,
            IrConst::Zero => 0,
            IrConst::F32(v) => {
                // Write float as 4 bytes
                let bits = v.to_bits().to_le_bytes();
                for (i, &b) in bits.iter().enumerate() {
                    if offset + i < bytes.len() {
                        bytes[offset + i] = b;
                    }
                }
                return;
            }
            IrConst::F64(v) => {
                // Write double as 8 bytes
                let bits = v.to_bits().to_le_bytes();
                for (i, &b) in bits.iter().enumerate() {
                    if offset + i < bytes.len() {
                        bytes[offset + i] = b;
                    }
                }
                return;
            }
        };

        // Write integer value in little-endian at the appropriate size
        let le_bytes = int_val.to_le_bytes();
        let size = ty.size();
        for i in 0..size {
            if offset + i < bytes.len() {
                bytes[offset + i] = le_bytes[i];
            }
        }
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
        if let TypeSpecifier::Enum(_, Some(variants)) = ts {
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
    pub(super) fn pointee_ir_type(&self, type_spec: &TypeSpecifier) -> Option<IrType> {
        match type_spec {
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

    /// Get the element size for pointer arithmetic on a variable.
    /// For `int *p`, returns 4. For `char *s`, returns 1.
    pub(super) fn get_pointer_elem_size(&self, expr: &Expr) -> usize {
        if let Expr::Identifier(name, _) = expr {
            if let Some(info) = self.locals.get(name) {
                if info.elem_size > 0 {
                    return info.elem_size;
                }
                if let Some(pt) = info.pointee_type {
                    return pt.size();
                }
            }
            if let Some(ginfo) = self.globals.get(name) {
                if ginfo.elem_size > 0 {
                    return ginfo.elem_size;
                }
                if let Some(pt) = ginfo.pointee_type {
                    return pt.size();
                }
            }
        }
        // Default: treat as pointer to 8-byte values
        8
    }

    /// Get the element size for a pointer expression (for scaling in pointer arithmetic).
    pub(super) fn get_pointer_elem_size_from_expr(&self, expr: &Expr) -> usize {
        match expr {
            Expr::Identifier(_, _) => {
                self.get_pointer_elem_size(expr)
            }
            Expr::PostfixOp(_, inner, _) => self.get_pointer_elem_size_from_expr(inner),
            Expr::UnaryOp(op, inner, _) => {
                match op {
                    UnaryOp::PreInc | UnaryOp::PreDec => self.get_pointer_elem_size_from_expr(inner),
                    _ => 8,
                }
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
    fn flatten_global_array_init(
        &self,
        items: &[InitializerItem],
        array_dim_strides: &[usize],
        base_ty: IrType,
        values: &mut Vec<IrConst>,
    ) {
        if array_dim_strides.len() <= 1 {
            for item in items {
                self.flatten_global_init_item(&item.init, base_ty, values);
            }
            return;
        }
        let sub_elem_count = if array_dim_strides[0] > 0 && array_dim_strides.last().copied().unwrap_or(1) > 0 {
            array_dim_strides[0] / array_dim_strides.last().copied().unwrap_or(1)
        } else {
            1
        };
        for item in items {
            let start_len = values.len();
            match &item.init {
                Initializer::List(sub_items) => {
                    self.flatten_global_array_init(sub_items, &array_dim_strides[1..], base_ty, values);
                }
                Initializer::Expr(expr) => {
                    if let Some(val) = self.eval_const_expr(expr) {
                        values.push(val);
                    } else {
                        values.push(self.zero_const(base_ty));
                    }
                }
            }
            while values.len() < start_len + sub_elem_count {
                values.push(self.zero_const(base_ty));
            }
        }
    }

    /// Flatten a single initializer item, recursing into nested lists.
    fn flatten_global_init_item(&self, init: &Initializer, base_ty: IrType, values: &mut Vec<IrConst>) {
        match init {
            Initializer::Expr(expr) => {
                if let Some(val) = self.eval_const_expr(expr) {
                    values.push(val);
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
