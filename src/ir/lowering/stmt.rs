use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, CType, StructLayout, StructField};
use super::lowering::{Lowerer, LocalInfo, GlobalInfo};

impl Lowerer {
    pub(super) fn lower_compound_stmt(&mut self, compound: &CompoundStmt) {
        // Save current locals for block scope restoration
        let saved_locals = self.locals.clone();

        for item in &compound.items {
            match item {
                BlockItem::Declaration(decl) => {
                    // Collect enum constants from declarations within this block
                    self.collect_enum_constants(&decl.type_spec);
                    self.lower_local_decl(decl);
                }
                BlockItem::Statement(stmt) => self.lower_stmt(stmt),
            }
        }

        // Restore outer scope locals (removes block-scoped declarations)
        self.locals = saved_locals;
    }

    pub(super) fn lower_local_decl(&mut self, decl: &Declaration) {
        // First, register any struct/union definition from the type specifier
        self.register_struct_type(&decl.type_spec);

        // If this is a typedef, register the mapping and skip variable emission
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
            return;
        }

        for declarator in &decl.declarators {
            if declarator.name.is_empty() {
                continue; // Skip anonymous declarations (e.g., bare struct definitions)
            }

            // Handle extern declarations inside function bodies:
            // they reference a global symbol, not a local variable
            if decl.is_extern {
                self.locals.remove(&declarator.name);
                if !self.globals.contains_key(&declarator.name) {
                    let ty = self.type_spec_to_ir(&decl.type_spec);
                    let is_pointer = declarator.derived.iter().any(|d| matches!(d, DerivedDeclarator::Pointer));
                    let var_ty = if is_pointer { IrType::Ptr } else { ty };
                    let pointee_type = self.compute_pointee_type(&decl.type_spec, &declarator.derived);
                    let c_type = Some(self.build_full_ctype(&decl.type_spec, &declarator.derived));
                    self.globals.insert(declarator.name.clone(), GlobalInfo {
                        ty: var_ty,
                        elem_size: 0,
                        is_array: false,
                        pointee_type,
                        struct_layout: None,
                        is_struct: false,
                        array_dim_strides: vec![],
                        c_type,
                    });
                }
                continue;
            }

            // Handle block-scope function declarations: int f(int);
            // These declare an external function, not a local variable.
            // Register in known_functions and skip local variable allocation.
            {
                let mut ptr_count = 0;
                let mut func_info = None;
                for d in &declarator.derived {
                    match d {
                        DerivedDeclarator::Pointer => ptr_count += 1,
                        DerivedDeclarator::Function(p, v) => {
                            func_info = Some((p.clone(), *v));
                            break;
                        }
                        DerivedDeclarator::FunctionPointer(_, _) => break,
                        _ => {}
                    }
                }
                if let Some((params, variadic)) = func_info {
                    // This is a function declaration, not a variable
                    self.known_functions.insert(declarator.name.clone());
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
                    if variadic {
                        self.function_variadic.insert(declarator.name.clone());
                    }
                    // Remove from locals if previously added (e.g., shadowed by this declaration)
                    self.locals.remove(&declarator.name);
                    continue;
                }
            }

            let base_ty = self.type_spec_to_ir(&decl.type_spec);
            let (mut alloc_size, elem_size, is_array, is_pointer, mut array_dim_strides) = self.compute_decl_info(&decl.type_spec, &declarator.derived);
            // _Bool type check: only for direct scalar variables, not pointers or arrays
            let has_derived_ptr = declarator.derived.iter().any(|d| matches!(d, DerivedDeclarator::Pointer));
            let is_bool_type = matches!(self.resolve_type_spec(&decl.type_spec), TypeSpecifier::Bool)
                && !has_derived_ptr && !is_array;
            // For array-of-pointers (int *arr[N]), the element type is Ptr, not the base type.
            // Detect: Pointer before Array in derived declarators.
            let is_array_of_pointers = is_array && {
                let ptr_pos = declarator.derived.iter().position(|d| matches!(d, DerivedDeclarator::Pointer));
                let arr_pos = declarator.derived.iter().position(|d| matches!(d, DerivedDeclarator::Array(_)));
                matches!((ptr_pos, arr_pos), (Some(pp), Some(ap)) if pp < ap)
            };
            let var_ty = if is_pointer || is_array_of_pointers { IrType::Ptr } else { base_ty };

            // For unsized arrays (int a[] = {...}), compute actual size from initializer
            let is_unsized_array = is_array && declarator.derived.iter().any(|d| {
                matches!(d, DerivedDeclarator::Array(None))
            });
            if is_unsized_array {
                if let Some(ref init) = declarator.init {
                    match init {
                        Initializer::Expr(expr) => {
                            // Fix alloc size for unsized char arrays initialized with string literals.
                            // For `char s[] = "hello"`, allocate strlen+1 bytes instead of default 256.
                            if base_ty == IrType::I8 || base_ty == IrType::U8 {
                                if let Expr::StringLiteral(s, _) = expr {
                                    alloc_size = s.as_bytes().len() + 1; // +1 for null terminator
                                }
                            }
                        }
                        Initializer::List(items) => {
                            let actual_count = self.compute_init_list_array_size_for_char_array(items, base_ty);
                            if elem_size > 0 {
                                alloc_size = actual_count * elem_size;
                                // Update strides for 1D unsized array
                                if array_dim_strides.len() == 1 {
                                    array_dim_strides = vec![elem_size];
                                }
                            }
                        }
                    }
                }
            }

            // Determine if this is a struct/union variable or pointer-to-struct.
            // For pointer-to-struct, we still store the layout so p->field works.
            let struct_layout = self.get_struct_layout_for_type(&decl.type_spec)
                .or_else(|| {
                    // For typedef'd pointer-to-struct (e.g., typedef struct Foo *FooPtr),
                    // the resolved type is Pointer(inner). Peel off Pointer to get
                    // the struct layout for -> member access.
                    let resolved = self.resolve_type_spec(&decl.type_spec);
                    if let TypeSpecifier::Pointer(inner) = resolved {
                        self.get_struct_layout_for_type(inner)
                    } else {
                        None
                    }
                });
            let is_struct = struct_layout.is_some() && !is_pointer && !is_array;

            // For struct variables, use the struct's actual size;
            // but for arrays of structs, use the full array allocation size.
            let actual_alloc_size = if let Some(ref layout) = struct_layout {
                if is_array {
                    alloc_size
                } else {
                    layout.size
                }
            } else {
                alloc_size
            };

            // Handle static local variables: emit as globals with mangled names
            if decl.is_static {
                let static_id = self.next_static_local;
                let static_name = format!("{}.{}.{}", self.current_function_name, declarator.name, static_id);

                // Register the bare name -> mangled name mapping before processing the initializer
                // so that &x in another static's initializer can resolve to the mangled name.
                self.static_local_names.insert(declarator.name.clone(), static_name.clone());

                // Determine initializer (evaluated at compile time for static locals)
                let init = if let Some(ref initializer) = declarator.init {
                    self.lower_global_init(initializer, &decl.type_spec, base_ty, is_array, elem_size, actual_alloc_size, &struct_layout, &array_dim_strides)
                } else {
                    GlobalInit::Zero
                };

                let align = match var_ty {
                    IrType::I8 | IrType::U8 => 1,
                    IrType::I16 | IrType::U16 => 2,
                    IrType::I32 | IrType::U32 => 4,
                    IrType::I64 | IrType::U64 | IrType::Ptr => 8,
                    IrType::F32 => 4,
                    IrType::F64 => 8,
                    IrType::Void => 1,
                };

                // Add as a global variable (with static linkage = not exported)
                self.module.globals.push(IrGlobal {
                    name: static_name.clone(),
                    ty: var_ty,
                    size: actual_alloc_size,
                    align,
                    init,
                    is_static: true,
                    is_extern: false,
                });

                // Track as a global for access via GlobalAddr
                let pointee_type = self.compute_pointee_type(&decl.type_spec, &declarator.derived);
                let c_type = Some(self.build_full_ctype(&decl.type_spec, &declarator.derived));
                self.globals.insert(static_name.clone(), GlobalInfo {
                    ty: var_ty,
                    elem_size,
                    is_array,
                    pointee_type,
                    struct_layout: struct_layout.clone(),
                    is_struct,
                    array_dim_strides: array_dim_strides.clone(),
                    c_type: c_type.clone(),
                });

                // Also add an alias so the local name resolves to the global
                // We emit a GlobalAddr to get the address, then treat it like an alloca
                let addr = self.fresh_value();
                self.emit(Instruction::GlobalAddr {
                    dest: addr,
                    name: static_name,
                });

                let pointee_type = self.compute_pointee_type(&decl.type_spec, &declarator.derived);
                let is_bool = matches!(self.resolve_type_spec(&decl.type_spec), TypeSpecifier::Bool) && !is_pointer && !is_array;
                self.locals.insert(declarator.name.clone(), LocalInfo {
                    alloca: addr,
                    elem_size,
                    is_array,
                    ty: var_ty,
                    pointee_type,
                    struct_layout,
                    is_struct,
                    alloc_size: actual_alloc_size,
                    array_dim_strides: array_dim_strides.clone(),
                    c_type,
                    is_bool,
                });

                self.next_static_local += 1;
                // Static locals are initialized once at program start (via .data/.bss),
                // not at every function call, so skip the runtime initialization below
                continue;
            }

            let alloca = self.fresh_value();
            self.emit(Instruction::Alloca {
                dest: alloca,
                ty: if is_array || is_struct { IrType::Ptr } else { var_ty },
                size: actual_alloc_size,
            });
            let pointee_type = self.compute_pointee_type(&decl.type_spec, &declarator.derived);
            let c_type = Some(self.build_full_ctype(&decl.type_spec, &declarator.derived));
            let is_bool = matches!(self.resolve_type_spec(&decl.type_spec), TypeSpecifier::Bool) && !is_pointer && !is_array;
            self.locals.insert(declarator.name.clone(), LocalInfo {
                alloca,
                elem_size,
                is_array,
                ty: var_ty,
                pointee_type,
                struct_layout,
                is_struct,
                alloc_size: actual_alloc_size,
                array_dim_strides: array_dim_strides.clone(),
                c_type,
                is_bool,
            });

            // Track function pointer return and param types for correct calling convention
            for d in &declarator.derived {
                if let DerivedDeclarator::FunctionPointer(params, _) = d {
                    let ret_ty = self.type_spec_to_ir(&decl.type_spec);
                    self.function_ptr_return_types.insert(declarator.name.clone(), ret_ty);
                    let param_tys: Vec<IrType> = params.iter().map(|p| {
                        self.type_spec_to_ir(&p.type_spec)
                    }).collect();
                    self.function_ptr_param_types.insert(declarator.name.clone(), param_tys);
                    break;
                }
            }

            if let Some(ref init) = declarator.init {
                match init {
                    Initializer::Expr(expr) => {
                        if is_array && (base_ty == IrType::I8 || base_ty == IrType::U8) {
                            // Char array from string literal: char s[] = "hello"
                            if let Expr::StringLiteral(s, _) = expr {
                                self.emit_string_to_alloca(alloca, s, 0);
                            } else {
                                // Non-string expression initializer for char array (e.g., pointer assignment)
                                let val = self.lower_expr(expr);
                                self.emit(Instruction::Store { val, ptr: alloca, ty: var_ty });
                            }
                        } else if is_struct {
                            // Struct copy-initialization: struct Point b = a;
                            // For function calls returning small structs (<= 8 bytes),
                            // the return value IS the struct data in rax (not an address).
                            // Store it directly instead of using memcpy.
                            if matches!(expr, Expr::FunctionCall(_, _, _)) && actual_alloc_size <= 8 {
                                let val = self.lower_expr(expr);
                                self.emit(Instruction::Store { val, ptr: alloca, ty: IrType::I64 });
                            } else {
                                let src_addr = self.get_struct_base_addr(expr);
                                self.emit(Instruction::Memcpy {
                                    dest: alloca,
                                    src: src_addr,
                                    size: actual_alloc_size,
                                });
                            }
                        } else {
                            let val = self.lower_expr(expr);
                            // Insert implicit cast for type mismatches
                            // (e.g., float f = 'a', int x = 3.14, char c = 99.0)
                            let expr_ty = self.get_expr_type(expr);
                            let val = self.emit_implicit_cast(val, expr_ty, var_ty);
                            // _Bool variables clamp any value to 0 or 1
                            let val = if is_bool { self.emit_bool_normalize(val) } else { val };
                            self.emit(Instruction::Store { val, ptr: alloca, ty: var_ty });
                        }
                    }
                    Initializer::List(items) => {
                        if is_struct {
                            // Initialize struct fields from initializer list
                            // Supports designated initializers and nested struct init
                            if let Some(layout) = self.locals.get(&declarator.name).and_then(|l| l.struct_layout.clone()) {
                                // Zero-initialize first if any designators or partial init
                                let has_designators = items.iter().any(|item| !item.designators.is_empty());
                                if has_designators || items.len() < layout.fields.len() {
                                    self.zero_init_alloca(alloca, layout.size);
                                }
                                self.emit_struct_init(items, alloca, &layout, 0);
                            }
                        } else if is_array && elem_size > 0 {
                            // Check if this is an array of structs
                            let elem_struct_layout = self.locals.get(&declarator.name)
                                .and_then(|l| l.struct_layout.clone());
                            if array_dim_strides.len() > 1 {
                                // Multi-dimensional array init: zero first, then fill
                                self.zero_init_alloca(alloca, alloc_size);
                                self.lower_array_init_list(items, alloca, base_ty, &array_dim_strides);
                            } else if let Some(ref s_layout) = elem_struct_layout {
                                // Array of structs: init each element using struct layout
                                self.zero_init_alloca(alloca, alloc_size);
                                let mut current_idx = 0usize;
                                for item in items.iter() {
                                    if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                                        if let Some(idx_val) = self.eval_const_expr_for_designator(idx_expr) {
                                            current_idx = idx_val;
                                        }
                                    }
                                    let base_byte_offset = current_idx * elem_size;
                                    let elem_base = self.fresh_value();
                                    self.emit(Instruction::GetElementPtr {
                                        dest: elem_base,
                                        base: alloca,
                                        offset: Operand::Const(IrConst::I64(base_byte_offset as i64)),
                                        ty: IrType::I8,
                                    });
                                    match &item.init {
                                        Initializer::List(sub_items) => {
                                            // Initialize struct fields from sub-list
                                            self.lower_local_struct_init(sub_items, elem_base, s_layout);
                                        }
                                        Initializer::Expr(e) => {
                                            // Single expr: assign to first field
                                            if !s_layout.fields.is_empty() {
                                                let field = &s_layout.fields[0];
                                                let field_ty = IrType::from_ctype(&field.ty);
                                                let val = self.lower_expr(e);
                                                let field_addr = self.fresh_value();
                                                self.emit(Instruction::GetElementPtr {
                                                    dest: field_addr,
                                                    base: elem_base,
                                                    offset: Operand::Const(IrConst::I64(field.offset as i64)),
                                                    ty: field_ty,
                                                });
                                                self.emit(Instruction::Store { val, ptr: field_addr, ty: field_ty });
                                            }
                                        }
                                    }
                                    current_idx += 1;
                                }
                            } else {
                                // 1D array: supports designated initializers [idx] = val
                                // Also zero-fill first if partially initialized.
                                let num_elems = alloc_size / elem_size.max(1);
                                let has_designators = items.iter().any(|item| !item.designators.is_empty());
                                if has_designators || items.len() < num_elems {
                                    // Partial initialization or designators: zero the entire array first
                                    self.zero_init_alloca(alloca, alloc_size);
                                }

                                // For arrays of pointers, the element type is Ptr (8 bytes),
                                // not the base type spec (which would be e.g. I32 for int *arr[N]).
                                let elem_store_ty = if is_array_of_pointers { IrType::I64 } else { base_ty };

                                let mut current_idx = 0usize;
                                for item in items.iter() {
                                    // Check for index designator
                                    if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                                        if let Some(idx_val) = self.eval_const_expr_for_designator(idx_expr) {
                                            current_idx = idx_val;
                                        }
                                    }

                                    // Handle each item, with special case for string literals in char arrays
                                    if let Initializer::Expr(e) = &item.init {
                                        if base_ty == IrType::I8 || base_ty == IrType::U8 {
                                            if let Expr::StringLiteral(s, _) = e {
                                                self.emit_string_to_alloca(alloca, s, current_idx * elem_size);
                                                current_idx += 1;
                                                continue;
                                            }
                                        }
                                        let val = self.lower_expr(e);
                                        let expr_ty = self.get_expr_type(e);
                                        let val = self.emit_implicit_cast(val, expr_ty, elem_store_ty);
                                        self.emit_array_element_store(alloca, val, current_idx * elem_size, elem_store_ty);
                                    }
                                    current_idx += 1;
                                }
                            }
                        } else {
                            // Scalar with braces: int x = { 1 };
                            // C11 6.7.9: A scalar can be initialized with a braced expression.
                            if let Some(first) = items.first() {
                                if let Initializer::Expr(expr) = &first.init {
                                    let val = self.lower_expr(expr);
                                    let expr_ty = self.get_expr_type(expr);
                                    let val = self.emit_implicit_cast(val, expr_ty, var_ty);
                                    let val = if is_bool { self.emit_bool_normalize(val) } else { val };
                                    self.emit(Instruction::Store { val, ptr: alloca, ty: var_ty });
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Initialize a local struct from an initializer list.
    /// `base` is the base address of the struct in memory.
    fn lower_local_struct_init(
        &mut self,
        items: &[InitializerItem],
        base: Value,
        layout: &StructLayout,
    ) {
        let mut current_field_idx = 0usize;
        for item in items {
            let field_idx = if let Some(Designator::Field(ref name)) = item.designators.first() {
                layout.fields.iter().position(|f| f.name == *name).unwrap_or(current_field_idx)
            } else {
                let mut idx = current_field_idx;
                while idx < layout.fields.len() && layout.fields[idx].name.is_empty() {
                    idx += 1;
                }
                idx
            };

            if field_idx >= layout.fields.len() { break; }
            let field = &layout.fields[field_idx];
            let field_offset = field.offset;
            let field_ty = IrType::from_ctype(&field.ty);

            match &item.init {
                Initializer::Expr(e) => {
                    let val = self.lower_expr(e);
                    let field_addr = self.fresh_value();
                    self.emit(Instruction::GetElementPtr {
                        dest: field_addr,
                        base,
                        offset: Operand::Const(IrConst::I64(field_offset as i64)),
                        ty: field_ty,
                    });
                    self.emit(Instruction::Store { val, ptr: field_addr, ty: field_ty });
                }
                Initializer::List(sub_items) => {
                    let field_addr = self.fresh_value();
                    self.emit(Instruction::GetElementPtr {
                        dest: field_addr,
                        base,
                        offset: Operand::Const(IrConst::I64(field_offset as i64)),
                        ty: field_ty,
                    });
                    self.lower_struct_field_init_list(sub_items, field_addr, &field.ty);
                }
            }
            current_field_idx = field_idx + 1;
        }
    }

    /// Initialize a struct field from a nested initializer list.
    /// Handles sub-struct fields and array fields.
    fn lower_struct_field_init_list(
        &mut self,
        items: &[InitializerItem],
        base: Value,
        field_ctype: &CType,
    ) {
        match field_ctype {
            CType::Struct(st) => {
                let sub_layout = StructLayout::for_struct(&st.fields);
                self.lower_local_struct_init(items, base, &sub_layout);
            }
            CType::Union(st) => {
                let sub_layout = StructLayout::for_union(&st.fields);
                self.lower_local_struct_init(items, base, &sub_layout);
            }
            CType::Array(ref elem_ty, _) => {
                // Array field: init elements sequentially
                let elem_ir_ty = IrType::from_ctype(elem_ty);
                let elem_size = elem_ty.size();
                for (i, item) in items.iter().enumerate() {
                    if let Initializer::Expr(e) = &item.init {
                        let val = self.lower_expr(e);
                        let elem_addr = self.fresh_value();
                        self.emit(Instruction::GetElementPtr {
                            dest: elem_addr,
                            base,
                            offset: Operand::Const(IrConst::I64((i * elem_size) as i64)),
                            ty: elem_ir_ty,
                        });
                        self.emit(Instruction::Store { val, ptr: elem_addr, ty: elem_ir_ty });
                    }
                }
            }
            _ => {
                // Scalar field with nested braces (e.g., int x = {5})
                if let Some(first) = items.first() {
                    if let Initializer::Expr(e) = &first.init {
                        let val = self.lower_expr(e);
                        let field_ir_ty = IrType::from_ctype(field_ctype);
                        self.emit(Instruction::Store { val, ptr: base, ty: field_ir_ty });
                    }
                }
            }
        }
    }

    /// Lower a multi-dimensional array initializer list.
    /// Handles nested braces like `{{1,2,3},{4,5,6}}` for `int a[2][3]`.
    pub(super) fn lower_array_init_list(
        &mut self,
        items: &[InitializerItem],
        alloca: Value,
        base_ty: IrType,
        array_dim_strides: &[usize],
    ) {
        let mut flat_index = 0usize;
        self.lower_array_init_recursive(items, alloca, base_ty, array_dim_strides, &mut flat_index);
    }

    /// Recursive helper for multi-dimensional array initialization.
    /// Processes each initializer item, recursing for nested braces and
    /// advancing the flat_index to track the current element position.
    fn lower_array_init_recursive(
        &mut self,
        items: &[InitializerItem],
        alloca: Value,
        base_ty: IrType,
        array_dim_strides: &[usize],
        flat_index: &mut usize,
    ) {
        let elem_size = *array_dim_strides.last().unwrap_or(&1);
        let sub_elem_count = if array_dim_strides.len() > 1 && elem_size > 0 {
            array_dim_strides[0] / elem_size
        } else {
            1
        };

        for item in items {
            let start_index = *flat_index;
            match &item.init {
                Initializer::List(sub_items) => {
                    if array_dim_strides.len() > 1 {
                        self.lower_array_init_recursive(sub_items, alloca, base_ty, &array_dim_strides[1..], flat_index);
                    } else {
                        // Bottom level: treat as flat elements
                        for sub_item in sub_items {
                            if let Initializer::Expr(e) = &sub_item.init {
                                let val = self.lower_and_cast_init_expr(e, base_ty);
                                self.emit_array_element_store(alloca, val, *flat_index * elem_size, base_ty);
                                *flat_index += 1;
                            }
                        }
                    }
                }
                Initializer::Expr(e) => {
                    // String literal fills a sub-array in char arrays
                    if base_ty == IrType::I8 || base_ty == IrType::U8 {
                        if let Expr::StringLiteral(s, _) = e {
                            self.emit_string_to_alloca(alloca, s, *flat_index * elem_size);
                            *flat_index += sub_elem_count;
                            // Skip boundary adjustment since we already advanced by sub_elem_count
                            continue;
                        }
                    }
                    let val = self.lower_and_cast_init_expr(e, base_ty);
                    self.emit_array_element_store(alloca, val, *flat_index * elem_size, base_ty);
                    *flat_index += 1;
                }
            }
            // Advance to next sub-array boundary if multi-dim
            if array_dim_strides.len() > 1 {
                let boundary = start_index + sub_elem_count;
                if *flat_index < boundary {
                    *flat_index = boundary;
                }
            }
        }
    }

    /// Lower an expression and cast it to the target array element type.
    fn lower_and_cast_init_expr(&mut self, expr: &Expr, target_ty: IrType) -> Operand {
        let val = self.lower_expr(expr);
        let expr_ty = self.get_expr_type(expr);
        self.emit_implicit_cast(val, expr_ty, target_ty)
    }

    pub(super) fn lower_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Return(expr, _span) => {
                let op = expr.as_ref().map(|e| {
                    // For small struct returns (<= 8 bytes), load the struct data
                    // from its address so it goes into rax as data (not as a pointer).
                    if self.expr_is_struct_value(e) {
                        let struct_size = self.get_struct_size_for_expr(e);
                        if struct_size <= 8 {
                            let addr = self.get_struct_base_addr(e);
                            let dest = self.fresh_value();
                            self.emit(Instruction::Load { dest, ptr: addr, ty: IrType::I64 });
                            return Operand::Value(dest);
                        }
                    }
                    let val = self.lower_expr(e);
                    let ret_ty = self.current_return_type;
                    let expr_ty = self.get_expr_type(e);
                    // Insert implicit cast for return value type mismatch
                    // Handles: float<->int, float<->float, and integer narrowing
                    let val = self.emit_implicit_cast(val, expr_ty, ret_ty);
                    // _Bool functions clamp return value to 0 or 1
                    if self.current_return_is_bool { self.emit_bool_normalize(val) } else { val }
                });
                self.terminate(Terminator::Return(op));
                // Start a new unreachable block for any code after return
                let label = self.fresh_label("post_ret");
                self.start_block(label);
            }
            Stmt::Expr(Some(expr)) => {
                self.lower_expr(expr);
            }
            Stmt::Expr(None) => {}
            Stmt::Compound(compound) => {
                self.lower_compound_stmt(compound);
            }
            Stmt::If(cond, then_stmt, else_stmt, _span) => {
                let cond_val = self.lower_expr(cond);
                let then_label = self.fresh_label("then");
                let else_label = self.fresh_label("else");
                let end_label = self.fresh_label("endif");

                if else_stmt.is_some() {
                    self.terminate(Terminator::CondBranch {
                        cond: cond_val,
                        true_label: then_label.clone(),
                        false_label: else_label.clone(),
                    });
                } else {
                    self.terminate(Terminator::CondBranch {
                        cond: cond_val,
                        true_label: then_label.clone(),
                        false_label: end_label.clone(),
                    });
                }

                // Then block
                self.start_block(then_label);
                self.lower_stmt(then_stmt);
                self.terminate(Terminator::Branch(end_label.clone()));

                // Else block
                if let Some(else_stmt) = else_stmt {
                    self.start_block(else_label);
                    self.lower_stmt(else_stmt);
                    self.terminate(Terminator::Branch(end_label.clone()));
                }

                self.start_block(end_label);
            }
            Stmt::While(cond, body, _span) => {
                let cond_label = self.fresh_label("while_cond");
                let body_label = self.fresh_label("while_body");
                let end_label = self.fresh_label("while_end");

                self.break_labels.push(end_label.clone());
                self.continue_labels.push(cond_label.clone());

                self.terminate(Terminator::Branch(cond_label.clone()));

                self.start_block(cond_label);
                let cond_val = self.lower_expr(cond);
                self.terminate(Terminator::CondBranch {
                    cond: cond_val,
                    true_label: body_label.clone(),
                    false_label: end_label.clone(),
                });

                self.start_block(body_label);
                self.lower_stmt(body);
                self.terminate(Terminator::Branch(self.continue_labels.last().unwrap().clone()));

                self.break_labels.pop();
                self.continue_labels.pop();

                self.start_block(end_label);
            }
            Stmt::For(init, cond, inc, body, _span) => {
                // Save locals for for-init scope (C99: for-init decl has its own scope)
                let saved_locals = self.locals.clone();

                // Init
                if let Some(init) = init {
                    match init.as_ref() {
                        ForInit::Declaration(decl) => self.lower_local_decl(decl),
                        ForInit::Expr(expr) => { self.lower_expr(expr); },
                    }
                }

                let cond_label = self.fresh_label("for_cond");
                let body_label = self.fresh_label("for_body");
                let inc_label = self.fresh_label("for_inc");
                let end_label = self.fresh_label("for_end");

                self.break_labels.push(end_label.clone());
                self.continue_labels.push(inc_label.clone());

                let cond_label_ref = cond_label.clone();
                self.terminate(Terminator::Branch(cond_label.clone()));

                // Condition
                self.start_block(cond_label);
                if let Some(cond) = cond {
                    let cond_val = self.lower_expr(cond);
                    self.terminate(Terminator::CondBranch {
                        cond: cond_val,
                        true_label: body_label.clone(),
                        false_label: end_label.clone(),
                    });
                } else {
                    self.terminate(Terminator::Branch(body_label.clone()));
                }

                // Body
                self.start_block(body_label);
                self.lower_stmt(body);
                self.terminate(Terminator::Branch(inc_label.clone()));

                // Increment
                self.start_block(inc_label);
                if let Some(inc) = inc {
                    self.lower_expr(inc);
                }
                self.terminate(Terminator::Branch(cond_label_ref));

                self.break_labels.pop();
                self.continue_labels.pop();

                self.start_block(end_label);

                // Restore locals to exit for-init scope
                self.locals = saved_locals;
            }
            Stmt::DoWhile(body, cond, _span) => {
                let body_label = self.fresh_label("do_body");
                let cond_label = self.fresh_label("do_cond");
                let end_label = self.fresh_label("do_end");

                self.break_labels.push(end_label.clone());
                self.continue_labels.push(cond_label.clone());

                self.terminate(Terminator::Branch(body_label.clone()));

                self.start_block(body_label.clone());
                self.lower_stmt(body);
                self.terminate(Terminator::Branch(cond_label.clone()));

                self.start_block(cond_label);
                let cond_val = self.lower_expr(cond);
                self.terminate(Terminator::CondBranch {
                    cond: cond_val,
                    true_label: body_label,
                    false_label: end_label.clone(),
                });

                self.break_labels.pop();
                self.continue_labels.pop();

                self.start_block(end_label);
            }
            Stmt::Break(_span) => {
                if let Some(label) = self.break_labels.last().cloned() {
                    self.terminate(Terminator::Branch(label));
                    let dead = self.fresh_label("post_break");
                    self.start_block(dead);
                }
            }
            Stmt::Continue(_span) => {
                if let Some(label) = self.continue_labels.last().cloned() {
                    self.terminate(Terminator::Branch(label));
                    let dead = self.fresh_label("post_continue");
                    self.start_block(dead);
                }
            }
            Stmt::Switch(expr, body, _span) => {
                // Evaluate switch expression and store it in an alloca so we can
                // reload it in the dispatch chain (which is emitted after the body).
                let val = self.lower_expr(expr);
                let switch_alloca = self.fresh_value();
                self.emit(Instruction::Alloca {
                    dest: switch_alloca,
                    ty: IrType::I64,
                    size: 8,
                });
                self.emit(Instruction::Store {
                    val,
                    ptr: switch_alloca,
                    ty: IrType::I64,
                });

                let dispatch_label = self.fresh_label("switch_dispatch");
                let end_label = self.fresh_label("switch_end");
                let body_label = self.fresh_label("switch_body");

                // Push switch context
                self.switch_end_labels.push(end_label.clone());
                self.break_labels.push(end_label.clone());
                self.switch_cases.push(Vec::new());
                self.switch_default.push(None);
                self.switch_val_allocas.push(switch_alloca);

                // Jump to dispatch (which will be emitted after the body)
                self.terminate(Terminator::Branch(dispatch_label.clone()));

                // Lower the switch body - case/default stmts will register
                // their labels in switch_cases/switch_default
                self.start_block(body_label.clone());
                self.lower_stmt(body);
                // Fall off end of switch body -> go to end
                self.terminate(Terminator::Branch(end_label.clone()));

                // Pop switch context and collect the case/default info
                self.switch_end_labels.pop();
                self.break_labels.pop();
                let cases = self.switch_cases.pop().unwrap_or_default();
                let default_label = self.switch_default.pop().flatten();
                self.switch_val_allocas.pop();

                // Now emit the dispatch chain: a series of comparison blocks
                // that check each case value and branch accordingly.
                let fallback = default_label.unwrap_or_else(|| end_label.clone());

                self.start_block(dispatch_label);

                if cases.is_empty() {
                    // No cases, just go to default or end
                    self.terminate(Terminator::Branch(fallback));
                } else {
                    // Emit comparison chain: each check block loads the switch
                    // value, compares against a case constant, and branches.
                    for (i, (case_val, case_label)) in cases.iter().enumerate() {
                        // Load the switch value in this block
                        let loaded = self.fresh_value();
                        self.emit(Instruction::Load {
                            dest: loaded,
                            ptr: switch_alloca,
                            ty: IrType::I64,
                        });

                        let cmp_result = self.fresh_value();
                        self.emit(Instruction::Cmp {
                            dest: cmp_result,
                            op: IrCmpOp::Eq,
                            lhs: Operand::Value(loaded),
                            rhs: Operand::Const(IrConst::I64(*case_val)),
                            ty: IrType::I64,
                        });

                        let next_check = if i + 1 < cases.len() {
                            self.fresh_label("switch_check")
                        } else {
                            fallback.clone()
                        };

                        self.terminate(Terminator::CondBranch {
                            cond: Operand::Value(cmp_result),
                            true_label: case_label.clone(),
                            false_label: next_check.clone(),
                        });

                        // Start next check block (unless this was the last case)
                        if i + 1 < cases.len() {
                            self.start_block(next_check);
                        }
                    }
                }

                self.start_block(end_label);
            }
            Stmt::Case(expr, stmt, _span) => {
                // Evaluate the case constant expression
                let case_val = self.eval_const_expr(expr)
                    .and_then(|c| self.const_to_i64(&c))
                    .unwrap_or(0);

                // Create a label for this case
                let label = self.fresh_label("case");

                // Register this case with the enclosing switch
                if let Some(cases) = self.switch_cases.last_mut() {
                    cases.push((case_val, label.clone()));
                }

                // Terminate current block and start the case block.
                // The previous case falls through to this one (C semantics).
                self.terminate(Terminator::Branch(label.clone()));
                self.start_block(label);
                self.lower_stmt(stmt);
            }
            Stmt::Default(stmt, _span) => {
                let label = self.fresh_label("default");

                // Register as default with enclosing switch
                if let Some(default) = self.switch_default.last_mut() {
                    *default = Some(label.clone());
                }

                // Fallthrough from previous case
                self.terminate(Terminator::Branch(label.clone()));
                self.start_block(label);
                self.lower_stmt(stmt);
            }
            Stmt::Goto(label, _span) => {
                let scoped_label = self.get_or_create_user_label(label);
                self.terminate(Terminator::Branch(scoped_label));
                let dead = self.fresh_label("post_goto");
                self.start_block(dead);
            }
            Stmt::Label(name, stmt, _span) => {
                let label = self.get_or_create_user_label(name);
                self.terminate(Terminator::Branch(label.clone()));
                self.start_block(label);
                self.lower_stmt(stmt);
            }
        }
    }

    /// Recursively emit struct field initialization from an initializer list.
    /// `base_alloca` is the alloca for the struct, `base_offset` is the byte offset
    /// from the outermost struct (for nested structs).
    /// Returns the number of initializer items consumed (for flat init across nested structs).
    pub(super) fn emit_struct_init(
        &mut self,
        items: &[InitializerItem],
        base_alloca: Value,
        layout: &StructLayout,
        base_offset: usize,
    ) -> usize {
        let mut item_idx = 0usize;
        let mut current_field_idx = 0usize;

        while item_idx < items.len() {
            let item = &items[item_idx];

            // Determine target field from designator or position
            let field_idx = if let Some(Designator::Field(ref name)) = item.designators.first() {
                // Designated initializer: look up field by name regardless of current_field_idx
                layout.fields.iter().position(|f| f.name == *name).unwrap_or(current_field_idx)
            } else {
                // Positional init: use current_field_idx, skip unnamed fields
                if current_field_idx >= layout.fields.len() { break; }
                let mut idx = current_field_idx;
                while idx < layout.fields.len() && layout.fields[idx].name.is_empty() {
                    idx += 1;
                }
                idx
            };

            if field_idx >= layout.fields.len() { break; }
            let field = &layout.fields[field_idx].clone();
            let field_offset = base_offset + field.offset;

            match &field.ty {
                CType::Struct(st) => {
                    // Nested struct field
                    let sub_layout = StructLayout::for_struct(&st.fields);
                    match &item.init {
                        Initializer::List(sub_items) => {
                            // Nested braces: { {10, 20}, 30 } - the sub_items init the inner struct
                            self.emit_struct_init(sub_items, base_alloca, &sub_layout, field_offset);
                            item_idx += 1;
                        }
                        Initializer::Expr(expr) => {
                            if self.expr_is_struct_value(expr) {
                                // Struct copy in init list: { 2, b } where b is a struct variable
                                // Emit memcpy from source struct to the target field offset
                                let src_addr = self.get_struct_base_addr(expr);
                                let dest_addr = self.fresh_value();
                                self.emit(Instruction::GetElementPtr {
                                    dest: dest_addr,
                                    base: base_alloca,
                                    offset: Operand::Const(IrConst::I64(field_offset as i64)),
                                    ty: IrType::Ptr,
                                });
                                self.emit(Instruction::Memcpy {
                                    dest: dest_addr,
                                    src: src_addr,
                                    size: sub_layout.size,
                                });
                                item_idx += 1;
                            } else {
                                // Flat init: { 10, 20, 30 } - consume items for inner struct fields
                                let consumed = self.emit_struct_init(&items[item_idx..], base_alloca, &sub_layout, field_offset);
                                item_idx += consumed;
                            }
                        }
                    }
                }
                CType::Union(st) => {
                    // Union: init first field only
                    let sub_layout = StructLayout::for_union(&st.fields);
                    match &item.init {
                        Initializer::List(sub_items) => {
                            if !sub_items.is_empty() {
                                self.emit_struct_init(sub_items, base_alloca, &sub_layout, field_offset);
                            }
                            item_idx += 1;
                        }
                        Initializer::Expr(expr) => {
                            if self.expr_is_struct_value(expr) {
                                // Union copy in init list
                                let src_addr = self.get_struct_base_addr(expr);
                                let dest_addr = self.fresh_value();
                                self.emit(Instruction::GetElementPtr {
                                    dest: dest_addr,
                                    base: base_alloca,
                                    offset: Operand::Const(IrConst::I64(field_offset as i64)),
                                    ty: IrType::Ptr,
                                });
                                self.emit(Instruction::Memcpy {
                                    dest: dest_addr,
                                    src: src_addr,
                                    size: sub_layout.size,
                                });
                                item_idx += 1;
                            } else {
                                let consumed = self.emit_struct_init(&items[item_idx..], base_alloca, &sub_layout, field_offset);
                                item_idx += consumed;
                            }
                        }
                    }
                }
                CType::Array(elem_ty, Some(arr_size)) => {
                    // Array field: init elements
                    let elem_size = elem_ty.size();
                    match &item.init {
                        Initializer::List(sub_items) => {
                            // Check for brace-wrapped string literal: {"hello"} for char[]
                            if sub_items.len() == 1 && sub_items[0].designators.is_empty() {
                                if let Initializer::Expr(Expr::StringLiteral(s, _)) = &sub_items[0].init {
                                    if matches!(elem_ty.as_ref(), CType::Char | CType::UChar) {
                                        self.emit_string_to_alloca(base_alloca, s, field_offset);
                                        item_idx += 1;
                                        current_field_idx = field_idx + 1;
                                        continue;
                                    }
                                }
                            }
                            // Braced array init
                            for (ai, sub_item) in sub_items.iter().enumerate() {
                                if ai >= *arr_size { break; }
                                let elem_offset = field_offset + ai * elem_size;
                                if let Initializer::Expr(e) = &sub_item.init {
                                    let expr_ty = self.get_expr_type(e);
                                    let val = self.lower_expr(e);
                                    let elem_ir_ty = IrType::from_ctype(elem_ty);
                                    let val = self.emit_implicit_cast(val, expr_ty, elem_ir_ty);
                                    let addr = self.fresh_value();
                                    self.emit(Instruction::GetElementPtr {
                                        dest: addr,
                                        base: base_alloca,
                                        offset: Operand::Const(IrConst::I64(elem_offset as i64)),
                                        ty: elem_ir_ty,
                                    });
                                    self.emit(Instruction::Store { val, ptr: addr, ty: elem_ir_ty });
                                }
                            }
                            item_idx += 1;
                        }
                        Initializer::Expr(e) => {
                            // String literal for char array field: copy bytes
                            if let Expr::StringLiteral(s, _) = e {
                                if matches!(elem_ty.as_ref(), CType::Char | CType::UChar) {
                                    self.emit_string_to_alloca(base_alloca, s, field_offset);
                                    item_idx += 1;
                                    current_field_idx = field_idx + 1;
                                    continue;
                                }
                            }
                            // Other single expression for array
                            let val = self.lower_expr(e);
                            let field_ty = IrType::from_ctype(&field.ty);
                            let addr = self.fresh_value();
                            self.emit(Instruction::GetElementPtr {
                                dest: addr,
                                base: base_alloca,
                                offset: Operand::Const(IrConst::I64(field_offset as i64)),
                                ty: field_ty,
                            });
                            self.emit(Instruction::Store { val, ptr: addr, ty: field_ty });
                            item_idx += 1;
                        }
                    }
                }
                _ => {
                    // Scalar field
                    let field_ty = IrType::from_ctype(&field.ty);
                    let (val, expr_ty) = match &item.init {
                        Initializer::Expr(e) => {
                            let et = self.get_expr_type(e);
                            (self.lower_expr(e), et)
                        }
                        Initializer::List(sub_items) => {
                            // Scalar with braces, e.g., int x = {5};
                            if let Some(first) = sub_items.first() {
                                if let Initializer::Expr(e) = &first.init {
                                    let et = self.get_expr_type(e);
                                    (self.lower_expr(e), et)
                                } else {
                                    (Operand::Const(IrConst::I64(0)), IrType::I64)
                                }
                            } else {
                                (Operand::Const(IrConst::I64(0)), IrType::I64)
                            }
                        }
                    };
                    // Implicit cast for type mismatches (e.g., int literal to float field)
                    let val = self.emit_implicit_cast(val, expr_ty, field_ty);
                    let addr = self.fresh_value();
                    self.emit(Instruction::GetElementPtr {
                        dest: addr,
                        base: base_alloca,
                        offset: Operand::Const(IrConst::I64(field_offset as i64)),
                        ty: field_ty,
                    });
                    self.emit(Instruction::Store { val, ptr: addr, ty: field_ty });
                    item_idx += 1;
                }
            }
            current_field_idx = field_idx + 1;
        }
        item_idx
    }
}
