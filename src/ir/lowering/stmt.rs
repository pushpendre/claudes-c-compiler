use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, CType, StructLayout};
use super::lowering::{Lowerer, LocalInfo, GlobalInfo, DeclAnalysis, SwitchFrame, resolve_typedef_derived};

impl Lowerer {
    pub(super) fn lower_compound_stmt(&mut self, compound: &CompoundStmt) {
        // Save current locals, static local names, const values, and enum constants
        // for block scope restoration. Enum constants declared in this block should
        // not be visible in outer scopes.
        let saved_locals = self.locals.clone();
        let saved_static_local_names = self.static_local_names.clone();
        let saved_const_local_values = self.const_local_values.clone();
        let saved_enum_constants = self.enum_constants.clone();

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

        // Restore outer scope locals, static local names, const values, and enum constants
        self.locals = saved_locals;
        self.static_local_names = saved_static_local_names;
        self.const_local_values = saved_const_local_values;
        self.enum_constants = saved_enum_constants;
    }

    pub(super) fn lower_local_decl(&mut self, decl: &Declaration) {
        // Resolve typeof(expr) to concrete type before processing
        let resolved_decl;
        let decl = if matches!(&decl.type_spec, TypeSpecifier::Typeof(_) | TypeSpecifier::TypeofType(_)) {
            let resolved_type_spec = self.resolve_typeof(&decl.type_spec);
            resolved_decl = Declaration {
                type_spec: resolved_type_spec,
                declarators: decl.declarators.clone(),
                is_typedef: decl.is_typedef,
                is_static: decl.is_static,
                is_extern: decl.is_extern,
                is_const: decl.is_const,
                span: decl.span,
            };
            &resolved_decl
        } else {
            decl
        };

        // First, register any struct/union definition from the type specifier
        self.register_struct_type(&decl.type_spec);

        // If this is a typedef, register the mapping and skip variable emission.
        // Uses resolve_typedef_derived() to avoid duplicating the derived-declarator
        // walk logic (Pointer wrapping, Array dim collection in reverse order).
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
                continue; // Skip anonymous declarations (e.g., bare struct definitions)
            }

            // Handle extern declarations inside function bodies:
            // they reference a global symbol, not a local variable
            if decl.is_extern {
                self.locals.remove(&declarator.name);
                // Also remove from static_local_names so that the extern name
                // resolves to the true global, not a same-named static local
                self.static_local_names.remove(&declarator.name);
                // Check if this is a function declaration (extern int f(int))
                // before treating it as a variable
                let is_func_decl = declarator.derived.iter().any(|d| matches!(d, DerivedDeclarator::Function(_, _)));
                if is_func_decl {
                    // Fall through to the function declaration handler below
                } else {
                    if !self.globals.contains_key(&declarator.name) {
                        let ext_da = self.analyze_declaration(&decl.type_spec, &declarator.derived);
                        self.globals.insert(declarator.name.clone(), GlobalInfo::from_analysis(&ext_da));
                    }
                    continue;
                }
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
                    self.func_meta.return_types.insert(declarator.name.clone(), ret_ty);
                    let param_tys: Vec<IrType> = params.iter().map(|p| {
                        self.type_spec_to_ir(&p.type_spec)
                    }).collect();
                    let param_bool_flags: Vec<bool> = params.iter().map(|p| {
                        matches!(self.resolve_type_spec(&p.type_spec), TypeSpecifier::Bool)
                    }).collect();
                    if !variadic || !param_tys.is_empty() {
                        self.func_meta.param_types.insert(declarator.name.clone(), param_tys);
                        self.func_meta.param_bool_flags.insert(declarator.name.clone(), param_bool_flags);
                    }
                    if variadic {
                        self.func_meta.variadic.insert(declarator.name.clone());
                    }
                    // Remove from locals if previously added (e.g., shadowed by this declaration)
                    self.locals.remove(&declarator.name);
                    continue;
                }

                // Also handle typedef-based function declarations in block scope
                // (e.g., `func_t add;` where func_t is `typedef int func_t(int);`)
                if declarator.derived.is_empty() && declarator.init.is_none() {
                    if let TypeSpecifier::TypedefName(tname) = &decl.type_spec {
                        if let Some(fti) = self.function_typedefs.get(tname).cloned() {
                            self.known_functions.insert(declarator.name.clone());
                            let ret_ty = self.type_spec_to_ir(&fti.return_type);
                            self.func_meta.return_types.insert(declarator.name.clone(), ret_ty);
                            let param_tys: Vec<IrType> = fti.params.iter().map(|p| {
                                self.type_spec_to_ir(&p.type_spec)
                            }).collect();
                            let param_bool_flags: Vec<bool> = fti.params.iter().map(|p| {
                                matches!(self.resolve_type_spec(&p.type_spec), TypeSpecifier::Bool)
                            }).collect();
                            if !fti.variadic || !param_tys.is_empty() {
                                self.func_meta.param_types.insert(declarator.name.clone(), param_tys);
                                self.func_meta.param_bool_flags.insert(declarator.name.clone(), param_bool_flags);
                            }
                            if fti.variadic {
                                self.func_meta.variadic.insert(declarator.name.clone());
                            }
                            self.locals.remove(&declarator.name);
                            continue;
                        }
                    }
                }
            }

            // Shared declaration analysis: computes base_ty, var_ty, array/pointer info,
            // struct layout, pointee type, etc. in one place (also used by lower_global_decl).
            let mut da = self.analyze_declaration(&decl.type_spec, &declarator.derived);
            self.fixup_unsized_array(&mut da, &decl.type_spec, &declarator.derived, &declarator.init);

            // Detect complex type variables and arrays of complex elements
            let resolved_ts = self.resolve_type_spec(&decl.type_spec);
            let is_complex = !da.is_pointer && !da.is_array && matches!(
                resolved_ts,
                TypeSpecifier::ComplexFloat | TypeSpecifier::ComplexDouble | TypeSpecifier::ComplexLongDouble
            );
            let complex_elem_ctype: Option<CType> = if da.is_array && !da.is_pointer {
                match &resolved_ts {
                    TypeSpecifier::ComplexFloat => Some(CType::ComplexFloat),
                    TypeSpecifier::ComplexDouble => Some(CType::ComplexDouble),
                    TypeSpecifier::ComplexLongDouble => Some(CType::ComplexLongDouble),
                    _ => None,
                }
            } else {
                None
            };

            // Handle static local variables: emit as globals with mangled names
            if decl.is_static {
                self.lower_local_static_decl(&decl, &declarator, &da);
                continue;
            }

            // Compute VLA runtime size if any array dimension is non-constant
            let vla_size = if da.is_array {
                self.compute_vla_runtime_size(&decl.type_spec, &declarator.derived)
            } else {
                None
            };

            let alloca = self.fresh_value();
            self.emit(Instruction::Alloca {
                dest: alloca,
                ty: if da.is_array || da.is_struct || is_complex { IrType::Ptr } else { da.var_ty },
                size: da.actual_alloc_size,
            });
            let mut local_info = LocalInfo::from_analysis(&da, alloca);
            local_info.vla_size = vla_size;
            self.locals.insert(declarator.name.clone(), local_info);

            // Track function pointer return and param types for correct calling convention
            for d in &declarator.derived {
                if let DerivedDeclarator::FunctionPointer(params, _) = d {
                    let ret_ty = self.type_spec_to_ir(&decl.type_spec);
                    self.func_meta.ptr_return_types.insert(declarator.name.clone(), ret_ty);
                    let param_tys: Vec<IrType> = params.iter().map(|p| {
                        self.type_spec_to_ir(&p.type_spec)
                    }).collect();
                    self.func_meta.ptr_param_types.insert(declarator.name.clone(), param_tys);
                    break;
                }
            }

            if let Some(ref init) = declarator.init {
                match init {
                    Initializer::Expr(expr) => {
                        if da.is_array && (da.base_ty == IrType::I8 || da.base_ty == IrType::U8) {
                            // Char array from string literal: char s[] = "hello"
                            if let Expr::StringLiteral(s, _) = expr {
                                self.emit_string_to_alloca(alloca, s, 0);
                            } else {
                                // Non-string expression initializer for char array (e.g., pointer assignment)
                                let val = self.lower_expr(expr);
                                self.emit(Instruction::Store { val, ptr: alloca, ty: da.var_ty });
                            }
                        } else if da.is_struct {
                            // Struct copy-initialization: struct Point b = a;
                            // For expressions producing packed struct data (small struct
                            // function call returns, ternaries over them, etc.), the value
                            // IS the struct data, not an address. Store directly.
                            if self.expr_produces_packed_struct_data(expr) && da.actual_alloc_size <= 8 {
                                let val = self.lower_expr(expr);
                                self.emit(Instruction::Store { val, ptr: alloca, ty: IrType::I64 });
                            } else {
                                let src_addr = self.get_struct_base_addr(expr);
                                self.emit(Instruction::Memcpy {
                                    dest: alloca,
                                    src: src_addr,
                                    size: da.actual_alloc_size,
                                });
                            }
                        } else if is_complex {
                            // Complex variable initialization: _Complex double z = expr;
                            // The expression might be:
                            // 1. Already complex (returns ptr to {real, imag} pair) -> memcpy
                            // 2. A real/integer scalar -> convert to complex first
                            let expr_ctype = self.expr_ctype(expr);
                            let val = self.lower_expr(expr);
                            let resolved_ts = self.resolve_type_spec(&decl.type_spec);
                            let complex_ctype = self.type_spec_to_ctype(&resolved_ts);
                            let src = if expr_ctype.is_complex() {
                                // Already complex - might need conversion between complex types
                                if expr_ctype != complex_ctype {
                                    let val_v = self.operand_to_value(val);
                                    let converted = self.complex_to_complex(val_v, &expr_ctype, &complex_ctype);
                                    self.operand_to_value(converted)
                                } else {
                                    self.operand_to_value(val)
                                }
                            } else {
                                // Real/integer scalar: convert to complex {val, 0}
                                let converted = self.real_to_complex(val, &expr_ctype, &complex_ctype);
                                self.operand_to_value(converted)
                            };
                            self.emit(Instruction::Memcpy {
                                dest: alloca,
                                src,
                                size: da.actual_alloc_size,
                            });
                        } else {
                            // Track const-qualified integer variable values for compile-time
                            // array size evaluation (e.g., const int len = 5000; int arr[len];)
                            if decl.is_const && !da.is_pointer && !da.is_array && !da.is_struct {
                                if let Some(const_val) = self.eval_const_expr(expr) {
                                    if let Some(ival) = self.const_to_i64(&const_val) {
                                        self.const_local_values.insert(declarator.name.clone(), ival);
                                    }
                                }
                            }
                            // Check if RHS is complex but LHS is non-complex:
                            // extract real part first, then convert to target type.
                            let rhs_ctype = self.expr_ctype(expr);
                            let val = if rhs_ctype.is_complex() && !is_complex {
                                let complex_val = self.lower_expr(expr);
                                let ptr = self.operand_to_value(complex_val);
                                let real_part = self.load_complex_real(ptr, &rhs_ctype);
                                let from_ty = Self::complex_component_ir_type(&rhs_ctype);
                                self.emit_implicit_cast(real_part, from_ty, da.var_ty)
                            } else {
                                let val = self.lower_expr(expr);
                                // Insert implicit cast for type mismatches
                                // (e.g., float f = 'a', int x = 3.14, char c = 99.0)
                                let expr_ty = self.get_expr_type(expr);
                                self.emit_implicit_cast(val, expr_ty, da.var_ty)
                            };
                            // _Bool variables clamp any value to 0 or 1
                            let val = if da.is_bool { self.emit_bool_normalize(val) } else { val };
                            self.emit(Instruction::Store { val, ptr: alloca, ty: da.var_ty });
                        }
                    }
                    Initializer::List(items) => {
                        if is_complex { // complex_elem_ctype check already set above
                            // Complex initializer list: _Complex double z = {real, imag}
                            let resolved_ts = self.resolve_type_spec(&decl.type_spec);
                            let complex_ctype = self.type_spec_to_ctype(&resolved_ts);
                            let comp_ty = Self::complex_component_ir_type(&complex_ctype);
                            // Store real part (first item)
                            if let Some(item) = items.first() {
                                if let Initializer::Expr(expr) = &item.init {
                                    let val = self.lower_expr(expr);
                                    let expr_ty = self.get_expr_type(expr);
                                    let val = self.emit_implicit_cast(val, expr_ty, comp_ty);
                                    self.emit(Instruction::Store { val, ptr: alloca, ty: comp_ty });
                                }
                            } else {
                                // Zero-init real part
                                let zero = match comp_ty {
                                    IrType::F32 => Operand::Const(IrConst::F32(0.0)),
                                    _ => Operand::Const(IrConst::F64(0.0)),
                                };
                                self.emit(Instruction::Store { val: zero, ptr: alloca, ty: comp_ty });
                            }
                            // Store imag part (second item) at offset
                            let comp_size = Self::complex_component_size(&complex_ctype);
                            let imag_ptr = self.fresh_value();
                            self.emit(Instruction::GetElementPtr {
                                dest: imag_ptr,
                                base: alloca,
                                offset: Operand::Const(IrConst::I64(comp_size as i64)),
                                ty: IrType::I8,
                            });
                            if let Some(item) = items.get(1) {
                                if let Initializer::Expr(expr) = &item.init {
                                    let val = self.lower_expr(expr);
                                    let expr_ty = self.get_expr_type(expr);
                                    let val = self.emit_implicit_cast(val, expr_ty, comp_ty);
                                    self.emit(Instruction::Store { val, ptr: imag_ptr, ty: comp_ty });
                                }
                            } else {
                                // Zero-init imag part if not provided
                                let zero = match comp_ty {
                                    IrType::F32 => Operand::Const(IrConst::F32(0.0)),
                                    _ => Operand::Const(IrConst::F64(0.0)),
                                };
                                self.emit(Instruction::Store { val: zero, ptr: imag_ptr, ty: comp_ty });
                            }
                        } else if da.is_struct {
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
                        } else if da.is_array && da.elem_size > 0 {
                            // Check if this is an array of structs
                            let elem_struct_layout = self.locals.get(&declarator.name)
                                .and_then(|l| l.struct_layout.clone());
                            if da.array_dim_strides.len() > 1 && elem_struct_layout.is_none() {
                                // Multi-dimensional array of scalars: zero first, then fill
                                self.zero_init_alloca(alloca, da.alloc_size);
                                // For pointer arrays (elem_size=8 but elem_ir_ty=I8),
                                // use I64 as the element type for stores.
                                let md_elem_ty = if da.is_array_of_pointers || da.is_array_of_func_ptrs { IrType::I64 } else { da.elem_ir_ty };
                                self.lower_array_init_list(items, alloca, md_elem_ty, &da.array_dim_strides);
                            } else if let Some(ref s_layout) = elem_struct_layout {
                                // Array of structs: init each element using struct layout.
                                // For multi-dimensional arrays (e.g., struct s arr[2][2]),
                                // da.elem_size is the row stride, but we need the actual struct
                                // size for per-element indexing in flat init.
                                let struct_size = s_layout.size;
                                // For multi-dim arrays, the number of struct elements is
                                // alloc_size / struct_size (total flattened count).
                                let is_multidim = da.array_dim_strides.len() > 1;
                                // row_size: number of struct elements per outer dimension
                                let row_size = if is_multidim && struct_size > 0 {
                                    da.elem_size / struct_size
                                } else {
                                    0
                                };
                                self.zero_init_alloca(alloca, da.alloc_size);
                                let mut flat_struct_idx = 0usize;
                                let mut item_idx = 0usize;
                                while item_idx < items.len() {
                                    let item = &items[item_idx];
                                    // Handle designators for index positioning
                                    if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                                        if let Some(idx_val) = self.eval_const_expr_for_designator(idx_expr) {
                                            if is_multidim {
                                                // For 2D arrays, [idx] designates the outer dimension
                                                flat_struct_idx = idx_val * row_size;
                                            } else {
                                                flat_struct_idx = idx_val;
                                            }
                                        }
                                    }
                                    let base_byte_offset = flat_struct_idx * struct_size;
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
                                            if is_multidim {
                                                // Multi-dim braced sub-list: {1,2,3,4} fills one row
                                                // of structs. Consume sub_items across struct elements.
                                                let mut sub_idx = 0usize;
                                                let mut row_elem = 0usize;
                                                while sub_idx < sub_items.len() && row_elem < row_size {
                                                    let row_offset = (flat_struct_idx + row_elem) * struct_size;
                                                    match &sub_items[sub_idx].init {
                                                        Initializer::List(inner) => {
                                                            // Braced struct init: {a, b}
                                                            let elem_base = self.fresh_value();
                                                            self.emit(Instruction::GetElementPtr {
                                                                dest: elem_base,
                                                                base: alloca,
                                                                offset: Operand::Const(IrConst::I64(row_offset as i64)),
                                                                ty: IrType::I8,
                                                            });
                                                            self.lower_local_struct_init(inner, elem_base, s_layout);
                                                            sub_idx += 1;
                                                            row_elem += 1;
                                                        }
                                                        Initializer::Expr(_) => {
                                                            // Flat scalars filling struct fields
                                                            let consumed = self.emit_struct_init(&sub_items[sub_idx..], alloca, s_layout, row_offset);
                                                            sub_idx += consumed.max(1);
                                                            row_elem += 1;
                                                        }
                                                    }
                                                }
                                                flat_struct_idx += row_size;
                                            } else {
                                                // 1D: Initialize struct fields from sub-list
                                                let elem_base = self.fresh_value();
                                                self.emit(Instruction::GetElementPtr {
                                                    dest: elem_base,
                                                    base: alloca,
                                                    offset: Operand::Const(IrConst::I64(base_byte_offset as i64)),
                                                    ty: IrType::I8,
                                                });
                                                self.lower_local_struct_init(sub_items, elem_base, s_layout);
                                                flat_struct_idx += 1;
                                            }
                                            item_idx += 1;
                                        }
                                        Initializer::Expr(e) => {
                                            if let Some(ref fname) = field_designator_name {
                                                // Designated field: [idx].field = val
                                                if let Some(field) = s_layout.fields.iter().find(|f| &f.name == fname) {
                                                    let field_ty = IrType::from_ctype(&field.ty);
                                                    let val = self.lower_and_cast_init_expr(e, field_ty);
                                                    let field_addr = self.fresh_value();
                                                    self.emit(Instruction::GetElementPtr {
                                                        dest: field_addr,
                                                        base: alloca,
                                                        offset: Operand::Const(IrConst::I64(base_byte_offset as i64 + field.offset as i64)),
                                                        ty: field_ty,
                                                    });
                                                    self.emit(Instruction::Store { val, ptr: field_addr, ty: field_ty });
                                                }
                                                item_idx += 1;
                                            } else {
                                                // Flat init: consume items for all fields of this struct
                                                let consumed = self.emit_struct_init(&items[item_idx..], alloca, s_layout, base_byte_offset);
                                                item_idx += consumed.max(1);
                                                flat_struct_idx += 1;
                                                continue; // skip the increment below
                                            }
                                        }
                                    }
                                }
                            } else if let Some(ref cplx_ctype) = complex_elem_ctype {
                                // Array of complex elements: each element is a {real, imag} pair
                                // Use Memcpy for each element from the expression result
                                self.zero_init_alloca(alloca, da.alloc_size);
                                let mut current_idx = 0usize;
                                for item in items.iter() {
                                    if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                                        if let Some(idx_val) = self.eval_const_expr_for_designator(idx_expr) {
                                            current_idx = idx_val;
                                        }
                                    }
                                    let init_expr = match &item.init {
                                        Initializer::Expr(e) => Some(e),
                                        Initializer::List(sub_items) => {
                                            Self::unwrap_nested_init_expr(sub_items)
                                        }
                                    };
                                    if let Some(e) = init_expr {
                                        let expr_ctype = self.expr_ctype(e);
                                        let val = self.lower_expr(e);
                                        // Convert to the target complex type if needed
                                        let src = if expr_ctype.is_complex() {
                                            if expr_ctype != *cplx_ctype {
                                                let val_v = self.operand_to_value(val);
                                                let converted = self.complex_to_complex(val_v, &expr_ctype, cplx_ctype);
                                                self.operand_to_value(converted)
                                            } else {
                                                self.operand_to_value(val)
                                            }
                                        } else {
                                            // Real scalar -> complex {val, 0}
                                            let converted = self.real_to_complex(val, &expr_ctype, cplx_ctype);
                                            self.operand_to_value(converted)
                                        };
                                        // Compute target address for this element
                                        let elem_addr = self.fresh_value();
                                        self.emit(Instruction::GetElementPtr {
                                            dest: elem_addr,
                                            base: alloca,
                                            offset: Operand::Const(IrConst::I64((current_idx * da.elem_size) as i64)),
                                            ty: IrType::I8,
                                        });
                                        self.emit(Instruction::Memcpy {
                                            dest: elem_addr,
                                            src,
                                            size: da.elem_size,
                                        });
                                    }
                                    current_idx += 1;
                                }
                            } else {
                                // 1D array: supports designated initializers [idx] = val
                                // Also zero-fill first if partially initialized.
                                let num_elems = da.alloc_size / da.elem_size.max(1);
                                let has_designators = items.iter().any(|item| !item.designators.is_empty());
                                if has_designators || items.len() < num_elems {
                                    // Partial initialization or designators: zero the entire array first
                                    self.zero_init_alloca(alloca, da.alloc_size);
                                }

                                // Detect arrays of complex types. Complex elements are stored as
                                // {real, imag} pairs and lower_expr returns a pointer to a stack
                                // temp. We need to Memcpy from that pointer to the array slot.
                                let is_complex_elem_array = matches!(
                                    self.resolve_type_spec(&decl.type_spec),
                                    TypeSpecifier::ComplexFloat | TypeSpecifier::ComplexDouble | TypeSpecifier::ComplexLongDouble
                                );

                                // For arrays of pointers, the element type is Ptr (8 bytes),
                                // not the base type spec (which would be e.g. I32 for int *arr[N]).
                                let elem_store_ty = if da.is_array_of_pointers || da.is_array_of_func_ptrs { IrType::I64 } else { da.elem_ir_ty };

                                let mut current_idx = 0usize;
                                for item in items.iter() {
                                    // Check for index designator
                                    if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                                        if let Some(idx_val) = self.eval_const_expr_for_designator(idx_expr) {
                                            current_idx = idx_val;
                                        }
                                    }

                                    // Handle each item, with special case for string literals in char arrays
                                    // For double-brace init like {{expr}}, unwrap the nested list
                                    let init_expr = match &item.init {
                                        Initializer::Expr(e) => Some(e),
                                        Initializer::List(sub_items) => {
                                            // Double-brace: unwrap nested list to get the first expression
                                            Self::unwrap_nested_init_expr(sub_items)
                                        }
                                    };
                                    if let Some(e) = init_expr {
                                        if !da.is_array_of_pointers && (da.elem_ir_ty == IrType::I8 || da.elem_ir_ty == IrType::U8) {
                                            if let Expr::StringLiteral(s, _) = e {
                                                self.emit_string_to_alloca(alloca, s, current_idx * da.elem_size);
                                                current_idx += 1;
                                                continue;
                                            }
                                        }
                                        if is_complex_elem_array {
                                            // Complex array element: lower_expr returns a pointer to
                                            // a stack-allocated {real, imag} pair. Memcpy from that
                                            // pointer to the target array slot.
                                            let val = self.lower_expr(e);
                                            let src = self.operand_to_value(val);
                                            let dest = self.fresh_value();
                                            self.emit(Instruction::GetElementPtr {
                                                dest,
                                                base: alloca,
                                                offset: Operand::Const(IrConst::I64((current_idx * da.elem_size) as i64)),
                                                ty: IrType::I8,
                                            });
                                            self.emit(Instruction::Memcpy {
                                                dest,
                                                src,
                                                size: da.elem_size,
                                            });
                                        } else {
                                            let val = self.lower_expr(e);
                                            let expr_ty = self.get_expr_type(e);
                                            let val = self.emit_implicit_cast(val, expr_ty, elem_store_ty);
                                            self.emit_array_element_store(alloca, val, current_idx * da.elem_size, elem_store_ty);
                                        }
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
                                    let val = self.emit_implicit_cast(val, expr_ty, da.var_ty);
                                    let val = if da.is_bool { self.emit_bool_normalize(val) } else { val };
                                    self.emit(Instruction::Store { val, ptr: alloca, ty: da.var_ty });
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Handle static local variable declarations: emit as globals with mangled names.
    /// Static locals are initialized once at program start (via .data/.bss),
    /// not at every function call.
    fn lower_local_static_decl(&mut self, decl: &Declaration, declarator: &InitDeclarator, da: &DeclAnalysis) {
        let static_id = self.next_static_local;
        let static_name = format!("{}.{}.{}", self.current_function_name, declarator.name, static_id);

        // Register the bare name -> mangled name mapping before processing the initializer
        // so that &x in another static's initializer can resolve to the mangled name.
        self.static_local_names.insert(declarator.name.clone(), static_name.clone());

        // Determine initializer (evaluated at compile time for static locals)
        let init = if let Some(ref initializer) = declarator.init {
            self.lower_global_init(
                initializer, &decl.type_spec, da.base_ty, da.is_array,
                da.elem_size, da.actual_alloc_size, &da.struct_layout, &da.array_dim_strides,
            )
        } else {
            GlobalInit::Zero
        };

        let align = da.var_ty.align();

        // For struct initializers emitted as byte arrays, set element type to I8
        let global_ty = if matches!(&init, GlobalInit::Array(vals) if !vals.is_empty() && matches!(vals[0], IrConst::I8(_))) {
            IrType::I8
        } else if da.is_struct && matches!(&init, GlobalInit::Array(_)) {
            IrType::I8
        } else {
            da.var_ty
        };

        self.module.globals.push(IrGlobal {
            name: static_name.clone(),
            ty: global_ty,
            size: da.actual_alloc_size,
            align,
            init,
            is_static: true,
            is_extern: false,
        });

        // Track as a global for access via GlobalAddr
        self.globals.insert(static_name.clone(), GlobalInfo::from_analysis(da));

        // Store type info in locals (with static_global_name set so each use site
        // emits a fresh GlobalAddr in its own basic block, avoiding unreachable-block issues).
        self.locals.insert(declarator.name.clone(), LocalInfo::for_static(da, static_name));
        self.next_static_local += 1;
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
            let desig_name = match item.designators.first() {
                Some(Designator::Field(ref name)) => Some(name.as_str()),
                _ => None,
            };
            let field_idx = match layout.resolve_init_field_idx(desig_name, current_field_idx) {
                Some(idx) => idx,
                None => break,
            };

            let field = &layout.fields[field_idx];
            let field_offset = field.offset;

            // Complex fields need special handling: memcpy data instead of storing pointer
            if field.ty.is_complex() {
                let complex_ctype = field.ty.clone();
                let complex_size = complex_ctype.size();
                let dest_addr = self.fresh_value();
                self.emit(Instruction::GetElementPtr {
                    dest: dest_addr,
                    base,
                    offset: Operand::Const(IrConst::I64(field_offset as i64)),
                    ty: IrType::Ptr,
                });
                match &item.init {
                    Initializer::Expr(e) => {
                        let expr_ctype = self.expr_ctype(e);
                        let val = self.lower_expr(e);
                        let src = if expr_ctype.is_complex() {
                            if expr_ctype != complex_ctype {
                                let val_v = self.operand_to_value(val);
                                let converted = self.complex_to_complex(val_v, &expr_ctype, &complex_ctype);
                                self.operand_to_value(converted)
                            } else {
                                self.operand_to_value(val)
                            }
                        } else {
                            let converted = self.real_to_complex(val, &expr_ctype, &complex_ctype);
                            self.operand_to_value(converted)
                        };
                        self.emit(Instruction::Memcpy {
                            dest: dest_addr,
                            src,
                            size: complex_size,
                        });
                    }
                    Initializer::List(sub_items) => {
                        let comp_ty = Self::complex_component_ir_type(&complex_ctype);
                        let comp_size = Self::complex_component_size(&complex_ctype);
                        if let Some(first) = sub_items.first() {
                            if let Initializer::Expr(e) = &first.init {
                                let val = self.lower_expr(e);
                                let expr_ty = self.get_expr_type(e);
                                let val = self.emit_implicit_cast(val, expr_ty, comp_ty);
                                self.emit(Instruction::Store { val, ptr: dest_addr, ty: comp_ty });
                            }
                        } else {
                            let zero = match comp_ty {
                                IrType::F32 => Operand::Const(IrConst::F32(0.0)),
                                _ => Operand::Const(IrConst::F64(0.0)),
                            };
                            self.emit(Instruction::Store { val: zero, ptr: dest_addr, ty: comp_ty });
                        }
                        let imag_ptr = self.fresh_value();
                        self.emit(Instruction::GetElementPtr {
                            dest: imag_ptr,
                            base: dest_addr,
                            offset: Operand::Const(IrConst::I64(comp_size as i64)),
                            ty: IrType::I8,
                        });
                        if let Some(item) = sub_items.get(1) {
                            if let Initializer::Expr(e) = &item.init {
                                let val = self.lower_expr(e);
                                let expr_ty = self.get_expr_type(e);
                                let val = self.emit_implicit_cast(val, expr_ty, comp_ty);
                                self.emit(Instruction::Store { val, ptr: imag_ptr, ty: comp_ty });
                            }
                        } else {
                            let zero = match comp_ty {
                                IrType::F32 => Operand::Const(IrConst::F32(0.0)),
                                _ => Operand::Const(IrConst::F64(0.0)),
                            };
                            self.emit(Instruction::Store { val: zero, ptr: imag_ptr, ty: comp_ty });
                        }
                    }
                }
                current_field_idx = field_idx + 1;
                continue;
            }

            let field_ty = IrType::from_ctype(&field.ty);

            match &item.init {
                Initializer::Expr(e) => {
                    let val = self.lower_and_cast_init_expr(e, field_ty);
                    let field_addr = self.fresh_value();
                    self.emit(Instruction::GetElementPtr {
                        dest: field_addr,
                        base,
                        offset: Operand::Const(IrConst::I64(field_offset as i64)),
                        ty: field_ty,
                    });
                    if let (Some(bit_offset), Some(bit_width)) = (field.bit_offset, field.bit_width) {
                        // Bitfield: read-modify-write
                        self.store_bitfield(field_addr, field_ty, bit_offset, bit_width, val);
                    } else {
                        self.emit(Instruction::Store { val, ptr: field_addr, ty: field_ty });
                    }
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
                // Array field: init elements with [idx]=val designator support
                let elem_ir_ty = IrType::from_ctype(elem_ty);
                let elem_size = elem_ty.size();
                let mut ai = 0usize;
                for item in items.iter() {
                    // Check for index designator: [idx]=val
                    if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                        if let Some(idx) = self.eval_const_expr_for_designator(idx_expr) {
                            ai = idx;
                        }
                    }
                    if let Initializer::Expr(e) = &item.init {
                        let val = self.lower_and_cast_init_expr(e, elem_ir_ty);
                        let elem_addr = self.fresh_value();
                        self.emit(Instruction::GetElementPtr {
                            dest: elem_addr,
                            base,
                            offset: Operand::Const(IrConst::I64((ai * elem_size) as i64)),
                            ty: elem_ir_ty,
                        });
                        self.emit(Instruction::Store { val, ptr: elem_addr, ty: elem_ir_ty });
                    }
                    ai += 1;
                }
            }
            _ => {
                // Scalar field with nested braces (e.g., int x = {5})
                if let Some(first) = items.first() {
                    if let Initializer::Expr(e) = &first.init {
                        let field_ir_ty = IrType::from_ctype(field_ctype);
                        let val = self.lower_and_cast_init_expr(e, field_ir_ty);
                        self.emit(Instruction::Store { val, ptr: base, ty: field_ir_ty });
                    }
                }
            }
        }
    }

    /// Unwrap nested `Initializer::List` items to find the innermost expression.
    /// Used for double-brace init like `{{expr}}` to extract the `expr`.
    pub(super) fn unwrap_nested_init_expr(items: &[InitializerItem]) -> Option<&Expr> {
        if let Some(first) = items.first() {
            match &first.init {
                Initializer::Expr(e) => Some(e),
                Initializer::List(sub_items) => Self::unwrap_nested_init_expr(sub_items),
            }
        } else {
            None
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
                    // Advance to next sub-array boundary after a braced sub-list
                    // This handles partial initialization: {{1,2},{3}} for int a[2][3]
                    // After {1,2} we advance to index 3 (next row), after {3} to index 6
                    if array_dim_strides.len() > 1 {
                        let boundary = start_index + sub_elem_count;
                        if *flat_index < boundary {
                            *flat_index = boundary;
                        }
                    }
                }
                Initializer::Expr(e) => {
                    // String literal fills a sub-array in char arrays
                    if base_ty == IrType::I8 || base_ty == IrType::U8 {
                        if let Expr::StringLiteral(s, _) = e {
                            self.emit_string_to_alloca(alloca, s, *flat_index * elem_size);
                            // String literal fills the innermost char sub-array.
                            // For multi-dim arrays like char[2][3][1][1][3], a string fills
                            // the innermost [1][1][3] sub-array (3 chars), not the full
                            // first-dim sub-array [3][1][1][3] (9 chars).
                            // Use strides[len-2] if available (second-to-last stride = innermost sub-array size).
                            let string_stride = if array_dim_strides.len() >= 2 {
                                array_dim_strides[array_dim_strides.len() - 2]
                            } else {
                                sub_elem_count
                            };
                            *flat_index += string_stride;
                            continue;
                        }
                    }
                    // Bare scalar: fills one base element without sub-array padding
                    let val = self.lower_and_cast_init_expr(e, base_ty);
                    self.emit_array_element_store(alloca, val, *flat_index * elem_size, base_ty);
                    *flat_index += 1;
                }
            }
        }
    }

    /// Lower an expression and cast it to the target type (e.g., int to float).
    pub(super) fn lower_and_cast_init_expr(&mut self, expr: &Expr, target_ty: IrType) -> Operand {
        let val = self.lower_expr(expr);
        let expr_ty = self.get_expr_type(expr);
        self.emit_implicit_cast(val, expr_ty, target_ty)
    }

    pub(super) fn lower_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Return(expr, _span) => {
                let op = expr.as_ref().map(|e| {
                    // For struct returns with sret (> 8 bytes), copy struct data to the
                    // hidden sret pointer and return that pointer.
                    if let Some(sret_alloca) = self.current_sret_ptr {
                        if let Some(struct_size) = self.struct_value_size(e) {
                            if struct_size > 8 {
                                let src_addr = self.get_struct_base_addr(e);
                                // Load the sret pointer from its alloca
                                let sret_ptr = self.fresh_value();
                                self.emit(Instruction::Load { dest: sret_ptr, ptr: sret_alloca, ty: IrType::Ptr });
                                // Memcpy struct data to sret destination
                                self.emit(Instruction::Memcpy { dest: sret_ptr, src: src_addr, size: struct_size });
                                // Return the sret pointer
                                return Operand::Value(sret_ptr);
                            }
                        }
                        // For complex returns with sret, copy complex data to sret pointer
                        let expr_ct = self.expr_ctype(e);
                        let ret_ct = self.func_return_ctypes.get(&self.current_function_name.clone()).cloned();
                        if expr_ct.is_complex() {
                            let val = self.lower_expr(e);
                            let src_addr = if let Some(ref rct) = ret_ct {
                                if rct.is_complex() && expr_ct != *rct {
                                    // Convert complex expression to function return type
                                    let ptr = self.operand_to_value(val);
                                    let converted = self.complex_to_complex(ptr, &expr_ct, rct);
                                    self.operand_to_value(converted)
                                } else {
                                    self.operand_to_value(val)
                                }
                            } else {
                                self.operand_to_value(val)
                            };
                            let complex_size = ret_ct.as_ref().unwrap_or(&expr_ct).size();
                            let sret_ptr = self.fresh_value();
                            self.emit(Instruction::Load { dest: sret_ptr, ptr: sret_alloca, ty: IrType::Ptr });
                            self.emit(Instruction::Memcpy { dest: sret_ptr, src: src_addr, size: complex_size });
                            return Operand::Value(sret_ptr);
                        }
                        // Non-complex expression returned from sret complex function:
                        // convert scalar to complex and copy to sret pointer.
                        // Use scalar_to_complex (IR-type based) for accurate type handling
                        // when expr_ctype may not match the actual IR type (e.g., function calls).
                        if let Some(ref rct) = ret_ct {
                            if rct.is_complex() {
                                let val = self.lower_expr(e);
                                let src_ir_ty = self.get_expr_type(e);
                                let rct_clone = rct.clone();
                                let complex_val = self.scalar_to_complex(val, src_ir_ty, &rct_clone);
                                let src_addr = self.operand_to_value(complex_val);
                                let complex_size = rct_clone.size();
                                let sret_ptr = self.fresh_value();
                                self.emit(Instruction::Load { dest: sret_ptr, ptr: sret_alloca, ty: IrType::Ptr });
                                self.emit(Instruction::Memcpy { dest: sret_ptr, src: src_addr, size: complex_size });
                                return Operand::Value(sret_ptr);
                            }
                        }
                    }
                    // For small struct returns (<= 8 bytes), load the struct data
                    // from its address so it goes into rax as data (not as a pointer).
                    if let Some(struct_size) = self.struct_value_size(e) {
                        if struct_size <= 8 {
                            let addr = self.get_struct_base_addr(e);
                            let dest = self.fresh_value();
                            self.emit(Instruction::Load { dest, ptr: addr, ty: IrType::I64 });
                            return Operand::Value(dest);
                        }
                    }
                    // For non-sret complex returns (e.g., _Complex float), handle
                    // implicit type conversion when expression type differs from return type.
                    let expr_ct = self.expr_ctype(e);
                    let ret_ct = self.func_return_ctypes.get(&self.current_function_name.clone()).cloned();
                    if expr_ct.is_complex() {
                        if let Some(ref rct) = ret_ct {
                            if rct.is_complex() && expr_ct != *rct {
                                // Complex-to-complex conversion (different precision)
                                let val = self.lower_expr(e);
                                let ptr = self.operand_to_value(val);
                                let converted = self.complex_to_complex(ptr, &expr_ct, rct);
                                // For non-sret complex float, pack into I64 for return
                                if *rct == CType::ComplexFloat && !self.func_meta.sret_functions.contains_key(&self.current_function_name) {
                                    let conv_ptr = self.operand_to_value(converted);
                                    let packed = self.fresh_value();
                                    self.emit(Instruction::Load { dest: packed, ptr: conv_ptr, ty: IrType::I64 });
                                    return Operand::Value(packed);
                                }
                                return converted;
                            }
                            // Same complex type, non-sret: pack into I64 for register return
                            if *rct == CType::ComplexFloat && !self.func_meta.sret_functions.contains_key(&self.current_function_name) {
                                let val = self.lower_expr(e);
                                let ptr = self.operand_to_value(val);
                                let packed = self.fresh_value();
                                self.emit(Instruction::Load { dest: packed, ptr, ty: IrType::I64 });
                                return Operand::Value(packed);
                            }
                        }
                        // Complex expression returned from non-complex function:
                        // extract real part and convert to return type
                        let ret_ty = self.current_return_type;
                        if ret_ty != IrType::Ptr {
                            let val = self.lower_expr(e);
                            let ptr = self.operand_to_value(val);
                            let real_part = self.load_complex_real(ptr, &expr_ct);
                            let from_ty = Self::complex_component_ir_type(&expr_ct);
                            let val = self.emit_implicit_cast(real_part, from_ty, ret_ty);
                            return if self.current_return_is_bool { self.emit_bool_normalize(val) } else { val };
                        }
                    }
                    // Non-complex expression returned from a complex-returning function:
                    // convert scalar to complex (real=scalar, imag=0).
                    // Use scalar_to_complex for accurate IR-type-based conversion.
                    {
                        let ret_ct = self.func_return_ctypes.get(&self.current_function_name.clone()).cloned();
                        if let Some(ref rct) = ret_ct {
                            if rct.is_complex() && !expr_ct.is_complex() {
                                let val = self.lower_expr(e);
                                let src_ir_ty = self.get_expr_type(e);
                                let rct_clone = rct.clone();
                                let complex_val = self.scalar_to_complex(val, src_ir_ty, &rct_clone);
                                // For sret returns, copy to the hidden pointer
                                if let Some(sret_alloca) = self.current_sret_ptr {
                                    let src_addr = self.operand_to_value(complex_val);
                                    let complex_size = rct_clone.size();
                                    let sret_ptr = self.fresh_value();
                                    self.emit(Instruction::Load { dest: sret_ptr, ptr: sret_alloca, ty: IrType::Ptr });
                                    self.emit(Instruction::Memcpy { dest: sret_ptr, src: src_addr, size: complex_size });
                                    return Operand::Value(sret_ptr);
                                }
                                // For non-sret complex float, pack into I64 for register return
                                if rct_clone == CType::ComplexFloat && !self.func_meta.sret_functions.contains_key(&self.current_function_name) {
                                    let ptr = self.operand_to_value(complex_val);
                                    let packed = self.fresh_value();
                                    self.emit(Instruction::Load { dest: packed, ptr, ty: IrType::I64 });
                                    return Operand::Value(packed);
                                }
                                return complex_val;
                            }
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
                let cond_val = self.lower_condition_expr(cond);
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
                let cond_val = self.lower_condition_expr(cond);
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
                let saved_static_local_names = self.static_local_names.clone();
                let saved_const_local_values = self.const_local_values.clone();
                let saved_enum_constants = self.enum_constants.clone();

                // Init
                if let Some(init) = init {
                    match init.as_ref() {
                        ForInit::Declaration(decl) => {
                            self.collect_enum_constants(&decl.type_spec);
                            self.lower_local_decl(decl);
                        }
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
                    let cond_val = self.lower_condition_expr(cond);
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

                // Restore locals, static local names, const values, and enum constants
                // to exit for-init scope
                self.locals = saved_locals;
                self.static_local_names = saved_static_local_names;
                self.const_local_values = saved_const_local_values;
                self.enum_constants = saved_enum_constants;
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
                let cond_val = self.lower_condition_expr(cond);
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
                // C99 6.8.4.2: Integer promotions are performed on the controlling expression.
                let raw_expr_ty = self.get_expr_type(expr);
                let switch_expr_ty = match raw_expr_ty {
                    IrType::I8 | IrType::U8 | IrType::I16 | IrType::U16 => IrType::I32,
                    _ => raw_expr_ty,
                };
                let val = self.lower_expr(expr);
                // If the expression type was sub-int, widen it via implicit cast
                let val = if switch_expr_ty != raw_expr_ty {
                    self.emit_implicit_cast(val, raw_expr_ty, switch_expr_ty)
                } else {
                    val
                };
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
                self.switch_stack.push(SwitchFrame {
                    end_label: end_label.clone(),
                    cases: Vec::new(),
                    default_label: None,
                    val_alloca: switch_alloca,
                    expr_type: switch_expr_ty,
                });
                self.break_labels.push(end_label.clone());

                // Jump to dispatch (which will be emitted after the body)
                self.terminate(Terminator::Branch(dispatch_label.clone()));

                // Lower the switch body - case/default stmts will register
                // their labels in switch_cases/switch_default
                self.start_block(body_label.clone());
                self.lower_stmt(body);
                // Fall off end of switch body -> go to end
                self.terminate(Terminator::Branch(end_label.clone()));

                // Pop switch context and collect the case/default info
                let switch_frame = self.switch_stack.pop();
                self.break_labels.pop();
                let cases = switch_frame.as_ref().map(|f| f.cases.clone()).unwrap_or_default();
                let default_label = switch_frame.as_ref().and_then(|f| f.default_label.clone());

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

                        let cmp_result = self.emit_cmp_val(IrCmpOp::Eq, Operand::Value(loaded), Operand::Const(IrConst::I64(*case_val)), IrType::I64);

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
                let mut case_val = self.eval_const_expr(expr)
                    .and_then(|c| self.const_to_i64(&c))
                    .unwrap_or(0);

                // Truncate case value to the switch controlling expression type.
                // C requires case constants to be converted to the promoted type
                // of the controlling expression (e.g., case 2^33 in switch(int) -> 0).
                if let Some(switch_ty) = self.switch_stack.last().map(|f| &f.expr_type) {
                    case_val = match switch_ty {
                        IrType::I8 => case_val as i8 as i64,
                        IrType::U8 => case_val as u8 as i64,
                        IrType::I16 => case_val as i16 as i64,
                        IrType::U16 => case_val as u16 as i64,
                        IrType::I32 => case_val as i32 as i64,
                        IrType::U32 => case_val as u32 as i64,
                        _ => case_val,
                    };
                }

                // Create a label for this case
                let label = self.fresh_label("case");

                // Register this case with the enclosing switch
                if let Some(frame) = self.switch_stack.last_mut() {
                    frame.cases.push((case_val, label.clone()));
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
                if let Some(frame) = self.switch_stack.last_mut() {
                    frame.default_label = Some(label.clone());
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
            Stmt::GotoIndirect(expr, _span) => {
                let target = self.lower_expr(expr);
                // Collect all known user labels as possible targets
                let possible_targets: Vec<String> = self.user_labels.values().cloned().collect();
                self.terminate(Terminator::IndirectBranch { target, possible_targets });
                let dead = self.fresh_label("post_indirect_goto");
                self.start_block(dead);
            }
            Stmt::Label(name, stmt, _span) => {
                let label = self.get_or_create_user_label(name);
                self.terminate(Terminator::Branch(label.clone()));
                self.start_block(label);
                self.lower_stmt(stmt);
            }
            Stmt::InlineAsm { template, outputs, inputs, clobbers } => {
                let mut ir_outputs = Vec::new();
                let mut ir_inputs = Vec::new();
                let mut operand_types = Vec::new();

                // Process output operands: get the address (pointer) to store result
                // Also collect types for synthetic "+" inputs separately
                let mut plus_input_types = Vec::new();
                for out in outputs {
                    let constraint = out.constraint.clone();
                    let name = out.name.clone();
                    let out_ty = self.get_expr_type(&out.expr);
                    // Get the lvalue address for the output expression
                    if let Some(lv) = self.lower_lvalue(&out.expr) {
                        let ptr = match lv {
                            super::lowering::LValue::Variable(v) | super::lowering::LValue::Address(v) => v,
                        };
                        // For "+r" (read-write), also treat as input (load current value)
                        if constraint.contains('+') {
                            let cur_val = self.fresh_value();
                            let ty = self.get_expr_type(&out.expr);
                            self.emit(Instruction::Load { dest: cur_val, ptr, ty });
                            ir_inputs.push((constraint.replace('+', "").to_string(), Operand::Value(cur_val), name.clone()));
                            plus_input_types.push(out_ty.clone());
                        }
                        ir_outputs.push((constraint, ptr, name));
                        operand_types.push(out_ty);
                    }
                }
                // Add types for synthetic "+" inputs (they go at the beginning of ir_inputs)
                for ty in plus_input_types {
                    operand_types.push(ty);
                }

                // Process input operands: evaluate expression to get value
                for inp in inputs {
                    let constraint = inp.constraint.clone();
                    let name = inp.name.clone();
                    let inp_ty = self.get_expr_type(&inp.expr);
                    // For immediate constraints ("I", "i", "n"), try to evaluate as
                    // a compile-time constant to preserve the value for direct substitution
                    let stripped = constraint.trim_start_matches(|c: char| c == '=' || c == '+' || c == '&');
                    let val = if matches!(stripped, "I" | "i" | "n") {
                        if let Some(const_val) = self.eval_const_expr(&inp.expr) {
                            Operand::Const(const_val)
                        } else {
                            self.lower_expr(&inp.expr)
                        }
                    } else {
                        self.lower_expr(&inp.expr)
                    };
                    ir_inputs.push((constraint, val, name));
                    operand_types.push(inp_ty);
                }

                self.emit(Instruction::InlineAsm {
                    template: template.clone(),
                    outputs: ir_outputs,
                    inputs: ir_inputs,
                    clobbers: clobbers.clone(),
                    operand_types,
                });
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

            let desig_name = match item.designators.first() {
                Some(Designator::Field(ref name)) => Some(name.as_str()),
                _ => None,
            };
            // Check for array index designator after field designator: .field[idx]
            let array_start_idx: Option<usize> = if desig_name.is_some() {
                item.designators.iter().find_map(|d| {
                    if let Designator::Index(ref idx_expr) = d {
                        self.eval_const_expr_for_designator(idx_expr)
                    } else {
                        None
                    }
                })
            } else {
                // Bare [idx] designator on current field
                match item.designators.first() {
                    Some(Designator::Index(ref idx_expr)) => {
                        self.eval_const_expr_for_designator(idx_expr)
                    }
                    _ => None,
                }
            };
            let field_idx = match layout.resolve_init_field_idx(desig_name, current_field_idx) {
                Some(idx) => idx,
                None => break,
            };
            let field = &layout.fields[field_idx].clone();
            let field_offset = base_offset + field.offset;

            // Handle nested designators (e.g., .a.j = 2): drill into sub-struct
            let has_nested_designator = item.designators.len() > 1
                && matches!(item.designators.first(), Some(Designator::Field(_)));

            match &field.ty {
                CType::Struct(st) if has_nested_designator => {
                    // Nested designator like .a.j = 2: create sub-item with remaining designators
                    let sub_layout = StructLayout::for_struct(&st.fields);
                    let sub_item = InitializerItem {
                        designators: item.designators[1..].to_vec(),
                        init: item.init.clone(),
                    };
                    self.emit_struct_init(&[sub_item], base_alloca, &sub_layout, field_offset);
                    item_idx += 1;
                }
                CType::Union(st) if has_nested_designator => {
                    let sub_layout = StructLayout::for_union(&st.fields);
                    let sub_item = InitializerItem {
                        designators: item.designators[1..].to_vec(),
                        init: item.init.clone(),
                    };
                    self.emit_struct_init(&[sub_item], base_alloca, &sub_layout, field_offset);
                    item_idx += 1;
                }
                CType::Array(elem_ty, Some(arr_size)) if has_nested_designator => {
                    // .field[idx] = val: resolve index and store element, then
                    // continue consuming subsequent non-designated items for the
                    // remaining array positions (C11 6.7.9p17).
                    let elem_size = elem_ty.size();
                    let elem_ir_ty = IrType::from_ctype(elem_ty);
                    let arr_size = *arr_size;

                    // Find the first index designator in remaining designators
                    let remaining = &item.designators[1..];
                    let (first_idx_pos, idx) = remaining.iter().enumerate().find_map(|(i, d)| {
                        if let Designator::Index(ref idx_expr) = d {
                            self.eval_const_expr(idx_expr).and_then(|c| c.to_usize()).map(|v| (i, v))
                        } else {
                            None
                        }
                    }).unwrap_or((0, 0));

                    if idx < arr_size {
                        let elem_offset = field_offset + idx * elem_size;

                        // Check for further designators after the first Index
                        let after_first_idx = &remaining[first_idx_pos + 1..];
                        let remaining_field_desigs: Vec<_> = after_first_idx.iter()
                            .filter(|d| matches!(d, Designator::Field(_)))
                            .cloned()
                            .collect();
                        let remaining_index_desigs: Vec<_> = after_first_idx.iter()
                            .filter(|d| matches!(d, Designator::Index(_)))
                            .cloned()
                            .collect();

                        if !remaining_field_desigs.is_empty() {
                            // .a[idx].b = val - drill into struct element
                            if let CType::Struct(ref st) = elem_ty.as_ref() {
                                let sub_layout = StructLayout::for_struct(&st.fields);
                                // Include any remaining index designators for nested arrays
                                let sub_desigs: Vec<_> = after_first_idx.iter().cloned().collect();
                                let sub_item = InitializerItem {
                                    designators: sub_desigs,
                                    init: item.init.clone(),
                                };
                                self.emit_struct_init(&[sub_item], base_alloca, &sub_layout, elem_offset);
                            }
                        } else if !remaining_index_desigs.is_empty() {
                            // Multi-dimensional array: .a[1][2] = val
                            if let CType::Array(inner_elem_ty, Some(inner_size)) = elem_ty.as_ref() {
                                let inner_idx = remaining_index_desigs.iter().find_map(|d| {
                                    if let Designator::Index(ref idx_expr) = d {
                                        self.eval_const_expr(idx_expr).and_then(|c| c.to_usize())
                                    } else {
                                        None
                                    }
                                }).unwrap_or(0);
                                if inner_idx < *inner_size {
                                    let inner_elem_size = inner_elem_ty.size();
                                    let inner_ir_ty = IrType::from_ctype(inner_elem_ty);
                                    let inner_offset = elem_offset + inner_idx * inner_elem_size;
                                    if let Initializer::Expr(e) = &item.init {
                                        if let Expr::StringLiteral(s, _) = e {
                                            if matches!(inner_elem_ty.as_ref(), CType::Char | CType::UChar) {
                                                self.emit_string_to_alloca(base_alloca, s, inner_offset);
                                            }
                                        } else {
                                            let expr_ty = self.get_expr_type(e);
                                            let val = self.lower_expr(e);
                                            let val = self.emit_implicit_cast(val, expr_ty, inner_ir_ty);
                                            let addr = self.fresh_value();
                                            self.emit(Instruction::GetElementPtr {
                                                dest: addr,
                                                base: base_alloca,
                                                offset: Operand::Const(IrConst::I64(inner_offset as i64)),
                                                ty: inner_ir_ty,
                                            });
                                            self.emit(Instruction::Store { val, ptr: addr, ty: inner_ir_ty });
                                        }
                                    }
                                }
                            }
                        } else if let Initializer::Expr(e) = &item.init {
                            // No further designators - handle string literal for char array
                            if let Expr::StringLiteral(s, _) = e {
                                if let CType::Array(inner, Some(_inner_size)) = elem_ty.as_ref() {
                                    if matches!(inner.as_ref(), CType::Char | CType::UChar) {
                                        self.emit_string_to_alloca(base_alloca, s, elem_offset);
                                    }
                                } else {
                                    let expr_ty = self.get_expr_type(e);
                                    let val = self.lower_expr(e);
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
                            } else {
                                let expr_ty = self.get_expr_type(e);
                                let val = self.lower_expr(e);
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
                        } else if let Initializer::List(sub_items) = &item.init {
                            // Handle list init for array element (e.g., .a[1] = {1,2,3})
                            match elem_ty.as_ref() {
                                CType::Array(inner_elem_ty, Some(inner_size)) => {
                                    let inner_elem_size = inner_elem_ty.size();
                                    let inner_ir_ty = IrType::from_ctype(inner_elem_ty);
                                    let mut si = 0;
                                    for sub_item in sub_items.iter() {
                                        if si >= *inner_size { break; }
                                        if let Initializer::Expr(e) = &sub_item.init {
                                            let expr_ty = self.get_expr_type(e);
                                            let val = self.lower_expr(e);
                                            let val = self.emit_implicit_cast(val, expr_ty, inner_ir_ty);
                                            let inner_offset = elem_offset + si * inner_elem_size;
                                            let addr = self.fresh_value();
                                            self.emit(Instruction::GetElementPtr {
                                                dest: addr,
                                                base: base_alloca,
                                                offset: Operand::Const(IrConst::I64(inner_offset as i64)),
                                                ty: inner_ir_ty,
                                            });
                                            self.emit(Instruction::Store { val, ptr: addr, ty: inner_ir_ty });
                                        }
                                        si += 1;
                                    }
                                }
                                CType::Struct(ref st) => {
                                    let sub_layout = StructLayout::for_struct(&st.fields);
                                    self.emit_struct_init(sub_items, base_alloca, &sub_layout, elem_offset);
                                }
                                _ => {}
                            }
                        }
                    }
                    item_idx += 1;
                    // Continue consuming subsequent non-designated items for array
                    // positions idx+1, idx+2, ... (C11 6.7.9p17: initialization
                    // continues in order from the next element after the designated one)
                    let mut ai = idx + 1;
                    while ai < arr_size && item_idx < items.len() {
                        let next_item = &items[item_idx];
                        // Stop if the next item has any designator
                        if !next_item.designators.is_empty() {
                            break;
                        }
                        if let Initializer::Expr(e) = &next_item.init {
                            let expr_ty = self.get_expr_type(e);
                            let val = self.lower_expr(e);
                            let val = self.emit_implicit_cast(val, expr_ty, elem_ir_ty);
                            let elem_offset = field_offset + ai * elem_size;
                            let addr = self.fresh_value();
                            self.emit(Instruction::GetElementPtr {
                                dest: addr,
                                base: base_alloca,
                                offset: Operand::Const(IrConst::I64(elem_offset as i64)),
                                ty: elem_ir_ty,
                            });
                            self.emit(Instruction::Store { val, ptr: addr, ty: elem_ir_ty });
                        } else {
                            // Sub-list initializer - stop flat continuation
                            break;
                        }
                        item_idx += 1;
                        ai += 1;
                    }
                    // Stay on the same field (the array) rather than advancing,
                    // since we already consumed the continuation items above.
                    // current_field_idx is updated at line end; we want it to
                    // point past this array field if we exhausted the continuation.
                }
                CType::Struct(st) => {
                    // Nested struct field
                    let sub_layout = StructLayout::for_struct(&st.fields);
                    match &item.init {
                        Initializer::List(sub_items) => {
                            // Nested braces: { {10, 20}, 30 } - the sub_items init the inner struct
                            // Zero-init the sub-struct area if it has designators or partial init,
                            // so that non-designated fields are properly zeroed.
                            let has_desig = sub_items.iter().any(|si| !si.designators.is_empty());
                            if has_desig || sub_items.len() < sub_layout.fields.len() {
                                self.zero_init_region(base_alloca, field_offset, sub_layout.size);
                            }
                            self.emit_struct_init(sub_items, base_alloca, &sub_layout, field_offset);
                            item_idx += 1;
                        }
                        Initializer::Expr(expr) => {
                            if self.struct_value_size(expr).is_some() {
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
                                if consumed == 0 { item_idx += 1; } else { item_idx += consumed; }
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
                            if self.struct_value_size(expr).is_some() {
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
                                if consumed == 0 { item_idx += 1; } else { item_idx += consumed; }
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
                            if let CType::Struct(ref st) = elem_ty.as_ref() {
                                // Array of structs: each sub_item inits one struct element
                                let sub_layout = StructLayout::for_struct(&st.fields);
                                let mut ai = 0;
                                let mut si = 0;
                                while si < sub_items.len() && ai < *arr_size {
                                    let elem_offset = field_offset + ai * elem_size;
                                    match &sub_items[si].init {
                                        Initializer::List(struct_items) => {
                                            self.emit_struct_init(struct_items, base_alloca, &sub_layout, elem_offset);
                                            si += 1;
                                            ai += 1;
                                        }
                                        Initializer::Expr(e) => {
                                            if self.struct_value_size(e).is_some() {
                                                let src_addr = self.get_struct_base_addr(e);
                                                let dest_addr = self.fresh_value();
                                                self.emit(Instruction::GetElementPtr {
                                                    dest: dest_addr,
                                                    base: base_alloca,
                                                    offset: Operand::Const(IrConst::I64(elem_offset as i64)),
                                                    ty: IrType::Ptr,
                                                });
                                                self.emit(Instruction::Memcpy {
                                                    dest: dest_addr,
                                                    src: src_addr,
                                                    size: sub_layout.size,
                                                });
                                                si += 1;
                                                ai += 1;
                                            } else {
                                                // Flat scalars filling struct fields across array elements
                                                let consumed = self.emit_struct_init(&sub_items[si..], base_alloca, &sub_layout, elem_offset);
                                                si += consumed;
                                                ai += 1;
                                            }
                                        }
                                    }
                                }
                            } else {
                                // Supports [idx]=val designators within the sub-list
                                let mut ai = 0usize;
                                for sub_item in sub_items.iter() {
                                    // Check for index designator: [idx]=val
                                    if let Some(Designator::Index(ref idx_expr)) = sub_item.designators.first() {
                                        if let Some(idx) = self.eval_const_expr_for_designator(idx_expr) {
                                            ai = idx;
                                        }
                                    }
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
                                    } else if let Initializer::List(inner_items) = &sub_item.init {
                                        // Handle braced sub-init for array elements (e.g., int arr[2][3] = {{1,2,3},{4,5,6}})
                                        if let CType::Array(inner_elem_ty, Some(inner_size)) = elem_ty.as_ref() {
                                            let inner_elem_ir_ty = IrType::from_ctype(inner_elem_ty);
                                            let inner_elem_size = inner_elem_ty.size();
                                            for (ii, inner_item) in inner_items.iter().enumerate() {
                                                if ii >= *inner_size { break; }
                                                if let Initializer::Expr(e) = &inner_item.init {
                                                    let expr_ty = self.get_expr_type(e);
                                                    let val = self.lower_expr(e);
                                                    let val = self.emit_implicit_cast(val, expr_ty, inner_elem_ir_ty);
                                                    let inner_offset = elem_offset + ii * inner_elem_size;
                                                    let addr = self.fresh_value();
                                                    self.emit(Instruction::GetElementPtr {
                                                        dest: addr,
                                                        base: base_alloca,
                                                        offset: Operand::Const(IrConst::I64(inner_offset as i64)),
                                                        ty: inner_elem_ir_ty,
                                                    });
                                                    self.emit(Instruction::Store { val, ptr: addr, ty: inner_elem_ir_ty });
                                                }
                                            }
                                        }
                                    }
                                    ai += 1;
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
                            if let CType::Struct(ref st) = elem_ty.as_ref() {
                                // Flat init for array of structs: consume items for each
                                // struct element's fields across the array
                                let sub_layout = StructLayout::for_struct(&st.fields);
                                let start_ai = array_start_idx.unwrap_or(0);
                                let mut total_consumed = 0usize;
                                for ai in start_ai..*arr_size {
                                    if item_idx + total_consumed >= items.len() { break; }
                                    let elem_offset = field_offset + ai * elem_size;
                                    let consumed = self.emit_struct_init(&items[item_idx + total_consumed..], base_alloca, &sub_layout, elem_offset);
                                    total_consumed += consumed;
                                }
                                item_idx += total_consumed.max(1);
                            } else {
                                // Flat init: consume up to arr_size items from the init list
                                // to fill the array elements (e.g., struct { int a[3]; int b; } x = {1,2,3,4};)
                                // If array_start_idx is set (e.g., .field[3] = val), start from that index.
                                let elem_ir_ty = IrType::from_ctype(elem_ty);
                                let start_ai = array_start_idx.unwrap_or(0);
                                let mut consumed = 0usize;
                                let mut ai = start_ai;
                                while ai < *arr_size && (item_idx + consumed) < items.len() {
                                    let cur_item = &items[item_idx + consumed];
                                    // Stop if we hit a designator (which targets a different field/index)
                                    if !cur_item.designators.is_empty() && consumed > 0 { break; }
                                    if let Initializer::Expr(expr) = &cur_item.init {
                                        let expr_ty = self.get_expr_type(expr);
                                        let val = self.lower_expr(expr);
                                        let val = self.emit_implicit_cast(val, expr_ty, elem_ir_ty);
                                        let elem_offset = field_offset + ai * elem_size;
                                        let addr = self.fresh_value();
                                        self.emit(Instruction::GetElementPtr {
                                            dest: addr,
                                            base: base_alloca,
                                            offset: Operand::Const(IrConst::I64(elem_offset as i64)),
                                            ty: elem_ir_ty,
                                        });
                                        self.emit(Instruction::Store { val, ptr: addr, ty: elem_ir_ty });
                                        consumed += 1;
                                        ai += 1;
                                    } else {
                                        // Hit a nested List - stop flat consumption
                                        break;
                                    }
                                }
                                item_idx += consumed.max(1);
                            }
                        }
                    }
                }
                // Complex field: lower expression and memcpy data
                CType::ComplexFloat | CType::ComplexDouble | CType::ComplexLongDouble => {
                    let complex_ctype = field.ty.clone();
                    let complex_size = complex_ctype.size();
                    let dest_addr = self.fresh_value();
                    self.emit(Instruction::GetElementPtr {
                        dest: dest_addr,
                        base: base_alloca,
                        offset: Operand::Const(IrConst::I64(field_offset as i64)),
                        ty: IrType::Ptr,
                    });
                    match &item.init {
                        Initializer::Expr(e) => {
                            let expr_ctype = self.expr_ctype(e);
                            let val = self.lower_expr(e);
                            let src = if expr_ctype.is_complex() {
                                // Already complex: might need conversion
                                if expr_ctype != complex_ctype {
                                    let val_v = self.operand_to_value(val);
                                    let converted = self.complex_to_complex(val_v, &expr_ctype, &complex_ctype);
                                    self.operand_to_value(converted)
                                } else {
                                    self.operand_to_value(val)
                                }
                            } else {
                                // Real/integer scalar: convert to complex {val, 0}
                                let converted = self.real_to_complex(val, &expr_ctype, &complex_ctype);
                                self.operand_to_value(converted)
                            };
                            self.emit(Instruction::Memcpy {
                                dest: dest_addr,
                                src,
                                size: complex_size,
                            });
                        }
                        Initializer::List(sub_items) => {
                            // {real, imag} list init
                            let comp_ty = Self::complex_component_ir_type(&complex_ctype);
                            let comp_size = Self::complex_component_size(&complex_ctype);
                            // Store real part
                            if let Some(item) = sub_items.first() {
                                if let Initializer::Expr(e) = &item.init {
                                    let val = self.lower_expr(e);
                                    let expr_ty = self.get_expr_type(e);
                                    let val = self.emit_implicit_cast(val, expr_ty, comp_ty);
                                    self.emit(Instruction::Store { val, ptr: dest_addr, ty: comp_ty });
                                }
                            } else {
                                let zero = match comp_ty {
                                    IrType::F32 => Operand::Const(IrConst::F32(0.0)),
                                    _ => Operand::Const(IrConst::F64(0.0)),
                                };
                                self.emit(Instruction::Store { val: zero, ptr: dest_addr, ty: comp_ty });
                            }
                            // Store imag part
                            let imag_ptr = self.fresh_value();
                            self.emit(Instruction::GetElementPtr {
                                dest: imag_ptr,
                                base: dest_addr,
                                offset: Operand::Const(IrConst::I64(comp_size as i64)),
                                ty: IrType::I8,
                            });
                            if let Some(item) = sub_items.get(1) {
                                if let Initializer::Expr(e) = &item.init {
                                    let val = self.lower_expr(e);
                                    let expr_ty = self.get_expr_type(e);
                                    let val = self.emit_implicit_cast(val, expr_ty, comp_ty);
                                    self.emit(Instruction::Store { val, ptr: imag_ptr, ty: comp_ty });
                                }
                            } else {
                                let zero = match comp_ty {
                                    IrType::F32 => Operand::Const(IrConst::F32(0.0)),
                                    _ => Operand::Const(IrConst::F64(0.0)),
                                };
                                self.emit(Instruction::Store { val: zero, ptr: imag_ptr, ty: comp_ty });
                            }
                        }
                    }
                    item_idx += 1;
                }
                _ => {
                    // Scalar field (possibly a bitfield)
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
                    // Bitfield fields need read-modify-write instead of plain store
                    if let (Some(bit_offset), Some(bit_width)) = (field.bit_offset, field.bit_width) {
                        self.store_bitfield(addr, field_ty, bit_offset, bit_width, val);
                    } else {
                        self.emit(Instruction::Store { val, ptr: addr, ty: field_ty });
                    }
                    item_idx += 1;
                }
            }
            current_field_idx = field_idx + 1;
        }
        item_idx
    }

    /// Compute the runtime sizeof for a VLA local variable.
    /// Returns Some(Value) if any array dimension is a non-constant expression.
    /// The Value holds the total byte size (product of all dimensions * element_size).
    /// Returns None if all dimensions are compile-time constants.
    pub(super) fn compute_vla_runtime_size(
        &mut self,
        type_spec: &TypeSpecifier,
        derived: &[DerivedDeclarator],
    ) -> Option<Value> {
        // Collect array dimensions from derived declarators
        let array_dims: Vec<&Option<Box<Expr>>> = derived.iter().filter_map(|d| {
            if let DerivedDeclarator::Array(size) = d {
                Some(size)
            } else {
                None
            }
        }).collect();

        if array_dims.is_empty() {
            // Check if the type_spec itself is an Array (typedef'd VLA)
            return self.compute_vla_size_from_type_spec(type_spec);
        }

        // Check if any dimension is non-constant
        let mut has_vla = false;
        for dim in &array_dims {
            if let Some(expr) = dim {
                if self.expr_as_array_size(expr).is_none() {
                    has_vla = true;
                    break;
                }
            }
        }

        if !has_vla {
            return None; // All dimensions are compile-time constants
        }

        // Compute element size (the base type size)
        let resolved = self.resolve_type_spec(type_spec);
        let base_elem_size = self.sizeof_type(&resolved);

        // Build runtime product: dim0 * dim1 * ... * base_elem_size
        let mut result: Option<Value> = None;
        let mut const_product: usize = base_elem_size;

        for dim in &array_dims {
            if let Some(expr) = dim {
                if let Some(const_val) = self.expr_as_array_size(expr) {
                    // Constant dimension - accumulate
                    const_product *= const_val as usize;
                } else {
                    // Runtime dimension - emit multiplication
                    let dim_val = self.lower_expr(expr);
                    let dim_value = self.operand_to_value(dim_val);

                    result = if let Some(prev) = result {
                        let mul = self.emit_binop_val(IrBinOp::Mul, Operand::Value(prev), Operand::Value(dim_value), IrType::I64);
                        Some(mul)
                    } else {
                        // First runtime dim: multiply by accumulated constants
                        if const_product > 1 {
                            let mul = self.emit_binop_val(IrBinOp::Mul, Operand::Value(dim_value), Operand::Const(IrConst::I64(const_product as i64)), IrType::I64);
                            const_product = 1;
                            Some(mul)
                        } else {
                            Some(dim_value)
                        }
                    };
                }
            }
        }

        // If we have remaining constant factors, multiply them in
        if let Some(prev) = result {
            if const_product > 1 {
                let mul = self.emit_binop_val(IrBinOp::Mul, Operand::Value(prev), Operand::Const(IrConst::I64(const_product as i64)), IrType::I64);
                return Some(mul);
            }
            return Some(prev);
        }

        None
    }

    /// Compute VLA size from a typedef'd array type (e.g., typedef char buf[n]).
    fn compute_vla_size_from_type_spec(&mut self, type_spec: &TypeSpecifier) -> Option<Value> {
        let resolved = self.resolve_type_spec(type_spec).clone();
        match &resolved {
            TypeSpecifier::Array(elem, Some(size_expr)) => {
                if self.expr_as_array_size(size_expr).is_some() {
                    return None; // Constant size
                }
                // Clone what we need before mutable borrow
                let size_expr_clone = size_expr.clone();
                let elem_size = self.sizeof_type(elem);
                // Runtime size expression
                let dim_val = self.lower_expr(&size_expr_clone);
                let dim_value = self.operand_to_value(dim_val);
                if elem_size > 1 {
                    let mul = self.emit_binop_val(IrBinOp::Mul, Operand::Value(dim_value), Operand::Const(IrConst::I64(elem_size as i64)), IrType::I64);
                    Some(mul)
                } else {
                    Some(dim_value)
                }
            }
            _ => None,
        }
    }
}
