use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, CType, StructLayout};
use super::lowering::{Lowerer, LocalInfo, GlobalInfo, DeclAnalysis, SwitchFrame, FuncSig, FunctionTypedefInfo, resolve_typedef_derived};

impl Lowerer {
    pub(super) fn lower_compound_stmt(&mut self, compound: &CompoundStmt) {
        // Check if this block contains any declarations. If not, we can skip
        // scope tracking entirely since statements don't introduce new bindings.
        let has_declarations = compound.items.iter().any(|item| matches!(item, BlockItem::Declaration(_)));

        if has_declarations {
            // Push a scope frame to track additions/modifications in this block.
            // On scope exit, we undo changes instead of cloning entire HashMaps.
            self.push_scope();

            for item in &compound.items {
                match item {
                    BlockItem::Declaration(decl) => {
                        self.collect_enum_constants_scoped(&decl.type_spec);
                        self.lower_local_decl(decl);
                    }
                    BlockItem::Statement(stmt) => self.lower_stmt(stmt),
                }
            }

            self.pop_scope();
        } else {
            // No declarations: just lower statements without scope overhead.
            for item in &compound.items {
                if let BlockItem::Statement(stmt) = item {
                    self.lower_stmt(stmt);
                }
            }
        }
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
                is_common: decl.is_common,
                alignment: decl.alignment,
                span: decl.span,
            };
            &resolved_decl
        } else {
            decl
        };

        self.register_struct_type(&decl.type_spec);

        if decl.is_typedef {
            for declarator in &decl.declarators {
                if !declarator.name.is_empty() {
                    let resolved_type = resolve_typedef_derived(&decl.type_spec, &declarator.derived);
                    self.types.typedefs.insert(declarator.name.clone(), resolved_type);

                    // Track function pointer typedefs for correct CType resolution
                    let has_fptr_derived = declarator.derived.iter().any(|d|
                        matches!(d, DerivedDeclarator::FunctionPointer(_, _)));
                    if has_fptr_derived {
                        if let Some(DerivedDeclarator::FunctionPointer(params, variadic)) =
                            declarator.derived.iter().find(|d| matches!(d, DerivedDeclarator::FunctionPointer(_, _)))
                        {
                            let mut return_type = decl.type_spec.clone();
                            for d in &declarator.derived {
                                match d {
                                    DerivedDeclarator::Pointer => {
                                        return_type = TypeSpecifier::Pointer(Box::new(return_type));
                                    }
                                    DerivedDeclarator::FunctionPointer(_, _) => break,
                                    _ => break,
                                }
                            }
                            self.types.func_ptr_typedefs.insert(declarator.name.clone());
                            self.types.func_ptr_typedef_info.insert(declarator.name.clone(), FunctionTypedefInfo {
                                return_type,
                                params: params.clone(),
                                variadic: *variadic,
                            });
                        }
                    }
                }
            }
            return;
        }

        for declarator in &decl.declarators {
            if declarator.name.is_empty() {
                continue;
            }

            // Extern declarations reference a global symbol, not a local variable
            if decl.is_extern {
                if self.lower_extern_decl(decl, declarator) {
                    continue;
                }
                // Fall through to function declaration handler if it was an extern func decl
            }

            // Block-scope function declarations: `int f(int);` or typedef-based `func_t add;`
            if self.try_lower_block_func_decl(decl, declarator) {
                continue;
            }

            // Shared declaration analysis
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

            if decl.is_static {
                self.lower_local_static_decl(&decl, &declarator, &da);
                continue;
            }

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
                align: decl.alignment.unwrap_or(0),
            });
            let mut local_info = LocalInfo::from_analysis(&da, alloca);
            local_info.vla_size = vla_size;
            self.insert_local_scoped(declarator.name.clone(), local_info);

            // Track function pointer return and param types
            for d in &declarator.derived {
                if let DerivedDeclarator::FunctionPointer(params, _) = d {
                    let ret_ty = self.type_spec_to_ir(&decl.type_spec);
                    let param_tys: Vec<IrType> = params.iter().map(|p| {
                        self.type_spec_to_ir(&p.type_spec)
                    }).collect();
                    self.func_meta.ptr_sigs.insert(declarator.name.clone(), FuncSig {
                        return_type: ret_ty,
                        return_ctype: None,
                        param_types: param_tys,
                        param_ctypes: Vec::new(),
                        param_bool_flags: Vec::new(),
                        is_variadic: false,
                        sret_size: None,
                        two_reg_ret_size: None,
                        param_struct_sizes: Vec::new(),
                    });
                    break;
                }
            }

            if let Some(ref init) = declarator.init {
                match init {
                    Initializer::Expr(expr) => {
                        // Track const values before lowering (needed for VLA sizes)
                        if decl.is_const && !da.is_pointer && !da.is_array && !da.is_struct && !is_complex {
                            if let Some(const_val) = self.eval_const_expr(expr) {
                                if let Some(ival) = self.const_to_i64(&const_val) {
                                    self.insert_const_local_scoped(declarator.name.clone(), ival);
                                }
                            }
                        }
                        self.lower_local_init_expr(expr, alloca, &da, is_complex, decl);
                    }
                    Initializer::List(items) => {
                        self.lower_local_init_list(
                            items, alloca, &da, is_complex, &complex_elem_ctype,
                            decl, &declarator.name,
                        );
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
        let static_name = format!("{}.{}.{}", self.func_mut().name, declarator.name, static_id);

        // Register the bare name -> mangled name mapping before processing the initializer
        // so that &x in another static's initializer can resolve to the mangled name.
        self.insert_static_local_scoped(declarator.name.clone(), static_name.clone());

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

        self.emitted_global_names.insert(static_name.clone());
        self.module.globals.push(IrGlobal {
            name: static_name.clone(),
            ty: global_ty,
            size: da.actual_alloc_size,
            align,
            init,
            is_static: true,
            is_extern: false,
            is_common: false,
        });

        // Track as a global for access via GlobalAddr
        self.globals.insert(static_name.clone(), GlobalInfo::from_analysis(da));

        // Store type info in locals (with static_global_name set so each use site
        // emits a fresh GlobalAddr in its own basic block, avoiding unreachable-block issues).
        self.insert_local_scoped(declarator.name.clone(), LocalInfo::for_static(da, static_name));
        self.next_static_local += 1;
    }

    /// Lower a {real, imag} list initializer for a complex field.
    /// Stores the real and imaginary parts at dest_addr and dest_addr+comp_size.
    fn lower_complex_list_init(
        &mut self,
        sub_items: &[InitializerItem],
        dest_addr: Value,
        complex_ctype: &CType,
    ) {
        let comp_ty = Self::complex_component_ir_type(complex_ctype);
        let comp_size = Self::complex_component_size(complex_ctype);
        // Store real part
        if let Some(first) = sub_items.first() {
            if let Initializer::Expr(e) = &first.init {
                let val = self.lower_expr(e);
                let expr_ty = self.get_expr_type(e);
                let val = self.emit_implicit_cast(val, expr_ty, comp_ty);
                self.emit(Instruction::Store { val, ptr: dest_addr, ty: comp_ty });
            }
        } else {
            let zero = Self::complex_zero(comp_ty);
            self.emit(Instruction::Store { val: zero, ptr: dest_addr, ty: comp_ty });
        }
        // Store imag part
        let imag_ptr = self.emit_gep_offset(dest_addr, comp_size, IrType::I8);
        if let Some(item) = sub_items.get(1) {
            if let Initializer::Expr(e) = &item.init {
                let val = self.lower_expr(e);
                let expr_ty = self.get_expr_type(e);
                let val = self.emit_implicit_cast(val, expr_ty, comp_ty);
                self.emit(Instruction::Store { val, ptr: imag_ptr, ty: comp_ty });
            }
        } else {
            let zero = Self::complex_zero(comp_ty);
            self.emit(Instruction::Store { val: zero, ptr: imag_ptr, ty: comp_ty });
        }
    }

    /// Lower an expression to a complex value, converting if needed.
    /// Returns a Value (pointer to the complex {real, imag} pair).
    pub(super) fn lower_expr_to_complex(&mut self, expr: &Expr, target_ctype: &CType) -> Value {
        let expr_ctype = self.expr_ctype(expr);
        let val = self.lower_expr(expr);
        if expr_ctype.is_complex() {
            if expr_ctype != *target_ctype {
                let val_v = self.operand_to_value(val);
                let converted = self.complex_to_complex(val_v, &expr_ctype, target_ctype);
                self.operand_to_value(converted)
            } else {
                self.operand_to_value(val)
            }
        } else {
            let converted = self.real_to_complex(val, &expr_ctype, target_ctype);
            self.operand_to_value(converted)
        }
    }

    /// Initialize a local struct from an initializer list.
    /// `base` is the base address of the struct in memory.
    pub(super) fn lower_local_struct_init(
        &mut self,
        items: &[InitializerItem],
        base: Value,
        layout: &StructLayout,
    ) {
        use crate::common::types::InitFieldResolution;
        let mut current_field_idx = 0usize;
        let mut item_idx = 0usize;
        while item_idx < items.len() {
            let item = &items[item_idx];
            let desig_name = match item.designators.first() {
                Some(Designator::Field(ref name)) => Some(name.as_str()),
                _ => None,
            };
            let resolution = match layout.resolve_init_field(desig_name, current_field_idx, &self.types) {
                Some(r) => r,
                None => break,
            };

            // Handle anonymous member: drill into the anonymous struct/union
            let field_idx = match &resolution {
                InitFieldResolution::Direct(idx) => *idx,
                InitFieldResolution::AnonymousMember { anon_field_idx, inner_name } => {
                    let anon_field = &layout.fields[*anon_field_idx].clone();
                    let anon_offset = anon_field.offset;
                    let sub_layout = match &anon_field.ty {
                        CType::Struct(key) | CType::Union(key) => {
                            match self.types.struct_layouts.get(key) {
                                Some(l) => l.clone(),
                                None => { current_field_idx = *anon_field_idx + 1; item_idx += 1; continue; }
                            }
                        }
                        _ => { current_field_idx = *anon_field_idx + 1; item_idx += 1; continue; }
                    };
                    // Create a synthetic item with the inner designator
                    let sub_item = InitializerItem {
                        designators: vec![Designator::Field(inner_name.clone())],
                        init: item.init.clone(),
                    };
                    let sub_base = self.emit_gep_offset(base, anon_offset, IrType::Ptr);
                    self.lower_local_struct_init(&[sub_item], sub_base, &sub_layout);
                    current_field_idx = *anon_field_idx + 1;
                    item_idx += 1;
                    continue;
                }
            };

            let field = &layout.fields[field_idx];
            let field_offset = field.offset;

            // Complex fields need special handling: memcpy data instead of storing pointer
            if field.ty.is_complex() {
                let complex_ctype = field.ty.clone();
                let complex_size = complex_ctype.size();
                let dest_addr = self.emit_gep_offset(base, field_offset, IrType::Ptr);
                match &item.init {
                    Initializer::Expr(e) => {
                        let src = self.lower_expr_to_complex(e, &complex_ctype);
                        self.emit(Instruction::Memcpy {
                            dest: dest_addr,
                            src,
                            size: complex_size,
                        });
                    }
                    Initializer::List(sub_items) => {
                        self.lower_complex_list_init(sub_items, dest_addr, &complex_ctype);
                    }
                }
                current_field_idx = field_idx + 1;
                item_idx += 1;
                continue;
            }

            let field_ty = IrType::from_ctype(&field.ty);

            match &item.init {
                Initializer::Expr(e) => {
                    // For struct/union fields initialized from expressions that produce
                    // struct values (e.g., function calls returning structs via sret),
                    // we need to emit a memcpy rather than a scalar store, because
                    // lower_expr returns the sret alloca address (a pointer), not the
                    // struct data itself.
                    let is_struct_field = matches!(field.ty, CType::Struct(_) | CType::Union(_));
                    if is_struct_field && self.struct_value_size(e).is_some() {
                        let src_addr = self.get_struct_base_addr(e);
                        let field_size = field.ty.size();
                        let field_addr = self.emit_gep_offset(base, field_offset, IrType::Ptr);
                        self.emit(Instruction::Memcpy {
                            dest: field_addr,
                            src: src_addr,
                            size: field_size,
                        });
                    } else if let CType::Array(ref elem_ty, Some(arr_size)) = field.ty {
                        // Char array field initialized by a string literal:
                        // copy the string bytes instead of storing the pointer.
                        let is_char_array = matches!(**elem_ty, CType::Char | CType::UChar);
                        if is_char_array {
                            if let Expr::StringLiteral(ref s, _) = e {
                                self.emit_string_to_alloca(base, s, field_offset);
                                // Zero-fill remaining bytes if string is shorter than array
                                let str_len = s.chars().count() + 1; // +1 for null terminator
                                for i in str_len..arr_size {
                                    let val = Operand::Const(IrConst::I8(0));
                                    self.emit_store_at_offset(base, field_offset + i, val, IrType::I8);
                                }
                            } else {
                                let val = self.lower_and_cast_init_expr(e, field_ty);
                                let field_addr = self.emit_gep_offset(base, field_offset, field_ty);
                                self.emit(Instruction::Store { val, ptr: field_addr, ty: field_ty });
                            }
                        } else {
                            // Non-char array field with flat expression initializer:
                            // consume up to arr_size items from the init list to fill array elements
                            let elem_ir_ty = IrType::from_ctype(elem_ty);
                            let elem_size = elem_ir_ty.size().max(1);
                            let val = self.lower_and_cast_init_expr(e, elem_ir_ty);
                            let field_addr = self.emit_gep_offset(base, field_offset, elem_ir_ty);
                            self.emit(Instruction::Store { val, ptr: field_addr, ty: elem_ir_ty });
                            // Consume additional items for remaining array elements
                            let mut arr_idx = 1usize;
                            while arr_idx < arr_size && item_idx + 1 < items.len() {
                                item_idx += 1;
                                let next_item = &items[item_idx];
                                // Stop if we hit a designator (it targets a different field)
                                if !next_item.designators.is_empty() {
                                    item_idx -= 1; // put it back
                                    break;
                                }
                                if let Initializer::Expr(next_e) = &next_item.init {
                                    let next_val = self.lower_and_cast_init_expr(next_e, elem_ir_ty);
                                    let offset = field_offset + arr_idx * elem_size;
                                    let elem_addr = self.emit_gep_offset(base, offset, elem_ir_ty);
                                    self.emit(Instruction::Store { val: next_val, ptr: elem_addr, ty: elem_ir_ty });
                                }
                                arr_idx += 1;
                            }
                        }
                    } else {
                        let val = self.lower_and_cast_init_expr(e, field_ty);
                        let field_addr = self.emit_gep_offset(base, field_offset, field_ty);
                        if let (Some(bit_offset), Some(bit_width)) = (field.bit_offset, field.bit_width) {
                            self.store_bitfield(field_addr, field_ty, bit_offset, bit_width, val);
                        } else {
                            self.emit(Instruction::Store { val, ptr: field_addr, ty: field_ty });
                        }
                    }
                }
                Initializer::List(sub_items) => {
                    let field_addr = self.emit_gep_offset(base, field_offset, field_ty);
                    self.lower_struct_field_init_list(sub_items, field_addr, &field.ty);
                }
            }
            current_field_idx = field_idx + 1;
            item_idx += 1;
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
            CType::Struct(key) | CType::Union(key) => {
                if let Some(sub_layout) = self.types.struct_layouts.get(key).cloned() {
                    self.lower_local_struct_init(items, base, &sub_layout);
                }
            }
            CType::Array(ref elem_ty, arr_size_opt) => {
                // Check for char array initialized by a brace-wrapped string literal:
                // e.g., struct field `char a[10]` initialized as `{"hello"}`
                let is_char_array = matches!(**elem_ty, CType::Char | CType::UChar);
                if is_char_array && items.len() == 1 {
                    if let Initializer::Expr(Expr::StringLiteral(ref s, _)) = items[0].init {
                        self.emit_string_to_alloca(base, s, 0);
                        // Zero-fill remaining bytes if string is shorter than array
                        if let Some(arr_size) = arr_size_opt {
                            let str_len = s.chars().count() + 1; // +1 for null terminator
                            for i in str_len..*arr_size {
                                let val = Operand::Const(IrConst::I8(0));
                                self.emit_store_at_offset(base, i, val, IrType::I8);
                            }
                        }
                        return;
                    }
                }
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
                    match &item.init {
                        Initializer::Expr(e) => {
                            let val = self.lower_and_cast_init_expr(e, elem_ir_ty);
                            self.emit_store_at_offset(base, ai * elem_size, val, elem_ir_ty);
                        }
                        Initializer::List(sub_items) => {
                            // Braced sub-initializer for array element (e.g., struct element)
                            let elem_addr = self.emit_gep_offset(base, ai * elem_size, elem_ir_ty);
                            self.lower_struct_field_init_list(sub_items, elem_addr, elem_ty);
                        }
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
        let base_type_size = base_ty.size().max(1);
        let sub_elem_count = if array_dim_strides.len() > 1 && elem_size > 0 {
            array_dim_strides[0] / elem_size
        } else {
            1
        };

        for item in items {
            // Check for multi-dimensional index designators: [i][j]...
            let index_designators: Vec<usize> = item.designators.iter().filter_map(|d| {
                if let Designator::Index(ref idx_expr) = d {
                    self.eval_const_expr_for_designator(idx_expr)
                } else {
                    None
                }
            }).collect();

            if !index_designators.is_empty() {
                // Compute flat scalar index from multi-dimensional indices
                let mut target_flat = 0usize;
                for (i, &idx) in index_designators.iter().enumerate() {
                    let elems_per_entry = if i < array_dim_strides.len() && base_type_size > 0 {
                        array_dim_strides[i] / base_type_size
                    } else {
                        1
                    };
                    target_flat += idx * elems_per_entry;
                }

                match &item.init {
                    Initializer::List(sub_items) => {
                        // Set flat_index to designated position and recurse
                        *flat_index = target_flat;
                        let remaining_dims = array_dim_strides.len().saturating_sub(index_designators.len());
                        let sub_strides = &array_dim_strides[array_dim_strides.len() - remaining_dims..];
                        if remaining_dims > 0 {
                            self.lower_array_init_recursive(sub_items, alloca, base_ty, sub_strides, flat_index);
                        } else {
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
                        if base_ty == IrType::I8 || base_ty == IrType::U8 {
                            if let Expr::StringLiteral(s, _) = e {
                                self.emit_string_to_alloca(alloca, s, target_flat * elem_size);
                                *flat_index = target_flat + sub_elem_count;
                                continue;
                            }
                        }
                        let val = self.lower_and_cast_init_expr(e, base_ty);
                        self.emit_array_element_store(alloca, val, target_flat * elem_size, base_ty);
                        *flat_index = target_flat + 1;
                    }
                }
                continue;
            }

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
                let op = expr.as_ref().map(|e| self.lower_return_expr(e));
                self.terminate(Terminator::Return(op));
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

                self.func_mut().break_labels.push(end_label.clone());
                self.func_mut().continue_labels.push(cond_label.clone());

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
                let label = self.func().continue_labels.last().unwrap().clone(); self.terminate(Terminator::Branch(label));

                self.func_mut().break_labels.pop();
                self.func_mut().continue_labels.pop();

                self.start_block(end_label);
            }
            Stmt::For(init, cond, inc, body, _span) => {
                // C99: for-init declarations have their own scope.
                let has_decl_init = init.as_ref().map_or(false, |i| matches!(i.as_ref(), ForInit::Declaration(_)));
                if has_decl_init {
                    self.push_scope();
                }

                // Init
                if let Some(init) = init {
                    match init.as_ref() {
                        ForInit::Declaration(decl) => {
                            self.collect_enum_constants_scoped(&decl.type_spec);
                            self.lower_local_decl(decl);
                        }
                        ForInit::Expr(expr) => { self.lower_expr(expr); },
                    }
                }

                let cond_label = self.fresh_label("for_cond");
                let body_label = self.fresh_label("for_body");
                let inc_label = self.fresh_label("for_inc");
                let end_label = self.fresh_label("for_end");

                self.func_mut().break_labels.push(end_label.clone());
                self.func_mut().continue_labels.push(inc_label.clone());

                self.terminate(Terminator::Branch(cond_label.clone()));

                // Condition
                self.start_block(cond_label.clone());
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
                self.terminate(Terminator::Branch(cond_label));

                self.func_mut().break_labels.pop();
                self.func_mut().continue_labels.pop();

                self.start_block(end_label);

                // Restore for-init scope
                if has_decl_init {
                    self.pop_scope();
                }
            }
            Stmt::DoWhile(body, cond, _span) => {
                let body_label = self.fresh_label("do_body");
                let cond_label = self.fresh_label("do_cond");
                let end_label = self.fresh_label("do_end");

                self.func_mut().break_labels.push(end_label.clone());
                self.func_mut().continue_labels.push(cond_label.clone());

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

                self.func_mut().break_labels.pop();
                self.func_mut().continue_labels.pop();

                self.start_block(end_label);
            }
            Stmt::Break(_span) => {
                if let Some(label) = self.func_mut().break_labels.last().cloned() {
                    self.terminate(Terminator::Branch(label));
                    let dead = self.fresh_label("post_break");
                    self.start_block(dead);
                }
            }
            Stmt::Continue(_span) => {
                if let Some(label) = self.func_mut().continue_labels.last().cloned() {
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
                    align: 0,
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
                self.func_mut().switch_stack.push(SwitchFrame {
                    cases: Vec::new(),
                    default_label: None,
                    expr_type: switch_expr_ty,
                });
                self.func_mut().break_labels.push(end_label.clone());

                // Jump to dispatch (which will be emitted after the body)
                self.terminate(Terminator::Branch(dispatch_label.clone()));

                // Lower the switch body - case/default stmts will register
                // their labels in switch_cases/switch_default
                self.start_block(body_label.clone());
                self.lower_stmt(body);
                // Fall off end of switch body -> go to end
                self.terminate(Terminator::Branch(end_label.clone()));

                // Pop switch context and collect the case/default info
                let switch_frame = self.func_mut().switch_stack.pop();
                self.func_mut().break_labels.pop();
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
                if let Some(switch_ty) = self.func_mut().switch_stack.last().map(|f| &f.expr_type) {
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
                if let Some(frame) = self.func_mut().switch_stack.last_mut() {
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
                if let Some(frame) = self.func_mut().switch_stack.last_mut() {
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
                let possible_targets: Vec<String> = self.func_mut().user_labels.values().cloned().collect();
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
                    } else if stripped == "m" {
                        // Memory constraint: need the address of the operand, not its value.
                        // Use lower_lvalue to get the memory address.
                        if let Some(lv) = self.lower_lvalue(&inp.expr) {
                            let ptr = match lv {
                                super::lowering::LValue::Variable(v) | super::lowering::LValue::Address(v) => v,
                            };
                            Operand::Value(ptr)
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
            let resolution = layout.resolve_init_field(desig_name, current_field_idx, &self.types);
            let field_idx = match &resolution {
                Some(crate::common::types::InitFieldResolution::Direct(idx)) => *idx,
                Some(crate::common::types::InitFieldResolution::AnonymousMember { anon_field_idx, inner_name }) => {
                    // Designator targets a field inside an anonymous struct/union member.
                    // Drill into the anonymous member with the original designator.
                    let anon_field = &layout.fields[*anon_field_idx].clone();
                    let anon_offset = base_offset + anon_field.offset;
                    let sub_layout = match &anon_field.ty {
                        CType::Struct(key) | CType::Union(key) => {
                            match self.types.struct_layouts.get(key) {
                                Some(l) => l.clone(),
                                None => { item_idx += 1; current_field_idx = *anon_field_idx + 1; continue; }
                            }
                        }
                        _ => { item_idx += 1; current_field_idx = *anon_field_idx + 1; continue; }
                    };
                    let sub_item = InitializerItem {
                        designators: vec![Designator::Field(inner_name.clone())],
                        init: item.init.clone(),
                    };
                    self.emit_struct_init(&[sub_item], base_alloca, &sub_layout, anon_offset);
                    item_idx += 1;
                    current_field_idx = *anon_field_idx + 1;
                    continue;
                }
                None => break,
            };
            let field = &layout.fields[field_idx].clone();
            let field_offset = base_offset + field.offset;

            // Check if designator targets a field inside an anonymous struct/union member.
            // resolve_init_field_idx returns the anonymous member's index in this case.
            let is_anon_member_designator = desig_name.is_some()
                && field.name.is_empty()
                && matches!(&field.ty, CType::Struct(_) | CType::Union(_));

            // Handle nested designators (e.g., .a.j = 2): drill into sub-struct
            let has_nested_designator = item.designators.len() > 1
                && matches!(item.designators.first(), Some(Designator::Field(_)));

            match &field.ty {
                CType::Struct(key) | CType::Union(key) if has_nested_designator || is_anon_member_designator => {
                    if let Some(sub_layout) = self.types.struct_layouts.get(key).cloned() {
                        let sub_designators = if is_anon_member_designator && !has_nested_designator {
                            // Designator targets a field inside the anonymous struct/union:
                            // pass all designators through to the sub-struct init
                            item.designators.clone()
                        } else {
                            item.designators[1..].to_vec()
                        };
                        let sub_item = InitializerItem {
                            designators: sub_designators,
                            init: item.init.clone(),
                        };
                        self.emit_struct_init(&[sub_item], base_alloca, &sub_layout, field_offset);
                    }
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
                            if let CType::Struct(ref key) | CType::Union(ref key) = elem_ty.as_ref() {
                                if let Some(sub_layout) = self.types.struct_layouts.get(key).cloned() {
                                    // Include any remaining index designators for nested arrays
                                    let sub_desigs: Vec<_> = after_first_idx.iter().cloned().collect();
                                    let sub_item = InitializerItem {
                                        designators: sub_desigs,
                                        init: item.init.clone(),
                                    };
                                    self.emit_struct_init(&[sub_item], base_alloca, &sub_layout, elem_offset);
                                }
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
                                            self.emit_init_expr_to_offset(e, base_alloca, inner_offset, inner_ir_ty);
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
                                    self.emit_init_expr_to_offset(e, base_alloca, elem_offset, elem_ir_ty);
                                }
                            } else {
                                self.emit_init_expr_to_offset(e, base_alloca, elem_offset, elem_ir_ty);
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
                                            let inner_offset = elem_offset + si * inner_elem_size;
                                            self.emit_init_expr_to_offset(e, base_alloca, inner_offset, inner_ir_ty);
                                        }
                                        si += 1;
                                    }
                                }
                                CType::Struct(ref key) | CType::Union(ref key) => {
                                    if let Some(sub_layout) = self.types.struct_layouts.get(key).cloned() {
                                        self.emit_struct_init(sub_items, base_alloca, &sub_layout, elem_offset);
                                    }
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
                            let elem_offset = field_offset + ai * elem_size;
                            self.emit_init_expr_to_offset(e, base_alloca, elem_offset, elem_ir_ty);
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
                CType::Struct(key) | CType::Union(key) => {
                    if let Some(sub_layout) = self.types.struct_layouts.get(key).cloned() {
                        match &item.init {
                            Initializer::List(sub_items) => {
                                // Always zero-init the sub-struct/union region before writing explicit values.
                                // C11 6.7.9p21: unspecified members are implicitly zero-initialized.
                                // This handles partial array field init within nested structs.
                                self.zero_init_region(base_alloca, field_offset, sub_layout.size);
                                self.emit_struct_init(sub_items, base_alloca, &sub_layout, field_offset);
                                item_idx += 1;
                            }
                            Initializer::Expr(expr) => {
                                if self.struct_value_size(expr).is_some() {
                                    // Struct/union copy in init list
                                    let src_addr = self.get_struct_base_addr(expr);
                                    self.emit_memcpy_at_offset(base_alloca, field_offset, src_addr, sub_layout.size);
                                    item_idx += 1;
                                } else {
                                    // Flat init: consume items for inner struct/union fields
                                    let consumed = self.emit_struct_init(&items[item_idx..], base_alloca, &sub_layout, field_offset);
                                    if consumed == 0 { item_idx += 1; } else { item_idx += consumed; }
                                }
                            }
                        }
                    } else {
                        item_idx += 1;
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
                            if let CType::Struct(ref key) | CType::Union(ref key) = elem_ty.as_ref() {
                                if let Some(sub_layout) = self.types.struct_layouts.get(key).cloned() {
                                // Array of structs: each sub_item inits one struct element
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
                                                self.emit_memcpy_at_offset(base_alloca, elem_offset, src_addr, sub_layout.size);
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
                                        let elem_ir_ty = IrType::from_ctype(elem_ty);
                                        self.emit_init_expr_to_offset(e, base_alloca, elem_offset, elem_ir_ty);
                                    } else if let Initializer::List(inner_items) = &sub_item.init {
                                        // Handle braced sub-init for array elements (e.g., int arr[2][3] = {{1,2,3},{4,5,6}})
                                        if let CType::Array(inner_elem_ty, Some(inner_size)) = elem_ty.as_ref() {
                                            let inner_elem_ir_ty = IrType::from_ctype(inner_elem_ty);
                                            let inner_elem_size = inner_elem_ty.size();
                                            for (ii, inner_item) in inner_items.iter().enumerate() {
                                                if ii >= *inner_size { break; }
                                                if let Initializer::Expr(e) = &inner_item.init {
                                                    let inner_offset = elem_offset + ii * inner_elem_size;
                                                    self.emit_init_expr_to_offset(e, base_alloca, inner_offset, inner_elem_ir_ty);
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
                            if let CType::Struct(ref key) | CType::Union(ref key) = elem_ty.as_ref() {
                                if let Some(sub_layout) = self.types.struct_layouts.get(key).cloned() {
                                    // Flat init for array of structs: consume items for each
                                    // struct element's fields across the array
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
                                    item_idx += 1;
                                }
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
                                        let elem_offset = field_offset + ai * elem_size;
                                        self.emit_init_expr_to_offset(expr, base_alloca, elem_offset, elem_ir_ty);
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
                    let dest_addr = self.emit_gep_offset(base_alloca, field_offset, IrType::Ptr);
                    match &item.init {
                        Initializer::Expr(e) => {
                            let src = self.lower_expr_to_complex(e, &complex_ctype);
                            self.emit(Instruction::Memcpy {
                                dest: dest_addr,
                                src,
                                size: complex_size,
                            });
                        }
                        Initializer::List(sub_items) => {
                            self.lower_complex_list_init(sub_items, dest_addr, &complex_ctype);
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
                    let addr = self.emit_gep_offset(base_alloca, field_offset, field_ty);
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

            // For unions without a designator, only the first field is initialized.
            // Stop iterating fields after processing one element.
            if layout.is_union && desig_name.is_none() {
                break;
            }
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
