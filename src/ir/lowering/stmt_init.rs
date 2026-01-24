//! Local variable initialization lowering.
//!
//! Extracted from stmt.rs to reduce that file's size. Contains the logic for
//! lowering `Initializer::Expr` and `Initializer::List` for local variable
//! declarations, plus helpers for registering block-scope function declarations.

use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, CType};
use super::lowering::{Lowerer, LocalInfo, GlobalInfo, DeclAnalysis, FuncSig};

impl Lowerer {
    /// Handle extern declarations inside function bodies.
    /// They reference a global symbol, not a local variable.
    pub(super) fn lower_extern_decl(
        &mut self,
        decl: &Declaration,
        declarator: &InitDeclarator,
    ) -> bool {
        // Remove from locals, but track in scope frame so pop_scope()
        // restores the local when this block exits.
        self.shadow_local_for_scope(&declarator.name);
        // Also remove from static_local_names so that the extern name
        // resolves to the true global, not a same-named static local
        self.shadow_static_for_scope(&declarator.name);

        // Check if this is a function declaration (extern int f(int))
        let is_func_decl = declarator.derived.iter().any(|d| matches!(d, DerivedDeclarator::Function(_, _)));
        if is_func_decl {
            return false; // Fall through to the function declaration handler
        }

        if !self.globals.contains_key(&declarator.name) {
            let ext_da = self.analyze_declaration(&decl.type_spec, &declarator.derived);
            self.globals.insert(declarator.name.clone(), GlobalInfo::from_analysis(&ext_da));
        }
        true // Handled, caller should continue to next declarator
    }

    /// Handle block-scope function declarations: `int f(int);`
    /// These declare an external function, not a local variable.
    /// Returns true if this was a function declaration (caller should continue to next declarator).
    pub(super) fn try_lower_block_func_decl(
        &mut self,
        decl: &Declaration,
        declarator: &InitDeclarator,
    ) -> bool {
        // Check for direct function declarator: int f(int, int)
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
            self.register_block_func_meta(&declarator.name, &decl.type_spec, ptr_count, &params, variadic);
            self.shadow_local_for_scope(&declarator.name);
            return true;
        }

        // Also handle typedef-based function declarations in block scope
        // (e.g., `func_t add;` where func_t is `typedef int func_t(int);`)
        if declarator.derived.is_empty() && declarator.init.is_none() {
            if let TypeSpecifier::TypedefName(tname) = &decl.type_spec {
                if let Some(fti) = self.function_typedefs.get(tname).cloned() {
                    self.register_block_func_meta(&declarator.name, &fti.return_type, 0, &fti.params, fti.variadic);
                    self.shadow_local_for_scope(&declarator.name);
                    return true;
                }
            }
        }

        false
    }

    /// Register function metadata for a block-scope function declaration.
    /// Simpler than the full `register_function_meta` since block-scope declarations
    /// don't need sret/two-reg detection or complex return CType tracking.
    fn register_block_func_meta(
        &mut self,
        name: &str,
        ret_type_spec: &TypeSpecifier,
        ptr_count: usize,
        params: &[ParamDecl],
        variadic: bool,
    ) {
        self.known_functions.insert(name.to_string());
        let mut ret_ty = self.type_spec_to_ir(ret_type_spec);
        if ptr_count > 0 {
            ret_ty = IrType::Ptr;
        }
        let param_tys: Vec<IrType> = params.iter().map(|p| {
            self.type_spec_to_ir(&p.type_spec)
        }).collect();
        let param_bool_flags: Vec<bool> = params.iter().map(|p| {
            matches!(self.resolve_type_spec(&p.type_spec), TypeSpecifier::Bool)
        }).collect();
        let sig = if !variadic || !param_tys.is_empty() {
            FuncSig {
                return_type: ret_ty,
                return_ctype: None,
                param_types: param_tys,
                param_ctypes: Vec::new(),
                param_bool_flags,
                is_variadic: variadic,
                sret_size: None,
                two_reg_ret_size: None,
                param_struct_sizes: Vec::new(),
            }
        } else {
            FuncSig {
                return_type: ret_ty,
                return_ctype: None,
                param_types: Vec::new(),
                param_ctypes: Vec::new(),
                param_bool_flags: Vec::new(),
                is_variadic: variadic,
                sret_size: None,
                two_reg_ret_size: None,
                param_struct_sizes: Vec::new(),
            }
        };
        self.func_meta.sigs.insert(name.to_string(), sig);
    }

    /// Lower an `Initializer::Expr` for a local variable declaration.
    /// Handles char arrays from string literals, struct copy-init, complex init,
    /// and scalar init with implicit casts.
    pub(super) fn lower_local_init_expr(
        &mut self,
        expr: &Expr,
        alloca: Value,
        da: &DeclAnalysis,
        is_complex: bool,
        decl: &Declaration,
    ) {
        if da.is_array && (da.base_ty == IrType::I8 || da.base_ty == IrType::U8) {
            self.lower_char_array_init_expr(expr, alloca, da);
        } else if da.is_array && da.base_ty == IrType::I32 {
            self.lower_wchar_array_init_expr(expr, alloca, da);
        } else if da.is_struct {
            self.lower_struct_copy_init(expr, alloca, da);
        } else if is_complex {
            self.lower_complex_var_init(expr, alloca, da, decl);
        } else {
            self.lower_scalar_init_expr(expr, alloca, da, decl);
        }
    }

    /// Char array from string literal: `char s[] = "hello"`
    fn lower_char_array_init_expr(&mut self, expr: &Expr, alloca: Value, da: &DeclAnalysis) {
        match expr {
            Expr::StringLiteral(s, _) | Expr::WideStringLiteral(s, _) => {
                self.emit_string_to_alloca(alloca, s, 0);
                // Zero-fill remaining bytes if string is shorter than array
                let str_len = s.chars().count() + 1; // +1 for null terminator
                let arr_size = da.alloc_size;
                for i in str_len..arr_size {
                    let val = Operand::Const(IrConst::I8(0));
                    self.emit_store_at_offset(alloca, i, val, IrType::I8);
                }
            }
            _ => {
                let val = self.lower_expr(expr);
                self.emit(Instruction::Store { val, ptr: alloca, ty: da.var_ty });
            }
        }
    }

    /// wchar_t/int array from wide string: `wchar_t s[] = L"hello"`
    fn lower_wchar_array_init_expr(&mut self, expr: &Expr, alloca: Value, da: &DeclAnalysis) {
        match expr {
            Expr::WideStringLiteral(s, _) | Expr::StringLiteral(s, _) => {
                self.emit_wide_string_to_alloca(alloca, s, 0);
            }
            _ => {
                let val = self.lower_expr(expr);
                self.emit(Instruction::Store { val, ptr: alloca, ty: da.var_ty });
            }
        }
    }

    /// Struct copy-initialization: `struct Point b = a;`
    fn lower_struct_copy_init(&mut self, expr: &Expr, alloca: Value, da: &DeclAnalysis) {
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
    }

    /// Complex variable initialization: `_Complex double z = expr;`
    fn lower_complex_var_init(&mut self, expr: &Expr, alloca: Value, da: &DeclAnalysis, decl: &Declaration) {
        let resolved_ts = self.resolve_type_spec(&decl.type_spec);
        let complex_ctype = self.type_spec_to_ctype(&resolved_ts);
        let src = self.lower_expr_to_complex(expr, &complex_ctype);
        self.emit(Instruction::Memcpy {
            dest: alloca,
            src,
            size: da.actual_alloc_size,
        });
    }

    /// Scalar variable initialization with implicit casts and const tracking.
    fn lower_scalar_init_expr(&mut self, expr: &Expr, alloca: Value, da: &DeclAnalysis, decl: &Declaration) {
        // Track const-qualified integer variable values for compile-time
        // array size evaluation (e.g., const int len = 5000; int arr[len];)
        if decl.is_const && !da.is_pointer && !da.is_array && !da.is_struct {
            if let Some(const_val) = self.eval_const_expr(expr) {
                if let Some(ival) = self.const_to_i64(&const_val) {
                    // Use declarator name from the declaration context
                    // (passed via da which carries the name context)
                    // Actually we need the name; we'll handle this at the call site
                }
            }
        }
        // Check if RHS is complex but LHS is non-complex:
        // extract real part first, then convert to target type.
        let rhs_ctype = self.expr_ctype(expr);
        let val = if rhs_ctype.is_complex() {
            let complex_val = self.lower_expr(expr);
            let ptr = self.operand_to_value(complex_val);
            let real_part = self.load_complex_real(ptr, &rhs_ctype);
            let from_ty = Self::complex_component_ir_type(&rhs_ctype);
            if da.is_bool {
                // For _Bool targets, normalize at the source type before any truncation.
                // C11 6.3.1.2: conversion to _Bool yields (val != 0) ? 1 : 0.
                self.emit_bool_normalize_typed(real_part, from_ty)
            } else {
                self.emit_implicit_cast(real_part, from_ty, da.var_ty)
            }
        } else if da.is_bool {
            // For _Bool targets, normalize at the source type before any truncation.
            // Truncating first (e.g. 0x100 -> U8 = 0) then normalizing gives wrong results.
            let val = self.lower_expr(expr);
            let expr_ty = self.get_expr_type(expr);
            self.emit_bool_normalize_typed(val, expr_ty)
        } else {
            let val = self.lower_expr(expr);
            let expr_ty = self.get_expr_type(expr);
            self.emit_implicit_cast(val, expr_ty, da.var_ty)
        };
        self.emit(Instruction::Store { val, ptr: alloca, ty: da.var_ty });
    }

    /// Lower `Initializer::List` for a local variable declaration.
    /// Dispatches to complex, struct, array, or scalar-with-braces handlers.
    pub(super) fn lower_local_init_list(
        &mut self,
        items: &[InitializerItem],
        alloca: Value,
        da: &DeclAnalysis,
        is_complex: bool,
        complex_elem_ctype: &Option<CType>,
        decl: &Declaration,
        declarator_name: &str,
    ) {
        if is_complex {
            self.lower_complex_init_list(items, alloca, decl);
        } else if da.is_struct {
            self.lower_struct_init_list(items, alloca, declarator_name);
        } else if da.is_array && da.elem_size > 0 {
            self.lower_array_init_list_dispatch(items, alloca, da, complex_elem_ctype, decl, declarator_name);
        } else {
            // Scalar with braces: int x = { 1 };
            self.lower_scalar_braced_init(items, alloca, da);
        }
    }

    /// Complex initializer list: `_Complex double z = {real, imag}`
    fn lower_complex_init_list(&mut self, items: &[InitializerItem], alloca: Value, decl: &Declaration) {
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
            let zero = Self::complex_zero(comp_ty);
            self.emit(Instruction::Store { val: zero, ptr: alloca, ty: comp_ty });
        }
        // Store imag part (second item) at offset
        let comp_size = Self::complex_component_size(&complex_ctype);
        let imag_ptr = self.emit_gep_offset(alloca, comp_size, IrType::I8);
        if let Some(item) = items.get(1) {
            if let Initializer::Expr(expr) = &item.init {
                let val = self.lower_expr(expr);
                let expr_ty = self.get_expr_type(expr);
                let val = self.emit_implicit_cast(val, expr_ty, comp_ty);
                self.emit(Instruction::Store { val, ptr: imag_ptr, ty: comp_ty });
            }
        } else {
            let zero = Self::complex_zero(comp_ty);
            self.emit(Instruction::Store { val: zero, ptr: imag_ptr, ty: comp_ty });
        }
    }

    /// Struct initializer list with designated initializer support.
    fn lower_struct_init_list(&mut self, items: &[InitializerItem], alloca: Value, declarator_name: &str) {
        if let Some(layout) = self.func_mut().locals.get(declarator_name).and_then(|l| l.struct_layout.clone()) {
            let has_designators = items.iter().any(|item| !item.designators.is_empty());
            if has_designators || items.len() < layout.fields.len() {
                self.zero_init_alloca(alloca, layout.size);
            }
            self.emit_struct_init(items, alloca, &layout, 0);
        }
    }

    /// Dispatch array initializer list handling based on array kind.
    fn lower_array_init_list_dispatch(
        &mut self,
        items: &[InitializerItem],
        alloca: Value,
        da: &DeclAnalysis,
        complex_elem_ctype: &Option<CType>,
        decl: &Declaration,
        declarator_name: &str,
    ) {
        let elem_struct_layout = self.func_mut().locals.get(declarator_name)
            .and_then(|l| l.struct_layout.clone());

        if let Some(ref cplx_ctype) = complex_elem_ctype {
            // Array of complex elements (handles both 1D and multi-dimensional)
            self.lower_array_of_complex_init(items, alloca, da, cplx_ctype);
        } else if da.array_dim_strides.len() > 1 && elem_struct_layout.is_none() {
            // Multi-dimensional array of scalars
            self.zero_init_alloca(alloca, da.alloc_size);
            let md_elem_ty = if da.is_array_of_pointers || da.is_array_of_func_ptrs { IrType::I64 } else { da.elem_ir_ty };
            self.lower_array_init_list(items, alloca, md_elem_ty, &da.array_dim_strides);
        } else if let Some(ref s_layout) = elem_struct_layout {
            // Array of structs
            self.lower_array_of_structs_init(items, alloca, da, s_layout);
        } else {
            // 1D array of scalars
            self.lower_1d_array_init(items, alloca, da, decl);
        }
    }

    /// Array of structs initialization with designator support.
    fn lower_array_of_structs_init(
        &mut self,
        items: &[InitializerItem],
        alloca: Value,
        da: &DeclAnalysis,
        s_layout: &crate::common::types::StructLayout,
    ) {
        let struct_size = s_layout.size;
        let is_multidim = da.array_dim_strides.len() > 1;
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
                        flat_struct_idx = idx_val * row_size;
                    } else {
                        flat_struct_idx = idx_val;
                    }
                }
            }
            let base_byte_offset = flat_struct_idx * struct_size;
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
                        let mut sub_idx = 0usize;
                        let mut row_elem = 0usize;
                        while sub_idx < sub_items.len() && row_elem < row_size {
                            let row_offset = (flat_struct_idx + row_elem) * struct_size;
                            match &sub_items[sub_idx].init {
                                Initializer::List(inner) => {
                                    let elem_base = self.emit_gep_offset(alloca, row_offset, IrType::I8);
                                    self.lower_local_struct_init(inner, elem_base, s_layout);
                                    sub_idx += 1;
                                    row_elem += 1;
                                }
                                Initializer::Expr(_) => {
                                    let consumed = self.emit_struct_init(&sub_items[sub_idx..], alloca, s_layout, row_offset);
                                    sub_idx += consumed.max(1);
                                    row_elem += 1;
                                }
                            }
                        }
                        flat_struct_idx += row_size;
                    } else {
                        let elem_base = self.emit_gep_offset(alloca, base_byte_offset, IrType::I8);
                        self.lower_local_struct_init(sub_items, elem_base, s_layout);
                        flat_struct_idx += 1;
                    }
                    item_idx += 1;
                }
                Initializer::Expr(e) => {
                    if let Some(ref fname) = field_designator_name {
                        if let Some(field) = s_layout.fields.iter().find(|f| &f.name == fname) {
                            let field_ty = IrType::from_ctype(&field.ty);
                            let val = self.lower_and_cast_init_expr(e, field_ty);
                            self.emit_store_at_offset(alloca, base_byte_offset + field.offset, val, field_ty);
                        }
                        item_idx += 1;
                    } else {
                        let consumed = self.emit_struct_init(&items[item_idx..], alloca, s_layout, base_byte_offset);
                        item_idx += consumed.max(1);
                        flat_struct_idx += 1;
                        continue;
                    }
                }
            }
        }
    }

    /// Array of complex elements initialization.
    /// Handles both 1D and multi-dimensional arrays of complex types.
    fn lower_array_of_complex_init(
        &mut self,
        items: &[InitializerItem],
        alloca: Value,
        da: &DeclAnalysis,
        cplx_ctype: &CType,
    ) {
        self.zero_init_alloca(alloca, da.alloc_size);
        let mut flat_idx = 0usize;
        self.lower_complex_init_recursive(items, alloca, da, cplx_ctype, &da.array_dim_strides.clone(), &mut flat_idx);
    }

    /// Recursive helper for multi-dimensional complex array initialization.
    /// Flattens nested brace initializers into flat element indices.
    fn lower_complex_init_recursive(
        &mut self,
        items: &[InitializerItem],
        alloca: Value,
        da: &DeclAnalysis,
        cplx_ctype: &CType,
        dim_strides: &[usize],
        flat_idx: &mut usize,
    ) {
        // The leaf complex element size is always the last stride
        let leaf_size = *da.array_dim_strides.last().unwrap_or(&da.elem_size);
        // How many leaf elements per sub-array at this level
        let sub_elem_count = if dim_strides.len() > 1 && leaf_size > 0 {
            dim_strides[0] / leaf_size
        } else {
            1
        };

        for item in items.iter() {
            let start_index = *flat_idx;

            if let Some(Designator::Index(ref idx_expr)) = item.designators.first() {
                if let Some(idx_val) = self.eval_const_expr_for_designator(idx_expr) {
                    if dim_strides.len() > 1 {
                        // Designator at outer dimension: set flat index to start of that row
                        *flat_idx = idx_val * sub_elem_count;
                    } else {
                        *flat_idx = idx_val;
                    }
                }
            }

            match &item.init {
                Initializer::List(sub_items) => {
                    if dim_strides.len() > 1 {
                        // Recurse into nested brace list for inner dimensions
                        self.lower_complex_init_recursive(sub_items, alloca, da, cplx_ctype, &dim_strides[1..], flat_idx);
                        // Advance to next sub-array boundary
                        let boundary = start_index + sub_elem_count;
                        if *flat_idx < boundary {
                            *flat_idx = boundary;
                        }
                    } else {
                        // At leaf dimension, unwrap single-element brace list
                        if let Some(e) = Self::unwrap_nested_init_expr(sub_items) {
                            let src = self.lower_expr_to_complex(e, cplx_ctype);
                            self.emit_memcpy_at_offset(alloca, *flat_idx * leaf_size, src, leaf_size);
                        }
                        *flat_idx += 1;
                    }
                }
                Initializer::Expr(e) => {
                    let src = self.lower_expr_to_complex(e, cplx_ctype);
                    self.emit_memcpy_at_offset(alloca, *flat_idx * leaf_size, src, leaf_size);
                    *flat_idx += 1;
                }
            }
        }
    }

    /// 1D array initialization with designator and string literal support.
    fn lower_1d_array_init(
        &mut self,
        items: &[InitializerItem],
        alloca: Value,
        da: &DeclAnalysis,
        decl: &Declaration,
    ) {
        let num_elems = da.alloc_size / da.elem_size.max(1);
        let has_designators = items.iter().any(|item| !item.designators.is_empty());
        if has_designators || items.len() < num_elems {
            self.zero_init_alloca(alloca, da.alloc_size);
        }

        let is_complex_elem_array = matches!(
            self.resolve_type_spec(&decl.type_spec),
            TypeSpecifier::ComplexFloat | TypeSpecifier::ComplexDouble | TypeSpecifier::ComplexLongDouble
        );

        let elem_store_ty = if da.is_array_of_pointers || da.is_array_of_func_ptrs { IrType::I64 } else { da.elem_ir_ty };

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
                if !da.is_array_of_pointers && (da.elem_ir_ty == IrType::I8 || da.elem_ir_ty == IrType::U8) {
                    if let Expr::StringLiteral(s, _) = e {
                        self.emit_string_to_alloca(alloca, s, current_idx * da.elem_size);
                        current_idx += 1;
                        continue;
                    }
                }
                if is_complex_elem_array {
                    let val = self.lower_expr(e);
                    let src = self.operand_to_value(val);
                    self.emit_memcpy_at_offset(alloca, current_idx * da.elem_size, src, da.elem_size);
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

    /// Scalar with braces: `int x = { 1 };`
    fn lower_scalar_braced_init(&mut self, items: &[InitializerItem], alloca: Value, da: &DeclAnalysis) {
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
