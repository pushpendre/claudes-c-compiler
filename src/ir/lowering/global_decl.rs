//! Global declaration lowering and shared declaration analysis.
//!
//! - `lower_global_decl`: processes top-level declarations (typedefs, global variables,
//!   extern declarations) into IR globals with proper initializers, alignment, and linkage.
//! - `analyze_declaration`: shared analysis for both local and global variable declarations,
//!   computing type properties, array/pointer info, struct layout, etc.
//! - `fixup_unsized_array`: resolves unsized array declarations from initializer size.

use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::{IrType, CType};
use super::lowering::Lowerer;
use super::definitions::{GlobalInfo, DeclAnalysis};

impl Lowerer {
    // --- Global declaration lowering ---

    pub(super) fn lower_global_decl(&mut self, decl: &Declaration) {
        // Register any struct/union definitions
        self.register_struct_type(&decl.type_spec);

        // Collect enum constants from top-level enum type declarations
        self.collect_enum_constants(&decl.type_spec);

        // If this is a typedef, register the mapping and skip variable emission
        if decl.is_typedef {
            // Check if this typedef aliases an enum type (directly or transitively).
            // Used to treat enum-typedef bitfields as unsigned (GCC compat).
            let is_enum_type = self.is_enum_type_spec(&decl.type_spec);
            for declarator in &decl.declarators {
                if !declarator.name.is_empty() {
                    let mut resolved_ctype = self.build_full_ctype(&decl.type_spec, &declarator.derived);
                    // Apply __attribute__((vector_size(N))): wrap base type in Vector
                    if let Some(vs) = decl.vector_size {
                        resolved_ctype = CType::Vector(Box::new(resolved_ctype), vs);
                    }
                    self.types.typedefs.insert(declarator.name.clone(), resolved_ctype);
                    if is_enum_type && declarator.derived.is_empty() {
                        self.types.enum_typedefs.insert(declarator.name.clone());
                    }
                    // Preserve alignment override from __attribute__((aligned(N))) on the typedef.
                    // Recompute sizeof for aligned(sizeof(type)) with full layout info.
                    let effective_alignment = {
                        let mut align = decl.alignment;
                        if let Some(ref sizeof_ts) = decl.alignment_sizeof_type {
                            let real_sizeof = self.sizeof_type(sizeof_ts);
                            align = Some(align.map_or(real_sizeof, |a| a.max(real_sizeof)));
                        }
                        align.or_else(|| {
                            decl.alignas_type.as_ref().map(|ts| self.alignof_type(ts))
                        })
                    };
                    if let Some(align) = effective_alignment {
                        self.types.typedef_alignments.insert(declarator.name.clone(), align);
                    }
                }
            }
            // Mark transparent_union on the union's StructLayout
            if decl.is_transparent_union {
                self.mark_transparent_union(decl);
            }
            return;
        }

        for declarator in &decl.declarators {
            if declarator.name.is_empty() {
                continue;
            }

            // Skip function declarations (prototypes), but NOT function pointer variables.
            if declarator.derived.iter().any(|d| matches!(d, DerivedDeclarator::Function(_, _)))
                && !declarator.derived.iter().any(|d| matches!(d, DerivedDeclarator::FunctionPointer(_, _)))
                && declarator.init.is_none()
            {
                continue;
            }

            // Skip declarations using function typedefs
            if declarator.init.is_none() {
                if let TypeSpecifier::TypedefName(tname) = &decl.type_spec {
                    if self.types.function_typedefs.contains_key(tname) {
                        continue;
                    }
                }
            }

            // Global register variable: `register <type> <name> __asm__("reg")`
            // No storage is emitted; reads/writes map directly to the named register.
            if let Some(ref reg_name) = declarator.asm_register {
                let da = self.analyze_declaration(&decl.type_spec, &declarator.derived);
                let mut ginfo = GlobalInfo::from_analysis(&da);
                ginfo.asm_register = Some(reg_name.clone());
                ginfo.var.address_space = decl.address_space;
                self.globals.insert(declarator.name.clone(), ginfo);
                continue;
            }

            // extern without initializer: track the type but don't emit a .bss entry
            if decl.is_extern && declarator.init.is_none() {
                if !self.globals.contains_key(&declarator.name) {
                    let da = self.analyze_declaration(&decl.type_spec, &declarator.derived);
                    let mut ginfo = GlobalInfo::from_analysis(&da);
                    ginfo.var.address_space = decl.address_space;
                    self.globals.insert(declarator.name.clone(), ginfo);
                }
                continue;
            }

            // Handle tentative definitions and re-declarations
            // When a global was previously declared with __weak and is now
            // redefined with an initializer (without __weak), preserve the
            // weak attribute from the earlier declaration. GCC keeps the weak
            // binding in this case (e.g., Linux kernel's init/version.c uses
            // `const char linux_banner[] __weak;` followed by a strong definition).
            let mut prior_was_weak = false;
            if self.globals.contains_key(&declarator.name) {
                if declarator.init.is_none() {
                    if self.emitted_global_names.contains(&declarator.name) {
                        // Even though we skip re-emitting, propagate __weak
                        // to the already-emitted global if this redeclaration
                        // carries the weak attribute.
                        if declarator.is_weak {
                            for g in &mut self.module.globals {
                                if g.name == declarator.name {
                                    g.is_weak = true;
                                    break;
                                }
                            }
                        }
                        continue;
                    }
                } else {
                    // Save the is_weak flag from the previous tentative definition
                    for g in &self.module.globals {
                        if g.name == declarator.name {
                            prior_was_weak = g.is_weak;
                            break;
                        }
                    }
                    self.module.globals.retain(|g| g.name != declarator.name);
                    self.emitted_global_names.remove(&declarator.name);
                }
            }

            let mut da = self.analyze_declaration(&decl.type_spec, &declarator.derived);

            // For global arrays-of-pointers, clear struct_layout
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

            let is_extern_decl = decl.is_extern && declarator.init.is_none();

            // Register the global before evaluating its initializer so that
            // self-referential initializers (e.g., `struct Node n = {&n}`)
            // can resolve &n via eval_global_addr_expr.
            let mut ginfo = GlobalInfo::from_analysis(&da);
            ginfo.var.address_space = decl.address_space;
            self.globals.insert(declarator.name.clone(), ginfo);

            let init = if let Some(ref initializer) = declarator.init {
                // For pointer globals (char *p = ...) and pointer arrays (char *a[] = ...),
                // use Ptr as the base type for initializer coercion, not the pointee type.
                // base_ty is the pointee type (e.g., I8 for char*), but the stored value
                // is always pointer-sized. For non-pointer scalars and non-pointer arrays,
                // base_ty is correct.
                let init_base_ty = if da.is_pointer && !da.is_array {
                    da.var_ty  // scalar pointer: var_ty = Ptr
                } else if da.is_array_of_pointers || da.is_array_of_func_ptrs {
                    IrType::Ptr  // pointer array: each element is Ptr
                } else {
                    da.base_ty
                };
                self.lower_global_init(initializer, &decl.type_spec, init_base_ty, da.is_array, da.elem_size, da.actual_alloc_size, &da.struct_layout, &da.array_dim_strides)
            } else {
                GlobalInit::Zero
            };

            let (align, has_explicit_align) = {
                let c_align = self.alignof_type(&decl.type_spec);
                let natural = if c_align > 0 { c_align.max(da.var_ty.align()) } else { da.var_ty.align() };
                // Resolve _Alignas alignment: use the lowerer's alignof_type (which can
                // resolve typedefs) for the alignas type specifier, falling back to the
                // parser's pre-computed numeric alignment.
                let mut explicit = if let Some(ref alignas_ts) = decl.alignas_type {
                    Some(self.alignof_type(alignas_ts))
                } else {
                    decl.alignment
                };
                // Recompute sizeof for aligned(sizeof(type)) with full layout info
                if let Some(ref sizeof_ts) = decl.alignment_sizeof_type {
                    let real_sizeof = self.sizeof_type(sizeof_ts);
                    explicit = Some(explicit.map_or(real_sizeof, |a| a.max(real_sizeof)));
                }
                // Also incorporate alignment from typedef
                if let Some(&td_align) = self.typedef_alignment_for_type_spec(&decl.type_spec) {
                    explicit = Some(explicit.map_or(td_align, |a| a.max(td_align)));
                }
                if let Some(explicit_val) = explicit {
                    (natural.max(explicit_val), true)
                } else {
                    (natural, false)
                }
            };

            // Store explicit alignment in GlobalInfo so _Alignof(var) returns
            // the correct alignment per C11 6.2.8p3.
            if has_explicit_align {
                if let Some(ginfo) = self.globals.get_mut(&declarator.name) {
                    ginfo.var.explicit_alignment = Some(align);
                }
            }

            let is_static = decl.is_static;

            let global_ty = da.resolve_global_ty(&init);

            let final_size = match &init {
                GlobalInit::Array(vals) if da.is_struct && vals.len() > da.actual_alloc_size => vals.len(),
                GlobalInit::Compound(_) if da.is_struct => {
                    let emitted = init.emitted_byte_size();
                    emitted.max(da.actual_alloc_size)
                }
                _ => da.actual_alloc_size,
            };

            // For pointer variables, decl.is_const refers to the pointee type
            // (e.g., `const char *p` means p points to const char, but p itself is mutable).
            // Only non-pointer variables get is_const from the declaration.
            // Our parser doesn't track `* const` (pointer-to-const qualifier), so for
            // pointers we conservatively mark as non-const.
            let var_is_const = decl.is_const && !da.is_pointer
                && !da.is_array_of_pointers && !da.is_array_of_func_ptrs;

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
                section: declarator.section.clone(),
                is_weak: declarator.is_weak || prior_was_weak,
                visibility: declarator.visibility.clone(),
                has_explicit_align,
                is_const: var_is_const,
                is_used: declarator.is_used,
            });
        }
    }

    // --- Declaration analysis ---

    /// Perform shared declaration analysis for both local and global variable declarations.
    ///
    /// Computes all type-related properties (base type, array info, pointer info, struct layout,
    /// pointee type, etc.) that both `lower_local_decl` and `lower_global_decl` need.
    pub(super) fn analyze_declaration(&self, type_spec: &TypeSpecifier, derived: &[DerivedDeclarator]) -> DeclAnalysis {
        let mut base_ty = self.type_spec_to_ir(type_spec);
        let (alloc_size, elem_size, is_array, is_pointer, array_dim_strides) =
            self.compute_decl_info(type_spec, derived);

        // For typedef'd array types, type_spec_to_ir returns Ptr but we need the element type
        if is_array && base_ty == IrType::Ptr && !is_pointer {
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
            if !found {
                let ctype = self.type_spec_to_ctype(type_spec);
                let mut ct = &ctype;
                while let CType::Array(inner, _) = ct {
                    ct = inner.as_ref();
                }
                base_ty = IrType::from_ctype(ct);
            }
        }

        // Detect arrays-of-pointers and arrays-of-function-pointers.
        // For declarations like `int (*ap[3])[4]` (array of pointers-to-arrays),
        // derived = [Array(4), Pointer, Array(3)]. The last element is Array(3),
        // making this an array-of-pointers. We check:
        //   1. Any pointer before the LAST array in derived (not just the first array),
        //      matching the logic in compute_decl_info which checks `last_is_array`.
        //   2. Typedef'd pointer with array dimensions.
        let is_array_of_pointers = is_array && {
            let ptr_pos = derived.iter().position(|d| matches!(d, DerivedDeclarator::Pointer));
            let last_arr_pos = derived.iter().rposition(|d| matches!(d, DerivedDeclarator::Array(_)));
            let has_derived_ptr_before_last_arr = matches!((ptr_pos, last_arr_pos), (Some(pp), Some(ap)) if pp < ap);
            let typedef_ptr_array = ptr_pos.is_none() && last_arr_pos.is_some() &&
                self.is_type_pointer(type_spec);
            has_derived_ptr_before_last_arr || typedef_ptr_array
        };
        let is_array_of_func_ptrs = is_array && derived.iter().any(|d|
            matches!(d, DerivedDeclarator::FunctionPointer(_, _) | DerivedDeclarator::Function(_, _)));
        let var_ty = if is_pointer || is_array_of_pointers || is_array_of_func_ptrs {
            IrType::Ptr
        } else {
            base_ty
        };

        // Compute element IR type for arrays
        let elem_ir_ty = if is_array && base_ty == IrType::Ptr && !is_array_of_pointers {
            let ctype = self.type_spec_to_ctype(type_spec);
            if let CType::Array(ref elem_ct, _) = ctype {
                IrType::from_ctype(elem_ct)
            } else {
                base_ty
            }
        } else {
            base_ty
        };

        let has_derived_ptr = derived.iter().any(|d| matches!(d, DerivedDeclarator::Pointer));
        let is_bool = self.is_type_bool(type_spec) && !has_derived_ptr && !is_array;

        let struct_layout = self.get_struct_layout_for_type(type_spec)
            .or_else(|| {
                let ctype = self.type_spec_to_ctype(type_spec);
                match &ctype {
                    CType::Pointer(inner, _) => self.struct_layout_from_ctype(inner),
                    CType::Array(_, _) => {
                        // Unwrap all Array levels to find the innermost element type.
                        // This handles multi-dimensional typedef'd arrays like:
                        // typedef struct s tyst[2][2]; -> CType::Array(Array(Struct, 2), 2)
                        let mut ct = &ctype;
                        while let CType::Array(inner, _) = ct {
                            ct = inner.as_ref();
                        }
                        self.struct_layout_from_ctype(ct)
                    }
                    _ => None,
                }
            });
        let is_struct = struct_layout.is_some() && !is_pointer && !is_array;

        let actual_alloc_size = if let Some(ref layout) = struct_layout {
            if is_array || is_pointer { alloc_size } else { layout.size }
        } else {
            alloc_size
        };

        let pointee_type = self.compute_pointee_type(type_spec, derived);
        let c_type = Some(self.build_full_ctype(type_spec, derived));

        // Detect pointer-to-function-pointer: declared with 2+ consecutive Pointer
        // entries before the first FunctionPointer (e.g., int (**fpp)(int, int)
        // has derived=[Pointer, Pointer, FunctionPointer(...)]).
        // This is NOT triggered for function-pointer-returning-function-pointer like
        // int (*(*p)(int,int))(int,int) where Pointer entries are separated by
        // FunctionPointer entries.
        // This detection is needed because build_full_ctype absorbs extra pointer
        // levels into the function's return type, making the CType shape ambiguous.
        let is_ptr_to_func_ptr = {
            // A pointer-to-function-pointer (e.g., int (**fpp)(int, int)) has derived
            // = [Pointer, FunctionPointer, Pointer]. The extra indirection Pointer
            // appears AFTER the FunctionPointer entry (added by combine_declarator_parts
            // as "extra indirection" from inner_derived).
            //
            // In contrast, int *(*fnc)() has derived = [Pointer, Pointer, FunctionPointer]
            // where pointers BEFORE FunctionPointer are return-type pointers, not extra
            // indirection. The syntax-marker Pointer is always immediately before FunctionPointer.
            //
            // So the correct check is: has any Pointer entries AFTER the FunctionPointer entry.
            let fptr_count = derived.iter().filter(|d|
                matches!(d, DerivedDeclarator::FunctionPointer(_, _) | DerivedDeclarator::Function(_, _))).count();
            if fptr_count == 1 {
                let mut found_fptr = false;
                let mut ptrs_after_fptr = 0usize;
                for d in derived {
                    match d {
                        DerivedDeclarator::FunctionPointer(_, _) | DerivedDeclarator::Function(_, _) => {
                            found_fptr = true;
                        }
                        DerivedDeclarator::Pointer if found_fptr => {
                            ptrs_after_fptr += 1;
                        }
                        _ => {}
                    }
                }
                ptrs_after_fptr >= 1
            } else {
                false
            }
        };

        DeclAnalysis {
            base_ty, var_ty, alloc_size, elem_size, is_array, is_pointer,
            array_dim_strides, is_array_of_pointers, is_array_of_func_ptrs,
            struct_layout, is_struct, actual_alloc_size, pointee_type, c_type,
            is_bool, elem_ir_ty, is_ptr_to_func_ptr,
        }
    }

    /// Fix up allocation size and strides for unsized arrays (int a[] = {...}).
    /// Also updates c_type from Array(elem, None) to Array(elem, Some(count))
    /// so that sizeof() returns the correct array size rather than pointer size.
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
                    if da.base_ty == IrType::I8 || da.base_ty == IrType::U8 {
                        if let Expr::StringLiteral(s, _) = expr {
                            da.alloc_size = s.chars().count() + 1;
                            da.actual_alloc_size = da.alloc_size;
                        }
                        if let Expr::WideStringLiteral(s, _) | Expr::Char16StringLiteral(s, _) = expr {
                            da.alloc_size = s.chars().count() + 1;
                            da.actual_alloc_size = da.alloc_size;
                        }
                    }
                    if da.base_ty == IrType::I32 || da.base_ty == IrType::U32 {
                        if let Expr::WideStringLiteral(s, _) = expr {
                            let char_count = s.chars().count() + 1;
                            da.alloc_size = char_count * 4;
                            da.actual_alloc_size = da.alloc_size;
                        }
                        if let Expr::StringLiteral(s, _) | Expr::Char16StringLiteral(s, _) = expr {
                            let char_count = s.chars().count() + 1;
                            da.alloc_size = char_count * 4;
                            da.actual_alloc_size = da.alloc_size;
                        }
                    }
                    if da.base_ty == IrType::I16 || da.base_ty == IrType::U16 {
                        if let Expr::Char16StringLiteral(s, _)
                            | Expr::StringLiteral(s, _)
                            | Expr::WideStringLiteral(s, _) = expr {
                            let char_count = s.chars().count() + 1;
                            da.alloc_size = char_count * 2;
                            da.actual_alloc_size = da.alloc_size;
                        }
                    }
                }
                Initializer::List(items) => {
                    let actual_count = if let Some(ref layout) = da.struct_layout {
                        self.compute_struct_array_init_count(items, layout)
                    } else {
                        self.compute_init_list_array_size_for_char_array(items, da.base_ty)
                    };
                    if da.elem_size > 0 {
                        // For multi-dimensional arrays with nested brace init
                        // (e.g., int*[][3] = {{&x,&y,&z}, {&w,0,0}}), each
                        // top-level item spans the outermost stride.
                        // For flat init (e.g., {&x,0,0,&y,0,0}), each item
                        // is one element.
                        let has_nested_lists = items.iter().any(|item| {
                            matches!(&item.init, Initializer::List(_))
                        });
                        let item_size = if da.array_dim_strides.len() > 1 && has_nested_lists {
                            da.array_dim_strides[0]
                        } else {
                            da.elem_size
                        };
                        da.alloc_size = actual_count * item_size;
                        da.actual_alloc_size = da.alloc_size;
                        if da.array_dim_strides.len() == 1 {
                            da.array_dim_strides = vec![da.elem_size];
                        }
                    }
                }
            }
            // Update c_type from Array(elem, None) to Array(elem, Some(count))
            // so that sizeof() on this array returns the correct total size.
            if da.elem_size > 0 && da.alloc_size > 0 {
                let resolved_count = da.alloc_size / da.elem_size;
                if let Some(ref ct) = da.c_type {
                    if let CType::Array(elem, None) = ct {
                        da.c_type = Some(CType::Array(elem.clone(), Some(resolved_count)));
                    }
                }
            }
        }
    }
}
