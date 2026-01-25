use crate::common::type_builder;
use crate::frontend::parser::ast::*;
use crate::common::types::{IrType, StructField, StructLayout, CType};
use super::lowering::Lowerer;
use super::definitions::FuncSig;

impl Lowerer {

    /// Resolve a TypeSpecifier, following TypeofType wrappers.
    /// TypedefName resolution now goes through CType (see type_spec_to_ctype).
    /// This only resolves non-typedef wrappers like TypeofType.
    pub(super) fn resolve_type_spec<'a>(&'a self, ts: &'a TypeSpecifier) -> &'a TypeSpecifier {
        let mut current = ts;
        for _ in 0..32 {
            match current {
                TypeSpecifier::TypeofType(inner) => {
                    current = inner;
                    continue;
                }
                _ => {}
            }
            break;
        }
        current
    }

    /// Resolve a TypeSpecifier to its underlying CType for typedef/typeof,
    /// returning None for non-typedef/typeof specifiers.
    /// This is a lightweight lookup (no array size evaluation or function pointer building).
    fn resolve_typedef_ctype(&self, ts: &TypeSpecifier) -> Option<CType> {
        match ts {
            TypeSpecifier::TypedefName(name) => self.types.typedefs.get(name).cloned(),
            TypeSpecifier::TypeofType(inner) => self.resolve_typedef_ctype(inner),
            _ => None,
        }
    }

    /// Check if a TypeSpecifier resolves to a Bool type (through typedefs).
    pub(super) fn is_type_bool(&self, ts: &TypeSpecifier) -> bool {
        matches!(ts, TypeSpecifier::Bool)
            || self.resolve_typedef_ctype(ts).is_some_and(|ct| matches!(ct, CType::Bool))
    }

    /// Check if a TypeSpecifier resolves to a struct or union type (through typedefs).
    pub(super) fn is_type_struct_or_union(&self, ts: &TypeSpecifier) -> bool {
        matches!(ts, TypeSpecifier::Struct(..) | TypeSpecifier::Union(..))
            || self.resolve_typedef_ctype(ts).is_some_and(|ct| matches!(ct, CType::Struct(_) | CType::Union(_)))
    }

    /// Check if a TypeSpecifier is a transparent union (passed as first member for ABI).
    pub(super) fn is_transparent_union(&self, ts: &TypeSpecifier) -> bool {
        let key = match ts {
            TypeSpecifier::Union(tag, _, _, _, _) => {
                tag.as_ref().map(|t| format!("union.{}", t))
            }
            TypeSpecifier::TypedefName(name) => {
                if let Some(CType::Union(key)) = self.types.typedefs.get(name) {
                    Some(key.clone())
                } else {
                    None
                }
            }
            _ => None,
        };
        if let Some(key) = key {
            self.types.struct_layouts.get(&key).map_or(false, |l| l.is_transparent_union)
        } else {
            false
        }
    }

    /// Check if a TypeSpecifier resolves to a complex type (through typedefs).
    pub(super) fn is_type_complex(&self, ts: &TypeSpecifier) -> bool {
        matches!(ts, TypeSpecifier::ComplexFloat | TypeSpecifier::ComplexDouble | TypeSpecifier::ComplexLongDouble)
            || self.resolve_typedef_ctype(ts).is_some_and(|ct| ct.is_complex())
    }

    /// Check if a TypeSpecifier resolves to a pointer type (through typedefs).
    pub(super) fn is_type_pointer(&self, ts: &TypeSpecifier) -> bool {
        matches!(ts, TypeSpecifier::Pointer(_))
            || self.resolve_typedef_ctype(ts).is_some_and(|ct| matches!(ct, CType::Pointer(_)))
    }

    /// Resolve typeof(expr) to a concrete TypeSpecifier by analyzing the expression type.
    /// Returns a new TypeSpecifier if the input is Typeof, otherwise returns a clone of the input.
    /// Note: TypedefName resolution now goes through CType, so typeof on a typedef
    /// is handled by resolving the typedef to CType first.
    pub(super) fn resolve_typeof(&self, ts: &TypeSpecifier) -> TypeSpecifier {
        match ts {
            TypeSpecifier::Typeof(expr) => {
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    Self::ctype_to_type_spec(&ctype)
                } else {
                    TypeSpecifier::Int // fallback
                }
            }
            TypeSpecifier::TypeofType(inner) => {
                self.resolve_typeof(inner)
            }
            TypeSpecifier::TypedefName(name) => {
                // Typedefs now store CType. Convert back to TypeSpecifier for
                // code that still needs a TypeSpecifier (e.g., typeof resolution).
                if let Some(ctype) = self.types.typedefs.get(name) {
                    Self::ctype_to_type_spec(ctype)
                } else {
                    ts.clone()
                }
            }
            other => other.clone(),
        }
    }

    /// Evaluate __builtin_types_compatible_p(type1, type2).
    /// Returns 1 if the unqualified types are compatible (same type after resolving
    /// typedefs and typeof), 0 otherwise. Follows GCC semantics: ignores top-level
    /// qualifiers, resolves typedefs, but considers signed/unsigned as distinct.
    pub(super) fn eval_types_compatible(&self, type1: &TypeSpecifier, type2: &TypeSpecifier) -> i32 {
        let ctype1 = self.type_spec_to_ctype(type1);
        let ctype2 = self.type_spec_to_ctype(type2);
        // Strip top-level qualifiers (CType doesn't carry qualifiers, so this is already done).
        // Compare the resolved CTypes. GCC considers enum types as their underlying int type,
        // and considers long/int as distinct even if same size on the platform.
        if Self::ctypes_compatible(&ctype1, &ctype2) { 1 } else { 0 }
    }

    /// Check if two CTypes are compatible for __builtin_types_compatible_p purposes.
    /// This is structural equality with special handling for:
    /// - Arrays: compatible if element types match (ignore size for unsized arrays)
    /// - Pointers: compatible if pointee types are compatible
    /// - Enums: treated as compatible with int
    fn ctypes_compatible(a: &CType, b: &CType) -> bool {
        // Normalize enum to int for compatibility purposes
        let a_norm = match a { CType::Enum(_) => &CType::Int, other => other };
        let b_norm = match b { CType::Enum(_) => &CType::Int, other => other };

        match (a_norm, b_norm) {
            // Pointers: pointee types must be compatible
            (CType::Pointer(p1), CType::Pointer(p2)) => Self::ctypes_compatible(p1, p2),
            // Arrays: element types must be compatible, sizes must match (or both unsized)
            (CType::Array(e1, s1), CType::Array(e2, s2)) => {
                Self::ctypes_compatible(e1, e2) && s1 == s2
            }
            // Structs/Unions: use derived PartialEq (compares name + fields)
            (CType::Struct(s1), CType::Struct(s2)) => s1 == s2,
            (CType::Union(u1), CType::Union(u2)) => u1 == u2,
            // Function types
            (CType::Function(f1), CType::Function(f2)) => f1 == f2,
            // All other types (scalars, void, bool, etc.): direct enum equality
            _ => a_norm == b_norm,
        }
    }

    /// Convert a CType back to a TypeSpecifier (for typeof and __auto_type resolution).
    pub(super) fn ctype_to_type_spec(ctype: &CType) -> TypeSpecifier {
        match ctype {
            CType::Void => TypeSpecifier::Void,
            CType::Bool => TypeSpecifier::Bool,
            CType::Char => TypeSpecifier::Char,
            CType::UChar => TypeSpecifier::UnsignedChar,
            CType::Short => TypeSpecifier::Short,
            CType::UShort => TypeSpecifier::UnsignedShort,
            CType::Int => TypeSpecifier::Int,
            CType::UInt => TypeSpecifier::UnsignedInt,
            CType::Long => TypeSpecifier::Long,
            CType::ULong => TypeSpecifier::UnsignedLong,
            CType::LongLong => TypeSpecifier::LongLong,
            CType::ULongLong => TypeSpecifier::UnsignedLongLong,
            CType::Int128 => TypeSpecifier::Int128,
            CType::UInt128 => TypeSpecifier::UnsignedInt128,
            CType::Float => TypeSpecifier::Float,
            CType::Double => TypeSpecifier::Double,
            CType::LongDouble => TypeSpecifier::LongDouble,
            CType::ComplexFloat => TypeSpecifier::ComplexFloat,
            CType::ComplexDouble => TypeSpecifier::ComplexDouble,
            CType::ComplexLongDouble => TypeSpecifier::ComplexLongDouble,
            CType::Pointer(inner) => TypeSpecifier::Pointer(Box::new(Self::ctype_to_type_spec(inner))),
            CType::Array(elem, size) => TypeSpecifier::Array(
                Box::new(Self::ctype_to_type_spec(elem)),
                size.map(|s| Box::new(Expr::IntLiteral(s as i64, crate::common::source::Span::dummy()))),
            ),
            CType::Struct(key) => {
                // Extract tag name from key (e.g., "struct.Foo" -> "Foo")
                if let Some(tag) = key.strip_prefix("struct.") {
                    TypeSpecifier::Struct(Some(tag.to_string()), None, false, None, None)
                } else {
                    TypeSpecifier::Int // anonymous struct fallback
                }
            }
            CType::Union(key) => {
                // Extract tag name from key (e.g., "union.Bar" -> "Bar")
                if let Some(tag) = key.strip_prefix("union.") {
                    TypeSpecifier::Union(Some(tag.to_string()), None, false, None, None)
                } else {
                    TypeSpecifier::Int // anonymous union fallback
                }
            }
            CType::Enum(et) => {
                TypeSpecifier::Enum(et.name.clone(), None)
            }
            CType::Function { .. } => TypeSpecifier::Int, // function type fallback
        }
    }

    /// Pre-populate typedef mappings for builtin/standard types.
    /// Now inserts CType values directly.
    pub(super) fn seed_builtin_typedefs(&mut self) {
        let builtins: &[(&str, CType)] = &[
            // <stddef.h>
            ("size_t", CType::ULong),
            ("ssize_t", CType::Long),
            ("ptrdiff_t", CType::Long),
            ("wchar_t", CType::Int),
            ("wint_t", CType::UInt),
            // <stdint.h> - exact width types
            ("int8_t", CType::Char),
            ("int16_t", CType::Short),
            ("int32_t", CType::Int),
            ("int64_t", CType::Long),
            ("uint8_t", CType::UChar),
            ("uint16_t", CType::UShort),
            ("uint32_t", CType::UInt),
            ("uint64_t", CType::ULong),
            ("intptr_t", CType::Long),
            ("uintptr_t", CType::ULong),
            ("intmax_t", CType::Long),
            ("uintmax_t", CType::ULong),
            // least types
            ("int_least8_t", CType::Char),
            ("int_least16_t", CType::Short),
            ("int_least32_t", CType::Int),
            ("int_least64_t", CType::Long),
            ("uint_least8_t", CType::UChar),
            ("uint_least16_t", CType::UShort),
            ("uint_least32_t", CType::UInt),
            ("uint_least64_t", CType::ULong),
            // fast types
            ("int_fast8_t", CType::Char),
            ("int_fast16_t", CType::Long),
            ("int_fast32_t", CType::Long),
            ("int_fast64_t", CType::Long),
            ("uint_fast8_t", CType::UChar),
            ("uint_fast16_t", CType::ULong),
            ("uint_fast32_t", CType::ULong),
            ("uint_fast64_t", CType::ULong),
            // <signal.h>
            ("sig_atomic_t", CType::Int),
            // <time.h>
            ("time_t", CType::Long),
            ("clock_t", CType::Long),
            ("timer_t", CType::Pointer(Box::new(CType::Void))),
            ("clockid_t", CType::Int),
            // <sys/types.h>
            ("off_t", CType::Long),
            ("pid_t", CType::Int),
            ("uid_t", CType::UInt),
            ("gid_t", CType::UInt),
            ("mode_t", CType::UInt),
            ("dev_t", CType::ULong),
            ("ino_t", CType::ULong),
            ("nlink_t", CType::ULong),
            ("blksize_t", CType::Long),
            ("blkcnt_t", CType::Long),
            // GNU/glibc common
            ("ulong", CType::ULong),
            ("ushort", CType::UShort),
            ("uint", CType::UInt),
            ("__u8", CType::UChar),
            ("__u16", CType::UShort),
            ("__u32", CType::UInt),
            ("__u64", CType::ULong),
            ("__s8", CType::Char),
            ("__s16", CType::Short),
            ("__s32", CType::Int),
            ("__s64", CType::Long),
            // <locale.h>
            ("locale_t", CType::Pointer(Box::new(CType::Void))),
            // <pthread.h> - opaque types, treat as unsigned long or pointer
            ("pthread_t", CType::ULong),
            ("pthread_mutex_t", CType::Pointer(Box::new(CType::Void))),
            ("pthread_cond_t", CType::Pointer(Box::new(CType::Void))),
            ("pthread_key_t", CType::UInt),
            ("pthread_attr_t", CType::Pointer(Box::new(CType::Void))),
            ("pthread_once_t", CType::Int),
            ("pthread_mutexattr_t", CType::Pointer(Box::new(CType::Void))),
            ("pthread_condattr_t", CType::Pointer(Box::new(CType::Void))),
            // <setjmp.h>
            ("jmp_buf", CType::Pointer(Box::new(CType::Void))),
            ("sigjmp_buf", CType::Pointer(Box::new(CType::Void))),
            // <stdio.h>
            ("FILE", CType::Pointer(Box::new(CType::Void))),
            ("fpos_t", CType::Long),
            // <dirent.h>
            ("DIR", CType::Pointer(Box::new(CType::Void))),
        ];
        for (name, ct) in builtins {
            self.types.typedefs.insert(name.to_string(), ct.clone());
        }
        // Target-dependent va_list definition.
        use crate::backend::Target;
        let va_list_type = match self.target {
            Target::Riscv64 => {
                // RISC-V: va_list = void * (8 bytes, passed by value)
                CType::Pointer(Box::new(CType::Void))
            }
            Target::Aarch64 => {
                // AArch64: va_list is a 32-byte struct, represented as char[32]
                CType::Array(Box::new(CType::Char), Some(32))
            }
            Target::X86_64 => {
                // x86-64: va_list is __va_list_tag[1], 24 bytes, represented as char[24]
                CType::Array(Box::new(CType::Char), Some(24))
            }
        };
        self.types.typedefs.insert("va_list".to_string(), va_list_type.clone());
        self.types.typedefs.insert("__builtin_va_list".to_string(), va_list_type.clone());
        self.types.typedefs.insert("__gnuc_va_list".to_string(), va_list_type);
        // POSIX internal names
        let posix_extras: &[(&str, CType)] = &[
            ("__u_char", CType::UChar),
            ("__u_short", CType::UShort),
            ("__u_int", CType::UInt),
            ("__u_long", CType::ULong),
            ("__int8_t", CType::Char),
            ("__int16_t", CType::Short),
            ("__int32_t", CType::Int),
            ("__int64_t", CType::Long),
            ("__uint8_t", CType::UChar),
            ("__uint16_t", CType::UShort),
            ("__uint32_t", CType::UInt),
            ("__uint64_t", CType::ULong),
        ];
        for (name, ct) in posix_extras {
            self.types.typedefs.insert(name.to_string(), ct.clone());
        }
    }

    /// Seed known libc math function signatures for correct calling convention.
    /// Without these, calls like atanf(1) would pass integer args in %rdi instead of %xmm0.
    /// Helper to insert a builtin function signature into func_meta.
    fn insert_builtin_sig(&mut self, name: &str, return_type: IrType, param_types: Vec<IrType>, param_ctypes: Vec<CType>) {
        self.func_meta.sigs.insert(name.to_string(), FuncSig {
            return_type,
            return_ctype: None,
            param_types,
            param_ctypes,
            param_bool_flags: Vec::new(),
            is_variadic: false,
            sret_size: None,
            two_reg_ret_size: None,
            param_struct_sizes: Vec::new(),
        });
    }

    pub(super) fn seed_libc_math_functions(&mut self) {
        use IrType::*;
        // float func(float) - single-precision math
        let f_f: &[&str] = &[
            "sinf", "cosf", "tanf", "asinf", "acosf", "atanf",
            "sinhf", "coshf", "tanhf", "asinhf", "acoshf", "atanhf",
            "expf", "exp2f", "expm1f", "logf", "log2f", "log10f", "log1pf",
            "sqrtf", "cbrtf", "fabsf", "ceilf", "floorf", "roundf", "truncf",
            "rintf", "nearbyintf", "erff", "erfcf", "tgammaf", "lgammaf",
        ];
        for name in f_f {
            self.insert_builtin_sig(name, F32, vec![F32], Vec::new());
        }
        // double func(double) - double-precision math
        let d_d: &[&str] = &[
            "sin", "cos", "tan", "asin", "acos", "atan",
            "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
            "exp", "exp2", "expm1", "log", "log2", "log10", "log1p",
            "sqrt", "cbrt", "fabs", "ceil", "floor", "round", "trunc",
            "rint", "nearbyint", "erf", "erfc", "tgamma", "lgamma",
        ];
        for name in d_d {
            self.insert_builtin_sig(name, F64, vec![F64], Vec::new());
        }
        // float func(float, float) - two-arg single-precision
        let f_ff: &[&str] = &["atan2f", "powf", "fmodf", "remainderf", "copysignf", "fminf", "fmaxf", "fdimf", "hypotf"];
        for name in f_ff {
            self.insert_builtin_sig(name, F32, vec![F32, F32], Vec::new());
        }
        // double func(double, double) - two-arg double-precision
        let d_dd: &[&str] = &["atan2", "pow", "fmod", "remainder", "copysign", "fmin", "fmax", "fdim", "hypot"];
        for name in d_dd {
            self.insert_builtin_sig(name, F64, vec![F64, F64], Vec::new());
        }
        // int/long returning functions
        self.insert_builtin_sig("abs", I32, vec![I32], Vec::new());
        self.insert_builtin_sig("labs", I64, vec![I64], Vec::new());
        // float/double func(float/double, int)
        self.insert_builtin_sig("ldexpf", F32, vec![F32, I32], Vec::new());
        self.insert_builtin_sig("ldexp", F64, vec![F64, I32], Vec::new());
        self.insert_builtin_sig("scalbnf", F32, vec![F32, I32], Vec::new());
        self.insert_builtin_sig("scalbn", F64, vec![F64, I32], Vec::new());
        // Complex math functions: register return types and param_ctypes.
        // param_types left empty for complex since arg-casting uses param_ctypes for decomposition.
        self.insert_builtin_sig("cabs", F64, Vec::new(), vec![CType::ComplexDouble]);
        self.insert_builtin_sig("cabsf", F32, Vec::new(), vec![CType::ComplexFloat]);
        self.insert_builtin_sig("cabsl", F64, Vec::new(), Vec::new());
        self.insert_builtin_sig("carg", F64, Vec::new(), vec![CType::ComplexDouble]);
        self.insert_builtin_sig("cargf", F32, Vec::new(), vec![CType::ComplexFloat]);
        self.insert_builtin_sig("creal", F64, Vec::new(), vec![CType::ComplexDouble]);
        self.insert_builtin_sig("cimag", F64, Vec::new(), vec![CType::ComplexDouble]);
        self.insert_builtin_sig("crealf", F32, Vec::new(), vec![CType::ComplexFloat]);
        self.insert_builtin_sig("cimagf", F32, Vec::new(), vec![CType::ComplexFloat]);
        // Functions returning _Complex double (real in xmm0, imag in xmm1):
        let cd_cd: &[&str] = &[
            "csqrt", "cexp", "clog", "csin", "ccos", "ctan",
            "casin", "cacos", "catan", "csinh", "ccosh", "ctanh",
            "casinh", "cacosh", "catanh", "conj",
        ];
        for name in cd_cd {
            self.insert_builtin_sig(name, F64, Vec::new(), vec![CType::ComplexDouble]);
            self.types.func_return_ctypes.insert(name.to_string(), CType::ComplexDouble);
        }

        // Functions returning _Complex float (packed two F32 in I64):
        let cf_cf: &[&str] = &[
            "csqrtf", "cexpf", "clogf", "csinf", "ccosf", "ctanf",
            "casinf", "cacosf", "catanf", "csinhf", "ccoshf", "ctanhf",
            "casinhf", "cacoshf", "catanhf", "conjf",
        ];
        for name in cf_cf {
            self.insert_builtin_sig(name, F64, Vec::new(), vec![CType::ComplexFloat]);
            self.types.func_return_ctypes.insert(name.to_string(), CType::ComplexFloat);
        }

        // cpow/cpowf take two complex args
        self.insert_builtin_sig("cpow", F64, Vec::new(), vec![CType::ComplexDouble, CType::ComplexDouble]);
        self.types.func_return_ctypes.insert("cpow".to_string(), CType::ComplexDouble);
        self.insert_builtin_sig("cpowf", F64, Vec::new(), vec![CType::ComplexFloat, CType::ComplexFloat]);
        self.types.func_return_ctypes.insert("cpowf".to_string(), CType::ComplexFloat);
    }

    pub(super) fn type_spec_to_ir(&self, ts: &TypeSpecifier) -> IrType {
        match ts {
            TypeSpecifier::Void => IrType::Void,
            TypeSpecifier::Bool => IrType::U8,
            TypeSpecifier::Char => IrType::I8,
            TypeSpecifier::UnsignedChar => IrType::U8,
            TypeSpecifier::Short => IrType::I16,
            TypeSpecifier::UnsignedShort => IrType::U16,
            TypeSpecifier::Int | TypeSpecifier::Signed => IrType::I32,
            TypeSpecifier::UnsignedInt | TypeSpecifier::Unsigned => IrType::U32,
            TypeSpecifier::Long | TypeSpecifier::LongLong => IrType::I64,
            TypeSpecifier::UnsignedLong | TypeSpecifier::UnsignedLongLong => IrType::U64,
            TypeSpecifier::Int128 => IrType::I128,
            TypeSpecifier::UnsignedInt128 => IrType::U128,
            TypeSpecifier::Float => IrType::F32,
            TypeSpecifier::Double => IrType::F64,
            TypeSpecifier::LongDouble => IrType::F128,
            TypeSpecifier::ComplexFloat | TypeSpecifier::ComplexDouble | TypeSpecifier::ComplexLongDouble => IrType::Ptr,
            TypeSpecifier::Pointer(_) => IrType::Ptr,
            TypeSpecifier::Array(_, _) => IrType::Ptr,
            TypeSpecifier::Struct(..) | TypeSpecifier::Union(..) => IrType::Ptr,
            TypeSpecifier::Enum(_, _) => IrType::I32,
            TypeSpecifier::TypedefName(name) => {
                // Resolve typedef through CType
                if let Some(ctype) = self.types.typedefs.get(name) {
                    IrType::from_ctype(ctype)
                } else {
                    IrType::I64 // fallback for unresolved typedef
                }
            }
            TypeSpecifier::Typeof(expr) => {
                if let Some(ctype) = self.get_expr_ctype(expr) {
                    IrType::from_ctype(&ctype)
                } else {
                    IrType::I64
                }
            }
            TypeSpecifier::TypeofType(inner) => self.type_spec_to_ir(inner),
            TypeSpecifier::FunctionPointer(_, _, _) => IrType::Ptr, // function pointer is a pointer
            // AutoType should be resolved before reaching here (in lower_local_decl)
            TypeSpecifier::AutoType => IrType::I64,
        }
    }

    /// Get the (size, alignment) for a scalar type specifier. Returns None for
    /// compound types (arrays, structs, unions) that need recursive computation.
    fn scalar_type_size_align(ts: &TypeSpecifier) -> Option<(usize, usize)> {
        // For scalar types, size == alignment on x86-64 (except LongDouble).
        match ts {
            TypeSpecifier::Void | TypeSpecifier::Bool => Some((1, 1)),
            TypeSpecifier::Char | TypeSpecifier::UnsignedChar => Some((1, 1)),
            TypeSpecifier::Short | TypeSpecifier::UnsignedShort => Some((2, 2)),
            TypeSpecifier::Int | TypeSpecifier::UnsignedInt
            | TypeSpecifier::Signed | TypeSpecifier::Unsigned => Some((4, 4)),
            TypeSpecifier::Long | TypeSpecifier::UnsignedLong
            | TypeSpecifier::LongLong | TypeSpecifier::UnsignedLongLong => Some((8, 8)),
            TypeSpecifier::Int128 | TypeSpecifier::UnsignedInt128 => Some((16, 16)),
            TypeSpecifier::Float => Some((4, 4)),
            TypeSpecifier::Double => Some((8, 8)),
            TypeSpecifier::LongDouble => Some((16, 16)),
            TypeSpecifier::ComplexFloat => Some((8, 4)),
            TypeSpecifier::ComplexDouble => Some((16, 8)),
            TypeSpecifier::ComplexLongDouble => Some((32, 16)),
            TypeSpecifier::Pointer(_) => Some((8, 8)),
            TypeSpecifier::Enum(_, _) => Some((4, 4)),
            TypeSpecifier::TypedefName(_) => Some((8, 8)), // fallback for unresolved typedefs
            _ => None,
        }
    }

    /// Look up a struct/union layout by tag name, returning the full layout.
    fn get_struct_union_layout_by_tag(&self, kind: &str, tag: &str) -> Option<&StructLayout> {
        let key = format!("{}.{}", kind, tag);
        self.types.struct_layouts.get(&key)
    }

    /// Get the struct/union layout for a resolved TypeSpecifier.
    /// Handles both inline field definitions and tag-only forward references.
    fn struct_union_layout(&self, ts: &TypeSpecifier) -> Option<StructLayout> {
        match ts {
            TypeSpecifier::Struct(tag, Some(fields), is_packed, pragma_pack, _) => {
                // Use cached layout for tagged structs
                if let Some(tag) = tag {
                    if let Some(layout) = self.types.struct_layouts.get(&format!("struct.{}", tag)) {
                        return Some(layout.clone());
                    }
                }
                let max_field_align = if *is_packed { Some(1) } else { *pragma_pack };
                Some(self.compute_struct_union_layout_packed(fields, false, max_field_align))
            }
            TypeSpecifier::Union(tag, Some(fields), is_packed, pragma_pack, _) => {
                // Use cached layout for tagged unions
                if let Some(tag) = tag {
                    if let Some(layout) = self.types.struct_layouts.get(&format!("union.{}", tag)) {
                        return Some(layout.clone());
                    }
                }
                let max_field_align = if *is_packed { Some(1) } else { *pragma_pack };
                Some(self.compute_struct_union_layout_packed(fields, true, max_field_align))
            }
            TypeSpecifier::Struct(Some(tag), None, _, _, _) =>
                self.get_struct_union_layout_by_tag("struct", tag).cloned(),
            TypeSpecifier::Union(Some(tag), None, _, _, _) =>
                self.get_struct_union_layout_by_tag("union", tag).cloned(),
            _ => None,
        }
    }

    pub(super) fn compute_struct_union_layout_packed(&self, fields: &[StructFieldDecl], is_union: bool, max_field_align: Option<usize>) -> StructLayout {
        let struct_fields: Vec<StructField> = fields.iter().map(|f| {
            let bit_width = f.bit_width.as_ref().and_then(|bw| {
                self.eval_const_expr(bw).and_then(|c| c.to_u32())
            });
            let mut ty = self.struct_field_ctype(f);
            // GCC treats enum bitfields as unsigned (see struct_or_union_to_ctype)
            if bit_width.is_some() {
                if let TypeSpecifier::Enum(_, _) = &f.type_spec {
                    if ty == CType::Int {
                        ty = CType::UInt;
                    }
                }
            }
            StructField {
                name: f.name.clone().unwrap_or_default(),
                ty,
                bit_width,
                alignment: f.alignment,
            }
        }).collect();
        if is_union {
            StructLayout::for_union(&struct_fields, &self.types)
        } else {
            StructLayout::for_struct_with_packing(&struct_fields, max_field_align, &self.types)
        }
    }

    pub(super) fn sizeof_type(&self, ts: &TypeSpecifier) -> usize {
        // Handle TypedefName through CType
        if let TypeSpecifier::TypedefName(name) = ts {
            if let Some(ctype) = self.types.typedefs.get(name) {
                return ctype.size_ctx(&self.types.struct_layouts);
            }
            return 8; // fallback
        }
        let ts = self.resolve_type_spec(ts);
        if let Some((size, _)) = Self::scalar_type_size_align(ts) {
            return size;
        }
        if let TypeSpecifier::Array(elem, Some(size_expr)) = ts {
            let elem_size = self.sizeof_type(elem);
            return self.expr_as_array_size(size_expr)
                .map(|n| elem_size * n as usize)
                .unwrap_or(elem_size);
        }
        self.struct_union_layout(ts).map(|l| l.size).unwrap_or(8)
    }

    /// Compute the alignment of a type in bytes (_Alignof).
    pub(super) fn alignof_type(&self, ts: &TypeSpecifier) -> usize {
        // Handle TypedefName through CType
        if let TypeSpecifier::TypedefName(name) = ts {
            if let Some(ctype) = self.types.typedefs.get(name) {
                return ctype.align_ctx(&self.types.struct_layouts);
            }
            return 8; // fallback
        }
        let ts = self.resolve_type_spec(ts);
        if let Some((_, align)) = Self::scalar_type_size_align(ts) {
            return align;
        }
        if let TypeSpecifier::Array(elem, _) = ts {
            return self.alignof_type(elem);
        }
        self.struct_union_layout(ts).map(|l| l.align).unwrap_or(8)
    }

    /// Collect array dimensions from derived declarators.
    /// Returns None for unsized dimensions (e.g., `int arr[]`).
    fn collect_derived_array_dims(&self, derived: &[DerivedDeclarator]) -> Vec<Option<usize>> {
        derived.iter().filter_map(|d| {
            if let DerivedDeclarator::Array(size_expr) = d {
                Some(size_expr.as_ref().and_then(|e| self.expr_as_array_size(e).map(|n| n as usize)))
            } else {
                None
            }
        }).collect()
    }

    /// Compute strides from an array of dimension sizes and a base element size.
    /// stride[i] = product(dims[i+1..]) * base_elem_size.
    /// E.g., dims=[3,4], base=4 -> strides=[16, 4].
    fn compute_strides_from_dims(dims: &[usize], base_elem_size: usize) -> Vec<usize> {
        let mut strides = Vec::with_capacity(dims.len());
        for i in 0..dims.len() {
            let stride: usize = dims[i+1..].iter().product::<usize>().max(1) * base_elem_size;
            strides.push(stride);
        }
        strides
    }

    /// For a pointer-to-array parameter type (e.g., Pointer(Array(Array(Int, 4), 3))),
    /// compute the array dimension strides for multi-dimensional subscript access.
    /// Returns strides for depth 0, 1, 2, ... where depth 0 is the outermost subscript.
    /// E.g., for int (*arr)[3][4]: strides = [3*4*4=48, 4*4=16, 4]
    /// For int (*arr)[3]: strides = [3*4=12, 4]
    pub(super) fn compute_ptr_array_strides(&self, type_spec: &TypeSpecifier) -> Vec<usize> {
        let ts = self.resolve_type_spec(type_spec);
        if let TypeSpecifier::Pointer(inner) = ts {
            // Collect dimensions from nested Array types
            let mut dims: Vec<usize> = Vec::new();
            let mut current = &*inner;
            loop {
                let resolved = self.resolve_type_spec(current);
                if let TypeSpecifier::Array(elem, size_expr) = resolved {
                    let n = size_expr.as_ref().and_then(|e| self.expr_as_array_size(e)).unwrap_or(1);
                    dims.push(n as usize);
                    current = &*elem;
                } else {
                    break;
                }
            }
            if dims.is_empty() {
                return vec![];
            }
            // Compute base element size (the innermost non-array type)
            let base_elem_size = self.sizeof_type(current);
            // Strides: [full_size, stride_for_dim_1, ..., base_elem_size]
            // full_size = product(all_dims) * base, then per-dim strides
            let full_size: usize = dims.iter().product::<usize>() * base_elem_size;
            let mut strides = vec![full_size];
            strides.extend(Self::compute_strides_from_dims(&dims, base_elem_size));
            strides
        } else {
            // Fall back to CType for typedef'd pointer-to-array types
            let ctype = self.type_spec_to_ctype(type_spec);
            if let CType::Pointer(ref inner_ct) = ctype {
                let mut dims: Vec<usize> = Vec::new();
                let mut current_ct = inner_ct.as_ref();
                while let CType::Array(elem_ct, size) = current_ct {
                    dims.push(size.unwrap_or(1));
                    current_ct = elem_ct.as_ref();
                }
                if dims.is_empty() {
                    return vec![];
                }
                let base_elem_size = current_ct.size_ctx(&self.types.struct_layouts).max(1);
                let full_size: usize = dims.iter().product::<usize>() * base_elem_size;
                let mut strides = vec![full_size];
                strides.extend(Self::compute_strides_from_dims(&dims, base_elem_size));
                strides
            } else {
                vec![]
            }
        }
    }

    /// Compute allocation info for a declaration.
    /// Returns (alloc_size, elem_size, is_array, is_pointer, array_dim_strides).
    /// For multi-dimensional arrays like int a[2][3], array_dim_strides = [12, 4]
    /// (stride for dim 0 = 3*4=12, stride for dim 1 = 4).
    pub(super) fn compute_decl_info(&self, ts: &TypeSpecifier, derived: &[DerivedDeclarator]) -> (usize, usize, bool, bool, Vec<usize>) {
        let ts = self.resolve_type_spec(ts);
        // Resolve the type spec through CType for typedef detection
        let resolved_ctype = self.type_spec_to_ctype(ts);
        // Check for pointer declarators (from derived or from the resolved type itself)
        let has_pointer = derived.iter().any(|d| matches!(d, DerivedDeclarator::Pointer))
            || matches!(ts, TypeSpecifier::Pointer(_))
            || matches!(resolved_ctype, CType::Pointer(_));

        let has_array = derived.iter().any(|d| matches!(d, DerivedDeclarator::Array(_)))
            || matches!(resolved_ctype, CType::Array(_, _));

        // Handle pointer and array combinations
        if has_pointer && !has_array {
            // Simple pointer: int *p, or typedef'd pointer (e.g., typedef struct Foo *FooPtr)
            let ptr_count = derived.iter().filter(|d| matches!(d, DerivedDeclarator::Pointer)).count();
            let elem_size = if let TypeSpecifier::Pointer(inner) = ts {
                if ptr_count >= 1 {
                    8
                } else {
                    self.sizeof_type(inner)
                }
            } else if let CType::Pointer(ref inner_ct) = resolved_ctype {
                // Pointer from typedef resolution
                if ptr_count >= 1 {
                    8
                } else {
                    inner_ct.size_ctx(&self.types.struct_layouts)
                }
            } else if ptr_count >= 2 {
                8
            } else {
                self.sizeof_type(ts)
            };
            return (8, elem_size, false, true, vec![]);
        }
        if has_pointer && has_array {
            // Determine whether this is "array of pointers" or "pointer to array"
            // by looking at the LAST element of the derived list (outermost type wrapper).
            // The last element determines what the overall declaration IS:
            // - Last is Array: it's an array (of pointers to something)
            //   e.g., int *arr[3] -> derived=[Pointer, Array(3)] -> array of ptrs
            //   e.g., int (*ptrs[3])[4] -> derived=[Array(4), Pointer, Array(3)] -> array of ptrs-to-arrays
            // - Last is Pointer: it's a pointer (to an array)
            //   e.g., int (*p)[5] -> derived=[Array(5), Pointer] -> pointer to array
            let has_func_ptr = derived.iter().any(|d| matches!(d,
                DerivedDeclarator::FunctionPointer(_, _) | DerivedDeclarator::Function(_, _)));

            // If pointer is from resolved type spec (not in derived), and array is in derived,
            // this is an array of typedef'd pointers
            let ptr_pos = derived.iter().position(|d| matches!(d, DerivedDeclarator::Pointer));
            let pointer_from_type_spec = ptr_pos.is_none() && (matches!(ts, TypeSpecifier::Pointer(_)) || matches!(resolved_ctype, CType::Pointer(_)));

            // Check if the outermost (last) derived element is an Array
            let last_is_array = matches!(derived.last(), Some(DerivedDeclarator::Array(_)));

            if has_func_ptr || pointer_from_type_spec || last_is_array {
                // Array of pointers (or array of pointers-to-arrays, etc.)
                // Each element is a pointer (8 bytes).
                // Collect the variable's own array dimensions:
                // - For regular pointer arrays like int *arr[3] (derived=[Pointer, Array(3)]):
                //   Array dims AFTER the last Pointer are the variable's dimensions.
                // - For function pointer arrays like int (*ops[3])(int,int)
                //   (derived=[Array(3), Pointer, FunctionPointer(...)]):
                //   Array dims BEFORE the Pointer are the variable's dimensions,
                //   because the Pointer+FunctionPointer group describes the element type.
                let last_ptr_pos = derived.iter().rposition(|d| matches!(d, DerivedDeclarator::Pointer));
                let array_dims: Vec<Option<usize>> = if let Some(lpp) = last_ptr_pos {
                    // First try: collect Array dims after the last pointer
                    let after_dims: Vec<Option<usize>> = derived[lpp + 1..].iter().filter_map(|d| {
                        if let DerivedDeclarator::Array(size_expr) = d {
                            Some(size_expr.as_ref().and_then(|e| self.expr_as_array_size(e).map(|n| n as usize)))
                        } else {
                            None
                        }
                    }).collect();
                    if !after_dims.is_empty() {
                        after_dims
                    } else if has_func_ptr {
                        // For function pointer arrays, array dims come BEFORE the
                        // Pointer+FunctionPointer group (e.g., [Array(3), Pointer, FuncPtr])
                        derived[..lpp].iter().filter_map(|d| {
                            if let DerivedDeclarator::Array(size_expr) = d {
                                Some(size_expr.as_ref().and_then(|e| self.expr_as_array_size(e).map(|n| n as usize)))
                            } else {
                                None
                            }
                        }).collect()
                    } else {
                        after_dims
                    }
                } else {
                    // Pointer comes from type spec (typedef'd pointer/func ptr),
                    // not from derived list. All array dims in derived belong to
                    // the variable's own dimensions.
                    self.collect_derived_array_dims(derived)
                };
                let resolved_dims: Vec<usize> = array_dims.iter().map(|d| d.unwrap_or(256)).collect();
                let total_size: usize = resolved_dims.iter().product::<usize>() * 8;
                let strides = if resolved_dims.len() > 1 {
                    Self::compute_strides_from_dims(&resolved_dims, 8)
                } else {
                    vec![8]  // 1D pointer array: stride is just pointer size
                };
                return (total_size, 8, true, false, strides);
            }
            // Pointer to array (e.g., int (*p)[5]) - treat as pointer
            // Compute strides from the Array dims BEFORE the pointer in the derived list
            // (these are the pointed-to array dimensions)
            let array_dims: Vec<usize> = derived.iter()
                .take_while(|d| !matches!(d, DerivedDeclarator::Pointer))
                .filter_map(|d| {
                    if let DerivedDeclarator::Array(size_expr) = d {
                        Some(size_expr.as_ref()
                            .and_then(|e| self.expr_as_array_size(e).map(|n| n as usize))
                            .unwrap_or(1))
                    } else {
                        None
                    }
                }).collect();
            let base_elem_size = self.sizeof_type(ts);
            let full_array_size: usize = if array_dims.is_empty() {
                base_elem_size
            } else {
                array_dims.iter().product::<usize>() * base_elem_size
            };
            // strides[0] = full pointed-to array size, then per-dim strides
            let mut strides = vec![full_array_size];
            if !array_dims.is_empty() {
                strides.extend(Self::compute_strides_from_dims(&array_dims, base_elem_size));
            }
            let elem_size = full_array_size;
            return (8, elem_size, false, true, strides);
        }

        // If the resolved type itself is an Array (e.g., va_list = Array(Char, 24),
        // or typedef'd multi-dimensional arrays like typedef int arr_t[2][3])
        // and there are no derived array declarators, handle it as an array type.
        let derived_has_array = derived.iter().any(|d| matches!(d, DerivedDeclarator::Array(_)));
        if !derived_has_array && !has_pointer {
            // Check both TypeSpecifier::Array and CType::Array (for typedef'd arrays)
            let is_ts_array = matches!(ts, TypeSpecifier::Array(_, _));
            let is_ctype_array = matches!(resolved_ctype, CType::Array(_, _));
            if is_ts_array {
                let all_dims = self.collect_type_array_dims(ts);
                let mut inner = ts;
                while let TypeSpecifier::Array(elem, _) = inner {
                    inner = elem.as_ref();
                }
                let base_elem_size = self.sizeof_type(inner).max(1);
                let total: usize = all_dims.iter().product::<usize>() * base_elem_size;
                let strides = Self::compute_strides_from_dims(&all_dims, base_elem_size);
                let elem_size = if strides.len() > 1 { strides[0] } else { base_elem_size };
                return (total, elem_size, true, false, strides);
            } else if is_ctype_array && !is_ts_array {
                // Typedef'd array (e.g., va_list = CType::Array(Char, 24))
                let all_dims = Self::collect_ctype_array_dims(&resolved_ctype);
                let base_elem_size = Self::ctype_innermost_elem_size(&resolved_ctype, &self.types.struct_layouts);
                let total: usize = all_dims.iter().product::<usize>() * base_elem_size;
                let strides = Self::compute_strides_from_dims(&all_dims, base_elem_size);
                let elem_size = if strides.len() > 1 { strides[0] } else { base_elem_size };
                return (total, elem_size, true, false, strides);
            }
        }

        // Check for array declarators - collect all dimensions
        let array_dims = self.collect_derived_array_dims(derived);

        if !array_dims.is_empty() {
            let has_func_ptr = derived.iter().any(|d| matches!(d,
                DerivedDeclarator::Function(_, _) | DerivedDeclarator::FunctionPointer(_, _)));
            // Account for array dimensions in the type specifier itself
            // Check both TypeSpecifier::Array and CType::Array (for typedef'd arrays)
            let type_dims = if matches!(ts, TypeSpecifier::Array(_, _)) {
                self.collect_type_array_dims(ts)
            } else if matches!(resolved_ctype, CType::Array(_, _)) {
                Self::collect_ctype_array_dims(&resolved_ctype)
            } else {
                vec![]
            };

            let base_elem_size = if has_func_ptr {
                8
            } else if !type_dims.is_empty() {
                // Use CType for innermost element size (works for both direct and typedef'd arrays)
                Self::ctype_innermost_elem_size(&resolved_ctype, &self.types.struct_layouts)
            } else {
                resolved_ctype.size_ctx(&self.types.struct_layouts).max(1)
            };

            // Combine: derived dims come first (outermost), then type dims
            let all_dims: Vec<usize> = array_dims.iter().map(|d| d.unwrap_or(256))
                .chain(type_dims.iter().copied())
                .collect();

            // Compute total size = product of all dims * base_elem_size
            let total: usize = all_dims.iter().product::<usize>() * base_elem_size;

            // Compute strides: stride[i] = product of dims[i+1..] * base_elem_size
            let strides = Self::compute_strides_from_dims(&all_dims, base_elem_size);

            // elem_size is the stride of the outermost dimension (for 1D compat, it's base_elem_size)
            let elem_size = if strides.len() > 1 { strides[0] } else { base_elem_size };

            return (total, elem_size, true, false, strides);
        }

        // For struct/union types, use their layout size
        if let Some(layout) = self.get_struct_layout_for_type(ts) {
            return (layout.size, 0, false, false, vec![]);
        }
        // Also check CType for typedef'd structs/unions
        if matches!(resolved_ctype, CType::Struct(_) | CType::Union(_)) {
            if let Some(layout) = self.struct_layout_from_ctype(&resolved_ctype) {
                return (layout.size, 0, false, false, vec![]);
            }
        }

        // Regular scalar - use sizeof_type for the allocation size
        // (8 bytes for most scalars, 16 for long double)
        let scalar_size = resolved_ctype.size_ctx(&self.types.struct_layouts).max(8);
        (scalar_size, 0, false, false, vec![])
    }

    /// For Array(Array(Int, 3), 2), returns [2, 3] (but we skip the outermost
    /// since that comes from the derived declarator).
    fn collect_type_array_dims(&self, ts: &TypeSpecifier) -> Vec<usize> {
        let mut dims = Vec::new();
        let mut current_owned = ts.clone();
        loop {
            let resolved = self.resolve_type_spec(&current_owned);
            if let TypeSpecifier::Array(inner, Some(size_expr)) = &resolved {
                if let Some(n) = self.expr_as_array_size(size_expr) {
                    dims.push(n as usize);
                }
                current_owned = inner.as_ref().clone();
            } else if let TypeSpecifier::TypedefName(name) = &resolved {
                // Follow typedef to CType for typedef'd arrays
                if let Some(ctype) = self.types.typedefs.get(name) {
                    if matches!(ctype, CType::Array(_, _)) {
                        dims.extend(Self::collect_ctype_array_dims(ctype));
                    }
                }
                break;
            } else {
                break;
            }
        }
        dims
    }

    /// Collect array dimensions from a CType::Array chain.
    /// For CType::Array(CType::Array(Int, Some(3)), Some(2)), returns [2, 3].
    fn collect_ctype_array_dims(ctype: &CType) -> Vec<usize> {
        let mut dims = Vec::new();
        let mut current = ctype;
        while let CType::Array(inner, size) = current {
            dims.push(size.unwrap_or(1));
            current = inner.as_ref();
        }
        dims
    }

    /// Get the innermost element size for a CType::Array chain.
    fn ctype_innermost_elem_size(ctype: &CType, layouts: &crate::common::fx_hash::FxHashMap<String, StructLayout>) -> usize {
        let mut current = ctype;
        while let CType::Array(inner, _) = current {
            current = inner.as_ref();
        }
        current.size_ctx(layouts).max(1)
    }

    /// Map an element size in bytes to an appropriate IrType.
    pub(super) fn ir_type_for_elem_size(&self, size: usize) -> IrType {
        match size {
            1 => IrType::I8,
            2 => IrType::I16,
            4 => IrType::I32,
            8 => IrType::I64,
            _ => IrType::I64,
        }
    }

    /// Build the CType for a function parameter, correctly handling function
    /// pointer parameters (both explicit syntax and typedef'd).
    ///
    /// For explicit function pointer params like `void (*callback)(int, int)`,
    /// the parser sets `fptr_params` to Some(...) and we build
    /// CType::Pointer(CType::Function(...)).
    ///
    /// For typedef'd function pointer params like `lua_Alloc f`, the parser
    /// doesn't set `fptr_params` (the function pointer nature is hidden in the
    /// typedef). We detect this by checking if the resolved type spec has
    /// DerivedDeclarator::FunctionPointer info stored in function_typedefs or
    /// by checking the original typedef declaration.
    pub(super) fn param_ctype(&self, param: &crate::frontend::parser::ast::ParamDecl) -> CType {
        // Case 1: explicit function pointer syntax - fptr_params is set
        if let Some(ref fptr_params) = param.fptr_params {
            let return_ctype = self.type_spec_to_ctype(&param.type_spec);
            // Peel ALL Pointer layers from the return type.
            // For `int (*f)(...)`, type_spec is Pointer(Int) → 1 pointer layer → return Int
            // For `int (**fpp)(...)`, type_spec is Pointer(Pointer(Int)) → 2 layers
            //   The first layer is the (*name) indirection, extra layers are
            //   pointer-to-function-pointer levels that wrap the result.
            let mut actual_return = return_ctype;
            let mut extra_ptr_layers = 0usize;
            while let CType::Pointer(inner) = actual_return {
                actual_return = *inner;
                extra_ptr_layers += 1;
            }
            // First pointer layer is the function-pointer indirection (*name),
            // remaining layers are pointer-to-function-pointer wrapping.
            let wrap_layers = extra_ptr_layers.saturating_sub(1);

            let param_types: Vec<(CType, Option<String>)> = fptr_params.iter()
                .map(|p| (self.type_spec_to_ctype(&p.type_spec), p.name.clone()))
                .collect();
            let func_type = CType::Function(Box::new(crate::common::types::FunctionType {
                return_type: actual_return,
                params: param_types,
                variadic: false,
            }));
            let mut result = CType::Pointer(Box::new(func_type));
            for _ in 0..wrap_layers {
                result = CType::Pointer(Box::new(result));
            }
            return result;
        }

        // Case 2: typedef'd function pointer (e.g., lua_Alloc f)
        // Check if the parameter's type resolves to a typedef that was
        // declared as a function pointer typedef
        if let TypeSpecifier::TypedefName(tname) = &param.type_spec {
            if self.is_typedef_function_pointer(tname) {
                // Build the full function pointer CType from the typedef info
                if let Some(fptr_ctype) = self.build_function_pointer_ctype_from_typedef(tname) {
                    return fptr_ctype;
                }
            }
        }

        // Default: use the standard type_spec_to_ctype
        self.type_spec_to_ctype(&param.type_spec)
    }

    /// Check if a typedef name refers to a function pointer type.
    /// This handles typedefs like `typedef void *(*lua_Alloc)(void *, ...)`.
    fn is_typedef_function_pointer(&self, tname: &str) -> bool {
        // Check if the typedef's resolved type is a Pointer AND the original
        // typedef had a FunctionPointer derived declarator.
        // We track this via function_typedefs which stores function type info
        // for both bare function typedefs and function pointer typedefs.
        //
        // However, function_typedefs only stores bare function typedefs
        // (typedef int func_t(int)), not function pointer typedefs.
        // For function pointer typedefs, we check the resolved typedef: if it's
        // a Pointer type and the original declaration context implies function pointer.
        //
        // Heuristic: if the typedef resolves to Pointer(X) and there's also a
        // function_typedefs entry, it was a function pointer typedef. But this
        // isn't reliable. Instead, track function pointer typedefs explicitly.
        self.types.func_ptr_typedefs.contains(tname)
    }

    /// Build a CType::Pointer(CType::Function(...)) from a function pointer typedef.
    fn build_function_pointer_ctype_from_typedef(&self, tname: &str) -> Option<CType> {
        // Look up the stored function pointer typedef info
        if let Some(fti) = self.types.func_ptr_typedef_info.get(tname) {
            let return_ctype = self.type_spec_to_ctype(&fti.return_type);
            let param_types: Vec<(CType, Option<String>)> = fti.params.iter()
                .map(|p| (self.type_spec_to_ctype(&p.type_spec), p.name.clone()))
                .collect();
            let func_type = CType::Function(Box::new(crate::common::types::FunctionType {
                return_type: return_ctype,
                params: param_types,
                variadic: fti.variadic,
            }));
            return Some(CType::Pointer(Box::new(func_type)));
        }
        None
    }

    /// Convert a TypeSpecifier to CType (for struct layout computation).
    /// Delegates to the shared `TypeConvertContext::resolve_type_spec_to_ctype` default
    /// method, which handles all 22 primitive types and delegates struct/union/enum/typedef
    /// to lowering-specific trait methods.
    pub(super) fn type_spec_to_ctype(&self, ts: &TypeSpecifier) -> CType {
        use crate::common::type_builder::TypeConvertContext;
        self.resolve_type_spec_to_ctype(ts)
    }

    /// Convert a struct or union TypeSpecifier to CType.
    /// `is_union` selects between struct and union semantics.
    /// `is_packed` indicates __attribute__((packed)).
    /// `pragma_pack` is the #pragma pack(N) alignment, if any.
    fn struct_or_union_to_ctype(
        &self,
        name: &Option<String>,
        fields: &Option<Vec<StructFieldDecl>>,
        is_union: bool,
        is_packed: bool,
        pragma_pack: Option<usize>,
        struct_aligned: Option<usize>,
    ) -> CType {
        let prefix = if is_union { "union" } else { "struct" };
        let wrap = |key: String| -> CType {
            if is_union { CType::Union(key) } else { CType::Struct(key) }
        };
        // __attribute__((packed)) forces alignment 1; #pragma pack(N) caps to N.
        let max_field_align = if is_packed { Some(1) } else { pragma_pack };

        if let Some(fs) = fields {
            // Inline definition with fields: check if register_struct_type already
            // inserted the definitive layout for this named struct/union.
            // If so, skip re-computing and re-inserting (which would corrupt the
            // scope undo-log with a redundant shadow entry).
            // Only skip when the existing layout has fields (not a forward-declaration stub).
            if let Some(tag) = name {
                let cache_key = format!("{}.{}", prefix, tag);
                if let Some(existing) = self.types.struct_layouts.get(&cache_key) {
                    if !existing.fields.is_empty() {
                        let result = wrap(cache_key.clone());
                        self.types.ctype_cache.borrow_mut().insert(cache_key, result.clone());
                        return result;
                    }
                }
            }
            let struct_fields: Vec<StructField> = fs.iter().map(|f| {
                let bit_width = f.bit_width.as_ref().and_then(|bw| {
                    self.eval_const_expr(bw).and_then(|c| c.to_u32())
                });
                let mut ty = self.struct_field_ctype(f);
                // GCC treats enum bitfields as unsigned: values are zero-extended
                // on load, not sign-extended. When the type specifier is Enum but
                // type_spec_to_ctype collapsed it to CType::Int, switch to UInt
                // for bitfield storage so that extract_bitfield uses the unsigned path.
                if bit_width.is_some() {
                    if let TypeSpecifier::Enum(_, _) = &f.type_spec {
                        if ty == CType::Int {
                            ty = CType::UInt;
                        }
                    }
                }
                StructField {
                    name: f.name.clone().unwrap_or_default(),
                    ty,
                    bit_width,
                    alignment: f.alignment,
                }
            }).collect();
            let mut layout = if is_union {
                StructLayout::for_union(&struct_fields, &self.types)
            } else {
                StructLayout::for_struct_with_packing(&struct_fields, max_field_align, &self.types)
            };
            // Apply struct-level __attribute__((aligned(N))): sets minimum alignment
            if let Some(a) = struct_aligned {
                if a > layout.align {
                    layout.align = a;
                    let mask = layout.align - 1;
                    layout.size = (layout.size + mask) & !mask;
                }
            }
            let key = if let Some(tag) = name {
                format!("{}.{}", prefix, tag)
            } else {
                let id = self.types.next_anon_struct_id();
                format!("__anon_struct_{}", id)
            };
            self.types.insert_struct_layout_scoped_from_ref(&key, layout);
            self.types.invalidate_ctype_cache_scoped_from_ref(&key);
            let result = wrap(key.clone());
            self.types.ctype_cache.borrow_mut().insert(key, result.clone());
            result
        } else if let Some(tag) = name {
            let key = format!("{}.{}", prefix, tag);
            // Check cache first
            if let Some(cached) = self.types.ctype_cache.borrow().get(&key) {
                return cached.clone();
            }
            // Forward declaration: insert an empty layout if not already present
            if self.types.struct_layouts.get(&key).is_none() {
                let empty_layout = StructLayout {
                    fields: Vec::new(),
                    size: 0,
                    align: 1,
                    is_union,
                    is_transparent_union: false,
                };
                self.types.insert_struct_layout_from_ref(&key, empty_layout);
            }
            let result = wrap(key.clone());
            self.types.ctype_cache.borrow_mut().insert(key, result.clone());
            result
        } else {
            // Anonymous forward declaration (no name, no fields)
            let id = self.types.next_anon_struct_id();
            let key = format!("__anon_struct_{}", id);
            let empty_layout = StructLayout {
                fields: Vec::new(),
                size: 0,
                align: 1,
                is_union,
                is_transparent_union: false,
            };
            self.types.insert_struct_layout_from_ref(&key, empty_layout);
            wrap(key)
        }
    }

    /// Get the CType for a struct field declaration, accounting for derived declarators.
    /// For simple fields (derived is empty), just converts type_spec.
    /// For complex fields (function pointers, etc.), uses build_full_ctype.
    pub(super) fn struct_field_ctype(&self, f: &StructFieldDecl) -> CType {
        if f.derived.is_empty() {
            self.type_spec_to_ctype(&f.type_spec)
        } else {
            self.build_full_ctype(&f.type_spec, &f.derived)
        }
    }

    /// Build a full CType from a TypeSpecifier and DerivedDeclarator chain.
    /// Delegates to the shared type_builder module for canonical inside-out
    /// declarator application logic.
    pub(super) fn build_full_ctype(&self, type_spec: &TypeSpecifier, derived: &[DerivedDeclarator]) -> CType {
        type_builder::build_full_ctype(self, type_spec, derived)
    }

}

/// Implement TypeConvertContext so shared type_builder functions can call back
/// into the lowerer for type resolution and constant expression evaluation.
///
/// The 4 divergent methods handle lowering-specific behavior:
/// - typedef: also checks function pointer typedefs for richer type info
/// - struct/union: has caching, forward-declaration handling, enum bitfield fixup
/// - enum: returns CType::Int (enums are plain ints at IR level)
/// - typeof: evaluates the expression's actual type
impl type_builder::TypeConvertContext for Lowerer {
    fn resolve_typedef(&self, name: &str) -> CType {
        // Check function pointer typedefs first (they carry richer type info)
        if let Some(fptr_ctype) = self.build_function_pointer_ctype_from_typedef(name) {
            return fptr_ctype;
        }
        // Direct CType lookup from typedef map
        if let Some(ctype) = self.types.typedefs.get(name) {
            return ctype.clone();
        }
        CType::Int // fallback for unresolved typedef
    }

    fn resolve_struct_or_union(
        &self,
        name: &Option<String>,
        fields: &Option<Vec<StructFieldDecl>>,
        is_union: bool,
        is_packed: bool,
        pragma_pack: Option<usize>,
        struct_aligned: Option<usize>,
    ) -> CType {
        self.struct_or_union_to_ctype(name, fields, is_union, is_packed, pragma_pack, struct_aligned)
    }

    fn resolve_enum(&self, _name: &Option<String>, _variants: &Option<Vec<EnumVariant>>) -> CType {
        CType::Int
    }

    fn resolve_typeof_expr(&self, expr: &Expr) -> CType {
        self.get_expr_ctype(expr).unwrap_or(CType::Int)
    }

    fn eval_const_expr_as_usize(&self, expr: &Expr) -> Option<usize> {
        self.expr_as_array_size(expr).map(|n| n as usize)
    }
}
