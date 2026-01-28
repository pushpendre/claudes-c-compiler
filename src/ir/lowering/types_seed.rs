//! Builtin type and function seeding for the lowerer.
//!
//! Pre-populates typedef mappings for standard C types (stddef.h, stdint.h,
//! sys/types.h, etc.) and registers known libc math function signatures
//! for correct calling convention.

use crate::common::types::{AddressSpace, IrType, CType};
use super::lowering::Lowerer;
use super::definitions::FuncSig;

impl Lowerer {
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
            ("timer_t", CType::Pointer(Box::new(CType::Void), AddressSpace::Default)),
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
            ("locale_t", CType::Pointer(Box::new(CType::Void), AddressSpace::Default)),
            // <pthread.h> - opaque types, treat as unsigned long or pointer
            ("pthread_t", CType::ULong),
            ("pthread_mutex_t", CType::Pointer(Box::new(CType::Void), AddressSpace::Default)),
            ("pthread_cond_t", CType::Pointer(Box::new(CType::Void), AddressSpace::Default)),
            ("pthread_key_t", CType::UInt),
            ("pthread_attr_t", CType::Pointer(Box::new(CType::Void), AddressSpace::Default)),
            ("pthread_once_t", CType::Int),
            ("pthread_mutexattr_t", CType::Pointer(Box::new(CType::Void), AddressSpace::Default)),
            ("pthread_condattr_t", CType::Pointer(Box::new(CType::Void), AddressSpace::Default)),
            // <setjmp.h>
            ("jmp_buf", CType::Pointer(Box::new(CType::Void), AddressSpace::Default)),
            ("sigjmp_buf", CType::Pointer(Box::new(CType::Void), AddressSpace::Default)),
            // <stdio.h>
            ("FILE", CType::Pointer(Box::new(CType::Void), AddressSpace::Default)),
            ("fpos_t", CType::Long),
            // <dirent.h>
            ("DIR", CType::Pointer(Box::new(CType::Void), AddressSpace::Default)),
        ];
        for (name, ct) in builtins {
            self.types.typedefs.insert(name.to_string(), ct.clone());
        }
        // Target-dependent va_list definition.
        use crate::backend::Target;
        let va_list_type = match self.target {
            Target::Riscv64 => {
                // RISC-V: va_list = void * (8 bytes, passed by value)
                CType::Pointer(Box::new(CType::Void), AddressSpace::Default)
            }
            Target::Aarch64 => {
                // AArch64: va_list is a 32-byte struct, represented as char[32]
                CType::Array(Box::new(CType::Char), Some(32))
            }
            Target::X86_64 => {
                // x86-64: va_list is __va_list_tag[1], 24 bytes, represented as char[24]
                CType::Array(Box::new(CType::Char), Some(24))
            }
            Target::I686 => {
                // i686 cdecl: va_list = char* (simple pointer to stack args)
                CType::Pointer(Box::new(CType::Char), AddressSpace::Default)
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
        let mut sig = FuncSig::for_ptr(return_type, param_types);
        sig.param_ctypes = param_ctypes;
        self.func_meta.sigs.insert(name.to_string(), sig);
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
}
