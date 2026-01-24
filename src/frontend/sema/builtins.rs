//! Maps GCC __builtin_* function names to their libc/standard equivalents.
//!
//! Many C programs use GCC builtins (e.g., __builtin_abort, __builtin_memcpy).
//! We map these to their standard library equivalents so the linker can resolve them.

use std::collections::HashMap;
use std::sync::LazyLock;

/// Static mapping of __builtin_* names to their libc equivalents.
static BUILTIN_MAP: LazyLock<HashMap<&'static str, BuiltinInfo>> = LazyLock::new(|| {
    let mut m = HashMap::new();

    // Abort/exit
    m.insert("__builtin_abort", BuiltinInfo::simple("abort"));
    m.insert("__builtin_exit", BuiltinInfo::simple("exit"));
    m.insert("__builtin_trap", BuiltinInfo::simple("abort"));
    m.insert("__builtin_unreachable", BuiltinInfo::simple("abort"));

    // Memory functions
    m.insert("__builtin_memcpy", BuiltinInfo::simple("memcpy"));
    m.insert("__builtin_memmove", BuiltinInfo::simple("memmove"));
    m.insert("__builtin_memset", BuiltinInfo::simple("memset"));
    m.insert("__builtin_memcmp", BuiltinInfo::simple("memcmp"));
    m.insert("__builtin_strlen", BuiltinInfo::simple("strlen"));
    m.insert("__builtin_strcpy", BuiltinInfo::simple("strcpy"));
    m.insert("__builtin_strncpy", BuiltinInfo::simple("strncpy"));
    m.insert("__builtin_strcmp", BuiltinInfo::simple("strcmp"));
    m.insert("__builtin_strncmp", BuiltinInfo::simple("strncmp"));
    m.insert("__builtin_strcat", BuiltinInfo::simple("strcat"));
    m.insert("__builtin_strchr", BuiltinInfo::simple("strchr"));
    m.insert("__builtin_strrchr", BuiltinInfo::simple("strrchr"));
    m.insert("__builtin_strstr", BuiltinInfo::simple("strstr"));

    // Math functions
    m.insert("__builtin_abs", BuiltinInfo::simple("abs"));
    m.insert("__builtin_labs", BuiltinInfo::simple("labs"));
    m.insert("__builtin_llabs", BuiltinInfo::simple("llabs"));
    m.insert("__builtin_fabs", BuiltinInfo::simple("fabs"));
    m.insert("__builtin_fabsf", BuiltinInfo::simple("fabsf"));
    m.insert("__builtin_fabsl", BuiltinInfo::simple("fabsl"));
    m.insert("__builtin_sqrt", BuiltinInfo::simple("sqrt"));
    m.insert("__builtin_sqrtf", BuiltinInfo::simple("sqrtf"));
    m.insert("__builtin_sin", BuiltinInfo::simple("sin"));
    m.insert("__builtin_sinf", BuiltinInfo::simple("sinf"));
    m.insert("__builtin_cos", BuiltinInfo::simple("cos"));
    m.insert("__builtin_cosf", BuiltinInfo::simple("cosf"));
    m.insert("__builtin_log", BuiltinInfo::simple("log"));
    m.insert("__builtin_logf", BuiltinInfo::simple("logf"));
    m.insert("__builtin_log2", BuiltinInfo::simple("log2"));
    m.insert("__builtin_exp", BuiltinInfo::simple("exp"));
    m.insert("__builtin_expf", BuiltinInfo::simple("expf"));
    m.insert("__builtin_pow", BuiltinInfo::simple("pow"));
    m.insert("__builtin_powf", BuiltinInfo::simple("powf"));
    m.insert("__builtin_floor", BuiltinInfo::simple("floor"));
    m.insert("__builtin_floorf", BuiltinInfo::simple("floorf"));
    m.insert("__builtin_ceil", BuiltinInfo::simple("ceil"));
    m.insert("__builtin_ceilf", BuiltinInfo::simple("ceilf"));
    m.insert("__builtin_round", BuiltinInfo::simple("round"));
    m.insert("__builtin_roundf", BuiltinInfo::simple("roundf"));
    m.insert("__builtin_fmin", BuiltinInfo::simple("fmin"));
    m.insert("__builtin_fmax", BuiltinInfo::simple("fmax"));
    m.insert("__builtin_copysign", BuiltinInfo::simple("copysign"));
    m.insert("__builtin_copysignf", BuiltinInfo::simple("copysignf"));
    m.insert("__builtin_nan", BuiltinInfo::simple("nan"));
    m.insert("__builtin_nanf", BuiltinInfo::simple("nanf"));
    m.insert("__builtin_inf", BuiltinInfo::constant_f64(f64::INFINITY));
    m.insert("__builtin_inff", BuiltinInfo::constant_f64(f64::INFINITY));
    m.insert("__builtin_infl", BuiltinInfo::constant_f64(f64::INFINITY));
    m.insert("__builtin_huge_val", BuiltinInfo::constant_f64(f64::INFINITY));
    m.insert("__builtin_huge_valf", BuiltinInfo::constant_f64(f64::INFINITY));
    m.insert("__builtin_huge_vall", BuiltinInfo::constant_f64(f64::INFINITY));
    m.insert("__builtin_nanl", BuiltinInfo::simple("nan"));

    // I/O
    m.insert("__builtin_printf", BuiltinInfo::simple("printf"));
    m.insert("__builtin_fprintf", BuiltinInfo::simple("fprintf"));
    m.insert("__builtin_sprintf", BuiltinInfo::simple("sprintf"));
    m.insert("__builtin_snprintf", BuiltinInfo::simple("snprintf"));
    m.insert("__builtin_puts", BuiltinInfo::simple("puts"));
    m.insert("__builtin_putchar", BuiltinInfo::simple("putchar"));

    // Allocation
    m.insert("__builtin_malloc", BuiltinInfo::simple("malloc"));
    m.insert("__builtin_calloc", BuiltinInfo::simple("calloc"));
    m.insert("__builtin_realloc", BuiltinInfo::simple("realloc"));
    m.insert("__builtin_free", BuiltinInfo::simple("free"));

    // Stack allocation - handled specially in try_lower_builtin_call as DynAlloca
    m.insert("__builtin_alloca", BuiltinInfo::intrinsic(BuiltinIntrinsic::Alloca));
    m.insert("__builtin_alloca_with_align", BuiltinInfo::intrinsic(BuiltinIntrinsic::Alloca));

    // Return address / frame address (return 0 as approximation)
    m.insert("__builtin_return_address", BuiltinInfo::constant_i64(0));
    m.insert("__builtin_frame_address", BuiltinInfo::constant_i64(0));
    m.insert("__builtin_extract_return_addr", BuiltinInfo::identity());

    // Compiler hints (these become no-ops or identity)
    m.insert("__builtin_expect", BuiltinInfo::identity()); // returns first arg
    m.insert("__builtin_expect_with_probability", BuiltinInfo::identity());
    m.insert("__builtin_assume_aligned", BuiltinInfo::identity());

    // Type queries (compile-time constants)
    m.insert("__builtin_constant_p", BuiltinInfo::constant_i64(0)); // conservative: always 0
    m.insert("__builtin_types_compatible_p", BuiltinInfo::constant_i64(0));

    // Floating-point comparison builtins
    m.insert("__builtin_isgreater", BuiltinInfo::intrinsic(BuiltinIntrinsic::FpCompare));
    m.insert("__builtin_isgreaterequal", BuiltinInfo::intrinsic(BuiltinIntrinsic::FpCompare));
    m.insert("__builtin_isless", BuiltinInfo::intrinsic(BuiltinIntrinsic::FpCompare));
    m.insert("__builtin_islessequal", BuiltinInfo::intrinsic(BuiltinIntrinsic::FpCompare));
    m.insert("__builtin_islessgreater", BuiltinInfo::intrinsic(BuiltinIntrinsic::FpCompare));
    m.insert("__builtin_isunordered", BuiltinInfo::intrinsic(BuiltinIntrinsic::FpCompare));

    // Floating-point classification builtins
    m.insert("__builtin_fpclassify", BuiltinInfo::intrinsic(BuiltinIntrinsic::FpClassify));
    m.insert("__builtin_isnan", BuiltinInfo::intrinsic(BuiltinIntrinsic::IsNan));
    m.insert("__builtin_isinf", BuiltinInfo::intrinsic(BuiltinIntrinsic::IsInf));
    m.insert("__builtin_isfinite", BuiltinInfo::intrinsic(BuiltinIntrinsic::IsFinite));
    m.insert("__builtin_isnormal", BuiltinInfo::intrinsic(BuiltinIntrinsic::IsNormal));
    m.insert("__builtin_signbit", BuiltinInfo::intrinsic(BuiltinIntrinsic::SignBit));
    m.insert("__builtin_signbitf", BuiltinInfo::intrinsic(BuiltinIntrinsic::SignBit));
    m.insert("__builtin_signbitl", BuiltinInfo::intrinsic(BuiltinIntrinsic::SignBit));
    m.insert("__builtin_isinf_sign", BuiltinInfo::intrinsic(BuiltinIntrinsic::IsInfSign));

    // Bit manipulation
    m.insert("__builtin_clz", BuiltinInfo::intrinsic(BuiltinIntrinsic::Clz));
    m.insert("__builtin_clzl", BuiltinInfo::intrinsic(BuiltinIntrinsic::Clz));
    m.insert("__builtin_clzll", BuiltinInfo::intrinsic(BuiltinIntrinsic::Clz));
    m.insert("__builtin_ctz", BuiltinInfo::intrinsic(BuiltinIntrinsic::Ctz));
    m.insert("__builtin_ctzl", BuiltinInfo::intrinsic(BuiltinIntrinsic::Ctz));
    m.insert("__builtin_ctzll", BuiltinInfo::intrinsic(BuiltinIntrinsic::Ctz));
    m.insert("__builtin_popcount", BuiltinInfo::intrinsic(BuiltinIntrinsic::Popcount));
    m.insert("__builtin_popcountl", BuiltinInfo::intrinsic(BuiltinIntrinsic::Popcount));
    m.insert("__builtin_popcountll", BuiltinInfo::intrinsic(BuiltinIntrinsic::Popcount));
    m.insert("__builtin_bswap16", BuiltinInfo::intrinsic(BuiltinIntrinsic::Bswap));
    m.insert("__builtin_bswap32", BuiltinInfo::intrinsic(BuiltinIntrinsic::Bswap));
    m.insert("__builtin_bswap64", BuiltinInfo::intrinsic(BuiltinIntrinsic::Bswap));
    m.insert("__builtin_ffs", BuiltinInfo::simple("ffs"));
    m.insert("__builtin_ffsl", BuiltinInfo::simple("ffsl"));
    m.insert("__builtin_ffsll", BuiltinInfo::simple("ffsll"));
    m.insert("__builtin_parity", BuiltinInfo::intrinsic(BuiltinIntrinsic::Parity));
    m.insert("__builtin_parityl", BuiltinInfo::intrinsic(BuiltinIntrinsic::Parity));
    m.insert("__builtin_parityll", BuiltinInfo::intrinsic(BuiltinIntrinsic::Parity));

    // Atomics (map to libc atomic helpers for now)
    m.insert("__sync_synchronize", BuiltinInfo::intrinsic(BuiltinIntrinsic::Fence));

    // Complex number functions (C99 <complex.h>)
    m.insert("creal", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexReal));
    m.insert("crealf", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexReal));
    m.insert("creall", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexReal));
    m.insert("cimag", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexImag));
    m.insert("cimagf", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexImag));
    m.insert("cimagl", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexImag));
    m.insert("__builtin_creal", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexReal));
    m.insert("__builtin_crealf", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexReal));
    m.insert("__builtin_creall", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexReal));
    m.insert("__builtin_cimag", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexImag));
    m.insert("__builtin_cimagf", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexImag));
    m.insert("__builtin_cimagl", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexImag));
    m.insert("conj", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexConj));
    m.insert("conjf", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexConj));
    m.insert("conjl", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexConj));

    // Complex construction
    m.insert("__builtin_complex", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexConstruct));

    m
});

/// How a builtin should be handled during lowering.
#[derive(Debug, Clone)]
pub struct BuiltinInfo {
    pub kind: BuiltinKind,
}

/// The kind of builtin behavior.
#[derive(Debug, Clone)]
pub enum BuiltinKind {
    /// Map directly to a libc function call.
    LibcAlias(String),
    /// Return the first argument unchanged (__builtin_expect).
    Identity,
    /// Evaluate to a compile-time integer constant.
    ConstantI64(i64),
    /// Evaluate to a compile-time float constant.
    ConstantF64(f64),
    /// Requires special codegen (CLZ, CTZ, popcount, bswap, etc.).
    Intrinsic(BuiltinIntrinsic),
}

/// Intrinsics that need target-specific codegen.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinIntrinsic {
    Clz,
    Ctz,
    Popcount,
    Bswap,
    Fence,
    FpCompare,
    Parity,
    /// creal/crealf/creall: extract real part of complex number
    ComplexReal,
    /// cimag/cimagf/cimagl: extract imaginary part of complex number
    ComplexImag,
    /// conj/conjf/conjl: compute complex conjugate
    ComplexConj,
    /// __builtin_fpclassify(nan, inf, norm, subnorm, zero, x) -> int
    FpClassify,
    /// __builtin_isnan(x) -> int (1 if NaN, 0 otherwise)
    IsNan,
    /// __builtin_isinf(x) -> int (1 if +/-inf, 0 otherwise)
    IsInf,
    /// __builtin_isfinite(x) -> int (1 if finite, 0 otherwise)
    IsFinite,
    /// __builtin_isnormal(x) -> int (1 if normal, 0 otherwise)
    IsNormal,
    /// __builtin_signbit(x) -> int (nonzero if sign bit set)
    SignBit,
    /// __builtin_isinf_sign(x) -> int (-1 if -inf, 1 if +inf, 0 otherwise)
    IsInfSign,
    /// __builtin_alloca(size) -> dynamic stack allocation
    Alloca,
    /// __builtin_complex(real, imag) -> construct complex number
    ComplexConstruct,
}

impl BuiltinInfo {
    fn simple(libc_name: &str) -> Self {
        Self { kind: BuiltinKind::LibcAlias(libc_name.to_string()) }
    }

    fn identity() -> Self {
        Self { kind: BuiltinKind::Identity }
    }

    fn constant_i64(val: i64) -> Self {
        Self { kind: BuiltinKind::ConstantI64(val) }
    }

    fn constant_f64(val: f64) -> Self {
        Self { kind: BuiltinKind::ConstantF64(val) }
    }

    fn intrinsic(intr: BuiltinIntrinsic) -> Self {
        Self { kind: BuiltinKind::Intrinsic(intr) }
    }
}

/// Look up a function name and return its builtin info, if it's a known builtin.
pub fn resolve_builtin(name: &str) -> Option<&'static BuiltinInfo> {
    BUILTIN_MAP.get(name)
}

/// Returns the libc name for a builtin, or None if it's not a simple alias.
pub fn builtin_to_libc_name(name: &str) -> Option<&str> {
    match resolve_builtin(name) {
        Some(info) => match &info.kind {
            BuiltinKind::LibcAlias(libc_name) => Some(libc_name),
            _ => None,
        },
        None => None,
    }
}

/// Check if a name is a known builtin function.
pub fn is_builtin(name: &str) -> bool {
    BUILTIN_MAP.contains_key(name)
}
