//! Maps GCC __builtin_* function names to their libc/standard equivalents.
//!
//! Many C programs use GCC builtins (e.g., __builtin_abort, __builtin_memcpy).
//! We map these to their standard library equivalents so the linker can resolve them.

use crate::common::fx_hash::FxHashMap;
use std::sync::LazyLock;

/// Static mapping of __builtin_* names to their libc equivalents.
static BUILTIN_MAP: LazyLock<FxHashMap<&'static str, BuiltinInfo>> = LazyLock::new(|| {
    let mut m = FxHashMap::default();

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

    // Return address / frame address
    m.insert("__builtin_return_address", BuiltinInfo::intrinsic(BuiltinIntrinsic::ReturnAddress));
    m.insert("__builtin_frame_address", BuiltinInfo::intrinsic(BuiltinIntrinsic::FrameAddress));
    m.insert("__builtin_extract_return_addr", BuiltinInfo::identity());

    // Compiler hints (these become no-ops or identity)
    m.insert("__builtin_expect", BuiltinInfo::identity()); // returns first arg
    m.insert("__builtin_expect_with_probability", BuiltinInfo::identity());
    m.insert("__builtin_assume_aligned", BuiltinInfo::identity());

    // Type queries (compile-time constants)
    m.insert("__builtin_constant_p", BuiltinInfo::intrinsic(BuiltinIntrinsic::ConstantP));
    m.insert("__builtin_object_size", BuiltinInfo::intrinsic(BuiltinIntrinsic::ObjectSize));
    m.insert("__builtin_classify_type", BuiltinInfo::intrinsic(BuiltinIntrinsic::ClassifyType));
    // Note: __builtin_types_compatible_p is handled as a special AST node (BuiltinTypesCompatibleP),
    // parsed directly in the parser and evaluated at compile-time in the lowerer.

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
    m.insert("__builtin_clrsb", BuiltinInfo::intrinsic(BuiltinIntrinsic::Clrsb));
    m.insert("__builtin_clrsbl", BuiltinInfo::intrinsic(BuiltinIntrinsic::Clrsb));
    m.insert("__builtin_clrsbll", BuiltinInfo::intrinsic(BuiltinIntrinsic::Clrsb));

    // Overflow-checking arithmetic builtins
    // Generic (type-deduced from arguments):
    m.insert("__builtin_add_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::AddOverflow));
    m.insert("__builtin_sub_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::SubOverflow));
    m.insert("__builtin_mul_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::MulOverflow));
    // Signed int variants:
    m.insert("__builtin_sadd_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::AddOverflow));
    m.insert("__builtin_saddl_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::AddOverflow));
    m.insert("__builtin_saddll_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::AddOverflow));
    m.insert("__builtin_ssub_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::SubOverflow));
    m.insert("__builtin_ssubl_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::SubOverflow));
    m.insert("__builtin_ssubll_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::SubOverflow));
    m.insert("__builtin_smul_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::MulOverflow));
    m.insert("__builtin_smull_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::MulOverflow));
    m.insert("__builtin_smulll_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::MulOverflow));
    // Unsigned int variants:
    m.insert("__builtin_uadd_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::AddOverflow));
    m.insert("__builtin_uaddl_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::AddOverflow));
    m.insert("__builtin_uaddll_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::AddOverflow));
    m.insert("__builtin_usub_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::SubOverflow));
    m.insert("__builtin_usubl_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::SubOverflow));
    m.insert("__builtin_usubll_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::SubOverflow));
    m.insert("__builtin_umul_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::MulOverflow));
    m.insert("__builtin_umull_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::MulOverflow));
    m.insert("__builtin_umulll_overflow", BuiltinInfo::intrinsic(BuiltinIntrinsic::MulOverflow));

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
    m.insert("__builtin_conj", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexConj));
    m.insert("__builtin_conjf", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexConj));
    m.insert("__builtin_conjl", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexConj));

    // Complex construction
    m.insert("__builtin_complex", BuiltinInfo::intrinsic(BuiltinIntrinsic::ComplexConstruct));

    // Variadic argument builtins - these are handled specially in IR lowering
    // (expr.rs try_lower_builtin_call), but must be registered here so sema
    // does not emit "implicit declaration" warnings. Those warnings break
    // configure scripts that check stderr for errors (e.g., zlib).
    m.insert("__builtin_va_start", BuiltinInfo::intrinsic(BuiltinIntrinsic::VaStart));
    m.insert("__builtin_va_end", BuiltinInfo::intrinsic(BuiltinIntrinsic::VaEnd));
    m.insert("__builtin_va_copy", BuiltinInfo::intrinsic(BuiltinIntrinsic::VaCopy));

    // Prefetch (no-op, handled separately in lowering)
    m.insert("__builtin_prefetch", BuiltinInfo::intrinsic(BuiltinIntrinsic::Nop));

    // Vector construction builtins used by SSE header wrapper functions.
    // The actual _mm_set1_* calls are intercepted as direct builtins, but the
    // function bodies in emmintrin.h still reference these, so register as Nop
    // to avoid linker errors from the compiled (but never called) wrappers.
    m.insert("__builtin_ia32_vec_init_v16qi", BuiltinInfo::intrinsic(BuiltinIntrinsic::Nop));
    m.insert("__builtin_ia32_vec_init_v4si", BuiltinInfo::intrinsic(BuiltinIntrinsic::Nop));

    // x86 SSE/SSE2/SSE4.2 intrinsic builtins (__builtin_ia32_* names)
    m.insert("__builtin_ia32_lfence", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Lfence));
    m.insert("__builtin_ia32_mfence", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Mfence));
    m.insert("__builtin_ia32_sfence", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Sfence));
    m.insert("__builtin_ia32_clflush", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Clflush));
    m.insert("__builtin_ia32_pause", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Pause));
    m.insert("__builtin_ia32_movnti", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Movnti));
    m.insert("__builtin_ia32_movnti64", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Movnti64));
    m.insert("__builtin_ia32_movntdq", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Movntdq));
    m.insert("__builtin_ia32_movntpd", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Movntpd));
    m.insert("__builtin_ia32_loaddqu", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Loaddqu));
    m.insert("__builtin_ia32_storedqu", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Storedqu));
    m.insert("__builtin_ia32_pcmpeqb128", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Pcmpeqb128));
    m.insert("__builtin_ia32_pcmpeqd128", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Pcmpeqd128));
    m.insert("__builtin_ia32_psubusb128", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Psubusb128));
    m.insert("__builtin_ia32_por128", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Por128));
    m.insert("__builtin_ia32_pand128", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Pand128));
    m.insert("__builtin_ia32_pxor128", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Pxor128));
    m.insert("__builtin_ia32_pmovmskb128", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Pmovmskb128));
    m.insert("__builtin_ia32_set1_epi8", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Set1Epi8));
    m.insert("__builtin_ia32_set1_epi32", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Set1Epi32));
    m.insert("__builtin_ia32_crc32qi", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Crc32_8));
    m.insert("__builtin_ia32_crc32hi", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Crc32_16));
    m.insert("__builtin_ia32_crc32si", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Crc32_32));
    m.insert("__builtin_ia32_crc32di", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Crc32_64));

    // Direct _mm_* function name mappings (bypass wrapper functions, avoid ABI issues)
    m.insert("_mm_loadu_si128", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Loaddqu));
    m.insert("_mm_load_si128", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Loaddqu));
    m.insert("_mm_storeu_si128", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Storedqu));
    m.insert("_mm_store_si128", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Storedqu));
    m.insert("_mm_set1_epi8", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Set1Epi8));
    m.insert("_mm_set1_epi32", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Set1Epi32));
    m.insert("_mm_setzero_si128", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Set1Epi8));
    m.insert("_mm_cmpeq_epi8", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Pcmpeqb128));
    m.insert("_mm_cmpeq_epi32", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Pcmpeqd128));
    m.insert("_mm_subs_epu8", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Psubusb128));
    m.insert("_mm_or_si128", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Por128));
    m.insert("_mm_and_si128", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Pand128));
    m.insert("_mm_xor_si128", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Pxor128));
    m.insert("_mm_movemask_epi8", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Pmovmskb128));
    m.insert("_mm_stream_si128", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Movntdq));
    m.insert("_mm_stream_si64", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Movnti64));
    m.insert("_mm_stream_si32", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Movnti));
    m.insert("_mm_stream_pd", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Movntpd));
    m.insert("_mm_lfence", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Lfence));
    m.insert("_mm_mfence", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Mfence));
    m.insert("_mm_sfence", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Sfence));
    m.insert("_mm_clflush", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Clflush));
    m.insert("_mm_pause", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Pause));
    m.insert("_mm_crc32_u8", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Crc32_8));
    m.insert("_mm_crc32_u16", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Crc32_16));
    m.insert("_mm_crc32_u32", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Crc32_32));
    m.insert("_mm_crc32_u64", BuiltinInfo::intrinsic(BuiltinIntrinsic::X86Crc32_64));

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
    Clrsb,
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
    /// __builtin_va_start(ap, last) -> initialize va_list (lowered specially in IR)
    VaStart,
    /// __builtin_va_end(ap) -> cleanup va_list (lowered specially in IR)
    VaEnd,
    /// __builtin_va_copy(dest, src) -> copy va_list (lowered specially in IR)
    VaCopy,
    /// __builtin_constant_p(expr) -> 1 if expr is a compile-time constant, 0 otherwise
    ConstantP,
    /// __builtin_object_size(ptr, type) -> size of object ptr points to, or -1/0 if unknown
    ObjectSize,
    /// __builtin_classify_type(expr) -> integer type class of the expression's type
    ClassifyType,
    /// No-op builtin (evaluates args, returns 0)
    Nop,
    /// __builtin_add_overflow(a, b, result_ptr) -> bool (1 if overflow)
    AddOverflow,
    /// __builtin_sub_overflow(a, b, result_ptr) -> bool (1 if overflow)
    SubOverflow,
    /// __builtin_mul_overflow(a, b, result_ptr) -> bool (1 if overflow)
    MulOverflow,
    /// __builtin_frame_address(level) -> returns frame pointer
    FrameAddress,
    /// __builtin_return_address(level) -> returns return address
    ReturnAddress,
    // X86 SSE intrinsics
    X86Lfence,
    X86Mfence,
    X86Sfence,
    X86Pause,
    X86Clflush,
    X86Movnti,
    X86Movnti64,
    X86Movntdq,
    X86Movntpd,
    X86Loaddqu,
    X86Storedqu,
    X86Pcmpeqb128,
    X86Pcmpeqd128,
    X86Psubusb128,
    X86Por128,
    X86Pand128,
    X86Pxor128,
    X86Pmovmskb128,
    X86Set1Epi8,
    X86Set1Epi32,
    X86Crc32_8,
    X86Crc32_16,
    X86Crc32_32,
    X86Crc32_64,
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
///
/// This includes both explicitly registered builtins (in BUILTIN_MAP) and
/// atomic/sync builtins that are handled by pattern matching in the IR lowering
/// code (expr_atomics.rs). The atomic builtins must be recognized here so that
/// sema does not emit "implicit declaration" warnings for them.
pub fn is_builtin(name: &str) -> bool {
    if BUILTIN_MAP.contains_key(name) {
        return true;
    }
    // Builtins handled by name in try_lower_builtin_call (before map lookup)
    if name == "__builtin_choose_expr" {
        return true;
    }
    // Atomic builtins handled by pattern matching in expr_atomics.rs
    is_atomic_builtin(name)
}

/// Check if a name is an atomic/sync builtin handled by the IR lowering code.
/// These are dispatched by name pattern in try_lower_atomic_builtin() and
/// classify_fetch_op()/classify_op_fetch() rather than through the BUILTIN_MAP.
fn is_atomic_builtin(name: &str) -> bool {
    // __atomic_* family (C11-style)
    if name.starts_with("__atomic_") {
        return matches!(name,
            "__atomic_fetch_add" | "__atomic_fetch_sub" | "__atomic_fetch_and" |
            "__atomic_fetch_or" | "__atomic_fetch_xor" | "__atomic_fetch_nand" |
            "__atomic_add_fetch" | "__atomic_sub_fetch" | "__atomic_and_fetch" |
            "__atomic_or_fetch" | "__atomic_xor_fetch" | "__atomic_nand_fetch" |
            "__atomic_exchange_n" | "__atomic_exchange" |
            "__atomic_compare_exchange_n" | "__atomic_compare_exchange" |
            "__atomic_load_n" | "__atomic_load" |
            "__atomic_store_n" | "__atomic_store" |
            "__atomic_test_and_set" | "__atomic_clear" |
            "__atomic_thread_fence" | "__atomic_signal_fence" |
            "__atomic_is_lock_free" | "__atomic_always_lock_free"
        );
    }
    // __sync_* family (legacy GCC-style)
    if name.starts_with("__sync_") {
        return matches!(name,
            "__sync_fetch_and_add" | "__sync_fetch_and_sub" | "__sync_fetch_and_and" |
            "__sync_fetch_and_or" | "__sync_fetch_and_xor" | "__sync_fetch_and_nand" |
            "__sync_add_and_fetch" | "__sync_sub_and_fetch" | "__sync_and_and_fetch" |
            "__sync_or_and_fetch" | "__sync_xor_and_fetch" | "__sync_nand_and_fetch" |
            "__sync_val_compare_and_swap" | "__sync_bool_compare_and_swap" |
            "__sync_lock_test_and_set" | "__sync_lock_release" |
            "__sync_synchronize"
        );
    }
    false
}
