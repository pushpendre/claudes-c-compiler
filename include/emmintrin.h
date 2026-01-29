/* CCC compiler bundled emmintrin.h - SSE2 intrinsics */
#ifndef _EMMINTRIN_H_INCLUDED
#define _EMMINTRIN_H_INCLUDED

#include <xmmintrin.h>

typedef struct __attribute__((__aligned__(16))) {
    long long __val[2];
} __m128i;

typedef struct __attribute__((__aligned__(1))) {
    long long __val[2];
} __m128i_u;

typedef struct __attribute__((__aligned__(16))) {
    double __val[2];
} __m128d;

typedef struct __attribute__((__aligned__(1))) {
    double __val[2];
} __m128d_u;

/* Internal vector types referenced by GCC system headers (wmmintrin.h, etc.).
 * These enable parsing of system headers that use (__v2di)expr casts.
 * Note: vector_size attribute is parsed but vectors are lowered as aggregates. */
typedef double __v2df __attribute__ ((__vector_size__ (16)));
typedef long long __v2di __attribute__ ((__vector_size__ (16)));
typedef unsigned long long __v2du __attribute__ ((__vector_size__ (16)));
typedef int __v4si __attribute__ ((__vector_size__ (16)));
typedef unsigned int __v4su __attribute__ ((__vector_size__ (16)));
typedef short __v8hi __attribute__ ((__vector_size__ (16)));
typedef unsigned short __v8hu __attribute__ ((__vector_size__ (16)));
typedef char __v16qi __attribute__ ((__vector_size__ (16)));
typedef signed char __v16qs __attribute__ ((__vector_size__ (16)));
typedef unsigned char __v16qu __attribute__ ((__vector_size__ (16)));

/* Helper to convert intrinsic result pointer to __m128i value.
 * Our SSE builtins return a pointer to 16-byte result data.
 * This macro dereferences that pointer to get the __m128i struct value. */
#define __CCC_M128I_FROM_BUILTIN(expr) (*(__m128i *)(expr))

/* === Load / Store === */

static __inline__ __m128i __attribute__((__always_inline__))
_mm_loadu_si128(__m128i_u const *__p)
{
    return *__p;
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_load_si128(__m128i const *__p)
{
    return *__p;
}

static __inline__ void __attribute__((__always_inline__))
_mm_storeu_si128(__m128i_u *__p, __m128i __b)
{
    *__p = __b;
}

static __inline__ void __attribute__((__always_inline__))
_mm_store_si128(__m128i *__p, __m128i __b)
{
    *__p = __b;
}

/* === Set === */

static __inline__ __m128i __attribute__((__always_inline__))
_mm_set1_epi8(char __b)
{
    return __CCC_M128I_FROM_BUILTIN(
        __builtin_ia32_vec_init_v16qi(__b, __b, __b, __b,
                                      __b, __b, __b, __b,
                                      __b, __b, __b, __b,
                                      __b, __b, __b, __b));
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_set1_epi32(int __i)
{
    return __CCC_M128I_FROM_BUILTIN(
        __builtin_ia32_vec_init_v4si(__i, __i, __i, __i));
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_setzero_si128(void)
{
    return (__m128i){ { 0LL, 0LL } };
}

/* === Compare === */

static __inline__ __m128i __attribute__((__always_inline__))
_mm_cmpeq_epi8(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_pcmpeqb128(__a, __b));
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_cmpeq_epi32(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_pcmpeqd128(__a, __b));
}

/* === Arithmetic === */

static __inline__ __m128i __attribute__((__always_inline__))
_mm_subs_epu8(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_psubusb128(__a, __b));
}

/* === Bitwise === */

static __inline__ __m128i __attribute__((__always_inline__))
_mm_or_si128(__m128i __a, __m128i __b)
{
    return (__m128i){ { __a.__val[0] | __b.__val[0],
                        __a.__val[1] | __b.__val[1] } };
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_and_si128(__m128i __a, __m128i __b)
{
    return (__m128i){ { __a.__val[0] & __b.__val[0],
                        __a.__val[1] & __b.__val[1] } };
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_xor_si128(__m128i __a, __m128i __b)
{
    return (__m128i){ { __a.__val[0] ^ __b.__val[0],
                        __a.__val[1] ^ __b.__val[1] } };
}

/* === 16-bit Arithmetic === */

static __inline__ __m128i __attribute__((__always_inline__))
_mm_add_epi16(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_paddw128(__a, __b));
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_sub_epi16(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_psubw128(__a, __b));
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_mulhi_epi16(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_pmulhw128(__a, __b));
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_madd_epi16(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_pmaddwd128(__a, __b));
}

/* === 32-bit Arithmetic === */

static __inline__ __m128i __attribute__((__always_inline__))
_mm_add_epi32(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_paddd128(__a, __b));
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_sub_epi32(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_psubd128(__a, __b));
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_cmpgt_epi16(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_pcmpgtw128(__a, __b));
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_cmpgt_epi8(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_pcmpgtb128(__a, __b));
}

/* === Pack / Unpack === */

static __inline__ __m128i __attribute__((__always_inline__))
_mm_packs_epi32(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_packssdw128(__a, __b));
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_packus_epi16(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_packuswb128(__a, __b));
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_unpacklo_epi8(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_punpcklbw128(__a, __b));
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_unpackhi_epi8(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_punpckhbw128(__a, __b));
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_unpacklo_epi16(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_punpcklwd128(__a, __b));
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_unpackhi_epi16(__m128i __a, __m128i __b)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_punpckhwd128(__a, __b));
}

/* === Set / Broadcast === */

static __inline__ __m128i __attribute__((__always_inline__))
_mm_set1_epi16(short __w)
{
    return __CCC_M128I_FROM_BUILTIN(
        __builtin_ia32_vec_init_v8hi(__w, __w, __w, __w,
                                     __w, __w, __w, __w));
}

// TODO: _mm_setr_epi16 and _mm_setr_epi32 rely on vec_init builtins that are
// currently stubbed as Nop. They will produce zeroed vectors instead of the
// correct result. Implement proper vec_init codegen to fix these.
static __inline__ __m128i __attribute__((__always_inline__))
_mm_setr_epi16(short __w0, short __w1, short __w2, short __w3,
               short __w4, short __w5, short __w6, short __w7)
{
    return __CCC_M128I_FROM_BUILTIN(
        __builtin_ia32_vec_init_v8hi(__w0, __w1, __w2, __w3,
                                     __w4, __w5, __w6, __w7));
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_setr_epi32(int __i0, int __i1, int __i2, int __i3)
{
    return __CCC_M128I_FROM_BUILTIN(
        __builtin_ia32_vec_init_v4si(__i0, __i1, __i2, __i3));
}

/* === Insert / Extract === */

#define _mm_insert_epi16(a, i, imm) \
    __CCC_M128I_FROM_BUILTIN(__builtin_ia32_pinsrw128((a), (i), (imm)))

#define _mm_extract_epi16(a, imm) \
    __builtin_ia32_pextrw128((a), (imm))

/* === Convert / Move === */

static __inline__ int __attribute__((__always_inline__))
_mm_cvtsi128_si32(__m128i __a)
{
    return __builtin_ia32_cvtsi128si32(__a);
}

static __inline__ __m128i __attribute__((__always_inline__))
_mm_cvtsi32_si128(int __a)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_cvtsi32si128(__a));
}

static __inline__ long long __attribute__((__always_inline__))
_mm_cvtsi128_si64(__m128i __a)
{
    return __builtin_ia32_cvtsi128si64(__a);
}

#define _mm_cvtsi128_si64x(a) _mm_cvtsi128_si64(a)

/* === Store low 64 bits === */

static __inline__ void __attribute__((__always_inline__))
_mm_storel_epi64(__m128i *__p, __m128i __a)
{
    __builtin_ia32_storeldi128(__p, __a);
}

/* === Shuffle 16-bit === */

#define _mm_shufflelo_epi16(a, imm) \
    __CCC_M128I_FROM_BUILTIN(__builtin_ia32_pshuflw128((a), (imm)))

#define _mm_shufflehi_epi16(a, imm) \
    __CCC_M128I_FROM_BUILTIN(__builtin_ia32_pshufhw128((a), (imm)))

/* === Shift operations === */

/* Bit-level shift left on each 16-bit element (PSLLW) */
#define _mm_slli_epi16(a, count) \
    __CCC_M128I_FROM_BUILTIN(__builtin_ia32_psllwi128((a), (count)))

/* Bit-level shift right logical on each 16-bit element (PSRLW) */
#define _mm_srli_epi16(a, count) \
    __CCC_M128I_FROM_BUILTIN(__builtin_ia32_psrlwi128((a), (count)))

/* Bit-level shift right arithmetic on each 16-bit element (PSRAW) */
#define _mm_srai_epi16(a, count) \
    __CCC_M128I_FROM_BUILTIN(__builtin_ia32_psrawi128((a), (count)))

/* Bit-level shift right arithmetic on each 32-bit element (PSRAD) */
#define _mm_srai_epi32(a, count) \
    __CCC_M128I_FROM_BUILTIN(__builtin_ia32_psradi128((a), (count)))

/* Bit-level shift left on each 32-bit element (PSLLD) */
#define _mm_slli_epi32(a, count) \
    __CCC_M128I_FROM_BUILTIN(__builtin_ia32_pslldi128((a), (count)))

/* Bit-level shift right logical on each 32-bit element (PSRLD) */
#define _mm_srli_epi32(a, count) \
    __CCC_M128I_FROM_BUILTIN(__builtin_ia32_psrldi128((a), (count)))

/* Byte-level shift left (PSLLDQ): shift __a left by __N bytes, zero-fill */
#define _mm_slli_si128(a, N) \
    __CCC_M128I_FROM_BUILTIN(__builtin_ia32_pslldqi128((a), (N)))

/* Byte-level shift right (PSRLDQ): shift __a right by __N bytes, zero-fill */
#define _mm_srli_si128(a, N) \
    __CCC_M128I_FROM_BUILTIN(__builtin_ia32_psrldqi128((a), (N)))

/* Bit-level shift left on each 64-bit element (PSLLQ) */
#define _mm_slli_epi64(a, count) \
    __CCC_M128I_FROM_BUILTIN(__builtin_ia32_psllqi128((a), (count)))

/* Bit-level shift right on each 64-bit element (PSRLQ) */
#define _mm_srli_epi64(a, count) \
    __CCC_M128I_FROM_BUILTIN(__builtin_ia32_psrlqi128((a), (count)))

/* Shuffle 32-bit integers (PSHUFD) */
#define _mm_shuffle_epi32(a, imm) \
    __CCC_M128I_FROM_BUILTIN(__builtin_ia32_pshufd128((a), (imm)))

/* Load low 64 bits into lower half, zero upper half (MOVQ) */
static __inline__ __m128i __attribute__((__always_inline__))
_mm_loadl_epi64(__m128i const *__p)
{
    return __CCC_M128I_FROM_BUILTIN(__builtin_ia32_loadldi128(__p));
}

/* === Miscellaneous === */

static __inline__ int __attribute__((__always_inline__))
_mm_movemask_epi8(__m128i __a)
{
    return __builtin_ia32_pmovmskb128(__a);
}

/* === Streaming / Non-temporal stores === */

static __inline__ void __attribute__((__always_inline__))
_mm_stream_si128(__m128i *__p, __m128i __a)
{
    __builtin_ia32_movntdq(__p, __a);
}

static __inline__ void __attribute__((__always_inline__))
_mm_stream_si64(long long *__p, long long __a)
{
    __builtin_ia32_movnti64(__p, __a);
}

static __inline__ void __attribute__((__always_inline__))
_mm_stream_si32(int *__p, int __a)
{
    __builtin_ia32_movnti(__p, __a);
}

static __inline__ void __attribute__((__always_inline__))
_mm_stream_pd(double *__p, __m128d __a)
{
    __builtin_ia32_movntpd(__p, __a);
}

/* === Fence / Cache === */

static __inline__ void __attribute__((__always_inline__))
_mm_lfence(void)
{
    __builtin_ia32_lfence();
}

static __inline__ void __attribute__((__always_inline__))
_mm_mfence(void)
{
    __builtin_ia32_mfence();
}

static __inline__ void __attribute__((__always_inline__))
_mm_clflush(void const *__p)
{
    __builtin_ia32_clflush(__p);
}

#endif /* _EMMINTRIN_H_INCLUDED */
