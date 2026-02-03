/* CCC compiler bundled avx2intrin.h - AVX2 integer intrinsics */
#ifndef _AVX2INTRIN_H_INCLUDED
#define _AVX2INTRIN_H_INCLUDED

#include <avxintrin.h>

/* === Set === */

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_set1_epi8(char __b)
{
    unsigned char __ub = (unsigned char)__b;
    long long __q = (long long)__ub;
    __q |= __q << 8;
    __q |= __q << 16;
    __q |= __q << 32;
    return (__m256i){ { __q, __q, __q, __q } };
}

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_set1_epi16(short __w)
{
    unsigned short __uw = (unsigned short)__w;
    long long __q = (long long)__uw;
    __q |= __q << 16;
    __q |= __q << 32;
    return (__m256i){ { __q, __q, __q, __q } };
}

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_set1_epi32(int __i)
{
    long long __q = (long long)(unsigned int)__i
                  | ((long long)(unsigned int)__i << 32);
    return (__m256i){ { __q, __q, __q, __q } };
}

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_set1_epi64x(long long __q)
{
    return (__m256i){ { __q, __q, __q, __q } };
}

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_setr_epi8(
    char __b0,  char __b1,  char __b2,  char __b3,
    char __b4,  char __b5,  char __b6,  char __b7,
    char __b8,  char __b9,  char __b10, char __b11,
    char __b12, char __b13, char __b14, char __b15,
    char __b16, char __b17, char __b18, char __b19,
    char __b20, char __b21, char __b22, char __b23,
    char __b24, char __b25, char __b26, char __b27,
    char __b28, char __b29, char __b30, char __b31)
{
    __m256i __r;
    unsigned char *__p = (unsigned char *)&__r;
    __p[0]  = (unsigned char)__b0;  __p[1]  = (unsigned char)__b1;
    __p[2]  = (unsigned char)__b2;  __p[3]  = (unsigned char)__b3;
    __p[4]  = (unsigned char)__b4;  __p[5]  = (unsigned char)__b5;
    __p[6]  = (unsigned char)__b6;  __p[7]  = (unsigned char)__b7;
    __p[8]  = (unsigned char)__b8;  __p[9]  = (unsigned char)__b9;
    __p[10] = (unsigned char)__b10; __p[11] = (unsigned char)__b11;
    __p[12] = (unsigned char)__b12; __p[13] = (unsigned char)__b13;
    __p[14] = (unsigned char)__b14; __p[15] = (unsigned char)__b15;
    __p[16] = (unsigned char)__b16; __p[17] = (unsigned char)__b17;
    __p[18] = (unsigned char)__b18; __p[19] = (unsigned char)__b19;
    __p[20] = (unsigned char)__b20; __p[21] = (unsigned char)__b21;
    __p[22] = (unsigned char)__b22; __p[23] = (unsigned char)__b23;
    __p[24] = (unsigned char)__b24; __p[25] = (unsigned char)__b25;
    __p[26] = (unsigned char)__b26; __p[27] = (unsigned char)__b27;
    __p[28] = (unsigned char)__b28; __p[29] = (unsigned char)__b29;
    __p[30] = (unsigned char)__b30; __p[31] = (unsigned char)__b31;
    return __r;
}

/* === Bitwise === */

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_and_si256(__m256i __a, __m256i __b)
{
    return (__m256i){ { __a.__val[0] & __b.__val[0],
                        __a.__val[1] & __b.__val[1],
                        __a.__val[2] & __b.__val[2],
                        __a.__val[3] & __b.__val[3] } };
}

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_andnot_si256(__m256i __a, __m256i __b)
{
    return (__m256i){ { ~__a.__val[0] & __b.__val[0],
                        ~__a.__val[1] & __b.__val[1],
                        ~__a.__val[2] & __b.__val[2],
                        ~__a.__val[3] & __b.__val[3] } };
}

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_or_si256(__m256i __a, __m256i __b)
{
    return (__m256i){ { __a.__val[0] | __b.__val[0],
                        __a.__val[1] | __b.__val[1],
                        __a.__val[2] | __b.__val[2],
                        __a.__val[3] | __b.__val[3] } };
}

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_xor_si256(__m256i __a, __m256i __b)
{
    return (__m256i){ { __a.__val[0] ^ __b.__val[0],
                        __a.__val[1] ^ __b.__val[1],
                        __a.__val[2] ^ __b.__val[2],
                        __a.__val[3] ^ __b.__val[3] } };
}

/* === Shift === */

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_slli_epi32(__m256i __a, int __count)
{
    if (__count < 0 || __count > 31)
        return _mm256_setzero_si256();
    unsigned int *__pa = (unsigned int *)&__a;
    __m256i __r;
    unsigned int *__pr = (unsigned int *)&__r;
    for (int __i = 0; __i < 8; __i++)
        __pr[__i] = __pa[__i] << __count;
    return __r;
}

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_srli_epi32(__m256i __a, int __count)
{
    if (__count < 0 || __count > 31)
        return _mm256_setzero_si256();
    unsigned int *__pa = (unsigned int *)&__a;
    __m256i __r;
    unsigned int *__pr = (unsigned int *)&__r;
    for (int __i = 0; __i < 8; __i++)
        __pr[__i] = __pa[__i] >> __count;
    return __r;
}

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_slli_epi64(__m256i __a, int __count)
{
    if (__count < 0 || __count > 63)
        return _mm256_setzero_si256();
    unsigned long long *__pa = (unsigned long long *)&__a;
    __m256i __r;
    unsigned long long *__pr = (unsigned long long *)&__r;
    for (int __i = 0; __i < 4; __i++)
        __pr[__i] = __pa[__i] << __count;
    return __r;
}

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_srli_epi64(__m256i __a, int __count)
{
    if (__count < 0 || __count > 63)
        return _mm256_setzero_si256();
    unsigned long long *__pa = (unsigned long long *)&__a;
    __m256i __r;
    unsigned long long *__pr = (unsigned long long *)&__r;
    for (int __i = 0; __i < 4; __i++)
        __pr[__i] = __pa[__i] >> __count;
    return __r;
}

/* === Compare / Min / Max === */

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_max_epu8(__m256i __a, __m256i __b)
{
    unsigned char *__pa = (unsigned char *)&__a;
    unsigned char *__pb = (unsigned char *)&__b;
    __m256i __r;
    unsigned char *__pr = (unsigned char *)&__r;
    for (int __i = 0; __i < 32; __i++)
        __pr[__i] = __pa[__i] > __pb[__i] ? __pa[__i] : __pb[__i];
    return __r;
}

/* === Shuffle === */

/* _mm256_shuffle_epi8: VPSHUFB - byte shuffle within 128-bit lanes */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_shuffle_epi8(__m256i __a, __m256i __b)
{
    unsigned char *__pa = (unsigned char *)&__a;
    unsigned char *__pb = (unsigned char *)&__b;
    __m256i __r;
    unsigned char *__pr = (unsigned char *)&__r;
    /* Low 128-bit lane */
    for (int __i = 0; __i < 16; __i++) {
        if (__pb[__i] & 0x80)
            __pr[__i] = 0;
        else
            __pr[__i] = __pa[__pb[__i] & 0x0F];
    }
    /* High 128-bit lane */
    for (int __i = 16; __i < 32; __i++) {
        if (__pb[__i] & 0x80)
            __pr[__i] = 0;
        else
            __pr[__i] = __pa[16 + (__pb[__i] & 0x0F)];
    }
    return __r;
}

/* === Extract === */

/* _mm256_extracti128_si256: extract 128-bit lane (imm must be 0 or 1) */
static __inline__ __m128i __attribute__((__always_inline__))
_mm256_extracti128_si256(__m256i __a, int __imm)
{
    if (__imm & 1)
        return (__m128i){ { __a.__val[2], __a.__val[3] } };
    else
        return (__m128i){ { __a.__val[0], __a.__val[1] } };
}

/* === Add / Sub === */

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_add_epi8(__m256i __a, __m256i __b)
{
    unsigned char *__pa = (unsigned char *)&__a;
    unsigned char *__pb = (unsigned char *)&__b;
    __m256i __r;
    unsigned char *__pr = (unsigned char *)&__r;
    for (int __i = 0; __i < 32; __i++)
        __pr[__i] = __pa[__i] + __pb[__i];
    return __r;
}

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_add_epi32(__m256i __a, __m256i __b)
{
    unsigned int *__pa = (unsigned int *)&__a;
    unsigned int *__pb = (unsigned int *)&__b;
    __m256i __r;
    unsigned int *__pr = (unsigned int *)&__r;
    for (int __i = 0; __i < 8; __i++)
        __pr[__i] = __pa[__i] + __pb[__i];
    return __r;
}

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_add_epi64(__m256i __a, __m256i __b)
{
    return (__m256i){ { __a.__val[0] + __b.__val[0],
                        __a.__val[1] + __b.__val[1],
                        __a.__val[2] + __b.__val[2],
                        __a.__val[3] + __b.__val[3] } };
}

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_sub_epi32(__m256i __a, __m256i __b)
{
    unsigned int *__pa = (unsigned int *)&__a;
    unsigned int *__pb = (unsigned int *)&__b;
    __m256i __r;
    unsigned int *__pr = (unsigned int *)&__r;
    for (int __i = 0; __i < 8; __i++)
        __pr[__i] = __pa[__i] - __pb[__i];
    return __r;
}

/* === Compare === */

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_cmpeq_epi8(__m256i __a, __m256i __b)
{
    unsigned char *__pa = (unsigned char *)&__a;
    unsigned char *__pb = (unsigned char *)&__b;
    __m256i __r;
    unsigned char *__pr = (unsigned char *)&__r;
    for (int __i = 0; __i < 32; __i++)
        __pr[__i] = (__pa[__i] == __pb[__i]) ? 0xFF : 0x00;
    return __r;
}

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_cmpeq_epi32(__m256i __a, __m256i __b)
{
    unsigned int *__pa = (unsigned int *)&__a;
    unsigned int *__pb = (unsigned int *)&__b;
    __m256i __r;
    unsigned int *__pr = (unsigned int *)&__r;
    for (int __i = 0; __i < 8; __i++)
        __pr[__i] = (__pa[__i] == __pb[__i]) ? 0xFFFFFFFFu : 0;
    return __r;
}

/* === Permute === */

static __inline__ __m256i __attribute__((__always_inline__))
_mm256_permute2x128_si256(__m256i __a, __m256i __b, int __imm)
{
    int __sel_lo = __imm & 0x3;
    int __sel_hi = (__imm >> 4) & 0x3;
    long long __result[4];

    /* Select low 128 bits */
    switch (__sel_lo) {
        case 0: __result[0] = __a.__val[0]; __result[1] = __a.__val[1]; break;
        case 1: __result[0] = __a.__val[2]; __result[1] = __a.__val[3]; break;
        case 2: __result[0] = __b.__val[0]; __result[1] = __b.__val[1]; break;
        case 3: __result[0] = __b.__val[2]; __result[1] = __b.__val[3]; break;
    }
    if (__imm & 0x08) { __result[0] = 0; __result[1] = 0; }

    /* Select high 128 bits */
    switch (__sel_hi) {
        case 0: __result[2] = __a.__val[0]; __result[3] = __a.__val[1]; break;
        case 1: __result[2] = __a.__val[2]; __result[3] = __a.__val[3]; break;
        case 2: __result[2] = __b.__val[0]; __result[3] = __b.__val[1]; break;
        case 3: __result[2] = __b.__val[2]; __result[3] = __b.__val[3]; break;
    }
    if (__imm & 0x80) { __result[2] = 0; __result[3] = 0; }

    return (__m256i){ { __result[0], __result[1], __result[2], __result[3] } };
}

/* === Set (non-broadcast) === */

/* _mm256_set_epi32: set 8 ints (high-to-low order like Intel) */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_set_epi32(int __e7, int __e6, int __e5, int __e4,
                 int __e3, int __e2, int __e1, int __e0)
{
    __m256i __r;
    int *__p = (int *)&__r;
    __p[0] = __e0; __p[1] = __e1; __p[2] = __e2; __p[3] = __e3;
    __p[4] = __e4; __p[5] = __e5; __p[6] = __e6; __p[7] = __e7;
    return __r;
}

/* _mm256_set_epi8: set 32 bytes (high-to-low order like Intel) */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_set_epi8(
    char __b31, char __b30, char __b29, char __b28,
    char __b27, char __b26, char __b25, char __b24,
    char __b23, char __b22, char __b21, char __b20,
    char __b19, char __b18, char __b17, char __b16,
    char __b15, char __b14, char __b13, char __b12,
    char __b11, char __b10, char __b9,  char __b8,
    char __b7,  char __b6,  char __b5,  char __b4,
    char __b3,  char __b2,  char __b1,  char __b0)
{
    __m256i __r;
    unsigned char *__p = (unsigned char *)&__r;
    __p[0]  = (unsigned char)__b0;  __p[1]  = (unsigned char)__b1;
    __p[2]  = (unsigned char)__b2;  __p[3]  = (unsigned char)__b3;
    __p[4]  = (unsigned char)__b4;  __p[5]  = (unsigned char)__b5;
    __p[6]  = (unsigned char)__b6;  __p[7]  = (unsigned char)__b7;
    __p[8]  = (unsigned char)__b8;  __p[9]  = (unsigned char)__b9;
    __p[10] = (unsigned char)__b10; __p[11] = (unsigned char)__b11;
    __p[12] = (unsigned char)__b12; __p[13] = (unsigned char)__b13;
    __p[14] = (unsigned char)__b14; __p[15] = (unsigned char)__b15;
    __p[16] = (unsigned char)__b16; __p[17] = (unsigned char)__b17;
    __p[18] = (unsigned char)__b18; __p[19] = (unsigned char)__b19;
    __p[20] = (unsigned char)__b20; __p[21] = (unsigned char)__b21;
    __p[22] = (unsigned char)__b22; __p[23] = (unsigned char)__b23;
    __p[24] = (unsigned char)__b24; __p[25] = (unsigned char)__b25;
    __p[26] = (unsigned char)__b26; __p[27] = (unsigned char)__b27;
    __p[28] = (unsigned char)__b28; __p[29] = (unsigned char)__b29;
    __p[30] = (unsigned char)__b30; __p[31] = (unsigned char)__b31;
    return __r;
}

/* === Insert === */

/* _mm256_inserti128_si256: insert 128-bit lane into 256-bit register */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_inserti128_si256(__m256i __a, __m128i __b, int __imm)
{
    __m256i __r = __a;
    if (__imm & 1) {
        __r.__val[2] = __b.__val[0];
        __r.__val[3] = __b.__val[1];
    } else {
        __r.__val[0] = __b.__val[0];
        __r.__val[1] = __b.__val[1];
    }
    return __r;
}

/* === Blend === */

/* _mm256_blend_epi32: VPBLENDD - blend 32-bit elements by immediate mask */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_blend_epi32(__m256i __a, __m256i __b, int __imm)
{
    int *__pa = (int *)&__a;
    int *__pb = (int *)&__b;
    __m256i __r;
    int *__pr = (int *)&__r;
    for (int __i = 0; __i < 8; __i++)
        __pr[__i] = (__imm & (1 << __i)) ? __pb[__i] : __pa[__i];
    return __r;
}

/* === Shuffle (32-bit) === */

/* _mm256_shuffle_epi32: VPSHUFD - shuffle 32-bit ints within 128-bit lanes */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_shuffle_epi32(__m256i __a, int __imm)
{
    unsigned int *__pa = (unsigned int *)&__a;
    __m256i __r;
    unsigned int *__pr = (unsigned int *)&__r;
    /* Low 128-bit lane */
    __pr[0] = __pa[(__imm >> 0) & 3];
    __pr[1] = __pa[(__imm >> 2) & 3];
    __pr[2] = __pa[(__imm >> 4) & 3];
    __pr[3] = __pa[(__imm >> 6) & 3];
    /* High 128-bit lane */
    __pr[4] = __pa[4 + ((__imm >> 0) & 3)];
    __pr[5] = __pa[4 + ((__imm >> 2) & 3)];
    __pr[6] = __pa[4 + ((__imm >> 4) & 3)];
    __pr[7] = __pa[4 + ((__imm >> 6) & 3)];
    return __r;
}

/* === Shift (16-bit) === */

/* _mm256_slli_epi16: VPSLLW - shift 16-bit ints left by immediate */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_slli_epi16(__m256i __a, int __count)
{
    if (__count < 0 || __count > 15)
        return _mm256_setzero_si256();
    unsigned short *__pa = (unsigned short *)&__a;
    __m256i __r;
    unsigned short *__pr = (unsigned short *)&__r;
    for (int __i = 0; __i < 16; __i++)
        __pr[__i] = (unsigned short)(__pa[__i] << __count);
    return __r;
}

/* _mm256_srli_epi16: VPSRLW - shift 16-bit ints right by immediate */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_srli_epi16(__m256i __a, int __count)
{
    if (__count < 0 || __count > 15)
        return _mm256_setzero_si256();
    unsigned short *__pa = (unsigned short *)&__a;
    __m256i __r;
    unsigned short *__pr = (unsigned short *)&__r;
    for (int __i = 0; __i < 16; __i++)
        __pr[__i] = __pa[__i] >> __count;
    return __r;
}

/* === Movemask === */

/* _mm256_movemask_epi8: VPMOVMSKB - create 32-bit mask from MSBs of bytes */
static __inline__ int __attribute__((__always_inline__))
_mm256_movemask_epi8(__m256i __a)
{
    unsigned char *__pa = (unsigned char *)&__a;
    int __mask = 0;
    for (int __i = 0; __i < 32; __i++) {
        if (__pa[__i] & 0x80)
            __mask |= (1 << __i);
    }
    return __mask;
}

/* === Permute (64-bit) === */

/* _mm256_permute4x64_epi64: VPERMQ - permute 64-bit elements across lanes */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_permute4x64_epi64(__m256i __a, int __imm)
{
    __m256i __r;
    __r.__val[0] = __a.__val[(__imm >> 0) & 3];
    __r.__val[1] = __a.__val[(__imm >> 2) & 3];
    __r.__val[2] = __a.__val[(__imm >> 4) & 3];
    __r.__val[3] = __a.__val[(__imm >> 6) & 3];
    return __r;
}

/* _mm256_permutevar8x32_epi32: VPERMD - permute 32-bit ints by variable index */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_permutevar8x32_epi32(__m256i __a, __m256i __idx)
{
    unsigned int *__pa = (unsigned int *)&__a;
    unsigned int *__pi = (unsigned int *)&__idx;
    __m256i __r;
    unsigned int *__pr = (unsigned int *)&__r;
    for (int __i = 0; __i < 8; __i++)
        __pr[__i] = __pa[__pi[__i] & 7];
    return __r;
}

/* === Unpack / Interleave === */

/* _mm256_unpacklo_epi8: VPUNPCKLBW - interleave low bytes within 128-bit lanes */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_unpacklo_epi8(__m256i __a, __m256i __b)
{
    unsigned char *__pa = (unsigned char *)&__a;
    unsigned char *__pb = (unsigned char *)&__b;
    __m256i __r;
    unsigned char *__pr = (unsigned char *)&__r;
    /* Low 128-bit lane: interleave bytes 0..7 */
    for (int __i = 0; __i < 8; __i++) {
        __pr[__i * 2]     = __pa[__i];
        __pr[__i * 2 + 1] = __pb[__i];
    }
    /* High 128-bit lane: interleave bytes 16..23 */
    for (int __i = 0; __i < 8; __i++) {
        __pr[16 + __i * 2]     = __pa[16 + __i];
        __pr[16 + __i * 2 + 1] = __pb[16 + __i];
    }
    return __r;
}

/* _mm256_unpackhi_epi8: VPUNPCKHBW - interleave high bytes within 128-bit lanes */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_unpackhi_epi8(__m256i __a, __m256i __b)
{
    unsigned char *__pa = (unsigned char *)&__a;
    unsigned char *__pb = (unsigned char *)&__b;
    __m256i __r;
    unsigned char *__pr = (unsigned char *)&__r;
    /* Low 128-bit lane: interleave bytes 8..15 */
    for (int __i = 0; __i < 8; __i++) {
        __pr[__i * 2]     = __pa[8 + __i];
        __pr[__i * 2 + 1] = __pb[8 + __i];
    }
    /* High 128-bit lane: interleave bytes 24..31 */
    for (int __i = 0; __i < 8; __i++) {
        __pr[16 + __i * 2]     = __pa[24 + __i];
        __pr[16 + __i * 2 + 1] = __pb[24 + __i];
    }
    return __r;
}

/* _mm256_unpacklo_epi16: VPUNPCKLWD - interleave low 16-bit ints within lanes */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_unpacklo_epi16(__m256i __a, __m256i __b)
{
    unsigned short *__pa = (unsigned short *)&__a;
    unsigned short *__pb = (unsigned short *)&__b;
    __m256i __r;
    unsigned short *__pr = (unsigned short *)&__r;
    /* Low 128-bit lane: interleave words 0..3 */
    for (int __i = 0; __i < 4; __i++) {
        __pr[__i * 2]     = __pa[__i];
        __pr[__i * 2 + 1] = __pb[__i];
    }
    /* High 128-bit lane: interleave words 8..11 */
    for (int __i = 0; __i < 4; __i++) {
        __pr[8 + __i * 2]     = __pa[8 + __i];
        __pr[8 + __i * 2 + 1] = __pb[8 + __i];
    }
    return __r;
}

/* _mm256_unpackhi_epi16: VPUNPCKHWD - interleave high 16-bit ints within lanes */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_unpackhi_epi16(__m256i __a, __m256i __b)
{
    unsigned short *__pa = (unsigned short *)&__a;
    unsigned short *__pb = (unsigned short *)&__b;
    __m256i __r;
    unsigned short *__pr = (unsigned short *)&__r;
    /* Low 128-bit lane: interleave words 4..7 */
    for (int __i = 0; __i < 4; __i++) {
        __pr[__i * 2]     = __pa[4 + __i];
        __pr[__i * 2 + 1] = __pb[4 + __i];
    }
    /* High 128-bit lane: interleave words 12..15 */
    for (int __i = 0; __i < 4; __i++) {
        __pr[8 + __i * 2]     = __pa[12 + __i];
        __pr[8 + __i * 2 + 1] = __pb[12 + __i];
    }
    return __r;
}

/* _mm256_unpacklo_epi32: VPUNPCKLDQ - interleave low 32-bit ints within lanes */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_unpacklo_epi32(__m256i __a, __m256i __b)
{
    unsigned int *__pa = (unsigned int *)&__a;
    unsigned int *__pb = (unsigned int *)&__b;
    __m256i __r;
    unsigned int *__pr = (unsigned int *)&__r;
    /* Low 128-bit lane */
    __pr[0] = __pa[0]; __pr[1] = __pb[0];
    __pr[2] = __pa[1]; __pr[3] = __pb[1];
    /* High 128-bit lane */
    __pr[4] = __pa[4]; __pr[5] = __pb[4];
    __pr[6] = __pa[5]; __pr[7] = __pb[5];
    return __r;
}

/* _mm256_unpackhi_epi32: VPUNPCKHDQ - interleave high 32-bit ints within lanes */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_unpackhi_epi32(__m256i __a, __m256i __b)
{
    unsigned int *__pa = (unsigned int *)&__a;
    unsigned int *__pb = (unsigned int *)&__b;
    __m256i __r;
    unsigned int *__pr = (unsigned int *)&__r;
    /* Low 128-bit lane */
    __pr[0] = __pa[2]; __pr[1] = __pb[2];
    __pr[2] = __pa[3]; __pr[3] = __pb[3];
    /* High 128-bit lane */
    __pr[4] = __pa[6]; __pr[5] = __pb[6];
    __pr[6] = __pa[7]; __pr[7] = __pb[7];
    return __r;
}

/* _mm256_unpacklo_epi64: VPUNPCKLQDQ - interleave low 64-bit ints within lanes */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_unpacklo_epi64(__m256i __a, __m256i __b)
{
    return (__m256i){ { __a.__val[0], __b.__val[0], __a.__val[2], __b.__val[2] } };
}

/* _mm256_unpackhi_epi64: VPUNPCKHQDQ - interleave high 64-bit ints within lanes */
static __inline__ __m256i __attribute__((__always_inline__))
_mm256_unpackhi_epi64(__m256i __a, __m256i __b)
{
    return (__m256i){ { __a.__val[1], __b.__val[1], __a.__val[3], __b.__val[3] } };
}

#endif /* _AVX2INTRIN_H_INCLUDED */
