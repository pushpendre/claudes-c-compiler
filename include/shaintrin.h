/* CCC compiler bundled shaintrin.h - SHA-NI intrinsics */
#ifndef _SHAINTRIN_H_INCLUDED
#define _SHAINTRIN_H_INCLUDED

#include <emmintrin.h>

/* SHA256 round function: perform 2 rounds of SHA-256 using state in __a, __b
 * and message/constant sum in __c (only low 2 dwords of __c are used).
 * Corresponds to x86 SHA256RNDS2 instruction. */
static __inline__ __m128i __attribute__((__always_inline__))
_mm_sha256rnds2_epu32(__m128i __a, __m128i __b, __m128i __c)
{
    unsigned int *__pa = (unsigned int *)&__a;
    unsigned int *__pb = (unsigned int *)&__b;
    unsigned int *__pc = (unsigned int *)&__c;
    __m128i __r;
    unsigned int *__pr = (unsigned int *)&__r;

    /* State mapping (SHA-256 working variables):
     * __a = {C1, D1, G1, H1}  (indices 0,1,2,3)
     * __b = {A1, B1, E1, F1}  (indices 0,1,2,3)
     * __c low 2 dwords = WK0, WK1 */

    /* ABEF layout: __b[3]=A, __b[2]=B, __b[1]=E, __b[0]=F
     * CDGH layout: __a[3]=C, __a[2]=D, __a[1]=G, __a[0]=H */
    unsigned int A = __pb[3], B = __pb[2], E = __pb[1], F = __pb[0];
    unsigned int C = __pa[3], D = __pa[2], G = __pa[1], H = __pa[0];

    unsigned int W0K = __pc[0], W1K = __pc[1];

    /* Utility macros */
#define __SHA256_CH(e, f, g)  (((e) & (f)) ^ (~(e) & (g)))
#define __SHA256_MAJ(a, b, c) (((a) & (b)) ^ ((a) & (c)) ^ ((b) & (c)))
#define __SHA256_ROR(x, n)    (((x) >> (n)) | ((x) << (32 - (n))))
#define __SHA256_SIGMA0(a)    (__SHA256_ROR(a, 2) ^ __SHA256_ROR(a, 13) ^ __SHA256_ROR(a, 22))
#define __SHA256_SIGMA1(e)    (__SHA256_ROR(e, 6) ^ __SHA256_ROR(e, 11) ^ __SHA256_ROR(e, 25))

    /* Round 0 (using W0K) */
    unsigned int T1_0 = H + __SHA256_SIGMA1(E) + __SHA256_CH(E, F, G) + W0K;
    unsigned int T2_0 = __SHA256_SIGMA0(A) + __SHA256_MAJ(A, B, C);
    unsigned int H2 = G, G2 = F, F2 = E, E2 = D + T1_0;
    unsigned int D2 = C, C2 = B, B2 = A, A2 = T1_0 + T2_0;

    /* Round 1 (using W1K) */
    unsigned int T1_1 = H2 + __SHA256_SIGMA1(E2) + __SHA256_CH(E2, F2, G2) + W1K;
    unsigned int T2_1 = __SHA256_SIGMA0(A2) + __SHA256_MAJ(A2, B2, C2);
    /* Only A3, B3, E3 and F3(=E2) are needed for the output ABEF state */
    unsigned int E3 = D2 + T1_1;
    unsigned int B3 = A2;
    unsigned int A3 = T1_1 + T2_1;

    /* Output: new {A, B, E, F} */
    __pr[3] = A3;
    __pr[2] = B3;
    __pr[1] = E3;
    __pr[0] = E2; /* F3 = E2 */

#undef __SHA256_CH
#undef __SHA256_MAJ
#undef __SHA256_ROR
#undef __SHA256_SIGMA0
#undef __SHA256_SIGMA1

    return __r;
}

/* SHA256MSG1: perform an intermediate calculation for the next four
 * SHA256 message dwords. Corresponds to SHA256MSG1 instruction. */
static __inline__ __m128i __attribute__((__always_inline__))
_mm_sha256msg1_epu32(__m128i __a, __m128i __b)
{
    unsigned int *__pa = (unsigned int *)&__a;
    unsigned int *__pb = (unsigned int *)&__b;
    __m128i __r;
    unsigned int *__pr = (unsigned int *)&__r;

#define __SHA256_SIGMA0_MSG(x) \
    (((x) >> 7 | (x) << 25) ^ ((x) >> 18 | (x) << 14) ^ ((x) >> 3))

    __pr[0] = __pa[0] + __SHA256_SIGMA0_MSG(__pa[1]);
    __pr[1] = __pa[1] + __SHA256_SIGMA0_MSG(__pa[2]);
    __pr[2] = __pa[2] + __SHA256_SIGMA0_MSG(__pa[3]);
    __pr[3] = __pa[3] + __SHA256_SIGMA0_MSG(__pb[0]);

#undef __SHA256_SIGMA0_MSG

    return __r;
}

/* SHA256MSG2: perform the final calculation for the next four
 * SHA256 message dwords. Corresponds to SHA256MSG2 instruction. */
static __inline__ __m128i __attribute__((__always_inline__))
_mm_sha256msg2_epu32(__m128i __a, __m128i __b)
{
    unsigned int *__pa = (unsigned int *)&__a;
    unsigned int *__pb = (unsigned int *)&__b;
    __m128i __r;
    unsigned int *__pr = (unsigned int *)&__r;

#define __SHA256_SIGMA1_MSG(x) \
    (((x) >> 17 | (x) << 15) ^ ((x) >> 19 | (x) << 13) ^ ((x) >> 10))

    __pr[0] = __pa[0] + __SHA256_SIGMA1_MSG(__pb[2]);
    __pr[1] = __pa[1] + __SHA256_SIGMA1_MSG(__pb[3]);
    __pr[2] = __pa[2] + __SHA256_SIGMA1_MSG(__pr[0]);
    __pr[3] = __pa[3] + __SHA256_SIGMA1_MSG(__pr[1]);

#undef __SHA256_SIGMA1_MSG

    return __r;
}

#endif /* _SHAINTRIN_H_INCLUDED */
