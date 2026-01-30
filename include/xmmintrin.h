/* CCC compiler bundled xmmintrin.h - SSE intrinsics */
#ifndef _XMMINTRIN_H_INCLUDED
#define _XMMINTRIN_H_INCLUDED

#include <mmintrin.h>

typedef struct __attribute__((__aligned__(16))) {
    float __val[4];
} __m128;

/* Internal vector type referenced by GCC system headers.
 * Note: vector_size attribute is parsed but vectors are lowered as aggregates. */
typedef float __v4sf __attribute__ ((__vector_size__ (16)));

/* _MM_SHUFFLE: build an immediate for _mm_shuffle_ps / _mm_shuffle_epi32.
 * The result encodes four 2-bit lane selectors as (z<<6|y<<4|x<<2|w). */
#define _MM_SHUFFLE(z, y, x, w) (((z) << 6) | ((y) << 4) | ((x) << 2) | (w))

/* === Set / Broadcast === */

static __inline__ __m128 __attribute__((__always_inline__))
_mm_setzero_ps(void)
{
    return (__m128){ { 0.0f, 0.0f, 0.0f, 0.0f } };
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_set1_ps(float __w)
{
    return (__m128){ { __w, __w, __w, __w } };
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_set_ps(float __z, float __y, float __x, float __w)
{
    return (__m128){ { __w, __x, __y, __z } };
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_setr_ps(float __w, float __x, float __y, float __z)
{
    return (__m128){ { __w, __x, __y, __z } };
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_set_ss(float __w)
{
    return (__m128){ { __w, 0.0f, 0.0f, 0.0f } };
}

/* === Load === */

static __inline__ __m128 __attribute__((__always_inline__))
_mm_loadu_ps(const float *__p)
{
    __m128 __r;
    __builtin_memcpy(&__r, __p, 16);
    return __r;
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_load_ps(const float *__p)
{
    return *(const __m128 *)__p;
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_load_ss(const float *__p)
{
    return (__m128){ { *__p, 0.0f, 0.0f, 0.0f } };
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_load1_ps(const float *__p)
{
    float __v = *__p;
    return (__m128){ { __v, __v, __v, __v } };
}

#define _mm_load_ps1(p) _mm_load1_ps(p)

/* === Store === */

static __inline__ void __attribute__((__always_inline__))
_mm_storeu_ps(float *__p, __m128 __a)
{
    __builtin_memcpy(__p, &__a, 16);
}

static __inline__ void __attribute__((__always_inline__))
_mm_store_ps(float *__p, __m128 __a)
{
    *((__m128 *)__p) = __a;
}

static __inline__ void __attribute__((__always_inline__))
_mm_store_ss(float *__p, __m128 __a)
{
    *__p = __a.__val[0];
}

static __inline__ void __attribute__((__always_inline__))
_mm_store1_ps(float *__p, __m128 __a)
{
    __p[0] = __a.__val[0]; __p[1] = __a.__val[0];
    __p[2] = __a.__val[0]; __p[3] = __a.__val[0];
}

#define _mm_store_ps1(p, a) _mm_store1_ps(p, a)

/* Store the lower 2 floats of __m128 to __m64* memory location. */
static __inline__ void __attribute__((__always_inline__))
_mm_storel_pi(__m64 *__p, __m128 __a)
{
    __builtin_memcpy(__p, &__a, 8);
}

/* Store the upper 2 floats of __m128 to __m64* memory location. */
static __inline__ void __attribute__((__always_inline__))
_mm_storeh_pi(__m64 *__p, __m128 __a)
{
    __builtin_memcpy(__p, (const char *)&__a + 8, 8);
}

/* === Arithmetic === */

static __inline__ __m128 __attribute__((__always_inline__))
_mm_add_ps(__m128 __a, __m128 __b)
{
    return (__m128){ { __a.__val[0] + __b.__val[0], __a.__val[1] + __b.__val[1],
                       __a.__val[2] + __b.__val[2], __a.__val[3] + __b.__val[3] } };
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_sub_ps(__m128 __a, __m128 __b)
{
    return (__m128){ { __a.__val[0] - __b.__val[0], __a.__val[1] - __b.__val[1],
                       __a.__val[2] - __b.__val[2], __a.__val[3] - __b.__val[3] } };
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_mul_ps(__m128 __a, __m128 __b)
{
    return (__m128){ { __a.__val[0] * __b.__val[0], __a.__val[1] * __b.__val[1],
                       __a.__val[2] * __b.__val[2], __a.__val[3] * __b.__val[3] } };
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_div_ps(__m128 __a, __m128 __b)
{
    return (__m128){ { __a.__val[0] / __b.__val[0], __a.__val[1] / __b.__val[1],
                       __a.__val[2] / __b.__val[2], __a.__val[3] / __b.__val[3] } };
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_min_ps(__m128 __a, __m128 __b)
{
    return (__m128){ { __a.__val[0] < __b.__val[0] ? __a.__val[0] : __b.__val[0],
                       __a.__val[1] < __b.__val[1] ? __a.__val[1] : __b.__val[1],
                       __a.__val[2] < __b.__val[2] ? __a.__val[2] : __b.__val[2],
                       __a.__val[3] < __b.__val[3] ? __a.__val[3] : __b.__val[3] } };
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_max_ps(__m128 __a, __m128 __b)
{
    return (__m128){ { __a.__val[0] > __b.__val[0] ? __a.__val[0] : __b.__val[0],
                       __a.__val[1] > __b.__val[1] ? __a.__val[1] : __b.__val[1],
                       __a.__val[2] > __b.__val[2] ? __a.__val[2] : __b.__val[2],
                       __a.__val[3] > __b.__val[3] ? __a.__val[3] : __b.__val[3] } };
}

/* Scalar operations (lowest element only, rest pass through __a) */

static __inline__ __m128 __attribute__((__always_inline__))
_mm_add_ss(__m128 __a, __m128 __b)
{
    __a.__val[0] += __b.__val[0];
    return __a;
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_sub_ss(__m128 __a, __m128 __b)
{
    __a.__val[0] -= __b.__val[0];
    return __a;
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_mul_ss(__m128 __a, __m128 __b)
{
    __a.__val[0] *= __b.__val[0];
    return __a;
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_div_ss(__m128 __a, __m128 __b)
{
    __a.__val[0] /= __b.__val[0];
    return __a;
}

/* === Shuffle === */

/* _mm_shuffle_ps: shuffle floats from __a and __b using immediate mask.
 * Bits [1:0] select from __a for element 0, [3:2] for element 1,
 * [5:4] select from __b for element 2, [7:6] for element 3. */
#define _mm_shuffle_ps(__a, __b, __imm) __extension__ ({ \
    __m128 __r; \
    __r.__val[0] = (__a).__val[(__imm) & 3]; \
    __r.__val[1] = (__a).__val[((__imm) >> 2) & 3]; \
    __r.__val[2] = (__b).__val[((__imm) >> 4) & 3]; \
    __r.__val[3] = (__b).__val[((__imm) >> 6) & 3]; \
    __r; \
})

/* === Unpack / Interleave === */

static __inline__ __m128 __attribute__((__always_inline__))
_mm_unpacklo_ps(__m128 __a, __m128 __b)
{
    return (__m128){ { __a.__val[0], __b.__val[0], __a.__val[1], __b.__val[1] } };
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_unpackhi_ps(__m128 __a, __m128 __b)
{
    return (__m128){ { __a.__val[2], __b.__val[2], __a.__val[3], __b.__val[3] } };
}

/* === Move === */

static __inline__ __m128 __attribute__((__always_inline__))
_mm_movehl_ps(__m128 __a, __m128 __b)
{
    return (__m128){ { __b.__val[2], __b.__val[3], __a.__val[2], __a.__val[3] } };
}

static __inline__ __m128 __attribute__((__always_inline__))
_mm_movelh_ps(__m128 __a, __m128 __b)
{
    return (__m128){ { __a.__val[0], __a.__val[1], __b.__val[0], __b.__val[1] } };
}

static __inline__ float __attribute__((__always_inline__))
_mm_cvtss_f32(__m128 __a)
{
    return __a.__val[0];
}

/* === Compare (packed) - return all-ones or all-zeros per lane === */

static __inline__ int __attribute__((__always_inline__))
_mm_movemask_ps(__m128 __a)
{
    int __r = 0;
    unsigned int __u;
    __builtin_memcpy(&__u, &__a.__val[0], 4); __r |= (__u >> 31);
    __builtin_memcpy(&__u, &__a.__val[1], 4); __r |= ((__u >> 31) << 1);
    __builtin_memcpy(&__u, &__a.__val[2], 4); __r |= ((__u >> 31) << 2);
    __builtin_memcpy(&__u, &__a.__val[3], 4); __r |= ((__u >> 31) << 3);
    return __r;
}

/* === Fence === */

static __inline__ void __attribute__((__always_inline__))
_mm_sfence(void)
{
    __builtin_ia32_sfence();
}

static __inline__ void __attribute__((__always_inline__))
_mm_pause(void)
{
    __builtin_ia32_pause();
}

#endif /* _XMMINTRIN_H_INCLUDED */
