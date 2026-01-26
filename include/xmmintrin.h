/* CCC compiler bundled xmmintrin.h - SSE intrinsics */
#ifndef _XMMINTRIN_H_INCLUDED
#define _XMMINTRIN_H_INCLUDED

typedef struct __attribute__((__aligned__(16))) {
    float __val[4];
} __m128;

/* Internal vector type referenced by GCC system headers.
 * Note: vector_size attribute is parsed but vectors are lowered as aggregates. */
typedef float __v4sf __attribute__ ((__vector_size__ (16)));

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
