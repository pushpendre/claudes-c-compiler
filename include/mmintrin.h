/* CCC compiler bundled mmintrin.h - MMX intrinsics */
#ifndef _MMINTRIN_H_INCLUDED
#define _MMINTRIN_H_INCLUDED

/* __m64: 64-bit MMX vector type.
 * Used by SSE storel/storeh intrinsics and legacy MMX code. */
typedef struct __attribute__((__aligned__(8))) {
    long long __val;
} __m64;

#endif /* _MMINTRIN_H_INCLUDED */
