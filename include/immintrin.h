/* CCC compiler bundled immintrin.h - all x86 SIMD intrinsics */
#ifndef _IMMINTRIN_H_INCLUDED
#define _IMMINTRIN_H_INCLUDED

/* x86 SIMD intrinsics are only available on x86/x86-64 targets */
#if !defined(__x86_64__) && !defined(__i386__) && !defined(__i686__)
#error "x86 SIMD intrinsics (immintrin.h) require an x86 target"
#endif

#include <wmmintrin.h>
#include <smmintrin.h>
#include <shaintrin.h>
#include <bmi2intrin.h>
#include <avxintrin.h>
#include <avx2intrin.h>
#include <fmaintrin.h>
#include <avx512fintrin.h>

#endif /* _IMMINTRIN_H_INCLUDED */
