#ifndef SSE41_H
#define SSE41_H

#include <smmintrin.h>

// Number of 32 bits integers in the 128 bits SSE register.
const int N_INT_128 = sizeof (__m128i) / sizeof (int);

// Returns the sum of double precision numbers in the register.
inline int sum_m128d(__m128d vec)
{
    return _mm_cvtsd_f64(vec)                        //   vec[0]
         + _mm_cvtsd_f64(_mm_unpackhi_pd(vec, vec)); // + vec[1]
}

// Returns the sum of 32 bits integers in the register.
inline int sum_m128i_int(__m128i vec)
{
    __m128i sum = _mm_add_epi32(
        vec, _mm_srli_si128(vec, 8) // vec >> 8
    ); // sum[0..1] = vec[0..1] + vec[2..3]
    sum = _mm_add_epi32(sum, _mm_srli_si128(sum, 4)); // sum[0] = sum[0]
                                                      //        + sum[1]
    return _mm_extract_epi32(sum, 0);                 // return sum[0]
}

#endif
