#ifndef AVX2_H
#define AVX2_H

#include <immintrin.h>

#include "sse41.h"

// Number of 32 bits integers in the 256 bits AVX register.
const int N_INT_256 = sizeof (__m256i) / sizeof (int);

// Returns the sum of 32 bits integers in the register.
inline int sum_m256i_int(__m256i vec)
{
    __m256i sum = _mm256_add_epi32(
        vec, _mm256_srli_si256(vec, 16) // vec >> 16
    ); // sum[0..3] = vec[0..3] + vec[4..7]
    return sum_m128i_int(_mm256_extracti128_si256(sum, 0));
}

#endif
