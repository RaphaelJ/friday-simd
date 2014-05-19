#ifndef AVX2_H
#define AVX2_H

#include <immintrin.h>

#include "avx.h"

// Returns the sum of 32 bits integers in the register.
inline int sum_m256i_int(__m256i vec)
{
    return sum_m128i_int(_mm256_extracti128_si256(vec, 0))
         + sum_m128i_int(_mm256_extracti128_si256(vec, 1));
}

#endif
