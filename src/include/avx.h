#ifndef AVX_H
#define AVX_H

#include <immintrin.h>

#include "sse41.h"

// Number of 32 bits integers in the 256 bits AVX register.
const int N_INT_256 = sizeof (__m256) / sizeof (int);

// Returns the sum of 64 bits floating point numbers in the register.
inline int sum_m256d(__m256d vec)
{
    return sum_m128d(_mm256_extractf128_pd(vec, 0))
         + sum_m128d(_mm256_extractf128_pd(vec, 0));
}

#endif
