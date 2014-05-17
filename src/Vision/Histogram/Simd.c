#include <emmintrin.h>
#include <stdio.h>

#if SSE41
    #include "sse41.h"
#endif

#if AVX
#endif

#if AVX2
    #include "avx2.h"
#endif

inline int min_int(int a, int b)
{
    if (a >= b)
        return a;
    else
        return b;
}

int compare_intersect_int(const int *vec1, const int *vec2, size_t len)
{
    int sum = 0;
    size_t i = 0;

    #if AVX2
        if (len >= N_INT_256) {
            __m256i sum_256 = _mm256_setzero_si256();   // sum_256 = 0

            do {
                // v1 = vec1[0..7]
                // v2 = vec2[0..7]
                __m256i v1 = _mm256_loadu_si256((const __m256i *) vec1)
                      , v2 = _mm256_loadu_si256((const __m256i *) vec2);

                __m256i m = _mm256_min_epi32(v1, v2);   // m = min(v1, v2)
                sum_256 = _mm256_add_epi32(sum_256, m); // sum_256 += m

                vec1 += N_INT_256;
                vec2 += N_INT_256;
                i    += N_INT_256;
            } while (len - i >= N_INT_256);

            sum += sum_m256i_int(sum_256);              // sum += sum_256
        }
    #endif

    #if SSE41
        if (len - i >= N_INT_128) {
            __m128i sum_128 = _mm_setzero_si128();      // sum_128 = 0

            do {
                // v1 = vec1[0..3]
                // v2 = vec2[0..3]
                __m128i v1 = _mm_loadu_si128((const __m128i *) vec1)
                      , v2 = _mm_loadu_si128((const __m128i *) vec2);

                __m128i m = _mm_min_epi32(v1, v2);      // m = min(v1, v2)
                sum_128 = _mm_add_epi32(sum_128, m);    // sum_128 += m

                vec1 += N_INT_128;
                vec2 += N_INT_128;
                i    += N_INT_128;
            } while (len - i >= N_INT_128);

            sum += sum_m128i_int(sum_128);              // sum += sum_128
        }
    #endif

    for (; i < len; i++, vec1++, vec2++)
        sum += min_int(vec1[0], vec2[0]);

    return sum;
}
