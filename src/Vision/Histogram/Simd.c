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

double compare_chi_int(const int *vec1, const int *vec2, size_t len)
{
    double sum = 0;
    size_t i = 0;

    #if AVX2
        if (len >= N_INT_128) {
            // Uses two 256 bits registers to store the accumulators as the
            // coefficient (64 bits) is two times as large as the histogram
            // values (32 bits).
            __m256d sum_256 = _mm256_setzero_pd();    // sum_256 = 0

            __m256d zeros   = _mm256_setzero_pd();

            do {
                // v1 = vec1[0..3]
                // v2 = vec2[0..3]
                __m128i v1 = _mm_loadu_si128((const __m128i *) vec1)
                      , v2 = _mm_loadu_si128((const __m128i *) vec2);

                // diff   = v1 - v2
                __m128i diff = _mm_sub_epi32(v1, v2);
                // square = diff * diff
                __m128i square  = _mm_mul_epi32(diff, diff);
                // add    = v1 + v2
                __m128i add     = _mm_add_epi32(v1, v2);

                // Converts 'add' and 'square' to double vectors.
                __m256d fadd    = _mm256_cvtepi32_pd(add)
                      , fsquare = _mm256_cvtepi32_pd(square);

                // div    = square / add
                __m256d div    = _mm256_div_pd(fsquare, fadd);

                // Fixs division by zero. Replaces division results by zero for
                // cell on which 'fadd' was equal to zero.
                __m256d is_zero = _mm256_cmp_pd(fadd, zeros, _CMP_EQ_OQ);
                __m256d div_2 = _mm256_blendv_pd(div, zeros, is_zero);

                // sum_256    += div_2
                sum_256 = _mm256_add_pd(sum_256, div_2);

                vec1 += N_INT_128;
                vec2 += N_INT_128;
                i    += N_INT_128;
            } while (len - i >= N_INT_128);

            sum += sum_m256d(sum_256);                // sum += sum_256
        }
    #endif

    for (; i < len; i++, vec1++, vec2++) {
        int v1 = vec1[0]
          , v2 = vec2[0];
        int denom = v1 + v2;

        if (denom != 0) {
            int diff = v1 - v2;
            sum += (double) (diff * diff) / (double) denom;
        }
    }

    return sum * 2;
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

    #if SSE41 // _mm_min_epi32 is from SSE 4.1
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
