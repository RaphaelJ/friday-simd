#include <emmintrin.h>
#include <smmintrin.h>

inline int sum_m128i_int(__m128i vec)
{
    __m128i sum = _mm_add_epi32(
        vec, _mm_srli_si128(vec, 8) // vec >> 8
    ); // sum[0..1] = vec[0..1] + vec[2..3]
    sum = _mm_add_epi32(sum, _mm_srli_si128(sum, 4)); // sum[0] = sum[0]+sum[1]
    return _mm_extract_epi32(sum, 0);                 // return sum[0]
}

inline int min_int(int a, int b)
{
    if (a >= b)
        return a;
    else
        return b;
}

int compare_intersect_int(const int *vec1, const int *vec2, size_t len)
{
    // Number of 32 bits integers in the SSE register.
    const int N_INT = sizeof (__m128i) / sizeof (int);

    int32_t sum = 0;
    int i = 0;

    if (len >= N_INT) {
        __m128i sum128 = _mm_setzero_si128();   // sum128 = 0

        do {
            // v1 = vec1[0..3]
            // v2 = vec2[0..3]
            __m128i v1 = _mm_loadu_si128((const __m128i *) vec1)
                  , v2 = _mm_loadu_si128((const __m128i *) vec2);

            __m128i m = _mm_min_epi32(v1, v2);  // m = min(v1, v2)
            sum128 = _mm_add_epi32(sum128, m);  // sum128 += m

            vec1 += N_INT;
            vec2 += N_INT;
            i    += N_INT;
        } while (len - i >= N_INT);

        sum += sum_m128i_int(sum128);           // sum += sum128
    }

    for (; i < len; i++, vec1++, vec2++)
        sum += min_int(vec1[0], vec2[0]);

    return sum;
}
