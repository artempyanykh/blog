#include <stdio.h>
#include <immintrin.h>

__m128 mvec   = {1.0, 2.0, 3.0, 42.0};
float  fvec[] = {.0, .0, .0, .0};

float f(void)
{
        _mm_store_ps(&fvec, mvec);
        return fvec[3];
}
