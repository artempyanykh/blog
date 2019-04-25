#include <stdint.h>
#include <stdalign.h>

alignas(64) int foo = 42;

int isAligned()
{
        return ((uintptr_t)&foo & -64) == 0;
}
