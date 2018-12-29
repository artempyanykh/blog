#include <stdio.h>

int main(int argc, const char *argv[])
{
        int res = 0;
        res = 1 / 0;
        printf("Result is %d", res);

        return 0;
}
