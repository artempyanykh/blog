#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>

int main(int argc, const char *argv[])
{
        if (argc < 2) {
                printf("Usage: c-fizzbuzz <number>\n");
                return 0;
        }

        char *endptr;
        long n = strtol(argv[1], &endptr, 10);
        if (endptr[0] != '\0') {
                printf("Error: not a number\n");
                return 1;
        }

        for (long i = 1; i <= n; i++) {
                long mod3 = i % 3;
                long mod5 = i % 5;
                if (mod3 == 0) {
                        printf("Fizz");
                }
                if (mod5 == 0) {
                        printf("Buzz");
                } else {
                        if (mod3 != 0) {
                                printf("%ld", i);
                        }
                }
                printf("\n");
        }

        return 0;
}
