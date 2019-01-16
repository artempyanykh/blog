#include <stdio.h>

extern char **environ;

int main(int argc, const char *argv[])
{
        int envn = 0;

        while (*(environ + envn) != NULL) {
                printf("envp[%d] = %s\n", envn, *(environ + envn));
                envn++;
        }

        return 0;
}
