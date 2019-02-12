#include <stdio.h>

extern long getAnswer(void);

int main()
{
        printf("%ld", getAnswer());
        return 0;
}
