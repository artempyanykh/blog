int i = 10;
int j = 0;
int k;
extern int n;
static int m = 0;

int sum(void)
{
        static int nCall;
        nCall++;
        m++;
        return i + j;
}
