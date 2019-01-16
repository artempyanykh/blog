extern int printf(const char *, ...);

int i = 0;

int inci(void)
{
        printf("%d", i++);
        return i;
}
