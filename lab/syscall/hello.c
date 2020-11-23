#include <unistd.h>
#include <sys/syscall.h>
#include <string.h>

int main()
{
    const char *msg = "Hello, world\n";

    syscall(SYS_write, 1, msg, strlen(msg));
    return 42;
}