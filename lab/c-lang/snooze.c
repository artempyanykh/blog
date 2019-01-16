#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>

void sig_ign(int signum) {
        printf("Caught sigint, going to sleep...");
        sleep(2);
        printf(" Done sleeping.\n");
}

int main(int argc, const char *argv[])
{
        if (argc < 2) {
                printf("Not enough arguments\n");
                exit(0);
        }
        int to_sleep = atoi(argv[1]);

        signal(SIGINT, &sig_ign);

        int left = sleep(to_sleep);

        printf("Slept for %d out of %d secs.\n", to_sleep - left, to_sleep);

        return 0;
}
