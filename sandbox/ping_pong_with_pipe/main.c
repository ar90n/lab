#include <stdio.h>
#include <unistd.h>
#include <time.h>

int main(int argc, char *argv[])
{
    int pipe_req[2];
    int pipe_ack[2];

    pipe(pipe_req);
    pipe(pipe_ack);
    if(fork() == 0) {
        int buf;
        while(read(pipe_req[0], &buf, sizeof(int)) == sizeof(int)) {
            write(pipe_ack[1], &buf, sizeof(int));
        }
        return 0;
    }

    int buf;
    int count = 0;
    int n_max = 64 * 1024;
    clock_t before = clock();
    for(int i = 0; i < n_max; i++) {
        write(pipe_req[1], &i, sizeof(int));
        read(pipe_ack[0], &buf, sizeof(int));
        count += i == buf;
    }
    clock_t after = clock();
    double secs = (after - before) / (double)CLOCKS_PER_SEC;
    printf("%f [times/sec]\n", count/secs);
    if(n_max != count) {
        printf("error: %d != %d\n", n_max, count);
    }
    return 0;
}