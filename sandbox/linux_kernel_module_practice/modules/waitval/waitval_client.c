#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define DEV_PATH "/dev/waitval0"
#define BUF_SIZE 256

static int read_block(void)
{
    int fd;
    char buf[BUF_SIZE + 1];
    ssize_t n;

    fd = open(DEV_PATH, O_RDONLY);
    if (fd < 0) {
        perror("open");
        return 1;
    }

    printf("blocking read: waiting...\n");

    n = read(fd, buf, BUF_SIZE);
    if (n < 0) {
        perror("read");
        close(fd);
        return 1;
    }

    buf[n] = '\0';
    printf("read %zd bytes: %s", n, buf);

    if (n == 0 || buf[n - 1] != '\n')
        printf("\n");

    close(fd);
    return 0;
}

static int read_nonblock(void)
{
    int fd;
    char buf[BUF_SIZE + 1];
    ssize_t n;

    fd = open(DEV_PATH, O_RDONLY | O_NONBLOCK);
    if (fd < 0) {
        perror("open");
        return 1;
    }

    n = read(fd, buf, BUF_SIZE);
    if (n < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            printf("no data: EAGAIN/EWOULDBLOCK\n");
            close(fd);
            return 0;
        }

        perror("read");
        close(fd);
        return 1;
    }

    buf[n] = '\0';
    printf("read %zd bytes: %s", n, buf);

    if (n == 0 || buf[n - 1] != '\n')
        printf("\n");

    close(fd);
    return 0;
}

static int write_msg(const char *msg)
{
    int fd;
    ssize_t n;

    fd = open(DEV_PATH, O_WRONLY);
    if (fd < 0) {
        perror("open");
        return 1;
    }

    n = write(fd, msg, strlen(msg));
    if (n < 0) {
        perror("write");
        close(fd);
        return 1;
    }

    printf("wrote %zd bytes\n", n);

    close(fd);
    return 0;
}

static void usage(const char *prog)
{
    fprintf(stderr,
            "Usage:\n"
            "  %s read-block\n"
            "  %s read-nonblock\n"
            "  %s write <message>\n",
            prog, prog, prog);
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        usage(argv[0]);
        return 1;
    }

    if (strcmp(argv[1], "read-block") == 0)
        return read_block();

    if (strcmp(argv[1], "read-nonblock") == 0)
        return read_nonblock();

    if (strcmp(argv[1], "write") == 0) {
        if (argc != 3) {
            usage(argv[0]);
            return 1;
        }

        return write_msg(argv[2]);
    }

    usage(argv[0]);
    return 1;
}
