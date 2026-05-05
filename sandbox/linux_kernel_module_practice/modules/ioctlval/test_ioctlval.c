#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

#include "ioctlval_uapi.h"

static void usage(const char *prog)
{
    fprintf(stderr,
            "Usage:\n"
            "  %s get\n"
            "  %s set <value>\n",
            prog, prog);
}

int main(int argc, char **argv)
{
    const char *path = "/dev/ioctlval0";
    int fd;
    int32_t val;

    if (argc < 2) {
        usage(argv[0]);
        return 1;
    }

    fd = open(path, O_RDWR);
    if (fd < 0) {
        fprintf(stderr, "open %s failed: %s\n", path, strerror(errno));
        return 1;
    }

    if (strcmp(argv[1], "get") == 0) {
        if (ioctl(fd, IOCTLVAL_GET_VAL, &val) < 0) {
            fprintf(stderr, "ioctl GET_VAL failed: %s\n", strerror(errno));
            close(fd);
            return 1;
        }

        printf("%d\n", val);
    } else if (strcmp(argv[1], "set") == 0) {
        if (argc != 3) {
            usage(argv[0]);
            close(fd);
            return 1;
        }

        val = (int32_t)strtol(argv[2], NULL, 0);

        if (ioctl(fd, IOCTLVAL_SET_VAL, &val) < 0) {
            fprintf(stderr, "ioctl SET_VAL failed: %s\n", strerror(errno));
            close(fd);
            return 1;
        }
    } else {
        usage(argv[0]);
        close(fd);
        return 1;
    }

    close(fd);
    return 0;
}
