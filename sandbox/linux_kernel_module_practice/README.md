# Linux Device Driver Programming Practice

A QEMU x86_64 sandbox for practicing Linux kernel module development.

## Stack

- Kernel: Linux v6.6
- rootfs: minimal initramfs built from statically-linked BusyBox
- Host ⇄ guest file sharing: 9p virtfs (`shared/` → `/mnt/shared`)

## Prerequisites

- Linux host (Ubuntu 24.04 etc.)
- CPU with hardware virtualization (recommended)

## Layout

```
.
├── env/         # gitignored: kernel + busybox sources
├── modules/     # your kernel modules
├── scripts/     # setup/build/run scripts
├── initramfs/   # init script
└── shared/      # gitignored: drop .ko here
```

## Setup

```bash
./scripts/setup.sh           # install deps, clone kernel & busybox
./scripts/build-kernel.sh    # build the kernel
./scripts/build-busybox.sh   # build busybox
./scripts/mkinitramfs.sh     # assemble the initramfs
```

## Run

```bash
./scripts/run.sh
```

Exit from inside the guest:

```sh
echo o > /proc/sysrq-trigger
```

Or kill QEMU with `Ctrl-E X` (the escape prefix is remapped from `Ctrl-A`).

## Writing a Module

Create a directory under `modules/` containing a `Makefile` and your `.c` source.

### Makefile template

```makefile
HERE   := $(abspath $(CURDIR))
KDIR   ?= $(abspath $(HERE)/../../env/linux)
SHARED ?= $(abspath $(HERE)/../../shared)

obj-m += mymod.o

.PHONY: all install clean

all:
	$(MAKE) -C $(KDIR) M=$(HERE) modules

install: all
	@mkdir -p $(SHARED)
	cp *.ko $(SHARED)/

clean:
	$(MAKE) -C $(KDIR) M=$(HERE) clean
```

### Development loop

```bash
cd modules/mymod
make install     # build and copy to shared/
```

In the guest:

```sh
/ # insmod /mnt/shared/mymod.ko
/ # dmesg | tail
/ # rmmod mymod
```

### Userspace helpers

Link statically — the BusyBox rootfs has no glibc:

```bash
gcc -static -O2 -Wall test.c -o test
```

### Shared UAPI headers

For headers shared between kernel and userspace, use kernel-portable types:

```c
#include <linux/types.h>     /* not <stdint.h> */
#include <linux/ioctl.h>

struct mymod_data {
    __u32 value;             /* not uint32_t */
};
```

## Debugging

Console output:

```sh
dmesg | tail
echo 8 > /proc/sys/kernel/printk   # raise console log level
```

Attach gdb on the host:

```bash
cd env/linux
gdb vmlinux
(gdb) target remote :1234
```

## Troubleshooting

| Symptom | Fix |
|---|---|
| `bc: not found` during kernel build | `sudo apt install bc` |
| `Module.symvers is missing` | `cd env/linux && make -j$(nproc) modules` |
| Guest reports `./prog: not found` (the file is there) | rebuild the program with `-static` |
| Terminal stuck in raw mode after QEMU exit | `reset` or `stty sane` |
