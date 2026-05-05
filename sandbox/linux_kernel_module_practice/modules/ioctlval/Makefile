# Resolve paths relative to this Makefile
HERE   := $(abspath $(CURDIR))
KDIR   ?= $(abspath $(HERE)/../../env/linux)
SHARED ?= $(abspath $(HERE)/../../shared)

obj-m += ioctlval.o

.PHONY: all install clean

all: module test_ioctlval

test_ioctlval:
	$(CC) -static -Wall -Wextra -O2 -o $@ test_ioctlval.c

module:
	$(MAKE) -C $(KDIR) M=$(HERE) modules

install: module
	@mkdir -p $(SHARED)
	cp *.ko test_ioctlval $(SHARED)/
	@echo "Installed to $(SHARED)"

clean:
	$(MAKE) -C $(KDIR) M=$(HERE) clean
