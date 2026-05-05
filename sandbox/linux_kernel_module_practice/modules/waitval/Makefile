# Resolve paths relative to this Makefile
HERE   := $(abspath $(CURDIR))
KDIR   ?= $(abspath $(HERE)/../../env/linux)
SHARED ?= $(abspath $(HERE)/../../shared)

obj-m += waitval.o

.PHONY: all install clean

all: module waitval_client

waitval_client:
	$(CC) -static -Wall -Wextra -O2 -o $@ waitval_client.c

module:
	$(MAKE) -C $(KDIR) M=$(HERE) modules

install: module
	@mkdir -p $(SHARED)
	cp *.ko waitval_client $(SHARED)/
	@echo "Installed to $(SHARED)"

clean:
	$(MAKE) -C $(KDIR) M=$(HERE) clean

