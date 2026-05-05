# Resolve paths relative to this Makefile
HERE   := $(abspath $(CURDIR))
KDIR   ?= $(abspath $(HERE)/../../env/linux)
SHARED ?= $(abspath $(HERE)/../../shared)

obj-m += procval.o

.PHONY: all install clean

all:
	$(MAKE) -C $(KDIR) M=$(HERE) modules

install: all
	@mkdir -p $(SHARED)
	cp *.ko $(SHARED)/
	@echo "Installed to $(SHARED)"

clean:
	$(MAKE) -C $(KDIR) M=$(HERE) clean
