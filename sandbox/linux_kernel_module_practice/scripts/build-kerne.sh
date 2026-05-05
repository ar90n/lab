#!/usr/bin/env bash
# Configure (first run only) and build the kernel.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$HERE/scripts/lib.sh"

[ -d "$KERNEL_DIR" ] || { err "Kernel not cloned. Run ./scripts/setup.sh first"; exit 1; }
cd "$KERNEL_DIR"

if [ ! -f .config ]; then
    log "Configuring kernel (defconfig + debug/9p options)"
    make defconfig
    ./scripts/config \
        -e DEBUG_KERNEL -e DEBUG_INFO -e DEBUG_INFO_DWARF5 \
        -e GDB_SCRIPTS -e KGDB -e KGDB_SERIAL_CONSOLE \
        -e MAGIC_SYSRQ -e FRAME_POINTER \
        -e DEVTMPFS -e DEVTMPFS_MOUNT \
        -e BLK_DEV_INITRD -e RD_GZIP \
        -e NET_9P -e NET_9P_VIRTIO -e 9P_FS \
        -d RANDOMIZE_BASE
    make olddefconfig
fi

log "Building kernel and in-tree modules"
make -j"$(nproc)" bzImage modules

log "Generating compile_commands.json (for clangd)"
make compile_commands.json 2>/dev/null || log "(skipped: not supported on this kernel)"

log "Kernel built: $KERNEL_IMG"
