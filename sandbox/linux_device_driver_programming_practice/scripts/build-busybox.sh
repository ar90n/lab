#!/usr/bin/env bash
# Configure (first run only) and build BusyBox statically.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$HERE/scripts/lib.sh"

[ -d "$BUSYBOX_DIR" ] || { err "BusyBox not cloned. Run ./scripts/setup.sh first"; exit 1; }
cd "$BUSYBOX_DIR"

if [ ! -f .config ]; then
    log "Configuring BusyBox (defconfig + static link, tc disabled)"
    yes "" | make defconfig
    sed -i 's/.*CONFIG_STATIC[ =].*/CONFIG_STATIC=y/' .config
    sed -i 's/^CONFIG_TC=y/# CONFIG_TC is not set/' .config
    yes "" | make oldconfig
fi

log "Building BusyBox"
make -j"$(nproc)"

log "Installing to _install/"
make install

log "BusyBox ready: $BUSYBOX_DIR/_install/"
