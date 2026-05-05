#!/usr/bin/env bash
# Install host packages and clone kernel + busybox.
# Idempotent: safe to re-run.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$HERE/scripts/lib.sh"

log "Installing host packages"
sudo apt-get update || log "apt-get update had errors (continuing anyway)"
sudo apt-get install -y \
    build-essential libncurses-dev bison flex libssl-dev libelf-dev \
    bc cpio rsync kmod \
    qemu-system-x86 qemu-utils \
    gdb git wget

mkdir -p "$ENV_DIR" "$SHARED_DIR"

if [ ! -d "$KERNEL_DIR/.git" ]; then
    log "Cloning Linux kernel ($KERNEL_VERSION)"
    git clone --depth 1 --branch "$KERNEL_VERSION" \
        https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git \
        "$KERNEL_DIR"
else
    log "Kernel already cloned, skipping"
fi

if [ ! -d "$BUSYBOX_DIR/.git" ]; then
    log "Cloning BusyBox"
    git clone --depth 1 \
        https://git.busybox.net/busybox \
        "$BUSYBOX_DIR"
else
    log "BusyBox already cloned, skipping"
fi

log "Setup done. Next: ./scripts/build-kernel.sh && ./scripts/build-busybox.sh"
