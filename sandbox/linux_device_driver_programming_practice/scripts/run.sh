#!/usr/bin/env bash
# Launch QEMU with the built kernel + initramfs.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$HERE/scripts/lib.sh"

[ -f "$KERNEL_IMG" ]    || { err "Kernel missing: $KERNEL_IMG"; exit 1; }
[ -f "$INITRAMFS_IMG" ] || { err "initramfs missing: $INITRAMFS_IMG"; exit 1; }

mkdir -p "$SHARED_DIR"

KVM_OPTS=()
if [ -r /dev/kvm ] && [ -w /dev/kvm ]; then
    KVM_OPTS=(-enable-kvm -cpu host)
else
    log "KVM unavailable, falling back to TCG (slow)"
fi

log "Starting QEMU. Exit with Ctrl-A X."
exec qemu-system-x86_64 \
    "${KVM_OPTS[@]}" -smp 2 -m 1G \
    -kernel "$KERNEL_IMG" \
    -initrd "$INITRAMFS_IMG" \
    -append "console=ttyS0 nokaslr loglevel=7" \
    -netdev user,id=n0,hostfwd=tcp::2222-:22 \
    -device virtio-net-pci,netdev=n0 \
    -virtfs local,path="$SHARED_DIR",mount_tag=shared,security_model=none,id=shared \
    -nographic \
    -s \
    "$@"
