#!/usr/bin/env bash
# Common variables. Sourced by other scripts.
# HERE must be set by the calling script before sourcing this file.

ENV_DIR="$HERE/env"
KERNEL_DIR="$ENV_DIR/linux"
BUSYBOX_DIR="$ENV_DIR/busybox"
INITRAMFS_DIR="$HERE/initramfs"
SHARED_DIR="$HERE/shared"
INITRAMFS_IMG="$HERE/initramfs.cpio.gz"
KERNEL_IMG="$KERNEL_DIR/arch/x86/boot/bzImage"

KERNEL_VERSION="${KERNEL_VERSION:-v6.6}"

log() { printf "\033[1;32m==>\033[0m %s\n" "$*" >&2; }
err() { printf "\033[1;31m!!\033[0m %s\n" "$*" >&2; }
