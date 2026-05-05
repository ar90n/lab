#!/usr/bin/env bash
# Assemble initramfs/ from busybox + tracked files, pack as cpio.gz.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$HERE/scripts/lib.sh"

[ -d "$BUSYBOX_DIR/_install" ] || { err "BusyBox not built. Run ./scripts/build-busybox.sh"; exit 1; }
[ -x "$INITRAMFS_DIR/init" ]   || { err "initramfs/init missing or not executable"; exit 1; }

log "Syncing BusyBox files into $INITRAMFS_DIR"
# busyboxが生成するbin/sbin/usr/linuxrcだけ上書き
# init や etc/ などgit管理してるファイルは触らない
rsync -a "$BUSYBOX_DIR/_install/bin/"  "$INITRAMFS_DIR/bin/"
rsync -a "$BUSYBOX_DIR/_install/sbin/" "$INITRAMFS_DIR/sbin/"
rsync -a "$BUSYBOX_DIR/_install/usr/"  "$INITRAMFS_DIR/usr/"
[ -e "$BUSYBOX_DIR/_install/linuxrc" ] && cp -a "$BUSYBOX_DIR/_install/linuxrc" "$INITRAMFS_DIR/"

log "Ensuring mount points"
mkdir -p "$INITRAMFS_DIR"/{dev,proc,sys,tmp,root,mnt/shared}

log "Packing $INITRAMFS_IMG"
( cd "$INITRAMFS_DIR" && find . -print0 | cpio --null -o --format=newc 2>/dev/null ) \
    | gzip -9 > "$INITRAMFS_IMG"

SIZE=$(ls -lh "$INITRAMFS_IMG" | awk '{print $5}')
log "Done: $INITRAMFS_IMG ($SIZE)"