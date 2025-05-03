#!/bin/bash
# /etc/config.sh - 3-stage init script for direct partitioning and user setup

export DEBIAN_FRONTEND=noninteractive

STAGE_FILE="/etc/config-stage"
ENV="/etc/environment"
[ -f "$ENV" ] && source "$ENV"

DISK="/dev/nvme0n1"
MOUNT_BASE="/mnt/ssd"

# === Stage 1: Partition, format, and copy from SD ===
if [ ! -f "$STAGE_FILE" ]; then
  echo "[Stage 1] Partitioning and copying rootfs..."

  # Disable services
  systemctl mask wpa_supplicant.service
  systemctl stop hostapd.service
  systemctl disable hostapd.service
  systemctl stop bluetooth.target
  systemctl disable bluetooth.target
  systemctl stop wpa_supplicant-nl80211@.service
  systemctl disable wpa_supplicant-nl80211@.service
  systemctl stop wpa_supplicant-wired@.service
  systemctl disable wpa_supplicant-wired@.service
  systemctl stop wpa_supplicant@.service
  systemctl disable wpa_supplicant@.service
  systemctl stop dbus-fi.w1.wpa_supplicant1.service
  systemctl disable dbus-fi.w1.wpa_supplicant1.service
  systemctl stop NetworkManager
  systemctl disable NetworkManager

  # Install packages
  ntpdate pool.ntp.org
  dpkg --configure -a || true
  apt --fix-broken install -y || true
  apt-get update
  apt-get install -y rsync

  # Uninstall packages
  apt-get purge -y x11-common hostapd wpasupplicant bluez pulseaudio

  # Partition the disk (swap, home, usr, var)
  wipefs -a $DISK
  parted -s $DISK mklabel gpt
  parted -s $DISK mkpart primary linux-swap 1MiB 4097MiB
  parted -s $DISK mkpart primary ext4 4097MiB 20481MiB     # home
  parted -s $DISK mkpart primary ext4 20481MiB 36865MiB    # usr
  parted -s $DISK mkpart primary ext4 36865MiB 100%

  # Wait for partitions to settle
  sleep 2
  partprobe
  sleep 1

  mkswap ${DISK}p1
  mkfs.ext4 -F ${DISK}p2  # home
  mkfs.ext4 -F ${DISK}p3  # usr
  mkfs.ext4 -F ${DISK}p4  # var

  mkdir -p $MOUNT_BASE/home $MOUNT_BASE/usr $MOUNT_BASE/var
  mount ${DISK}p2 $MOUNT_BASE/home
  rsync -aAX /home/ $MOUNT_BASE/home/ || exit 0
  umount $MOUNT_BASE/home

  mount ${DISK}p3 $MOUNT_BASE/usr
  rsync -aAX /usr/ $MOUNT_BASE/usr/ || exit 0
  umount $MOUNT_BASE/usr

  mount ${DISK}p4 $MOUNT_BASE/var
  rsync -aAX /var/ $MOUNT_BASE/var/ || exit 0
  umount $MOUNT_BASE/var

  echo "UUID=$(blkid -s UUID -o value ${DISK}p1) none swap sw 0 0" >> /etc/fstab
  echo "UUID=$(blkid -s UUID -o value ${DISK}p2) /home ext4 defaults 0 2" >> /etc/fstab
  echo "UUID=$(blkid -s UUID -o value ${DISK}p3) /usr  ext4 defaults 0 2" >> /etc/fstab
  echo "UUID=$(blkid -s UUID -o value ${DISK}p4) /var  ext4 defaults 0 2" >> /etc/fstab

  echo "stage2" > "$STAGE_FILE"
  sync
  reboot
  exit 0
fi

# === Stage 2: Install base packages and create user ===
if grep -q "stage2" "$STAGE_FILE"; then
  echo "[Stage 2] Installing base packages and creating user..."

  apt install -y avahi-daemon avahi-utils zsh chrony curl gnupg ca-certificates lsb-release
  systemctl enable avahi-daemon
  systemctl disable avahi-daemon.socket
  cp /lib/systemd/system/avahi-daemon.service /etc/systemd/system/avahi-daemon.service

  if [ -n "$USERNAME" ] && [ -n "$GROUPNAME" ] && [ -n "$USER_ID" ] && [ -n "$GROUP_ID" ]; then
    groupadd -g "$GROUP_ID" "$GROUPNAME"
    useradd -m -u "$USER_ID" -g "$GROUP_ID" -s /usr/bin/zsh "$USERNAME"
    echo "$USERNAME:$USERNAME" | chpasswd
    usermod -aG sudo "$USERNAME"

    mkdir -p "/home/$USERNAME/.ssh"
    if [ -n "$GITHUB_ID" ]; then
      curl -sL "https://github.com/$GITHUB_ID.keys" > "/home/$USERNAME/.ssh/authorized_keys"
      chown "$USERNAME:$GROUPNAME" "/home/$USERNAME/.ssh/authorized_keys"
      chmod 600 "/home/$USERNAME/.ssh/authorized_keys"
    fi
    chown -R "$USERNAME:$GROUPNAME" "/home/$USERNAME/.ssh"
  fi

  userdel -r guest

  timedatectl set-timezone Asia/Tokyo

  echo "stage3" > "$STAGE_FILE"
  reboot
  exit 0
fi

# === Stage 3: Install Docker CE and finalize user setup ===
if grep -q "stage3" "$STAGE_FILE"; then
  echo "[Stage 3] Installing Docker CE..."

  install -m 0755 -d /etc/apt/keyrings
  curl -fsSL https://download.docker.com/linux/debian/gpg | gpg --dearmor -o /etc/apt/keyrings/docker.gpg
  chmod a+r /etc/apt/keyrings/docker.gpg

  echo \
    "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/debian \
    $(lsb_release -cs) stable" | \
    tee /etc/apt/sources.list.d/docker.list > /dev/null

  apt update
  apt install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
  systemctl enable docker

  if [ -n "$USERNAME" ]; then
    usermod -aG docker "$USERNAME"
  fi

  echo "cleanup" > "$STAGE_FILE"
  reboot
  exit 0
fi

# === Cleanup state: remove self and rc.local entry ===
if grep -q "cleanup" "$STAGE_FILE"; then
  echo "[Stage Cleanup] Removing config.sh and rc.local entry"
  rm -f /etc/config.sh
  sed -i '/\/etc\/config.sh/d' /etc/rc.local
  rm -f "$STAGE_FILE"
  exit 0
fi
