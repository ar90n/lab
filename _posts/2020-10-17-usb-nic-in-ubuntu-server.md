---
toc: true
layout: post
description: USB-NIC in Ubuntu Server
categories: [DICOM]
title: Ubuntu 20.04 LTS Server でUSB-NICを使う
---

## はじめに
[このUSB-NIC](https://www.amazon.co.jp/gp/product/B0871W1WPJ)をUbuntu 20.04から使用するために行った作業記録です．

## やったこと
* ネットワークデバイスを有効
* netplanで静的IPアドレスを設定

## 現状の確認
USBポートに差し込んだのみでは，ネットワークデバイスとして使用可能な状態にはなりませんでした．
まずは，USBデバイスとして適切に認識されていることを確認します．

```bash
$ lsusb
Bus 002 Device 002: ID 0b95:1790 ASIX Electronics Corp. AX88179 Gigabit Ethernet
Bus 002 Device 001: ID 1d6b:0003 Linux Foundation 3.0 root hub
Bus 001 Device 002: ID 8087:0a2a Intel Corp.
Bus 001 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
```

`ASIX Electronics Corp. AX88179 Gigabit Ethernet` このデバイスが対象のUSB-NICであるようです．

## ネットワークデバイスとして認識する
[参考サイト](https://qiita.com/tackey/items/794ea92e5cb31f9febbc)の方法に従ってネットワークデバイスとして認識させます．

```bash
$ sudo lshw -c Network
...
  *-network:0 DISABLED
       description: Ethernet interface
       physical id: 1
       bus info: usb@2:4
       logical name: enx000ec6853d1a
       serial: 00:0e:c6:85:3d:1a
       size: 10Mbit/s
       capacity: 1Gbit/s
       capabilities: ethernet physical tp mii 10bt 10bt-fd 100bt 100bt-fd 1000bt 1000bt-fd autonegotiation
       configuration: autonegotiation=off broadcast=yes driver=ax88179_178a duplex=half link=no multicast=yes port=MII speed=10Mbit/s
...
$ sudo ifconfig enx000ec6853d1a up
$ ifconfig
...
enx000ec6853d1a: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
        inet6 fe80::20e:c6ff:fe85:3d1a  prefixlen 64  scopeid 0x20<link>
        ether 00:0e:c6:85:3d:1a  txqueuelen 1000  (イーサネット)
        RX packets 39  bytes 3635 (3.6 KB)
        RX errors 0  dropped 32  overruns 0  frame 0
        TX packets 7  bytes 882 (882.0 B)
        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0
...
```

## `/etc/cloud/cloud.cfg.d/50-curtin-networking.cfg`にenx000ec6853d1aの設定を追加

```bash
$ cat /etc/cloud/cloud.cfg.d/50-curtin-networking.cfg
network:
  ethernets:
    enp2s0:
      addresses: []
      dhcp4: true
    enx000ec6853d1a:
      addresses:
      - 10.0.100.1/24
      gateway4: 10.0.0.1
      dhcp4: false
  version: 2
$ sudo cloud-init clean -r
```

ここでシステムが再起動します．その後，以下の様に設定確認します．

```bash
$ ifconfig
...
enx000ec6853d1a: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
        inet 10.0.100.1  netmask 255.255.255.0  broadcast 10.0.100.255
        inet6 fe80::20e:c6ff:fe85:3d1a  prefixlen 64  scopeid 0x20<link>
        ether 00:0e:c6:85:3d:1a  txqueuelen 1000  (イーサネット)
        RX packets 2163  bytes 212391 (212.3 KB)
        RX errors 0  dropped 1734  overruns 0  frame 0
        TX packets 18  bytes 1900 (1.9 KB)
        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0
...
```

## 参考
* [Ubuntu 16.04.5で有線LANが繋がらない](https://qiita.com/tackey/items/794ea92e5cb31f9febbc)