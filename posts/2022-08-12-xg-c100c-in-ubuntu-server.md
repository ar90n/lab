---
toc: true
layout: post
date: 2022-08-12
categories: [Ubuntu]
title: XG-C100Cを追加してオンボードNICとブリッジ接続する
---

## はじめに
Ubuntu 22.04で運用している開発用マシンに[XG-C100C](https://www.asus.com/jp/Networking-IoT-Servers/Wired-Networking/All-series/XG-C100C/)を追加した記録です。
最終的に以下のような環境を構築します。

```
┌───────────────┐                     ┌──────────────┐              ┌────────────────┐
│               │                     │              │              │                │
│               │ 10.0.0.100/24       │              │ Dynamic      │                │
│  MacBook Pro  ├─────────────────────┤  Dev Server  ├──────────────┤  Home Network  │
│               │                     │              │              │                │
│               │                     │              │              │                │
└───────────────┘                     └──────────────┘              └────────────────┘
```


## やったこと
* Ubuntu22.04にXG-C100Cを導入
* 既存のNIC（オンボード）とXG-C100Cとの間をブリッジ接続

## XG-C100Cの導入
ASUSの[サイト](https://www.asus.com/jp/Networking-IoT-Servers/Wired-Networking/All-series/XG-C100C/HelpDesk_Download/)にあるドライバはバージョンが古い（5.0.3.3）ため私の環境（kernel 5.15.0-43）でコンパイルすることができませんでした。
そこで、 [Marvellのダウンロードページ](https://www.marvell.com/support/downloads.html)からAQC107Cを選択してドライバを取得します。

ダウンロード完了後、以下のように指示通り作業を行うとドライバを導入します。

```bash
$ unzip Marvell_Linux_2.5.5.zip
Archive:  Marvell_Linux_2.5.5.zip
   creating: 05-23-22_Marvell_Linux_2.5.5/
  inflating: 05-23-22_Marvell_Linux_2.5.5/atlantic-2.5.5.0-1.noarch.rpm
 extracting: 05-23-22_Marvell_Linux_2.5.5/atlantic.tar.gz
  inflating: 05-23-22_Marvell_Linux_2.5.5/README.txt
  inflating: 05-23-22_Marvell_Linux_2.5.5/Release_Notes_Linux_2.5.5.txt
$ cd 05-23-22_Marvell_Linux_2.5.5
$ tar -zxf atlantic.tar.gz
$ sudo ./dkms.sh install
```

追加したNICの名称を確認します。正しい方法がわからないので、`dmesg` からそれっぽいものを持ってきます。
今回追加したNICは`enp4s0`という名称が付けられているようです。

```bash
$ sudo dmesg| grep atlantic
[    2.502420] atlantic 0000:04:00.0: enabling device (0000 -> 0002)
[    2.540366] atlantic: Detect ATL2FW 1030012
[    5.859800] atlantic 0000:04:00.0 enp4s0: renamed from eth1
[   17.700034] atlantic 0000:04:00.0 enp4s0: atlantic: link change old 0 new 1000
```

## NIC間のブリッジ接続

'/etc/netplan/00-installer-config.yaml'を修正して既存のNIC(`eno1`)とブリッジ接続します。

```bash
$ cat /etc/netplan/00-installer-config.yaml
# This is the network config written by 'subiquity'
network:
  ethernets:
    eno1: {}
    enp4s0: {}
  bridges:
    br0:
      interfaces:
        - eno1
        - enp4s0
      dhcp4: true
  version: 2
```

MacBook Pro <-> Home Network間の通信を透過的に行うためufwを設定します。

```bash
$ sudo ufw allow from 10.0.0.0/24
```

`/etc/default/ufw ufw.org`も以下のように修正します。

```bash
$ diff /etc/default/ufw ufw.org
19c19
< DEFAULT_FORWARD_POLICY="ACCEPT"
---
> DEFAULT_FORWARD_POLICY="DROP"
```

## 参考

* [Marvell Drivers](https://www.marvell.com/support/downloads.html)
* [Netplan reference](https://netplan.io/reference)
