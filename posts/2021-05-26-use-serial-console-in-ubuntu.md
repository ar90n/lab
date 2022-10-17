---
toc: true
layout: post
date: 2021-05-26
description: Use serial console in Ubuntu 20.04
categories: [Ubuntu]
title: Ubuntu 20.04をシリアルコンソール経由で操作する
---

## はじめに
普段SSHで管理しているサーバーが，ネットワーク関連のトラブルに見舞われると，いつも通りにログインできず色々と大変です．
そこで，Ubuntu 20.04をシリアルコンソール経由で操作する環境を構築します．

## やったこと
* USB-TTLケーブルによる代用シリアルケーブルの作成
* udevによるケーブル挿入をトリガーとしたシリアルコンソールの有効化

## ケーブル作成
特殊なケースを除くと，最近のPCにはシリアルポートが付属していません．
そこで，２本のUSB-TTL変換ケーブルを直結することでシリアルケーブルの代用とします．
今回，安価に購入可能な[こちらのケーブル](https://www.amazon.co.jp/gp/product/B00K7YYFNM/)を使用しました．
代用ケーブルの作成は，両ケーブルのピンソケットを切断し，以下の様に緑と白を交差して結線します．

```
<赤> ---------------- <赤>
<緑> -------\/------- <緑>
<白> -------/\------- <白>
<黒> ---------------- <黒>
```

作成したケーブルの動作確認は両端をUSBポートに挿入し，接続したマシンで以下の様に`cu`コマンドを実行します．
接続が適切に行われると，一方の端末で入力した文字列が他方の端末に表示されると思います． （同一マシンでも可能です）

```bash
$ sudo cu -l /dev/ttyUSB0 # <- ttyUSB0の部分は環境によります
```

## シリアルコンソールの有効化
代用ケーブルを接続したマシンにて，以下のコマンド実行するとシリアルコンソールを有効化することができます．

```
$ sudo systemctl enable serial-getty@ttyUSB0.service  # <- ttyUSB0の部分は環境によります
$ sudo systemctl start serial-getty@ttyUSB0.service  # <- ttyUSB0の部分は環境によります
```

その後，もう一方を接続したマシンにて，`cu`コマンドを用いて接続します．すると，以下の様にログインプロンプトが確認できます．(slimeはホスト名です)

```
$ sudo cu -l /dev/ttyUSB0 # <- ttyUSB0の部分は環境によります
Password:
Connected.

slime login:
```


## udevによるシリアルコンソールの自動起動

常時ケーブルを刺しっぱなしにするのは不便なので，ケーブルを挿入したタイミングで，シリアルコンソールが有効化されるように設定します．
ですが，普通にUSB-TTL変換ケーブルを使いたい時もあるので，単純に`ttyUSB0`を関しするだけでは都合が悪そうです．
そこで，特定のVendor IDとProduct IDを持つUSB-Serialデバイスが接続された場合のみシリアルコンソールを有効化します．
Vendor IDとProduct IDは以下のコマンドで確認します．

```
$ udevadm test-builtin usb_id /sys/class/tty/ttyUSB0
Load module index
Parsed configuration file /usr/lib/systemd/network/99-default.link
Parsed configuration file /usr/lib/systemd/network/73-usb-net-by-mac.link
Created link configuration context.
ID_VENDOR=Prolific_Technology_Inc.
ID_VENDOR_ENC=Prolific\x20Technology\x20Inc.
ID_VENDOR_ID=067b
ID_MODEL=USB-Serial_Controller
ID_MODEL_ENC=USB-Serial\x20Controller
ID_MODEL_ID=2303
ID_REVISION=0300
ID_SERIAL=Prolific_Technology_Inc._USB-Serial_Controller
ID_TYPE=generic
ID_BUS=usb
ID_USB_INTERFACES=:ff0000:
ID_USB_INTERFACE_NUM=00
ID_USB_DRIVER=pl2303
Unload module index
Unloaded link configuration context.
```

`ID_VENDOR_ID=067b` および `ID_MODEL_ID=2303` とあるので，Vendor IDが067bかつProduct IDが2303であることが確認できます．
これは，今回使用するケーブルはPL2303を使っているためです.従って，PL2303が接続されるとシリアルコンソールが有効化されることになります．


以上の結果を元に，以下の様にudevの設定を追加します．

```
$ cat /etc/udev/rules.d/65-serial-console.rules
ACTION=="remove", GOTO="serial_end"
SUBSYSTEM!="tty", GOTO="serial_end"

ENV{ID_VENDOR_ID}=="067b", ENV{ID_MODEL_ID}=="2303", ENV{SYSTEMD_WANTS}+="serial-getty@ttyUSB$env{.ID_PORT}.service"

LABEL="serial_end"
```

1,2行目は，USB-TTLケーブル挿入時以外は処理をスキップすることを表しています．
4行目はVendor IDが067bかつProduct IDが2303である場合，対応するttyUSBに対してserial-gettyサービスを有効化することを表しています．

設定後，以下のコマンドでケーブル挿入の前後で`serial-getty@ttyUSB0.service`が有効化されていることを確認できます．

* ケーブル挿入前

```
$sudo systemctl status serial-getty@ttyUSB0.service
● serial-getty@ttyUSB0.service - Serial Getty on ttyUSB0
     Loaded: loaded (/lib/systemd/system/serial-getty@.service; enabled; vendor preset: enabled)
     Active: inactive (dead) since Sun 2021-05-23 03:26:14 UTC; 2s ago
       Docs: man:agetty(8)
             man:systemd-getty-generator(8)
             http://0pointer.de/blog/projects/serial-console.html
    Process: 3803 ExecStart=/sbin/agetty -o -p -- \u --keep-baud 115200,38400,9600 ttyUSB0 $TERM (code=killed, signal=HUP)
   Main PID: 3803 (code=killed, signal=HUP)

May 23 03:26:14 slime systemd[1]: Stopped Serial Getty on ttyUSB0.
```

* ケーブル挿入後

```
$ sudo systemctl status serial-getty@ttyUSB0.service
● serial-getty@ttyUSB0.service - Serial Getty on ttyUSB0
     Loaded: loaded (/lib/systemd/system/serial-getty@.service; enabled; vendor preset: enabled)
     Active: active (running) since Sun 2021-05-23 03:26:21 UTC; 1s ago
       Docs: man:agetty(8)
             man:systemd-getty-generator(8)
             http://0pointer.de/blog/projects/serial-console.html
   Main PID: 4494 (agetty)
      Tasks: 1 (limit: 76982)
     Memory: 348.0K
     CGroup: /system.slice/system-serial\x2dgetty.slice/serial-getty@ttyUSB0.service
             └─4494 /sbin/agetty -o -p -- \u --keep-baud 115200,38400,9600 ttyUSB0 vt220

May 23 03:26:21 slime systemd[1]: Started Serial Getty on ttyUSB0.
```

## 参考

* [Ubuntu 15.10 でシリアルコンソールを有効にする](https://qiita.com/falcon8823/items/cf6ace48b94946330f24)
* [Ubuntu Weekly Recipe 第555回　いま，あらためてudev](https://gihyo.jp/admin/serial/01/ubuntu-recipe/0555)
* [Raspberry Pi 2 + systemd + udevで、USBデバイス挿入時にサービスを起動する](https://thinkami.hatenablog.com/entry/2015/06/25/064658)
