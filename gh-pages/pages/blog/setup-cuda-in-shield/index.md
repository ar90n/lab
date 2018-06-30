---
title: SHIELD Android TVでCUDA開発環境を整える
date: "2017-04-23T15:45:04Z"
lang: "ja"
path: "setup-cuda-in-shield"
category:
  - "CUDA"
description: "Amazon.comから買ったSHIELD Android TV(2015) RefurbishedでCUDAの開発環境を整えたメモ．"
cover: "development"
---
GPGPU環境はCUDA一択な感じがしてきたので，CUDAの勉強するためにも開発環境を整えることとしました．  
もちろん，TITAN-Xなんて購入する余裕がないので，とにかく安価に済ませることを目標とします．

### やったこと
* こちらの[参考サイト](http://musyoku.github.io/2016/08/12/Jetson-TX1%E3%81%AE%E4%BB%A3%E3%82%8F%E3%82%8A%E3%81%ABSHIELD-Android-TV%E3%81%A7Tegra-X1%E9%96%8B%E7%99%BA%E7%92%B0%E5%A2%83%E3%82%92%E6%A7%8B%E7%AF%89%E3%81%99%E3%82%8B/)の方法を元にCUDA環境を構築
* CUDA Samplesからマージソートのサンプルプログラムのビルドと実行を確認

### ルートファイルシステムの構築
[参考サイト](http://musyoku.github.io/2016/08/12/Jetson-TX1%E3%81%AE%E4%BB%A3%E3%82%8F%E3%82%8A%E3%81%ABSHIELD-Android-TV%E3%81%A7Tegra-X1%E9%96%8B%E7%99%BA%E7%92%B0%E5%A2%83%E3%82%92%E6%A7%8B%E7%AF%89%E3%81%99%E3%82%8B/)ではmicroSDに対してルートファイルシステムを構築していますが，手元にあるSATVではmicroSDを認識してくれなかったため，USBドライブに構築します．  
各コマンドが何を表しているかは[参考サイト](http://musyoku.github.io/2016/08/12/Jetson-TX1%E3%81%AE%E4%BB%A3%E3%82%8F%E3%82%8A%E3%81%ABSHIELD-Android-TV%E3%81%A7Tegra-X1%E9%96%8B%E7%99%BA%E7%92%B0%E5%A2%83%E3%82%92%E6%A7%8B%E7%AF%89%E3%81%99%E3%82%8B/)を参照してください．
```bash
$ parted -s -a optimal /dev/sda mklabel msdos
$ parted -s -a optimal /dev/sda -- mkpart primary ext4 1 -1
$ mkfs.ext4 /dev/sda1
$ tar -jxvf Tegra210_Linux_R24.2.1_aarch64.tbz2
$ cd Linux_for_Tegra
$ mount /dev/sda1 ./rootfs
$ tar -jxpf Tegra_Linux_Sample-Root-Filesystem_R24.2.1_aarch64.tbz2
$ ./apply_binaries.sh
$ sync
$ umount /dev/sda1
```

### ブートローダの書き込み
ブートローダの書き込みに関する作業はfast bootモードで起動して行います．fast bootモードの起動はadbから行う方法と電源ボタンから行う方法とがあります．詳細については，こちらも[参考サイト](http://musyoku.github.io/2016/08/12/Jetson-TX1%E3%81%AE%E4%BB%A3%E3%82%8F%E3%82%8A%E3%81%ABSHIELD-Android-TV%E3%81%A7Tegra-X1%E9%96%8B%E7%99%BA%E7%92%B0%E5%A2%83%E3%82%92%E6%A7%8B%E7%AF%89%E3%81%99%E3%82%8B/)を参照してください．また，電源ボタンを用いた起動についてはデバイスによってタイミングなどを異なるようです．（ロットやファームウェア(？)によるのでしょうか？）  
手元にあるSATVでは以下のような手順で電源ボタンを操作するとfastbootモードを起動することができました.
1. 電源ケーブルを接続．この時点では電源ボタンを触れてはいない．
2. 本体LED（緑色）が点灯後，１秒程度待ってから電源ボタンに触れる．
3. 電源ボタンを触れたままfastbootモードが起動するのを待つ．

ブートローダの書き込み際して，事前にSATVのブートローダをアンロックしておく必要があります．  アンロックは，ホストとSATVをOTGケーブルで接続の後に以下のコマンドを実行します．
```bash
$ fastboot oem unlock
```

ブートローダの書き込み手順は，バージョン24.2.1のシステムを用いるため，[参考サイト](http://musyoku.github.io/2016/08/12/Jetson-TX1%E3%81%AE%E4%BB%A3%E3%82%8F%E3%82%8A%E3%81%ABSHIELD-Android-TV%E3%81%A7Tegra-X1%E9%96%8B%E7%99%BA%E7%92%B0%E5%A2%83%E3%82%92%E6%A7%8B%E7%AF%89%E3%81%99%E3%82%8B/)と若干異なります．
[bootimages.zip](https://drive.google.com/file/d/0Bz5kaPQJx_AgZ3lvWWZFNmJFcmM/view)を取得後，以下のコマンドを実行します．
```bash
$ unzip bootimages.zip
$ cd bootimages
$ fastboot flash boot sda1.img
$ fastboot flash path/to/Linux_for_Tegra/kernel/dtb/tegra210-foster-e-p2530-0930-e02-00.dtb
```

### JetPackの導入
SATVへJetPackを導入するためには，Ubuntuホストから[JetPack-L4T-3.0-linux-x64.run]()を起動し，必要なファイルを転送する必要があります．  
今回，Ubuntuがインストール済みのコンピュータを所持していないためDockerを用いてこの作業を行いました．
```bash
$ docker pull dorowu/ubuntu-desktop-lxde-vnc
$ docker run -it --rm -p 6080:80 dorowu/ubuntu-desktop-lxde-vnc
```
そして，ブラウザでhttp://localhost:6080に接続することでコンテナをVNC経由で操作可能となります．  
VNC内のターミナルで以下のコマンドを実行しJetPackのインストーラーを起動しました．
```bash
$ apt-get update
$ apt-get -y install sshpass xterm
$ ./JetPack-L4T-3.0-linux-x64.run
```

![jetpack](setup-cuda-in-shield/jetpack.png)


以降，[参考サイト](http://musyoku.github.io/2016/08/12/Jetson-TX1%E3%81%AE%E4%BB%A3%E3%82%8F%E3%82%8A%E3%81%ABSHIELD-Android-TV%E3%81%A7Tegra-X1%E9%96%8B%E7%99%BA%E7%92%B0%E5%A2%83%E3%82%92%E6%A7%8B%E7%AF%89%E3%81%99%E3%82%8B/)を参照のこと．

### ユーザの追加
```bash
$ sudo adduser argon
$ sudo addgroup argon sudo
$ sudo addgroup argon video
```

### サンプルプログラムのビルド
以下のサンプルをビルドして実行しました．
```bash
$ make
/usr/local/cuda-8.0/bin/nvcc -ccbin g++ -I../../common/inc  -m64    -gencode arch=compute_53,code=sm_53 -gencode arch=compute_53,code=compute_53 -o bitonic.o -c bitonic.cu
/usr/local/cuda-8.0/bin/nvcc -ccbin g++ -I../../common/inc  -m64    -gencode arch=compute_53,code=sm_53 -gencode arch=compute_53,code=compute_53 -o main.o -c main.cpp
/usr/local/cuda-8.0/bin/nvcc -ccbin g++ -I../../common/inc  -m64    -gencode arch=compute_53,code=sm_53 -gencode arch=compute_53,code=compute_53 -o mergeSort.o -c mergeSort.cu
/usr/local/cuda-8.0/bin/nvcc -ccbin g++ -I../../common/inc  -m64    -gencode arch=compute_53,code=sm_53 -gencode arch=compute_53,code=compute_53 -o mergeSort_host.o -c mergeSort_host.cpp
/usr/local/cuda-8.0/bin/nvcc -ccbin g++ -I../../common/inc  -m64    -gencode arch=compute_53,code=sm_53 -gencode arch=compute_53,code=compute_53 -o mergeSort_validate.o -c mergeSort_validate.cpp
/usr/local/cuda-8.0/bin/nvcc -ccbin g++   -m64      -gencode arch=compute_53,code=sm_53 -gencode arch=compute_53,code=compute_53 -o mergeSort bitonic.o main.o mergeSort.o mergeSort_host.o mergeSort_validate.o
mkdir -p ../../bin/aarch64/linux/release
cp mergeSort ../../bin/aarch64/linux/release
ubuntu@tegra-ubuntu:~/samples/6_Advanced/mergeSort$ ls
Makefile  NsightEclipse.xml  bitonic.cu  bitonic.o  main.cpp  main.o  mergeSort  mergeSort.cu  mergeSort.o  mergeSort_common.h  mergeSort_host.cpp  mergeSort_host.o  mergeSort_validate.cpp  mergeSort_validate.o  readme.txt
ubuntu@tegra-ubuntu:~/samples/6_Advanced/mergeSort$ ./mergeSort
./mergeSort Starting...

GPU Device 0: "NVIDIA Tegra X1" with compute capability 5.3

Allocating and initializing host arrays...

Allocating and initializing CUDA arrays...

Initializing GPU merge sort...
Running GPU merge sort...
Time: 167.216995 ms
Reading back GPU merge sort results...
Inspecting the results...
...inspecting keys array: OK
...inspecting keys and values array: OK
...stability property: stable!
Shutting down...
```
