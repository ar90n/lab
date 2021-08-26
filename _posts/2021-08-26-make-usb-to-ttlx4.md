---
toc: true
layout: post
description: make USB to TTLx4 
categories: [hardware]
title: USB to TTLx4ケーブルの作成
---

## はじめに
USBハブとUSBシリアルコンバータとを組み合わせることで，USB(1ch) to TTL(4ch) の変換ケーブルを作成しました．

## やったこと
* [type-cコネクタ](https://www.aitendo.com/product/18898)，[USBハブコントローラ基板](https://www.aitendo.com/product/20012)，[USBシリアルコンバータ](https://www.aitendo.com/product/17408)を組み合わせてUSB(1ch) to TTL(4ch)変換ケーブルを作成

## 回路設計
特別なことは特に何もしていません．[CH330のデータシート](http://aitendo3.sakura.ne.jp/aitendo_data/product_img/ic/inteface/CH330N/CH330N.PDF)に従って単純に部品を繋いで行きました．type-cコネクタは`VCC`端子より`5v`を取得するため， `cc`を`5.1kΩ`でプルダウンしています．

今回，[EasyEDA](`https://easyeda.com/`)を使って回路図を起こしてみたのですが中々使い勝手がよかったです．ブラウザ内で全てが完結するので環境構築が非常に簡単です．私は使用しませんでしたがPCBエディタとの連携機能もあるようです．また，各種ボード(`USB1`と`HUB1`)とケーブル(`TTL1`から`TTL4`)のピン番号はシステムの都合上割り当てています．そのため，実際にはピン番号はありません．何か良い方法があると良いのですが．

![]({{site.baseurl}}/assets/img/2021-08-28-make-usb-to-ttlx4/media/Schematic_USB_to_TTL4_2021-08-25.png)

## 実装
可能な限りケーブルの様に使いたかったので，USBハブコントローラ背面にカプトンテープを貼り，その上にUSBシリアルコンバータを実装しました．

![]({{site.baseurl}}/assets/img/2021-08-28-make-usb-to-ttlx4/media/dev1.jpg)

次にケーブルを取り付け全体をグルーガンで固定します．

![]({{site.baseurl}}/assets/img/2021-08-28-make-usb-to-ttlx4/media/dev2.jpg)

最後に熱収縮ケーブルで全体をさらに固定して完成です．

![]({{site.baseurl}}/assets/img/2021-08-28-make-usb-to-ttlx4/media/dev3.jpg)

## 動作確認
`lsusb`コマンドを実行すると以下のデバイスを確認できました．意図した通りに動作しているようです．


```
Bus 020 Device 018: ID 1a40:0101 TERMINUS TECHNOLOGY INC. USB 2.0 Hub
Bus 020 Device 021: ID 1a86:7523 1a86 USB2.0-Serial
Bus 020 Device 029: ID 1a86:7523 1a86 USB2.0-Serial
Bus 020 Device 024: ID 1a86:7523 1a86 USB2.0-Serial
Bus 020 Device 010: ID 1a86:7523 1a86 USB2.0-Serial
```

## 後から気が付いたこと
今回はUSBハブとUSBシリアルコンバータとを組み合わせて所望の機能を実現しました．しかしながら，調べてみると同様の機能をワンチップで実現する[IC](https://jp.silabs.com/interface/usb-bridges/classic/device.cp2108)があるようです．次に同様のものを作る時はこちらを使いたいですね．


## 参考
* [USB 转串口芯片 CH330 ](http://aitendo3.sakura.ne.jp/aitendo_data/product_img/ic/inteface/CH330N/CH330N.PDF)
* [組込み技術ラボ](https://emb.macnica.co.jp/articles/8968/)
