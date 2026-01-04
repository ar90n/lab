---
title: EasyThreed K10にネットワーク経由でファイルを送信する
date: 2026-1-4
categories: [組み込み,ESP32,3Dプリンタ]
image: /assets/img/2026-01-04-esp32-as-sd-card-gateway/thumb.jpg
format:
  html:
    code-fold: false
toc: true
layout: post
code-annotations: below
nocite: |
  @ESPWevDAV
---

## はじめに
EasyThreed K10という非常に安価な3Dプリンタがあります。本機はマザーボードの機能が限定的であり、ネットワーク経由で制御することができません。そのため、microSDカードを都度抜き差しする運用が必要でした。そこで、microSDカードにネットワーク経由でファイルを書き込むことで運用の簡易化を試みます。

## やったこと

* XIAO ESP32C3を使用してHTTP PUTでmicroSDにファイルを書き込み
* microSD スニッファーボードを使用してmicroSDを共有
* EasyThreed K10に対してネットワーク経由でgcodeファイルを送信

## ハードウェアの準備
microSDを制御するマイコンにはXIAO ESP32C3を、ターゲットデバイスのmicroSDソケットへのアクセスには[microSD スニッファーボード](https://www.google.com/search?q=microSD+%E3%82%B9%E3%83%8B%E3%83%83%E3%83%95%E3%82%A1%E3%83%BC%E3%83%9C%E3%83%BC%E3%83%89)を使用しました。

### microSD スニッファーボードのパターンカットと抵抗の挿入
ターゲットデバイスとのmicroSDの共有方法はESPWebDAVを参考にします。これは、`CS`、`MOSI`、`SCK`に抵抗を挿入するというものです。これにより、デバイス側の状態によらず、これらのピンの制御が可能となるはずです。

しかしながら、microSD スニッファーボードにはこれらの抵抗を挿入するパターンがありません。したがって、以下のようにレジストの剥離とパターンのカットを行い、そこに抵抗を挿入します。レジストの剥離は超音波カッターにて該当箇所をなぞると簡単にできます。

![](/assets/img/2026-01-04-esp32-as-sd-card-gateway/sniffer_board.jpg)

### XIAO ESP32C3の接続
以下に示すようにmicroSD スニッファーボードと接続を行いました。

![](/assets/img/2026-01-04-esp32-as-sd-card-gateway/schematics.png)

また、ESPWebDAVにある回路と異なる点は以下の通りです。

* 空いているピンにステータス表示用のLEDを追加
* microSDリセット用のMOS FETを追加

なお、後者のmicroSDリセット用MOS FETは想定通りに機能しなかったため、本実装では使用していません。

## ソフトウェアの準備
PlatformIOを用いて環境構築を行いました。環境一式は以下の通りです。

[GitHubリポジトリ](https://github.com/ar90n/lab/tree/main/sandbox/esp_put_store)

`secrets.h.template`を修正して`secrets.h`を作成し、以下のコマンドでファームウェアを書き込みます。

```bash
$ pio run -t upload
```

### HTTP PUTによるファイルの書き込み
上述のハードウェアとソフトウェアの準備が完了したら、`curl`を用いてファイルをPUTすると以下のような結果が得られます。ここでは、`10.0.0.24`がESP32-C3のIPアドレスとして割り当てられているものとします。

```bash
❯ curl -v --http1.1 -T test6.gcode http://10.0.0.24/upload
*   Trying 10.0.0.24:80...
* Connected to 10.0.0.24 (10.0.0.24) port 80
> PUT /upload HTTP/1.1
> Host: 10.0.0.24
> User-Agent: curl/8.7.1
> Accept: */*
> Content-Length: 149446
>
* upload completely sent off: 149446 bytes
< HTTP/1.1 200 OK
< Connection: close
< Content-Type: text/plain
< Content-Length: 22
<
OK saved /model.gcode
* Closing connection
```

### WebDAVの検討と諦め
当初、ESPWebDAVをそのまま使用することを想定していました。しかしながら、私の環境ではファイルサイズが1MB程度になるとアップロードに失敗しました。したがって、WebDAVの使用を諦めました。

## 動作確認
ESP32-C3からはSPIモードでmicroSDにファイルを書き込みますが、EasyThreed K10はネイティブモードでmicroSDからファイルを読み込みます。したがって、ファイルのアップロードが完了したら、プリント開始前にmicroSDの電源（EasyThreed K10の電源）をOFFにする必要があります。

以下はペンプロッタとして動かした例になります。`curl`にてgcodeを送信したのちにスタートボタンを押下しました。ペンプロッタ用の治具は[こちら](https://www.printables.com/model/1285208-easythreed-k10-cute-pen-plotter-head-mod)からお借りしました。

{{< video /assets/video/2026-01-04-esp32-as-sd-card-gateway/k10.mp4 >}}


## 参考
::: {#refs}
:::