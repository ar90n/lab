---
title: RancherOSをConohaのVPSに導入する 
aate: "2017-01-13T16:17:16.915Z"
layout: post
path: "/blog/2017-01-13T16:17:16.915Z/"
category: "Conoha"
description: "RancherOSをConohaに導入してDcoker環境を作ってみました．"
---

色々とサービスを開発しようと思うと，簡単なホスティング環境が欲しくなります．
AWSやGCPなどでも良いのですが，経済的事情により１円でも安く運用したいので，Conoha(VPS)とRancherOSで構築してみました．

### RancherOSとは？

[RancherOS](http://rancher.com/rancher-os/)とは，Docker向けに最適化された軽量Linuxの一種です．
同種のものにはCoreOSやCentOS Atomic Hostなどがあります．

RancherOSの特徴の一つに，全ての機能がDockerコンテナとして提供されていることがあげられます．
これは，以下の図^[http://rancher.com/rancher-os/]のようにカーネルの上にDocker(System Docker)プロセスがあり，その上にシステムが提供する機能が
コンテナとして存在しています．ユーザアプリケーションは前述のDocker(System Docker)上に存在するDocker(User Docker)上動作します．

![rancheros-container](./rancheros_container.png)

### Conohaにインストール

ConohaのVPSの設定は基本的に提供されているAPIを呼び出すことで行ないます．
APIを呼び出すために必要な設定については[こちら](https://www.conoha.jp/guide/apitokens.php)を参考にしました．

ConohaのVPSに任意のOSを導入するためには，ISOイメージをダウンロードしてVMのドライブに挿入する必要があります．
しかしながら，この作業が中々のハマリ所で，ISOイメージが配置されているディレクトリがhttp接続でリスト表示される必要があります． ^[http://qiita.com/marukei/items/6b6ccee3e7a553f64f1e]

RancherOSのISOイメージはこの条件を満たしていないため，直接ConohaのVPSにダウンロードすることはできませんでした．
そこで，所望のISOイメージをS3にアップロードしそちらからダウンロードすることとしました．

```bash
$ cat > index.html
<html>
<a href="https://s3-ap-northeast-1.amazonaws.com/${BUCKET_NAME}/rancheros.iso">rancheros.iso</a>
</html>
$ aws s3 mb s3://${BUCKET_NAME}
$ aws s3 website s3://${BUCKET_NAME}  --index-document index.html
$ aws s3 cp index.html s3://${BUCKET_NAME}/ --acl public-read
$ aws s3 cp rancheros.iso s3://${BUCKET_NAME}/ --acl public-read
$ conoha-iso download -i http://${BUCKET_NAME}.s3-website-ap-northeast-1.amazonaws.com/rancheros.iso
$ conoha-iso insert
1
1
```

ConohaのWebコンソールよりサーバーを起動し，以下の手順でOSのセットアップを行ないます．

```bash
$ cat > cloud-config.yml
cloud-config

ssh_authorized_keys:
  - ssh-rsa AAAAB...
$ sudo fdisk /dev/vda
p
d
2
$ mkfs.ext4 -L RANCHER_STATE /dev/vda
$ sudo ros install -c cloud-config.yml -d /dev/vda
```
### 動作確認用コンテナをデプロイ

動作確認として，[nginx-proxy](https://github.com/jwilder/nginx-proxy)のサンプルを動作させてみる．
RancherOSはDocker composeのV2フォーマットには対応していないため，以下のように一部をコメントアウトした．

```yml
#version: '2'
#services:
  nginx-proxy:
    image: jwilder/nginx-proxy
    container_name: nginx-proxy
    ports:
      - "80:80"
    volumes:
      - /var/run/docker.sock:/tmp/docker.sock:ro

  whoami:
    image: jwilder/whoami
    container_name: whoami
    environment:
      - VIRTUAL_HOST=whoami.local
```

以下のコマンドでコンテナを立ち上げる．

```bash
local$ ssh rancher@${CONOHA_VPS_IP} -L 8080:localhost:8080
rancher$ sudo ros s enable https://gist.githubusercontent.com/ar90n/10bb4b07d39495bc8c235301da207834/raw/3bea3cf17a893d8a1da63fe4bfbb30dcd919dde2/docker-compose.yml
rancher$ sudo ros s up -d nginx-proxy
rancher$ sudo ros s up -d whoami
```

curlを使ってhttp接続を行う

```bash
rancher$ logout
local$ curl -H "Host: whoami.local" ${CONOHA_VPS_IP}
I'm 777f4ae5fa4e
```
