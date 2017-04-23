---
title: Rancherを導入してプライベートDockerレジストリを建てる
date: "2017-04-20T15:36:21Z"
layout: post
path: "/blog/2017-04-20T15:36:21Z/"
category:
- "Rancher"
- "Docker"
description: "Rancherを導入してプライベートDockerレジストリを建てた作業メモ．"
---
直接RancherOSのコマンドを叩くのはしんどくなってきたので、Rancherを導入しました．
あと，プライベートDockerレジストリも建ててみました．

### やったこと
* ローカルのDockerにRancher(Server)を導入
* Conoha上のRancherOSにRancher(Client)を導入
* Rancher ComposeにてプライベートDockerレジストリを建てる

### Rancher(Server)の導入
RancherとはサーバーとクライアントからなるDockerの管理・運用を支援するツールです．
名前は似ていますが，以前言及したRancherOSとは別物です．

今回はローカルのDockerに以下のようにコンテナを起動させました．
```
$docker run --name rancher-server -d --restart=always -p 8888:8080 rancher/server
```

### Rancher(Client)の導入
今回はConohaのインスタンスにRancher(Client)を導入します．
従って，まずは以下のようにインスタンスの所定のポートへのアクセスを許可します．

今回は簡単に設定するため[conoha-net](https://github.com/hironobu-s/conoha-net)を使用しました．
```
$conoha-net create-group docker-registry
$conoha-net create-rule -d ingress -e IPv4 -p "5000" -P tcp docker-registry
$conoha-net create-rule -d ingress -e IPv4 -p "5001" -P tcp docker-registry
$conoha-net attach -n instance_name docker-registry
$conoha-net create-group rancher-client
$conoha-net create-rule -d ingress -e IPv4 -p 500 -P udp rancher-client
$conoha-net create-rule -d ingress -e IPv4 -p 4500 -P udp rancher-client
$conoha-net attach -n instance_name reancher-client
```

次にRancherにアクセスし、 INFRASTRUCTURE -> Hosts -> Add Host から所定の情報を入力する.  
これによって，Rancher(Client)を導入するコマンドが生成される.

### プライベートDockerレジストリの構築
プレイベートDockerレジストリは[こちら](https://github.com/monami-ya/docker-registry-auth)の構成を参考に， dockerauth-config(データコンテナ)，dockerauth(認証サーバー)，registr(Dockerレジストリです)という３つのコンテナを立ち上げる事にしました．  
Dockerfileとdocker-compose.ymlは以下ような感じです．
```
FROM ubuntu:latest
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

ENV SUBJECT "/C=/ST=/L=/O=/CN=common_name"

WORKDIR /

RUN apt-get update && \
    apt-get install -y openssl && \
    mkdir ssl && \
    openssl genrsa -out ssl/server.key 2048 && \
    openssl req -new -newkey rsa:4096 -days 3650 -nodes -subj $SUBJECT -keyout ssl/server.key -out ssl/server.csr && \
    /usr/bin/openssl x509 -req -days 365 -in ssl/server.csr -signkey ssl/server.key -out ssl/server.crt && \
    openssl x509 -outform PEM -in ssl/server.crt -out ssl/server.pem -text && \
    apt-get autoremove -y openssl && \
    apt-get clean
VOLUME /ssl

RUN mkdir /config && \
    echo "\
server:                                                                         \n\
  addr: ':5001'                                                                 \n\
  certificate: '/ssl/server.pem'                                                \n\
  key: '/ssl/server.key'                                                        \n\
token:                                                                          \n\
  issuer: 'Auth Service'                                                        \n\
  expiration: 900                                                               \n\
users:                                                                          \n\
  'admin':                                                                      \n\
    password: '\$2y\$05\$kmGJGLjcRNw7Y9lNhqxKUeiiGzvvljMQ8gTNM9xLfAQdrFMfF3pli' \n\
acl:                                                                            \n\
  - match: {account: 'admin'}                                                   \n\
    actions: ['*']                                                              \n\
comment: 'Admin has full access to everything.'" > /config/auth_config.yml
    VOLUME /config

    RUN mkdir /logs
    VOLUME /logs

    VOLUME /var/lib/registry
```
```
version: '2'

services:
  dockerauth-config:
    build: .
    stdin_open: true
    tty: true
    volumes:
      - /data/private-docker-registry/var/lib/registry:/var/lib/registry

  dockerauth:
    image: cesanta/docker_auth
    ports:
      - "5001:5001"
    volumes_from:
      - dockerauth-config
    command: /config/auth_config.yml
    stdin_open: true
    tty: true
    restart: always

  registry:
    image: registry:latest
    ports:
      - "5000:5000"
    labels:
      io.rancher.sidekicks: dockerauth-config, dockerauth
    volumes_from:
      - dockerauth-config
    stdin_open: true
    tty: true
    restart: always
    environment:
      - REGISTRY_STORAGE_FILESYSTEM_ROOTDIRECTORY=/var/lib/registry
      - REGISTRY_AUTH=token
      - REGISTRY_AUTH_TOKEN_REALM=https://path.to.server:5001/auth
      - REGISTRY_AUTH_TOKEN_SERVICE="Docker registry"
      - REGISTRY_AUTH_TOKEN_ISSUER="Auth Service"
      - REGISTRY_AUTH_TOKEN_ROOTCERTBUNDLE=/ssl/server.pem
```

Rancherを使用する場合は，docker-composeではなくrancher-composeを使用します．  
rancher-composeを使用する際は，アクセスキーが必要となります．  
Rancherにアクセスし， API -> Keys -> ADVANCED OPTIONS -> Add Environment API Key から Access Key と Secret Key を作成します.

コンテナのデプロイはdocker-composeと同様に以下のようにコマンドを実行します．
```
$rancher-compose --url rancher_url --access-key environment_access_key --secret-key environment_secret_key up
```
