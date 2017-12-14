---
title: Dockerで開発環境を整備する
date: "2017-12-03T23:33:26Z "
layout: post
path: "/blog/2017-12-03T23:33:26Z/"
category:
- "Docker"
description: "イミュータブルなコンテナをコマンド毎に使い捨てて開発環境を整備しました．"
---
Anacondaを使って開発環境を構築していたのですが，

- AnacondaとHomeBrewの設定が競合する
- システムでつかってるライブラリとバージョン違いを持ってくるとハマる
- python2とpython3の共存が面倒

など，色々と不便な点がありました．なので，今回はDockerを使ってこれらの解決を試みました．  
この作業の結果は，[https://github.com/ar90n/lab/tree/master/docker_settings](https://github.com/ar90n/lab/tree/master/docker_settings)にまとめてあります．

## ベースとなるイメージを準備
まずはベースとなるイメージar90n/dev_envを作成します．ar90n/dev_envは実行ユーザーと必要最低限のパッケージのみを導入します．  
実際に開発で使用するパッケージは，それぞれのプロジェクトでar90n/dev_envを継承したイメージが導入します．  
Dockerfileは以下のような感じ．

```
FROM ubuntu:latest
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

ARG user
ARG uid
ARG group
ARG gid

RUN echo $gid $group
RUN groupadd -g $gid $group
RUN useradd -u $uid -g $group -M $user -s /bin/nologin

RUN ln -s /home /Users

USER root
RUN set -x && \
    apt-get update -qq && \
    apt-get upgrade -qq && \
    apt-get install -y software-properties-common && \
    apt-get install -y language-pack-ja && \
    apt-get install -y build-essential && \
    apt-get install -y curl faketime tzdata && \
    update-locale LANG=ja_JP.UTF-8 && \
    apt-get clean

# opt
RUN set -x && \
    chmod 755 /opt && \
    mkdir /opt/bin && \
    chown -R $user:$group /opt

ENV TZ=Asia/Tokyo
RUN rm /etc/localtime && \
    ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && \
    echo $TZ > /etc/timezone
RUN dpkg-reconfigure --frontend noninteractive tzdata

ADD environment /etc/environment
ENV PATH /opt/bin:$PATH
```

## Minicondaを導入して開発用イメージを作成
ar90n/dir_envを継承してminiconda3を導入したイメージar90n/miniconda3を，さらにそれを継承してtensorflowを導入したイメージar90n/tensorflowを作成します．  
まずはminiconda3の導入

```
FROM ar90n/dev_env
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

USER $USER
WORKDIR /tmp
RUN set -x && \
    curl -O https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh && \
    bash Miniconda3-latest-Linux-x86_64.sh -b -p /opt/miniconda3 && \
    sed  -i -e 's/\(PATH=\)"\(.*\)"/\1"\/opt\/miniconda\/bin\/:\2"/g' /etc/environment && \
    rm  Miniconda3-latest-Linux-x86_64.sh

ENV PATH /opt/miniconda3/bin:$PATH
CMD ["python"]
```

次に，tensorflow

```
FROM ar90n/miniconda3
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

USER $USER
WORKDIR /tmp
RUN set -x && \
    conda install tensorflow
```

そして，以下のようにまとめてビルド．

```
#!/usr/bin/env sh
_USER=${1:-`whoami`}
_UID=`id -u ${USER}`
_GROUP=`id -gn ${USER}`
_GID=`id -g ${USER}`
DOCKER_CMD=${DOCKER_CMD:-'docker'}

${DOCKER_CMD} build --build-arg user=$_USER --build-arg uid=$_UID --build-arg group=$_GROUP --build-arg gid=$_GID -t ar90n/dev_env ./dev_env
${DOCKER_CMD} build --build-arg user=$_USER --build-arg uid=$_UID --build-arg group=$_GROUP --build-arg gid=$_GID -t ar90n/miniconda3 ./miniconda3
${DOCKER_CMD} build --build-arg user=$_USER --build-arg uid=$_UID --build-arg group=$_GROUP --build-arg gid=$_GID -t ar90n/miniconda2 ./miniconda2
${DOCKER_CMD} build --build-arg user=$_USER --build-arg uid=$_UID --build-arg group=$_GROUP --build-arg gid=$_GID -t ar90n/tensorflow ./tensorflow
```

## direnvと実行用スクリプトを設定
このままでは，実行のたびに'docker run ... 'と長いコマンドを打つ必要があるので，実行スクリプトとdirenvの設定を追加します．  
実行スクリプトは以下のような感じで，引数にDockerイメージとコマンドを指定します．

```bash
#!/usr/bin/env sh

_DOCKER=${DOCKER_CMD:-'docker'}
_PWD=`pwd`
_USER=`whoami`
_DATE=`date +"%Y-%m-%d %R:%S"`
_IMAGE=$1

shift
docker run -it --rm -u $_USER -w $_PWD -v $HOME:/home/$_USER $_IMAGE faketime "$_DATE" $@
```

さらに~/.direnvrcに以下の設定を追加します．  
この設定を追加することで，direnvの動作時にエイリアスを作成することができます．

```
alias_dir=$PWD/.direnv/aliases
rm -rf "$alias_dir"

export_alias() {
  local name=$1
  shift
  local target="$alias_dir/$name"
  mkdir -p "$alias_dir"
  PATH_add "$alias_dir"
  echo "#!/usr/bin/env bash" > "$target"
  echo "$@ \"\$@\"" >> "$target"
  chmod +x "$target"
}
```

## 動作確認

```bash
$ cat .envrc
export_alias python "run_in ar90n/tensorflow python"
$ python
Python 3.6.3 |Anaconda, Inc.| (default, Oct 13 2017, 12:02:49)
[GCC 7.2.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> import tensorflow
>>>
```
