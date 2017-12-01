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
