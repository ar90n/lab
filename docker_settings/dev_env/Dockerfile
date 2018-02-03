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
    apt-get install -y sudo git curl faketime tzdata && \
    echo "$user ALL=NOPASSWD: ALL" > /etc/sudoers.d/$user && \
    update-locale LANG=C.UTF-8 && \
    apt-get clean
ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

# neovim
ENV XDG_CONFIG_HOME /opt/
ENV XDG_DATA_HOME /opt/
ADD init.vim /opt/nvim/init.vim
ADD dein.toml /opt/nvim/dein.toml
ADD template /opt/nvim/template/
RUN set -x && \
    add-apt-repository ppa:neovim-ppa/unstable && \
    apt-get update && \
    apt-get -y install neovim && \
    apt-get -y install python-dev python-pip python3-dev python3-pip && \
    apt-get clean && \
    pip2 install --upgrade neovim && \
    pip3 install --upgrade neovim && \
    mkdir /opt/nvim/specs && \
    nvim -c 'call dein#install()' -c 'q' && \
    nvim -c 'UpdateRemotePlugins' -c 'q'

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
