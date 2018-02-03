FROM ar90n/dev_env
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

ARG user

USER root

RUN set -x && \
    apt-get update -qq && \
    apt-get upgrade -qq && \
    apt-get install -y make imagemagick texlive-full && \
    apt-get clean

ADD npmrc /usr/local/etc/npmrc
RUN set -x && \
    apt-get update && \
    apt-get -y install nodejs npm && \
    apt-get clean && \
    npm cache clean && \
    npm install n -g && \
    n latest && \
    ln -sf /usr/local/bin/node /usr/bin/node && \
    apt-get -y purge nodejs

RUN npm install madoko -g

RUN apt-get install -y inotify-tools xzdec
#RUN tlmgr init-usertree
USER $user
