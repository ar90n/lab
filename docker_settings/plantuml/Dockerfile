FROM ar90n/dev_env
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

ARG user

USER root
RUN set -x && \
    apt-get update -qq && \
    apt-get upgrade -qq && \
    apt-get install -y default-jre && \
    apt-get clean

ADD plantuml /opt/bin
WORKDIR /opt
RUN set -x && \
    chmod 755 /opt/bin/plantuml && \
    mkdir plantuml && \
    curl -L http://sourceforge.net/projects/plantuml/files/plantuml.jar/download -o ./plantuml/plantuml.jar

USER $user
