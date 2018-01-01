FROM ar90n/dev_env
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

ARG user

USER root
WORKDIR /tmp
RUN set -x && \
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF && \
    echo "deb http://download.mono-project.com/repo/ubuntu xenial main" >> /etc/apt/sources.list.d/mono-official.list && \
    apt-get update && \
    apt-get install -y mono-devel fsharp

USER $user
ADD mono_dein.toml /opt/nvim/specs/mono_dein.toml
ADD fsharp.snippets /opt/nvim/procon_snippets/fsharp.snippets
RUN set -x && \
    nvim -c 'call dein#install()' -c 'q' && \
    nvim -c 'UpdateRemotePlugins' -c 'q'

CMD ["fsharpi"]
