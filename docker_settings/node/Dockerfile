FROM ar90n/dev_env
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

ARG user
ARG node_version

USER root
WORKDIR /tmp
ADD npmrc /usr/local/etc/npmrc
RUN set -x && \
    apt-get update && \
    apt-get -y install nodejs npm && \
    apt-get clean && \
    npm cache clean && \
    npm install n -g

RUN set -x && \
    n $node_version && \
    ln -sf /usr/local/bin/node /usr/bin/node && \
    apt-get -y purge nodejs

RUN set -x && \
    npm install webpack webpack-dev-server -g && \
    npm install flow-bin flow-typed typescript -g && \
    npm install tslint babel-eslint eslint eslint-config-airbnb eslint-plugin-import eslint-plugin-jsx-a11y eslint-plugin-react eslint-plugin-flowtype -g && \
    npm install mocha chai

USER $user
ADD node_dein.toml /opt/nvim/specs/node_dein.toml
RUN set -x && \
    nvim -c 'call dein#install()' -c 'q' && \
    nvim -c 'UpdateRemotePlugins' -c 'q'

CMD ["node"]
