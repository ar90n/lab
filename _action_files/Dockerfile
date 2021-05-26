FROM node:12.19.0-buster-slim as nodejs

FROM fastai/jekyll:latest

WORKDIR /fastpages
COPY . .
RUN chmod u+x action_entrypoint.sh
RUN chmod u+x word2post.sh
RUN dos2unix /fastpages/*.sh
RUN pip install -U pip
RUN pip install torch==1.8.1+cpu torchvision==0.9.1+cpu torchaudio==0.8.1 -f https://download.pytorch.org/whl/torch_stable.html
RUN pip install -r ./requirements.txt

# Install required packages
RUN apt-get update \
    && apt-get upgrade -y \
    && apt-get install -y --no-install-recommends \
            openssl \
            libasound2-dev \
            libfreetype6-dev \
            libexpat1-dev \
            libxcb-composite0-dev \
            libssl-dev \
            libx11-dev \
            build-essential \
            cmake \
            gnupg \
            locales \
            fonts-noto-cjk \
            # for ZeroMQ
            libtool \
            libffi-dev \
            libzmq3-dev \
            libczmq-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install Rust 
ENV RUSTUP_HOME=/usr/local/rustup
ENV CARGO_HOME=/usr/local/cargo
ENV PATH=/usr/local/cargo/bin:$PATH
ENV RUST_VERSION=nightly
ENV rustupSha256='49c96f3f74be82f4752b8bffcf81961dea5e6e94ce1ccba94435f12e871c3bdb'
RUN set -eux; \
    url="https://static.rust-lang.org/rustup/archive/1.22.1/x86_64-unknown-linux-gnu/rustup-init"; \
    wget "$url"; \
    echo "${rustupSha256} *rustup-init" | sha256sum -c -; \
    chmod +x rustup-init; \
    ./rustup-init -y --no-modify-path --default-toolchain $RUST_VERSION; \
    rm rustup-init; \
    chmod -R a+w $RUSTUP_HOME $CARGO_HOME; \
    rustup --version; \
    cargo --version; \
    rustc --version;
RUN cargo install evcxr_jupyter \
    && evcxr_jupyter --install

# Install Javascript
ENV NODE_VERSION 12.19.0
ENV YARN_VERSION 1.22.5
RUN mkdir -p /opt
COPY --from=nodejs /opt/yarn-v${YARN_VERSION} /opt/yarn
COPY --from=nodejs /usr/local/bin/node /usr/local/bin/
COPY --from=nodejs /usr/local/lib/node_modules/ /usr/local/lib/node_modules/
RUN ln -s /opt/yarn/bin/yarn /usr/local/bin/yarn \
    && ln -s /opt/yarn/bin/yarn /usr/local/bin/yarnpkg \
    && ln -s /usr/local/bin/node /usr/local/bin/nodejs \
    && ln -s /usr/local/lib/node_modules/npm/bin/npm-cli.js /usr/local/bin/npm \
    && ln -s /usr/local/lib/node_modules/npm/bin/npm-cli.js /usr/local/bin/npx
RUN yarn global add ijavascript typescript itypescript @types/node \
    && ijsinstall \
    && its --install=global

# Install jupyter labextensions
RUN jupyter labextension install \
            @jupyter-widgets/jupyterlab-manager \ 
            jupyter-leaflet \
    && jupyter lab build
RUN jupyter nbextension install https://github.com/drillan/jupyter-black/archive/master.zip
RUN jupyter nbextension enable jupyter-black-master/jupyter-black

CMD [ "/fastpages/action_entrypoint.sh" ]
