FROM ar90n/dev_env
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

ARG user

#    cargo install --force rustfmt-nightly

USER $user
WORKDIR /tmp
ENV RUSTUP_HOME /opt/rustup
ENV CARGO_HOME /opt/cargo
ADD rust_dein.toml /opt/nvim/specs/rust_dein.toml
RUN set -x && \
    mkdir /opt/rustup && \
    curl -sSf https://sh.rustup.rs -o rustup.sh && \
    bash rustup.sh -y --no-modify-path --default-toolchain nightly && \
    /opt/cargo/bin/cargo install racer && \
    /opt/cargo/bin/cargo install --force rustfmt-nightly && \
    nvim -c 'call dein#install()' -c 'q' && \
    nvim -c 'UpdateRemotePlugins' -c 'q'

ENV LD_LIBRARY_PATH /opt/rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib:$LD_LIBRARY_PATH
ENV PATH /opt/cargo/bin:$PATH
CMD ["cargo"]
