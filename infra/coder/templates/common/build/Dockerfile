ARG BASE_IMAGE=ubuntu:24.04
FROM ${BASE_IMAGE}

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl git build-essential sudo ca-certificates locales openssh-client \
    pkg-config libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev libffi-dev liblzma-dev \
    iproute2 iputils-ping traceroute mtr-tiny dnsutils netcat-openbsd tcpdump socat nmap whois rsync \
    htop strace ltrace gdb valgrind sysstat iotop linux-tools-generic \
    # 最小限の X11 環境 (xeyes 等の x11-apps、xdpyinfo 等の x11-utils)。
    # DISPLAY=:20000 (Xpra) への疎通確認・簡単な GUI 表示がすぐ使える。
    x11-apps x11-utils \
    && rm -rf /var/lib/apt/lists/*

# ホストの argon (uid 501) と揃えることで、bind mount した共有キャッシュディレクトリを
# 権限エラー無く読み書きできるようにする。
RUN useradd -u 501 -m -s /bin/bash coder \
    && echo "coder ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/coder

USER coder
WORKDIR /home/coder

# mise 本体だけを焼き込む。rust/go/python/node/gh/pixi 等の実体は起動時に
# bind mount される共有キャッシュ (mise-data/mise-cache/cargo/rustup) に
# インストールされるため、ここでは入れない。
RUN curl -fsSL https://mise.run | sh

# Ubuntu 既定の ~/.bashrc は非対話シェルだと冒頭の
#   case $- in *i*) ;; *) return;; esac
# で即座に return するため、`docker exec`/`coder ssh -- <cmd>` のような
# 非対話実行では .bashrc に書いた `mise activate` 等が一切効かない。
# そのため PATH は shell rc に頼らず ENV で静的に通す。
# mise の shims ディレクトリ (~/.local/share/mise/shims) は「対話シェル用の
# activate」とは別に、どの実行コンテキストでも動く静的な解決方式。
ENV PATH="/home/coder/.local/share/mise/shims:/home/coder/.local/bin:/home/coder/.cargo/bin:/home/coder/.opencode/bin:${PATH}"
RUN echo 'eval "$(~/.local/bin/mise activate bash)"' >> ~/.bashrc

# starship / claude / opencode は軽量な静的バイナリなのでここで焼き込む。
RUN curl -sS https://starship.rs/install.sh | sh -s -- -y -b ~/.local/bin \
    && echo 'eval "$(~/.local/bin/starship init bash)"' >> ~/.bashrc
RUN curl -fsSL https://claude.ai/install.sh | bash
RUN curl -fsSL https://opencode.ai/install | bash
