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
    # Chrome / Electron 系 (Playwright, Puppeteer, Electron アプリ) が要求する
    # 共有ライブラリ。これが無いと ldd で 14 個の soname が欠落して起動しない。
    # パッケージ数が 12 なのは libnss3 が libnss3.so / libnssutil3.so /
    # libsmime3.so の 3 つを供給するため。t64 接尾辞は Ubuntu 24.04 の
    # time_t 64bit 移行によるもので、24.04 以外のベースでは名前が変わる。
    libnss3 libnspr4 libatk1.0-0t64 libatk-bridge2.0-0t64 libatspi2.0-0t64 \
    libcups2t64 libgtk-3-0t64 libpango-1.0-0 libcairo2 libasound2t64 \
    libxdamage1 libxkbcommon0 \
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

# herdr (herdr-mirror でこのワークスペースを Mac 側サイドバーに集約するために使う)。
# 意図的に /home/coder ではなく /usr/local/bin に入れている: /home/coder は
# docker_volume でマウントされるため、volume が既に存在する既存ワークスペースでは
# イメージ側の /home/coder 配下が完全にシャドウされ、Dockerfile への追加が届かない
# (マーカーファイルを仕込んで実機検証済み。/opt や /usr/local は届く)。
# 公式インストーラは stable を取得する。stable 0.8.0 は protocol 19 で Mac 側の
# preview 版と同一プロトコルのため mirror は問題なく通る。
USER root
RUN curl -fsSL https://herdr.dev/install | HERDR_INSTALL_DIR=/usr/local/bin sh
USER coder
