FROM ar90n/dev_env
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

ARG user

USER root
WORKDIR /tmp
RUN set -x && \
    apt-get update && \
    apt-get install -y zlib1g-dev libjpeg-dev libpng-dev libtiff5-dev protobuf-compiler libprotoc-dev protobuf-c-compiler && \
    apt-get install -y clang-4.0 libclang-common-4.0-dev libclang1-4.0 clang-4.0-doc clang-4.0-examples clang-format-4.0 clang-tidy-4.0 libclang-4.0-dev libclang1-4.0-dbg && \
    apt-get install -y lldb-4.0 liblldb-4.0 liblldb-4.0-dbg liblldb-4.0-dev && \
    apt-get install -y libllvm4.0 libllvm4.0-dbg llvm-4.0 llvm-4.0-dev llvm-4.0-doc llvm-4.0-examples llvm-4.0-runtime llvm-4.0-tools && \
    apt-get install -y libssl-dev libboost-all-dev libreadline-dev && \
    apt-get install -y gcc gdb cgdb cmake make autoconf automake pkg-config && \
    apt-get install -y default-jre default-jdk ant && \
    apt-get install -y valgrind && \
    apt-get clean

# conan
RUN set -x && \
    pip install conan

# watchman
WORKDIR tmp
RUN git clone https://github.com/facebook/watchman.git && \
    cd watchman && \
    git checkout -b v4.9.0 refs/tags/v4.9.0 && \
    ./autogen.sh && \
    ./configure && \
    make && make install && make clean && \
    cd .. && \
    rm -rf watchman

# buck
WORKDIR tmp
RUN set -x &&\
    git clone https://github.com/facebook/buck.git && \
    cd buck && \
    ant && \
    cp ./bin/buck /usr/local/bin && \
    cp ./bin/buckd /usr/local/bin && \
    cd /opt/bin && \
    cd .. && \
    rm -rf buck

# rtags
WORKDIR tmp
RUN set -x &&\
    git clone https://github.com/Andersbakken/rtags && \
    cd rtags && \
    git submodule init && \
    git submodule update && \
    mkdir build && \
    cd build && \
    cmake .. && \
    make && make install && \
    cd ../.. && \
    rm -rf rtags

# bear
WORKDIR tmp
RUN set -x && \
    git clone https://github.com/rizsotto/Bear && \
    cd Bear && \
    mkdir build && \
    cd build && \
    cmake .. && \
    make && \
    make install && \
    cd ../.. && \
    rm -rf Bear

USER $user
ADD cxx_dein.toml /opt/nvim/specs/cxx_dein.toml
RUN set -x && \
    nvim -c 'call dein#install()' -c 'q' && \
    nvim -c 'UpdateRemotePlugins' -c 'q'

CMD ["clang++"]
