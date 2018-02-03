FROM ar90n/cxx
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

ARG user

USER root
WORKDIR /tmp
RUN set -x && \
    apt-get update && \
    apt-get install -y gettext bison flex xutils-dev libxext-dev libdrm-dev libx11-xcb-dev libxcb-xfixes0-dev libxcb-dri2-0-dev llvm-5.0-dev cmake-curses-gui && \
    pip install mako && \
    apt-get clean

RUN update-alternatives --install /usr/bin/llvm-config       llvm-config      /usr/bin/llvm-config-5.0  200 \
                        --slave   /usr/bin/llvm-ar           llvm-ar          /usr/bin/llvm-ar-5.0 \
                        --slave   /usr/bin/llvm-as           llvm-as          /usr/bin/llvm-as-5.0 \
                        --slave   /usr/bin/llvm-bcanalyzer   llvm-bcanalyzer  /usr/bin/llvm-bcanalyzer-5.0 \
                        --slave   /usr/bin/llvm-cov          llvm-cov         /usr/bin/llvm-cov-5.0 \
                        --slave   /usr/bin/llvm-diff         llvm-diff        /usr/bin/llvm-diff-5.0 \
                        --slave   /usr/bin/llvm-dis          llvm-dis         /usr/bin/llvm-dis-5.0 \
                        --slave   /usr/bin/llvm-dwarfdump    llvm-dwarfdump   /usr/bin/llvm-dwarfdump-5.0 \
                        --slave   /usr/bin/llvm-extract      llvm-extract     /usr/bin/llvm-extract-5.0 \
                        --slave   /usr/bin/llvm-link         llvm-link        /usr/bin/llvm-link-5.0 \
                        --slave   /usr/bin/llvm-mc           llvm-mc          /usr/bin/llvm-mc-5.0 \
                        --slave   /usr/bin/llvm-mcmarkup     llvm-mcmarkup    /usr/bin/llvm-mcmarkup-5.0 \
                        --slave   /usr/bin/llvm-nm           llvm-nm          /usr/bin/llvm-nm-5.0 \
                        --slave   /usr/bin/llvm-objdump      llvm-objdump     /usr/bin/llvm-objdump-5.0 \
                        --slave   /usr/bin/llvm-ranlib       llvm-ranlib      /usr/bin/llvm-ranlib-5.0 \
                        --slave   /usr/bin/llvm-readobj      llvm-readobj     /usr/bin/llvm-readobj-5.0 \
                        --slave   /usr/bin/llvm-rtdyld       llvm-rtdyld      /usr/bin/llvm-rtdyld-5.0 \
                        --slave   /usr/bin/llvm-size         llvm-size        /usr/bin/llvm-size-5.0 \
                        --slave   /usr/bin/llvm-stress       llvm-stress      /usr/bin/llvm-stress-5.0 \
                        --slave   /usr/bin/llvm-symbolizer   llvm-symbolizer  /usr/bin/llvm-symbolizer-5.0 \
                        --slave   /usr/bin/llvm-tblgen       llvm-tblgen      /usr/bin/llvm-tblgen-5.0

RUN set -x && \
    git clone git://anongit.freedesktop.org/git/xorg/proto/glproto && \
    cd glproto && \
    ./autogen.sh && \
    ./configure && \
    make install

RUN set -x && \
    curl -L -O https://github.com/mesa3d/mesa/archive/mesa-17.3.3.tar.gz && \
    tar -zxvf mesa-17.3.3.tar.gz && \
    cd mesa-mesa-17.3.3 && \
    ./autogen.sh --enable-gallium-osmesa  --enable-llvm --disable-driglx-direct --disable-dri --disable-gbm --disable-egl --with-gallium-drivers=swrast && \
    make && \
    make install

RUN set -x && \
    curl -L -O https://github.com/Kitware/VTK/archive/v8.1.0.tar.gz &&\
    tar -zxvf v8.1.0.tar.gz && \
    cd VTK-8.1.0 && \
    mkdir build && \
    cd build && \
    cmake -DVTK_DEFAULT_RENDER_WINDOW_OFFSCREEN=ON -DVTK_USE_X=OFF -DVTK_OPENGL_HAS_OSMESA=ON -DOSMESA_LIBRARY=/usr/local/lib/libOSMesa.so -DOSMESA_INCLUDE_DIR=/usr/local/include/GL -DBUILD_SHARED_LIBS=ON -DBUILD_TESTING=ON -DCMAKE_BUILD_TYPE=Release -DVTK_RENDERING_BACKEND=OpenGL2 .. && \
    make && \
    make install
