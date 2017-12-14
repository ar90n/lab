FROM ar90n/dev_env
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

USER root
WORKDIR /tmp
RUN set -x && \
    apt-get update && \
    apt-get install -y zlib1g-dev libjpeg-dev libpng-dev libtiff5-dev libjasper-dev libopenexr-dev protobuf-compiler libprotoc-dev protobuf-c-compiler && \
    apt-get install -y libhdf5-dev libdcmtk-dev libodil0-dev libgdcm2-dev && \
    apt-get install -y clang-4.0 libclang-common-4.0-dev libclang1-4.0 clang-4.0-doc clang-4.0-examples clang-format-4.0 clang-tidy-4.0 libclang-4.0-dev libclang1-4.0-dbg && \
    apt-get install -y lldb-4.0 liblldb-4.0 liblldb-4.0-dbg liblldb-4.0-dev && \
    apt-get install -y libllvm4.0 libllvm4.0-dbg llvm-4.0 llvm-4.0-dev llvm-4.0-doc llvm-4.0-examples llvm-4.0-runtime llvm-4.0-tools && \
    apt-get install -y libboost-all-dev libreadline-dev && \
    apt-get install -y libfftw3-dev liblinear-dev libsvm-dev libgrpc-dev && \
    apt-get install -y libeigen3-dev libopenblas-dev libopenblas-base liblapack-dev liblapack3 liblapacke liblapacke-dev  && \
    apt-get install -y libgl1-mesa-dev libosmesa6-dev && \
    apt-get install -y gcc gdb cgdb cmake make autoconf automake pkg-config && \
    apt-get -y install default-jre default-jdk ant && \
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

# Halide
WORKDIR tmp
RUN set -x && \
    curl -sL -o /tmp/halide-linux-64-gcc53.tgz -O https://github.com/halide/Halide/releases/download/release_2017_10_30/halide-linux-64-gcc53-trunk-3af238615667312dcb46607752e3ae5d0ec5d713.tgz && \
    tar -zxf /tmp/halide-linux-64-gcc53.tgz -C /tmp && \ 
    install -d /opt/lib && \
    install /tmp/halide/lib/libHalide.a /usr/local/lib && \
    install /tmp/halide/bin/libHalide.so /usr/local/lib && \
    install /tmp/halide/include/* /usr/local/include && \
    rm -rf /tmp/halide

# VTK
WORKDIR tmp
RUN set -x && \
    curl -sL -o /tmp/v8.0.1.tar.gz -O https://github.com/Kitware/VTK/archive/v8.0.1.tar.gz && \
    tar -zxf /tmp/v8.0.1.tar.gz -C /tmp && \
    mkdir /tmp/VTK-8.0.1/build && \
    cd /tmp/VTK-8.0.1/build && \
    cmake -DVTK_USE_OFFSCREEN=ON -DVTK_USE_X=OFF -DVTK_OPENGL_HAS_OSMESA=ON -DOSMESA_LIBRARY=/usr/lib/x86_64-linux-gnu/libOSMesa.so -DOSMESA_INCLUDE_DIR=/usr/include/GL -DBUILD_SHARED_LIBS=ON -DBUILD_TESTING=OFF -DCMAKE_BUILD_TYPE=Release -DVTK_RENDERING_BACKEND=OpenGL .. && \
    make && \
    make install && \
    cd /  && \
    rm -rf /tmp/VTK-8.0.1 /tmp/v8.0.1.tar.gz

#opencv
RUN set -x && \
    apt-get install -y libwebp-dev libavcodec-dev

WORKDIR tmp
RUN set -x && \
    curl -sL -o /tmp/3.3.1.tar.gz -O https://github.com/opencv/opencv/archive/3.3.1.tar.gz && \
    tar -zxf /tmp/3.3.1.tar.gz -C /tmp && \
    mkdir -p /tmp/opencv-3.3.1/build && \
    cd /tmp/opencv-3.3.1/build && \
cmake -D CMAKE_CXX_FLAGS=-std=c++11 -D BUILD_JASPER=OFF -D BUILD_JPEG=OFF -D BUILD_OPENEXR=OFF -D BUILD_PNG=OFF -D BUILD_PROTOBUF=OFF -D BUILD_TIFF=OFF -D BUILD_ZLIB=OFF -D BUILD_opencv_java=OFF -D WITH_1394=OFF -D WITH_CUBLAS=OFF -D WITH_CUDA=OFF -D WITH_CUFFT=OFF -D WITH_GDCM=ON -D WITH_GPHOTO2=OFF -D WITH_GSTREAMER=OFF -D WITH_GSTREAMER_0_10=OFF -D WITH_HALIDE=ON -D WITH_MATLAB=OFF -D WITH_OPENCL=OFF -D WITH_OPENCLAMDBLAS=OFF -D WITH_OPENCLAMDFFT=OFF -D WITH_OPENCL_SVM=OFF -D WITH_V4L=OFF -D WITH_VTK=OFF -D BUILD_opencv_dnn=OFF -D ENABLE_CXX11=ON .. && \
    make && \
    make install && \
    cd ../../ && \
    rm -rf opencv-3.3.1

USER $USER
ADD cxx_dein.toml /opt/nvim/specs/cxx_dein.toml
RUN set -x && \
    nvim -c 'call dein#install()' -c 'q' && \
    nvim -c 'UpdateRemotePlugins' -c 'q'

CMD ["clang++"]
