FROM ar90n/cxx
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

ARG user

USER root
RUN set -x && \
    apt-get update && \
    apt-get install -y libjasper-dev libopenexr-dev protobuf-compiler libprotoc-dev protobuf-c-compiler libwebp-dev libavcodec-dev && \
    apt-get install -y libhdf5-dev libdcmtk-dev libodil0-dev libgdcm2-dev && \
    apt-get install -y libfftw3-dev liblinear-dev libsvm-dev libgrpc-dev && \
    apt-get install -y libeigen3-dev libopenblas-dev libopenblas-base liblapack-dev liblapack3 liblapacke liblapacke-dev  && \
    apt-get install -y libgl1-mesa-dev libosmesa6-dev && \
    apt-get clean

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

USER $user
