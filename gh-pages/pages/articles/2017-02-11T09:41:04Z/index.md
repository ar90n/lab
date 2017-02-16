---
title: tiny-dnnをビルドしてみる
date: "2017-02-11T09:41:04Z"
layout: post
path: "/blog/2017-02-11T09:41:04Z/"
category: "Deep Learning"
description: "libDNN，NNPACKと共にtiny-dnnをビルドしてた"
---
tiny-dnnとはC++で実装されたHeader onlyでシンプルな実装が非常に良い感じなライブラリです.  
バックエンドにはtiny-dnnが自前で用意しているものの他にlibDNNやNNPACKを用いることも可能なので，今回はこれらと共にビルドしてみました．
以下，その時のログです．

### NNPACKのビルド
```bash
$git clone https://github.com/Maratyszcza/PeachPy.git
$cd PeachPy
$pip install --upgrade -r requirements.txt
$python setup.py generate
$pip install --upgrade .
$brew install ninja
$pip install ninja-syntax
$git clone --recursive https://github.com/Maratyszcza/NNPACK.git
$cd NNPACK
$python ./configure.py
$ninja
```

### libDNNNのビルド
```bash
$cmake -DUSE_CUDA=OFF -DCMAKE_INSTALL_PREFIX=/opt ..
$make -j
$sudo make install
```

### tiny-dnnのビルド
```bash
brew install tbb
brew install homebrew/science/opencv3 --c++11 --wtih-tbb --without-numpy --without-openexr --without-python
$cmake -DCMAKE_PREFIX_PATH=/opt/share/GreenteaLibDNN/  -DNNPACK_LIB=../../NNPACK/lib/ -DNNPACK_INCLUDE_DIR=../../NNPACK/include/ -DUSE_TBB=ON -DUSE_GEMMLOWP=ON -DUSE_LIBDNN=ON -DUSE_NNPACK=ON -DUSE_OPENCL=ON -DUSE_AVX2=ON  -DCMAKE_INSTALL_PREFIX=/opt ..
$make -j
$sudo make install
```
