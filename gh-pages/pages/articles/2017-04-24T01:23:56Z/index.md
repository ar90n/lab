---
title: Chainerの環境構築
date: "2017-04-24T01:23:56Z"
layout: post
path: "/blog/2017-04-24T01:23:56Z/"
category:
- "CUDA"
- "Deep Learning"
- "Chainer"
description: "せっかくCUDAが動作するのでChainerもいれてみる．"
---
さくっとChainerを導入してみたメモです．

### pytenvの導入
折角なのでpyenvでpythonのバージョンを管理することにしました．
```bash
$ sudo apt-get install python3-pip curl libssl-dev libbz2-dev libreadline-dev libsqlite3-dev
$ pip install -U pip
$ curl https://raw.githubusercontent.com/KodairaTomonori/Qiita/master/shell/construct_pyenv.sh | sh
$ pyenv install 3.6.1
$ pyenv global 3.6.1
```

### Chainerの導入
```bash
$ pip install chainer matplotlib
$ python ./train_mnist.py --gpu 0
GPU: 0
# unit: 1000
# Minibatch-size: 100
# epoch: 20

epoch       main/loss   validation/main/loss  main/accuracy  validation/main/accuracy  elapsed_time
1           0.189677    0.105867              0.9419         0.9665                    13.5156
2           0.0729073   0.0773182             0.976699       0.976                     23.7467
3           0.0482483   0.0632961             0.984265       0.9794                    34.0212
4           0.0349474   0.081715              0.988465       0.9762                    44.3018
5           0.0304497   0.0814216             0.989882       0.9778                    54.588
6           0.0230233   0.0720032             0.992015       0.9816                    64.9659
7           0.0181515   0.0789226             0.994098       0.9812                    75.328
8           0.0194763   0.0868434             0.993932       0.9792                    85.7018
9           0.0171396   0.0894658             0.994732       0.9785                    96.0787
10          0.0145405   0.0710021             0.995466       0.9842                    106.47
11          0.0126733   0.0943754             0.996098       0.9816                    116.815
12          0.0170367   0.0958864             0.994498       0.9788                    127.117
13          0.0125324   0.0834046             0.996216       0.9828                    137.432
14          0.00679618  0.0805982             0.997932       0.9845                    147.746
15          0.0122508   0.0974698             0.996232       0.9821                    158.148
16          0.0109486   0.0937799             0.996682       0.981                     168.508
17          0.00987097  0.0976787             0.996949       0.9805                    178.893
18          0.00905929  0.115255              0.997299       0.9793                    189.315
19          0.0113884   0.101689              0.997066       0.9821                    199.672
20          0.00787066  0.104861              0.997533       0.9813                    210.029
```
