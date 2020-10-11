---
toc: true
layout: post
description: An implementation of Poisson Blending Algorithm in 5 lines
categories: [Image Processing, Python]
title: 5行で書くポアソンブレンディング
---

## はじめに
画像の滑らかな合成アルゴリズムに[ポアソンブレンディング(Poisson Image Editting)](https://ja.wikipedia.org/wiki/Poisson_Image_Editing)があります．これは，ポアソン方程式を解くことで元の勾配を保ちながら，境界部分が連続となる合成画像を推定するというものです．
ネットを調べると，ポアソン方程式の計算にはSOR法やマルチグリッド法などを実装した高速な連立方程式ソルバを利用することが多いようです．しかしながら，外部のソルバを利用するとプロジェクトの規模が大きくなってしまいます．また，外部のソルバを利用せず，上述のアルゴリズムを自前で実装することは非常に困難な作業です．そこで，ヤコビ法をラプラシアンフィルタで記述し実装を簡略化しました．

## やったこと
* ラプラシアンフィルタを使用してヤコビ法を記述
* ポアソンブレンディングを５行で実装
* プロジェクトをGithubに作成

## ポアソン方程式をラプラシアンフィルタで記述する
[参考記事](https://ja.wikipedia.org/wiki/Poisson_Image_Editing)によると，座標$p$における画素値$f_p$は以下の式で求められます．

$$
f_p = \frac{\sum_{q \in N} f_q + 4 g_{q} - \sum_{q \in N} g_q}{4} 
$$

ここで，ヤコビ法によって$f_p$を求めます．$k$回目のイテレーションにおける座標$p$における画素値を$f_{p}^{k}$とします．すると，$f_{p}^{k}$は以下のように表記することができます．

$$
\begin{aligned}
f_{p}^{k+1} &=  \frac{\sum_{q \in N} f_{q}^{k} + 4 g_{p} - \sum_{q \in N} g_{q}}{4} \\\\
&= \frac{4f_{p}^{k} -4 f_{p}^{k}  + \sum_{q \in N} f_{q}^{k} + 4 g_{p} - \sum_{q \in N} g_q}{4} \\\\
&= f_{p}^{k} + \frac{-4 (f_{p}^{k} - g_{p})  + \sum_{q \in N} f_{q}^{k}  - g_q}{4} \\\\
&= f_{p}^{k} + \frac{1}{4} \Delta_{p} \left(f - g\right)
\end{aligned}
$$

以上の結果より，注目領域に$\frac{1}{4} \Delta_{p} \left(f - g\right)$を加算していくことで合成を行います． $\Delta_{p}$は座標$p$におけるラプラシアンを表します．以下にscipyを用いで実装したコードを示します．関数の引数はそれぞれ，target_imgが合成先画像，src_imgが合成元画像, mask_imgが合成領域マスク, iterが反復処理の回数をそれぞれ表します．コードサイズを5行に抑えるため，省いた処理（入力のバリデーション，反復処理の打ち切り）や冗長な処理（target_imgとsrc_imgとの差分を毎ループ計算している）がありますが，アルゴリズムのエッセンスは十分表現できていると思います．

## 5行で実装
```python
from scipy.ndimage import laplace
def poisson_blend(target_img, src_img, mask_img, iter: int = 1024):
    for _ in range(iter):
        target_img = target_img + 0.25 * mask_img * laplace(target_img - src_img)
    return target_img.clip(0, 1)
```
poisson_blendの入力と出力との関係を以下の図に示します．以下の図は，左側から合成元画像(src_img)，合成領域マスク(mask_img)，合成先画像(target_img)，合成画像を表します．境界領域が滑らかに合成されていることが確認できます．

![output](https://raw.githubusercontent.com/ar90n/poisson-blending-in-5lines/assets/image/output.jpg)

上記の関数に加え，結果確認用のノートブックなどを追加した[リポジトリ](https://github.com/ar90n/poisson-blending-in-5lines
)を作成しました．よろしければ，こちらも参考にしてください.

## 参考
* [Poisson Image Editing](https://ja.wikipedia.org/wiki/Poisson_Image_Editing)
* [ar90n/poisson-blending-in-5lines](https://github.com/ar90n/poisson-blending-in-5lines
)

