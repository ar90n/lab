---
toc: true
layout: post
date: 2021-03-30
categories: [Rust, コンピュータグラフィックス]
title: Rustでレイトレーシングをしてみる
nocite: |
  @Shirley2020RTW1, @Shirley2020RTW2
---

## はじめに
Rustの勉強を兼ねて，[こちら](http://in1weekend.blogspot.com/)を参考にレイトレーシングを写経してみました．

<div class="github-card" data-github="ar90n/ray-tracing-in-weekends-with-rust" data-width="400" data-height="153" data-theme="default"></div>
<script src="//cdn.jsdelivr.net/github-cards/latest/widget.js"></script>

## メモ

* 十分な数のRayを計算しなければ綺麗な画像は得られない．レイトレーシング結果のサンプルで，画素が黒く欠損しているのは十分な数のRayを計算していないから
* Rayによってランダムにサンプリングすることで，ブラーやボケやアンチエイリアシングを表現する
* 思っていたよりも単純

## 結果
* 1週目
![first_weekend](https://raw.githubusercontent.com/ar90n/ray-tracing-in-weekends-with-rust/main/assets/output_firest_weekend.jpg)

* 2週目
![second_weekend](https://raw.githubusercontent.com/ar90n/ray-tracing-in-weekends-with-rust/main/assets/output_second_weekend.jpg)

## 参考
::: {#refs}
:::