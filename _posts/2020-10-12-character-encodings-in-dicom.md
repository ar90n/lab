---
toc: true
layout: post
description: A study of handling Japanese characters in DICOM
categories: [DICOM]
title: DICOMにおける文字コードの取り扱い
---

## はじめに
DICOMを取り扱うライブラリの多くは海外製であるため，日本語の取り扱いが得意で無いことが多いです. そこで，DICOMにおいて日本語を正しく扱うために色々と調べたのでまとめます．

## やったこと
* DICOMで日本語を表現する方法を調査

## 文字コードについて復習
DICOMにおける文字コードの取り扱いについて述べる前に，いわゆる”文字コード”と呼ばれているものは何であるかを簡単に復習します．より正しく”文字コード”について理解するためには，（符号化）文字集合と（文字）符号化方式を区別することが重要です．文字集合とは表現する文字の集合（アルファベット全てやひらがな全てなど）です．符号化方式とは文字集合の要素をコンピュータ上で取り扱うことが可能な形式に変換する方法です．従って，同一の文字集合に対して複数の符号化方式が存在します．

## ISO/IEC 2022 とはなんだろう
ISO/IEC 2022とは文字集合を７ビット文字または８ビット文字にて表現する符号化方式です．特徴としてはエスケープシーケンスを利用することで，複数の文字集合を同時に取り扱うことが可能である点が挙げられます．日本語においては，このISO/IEC 2022の機構を利用したISO-2022-JPが広く利用されています．ISO-2022-JPは一般的にJISコードと呼ばれます．これには漢字，ひらがな，カタカナ，ラテン文字，ギリシア文字，キリル文字など多くの文字集合が含まれます．（半角カタカナは含まれません）

上述した通り，ISO/IEC 2022には7ビット文字を利用したものと8ビット文字を利用したものとが存在します．ここでは８ビット文字を利用した場合のみを考慮します．ISO/IEC 2022の符号表は図形文字の領域（GL，GR）と制御文字の領域（CL，CR)からなります．また，４つの仮想的なバッファ(G0, G1, G2,G3)が存在します．ISO/IEC 2022を利用するには，エスケープシーケンスを用いて任意のバッファへ文字集合をロードし，それを図形文字の領域（GL，GR)に呼び出します．呼び出しには永続的に呼び出すロッキングシフトと１文字のみのシングルシフトが存在します．

## DICOMにおけるISO/IEC 2022
DICOMでは符号方式としてISO/IEC 2022のサブセットを用いています．具体的には

* 8ビット文字をサポート
* デフォルトの文字集合としてISO646を使用
* ISO646は必ずGL空間に呼び出される
* ISO646以外の文字集合を用いる場合は`Specific Character Set(0008,0005)`に指定する
* エスケープを利用した符号拡張を使う(ISO/IEC 2022)場合はSpecific Character Setに二つ以上の文字集合を指定する
* バッファG0はGLに，バッファG1はGRにロードされる
* G2，G3は利用不可
* G0，G1は常に呼び出し状態にあるのでロッキングシフトは不要

という制約があります．

## 実際にDICOMで日本語を扱ってみる
DICOMで日本語を扱うためには，`Specific Character Set(0008,0005)`に使用する符号方式を設定する必要があります．ここでは，以下の患者名を符号化することを考えます．

```
ﾔﾏﾀﾞ^ﾀﾛｳ=山田^太郎=やまだ^たろう
```

この患者名には，半角かな，漢字，全角ひらがなが含まれています．英数字と半角かなはISO 2022 IR 13で，漢字と全角ひらがなはISO 2022 IR 87で表現が可能です．
従って，`Specific Character Set(0008,0005)`には以下の値を設定します．

```
ISO 2022 IR 13\ISO 2022 IR 87
```

前述の通り，ISO/IEC 2022ではエスケープシーケンスを用いて複数の符号方式を切り替えます．
従って，上述の患者名においても適切なタイミングでエスケープする必要があります． エスケープシーケンスを追加した例を以下にしまします．

```
ﾔﾏﾀﾞ^ﾀﾛｳ= ESC 02/04 04/02 山田 ESC 02/08 04/10 ^ ESC 02/04 04/02 太郎 ESC 02/08 04/10 = ESC 02/04 04/02 やまだ ESC 02/08 04/10 ^ ESC 02/04 04/02 たろう ESC 02/08 04/10
```

また，各エスケープシーケンスの詳細を以下に示します．

|エスケープシーケンス|処理|
| :----: | :----: | 
| ESC 02/08 04/10 | 英数字(ISO-IR 14)をGLにロード |
| ESC 02/04 04/02 | 漢字とひらがな(ISO-IR 87)をGLにロード |

これらを踏まえて，実際にエンコーディングすると以下の値が得られます．
```
13/04 12/15 12/00 13/14 05/14 12/00 13/11 11/03 03/13 01/11 02/04 04/02 03/11 03/03 04/05 04/04 01/11 02/08 04/10 05/14 01/11 02/04 04/02 04/02 04/00 04/15 03/10 01/11 02/08 04/10 03/13 01/11 02/04 04/02 02/04 06/04 02/04 05/14 02/04 04/00 01/11 02/08 04/10 05/14 01/11 02/04 04/02 02/04 03/15 02/04 06/13 02/04 02/06 01/11 02/08 04/10
```

## 参考

* [JIS漢字コード](https://ja.wikipedia.org/wiki/JIS%E6%BC%A2%E5%AD%97%E3%82%B3%E3%83%BC%E3%83%89)
* [ISO/IEC 2022](https://ja.wikipedia.org/wiki/ISO/IEC_2022)
* [JIS X 0201](https://ja.wikipedia.org/wiki/JIS_X_0201)
* [２０１１ ST講座 入門講座 DICOM規格 初級 –DICOMをうまく使いこなす–](http://www.jira-net.or.jp/dicom/file/dicom_2011_standard_jrc_st.pdf)
* [DICOMの日本語エンコーディング処理実装](https://blog.goo.ne.jp/satomi_takeo/e/4bd08b26b1750bbf15ecefda00789318)
* [DICOM に慣れる － 現場で DICOM 接続に慌てないための知識 （２） 文字系の通信 －](http://www.jira-net.or.jp/dicom/file/dicom_201002_MRC_vol19-1.pdf)
* [H.3 Example of Person Name Value Representation in the Japanese Language](http://dicom.nema.org/dicom/2013/output/chtml/part05/sect_H.3.html)