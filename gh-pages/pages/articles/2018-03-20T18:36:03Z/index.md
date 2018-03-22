---
title: サイトジェネレータをReact Staticに変更する1
date: "2018-03-20T18:36:03Z"
layout: post
path: "/blog/2018-03-20T18:36:03Z/"
category: "React"
description: "サイトジェネレータをReact Staticに変更するべき作業を開始！"
---
現状，Gatsbyを使ってサイトを生成しているのですが，以下のように色々と辛さを感じることがあります．

* Googleにインデックスしてもらえない(JSファイルが大きすぎ?)
* ロードが遅い
* もう少し自由にカスタマイズしたい

色々と調べてみると[React Static](https://react-static.js.org/)が自分の用途に合っていそうなので，こちらに変更すべく作業を開始しました．

## やったこと

1. React Staticを導入
1. Typescriptテンプレートで環境構築
1. [Core Concepts](https://react-static.js.org/concepts)を読み概要を把握

## 作業詳細
### 動作確認
Quick Startに従って以下のようにreact-staticを導入．
```bash
$ yarn global add react-static
```
引数からTypescriptテンプレートを指定してプロジェクトを作成．
```bash
$ react-static create -n lab.ar90n.net -t typescript
```
プロジェクトディレクトリに入ってWebサーバーを起動する．
```bash
$ cd lab.ar90n.net
$ yarn start
```
ブラウザからhttp://localhost:3000/にアクセスして動作を確認する．
![トップページ](./sc01.png)

### Core Conceptsのメモ

## 所感
Reactを使ったSPAなのはGatsbyと似ているが，こちらの方が細かな調整が効く感じがする．ブログ以外のサイトやアプリの雛形なんかにも使えそうな気がしてきました．
