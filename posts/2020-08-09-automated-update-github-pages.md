---
toc: true
layout: post
date: 2020-08-09
description: Update GitHubPages automatically
categories: [Elm, Web]
title: GitHub ActionsでWebサイトを自動更新する
---

## はじめに
GitHubPages + GitHub Actions でコンテンツを自動的に更新するWebサイトをつくってみました．GitHub Trending に登場したリポジトリに付与されていた topics を一覧表示するサイトです．言語ごとの用途がざっくりとした感じでみることができます．

[Topics in GitHub Trending](https://ar90n.github.io/github-topics-trending/) - [GitHub](https://github.com/ar90n/github-topics-trending
)

今回，勉強も兼ねてフロントエンドとデータのクローリング処理共に Elm で記述しました．

## GitHub Actions によるバッチ処理の定刻実行

GitHub Actionsによって定刻実行されるバッチ処理はは以下の3つのジョブからなります．

* build
* crawl
* push

それぞれ，クローリングスクリプトのビルド，データのクローリング，データのリポジトリへのプッシュを行います．設定の詳細は[こちら](https://github.com/ar90n/github-topics-trending/blob/master/.github/workflows/crawling.yml)を参考にしてください．

## CLIプログラムを Elm で書く
crawlジョブではElmで記述されたCLIプログラムを実行してデータの取得を行います．ElmはWebフロントエンドを記述することに特化している言語です．そのため，CLIプログラムの開発に必要な，コマンドライン引数の処理といった機能がサポートされいません．そこで，今回は以下のパッケージを持ちました．

[pdillonkearns/elm-cli-options-parser](https://github.com/dillonkearns/elm-cli-options-parser)

 こちらのパッケージを用いると，Flagを用いてコマンドライン引数へのアクセスが可能となります．今回のプログラムでは，以下の様に`githubToken` と `language` と `dataRange` とを引数から受け取っています．

```elm
init : Flags -> TrendingApiOptions -> ( Model, Cmd Msg )
init { githubToken } { language, dateRange } =
    ( { githubToken = githubToken }
    , attemptApi TrendingApiResponse (fetchTrending githubToken language dateRange)
    )
```

また，GitHub Tredingsの取得は以下のAPIを利用させていただきました．

[github-trending-api](https://github.com/huchenme/github-trending-api)


## クローリング結果の保存
今回，GitHub PagesのみでWebサイトをホスティングするため，データベースを使用することはできません．そのため，クローリング結果はJSONに整形されたのちに，直接GitHub Pagesのリポジトリにプッシュされます．

[topics](https://github.com/ar90n/github-topics-trending/tree/gh-pages/topics)

この，JSONをフロントエンドから取得することで動的なコンテンツ更新を実現します．

## Elmによるフロントエンド開発
特筆すべきことはありません．Elmは本当に書きやすい言語でした．

## 所感
Elmの勉強としてはちょうど良い分量でした．GitHub PagesとGitHub Actionsで結構なことができる印象なので，また何かチャレンジしたいですね．

