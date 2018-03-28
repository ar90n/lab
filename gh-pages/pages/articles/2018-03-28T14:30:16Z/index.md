---
title: サイトジェネレータをReact Staticに変更する2
date: "2018-03-28T14:30:16Z"
layout: post
path: "/blog/2018-03-28T14:30:16Z/"
category: "React"
description: "markdownのインポートとシンタックスハイライトに対応"
---
まずはデータのインポートから．

## やったこと

1. gray-matter, remark, remark-reactを導入してmarkdownのインポートに対応
1. remark-react-lowlightを導入してシンタックスハイライトに対応　

## markdownのインポート対応

## シンタックスハイライトの対応

## 所感
static.config.jsでデータを整形さえすれば後はReact Staticがパッケージングしてくれるので非常に楽だった．
とりあえずデータのインポートはできそうなので，次回からはUI周りを作っていく．
