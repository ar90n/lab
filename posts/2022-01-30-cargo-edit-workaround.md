---
toc: true
layout: post
date: 2022-01-30
categories: [Rust]
title: cargo-editが上手く動かない時の回避策
---

## はじめに
[cargo-edit](https://github.com/killercup/cargo-edit) v0.8.0が上手く動かなかったので，その対処法についてのメモです．

## やったこと
* ssh-agentに秘密鍵を登録
* v0.7.0にダウングレード

## `error authenticating: no auth sock variable; class=Ssh (23)` で`cargo-edit`をインストールすることができない
単純に `cargo install cargo-edit` をすると，以下の様に`ssh-agent`との連携に失敗してエラーとなりました．

```bash
$ cargo install cargo-edit
    Updating crates.io index
error: failed to fetch `https://github.com/rust-lang/crates.io-index`

Caused by:
  failed to authenticate when downloading repository: ssh://git@github.com/rust-lang/crates.io-index

  * attempted ssh-agent authentication, but no usernames succeeded: `git`

  if the git CLI succeeds then `net.git-fetch-with-cli` may help here
  https://doc.rust-lang.org/cargo/reference/config.html#netgit-fetch-with-cli

Caused by:
  error authenticating: no auth sock variable; class=Ssh (23)
```

対処法はエラーメッセージの中に書いてあるので，以下の様にssh-agentの起動と秘密鍵の登録を行います．

```bash
$ eval `ssh-agent -s`
Agent pid 1756
$ ssh-add
Identity added: <path to home>/.ssh/id_rsa (<mail address>)
```

再度，`cargo install cargo-edit` を実行すると無事にインストールできました．

```bash
$ cargo install cargo-edit
    Updating crates.io index
  Downloaded cargo-edit v0.8.0
  Downloaded 1 crate (61.1 KB) in 0.40s
  Installing cargo-edit v0.8.0

...

   Compiling crates-index v0.17.0
   Compiling cargo-edit v0.8.0
    Finished release [optimized] target(s) in 1m 05s
  Installing /usr/local/cargo/bin/cargo-add
  Installing /usr/local/cargo/bin/cargo-rm
  Installing /usr/local/cargo/bin/cargo-set-version
  Installing /usr/local/cargo/bin/cargo-upgrade
   Installed package `cargo-edit v0.8.0` (executables `cargo-add`, `cargo-rm`, `cargo-set-version`, `cargo-upgrade`)
```

## `Command failed due to unhandled error: authentication required but no callback set; class=Ssh (23); code=Auth (-16)` で `cargo add` できない
`cargo add` でクレートを追加しようとすると，以下の様にエラーが発生します．

```bash
$ cargo add anyhow
    Updating 'https://github.com/rust-lang/crates.io-index' index
Command failed due to unhandled error: authentication required but no callback set; class=Ssh (23); code=Auth (-16)
```
[こちら](https://github.com/killercup/cargo-edit/issues/333)や[こちら](https://github.com/killercup/cargo-edit/issues/515)など，同様のエラーに関する報告はちらほら上がっているようです．しかしながら，それらの回避策は私の環境では有効性を発揮しませんでした．
従って，`cargo-edit` のバージョンを `v0.7.0` にダウンロードすることとしました．

```bash
$  cargo install --version 0.7.0 cargo-edit
  Downloaded cargo-edit v0.7.0
  Downloaded 1 crate (57.6 KB) in 0.69s
    Updating crates.io index
  Installing cargo-edit v0.7.0

...

 Replacing /usr/local/cargo/bin/cargo-upgrade
    Removing executable `/usr/local/cargo/bin/cargo-set-version` from previous version cargo-edit v0.8.0
    Replaced package `cargo-edit v0.8.0` with `cargo-edit v0.7.0` (executables `cargo-add`, `cargo-rm`, `cargo-upgrade`)
```

この結果，以下の様に `cargo add` が動作する様になりました．

```bash
$ cargo add anyhow
    Updating 'https://github.com/rust-lng/crates.io-index' index
      Adding anyhow v1.0.53 to dependenciesa
```

## 参考
* [cargo add no longer works with Git using SSH in 0.8.0 #515
](https://github.com/killercup/cargo-edit/issues/515)
* [SSH support #333 ](https://github.com/killercup/cargo-edit/issues/333)
