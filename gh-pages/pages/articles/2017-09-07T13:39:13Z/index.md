---
title: Ubuntu on aarch64でflow
date: "2017-09-07T13:39:13Z"
layout: post
path: "/blog/2017-09-07T13:39:13Z/"
category:
- "javascript"
description: "Ubuntu on aarch64 でflowを使うメモ．"
---
```bash
$sudo apt-get install opam
$opam init --comp 4.03.0
$opam update
$opam install depext
$opam depext --install flowtype
```

でいける．
