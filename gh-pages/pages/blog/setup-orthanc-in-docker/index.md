---
title: DockerでOrthancをさくっと立ち上げる
date: "2017-04-25T17:52:30Z"
lang: "ja"
path: "setup-orthanc-in-docker"
category:
- "Medical"
- "Docker"
description: "家でもPACSの勉強をするためローカルに立ち上げてみました．"
cover: "medical"
---
DockerでOrthancをさくっと立ち上げたメモです．

### Orthancの起動
[Orthanc for Docker](http://book.orthanc-server.com/users/docker.html)からコピペです．ボリュームのマウント元だけNASに変更しました．
```bash
$ docker run --name orthanc-server -d --restart=always  -p 4242:4242 -p 8042:8042 -v /Volumes/PACS/orthanc_db/:/var/lib/orthanc/db/ jodogne/orthanc
```
