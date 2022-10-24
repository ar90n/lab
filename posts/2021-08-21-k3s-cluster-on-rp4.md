---
toc: true
layout: post
date: 2021-08-21
categories: [Kubernetes]
title: k3sクラスタ on Raspberry Pi 4
---

## はじめに
Raspberry Pi 4+k3sで構築したクラスタに関するメモです．

## やったこと
* PC(master) + Raspberry Pi 4(worker)という構成でk3sクラスタを構築
* grafana + ptometheus + loki + node-exporter + promtailという構成で監視基盤を構築

## Role関連の設定
クラスタの監視を行うためには適切にサービスディスカバリを行う必要があります．そこで，サービスディスカバリに必要な権限を持つClusterRoleと対応するServiceAccountを定義します．
`/metrics`にアクセスするためには`Role`ではなく`ClusterRole`である必要があるようです．(`monitoring`ネームスペース外にアクセスするから？)

```yml
apiVersion: v1
kind: Namespace
metadata:
  name: monitoring
---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRole
metadata:
  name: monitoring
rules:
- apiGroups: [""]
  resources:
  - nodes
  - nodes/proxy
  - services
  - endpoints
  - pods
  verbs: ["get", "list", "watch"]
- apiGroups:
  - extensions
  resources:
  - ingresses
  verbs: ["get", "list", "watch"]
- nonResourceURLs: ["/metrics"]
  verbs: ["get"]
---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRoleBinding
metadata:
  name: monitoring
roleRef:
  kind: ClusterRole
  name: monitoring
  apiGroup: rbac.authorization.k8s.io
subjects:
- kind: ServiceAccount
  name: monitoring
  namespace: monitoring
---
apiVersion: v1
kind: ServiceAccount
metadata:
  name: monitoring
  namespace: monitoring
```

## promtailの導入　
とりあえず，ホストのログ(`/var/log/*log`)をmasterで稼働しているlokiに送信します．各podのログ収集については中途半端な設定となっています．（多分動かない）
各ログには`hostname`タグにホスト名を設定しています．これは，`spec.nodeName`を環境変数`NODE_NAME`に設定し，起動引数に`--client.external-labels=hostname=$(NODE_NAME)`を追加することで実現しています．

lokiはk3sクラスタ内ではなく，master上のDockerコンテナとしてホストされています．また，promtailはworkerとmaster両ノードにデプロイされます．
そのため，両環境下から`k3s-master`の名前解決を行うため，`dnsPolicy`を`None`として明示的にdnsサーバーを設定しています．
これは，`ClusterFirst`ではクラスタ外にある`k3s-master`の名前解決を行うことができず，`Default`ではworkerとmasterとで共通の舐め解決設定を作り出すことができなかったためです．

```yml
apiVersion: v1
kind: ConfigMap
metadata:
  name: promtail-config
  namespace: monitoring
data:
  config.yaml: |
    server:
      http_listen_port: 9080
      grpc_listen_port: 0
    positions:
      filename: /tmp/positions.yaml
    clients:
      - url: http://k3s-master:3100/loki/api/v1/push
    scrape_configs:
    - job_name: system
      static_configs:
      - targets:
          - localhost
        labels:
          job: varlogs
          __path__: /var/log/*log
    - job_name: kubernetes-pods-app
      kubernetes_sd_configs:
      - role: pod
---
apiVersion: apps/v1
kind: DaemonSet
metadata:
  name: promtail
  namespace: monitoring
spec:
  selector:
    matchLabels:
      app: promtail
  template:
    metadata:
      labels:
        app: promtail
    spec:
      dnsPolicy: None
      dnsConfig:
        nameservers:
          - 10.0.100.1
      serviceAccountName: monitoring
      containers:
      - name: promtail
        image: grafana/promtail:latest
        env:
        - name: TZ
          value: Asia/Tokyo
        - name: NODE_NAME
          valueFrom:
            fieldRef:
              fieldPath: spec.nodeName
        args:
        - --config.file=/etc/promtail/config.yaml 
        - --client.external-labels=hostname=$(NODE_NAME)
        ports:
        - name: webui
          containerPort: 9080
        volumeMounts:
        - name: config-volume
          mountPath: /etc/promtail
        - name: varlog
          mountPath: /var/log
        - name: secret-volume
          mountPath: /var/run/secrets
        - name: run
          mountPath: /run/promtail
      volumes:
      - name: config-volume
        configMap:
          name: promtail-config
      - name: varlog
        hostPath:
          path: /var/log
      - name: secret-volume
        hostPath:
          path: /var/run/secrets
      - name: run
        hostPath:
          path: /run/promtail
```

## node-exporterの導入
`node-exporter`もworkerとmaster両環境にデプロイされます．

```yml
apiVersion: apps/v1
kind: DaemonSet
metadata:
  name: node-exporter
  namespace: monitoring
spec:
  selector:
    matchLabels:
      app: node-exporter
  template:
    metadata:
      labels:
        app: node-exporter
      annotations:
        prometheus.io/scrape: 'true'
        prometheus.io/port: '9100'
        prometheus.io/path: /metrics
    spec:
      dnsPolicy: Default
      serviceAccountName: monitoring
      containers:
      - name: node-exporter
        image: prom/node-exporter:latest
        env:
        - name: TZ
          value: Asia/Tokyo
        - name: NODE_NAME
          valueFrom:
            fieldRef:
              fieldPath: spec.nodeName
        args:
        - --path.procfs=/host/proc
        - --path.sysfs=/host/sys
        - --collector.filesystem.ignored-mount-points
        - ^/(sys|proc|dev|host|etc|rootfs/var/lib/docker/containers|rootfs/var/lib/docker/overlay2|rootfs/run/docker/netns|rootfs/var/lib/docker/aufs)($$|/)
        ports:
        - containerPort: 9100
          name: http
        volumeMounts:
        - name: proc
          mountPath: /host/proc
        - name: sys
          mountPath: /host/sys
        - name: rootfs
          mountPath: /rootfs
      volumes:
      - name: proc
        hostPath:
          path: /proc
      - name: sys
        hostPath:
          path: /sys
      - name: rootfs
        hostPath:
          path: /rootfs
```

## loki，prometheus，grafanaの導入
これらのソフトウェアはDockerコンテナとしてホストされています．`docker-compose.yml`は以下の通りです．

```yml
version: "3.2"

networks:
  monitoring:

services:
  loki:
    image: grafana/loki:latest
    ports:
      - "3100:3100"
    command: -config.file=/etc/loki/local-config.yaml
    volumes:
      - type: bind
        source: ./loki/local-config.yml
        target: /etc/loki/local-config.yaml
    networks:
      - monitoring
    restart: always

  grafana:
    image: grafana/grafana:latest
    ports:
      - "3000:3000"
    volumes:
      - type: bind
        source: ./grafana/provisioning
        target: /etc/grafana/provisioning
      - type: volume
        source: grafana_data
        target: /var/lib/grafana
    networks:
      - monitoring
    restart: always

volumes:
  grafana_data:
```

`local-config.yaml`は以下の通りです．デフォルトから特に変更していなかったと思います．
```yml
auth_enabled: false

server:
  http_listen_port: 3100

ingester:
  lifecycler:
    address: 127.0.0.1
    ring:
      kvstore:
        store: inmemory
      replication_factor: 1
    final_sleep: 0s
  chunk_idle_period: 1h       # Any chunk not receiving new logs in this time will be flushed
  max_chunk_age: 1h           # All chunks will be flushed when they hit this age, default is 1h
  chunk_target_size: 1048576  # Loki will attempt to build chunks up to 1.5MB, flushing first if chunk_idle_period or max_chunk_age is reached first
  chunk_retain_period: 30s    # Must be greater than index read cache TTL if using an index cache (Default index read cache TTL is 5m)
  max_transfer_retries: 0     # Chunk transfers disabled

schema_config:
  configs:
    - from: 2020-10-24
      store: boltdb-shipper
      object_store: filesystem
      schema: v11
      index:
        prefix: index_
        period: 24h

storage_config:
  boltdb_shipper:
    active_index_directory: /loki/boltdb-shipper-active
    cache_location: /loki/boltdb-shipper-cache
    cache_ttl: 24h         # Can be increased for faster performance over longer query periods, uses more disk space
    shared_store: filesystem
  filesystem:
    directory: /loki/chunks

compactor:
  working_directory: /loki/boltdb-shipper-compactor
  shared_store: filesystem

limits_config:
  reject_old_samples: true
  reject_old_samples_max_age: 168h

chunk_store_config:
  max_look_back_period: 0s

table_manager:
  retention_deletes_enabled: false
  retention_period: 0s

ruler:
  storage:
    type: local
    local:
      directory: /loki/rules
  rule_path: /loki/rules-temp
  alertmanager_url: http://localhost:9093
  ring:
    kvstore:
      store: inmemory
  enable_api: true
```

## 参考
* [Lokiとpromtailことはじめ](https://i101330.hatenablog.com/entry/2019/08/18/142339)
* [Kubernetesの全NodeをPrometheusで監視する方法](https://www.tmp1024.com/observe-kubernetes-all-nodes-of-prometheus/)
