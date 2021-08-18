#!/usr/bin/sh
kubectl label node/k3s-worker0.local --overwrite node-role.kubernetes.io/worker=true
kubectl label node/k3s-worker1.local --overwrite node-role.kubernetes.io/worker=true
kubectl label node/k3s-worker2.local --overwrite node-role.kubernetes.io/worker=true
kubectl label node/k3s-worker3.local --overwrite node-role.kubernetes.io/worker=true
