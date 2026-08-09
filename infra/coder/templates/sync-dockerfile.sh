#!/usr/bin/env bash
# Coder は `coder templates push --directory <dir>` で指定したディレクトリの
# 中身しか provisioner に送らない（ディレクトリの外を Terraform から参照できない）。
# そのため common/build/Dockerfile を単一のソースとして持ち、
# push 前に各テンプレートディレクトリへ複製する。
set -euo pipefail
cd "$(dirname "$0")"
for tpl in general cuda; do
  mkdir -p "$tpl/build"
  cp common/build/Dockerfile "$tpl/build/Dockerfile"
done
echo "synced common/build/Dockerfile -> general/build/, cuda/build/"
