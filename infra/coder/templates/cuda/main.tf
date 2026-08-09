terraform {
  required_providers {
    coder = {
      source = "coder/coder"
    }
    docker = {
      source = "kreuzwerker/docker"
    }
  }
}

variable "docker_socket" {
  default     = ""
  description = "(Optional) Docker socket URI"
  type        = string
}

provider "docker" {
  host = var.docker_socket != "" ? var.docker_socket : null
}

data "coder_provisioner" "me" {}
data "coder_workspace" "me" {}
data "coder_workspace_owner" "me" {}

# ワークスペース名とは独立に、~/workspace 以下のどのディレクトリで作業するかを指定する。
# 1つのプロジェクトに複数ワークスペースを使うこともあるため、ワークスペース名との
# 一致は前提にしない (Docker の WORKDIR に相当する発想)。
data "coder_parameter" "workdir" {
  name         = "workdir"
  display_name = "Working directory"
  description  = "~/workspace 以下の相対パス (例: lab)。空なら ~/workspace 直下"
  type         = "string"
  default      = ""
  mutable      = true
}

resource "coder_agent" "main" {
  arch = data.coder_provisioner.me.arch
  os   = "linux"

  startup_script = <<-EOT
    set -e

    # Dockerfile のビルド時に ~/.bashrc へ mise/starship の activate 行を
    # 追記済みなので、公式サンプルにある /etc/skel コピー処理は入れない
    # (入れると焼き込んだ .bashrc の中身が消えてしまう)。

    # 作業ディレクトリ (workdir パラメータ) は docker_container.workspace の
    # working_dir 属性 (docker run -w 相当) で設定している。対話シェル限定の
    # .bashrc ハックより、コンテナのプロセス全体に一律で効くこちらが正しい。

    # rust/go/python/node は共有キャッシュ (bind mount) にインストールされるため、
    # このワークスペースが最初に実行した時だけ実質的なダウンロード/ビルドが発生し、
    # 以降の全ワークスペースでは即座に使えるようになる。
    export PATH="$HOME/.local/bin:$HOME/.cargo/bin:$PATH"
    mise use -g rust@stable go@latest python@3.13 node@lts gh@latest pixi@latest

    cargo install --locked \
      ripgrep fd-find bat eza zoxide du-dust bottom tokei hyperfine \
      cargo-edit cargo-watch sccache flamegraph py-spy
  EOT

  env = {
    GIT_AUTHOR_NAME     = coalesce(data.coder_workspace_owner.me.full_name, data.coder_workspace_owner.me.name)
    GIT_AUTHOR_EMAIL    = "${data.coder_workspace_owner.me.email}"
    GIT_COMMITTER_NAME  = coalesce(data.coder_workspace_owner.me.full_name, data.coder_workspace_owner.me.name)
    GIT_COMMITTER_EMAIL = "${data.coder_workspace_owner.me.email}"
  }

  metadata {
    display_name = "CPU Usage"
    key          = "0_cpu_usage"
    script       = "coder stat cpu"
    interval     = 10
    timeout      = 1
  }

  metadata {
    display_name = "RAM Usage"
    key          = "1_ram_usage"
    script       = "coder stat mem"
    interval     = 10
    timeout      = 1
  }

  metadata {
    display_name = "Home Disk"
    key          = "2_home_disk"
    script       = "coder stat disk --path $${HOME}"
    interval     = 60
    timeout      = 1
  }

  metadata {
    display_name = "GPU"
    key          = "3_gpu"
    script       = "nvidia-smi --query-gpu=name,memory.used,memory.total --format=csv,noheader 2>/dev/null || echo 'n/a'"
    interval     = 30
    timeout      = 5
  }

  metadata {
    display_name = "CUDA (nvcc)"
    key          = "4_nvcc"
    script       = "nvcc --version 2>/dev/null | grep release || echo 'n/a'"
    interval     = 60
    timeout      = 5
  }
}

resource "docker_image" "main" {
  name = "coder-home-docker-cuda-${data.coder_workspace.me.id}"
  build {
    context    = "${path.module}/build"
    dockerfile = "Dockerfile"
    build_args = {
      BASE_IMAGE = "nvidia/cuda:12.9.2-devel-ubuntu24.04"
    }
  }
  triggers = {
    dir_sha1 = sha1(join("", [for f in fileset("${path.module}/build", "**") : filesha1("${path.module}/build/${f}")]))
  }
}

resource "docker_volume" "home_volume" {
  name = "coder-${data.coder_workspace.me.id}-home"
  lifecycle {
    ignore_changes = all
  }
  labels {
    label = "coder.owner"
    value = data.coder_workspace_owner.me.name
  }
  labels {
    label = "coder.owner_id"
    value = data.coder_workspace_owner.me.id
  }
  labels {
    label = "coder.workspace_id"
    value = data.coder_workspace.me.id
  }
  labels {
    label = "coder.workspace_name_at_creation"
    value = data.coder_workspace.me.name
  }
}

resource "docker_container" "workspace" {
  count      = data.coder_workspace.me.start_count
  image      = docker_image.main.name
  name       = "coder-${data.coder_workspace_owner.me.name}-${lower(data.coder_workspace.me.name)}"
  hostname   = data.coder_workspace.me.name
  entrypoint = ["sh", "-c", replace(coder_agent.main.init_script, "/localhost|127\\.0\\.0\\.1/", "host.docker.internal")]

  # docker run -w 相当。workdir パラメータ (~/workspace 以下の相対パス) が空なら
  # ~/workspace 直下になる。対話シェルに限らずコンテナのプロセス全体に一律で効く。
  # 値がホスト側に実在しないディレクトリを指す場合、パスが存在せず
  # コンテナ起動に失敗する点に注意 (workdir パラメータの説明に明記している)。
  working_dir = "/home/coder/workspace/${data.coder_parameter.workdir.value}"

  # GPU (no-cgroups=true 環境での既知の回避策): privileged なしだと
  # nvidia ランタイムの cgroup デバイス許可が入らず NVML 初期化に失敗する。
  privileged = true
  runtime    = "nvidia"

  env = [
    "CODER_AGENT_TOKEN=${coder_agent.main.token}",
    "NVIDIA_VISIBLE_DEVICES=all",
    "NVIDIA_DRIVER_CAPABILITIES=all",
  ]

  host {
    host = "host.docker.internal"
    ip   = "host-gateway"
  }

  volumes {
    container_path = "/home/coder"
    volume_name     = docker_volume.home_volume.name
    read_only       = false
  }

  # ホストの ~/workspace をそのまま見せる。プロジェクトごとに git clone し直さない。
  volumes {
    host_path      = "/home/argon/workspace"
    container_path = "/home/coder/workspace"
    read_only      = false
  }

  # mise/cargo/rustup のインストール先を全ワークスペースで共有する。
  volumes {
    host_path      = "/home/argon/.cache/coder-workspaces/mise-data"
    container_path = "/home/coder/.local/share/mise"
    read_only      = false
  }
  volumes {
    host_path      = "/home/argon/.cache/coder-workspaces/mise-cache"
    container_path = "/home/coder/.cache/mise"
    read_only      = false
  }
  volumes {
    host_path      = "/home/argon/.cache/coder-workspaces/cargo"
    container_path = "/home/coder/.cargo"
    read_only      = false
  }
  volumes {
    host_path      = "/home/argon/.cache/coder-workspaces/rustup"
    container_path = "/home/coder/.rustup"
    read_only      = false
  }

  labels {
    label = "coder.owner"
    value = data.coder_workspace_owner.me.name
  }
  labels {
    label = "coder.owner_id"
    value = data.coder_workspace_owner.me.id
  }
  labels {
    label = "coder.workspace_id"
    value = data.coder_workspace.me.id
  }
  labels {
    label = "coder.workspace_name"
    value = data.coder_workspace.me.name
  }
}
