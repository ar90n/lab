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

# ワークスペース名とは独立に、コンテナ内のどこで作業するかを指定する
# (docker run -w 相当。docker_container.workspace.working_dir にそのまま渡す)。
# 1つのプロジェクトに複数ワークスペースを使うこともあるため、ワークスペース名との
# 一致は前提にしない。
data "coder_parameter" "workdir" {
  name         = "workdir"
  display_name = "Working directory"
  description  = "コンテナ内の絶対パス (例: /home/coder/workspace/lab)"
  type         = "string"
  default      = "/home/coder/workspace"
  mutable      = true
}

resource "coder_agent" "main" {
  arch = data.coder_provisioner.me.arch
  os   = "linux"

  # coder ssh のセッションはこの dir 属性でしか制御できない。
  # docker_container.workspace.working_dir (docker run -w 相当) は Docker の
  # ENTRYPOINT / docker exec のデフォルトを決めるだけで、Coder agent 自身の
  # SSH セッション生成ロジックはこれを見ておらず常に $HOME に落ちる
  # (docker exec では working_dir が効くように見えたため誤って working_dir
  # だけで足りると判断していたが、実際に coder ssh で検証したところ
  # $HOME に固定されることが分かった)。dir は deprecated だが、これ以外に
  # coder ssh の着地先を制御する手段が無いため使う。
  dir = data.coder_parameter.workdir.value

  startup_script = <<-EOT
    set -e

    # ~/.config/gh・~/.claude/plugins・~/.local/share/opencode/auth.json 等を
    # bind mount すると、Docker がマウントポイントの親ディレクトリ (~/.config、
    # ~/.claude、~/.local/share/opencode) を root 所有で自動作成してしまい、
    # coder ユーザーがそこに新規ファイルを作れなくなる (mise use -g が
    # Permission denied で失敗する等)。起動のたびに直す。
    sudo chown coder:coder ~/.config ~/.claude ~/.local/share/opencode

    # --- herdr (Mac 側の herdr-mirror からこのワークスペースを見るために常駐させる) ---
    # 重い mise/cargo のインストールより先に済ませる。初回作成時は下の cargo install が
    # 数分かかるため、後ろに置くとその間ずっとサイドバーに現れない。

    # settings.json は seed (初回コピー) 方式。理由は下の volumes のコメント参照。
    # 判定は -f (存在) ではなく -s (存在かつ非空) で行う: 以前 bind mount していた
    # パスには mount を外した後も Docker が作った 0 バイトのマウントポイントが
    # 残るため、-f だと「既にある」と誤判定して seed をスキップし、空 JSON を
    # 掴んだ claude/herdr がパースエラーで落ちる (実際に踏んだ)。
    if [ ! -s ~/.claude/settings.json ] && [ -s /opt/claude-host/settings.json ]; then
      # 残骸は root 所有の 0 バイトファイルなので、上書き cp は Permission denied に
      # なる。先に消してから置く (~/.claude 自体は直前の chown で coder 所有なので
      # ファイルの削除・再作成は通る)。
      rm -f ~/.claude/settings.json \
        && cp /opt/claude-host/settings.json ~/.claude/settings.json \
        || echo "herdr: settings.json seed failed" >&2
    fi

    # claude の状態 (working / blocked / idle) を herdr に報告する SessionStart フック。
    # ~/.claude/hooks/ は mount していないのでコンテナ内に閉じている。
    # 以降 herdr 関連は失敗しても mise/cargo のセットアップを巻き添えにしないよう
    # 非致命化する (set -e の下で素通しにすると開発環境ごと構築されなくなる)。
    herdr integration install claude || echo "herdr: integration install failed" >&2

    # ヘッドレスサーバーを常駐させる。setsid で startup_script のプロセスグループから
    # 切り離さないと、startup_script の終了に巻き込まれてサーバーごと落ちる。
    # 稼働判定は終了コードではなく出力で行う: `herdr status server` はサーバーが
    # 止まっていても "status: not running" を出して exit 0 を返すため、
    # `if ! herdr status server` は常に偽になり起動処理が丸ごとスキップされる
    # (実機で両方の状態の exit code が 0 であることを確認済み)。
    if ! herdr status server 2>/dev/null | grep -q "^status: running"; then
      setsid nohup herdr server >~/.herdr-server.log 2>&1 &
      for _ in $(seq 30); do
        herdr status server 2>/dev/null | grep -q "^status: running" && break
        sleep 1
      done
    fi

    # mirror の remote-new-tab / remote-split-* 等は「ミラー pane 上で呼ぶ」ことが前提で、
    # リモートに pane が1つも無いとローカル動作に degrade する (herdr-mirror README:
    # "outside a mirror they degrade to the plain local action")。つまり足場が無いと
    # Mac 側から永久にリモートのタブを作れない。ブートストラップ用に必ず1つ用意する。
    if ! herdr workspace list 2>/dev/null | grep -q workspace_id; then
      herdr workspace create \
        --cwd "${data.coder_parameter.workdir.value}" \
        --label "${data.coder_workspace.me.name}" \
        --no-focus >/dev/null || echo "herdr: bootstrap workspace create failed" >&2
    fi

    # Dockerfile のビルド時に ~/.bashrc へ mise/starship の activate 行を
    # 追記済みなので、公式サンプルにある /etc/skel コピー処理は入れない
    # (入れると焼き込んだ .bashrc の中身が消えてしまう)。

    # rust/go/python/node は共有キャッシュ (bind mount) にインストールされるため、
    # このワークスペースが最初に実行した時だけ実質的なダウンロード/ビルドが発生し、
    # 以降の全ワークスペースでは即座に使えるようになる。
    export PATH="$HOME/.local/bin:$HOME/.cargo/bin:$PATH"
    mise use -g rust@stable go@latest python@3.13 node@lts gh@latest pixi@latest

    cargo install --locked \
      ripgrep fd-find bat eza zoxide du-dust bottom tokei hyperfine \
      cargo-edit cargo-watch sccache flamegraph py-spy
  EOT

  # GIT_AUTHOR_NAME 等は意図的に設定しない。~/.ssh・~/.gitconfig・~/.config/gh を
  # ホストからそのまま bind mount しているので、git はホストと同じ ~/.gitconfig の
  # [user] セクションを見る。GIT_AUTHOR_NAME 等の環境変数は git の設定ファイルより
  # 優先されるため、ここで設定すると mount した .gitconfig が無視されてしまう。
  env = {
    # docker_container 側の env は docker exec には効くが coder ssh セッションには
    # 伝播しない (dir/working_dir と同じ split)。coder ssh からも X11 に繋げるため
    # ここにも設定する。
    DISPLAY = ":20000"
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

  # docker run -w 相当。ENTRYPOINT や docker exec のデフォルトになる
  # (coder ssh の着地先は別 = coder_agent.main.dir で制御している)。
  # 実在しないディレクトリを指すとコンテナ起動に失敗する点に注意
  # (workdir パラメータの説明に明記している)。
  working_dir = data.coder_parameter.workdir.value

  # GPU (no-cgroups=true 環境での既知の回避策): privileged なしだと
  # nvidia ランタイムの cgroup デバイス許可が入らず NVML 初期化に失敗する。
  privileged = true
  runtime    = "nvidia"

  env = [
    "CODER_AGENT_TOKEN=${coder_agent.main.token}",
    "NVIDIA_VISIBLE_DEVICES=all",
    "NVIDIA_DRIVER_CAPABILITIES=all",
    # slime 上で常時稼働している Xpra のバーチャルディスプレイ (Xvfb :20000)。
    # ~/workspace/lab/sandbox/xpra_container が使っているのと同じディスプレイ番号。
    "DISPLAY=:20000",
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

  # GUI アプリを DISPLAY=:20000 (Xpra) に表示するための X11 ソケット。
  # ~/workspace/lab/sandbox/xpra_container/docker-compose.yml と同じマウント。
  volumes {
    host_path      = "/tmp/.X11-unix"
    container_path = "/tmp/.X11-unix"
    read_only      = false
  }

  # ホストの git/GitHub 認証情報をそのまま使う。ワークスペースは docker.sock 経由で
  # 既にホスト root 相当の信頼を持つ構成 (README 参照) なので、実鍵を渡すことによる
  # 追加のリスクは実質増えない。Coder 独自の SSH 鍵自動生成や external-auth の
  # OAuth フローを使わずに済む。
  volumes {
    host_path      = "/home/argon/.ssh"
    container_path = "/home/coder/.ssh"
    read_only      = false
  }
  volumes {
    host_path      = "/home/argon/.gitconfig"
    container_path = "/home/coder/.gitconfig"
    read_only      = false
  }
  volumes {
    host_path      = "/home/argon/.config/gh"
    container_path = "/home/coder/.config/gh"
    read_only      = false
  }

  # claude の plugins だけホストと実体共有する。可変ファイル (.claude.json /
  # .claude/.credentials.json / settings.json) は意図的に共有しない:
  #   - 認証情報は claude がトークン更新のたびに rename で書き換えるため、
  #     Docker の単一ファイル bind mount では古い inode を掴んだまま同期が壊れる
  #     (ホストとコンテナで inode 番号が一致しないことを実機で確認済み)。
  #   - settings.json は `herdr integration install claude` が
  #     /home/coder/.claude/hooks/... というコンテナ固有の絶対パスを書き込むため、
  #     共有するとホスト側の settings.json にそのパスが漏れ、ホストの claude が
  #     存在しないフックを毎回叩くようになる (実際に汚染していたのを修復済み)。
  # settings.json は「共有」ではなく「初回コピーの種 (seed)」として読み取り専用で
  # 別パスに見せ、startup_script が初回だけ ~/.claude/settings.json へコピーする。
  # これで permissions/enabledPlugins 等はホストから引き継ぎつつ、以降の書き換えは
  # ワークスペース内に閉じる。認証は各ワークスペースで初回だけ `claude /login` する。
  volumes {
    host_path      = "/home/argon/.claude/settings.json"
    container_path = "/opt/claude-host/settings.json"
    read_only      = true
  }
  volumes {
    host_path      = "/home/argon/.claude/plugins"
    container_path = "/home/coder/.claude/plugins"
    read_only      = false
  }

  # opencode の設定・skills (node_modules 込み) と認証をホストと共有する。
  # skill が JS ヘルパースクリプトに依存する場合があるため、skills/ だけでなく
  # ディレクトリ全体を bind mount する (ディスクの二重消費にはならない)。
  volumes {
    host_path      = "/home/argon/.config/opencode"
    container_path = "/home/coder/.config/opencode"
    read_only      = false
  }
  volumes {
    host_path      = "/home/argon/.local/share/opencode/auth.json"
    container_path = "/home/coder/.local/share/opencode/auth.json"
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
