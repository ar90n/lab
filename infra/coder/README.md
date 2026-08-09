# Coder server (slime)

slime 上でセルフホストしている [Coder](https://coder.com) v2 の構成。

- **coder server**: ホストネイティブ。mise 管理の `coder` バイナリを systemd **user** サービスで常駐
- **PostgreSQL**: Docker の `postgres:17` コンテナ。`127.0.0.1:5432` にのみ公開
- **アクセス URL**: <http://slime.tail38ea2.ts.net:7080>

slime は `sudo` にパスワードが要るため、すべてユーザー権限で完結する構成にしてある。
systemd システムサービスではなく user サービス + `loginctl enable-linger` を使うのはそのため。

Coder 公式は個人/単一サーバー向けにはネイティブバイナリを推奨しており（Docker は
Hosted/Enterprise 向けの選択肢）、この構成はそれに沿っている。PostgreSQL だけを
コンテナにしているのは、sudo なしで `apt install postgresql` ができないため。

## ファイル

| ファイル | 役割 |
|---|---|
| `docker-compose.yml` | PostgreSQL コンテナのみ |
| `coder.service` | systemd user unit。`~/.config/systemd/user/coder.service` からシンボリックリンクする |
| `.env.sample` | 環境変数のテンプレート |
| `.env` | 実際の値（**git 管理外**、chmod 600）。docker compose と systemd の両方から読まれる |

## 初期構築

```sh
cd ~/workspace/lab/infra/coder

# 1. .env を作る
cp .env.sample .env
chmod 600 .env
openssl rand -hex 24                    # 生成した値を POSTGRES_PASSWORD と
                                        # CODER_PG_CONNECTION_URL の両方に埋める

# 2. PostgreSQL を起動
docker compose up -d

# 3. coder server を systemd user サービスとして登録
ln -s "$PWD/coder.service" ~/.config/systemd/user/coder.service
systemctl --user daemon-reload
loginctl enable-linger argon            # ログアウト後もサービスを維持する
systemctl --user enable --now coder

# 4. 起動確認
tail -f ~/.local/state/coder/coder.log
curl -sS http://localhost:7080/healthz
```

`systemctl --user enable` がシンボリックリンクを拒否する場合は、先に
`systemctl --user link "$PWD/coder.service"` を実行してから enable する。

## 初回セットアップ（対話が必要）

ブラウザで <http://slime.tail38ea2.ts.net:7080> を開き、最初の管理者アカウントを作る。
CLI からでもよい:

```sh
coder login http://slime.tail38ea2.ts.net:7080
```

その後、Web UI の Templates から Docker テンプレートを作成する。Terraform の事前導入は
不要で、Coder が初回ビルド時に Terraform 1.14.5 を自動取得する。

## 日常の操作

```sh
systemctl --user status coder            # 状態
systemctl --user restart coder           # 再起動
tail -f ~/.local/state/coder/coder.log   # ログ追尾

docker compose ps                        # DB の状態
docker compose logs -f database          # DB のログ
```

### ログについて

このホストの `/var/log/journal` は ACL が `adm` グループにしか読み取りを許しておらず、
user journal ファイルも生成されないため、`journalctl --user -u coder` では自分の
サービスのログが読めない。そのため unit で `StandardOutput=append:` を指定して
`~/.local/state/coder/coder.log` へ直接書き出している。

ローテーションは設定していないので、肥大化したら手で切り詰める:

```sh
systemctl --user stop coder
: > ~/.local/state/coder/coder.log
systemctl --user start coder
```

`sudo` が使える状況なら `usermod -aG adm argon` して journald に戻す方が筋は良い。

## バージョン更新

`coder` は mise 管理なので unit の編集は不要:

```sh
mise use -g coder@<新バージョン>
systemctl --user restart coder
```

CLI と server は同一バイナリなので、これで両方が上がる。
Coder は mainline (2.36.x) と stable (2.35.x) の 2 系統をリリースしている。
セルフホストなら stable 系を選ぶ。

## バックアップ

状態はすべて PostgreSQL にある:

```sh
docker exec coder-database pg_dump -U coder coder | gzip > coder-$(date +%F).sql.gz
```

`~/.config/coderv2/` にはサーバーのローカル状態（鍵など）が入る。DB と併せて保全する。

## 家庭用チューニング

`.env` の後半で、単一ユーザーの家庭利用に不要な既定を切ってある。詳細は `.env.sample`
のコメントを参照。

| 設定 | 既定 | 変更後 | 理由 |
|---|---|---|---|
| `CODER_SESSION_DURATION` | 24h | 720h | 毎日ログインしなくて済む |
| `CODER_DEFAULT_OAUTH_REFRESH_LIFETIME` | 720h | 1440h | 上と衝突するため引き上げ（下記） |
| `CODER_TELEMETRY_ENABLE` | true | false | Coder 社への利用データ送信を停止 |
| `CODER_AI_GATEWAY_ENABLED` | true | false | aibridge は not_entitled、AI キーも未設定 |
| `CODER_HIDE_AI_TASKS` | false | true | ダッシュボードの整理 |
| `CODER_OAUTH2_GITHUB_DEFAULT_PROVIDER_ENABLE` | true | false | パスワード認証のみ使うのでサインイン経路を削減 |

**ハマりどころ**: `CODER_SESSION_DURATION` は `CODER_DEFAULT_OAUTH_REFRESH_LIFETIME`
より**厳密に短く**なければならず、両方の既定が 720h。session duration だけを 720h に
すると起動時に

```
error: default OAuth refresh lifetime (720h0m0s) must be strictly greater than
session duration (720h0m0s)
```

で失敗し、`Restart=on-failure` により再起動ループに入る。`systemctl --user is-active`
は瞬間的に `active` を返すので、状態確認は healthz か `NRestarts` を見ること。

**残した既定**:

- `CODER_UPDATE_CHECK` (true) — セキュリティ更新に気づくため残す。バージョン管理自体は mise で行う
- `CODER_DERP_SERVER_ENABLE` (true) — ワークスペース接続のリレーに**必須**
- `CODER_EXTERNAL_AUTH_GITHUB_DEFAULT_PROVIDER_ENABLE` (true) — ワークスペースから private repo を clone するのに使う
- `CODER_NOTIFICATIONS_INBOX_ENABLED` (true) — アプリ内通知。SMTP smarthost 未設定なので外部送信は起きない

テレメトリを切った副作用として、起動時に
`telemetry disabled, unable to notify of security issues` という warn が毎回出る。
仕様どおりの挙動で、異常ではない。

## ライセンスについて

ライセンス無し（trial 未開始）で問題ない。`/api/v2/entitlements` で確認したところ
`user_limit` は `not_entitled` かつ `actual: 0` で、**ユーザー数の上限は課されない**。
期限切れの概念も無い。

ライセンスで開くのは `audit_log` / `scim` / `template_rbac` / `custom_roles` /
`multiple_organizations` / `high_availability` / `external_provisioner_daemons` /
`workspace_proxy` / `appearance` などの組織向け機能のみ。ワークスペース作成、
テンプレート、`coder ssh`、ポートフォワード、VS Code Remote、Coder Desktop の
VPN 接続はすべて無償の範囲。

## 注意

ワークスペースはホストの dockerd が作る Docker コンテナで、ユーザーは docker グループに
所属している。したがって **Coder の管理者権限は実質 slime の root 相当**になる。
個人利用に留め、他人にアカウントを配らないこと。

`docker_settings/coder_server_all_in_one/`（2022年）は名前が紛らわしいが中身は
`codercom/code-server`（ブラウザ版 VS Code）で、この Coder v2 とは別製品。

---

## ワークスペーステンプレート (`templates/`)

`~/workspace` 配下のプロジェクトごとにワークスペースを作っていく運用を想定した
Docker テンプレートが2つある。

```
templates/
├── common/build/Dockerfile   # 単一のソース (直接 push には使わない)
├── general/                  # 汎用。BASE_IMAGE=ubuntu:24.04
├── cuda/                     # CUDA 開発用。BASE_IMAGE=nvidia/cuda:12.9.2-devel-ubuntu24.04
└── sync-dockerfile.sh        # common/build/Dockerfile を general/ と cuda/ に複製する
```

### なぜ Dockerfile が3箇所にあるのか

`coder templates push --directory <dir>` は指定したディレクトリの中身しか provisioner に
送らない。`../common/build` のようにディレクトリの外を Terraform から参照すると、
サーバー側には存在せずビルドに失敗する（`dockerfile not found at path: ...` エラーで
一度実際にハマった）。そのため `common/build/Dockerfile` を単一のソースとして持ち、
**push 前に必ず `./sync-dockerfile.sh` を実行**して `general/build/` と `cuda/build/` に
複製する。Dockerfile を編集したら sync を忘れないこと。

### 特徴

- **ワークスペース = privileged な Docker コンテナ**。ホストの `~/workspace` を
  `/home/coder/workspace` に bind mount するので、プロジェクトごとに git clone
  し直す必要がない
- **GPU**: `no-cgroups = true`（`/etc/nvidia-container-runtime/config.toml`）という
  ホスト側の設定により、素の `--gpus all` だけでは `Failed to initialize NVML` になる。
  `privileged = true` + `runtime = "nvidia"` の組み合わせで回避している
- **mise の共有キャッシュ**: `~/.local/share/mise`・`~/.cache/mise`・`~/.cargo`・
  `~/.rustup` を `/home/argon/.cache/coder-workspaces/` 配下のホストディレクトリへ
  bind mount している。rust/go/python/node/gh/pixi は起動時の `startup_script` で
  `mise use -g` するが、**最初のワークスペースがインストールした時点で以降の
  全ワークスペースに即座に反映される**（ストレージも1コピーだけ）。cargo 製 CLI
  ツール（ripgrep 等）も同じ仕組みで共有 `CARGO_HOME` に乗る
- **イメージに焼き込むもの**: apt パッケージ全般（ネットワーク診断・プロファイリング系
  含む）、mise 本体、starship/claude/opencode。これらは bind mount 対象と重ならない
  ので素直に image レイヤーとして全ワークスペースに共有される
- `coder` ユーザーは **uid 501**（ホストの `argon` と同じ）で作成している。bind mount
  した共有ディレクトリを権限エラーなく読み書きするため
- **X11**: slime では Xpra がバーチャルディスプレイ `:20000` を常時起動している
  （`Xvfb-for-Xpra ... :20000`、`xhost +` 済みで認証不要）。`~/workspace/lab/sandbox/xpra_container`
  と同じ `/tmp/.X11-unix` を bind mount し、`DISPLAY=:20000` を設定しているので、
  RViz のような GUI アプリをワークスペースから起動して確認できる。X11 クライアント
  ライブラリ（`x11-utils` 等）はイメージに焼き込んでおらず、必要なプロジェクトで
  都度 `sudo apt-get install` する想定（GUI を使わないワークスペースまでイメージを
  太らせないため）

### ハマった点（実装時に踏んだ罠）

1. **`../common/build` 参照エラー** — 上記の通り。`build/` はテンプレート自身の
   ディレクトリ配下に置く必要がある
2. **`.bashrc` が公式サンプルの `/etc/skel` コピー処理で消される** — Coder 公式の
   docker テンプレート例は `startup_script` の冒頭で
   ```sh
   if [ ! -f ~/.init_done ]; then
     cp -rT /etc/skel ~
     touch ~/.init_done
   fi
   ```
   をやるが、これは「素の base image を前提に home を初期化する」ためのもの。
   うちの Dockerfile は image ビルド時に `~/.bashrc` へ `mise activate`/`starship init`
   を追記済みなので、この処理をそのまま使うと**焼き込んだ内容が上書きされて消える**。
   このテンプレートでは丸ごと削除している
3. **`.bashrc` は非対話シェルでは実行されない** — Ubuntu 既定の `~/.bashrc` は冒頭に
   ```sh
   case $- in
       *i*) ;;
         *) return;;
   esac
   ```
   というガードがあり、`docker exec`/`coder ssh -- <cmd>` のような非対話実行では
   即座に return して以降の内容（mise activate、starship init、opencode の PATH 追記）
   が一切実行されない。VS Code Remote-SSH の統合ターミナルのような対話シェルでは
   問題にならないが、非対話実行に依存する自動化スクリプトは壊れる。
   このテンプレートでは **PATH を shell rc に頼らず Dockerfile の `ENV PATH` で
   静的に設定**することで回避している（`~/.local/share/mise/shims` を含める。
   これは mise の「shims 方式」で、activate 不要でどのコンテキストでも解決できる）
4. **CUDA イメージのタグ不存在** — `nvidia/cuda:12.4.1-devel-ubuntu24.04` は存在しない
   （Ubuntu 24.04 ベースは 12.5.1 以降からしか出ていない）。ビルドが
   `not found` で即座に失敗する。ホストのドライバ（575.64.03、`nvidia-smi` の
   `CUDA Version` 表示で確認）が対応する上限は CUDA 12.9 なので `12.9.2-devel-ubuntu24.04`
   を採用した。ドライバを跨いだ新しい CUDA を指定すると `nvcc` はビルドできても
   実行時にドライバ非対応で失敗するので、変更する場合は `nvidia-smi` の表示と
   `hub.docker.com/r/nvidia/cuda/tags` を突き合わせること
5. **`docker_container.env` は `coder ssh` セッションに伝播しない** — X11 用に
   `DISPLAY=:20000` を `docker_container.workspace.env` にだけ設定したところ、
   `docker exec` では見えるのに `coder ssh` からは空だった。`working_dir`/`dir`
   と全く同じ split で、Coder agent の SSH セッションは Docker レベルの `env` を
   引き継がない。`coder_agent.main.env`（`GIT_AUTHOR_NAME` 等と同じブロック）に
   **も** 設定して解決した。「`docker_container` 側の設定は docker exec と
   ENTRYPOINT にしか効かず、`coder ssh` に効かせたいものは必ず `coder_agent` 側にも
   要る」という教訓は `dir` のときと合わせて2回目なので、今後何か新しい変数を
   ワークスペースに持ち込むときはこの split を最初に疑うこと

### `workdir` パラメータ — 作業ディレクトリの指定

`coder create` はコンテナを起動するだけで、git clone も IDE 起動も何もしない。
`~/workspace` 全体を bind mount しているので、SSH した直後の作業ディレクトリは
何も指定しなければ `/home/coder`（ホーム、プロジェクトのファイルは無い）になる。

`workdir` パラメータ（`coder_parameter`、**コンテナ内の絶対パス**）を
`docker_container.workspace` の `working_dir` 属性（`docker run -w` 相当）へ
そのまま渡している:

```sh
coder create myproj --template home-docker --parameter workdir=/home/coder/workspace/lab --yes
coder ssh myproj                          # -> /home/coder/workspace/lab に着地
docker exec <container> pwd               # -> 同じく /home/coder/workspace/lab

# 指定を省略してデフォルト (/home/coder/workspace) を使う場合は
# --use-parameter-defaults が必要 (無いと非対話環境ではプロンプトで
# 止まってエラーになる。--yes だけでは default 値は自動採用されない)
coder create myproj --template home-docker --use-parameter-defaults --yes
```

`docker run -w` と同じ仕組みなので、`coder ssh` のような対話シェルに限らず
`docker exec` などの非対話実行でも一律に効く（`.bashrc` に `cd` を仕込むような
シェル依存の方式より確実）。Dockerfile 側の `WORKDIR /home/coder` はイメージの
ビルド時に焼き込まれる別物で、`working_dir` 属性がそれを実行時に上書きする形になる。
ワークスペース名とプロジェクトディレクトリ名を一致させる必要はなく、1プロジェクトに
複数ワークスペースを作る場合もそれぞれ独立して指定できる。`mutable = true` なので
後から `coder update myproj --parameter workdir=other` で変更もできる。

**なぜ相対パスではなくフルパスなのか**: 当初 `~/workspace` からの相対パス
（`workdir=lab`）にしていたが、`working_dir` という名前自体が `docker run -w`
そのものの発想である以上、実体も素直に絶対パスにすべきと判断して変更した。
相対パス方式だと「フルパスっぽい値を渡すと `/home/coder/workspace//home/coder/...`
のように壊れる」という踏みやすい罠もあった。

**注意**: `workdir` が実在しないディレクトリを指すとコンテナの起動に失敗する。

### 使い方

```sh
cd ~/workspace/lab/infra/coder/templates
./sync-dockerfile.sh
cd general && coder templates push home-docker --directory . --yes
cd ../cuda && coder templates push home-docker-cuda --directory . --yes

# プロジェクトごとにワークスペースを作る
coder create <project-name> --template home-docker --parameter workdir=/home/coder/workspace/<プロジェクト名> --yes
coder ssh <project-name>    # working_dir で指定した場所に着地する

coder list
coder stop <project-name>
coder delete <project-name> --yes
```

CUDA (`nvcc`) が要る場合だけ `--template home-docker-cuda` を使う。それ以外の
言語ランタイム・CLI ツールは両テンプレートで共通（共有キャッシュも共通）。
