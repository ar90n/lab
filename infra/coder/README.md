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

## 注意

ワークスペースはホストの dockerd が作る Docker コンテナで、ユーザーは docker グループに
所属している。したがって **Coder の管理者権限は実質 slime の root 相当**になる。
個人利用に留め、他人にアカウントを配らないこと。

`docker_settings/coder_server_all_in_one/`（2022年）は名前が紛らわしいが中身は
`codercom/code-server`（ブラウザ版 VS Code）で、この Coder v2 とは別製品。
