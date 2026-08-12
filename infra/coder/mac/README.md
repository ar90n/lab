# Mac 側: Coder ワークスペースを herdr-mirror で集約する

slime の Coder ワークスペースは、テンプレート側で herdr が自動常駐するように
なっている（`../README.md` の「herdr」の項）。ここに置くのは **Mac 側だけで
必要になるもの**。

| ファイル | 置き先 | 役割 |
|---|---|---|
| `coder-herdr-sync` | `~/.local/bin/` | `coder list` から `hosts.toml` を生成する |
| `pane-url` | `~/.local/bin/` | pane に出た URL を折り返しを繋いで取り出す |
| `herdr-config-snippet.toml` | `~/.config/herdr/config.toml` に追記 | `prefix+c` をミラー用に差し替える |

`~/.config/herdr-mirror/hosts.toml` 自体は `coder-herdr-sync` が生成するので
追跡しない。

## セットアップ

```sh
# herdr 本体と mirror プラグイン
herdr channel set preview          # mirror は preview 系の terminal session streams を使う
herdr plugin install nikok6/herdr-mirror
herdr server reload-config

# このディレクトリのスクリプト
cp coder-herdr-sync pane-url ~/.local/bin/

# キーバインド (config.toml の末尾に追記)
cat herdr-config-snippet.toml >> ~/.config/herdr/config.toml
herdr server reload-config

# hosts.toml を生成
coder-herdr-sync --apply
```

`coder.<workspace>` への SSH は `~/.ssh/config` の `Host coder.*`
（`coder config-ssh` が書く ProxyCommand）で通るので、ホストごとの設定は要らない。
mirror は非対話の鍵認証を要求するため `ssh -o BatchMode=yes coder.<name> true`
が通ることだけ確認する。

## 運用

```sh
coder-herdr-sync           # 差分を見るだけ
coder-herdr-sync --apply   # 反映してデーモンを入れ直す
herdr-mirror status        # host ごとに "N mirror workspaces" が出れば繋がっている
```

ワークスペースを作ったら `coder-herdr-sync --apply` を叩く。リモート側の作業は無い。

**自動追従はしない。** `coder-herdr-sync` は人が叩いたときだけ動く。Coder の変化を
検知する仕組みは無く、CLI にイベントを購読する手段も無い (`coder notifications` は
通知の送信・停止のみで、`watch` / `events` に相当するサブコマンドは存在しない)。
自動化するなら定期実行するか、サーバー側の webhook を設定して Mac に受け口を立てる
ことになる。どちらも未実装。

`hosts.toml` は生成物なので手で編集しない。`coder list` の結果から丸ごと作り直される。
ssh 接頭辞は `~/.ssh/config` から、書き込み先は `herdr-mirror status` の `config:` から
取るので、どちらも決め打ちしていない。

## 分かっている制約

- **新しく作ったリモートのタブに自動遷移しない。** mirror プラグインが
  `layout.apply` を `"focus": false` 固定で呼んでいる（`src/mirror.rs`）。設定項目は
  無い。デーモンは `poll_seconds` ごとにリコンサイルするので、ここでフォーカスを
  動かすと同期のたびに奪われる、という判断だと思われる。直すならプラグインに
  パッチを当てるしかない
- **リモートに作られる分、タブ名の入力プロンプトが出ず、数秒のラグがある。**
  ネイティブの `new_tab` と違い、リモートに作ってからミラーされて戻ってくる二段構えのため
- **ミラー pane はマウスで選択するとサイドバーごと選択される。** ホストの
  ターミナルから見ると herdr の画面は1枚の文字画面で pane 境界が無いため。
  herdr 自身の `copy_mode`（`prefix+?` でキーを確認）を使えば pane 内だけを
  選択できる（未検証）。URL を取り出すだけなら `pane-url` が使える
- **pane のスクロールバックはワークスペース再起動で消える。** レイアウトと
  agent セッションは復元される（`claude --resume <id>` が自動で走る）
- **`herdr pane process-info` がこのコンテナでは前景プロセスを返さない。**
  agent 検出はフック経由なので正しく動く。プロセス検出が要るなら
  `HERDR_PROCESS_DETECTION=child-groups` を試す
- **権限プロンプトにデフォルト以外の選択肢を選ぶには方向キーを送る。**
  `herdr pane send-text <pane> 3` は選択肢のショートカットとしては効かず、
  続く Enter がハイライト中のデフォルトを確定させてしまう
