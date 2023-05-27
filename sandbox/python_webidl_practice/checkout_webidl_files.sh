git clone -n --depth 1 --filter=tree:0 git@github.com:mozilla/gecko-dev.git
cd gecko-dev
git sparse-checkout set --no-cone dom/webidl