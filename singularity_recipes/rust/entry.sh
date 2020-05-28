#!/bin/bash

mkdir -p ${XDG_CONFIG_HOME}/nvim/specs
mkdir -p ${XDG_CONFIG_HOME}/cargo/registry
cp rust_dein.toml ${XDG_CONFIG_HOME}/nvim/specs

nvim -c 'call dein#install()' -c 'q'
nvim -c 'UpdateRemotePlugins' -c 'q'
nvim -c 'CocInstall -sync coc-rls coc-rust-analyzer|q'
