#!/bin/bash

mkdir -p ${XDG_CONFIG_HOME}/nvim
cp init.vim ${XDG_CONFIG_HOME}/nvim
cp dein.toml ${XDG_CONFIG_HOME}/nvim
cp -rf template ${XDG_CONFIG_HOME}/nvim
cp -rf snippets ${XDG_CONFIG_HOME}/nvim
cp coc-settings.json ${XDG_CONFIG_HOME}/nvim

mkdir -p ${XDG_CONFIG_HOME}/efm-langserver
cp config.yaml ${XDG_CONFIG_HOME}/efm-langserver

nvim -c 'call dein#install()' -c 'q'
nvim -c 'UpdateRemotePlugins' -c 'q'
nvim -c 'CocInstall -sync coc-json|q'
nvim -c 'CocInstall -sync coc-yaml|q'
nvim -c 'CocInstall -sync coc-snippets|q'
nvim -c 'CocInstall -sync coc-git|q'
nvim -c 'CocInstall -sync coc-todolist|q'
nvim -c 'CocInstall -sync coc-explorer|q'
nvim -c 'CocInstall -sync coc-lists|q'
