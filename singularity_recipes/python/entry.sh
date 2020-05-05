#!/bin/bash

mkdir -p ${XDG_CONFIG_HOME}/nvim/specs
cp python_dein.toml ${XDG_CONFIG_HOME}/nvim/specs
jq -s '.[0] * .[1]' coc-settings.json ${XDG_CONFIG_HOME}/nvim/coc-settings.json > ${XDG_CONFIG_HOME}/nvim/coc-settings.json.tmp
mv ${XDG_CONFIG_HOME}/nvim/coc-settings.json.tmp ${XDG_CONFIG_HOME}/nvim/coc-settings.json

nvim -c 'call dein#install()' -c 'q'
nvim -c 'UpdateRemotePlugins' -c 'q'
nvim -c 'CocInstall -sync coc-pyright|q'
nvim -c 'CocInstall -sync coc-python|q'
