#!/bin/bash

mkdir -p ${XDG_CONFIG_HOME}/nvim/specs
cp node_dein.toml ${XDG_CONFIG_HOME}/nvim/specs/node_dein.toml
jq -s add coc-settings.json ${XDG_CONFIG_HOME}/nvim/coc-settings.json > ${XDG_CONFIG_HOME}/nvim/coc-settings.json.tmp
mv ${XDG_CONFIG_HOME}/nvim/coc-settings.json.tmp ${XDG_CONFIG_HOME}/nvim/coc-settings.json

nvim -c 'call dein#install()' -c 'q'
nvim -c 'UpdateRemotePlugins' -c 'q'
nvim -c 'CocInstall -sync coc-tsserver coc-css coc-cssmodule coc-html|q'
