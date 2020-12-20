#!/bin/bash

mkdir -p ${XDG_CONFIG_HOME}/nvim/specs
cp dotnet_dein.toml ${XDG_CONFIG_HOME}/nvim/specs

nvim -c 'call dein#install()' -c 'q'
nvim -c 'UpdateRemotePlugins' -c 'q'
nvim -c 'CocInstall -sync coc-fsharp coc-omnisharp|q' -c 'q'
