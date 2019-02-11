#!/bin/bash

mkdir -p ${XDG_CONFIG_HOME}/nvim/specs
cp rust_dein.toml ${XDG_CONFIG_HOME}/nvim/specs

nvim -c 'call dein#install()' -c 'q'
nvim -c 'UpdateRemotePlugins' -c 'q'
