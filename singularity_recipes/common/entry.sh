#!/bin/bash

mkdir -p ${XDG_CONFIG_HOME}/nvim
cp init.vim ${XDG_CONFIG_HOME}/nvim
cp dein.toml ${XDG_CONFIG_HOME}/nvim
cp -rf template ${XDG_CONFIG_HOME}/nvim

nvim -c 'call dein#install()' -c 'q'
nvim -c 'UpdateRemotePlugins' -c 'q'
