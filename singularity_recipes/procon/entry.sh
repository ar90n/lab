#!/bin/bash

mkdir -p ${XDG_CONFIG_HOME}/nvim/specs
mkdir -p ${XDG_CONFIG_HOME}/nvim/template
cp procon_dein.toml ${XDG_CONFIG_HOME}/nvim/specs
cp procon_init.vim ${XDG_CONFIG_HOME}/nvim/specs
cp procon_python.template ${XDG_CONFIG_HOME}/nvim/template
cp python.snip ${XDG_CONFIG_HOME}/nvim/snippets
cp cpp.snip ${XDG_CONFIG_HOME}/nvim/snippets

nvim -c 'call dein#install()' -c 'q'
nvim -c 'UpdateRemotePlugins' -c 'q'
