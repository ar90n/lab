#!/bin/bash

mkdir -p ${XDG_CONFIG_HOME}/nvim/specs
cp python_dein.toml ${XDG_CONFIG_HOME}/nvim/specs

mkdir -p ${JUPYTER_CONFIG_DIR}
mkdir -p ${JUPYTER_CONFIG_DIR}/nbextensions
/opt/miniconda3/bin/jupyter notebook --generate-config
git clone https://github.com/lambdalisue/jupyter-vim-binding.git ${XDG_CONFIG_HOME}/jupyter/nbextensions/vim_binding
/opt/miniconda3/bin/jupyter contrib nbextension install --user
/opt/miniconda3/bin/jupyter nbextensions_configurator enable --user
/opt/miniconda3/bin/jupyter-nbextension enable vim_binding/vim_binding
/opt/miniconda3/bin/jupyter-nbextension enable --py widgetsnbextension --user
/opt/miniconda3/bin/jupyter-nbextension install rise --py --user
/opt/miniconda3/bin/jupyter-nbextension enable rise --py --user

nvim -c 'call dein#install()' -c 'q'
nvim -c 'UpdateRemotePlugins' -c 'q'
