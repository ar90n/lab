#!/bin/bash
#from https://qiita.com/hanon/items/d8cbe25aa8f3a9347b0b

# Avoid "JavaScript heap out of memory" errors during extension installation
# (OS X/Linux)
export NODE_OPTIONS=--max-old-space-size=4096

# jupytext
poetry run jupyter labextension install jupyterlab-jupytext@0.19 --no-build
poetry run jupyter nbextension install --py jupytext
poetry run jupyter nbextension enable --py jupytext

# Jupyter widgets extension
poetry run jupyter labextension install @jupyter-widgets/jupyterlab-manager@1.0 --no-build

# FigureWidget support
poetry run jupyter labextension install plotlywidget@1.2.0 --no-build

# and jupyterlab renderer support
poetry run jupyter labextension install jupyterlab-plotly@1.2.0 --no-build

# JupyterLab chart editor support (optional)
poetry run jupyter labextension install jupyterlab-chart-editor@1.2 --no-build

# Build extensions (must be done to activate extensions since --no-build is used above)
poetry run jupyter lab build

# Unset NODE_OPTIONS environment variable
# (OS X/Linux)
unset NODE_OPTIONS
