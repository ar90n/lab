FROM ar90n/miniconda3
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

ARG user

USER root
WORKDIR /tmp
RUN set -x && \
    conda install -y jupyter && \
    conda install -y -c r libiconv && \
    conda install -y -c conda-forge jupyter_contrib_nbextensions && \
    conda install -y -c conda-forge opencv pillow && \
    conda install -y -c clinicalgraphics pydicom && \
    conda install -y -c simpleitk simpleitk && \
    conda install -y -c conda-forge keras tensorflow && \
    conda install -y -c conda-forge networkx && \
    conda install -y -c conda-forge numpy && \
    conda install -y -c conda-forge pandas && \
    conda install -y -c conda-forge seaborn && \
    conda install -y -c conda-forge xgboost lightgbm && \
    conda install -y -c conda-forge scrapy requests && \
    conda install -y -c conda-forge scipy scikit-learn scikit-image matplotlib && \
    conda install -y cython && \
    conda clean -t && \
    yes | pip install https://github.com/ipython-contrib/IPython-notebook-extensions/archive/master.zip --user

USER $user
ENV JUPYTER_CONFIG_DIR /opt/jupyter_config
ENV JUPYTER_PATH /opt/jupyter_data
RUN set -x && \
    mkdir $JUPYTER_CONFIG_DIR && \
    mkdir $JUPYTER_PATH

ADD jupyter-server /opt/bin/jupyter-server
ADD jupyter_notebook_config.json $JUPYTER_CONFIG_DIR/
ADD jupyter_notebook_config.py $JUPYTER_CONFIG_DIR/
ADD notebook.json $JUPYTER_CONFIG_DIR/nbconfig/
RUN set -x && \
    mkdir -p $JUPYTER_PATH/nbextensions/usability && \
    cd $JUPYTER_PATH/nbextensions/usability && \
    git clone https://github.com/lambdalisue/jupyter-vim-binding.git vim_binding && \
    chmod -R go-w vim_binding
