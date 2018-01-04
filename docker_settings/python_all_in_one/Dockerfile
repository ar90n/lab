FROM ar90n/miniconda3
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

ARG user
ARG group

USER root
WORKDIR /tmp
RUN set -x && \
    conda install -y jupyter && \
    conda install -y -c r libiconv && \
    conda install -y -c conda-forge jupyter_contrib_nbextensions jupyter_nbextensions_configurator ipywidgets && \
    conda install -y -c damianavila82 rise && \
    conda install -y -c conda-forge opencv pillow && \
    conda install -y -c clinicalgraphics pydicom && \
    conda install -y -c simpleitk simpleitk && \
    conda install -y -c conda-forge keras tensorflow && \
    conda install -y -c conda-forge networkx && \
    conda install -y -c conda-forge numpy && \
    conda install -y -c conda-forge pandas pandas-datareader && \
    conda install -y -c conda-forge seaborn && \
    conda install -y -c conda-forge xgboost lightgbm && \
    conda install -y -c conda-forge scrapy requests && \
    conda install -y -c conda-forge scipy scikit-learn scikit-image matplotlib bokeh holoviews && \
    conda install -y cython && \
    conda clean -t && \
    pip install jupyter-tensorboard && \
    pip install chainer chainercv

ENV JUPYTER_CONFIG_DIR /opt/jupyter_config
ENV JUPYTER_PATH /opt/jupyter
ADD jupyter-server /opt/bin/jupyter-server
#ADD jupyter_notebook_config.py $JUPYTER_CONFIG_DIR/
#ADD jupyter_notebook_config.json $JUPYTER_CONFIG_DIR/
ADD notebook.json $JUPYTER_CONFIG_DIR/nbconfig/
RUN set -x && \
    mkdir -p $JUPYTER_PATH/nbextensions && \
    cd $JUPYTER_PATH/nbextensions && \
    git clone https://github.com/lambdalisue/jupyter-vim-binding.git vim_binding && \
    chmod -R go-w vim_binding && \
    jupyter contrib nbextension install --user && \
    jupyter nbextensions_configurator enable --user && \
    jupyter-nbextension enable vim_binding/vim_binding && \
    jupyter-nbextension enable --py widgetsnbextension --user && \
    jupyter-nbextension enable --py jupyter_tensorboard --user && \
    jupyter-nbextension install rise --py --user && \
    jupyter-nbextension enable rise --py --user && \
    chown -R $user:$group $JUPYTER_CONFIG_DIR && \
    chown -R $user:$group $JUPYTER_PATH
USER $user
