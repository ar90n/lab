FROM rootproject/root-ubuntu16
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>


ARG user
ARG uid
ARG group
ARG gid
ARG port

RUN echo $gid $group
RUN groupadd -g $gid $group
RUN useradd -u $uid -g $group -M $user -s /bin/nologin

RUN ln -s /home /Users

USER root
RUN set -x && \
    apt-get update -qq && \
    apt-get upgrade -qq && \
    apt-get install -y language-pack-ja tzdata && \
    update-locale LANG=C.UTF-8 && \
    apt-get install -y python-pip && \
    pip install --upgrade pip && \
    pip install jupyter metakernel zmq && \
    pip install autopep8 && \
    pip install jupyter_contrib_nbextensions jupyter_nbextensions_configurator ipywidgets rise && \
    rm -rf /var/lib/apt/lists/*
ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

ENV PATH /opt/bin:$PATH
RUN set -x && \
    chmod 755 /opt && \
    mkdir /opt/bin && \
    chown -R $user:$group /opt

ENV XDG_CONFIG_HOME /opt/
ENV XDG_DATA_HOME /opt/
ENV JUPYTER_CONFIG_DIR /opt/jupyter_config
ENV JUPYTER_PATH /opt/jupyter
ADD notebook.json $JUPYTER_CONFIG_DIR/nbconfig/
ADD jupyter-server /opt/bin/jupyter-server
RUN set -x && \
    mkdir -p $JUPYTER_PATH/nbextensions && \
    cd $JUPYTER_PATH/nbextensions && \
    git clone https://github.com/lambdalisue/jupyter-vim-binding.git vim_binding && \
    chmod -R go-w vim_binding && \
    jupyter contrib nbextension install --user && \
    jupyter nbextensions_configurator enable --user && \
    jupyter-nbextension enable vim_binding/vim_binding && \
    jupyter-nbextension enable --py widgetsnbextension --user && \
    jupyter-nbextension install rise --py --user && \
    jupyter-nbextension enable rise --py --user && \
    chown -R $user:$group $JUPYTER_CONFIG_DIR && \
    chown -R $user:$group $JUPYTER_PATH

ENV TZ=Asia/Tokyo
RUN rm /etc/localtime && \
    ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && \
    echo $TZ > /etc/timezone
RUN dpkg-reconfigure --frontend noninteractive tzdata

USER ${user}
EXPOSE ${port}
CMD ["jupyter-server", "${port}"]
