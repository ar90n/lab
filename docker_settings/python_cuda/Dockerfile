FROM pytorch/pytorch:nightly-devel-cuda10.0-cudnn7
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

RUN set -x && \
    apt-get update -qq && \
    apt-get upgrade -qq && \
    apt-get install -y software-properties-common && \
    apt-get install -y language-pack-ja && \
    apt-get install -y build-essential git curl vim libffi-dev libsm6 libxrender1 && \
    apt-get install -y tmux unison unzip && \
    apt-get clean

WORKDIR /tmp
RUN set -x && \
    curl -O https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh && \
    bash Miniconda3-latest-Linux-x86_64.sh -b -p /opt/miniconda3 && \
    rm  Miniconda3-latest-Linux-x86_64.sh && \
    /opt/miniconda3/bin/conda install python=3.6 && \
    curl https://bootstrap.pypa.io/get-pip.py -o /tmp/get-pip.py && \
    /opt/miniconda3/bin/python /tmp/get-pip.py && \
    /opt/miniconda3/bin/pip install poetry

ENV PATH /opt/miniconda3/bin:$PATH
CMD ["python"]
