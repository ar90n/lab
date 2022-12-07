FROM pytorch/pytorch:1.11.0-cuda11.3-cudnn8-devel
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

RUN apt-key del 7fa2af80
RUN apt-key adv --fetch-keys https://developer.download.nvidia.com/compute/cuda/repos/ubuntu2004/x86_64/3bf863cc.pub
RUN apt-key adv --fetch-keys https://developer.download.nvidia.com/compute/machine-learning/repos/ubuntu2004/x86_64/7fa2af80.pub

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
    curl -O https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh && \
    bash Miniconda3-latest-Linux-x86_64.sh -b -p /opt/miniconda3 && \
    rm  Miniconda3-latest-Linux-x86_64.sh && \
    /opt/miniconda3/bin/conda install python=3.7 && \
    curl https://bootstrap.pypa.io/get-pip.py -o /tmp/get-pip.py && \
    /opt/miniconda3/bin/python /tmp/get-pip.py && \
    /opt/miniconda3/bin/pip install poetry

ENV PATH /opt/miniconda3/bin:$PATH
CMD ["python"]
