FROM ubuntu:20.04

RUN apt-get update 
    && apt-get install -y \
        git \
        wget \
        cmake \
        build-essential \
        curl \
        unzip \
        zlib1g-dev \
    && apt-get clean \
    && echo 'export PATH=/opt/conda/bin:$PATH' > /etc/profile.d/conda.sh \
    && wget \
        --quiet "https://github.com/jjhelmus/conda4aarch64/releases/download/1.0.0/c4aarch64_installer-1.0.0-Linux-aarch64.sh" \
        -O /tmp/c4aarch64_installer-1.0.0-Linux-aarch64.sh \
    && /bin/bash /tmp/c4aarch64_installer-1.0.0-Linux-aarch64.sh -b -p /opt/conda \
    && rm /tmp/c4aarch64_installer-1.0.0-Linux-aarch64.sh \
    && /opt/conda/bin/conda config --add channels c4aarch64 \
    && /opt/conda/bin/conda config --add channels conda-forge

ENV PATH "/opt/conda/bin:$PATH"
