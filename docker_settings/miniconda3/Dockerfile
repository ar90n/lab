FROM ar90n/dev_env
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

USER $USER
WORKDIR /tmp
RUN set -x && \
    curl -O https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh && \
    bash Miniconda3-latest-Linux-x86_64.sh -b -p /opt/miniconda3 && \
    sed  -i -e 's/\(PATH=\)"\(.*\)"/\1"\/opt\/miniconda\/bin\/:\2"/g' /etc/environment && \
    rm  Miniconda3-latest-Linux-x86_64.sh

ENV PATH /opt/miniconda3/bin:$PATH
CMD ["python"]
