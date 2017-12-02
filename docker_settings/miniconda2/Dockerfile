FROM ar90n/dev_env
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

USER $USER
WORKDIR /tmp
RUN set -x && \
    curl -O https://repo.continuum.io/miniconda/Miniconda2-latest-Linux-x86_64.sh && \
    bash Miniconda2-latest-Linux-x86_64.sh -b -p /opt/miniconda2 && \
    sed  -i -e 's/\(PATH=\)"\(.*\)"/\1"\/opt\/miniconda\/bin\/:\2"/g' /etc/environment && \
    rm  Miniconda2-latest-Linux-x86_64.sh

ENV PATH /opt/miniconda2/bin:$PATH
CMD ["python"]
