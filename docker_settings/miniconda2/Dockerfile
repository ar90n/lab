FROM ar90n/dev_env
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

ARG user

USER root
RUN sed  -i -e 's/\(PATH=\)"\(.*\)"/\1"\/opt\/miniconda\/bin\/:\2"/g' /etc/environment

USER $user
WORKDIR /tmp
ADD miniconda2_dein.toml /opt/nvim/specs/miniconda2_dein.toml
RUN set -x && \
    curl -O https://repo.continuum.io/miniconda/Miniconda2-latest-Linux-x86_64.sh && \
    bash Miniconda2-latest-Linux-x86_64.sh -b -p /opt/miniconda2 && \
    rm  Miniconda2-latest-Linux-x86_64.sh && \
    /opt/miniconda2/bin/pip install jedi autopep8 pycodestyle pyflakes flake8 hacking pylint && \
    nvim -c 'call dein#install()' -c 'q' && \
    nvim -c 'UpdateRemotePlugins' -c 'q'

ENV PATH /opt/miniconda2/bin:$PATH
ENV PYTHONPATH /opt/miniconda2/lib/python2.7/site-packages:PYTHONPATH
CMD ["python"]
