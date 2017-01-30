FROM python:3.5.2
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

RUN groupadd web
RUN useradd -d /home/bottle -m bottle

run set -x && \
    pip install gunicorn circus


USER bottle
WORKDIR /home/bottle
run set -x && \
    git clone https://github.com/ar90n/lab.git && \
    cd lab && \
    git config core.sparsecheckout true && \
    echo 'services/pref-comp' > .git/info/sparse-checkout && \
    git read-tree -m -u HEAD

WORKDIR /home/bottle/lab/services/pref-comp/app
RUN pip install -r packages.txt --user

EXPOSE 8080
ENTRYPOINT ["/usr/local/bin/python", "/home/bottle/lab/services/pref-comp/app/index.py"]
