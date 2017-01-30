FROM python:3.5.2
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

RUN groupadd web
RUN useradd -d /home/bottle -m bottle

run set -x && \
    apt-get update && \
    apt-get -y install nodejs npm && \
    npm cache clean && \
    npm install n -g && \
    n stable && \
    ln -sf /usr/local/bin/node /usr/bin/node && \
    apt-get -y purge nodejs npm && \
    apt-get clean && \
    pip install gunicorn circus

USER bottle
WORKDIR /home/bottle
run set -x && \
    git clone https://github.com/ar90n/lab.git && \
    cd lab && \
    git config core.sparsecheckout true && \
    echo 'services/pref-comp' > .git/info/sparse-checkout && \
    git read-tree -m -u HEAD



ENV REACT_APP_DECORATORS true
WORKDIR /home/bottle/lab/services/pref-comp/app
RUN set -x && \
    pip install -r packages.txt --user && \
    cd client && \
    npm install && \
    npm run build

EXPOSE 8080
ENTRYPOINT ["/usr/local/bin/python", "/home/bottle/lab/services/pref-comp/app/index.py"]
