FROM python:3.5.2
MAINTAINER Masahiro Wada <argon.argon.argon@gmail.com>

RUN groupadd web
RUN useradd -d /home/bottle -m bottle

ADD app /home/bottle/app

WORKDIR /home/bottle/app
RUN pip install -r packages.txt

EXPOSE 8080
ENTRYPOINT ["/usr/local/bin/python", "/home/bottle/app/index.py"]
USER bottle
