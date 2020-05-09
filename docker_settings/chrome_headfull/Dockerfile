# Derive from https://github.com/atlassian/docker-chromium-xvfb
FROM debian:stretch

RUN apt-get update && apt-get install -y curl xvfb chromium
COPY pin_nodesource /etc/apt/preferences.d/nodesource

ADD xvfb-chromium /usr/bin/xvfb-chromium
RUN ln -s /usr/bin/xvfb-chromium /usr/bin/google-chrome
RUN ln -s /usr/bin/xvfb-chromium /usr/bin/chromium-browser

RUN apt-get update && apt-get install -y \
    python3 python3-pip curl unzip libgconf-2-4

RUN pip3 install poetry pytest selenium chromedriver-binary~=$(chromium --version | perl -pe 's/([^0-9]+)([0-9]+\.[0-9]+).+/$2/g')
