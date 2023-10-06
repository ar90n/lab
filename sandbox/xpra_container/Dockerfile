FROM ubuntu:lunar

ARG DEFAULT_DISPLAY_NUMBER=20000
ARG DEFAULT_PORT=20000

VOLUME /tmp/.X11-unix

RUN <<EOF
 apt update
 apt-get install -y wget gnupg
 echo "deb [arch=amd64] https://xpra.org/ lunar main" > /etc/apt/sources.list.d/xpra.list
 wget -q https://xpra.org/gpg.asc -O- | apt-key add -
EOF

RUN <<EOF
 apt update
 DEBIAN_FRONTEND=noninteractive apt install -y xpra x11-xserver-utils x11-apps
EOF

RUN cat <<EOF > /opt/init.sh
#! /usr/bin/env bash
xpra start :\${DISPLAY_NUMBER:-$DEFAULT_DISPLAY_NUMBER} --bind-tcp=0.0.0.0:\${PORT:-$DEFAULT_PORT} --mdns=no --webcam=no --no-daemon --start="xhost +"
EOF
RUN chmod +x /opt/init.sh

ENTRYPOINT ["/opt/init.sh"]
