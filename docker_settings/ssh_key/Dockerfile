FROM alpine

RUN   apk update && \
      apk add --no-cache \
      openssh-keygen 

RUN yes '' | ssh-keygen -t rsa -N '' -f /opt/id_rsa > /dev/null
RUN cat /opt/id_rsa.pub
