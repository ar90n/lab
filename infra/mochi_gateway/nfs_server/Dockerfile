FROM debian:stable-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    nfs-kernel-server && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p /var/nfsroot
VOLUME ["/var/nfsroot"]

COPY exports /etc/exports

EXPOSE 2049 111/udp 111/tcp

CMD ["/bin/sh", "-c", "\
  mkdir -p /run/rpcbind && \
  rpcbind && \
  exportfs -rv && \
  /usr/sbin/rpc.nfsd && \
  /usr/sbin/rpc.mountd -F -N 2 && \
  tail -f /dev/null"]

