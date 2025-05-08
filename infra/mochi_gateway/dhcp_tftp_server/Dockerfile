FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    dnsmasq \
    && rm -rf /var/lib/apt/lists/*

CMD ["dnsmasq", "--no-daemon", "--conf-file=/etc/dnsmasq.conf"]
