import time
import socket
from dataclasses import dataclass
from enum import Enum
import struct
from contextlib import contextmanager
from typing import Optional, Tuple


class ICMPType(Enum):
    ECHOREPLY = 0
    ECHO = 8

    def __int__(self):
        return self.value


def calc_checksum(data: bytes) -> int:
    if len(data) % 2 == 1:
        data += b"\x00"
    u16_counts = len(data) // 2
    checksum = sum(struct.unpack(f"!{u16_counts}H", data))
    while 0xFFFF < checksum:
        checksum = (checksum & 0xFFFF) + (checksum >> 16)
    if checksum != 0xFFFF:
        checksum = ~checksum
    return checksum & 0xFFFF


@dataclass(frozen=True)
class IPHeader:
    v: int
    hl: int
    tos: int
    len: int
    id: int
    off: int
    ttl: int
    p: int
    sum: int
    src: str
    dst: str

    @staticmethod
    def from_bytes(packed: bytes) -> "IPHeader":
        v_hl, tos, len, id, off, ttl, p, sum, src, dst = struct.unpack(
            "!BBHHHBBHII", packed
        )
        v = v_hl >> 4
        hl = v_hl & 0x0F

        return IPHeader(
            v,
            hl,
            tos,
            len,
            id,
            off,
            ttl,
            p,
            sum,
            socket.inet_ntoa(src.to_bytes(4, byteorder="big")),
            socket.inet_ntoa(dst.to_bytes(4, byteorder="big")),
        )


@dataclass(frozen=True)
class ICMPEcho:
    type: ICMPType
    code: int
    id: int
    seq: int
    data: bytes
    checksum: Optional[int] = None

    def __post_init__(self):
        if self.checksum is None:
            object.__setattr__(self, "checksum", 0)
            object.__setattr__(self, "checksum", calc_checksum(self.to_bytes()))

    def to_bytes(self) -> bytes:
        return struct.pack(
            f"!BBHHH{len(self.data)}s",
            int(self.type),
            self.code,
            self.checksum,
            self.id,
            self.seq,
            self.data,
        )

    @classmethod
    def from_bytes(cls, packed: bytes) -> "ICMPEcho":
        _type, code, checksum, id, seq = struct.unpack("!BBHHH", packed[:8])
        type = ICMPType(_type)
        data = packed[8:]
        return ICMPEcho(type, code, id, seq, data, checksum=checksum)


@contextmanager
def raw_socket():
    sock = socket.socket(socket.AF_INET, socket.SOCK_RAW, socket.IPPROTO_ICMP)
    try:
        yield sock
    finally:
        sock.close()


def parse_ip_packet(data: bytes) -> Tuple[IPHeader, bytes]:
    ip_header = IPHeader.from_bytes(data[:20])
    payload = data[20:]
    return (ip_header, payload)


def print_response(ip_header: IPHeader, echo_reply: ICMPEcho) -> None:
    print(
        f"ping echo reply from {ip_header.src}: icmp_seq={echo_reply.seq} ttl={ip_header.ttl}"
    )


def ping(host: str, seq: int) -> None:
    with raw_socket() as sock:
        packet = ICMPEcho(ICMPType.ECHO, 0, 0, seq, b"\xff").to_bytes()
        sock.sendto(packet, (host, 0))
        ip_header, payload = parse_ip_packet(sock.recvfrom(4096)[0])
        echo_reply = ICMPEcho.from_bytes(payload)
        print_response(ip_header, echo_reply)


def main(host: str, c: int = 10) -> None:
    for i in range(c):
        ping(host, i)
        time.sleep(1)


if __name__ == "__main__":
    main("8.8.8.8")
