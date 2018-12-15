import heapq
from collections import Counter, namedtuple

from common import BitStream
from huffman_common import test_huffman_common


def _make_table(vs):
    queue = []
    for v, f in Counter(vs).items():
        heapq.heappush(queue, (f, v, None, None))

    while 1 < len(queue):
        l = heapq.heappop(queue)
        r = heapq.heappop(queue)
        heapq.heappush(queue, (l[0] + r[0], 0, l, r))

    enc_table = {}
    dec_table = {}
    queue.append((1, 1, *queue.pop()))
    while 0 < len(queue):
        c, w, _, v, lt, rt = queue.pop()

        if lt is None and rt is None:
            enc_table[v] = (c, w)
            dec_table.setdefault(w, {})[c] = v

        if lt is not None:
            queue.append((((c << 1) | 0), (w + 1), *lt))

        if rt is not None:
            queue.append((((c << 1) | 1), (w + 1), *rt))

    return enc_table, dec_table


def _encode():
    bs = BitStream()

    def f(table, v):
        if v is None:
            ret = bs.flush()
            return [] if ret is None else [ret]

        c, w = table[v]
        bs.push(c, w)

        result = []
        while 8 <= bs.length:
            result.append(bs.pop(8))
        return result

    return f


def _decode():
    bs = BitStream()

    def f(table, v):
        if v is None:
            return []

        bs.push(v, 8)

        result = []
        for w, codes in table.items():
            while codes.get(bs.peek(w)) is not None:
                result.append(codes.get(bs.peek(w)))
                bs.pop(w)
        return result

    return f


def main():
    test_huffman_common(_make_table, _encode(), _decode())


if __name__ == "__main__":
    main()
