from collections import Counter, namedtuple
import heapq

from huffman_common import test_huffman_common


def _make_table(vs):
    queue = []
    for v, f in Counter(vs).items():
        heapq.heappush(queue, (f, v, None, None))

    while 1 < len(queue):
        l =  heapq.heappop(queue)
        r =  heapq.heappop(queue)
        heapq.heappush(queue, (l[0] + r[0], 0, l, r))

    table = {}
    queue.append((0, 1, *queue.pop()))
    while 0 < len(queue):
        c, w, _, v, lt, rt = queue.pop()

        if lt is None:
            table[v] = (c, w)
        else:
            queue.append((((c << 1) | 0), (w + 1), *lt))

        if rt is None:
            table[v] = (c, w)
        else:
            queue.append((((c << 1) | 1), (w + 1), *rt))
    return table


def _encode(table, v):
    if v is None:
        return []
    print(table[v])
    return [v]


def _decode(table, v):
    if v is None:
        return []
    return [v]


def main():
    test_huffman_common(_make_table, _encode, _decode)


if __name__ == "__main__":
    main()
