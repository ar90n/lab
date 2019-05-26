#!/usr/bin/env python3
import sys
from math import *
from itertools import *
from collections import *
from functools import *
try:
    from math import gcd
except Exception:
    from fractions import gcd



def solve(L: int):
    r = 0
    while (2 ** r) <= L:
        r += 1
    r -= 1

    edges = []
    nodes = list(range(1, r+2))
    for s, d in zip(nodes, nodes[1:]):
        edges.append((s, d, 0))
        edges.append((s, d, 2 ** (s - 1)))

    base = 2 ** r
    for n in list(reversed(nodes))[1:]:
        c = 2 ** (n - 1)
        if base <= (L - c):
            edges.append((n, nodes[-1], L - c))
            L -= c
    ret = ['{} {}'.format(r + 1, len(edges))]  + ['{} {} {}'.format(s, d, c) for s, d, c in edges]
    return '\n'.join(ret)


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    L = int(next(tokens))  # type: int
    result = solve(L)
    print(result)

if __name__ == '__main__':
    main()
