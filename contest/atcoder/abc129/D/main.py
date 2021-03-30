#!/usr/bin/env python3
import sys
from math import *
from itertools import *
from collections import *
from functools import *
from operator import *
try:
    from math import gcd
except Exception:
    from fractions import gcd


class UnionFind:
    def __init__(self, n):
        self._parents = list(range(n))
        self._ranks = [0] * n
        self._size = [1] * n

    def root(self, x):
        if self._parents[x] == x:
            return x
        self._parents[x] = self.root(self._parents[x])
        return self._parents[x]

    def size(self, x):
        return self._size[self.root(x)]

    def unite(self, x, y):
        rx = self.root(x)
        ry = self.root(y)
        if rx == ry:
            return

        if self._ranks[rx] < self._ranks[ry]:
            self._parents[rx] = ry
            self._size[ry] += self._size[rx]
        else:
            self._parents[ry] = rx
            self._size[rx] += self._size[ry]
            if self._ranks[rx] == self._ranks[ry]:
                self._ranks[rx] += 1

    def same(self, x, y):
        return self.root(x) == self.root(y)


def solve(H: int, W: int, S: "List[str]"):
    uf_h = UnionFind(H * W)
    uf_v = UnionFind(H * W)
    for y in range(H):
        for x in range(W):
            i = y * W + x
            phi =  y * W + (x - 1)
            pvi = (y-1) * W + x
            if 0 < x and S[y][x] == '.' and S[y][x-1] == '.':
                uf_h.unite(i, phi)
            if 0 < y and S[y][x] == '.' and S[y-1][x] == '.':
                uf_v.unite(i, pvi)

    ret = 0
    for i in range(H * W):
        ret = max(ret, uf_h.size(i) + uf_v.size(i) - 1)
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    H = int(next(tokens))  # type: int
    W = int(next(tokens))  # type: int
    S = [ next(tokens) for _ in range(H) ]  # type: "List[str]"
    result = solve(H, W, S)
    print(result)

if __name__ == '__main__':
    main()
