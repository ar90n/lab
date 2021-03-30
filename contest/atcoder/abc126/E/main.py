#!/usr/bin/env python3
import sys
from collections.abc import Iterable
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



def solve(N: int, M: int, X: "List[int]", Y: "List[int]", Z: "List[int]"):
    uf = UnionFind(N)
    qq = UnionFind(N)

    for x, y, z in zip(X, Y, Z):
        x -= 1
        y -= 1
        uf.unite(x, y)
        #if (z % 2) == 0:
        #    uf.unite(x, y)
        #else:
        #    qq.unite(x, y)

    return len(set([uf.root(i) for i in range(N)]))


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    X = [int()] * (M)  # type: "List[int]" 
    Y = [int()] * (M)  # type: "List[int]" 
    Z = [int()] * (M)  # type: "List[int]" 
    for i in range(M):
        X[i] = int(next(tokens))
        Y[i] = int(next(tokens))
        Z[i] = int(next(tokens))
    result = solve(N, M, X, Y, Z)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
