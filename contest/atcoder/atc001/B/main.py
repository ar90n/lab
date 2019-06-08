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

YES = "Yes"  # type: str
NO = "No"  # type: str


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


def solve(N: int, Q: int, P: "List[int]", A: "List[int]", B: "List[int]"):
    uf = UnionFind(N)

    result = []
    for p, a, b in zip(P, A, B):
        if p == 1:
            result.append(YES if uf.same(a, b) else NO)
        else:
            uf.unite(a, b)
        
    return result


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    Q = int(next(tokens))  # type: int
    P = [int()] * (Q)  # type: "List[int]" 
    A = [int()] * (Q)  # type: "List[int]" 
    B = [int()] * (Q)  # type: "List[int]" 
    for i in range(Q):
        P[i] = int(next(tokens))
        A[i] = int(next(tokens))
        B[i] = int(next(tokens))
    result = solve(N, Q, P, A, B)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
