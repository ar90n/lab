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

MOD = 1000000007  # type: int


class DGraph:
    def __init__(self, n, edges):
        self._vertice = [{} for _ in range(n)]
        for e in edges:
            self._vertice[e[0]][e[1]] = e[2] if len(e) == 3 else 1

    def __len__(self):
        return len(self._vertice)

    def edges(self, n=None):
        srcs = range(len(self)) if n is None else [n]
        return sum([[(s, d, c) for d, c in self._vertice[s].items()] for s in srcs], [])

    def transpose(self):
        tedges = [(t, f, c) for f, t, c in self.edges()]
        return DGraph(len(self), tedges)

    def cost(self, src, dst):
        return self._vertice[src].get(dst, float('inf'))

class Graph(DGraph):
    def __init__(self, n, edges):
        nd_edges = edges + [(d, s, c) for s, d, c in edges]
        super().__init__(n, nd_edges)

    def transpose(self):
        return self


def perm_mod(n, m, M):
    ret = 1
    for i in range(n, n - m, -1):
        ret = (ret * i) % M
    return ret


def solve(N: int, K: int, a: "List[int]", b: "List[int]"):
    es = [(ca-1, cb-1, 1) for ca, cb in zip(a, b)]
    g = Graph(N, es)

    ret = K
    visited = [False] * N
    q = deque([(0, K)])
    while 0 < len(q):
        cv, k = q.popleft()
        if visited[cv]:
            contains
        visited[cv] = True

        cs = [d for _, d, _ in g.edges(cv) if not visited[d]]
        ck = max(K - 2, k - 1)
        if 0 < len(cs):
            ret = (ret * perm_mod(ck, len(cs), MOD)) % MOD
        for c in cs:
            q.append((c, ck))
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    K = int(next(tokens))  # type: int
    a = [int()] * (N-1)  # type: "List[int]" 
    b = [int()] * (N-1)  # type: "List[int]" 
    for i in range(N-1):
        a[i] = int(next(tokens))
        b[i] = int(next(tokens))
    result = solve(N, K, a, b)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
