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


def solve(N: int, u: "List[int]", v: "List[int]", w: "List[int]"):
    edges = [(s - 1, d - 1, w) for s, d, w in zip(u, v, w)]
    g = Graph(N, edges)

    visited = [False] * len(g)
    res = [0] * len(g)
    q = deque([(0, 0)])

    while q:
        i, c = q.popleft()
        if visited[i]:
            continue
        visited[i] = True
        res[i] = c

        for s, d, w in g.edges(i):
            if w % 2 == 0:
                q.append((d, c))
            else:
                q.append((d, (c + 1) % 2))

    return res


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    u = [int()] * (N-1)  # type: "List[int]" 
    v = [int()] * (N-1)  # type: "List[int]" 
    w = [int()] * (N-1)  # type: "List[int]" 
    for i in range(N-1):
        u[i] = int(next(tokens))
        v[i] = int(next(tokens))
        w[i] = int(next(tokens))
    result = solve(N, u, v, w)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
