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


def solve(N: int, x: "List[int]", y: "List[int]"):
    xs = {v:i for i, v in enumerate(set(x))}
    ys = {v:i for i, v in enumerate(set(y), len(xs))}
    
    n = len(xs) + len(ys)
    es = [(xs[cx], ys[cy], 1) for cx, cy in zip(x, y)]
    g = Graph(n, es)

    visited = [False] * len(g)
    def dfs(g, src):
        nonlocal visited
        stack = [(src, 0)]
        while stack:
            n, c = stack.pop()
            if visited[n]:
                continue
            visited[n] = True

            for _, d, _ in g.edges(n):
                #yield d
                stack.append((d, c + 1))
            yield n

    def _f(i):
        ls = list(i)
        if len(ls) == 0:
            return 0

        xc = 0
        yc = 0
        es = 0
        for i in ls:
            es += len(g.edges(i))
            if i < len(xs):
                xc += 1
            else:
                yc += 1
        es //= 2

        if xc < 2 or yc < 2:
            return 0

        return xc * yc - es

    return sum([_f(dfs(g, i)) for i in range(n)])


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    x = [int()] * (N)  # type: "List[int]" 
    y = [int()] * (N)  # type: "List[int]" 
    for i in range(N):
        x[i] = int(next(tokens))
        y[i] = int(next(tokens))
    result = solve(N, x, y)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
