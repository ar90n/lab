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

def dfs(g, src):
    visited = [False] * len(g)
    stack = [(src, (0, -1, {}))]
    while stack:
        n, (c, p, acc) = stack.pop()
        if visited[n]:
            continue
        visited[n] = True

        for _, d, (dd, cc) in g.edges(n):
            _acc = dict(acc)
            if cc not in _acc:
                _acc[cc] = (0, 0)
            _acc[cc] = (_acc[cc][0] + 1, _acc[cc][1] + dd)
            stack.append((d, (c + 1, n, _acc)))
        yield n, c, p, acc


def solve(N: int, Q: int, a: "List[int]", b: "List[int]", c: "List[int]", d: "List[int]", x: "List[int]", y: "List[int]", u: "List[int]", v: "List[int]"):
    es = []
    for _a, _b, _c, _d in zip(a, b, c, d):
        es.append((_a - 1, _b - 1, (_d, _c)))

    h = int(ceil(log2(N)))
    g = Graph(N, es)
    ds = [-1] * N
    ps = [[-1] * h for _ in range(N)]
    accs = [{}] * N
    for i, d, p, acc in dfs(g, 0):
        ds[i] = d
        ps[i][0] = p
        accs[i] = acc

    for j in range(h - 1):
        for i in range(N):
            ps[i][j + 1] = ps[ps[i][j]][j]

    ret = []
    for _x, _y, _u, _v in zip(x, y, u, v):
        uu = _u - 1
        vv = _v - 1
        while ds[uu] < ds[vv]:
            vv = ps[vv][0]
        while ds[vv] < ds[uu]:
            uu = ps[uu][0]
        if uu == vv:
            lca = uu
        else:
            #for i in range(h-1, 0, -1):
            #    if ps[uu][i] != ps[vv][i]:
            #        uu = ps[uu][i]
            #        vv = ps[vv][i]
            #lca = ps[uu][0]

        du = sum([ddd for _, ddd in accs[_u - 1].values()])
        if _x in accs[_u - 1]:
            du += _y * accs[_u - 1][_x][0] - accs[_u - 1][_x][1]
        dv = sum([ddd for _, ddd in accs[_v - 1].values()])
        if _x in accs[_v - 1]:
            dv += _y * accs[_v - 1][_x][0] - accs[_v - 1][_x][1]
        dlca = sum([ddd for _, ddd in accs[lca].values()])
        if _x in accs[lca]:
            dlca += _y * accs[lca][_x][0] - accs[lca][_x][1]
        dist = du + dv - 2 * dlca
        ret.append(dist)
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    Q = int(next(tokens))  # type: int
    a = [int()] * (N-1)  # type: "List[int]" 
    b = [int()] * (N-1)  # type: "List[int]" 
    c = [int()] * (N-1)  # type: "List[int]" 
    d = [int()] * (N-1)  # type: "List[int]" 
    for i in range(N-1):
        a[i] = int(next(tokens))
        b[i] = int(next(tokens))
        c[i] = int(next(tokens))
        d[i] = int(next(tokens))
    x = [int()] * (Q)  # type: "List[int]" 
    y = [int()] * (Q)  # type: "List[int]" 
    u = [int()] * (Q)  # type: "List[int]" 
    v = [int()] * (Q)  # type: "List[int]" 
    for i in range(Q):
        x[i] = int(next(tokens))
        y[i] = int(next(tokens))
        u[i] = int(next(tokens))
        v[i] = int(next(tokens))
    result = solve(N, Q, a, b, c, d, x, y, u, v)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
