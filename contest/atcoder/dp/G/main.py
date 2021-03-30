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


def topological_sort(g):
    result = []
    status = [0] * len(g)

    def _dfs(v):
        st = [v]
        while 0 < len(st):
            cv = st[-1]
            if status[cv] == 0:
                status[cv] = 1
                for _, d, _ in g.edges(cv):
                    if status[d] == 1:
                        raise ValueError('has cyclic')
                    st.append(d)
            elif status[cv] == 1:
                status[cv] = 2
                result.append(cv)
            if status[cv] == 2:
                st.pop()

    for v in range(len(g)):
        _dfs(v)
    return list(reversed(result))


def solve(N: int, M: int, x: "List[int]", y: "List[int]"):
    es = [(xx-1, yy-1, 0) for xx, yy in zip(x,y)]
    g = DGraph(N, es)

    order = topological_sort(g)
    dp = [0] * N
    for i in order:
        for _, j, _ in g.edges(i):
            dp[j] = max(dp[j], dp[i] + 1)
    return max(dp)

def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    x = [int()] * (M)  # type: "List[int]" 
    y = [int()] * (M)  # type: "List[int]" 
    for i in range(M):
        x[i] = int(next(tokens))
        y[i] = int(next(tokens))
    result = solve(N, M, x, y)
    print(result)

if __name__ == '__main__':
    main()
