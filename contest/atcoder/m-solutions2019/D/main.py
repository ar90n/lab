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



def solve(N: int, a: "List[int]", b: "List[int]", c: "List[int]"):
    edges = [(ca - 1, cb - 1, 0) for ca, cb in zip(a, b)]
    g = Graph(N, edges)
    s = [-1] * N

    c = deque(sorted(c, reverse=True))
    q = deque([0])
    while 0 < len(q):
        n = q.popleft()
        if s[n] != -1:
            continue
        s[n] = c.popleft()
        for _, d, _ in g.edges(n):
            q.append(d)

    ret = 0
    for b, e, _ in g.edges():
        ret += min(s[b], s[e])
    ret //= 2
    return '\n'.join([str(ret), ' '.join([str(n) for n in s])])

def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    a = [int()] * (N-1)  # type: "List[int]" 
    b = [int()] * (N-1)  # type: "List[int]" 
    for i in range(N-1):
        a[i] = int(next(tokens))
        b[i] = int(next(tokens))
    c = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, a, b, c)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
