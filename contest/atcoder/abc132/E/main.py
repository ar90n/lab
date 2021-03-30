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


def dijkstra(g, src, dst):
    import heapq

    min_cost = [[float('inf')] * 3 for _ in range(len(g))]

    node_heap = []
    heapq.heappush(node_heap, (0, 0, src))
    while node_heap:
        cost, hop, node = heapq.heappop(node_heap)
        if min_cost[node][hop] <= cost:
            continue
        min_cost[node][hop] = cost

        for _, d, c in g.edges(node):
            dc = min_cost[node][hop] + c
            dh = (hop + 1) % 3
            heapq.heappush(node_heap, (dc, dh, d))
    ret = min_cost[dst][0] // 3
    if isnan(ret):
        return -1
    return ret



def solve(N: int, M: int, u: "List[int]", v: "List[int]", S: int, T: int):
    es = [(cu - 1, cv - 1, 1) for cu, cv in zip(u, v)]
    g = DGraph(N, es)

    S = S - 1
    T = T - 1
    cost = dijkstra(g, S, T)
    return cost


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    u = [int()] * (M)  # type: "List[int]" 
    v = [int()] * (M)  # type: "List[int]" 
    for i in range(M):
        u[i] = int(next(tokens))
        v[i] = int(next(tokens))
    S = int(next(tokens))  # type: int
    T = int(next(tokens))  # type: int
    result = solve(N, M, u, v, S, T)
    print(result)

if __name__ == '__main__':
    main()
