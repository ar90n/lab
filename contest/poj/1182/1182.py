import sys
import numpy as np

reatInt = lambda: int(sys.stdin.readline())
reatIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

n, k = reatIntN()

ts = []
xs = []
ys = []
for i in range(k):
    t, x, y = reatIntN()
    ts.append(t)
    xs.append(x)
    ys.append(y)


class UnionFind:
    def __init__(self, n):
        self._parents = list(range(n))
        self._ranks = [0] * n

    def root(self, x):
        if self._parents[x] == x:
            return x
        self._parents[x] = self.root(self._parents[x])
        return self._parents[x]

    def unite(self, x, y):
        rx = self.root(x)
        ry = self.root(y)
        if rx == ry:
            return

        if self._ranks[rx] < self._ranks[ry]:
            self._parents[rx] = ry
        else:
            self._parents[ry] = rx
            if self._ranks[rx] == self._ranks[ry]:
                self._ranks[rx] += 1


ret = 0
uf = UnionFind(3 * n)
for t, x, y in zip(ts, xs, ys):
    if x < 0 or 100 <= x or y < 0 or 100 <= y:
        ret += 1
        continue

    if t == 1:
        rx = uf.root(x)
        ry0 = uf.root(y + n)
        ry1 = uf.root(y + 2 * n)
        if rx == ry0 or rx == ry1:
            ret += 1
            continue
        else:
            uf.unite(x, y)
            uf.unite(x + n, y + n)
            uf.unite(x + 2 * n, y + 2 * n)
    else:
        rx = uf.root(x)
        ry0 = uf.root(y)
        ry1 = uf.root(y + 2 * n)
        if rx == ry0 or rx == ry1:
            ret += 1
            continue
        else:
            uf.unite(x, y + n)
            uf.unite(x + n, y + 2 * n)
            uf.unite(x + 2 * n, y)
print(ret)
