#!/usr/bin/env python3
import sys
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



def solve(N: int, M: int, A: "List[int]", B: "List[int]"):
    uf = UnionFind(N)
    ret = [N * (N - 1) // 2]
    for a, b in zip(reversed(A), reversed(B)):
        a -= 1
        b -= 1
        sa = uf.size(a)
        sb = uf.size(b)
        cur_ret = ret[-1]
        if uf.root(a) != uf.root(b):
            cur_ret -= sa * sb
        ret.append(cur_ret)
        uf.unite(a, b)
    ret.reverse()
    return '\n'.join([str(x) for x in ret[1:]])


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    A = [int()] * (M)  # type: "List[int]" 
    B = [int()] * (M)  # type: "List[int]" 
    for i in range(M):
        A[i] = int(next(tokens))
        B[i] = int(next(tokens))
    result = solve(N, M, A, B)
    print(result)

if __name__ == '__main__':
    main()
