#!/usr/bin/env python3
import sys
from math import *
from itertools import *
from collections import *
from functools import *
try:
    from math import gcd
except Exception:
    from fractions import gcd


class BinaryIndexTree:
    def __init__(self, n):
        self._size = n
        self._tree = [0] * (n+1)

    def sum(self, i):
        s = 0
        i += 1
        while 0 < i:
            s += self._tree[i-1]
            i -= i & -i
        return s

    def add(self, i, x):
        i += 1
        while i <= self._size:
            self._tree[i-1] += x
            i += i & -i


def solve(N: int, a: "List[int]"):
    c = N * (N + 1) // 2
    th = (c + 1) // 2

    xs = list(sorted(set(a)))
    lo = 0
    hi = len(xs)

    def _f(x):
        ss = [0] + list(accumulate([1 if x <= v else -1 for v in a]))
        n = max([abs(s) for s in ss])
        bit = BinaryIndexTree(2 * n + 1)

        ret = 0
        for s in ss:
            ret += bit.sum(s + n) 
            bit.add(s + n, 1)
        return ret

    while lo < hi:
        mid = (lo + hi) // 2
        if th <= _f(xs[mid]):
            lo = mid + 1
        else:
            hi = mid
    return xs[lo]


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    a = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, a)
    print(result)

if __name__ == '__main__':
    main()
