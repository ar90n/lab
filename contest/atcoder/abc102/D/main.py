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


def bs(a, lo, hi, f):
    olo = lo
    ohi = hi
    while (lo + 1) < hi:
        mid = (lo + hi) // 2
        v = f(ohi, mid, olo)
        if 0 < v:
            lo = mid
        elif v < 0:
            hi = mid
        else:
            return mid
    if abs(f(ohi, lo, olo)) < abs(f(ohi, hi, olo)):
        return lo
    else:
        return hi

def solve(N: int, A: "List[int]"):

    def _g(li, ci, ri):
        a = acc[li - 1] - acc[0]
        b = acc[ci - 1] - acc[li-1]
        c = acc[ri - 1] - acc[ci - 1]
        d = acc[-1] - acc[ri - 1]
        cr = max([a, b, c, d]) - min([a, b, c, d])
        return cr

    def _f(h, m, l):
        return ((acc[h - 1] - acc[m-1]) - (acc[m - 1] - acc[l - 1]))

    acc = [0] + list(accumulate(A))

    ret = float('inf')
    for i in range(3, len(acc) - 1):
        li = bs(acc, 1, i, _f)
        ri = bs(acc, i, len(acc), _f)
        cv = _g(li, i, ri)
        ret = min(ret, cv)
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    A = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, A)
    print(result)

if __name__ == '__main__':
    main()
