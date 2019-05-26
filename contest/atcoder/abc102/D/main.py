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


def solve(N: int, A: "List[int]"):
    acc = list(accumulate(A))
    li = 1
    rdiff = float('inf')
    ri = 0
    for i in range(3, len(acc)):
        d = acc[-1] - acc[i-1]
        c = acc[i-1] - acc[1]
        if abs(d - c) < rdiff:
            rdiff = abs(d -c)
            ri = i
    ci = 2

    def _g(li, ci, ri):
        a = acc[li - 1]
        b = acc[ci - 1] - acc[li-1]
        c = acc[ri - 1] - acc[ci - 1]
        d = acc[-1] - acc[ri - 1]
        cr = max([a, b, c, d]) - min([a, b, c, d])
        return cr

    def _f(h, m, l):
        bbb = acc[l-1] if 0 < l else 0
        return abs((acc[h - 1] - acc[m-1]) - (acc[m - 1] - bbb))
  
    r = _g(li, ci, ri)
    for i in range(3, len(acc) - 1):
        while _f(i, (li+1), 0) <  _f(i, li, 0):
            li += 1
        while _f(len(acc), (ri+1), i) <  _f(len(acc), ri, i):
            ri += 1
        cr = _g(li, i, ri)
        if cr < r:
            r = cr
    return r


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
