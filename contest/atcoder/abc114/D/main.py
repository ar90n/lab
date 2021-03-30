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


def prime_table(n):
    t = [True] * (n + 1)
    t[0] = False
    t[1] = False

    i = 2
    while i * i <= n:
        for ii in range(2 * i, n + 1, i):
            t[ii] = False
        i += 1
    return [x for x, i in enumerate(t) if i == True]


def solve(N: int):
    t = prime_table(100)

    ds = defaultdict(int)
    for i in range(1, N+1):
        for j in t:
            while i % j == 0:
                ds[j] += 1
                i = i // j

    ret = 0
    fs = list(ds.keys())
    for pi in range(len(fs)):
        p = ds[fs[pi]]
        if 74 <= p:
            ret += 1
        for qi in range(pi + 1, len(fs)):
            q = ds[fs[qi]]
            for cs in [[14, 4], [24, 2]]:
                if cs[0] <= p and cs[1] <= q:
                    ret += 1
                if cs[1] <= p and cs[0] <= q:
                    ret += 1
            for ri in range(qi + 1, len(fs)):
                r = ds[fs[ri]]
                if 2 <= p and 4 <= q and 4 <= r:
                    ret += 1
                if 4 <= p and 2 <= q and 4 <= r:
                    ret += 1
                if 4 <= p and 4 <= q and 2 <= r:
                    ret += 1
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    result = solve(N)
    print(result)

if __name__ == '__main__':
    main()
