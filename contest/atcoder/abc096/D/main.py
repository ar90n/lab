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
    res = []
    for p in prime_table(55555):
        if p % 5 == 1:
            res.append(p)
            if len(res) == N:
                break
    return res


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    result = solve(N)
    if isinstance(result, Iterable):
        result = ' '.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
