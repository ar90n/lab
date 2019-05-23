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



def solve(N: int, X: int):
    a, p = [1], [1]
    for i in range(N):
        a.append(a[i] * 2 + 3)
        p.append(p[i] * 2 + 1)

    def _f(N, X):
        if N == 0:
            return 0 if X <= 0 else 1
        elif X < (2 + a[N-1]):
            return _f(N-1, X-1)
        elif X == (2 + a[N-1]):
            return 1 + p[N-1]
        elif X < a[N]:
            return p[N-1] + 1 + _f(N-1, X - 2 - a[N-1])
        else:
            return p[N]

    return _f(N, X)


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    X = int(next(tokens))  # type: int
    result = solve(N, X)
    print(result)

if __name__ == '__main__':
    main()
