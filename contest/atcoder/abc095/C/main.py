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


def solve(A: int, B: int, C: int, X: int, Y: int):
    a = A * X + B * Y
    minv = min(X, Y)
    b = C * 2 * minv + A * (X - minv) + B * (Y - minv)
    c = 2 * max(X, Y) * C

    return min(a, b, c)


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    A = int(next(tokens))  # type: int
    B = int(next(tokens))  # type: int
    C = int(next(tokens))  # type: int
    X = int(next(tokens))  # type: int
    Y = int(next(tokens))  # type: int
    result = solve(A, B, C, X, Y)
    if isinstance(result, Iterable):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
