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


def solve(A: int, B: int, C: int, D: int):
    def doit(N):
        a = A
        if a % N != 0:
            a += (N - A % N)
        return (B - a) // N + 1
    cs = doit(C)
    ds = doit(D)

    E = (C * D) // gcd(C, D)
    es = doit(E)

    return (B - A + 1) - cs - ds + es


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    A = int(next(tokens))  # type: int
    B = int(next(tokens))  # type: int
    C = int(next(tokens))  # type: int
    D = int(next(tokens))  # type: int
    result = solve(A, B, C, D)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
