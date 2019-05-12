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


def solve(R: int, G: int, B: int, N: int):
    ret = 0
    for r in range((N // R) + 1):
        rc = R * r
        for g in range((N // G) + 1):
            gc = G * g
            bc = N - rc - gc
            if (0 <= bc) and  (bc % B == 0):
                ret += 1
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    R = int(next(tokens))  # type: int
    G = int(next(tokens))  # type: int
    B = int(next(tokens))  # type: int
    N = int(next(tokens))  # type: int
    result = solve(R, G, B, N)
    if isinstance(result, Iterable):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
