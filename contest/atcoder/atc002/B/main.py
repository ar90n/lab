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

MOD = 15  # type: int


def pow_mod(a, k, M):
    if k == 0:
        return 1

    t = pow_mod(a, k // 2, M)
    res = (t * t) % M
    if k % 2 == 1:
        res = (res * a) % M

    return res

def solve(N: int, M: int, P: int):
    return pow_mod(N, P, M) 


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    P = int(next(tokens))  # type: int
    result = solve(N, M, P)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
