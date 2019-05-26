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


def solve(N: int, K: int):
    ns0 = len(range(K, N+1, K))
    ret = ns0 ** 3

    if K % 2 == 0:
        ns1 = len(range(K//2, N+1, K))
        ret += ns1 ** 3

    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    K = int(next(tokens))  # type: int
    result = solve(N, K)
    print(result)

if __name__ == '__main__':
    main()
