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

MOD = 1000000007  # type: int

def comb_mod(n, m, M):
    return (perm_mod(n, m, M) * inv_mod(fact_mod(m, M), M)) % M

def perm_mod(n, m, M):
    ret = 1
    for i in range(n, n - m, -1):
        ret = (ret * i) % M
    return ret

def inv_mod(a, M):
    return pow_mod(a, M - 2, M)

def fact_mod(a, M):
    ret = 1
    for i in range(2, a + 1):
        ret = (ret * i) % M
    return ret

def pow_mod(a, k, M):
    if k == 0:
        return 1

    t = pow_mod(a, k // 2, M)
    res = (t * t) % M
    if k % 2 == 1:
        res = (res * a) % M

    return res


def solve(N: int, K: int):
    B = K
    R = N - B

    ret = []
    for i in range(K):
        r = (comb_mod(R + 1, i + 1, MOD) *  comb_mod(B - 1, i, MOD)) % MOD
        ret.append(r)
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
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
