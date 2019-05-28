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

MOD = 1000000007  # type: int


def prime_table(n):
    t = [True] * (n + 1)
    t[0] = False
    t[1] = False

    for p in range(2, n + 1, 2):
        if n < p ** 2:
            break
        if t[p]:
            for i in range(p * p, n + 1, 2 * p):
                t[i] = False
    return [2] + [p for p in range(3, n + 1, 2) if t[p]]


def pow_mod(a, k, M):
    if k == 0:
        return 1
    t = pow_mod(a, k // 2, M)
    res = (t * t) % M
    if k % 2 == 1:
        res = (res * a) % M

    return res


def inv_mod(a, M):
    return pow_mod(a, M - 2, M)


def fact_mod(a, M):
    ret = 1
    for i in range(2, a + 1):
        ret = (ret * i) % M
    return ret

def perm_mod(n, m, M):
    ret = 1
    for i in range(n, n - m, -1):
        ret = (ret * i) % M
    return ret


def comb_mod(n, m, M):
    return (perm_mod(n, m, M) * inv_mod(fact_mod(m, M), M)) % M


def solve(N: int, M: int):
    fs = defaultdict(int)
    for p in prime_table(int(M ** 0.5) + 1):
        while (M % p) == 0:
            fs[p] += 1
            M //= p
    if M != 1:
        fs[M] = 1

    ret = 1
    for c in fs.values():
        ret = (ret * comb_mod(c + N - 1, c, MOD)) % MOD

    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    result = solve(N, M)
    print(result)

if __name__ == '__main__':
    main()
