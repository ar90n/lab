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
    rn = int(ceil(sqrt(n)))
    t = [True] * (rn + 1)
    t[0] = False
    t[1] = False

    i = 2
    while i * i <= n:
        for ii in range(2 * i, rn + 1, i):
            t[ii] = False
        i += 1
    return [x for x, i in enumerate(t) if i == True]


def combination(n, m):
    return reduce(mul, range(n, n - m, -1), 1) // factorial(m)

def solve(N: int, M: int):
    fs = defaultdict(int)
    for p in prime_table(M):
        while (M % p) == 0:
            fs[p] += 1
            M //= p
    if M != 1:
        fs[M] = 1

    ret = 1
    for c in fs.values():
        ret *= combination(c + N - 1, c)

    return ret % MOD


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
