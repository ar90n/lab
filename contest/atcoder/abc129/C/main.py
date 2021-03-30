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

def solve(N: int, M: int, a: "List[int]"):
    dp = [0] * (N + 2)
    dp[0] = 1
    a =set(a)

    for i in range(1, N+1):
        if i in a:
            dp[i] = 0
        else:
            dp[i] = (dp[i-1] + dp[i-2]) % MOD
    return dp[N]


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    a = [ int(next(tokens)) for _ in range(M) ]  # type: "List[int]"
    result = solve(N, M, a)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
