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

def solve(L):
    dp = [[0] * 2 for _ in range(len(L) + 1)]
    dp[0][0] = 1

    for i, c in enumerate(L):
        # a=0, b=0
        if c == '0':
            dp[i+1][0] = dp[i][0]
            dp[i+1][1] = dp[i][1]
        else:
            dp[i+1][1] = (dp[i][0] + dp[i][1])

        # a=1, b=0
        if c == '0':
            dp[i+1][1] += dp[i][1]
        else:
            dp[i+1][0] += dp[i][0]
            dp[i+1][1] += dp[i][1]

        # a=0, b=1
        if c == '0':
            dp[i+1][1] += dp[i][1]
        else:
            dp[i+1][0] += dp[i][0]
            dp[i+1][1] += dp[i][1]

        dp[i+1][0] %= MOD
        dp[i+1][1] %= MOD

    return sum(dp[-1]) % MOD


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    L = next(tokens)  # type: int
    result = solve(L)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
