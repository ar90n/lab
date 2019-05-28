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

MOD = 1000000007  # type: int

def solve(S: str):
    dp = [[0] * 5 for _ in range(len(S) + 1)]
    dp[0][1] = 1

    for i, s in enumerate(S):
        for j, a in enumerate('_ABC', 1):
            dp[i+1][j] = (3 * dp[i][j]) if s == '?' else dp[i][j]
            if s == a or s == '?':
                dp[i+1][j] += dp[i][j-1]
            dp[i+1][j] %= MOD
    return dp[-1][-1] % MOD


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    S = next(tokens)  # type: str
    result = solve(S)
    print(result)

if __name__ == '__main__':
    main()
