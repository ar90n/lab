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

def solve(H: int, W: int, a: "List[str]"):
    dp = [[0] * W for _ in range(H)]
    dp[0][0] = 1

    for i in range(1, W):
        dp[0][i] = dp[0][i-1] if a[0][i] == '.' else 0

    for i in range(1, H):
        dp[i][0] = dp[i-1][0] if a[i][0] == '.' else 0
    
    for i in range(1, H):
        for j in range(1, W):
            if a[i][j] == '.':
                dp[i][j] = (dp[i-1][j] + dp[i][j-1]) % MOD
    return dp[-1][-1]


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    H = int(next(tokens))  # type: int
    W = int(next(tokens))  # type: int
    a = [ next(tokens) for _ in range(H) ]  # type: "List[str]"
    result = solve(H, W, a)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
