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


def solve(N: int, p: "List[float]"):
    dp = [[0] * (N + 1) for _ in range(N + 1)]
    dp[0][0] = 1.0

    for i in range(N):
        for j in range(N):
            dp[i+1][j+1] += p[i] * dp[i][j]
            dp[i+1][j] += (1.0 - p[i]) * dp[i][j]
    return sum(dp[-1][(N // 2 + 1):])


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    p = [ float(next(tokens)) for _ in range(N) ]  # type: "List[float]"
    result = solve(N, p)
    print(result)

if __name__ == '__main__':
    main()
