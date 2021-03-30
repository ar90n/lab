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


def solve(N: int, W: int, w: "List[int]", v: "List[int]"):
    dp = [[0] * (W + 1) for _ in range(N + 1)]

    for i, (cw, cv) in enumerate(zip(w, v)):
        for j in range(W + 1):
            dp[i+1][j] = dp[i][j]
            if 0 <= j - cw :
                dp[i+1][j] = max(dp[i+1][j], dp[i][j - cw] + cv)
    return max(dp[-1])


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    W = int(next(tokens))  # type: int
    w = [int()] * (N)  # type: "List[int]" 
    v = [int()] * (N)  # type: "List[int]" 
    for i in range(N):
        w[i] = int(next(tokens))
        v[i] = int(next(tokens))
    result = solve(N, W, w, v)
    print(result)

if __name__ == '__main__':
    main()
