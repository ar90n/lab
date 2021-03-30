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


def solve(N: int, K: int, h: "List[int]"):
    dp = [float('inf')] * N
    dp[0] = 0

    for i in range(N):
        for j in range(1, K+1):
            if (i + j) < N:
                dp[i+j] = min(dp[i+j], dp[i] + abs(h[i] - h[i+j]))
    return dp[-1]


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    K = int(next(tokens))  # type: int
    h = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, K, h)
    print(result)

if __name__ == '__main__':
    main()
