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


def solve(N: int, h: "List[int]"):
    dp = [float('inf')] * N
    dp[0] = 0

    for i in range(len(dp)):
        if i < N - 1:
            dp[i+1] = min(dp[i+1], dp[i] + abs(h[i] - h[i+1]))
        if i < N - 2:
            dp[i+2] = min(dp[i+2], dp[i] + abs(h[i] - h[i+2]))
    return dp[-1]


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    h = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, h)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
