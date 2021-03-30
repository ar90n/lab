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


def solve(N: int, a: "List[int]", b: "List[int]", c: "List[int]"):
    dp = [[0] * 3 for _ in range(N+1)]

    for i, (ca, cb, cc) in enumerate(zip(a, b, c)):
        dp[i + 1][0] = max(dp[i][1], dp[i][2]) + ca
        dp[i + 1][1] = max(dp[i][0], dp[i][2]) + cb
        dp[i + 1][2] = max(dp[i][0], dp[i][1]) + cc
    return max(dp[-1])


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    a = [int()] * (N)  # type: "List[int]" 
    b = [int()] * (N)  # type: "List[int]" 
    c = [int()] * (N)  # type: "List[int]" 
    for i in range(N):
        a[i] = int(next(tokens))
        b[i] = int(next(tokens))
        c[i] = int(next(tokens))
    result = solve(N, a, b, c)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
