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


def solve(N: int, M: int, Q: int, L: "List[int]", R: "List[int]", p: "List[int]", q: "List[int]"):
    dp = [[0] * (N + 1) for _ in range(N + 1)]
    for l, r in zip(L, R):
        dp[l-1][r-1] += 1

    for i in range(0, N):
        for j in range(0, N):
            dp[i][j] += dp[i-1][j] + dp[i][j-1] - dp[i-1][j-1]
    ret = []
    for pp, qq in zip(p, q):
        c = dp[qq-1][qq-1] - dp[pp - 2][qq - 1] - dp[qq - 1][pp - 2] + dp[pp - 2][pp - 2]
        ret.append(c)

    return '\n'.join([str(r) for r in ret])


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    Q = int(next(tokens))  # type: int
    L = [int()] * (M)  # type: "List[int]" 
    R = [int()] * (M)  # type: "List[int]" 
    for i in range(M):
        L[i] = int(next(tokens))
        R[i] = int(next(tokens))
    p = [int()] * (Q)  # type: "List[int]" 
    q = [int()] * (Q)  # type: "List[int]" 
    for i in range(Q):
        p[i] = int(next(tokens))
        q[i] = int(next(tokens))
    result = solve(N, M, Q, L, R, p, q)
    print(result)

if __name__ == '__main__':
    main()
