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

MOD = 1000000007  # type: int

def solve(N: int, M: int, S: "List[int]", T: "List[int]"):
    dp = [[0] * (M + 1) for _ in range(N + 1)]

    for i in range(N):
        for j in range(M):
            dp[i + 1][j + 1] = (dp[i+1][j] + dp[i][j+1] - dp[i][j]) % MOD
            if S[i] == T[j]:
                dp[i+1][j+1] += dp[i][j] + 1
            dp[i+1][j+1] %= MOD
    return (dp[-1][-1] + 1) % MOD


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    S = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    T = [ int(next(tokens)) for _ in range(M) ]  # type: "List[int]"
    result = solve(N, M, S, T)
    print(result)

if __name__ == '__main__':
    main()
