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

def solve(H: int, W: int, K: int):
    dp = [[0] * (W + 2) for _ in range(H+1)]
    dp[0][1] = 1

    for h in range(H):
        for i in range(0, (1 << W), 2):
            f = True
            c = [0] * (W + 2)
            for w in range(1, W + 1):
                if ((i >> (w-1)) & 0x03) == 3:
                    f = False
                    break
                if i & (1 << (w - 1)):
                    c[w-1] += dp[h][w]
                elif i & (1 << w):
                    c[w+1] += dp[h][w]
                else:
                    c[w] += dp[h][w]
            if f:
                for j, cc in enumerate(c):
                    dp[h+1][j] += cc

    return dp[-1][K] % MOD


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    H = int(next(tokens))  # type: int
    W = int(next(tokens))  # type: int
    K = int(next(tokens))  # type: int
    result = solve(H, W, K)
    print(result)

if __name__ == '__main__':
    main()
