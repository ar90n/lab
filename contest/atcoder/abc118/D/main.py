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


def solve(N: int, M: int, A: "List[int]"):
    A.sort(reverse=True)
    cs = [0, 2 , 5 , 5 , 4 , 5 , 6 , 3 , 7 , 6 ]
    dp = [-float('inf')] * (N + 1)

    dp[0] = 0
    for i in range(1, len(dp)):
        for a in A:
            c = cs[a]
            if 0 <= i - c:
                dp[i] = max(dp[i], dp[i-c] + 1)
    i = N
    ret = 0
    while 0 < dp[i]:
        for a in A:
            c = cs[a]
            if (0 <= i - c) and (dp[i] == (dp[i - c] + 1)):
                i -= c
                ret = 10 * ret + a
                break
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    A = [ int(next(tokens)) for _ in range(M) ]  # type: "List[int]"
    result = solve(N, M, A)
    print(result)

if __name__ == '__main__':
    main()
