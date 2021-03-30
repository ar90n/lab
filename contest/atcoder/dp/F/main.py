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


def lcs(xs, ys):
    dp = [([0] * (len(ys) + 1)) for _ in range(len(xs) + 1)]
    for i, x in enumerate(xs):
        for j, y in enumerate(ys):
            dp[i + 1][j + 1] = max(dp[i][j + 1], dp[i + 1][j], dp[i][j] + int(x == y))

    s = []
    p = (-1, -1)
    while 0 < dp[p[0]][p[1]]:
        if dp[p[0]][p[1]] == dp[p[0]-1][p[1]]:
            p = (p[0]-1, p[1])
        elif dp[p[0]][p[1]] == (dp[p[0]-1][p[1]-1] + int(xs[p[0]] == ys[p[1]])):
            s.append(xs[p[0]])
            p = (p[0]-1, p[1]-1)
        else:
            p = (p[0], p[1]-1)

    return ''.join(reversed(s))


def solve(s: str, t: str):
    return lcs(s, t)


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    s = next(tokens)  # type: str
    t = next(tokens)  # type: str
    result = solve(s, t)
    print(result)

if __name__ == '__main__':
    main()
