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


def dfs(s, N):
    if N < int(s):
        return 0

    ret = 1 if all([c in s for c in '753']) else 0
    for c in '753':
        ret += dfs(s + c, N)
    return ret


def solve(N: int):
    return dfs('0', N)


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    result = solve(N)
    print(result)

if __name__ == '__main__':
    main()
