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


def solve(N: int, M: int, X: int, Y: int, x: "List[int]", y: "List[int]"):
    return 'No War' if X < max(x) < min(y) <= Y else 'War'


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    X = int(next(tokens))  # type: int
    Y = int(next(tokens))  # type: int
    x = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    y = [ int(next(tokens)) for _ in range(M) ]  # type: "List[int]"
    result = solve(N, M, X, Y, x, y)
    print(result)

if __name__ == '__main__':
    main()
