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


def solve(N: int, X: int, x: "List[int]"):
    return reduce(gcd, [abs(X - xx) for xx in x])


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    X = int(next(tokens))  # type: int
    x = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, X, x)
    print(result)

if __name__ == '__main__':
    main()
