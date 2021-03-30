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


def solve(N: int, M: int, X: "List[int]"):
    X = sorted(X)
    d = sorted([e - b for b, e in zip(X, X[1:])], reverse=True)
    return X[-1] - X[0] - sum(d[:N-1])


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    X = [ int(next(tokens)) for _ in range(M) ]  # type: "List[int]"
    result = solve(N, M, X)
    print(result)

if __name__ == '__main__':
    main()
