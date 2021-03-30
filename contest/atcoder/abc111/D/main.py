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


def solve(N: int, X: "List[int]", Y: "List[int]"):
    return 0


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    X = [int()] * (N)  # type: "List[int]" 
    Y = [int()] * (N)  # type: "List[int]" 
    for i in range(N):
        X[i] = int(next(tokens))
        Y[i] = int(next(tokens))
    result = solve(N, X, Y)
    print(result)

if __name__ == '__main__':
    main()
