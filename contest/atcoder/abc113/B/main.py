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


def solve(N: int, T: int, A: int, H: "List[int]"):
    d = float('inf')
    ret = 0
    for i, h in enumerate(H):
        if abs(A - (T - h * 0.006)) < d:
            ret = i
            d = abs(A - (T - h * 0.006))
    return ret + 1


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    T = int(next(tokens))  # type: int
    A = int(next(tokens))  # type: int
    H = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, T, A, H)
    print(result)

if __name__ == '__main__':
    main()
