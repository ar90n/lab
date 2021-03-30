#!/usr/bin/env python3
import sys
from collections.abc import Iterable
from math import *
from itertools import *
from collections import *
from functools import *
from operator import *
try:
    from math import gcd
except Exception:
    from fractions import gcd


def solve(N: int, x: "List[int]", y: "List[int]"):
    ds = []
    for i in range(N):
        for j in range(N):
            if i == j:
                continue
            dx = x[j] - x[i]
            dy = y[j] - y[i]
            ds.append((dx, dy))

    return 1 if N == 1 else (N - max(Counter(ds).values()))


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    x = [int()] * (N)  # type: "List[int]" 
    y = [int()] * (N)  # type: "List[int]" 
    for i in range(N):
        x[i] = int(next(tokens))
        y[i] = int(next(tokens))
    result = solve(N, x, y)
    print(result)

if __name__ == '__main__':
    main()
