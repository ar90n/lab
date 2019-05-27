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


def solve(N: int, x: "List[int]", y: "List[int]", h: "List[int]"):
    for i, j in product(range(0, 101), range(0, 101)):
        H = set()
        for xx, yy, hh in zip(x, y, h):
            if 0 < hh:
                ch = hh + abs(xx - i) + abs(yy - j)
                H.add(ch)
        if len(H) != 1:
            continue

        hhh = list(H)[0]
        no = False
        for xx, yy, hh in zip(x, y, h):
            if 0 == hh:
                if 0 < hhh - abs(xx - i) - abs(yy - j):
                    no = True
        if no:
            continue

        return "{} {} {}".format(i, j, hhh)


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    x = [int()] * (N)  # type: "List[int]" 
    y = [int()] * (N)  # type: "List[int]" 
    h = [int()] * (N)  # type: "List[int]" 
    for i in range(N):
        x[i] = int(next(tokens))
        y[i] = int(next(tokens))
        h[i] = int(next(tokens))
    result = solve(N, x, y, h)
    print(result)

if __name__ == '__main__':
    main()
