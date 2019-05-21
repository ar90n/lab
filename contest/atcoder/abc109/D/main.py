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



def solve(H: int, W: int, a: "List[List[int]]"):
    def xy(i):
        y = i // W
        x = (i % W)
        if (y % 2 == 1):
            x = (W - 1) - x
        return y ,x

    result = []
    for i in range(H * W-1):
        y, x = xy(i)
        if (a[y][x] % 2) == 1:
            y2, x2 = xy(i+1)
            a[y][x] -= 1
            a[y2][x2] += 1
            result.append("{} {} {} {}".format((y+1), (x+1), (y2+1), (x2+1)))

    return str(len(result)) + '\n' + '\n'.join(result)


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    H = int(next(tokens))  # type: int
    W = int(next(tokens))  # type: int
    a = [ [ int(next(tokens)) for _ in range(W) ] for _ in range(H) ]  # type: "List[List[int]]"
    result = solve(H, W, a)
    print(result)

if __name__ == '__main__':
    main()
