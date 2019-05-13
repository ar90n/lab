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


def solve(N: int, h: "List[int]"):
    ret = 0
    while True:
        b = 0
        while b < len(h) and  h[b] == 0:
            b += 1

        e = b
        while e < len(h) and 0 < h[e]:
            e += 1

        if b == e:
            break

        ret += 1
        for i in range(b, e):
            h[i] -= 1

    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    h = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, h)
    print(result)

if __name__ == '__main__':
    main()
