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


def solve(N: int, A: int, B: int, C: int, l: "List[int]"):
    e = [A, B, C]

    ret = float('inf')
    for i in range(4 ** N):
        ls = [0, 0, 0, 0]
        cs = [0, 0, 0, 0]
        for j, ll in enumerate(l):
            key = (i >> (2 * j)) & 0x03
            ls[key] += ll
            cs[key] += 10

        if cs[0] == 0 or cs[1] == 0 or cs[2] == 0:
            continue

        for i0, i1, i2 in permutations([0, 1, 2], 3):
            cc = cs[0] + abs(e[i0] - ls[0]) - 10
            cc += cs[1] + abs(e[i1] - ls[1]) - 10
            cc += cs[2] + abs(e[i2] - ls[2]) - 10
            if cc < ret:
                ret = cc
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    A = int(next(tokens))  # type: int
    B = int(next(tokens))  # type: int
    C = int(next(tokens))  # type: int
    l = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, A, B, C, l)
    print(result)

if __name__ == '__main__':
    main()
