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


def solve(N: int, M: int, P: "List[int]", Y: "List[int]"):
    d = {}
    for i, (p, y) in enumerate(zip(P, Y)):
        d.setdefault(p, []).append((y, i))

    r = []
    for p, vs in d.items():
        r += [(n, "{:06}{:06}".format(p, i+1)) for i, (_, n) in enumerate(sorted(vs))]
    r.sort()

    return '\n'.join([c for _, c in r])


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    P = [int()] * (M)  # type: "List[int]" 
    Y = [int()] * (M)  # type: "List[int]" 
    for i in range(M):
        P[i] = int(next(tokens))
        Y[i] = int(next(tokens))
    result = solve(N, M, P, Y)
    print(result)

if __name__ == '__main__':
    main()
