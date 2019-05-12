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


def solve(N: int, x: "List[float]", u: "List[str]"):
    ret = 0
    for xx, uu in zip(x, u):
        ret += xx * {
            'JPY': 1.0,
            'BTC': 380000.0
        }[uu]
    return ret

def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    x = [float()] * (N)  # type: "List[float]" 
    u = [str()] * (N)  # type: "List[str]" 
    for i in range(N):
        x[i] = float(next(tokens))
        u[i] = next(tokens)
    result = solve(N, x, u)
    print(result)

if __name__ == '__main__':
    main()
