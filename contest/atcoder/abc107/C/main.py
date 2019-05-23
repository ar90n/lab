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


def solve(N: int, K: int, x: "List[int]"):
    ret = float('inf')
    for l, r in zip(x, x[K-1:]):
        ret = min(ret, abs(r - l) + min(abs(l), abs(r)))
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    K = int(next(tokens))  # type: int
    x = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, K, x)
    print(result)

if __name__ == '__main__':
    main()
