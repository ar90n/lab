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


def solve(N: int, M: int, L: "List[int]", R: "List[int]"):
    maxl = -float('inf')
    minr = float('inf')
    for l, r in zip(L, R):
        maxl = max(l, maxl)
        minr = min(r, minr)
    
    return max(0, (minr - maxl + 1))


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    L = [int()] * (M)  # type: "List[int]" 
    R = [int()] * (M)  # type: "List[int]" 
    for i in range(M):
        L[i] = int(next(tokens))
        R[i] = int(next(tokens))
    result = solve(N, M, L, R)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
