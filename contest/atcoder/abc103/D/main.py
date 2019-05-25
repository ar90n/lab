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


def solve(N: int, M: int, a: "List[int]", b: "List[int]"):
    qs = sorted(list(zip(a, b)), reverse=True)
    
    ret = 0
    d = float('inf')
    for ai, bi in qs:
        if bi < d:
            ret += 1
            d = ai + 1
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    a = [int()] * (M)  # type: "List[int]" 
    b = [int()] * (M)  # type: "List[int]" 
    for i in range(M):
        a[i] = int(next(tokens))
        b[i] = int(next(tokens))
    result = solve(N, M, a, b)
    print(result)

if __name__ == '__main__':
    main()
