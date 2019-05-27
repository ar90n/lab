#!/usr/bin/env python3
import sys
import bisect
from math import *
from itertools import *
from collections import *
from functools import *
try:
    from math import gcd
except Exception:
    from fractions import gcd


def solve(N: int, M: int):
    ds = []
    for i in range(1, int(ceil(sqrt(M))) + 1):
        if M % i == 0:
            ds.append(i)
            ds.append(M // i)
    ds.sort()
  
    th = M // N
    return ds[bisect.bisect(ds, th)-1]


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    result = solve(N, M)
    print(result)

if __name__ == '__main__':
    main()
