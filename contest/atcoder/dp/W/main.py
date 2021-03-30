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


def solve(N: int, M: int, l: "List[int]", r: "List[int]", a: "List[int]"):
    return 0


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    l = [int()] * (M)  # type: "List[int]" 
    r = [int()] * (M)  # type: "List[int]" 
    a = [int()] * (M)  # type: "List[int]" 
    for i in range(M):
        l[i] = int(next(tokens))
        r[i] = int(next(tokens))
        a[i] = int(next(tokens))
    result = solve(N, M, l, r, a)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
