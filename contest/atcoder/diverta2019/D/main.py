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


def solve(N: int):
    ret = 0

    for i in range(1, int(sqrt(N)) + 1):
        m = (N-1) // i
        if m == 0:
            continue
        if (N // m) == (N % m):
            ret += m
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    result = solve(N)
    if isinstance(result, Iterable):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()

