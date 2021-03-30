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


def solve(N: int, g_A: int, s_A: int, b_A: int, g_B: int, s_B: int, b_B: int):
    return 0


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    g_A = int(next(tokens))  # type: int
    s_A = int(next(tokens))  # type: int
    b_A = int(next(tokens))  # type: int
    g_B = int(next(tokens))  # type: int
    s_B = int(next(tokens))  # type: int
    b_B = int(next(tokens))  # type: int
    result = solve(N, g_A, s_A, b_A, g_B, s_B, b_B)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
