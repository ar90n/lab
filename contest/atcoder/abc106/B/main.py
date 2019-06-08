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


def solve(N: int):
    ret = 0
    for i in range(1, N + 1 , 2):
        c = 0
        for j in range(1, i + 1):
            c += int((i % j) == 0)

        if c == 8:
            ret += 1
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    result = solve(N)
    print(result)

if __name__ == '__main__':
    main()
