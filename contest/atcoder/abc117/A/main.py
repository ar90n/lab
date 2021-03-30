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


def solve(T: int, X: int):
    return  T / X


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    T = int(next(tokens))  # type: int
    X = int(next(tokens))  # type: int
    result = solve(T, X)
    print(result)

if __name__ == '__main__':
    main()
