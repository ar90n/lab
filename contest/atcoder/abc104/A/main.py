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


def solve(R: int):
    if R < 1200:
        return 'ABC'
    elif R < 2800:
        return 'ARC'
    return 'AGC'


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    R = int(next(tokens))  # type: int
    result = solve(R)
    print(result)

if __name__ == '__main__':
    main()
