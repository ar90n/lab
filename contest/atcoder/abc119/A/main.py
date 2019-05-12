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


def solve(S: str):
    y, m, d = [int(x) for x in S.split('/')]
    if y < 2019:
        return 'Heisei'
    if y == 2019 and m <= 4:
        return 'Heisei'
    return 'TBD'


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    S = next(tokens)  # type: str
    result = solve(S)
    print(result)

if __name__ == '__main__':
    main()
