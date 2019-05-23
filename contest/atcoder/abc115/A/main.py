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


def solve(D: int):
    return {
            25: 'Christmas', 
            24: 'Christmas Eve', 
            23: 'Christmas Eve Eve', 
            22: 'Christmas Eve Eve Eve'
    }[D]


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    D = int(next(tokens))  # type: int
    result = solve(D)
    print(result)

if __name__ == '__main__':
    main()
