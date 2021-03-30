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

YES = "YES"  # type: str
NO = "NO"  # type: str

def solve(X: int):
    return YES if X in [7, 5, 3] else NO


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    X = int(next(tokens))  # type: int
    result = solve(X)
    print(result)

if __name__ == '__main__':
    main()
