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


def solve(A: "List[int]"):
    A.sort()
    return A[-1] - A[0]


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    A = [ int(next(tokens)) for _ in range(3) ]  # type: "List[int]"
    result = solve(A)
    print(result)

if __name__ == '__main__':
    main()
