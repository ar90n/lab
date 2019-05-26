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


def solve(N: int, A: "List[int]"):
    A = sorted([a - i for i, a in enumerate(A, 1)])
    b = A[len(A) // 2]
    return sum([abs(a - b) for a in A])


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    A = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, A)
    print(result)

if __name__ == '__main__':
    main()
