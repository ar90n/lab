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
    while len(A) != 1:
        A.sort(reverse=True)
        while A[-1] == 0:
            A.pop()
        for i in range(len(A) - 1):
            A[i] %= A[-1]
    return A[0]


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
