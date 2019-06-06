#!/usr/bin/env python3
import sys
from math import *
from itertools import *
from collections import *
from functools import *
from operator import *
try:
    from math import gcd
except Exception:
    from fractions import gcd


def solve(N: int, M: int, A: "List[int]"):
    mm = lambda x: mod(x, M)
    A = [0] + A
    c = Counter(map(mm, accumulate(map(mm, A))))
    return sum([v * (v - 1) // 2 for v in c.values()])


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    A = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, M, A)
    print(result)

if __name__ == '__main__':
    main()
