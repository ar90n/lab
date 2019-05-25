#!/usr/bin/env python3
import sys
from math import *
from itertools import *
from collections import *
from functools import *
import operator
try:
    from math import gcd
except Exception:
    from fractions import gcd


def solve(N: int, a: "List[int]"):
    m = reduce(operator.mul, a) - 1
    return sum([m % x for x in a])


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    a = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, a)
    print(result)

if __name__ == '__main__':
    main()
