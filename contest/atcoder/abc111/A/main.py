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


def solve(n: str):
    res = []
    for c in n:
        res.append('9' if c == '1' else '1')
    return int(''.join(res))


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    n = (next(tokens))  # type: int
    result = solve(n)
    print(result)

if __name__ == '__main__':
    main()
