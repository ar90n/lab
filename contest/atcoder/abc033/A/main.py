#!/usr/bin/env python3
import sys
from collections.abc import Iterable
from math import *
from itertools import *
from collections import *
from functools import *
from operator import *
try:
    from math import gcd
except Exception:
    from fractions import gcd


def solve(N: str):
    return 'DIFFERENT' if len(set(N)) != 1 else 'SAME'


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = (next(tokens))  # type: int
    result = solve(N)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
