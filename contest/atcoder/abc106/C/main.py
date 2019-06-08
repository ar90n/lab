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


def solve(S: int, K: int):
    ret = 1
    for i, s in enumerate(str(S)):
        if i == (K - 1):
            ret = s
            break
        if s != '1':
            ret = s
            break
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    S = int(next(tokens))  # type: int
    K = int(next(tokens))  # type: int
    result = solve(S, K)
    print(result)

if __name__ == '__main__':
    main()
