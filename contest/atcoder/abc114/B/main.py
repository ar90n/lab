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


def solve(S: int):
    ret = float('inf')
    for i in range(len(S) - 2):
        ret = min(ret, abs(753 - int(S[i:i+3])))
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    S = (next(tokens))  # type: int
    result = solve(S)
    print(result)

if __name__ == '__main__':
    main()
