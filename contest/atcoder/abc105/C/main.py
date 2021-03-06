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


def solve(N: int):
    ret = []
    while 0 < abs(N):
        if N % -2  == 0:
            N //= -2
            ret.append('0')
        else:
            N = (N - 1) // -2
            ret.append('1')
    if ret == []:
        ret = ['0']
    return ''.join(reversed(ret))


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    result = solve(N)
    print(result)

if __name__ == '__main__':
    main()
