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


def solve(s: int):
    res = [0] * 1000001
    a = s
    i = 1
    while True:
        if res[a] == 1:
            break
        res[a] = 1
        if a % 2 == 0:
            a = a // 2
        else:
            a = 3 * a + 1
        i += 1
    return i


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    s = int(next(tokens))  # type: int
    result = solve(s)
    print(result)

if __name__ == '__main__':
    main()
