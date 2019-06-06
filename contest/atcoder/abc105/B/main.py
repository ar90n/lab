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

YES = "Yes"  # type: str
NO = "No"  # type: str

def solve(N: int):
    for i in range(0, N+1):
        if (i % 4 == 0) and ((N - i) % 7 ==0):
            return YES
    return NO


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
