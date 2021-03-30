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

def solve(N: int, L: "List[int]"):
    ll = sorted(L)
    return YES if ll[-1] < sum(ll[:-1]) else NO


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    L = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, L)
    print(result)

if __name__ == '__main__':
    main()
