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

def solve(N: int, W: "List[str]"):
    s = set([W[0]])
    for i in range(1, len(W)):
        if (W[i] in s) or (W[i-1][-1] != W[i][0]):
            return NO
        s.add(W[i])
    return YES


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    W = [ next(tokens) for _ in range(N) ]  # type: "List[str]"
    result = solve(N, W)
    print(result)

if __name__ == '__main__':
    main()
