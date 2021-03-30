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

YES = "YES"  # type: str
NO = "NO"  # type: str

def solve(H: int, W: int, N: int, s_r: int, s_c: int, S: str, T: str):
    d = {
        'L': 0,
        'R': 0,
        'U': 0,
        'D': 0
    }
    r = {
        'L' : 'R',
        'R' : 'L',
        'U' : 'D',
        'D' : 'U',
    }
    for s, t in zip(S, T):
        d[s] += 1
        if (W < s_c + d['R']) or (s_c - d['L'] < 1) or (H < s_r + d['D']) or (s_r - d['U'] < 1):
            return NO
        d[r[t]] = max(d[r[t]] - 1, 0)
    return YES


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    H = int(next(tokens))  # type: int
    W = int(next(tokens))  # type: int
    N = int(next(tokens))  # type: int
    s_r = int(next(tokens))  # type: int
    s_c = int(next(tokens))  # type: int
    S = next(tokens)  # type: str
    T = next(tokens)  # type: str
    result = solve(H, W, N, s_r, s_c, S, T)
    print(result)

if __name__ == '__main__':
    main()
