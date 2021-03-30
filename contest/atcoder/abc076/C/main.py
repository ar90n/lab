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
readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]



def solve(S, T):
    res = []
    for i in range(len(S) - len(T) + 1):
        cc = S[:i] + T + S[i+len(T):]
        q = set([s for s,c in zip(S, cc) if s != c])
        if q == set(['?']) or q == set():
            res.append(cc.replace('?', 'a'))

    res.sort()
    if 0 < len(res):
        return res[0]

    return 'UNRESTORABLE'


def main():
    S = input()
    T = input()
    result = solve(S, T)
    print(result)

if __name__ == '__main__':
    main()
