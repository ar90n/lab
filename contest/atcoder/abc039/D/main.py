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

YES = "possible"  # type: str
NO = "impossible"  # type: str

def solve(H: int, W: int, S: "List[str]"):
    S0 = [list(s) for s in S]
    S1 = [list(s) for s in S]

    for i in range(H):
        for j in range(W):
            if S[i][j] == '.':
                S1[max(i-1, 0)][j] = '.'
                S1[i][j] = '.'
                S1[min(i+1, W-1)][j] = '.'
    S2 = [list(s) for s in S1]
    print(S1)
    for i in range(W):
        for j in range(H):
            if S1[i][j] == '.':
                S2[j][max(i-1, 0)] = '.'
                S2[j][i] = '.'
                S2[j][min(i+1, W-1)] = '.'
    print(S2)
    for i in range(H):
        for j in range(W):
            if S2[i][j] == '#':
                S1[max(i-1, 0)][j] = '#'
                S1[i][j] = '#'
                S1[min(i+1, W-1)][j] = '#'
    for i in range(W):
        for j in range(H):
            if S1[i][j] == '#':
                S2[j][max(i-1, 0)] = '#'
                S2[j][i] = '#'
                S2[j][min(i+1, W-1)] = '#'
    ret = True
    for i in range(H):
        for j in range(W):
            ret &= S2[i][j] == S[i][j]
    if ret:
        return YES + '\n' + '\n'.join([''.join(s) for s in S2])
    else:
        return NO


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    H = int(next(tokens))  # type: int
    W = int(next(tokens))  # type: int
    S = [ next(tokens) for _ in range(H) ]  # type: "List[str]"
    result = solve(H, W, S)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
