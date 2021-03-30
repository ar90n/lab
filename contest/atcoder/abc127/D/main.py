#!/usr/bin/env python3
import sys
import heapq
import bisect
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


def solve(N: int, M: int, A: "List[int]", B: "List[int]", C: "List[int]"):
    ret = sum(A)
    cs = Counter(A)
    for c in C:
        if c not in cs:
            cs[c] = 0
    vs = sorted(cs.keys())

    i = 0
    while cs[vs[i]] == 0:
        i += 1

    for b, c in zip(B, C):
        while 0 < b:
            if c <= vs[i]:
                break
            bb = min(b, cs[vs[i]])
            b -= bb
            cs[vs[i]] -= bb
            cs[c] += bb
            ret += bb * (c - vs[i])
            if cs[vs[i]] == 0:
                i += 1
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    A = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    B = [int()] * (M)  # type: "List[int]" 
    C = [int()] * (M)  # type: "List[int]" 
    for i in range(M):
        B[i] = int(next(tokens))
        C[i] = int(next(tokens))
    result = solve(N, M, A, B, C)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
