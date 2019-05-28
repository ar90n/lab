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


def solve(D: int, G: int, p: "List[int]", c: "List[int]"):
    w = 2 ** D
    ret = float('inf')
    for i in range(w):
        s = 0
        cc = 0
        ri = 0
        for j in range(D):
            if ((i >> j) & 1) == 1:
                s += 100 * (j + 1) * p[j] + c[j]
                cc += p[j]
            else:
                ri = j
        if s < G:
            for j in range(p[ri] - 1):
                s += 100 * (ri + 1)
                cc += 1
                if G <= s:
                    break
        if G <= s:
            ret = min(ret, cc)
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    D = int(next(tokens))  # type: int
    G = int(next(tokens))  # type: int
    p = [int()] * (D)  # type: "List[int]" 
    c = [int()] * (D)  # type: "List[int]" 
    for i in range(D):
        p[i] = int(next(tokens))
        c[i] = int(next(tokens))
    result = solve(D, G, p, c)
    print(result)

if __name__ == '__main__':
    main()
