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

def solve(S: str, T: str):
    ds = {}
    dt = {}
    cs = []
    ct = []
    for s, t in zip(S, T):
        if s in ds:
            cs.append(ds[s])
        else:
            cc = len(ds) + 1
            ds[s] = cc
            cs.append(cc)
        if t in dt:
            ct.append(dt[t])
        else:
            cc = len(dt) + 1
            dt[t] = cc
            ct.append(cc)
    return YES if cs == ct else NO


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    S = next(tokens)  # type: str
    T = next(tokens)  # type: str
    result = solve(S, T)
    print(result)

if __name__ == '__main__':
    main()
