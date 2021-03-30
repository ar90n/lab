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

MOD = 2  # type: int


def main():
    N, M = readIntN()
    ks = []
    ss = []
    for i in range(M):
        tmp = readIntN()
        ks.append(tmp[0])
        ss.append(tmp[1:])
    ps = readIntN()

    qs = [] 
    for s in ss:
        q = 0
        for c in s:
            q |= (1 << (c - 1))
        qs.append(q)

    ret = 0
    for i in range(1 << N):
        ok = True
        for q, p in zip(qs, ps):
            ok &= (bin(i & q).count('1') % MOD) == p
        if ok:
            ret += 1
    print(ret)

if __name__ == '__main__':
    main()
