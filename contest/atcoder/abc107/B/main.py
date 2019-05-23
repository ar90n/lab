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
readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]



def main():
    h, w = readIntN()
    a = []
    for _ in range(h):
        s = input()
        if list(set(s)) != ['.']:
            a.append(s)
    r = []
    for i in range(w):
        ll = [s[i] for s in a]
        if len(set(ll)) == 1 and '.' in ll:
            continue
        r.append(ll)

    for j in range(len(r[0])):
        u = [r[i][j] for i in range(len(r))]
        print(''.join(u))

if __name__ == '__main__':
    main()
