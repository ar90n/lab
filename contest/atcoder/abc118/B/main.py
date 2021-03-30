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
    n, m = readIntN()
    res = set(readIntN()[1:])
    for i in range(n-1):
        res = res.intersection(set(readIntN()[1:]))
    print(len(res))

if __name__ == '__main__':
    main()
