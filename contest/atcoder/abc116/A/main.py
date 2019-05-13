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
    a, b, c = readIntN()
    result = a * b // 2
    print(result)

if __name__ == '__main__':
    main()
