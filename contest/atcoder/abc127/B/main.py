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



def main():
    # Failed to predict input format
    r, D, x = readIntN()
    for i in range(10):
        x = r * x - D
        print(x)

if __name__ == '__main__':
    main()
