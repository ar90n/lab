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
    n = readInt()
    result = 'Hello World'
    if n == 2:
        a = readInt()
        b = readInt()
        result = a + b
    print(result)

if __name__ == '__main__':
    main()
