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


def solve(x: "List[int]", y: "List[int]"):
    d = (x[1] - x[0], y[1] - y[0])
    x2 = x[1] - d[1] 
    y2 = y[1] + d[0] 
    x3 = x[0] - d[1] 
    y4 = y[0] + d[0] 
    return ' '.join([str(z) for z in [x2, y2, x3, y4]])


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    x = [int()] * (2)  # type: "List[int]" 
    y = [int()] * (2)  # type: "List[int]" 
    for i in range(2):
        x[i] = int(next(tokens))
        y[i] = int(next(tokens))
    result = solve(x, y)
    print(result)

if __name__ == '__main__':
    main()
