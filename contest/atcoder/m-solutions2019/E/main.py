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

MOD = 1000003  # type: int

def solve(Q: int, x: "List[int]", d: "List[int]", n: "List[int]"):
    return 0


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    Q = int(next(tokens))  # type: int
    x = [int()] * (Q)  # type: "List[int]" 
    d = [int()] * (Q)  # type: "List[int]" 
    n = [int()] * (Q)  # type: "List[int]" 
    for i in range(Q):
        x[i] = int(next(tokens))
        d[i] = int(next(tokens))
        n[i] = int(next(tokens))
    result = solve(Q, x, d, n)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
