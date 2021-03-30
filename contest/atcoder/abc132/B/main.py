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


def solve(n: int, p: "List[int]"):
    ret = 0
    for p0, p1, p2 in zip(p, p[1:], p[2:]):
        s0, s1, s2 = sorted([p0, p1, p2])
        ret += int(s1 == p1)
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    n = int(next(tokens))  # type: int
    p = [ int(next(tokens)) for _ in range(n) ]  # type: "List[int]"
    result = solve(n, p)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
