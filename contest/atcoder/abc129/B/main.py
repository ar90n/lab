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


def solve(N: int, W: "List[int]"):
    acc = list(accumulate(W))
    ret = float('inf')
    for i in range(N-1):
        s1 = acc[i]
        s2 = acc[-1] - acc[i]
        ret = min(ret, abs(s1 -s2))
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    W = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, W)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
