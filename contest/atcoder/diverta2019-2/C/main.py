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


def solve(N: int, A: "List[int]"):
    A = sorted(A)
    accm = A[0]
    accp = A[-1]
    pas = deque(sorted([a for a in A[1:-1] if 0 <= a], reverse = True))
    mas = deque(sorted([a for a in A[1:-1] if a < 0]))

    result = []
    while 0 < len(pas):
        p = pas.popleft()
        result.append("{} {}".format(accm, p))
        accm -= p
    result.append("{} {}".format(accp, accm))
    accp -= accm

    while 0 < len(mas):
        m = mas.popleft()
        result.append("{} {}".format(accp, m))
        accp -= m
    result = [str(accp)] + result
    return result


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    A = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, A)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
