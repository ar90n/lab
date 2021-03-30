#!/usr/bin/env python3
import sys
from collections.abc import Iterable
from math import *
from itertools import *
from collections import *
from functools import *
from operator import *
import bisect
try:
    from math import gcd
except Exception:
    from fractions import gcd


def inchworm(sweep_range, f, g):
    beg, end = sweep_range
    left = beg
    right = beg

    while right < end:
        while right < end and f(left, right):
            right += 1

        while left < right and g(left, right):
            left += 1


def bisect_func(search_range, f):
    lo, hi = search_range
    while (lo+1) < hi:
        mid= (lo + hi) // 2
        if 0 <= f(mid):
            hi = mid
        else:
            lo = mid
    return lo + 1


#def solve(N: int, K: int, a: "List[int]"):
#    acc = 0
#    c = 0
#
#    left = 0
#    right = 0
#
#    def _f(left, right):
#        nonlocal acc
#
#        if K <= acc:
#            return False
#        acc += a[right]
#        return True
#
#    def _g(left, right):
#        nonlocal acc
#        nonlocal c
#
#        if acc < K:
#            return False
#
#        acc -= a[left]
#        c += (N + 1) - right
#        return True
#
#    inchworm((0, N), _f, _g)
#    return c

def solve(N: int, K: int, a: "List[int]"):
    acc = [0] + list(accumulate(a))

    def _f(base, mid):
        return acc[mid] - base -K

    ret = 0
    for i in range(N + 1):
        n = bisect_func((i, len(acc)), partial(_f, acc[i]))
        ret += (N + 1) - n

    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    K = int(next(tokens))  # type: int
    a = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, K, a)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
