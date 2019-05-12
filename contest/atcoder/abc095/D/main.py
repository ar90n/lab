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


def solve(N: int, C: int, x: "List[int]", v: "List[int]"):
    x = [0] + x
    v = [0] + v

    mx_r = [0]
    for xx, acc in zip(x[1:], accumulate(v[1:], add)):
        mx_r.append(max(acc - xx, mx_r[-1]))

    mx_l = [0]
    for xx, cal in zip(reversed(x), accumulate(reversed(v), add)):
        mx_l.append(max(cal - (C - xx), mx_l[-1]))
    mx_l.reverse()

    ans = 0
    for i in range(N+1):
        ans = max(mx_r[i], mx_r[i] - x[i] + mx_l[i+1], ans)
        if i != 0:
            ans = max(mx_l[i], mx_l[i] - (C - x[i]) + mx_r[i-1], ans)
    return ans


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    C = int(next(tokens))  # type: int
    x = [int()] * (N)  # type: "List[int]" 
    v = [int()] * (N)  # type: "List[int]" 
    for i in range(N):
        x[i] = int(next(tokens))
        v[i] = int(next(tokens))
    result = solve(N, C, x, v)
    if isinstance(result, Iterable):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
