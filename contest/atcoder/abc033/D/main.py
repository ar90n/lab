#!/usr/bin/env python3
import sys
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

eps = 10 * sys.float_info.epsilon

def solve(N: int, x: "List[int]", y: "List[int]"):
    s1 = 0
    s2 = 0
    for i, (cx, cy) in enumerate(zip(x, y)):
        s = sorted([atan2(yy - cy, xx - cx) for j, (xx, yy) in enumerate(zip(x, y)) if i != j])
        ss = s + [v + 2 * pi for v in s]

        for j, cs in enumerate(s):
            i0 = bisect.bisect_left(ss, cs + (pi - 2 * eps) / 2, 0, len(ss))
            i1 = bisect.bisect_left(ss, cs + (pi + 2 * eps) / 2, 0, len(ss))
            i2 = bisect.bisect_left(ss, cs + pi, 0, len(ss))

            s1 += i1 - i0
            s2 += i2 - i1
    sa = N * (N-1) * (N-2) // 6
    return ' '.join([str(sa - s1 - s2), str(s1), str(s2)])


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    x = [int()] * (N)  # type: "List[int]" 
    y = [int()] * (N)  # type: "List[int]" 
    for i in range(N):
        x[i] = int(next(tokens))
        y[i] = int(next(tokens))
    result = solve(N, x, y)
    print(result)

if __name__ == '__main__':
    main()
