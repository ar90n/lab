#!/usr/bin/env python3
import sys
from math import *
from itertools import *
from collections import *
from functools import *
from heapq import *
try:
    from math import gcd
except Exception:
    from fractions import gcd


def solve(N: int, K: int, t: "List[int]", d: "List[int]"):
    dt = sorted(zip(d, t), reverse=True)

    q0 = []

    acc = 0
    ts = 0
    p = [-1] * (N+1)
    for dd, tt in dt[:K]:
        acc += dd
        v = min(p[tt], dd)
        p[tt] = max(p[tt], dd)
        if 0 < v:
            heappush(q0, (v, tt))
        else:
            ts += 1

    q1 = []
    for dd, tt in dt[K:]:
        heappush(q1, (-dd, tt))

    mmm = acc + ts * ts
    while 0 < len(q1) and 0 < len(q0):
        d1, t1 = heappop(q1)
        if 0 < p[t1]:
            continue
        acc -= d1
        p[t1] = -d1
        ts += 1

        d0, _ = heappop(q0)
        acc -= d0
        mmm = max(mmm, acc + ts * ts)

    return mmm


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    K = int(next(tokens))  # type: int
    t = [int()] * (N)  # type: "List[int]" 
    d = [int()] * (N)  # type: "List[int]" 
    for i in range(N):
        t[i] = int(next(tokens))
        d[i] = int(next(tokens))
    result = solve(N, K, t, d)
    print(result)

if __name__ == '__main__':
    main()
