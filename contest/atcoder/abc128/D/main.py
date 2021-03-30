#!/usr/bin/env python3
import sys
import heapq
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


def solve(N: int, K: int, V: "List[int]"):

    lq = []
    acc_l = 0
    acc_ml = []
    ret = 0
    for li in range(min(N+1, K + 1)):
        if 0 < li:
            acc_l += V[li - 1]
            if V[li - 1] < 0:
                acc_ml.append(V[li - 1])

        if ret < acc_l:
            ret = acc_l
        acc_r = 0
        ms = [v for v in acc_ml]
        ms.sort()
        for _ri in range(min(N+1, K + 1) - li):
            if 0 < _ri:
                ri = N - _ri
                acc_r += V[ri]
                if V[ri] < 0:
                    ms.append(V[ri])
                    ms.sort()
            ci = min(len(ms), K - li - _ri)
            acc_c = sum(ms[:ci])
            acc = acc_l + acc_r - acc_c
            if ret < acc:
                ret = acc
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    K = int(next(tokens))  # type: int
    V = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, K, V)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
