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


def solve(N: int, K: int):
    g = [[1] * N for _ in range(N)]
    for i in range(N):
        g[i][i] = 0

    k = K
    for i in range(N):
        if k == 0:
            break
        for j in range(N - 1):
            if j <= i:
                continue

            g[i][j] = 0
            g[j][i] = 0
            k -= 1
            if k == 0:
                break

    if 0 < k:
        return '-1'

    ret = []
    for i in range(N):
        for j in range(N):
            if j <= i:
                continue

            if g[i][j] != 0:
                ret.append('{} {}'.format(i+1, j+1))
    ret = [str(len(ret))] + ret
    return '\n'.join(ret)


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    K = int(next(tokens))  # type: int
    result = solve(N, K)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
