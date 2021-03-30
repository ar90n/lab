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

MOD = 2019  # type: int

def solve(L: int, R: int):
    R = min(R, L + MOD)

    ret = 2020
    for i in range(L, R + 1):
        for j in range(L, R + 1):
            if j <= i:
                continue
            tmp = ((i % MOD) * (j % MOD)) % MOD
            ret= min(ret, tmp)
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    L = int(next(tokens))  # type: int
    R = int(next(tokens))  # type: int
    result = solve(L, R)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
