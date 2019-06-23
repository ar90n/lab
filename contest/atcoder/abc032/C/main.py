#!/usr/bin/env python3
import sys
from collections.abc import Iterable
from math import *
from itertools import *
from collections import *
from functools import *
from operator import *
from bisect import *
try:
    from math import gcd
except Exception:
    from fractions import gcd



def solve(N: int, K: int, s: "List[int]"):
    if 0 in s:
        return N
    if K == 0:
        return 0

    left = 0
    right = 0
    ret = 0
    acc = 1
    while min(right, left) < N:
        while right < N and acc <= K:
            ret = max(ret, right - left)
            acc *= s[right]
            right += 1
        if acc <= K:
            ret = max(ret, right - left)

        acc /= s[left]
        left += 1
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    K = int(next(tokens))  # type: int
    s = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, K, s)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
