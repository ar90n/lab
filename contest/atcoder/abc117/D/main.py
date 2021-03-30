#!/usr/bin/env python3
import sys
from math import *
from itertools import *
from collections import *
from functools import *
try:
    from math import gcd
except Exception:
    from fractions import gcd


def solve(N: int, K: int, A: "List[int]"):
    bits = [0] * int(ceil(log(max(K, max(A))) / log(2)))
    for a in A:
        i = 0
        while 0 < (a >> i):
            bits[i] += (a >> i) & 1
            i += 1

    x = 0
    for i in range(len(bits)-1, -1, -1):
        c = bits[i]
        diff = (1 << i) if 2 * c <= N else 0
        if x + diff <= K:
            x += diff

    ret = 0
    for a in A:
        ret += x ^ a
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    K = int(next(tokens))  # type: int
    A = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, K, A)
    print(result)

if __name__ == '__main__':
    main()
