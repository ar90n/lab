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


def solve(N: int, S: "List[str]", P: "List[int]"):
    return [v for _, _, v in sorted([(s, -p, i + 1) for i , (s, p) in enumerate(zip(S, P))])]


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    S = [str()] * (N)  # type: "List[str]" 
    P = [int()] * (N)  # type: "List[int]" 
    for i in range(N):
        S[i] = next(tokens)
        P[i] = int(next(tokens))
    result = solve(N, S, P)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
