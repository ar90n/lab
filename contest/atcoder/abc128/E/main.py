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


def solve(N: int, Q: int, S: "List[int]", T: "List[int]", X: "List[int]", D: "List[int]"):
    return 0


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    Q = int(next(tokens))  # type: int
    S = [int()] * (N)  # type: "List[int]" 
    T = [int()] * (N)  # type: "List[int]" 
    X = [int()] * (N)  # type: "List[int]" 
    for i in range(N):
        S[i] = int(next(tokens))
        T[i] = int(next(tokens))
        X[i] = int(next(tokens))
    D = [ int(next(tokens)) for _ in range(Q) ]  # type: "List[int]"
    result = solve(N, Q, S, T, X, D)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
