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

YES = "Yes"  # type: str
NO = "No"  # type: str

def solve(N: int, A: "List[int]", B: "List[int]"):
    #s = sorted([(b - a, i ) for i, (a, b) in enumerate(zip(A, B))])
    s = sorted([(b, i ) for i, (a, b) in enumerate(zip(A, B))])

    t = 0
    for _, i in s:
        a, b = A[i], B[i]
        t += a
        if b < t:
            return 'No'

    return 'Yes'


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    A = [int()] * (N)  # type: "List[int]" 
    B = [int()] * (N)  # type: "List[int]" 
    for i in range(N):
        A[i] = int(next(tokens))
        B[i] = int(next(tokens))
    result = solve(N, A, B)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
