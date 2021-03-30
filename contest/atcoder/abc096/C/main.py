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

def solve(H: int, W: int, s: "List[str]"):
    m = [[0] * W for _ in range(H)]
    for j in range(H):
        for i in range(W):
            if s[j][i] == '#':
                if j < (H - 1) and s[j+1][i] == '#':
                    m[j][i] += 1
                    m[j+1][i] += 1
                if i < (W - 1) and s[j][i+1] == '#':
                    m[j][i] += 1
                    m[j][i+1] += 1
                if m[j][i] == 0:
                    return NO
    return YES


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    H = int(next(tokens))  # type: int
    W = int(next(tokens))  # type: int
    s = [ next(tokens) for _ in range(H)]  # type: "List[str]"
    result = solve(H, W, s)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
