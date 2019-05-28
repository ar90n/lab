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


def solve(S: str):
    a = S[0] == 'A'
    bs = list(filter(lambda x: x != 'C', S[2:-1]))
    cs = list(filter(lambda x: ord('A') <= ord(x) <= ord('Z'), [S[1]] + bs + list(S[-1:])))
    r =  a and len(bs) == (len(S[2:-1]) - 1) and len(cs) == 0
    return 'AC' if r else 'WA'


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    S = next(tokens)  # type: str
    result = solve(S)
    print(result)

if __name__ == '__main__':
    main()
