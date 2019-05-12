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


def solve(N: int, s: "List[str]"):
    hb = 0
    ta = 0
    hbta = 0
    ab = 0
    for ss in s:
        hb += 1 if (ss[0] == 'B' and ss[-1] != 'A') else 0
        ta += 1 if (ss[0] != 'B' and ss[-1] == 'A') else 0
        hbta += 1 if (ss[0] == 'B' and ss[-1] == 'A') else 0
        ab += ss.count('AB')

    if hb == 0 and 0 < hbta:
        hb += 1
        hbta -= 1
    if ta == 0 and 0 < hbta:
        ta += 1
        hbta -= 1

    if (0 < hb) and (0 < ta):
        ab += hbta + 1
        hb -= 1
        ta -= 1
    ab += min(hb, ta)
    return ab


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    s = [ next(tokens) for _ in range(N) ]  # type: "List[str]"
    result = solve(N, s)
    if isinstance(result, Iterable):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
