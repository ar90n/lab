#!/usr/bin/env python3
import sys
from math import *
from itertools import *
from collections import *
from functools import *
from bisect import *
try:
    from math import gcd
except Exception:
    from fractions import gcd


def solve(A: int, B: int, Q: int, s: "List[int]", t: "List[int]", x: "List[int]"):
    rs = []
    for xx in x:
        si = bisect(s, xx)
        ti = bisect(t, xx)

        ret = float('inf')
        for ss, tt in product([s[max(0, si - 1)], s[min(si, A - 1)]], [t[max(0, ti - 1)], t[min(ti, B - 1)]]):
            ret = min(ret, abs(ss - xx) + abs(tt - ss))
            ret = min(ret, abs(tt - xx) + abs(tt - ss))
        rs.append(ret)
    return '\n'.join([str(r) for r in rs])


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    A = int(next(tokens))  # type: int
    B = int(next(tokens))  # type: int
    Q = int(next(tokens))  # type: int
    s = [ int(next(tokens)) for _ in range(A) ]  # type: "List[int]"
    t = [ int(next(tokens)) for _ in range(B) ]  # type: "List[int]"
    x = [ int(next(tokens)) for _ in range(Q) ]  # type: "List[int]"
    result = solve(A, B, Q, s, t, x)
    print(result)

if __name__ == '__main__':
    main()
