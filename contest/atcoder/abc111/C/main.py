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


def solve(n: int, v: "List[int]"):
    e = v[0::2]
    o = v[1::2]

    ec = sorted([(vv, k) for k, vv in Counter(e).items()], reverse=True) + [(0, None)]
    oc = sorted([(vv, k) for k, vv in Counter(o).items()], reverse=True) + [(0, None)]

    ee1 = ec[0]
    oo1 = oc[1] if oc[0][1] == ee1[1] else oc[0]
    ret1 = len(v) - ee1[0] - oo1[0]

    oo2 = oc[0]
    ee2 = ec[1] if ec[0][1] == oo2[1] else ec[0]
    ret2 = len(v) - ee2[0] - oo2[0]
   
    return min(ret1, ret2)


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    n = int(next(tokens))  # type: int
    v = [ int(next(tokens)) for _ in range(n) ]  # type: "List[int]"
    result = solve(n, v)
    print(result)

if __name__ == '__main__':
    main()
