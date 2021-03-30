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


def solve(S: str):
    ab = int(S[:2])
    db = int(S[2:])

    yymm = False
    if (0 <= ab <= 99) and (1 <= db <= 12):
        yymm = True
    mmyy = False
    if (0 <= db <= 99) and (1 <= ab <= 12):
        mmyy = True

    if yymm and mmyy:
        return 'AMBIGUOUS'
    if not (yymm or mmyy):
        return 'NA'

    if yymm:
        return 'YYMM'

    return 'MMYY'


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    S = (next(tokens))  # type: int
    result = solve(S)
    if isinstance(result, Iterable) and not isinstance(result, str):
        result = '\n'.join([str(v) for v in result])
    print(result)

if __name__ == '__main__':
    main()
