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
readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

YES = "Yes"  # type: str
NO = "No"  # type: str


def main():
    H, W = readIntN()
    m = [list(input()) for _ in  range(H)]

    s = None
    g = None
    for y, r in enumerate(m):
        for x, v in enumerate(r):
            if m[y][x] == 's':
                s = (y, x)
            elif m[y][x] == 'g':
                g = (y, x)
    p = [s]
    found = False
    while 0 < len(p):
        y, x = p.pop()
        if m[y][x] == '#':
            continue
        elif m[y][x] == 'g':
            found = True
            break
        m[y][x] = '#'

        dxs = [-1, 0, 0, 1]
        dys = [0, 1, -1, 0]
        for dx, dy in zip(dxs, dys):
            nx = x + dx
            ny = y + dy
            if nx < 0 or W <= nx or ny < 0 or H <= ny:
                continue
            p.append((ny, nx))
        pass

    result = YES if found else NO
    print(result)

if __name__ == '__main__':
    main()
