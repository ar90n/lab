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



def solve(m, s, g):
    q = deque([(s[0], s[1], 0)])
    while 0 < len(q):
        y, x, c = q.popleft()
        if m[y][x] != '.':
            continue
        m[y][x] = c
        if (y, x) == g:
            break

        dxs = [-1, 0, 1, 0]
        dys = [0, 1, 0, -1]
        for dy, dx in zip(dys, dxs):
            ny = y + dy
            nx = x + dx
            q.append((ny, nx, c + 1))

    return m[g[0]][g[1]]


def main():
    R, C = readIntN()
    sy, sx = readIntN()
    gy, gx = readIntN()
    m = [list(input()) for _ in range(R)]
    result = solve(m, (sy-1, sx-1), (gy -1, gx-1))
    print(result)

if __name__ == '__main__':
    main()
