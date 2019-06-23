#!/usr/bin/env python3
import sys
from math import *
from itertools import *
from collections import *
from functools import *
from operator import *
try:
    from math import gcd
except Exception:
    from fractions import gcd



def bisect_func(search_range, f):
    lo, hi = search_range
    while (lo+1) < hi:
        mid= (lo + hi) // 2
        if 0 < f(mid):
            hi = mid
        else:
            lo = mid
    return lo


def solve(N: int, W: int, vs: "List[int]", ws: "List[int]"):
    def case1():
        ps = list(zip(ws, vs))
        p0 = ps[:len(ps)//2]
        p1 = ps[len(ps)//2:]
        def _agg(ps):
            dp = [[-1, -1] for _ in range(2 ** len(ps))]
            dp[0] = [0, 0]
            m = {1 << i: v for i, v in enumerate(ps)}
            for i in range(1, len(dp)):
                dp[i][0] = dp[(i-1)&i][0] + m[((i-1)&i) ^ i][0]
                dp[i][1] = dp[(i-1)&i][1] + m[((i-1)&i) ^ i][1]

            dp = sorted([(w, -v) for w, v in dp])

            ret = []
            mm = float('inf')
            for w, v in dp:
                if v < mm:
                    mm = v
                    ret.append((w,-v))
            return ret

        pp0 = _agg(p0)
        pp1 = _agg(p1)
        rere = (0, 0)
        for w, v in pp0:
            def _f(m):
                return pp1[m][0] + w - W
            bi = bisect_func((0, len(pp1)), _f)
            if (rere[1] < pp1[bi][1] + v) and (pp1[bi][0] + w <= W):
                rere = (pp1[bi][0] + w,pp1[bi][1] + v)
        return rere[1]


    def case2():
        mw = max(ws)

        H = min(mw * N, W) + 1
        dp = [[0] * H for _ in range(N+1)]
        for i in range(N):
            for j in range(H):
                dp[i+1][j] = dp[i][j]
                if 0 <= j - ws[i]:
                    dp[i+1][j] = max(dp[i+1][j], dp[i][j-ws[i]] + vs[i])
        return max(dp[-1])

    def case3():
        mv = max(vs)

        H = mv * N + 1
        dp = [float('inf')] * H
        dp[0] = 0
        for i in range(N):
            for j in range(H-1, vs[i]-1, -1):
                dp[j] = min(dp[j - vs[i]] + ws[i], dp[j])
        return max([v for v, w in enumerate(dp) if w <= W])

    if N <= 30:
        return case1()
    elif max(ws) <= 1000:
        return case2()
    return case3()


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    W = int(next(tokens))  # type: int
    v = [int()] * (N)  # type: "List[int]" 
    w = [int()] * (N)  # type: "List[int]" 
    for i in range(N):
        v[i] = int(next(tokens))
        w[i] = int(next(tokens))
    result = solve(N, W, v, w)
    print(result)

if __name__ == '__main__':
    main()
