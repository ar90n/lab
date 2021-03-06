#!/usr/bin/env python3
import sys
from itertools import accumulate

try:
    from math import gcd
except Exception:
    from fractions import gcd

def solve(N: int, A: "List[int]"):
    gcds0 = list(accumulate(A, gcd))
    gcds1 = list(accumulate(A[::-1], gcd))[::-1]

    ret = 0
    for i in range(N):
        if i == 0:
            ret = max(ret, gcds1[i+1])
        if i == (N-1):
            ret = max(ret, gcds0[i-1])
        else:
            ret = max(ret, gcd(gcds0[i-1], gcds1[i+1]))
    print(ret)

# Generated by 1.1.4 https://github.com/kyuridenamida/atcoder-tools  (tips: You use the default template now. You can remove this line by using your custom template)
def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    A = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    solve(N, A)

if __name__ == '__main__':
    main()
