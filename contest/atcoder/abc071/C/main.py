#!/usr/bin/env python3
import sys
try:
    from math import gcd
except Exception:
    from fractions import gcd
from collections import Counter

def solve(N: int, A: "List[int]"):
    tmp = [(i,c) for i, c in Counter(A).items() if 2 <= c]
    tmp = sorted(tmp, reverse=True)
    if 2 <= len(tmp):
        return (tmp[0][0] ** 2) if 4 <= tmp[0][1] else (tmp[0][0] * tmp[1][0])
    else:
        return 0


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    A = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, A)
    print(result)

if __name__ == '__main__':
    main()
