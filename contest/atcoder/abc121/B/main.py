#!/usr/bin/env python3
import sys
try:
    from math import gcd
except Exception:
    from fractions import gcd


def solve(N: int, M: int, C: int, B: "List[int]", A: "List[List[int]]"):
    ret = 0
    for aa in A:
        if 0 < sum([a * b for (a, b) in zip(aa, B)]) + C:
            ret += 1
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    M = int(next(tokens))  # type: int
    C = int(next(tokens))  # type: int
    B = [ int(next(tokens)) for _ in range(M) ]  # type: "List[int]"
    A = [ [ int(next(tokens)) for _ in range(M) ] for _ in range(N) ]  # type: "List[List[int]]"
    result = solve(N, M, C, B, A)
    print(result)

if __name__ == '__main__':
    main()
