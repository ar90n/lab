#!/usr/bin/env python3
import sys
try:
    from math import gcd
except Exception:
    from fractions import gcd


def solve(A: int, B: int, K: int):
    d = gcd(A, B)
    return [i for i in range(1, d + 1) if (d % i) == 0][-K]


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    A = int(next(tokens))  # type: int
    B = int(next(tokens))  # type: int
    K = int(next(tokens))  # type: int
    result = solve(A, B, K)
    print(result)

if __name__ == '__main__':
    main()
