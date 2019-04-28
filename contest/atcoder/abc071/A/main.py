#!/usr/bin/env python3
import sys
try:
    from math import gcd
except Exception:
    from fractions import gcd


def solve(x: int, a: int, b: int):
    return 'A' if abs(a - x) < abs(b - x) else 'B'


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    x = int(next(tokens))  # type: int
    a = int(next(tokens))  # type: int
    b = int(next(tokens))  # type: int
    result = solve(x, a, b)
    print(result)

if __name__ == '__main__':
    main()
