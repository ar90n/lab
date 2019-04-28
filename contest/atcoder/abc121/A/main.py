#!/usr/bin/env python3
import sys
try:
    from math import gcd
except Exception:
    from fractions import gcd


def solve(H: int, W: int, h: int, w: int):
    return (H - h ) * (W - w)


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    H = int(next(tokens))  # type: int
    W = int(next(tokens))  # type: int
    h = int(next(tokens))  # type: int
    w = int(next(tokens))  # type: int
    result = solve(H, W, h, w)
    print(result)

if __name__ == '__main__':
    main()
