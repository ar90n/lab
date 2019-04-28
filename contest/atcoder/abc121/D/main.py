#!/usr/bin/env python3
import sys
try:
    from math import gcd
except Exception:
    from fractions import gcd


def solve(A: int, B: int):
    B += 1
    A = ((A // 2) % 2) ^ ((A-1) * (A % 2))
    B = ((B // 2) % 2) ^ ((B-1) * (B % 2))
    return A ^ B


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    A = int(next(tokens))  # type: int
    B = int(next(tokens))  # type: int
    result = solve(A, B)
    print(result)

if __name__ == '__main__':
    main()
