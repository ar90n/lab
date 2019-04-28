#!/usr/bin/env python3
import sys
try:
    from math import gcd
except Exception:
    from fraction import gcd


def solve(K: int):
    return 0


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    K = int(next(tokens))  # type: int
    result = solve(K)
    print(result)

if __name__ == '__main__':
    main()
