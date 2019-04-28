#!/usr/bin/env python3
import sys
try:
    from math import gcd
except Exception:
    from fractions import gcd

from collections import Counter


def solve(S: str):
    c = Counter(S)
    c.setdefault('1', 0)
    c.setdefault('0', 0)
    return 2 * min(c.values())


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    S = (next(tokens))  # type: str
    result = solve(S)
    print(result)

if __name__ == '__main__':
    main()
