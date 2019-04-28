#!/usr/bin/env python3
import sys
try:
    from math import gcd
except Exception:
    from fractions import gcd

YES = "Yes"  # type: str
NO = "No"  # type: str

def solve(N: int):
    return NO if bool(N % (sum([int(v) for v in str(N)]))) else YES


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    result = solve(N)
    print(result)

if __name__ == '__main__':
    main()
