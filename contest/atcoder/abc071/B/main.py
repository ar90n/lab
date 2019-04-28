#!/usr/bin/env python3
import sys
try:
    from math import gcd
except Exception:
    from fractions import gcd


def solve(S: str):
    res = sorted(list(set([chr(c) for c in range(ord('a'),ord('z')+1)]) - set(S)))
    return 'None' if len(res) == 0 else res[0]

def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    S = next(tokens)  # type: str
    result = solve(S)
    print(result)

if __name__ == '__main__':
    main()
