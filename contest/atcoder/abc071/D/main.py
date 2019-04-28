#!/usr/bin/env python3
import sys
try:
    from math import gcd
except Exception:
    from fractions import gcd

MOD = 1000000007  # type: int

def solve(N: int, S: "List[str]"):
    conv = []
    i = 0
    while i < N:
        c, o = (0, 1) if S[0][i] == S[1][i] else (1, 2)
        conv.append(c)
        i += o

    ret = 3 if conv[0] == 0 else 6
    for b, c in zip(conv, conv[1:]):
        if b == 0 and c == 0:
            ret *= 2
        elif b == 0 and c == 1:
            ret *= 2
        elif b == 1 and c == 0 :
            ret *= 1
        elif b == 1 and c == 1:
            ret *= 3
    return ret % MOD

def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    S = [ next(tokens) for _ in range(2) ]  # type: "List[str]"
    result = solve(N, S)
    print(result)

if __name__ == '__main__':
    main()
