#!/usr/bin/env python3
import sys
try:
    from math import gcd
except Exception:
    from fractions import gcd


def solve(N: int, K: int, A: "List[int]"):
    minv = min(A)
    minvs = [i  for i, a in enumerate(A) if a == minv]
   
    c = 0
    ret = 0
    for p in minvs:
        if p <= c:
            c += 1
            continue

        n = (p - c + (K - 2)) // (K - 1)
        ret += n
        c += n * (K - 1)
        c = max(c, p) + 1
    c -= 1
    n = (N-1 - c + (K - 2)) // (K - 1)
    ret += n
    return ret


def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    K = int(next(tokens))  # type: int
    A = [ int(next(tokens)) for _ in range(N) ]  # type: "List[int]"
    result = solve(N, K, A)
    print(result)

if __name__ == '__main__':
    main()
