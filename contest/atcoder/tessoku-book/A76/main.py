#!/usr/bin/env python3
import sys

MOD = 1000000007  # type: int


def solve(N: int, W: int, L: int, R: int, X: "List[int]"):
    return


# Generated by 2.13.0 https://github.com/kyuridenamida/atcoder-tools  (tips: You use the default template now. You can remove this line by using your custom template)
def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    W = int(next(tokens))  # type: int
    L = int(next(tokens))  # type: int
    R = int(next(tokens))  # type: int
    X = [int(next(tokens)) for _ in range(N)]  # type: "List[int]"
    solve(N, W, L, R, X)

if __name__ == '__main__':
    main()
