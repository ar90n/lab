#!/usr/bin/env python3
import sys

MOD = 10000  # type: int


def solve(N: int, T: "List[str]", A: "List[int]"):
    return


# Generated by 2.13.0 https://github.com/kyuridenamida/atcoder-tools  (tips: You use the default template now. You can remove this line by using your custom template)
def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    T = [str()] * (N)  # type: "List[str]"
    A = [int()] * (N)  # type: "List[int]"
    for i in range(N):
        T[i] = next(tokens)
        A[i] = int(next(tokens))
    solve(N, T, A)

if __name__ == '__main__':
    main()
