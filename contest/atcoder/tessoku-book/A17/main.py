#!/usr/bin/env python3
import sys


def solve(N: int, A: "List[int]", B: "List[int]"):
    dp = [(100 * N, -1)] * N
    dp[0] = (0, 0)
    dp[1] = (A[0], 0)

    for i in range(2, N):
        c1 = dp[i - 1][0] + A[i - 1]
        c2 = dp[i - 2][0] + B[i - 2]
        if c1 <= c2:
            dp[i] = (c1, i - 1)
        else:
            dp[i] = (c2, i - 2)

    ret = [N-1]
    while 0 < ret[-1]:
        ret.append(dp[ret[-1]][1])

    print(len(ret))
    print(" ".join([str(v + 1) for v in reversed(ret)]))
    return


# Generated by 2.13.0 https://github.com/kyuridenamida/atcoder-tools  (tips: You use the default template now. You can remove this line by using your custom template)
def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    A = [int(next(tokens)) for _ in range(N - 2 + 1)]  # type: "List[int]"
    B = [int(next(tokens)) for _ in range(N - 3 + 1)]  # type: "List[int]"
    solve(N, A, B)

if __name__ == '__main__':
    main()
