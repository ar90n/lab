#!/usr/bin/env python3
import sys

YES = "Yes"  # type: str
NO = "No"  # type: str


def solve(N: int, S: int, A: "List[int]"):
    dp = [False] * (S + 1)
    dp[0] = True

    for a in A:
        for j in [i for i, v in enumerate(dp) if v]:
            if j + a <= S:
                dp[j + a] = True
    if dp[S]:
        print("Yes")
    else:
        print("No")
    return


# Generated by 2.13.0 https://github.com/kyuridenamida/atcoder-tools  (tips: You use the default template now. You can remove this line by using your custom template)
def main():
    def iterate_tokens():
        for line in sys.stdin:
            for word in line.split():
                yield word
    tokens = iterate_tokens()
    N = int(next(tokens))  # type: int
    S = int(next(tokens))  # type: int
    A = [int(next(tokens)) for _ in range(N)]  # type: "List[int]"
    solve(N, S, A)

if __name__ == '__main__':
    main()
