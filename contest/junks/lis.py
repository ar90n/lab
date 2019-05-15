import sys

import numpy as np

reatInt = lambda: int(sys.stdin.readline())
reatIntN = lambda: [int(v) for v in sys.stdin.readline().split(" ")]

n = reatInt()
a = reatIntN()

dp = np.zeros((n + 1, n + 1), dtype=np.uint64)
ass = sorted(a)
for i, aa in enumerate(a):
    jj = 0
    for j in range(n):
        if ass[j] < aa:
            dp[i + 1][j + 1] = max(dp[i][j + 1], dp[i + 1][j])
            jj = j + 1
        else:
            dp[i + 1][j + 1] = max(dp[i][j + 1], dp[i + 1][jj] + 1)
print(dp[-1,-1])
