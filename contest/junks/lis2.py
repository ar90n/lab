import sys
import numpy as np

reatInt = lambda: int(sys.stdin.readline())
reatIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

n = reatInt()
a = reatIntN()

dp = [0] * n

for i, aa in enumerate(a):
    dp[i] = 1
    for j in range(i):
        if a[j] < a[i]:
            dp[i] = max(dp[i], dp[j] + 1)
print(dp[-1])
