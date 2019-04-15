import sys
import numpy as np

reatInt = lambda: int(sys.stdin.readline())
reatIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

n = reatInt()
l = []
for i in range(n):
    w, v = reatIntN()
    l.append((w,v))
W = reatInt()

dp = np.zeros((n+1, W+1), dtype=np.uint64)
for i, (w, v) in enumerate(l, 1):
    for j in range(W+1):
        if w <= j:
            v2 = dp[i-1][j-w] + v
        else:
            v2 = dp[i][0]
        dp[i, j] = max(dp[i-1,j], v2 )
ret = max(dp[-1,:])
print(ret)
