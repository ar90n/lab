import sys
import numpy as np

reatInt = lambda: int(sys.stdin.readline())
reatIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

n = reatInt()
l = []
acc_v = 0
for i in range(n):
    w, v = reatIntN()
    l.append((w,v))
    acc_v += v
W = reatInt()

dp = np.zeros((n+1, acc_v+1), dtype=np.uint64)
dp[:, :] = 1 << 63
dp[0,0] = 0

for i, (w, v) in enumerate(l,1):
    for j in range(acc_v):
        if v <= j:
            v2 = dp[i-1][j-v] + w
        else:
            v2 = 1 << 63
        dp[i, j] = min(dp[i-1,j], v2 )
ret = np.max(np.where(dp[-1, :] <= W))
print(ret)
