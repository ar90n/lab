import sys
import numpy as np

reatInt = lambda: int(sys.stdin.readline())
reatIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

n = reatInt()
m = reatInt()
s = sys.stdin.readline().strip()
t = sys.stdin.readline().strip()

dp = np.zeros((len(s)+1, len(t)+1), dtype=np.uint64)
a, b = dp.shape
for i in range(a - 1):
    for j in range(b - 1):
        dp[i+1, j+1] = max(dp[i][j+1], dp[i+1][j], dp[i][j] + int(s[i] == t[j]))
print(dp[-1,-1])
