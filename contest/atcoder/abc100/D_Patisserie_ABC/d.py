import sys
import numpy as np

readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

n, m = readIntN()
ps = [readIntN() for _ in range(n)]

ret = 0
for i in range(8):
    a = -1 if (i & 4) else 1
    b = -1 if (i & 2) else 1
    c = -1 if (i & 1) else 1
    ret = max(ret, sum(sorted([a * p0 + b * p1 + c * p2 for p0, p1, p2 in ps], reverse=True)[:m]))
print(ret)
