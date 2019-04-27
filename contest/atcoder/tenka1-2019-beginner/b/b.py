import sys
import numpy as np

readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

n = readInt()
s = sys.stdin.readline().strip()
k = readInt()

v = s[k - 1]
ss = list(s)
for i,c  in enumerate(s):
    if c != v:
        ss[i] = '*'
ret = ''.join(ss)
print(ret)
