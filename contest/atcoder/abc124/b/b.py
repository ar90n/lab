import sys
import numpy as np

reatInt = lambda: int(sys.stdin.readline())
reatIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]


n = reatInt()
hs = reatIntN()

maxv = 0
ret = 0
for h in hs:
    if maxv <= h:
        ret += 1
        maxv = h
print(ret)
