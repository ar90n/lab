import sys
import numpy as np

reatInt = lambda: int(sys.stdin.readline())
reatIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]


ss = sys.stdin.readline()
e = 0
o = 0
for i, s in enumerate(ss):
    if i % 2 == 0:
        e += int(s == '0')
        o += int(s == '1')
    else:
        e += int(s == '1')
        o += int(s == '0')
ret = min(e, o)
print(ret)
