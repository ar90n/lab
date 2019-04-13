import sys
import numpy as np

reatInt = lambda: int(sys.stdin.readline())
reatIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]


n, k = reatIntN()
ss = sys.stdin.readline().strip()

l = []
i = 0
if ss[0] == '0':
    l.append((1, 0))
else:
    l.append((0, 0))
while i < len(ss):
    c = ss[i]
    while (i < len(ss)) and (c == ss[i]):
        i += 1
    l.append((int(c), i))

i = 1
ret = 0
while i < len(l):
    q = 1 if  l[i][0] == 0 else 0
    b = i - 1
    e = min(i + 2 * k - q, len(l) - 1)
    ret = max(ret, l[e][1] - l[b][1])
    i += 1
print(ret)
