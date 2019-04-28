import sys
import numpy as np

readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

n, q = readIntN()
s = input()
lrs = [readIntN() for i in range(q)]

acc = [0] * n
for i, (b, e) in enumerate(zip(s, s[1:]), 1):
    acc[i] = acc[i-1]
    if b == 'A' and e == 'C':
        acc[i] += 1

for b, e in lrs:
    print(acc[e-1] - acc[b-1])
