import sys
import numpy as np

readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

n = readInt()
ret = n
for s in range(n+1):
    c = 0
    v = s
    while 0 < v:
        c += v % 6
        v //= 6
    v = n - s
    while 0 < v:
        c += v % 9
        v //= 9
    ret = min(ret, c)
print(ret)
