import sys
import numpy as np

readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

n = readInt()
a = readIntN()

ret = 0
for v in a:
    while (v % 2) == 0:
        ret += 1
        v /= 2
print(ret)
