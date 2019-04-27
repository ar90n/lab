import sys
import numpy as np

readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

a, b, c = readIntN()
minv = min(a, b)
maxv = max(a, b)
ret = 'Yes' if minv < c < maxv else 'No'
print(ret)
