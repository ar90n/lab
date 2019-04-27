import sys
import numpy as np

readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

d, n = readIntN()
if n == 100:
    n = 101
ret = n * (100 ** d)
print(ret)
