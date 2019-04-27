import sys
import numpy as np

readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

a, b = readIntN()
l = b - a
ret = (l * (l + 1) // 2) - b
print(ret)
