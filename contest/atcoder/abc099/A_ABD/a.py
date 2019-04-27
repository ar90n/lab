import sys
import numpy as np

readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

n = readInt()
ret = 'AB' + ('C' if n < 1000 else 'D')
print(ret)
