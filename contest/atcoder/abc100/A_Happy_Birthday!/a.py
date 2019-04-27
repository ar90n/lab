import sys
import numpy as np

readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

a, b = readIntN()
ret = 'Yay!' if max(a, b) <= 8 else ':('
print(ret)
