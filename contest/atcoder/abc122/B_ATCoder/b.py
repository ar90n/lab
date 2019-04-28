import sys
import numpy as np

readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

s = input()

i = 0
j = 0
ret = 0
while i < len(s):
    while j < len(s) and s[j] in "ACGT":
        j += 1
    ret = max(ret, j - i)
    j += 1
    i = j
print(ret)
