import sys
import numpy as np

readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

n = readInt()
s = sys.stdin.readline().strip()

w = 0
b = 0
for c in s:
    w += int(c == '.')
    b += int(c == '#')


wc = 0
bc = 0
ret = min(w, b)
for c in s:
    wc += int(c == '.')
    bc += int(c == '#')

    ret = min(ret, bc + (w - wc))
print(ret)
