import sys
import numpy as np

reatInt = lambda: int(sys.stdin.readline())
reatIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]


a, b = reatIntN()
ma = max(a, b)
mi = min(a, b)

ret = (2 * ma - 1) if 2 <= (ma - mi) else (ma + mi)
print(ret)
