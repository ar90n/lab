import sys
from itertools import product

_, _, _, k = [int(v) for v in sys.stdin.readline().split(" ")]
xs = [int(v) for v in sys.stdin.readline().split(" ")]
ys = [int(v) for v in sys.stdin.readline().split(" ")]
zs = [int(v) for v in sys.stdin.readline().split(" ")]


xys = sorted([x + y for x, y in product(xs, ys)], reverse=True)
zs = sorted(zs, reverse=True)
ret = [a + b for a, b in zip([zs[0]] * k, xys)]
for z in zs[1:]:
    rr = ret[:]
    r2 = [a + b for a, b in zip([z] * k, xys)]
    ret = sorted(ret + r2, reverse=True)[:k]
    if ret == rr:
        break
print('\n'.join([str(v) for v in ret]))
