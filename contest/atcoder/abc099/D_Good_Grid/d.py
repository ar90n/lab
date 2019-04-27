import sys

readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(' ')]

n, c = readIntN()
ds = [readIntN() for _ in range(c)]
cs = [readIntN() for _ in range(n)]

acc = [[0] * c for _ in range(3)]
for i in range(n):
    for j in range(n):
        a = (i + j) % 3
        acc[a][cs[i][j] - 1] += 1

ret = float('inf')
for i in range(c):
    for j in range(c):
        if i == j:
            continue
        for k in range(c):
            if i == k or j == k:
                continue
            cur = 0
            for l in range(c):
                cur += ds[l][i] * acc[0][l]
                cur += ds[l][j] * acc[1][l]
                cur += ds[l][k] * acc[2][l]
            ret = min(ret, cur)

print(ret)
