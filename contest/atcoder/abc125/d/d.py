import sys

readInt = lambda: int(sys.stdin.readline())
readIntN = lambda: [int(v) for v in sys.stdin.readline().split(" ")]

total = 0
ret = 1
n = readInt()
aa = []
for i in range(n):
    v = readInt()
    aa.append(v)
    total += v
    ret *= 3


L = (n * max(aa) + 1) 
memo = [-1] * ((n+1) * L * 2)
def dfs(i, c):
    key = (i * L * 2) + 2 * c
    if 0 <= memo[key]:
        return memo[key: key+2]

    if i == n:
        memo[key + 0] = 3 if total <= (2 * c) else 0
        memo[key + 1] = 3 if total == (2 * c) else 0
        return memo[key: key + 2]

    v0 = dfs(i + 1, c)
    v1 = dfs(i + 1, c + aa[i])
 
    memo[key + 0] = (2 * v0[0] + v1[0]) % 998244353
    memo[key + 1] = (v0[1] + v1[1]) % 998244353
    return memo[key: key + 2]

a, b = dfs(0, 0)
ret = (ret - a + b) % 998244353
print(ret)
