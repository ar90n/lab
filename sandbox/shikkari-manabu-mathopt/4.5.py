# %%
import math
# %%
def obj(x1: int, x2: int, x3: int) -> float:
    x1_term = x1 ** 0.5 + x1 / 4.0
    x2_term = math.log2(x2 + 1.0)
    x3_term = x3 ** 2.0 / 4.0
    return  x1_term + x2_term + x3_term
# %%
max_resources = 4


# %%
dp = [[(float('inf'), (0, 0, 0))] * (max_resources + 1) for _ in range(3)]

for i in range(3):
    dp[i][0] = (obj(0, 0, 0), (0, 0, 0))

for i in range(max_resources + 1):
    dp[0][i] = (obj(i, 0, 0), (i, 0, 0))

for k in range(1, 3):
    for i in range(1, max_resources + 1):
        args = [a + int((j == k)) for j, a in enumerate(dp[k][i -1][1])]
        v = obj(*args)
        dp[k][i] = min((v, args), dp[k-1][i])

# %%
print(dp[-1][-1])

# %%
