# %%
T = 5
dts = [0, 5, 7, 3, 6, 4]
cts = [0, 1, 1, 3, 3, 3]
fts = [0, 3, 3, 3, 3, 3]
gts = [0, 1, 1, 1, 1, 1]

# %%
dp = [[(float('inf'), [0] * (T + 1))] * (T + 1) for _ in range(T + 1)]
dp[0][0] = (0, [0] * (T + 1))
for t in range(1, T + 1):
    dp[t-1][t] = dp[t-1][t-1]
    for k in range(t, T + 1):
        dp[k][t] = min(dp[k][t - 1], (dp[k-1][t][0] +fts[t] * (t == k) + dts[k] * (cts[t] + sum(gts[t: k])), [*dp[k-1][t][1][:t], dp[k-1][t][1][t] + dts[k],*dp[k-1][t][1][t+1:]]))
# %%
z = dp[-1][-1][0]
xs = dp[-1][-1][1][1:]
ss = [0] * (T + 1)
for i in range(1, T + 1):
    ss[i] = ss[i-1] + xs[i-1] - dts[i]
ss = ss[1:]
# %%
print(f"{z=}, {xs=}, {ss=}")