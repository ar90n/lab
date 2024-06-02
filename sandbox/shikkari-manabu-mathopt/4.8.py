# %%
G = {
    1: {2: 5, 4: 3},
    2: {3: -3, 5: 2},
    3: {4: 2},
    4: {2: 1, 5: 2},
    5: {3: -5}
}
# %%
for i in range(1, 6):
    G[i][i] = 0
    for j in range(1, 6):
        for k in range(1,6):
            G[i][j] = min(G[i].get(j, float('inf')), G[i].get(k, float('inf')) + G[k].get(j, float('inf')))
# %%
print(G)
# %%
