# %%
from copy import deepcopy
# %%
G = {
    1: {2: 16, 4: 13},
    2: {3: 12, 5: 2},
    3: {4: 9, 6: 20},
    4: {2: 4, 5: 14},
    5: {3: 7, 6: 4},
    6: {}
}
RG = deepcopy(G)
for i in RG:
    for j in RG:
        RG[i].setdefault(j, 0)
        RG[j].setdefault(i, 0)
# %%
def find_path(G: dict[int, dict[int, int]], start: int, end: int) -> list[int]:
    queue = [(start, start)]
    visited = {}
    while 0 < len(queue):
        node, prev = queue.pop(0)
        if node in visited:
            continue
        visited[node] = prev
        for neighbor, r in G[node].items():
            if r <= 0:
                continue
            if neighbor not in visited:
                queue.append((neighbor, node))
    if end not in visited:
        return []

    path = []
    cur = end
    while cur != start:
        path.append(cur)
        cur = visited[cur]
    path.append(start)
    return path[::-1]

while 0 < len(p := find_path(RG, 1, 6)):
    min_flow = min(RG[i][j] for i, j in zip(p, p[1:]))
    for i, j in zip(p, p[1:]):
        RG[i][j] -= min_flow
        RG[j][i] += min_flow
max_flow = sum(G[1].values()) - sum(RG[1].values())
# %%
print(max_flow)