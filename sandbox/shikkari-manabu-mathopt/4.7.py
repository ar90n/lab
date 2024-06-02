# %%
from heapq import heappop, heappush
# %%
G = {
    1: {2: 5, 4: 2},
    2: {3: 4, 5: 2},
    3: {},
    4: {2: 1, 5: 4},
    5: {3: 1}
}
# %%
queue = [(0, None, 1)]
visited = {}
while 0 < len(queue):
    cost, prev, node = heappop(queue)
    if node in visited:
        continue
    visited[node] = (cost, prev)
    for neighbor, weight in G[node].items():
        heappush(queue, (cost + weight, node, neighbor))
# %%
print(visited)
