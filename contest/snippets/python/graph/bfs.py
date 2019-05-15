def bfs(g, src):
    from collections import deque

    visited = [False] * len(g)
    queue = deque([src])
    while queue:
        n = queue.popleft()
        if visited[n]:
            continue
        visited[n] = True

        for _, d, _ in g.edges(n):
            queue.append(d)
        yield n
