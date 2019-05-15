def dfs(g, src):
    visited = [False] * len(g)
    stack = [src]
    while stack:
        n = stack.pop()
        if visited[n]:
            continue
        visited[n] = True

        for _, d, _ in g.edges(n):
            #yield d
            stack.append(d)
        yield n
