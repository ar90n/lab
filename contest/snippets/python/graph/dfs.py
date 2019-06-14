def dfs(g, src):
    visited = [False] * len(g)
    stack = [(src, 0)]
    while stack:
        n, c = stack.pop()
        if visited[n]:
            continue
        visited[n] = True

        for _, d, _ in g.edges(n):
            #yield d
            stack.append((d, c + 1))
        yield n, c
