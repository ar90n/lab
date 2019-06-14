def diameter(g):
    _, n = max([(c, n) for n, c in bfs(g, 0)])
    return max([c for _, c in bfs(g, n)])
