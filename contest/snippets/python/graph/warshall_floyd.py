def warshall_floyd(g):
    n = len(g)
    costs = [[float('inf') for i in range(n)] for j in range(n)]
    for i in range(n):
        costs[i][i] = 0
    for s, d, c in g.edges():
        costs[s][d] = c

    for k in range(n):
        for i in range(n):
            for j in range(n):
                costs[i][j] = min(costs[i][j], costs[i][k] + costs[k][j])
    return costs
