def bellman_ford(g, src):
    min_cost = [float('inf')] * len(g)
    min_cost[src] = 0

    for _ in range(len(min_cost)):
        is_changed = False
        for s, d, c in g.edges():
            if min_cost[s] + c < min_cost[d]:
                is_changed = True
                min_cost[d] = min_cost[s] + c
        if not is_changed:
            break
    return min_cost
