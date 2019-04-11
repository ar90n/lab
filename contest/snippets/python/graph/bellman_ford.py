def find_bellman_ford(g, src):
    from functools import reduce
    import sys

    all_nodes = reduce(
        lambda x, y: x.union(y), [set(g.keys())] + [set(g[x].keys()) for x in g]
    )
    node_num = len(all_nodes)
    min_cost = dict(zip(all_nodes, [sys.maxsize] * node_num))

    min_cost[src] = 0
    for i in range(node_num):
        is_changed = False
        for node, neighbors in zip(g.keys(), g.values()):
            for neighbor, w in zip(neighbors.keys(), neighbors.values()):
                new_cost = min_cost[node] + w
                current_cost = min_cost[neighbor]
                if new_cost < current_cost:
                    min_cost[neighbor] = new_cost
                    is_changed = True
        if not is_changed:
            break
    return min_cost
