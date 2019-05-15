def dijkstra(g, src):
    import heapq

    min_cost = [float('inf')] * len(g)

    node_heap = []
    heapq.heappush(node_heap, (0, src))
    while node_heap:
        cost, node = heapq.heappop(node_heap)
        if min_cost[node] <= cost:
            continue
        min_cost[node] = cost

        for _, d, c in g.edges(node):
            dc = min_cost[node] + c
            heapq.heappush(node_heap, (dc, d))
    return min_cost
