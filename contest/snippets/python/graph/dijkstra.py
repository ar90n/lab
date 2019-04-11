def find_dijkstra(g, src):
    import heapq

    min_cost = {}

    node_heap = []
    heapq.heapify(node_heap)
    heapq.heappush(node_heap, (0, src))
    while node_heap:
        cost, node = heapq.heappop(node_heap)
        if node in min_cost:
            continue
        min_cost[node] = cost

        n = g.setdefault(node, {})
        for neighbor, w in zip(n.keys(), n.values()):
            neighbor_cost = min_cost[node] + w
            heapq.heappush(node_heap, (neighbor_cost, neighbor))
    return min_cost
