def find_prim(g, src):
    import heapq

    min_spaning_tree = {}

    edge_heap = []
    heapq.heapify(edge_heap)
    heapq.heappush(edge_heap, (0, (src, src)))
    while edge_heap:
        cost, edge = heapq.heappop(edge_heap)
        if edge[1] in min_spaning_tree:
            continue
        min_spaning_tree.setdefault(edge[0], {})[edge[1]] = cost
        min_spaning_tree.setdefault(edge[1], {})[edge[0]] = cost

        for neighbor, w in zip(g[edge[1]].keys(), g[edge[1]].values()):
            heapq.heappush(edge_heap, (w, (edge[1], neighbor)))

    min_spaning_tree[src].pop(src)
    return min_spaning_tree
