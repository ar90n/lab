def prim(g, src):
    import heapq

    visited = [False] * len(g)
    spanning_tree_edges = []
    edge_heap = []
    heapq.heappush(edge_heap, (0, (src, src)))
    while edge_heap:
        c, (s, d) = heapq.heappop(edge_heap)
        if visited[d]:
            continue
        visited[d] = True
        spanning_tree_edges.append((s, d, c))

        for _, nd, ndc in g.edges(d):
            heapq.heappush(edge_heap, (ndc, (d, nd)))

    spanning_tree_edges = spanning_tree_edges[1:]
    return Graph(len(g), spanning_tree_edges)
