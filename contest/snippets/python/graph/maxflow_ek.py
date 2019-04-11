def find_maxflow_ek(rg, source, sink):
    def find_source_to_sink_path(rg, source, sink):
        from collections import deque
        import sys

        bfs_queue = deque([source])
        bfs_visited = set([])
        parents_map = {source: -1}
        source_to_sink_path = []

        flow = sys.float_info.max
        while bfs_queue:
            current_node = bfs_queue.popleft()
            bfs_visited.add(current_node)

            if current_node == sink:
                r_current_node = current_node
                r_parent_node = parents_map[r_current_node]
                while r_parent_node != -1:
                    capacity = rg[r_parent_node][r_current_node]
                    flow = min(flow, capacity)
                    source_to_sink_path.append(r_current_node)
                    r_current_node = r_parent_node
                    r_parent_node = parents_map[r_parent_node]
                source_to_sink_path.append(source)
                source_to_sink_path.reverse()
                return source_to_sink_path, flow, set([])

            for next_node in rg[current_node].keys():
                if 0 < rg[current_node][next_node] and next_node not in bfs_visited:
                    parents_map[next_node] = current_node
                    bfs_queue.append(next_node)

        return [], 0.0, set(bfs_visited)

    maxflow = 0.0
    while True:
        source_to_sink_path, current_flow, source_nodes = find_source_to_sink_path(
            rg, source, sink
        )
        print(source_to_sink_path, source_nodes)
        if current_flow == 0.0:
            break

        maxflow += current_flow
        for from_node, to_node in zip(
            source_to_sink_path[0:-1], source_to_sink_path[1:]
        ):
            rg[from_node][to_node] -= current_flow
            rg[to_node][from_node] += current_flow

    all_nodes = set()
    for k, v in rg.items():
        all_nodes.add(k)
        for kk in v.keys():
            all_nodes.add(kk)
    sink_nodes = all_nodes - source_nodes
    return source_nodes, sink_nodes, maxflow
