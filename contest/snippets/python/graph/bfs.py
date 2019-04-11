def bfs(g, src, dst):
    from collections import deque

    visited = set([])
    vertex_queue = deque([src])
    while vertex_queue:
        current_vertex = vertex_queue.popleft()
        if current_vertex == dst:
            return True

        if current_vertex in visited:
            continue
        visited.add(current_vertex)

        for next_vertex in g.setdefault(current_vertex, {}).keys():
            vertex_queue.append(next_vertex)
    return False
