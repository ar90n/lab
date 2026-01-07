# %%
from __future__ import annotations
import random
from dataclasses import dataclass, field
from typing import Optional


@dataclass
class DelaunayMesh:
    coords: list[tuple[float, float]] = field(default_factory=list)
    edges: list[tuple[int, int]] = field(default_factory=list)
    triangles: list[tuple[int, int, int]] = field(default_factory=list)
    children: list[list[int]] = field(default_factory=list)
    neighbors: list[list[Optional[int]]] = field(default_factory=list)
    super_nodes: tuple[int, int, int] = (0, 0, 0)


def is_leaf(mesh: DelaunayMesh, tri_idx: int) -> bool:
    return len(mesh.children[tri_idx]) == 0


def add_node(mesh: DelaunayMesh, x: float, y: float) -> int:
    idx = len(mesh.coords)
    mesh.coords.append((x, y))
    return idx


def add_edge(mesh: DelaunayMesh, n0: int, n1: int) -> int:
    """Add a new edge."""
    idx = len(mesh.edges)
    mesh.edges.append((n0, n1))
    return idx


def get_edge_nodes(mesh: DelaunayMesh, edge_idx: int) -> tuple[int, int]:
    return mesh.edges[edge_idx]


def get_triangle_nodes(mesh: DelaunayMesh, tri_idx: int) -> tuple[int, int, int]:
    """Get the 3 vertices of a triangle (always counter-clockwise order)."""
    e0, e1, e2 = mesh.triangles[tri_idx]
    n0a, n0b = mesh.edges[e0]
    n1a, n1b = mesh.edges[e1]

    # Find shared vertex between e0 and e1
    if n0a == n1a or n0a == n1b:
        shared = n0a
        opposite_e0 = n0b
    else:
        shared = n0b
        opposite_e0 = n0a

    # The other vertex of e1
    opposite_e1 = n1a if n1b == shared else n1b

    n0, n1, n2 = opposite_e0, shared, opposite_e1

    # Swap if clockwise
    if signed_area_coords(mesh.coords[n0], mesh.coords[n1], mesh.coords[n2]) < 0:
        n1, n2 = n2, n1

    return (n0, n1, n2)


def signed_area_coords(p0: tuple, p1: tuple, p2: tuple) -> float:
    """Compute signed area from coordinates."""
    return (p1[0] - p0[0]) * (p2[1] - p0[1]) - (p2[0] - p0[0]) * (p1[1] - p0[1])


def get_triangle_edge_position(mesh: DelaunayMesh, tri_idx: int, edge_idx: int) -> int:
    """Get the position (0, 1, or 2) of an edge within a triangle."""
    return mesh.triangles[tri_idx].index(edge_idx)


def get_opposite_node(mesh: DelaunayMesh, tri_idx: int, edge_idx: int) -> int:
    """Get the vertex opposite to the given edge."""
    edge_nodes = set(get_edge_nodes(mesh, edge_idx))
    tri_nodes = set(get_triangle_nodes(mesh, tri_idx))
    return next(iter(tri_nodes - edge_nodes))


def get_opposite_edge(mesh: DelaunayMesh, tri_idx: int, node_idx: int) -> int:
    """Get the edge opposite to the given vertex."""
    for edge_idx in mesh.triangles[tri_idx]:
        n0, n1 = get_edge_nodes(mesh, edge_idx)
        if node_idx != n0 and node_idx != n1:
            return edge_idx
    raise ValueError("Node not in triangle")


def find_shared_edge(mesh: DelaunayMesh, tri1: int, tri2: int) -> Optional[int]:
    """Find the shared edge between two triangles."""
    shared = set(mesh.triangles[tri1]) & set(mesh.triangles[tri2])
    return next(iter(shared), None)


def set_neighbor(
    mesh: DelaunayMesh, tri_idx: int, edge_idx: int, neighbor_idx: Optional[int]
):
    """Set the neighbor of tri_idx across edge_idx."""
    pos = get_triangle_edge_position(mesh, tri_idx, edge_idx)
    mesh.neighbors[tri_idx][pos] = neighbor_idx


def get_neighbor(mesh: DelaunayMesh, tri_idx: int, edge_idx: int) -> Optional[int]:
    """Get the neighbor of tri_idx across edge_idx."""
    pos = get_triangle_edge_position(mesh, tri_idx, edge_idx)
    return mesh.neighbors[tri_idx][pos]


def add_triangle(mesh: DelaunayMesh, e0: int, e1: int, e2: int) -> int:
    """Add a triangle specified by edge indices."""
    idx = len(mesh.triangles)
    mesh.triangles.append((e0, e1, e2))
    mesh.children.append([])
    mesh.neighbors.append([None, None, None])
    return idx


def signed_area(mesh: DelaunayMesh, n0: int, n1: int, n2: int) -> float:
    x0, y0 = mesh.coords[n0]
    x1, y1 = mesh.coords[n1]
    x2, y2 = mesh.coords[n2]
    return (x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)


def node_in_triangle(mesh: DelaunayMesh, node_idx: int, tri_idx: int) -> bool:
    """Check if a node is inside a triangle (including boundary)."""
    px, py = mesh.coords[node_idx]
    n0, n1, n2 = get_triangle_nodes(mesh, tri_idx)
    x0, y0 = mesh.coords[n0]
    x1, y1 = mesh.coords[n1]
    x2, y2 = mesh.coords[n2]

    def sign(px, py, ax, ay, bx, by):
        return (px - bx) * (ay - by) - (ax - bx) * (py - by)

    d1 = sign(px, py, x0, y0, x1, y1)
    d2 = sign(px, py, x1, y1, x2, y2)
    d3 = sign(px, py, x2, y2, x0, y0)

    has_neg = (d1 < 0) or (d2 < 0) or (d3 < 0)
    has_pos = (d1 > 0) or (d2 > 0) or (d3 > 0)

    return not (has_neg and has_pos)


def in_circumcircle(mesh: DelaunayMesh, tri_idx: int, node_idx: int) -> bool:
    """Check if a point is inside the circumcircle of a triangle."""
    n0, n1, n2 = get_triangle_nodes(mesh, tri_idx)
    x0, y0 = mesh.coords[n0]
    x1, y1 = mesh.coords[n1]
    x2, y2 = mesh.coords[n2]
    px, py = mesh.coords[node_idx]

    ax, ay = x0 - px, y0 - py
    bx, by = x1 - px, y1 - py
    cx, cy = x2 - px, y2 - py

    det = (
        (ax * ax + ay * ay) * (bx * cy - cx * by)
        - (bx * bx + by * by) * (ax * cy - cx * ay)
        + (cx * cx + cy * cy) * (ax * by - bx * ay)
    )
    return det > 1e-10


def create_delaunay(bounds: tuple[float, float, float, float]) -> DelaunayMesh:
    min_x, min_y, max_x, max_y = bounds
    margin = max(max_x - min_x, max_y - min_y) * 2

    mesh = DelaunayMesh()

    s0 = add_node(mesh, min_x - margin, min_y - margin)
    s1 = add_node(mesh, max_x + margin * 2, min_y - margin)
    s2 = add_node(mesh, (min_x + max_x) / 2, max_y + margin * 2)

    mesh.super_nodes = (s0, s1, s2)

    # Create super triangle edges and triangle
    e0 = add_edge(mesh, s0, s1)
    e1 = add_edge(mesh, s1, s2)
    e2 = add_edge(mesh, s2, s0)
    add_triangle(mesh, e0, e1, e2)

    return mesh


def locate(mesh: DelaunayMesh, node_idx: int) -> Optional[int]:
    """Find the leaf triangle containing the node (tree search)."""

    def rec(tri_idx: int) -> Optional[int]:
        if not node_in_triangle(mesh, node_idx, tri_idx):
            return None

        if is_leaf(mesh, tri_idx):
            return tri_idx

        for child_idx in mesh.children[tri_idx]:
            result = rec(child_idx)
            if result is not None:
                return result
        return None

    return rec(0)


def split_triangle(mesh: DelaunayMesh, tri_idx: int, node_idx: int) -> list[int]:
    """Split a triangle into 3 by inserting a node."""
    n0, n1, n2 = get_triangle_nodes(mesh, tri_idx)
    p = node_idx

    # Existing edges (edges of the original triangle)
    e_n0_n1 = get_opposite_edge(mesh, tri_idx, n2)
    e_n1_n2 = get_opposite_edge(mesh, tri_idx, n0)
    e_n2_n0 = get_opposite_edge(mesh, tri_idx, n1)

    # New edges (from vertices to center point)
    e_n0_p = add_edge(mesh, n0, p)
    e_n1_p = add_edge(mesh, n1, p)
    e_n2_p = add_edge(mesh, n2, p)

    # Create 3 new triangles
    t0 = add_triangle(mesh, e_n0_n1, e_n1_p, e_n0_p)
    t1 = add_triangle(mesh, e_n1_n2, e_n2_p, e_n1_p)
    t2 = add_triangle(mesh, e_n2_n0, e_n0_p, e_n2_p)

    # Set adjacency between new triangles (sharing internal edges)
    set_neighbor(mesh, t0, e_n1_p, t1)
    set_neighbor(mesh, t1, e_n1_p, t0)

    set_neighbor(mesh, t1, e_n2_p, t2)
    set_neighbor(mesh, t2, e_n2_p, t1)

    set_neighbor(mesh, t2, e_n0_p, t0)
    set_neighbor(mesh, t0, e_n0_p, t2)

    # Inherit adjacency from the original triangle
    outer_edges = [
        (t0, e_n0_n1),
        (t1, e_n1_n2),
        (t2, e_n2_n0),
    ]

    for new_tri, edge in outer_edges:
        old_neighbor = get_neighbor(mesh, tri_idx, edge)
        if old_neighbor is not None:
            set_neighbor(mesh, new_tri, edge, old_neighbor)
            set_neighbor(mesh, old_neighbor, edge, new_tri)

    # Update tree structure
    mesh.children[tri_idx] = [t0, t1, t2]

    return [t0, t1, t2]


def flip_edge(
    mesh: DelaunayMesh, t1: int, t2: int
) -> tuple[int, int, list[tuple[int, int]]]:
    shared_edge = find_shared_edge(mesh, t1, t2)
    if shared_edge is None:
        raise ValueError("No shared edge between triangles")

    pr = get_opposite_node(mesh, t1, shared_edge)
    pk = get_opposite_node(mesh, t2, shared_edge)
    ep1, ep2 = get_edge_nodes(mesh, shared_edge)

    # Existing edges (outer edges of original triangles)
    e_pr_ep1 = get_opposite_edge(mesh, t1, ep2)
    e_pr_ep2 = get_opposite_edge(mesh, t1, ep1)
    e_pk_ep1 = get_opposite_edge(mesh, t2, ep2)
    e_pk_ep2 = get_opposite_edge(mesh, t2, ep1)

    # New edge
    e_pr_pk = add_edge(mesh, pr, pk)

    # Create new triangles
    new_t1 = add_triangle(mesh, e_pr_ep1, e_pk_ep1, e_pr_pk)
    new_t2 = add_triangle(mesh, e_pr_pk, e_pk_ep2, e_pr_ep2)

    # Set adjacency between new triangles
    set_neighbor(mesh, new_t1, e_pr_pk, new_t2)
    set_neighbor(mesh, new_t2, e_pr_pk, new_t1)

    # Inherit outer adjacency
    # e_pr_ep1, e_pr_ep2 are from t1; e_pk_ep1, e_pk_ep2 are from t2
    outer_edges = [
        (new_t1, e_pr_ep1, t1),
        (new_t1, e_pk_ep1, t2),
        (new_t2, e_pr_ep2, t1),
        (new_t2, e_pk_ep2, t2),
    ]

    for new_tri, edge, old_tri in outer_edges:
        neighbor = get_neighbor(mesh, old_tri, edge)
        if neighbor is not None:
            set_neighbor(mesh, new_tri, edge, neighbor)
            set_neighbor(mesh, neighbor, edge, new_tri)

    # Update tree structure
    mesh.children[t1].extend([new_t1, new_t2])
    mesh.children[t2].extend([new_t1, new_t2])

    return new_t1, new_t2, [(nt, e) for nt, e, _ in outer_edges]


def insert_node(mesh: DelaunayMesh, x: float, y: float):
    """Insert a node into the triangulation."""
    node_idx = add_node(mesh, x, y)

    container = locate(mesh, node_idx)
    if container is None:
        raise ValueError(f"Point ({x}, {y}) outside super triangle")

    new_tris = split_triangle(mesh, container, node_idx)

    # Stack for flip operations: (tri_idx, edge_idx)
    stack = []
    for new_tri in new_tris:
        outer_edge = get_opposite_edge(mesh, new_tri, node_idx)
        neighbor = get_neighbor(mesh, new_tri, outer_edge)
        if neighbor is not None and is_leaf(mesh, neighbor):
            stack.append((new_tri, outer_edge))

    while stack:
        tri, edge = stack.pop()
        neighbor = get_neighbor(mesh, tri, edge)

        if neighbor is None:
            continue
        if not is_leaf(mesh, tri) or not is_leaf(mesh, neighbor):
            continue

        pk = get_opposite_node(mesh, neighbor, edge)

        if not in_circumcircle(mesh, tri, pk):
            continue

        _, _, outer_edges = flip_edge(mesh, tri, neighbor)

        # Add outer edges to stack
        for nt, e in outer_edges:
            adj = get_neighbor(mesh, nt, e)
            if adj is not None and is_leaf(mesh, adj):
                stack.append((nt, e))


def get_result_triangles(mesh: DelaunayMesh) -> list[tuple[int, int, int]]:
    """Get valid triangles (excluding those with super triangle vertices)."""
    result = []
    super_set = set(mesh.super_nodes)

    for tri_idx in range(len(mesh.triangles)):
        if not is_leaf(mesh, tri_idx):
            continue
        nodes = get_triangle_nodes(mesh, tri_idx)
        if set(nodes) & super_set:
            continue
        result.append(nodes)

    return result


def visualize(mesh: DelaunayMesh, input_nodes: list[int]):
    import matplotlib.pyplot as plt
    from matplotlib.patches import Polygon
    from matplotlib.collections import PatchCollection

    fig, ax = plt.subplots(figsize=(10, 10))

    triangles = get_result_triangles(mesh)

    patches = []
    for tri in triangles:
        pts = [mesh.coords[n] for n in tri]
        patches.append(Polygon(pts, closed=True))

    pc = PatchCollection(
        patches, facecolor="lightblue", edgecolor="blue", linewidth=1, alpha=0.5
    )
    ax.add_collection(pc)

    xs = [mesh.coords[n][0] for n in input_nodes]
    ys = [mesh.coords[n][1] for n in input_nodes]
    ax.scatter(xs, ys, c="red", s=50, zorder=5)

    for i, n in enumerate(input_nodes):
        x, y = mesh.coords[n]
        ax.annotate(
            str(i), (x, y), textcoords="offset points", xytext=(5, 5), fontsize=8
        )

    ax.set_aspect("equal")
    ax.autoscale()
    plt.title(
        f"Delaunay Triangulation ({len(input_nodes)} points, {len(triangles)} triangles)"
    )
    plt.show()
    plt.close()


random.seed(42)
mesh = create_delaunay(bounds=(0, 0, 100, 100))

n_points = 20
input_nodes = []

for i in range(n_points):
    x = random.uniform(0, 100)
    y = random.uniform(0, 100)
    insert_node(mesh, x, y)
    input_nodes.append(len(mesh.coords) - 1)
    print(f"Inserted {i + 1}/{n_points}")

triangles = get_result_triangles(mesh)
print(f"\nTotal nodes: {len(mesh.coords)}")
print(f"Total edges: {len(mesh.edges)}")
print(f"Total triangles (all): {len(mesh.triangles)}")
print(f"Valid triangles: {len(triangles)}")

# Verify all input nodes are in triangulation
nodes_in_tris = set()
for tri in triangles:
    nodes_in_tris.update(tri)

missing = set(input_nodes) - nodes_in_tris
if missing:
    print(f"ERROR: {len(missing)} nodes missing!")
else:
    print("OK: All input nodes are in triangulation")

visualize(mesh, input_nodes)
