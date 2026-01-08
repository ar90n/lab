# %%
"""
Greedy Insertion for Terrain Mesh Generation

Implements Garland & Heckbert (1995) algorithm with incremental Delaunay triangulation.
"""

from __future__ import annotations
from dataclasses import dataclass, field
import numpy as np
import heapq


# ============================================
# Delaunay Mesh Data Structure
# ============================================

@dataclass
class Mesh:
    """
    Edge-indexed Delaunay triangulation.
    
    - coords[node_idx] = (x, y)
    - edges[edge_idx] = (node_a, node_b)
    - triangles[tri_idx] = (edge_a, edge_b, edge_c)
    - children[tri_idx] = [child_tri_idx, ...]  # DAG for point location
    - neighbors[tri_idx] = [neighbor_0, neighbor_1, neighbor_2]  # adjacency
    """
    coords: list[tuple[float, float]] = field(default_factory=list)
    edges: list[tuple[int, int]] = field(default_factory=list)
    triangles: list[tuple[int, int, int]] = field(default_factory=list)
    children: list[list[int]] = field(default_factory=list)
    neighbors: list[list[int | None]] = field(default_factory=list)


# ============================================
# Basic Operations
# ============================================

def add_node(mesh: Mesh, x: float, y: float) -> int:
    idx = len(mesh.coords)
    mesh.coords.append((x, y))
    return idx


def add_edge(mesh: Mesh, n0: int, n1: int) -> int:
    idx = len(mesh.edges)
    mesh.edges.append((n0, n1))
    return idx


def add_triangle(mesh: Mesh, e0: int, e1: int, e2: int) -> int:
    idx = len(mesh.triangles)
    mesh.triangles.append((e0, e1, e2))
    mesh.children.append([])
    mesh.neighbors.append([None, None, None])
    return idx


def is_leaf(mesh: Mesh, tri_idx: int) -> bool:
    return len(mesh.children[tri_idx]) == 0


def get_triangle_nodes(mesh: Mesh, tri_idx: int) -> tuple[int, int, int]:
    """Get 3 vertices of a triangle in CCW order."""
    e0, e1, e2 = mesh.triangles[tri_idx]
    n0a, n0b = mesh.edges[e0]
    n1a, n1b = mesh.edges[e1]
    
    # Find shared vertex between e0 and e1
    if n0a == n1a or n0a == n1b:
        shared, opposite_e0 = n0a, n0b
    else:
        shared, opposite_e0 = n0b, n0a
    
    opposite_e1 = n1a if n1b == shared else n1b
    n0, n1, n2 = opposite_e0, shared, opposite_e1
    
    # Ensure CCW order
    x0, y0 = mesh.coords[n0]
    x1, y1 = mesh.coords[n1]
    x2, y2 = mesh.coords[n2]
    if (x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0) < 0:
        n1, n2 = n2, n1
    
    return n0, n1, n2


def get_edge_position(mesh: Mesh, tri_idx: int, edge_idx: int) -> int:
    return mesh.triangles[tri_idx].index(edge_idx)


def get_opposite_node(mesh: Mesh, tri_idx: int, edge_idx: int) -> int:
    """Get vertex opposite to edge."""
    edge_nodes = set(mesh.edges[edge_idx])
    tri_nodes = set(get_triangle_nodes(mesh, tri_idx))
    return next(iter(tri_nodes - edge_nodes))


def get_opposite_edge(mesh: Mesh, tri_idx: int, node_idx: int) -> int:
    """Get edge opposite to vertex."""
    for edge_idx in mesh.triangles[tri_idx]:
        n0, n1 = mesh.edges[edge_idx]
        if node_idx != n0 and node_idx != n1:
            return edge_idx
    raise ValueError("Node not in triangle")


def find_shared_edge(mesh: Mesh, tri1: int, tri2: int) -> int | None:
    shared = set(mesh.triangles[tri1]) & set(mesh.triangles[tri2])
    return next(iter(shared), None)


def set_neighbor(mesh: Mesh, tri_idx: int, edge_idx: int, neighbor: int | None):
    pos = get_edge_position(mesh, tri_idx, edge_idx)
    mesh.neighbors[tri_idx][pos] = neighbor


def get_neighbor(mesh: Mesh, tri_idx: int, edge_idx: int) -> int | None:
    pos = get_edge_position(mesh, tri_idx, edge_idx)
    return mesh.neighbors[tri_idx][pos]


# ============================================
# Geometric Tests
# ============================================

def point_in_triangle(px: float, py: float,
                      x0: float, y0: float,
                      x1: float, y1: float,
                      x2: float, y2: float) -> bool:
    """Check if point is inside triangle (including boundary)."""
    def sign(px, py, ax, ay, bx, by):
        return (px - bx) * (ay - by) - (ax - bx) * (py - by)
    
    d1 = sign(px, py, x0, y0, x1, y1)
    d2 = sign(px, py, x1, y1, x2, y2)
    d3 = sign(px, py, x2, y2, x0, y0)
    
    has_neg = (d1 < 0) or (d2 < 0) or (d3 < 0)
    has_pos = (d1 > 0) or (d2 > 0) or (d3 > 0)
    
    return not (has_neg and has_pos)


def in_circumcircle(mesh: Mesh, tri_idx: int, px: float, py: float) -> bool:
    """Check if point is inside circumcircle of triangle."""
    n0, n1, n2 = get_triangle_nodes(mesh, tri_idx)
    x0, y0 = mesh.coords[n0]
    x1, y1 = mesh.coords[n1]
    x2, y2 = mesh.coords[n2]
    
    ax, ay = x0 - px, y0 - py
    bx, by = x1 - px, y1 - py
    cx, cy = x2 - px, y2 - py
    
    det = (
        (ax*ax + ay*ay) * (bx*cy - cx*by)
      - (bx*bx + by*by) * (ax*cy - cx*ay)
      + (cx*cx + cy*cy) * (ax*by - bx*ay)
    )
    return det > 1e-10


# ============================================
# Mesh Initialization
# ============================================

def create_mesh(width: int, height: int) -> Mesh:
    """
    Create mesh with 4 corner vertices forming 2 triangles.
    
        (0,H-1) +-----+ (W-1,H-1)
                |\\    |
                | \\   |
                |  \\  |
                |   \\ |
                |    \\|
          (0,0) +-----+ (W-1,0)
    """
    mesh = Mesh()
    
    # 4 corners
    n_bl = add_node(mesh, 0, 0)
    n_br = add_node(mesh, width - 1, 0)
    n_tl = add_node(mesh, 0, height - 1)
    n_tr = add_node(mesh, width - 1, height - 1)
    
    # Edges
    e_bottom = add_edge(mesh, n_bl, n_br)
    e_right = add_edge(mesh, n_br, n_tr)
    e_top = add_edge(mesh, n_tr, n_tl)
    e_left = add_edge(mesh, n_tl, n_bl)
    e_diag = add_edge(mesh, n_bl, n_tr)
    
    # 2 triangles
    t0 = add_triangle(mesh, e_bottom, e_right, e_diag)
    t1 = add_triangle(mesh, e_diag, e_top, e_left)
    
    # Set diagonal neighbors
    set_neighbor(mesh, t0, e_diag, t1)
    set_neighbor(mesh, t1, e_diag, t0)
    
    return mesh


# ============================================
# Point Location & Insertion
# ============================================

def locate(mesh: Mesh, px: float, py: float) -> int | None:
    """Find leaf triangle containing point. Search from both root triangles."""
    def search(tri_idx: int) -> int | None:
        n0, n1, n2 = get_triangle_nodes(mesh, tri_idx)
        x0, y0 = mesh.coords[n0]
        x1, y1 = mesh.coords[n1]
        x2, y2 = mesh.coords[n2]
        
        if not point_in_triangle(px, py, x0, y0, x1, y1, x2, y2):
            return None
        
        if is_leaf(mesh, tri_idx):
            return tri_idx
        
        for child in mesh.children[tri_idx]:
            result = search(child)
            if result is not None:
                return result
        return None
    
    # Try both root triangles
    result = search(0)
    if result is not None:
        return result
    return search(1)


def split_triangle(mesh: Mesh, tri_idx: int, node_idx: int) -> list[int]:
    """Split triangle into 3 by inserting node."""
    n0, n1, n2 = get_triangle_nodes(mesh, tri_idx)
    p = node_idx
    
    # Original edges
    e_n0_n1 = get_opposite_edge(mesh, tri_idx, n2)
    e_n1_n2 = get_opposite_edge(mesh, tri_idx, n0)
    e_n2_n0 = get_opposite_edge(mesh, tri_idx, n1)
    
    # New edges to center
    e_n0_p = add_edge(mesh, n0, p)
    e_n1_p = add_edge(mesh, n1, p)
    e_n2_p = add_edge(mesh, n2, p)
    
    # 3 new triangles
    t0 = add_triangle(mesh, e_n0_n1, e_n1_p, e_n0_p)
    t1 = add_triangle(mesh, e_n1_n2, e_n2_p, e_n1_p)
    t2 = add_triangle(mesh, e_n2_n0, e_n0_p, e_n2_p)
    
    # Internal adjacency
    set_neighbor(mesh, t0, e_n1_p, t1)
    set_neighbor(mesh, t1, e_n1_p, t0)
    set_neighbor(mesh, t1, e_n2_p, t2)
    set_neighbor(mesh, t2, e_n2_p, t1)
    set_neighbor(mesh, t2, e_n0_p, t0)
    set_neighbor(mesh, t0, e_n0_p, t2)
    
    # Inherit outer neighbors
    for new_tri, edge in [(t0, e_n0_n1), (t1, e_n1_n2), (t2, e_n2_n0)]:
        neighbor = get_neighbor(mesh, tri_idx, edge)
        if neighbor is not None:
            set_neighbor(mesh, new_tri, edge, neighbor)
            set_neighbor(mesh, neighbor, edge, new_tri)
    
    mesh.children[tri_idx] = [t0, t1, t2]
    return [t0, t1, t2]


def flip_edge(mesh: Mesh, t1: int, t2: int) -> tuple[int, int, list[tuple[int, int]]]:
    """Flip shared edge between t1 and t2."""
    shared = find_shared_edge(mesh, t1, t2)
    
    pr = get_opposite_node(mesh, t1, shared)
    pk = get_opposite_node(mesh, t2, shared)
    ep1, ep2 = mesh.edges[shared]
    
    # Get outer edges
    e_pr_ep1 = get_opposite_edge(mesh, t1, ep2)
    e_pr_ep2 = get_opposite_edge(mesh, t1, ep1)
    e_pk_ep1 = get_opposite_edge(mesh, t2, ep2)
    e_pk_ep2 = get_opposite_edge(mesh, t2, ep1)
    
    # New diagonal
    e_pr_pk = add_edge(mesh, pr, pk)
    
    # New triangles
    new_t1 = add_triangle(mesh, e_pr_ep1, e_pk_ep1, e_pr_pk)
    new_t2 = add_triangle(mesh, e_pr_pk, e_pk_ep2, e_pr_ep2)
    
    # Internal adjacency
    set_neighbor(mesh, new_t1, e_pr_pk, new_t2)
    set_neighbor(mesh, new_t2, e_pr_pk, new_t1)
    
    # Inherit outer neighbors
    outer = [
        (new_t1, e_pr_ep1, t1), (new_t1, e_pk_ep1, t2),
        (new_t2, e_pr_ep2, t1), (new_t2, e_pk_ep2, t2),
    ]
    for new_tri, edge, old_tri in outer:
        neighbor = get_neighbor(mesh, old_tri, edge)
        if neighbor is not None:
            set_neighbor(mesh, new_tri, edge, neighbor)
            set_neighbor(mesh, neighbor, edge, new_tri)
    
    mesh.children[t1].extend([new_t1, new_t2])
    mesh.children[t2].extend([new_t1, new_t2])
    
    return new_t1, new_t2, [(nt, e) for nt, e, _ in outer]


def insert_point(mesh: Mesh, x: float, y: float) -> list[int]:
    """
    Insert point into triangulation with Delaunay flipping.
    Returns list of new/modified leaf triangles.
    """
    node_idx = add_node(mesh, x, y)
    
    container = locate(mesh, x, y)
    if container is None:
        raise ValueError(f"Point ({x}, {y}) outside mesh")
    
    new_tris = split_triangle(mesh, container, node_idx)
    affected = set(new_tris)
    
    # Flip stack
    stack = []
    for tri in new_tris:
        edge = get_opposite_edge(mesh, tri, node_idx)
        neighbor = get_neighbor(mesh, tri, edge)
        if neighbor is not None and is_leaf(mesh, neighbor):
            stack.append((tri, edge))
    
    while stack:
        tri, edge = stack.pop()
        neighbor = get_neighbor(mesh, tri, edge)
        
        if neighbor is None or not is_leaf(mesh, tri) or not is_leaf(mesh, neighbor):
            continue
        
        pk_x, pk_y = mesh.coords[get_opposite_node(mesh, neighbor, edge)]
        
        if not in_circumcircle(mesh, tri, pk_x, pk_y):
            continue
        
        new_t1, new_t2, outer_edges = flip_edge(mesh, tri, neighbor)
        affected.discard(tri)
        affected.discard(neighbor)
        affected.add(new_t1)
        affected.add(new_t2)
        
        for nt, e in outer_edges:
            adj = get_neighbor(mesh, nt, e)
            if adj is not None and is_leaf(mesh, adj):
                stack.append((nt, e))
    
    return list(affected)


def get_triangles(mesh: Mesh) -> list[tuple[int, int, int]]:
    """Get all leaf triangles as node indices."""
    return [get_triangle_nodes(mesh, i) 
            for i in range(len(mesh.triangles)) 
            if is_leaf(mesh, i)]


# ============================================
# Greedy Terrain Generator
# ============================================

class GreedyTerrain:
    """
    Greedy Insertion terrain mesh generator.
    
    Algorithm:
    1. Start with 4 corner triangulation
    2. Find point with maximum interpolation error
    3. Insert into Delaunay triangulation
    4. Repeat until error < threshold
    """
    
    def __init__(self, terrain: np.ndarray):
        if terrain.ndim != 2:
            raise ValueError("terrain must be 2D")
        self.terrain = terrain.astype(np.float32)
        self.height, self.width = terrain.shape
    
    def _interp_height(self, x: int, y: int,
                       x0: float, y0: float,
                       x1: float, y1: float,
                       x2: float, y2: float) -> float | None:
        """Barycentric interpolation of height at (x, y)."""
        H, W = self.height, self.width
        
        # Check bounds
        for vx, vy in [(x0, y0), (x1, y1), (x2, y2)]:
            if vx < 0 or vx >= W or vy < 0 or vy >= H:
                return None
        
        denom = (y1 - y2) * (x0 - x2) + (x2 - x1) * (y0 - y2)
        if abs(denom) < 1e-10:
            return None
        
        w0 = ((y1 - y2) * (x - x2) + (x2 - x1) * (y - y2)) / denom
        w1 = ((y2 - y0) * (x - x2) + (x0 - x2) * (y - y2)) / denom
        w2 = 1 - w0 - w1
        
        h0 = self.terrain[int(y0), int(x0)]
        h1 = self.terrain[int(y1), int(x1)]
        h2 = self.terrain[int(y2), int(x2)]
        
        return w0 * h0 + w1 * h1 + w2 * h2
    
    def _find_max_error(self, mesh: Mesh, tri_idx: int,
                        used: np.ndarray) -> tuple[float, int, int]:
        """Find point with maximum error in triangle."""
        n0, n1, n2 = get_triangle_nodes(mesh, tri_idx)
        x0, y0 = mesh.coords[n0]
        x1, y1 = mesh.coords[n1]
        x2, y2 = mesh.coords[n2]
        
        # Bounding box
        min_x = max(0, int(min(x0, x1, x2)))
        max_x = min(self.width - 1, int(max(x0, x1, x2)) + 1)
        min_y = max(0, int(min(y0, y1, y2)))
        max_y = min(self.height - 1, int(max(y0, y1, y2)) + 1)
        
        best_err, best_x, best_y = 0.0, -1, -1
        
        for y in range(min_y, max_y + 1):
            for x in range(min_x, max_x + 1):
                if used[y, x]:
                    continue
                if not point_in_triangle(float(x), float(y), x0, y0, x1, y1, x2, y2):
                    continue
                
                interp = self._interp_height(x, y, x0, y0, x1, y1, x2, y2)
                if interp is None:
                    continue
                
                err = abs(interp - self.terrain[y, x])
                if err > best_err:
                    best_err, best_x, best_y = err, x, y
        
        return best_err, best_x, best_y
    
    def generate(self, max_error: float = 0.0,
                 max_triangles: int = None,
                 max_vertices: int = None) -> tuple[np.ndarray, np.ndarray]:
        """
        Generate terrain mesh.
        
        Returns: (vertices, triangles)
            vertices: (N, 3) array of (x, y, z)
            triangles: (M, 3) array of vertex indices
        """
        H, W = self.height, self.width
        mesh = create_mesh(W, H)
        
        used = np.zeros((H, W), dtype=bool)
        for x, y in [(0, 0), (W-1, 0), (0, H-1), (W-1, H-1)]:
            used[y, x] = True
        
        # Error tracking per triangle
        # tri_errors[tri_idx] = (err, x, y)
        # heap: [(-err, tri_idx, x, y), ...]  (lazy deletion)
        tri_errors: dict[int, tuple[float, int, int]] = {}
        heap: list[tuple[float, int, int, int]] = []
        
        def update_triangle(tri_idx: int):
            """Update error for a single triangle."""
            err, x, y = self._find_max_error(mesh, tri_idx, used)
            if err > 0 and x >= 0:
                tri_errors[tri_idx] = (err, x, y)
                heapq.heappush(heap, (-err, tri_idx, x, y))
        
        # Initial: only 2 triangles
        for tri_idx in range(len(mesh.triangles)):
            update_triangle(tri_idx)
        
        num_verts = 4
        iteration = 0
        
        while heap:
            neg_err, tri_idx, x, y = heapq.heappop(heap)
            err = -neg_err
            
            # Validate (lazy deletion check)
            if tri_idx not in tri_errors or tri_errors[tri_idx] != (err, x, y):
                continue
            if used[y, x] or not is_leaf(mesh, tri_idx):
                continue
            
            # Termination
            if err <= max_error:
                break
            if max_vertices and num_verts >= max_vertices:
                break
            
            # Insert and get affected triangles
            affected = insert_point(mesh, float(x), float(y))
            used[y, x] = True
            num_verts += 1
            
            # Remove old entry
            del tri_errors[tri_idx]
            
            if max_triangles and len(get_triangles(mesh)) >= max_triangles:
                break
            
            # Update only affected triangles
            for t in affected:
                update_triangle(t)
            
            iteration += 1
            if iteration % 100 == 0:
                print(f"  Iteration {iteration}: {num_verts} vertices, error={err:.4f}")
        
        # Build output
        result_tris = get_triangles(mesh)
        node_to_idx: dict[int, int] = {}
        vertices = []
        
        for tri in result_tris:
            for n in tri:
                if n not in node_to_idx:
                    x, y = mesh.coords[n]
                    z = self.terrain[int(y), int(x)]
                    node_to_idx[n] = len(vertices)
                    vertices.append([x, y, z])
        
        triangles = [[node_to_idx[n] for n in tri] for tri in result_tris]
        
        return np.array(vertices), np.array(triangles)


# ============================================
# Utilities
# ============================================

def fractal_noise(shape, grid_shape, octaves=4, persistence=0.5, seed=None):
    """Generate fractal Perlin noise terrain."""
    def perlin(shape, grids, seed):
        if seed is not None:
            np.random.seed(seed)
        
        h, w = shape
        gy, gx = grids
        
        grad = np.random.randn(gy + 1, gx + 1, 2)
        grad /= np.linalg.norm(grad, axis=-1, keepdims=True)
        
        p = np.stack(np.meshgrid(
            np.linspace(0, gx, w, endpoint=False),
            np.linspace(0, gy, h, endpoint=False),
        ), axis=-1)
        
        p0 = p.astype(int)
        f = p - p0
        
        corners = np.array([[0, 0], [1, 0], [0, 1], [1, 1]])
        v = p0[:, :, None, :] + corners
        g = grad[v[..., 1], v[..., 0]]
        d = f[:, :, None, :] - corners
        n = (g * d).sum(axis=-1)
        
        t = f**2 * (3 - 2*f)
        w = np.where(corners == 0, 1 - t[:, :, None, :], t[:, :, None, :]).prod(axis=-1)
        
        return (w * n).sum(axis=-1)
    
    result = np.zeros(shape)
    amp = 1.0
    grids = np.array(grid_shape)
    
    for i in range(octaves):
        result += amp * perlin(shape, tuple(grids), seed + i if seed else None)
        amp *= persistence
        grids *= 2
    
    return (result - result.min()) / (result.max() - result.min())


import matplotlib.pyplot as plt
from matplotlib.collections import PolyCollection

size = 33
heightmap = fractal_noise((size, size), (4, 4), octaves=4, seed=42).astype(np.float32)

fig, axes = plt.subplots(1, 3, figsize=(15, 5))
thresholds = [0.1, 0.05, 0.02]

for i, thresh in enumerate(thresholds):
    print(f"Generating max_error={thresh}...")
    gt = GreedyTerrain(heightmap)
    verts, tris = gt.generate(max_error=thresh)
    
    verts_2d = verts[:, :2]
    tri_verts = [verts_2d[t] for t in tris]
    colors = [heightmap[min(int(verts_2d[t][:, 1].mean()), size-1),
                       min(int(verts_2d[t][:, 0].mean()), size-1)]
              for t in tris]
    
    pc = PolyCollection(tri_verts, array=np.array(colors), cmap='terrain',
                        edgecolors='black', linewidths=0.3)
    axes[i].add_collection(pc)
    axes[i].set_xlim(-0.5, size - 0.5)
    axes[i].set_ylim(-0.5, size - 0.5)
    axes[i].set_aspect('equal')
    axes[i].axis('off')
    axes[i].set_title(f'max_error={thresh}\n{len(tris)} triangles')

plt.tight_layout()
plt.plot()

# %%
