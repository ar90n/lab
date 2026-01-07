# %%
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.collections import PolyCollection


def perlin_noise(shape, grid_shape, seed=None):
    if seed is not None:
        np.random.seed(seed)

    height, width = shape
    y_grids, x_grids = grid_shape

    grads = np.random.randn(y_grids + 1, x_grids + 1, 2)
    grads /= np.linalg.norm(grads, axis=-1, keepdims=True)

    p = np.stack(
        np.meshgrid(
            np.linspace(0, x_grids, width, endpoint=False),
            np.linspace(0, y_grids, height, endpoint=False),
        ),
        axis=-1,
    )
    xy0 = p.astype(int)
    f = p - xy0

    idx_offset = np.array([[0, 0], [1, 0], [0, 1], [1, 1]])
    vertex = xy0[:, :, np.newaxis, :] + idx_offset
    g = grads[vertex[..., 1], vertex[..., 0]]
    d = f[:, :, np.newaxis, :] - idx_offset
    n = (g * d).sum(axis=-1)

    # smoothstep補間
    t = f**2 * (3 - 2 * f)
    w = np.where(
        idx_offset == 0, 1 - t[:, :, np.newaxis, :], t[:, :, np.newaxis, :]
    ).prod(axis=-1)

    return (w * n).sum(axis=-1)


def fractal_noise(shape, grid_shape, octaves=4, persistence=0.5, seed=None):
    result = np.zeros(shape)
    amplitude = 1.0
    grids = np.array(grid_shape)

    for i in range(octaves):
        result += amplitude * perlin_noise(
            shape, tuple(grids), seed=seed + i if seed else None
        )
        amplitude *= persistence
        grids *= 2

    result = (result - result.min()) / (result.max() - result.min())
    return result


class Martini:
    def __init__(self, grid_size=257):
        self.grid_size = grid_size
        tile_size = grid_size - 1

        if tile_size & (tile_size - 1):
            raise ValueError(f"Expected grid size to be 2^n+1, got {grid_size}")

        self.max_num_triangles = tile_size * tile_size * 2 - 2
        self.num_parent_triangles = self.max_num_triangles - tile_size * tile_size
        self.coords = np.zeros((self.max_num_triangles, 4), dtype=np.uint16)

        for i in range(self.max_num_triangles):
            _id = i + 2  # _id is 1-based and starts from 2

            if _id & 1:
                # odd: left-bottom triangle
                ax, ay = 0, 0
                bx, by = tile_size, tile_size
                cx, cy = tile_size, 0
            else:
                # even: right-top triangle
                ax, ay = tile_size, tile_size
                bx, by = 0, 0
                cx, cy = 0, tile_size

            while (_id >> 1) > 1:
                _id = _id >> 1
                mx = (ax + bx) >> 1
                my = (ay + by) >> 1

                if _id & 1:
                    bx, by = ax, ay
                    ax, ay = cx, cy
                else:
                    ax, ay = bx, by
                    bx, by = cx, cy

                cx, cy = mx, my

            self.coords[i] = [ax, ay, bx, by]

    def create_tile(self, terrain):
        return Tile(terrain, self)


class Tile:
    def __init__(self, terrain, martini):
        self.grid_size = martini.grid_size
        self.max_num_triangles = martini.max_num_triangles
        self.num_parent_triangles = martini.num_parent_triangles
        self.coords = martini.coords

        if terrain.ndim > 1:
            terrain = terrain.flatten("C")

        self.terrain = terrain.astype(np.float32)
        self.errors = np.zeros(len(terrain), dtype=np.float32)

        self._update()

    def _update(self):
        size = self.grid_size

        for i in range(self.max_num_triangles - 1, -1, -1):
            ax, ay, bx, by = self.coords[i]

            mx = (ax + bx) >> 1
            my = (ay + by) >> 1

            cx = mx + my - ay
            cy = my + ax - mx

            interpolated = (
                self.terrain[ay * size + ax] + self.terrain[by * size + bx]
            ) / 2
            middle_index = my * size + mx
            middle_error = abs(interpolated - self.terrain[middle_index])

            self.errors[middle_index] = max(self.errors[middle_index], middle_error)

            # if not a leaf triangle, propagate error to parent
            if i < self.num_parent_triangles:
                left_child_index = ((ay + cy) >> 1) * size + ((ax + cx) >> 1)
                right_child_index = ((by + cy) >> 1) * size + ((bx + cx) >> 1)
                self.errors[middle_index] = max(
                    self.errors[middle_index],
                    self.errors[left_child_index],
                    self.errors[right_child_index],
                )

    def get_mesh(self, max_error=0):
        size = self.grid_size
        _max = size - 1

        indices = np.zeros(size * size, dtype=np.uint32)
        vertices = []
        triangles = []

        def process_triangle(ax, ay, bx, by, cx, cy):
            mx = (ax + bx) >> 1
            my = (ay + by) >> 1

            if (abs(ax - cx) + abs(ay - cy) > 1) and (
                self.errors[my * size + mx] > max_error
            ):
                process_triangle(cx, cy, ax, ay, mx, my)
                process_triangle(bx, by, cx, cy, mx, my)
            else:
                nonlocal indices

                for vx, vy in [(ax, ay), (bx, by), (cx, cy)]:
                    idx = vy * size + vx
                    if indices[idx] == 0:
                        indices[idx] = len(vertices) + 1
                        vertices.append((vx, vy))

                a = indices[ay * size + ax] - 1
                b = indices[by * size + bx] - 1
                c = indices[cy * size + cx] - 1
                triangles.append((a, b, c))

        process_triangle(0, 0, _max, _max, _max, 0)
        process_triangle(_max, _max, 0, 0, 0, _max)

        return np.array(vertices), np.array(triangles)

    def get_mesh_with_heights(self, max_error=0):
        vertices, triangles = self.get_mesh(max_error)

        size = self.grid_size
        heights = np.array([self.terrain[v[1] * size + v[0]] for v in vertices])
        vertices_3d = np.column_stack([vertices, heights])

        return vertices_3d, triangles

def plot_mesh(
    vertices,
    triangles,
    heightmap=None,
    ax=None,
    show_wireframe=True,
    cmap="terrain",
    title=None,
):
    if ax is None:
        fig, ax = plt.subplots(figsize=(8, 8))

    verts_2d = [vertices[tri] for tri in triangles]

    if heightmap is not None:
        colors = []
        for tri in triangles:
            pts = vertices[tri]
            cx, cy = int(pts[:, 0].mean()), int(pts[:, 1].mean())
            cy = min(cy, heightmap.shape[0] - 1)
            cx = min(cx, heightmap.shape[1] - 1)
            colors.append(heightmap[cy, cx])

        pc = PolyCollection(
            verts_2d,
            array=np.array(colors),
            cmap=cmap,
            edgecolors="k" if show_wireframe else "none",
            linewidths=0.3 if show_wireframe else 0,
        )
    else:
        pc = PolyCollection(
            verts_2d, facecolors="lightblue", edgecolors="k", linewidths=0.5
        )

    ax.add_collection(pc)
    ax.set_xlim(0, vertices[:, 0].max())
    ax.set_ylim(0, vertices[:, 1].max())
    ax.set_aspect("equal")
    ax.axis("off")

    if title:
        ax.set_title(title)

    return ax


def compare_thresholds(tile, heightmap, thresholds, figsize=(15, 5)):
    fig, axes = plt.subplots(1, len(thresholds), figsize=figsize)

    for i, threshold in enumerate(thresholds):
        vertices, triangles = tile.get_mesh(threshold)
        title = f"threshold={threshold}\n{len(triangles)} triangles, {len(vertices)} vertices"
        plot_mesh(vertices, triangles, heightmap, axes[i], title=title)

    plt.tight_layout()
    return fig

# grid size: 2^7 + 1 = 129
size_n = 7
grid_size = (1 << size_n) + 1

heightmap = fractal_noise((grid_size, grid_size), (4, 4), octaves=1, seed=42)
martini = Martini(grid_size)
tile = martini.create_tile(heightmap)
_ = compare_thresholds(tile, heightmap, [0.08, 0.02, 0.005])
# %%
