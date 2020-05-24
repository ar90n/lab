#!/usr/bin/env python
import numpy as np
from numba import njit
import itertools
import os
import ray
import matplotlib.pyplot as plt
from timeit import timeit

if (os.environ.get("RAY_HEAD_SERVICE_HOST") is None):
    ray.init()
else:
    redis_host = os.environ["RAY_HEAD_SERVICE_HOST"]
    ray.init(address=redis_host + ":6379")

n_x_grids = int(os.environ.get("X_GRIDS", 32))
n_y_grids = int(os.environ.get("Y_GRIDS", 32))
grid_width = int(os.environ.get("GRID_WIDTH", 100))
grid_height = int(os.environ.get("GRID_HEIGHT", 100))


def grid_range(begin, end, n_grids):
    ih, it = itertools.tee(np.linspace(begin, end, n_grids + 1))
    next(it)
    return ((h,t) for h,t in zip(ih, it))

@ray.remote
def mandelbrot(c, n = 32, th = 200):
    z = 1j * np.zeros(c.shape)
    r = np.zeros(c.shape)
    for i in range(n):
        mask = np.abs(z) <= th
        z += (z * z + c - z) * mask.astype(np.int)  
        r[mask] = i
    # make smooth
    return r - np.log2(np.log2(np.abs(z) + 1))

def rendering(n_xg, n_yg, gw, gh):
    res = []
    xs = grid_range(-2, 1, n_xg)
    ys = grid_range(-1, 1, n_yg)
    for ((xb, xe), (yb, ye)) in itertools.product(xs, ys):
        x, y = np.meshgrid(np.linspace(xb, xe, gw), np.linspace(yb, ye, gh))
        c = x + 1j * y
        res.append(mandelbrot.remote(c))
    res = ray.get(res)
    return np.concatenate(np.concatenate(np.array(res).reshape(n_yg, n_xg, gh, gw), axis=2),axis=0)


benchmark = timeit(lambda: rendering(n_x_grids, n_y_grids, grid_width, grid_height), number=8) / 8
print(f"time: {benchmark}")


img = rendering(n_x_grids, n_y_grids, grid_width, grid_height)
plt.figure(dpi=200)
plt.imshow(img, cmap='rainbow', interpolation='bilinear', extent=[-2, 1, -1, 1])
plt.xticks(color='None')
plt.yticks(color='None')
plt.tick_params(length=0)
plt.savefig('figure.png') 
