# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py:hydrogen
#     text_representation:
#       extension: .py
#       format_name: hydrogen
#       format_version: '1.3'
#       jupytext_version: 1.4.2
#   kernelspec:
#     display_name: Python 3
#     language: python
#     name: python3
# ---

# %%
import numpy as np
from scipy.ndimage import sobel, laplace, convolve


# %%
def calc_residual(delta_flow: np.ndarray) -> float:
    return np.sum(np.real(delta_flow @ delta_flow.conj().T)) / delta_flow.size


def calc_edge_map(org: np.ndarray, sigma=3) -> np.ndarray:
    blur = gaussian_filter(org, sigma=sigma)
    return sobel(blur, axis=1, mode="mirror") + 1j * sobel(blur, axis=0, mode="mirror")


def calc_gvf(
    edge_map: np.ndarray,
    sigma=3,
    mu: float = 0.2,
    max_iter: int = 1024,
    conv_criteria: float = 1e-2,
) -> np.ndarray:
    init_flow = sobel(np.real(edge_map), axis=1, mode="mirror") + 1j * sobel(
        np.imag(edge_map), axis=0, mode="mirror"
    )
    init_power = init_flow * init_flow.conj()

    flow = init_flow
    for i in range(max_iter):
        laplace_flow = laplace(np.real(flow), mode="mirror") + 1j * laplace(
            np.imag(flow), mode="mirror"
        )
        delta_flow = mu * laplace_flow - init_power * (flow - init_flow)
        residual = calc_residual(delta_flow)
        if residual < conv_criteria:
            break

        flow += delta_flow
    return flow


def _calc_vector_field_kernel(
    half_kernel: int, radius: float, gamma: float = 2
) -> np.ndarray:
    flow_x, flow_y = np.meshgrid(
        range(-half_kernel, half_kernel + 1, 1), range(-half_kernel, half_kernel + 1, 1)
    )
    flow = flow_x + 1j * flow_y
    dist = np.abs(flow)
    magnitude = np.power((dist + 1e-7), -gamma)
    magnitude[radius < dist] = 0
    return magnitude * flow


def calc_vfc(
    edge_map: np.ndarray,
    sigma: float = 3.0,
    half_kernel: int = 31,
    radius: float = 30,
    gamma=1,
) -> np.ndarray:
    flow_kernel = _calc_vector_field_kernel(half_kernel, radius, gamma)
    flow = convolve(np.real(edge_map), np.real(flow_kernel)) + 1j * convolve(
        np.imag(edge_map), np.imag(flow_kernel)
    )
    return -flow


# %%
from skimage import data, io

# %%
org = data.camera().astype(np.float32)
edge_map = calc_edge_map(org)

# %%
flow_gvf = calc_gvf(edge_map)

# %%
io.imshow(np.angle(flow_gvf))

# %%
flow_vfc = calc_vfc(edge_map)

# %%
io.imshow(np.angle(flow_vfc))
