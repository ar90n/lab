import time
from typing import Callable
from contextlib import contextmanager

import pycuda.driver as cuda
import pycuda.autoinit
from pycuda.compiler import SourceModule

import numpy.typing as npt
import numpy as np


@contextmanager
def timer(callback: Callable[[float], None]):
    start = time.time()
    try:
        yield
    finally:
        end = time.time()
        callback(end - start)


def sum_arrays_on_cpu(
    A: npt.NDArray[np.float32], B: npt.NDArray[np.float32]
) -> npt.NDArray[np.float32]:
    return A + B


def sum_arrays_on_gpu(
    A: npt.NDArray[np.float32], B: npt.NDArray[np.float32], block_size: int
) -> npt.NDArray[np.float32]:
    kernel_source = f"""
    __global__ void sumArraysOnGPU(float *A, float *B, float *c) 
    {{
        int i = blockIdx.x * blockDim.x + threadIdx.x;
        if (i < {A.size}) c[i] = A[i] + B[i];
    }}
    """
    mod = SourceModule(kernel_source)

    c = np.zeros_like(A)
    func = mod.get_function("sumArraysOnGPU")
    grid_size = (A.size + block_size - 1) // block_size
    func(cuda.In(A), cuda.In(B), cuda.Out(c), block=(block_size, 1, 1), grid=(grid_size, 1))
    return c


def main():
    n_elem = 1 << 24
    A = np.random.rand(n_elem).astype(np.float32)
    B = np.random.rand(n_elem).astype(np.float32)
    with timer(lambda t: print(f"CPU time: {t:.6f}")):
        C_cpu = sum_arrays_on_cpu(A, B)
    with timer(lambda t: print(f"GPU time: {t:.6f}")):
        C_gpu = sum_arrays_on_gpu(A, B, 1024)

    epsilon = 1.0e-8
    if epsilon < np.max(abs(C_cpu - C_gpu)):
        print("Arrays do not match!")
    else:
        print("Arrays match!")


if __name__ == "__main__":
    main()
