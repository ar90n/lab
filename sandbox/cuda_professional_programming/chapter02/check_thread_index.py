import pycuda.driver as cuda
import pycuda.autoinit
from pycuda.compiler import SourceModule
import numpy as np


kernel_source = """
__global__ void printThreadIndex(int *A, const int nx, const int ny)
{
    int ix = threadIdx.x + blockIdx.x * blockDim.x;
    int iy = threadIdx.y + blockIdx.y * blockDim.y;
    unsigned int idx = ix + iy * nx;

    printf("thread_id (%d, %d) block_id (%d, %d) coordinate (%d, %d) global index %2d ival %2d\\n",
        threadIdx.x, threadIdx.y, blockIdx.x, blockIdx.y, ix, iy, idx, A[idx]);
}
"""

mod = SourceModule(kernel_source)
fun = mod.get_function("printThreadIndex")

nx = 8
ny = 6
a = np.arange(nx * ny, dtype=np.int32).reshape(ny, nx)

block = (4, 2, 1)
grid = ((nx + block[0] - 1) // block[0], (ny + block[1] - 1) // block[1])
fun(cuda.In(a), np.int32(nx), np.int32(ny), block=block, grid=grid)