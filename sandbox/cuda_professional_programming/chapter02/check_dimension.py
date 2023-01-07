import pycuda.autoinit

from pycuda.compiler import SourceModule

#    printf("threadIdx:(%d, %d, %d) blockIdx:(%d, %d, %d) blockDim:(%d, %d, %d) gridDim:(%d, %d, %d)\n",
#        threadIdx.x, threadIdx.y, threadIdx.z, blockIdx.x, blockIdx.y, blockIdx.z,
#        blockDim.x, blockDim.y, blockDim.z, gridDim.x, gridDim.y, gridDim.z);
kernel_source = """
__global__ void checkIndex(void)
{
    printf("threadIdx:(%d, %d, %d)\\n", threadIdx.x, threadIdx.y, threadIdx.z);
    printf("blockIdx:(%d, %d, %d)\\n", blockIdx.x, blockIdx.y, blockIdx.z);
    printf("blockDim:(%d, %d, %d)\\n", blockDim.x, blockDim.y, blockDim.z);
    printf("gridDim:(%d, %d, %d)\\n", gridDim.x, gridDim.y, gridDim.z);
}
"""

nElem = 6
block = (3, 1, 1)
#grid = (((nElem + block[0] - 1) // block[0]), 1)
grid = ((nElem + block[0] - 1) // block[0])
#block = 3
#grid = ((nElem + block - 1) // block)
SourceModule(kernel_source).get_function("checkIndex")(grid=grid, block=block)