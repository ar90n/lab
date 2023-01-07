import pycuda.driver as cuda
import pycuda.autoinit
from pycuda.compiler import SourceModule

import numpy as np


mod = SourceModule("""
__global__ void doublify(float *a)
{
    int idx = threadIdx.x + threadIdx.y * 4;
    a[idx] *= 2;
}
""")
func = mod.get_function("doublify")

a = np.random.rand(4, 4)
a = a.astype(np.float32)
print(a)

if False:
    a_gpu = cuda.mem_alloc(a.nbytes)
    cuda.memcpy_htod(a_gpu, a)
    func(a_gpu, block=(4, 4, 1))
    a_doubled = np.empty_like(a)
    cuda.memcpy_dtoh(a, a_gpu)
else:
    func(cuda.InOut(a), block=(4, 4, 1))
print(a)