import pycuda.gpuarray as gpuarray
import pycuda.driver as cuda
import pycuda.autoinit
import numpy as np

a_gpu = gpuarray.to_gpu(np.random.rand(4, 4).astype(np.float32))
a_doubled = (2 * a_gpu).get()
print(a_doubled)
print(a_gpu)