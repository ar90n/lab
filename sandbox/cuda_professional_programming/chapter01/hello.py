import pycuda.autoinit

from pycuda.compiler import SourceModule

kernel_source = """
__global__ void helloFromGPU()
{
    printf("Hello World from GPU!\\n");
}
"""

SourceModule(kernel_source).get_function("helloFromGPU")(block=(1, 1, 10), grid=(1, 1))
