import pycuda.driver as cuda
import pycuda.autoinit

cuda_device_cnt = cuda.Device.count()
print(f"cuda device count: {cuda_device_cnt}")

for i in range(cuda_device_cnt):
    device = cuda.Device(i)
    print(f"======= device {i}: {device.name()} =======")
    for key in cuda.device_attribute.values.values():
        value = device.get_attribute(key)
        print(f"{key}: {value}")
    print()