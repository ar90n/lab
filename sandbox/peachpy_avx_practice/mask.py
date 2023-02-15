from peachpy import *
from peachpy.x86_64 import *
import numpy as np
import ctypes


def gen_maskload_pd():
    x = Argument(ptr(const_double_))
    y = Argument(ptr(const_uint64_t))
    z = Argument(ptr(double_))

    with Function(
        "MaskLoad", (x, y, z), target=uarch.default + isa.avx2
    ) as asm_function:
        reg_x = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_x, x)

        reg_y = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_y, y)

        reg_z = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_z, z)

        ymm0 = YMMRegister()
        ymm1 = YMMRegister()
        VMOVUPD(ymm1, [reg_y])
        VMASKMOVPD(ymm0, ymm1, [reg_x])
        VMOVUPD([reg_z], ymm0)
        RETURN()

    return asm_function.finalize(abi.detect()).encode().load()


def gen_maskload_ps():
    x = Argument(ptr(const_float_))
    y = Argument(ptr(const_uint32_t))
    z = Argument(ptr(float_))

    with Function(
        "MaskLoad", (x, y, z), target=uarch.default + isa.avx2
    ) as asm_function:
        reg_x = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_x, x)

        reg_y = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_y, y)

        reg_z = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_z, z)

        ymm0 = YMMRegister()
        ymm1 = YMMRegister()
        VMOVUPD(ymm1, [reg_y])
        VMASKMOVPS(ymm0, ymm1, [reg_x])
        VMOVUPD([reg_z], ymm0)
        RETURN()

    return asm_function.finalize(abi.detect()).encode().load()


maskload_pd = gen_maskload_pd()
maskload_ps = gen_maskload_ps()


x = np.array([1.0, 2.0, 3.0, 4.0], dtype=np.float64)
y = np.array([0, 1 << 63, 0, 1 << 63], dtype=np.uint64)
z = np.array([0.0, 0.0, 0.0, 0.0], dtype=np.float64)
maskload_pd(
    x.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
    y.ctypes.data_as(ctypes.POINTER(ctypes.c_uint64)),
    z.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
)
assert np.allclose(z, np.array([0.0, 2.0, 0.0, 4.0], dtype=np.float64))

x = np.array([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0], dtype=np.float32)
y = np.array([0, 1 << 31, 0, 1 << 31, 1 << 31, 1 << 31, 0, 0], dtype=np.uint32)
z = np.array([0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], dtype=np.float32)
maskload_ps(
    x.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    y.ctypes.data_as(ctypes.POINTER(ctypes.c_uint32)),
    z.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
)
assert np.allclose(z, np.array([0.0, 2.0, 0.0, 4.0, 5.0, 6.0, 0.0, 0.0], dtype=np.float32))
