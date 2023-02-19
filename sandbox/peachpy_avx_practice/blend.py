from peachpy import *
from peachpy.x86_64 import *
import numpy as np
import ctypes

def gen_blend_ps():
    x = Argument(ptr(const_float_))
    y = Argument(ptr(const_float_))
    z = Argument(ptr(float_))

    with Function("Blend", (x, y, z), target=uarch.default + isa.avx2) as asm_function:
        reg_x = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_x, x)

        reg_y = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_y, y)

        reg_z = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_z, z)

        ymm = YMMRegister()
        VMOVUPS(ymm, [reg_x])
        VBLENDPS(ymm, ymm, [reg_y], 0b01010101)
        VMOVUPS([reg_z], ymm)
        RETURN()

    return asm_function.finalize(abi.detect()).encode().load()

blend_ps = gen_blend_ps()
x = np.array([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0], dtype=np.float32)
y = np.array([10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0], dtype=np.float32)
z = np.zeros(8, dtype=np.float32)
blend_ps(
    x.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    y.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    z.ctypes.data_as(ctypes.POINTER(ctypes.c_float))
)
assert np.allclose(
    z,
    np.array(
        [10.0, 2.0, 30.0, 4.0, 50.0, 6.0, 70.0, 8.0],
        dtype=np.float32
    ),
    atol=1e-3
)
