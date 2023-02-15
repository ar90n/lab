from peachpy import *
from peachpy.x86_64 import *
import numpy as np
import ctypes


def gen_andnot_ps():
    x = Argument(ptr(const_float_))
    y = Argument(ptr(const_float_))
    z = Argument(ptr(float_))

    with Function("AndNot", (x, y, z), target=uarch.default + isa.avx2) as asm_function:
        reg_x = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_x, x)

        reg_y = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_y, y)

        reg_z = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_z, z)

        ymm = YMMRegister()
        VMOVUPD(ymm, [reg_x])
        VANDNPS(ymm, ymm, [reg_y])
        VMOVUPD([reg_z], ymm)
        RETURN()

    return asm_function.finalize(abi.detect()).encode().load()


andnot_ps = gen_andnot_ps()

x = np.array(
    [
        0x59595959,
        0x59595959,
        0x59595959,
        0x59595959,
        0x59595959,
        0x59595959,
        0x59595959,
        0x59595959,
    ],
    dtype=np.uint32,
)
y = np.array(
    [
        0x95959595,
        0x95959595,
        0x95959595,
        0x95959595,
        0x95959595,
        0x95959595,
        0x95959595,
        0x95959595,
    ],
    dtype=np.uint32,
)
z = np.empty(8, dtype=np.uint32)
andnot_ps(
    x.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    y.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    z.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
)
assert np.allclose(
    z,
    np.array(
        [
            0x84848484,
            0x84848484,
            0x84848484,
            0x84848484,
            0x84848484,
            0x84848484,
            0x84848484,
            0x84848484,
        ],
        dtype=np.uint32,
    ),
)
