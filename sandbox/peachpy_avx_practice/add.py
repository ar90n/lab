from peachpy import *
from peachpy.x86_64 import *
import numpy as np
import ctypes


def gen_add_x86():
    x = Argument(int32_t)
    y = Argument(int32_t)

    with Function("Add", (x, y), int32_t) as asm_function:
        reg_x = GeneralPurposeRegister32()
        reg_y = GeneralPurposeRegister32()

        LOAD.ARGUMENT(reg_x, x)
        LOAD.ARGUMENT(reg_y, y)

        ADD(reg_x, reg_y)

        RETURN(reg_x)

    return asm_function.finalize(abi.detect()).encode().load()


def gen_add_sse():
    x = Argument(ptr(const_float_))
    y = Argument(ptr(const_float_))

    with Function("Add", (x, y)) as asm_function:
        reg_x = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_x, x)

        reg_y = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_y, y)

        xmm_x = XMMRegister()
        xmm_y = XMMRegister()

        MOVUPS(xmm_x, [reg_x])
        MOVUPS(xmm_y, [reg_y])
        ADDPS(xmm_x, xmm_y)
        MOVUPS([reg_x], xmm_x)
        RETURN()

    return asm_function.finalize(abi.detect()).encode().load()

def gen_add_avx():
    x = Argument(ptr(const_float_))
    y = Argument(ptr(const_float_))

    with Function("Add", (x, y), target=uarch.default + isa.avx) as asm_function:
        reg_x = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_x, x)

        reg_y = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_y, y)

        ymm = YMMRegister()
        VMOVUPS(ymm, [reg_x])
        VADDPS(ymm, ymm, [reg_y])
        VMOVUPS([reg_x], ymm)
        RETURN()

    return asm_function.finalize(abi.detect()).encode().load()


add_x86 = gen_add_x86()
add_sse = gen_add_sse()
add_avx = gen_add_avx()

assert add_x86(2, 2) == 4

x = np.array([1.0, 2.0, 3.0, 4.0], dtype=np.float32)
y = np.array([2.0, 2.0, 3.0, 4.0], dtype=np.float32)
add_sse(
    x.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    y.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
)
assert np.allclose(x, [3.0, 4.0, 6.0, 8.0])

x = np.array([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0], dtype=np.float32)
y = np.array([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0], dtype=np.float32)
add_avx(
    x.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    y.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
)
assert np.allclose(x, [2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0])