from peachpy import *
from peachpy.x86_64 import *
import numpy as np
import ctypes


def gen_broadcast_pd():
    x = Argument(ptr(const_double_))
    y = Argument(ptr(double_))

    with Function("Broadcast", (x, y), target=uarch.default + isa.avx2) as asm_function:
        reg_x = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_x, x)

        reg_y = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_y, y)

        ymm = YMMRegister()
        VBROADCASTF128(ymm, [reg_x])
        VMOVUPD([reg_y], ymm)
        RETURN()

    return asm_function.finalize(abi.detect()).encode().load()


def gen_broadcast_ps():
    x = Argument(ptr(const_float_))
    y = Argument(ptr(float_))

    with Function("Broadcast", (x, y), target=uarch.default + isa.avx2) as asm_function:
        reg_x = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_x, x)

        reg_y = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_y, y)

        ymm = YMMRegister()
        VBROADCASTF128(ymm, [reg_x])
        VMOVUPD([reg_y], ymm)
        RETURN()

    return asm_function.finalize(abi.detect()).encode().load()

def gen_broadcast_sd():
    x = Argument(ptr(const_double_))
    y = Argument(ptr(double_))

    with Function("Broadcast", (x, y), target=uarch.default + isa.avx2) as asm_function:
        reg_x = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_x, x)

        reg_y = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_y, y)

        ymm = YMMRegister()
        VBROADCASTSD(ymm, [reg_x])
        VMOVUPD([reg_y], ymm)
        RETURN()

    return asm_function.finalize(abi.detect()).encode().load()

def gen_broadcast_ss():
    x = Argument(ptr(const_float_))
    y = Argument(ptr(float_))

    with Function("Broadcast", (x, y), target=uarch.default + isa.avx2) as asm_function:
        reg_x = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_x, x)

        reg_y = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_y, y)

        ymm = YMMRegister()
        VBROADCASTSS(ymm, [reg_x])
        VMOVUPS([reg_y], ymm)
        RETURN()

    return asm_function.finalize(abi.detect()).encode().load()

broadcast_pd = gen_broadcast_pd()
broadcast_ps = gen_broadcast_ps()
broadcast_sd = gen_broadcast_sd()
broadcast_ss = gen_broadcast_ss()

x = np.array([1.0, 2.0], dtype=np.float64)
y = np.zeros(4, dtype=np.float64)
broadcast_pd(
    x.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
    y.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
)

assert np.allclose(y, np.array([1.0, 2.0, 1.0, 2.0], dtype=np.float64))

x = np.array([1.0, 2.0, 3.0, 4.0], dtype=np.float32)
y = np.zeros(8, dtype=np.float32)
broadcast_ps(
    x.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    y.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
)

assert np.allclose(y, np.array([1.0, 2.0, 3.0, 4.0, 1.0, 2.0, 3.0, 4.0], dtype=np.float32))

x = np.array([1.0], dtype=np.float64)
y = np.zeros(4, dtype=np.float64)
broadcast_sd(
    x.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
    y.ctypes.data_as(ctypes.POINTER(ctypes.c_double)),
)

assert np.allclose(y, np.array([1.0, 1.0, 1.0, 1.0], dtype=np.float64))

x = np.array([1.0], dtype=np.float32)
y = np.zeros(8, dtype=np.float32)
broadcast_ss(
    x.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    y.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
)

assert np.allclose(y, np.array([1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0], dtype=np.float32))