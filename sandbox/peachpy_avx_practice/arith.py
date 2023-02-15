from peachpy import *
from peachpy.x86_64 import *
import numpy as np
import ctypes


def gen_addsub_ps():
    x = Argument(ptr(const_float_))
    y = Argument(ptr(const_float_))
    z = Argument(ptr(float_))

    with Function("AddSub", (x, y, z), target=uarch.default + isa.avx2) as asm_function:
        reg_x = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_x, x)

        reg_y = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_y, y)

        reg_z = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_z, z)

        ymm = YMMRegister()
        VMOVUPD(ymm, [reg_x])
        VADDSUBPS(ymm, ymm, [reg_y])
        VMOVUPD([reg_z], ymm)
        RETURN()

    return asm_function.finalize(abi.detect()).encode().load()


def gen_hadd_ps():
    x = Argument(ptr(const_float_))
    y = Argument(ptr(const_float_))
    z = Argument(ptr(float_))

    with Function("Hadd", (x, y, z), target=uarch.default + isa.avx2) as asm_function:
        reg_x = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_x, x)

        reg_y = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_y, y)

        reg_z = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_z, z)

        ymm = YMMRegister()
        VMOVUPD(ymm, [reg_x])
        VHADDPS(ymm, ymm, [reg_y])
        VMOVUPD([reg_z], ymm)
        RETURN()
    return asm_function.finalize(abi.detect()).encode().load()


def gen_hsub_ps():
    x = Argument(ptr(const_float_))
    y = Argument(ptr(const_float_))
    z = Argument(ptr(float_))

    with Function("Hsub", (x, y, z), target=uarch.default + isa.avx2) as asm_function:
        reg_x = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_x, x)

        reg_y = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_y, y)

        reg_z = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_z, z)

        ymm = YMMRegister()
        VMOVUPD(ymm, [reg_x])
        VHSUBPS(ymm, ymm, [reg_y])
        VMOVUPD([reg_z], ymm)
        RETURN()
    return asm_function.finalize(abi.detect()).encode().load()


def gen_sqrt_ps():
    x = Argument(ptr(const_float_))
    y = Argument(ptr(float_))

    with Function("Sqrt", (x, y), target=uarch.default + isa.avx2) as asm_function:
        reg_x = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_x, x)

        reg_y = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_y, y)

        ymm = YMMRegister()
        VMOVUPD(ymm, [reg_x])
        VSQRTPS(ymm, ymm)
        VMOVUPD([reg_y], ymm)
        RETURN()
    return asm_function.finalize(abi.detect()).encode().load()


def gen_rsqrt_ps():
    x = Argument(ptr(const_float_))
    y = Argument(ptr(float_))

    with Function("Rsqrt", (x, y), target=uarch.default + isa.avx2) as asm_function:
        reg_x = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_x, x)

        reg_y = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_y, y)

        ymm = YMMRegister()
        VMOVUPD(ymm, [reg_x])
        VRSQRTPS(ymm, ymm)
        VMOVUPD([reg_y], ymm)
        RETURN()

    return asm_function.finalize(abi.detect()).encode().load()


def gen_rcp_ps():
    x = Argument(ptr(const_float_))
    y = Argument(ptr(float_))

    with Function("Rcp", (x, y), target=uarch.default + isa.avx2) as asm_function:
        reg_x = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_x, x)

        reg_y = GeneralPurposeRegister64()
        LOAD.ARGUMENT(reg_y, y)

        ymm = YMMRegister()
        VMOVUPD(ymm, [reg_x])
        VRCPPS(ymm, ymm)
        VMOVUPD([reg_y], ymm)
        RETURN()

    return asm_function.finalize(abi.detect()).encode().load()


addsub_ps = gen_addsub_ps()
hadd_ps = gen_hadd_ps()
hsub_ps = gen_hsub_ps()
sqrt_ps = gen_sqrt_ps()
rsqrt_ps = gen_rsqrt_ps()
rcp_ps = gen_rcp_ps()

x = np.array([7.0, 4.0, 2.0, 6.0, 1.0, 3.0, 0.0, 5.0], dtype=np.float32)
y = np.array([4.0, 5.0, 7.0, 3.0, 0.0, 6.0, 1.0, 2.0], dtype=np.float32)
z = np.empty(8, dtype=np.float32)
addsub_ps(
    x.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    y.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    z.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
)
assert np.allclose(
    z, np.array([3.0, 9.0, -5.0, 9.0, 1.0, 9.0, -1.0, 7.0], dtype=np.float32)
)

hadd_ps(
    x.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    y.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    z.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
)
assert np.allclose(
    z, np.array([11.0, 8.0, 9.0, 10.0, 4.0, 5.0, 6.0, 3.0], dtype=np.float32)
)


hsub_ps(
    x.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    y.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    z.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
)
assert np.allclose(
    z, np.array([3.0, -4.0, -1.0, 4.0, -2.0, -5.0, -6.0, -1.0], dtype=np.float32)
)

sqrt_ps(
    x.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    z.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
)
assert np.allclose(
    z,
    np.array(
        [2.6457512, 2.0, 1.4142135, 2.4494897, 1.0, 1.7320508, 0.0, 2.2360679],
        dtype=np.float32,
    ),
)

rsqrt_ps(
    x.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    z.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
)
assert np.allclose(
    z,
    np.array(
        [0.37796447, 0.5, 0.70710677, 0.40824828, 1.0, 0.57735026, np.inf, 0.4472136],
        dtype=np.float32,
    ),
    atol=1e-3,
)

rcp_ps(
    x.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
    z.ctypes.data_as(ctypes.POINTER(ctypes.c_float)),
)
assert np.allclose(
    z,
    np.array(
        [0.14285713, 0.25, 0.5, 0.16666667, 1.0, 0.33333334, np.inf, 0.2],
        dtype=np.float32,
    ),
    atol=1e-3,
)