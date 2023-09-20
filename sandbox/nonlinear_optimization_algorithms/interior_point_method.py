# %%
from typing import Callable, Any, Iterable
from sympy import lambdify, Matrix, MatrixSymbol, Eq, symbols, Inverse, diff, log, linear_eq_to_matrix
import numpy as np
import matplotlib.pyplot as plt


# %%
M = 5
x = MatrixSymbol("x", 2, 1)
Q = MatrixSymbol("Q", 2, 2)
c = MatrixSymbol("c", 2, 1)
u = MatrixSymbol("u", 1, 1)

s = MatrixSymbol("s", 1, 1)
u = MatrixSymbol("u", 1, 1)
rho, sigma = symbols("rho sigma")

dx = MatrixSymbol("dx", 2, 1)
ds = MatrixSymbol("ds", 1, 1)
du = MatrixSymbol("du", 1, 1)


# %%
def gradient(f, x):
    return Matrix([f]).jacobian(x).T


def hessian(f, x):
    return gradient(gradient(f, x), x)


# %%
f_sym = (x[0] - 2) ** 4 + (x[0] - 2 * x[1]) ** 2
g_syms = [Eq(x[0] ** 2 - x[1] + s[0], 0, evaluate=False)]
L_sym = (
    f_sym
    - rho * sum(log(si) for si in s)
    + sum(ui * gi.lhs for ui, gi in zip(u, g_syms))
)
# %%
dL_dx_sym = gradient(L_sym, x)
dL_ds_sym = gradient(L_sym, s)
dL_su_sym = gradient(L_sym, u)
# %%

# %%
eqs = [
    Eq(
        hessian(f_sym, x) * dx + sum([ui * hessian(gi.lhs, x) * dx + dui * gradient(gi.lhs, x) for ui, dui, gi in zip(u, du, g_syms)], Matrix.zeros(2, 1)),
        -gradient(f_sym, x)
        - sum(
            [ui * gradient(gi.lhs, x) for ui, gi in zip(u, g_syms)], Matrix.zeros(2, 1)
        )
    ),
    *[Eq(ui * dsi + dui * si, rho - ui * si, evaluate=False) for ui, si, dui, dsi in zip(u, s, du, ds)],
    *[
        Eq((gradient(gi.lhs, x).T * dx)[0] + dsi, -gi.lhs - si, evaluate=False)
        for si, dsi, gi in zip(s, ds, g_syms)
    ],
]
# %%
from sympy.matrices.expressions import MatrixExpr
eqs2 = []
for eq in eqs:
    eq2 = eq.lhs - eq.rhs
    if isinstance(eq2, MatrixExpr):
        eqs2.append([*eq2])
    else:
        eqs2.append([eq2])
eqs2 = sum(eqs2, [])
# %%
eqs2[2]
# %%
A_sym, b_sym = linear_eq_to_matrix(eqs2, [*dx, *ds, *du])
# %%
A_sym
# %%
A_func = lambdify([x,s, u], A_sym)
b_func = lambdify([x, s, u, rho], b_sym)

# %%
rho_k_1_sym = sigma * u.T * s / s.shape[0]
rho_k_1 = lambdify([sigma, u, s], rho_k_1_sym)

# %%
initial_point = np.array([[1, 2]]).T
initial_s = np.array([[1]])
initial_u = np.array([[0]])
initial_rho = 1.0
cur_sigma = 0.1
criteria = 1e-8
max_iter = 4096

# %%
xs = [initial_point]
cur_s = initial_s
cur_u = initial_u
cur_rho = initial_rho

# %%
k = 0
while criteria < cur_rho:
    cur_A = A_func(xs[-1], cur_s, cur_u)
    cur_b = b_func(xs[-1], cur_s, cur_u, cur_rho)
    ret = np.linalg.solve(cur_A, cur_b)

    alpha_by_s = - cur_s / ret[2]
    alpha_by_u = - cur_u / ret[3]
    bigger_alpha = max(alpha_by_s, alpha_by_u)
    smaller_alpha = min(alpha_by_s, alpha_by_u)

    cur_alpha = 0
    if 0 < min(ret[2], ret[3]):
        cur_alpha = bigger_alpha / 0.8
    elif max(ret[2], ret[3]) < 0:
        cur_alpha = smaller_alpha * 0.8
    else:
        cur_alpha = bigger_alpha * 0.8

    xs.append(xs[-1] + cur_alpha * ret[:2])
    print(cur_s)
    cur_s = cur_s + cur_alpha * ret[2]
    print(cur_s)
    print(cur_u)
    cur_u = cur_u + cur_alpha * ret[3]
    print(cur_u)
    cur_rho = rho_k_1(cur_sigma, cur_u, cur_s)[0, 0]
    print(cur_rho)
    k +=1 
    if max_iter < k:
        break