# %%
from typing import Callable, Any, Iterable
from sympy import (
    lambdify,
    Matrix,
    MatrixSymbol,
    Lt,
    linear_eq_to_matrix,
)
import numpy as np
import matplotlib.pyplot as plt


# %%
M = 5
x = MatrixSymbol("x", 1, 2)
Q = MatrixSymbol("Q", 2, 2)
c = MatrixSymbol("c", 2, 1)
u = MatrixSymbol("u", 1, M)
U = MatrixSymbol("U", 2, M)


# %%
def gradient(f, x):
    return Matrix([f]).jacobian(x).T


def hessian(f, x):
    return gradient(gradient(f, x), x)


# %%
f_sym = (x[0] - 1) ** 2 + (x[1] - 2.5) ** 2
g_syms = [
    Lt(-x[0] + 2 * x[1] - 2, 0, evaluate=False),
    Lt(x[0] + 2 * x[1] - 6, 0, evaluate=False),
    Lt(x[0] - 2 * x[1] - 2, 0, evaluate=False),
    Lt(-x[0], 0, evaluate=False),
    Lt(-x[1], 0, evaluate=False),
]
L_sym = f_sym + sum([ui * gi.lhs for ui, gi in zip(u, g_syms)])
# %%
f = lambdify([x], f_sym, "numpy")
# %%
A, b = linear_eq_to_matrix([*gradient(L_sym, u), *gradient(L_sym, x)], [*u, *x])
A = np.array(A).astype(np.float64)
b = np.array(b).astype(np.float64)
# %%
# %%
def active_set_method(
    initial_point: np.array,
):
    xs = [initial_point]
    actives = np.where(np.abs(A[:-2, -2:] @ xs[-1] - b[:-2]) < 1e-12)[0]
    max_iter= 4096

    for _ in range(max_iter):
        active_indice = [*actives, -2, -1]

        ans = np.linalg.solve(A[active_indice, :][:, active_indice], b[active_indice])
        cur_u = ans[:-2]
        cur_x = ans[-2:]

        if np.linalg.norm(cur_x - xs[-1]) < 1e-12:
            if np.all(cur_u >= 0):
                break
            
            min_index = np.argmin(cur_u)
            actives = np.array([a for i, a in enumerate(actives) if i != min_index])
        elif not np.all(A[:-2, -2:] @ cur_x - b[:-2] < 1e-12):
            orig = xs[-1]
            d = cur_x - orig
            alpha = -float("inf")
            for i in range(M):
                a_i = A[i : i + 1, -2:].T
                A_i = np.hstack([d, np.array([[0, 1], [-1, 0]]) @ a_i])
                if abs(np.linalg.det(A_i)) < 1e-12:
                    continue

                max_a_index = np.argmax(np.abs(a_i))
                orig_i = np.zeros(2)
                orig_i[max_a_index] = b[i,0] / a_i[max_a_index,0]

                O_i = orig_i - orig.flatten()
                alpha_i = np.linalg.solve(A_i, O_i)[0]

                x_i = orig + alpha_i * d
                if np.all(A[:-2, -2:] @ x_i - b[:-2] < 1e-12):
                    alpha = max(alpha, alpha_i)
            cur_x = orig + alpha * d
            actives = np.where(np.abs(A[:-2, -2:] @ cur_x - b[:-2]) < 1e-12)[0]
        xs.append(cur_x)
    return xs
# %%
xs = active_set_method(
    np.array([[2, 0]]).T
)
# %%
xs

# %%
N = 128
X, Y = np.meshgrid(np.linspace(-1, 5, N), np.linspace(-1, 5, N))
ps = np.stack([X, Y]).transpose([1, 2, 0])
Z = np.apply_along_axis(lambda p: f(p.reshape(1, 2)), -1, ps.reshape(-1, 2)).reshape(
    N, N
)
fig, ax = plt.subplots()
C = 10
levels = np.linspace(0, (np.max(Z) - np.min(Z)) ** 0.5, C) ** 2 + np.min(Z)
ax.contour(X, Y, Z, levels, colors="b")
ax.set_aspect("equal", "box")

# x=0, y=0の位置にx軸とy軸を表示
ax.spines["left"].set_position("zero")  # y軸の位置を0に設定
ax.spines["right"].set_color("none")  # 右の枠線を非表示
ax.spines["bottom"].set_position("zero")  # x軸の位置を0に設定
ax.spines["top"].set_color("none")  # 上の枠線を非表示

# x軸、y軸のラベルを設定
ax.xaxis.set_label_coords(0, 0)
ax.yaxis.set_label_coords(0, 0)
ax.set_xlabel("x1", fontsize=14)
ax.set_ylabel("x2", fontsize=14)

L = 5
for i, (pt, last_pt) in enumerate(zip(xs[:L], [0, *xs[: L - 1]])):
    ax.scatter(*pt, color="red", s=20, edgecolors="black", zorder=100)

    if i == 0:
        ax.text(
            pt[0] - 0.1,
            pt[1] + 0.15,
            f"$x^{i}$",
            fontsize=8,
            bbox=dict(facecolor="white"),
        )
    if 0 < i:
        ax.annotate(
            "",
            xy=pt,
            xycoords="data",
            xytext=last_pt,
            textcoords="data",
            arrowprops=dict(arrowstyle="-", connectionstyle="arc3", color="red"),
            zorder=50,
        )

ax.scatter(*xs[-1], color="green", s=20, edgecolors="black", zorder=100)
ax.annotate(
    "",
    xy=xs[-1],
    xycoords="data",
    xytext=xs[L - 1],
    textcoords="data",
    arrowprops=dict(
        arrowstyle="-", linestyle="dotted", connectionstyle="arc3", color="red"
    ),
    zorder=50,
)
ax.text(
    xs[-1][0] - 0.1,
    xs[-1][1] + 0.15,
    f"$x^{{{len(xs) - 1}}}$",
    fontsize=8,
    bbox=dict(facecolor="white"),
)

plt.show()
# %%
