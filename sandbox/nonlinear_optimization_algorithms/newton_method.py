# %%
from typing import Callable, Any, Iterable
from sympy import lambdify, Matrix, MatrixSymbol
import numpy as np
import matplotlib.pyplot as plt


# %%
x = MatrixSymbol("x", 2, 1)


# %%
def gradient(f, x):
    return Matrix([f]).jacobian(x).T


def hessian(f, x):
    return gradient(gradient(f, x), x)


# %%
f_sym = (x[0] - 2) ** 4 + (x[0] - 2 * x[1]) ** 2
d_sym = -hessian(f_sym, x).inv() * gradient(f_sym, x)
# %%
f = lambdify([x], f_sym, "numpy")
grad_f = lambdify([x], gradient(f_sym, x), "numpy")
d = lambdify([x], d_sym, "numpy")
# %%
criteria = 1e-4
# %%
k = 0
xs = [np.array([[0, 3]]).T]

# %%
while criteria < np.linalg.norm(grad_f(xs[-1])):
    xs.append(xs[-1] + d(xs[-1]))

# %%
xs[-1]

# %%
X, Y = np.meshgrid(np.linspace(-1, 5, N), np.linspace(-1, 5, N))
ps = np.stack([X, Y]).transpose([1, 2, 0])
Z = np.apply_along_axis(lambda p: f(p.reshape(2, 1)), -1, ps.reshape(-1, 2)).reshape(
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

L = 4
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
