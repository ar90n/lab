# %%
import functools
from typing import Callable, Any, Iterable
from sympy import symbols, lambdify, Matrix, MatrixSymbol, diff, Inverse, Function
import numpy as np
import matplotlib.pyplot as plt

# %%
from gradient_descent import (
    secant_method_alpha_search,
    is_wolfe_condition_satisfied,
    secant_method_alpha_update,
)


# %%
def gradient(f, x):
    return Matrix([f]).jacobian(x).T


def hessian(f, x):
    return gradient(gradient(f, x), x)


# %%
alpha = symbols("alpha")
x = MatrixSymbol("x", 2, 1)
B = MatrixSymbol("B", 2, 2)
s = MatrixSymbol("s", 2, 1)
y = MatrixSymbol("y", 2, 1)

# %%
bfgs_sym = B - ((B * s) * (B * s).T) / (s.T * B * s)[0, 0] + (y * y.T) / (s.T * y)[0, 0]
# %%
bfgs = lambdify([B, s, y], bfgs_sym, "numpy")
def update_B(
    B: np.ndarray,
    grad_f: Callable[[np.ndarray], np.ndarray],
    cur_x: np.ndarray,
    prev_x: np.ndarray,
) -> np.ndarray:
    s = cur_x - prev_x
    y = grad_f(cur_x) - grad_f(prev_x)
    return bfgs(B, s, y)


# %%
def quansi_newton_method(
    g,
    grad_g,
    grad_f,
    initial_point: np.array,
    initial_alpha: float,
    tau_1: float = 0.3,
    tau_2: float = 0.5,
    criteria: float = 1e-4,
    max_iter: int = 4096,
):
    xs = [initial_point]
    initial_alpha = initial_alpha
    cur_B = np.eye(initial_point.shape[0])
    cur_alpha = initial_alpha

    k = 0
    while criteria < np.linalg.norm(grad_f(xs[-1])):
        cur_g = functools.partial(g, xs[-1], cur_B)
        cur_grad_g = functools.partial(grad_g, xs[-1], cur_B)
        cur_update_alpha = functools.partial(secant_method_alpha_update, cur_grad_g)
        cur_cond_func = functools.partial(
            is_wolfe_condition_satisfied, cur_g, cur_grad_g, tau_1, tau_2
        )

        cur_alpha = secant_method_alpha_search(
            cur_cond_func, cur_update_alpha, cur_alpha
        )

        xs.append(xs[-1] - cur_alpha * np.linalg.inv(cur_B) @ grad_f(xs[-1]))
        cur_B = update_B(cur_B, grad_f, xs[-1], xs[-2])

        k += 1
        if k == max_iter:
            raise ValueError("not converged")
    return xs


# %%
if __name__ == "__main__":
    f_sym = (x[0] - 2) ** 4 + (x[0] - 2 * x[1]) ** 2
    grad_f_sym = gradient(f_sym, x)
    d_sym = -Inverse(B) * grad_f_sym
    g_sym = f_sym.replace(x, x + alpha * d_sym).doit()

    f = lambdify([x], f_sym, "numpy")
    g = lambdify([x, B, alpha], g_sym, "numpy")
    grad_f = lambdify([x], grad_f_sym, "numpy")
    grad_g = lambdify([x, B, alpha], diff(g_sym, alpha), "numpy")

    xs = quansi_newton_method(
        g, grad_g, grad_f, initial_point=np.array([[0.0, 3.0]]).T, initial_alpha=1.0
    )

    # %%
    N = 128
    X, Y = np.meshgrid(np.linspace(-1, 5, N), np.linspace(-1, 5, N))
    ps = np.stack([X, Y]).transpose([1, 2, 0])
    Z = np.apply_along_axis(
        lambda p: f(p.reshape(2, 1)), -1, ps.reshape(-1, 2)
    ).reshape(N, N)
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
