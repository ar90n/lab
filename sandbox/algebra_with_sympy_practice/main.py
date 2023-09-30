# %%
from algebra_with_sympy import *
# %%
var("a b c d")
eq1 = Eqn(a/ b, c /d)
eq1

# %%
eq2 = eq1 * b
# %%
eq2
# %%
var('kg m s', positive = True)
eq2.evalf(3, subs={b: 2.50 * kg, c: 4.54 * m, d: 3.75 * s ** 2})
# %%
eq3 =@ a=ln(c/d)
eq3
# %%
eq4 = exp(eq3)
eq4
# %%
p, V, n, R, T = var('p V n R T')
eq1 =@ p * V = n * R * T
eq1
# %%
eq2 = eq1 / V
eq2
# %%
eq3 = eq2 / R / T
eq3
# %%
eq4 = eq3 * R / p
eq4
# %%
eq5 = 1/ eq4 -T
eq5
# %%
L, atm, mol, K = var('L atm mol K', positive=True, real=True)

# %%
eq2.subs({R: 0.08206 * L * atm / mol /K, T: 273 * K, n: 1.00 * mol, V:240.0 * L}).evalf(4)
# %%
q = Eqn(a * c, b / c ** 2)
q
# %%
diff(q, b)
# %%
diff(q, c)
# %%
diff(log(q), b)
# %%
diff(q, c, 2)
# %%
diff(diff(q, c), b)
# %%
diff(diff(q, b), c)
# %%
q =@ a * c = b /c
# %%
integrate(q, b, side='rhs')
# %%
integrate(q, b, side='lhs')

# %%
Eqn(integrate(q, b, side='rhs'), integrate(q, b, side='lhs'))
# %%
tosolv = Eqn(a - b, c / a)
# %%
solve(tosolv, a)
# %%
