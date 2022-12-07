# %%
import pulp
from functools import reduce
from operator import iadd

x = pulp.LpVariable("x", cat="Continuous")
y = pulp.LpVariable("y", cat="Continuous")

problem = reduce(
    iadd,
    [1 * x + 3 * y <= 30, 2 * x + 1 * y <= 40, x >= 0, y >= 0, x + 2 * y],
    pulp.LpProblem("LP", pulp.LpMaximize),
)
status = problem.solve()

# %%
print("Status:", pulp.LpStatus[status])
print("x=", x.value(), "y=", y.value(), "obj=", problem.objective.value())
