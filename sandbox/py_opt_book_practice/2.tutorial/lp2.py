# %%
from functools import reduce
from operator import iadd

import pandas as pd
import pulp
# %%
stock_df =  pd.read_csv("stocks.csv")
stock_df
# %%
require_df = pd.read_csv("requires.csv")
require_df
# %%
gain_df = pd.read_csv("gains.csv")
gain_df
# %%
P = gain_df["p"].to_list()
M = stock_df["m"].to_list()
# %%
stock = {row.m: row.stock for row in stock_df.itertuples()}
require = {(row.p, row.m): row.require for row in require_df.itertuples()}
gain = {row.p:row.gain for row in gain_df.itertuples()}
# %%
#x = pulp.LpVariable.dict("x", P, cat="Continuous")
x = pulp.LpVariable.dict("x", P, cat="Integer")

# %%
problem = pulp.LpProblem("lp2", pulp.LpMaximize)
# %%
problem = reduce(
    iadd,
    [x >= 0 for x in x.values()],
    problem
)
# %%
for m in M:
    problem += pulp.lpSum([require[p, m] * x[p] for p in P]) <= stock[m]

# %%
problem += pulp.lpSum([gain[p] * x[p] for p in P])
# %%
status = problem.solve()

# %%
print('Status:', pulp.LpSenses[status])
# %%
for p in P:
    print(p, x[p].value())
print('obj=', problem.objective.value())
# %%
