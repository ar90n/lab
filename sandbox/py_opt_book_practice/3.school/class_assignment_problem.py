# %%
import pandas as pd
import pulp
import itertools
from functools import reduce
from operator import iadd
from collections import defaultdict
import matplotlib.pyplot as plt

# %%
s_df = pd.read_csv("./students.csv")
s_df["score_rank"] = s_df["score"].rank(ascending=False, method="first")
s_df["init_assigned_class"] = s_df["score_rank"].map(lambda x: x % 8).map(dict(enumerate("ABCDEFGH")))
s_pair_df = pd.read_csv("./student_pairs.csv")
# %%
S = s_df["student_id"].tolist()
C = list("ABCDEFGH")
SC = list(itertools.product(S, C))
S_male = [row.student_id for row in s_df.itertuples() if row.gender == 1]
S_female = [row.student_id for row in s_df.itertuples() if row.gender == 0]
S_leader = [row.student_id for row in s_df.itertuples() if row.leader_flag == 1]
S_support = [row.student_id for row in s_df.itertuples() if row.support_flag == 1]
score = {row.student_id: row.score for row in s_df.itertuples()}
SS = [(row.student_id1, row.student_id2) for row in s_pair_df.itertuples()]
score_mean = s_df["score"].mean()
init_flag = defaultdict(int, {(row.student_id, row.init_assigned_class): 1 for row in s_df.itertuples()})
# %%
x = pulp.LpVariable.dicts("x", SC, cat="Binary")
# %%
prob = pulp.LpProblem("ClassAssignmentProblem", pulp.LpMaximize)
# %%
prob = reduce(iadd, [pulp.lpSum(x[s, c] for c in C) == 1 for s in S], prob)
prob = reduce(iadd, [pulp.lpSum(x[s, c] for s in S) >= 39 for c in C], prob)
prob = reduce(iadd, [pulp.lpSum(x[s, c] for s in S) <= 40 for c in C], prob)
prob = reduce(iadd, [pulp.lpSum(x[s, c] for s in S_male) <= 20 for c in C], prob)
prob = reduce(iadd, [pulp.lpSum(x[s, c] for s in S_female) <= 20 for c in C], prob)
prob = reduce(
    iadd,
    [
        pulp.lpSum(x[s, c] * score[s] for s in S)
        >= (score_mean - 10) * pulp.lpSum(x[s, c] for s in S)
        for c in C
    ],
    prob,
)
prob = reduce(
    iadd,
    [
        pulp.lpSum(x[s, c] * score[s] for s in S)
        <= (score_mean + 10) * pulp.lpSum(x[s, c] for s in S)
        for c in C
    ],
    prob,
)
prob = reduce(iadd, [pulp.lpSum(x[s, c] for s in S_leader) >= 2 for c in C], prob)
prob = reduce(iadd, [pulp.lpSum(x[s, c] for s in S_support) <= 1 for c in C], prob)
prob = reduce(
    iadd, [x[s1, c] + x[s2, c] <= 1 for (s1, s2), c in itertools.product(SS, C)], prob
)
prob += pulp.lpSum(x[s, c] * init_flag[s, c] for s, c in SC)
# %%
status = prob.solve()
print(f"Status: {pulp.LpStatus[status]}")
# %%
C2Ss = {
    c: [s for s in S if x[s, c].value() == 1]
    for c in C
}
# %%
for c, Ss in C2Ss.items():
    print(f"Class: {c}")
    print(f"Num: {len(Ss)}")
    print(f"StudentNum: {Ss}")

# %%
result_df2 = s_df.copy()
S2C = {s:c for s, c in itertools.product(S, C) if x[s,c].value() == 1}
result_df2["assigned_class"] = result_df2["student_id"].map(S2C)
result_df2.head(5)

# %%
fig = plt.figure(figsize=(12,20))
for i, c in enumerate(C):
    cls_df = result_df2[result_df2["assigned_class"] == c]
    ax = fig.add_subplot(4,2, i+1, xlabel="score", ylabel="num", xlim=(0,500), ylim=(0,20), title=f"Class: {c}"
    )
    ax.hist(cls_df["score"], bins=range(0,500,40))