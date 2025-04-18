# %%
import math
from build123d import *
from ocp_vscode import show
# %%
collector_inner_diameter = 40.0
collector_outer_diameter = collector_inner_diameter + 3.4
outer_height = 32.0

hose_inner_diameter = 36.8
hose_outer_diameter = hose_inner_diameter + 3.4
outer_height = 35.0

mid_height = (collector_outer_diameter - hose_outer_diameter) / 2.0

# %%
pts = [
    (-collector_outer_diameter / 2.0, 0),
    (-collector_inner_diameter / 2.0, 0),
    (-collector_inner_diameter / 2.0, outer_height),
    (-hose_inner_diameter / 2.0, outer_height + mid_height),
    (-hose_inner_diameter / 2.0, outer_height + mid_height + outer_height),
    (-hose_outer_diameter / 2.0, outer_height + mid_height + outer_height),
    (-hose_outer_diameter / 2.0, outer_height + mid_height),
    (-collector_outer_diameter / 2.0, outer_height),
    (-collector_outer_diameter / 2.0, 0),
]
# %%
with BuildPart() as neck_shape:
    with BuildSketch(Plane.XZ) as sk:
        with BuildLine():
            Polyline(*pts)
        make_face()
    revolve(axis=Axis.Z)

    chamfer(neck_shape.part.edges().filter_by(GeomType.CIRCLE).sort_by(Axis.Z).group_by(Axis.Z)[0].sort_by(SortBy.RADIUS)[-2], 1.0)
    chamfer(neck_shape.part.edges().filter_by(GeomType.CIRCLE).sort_by(Axis.Z).group_by(Axis.Z)[-1].sort_by(SortBy.RADIUS)[-2], 1.0)

# %%
show(neck_shape)
# %%
export_stl(neck_shape.part, "simple_hose_joint.stl")
# %%
