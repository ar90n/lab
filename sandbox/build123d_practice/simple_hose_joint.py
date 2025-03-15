# %%
import math
from build123d import *
from ocp_vscode import show
# %%
outer_width = 2.8
inner_width = 0.75
collector_inner_diameter = 38.0 - 2.0 * inner_width
collector_outer_diameter = 40.0
outer_height = 80.0

# %%
with BuildPart() as neck_shape:
    with BuildSketch():
        Circle((collector_outer_diameter + 3.0) / 2.0)
        Circle(collector_outer_diameter / 2.0, mode=Mode.SUBTRACT)
    extrude(amount=-outer_height)

    chamfer(neck_shape.part.edges().filter_by(GeomType.CIRCLE).sort_by(Axis.Z).group_by(Axis.Z)[0].sort_by(SortBy.RADIUS)[-2], 1.0)
    chamfer(neck_shape.part.edges().filter_by(GeomType.CIRCLE).sort_by(Axis.Z).group_by(Axis.Z)[-1].sort_by(SortBy.RADIUS)[-2], 1.0)

# %%
show(neck_shape)
# %%
export_stl(neck_shape.part, "simple_hose_joint.stl")
# %%
