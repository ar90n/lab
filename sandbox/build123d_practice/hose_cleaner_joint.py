# %%
import math
from build123d import *
from bd_warehouse.thread import (
    IsoThread,
)
from ocp_vscode import show
# %%
outer_width = 2.8
inner_width = 0.75
collector_inner_diameter = 38.0 - 2.0 * inner_width
collector_outer_diameter = 40.0
outer_height = 45.0

# %%
iso_internal = IsoThread(
    major_diameter=41.0 * MM,
    pitch=5.2 * MM,
    length=35.0 * MM,
    external=False,
    end_finishes=("chamfer", "fade"),
    hand="left",
)
iso_internal.thread_angle = 45.0
# %%
with BuildPart() as neck_shape:
    with BuildSketch():
        Circle(43.0 / 2.0)
        Circle(41.0 / 2.0, mode=Mode.SUBTRACT)
    add(iso_internal.fuse(extrude(amount=35.0)))

    with BuildSketch():
        Circle((collector_outer_diameter + 3.0) / 2.0)
        Circle(collector_outer_diameter / 2.0, mode=Mode.SUBTRACT)
    extrude(amount=-outer_height)

    chamfer(neck_shape.part.edges().filter_by(GeomType.CIRCLE).sort_by(Axis.Z).group_by(Axis.Z)[0].sort_by(SortBy.RADIUS)[-2], 1.0)

# %%
show(neck_shape)
# %%
neck_shape.part.export_stl("hose_cleaner_joint.stl")
# %%
