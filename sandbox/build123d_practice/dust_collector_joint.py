# %%
import math
from build123d import *
from ocp_vscode import show
# %%
outer_width = 1.8
inner_width = 1.2
width = outer_width + inner_width
base_inner_diameter = 57.0 - 2.0 * inner_width
base_outer_diameter = base_inner_diameter + 2 * width
collector_inner_diameter = 37.0 - 2.0 * inner_width
collector_outer_diameter = collector_inner_diameter + 2 * width

# %%
base_height = 35.0
mid_height = ((base_inner_diameter - collector_inner_diameter) / 2.0) /  math.tan(math.radians(45.0))

# %%
pts = [
    (-base_outer_diameter / 2.0, 0),
    (-base_inner_diameter / 2.0, 0),
    (-base_inner_diameter / 2.0, base_height),
    (-collector_inner_diameter / 2.0, base_height + mid_height),
    (-collector_inner_diameter / 2.0, base_height + mid_height + base_height),
    (-collector_outer_diameter / 2.0, base_height + mid_height + base_height),
    (-collector_outer_diameter / 2.0, base_height + mid_height),
    (-base_outer_diameter / 2.0, base_height),
]
# %%
with BuildPart() as p:
    with BuildSketch(Plane.XZ) as sk:
        with BuildLine():
            l1 = Polyline(*pts)
            l2 = Line(l1 @ 1, l1 @ 0)
        make_face()
    revolve(axis=Axis.Z)
# %%
show(p)
# %%
p.part.export_stl("dust_collector_joint.stl")