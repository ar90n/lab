# %%
import math
from build123d import *
from ocp_vscode import show
# %%
outer_width = 2.8
inner_width = 0.5
width = outer_width + inner_width
collector_inner_diameter = 38.0 - 2.0 * inner_width
collector_outer_diameter = collector_inner_diameter + 2 * width
height = 65.0

# %%
# %%
with BuildPart() as p:
    with BuildSketch(Plane.XZ) as sk:
        Circle(collector_outer_diameter / 2.0)
        Circle(collector_inner_diameter / 2.0, mode=Mode.SUBTRACT)
    extrude(amount=height)
# %%
show(p)
# %%
p.part.export_stl("sander_dust_joint.stl")
# %%
