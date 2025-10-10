# %%
import math
from build123d import *
from ocp_vscode import show, show_object
import numpy as np

# %%
half_base_hole_interval = 88.5 / 2.0
base_margin = 6.0
base_width = base_heght = 2 * (half_base_hole_interval + base_margin)
base_thickness = 6.0
# %%
with BuildPart() as base:
    with BuildSketch():
        RectangleRounded(base_width, base_heght, radius=3.0)
    extrude(amount=base_thickness)

    top_f = base.faces().sort_by(Axis.Z)[-1]
    with BuildSketch(top_f):
        with Locations((10, 10)):
            with Locations([(-19.0, -19.0), (-19.0, 19.0), (19.0, -19.0), (19.0, 19.0)]):
                Circle(radius=2.4)
        with Locations([(-half_base_hole_interval, -half_base_hole_interval), (-half_base_hole_interval, half_base_hole_interval), (half_base_hole_interval, -half_base_hole_interval), (half_base_hole_interval, half_base_hole_interval)]):
            Circle(radius=2.4)
    extrude(amount=-4.0, mode=Mode.SUBTRACT)

# %%
show(base.part)
# %%
export_stl(base.part, "adx_s_base.stl")
# %%
