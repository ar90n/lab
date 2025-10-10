# %%
import math
from build123d import *
from ocp_vscode import show, show_object
import numpy as np

# %%
extender_width = 12.1
extender_height = 7.1
extender_round = extender_height / 2.0
width = 25.0
height = 25.0
thickness = 6.0

# %%
with BuildPart() as base:
    with BuildSketch():
        RectangleRounded(width, height, radius=3.0)
        RectangleRounded(extender_width, extender_height, extender_round - 1e-3, mode=Mode.SUBTRACT)
        with Locations([
            ((-extender_width + extender_round)/2.0,0),
            ((extender_width - extender_round)/2.0,0),
        ]):
            Rectangle(extender_round * 0.80, extender_height * 0.80, mode=Mode.SUBTRACT)
        with Locations([
            Location((0.0, 0.0), 45.0),
            Location((0.0, 0.0), -45.0)
        ]):
            Rectangle(extender_height * 0.88, extender_height * 0.88, mode=Mode.SUBTRACT)
    extrude(amount=thickness)

# %%
show(base.part)
# %%
export_stl(base.part, "type_c_extender_mount.stl")
# %%
