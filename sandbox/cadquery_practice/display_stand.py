# %%
import math
import cadquery as cq
import numpy as np
from jupyter_cadquery.viewer.client import show, show_object

# %%
#extrude_length = 265
extrude_length = 30
theta = 15
height = 160
front_guard_height = 22
back_thickness = 11
front_thickness = 8
bottom_thickness = 11
display_thickness = 11
holder_thicknes = back_thickness + display_thickness + front_thickness
front_height = holder_thicknes * math.tan(math.pi * theta / 180)
front_depth = holder_thicknes / math.cos(math.pi * theta / 180)
back_depth = 230 * math.sin(math.pi * theta / 180)

# %%
holder = (
    cq.Workplane("XY")
    .box(height, extrude_length, back_thickness)
    .edges("<X and <Z")
    .box(
        5,
        extrude_length,
        back_thickness + display_thickness,
        centered=[True, True, False],
    )
    .edges("|Y and >Z")
    .edges("<X")
    .box(
        front_guard_height + 5,
        extrude_length,
        front_thickness,
        centered=[False, True, False],
    )
    .faces("<X")
    .wires()
    .toPending()
    .workplane(offset=front_height, centerOption="CenterOfMass")
    .transformed(rotate=cq.Vector(theta, 0, 0))
    .rect(extrude_length, front_depth)
    .loft()
    .faces("<X")
    .workplane(invert=True)
    .box(
        extrude_length,
        front_depth + back_depth,
        bottom_thickness,
        centered=[True, False, False],
    )
    .edges("|Y")
    .fillet(2)
    .rotate((0, 0, 0), (0, 1, 0), theta - 90.0)
)
# %%
show(holder)
# %%
cq.exporters.export(holder, "display_holder.stl")

# %%
