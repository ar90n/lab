# %%
import math
from build123d import *
from ocp_vscode import show, show_object
# %%
width = 27.0
height = 21.0
depth = 12.0
thickness = 3.0
hole_interval = (10.6 + 17.3) / 2.0
camera_hole_interval = (18.4 + 23.0) / 2.0
# %%
with BuildPart() as mounter:
    with BuildSketch(Plane.XZ):
        with BuildLine():
            Polyline(*[
                (0.0, 0.0),
                (depth, 0.0),
                (depth, height),
                (depth - thickness, height),
                (depth - thickness, thickness),
                (0, thickness),
                (0.0, 0.0)
            ])
        make_face()
    extrude(amount = width / 2.0)

    base_topf = mounter.faces().filter_by(Plane.XY).sort_by(Axis.Z)[1]
    with Locations(base_topf):
        with Locations((2.0, hole_interval / 2.0 - width / 4.0)):
            Hole(3.0 / 2.0)

    back_frontf = mounter.faces().filter_by(Plane.ZY).sort_by(Axis.X)[1]
    with Locations(back_frontf):
        with Locations(((height - thickness) / 2.0 - 15.0, camera_hole_interval / 2.0 - width / 4.0)):
            Hole(2.6 / 2.0)

    chamfer(mounter.faces().sort_by(Axis.X).first.edges().sort_by(Axis.Y).first, 5, angle=45.0)
    mirror(about=Plane.XZ)

show(mounter)
# %%
mounter.part.export_stl("camera_mounter.stl")